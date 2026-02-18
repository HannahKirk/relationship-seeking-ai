# 0. imports
import json
import os
import random
from dataclasses import dataclass, field
from datetime import datetime
from functools import partial
from typing import Dict, Optional

import numpy as np
import pandas as pd
import torch
from accelerate import FullyShardedDataParallelPlugin, PartialState
from accelerate.accelerator import Accelerator
from datasets import Dataset, Features, Value, load_dataset
from peft import LoraConfig
from torch.distributed.fsdp import (
    FullStateDictConfig,
    MixedPrecision,
    StateDictType,
)
from torch.distributed.fsdp.wrap import transformer_auto_wrap_policy
from transformers import (
    AutoModelForCausalLM,
    AutoTokenizer,
    HfArgumentParser,
    TrainingArguments,
)
from transformers.models.llama.modeling_llama import LlamaDecoderLayer
from trl import BiPOTrainer, DPOConfig

# accelerator.print accelerator config
state = PartialState()
# accelerator.print("Accelerator config:", state.distributed_type)
accelerator = Accelerator()

os.environ["FSDP_CPU_RAM_EFFICIENT_LOADING"] = "1"
os.environ["TRANSFORMERS_CACHE"] = "model_cache"
os.environ["HF_HOME"] = "model_cache"


def set_seed(seed=42):
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.cuda.manual_seed_all(seed)  # if using multi-GPU
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False


def print_trainable_parameters(model):
    """
    accelerator.prints the number of trainable parameters in the model.
    """
    trainable_params = 0
    all_param = 0
    for _, param in model.named_parameters():
        all_param += param.numel()
        if param.requires_grad:
            trainable_params += param.numel()
    accelerator.print(
        f"trainable params: {trainable_params} || all params: {all_param} || trainable%: {100 * trainable_params / all_param}"
    )


# First, let's add a debug function to accelerator.print parameter names
def debug_model_params(model):
    """accelerator.print all parameter names and their requires_grad status"""
    accelerator.print("\nModel parameters:")
    for name, param in model.named_parameters():
        accelerator.print(f"{name}: requires_grad = {param.requires_grad}")


# Modified parameter setting code
def set_trainable_params(model, layer_num):
    # First, disable gradients for all parameters
    for param in model.parameters():
        param.requires_grad = False

    # Then enable only the vec parameter
    for name, param in model.named_parameters():
        if f"model.layers.{layer_num}.vec" in name:
            param.requires_grad = True
            accelerator.print(f"Enabled training for: {name}")

    # accelerator.print the trainable parameters
    print_trainable_parameters(model)

    return model


class BlockWrapper(torch.nn.Module):
    def __init__(self, block, vec=None):
        super().__init__()
        self.multiplier = 1.0
        self.block = block
        if vec is not None:
            self.vec = torch.nn.Parameter(vec)
        else:
            # Zero Init
            hidden_size = model.config.hidden_size
            self.vec = torch.nn.Parameter(
                torch.zeros(hidden_size, dtype=torch.bfloat16)
            )  # Added this line
            print("BlockWrapper init: hidden_size =", model.config.hidden_size)
            print("Created self.vec with shape =", self.vec.shape)

    def forward(self, *args, **kwargs):
        output = self.block(*args, **kwargs)
        output = (output[0] + (self.multiplier * self.vec),) + output[1:]
        return output

    def set_multiplier(self, multiplier):
        self.multiplier = multiplier


# Define and parse arguments.
@dataclass
class ScriptArguments:
    """
    The arguments for the DPO training script.
    """

    # data parameters
    beta: Optional[float] = field(
        default=0.1, metadata={"help": "the beta parameter for DPO loss"}
    )

    # training parameters
    model_name_or_path: Optional[str] = field(
        default="meta-llama/Llama-2-7b-chat-hf",
        metadata={"help": "Llama model path (e.g. meta-llama/Llama-3.1-70B-Instruct)"},
    )
    learning_rate: Optional[float] = field(
        default=5e-4, metadata={"help": "optimizer learning rate"}
    )
    lr_scheduler_type: Optional[str] = field(
        default="cosine", metadata={"help": "the lr scheduler type"}
    )
    warmup_steps: Optional[int] = field(
        default=100, metadata={"help": "the number of warmup steps"}
    )
    weight_decay: Optional[float] = field(
        default=0.05, metadata={"help": "the weight decay"}
    )
    optimizer_type: Optional[str] = field(
        default="adamw_torch", metadata={"help": "the optimizer type"}
    )

    per_device_train_batch_size: Optional[int] = field(
        default=4, metadata={"help": "train batch size per device"}
    )
    per_device_eval_batch_size: Optional[int] = field(
        default=1, metadata={"help": "eval batch size per device"}
    )
    gradient_accumulation_steps: Optional[int] = field(
        default=1, metadata={"help": "the number of gradient accumulation steps"}
    )
    gradient_checkpointing: Optional[bool] = field(
        default=False, metadata={"help": "whether to use gradient checkpointing"}
    )

    max_prompt_length: Optional[int] = field(
        default=2048, metadata={"help": "the maximum prompt length"}
    )
    max_length: Optional[int] = field(
        default=2048, metadata={"help": "the maximum sequence length"}
    )
    num_train_epochs: Optional[int] = field(
        default=20, metadata={"help": "the number of training epochs"}
    )
    logging_steps: Optional[int] = field(
        default=1, metadata={"help": "the logging frequency"}
    )

    log_freq: Optional[int] = field(
        default=1, metadata={"help": "the logging frequency"}
    )

    behavior: Optional[str] = field(
        default="power-seeking", metadata={"help": "the behavior"}
    )
    layer: Optional[int] = field(
        default=15, metadata={"help": "the layer the steering vector extracted from"}
    )

    # instrumentation
    report_to: Optional[str] = field(
        default="none",
        metadata={
            "help": 'The list of integrations to report the results and logs to. Supported platforms are `"azure_ml"`,'
            '`"comet_ml"`, `"mlflow"`, `"neptune"`, `"tensorboard"`,`"clearml"` and `"wandb"`. '
            'Use `"all"` to report to all integrations installed, `"none"` for no integrations.'
        },
    )

    # debug argument for distributed training
    ignore_bias_buffers: Optional[bool] = field(
        default=False,
        metadata={
            "help": "fix for DDP issues with LM bias/mask buffers - invalid scalar type,`inplace operation. See"
            "https://github.com/huggingface/transformers/issues/22482#issuecomment-1595790992"
        },
    )


from datasets import Dataset
from transformers import AutoTokenizer


def get_data(
    num_proc=1,
    behavior="relationship-seeking",
    train=True,
    model_name="meta-llama/Llama-3.1-8B-Instruct",
    system_prompt=None,
):
    """
    Load and process the dataset for DPO training.
    Uses Hugging Face's chat templates for formatting conversations.
    """
    tokenizer = AutoTokenizer.from_pretrained(model_name)

    # Load JSONL from shared data folder at repo root
    file_path = f"../data/{behavior}/{'train' if train else 'test'}.jsonl"
    array_df = pd.read_json(file_path, lines=True)

    # Convert arrays to template-formatted strings
    def format_chat(
        convo_array, model_name, system_prompt=None, add_generation_prompt=False
    ):
        messages = []
        if system_prompt:
            messages.append({"role": "system", "content": system_prompt})
        messages.extend(convo_array)
        input_chat = tokenizer.apply_chat_template(
            messages, tokenize=False, add_generation_prompt=add_generation_prompt
        )
        if "llama-3" in model_name.lower():
            input_chat = input_chat.replace(
                "\n\nCutting Knowledge Date: December 2023\nToday Date: 26 Jul 2024", ""
            )
        return input_chat

    array_df["prompt"] = array_df["prompt_arr"].apply(
        lambda x: format_chat(
            x,
            model_name,
            system_prompt="You are a conversational assistant.",
            add_generation_prompt=False,
        )
    )
    array_df["chosen"] = array_df["chosen_arr"].apply(lambda x: x[0]["content"])
    array_df["rejected"] = array_df["rejected_arr"].apply(lambda x: x[0]["content"])

    # Drop unused columns
    array_df = array_df.drop(columns=["prompt_arr", "chosen_arr", "rejected_arr"])

    # Convert to Hugging Face Dataset
    dataset = Dataset.from_pandas(array_df)

    original_columns = dataset.column_names

    # Define the batched processing function
    def process_for_batching(samples):
        return {
            "prompt": samples["prompt"],
            "chosen": samples["chosen"],
            "rejected": samples["rejected"],
        }

    return dataset.map(
        process_for_batching,
        batched=True,
        num_proc=num_proc,
        remove_columns=original_columns,
    )


if __name__ == "__main__":
    parser = HfArgumentParser(ScriptArguments)
    script_args = parser.parse_args_into_dataclasses()[0]
    set_seed(seed=11)
    supported_models = [
        "meta-llama/Llama-3.1-8B-Instruct",
        "meta-llama/Llama-3.1-70B-Instruct",
    ]
    if script_args.model_name_or_path not in supported_models:
        accelerator.print(
            f"{script_args.model_name_or_path} is not in supported model list. Supported models are: {supported_models} "
        )
    accelerator.print(
        "[Behavior:] ",
        script_args.behavior,
        "[Layer:] ",
        script_args.layer,
        "[Model:] ",
        script_args.model_name_or_path,
    )

    # 1. Load a pretrained model
    cache_dir = "model_cache"
    model = AutoModelForCausalLM.from_pretrained(
        script_args.model_name_or_path,
        low_cpu_mem_usage=True,
        torch_dtype=torch.bfloat16,
        cache_dir=cache_dir,
    )

    model.model.layers[script_args.layer] = BlockWrapper(
        model.model.layers[script_args.layer]
    )
    print("Model Config:")
    print(model.config)

    model.config.use_cache = False

    if script_args.ignore_bias_buffers:
        # torch distributed hack
        model._ddp_params_and_buffers_to_ignore = [
            name for name, buffer in model.named_buffers() if buffer.dtype == torch.bool
        ]

    # 2. Load reference model
    model_ref = AutoModelForCausalLM.from_pretrained(
        script_args.model_name_or_path,
        low_cpu_mem_usage=True,
        torch_dtype=torch.bfloat16,
        cache_dir=cache_dir,
    )  #

    accelerator.print("-----------------------------")
    accelerator.print(script_args.model_name_or_path)

    # 2. Load tokenizer
    tokenizer = AutoTokenizer.from_pretrained(script_args.model_name_or_path)
    tokenizer.pad_token = tokenizer.eos_token

    accelerator.print("Finish loading pre-trained models...")

    accelerator.print("Checking trainable params...")
    # Use these functions after model initialization:
    # Apply to both models
    model = set_trainable_params(model, script_args.layer)

    # 3. Load training dataset
    SYSTEM_PROMPT = "You are a conversational assistant."
    train_dataset = get_data(
        behavior=script_args.behavior,
        train=True,
        model_name=script_args.model_name_or_path,
        system_prompt=SYSTEM_PROMPT,
    )

    # 4. Load val dataset
    test_dataset = get_data(
        behavior=script_args.behavior,
        train=False,
        model_name=script_args.model_name_or_path,
        system_prompt=SYSTEM_PROMPT,
    )

    # Print mini sample of data
    accelerator.print("Sample of training data:")
    # Generate random int
    random_int = random.randint(0, len(train_dataset))
    accelerator.print(train_dataset[random_int])

    # Print mini sample of data
    accelerator.print("Sample of test data:")
    random_int = random.randint(0, len(test_dataset))
    accelerator.print(test_dataset[random_int])

    accelerator.print("Finish loading datasets...")

    # 5. Initialize training arguments:
    training_args = DPOConfig(
        per_device_train_batch_size=script_args.per_device_train_batch_size,
        per_device_eval_batch_size=script_args.per_device_eval_batch_size,
        num_train_epochs=script_args.num_train_epochs,
        logging_steps=script_args.logging_steps,
        save_strategy="no",
        gradient_accumulation_steps=script_args.gradient_accumulation_steps,
        gradient_checkpointing=script_args.gradient_checkpointing,
        learning_rate=script_args.learning_rate,
        eval_strategy="epoch",
        output_dir="placeholder",
        report_to=script_args.report_to,
        lr_scheduler_type=script_args.lr_scheduler_type,
        warmup_steps=script_args.warmup_steps,
        optim=script_args.optimizer_type,
        bf16=True,  # Change this from False to True
        remove_unused_columns=False,
        max_prompt_length=script_args.max_prompt_length,
        max_length=script_args.max_length,
        # precompute_ref_log_probs=True,  # Could add this line if needd
    )

    accelerator.print("Finish setting config...")

    # 6. Set up FSDP

    accelerator.print("Setting up FSDP...")

    mixed_precision_policy = MixedPrecision(
        param_dtype=torch.bfloat16,
        reduce_dtype=torch.bfloat16,
        buffer_dtype=torch.bfloat16,
    )

    if "Llama" in script_args.model_name_or_path:
        auto_wrap_policy = partial(
            transformer_auto_wrap_policy, transformer_layer_cls=(LlamaDecoderLayer,)
        )
    else:
        accelerator.print("Model not supported - only Llama models are supported")

    fsdp_plugin = FullyShardedDataParallelPlugin(
        state_dict_config=FullStateDictConfig(offload_to_cpu=True, rank0_only=True),
        auto_wrap_policy=auto_wrap_policy,
        mixed_precision_policy=mixed_precision_policy,
        param_init_fn=lambda module: module.to(torch.bfloat16),
        sync_module_states=True,
        limit_all_gathers=True,
        use_orig_params=True,
    )

    # 7. Prepare models and set trainable parameters
    # Before accelerator prepare
    accelerator.print("Before accelerator prepare:")
    print_trainable_parameters(model)

    # Prepare models
    accelerator = Accelerator(fsdp_plugin=fsdp_plugin)
    model, model_ref = accelerator.prepare(model, model_ref)

    # After accelerator prepare
    accelerator.print("After accelerator prepare:")
    print_trainable_parameters(model)

    # Then set parameters
    for name, param in model.named_parameters():
        if f".vec" in name:
            param.requires_grad = True
            accelerator.print(f"Enabled training for: {name}")
        else:
            param.requires_grad = False

    # Add a verification loop
    found_trainable = False
    for name, param in model.named_parameters():
        if param.requires_grad:
            found_trainable = True
            accelerator.print(f"Verified trainable: {name}")

    if not found_trainable:
        accelerator.print("Warning: No trainable parameters found after FSDP setup")

    # Reset trainable parameters after FSDP
    accelerator.print("After reset:")
    print_trainable_parameters(model)

    # 8. Initialize trainer
    dpo_trainer = BiPOTrainer(
        model,
        ref_model=model_ref,
        args=training_args,
        beta=script_args.beta,
        train_dataset=train_dataset,
        eval_dataset={
            "test_dataset_add": test_dataset,
            "test_dataset_sub": test_dataset,
        },
        tokenizer=tokenizer,
        behavior=script_args.behavior,
        layer=script_args.layer,
    )

    # 9. Start training
    # Record start time
    start_time = datetime.now().isoformat()
    dpo_trainer.train()

    # 10. Log experiment completion
    if accelerator.is_main_process:
        # Get short model name from full path
        short_model = script_args.model_name_or_path.split("/")[-1]

        log_entry = {
            "short_model": short_model,
            "layer": script_args.layer,
            "n_epochs": script_args.num_train_epochs,
            "start_time": start_time,
            "end_time": datetime.now().isoformat(),
        }

        with open("./vector/experiment_log.jsonl", "a") as f:
            f.write(json.dumps(log_entry) + "\n")
