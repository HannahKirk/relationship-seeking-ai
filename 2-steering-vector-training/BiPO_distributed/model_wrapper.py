import os

os.environ["HF_HOME"] = "/home/ubuntu/relationship-seeking-ai/1-steering-vector-training/BiPO/model_cache"
import torch as t
import transformers
from transformers import AutoModelForCausalLM, AutoTokenizer


class BlockOutputWrapper(t.nn.Module):
    def __init__(self, block):
        super().__init__()
        self.block = block
        self.add_activations = None
        self.activations = None

    def forward(self, *args, **kwargs):
        output = self.block(*args, **kwargs)
        self.activations = output[0]
        if self.add_activations is not None:
            output = (output[0] + self.add_activations,) + output[1:]
        return output

    def add(self, activations, do_projection=False):
        self.add_activations = activations

    def reset(self):
        self.add_activations = None
        self.activations = None


class ModelWrapper:
    def __init__(self, model_name_or_path, system_prompt, token=None):
        self.device = "cuda" if t.cuda.is_available() else "cpu"
        self.system_prompt = system_prompt
        self.model_name = model_name_or_path
        cache_dir = "/home/ubuntu/relationship-seeking-ai/1-steering-vector-training/BiPO/model_cache"

        self.tokenizer = AutoTokenizer.from_pretrained(
            self.model_name, use_auth_token=token, cache_dir=cache_dir
        )
        self.model = AutoModelForCausalLM.from_pretrained(
            self.model_name,
            use_auth_token=token,
            torch_dtype=t.bfloat16,
            cache_dir=cache_dir,
        ).to(self.device)

        for i, layer in enumerate(self.model.model.layers):
            self.model.model.layers[i] = BlockOutputWrapper(layer)

    def set_add_activations(self, layer, activations):
        layer_device = next(self.model.parameters()).device

        # Ensure activations are on the same device
        if activations.device != layer_device:
            activations = activations.to(layer_device)

        self.model.model.layers[layer].add(activations)

    def generate_text_with_conversation_history(
        self, history, max_new_tokens=50, decode=True, skip_special_tokens=True
    ):
        messages = [{"role": "system", "content": self.system_prompt}] + history
        input_chat = self.tokenizer.apply_chat_template(
            messages, tokenize=False, add_generation_prompt=True
        )
        if "llama-3" in self.model_name.lower():
            input_chat = input_chat.replace(
                "\n\nCutting Knowledge Date: December 2023\nToday Date: 26 Jul 2024", ""
            )
        input_tokens = self.tokenizer(
            input_chat, add_special_tokens=False, return_tensors="pt"
        ).to(self.device)

        pad_token_id = self.tokenizer.eos_token_id
        with t.no_grad():
            output_tokens = self.model.generate(
                input_tokens["input_ids"],
                max_new_tokens=max_new_tokens,
                temperature=1.0,
                do_sample = True,
                pad_token_id=pad_token_id,
                attention_mask=input_tokens["attention_mask"],
            )
        # Only decode the new tokens (excluding the input)
        new_tokens = output_tokens[0][input_tokens["input_ids"].shape[1] :]

        if decode:
            return self.tokenizer.decode(
                new_tokens, skip_special_tokens=skip_special_tokens
            )
        return new_tokens

    def generate_with_streaming(self, history, max_new_tokens=200, temperature=1.0, skip_special_tokens=True):
        """
        Generate text with streaming outputs for a more interactive experience.
        Uses a simpler implementation with TextIteratorStreamer for a reliable token stream.
        """
        from transformers import TextIteratorStreamer
        import threading

        messages = [{"role": "system", "content": self.system_prompt}] + history
        input_chat = self.tokenizer.apply_chat_template(
            messages, tokenize=False, add_generation_prompt=True
        )

        # Remove specific date strings if using Llama-3
        if "llama-3" in self.model_name.lower():
            input_chat = input_chat.replace(
                "\n\nCutting Knowledge Date: December 2023\nToday Date: 26 Jul 2024", ""
            )

        input_tokens = self.tokenizer(
            input_chat, add_special_tokens=False, return_tensors="pt"
        ).to(self.device)

        # Get the length of the prompt
        input_length = input_tokens.input_ids.shape[1]

        # Create a TextIteratorStreamer to get a stream of tokens
        streamer = TextIteratorStreamer(self.tokenizer, skip_special_tokens=skip_special_tokens, skip_prompt=True)

        # Set generation parameters
        generation_kwargs = {
            "input_ids": input_tokens.input_ids,
            "attention_mask": input_tokens.attention_mask,
            "max_new_tokens": max_new_tokens,
            "temperature": 1.0,
            "do_sample": temperature > 0,
            "pad_token_id": self.tokenizer.eos_token_id,
            "streamer": streamer,
        }

        # Create a thread to run the generation
        generation_thread = threading.Thread(target=self.model.generate, kwargs=generation_kwargs)
        generation_thread.start()

        # Initialize an empty string to build the response
        generated_text = ""

        # Yield tokens as they're generated
        for token in streamer:
            generated_text += token
            yield token, generated_text
    def get_logits(self, tokens):
        """Returns the logits for the given tokens."""
        with t.no_grad():
            logits = self.model(tokens).logits
        return logits

    def reset_all(self):
        for layer in self.model.model.layers:
            layer.reset()
