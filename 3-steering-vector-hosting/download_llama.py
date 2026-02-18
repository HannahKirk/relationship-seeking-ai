from transformers import AutoModelForCausalLM, AutoTokenizer
import torch

cache_dir = "model_cache"
model = AutoModelForCausalLM.from_pretrained(
    "meta-llama/Llama-3.1-70B-Instruct",
    torch_dtype=torch.bfloat16,
    cache_dir=cache_dir,
)

tokenizer = AutoTokenizer.from_pretrained(
    "meta-llama/Llama-3.1-70B-Instruct",
    cache_dir=cache_dir,
)

print(f"Model downloaded to {cache_dir}")
