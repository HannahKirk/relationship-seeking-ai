#!/bin/bash
# Launch vLLM server for self-hosted model generation
#
# Prerequisites:
#   pip install vllm
#   export HF_TOKEN=your-token-here  # For gated models like Llama
#
# Customize the variables below for your setup:

# Model configuration
MODEL_NAME="meta-llama/Llama-3.1-70B-Instruct"  # HuggingFace model ID
TENSOR_PARALLEL_SIZE=8                           # Number of GPUs (adjust based on your hardware)
MAX_MODEL_LEN=30000                              # Max context length
PORT=8000                                        # Server port

# Optional: Custom cache directory (useful for multi-disk setups)
# Uncomment and modify if needed:
# export HF_HOME=/path/to/model_cache
# DOWNLOAD_DIR=/path/to/model_cache

# Launch vLLM server with OpenAI-compatible API
python -m vllm.entrypoints.openai.api_server \
    --model "$MODEL_NAME" \
    --tensor-parallel-size "$TENSOR_PARALLEL_SIZE" \
    --dtype float16 \
    --max-model-len "$MAX_MODEL_LEN" \
    --port "$PORT" \
    --guided-decoding-backend lm-format-enforcer \
    ${DOWNLOAD_DIR:+--download-dir "$DOWNLOAD_DIR"}