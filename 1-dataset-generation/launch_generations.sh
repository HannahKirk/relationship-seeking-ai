#!/bin/bash
# Launch all generation jobs in parallel
# Run from 1-dataset-generation/ directory

mkdir -p logs

# API models
nohup python scripts/generate_cases.py claude > logs/claude_generation.out 2>&1 &
nohup python scripts/generate_cases.py gpt > logs/gpt_generation.out 2>&1 &

# Self-hosted model (requires vLLM server running)
nohup python scripts/generate_cases_vllm.py --model llama-3.1-70b > logs/llama70_generation.out 2>&1 &

echo "Generation jobs launched. Check logs/ for progress."
