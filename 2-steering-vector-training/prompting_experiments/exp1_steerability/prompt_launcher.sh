#!/bin/bash
# Prompt launcher for steerability experiments
# Compares prompting baselines (Claude/GPT) against steering vectors

# =============================================================================
# Step 1: Generate prompting baseline responses
# =============================================================================

# Generate responses at all 7 levels (matches steering vector multipliers -1.5 to +1.5)
# python eval_prompting_generation.py claude
# python eval_prompting_generation.py gpt

# Save an example prompt for inspection
# python eval_prompting_generation.py claude --save-example --limit 1

# =============================================================================
# Step 2: Rate prompting baseline responses (coherence and relationship-seeking)
# =============================================================================

# Note: eval_rating.py is in scripts/ folder, run from 2-steering-vector-training/
# cd ../..
# python scripts/eval_rating.py --model_name claude_levels_7 --task coherence --prompting_baseline --epoch 0 --layer 0
# python scripts/eval_rating.py --model_name claude_levels_7 --task relationship --prompting_baseline --epoch 0 --layer 0
# python scripts/eval_rating.py --model_name gpt_levels_7 --task coherence --prompting_baseline --epoch 0 --layer 0
# python scripts/eval_rating.py --model_name gpt_levels_7 --task relationship --prompting_baseline --epoch 0 --layer 0

# =============================================================================
# Step 3: Rank prompting baselines against steering vectors
# =============================================================================

# Compare Claude prompting vs Llama steering vectors
# python eval_ranking_baseline.py \
#     --model_name meta-llama/Llama-3.1-70B-Instruct \
#     --baseline_model claude \
#     --epoch 10 \
#     --layer 31

# Compare GPT prompting vs Llama steering vectors
# python eval_ranking_baseline.py \
#     --model_name meta-llama/Llama-3.1-70B-Instruct \
#     --baseline_model gpt \
#     --epoch 10 \
#     --layer 31
