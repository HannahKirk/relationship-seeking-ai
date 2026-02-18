#!/bin/bash
# Persona Stability Experiment Launcher
# Generates conversations and scores them for persona stability analysis

# Available themes: casual_chat, ethics_chat, learning_chat, technical_chat, friendship_chat

# =============================================================================
# Step 1: Generate persona stability conversations
# =============================================================================

# Claude
python generate_persona_attacks_gpu.py \
  --model-type claude \
  --model-config "claude-3-7-sonnet" \
  --theme casual_chat \
  --output-dir levels_7 \
  --gpu-id 0

# GPT
python generate_persona_attacks_gpu.py \
  --model-type gpt \
  --model-config "gpt-4o" \
  --theme casual_chat \
  --output-dir levels_7 \
  --gpu-id 0

# Llama with steering vectors (runs all themes)
for theme in casual_chat ethics_chat learning_chat technical_chat friendship_chat; do
  python generate_persona_attacks_gpu.py \
    --model-type llama \
    --model-config 70B-layer31-ep10 \
    --theme $theme \
    --output-dir vector_results \
    --gpu-id 0
done

# =============================================================================
# Step 2: Score persona stability responses
# =============================================================================

python score_persona_attacks.py \
  --model claude \
  --themes casual_chat ethics_chat learning_chat technical_chat friendship_chat

python score_persona_attacks.py \
  --model gpt \
  --themes casual_chat ethics_chat learning_chat technical_chat friendship_chat

python score_persona_attacks.py \
  --model llama_70B-layer31-ep10 \
  --themes casual_chat ethics_chat learning_chat technical_chat friendship_chat
