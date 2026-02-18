#!/usr/bin/env bash

# Adjust if desired:
MODEL_NAME="Llama-3.1-70B-Instruct"
EPOCHS=(10 15 20)
LAYERS=(9 15 21 31 33 35 27 29 41)


for LAYER in "${LAYERS[@]}"; do
  for EPOCH in "${EPOCHS[@]}"; do

    echo "Creating pairwise batch for: model=$MODEL_NAME epoch=$EPOCH layer=$LAYER"
    python scripts/create_ranking_batch.py \
      --model_name "$MODEL_NAME" \
      --epoch "$EPOCH" \
      --layer "$LAYER"

    echo "Creating coherence batch for: model=$MODEL_NAME epoch=$EPOCH layer=$LAYER"
    python scripts/create_rating_batch.py \
      --model_name "$MODEL_NAME" \
      --epoch "$EPOCH" \
      --layer "$LAYER" \
      --task coherence

    echo "Creating relationship batch for: model=$MODEL_NAME epoch=$EPOCH layer=$LAYER"
    python scripts/create_rating_batch.py \
      --model_name "$MODEL_NAME" \
      --epoch "$EPOCH" \
      --layer "$LAYER" \
      --task relationship

  done
done
