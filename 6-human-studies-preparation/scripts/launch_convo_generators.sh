#!/bin/bash
# Generate prepopulated multi-turn conversations with steered Llama

python calibration_generate_prepopulated_convos.py --model-config 70B-layer31-ep10 --gpu-id 0
