#!/bin/bash
# =============================================================================
# Experiment Pipeline Launcher
# =============================================================================
#
# This script launches the workhorse processes that coordinate the full
# training → generation → scoring pipeline. All three can run in parallel
# because each stage checks if previous stages are complete before proceeding.
#
# How it works:
# - exp_train_workhorse.py: Monitors GPUs, launches training when available
# - exp_generations_workhorse.py: Waits for training, launches generation
# - exp_scoring_workhorse.py: Waits for generation, runs API-based scoring
#
# Coordination is done via file existence checks and lock files, so all three
# can safely run simultaneously.
#
# =============================================================================

# Create logs directory
mkdir -p logs

# -----------------------------------------------------------------------------
# Option 1: Run all stages in parallel (recommended)
# Each workhorse will wait for its dependencies automatically
# -----------------------------------------------------------------------------

# Training (uses GPUs 0-7)
PYTHONUNBUFFERED=1 nohup python scripts/exp_train_workhorse.py \
    > logs/workhorse_train.log 2>&1 &
echo "Started training workhorse (PID: $!)"

# Generation (uses available GPUs, waits for training)
PYTHONUNBUFFERED=1 nohup python scripts/exp_generations_workhorse.py \
    --epoch 20 \
    > logs/workhorse_generate_ep20.log 2>&1 &
echo "Started generation workhorse (PID: $!)"

# Scoring (API calls only, waits for generation)
# Can run all three scoring tasks in parallel since they use different lock files
PYTHONUNBUFFERED=1 nohup python scripts/exp_scoring_workhorse.py \
    --epoch 20 --task coherence \
    > logs/workhorse_score_coherence_ep20.log 2>&1 &
echo "Started coherence scoring workhorse (PID: $!)"

PYTHONUNBUFFERED=1 nohup python scripts/exp_scoring_workhorse.py \
    --epoch 20 --task relationship \
    > logs/workhorse_score_relationship_ep20.log 2>&1 &
echo "Started relationship scoring workhorse (PID: $!)"

PYTHONUNBUFFERED=1 nohup python scripts/exp_scoring_workhorse.py \
    --epoch 20 --task pairwise \
    > logs/workhorse_score_pairwise_ep20.log 2>&1 &
echo "Started pairwise scoring workhorse (PID: $!)"

echo ""
echo "All workhorse processes started. Check logs/ for progress."
echo "Use 'ps aux | grep workhorse' to see running processes."
echo "Use 'tail -f logs/workhorse_*.log' to monitor progress."

# -----------------------------------------------------------------------------
# Option 2: Run individual stages manually
# Uncomment and modify as needed
# -----------------------------------------------------------------------------

# # Training only
# python scripts/exp_train_workhorse.py

# # Generation only (for specific epoch)
# python scripts/exp_generations_workhorse.py --epoch 20

# # Scoring only (for specific task and epoch)
# python scripts/exp_scoring_workhorse.py --epoch 20 --task coherence
# python scripts/exp_scoring_workhorse.py --epoch 20 --task relationship
# python scripts/exp_scoring_workhorse.py --epoch 20 --task pairwise

# # Or run all scoring tasks at once
# python scripts/exp_scoring_workhorse.py --epoch 20 --task all
