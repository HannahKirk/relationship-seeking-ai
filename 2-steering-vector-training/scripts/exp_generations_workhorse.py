#!/usr/bin/env python
import argparse
import subprocess
import time
from pathlib import Path

from exp_config import N_RATINGS, ModelConfig
from utils.workhorse_utils import (
    acquire_lock,
    release_lock,
    load_state_file,
    save_state_file,
    check_process_running,
    check_generation_file,
    check_gpu_available,
    get_available_gpus,
)

# File to track running generations
LOCK_FILE = Path("running_generations.lock")
STATE_FILE = Path("running_generations.json")


def get_running_generations():
    """Get currently running generations."""
    state = load_state_file(STATE_FILE)
    return state if state else {}


def update_running_generations(running_gens):
    """Update running generations file."""
    save_state_file(STATE_FILE, running_gens)


def clean_dead_processes(running_gens):
    """Remove any completed processes from running generations."""
    return {
        gpu_id: (model, layer, pid)
        for gpu_id, (model, layer, pid) in running_gens.items()
        if check_process_running(pid)
    }


def check_training_done(short_model: str, layer: int, epochs: int, target_epoch: int) -> bool:
    """Check if training is complete by looking for the vector file."""
    vector_path = Path(
        f"vector/relationship-seeking_{short_model}/layer{layer}/vec_ep{target_epoch}_layer{layer}_gpu3.pt"
    )
    return vector_path.exists()


def launch_generation(model_name: str, layer: int, gpu_id: int, target_epoch: int) -> int:
    """Launch generation for a model/layer combo on specified GPU. Returns process ID."""
    cmd = f"""CUDA_VISIBLE_DEVICES={gpu_id} python scripts/eval_generation.py \
        --model_name {model_name} \
        --layer {layer} \
        --epoch {target_epoch} \
        --behavior relationship-seeking"""

    log_file = f"logs/log_generate_{model_name.split('/')[-1]}_layer{layer}_ep{target_epoch}.log"

    with open(log_file, "w") as f:
        process = subprocess.Popen(
            cmd, shell=True, stdout=f, stderr=subprocess.STDOUT, executable="/bin/bash"
        )
        return process.pid


def main():
    parser = argparse.ArgumentParser(description="Multi-GPU Generation Manager")
    parser.add_argument("--epoch", type=int, default=20, help="Target epoch to evaluate")
    parser.add_argument("--force", action="store_true", help="Force rerun of incomplete generations")
    args = parser.parse_args()

    target_epoch = args.epoch
    force_rerun = args.force

    print(f"Starting Multi-GPU Generation Checker for epoch {target_epoch}...")
    print(f"Expected ratings per model-layer: {N_RATINGS}")
    if force_rerun:
        print("Force rerun enabled - will relaunch incomplete generations")

    available_gpus = get_available_gpus()
    print(f"Found {len(available_gpus)} GPUs: {available_gpus}")

    while True:
        print("\nChecking experiment status...")

        # Get lock before checking/updating running generations
        lock_fd = acquire_lock(LOCK_FILE)
        running_gens = get_running_generations()

        # Clean up completed processes
        running_gens = clean_dead_processes(running_gens)
        update_running_generations(running_gens)

        pending_training = []
        incomplete_generations = []
        complete = []
        running = []

        # Check all experiments
        for short_name, layer, epochs in ModelConfig.get_experiments():
            # Skip if this generation is already running
            is_running = any(
                model.split("/")[-1] == short_name and l == layer
                for model, l, _ in running_gens.values()
            )
            if is_running:
                running.append((short_name, layer))
                continue

            # First check if training is done
            if not check_training_done(short_name, layer, epochs, target_epoch):
                print(f"- {short_name} Layer {layer}: Training not complete")
                pending_training.append((short_name, layer))
                continue

            # Check generation file
            has_file, num_rows = check_generation_file(short_name, layer, target_epoch)

            if not has_file or (force_rerun and num_rows < N_RATINGS):
                full_name = next(
                    name
                    for name in ModelConfig.MODELS
                    if name.split("/")[-1] == short_name
                )

                if not has_file:
                    print(f"- {short_name} Layer {layer}: No generation file")
                else:
                    print(f"- {short_name} Layer {layer}: Incomplete ({num_rows}/{N_RATINGS} rows)")

                incomplete_generations.append((full_name, layer))
            elif num_rows < N_RATINGS:
                print(f"- {short_name} Layer {layer}: Incomplete ({num_rows}/{N_RATINGS} rows)")
                full_name = next(
                    name
                    for name in ModelConfig.MODELS
                    if name.split("/")[-1] == short_name
                )
                incomplete_generations.append((full_name, layer))
            else:
                print(f"- {short_name} Layer {layer}: Complete ({num_rows} rows)")
                complete.append((short_name, layer))

        print(f"\nSummary:")
        print(f"- Pending training: {len(pending_training)}")
        print(f"- Incomplete generations: {len(incomplete_generations)}")
        print(f"- Currently running: {len(running)}")
        print(f"- Complete: {len(complete)}")

        # Try to launch generations on available GPUs
        for gpu_id in available_gpus:
            # Skip if GPU is already running something
            if gpu_id in running_gens or not check_gpu_available(gpu_id):
                continue

            # Get next generation to run
            if not incomplete_generations:
                break

            model, layer = incomplete_generations.pop(0)
            print(
                f"\nLaunching generation for {model.split('/')[-1]} Layer {layer} on GPU {gpu_id}"
            )

            # Launch and record the process
            pid = launch_generation(model, layer, gpu_id, target_epoch)
            running_gens[gpu_id] = (model, layer, pid)
            update_running_generations(running_gens)

        # Release lock after updates
        release_lock(lock_fd)

        # Exit if everything is done
        if not pending_training and not incomplete_generations and not running:
            print("\nAll training and generations are complete! Shutting down.")
            break

        print("\nWaiting 20 minutes before next check...")
        time.sleep(1200)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nShutting down...")
