#!/usr/bin/env python
import argparse
import subprocess
import time
from pathlib import Path

import pandas as pd

from exp_config import N_PAIRWISE_RANKINGS, N_RATINGS, ModelConfig
from utils.workhorse_utils import (
    acquire_lock,
    release_lock,
    load_state_file,
    save_state_file,
    check_process_running,
    check_generation_file,
)

# Default task name for behavior-specific filenames (can be overridden via --task-name)
TASK_NAME = "relationship"


#
# Task-specific lock/state functionality with epoch awareness
#
def get_lock_file(task, epoch):
    """Return lock file path for specific task and epoch."""
    return Path(f"running_scoring_{task}_ep{epoch}.lock")


def get_state_file(task, epoch):
    """Return state file path for specific task and epoch."""
    return Path(f"running_scoring_{task}_ep{epoch}.json")


def acquire_task_lock(task, epoch):
    """Get exclusive lock for a specific task and epoch."""
    return acquire_lock(get_lock_file(task, epoch))


def get_running_process(task, epoch):
    """Get currently running scoring process for a specific task and epoch."""
    return load_state_file(get_state_file(task, epoch))


def update_running_process(task, epoch, process_info):
    """Update running process file for a specific task and epoch."""
    save_state_file(get_state_file(task, epoch), process_info)

def check_score_file(short_model: str, layer: int, task: str, epoch: int) -> tuple[bool, int]:
    """Check if scoring file exists and count rows"""
    file_mapping = {
        "coherence": (
            f"vector_evals/{short_model}/layer{layer}/coherence_scores_ep{epoch}.jsonl",
            N_RATINGS,
        ),
        "relationship": (
            f"vector_evals/{short_model}/layer{layer}/{TASK_NAME}_scores_ep{epoch}.jsonl",
            N_RATINGS,
        ),
        "pairwise": (
            f"vector_evals/{short_model}/layer{layer}/pairwise_{TASK_NAME}_scores_ep{epoch}.jsonl",
            N_PAIRWISE_RANKINGS,
        ),
    }

    filepath, expected_rows = file_mapping[task]
    score_file = Path(filepath)

    if not score_file.exists():
        return False, 0
    try:
        df = pd.read_json(score_file, lines=True)
        return True, len(df)
    except:
        return False, 0

def launch_scoring(model_name: str, layer: int, task: str, epoch: int) -> int:
    """Launch scoring for a specific task and return process ID."""
    if task in ["coherence", "relationship"]:
        cmd = f"python scripts/eval_rating.py --model_name {model_name} --layer {layer} --epoch {epoch} --task {task}"
    else:  # pairwise
        cmd = f"python scripts/eval_ranking.py --model_name {model_name} --layer {layer} --epoch {epoch}"

    log_file = f"logs/log_score_{task}_{model_name.split('/')[-1]}_layer{layer}_ep{epoch}.log"

    # Ensure logs directory exists
    Path("logs").mkdir(exist_ok=True)

    print(f"Launching: {cmd}")
    print(f"Log file: {log_file}")

    with open(log_file, "w") as f:
        process = subprocess.Popen(cmd, shell=True, stdout=f, stderr=subprocess.STDOUT)
        return process.pid

def check_and_manage_tasks(tasks=None, epoch=20):
    """
    Check all tasks and manage running processes
    If tasks is None, check all three task types
    Returns a summary of what was done
    """
    if tasks is None:
        tasks = ["coherence", "relationship", "pairwise"]

    tasks_launched = []

    for task in tasks:
        print(f"\nChecking {task} scoring status for epoch {epoch}...")

        # Get lock before checking/updating running process
        lock_fd = acquire_task_lock(task, epoch)
        running_process = get_running_process(task, epoch)

        # Check if running process is still active
        if running_process:
            model, layer, pid = (
                running_process["model"],
                running_process["layer"],
                running_process["pid"],
            )
            if not check_process_running(pid):
                print(f"Process for {task} on {model} layer {layer} epoch {epoch} is no longer running")
                running_process = None
                update_running_process(task, epoch, None)
            else:
                print(f"Process for {task} is running: {model} layer {layer} epoch {epoch} (PID {pid})")

        # Build queue of work to be done
        pending_generation = []
        scoring_queue = []
        complete = []
        in_progress = []

        expected_rows = N_PAIRWISE_RANKINGS if task == "pairwise" else N_RATINGS

        # Check all experiments
        for short_name, layer, _ in ModelConfig.get_experiments():
            # Check generation status
            has_gen, gen_rows = check_generation_file(short_name, layer, epoch)

            if not has_gen or gen_rows < N_RATINGS:
                pending_generation.append((short_name, layer))
                continue

            # Check scoring status
            has_score, score_rows = check_score_file(short_name, layer, task, epoch)

            if not has_score:
                full_name = next(
                    name
                    for name in ModelConfig.MODELS
                    if name.split("/")[-1] == short_name
                )
                scoring_queue.append((full_name, layer))
                print(f"  * {short_name} Layer {layer}: 0/{expected_rows} rows (needs scoring)")
            elif score_rows < expected_rows:
                print(f"  * {short_name} Layer {layer}: {score_rows}/{expected_rows} rows (in progress)")
                in_progress.append((short_name, layer))
            else:
                print(f"  * {short_name} Layer {layer}: complete ({score_rows} rows)")
                complete.append((short_name, layer))

        # Launch next scoring job if nothing is running
        if not running_process and scoring_queue:
            model, layer = scoring_queue[0]
            print(
                f"\nStarting {task} scoring for {model.split('/')[-1]} Layer {layer} epoch {epoch}"
            )

            pid = launch_scoring(model, layer, task, epoch)
            running_process = {"model": model, "layer": layer, "pid": pid}
            update_running_process(task, epoch, running_process)
            tasks_launched.append(task)

        # Release lock after updates
        release_lock(lock_fd)

        print(f"\nSummary for {task} (epoch {epoch}):")
        print(f"- Waiting for generation: {len(pending_generation)}")
        print(f"- Need {task} scoring: {len(scoring_queue)}")
        print(f"- {task} scoring in progress: {len(in_progress)}")
        print(f"- {task} scoring complete: {len(complete)}")

    return tasks_launched

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--task",
        type=str,
        choices=["coherence", "relationship", "pairwise", "all"],
        default="all",
        help="Which scoring task to run (default: all)",
    )
    parser.add_argument(
        "--epoch",
        type=int,
        default=20,
        help="Epoch number to use for scoring (default: 20)",
    )
    parser.add_argument(
        "--task-name",
        type=str,
        default="relationship",
        help="Task name for behavior-specific filenames (default: relationship)",
    )
    args = parser.parse_args()

    # Make task_name available globally for check_score_file
    global TASK_NAME
    TASK_NAME = args.task_name.replace("-", "_")

    print(f"Starting Scoring Manager for epoch {args.epoch}...")

    tasks_to_check = ["coherence", "relationship", "pairwise"] if args.task == "all" else [args.task]

    while True:
        print("\n" + "="*50)
        print(f"Checking status at {time.strftime('%Y-%m-%d %H:%M:%S')}")
        print("="*50)

        # Run task manager for specified tasks and epoch
        tasks_launched = check_and_manage_tasks(tasks_to_check, args.epoch)

        # Calculate if we're completely done
        all_tasks_done = True
        for task in tasks_to_check:
            # Get lock before checking running process
            lock_fd = acquire_lock(task, args.epoch)
            running_process = get_running_process(task, args.epoch)
            release_lock(lock_fd)

            if running_process:
                all_tasks_done = False

        # Check if anything is still pending generation
        pending_generation = []
        for short_name, layer, _ in ModelConfig.get_experiments():
            has_gen, gen_rows = check_generation_file(short_name, layer, args.epoch)
            if not has_gen or gen_rows < N_RATINGS:
                pending_generation.append((short_name, layer))

        if pending_generation:
            all_tasks_done = False

        # Exit if everything is done
        if all_tasks_done:
            print(f"\nAll specified scoring tasks for epoch {args.epoch} are complete! Shutting down.")
            break

        if tasks_launched:
            # If we just launched tasks, wait a bit less time before checking again
            wait_time = 300  # 5 minutes
        else:
            wait_time = 600  # 10 minutes

        print(f"\nWaiting {wait_time//60} minutes before next check...")
        time.sleep(wait_time)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nShutting down...")
