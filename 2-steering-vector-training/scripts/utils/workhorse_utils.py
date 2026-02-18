"""
Shared utilities for workhorse scripts (train, generations, scoring).
"""
import fcntl
import json
import subprocess
from pathlib import Path

import pandas as pd


def check_process_running(pid: int) -> bool:
    """Check if a process is still running."""
    try:
        subprocess.check_output(["ps", "-p", str(pid)])
        return True
    except subprocess.CalledProcessError:
        return False


def acquire_lock(lock_file: Path):
    """Get exclusive lock on a lock file."""
    lock_fd = open(lock_file, "w")
    fcntl.flock(lock_fd, fcntl.LOCK_EX)
    return lock_fd


def release_lock(lock_fd):
    """Release lock on state file."""
    fcntl.flock(lock_fd, fcntl.LOCK_UN)
    lock_fd.close()


def load_state_file(state_file: Path):
    """Load state from JSON file."""
    if not state_file.exists():
        return None
    try:
        with open(state_file, "r") as f:
            return json.load(f)
    except:
        return None


def save_state_file(state_file: Path, state):
    """Save state to JSON file."""
    with open(state_file, "w") as f:
        json.dump(state, f)


def check_generation_file(short_model: str, layer: int, epoch: int) -> tuple:
    """
    Check if generation file exists and count rows.
    Returns (exists: bool, num_rows: int).
    """
    gen_file = Path(f"vector_evals/{short_model}/layer{layer}/generations_ep{epoch}.jsonl")
    if not gen_file.exists():
        return False, 0
    try:
        df = pd.read_json(gen_file, lines=True)
        return True, len(df)
    except:
        return False, 0


def check_gpu_available(gpu_id: int, threshold_mb: int = 1000) -> bool:
    """Check if specified GPU has less than threshold_mb memory used."""
    try:
        result = subprocess.run(
            ["nvidia-smi", "--query-gpu=memory.used", "--format=csv,noheader,nounits"],
            capture_output=True,
            text=True,
        )
        used_memory = int(result.stdout.strip().split("\n")[gpu_id])
        return used_memory < threshold_mb
    except:
        return False


def get_available_gpus() -> list:
    """Get list of available GPU IDs."""
    try:
        result = subprocess.run(
            ["nvidia-smi", "--query-gpu=memory.used", "--format=csv,noheader,nounits"],
            capture_output=True,
            text=True,
        )
        gpu_list = result.stdout.strip().split("\n")
        return list(range(len(gpu_list)))
    except:
        return [0]  # Default to single GPU if nvidia-smi fails
