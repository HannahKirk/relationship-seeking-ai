"""Shared utility functions for dataset generation scripts."""

import json
import logging
import os


def setup_logging():
    """Set up logging configuration."""
    logging.basicConfig(
        level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
    )


def load_jsonl_file(filepath):
    """Load a JSONL file and return list of dicts."""
    data = []
    with open(filepath, "r", encoding="utf-8") as f:
        for line in f:
            if line.strip():
                data.append(json.loads(line))
    return data


def load_json_file(filepath):
    """Load a JSON file."""
    with open(filepath, "r", encoding="utf-8") as f:
        return json.load(f)


def append_to_jsonl(file_path, data):
    """Append data to a JSONL file."""
    with open(file_path, "a", encoding="utf-8") as f:
        f.write(json.dumps(data, ensure_ascii=False) + "\n")
    logging.info(f"Appended data to {file_path}")


def save_jsonl(file_path, data_list):
    """Write a list of dicts to a JSONL file (overwrite)."""
    os.makedirs(os.path.dirname(file_path), exist_ok=True)
    with open(file_path, "w", encoding="utf-8") as f:
        for item in data_list:
            f.write(json.dumps(item, ensure_ascii=False) + "\n")
    logging.info(f"Saved {len(data_list)} items to {file_path}")


def get_existing_generations(file_path):
    """Load existing generations and return set of (metaprompt_id, model) pairs."""
    existing = set()
    if not os.path.exists(file_path):
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, "w", encoding="utf-8") as f:
            pass
        return existing
    with open(file_path, "r", encoding="utf-8") as f:
        for line in f:
            if line.strip():
                try:
                    data = json.loads(line)
                    existing.add((data["metaprompt_id"], data["model"]))
                except (json.JSONDecodeError, KeyError) as e:
                    logging.warning(f"Error parsing line in {file_path}: {e}")
    return existing


def calculate_cost(model, prompt_tokens, completion_tokens):
    """Calculate estimated cost in USD based on token usage."""
    pricing = {
        "claude-3-7-sonnet-20250219": {"prompt": 3.00, "completion": 15.00},
        "gpt-4o-2024-08-06": {"prompt": 5.00, "completion": 15.00}
    }
    if model not in pricing:
        logging.warning(f"No pricing information for model {model}")
        return None
    prompt_cost = (prompt_tokens / 1_000_000) * pricing[model]["prompt"]
    completion_cost = (completion_tokens / 1_000_000) * pricing[model]["completion"]
    return {
        "prompt_cost": round(prompt_cost, 2),
        "completion_cost": round(completion_cost, 2),
        "total_cost": round(prompt_cost + completion_cost, 2)
    }
