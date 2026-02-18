"""Shared utilities for OpenAI Batch API scripts."""

import json
from pathlib import Path
from typing import Dict, List

import pandas as pd


def load_jsonl(file_path: str | Path) -> pd.DataFrame:
    """Load JSONL file into a pandas DataFrame."""
    data = []
    with open(file_path, "r") as f:
        for line in f:
            data.append(json.loads(line))
    return pd.DataFrame(data)


def save_jsonl(df: pd.DataFrame, file_path: str | Path) -> None:
    """Save DataFrame to JSONL file."""
    df.to_json(file_path, orient="records", lines=True)


def simple_conv_history_processer(conv_history: List[Dict[str, str]]) -> str:
    """Process a conversation history (list of turns) into a single string."""
    processed_conv_history = ""
    for turn in conv_history:
        processed_conv_history += f"{turn['role']}: {turn['content']}\n"
    return processed_conv_history.strip()


def write_batch_file(lines: List[dict], output_path: Path) -> int:
    """Write batch request lines to a JSONL file.

    Returns the number of lines written.
    """
    with open(output_path, "w", encoding="utf-8") as f:
        for line_dict in lines:
            f.write(json.dumps(line_dict))
            f.write("\n")
    return len(lines)
