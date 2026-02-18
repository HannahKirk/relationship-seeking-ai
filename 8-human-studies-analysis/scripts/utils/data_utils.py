#!/usr/bin/env python3
"""
Data loading utilities for human studies analysis.

Loads data from data/human_study/ which contains:
- calibration_study/*.jsonl
- main_studies/*.jsonl
- main_studies/participant_characteristics/*.jsonl
"""

import os
from pathlib import Path

import pandas as pd

# Default paths
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent.parent  # relationship-seeking-ai/
DATA_DIR = PROJECT_ROOT / "data" / "human_study"


def load_jsonl(file_path):
    """Load a single JSONL file as a DataFrame."""
    return pd.read_json(file_path, lines=True)


def load_study_data(task_name, study="main_studies", data_dir=None):
    """
    Simple loader that returns a single DataFrame.

    Args:
        task_name: Name of the task file (without .jsonl extension)
        study: "main_studies" or "calibration_study"
        data_dir: Optional override for data directory

    Returns:
        DataFrame with all data (study_id is a column if multiple studies)
    """
    if data_dir is None:
        data_dir = DATA_DIR

    data_dir = Path(data_dir)
    file_path = data_dir / study / f"{task_name}.jsonl"

    if not file_path.exists():
        raise FileNotFoundError(f"File not found: {file_path}")

    df = load_jsonl(file_path)
    print(f"Loaded: {file_path.name} ({len(df)} rows)")
    return df


def save_data(df, filename, output_dir, fmt="csv", relative_to=None):
    """
    Save dataframe and print the relative path.

    Args:
        df: DataFrame to save
        filename: Name of the file (without extension)
        output_dir: Directory to save to (Path object)
        fmt: Format - "csv" or "jsonl"
        relative_to: Path to compute relative path from (e.g., notebook directory).
                     If None, prints absolute path.

    Returns:
        Path to saved file
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / f"{filename}.{fmt}"

    if fmt == "csv":
        df.to_csv(output_path, index=False)
    elif fmt == "jsonl":
        df.to_json(output_path, orient="records", lines=True)
    else:
        raise ValueError(f"Unsupported format: {fmt}. Use 'csv' or 'jsonl'.")

    if relative_to is not None:
        rel_path = os.path.relpath(output_path, Path(relative_to).resolve())
        print(f"Saved: {rel_path}")
    else:
        print(f"Saved: {output_path}")

    return output_path
