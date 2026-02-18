#!/usr/bin/env python3
"""
Create train/test splits for DPO training.

This script:
1. Loads scored cases and merges with processed data
2. Filters out low-scoring cases (score < threshold)
3. Explodes conversations into DPO format (chosen/rejected pairs)
4. Creates train/test split ensuring no prompt_id overlap
5. Saves to shared data/synthetic_steering_data/ folder
"""

import argparse
import json
import logging
import os
import sys

import numpy as np
import pandas as pd
from sklearn.model_selection import GroupShuffleSplit

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from utils.generation_utils import setup_logging


def load_and_merge_data(processed_path, scored_path, metaprompts_path):
    """Load and merge processed cases with scores and metadata."""
    # Load processed cases
    df = pd.read_json(processed_path, lines=True)
    print(f"Loaded {len(df)} processed cases")

    # Load scores
    scored_cases = pd.read_json(scored_path, lines=True)
    print(f"Loaded {len(scored_cases)} scored cases")

    # Merge scores
    df = df.merge(
        scored_cases[["prompt_id", "score", "response"]],
        on="prompt_id",
        how="left",
    )

    # Load and merge metaprompts if available
    if os.path.exists(metaprompts_path):
        metaprompts = pd.read_json(metaprompts_path, lines=True)
        df = df.merge(metaprompts, on="metaprompt_id", how="left")
        print(f"Merged with {len(metaprompts)} metaprompts")

    return df


def filter_by_score(df, min_score=6):
    """Filter out cases with scores below threshold."""
    before_count = len(df)
    df = df[df["score"] >= min_score].copy()
    after_count = len(df)

    drop_pct = (before_count - after_count) / before_count * 100
    print(f"Filtered by score >= {min_score}:")
    print(f"  Before: {before_count}, After: {after_count}")
    print(f"  Dropped: {drop_pct:.1f}%, Retained: {100-drop_pct:.1f}%")

    return df


def explode_rows_for_dpo(df):
    """
    Explode each row to create DPO training pairs.

    For first turns: creates one row (no history to differ on).
    For later turns: creates two rows (one with chosen history, one with rejected).
    """
    new_rows = []

    for _, row in df.iterrows():
        target_conv = row["target_conv_arr"]
        antitarget_conv = row["antitarget_conv_arr"]

        # Get all assistant turn indices
        assistant_indices = [
            i for i, turn in enumerate(target_conv) if turn["role"] == "assistant"
        ]

        # Process each assistant turn
        for assistant_idx in assistant_indices:
            if assistant_idx == assistant_indices[0]:
                # First turn: single row (no history difference)
                new_rows.append({
                    "prompt_id": row["prompt_id"],
                    "metaprompt_id": row["metaprompt_id"],
                    "assistant_turn_idx": assistant_idx,
                    "prompt_arr": target_conv[:assistant_idx],
                    "chosen_arr": [target_conv[assistant_idx]],
                    "rejected_arr": [antitarget_conv[assistant_idx]],
                    "history_type": "NA",
                })
            else:
                # Later turns: two rows with different histories
                for history_type in ["chosen", "rejected"]:
                    history_conv = target_conv if history_type == "chosen" else antitarget_conv
                    prompt_arr = history_conv[:assistant_idx]

                    new_rows.append({
                        "prompt_id": row["prompt_id"],
                        "metaprompt_id": row["metaprompt_id"],
                        "assistant_turn_idx": assistant_idx,
                        "prompt_arr": prompt_arr,
                        "chosen_arr": [target_conv[assistant_idx]],
                        "rejected_arr": [antitarget_conv[assistant_idx]],
                        "history_type": history_type,
                    })

    result_df = pd.DataFrame(new_rows)

    print(f"\nAfter exploding conversations:")
    print(f"  Total rows: {len(result_df)}")
    print(f"  History type distribution:")
    print(result_df["history_type"].value_counts().to_string())

    return result_df


def create_train_test_split(df, train_size=0.95, random_state=42):
    """
    Create train/test split where all rows with same prompt_id stay together.
    For test set, sample one row per conversation.
    """
    # Split by prompt_id groups
    splitter = GroupShuffleSplit(
        n_splits=1, train_size=train_size, random_state=random_state
    )
    train_idx, test_idx = next(splitter.split(df, groups=df["prompt_id"]))

    train_df = df.iloc[train_idx].copy()
    test_df_full = df.iloc[test_idx].copy()

    # Sample one row per prompt_id in test set
    test_prompt_ids = test_df_full["prompt_id"].unique()
    test_samples = []
    counter = 0

    for pid in test_prompt_ids:
        pid_rows = test_df_full[test_df_full["prompt_id"] == pid]
        later_turns = pid_rows[pid_rows["assistant_turn_idx"] > 1]

        if len(later_turns) > 0:
            # For multi-turn, sample from later turns
            sampled_turn = (
                later_turns["assistant_turn_idx"]
                .sample(n=1, random_state=random_state)
                .iloc[0]
            )
            sampled_rows = later_turns[later_turns["assistant_turn_idx"] == sampled_turn]

            # Alternate between chosen/rejected history
            if len(sampled_rows) == 2:
                history = "chosen" if counter % 2 == 0 else "rejected"
                sampled_row = sampled_rows[sampled_rows["history_type"] == history]
            else:
                sampled_row = sampled_rows.sample(n=1, random_state=random_state)
            counter += 1
        else:
            # Single-turn: take the only row
            sampled_row = pid_rows.sample(n=1, random_state=random_state)

        test_samples.append(sampled_row)

    test_df = pd.concat(test_samples, ignore_index=True)

    # Shuffle training set
    train_df = train_df.sample(frac=1, random_state=random_state).reset_index(drop=True)

    # Add entry_id
    train_df["entry_id"] = train_df.index
    test_df["entry_id"] = test_df.index

    # Print statistics
    print(f"\nTrain/test split:")
    print(f"  Total conversations: {df['prompt_id'].nunique()}")
    print(f"  Train conversations: {train_df['prompt_id'].nunique()} ({train_df['prompt_id'].nunique()/df['prompt_id'].nunique():.1%})")
    print(f"  Test conversations: {test_df['prompt_id'].nunique()} ({test_df['prompt_id'].nunique()/df['prompt_id'].nunique():.1%})")
    print(f"  Train samples: {len(train_df)}, Test samples: {len(test_df)}")

    # Verify no overlap
    train_ids = set(train_df["prompt_id"])
    test_ids = set(test_df["prompt_id"])
    assert len(train_ids & test_ids) == 0, "Train/test overlap detected!"

    return train_df, test_df


def main():
    parser = argparse.ArgumentParser(description="Create train/test splits for DPO")
    parser.add_argument(
        "--test",
        action="store_true",
        help="Test mode: process limited data, print results, no files written",
    )
    parser.add_argument(
        "--processed-path",
        default="data/processed_cases/processed_cases.jsonl",
        help="Path to processed cases",
    )
    parser.add_argument(
        "--scored-path",
        default="data/processed_cases/scored_cases.jsonl",
        help="Path to scored cases",
    )
    parser.add_argument(
        "--metaprompts-path",
        default="data/metaprompts.jsonl",
        help="Path to metaprompts",
    )
    parser.add_argument(
        "--output-dir",
        default="../../data/relationship-seeking",
        help="Output directory for train/test splits",
    )
    parser.add_argument(
        "--min-score",
        type=float,
        default=6.0,
        help="Minimum score threshold (default: 6.0)",
    )
    parser.add_argument(
        "--train-size",
        type=float,
        default=0.95,
        help="Proportion for training set (default: 0.95)",
    )
    parser.add_argument(
        "--seed",
        type=int,
        default=123,
        help="Random seed (default: 123)",
    )
    args = parser.parse_args()

    setup_logging()

    # Load and merge data
    print("Loading data...")
    df = load_and_merge_data(args.processed_path, args.scored_path, args.metaprompts_path)

    if args.test:
        df = df.head(50)
        print(f"Test mode: limited to {len(df)} cases")

    # Filter by score
    print("\nFiltering by score...")
    df = filter_by_score(df, min_score=args.min_score)

    # Select base columns for DPO
    base_cols = ["prompt_id", "metaprompt_id", "target_conv_arr", "antitarget_conv_arr"]
    df = df[base_cols].copy()

    # Explode into DPO format
    print("\nExploding conversations...")
    df = explode_rows_for_dpo(df)

    # Create train/test split
    print("\nCreating train/test split...")
    train_df, test_df = create_train_test_split(
        df, train_size=args.train_size, random_state=args.seed
    )

    # Select final columns
    keep_cols = [
        "entry_id",
        "prompt_id",
        "metaprompt_id",
        "assistant_turn_idx",
        "history_type",
        "prompt_arr",
        "chosen_arr",
        "rejected_arr",
    ]
    train_df = train_df[keep_cols].copy()
    test_df = test_df[keep_cols].copy()

    if args.test:
        print("\n=== TEST MODE: Results (not saved) ===")
        print(f"Train shape: {train_df.shape}")
        print(f"Test shape: {test_df.shape}")
        print("\nSample train row:")
        print(train_df.iloc[0].to_dict())
    else:
        # Save outputs
        os.makedirs(args.output_dir, exist_ok=True)

        train_path = os.path.join(args.output_dir, "train.jsonl")
        test_path = os.path.join(args.output_dir, "test.jsonl")

        train_df.to_json(train_path, orient="records", lines=True)
        test_df.to_json(test_path, orient="records", lines=True)

        print(f"\nSaved:")
        print(f"  {train_path} ({len(train_df)} rows)")
        print(f"  {test_path} ({len(test_df)} rows)")


if __name__ == "__main__":
    main()
