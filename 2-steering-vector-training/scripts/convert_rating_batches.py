"""Convert downloaded rating batch results to expected format."""

import argparse
import json
import os
import re
import sys
from pathlib import Path

import pandas as pd
from tqdm import tqdm

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from utils.batch_utils import load_jsonl, save_jsonl
from utils.exp_cost_tracking import calculate_cost
from exp_config import ModelConfig


def extract_response_data(row):
    """Extract score, token_usage, and cost from batch results."""
    response_data = row["response"]
    body = response_data["body"]

    # Extract score from the content, which is in JSON format
    content = body["choices"][0]["message"]["content"].strip()
    try:
        score_data = json.loads(content)
        score = score_data.get("score")
    except json.JSONDecodeError:
        # Try to extract the score using regex if JSON parsing fails
        score_match = re.search(r'"score"\s*:\s*(\d+)', content)
        if score_match:
            score = int(score_match.group(1))
        else:
            raise ValueError(f"Could not extract score from: {content}")

    token_usage = body["usage"]

    # Calculate cost
    prompt_tokens = token_usage["prompt_tokens"]
    completion_tokens = token_usage["completion_tokens"]
    cost = calculate_cost("gpt-4o-2024-08-06", prompt_tokens, completion_tokens)

    return score, token_usage, cost


def process_rating_results(b_result_df, gen_df):
    """Process rating results for coherence or relationship tasks."""
    results = []

    for _, row in tqdm(
        b_result_df.iterrows(), total=len(b_result_df), desc="Processing ratings"
    ):
        try:
            # Extract the generation ID from custom_id
            generation_id = int(row["custom_id"])

            # Extract score, token usage, and cost
            score, token_usage, cost = extract_response_data(row)

            # Look up the generation details from the generations dataframe
            gen_row = gen_df[gen_df["generation_id"] == generation_id]

            if len(gen_row) == 0:
                print(
                    f"Warning: Could not find generation with ID {generation_id} in generations file"
                )
                continue

            # Extract generation details
            test_prompt_id = gen_row["test_prompt_id"].values[0]
            multiplier = gen_row["multiplier"].values[0]

            # Create the result row
            result = {
                "generation_id": generation_id,
                "test_prompt_id": test_prompt_id,
                "multiplier": multiplier,
                "score": score,
                "token_usage": token_usage,
                "cost": cost,
            }

            results.append(result)

        except Exception as e:
            print(
                f"Error processing row with custom_id {row.get('custom_id', 'unknown')}: {e}"
            )
            continue

    return pd.DataFrame(results)


def main():
    parser = argparse.ArgumentParser(
        description="Convert downloaded rating batch results to expected format"
    )
    parser.add_argument(
        "--epochs",
        type=int,
        nargs="+",
        default=[10, 15, 20],
        help="Epochs to process (default: 10 15 20)",
    )
    parser.add_argument(
        "--tasks",
        type=str,
        nargs="+",
        default=["coherence", "relationship"],
        help="Tasks to process (default: coherence relationship)",
    )
    args = parser.parse_args()

    # Process each model-layer combination
    for short_name, layer, _ in ModelConfig.get_experiments():
        model_layer_path = Path(f"vector_evals/{short_name}/layer{layer}")
        print(f"Processing {short_name} - layer{layer}")

        if not model_layer_path.exists():
            print(f"Directory {model_layer_path} does not exist. Skipping.")
            continue

        # Process each epoch
        for epoch in args.epochs:
            # Load generations file
            gen_file = model_layer_path / f"generations_ep{epoch}.jsonl"
            if not gen_file.exists():
                print(f"Generations file {gen_file} does not exist. Skipping.")
                continue

            print(f"Loading generations from {gen_file}")
            gen_df = load_jsonl(gen_file)

            # Process each task
            for task in args.tasks:
                rating_file = model_layer_path / f"raw_{task}_scores_ep{epoch}.jsonl"
                formatted_file = model_layer_path / f"{task}_scores_ep{epoch}.jsonl"

                # Check if already processed
                if formatted_file.exists():
                    print(f"Formatted file {formatted_file} already exists. Skipping.")
                    continue

                if not rating_file.exists():
                    print(f"{task} scores file {rating_file} does not exist. Skipping.")
                    continue

                print(f"Processing {task} scores for epoch {epoch}")

                # Load and process rating results
                b_result_df = load_jsonl(rating_file)
                results_df = process_rating_results(b_result_df, gen_df)

                print(
                    f"Successfully processed {len(results_df)} out of {len(b_result_df)} {task} ratings"
                )

                # Save processed results
                save_jsonl(results_df, formatted_file)
                print(f"Saved processed results to {formatted_file}")

    print("Processing complete")


if __name__ == "__main__":
    main()
