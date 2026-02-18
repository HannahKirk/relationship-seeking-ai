"""Convert downloaded ranking batch results to expected format."""

import argparse
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
    """Extract model_response, token_usage, and cost from batch results."""
    response_data = row["response"]
    body = response_data["body"]

    model_response = body["choices"][0]["message"]["content"].strip()
    token_usage = body["usage"]

    # Calculate cost
    prompt_tokens = token_usage["prompt_tokens"]
    completion_tokens = token_usage["completion_tokens"]
    cost = calculate_cost("gpt-4o-2024-08-06", prompt_tokens, completion_tokens)

    return model_response, token_usage, cost


def process_batch_results(b_result_df, batch_df, gen_df):
    """Process batch results for pairwise rankings."""
    # Step 1: Extract model_response, token_usage, and cost from batch results
    results = []
    for _, row in tqdm(
        b_result_df.iterrows(),
        total=len(b_result_df),
        desc="Extracting response data",
    ):
        try:
            model_response, token_usage, cost = extract_response_data(row)
            results.append(
                [row["id"], row["custom_id"], model_response, token_usage, cost]
            )
        except Exception as e:
            print(f"Error processing row: {e}")
            continue

    result_df = pd.DataFrame(
        results, columns=["id", "pair_id", "model_response", "token_usage", "cost"]
    )

    # Step 2: Create lookup dictionaries from gen_df
    gen_text_dict = dict(
        zip(gen_df["generation_id"], gen_df["response_str"].str.strip())
    )
    gen_multiplier_dict = dict(zip(gen_df["generation_id"], gen_df["multiplier"]))
    gen_prompt_dict = dict(zip(gen_df["generation_id"], gen_df["test_prompt_id"]))

    # Step 3: Match generations to A/B positions
    final_results = []

    for _, row in tqdm(
        result_df.iterrows(), total=len(result_df), desc="Matching A/B positions"
    ):
        try:
            # Parse pair_id to get generation IDs
            gen_id_1, gen_id_2 = map(int, row["pair_id"].split("-"))

            # Get the corresponding batch request
            batch_row = batch_df[batch_df["custom_id"] == row["pair_id"]].iloc[0]
            prompt_content = batch_row["body"]["messages"][0]["content"]

            # Extract the LAST response A and B from prompt
            response_a_matches = re.findall(
                r"<response A>(.*?)</response A>", prompt_content, re.DOTALL
            )
            response_b_matches = re.findall(
                r"<response B>(.*?)</response B>", prompt_content, re.DOTALL
            )

            if not response_a_matches or not response_b_matches:
                print(f"Could not find response tags in pair {row['pair_id']}")
                continue

            response_a = response_a_matches[-1].strip()
            response_b = response_b_matches[-1].strip()

            # Determine which generation is A and which is B
            gen_text_1 = gen_text_dict[gen_id_1]
            gen_text_2 = gen_text_dict[gen_id_2]

            if gen_text_1 == response_a and gen_text_2 == response_b:
                gen_id_a, gen_id_b = gen_id_1, gen_id_2
            elif gen_text_1 == response_b and gen_text_2 == response_a:
                gen_id_a, gen_id_b = gen_id_2, gen_id_1
            else:
                # Try with more relaxed matching (trimming more whitespace)
                gen_text_1_clean = " ".join(gen_text_1.split())
                gen_text_2_clean = " ".join(gen_text_2.split())
                response_a_clean = " ".join(response_a.split())
                response_b_clean = " ".join(response_b.split())

                if (
                    gen_text_1_clean == response_a_clean
                    and gen_text_2_clean == response_b_clean
                ):
                    gen_id_a, gen_id_b = gen_id_1, gen_id_2
                elif (
                    gen_text_1_clean == response_b_clean
                    and gen_text_2_clean == response_a_clean
                ):
                    gen_id_a, gen_id_b = gen_id_2, gen_id_1
                else:
                    print(
                        f"No match after cleaning whitespace for {row['pair_id']}"
                    )
                    continue

            # Get test_prompt_id, multipliers
            test_prompt_id = gen_prompt_dict[gen_id_a]
            multiplier_a = gen_multiplier_dict[gen_id_a]
            multiplier_b = gen_multiplier_dict[gen_id_b]

            # Add to results
            final_results.append(
                {
                    "test_prompt_id": test_prompt_id,
                    "generation_id_a": gen_id_a,
                    "multiplier_a": multiplier_a,
                    "generation_id_b": gen_id_b,
                    "multiplier_b": multiplier_b,
                    "model_response": row["model_response"],
                    "pair_id": row["pair_id"],
                    "token_usage": row["token_usage"],
                    "cost": row["cost"],
                }
            )

        except Exception as e:
            print(f"Error processing pair {row['pair_id']}: {e}")
            continue

    return pd.DataFrame(final_results)


def main():
    parser = argparse.ArgumentParser(
        description="Convert downloaded ranking batch results to expected format"
    )
    parser.add_argument(
        "--epochs",
        type=int,
        nargs="+",
        default=[10, 15, 20],
        help="Epochs to process (default: 10 15 20)",
    )
    parser.add_argument(
        "--task-name",
        type=str,
        default="relationship",
        help="Task name for filenames (default: relationship)",
    )
    args = parser.parse_args()
    task_name = args.task_name.replace("-", "_")  # Normalize for filename

    # Process each model-layer combination
    for short_name, layer, _ in ModelConfig.get_experiments():
        model_layer_path = Path(f"vector_evals/{short_name}/layer{layer}")
        print(f"Processing {short_name} - layer{layer}")

        if not model_layer_path.exists():
            print(f"Directory {model_layer_path} does not exist. Skipping.")
            continue

        # Process each epoch
        for epoch in args.epochs:
            formatted_file = (
                model_layer_path / f"pairwise_{task_name}_scores_ep{epoch}.jsonl"
            )

            # Check if already processed
            if formatted_file.exists():
                print(f"Formatted file {formatted_file} already exists. Skipping.")
                continue

            # Load generations file
            gen_file = model_layer_path / f"generations_ep{epoch}.jsonl"
            if not gen_file.exists():
                print(f"Generations file {gen_file} does not exist. Skipping.")
                continue

            print(f"Loading generations from {gen_file}")
            gen_df = load_jsonl(gen_file)

            # Load pairwise batch results
            pairwise_file = (
                model_layer_path / f"raw_pairwise_{task_name}_scores_ep{epoch}.jsonl"
            )
            if not pairwise_file.exists():
                print(f"Pairwise file {pairwise_file} does not exist. Skipping.")
                continue

            print(f"Processing epoch {epoch}")
            b_result_df = load_jsonl(pairwise_file)

            # Load the batch file
            batch_file = model_layer_path / f"batch_pairwise_ep{epoch}.jsonl"
            if not batch_file.exists():
                print(f"Batch file {batch_file} does not exist. Skipping.")
                continue

            batch_df = load_jsonl(batch_file)

            # Process the batch results
            final_df = process_batch_results(b_result_df, batch_df, gen_df)

            print(
                f"Successfully processed {len(final_df)} out of {len(b_result_df)} pairs"
            )

            # Save processed results
            save_jsonl(final_df, formatted_file)
            print(f"Saved processed results to {formatted_file}")

    print("Processing complete")


if __name__ == "__main__":
    main()
