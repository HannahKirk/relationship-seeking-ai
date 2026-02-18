#!/usr/bin/env python3
# eval_ranking_baseline.py
import argparse
import json
import os
import random
import sys
from pathlib import Path
from typing import Tuple

import pandas as pd
from openai import OpenAI
from tqdm import tqdm

# Add parent directories to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", ".."))

from setup.api_utils import get_api_key
from scripts.utils.pairwise_scoring import PairwiseScoringSystem, get_existing_scores
from prompt_baseline_funcs import get_multiplier_level_pairs

# Number of test prompts
N_TEST_PROMPTS = 245


def load_generations(
    steering_file: Path, baseline_file: Path, multiplier_mapping: dict
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """Load and prepare both steering vector and baseline generations."""
    # Load steering vector generations
    steering_df = pd.read_json(steering_file, lines=True)
    # Filter to keep only multipliers in our range of interest
    steering_df = steering_df[
        steering_df["multiplier"].isin(multiplier_mapping.values())
    ]

    # Load baseline generations
    baseline_df = pd.read_json(baseline_file, lines=True)
    # Filter to keep only levels in our range of interest
    baseline_df = baseline_df[baseline_df["level"].isin(multiplier_mapping.keys())]

    # Verify column names
    print(f"Steering dataframe columns: {steering_df.columns.tolist()}")
    print(f"Baseline dataframe columns: {baseline_df.columns.tolist()}")

    return steering_df, baseline_df


def main():
    parser = argparse.ArgumentParser(
        description="Ranking evaluation of baseline vs steering vector generations"
    )
    parser.add_argument(
        "--model_name",
        required=True,
        help="Model name for experiment directory (Llama-3.1-8B-Instruct)",
    )
    parser.add_argument(
        "--baseline_model",
        required=True,
        choices=["claude", "gpt"],
        help="Baseline model to compare against the steering vector",
    )
    parser.add_argument(
        "--epoch", type=int, required=True, help="Epoch number for generations"
    )
    parser.add_argument(
        "--layer", type=int, required=True, help="Layer number for experiment directory"
    )
    parser.add_argument(
        "--num-levels",
        type=int,
        choices=[7, 9],
        default=7,
        help="Number of levels to use (7 or 9). Must match the prompting baseline generations."
    )

    args = parser.parse_args()

    # Get multiplier-level mapping based on num_levels
    multiplier_level_pairs = get_multiplier_level_pairs(args.num_levels)
    # Convert to dict: {level: multiplier}
    multiplier_mapping = {level: mult for mult, level in multiplier_level_pairs}
    n_total_comparisons = N_TEST_PROMPTS * len(multiplier_mapping)

    # Define paths (relative to 2-steering-vector-training/)
    script_dir = Path(__file__).parent
    base_dir = script_dir.parent.parent  # 2-steering-vector-training/

    short_model_name = args.model_name.split("/")[-1]
    experiment_dir = base_dir / f"vector_evals/{short_model_name}/layer{args.layer}"
    if not experiment_dir.exists():
        print(f"Experiment directory does not exist: {experiment_dir}")
        return

    steering_gen_file = experiment_dir / f"generations_ep{args.epoch}.jsonl"
    baseline_gen_file = script_dir / f"{args.baseline_model}_levels_{args.num_levels}/generations.jsonl"

    # Verify files exist
    if not steering_gen_file.exists():
        print(f"Steering vector generations file does not exist: {steering_gen_file}")
        return
    if not baseline_gen_file.exists():
        print(f"Baseline generations file does not exist: {baseline_gen_file}")
        return

    # Define output file in the specified baseline directory
    output_file = (
        script_dir
        / f"{args.baseline_model}_levels_{args.num_levels}/ranking_{args.baseline_model}_vs_steering_{short_model_name}_layer{args.layer}_ep{args.epoch}.jsonl"
    )

    # Check if task is already complete
    existing_scores, is_complete = get_existing_scores(output_file, n_total_comparisons)
    if is_complete:
        print(f"All {n_total_comparisons} comparisons already scored. Exiting.")
        return

    # Load generations
    try:
        steering_df, baseline_df = load_generations(
            steering_gen_file, baseline_gen_file, multiplier_mapping
        )
    except Exception as e:
        print(f"Error loading generations: {e}")
        return

    print(
        f"Loaded {len(steering_df)} steering vector generations and {len(baseline_df)} baseline generations"
    )
    print(f"Found {len(existing_scores)} existing scored pairs")

    # Setup scoring
    api_key = get_api_key("OPENAI_API_KEY")
    client = OpenAI(api_key=api_key)
    scorer = PairwiseScoringSystem()

    # Track cost totals
    total_prompt_cost = 0
    total_completion_cost = 0
    total_cost = 0
    total_tokens = 0

    RUN_INDEX = 0

    # Get unique test_prompt_ids that exist in both dataframes
    steering_prompts = set(steering_df["test_prompt_id"].unique())
    baseline_prompts = set(baseline_df["test_prompt_id"].unique())
    common_prompts = sorted(steering_prompts.intersection(baseline_prompts))
    assert len(steering_prompts) == len(baseline_prompts) == len(common_prompts)

    print(f"Found {len(common_prompts)} common test prompts between datasets")

    if len(common_prompts) == 0:
        print("ERROR: No common test prompts found between datasets")
        return

    with output_file.open("a") as fout:
        # Group by test_prompt_id
        for test_prompt_id in tqdm(common_prompts):
            # Filter generations for this test prompt
            steering_prompt_gens = steering_df[
                steering_df["test_prompt_id"] == test_prompt_id
            ]
            baseline_prompt_gens = baseline_df[
                baseline_df["test_prompt_id"] == test_prompt_id
            ]

            # Skip if we don't have both generations for this prompt
            if len(steering_prompt_gens) == 0 or len(baseline_prompt_gens) == 0:
                print(
                    f"Missing generations for test_prompt_id {test_prompt_id}, skipping"
                )
                continue

            # Compare each multiplier level
            for baseline_level, steering_multiplier in multiplier_mapping.items():
                # Get baseline response for this level
                baseline_gen = baseline_prompt_gens[
                    baseline_prompt_gens["level"] == baseline_level
                ]
                if len(baseline_gen) == 0:
                    print(
                        f"Missing baseline generation for prompt {test_prompt_id} at level {baseline_level}"
                    )
                    continue

                # Get steering vector response for this multiplier
                steering_gen = steering_prompt_gens[
                    steering_prompt_gens["multiplier"] == steering_multiplier
                ]
                if len(steering_gen) == 0:
                    print(
                        f"Missing steering generation for prompt {test_prompt_id} at multiplier {steering_multiplier}"
                    )
                    continue

                # Extract responses and IDs
                baseline_row = baseline_gen.iloc[0]
                steering_row = steering_gen.iloc[0]

                baseline_response = baseline_row["response_str"]
                steering_response = steering_row["response_str"]

                # Use a consistent pair_id format
                baseline_id = f"{args.baseline_model}_{test_prompt_id}_{baseline_level}"
                steering_id = f"steering_{test_prompt_id}_{steering_multiplier}"
                pair_id = f"{baseline_id}-{steering_id}"

                # Skip if already scored
                if pair_id in existing_scores:
                    continue

                # Assign responses to A/B (with randomization)
                if random.random() < 0.5:
                    response_A, response_B = baseline_response, steering_response
                    is_baseline_A = True
                else:
                    response_A, response_B = steering_response, baseline_response
                    is_baseline_A = False

                # Score the pair
                model_response, usage_info = scorer.score_pair(
                    client, response_A, response_B, verbose_index=RUN_INDEX
                )

                if model_response is not None:
                    # Determine which model won based on the response and randomization
                    if (is_baseline_A and model_response == "A") or (
                        not is_baseline_A and model_response == "B"
                    ):
                        winner = args.baseline_model
                    else:
                        winner = "steering_vector"

                    # Create result record
                    result = {
                        "test_prompt_id": int(test_prompt_id),
                        "baseline_model": args.baseline_model,
                        "baseline_level": int(baseline_level),
                        "steering_multiplier": float(steering_multiplier),
                        "is_baseline_A": bool(is_baseline_A),
                        "model_response": model_response,
                        "winner": winner,
                        "pair_id": pair_id,
                    }

                    # Add token usage and cost information
                    if usage_info:
                        result.update(usage_info)

                        # Update total costs
                        if "cost" in usage_info and usage_info["cost"]:
                            total_prompt_cost += usage_info["cost"]["prompt_cost"]
                            total_completion_cost += usage_info["cost"][
                                "completion_cost"
                            ]
                            total_cost += usage_info["cost"]["total_cost"]
                            total_tokens += usage_info["token_usage"]["total_tokens"]

                    # Write result
                    json.dump(result, fout)
                    fout.write("\n")
                    fout.flush()
                    existing_scores.add(pair_id)
                    RUN_INDEX += 1
                else:
                    print(
                        f"Failed to score pair for prompt {test_prompt_id}, level {baseline_level}"
                    )

    # Log total cost at the end
    print(f"\nSummary of API usage:")
    print(f"Total tokens used: {total_tokens}")
    print(f"Total prompt cost: ${total_prompt_cost:.6f}")
    print(f"Total completion cost: ${total_completion_cost:.6f}")
    print(f"Total cost: ${total_cost:.6f}")

    # Calculate and print win rates
    if output_file.exists():
        results_df = pd.read_json(output_file, lines=True)
        for level, multiplier in multiplier_mapping.items():
            level_results = results_df[
                (results_df["baseline_level"] == level)
                & (results_df["steering_multiplier"] == multiplier)
            ]
            if len(level_results) > 0:
                baseline_wins = sum(level_results["winner"] == args.baseline_model)
                win_rate = baseline_wins / len(level_results) * 100
                print(
                    f"{args.baseline_model} level {level} vs steering {multiplier}: "
                    f"{baseline_wins}/{len(level_results)} ({win_rate:.2f}% win rate)"
                )

        # Overall win rate
        baseline_wins = sum(results_df["winner"] == args.baseline_model)
        total_comparisons = len(results_df)
        if total_comparisons > 0:
            overall_win_rate = baseline_wins / total_comparisons * 100
            print(
                f"\nOverall {args.baseline_model} win rate: "
                f"{baseline_wins}/{total_comparisons} ({overall_win_rate:.2f}%)"
            )


if __name__ == "__main__":
    main()
