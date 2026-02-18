import argparse
import itertools
import json
import os
import random
import sys
from pathlib import Path

import pandas as pd
from openai import OpenAI
from tqdm import tqdm

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from setup.api_utils import get_api_key
from exp_config import N_PAIRWISE_RANKINGS
from utils.pairwise_scoring import PairwiseScoringSystem, get_existing_scores


def calculate_expected_pairs(generations_df: pd.DataFrame) -> int:
    """Calculate expected number of pairs based on multiplier combinations."""
    grouped = generations_df.groupby("test_prompt_id")
    multipliers = (
        grouped["multiplier"]
        .unique()
        .explode()[(lambda x: (x >= -4) & (x <= 4))]
        .unique()
    )
    n_pairs = len(list(itertools.combinations(multipliers, 2)))
    return len(grouped) * n_pairs


def main():
    parser = argparse.ArgumentParser(description="Pairwise scoring of generations")
    parser.add_argument(
        "--model_name", required=True, help="Model name for experiment directory"
    )
    parser.add_argument(
        "--epoch", type=int, required=True, help="Epoch number for generations"
    )
    parser.add_argument(
        "--layer", type=int, required=True, help="Layer number for experiment directory"
    )
    parser.add_argument(
        "--task-name",
        type=str,
        default="relationship",
        help="Task name for output filename (default: relationship)"
    )

    args = parser.parse_args()

    # Define paths
    short_model_name = args.model_name.split("/")[-1]
    experiment_dir = Path(f"vector_evals/{short_model_name}/layer{args.layer}")
    if not experiment_dir.exists():
        print(f"Experiment directory does not exist: {experiment_dir}")
        return

    generation_file = f"{experiment_dir}/generations_ep{args.epoch}.jsonl"
    task_name = args.task_name.replace("-", "_")  # Normalize for filename
    output_file = Path(
        f"{experiment_dir}/pairwise_{task_name}_scores_ep{args.epoch}.jsonl"
    )

    # Check if task is already complete
    existing_scores, is_complete = get_existing_scores(output_file)
    if is_complete:
        print(f"All {N_PAIRWISE_RANKINGS} pairs already scored. Exiting.")
        return

    # Load generations
    try:
        generations = pd.read_json(generation_file, lines=True)
    except Exception as e:
        print(f"Error loading generations: {e}")
        return

    # Validate expected pairs
    expected_pairs = calculate_expected_pairs(generations)
    if expected_pairs != N_PAIRWISE_RANKINGS:
        print(
            f"Error: Expected {N_PAIRWISE_RANKINGS} pairs but calculated {expected_pairs}"
        )
        return

    print(f"Found {len(existing_scores)} existing scored pairs")

    # Setup scoring
    client = OpenAI(api_key=get_api_key("OPENAI_API_KEY"))
    scorer = PairwiseScoringSystem()

    # Track cost totals
    total_prompt_cost = 0
    total_completion_cost = 0
    total_cost = 0
    total_tokens = 0

    # Group by test_prompt_id and get valid multipliers
    grouped = generations.groupby("test_prompt_id")
    multipliers = (
        grouped["multiplier"]
        .unique()
        .explode()[(lambda x: (x >= -4) & (x <= 4))]
        .unique()
    )
    multiplier_pairs = list(itertools.combinations(multipliers, 2))

    RUN_INDEX = 0

    with output_file.open("a") as fout:
        for test_prompt_id, group in tqdm(grouped):
            for mult_a, mult_b in multiplier_pairs:
                # Get responses and IDs
                row_a = group[group["multiplier"] == mult_a].iloc[0]
                row_b = group[group["multiplier"] == mult_b].iloc[0]

                response_A = row_a["response_str"]
                response_B = row_b["response_str"]
                gen_id_a = row_a["generation_id"]
                gen_id_b = row_b["generation_id"]

                # Create consistent pair_id (smaller ID first)
                gen_id_min = min(gen_id_a, gen_id_b)
                gen_id_max = max(gen_id_a, gen_id_b)
                pair_id = f"{gen_id_min}-{gen_id_max}"

                # Skip if already scored
                if pair_id in existing_scores:
                    continue

                # Randomly decide response order (set seed)
                if random.random() < 0.5:
                    response_A, response_B = response_B, response_A
                    mult_a, mult_b = mult_b, mult_a
                    gen_id_a, gen_id_b = gen_id_b, gen_id_a

                # Score the pair
                model_response, usage_info = scorer.score_pair(client, response_A, response_B, verbose_index=RUN_INDEX)
                if model_response is not None:
                    result = {
                        "test_prompt_id": test_prompt_id,
                        "generation_id_a": int(gen_id_a),
                        "multiplier_a": float(mult_a),
                        "generation_id_b": int(gen_id_b),
                        "multiplier_b": float(mult_b),
                        "model_response": model_response,
                        "pair_id": pair_id,
                    }

                    # Add token usage and cost information
                    if usage_info:
                        result.update(usage_info)

                        # Update total costs
                        if "cost" in usage_info and usage_info["cost"]:
                            total_prompt_cost += usage_info["cost"]["prompt_cost"]
                            total_completion_cost += usage_info["cost"]["completion_cost"]
                            total_cost += usage_info["cost"]["total_cost"]
                            total_tokens += usage_info["token_usage"]["total_tokens"]

                    json.dump(result, fout)
                    fout.write("\n")
                    fout.flush()
                    existing_scores.add(pair_id)
                    RUN_INDEX += 1
                else:
                    print(f"Failed to score pair for prompt {test_prompt_id}")

    # Log total cost at the end
    print(f"\nSummary of API usage:")
    print(f"Total tokens used: {total_tokens}")
    print(f"Total prompt cost: ${total_prompt_cost:.6f}")
    print(f"Total completion cost: ${total_completion_cost:.6f}")
    print(f"Total cost: ${total_cost:.6f}")


if __name__ == "__main__":
    main()
