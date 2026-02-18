import argparse
import json
import os
import sys
from pathlib import Path
from typing import Dict, List, Optional, Set, Tuple

import pandas as pd

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from setup.api_utils import get_api_key
from utils.eval_rubrics import (
    COHERENCE_EXAMPLES,
    COHERENCE_RUBRIC,
    BEHAVIOR_EXAMPLES,
    BEHAVIOR_RUBRIC,
)
from exp_config import N_RATINGS
from openai import OpenAI
from pydantic import BaseModel
from tqdm import tqdm
import random

# Import cost tracking
from utils.exp_cost_tracking import calculate_cost


class Score(BaseModel):
    score: float


def simple_conv_history_processer(conv_history: List[Dict[str, str]]) -> str:
    """Process a conversation history (list of turns) into a single string."""
    processed_conv_history = ""
    for turn in conv_history:
        processed_conv_history += f"{turn['role']}: {turn['content']}\n"
    return processed_conv_history.strip()


def get_existing_scores(file_path: Path) -> Tuple[Set[str], bool]:
    """
    Load existing scores and check completion status.
    Returns (existing_ids, is_complete).
    """
    existing = set()
    if file_path.exists():
        try:
            df = pd.read_json(file_path, lines=True)
            existing = set(df["generation_id"].unique())
            if len(existing) >= N_RATINGS:
                return existing, True
        except Exception as e:
            print(f"Error reading existing scores: {e}")
            return set(), False
    return existing, False


class ScoringSystem:
    def __init__(self):
        self.coherence_examples = COHERENCE_EXAMPLES
        self.coherence_rubric = COHERENCE_RUBRIC
        self.relationship_examples = BEHAVIOR_EXAMPLES
        self.relationship_rubric = BEHAVIOR_RUBRIC
        self.model = "gpt-4o-2024-08-06"

    def score_response(
        self,
        client: OpenAI,
        history_str: str,
        response_str: str,
        temperature: float = 1.0,
        max_attempts: int = 3,
        task: str = "coherence",
        single_example=True,
        example_idx=None,
        RUN_INDEX: int = 0,
    ) -> Tuple[Optional[float], Optional[Dict]]:
        """Score a response by sending a prompt template to the model."""
        if task == "coherence":
            examples = self.coherence_examples
            rubric = self.coherence_rubric
        elif task == "relationship":
            examples = self.relationship_examples
            rubric = self.relationship_rubric
        else:
            print(f"Unknown task: {task}")
            return None, None

        def create_example_string(example_history_str, score1_text, score10_text):
            return f"""
<conversation history>
{example_history_str}
</conversation history>

</score 1 final assistant response>
{score1_text}
</score 1 final assistant response>

<score 10 final assistant response>
{score10_text}
</score 10 final assistant response>
"""

        if single_example:
            example = (
                examples[example_idx]
                if example_idx is not None
                else random.choice(examples)
            )
            example_history_str = simple_conv_history_processer(example["prompt"])
            score1_text = example["score_1"]["content"]
            score10_text = example["score_10"]["content"]
            example_message = create_example_string(
                example_history_str, score1_text, score10_text
            )
        else:
            example_message = ""
            # Randomly sample two items from examples
            sampled_examples = random.sample(examples, 2)
            for e in sampled_examples:
                example_history_str = simple_conv_history_processer(e["prompt"])
                score1_text = e["score_1"]["content"]
                score10_text = e["score_10"]["content"]
                example_message += create_example_string(
                    example_history_str, score1_text, score10_text
                )

        message_content = f"""
<context>
{rubric}
</context>

<examples>
{example_message}
</examples>

<instruction>
Evaluate the following response and assign a score between 1 and 10.
</instruction>

<conversation history>
{history_str}
</conversation history>

<final assistant response>
{response_str}
</final assistant response>
    """.strip()

        messages = [
            {"role": "user", "content": message_content},
        ]

        if RUN_INDEX < 21:
            print(f"####EXAMPLE MESSAGE {RUN_INDEX}######")
            print(message_content)
            print(f"#############################")

        for attempt in range(max_attempts):
            try:
                completion = client.beta.chat.completions.parse(
                    model=self.model,
                    messages=messages,
                    response_format=Score,
                    max_completion_tokens=10,
                    temperature=0,
                )

                # Calculate cost
                token_usage = {
                    "prompt_tokens": completion.usage.prompt_tokens,
                    "completion_tokens": completion.usage.completion_tokens,
                    "total_tokens": completion.usage.total_tokens
                }

                cost = calculate_cost(
                    self.model,
                    token_usage["prompt_tokens"],
                    token_usage["completion_tokens"]
                )

                return completion.choices[0].message.parsed.score, {
                    "token_usage": token_usage,
                    "cost": cost
                }

            except Exception as e:
                print(f"Attempt {attempt + 1}: Error scoring response: {str(e)}")
        print(f"Failed to score response after {max_attempts} attempts")
        return None, None


def main():
    parser = argparse.ArgumentParser(description="Score generations with gpt as a judge")
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
        "--task",
        type=str,
        required=True,
        help="Task to score (coherence or relationship)",
    )
    parser.add_argument(
        "--prompting_baseline",
        action="store_true",
        help="Score prompting baseline instead of vector generations"
    )
    parser.add_argument(
        "--behavior",
        type=str,
        default="relationship-seeking",
        help="Behavior name for data directory"
    )

    args = parser.parse_args()

    # Define paths differently based on whether we're scoring vector-based or prompting-based
    if args.prompting_baseline:
        # For prompting baselines
        model_name = args.model_name
        experiment_dir = Path(f"prompt_baseline/{model_name}")
        generation_file = f"{experiment_dir}/generations.jsonl"
        output_file = Path(f"{experiment_dir}/{args.task}_scores.jsonl")
    else:
        # For vector-based experiments (original behavior)
        short_model_name = args.model_name.split("/")[-1]
        experiment_dir = Path(f"vector_evals/{short_model_name}/layer{args.layer}")
        generation_file = f"{experiment_dir}/generations_ep{args.epoch}.jsonl"
        output_file = Path(f"{experiment_dir}/{args.task}_scores_ep{args.epoch}.jsonl")

    if not Path(generation_file).exists():
        print(f"Generation file does not exist: {generation_file}")
        return

    # Check if task is already complete
    existing_scores, is_complete = get_existing_scores(output_file)
    if is_complete:
        print(f"All {N_RATINGS} generations already scored. Exiting.")
        return

    # Load data files
    try:
        generations = pd.read_json(generation_file, lines=True)
        test_file = f"data/{args.behavior}/test.jsonl"
        test_prompts = pd.read_json(test_file, lines=True)
    except Exception as e:
        print(f"Error loading data files: {e}")
        return

    # For prompting baselines, we don't need to enforce the rating number
    if not args.prompting_baseline:
        # Validate number of generations
        if len(generations) != N_RATINGS:
            print(f"Error: Found {len(generations)} generations, expected {N_RATINGS}")
            return

    # Merge prompts with generations
    merged_generations = generations.merge(
        test_prompts[["prompt_id", "prompt_arr"]],
        left_on="test_prompt_id",
        right_on="prompt_id",
        how="left",
        indicator=True,
    )

    print(f"Merge status: {merged_generations['_merge'].value_counts()}")
    print(f"Found {len(existing_scores)} existing scored items")

    # Setup scoring
    client = OpenAI(api_key=get_api_key("OPENAI_API_KEY"))
    scorer = ScoringSystem()

    # Track cost totals
    total_prompt_cost = 0
    total_completion_cost = 0
    total_cost = 0
    total_tokens = 0

    # Process generations that haven't been scored
    generations_to_score = merged_generations[
        ~merged_generations["generation_id"].isin(existing_scores)
    ]
    RUN_INDEX = 0
    with output_file.open("a") as fout:
        for _, item in tqdm(
            generations_to_score.iterrows(), total=len(generations_to_score)
        ):
            score, usage_info = scorer.score_response(
                client,
                history_str=simple_conv_history_processer(item["prompt_arr"]),
                response_str=item["response_str"],
                task=args.task,
                single_example=False,
                example_idx=None,
                RUN_INDEX=RUN_INDEX,
            )

            if score is not None:
                result = {
                    "generation_id": item["generation_id"],
                    "test_prompt_id": item["test_prompt_id"],
                    "multiplier": item["multiplier"],
                    "score": score,
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
                existing_scores.add(item["generation_id"])
                RUN_INDEX += 1
            else:
                print(f"Failed to score generation {item['generation_id']}")

    # Log total cost at the end
    print(f"\nSummary of API usage:")
    print(f"Total tokens used: {total_tokens}")
    print(f"Total prompt cost: ${total_prompt_cost:.6f}")
    print(f"Total completion cost: ${total_completion_cost:.6f}")
    print(f"Total cost: ${total_cost:.6f}")


if __name__ == "__main__":
    import random

    main()
