#!/usr/bin/env python3
"""
Score test cases using LLM-based evaluation.

This script uses an LLM to score test cases based on how effectively they
evaluate relationship-seeking behaviors.
"""

import argparse
import json
import logging
import os
import sys
import numpy as np
from pathlib import Path
from typing import Any, Dict, List, Tuple

from openai import OpenAI

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))

from setup.api_utils import get_api_key
from utils.generation_utils import setup_logging
from utils.scorer_rubric import RUBRIC, examples


def calculate_cost(model, prompt_tokens, completion_tokens):
    """Calculate the estimated cost in USD based on token usage."""
    # Pricing per 1M tokens (as of February 2025)
    pricing = {
        "o1-mini-2024-09-12": {
            "prompt": 3.00,  # $3.00 per 1M input tokens
            "completion": 15.00  # $15.00 per 1M output tokens
        }
    }

    if model not in pricing:
        logging.warning(f"No pricing information for model {model}")
        return None

    # Calculate cost
    prompt_cost = (prompt_tokens / 1_000_000) * pricing[model]["prompt"]
    completion_cost = (completion_tokens / 1_000_000) * pricing[model]["completion"]
    total_cost = prompt_cost + completion_cost

    return {
        "prompt_cost": np.round(prompt_cost, 2),
        "completion_cost": np.round(completion_cost, 2),
        "total_cost": np.round(total_cost, 2)
    }


def get_existing_scores(file_path: Path) -> set:
    """Load existing scores and return set of prompt_ids."""
    existing = set()
    if file_path.exists():
        with file_path.open("r") as f:
            for line in f:
                data = json.loads(line)
                existing.add(data["prompt_id"])
    return existing


def format_examples(examples: List[Dict[str, str]]) -> str:
    """
    Formats a list of example dictionaries into a string.
    Each example consists of a user and assistant pair, separated by a delimiter.
    """
    formatted = ""
    delimiter = "\n---\n"  # Delimiter between examples
    for i in range(0, len(examples), 2):
        user_example = examples[i]["content"]
        assistant_example = examples[i + 1]["content"]
        formatted += (
            f"\n<test case>\n{user_example}\n</test case>\n<response>\n{assistant_example}\n</response>\n\n"
        )
    return formatted.strip(delimiter)  # Remove the last delimiter


def score_question(
    client: OpenAI,
    question: Dict[str, Any],
    formatted_examples: str,
    model_name: str = "o1-mini-2024-09-12",
    temperature: float = 1.0,
) -> Tuple[int, str, Dict, Dict]:
    """
    Score a single question using the provided OpenAI client.
    Returns tuple of (score, full_response, token_usage, cost) or (None, None, None, None) if failed.
    """
    valid_response = False
    attempts = 0
    max_attempts = 3

    while not valid_response and attempts < max_attempts:
        try:
            # Construct the message according to O1 best practices
            message_content = f"""
<CONTEXT>
{RUBRIC}
</CONTEXT>
\n
<EXAMPLES>
{formatted_examples}
</EXAMPLES>
\n
<INSTRUCTION>
Score the following test case. Provide a numerical score based on the rubric criteria.
<test case>
{
                json.dumps(
                    {
                        "target_conv": question["target_conv_str"],
                        "antitarget_conv": question["antitarget_conv_str"],
                    }
                )
            }
</test case>
</INSTRUCTION>
""".strip()
            # print(message_content)

            messages = [{"role": "user", "content": message_content}]

            response = client.chat.completions.create(
                model=model_name,
                messages=messages,
                max_completion_tokens=5000,
                temperature=1.0,
            )

            response_text = response.choices[0].message.content

            # Extract token usage
            token_usage = {
                "prompt_tokens": response.usage.prompt_tokens,
                "completion_tokens": response.usage.completion_tokens,
                "total_tokens": response.usage.total_tokens
            }

            logging.info(
                f"Token usage - Prompt: {token_usage['prompt_tokens']}, "
                f"Completion: {token_usage['completion_tokens']}, "
                f"Total: {token_usage['total_tokens']}"
            )

            # Calculate cost
            cost = calculate_cost(model_name, token_usage["prompt_tokens"], token_usage["completion_tokens"])
            if cost:
                logging.info(
                    f"Estimated cost - Prompt: ${cost['prompt_cost']:.4f}, "
                    f"Completion: ${cost['completion_cost']:.4f}, "
                    f"Total: ${cost['total_cost']:.4f}"
                )

            # Extract the score from the response, assuming it's enclosed in <score> tags
            try:
                clean_response = int(
                    response_text.split("<score>")[1].split("</score>")[0]
                )
            except (IndexError, ValueError) as parse_error:
                print(response)
                raise ValueError(
                    f"Failed to parse score from response: {response_text}"
                ) from parse_error

            valid_response = True

            logging.info(
                f"Prompt ID {question['prompt_id']} scored: {clean_response}"
            )
            return clean_response, response_text, token_usage, cost

        except Exception as e:
            attempts += 1
            logging.error(f"Attempt {attempts}: Error scoring question: {str(e)}")

    logging.error(
        f"Failed to get valid response after {max_attempts} attempts for Question ID {question['prompt_id']}"
    )
    return None, None, None, None  # Return None to indicate failure


def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description="Score test cases using GPT-4o")
    parser.add_argument(
        "--test", action="store_true", help="Process only one question for testing"
    )
    parser.add_argument(
        "--input",
        default="data/processed_cases/processed_cases.jsonl",
        help="Input file path",
    )
    parser.add_argument(
        "--output",
        default="data/processed_cases/scored_cases.jsonl",
        help="Output file path",
    )
    parser.add_argument(
        "--until",
        type=int,
        default=None,
        help="Process test cases up to this prompt index (exclusive).",
    )
    parser.add_argument(
        "--prompt-ids",
        type=str,
        help="Comma-separated list of prompt IDs to process",
    )
    parser.add_argument(
        "--model",
        default="o1-mini-2024-09-12",
        help="OpenAI model to use for scoring (default: o1-mini-2024-09-12)",
    )
    args = parser.parse_args()

    setup_logging()
    logging.info("Loaded rubric and examples from module")

    # Initialize OpenAI client
    client = OpenAI(api_key=get_api_key("OPENAI_API_KEY"))

    input_file = Path(args.input)
    output_file = Path(args.output)

    # Load existing scores
    existing_scores = get_existing_scores(output_file)
    logging.info(f"Found {len(existing_scores)} existing scored prompts")

    # First, load all questions to know how many we have
    with input_file.open("r") as fin:
        questions = [json.loads(line) for line in fin]

    total_questions = len(questions)

    # Filter questions based on prompt IDs if provided
    if args.prompt_ids:
        target_ids = {int(id.strip()) for id in args.prompt_ids.split(",")}
        questions_to_score = [
            q
            for q in questions
            if q["prompt_id"] in target_ids and q["prompt_id"] not in existing_scores
        ]
        logging.info(f"Found {len(target_ids)} requested prompt IDs")
        if len(questions_to_score) < len(target_ids):
            missing_ids = target_ids - {q["prompt_id"] for q in questions_to_score}
            logging.warning(f"Could not find questions for prompt IDs: {missing_ids}")
    else:
        questions_to_score = [
            q for q in questions if q["prompt_id"] not in existing_scores
        ]

    logging.info(
        f"Found {total_questions} total questions, {len(questions_to_score)} need scoring"
    )

    if not questions_to_score:
        logging.info("No new questions to score.")
        return

    # If --until is specified and --prompt-ids is not, limit the number of questions to process
    if args.until is not None and not args.prompt_ids:
        if args.until < 0:
            logging.error("--until must be a non-negative integer.")
            return
        questions_to_score = questions_to_score[: args.until]
        logging.info(
            f"Processing up to prompt index {args.until} ({len(questions_to_score)} questions)."
        )

    # Format examples once to avoid repeated formatting
    formatted_examples = format_examples(examples)

    # Test mode: score first question, print result, don't save
    if args.test:
        question = questions[0]  # Use first question, not first unscored
        logging.info(f"Test mode: scoring prompt_id {question['prompt_id']}")
        try:
            score, response_text, token_usage, cost = score_question(
                client, question, formatted_examples, model_name=args.model, temperature=1
            )
            if score is not None:
                print(f"\n=== TEST MODE: Results (not saved) ===")
                print(f"Prompt ID: {question['prompt_id']}")
                print(f"Score: {score}")
                print(f"Token usage: {token_usage}")
                print(f"Cost: {cost}")
                print(f"\nResponse:\n{response_text[:500]}...")
            else:
                logging.error("Scoring failed")
        except Exception as e:
            logging.error(f"Error: {e}")
        return

    # Track total cost
    total_prompt_cost = 0
    total_completion_cost = 0
    total_cost = 0
    total_tokens = 0

    # Process questions
    with output_file.open("a") as fout:
        for i, question in enumerate(questions_to_score):
            try:
                score, response_text, token_usage, cost = score_question(
                    client, question, formatted_examples, model_name=args.model, temperature=1
                )
                if score is not None:
                    result = {
                        "prompt_id": question["prompt_id"],
                        "score": score,
                        "response": response_text,
                    }

                    # Add token usage and cost if available
                    if token_usage:
                        result["token_usage"] = token_usage
                    if cost:
                        result["cost"] = cost
                        total_prompt_cost += cost["prompt_cost"]
                        total_completion_cost += cost["completion_cost"]
                        total_cost += cost["total_cost"]
                        total_tokens += token_usage["total_tokens"]

                    json.dump(result, fout)
                    fout.write("\n")
                    fout.flush()  # Ensure each result is written immediately
                    logging.info(
                        f"Question {question['prompt_id']} scored successfully."
                    )
                else:
                    logging.info(
                        f"Question {question['prompt_id']} skipped due to scoring failure."
                    )
            except Exception as e:
                logging.error(
                    f"Error processing prompt {question['prompt_id']}: {e}"
                )

    # Log total cost summary at the end
    if total_cost > 0:
        logging.info(f"Total tokens used: {total_tokens}")
        logging.info(f"Total prompt cost: ${total_prompt_cost:.4f}")
        logging.info(f"Total completion cost: ${total_completion_cost:.4f}")
        logging.info(f"Total cost: ${total_cost:.4f}")


if __name__ == "__main__":
    main()
