#!/usr/bin/env python3
import argparse
import json
import logging
import os
import sys
import time
from pathlib import Path
from tqdm import tqdm
from anthropic import Anthropic
from openai import OpenAI

# Add parent directories to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", ".."))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", ".."))

from setup.api_utils import get_api_key

# Import shared functions (local to this folder)
from prompt_baseline_funcs import (
    get_timestamp, load_jsonl, load_json, append_to_jsonl, get_existing_generations,
    get_behavior_definition, sample_examples, format_conversation_history,
    create_system_prompt, get_claude_completion, get_gpt_completion,
    get_llama_completion, calculate_cost, get_multiplier_level_pairs, save_example_prompt
)

def main():
    parser = argparse.ArgumentParser(description="Generate prompting baseline responses")
    parser.add_argument(
        "model",
        choices=["llama", "gpt", "claude"],
        help="Model to use for generation"
    )
    # Get default paths relative to repo root
    script_dir = Path(__file__).parent
    repo_root = script_dir.parent.parent.parent  # relationship-seeking-ai/

    parser.add_argument(
        "--behavior",
        default="relationship-seeking",
        help="Behavior name for data directory"
    )
    parser.add_argument(
        "--test-file",
        default=None,  # Will be set based on --behavior if not provided
        help="Path to test prompts file (default: data/{behavior}/test.jsonl)"
    )
    parser.add_argument(
        "--examples-file",
        default=str(repo_root / "1-dataset-generation/data/prompt_components/examples.json"),
        help="Path to examples file"
    )
    parser.add_argument(
        "--vllm-url",
        default="http://localhost:8000/v1",
        help="URL of the vLLM server for Llama model"
    )
    parser.add_argument(
        "--limit",
        type=int,
        help="Limit the number of test prompts to process"
    )
    parser.add_argument(
        "--force",
        action="store_true",
        help="Force regeneration even if already generated"
    )
    parser.add_argument(
        "--level",
        type=int,
        choices=range(1, 10),  # Support up to 9 levels
        default=None,
        help="Specific level to generate. If not specified, all levels will be generated."
    )
    parser.add_argument(
        "--num-levels",
        type=int,
        choices=[7, 9],
        default=7,
        help="Number of levels to use (7 or 9)"
    )
    parser.add_argument(
        "--save-example",
        action="store_true",
        help="Save an example prompt to a text file for reference"
    )
    args = parser.parse_args()

    # Set test_file default based on behavior if not provided
    if args.test_file is None:
        args.test_file = str(repo_root / f"data/{args.behavior}/test.jsonl")

    # Configure logging
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s - %(levelname)s - %(message)s"
    )

    # Get multiplier-level pairs
    multiplier_level_pairs = get_multiplier_level_pairs(args.num_levels)

    # Set levels to generate based on input arguments
    if args.level is not None:
        # Find the multiplier that corresponds to the requested level
        multiplier = None
        for m, l in multiplier_level_pairs:
            if l == args.level:
                multiplier = m
                break
        if multiplier is None:
            logging.error(f"Level {args.level} not found in the {args.num_levels}-level scale")
            return
        levels = [args.level]
        # Update multiplier_level_pairs to only include the specified level
        multiplier_level_pairs = [(multiplier, args.level)]
    else:
        # Use all levels
        levels = [l for _, l in multiplier_level_pairs]

    # Set output filename based on model and number of levels
    output_dir = script_dir / f"{args.model}_levels_{args.num_levels}"
    output_dir.mkdir(parents=True, exist_ok=True)
    output_file = output_dir / "generations.jsonl"

    # Check for existing generations
    existing_generations = set() if args.force else get_existing_generations(output_file)

    # Load test prompts
    test_data = load_jsonl(args.test_file)
    if args.limit:
        test_data = test_data[:args.limit]

    # Load examples
    try:
        examples_data = load_json(args.examples_file)
        # Flatten the examples from different categories into a single list
        all_examples = []
        for category, examples in examples_data.items():
            all_examples.extend(examples)
        examples_data = all_examples
    except Exception as e:
        logging.error(f"Error loading examples: {e}")
        return

    # Initialize API clients based on model
    if args.model == "gpt":
        api_key = get_api_key("OPENAI_API_KEY")
        client = OpenAI(api_key=api_key)
    elif args.model == "claude":
        api_key = get_api_key("ANTHROPIC_API_KEY")
        client = Anthropic(api_key=api_key)
    else:  # llama uses vLLM server
        client = None

    # Initialize generation ID counter
    gen_idx = 0
    # Get the highest gen_idx from existing generations if any
    if output_file.exists():
        with open(output_file, "r") as f_existing:
            for line in f_existing:
                try:
                    record = json.loads(line)
                    if "generation_id" in record and record["generation_id"] > gen_idx:
                        gen_idx = record["generation_id"]
                except Exception as e:
                    pass
        # Start from the next index
        gen_idx += 1

    # Process each test prompt
    total_levels = len(levels)
    total_prompts = len(test_data)
    total_generations = total_prompts * total_levels

    # Flag to track if we've saved an example
    example_saved = False

    with tqdm(total=total_generations, desc=f"Generating {args.model} responses") as pbar:
        # For each prompt
        for prompt_data in test_data:
            prompt_id = prompt_data["prompt_id"]
            prompt_arr = prompt_data["prompt_arr"]

            # Format conversation history
            conversation_history = format_conversation_history(prompt_arr)

            # Generate for each multiplier-level pair
            for multiplier, level in multiplier_level_pairs:
                # Skip if this prompt-level pair has already been generated
                if not args.force and (prompt_id, level) in existing_generations:
                    pbar.update(1)
                    continue

                # Sample 3 examples
                sampled_examples = sample_examples(examples_data, num_examples=3)

                # Create system prompt
                system_prompt = create_system_prompt(
                    sampled_examples,
                    level,
                    max_level=args.num_levels
                )

                # Save an example prompt if requested (only save the first one)
                if args.save_example and not example_saved:
                    save_example_prompt(output_dir, system_prompt, conversation_history)
                    example_saved = True

                # Get completion based on model
                if args.model == "claude":
                    response_str, token_usage = get_claude_completion(
                        system_prompt,
                        conversation_history,
                        client
                    )
                elif args.model == "gpt":
                    response_str, token_usage = get_gpt_completion(
                        system_prompt,
                        conversation_history,
                        client
                    )
                else:  # llama
                    response_str, token_usage = get_llama_completion(
                        system_prompt,
                        conversation_history,
                        args.vllm_url
                    )

                if response_str is None:
                    logging.error(f"Failed to generate for prompt {prompt_id} with level {level}")
                    pbar.update(1)
                    continue

                # Prepare output data
                result = {
                    "test_prompt_id": prompt_id,
                    "timestamp": get_timestamp(),
                    "model_name": args.model,
                    "multiplier": multiplier,
                    "level": level,
                    "num_levels": args.num_levels,
                    "generation_id": gen_idx,
                    "response_str": response_str,
                    "token_usage": token_usage
                }

                # Calculate and add cost if token usage is available
                if token_usage and all(k in token_usage for k in ["prompt_tokens", "completion_tokens"]):
                    cost = calculate_cost(
                        args.model,
                        token_usage["prompt_tokens"],
                        token_usage["completion_tokens"]
                    )
                    result["cost"] = cost

                # Append to JSONL file
                append_to_jsonl(output_file, result)

                # Add to existing generations
                existing_generations.add((prompt_id, level))

                # Increment generation ID
                gen_idx += 1

                # Update progress bar
                pbar.update(1)

                # Sleep briefly to avoid API rate limits
                time.sleep(0.5)

    # Final log message showing completion
    print(f"\nGeneration completed. Generated {gen_idx} responses.")


if __name__ == "__main__":
    main()
