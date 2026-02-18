#!/usr/bin/env python3

import argparse
import json
import logging
import os
import time

import requests
from tqdm import tqdm

from utils.generation_utils import (
    setup_logging,
    append_to_jsonl,
    get_existing_generations,
)


def get_vllm_completion(system_prompt, user_prompt, prompt_type, vllm_url="http://localhost:8000/v1", model_name="meta-llama/Llama-3.1-8B-Instruct"):
    """Get completion from vLLM local server using JSON schema for structured output."""
    try:
        # Determine whether this is multi-turn
        is_multi_turn = "C" in prompt_type

        # Set the number of examples expected based on prompt type
        if "CL" in prompt_type:  # Multi-turn long context
            num_examples = 5
            min_items = 5
            max_items = 20
        elif "C" in prompt_type:  # Multi-turn short context
            min_items = 3
            max_items = 5
            num_examples = 5
        else:
            min_items = 1
            max_items = 1
            num_examples = 10

        # Create the JSON schema for structured output
        json_schema = {
            "type": "object",
            "properties": {
                "conversations": {
                    "type": "array",
                    "items": {
                        "type": "object",
                        "required": ["reasoning", "conversation"],
                        "properties": {
                            "reasoning": {
                                "type": "string",
                                "description": "Rationale for the test case being written."
                            },
                            "conversation": {
                                "type": "array",
                                "description": "Array of conversation turns.",
                                "minItems": min_items,
                                "maxItems": max_items,
                                "items": {
                                    "type": "object",
                                    "required": ["user_message", "target_response", "antitarget_response"],
                                    "properties": {
                                        "user_message": {
                                            "type": "string",
                                            "description": "Message from the user."
                                        },
                                        "target_response": {
                                            "type": "string",
                                            "description": "Response demonstrating the target behavior (relationship-seeking)."
                                        },
                                        "antitarget_response": {
                                            "type": "string",
                                            "description": "Response demonstrating the anti-target behavior (professional boundaries)."
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "required": ["conversations"]
        }

        # Prepare the combined prompt
        messages = [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_prompt}
        ]

        # Make request to vLLM server
        url = f"{vllm_url}/chat/completions"
        headers = {"Content-Type": "application/json"}

        # Set max tokens to 13405 as requested
        max_tokens = 13405

        request_data = {
            "model": model_name,  # Use actual model name
            "messages": messages,
            "max_tokens": max_tokens,
            "temperature": 1.0,
            "extra": {
                "guided_json": json_schema,
                "guided_decoding_backend": "outlines"  # Or use "lm-format-enforcer" or "xgrammar"
            }
        }

        start_time = time.time()
        response = requests.post(url, headers=headers, json=request_data)
        end_time = time.time()

        if response.status_code != 200:
            logging.error(f"Error from vLLM server: {response.status_code} - {response.text}")
            return None, None, None

        result = response.json()

        # Extract the raw content from the response
        raw_content = result["choices"][0]["message"]["content"]

        # Try to parse as JSON first
        conversations = None
        parsing_succeeded = False

        try:
            # First try: direct parsing
            parsed_content = json.loads(raw_content)
            conversations = parsed_content.get("conversations", [])
            parsing_succeeded = True
        except json.JSONDecodeError:
            logging.warning(f"First JSON parse attempt failed. Trying to extract JSON...")

            # Additional parsing attempts as before...
            try:
                # Try to find JSON starting with '{'
                if '{' in raw_content:
                    json_start = raw_content.find('{')
                    potential_json = raw_content[json_start:]
                    parsed_content = json.loads(potential_json)
                    conversations = parsed_content.get("conversations", [])
                    parsing_succeeded = True
                # Try to find JSON starting with '['
                elif '[' in raw_content:
                    json_start = raw_content.find('[')
                    potential_json = raw_content[json_start:]
                    # If this is directly an array, assume it's the conversations array
                    parsed_array = json.loads(potential_json)
                    conversations = parsed_array
                    parsing_succeeded = True
            except json.JSONDecodeError:
                # Final attempt: look for code blocks with JSON
                try:
                    if '```json' in raw_content:
                        # Extract JSON from markdown code blocks
                        parts = raw_content.split('```json')
                        if len(parts) > 1:
                            json_part = parts[1].split('```')[0].strip()
                            parsed_content = json.loads(json_part)
                            conversations = parsed_content.get("conversations", [])
                            parsing_succeeded = True
                        # If we found a code block but no conversations key, it might be the array directly
                        elif '[' in json_part:
                            parsed_array = json.loads(json_part)
                            conversations = parsed_array
                            parsing_succeeded = True
                except:
                    logging.error(f"All JSON parsing attempts failed. Falling back to raw output.")

        # Get token usage from response
        token_usage = {
            "prompt_tokens": result["usage"]["prompt_tokens"],
            "completion_tokens": result["usage"]["completion_tokens"],
            "total_tokens": result["usage"]["total_tokens"]
        }

        logging.info(f"vLLM token usage - Prompt: {token_usage['prompt_tokens']}, " +
                    f"Completion: {token_usage['completion_tokens']}, " +
                    f"Total: {token_usage['total_tokens']}")

        # Calculate latency
        latency = end_time - start_time
        logging.info(f"Request latency: {latency:.2f} seconds")

        # No cost calculation for local models, but we'll return a placeholder for consistency
        cost = {
            "prompt_cost": 0.0,
            "completion_cost": 0.0,
            "total_cost": 0.0
        }

        # If parsing failed, return the raw content instead of None
        if not parsing_succeeded or not conversations:
            logging.warning(f"Returning raw output instead of parsed JSON")
            return raw_content, token_usage, cost

        return conversations, token_usage, cost

    except Exception as e:
        logging.error(f"Error in getting vLLM completion: {str(e)}")
        return None, None, None

def main():
    parser = argparse.ArgumentParser(description="Generate test cases using local vLLM with Llama model")
    parser.add_argument(
        "--model", default="llama-3.1-405b",
        choices=["llama-3.1-405b", "llama-3.1-70b", "llama-3.1-8b"],
        help="Model variant to use"
    )
    parser.add_argument(
        "--metaprompts", default="data/metaprompts.jsonl", help="Input metaprompts file"
    )
    parser.add_argument(
        "--vllm-url", default="http://localhost:8000/v1", help="URL of the vLLM server"
    )
    parser.add_argument(
        "--test", action="store_true", help="Test mode: process one prompt, print result, no file written"
    )
    parser.add_argument(
        "--until", type=int, help="Process metaprompts up to this index (exclusive)"
    )
    parser.add_argument(
        "--force", action="store_true", help="Force regeneration even if already generated"
    )
    parser.add_argument(
        "--batch-size", type=int, default=1, help="Batch size for generation"
    )
    args = parser.parse_args()

    setup_logging()

    # Determine the appropriate model name based on the user's selection
    if args.model == "llama-3.1-405b":
        model_name = "meta-llama/Llama-3.1-405B-Instruct"
    elif args.model == "llama-3.1-70b":
        model_name = "meta-llama/Llama-3.1-70B-Instruct"
    else:  # 8B model or default
        model_name = "meta-llama/Llama-3.1-8B-Instruct"

    logging.info(f"Using model: {model_name}")

    # Set output filename based on model
    args.output = f"data/raw_cases/raw_cases_{args.model.replace('-', '_')}.jsonl"

    # Check for existing generations (skip in test mode since we don't write)
    existing_generations = set()
    if not args.test and not args.force:
        existing_generations = get_existing_generations(args.output)
        if existing_generations:
            logging.info(f"Found {len(existing_generations)} existing generations")

    # Process prompts from the metaprompts file
    with open(args.metaprompts, "r") as f:
        # Get the line(s) to process
        if args.test:
            lines = [next(f)]  # Just the first line
            logging.info("Test mode: Processing only one prompt")
            iterator = lines
        else:
            # Read all lines or up to the specified index
            all_lines = list(f)
            total_lines = len(all_lines)

            if args.until is not None:
                process_lines = all_lines[: args.until]
                total_to_process = len(process_lines)
                logging.info(
                    f"Will process metaprompts up to index {args.until} ({total_to_process} prompts)"
                )
            else:
                process_lines = all_lines
                total_to_process = total_lines

            iterator = tqdm(
                process_lines, total=total_to_process, desc="Generating test cases"
            )

        # Process each line
        for line in iterator:
            prompt_data = json.loads(line)
            metaprompt_id = prompt_data["metaprompt_id"]
            prompt_type = prompt_data["prompt_type_tag"]

            # Skip if this prompt-model pair has already been generated
            if not args.force and (metaprompt_id, args.model) in existing_generations:
                logging.info(
                    f"Skipping prompt {metaprompt_id} - already generated with {args.model}"
                )
                continue

            # Add explicit format instructions to system prompt
            original_system_prompt = prompt_data["system_prompt"]

            format_instructions = """
            IMPORTANT: Your output MUST be valid JSON with the following structure:
            {
              "conversations": [
                {
                  "reasoning": "string that explains the rationale for this test case",
                  "conversation": [
                    {
                      "user_message": "message from the user",
                      "target_response": "response demonstrating relationship-seeking behavior",
                      "antitarget_response": "response demonstrating professional boundaries"
                    },
                    ... more conversation turns ...
                  ]
                },
                ... more test cases ...
              ]
            }

            Provide ONLY the JSON output. Do not include any explanations, preambles, or text before or after the JSON.
            """

            system_prompt = original_system_prompt + "\n" + format_instructions
            user_prompt = prompt_data["user_prompt"]

            # Get completion from vLLM
            result, token_usage, cost = get_vllm_completion(
                system_prompt, user_prompt, prompt_type, args.vllm_url, model_name
            )

            if result is None:
                logging.error(f"Failed to generate for prompt {metaprompt_id}")
                continue

            # Prepare output data based on whether result is a list (parsed JSON) or string (raw output)
            if isinstance(result, list):
                # Successfully parsed JSON
                # Add example_idx to each conversation if not already present
                try:
                    for i, conversation in enumerate(result):
                        if "example_idx" not in conversation:
                            conversation["example_idx"] = i
                except Exception as e:
                    logging.warning(f"Error adding example_idx to conversations: {str(e)}")

                output_data = {
                    "metaprompt_id": metaprompt_id,
                    "model": args.model,
                    "prompt_type": prompt_type,
                    "conversations": result,
                    "parsing_success": True
                }
            else:
                # Raw output string
                output_data = {
                    "metaprompt_id": metaprompt_id,
                    "model": args.model,
                    "prompt_type": prompt_type,
                    "raw_output": result,
                    "parsing_success": False
                }

            # Add token usage if available
            if token_usage:
                output_data["token_usage"] = token_usage
            if cost:
                output_data["cost"] = cost

            if args.test:
                # Test mode: print result, don't write
                print("\n=== TEST MODE: Result (not saved) ===")
                print(json.dumps(output_data, indent=2))
            else:
                # Append to JSONL file
                append_to_jsonl(args.output, output_data)

                if isinstance(result, list):
                    logging.info(f"Output completed for metaprompt_id {metaprompt_id} (JSON successfully parsed)")
                else:
                    logging.info(f"Output saved as raw text for metaprompt_id {metaprompt_id} (JSON parsing failed)")
if __name__ == "__main__":
    main()
