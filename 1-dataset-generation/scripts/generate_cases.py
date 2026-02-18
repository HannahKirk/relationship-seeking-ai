import argparse
import json
import logging
import os
import sys

# Add repo root to path for shared utilities
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from anthropic import Anthropic
from openai import OpenAI
from tqdm import tqdm

from setup.api_utils import get_api_key
from utils.generation_utils import (
    setup_logging,
    append_to_jsonl,
    get_existing_generations,
    calculate_cost,
)


def get_claude_completion(system_prompt, user_prompt, anthropic_client, prompt_type):
    """Get completion from Anthropic's Claude model using the new format."""
    try:
        # Determine whether this is multi-turn
        is_multi_turn = "C" in prompt_type
        model_name = "claude-3-7-sonnet-20250219"

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

        response = anthropic_client.messages.create(
            model="claude-3-7-sonnet-20250219",
            max_tokens=16384,
            messages=[{"role": "user", "content": user_prompt}],
            system=system_prompt,
            temperature=1.0,
            tools=[
                {
                    "name": "json_mode",
                    "description": f"Output {num_examples} {'multi-turn' if is_multi_turn else 'single-turn'} conversations in the new format.",
                    "input_schema": {
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
                }
            ],
            tool_choice={"type": "tool", "name": "json_mode"},
        )

        # Get token usage
        token_usage = {
            "prompt_tokens": response.usage.input_tokens,
            "completion_tokens": response.usage.output_tokens,
            "total_tokens": response.usage.input_tokens + response.usage.output_tokens
        }

        logging.info(f"Claude token usage - Prompt: {token_usage['prompt_tokens']}, Completion: {token_usage['completion_tokens']}, Total: {token_usage['total_tokens']}")

        # Calculate cost
        cost = calculate_cost(model_name, token_usage["prompt_tokens"], token_usage["completion_tokens"])
        if cost:
            logging.info(f"Estimated cost - Prompt: ${cost['prompt_cost']:.4f}, Completion: ${cost['completion_cost']:.4f}, Total: ${cost['total_cost']:.4f}")

        # Store the raw response for fallback
        try:
            raw_output = response.content[0].input

            # Try to parse the response
            # 'response.content[0].input' is typically a dict with key "conversations",
            # but that value might itself be a raw JSON string.
            value = raw_output

            # If `value` is a dict containing "conversations":
            if isinstance(value, dict) and "conversations" in value:
                maybe_str = value["conversations"]
                if isinstance(maybe_str, str):
                    # This is the raw JSON array as a string → parse it
                    result = json.loads(maybe_str)
                else:
                    # If it's already a list/dict, just use it
                    result = maybe_str
            # If `value` is already a JSON string with the entire schema
            elif isinstance(value, str):
                result = json.loads(value)
            else:
                # fallback
                result = value

            # At this point, 'result' should be the parsed JSON list of conversation objects
            conversations = result
            parsing_success = True

            return conversations, token_usage, cost, parsing_success, raw_output

        except Exception as e:
            logging.warning(f"Error parsing Claude JSON response: {str(e)}")
            # Return the raw output if parsing fails
            raw_output = response.content[0].input
            if isinstance(raw_output, dict):
                raw_output = json.dumps(raw_output)
            parsing_success = False
            return raw_output, token_usage, cost, parsing_success, raw_output

    except Exception as e:
        logging.error(f"Error in getting Claude completion: {str(e)}")
        return None, None, None, False, None


def get_gpt_completion(system_prompt, user_prompt, openai_client, prompt_type):
    """Get completion from OpenAI's GPT model using the new format."""
    try:
        # Determine whether this is multi-turn
        is_multi_turn = "C" in prompt_type
        model_name = "gpt-4o-2024-08-06"

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

        response = openai_client.chat.completions.create(
            model=model_name,
            messages=[
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ],
            max_tokens=16384,
            temperature=1.0,
            tools=[
                {
                    "type": "function",
                    "function": {
                        "name": "output_conversations",
                        "description": f"Output {num_examples} {'multi-turn' if is_multi_turn else 'single-turn'} conversations in the new format.",
                        "parameters": {
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
                    }
                }
            ],
            tool_choice={"type": "function", "function": {"name": "output_conversations"}},
        )

        # Get token usage directly from the response
        token_usage = {
            "prompt_tokens": response.usage.prompt_tokens,
            "completion_tokens": response.usage.completion_tokens,
            "total_tokens": response.usage.total_tokens
        }

        logging.info(f"GPT token usage - Prompt: {token_usage['prompt_tokens']}, Completion: {token_usage['completion_tokens']}, Total: {token_usage['total_tokens']}")

        # Calculate cost
        cost = calculate_cost(model_name, token_usage["prompt_tokens"], token_usage["completion_tokens"])
        if cost:
            logging.info(f"Estimated cost - Prompt: ${cost['prompt_cost']:.4f}, Completion: ${cost['completion_cost']:.4f}, Total: ${cost['total_cost']:.4f}")

        # Store the raw response for fallback
        try:
            raw_output = response.choices[0].message.tool_calls[0].function.arguments

            # Parse the response and extract the conversations array
            result = json.loads(raw_output)
            conversations = result.get("conversations", [])
            parsing_success = True

            return conversations, token_usage, cost, parsing_success, raw_output

        except Exception as e:
            logging.warning(f"Error parsing GPT JSON response: {str(e)}")
            # Return the raw output if parsing fails
            parsing_success = False
            try:
                raw_output = response.choices[0].message.tool_calls[0].function.arguments
            except:
                # If we can't even get the arguments, fallback to the content
                raw_output = response.choices[0].message.content
            return raw_output, token_usage, cost, parsing_success, raw_output

    except Exception as e:
        logging.error(f"Error in getting GPT completion: {str(e)}")
        return None, None, None, False, None


def main():
    parser = argparse.ArgumentParser(description="Generate test cases using LLM with new format")
    parser.add_argument(
        "model", choices=["claude", "gpt"], help="Model to use for generation"
    )
    parser.add_argument(
        "--metaprompts", default="data/metaprompts.jsonl", help="Input metaprompts file"
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
    args = parser.parse_args()

    setup_logging()

    # Initialize API clients
    if args.model == "gpt":
        client = OpenAI(api_key=get_api_key("OPENAI_API_KEY"))
    else:  # claude
        client = Anthropic(api_key=get_api_key("ANTHROPIC_API_KEY"))

    args.output = f"data/raw_cases/raw_cases_{args.model}.jsonl"

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

            system_prompt = prompt_data["system_prompt"]
            user_prompt = prompt_data["user_prompt"]

            # Get completion from selected model
            if args.model == "claude":
                result, token_usage, cost, parsing_success, raw_output = get_claude_completion(
                    system_prompt, user_prompt, client, prompt_type
                )
            else:
                result, token_usage, cost, parsing_success, raw_output = get_gpt_completion(
                    system_prompt, user_prompt, client, prompt_type
                )

            if result is not None:
                # Prepare output data based on whether parsing succeeded
                if parsing_success:
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
                    # Raw output string/dict when parsing failed
                    output_data = {
                        "metaprompt_id": metaprompt_id,
                        "model": args.model,
                        "prompt_type": prompt_type,
                        "raw_output": result,
                        "parsing_success": False
                    }

                # Add token usage and cost if available
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

                    if parsing_success:
                        logging.info(f"Output completed for metaprompt_id {metaprompt_id} (JSON successfully parsed)")
                    else:
                        logging.info(f"Output saved as raw text for metaprompt_id {metaprompt_id} (JSON parsing failed)")
            else:
                logging.error(f"Failed to generate for prompt {metaprompt_id}")


if __name__ == "__main__":
    main()
