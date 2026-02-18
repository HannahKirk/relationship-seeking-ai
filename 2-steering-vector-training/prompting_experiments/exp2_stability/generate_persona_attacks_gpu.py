#!/usr/bin/env python3
import argparse
import json
import logging
import os
import time
from datetime import datetime
from pathlib import Path
import random
import sys

import torch
import requests
from tqdm import tqdm
from anthropic import Anthropic
from openai import OpenAI

# Add parent directories to path for imports
script_dir = Path(__file__).parent
sys.path.insert(0, str(script_dir.parent.parent))  # 2-steering-vector-training/
sys.path.insert(0, str(script_dir.parent.parent.parent))  # relationship-seeking-ai/

from setup.api_utils import get_api_key
from BiPO_distributed.model_wrapper import ModelWrapper

# Import shared functions from exp1_steerability
from prompting_experiments.exp1_steerability.prompt_baseline_funcs import (
    get_timestamp,
    append_to_jsonl,
    load_jsonl,
    load_json,
    get_behavior_definition,
    sample_examples,
    create_system_prompt,
    get_claude_completion,
    get_gpt_completion,
    get_multiplier_level_pairs,
    format_conversation_history,
    calculate_cost
)
from stability_scenarios import THEMES, create_persona_stability_scenarios

# Setup logging configuration
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s"
)


def load_vector(vector_path, device, model_dtype):
    """Load the steering vector from the given path with proper device and dtype."""
    if not os.path.exists(vector_path):
        logging.error(f"Vector file not found at {vector_path}")
        return None

    # Load vector with correct device and dtype
    vector = torch.load(vector_path, map_location=device).to(dtype=model_dtype)

    # Ensure correct shape (1, -1)
    if len(vector.shape) == 1:
        vector = vector.view(1, -1)

    logging.info(f"Vector loaded with shape {vector.shape}, dtype {vector.dtype}, device {vector.device}")
    return vector


def get_llama_response(model_manager, vector, conversation_history, multiplier, layer, max_tokens=50):
    """Get a response from Llama model with steering vector applied."""
    # Reset any existing activations
    model_manager.reset_all()

    # Apply modified vector with multiplier
    modified_vector = multiplier * vector
    model_manager.set_add_activations(layer, modified_vector)

    # Generate response
    response = model_manager.generate_text_with_conversation_history(
        conversation_history, max_new_tokens=max_tokens,
    )
    return response


def process_stability_experiment(model_type, model_config, persona_stability_scenarios, multipliers,
                                output_file, base_prompts_file, examples_file, num_levels, n_examples, gpu_id=None,
                                behavior="relationship-seeking"):
    """
    Process stability experiment across different scenarios, tracking how models respond
    to attempts to change their anthropomorphism level.
    """
    logging.info(f"Running stability experiment with {model_type} {model_config} on GPU {gpu_id}")

    # Get multiplier-level pairs from the centralized function
    multiplier_level_pairs = get_multiplier_level_pairs(num_levels)

    # Initialize cost tracking
    total_cost = 0.0
    prompt_cost = 0.0
    completion_cost = 0.0

    # Check if output file exists, and if so, load existing conversations
    existing_records = []
    if os.path.exists(output_file):
        existing_records = load_jsonl(output_file)

    # Create a set of (scenario_name, multiplier) pairs that already exist
    existing_convos = set()
    for record in existing_records:
        if "scenario_name" in record and "multiplier" in record:
            existing_convos.add((record.get("scenario_name"), record.get("multiplier")))

    logging.info(f"Found {len(existing_convos)} existing experiment runs")

    # Set device if specified
    if gpu_id is not None:
        os.environ["CUDA_VISIBLE_DEVICES"] = str(gpu_id)
        device = f"cuda:0"  # When using CUDA_VISIBLE_DEVICES, the device is always 0
    else:
        device = "cuda" if torch.cuda.is_available() else "cpu"

    logging.info(f"Using device: {device}")

    # Initialize appropriate client or model
    if model_type == "claude":
        api_key = get_api_key("ANTHROPIC_API_KEY")
        client = Anthropic(api_key=api_key)
        model_name = "claude-3-7-sonnet"
        layer = None
        epoch = None
        vector = None
        model_manager = None

    elif model_type == "gpt":
        api_key = get_api_key("OPENAI_API_KEY")
        client = OpenAI(api_key=api_key)
        model_name = "gpt-4o"
        layer = None
        epoch = None
        vector = None
        model_manager = None

    elif model_type == "llama":
        client = None
        # Parse model config to get layer and epoch
        # Format: "70B-layer31-ep10"
        parts = model_config.split("-")
        model_name = parts[0]  # e.g., "70B"

        layer_part = [p for p in parts if p.startswith("layer")][0]
        layer = int(layer_part.replace("layer", ""))

        epoch_part = [p for p in parts if p.startswith("ep")][0]
        epoch = int(epoch_part.replace("ep", ""))

        # Initialize model
        logging.info(f"Loading model meta-llama/Llama-3.1-70B-Instruct on device {device}")
        system_prompt = "You are a conversational assistant."
        model_manager = ModelWrapper(
            "meta-llama/Llama-3.1-70B-Instruct",
            system_prompt
        )

        # Move model to the specified device
        model_manager.model = model_manager.model.to(device)

        # Construct vector path (relative to script location)
        full_model_name = "Llama-3.1-70B-Instruct"
        vector_base_path = script_dir.parent.parent / "vector"  # 2-steering-vector-training/vector/
        vector_gpu_id = 3  # Default GPU ID where vector was saved

        vector_path = os.path.join(
            vector_base_path,
            f"{behavior}_{full_model_name}",
            f"layer{layer}",
            f"vec_ep{epoch}_layer{layer}_gpu{vector_gpu_id}.pt"
        )

        # Load vector with proper dtype matching the model
        vector = load_vector(vector_path, device, model_manager.model.dtype)
        if vector is None:
            return

    else:
        logging.error(f"Unknown model type: {model_type}")
        return

    # Process each multiplier
    for multiplier in tqdm(multipliers, desc=f"Processing multipliers for stability experiment"):
        # Process each scenario
        for scenario_name, scenario_data in persona_stability_scenarios.items():
            # Skip if this scenario and multiplier combination already exists
            if (scenario_name, multiplier) in existing_convos:
                logging.info(f"Skipping {scenario_name} with multiplier {multiplier} - already exists")
                continue

            logging.info(f"Running {scenario_name} with multiplier {multiplier}")

            # Initialize conversation with first message and assistant response
            conversation_history = []

            # First user message and assistant response
            conversation_history.append({"role": "user", "content": scenario_data["initial_message"]})

            # Generate first assistant response based on model type
            if model_type == "claude":
                # Find the level corresponding to this multiplier
                level = next((l for m, l in multiplier_level_pairs if m == multiplier), None)
                if level is None:
                    raise ValueError(f"Multiplier {multiplier} not found in multiplier-level pairs")

                # Format conversation for Claude
                conv_history_formatted = format_conversation_history(conversation_history)

                # Load examples
                examples_data = load_json(examples_file)
                examples = sample_examples(examples_data, n_examples)

                # Create system prompt
                system_prompt = create_system_prompt(examples, level, num_levels, n_examples)

                # Get response using get_claude_completion
                first_response, token_usage = get_claude_completion(system_prompt, conv_history_formatted, client, max_tokens=50)

                # Calculate cost for this API call
                if token_usage:
                    call_cost = calculate_cost("claude",
                                              token_usage["prompt_tokens"],
                                              token_usage["completion_tokens"])

                    # Update cost tracking
                    prompt_cost += call_cost["prompt_cost"]
                    completion_cost += call_cost["completion_cost"]
                    total_cost += call_cost["total_cost"]

            elif model_type == "gpt":
                level = next((l for m, l in multiplier_level_pairs if m == multiplier), None)
                if level is None:
                    raise ValueError(f"Multiplier {multiplier} not found in multiplier-level pairs")

                # Format conversation for GPT
                conv_history_formatted = format_conversation_history(conversation_history)

                # Load examples
                examples_data = load_json(examples_file)
                examples = sample_examples(examples_data, n_examples)

                # Create system prompt
                system_prompt = create_system_prompt(examples, level, num_levels, n_examples)

                # Get response
                first_response, token_usage = get_gpt_completion(system_prompt, conv_history_formatted, client, max_tokens=50)

                # Calculate cost for this API call
                if token_usage:
                    call_cost = calculate_cost("gpt",
                                              token_usage["prompt_tokens"],
                                              token_usage["completion_tokens"])

                    # Update cost tracking
                    prompt_cost += call_cost["prompt_cost"]
                    completion_cost += call_cost["completion_cost"]
                    total_cost += call_cost["total_cost"]

            elif model_type == "llama":
                first_response = get_llama_response(model_manager, vector, conversation_history, multiplier, layer)

            # Add first assistant response to history
            conversation_history.append({"role": "assistant", "content": first_response})

            # Process regular conversation turns (before persona attack)
            for turn in scenario_data["regular_turns"]:
                # Add user message
                conversation_history.append({"role": "user", "content": turn})

                # Generate assistant response based on model type
                if model_type == "claude":
                    # Format conversation for Claude
                    conv_history_formatted = format_conversation_history(conversation_history)

                    # Keep using the same system prompt for consistency
                    regular_response, token_usage = get_claude_completion(system_prompt, conv_history_formatted, client, max_tokens=50)

                    # Update cost tracking
                    if token_usage:
                        call_cost = calculate_cost("claude",
                                                  token_usage["prompt_tokens"],
                                                  token_usage["completion_tokens"])
                        prompt_cost += call_cost["prompt_cost"]
                        completion_cost += call_cost["completion_cost"]
                        total_cost += call_cost["total_cost"]

                elif model_type == "gpt":
                    # Format conversation for GPT
                    conv_history_formatted = format_conversation_history(conversation_history)

                    # Keep using the same system prompt for consistency
                    regular_response, token_usage = get_gpt_completion(system_prompt, conv_history_formatted, client, max_tokens=50)

                    # Update cost tracking
                    if token_usage:
                        call_cost = calculate_cost("gpt",
                                                  token_usage["prompt_tokens"],
                                                  token_usage["completion_tokens"])
                        prompt_cost += call_cost["prompt_cost"]
                        completion_cost += call_cost["completion_cost"]
                        total_cost += call_cost["total_cost"]

                elif model_type == "llama":
                    regular_response = get_llama_response(model_manager, vector, conversation_history, multiplier, layer)

                # Add assistant response to history
                conversation_history.append({"role": "assistant", "content": regular_response})

            # Now process the persona attack turns
            persona_responses = []

            for attack in scenario_data["persona_attacks"]:
                # Add user attack message
                conversation_history.append({"role": "user", "content": attack})

                # Generate assistant response based on model type
                if model_type == "claude":
                    # Format conversation for Claude
                    conv_history_formatted = format_conversation_history(conversation_history)

                    # Keep using the same system prompt
                    attack_response, token_usage = get_claude_completion(system_prompt, conv_history_formatted, client, max_tokens=50)

                    # Update cost tracking
                    if token_usage:
                        call_cost = calculate_cost("claude",
                                                  token_usage["prompt_tokens"],
                                                  token_usage["completion_tokens"])
                        prompt_cost += call_cost["prompt_cost"]
                        completion_cost += call_cost["completion_cost"]
                        total_cost += call_cost["total_cost"]

                elif model_type == "gpt":
                    # Format conversation for GPT
                    conv_history_formatted = format_conversation_history(conversation_history)

                    # Keep using the same system prompt
                    attack_response, token_usage = get_gpt_completion(system_prompt, conv_history_formatted, client, max_tokens=50)

                    # Update cost tracking
                    if token_usage:
                        call_cost = calculate_cost("gpt",
                                                  token_usage["prompt_tokens"],
                                                  token_usage["completion_tokens"])
                        prompt_cost += call_cost["prompt_cost"]
                        completion_cost += call_cost["completion_cost"]
                        total_cost += call_cost["total_cost"]

                elif model_type == "llama":
                    attack_response = get_llama_response(model_manager, vector, conversation_history, multiplier, layer)

                # Add assistant response to history
                conversation_history.append({"role": "assistant", "content": attack_response})

                # Store the response for analysis
                persona_responses.append(attack_response)

            # Create record for completed experiment
            record = {
                "scenario_name": scenario_name,
                "model_type": model_type,
                "model_name": model_name,
                "layer": layer,
                "epoch": epoch,
                "multiplier": multiplier,
                "level": level if model_type in ["claude", "gpt"] else None,
                "timestamp": get_timestamp(),
                "conversation_history": conversation_history.copy(),
                "first_response": first_response,
                "persona_responses": persona_responses
            }

            # Add cost information for cloud models
            if model_type in ["claude", "gpt"]:
                record["cost"] = {
                    "prompt_cost": prompt_cost,
                    "completion_cost": completion_cost,
                    "total_cost": total_cost,
                }

            # Append to output file
            append_to_jsonl(output_file, record)

            # Small delay to avoid API rate limits for cloud models
            if model_type in ["claude", "gpt"]:
                time.sleep(0.5)

    # Log the total cost
    if model_type in ["claude", "gpt"]:
        logging.info(f"Total cost for stability experiment with {model_type}: ${total_cost:.4f}")

    # Clean up
    if model_type == "llama" and model_manager is not None:
        model_manager.reset_all()
        # Free GPU memory
        del model_manager
        torch.cuda.empty_cache()

def main():
    parser = argparse.ArgumentParser(description="Run persona stability experiments")
    parser.add_argument("--model-type", choices=["llama", "claude", "gpt"], required=True,
                        help="Type of model to use")
    parser.add_argument("--model-config", type=str, required=True,
                        help="Model configuration (e.g., '70B-layer31-ep10')")
    parser.add_argument("--theme", type=str, required=True, choices=THEMES,
                        help="Theme to run")
    parser.add_argument("--output-dir", default="persona_stability_experiments",
                        help="Output directory for results")
    parser.add_argument("--gpu-id", type=int, required=True,
                        help="GPU ID to use (for Llama models)")
    parser.add_argument("--behavior", default="relationship-seeking",
                        help="Behavior name for vector directory")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)

    # Define persona stability scenarios for the given theme
    persona_stability_scenarios = create_persona_stability_scenarios(args.theme)

    multiplier_level_pairs = get_multiplier_level_pairs(7)  # 7 levels
    multipliers = [m for m, _ in multiplier_level_pairs]

    # Set output filename based on theme and model
    output_file = os.path.join(args.output_dir, f"persona_stability_{args.model_type}_{args.model_config}_{args.theme}.jsonl")

    # Assign GPU
    os.environ["CUDA_VISIBLE_DEVICES"] = str(args.gpu_id)

    # Data paths relative to repo root
    repo_root = script_dir.parent.parent.parent  # relationship-seeking-ai/
    base_prompts_file = repo_root / "1-dataset-generation/data/prompt_components/base_prompts.jsonl"
    examples_file = repo_root / "1-dataset-generation/data/prompt_components/examples.json"

    process_stability_experiment(
        args.model_type,
        args.model_config,
        persona_stability_scenarios,
        multipliers,
        output_file,
        str(base_prompts_file),
        str(examples_file),
        7,  # num_levels
        3,  # n_examples
        args.gpu_id,
        args.behavior
    )

    logging.info(f"Persona stability experiment for theme '{args.theme}' completed and saved to {output_file}")

if __name__ == "__main__":
    main()
