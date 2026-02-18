"""
Generate multi-turn conversations with steered Llama models for human studies.

This script generates pre-populated conversations across different steering multipliers
for use in human evaluation experiments.
"""

import argparse
import json
import logging
import os
from datetime import datetime
from pathlib import Path
import sys

import torch
from tqdm import tqdm

# Get the project root directory
SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent.parent

# Add path for model wrapper import
sys.path.append(str(PROJECT_ROOT / "2-steering-vector-training" / "BiPO_distributed"))

from model_wrapper import ModelWrapper
from utils.calibration_stimuli_building_blocks import CHAT_SCENARIOS

# Setup logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s"
)


def get_timestamp():
    """Get current timestamp in ISO format."""
    return datetime.now().isoformat()


def load_jsonl(file_path):
    """Load data from a JSONL file."""
    data = []
    with open(file_path, "r", encoding="utf-8") as f:
        for line in f:
            if line.strip():
                try:
                    data.append(json.loads(line))
                except json.JSONDecodeError as e:
                    logging.warning(f"Error parsing line in {file_path}: {e}")
    return data


def append_to_jsonl(file_path, data):
    """Append data to a JSONL file."""
    os.makedirs(os.path.dirname(file_path), exist_ok=True)
    with open(file_path, "a", encoding="utf-8") as f:
        f.write(json.dumps(data, ensure_ascii=False) + "\n")


# Steering multipliers used in experiments
MULTIPLIERS = [-1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5]


def load_vector(vector_path, device, model_dtype):
    """Load the steering vector from the given path with proper device and dtype."""
    if not os.path.exists(vector_path):
        logging.error(f"Vector file not found at {vector_path}")
        return None

    vector = torch.load(vector_path, map_location=device).to(dtype=model_dtype)

    # Ensure correct shape (1, -1)
    if len(vector.shape) == 1:
        vector = vector.view(1, -1)

    logging.info(
        f"Vector loaded with shape {vector.shape}, dtype {vector.dtype}, device {vector.device}"
    )
    return vector


def get_llama_response(
    model_manager, vector, conversation_history, multiplier, layer, max_tokens=600
):
    """Get a response from Llama model with steering vector applied."""
    model_manager.reset_all()

    # Apply steering vector with multiplier
    modified_vector = multiplier * vector
    model_manager.set_add_activations(layer, modified_vector)

    response = model_manager.generate_text_with_conversation_history(
        conversation_history,
        max_new_tokens=max_tokens,
    )
    return response


def process_conversation(
    model_config, chat_name, chat_data, multipliers, output_file, gpu_id=None
):
    """Process a single conversation across all multipliers."""
    logging.info(f"Processing {chat_name} with config {model_config} on GPU {gpu_id}")

    # Check for existing conversations to skip
    existing_convos = set()
    if os.path.exists(output_file):
        for record in load_jsonl(output_file):
            if record.get("chat_name") == chat_name:
                existing_convos.add((record.get("chat_name"), record.get("multiplier")))
    logging.info(f"Found {len(existing_convos)} existing conversations for {chat_name}")

    user_messages = chat_data["user_messages"]
    first_assistant_message = chat_data["first_assistant_message"]

    # Set device
    if gpu_id is not None:
        os.environ["CUDA_VISIBLE_DEVICES"] = str(gpu_id)
        device = "cuda:0"
    else:
        device = "cuda" if torch.cuda.is_available() else "cpu"
    logging.info(f"Using device: {device}")

    # Parse model config (format: "70B-layer31-ep10")
    parts = model_config.split("-")
    model_size = parts[0]
    layer = int([p for p in parts if p.startswith("layer")][0].replace("layer", ""))
    epoch = int([p for p in parts if p.startswith("ep")][0].replace("ep", ""))

    # Initialize model
    logging.info(f"Loading model meta-llama/Llama-3.1-70B-Instruct on device {device}")
    model_manager = ModelWrapper(
        "meta-llama/Llama-3.1-70B-Instruct", "You are a conversational assistant."
    )
    model_manager.model = model_manager.model.to(device)

    # Load steering vector
    vector_path = (
        PROJECT_ROOT
        / "2-steering-vector-training"
        / "vector"
        / f"relationship-seeking_Llama-3.1-70B-Instruct"
        / f"layer{layer}"
        / f"vec_ep{epoch}_layer{layer}_gpu3.pt"
    )
    vector = load_vector(vector_path, device, model_manager.model.dtype)
    if vector is None:
        return

    # Process each multiplier
    for multiplier in tqdm(multipliers, desc=f"Processing multipliers for {chat_name}"):
        if (chat_name, multiplier) in existing_convos:
            logging.info(
                f"Skipping {chat_name} with multiplier {multiplier} - already exists"
            )
            continue

        # Build conversation
        conversation_history = [
            {"role": "user", "content": user_messages[0]},
            {"role": "assistant", "content": first_assistant_message},
        ]

        # Generate responses for remaining turns
        for i in range(1, len(user_messages)):
            conversation_history.append({"role": "user", "content": user_messages[i]})
            response = get_llama_response(
                model_manager, vector, conversation_history, multiplier, layer
            )
            conversation_history.append({"role": "assistant", "content": response})

        # Save record
        record = {
            "chat_name": chat_name,
            "model_config": model_config,
            "layer": layer,
            "epoch": epoch,
            "multiplier": multiplier,
            "timestamp": get_timestamp(),
            "conversation_arr": conversation_history.copy(),
        }
        append_to_jsonl(output_file, record)

    # Cleanup
    model_manager.reset_all()
    del model_manager
    torch.cuda.empty_cache()


def main():
    default_output_dir = str(
        SCRIPT_DIR.parent
        / "stimuli"
        / "calibration_study"
        / "inputs"
        / "prepopulated_convos"
    )

    parser = argparse.ArgumentParser(
        description="Generate multi-turn conversations with steered Llama"
    )
    parser.add_argument(
        "--model-config",
        type=str,
        default="70B-layer31-ep10",
        help="Model configuration (e.g., '70B-layer31-ep10')",
    )
    parser.add_argument(
        "--output-dir", default=default_output_dir, help="Output directory for results"
    )
    parser.add_argument("--gpu-id", type=int, help="GPU ID to use")
    args = parser.parse_args()

    os.makedirs(args.output_dir, exist_ok=True)

    output_file = os.path.join(args.output_dir, f"llama_{args.model_config}.jsonl")

    for chat_name, chat_data in CHAT_SCENARIOS.items():
        process_conversation(
            args.model_config,
            chat_name,
            chat_data,
            MULTIPLIERS,
            output_file,
            args.gpu_id,
        )

    logging.info(f"All conversations saved to {output_file}")


if __name__ == "__main__":
    main()
