#!/usr/bin/env python3
"""
Command-line chat interface for testing steering vectors.

Usage:
    python chat_cli.py --model_name meta-llama/Llama-3.1-8B-Instruct --layer 14 --epoch 20

Chat commands:
    - Type a message and press Enter to chat
    - Change multiplier on-the-fly: "Hello! || 1.5"
    - Type "quit" to exit
"""
import argparse
import os
import sys
import time

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import torch
from accelerate import Accelerator

from BiPO_distributed.model_wrapper import ModelWrapper


class ModelManager:
    def __init__(self, model_name, vector_path, layer, gpu_id, accelerator):
        self.accelerator = accelerator

        # Create the model wrapper with auto device mapping
        self.model = ModelWrapper(model_name, "You are a conversational assistant.")

        # Load vector and move to the device where the target layer is
        print(f"Loading vector from {vector_path}")
        # First load on CPU then we'll move it to the right device
        self.vector = torch.load(vector_path, map_location="cpu")

        # Convert vector to the same dtype as model
        self.vector = self.vector.to(dtype=torch.bfloat16)
        print(f"Vector loaded with shape {self.vector.shape}")

        self.layer = layer
        self.conv_history = []

    def chat(self, message, multiplier=1.0):
        # Clear hooks from previous conversation
        self.model.reset_all()

        # Scale vector by multiplier
        modified_vector = multiplier * self.vector
        print(f"Setting activations for layer {self.layer} with multiplier {multiplier}")

        # Set the vector to be added during forward pass
        self.model.set_add_activations(self.layer, modified_vector)

        # Prepare message and generate response
        messages = [{"role": "user", "content": message}]
        response = self.model.generate_text_with_conversation_history(
            self.conv_history + messages, max_new_tokens=200
        )

        # Update conversation history
        self.conv_history.extend(messages)
        self.conv_history.append({"role": "assistant", "content": response})

        return response


def main():
    parser = argparse.ArgumentParser(description="Command-line chat with steering vectors")
    parser.add_argument("--model_name", default="meta-llama/Llama-3.1-70B-Instruct")
    parser.add_argument("--behavior", default="relationship-seeking")
    parser.add_argument("--layer", type=int, default=31)
    parser.add_argument("--epoch", type=int, default=20)
    parser.add_argument("--gpu_id", type=int, default=3, help="GPU ID where vector was saved")
    parser.add_argument("--multiplier", type=float, default=1.0, help="Default multiplier")
    args = parser.parse_args()

    print(f"\n=== Starting Chat CLI ===")
    print(f"Model: {args.model_name}")
    print(f"Behavior: {args.behavior}")
    print(f"Layer: {args.layer}")
    print(f"Epoch: {args.epoch}")
    print(f"Default multiplier: {args.multiplier}")

    # For multi-GPU setup, we only need one accelerator instance
    accelerator = Accelerator()
    rank = accelerator.process_index

    model_name = args.model_name.split("/")[-1]
    # Path relative to parent directory (2-steering-vector-training/)
    parent_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    vector_path = os.path.join(parent_dir, f"vector/{args.behavior}_{model_name}/layer{args.layer}/vec_ep{args.epoch}_layer{args.layer}_gpu{args.gpu_id}.pt")

    manager = ModelManager(
        args.model_name, vector_path, args.layer, args.gpu_id, accelerator
    )

    if rank == 0:
        print("\nChat initialized. Type 'quit' to exit.")
        print("Enter messages in format: text || multiplier")
        print("Example: Hello! || 1.5")
        while True:
            user_input = input("\nYou: ").strip()
            if user_input.lower() == "quit":
                break

            parts = user_input.split("||")
            message = parts[0].strip()
            multiplier = float(parts[1].strip()) if len(parts) > 1 else args.multiplier

            start_time = time.time()
            response = manager.chat(message, multiplier)
            end_time = time.time()

            print(f"\nAssistant: {response}")
            print(f"Generation time: {end_time - start_time:.2f} seconds")
    else:
        while True:
            time.sleep(999999)


if __name__ == "__main__":
    main()
