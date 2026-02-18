import argparse
import os
import sys

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

os.environ["HF_HOME"] = "/extra-space/model_cache"
import torch
from flask import Flask, jsonify, request, send_from_directory
from flask_cors import CORS
from BiPO_distributed.model_wrapper import ModelWrapper

# Get parent directory for relative paths
PARENT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

app = Flask(__name__)
CORS(app)


class SingleModelManager:
    def __init__(self, model_wrapper, vector, layer, gpu_id):
        print(f"Initializing ModelManager on GPU {gpu_id} with layer {layer}")
        self.gpu_id = gpu_id
        self.model = model_wrapper
        self.layer = layer
        self.conv_history = []
        self.current_multiplier = 1.0

        # Set model device explicitly
        self.device = f"cuda:{gpu_id}"
        self.model.model = self.model.model.to(self.device)

        # Move vector to the same device as the model
        model_device = next(self.model.model.parameters()).device
        self.vector = vector.to(device=model_device)

        print(f"Vector loaded with shape {self.vector.shape} on device {self.vector.device}")
        print(f"Model is on device: {model_device}")
        print(f"Using layer: {self.layer}")

    def chat(self, message, multiplier=1.0):
        print(f"\n=== New Chat Request on GPU {self.gpu_id} ===")
        print(f"Multiplier: {multiplier}")
        print(f"Message: {message}")

        # Store multiplier
        self.current_multiplier = multiplier

        # Reset any existing activations
        self.model.reset_all()
        print(f"Reset activations on GPU {self.gpu_id}")

        # Get the current device of the model (it might have moved)
        model_device = next(self.model.model.parameters()).device
        print(f"Current model device: {model_device}")

        # Ensure model is on the correct device
        if str(model_device) != self.device:
            print(f"Model device changed from {self.device} to {model_device}. Moving it back...")
            self.model.model = self.model.model.to(self.device)
            model_device = next(self.model.model.parameters()).device
            print(f"Model now on device: {model_device}")

        # Ensure vector is on same device as model
        if self.vector.device != model_device:
            print(f"Moving vector from {self.vector.device} to {model_device}")
            self.vector = self.vector.to(model_device)

        # Compute modified vector on the same device
        modified_vector = multiplier * self.vector

        print(f"Vector shape: {modified_vector.shape}")
        print(f"Vector dtype: {modified_vector.dtype}")
        print(f"Vector device: {modified_vector.device}")

        # Ensure vector has the right shape for broadcasting
        if len(modified_vector.shape) == 1:
            modified_vector = modified_vector.unsqueeze(0)
            print(f"Reshaped vector to: {modified_vector.shape}")

        # Set the activations directly, ensuring they're on the model's device
        print(f"Setting activations for layer {self.layer} with multiplier {multiplier}")
        self.model.set_add_activations(self.layer, modified_vector)

        # Generate response with current history
        messages = [{"role": "user", "content": message}]
        print(f"Generating response on GPU {self.gpu_id}...")

        response = self.model.generate_text_with_conversation_history(
            self.conv_history + messages, max_new_tokens=200
        )

        # Update conversation history
        self.conv_history.extend(messages)
        self.conv_history.append({"role": "assistant", "content": response})

        print(f"Response generated on GPU {self.gpu_id}")
        return response

    def reset(self):
        print(f"\n=== Resetting Conversation on GPU {self.gpu_id} ===")
        self.conv_history = []
        self.model.reset_all()
        print(f"Reset conversation history and model activations on GPU {self.gpu_id}")


# Global manager instances, one per GPU
left_model_manager = None
right_model_manager = None


@app.route("/")
def home():
    return send_from_directory("static", "index.html")


@app.route("/chat", methods=["POST"])
def chat():
    try:
        data = request.json
        message = data.get("message")
        multiplier = float(data.get("multiplier", 1.0))
        chat_id = data.get("chat_id", "left")

        print(f"\nReceived chat request:")
        print(f"Message: {message}")
        print(f"Multiplier: {multiplier}")
        print(f"Chat ID: {chat_id}")

        if not message:
            return jsonify({"error": "No message provided"}), 400

        # Use the appropriate model based on chat_id
        if chat_id == "left":
            response = left_model_manager.chat(message, multiplier)
        else:
            response = right_model_manager.chat(message, multiplier)

        # Include debug info in response
        return jsonify(
            {
                "response": response,
                "debug_info": {
                    "multiplier_used": multiplier,
                    "chat_id": chat_id,
                    "gpu_id": left_model_manager.gpu_id if chat_id == "left" else right_model_manager.gpu_id
                }
            }
        )
    except Exception as e:
        print(f"Error in chat endpoint: {str(e)}")
        import traceback
        traceback.print_exc()  # Print full traceback for debugging
        return jsonify({"error": str(e)}), 500


@app.route("/reset", methods=["POST"])
def reset():
    try:
        data = request.json or {}
        chat_id = data.get("chat_id")
        print(f"\nReceived reset request for chat_id: {chat_id}")

        if chat_id is None or chat_id == "all":
            # Reset both models
            left_model_manager.reset()
            right_model_manager.reset()
        elif chat_id == "left":
            left_model_manager.reset()
        elif chat_id == "right":
            right_model_manager.reset()

        return jsonify({"status": "success"})
    except Exception as e:
        print(f"Error in reset endpoint: {str(e)}")
        return jsonify({"error": str(e)}), 500


def create_model_on_gpu(model_name, system_prompt, gpu_id):
    # Set CUDA device for this thread
    torch.cuda.set_device(gpu_id)

    print(f"Creating model on GPU {gpu_id}...")

    # Create model wrapper
    model_wrapper = ModelWrapper(
        model_name,
        system_prompt
    )

    # Force model to the specified GPU
    model_wrapper.model = model_wrapper.model.to(f"cuda:{gpu_id}")

    # Verify the model is on the correct device
    model_device = next(model_wrapper.model.parameters()).device
    print(f"Model created on device: {model_device}")

    if str(model_device) != f"cuda:{gpu_id}":
        print(f"WARNING: Model not on expected device. Expected cuda:{gpu_id}, got {model_device}")
        print("Attempting to force device again...")
        model_wrapper.model = model_wrapper.model.to(f"cuda:{gpu_id}")
        model_device = next(model_wrapper.model.parameters()).device
        print(f"After second attempt, model is on: {model_device}")

    # Override the device in model_wrapper to use our specific GPU
    model_wrapper.device = f"cuda:{gpu_id}"

    return model_wrapper


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--model_name", default="meta-llama/Llama-3.1-8B-Instruct")
    parser.add_argument("--behavior", default="relationship-seeking")
    parser.add_argument("--layer", type=int, default=15)
    parser.add_argument("--epoch", type=int, default=5)
    parser.add_argument("--left_gpu_id", type=int, default=0, help="GPU ID for left chat")
    parser.add_argument("--right_gpu_id", type=int, default=1, help="GPU ID for right chat")
    parser.add_argument("--vector_gpu_id", type=int, default=3, help="GPU ID where vector was saved")
    args = parser.parse_args()

    print("\n=== Starting Dual-GPU Application ===")
    print(f"Model name: {args.model_name}")
    print(f"Behavior: {args.behavior}")
    print(f"Layer: {args.layer}")
    print(f"Left GPU: {args.left_gpu_id}")
    print(f"Right GPU: {args.right_gpu_id}")
    print(f"Vector GPU: {args.vector_gpu_id}")

    # Set environment variables to avoid auto device placement
    os.environ["CUDA_DEVICE_ORDER"] = "PCI_BUS_ID"

    # Load vector to CPU first
    model_name = args.model_name.split("/")[-1]
    vector_path = os.path.join(PARENT_DIR, f"vector/{args.behavior}_{model_name}/layer{args.layer}/vec_ep{args.epoch}_layer{args.layer}_gpu{args.vector_gpu_id}.pt")
    print(f"Loading vector from {vector_path}")
    vector = torch.load(vector_path, map_location="cpu")  # First load to CPU
    print(f"Vector loaded with shape {vector.shape}")

    # Create model wrappers on separate GPUs
    print("\nCreating left model...")
    left_model = create_model_on_gpu(
        args.model_name,
        "You are a conversational assistant.",
        args.left_gpu_id
    )

    # Make sure to fully init the first model before starting the second one
    torch.cuda.synchronize(args.left_gpu_id)

    print("\nCreating right model...")
    right_model = create_model_on_gpu(
        args.model_name,
        "You are a conversational assistant.",
        args.right_gpu_id
    )

    torch.cuda.synchronize(args.right_gpu_id)

    # Create copy of vector for each model in appropriate dtype
    left_vector = vector.clone().to(dtype=left_model.model.dtype)
    right_vector = vector.clone().to(dtype=right_model.model.dtype)

    # Create separate managers for each model
    global left_model_manager, right_model_manager

    print("\nInitializing left model manager...")
    left_model_manager = SingleModelManager(left_model, left_vector, args.layer, args.left_gpu_id)

    print("\nInitializing right model manager...")
    right_model_manager = SingleModelManager(right_model, right_vector, args.layer, args.right_gpu_id)

    print("\nStarting Flask server...")
    app.run(host="0.0.0.0", port=5000)


if __name__ == "__main__":
    main()
