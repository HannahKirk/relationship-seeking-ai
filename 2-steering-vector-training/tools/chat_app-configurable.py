import argparse
import os
import sys
import json
import threading
import time
import queue

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

os.environ["HF_HOME"] = "/extra-space/model_cache"
import torch
from flask import Flask, jsonify, request, send_from_directory, Response
from flask_cors import CORS
from BiPO_distributed.model_wrapper import ModelWrapper

app = Flask(__name__)
CORS(app)

# Stream data storage for each connection
stream_data = {}

class ModelManager:
    def __init__(self, model_wrapper, vector, layer, epoch, gpu_id, model_name, display_name):
        print(f"Initializing ModelManager on GPU {gpu_id} with layer {layer} and epoch {epoch}")
        self.gpu_id = gpu_id
        self.model = model_wrapper
        self.layer = layer
        self.epoch = epoch
        self.model_name = model_name  # Full HF model name
        self.display_name = display_name  # Short name for display/vector path
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
        print(f"Using layer: {self.layer}, epoch: {self.epoch}, model: {self.display_name}")

    def chat(self, message, multiplier=1.0):
        print(f"\n=== New Chat Request on GPU {self.gpu_id} with Layer {self.layer} Epoch {self.epoch} ===")
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

    def chat_streaming(self, message, multiplier=1.0, stream_id=None):
        """Chat with streaming responses for more interactive experience"""
        print(f"\n=== New Streaming Chat Request on GPU {self.gpu_id} with Layer {self.layer} Epoch {self.epoch} ===")
        print(f"Multiplier: {multiplier}")
        print(f"Message: {message}")
        print(f"Stream ID: {stream_id}")

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

        # Ensure vector has the right shape for broadcasting
        if len(modified_vector.shape) == 1:
            modified_vector = modified_vector.unsqueeze(0)

        # Set the activations directly, ensuring they're on the model's device
        print(f"Setting activations for layer {self.layer} with multiplier {multiplier}")
        self.model.set_add_activations(self.layer, modified_vector)

        # Generate response with current history
        messages = [{"role": "user", "content": message}]
        print(f"Generating streaming response on GPU {self.gpu_id}...")

        # Initialize stream queue if needed
        config_key = f"{self.display_name}-layer{self.layer}-ep{self.epoch}"
        if stream_id not in stream_data:
            stream_data[stream_id] = {
                config_key: queue.Queue()
            }
        elif config_key not in stream_data[stream_id]:
            stream_data[stream_id][config_key] = queue.Queue()

        # Start streaming generation
        full_response = ""
        for token, current_text in self.model.generate_with_streaming(
            self.conv_history + messages, max_new_tokens=200
        ):
            # Send each token to the stream
            if stream_id:
                stream_data[stream_id][config_key].put({
                    "token": token,
                    "full_text": current_text,
                    "model": self.display_name,
                    "layer": self.layer,
                    "epoch": self.epoch,
                    "config_key": config_key,
                    "gpu_id": self.gpu_id,
                    "finished": False
                })
            full_response = current_text

        # Mark stream as finished
        if stream_id:
            stream_data[stream_id][config_key].put({
                "token": "",
                "full_text": full_response,
                "model": self.display_name,
                "layer": self.layer,
                "epoch": self.epoch,
                "config_key": config_key,
                "gpu_id": self.gpu_id,
                "finished": True
            })

        # Update conversation history
        self.conv_history.extend(messages)
        self.conv_history.append({"role": "assistant", "content": full_response})

        print(f"Streaming response completed on GPU {self.gpu_id}")
        return full_response

    def reset(self):
        print(f"\n=== Resetting Conversation on GPU {self.gpu_id} ===")
        self.conv_history = []
        self.model.reset_all()
        print(f"Reset conversation history and model activations on GPU {self.gpu_id}")


# Global dictionary to store manager instances, one for each configuration
model_managers = {}


@app.route("/")
def home():
    return send_from_directory("static", "index-multi.html")


@app.route("/config")
def config():
    """Return current configurations"""
    configs = []
    for config_key, manager in model_managers.items():
        configs.append({
            "config_key": config_key,
            "model": manager.display_name,
            "layer": manager.layer,
            "epoch": manager.epoch,
            "gpu_id": manager.gpu_id
        })
    return jsonify({"configs": configs})


@app.route("/chat", methods=["POST"])
def chat():
    try:
        data = request.json
        message = data.get("message")
        multiplier = float(data.get("multiplier", 1.0))
        stream = data.get("stream", False)  # Whether to use streaming mode

        print(f"\nReceived {'streaming ' if stream else ''}chat request:")
        print(f"Message: {message}")
        print(f"Multiplier: {multiplier}")

        if not message:
            return jsonify({"error": "No message provided"}), 400

        if not stream:
            # Generate responses from all models in parallel (non-streaming)
            responses = {}

            def get_response(config_key):
                try:
                    manager = model_managers[config_key]
                    response = manager.chat(message, multiplier)
                    responses[config_key] = {
                        "response": response,
                        "model": manager.display_name,
                        "layer": manager.layer,
                        "epoch": manager.epoch,
                        "gpu_id": manager.gpu_id
                    }
                except Exception as e:
                    print(f"Error in chat for config {config_key}: {str(e)}")
                    responses[config_key] = {
                        "error": str(e),
                        "model": manager.display_name,
                        "layer": manager.layer,
                        "epoch": manager.epoch,
                        "gpu_id": manager.gpu_id
                    }

            # Create a thread for each model
            threads = []
            for config_key in model_managers:
                thread = threading.Thread(target=get_response, args=(config_key,))
                threads.append(thread)
                thread.start()

            # Wait for all threads to complete
            for thread in threads:
                thread.join()

            # Return all responses
            return jsonify({
                "responses": responses,
                "multiplier_used": multiplier
            })
        else:
            # Start streaming mode
            stream_id = f"stream_{int(time.time())}_{id(message)}"

            # Start streaming generation for each model in parallel
            for config_key, manager in model_managers.items():
                # Create a thread for each model's streaming generation
                threading.Thread(
                    target=manager.chat_streaming,
                    args=(message, multiplier, stream_id)
                ).start()

            # Return the stream ID for the client to connect to the stream endpoint
            return jsonify({
                "stream_id": stream_id,
                "configs": [
                    {
                        "config_key": config_key,
                        "model": manager.display_name,
                        "layer": manager.layer,
                        "epoch": manager.epoch
                    } for config_key, manager in model_managers.items()
                ]
            })

    except Exception as e:
        print(f"Error in chat endpoint: {str(e)}")
        import traceback
        traceback.print_exc()  # Print full traceback for debugging
        return jsonify({"error": str(e)}), 500


@app.route("/stream/<stream_id>", methods=["GET"])
def stream(stream_id):
    """Server-Sent Events endpoint for streaming responses"""
    def generate():
        if stream_id not in stream_data:
            yield f"data: {json.dumps({'error': 'Stream not found'})}\n\n"
            return

        configs_completed = set()
        expected_configs = set(model_managers.keys())

        yield f"data: {json.dumps({'event': 'start', 'configs': list(expected_configs)})}\n\n"

        # Keep checking for new tokens until all configs are done
        while configs_completed != expected_configs:
            for config_key in expected_configs:
                if config_key in configs_completed:
                    continue

                if config_key not in stream_data[stream_id]:
                    # Config might not have started generating yet
                    continue

                try:
                    # Non-blocking get with timeout
                    data = stream_data[stream_id][config_key].get(block=False)

                    # Check if this config is now finished
                    if data["finished"]:
                        configs_completed.add(config_key)

                    # Send the token data
                    yield f"data: {json.dumps(data)}\n\n"

                except queue.Empty:
                    # No new tokens from this config yet
                    pass

            # Brief pause to avoid spinning too fast
            time.sleep(0.01)

        # All configs have completed
        yield f"data: {json.dumps({'event': 'end'})}\n\n"

        # Clean up the stream data
        del stream_data[stream_id]

    return Response(generate(), mimetype="text/event-stream")


@app.route("/reset", methods=["POST"])
def reset():
    try:
        # Reset all model managers
        for config_key, manager in model_managers.items():
            manager.reset()
        return jsonify({"status": "success"})
    except Exception as e:
        print(f"Error in reset endpoint: {str(e)}")
        return jsonify({"error": str(e)}), 500


def create_model_on_gpu(model_name, system_prompt, gpu_id):
    # Set CUDA device for this thread
    torch.cuda.set_device(gpu_id)

    print(f"Creating model on GPU {gpu_id}...")

    # We need to use the full HF model name for loading
    full_model_name = "meta-llama/Llama-3.1-70B-Instruct"  # Hardcoded to match what's in cache

    print(f"Using model: {full_model_name}")

    try:
        # Create model wrapper
        model_wrapper = ModelWrapper(
            full_model_name,
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

    except Exception as e:
        print(f"Error creating model: {str(e)}")
        import traceback
        traceback.print_exc()
        return None


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--behavior", default="relationship-seeking")
    parser.add_argument("--vector_gpu_id", type=int, default=3, help="GPU ID where vector was saved")
    parser.add_argument("--vector_base_path", type=str, default="/home/ubuntu/relationship-seeking-ai/1-steering-vector-training/BiPO/vector",
                       help="Base path for the vector directories")
    parser.add_argument("--config", type=str, nargs='+', required=True,
                      help="Configurations in format: model-layerN-epM,gpu_id e.g. 70B-layer31-ep10,0")
    args = parser.parse_args()

    print("\n=== Starting Configurable Multi-GPU Application ===")
    print(f"Behavior: {args.behavior}")
    print(f"Vector GPU: {args.vector_gpu_id}")
    print(f"Vector Base Path: {args.vector_base_path}")

    # Set environment variables to avoid auto device placement
    os.environ["CUDA_DEVICE_ORDER"] = "PCI_BUS_ID"

    # Parse configurations
    configurations = []
    for config_str in args.config:
        parts = config_str.split(',')
        if len(parts) != 2:
            print(f"Invalid configuration format: {config_str}. Expected format: model-layerN-epM,gpu_id")
            continue

        config_spec, gpu_id = parts

        # Parse the configuration
        model_layer_epoch = config_spec.split('-')
        if len(model_layer_epoch) < 3:
            print(f"Invalid config specification: {config_spec}. Must include model, layer, and epoch.")
            continue

        # Extract layer and epoch from the format
        layer_part = None
        epoch_part = None
        model_parts = []

        for part in model_layer_epoch:
            if part.startswith("layer"):
                layer_part = part
            elif part.startswith("ep"):
                epoch_part = part
            else:
                model_parts.append(part)

        if not layer_part or not epoch_part:
            print(f"Missing layer or epoch in config: {config_spec}")
            continue

        # Extract layer number and epoch number
        layer = int(layer_part.replace("layer", ""))
        epoch = int(epoch_part.replace("ep", ""))

        # Create the display name (short form)
        display_name = "-".join(model_parts)

        # For vector loading, we need the full model name that matches the folder structure
        model_name = "Llama-3.1-70B-Instruct"  # Hardcoded to match your vector path

        # Add the full configuration
        configurations.append({
            "model_name": model_name,          # Full model name for HF
            "display_name": display_name,      # Short display name (e.g., "70B")
            "layer": layer,
            "epoch": epoch,
            "gpu_id": int(gpu_id)
        })

    print(f"\nParsed {len(configurations)} configurations:")
    for i, config in enumerate(configurations):
        print(f"Configuration {i+1}:")
        print(f"  Model: {config['model_name']} (display: {config['display_name']})")
        print(f"  Layer: {config['layer']}")
        print(f"  Epoch: {config['epoch']}")
        print(f"  GPU: {config['gpu_id']}")

    # Initialize models with the specified configurations
    for config in configurations:
        model_name = config["model_name"]
        display_name = config["display_name"]
        layer = config["layer"]
        epoch = config["epoch"]
        gpu_id = config["gpu_id"]

        # Create a unique key for this configuration
        config_key = f"{display_name}-layer{layer}-ep{epoch}"

        # Construct the vector path based on the folder structure you provided
        vector_path = os.path.join(
            args.vector_base_path,
            f"{args.behavior}_{model_name}",
            f"layer{layer}",
            f"vec_ep{epoch}_layer{layer}_gpu{args.vector_gpu_id}.pt"
        )

        print(f"\nLoading vector from {vector_path}")

        try:
            if not os.path.exists(vector_path):
                print(f"ERROR: Vector file not found at {vector_path}")
                continue

            vector = torch.load(vector_path, map_location="cpu")  # First load to CPU
            print(f"Vector loaded with shape {vector.shape}")

            # Create model wrapper on specified GPU
            print(f"\nCreating model for {config_key} on GPU {gpu_id}...")
            model = create_model_on_gpu(
                model_name,
                "You are a conversational assistant.",
                gpu_id
            )

            if model is None:
                print(f"ERROR: Failed to create model for {config_key}")
                continue

            # Make sure to fully init the model
            torch.cuda.synchronize(gpu_id)

            # Create copy of vector for the model in appropriate dtype
            model_vector = vector.clone().to(dtype=model.model.dtype)

            # Create manager for the model
            print(f"\nInitializing model manager for {config_key} on GPU {gpu_id}...")
            model_managers[config_key] = ModelManager(
                model, model_vector, layer, epoch, gpu_id, model_name, display_name
            )

        except Exception as e:
            print(f"Error loading vector or initializing model for {config_key}: {str(e)}")
            import traceback
            traceback.print_exc()

    if not model_managers:
        print("No models were successfully initialized. Exiting.")
        return

    print(f"\nSuccessfully initialized {len(model_managers)} configurations")
    print("\nStarting Flask server...")
    app.run(host="0.0.0.0", port=5000)


if __name__ == "__main__":
    main()
