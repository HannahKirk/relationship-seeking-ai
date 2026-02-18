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

# Get parent directory for relative paths
PARENT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

app = Flask(__name__)
CORS(app)

# Stream data storage for each connection
stream_data = {}

class ModelManager:
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
        print(f"\n=== New Chat Request on GPU {self.gpu_id} with Layer {self.layer} ===")
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
        print(f"\n=== New Streaming Chat Request on GPU {self.gpu_id} with Layer {self.layer} ===")
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
        if stream_id not in stream_data:
            stream_data[stream_id] = {
                self.layer: queue.Queue()
            }
        elif self.layer not in stream_data[stream_id]:
            stream_data[stream_id][self.layer] = queue.Queue()

        # Start streaming generation
        full_response = ""
        for token, current_text in self.model.generate_with_streaming(
            self.conv_history + messages, max_new_tokens=200
        ):
            # Send each token to the stream
            if stream_id:
                stream_data[stream_id][self.layer].put({
                    "token": token,
                    "full_text": current_text,
                    "layer": self.layer,
                    "gpu_id": self.gpu_id,
                    "finished": False
                })
            full_response = current_text

        # Mark stream as finished
        if stream_id:
            stream_data[stream_id][self.layer].put({
                "token": "",
                "full_text": full_response,
                "layer": self.layer,
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


# Global dictionary to store manager instances, one for each GPU/layer
model_managers = {}


@app.route("/")
def home():
    return send_from_directory("static", "index-multi.html")


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

            def get_response(layer):
                try:
                    gpu_id = model_managers[layer].gpu_id
                    response = model_managers[layer].chat(message, multiplier)
                    responses[layer] = {
                        "response": response,
                        "gpu_id": gpu_id,
                        "layer": layer
                    }
                except Exception as e:
                    print(f"Error in chat for layer {layer}: {str(e)}")
                    responses[layer] = {
                        "error": str(e),
                        "gpu_id": model_managers[layer].gpu_id,
                        "layer": layer
                    }

            # Create a thread for each model
            threads = []
            for layer in model_managers:
                thread = threading.Thread(target=get_response, args=(layer,))
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
            for layer in model_managers:
                # Create a thread for each model's streaming generation
                threading.Thread(
                    target=model_managers[layer].chat_streaming,
                    args=(message, multiplier, stream_id)
                ).start()

            # Return the stream ID for the client to connect to the stream endpoint
            return jsonify({
                "stream_id": stream_id,
                "layers": list(model_managers.keys())
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

        layers_completed = set()
        expected_layers = set(model_managers.keys())

        yield f"data: {json.dumps({'event': 'start', 'layers': list(expected_layers)})}\n\n"

        # Keep checking for new tokens until all layers are done
        while layers_completed != expected_layers:
            for layer in expected_layers:
                if layer in layers_completed:
                    continue

                if layer not in stream_data[stream_id]:
                    # Layer might not have started generating yet
                    continue

                try:
                    # Non-blocking get with timeout
                    data = stream_data[stream_id][layer].get(block=False)

                    # Check if this layer is now finished
                    if data["finished"]:
                        layers_completed.add(layer)

                    # Send the token data
                    yield f"data: {json.dumps(data)}\n\n"

                except queue.Empty:
                    # No new tokens from this layer yet
                    pass

            # Brief pause to avoid spinning too fast
            time.sleep(0.01)

        # All layers have completed
        yield f"data: {json.dumps({'event': 'end'})}\n\n"

        # Clean up the stream data
        del stream_data[stream_id]

    return Response(generate(), mimetype="text/event-stream")


@app.route("/reset", methods=["POST"])
def reset():
    try:
        # Reset all model managers
        for layer, manager in model_managers.items():
            manager.reset()
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
    parser.add_argument("--epoch", type=int, default=5)
    parser.add_argument("--vector_gpu_id", type=int, default=3, help="GPU ID where vector was saved")
    args = parser.parse_args()

    print("\n=== Starting Multi-GPU Application ===")
    print(f"Model name: {args.model_name}")
    print(f"Behavior: {args.behavior}")
    print(f"Vector GPU: {args.vector_gpu_id}")

    # Set environment variables to avoid auto device placement
    os.environ["CUDA_DEVICE_ORDER"] = "PCI_BUS_ID"

    # Define the layers and corresponding GPUs
    layers_gpus = {
        27: 0,
        29: 1,
        31: 2,
        33: 3,
        35: 4
    }

    # Initialize models on each GPU
    model_name = args.model_name.split("/")[-1]

    for layer, gpu_id in layers_gpus.items():
        # Load vector for this layer to CPU first
        vector_path = os.path.join(PARENT_DIR, f"vector/{args.behavior}_{model_name}/layer{layer}/vec_ep{args.epoch}_layer{layer}_gpu{args.vector_gpu_id}.pt")
        print(f"\nLoading vector from {vector_path}")
        try:
            vector = torch.load(vector_path, map_location="cpu")  # First load to CPU
            print(f"Vector loaded with shape {vector.shape}")

            # Create model wrapper on specified GPU
            print(f"\nCreating model for layer {layer} on GPU {gpu_id}...")
            model = create_model_on_gpu(
                args.model_name,
                "You are a conversational assistant.",
                gpu_id
            )

            # Make sure to fully init the model
            torch.cuda.synchronize(gpu_id)

            # Create copy of vector for the model in appropriate dtype
            model_vector = vector.clone().to(dtype=model.model.dtype)

            # Create manager for the model
            print(f"\nInitializing model manager for layer {layer} on GPU {gpu_id}...")
            model_managers[layer] = ModelManager(model, model_vector, layer, gpu_id)

        except Exception as e:
            print(f"Error loading vector or initializing model for layer {layer}: {str(e)}")
            import traceback
            traceback.print_exc()

    if not model_managers:
        print("No models were successfully initialized. Exiting.")
        return

    print(f"\nSuccessfully initialized {len(model_managers)} models on {len(set(layers_gpus.values()))} GPUs")
    print("\nStarting Flask server...")
    app.run(host="0.0.0.0", port=5000)


if __name__ == "__main__":
    main()
