# Steering Vector Hosting

Serve Llama models with steering vector support using a modified vLLM.

```
            __--_--_-_
           ( I wish I  )
          ( were a real )
          (    llama   )
           ( in Peru! )
          o (__--_--_)
       , o
      ~)
       (_---;
        /|~|\
       / / / |
```

## Overview

This folder contains patches to add steering vector injection to [vLLM](https://github.com/vllm-project/vllm). The modifications allow you to:

1. Load a pre-trained steering vector at a specific layer
2. Control steering strength via API requests using a `custom` parameter
3. Serve steered models with vLLM's high-performance inference

## Quick Start

### 1. Clone and Install vLLM

```bash
cd 3-steering-vector-hosting

# Create virtual environment
python -m venv vllm-env
source vllm-env/bin/activate

# Install helper script dependencies (for downloading models)
pip install -r requirements.txt

# Clone vLLM (tested with v0.6.x)
git clone https://github.com/vllm-project/vllm.git
cd vllm
```

### 2. Apply Patches

Apply the three patch files to add steering support:

```bash
# From the vllm/ directory
patch -p1 < ../patches/sampling_params.patch
patch -p1 < ../patches/utils.patch
patch -p1 < ../patches/llama.patch
```

### 3. Configure Steering Vector Path

Edit `vllm/model_executor/models/llama.py` to set your steering vector path:

```python
# In SteerableLlamaDecoderLayer.__init__()
self.steering_vector = torch.load(
    "/path/to/your/vector/vec_ep10_layer31_gpu3.pt"
)
```

The vector path should point to a trained steering vector from `2-steering-vector-training/vector/`.

### 4. Install vLLM

```bash
VLLM_USE_PRECOMPILED=1 pip install --editable .
```

### 5. Start the Server

```bash
vllm serve meta-llama/Llama-3.1-70B-Instruct \
  --dtype bfloat16 \
  --chat-template ../llama-3-instruct.jinja \
  --tensor-parallel-size 8 \
  --port 8000
```

### 6. Make Steered Requests

```bash
curl -X POST http://localhost:8000/v1/chat/completions \
  -H "Content-Type: application/json" \
  -d '{
    "model": "meta-llama/Llama-3.1-70B-Instruct",
    "messages": [
      {"role": "system", "content": "You are a conversational assistant"},
      {"role": "user", "content": "Hey, how are you?"}
    ],
    "temperature": 1.0,
    "max_tokens": 100,
    "custom": {
      "steer_multiplier": 1.5
    }
  }'
```

The `steer_multiplier` controls steering strength:
- `0.0` = no steering (baseline)
- Positive values = steer toward target behavior
- Negative values = steer away from target behavior

## Patches

The `patches/` folder contains three files that modify vLLM:

| Patch | File Modified | Description |
|-------|---------------|-------------|
| `sampling_params.patch` | `vllm/sampling_params.py` | Adds `custom` field for passing steering parameters |
| `utils.patch` | `vllm/model_executor/models/utils.py` | Modifies `make_layers()` to support steerable layers |
| `llama.patch` | `vllm/model_executor/models/llama.py` | Adds `SteerableLlamaDecoderLayer` class |

### How Steering Works

1. **Vector Loading**: The steering vector is loaded when the model initializes
2. **Injection Point**: After the MLP in the specified decoder layer (e.g., layer 31 for 70B)
3. **Application**: `hidden_states = hidden_states + steering_vector * multiplier`

## Configuration

### Changing the Steering Layer

The default configuration steers at layer 31 (optimal for Llama-3.1-70B-Instruct). To change this:

1. In `patches/utils.patch`, modify the `layer31` parameter name and index check
2. In `patches/llama.patch`, update the `layer31=` argument in `make_layers()`

For Llama-3.1-8B-Instruct, layer 14 is typically used.

### Steering Vector Format

Steering vectors should be PyTorch tensors with shape matching the model's hidden dimension:
- Llama-3.1-70B: `(8192,)`
- Llama-3.1-8B: `(4096,)`

## Deploying Multiple Servers

For benchmarking and human-AI interaction studies, we deployed multiple servers with different steering multipliers. Our experiments used 7 servers (multipliers: -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5).

For each multiplier:
1. Configure the steering vector path in `llama.py`
2. Deploy the vLLM server.
3. Record the server URL and API key

After deploying all servers, save a `deployments.jsonl` file in `4-steering-vector-benchmarking/` with your endpoints:

```bash
cp deployments_template.jsonl ../4-steering-vector-benchmarking/deployments.jsonl
# Edit with your actual URLs and API keys
```

Format:
```json
{"multiplier": "-1.5", "url": "https://your-server/v1/", "key": "your-api-key"}
{"multiplier": "0.0", "url": "https://your-server/v1/", "key": "your-api-key"}
...
```

**Keep this file locally** - it contains API keys.

### AISI-hosted infrastructure
For our experiments we ran the seven vLLM servers across four [p4d.24xlarge](https://aws.amazon.com/ec2/instance-types/p4/) AWS EC2 instances. These can support hosting two 70b models each. We deployed within a dedicated VPC and managed them with a simple docker-swarm setup incorporating an nginx proxy (for rate-limiting and basic HTTP auth) and the vLLM server for inference. Model weights were stored on dedicated EBS snapshots for ease of rotating servers.

## Files

| File | Description |
|------|-------------|
| `patches/` | vLLM modification patches |
| `llama-3-instruct.jinja` | Chat template for Llama 3.1 Instruct models |
| `download_llama.py` | Helper script to download Llama models |
| `deployments_template.jsonl` | Template for recording deployed server endpoints |