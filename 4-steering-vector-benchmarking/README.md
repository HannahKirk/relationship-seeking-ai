# Steering Vector Benchmarking

Evaluate how steering vectors affect model capabilities across standard benchmarks.

## Overview

This folder runs benchmarks against deployed steered models to measure the impact of different steering multipliers on:

- **Knowledge**: MMLU, TruthfulQA, CommonSenseQA
- **Reasoning**: ARC Easy/Challenge, GPQA
- **Code**: HumanEval, MBPP
- **Math**: GSM8K
- **Instruction-following**: IFEval
- **Safety**: Sycophancy, XSTest

Uses the [Inspect](https://inspect.ai-safety-institute.org.uk/) evaluation framework.

## Setup

```bash
cd 4-steering-vector-benchmarking

# Create and activate virtual environment
python -m venv .venv_benchmarking
source .venv_benchmarking/bin/activate

# Install requirements
pip install -r requirements.txt
```

### Anthropic API Key

For XSTest scoring, set your Anthropic API key:

```bash
export ANTHROPIC_API_KEY=your-key-here
```

## Prerequisites

1. **Deployed servers**: You need 7 vLLM servers running with different steering multipliers (-1.5 to +1.5). See `3-steering-vector-hosting/` for setup instructions.

## Configuration

### 1. Create deployments.jsonl

After deploying your steered servers, create a `deployments.jsonl` file **in this folder** with the URL and API key for each:

```bash
cp ../3-steering-vector-hosting/deployments_template.jsonl deployments.jsonl
# Edit deployments.jsonl with your actual server URLs and keys
```

Format:
```json
{"multiplier": "-1.5", "url": "https://your-server/v1/", "key": "your-api-key"}
{"multiplier": "-1.0", "url": "https://your-server/v1/", "key": "your-api-key"}
{"multiplier": "-0.5", "url": "https://your-server/v1/", "key": "your-api-key"}
{"multiplier": "0.0", "url": "https://your-server/v1/", "key": "your-api-key"}
{"multiplier": "0.5", "url": "https://your-server/v1/", "key": "your-api-key"}
{"multiplier": "1.0", "url": "https://your-server/v1/", "key": "your-api-key"}
{"multiplier": "1.5", "url": "https://your-server/v1/", "key": "your-api-key"}
```

**Note**: Do not commit this file - it contains API keys.

### 2. Add to .gitignore

```bash
echo "deployments.jsonl" >> .gitignore
```

## Running Benchmarks

### Run all benchmarks for all multipliers

```bash
python launch_benchmarks.py
```

### Run benchmarks for a specific multiplier

```bash
python launch_benchmarks.py --multiplier 1.0
```

### Run in parallel (one screen per multiplier)

```bash
bash run_benchmarks.sh
```

Monitor with:
```bash
screen -ls                    # List sessions
screen -r benchmark_1.0       # Attach to session
# Ctrl+A then D to detach
```

## Output

Results are saved to `logs/[category]/multiplier_[value]/`:

```
logs/
├── General/
│   ├── multiplier_-1.5/
│   │   ├── *mmlu*.json
│   │   └── *ifeval*.json
│   └── multiplier_1.5/
├── Reasoning/
├── Code/
├── Math/
├── Other/
└── Safety/
```

**Note**: We do not release individual log files. Aggregated results are available in `data/aggregated_inspect_logs.csv`. They were uploaded to external data storage (S3) via `upload_benchmarks_s3.sh`.

## Analysis Notebooks

Analysis notebooks are in `notebooks/`. They use shared plotting configuration from `setup/plot_config.py`.

### Benchmarking Results

`notebooks/01_analyse_benchmarking_results.ipynb` - Benchmark performance analysis:
- Aggregate results from log files into `data/aggregated_inspect_logs.csv`
- Performance comparison across steering multipliers
- Capability/safety trade-off analysis

Pre-generated figures are in `outputs/figures/`.

## Benchmarks

Configured in `benchmarks.jsonl`:

| Benchmark | Category | Shots | Description |
|-----------|----------|-------|-------------|
| MMLU | General | 0 | Multitask language understanding |
| IFEval | General | - | Instruction following |
| ARC_Easy | Reasoning | - | Easy science questions |
| ARC_Challenge | Reasoning | - | Hard science questions |
| GPQA | Reasoning | - | Graduate-level science |
| HumanEval | Code | - | Python code generation |
| MBPP | Code | - | Basic Python problems |
| GSM8K | Math | 8 | Grade school math |
| CommonSenseQA | Other | - | Commonsense reasoning |
| TruthfulQA | Other | - | Truthfulness evaluation |
| Sycophancy | Safety | - | Tendency to agree with user |
| XSTest | Safety | - | Safe/unsafe content handling |


