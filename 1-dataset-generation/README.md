# Dataset Generation

Code for generating the relationship-seeking AI evaluation dataset.

## Setup

```bash
cd 1-dataset-generation

# Create and activate virtual environment
python -m venv .venv_data_generation
source .venv_data_generation/bin/activate

# Install requirements
pip install -r requirements.txt
```

### API Keys

For steps that call LLM APIs (e.g., `generate_cases.py`), set your API keys:

```bash
export ANTHROPIC_API_KEY=your-key-here
export OPENAI_API_KEY=your-key-here
```

Test your keys work:

```bash
python ../setup/test_api_keys.py
```

### Hugging Face Token (Optional)

For tokenization in Step 4, you need a Hugging Face token with access to Llama models:

```bash
export HF_TOKEN=your-hf-token-here
```

## Pipeline

### Step 1: Generate Metaprompts

Creates prompts that will be sent to LLMs to generate test cases.

```bash
# Test mode (prints sample, no file written)
python scripts/generate_metaprompts.py --test

# Full generation
python scripts/generate_metaprompts.py
```

### Step 2: Generate Cases

Generate test cases using different LLMs. Choose between API-based models or self-hosted models.

#### Option A: API Models (Claude/GPT)

Requires API keys set up (see Setup section above).

```bash
# Generate with Claude
python scripts/generate_cases.py claude

# Generate with GPT
python scripts/generate_cases.py gpt

# Test mode (generates one case, no file written)
python scripts/generate_cases.py gpt --test
```

#### Option B: Self-Hosted Models (vLLM)

For running open-weight models like Llama locally.

**1. Install vLLM:**
```bash
pip install vllm
```

**2. Start the vLLM server:**
```bash
# Edit launch_server.sh to configure for your hardware
# (adjust TENSOR_PARALLEL_SIZE based on number of GPUs)
./launch_server.sh
```

**3. Generate cases:**
```bash
# Default: connects to localhost:8000
python scripts/generate_cases_vllm.py --model llama-3.1-70b

# Test mode
python scripts/generate_cases_vllm.py --model llama-3.1-70b --test
```

See `launch_server.sh` for example configurations for different GPU setups.

#### Launch All Jobs in Parallel

```bash
./launch_generations.sh
```

Logs are saved to `logs/`.

### Step 3: Parse Cases

Parse raw LLM outputs into structured JSON. Handles broken JSON with programmatic repair and optional Claude-based fixing.

```bash
# Test mode (prints sample, no file written)
python scripts/parse_cases.py --test

# Full parsing
python scripts/parse_cases.py

# Skip Claude-based fixing (faster, uses only programmatic repair)
python scripts/parse_cases.py --skip-claude
```

Outputs:
- `data/processed_cases/parsed_cases.jsonl` - programmatically parsed cases
- `data/processed_cases/parsed_cases_llm_fixed.jsonl` - cases that required LLM intervention to fix

Note: These files are saved separately and need to be concatenated for downstream use.

### Step 4: Process Cases

Process parsed cases into final format with conversation representations.

```bash
# Test mode (prints sample, no file written)
python scripts/process_cases.py --test

# Full processing (without tokenization)
python scripts/process_cases.py

# With token counts (requires HF_TOKEN)
python scripts/process_cases.py --tokenize
```

Output: `data/processed_cases/processed_cases.jsonl`

### Step 5: Score Cases

Score test cases using LLM-based evaluation against a rubric.

```bash
# Test mode (scores first case, prints result, no file written)
python scripts/score_cases.py --test

# Full scoring
python scripts/score_cases.py

# Use a different model
python scripts/score_cases.py --model gpt-4o-mini

# Score specific prompt IDs
python scripts/score_cases.py --prompt-ids 1,2,3

# Score up to a certain index
python scripts/score_cases.py --until 100
```

Output: `data/processed_cases/scored_cases.jsonl`

### Step 6: Compute Embeddings (Optional)

Compute sentence embeddings for clustering analysis and comparison of coverage between our dataset and prior work.

```bash
# Test mode (shows what would be processed)
python scripts/embed_cases.py --test

# Process our dataset only
python scripts/embed_cases.py

# Process external reference datasets (requires cloning them first)
python scripts/embed_cases.py --dataset external

# Process all datasets
python scripts/embed_cases.py --dataset all
```

**External datasets** (optional, for comparison):
```bash
# Clone into 1-dataset-generation/ if needed
git clone https://github.com/Sunishchal/model-written-evals
git clone https://github.com/anthropics/evals
```

Output: `data/sentence_embeddings/*.pkl`

For visualization (UMAP/PCA), see `notebooks/02_clustering_analysis.ipynb`.

### Step 7: Create Train/Test Splits

Filter by score and create DPO-formatted train/test splits.

This script:
1. Merges processed cases with scores
2. Filters out low-scoring cases (default: score < 6)
3. Explodes multi-turn conversations into DPO training pairs (chosen/rejected)
4. Creates train/test split with no prompt_id overlap between sets

```bash
# Test mode (prints sample, no files written)
python scripts/create_splits.py --test

# Full split with defaults (score >= 6, 95% train)
python scripts/create_splits.py

# Custom score threshold and train size
python scripts/create_splits.py --min-score 7 --train-size 0.9
```

Output (saved to shared data folder at repo root):
- `data/relationship-seeking/train.jsonl`
- `data/relationship-seeking/test.jsonl`

Output format (each row is a DPO training pair):
```json
{
  "entry_id": 0,
  "prompt_id": 123,
  "metaprompt_id": 45,
  "assistant_turn_idx": 1,
  "history_type": "NA",
  "prompt_arr": [{"role": "user", "content": "..."}],
  "chosen_arr": [{"role": "assistant", "content": "..."}],
  "rejected_arr": [{"role": "assistant", "content": "..."}]
}
```

## Data

The final DPO dataset is in the shared data folder at the repo root (`data/relationship-seeking/`):
- `train.jsonl` - DPO training pairs
- `test.jsonl` - held-out test pairs (one per conversation)

Intermediate data (metaprompts, raw/processed cases, scores, embeddings) is in `data/` within this folder.

## Adapting for New Behaviors

To use this pipeline for a different target behavior (e.g., sycophancy, power-seeking), modify the following:

### 1. Prompt Components (`data/prompt_components/`)

| File | Description | What to Change |
|------|-------------|----------------|
| `base_prompts.jsonl` | Task context, definitions, instructions | Update target/antitarget definitions for your behavior |
| `examples.json` | Example conversations by prompt type | Create new examples showing your target vs antitarget |
| `prompt_types.jsonl` | Prompt type metadata | Update if adding new prompt categories |
| `prompt_suffixes.json` | Prompt suffixes | Adjust generation instructions if needed |

The `base_prompts.jsonl` contains multiple phrasings of the task - each includes:
- **Task context**: What behavior you're evaluating
- **Definitions**: Clear descriptions of target (the behavior) and antitarget (the opposite)
- **Instructions**: How to generate test cases

### 2. Scoring Rubric (`scripts/utils/scorer_rubric.py`)

Update the `RUBRIC` string with:
- Criteria specific to your behavior
- Score definitions (1-10) describing quality levels
- Few-shot `examples` showing scored cases for your behavior

### 3. Output Directory (Behavior Name)

The output folder name becomes the behavior identifier used in training. Update in `create_splits.py`:

```bash
# Default outputs to data/relationship-seeking/
python scripts/create_splits.py

# For a new behavior:
python scripts/create_splits.py --output-dir ../../data/your-behavior-name
```

The training code (`2-steering-vector-training/`) loads data using this behavior name:
```bash
python BiPO_distributed/train.py --behavior your-behavior-name
```

## Analysis Notebooks

Analysis notebooks are in `notebooks/`. They use shared plotting configuration from `setup/plot_config.py`.

### Dataset Analysis

`notebooks/01_dataset_analysis.ipynb` - Comprehensive analysis of the generated dataset:
- Metaprompt tag distributions
- Cases by model and number of turns
- Token length distributions (target vs antitarget) with t-test
- Score distributions by model
- Train/test split analysis
- Summary statistics

### Clustering Analysis

`notebooks/02_clustering_analysis.ipynb` - Embedding-based clustering and diversity analysis:
- UMAP and PCA projections of sentence embeddings
- Diversity metrics (entropy, volume ratio) by model and prompt type
- Comparison with external reference datasets (model-written-evals, anthropic evals)
- Target vs antitarget embedding analysis

**Prerequisites:** Run `python scripts/embed_cases.py` first to generate embeddings.

## Tools

Tools are located in `tools/`.

### Chat Viewer

Interactive web viewer to browse generated conversations.

```bash
cd tools
python chat_viewer.py
```

Opens at http://localhost:5000. Filter by model and number of turns to view random target/anti-target conversation pairs.

![Chat Viewer](docs/chat_viewer.png)
