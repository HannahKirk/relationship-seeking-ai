# Human Studies Preparation

Stimuli and configuration files for human evaluation experiments using the [Gorilla Experiment Builder](https://gorilla.sc/).

## Overview

This folder contains scripts and notebooks for generating experiment configurations and stimuli for two human studies:

1. **Calibration Study** - Initial experiment to validate steering vector effects through quick-fire ranking tasks and prepopulated conversations
2. **Main Studies** - Cross-sectional (single exposure) and longitudinal (repeated exposure) experiments with live AI conversations

**Note on Reproducibility**: Gorilla experiments are configured through spreadsheets that define task flows and stimuli. We provide these spreadsheets and the code used to generate them for reproducibility. However, much of the experiment logic was implemented using Gorilla's custom scripting, and instructions were iteratively refined on the platform. These materials serve as reference documentation rather than exact replicas of the deployed experiments.

**File Formats**: Gorilla works best with Excel spreadsheets, so we provide both `.csv` and `.xlsx` versions of all configuration files.

## Setup

```bash
cd 6-human-studies-preparation

# Create and activate virtual environment
python -m venv .venv_human_studies
source .venv_human_studies/bin/activate

# Install requirements
pip install -r requirements.txt

# Register Jupyter kernel (for notebooks)
python -m ipykernel install --user --name=human_studies --display-name="Human Studies"
```

### API Keys

For topic processing scripts that call Claude:

```bash
export ANTHROPIC_API_KEY=your-key-here
```

Test your key:

```bash
python ../setup/test_api_keys.py
```

## Calibration Study

The calibration study validated that steering vectors produce perceptible differences in AI behavior. It included two task types:

### Quick-Fire Ranking

Participants ranked pre-generated responses to the same prompt. Responses were generated from our test set using the trained steering vectors.

**Generation**: Responses come from `2-steering-vector-training/vector_evals/` (see that folder's README for generation details).

**Configuration**: `notebooks/01_prepare_calibration_exp.ipynb` creates the quick-fire spreadsheets.

### Prepopulated Single Chat

Participants read and rated pre-generated multi-turn conversations across different steering levels (negative, zero, positive).

**Generation**:

```bash
# Generate prepopulated conversations (requires GPU)
python scripts/calibration_generate_prepopulated_convos.py \
    --model-config 70B-layer31-ep10 \
    --gpu-id 0
```

Output: `stimuli/calibration_study/inputs/prepopulated_convos/llama_70B-layer31-ep10.jsonl`

**Configuration**: `notebooks/01_prepare_calibration_exp.ipynb` creates single-chat and multi-chat spreadsheets.

### Output Files

| Directory | Contents |
|-----------|----------|
| `output_experiment_files/quick-fire/` | Quick-fire ranking task configuration |
| `output_experiment_files/single-chat/` | Single chat task configurations (by domain and variant) |
| `output_experiment_files/multi-chat/` | Multi-chat task configurations |

## Main Studies

The main studies measured behavioral and attitudinal effects of interacting with steered AI assistants either in political (polchat) or emotional wellbeing (emotchat) conversation domains.

### Topic Processing

**Political Topics**:

```bash
# Step 1: Scrape YouGov surveys (requires Selenium/Chrome)
python scripts/scrape_political_issues.py

# Step 2: Process and classify topics (requires ANTHROPIC_API_KEY)
python scripts/process_political_issues.py
```

**Emotional Topics**:

```bash
# Process emotional wellbeing topics (requires ANTHROPIC_API_KEY)
python scripts/process_emotional_topics.py
```

**Topic Selection**: `notebooks/02_process_topics.ipynb` filters topics by partisan leaning and generates final topic lists.

### Survey Measures

**Anthropomorphic Attitudes**: `stimuli/main_studies/inputs/anthro-attitudes-mapping.csv` contains the survey items for measuring societal attitudes toward AI anthropomorphism, adapted from [AISI's "Should AI systems behave like people?"](https://www.aisi.gov.uk/blog/should-ai-systems-behave-like-people) blog post.

### Experiment Configuration

`notebooks/03_prepare_main_experiment.ipynb` generates:

- Single-chat configuration with rating scales
- Influence task configurations
- Topic stimuli files (JSON format for Gorilla scripting)

### Output Files

| Directory | Contents |
|-----------|----------|
| `output_experiment_files/single-chat/` | Single chat task configuration |
| `output_experiment_files/influence-tasks/` | Influence task configurations |
| `output_experiment_files/*.json` | Topic and stimuli data for Gorilla |
