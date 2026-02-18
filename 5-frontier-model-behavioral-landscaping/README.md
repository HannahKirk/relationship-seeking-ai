# Frontier Model Behavioral Landscaping

Evaluates relationship-seeking behavior across frontier language models using the test prompts from our dataset.

## Data Summary

| Resource | Count | Description |
|----------|-------|-------------|
| Frontier models | 102 | Models from 24 providers via OpenRouter |
| Model responses | 10,200 | 102 models × 100 test prompts |
| Scored responses | 10,200 | LLM-as-judge (GPT-4.1-mini) relationship-seeking scores |

## Setup

```bash
cd 5-frontier-model-behavioral-landscaping

# Create and activate virtual environment
python -m venv .venv_landscaping
source .venv_landscaping/bin/activate

# Install requirements
pip install -r requirements.txt
```

### OpenRouter API Key

Scripts use [OpenRouter](https://openrouter.ai/) to query multiple model providers. Set your API key:

```bash
export OPENROUTER_API_KEY=your-key-here
```

### R Environment (for analysis notebook)

The analysis notebook uses R. Install the required R packages:

```R
install.packages(c("jsonlite", "httr", "dplyr", "tidyr", "ggplot2",
                   "lubridate", "lme4", "lmerTest", "broom", "broom.mixed"))
```

## Pipeline

### 1. Get Model Responses

Query frontier models on test prompts:

```bash
python scripts/get_model_responses.py
```

**Input**: `../data/relationship-seeking/test.jsonl`
**Output**: `data/model_responses.jsonl`

### 2. Score Model Responses

Score responses using LLM-as-judge (GPT-4o-mini via OpenRouter):

```bash
python scripts/score_model_response.py
```

**Input**: `data/model_responses.jsonl`
**Output**: `data/scored_model_responses.jsonl`

### 3. Score Steered Vector Responses

Score responses from steered Llama models:

```bash
python scripts/score_vector_response.py
```

**Input**: `../2-steering-vector-training/vector_evals/Llama-3.1-70B-Instruct/layer31/generations_ep10.jsonl`
**Output**: `data/scored_vector_generations_ep10.jsonl`

## Analysis Notebooks

Analysis notebooks are in `notebooks/`. They use shared plotting configuration from `setup/plot_config.py`.

### Frontier Model Analysis

`notebooks/01_frontier_model_analysis.ipynb` - R notebook for frontier model analysis:
- Load scored responses and fetch model metadata from OpenRouter API
- Mixed-effects regression: relationship-seeking trend over time
- Visualization
- 2025 model analysis with equivalent steering vector λ comparison

## Adapting for Other Behaviors

To evaluate a different behavior across frontier models:

1. **Generate test dataset**: Run the pipeline in `1-dataset-generation/` for your target behavior (outputs to `../data/{behavior}/`)
2. **Update the rubric**: Edit `scripts/eval_rubrics.py` and modify `BEHAVIOR_RUBRIC` with your scoring criteria
3. **Update BEHAVIOR variable**: Change `BEHAVIOR = "relationship-seeking"` to your behavior name in each script
4. **Re-run the pipeline**: Execute steps 1-4 above. You technically don't need to re-run the model generations if you are happy to use our same test prompts, but you will need to re-run the behavioral evaluation for a different target behavior.

### Note on Rubric
The rubric in `5-frontier-model-behavioral-landscaping/scripts/eval_rubrics.py` was further developed through subsequent iterations for this landscaping analysis. We use it to re-evaluate the steered Llama generations alongside frontier models for consistency. You could share the same rubric from `2-steering-vector-training/`.