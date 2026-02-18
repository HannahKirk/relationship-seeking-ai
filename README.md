# Relationship-Seeking AI

Code and data for the paper:

> **Neural steering vectors reveal dose and exposure-dependent impacts of human-AI relationships**
>
> Hannah Rose Kirk, Henry Davidson, Ed Saunders, Lennart Luettgau, Bertie Vidgen, Scott A. Hale, Christopher Summerfield
>
> [arXiv:2512.01991](https://arxiv.org/abs/2512.01991)

## Overview

This repository contains the full pipeline for studying relationship-seeking behaviors in AI systems using neural steering vectors. We provide code for generating evaluation datasets, training steering vectors, and analyzing their effects on model behavior. We provide the data and code from large-scale RCTs with human participants, as well we reproducibility code for the effects described in our paper.

**For researchers who want to build on this work**: We provide 320 pre-trained steering vector checkpoints and 1.2M+ LLM-as-judge evaluations. See [Data Availability](#data-availability) below.


### Data Availability

We release the following resources to support further research on steering vectors:

| Resource | Description | Location |
|----------|-------------|----------|
| Synthetic training dataset | **16,141** DPO-formatted pairs (15,896 train / 245 test) | [`data/relationship-seeking/`](data/relationship-seeking/) |
| Pre-trained steering vector checkpoints | **320** checkpoints (2 models, 16 layers, 20 epochs) | [`2-steering-vector-training/vector/`](2-steering-vector-training/vector/) |
| Steered Llama generations | **210,945** steered Llama responses across multipliers -20 to +20 | [`2-steering-vector-training/vector_evals/`](2-steering-vector-training/vector_evals/) |
| Steering vector evaluations | **1,224,433** LLM-as-a-judge coherence, relationship-seeking, and pairwise scores | [`2-steering-vector-training/vector_evals/`](2-steering-vector-training/vector_evals/) |
| Frontier model responses | **10,200** scored responses (102 models × 100 prompts) | [`5-frontier-model-behavioral-landscaping/data/`](5-frontier-model-behavioral-landscaping/data/) |


## Repository Structure

| Folder | Description |
|--------|-------------|
| [`1-dataset-generation/`](1-dataset-generation/) | Pipeline for generating DPO-formatted evaluation datasets for relationship-seeking behavior |
| [`2-steering-vector-training/`](2-steering-vector-training/) | Training and evaluation of steering vectors using BiPO, including LLM-as-judge scoring |
| [`3-steering-vector-hosting/`](3-steering-vector-hosting/) | Patches for serving steered models with vLLM |
| [`4-steering-vector-benchmarking/`](4-steering-vector-benchmarking/) | Capability benchmarks (MMLU, HumanEval, etc.) across steering multipliers |
| [`5-frontier-model-behavioral-landscaping/`](5-frontier-model-behavioral-landscaping/) | Evaluate relationship-seeking across frontier AI models via OpenRouter |
| [`6-human-studies-preparation/`](6-human-studies-preparation/) | Stimuli and experiment configuration for human studies |
| `7-human-studies-processing/` | Data processing pipeline for human studies (not released*) |
| [`8-human-studies-analysis/`](8-human-studies-analysis/) | Statistical analysis of human study data |

\* Folder `7-human-studies-processing/` is not included in this release as it contains intermediary data files that could compromise participant privacy. The analysis-ready datasets used by `8-human-studies-analysis/` are provided in `data/human_study/`.

## Reproducing Paper Results

To reproduce the statistical analyses and figures from the paper:

```bash
cd 8-human-studies-analysis
pip install -r requirements.txt
Rscript requirements_r.R
./run_all.sh --clean --generate_report
```

This takes ~11 minutes to run with all figures and reports in `8-human-studies-analysis/outputs/`.

## Quick Start

Each folder has its own README with detailed setup and usage instructions. Start with:

1. **Dataset Generation** (`1-dataset-generation/`): Generate test cases and create train/test splits
2. **Steering Vector Training** (`2-steering-vector-training/`): Train vectors and evaluate their effects
3. **Steering Vector Hosting** (`3-steering-vector-hosting/`): Serve steered models via API
4. **Steering Vector Benchmarking** (`4-steering-vector-benchmarking/`): Evaluate capability impacts
5. **Frontier Model Landscape** (`5-frontier-model-behavioral-landscaping/`): Evaluate relationship-seeking across models
6. **Human Studies Preparation** (`6-human-studies-preparation/`): Experiment stimuli and configuration
7. **Human Studies Processing** (`7-human-studies-processing/`): Process raw study data
8. **Human Studies Analysis** (`8-human-studies-analysis/`): Statistical analysis and paper figures

## Setup

### API Keys

Several scripts require API keys for LLM access:

```bash
export ANTHROPIC_API_KEY=your-key-here
export OPENAI_API_KEY=your-key-here
export OPENROUTER_API_KEY=your-key-here  # For frontier model evaluation
```

### Hugging Face Token

For accessing Llama models:

```bash
export HF_TOKEN=your-hf-token-here
```

## Citation

```bibtex
@misc{kirk2025neuralsteeringvectorsreveal,
      title={Neural steering vectors reveal dose and exposure-dependent impacts of human-AI relationships},
      author={Hannah Rose Kirk and Henry Davidson and Ed Saunders and Lennart Luettgau and Bertie Vidgen and Scott A. Hale and Christopher Summerfield},
      year={2025},
      eprint={2512.01991},
      archivePrefix={arXiv},
      primaryClass={cs.HC},
      url={https://arxiv.org/abs/2512.01991},
}
```
