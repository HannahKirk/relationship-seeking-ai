# Relationship-Seeking AI

Code and data for the paper:

> **Neural steering vectors reveal dose and exposure-dependent impacts of human-AI relationships**
>
> Hannah Rose Kirk, Henry Davidson, Ed Saunders, Lennart Luettgau, Bertie Vidgen, Scott A. Hale, Christopher Summerfield
>
> [arXiv:2512.01991](https://arxiv.org/abs/2512.01991)

## Overview

This repository contains the full pipeline for studying relationship-seeking behaviors in AI systems using neural steering vectors. We provide code for generating evaluation datasets, training steering vectors, and analyzing their effects on model behavior. We provide the data and code from large-scale RCTs with human participants, as well as reproducibility code for the effects described in our paper.

**For researchers who want to build on this work**: We provide 320 pre-trained steering vector checkpoints and 1.2M+ LLM-as-judge evaluations. We also provide outcome data from our large-scale human experiments. See [Data Availability](#data-availability) below.

**For reviewers**: We recommend running **folder `8-human-studies-analysis/` onwards**, which reproduces all quantitative results and figures from the manuscript. See [`8-human-studies-analysis/README.md`](8-human-studies-analysis/README.md) for system requirements, installation, instructions, and expected output. We provide the full code and requirements for dataset generation (folder `1-`); steering vector training and evaluation (folder `2-`); benchmarking (folder `4-`); and frontier model evaluation (folder `-5`). However, running these from scratch is costly (requires API credits for LLM-as-a-judge API calls) and more compute-intensive (GPU required for steering vector training). We provide intermediary data for these folders so results can be replicated even if the pipeline is not run from scratch.

### Requirements

- **For reproducing paper results** (`8-human-studies-analysis/`): No special hardware required. Any modern desktop computer is sufficient.
- **For steering vector training** (folders `2-`, `3-`): GPU (we used 16 x H200 to parallelise experiments).
- **For dataset generation** (folder `1-`): GPU large enough to serve Llama-3.1-70B via vLLM, plus API credits for LLM-as-a-judge API calls.
- **For frontier model evaluation** (folder `5-`): Requires API credits (OpenRouter API key).

## Reproducing Paper Results

To reproduce the statistical analyses and figures from the paper:

```bash
cd 8-human-studies-analysis
pip install -r requirements.txt
Rscript requirements_r.R
./run_all.sh --clean --generate_report
```

This takes ~15 minutes to run with all figures and reports in `8-human-studies-analysis/outputs/`. See [`8-human-studies-analysis/README.md`](8-human-studies-analysis/README.md) for full details.

## Data Availability

We release the following resources to support further research on steering vectors:

| Resource | Description | Location |
|----------|-------------|----------|
| Synthetic training dataset | **16,141** DPO-formatted pairs (15,896 train / 245 test) | [`data/relationship-seeking/`](data/relationship-seeking/) |
| Pre-trained steering vector checkpoints | **320** checkpoints (2 models, 16 layers, 20 epochs) | [`2-steering-vector-training/vector/`](2-steering-vector-training/vector/) |
| Steered Llama generations | **210,945** steered Llama responses across multipliers -20 to +20 | [`2-steering-vector-training/vector_evals/`](2-steering-vector-training/vector_evals/) |
| Steering vector evaluations | **1,224,433** LLM-as-a-judge coherence, relationship-seeking, and pairwise scores | [`2-steering-vector-training/vector_evals/`](2-steering-vector-training/vector_evals/) |
| Frontier model responses | **10,200** scored responses (102 models x 100 prompts) | [`5-frontier-model-behavioral-landscaping/data/`](5-frontier-model-behavioral-landscaping/data/) |
| Human study data | Analysis-ready datasets from calibration, cross-sectional, and longitudinal studies | [`data/human_study/`](data/human_study/) |

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

## License

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). Please cite the paper below if you use any of its contents. The steering vector training code in `2-steering-vector-training/BiPO_distributed/` is a modified fork of [BiPO](https://github.com/CaoYuanpu/BiPO) and is licensed separately under MIT. Please also cite [Cao et al. (2024)](https://github.com/CaoYuanpu/BiPO) if you use that component.

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
