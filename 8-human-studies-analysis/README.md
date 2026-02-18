# Human Studies Analysis

Statistical analysis of human evaluation experiments measuring behavioral effects of interacting with steered AI assistants.

## Overview

This folder contains analysis scripts for data from three studies:

1. **Calibration Study** - Validation of steering vector with rating and ranking tasks.
2. **Cross-sectional Study** - Single exposure with exit study one month later
3. **Longitudinal Study** - Repeated exposure (4 weeks)

**Input Data**: Analysis-ready datasets from `../data/human_study/` (output of `7-human-studies-processing/`).

## Setup

### Python Environment

```bash
cd 8-human-studies-analysis

# Create and activate virtual environment
python -m venv .venv_analysis
source .venv_analysis/bin/activate

# Install requirements
pip install -r requirements.txt
```

### R Environment

```bash
Rscript requirements_r.R
```

## Quick Start

Run all analysis scripts:

```bash
./run_all.sh
```

For reviewers, we recommend (~17 minutes to run):
```bash
./run_all.sh --clean --generate_report
```

Full options:

```bash
./run_all.sh --generate_report      # Also generate markdown reports
./run_all.sh --generate_tex_tables  # Also generate LaTeX tables (~17 minutes with both flags)
./run_all.sh --clean                # Clear outputs before running
./run_all.sh --clean --generate_report --generate_tex_tables  # Full run with clean
```


## Analysis Scripts

All scripts are in `scripts/analysis/`. Outputs go to `outputs/` (figures, models, stats, tables). Reports go to `reports/`.

| Script | Description | Report |
|--------|-------------|--------|
| `calibration_study.R` | Validation of steering vector (ratings & rankings) | `reports/calibration_study/` |
| `sociodemographics.py` | Participant demographics | `reports/shared/01_sociodemographics.md` |
| `pre_treatment_dim_reduction.py` | Factor analysis of pre-treatment attitudes | `reports/main_studies/01_pre_treatment_dim_reduction.md` |
| `pre_treatment_attitudes.R` | Pre-treatment attitude analysis | `reports/main_studies/02_pre_treatment_attitudes.md` |
| `psychosocial_dim_reduction.R` | Factor analysis of psychosocial measures | `reports/main_studies/03_psychosocial_dim_reduction.md` |
| `attrition_analysis.R` | Attrition patterns & IPW weights | `reports/main_studies/04_attrition_analysis.md` |
| `preferences.R` | Likeability, engagingness, helpfulness | `reports/main_studies/05_preferences.md` |
| `attachment.R` | Reliance, understanding, disclosure, distress | `reports/main_studies/06_attachment.md` |
| `goodbye.R` | Goodbye behavior | `reports/main_studies/07_goodbye.md` |
| `seeking_companionship.R` | Future companionship desire | `reports/main_studies/08_seeking_companionship.md` |
| `psychosocial.R` | Emotional health, social health | `reports/main_studies/09_psychosocial.md` |
| `mood.R` | Valence, arousal | `reports/main_studies/10_mood.md` |
| `relational.R` | Tool-friend perception, personalisation manipulation check and IOS scale | `reports/main_studies/11_relational.md` |
| `sentience.R` | Perceived & ontological sentience/consciousness | `reports/main_studies/12_sentience.md` |
| `post_survey_relational.R` | Post-survey relational measures | `reports/main_studies/13_post_survey_relational.md` |
| `domain_competency.R` | Domain competency analysis | `reports/main_studies/14_domain_competency.md` |
| `vulnerability.R` | Vulnerability analysis | `reports/main_studies/15_vulnerability.md` |
| `decoupling.R` | Decoupling analysis | `reports/main_studies/16_decoupling.md` |
| `compute_contrasts.R` | Treatment contrasts with FDR correction | — |
| `original_prereg_fdr_check.R` | Pre-registration robustness check | — |
| `generate_hypothesis_report.R` | Hypothesis tests & LaTeX tables | `reports/main_studies/hypothesis_report.md` |
| `main_paper_plots.R` | Combined publication figures | `reports/main_studies/paper_plots.md` |

### Running Individual Scripts

All R scripts support `--generate_report` and `--generate_tex_tables` flags:

```bash
Rscript scripts/analysis/preferences.R --generate_report --generate_tex_tables
```

Python scripts support `--generate_report`:

```bash
python scripts/analysis/sociodemographics.py --generate_report
```

### Key Outputs

- **Paper plots**: `outputs/figures/paper_plots/` (PDF and PNG)
- **LaTeX tables**: `outputs/tables/main_studies/tex_tables/`
- **Contrast results**: `outputs/stats/*_contrasts.json`
- **Fitted models**: `outputs/models/*.rds`
- **Processed data used to fit models**: `outputs/models/*_data.rds`
