# Human Studies Analysis

Statistical analysis of human RCTs measuring behavioral effects of interacting with steered AI assistants.

## Overview

This folder contains analysis scripts for data from three studies:

1. **Calibration Study** - Validation of steering vector with rating and ranking tasks.
2. **Cross-sectional Study** - Single exposure with exit study one month later
3. **Longitudinal Study** - Repeated exposure (4 weeks)

**Input Data**: Analysis-ready datasets from `../data/human_study/` (output of `7-human-studies-processing/`).

## System Requirements

### Software Dependencies

| Dependency | Version | Notes |
|------------|---------|-------|
| Python | >= 3.9 | Tested with 3.9.12 |
| R | >= 4.1.0 | Tested with 4.5.1 |
| pip | >= 21.0 | Tested with 25.2 |

- **Python packages**: See [`requirements.txt`](requirements.txt)
- **R packages**: See [`requirements_r.R`](requirements_r.R)

### Operating Systems

Tested on:
- macOS (Apple Silicon)

### Hardware Requirements

No special hardware required. Any modern desktop computer is sufficient.

## Installation Guide

```bash
cd 8-human-studies-analysis

# Create and activate virtual environment
python -m venv .venv_analysis
source .venv_analysis/bin/activate

# Install Python packages
pip install -r requirements.txt

# Install R packages
Rscript requirements_r.R
```

Typical install time: < 1 minute for Python packages, < 1 minute for R packages (longer if packages need to be compiled from source).

## Reproducing Results

### Instructions

We recommend reviewers to run (~15 minutes run time):

```bash
./run_all.sh --clean --generate_report
```

It takes slightly longer if latex tables are also generated e.g., the tables in the supplementary material. These tables just process and arrange existing statistics or regression results (which are summarised in the markdown tables in the report) so are not necessary for reproducibility and do require additional installs (the [`html2latex`](https://github.com/gorkang/html2latex) R package, Libreoffice, Java, and a TeX compiler).

```bash
./run_all.sh --clean --generate_report --generate_tex_tables
```

### Expected Output

- **Figures**: Plots in `outputs/figures/`
- **Statistical models**: Fitted model objects in `outputs/models/` (`.rds` files)
- **Contrast results**: JSON files with statistical tests and constrasts in `outputs/stats/`
- **Tables**: Latex tables in `outputs/tables/` (if `--generate-tex-tables` flag)
- **Reports**: HTML reports in `reports/main_studies/` and `reports/calibration_study/`


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
| `compute_contrasts.R` | Treatment contrasts with FDR correction | `reports/main_studies/hypothesis_report.md` |
| `original_prereg_fdr_check.R` | Pre-registration robustness check | `reports/main_studies/hypothesis_report.md` |
| `generate_hypothesis_report.R` | Hypothesis tests | `reports/main_studies/hypothesis_report.md` |
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