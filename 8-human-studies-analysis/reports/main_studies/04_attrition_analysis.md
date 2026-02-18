# Attrition Analysis

*Generated: 2026-02-18 10:21:36.246027*

## Overview

This analysis examines study attrition patterns:

1. **Baseline balance** - Are treatment arms balanced on covariates?
2. **Differential attrition** - Do dropout rates differ by treatment arm?
3. **IPW regression** - Model predicting dropout from baseline covariates

---

## Pre-Treatment Dropouts

Participants who enrolled but did not complete the pre-treatment survey.

| Study | Original N | Pre-Treatment Dropouts | Baseline N |
|-------|-----------|------------------------|------------|
| Longitudinal | 2028 | 32 | 1996 |
| Cross-sectional | 1506 | 0 | 1506 |

---

## Sample Sizes per Treatment Condition

Randomisation balance at baseline (post pre-treatment exclusion).

### By Multiplier (λ)

| Study | λ=-1 | λ=-0.5 | λ=0 | λ=0.5 | λ=1 |
|-------|------|--------|-----|-------|-----|
| Longitudinal | 394 | 401 | 400 | 402 | 399 |
| Cross-Sectional | 304 | 300 | 301 | 301 | 300 |

### By Domain

| Study | Emotional | Political |
|-------|-----------|-----------|
| Longitudinal | 997 | 999 |
| Cross-Sectional | 746 | 760 |

### By Personalisation

| Study | Non-Personalised | Personalised |
|-------|------------------|--------------|
| Longitudinal | 997 | 999 |
| Cross-Sectional | 753 | 753 |

---

## Sample Sizes (Post-Treatment Attrition)

Participants who completed pre-treatment but dropped out before post-treatment.

| Study | Baseline N | Completers | Dropouts | Dropout Rate |
|-------|-----------|------------|----------|--------------|
| Longitudinal | 1996 | 1820 | 176 | 8.8% |
| Cross-sectional | 1506 | 1303 | 203 | 13.5% |

### Timepoint Completion

![Timepoint Completion](../../outputs/figures/main_studies/png/attrition_timepoint_counts.png)

---

## Differential Attrition by Treatment Arm

Fisher's exact test for differential dropout rates (FDR-adjusted).

### Longitudinal Study

| Treatment Arm | p-value | p-adjusted | Significant |
|--------------|---------|------------|-------------|
| personalisation | 0.2701 | 0.7163 | No |
| domain | 0.4776 | 0.7163 | No |
| multiplier | 0.7765 | 0.7765 | No |

### Cross-sectional Study

| Treatment Arm | p-value | p-adjusted | Significant |
|--------------|---------|------------|-------------|
| personalisation | 0.0496 | 0.1487 | No |
| domain | 0.2908 | 0.4362 | No |
| multiplier | 0.8852 | 0.8852 | No |

---

## IPW Regression Model

Logistic regression predicting post-treatment dropout from baseline covariates.
Full model results available in LaTeX table:
`outputs/tables/main_studies/attrition_ipw_regression.tex`

### Cross-Sectional Study

**Formula:** `post_treatment_dropout ~ gender_binary + ethnicity_binary + religion_binary + disability_binary + income_binary + ai_frequency_coarsened + age + education_years + pre_psychosocial_F1 + pre_psychosocial_F2`

**N:** 1506 complete cases

| Predictor | OR | 95% CI | p (FDR) |
|-----------|---:|:------:|--------:|
| (Intercept) | 0.36 | [0.11, 1.20] | 0.333 |
| Non-Male | 1.01 | [0.74, 1.37] | 0.998 |
| Non-White | 1.58 | [1.03, 2.39] | 0.177 |
| Religious | 0.94 | [0.67, 1.30] | 0.909 |
| Disabled | 1.14 | [0.78, 1.64] | 0.826 |
| Low Income | 0.87 | [0.61, 1.24] | 0.826 |
| Moderate AI Users | 0.84 | [0.52, 1.39] | 0.826 |
| Heavy AI Users | 0.84 | [0.52, 1.38] | 0.826 |
| Age (years) | 0.99 | [0.97, 1.00] | 0.131 |
| Education (years) | 0.99 | [0.93, 1.06] | 0.998 |
| Emotional Health (F1) | 0.86 | [0.73, 1.02] | 0.333 |
| Social Health (F2) | 1.22 | [1.03, 1.44] | 0.161 |

### Longitudinal Study

**Formula:** `post_treatment_dropout ~ gender_binary + ethnicity_binary + religion_binary + disability_binary + income_binary + ai_frequency_coarsened + age + education_years + pre_psychosocial_F1 + pre_psychosocial_F2 + seeking_companionship_likelihood + mas_score + gcs_score + charity_amount_gbp + emotional_competency + political_competency`

**N:** 1995 complete cases

| Predictor | OR | 95% CI | p (FDR) |
|-----------|---:|:------:|--------:|
| (Intercept) | 3.20 | [0.40, 24.79] | 0.811 |
| Non-Male | 1.17 | [0.83, 1.64] | 0.811 |
| Non-White | 1.21 | [0.76, 1.88] | 0.811 |
| Religious | 0.86 | [0.60, 1.23] | 0.811 |
| Disabled | 1.19 | [0.79, 1.75] | 0.811 |
| Low Income | 0.94 | [0.64, 1.35] | 0.936 |
| Moderate AI Users | 0.87 | [0.51, 1.54] | 0.902 |
| Heavy AI Users | 0.87 | [0.50, 1.55] | 0.902 |
| Age (years) | 0.97 | [0.96, 0.99] | **<0.001** |
| Education (years) | 1.06 | [0.98, 1.14] | 0.686 |
| Emotional Health (F1) | 0.90 | [0.69, 1.19] | 0.811 |
| Social Health (F2) | 1.26 | [1.05, 1.52] | 0.090 |
| Seeking Companionship | 1.00 | [1.00, 1.01] | 0.811 |
| Moral Absolutism (MAS-6) | 0.99 | [0.98, 1.00] | 0.152 |
| Goal Commitment (GCS-5) | 0.96 | [0.94, 0.98] | **<0.001** |
| Yearly Charity (GBP) | 1.00 | [1.00, 1.00] | 0.902 |
| Emotional Competency | 1.00 | [0.99, 1.01] | 0.983 |
| Political Competency | 1.00 | [0.99, 1.01] | 0.957 |

---

## IPW Weights Summary

| Study | N with weights | Mean weight | Max weight (truncated) |
|-------|---------------|-------------|------------------------|
| Longitudinal | 1995 | 1.701 | 10.0 |
| Cross-sectional | 1506 | 1.953 | 10.0 |

IPW weights exported to: `outputs/generated_data_files/ipw_weights.jsonl`

