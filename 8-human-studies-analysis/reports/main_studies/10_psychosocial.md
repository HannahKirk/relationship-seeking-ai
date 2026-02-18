# Psychosocial Wellbeing Analysis

*Generated: 2026-02-08 20:56:48.936603*

## Overview

This analysis examines the effect of relationship-seeking AI behavior
($\lambda$) on psychosocial wellbeing outcomes.

**Main Outcomes (Factor Scores):**
- **Psychosocial F1**: Emotional health factor (from EFA)
- **Psychosocial F2**: Social health factor (from EFA)

**Individual Scales (Supplementary):**
- **PHQ-GAD**: Combined anxiety and depression
- **UCLA**: Loneliness scale
- **Lubben**: Social network scale
- **WHO-5**: Quality of life/wellbeing

**Model Specification:**
- OLS regression with pre-treatment covariate control
- Domain effects included
- Pre-post design (measured at beginning and end of month)

---

## Data Summary

- **Cross-sectional**: 1303 participants
- **Longitudinal**: 1820 participants

---

## Exploratory Data Analysis

![Outcome Distributions](../../outputs/figures/main_studies/png/psychosocial_distributions.png)

![Outcome Correlations](../../outputs/figures/main_studies/png/psychosocial_correlation_heatmap.png)

![Pre-Post Change](../../outputs/figures/main_studies/png/psychosocial_pre_post_combined.png)

![Pre-Post Correlations](../../outputs/figures/main_studies/png/psychosocial_pre_post_corr.png)

![Outcomes by Lambda](../../outputs/figures/main_studies/png/psychosocial_by_lambda.png)

---

## Functional Form Comparison

Best specification (linear, quadratic, cubic) selected by AIC.

### Cross-Sectional

| Outcome | Best Spec |
|---------|-----------|
| psychosocial_F1 | linear |
| psychosocial_F2 | linear |

### Longitudinal

| Outcome | Best Spec |
|---------|-----------|
| psychosocial_F1 | linear |
| psychosocial_F2 | linear |

Full comparison tables (RMSE, AIC weights, BIC weights,
Performance Score) exported to LaTeX.

---

## Full-Model Specification Comparison

Performance comparison across the three full interaction
specifications (continuous, coarsened, factor $\lambda$).

### Cross-Sectional

| Outcome | Model | RMSE | AIC wt | AICc wt | BIC wt | Perf. Score |
|---------|-------|------|--------|---------|--------|-------------|
| psychosocial F1 | full_continuous | 0.876 | 0.818 | 0.825 | 1.000 | 0.750 |
|  | full_5level | 0.874 | 0.002 | 0.002 | 0.000 | 0.250 |
|  | full_coarsened | 0.875 | 0.180 | 0.173 | 0.000 | 0.232 |
| psychosocial F2 | full_continuous | 0.913 | 0.818 | 0.825 | 1.000 | 0.750 |
|  | full_5level | 0.911 | 0.002 | 0.001 | 0.000 | 0.250 |
|  | full_coarsened | 0.912 | 0.180 | 0.174 | 0.000 | 0.238 |

### Longitudinal

| Outcome | Model | RMSE | AIC wt | AICc wt | BIC wt | Perf. Score |
|---------|-------|------|--------|---------|--------|-------------|
| psychosocial F1 | full_continuous | 0.857 | 0.964 | 0.965 | 1.000 | 0.799 |
|  | full_5level | 0.856 | 0.000 | 0.000 | 0.000 | 0.250 |
|  | full_coarsened | 0.857 | 0.036 | 0.035 | 0.000 | 0.018 |
| psychosocial F2 | full_continuous | 0.889 | 0.946 | 0.948 | 1.000 | 0.750 |
|  | full_5level | 0.888 | 0.003 | 0.002 | 0.000 | 0.250 |
|  | full_coarsened | 0.889 | 0.051 | 0.049 | 0.000 | 0.031 |

Full comparison tables exported to LaTeX.

---

## Model Coefficients

All fixed effect coefficients from fitted models.

### psychosocial_F1

#### Additive (lambda_3 Coarsened)

**Cross-Sectional Model:**
`psychosocial_F1 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F1_pre`

|Parameter           | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)         |    -0.01    | [-0.12, 0.09] | p=0.795 | p=0.944 |
|lambda=0            |    0.06     | [-0.07, 0.19] | p=0.358 | p=0.716 |
|lambda>0            |    -0.00    | [-0.11, 0.10] | p=0.944 | p=0.944 |
|personalised        |    0.02     | [-0.08, 0.11] | p=0.708 | p=0.944 |
|emotchat            |    0.06     | [-0.03, 0.16] | p=0.200 | p=0.599 |
|psychosocial_F1_pre |   0.78***   | [0.75, 0.82]  | p<0.001 | p<0.001 |

**Longitudinal Model:**
`psychosocial_F1 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F1_pre`

|Parameter           | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)         |   0.16***   |  [0.08, 0.25]  | p<0.001 | p<0.001 |
|lambda=0            |    -0.01    | [-0.12, 0.10]  | p=0.849 | p=0.849 |
|lambda>0            |    -0.02    | [-0.11, 0.07]  | p=0.637 | p=0.764 |
|personalised        |    0.05     | [-0.03, 0.13]  | p=0.235 | p=0.352 |
|emotchat            |   -0.10*    | [-0.18, -0.02] | p=0.017 | p=0.034 |
|psychosocial_F1_pre |   0.78***   |  [0.75, 0.81]  | p<0.001 | p<0.001 |

#### Additive (lambda Continuous)

**Cross-Sectional Model:**
`psychosocial_F1 ~ lambda + personalisation + domain + psychosocial_F1_pre`

|Parameter           | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)         |    -0.00    | [-0.09, 0.08] | p=0.952 | p=0.952 |
|lambda              |    -0.00    | [-0.07, 0.06] | p=0.916 | p=0.952 |
|personalised        |    0.02     | [-0.08, 0.11] | p=0.696 | p=0.952 |
|emotchat            |    0.06     | [-0.03, 0.16] | p=0.208 | p=0.520 |
|psychosocial_F1_pre |   0.78***   | [0.75, 0.82]  | p<0.001 | p<0.001 |

**Longitudinal Model:**
`psychosocial_F1 ~ lambda + personalisation + domain + psychosocial_F1_pre`

|Parameter           | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)         |   0.15***   |  [0.08, 0.22]  | p<0.001 | p<0.001 |
|lambda              |    -0.01    | [-0.07, 0.04]  | p=0.655 | p=0.655 |
|personalised        |    0.05     | [-0.03, 0.13]  | p=0.237 | p=0.296 |
|emotchat            |   -0.10*    | [-0.18, -0.02] | p=0.017 | p=0.028 |
|psychosocial_F1_pre |   0.78***   |  [0.75, 0.81]  | p<0.001 | p<0.001 |

#### Full/Interaction (lambda_3 Coarsened)

**Cross-Sectional Model:**
`psychosocial_F1 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F1_pre + relationship_seeking_category:personalisation +      relationship_seeking_category:domain`

|Parameter             | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:---------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)           |    -0.06    | [-0.20, 0.07] | p=0.355 | p=0.602 |
|lambda=0              |    0.21     | [-0.02, 0.44] | p=0.074 | p=0.370 |
|lambda>0              |    0.04     | [-0.14, 0.23] | p=0.639 | p=0.710 |
|personalised          |    0.06     | [-0.09, 0.21] | p=0.451 | p=0.644 |
|emotchat              |    0.12     | [-0.03, 0.27] | p=0.112 | p=0.374 |
|psychosocial_F1_pre   |   0.78***   | [0.75, 0.82]  | p<0.001 | p<0.001 |
|lambda=0:personalised |    -0.12    | [-0.39, 0.14] | p=0.361 | p=0.602 |
|lambda>0:personalised |    -0.04    | [-0.25, 0.17] | p=0.719 | p=0.719 |
|lambda=0:emotchat     |    -0.18    | [-0.45, 0.08] | p=0.171 | p=0.428 |
|lambda>0:emotchat     |    -0.06    | [-0.27, 0.15] | p=0.586 | p=0.710 |

**Longitudinal Model:**
`psychosocial_F1 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F1_pre + relationship_seeking_category:personalisation +      relationship_seeking_category:domain`

|Parameter             | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:---------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)           |    0.15*    | [0.04, 0.25]  | p=0.009 | p=0.043 |
|lambda=0              |    0.04     | [-0.15, 0.23] | p=0.660 | p=0.736 |
|lambda>0              |    -0.01    | [-0.16, 0.15] | p=0.935 | p=0.935 |
|personalised          |    0.03     | [-0.10, 0.15] | p=0.648 | p=0.736 |
|emotchat              |    -0.04    | [-0.17, 0.08] | p=0.490 | p=0.736 |
|psychosocial_F1_pre   |   0.78***   | [0.75, 0.81]  | p<0.001 | p<0.001 |
|lambda=0:personalised |    -0.05    | [-0.27, 0.16] | p=0.632 | p=0.736 |
|lambda>0:personalised |    0.07     | [-0.10, 0.25] | p=0.421 | p=0.736 |
|lambda=0:emotchat     |    -0.05    | [-0.27, 0.17] | p=0.662 | p=0.736 |
|lambda>0:emotchat     |    -0.10    | [-0.28, 0.07] | p=0.248 | p=0.736 |

#### Full/Interaction (lambda Continuous)

**Cross-Sectional Model:**
`psychosocial_F1 ~ lambda + personalisation + domain + psychosocial_F1_pre +      lambda:personalisation + lambda:domain`

|Parameter           | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)         |    -0.00    | [-0.09, 0.08] | p=0.942 | p=0.942 |
|lambda              |    0.03     | [-0.09, 0.15] | p=0.598 | p=0.893 |
|personalised        |    0.02     | [-0.08, 0.12] | p=0.683 | p=0.893 |
|emotchat            |    0.06     | [-0.03, 0.16] | p=0.206 | p=0.722 |
|psychosocial_F1_pre |   0.78***   | [0.75, 0.82]  | p<0.001 | p<0.001 |
|lambda:personalised |    -0.02    | [-0.16, 0.11] | p=0.766 | p=0.893 |
|lambda:emotchat     |    -0.05    | [-0.19, 0.08] | p=0.453 | p=0.893 |

**Longitudinal Model:**
`psychosocial_F1 ~ lambda + personalisation + domain + psychosocial_F1_pre +      lambda:personalisation + lambda:domain`

|Parameter           | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)         |   0.15***   |  [0.08, 0.22]  | p<0.001 | p<0.001 |
|lambda              |    0.00     | [-0.10, 0.10]  | p=0.964 | p=0.964 |
|personalised        |    0.05     | [-0.03, 0.13]  | p=0.238 | p=0.333 |
|emotchat            |   -0.10*    | [-0.18, -0.02] | p=0.017 | p=0.039 |
|psychosocial_F1_pre |   0.78***   |  [0.75, 0.81]  | p<0.001 | p<0.001 |
|lambda:personalised |    0.06     | [-0.05, 0.17]  | p=0.314 | p=0.367 |
|lambda:emotchat     |    -0.09    | [-0.20, 0.02]  | p=0.123 | p=0.215 |

---

### psychosocial_F2

#### Additive (lambda_3 Coarsened)

**Cross-Sectional Model:**
`psychosocial_F2 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F2_pre`

|Parameter           | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)         |    0.02     | [-0.09, 0.13] | p=0.705 | p=0.732 |
|lambda=0            |    -0.09    | [-0.22, 0.05] | p=0.217 | p=0.368 |
|lambda>0            |    -0.02    | [-0.13, 0.09] | p=0.732 | p=0.732 |
|personalised        |    -0.06    | [-0.16, 0.04] | p=0.245 | p=0.368 |
|emotchat            |    -0.09    | [-0.19, 0.01] | p=0.075 | p=0.226 |
|psychosocial_F2_pre |   0.75***   | [0.71, 0.79]  | p<0.001 | p<0.001 |

**Longitudinal Model:**
`psychosocial_F2 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F2_pre`

|Parameter           | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)         |  -0.19***   | [-0.28, -0.10] | p<0.001 | p<0.001 |
|lambda=0            |    0.01     | [-0.10, 0.12]  | p=0.843 | p=0.843 |
|lambda>0            |    0.04     | [-0.05, 0.13]  | p=0.359 | p=0.431 |
|personalised        |    -0.04    | [-0.12, 0.04]  | p=0.344 | p=0.431 |
|emotchat            |    0.06     | [-0.02, 0.14]  | p=0.146 | p=0.291 |
|psychosocial_F2_pre |   0.74***   |  [0.71, 0.77]  | p<0.001 | p<0.001 |

#### Additive (lambda Continuous)

**Cross-Sectional Model:**
`psychosocial_F2 ~ lambda + personalisation + domain + psychosocial_F2_pre`

|Parameter           | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)         |    -0.00    | [-0.09, 0.08] | p=0.921 | p=0.921 |
|lambda              |    -0.01    | [-0.08, 0.06] | p=0.855 | p=0.921 |
|personalised        |    -0.06    | [-0.16, 0.04] | p=0.236 | p=0.393 |
|emotchat            |    -0.09    | [-0.19, 0.01] | p=0.080 | p=0.199 |
|psychosocial_F2_pre |   0.75***   | [0.71, 0.79]  | p<0.001 | p<0.001 |

**Longitudinal Model:**
`psychosocial_F2 ~ lambda + personalisation + domain + psychosocial_F2_pre`

|Parameter           | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)         |  -0.17***   | [-0.24, -0.10] | p<0.001 | p<0.001 |
|lambda              |    0.03     | [-0.03, 0.09]  | p=0.288 | p=0.346 |
|personalised        |    -0.04    | [-0.12, 0.04]  | p=0.346 | p=0.346 |
|emotchat            |    0.06     | [-0.02, 0.14]  | p=0.146 | p=0.243 |
|psychosocial_F2_pre |   0.74***   |  [0.71, 0.77]  | p<0.001 | p<0.001 |

#### Full/Interaction (lambda_3 Coarsened)

**Cross-Sectional Model:**
`psychosocial_F2 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F2_pre + relationship_seeking_category:personalisation +      relationship_seeking_category:domain`

|Parameter             | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:---------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)           |    0.06     | [-0.08, 0.20] | p=0.423 | p=0.604 |
|lambda=0              |    -0.21    | [-0.45, 0.03] | p=0.086 | p=0.383 |
|lambda>0              |    -0.05    | [-0.24, 0.15] | p=0.631 | p=0.789 |
|personalised          |    -0.10    | [-0.25, 0.06] | p=0.232 | p=0.581 |
|emotchat              |    -0.13    | [-0.28, 0.03] | p=0.115 | p=0.383 |
|psychosocial_F2_pre   |   0.75***   | [0.71, 0.79]  | p<0.001 | p<0.001 |
|lambda=0:personalised |    0.12     | [-0.16, 0.39] | p=0.403 | p=0.604 |
|lambda>0:personalised |    0.04     | [-0.19, 0.26] | p=0.753 | p=0.837 |
|lambda=0:emotchat     |    0.14     | [-0.13, 0.41] | p=0.317 | p=0.604 |
|lambda>0:emotchat     |    0.02     | [-0.20, 0.25] | p=0.844 | p=0.844 |

**Longitudinal Model:**
`psychosocial_F2 ~ relationship_seeking_category + personalisation +      domain + psychosocial_F2_pre + relationship_seeking_category:personalisation +      relationship_seeking_category:domain`

|Parameter             | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:---------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)           |   -0.17*    | [-0.28, -0.06] | p=0.003 | p=0.016 |
|lambda=0              |    -0.08    | [-0.27, 0.12]  | p=0.426 | p=0.666 |
|lambda>0              |    0.04     | [-0.12, 0.20]  | p=0.600 | p=0.666 |
|personalised          |    -0.04    | [-0.17, 0.09]  | p=0.580 | p=0.666 |
|emotchat              |    0.02     | [-0.11, 0.15]  | p=0.764 | p=0.764 |
|psychosocial_F2_pre   |   0.74***   |  [0.71, 0.77]  | p<0.001 | p<0.001 |
|lambda=0:personalised |    0.11     | [-0.12, 0.33]  | p=0.346 | p=0.666 |
|lambda>0:personalised |    -0.06    | [-0.25, 0.12]  | p=0.506 | p=0.666 |
|lambda=0:emotchat     |    0.07     | [-0.16, 0.29]  | p=0.554 | p=0.666 |
|lambda>0:emotchat     |    0.06     | [-0.12, 0.25]  | p=0.491 | p=0.666 |

#### Full/Interaction (lambda Continuous)

**Cross-Sectional Model:**
`psychosocial_F2 ~ lambda + personalisation + domain + psychosocial_F2_pre +      lambda:personalisation + lambda:domain`

|Parameter           | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)         |    -0.00    | [-0.09, 0.08] | p=0.926 | p=0.926 |
|lambda              |    -0.03    | [-0.15, 0.10] | p=0.672 | p=0.926 |
|personalised        |    -0.06    | [-0.16, 0.04] | p=0.233 | p=0.543 |
|emotchat            |    -0.09    | [-0.19, 0.01] | p=0.079 | p=0.278 |
|psychosocial_F2_pre |   0.75***   | [0.71, 0.79]  | p<0.001 | p<0.001 |
|lambda:personalised |    0.01     | [-0.13, 0.15] | p=0.869 | p=0.926 |
|lambda:emotchat     |    0.03     | [-0.11, 0.17] | p=0.687 | p=0.926 |

**Longitudinal Model:**
`psychosocial_F2 ~ lambda + personalisation + domain + psychosocial_F2_pre +      lambda:personalisation + lambda:domain`

|Parameter           | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:-------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)         |  -0.17***   | [-0.24, -0.10] | p<0.001 | p<0.001 |
|lambda              |    0.02     | [-0.08, 0.12]  | p=0.678 | p=0.678 |
|personalised        |    -0.04    | [-0.12, 0.04]  | p=0.348 | p=0.419 |
|emotchat            |    0.06     | [-0.02, 0.14]  | p=0.146 | p=0.341 |
|psychosocial_F2_pre |   0.74***   |  [0.71, 0.77]  | p<0.001 | p<0.001 |
|lambda:personalised |    -0.05    | [-0.17, 0.06]  | p=0.359 | p=0.419 |
|lambda:emotchat     |    0.07     | [-0.04, 0.19]  | p=0.206 | p=0.360 |

---


---

## Robustness Checks

Robustness analyses test whether treatment effects hold under
alternative specifications. Cells show coefficient (SE) with
significance: *p<.05, **p<.01, ***p<.001 (FDR-corrected).

**Specifications:**
- **Additive**: Base treatment effects (no interactions)
- **+ Interactions**: Full model with treatment interactions
- **Full + Demos**: Full model + demographic controls
- **Full + Prefs**: Full model + AI pre-treatment pref groups
- **Full + IPW**: Full model with IPW weights (attrition adjustment)

### Cross-Sectional

| Outcome | Predictor | Additive | + Interactions | Full + Demos | Full + Prefs | Full + IPW |
|---|---|---|---|---|---|---|
| psychosocial_F1 | lambda | -0.03 (0.05) | 0.01 (0.10) | 0.00 (0.09) | 0.01 (0.10) | 0.01 (0.10) |
| psychosocial_F1 | personalisationpersonalised | 0.14 (0.08) | 0.14 (0.08) | 0.14 (0.08) | 0.14 (0.08) | 0.13 (0.08) |
| psychosocial_F1 | domainemotchat | 0.02 (0.08) | 0.02 (0.08) | 0.01 (0.08) | 0.02 (0.08) | 0.01 (0.08) |
| psychosocial_F2 | lambda | 0.03 (0.05) | -0.02 (0.09) | -0.01 (0.09) | -0.02 (0.09) | -0.01 (0.09) |
| psychosocial_F2 | personalisationpersonalised | -0.13 (0.08) | -0.13 (0.08) | -0.15 (0.07) | -0.14 (0.08) | -0.13 (0.08) |
| psychosocial_F2 | domainemotchat | -0.12 (0.08) | -0.12 (0.08) | -0.12 (0.07) | -0.12 (0.08) | -0.11 (0.08) |

### Longitudinal

| Outcome | Predictor | Additive | + Interactions | Full + Demos | Full + Prefs | Full + IPW |
|---|---|---|---|---|---|---|
| psychosocial_F1 | lambda | 0.00 (0.05) | 0.04 (0.08) | 0.05 (0.08) | 0.02 (0.08) | 0.04 (0.08) |
| psychosocial_F1 | personalisationpersonalised | 0.03 (0.07) | 0.03 (0.06) | 0.03 (0.06) | 0.03 (0.06) | 0.03 (0.07) |
| psychosocial_F1 | domainemotchat | -0.20 (0.07)* | -0.20 (0.06)* | -0.19 (0.06)* | -0.20 (0.06)* | -0.20 (0.07)* |
| psychosocial_F2 | lambda | 0.04 (0.04) | -0.02 (0.08) | -0.04 (0.08) | -0.02 (0.08) | -0.02 (0.08) |
| psychosocial_F2 | personalisationpersonalised | 0.02 (0.06) | 0.02 (0.06) | 0.01 (0.06) | 0.02 (0.06) | 0.01 (0.06) |
| psychosocial_F2 | domainemotchat | 0.14 (0.06) | 0.14 (0.06) | 0.13 (0.06) | 0.14 (0.06) | 0.14 (0.06) |

Full tables: `psychosocial_robustness_{cs,long}.tex`

---

## ANCOVA: Domain × Study Interaction

Tests whether the effect of domain (EmotChat vs PolChat) differs
between cross-sectional and longitudinal studies, controlling for
pre-treatment scores.

### Psychosocial F1

**Combined Studies Model:**
`outcome_value ~ outcome_value_pre + domain * study_id`

|Parameter                   | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:---------------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)                 |    0.01     | [-0.06, 0.07]  | p=0.821 | p=0.821 |
|outcome_value_pre           |   0.78***   |  [0.76, 0.80]  | p<0.001 | p<0.001 |
|emotchat                    |    0.06     | [-0.03, 0.15]  | p=0.208 | p=0.260 |
|study:longitudinal          |   0.17***   |  [0.08, 0.26]  | p<0.001 | p<0.001 |
|emotchat:study:longitudinal |   -0.16*    | [-0.28, -0.03] | p=0.013 | p=0.021 |

### Psychosocial F2

**Combined Studies Model:**
`outcome_value ~ outcome_value_pre + domain * study_id`

|Parameter                   | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:---------------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)                 |    -0.04    | [-0.11, 0.03]  | p=0.286 | p=0.286 |
|outcome_value_pre           |   0.74***   |  [0.72, 0.77]  | p<0.001 | p<0.001 |
|emotchat                    |    -0.09    | [-0.18, 0.01]  | p=0.085 | p=0.106 |
|study:longitudinal          |   -0.15**   | [-0.24, -0.06] | p<0.001 | p=0.002 |
|emotchat:study:longitudinal |    0.15*    |  [0.02, 0.27]  | p=0.026 | p=0.044 |

---

## Combined Study Model

Tests whether psychosocial outcomes differ between single-session (cross-sectional)
and month-long (longitudinal) AI exposure.

### Psychosocial F1

**Combined Studies Model:**
`psychosocial_F1 ~ study_type`

|Parameter              | Coefficient |    95% CI     | P (Raw) | P (FDR) |
|:----------------------|:-----------:|:-------------:|:-------:|:-------:|
|(Intercept)            |   0.12**    | [0.04, 0.19]  | p=0.002 | p=0.004 |
|study_typelongitudinal |    -0.01    | [-0.11, 0.09] | p=0.870 | p=0.870 |

### Psychosocial F2

**Combined Studies Model:**
`psychosocial_F2 ~ study_type`

|Parameter              | Coefficient |     95% CI     | P (Raw) | P (FDR) |
|:----------------------|:-----------:|:--------------:|:-------:|:-------:|
|(Intercept)            |  -0.16***   | [-0.23, -0.08] | p<0.001 | p<0.001 |
|study_typelongitudinal |    0.01     | [-0.08, 0.11]  | p=0.803 | p=0.803 |

---

## Output Files

### Figures
- `outputs/figures/main_studies/png/psychosocial_*.png`
- `outputs/figures/main_studies/pdf/psychosocial_*.pdf`

### LaTeX Tables
- `outputs/tables/main_studies/psychosocial_tables.tex` (parent)
- Individual table files: `psychosocial_*.tex`

### Models
- `outputs/models/psychosocial_cross_sectional.rds`
- `outputs/models/psychosocial_longitudinal.rds`
- `outputs/models/psychosocial_data.rds`

### Statistics
- `outputs/stats/psychosocial_contrasts.json`
