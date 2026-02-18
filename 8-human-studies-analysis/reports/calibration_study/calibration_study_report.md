# Calibration Study Analysis Report

*Generated: 2026-02-18 10:21:04.394056*

---

## Overview

This analysis validates the relationship-seeking multiplier ($\lambda$) using a
calibration study design. Participants rated AI conversations on coherence (off-target)
and relationship-seeking qualities (on-target) across different multiplier levels.

---

## Data Summary

- **Rating observations:** 1188
- **Rating participants:** 297
- **Ranking observations:** 2368
- **Ranking participants:** 296

---

## Rating EDA

### Multiplier Distribution

```

-1.5   -1 -0.5    0  0.5    1  1.5 
 113  148  157  349  139  140  142 
```

### Conversation Modes

```

pre-populated       dynamic 
          891           297 
```

### Trajectories by Multiplier (Means)

![Rating Trajectories](../../outputs/figures/calibration_study/png/rating_trajectories.png)

### Distributions (Means)

![Rating Distributions](../../outputs/figures/calibration_study/png/rating_distributions.png)

### Coherence Items

![Coherence Distribution](../../outputs/figures/calibration_study/png/coherence_items_distribution.png)

![Coherence Trajectory](../../outputs/figures/calibration_study/png/coherence_items_trajectory.png)

### Relationship-Seeking Items

![RS Distribution](../../outputs/figures/calibration_study/png/rs_items_distribution.png)

![RS Trajectory](../../outputs/figures/calibration_study/png/rs_items_trajectory.png)

### Preference Items (incl. WTP)

![Preference Distribution](../../outputs/figures/calibration_study/png/preference_items_distribution.png)

![Preference Trajectory](../../outputs/figures/calibration_study/png/preference_items_trajectory.png)

---

## Rating Regression Results

**Model (Truncated Quadratic):**

`outcome ~ conversation_mode + multiplier + I(multiplier^2) + (1 | ppt_id)`

(Truncated = excluding $\lambda = \pm 1.5$)

### Coherence mean

**Model Comparison:**

| Model | AIC | BIC | RMSE | Performance Score |
|-------|-----|-----|------|-------------------|
| Truncated Quadratic | 7318.5 | 7347.6 | 8.72 | 0.998 |
| Truncated Linear | 7334.4 | 7358.6 | 8.84 | 0.002 |
| Non-linear | 9699.5 | 9750.3 | 11.39 | 0.000 |
| Quadratic | 9717.4 | 9747.9 | 11.45 | 0.000 |
| Linear | 9835.1 | 9860.5 | 12.18 | 0.000 |
| Null | 9879.8 | 9895.1 | 12.52 | 0.000 |

**Best model coefficients (Truncated Quadratic):**

| Term | Estimate | SE | t | p |
|------|----------|----|----|---|
| (Intercept) | 88.129 | 0.719 | 122.65 | <0.001 |
| conversation_modedynamic | -0.841 | 0.797 | -1.06 | 0.291 |
| multiplier | -0.455 | 0.556 | -0.82 | 0.413 |
| I(multiplier^2) | -3.422 | 0.842 | -4.07 | <0.001 |

### Relationship seeking mean

**Model Comparison:**

| Model | AIC | BIC | RMSE | Performance Score |
|-------|-----|-----|------|-------------------|
| Truncated Quadratic | 8073.1 | 8102.1 | 13.36 | 0.787 |
| Truncated Linear | 8078.1 | 8102.3 | 13.42 | 0.213 |
| Non-linear | 10283.7 | 10334.5 | 14.53 | 0.000 |
| Quadratic | 10313.3 | 10343.7 | 14.75 | 0.000 |
| Linear | 10360.0 | 10385.4 | 15.11 | 0.000 |
| Null | 11104.6 | 11119.9 | 23.57 | 0.000 |

**Best model coefficients (Truncated Quadratic):**

| Term | Estimate | SE | t | p |
|------|----------|----|----|---|
| (Intercept) | 61.926 | 1.056 | 58.66 | <0.001 |
| conversation_modedynamic | 1.446 | 1.212 | 1.19 | 0.233 |
| multiplier | 22.126 | 0.845 | 26.18 | <0.001 |
| I(multiplier^2) | -2.756 | 1.278 | -2.16 | 0.031 |

### Preference mean

**Model Comparison:**

| Model | AIC | BIC | RMSE | Performance Score |
|-------|-----|-----|------|-------------------|
| Truncated Quadratic | 8447.5 | 8476.5 | 17.42 | 1.000 |
| Truncated Linear | 8472.1 | 8496.3 | 17.73 | 0.000 |
| Non-linear | 10881.2 | 10932.0 | 19.64 | 0.000 |
| Quadratic | 10898.9 | 10929.4 | 19.76 | 0.000 |
| Linear | 11043.0 | 11068.4 | 21.45 | 0.000 |
| Null | 11133.3 | 11148.5 | 22.61 | 0.000 |

**Best model coefficients (Truncated Quadratic):**

| Term | Estimate | SE | t | p |
|------|----------|----|----|---|
| (Intercept) | 69.300 | 1.218 | 56.89 | <0.001 |
| conversation_modedynamic | -0.329 | 1.541 | -0.21 | 0.831 |
| multiplier | 10.116 | 1.071 | 9.45 | <0.001 |
| I(multiplier^2) | -7.940 | 1.615 | -4.92 | <0.001 |

### Rating Model Effects (5 Specifications)

#### Coherence

![Coherence Effects](../../outputs/figures/calibration_study/png/coherence_effects_combined.png)

#### Relationship-Seeking

![RS Effects](../../outputs/figures/calibration_study/png/relationship_seeking_effects_combined.png)

#### Preference

![Preference Effects](../../outputs/figures/calibration_study/png/preference_effects_combined.png)

---

## Ranking EDA

### Multiplier Counts (by Subtask)

![Multiplier Counts](../../outputs/figures/calibration_study/png/ranking_multiplier_counts.png)

### Mean Rank by Ranking Type (Multi-chat)

![Mean Rank by Type](../../outputs/figures/calibration_study/png/ranking_mean_rank_by_type.png)

### Mean Rank by Subtask (Relationship-seeking)

![Mean Rank by Subtask](../../outputs/figures/calibration_study/png/ranking_mean_rank_by_subtask.png)

### Rank Distribution Heatmap

![Rank Heatmap](../../outputs/figures/calibration_study/png/ranking_heatmap.png)

### Correct Ranking Order (Relationship-Seeking)

![Correct Order](../../outputs/figures/calibration_study/png/ranking_correct_order.png)

### Winrate Analysis

![Winrate Heatmap](../../outputs/figures/calibration_study/png/winrate_heatmap.png)

![Mean Winrate](../../outputs/figures/calibration_study/png/winrate_mean.png)

---

## Ranking Regression Results (PlackettLuce)

**Model:** Plackett-Luce model estimating worth parameters for each multiplier level.

(Truncated = excluding $\lambda = \pm 1.5$)

### Preference Rankings

![Preference Log-Worth](../../outputs/figures/calibration_study/png/plackett_luce_preference.png)

### Relationship-Seeking Rankings

![RS Log-Worth](../../outputs/figures/calibration_study/png/plackett_luce_relationship_seeking.png)

