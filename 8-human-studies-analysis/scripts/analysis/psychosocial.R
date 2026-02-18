#!/usr/bin/env Rscript
# =============================================================================
# Psychosocial Wellbeing Analysis
# =============================================================================
#
# Regression analysis of psychosocial wellbeing outcomes:
#   - Main outcomes: psychosocial_F1, psychosocial_F2 (factor scores)
#   - Individual scales: phq_gad_score, ucla_score, lubben_score, who_score
#
#
# Usage:
#   Rscript scripts/analysis/psychosocial.R
#   Rscript scripts/analysis/psychosocial.R --generate_tex_tables
#   Rscript scripts/analysis/psychosocial.R --generate_report
#
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(knitr)
library(sjPlot)

set.seed(1234)

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
generate_tex_tables <- "--generate_tex_tables" %in% args
generate_report <- "--generate_report" %in% args

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

PROJECT_ROOT <- paths$PROJECT_ROOT
REPO_ROOT <- paths$REPO_ROOT
FIGURE_DIR <- paths$FIGURE_DIR
TABLE_DIR <- file.path(PROJECT_ROOT, "outputs/tables/main_studies")
STATS_DIR <- file.path(PROJECT_ROOT, "outputs/stats")
MODEL_DIR <- file.path(PROJECT_ROOT, "outputs/models")
REPORT_DIR <- paths$REPORT_DIR
DATA_DIR <- paths$DATA_DIR
GENERATED_DIR <- paths$GENERATED_DIR

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

# Source utilities
source(file.path(PROJECT_ROOT, "scripts/utils_r/data_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/coarsen_and_factor_sociodemos.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_config.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/labelling_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/eda_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/regression_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/model_comparison_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/robustness_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/extract_coefficients.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/sjplot_to_latex.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/report_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Psychosocial Wellbeing Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variables
MAIN_OUTCOMES <- c("psychosocial_F1", "psychosocial_F2")
INDIVIDUAL_OUTCOMES <- c("phq_gad_score", "ucla_score", "lubben_score", "who_score")
ALL_OUTCOMES <- c(MAIN_OUTCOMES, INDIVIDUAL_OUTCOMES)

# =============================================================================
# SECTION 2: DATA LOADING & TRANSFORMATION
# =============================================================================

cat("\n--- Loading Data ---\n\n")

# Load raw psychosocial data
psychosocial_raw <- load_task_data("psychosocial", DATA_DIR)

# Filter to participants with BOTH pre AND post data (paired only)
ppts_with_both <- psychosocial_raw %>%
  group_by(ppt_id) %>%
  summarise(
    has_pre = any(timepoint == "pre"),
    has_post = any(timepoint == "post"),
    .groups = "drop"
  ) %>%
  filter(has_pre & has_post) %>%
  pull(ppt_id)

psychosocial_raw <- psychosocial_raw %>%
  filter(ppt_id %in% ppts_with_both)
cat(sprintf("Filtered to %d participants with both pre and post data\n", length(ppts_with_both)))
cat(sprintf("Remaining observations: %d\n", nrow(psychosocial_raw)))

# Pivot to wide format (1 row per participant)
# Keep treatment arms from post timepoint
psychosocial_raw <- psychosocial_raw %>%
  distinct(ppt_id, timepoint, .keep_all = TRUE)

psychosocial_wide <- psychosocial_raw %>%
  select(ppt_id, study_id, timepoint,
         phq_gad_score, ucla_score, lubben_score, who_score,
         multiplier, domain, personalisation) %>%
  # Get treatment arms from post timepoint
  group_by(ppt_id) %>%
  mutate(
    multiplier = first(multiplier[timepoint == "post"]),
    domain = first(domain[timepoint == "post"]),
    personalisation = first(personalisation[timepoint == "post"]),
    study_id = first(study_id[timepoint == "post"])
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(ppt_id, study_id, multiplier, domain, personalisation),
    names_from = timepoint,
    values_from = c(phq_gad_score, ucla_score, lubben_score, who_score),
    names_glue = "{.value}_{timepoint}"
  ) %>%
  rename(
    # Post values are the outcomes (drop _post suffix for main outcome vars)
    phq_gad_score = phq_gad_score_post,
    ucla_score = ucla_score_post,
    lubben_score = lubben_score_post,
    who_score = who_score_post
  )

cat("Pivoted to wide format:", nrow(psychosocial_wide), "participants\n")

# Load factor scores and rename to _pre pattern
factors <- load_task_data("psychosocial_factors", GENERATED_DIR) %>%
  mutate(
    across(c(pre_psychosocial_F1, post_psychosocial_F1,
             pre_psychosocial_F2, post_psychosocial_F2), as.numeric)
  ) %>%
  rename(
    psychosocial_F1_pre = pre_psychosocial_F1,
    psychosocial_F1 = post_psychosocial_F1,
    psychosocial_F2_pre = pre_psychosocial_F2,
    psychosocial_F2 = post_psychosocial_F2
  ) %>%
  filter(!is.na(psychosocial_F1) & !is.na(psychosocial_F2)) %>%
  select(ppt_id, study_id, psychosocial_F1, psychosocial_F1_pre,
         psychosocial_F2, psychosocial_F2_pre)
cat("Factor scores with post data:", nrow(factors), "participants\n")

# Merge factors with wide data (join by both ppt_id and study_id)
data_merged <- psychosocial_wide %>%
  left_join(factors, by = c("ppt_id", "study_id")) %>%
  filter(!is.na(psychosocial_F1) & !is.na(psychosocial_F2))
cat("Merged data (complete factor scores):", nrow(data_merged), "participants\n")

# Prepare treatment arms and filter to main studies
data_prepped <- prepare_treatment_arms(data_merged) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

cat("After treatment arm prep:", nrow(data_prepped), "observations\n")

# Split by study type
data_cross_sectional <- data_prepped %>% filter(study_id == "cross-sectional")
data_longitudinal <- data_prepped %>% filter(study_id == "longitudinal")

cat("  Cross-sectional:", nrow(data_cross_sectional), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "participants\n")

# Load participant characteristics (sociodemos, pref groups, IPW weights)
# Note: include_psychosocial = FALSE since we already have factors from factors.jsonl
ppt_chars <- load_ppt_characteristics(
  DATA_DIR, GENERATED_DIR,
  include_psychosocial = FALSE,
  apply_coarsening = TRUE
)

# Merge participant characteristics (incl. IPW weights) with main datasets
data_cross_sectional <- data_cross_sectional %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))
data_longitudinal <- data_longitudinal %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))

# Create pooled data for plotting (with pre values for delta calculations)
cat("\nCreating pooled data...\n")
data_cross_sectional_pooled <- create_pooled_data(
  data = data_cross_sectional,
  outcome_vars = ALL_OUTCOMES,
  include_pre = TRUE
) %>%
  mutate(outcome_value_delta = outcome_value - outcome_value_pre)

data_longitudinal_pooled <- create_pooled_data(
  data = data_longitudinal,
  outcome_vars = ALL_OUTCOMES,
  include_pre = TRUE
) %>%
  mutate(outcome_value_delta = outcome_value - outcome_value_pre)

cat("  Cross-sectional pooled:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Longitudinal pooled:", nrow(data_longitudinal_pooled), "rows\n")

cat("\nData loading complete.\n")

# =============================================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n--- Exploratory Data Analysis ---\n\n")

# Print dataset summary for main outcomes
print_dataset_summary(data_cross_sectional, "psychosocial_F1")
print_dataset_summary(data_longitudinal, "psychosocial_F1")

# --- Correlation Heatmaps ---
cat("\nCreating correlation heatmaps...\n")
corr_plot <- plot_corr_heatmaps_combined(
  data_cs = data_cross_sectional,
  data_long = data_longitudinal,
  outcome_vars = ALL_OUTCOMES
)
save_plot(FIGURE_DIR, corr_plot, "psychosocial_correlation_heatmap", 14, 7)

# --- Distribution Histograms ---
cat("\nCreating distribution histograms...\n")

# Create combined data for distributions
psychosocial_combined <- bind_rows(
  data_cross_sectional %>% select(ppt_id, study_id, all_of(ALL_OUTCOMES)),
  data_longitudinal %>% select(ppt_id, study_id, all_of(ALL_OUTCOMES))
)

# Custom distribution plot with free scales (outcomes have different ranges)
dist_data <- psychosocial_combined %>%
  pivot_longer(
    cols = all_of(ALL_OUTCOMES),
    names_to = "outcome",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(outcome = factor(outcome, levels = ALL_OUTCOMES))

dist_plot <- ggplot(dist_data, aes(x = value, fill = study_id)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.85) +
  facet_grid(study_id ~ outcome, scales = "free") +
  scale_fill_study() +
  labs(
    x = "Score",
    y = "Count",
    title = "Distribution of Psychosocial Outcomes"
  ) +
  theme_pub() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

save_plot(FIGURE_DIR, dist_plot, "psychosocial_distributions", 16, 6)

# --- Individual Scales Distribution Plot (free scales) ---
cat("\nCreating individual scales distribution plot...\n")

individual_labels <- c(
  "phq_gad_score" = "PHQ-GAD",
  "ucla_score" = "UCLA",
  "lubben_score" = "Lubben",
  "who_score" = "WHO"
)

dist_plot_individual <- plot_distributions(
  data = psychosocial_combined,
  outcome_vars = INDIVIDUAL_OUTCOMES,
  outcome_labels = individual_labels,
  binwidth = 2,
  scales = "free"
)

save_plot(FIGURE_DIR, dist_plot_individual,
          "psychosocial_distributions_individual", 14, 6)

# --- Pre-Post Change Plot ---
cat("\nCreating pre-post change plot...\n")

# Combine both studies for pre-post plots (both have pre and post)
data_combined <- bind_rows(data_cross_sectional, data_longitudinal)

p_pre_post <- plot_pre_post_change_combined(
  data = data_combined,
  outcome_vars = MAIN_OUTCOMES,
  facet_var = "study_id",
  total_height = 8,
  total_width = 12,
  figure_dir = FIGURE_DIR,
  save_pdf = TRUE,
  filename_prefix = "psychosocial"
)

cat("Saved: psychosocial_pre_post_combined.pdf/png\n")

# --- Pre-Post Correlation Scatter Plot ---
cat("\nCreating pre-post correlation scatter plot...\n")

p_corr <- plot_pre_post_correlations(
  data = data_combined,
  outcome_vars = MAIN_OUTCOMES,
  total_height = 5,
  total_width = 12,
  figure_dir = FIGURE_DIR,
  save_pdf = TRUE,
  filename_prefix = "psychosocial"
)

cat("Saved: psychosocial_pre_post_corr.pdf/png\n")

# --- Plot by Lambda (change scores) ---
cat("\nCreating raw lambda plot (change scores)...\n")

p_lambda <- plot_raw_lambda(
  data_long = data_longitudinal_pooled,
  data_cross = data_cross_sectional_pooled,
  plot_study = "both",
  plot_smoothed = FALSE,
  outcome_var = "outcome_value_delta",
  height = 10, width = 10
)
save_plot(FIGURE_DIR, p_lambda, "psychosocial_by_lambda", 10, 10)

cat("Saved: psychosocial_by_lambda.pdf/png\n")

cat("\nEDA complete.\n")

# =============================================================================
# SECTION 5A: FUNCTIONAL FORM COMPARISON (Main Outcomes)
# =============================================================================

cat("\n--- Functional Form Comparison (Main Outcomes) ---\n\n")

perf_cross_sectional <- list()
perf_longitudinal <- list()
best_specs_cs <- data.frame()
best_specs_long <- data.frame()

for (outcome in MAIN_OUTCOMES) {
  cat(sprintf("\n=== %s ===\n", outcome))

  # Cross-sectional (pre-post design with baseline control)
  cat("\n  Cross-Sectional:\n")
  result_cs <- compare_functional_forms(
    data = data_cross_sectional,
    outcome_var = outcome,
    add_domain = TRUE,
    add_pre = TRUE,
    add_time = FALSE,
    model_family = "ols",
    select_by = "AIC"
  )
  perf_cross_sectional[[outcome]] <- result_cs$comparison
  best_specs_cs <- rbind(best_specs_cs, data.frame(
    outcome = outcome,
    best_spec = result_cs$best_spec,
    stringsAsFactors = FALSE
  ))
  cat(sprintf("    Best: %s\n", result_cs$best_spec))
  print(result_cs$comparison)

  # Longitudinal (pre-post design with baseline control)
  cat("\n  Longitudinal:\n")
  result_long <- compare_functional_forms(
    data = data_longitudinal,
    outcome_var = outcome,
    add_domain = TRUE,
    add_pre = TRUE,
    add_time = FALSE,
    model_family = "ols",
    select_by = "AIC"
  )
  perf_longitudinal[[outcome]] <- result_long$comparison
  best_specs_long <- rbind(best_specs_long, data.frame(
    outcome = outcome,
    best_spec = result_long$best_spec,
    stringsAsFactors = FALSE
  ))
  cat(sprintf("    Best: %s\n", result_long$best_spec))
  print(result_long$comparison)
}

cat("\nBest specifications (Cross-Sectional, by AIC):\n")
print(best_specs_cs)
cat("\nBest specifications (Longitudinal, by AIC):\n")
print(best_specs_long)


# =============================================================================
# SECTION 5B: FIT ALL MODELS (Main Outcomes: F1, F2)
# =============================================================================

cat("\n--- Fitting Models (Main Outcomes) ---\n\n")

mods_cross_sectional <- list()
mods_longitudinal <- list()

for (outcome in MAIN_OUTCOMES) {
  cat("\n=== Fitting models for:", outcome, "===\n")

  # Get best spec for this outcome
  best_spec_cs <- best_specs_cs$best_spec[best_specs_cs$outcome == outcome]
  best_spec_long <- best_specs_long$best_spec[best_specs_long$outcome == outcome]

  cat("  Using", best_spec_cs, "for cross-sectional,", best_spec_long, "for longitudinal\n")

  # Cross-sectional (OLS with pre-treatment control)
  cat("\nCross-sectional:\n")
  mods_cross_sectional[[outcome]] <- fit_models(
    data = data_cross_sectional,
    outcome_var = outcome,
    continuous_spec = best_spec_cs,
    add_domain = TRUE,
    add_pre = TRUE,  # Control for baseline
    add_time = FALSE,
    model_family = "ols"
  )

  # Longitudinal (OLS with pre-treatment control)
  cat("\nLongitudinal:\n")
  mods_longitudinal[[outcome]] <- fit_models(
    data = data_longitudinal,
    outcome_var = outcome,
    continuous_spec = best_spec_long,
    add_domain = TRUE,
    add_pre = TRUE,  # Control for baseline
    add_time = FALSE,  # Pre-post only (no repeated measures)
    model_family = "ols"
  )
}

cat("\n--- Printing Model Coefficients (Main Outcomes) ---\n")

print_model_coefficients_section(
  mods_cross_sectional = mods_cross_sectional,
  mods_longitudinal = mods_longitudinal,
  outcome_vars = MAIN_OUTCOMES
)

# =============================================================================
# SECTION 5C: FULL-MODEL PERFORMANCE COMPARISON
# =============================================================================

cat("\n--- Full-Model Performance Comparison ---\n\n")

full_spec_results <- compute_full_spec_comparison(
  mods_cs = mods_cross_sectional,
  mods_long = mods_longitudinal,
  outcome_vars = MAIN_OUTCOMES
)
full_spec_cs <- full_spec_results$full_spec_cs
full_spec_long <- full_spec_results$full_spec_long

# =============================================================================
# SECTION 5D: ROBUSTNESS ANALYSIS (Main Outcomes Only)
# =============================================================================

cat("\n--- Robustness Analysis (Main Outcomes) ---\n\n")

# Cross-sectional robustness (with IPW for 1-month attrition)
cat("Cross-sectional robustness:\n")
robustness_cs <- run_robustness_analysis(
  data = data_cross_sectional,
  outcome_vars = MAIN_OUTCOMES,
  rs_variable = "lambda",
  specs_lookup = best_specs_cs,
  study_type = "cross_sectional",
  add_domain = TRUE,
  add_pre = TRUE,
  add_time = FALSE,
  use_weights = TRUE
)

# Longitudinal robustness (with IPW)
cat("\nLongitudinal robustness:\n")
robustness_long <- run_robustness_analysis(
  data = data_longitudinal,
  outcome_vars = MAIN_OUTCOMES,
  rs_variable = "lambda",
  specs_lookup = best_specs_long,
  study_type = "longitudinal",
  add_domain = TRUE,
  add_pre = TRUE,
  add_time = FALSE,
  use_weights = TRUE
)

# =============================================================================
# SECTION 6: ANCOVA - DOMAIN x STUDY INTERACTION
# =============================================================================

cat("\n--- ANCOVA: Domain x Study Interaction ---\n\n")

# Combine pooled data for ANCOVA (use outcome_measure directly)
data_combined_ancova <- bind_rows(
  data_cross_sectional_pooled,
  data_longitudinal_pooled
) %>%
  filter(outcome_measure %in% MAIN_OUTCOMES) %>%
  filter(!is.na(outcome_value) & !is.na(outcome_value_pre))

cat("Combined ANCOVA data:", nrow(data_combined_ancova), "rows\n")
cat("Outcomes:", paste(unique(data_combined_ancova$outcome_measure), collapse = ", "), "\n")

# Fit ANCOVA models for each outcome
ancova_models <- list()

for (outcome in MAIN_OUTCOMES) {
  cat(sprintf("\nFitting ANCOVA for %s...\n", outcome))

  outcome_data <- data_combined_ancova %>%
    filter(outcome_measure == outcome)

  ancova_models[[outcome]] <- lm(
    outcome_value ~ outcome_value_pre + domain * study_id,
    data = outcome_data
  )

  cat("Model summary:\n")
  print(summary(ancova_models[[outcome]]))

  # Extract ANOVA table for interaction effect
  cat("\nANOVA table:\n")
  print(anova(ancova_models[[outcome]]))
}

# =============================================================================
# SECTION 7: COMBINED STUDY MODEL
# =============================================================================

cat("\n--- Combined Study Model ---\n\n")

# Run combined study analysis for MAIN outcomes (factor scores)
combined_study_results <- list()

for (outcome in MAIN_OUTCOMES) {
  cat(sprintf("\n=== %s ===\n", outcome))

  combined_study_results[[outcome]] <- run_combined_study_analysis(
    data_cs = data_cross_sectional,
    data_long = data_longitudinal,
    outcome_var = outcome,
    model_family = "ols",
    add_pre = TRUE
  )
}

# Run combined study analysis for INDIVIDUAL scales
combined_study_individual <- list()

for (outcome in INDIVIDUAL_OUTCOMES) {
  cat(sprintf("\n=== %s ===\n", outcome))

  combined_study_individual[[outcome]] <- run_combined_study_analysis(
    data_cs = data_cross_sectional,
    data_long = data_longitudinal,
    outcome_var = outcome,
    model_family = "ols",
    add_pre = TRUE
  )
}

# =============================================================================
# SECTION 8: INDIVIDUAL SCALE ANALYSIS (Additional)
# =============================================================================

cat("\n--- Individual Scale Analysis (Additional) ---\n\n")

# Functional form comparison for individual scales
perf_individual_cs <- list()
perf_individual_long <- list()
best_specs_individual_cs <- data.frame()
best_specs_individual_long <- data.frame()

for (outcome in INDIVIDUAL_OUTCOMES) {
  cat(sprintf("\n=== %s ===\n", outcome))

  # Cross-sectional (pre-post design with baseline control)
  cat("\n  Cross-Sectional:\n")
  result_cs <- compare_functional_forms(
    data = data_cross_sectional,
    outcome_var = outcome,
    add_domain = TRUE,
    add_pre = TRUE,
    add_time = FALSE,
    model_family = "ols",
    select_by = "AIC"
  )
  perf_individual_cs[[outcome]] <- result_cs$comparison
  best_specs_individual_cs <- rbind(best_specs_individual_cs, data.frame(
    outcome = outcome,
    best_spec = result_cs$best_spec,
    stringsAsFactors = FALSE
  ))
  cat(sprintf("    Best: %s\n", result_cs$best_spec))

  # Longitudinal (pre-post design with baseline control)
  cat("\n  Longitudinal:\n")
  result_long <- compare_functional_forms(
    data = data_longitudinal,
    outcome_var = outcome,
    add_domain = TRUE,
    add_pre = TRUE,
    add_time = FALSE,
    model_family = "ols",
    select_by = "AIC"
  )
  perf_individual_long[[outcome]] <- result_long$comparison
  best_specs_individual_long <- rbind(best_specs_individual_long, data.frame(
    outcome = outcome,
    best_spec = result_long$best_spec,
    stringsAsFactors = FALSE
  ))
  cat(sprintf("    Best: %s\n", result_long$best_spec))
}

# Fit models for individual scales
mods_individual_cs <- list()
mods_individual_long <- list()

for (outcome in INDIVIDUAL_OUTCOMES) {
  cat("\n=== Fitting models for:", outcome, "===\n")

  best_spec_cs <- best_specs_individual_cs$best_spec[best_specs_individual_cs$outcome == outcome]
  best_spec_long <- best_specs_individual_long$best_spec[best_specs_individual_long$outcome == outcome]

  cat("  Using", best_spec_cs, "for cross-sectional,", best_spec_long, "for longitudinal\n")

  # Cross-sectional
  cat("\nCross-sectional:\n")
  mods_individual_cs[[outcome]] <- fit_models(
    data = data_cross_sectional,
    outcome_var = outcome,
    continuous_spec = best_spec_cs,
    add_domain = TRUE,
    add_pre = TRUE,
    add_time = FALSE,
    model_family = "ols"
  )

  # Longitudinal
  cat("\nLongitudinal:\n")
  mods_individual_long[[outcome]] <- fit_models(
    data = data_longitudinal,
    outcome_var = outcome,
    continuous_spec = best_spec_long,
    add_domain = TRUE,
    add_pre = TRUE,
    add_time = FALSE,
    model_family = "ols"
  )
}

# Full-spec comparison for individual scales
individual_full_spec_results <- compute_full_spec_comparison(
  mods_cs = mods_individual_cs,
  mods_long = mods_individual_long,
  outcome_vars = INDIVIDUAL_OUTCOMES
)

# Print individual scale coefficients
cat("\n--- Printing Model Coefficients (Individual Scales) ---\n")
print_model_coefficients_section(
  mods_cross_sectional = mods_individual_cs,
  mods_longitudinal = mods_individual_long,
  outcome_vars = INDIVIDUAL_OUTCOMES
)

# NOTE: No robustness checks for individual scales (these are already additional)

# =============================================================================
# SECTION 9: SAVE
# =============================================================================

cat("\n--- Saving Models and Data ---\n")

# Main outcome models
saveRDS(mods_cross_sectional,
        file.path(MODEL_DIR, "psychosocial_cross_sectional.rds"))
saveRDS(mods_longitudinal,
        file.path(MODEL_DIR, "psychosocial_longitudinal.rds"))

# Individual scale models
saveRDS(mods_individual_cs,
        file.path(MODEL_DIR, "psychosocial_individual_cross_sectional.rds"))
saveRDS(mods_individual_long,
        file.path(MODEL_DIR, "psychosocial_individual_longitudinal.rds"))

# ANCOVA models
saveRDS(ancova_models,
        file.path(MODEL_DIR, "psychosocial_ancova_domain_study.rds"))

# Combined study models
saveRDS(combined_study_results,
        file.path(MODEL_DIR, "psychosocial_combined_study.rds"))
saveRDS(combined_study_individual,
        file.path(MODEL_DIR, "psychosocial_individual_combined_study.rds"))

# Pooled data
psychosocial_data <- bind_rows(
  data_cross_sectional_pooled,
  data_longitudinal_pooled
)
saveRDS(psychosocial_data,
        file.path(MODEL_DIR, "psychosocial_data.rds"))

# Spec comparison data
spec_data <- list(
  best_specs_cs = best_specs_cs,
  best_specs_long = best_specs_long,
  full_spec_cs = full_spec_cs,
  full_spec_long = full_spec_long,
  best_specs_individual_cs = best_specs_individual_cs,
  best_specs_individual_long = best_specs_individual_long,
  individual_full_spec_cs = individual_full_spec_results$full_spec_cs,
  individual_full_spec_long = individual_full_spec_results$full_spec_long
)
saveRDS(spec_data,
        file.path(MODEL_DIR, "psychosocial_spec_data.rds"))

cat("  All models and data saved to:", MODEL_DIR, "\n")

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 10: GENERATE LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n")

  # -------------------------------------------------------------------------
  # Main Outcomes Tables
  # -------------------------------------------------------------------------

  cat("  Main outcomes functional form tables...\n")
  generate_functional_form_tables(
    perf_cs = perf_cross_sectional,
    perf_long = perf_longitudinal,
    table_prefix = "psychosocial",
    table_dir = TABLE_DIR,
    task_name = "Psychosocial Wellbeing"
  )

  cat("  Main outcomes regression tables...\n")
  generate_regression_tables(
    mods_cs = mods_cross_sectional,
    mods_long = mods_longitudinal,
    outcome_vars = MAIN_OUTCOMES,
    best_specs_cs = best_specs_cs,
    best_specs_long = best_specs_long,
    table_prefix = "psychosocial",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels(NULL)
  )

  cat("  Main outcomes full-spec comparison tables...\n")
  generate_full_spec_tables(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long,
    table_prefix = "psychosocial",
    table_dir = TABLE_DIR,
    task_name = "Psychosocial Wellbeing"
  )

  cat("  Main outcomes robustness tables...\n")
  create_robustness_latex_table(
    wide_df = robustness_cs$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "psychosocial_robustness_cs",
    caption = "Psychosocial Robustness",
    study_type = "cross_sectional"
  )
  create_robustness_latex_table(
    wide_df = robustness_long$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "psychosocial_robustness_long",
    caption = "Psychosocial Robustness",
    study_type = "longitudinal"
  )

  # -------------------------------------------------------------------------
  # ANCOVA Tables
  # -------------------------------------------------------------------------

  cat("  ANCOVA table (combined F1 and F2)...\n")
  sjplot_to_latex(
    models = list(
      ancova_models[["psychosocial_F1"]],
      ancova_models[["psychosocial_F2"]]
    ),
    model_labels = c("Emotional Health (F1)", "Social Health (F2)"),
    pred_labels = get_pred_labels(NULL),
    filename = "psychosocial_ancova",
    table_dir = TABLE_DIR,
    caption = "ANCOVA: Domain × Study",
    dependent_var = "Psychosocial Wellbeing"
  )

  # -------------------------------------------------------------------------
  # Combined Study Table
  # -------------------------------------------------------------------------

  cat("  Combined study table (F1 and F2)...\n")
  sjplot_to_latex(
    models = list(
      combined_study_results[["psychosocial_F1"]],
      combined_study_results[["psychosocial_F2"]]
    ),
    model_labels = c("Emotional Health (F1)", "Social Health (F2)"),
    pred_labels = c(
      "(Intercept)" = "Intercept",
      "study_typelongitudinal" = "Longitudinal (vs Cross-Sectional)"
    ),
    filename = "psychosocial_combined_study",
    table_dir = TABLE_DIR,
    caption = "Psychosocial Wellbeing by Study Type",
    dependent_var = "Psychosocial Wellbeing"
  )

  cat("  Combined study tables (individual scales)...\n")
  individual_labels <- c(
    "phq_gad_score" = "PHQ-GAD (Anxiety/Depression)",
    "ucla_score" = "UCLA (Loneliness)",
    "lubben_score" = "Lubben (Social Connectedness)",
    "who_score" = "WHO (Quality of Life)"
  )
  for (outcome in INDIVIDUAL_OUTCOMES) {
    outcome_label <- individual_labels[[outcome]]
    sjplot_to_latex(
      models = list(combined_study_individual[[outcome]]),
      model_labels = c("Study Type Effect"),
      pred_labels = c(
        "(Intercept)" = "Intercept",
        "study_typelongitudinal" = "Longitudinal (vs Cross-Sectional)"
      ),
      filename = paste0("psychosocial_individual_combined_study_", outcome),
      table_dir = TABLE_DIR,
      caption = paste0(outcome_label, " by Study Type"),
      dependent_var = outcome_label
    )
  }

  # -------------------------------------------------------------------------
  # Individual Scales Tables
  # -------------------------------------------------------------------------

  cat("  Individual scales regression tables...\n")
  generate_regression_tables(
    mods_cs = mods_individual_cs,
    mods_long = mods_individual_long,
    outcome_vars = INDIVIDUAL_OUTCOMES,
    best_specs_cs = best_specs_individual_cs,
    best_specs_long = best_specs_individual_long,
    table_prefix = "psychosocial_individual",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels(NULL)
  )

  cat("  Individual scales functional form tables...\n")
  generate_functional_form_tables(
    perf_cs = perf_individual_cs,
    perf_long = perf_individual_long,
    table_prefix = "psychosocial_individual",
    table_dir = TABLE_DIR,
    task_name = "Psychosocial Individual Scales"
  )

  cat("  Individual scales full-spec tables...\n")
  generate_full_spec_tables(
    full_spec_cs = individual_full_spec_results$full_spec_cs,
    full_spec_long = individual_full_spec_results$full_spec_long,
    table_prefix = "psychosocial_individual",
    table_dir = TABLE_DIR,
    task_name = "Psychosocial Individual Scales"
  )

  # -------------------------------------------------------------------------
  # Parent TeX Files
  # -------------------------------------------------------------------------

  cat("  Parent TeX files...\n")
  generate_parent_tex(
    section_name = "Psychosocial Wellbeing",
    constructs = MAIN_OUTCOMES,
    construct_labels = setNames(
      c("Psychosocial F1 (Emotional Health)",
        "Psychosocial F2 (Social Health)"),
      MAIN_OUTCOMES
    ),
    func_form_prefix = "psychosocial_functional_form",
    main_reg_prefix = "psychosocial",
    full_spec_prefix = "psychosocial_full_spec",
    robustness_prefix = "psychosocial_robustness",
    ancova_prefix = "psychosocial_ancova",
    combined_study_prefix = "psychosocial_combined_study",
    table_dir = TABLE_DIR,
    output_filename = "psychosocial_parent"
  )

  generate_parent_tex(
    section_name = "Psychosocial Individual Scales (Supplementary)",
    constructs = INDIVIDUAL_OUTCOMES,
    construct_labels = setNames(
      c("PHQ-GAD (Anxiety/Depression)", "UCLA (Loneliness)",
        "Lubben (Social Connectedness)", "WHO (Quality of Life)"),
      INDIVIDUAL_OUTCOMES
    ),
    func_form_prefix = "psychosocial_individual_functional_form",
    main_reg_prefix = "psychosocial_individual",
    full_spec_prefix = "psychosocial_individual_full_spec",
    table_dir = TABLE_DIR,
    output_filename = "psychosocial_individual_parent"
  )

  cat("\n  All LaTeX tables generated.\n")
}

# =============================================================================
# SECTION 10: OPTIONAL REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "09_psychosocial.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  # Header and overview
  lines <- c(
    "# Psychosocial Wellbeing Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines effects on psychosocial wellbeing outcomes.",
    "",
    "**Main Outcomes (Factor Scores):**",
    "- **Psychosocial F1**: Emotional health factor (from EFA)",
    "- **Psychosocial F2**: Social health factor (from EFA)",
    "",
    "**Individual Scales (Supplementary):**",
    "- **PHQ-GAD**: Combined anxiety and depression",
    "- **UCLA**: Loneliness scale",
    "- **Lubben**: Social network scale",
    "- **WHO-5**: Quality of life/wellbeing",
    "",
    "**Model Specification:**",
    "- OLS regression with pre-treatment covariate control",
    "- Domain effects included",
    "- Pre-post design (measured at beginning and end of month)",
    "",
    "---",
    ""
  )

  # Data summary
  lines <- c(lines,
    generate_data_summary_md(data_cross_sectional_pooled, data_longitudinal_pooled),
    "",
    "---",
    "",
    "## Exploratory Data Analysis",
    "",
    paste0("![Outcome Distributions](", fig_rel_path, "/psychosocial_distributions.png)"),
    "",
    paste0("![Outcome Correlations](", fig_rel_path, "/psychosocial_correlation_heatmap.png)"),
    "",
    paste0("![Pre-Post Change](", fig_rel_path, "/psychosocial_pre_post_combined.png)"),
    "",
    paste0("![Pre-Post Correlations](", fig_rel_path, "/psychosocial_pre_post_corr.png)"),
    "",
    paste0("![Outcomes by Lambda](", fig_rel_path, "/psychosocial_by_lambda.png)"),
    "",
    "---",
    ""
  )

  # Functional form comparison
  if (exists("best_specs_cs") && nrow(best_specs_cs) > 0) {
    lines <- c(lines, generate_best_specs_md(best_specs_cs, best_specs_long))
  }

  # Full-model specification comparison
  if (exists("full_spec_cs") && length(full_spec_cs) > 0) {
    lines <- c(lines, generate_full_spec_comparison_md(
      full_spec_cs, full_spec_long, MAIN_OUTCOMES))
  }

  # Model coefficients
  if (exists("mods_cross_sectional") && exists("mods_longitudinal")) {
    cat("  Adding model coefficients...\n")
    lines <- c(lines, generate_coefficients_md(
      mods_cross_sectional, mods_longitudinal, MAIN_OUTCOMES), "", "---", "")
  }

  # Robustness summary
  if (exists("robustness_cs") && exists("robustness_long")) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      robustness_cs, robustness_long, MAIN_OUTCOMES, "psychosocial"))
  }

  # ANCOVA: Domain x Study interaction
  if (exists("ancova_models") && length(ancova_models) > 0) {
    cat("  Adding ANCOVA models...\n")
    lines <- c(lines,
      "## ANCOVA: Domain × Study Interaction",
      "",
      "Tests whether the effect of domain (EmotChat vs PolChat) differs",
      "between cross-sectional and longitudinal studies, controlling for",
      "pre-treatment scores.",
      ""
    )
    for (construct_name in names(ancova_models)) {
      lines <- c(lines,
        paste0("### ", format_outcome_label(construct_name)),
        ""
      )
      lines <- c(lines, format_coefficients_md(
        ancova_models[[construct_name]],
        construct_name,
        "Combined Studies"
      ))
    }
    lines <- c(lines, "---", "")
  }

  # Combined study results
  if (exists("combined_study_results") && length(combined_study_results) > 0) {
    cat("  Adding combined study results...\n")
    lines <- c(lines,
      "## Combined Study Model",
      "",
      "Tests whether psychosocial outcomes differ between single-session (cross-sectional)",
      "and month-long (longitudinal) AI exposure.",
      ""
    )
    for (outcome in names(combined_study_results)) {
      lines <- c(lines,
        paste0("### ", format_outcome_label(outcome)),
        ""
      )
      lines <- c(lines, format_coefficients_md(
        combined_study_results[[outcome]],
        outcome,
        "Combined Studies"
      ))
    }
    lines <- c(lines, "---", "")
  }

  # Output files
  lines <- c(lines, generate_output_files_md("psychosocial"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
