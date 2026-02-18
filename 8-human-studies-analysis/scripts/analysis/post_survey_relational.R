#!/usr/bin/env Rscript
# =============================================================================
# Post-Survey Relational Measures Analysis
# =============================================================================
#
# Analyzes final (post-survey) relational measures from the longitudinal study:
#   - Categorical: relational_change_binary → relational_change_recoded
#   - Continuous: will_miss_ai, distant_close_change, tool_friend_change
#
# Key characteristics:
#   - EDA and treatment tests for all variables
#   - Regression analysis for will_miss_ai (for pre-registration hypothesis tests)
#   - Longitudinal study only (data is longitudinal-only)
#   - OLS regression (continuous outcome, single time point)
#   - Domain effects included (add_domain = TRUE)
#   - No time effects (add_time = FALSE)
#
# Usage:
#   Rscript scripts/analysis/post_survey_relational.R
#   Rscript scripts/analysis/post_survey_relational.R --generate_tex_tables
#   Rscript scripts/analysis/post_survey_relational.R --generate_report
#
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(knitr)
library(pheatmap)
library(RColorBrewer)
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
cat("Post-Survey Relational Measures Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variables
CATEGORICAL_VARS <- c("relational_change_recoded")
CONTINUOUS_VARS <- c("will_miss_ai", "distant_close_change", "tool_friend_change")

# =============================================================================
# SECTION 2: DATA LOADING
# =============================================================================

cat("\n--- Loading Data ---\n\n")

# Load final relational measures data
relational_path <- file.path(DATA_DIR, "final-relational-measures.jsonl")
if (!file.exists(relational_path)) {
  stop("final-relational-measures.jsonl not found. Run 7-folder processing first.")
}

data_raw <- load_jsonl(relational_path)
cat("Loaded", nrow(data_raw), "raw observations\n")

# Prepare treatment arms
data_merged <- prepare_treatment_arms(data_raw)

# Filter to longitudinal only (data is longitudinal-only anyway)
data_stats <- data_merged %>%
  filter(study_id == "longitudinal")

cat("After filtering to longitudinal:", nrow(data_stats), "observations\n")
cat("Participants:", n_distinct(data_stats$ppt_id), "\n")

# Load participant characteristics
ppt_chars <- load_ppt_characteristics(
  DATA_DIR, GENERATED_DIR,
  include_psychosocial = TRUE,
  apply_coarsening = TRUE
)

# Merge participant characteristics
data_stats <- data_stats %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))

cat("Merged participant characteristics\n")

# =============================================================================
# SECTION 3: PROCESS CATEGORICAL VARIABLE
# =============================================================================

cat("\n--- Processing Categorical Variables ---\n\n")

# Recode relational_change_binary → relational_change_recoded
data_stats <- data_stats %>%
  mutate(
    relational_change_recoded = case_when(
      relational_change_binary == 0 ~ "Relationship unchanged",
      relational_change_binary == 1 ~ "Relationship changed",
      TRUE ~ NA_character_
    ),
    relational_change_recoded = factor(
      relational_change_recoded,
      levels = c("Relationship unchanged", "Relationship changed")
    )
  )

cat("Relational change recoded distribution:\n")
print(table(data_stats$relational_change_recoded, useNA = "ifany"))

# =============================================================================
# SECTION 4A: EDA - CATEGORICAL VARIABLE
# =============================================================================

cat("\n--- EDA: Categorical Variable ---\n\n")

# --- Plot: Relational Change by RS Category ---
cat("Creating relational change bar plot...\n")

rs_colors <- c(
  "pos_lambda" = "#b40426",
  "zero_lambda" = "#918f8f",
  "neg_lambda" = "#3b4cc0"
)
rs_labels <- c(
  "pos_lambda" = "pos lambda",
  "zero_lambda" = "zero lambda",
  "neg_lambda" = "neg lambda"
)

data_rs <- data_stats %>%
  filter(relationship_seeking_category %in% c("pos_lambda", "zero_lambda", "neg_lambda")) %>%
  mutate(relationship_seeking_category = factor(
    relationship_seeking_category,
    levels = c("pos_lambda", "zero_lambda", "neg_lambda")
  ))

p_change_by_rs <- plot_categorical_by_group(
  data = data_rs,
  response_var = "relational_change_recoded",
  group_var = "relationship_seeking_category",
  response_levels = c("Relationship unchanged", "Relationship changed"),
  title = "Post-Survey Relational Change\n(by Lambda Category)",
  subtitle = "Longitudinal study only",
  colors = rs_colors,
  labels = rs_labels
) +
  labs(x = "Percentage of Participants") +
  theme(plot.title = element_text(size = rel(1.0)))

save_plot(FIGURE_DIR, p_change_by_rs, "post_survey_relational_change", 10, 6)
cat("Saved: post_survey_relational_change.pdf/png\n")

# --- Treatment Tests for Categorical Variable ---
cat("\nRunning treatment tests for categorical variable...\n")

# Run standard treatment tests (longitudinal only)
# Since data is longitudinal-only, we just run tests within longitudinal
categorical_test_results <- run_treatment_tests(
  data = data_stats,
  outcome_vars = CATEGORICAL_VARS,
  treatment_vars = c("personalisation", "domain", "relationship_seeking_category", "lambda_factor")
)

# Save categorical test results
write_json(categorical_test_results, file.path(STATS_DIR, "post_survey_relational_categorical_tests.json"), pretty = TRUE)
cat("\nSaved: post_survey_relational_categorical_tests.json\n")

# =============================================================================
# SECTION 4B: EDA - CONTINUOUS VARIABLES
# =============================================================================

cat("\n--- EDA: Continuous Variables ---\n\n")

# --- Boxplots by study (longitudinal only, so just overall distributions) ---
cat("Creating distribution boxplots...\n")

# Create a simple boxplot for the continuous variables
box_data <- data_stats %>%
  select(all_of(CONTINUOUS_VARS)) %>%
  pivot_longer(
    cols = all_of(CONTINUOUS_VARS),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(variable_clean = clean_var_vector(variable, add_newlines = TRUE))

box_plot <- ggplot(box_data, aes(x = variable_clean, y = value)) +
  geom_boxplot(fill = STUDY_COLORS$study_id["longitudinal"], alpha = 0.7) +
  labs(x = "", y = "Rating", title = "Post-Survey Relational Measures") +
  theme_pub()

save_plot(FIGURE_DIR, box_plot, "post_survey_relational_distributions", 12, 6)
cat("Saved: post_survey_relational_distributions.pdf/png\n")

# --- Correlation Heatmap (longitudinal only) ---
cat("Creating correlation heatmap...\n")

corr_plot <- plot_corr_heatmaps_combined(
  data_cs = NULL,
  data_long = data_stats,
  outcome_vars = CONTINUOUS_VARS
)
save_plot(FIGURE_DIR, corr_plot, "post_survey_relational_correlation", 8, 7)

# =============================================================================
# SECTION 4C: TWO-SAMPLE T-TESTS (Between treatment groups)
# =============================================================================

cat("\n--- Two-Sample T-Tests: Between Treatment Groups ---\n")
cat("Comparing outcomes between treatment arms (independent samples).\n\n")

# Uses run_two_sample_ttest from eda_utils.R

# Run two-sample t-tests for all combinations
outcome_cols <- c("distant_close_change", "tool_friend_change", "will_miss_ai")
treatment_cols <- c("personalisation", "domain", "relationship_seeking_category")

two_sample_results <- list()
for (outcome in outcome_cols) {
  for (treatment in treatment_cols) {
    result <- run_two_sample_ttest(data_stats, outcome, treatment, "longitudinal")
    if (!is.null(result)) {
      key <- paste(outcome, treatment, sep = "_")
      two_sample_results[[key]] <- result
    }
  }
}

# Convert results to data frame
if (length(two_sample_results) > 0) {
  two_sample_df <- bind_rows(two_sample_results)

  cat("\n\nTwo-Sample T-Test Results Summary:\n")
  print(kable(two_sample_df %>% select(outcome, treatment, group1, group2,
                                   mean1, mean2, t_stat, df, p_value),
              digits = 3))

  # Save two-sample t-test results
  write_json(two_sample_df, file.path(STATS_DIR, "post_survey_relational_two_sample_ttest.json"), pretty = TRUE)
  cat("\nSaved: post_survey_relational_two_sample_ttest.json\n")
} else {
  two_sample_df <- NULL
  cat("\nNo two-sample t-test results to save\n")
}

# =============================================================================
# SECTION 5: WILL_MISS_AI REGRESSION ANALYSIS
# =============================================================================
#
# Fit regression models for will_miss_ai to enable pre-registration hypothesis
# tests. This follows the same pattern as goodbye.R.
#
# Model type: OLS (continuous outcome)
# No time effects (single time point)
# Has domain (emotchat vs polchat)
# Longitudinal only (no cross-sectional data for this measure)
#
# =============================================================================

cat("\n--- will_miss_ai Regression Analysis ---\n\n")

OUTCOME_VAR <- "will_miss_ai"

# Functional form comparison (longitudinal only)
cat("Comparing functional forms for will_miss_ai...\n")
func_form_result_long <- compare_functional_forms(
  data = data_stats,
  outcome_var = OUTCOME_VAR,
  add_domain = TRUE,
  add_time = FALSE,
  model_family = "ols",
  select_by = "AIC"
)
best_spec_long <- func_form_result_long$best_spec
cat(sprintf("  Best specification: %s\n", best_spec_long))
print(func_form_result_long$comparison)

# Create best specs data frame for downstream use
best_specs_long <- data.frame(
  outcome = OUTCOME_VAR,
  best_spec = best_spec_long,
  stringsAsFactors = FALSE
)

# =============================================================================
# SECTION 5A: FIT MODELS
# =============================================================================

cat("\n--- Fitting Models ---\n\n")

# Longitudinal (OLS)
cat("Longitudinal:\n")
mods_longitudinal <- list()
mods_longitudinal[[OUTCOME_VAR]] <- fit_models(
  data = data_stats,
  outcome_var = OUTCOME_VAR,
  continuous_spec = best_spec_long,
  add_domain = TRUE,
  add_pre = FALSE,
  add_time = FALSE,
  model_family = "ols"
)

cat("\n--- Printing Model Coefficients ---\n")

# For longitudinal-only analysis, print coefficients directly
cat("\n=== will_miss_ai (Longitudinal Only) ===\n")
if (!is.null(mods_longitudinal[[OUTCOME_VAR]]$full_continuous)) {
  print(summary(mods_longitudinal[[OUTCOME_VAR]]$full_continuous))
}

# =============================================================================
# SECTION 5B: FULL-MODEL PERFORMANCE COMPARISON
# =============================================================================

cat("\n--- Full-Model Performance Comparison ---\n\n")

# Compute for longitudinal only (create empty cs for compatibility)
mods_cross_sectional <- list()
full_spec_results <- compute_full_spec_comparison(
  mods_cross_sectional, mods_longitudinal, OUTCOME_VAR
)

full_spec_cs <- full_spec_results$full_spec_cs
full_spec_long <- full_spec_results$full_spec_long

# =============================================================================
# SECTION 5C: ROBUSTNESS ANALYSIS
# =============================================================================

cat("\n--- Robustness Analysis ---\n\n")

# Longitudinal robustness only
cat("Longitudinal robustness:\n")
robustness_long <- run_robustness_analysis(
  data = data_stats,
  outcome_vars = OUTCOME_VAR,
  rs_variable = "lambda",
  specs_lookup = best_specs_long,
  study_type = "longitudinal",
  add_domain = TRUE,
  add_time = FALSE,
  use_weights = TRUE,
  model_family = "ols"
)

# =============================================================================
# SECTION 6: SAVE MODELS AND DATA
# =============================================================================

cat("\n--- Saving Models and Data ---\n")

saveRDS(mods_longitudinal, file.path(MODEL_DIR, "post_survey_relational_long.rds"))
cat("  Saved: post_survey_relational_long.rds\n")

# Save pooled data with construct column for compute_contrasts.R
pooled_data <- create_pooled_data(data_stats, OUTCOME_VAR)
saveRDS(pooled_data, file.path(MODEL_DIR, "post_survey_relational_data.rds"))
cat("  Saved: post_survey_relational_data.rds\n")

# Save spec comparison data
spec_data <- list(
  best_specs_long = best_specs_long,
  full_spec_long = full_spec_long
)
saveRDS(spec_data, file.path(MODEL_DIR, "post_survey_relational_spec_data.rds"))
cat("  Saved: post_survey_relational_spec_data.rds\n")

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 7: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  # Functional form tables (longitudinal only)
  generate_functional_form_tables(
    perf_cs = list(),
    perf_long = list(will_miss_ai = func_form_result_long$comparison),
    table_prefix = "post_survey_relational",
    table_dir = TABLE_DIR,
    task_name = "Post-Survey Relational"
  )

  # Regression tables (longitudinal only)
  generate_regression_tables(
    mods_cs = list(),
    mods_long = mods_longitudinal,
    outcome_vars = OUTCOME_VAR,
    best_specs_cs = data.frame(outcome = character(), best_spec = character()),
    best_specs_long = best_specs_long,
    table_prefix = "post_survey_relational",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels(NULL)
  )

  # Full spec comparison tables
  generate_full_spec_tables(
    full_spec_cs = list(),
    full_spec_long = full_spec_long,
    table_prefix = "post_survey_relational",
    table_dir = TABLE_DIR,
    task_name = "Post-Survey Relational"
  )

  # Robustness tables (longitudinal only)
  if (!is.null(robustness_long) && nrow(robustness_long$coeffs_wide) > 0) {
    create_robustness_latex_table(
      wide_df = robustness_long$coeffs_wide,
      table_dir = TABLE_DIR,
      filename_prefix = "post_survey_relational_robustness_long",
      caption = "Post-Survey Relational Robustness",
      study_type = "longitudinal"
    )
  }

  # Two-sample t-test table (keep existing)
  if (!is.null(two_sample_df) && nrow(two_sample_df) > 0) {
    create_ttest_latex(
      results_df = two_sample_df,
      test_type = "two_sample",
      table_dir = TABLE_DIR,
      filename = "post_survey_relational_ttest",
      caption = "Post-Survey Relational Measures: Two-Sample T-Tests",
      label = "tab:post_survey_relational_ttest"
    )
  }

  # Chi-square test table with breakdown for significant results
  # Wrap in longitudinal structure for create_chisq_with_breakdown_latex
  if (exists("categorical_test_results") && length(categorical_test_results) > 0) {
    create_chisq_with_breakdown_latex(
      test_results = list(longitudinal = categorical_test_results),
      table_dir = TABLE_DIR,
      filename = "post_survey_relational_chisq",
      caption = "Post-Survey Relational: Chi-Square Tests",
      label = "tab:post_survey_relational_chisq"
    )
  }

  # Parent TeX file
  generate_parent_tex(
    section_name = "Post-Survey Relational",
    constructs = OUTCOME_VAR,
    construct_labels = setNames("Will Miss AI", OUTCOME_VAR),
    func_form_prefix = "post_survey_relational_functional_form",
    main_reg_prefix = "post_survey_relational",
    full_spec_prefix = "post_survey_relational_full_spec",
    robustness_prefix = "post_survey_relational_robustness",
    table_dir = TABLE_DIR,
    output_filename = "post_survey_relational_parent"
  )
  cat("  Saved: post_survey_relational_parent.tex\n")
}

# =============================================================================
# SECTION 8: OPTIONAL REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "13_post_survey_relational.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  # Header and overview
  lines <- c(
    "# Post-Survey Relational Measures Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines post-survey relational measures from the",
    "**longitudinal study only**. These are final measures collected",
    "after the intervention period.",
    "",
    "**Categorical Outcomes:**",
    "- **Relational Change**: Whether participants felt their relationship",
    "  with the AI changed (binary: unchanged vs changed)",
    "",
    "**Continuous Outcomes (EDA only):**",
    "- **Distant-Close Change**: Change in perceived closeness",
    "- **Tool-Friend Change**: Change in perception from tool to friend",
    "",
    "**Regression Outcome:**",
    "- **Will Miss AI**: How much participants will miss the AI",
    "",
    "**Model Type:** OLS regression (continuous outcome, longitudinal only)",
    "",
    "---",
    "",
    "## Data Summary",
    "",
    paste0("- **Longitudinal**: ", nrow(data_stats), " participants"),
    "",
    "---",
    "",
    "## Categorical Variable: Relational Change",
    "",
    paste0("![Relational Change by RS Category](", fig_rel_path,
           "/post_survey_relational_change.png)"),
    "",
    "### Chi-Square Tests",
    "",
    "Tests performed against: personalisation, domain, relationship_seeking_category, lambda_factor",
    ""
  )

  # Add categorical test results using utility function
  # Wrap in longitudinal structure since this is longitudinal-only data
  wrapped_results <- list(longitudinal = categorical_test_results)
  data_by_study <- list(longitudinal = data_stats)
  lines <- c(lines, format_treatment_tests_md(wrapped_results, data_by_study))

  lines <- c(lines,
    "---",
    "",
    "## Continuous Variables",
    "",
    "### Distributions",
    "",
    paste0("![Distributions](", fig_rel_path,
           "/post_survey_relational_distributions.png)"),
    "",
    "### Correlation Heatmap",
    "",
    paste0("![Correlations](", fig_rel_path,
           "/post_survey_relational_correlation.png)"),
    "",
    "---",
    "",
    "## Two-Sample T-Tests (Between Treatment Groups)",
    "",
    "Independent samples t-tests comparing outcomes between treatment arms.",
    "For relationship_seeking_category, tests compare pos_lambda vs neg_lambda.",
    ""
  )

  # Add two-sample t-test results table
  if (!is.null(two_sample_df) && nrow(two_sample_df) > 0) {
    lines <- c(lines,
      "| Outcome | Treatment | Group 1 | Group 2 | Mean 1 | Mean 2 | t | df | p |",
      "|---------|-----------|---------|---------|--------|--------|---|----|----|"
    )

    for (i in seq_len(nrow(two_sample_df))) {
      row <- two_sample_df[i, ]
      sig_star <- if (row$p_value < 0.001) "***" else
                  if (row$p_value < 0.01) "**" else
                  if (row$p_value < 0.05) "*" else ""
      lines <- c(lines, sprintf(
        "| %s | %s | %s | %s | %.2f | %.2f | %.2f | %.1f | %.4f%s |",
        row$outcome, row$treatment, row$group1, row$group2,
        row$mean1, row$mean2, row$t_stat, row$df, row$p_value, sig_star
      ))
    }
    lines <- c(lines, "", "*p < .05, **p < .01, ***p < .001", "")
  } else {
    lines <- c(lines, "*No two-sample t-test results available*", "")
  }

  # Regression Analysis Section
  lines <- c(lines,
    "---",
    "",
    "## Regression Analysis: Will Miss AI",
    "",
    "OLS regression for `will_miss_ai` (longitudinal study only).",
    ""
  )

  # Functional form comparison (longitudinal only)
  lines <- c(lines,
    "### Functional Form Comparison",
    "",
    sprintf("Best specification (longitudinal): **%s**", best_spec_long),
    "",
    "---",
    ""
  )

  # Model coefficients (longitudinal only)
  if (exists("mods_longitudinal") && !is.null(mods_longitudinal[[OUTCOME_VAR]])) {
    cat("  Adding model coefficients...\n")
    lines <- c(lines,
      "### Model Coefficients (Longitudinal)",
      ""
    )
    lines <- c(lines, format_coefficients_md(
      mods_longitudinal[[OUTCOME_VAR]]$full_continuous,
      OUTCOME_VAR,
      "Longitudinal"
    ))
    lines <- c(lines, "---", "")
  }

  # Robustness summary (longitudinal only, pass NULL for cross-sectional)
  if (exists("robustness_long") && !is.null(robustness_long)) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      NULL, robustness_long, OUTCOME_VAR, "post_survey_relational"))
  }

  # Output files
  lines <- c(lines, generate_output_files_md("post_survey_relational"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
