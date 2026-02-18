#!/usr/bin/env Rscript
# =============================================================================
# Domain Competency Analysis (EDA Only)
# =============================================================================
#
# Analyzes perceived domain competency measures:
#   - emotchat_effectiveness, emotchat_satis → emotional_competency (composite)
#   - polchat_confidence, polchat_knowledge → political_competency (composite)
#
# Key characteristics:
#   - EDA and pre-post change tests ONLY - no regressions
#   - Creates composite measures from raw items
#   - T-tests on change scores (post - pre)
#
# Usage:
#   Rscript scripts/analysis/domain_competency.R
#   Rscript scripts/analysis/domain_competency.R --generate_tex_tables
#   Rscript scripts/analysis/domain_competency.R --generate_report
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
REPORT_DIR <- paths$REPORT_DIR
DATA_DIR <- paths$DATA_DIR
GENERATED_DIR <- paths$GENERATED_DIR

dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)

# Source utilities
source(file.path(PROJECT_ROOT, "scripts/utils_r/data_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/coarsen_and_factor_sociodemos.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_config.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/plot_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/eda_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/report_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Domain Competency Analysis (EDA Only)\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variables
RAW_OUTCOME_VARS <- c(
  "emotchat_effectiveness", "emotchat_satis",
  "polchat_confidence", "polchat_knowledge"
)
COMPOSITE_OUTCOME_VARS <- c("emotional_competency", "political_competency")

# =============================================================================
# SECTION 2: DATA LOADING
# =============================================================================

cat("\n--- Loading Data ---\n\n")

# Load domain competency data
competency_path <- file.path(DATA_DIR, "domain-competency.jsonl")
if (!file.exists(competency_path)) {
  stop("domain-competency.jsonl not found. Run format_main_study_data.py first.")
}

data_raw <- load_jsonl(competency_path)
cat("Loaded", nrow(data_raw), "raw observations\n")

# Prepare treatment arms
data_merged <- prepare_treatment_arms(data_raw)

# Filter to main studies only
data_stats <- data_merged %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

cat("After filtering to main studies:", nrow(data_stats), "observations\n")
cat("Studies:", paste(unique(data_stats$study_id), collapse = ", "), "\n")
cat("Timepoints:", paste(unique(data_stats$timepoint), collapse = ", "), "\n")

# Split by study type
data_cross_sectional <- data_stats %>% filter(study_id == "cross-sectional")
data_longitudinal <- data_stats %>% filter(study_id == "longitudinal")

cat("\n  Cross-sectional:", nrow(data_cross_sectional), "obs from",
    n_distinct(data_cross_sectional$ppt_id), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "obs from",
    n_distinct(data_longitudinal$ppt_id), "participants\n")

# =============================================================================
# SECTION 3: EDA - RAW VARIABLES CORRELATION
# =============================================================================

cat("\n--- EDA: Raw Variable Correlations ---\n\n")

# Filter to pre-treatment data for correlations (baseline relationships)
data_cs_pre <- data_cross_sectional %>%
  filter(!is.na(emotchat_effectiveness))
data_long_pre <- data_longitudinal %>%
  filter(timepoint == "pre") %>%
  filter(!is.na(emotchat_effectiveness))

cat("Pre-treatment observations:\n")
cat("  Cross-sectional:", nrow(data_cs_pre), "\n")
cat("  Longitudinal:", nrow(data_long_pre), "\n")

# Correlation heatmap for raw variables
cat("\nCreating raw variables correlation heatmap...\n")

raw_corr_plot <- plot_corr_heatmaps_combined(
  data_cs = data_cs_pre,
  data_long = data_long_pre,
  outcome_vars = RAW_OUTCOME_VARS
)
save_plot(FIGURE_DIR, raw_corr_plot, "domain_competency_raw_correlation", 14, 8)

# =============================================================================
# SECTION 4: CREATE COMPOSITE MEASURES
# =============================================================================

cat("\n--- Creating Composite Measures ---\n\n")

# Create composites for all data
data_stats <- data_stats %>%
  mutate(
    emotional_competency = rowMeans(
      select(., emotchat_satis, emotchat_effectiveness),
      na.rm = TRUE
    ),
    political_competency = rowMeans(
      select(., polchat_knowledge, polchat_confidence),
      na.rm = TRUE
    )
  )

# Update split datasets
data_cross_sectional <- data_stats %>% filter(study_id == "cross-sectional")
data_longitudinal <- data_stats %>% filter(study_id == "longitudinal")

cat("Composite measures created:\n")
cat("  emotional_competency = mean(emotchat_satis, emotchat_effectiveness)\n")
cat("  political_competency = mean(polchat_knowledge, polchat_confidence)\n")

# Summary stats for composites
cat("\nComposite summary (post-treatment, longitudinal):\n")
data_long_post_comp <- data_longitudinal %>%
  filter(timepoint == "post")

for (var in COMPOSITE_OUTCOME_VARS) {
  cat(sprintf("  %s: mean = %.2f, sd = %.2f, n = %d\n",
              var,
              mean(data_long_post_comp[[var]], na.rm = TRUE),
              sd(data_long_post_comp[[var]], na.rm = TRUE),
              sum(!is.na(data_long_post_comp[[var]]))))
}

# =============================================================================
# SECTION 5: EDA - COMPOSITE CORRELATION
# =============================================================================

cat("\n--- EDA: Composite Variable Correlations ---\n\n")

# Prepare data for composite correlation (pre-treatment, split by study)
data_cs_pre_comp <- data_cross_sectional %>%
  filter(!is.na(emotional_competency))
data_long_pre_comp <- data_longitudinal %>%
  filter(timepoint == "pre") %>%
  filter(!is.na(emotional_competency))

# Correlation heatmap for composite variables
comp_corr_plot <- plot_corr_heatmaps_combined(
  data_cs = data_cs_pre_comp,
  data_long = data_long_pre_comp,
  outcome_vars = COMPOSITE_OUTCOME_VARS
)
save_plot(FIGURE_DIR, comp_corr_plot, "domain_competency_composite_correlation", 14, 8)

# =============================================================================
# SECTION 6: CREATE PRE-POST WIDE FORMAT
# =============================================================================

cat("\n--- Creating Pre-Post Wide Format ---\n\n")

# Create wide format with measure, measure_pre, measure_diff
create_pre_post_wide <- function(data, outcome_vars) {
  # Get pre and post data
  pre_data <- data %>%
    filter(timepoint == "pre") %>%
    select(ppt_id, study_id, multiplier, domain, personalisation,
           all_of(outcome_vars))

  post_data <- data %>%
    filter(timepoint == "post") %>%
    select(ppt_id, study_id, multiplier, domain, personalisation,
           all_of(outcome_vars))

  # Rename pre columns
  pre_renamed <- pre_data
  for (var in outcome_vars) {
    pre_renamed <- pre_renamed %>%
      rename(!!paste0(var, "_pre") := !!sym(var))
  }

  # Merge pre and post
  wide_data <- post_data %>%
    left_join(
      pre_renamed %>% select(ppt_id, ends_with("_pre")),
      by = "ppt_id"
    )

  # Calculate diff columns
  for (var in outcome_vars) {
    pre_col <- paste0(var, "_pre")
    diff_col <- paste0(var, "_diff")
    wide_data <- wide_data %>%
      mutate(!!diff_col := !!sym(var) - !!sym(pre_col))
  }

  return(wide_data)
}

# Create wide format for composites
data_wide <- create_pre_post_wide(data_stats, COMPOSITE_OUTCOME_VARS)
data_wide_long <- data_wide %>% filter(study_id == "longitudinal")
data_wide_cs <- data_wide %>% filter(study_id == "cross-sectional")

cat("Wide format created:\n")
cat("  All:", nrow(data_wide), "participants\n")
cat("  Longitudinal:", nrow(data_wide_long), "participants\n")
cat("  Cross-sectional:", nrow(data_wide_cs), "participants\n")

# Also create wide format for raw variables
data_wide_raw <- create_pre_post_wide(data_stats, RAW_OUTCOME_VARS)
data_wide_raw_long <- data_wide_raw %>% filter(study_id == "longitudinal")

cat("\nWide format columns (composites):\n")
cat("  ", paste(names(data_wide)[grepl("competency", names(data_wide))], collapse = ", "), "\n")

# =============================================================================
# SECTION 7: PRE-POST CHANGE PLOTS - BY STUDY
# =============================================================================

cat("\n--- Pre-Post Change Plots (by Study) ---\n\n")

# Plot pre-post change by study_id
p_change_study <- plot_pre_post_change_combined(
  data = data_wide,
  outcome_vars = COMPOSITE_OUTCOME_VARS,
  facet_var = "study_id",
  total_height = 10,
  total_width = 14
)
save_plot(FIGURE_DIR, p_change_study, "domain_competency_pre_post_by_study", 14, 10)
cat("Saved: domain_competency_pre_post_by_study.pdf/png\n")

# =============================================================================
# SECTION 8: PRE-POST CHANGE PLOTS - BY DOMAIN
# =============================================================================

cat("\n--- Pre-Post Change Plots (by Domain) ---\n\n")

# Plot pre-post change by domain (longitudinal only)
p_change_domain <- plot_pre_post_change_combined(
  data = data_wide_long,
  outcome_vars = COMPOSITE_OUTCOME_VARS,
  facet_var = "domain",
  total_height = 10,
  total_width = 10
)
save_plot(FIGURE_DIR, p_change_domain, "domain_competency_pre_post_by_domain", 10, 10)
cat("Saved: domain_competency_pre_post_by_domain.pdf/png\n")

# =============================================================================
# SECTION 9: ONE-SAMPLE T-TESTS (Change differs from zero)
# =============================================================================

cat("\n--- One-Sample T-Tests: Change Scores vs Zero ---\n")
cat("Testing whether pre-post change differs significantly from 0.\n\n")

#' Run one-sample t-test testing if change differs from zero
#'
#' @param data Data frame with _diff columns
#' @param outcome_var Base outcome variable name (without _diff suffix)
#' @param study_name Label for study/subset
#' @return List with test results or NULL
run_one_sample_ttest_vs_zero <- function(data, outcome_var, study_name = "All") {
  diff_col <- paste0(outcome_var, "_diff")

  if (!diff_col %in% names(data)) {
    cat(sprintf("  %s: Column %s not found\n", outcome_var, diff_col))
    return(NULL)
  }

  change_values <- data[[diff_col]]
  change_values <- change_values[!is.na(change_values)]

  if (length(change_values) < 5) {
    cat(sprintf("  %s: Not enough data (n = %d)\n", outcome_var, length(change_values)))
    return(NULL)
  }

  t_result <- t.test(change_values, mu = 0)

  cat(sprintf("\n%s - %s:\n", study_name, outcome_var))
  cat(sprintf("  Mean change: %.2f (SD = %.2f)\n",
              mean(change_values), sd(change_values)))
  cat(sprintf("  One-sample t(%d) = %.3f, p = %.4f\n",
              as.integer(t_result$parameter),
              t_result$statistic,
              t_result$p.value))
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n",
              t_result$conf.int[1], t_result$conf.int[2]))

  sig <- if (t_result$p.value < 0.001) "***" else
         if (t_result$p.value < 0.01) "**" else
         if (t_result$p.value < 0.05) "*" else ""

  if (sig != "") {
    direction <- if (mean(change_values) > 0) "INCREASED" else "DECREASED"
    cat(sprintf("  SIGNIFICANT %s: Competency %s from pre to post\n", sig, direction))
  }

  return(list(
    study = study_name,
    outcome = outcome_var,
    n = length(change_values),
    mean_change = mean(change_values),
    sd_change = sd(change_values),
    t_stat = as.numeric(t_result$statistic),
    df = as.numeric(t_result$parameter),
    p_value = t_result$p.value,
    ci_lower = t_result$conf.int[1],
    ci_upper = t_result$conf.int[2]
  ))
}

# Run one-sample t-tests for composite measures
cat("=== Composite Measures ===\n")

one_sample_results <- list()

# Longitudinal
for (var in COMPOSITE_OUTCOME_VARS) {
  result <- run_one_sample_ttest_vs_zero(data_wide_long, var, "Longitudinal")
  if (!is.null(result)) {
    one_sample_results[[paste("long", var, sep = "_")]] <- result
  }
}

# Cross-sectional (if has pre data - may be limited)
for (var in COMPOSITE_OUTCOME_VARS) {
  result <- run_one_sample_ttest_vs_zero(data_wide_cs, var, "Cross-sectional")
  if (!is.null(result)) {
    one_sample_results[[paste("cs", var, sep = "_")]] <- result
  }
}

# Run one-sample t-tests for raw variables (longitudinal only)
cat("\n=== Raw Variables (Longitudinal) ===\n")

for (var in RAW_OUTCOME_VARS) {
  result <- run_one_sample_ttest_vs_zero(data_wide_raw_long, var, "Longitudinal")
  if (!is.null(result)) {
    one_sample_results[[paste("long_raw", var, sep = "_")]] <- result
  }
}

# Convert to data frame
if (length(one_sample_results) > 0) {
  one_sample_df <- bind_rows(one_sample_results)

  cat("\n\nOne-Sample T-Test Results Summary:\n")
  print(kable(one_sample_df %>%
                select(study, outcome, n, mean_change, sd_change, t_stat, df, p_value) %>%
                mutate(sig = case_when(
                  p_value < 0.001 ~ "***",
                  p_value < 0.01 ~ "**",
                  p_value < 0.05 ~ "*",
                  TRUE ~ ""
                )),
              digits = 3))

  # Save one-sample t-test results
  write_json(one_sample_df, file.path(STATS_DIR, "domain_competency_one_sample_ttest.json"), pretty = TRUE)
  cat("\nSaved: domain_competency_one_sample_ttest.json\n")
} else {
  one_sample_df <- NULL
  cat("\nNo one-sample t-test results to save\n")
}

# =============================================================================
# SECTION 10: TWO-SAMPLE T-TESTS (Between treatment groups)
# =============================================================================

cat("\n--- Two-Sample T-Tests: Between Treatment Groups ---\n")
cat("Comparing outcomes between treatment arms (independent samples).\n\n")

# Uses run_two_sample_ttest from eda_utils.R

# Prepare treatment arms in wide data
data_wide_long <- data_wide_long %>%
  mutate(
    lambda_factor = factor(multiplier,
                           levels = c(-1, -0.5, 0, 0.5, 1),
                           labels = c("neg1", "neg0.5", "zero", "pos0.5", "pos1")),
    relationship_seeking_category = case_when(
      multiplier %in% c(-1, -0.5) ~ "neg_lambda",
      multiplier == 0 ~ "zero_lambda",
      multiplier %in% c(0.5, 1) ~ "pos_lambda"
    ),
    relationship_seeking_category = factor(relationship_seeking_category,
                                           levels = c("neg_lambda", "zero_lambda", "pos_lambda"))
  )

# Run two-sample t-tests
treatment_cols <- c("personalisation", "domain", "relationship_seeking_category")
outcome_cols_for_ttest <- c(COMPOSITE_OUTCOME_VARS, paste0(COMPOSITE_OUTCOME_VARS, "_diff"))

two_sample_results <- list()

cat("=== Longitudinal: Composite Measures & Change Scores ===\n")

for (outcome in outcome_cols_for_ttest) {
  for (treatment in treatment_cols) {
    result <- run_two_sample_ttest(data_wide_long, outcome, treatment, "Longitudinal")
    if (!is.null(result)) {
      key <- paste(outcome, treatment, sep = "_")
      two_sample_results[[key]] <- result
    }
  }
}

# Convert to data frame
if (length(two_sample_results) > 0) {
  two_sample_df <- bind_rows(two_sample_results)

  cat("\n\nTwo-Sample T-Test Results Summary:\n")
  print(kable(two_sample_df %>%
                select(outcome, treatment, group1, group2, mean1, mean2, t_stat, df, p_value) %>%
                mutate(sig = case_when(
                  p_value < 0.001 ~ "***",
                  p_value < 0.01 ~ "**",
                  p_value < 0.05 ~ "*",
                  TRUE ~ ""
                )),
              digits = 3))

  # Save two-sample t-test results
  write_json(two_sample_df, file.path(STATS_DIR, "domain_competency_two_sample_ttest.json"), pretty = TRUE)
  cat("\nSaved: domain_competency_two_sample_ttest.json\n")
} else {
  two_sample_df <- NULL
  cat("\nNo two-sample t-test results to save\n")
}

# =============================================================================
# SECTION 11: ONE-SAMPLE T-TESTS BY DOMAIN
# =============================================================================

cat("\n--- One-Sample T-Tests by Domain (Longitudinal) ---\n")
cat("Testing whether pre-post change differs from 0, split by domain.\n\n")

domain_one_sample_results <- list()

for (dom in c("polchat", "emotchat")) {
  cat(sprintf("\n=== Domain: %s ===\n", dom))

  dom_data <- data_wide_long %>% filter(domain == dom)

  for (var in COMPOSITE_OUTCOME_VARS) {
    result <- run_one_sample_ttest_vs_zero(dom_data, var, paste("Longitudinal", dom))
    if (!is.null(result)) {
      domain_one_sample_results[[paste(dom, var, sep = "_")]] <- result
    }
  }
}

if (length(domain_one_sample_results) > 0) {
  domain_one_sample_df <- bind_rows(domain_one_sample_results)

  cat("\n\nOne-Sample T-Test by Domain Summary:\n")
  print(kable(domain_one_sample_df %>%
                select(study, outcome, n, mean_change, t_stat, p_value) %>%
                mutate(sig = case_when(
                  p_value < 0.001 ~ "***",
                  p_value < 0.01 ~ "**",
                  p_value < 0.05 ~ "*",
                  TRUE ~ ""
                )),
              digits = 3))

  # Save domain one-sample t-test results
  write_json(domain_one_sample_df, file.path(STATS_DIR, "domain_competency_one_sample_by_domain.json"), pretty = TRUE)
  cat("\nSaved: domain_competency_one_sample_by_domain.json\n")
}

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 12: LATEX TABLES
# =============================================================================

if (generate_tex_tables && !is.null(two_sample_df) && nrow(two_sample_df) > 0) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  create_ttest_latex(
    results_df = two_sample_df,
    test_type = "two_sample",
    table_dir = TABLE_DIR,
    filename = "domain_competency_two_sample_ttest",
    caption = "Domain Competency: Two-Sample T-Tests by Treatment Arm (Longitudinal)"
  )
}

# =============================================================================
# SECTION 13: OPTIONAL REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "14_domain_competency.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  lines <- c(
    "# Domain Competency Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines perceived domain competency - how effective",
    "participants perceive the AI to be at different tasks.",
    "",
    "**Raw Variables:**",
    "- `emotchat_effectiveness` - Perceived effectiveness for emotional support",
    "- `emotchat_satis` - Satisfaction with emotional support",
    "- `polchat_confidence` - Confidence in political discussions",
    "- `polchat_knowledge` - Perceived political knowledge",
    "",
    "**Composite Measures:**",
    "- `emotional_competency` = mean(emotchat_satis, emotchat_effectiveness)",
    "- `political_competency` = mean(polchat_knowledge, polchat_confidence)",
    "",
    "**Note:** This is an EDA-only analysis - no regression models are fitted.",
    "",
    "---",
    "",
    "## Data Summary",
    "",
    paste0("- **Cross-sectional**: ", n_distinct(data_cross_sectional$ppt_id), " participants"),
    paste0("- **Longitudinal**: ", n_distinct(data_longitudinal$ppt_id), " participants"),
    "",
    "---",
    "",
    "## Correlations",
    "",
    "### Raw Variables",
    "",
    paste0("![Raw Correlations](", fig_rel_path, "/domain_competency_raw_correlation.png)"),
    "",
    "### Composite Variables",
    "",
    paste0("![Composite Correlations](", fig_rel_path, "/domain_competency_composite_correlation.png)"),
    "",
    "---",
    "",
    "## Pre-Post Change",
    "",
    "### By Study",
    "",
    paste0("![Pre-Post by Study](", fig_rel_path, "/domain_competency_pre_post_by_study.png)"),
    "",
    "### By Domain (Longitudinal)",
    "",
    paste0("![Pre-Post by Domain](", fig_rel_path, "/domain_competency_pre_post_by_domain.png)"),
    "",
    "---",
    "",
    "## One-Sample T-Tests (Change vs Zero)",
    "",
    "One-sample t-tests testing whether pre-post change differs from 0.",
    ""
  )

  # Add one-sample t-test results table
  if (!is.null(one_sample_df) && nrow(one_sample_df) > 0) {
    lines <- c(lines,
      "### Overall Results",
      "",
      "| Study | Outcome | n | Mean Change | SD | t | df | p |",
      "|-------|---------|---|-------------|-----|---|----|----|"
    )

    for (i in seq_len(nrow(one_sample_df))) {
      row <- one_sample_df[i, ]
      sig_star <- if (row$p_value < 0.001) "***" else
                  if (row$p_value < 0.01) "**" else
                  if (row$p_value < 0.05) "*" else ""
      lines <- c(lines, sprintf(
        "| %s | %s | %d | %.2f | %.2f | %.2f | %.0f | %.4f%s |",
        row$study, row$outcome, row$n, row$mean_change, row$sd_change,
        row$t_stat, row$df, row$p_value, sig_star
      ))
    }
    lines <- c(lines, "", "*p < .05, **p < .01, ***p < .001", "")
  }

  # Add domain one-sample t-test results
  if (exists("domain_one_sample_df") && nrow(domain_one_sample_df) > 0) {
    lines <- c(lines,
      "### By Domain (Longitudinal)",
      "",
      "| Domain | Outcome | n | Mean Change | t | p |",
      "|--------|---------|---|-------------|---|---|"
    )

    for (i in seq_len(nrow(domain_one_sample_df))) {
      row <- domain_one_sample_df[i, ]
      sig_star <- if (row$p_value < 0.001) "***" else
                  if (row$p_value < 0.01) "**" else
                  if (row$p_value < 0.05) "*" else ""
      lines <- c(lines, sprintf(
        "| %s | %s | %d | %.2f | %.2f | %.4f%s |",
        row$study, row$outcome, row$n, row$mean_change, row$t_stat,
        row$p_value, sig_star
      ))
    }
    lines <- c(lines, "", "*p < .05, **p < .01, ***p < .001", "")
  }

  # Add two-sample t-test section
  lines <- c(lines,
    "---",
    "",
    "## Two-Sample T-Tests (Between Treatment Groups)",
    "",
    "Independent samples t-tests comparing outcomes between treatment arms.",
    "For relationship_seeking_category, tests compare pos_lambda vs neg_lambda.",
    ""
  )

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
  }

  # Output files
  lines <- c(lines, generate_output_files_md("domain_competency"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
