#!/usr/bin/env Rscript
# =============================================================================
# Preferences Analysis
# =============================================================================
#
# Regression analysis of preference outcomes (likeability, engagingness, helpfulness)
#
#
# Usage:
#   Rscript scripts/analysis/preferences.R                        # Core analysis only
#   Rscript scripts/analysis/preferences.R --generate_tex_tables  # Also generate LaTeX tables
#   Rscript scripts/analysis/preferences.R --generate_report      # Also generate markdown report
#
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(patchwork)
library(knitr)
library(sjPlot)

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
source(file.path(PROJECT_ROOT, "scripts/regressions/regression_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/model_comparison_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/robustness_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/extract_coefficients.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/sjplot_to_latex.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/report_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Preferences Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variables
OUTCOME_VARS <- c("likeability", "engagingness", "helpfulness")

# Set seed
set.seed(1234)

# =============================================================================
# SECTION 2: DATA PREPARATION
# =============================================================================

cat("\n--- Loading Data ---\n\n")

prefs_raw <- load_task_data("preferences", DATA_DIR)
prefs_data <- prepare_treatment_arms(prefs_raw) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

# Split by study type
data_cross_sectional <- prefs_data %>% filter(study_id == "cross-sectional")
data_longitudinal <- prefs_data %>% filter(study_id == "longitudinal")

cat("  Cross-sectional:", nrow(data_cross_sectional), "obs from",
    n_distinct(data_cross_sectional$ppt_id), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "obs from",
    n_distinct(data_longitudinal$ppt_id), "participants\n")

# Load participant characteristics (sociodemos, pref groups, IPW weights)
ppt_chars <- load_ppt_characteristics(
  DATA_DIR, GENERATED_DIR,
  include_psychosocial = TRUE,
  apply_coarsening = TRUE
)

# Merge participant characteristics with main datasets
data_cross_sectional <- data_cross_sectional %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))
data_longitudinal <- data_longitudinal %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))

cat("\n--- Creating Pooled Data ---\n\n")

# Create pooled (long) format for contrast analysis
# 1:1 mapping: each outcome_var is its own construct (no construct_mapping needed)
data_cross_sectional_pooled <- create_pooled_data(
  data_cross_sectional, OUTCOME_VARS)

data_longitudinal_pooled <- create_pooled_data(
  data_longitudinal, OUTCOME_VARS)

cat("  Cross-sectional pooled:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Longitudinal pooled:", nrow(data_longitudinal_pooled), "rows\n")

# =============================================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n--- Exploratory Data Analysis ---\n\n")

# Summary by study and lambda
cat("\nSummary statistics by study and lambda:\n")
summary_by_lambda <- prefs_data %>%
  group_by(study_id, lambda) %>%
  summarise(
    across(all_of(OUTCOME_VARS),
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE))),
    n = n(),
    .groups = "drop"
  )
print(summary_by_lambda)

# Outcome correlations
cat("\nOutcome correlations (cross-sectional):\n")
print(round(cor(data_cross_sectional[, OUTCOME_VARS], use = "pairwise.complete.obs"), 3))

cat("\nOutcome correlations (longitudinal):\n")
print(round(cor(data_longitudinal[, OUTCOME_VARS], use = "pairwise.complete.obs"), 3))

# Trajectory plots (longitudinal only)
cat("\nCreating trajectory plots...\n")

traj_plot_rs <- plot_raw_trends(
  data_longitudinal_pooled,
  time_var = "session_numeric",
  facet_vars = c("relationship_seeking_category"),
  plot_smoothed = FALSE,
  height = 4, width = 8
)
save_plot(FIGURE_DIR, traj_plot_rs, "preferences_trajectories_by_rs", 14, 10)

traj_plot_full <- plot_raw_trends(
  data_longitudinal_pooled,
  time_var = "session_numeric",
  facet_vars = c("relationship_seeking_category", "personalisation", "domain"),
  plot_smoothed = FALSE,
  height = 8, width = 14
)
save_plot(FIGURE_DIR, traj_plot_full, "preferences_trajectories_full", 14, 10)

# --- Raw Lambda Plot ---
cat("\nCreating raw lambda plots...\n")
lambda_plot <- plot_raw_lambda(
  data_long = data_longitudinal_pooled,
  data_cross = data_cross_sectional_pooled,
  plot_study = "both",
  plot_smoothed = FALSE,
  scales = "free"
)
save_plot(FIGURE_DIR, lambda_plot, "preferences_by_lambda", 14, 10)

# --- Correlation Heatmaps ---
cat("\nCreating correlation heatmaps...\n")
corr_plot <- plot_corr_heatmaps_combined(
  data_cs = data_cross_sectional,
  data_long = data_longitudinal,
  outcome_vars = OUTCOME_VARS
)
save_plot(FIGURE_DIR, corr_plot, "preferences_correlation_heatmap", 10, 5)

# --- Distribution Histograms (3x2 facet: outcome × study) ---
cat("\nCreating distribution histograms...\n")
dist_plot <- plot_distributions(
  data = prefs_data,
  outcome_vars = OUTCOME_VARS,
  binwidth = 10
)
save_plot(FIGURE_DIR, dist_plot, "preferences_distributions", 10, 6)

# --- Boxplots by Study and Outcome ---
cat("\nCreating boxplots...\n")
box_plot <- plot_boxplots(
  data = prefs_data,
  outcome_vars = OUTCOME_VARS
)
save_plot(FIGURE_DIR, box_plot, "preferences_boxplots", 8, 5)

# =============================================================================
# SECTION 4: FUNCTIONAL FORM COMPARISON
# =============================================================================

cat("\n--- Functional Form Comparison ---\n\n")

# Uses compare_functional_forms() for all outcomes.
# Selects best specification by AIC.

perf_cross_sectional <- list()
perf_longitudinal <- list()
best_specs_cs <- data.frame()
best_specs_long <- data.frame()

for (outcome in OUTCOME_VARS) {
  cat(sprintf("\n=== %s ===\n", outcome))

  # Cross-sectional
  cat("\n  Cross-Sectional:\n")
  result_cs <- compare_functional_forms(
    data = data_cross_sectional,
    outcome_var = outcome,
    add_domain = TRUE,
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

  # Longitudinal
  cat("\n  Longitudinal:\n")
  result_long <- compare_functional_forms(
    data = data_longitudinal,
    outcome_var = outcome,
    add_domain = TRUE,
    add_time = TRUE,
    time_var = "session_numeric",
    model_family = "lmer",
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
# SECTION 5: FIT MODELS
# =============================================================================

cat("\n--- Fitting Models ---\n\n")

mods_cross_sectional <- list()
mods_longitudinal <- list()

for (outcome in OUTCOME_VARS) {
  cat("\n=== Fitting models for:", outcome, "===\n")

  # Get best spec for this outcome (use [1] to ensure scalar, not vector)
  best_spec_cs <- best_specs_cs$best_spec[best_specs_cs$outcome == outcome][1]
  best_spec_long <- best_specs_long$best_spec[best_specs_long$outcome == outcome][1]

  cat("  Using", best_spec_cs, "for cross-sectional,", best_spec_long, "for longitudinal\n")

  # Cross-sectional
  cat("\nCross-sectional:\n")
  mods_cross_sectional[[outcome]] <- fit_models(
    data = data_cross_sectional,
    outcome_var = outcome,
    continuous_spec = best_spec_cs,
    add_domain = TRUE,
    add_pre = FALSE,
    add_time = FALSE,
    model_family = "ols"
  )

  # Longitudinal
  cat("\nLongitudinal:\n")
  mods_longitudinal[[outcome]] <- fit_models(
    data = data_longitudinal,
    outcome_var = outcome,
    continuous_spec = best_spec_long,
    add_domain = TRUE,
    add_pre = FALSE,
    add_time = TRUE,
    time_var = "session_numeric",
    model_family = "lmer"
  )
}

cat("\n--- Printing Model Coefficients ---\n")

print_model_coefficients_section(
  mods_cross_sectional = mods_cross_sectional,
  mods_longitudinal = mods_longitudinal,
  outcome_vars = OUTCOME_VARS
)

# =============================================================================
# SECTION 6: PERFORMANCE COMPARISON
# =============================================================================

cat("\n--- Full-Model Performance Comparison ---\n\n")

full_spec_results <- compute_full_spec_comparison(                                                                  
    mods_cross_sectional, mods_longitudinal, OUTCOME_VARS                                                             
  )                                                                                                                   

full_spec_cs <- full_spec_results$full_spec_cs                                                                      
full_spec_long <- full_spec_results$full_spec_long


# =============================================================================
# SECTION 7: ROBUSTNESS CHECKS
# =============================================================================

cat("\n--- Robustness Analysis ---\n\n")

# Cross-sectional robustness
cat("Cross-sectional robustness:\n")
robustness_cs <- run_robustness_analysis(
  data = data_cross_sectional,
  outcome_vars = OUTCOME_VARS,
  rs_variable = "lambda",
  specs_lookup = best_specs_cs,
  study_type = "cross_sectional",
  add_domain = TRUE,
  add_time = FALSE,
  use_weights = FALSE
)

# Longitudinal robustness
cat("\nLongitudinal robustness:\n")
robustness_long <- run_robustness_analysis(
  data = data_longitudinal,
  outcome_vars = OUTCOME_VARS,
  rs_variable = "lambda",
  specs_lookup = best_specs_long,
  study_type = "longitudinal",
  add_domain = TRUE,
  add_time = TRUE,
  time_var = "session_numeric",
  use_weights = TRUE
)

# =============================================================================
# SECTION 8: TOPIC FIXED EFFECTS ANALYSIS
# =============================================================================

cat("\n--- Topic Fixed Effects Analysis ---\n\n")

# Outcomes for topic FE analysis
TOPIC_OUTCOME_VARS <- c("engagingness", "helpfulness", "likeability")

# Set reference topic (common topic across both domains if possible)
reference_topic <- "Alcohol consumption habits"

# Prepare data: ensure topic is a factor with specified reference level
data_cs_topic <- data_cross_sectional %>%
  filter(!is.na(topic)) %>%
  mutate(topic = relevel(factor(topic), ref = reference_topic))

data_long_topic <- data_longitudinal %>%
  filter(!is.na(topic)) %>%
  mutate(topic = relevel(factor(topic), ref = reference_topic))

cat("Reference topic:", reference_topic, "\n")
cat("N unique topics (CS):", n_distinct(data_cs_topic$topic), "\n")
cat("N unique topics (Long):", n_distinct(data_long_topic$topic), "\n")

# Create topic-domain mapping for label coloring
topic_domain_map <- data_cross_sectional %>%
  filter(!is.na(topic), !is.na(domain)) %>%
  distinct(topic, domain) %>%
  { setNames(.$domain, .$topic) }

cat("Topics by domain:\n")
cat("  Emotchat:", sum(topic_domain_map == "emotchat"), "\n")
cat("  Polchat:", sum(topic_domain_map == "polchat"), "\n")

# Fit topic fixed effects models for each outcome
topic_models <- list()

for (outcome in TOPIC_OUTCOME_VARS) {
  cat(sprintf("\n=== Topic effects for: %s ===\n", outcome))

  # Cross-sectional: OLS with topic FE
  cat("  Fitting cross-sectional topic model...\n")
  formula_cs <- build_formula(
    outcome_var = outcome,
    rs_variable = "lambda",
    model_spec = "additive",
    add_domain = FALSE,  # Domain is captured via topic
    add_topic = TRUE
  )
  cat("  Formula:", deparse(formula_cs), "\n")
  mod_cs <- run_reg_model(data_cs_topic, formula_cs, model_family = "ols")

  # Longitudinal: lmer with topic FE
  cat("  Fitting longitudinal topic model...\n")
  formula_long <- build_formula(
    outcome_var = outcome,
    rs_variable = "lambda",
    model_spec = "additive",
    add_domain = FALSE,  # Domain is captured via topic
    add_time = TRUE,
    time_var = "session_numeric",
    add_topic = TRUE
  )
  cat("  Formula:", deparse(formula_long), "\n")
  mod_long <- run_reg_model(data_long_topic, formula_long,
                            model_family = "lmer", REML = TRUE)

  # Store models
  topic_models[[outcome]] <- list(
    cross_sectional = mod_cs,
    longitudinal = mod_long
  )
}

# Create combined topic effects plots (one per study type)
cat("\nCreating combined topic effects plots...\n")

# Cross-sectional plot
p_topic_cs <- plot_topic_effects_combined(
  topic_models = topic_models,
  study_type = "cross_sectional",
  fe_var = "topic",
  reference_level = reference_topic,
  level_domains = topic_domain_map,
  fdr_correct = TRUE
)
save_plot(FIGURE_DIR, p_topic_cs,
          "preferences_topic_effects_cross_sectional", 14, 12)
cat("  Saved: preferences_topic_effects_cross_sectional.pdf/png\n")

# Longitudinal plot
p_topic_long <- plot_topic_effects_combined(
  topic_models = topic_models,
  study_type = "longitudinal",
  fe_var = "topic",
  reference_level = reference_topic,
  level_domains = topic_domain_map,
  fdr_correct = TRUE
)
save_plot(FIGURE_DIR, p_topic_long,
          "preferences_topic_effects_longitudinal", 14, 12)
cat("  Saved: preferences_topic_effects_longitudinal.pdf/png\n")

# Save topic models
saveRDS(topic_models, file.path(MODEL_DIR, "preferences_topic_models.rds"))
cat("\nSaved: preferences_topic_models.rds\n")

# =============================================================================
# SECTION 9: SAVE
# =============================================================================

cat("\n--- Saving Models and Data for Contrast Analysis ---\n")

# Save pooled data, combined cross-sectional and longitudinal
preferences_data <- bind_rows(
  data_cross_sectional_pooled,
  data_longitudinal_pooled
)

preferences_data_path <- file.path(MODEL_DIR, "preferences_data.rds")
saveRDS(preferences_data, preferences_data_path)
cat("  Saved pooled data:", preferences_data_path, "\n")

# Save spec comparison data for report generation
spec_data <- list(
  best_specs_cs = best_specs_cs,
  best_specs_long = best_specs_long,
  full_spec_cs = full_spec_cs,
  full_spec_long = full_spec_long
)
spec_data_path <- file.path(MODEL_DIR, "preferences_spec_data.rds")
saveRDS(spec_data, spec_data_path)
cat("  Saved spec comparison data:", spec_data_path, "\n")

# Save models
cat("\nSaving models...\n")
saveRDS(mods_cross_sectional, file.path(MODEL_DIR, "preferences_cross_sectional.rds"))
saveRDS(mods_longitudinal, file.path(MODEL_DIR, "preferences_longitudinal.rds"))

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 10: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  generate_functional_form_tables(
    perf_cs = perf_cross_sectional,
    perf_long = perf_longitudinal,
    table_prefix = "preferences",
    table_dir = TABLE_DIR,
    task_name = "Preferences"
  )

  generate_regression_tables(
    mods_cs = mods_cross_sectional,
    mods_long = mods_longitudinal,
    outcome_vars = OUTCOME_VARS,
    best_specs_cs = best_specs_cs,
    best_specs_long = best_specs_long,
    table_prefix = "preferences",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels("session_numeric")
  )

  generate_full_spec_tables(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long,
    table_prefix = "preferences",
    table_dir = TABLE_DIR,
    task_name = "Preferences"
  )

  create_robustness_latex_table(
    wide_df = robustness_cs$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "preferences_robustness_cs",
    caption = "Preferences Robustness",
    study_type = "cross_sectional"
  )
  create_robustness_latex_table(
    wide_df = robustness_long$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "preferences_robustness_long",
    caption = "Preferences Robustness",
    study_type = "longitudinal"
  )

  generate_parent_tex(
    section_name = "Preferences",
    constructs = OUTCOME_VARS,
    construct_labels = setNames(
      tools::toTitleCase(OUTCOME_VARS), OUTCOME_VARS
    ),
    func_form_prefix = "preferences_functional_form",
    main_reg_prefix = "preferences",
    full_spec_prefix = "preferences_full_spec",
    robustness_prefix = "preferences_robustness",
    table_dir = TABLE_DIR,
    output_filename = "preferences_parent"
  )
}
# =============================================================================
# SECTION 11: REPORT
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "05_preferences.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"
  outcomes <- c("likeability", "engagingness", "helpfulness")

  # Header and overview
  lines <- c(
    "# Preferences Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines effects on user preference ratings.",
    "",
    "**Outcomes:** Likeability, Engagingness, Helpfulness",
    "",
    "**Treatment Arms:**",
    "- **$\\lambda$**: Relationship-seeking intensity (-1 to +1)",
    "- **Domain**: polchat vs emotchat",
    "- **Personalisation**: personalised vs non-personalised",
    "",
    "---",
    ""
  )

  # Data summary by outcome
  lines <- c(lines, generate_data_summary_md(
    data_cross_sectional_pooled, data_longitudinal_pooled
  ))

  # EDA figures
  lines <- c(lines,
    "## Exploratory Data Analysis",
    "",
    paste0("![Boxplots](", fig_rel_path, "/preferences_boxplots.png)"),
    "",
    paste0("![Distributions](", fig_rel_path, "/preferences_distributions.png)"),
    "",
    paste0("![Correlations](", fig_rel_path,
           "/preferences_correlation_heatmap.png)"),
    "",
    paste0("![Trajectories](", fig_rel_path,
           "/preferences_trajectories_by_rs.png)"),
    "",
    paste0("![Outcomes by Lambda](", fig_rel_path,
           "/preferences_by_lambda.png)"),
    "",
    "---",
    ""
  )

  # Functional form comparison (if available)
  if (exists("best_specs_cs") && nrow(best_specs_cs) > 0) {
    lines <- c(lines, generate_best_specs_md(best_specs_cs, best_specs_long))
  }

  # Full-model specification comparison (if available)
  if (exists("full_spec_cs") && length(full_spec_cs) > 0) {
    lines <- c(lines, generate_full_spec_comparison_md(
      full_spec_cs, full_spec_long, outcomes))
  }

  # Model coefficients (if available)
  if (exists("mods_cross_sectional") && exists("mods_longitudinal")) {
    cat("  Adding model coefficients...\n")
    lines <- c(lines, generate_coefficients_md(
      mods_cross_sectional, mods_longitudinal, outcomes), "", "---", "")
  }

  # Robustness summary
  if (exists("robustness_cs") && exists("robustness_long")) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      robustness_cs, robustness_long, OUTCOME_VARS, "preferences"))
  }

  # Topic effects section
  if (exists("topic_models") && length(topic_models) > 0) {
    lines <- c(lines,
      "## Topic Fixed Effects",
      "",
      "Analysis of how different conversation topics affect preference outcomes,",
      paste0("with \"", reference_topic, "\" as the reference category."),
      "",
      "Forest plots show coefficients for each topic relative to the reference,",
      "with FDR-adjusted significance indicators.",
      "Labels are colored by domain: green = emotchat, grey = polchat.",
      "",
      "### Cross-sectional",
      "",
      paste0("![Topic Effects - Cross-sectional](",
             fig_rel_path, "/preferences_topic_effects_cross_sectional.png)"),
      "",
      "### Longitudinal",
      "",
      paste0("![Topic Effects - Longitudinal](",
             fig_rel_path, "/preferences_topic_effects_longitudinal.png)"),
      "",
      "---",
      ""
    )
  }

  # Output files
  lines <- c(lines, generate_output_files_md("preferences"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}

