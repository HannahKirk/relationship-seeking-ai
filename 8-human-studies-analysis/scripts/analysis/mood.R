#!/usr/bin/env Rscript
# =============================================================================
# Mood/Affect Analysis
# =============================================================================
#
# Regression analysis of daily mood/affect outcomes from domain-chat ratings:
#   - Valence (pleasantness, 0-100): renamed from affect_x
#   - Arousal (activation, 0-100): renamed from affect_y
#   - Control Satisfaction (0-100)
#
# Key characteristics:
#   - OLS for cross-sectional, lmer for longitudinal (repeated daily measures)
#   - Pre-treatment covariate control (add_pre = TRUE)
#   - No domain effects (add_domain = FALSE) - only emotchat data available
#   - Includes topic fixed effects analysis
#
# Usage:
#   Rscript scripts/analysis/mood.R
#   Rscript scripts/analysis/mood.R --generate_tex_tables
#   Rscript scripts/analysis/mood.R --generate_report
#
# =============================================================================

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

library(tidyverse)
library(jsonlite)
library(lme4)
library(lmerTest)
library(emmeans)
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
source(file.path(PROJECT_ROOT, "scripts/regressions/regression_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/model_comparison_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/robustness_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/extract_coefficients.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/sjplot_to_latex.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/report_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Mood/Affect Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variables
OUTCOME_VARS <- c("valence", "arousal", "satisfaction")

# =============================================================================
# SECTION 2: DATA LOADING
# =============================================================================

cat("\n--- Loading Data ---\n\n")

mood_raw <- load_task_data("mood", DATA_DIR)

# Rename columns for clarity:
#   affect_x -> valence (pleasantness, 0-100)
#   affect_y -> arousal (activation, 0-100)
#   control_satisfaction -> satisfaction
# Keep original affect_x/y columns for affect grid plot
mood_data <- mood_raw %>%
  mutate(
    valence = affect_x,
    valence_pre = affect_x_pre,
    arousal = affect_y,
    arousal_pre = affect_y_pre,
    satisfaction = control_satisfaction,
    satisfaction_pre = control_satisfaction_pre
  )

# Filter to emotchat domain only (polchat has null mood data)
mood_data <- mood_data %>%
  filter(domain == "emotchat")

cat("After filtering to emotchat mood data:", nrow(mood_data), "observations\n")

# Prepare treatment arms
mood_data <- prepare_treatment_arms(mood_data) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

# Split by study type
data_cross_sectional <- mood_data %>% filter(study_id == "cross-sectional")
data_longitudinal <- mood_data %>% filter(study_id == "longitudinal")

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

# Merge participant characteristics (includes IPW weights) with main datasets
data_cross_sectional <- data_cross_sectional %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))
data_longitudinal <- data_longitudinal %>%
  left_join(ppt_chars, by = c("ppt_id", "study_id"))


cat("\n--- Creating Pooled Data ---\n\n")

# Create pooled (long) format for plotting
# Mood data has multiple sessions per participant, so we need to include session
# in the join keys (can't use create_pooled_data which only joins by ppt_id)

AFFECT_VARS <- c("valence", "arousal")

# Helper function for mood-specific pooling (includes session in join)
create_mood_pooled <- function(data, outcome_vars) {
  # Pivot outcomes to long format
  pooled <- data %>%
    pivot_longer(
      cols = all_of(outcome_vars),
      names_to = "construct",
      values_to = "outcome_value"
    )

  # Create pre-value lookup (need session in join for multi-session data)
  pre_vars <- paste0(outcome_vars, "_pre")
  pre_lookup <- data %>%
    select(ppt_id, session, all_of(pre_vars)) %>%
    pivot_longer(
      cols = all_of(pre_vars),
      names_to = "construct_pre",
      values_to = "outcome_value_pre"
    ) %>%
    mutate(construct = gsub("_pre$", "", construct_pre)) %>%
    select(ppt_id, session, construct, outcome_value_pre)

  # Join with session key

  pooled <- pooled %>%
    left_join(pre_lookup, by = c("ppt_id", "session", "construct"))

  # Finalize
  pooled %>%
    filter(!is.na(outcome_value)) %>%
    mutate(
      construct = factor(construct, levels = outcome_vars),
      outcome_measure = construct,
      outcome_value_delta = outcome_value - outcome_value_pre
    )
}

data_cross_sectional_pooled <- create_mood_pooled(
  data_cross_sectional, OUTCOME_VARS
)
data_longitudinal_pooled <- create_mood_pooled(
  data_longitudinal, OUTCOME_VARS
)

cat("  Cross-sectional pooled:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Longitudinal pooled:", nrow(data_longitudinal_pooled), "rows\n")

# =============================================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n--- Exploratory Data Analysis ---\n\n")

# Print dataset summary
cat("\nSummary statistics by study and lambda:\n")
summary_by_lambda <- mood_data %>%
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

# --- Pre-Post Change Plot ---
cat("\nCreating pre-post change plot...\n")

# Combine both studies for pre-post plots
data_combined <- bind_rows(data_cross_sectional, data_longitudinal)

# Pre-post change plot for all mood outcomes
p_pre_post <- plot_pre_post_change_combined(
  data = data_combined,
  outcome_vars = OUTCOME_VARS,
  facet_var = "study_id",
  total_height = 8,
  total_width = 14,
  figure_dir = FIGURE_DIR,
  save_pdf = TRUE,
  filename_prefix = "mood"
)

cat("Saved: mood_pre_post_combined.pdf/png\n")

# --- Correlation Heatmaps ---
cat("\nCreating correlation heatmaps...\n")
corr_plot <- plot_corr_heatmaps_combined(
  data_cs = data_cross_sectional,
  data_long = data_longitudinal,
  outcome_vars = OUTCOME_VARS
)
save_plot(FIGURE_DIR, corr_plot, "mood_correlation_heatmap", 12, 6)

# --- Boxplots ---
cat("\nCreating boxplots...\n")
box_plot <- plot_boxplots(
  data = data_combined,
  outcome_vars = OUTCOME_VARS
)
save_plot(FIGURE_DIR, box_plot, "mood_boxplots", 10, 5)

# --- Distribution Histograms ---
cat("\nCreating distribution histograms...\n")
dist_plot <- plot_distributions(
  data = data_combined,
  outcome_vars = OUTCOME_VARS
)
save_plot(FIGURE_DIR, dist_plot, "mood_distributions", 12, 6)

# --- Affect Grid Overlay Plot ---
cat("\nCreating affect grid overlay plot...\n")

p_affect_grid <- plot_affect_grid_overlay(
  df = data_combined,
  facet_rows = "study_id",
  alpha = 0.3,
  bins = 8,
  show_points = TRUE
)
save_plot(FIGURE_DIR, p_affect_grid, "mood_affect_grid", 14, 8)

cat("Saved: mood_affect_grid.pdf/png\n")

# --- Trajectory Plots (longitudinal only) ---
cat("\nCreating trajectory plots (delta from pre)...\n")

traj_plot_rs <- plot_raw_trends(
  data_longitudinal_pooled,
  time_var = "session_numeric",
  facet_vars = c("relationship_seeking_category"),
  plot_smoothed = FALSE,
  outcome_var = "outcome_value_delta",
  height = 10, width = 10
)
save_plot(FIGURE_DIR, traj_plot_rs, "mood_trajectories_by_rs", 10, 10)

# --- Plot by Lambda ---
cat("\nCreating lambda plot (delta from pre)...\n")

p_lambda <- plot_raw_lambda(
  data_long = data_longitudinal_pooled,
  data_cross = data_cross_sectional_pooled,
  plot_study = "both",
  plot_smoothed = FALSE,
  outcome_var = "outcome_value_delta",
  height = 4, width = 10
)
save_plot(FIGURE_DIR, p_lambda, "mood_by_lambda", 10, 4)


# =============================================================================
# SECTION 4: FUNCTIONAL FORM COMPARISON
# =============================================================================

cat("\n--- Functional Form Comparison ---\n\n")

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
    add_domain = FALSE, # Only emotchat data
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

  # Longitudinal
  cat("\n  Longitudinal:\n")
  result_long <- compare_functional_forms(
    data = data_longitudinal,
    outcome_var = outcome,
    add_domain = FALSE,
    add_pre = TRUE,
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

  # Get best spec for this outcome
  best_spec_cs <- best_specs_cs$best_spec[best_specs_cs$outcome == outcome]
  best_spec_long <- best_specs_long$best_spec[best_specs_long$outcome == outcome]

  cat("  Using", best_spec_cs, "for cross-sectional,", best_spec_long, "for longitudinal\n")

  # Cross-sectional: OLS with pre-treatment control, no domain
  cat("\nCross-sectional:\n")
  mods_cross_sectional[[outcome]] <- fit_models(
    data = data_cross_sectional,
    outcome_var = outcome,
    continuous_spec = best_spec_cs,
    add_domain = FALSE,  # Only emotchat data
    add_pre = TRUE,      # Control for baseline
    add_time = FALSE,
    model_family = "ols"
  )

  # Longitudinal: lmer with pre-treatment control and time, no domain
  cat("\nLongitudinal:\n")
  mods_longitudinal[[outcome]] <- fit_models(
    data = data_longitudinal,
    outcome_var = outcome,
    continuous_spec = best_spec_long,
    add_domain = FALSE,  # Only emotchat data
    add_pre = TRUE,      # Control for baseline
    add_time = TRUE,     # Repeated daily measures
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
# SECTION 7: ROBUSTNESS ANALYSIS
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
  add_domain = FALSE,
  add_pre = TRUE,
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
  add_domain = FALSE,
  add_pre = TRUE,
  add_time = TRUE,
  time_var = "session_numeric",
  use_weights = TRUE
)

# =============================================================================
# SECTION 8: TOPIC FIXED EFFECTS ANALYSIS
# =============================================================================

cat("\n--- Topic Fixed Effects Analysis ---\n\n")

# Outcomes for topic FE analysis
TOPIC_OUTCOME_VARS <- c("satisfaction", "valence", "arousal")

# Set reference topic
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
    add_domain = FALSE,
    add_pre = TRUE,
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
    add_domain = FALSE,
    add_pre = TRUE,
    add_time = TRUE,
    time_var = "session_numeric",
    add_topic = TRUE
  )
  cat("  Formula:", deparse(formula_long), "\n")
  mod_long <- run_reg_model(data_long_topic, formula_long, model_family = "lmer", REML = TRUE)

  # Store models
  topic_models[[outcome]] <- list(
    cross_sectional = mod_cs,
    longitudinal = mod_long
  )
}

# Create combined topic effects plots (one per study type)
cat("\nCreating combined topic effects plots...\n")

# Cross-sectional plot (mood is emotchat only, no domain coloring needed)
p_topic_cs <- plot_topic_effects_combined(
  topic_models = topic_models,
  study_type = "cross_sectional",
  fe_var = "topic",
  reference_level = reference_topic,
  fdr_correct = TRUE
)
save_plot(FIGURE_DIR, p_topic_cs,
          "mood_topic_effects_cross_sectional", 10, 12)
cat("  Saved: mood_topic_effects_cross_sectional.pdf/png\n")

# Longitudinal plot
p_topic_long <- plot_topic_effects_combined(
  topic_models = topic_models,
  study_type = "longitudinal",
  fe_var = "topic",
  reference_level = reference_topic,
  fdr_correct = TRUE
)
save_plot(FIGURE_DIR, p_topic_long,
          "mood_topic_effects_longitudinal", 10, 12)
cat("  Saved: mood_topic_effects_longitudinal.pdf/png\n")

# Save topic models
saveRDS(topic_models, file.path(MODEL_DIR, "mood_topic_models.rds"))
cat("\nSaved: mood_topic_models.rds\n")

# =============================================================================
# SECTION 9: SAVE MODELS AND DATA
# =============================================================================

cat("\n--- Saving Models and Data ---\n")


# Save models
cat("\nSaving models...\n")
saveRDS(mods_cross_sectional, file.path(MODEL_DIR, "mood_cross_sectional.rds"))
saveRDS(mods_longitudinal, file.path(MODEL_DIR, "mood_longitudinal.rds"))


# Save pooled data
mood_pooled_data <- bind_rows(
  data_cross_sectional_pooled %>% mutate(study_id = "cross-sectional"),
  data_longitudinal_pooled %>% mutate(study_id = "longitudinal")
)
# Remove domain column - domain has only 1 level (emotchat)
mood_pooled_data <- mood_pooled_data %>% select(-domain)

mood_data_path <- file.path(MODEL_DIR, "mood_data.rds")
saveRDS(mood_pooled_data, mood_data_path)
cat("  Saved pooled data:", mood_data_path, "\n")

# Save spec comparison data
spec_data <- list(
  best_specs_cs = best_specs_cs,
  best_specs_long = best_specs_long,
  full_spec_cs = full_spec_cs,
  full_spec_long = full_spec_long
)
spec_data_path <- file.path(MODEL_DIR, "mood_spec_data.rds")
saveRDS(spec_data, spec_data_path)
cat("  Saved spec comparison data:", spec_data_path, "\n")

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 10: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n")

  # Functional form tables
  generate_functional_form_tables(
    perf_cs = perf_cross_sectional,
    perf_long = perf_longitudinal,
    table_prefix = "mood",
    table_dir = TABLE_DIR,
    task_name = "Mood/Affect"
  )

  # Main regression tables
  generate_regression_tables(
    mods_cs = mods_cross_sectional,
    mods_long = mods_longitudinal,
    outcome_vars = OUTCOME_VARS,
    best_specs_cs = best_specs_cs,
    best_specs_long = best_specs_long,
    table_prefix = "mood",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels("session_numeric")
  )

  # Full spec tables
  generate_full_spec_tables(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long,
    table_prefix = "mood",
    table_dir = TABLE_DIR,
    task_name = "Mood/Affect"
  )

  # Robustness tables
  create_robustness_latex_table(
    wide_df = robustness_cs$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "mood_robustness_cs",
    caption = "Mood Robustness",
    study_type = "cross_sectional"
  )
  create_robustness_latex_table(
    wide_df = robustness_long$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "mood_robustness_long",
    caption = "Mood Robustness",
    study_type = "longitudinal"
  )

  # Parent tex file
  generate_parent_tex(
    section_name = "Mood/Affect",
    constructs = OUTCOME_VARS,
    construct_labels = setNames(
      c("Valence", "Arousal", "Satisfaction"),
      OUTCOME_VARS
    ),
    func_form_prefix = "mood_functional_form",
    main_reg_prefix = "mood",
    full_spec_prefix = "mood_full_spec",
    robustness_prefix = "mood_robustness",
    table_dir = TABLE_DIR,
    output_filename = "mood_parent"
  )
}

# =============================================================================
# SECTION 11: OPTIONAL REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "10_mood.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  # Header and overview
  lines <- c(
    "# Mood/Affect Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines effects on daily mood/affect outcomes.",
    "",
    "**Outcomes (analysed in main paper):**",
    "- **Valence**: Pleasantness (0-100, unpleasant to pleasant)",
    "- **Arousal**: Activation level (0-100, sleepy to alert)",
    "",
    "**Additional measures (collected but not analysed in main paper):**",
    "- **Satisfaction**: How in control/satisfied participants felt about aspect of their personal wellbeing (0-100)",
    "",
    "**Key Features:**",
    "- Only emotchat domain data (no polchat for mood measures)",
    "- Pre-convo baseline control",
    "",
    "---",
    ""
  )

  # Data summary
  lines <- c(lines, generate_data_summary_md(
    data_cross_sectional_pooled, data_longitudinal_pooled
  ))

  lines <- c(lines,
    "## Exploratory Data Analysis",
    "",
    paste0("![Boxplots](", fig_rel_path, "/mood_boxplots.png)"),
    "",
    paste0("![Distributions](", fig_rel_path, "/mood_distributions.png)"),
    "",
    paste0("![Correlations](", fig_rel_path, "/mood_correlation_heatmap.png)"),
    "",
    "### Pre-Post Change",
    "",
    paste0("![Pre-Post Change](", fig_rel_path, "/mood_pre_post_combined.png)"),
    "",
    "### Affect Grid (Pre vs Post)",
    "",
    "The affect grid shows the distribution of valence (x-axis) and arousal (y-axis)",
    "before and after the intervention. Quadrants represent different affective states:",
    "- **Top-Left (Stress)**: High arousal, unpleasant",
    "- **Top-Right (Excitement)**: High arousal, pleasant",
    "- **Bottom-Left (Boredom)**: Low arousal, unpleasant",
    "- **Bottom-Right (Relaxation)**: Low arousal, pleasant",
    "",
    paste0("![Affect Grid](", fig_rel_path, "/mood_affect_grid.png)"),
    "",
    "### Trajectories Over Time",
    "",
    paste0("![Trajectories](", fig_rel_path, "/mood_trajectories_by_rs.png)"),
    "",
    "### Outcomes by Lambda",
    "",
    paste0("![By Lambda](", fig_rel_path, "/mood_by_lambda.png)"),
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
      full_spec_cs, full_spec_long, OUTCOME_VARS))
  }

  # Model coefficients
  if (exists("mods_cross_sectional") && exists("mods_longitudinal")) {
    cat("  Adding model coefficients...\n")
    lines <- c(lines, generate_coefficients_md(
      mods_cross_sectional, mods_longitudinal, OUTCOME_VARS), "", "---", "")
  }

  # Robustness summary
  if (exists("robustness_cs") && exists("robustness_long")) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      robustness_cs, robustness_long, OUTCOME_VARS, "mood"))
  }

  # Topic effects section
  if (exists("topic_models") && length(topic_models) > 0) {
    lines <- c(lines,
      "## Topic Fixed Effects",
      "",
      "Analysis of how different conversation topics affect mood outcomes,",
      paste0("with \"", reference_topic, "\" as the reference category."),
      "",
      "Forest plots show coefficients for each topic relative to the reference,",
      "with FDR-adjusted significance indicators.",
      "",
      "### Cross-sectional",
      "",
      paste0("![Topic Effects - Cross-sectional](",
             fig_rel_path, "/mood_topic_effects_cross_sectional.png)"),
      "",
      "### Longitudinal",
      "",
      paste0("![Topic Effects - Longitudinal](",
             fig_rel_path, "/mood_topic_effects_longitudinal.png)"),
      "",
      "---",
      ""
    )
  }

  # Output files
  lines <- c(lines, generate_output_files_md("mood"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
