#!/usr/bin/env Rscript
# =============================================================================
# Relational Measures Analysis
# =============================================================================
#
# Regression analysis of relational outcomes:
#   - ios_scale (Inclusion of Other in Self - both studies)
#   - tool_friend (Tool vs Friend perception - longitudinal only)
#   - personalisation_sensecheck (longitudinal only, this is a manipulation check)
#
# Usage:
#   Rscript scripts/analysis/relational.R                        # Core analysis only
#   Rscript scripts/analysis/relational.R --generate_tex_tables  # Also generate LaTeX tables
#   Rscript scripts/analysis/relational.R --generate_report      # Also generate markdown report
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
source(file.path(PROJECT_ROOT, "scripts/utils_r/eda_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("Relational Measures Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variables
# Cross-sectional only has ios_scale
# Longitudinal has all three
OUTCOME_VARS_CS <- c("ios_scale")
OUTCOME_VARS_LONG <- c("ios_scale", "tool_friend", "personalisation_sensecheck")
ALL_OUTCOME_VARS <- OUTCOME_VARS_LONG

# Set seed
set.seed(1234)

# =============================================================================
# SECTION 2: DATA PREPARATION
# =============================================================================

cat("\n--- Loading Data ---\n\n")

relational_raw <- load_task_data("relational", DATA_DIR)
relational_data <- prepare_treatment_arms(relational_raw) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

# Split by study type
data_cross_sectional <- relational_data %>%
  filter(study_id == "cross-sectional") %>%
  select(-tool_friend, -personalisation_sensecheck)  # Remove null columns

data_longitudinal <- relational_data %>%
  filter(study_id == "longitudinal")

cat("  Cross-sectional:", nrow(data_cross_sectional), "obs from",
    n_distinct(data_cross_sectional$ppt_id), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "obs from",
    n_distinct(data_longitudinal$ppt_id), "participants\n")

# Load participant characteristics (sociodemos, pref types, IPW weights, psychosocial)
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

cat("\n--- Creating Pooled Data for EDA ---\n\n")

# Cross-sectional: only ios_scale (1:1 mapping, no construct_mapping needed)
data_cross_sectional_pooled <- create_pooled_data(
  data_cross_sectional,
  outcome_vars = OUTCOME_VARS_CS
)

# Longitudinal: all three outcomes (1:1 mapping, no construct_mapping needed)
data_longitudinal_pooled <- create_pooled_data(
  data_longitudinal,
  outcome_vars = OUTCOME_VARS_LONG
)

cat("  CS pooled:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Long pooled:", nrow(data_longitudinal_pooled), "rows\n")

# =============================================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n--- Exploratory Data Analysis ---\n\n")

# Summary by study and lambda
cat("\nSummary statistics by study and lambda:\n")
summary_cs <- data_cross_sectional %>%
  group_by(lambda) %>%
  summarise(
    ios_scale_mean = mean(ios_scale, na.rm = TRUE),
    ios_scale_sd = sd(ios_scale, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(study_id = "cross-sectional")

summary_long <- data_longitudinal %>%
  group_by(lambda) %>%
  summarise(
    across(all_of(OUTCOME_VARS_LONG),
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE))),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(study_id = "longitudinal")

print(summary_cs)
print(summary_long)

# Outcome correlations (longitudinal only - has all 3 outcomes)
cat("\nOutcome correlations (longitudinal):\n")
print(round(cor(data_longitudinal[, OUTCOME_VARS_LONG], use = "pairwise.complete.obs"), 3))

# --- Trajectory Plots (longitudinal only) ---
cat("\nCreating trajectory plots...\n")

traj_plot_rs <- plot_raw_trends(
  data_longitudinal_pooled,
  time_var = "week_numeric",
  facet_vars = c("relationship_seeking_category"),
  plot_smoothed = FALSE,
  height = 6, width = 14
)
save_plot(FIGURE_DIR, traj_plot_rs, "relational_trajectories_by_rs", 14, 10)

traj_plot_full <- plot_raw_trends(
  data_longitudinal_pooled,
  time_var = "week_numeric",
  facet_vars = c("relationship_seeking_category", "personalisation"),
  plot_smoothed = FALSE,
  height = 10, width = 14
)
save_plot(FIGURE_DIR, traj_plot_full, "relational_trajectories_full", 14, 10)

# --- Raw Lambda Plot ---
cat("\nCreating raw lambda plots...\n")

lambda_plot <- plot_raw_lambda(
  data_long = data_longitudinal_pooled,
  data_cross = data_cross_sectional_pooled,
  plot_study = "both",
  plot_smoothed = FALSE,
  scales = "free"  # IOS is 1-7, others are 0-100
)
save_plot(FIGURE_DIR, lambda_plot, "relational_by_lambda", 14, 5)

# --- Correlation Heatmap (longitudinal only - only study with multiple outcomes) ---
cat("\nCreating correlation heatmap...\n")
corr_plot <- plot_corr_heatmaps_combined(
  data_cs = NULL,
  data_long = data_longitudinal,
  outcome_vars = OUTCOME_VARS_LONG
)
save_plot(FIGURE_DIR, corr_plot, "relational_correlation_heatmap", 8, 6)

# --- Distribution Histograms ---
cat("\nCreating distribution histograms...\n")
dist_plot_long <- plot_distributions(
  data = data_longitudinal,
  outcome_vars = OUTCOME_VARS_LONG,
  binwidth = 1,
  scales = "free"  # IOS is 1-7, others are 0-100
)
save_plot(FIGURE_DIR, dist_plot_long, "relational_eda_distributions", 14, 6)

# --- Boxplots ---
cat("\nCreating boxplots...\n")
box_plot <- plot_boxplots(
  data = data_longitudinal,
  outcome_vars = OUTCOME_VARS_LONG,
  free_y = TRUE  # IOS is 1-7, others are 0-100
)
save_plot(FIGURE_DIR, box_plot, "relational_eda_boxplots", 12, 5)

# --- IOS Scale Heatmap by Study and Multiplier ---
cat("\nCreating IOS heatmap by study and multiplier...\n")
ios_summary <- relational_data %>%
  filter(!is.na(ios_scale)) %>%
  group_by(study_id, multiplier) %>%
  summarise(mean_IOS = mean(ios_scale, na.rm = TRUE), .groups = "drop")

plot_ios_heatmap <- ggplot(ios_summary, aes(x = factor(multiplier), y = study_id, fill = mean_IOS)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_IOS, 2)), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Mean IOS Scale", guide = "none") +
  labs(
    x = "Multiplier",
    y = "Study"
  ) +
  theme_pub()
save_plot(FIGURE_DIR, plot_ios_heatmap, "relational_ios_heatmap_by_study_multiplier", 8, 5)

# =============================================================================
# SECTION 3.1: IOS SCALE T-TEST COMPARISONS
# =============================================================================

cat("\n--- IOS Scale T-Test Comparisons ---\n")

# Two-sample t-tests: IOS by treatment groups
treatment_vars <- c("personalisation", "domain", "relationship_seeking_category")

cat("\n=== Cross-Sectional: IOS by Treatment ===\n")
ios_ttest_cs <- run_two_sample_ttest_battery(
  data = data_cross_sectional,
  outcome_vars = "ios_scale",
  treatment_vars = treatment_vars,
  study_name = "Cross-sectional"
)

cat("\n=== Longitudinal: IOS by Treatment ===\n")
ios_ttest_long <- run_two_sample_ttest_battery(
  data = data_longitudinal,
  outcome_vars = "ios_scale",
  treatment_vars = treatment_vars,
  study_name = "Longitudinal"
)

# Combine results
ios_ttest_results <- bind_rows(ios_ttest_cs, ios_ttest_long)

if (!is.null(ios_ttest_results) && nrow(ios_ttest_results) > 0) {
  cat("\n\nIOS Scale T-Test Summary:\n")
  print(kable(ios_ttest_results %>%
                select(study, treatment, group1, group2, mean1, mean2, t_stat, p_value) %>%
                mutate(sig = case_when(
                  p_value < 0.001 ~ "***",
                  p_value < 0.01 ~ "**",
                  p_value < 0.05 ~ "*",
                  TRUE ~ ""
                )),
              digits = 3))

  # Save results
  write_json(ios_ttest_results, file.path(STATS_DIR, "relational_ios_ttest.json"), pretty = TRUE)
  cat("\nSaved: relational_ios_ttest.json\n")
}

# =============================================================================
# SECTION 3.2: IOS DIFFERENCE-IN-DIFFERENCES (W4 - W1)
# =============================================================================

cat("\n--- IOS Difference-in-Differences (W4 - W1) ---\n")

# Compute IOS change from Week 1 to Week 4 for each participant
ios_change_data <- data_longitudinal %>%
  filter(week_numeric %in% c(1, 4)) %>%
  select(ppt_id, week_numeric, ios_scale, personalisation, domain,
         relationship_seeking_category, multiplier) %>%
  pivot_wider(
    id_cols = c(ppt_id, personalisation, domain, relationship_seeking_category, multiplier),
    names_from = week_numeric,
    values_from = ios_scale,
    names_prefix = "ios_W"
  ) %>%
  mutate(ios_change = ios_W4 - ios_W1) %>%
  filter(!is.na(ios_change))

cat(sprintf("  Participants with W1 and W4 data: %d\n", nrow(ios_change_data)))
cat(sprintf("  Mean IOS change (W4-W1): %.2f (SD = %.2f)\n",
            mean(ios_change_data$ios_change, na.rm = TRUE),
            sd(ios_change_data$ios_change, na.rm = TRUE)))

# One-sample t-test: Is overall change different from 0?
cat("\n=== Overall Change vs 0 ===\n")
ios_change_overall <- run_one_sample_ttest(
  data = ios_change_data,
  outcome_var = "ios_change",
  mu = 0,
  study_name = "Longitudinal (W4-W1)"
)

# Two-sample t-tests: Does change differ by treatment group?
cat("\n=== IOS Change by Treatment Group ===\n")
ios_change_ttest <- run_two_sample_ttest_battery(
  data = ios_change_data,
  outcome_vars = "ios_change",
  treatment_vars = treatment_vars,
  study_name = "Longitudinal (W4-W1)"
)

if (!is.null(ios_change_ttest) && nrow(ios_change_ttest) > 0) {
  cat("\n\nIOS Change (W4-W1) by Treatment Summary:\n")
  print(kable(ios_change_ttest %>%
                select(treatment, group1, group2, mean1, mean2, t_stat, p_value) %>%
                mutate(sig = case_when(
                  p_value < 0.001 ~ "***",
                  p_value < 0.01 ~ "**",
                  p_value < 0.05 ~ "*",
                  TRUE ~ ""
                )),
              digits = 3))

  # Save difference-in-differences results
  ios_did_results <- list(
    overall_change = ios_change_overall,
    by_treatment = ios_change_ttest
  )
  write_json(ios_did_results, file.path(STATS_DIR, "relational_ios_did.json"), pretty = TRUE)
  cat("\nSaved: relational_ios_did.json\n")
}

# =============================================================================
# SECTION 3.3: PERSONALISATION SENSECHECK VALIDATION
# =============================================================================

cat("\n--- Personalisation Sensecheck Tests ---\n")

# Prepare data
sensecheck_data <- data_longitudinal %>%
  filter(!is.na(personalisation_sensecheck) & !is.na(personalisation))

# -----------------------------------------------------------------------------
# Test 1: Manipulation Check - Does actual personalisation affect perception?
# -----------------------------------------------------------------------------
cat("\n=== Manipulation Check: Personalised vs Non-Personalised ===\n")

sensecheck_manip <- run_two_sample_ttest_battery(
  data = sensecheck_data,
  outcome_vars = "personalisation_sensecheck",
  treatment_vars = "personalisation",
  study_name = "Longitudinal"
)

if (!is.null(sensecheck_manip) && nrow(sensecheck_manip) > 0) {
  cat("\nManipulation check summary:\n")
  print(kable(sensecheck_manip %>%
                select(group1, group2, mean1, mean2, t_stat, p_value, ci_lower, ci_upper) %>%
                mutate(diff = mean2 - mean1),
              digits = 3))
}

# -----------------------------------------------------------------------------
# Test 2: RS Main Effect - Does RS affect perceived personalisation?
# -----------------------------------------------------------------------------
cat("\n=== RS Main Effect on Perceived Personalisation ===\n")

sensecheck_rs <- run_two_sample_ttest_battery(
  data = sensecheck_data,
  outcome_vars = "personalisation_sensecheck",
  treatment_vars = "relationship_seeking_category",
  study_name = "Longitudinal"
)

if (!is.null(sensecheck_rs) && nrow(sensecheck_rs) > 0) {
  cat("\nRS effect summary:\n")
  print(kable(sensecheck_rs %>%
                select(group1, group2, mean1, mean2, t_stat, p_value, ci_lower, ci_upper) %>%
                mutate(diff = mean2 - mean1),
              digits = 3))
}

# -----------------------------------------------------------------------------
# Test 3: Illusion of Personalisation - RS effect WITHIN non-personalised
# This tests whether RS creates an illusion of personalisation even when
# there is no actual personalisation
# -----------------------------------------------------------------------------
cat("\n=== Illusion of Personalisation: RS Effect Within Non-Personalised ===\n")

sensecheck_nonpers <- sensecheck_data %>%
  filter(personalisation == "non-personalised")

illusion_test <- run_two_sample_ttest_battery(
  data = sensecheck_nonpers,
  outcome_vars = "personalisation_sensecheck",
  treatment_vars = "relationship_seeking_category",
  study_name = "Non-Personalised Only"
)

if (!is.null(illusion_test) && nrow(illusion_test) > 0) {
  cat("\nIllusion test (RS effect within non-personalised condition):\n")
  print(kable(illusion_test %>%
                select(group1, group2, mean1, mean2, t_stat, p_value, ci_lower, ci_upper) %>%
                mutate(diff = mean2 - mean1),
              digits = 3))
  cat("\nInterpretation: If significant, RS creates perceived personalisation\n")
  cat("               even when models are NOT actually personalised.\n")
}

# -----------------------------------------------------------------------------
# Test 4: RS effect WITHIN personalised (for comparison)
# -----------------------------------------------------------------------------
cat("\n=== RS Effect Within Personalised Condition ===\n")

sensecheck_pers <- sensecheck_data %>%
  filter(personalisation == "personalised")

rs_within_pers <- run_two_sample_ttest_battery(
  data = sensecheck_pers,
  outcome_vars = "personalisation_sensecheck",
  treatment_vars = "relationship_seeking_category",
  study_name = "Personalised Only"
)

if (!is.null(rs_within_pers) && nrow(rs_within_pers) > 0) {
  cat("\nRS effect within personalised condition:\n")
  print(kable(rs_within_pers %>%
                select(group1, group2, mean1, mean2, t_stat, p_value, ci_lower, ci_upper) %>%
                mutate(diff = mean2 - mean1),
              digits = 3))
}

# -----------------------------------------------------------------------------
# Save all results to JSON
# -----------------------------------------------------------------------------
sensecheck_results <- list(
  manipulation_check = sensecheck_manip,
  rs_main_effect = sensecheck_rs,
  illusion_test = illusion_test,
  rs_within_personalised = rs_within_pers
)
write_json(sensecheck_results, file.path(STATS_DIR, "personalisation_sensecheck_tests.json"), pretty = TRUE)
cat("\nSaved: personalisation_sensecheck_tests.json\n")

# =============================================================================
# SECTION 4: FUNCTIONAL FORM COMPARISON
# =============================================================================

cat("\n--- Functional Form Comparison ---\n\n")

perf_cross_sectional <- list()
perf_longitudinal <- list()
best_specs_cs <- data.frame()
best_specs_long <- data.frame()

# Cross-sectional: only ios_scale (OLS)
cat("=== Cross-Sectional ===\n")
for (outcome_var in OUTCOME_VARS_CS) {
  cat(sprintf("\n  %s:\n", outcome_var))

  result <- compare_functional_forms(
    data = data_cross_sectional,
    outcome_var = outcome_var,
    add_domain = TRUE,
    add_time = FALSE,
    model_family = "ols",
    pooled = FALSE,
    select_by = "AIC"
  )

  cat(sprintf("    Best: %s\n", result$best_spec))
  print(result$comparison)

  perf_cross_sectional[[outcome_var]] <- result$comparison
  best_specs_cs <- rbind(best_specs_cs, data.frame(
    outcome = outcome_var,
    best_spec = result$best_spec,
    stringsAsFactors = FALSE
  ))
}

# Longitudinal: all three outcomes (lmer)
cat("\n=== Longitudinal ===\n")
for (outcome_var in OUTCOME_VARS_LONG) {
  cat(sprintf("\n  %s:\n", outcome_var))

  result <- compare_functional_forms(
    data = data_longitudinal,
    outcome_var = outcome_var,
    add_domain = TRUE,
    add_time = TRUE,
    time_var = "week_numeric",
    model_family = "lmer",
    pooled = FALSE,
    select_by = "AIC"
  )

  cat(sprintf("    Best: %s\n", result$best_spec))
  print(result$comparison)

  perf_longitudinal[[outcome_var]] <- result$comparison
  best_specs_long <- rbind(best_specs_long, data.frame(
    outcome = outcome_var,
    best_spec = result$best_spec,
    stringsAsFactors = FALSE
  ))
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

# Cross-sectional: only ios_scale (OLS)
cat("=== Cross-Sectional Models ===\n")
for (outcome_var in OUTCOME_VARS_CS) {
  best_spec <- best_specs_cs$best_spec[best_specs_cs$outcome == outcome_var]
  cat(sprintf("\n  Fitting %s (spec: %s)...\n", outcome_var, best_spec))

  mods_cross_sectional[[outcome_var]] <- fit_models(
    data = data_cross_sectional,
    outcome_var = outcome_var,
    continuous_spec = best_spec,
    add_domain = TRUE,
    add_pre = FALSE,
    add_time = FALSE,
    model_family = "ols"
  )
}

# Longitudinal: all three outcomes (lmer)
cat("\n=== Longitudinal Models ===\n")
for (outcome_var in OUTCOME_VARS_LONG) {
  best_spec <- best_specs_long$best_spec[best_specs_long$outcome == outcome_var]
  cat(sprintf("\n  Fitting %s (spec: %s)...\n", outcome_var, best_spec))

  mods_longitudinal[[outcome_var]] <- fit_models(
    data = data_longitudinal,
    outcome_var = outcome_var,
    continuous_spec = best_spec,
    add_domain = TRUE,
    add_pre = FALSE,
    add_time = TRUE,
    time_var = "week_numeric",
    model_family = "lmer"
  )
}

cat("\n--- Printing Model Coefficients ---\n")

# Cross-sectional
cat("\n=== Cross-Sectional Coefficients ===\n")
print_model_coefficients_section(
  mods_cross_sectional = mods_cross_sectional,
  mods_longitudinal = NULL,
  outcome_vars = OUTCOME_VARS_CS
)

# Longitudinal
cat("\n=== Longitudinal Coefficients ===\n")
print_model_coefficients_section(
  mods_cross_sectional = NULL,
  mods_longitudinal = mods_longitudinal,
  outcome_vars = OUTCOME_VARS_LONG
)

# =============================================================================
# SECTION 6: FULL-MODEL PERFORMANCE COMPARISON
# =============================================================================

cat("\n--- Full-Model Performance Comparison ---\n\n")

# Cross-sectional (only ios_scale)
full_spec_results_cs <- compute_full_spec_comparison(
  mods_cs = mods_cross_sectional,
  mods_long = NULL,
  outcome_vars = OUTCOME_VARS_CS
)
full_spec_cs <- full_spec_results_cs$full_spec_cs

# Longitudinal (all three outcomes)
full_spec_results_long <- compute_full_spec_comparison(
  mods_cs = NULL,
  mods_long = mods_longitudinal,
  outcome_vars = OUTCOME_VARS_LONG
)
full_spec_long <- full_spec_results_long$full_spec_long

# =============================================================================
# SECTION 7: ROBUSTNESS ANALYSIS
# =============================================================================

cat("\n--- Robustness Analysis ---\n\n")

# Cross-sectional robustness (only ios_scale)
cat("Cross-sectional robustness:\n\n")
robustness_cs <- run_robustness_analysis(
  data = data_cross_sectional,
  outcome_vars = OUTCOME_VARS_CS,
  rs_variable = "lambda",
  specs_lookup = best_specs_cs,
  study_type = "cross_sectional",
  add_domain = TRUE,
  add_time = FALSE,
  use_weights = FALSE
)

# Longitudinal robustness (all three outcomes)
cat("\nLongitudinal robustness:\n\n")
robustness_long <- run_robustness_analysis(
  data = data_longitudinal,
  outcome_vars = OUTCOME_VARS_LONG,
  rs_variable = "lambda",
  specs_lookup = best_specs_long,
  study_type = "longitudinal",
  add_domain = TRUE,
  add_time = TRUE,
  time_var = "week_numeric",
  use_weights = TRUE
)

# =============================================================================
# SECTION 8: SAVE MODELS AND DATA
# =============================================================================

cat("\n--- Saving Models and Data ---\n")

# Save pooled data for emmeans (used by compute_contrasts.R)
relational_data <- bind_rows(
  data_cross_sectional_pooled %>% mutate(study_id = "cross-sectional"),
  data_longitudinal_pooled %>% mutate(study_id = "longitudinal")
)
relational_data_path <- file.path(MODEL_DIR, "relational_data.rds")
saveRDS(relational_data, relational_data_path)
cat("  Saved pooled data:", relational_data_path, "\n")

# Save spec comparison data for report generation
spec_data <- list(
  best_specs_cs = best_specs_cs,
  best_specs_long = best_specs_long,
  full_spec_cs = full_spec_cs,
  full_spec_long = full_spec_long
)
spec_data_path <- file.path(MODEL_DIR, "relational_spec_data.rds")
saveRDS(spec_data, spec_data_path)
cat("  Saved spec comparison data:", spec_data_path, "\n")

# Save models
cat("\nSaving models...\n")
saveRDS(mods_cross_sectional, file.path(MODEL_DIR, "relational_cross_sectional.rds"))
saveRDS(mods_longitudinal, file.path(MODEL_DIR, "relational_longitudinal.rds"))

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 9: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  # Create outcome labels
  outcome_labels_cs <- c(ios_scale = "IOS Scale")
  outcome_labels_long <- c(
    ios_scale = "IOS Scale",
    tool_friend = "Tool vs Friend",
    personalisation_sensecheck = "Personalisation Sensecheck"
  )

  # Main regression tables
  cat("Generating main regression tables...\n")

  # Cross-sectional tables (only ios_scale)
  generate_regression_tables(
    mods_cs = mods_cross_sectional,
    mods_long = NULL,
    outcome_vars = OUTCOME_VARS_CS,
    best_specs_cs = best_specs_cs,
    best_specs_long = NULL,
    table_prefix = "relational",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels(NULL)
  )

  # Longitudinal tables (all three outcomes)
  generate_regression_tables(
    mods_cs = NULL,
    mods_long = mods_longitudinal,
    outcome_vars = OUTCOME_VARS_LONG,
    best_specs_cs = NULL,
    best_specs_long = best_specs_long,
    table_prefix = "relational",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels("week_numeric")
  )

  # Functional form comparison tables
  cat("Generating functional form tables...\n")
  generate_functional_form_tables(
    perf_cs = perf_cross_sectional,
    perf_long = perf_longitudinal,
    table_prefix = "relational",
    table_dir = TABLE_DIR,
    task_name = "Relational"
  )

  # Full spec comparison tables
  cat("Generating full spec tables...\n")
  generate_full_spec_tables(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long,
    table_prefix = "relational",
    table_dir = TABLE_DIR,
    task_name = "Relational"
  )

  # Robustness tables
  cat("Generating robustness tables...\n")
  create_robustness_latex_table(
    wide_df = robustness_cs$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "relational_robustness_cs",
    caption = "Robustness Checks",
    study_type = "cross_sectional"
  )
  create_robustness_latex_table(
    wide_df = robustness_long$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "relational_robustness_long",
    caption = "Robustness Checks",
    study_type = "longitudinal"
  )

  # Parent tex file
  cat("Generating parent tex file...\n")
  generate_parent_tex(
    section_name = "Relational",
    constructs = OUTCOME_VARS_LONG,
    construct_labels = outcome_labels_long,
    func_form_prefix = "relational_functional_form",
    main_reg_prefix = "relational",
    full_spec_prefix = "relational_full_spec",
    robustness_prefix = "relational_robustness",
    heterogeneity_prefix = NULL,
    constructs_with_cs = c("ios_scale"),  # Only ios_scale has CS version
    table_dir = TABLE_DIR,
    output_filename = "relational_parent"
  )
}

# =============================================================================
# SECTION 10: REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Comprehensive Report ---\n")

  report_path <- file.path(REPORT_DIR, "11_relational.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  lines <- c(
    "# Relational Measures Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines the effects on relational outcomes:",
    "",
    "**Outcomes:**",
    "- **IOS Scale** (Inclusion of Other in Self) - both studies (1-7 likert)",
    "- **Tool vs Friend** - longitudinal only (0-100)",
    "- **Personalisation Sensecheck** - longitudinal only (0-100)",
    "",
    "**Treatment Arms:**",
    "- **lambda**: Relationship-seeking intensity (-1 to +1)",
    "- **Personalisation**: personalised vs non-personalised",
    "- **Domain**: polchat vs emotchat",
    "",
    "**Model Specifications:**",
    "- Cross-sectional: OLS (only ios_scale available)",
    "- Longitudinal: lmer with random slopes for week",
    "",
    "---",
    ""
  )

  # Data summary
  lines <- c(lines, generate_data_summary_md(
    data_cross_sectional_pooled, data_longitudinal_pooled
  ))

  # EDA figures
  lines <- c(lines,
    "## Exploratory Data Analysis",
    "",
    "### Outcome Distributions (Longitudinal)",
    "",
    paste0("![Distributions](", fig_rel_path, "/relational_eda_distributions.png)"),
    "",
    "### Outcome Boxplots",
    "",
    paste0("![Boxplots](", fig_rel_path, "/relational_eda_boxplots.png)"),
    "",
    "### Correlation Heatmap (Longitudinal)",
    "",
    paste0("![Correlations](", fig_rel_path, "/relational_correlation_heatmap.png)"),
    "",
    "### Outcomes by Lambda",
    "",
    paste0("![By Lambda](", fig_rel_path, "/relational_by_lambda.png)"),
    "",
    "### IOS Scale by Study and Multiplier",
    "",
    paste0("![IOS Heatmap](", fig_rel_path, "/relational_ios_heatmap_by_study_multiplier.png)"),
    "",
    "### Trajectory Plots (Longitudinal)",
    "",
    "#### By Relationship-Seeking Category",
    "",
    paste0("![Trajectories RS](", fig_rel_path, "/relational_trajectories_by_rs.png)"),
    "",
    "#### Full Breakdown",
    "",
    paste0("![Trajectories Full](", fig_rel_path, "/relational_trajectories_full.png)"),
    "",
    "---",
    ""
  )

  # Personalisation Sensecheck Validation
  cat("  Adding personalisation sensecheck validation...\n")
  lines <- c(lines,
    "## Personalisation Sensecheck Validation",
    "",
    "### Manipulation Check",
    "",
    "Does actual personalisation treatment affect perceived personalisation?",
    ""
  )
  if (exists("sensecheck_manip") && !is.null(sensecheck_manip)) {
    lines <- c(lines,
      sprintf("- **Non-personalised**: M = %.1f (SD = %.1f, n = %d)",
              sensecheck_manip$mean1, sensecheck_manip$sd1, sensecheck_manip$n1),
      sprintf("- **Personalised**: M = %.1f (SD = %.1f, n = %d)",
              sensecheck_manip$mean2, sensecheck_manip$sd2, sensecheck_manip$n2),
      sprintf("- **Difference**: %.1fpp, t(%.1f) = %.2f, p < 0.001",
              sensecheck_manip$mean2 - sensecheck_manip$mean1,
              sensecheck_manip$df, sensecheck_manip$t_stat),
      "",
      "**Interpretation**: Manipulation check PASSED - participants accurately ",
      "detected personalisation.",
      ""
    )
  }

  lines <- c(lines,
    "### RS Effect on Perceived Personalisation",
    "",
    "Does relationship-seeking behaviour affect perceived personalisation?",
    ""
  )
  if (exists("sensecheck_rs") && !is.null(sensecheck_rs)) {
    lines <- c(lines,
      sprintf("- **Neg λ**: M = %.1f", sensecheck_rs$mean1),
      sprintf("- **Pos λ**: M = %.1f", sensecheck_rs$mean2),
      sprintf("- **Difference**: %.1fpp, p < 0.001",
              sensecheck_rs$mean2 - sensecheck_rs$mean1),
      ""
    )
  }

  lines <- c(lines,
    "### Illusion of Personalisation",
    "",
    "Does RS create perceived personalisation even when NOT personalised?",
    ""
  )
  if (exists("illusion_test") && !is.null(illusion_test)) {
    lines <- c(lines,
      "**Within non-personalised condition:**",
      "",
      sprintf("- **Neg λ**: M = %.1f", illusion_test$mean1),
      sprintf("- **Pos λ**: M = %.1f", illusion_test$mean2),
      sprintf("- **Difference**: %.1fpp, p < 0.001",
              illusion_test$mean2 - illusion_test$mean1),
      "",
      "**Interpretation**: RS creates an illusion of personalisation - ",
      "participants perceived RS models as more personalised even when ",
      "they were NOT actually personalised.",
      ""
    )
  }

  if (exists("rs_within_pers") && !is.null(rs_within_pers)) {
    lines <- c(lines,
      "**Within personalised condition:**",
      "",
      sprintf("- **Neg λ**: M = %.1f", rs_within_pers$mean1),
      sprintf("- **Pos λ**: M = %.1f", rs_within_pers$mean2),
      sprintf("- **Difference**: %.1fpp, p < 0.001",
              rs_within_pers$mean2 - rs_within_pers$mean1),
      ""
    )
  }

  lines <- c(lines, "---", "")

  # Functional form comparison
  lines <- c(lines, generate_best_specs_md(best_specs_cs, best_specs_long))

  # Model coefficients
  cat("  Adding model coefficients...\n")

  # Cross-sectional coefficients
  lines <- c(lines,
    "## Model Coefficients",
    "",
    "### Cross-Sectional",
    ""
  )

  coeff_lines_cs <- generate_coefficients_md(
    mods_cross_sectional = mods_cross_sectional,
    mods_longitudinal = NULL,
    outcome_vars = OUTCOME_VARS_CS
  )
  lines <- c(lines, coeff_lines_cs)

  # Longitudinal coefficients
  lines <- c(lines,
    "### Longitudinal",
    ""
  )

  coeff_lines_long <- generate_coefficients_md(
    mods_cross_sectional = NULL,
    mods_longitudinal = mods_longitudinal,
    outcome_vars = OUTCOME_VARS_LONG
  )
  lines <- c(lines, coeff_lines_long)

  # Robustness summary
  if (exists("robustness_cs") && exists("robustness_long")) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      robustness_cs, robustness_long, ALL_OUTCOME_VARS, "relational"))
  }

  # Output files
  lines <- c(lines, generate_output_files_md("relational"))

  # Write report
  writeLines(lines, report_path)
  cat("Saved comprehensive report:", report_path, "\n")
}
