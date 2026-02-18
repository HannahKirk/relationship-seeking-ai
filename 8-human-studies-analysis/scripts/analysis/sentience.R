#!/usr/bin/env Rscript
# =============================================================================
# AI Sentience Perception Analysis
# =============================================================================
#
# Analyzes AI sentience perception measures:
#   - Continuous: ontological_sentience, perceived_sentience, pain, pleasure,
#                 awareness, emotions (+ confidence measures for EDA only)
#   - Categorical: change_sentience (recoded), future_view
#
# Key characteristics:
#   - OLS regression (single time point, post-treatment only)
#   - Domain effects included (add_domain = TRUE)
#   - No time effects (add_time = FALSE) - single measurement
#   - No pre-treatment control (add_pre = FALSE) - post-only data
#   - Heterogeneity testing for ontological_sentience construct (5 pooled outcomes)
#
# Constructs:
#   - perceived_sentience: single outcome (not pooled)
#   - ontological_sentience: pooled from ontological_sentience, pain, pleasure,
#                            emotions, awareness
#
# Usage:
#   Rscript scripts/analysis/sentience.R
#   Rscript scripts/analysis/sentience.R --generate_tex_tables
#   Rscript scripts/analysis/sentience.R --generate_report
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
source(file.path(PROJECT_ROOT, "scripts/utils_r/eda_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/regression_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/model_comparison_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/regressions/robustness_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/extract_coefficients.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/sjplot_to_latex.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/latex_utils.R"))
source(file.path(PROJECT_ROOT, "scripts/utils_r/report_utils.R"))

cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("AI Sentience Perception Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define outcome variables (all 6 individual measures for EDA)
ALL_OUTCOME_VARS <- c(
  "ontological_sentience", "perceived_sentience",
  "pain", "pleasure", "awareness", "emotions"
)
CONFIDENCE_VARS <- c("ontological_sentience_confidence", "perceived_sentience_confidence")
CATEGORICAL_VARS <- c("change_sentience_recoded")

# Define constructs for pooled analysis
# - ontological_sentience: pooled from 5 items (heterogeneity tested)
# - perceived_sentience: single item (not pooled)
construct_specs <- list(
  ontological_sentience = list(
    outcomes = c("ontological_sentience", "pain", "pleasure", "emotions", "awareness"),
    pooled = TRUE,
    heterogeneity = TRUE # This means we test for heterogeneity and include interactions if needed
  ),
  perceived_sentience = list(
    outcomes = c("perceived_sentience"),
    pooled = FALSE,
    heterogeneity = FALSE
  )
)

CONSTRUCT_NAMES <- names(construct_specs)

cat("Constructs defined:\n")
for (cn in CONSTRUCT_NAMES) {
  spec <- construct_specs[[cn]]
  cat(sprintf("  %s: %s (pooled=%s, heterogeneity=%s)\n",
              cn,
              paste(spec$outcomes, collapse = ", "),
              spec$pooled,
              spec$heterogeneity))
}

# Set seed
set.seed(1234)

# =============================================================================
# SECTION 2: DATA PREPARATION
# =============================================================================

cat("\n--- Loading Data ---\n\n")

sentience_raw <- load_task_data("sentience-measures", DATA_DIR)
sentience_data <- prepare_treatment_arms(sentience_raw) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

# Split by study type
data_cross_sectional <- sentience_data %>% filter(study_id == "cross-sectional")
data_longitudinal <- sentience_data %>% filter(study_id == "longitudinal")

cat("  Cross-sectional:", nrow(data_cross_sectional), "obs from",
    n_distinct(data_cross_sectional$ppt_id), "participants\n")
cat("  Longitudinal:", nrow(data_longitudinal), "obs from",
    n_distinct(data_longitudinal$ppt_id), "participants\n")

# Load participant characteristics (sociodemos, pref types, IPW weights)
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

# Define construct mapping: which outcomes belong to which construct
# ontological_sentience construct pools 5 outcomes
# perceived_sentience is a single outcome (maps to itself)
CONSTRUCT_MAPPING <- c(
  ontological_sentience = "ontological_sentience",
  pain = "ontological_sentience",
  pleasure = "ontological_sentience",
  emotions = "ontological_sentience",
  awareness = "ontological_sentience",
  perceived_sentience = "perceived_sentience"
)

# Create pooled data using the standard function
data_cross_sectional_pooled <- create_pooled_data(
  data_cross_sectional,
  outcome_vars = ALL_OUTCOME_VARS,
  construct_mapping = CONSTRUCT_MAPPING
)

data_longitudinal_pooled <- create_pooled_data(
  data_longitudinal,
  outcome_vars = ALL_OUTCOME_VARS,
  construct_mapping = CONSTRUCT_MAPPING
)

# Split by construct for models that need per-construct data
pooled_cs_list <- split(data_cross_sectional_pooled,
                        data_cross_sectional_pooled$construct, drop = TRUE)
pooled_long_list <- split(data_longitudinal_pooled,
                          data_longitudinal_pooled$construct, drop = TRUE)

# Print summary
for (cn in CONSTRUCT_NAMES) {
  cat(sprintf("  %s - CS: %d rows, Long: %d rows\n",
              cn, nrow(pooled_cs_list[[cn]]), nrow(pooled_long_list[[cn]])))
}

cat("\n  Combined pooled CS:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Combined pooled Long:", nrow(data_longitudinal_pooled), "rows\n")


# =============================================================================
# SECTION 3: EDA
# =============================================================================

cat("\n--- Processing Categorical Variables ---\n\n")

# Recode change_sentience to 3 levels
recode_change_sentience <- function(data) {
  data %>%
    mutate(
      change_sentience_recoded = case_when(
        change_sentience == "No change in my view" ~ "No change",
        change_sentience %in% c(
          "Somewhat more conscious than I thought before",
          "Much more conscious than I thought before"
        ) ~ "More conscious",
        change_sentience %in% c(
          "Somewhat less conscious than I thought before",
          "Much less conscious than I thought before"
        ) ~ "Less conscious",
        TRUE ~ NA_character_
      )
    )
}

data_cross_sectional <- recode_change_sentience(data_cross_sectional)
data_longitudinal <- recode_change_sentience(data_longitudinal)

# Define factor levels for categorical variables
CHANGE_LEVELS <- c("Less conscious", "No change", "More conscious")
FUTURE_VIEW_LEVELS <- c(
 "AI assistants will never have feelings or experiences",
  "I'm not sure",
  "AI assistants may have feelings or experiences one day",
  "AI assistants already have feelings or experiences"
)

# Apply factor levels
data_cross_sectional <- data_cross_sectional %>%
  mutate(
    change_sentience_recoded = factor(change_sentience_recoded, levels = CHANGE_LEVELS),
    future_view = factor(future_view, levels = FUTURE_VIEW_LEVELS)
  )

data_longitudinal <- data_longitudinal %>%
  mutate(
    change_sentience_recoded = factor(change_sentience_recoded, levels = CHANGE_LEVELS),
    future_view = factor(future_view, levels = FUTURE_VIEW_LEVELS)
  )

cat("Change sentience recoded levels:\n")
print(table(data_cross_sectional$change_sentience_recoded, useNA = "ifany"))

cat("\nFuture view levels:\n")
print(table(data_cross_sectional$future_view, useNA = "ifany"))


# =============================================================================
# SECTION 5A: EDA - CATEGORICAL VARIABLES
# =============================================================================

cat("\n--- EDA: Categorical Variables ---\n\n")

# Combine data for categorical plots
data_combined <- bind_rows(data_cross_sectional, data_longitudinal)

# --- Plot 1: Change in Sentience View by Study ---
cat("Creating change_sentience plots...\n")

p_change_by_study <- plot_categorical_by_group(
  data = data_combined,
  response_var = "change_sentience_recoded",
  group_var = "study_id",
  response_levels = CHANGE_LEVELS,
  title = "Perceived Change in AI Sentience View\n(by Study)"
) +
  labs(x = "Percentage of Participants") +
  theme(plot.title = element_text(size = rel(1.0)))

# --- Plot 2: Change in Sentience View by RS Category (longitudinal) ---
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

data_long_rs <- data_longitudinal %>%
  filter(relationship_seeking_category %in% c("pos_lambda", "zero_lambda", "neg_lambda")) %>%
  mutate(relationship_seeking_category = factor(
    relationship_seeking_category,
    levels = c("pos_lambda", "zero_lambda", "neg_lambda")
  ))

p_change_by_rs <- plot_categorical_by_group(
  data = data_long_rs,
  response_var = "change_sentience_recoded",
  group_var = "relationship_seeking_category",
  response_levels = CHANGE_LEVELS,
  title = "Perceived Change in AI Sentience View\n(by Lambda Category)",
  subtitle = "Longitudinal study only",
  colors = rs_colors,
  labels = rs_labels
) +
  labs(x = "Percentage of Participants") +
  theme(plot.title = element_text(size = rel(1.0)))

# Combine and save
p_change_combined <- p_change_by_study | p_change_by_rs
save_plot(FIGURE_DIR, p_change_combined, "sentience_change_in_view", 16, 6)
cat("Saved: sentience_change_in_view.pdf/png\n")

# --- Treatment Tests for Categorical Variables ---
cat("\nRunning treatment tests for categorical variables...\n")

# Use run_standard_treatment_tests for comprehensive testing
# Tests across: study_id, personalisation, domain, relationship_seeking_category, lambda_factor
categorical_test_results <- run_standard_treatment_tests(
  data_stats = data_combined,
  cross_study_outcomes = CATEGORICAL_VARS,
  longitudinal_outcomes = NULL
)

# Save categorical test results
write_json(categorical_test_results, file.path(STATS_DIR, "sentience_categorical_tests.json"), pretty = TRUE)
cat("\nSaved: sentience_categorical_tests.json\n")

# =============================================================================
# SECTION 5B: EDA - CONTINUOUS VARIABLES
# =============================================================================

cat("\n--- EDA: Continuous Variables ---\n\n")

# --- Boxplots by Study (all continuous vars including confidence) ---
cat("Creating distribution boxplots...\n")

all_continuous <- c(ALL_OUTCOME_VARS, CONFIDENCE_VARS)
box_plot <- boxplot_by_study(data_combined, all_continuous, nrow = 2)
save_plot(FIGURE_DIR, box_plot, "sentience_distributions_by_study", 16, 8)
cat("Saved: sentience_distributions_by_study.pdf/png\n")

# --- Distribution Histograms ---
cat("Creating distribution histograms...\n")
dist_plot <- plot_distributions(
  data = data_combined,
  outcome_vars = ALL_OUTCOME_VARS
)
save_plot(FIGURE_DIR, dist_plot, "sentience_distributions", 14, 8)

# --- Correlation Heatmaps (main continuous vars only, no confidence) ---
cat("Creating correlation heatmaps...\n")

corr_plot <- plot_corr_heatmaps_combined(
  data_cs = data_cross_sectional,
  data_long = data_longitudinal,
  outcome_vars = ALL_OUTCOME_VARS
)
save_plot(FIGURE_DIR, corr_plot, "sentience_correlation_heatmap", 14, 7)
cat("Saved: sentience_correlation_heatmap.pdf/png\n")

# --- Raw Lambda Plots ---
cat("Creating raw lambda plot...\n")

p_lambda <- plot_raw_lambda(
  data_long = data_longitudinal_pooled,
  data_cross = data_cross_sectional_pooled,
  plot_study = "both",
  plot_smoothed = FALSE,
  height = 6, width = 14
)
save_plot(FIGURE_DIR, p_lambda, "sentience_by_lambda", 14, 6)
cat("Saved: sentience_by_lambda.pdf/png\n")

# =============================================================================
# SECTION 6: HETEROGENEITY TESTS
# =============================================================================

cat("\n--- Heterogeneity Tests ---\n\n")

heterogeneity_results <- list()

# Test heterogeneity for each construct that has heterogeneity = TRUE
for (cn in CONSTRUCT_NAMES) {
  spec <- construct_specs[[cn]]
  if (!spec$heterogeneity) next

  cat(sprintf("\n=== Heterogeneity Test: %s ===\n", cn))

  for (study_type in c("cross_sectional", "longitudinal")) {
    study_label <- ifelse(study_type == "cross_sectional", "Cross-Sectional", "Longitudinal")
    cat(sprintf("\n  %s:\n", study_label))

    # Get pooled data for this construct
    if (study_type == "cross_sectional") {
      data_pooled <- pooled_cs_list[[cn]]
    } else {
      data_pooled <- pooled_long_list[[cn]]
    }

    # Fit homogeneous model (no heterogeneity interactions)
    formula_homo <- build_formula_pooled(
      rs_variable = "lambda",
      model_spec = "full",
      continuous_model_spec = "linear",
      heterogeneity = FALSE,
      add_domain = TRUE,
      add_time = FALSE
    )
    model_homo <- run_reg_model(data_pooled, formula_homo, model_family = "lmer",
                                 REML = FALSE, verbose = FALSE)

    # Fit heterogeneous model (with outcome_measure × treatment interactions)
    formula_hetero <- build_formula_pooled(
      rs_variable = "lambda",
      model_spec = "full",
      continuous_model_spec = "linear",
      heterogeneity = TRUE,
      add_domain = TRUE,
      add_time = FALSE
    )
    model_hetero <- run_reg_model(data_pooled, formula_hetero, model_family = "lmer",
                                   REML = FALSE, verbose = FALSE)

    # Likelihood ratio test
    lr_test <- anova(model_homo, model_hetero)
    cat("  LR Test:\n")
    print(lr_test)

    p_value <- lr_test$`Pr(>Chisq)`[2]
    cat(sprintf("  p-value: %.4f - %s\n",
                p_value,
                ifelse(p_value < 0.05,
                       "HETEROGENEOUS (outcomes differ, interactions needed)",
                       "HOMOGENEOUS (no interactions needed)")))

    heterogeneity_results[[paste(cn, study_type, sep = "_")]] <- list(
      construct = cn,
      study_type = study_type,
      lr_test = lr_test,
      p_value = p_value,
      heterogeneous = p_value < 0.05
    )
  }
}

# Save heterogeneity results
het_summary <- lapply(heterogeneity_results, function(x) {
  list(
    construct = x$construct,
    study_type = x$study_type,
    p_value = x$p_value,
    heterogeneous = x$heterogeneous
  )
})
jsonlite::write_json(
  het_summary,
  file.path(STATS_DIR, "sentience_heterogeneity.json"),
  pretty = TRUE, auto_unbox = TRUE
)
cat("\nSaved: sentience_heterogeneity.json\n")

# Build lookup: should this construct × study_type use heterogeneity in models?
use_heterogeneity <- list()
for (key in names(heterogeneity_results)) {
  hr <- heterogeneity_results[[key]]
  use_heterogeneity[[key]] <- hr$heterogeneous
}
cat("\nHeterogeneity decisions for model fitting:\n")
for (key in names(use_heterogeneity)) {
  cat(sprintf("  %s: heterogeneity=%s\n", key, use_heterogeneity[[key]]))
}

# =============================================================================
# SECTION 7: FUNCTIONAL FORM COMPARISON
# =============================================================================

cat("\n--- Functional Form Comparison ---\n\n")

# Uses compare_functional_forms() for all constructs (pooled and non-pooled).
# Selects best specification by AIC.
# Note: Sentience is post-treatment only (no time variable).

perf_cross_sectional <- list()
perf_longitudinal <- list()
best_specs_cs <- data.frame()
best_specs_long <- data.frame()

for (cn in CONSTRUCT_NAMES) {
  spec <- construct_specs[[cn]]
  cat(sprintf("\n=== %s ===\n", cn))

  for (study_type in c("cross_sectional", "longitudinal")) {
    study_label <- ifelse(
      study_type == "cross_sectional", "Cross-Sectional", "Longitudinal")
    cat(sprintf("\n  %s:\n", study_label))

    if (spec$pooled) {
      # Pooled: use pooled data and build_formula_pooled
      het_key <- paste(cn, study_type, sep = "_")
      use_het <- isTRUE(use_heterogeneity[[het_key]])
      cat(sprintf("    (pooled, heterogeneity=%s)\n", use_het))

      if (study_type == "cross_sectional") {
        pool_data <- pooled_cs_list[[cn]]
      } else {
        pool_data <- pooled_long_list[[cn]]
      }

      result <- compare_functional_forms(
        data = pool_data,
        outcome_var = "outcome_value",
        add_domain = TRUE,
        add_time = FALSE,  # Single timepoint
        model_family = "lmer",
        pooled = TRUE,
        heterogeneity = use_het,
        select_by = "AIC"
      )
    } else {
      # Non-pooled: use standard formula (OLS)
      outcome_var <- spec$outcomes[1]
      result <- compare_functional_forms(
        data = if (study_type == "cross_sectional") {
          data_cross_sectional
        } else { data_longitudinal },
        outcome_var = outcome_var,
        add_domain = TRUE,
        add_time = FALSE,  # Single timepoint
        model_family = "ols",
        pooled = FALSE,
        select_by = "AIC"
      )
    }

    cat(sprintf("    Best: %s\n", result$best_spec))
    print(result$comparison)

    if (study_type == "cross_sectional") {
      perf_cross_sectional[[cn]] <- result$comparison
      best_specs_cs <- rbind(best_specs_cs, data.frame(
        outcome = cn,
        best_spec = result$best_spec,
        stringsAsFactors = FALSE
      ))
    } else {
      perf_longitudinal[[cn]] <- result$comparison
      best_specs_long <- rbind(best_specs_long, data.frame(
        outcome = cn,
        best_spec = result$best_spec,
        stringsAsFactors = FALSE
      ))
    }
  }
}

cat("\nBest specifications (Cross-Sectional, by AIC):\n")
print(best_specs_cs)
cat("\nBest specifications (Longitudinal, by AIC):\n")
print(best_specs_long)

# =============================================================================
# SECTION 8: FIT MODELS
# =============================================================================

cat("\n--- Fitting Models ---\n\n")

mods_cross_sectional <- list()
mods_longitudinal <- list()

for (cn in CONSTRUCT_NAMES) {
  spec <- construct_specs[[cn]]
  best_spec_cs <- best_specs_cs$best_spec[best_specs_cs$outcome == cn][1]
  best_spec_long <- best_specs_long$best_spec[best_specs_long$outcome == cn][1]

  cat(sprintf("\n=== Fitting models for: %s ===\n", cn))
  cat(sprintf("  Using %s for cross-sectional, %s for longitudinal\n", best_spec_cs, best_spec_long))

  if (spec$pooled) {
    # Pooled constructs: use build_formula_pooled + run_reg_model directly
    # We need to fit 6 model specs: additive/full × continuous/coarsened/5level
    rs_variables <- list(
      additive_continuous = list(rs = "lambda", model_spec = "additive"),
      full_continuous = list(rs = "lambda", model_spec = "full"),
      additive_coarsened = list(rs = "relationship_seeking_category", model_spec = "additive"),
      full_coarsened = list(rs = "relationship_seeking_category", model_spec = "full"),
      additive_5level = list(rs = "lambda_factor", model_spec = "additive"),
      full_5level = list(rs = "lambda_factor", model_spec = "full")
    )

    # --- Cross-sectional (pooled: lmer with random intercept for ppt) ---
    het_cs <- isTRUE(use_heterogeneity[[paste(cn, "cross_sectional", sep = "_")]])
    cat(sprintf("\n  Cross-sectional (pooled, heterogeneity=%s):\n", het_cs))
    cs_mods <- list()
    for (mod_name in names(rs_variables)) {
      cat(sprintf("    Fitting %s...\n", mod_name))
      rs_info <- rs_variables[[mod_name]]

      # Determine continuous spec for this model
      cont_spec <- if (rs_info$rs == "lambda") best_spec_cs else "linear"

      formula <- build_formula_pooled(
        rs_variable = rs_info$rs,
        model_spec = rs_info$model_spec,
        continuous_model_spec = cont_spec,
        heterogeneity = het_cs,
        add_domain = TRUE,
        add_time = FALSE
      )
      cs_mods[[mod_name]] <- run_reg_model(
        pooled_cs_list[[cn]], formula,
        model_family = "lmer", REML = TRUE
      )
    }
    mods_cross_sectional[[cn]] <- cs_mods

    # --- Longitudinal (pooled: lmer with random intercept for ppt) ---
    het_long <- isTRUE(use_heterogeneity[[paste(cn, "longitudinal", sep = "_")]])
    cat(sprintf("\n  Longitudinal (pooled, heterogeneity=%s):\n", het_long))
    long_mods <- list()
    for (mod_name in names(rs_variables)) {
      cat(sprintf("    Fitting %s...\n", mod_name))
      rs_info <- rs_variables[[mod_name]]

      cont_spec <- if (rs_info$rs == "lambda") best_spec_long else "linear"

      formula <- build_formula_pooled(
        rs_variable = rs_info$rs,
        model_spec = rs_info$model_spec,
        continuous_model_spec = cont_spec,
        heterogeneity = het_long,
        add_domain = TRUE,
        add_time = FALSE  # Single timepoint
      )
      long_mods[[mod_name]] <- run_reg_model(
        pooled_long_list[[cn]], formula,
        model_family = "lmer", REML = TRUE
      )
    }
    mods_longitudinal[[cn]] <- long_mods

  } else {
    # Non-pooled constructs: use fit_models() directly (OLS)
    outcome_var <- spec$outcomes[1]

    cat("\n  Cross-sectional:\n")
    mods_cross_sectional[[cn]] <- fit_models(
      data = data_cross_sectional,
      outcome_var = outcome_var,
      continuous_spec = best_spec_cs,
      add_domain = TRUE,
      add_pre = FALSE,
      add_time = FALSE,
      model_family = "ols"
    )

    cat("\n  Longitudinal:\n")
    mods_longitudinal[[cn]] <- fit_models(
      data = data_longitudinal,
      outcome_var = outcome_var,
      continuous_spec = best_spec_long,
      add_domain = TRUE,
      add_pre = FALSE,
      add_time = FALSE,
      model_family = "ols"
    )
  }
}

cat("\n--- Printing Model Coefficients ---\n")

print_model_coefficients_section(
  mods_cross_sectional = mods_cross_sectional,
  mods_longitudinal = mods_longitudinal,
  outcome_vars = CONSTRUCT_NAMES
)

# =============================================================================
# SECTION 9: FULL-MODEL SPECIFICATION COMPARISON
# =============================================================================

cat("\n--- Full-Model Specification Comparison ---\n\n")

full_spec_results <- compute_full_spec_comparison(
  mods_cross_sectional, mods_longitudinal, CONSTRUCT_NAMES
)

full_spec_cs <- full_spec_results$full_spec_cs
full_spec_long <- full_spec_results$full_spec_long

# =============================================================================
# SECTION 10: ROBUSTNESS ANALYSIS
# =============================================================================
#
# Robustness analysis for sentience constructs.
# Pooled construct (ontological_sentience) uses run_robustness_analysis_pooled.
# Non-pooled construct (perceived_sentience) uses run_robustness_analysis.
#
# =============================================================================

cat("\n--- Robustness Analysis ---\n\n")

# Identify pooled vs non-pooled constructs
pooled_constructs <- names(construct_specs)[
  sapply(construct_specs, function(x) x$pooled)
]
non_pooled_constructs <- names(construct_specs)[
  sapply(construct_specs, function(x) !x$pooled)
]

# Build heterogeneity lookup for pooled constructs
het_lookup_cs <- list()
het_lookup_long <- list()
for (cn in pooled_constructs) {
  het_lookup_cs[[cn]] <- isTRUE(
    use_heterogeneity[[paste(cn, "cross_sectional", sep = "_")]]
  )
  het_lookup_long[[cn]] <- isTRUE(
    use_heterogeneity[[paste(cn, "longitudinal", sep = "_")]]
  )
}

# --- Cross-sectional robustness ---
cat("Cross-sectional robustness:\n\n")

robustness_cs_nonpooled <- NULL
robustness_cs_pooled <- NULL

# Non-pooled constructs (OLS)
if (length(non_pooled_constructs) > 0) {
  cat("  Non-pooled constructs:\n")
  robustness_cs_nonpooled <- run_robustness_analysis(
    data = data_cross_sectional,
    outcome_vars = sapply(
      non_pooled_constructs, function(cn) construct_specs[[cn]]$outcomes[1]),
    rs_variable = "lambda",
    specs_lookup = best_specs_cs,
    study_type = "cross_sectional",
    add_domain = TRUE,
    add_time = FALSE,
    use_weights = TRUE, # Sentience is measured in exit study
    model_family = "ols"
  )
}

# Pooled constructs (lmer)
if (length(pooled_constructs) > 0) {
  cat("\n  Pooled constructs:\n")
  robustness_cs_pooled <- run_robustness_analysis_pooled(
    data_list = pooled_cs_list[pooled_constructs],
    construct_names = pooled_constructs,
    heterogeneity_lookup = het_lookup_cs,
    rs_variable = "lambda",
    specs_lookup = best_specs_cs,
    study_type = "cross_sectional",
    add_domain = TRUE,
    add_time = FALSE,
    use_weights = TRUE # Sentience is measured in exit study
  )
}

# Combine pooled and non-pooled robustness results for report
# Use conditional access to avoid NULL$coeffs_wide crash
robustness_cs <- list(coeffs_wide = dplyr::bind_rows(
  if (!is.null(robustness_cs_nonpooled)) robustness_cs_nonpooled$coeffs_wide,
  if (!is.null(robustness_cs_pooled)) robustness_cs_pooled$coeffs_wide
))

# --- Longitudinal robustness (with IPW) ---
cat("\nLongitudinal robustness:\n\n")

robustness_long_nonpooled <- NULL
robustness_long_pooled <- NULL

# Non-pooled constructs (OLS)
if (length(non_pooled_constructs) > 0) {
  cat("  Non-pooled constructs:\n")
  robustness_long_nonpooled <- run_robustness_analysis(
    data = data_longitudinal,
    outcome_vars = sapply(
      non_pooled_constructs, function(cn) construct_specs[[cn]]$outcomes[1]),
    rs_variable = "lambda",
    specs_lookup = best_specs_long,
    study_type = "longitudinal",
    add_domain = TRUE,
    add_time = FALSE,
    use_weights = TRUE,
    model_family = "ols"
  )
}

# Pooled constructs (lmer with IPW)
if (length(pooled_constructs) > 0) {
  cat("\n  Pooled constructs:\n")
  robustness_long_pooled <- run_robustness_analysis_pooled(
    data_list = pooled_long_list[pooled_constructs],
    construct_names = pooled_constructs,
    heterogeneity_lookup = het_lookup_long,
    rs_variable = "lambda",
    specs_lookup = best_specs_long,
    study_type = "longitudinal",
    add_domain = TRUE,
    add_time = FALSE,
    use_weights = TRUE
  )
}

# Combine pooled and non-pooled robustness results for report
robustness_long <- list(coeffs_wide = dplyr::bind_rows(
  if (!is.null(robustness_long_nonpooled)) robustness_long_nonpooled$coeffs_wide,
  if (!is.null(robustness_long_pooled)) robustness_long_pooled$coeffs_wide
))

# =============================================================================
# SECTION 11: SAVE MODELS AND DATA
# =============================================================================

cat("\n--- Saving Models and Data ---\n")

# Save models
saveRDS(mods_cross_sectional, file.path(MODEL_DIR, "sentience_cross_sectional.rds"))
saveRDS(mods_longitudinal, file.path(MODEL_DIR, "sentience_longitudinal.rds"))
cat("  Saved: sentience_cross_sectional.rds\n")
cat("  Saved: sentience_longitudinal.rds\n")

# Save pooled data
sentience_data <- bind_rows(
  data_cross_sectional_pooled %>% mutate(study_id = "cross-sectional"),
  data_longitudinal_pooled %>% mutate(study_id = "longitudinal")
)
saveRDS(sentience_data, file.path(MODEL_DIR, "sentience_data.rds"))
cat("  Saved: sentience_data.rds\n")

# Save spec comparison data (include construct specs and heterogeneity)
spec_data <- list(
  best_specs_cs = best_specs_cs,
  best_specs_long = best_specs_long,
  full_spec_cs = full_spec_cs,
  full_spec_long = full_spec_long,
  construct_specs = construct_specs,
  heterogeneity_results = heterogeneity_results
)
saveRDS(spec_data, file.path(MODEL_DIR, "sentience_spec_data.rds"))
cat("  Saved: sentience_spec_data.rds\n")

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 12: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  # Heterogeneity table
  het_construct_labels <- c(
    ontological_sentience = "Ontological Sentience (Pooled)"
  )
  create_heterogeneity_latex(
    het_results = heterogeneity_results,
    construct_labels = het_construct_labels,
    table_dir = TABLE_DIR,
    filename = "sentience_heterogeneity",
    caption = "Heterogeneity Tests --- AI Sentience Perception",
    label = "tab:sentience_heterogeneity"
  )

  # Functional form tables
  generate_functional_form_tables(
    perf_cs = perf_cross_sectional,
    perf_long = perf_longitudinal,
    table_prefix = "sentience",
    table_dir = TABLE_DIR,
    task_name = "AI Sentience Perception"
  )

  # Reference outcomes for pooled constructs (first outcome in factor)
  reference_outcomes <- list(
    ontological_sentience = "Ontological Consciousness"
  )

  # Regression tables
  generate_regression_tables(
    mods_cs = mods_cross_sectional,
    mods_long = mods_longitudinal,
    outcome_vars = CONSTRUCT_NAMES,
    best_specs_cs = best_specs_cs,
    best_specs_long = best_specs_long,
    table_prefix = "sentience",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels(NULL),
    reference_outcomes = reference_outcomes
  )

  # Full specification comparison tables
  generate_full_spec_tables(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long,
    table_prefix = "sentience",
    table_dir = TABLE_DIR,
    task_name = "AI Sentience Perception"
  )

  # Robustness tables (already combined in Section 10)
  if (nrow(robustness_cs$coeffs_wide) > 0) {
    create_robustness_latex_table(
      wide_df = robustness_cs$coeffs_wide,
      table_dir = TABLE_DIR,
      filename_prefix = "sentience_robustness_cs",
      caption = "Sentience Robustness",
      study_type = "cross_sectional"
    )
  }
  if (nrow(robustness_long$coeffs_wide) > 0) {
    create_robustness_latex_table(
      wide_df = robustness_long$coeffs_wide,
      table_dir = TABLE_DIR,
      filename_prefix = "sentience_robustness_long",
      caption = "Sentience Robustness",
      study_type = "longitudinal"
    )
  }

  # Chi-square test table (with breakdown for significant results)
  if (exists("categorical_test_results") && length(categorical_test_results) > 0) {
    create_chisq_with_breakdown_latex(
      test_results = categorical_test_results,
      table_dir = TABLE_DIR,
      filename = "sentience_chisq",
      caption = "Sentience: Chi-Square Tests",
      label = "tab:sentience_chisq"
    )
  }

  # Parent TeX file
  generate_parent_tex(
    section_name = "AI Sentience Perception",
    constructs = CONSTRUCT_NAMES,
    construct_labels = setNames(
      c("Ontological Sentience (Pooled)", "Perceived Sentience"),
      CONSTRUCT_NAMES
    ),
    func_form_prefix = "sentience_functional_form",
    main_reg_prefix = "sentience",
    full_spec_prefix = "sentience_full_spec",
    robustness_prefix = "sentience_robustness",
    heterogeneity_prefix = "sentience_heterogeneity",
    table_dir = TABLE_DIR,
    output_filename = "sentience_parent"
  )
  cat("  Saved: sentience_parent.tex\n")
}

# =============================================================================
# SECTION 13: OPTIONAL REPORT GENERATION
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "12_sentience.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  # Header and overview
  lines <- c(
    "# AI Sentience Perception Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines effects on perceptions of AI sentience.",
    "",
    "**Continuous Outcomes:**",
    "- **Ontological Sentience**: Belief about whether AI actually has consciousness",
    "- **Perceived Sentience**: Subjective feeling of AI appearing to have consciousness",
    "- **Pain**: Attribution of capacity for suffering",
    "- **Pleasure**: Attribution of capacity for enjoyment",
    "- **Awareness**: Attribution of awareness/self-awareness",
    "- **Emotions**: Attribution of emotional capacity",
    "- **Confidence**: Confidence in ontological/perceived sentience ratings (EDA only)",
    "",
    "**Categorical Outcomes:**",
    "- **Change in Sentience View**: Whether view changed (less/no change/more conscious)",
    "- **Future View**: Beliefs about AI future sentience",
    "",
    "**Model Specification:**",
    "- OLS regression (single time point, post-treatment only)",
    "- Domain effects included",
    "- No pre-treatment control (post-only measurement)",
    "",
    "**Constructs:**",
    "- **Perceived Sentience**: Single outcome (not pooled)",
    paste0("- **Ontological Sentience**: Pooled from ontological_sentience, ",
           "pain, pleasure, emotions, awareness"),
    "",
    "---",
    ""
  )

  # Data summary
  lines <- c(lines, generate_data_summary_md(
    data_cross_sectional_pooled, data_longitudinal_pooled
  ))

  # Categorical Variables section
  lines <- c(lines,
    "## Categorical Variables",
    "",
    paste0("![Change in Sentience View](", fig_rel_path, "/sentience_change_in_view.png)"),
    "",
    "### Chi-Square Tests for Categorical Variables",
    ""
  )

  # Add categorical test results using utility function
  data_by_study <- list(
    cross_study = data_combined,
    cross_sectional = data_cross_sectional,
    longitudinal = data_longitudinal
  )
  lines <- c(lines,
    format_treatment_tests_md(categorical_test_results, data_by_study))

  lines <- c(lines,
    "---",
    "",
    "## Exploratory Data Analysis",
    "",
    paste0("![Boxplots](", fig_rel_path, "/sentience_distributions_by_study.png)"),
    "",
    paste0("![Distributions](", fig_rel_path, "/sentience_distributions.png)"),
    "",
    paste0("![Outcome Correlations](", fig_rel_path, "/sentience_correlation_heatmap.png)"),
    "",
    paste0("![Outcomes by Lambda](", fig_rel_path, "/sentience_by_lambda.png)"),
    "",
    "---",
    ""
  )

  # Heterogeneity tests
  if (exists("heterogeneity_results") && length(heterogeneity_results) > 0) {
    lines <- c(lines, generate_heterogeneity_md(heterogeneity_results))
  }

  # Functional form comparison
  if (exists("best_specs_cs") && nrow(best_specs_cs) > 0) {
    lines <- c(lines, generate_best_specs_md(best_specs_cs, best_specs_long))
  }

  # Full-model specification comparison
  if (exists("full_spec_cs") && length(full_spec_cs) > 0) {
    lines <- c(lines, generate_full_spec_comparison_md(
      full_spec_cs, full_spec_long, CONSTRUCT_NAMES))
  }

  # Model coefficients
  if (exists("mods_cross_sectional") && exists("mods_longitudinal")) {
    cat("  Adding model coefficients...\n")
    lines <- c(lines, generate_coefficients_md(
      mods_cross_sectional, mods_longitudinal, CONSTRUCT_NAMES), "", "---", "")
  }

  # Robustness summary
  if (exists("robustness_cs") && exists("robustness_long")) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      robustness_cs, robustness_long, CONSTRUCT_NAMES, "sentience"))
  }

  # Output files
  lines <- c(lines, generate_output_files_md("sentience"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}
