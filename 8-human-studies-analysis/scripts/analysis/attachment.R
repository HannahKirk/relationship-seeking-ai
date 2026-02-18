#!/usr/bin/env Rscript
# =============================================================================
# Attachment Analysis
# =============================================================================
#
# Regression analysis of attachment outcomes grouped into 4 constructs:
#   - reliance (pooled: behavioural_reliance, cognitive_reliance)
#   - perceived_understanding (pooled: connection, responsiveness, understanding)
#   - self_disclosure (single outcome)
#   - separation_distress (single outcome)
#
# Usage:
#   Rscript scripts/analysis/attachment.R                        # Core analysis only
#   Rscript scripts/analysis/attachment.R --generate_tex_tables  # Also generate LaTeX tables
#   Rscript scripts/analysis/attachment.R --generate_report      # Also generate markdown report
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
cat("Attachment Analysis\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Define all outcome variables (individual level)
ALL_OUTCOME_VARS <- c(
  "behavioural_reliance", "cognitive_reliance",
  "connection", "responsiveness", "understanding",
  "self_disclosure", "separation_distress"
)

# Set seed
set.seed(1234)

# =============================================================================
# SECTION 2: DATA PREPARATION
# =============================================================================

cat("\n--- Loading Data ---\n\n")

attach_raw <- load_task_data("attachment", DATA_DIR)
attach_data <- prepare_treatment_arms(attach_raw) %>%
  filter(study_id %in% c("cross-sectional", "longitudinal"))

# Split by study type
data_cross_sectional <- attach_data %>% filter(study_id == "cross-sectional")
data_longitudinal <- attach_data %>% filter(study_id == "longitudinal")

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

# =============================================================================
# SECTION 2.1: DEFINE CONSTRUCTS
# =============================================================================

cat("\n--- Defining Construct Specifications ---\n\n")

construct_specs <- list(
  reliance = list(
    outcomes = c("behavioural_reliance", "cognitive_reliance"),
    pooled = TRUE,
    heterogeneity = TRUE # This indicates whether we test for heterogeneity
  ),
  perceived_understanding = list(
    outcomes = c("connection", "responsiveness", "understanding"),
    pooled = TRUE,
    heterogeneity = TRUE # This indicates whether we test for heterogeneity
  ),
  self_disclosure = list(
    outcomes = c("self_disclosure"),
    pooled = FALSE,
    heterogeneity = FALSE
  ),
  separation_distress = list(
    outcomes = c("separation_distress"),
    pooled = FALSE,
    heterogeneity = FALSE
  )
)

CONSTRUCT_NAMES <- names(construct_specs)

# =============================================================================
# SECTION 2.2: CREATE POOLED DATA
# =============================================================================

cat("\n--- Creating Pooled Data ---\n\n")

# Define mapping from outcome variables to constructs
CONSTRUCT_MAPPING <- c(
  "behavioural_reliance" = "reliance",
  "cognitive_reliance" = "reliance",
  "connection" = "perceived_understanding",
  "responsiveness" = "perceived_understanding",
  "understanding" = "perceived_understanding",
  "self_disclosure" = "self_disclosure",
  "separation_distress" = "separation_distress"
)

# Create combined pooled data using create_pooled_data()
data_cross_sectional_pooled <- create_pooled_data(
  data_cross_sectional, ALL_OUTCOME_VARS, CONSTRUCT_MAPPING
)
data_longitudinal_pooled <- create_pooled_data(
  data_longitudinal, ALL_OUTCOME_VARS, CONSTRUCT_MAPPING
)

# Split into per-construct lists for model fitting
pooled_cs_list <- split(data_cross_sectional_pooled,
                        data_cross_sectional_pooled$construct, drop = TRUE)
pooled_long_list <- split(data_longitudinal_pooled,
                          data_longitudinal_pooled$construct, drop = TRUE)

# Print summary
for (cn in CONSTRUCT_NAMES) {
  cat(sprintf("  %s - CS: %d rows, Long: %d rows\n",
              cn, nrow(pooled_cs_list[[cn]]), nrow(pooled_long_list[[cn]])))
}

# Create version with all 7 outcomes stacked 1:1 (for EDA across all outcomes)
data_cs_all_outcomes <- create_pooled_data(data_cross_sectional, ALL_OUTCOME_VARS)
data_long_all_outcomes <- create_pooled_data(data_longitudinal, ALL_OUTCOME_VARS)

cat("\n  Combined pooled CS:", nrow(data_cross_sectional_pooled), "rows\n")
cat("  Combined pooled Long:", nrow(data_longitudinal_pooled), "rows\n")

# =============================================================================
# SECTION 3: EXPLORATORY DATA ANALYSIS
# =============================================================================

cat("\n--- Exploratory Data Analysis ---\n\n")

# Summary by study and lambda
cat("\nSummary statistics by study and lambda:\n")
summary_by_lambda <- attach_data %>%
  group_by(study_id, lambda) %>%
  summarise(
    across(all_of(ALL_OUTCOME_VARS),
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE))),
    n = n(),
    .groups = "drop"
  )
print(summary_by_lambda)

# Outcome correlations
cat("\nOutcome correlations (cross-sectional):\n")
print(round(cor(data_cross_sectional[, ALL_OUTCOME_VARS], use = "pairwise.complete.obs"), 3))

cat("\nOutcome correlations (longitudinal):\n")
print(round(cor(data_longitudinal[, ALL_OUTCOME_VARS], use = "pairwise.complete.obs"), 3))

# Trajectory plots (longitudinal only - use week_numeric)
cat("\nCreating trajectory plots...\n")

traj_plot_rs <- plot_raw_trends(
  data_long_all_outcomes,
  time_var = "week_numeric",
  facet_vars = c("relationship_seeking_category"),
  plot_smoothed = FALSE,
  height = 6, width = 14
)
save_plot(FIGURE_DIR, traj_plot_rs, "attachment_trajectories_by_rs", 14, 10)

traj_plot_full <- plot_raw_trends(
  data_long_all_outcomes,
  time_var = "week_numeric",
  facet_vars = c("relationship_seeking_category", "personalisation"),
  plot_smoothed = FALSE,
  height = 10, width = 14
)
save_plot(FIGURE_DIR, traj_plot_full, "attachment_trajectories_full", 14, 10)

# --- Raw Lambda Plot ---
cat("\nCreating raw lambda plots...\n")
lambda_plot <- plot_raw_lambda(
  data_long = data_long_all_outcomes,
  data_cross = data_cs_all_outcomes,
  plot_study = "both",
  plot_smoothed = FALSE,
  scales = "free"
)
save_plot(FIGURE_DIR, lambda_plot, "attachment_by_lambda", 14, 10)

# --- Correlation Heatmaps ---
cat("\nCreating correlation heatmaps...\n")
corr_plot <- plot_corr_heatmaps_combined(
  data_cs = data_cross_sectional,
  data_long = data_longitudinal,
  outcome_vars = ALL_OUTCOME_VARS
)
save_plot(FIGURE_DIR, corr_plot, "attachment_correlation_heatmap", 14, 7)

# --- Distribution Histograms ---
cat("\nCreating distribution histograms...\n")
dist_plot <- plot_distributions(
  data = attach_data,
  outcome_vars = ALL_OUTCOME_VARS,
  binwidth = 10
)
save_plot(FIGURE_DIR, dist_plot, "attachment_eda_distributions", 16, 8)

# --- Boxplots ---
cat("\nCreating boxplots...\n")
box_plot <- plot_boxplots(
  data = attach_data,
  outcome_vars = ALL_OUTCOME_VARS
)
save_plot(FIGURE_DIR, box_plot, "attachment_eda_boxplots", 18, 6)

# =============================================================================
# SECTION 4: HETEROGENEITY TESTS
# =============================================================================

cat("\n--- Heterogeneity Tests ---\n\n")

heterogeneity_results <- list()

for (cn in CONSTRUCT_NAMES) {
  spec <- construct_specs[[cn]]
  if (!spec$heterogeneity) next

  cat(sprintf("\n=== Heterogeneity Test: %s ===\n", cn))

  for (study_type in c("cross_sectional", "longitudinal")) {
    study_label <- ifelse(study_type == "cross_sectional", "Cross-Sectional", "Longitudinal")
    cat(sprintf("\n  %s:\n", study_label))

    # Get the right pooled data
    if (study_type == "cross_sectional") {
      data_pooled <- pooled_cs_list[[cn]]
      add_time <- FALSE
      time_var <- NULL
    } else {
      data_pooled <- pooled_long_list[[cn]]
      add_time <- TRUE
      time_var <- "week_numeric"
    }

    # Fit homogeneous model (no heterogeneity interactions)
    formula_homo <- build_formula_pooled(
      rs_variable = "lambda",
      model_spec = "full",
      continuous_model_spec = "linear",
      heterogeneity = FALSE,
      add_domain = TRUE,
      add_time = add_time,
      time_var = time_var
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
      add_time = add_time,
      time_var = time_var
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
                       "HETEROGENEOUS (outcomes differ in treatment effects, treatment interactions needed)",
                       "HOMOGENEOUS (no treatment interactions is justified)")))

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
jsonlite::write_json(het_summary, file.path(STATS_DIR, "attachment_heterogeneity.json"),
                     pretty = TRUE, auto_unbox = TRUE)
cat("\nSaved heterogeneity results to:", file.path(STATS_DIR, "attachment_heterogeneity.json"), "\n")

# Build lookup: should this construct × study_id use heterogeneity in models?
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
# SECTION 5: FUNCTIONAL FORM COMPARISON
# =============================================================================

cat("\n--- Functional Form Comparison ---\n\n")

# Uses compare_functional_forms() for all constructs (pooled and non-pooled).
# Selects best specification by AIC.

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
        add_time = (study_type == "longitudinal"),
        time_var = if (study_type == "longitudinal") "week_numeric" else NULL,
        model_family = "lmer",
        pooled = TRUE,
        heterogeneity = use_het,
        select_by = "AIC"
      )
    } else {
      # Non-pooled: use standard formula
      outcome_var <- spec$outcomes[1]
      result <- compare_functional_forms(
        data = if (study_type == "cross_sectional") {
          data_cross_sectional
        } else { data_longitudinal },
        outcome_var = outcome_var,
        add_domain = TRUE,
        add_time = (study_type == "longitudinal"),
        time_var = if (study_type == "longitudinal") "week_numeric" else NULL,
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
# SECTION 6: FIT MODELS
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
      # best_spec_cs is already scalar from [1] indexing above
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

    # --- Longitudinal (pooled: lmer with random slopes for week) ---
    het_long <- isTRUE(use_heterogeneity[[paste(cn, "longitudinal", sep = "_")]])
    cat(sprintf("\n  Longitudinal (pooled, heterogeneity=%s):\n", het_long))
    long_mods <- list()
    for (mod_name in names(rs_variables)) {
      cat(sprintf("    Fitting %s...\n", mod_name))
      rs_info <- rs_variables[[mod_name]]

      # best_spec_long is already scalar from [1] indexing above
      cont_spec <- if (rs_info$rs == "lambda") best_spec_long else "linear"

      formula <- build_formula_pooled(
        rs_variable = rs_info$rs,
        model_spec = rs_info$model_spec,
        continuous_model_spec = cont_spec,
        heterogeneity = het_long,
        add_domain = TRUE,
        add_time = TRUE,
        time_var = "week_numeric"
      )
      long_mods[[mod_name]] <- run_reg_model(
        pooled_long_list[[cn]], formula,
        model_family = "lmer", REML = TRUE
      )
    }
    mods_longitudinal[[cn]] <- long_mods

  } else {
    # Non-pooled constructs: use fit_models() directly
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
      add_time = TRUE,
      time_var = "week_numeric",
      model_family = "lmer"
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
# SECTION 7: PERFORMANCE COMPARISON
# =============================================================================

cat("\n--- Full-Model Performance Comparison ---\n\n")

full_spec_results <- compute_full_spec_comparison(
  mods_cross_sectional, mods_longitudinal, CONSTRUCT_NAMES
)
full_spec_cs <- full_spec_results$full_spec_cs
full_spec_long <- full_spec_results$full_spec_long


# =============================================================================
# SECTION 8: ROBUSTNESS CHECKS
# =============================================================================

cat("\n--- Robustness Analysis ---\n\n")

# Split constructs into pooled vs non-pooled
pooled_constructs <- names(construct_specs)[
  sapply(construct_specs, function(x) x$pooled)
]
nonpooled_constructs <- names(construct_specs)[
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

if (length(nonpooled_constructs) > 0) {
  cat("  Non-pooled constructs:\n")
  robustness_cs_nonpooled <- run_robustness_analysis(
    data = data_cross_sectional,
    outcome_vars = nonpooled_constructs,
    rs_variable = "lambda",
    specs_lookup = best_specs_cs,
    study_type = "cross_sectional",
    add_domain = TRUE,
    add_time = FALSE,
    use_weights = FALSE
  )
}

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
    use_weights = FALSE
  )
}

# --- Longitudinal robustness ---
cat("\nLongitudinal robustness:\n\n")

robustness_long_nonpooled <- NULL
robustness_long_pooled <- NULL

if (length(nonpooled_constructs) > 0) {
  cat("  Non-pooled constructs:\n")
  robustness_long_nonpooled <- run_robustness_analysis(
    data = data_longitudinal,
    outcome_vars = nonpooled_constructs,
    rs_variable = "lambda",
    specs_lookup = best_specs_long,
    study_type = "longitudinal",
    add_domain = TRUE,
    add_time = TRUE,
    time_var = "week_numeric",
    use_weights = TRUE
  )
}

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
    add_time = TRUE,
    time_var = "week_numeric",
    use_weights = TRUE
  )
}

# Combine pooled and non-pooled robustness results for report
# Use conditional access to avoid NULL$coeffs_wide crash
robustness_cs <- list(coeffs_wide = dplyr::bind_rows(
  if (!is.null(robustness_cs_nonpooled)) robustness_cs_nonpooled$coeffs_wide,
  if (!is.null(robustness_cs_pooled)) robustness_cs_pooled$coeffs_wide
))
robustness_long <- list(coeffs_wide = dplyr::bind_rows(
  if (!is.null(robustness_long_nonpooled)) robustness_long_nonpooled$coeffs_wide,
  if (!is.null(robustness_long_pooled)) robustness_long_pooled$coeffs_wide
))

# =============================================================================
# SECTION 9: SAVE
# =============================================================================

cat("\n--- Saving Models and Data for Contrast Analysis ---\n")

# Save pooled data for emmeans (used by compute_contrasts.R)
# Combine cross-sectional and longitudinal with study_id indicator
attachment_data <- bind_rows(
  data_cross_sectional_pooled,
  data_longitudinal_pooled
)

attachment_data_path <- file.path(MODEL_DIR, "attachment_data.rds")
saveRDS(attachment_data, attachment_data_path)
cat("  Saved pooled data:", attachment_data_path, "\n")

# Save spec comparison data for report generation
spec_data <- list(
  best_specs_cs = best_specs_cs,
  best_specs_long = best_specs_long,
  full_spec_cs = full_spec_cs,
  full_spec_long = full_spec_long,
  construct_specs = construct_specs,
  heterogeneity_results = heterogeneity_results
)
spec_data_path <- file.path(MODEL_DIR, "attachment_spec_data.rds")
saveRDS(spec_data, spec_data_path)
cat("  Saved spec comparison data:", spec_data_path, "\n")


# Save models
cat("\nSaving models...\n")
saveRDS(mods_cross_sectional, file.path(MODEL_DIR, "attachment_cross_sectional.rds"))
saveRDS(mods_longitudinal, file.path(MODEL_DIR, "attachment_longitudinal.rds"))

cat("\nANALYSIS COMPLETE\n")

# =============================================================================
# SECTION 10: LATEX TABLES
# =============================================================================

if (generate_tex_tables) {
  cat("\n--- Generating LaTeX Tables ---\n\n")

  construct_display <- setNames(
    gsub("_", " ", tools::toTitleCase(CONSTRUCT_NAMES)),
    CONSTRUCT_NAMES
  )

  # Functional form tables
  generate_functional_form_tables(
    perf_cs = perf_cross_sectional,
    perf_long = perf_longitudinal,
    table_prefix = "attachment",
    table_dir = TABLE_DIR,
    task_name = "Attachment"
  )

  # Main regression tables
  reference_outcomes <- list(
    reliance = "Behav. Reliance",
    perceived_understanding = "Connection"
  )
  generate_regression_tables(
    mods_cs = mods_cross_sectional,
    mods_long = mods_longitudinal,
    outcome_vars = CONSTRUCT_NAMES,
    best_specs_cs = best_specs_cs,
    best_specs_long = best_specs_long,
    table_prefix = "attachment",
    table_dir = TABLE_DIR,
    pred_labels = get_pred_labels("week_numeric"),
    reference_outcomes = reference_outcomes
  )

  # Full spec comparison tables
  generate_full_spec_tables(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long,
    table_prefix = "attachment",
    table_dir = TABLE_DIR,
    task_name = "Attachment"
  )

  # Robustness tables (use already-combined robustness_cs and robustness_long)
  create_robustness_latex_table(
    wide_df = robustness_cs$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "attachment_robustness_cs",
    caption = "Robustness Checks",
    study_type = "cross_sectional"
  )
  create_robustness_latex_table(
    wide_df = robustness_long$coeffs_wide,
    table_dir = TABLE_DIR,
    filename_prefix = "attachment_robustness_long",
    caption = "Robustness Checks",
    study_type = "longitudinal"
  )

  # Heterogeneity test table
  het_construct_labels <- c(
    reliance = "Reliance",
    perceived_understanding = "Perceived Understanding"
  )
  create_heterogeneity_latex(
    het_results = heterogeneity_results,
    construct_labels = het_construct_labels,
    table_dir = TABLE_DIR,
    filename = "attachment_heterogeneity",
    caption = "Heterogeneity Tests (Attachment)",
    label = "tab:attachment_heterogeneity"
  )

  # Parent tex file
  generate_parent_tex(
    section_name = "Attachment",
    constructs = CONSTRUCT_NAMES,
    construct_labels = construct_display,
    func_form_prefix = "attachment_functional_form",
    main_reg_prefix = "attachment",
    full_spec_prefix = "attachment_full_spec",
    robustness_prefix = "attachment_robustness",
    heterogeneity_prefix = "attachment_heterogeneity",
    table_dir = TABLE_DIR,
    output_filename = "attachment_parent"
  )
}

# =============================================================================
# SECTION 11: REPORT
# =============================================================================

if (generate_report) {
  cat("\n--- Generating Summary Report ---\n")

  report_path <- file.path(REPORT_DIR, "06_attachment.md")
  fig_rel_path <- "../../outputs/figures/main_studies/png"

  # Header and overview
  lines <- c(
    "# Attachment Analysis",
    "",
    paste0("*Generated: ", Sys.time(), "*"),
    "",
    "## Overview",
    "",
    "This analysis examines effects on attachment outcomes.",
    "",
    "**Constructs:** Reliance, Perceived Understanding, Self Disclosure,",
    "Separation Distress",
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
    paste0("![Boxplots](", fig_rel_path, "/attachment_eda_boxplots.png)"),
    "",
    paste0("![Distributions](", fig_rel_path,
           "/attachment_eda_distributions.png)"),
    "",
    paste0("![Correlations](", fig_rel_path,
           "/attachment_correlation_heatmap.png)"),
    "",
    paste0("![Trajectories](", fig_rel_path,
           "/attachment_trajectories_by_rs.png)"),
    "",
    paste0("![Outcomes by Lambda](", fig_rel_path,
           "/attachment_by_lambda.png)"),
    "",
    paste0("*Note: Cross-sectional and longitudinal values are not directly ",
           "comparable. Cross-sectional items measure overall expectations ",
           "of the relationship, while longitudinal items are context-specific ",
           "to each week. Only the shape of the relationship with lambda, not ",
           "the absolute values, should be compared across studies.*"),
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
      mods_cross_sectional, mods_longitudinal, CONSTRUCT_NAMES),
      "", "---", "")
  }


  # Robustness summary
  if (exists("robustness_cs") && exists("robustness_long")) {
    cat("  Adding robustness summary...\n")
    lines <- c(lines, generate_robustness_summary_md(
      robustness_cs, robustness_long, CONSTRUCT_NAMES, "attachment"))
  }

  # Output files
  lines <- c(lines, generate_output_files_md("attachment"))

  writeLines(lines, report_path)
  cat("Saved report:", report_path, "\n")
}

