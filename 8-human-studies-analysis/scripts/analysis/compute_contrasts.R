#!/usr/bin/env Rscript
# =============================================================================
# Compute Contrasts: Statistical Tests for All Measure Families
# =============================================================================
#
# Computes all contrasts from saved models and outputs structured JSON files.
# Implements hierarchical FDR correction with local and global families.
#
# Usage:
#   Rscript scripts/analysis/compute_contrasts.R --preferences
#   Rscript scripts/analysis/compute_contrasts.R --attachment
#   Rscript scripts/analysis/compute_contrasts.R --psychosocial
#   Rscript scripts/analysis/compute_contrasts.R --momentary-affect
#   Rscript scripts/analysis/compute_contrasts.R --perceptions
#   Rscript scripts/analysis/compute_contrasts.R --all
#
# Output:
#   outputs/stats/{measure_family}_contrasts.json
#
# JSON Structure:
#   {
#     "measure_family": "attachment",
#     "generated": "timestamp",
#     "family_structure": {
#       "local_families": { "attachment_core": [...], "seeking_companionship": [...], ... },
#       "global_family": ["attachment_core", "seeking_companionship", "goodbye"],
#       "global_family_temporal": ["attachment_core"]
#     },
#     "contrasts": [
#       {
#         "test": "RS: Avg(Pos) - Avg(Neg)",
#         "test_type": "main_effect_RS",  # or main_effect_Pers, main_effect_Domain
#         "local_family": "attachment_core",
#         "study_type": "longitudinal",
#         "outcome": "reliance",
#         "primary": { "estimate": ..., "lower_cl": ..., "upper_cl": ..., "p_raw": ..., "p_local": ..., "p_global": ... },
#         "coarsened": { "estimate": ..., "lower_cl": ..., "upper_cl": ..., "p_raw": ... },
#         "narrow": { "estimate": ..., "lower_cl": ..., "upper_cl": ..., "p_raw": ... },
#         "full": { "estimate": ..., "lower_cl": ..., "upper_cl": ..., "p_raw": ... }
#       },
#       ...
#     ]
#   }
#
# FDR Correction Hierarchy:
#   - p_raw: Uncorrected p-value
#   - p_local: FDR-corrected within local family (e.g., attachment_core outcomes)
#   - p_global: FDR-corrected within global family (e.g., all attachment outcomes)
#
# =============================================================================

library(tidyverse)
library(emmeans)
library(lme4)
library(lmerTest)
library(jsonlite)
library(parameters)  # For unified model parameter extraction across model types

set.seed(1234)

# Set emmeans options
emm_options(pbkrtest.limit = 50000, lmerTest.limit = 50000)
emm_options(msg.interaction = FALSE)

# =============================================================================
# SETUP
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
run_preferences <- "--preferences" %in% args || "--all" %in% args
run_attachment <- "--attachment" %in% args || "--all" %in% args
run_psychosocial <- "--psychosocial" %in% args || "--all" %in% args
run_momentary_affect <- "--momentary-affect" %in% args || "--all" %in% args
run_perceptions <- "--perceptions" %in% args || "--all" %in% args
run_will_miss_ai <- "--will_miss_ai" %in% args || "--all" %in% args # Needed for original pre-registration robustness check but not included in main paper
run_psychosocial_individual <- "--psychosocial-individual" %in% args || "--all" %in% args # Needed for original pre-registration robustness check but not included in main paper

# Study type: longitudinal (default) or cross-sectional
# Cross-sectional only generates main effects (X1), no temporal/dose-response/moderation
study_type <- if ("--cross-sectional" %in% args) "cross-sectional" else "longitudinal"
is_cross_sectional <- study_type == "cross-sectional"

if (length(args) == 0) {
  cat("
Compute Contrasts: Statistical Tests

Usage:
  Rscript scripts/analysis/compute_contrasts.R --preferences
  Rscript scripts/analysis/compute_contrasts.R --attachment
  Rscript scripts/analysis/compute_contrasts.R --psychosocial
  Rscript scripts/analysis/compute_contrasts.R --momentary-affect
  Rscript scripts/analysis/compute_contrasts.R --perceptions
  Rscript scripts/analysis/compute_contrasts.R --will_miss_ai
  Rscript scripts/analysis/compute_contrasts.R --psychosocial-individual
  Rscript scripts/analysis/compute_contrasts.R --all

Output: outputs/stats/{measure_family}_contrasts.json (one file per measure family)
")
  quit(status = 0)
}

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

PROJECT_ROOT <- paths$PROJECT_ROOT
MODEL_DIR <- file.path(PROJECT_ROOT, "outputs/models")
STATS_DIR <- file.path(PROJECT_ROOT, "outputs/stats")
dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# MEASURE FAMILY CONFIGURATION
# =============================================================================
# Each measure family contains local families (groups of related outcomes).
# FDR correction is applied hierarchically:
#   - p_local: within local family
#   - p_global: within global family (all local families combined)
#
# Model files are specified per local family. Outcomes are discovered at runtime
# from the keys in the loaded model .rds files.
# =============================================================================

MEASURE_FAMILIES <- list(
  preferences = list(
    local_families = list(
      preferences = list(
        file_cs = "preferences_cross_sectional.rds",
        file_long = "preferences_longitudinal.rds",
        file_data = "preferences_data.rds",
        outcomes = c("likeability", "engagingness", "helpfulness"),
        has_domain = TRUE,
        has_temporal = TRUE
      )
    ),
    time_var = "session_numeric",
    time_points = c(1, 20)
  ),
  attachment = list(
    local_families = list(
      attachment_core = list(
        file_cs = "attachment_cross_sectional.rds",
        file_long = "attachment_longitudinal.rds",
        file_data = "attachment_data.rds",
        outcomes = c("reliance", "perceived_understanding",
                     "self_disclosure", "separation_distress"),
        has_domain = TRUE,
        has_temporal = TRUE
      ),
      seeking_companionship = list(
        file_cs = "seeking_companionship_postonly_cross_sectional.rds",
        file_long = "seeking_companionship_longitudinal.rds",
        file_data = "seeking_companionship_data.rds",
        outcomes = c("seeking_companionship_likelihood"),
        has_domain = TRUE,
        has_temporal = FALSE
      ),
      goodbye = list(
        file_cs = "goodbye_cross_sectional.rds",
        file_long = "goodbye_longitudinal.rds",
        file_data = "goodbye_data.rds",
        outcomes = c("goodbye_action"),
        has_domain = TRUE,
        has_temporal = FALSE
      )
    ),
    time_var = "week_numeric",
    time_points = c(1, 4)
  ),
  psychosocial = list(
    local_families = list(
      psychosocial = list(
        file_cs = "psychosocial_cross_sectional.rds",
        file_long = "psychosocial_longitudinal.rds",
        file_data = "psychosocial_data.rds",
        outcomes = c("psychosocial_F1", "psychosocial_F2"),
        has_domain = TRUE,
        has_temporal = FALSE
      )
    ),
    time_var = NULL,
    time_points = NULL
  ),
  momentary_affect = list(
    local_families = list(
      momentary_affect = list(
        file_cs = "mood_cross_sectional.rds",
        file_long = "mood_longitudinal.rds",
        file_data = "mood_data.rds",
        outcomes = c("valence", "arousal"),
        has_domain = FALSE,  # Emotchat only
        has_temporal = TRUE
      )
    ),
    time_var = "session_numeric",
    time_points = c(1, 20)
  ),
  perceptions = list(
    local_families = list(
      relational = list(
        file_cs = "relational_cross_sectional.rds",
        file_long = "relational_longitudinal.rds",
        file_data = "relational_data.rds",
        outcomes = c("tool_friend"),
        has_domain = TRUE,
        has_temporal = TRUE
      ),
      sentience = list(
        file_cs = "sentience_cross_sectional.rds",
        file_long = "sentience_longitudinal.rds",
        file_data = "sentience_data.rds",
        outcomes = c("ontological_sentience", "perceived_sentience"),
        has_domain = TRUE,
        has_temporal = FALSE
      )
    ),
    time_var = "week_numeric",
    time_points = c(1, 4)
  ),
  # will_miss_ai: pre-registration robustness check, not in main paper
  # Longitudinal study only, single time point (post-survey)
  # Models are saved in post_survey_relational.R
  will_miss_ai = list(
    local_families = list(
      will_miss_ai = list(
        file_cs = NULL,  # No cross-sectional
        file_long = "post_survey_relational_long.rds",
        file_data = "post_survey_relational_data.rds",
        outcomes = c("will_miss_ai"),
        has_domain = TRUE,
        has_temporal = FALSE
      )
    ),
    time_var = NULL,
    time_points = NULL
  ),
  # psychosocial_individual: pre-registration robustness check
  # Individual scales instead of F1/F2 factor scores
  psychosocial_individual = list(
    local_families = list(
      psychosocial_individual = list(
        file_cs = "psychosocial_individual_cross_sectional.rds",
        file_long = "psychosocial_individual_longitudinal.rds",
        file_data = "psychosocial_data.rds",
        outcomes = c("phq_gad_score", "ucla_score", "lubben_score", "who_score"),
        has_domain = TRUE,
        has_temporal = FALSE
      )
    ),
    time_var = NULL,
    time_points = NULL
  )
)

#' Check that all required model files exist
#' @param family_name Name of the measure family
#' @param local_families Named list of local family configs
#' @param model_dir Directory containing model files
#' @return TRUE if all files exist, otherwise stops with error
check_model_files <- function(family_name, local_families, model_dir) {
  missing <- c()
  for (local_name in names(local_families)) {
    cfg <- local_families[[local_name]]
    # Check each file type (skip NULL entries like will_miss_ai's file_cs)
    for (file in c(cfg$file_cs, cfg$file_long, cfg$file_data)) {
      if (!is.null(file)) {
        path <- file.path(model_dir, file)
        if (!file.exists(path)) {
          missing <- c(missing, path)
        }
      }
    }
  }
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing model files for %s:\n  %s\n\nRun regression scripts first.",
      family_name,
      paste(missing, collapse = "\n  ")
    ))
  }
  TRUE
}

# =============================================================================
# HELPER: Clean emmeans result to data frame
# =============================================================================

clean_emmeans <- function(emm_result) {
  df <- as.data.frame(summary(emm_result, infer = TRUE))

  # Standardize column names
  if ("asymp.LCL" %in% names(df)) df <- rename(df, lower.CL = asymp.LCL, upper.CL = asymp.UCL)
  if ("lower.HPD" %in% names(df)) df <- rename(df, lower.CL = lower.HPD, upper.CL = upper.HPD)

  df
}

#' Validate factor levels for coarsened contrasts
#'
#' Coarsened contrasts use weights c(-1, 0, 1) which assume
#' relationship_seeking_category levels are (neg_lambda, zero_lambda, pos_lambda).
#' This function validates that assumption.
#'
#' @param data Data frame with relationship_seeking_category column
#' @return TRUE if valid, otherwise stops with error
validate_rs_category_levels <- function(data) {
  if (!"relationship_seeking_category" %in% names(data)) {
    return(TRUE)  # Column not present, nothing to validate
  }

  expected_levels <- c("neg_lambda", "zero_lambda", "pos_lambda")
  actual_levels <- levels(data$relationship_seeking_category)

  if (!identical(actual_levels, expected_levels)) {
    stop(sprintf(
      "relationship_seeking_category levels are wrong!\n  Expected: %s\n  Actual: %s\n\nCoarsened contrast weights c(-1, 0, 1) assume (neg, zero, pos) order.",
      paste(expected_levels, collapse = ", "),
      paste(actual_levels, collapse = ", ")
    ))
  }
  TRUE
}

# =============================================================================
# CONTRAST EXTRACTORS
# =============================================================================

#' Extract RS main effect with all robustness variants
extract_rs_contrasts <- function(models, data, outcome, study_type, local_family) {

  full_cont <- models$full_continuous
  full_coars <- models$full_coarsened

  result <- list(
    test = "RS: Avg(Pos) - Avg(Neg)",
    test_type = "main_effect_RS",
    local_family = local_family,
    study_type = study_type,
    outcome = outcome
  )

  # PRIMARY: Averaged contrast
  emm_avg <- emmeans(full_cont, ~lambda, at = list(lambda = c(-1, -0.5, 0.5, 1)), data = data)
  contrast_avg <- contrast(emm_avg, method = list("avg(pos) - avg(neg)" = c(-0.5, -0.5, 0.5, 0.5)))
  df_avg <- clean_emmeans(contrast_avg)

  result$primary <- list(
    estimate = df_avg$estimate[1],
    lower_cl = df_avg$lower.CL[1],
    upper_cl = df_avg$upper.CL[1],
    p_raw = df_avg$p.value[1]
  )

  # COARSENED: 3-level categorical
  emm_coars <- emmeans(full_coars, ~relationship_seeking_category)
  contrast_coars <- contrast(emm_coars, method = list("pos - neg" = c(-1, 0, 1)))
  df_coars <- clean_emmeans(contrast_coars)

  result$coarsened <- list(
    estimate = df_coars$estimate[1],
    lower_cl = df_coars$lower.CL[1],
    upper_cl = df_coars$upper.CL[1],
    p_raw = df_coars$p.value[1]
  )

  # NARROW: +/- 0.5
  emm_narrow <- emmeans(full_cont, ~lambda, at = list(lambda = c(-0.5, 0.5)), data = data)
  contrast_narrow <- contrast(emm_narrow, method = "revpairwise")
  df_narrow <- clean_emmeans(contrast_narrow)

  result$narrow <- list(
    estimate = df_narrow$estimate[1],
    lower_cl = df_narrow$lower.CL[1],
    upper_cl = df_narrow$upper.CL[1],
    p_raw = df_narrow$p.value[1]
  )

  # FULL: +/- 1.0
  emm_full <- emmeans(full_cont, ~lambda, at = list(lambda = c(-1, 1)), data = data)
  contrast_full <- contrast(emm_full, method = "revpairwise")
  df_full <- clean_emmeans(contrast_full)

  result$full <- list(
    estimate = df_full$estimate[1],
    lower_cl = df_full$lower.CL[1],
    upper_cl = df_full$upper.CL[1],
    p_raw = df_full$p.value[1]
  )

  result
}

#' Extract binary arm effect (domain or personalisation)
extract_binary_arm_contrasts <- function(models, data, outcome, study_type, arm_name, local_family) {

  full_cont <- models$full_continuous
  full_coars <- models$full_coarsened

  arm_label <- tools::toTitleCase(arm_name)


  # Map arm_name to specific main effect type for FDR correction
  test_type <- if (arm_name == "domain") {
    "main_effect_Domain"
  } else if (arm_name == "personalisation") {
    "main_effect_Pers"
  } else {
    paste0("main_effect_", arm_label)
  }

  result <- list(
    test = paste0(arm_label, ": Contrast"),
    test_type = test_type,
    local_family = local_family,
    study_type = study_type,
    outcome = outcome
  )

  # PRIMARY
  emm_arm <- emmeans(full_cont, as.formula(paste0("~", arm_name)), data = data)
  contrast_arm <- contrast(emm_arm, method = "revpairwise")
  df_arm <- clean_emmeans(contrast_arm)

  result$test <- paste0(arm_label, ": ", df_arm$contrast[1])

  result$primary <- list(
    estimate = df_arm$estimate[1],
    lower_cl = df_arm$lower.CL[1],
    upper_cl = df_arm$upper.CL[1],
    p_raw = df_arm$p.value[1]
  )

  # COARSENED
  emm_coars <- emmeans(full_coars, as.formula(paste0("~", arm_name)))
  contrast_coars <- contrast(emm_coars, method = "revpairwise")
  df_coars <- clean_emmeans(contrast_coars)

  result$coarsened <- list(
    estimate = df_coars$estimate[1],
    lower_cl = df_coars$lower.CL[1],
    upper_cl = df_coars$upper.CL[1],
    p_raw = df_coars$p.value[1]
  )

  # Narrow and Full not applicable for binary arms
  result$narrow <- NULL
  result$full <- NULL

  result
}

#' Extract polynomial (dose-response) coefficients
#' Uses the parameters package for unified extraction across model types
extract_polynomial_contrasts <- function(models, data, outcome, study_type, local_family) {

  additive_cont <- models$additive_continuous

  # Use parameters package to get standardized output across model types
  # This handles lmer (Pr(>|t|)), glm (Pr(>|z|)), etc. automatically
  params <- model_parameters(additive_cont, ci = 0.95)
  params_df <- as.data.frame(params)

  # Find lambda terms
  lambda_rows <- grep("^lambda$|^I\\(lambda", params_df$Parameter)

  results <- list()

  term_names <- c("lambda" = "λ (linear)", "I(lambda^2)" = "λ² (quadratic)", "I(lambda^3)" = "λ³ (cubic)")

  for (i in lambda_rows) {
    term <- params_df$Parameter[i]
    term_label <- if (term %in% names(term_names)) term_names[term] else term

    result <- list(
      test = paste0("Polynomial: ", term_label),
      test_type = "dose_response",
      local_family = local_family,
      study_type = study_type,
      outcome = outcome,
      primary = list(
        estimate = params_df$Coefficient[i],
        se = params_df$SE[i],
        lower_cl = params_df$CI_low[i],
        upper_cl = params_df$CI_high[i],
        p_raw = params_df$p[i]
      ),
      coarsened = NULL,
      narrow = NULL,
      full = NULL
    )

    results[[length(results) + 1]] <- result
  }

  results
}

#' Extract temporal regression coefficients
#'
#' Extracts time-related regression coefficients from the model.
#' These are an alternative presentation of temporal effects - instead of
#' emmeans contrasts (e.g., "S20 - S1 = 1.73"), this gives the per-unit-time
#' coefficient (e.g., "session_numeric = 0.09 per session").
#'
#' Both approaches test the same hypothesis (does the effect change over time?)
#' so they share the same FDR correction family.
#'
#' @param models List of fitted models
#' @param data Data frame used for model fitting
#' @param outcome Character name of outcome variable
#' @param time_var Character name of time variable (e.g., "session_numeric")
#' @param local_family Character name of local family
#' @param has_domain Logical, whether domain moderation is present
#' @return List of contrast results with test_type = "temporal_coefficient"
extract_temporal_coefficients <- function(models, data, outcome, time_var, local_family, has_domain) {

  full_cont <- models$full_continuous

  # Use parameters package for unified extraction across model types
  params <- model_parameters(full_cont, ci = 0.95)
  params_df <- as.data.frame(params)

  results <- list()


  # Define terms to extract and their labels
  # Pattern: exact match for main effect, or interaction with time_var
  term_patterns <- list(
    list(
      pattern = paste0("^", time_var, "$"),
      label = "Time",
      coef_name = time_var
    ),
    list(
      pattern = paste0("^lambda:", time_var, "$|^", time_var, ":lambda$"),
      label = "Relationship-Seeking × Time",
      coef_name = paste0("lambda:", time_var)
    ),
    list(
      pattern = paste0("^personalisation.*:", time_var, "$|^", time_var, ":personalisation"),
      label = "Personalisation × Time",
      coef_name = paste0("personalisation:", time_var)
    )
  )

  # Add domain if applicable
  if (has_domain) {
    term_patterns <- c(term_patterns, list(list(
      pattern = paste0("^domain.*:", time_var, "$|^", time_var, ":domain"),
      label = "Domain × Time",
      coef_name = paste0("domain:", time_var)
    )))
  }

  for (term_info in term_patterns) {
    # Find matching rows
    matching_rows <- grep(term_info$pattern, params_df$Parameter, ignore.case = TRUE)

    if (length(matching_rows) > 0) {
      i <- matching_rows[1]  # Take first match

      result <- list(
        test = paste0("Coefficient: ", term_info$label),
        test_type = "temporal_coefficient",
        local_family = local_family,
        study_type = "longitudinal",
        outcome = outcome,
        coefficient_name = term_info$coef_name,
        primary = list(
          estimate = params_df$Coefficient[i],
          se = params_df$SE[i],
          lower_cl = params_df$CI_low[i],
          upper_cl = params_df$CI_high[i],
          p_raw = params_df$p[i]
        ),
        coarsened = NULL,
        narrow = NULL,
        full = NULL
      )

      results[[length(results) + 1]] <- result
    }
  }

  results
}

#' Extract moderation contrasts (interaction + simple effects)
extract_moderation_contrasts <- function(models, data, outcome, study_type, moderator, local_family) {

  full_cont <- models$full_continuous
  full_coars <- models$full_coarsened

  mod_label <- tools::toTitleCase(moderator)
  results <- list()

  # INTERACTION TEST
  emm_by_mod <- emmeans(full_cont,
    as.formula(paste("~ lambda |", moderator)),
    at = list(lambda = c(-1, -0.5, 0.5, 1)),
    data = data
  )
  simple_effects <- contrast(emm_by_mod,
    method = list("pos_vs_neg" = c(-0.5, -0.5, 0.5, 0.5)),
    by = moderator
  )
  interaction_test <- contrast(simple_effects, method = "revpairwise", by = NULL)
  int_df <- clean_emmeans(interaction_test)

  result_int <- list(
    test = paste0("RS × ", mod_label, ": Interaction"),
    test_type = "moderation",
    local_family = local_family,
    study_type = study_type,
    outcome = outcome,
    primary = list(
      estimate = int_df$estimate[1],
      lower_cl = int_df$lower.CL[1],
      upper_cl = int_df$upper.CL[1],
      p_raw = int_df$p.value[1]
    )
  )

  # Coarsened interaction
  emm_coars <- emmeans(full_coars,
    as.formula(paste("~ relationship_seeking_category |", moderator))
  )
  simple_coars <- contrast(emm_coars,
    method = list("pos - neg" = c(-1, 0, 1)),
    by = moderator
  )
  int_coars <- contrast(simple_coars, method = "revpairwise", by = NULL)
  int_coars_df <- clean_emmeans(int_coars)

  result_int$coarsened <- list(
    estimate = int_coars_df$estimate[1],
    lower_cl = int_coars_df$lower.CL[1],
    upper_cl = int_coars_df$upper.CL[1],
    p_raw = int_coars_df$p.value[1]
  )

  result_int$narrow <- NULL
  result_int$full <- NULL

  results[[length(results) + 1]] <- result_int

  # SIMPLE EFFECTS (RS at each level of moderator)
  simple_df <- clean_emmeans(simple_effects)

  for (i in seq_len(nrow(simple_df))) {
    level <- simple_df[[moderator]][i]

    result_simple <- list(
      test = paste0("RS at ", mod_label, " = ", level),
      test_type = "simple_effect",
      local_family = local_family,
      study_type = study_type,
      outcome = outcome,
      primary = list(
        estimate = simple_df$estimate[i],
        lower_cl = simple_df$lower.CL[i],
        upper_cl = simple_df$upper.CL[i],
        p_raw = simple_df$p.value[i]
      ),
      coarsened = NULL,
      narrow = NULL,
      full = NULL
    )

    results[[length(results) + 1]] <- result_simple
  }

  results
}

#' Extract temporal dynamics contrasts
#'
#' Computes all temporal contrasts for Table X.4:
#' - Time main effect: S20 - S1 (marginal over all treatment levels)
#' - RS × Time interaction: Change in RS effect over time
#' - Domain × Time interaction: Change in domain effect over time
#' - Personalisation × Time interaction: Change in pers effect over time
#' - Simple effects at each timepoint for all moderators
#'
#' All interaction tests include coarsened robustness checks.
#'
#' @param models List containing full_continuous and full_coarsened models
#' @param data Data frame for emmeans
#' @param outcome Character name of outcome variable
#' @param time_var Character name of time variable (e.g., "session_numeric")
#' @param time_points Numeric vector of time points (e.g., c(1, 20))
#' @param local_family Character name of local family this outcome belongs to
#' @param has_domain Logical, whether this local family has domain variation
#' @return List of contrast results
extract_temporal_contrasts <- function(models, data, outcome, time_var, time_points, local_family, has_domain) {

  full_cont <- models$full_continuous
  full_coars <- models$full_coarsened
  results <- list()

  time_label <- paste0("S", time_points[2], " - S", time_points[1])

  # =========================================================================
  # 1. TIME MAIN EFFECT: S20 - S1 (marginal over all treatments)
  # =========================================================================

  # Primary: averaged over lambda levels
  at_time <- list()
  at_time[[time_var]] <- time_points

  emm_time <- emmeans(full_cont, as.formula(paste("~", time_var)), at = at_time, data = data)
  time_contrast <- contrast(emm_time, method = "revpairwise")
  time_df <- clean_emmeans(time_contrast)

  result_time <- list(
    test = paste0("Time: ", time_label),
    test_type = "temporal_main",
    local_family = local_family,
    study_type = "longitudinal",
    outcome = outcome,
    primary = list(
      estimate = time_df$estimate[1],
      lower_cl = time_df$lower.CL[1],
      upper_cl = time_df$upper.CL[1],
      p_raw = time_df$p.value[1]
    )
  )

  # Coarsened
  emm_time_coars <- emmeans(full_coars, as.formula(paste("~", time_var)), at = at_time)
  time_coars <- contrast(emm_time_coars, method = "revpairwise")
  time_coars_df <- clean_emmeans(time_coars)

  result_time$coarsened <- list(
    estimate = time_coars_df$estimate[1],
    lower_cl = time_coars_df$lower.CL[1],
    upper_cl = time_coars_df$upper.CL[1],
    p_raw = time_coars_df$p.value[1]
  )
  result_time$narrow <- NULL
  result_time$full <- NULL

  results[[length(results) + 1]] <- result_time

  # =========================================================================
  # 2. RS × TIME INTERACTION
  # =========================================================================

  # RS effect at each time point (primary)
  at_list <- list(lambda = c(-1, -0.5, 0.5, 1))
  at_list[[time_var]] <- time_points

  emm_by_time <- emmeans(full_cont,
    as.formula(paste("~ lambda |", time_var)),
    at = at_list,
    data = data
  )

  rs_by_time <- contrast(emm_by_time,
    method = list("pos_vs_neg" = c(-0.5, -0.5, 0.5, 0.5)),
    by = time_var
  )
  rs_time_df <- clean_emmeans(rs_by_time)

  # RS × Time interaction: Change in RS effect over time
  temporal_change <- contrast(rs_by_time, method = "revpairwise", by = NULL)
  change_df <- clean_emmeans(temporal_change)

  result_rs_time <- list(
    test = paste0("Relationship-Seeking × Time: Δ (", time_label, ")"),
    test_type = "temporal_interaction",
    local_family = local_family,
    study_type = "longitudinal",
    outcome = outcome,
    primary = list(
      estimate = change_df$estimate[1],
      lower_cl = change_df$lower.CL[1],
      upper_cl = change_df$upper.CL[1],
      p_raw = change_df$p.value[1]
    )
  )

  # Coarsened RS × Time
  at_coars <- list()
  at_coars[[time_var]] <- time_points

  emm_rs_time_coars <- emmeans(full_coars,
    as.formula(paste("~ relationship_seeking_category |", time_var)),
    at = at_coars
  )
  rs_coars_simple <- contrast(emm_rs_time_coars,
    method = list("pos - neg" = c(-1, 0, 1)),
    by = time_var
  )
  rs_coars_change <- contrast(rs_coars_simple, method = "revpairwise", by = NULL)
  rs_coars_df <- clean_emmeans(rs_coars_change)

  result_rs_time$coarsened <- list(
    estimate = rs_coars_df$estimate[1],
    lower_cl = rs_coars_df$lower.CL[1],
    upper_cl = rs_coars_df$upper.CL[1],
    p_raw = rs_coars_df$p.value[1]
  )
  result_rs_time$narrow <- NULL
  result_rs_time$full <- NULL

  results[[length(results) + 1]] <- result_rs_time

  # RS simple effects at each time point
  for (i in seq_len(nrow(rs_time_df))) {
    tp <- rs_time_df[[time_var]][i]

    result <- list(
      test = paste0("Relationship-Seeking at S", tp),
      test_type = "temporal_simple",
      local_family = local_family,
      study_type = "longitudinal",
      outcome = outcome,
      primary = list(
        estimate = rs_time_df$estimate[i],
        lower_cl = rs_time_df$lower.CL[i],
        upper_cl = rs_time_df$upper.CL[i],
        p_raw = rs_time_df$p.value[i]
      ),
      coarsened = NULL,
      narrow = NULL,
      full = NULL
    )

    results[[length(results) + 1]] <- result
  }

  # =========================================================================
  # 3. DOMAIN × TIME INTERACTION 
  # (if this local family has domain - mood is only emotchat)
  # =========================================================================

  if (has_domain) {
    # Domain effect at each time point
    emm_domain_time <- emmeans(full_cont,
      as.formula(paste("~ domain |", time_var)),
      at = at_time,
      data = data
    )
    domain_by_time <- contrast(emm_domain_time, method = "revpairwise", by = time_var)
    domain_time_df <- clean_emmeans(domain_by_time)

    # Domain × Time interaction
    domain_change <- contrast(domain_by_time, method = "revpairwise", by = NULL)
    domain_change_df <- clean_emmeans(domain_change)

    result_domain_time <- list(
      test = paste0("Domain × Time: Δ (", time_label, ")"),
      test_type = "temporal_interaction",
      local_family = local_family,
      study_type = "longitudinal",
      outcome = outcome,
      primary = list(
        estimate = domain_change_df$estimate[1],
        lower_cl = domain_change_df$lower.CL[1],
        upper_cl = domain_change_df$upper.CL[1],
        p_raw = domain_change_df$p.value[1]
      )
    )

    # Coarsened
    emm_domain_coars <- emmeans(full_coars,
      as.formula(paste("~ domain |", time_var)),
      at = at_coars
    )
    domain_coars_simple <- contrast(emm_domain_coars, method = "revpairwise", by = time_var)
    domain_coars_change <- contrast(domain_coars_simple, method = "revpairwise", by = NULL)
    domain_coars_df <- clean_emmeans(domain_coars_change)

    result_domain_time$coarsened <- list(
      estimate = domain_coars_df$estimate[1],
      lower_cl = domain_coars_df$lower.CL[1],
      upper_cl = domain_coars_df$upper.CL[1],
      p_raw = domain_coars_df$p.value[1]
    )
    result_domain_time$narrow <- NULL
    result_domain_time$full <- NULL

    results[[length(results) + 1]] <- result_domain_time

    # Domain simple effects at each time point
    for (i in seq_len(nrow(domain_time_df))) {
      tp <- domain_time_df[[time_var]][i]

      result <- list(
        test = paste0("Domain at S", tp),
        test_type = "temporal_simple",
        local_family = local_family,
        study_type = "longitudinal",
        outcome = outcome,
        primary = list(
          estimate = domain_time_df$estimate[i],
          lower_cl = domain_time_df$lower.CL[i],
          upper_cl = domain_time_df$upper.CL[i],
          p_raw = domain_time_df$p.value[i]
        ),
        coarsened = NULL,
        narrow = NULL,
        full = NULL
      )

      results[[length(results) + 1]] <- result
    }
  }

  # =========================================================================
  # 4. PERSONALISATION × TIME INTERACTION
  # (always present for time-varying)
  # =========================================================================

  {
    # Personalisation effect at each time point
    emm_pers_time <- emmeans(full_cont,
      as.formula(paste("~ personalisation |", time_var)),
      at = at_time,
      data = data
    )
    pers_by_time <- contrast(emm_pers_time, method = "revpairwise", by = time_var)
    pers_time_df <- clean_emmeans(pers_by_time)

    # Personalisation × Time interaction
    pers_change <- contrast(pers_by_time, method = "revpairwise", by = NULL)
    pers_change_df <- clean_emmeans(pers_change)

    result_pers_time <- list(
      test = paste0("Personalisation × Time: Δ (", time_label, ")"),
      test_type = "temporal_interaction",
      local_family = local_family,
      study_type = "longitudinal",
      outcome = outcome,
      primary = list(
        estimate = pers_change_df$estimate[1],
        lower_cl = pers_change_df$lower.CL[1],
        upper_cl = pers_change_df$upper.CL[1],
        p_raw = pers_change_df$p.value[1]
      )
    )

    # Coarsened
    emm_pers_coars <- emmeans(full_coars,
      as.formula(paste("~ personalisation |", time_var)),
      at = at_coars
    )
    pers_coars_simple <- contrast(emm_pers_coars, method = "revpairwise", by = time_var)
    pers_coars_change <- contrast(pers_coars_simple, method = "revpairwise", by = NULL)
    pers_coars_df <- clean_emmeans(pers_coars_change)

    result_pers_time$coarsened <- list(
      estimate = pers_coars_df$estimate[1],
      lower_cl = pers_coars_df$lower.CL[1],
      upper_cl = pers_coars_df$upper.CL[1],
      p_raw = pers_coars_df$p.value[1]
    )
    result_pers_time$narrow <- NULL
    result_pers_time$full <- NULL

    results[[length(results) + 1]] <- result_pers_time

    # Personalisation simple effects at each time point
    for (i in seq_len(nrow(pers_time_df))) {
      tp <- pers_time_df[[time_var]][i]

      result <- list(
        test = paste0("Personalisation at S", tp),
        test_type = "temporal_simple",
        local_family = local_family,
        study_type = "longitudinal",
        outcome = outcome,
        primary = list(
          estimate = pers_time_df$estimate[i],
          lower_cl = pers_time_df$lower.CL[i],
          upper_cl = pers_time_df$upper.CL[i],
          p_raw = pers_time_df$p.value[i]
        ),
        coarsened = NULL,
        narrow = NULL,
        full = NULL
      )

      results[[length(results) + 1]] <- result
    }
  }

  # =========================================================================
  # 5. TEMPORAL SLOPES (for Table X.5)
  # =========================================================================
  # Extract slopes at every combination of factor levels with robustness:
  # - Primary: continuous lambda at ±0.75
  # - Coarsened: 3-level categorical RS
  # - Narrow: continuous lambda at ±0.5 (RS slopes only)
  # - Full: continuous lambda at ±1.0 (RS slopes only)
  # =========================================================================

  trend_col <- paste0(time_var, ".trend")

  # Helper: safely extract slope from emtrends df (handles both CI column conventions)
  safe_slope <- function(df, idx, trend_col) {
    if (is.null(df) || nrow(df) < idx) return(list())
    # Handle both asymp.LCL/asymp.UCL and lower.CL/upper.CL column names
    lower_col <- if ("asymp.LCL" %in% names(df)) "asymp.LCL" else "lower.CL"
    upper_col <- if ("asymp.UCL" %in% names(df)) "asymp.UCL" else "upper.CL"
    list(
      estimate = df[[trend_col]][idx],
      lower_cl = df[[lower_col]][idx],
      upper_cl = df[[upper_col]][idx],
      p_raw = df$p.value[idx]
    )
  }

  # --- 1-WAY SLOPES ---

  # 1a. RS slopes with robustness
  # Primary (lambda = ±0.75)
  slopes_rs_prim <- as.data.frame(summary(emtrends(
    full_cont, ~ lambda, var = time_var,
    at = list(lambda = c(-0.75, 0.75)), data = data
  ), infer = TRUE))
  # Coarsened (categorical RS)
  slopes_rs_coars <- tryCatch({
    as.data.frame(summary(emtrends(
      full_coars, ~ relationship_seeking_category, var = time_var, data = data
    ), infer = TRUE))
  }, error = function(e) NULL)
  # Narrow (lambda = ±0.5)
  slopes_rs_narrow <- as.data.frame(summary(emtrends(
    full_cont, ~ lambda, var = time_var,
    at = list(lambda = c(-0.5, 0.5)), data = data
  ), infer = TRUE))
  # Full (lambda = ±1.0)
  slopes_rs_full <- as.data.frame(summary(emtrends(
    full_cont, ~ lambda, var = time_var,
    at = list(lambda = c(-1.0, 1.0)), data = data
  ), infer = TRUE))

  for (i in seq_len(nrow(slopes_rs_prim))) {
    rs_label <- if (slopes_rs_prim$lambda[i] < 0) "neg_λ" else "pos_λ"
    coars_idx <- if (!is.null(slopes_rs_coars)) {
      which(grepl(if (rs_label == "neg_λ") "neg" else "pos",
                  slopes_rs_coars$relationship_seeking_category, ignore.case = TRUE))[1]
    } else NA

    result <- list(
      test = paste("Slope:", rs_label),
      test_type = "temporal_slope",
      local_family = local_family,
      slope_type = "1way",
      study_type = "longitudinal",
      outcome = outcome,
      rs_level = rs_label,
      domain_level = NA,
      pers_level = NA,
      primary = safe_slope(slopes_rs_prim, i, trend_col),
      coarsened = if (!is.na(coars_idx)) safe_slope(slopes_rs_coars, coars_idx, trend_col) else list(),
      narrow = safe_slope(slopes_rs_narrow, i, trend_col),
      full = safe_slope(slopes_rs_full, i, trend_col)
    )
    results[[length(results) + 1]] <- result
  }

  # 1b. Domain slopes (coarsened only for robustness, no narrow/full)
  if (has_domain) {
    slopes_domain_prim <- as.data.frame(summary(emtrends(
      full_cont, ~ domain, var = time_var, data = data
    ), infer = TRUE))
    slopes_domain_coars <- tryCatch({
      as.data.frame(summary(emtrends(
        full_coars, ~ domain, var = time_var, data = data
      ), infer = TRUE))
    }, error = function(e) NULL)

    for (i in seq_len(nrow(slopes_domain_prim))) {
      dom_val <- as.character(slopes_domain_prim$domain[i])
      coars_idx <- if (!is.null(slopes_domain_coars)) {
        which(slopes_domain_coars$domain == dom_val)[1]
      } else NA

      result <- list(
        test = paste("Slope:", dom_val),
        test_type = "temporal_slope",
        local_family = local_family,
        slope_type = "1way",
        study_type = "longitudinal",
        outcome = outcome,
        rs_level = NA,
        domain_level = dom_val,
        pers_level = NA,
        primary = safe_slope(slopes_domain_prim, i, trend_col),
        coarsened = if (!is.na(coars_idx)) safe_slope(slopes_domain_coars, coars_idx, trend_col) else list(),
        narrow = list(),
        full = list()
      )
      results[[length(results) + 1]] <- result
    }
  }

  # 1c. Personalisation slopes (coarsened only for robustness, always present)
  {
    slopes_pers_prim <- as.data.frame(summary(emtrends(
      full_cont, ~ personalisation, var = time_var, data = data
    ), infer = TRUE))
    slopes_pers_coars <- tryCatch({
      as.data.frame(summary(emtrends(
        full_coars, ~ personalisation, var = time_var, data = data
      ), infer = TRUE))
    }, error = function(e) NULL)

    for (i in seq_len(nrow(slopes_pers_prim))) {
      pers_val <- as.character(slopes_pers_prim$personalisation[i])
      pers_label <- if (pers_val == "personalised") "pers" else "non-pers"
      coars_idx <- if (!is.null(slopes_pers_coars)) {
        which(slopes_pers_coars$personalisation == pers_val)[1]
      } else NA

      result <- list(
        test = paste("Slope:", pers_label),
        test_type = "temporal_slope",
        local_family = local_family,
        slope_type = "1way",
        study_type = "longitudinal",
        outcome = outcome,
        rs_level = NA,
        domain_level = NA,
        pers_level = pers_label,
        primary = safe_slope(slopes_pers_prim, i, trend_col),
        coarsened = if (!is.na(coars_idx)) safe_slope(slopes_pers_coars, coars_idx, trend_col) else list(),
        narrow = list(),
        full = list()
      )
      results[[length(results) + 1]] <- result
    }
  }

  # --- 2-WAY SLOPES ---

  # 2a. RS × Domain (with narrow/full)
  if (has_domain) {
    slopes_rd_prim <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | domain, var = time_var,
      at = list(lambda = c(-0.75, 0.75)), data = data
    ), infer = TRUE))
    slopes_rd_coars <- tryCatch({
      as.data.frame(summary(emtrends(
        full_coars, ~ relationship_seeking_category | domain, var = time_var, data = data
      ), infer = TRUE))
    }, error = function(e) NULL)
    slopes_rd_narrow <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | domain, var = time_var,
      at = list(lambda = c(-0.5, 0.5)), data = data
    ), infer = TRUE))
    slopes_rd_full <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | domain, var = time_var,
      at = list(lambda = c(-1.0, 1.0)), data = data
    ), infer = TRUE))

    for (i in seq_len(nrow(slopes_rd_prim))) {
      rs_label <- if (slopes_rd_prim$lambda[i] < 0) "neg_λ" else "pos_λ"
      dom_val <- as.character(slopes_rd_prim$domain[i])

      # Match coarsened: lambda < 0 -> neg_lambda, lambda > 0 -> pos_lambda
      coars_cat <- if (slopes_rd_prim$lambda[i] < 0) "neg_lambda" else "pos_lambda"
      coars_idx <- if (!is.null(slopes_rd_coars)) {
        which(slopes_rd_coars$relationship_seeking_category == coars_cat &
              slopes_rd_coars$domain == dom_val)[1]
      } else NA

      result <- list(
        test = paste("Slope:", rs_label, "×", dom_val),
        test_type = "temporal_slope",
        local_family = local_family,
        slope_type = "2way",
        study_type = "longitudinal",
        outcome = outcome,
        rs_level = rs_label,
        domain_level = dom_val,
        pers_level = NA,
        primary = safe_slope(slopes_rd_prim, i, trend_col),
        coarsened = if (!is.na(coars_idx)) safe_slope(slopes_rd_coars, coars_idx, trend_col) else list(),
        narrow = safe_slope(slopes_rd_narrow, i, trend_col),
        full = safe_slope(slopes_rd_full, i, trend_col)
      )
      results[[length(results) + 1]] <- result
    }
  }

  # 2b. RS × Personalisation (with narrow/full, always present)
  {
    slopes_rp_prim <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | personalisation, var = time_var,
      at = list(lambda = c(-0.75, 0.75)), data = data
    ), infer = TRUE))
    slopes_rp_coars <- tryCatch({
      as.data.frame(summary(emtrends(
        full_coars, ~ relationship_seeking_category | personalisation, var = time_var, data = data
      ), infer = TRUE))
    }, error = function(e) NULL)
    slopes_rp_narrow <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | personalisation, var = time_var,
      at = list(lambda = c(-0.5, 0.5)), data = data
    ), infer = TRUE))
    slopes_rp_full <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | personalisation, var = time_var,
      at = list(lambda = c(-1.0, 1.0)), data = data
    ), infer = TRUE))

    for (i in seq_len(nrow(slopes_rp_prim))) {
      rs_label <- if (slopes_rp_prim$lambda[i] < 0) "neg_λ" else "pos_λ"
      pers_val <- as.character(slopes_rp_prim$personalisation[i])
      pers_label <- if (pers_val == "personalised") "pers" else "non-pers"

      # Match coarsened: lambda < 0 -> neg_lambda, lambda > 0 -> pos_lambda
      coars_cat <- if (slopes_rp_prim$lambda[i] < 0) "neg_lambda" else "pos_lambda"
      coars_idx <- if (!is.null(slopes_rp_coars)) {
        which(slopes_rp_coars$relationship_seeking_category == coars_cat &
              slopes_rp_coars$personalisation == pers_val)[1]
      } else NA

      result <- list(
        test = paste("Slope:", rs_label, "×", pers_label),
        test_type = "temporal_slope",
        local_family = local_family,
        slope_type = "2way",
        study_type = "longitudinal",
        outcome = outcome,
        rs_level = rs_label,
        domain_level = NA,
        pers_level = pers_label,
        primary = safe_slope(slopes_rp_prim, i, trend_col),
        coarsened = if (!is.na(coars_idx)) safe_slope(slopes_rp_coars, coars_idx, trend_col) else list(),
        narrow = safe_slope(slopes_rp_narrow, i, trend_col),
        full = safe_slope(slopes_rp_full, i, trend_col)
      )
      results[[length(results) + 1]] <- result
    }
  }

  # 2c. Domain × Personalisation (coarsened only)
  if (has_domain) {
    slopes_dp_prim <- as.data.frame(summary(emtrends(
      full_cont, ~ domain | personalisation, var = time_var, data = data
    ), infer = TRUE))
    slopes_dp_coars <- tryCatch({
      as.data.frame(summary(emtrends(
        full_coars, ~ domain | personalisation, var = time_var, data = data
      ), infer = TRUE))
    }, error = function(e) NULL)

    for (i in seq_len(nrow(slopes_dp_prim))) {
      dom_val <- as.character(slopes_dp_prim$domain[i])
      pers_val <- as.character(slopes_dp_prim$personalisation[i])
      pers_label <- if (pers_val == "personalised") "pers" else "non-pers"

      coars_idx <- if (!is.null(slopes_dp_coars)) {
        which(slopes_dp_coars$domain == dom_val &
              slopes_dp_coars$personalisation == pers_val)[1]
      } else NA

      result <- list(
        test = paste("Slope:", dom_val, "×", pers_label),
        test_type = "temporal_slope",
        local_family = local_family,
        slope_type = "2way",
        study_type = "longitudinal",
        outcome = outcome,
        rs_level = NA,
        domain_level = dom_val,
        pers_level = pers_label,
        primary = safe_slope(slopes_dp_prim, i, trend_col),
        coarsened = if (!is.na(coars_idx)) safe_slope(slopes_dp_coars, coars_idx, trend_col) else list(),
        narrow = list(),
        full = list()
      )
      results[[length(results) + 1]] <- result
    }
  }

  # --- 3-WAY SLOPES ---

  if (has_domain) {
    slopes_3_prim <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | domain * personalisation, var = time_var,
      at = list(lambda = c(-0.75, 0.75)), data = data
    ), infer = TRUE))
    slopes_3_coars <- tryCatch({
      as.data.frame(summary(emtrends(
        full_coars, ~ relationship_seeking_category | domain * personalisation, var = time_var, data = data
      ), infer = TRUE))
    }, error = function(e) NULL)
    slopes_3_narrow <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | domain * personalisation, var = time_var,
      at = list(lambda = c(-0.5, 0.5)), data = data
    ), infer = TRUE))
    slopes_3_full <- as.data.frame(summary(emtrends(
      full_cont, ~ lambda | domain * personalisation, var = time_var,
      at = list(lambda = c(-1.0, 1.0)), data = data
    ), infer = TRUE))

    for (i in seq_len(nrow(slopes_3_prim))) {
      rs_label <- if (slopes_3_prim$lambda[i] < 0) "neg_λ" else "pos_λ"
      dom_val <- as.character(slopes_3_prim$domain[i])
      pers_val <- as.character(slopes_3_prim$personalisation[i])
      pers_label <- if (pers_val == "personalised") "pers" else "non-pers"

      # Match coarsened: lambda < 0 -> neg_lambda, lambda > 0 -> pos_lambda
      coars_cat <- if (slopes_3_prim$lambda[i] < 0) "neg_lambda" else "pos_lambda"
      coars_idx <- if (!is.null(slopes_3_coars)) {
        which(slopes_3_coars$relationship_seeking_category == coars_cat &
              slopes_3_coars$domain == dom_val &
              slopes_3_coars$personalisation == pers_val)[1]
      } else NA

      result <- list(
        test = paste("Slope:", rs_label, "×", dom_val, "×", pers_label),
        test_type = "temporal_slope",
        local_family = local_family,
        slope_type = "3way",
        study_type = "longitudinal",
        outcome = outcome,
        rs_level = rs_label,
        domain_level = dom_val,
        pers_level = pers_label,
        primary = safe_slope(slopes_3_prim, i, trend_col),
        coarsened = if (!is.na(coars_idx)) safe_slope(slopes_3_coars, coars_idx, trend_col) else list(),
        narrow = safe_slope(slopes_3_narrow, i, trend_col),
        full = safe_slope(slopes_3_full, i, trend_col)
      )
      results[[length(results) + 1]] <- result
    }
  }

  results
}

# =============================================================================
# FDR CORRECTION
# =============================================================================
#
# Hierarchical FDR correction with local and global families:
#   - p_raw: Uncorrected p-value
#   - p_local: FDR-corrected within local family (outcomes from same model file)
#   - p_global: FDR-corrected within global family (all outcomes in measure family)
#
# KEY DISTINCTION:
#   - INFERENTIAL tests: Included in FDR family → get p_raw, p_local, p_global
#   - DESCRIPTIVE tests: NOT in FDR family → get p_raw only (p_local = NULL)
#
# Inferential test_types: main_effect_RS, main_effect_Pers, main_effect_Domain,
#                         dose_response, moderation,
#                         temporal_main, temporal_interaction, temporal_slope
#
# Descriptive test_types: simple_effect, temporal_simple
# 
# =============================================================================

# FDR correction families by test_type (each corresponds to a different RQ)
# Family 1a: main_effect_RS - "Does relationship-seeking affect outcomes?"
# Family 1b: main_effect_Pers - "Does personalisation affect outcomes?"
# Family 1c: main_effect_Domain - "Does domain affect outcomes?"
# Family 2: dose_response - "What is the functional form?"
# Family 3: moderation - "Is there a moderating effect?"
# Family 4a: temporal_main + temporal_interaction - "What are time effects?"
# Family 4b: temporal_coefficient - "What are rates of change?" (same RQ as 4a, different presentation)
# Family 5: temporal_slope - "What are slopes?"

FDR_FAMILIES <- list(
  main_effect_RS = c("main_effect_RS"),
  main_effect_Pers = c("main_effect_Pers"),
  main_effect_Domain = c("main_effect_Domain"),
  dose_response = c("dose_response"),
  moderation = c("moderation"),
  temporal_contrast = c("temporal_main", "temporal_interaction"),
  temporal_coefficient = c("temporal_coefficient"),
  temporal_slope = c("temporal_slope")
)

DESCRIPTIVE_TYPES <- c("simple_effect", "temporal_simple")

#' Apply hierarchical FDR correction to contrasts by test_type
#'
#' Two-level correction:
#' - p_local: FDR within test_type × local_family
#' - p_global: FDR within test_type × global_family (all local families)
#'
#' @param contrasts List of contrast results
#' @param local_families_temporal Character vector of local family names that have temporal data
#' @return Updated contrasts with p_local and p_global
apply_hierarchical_fdr <- function(contrasts, local_families_temporal = NULL) {
  if (length(contrasts) == 0) return(contrasts)

  # Get all local families and study types

  local_families <- unique(sapply(contrasts, function(x) x$local_family))
  study_types <- unique(sapply(contrasts, function(x) x$study_type))

  # Helper to get FDR family for a test_type
  get_fdr_family <- function(test_type) {
    for (fam_name in names(FDR_FAMILIES)) {
      if (test_type %in% FDR_FAMILIES[[fam_name]]) return(fam_name)
    }
    return(NULL)  # Descriptive type
  }

  for (study_type in study_types) {
    study_indices <- which(sapply(contrasts, function(x) x$study_type == study_type))

    # Process each FDR family separately
    for (fdr_family in names(FDR_FAMILIES)) {
      test_types_in_family <- FDR_FAMILIES[[fdr_family]]

      # Skip temporal families for local families without temporal data
      is_temporal_family <- fdr_family %in% c("temporal_contrast", "temporal_coefficient", "temporal_slope")

      # ----- STEP 1: Apply LOCAL FDR within each local_family × fdr_family -----
      for (local_fam in local_families) {
        # Skip temporal for non-temporal local families
        if (is_temporal_family && !is.null(local_families_temporal) &&
            !(local_fam %in% local_families_temporal)) {
          next
        }

        local_indices <- study_indices[sapply(study_indices, function(i) {
          contrasts[[i]]$local_family == local_fam &&
            contrasts[[i]]$test_type %in% test_types_in_family
        })]

        if (length(local_indices) > 0) {
          p_raw <- sapply(local_indices, function(i) {
            p <- contrasts[[i]]$primary$p_raw
            if (is.null(p) || is.na(p)) 1 else p
          })

          p_local <- p.adjust(p_raw, method = "fdr")

          for (j in seq_along(local_indices)) {
            i <- local_indices[j]
            contrasts[[i]]$primary$p_local <- p_local[j]
          }
        }
      }

      # ----- STEP 2: Apply GLOBAL FDR across all local families for this fdr_family -----
      # For temporal families, only include local families that have temporal data
      if (is_temporal_family && !is.null(local_families_temporal)) {
        global_indices <- study_indices[sapply(study_indices, function(i) {
          contrasts[[i]]$test_type %in% test_types_in_family &&
            contrasts[[i]]$local_family %in% local_families_temporal
        })]
      } else {
        global_indices <- study_indices[sapply(study_indices, function(i) {
          contrasts[[i]]$test_type %in% test_types_in_family
        })]
      }

      if (length(global_indices) > 0) {
        p_raw <- sapply(global_indices, function(i) {
          p <- contrasts[[i]]$primary$p_raw
          if (is.null(p) || is.na(p)) 1 else p
        })

        p_global <- p.adjust(p_raw, method = "fdr")

        for (j in seq_along(global_indices)) {
          i <- global_indices[j]
          contrasts[[i]]$primary$p_global <- p_global[j]
        }
      }
    }

    # ----- STEP 3: Handle DESCRIPTIVE tests -----
    descriptive_indices <- study_indices[sapply(study_indices, function(i) {
      contrasts[[i]]$test_type %in% DESCRIPTIVE_TYPES
    })]

    for (i in descriptive_indices) {
      contrasts[[i]]$primary$p_local <- NULL
      contrasts[[i]]$primary$p_global <- NULL
    }
  }

  contrasts
}

# =============================================================================
# PROCESS ONE MEASURE FAMILY
# =============================================================================
#
# Processes all local families within a measure family:
# - Loads model files for each local family
# - Discovers outcomes from model keys
# - Extracts all contrasts with local_family metadata
# - Applies hierarchical FDR correction
# - Builds structured JSON output
# =============================================================================

process_measure_family <- function(measure_family_name) {

  cat("\n=== Processing:", toupper(measure_family_name), "===\n")

  # Get config for this measure family
  config <- MEASURE_FAMILIES[[measure_family_name]]
  if (is.null(config)) {
    stop(sprintf("Unknown measure family: %s", measure_family_name))
  }

  # Check all model files exist
  check_model_files(measure_family_name, config$local_families, MODEL_DIR)

  all_contrasts <- list()

  family_structure <- list(
    local_families = list(),
    global_family = names(config$local_families),
    global_family_temporal = names(config$local_families)[sapply(names(config$local_families), function(n) {
      config$local_families[[n]]$has_temporal
    })]
  )

  # Process each local family
  for (local_family_name in names(config$local_families)) {
    local_config <- config$local_families[[local_family_name]]
    has_domain <- local_config$has_domain
    has_temporal <- local_config$has_temporal

    cat("\n--- Local family:", local_family_name, "---\n")

    # Load models and data using explicit file names from config
    models_cs <- if (!is.null(local_config$file_cs)) {
      readRDS(file.path(MODEL_DIR, local_config$file_cs))
    } else NULL

    models_long <- if (!is.null(local_config$file_long)) {
      readRDS(file.path(MODEL_DIR, local_config$file_long))
    } else NULL

    pooled_data <- if (!is.null(local_config$file_data)) {
      readRDS(file.path(MODEL_DIR, local_config$file_data))
    } else NULL

    # Get outcomes from config
    outcomes <- local_config$outcomes
    family_structure$local_families[[local_family_name]] <- outcomes
    cat("  Outcomes:", paste(outcomes, collapse = ", "), "\n")

    # Split data by study type (pooled_data is always a data frame with study_id)
    data_cs <- pooled_data %>% filter(study_id == "cross-sectional")
    data_long <- pooled_data %>% filter(study_id == "longitudinal")

    # Validate factor levels for coarsened contrasts
    validate_rs_category_levels(pooled_data)

    # Process cross-sectional (only when running in cross-sectional mode)
    if (is_cross_sectional && !is.null(models_cs) && !is.null(data_cs)) {
      cat("  Cross-sectional:\n")
      for (outcome in outcomes) {
        cat("    ", outcome, "\n")
        models <- models_cs[[outcome]]
        # Skip if model doesn't exist for this outcome
        if (is.null(models)) {
          cat("      (no cross-sectional model, skipping)\n")
          next
        }
        # Filter to outcome - construct column is required
        if (!"construct" %in% names(data_cs)) {
          stop(sprintf("Data for %s is missing 'construct' column. Ensure create_pooled_data() was used.", local_family_name))
        }
        data <- data_cs %>% filter(construct == outcome)
        # Drop domain column if this local family doesn't have domain variation
        if (!has_domain && "domain" %in% names(data)) {
          data <- data[, names(data) != "domain", drop = FALSE]
        }

        # RS main effect
        rs <- extract_rs_contrasts(models, data, outcome, "cross_sectional", local_family_name)
        all_contrasts[[length(all_contrasts) + 1]] <- rs

        # Domain (only if this local family has domain variation)
        if (has_domain) {
          domain_c <- extract_binary_arm_contrasts(models, data, outcome, "cross_sectional", "domain", local_family_name)
          all_contrasts[[length(all_contrasts) + 1]] <- domain_c
        }

        # Personalisation (always present)
        pers_c <- extract_binary_arm_contrasts(models, data, outcome, "cross_sectional", "personalisation", local_family_name)
        all_contrasts[[length(all_contrasts) + 1]] <- pers_c

        # Polynomial (dose-response)
        poly <- extract_polynomial_contrasts(models, data, outcome, "cross_sectional", local_family_name)
        all_contrasts <- c(all_contrasts, poly)
      }
    }

    # Process longitudinal (only when running in longitudinal mode)
    if (!is_cross_sectional && !is.null(models_long) && !is.null(data_long)) {
      cat("  Longitudinal:\n")
      for (outcome in outcomes) {
        cat("    ", outcome, "\n")
        models <- models_long[[outcome]]
        # Skip if model doesn't exist for this outcome
        if (is.null(models)) {
          cat("      (no longitudinal model, skipping)\n")
          next
        }
        # Filter to outcome - construct column is required
        if (!"construct" %in% names(data_long)) {
          stop(sprintf("Data for %s is missing 'construct' column. Ensure create_pooled_data() was used.", local_family_name))
        }
        data <- data_long %>% filter(construct == outcome)
        # Drop domain column if this local family doesn't have domain variation and domain col is still present
        # (prevents emmeans from trying to use domain as a covariate)
        if (!has_domain && "domain" %in% names(data)) {
          data <- data[, names(data) != "domain", drop = FALSE]
        }

        # RS main effect
        rs <- extract_rs_contrasts(models, data, outcome, "longitudinal", local_family_name)
        all_contrasts[[length(all_contrasts) + 1]] <- rs

        # Domain (only if this local family has domain variation)
        if (has_domain) {
          domain_c <- extract_binary_arm_contrasts(models, data, outcome, "longitudinal", "domain", local_family_name)
          all_contrasts[[length(all_contrasts) + 1]] <- domain_c
        }

        # Personalisation (always present)
        pers_c <- extract_binary_arm_contrasts(models, data, outcome, "longitudinal", "personalisation", local_family_name)
        all_contrasts[[length(all_contrasts) + 1]] <- pers_c

        # Polynomial
        poly <- extract_polynomial_contrasts(models, data, outcome, "longitudinal", local_family_name)
        all_contrasts <- c(all_contrasts, poly)

        # Moderation
        if (has_domain) {
          mod_domain <- extract_moderation_contrasts(models, data, outcome, "longitudinal", "domain", local_family_name)
          all_contrasts <- c(all_contrasts, mod_domain)
        }
        mod_pers <- extract_moderation_contrasts(models, data, outcome, "longitudinal", "personalisation", local_family_name)
        all_contrasts <- c(all_contrasts, mod_pers)

        # Temporal (only for local families with temporal data)
        if (has_temporal) {
          temporal <- extract_temporal_contrasts(models, data, outcome, config$time_var, config$time_points, local_family_name, has_domain)
          all_contrasts <- c(all_contrasts, temporal)

          # Temporal coefficients (alternative presentation - per-unit-time rates)
          temporal_coefs <- extract_temporal_coefficients(models, data, outcome, config$time_var, local_family_name, has_domain)
          all_contrasts <- c(all_contrasts, temporal_coefs)
        }
      }
    }
  }

  # Apply hierarchical FDR correction
  all_contrasts <- apply_hierarchical_fdr(all_contrasts, family_structure$global_family_temporal)

  # Build output structure
  output <- list(
    measure_family = measure_family_name,
    generated = as.character(Sys.time()),
    family_structure = family_structure,
    n_contrasts = length(all_contrasts),
    contrasts = all_contrasts
  )

  output
}

# =============================================================================
# MAIN
# =============================================================================

# File suffix for output files
file_suffix <- if (is_cross_sectional) "_cs" else ""

cat("\nStudy type:", study_type, "\n")
cat("Output suffix:", if (file_suffix == "") "(default, no suffix)" else file_suffix, "\n")

if (run_preferences) {
  cat("\n", strrep("=", 60), "\n")
  cat("PREFERENCES CONTRASTS (", toupper(study_type), ")\n", sep = "")
  cat(strrep("=", 60), "\n")

  results <- process_measure_family("preferences")

  output_path <- file.path(STATS_DIR, paste0("preferences_contrasts", file_suffix, ".json"))
  write_json(results, output_path, pretty = TRUE, auto_unbox = TRUE)
  cat("\nSaved:", output_path, "\n")
  cat("Total contrasts:", results$n_contrasts, "\n")
}

if (run_attachment) {
  cat("\n", strrep("=", 60), "\n")
  cat("ATTACHMENT CONTRASTS (", toupper(study_type), ")\n", sep = "")
  cat(strrep("=", 60), "\n")

  results <- process_measure_family("attachment")

  output_path <- file.path(STATS_DIR, paste0("attachment_contrasts", file_suffix, ".json"))
  write_json(results, output_path, pretty = TRUE, auto_unbox = TRUE)
  cat("\nSaved:", output_path, "\n")
  cat("Total contrasts:", results$n_contrasts, "\n")
}

if (run_psychosocial) {
  cat("\n", strrep("=", 60), "\n")
  cat("PSYCHOSOCIAL CONTRASTS (", toupper(study_type), ")\n", sep = "")
  cat(strrep("=", 60), "\n")

  results <- process_measure_family("psychosocial")

  output_path <- file.path(STATS_DIR, paste0("psychosocial_contrasts", file_suffix, ".json"))
  write_json(results, output_path, pretty = TRUE, auto_unbox = TRUE)
  cat("\nSaved:", output_path, "\n")
  cat("Total contrasts:", results$n_contrasts, "\n")
}

if (run_momentary_affect) {
  cat("\n", strrep("=", 60), "\n")
  cat("MOMENTARY AFFECT CONTRASTS (", toupper(study_type), ")\n", sep = "")
  cat(strrep("=", 60), "\n")

  results <- process_measure_family("momentary_affect")

  output_path <- file.path(STATS_DIR, paste0("momentary_affect_contrasts", file_suffix, ".json"))
  write_json(results, output_path, pretty = TRUE, auto_unbox = TRUE)
  cat("\nSaved:", output_path, "\n")
  cat("Total contrasts:", results$n_contrasts, "\n")
}

if (run_perceptions) {
  cat("\n", strrep("=", 60), "\n")
  cat("PERCEPTIONS CONTRASTS (", toupper(study_type), ")\n", sep = "")
  cat(strrep("=", 60), "\n")

  results <- process_measure_family("perceptions")

  output_path <- file.path(STATS_DIR, paste0("perceptions_contrasts", file_suffix, ".json"))
  write_json(results, output_path, pretty = TRUE, auto_unbox = TRUE)
  cat("\nSaved:", output_path, "\n")
  cat("Total contrasts:", results$n_contrasts, "\n")
}

if (run_will_miss_ai) {
  cat("\n", strrep("=", 60), "\n")
  cat("WILL_MISS_AI CONTRASTS (", toupper(study_type), ")\n", sep = "")
  cat(strrep("=", 60), "\n")

  results <- process_measure_family("will_miss_ai")

  output_path <- file.path(STATS_DIR, paste0("will_miss_ai_contrasts", file_suffix, ".json"))
  write_json(results, output_path, pretty = TRUE, auto_unbox = TRUE)
  cat("\nSaved:", output_path, "\n")
  cat("Total contrasts:", results$n_contrasts, "\n")
}

if (run_psychosocial_individual) {
  cat("\n", strrep("=", 60), "\n")
  cat("PSYCHOSOCIAL INDIVIDUAL CONTRASTS (", toupper(study_type), ")\n", sep = "")
  cat(strrep("=", 60), "\n")

  results <- process_measure_family("psychosocial_individual")

  output_path <- file.path(STATS_DIR, paste0("psychosocial_individual_contrasts", file_suffix, ".json"))
  write_json(results, output_path, pretty = TRUE, auto_unbox = TRUE)
  cat("\nSaved:", output_path, "\n")
  cat("Total contrasts:", results$n_contrasts, "\n")
}

cat("\n", strrep("=", 60), "\n")
cat("DONE\n")
cat(strrep("=", 60), "\n")
