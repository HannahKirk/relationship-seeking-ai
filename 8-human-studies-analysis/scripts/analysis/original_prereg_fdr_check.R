#!/usr/bin/env Rscript
# =============================================================================
# Original Pre-Registration FDR Check (Robustness)
# =============================================================================
#
# Runs the original pre-registration hypotheses.
#
# This script is for robustness checking - the main paper uses a different
# organization (by measure family), but we run the original pre-reg here
# to verify results hold.
#
# Key Changes from Original Pre-Reg:
#   - tool_friend was in "attachment" pre-reg, moved to "perceptions" in paper
#
# FDR Families (treatment-specific):
#   attachment_RS:     6 tests (RS main effects + temporal on attachment)
#   attachment_Pers:   6 tests (Personalisation effects on attachment)
#   attachment_Domain: 6 tests (Domain effects on attachment)
#   wellbeing_RS:      2 tests (RS on F1, F2)
#   wellbeing_Pers:    2 tests (Pers on F1, F2)
#   wellbeing_Domain:  2 tests (Domain on F1, F2)
#   ai_treatment:      2 tests (study_type coefficient)
#   wellbeing_individual_RS:      4 tests (RS on individual scales - robustness)
#   wellbeing_individual_Pers:    4 tests (Pers on individual scales - robustness)
#   wellbeing_individual_Domain:  4 tests (Domain on individual scales - robustness)
#
# Usage:
#   Rscript scripts/analysis/original_prereg_fdr_check.R
#
# Output:
#   outputs/stats/original_prereg_fdr_results.json
#
# =============================================================================

library(tidyverse)
library(jsonlite)

set.seed(1234)

# =============================================================================
# SECTION 1: SETUP
# =============================================================================

cat("=", strrep("=", 59), "\n", sep = "")
cat("Original Pre-Registration FDR Check (Robustness)\n")
cat("=", strrep("=", 59), "\n\n", sep = "")

# Set paths using helper
source("scripts/utils_r/path_utils.R")
paths <- setup_project_paths()

PROJECT_ROOT <- paths$PROJECT_ROOT
MODEL_DIR <- file.path(PROJECT_ROOT, "outputs/models")
STATS_DIR <- file.path(PROJECT_ROOT, "outputs/stats")

dir.create(STATS_DIR, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# SECTION 2: HYPOTHESIS DEFINITIONS (Original Pre-Reg)
# =============================================================================
#
# Original pre-registration structure with CORRECTED FDR families.
# Each family is now treatment-specific.
#
# =============================================================================

HYPOTHESES <- list(
  # =========================================================================
  # RQ1b: Relationship-Seeking Effects on Attachment (6 tests)
  # FDR Family: attachment_RS
  # =========================================================================
  list(
    id = "RQ1b_RS_tool_friend",
    rq = "RQ1b",
    description = "RS -> tool_friend",
    source = "perceptions_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "tool_friend",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_RS"
  ),
  list(
    id = "RQ1b_RS_separation_distress",
    rq = "RQ1b",
    description = "RS -> separation_distress",
    source = "attachment_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "separation_distress",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_RS"
  ),
  list(
    id = "RQ1b_RSxweek_tool_friend",
    rq = "RQ1b",
    description = "RS x week -> tool_friend",
    source = "perceptions_contrasts.json",
    test_pattern = "Relationship-Seeking.*Time",
    outcome = "tool_friend",
    test_type = "temporal_coefficient",
    direction = "two-sided",
    fdr_family = "attachment_RS"
  ),
  list(
    id = "RQ1b_RSxweek_separation_distress",
    rq = "RQ1b",
    description = "RS x week -> separation_distress",
    source = "attachment_contrasts.json",
    test_pattern = "Relationship-Seeking.*Time",
    outcome = "separation_distress",
    test_type = "temporal_coefficient",
    direction = "two-sided",
    fdr_family = "attachment_RS"
  ),
  list(
    id = "RQ1b_RS_will_miss_AI",
    rq = "RQ1b",
    description = "RS -> will_miss_AI",
    source = "will_miss_ai_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "will_miss_ai",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_RS"
  ),
  list(
    id = "RQ1b_RS_goodbye_action",
    rq = "RQ1b",
    description = "RS -> goodbye_action",
    source = "attachment_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "goodbye_action",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_RS"
  ),

  # =========================================================================
  # RQ1c: Relationship-Seeking Effects on Wellbeing Factors (2 tests)
  # FDR Family: wellbeing_RS
  # =========================================================================
  list(
    id = "RQ1c_RS_F1",
    rq = "RQ1c",
    description = "RS -> psychosocial_F1",
    source = "psychosocial_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "psychosocial_F1",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_RS"
  ),
  list(
    id = "RQ1c_RS_F2",
    rq = "RQ1c",
    description = "RS -> psychosocial_F2",
    source = "psychosocial_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "psychosocial_F2",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_RS"
  ),

  # =========================================================================
  # RQ2b: Personalisation Effects on Attachment (6 tests)
  # FDR Family: attachment_Pers
  # =========================================================================
  list(
    id = "RQ2b_Pers_tool_friend",
    rq = "RQ2b",
    description = "Pers -> tool_friend",
    source = "perceptions_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "tool_friend",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Pers"
  ),
  list(
    id = "RQ2b_Pers_separation_distress",
    rq = "RQ2b",
    description = "Pers -> separation_distress",
    source = "attachment_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "separation_distress",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Pers"
  ),
  list(
    id = "RQ2b_Persxweek_tool_friend",
    rq = "RQ2b",
    description = "Pers x week -> tool_friend",
    source = "perceptions_contrasts.json",
    test_pattern = "Personalisation.*Time",
    outcome = "tool_friend",
    test_type = "temporal_coefficient",
    direction = "two-sided",
    fdr_family = "attachment_Pers"
  ),
  list(
    id = "RQ2b_Persxweek_separation_distress",
    rq = "RQ2b",
    description = "Pers x week -> separation_distress",
    source = "attachment_contrasts.json",
    test_pattern = "Personalisation.*Time",
    outcome = "separation_distress",
    test_type = "temporal_coefficient",
    direction = "two-sided",
    fdr_family = "attachment_Pers"
  ),
  list(
    id = "RQ2b_Pers_will_miss_AI",
    rq = "RQ2b",
    description = "Pers -> will_miss_AI",
    source = "will_miss_ai_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "will_miss_ai",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Pers"
  ),
  list(
    id = "RQ2b_Pers_goodbye_action",
    rq = "RQ2b",
    description = "Pers -> goodbye_action",
    source = "attachment_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "goodbye_action",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Pers"
  ),

  # =========================================================================
  # RQ2c: Personalisation Effects on Wellbeing Factors (2 tests)
  # FDR Family: wellbeing_Pers
  # =========================================================================
  list(
    id = "RQ2c_Pers_F1",
    rq = "RQ2c",
    description = "Pers -> psychosocial_F1",
    source = "psychosocial_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "psychosocial_F1",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_Pers"
  ),
  list(
    id = "RQ2c_Pers_F2",
    rq = "RQ2c",
    description = "Pers -> psychosocial_F2",
    source = "psychosocial_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "psychosocial_F2",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_Pers"
  ),

  # =========================================================================
  # RQ3b: Domain Effects on Attachment (6 tests)
  # FDR Family: attachment_Domain
  # =========================================================================
  list(
    id = "RQ3b_Domain_tool_friend",
    rq = "RQ3b",
    description = "Domain -> tool_friend",
    source = "perceptions_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "tool_friend",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Domain"
  ),
  list(
    id = "RQ3b_Domain_separation_distress",
    rq = "RQ3b",
    description = "Domain -> separation_distress",
    source = "attachment_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "separation_distress",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Domain"
  ),
  list(
    id = "RQ3b_Domainxweek_tool_friend",
    rq = "RQ3b",
    description = "Domain x week -> tool_friend",
    source = "perceptions_contrasts.json",
    test_pattern = "Domain.*Time",
    outcome = "tool_friend",
    test_type = "temporal_coefficient",
    direction = "two-sided",
    fdr_family = "attachment_Domain"
  ),
  list(
    id = "RQ3b_Domainxweek_separation_distress",
    rq = "RQ3b",
    description = "Domain x week -> separation_distress",
    source = "attachment_contrasts.json",
    test_pattern = "Domain.*Time",
    outcome = "separation_distress",
    test_type = "temporal_coefficient",
    direction = "two-sided",
    fdr_family = "attachment_Domain"
  ),
  list(
    id = "RQ3b_Domain_will_miss_AI",
    rq = "RQ3b",
    description = "Domain -> will_miss_AI",
    source = "will_miss_ai_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "will_miss_ai",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Domain"
  ),
  list(
    id = "RQ3b_Domain_goodbye_action",
    rq = "RQ3b",
    description = "Domain -> goodbye_action",
    source = "attachment_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "goodbye_action",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "attachment_Domain"
  ),

  # =========================================================================
  # RQ3c: Domain Effects on Wellbeing Factors (2 tests)
  # FDR Family: wellbeing_Domain
  # =========================================================================
  list(
    id = "RQ3c_Domain_F1",
    rq = "RQ3c",
    description = "Domain -> psychosocial_F1",
    source = "psychosocial_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "psychosocial_F1",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_Domain"
  ),
  list(
    id = "RQ3c_Domain_F2",
    rq = "RQ3c",
    description = "Domain -> psychosocial_F2",
    source = "psychosocial_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "psychosocial_F2",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_Domain"
  ),

  # =========================================================================
  # RQ4a: AI Treatment Effects on Wellbeing Factors (2 tests)
  # Combined study model - study_type coefficient
  # FDR Family: ai_treatment
  # =========================================================================
  list(
    id = "RQ4a_AItreatment_F1",
    rq = "RQ4a",
    description = "AI Treatment -> psychosocial_F1",
    source = "combined_study_model",
    model_file = "psychosocial_combined_study.rds",
    outcome = "psychosocial_F1",
    test_type = "combined_study",
    direction = "two-sided",
    fdr_family = "ai_treatment"
  ),
  list(
    id = "RQ4a_AItreatment_F2",
    rq = "RQ4a",
    description = "AI Treatment -> psychosocial_F2",
    source = "combined_study_model",
    model_file = "psychosocial_combined_study.rds",
    outcome = "psychosocial_F2",
    test_type = "combined_study",
    direction = "two-sided",
    fdr_family = "ai_treatment"
  ),

  # =========================================================================
  # ROBUSTNESS: RS Effects on Individual Wellbeing Scales (4 tests)
  # FDR Family: wellbeing_individual_RS
  # =========================================================================
  list(
    id = "ROB_RS_phq_gad",
    rq = "ROB",
    description = "RS -> phq_gad_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "phq_gad_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_RS"
  ),
  list(
    id = "ROB_RS_ucla",
    rq = "ROB",
    description = "RS -> ucla_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "ucla_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_RS"
  ),
  list(
    id = "ROB_RS_lubben",
    rq = "ROB",
    description = "RS -> lubben_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "lubben_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_RS"
  ),
  list(
    id = "ROB_RS_who",
    rq = "ROB",
    description = "RS -> who_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^RS: Avg\\(Pos\\) - Avg\\(Neg\\)$",
    outcome = "who_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_RS"
  ),

  # =========================================================================
  # ROBUSTNESS: Pers Effects on Individual Wellbeing Scales (4 tests)
  # FDR Family: wellbeing_individual_Pers
  # =========================================================================
  list(
    id = "ROB_Pers_phq_gad",
    rq = "ROB",
    description = "Pers -> phq_gad_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "phq_gad_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Pers"
  ),
  list(
    id = "ROB_Pers_ucla",
    rq = "ROB",
    description = "Pers -> ucla_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "ucla_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Pers"
  ),
  list(
    id = "ROB_Pers_lubben",
    rq = "ROB",
    description = "Pers -> lubben_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "lubben_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Pers"
  ),
  list(
    id = "ROB_Pers_who",
    rq = "ROB",
    description = "Pers -> who_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Personalisation:",
    outcome = "who_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Pers"
  ),

  # =========================================================================
  # ROBUSTNESS: Domain Effects on Individual Wellbeing Scales (4 tests)
  # FDR Family: wellbeing_individual_Domain
  # =========================================================================
  list(
    id = "ROB_Domain_phq_gad",
    rq = "ROB",
    description = "Domain -> phq_gad_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "phq_gad_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Domain"
  ),
  list(
    id = "ROB_Domain_ucla",
    rq = "ROB",
    description = "Domain -> ucla_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "ucla_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Domain"
  ),
  list(
    id = "ROB_Domain_lubben",
    rq = "ROB",
    description = "Domain -> lubben_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "lubben_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Domain"
  ),
  list(
    id = "ROB_Domain_who",
    rq = "ROB",
    description = "Domain -> who_score",
    source = "psychosocial_individual_contrasts.json",
    test_pattern = "^Domain:",
    outcome = "who_score",
    test_type = "main_effect",
    direction = "two-sided",
    fdr_family = "wellbeing_individual_Domain"
  )
)

# =============================================================================
# SECTION 3: DATA LOADING
# =============================================================================

cat("--- Loading Contrast Data ---\n\n")

#' Load all contrast JSON files into a named list
load_all_contrasts <- function(stats_dir) {
  contrast_files <- c(
    "attachment_contrasts.json",
    "perceptions_contrasts.json",
    "psychosocial_contrasts.json",
    "will_miss_ai_contrasts.json",
    "psychosocial_individual_contrasts.json"
  )

  contrasts <- list()

  for (file in contrast_files) {
    path <- file.path(stats_dir, file)
    if (file.exists(path)) {
      contrasts[[file]] <- fromJSON(path, simplifyVector = FALSE)
      cat("  Loaded:", file, "\n")
    } else {
      cat("  WARNING: Missing file:", file, "\n")
    }
  }

  contrasts
}

#' Extract study_type coefficient from combined study model
#'
#' Uses parameters::model_parameters() for unified coefficient extraction
#' across lm/lmer/glm models. This is consistent with how extract_polynomial_contrasts
#' and extract_temporal_coefficients work in compute_contrasts.R.
#'
#' @param model_file RDS file containing model or list of models
#' @param model_dir Directory containing model file
#' @param outcome Outcome name (key) if file contains a list of models
extract_combined_study_test <- function(model_file, model_dir, outcome = NULL) {
  path <- file.path(model_dir, model_file)

  if (!file.exists(path)) {
    return(list(
      estimate = NA,
      se = NA,
      lower_cl = NA,
      upper_cl = NA,
      p_raw = NA,
      error = "Model file not found"
    ))
  }

  model_data <- readRDS(path)

  # If outcome specified and data is a list, extract the specific model
  if (!is.null(outcome) && is.list(model_data) && !inherits(model_data, "lm")) {
    if (!(outcome %in% names(model_data))) {
      return(list(
        estimate = NA,
        se = NA,
        lower_cl = NA,
        upper_cl = NA,
        p_raw = NA,
        error = paste("Outcome", outcome, "not found in model list")
      ))
    }
    model <- model_data[[outcome]]
  } else {
    model <- model_data
  }

  # Use parameters::model_parameters for consistent extraction
  params <- as.data.frame(parameters::model_parameters(model, ci = 0.95))

  # Look for study_type coefficient
  idx <- grep("study_type", params$Parameter, ignore.case = TRUE)

  if (length(idx) == 0) {
    return(list(
      estimate = NA,
      se = NA,
      lower_cl = NA,
      upper_cl = NA,
      p_raw = NA,
      error = "study_type coefficient not found"
    ))
  }

  i <- idx[1]
  list(
    estimate = params$Coefficient[i],
    se = params$SE[i],
    lower_cl = params$CI_low[i],
    upper_cl = params$CI_high[i],
    p_raw = params$p[i]
  )
}

# Load contrast data
all_contrasts <- load_all_contrasts(STATS_DIR)

# =============================================================================
# SECTION 4: TEST EXTRACTION
# =============================================================================

cat("\n--- Extracting Tests ---\n\n")

#' Extract a specific contrast from loaded JSON data
#' @param fdr_family FDR family name (e.g., "attachment_RS") - used to determine JSON test_type
extract_contrast <- function(contrasts_list, source, test_pattern, outcome_val, test_type, fdr_family) {

  if (!source %in% names(contrasts_list)) {
    return(NULL)
  }

  json_data <- contrasts_list[[source]]

  # Determine expected test_type in JSON based on fdr_family
  # The JSON uses main_effect_RS, main_effect_Pers, main_effect_Domain
  # Hard-coded mapping from fdr_family suffix to JSON test_type
  expected_test_type <- test_type
  if (test_type == "main_effect") {
    # fdr_family format: "attachment_RS", "wellbeing_Pers", etc.
    # Extract the treatment suffix after the underscore
    fdr_suffix <- sub(".*_", "", fdr_family)  # Gets "RS", "Pers", or "Domain"
    expected_test_type <- paste0("main_effect_", fdr_suffix)
  }

  # Search through contrasts
  for (contrast in json_data$contrasts) {
    # Check if this contrast matches our criteria
    if (grepl(test_pattern, contrast$test, perl = TRUE) &&
        contrast$outcome == outcome_val &&
        contrast$test_type == expected_test_type &&
        contrast$study_type == "longitudinal") {

      primary <- contrast$primary

      return(list(
        test_name = contrast$test,
        estimate = primary$estimate,
        lower_cl = primary$lower_cl,
        upper_cl = primary$upper_cl,
        p_raw = primary$p_raw
      ))
    }
  }

  NULL
}

#' Collect all hypothesis test results
collect_hypothesis_results <- function(hypotheses, contrasts_list, model_dir) {

  results <- list()

  for (h in hypotheses) {
    cat(sprintf("  %s: %s\n", h$id, h$description))

    if (h$source == "combined_study_model") {
      test_result <- extract_combined_study_test(h$model_file, model_dir, h$outcome)
    } else {
      test_result <- extract_contrast(
        contrasts_list,
        h$source,
        h$test_pattern,
        h$outcome,
        h$test_type,
        h$fdr_family
      )
    }

    if (is.null(test_result)) {
      cat(sprintf("    WARNING: Test not found in %s\n", h$source))
      test_result <- list(
        test_name = h$description,
        estimate = NA,
        lower_cl = NA,
        upper_cl = NA,
        p_raw = NA
      )
    }

    results[[h$id]] <- list(
      id = h$id,
      rq = h$rq,
      description = h$description,
      outcome = h$outcome,
      test_type = h$test_type,
      direction = h$direction,
      fdr_family = h$fdr_family,
      test_name = test_result$test_name,
      estimate = test_result$estimate,
      lower_cl = test_result$lower_cl,
      upper_cl = test_result$upper_cl,
      p_raw = test_result$p_raw
    )
  }

  results
}

# Collect all results
hypothesis_results <- collect_hypothesis_results(HYPOTHESES, all_contrasts, MODEL_DIR)

# =============================================================================
# SECTION 5: FDR CORRECTION
# =============================================================================

cat("\n--- Applying FDR Correction (Treatment-Specific) ---\n\n")

#' Apply Benjamini-Hochberg FDR within each family
apply_prereg_fdr <- function(results) {

  # Convert to data frame
  results_df <- do.call(rbind, lapply(names(results), function(id) {
    r <- results[[id]]
    data.frame(
      id = id,
      rq = as.character(r$rq %||% NA),
      description = as.character(r$description %||% NA),
      outcome = as.character(r$outcome %||% NA),
      test_type = as.character(r$test_type %||% NA),
      direction = as.character(r$direction %||% NA),
      fdr_family = as.character(r$fdr_family %||% NA),
      test_name = as.character(r$test_name %||% NA),
      estimate = as.numeric(r$estimate %||% NA),
      lower_cl = as.numeric(r$lower_cl %||% NA),
      upper_cl = as.numeric(r$upper_cl %||% NA),
      p_raw = as.numeric(r$p_raw %||% NA),
      stringsAsFactors = FALSE
    )
  }))

  # Apply FDR within each family
  results_df <- results_df %>%
    group_by(fdr_family) %>%
    mutate(
      n_family = n(),
      p_fdr = p.adjust(p_raw, method = "fdr")
    ) %>%
    ungroup()

  # Print family sizes
  family_counts <- results_df %>%
    group_by(fdr_family) %>%
    summarise(n = n(), .groups = "drop")
  cat("FDR family sizes:\n")
  for (i in seq_len(nrow(family_counts))) {
    cat(sprintf("  %s: %d tests\n", family_counts$fdr_family[i], family_counts$n[i]))
  }

  # Convert back to list
  results_list <- split(results_df, results_df$id)
  results_list <- lapply(results_list, function(df) as.list(df[1, ]))

  results_list
}

# Apply FDR
hypothesis_results <- apply_prereg_fdr(hypothesis_results)

# =============================================================================
# SECTION 6: HYPOTHESIS EVALUATION
# =============================================================================

cat("\n--- Evaluating Hypotheses ---\n\n")

#' Evaluate if a single test is significant given direction
evaluate_test <- function(result, alpha = 0.05) {

  if (is.na(result$p_fdr) || is.na(result$estimate)) {
    return(list(
      significant = FALSE,
      direction_correct = NA,
      observed_direction = NA
    ))
  }

  observed_direction <- if (result$estimate > 0) "positive" else "negative"

  if (result$direction == "two-sided") {
    significant <- result$p_fdr < alpha
    direction_correct <- TRUE
  } else {
    direction_correct <- observed_direction == result$direction
    significant <- result$p_fdr < alpha && direction_correct
  }

  list(
    significant = significant,
    direction_correct = direction_correct,
    observed_direction = observed_direction
  )
}

# Evaluate each test
for (id in names(hypothesis_results)) {
  eval_result <- evaluate_test(hypothesis_results[[id]])
  hypothesis_results[[id]]$significant <- eval_result$significant
  hypothesis_results[[id]]$direction_correct <- eval_result$direction_correct
  hypothesis_results[[id]]$observed_direction <- eval_result$observed_direction
}

#' Evaluate family-level confirmation
evaluate_families <- function(results) {

  results_df <- bind_rows(lapply(results, as.data.frame))

  family_summary <- results_df %>%
    group_by(fdr_family) %>%
    summarise(
      n_tests = n(),
      n_significant = sum(significant, na.rm = TRUE),
      confirmed = any(significant, na.rm = TRUE),
      .groups = "drop"
    )

  family_summary
}

family_summary <- evaluate_families(hypothesis_results)

cat("Family-level summary:\n")
print(family_summary)

# =============================================================================
# SECTION 7: SAVE JSON OUTPUT
# =============================================================================

cat("\n--- Saving Results ---\n\n")

output_data <- list(
  generated = as.character(Sys.time()),
  description = "Original pre-registration hypotheses with treatment-specific FDR families",
  fdr_method = "Benjamini-Hochberg within each treatment-specific family",
  families = as.list(family_summary),
  hypotheses = hypothesis_results
)

json_path <- file.path(STATS_DIR, "original_prereg_fdr_results.json")
write_json(output_data, json_path, pretty = TRUE, auto_unbox = TRUE)
cat("Saved:", json_path, "\n")

# =============================================================================
# SECTION 8: SUMMARY
# =============================================================================

cat("\n")
cat("=", strrep("=", 59), "\n", sep = "")
cat("COMPLETE\n")
cat("=", strrep("=", 59), "\n\n", sep = "")

cat("\nOutput:\n")
cat("  JSON:", json_path, "\n")
