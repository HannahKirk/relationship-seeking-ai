# =============================================================================
# Extract and Print Model Coefficients
# =============================================================================
# Functions to extract and display all coefficients from regression models.
# Adapted from legacy extract_all_coefficients.R
#
# Usage:
#   source("scripts/utils_r/extract_coefficients.R")
#
# Functions:
#   - extract_all_coefficients(): Extract coefficients as data frame
#   - print_all_coefficients(): Print formatted coefficient table
#   - print_model_coefficients_section(): Print full section for all models
# =============================================================================

library(dplyr)
library(parameters)
library(stringr)
library(knitr)

# Source centralized labelling utilities (for clean_parameter_names)
# Note: labelling_utils.R is typically sourced by the main analysis script,
# but we source it here as a fallback
if (!exists("clean_parameter_names")) {
  script_dir <- dirname(sys.frame(1)$ofile)
  if (!is.null(script_dir) && file.exists(file.path(script_dir, "labelling_utils.R"))) {
    source(file.path(script_dir, "labelling_utils.R"))
  }
}

#' Extract and format all coefficients from a model
#'
#' @param model Fitted model object (lm, glm, lmer, glmer)
#' @param outcome_name Name of the outcome variable
#' @return Data frame with formatted coefficients
extract_all_coefficients <- function(model, outcome_name) {
  if (is.null(model)) {
    return(NULL)
  }

  # Check if model is binary (glm with binomial/quasibinomial family)
  is_binary <- inherits(model, "glm") &&
    model$family$family %in% c("binomial", "quasibinomial")

  # Check model type and extract parameters accordingly
  if (inherits(model, c("lmerMod", "glmerMod"))) {
    # For mixed-effects models
    params <- tryCatch({
      parameters::model_parameters(model, effects = "fixed",
                                   p_adjust = "none", verbose = FALSE)
    }, error = function(e) {
      coef_summary <- summary(model)$coefficients
      data.frame(
        Parameter = rownames(coef_summary),
        Coefficient = coef_summary[, "Estimate"],
        SE = coef_summary[, "Std. Error"],
        p = if (ncol(coef_summary) >= 5) coef_summary[, 5] else NA,
        stringsAsFactors = FALSE
      )
    })
  } else if (inherits(model, c("lm", "glm"))) {
    # For regular linear/generalized linear models
    # Use exponentiate = TRUE for binary models to get odds ratios
    params <- tryCatch({
      parameters::model_parameters(model, p_adjust = "none",
                                   exponentiate = is_binary, verbose = FALSE)
    }, error = function(e) {
      coef_summary <- summary(model)$coefficients
      data.frame(
        Parameter = rownames(coef_summary),
        Coefficient = coef_summary[, "Estimate"],
        SE = coef_summary[, "Std. Error"],
        p = if (ncol(coef_summary) >= 4) coef_summary[, 4] else NA,
        stringsAsFactors = FALSE
      )
    })
  } else {
    stop("Unsupported model type: ", class(model)[1])
  }

  # Convert to data frame and exclude random effects/SD parameters
  coeffs_df <- as.data.frame(params) %>%
    filter(
      !grepl("^SD \\(", Parameter) &
      !grepl("^Cor \\(", Parameter) &
      !grepl("^ICC", Parameter) &
      !grepl("^R2", Parameter) &
      !grepl("^Sigma", Parameter) &
      !grepl("^tau", Parameter) &
      !is.na(Parameter) &
      Parameter != ""
    ) %>%
    mutate(
      outcome = outcome_name,
      # Clean parameter names using centralized function from labelling_utils.R
      Parameter_clean = clean_parameter_names(Parameter),
      # Create significance stars based on raw p-values
      significance = case_when(
        p < 0.001 ~ "***",
        p < 0.01 ~ "**",
        p < 0.05 ~ "*",
        TRUE ~ ""
      ),
      # Format coefficient with significance
      coeff_formatted = sprintf("%.2f%s", Coefficient, significance),
      # Format confidence interval
      ci_formatted = sprintf("[%.2f, %.2f]", CI_low, CI_high),
      # Format p-value
      p_raw_formatted = case_when(
        p < 0.001 ~ sprintf("p<0.001"),
        TRUE ~ sprintf("p=%.3f", p)
      )
    )

  return(coeffs_df)
}


#' Print formatted coefficient table for a single model
#'
#' @param model Fitted model object
#' @param outcome_name Name of the outcome variable
print_all_coefficients <- function(model, outcome_name) {
  # Print formula
  print(formula(model))

  coeffs <- extract_all_coefficients(model, outcome_name)
  if (is.null(coeffs) || nrow(coeffs) == 0) {
    cat("No coefficients found for", outcome_name, "in model.\n")
    return()
  }

  cat(sprintf("\n%s - All Model Coefficients (Fixed Effects Only):\n", outcome_name))

  table_output <- coeffs %>%
    select(Parameter_clean, coeff_formatted, ci_formatted, p_raw_formatted) %>%
    rename(
      "Parameter" = Parameter_clean,
      "Coefficient" = coeff_formatted,
      "95% CI" = ci_formatted,
      "P-value" = p_raw_formatted
    ) %>%
    kable(format = "simple", align = "lccc")

  print(table_output)
}


#' Print complete coefficient section for all models
#'
#' Prints coefficients for all outcomes and model specifications,
#' organized by outcome and model type, for both study types.
#'
#' @param mods_cross_sectional List of cross-sectional models by outcome
#' @param mods_longitudinal List of longitudinal models by outcome
#' @param outcome_vars Vector of outcome variable names
print_model_coefficients_section <- function(mods_cross_sectional,
                                              mods_longitudinal,
                                              outcome_vars) {

  cat("\n")
  cat("================================================================================\n")
  cat("MODEL COEFFICIENTS - ALL OUTCOMES AND MODELS\n")
  cat("================================================================================\n")

  # Model specs to iterate over
  model_specs <- c("additive_coarsened", "additive_continuous", "full_coarsened", "full_continuous")

  for (outcome in outcome_vars) {
    cat("\n=== Outcome:", outcome, "===\n")

    for (spec in model_specs) {
      cat("\n\n--- Model:", spec, "---\n")

      # Cross-sectional
      if (!is.null(mods_cross_sectional[[outcome]][[spec]])) {
        cat("\nCross-Sectional Model Summary:\n")
        print_all_coefficients(mods_cross_sectional[[outcome]][[spec]], outcome)
      }

      # Longitudinal
      if (!is.null(mods_longitudinal[[outcome]][[spec]])) {
        cat("\nLongitudinal Model Summary:\n")
        print_all_coefficients(mods_longitudinal[[outcome]][[spec]], outcome)
      }
    }
  }

  cat("\n")
}
