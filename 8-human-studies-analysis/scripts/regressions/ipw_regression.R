# =============================================================================
# IPW Regression: Inverse Probability Weighting for Dropout
# =============================================================================
# Fits a logistic regression to predict post-treatment dropout and generates
# inverse probability weights (IPW) for use in downstream analyses.
#
# Usage:
#   source("scripts/regressions/ipw_regression.R")
#   result <- fit_ipw_regression(data, study_type = "longitudinal")
#   ipw_weights <- result$ipw_weights
#
# =============================================================================

# =============================================================================
# MODEL SPECIFICATIONS
# =============================================================================
# The IPW model predicts P(dropout | baseline covariates)
# Weights are calculated as: 1 / P(observed outcome)
#   - For completers: 1 / (1 - P(dropout))
#   - For dropouts: 1 / P(dropout)
# =============================================================================

# Covariate sets
CATEGORICAL_VARS <- c(
  "gender_binary",
  "ethnicity_binary",
  "religion_binary",
  "disability_binary",
  "income_binary",
  "ai_frequency_coarsened"
)

CONTINUOUS_VARS_BASE <- c(
  "age",
  "education_years",
  "pre_psychosocial_F1",
  "pre_psychosocial_F2"
)

CONTINUOUS_VARS_LONGITUDINAL <- c(
  "seeking_companionship_likelihood",
  "mas_score",
  "gcs_score",
  "charity_amount_gbp",
  "emotional_competency",
  "political_competency"
)

# =============================================================================
# MAIN FITTING FUNCTION
# =============================================================================

fit_ipw_regression <- function(data,
                               study_type = c("cross-sectional", "longitudinal"),
                               weight_truncation = 10,
                               verbose = TRUE) {
  #' Fit IPW regression model and calculate weights
  #'

  #' @param data Data frame with post_treatment_dropout and covariates
#' @param study_type Either "cross-sectional" or "longitudinal"
  #' @param weight_truncation Maximum weight value (default 10)
  #' @param verbose Print progress messages
  #'
  #' @return List containing:
  #'   - model: The fitted glm object
  #'   - ipw_weights: Data frame with ppt_id and weights
  #'   - formula: The model formula used
  #'   - diagnostics: Weight distribution statistics

  study_type <- match.arg(study_type)

  # Build covariate list based on study type
  if (study_type == "longitudinal") {
    continuous_vars <- c(CONTINUOUS_VARS_BASE, CONTINUOUS_VARS_LONGITUDINAL)
  } else {
    continuous_vars <- CONTINUOUS_VARS_BASE
  }

  all_covariates <- c(CATEGORICAL_VARS, continuous_vars)

  # Build formula
  formula_str <- paste("post_treatment_dropout ~",
                       paste(all_covariates, collapse = " + "))
  model_formula <- as.formula(formula_str)

  if (verbose) {
    cat("IPW REGRESSION MODEL\n")
    cat(rep("=", 60), "\n", sep = "")
    cat("Study type:", study_type, "\n")
    cat("Formula:", formula_str, "\n\n")
  }

  # Identify complete cases
  complete_vars <- c("post_treatment_dropout", "ppt_id", all_covariates)
  complete_cases <- complete.cases(data[, complete_vars, drop = FALSE])
  data_complete <- data[complete_cases, ]

  if (verbose) {
    cat("Sample size:\n")
    cat("  Total:", nrow(data), "\n")
    cat("  Complete cases:", nrow(data_complete),
        sprintf("(%.1f%%)\n", 100 * nrow(data_complete) / nrow(data)))
    cat("  Excluded (missing):", nrow(data) - nrow(data_complete), "\n\n")
  }

  # Fit logistic regression
  model <- glm(model_formula, data = data_complete, family = binomial)

  # Calculate predicted probabilities
  data_complete$dropout_prob <- predict(model, type = "response")

  # Calculate IPW weights
  # Weight = 1 / P(being in observed group)
  data_complete$ipw_weight <- ifelse(
    data_complete$post_treatment_dropout,
    1 / data_complete$dropout_prob,           # For dropouts
    1 / (1 - data_complete$dropout_prob)      # For completers
  )

  # Calculate diagnostics before truncation
  diagnostics <- list(
    mean_weight_raw = mean(data_complete$ipw_weight, na.rm = TRUE),
    min_weight = min(data_complete$ipw_weight, na.rm = TRUE),
    max_weight_raw = max(data_complete$ipw_weight, na.rm = TRUE),
    n_extreme = sum(data_complete$ipw_weight > weight_truncation, na.rm = TRUE)
  )

  if (verbose) {
    cat("IPW Weight Distribution (before truncation):\n")
    cat("  Mean:", round(diagnostics$mean_weight_raw, 3), "\n")
    cat("  Range:", round(diagnostics$min_weight, 3), "to",
        round(diagnostics$max_weight_raw, 3), "\n")
    cat("  Weights >", weight_truncation, ":", diagnostics$n_extreme, "\n\n")
  }

  # Truncate extreme weights
  data_complete$ipw_weight_truncated <- pmin(
    data_complete$ipw_weight,
    weight_truncation
  )

  diagnostics$mean_weight_truncated <- mean(
    data_complete$ipw_weight_truncated, na.rm = TRUE
  )
  diagnostics$max_weight_truncated <- max(
    data_complete$ipw_weight_truncated, na.rm = TRUE
  )

  if (verbose && diagnostics$n_extreme > 0) {
    cat("After truncation at", weight_truncation, ":\n")
    cat("  Mean:", round(diagnostics$mean_weight_truncated, 3), "\n")
    cat("  Max:", round(diagnostics$max_weight_truncated, 3), "\n\n")
  }

  # Create output weights dataframe
  ipw_weights <- data_complete %>%
    select(ppt_id, dropout_prob, ipw_weight, ipw_weight_truncated)

  return(list(
    model = model,
    ipw_weights = ipw_weights,
    formula = model_formula,
    diagnostics = diagnostics,
    study_type = study_type,
    n_complete = nrow(data_complete),
    n_total = nrow(data)
  ))
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

get_ipw_model_summary <- function(ipw_result) {
  #' Extract tidy model summary from IPW regression result
  #'
  #' @param ipw_result Output from fit_ipw_regression()
  #' @return Tidy data frame of model coefficients

  broom::tidy(ipw_result$model, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(
      odds_ratio = estimate,
      ci_lower = conf.low,
      ci_upper = conf.high
    ) %>%
    select(term, odds_ratio, ci_lower, ci_upper, p.value)
}

get_ipw_covariates <- function(study_type = c("cross-sectional", "longitudinal")) {
  #' Get list of covariates used in IPW model
  #'
  #' @param study_type Either "cross-sectional" or "longitudinal"
  #' @return Character vector of covariate names

  study_type <- match.arg(study_type)

  if (study_type == "longitudinal") {
    continuous_vars <- c(CONTINUOUS_VARS_BASE, CONTINUOUS_VARS_LONGITUDINAL)
  } else {
    continuous_vars <- CONTINUOUS_VARS_BASE
  }

  return(list(
    categorical = CATEGORICAL_VARS,
    continuous = continuous_vars,
    all = c(CATEGORICAL_VARS, continuous_vars)
  ))
}
