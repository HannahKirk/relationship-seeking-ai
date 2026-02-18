# =============================================================================
# Calibration Study Regression Utilities
# =============================================================================
#
# Functions for fitting and comparing calibration study models:
# - Rating models: lmer with (1 | ppt_id) random effects
# - Ranking models: PlackettLuce for rank-ordered data

# =============================================================================

library(lme4)
library(lmerTest)
library(PlackettLuce)
library(performance)
library(broom)
library(ggeffects)

# Helper for string concatenation (used in LaTeX table generation)
`%+%` <- function(a, b) paste0(a, b)

# =============================================================================
# RATING MODEL FUNCTIONS (lmer)
# =============================================================================

#' Fit null model for ratings
#'
#' @param data Data frame with outcome and ppt_id columns
#' @param outcome_var Name of outcome variable
#' @return lmerMod object
fit_null_rating <- function(data, outcome_var) {
  formula <- reformulate("1 + (1 | ppt_id)", response = outcome_var)
  lmerTest::lmer(formula, data = data, REML = TRUE)
}

#' Fit linear rating model
#'
#' @param data Data frame
#' @param outcome_var Name of outcome variable
#' @param include_conversation_mode Include conversation_mode predictor (default TRUE)
#' @param remove_extreme Remove extreme multiplier values (default FALSE)
#' @return lmerMod object
fit_linear_rating <- function(data, outcome_var, include_conversation_mode = TRUE,
                              remove_extreme = FALSE) {
  if (remove_extreme) {
    data <- data %>% filter(!multiplier %in% c(-1.5, 1.5))
  }

  if (include_conversation_mode) {
    formula <- reformulate("conversation_mode + multiplier + (1 | ppt_id)",
                          response = outcome_var)
  } else {
    formula <- reformulate("multiplier + (1 | ppt_id)", response = outcome_var)
  }

  lmerTest::lmer(formula, data = data, REML = TRUE)
}

#' Fit quadratic rating model
#'
#' @param data Data frame
#' @param outcome_var Name of outcome variable
#' @param include_conversation_mode Include conversation_mode predictor (default TRUE)
#' @param remove_extreme Remove extreme multiplier values (default FALSE)
#' @return lmerMod object
fit_quadratic_rating <- function(data, outcome_var, include_conversation_mode = TRUE,
                                 remove_extreme = FALSE) {
  if (remove_extreme) {
    data <- data %>% filter(!multiplier %in% c(-1.5, 1.5))
  }

  if (include_conversation_mode) {
    formula <- reformulate("conversation_mode + multiplier + I(multiplier^2) + (1 | ppt_id)",
                          response = outcome_var)
  } else {
    formula <- reformulate("multiplier + I(multiplier^2) + (1 | ppt_id)",
                          response = outcome_var)
  }

  lmerTest::lmer(formula, data = data, REML = TRUE)
}

#' Fit nonlinear (factor) rating model
#'
#' @param data Data frame
#' @param outcome_var Name of outcome variable
#' @param include_conversation_mode Include conversation_mode predictor (default TRUE)
#' @param ref_level Reference level for multiplier_factor (default "neg1.5")
#' @return lmerMod object
fit_nonlinear_rating <- function(data, outcome_var, include_conversation_mode = TRUE,
                                 ref_level = "zero") {
  # Create factor with proper levels
  data$multiplier_factor <- factor(
    data$multiplier,
    levels = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5),
    labels = c("neg1.5", "neg1", "neg0.5", "zero", "pos0.5", "pos1", "pos1.5")
  )
  data$multiplier_factor <- relevel(data$multiplier_factor, ref = ref_level)

  if (include_conversation_mode) {
    formula <- reformulate("conversation_mode + multiplier_factor + (1 | ppt_id)",
                          response = outcome_var)
  } else {
    formula <- reformulate("multiplier_factor + (1 | ppt_id)", response = outcome_var)
  }

  lmerTest::lmer(formula, data = data, REML = TRUE)
}

#' Fit all rating models for an outcome
#'
#' @param data Data frame
#' @param outcome_var Name of outcome variable
#' @param include_conversation_mode Include conversation_mode predictor (default TRUE)
#' @return List of models: $null, $linear, $truncated_linear, $quadratic,
#'         $truncated_quadratic, $nonlinear
fit_rating_models <- function(data, outcome_var, include_conversation_mode = TRUE) {
  cat(sprintf("\nFitting rating models for: %s\n", outcome_var))

  models <- list(
    null = fit_null_rating(data, outcome_var),
    linear = fit_linear_rating(data, outcome_var, include_conversation_mode, FALSE),
    truncated_linear = fit_linear_rating(data, outcome_var, include_conversation_mode, TRUE),
    quadratic = fit_quadratic_rating(data, outcome_var, include_conversation_mode, FALSE),
    truncated_quadratic = fit_quadratic_rating(data, outcome_var, include_conversation_mode, TRUE),
    nonlinear = fit_nonlinear_rating(data, outcome_var, include_conversation_mode)
  )

  return(models)
}

# =============================================================================
# PLACKETT-LUCE RANKING MODEL FUNCTIONS
# =============================================================================

#' Prepare data for PlackettLuce
#'
#' Converts ranking data from long format to the rankings matrix format
#' expected by PlackettLuce.
#'
#' @param data Data frame with columns: ppt_id, multiplier, rank
#'        (where rank is 1-4, with 4=best in our data)
#' @return rankings object for PlackettLuce
prepare_plackett_luce_data <- function(data) {
  # Create item labels from multipliers and ensure rank is numeric
  # NOTE: PlackettLuce expects rank 1 = BEST, but our data has rank 4 = best

  # So we invert: new_rank = (max_rank + 1) - old_rank
  max_rank <- 4
  data <- data %>%
    mutate(
      item = paste0("m_", multiplier),
      rank = as.numeric(as.character(rank)),
      rank = (max_rank + 1) - rank  # Invert so 1 = best for PlackettLuce
    )

  # Create wide format: rows = participants, columns = items, values = ranks
  # NA for items not ranked by that participant
  rankings_wide <- data %>%
    select(ppt_id, item, rank) %>%
    tidyr::pivot_wider(names_from = item, values_from = rank) %>%
    select(-ppt_id)

  # Convert to numeric matrix
  rankings_matrix <- as.matrix(rankings_wide)
  storage.mode(rankings_matrix) <- "numeric"

  # Convert to rankings object using "rankings" input format
  rankings <- as.rankings(rankings_matrix, input = "rankings")

  return(rankings)
}

#' Fit null PlackettLuce model
#'
#' @param rankings grouped_rankings object from prepare_plackett_luce_data()
#' @return PlackettLuce object
fit_null_plackett_luce <- function(rankings) {
  PlackettLuce(rankings)
}

#' Fit PlackettLuce model with multiplier as covariate
#'
#' @param data Data frame
#' @param model_type Type of model: "linear", "quadratic", "nonlinear"
#' @param remove_extreme Remove extreme multiplier values (default FALSE)
#' @param ref_level Reference level for factor (default "zero")
#' @return PlackettLuce object
fit_plackett_luce <- function(data, model_type = "linear", remove_extreme = FALSE,
                              ref_level = "zero") {
  if (remove_extreme) {
    data <- data %>% filter(!multiplier %in% c(-1.5, 1.5))
  }

  # Prepare rankings - this creates the rankings object
  rankings <- prepare_plackett_luce_data(data)

  # Get item names from the rankings object (column names of the wide matrix)
  item_names <- colnames(attr(rankings, "rankings"))
  if (is.null(item_names)) {
    # Fallback: extract from items attribute
    item_names <- attr(rankings, "items")
  }

  # Extract numeric multipliers from item names (format: "m_X" where X is multiplier)
  multipliers <- as.numeric(gsub("m_", "", item_names))

  # Create item covariates based on model type
  if (model_type == "nonlinear") {
    # Factor model: separate worth parameter for each multiplier
    model <- PlackettLuce(rankings, maxit = 500)
  } else {
    # Create item covariate data frame - rownames must match item names
    item_data <- data.frame(
      multiplier = multipliers,
      multiplier_sq = multipliers^2,
      row.names = item_names
    )

    if (model_type == "linear") {
      model <- PlackettLuce(rankings, itemcov = item_data,
                            formula = ~ multiplier, maxit = 500)
    } else if (model_type == "quadratic") {
      model <- PlackettLuce(rankings, itemcov = item_data,
                            formula = ~ multiplier + multiplier_sq, maxit = 500)
    }
  }

  return(model)
}

#' Fit PlackettLuce ranking models for an outcome
#'
#' Fits two models:
#' - full: all multipliers (-1.5 to +1.5)
#' - truncated: extreme multipliers (±1.5) removed from data before fitting
#'
#' @param data Data frame with ranking data (long format)
#' @param outcome_var Name of outcome column (typically "rank")
#' @return List with $full and $truncated models
fit_ranking_models <- function(data, outcome_var = "rank") {

  cat("\nFitting PlackettLuce ranking models\n")

  # Full model with all multipliers
  cat("  Fitting full model (all multipliers)...\n")
  full_model <- fit_plackett_luce(data, "nonlinear", remove_extreme = FALSE)

 # Truncated model: remove ±1.5 from data BEFORE fitting
  cat("  Fitting truncated model (excluding ±1.5)...\n")
  truncated_model <- fit_plackett_luce(data, "nonlinear", remove_extreme = TRUE)

  models <- list(
    full = full_model,
    truncated = truncated_model
  )

  return(models)
}

# =============================================================================
# MODEL COMPARISON FUNCTIONS
# =============================================================================

#' Compare model performance for rating models
#'
#' @param models_list List of lmerMod models
#' @param model_names Optional vector of model names
#' @return Data frame with AIC, AICc, BIC, RMSE, weights, performance score
compare_rating_model_performance <- function(models_list, model_names = NULL) {
  if (is.null(model_names)) {
    model_names <- c("Null", "Linear", "Truncated Linear",
                     "Quadratic", "Truncated Quadratic", "Non-linear")
  }

  # Extract metrics manually for each model
  perf_df <- data.frame(
    Name = model_names,
    stringsAsFactors = FALSE
  )

  perf_df$AIC <- sapply(models_list, AIC)
  perf_df$BIC <- sapply(models_list, BIC)
  perf_df$LogLik <- sapply(models_list, function(x) as.numeric(logLik(x)))

  # Calculate AICc manually
  perf_df$AICc <- sapply(seq_along(models_list), function(i) {
    model <- models_list[[i]]
    n <- nobs(model)
    k <- length(fixef(model)) + 1  # +1 for random effect variance
    aic_val <- perf_df$AIC[i]
    if (n - k - 1 > 0) {
      aic_val + (2 * k^2 + 2 * k) / (n - k - 1)
    } else {
      aic_val
    }
  })

  # Calculate RMSE
  perf_df$RMSE <- sapply(models_list, function(model) {
    resids <- residuals(model)
    sqrt(mean(resids^2, na.rm = TRUE))
  })

  # Calculate weights
  perf_df$Delta_AIC <- perf_df$AIC - min(perf_df$AIC)
  perf_df$Delta_AICc <- perf_df$AICc - min(perf_df$AICc)
  perf_df$Delta_BIC <- perf_df$BIC - min(perf_df$BIC)

  perf_df$AIC_wt <- exp(-0.5 * perf_df$Delta_AIC) / sum(exp(-0.5 * perf_df$Delta_AIC))
  perf_df$AICc_wt <- exp(-0.5 * perf_df$Delta_AICc) / sum(exp(-0.5 * perf_df$Delta_AICc))
  perf_df$BIC_wt <- exp(-0.5 * perf_df$Delta_BIC) / sum(exp(-0.5 * perf_df$Delta_BIC))

  perf_df$Performance_Score <- (perf_df$AIC_wt + perf_df$AICc_wt + perf_df$BIC_wt) / 3

  # Sort by AIC
  perf_df <- perf_df[order(perf_df$AIC), ]
  rownames(perf_df) <- NULL

  return(perf_df)
}

#' Compare model performance for PlackettLuce models
#'
#' @param models_list List of PlackettLuce models
#' @param model_names Optional vector of model names
#' @return Data frame with AIC, BIC, LogLik, weights, performance score
compare_ranking_model_performance <- function(models_list, model_names = NULL) {
  if (is.null(model_names)) {
    model_names <- c("Null", "Linear", "Truncated Linear",
                     "Quadratic", "Truncated Quadratic", "Non-linear")
  }

  # Extract metrics for each model
  perf_df <- data.frame(
    Name = model_names,
    stringsAsFactors = FALSE
  )

  perf_df$AIC <- sapply(models_list, AIC)
  perf_df$BIC <- sapply(models_list, BIC)
  perf_df$LogLik <- sapply(models_list, function(x) as.numeric(logLik(x)))
  perf_df$Deviance <- sapply(models_list, function(x) -2 * as.numeric(logLik(x)))

  # Get number of observations from PlackettLuce model
  get_nobs_pl <- function(model) {
    length(model$rankings)
  }

  # Calculate AICc
  perf_df$AICc <- sapply(seq_along(models_list), function(i) {
    model <- models_list[[i]]
    n <- get_nobs_pl(model)
    k <- length(coef(model))
    aic_val <- perf_df$AIC[i]
    if (!is.null(n) && n - k - 1 > 0) {
      aic_val + (2 * k^2 + 2 * k) / (n - k - 1)
    } else {
      aic_val
    }
  })

  # Store N for each model
  perf_df$N <- sapply(models_list, get_nobs_pl)

  # Calculate delta values and weights
  perf_df$Delta_AIC <- perf_df$AIC - min(perf_df$AIC)
  perf_df$Delta_AICc <- perf_df$AICc - min(perf_df$AICc)
  perf_df$Delta_BIC <- perf_df$BIC - min(perf_df$BIC)

  perf_df$AIC_wt <- exp(-0.5 * perf_df$Delta_AIC) / sum(exp(-0.5 * perf_df$Delta_AIC))
  perf_df$AICc_wt <- exp(-0.5 * perf_df$Delta_AICc) / sum(exp(-0.5 * perf_df$Delta_AICc))
  perf_df$BIC_wt <- exp(-0.5 * perf_df$Delta_BIC) / sum(exp(-0.5 * perf_df$Delta_BIC))

  perf_df$Performance_Score <- (perf_df$AIC_wt + perf_df$AICc_wt + perf_df$BIC_wt) / 3

  # Sort by AIC
  perf_df <- perf_df[order(perf_df$AIC), ]
  rownames(perf_df) <- NULL

  return(perf_df)
}

#' Select best model based on criterion
#'
#' @param comparison_df Performance comparison data frame
#' @param criterion Selection criterion: "AIC", "AICc", "BIC", "Performance_Score"
#' @return Name of best model
select_best_model <- function(comparison_df, criterion = "AIC") {
  if (criterion == "Performance_Score") {
    best_idx <- which.max(comparison_df$Performance_Score)
  } else {
    best_idx <- which.min(comparison_df[[criterion]])
  }
  return(comparison_df$Name[best_idx])
}

# =============================================================================
# MARGINAL EFFECTS FUNCTIONS
# =============================================================================

#' Get marginal effects for rating model
#'
#' @param model lmerMod object
#' @param multiplier_range Numeric vector of multiplier values
#' @return Data frame with predicted means and CIs
get_rating_marginal_effects <- function(model, multiplier_range = seq(-1.5, 1.5, 0.1)) {
  effects <- ggeffects::ggpredict(model, terms = "multiplier [all]")
  as.data.frame(effects)
}

#' Get marginal effects for PlackettLuce model
#'
#' @param model PlackettLuce object
#' @return Data frame with worth parameters per multiplier
get_ranking_marginal_effects <- function(model) {
  # Extract worth parameters
  worths <- coef(model, log = FALSE)

  data.frame(
    multiplier = as.numeric(names(worths)),
    worth = as.numeric(worths),
    log_worth = log(as.numeric(worths))
  )
}

