# =============================================================================
# Regression Utilities
# =============================================================================
# Model fitting functions for regression analysis.
#
# Core Functions:
#   - check_convergence(): Validate model convergence (lm, glm, lmer, glmer)
#   - run_reg_model(): Unified model fitting interface
#   - fit_models(): Fit multiple model specifications for an outcome
#   - run_combined_study_analysis(): Compare cross-sectional vs longitudinal
# =============================================================================

library(lme4)
library(lmerTest)
library(dplyr)
library(parameters)

# Source formula utilities
source("scripts/regressions/formula_utils.R")

# =============================================================================
# MODEL FITTING
# =============================================================================

#' Check convergence of fitted model
#'
#' @param model Fitted model object (lm, glm, lmer, or glmer)
#' @param model_name Optional name for printing
#' @return Logical indicating whether model converged
check_convergence <- function(model, model_name = "Model") {
  if (inherits(model, "lmerMod") || inherits(model, "glmerMod")) {
    code <- model@optinfo$conv$opt
    optimizer <- model@optinfo$optimizer
    cat(
      model_name, ": optimizer (", optimizer, ") convergence code:", code,
      if (code == 0) "(OK)" else "(FAILED)", "\n"
    )
    return(code == 0)
  } else if (inherits(model, "glm")) {
    converged <- model$converged
    cat(model_name, ": glm model -",
        if (converged) "CONVERGED" else "FAILED", "\n")
    return(converged)
  } else if (inherits(model, "lm")) {
    converged <- all(is.finite(coef(model)))
    cat(model_name, ": lm model -",
        if (converged) "CONVERGED" else "FAILED (infinite coefficients)", "\n")
    return(converged)
  } else {
    cat(model_name, ": UNKNOWN MODEL TYPE (", class(model)[1], ")\n")
    return(NA)
  }
}


#' Fit regression model with specified family
#'
#' Unified interface for fitting OLS, logistic, and mixed-effects models.
#'
#' @param data Data frame
#' @param formula Model formula
#' @param model_family Model family: "ols", "binary", "lmer", or "glmer"
#' @param REML Use REML estimation for mixed models (default FALSE)
#' @param optimizer Optimizer for mixed models (default "bobyqa")
#' @param use_weights Use IPW weights (default FALSE)
#' @param verbose Print convergence info (default TRUE)
#' @return Fitted model object
run_reg_model <- function(data,
                          formula,
                          model_family = "ols",
                          REML = FALSE,
                          optimizer = "bobyqa",
                          use_weights = FALSE,
                          verbose = TRUE) {
  if (model_family == "ols") {
    if (use_weights && "ipw_weight_truncated" %in% names(data)) {
      model <- lm(formula, data = data, weights = ipw_weight_truncated)
    } else {
      model <- lm(formula, data = data)
    }
  } else if (model_family == "binary") {
    # Use quasibinomial when weights are used to avoid non-integer warning
    fam <- if (use_weights) quasibinomial(link = "logit") else binomial(link = "logit")
    if (use_weights && "ipw_weight_truncated" %in% names(data)) {
      model <- glm(formula, data = data, family = fam,
                   weights = ipw_weight_truncated)
    } else {
      model <- glm(formula, data = data, family = fam)
    }
  } else if (model_family == "lmer") {
    if (use_weights && "ipw_weight_truncated" %in% names(data)) {
      model <- lmer(formula, data = data, REML = REML, weights = ipw_weight_truncated,
                    control = lmerControl(optimizer = optimizer, optCtrl = list(maxfun = 200000)))
    } else {
      model <- lmer(formula, data = data, REML = REML,
                    control = lmerControl(optimizer = optimizer, optCtrl = list(maxfun = 200000)))
    }
  } else if (model_family == "glmer") {
    if (use_weights && "ipw_weight_truncated" %in% names(data)) {
      model <- glmer(formula, data = data, family = binomial(link = "logit"),
                     weights = ipw_weight_truncated,
                     control = glmerControl(optimizer = optimizer, optCtrl = list(maxfun = 600000)))
    } else {
      model <- glmer(formula, data = data, family = binomial(link = "logit"),
                     control = glmerControl(optimizer = optimizer, optCtrl = list(maxfun = 600000)))
    }
  } else {
    stop("Unsupported model family. Use 'ols', 'binary', 'lmer', or 'glmer'")
  }

  if (verbose) {
    check_convergence(model, "Model")
  }

  return(model)
}


#' Fit multiple model specifications for an outcome
#'
#' Fits additive and full models with different lambda specifications.
#'
#' @param data Data frame
#' @param outcome_var Outcome variable name
#' @param continuous_spec Functional form for lambda: "linear", "quadratic", "cubic"
#' @param add_domain Include domain treatment arm
#' @param add_pre Include pre-treatment covariate
#' @param add_time Include time variable (for longitudinal)
#' @param time_var Time variable name
#' @param model_family Model family for fitting
#' @return Named list of fitted models
fit_models <- function(data,
                       outcome_var,
                       continuous_spec = "linear",
                       add_domain = TRUE,
                       add_pre = FALSE,
                       add_time = FALSE,
                       time_var = NULL,
                       model_family = "ols") {
  mods <- list()

  # Determine actual model family (if ols was entered but add time, use lmer if add_time)
  actual_family <- if (add_time && model_family == "ols") "lmer" else model_family

  # Additive Coarsened (3-level)
  cat("  Fitting additive_coarsened...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = "relationship_seeking_category",
    model_spec = "additive",
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$additive_coarsened <- run_reg_model(data, formula, model_family = actual_family, REML = TRUE)

  # Additive Continuous
  cat("  Fitting additive_continuous...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = "lambda",
    model_spec = "additive",
    continuous_model_spec = continuous_spec,
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$additive_continuous <- run_reg_model(data, formula, model_family = actual_family, REML = TRUE)

  # Full Coarsened (3-level with interactions)
  cat("  Fitting full_coarsened...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = "relationship_seeking_category",
    model_spec = "full",
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$full_coarsened <- run_reg_model(data, formula, model_family = actual_family, REML = TRUE)

  # Full Continuous
  cat("  Fitting full_continuous...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = "lambda",
    model_spec = "full",
    continuous_model_spec = continuous_spec,
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$full_continuous <- run_reg_model(data, formula, model_family = actual_family, REML = TRUE)

  # 5-level factor model (additive)
  cat("  Fitting additive_5level...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = "lambda_factor",
    model_spec = "additive",
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$additive_5level <- run_reg_model(data, formula, model_family = actual_family, REML = TRUE)

  # 5-level factor model (full with interactions)
  cat("  Fitting full_5level...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = "lambda_factor",
    model_spec = "full",
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$full_5level <- run_reg_model(data, formula, model_family = actual_family, REML = TRUE)

  return(mods)
}

# =============================================================================
# COMBINED STUDY ANALYSIS
# =============================================================================

#' Run combined study analysis comparing cross-sectional and longitudinal
#'
#' Combines data from both study types and fits a simple regression testing
#' the effect of study type on the outcome. Cross-sectional is the reference group.
#'
#' @param data_cs Cross-sectional data frame
#' @param data_long Longitudinal data frame
#' @param outcome_var Outcome variable name (string)
#' @param model_family Model family: "ols" or "binary"
#' @return Fitted model object
run_combined_study_analysis <- function(data_cs,
                                         data_long,
                                         outcome_var,
                                         model_family = "ols",
                                         add_pre = FALSE) {
  cat("\n--- Combined Study Model ---\n\n")

  # Combine both studies with study_type factor (cross-sectional as reference)
  data_combined <- bind_rows(
    data_cs %>% mutate(study_type = "cross-sectional"),
    data_long %>% mutate(study_type = "longitudinal")
  ) %>%
    mutate(study_type = factor(study_type,
                               levels = c("cross-sectional", "longitudinal")))

  cat("Combined data:", nrow(data_combined), "observations\n")
  cat("  Cross-sectional:",
      sum(data_combined$study_type == "cross-sectional"), "\n")
  cat("  Longitudinal:",
      sum(data_combined$study_type == "longitudinal"), "\n")

  # Build formula
  pre_var <- paste0(outcome_var, "_pre")
  if (add_pre && pre_var %in% names(data_combined)) {
    formula <- as.formula(paste(outcome_var, "~", pre_var, "+ study_type"))
    cat("Formula includes pre-score:", pre_var, "\n")
  } else {
    formula <- as.formula(paste(outcome_var, "~ study_type"))
  }

  # Fit model based on family
  if (model_family == "binary") {
    combined_model <- glm(formula, data = data_combined,
                          family = binomial(link = "logit"))
  } else {
    combined_model <- lm(formula, data = data_combined)
  }

  cat("\nCombined Model Summary:\n")
  print(summary(combined_model))

  # Extract and print parameters (use odds ratios for binary models)
  use_or <- model_family == "binary"
  combined_params <- parameters::model_parameters(combined_model,
                                                   exponentiate = use_or)
  cat("\nModel Parameters", if (use_or) "(Odds Ratios)" else "", ":\n")
  print(combined_params)

  return(combined_model)
}
