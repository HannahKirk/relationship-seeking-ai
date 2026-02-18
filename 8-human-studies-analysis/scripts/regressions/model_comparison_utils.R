# =============================================================================
# Model Comparison Utilities
# =============================================================================
# Functions for comparing model specifications and selecting best fits.
#
# =============================================================================

library(performance)

# Source dependencies (regression_utils.R sources formula_utils.R)
source("scripts/regressions/regression_utils.R")

#' Compare performance of fitted models
#'
#' Core comparison function using performance::compare_performance().
#' Computes RMSE, AIC_wt, AICc_wt, BIC_wt, Performance_Score.
#'
#' @param models Named list of fitted model objects
#' @param metrics Metrics to compute (default: AIC, AICc, BIC, RMSE)
#' @param rank Whether to rank models by Performance_Score (default TRUE)
#' @return Data frame with Name, AIC, AICc, BIC, RMSE, AIC_wt, AICc_wt,
#'   BIC_wt, Performance_Score
compare_models <- function(models,
                           metrics = c("AIC", "AICc", "BIC", "RMSE"),
                           rank = TRUE) {
  perf <- do.call(performance::compare_performance,
                  c(models, list(rank = rank, metrics = metrics)))
  perf_df <- as.data.frame(perf)

  # Map generic names (model1, model2, ...) back to list names
  if ("Name" %in% names(perf_df)) {
    model_names <- names(models)
    for (i in seq_along(model_names)) {
      perf_df$Name[perf_df$Name == paste0("model", i)] <- model_names[i]
    }
  }

  # Keep key columns in stable order
  keep_cols <- intersect(
    c("Name", "Model", "AIC", "AICc", "BIC", "RMSE",
      "AIC_wt", "AICc_wt", "BIC_wt", "Performance_Score"),
    names(perf_df))
  perf_df <- perf_df[, keep_cols, drop = FALSE]

  return(perf_df)
}


#' Compare functional forms (linear/quadratic/cubic) for one outcome
#'
#' Fits models with linear, quadratic, cubic lambda, compares
#' using compare_models(), and returns the best specification.
#'
#' Works for both non-pooled outcomes (build_formula) and pooled
#' constructs (build_formula_pooled).
#'
#' @param data Data frame
#' @param outcome_var Outcome variable (or "outcome_value" for pooled)
#' @param add_domain Include domain arm
#' @param add_time Include time variable
#' @param time_var Time variable name
#' @param model_family Model family ("ols" or "lmer")
#' @param pooled Use build_formula_pooled (default FALSE)
#' @param heterogeneity Include outcome_measure heterogeneity (pooled only)
#' @param select_by Selection criterion: "AIC" (default) or "BIC"
#' @return List with $comparison (data frame) and $best_spec (character)
compare_functional_forms <- function(data,
                                      outcome_var,
                                      add_domain = TRUE,
                                      add_pre = FALSE,
                                      add_time = FALSE,
                                      time_var = NULL,
                                      model_family = "ols",
                                      pooled = FALSE,
                                      heterogeneity = FALSE,
                                      select_by = "AIC") {
  actual_family <- if (add_time && model_family == "ols") "lmer" else model_family
  specs <- c("linear", "quadratic", "cubic")
  models <- list()

  for (spec in specs) {
    cat(sprintf("  Fitting %s...\n", spec))

    if (pooled) {
      formula <- build_formula_pooled(
        outcome = outcome_var,
        rs_variable = "lambda",
        model_spec = "full",
        continuous_model_spec = spec,
        heterogeneity = heterogeneity,
        add_domain = add_domain,
        add_pre = add_pre,
        add_time = add_time,
        time_var = time_var
      )
    } else {
      formula <- build_formula(
        outcome_var = outcome_var,
        rs_variable = "lambda",
        model_spec = "full",
        continuous_model_spec = spec,
        add_domain = add_domain,
        add_pre = add_pre,
        add_time = add_time,
        time_var = time_var
      )
    }

    models[[spec]] <- run_reg_model(
      data, formula,
      model_family = actual_family,
      REML = FALSE,  # ML for model comparison
      verbose = FALSE
    )
  }

  # Compare using performance package
  comparison <- compare_models(models)

  # Select best by criterion
  if (select_by == "AIC" && "AIC" %in% names(comparison)) {
    best_idx <- which.min(comparison$AIC)
  } else if (select_by == "BIC" && "BIC" %in% names(comparison)) {
    best_idx <- which.min(comparison$BIC)
  } else {
    best_idx <- which.max(comparison$Performance_Score)
  }
  best_spec <- comparison$Name[best_idx]

  return(list(
    comparison = comparison,
    best_spec = best_spec
  ))
}


#' Compare all model specifications for a construct
#'
#' Takes the already-fitted models (from fit_models())
#' and compares performance. No model selection, just comparison table.
#'
#' @param models_list Named list containing full_continuous,
#'   full_coarsened, full_5level (and optionally additive variants)
#' @return Data frame with comparison metrics
compare_full_specs <- function(models_list) {
  spec_order <- c("full_continuous", "full_coarsened",
                   "full_5level")
  available <- intersect(spec_order, names(models_list))
  models <- models_list[available]

  comparison <- compare_models(models)
  return(comparison)
}


#' Compute full specification comparison for all outcomes
#'
#' Loops over outcomes and calls compare_full_specs() for each.
#' Returns comparison data for both study types (if provided).
#'
#' @param mods_cs Cross-sectional models (list of lists by outcome), or NULL
#' @param mods_long Longitudinal models (list of lists by outcome), or NULL
#' @param outcome_vars Character vector of outcome variable names
#' @return List with $full_spec_cs and $full_spec_long (each a list by outcome)
compute_full_spec_comparison <- function(mods_cs, mods_long, outcome_vars) {
  full_spec_cs <- if (!is.null(mods_cs)) list() else NULL
  full_spec_long <- if (!is.null(mods_long)) list() else NULL

  for (outcome in outcome_vars) {
    if (!is.null(mods_cs) && !is.null(mods_cs[[outcome]])) {
      full_spec_cs[[outcome]] <- compare_full_specs(mods_cs[[outcome]])
    }
    if (!is.null(mods_long) && !is.null(mods_long[[outcome]])) {
      full_spec_long[[outcome]] <- compare_full_specs(mods_long[[outcome]])
    }
  }

  list(
    full_spec_cs = full_spec_cs,
    full_spec_long = full_spec_long
  )
}