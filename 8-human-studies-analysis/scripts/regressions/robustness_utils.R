# =============================================================================
# Robustness Analysis Utilities
# =============================================================================
# Functions for fitting robustness model specifications and extracting results.
# =============================================================================

library(dplyr)
library(parameters)

# Source dependencies
source("scripts/regressions/regression_utils.R")

#' Fit robustness model specifications
#'
#' Fits a set of model specifications for robustness checking:
#' - additive: base additive model (no interactions)
#' - full: additive + pre-reg treatment interactions
#' - demos: full + demographic controls
#' - prefs: full + preference group controls
#' - weighted: full model with IPW weights
#'
#' @param data Data frame
#' @param outcome_var Outcome variable name
#' @param rs_variable Which RS variable to use
#' @param continuous_spec Functional form for lambda
#' @param add_domain Include domain treatment arm
#' @param add_time Include time variable
#' @param time_var Time variable name
#' @param model_family Model family for fitting
#' @param use_weights Whether to fit weighted models
#' @param demo_vars Demographic variable names
#' @param pref_var Preference group variable name
#' @return Named list of fitted models
fit_robustness_models <- function(data,
                                   outcome_var,
                                   rs_variable = "lambda",
                                   continuous_spec = "linear",
                                   add_domain = TRUE,
                                   add_pre = FALSE,
                                   add_time = FALSE,
                                   time_var = NULL,
                                   model_family = "ols",
                                   use_weights = FALSE,
                                   demo_vars = c("age", "gender_binary", "education_years",
                                                 "ai_frequency_coarsened", "ethnicity_binary",
                                                 "income_binary", "religion_binary"),
                                   pref_var = "cluster_name") {
  mods <- list()
  actual_family <- if (add_time && model_family == "ols") "lmer" else model_family

  # Check which control variables are available in data
  available_demo_vars <- intersect(demo_vars, names(data))
  has_prefs <- pref_var %in% names(data)
  has_weights <- "ipw_weight_truncated" %in% names(data)

  if (length(available_demo_vars) < length(demo_vars)) {
    missing <- setdiff(demo_vars, available_demo_vars)
    cat("  Note: Missing demographic vars:", paste(missing, collapse = ", "), "\n")
  }

  # 1. ADDITIVE: Base additive model (no interactions)
  cat("  Fitting additive...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = rs_variable,
    model_spec = "additive",
    continuous_model_spec = continuous_spec,
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$additive <- run_reg_model(data, formula, model_family = actual_family,
                                  REML = TRUE, use_weights = FALSE)

  # 2. FULL: Additive + treatment interactions
  cat("  Fitting full (+ interactions)...\n")
  formula <- build_formula(
    outcome_var = outcome_var,
    rs_variable = rs_variable,
    model_spec = "full",
    continuous_model_spec = continuous_spec,
    add_domain = add_domain,
    add_pre = add_pre,
    add_time = add_time,
    time_var = time_var
  )
  mods$full <- run_reg_model(data, formula, model_family = actual_family,
                              REML = TRUE, use_weights = FALSE)

  # 3. DEMOS: Full + demographic controls
  if (length(available_demo_vars) > 0) {
    cat("  Fitting demos (full + demographics)...\n")
    formula <- build_formula(
      outcome_var = outcome_var,
      rs_variable = rs_variable,
      model_spec = "full",
      continuous_model_spec = continuous_spec,
      add_domain = add_domain,
      add_pre = add_pre,
      add_time = add_time,
      time_var = time_var,
      add_demos = TRUE,
      demo_vars = available_demo_vars
    )
    mods$demos <- run_reg_model(data, formula, model_family = actual_family,
                                 REML = TRUE, use_weights = FALSE)
  }

  # 4. PREFS: Full + preference group controls
  if (has_prefs) {
    cat("  Fitting prefs (full + preferences)...\n")
    formula <- build_formula(
      outcome_var = outcome_var,
      rs_variable = rs_variable,
      model_spec = "full",
      continuous_model_spec = continuous_spec,
      add_domain = add_domain,
      add_pre = add_pre,
      add_time = add_time,
      time_var = time_var,
      add_prefs = TRUE,
      pref_var = pref_var
    )
    mods$prefs <- run_reg_model(data, formula, model_family = actual_family,
                                 REML = TRUE, use_weights = FALSE)
  }

  # 5. WEIGHTED: Full model with IPW weights (only if requested and available)
  if (use_weights && has_weights) {
    cat("  Fitting weighted (full + IPW)...\n")
    formula <- build_formula(
      outcome_var = outcome_var,
      rs_variable = rs_variable,
      model_spec = "full",
      continuous_model_spec = continuous_spec,
      add_domain = add_domain,
      add_pre = add_pre,
      add_time = add_time,
      time_var = time_var
    )
    mods$weighted <- run_reg_model(data, formula, model_family = actual_family,
                                    REML = TRUE, use_weights = TRUE)
  }

  return(mods)
}


#' Fit robustness models for pooled constructs
#'
#' Like fit_robustness_models but for pooled data (multiple outcomes stacked).
#' Always uses lmer with random intercepts/slopes.
#'
#' @param data Pooled data frame with outcome_value and outcome_measure columns
#' @param rs_variable RS variable name
#' @param continuous_spec Continuous lambda specification
#' @param heterogeneity Include outcome_measure × treatment interactions
#' @param add_domain Include domain arm
#' @param add_time Include time variable
#' @param time_var Time variable name
#' @param use_weights Use IPW weights
#' @param demo_vars Demographic variable names
#' @param pref_var Preference group variable name
#' @return Named list of fitted models
fit_robustness_models_pooled <- function(
    data,
    rs_variable = "lambda",
    continuous_spec = "linear",
    heterogeneity = FALSE,
    add_domain = TRUE,
    add_time = FALSE,
    time_var = NULL,
    use_weights = FALSE,
    demo_vars = c("age", "gender_binary", "education_years",
                  "ai_frequency_coarsened", "ethnicity_binary",
                  "income_binary", "religion_binary"),
    pref_var = "cluster_name") {

  mods <- list()

  # Check available controls
  available_demo_vars <- intersect(demo_vars, names(data))
  has_prefs <- pref_var %in% names(data)
  has_weights <- "ipw_weight_truncated" %in% names(data)

  if (length(available_demo_vars) < length(demo_vars)) {
    missing <- setdiff(demo_vars, available_demo_vars)
    cat("  Note: Missing demographic vars:", paste(missing, collapse = ", "), "\n")
  }

  # 1. ADDITIVE: Base additive model (no interactions)
  cat("  Fitting additive...\n")
  formula <- build_formula_pooled(
    outcome = "outcome_value",
    rs_variable = rs_variable,
    model_spec = "additive",
    continuous_model_spec = continuous_spec,
    heterogeneity = heterogeneity,
    add_domain = add_domain,
    add_time = add_time,
    time_var = time_var
  )
  mods$additive <- run_reg_model(data, formula, model_family = "lmer",
                                  REML = TRUE, use_weights = FALSE)

  # 2. FULL: Additive + treatment interactions
  cat("  Fitting full (+ interactions)...\n")
  formula <- build_formula_pooled(
    outcome = "outcome_value",
    rs_variable = rs_variable,
    model_spec = "full",
    continuous_model_spec = continuous_spec,
    heterogeneity = heterogeneity,
    add_domain = add_domain,
    add_time = add_time,
    time_var = time_var
  )
  mods$full <- run_reg_model(data, formula, model_family = "lmer",
                              REML = TRUE, use_weights = FALSE)

  # 3. DEMOS: Full + demographic controls
  if (length(available_demo_vars) > 0) {
    cat("  Fitting demos (full + demographics)...\n")
    formula <- build_formula_pooled(
      outcome = "outcome_value",
      rs_variable = rs_variable,
      model_spec = "full",
      continuous_model_spec = continuous_spec,
      heterogeneity = heterogeneity,
      add_domain = add_domain,
      add_time = add_time,
      time_var = time_var,
      add_demos = TRUE,
      demo_vars = available_demo_vars
    )
    mods$demos <- run_reg_model(data, formula, model_family = "lmer",
                                 REML = TRUE, use_weights = FALSE)
  }

  # 4. PREFS: Full + preference group controls
  if (has_prefs) {
    cat("  Fitting prefs (full + preferences)...\n")
    formula <- build_formula_pooled(
      outcome = "outcome_value",
      rs_variable = rs_variable,
      model_spec = "full",
      continuous_model_spec = continuous_spec,
      heterogeneity = heterogeneity,
      add_domain = add_domain,
      add_time = add_time,
      time_var = time_var,
      add_prefs = TRUE,
      pref_var = pref_var
    )
    mods$prefs <- run_reg_model(data, formula, model_family = "lmer",
                                 REML = TRUE, use_weights = FALSE)
  }

  # 5. WEIGHTED: Full model with IPW weights
  if (use_weights && has_weights) {
    cat("  Fitting weighted (full + IPW)...\n")
    formula <- build_formula_pooled(
      outcome = "outcome_value",
      rs_variable = rs_variable,
      model_spec = "full",
      continuous_model_spec = continuous_spec,
      heterogeneity = heterogeneity,
      add_domain = add_domain,
      add_time = add_time,
      time_var = time_var
    )
    mods$weighted <- run_reg_model(data, formula, model_family = "lmer",
                                    REML = TRUE, use_weights = TRUE)
  }

  return(mods)
}


# =============================================================================
# ROBUSTNESS TABLE FUNCTIONS
# =============================================================================

#' Extract model coefficients for target variables
#'
#' @param model Fitted model object
#' @param target_vars Variables to extract (patterns)
#' @return Data frame with coefficient information
extract_model_coeffs <- function(model, target_vars = c("lambda", "domain", "personalisation")) {
  if (is.null(model)) {
    return(data.frame(
      variable = character(0),
      coeff = numeric(0),
      se = numeric(0),
      p_value = numeric(0),
      ci_lower = numeric(0),
      ci_upper = numeric(0)
    ))
  }

  params <- parameters::model_parameters(model, ci = 0.95, verbose = FALSE)

  # Filter to fixed effects if Effects column exists
  if ("Effects" %in% names(params)) {
    params <- params[params$Effects == "fixed", ]
  }

  # Create data frame
  coeff_names <- params$Parameter
  coeffs <- params$Coefficient
  se_vals <- params$SE
  p_vals <- params$p
  ci_lower <- params$CI_low
  ci_upper <- params$CI_high

  # Filter to target variables (exclude intercept and interactions)
  if (!is.null(target_vars)) {
    matches <- sapply(coeff_names, function(coeff) {
      if (grepl("^\\(Intercept\\)$", coeff) || grepl(":", coeff)) {
        return(FALSE)
      }
      any(sapply(target_vars, function(target) {
        if (target == "lambda") {
          startsWith(coeff, "lambda") || coeff == "I(lambda^2)" || coeff == "I(lambda^3)"
        } else {
          startsWith(coeff, target)
        }
      }))
    })

    coeff_names <- coeff_names[matches]
    coeffs <- coeffs[matches]
    se_vals <- se_vals[matches]
    p_vals <- p_vals[matches]
    ci_lower <- ci_lower[matches]
    ci_upper <- ci_upper[matches]
  }

  results <- data.frame(
    variable = coeff_names,
    coeff = coeffs,
    se = se_vals,
    p_value = p_vals,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    stringsAsFactors = FALSE
  )

  return(results)
}


#' Get robustness coefficients across specifications
#'
#' @param robustness_models_list Named list of robustness models by outcome
#' @param outcome_vars Outcome variable names
#' @param rs_variable The RS variable used
#' @param study_type Study type label
#' @return Data frame with all coefficients
get_robustness_coeffs <- function(robustness_models_list,
                                   outcome_vars,
                                   rs_variable = "lambda",
                                   study_type = "cross_sectional") {
  all_results <- list()

  # Define target variables based on rs_variable
  if (rs_variable == "lambda") {
    target_vars <- c("lambda", "domain", "personalisation")
  } else if (rs_variable == "relationship_seeking_category") {
    target_vars <- c("relationship_seeking_category", "domain", "personalisation")
  } else if (rs_variable == "lambda_factor") {
    target_vars <- c("lambda_factor", "domain", "personalisation")
  } else {
    target_vars <- c(rs_variable, "domain", "personalisation")
  }

  for (outcome in outcome_vars) {
    if (!outcome %in% names(robustness_models_list)) next

    outcome_mods <- robustness_models_list[[outcome]]

    for (spec_name in names(outcome_mods)) {
      model <- outcome_mods[[spec_name]]
      coeffs_df <- extract_model_coeffs(model, target_vars)

      if (nrow(coeffs_df) > 0) {
        coeffs_df$outcome <- outcome
        coeffs_df$study_type <- study_type
        coeffs_df$rs_variable <- rs_variable
        coeffs_df$spec <- spec_name

        spec_id <- paste(outcome, study_type, rs_variable, spec_name, sep = "_")
        all_results[[spec_id]] <- coeffs_df
      }
    }
  }

  if (length(all_results) > 0) {
    combined <- do.call(rbind, all_results)
    rownames(combined) <- NULL
    # No FDR correction - robustness checks compare stability across specs,
    # not multiple testing. Use raw p-values for consistency with summary metrics.
    return(combined)
  } else {
    return(data.frame())
  }
}


#' Pivot robustness coefficients to wide format
#'
#' @param robustness_df Long-format robustness data frame
#' @param outcome_order Optional ordering for outcomes
#' @return Wide-format data frame
pivot_robustness_wide <- function(robustness_df, outcome_order = NULL) {
  if (nrow(robustness_df) == 0) return(data.frame())

  # Create group ID
  robustness_df$group_id <- paste(robustness_df$outcome, robustness_df$study_type,
                                   robustness_df$rs_variable, robustness_df$variable,
                                   sep = "_")

  result_df <- data.frame()
  specs <- c("additive", "full", "demos", "prefs", "weighted")

  for (group_id in unique(robustness_df$group_id)) {
    group_data <- robustness_df[robustness_df$group_id == group_id, ]

    base_info <- group_data[1, c("outcome", "study_type", "rs_variable", "variable")]
    row_data <- base_info

    # Add columns for each spec
    for (spec in specs) {
      spec_data <- group_data[group_data$spec == spec, ]
      if (nrow(spec_data) > 0) {
        row_data[[paste0(spec, "_coeff")]] <- spec_data$coeff
        row_data[[paste0(spec, "_se")]] <- spec_data$se
        row_data[[paste0(spec, "_p")]] <- spec_data$p_value
      } else {
        row_data[[paste0(spec, "_coeff")]] <- NA
        row_data[[paste0(spec, "_se")]] <- NA
        row_data[[paste0(spec, "_p")]] <- NA
      }
    }

    # Calculate summary statistics
    # Use full model as baseline for robustness comparison
    full_coeff <- row_data$full_coeff
    comparison_coeffs <- c(row_data$demos_coeff, row_data$prefs_coeff,
                           row_data$weighted_coeff)
    comparison_ps <- c(row_data$demos_p, row_data$prefs_p,
                       row_data$weighted_p)
    all_ps <- c(row_data$additive_p, row_data$full_p, comparison_ps)

    # Mean absolute difference from full model coefficient
    coeff_diffs <- abs(comparison_coeffs - full_coeff)
    row_data$mean_diff <- mean(coeff_diffs, na.rm = TRUE)

    # Percentage of significant coefficients
    n_signif <- sum(all_ps < 0.05, na.rm = TRUE)
    n_total <- sum(!is.na(all_ps))
    row_data$pct_signif <- if (n_total > 0) round(100 * n_signif / n_total, 0) else NA

    result_df <- rbind(result_df, row_data)
  }

  # Sort if outcome_order provided
  if (!is.null(outcome_order) && nrow(result_df) > 0) {
    result_df <- result_df[order(match(result_df$outcome, outcome_order)), ]
  }

  return(result_df)
}


#' Format coefficient with SE and significance stars
#'
#' @param coeff Coefficient value
#' @param se Standard error
#' @param p_val P-value
#' @return Formatted string
format_coeff_se_p <- function(coeff, se, p_val) {
  result <- rep("", length(coeff))
  valid <- !is.na(coeff) & !is.na(se)

  if (any(valid)) {
    stars <- ifelse(is.na(p_val), "",
                    ifelse(p_val < 0.001, "***",
                           ifelse(p_val < 0.01, "**",
                                  ifelse(p_val < 0.05, "*", ""))))

    formatted_coeff <- paste0(round(coeff[valid], 2), " (", round(se[valid], 2), ")", stars[valid])

    # Bold for significant coefficients
    has_stars <- stars[valid] != ""
    formatted_coeff[has_stars] <- paste0("\\textbf{", formatted_coeff[has_stars], "}")

    result[valid] <- formatted_coeff
  }
  return(result)
}



#' Full robustness analysis workflow
#'
#' Fits robustness models for each outcome and extracts coefficients.
#' Does NOT create LaTeX tables - use create_robustness_latex_table() separately.
#'
#' @param data Data frame
#' @param outcome_vars Outcome variable names
#' @param rs_variable RS variable to use
#' @param continuous_spec Functional form
#' @param specs_lookup Data frame or named vector mapping outcomes to specs
#' @param study_type Study type label
#' @param add_domain Include domain
#' @param add_time Include time
#' @param time_var Time variable
#' @param model_family Model family for fitting
#' @param use_weights Use IPW weights
#' @return List with models, coeffs_long, coeffs_wide, summary
run_robustness_analysis <- function(data,
                                     outcome_vars,
                                     rs_variable = "lambda",
                                     continuous_spec = "linear",
                                     specs_lookup = NULL,
                                     study_type = "cross_sectional",
                                     add_domain = TRUE,
                                     add_pre = FALSE,
                                     add_time = FALSE,
                                     time_var = NULL,
                                     model_family = "ols",
                                     use_weights = FALSE) {

  cat("\n=== Running Robustness Analysis ===\n")
  cat("Study type:", study_type, "\n")
  cat("RS variable:", rs_variable, "\n")
  cat("Outcomes:", paste(outcome_vars, collapse = ", "), "\n\n")

  # Fit models for each outcome
  all_models <- list()
  for (outcome in outcome_vars) {
    # Use per-outcome spec if provided, otherwise fall back to continuous_spec
    outcome_spec <- continuous_spec
    if (!is.null(specs_lookup)) {
      if (is.data.frame(specs_lookup)) {
        # specs_lookup is a data.frame with 'outcome' and 'best_spec' columns
        idx <- which(specs_lookup$outcome == outcome)
        if (length(idx) > 0) outcome_spec <- specs_lookup$best_spec[idx[1]]
      } else if (is.vector(specs_lookup) && outcome %in% names(specs_lookup)) {
        # specs_lookup is a named vector
        outcome_spec <- specs_lookup[[outcome]]
      }
    }
    cat("Fitting robustness models for:", outcome, "(spec:", outcome_spec, ")\n")
    all_models[[outcome]] <- fit_robustness_models(
      data = data,
      outcome_var = outcome,
      rs_variable = rs_variable,
      continuous_spec = outcome_spec,
      add_domain = add_domain,
      add_pre = add_pre,
      add_time = add_time,
      time_var = time_var,
      model_family = model_family,
      use_weights = use_weights
    )
  }

  # Extract coefficients
  cat("\nExtracting coefficients...\n")
  robustness_long <- get_robustness_coeffs(
    robustness_models_list = all_models,
    outcome_vars = outcome_vars,
    rs_variable = rs_variable,
    study_type = study_type
  )

  # Pivot to wide
  robustness_wide <- pivot_robustness_wide(robustness_long, outcome_order = outcome_vars)

  # Calculate robustness summary statistics
  summary_stats <- summarize_robustness(robustness_long, rs_variable)

  return(list(
    models = all_models,
    coeffs_long = robustness_long,
    coeffs_wide = robustness_wide,
    summary = summary_stats
  ))
}


#' Run robustness analysis for pooled constructs
#'
#' Like run_robustness_analysis but for pooled data (multiple outcomes stacked).
#' Handles constructs where outcomes are pooled into outcome_value/outcome_measure.
#' Does NOT create LaTeX tables - use create_robustness_latex_table() separately.
#'
#' @param data_list Named list of pooled data frames (one per construct)
#' @param construct_names Character vector of construct names
#' @param heterogeneity_lookup Named list: construct -> TRUE/FALSE for het
#' @param rs_variable RS variable name
#' @param continuous_spec Continuous lambda specification
#' @param specs_lookup Data frame or named vector mapping constructs to specs
#' @param study_type Study type for labelling
#' @param add_domain Include domain arm
#' @param add_time Include time variable
#' @param time_var Time variable name
#' @param use_weights Use IPW weights
#' @return List with models, coeffs_long, coeffs_wide, summary
run_robustness_analysis_pooled <- function(
    data_list,
    construct_names,
    heterogeneity_lookup = NULL,
    rs_variable = "lambda",
    continuous_spec = "linear",
    specs_lookup = NULL,
    study_type = "cross_sectional",
    add_domain = TRUE,
    add_time = FALSE,
    time_var = NULL,
    use_weights = FALSE) {

  cat("\n=== Running Pooled Robustness Analysis ===\n")
  cat("Study type:", study_type, "\n")
  cat("RS variable:", rs_variable, "\n")
  cat("Constructs:", paste(construct_names, collapse = ", "), "\n\n")

  # Fit models for each construct
  all_models <- list()
  for (cn in construct_names) {
    # Use per-construct spec if provided, otherwise fall back to continuous_spec
    construct_spec <- continuous_spec
    if (!is.null(specs_lookup)) {
      if (is.data.frame(specs_lookup)) {
        idx <- which(specs_lookup$outcome == cn)
        if (length(idx) > 0) construct_spec <- specs_lookup$best_spec[idx[1]]
      } else if (is.vector(specs_lookup) && cn %in% names(specs_lookup)) {
        construct_spec <- specs_lookup[[cn]]
      }
    }
    cat("Fitting robustness models for:", cn, "(spec:", construct_spec, ")\n")

    # Get heterogeneity setting for this construct
    het <- if (!is.null(heterogeneity_lookup) && cn %in% names(heterogeneity_lookup)) {
      heterogeneity_lookup[[cn]]
    } else {
      FALSE
    }

    all_models[[cn]] <- fit_robustness_models_pooled(
      data = data_list[[cn]],
      rs_variable = rs_variable,
      continuous_spec = construct_spec,
      heterogeneity = het,
      add_domain = add_domain,
      add_time = add_time,
      time_var = time_var,
      use_weights = use_weights
    )
  }

  # Extract coefficients
  cat("\nExtracting coefficients...\n")
  robustness_long <- get_robustness_coeffs(
    robustness_models_list = all_models,
    outcome_vars = construct_names,
    rs_variable = rs_variable,
    study_type = study_type
  )

  # Pivot to wide
  robustness_wide <- pivot_robustness_wide(
    robustness_long,
    outcome_order = construct_names
  )

  # Calculate robustness summary statistics
  summary_stats <- summarize_robustness(robustness_long, rs_variable)

  return(list(
    models = all_models,
    coeffs_long = robustness_long,
    coeffs_wide = robustness_wide,
    summary = summary_stats
  ))
}


#' Summarize robustness check results
#'
#' Calculates coefficient stability and significance persistence across
#' robustness specifications compared to baseline.
#'
#' @param coeffs_long Long-format coefficients from get_robustness_coeffs()
#' @param rs_variable The relationship-seeking variable name
#' @return List with summary statistics and formatted text
summarize_robustness <- function(coeffs_long, rs_variable = "lambda") {

  if (is.null(coeffs_long) || nrow(coeffs_long) == 0) {
    return(list(
      stability_df = NULL,
      text_summary = "No robustness results available."
    ))
  }

  # Define valid lambda terms: main effects only (no interactions)
  # Matches: "lambda", "I(lambda^2)", "I(lambda^3)"
  valid_terms <- c(
    rs_variable,
    paste0("I(", rs_variable, "^2)"),
    paste0("I(", rs_variable, "^3)")
  )

  # Filter to lambda main effects (excludes interactions like lambda:domain)
  lambda_coeffs <- coeffs_long %>%
    dplyr::filter(variable %in% valid_terms)

  if (nrow(lambda_coeffs) == 0) {
    return(list(
      stability_df = NULL,
      text_summary = "No lambda coefficients found in robustness results."
    ))
  }

  # Calculate stability metrics by outcome
  # Compare robustness specs (demos, prefs, weighted) against "full" as baseline
  stability_df <- lambda_coeffs %>%
    dplyr::group_by(outcome) %>%
    dplyr::mutate(
      # Get baseline (full model) coefficient
      baseline_coeff = coeff[spec == "full"][1],
      baseline_p = p_value[spec == "full"][1],
      baseline_sig = baseline_p < 0.05,
      # Calculate % change from baseline
      pct_change = ifelse(baseline_coeff != 0,
                          100 * (coeff - baseline_coeff) / abs(baseline_coeff),
                          NA),
      # Check if significance is maintained
      is_sig = p_value < 0.05,
      sig_maintained = (is_sig == baseline_sig)
    ) %>%
    # Remove baseline from comparison rows (comparing full to itself is trivially
    # pct_change=0, sig_maintained=TRUE - not informative for robustness)
    dplyr::filter(spec != "full") %>%
    dplyr::ungroup()

  # Summary statistics across all outcomes and specs
  n_comparisons <- nrow(stability_df)
  n_sig_maintained <- sum(stability_df$sig_maintained, na.rm = TRUE)
  pct_sig_maintained <- round(100 * n_sig_maintained / n_comparisons, 1)

  max_pct_change <- max(abs(stability_df$pct_change), na.rm = TRUE)
  mean_pct_change <- mean(abs(stability_df$pct_change), na.rm = TRUE)

  # Count how many times significance was lost (was sig in baseline, not in robustness)
  sig_lost <- stability_df %>%
    dplyr::filter(baseline_sig == TRUE & is_sig == FALSE)
  n_sig_lost <- nrow(sig_lost)

  # Count how many times significance was gained
  sig_gained <- stability_df %>%
    dplyr::filter(baseline_sig == FALSE & is_sig == TRUE)
  n_sig_gained <- nrow(sig_gained)

  # Create text summary
  text_parts <- c()

  if (n_sig_lost == 0 && n_sig_gained == 0) {
    text_parts <- c(text_parts,
      sprintf("Significance was maintained across all %d robustness comparisons.", n_comparisons))
  } else {
    if (n_sig_lost > 0) {
      lost_details <- sig_lost %>%
        dplyr::mutate(detail = paste0(outcome, " (", spec, ")")) %>%
        dplyr::pull(detail)
      text_parts <- c(text_parts,
        sprintf("Significance was lost in %d comparison(s): %s.",
                n_sig_lost, paste(lost_details, collapse = ", ")))
    }
    if (n_sig_gained > 0) {
      gained_details <- sig_gained %>%
        dplyr::mutate(detail = paste0(outcome, " (", spec, ")")) %>%
        dplyr::pull(detail)
      text_parts <- c(text_parts,
        sprintf("Significance was gained in %d comparison(s): %s.",
                n_sig_gained, paste(gained_details, collapse = ", ")))
    }
  }

  text_parts <- c(text_parts,
    sprintf("Mean absolute coefficient change: %.1f%% (max: %.1f%%).",
            mean_pct_change, max_pct_change))

  text_summary <- paste(text_parts, collapse = " ")

  # Create per-outcome summary
  outcome_summary <- stability_df %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      baseline_coeff = first(baseline_coeff),
      baseline_sig = first(baseline_sig),
      n_specs = dplyr::n(),
      n_sig_maintained = sum(sig_maintained, na.rm = TRUE),
      mean_pct_change = mean(abs(pct_change), na.rm = TRUE),
      max_pct_change = max(abs(pct_change), na.rm = TRUE),
      all_sig_maintained = all(sig_maintained, na.rm = TRUE),
      .groups = "drop"
    )

  return(list(
    stability_df = stability_df,
    outcome_summary = outcome_summary,
    n_comparisons = n_comparisons,
    n_sig_lost = n_sig_lost,
    n_sig_gained = n_sig_gained,
    pct_sig_maintained = pct_sig_maintained,
    mean_pct_change = mean_pct_change,
    max_pct_change = max_pct_change,
    text_summary = text_summary
  ))
}



# # =============================================================================
# # CONTRAST-BASED ROBUSTNESS ANALYSIS
# # =============================================================================

# #' Compute key emmeans contrasts from a fitted model
# #'
# #' Extracts the three primary contrasts (RS effect, personalisation,
# #' domain) from a model using emmeans.
# #'
# #' @param model Fitted model (lm or lmer)
# #' @param data Data used to fit the model
# #' @param add_domain Whether domain contrast should be computed
# #' @return Data frame with contrast_type, estimate, SE, p.value
# compute_key_contrasts <- function(model, data, add_domain = TRUE) {
#   results <- data.frame()

#   # 1. RS effect: avg(pos) - avg(neg)
#   tryCatch({
#     emm <- emmeans(model, ~lambda,
#                    at = list(lambda = c(-1, -0.5, 0.5, 1)),
#                    data = data)
#     cont <- contrast(emm,
#       method = list(
#         "avg(pos) - avg(neg)" = c(-0.5, -0.5, 0.5, 0.5)
#       ))
#     cont_df <- as.data.frame(summary(cont, infer = TRUE))
#     results <- rbind(results, data.frame(
#       contrast_type = "RS effect",
#       estimate = cont_df$estimate,
#       SE = cont_df$SE,
#       p.value = cont_df$p.value,
#       stringsAsFactors = FALSE
#     ))
#   }, error = function(e) {
#     cat("    Warning: RS contrast failed:", e$message, "\n")
#   })

#   # 2. Personalisation effect
#   tryCatch({
#     emm_pers <- emmeans(model,
#       ~personalisation, data = data)
#     cont_pers <- contrast(emm_pers, method = "revpairwise")
#     pers_df <- as.data.frame(
#       summary(cont_pers, infer = TRUE))
#     results <- rbind(results, data.frame(
#       contrast_type = "Personalisation",
#       estimate = pers_df$estimate,
#       SE = pers_df$SE,
#       p.value = pers_df$p.value,
#       stringsAsFactors = FALSE
#     ))
#   }, error = function(e) {
#     cat("    Warning: Personalisation contrast failed:",
#         e$message, "\n")
#   })

#   # 3. Domain effect
#   if (add_domain) {
#     tryCatch({
#       emm_dom <- emmeans(model, ~domain, data = data)
#       cont_dom <- contrast(emm_dom, method = "revpairwise")
#       dom_df <- as.data.frame(
#         summary(cont_dom, infer = TRUE))
#       results <- rbind(results, data.frame(
#         contrast_type = "Domain",
#         estimate = dom_df$estimate,
#         SE = dom_df$SE,
#         p.value = dom_df$p.value,
#         stringsAsFactors = FALSE
#       ))
#     }, error = function(e) {
#       cat("    Warning: Domain contrast failed:",
#           e$message, "\n")
#     })
#   }

#   return(results)
# }


# #' Run contrast-based robustness analysis for one construct
# #'
# #' Fits the same model as the baseline (full_continuous) but with
# #' added controls (demographics, preferences, IPW weights), then
# #' computes the three key emmeans contrasts from each.
# #'
# #' @param baseline_model Fitted full_continuous model
# #' @param data Data used to fit baseline model
# #' @param model_family "ols" or "lmer"
# #' @param add_domain Whether to include domain contrast
# #' @param use_weights Whether to fit IPW-weighted model
# #' @param demo_vars Demographic control variable names
# #' @param pref_var Preference control variable name
# #' @return Data frame with spec, contrast_type, estimate, SE, p.value
# compute_contrast_robustness <- function(
#     baseline_model,
#     data,
#     model_family = "lmer",
#     add_domain = TRUE,
#     use_weights = FALSE,
#     demo_vars = c("age", "gender_binary", "education_years",
#                   "ai_frequency_coarsened", "ethnicity_binary",
#                   "income_binary", "religion_binary"),
#     pref_var = "factor_1_score") {

#   base_formula <- formula(baseline_model)
#   all_results <- data.frame()

#   # 1. Baseline contrasts
#   cat("    Computing baseline contrasts...\n")
#   baseline <- compute_key_contrasts(
#     baseline_model, data, add_domain)
#   if (nrow(baseline) > 0) {
#     baseline$spec <- "Baseline"
#     all_results <- rbind(all_results, baseline)
#   }

#   # 2. + Demographics
#   available_demos <- intersect(demo_vars, names(data))
#   if (length(available_demos) > 0) {
#     cat("    Fitting + demos model...\n")
#     demo_terms <- paste(available_demos, collapse = " + ")
#     demo_formula <- update(
#       base_formula,
#       as.formula(paste0(". ~ . + ", demo_terms)))
#     tryCatch({
#       demo_model <- run_reg_model(
#         data, demo_formula,
#         model_family = model_family,
#         REML = TRUE, verbose = FALSE)
#       demos <- compute_key_contrasts(
#         demo_model, data, add_domain)
#       if (nrow(demos) > 0) {
#         demos$spec <- "+ Demos"
#         all_results <- rbind(all_results, demos)
#       }
#     }, error = function(e) {
#       cat("    Warning: Demos model failed:",
#           e$message, "\n")
#     })
#   }

#   # 3. + Preferences
#   if (pref_var %in% names(data)) {
#     cat("    Fitting + prefs model...\n")
#     prefs_formula <- update(
#       base_formula,
#       as.formula(paste0(". ~ . + ", pref_var)))
#     tryCatch({
#       prefs_model <- run_reg_model(
#         data, prefs_formula,
#         model_family = model_family,
#         REML = TRUE, verbose = FALSE)
#       prefs <- compute_key_contrasts(
#         prefs_model, data, add_domain)
#       if (nrow(prefs) > 0) {
#         prefs$spec <- "+ Prefs"
#         all_results <- rbind(all_results, prefs)
#       }
#     }, error = function(e) {
#       cat("    Warning: Prefs model failed:",
#           e$message, "\n")
#     })
#   }

#   # 4. + IPW weights
#   if (use_weights &&
#       "ipw_weight_truncated" %in% names(data)) {
#     cat("    Fitting + IPW model...\n")
#     tryCatch({
#       weighted_model <- run_reg_model(
#         data, base_formula,
#         model_family = model_family,
#         REML = TRUE, use_weights = TRUE,
#         verbose = FALSE)
#       weighted <- compute_key_contrasts(
#         weighted_model, data, add_domain)
#       if (nrow(weighted) > 0) {
#         weighted$spec <- "+ IPW"
#         all_results <- rbind(all_results, weighted)
#       }
#     }, error = function(e) {
#       cat("    Warning: IPW model failed:",
#           e$message, "\n")
#     })
#   }

#   return(all_results)
# }


# #' Run contrast robustness for all constructs
# #'
# #' Loops over constructs, calls compute_contrast_robustness()
# #' for each, and combines results.
# #'
# #' @param models_list Named list of model lists (by construct)
# #' @param data_list Named list of data frames (by construct)
# #' @param model_family_list Named list of model families
# #' @param add_domain Include domain contrast
# #' @param use_weights Whether to use IPW weights
# #' @return Data frame with construct, spec, contrast_type,
# #'   estimate, SE, p.value
# run_contrast_robustness <- function(
#     models_list,
#     data_list,
#     model_family_list = NULL,
#     add_domain = TRUE,
#     use_weights = FALSE) {

#   construct_names <- names(models_list)
#   all_results <- data.frame()

#   for (cn in construct_names) {
#     cat(sprintf("  Construct: %s\n", cn))
#     model <- models_list[[cn]][["full_continuous"]]
#     data <- data_list[[cn]]
#     family <- if (!is.null(model_family_list)) {
#       model_family_list[[cn]]
#     } else {
#       "lmer"
#     }

#     result <- compute_contrast_robustness(
#       baseline_model = model,
#       data = data,
#       model_family = family,
#       add_domain = add_domain,
#       use_weights = use_weights
#     )

#     if (nrow(result) > 0) {
#       result$construct <- cn
#       all_results <- rbind(all_results, result)
#     }
#   }

#   return(all_results)
# }


# #' Format estimate with significance stars
# #'
# #' @param estimate Numeric estimate
# #' @param p_value P-value for significance stars
# #' @return Formatted string like "2.82* (0.018)"
# format_est_p <- function(estimate, p_value) {
#   stars <- ifelse(p_value < 0.001, "***",
#            ifelse(p_value < 0.01, "**",
#            ifelse(p_value < 0.05, "*", "")))
#   p_fmt <- ifelse(p_value < 0.001, "<.001",
#                   sprintf("%.3f", p_value))
#   sprintf("%.2f%s (%.3f)", estimate, stars, p_value)
# }


# #' Create contrast robustness LaTeX table
# #'
# #' Generates a LaTeX table with multicolumn outcome headers,
# #' showing contrast estimates across robustness specifications
# #' with summary columns.
# #'
# #' @param results Data frame from run_contrast_robustness()
# #' @param construct_order Character vector of construct names
# #' @param construct_labels Named vector of display labels
# #' @param has_ipw Whether IPW column should be included
# #' @param table_dir Output directory
# #' @param filename Filename (without .tex)
# #' @param caption Table caption
# #' @param label Table label
# #' @return LaTeX table as character vector (also saved to file)
# create_contrast_robustness_latex <- function(
#     results,
#     construct_order,
#     construct_labels = NULL,
#     has_ipw = FALSE,
#     table_dir = "outputs/tables/main_studies",
#     filename = "robustness_contrasts",
#     caption = "Contrast-Based Robustness Checks",
#     label = "tab:robustness_contrasts") {

#   if (is.null(construct_labels)) {
#     construct_labels <- setNames(
#       gsub("_", " ", tools::toTitleCase(construct_order)),
#       construct_order)
#   }

#   # Define spec columns
#   spec_cols <- c("Baseline", "+ Demos", "+ Prefs")
#   if (has_ipw) spec_cols <- c(spec_cols, "+ IPW")
#   n_spec <- length(spec_cols)
#   # Total columns: Contrast + specs + Sig Maintained + Max |Δ%|
#   n_cols <- 1 + n_spec + 2
#   col_align <- paste0("l", paste(rep("c", n_cols - 1),
#                                   collapse = ""))

#   # Build header
#   header_specs <- paste(
#     sapply(spec_cols, function(s) {
#       paste0("\\textbf{", s, "}")
#     }),
#     collapse = " & ")
#   header <- paste0(
#     "\\textbf{Contrast} & ", header_specs,
#     " & \\textbf{Sig Maint.}",
#     " & \\textbf{Max $|\\Delta\\%|$} \\\\")

#   # Build caption with note (avoids widening the table)
#   caption_with_note <- paste0(
#     caption,
#     ". Cells show the coefficient estimate with ",
#     "$p$-value in parentheses. ",
#     "Significance is indicated by ",
#     "{\\itshape * $p<.05$, ** $p<.01$, *** $p<.001$}. ",
#     "Sig Maint.\\ reports the number of robustness specifications in which ",
#     "significance status matches the baseline. ",
#     "Max $|\\Delta\\%|$ gives the largest percentage change in the estimate ",
#     "relative to the baseline across robustness specifications."
#   )

#   # Start table
#   latex <- c(
#     "\\begin{table}[H]",
#     "\\centering",
#     paste0("\\caption{", caption_with_note, "}"),
#     paste0("\\label{", label, "}"),
#     paste0("\\footnotesize"),
#     paste0("\\begin{tabular}{", col_align, "}"),
#     "\\toprule",
#     header,
#     "\\midrule"
#   )

#   # Build rows per construct
#   for (cn in construct_order) {
#     cn_label <- construct_labels[[cn]]
#     # Multicolumn header for construct (centered)
#     latex <- c(latex, paste0(
#       "\\multicolumn{", n_cols, "}{c}{\\textbf{",
#       cn_label, "}} \\\\"))

#     cn_data <- results[results$construct == cn, ]
#     contrast_types <- unique(cn_data$contrast_type)

#     for (ct in contrast_types) {
#       ct_data <- cn_data[cn_data$contrast_type == ct, ]

#       # Get baseline
#       baseline_row <- ct_data[ct_data$spec == "Baseline", ]
#       if (nrow(baseline_row) == 0) next
#       baseline_est <- baseline_row$estimate
#       baseline_p <- baseline_row$p.value
#       baseline_sig <- baseline_p < 0.05

#       # Format each spec cell
#       cells <- c()
#       for (sp in spec_cols) {
#         sp_row <- ct_data[ct_data$spec == sp, ]
#         if (nrow(sp_row) > 0) {
#           cells <- c(cells, format_est_p(
#             sp_row$estimate, sp_row$p.value))
#         } else {
#           cells <- c(cells, "---")
#         }
#       }

#       # Summary: Sig Maintained
#       comparison_specs <- setdiff(spec_cols, "Baseline")
#       n_compared <- 0
#       n_maintained <- 0
#       pct_changes <- c()

#       for (sp in comparison_specs) {
#         sp_row <- ct_data[ct_data$spec == sp, ]
#         if (nrow(sp_row) > 0) {
#           n_compared <- n_compared + 1
#           sp_sig <- sp_row$p.value < 0.05
#           if (sp_sig == baseline_sig) {
#             n_maintained <- n_maintained + 1
#           }
#           if (abs(baseline_est) > 0.001) {
#             pct_change <- 100 * abs(
#               sp_row$estimate - baseline_est
#             ) / abs(baseline_est)
#             pct_changes <- c(pct_changes, pct_change)
#           }
#         }
#       }

#       sig_cell <- if (n_compared > 0) {
#         paste0(n_maintained, "/", n_compared)
#       } else { "---" }

#       delta_cell <- if (length(pct_changes) > 0) {
#         sprintf("%.1f\\%%", max(pct_changes))
#       } else { "---" }

#       row_str <- paste0(
#         ct, " & ",
#         paste(cells, collapse = " & "),
#         " & ", sig_cell,
#         " & ", delta_cell,
#         " \\\\")
#       latex <- c(latex, row_str)
#     }

#     # Add midrule between constructs (not after last)
#     if (cn != tail(construct_order, 1)) {
#       latex <- c(latex, "\\midrule")
#     }
#   }

#   # Close table
#   latex <- c(latex,
#     "\\bottomrule",
#     "\\end{tabular}",
#     "\\end{table}")

#   # Save
#   dir.create(table_dir, showWarnings = FALSE,
#              recursive = TRUE)
#   output_path <- file.path(table_dir,
#                            paste0(filename, ".tex"))
#   writeLines(latex, output_path)
#   cat("Saved contrast robustness table to:",
#       output_path, "\n")

#   return(latex)
# }