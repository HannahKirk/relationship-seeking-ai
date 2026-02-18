# =============================================================================
# FORMULA BUILDING
# =============================================================================

#' Build regression formula for a single outcome
#'
#' Constructs a formula with flexible lambda specification (linear/quadratic/cubic),
#' treatment arms, interactions, and random effects.
#'
#' @param outcome_var Outcome variable name
#' @param rs_variable Relationship-seeking variable: "lambda", "lambda_factor", or
#'   "relationship_seeking_category"
#' @param model_spec Model specification: "additive" or "full" (with interactions)
#' @param continuous_model_spec For lambda: "linear", "quadratic", or "cubic"
#' @param add_domain Include domain treatment arm (default TRUE)
#' @param add_pre Include pre-treatment covariate (adds outcome_var_pre)
#' @param add_time Include time variable and random slopes
#' @param time_var Time variable name (required if add_time = TRUE)
#' @param add_demos Include demographic control variables
#' @param demo_vars Character vector of demographic variable names
#' @param add_prefs Include preference group control variable
#' @param pref_var Name of preference group variable
#' @return Formula object
build_formula <- function(outcome_var,
                          rs_variable = "lambda",
                          model_spec = "additive",
                          continuous_model_spec = "linear",
                          add_domain = TRUE,
                          add_pre = FALSE,
                          add_time = FALSE,
                          time_var = NULL,
                          add_demos = FALSE,
                          demo_vars = c("age", "gender_binary", "education_years",
                                        "ai_frequency_coarsened", "ethnicity_binary",
                                        "income_binary", "religion_binary"),
                          add_prefs = FALSE,
                          pref_var = "cluster_name",
                          add_topic = FALSE) {
  # Specify relationship-seeking terms based on variable type

  rs_terms <- if (rs_variable == "lambda") {
    if (continuous_model_spec == "linear") {
      "lambda"
    } else if (continuous_model_spec == "quadratic") {
      "lambda + I(lambda^2)"
    } else if (continuous_model_spec == "cubic") {
      "lambda + I(lambda^2) + I(lambda^3)"
    } else {
      stop("Unsupported continuous_model_spec. Use 'linear', 'quadratic', or 'cubic'.")
    }
  } else if (rs_variable == "lambda_factor") {
    "lambda_factor"
  } else if (rs_variable == "relationship_seeking_category") {
    "relationship_seeking_category"
  } else {
    stop("Unsupported rs_variable. Use 'lambda', 'lambda_factor', or 'relationship_seeking_category'.")
  }

  # Build up terms
  terms <- rs_terms

  # Add other treatment arms
  terms <- c(terms, "personalisation")
  if (add_domain) {
    terms <- c(terms, "domain")
  }

  # Add pre-treatment covariate
  if (add_pre) {
    pre_var <- paste0(outcome_var, "_pre")
    terms <- c(terms, pre_var)
  }

  # Determine rs_var for interactions (we do not add higher order polynomial terms to interaction)
  rs_var <- if (rs_variable == "lambda") "lambda" else rs_variable

  # Add interactions for full model
  if (model_spec == "full") {
    terms <- c(terms, paste0(rs_var, ":personalisation"))
    if (add_domain) {
      terms <- c(terms, paste0(rs_var, ":domain"))
    }
  }

  # Add time components
  if (add_time) {
    if (is.null(time_var)) {
      stop("time_var must be specified when add_time = TRUE")
    }
    terms <- c(terms, time_var)
    if (model_spec == "full") {
      terms <- c(terms, paste0(rs_var, ":", time_var))
      terms <- c(terms, paste0("personalisation:", time_var))
      if (add_domain) {
        terms <- c(terms, paste0("domain:", time_var))
      }
    }
    random <- paste0(" + (1 + ", time_var, " | ppt_id)")
  } else {
    random <- ""
  }

  # Add demographic controls
  if (add_demos) {
    terms <- c(terms, demo_vars)
  }

  # Add preference group controls
  if (add_prefs) {
    terms <- c(terms, pref_var)
  }

  # Add topic fixed effects
  if (add_topic) {
    terms <- c(terms, "topic")
  }

  # Combine into formula
  formula_str <- paste(outcome_var, "~", paste(terms, collapse = " + "), random)
  return(as.formula(formula_str))
}


#' Build pooled formula for multiple outcomes
#'
#' Constructs a formula for stacked outcome data with outcome_measure fixed effect
#' and participant random intercepts.
#'
#' @param outcome Outcome variable name (default "outcome_value")
#' @param rs_variable Relationship-seeking variable
#' @param model_spec Model specification: "additive" or "full"
#' @param continuous_model_spec For lambda: "linear", "quadratic", or "cubic"
#' @param heterogeneity Include outcome_measure interactions to test pooling
#' @param add_domain Include domain treatment arm
#' @param add_pre Include pre-treatment covariate (outcome_value_pre)
#' @param add_time Include time variable
#' @param time_var Time variable name
#' @param add_demos Include demographic control variables
#' @param demo_vars Character vector of demographic variable names
#' @param add_prefs Include preference group control variable
#' @param pref_var Name of preference group variable
#' @return Formula object
build_formula_pooled <- function(outcome = "outcome_value",
                                  rs_variable = "lambda",
                                  model_spec = "additive",
                                  continuous_model_spec = "linear",
                                  heterogeneity = FALSE,
                                  add_domain = TRUE,
                                  add_pre = FALSE,
                                  add_time = FALSE,
                                  time_var = NULL,
                                  add_demos = FALSE,
                                  demo_vars = c("age", "gender_binary", "education_years",
                                                "ai_frequency_coarsened", "ethnicity_binary",
                                                "income_binary", "religion_binary"),
                                  add_prefs = FALSE,
                                  pref_var = "cluster_name") {
  # Specify relationship-seeking terms (same as build_formula)
  rs_terms <- if (rs_variable == "lambda") {
    if (continuous_model_spec == "linear") {
      "lambda"
    } else if (continuous_model_spec == "quadratic") {
      "lambda + I(lambda^2)"
    } else if (continuous_model_spec == "cubic") {
      "lambda + I(lambda^2) + I(lambda^3)"
    } else {
      stop("Unsupported continuous_model_spec. Use 'linear', 'quadratic', or 'cubic'.")
    }
  } else if (rs_variable == "lambda_factor") {
    "lambda_factor"
  } else if (rs_variable == "relationship_seeking_category") {
    "relationship_seeking_category"
  } else {
    stop("Unsupported rs_variable. Use 'lambda', 'lambda_factor', or 'relationship_seeking_category'.")
  }

  # Determine rs_var for interactions
  rs_var <- if (rs_variable == "lambda") "lambda" else rs_variable

  # Start building terms
  terms <- c(rs_terms, "personalisation", "outcome_measure")
  if (add_domain) {
    terms <- c(terms, "domain")
  }

  # Add heterogeneity interactions (outcome_measure with main treatment terms)
  if (heterogeneity) {
    terms <- c(terms, paste0(rs_var, ":outcome_measure"))
    terms <- c(terms, "personalisation:outcome_measure")
    if (add_domain) {
      terms <- c(terms, "domain:outcome_measure")
    }
  }

  # Pre-treatment covariate
  if (add_pre) {
    terms <- c(terms, "outcome_value_pre")
  }

  # Full model interactions (between treatments)
  if (model_spec == "full") {
    terms <- c(terms, paste0(rs_var, ":personalisation"))
    if (add_domain) {
      terms <- c(terms, paste0(rs_var, ":domain"))
    }
  }

  # Time components
  if (add_time) {
    if (is.null(time_var)) {
      stop("time_var must be specified when add_time = TRUE")
    }
    terms <- c(terms, time_var)
    # Add heterogeneity interaction (outcome_measure with main time term)
    if (heterogeneity) {
      terms <- c(terms, paste0(time_var, ":outcome_measure"))
    }
    if (model_spec == "full") {
      terms <- c(terms, paste0(rs_var, ":", time_var))
      terms <- c(terms, paste0("personalisation:", time_var))
      if (add_domain) {
        terms <- c(terms, paste0("domain:", time_var))
      }
    }
    random <- paste0(" + (1 + ", time_var, " | ppt_id)")
  } else {
    random <- " + (1 | ppt_id)"
  }

  # Add demographic controls
  if (add_demos) {
    terms <- c(terms, demo_vars)
  }

  # Add preference group controls
  if (add_prefs) {
    terms <- c(terms, pref_var)
  }

  formula_str <- paste(outcome, "~", paste(terms, collapse = " + "), random)
  return(as.formula(formula_str))
}