# =============================================================================
# Labelling Utilities
# =============================================================================
# Helpers for variable/outcome renaming, label formatting, and display name utilities.
# into human-readable labels for tables, reports, and LaTeX output.
# Note we can probably consolidate this further w/ Sjplot2latex functionality

#
# Usage:
#   source("scripts/utils_r/labelling_utils.R")
#   PRED_LABELS <- get_pred_labels("session_numeric")
#   SOCIODEMO_LABELS <- get_sociodemo_labels()
#   ALL_LABELS <- c(get_pred_labels("session_numeric"), get_sociodemo_labels())
#
# Some conversion to LaTeX ($\lambda$, etc.) is still handled
# by create_default_replacements() in sjplot_to_latex.R
# AFTER the HTML-to-LaTeX conversion step.
# =============================================================================

# =============================================================================
# OUTCOME LABEL FORMATTING
# =============================================================================

#' Format outcome variable name for display in LaTeX tables
#'
#' Converts snake_case outcome names to Title Case with spaces.
#' Use this universally for all table captions, headers, and labels.
#'
#' @param outcome Character string of outcome variable name (e.g., "goodbye_action")
#' @return Formatted string (e.g., "Goodbye Action")
#' @examples
#' format_outcome_label("goodbye_action")
#' # "Goodbye Action"
#' format_outcome_label("likeability")
#' # "Likeability"
format_outcome_label <- function(outcome) {
  tools::toTitleCase(gsub("_", " ", outcome))
}

# =============================================================================
# STUDY TYPE CAPTION
# =============================================================================

#' Format study type label with color for LaTeX captions
#'
#' Wraps the study type name in \\textcolor with the appropriate
#' color from STUDY_COLORS (cross-sectional = blue, longitudinal = orange).
#'
#' @param study_type "cross_sectional" or "longitudinal"
#' @return LaTeX string with colored, bolded study type label
study_type_caption <- function(study_type) {
  color_hex <- switch(study_type,
    "cross_sectional" = "32bcdd",
    "longitudinal" = "ff8a67",
    "000000"
  )
  label <- switch(study_type,
    "cross_sectional" = "Cross-Sectional",
    "longitudinal" = "Longitudinal",
    study_type
  )
  paste0(
    "\\textcolor[HTML]{", color_hex,
    "}{\\textbf{", label, "}}")
}

# =============================================================================
# PREDICTOR LABEL HELPER FUNCTIONS
# =============================================================================
# These helpers automatically format predictor names, handling:
# - Base term lookups (lambda -> "Multiplier" for text, "$\lambda$" for latex)
# - Outcome measure formatting
# - Interaction splitting and ordering (domain first)
# - Joining with "x" for text or "$\times$" for latex
#
# ARCHITECTURE:
# - "text" format: Used for sjPlot (generates HTML first), markdown, console
# - "latex" format: Used for direct LaTeX table generation (contrast tables)
#
# The text values here MUST match the patterns in create_default_replacements()
# in sjplot_to_latex.R for the HTML-to-LaTeX conversion to work correctly.
# =============================================================================

# Base term mappings: list(latex = "...", text = "...")
# - "text": Plain English labels that appear in HTML/markdown
# - "latex": LaTeX math notation for direct LaTeX output
.BASE_TERMS <- list(
  "lambda" = list(latex = "$\\lambda$", text = "Multiplier"),
  "I(lambda^2)" = list(latex = "$\\lambda^2$", text = "Multiplier Squared"),
  "I(lambda^3)" = list(latex = "$\\lambda^3$", text = "Multiplier Cubed"),
  "relationship_seeking_categorypos_lambda" = list(
    latex = "$\\lambda_{>0}$", text = "Pos Multiplier"),
  "relationship_seeking_categoryzero_lambda" = list(
    latex = "$\\lambda_0$", text = "Zero Multiplier"),
  "lambda_factorneg1" = list(
    latex = "$\\lambda_{-1}$", text = "Multiplier neg1"),
  "lambda_factorneg0.5" = list(
    latex = "$\\lambda_{-0.5}$", text = "Multiplier neg0.5"),
  "lambda_factorzero" = list(latex = "$\\lambda_0$", text = "Multiplier zero"),
  "lambda_factorpos0.5" = list(
    latex = "$\\lambda_{+0.5}$", text = "Multiplier pos0.5"),
  "lambda_factorpos1" = list(latex = "$\\lambda_{+1}$", text = "Multiplier pos1"),
  "domainemotchat" = list(latex = "Emot.", text = "Emot."),
  "personalisationpersonalised" = list(latex = "Pers", text = "Pers"),
  "study_idlongitudinal" = list(latex = "Longitudinal", text = "Longitudinal"),
  "study_typelongitudinal" = list(latex = "Longitudinal", text = "Longitudinal"),
  "time" = list(latex = "Time", text = "Time"),
  "week_numeric" = list(latex = "Week", text = "Week"),
  "session_numeric" = list(latex = "Session", text = "Session")
)

# Outcome measure mappings (suffix -> display label, same for both formats)
.OUTCOME_MEASURES <- c(
  "cognitive_reliance" = "Cog. Rel.",
  "behavioural_reliance" = "Behav. Rel.",
  "responsiveness" = "Resp.",
  "understanding" = "Underst.",
  "connection" = "Connection",
  "pain" = "Pain",
  "pleasure" = "Pleasure",
  "emotions" = "Emotions",
  "awareness" = "Awareness",
  "ontological_sentience" = "Ontological Consciousness",
  "perceived_sentience" = "Perceived Consciousness",
  "seeking_companionship_likelihood" = "Seek Companionship",
  "likeability" = "Likeability",
  "engagingness" = "Engagingness",
  "helpfulness" = "Helpfulness"
)

# Terms that should come first in interactions (for ordering)
.PRIORITY_TERMS <- c("domainemotchat", "time", "lambda", "relationship_seeking",
                     "lambda_factor", "personalisationpersonalised")

#' Format a single predictor term (not an interaction)
#'
#' @param term Raw R term name
#' @param time_var Optional time variable name for dynamic lookup
#' @param format "latex" or "text"
#' @return Formatted label
.format_single_term <- function(term, time_var = NULL, format = "latex") {
  # Check base terms first
  if (term %in% names(.BASE_TERMS)) {
    return(.BASE_TERMS[[term]][[format]])
  }

  # Check if it's an outcome_measure term
  if (startsWith(term, "outcome_measure")) {
    suffix <- sub("^outcome_measure", "", term)
    if (suffix %in% names(.OUTCOME_MEASURES)) {
      label <- .OUTCOME_MEASURES[[suffix]]
      if (format == "latex") {
        return(paste0("\\textit{", label, "}"))
      } else {
        return(label)
      }
    }
  }

  # Check if it's the time variable
  if (!is.null(time_var) && term == time_var) {
    return(switch(time_var,
      "session_numeric" = "Session",
      "week_numeric" = "Week",
      time_var))
  }

  # Fallback: title case
  tools::toTitleCase(gsub("_", " ", term))
}

#' Format a predictor name (handles interactions automatically)
#'
#' Splits interactions on ":", formats each part, orders correctly
#' (domain/time first), and joins with appropriate separator.
#'
#' @param predictor Raw R predictor name (e.g., "lambda:domainemotchat")
#' @param time_var Optional time variable name
#' @param format "latex" or "text"
#' @return Formatted label
format_predictor_label <- function(predictor, time_var = NULL, format = "latex") {
  sep <- if (format == "latex") " $\\times$ " else " x "

  # Handle interactions
  if (grepl(":", predictor)) {
    parts <- strsplit(predictor, ":")[[1]]
    formatted_parts <- sapply(parts, .format_single_term,
                              time_var = time_var, format = format)

    # Reorder: domain/time terms should come first
    priority_idx <- which(sapply(parts, function(p) {
      any(sapply(.PRIORITY_TERMS, function(pt) startsWith(p, pt)))
    }))

    if (length(priority_idx) > 0 && priority_idx[1] != 1) {
      # Swap order so priority term comes first
      formatted_parts <- rev(formatted_parts)
    }

    return(paste(formatted_parts, collapse = sep))
  }

  # Single term
  .format_single_term(predictor, time_var, format)
}

#' Generate predictor labels automatically for a model
#'
#' Takes coefficient names from a model and generates formatted labels.
#'
#' @param coef_names Character vector of coefficient names
#' @param time_var Optional time variable name
#' @param format "latex" or "text"
#' @return Named character vector of labels
auto_pred_labels <- function(coef_names, time_var = NULL, format = "latex") {
  labels <- sapply(coef_names, format_predictor_label,
                   time_var = time_var, format = format)
  setNames(labels, coef_names)
}

# =============================================================================
# PREDICTOR LABELS FOR REGRESSION TABLES
# =============================================================================
# Defines predictor label mappings used by sjplot_to_latex() across all
# analysis scripts. Many labels are now auto-generated via format_predictor_label()
# but explicit mappings are kept for special cases and backwards compatibility.
# =============================================================================

#' Get predictor labels for regression tables (plain text only)
#'
#' Returns a named character vector mapping raw coefficient names to
#' human-readable labels. These are used by sjPlot to generate
#' HTML tables. The conversion to LaTeX math notation (e.g., $\lambda$)
#' happens AFTER the HTML-to-LaTeX step via create_default_replacements()
#' in sjplot_to_latex.R.
#'
#' This function ALWAYS returns plain text labels.
#' For direct LaTeX output (e.g., contrast tables), use get_latex_symbol().
#'
#' @param time_var Time variable name ("session_numeric", "week_numeric", or NULL)
#' @return Named character vector of predictor labels (plain text)
get_pred_labels <- function(time_var = "session_numeric") {
  # Determine time label for display
  time_label <- if (is.null(time_var)) {
    NULL
  } else {
    switch(time_var,
      "session_numeric" = "Session",
      "week_numeric" = "Week",
      time_var)
  }


  # Base single-term labels - ALWAYS plain text

  # These must match the patterns in create_default_replacements() for LaTeX conversion
  labels <- c(
    "(Intercept)" = "Intercept",
    "lambda" = "Multiplier",
    "I(lambda^2)" = "Multiplier Squared",
    "I(lambda^3)" = "Multiplier Cubed",
    "relationship_seeking_categorypos_lambda" = "Pos Multiplier",
    "relationship_seeking_categoryzero_lambda" = "Zero Multiplier",
    "lambda_factorneg1" = "Multiplier neg1",
    "lambda_factorneg0.5" = "Multiplier neg0.5",
    "lambda_factorzero" = "Multiplier zero",
    "lambda_factorpos0.5" = "Multiplier pos0.5",
    "lambda_factorpos1" = "Multiplier pos1"
  )

  # Common labels
  labels <- c(labels,
    "domainemotchat" = "Emot.",
    "personalisationpersonalised" = "Pers",
    "outcome_measure" = "Outcome Measure",
    "time" = "Time",
    "study_idlongitudinal" = "Longitudinal",
    "outcome_value_pre" = "Pre-treatment Score",
    "seeking_companionship_likelihood_pre" = "Pre-Score",
    "psychosocial_F1_pre" = "Pre-Score",
    "psychosocial_F2_pre" = "Pre-Score",
    "phq_gad_score_pre" = "Pre-Score",
    "ucla_score_pre" = "Pre-Score",
    "lubben_score_pre" = "Pre-Score",
    "who_score_pre" = "Pre-Score",
    "valence_pre" = "Pre-Score",
    "arousal_pre" = "Pre-Score",
    "control_satisfaction_pre" = "Pre-Score"
  )

  # Add time variable label if provided
  if (!is.null(time_var) && !is.null(time_label)) {
    labels <- c(labels, setNames(time_label, time_var))
  }

  # Store metadata for lookup function
  class(labels) <- c("pred_labels", class(labels))
  attr(labels, "time_var") <- time_var
  labels
}

#' Look up or auto-generate a predictor label
#'
#' @param labels Labels from get_pred_labels()
#' @param predictor Predictor name to look up
#' @return Formatted label (plain text)
lookup_pred_label <- function(labels, predictor) {
  # Check explicit labels first
  if (predictor %in% names(labels)) {
    return(labels[[predictor]])
  }
  # Fall back to auto-generation using stored attributes
  time_var <- attr(labels, "time_var")
  format_predictor_label(predictor, time_var, format = "text")
}

# Note: Interaction labels are auto-generated via format_predictor_label().
# No explicit mappings needed - helper functions handle all cases.

# =============================================================================
# LATEX SYMBOL LOOKUP (for direct LaTeX tables)
# =============================================================================

#' Get LaTeX symbol for a term (for direct LaTeX table generation)
#'
#' Used by tables that generate LaTeX directly (e.g., contrast tables)
#' rather than going through sjPlot's HTML pipeline.
#'
#' @param term Raw R term name (e.g., "lambda", "I(lambda^2)")
#' @return LaTeX formatted string (e.g., "$\\lambda$", "$\\lambda^2$")
get_latex_symbol <- function(term) {
  if (term %in% names(.BASE_TERMS)) {
    return(.BASE_TERMS[[term]][["latex"]])
  }
  # Fallback: escape underscores for LaTeX
  gsub("_", "\\_", term, fixed = TRUE)
}


# =============================================================================
# Sociodemographic Predictor Labels
# =============================================================================
# Labels for demographics, psychosocial measures, AI usage, and preference
# clusters. Used in attrition analysis, seeking companionship analysis, and
# any other models with sociodemo predictors.
# =============================================================================

#' Get sociodemographic predictor labels
#'
#' Returns labels for demographic, psychosocial, and behavioral predictors.
#'
#' @return Named character vector of sociodemo predictor labels
get_sociodemo_labels <- function() {
  c(
    # --- Demographics (continuous) ---
    "age" = "Age (years)",
    "education_years" = "Education (years)",

    # --- Demographics (binary/categorical) ---
    "gender_binary" = "Gender",
    "gender_binaryNon-Male" = "Non-Male",
    "gender_binaryMale" = "Male",
    "gender_binaryPrefer not to say" = "Gender: Prefer not to say",

    "disability_binary" = "Disability",
    "disability_binaryHas disability" = "Disabled",
    "disability_binaryYes" = "Disabled",
    "disability_binaryPrefer not to say" = "Disability: Prefer not to say",

    "ethnicity_binary" = "Ethnicity",
    "ethnicity_binaryNon-White" = "Non-White",
    "ethnicity_binaryPrefer not to say" = "Ethnicity: Prefer not to say",

    "income_binary" = "Income",
    "income_binaryLow Income (<GBP30K)" = "Low Income",
    "income_binaryLow" = "Low Income",
    "income_binaryPrefer not to say" = "Income: Prefer not to say",

    "religion_binary" = "Religiosity",
    "religion_binaryReligious" = "Religious",
    "religion_binaryPrefer not to say" = "Religion: Prefer not to say",

    # --- AI Usage ---
    "ai_frequency_coarsened" = "AI Use Frequency",
    "ai_frequency_coarsenedHeavy users" = "Heavy AI Users",
    "ai_frequency_coarsenedModerate users" = "Moderate AI Users",
    "ai_frequency_coarsenedHigh" = "High AI Frequency",
    "ai_frequency_coarsenedLow" = "Low AI Frequency",

    # --- Preference Clusters ---
    "cluster_name" = "Preference Cluster",
    "cluster_nameAnthro Skeptic" = "Anthro Skeptic Cluster",
    "cluster_nameAnthro Enthusiast" = "Pre-Treatment Pro-Relationship-Seeking",
    "cluster_nameLow engagement" = "Low Engagement Cluster",
    "cluster_nameMixed preferences" = "Mixed Preferences Cluster",

    # --- Psychosocial Factors (from EFA) ---
    "pre_psychosocial_F1" = "Emotional Health (F1)",
    "pre_psychosocial_F2" = "Social Health (F2)",
    "post_psychosocial_F1" = "Emotional Health (F1, post)",
    "post_psychosocial_F2" = "Social Health (F2, post)",
    "mas_score" = "Moral Absolutism (MAS-6)",
    "gcs_score" = "Goal Commitment (GCS-5)",

    # --- Behavioral Measures ---
    "seeking_companionship_likelihood" = "Seeking Companionship",
    "charity_amount_gbp" = "Yearly Charity (GBP)",
    "emotchat_effectiveness" = "EmotChat Effectiveness",
    "emotchat_satis" = "EmotChat Satisfaction",
    "polchat_confidence" = "PolChat Confidence",
    "polchat_knowledge" = "PolChat Knowledge",
    "emotional_competency" = "Emotional Competency",
    "political_competency" = "Political Competency"
  )
}


# =============================================================================
# Contrast Table Variable Name Cleaning
# =============================================================================
# Cleans raw R variable names into LaTeX-ready display names for use in
# contrast analysis tables. Applied to text columns in
# format_contrast_df_to_latex() instead of plain underscore escaping.
# =============================================================================

#' Clean R variable names for LaTeX contrast tables
#'
#' Applies minimal, safe substitutions to raw R variable names.
#' Only replaces time variable shorthand (session_numeric -> s,
#' week_numeric -> w) and escapes underscores for LaTeX.
#' All other variable names pass through with underscore escaping only.
#'
#' @param x Character vector of variable names/labels
#' @return Character vector with LaTeX-safe display names
clean_contrast_varnames <- function(x) {
  # --- Time at specific values: session_numeric20 -> s20 ---
  x <- gsub("session_numeric(\\d+)", "s\\1", x)
  x <- gsub("week_numeric(\\d+)", "w\\1", x)

  # --- Time variable alone ---
  x <- gsub("\\bsession_numeric\\b", "s", x)
  x <- gsub("\\bweek_numeric\\b", "w", x)

  # --- Escape underscores for LaTeX ---
  x <- gsub("_", "\\_", x, fixed = TRUE)

  x
}

# =============================================================================
# Parameter Name Cleaning for Markdown/Console Output
# =============================================================================
# Used by extract_coefficients.R to clean raw coefficient names into
# human-readable format for markdown reports and console output.
# =============================================================================

#' Clean raw parameter names for display
#'
#' Transforms raw R coefficient names into human-readable labels for
#' markdown reports and console output. Used by extract_coefficients.R.
#'
#' @param x Character vector of raw parameter names
#' @return Character vector with cleaned display names
clean_parameter_names <- function(x) {
  x %>%
    # Treatment arm prefixes
    stringr::str_replace_all("relationship_seeking_category", "") %>%
    stringr::str_replace_all("outcome_measure", "") %>%
    stringr::str_replace_all("lambda_factor", "lambda=") %>%
    stringr::str_replace_all("personalisationpersonalised", "personalised") %>%
    stringr::str_replace_all("domainemotchat", "emotchat") %>%
    # Time variables
    stringr::str_replace_all("week_numeric", "week") %>%
    stringr::str_replace_all("session_numeric", "session") %>%
    # Study variable
    stringr::str_replace_all("study_id", "study:") %>%
    # Lambda category labels
    stringr::str_replace_all("zero_lambda", "lambda=0") %>%
    stringr::str_replace_all("pos_lambda", "lambda>0") %>%
    stringr::str_replace_all("neg_lambda", "lambda<0") %>%
    # Polynomial terms
    stringr::str_replace_all("I\\(lambda\\^2\\)", "lambda^2") %>%
    stringr::str_replace_all("I\\(lambda\\^3\\)", "lambda^3") %>%
    # 5-level lambda factor levels
    stringr::str_replace_all("neg1", "-1") %>%
    stringr::str_replace_all("neg0.5", "-0.5") %>%
    stringr::str_replace_all("pos0.5", "+0.5") %>%
    stringr::str_replace_all("pos1", "+1") %>%
    # Clean up stray colons
    stringr::str_replace_all("^:", "") %>%
    stringr::str_replace_all(":$", "") %>%
    stringr::str_replace_all("::", ":") %>%
    stringr::str_trim()
}
