# =============================================================================
# LaTeX Contrast Table Utilities
# =============================================================================
#
# Contrast table generation utilities extracted from generate_contrast_tables.R.
# Provides functions for generating LaTeX tables from contrast JSON data.
#
# Dependencies:
#   - latex_utils.R (for %+% operator and base formatting)
#   - tidyverse (dplyr, tidyr)
#   - jsonlite
#
# Usage:
#   source("scripts/utils_r/latex_utils.R")  # Must source first (provides %+%)
#   source("scripts/utils_r/latex_contrast_table_utils.R")
#
# =============================================================================

library(tidyverse)
library(jsonlite)

# =============================================================================
# FORMATTING HELPERS (contrast-specific)
# =============================================================================
# These have specialized functionality for contrast tables.
# Named with _contrast suffix or distinct names to avoid confusion with
# base versions in latex_utils.R.
# =============================================================================

#' Format estimate (contrast-specific)
#'
#' @param x Numeric value
#' @param digits Number of decimal places (default 2)
#' @return Character string
fmt_est <- function(x, digits = 2) {
  if (is.null(x) || is.na(x)) return("---")
  sprintf(paste0("%.", digits, "f"), x)
}

#' Format estimate with CI (contrast-specific, with makecell/colorbox support)
#'
#' More sophisticated than format_est_ci in latex_utils.R - supports:
#' - makecell for multi-line display
#' - colorbox for cell highlighting
#' - textcolor for text coloring
#'
#' @param est Numeric estimate
#' @param lower Lower CI bound
#' @param upper Upper CI bound
#' @param digits Number of decimal places (default 2)
#' @param use_makecell Use makecell for two-line display (default TRUE)
#' @param colorbox Optional colorbox color name
#' @param textcolor Optional textcolor name
#' @return Character string
fmt_ci <- function(est, lower, upper, digits = 2, use_makecell = TRUE,
                   colorbox = NULL, textcolor = NULL) {
  # Handle NULL, NA, empty lists, and length-0 vectors
  placeholder <- if (use_makecell) "\\makecell{---}" else "---"
  if (is.null(est) || length(est) == 0) return(placeholder)
  if (is.list(est)) est <- est[[1]]
  if (is.null(est) || length(est) == 0 || is.na(est)) return(placeholder)

  # Handle lower/upper similarly
  if (is.list(lower)) lower <- lower[[1]]
  if (is.list(upper)) upper <- upper[[1]]
  if (is.null(lower) || length(lower) == 0 || is.na(lower)) {
    return(fmt_est(est, digits))
  }

  if (use_makecell) {
    # Est on first line, [CI] on second line (using makecell, CI in scriptsize)
    inner <- sprintf("\\makecell{%s\\\\{\\scriptsize[%s, %s]}}",
                     fmt_est(est, digits), fmt_est(lower, digits), fmt_est(upper, digits))
    # Wrap with colorbox if requested (ensures full cell coloring with makecell)
    if (!is.null(colorbox)) {
      sprintf("\\colorbox{%s}{%s}", colorbox, inner)
    } else if (!is.null(textcolor)) {
      # Wrap with textcolor if requested (colors the text only)
      sprintf("\\textcolor{%s}{%s}", textcolor, inner)
    } else {
      inner
    }
  } else {
    # Est on first line, [CI] on second line (using shortstack)
    sprintf("\\shortstack{%s\\\\{[}%s, %s{]}}",
            fmt_est(est, digits), fmt_est(lower, digits), fmt_est(upper, digits))
  }
}

#' Format p-value (contrast-specific, with makecell support)
#'
#' @param p Numeric p-value
#' @param bold_threshold Threshold for bolding (default 0.05)
#' @param use_makecell Use makecell for placeholder (default TRUE)
#' @return Character string
fmt_p <- function(p, bold_threshold = 0.05, use_makecell = TRUE) {
  # Handle NULL, NA, empty lists, and length-0 vectors
  placeholder <- "\\makecell{---}"
  if (is.null(p) || length(p) == 0) return(placeholder)
  if (is.list(p)) p <- p[[1]]
  if (is.null(p) || length(p) == 0 || is.na(p)) return(placeholder)
  if (p < 0.001) {
    formatted <- "$<$.001"
  } else {
    formatted <- sprintf("%.3f", p)
  }
  if (p < bold_threshold) {
    formatted <- paste0("\\textbf{", formatted, "}")
  }
  formatted
}

#' Get outcome label for display
#'
#' @param outcome Outcome variable name
#' @return Display label
get_contrast_outcome_label <- function(outcome) {
  # Use \makecell for long labels that need to wrap over two lines
  labels <- c(
    # Preferences
    likeability = "Likeability",
    engagingness = "Engagingness",
    helpfulness = "Helpfulness",
    # Attachment
    perceived_understanding = "\\makecell[l]{Perceived\\\\Understanding}",
    self_disclosure = "Self-Disclosure",
    separation_distress = "\\makecell[l]{Separation\\\\Distress}",
    reliance = "Reliance",
    seeking_companionship_likelihood = "\\makecell[l]{Seeking\\\\Companionship}",
    goodbye_action = "Goodbye Action",
    # Wellbeing
    psychosocial_F1 = "Psychosocial F1",
    psychosocial_F2 = "Psychosocial F2",
    # Affect
    valence = "Valence",
    arousal = "Arousal",
    # Perceptions
    tool_friend = "Tool-Friend",
    perceived_sentience = "\\makecell[l]{Perceived\\\\Sentience}",
    ontological_sentience = "\\makecell[l]{Ontological\\\\Sentience}"
  )
  if (outcome %in% names(labels)) labels[outcome] else tools::toTitleCase(outcome)
}

#' Format measure family name for LaTeX display
#'
#' Converts snake_case family names to proper display names for captions.
#' Removes underscores (which break LaTeX) and applies title case.
#'
#' @param family_name Raw family name (e.g., "momentary_affect")
#' @return Display name (e.g., "Momentary Affect")
format_contrast_family_name <- function(family_name) {
  labels <- c(
    preferences = "Preferences",
    attachment = "Attachment",
    perceptions = "Perceptions",
    psychosocial = "Psychosocial Wellbeing",
    momentary_affect = "Momentary Affect",
    wellbeing = "Wellbeing"
  )
  if (family_name %in% names(labels)) {
    labels[family_name]
  } else {
    tools::toTitleCase(gsub("_", " ", family_name))
  }
}

#' Escape RS label for LaTeX
#'
#' Converts lambda symbols for LaTeX display.
#'
#' @param label Raw label (e.g., "neg_lambda")
#' @return LaTeX-escaped label
escape_rs_label <- function(label) {
  if (is.null(label) || is.na(label) || label == "") return("---")
  label <- gsub("neg_\u03bb", "$\\\\lambda < 0$", label)
  label <- gsub("pos_\u03bb", "$\\\\lambda > 0$", label)
  label
}

#' Get text color based on significance and direction
#'
#' Returns a textcolor name or NULL for no coloring.
#' Used for slope tables to highlight significant results.
#'
#' @param p_local Local p-value
#' @param estimate Estimate value
#' @return Textcolor name or NULL
get_slope_textcolor <- function(p_local, estimate) {
  if (is.list(p_local)) p_local <- p_local[[1]]
  if (is.list(estimate)) estimate <- estimate[[1]]
  if (is.null(p_local) || length(p_local) == 0 || is.na(p_local) || p_local >= 0.05) {
    return(NULL)
  }
  if (is.null(estimate) || length(estimate) == 0 || is.na(estimate)) {
    return(NULL)
  }
  if (estimate > 0) {
    return("green!50!black")
  } else {
    return("purple")
  }
}

# =============================================================================
# ROW FORMATTERS
# =============================================================================

#' Format a single row for main effects table
#'
#' @param row Data frame row
#' @param include_narrow_full Include narrow/full columns
#' @param study_suffix Study type suffix (e.g., " (CS)")
#' @return LaTeX row string
format_main_effect_row <- function(row, include_narrow_full = TRUE, study_suffix = "") {
  colored_suffix <- if (study_suffix == " (CS)") {
    " \\textcolor{cscolor}{(CS)}"
  } else if (study_suffix == " (Long)") {
    " \\textcolor{longcolor}{(Long)}"
  } else {
    study_suffix
  }
  outcome_label <- get_contrast_outcome_label(row$outcome[1]) %+% colored_suffix

  primary <- row$primary[[1]]
  est_ci <- fmt_ci(primary$estimate, primary$lower_cl, primary$upper_cl)
  p_raw <- fmt_p(primary$p_raw)
  p_local <- fmt_p(primary$p_local)
  p_global <- fmt_p(primary$p_global)

  coars <- row$coarsened[[1]]
  if (!is.null(coars) && !is.null(coars$estimate)) {
    coars_est <- fmt_ci(coars$estimate, coars$lower_cl, coars$upper_cl)
    coars_p <- fmt_p(coars$p_raw)
  } else {
    coars_est <- "\\makecell{---}"
    coars_p <- "\\makecell{---}"
  }

  if (include_narrow_full) {
    narrow <- row$narrow[[1]]
    if (!is.null(narrow) && !is.null(narrow$estimate)) {
      narrow_est <- fmt_ci(narrow$estimate, narrow$lower_cl, narrow$upper_cl)
      narrow_p <- fmt_p(narrow$p_raw)
    } else {
      narrow_est <- "\\makecell{---}"
      narrow_p <- "\\makecell{---}"
    }
    full <- row$full[[1]]
    if (!is.null(full) && !is.null(full$estimate)) {
      full_est <- fmt_ci(full$estimate, full$lower_cl, full$upper_cl)
      full_p <- fmt_p(full$p_raw)
    } else {
      full_est <- "\\makecell{---}"
      full_p <- "\\makecell{---}"
    }
  } else {
    narrow_est <- "\\makecell{---}"
    narrow_p <- "\\makecell{---}"
    full_est <- "\\makecell{---}"
    full_p <- "\\makecell{---}"
  }

  cells <- c(outcome_label, est_ci, p_raw, p_local, p_global,
             coars_est, coars_p, narrow_est, narrow_p, full_est, full_p)
  paste(cells, collapse = " & ") %+% " \\\\"
}

#' Format dose response row
#'
#' @param row Data frame row
#' @param study_suffix Study type suffix
#' @param show_outcome Show outcome label
#' @return LaTeX row string
format_dose_response_row <- function(row, study_suffix = "", show_outcome = TRUE) {
  if (show_outcome) {
    colored_suffix <- if (study_suffix == " (CS)") {
      " \\textcolor{cscolor}{(CS)}"
    } else if (study_suffix == " (Long)") {
      " \\textcolor{longcolor}{(Long)}"
    } else {
      study_suffix
    }
    outcome_label <- get_contrast_outcome_label(row$outcome[1]) %+% colored_suffix
  } else {
    outcome_label <- ""
  }

  term <- if (grepl("linear", row$test)) {
    "$\\lambda$"
  } else if (grepl("quadratic", row$test)) {
    "$\\lambda^2$"
  } else if (grepl("cubic", row$test)) {
    "$\\lambda^3$"
  } else {
    row$test
  }

  primary <- row$primary[[1]]
  est_ci <- fmt_ci(primary$estimate, primary$lower_cl, primary$upper_cl)
  p_raw <- fmt_p(primary$p_raw)
  p_local <- fmt_p(primary$p_local)

  cells <- c(outcome_label, term, est_ci, p_raw, p_local)
  paste(cells, collapse = " & ") %+% " \\\\"
}

#' Format moderation row
#'
#' @param row Data frame row
#' @param study_suffix Study type suffix
#' @param show_outcome Show outcome label
#' @param is_interaction Is this an interaction (inferential) row
#' @return LaTeX row string
format_moderation_row <- function(row, study_suffix = "", show_outcome = TRUE,
                                  is_interaction = FALSE) {
  if (show_outcome) {
    colored_suffix <- if (study_suffix == " (CS)") {
      " \\textcolor{cscolor}{(CS)}"
    } else if (study_suffix == " (Long)") {
      " \\textcolor{longcolor}{(Long)}"
    } else {
      study_suffix
    }
    outcome_label <- get_contrast_outcome_label(row$outcome[1]) %+% colored_suffix
  } else {
    outcome_label <- ""
  }

  test_label <- if (is_interaction) "Interaction" else sub(".*= ", "", row$test[1])

  primary <- row$primary[[1]]
  est_ci <- fmt_ci(primary$estimate, primary$lower_cl, primary$upper_cl, use_makecell = TRUE)
  p_raw <- fmt_p(primary$p_raw, use_makecell = FALSE)

  if (is_interaction) {
    outcome_label <- paste0("\\cellcolor{gray!20}", outcome_label)
    test_label <- paste0("\\cellcolor{gray!20}", test_label)
    p_local <- fmt_p(primary$p_local, use_makecell = FALSE)
    p_global <- fmt_p(primary$p_global, use_makecell = FALSE)

    coars <- row$coarsened[[1]]
    if (!is.null(coars) && length(coars) > 0 && !is.null(coars$estimate)) {
      coars_est <- fmt_ci(coars$estimate, coars$lower_cl, coars$upper_cl, use_makecell = TRUE)
      coars_p <- fmt_p(coars$p_raw, use_makecell = FALSE)
    } else {
      coars_est <- "\\makecell{---}"
      coars_p <- "---"
    }
  } else {
    p_local <- "---"
    p_global <- "---"
    coars_est <- "---"
    coars_p <- "---"
  }

  cells <- c(outcome_label, test_label, est_ci, p_raw, p_local, p_global, coars_est, coars_p)
  paste(cells, collapse = " & ") %+% " \\\\"
}

#' Format temporal row
#'
#' @param row Data frame row
#' @param show_outcome Show outcome label
#' @param is_interaction Is this an interaction (inferential) row
#' @param test_override Override test label
#' @return LaTeX row string
format_temporal_row <- function(row, show_outcome = TRUE, is_interaction = FALSE,
                                test_override = NULL) {
  outcome_label <- if (show_outcome) get_contrast_outcome_label(row$outcome[1]) else ""
  test_label <- if (!is.null(test_override)) test_override else row$test[1]

  primary <- row$primary[[1]]
  est_ci <- fmt_ci(primary$estimate, primary$lower_cl, primary$upper_cl, use_makecell = TRUE)
  p_raw <- fmt_p(primary$p_raw, use_makecell = FALSE)

  if (is_interaction) {
    outcome_label <- paste0("\\cellcolor{gray!20}", outcome_label)
    test_label <- paste0("\\cellcolor{gray!20}", test_label)
    p_local <- fmt_p(primary$p_local, use_makecell = FALSE)
    p_global <- fmt_p(primary$p_global, use_makecell = FALSE)

    coars <- row$coarsened[[1]]
    if (!is.null(coars) && length(coars) > 0 && !is.null(coars$estimate)) {
      coars_est <- fmt_ci(coars$estimate, coars$lower_cl, coars$upper_cl, use_makecell = TRUE)
      coars_p <- fmt_p(coars$p_raw, use_makecell = FALSE)
    } else {
      coars_est <- "\\makecell{---}"
      coars_p <- "---"
    }
  } else {
    p_local <- "---"
    p_global <- "---"
    coars_est <- "---"
    coars_p <- "---"
  }

  cells <- c(outcome_label, test_label, est_ci, p_raw, p_local, p_global, coars_est, coars_p)
  paste(cells, collapse = " & ") %+% " \\\\"
}

#' Format time coefficient row
#'
#' @param row Data frame row
#' @param show_outcome Show outcome label
#' @return LaTeX row string
format_time_coef_row <- function(row, show_outcome = TRUE) {
  outcome_label <- if (show_outcome) get_contrast_outcome_label(row$outcome[1]) else ""

  test_name <- row$test[1]
  condition <- sub("^Time coefficient: ", "", test_name)
  condition <- sub("^Time coefficient$", "Overall", condition)
  condition <- gsub("neg_\u03bb", "$\\\\lambda < 0$", condition)
  condition <- gsub("pos_\u03bb", "$\\\\lambda > 0$", condition)

  primary <- row$primary[[1]]
  est_ci <- fmt_ci(primary$estimate, primary$lower_cl, primary$upper_cl, use_makecell = TRUE)
  p_raw <- fmt_p(primary$p_raw, use_makecell = FALSE)
  p_local <- fmt_p(primary$p_local, use_makecell = FALSE)
  p_global <- fmt_p(primary$p_global, use_makecell = FALSE)

  cells <- c(outcome_label, condition, est_ci, p_raw, p_local, p_global)
  paste(cells, collapse = " & ") %+% " \\\\"
}

#' Format slope row with robustness columns
#'
#' @param row Data frame row
#' @param outcome_spec Outcome specification (list with label) or NULL
#' @param has_narrow_full Include narrow/full columns
#' @return LaTeX row string
format_slope_row <- function(row, outcome_spec = NULL, has_narrow_full = FALSE) {
  primary <- row$primary[[1]]
  text_color <- get_slope_textcolor(primary$p_local, primary$estimate)
  dash <- "\\makecell{---}"

  if (!is.null(outcome_spec)) {
    outcome_label <- get_contrast_outcome_label(outcome_spec$label)
  } else {
    outcome_label <- ""
  }

  rs_raw <- row$rs_level[[1]]
  rs_label <- if (!is.null(rs_raw) && !is.na(rs_raw)) escape_rs_label(rs_raw) else ""
  domain_raw <- row$domain_level[[1]]
  domain_label <- if (!is.null(domain_raw) && !is.na(domain_raw)) domain_raw else dash
  pers_raw <- row$pers_level[[1]]
  pers_label <- if (!is.null(pers_raw) && !is.na(pers_raw)) pers_raw else dash

  est_ci <- fmt_ci(primary$estimate, primary$lower_cl, primary$upper_cl,
                   digits = 2, use_makecell = TRUE, textcolor = text_color)
  p_raw <- fmt_p(primary$p_raw, use_makecell = FALSE)
  p_local <- fmt_p(primary$p_local, use_makecell = FALSE)

  coars <- row$coarsened[[1]]
  if (!is.null(coars) && length(coars) > 0 && !is.null(coars$estimate)) {
    coars_ci <- fmt_ci(coars$estimate, coars$lower_cl, coars$upper_cl,
                       digits = 2, use_makecell = TRUE, textcolor = text_color)
    coars_p <- fmt_p(coars$p_raw, use_makecell = FALSE)
  } else {
    coars_ci <- dash
    coars_p <- dash
  }

  if (has_narrow_full) {
    narrow <- row$narrow[[1]]
    if (!is.null(narrow) && length(narrow) > 0 && !is.null(narrow$estimate)) {
      narrow_ci <- fmt_ci(narrow$estimate, narrow$lower_cl, narrow$upper_cl,
                          digits = 2, use_makecell = TRUE, textcolor = text_color)
      narrow_p <- fmt_p(narrow$p_raw, use_makecell = FALSE)
    } else {
      narrow_ci <- dash
      narrow_p <- dash
    }

    full <- row$full[[1]]
    if (!is.null(full) && length(full) > 0 && !is.null(full$estimate)) {
      full_ci <- fmt_ci(full$estimate, full$lower_cl, full$upper_cl,
                        digits = 2, use_makecell = TRUE, textcolor = text_color)
      full_p <- fmt_p(full$p_raw, use_makecell = FALSE)
    } else {
      full_ci <- dash
      full_p <- dash
    }
    cells <- c(outcome_label, rs_label, domain_label, pers_label,
               est_ci, p_raw, p_local, coars_ci, coars_p,
               narrow_ci, narrow_p, full_ci, full_p)
  } else {
    cells <- c(outcome_label, rs_label, domain_label, pers_label,
               est_ci, p_raw, p_local, coars_ci, coars_p)
  }

  paste(cells, collapse = " & ") %+% " \\\\"
}

# =============================================================================
# TABLE GENERATORS
# =============================================================================

#' Generate X1 table for a specific effect type (RS, Domain, or Pers)
#'
#' @param contrasts Data frame of contrasts
#' @param family_name Name of measure family
#' @param effect_type One of "RS", "Domain", "Pers"
#' @param label_suffix Suffix for table label (e.g., "_long", "_cs")
#' @return LaTeX lines or NULL
generate_table_X1_single <- function(contrasts, family_name, effect_type,
                                     label_suffix = "") {
  cat("\n--- Generating Table X1_", effect_type, ": Main Effects ---\n", sep = "")

  effect_info <- list(
    RS = list(
      test_type = "main_effect_RS",
      test_pattern = "^RS:",
      title = "Relationship-Seeking: avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)",
      include_narrow_full = TRUE
    ),
    Domain = list(
      test_type = "main_effect_Domain",
      test_pattern = "^Domain:",
      title = "Domain: emotchat $-$ polchat",
      include_narrow_full = FALSE
    ),
    Pers = list(
      test_type = "main_effect_Pers",
      test_pattern = "^Personalisation:",
      title = "Personalisation: personalised $-$ non-personalised",
      include_narrow_full = FALSE
    )
  )

  info <- effect_info[[effect_type]]
  if (is.null(info)) {
    cat("  Unknown effect_type:", effect_type, "\n")
    return(NULL)
  }

  main_effects <- contrasts %>% filter(test_type == info$test_type)
  outcomes <- unique(main_effects$outcome)
  col_spec <- "l r r r r | r r | r r | r r"

  p_local_equals_global <- all(sapply(main_effects$primary, function(x) {
    if (is.null(x$p_local) || is.null(x$p_global)) return(TRUE)
    isTRUE(all.equal(x$p_local, x$p_global, tolerance = 1e-10))
  }))

  single_family_note <- if (p_local_equals_global) {
    " For " %+% format_contrast_family_name(family_name) %+%
      ", $p_{\\text{local}} = p_{\\text{global}}$ (single local family per study)."
  } else {
    ""
  }

  if (nrow(main_effects) == 0) {
    cat("  No", effect_type, "main effects found, skipping.\n")
    return(NULL)
  }

  has_cs <- any(main_effects$study_type == "cross_sectional")
  has_long <- any(main_effects$study_type == "longitudinal")
  study_type_note <- if (has_cs && !has_long) {
    "Cross-sectional study."
  } else if (!has_cs && has_long) {
    "Longitudinal study."
  } else {
    "CS = Cross-sectional. Long = Longitudinal."
  }

  caption_text <- effect_type %+% " Main Effects --- " %+%
    format_contrast_family_name(family_name) %+% ". " %+%
    "\\footnotesize{" %+%
    "Est [CI] = estimate with 95\\% confidence interval. " %+%
    "$p$ = uncorrected p-value. " %+%
    "$p_{\\text{local}}$ = FDR-corrected within " %+% effect_type %+% " effects. " %+%
    "$p_{\\text{global}}$ = FDR-corrected across all outcomes. " %+%
    "Robustness: Coarsened = 3-level $\\lambda$. " %+%
    (if (info$include_narrow_full)
      "Sensitivity: Narrow = $\\pm$0.5 range, Full = $\\pm$1.0 range. "
     else "") %+%
    "Robustness/sensitivity analyses report uncorrected $p$-values. " %+%
    study_type_note %+% single_family_note %+% "}"

  header_row1 <- " & \\multicolumn{4}{c|}{\\textit{Primary}} & \\multicolumn{2}{c|}{\\textit{Robustness}} & \\multicolumn{4}{c}{\\textit{Sensitivity}} \\\\"
  header_row2 <- " & & & & & \\multicolumn{2}{c|}{Coarsened} & \\multicolumn{2}{c|}{Narrow} & \\multicolumn{2}{c}{Full} \\\\"
  header_row3 <- "Outcome & \\multicolumn{1}{c}{Est [CI]} & $p$ & $p_{\\text{local}}$ & $p_{\\text{global}}$ & \\multicolumn{1}{c}{Est [CI]} & $p$ & \\multicolumn{1}{c}{Est [CI]} & $p$ & \\multicolumn{1}{c}{Est [CI]} & $p$ \\\\"

  table_label <- family_name %+% "_X1_" %+% effect_type %+% label_suffix

  lines <- c(
    "\\footnotesize",
    "\\begin{longtable}{" %+% col_spec %+% "}",
    "\\caption{" %+% caption_text %+% "}",
    "\\label{tab:" %+% table_label %+% "} \\\\",
    "\\toprule",
    header_row1,
    header_row2,
    "\\cmidrule{2-5} \\cmidrule{6-7} \\cmidrule{8-11}",
    header_row3,
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{11}{c}{\\textit{Table \\ref{tab:" %+% table_label %+% "} continued}} \\\\",
    "\\toprule",
    header_row1,
    header_row2,
    "\\cmidrule{2-5} \\cmidrule{6-7} \\cmidrule{8-11}",
    header_row3,
    "\\midrule",
    "\\endhead",
    "",
    "\\midrule",
    "\\multicolumn{11}{r}{\\textit{Continued on next page}} \\\\",
    "\\endfoot",
    "",
    "\\bottomrule",
    "\\endlastfoot"
  )

  lines <- c(lines,
    "\\midrule\\midrule",
    "\\multicolumn{11}{l}{\\textbf{" %+% info$title %+% "}} \\\\",
    "\\midrule\\midrule"
  )

  # Cross-sectional
  cs_effects <- main_effects %>% filter(study_type == "cross_sectional")
  if (nrow(cs_effects) > 0) {
    prev_local_family <- NULL
    for (out in outcomes) {
      row <- cs_effects %>% filter(outcome == out)
      if (nrow(row) > 0) {
        current_local_family <- row$local_family[1]
        if (!is.null(prev_local_family) && !is.na(current_local_family) &&
            current_local_family != prev_local_family) {
          lines <- c(lines, "\\midrule")
        }
        lines <- c(lines, format_main_effect_row(row, include_narrow_full = info$include_narrow_full, study_suffix = " (CS)"))
        prev_local_family <- current_local_family
      }
    }
  }

  lines <- c(lines, "\\cmidrule{1-11}")

  # Longitudinal
  long_effects <- main_effects %>% filter(study_type == "longitudinal")
  if (nrow(long_effects) > 0) {
    prev_local_family <- NULL
    for (out in outcomes) {
      row <- long_effects %>% filter(outcome == out)
      if (nrow(row) > 0) {
        current_local_family <- row$local_family[1]
        if (!is.null(prev_local_family) && !is.na(current_local_family) &&
            current_local_family != prev_local_family) {
          lines <- c(lines, "\\midrule")
        }
        lines <- c(lines, format_main_effect_row(row, include_narrow_full = info$include_narrow_full, study_suffix = " (Long)"))
        prev_local_family <- current_local_family
      }
    }
  }

  lines <- c(lines, "\\end{longtable}", "\\normalsize")
  lines
}

#' Generate all three X1 tables (RS, Domain, Pers)
#'
#' @param contrasts Data frame of contrasts
#' @param family_name Name of measure family
#' @param label_suffix Suffix for table labels (e.g., "_long", "_cs")
#' @return List of tables for each effect type
generate_table_X1 <- function(contrasts, family_name, label_suffix = "") {
  list(
    RS = generate_table_X1_single(contrasts, family_name, "RS", label_suffix),
    Domain = generate_table_X1_single(contrasts, family_name, "Domain", label_suffix),
    Pers = generate_table_X1_single(contrasts, family_name, "Pers", label_suffix)
  )
}

#' Generate X2 table: Dose-Response (Functional Form)
#'
#' @param contrasts Data frame of contrasts
#' @param family_name Name of measure family
#' @param label_suffix Suffix for table label (e.g., "_long", "_cs")
#' @return LaTeX lines or NULL
generate_table_X2 <- function(contrasts, family_name, label_suffix = "") {
  cat("\n--- Generating Table X.2: Dose-Response ---\n")

  dose_response <- contrasts %>% filter(test_type == "dose_response")

  if (nrow(dose_response) == 0) {
    cat("  No dose-response contrasts found, skipping.\n")
    return(NULL)
  }

  outcomes <- unique(dose_response$outcome)
  col_spec <- "l l r r r"

  caption_text <- "Dose-Response (Functional Form) --- " %+%
    format_contrast_family_name(family_name) %+% ". " %+%
    "\\footnotesize{" %+%
    "Est [CI] = estimate with 95\\% confidence interval. " %+%
    "$p$ = uncorrected p-value. " %+%
    "$p_{\\text{local}}$ = FDR-corrected within family. " %+%
    "CS = Cross-sectional. Long = Longitudinal. " %+%
    "Coarsened robustness checks are not applicable to polynomial " %+%
    "dose-response terms; the coarsened model uses categorical RS, " %+%
    "reported in the RS main effects table.}"

  header_row <- "Outcome & Term & \\multicolumn{1}{c}{Est [CI]} & $p$ & $p_{\\text{local}}$ \\\\"

  lines <- c(
    "\\footnotesize",
    "\\begin{longtable}{" %+% col_spec %+% "}",
    "\\caption{" %+% caption_text %+% "}",
    "\\label{tab:" %+% family_name %+% "_X2_dose_response" %+% label_suffix %+% "} \\\\",
    "\\toprule",
    header_row,
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{5}{c}{\\textit{Table \\ref{tab:" %+% family_name %+% "_X2_dose_response" %+% label_suffix %+% "} continued}} \\\\",
    "\\toprule",
    header_row,
    "\\midrule",
    "\\endhead",
    "",
    "\\midrule",
    "\\multicolumn{5}{r}{\\textit{Continued on next page}} \\\\",
    "\\endfoot",
    "",
    "\\bottomrule",
    "\\endlastfoot"
  )

  # Cross-sectional
  cs_data <- dose_response %>% filter(study_type == "cross_sectional")
  if (nrow(cs_data) > 0) {
    prev_local_family <- NULL
    for (out in outcomes) {
      out_data <- cs_data %>% filter(outcome == out) %>%
        mutate(term_order = case_when(
          grepl("linear", test) ~ 1, grepl("quadratic", test) ~ 2,
          grepl("cubic", test) ~ 3, TRUE ~ 4
        )) %>% arrange(term_order)
      if (nrow(out_data) > 0) {
        current_local_family <- out_data$local_family[1]
        if (!is.null(prev_local_family) && !is.na(current_local_family) &&
            current_local_family != prev_local_family) {
          lines <- c(lines, "\\midrule")
        }
        for (i in seq_len(nrow(out_data))) {
          lines <- c(lines, format_dose_response_row(out_data[i, ], " (CS)", i == 1))
        }
        prev_local_family <- current_local_family
      }
    }
  }

  lines <- c(lines, "\\midrule")

  # Longitudinal
  long_data <- dose_response %>% filter(study_type == "longitudinal")
  if (nrow(long_data) > 0) {
    prev_local_family <- NULL
    for (out in outcomes) {
      out_data <- long_data %>% filter(outcome == out) %>%
        mutate(term_order = case_when(
          grepl("linear", test) ~ 1, grepl("quadratic", test) ~ 2,
          grepl("cubic", test) ~ 3, TRUE ~ 4
        )) %>% arrange(term_order)
      if (nrow(out_data) > 0) {
        current_local_family <- out_data$local_family[1]
        if (!is.null(prev_local_family) && !is.na(current_local_family) &&
            current_local_family != prev_local_family) {
          lines <- c(lines, "\\midrule")
        }
        for (i in seq_len(nrow(out_data))) {
          lines <- c(lines, format_dose_response_row(out_data[i, ], " (Long)", i == 1))
        }
        prev_local_family <- current_local_family
      }
    }
  }

  lines <- c(lines, "\\end{longtable}", "\\normalsize")
  lines
}

#' Add moderation block to lines
#'
#' @param lines Existing lines
#' @param mod_data Moderation data
#' @param outcomes Outcome names
#' @param study_type Study type filter
#' @param moderator Moderator name
#' @param study_suffix Study type suffix
#' @return Updated lines
add_moderation_block <- function(lines, mod_data, outcomes, study_type, moderator,
                                 study_suffix) {
  block_data <- mod_data %>%
    filter(study_type == !!study_type, grepl(moderator, test, fixed = TRUE))
  if (nrow(block_data) == 0) return(lines)

  prev_local_family <- NULL
  for (out in outcomes) {
    out_data <- block_data %>% filter(outcome == out)
    if (nrow(out_data) == 0) next

    current_local_family <- out_data$local_family[1]
    if (!is.null(prev_local_family) && !is.na(current_local_family) &&
        current_local_family != prev_local_family) {
      lines <- c(lines, "\\midrule")
    }

    interaction <- out_data %>% filter(test_type == "moderation")
    simple_effects <- out_data %>% filter(test_type == "simple_effect")

    if (nrow(simple_effects) > 0) {
      simple_effects <- simple_effects %>%
        mutate(sort_order = case_when(
          grepl("emotchat", test) ~ 1,
          grepl("polchat", test) ~ 2,
          grepl("= personalised", test) ~ 1,
          grepl("non-personalised", test) ~ 2,
          TRUE ~ 99
        )) %>%
        arrange(sort_order)
    }

    if (nrow(interaction) > 0) {
      lines <- c(lines, format_moderation_row(interaction[1, ], study_suffix, TRUE, TRUE))
    }
    for (i in seq_len(nrow(simple_effects))) {
      lines <- c(lines, format_moderation_row(simple_effects[i, ], "", FALSE, FALSE))
    }
    prev_local_family <- current_local_family
  }
  lines
}

#' Generate X3 table: Moderation
#'
#' @param contrasts Data frame of contrasts
#' @param family_name Name of measure family
#' @return LaTeX lines or NULL
generate_table_X3 <- function(contrasts, family_name) {
  cat("\n--- Generating Table X.3: Moderation ---\n")

  mod_data <- contrasts %>% filter(test_type %in% c("moderation", "simple_effect"))

  if (nrow(mod_data) == 0) {
    cat("  No moderation contrasts found, skipping.\n")
    return(NULL)
  }

  outcomes <- unique(mod_data$outcome)
  col_spec <- "l l r r r r | r r"

  caption_text <- "Moderation Analyses --- " %+%
    format_contrast_family_name(family_name) %+% ". " %+%
    "\\footnotesize{" %+%
    "Est [CI] = estimate with 95\\% confidence interval. " %+%
    "$p$ = uncorrected p-value. " %+%
    "$p_{\\text{local}}$ = FDR-corrected within test family. " %+%
    "$p_{\\text{global}}$ = FDR-corrected across all test families within study design (CS and Long corrected separately). " %+%
    "Grey rows = interaction tests (inferential, included in FDR family). " %+%
    "White rows = simple effects (descriptive, not hypothesis tests). " %+%
    "CS = Cross-sectional. Long = Longitudinal. " %+%
    "Robustness analyses report uncorrected $p$-values. " %+%
    "For " %+% format_contrast_family_name(family_name) %+%
    ", $p_{\\text{local}} = p_{\\text{global}}$ (single local family per study).}"

  header_row1 <- " & & \\multicolumn{4}{c|}{\\textit{Primary}} & \\multicolumn{2}{c}{\\textit{Robustness}} \\\\"
  header_row2 <- " & & & & & & \\multicolumn{2}{c}{Coarsened} \\\\"
  header_row3 <- "Outcome & Test & \\multicolumn{1}{c}{Est [CI]} & $p$ & $p_{\\text{local}}$ & $p_{\\text{global}}$ & \\multicolumn{1}{c}{Est [CI]} & $p$ \\\\"

  lines <- c(
    "\\footnotesize",
    "\\begin{longtable}{" %+% col_spec %+% "}",
    "\\caption{" %+% caption_text %+% "}",
    "\\label{tab:" %+% family_name %+% "_X3_moderation} \\\\",
    "\\toprule",
    header_row1, header_row2,
    "\\cmidrule{3-6} \\cmidrule{7-8}",
    header_row3,
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{8}{c}{\\textit{Table \\ref{tab:" %+% family_name %+% "_X3_moderation} continued}} \\\\",
    "\\toprule",
    header_row1, header_row2,
    "\\cmidrule{3-6} \\cmidrule{7-8}",
    header_row3,
    "\\midrule",
    "\\endhead",
    "",
    "\\midrule",
    "\\multicolumn{8}{r}{\\textit{Continued on next page}} \\\\",
    "\\endfoot",
    "",
    "\\bottomrule",
    "\\endlastfoot"
  )

  # Block 1: RS x Domain
  lines <- c(lines,
    "\\midrule\\midrule",
    "\\multicolumn{8}{l}{\\textbf{Relationship-Seeking $\\times$ Domain}} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize Interaction = [avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)]$_{\\text{emotchat}}$ $-$ [avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)]$_{\\text{polchat}}$} \\\\",
    "\\midrule\\midrule"
  )
  lines <- add_moderation_block(lines, mod_data, outcomes, "cross_sectional", "Domain", " (CS)")
  lines <- c(lines, "\\midrule")
  lines <- add_moderation_block(lines, mod_data, outcomes, "longitudinal", "Domain", " (Long)")

  # Block 2: RS x Personalisation
  lines <- c(lines,
    "\\midrule\\midrule",
    "\\multicolumn{8}{l}{\\textbf{Relationship-Seeking $\\times$ Personalisation}} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize Interaction = [avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)]$_{\\text{pers}}$ $-$ [avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)]$_{\\text{non-pers}}$} \\\\",
    "\\midrule\\midrule"
  )
  lines <- add_moderation_block(lines, mod_data, outcomes, "cross_sectional", "Personalisation", " (CS)")
  lines <- c(lines, "\\midrule")
  lines <- add_moderation_block(lines, mod_data, outcomes, "longitudinal", "Personalisation", " (Long)")

  lines <- c(lines, "\\end{longtable}", "\\normalsize")
  lines
}

#' Generate X4a table: Temporal Dynamics
#'
#' @param contrasts Data frame of contrasts
#' @param family_name Name of measure family
#' @param time_prefix Time prefix ("S" for session, "W" for week)
#' @param time_points Time point range (e.g., c(1, 20))
#' @return LaTeX lines or NULL
generate_table_X4a <- function(contrasts, family_name, time_prefix = "S",
                               time_points = c(1, 20)) {
  cat("\n--- Generating Table X.4a: Temporal Dynamics ---\n")

  t1_label <- paste0(time_prefix, time_points[1])
  t2_label <- paste0(time_prefix, time_points[2])
  time_diff <- paste0(t2_label, " $-$ ", t1_label)

  temporal <- contrasts %>%
    filter(test_type %in% c("temporal_main", "temporal_interaction", "temporal_simple"),
           study_type == "longitudinal")

  if (nrow(temporal) == 0) {
    cat("  No temporal contrasts found, skipping.\n")
    return(NULL)
  }

  outcomes <- unique(temporal$outcome)
  col_spec <- "l l r r r r | r r"

  caption_text <- "Temporal Dynamics --- " %+%
    format_contrast_family_name(family_name) %+% ". " %+%
    "\\footnotesize{" %+%
    "Est [CI] = estimate with 95\\% confidence interval. " %+%
    "$p$ = uncorrected p-value. " %+%
    "$p_{\\text{local}}$ = FDR-corrected within test family. " %+%
    "$p_{\\text{global}}$ = FDR-corrected across all test families within study design (CS and Long corrected separately). " %+%
    "Grey rows = inferential tests (included in FDR family). " %+%
    "White rows = simple effects (descriptive, not hypothesis tests). " %+%
    "Robustness analyses report uncorrected $p$-values. " %+%
    "For " %+% format_contrast_family_name(family_name) %+%
    ", $p_{\\text{local}} = p_{\\text{global}}$ (single local family per study).}"

  header_row1 <- " & & \\multicolumn{4}{c|}{\\textit{Primary}} & \\multicolumn{2}{c}{\\textit{Robustness}} \\\\"
  header_row2 <- " & & & & & & \\multicolumn{2}{c}{Coarsened} \\\\"
  header_row3 <- "Outcome & Test & \\multicolumn{1}{c}{Est [CI]} & $p$ & $p_{\\text{local}}$ & $p_{\\text{global}}$ & \\multicolumn{1}{c}{Est [CI]} & $p$ \\\\"

  lines <- c(
    "\\footnotesize",
    "\\begin{longtable}{" %+% col_spec %+% "}",
    "\\caption{" %+% caption_text %+% "}",
    "\\label{tab:" %+% family_name %+% "_X4a_temporal} \\\\",
    "\\toprule",
    header_row1, header_row2,
    "\\cmidrule{3-6} \\cmidrule{7-8}",
    header_row3,
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{8}{c}{\\textit{Table \\ref{tab:" %+% family_name %+% "_X4a_temporal} continued}} \\\\",
    "\\toprule",
    header_row1, header_row2,
    "\\cmidrule{3-6} \\cmidrule{7-8}",
    header_row3,
    "\\midrule",
    "\\endhead",
    "",
    "\\midrule",
    "\\multicolumn{8}{r}{\\textit{Continued on next page}} \\\\",
    "\\endfoot",
    "",
    "\\bottomrule",
    "\\endlastfoot"
  )

  # Block 1: Time Main Effect
  lines <- c(lines,
    "\\midrule\\midrule",
    "\\multicolumn{8}{l}{\\textbf{Time Main Effect}: " %+% time_diff %+% "} \\\\",
    "\\midrule\\midrule"
  )
  time_main <- temporal %>% filter(test_type == "temporal_main")
  for (out in outcomes) {
    row <- time_main %>% filter(outcome == out)
    if (nrow(row) > 0) {
      lines <- c(lines, format_temporal_row(row, TRUE, TRUE, time_diff))
    }
  }

  # Block 2: RS x Time
  lines <- c(lines,
    "\\midrule\\midrule",
    "\\multicolumn{8}{l}{\\textbf{Relationship-Seeking $\\times$ Time}} \\\\",
    "\\multicolumn{8}{l}{\\footnotesize Interaction = [avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)]$_{\\text{" %+%
      t2_label %+% "}}$ $-$ [avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)]$_{\\text{" %+%
      t1_label %+% "}}$} \\\\",
    "\\midrule\\midrule"
  )
  rs_time_int <- temporal %>% filter(grepl("Relationship-Seeking.*Time", test))
  rs_time_simple <- temporal %>% filter(grepl("Relationship-Seeking at S|Relationship-Seeking at W", test))
  for (out in outcomes) {
    int_row <- rs_time_int %>% filter(outcome == out)
    if (nrow(int_row) > 0) {
      lines <- c(lines, format_temporal_row(int_row, TRUE, TRUE, "Interaction"))
    }
    simple_rows <- rs_time_simple %>% filter(outcome == out)
    if (nrow(simple_rows) > 0) {
      simple_rows <- simple_rows %>%
        mutate(sort_order = case_when(
          grepl("S20|W4", test) ~ 1,
          grepl("S1|W1", test) ~ 2,
          TRUE ~ 99
        )) %>%
        arrange(sort_order)
      for (i in seq_len(nrow(simple_rows))) {
        test_label <- sub("Relationship-Seeking at ", "", simple_rows$test[i])
        lines <- c(lines, format_temporal_row(simple_rows[i, ], FALSE, FALSE, test_label))
      }
    }
  }

  # Block 3: Domain x Time
  domain_time_int <- temporal %>% filter(grepl("Domain.*Time", test))
  domain_time_simple <- temporal %>% filter(grepl("Domain at S|Domain at W", test))
  if (nrow(domain_time_int) > 0 || nrow(domain_time_simple) > 0) {
    lines <- c(lines,
      "\\midrule\\midrule",
      "\\multicolumn{8}{l}{\\textbf{Domain $\\times$ Time}} \\\\",
      "\\multicolumn{8}{l}{\\footnotesize Interaction = [emotchat $-$ polchat]$_{\\text{" %+%
        t2_label %+% "}}$ $-$ [emotchat $-$ polchat]$_{\\text{" %+% t1_label %+% "}}$} \\\\",
      "\\midrule\\midrule"
    )
    for (out in outcomes) {
      int_row <- domain_time_int %>% filter(outcome == out)
      if (nrow(int_row) > 0) {
        lines <- c(lines, format_temporal_row(int_row, TRUE, TRUE, "Interaction"))
      }
      simple_rows <- domain_time_simple %>% filter(outcome == out)
      if (nrow(simple_rows) > 0) {
        simple_rows <- simple_rows %>%
          mutate(sort_order = case_when(
            grepl("S20|W4", test) ~ 1,
            grepl("S1|W1", test) ~ 2,
            TRUE ~ 99
          )) %>%
          arrange(sort_order)
        for (i in seq_len(nrow(simple_rows))) {
          test_label <- sub("Domain at ", "", simple_rows$test[i])
          lines <- c(lines, format_temporal_row(simple_rows[i, ], FALSE, FALSE, test_label))
        }
      }
    }
  }

  # Block 4: Personalisation x Time
  pers_time_int <- temporal %>% filter(grepl("Personalisation.*Time", test))
  pers_time_simple <- temporal %>% filter(grepl("Personalisation at S|Personalisation at W", test))
  if (nrow(pers_time_int) > 0 || nrow(pers_time_simple) > 0) {
    lines <- c(lines,
      "\\midrule\\midrule",
      "\\multicolumn{8}{l}{\\textbf{Personalisation $\\times$ Time}} \\\\",
      "\\multicolumn{8}{l}{\\footnotesize Interaction = [pers $-$ non-pers]$_{\\text{" %+%
        t2_label %+% "}}$ $-$ [pers $-$ non-pers]$_{\\text{" %+% t1_label %+% "}}$} \\\\",
      "\\midrule\\midrule"
    )
    for (out in outcomes) {
      int_row <- pers_time_int %>% filter(outcome == out)
      if (nrow(int_row) > 0) {
        lines <- c(lines, format_temporal_row(int_row, TRUE, TRUE, "Interaction"))
      }
      simple_rows <- pers_time_simple %>% filter(outcome == out)
      if (nrow(simple_rows) > 0) {
        simple_rows <- simple_rows %>%
          mutate(sort_order = case_when(
            grepl("S20|W4", test) ~ 1,
            grepl("S1|W1", test) ~ 2,
            TRUE ~ 99
          )) %>%
          arrange(sort_order)
        for (i in seq_len(nrow(simple_rows))) {
          test_label <- sub("Personalisation at ", "", simple_rows$test[i])
          lines <- c(lines, format_temporal_row(simple_rows[i, ], FALSE, FALSE, test_label))
        }
      }
    }
  }

  lines <- c(lines, "\\end{longtable}", "\\normalsize")
  lines
}

#' Generate X4b table: Time Coefficients (Rate of Change)
#'
#' @param contrasts Data frame of contrasts
#' @param family_name Name of measure family
#' @param time_prefix Time prefix
#' @return LaTeX lines or NULL
generate_table_X4b <- function(contrasts, family_name, time_prefix = "S") {
  cat("\n--- Generating Table X.4b: Time Coefficients ---\n")

  time_coef <- contrasts %>%
    filter(test_type == "temporal_coefficient", study_type == "longitudinal")

  if (nrow(time_coef) == 0) {
    cat("  No time coefficient contrasts found, skipping.\n")
    return(NULL)
  }

  outcomes <- unique(time_coef$outcome)
  col_spec <- "l l r r r r"
  time_unit <- if (time_prefix == "W") "week" else "session"

  caption_text <- "Time Coefficients (Rate of Change) --- " %+%
    format_contrast_family_name(family_name) %+% ". " %+%
    "\\footnotesize{" %+%
    "Est [CI] = estimated change per " %+% time_unit %+%
    " with 95\\% confidence interval. " %+%
    "$p$ = uncorrected p-value. " %+%
    "$p_{\\text{local}}$ = FDR-corrected within family. " %+%
    "$p_{\\text{global}}$ = FDR-corrected across all outcomes. " %+%
    "For " %+% format_contrast_family_name(family_name) %+%
    ", $p_{\\text{local}} = p_{\\text{global}}$ (single local family per study).}"

  header_row <- paste0(
    "Outcome & Condition & \\multicolumn{1}{c}{Est [CI]} & ",
    "$p$ & $p_{\\text{local}}$ & $p_{\\text{global}}$ \\\\"
  )

  lines <- c(
    "\\footnotesize",
    "\\begin{longtable}{" %+% col_spec %+% "}",
    "\\caption{" %+% caption_text %+% "}",
    "\\label{tab:" %+% family_name %+% "_X4b_timecoef} \\\\",
    "\\toprule",
    header_row,
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{6}{c}{\\textit{Table \\ref{tab:" %+%
      family_name %+% "_X4b_timecoef} continued}} \\\\",
    "\\toprule",
    header_row,
    "\\midrule",
    "\\endhead",
    "",
    "\\midrule",
    "\\multicolumn{6}{r}{\\textit{Continued on next page}} \\\\",
    "\\endfoot",
    "",
    "\\bottomrule",
    "\\endlastfoot"
  )

  prev_local_family <- NULL
  for (out in outcomes) {
    out_data <- time_coef %>% filter(outcome == out)
    if (nrow(out_data) == 0) next

    current_local_family <- out_data$local_family[1]
    if (!is.null(prev_local_family) && !is.na(current_local_family) &&
        current_local_family != prev_local_family) {
      lines <- c(lines, "\\midrule")
    }

    for (i in seq_len(nrow(out_data))) {
      row <- out_data[i, ]
      show_outcome <- (i == 1)
      lines <- c(lines, format_time_coef_row(row, show_outcome))
    }
    prev_local_family <- current_local_family
  }

  lines <- c(lines, "\\end{longtable}", "\\normalsize")
  lines
}

#' Generate a single slope sub-table
#'
#' @param slopes_data Slopes data
#' @param family_name Family name
#' @param table_suffix Table suffix (e.g., "X5a")
#' @param way_label Way label (e.g., "1-Way")
#' @param subtitle Subtitle
#' @param outcomes Outcomes
#' @param has_narrow_full Include narrow/full columns
#' @return LaTeX lines
generate_slope_subtable <- function(slopes_data, family_name, table_suffix, way_label,
                                    subtitle, outcomes, has_narrow_full = TRUE) {
  if (has_narrow_full) {
    col_spec <- "l l l l r r r | r r | r r | r r"
    ncols <- 13
  } else {
    col_spec <- "l l l l r r r | r r"
    ncols <- 9
  }

  caption_text <- way_label %+% " Temporal Slopes --- " %+%
    format_contrast_family_name(family_name) %+% ". " %+%
    "\\footnotesize{" %+% subtitle %+% " " %+%
    "Slope = change per session. " %+%
    "$p$ = uncorrected. $p_{\\text{local}}$ = FDR-corrected within " %+%
    way_label %+% " slopes. " %+%
    "\\textcolor{green!50!black}{Green text} = significant positive slope. " %+%
    "\\textcolor{purple}{Purple text} = significant negative slope. " %+%
    "Robustness: Coarsened = 3-level $\\lambda$; " %+%
    "Narrow = $\\pm$0.5 $\\lambda$; Full = $\\pm$1.0 $\\lambda$. " %+%
    "Robustness/sensitivity analyses report uncorrected $p$-values.}"

  if (has_narrow_full) {
    header_row1 <- " & & & & \\multicolumn{3}{c|}{\\textit{Primary}} & \\multicolumn{2}{c|}{\\textit{Coarsened}} & \\multicolumn{2}{c|}{\\textit{Narrow}} & \\multicolumn{2}{c}{\\textit{Full}} \\\\"
    header_row2 <- "Outcome & RS & Domain & Pers & Est [CI] & $p$ & $p_{\\text{local}}$ & Est [CI] & $p$ & Est [CI] & $p$ & Est [CI] & $p$ \\\\"
    cmidrule <- "\\cmidrule{5-7} \\cmidrule{8-9} \\cmidrule{10-11} \\cmidrule{12-13}"
  } else {
    header_row1 <- " & & & & \\multicolumn{3}{c|}{\\textit{Primary}} & \\multicolumn{2}{c}{\\textit{Coarsened}} \\\\"
    header_row2 <- "Outcome & RS & Domain & Pers & Est [CI] & $p$ & $p_{\\text{local}}$ & Est [CI] & $p$ \\\\"
    cmidrule <- "\\cmidrule{5-7} \\cmidrule{8-9}"
  }

  lines <- c(
    "\\footnotesize",
    "\\setlength{\\tabcolsep}{1.5pt}",
    "\\begin{longtable}{" %+% col_spec %+% "}",
    "\\caption{" %+% caption_text %+% "}",
    "\\label{tab:" %+% family_name %+% "_" %+% table_suffix %+% "_slopes} \\\\",
    "\\toprule",
    header_row1,
    cmidrule,
    header_row2,
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{" %+% ncols %+% "}{c}{\\textit{Table \\ref{tab:" %+%
      family_name %+% "_" %+% table_suffix %+% "_slopes} continued}} \\\\",
    "\\toprule",
    header_row1,
    cmidrule,
    header_row2,
    "\\midrule",
    "\\endhead",
    "",
    "\\midrule",
    "\\multicolumn{" %+% ncols %+% "}{r}{\\textit{Continued on next page}} \\\\",
    "\\endfoot",
    "",
    "\\bottomrule",
    "\\endlastfoot"
  )

  # Define sections based on way_label
  if (way_label == "1-Way") {
    sections <- list(
      list(
        filter_fn = function(d) !is.na(d$rs_level) & is.na(d$domain_level) & is.na(d$pers_level),
        header = "\\textbf{Relationship-Seeking}",
        desc = "Slope at $\\lambda < 0$ (relationship-avoiding) and $\\lambda > 0$ (relationship-seeking)"
      ),
      list(
        filter_fn = function(d) is.na(d$rs_level) & !is.na(d$domain_level) & is.na(d$pers_level),
        header = "\\textbf{Domain}",
        desc = "Slope within each conversation domain"
      ),
      list(
        filter_fn = function(d) is.na(d$rs_level) & is.na(d$domain_level) & !is.na(d$pers_level),
        header = "\\textbf{Personalisation}",
        desc = "Slope within each personalisation condition"
      )
    )
  } else if (way_label == "2-Way") {
    sections <- list(
      list(
        filter_fn = function(d) !is.na(d$rs_level) & !is.na(d$domain_level) & is.na(d$pers_level),
        header = "\\textbf{Relationship-Seeking $\\times$ Domain}",
        desc = "Slope at each RS level within each domain"
      ),
      list(
        filter_fn = function(d) !is.na(d$rs_level) & is.na(d$domain_level) & !is.na(d$pers_level),
        header = "\\textbf{Relationship-Seeking $\\times$ Personalisation}",
        desc = "Slope at each RS level within each personalisation condition"
      ),
      list(
        filter_fn = function(d) is.na(d$rs_level) & !is.na(d$domain_level) & !is.na(d$pers_level),
        header = "\\textbf{Domain $\\times$ Personalisation}",
        desc = "Slope within each domain-personalisation combination"
      )
    )
  } else {
    sections <- list(
      list(
        filter_fn = function(d) !is.na(d$rs_level) & !is.na(d$domain_level) & !is.na(d$pers_level),
        header = "\\textbf{Relationship-Seeking $\\times$ Domain $\\times$ Personalisation}",
        desc = "Slope at each RS level within each domain-personalisation combination"
      )
    )
  }

  first_section <- TRUE
  for (sec in sections) {
    sec_data <- slopes_data %>% filter(sec$filter_fn(.))
    if (nrow(sec_data) == 0) next

    if (!first_section) {
      lines <- c(lines, "\\midrule\\midrule")
    } else {
      lines <- c(lines, "\\midrule\\midrule")
      first_section <- FALSE
    }
    lines <- c(lines,
      "\\multicolumn{" %+% ncols %+% "}{l}{" %+% sec$header %+% "} \\\\",
      "\\multicolumn{" %+% ncols %+% "}{l}{\\footnotesize " %+% sec$desc %+% "} \\\\",
      "\\midrule\\midrule"
    )

    first_outcome <- TRUE
    for (out in outcomes) {
      out_data <- sec_data %>% filter(outcome == out)
      if (nrow(out_data) == 0) next

      if (!first_outcome) {
        lines <- c(lines, "\\addlinespace[0.5em]")
      }
      first_outcome <- FALSE

      for (i in seq_len(nrow(out_data))) {
        outcome_spec <- if (i == 1) list(label = out) else NULL
        lines <- c(lines, format_slope_row(out_data[i, ], outcome_spec, has_narrow_full))
      }
    }
  }

  lines <- c(lines, "\\end{longtable}", "\\normalsize")
  lines
}

#' Generate X5 table: Temporal Slopes
#'
#' @param contrasts Data frame of contrasts
#' @param family_name Name of measure family
#' @param time_prefix Time prefix
#' @return LaTeX lines or NULL
generate_table_X5 <- function(contrasts, family_name, time_prefix = "S") {
  cat("\n--- Generating Table X.5: Temporal Slopes ---\n")

  slopes <- contrasts %>% filter(test_type == "temporal_slope")

  if (nrow(slopes) == 0) {
    cat("  No temporal slope contrasts found, skipping.\n")
    return(NULL)
  }

  outcomes <- unique(slopes$outcome)
  lines <- c()

  # X5a: 1-Way Slopes
  slopes_1way <- slopes %>% filter(slope_type == "1way")
  if (nrow(slopes_1way) > 0) {
    lines <- c(lines, generate_slope_subtable(
      slopes_1way, family_name, "X5a", "1-Way",
      "Slopes by single treatment factor.", outcomes, has_narrow_full = TRUE
    ))
  }

  lines
}

# =============================================================================
# WORKFLOW FUNCTIONS
# =============================================================================

#' Write LaTeX table to file
#'
#' @param lines LaTeX lines
#' @param path Output file path
write_contrast_table <- function(lines, path) {
  writeLines(lines, path)
  cat("  Wrote:", path, "\n")
}

#' Process a measure family and generate all tables
#'
#' @param measure_family Name of measure family
#' @param contrasts_data Parsed contrast data (list from JSON)
#' @param table_dir Output directory for tables
#' @param time_prefix Time prefix ("S" or "W")
#' @param time_points Time point range
#' @param tables_to_include Which tables to generate (default all: X1, X2, X3, X4a, X4b, X5)
#' @param study_filter Filter to specific study type (NULL for all)
#' @param file_suffix Suffix to add to filenames (e.g., "_long", "_cs")
#' @return List of generated tables
process_contrast_measure_family <- function(measure_family, contrasts_data, table_dir,
                                            time_prefix = "S", time_points = c(1, 20),
                                            tables_to_include = c("X1", "X2", "X3", "X4a", "X4b", "X5"),
                                            study_filter = NULL,
                                            file_suffix = "") {
  suffix_label <- if (file_suffix != "") paste0(" (", file_suffix, ")") else ""
  cat("\nProcessing contrast tables for:", measure_family, suffix_label, "\n")

  # Convert JSON data to tibble
  contrasts <- bind_rows(lapply(contrasts_data$contrasts, function(x) {
    tibble(
      test = x$test,
      test_type = x$test_type,
      local_family = x$local_family %||% NA_character_,
      slope_type = x$slope_type %||% NA_character_,
      study_type = x$study_type,
      outcome = x$outcome,
      rs_level = x$rs_level %||% NA_character_,
      domain_level = x$domain_level %||% NA_character_,
      pers_level = x$pers_level %||% NA_character_,
      primary = list(x$primary),
      coarsened = list(x$coarsened),
      narrow = list(x$narrow),
      full = list(x$full)
    )
  }))

  # Filter by study type if specified
 if (!is.null(study_filter)) {
    contrasts <- contrasts %>% filter(study_type == study_filter)
  }

  cat("  Loaded", nrow(contrasts), "contrasts\n")

  if (nrow(contrasts) == 0) {
    cat("  No contrasts for this study type, skipping.\n")
    return(list())
  }

  all_tables <- list()

  # X1: Main Effects
  x1_tables <- generate_table_X1(contrasts, measure_family, file_suffix)
  for (effect_type in names(x1_tables)) {
    if (!is.null(x1_tables[[effect_type]])) {
      table_name <- paste0("X1_", effect_type)
      all_tables[[table_name]] <- x1_tables[[effect_type]]
      fname <- paste0(measure_family, "_", table_name, file_suffix, ".tex")
      write_contrast_table(x1_tables[[effect_type]], file.path(table_dir, fname))
    }
  }

  # X2: Dose Response
  if ("X2" %in% tables_to_include) {
    lines <- generate_table_X2(contrasts, measure_family, file_suffix)
    if (!is.null(lines)) {
      all_tables[["X2"]] <- lines
      fname <- paste0(measure_family, "_X2", file_suffix, ".tex")
      write_contrast_table(lines, file.path(table_dir, fname))
    }
  }

  # X3: Moderation
  if ("X3" %in% tables_to_include) {
    lines <- generate_table_X3(contrasts, measure_family)
    if (!is.null(lines)) {
      all_tables[["X3"]] <- lines
      fname <- paste0(measure_family, "_X3", file_suffix, ".tex")
      write_contrast_table(lines, file.path(table_dir, fname))
    }
  }

  # X4a: Temporal Dynamics
  if ("X4a" %in% tables_to_include) {
    lines <- generate_table_X4a(contrasts, measure_family, time_prefix, time_points)
    if (!is.null(lines)) {
      all_tables[["X4a"]] <- lines
      fname <- paste0(measure_family, "_X4a", file_suffix, ".tex")
      write_contrast_table(lines, file.path(table_dir, fname))
    }
  }

  # X4b: Time Coefficients
  if ("X4b" %in% tables_to_include) {
    lines <- generate_table_X4b(contrasts, measure_family, time_prefix)
    if (!is.null(lines)) {
      all_tables[["X4b"]] <- lines
      fname <- paste0(measure_family, "_X4b", file_suffix, ".tex")
      write_contrast_table(lines, file.path(table_dir, fname))
    }
  }

  # X5: Slopes
  if ("X5" %in% tables_to_include) {
    lines <- generate_table_X5(contrasts, measure_family, time_prefix)
    if (!is.null(lines)) {
      all_tables[["X5"]] <- lines
      fname <- paste0(measure_family, "_X5", file_suffix, ".tex")
      write_contrast_table(lines, file.path(table_dir, fname))
    }
  }

  skipped <- setdiff(c("X2", "X3", "X4a", "X4b", "X5"), tables_to_include)
  if (length(skipped) > 0) {
    cat("  (Skipping tables:", paste(skipped, collapse = ", "), ")\n")
  }

  return(all_tables)
}

#' Generate contrast coordinator file
#'
#' Creates a parent .tex file that includes all contrast tables.
#'
#' @param table_dir Directory containing the .tex table files
#' @param output_dir Directory for the coordinator file
#' @param table_subdir Path fragment for \\input{} calls
generate_contrast_coordinator <- function(table_dir, output_dir,
                                          table_subdir = "tables/main_studies/tex_tables") {
  output_file <- file.path(output_dir, "contrasts_parent.tex")

  # Find all contrast table files
  table_files <- list.files(table_dir, pattern = "\\.tex$", full.names = FALSE)

  # Define family order: preferences, attachment, wellbeing (both), perceptions
  family_order <- c("preferences", "attachment", "psychosocial",
                    "momentary_affect", "perceptions")
  # Get families present in files and filter to ordered list
  present_families <- unique(sub("_X[0-9].*\\.tex$", "", table_files))
  families <- family_order[family_order %in% present_families]

  family_labels <- c(
    preferences = "Preferences",
    attachment = "Attachment",
    perceptions = "Perceptions",
    psychosocial = "Psychosocial Wellbeing",
    momentary_affect = "Momentary Affect"
  )

  # Build preamble with explanatory sections
  lines <- c(
    "% Auto-generated contrast tables parent file",
    "% Generated by latex_contrast_table_utils.R",
    "",
    "% ---------------------------------------------------------------------------",
    "% Outcome Measure Families",
    "% ---------------------------------------------------------------------------",
    "",
    "\\subsubsection{Outcome Measure Families}",
    "\\label{sec:measure_families}",
    "",
    "Outcomes are organized into measure families for FDR correction.",
    "Each family contains one or more \\textit{local families} of related outcomes.",
    "Within tables, horizontal rules separate local families.",
    "",
    "\\begin{description}",
    "",
    "  \\item[Preferences] Single local family: likeability, engagingness, helpfulness.",
    "",
    "  \\item[Attachment] Three local families:",
    "  \\begin{itemize}",
    "    \\item \\textit{Attachment (Self-Reported)}: reliance, perceived understanding, self-disclosure, separation distress",
    "    \\item \\textit{Seeking Companionship}: seeking companionship likelihood",
    "    \\item \\textit{Goodbye}: goodbye action",
    "  \\end{itemize}",
    "",
    "  \\item[Psychosocial Wellbeing] Single local family: Emotional Health (F1), Social Health (F2).",
    "",
    "  \\item[Momentary Affect] Single local family: valence, arousal.",
    "",
    "  \\item[Perceptions] Two local families:",
    "  \\begin{itemize}",
    "    \\item \\textit{Relational}: tool-friend scale",
    "    \\item \\textit{Sentience}: perceived sentience, ontological sentience",
    "  \\end{itemize}",
    "",
    "\\end{description}",
    "",
    "% ---------------------------------------------------------------------------",
    "% FDR Correction Structure",
    "% ---------------------------------------------------------------------------",
    "",
    "\\subsubsection{FDR Correction Structure}",
    "\\label{sec:fdr_structure}",
    "",
    "FDR correction (Benjamini-Hochberg) is applied separately for each treatment effect within each measure family.",
    "Table~\\ref{tab:fdr_families} summarizes the FDR family structure.",
    "",
    "\\begin{table}[h]",
    "\\centering",
    "\\caption{FDR Family Structure for Main Effect Tests}",
    "\\label{tab:fdr_families}",
    "\\footnotesize",
    "\\begin{tabular}{lllc}",
    "\\toprule",
    "\\textbf{Test} & \\textbf{Treatment} & \\textbf{Outcomes} & \\textbf{N} \\\\",
    "\\midrule",
    "Does RS affect preferences? & Relationship-Seeking & Preferences & 3 \\\\",
    "Does Domain affect preferences? & Domain & Preferences & 3 \\\\",
    "Does Personalisation affect preferences? & Personalisation & Preferences & 3 \\\\",
    "\\midrule",
    "Does RS affect attachment? & Relationship-Seeking & Attachment (all) & 6 \\\\",
    "Does Domain affect attachment? & Domain & Attachment (all) & 6 \\\\",
    "Does Personalisation affect attachment? & Personalisation & Attachment (all) & 6 \\\\",
    "\\midrule",
    "Does RS affect psychosocial wellbeing? & Relationship-Seeking & Psychosocial F1, F2 & 2 \\\\",
    "Does Domain affect psychosocial wellbeing? & Domain & Psychosocial F1, F2 & 2 \\\\",
    "Does Personalisation affect psychosocial wellbeing? & Personalisation & Psychosocial F1, F2 & 2 \\\\",
    "\\midrule",
    "Does RS affect momentary affect? & Relationship-Seeking & Valence, Arousal & 2 \\\\",
    "Does Personalisation affect momentary affect? & Personalisation & Valence, Arousal & 2 \\\\",
    "\\midrule",
    "Does RS affect perceptions? & Relationship-Seeking & Perceptions (all) & 3 \\\\",
    "Does Domain affect perceptions? & Domain & Perceptions (all) & 3 \\\\",
    "Does Personalisation affect perceptions? & Personalisation & Perceptions (all) & 3 \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    "",
    "Three p-values are reported:",
    "\\begin{itemize}",
    "  \\item $p_{\\text{raw}}$: Uncorrected p-value",
    "  \\item $p_{\\text{local}}$: FDR-corrected within local family",
    "  \\item $p_{\\text{global}}$: FDR-corrected across measure family (used for inference)",
    "\\end{itemize}",
    "",
    "% ---------------------------------------------------------------------------",
    "% Table Structure",
    "% ---------------------------------------------------------------------------",
    "",
    "\\subsubsection{Table Structure}",
    "\\label{sec:table_structure}",
    "",
    "For each measure family:",
    "",
    "\\begin{description}",
    "  \\item[Table X1\\_RS] Relationship-seeking main effects: avg($\\lambda > 0$) $-$ avg($\\lambda < 0$)",
    "  \\item[Table X1\\_Domain] Domain main effects: emotional chat $-$ political chat",
    "  \\item[Table X1\\_Pers] Personalisation main effects: personalised $-$ non-personalised",
    "  \\item[Table X.2] Dose-response: polynomial terms ($\\lambda$, $\\lambda^2$, $\\lambda^3$)",
    "  \\item[Table X.3] Moderation: RS $\\times$ Domain and RS $\\times$ Personalisation interactions",
    "  \\item[Table X.4a] Temporal dynamics: time effects and treatment $\\times$ time interactions",
    "  \\item[Table X.4b] Time coefficients: rate of change per time unit",
    "  \\item[Table X.5] Temporal slopes: estimated change per time unit by condition",
    "\\end{description}",
    "",
    "% ===========================================================================",
    "% LONGITUDINAL STUDY - ALL CONTRAST TABLES",
    "% ===========================================================================",
    "",
    "\\subsubsection{Longitudinal Study}",
    "\\label{sec:longitudinal_contrasts}",
    ""
  )

  # Custom sort: X1_RS, X1_Domain, X1_Pers, then X2, X3, X4a, X4b, X5
  sort_contrast_files <- function(files) {
    get_sort_key <- function(f) {
      if (grepl("X1_RS", f)) return("1_RS")
      if (grepl("X1_Domain", f)) return("2_Domain")
      if (grepl("X1_Pers", f)) return("3_Pers")
      if (grepl("X2", f)) return("4_X2")
      if (grepl("X3", f)) return("5_X3")
      if (grepl("X4a", f)) return("6_X4a")
      if (grepl("X4b", f)) return("7_X4b")
      if (grepl("X5", f)) return("8_X5")
      return("9_other")
    }
    files[order(sapply(files, get_sort_key))]
  }

  # Add longitudinal tables for each family
  for (family in families) {
    family_label <- if (family %in% names(family_labels)) {
      family_labels[family]
    } else {
      tools::toTitleCase(gsub("_", " ", family))
    }

    lines <- c(lines, paste0("\\paragraph{", family_label, "}"), "")

    # Get longitudinal tables for this family (X1-X5)
    long_files <- table_files[grepl(paste0("^", family, "_.*_long\\.tex$"), table_files)]
    long_files <- sort_contrast_files(long_files)

    for (tf in long_files) {
      lines <- c(lines, paste0("\\input{", table_subdir, "/", sub("\\.tex$", "", tf), "}"), "")
    }
  }

  # Cross-sectional section
  lines <- c(lines,
    "% ===========================================================================",
    "% CROSS-SECTIONAL STUDY - MAIN EFFECTS AND DOSE-RESPONSE",
    "% ===========================================================================",
    "",
    "\\subsubsection{Cross-Sectional Study}",
    "\\label{sec:cross_sectional_contrasts}",
    "",
    paste0("For the cross-sectional study, main effect (X1) and dose-response ",
           "(X2) tables are generated."),
    ""
  )

  # Add cross-sectional tables for each family (X1 and X2)
  for (family in families) {
    family_label <- if (family %in% names(family_labels)) {
      family_labels[family]
    } else {
      tools::toTitleCase(gsub("_", " ", family))
    }

    # Get CS tables for this family (X1 and X2)
    cs_files <- table_files[grepl(paste0("^", family, "_(X1|X2).*_cs\\.tex$"), table_files)]
    cs_files <- sort_contrast_files(cs_files)

    if (length(cs_files) > 0) {
      lines <- c(lines, paste0("\\paragraph{", family_label, "}"), "")
      for (tf in cs_files) {
        lines <- c(lines, paste0("\\input{", table_subdir, "/", sub("\\.tex$", "", tf), "}"), "")
      }
      lines <- c(lines, "")
    }
  }

  writeLines(lines, output_file)
  cat("Generated contrast parent:", output_file, "\n")
}
