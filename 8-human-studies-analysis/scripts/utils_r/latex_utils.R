# =============================================================================
# LaTeX Utilities
# =============================================================================

# NOTE: Contrast-specific table generation utilities (for statistical contrast
#       tables X1-X5) are in latex_contrast_table_utils.R, which sources this
#       file for shared utilities
#
# Dependencies:
#   - labelling_utils.R (for format_outcome_label, study_type_caption,
#                        clean_contrast_varnames)
#
# Usage:
#   source("scripts/utils_r/labelling_utils.R")  # Must source first
#   source("scripts/utils_r/latex_utils.R")
# =============================================================================

library(dplyr)

# =============================================================================
# HELPER OPERATORS
# =============================================================================

# String concatenation operator for cleaner code
`%+%` <- function(a, b) paste0(a, b)

#' Wrap LaTeX content in footnotesize
#'
#' @param content LaTeX string or kable object to wrap
#' @return Character string wrapped in {\footnotesize ... }
wrap_footnotesize <- function(content) {
  paste0("{\\footnotesize\n", as.character(content), "\n}")
}

#' Create balance table LaTeX output
#'
#' @param balance_table Data frame with balance test results
#' @param pred_labels Named vector of predictor labels
#' @param study_name Study name for caption
#' @param table_label LaTeX label for the table
#' @return LaTeX string wrapped in footnotesize
format_balance_table_latex <- function(balance_table, pred_labels, study_name,
                                        table_label) {
  # Helper to format p-value with NA handling

  fmt_p <- function(p) {
    ifelse(is.na(p), "---",
      ifelse(p < 0.05,
        paste0("\\textbf{", sprintf("%.2f", p), "}"),
        sprintf("%.2f", p)))
  }

  formatted <- balance_table %>%
    mutate(
      predictor = ifelse(predictor %in% names(pred_labels),
                        pred_labels[predictor], predictor),
      domain_p = fmt_p(domain_p),
      personalisation_p = fmt_p(personalisation_p),
      multiplier_p = fmt_p(multiplier_p)
    )

  # Check if any Fisher simulation was used
  has_fisher_sim <- any(grepl("Fisher \\(sim\\)", balance_table$test_type))
  caption_note <- if (has_fisher_sim) {
    paste0(" Fisher (sim) indicates Fisher's test with Monte Carlo",
           " simulation (2000 replicates) used where exact computation",
           " was infeasible.")
  } else {
    ""
  }

  knitr::kable(formatted, format = "latex", escape = FALSE,
        col.names = c("Predictor", "Test", "Domain p", "Pers. p", "Mult. p"),
        caption = paste0(study_name,
                        ": Baseline covariate balance (FDR-adjusted).",
                        caption_note),
        label = table_label,
        booktabs = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>%
    wrap_footnotesize()
}

#' Create differential attrition table LaTeX output
#'
#' @param attrition_table Data frame with attrition by treatment arm
#' @param study_name Study name for caption
#' @param table_label LaTeX label for the table
#' @return LaTeX string wrapped in footnotesize
format_diff_attrition_table_latex <- function(attrition_table, study_name,
                                               table_label) {
  formatted <- attrition_table %>%
    mutate(
      attrition_rate_pct = paste0(sprintf("%.1f", dropout_rate * 100), "%"),
      fisher_p_fmt = sprintf("%.3f", fisher_p),
      arm_clean = case_when(
        treatment_arm == "personalisation" ~ "Personalisation",
        treatment_arm == "domain" ~ "Domain",
        treatment_arm == "multiplier" ~ "Multiplier",
        TRUE ~ treatment_arm
      )
    ) %>%
    mutate(sort_key = case_when(
      arm_clean == "Multiplier" ~ match(
        treatment_level,
        c("neg1", "neg0.5", "zero", "pos0.5", "pos1")
      ),
      TRUE ~ 0
    )) %>%
    arrange(arm_clean, sort_key) %>%
    select(-sort_key) %>%
    group_by(arm_clean) %>%
    mutate(
      arm_display = ifelse(row_number() == 1, arm_clean, ""),
      p_display = ifelse(row_number() == 1, fisher_p_fmt, "")
    ) %>%
    ungroup() %>%
    select(arm_display, p_display, treatment_level,
          n_baseline, n_dropouts, attrition_rate_pct)

  knitr::kable(formatted, format = "latex", escape = TRUE,
        col.names = c("Arm", "Fisher's p", "Level",
                      "Baseline N", "Attrition", "Rate"),
        caption = paste0(study_name,
                        ": Attrition rates by treatment arm (FDR-adjusted)"),
        label = table_label,
        booktabs = TRUE, linesep = "") %>%
    kableExtra::kable_styling(latex_options = c("striped", "hold_position")) %>%
    wrap_footnotesize()
}

#' Get the tex_tables subdirectory for individual table files
#'
#' Returns the path to the tex_tables subdirectory, creating it if necessary.
#' This ensures all individual table files go to table_dir/tex_tables/
#'
#' @param table_dir Base table directory (e.g., outputs/tables/main_studies)
#' @return Path to tex_tables subdirectory
get_tex_tables_dir <- function(table_dir) {
  tex_tables_dir <- file.path(table_dir, "tex_tables")
  dir.create(tex_tables_dir, recursive = TRUE, showWarnings = FALSE)
  return(tex_tables_dir)
}

# =============================================================================
# VALUE FORMATTING (from latex_table_utils.R)
# =============================================================================

#' Format estimate with confidence interval
#'
#' @param est Numeric estimate
#' @param lower Lower confidence bound
#' @param upper Upper confidence bound
#' @param digits Number of decimal places (default 2)
#' @return Character string "Est [lower, upper]"
format_est_ci <- function(est, lower, upper, digits = 2) {
  if (is.na(est)) return("")
  fmt <- paste0("%.", digits, "f")
  sprintf(paste0(fmt, " [", fmt, ", ", fmt, "]"),
          est, lower, upper)
}

#' Format p-value with optional bolding for significance
#'
#' @param p Numeric p-value
#' @param threshold Significance threshold (default 0.05)
#' @param bold_sig Whether to bold significant values (default TRUE)
#' @return Character string formatted p-value
format_p_value <- function(p, threshold = 0.05, bold_sig = TRUE) {
  if (is.na(p)) return("---")

  if (p < 0.001) {
    formatted <- "$<$.001"
  } else {
    formatted <- sprintf("%.3f", p)
  }

  if (bold_sig && p < threshold) {
    formatted <- paste0("\\textbf{", formatted, "}")
  }

  return(formatted)
}

#' Get significance stars
#'
#' @param p Numeric p-value
#' @return Character string with stars
get_sig_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("")
}

#' Escape special LaTeX characters in text
#'
#' @param x Character vector
#' @return Character vector with escaped LaTeX characters
escape_latex_text <- function(x) {
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x <- gsub("\u03bb", "$\\lambda$", x, fixed = TRUE)  # lambda
  x <- gsub("\u00b2", "$^2$", x, fixed = TRUE)  # superscript 2
  x <- gsub("\u00b3", "$^3$", x, fixed = TRUE)  # superscript 3
  x <- gsub("\u00d7", "$\\times$", x, fixed = TRUE)  # multiplication
  x <- gsub("\u0394", "$\\Delta$", x, fixed = TRUE)  # Delta
  x <- gsub("\u00b1", "$\\pm$", x, fixed = TRUE)  # plus-minus
  x
}

#' Convert text with Unicode symbols to LaTeX
#'
#' @param text Character string
#' @return Character string with LaTeX math symbols
to_latex_text <- function(text) {
  if (is.null(text)) return(NULL)
  # Escape braces first (set notation {0.5, 1.0} -> \{0.5, 1.0\})
  text <- gsub("{", "\\{", text, fixed = TRUE)
  text <- gsub("}", "\\}", text, fixed = TRUE)
  # Then replace Unicode symbols with LaTeX math
  text <- gsub("\u03bb", "$\\lambda$", text, fixed = TRUE)
  text <- gsub("\u00b1", "$\\pm$", text, fixed = TRUE)
  text <- gsub("\u00d7", "$\\times$", text, fixed = TRUE)
  text <- gsub("\u2208", "$\\in$", text, fixed = TRUE)
  text
}

#' Format coefficient with SE and p-value for robustness tables
#'
#' @param coeff Coefficient estimate
#' @param se Standard error
#' @param p_val P-value
#' @return Formatted string with bolding for significant
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

#' Format estimate with p-value for contrast robustness tables
#'
#' @param est Estimate
#' @param p P-value
#' @return Formatted string
format_est_p <- function(est, p) {
  if (is.na(est)) return("---")
  stars <- get_sig_stars(p)
  p_fmt <- if (p < 0.001) "$<$.001" else sprintf("%.3f", p)
  sprintf("%.2f (%s)%s", est, p_fmt, stars)
}

# =============================================================================
# PARENT TEX FILE GENERATION
# =============================================================================

#' Generate a parent .tex file that inputs all tables for one analysis
#'
#' Writes a .tex fragment with a descriptive blurb (using \\cref)
#' followed by \\input{} for every table, organised into subsubsections:
#' Heterogeneity (optional) -> Performance Comparisons ->
#' Regression Outputs -> Contrast Tests -> Robustness Checks.
#'
#' @param section_name  Display name for the section
#' @param constructs    Character vector of construct keys
#' @param construct_labels Named vector mapping keys to display names
#' @param func_form_prefix  Label/filename prefix for functional form
#' @param main_reg_prefix   Label/filename prefix for main regressions
#' @param full_spec_prefix  Label/filename prefix for full-spec tables
#' @param robustness_prefix Label/filename prefix for robustness tables
#' @param heterogeneity_prefix Label/filename prefix for heterogeneity tests
#' @param postonly_prefix   Label/filename prefix for post-only supplementary tables
#' @param main_includes_cs  Whether main analysis includes cross-sectional (default TRUE)
#' @param table_subdir  Path fragment used inside \\input{} calls
#' @param table_dir     Absolute path to the output directory
#' @param output_filename  Filename (no extension) for the parent file
generate_parent_tex <- function(section_name,
                                constructs,
                                construct_labels,
                                func_form_prefix,
                                main_reg_prefix,
                                full_spec_prefix,
                                robustness_prefix = NULL,
                                heterogeneity_prefix = NULL,
                                postonly_prefix = NULL,
                                combined_study_prefix = NULL,
                                ancova_prefix = NULL,
                                main_includes_cs = TRUE,
                                constructs_with_cs = NULL,
                                table_subdir = "tables/main_studies/tex_tables",
                                table_dir,
                                output_filename) {
  # constructs_with_cs: optional vector of construct names that have CS versions.

  # If NULL, uses main_includes_cs for all constructs.

  # Create subdirectories (table_coordinators for parent, tex_tables for individual tables)
  coordinator_dir <- file.path(table_dir, "table_coordinators")
  tables_dir <- file.path(table_dir, "tex_tables")
  dir.create(coordinator_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Build label refs ---
  # Combined tables have single labels (no _cs/_long suffix)
  base_prefix <- sub("_functional_form$", "", func_form_prefix)
  ff_combined <- paste0("tab:", base_prefix, "_functional_form")
  fs_combined <- paste0("tab:", base_prefix, "_full_spec")

  main_refs <- c()
  for (cn in constructs) {
    long_lab <- paste0("tab:", main_reg_prefix, "_", cn, "_long")
    label <- construct_labels[[cn]]
    # Check if this construct has CS version
    has_cs <- if (!is.null(constructs_with_cs)) {
      cn %in% constructs_with_cs
    } else {
      main_includes_cs
    }
    if (has_cs) {
      cs_lab <- paste0("tab:", main_reg_prefix, "_", cn, "_cs")
      main_refs <- c(main_refs, sprintf(
        "\\cref{%s,%s} (%s)", cs_lab, long_lab, label
      ))
    } else {
      main_refs <- c(main_refs, sprintf(
        "\\cref{%s} (%s, longitudinal only)", long_lab, label
      ))
    }
  }
  main_refs_str <- paste(main_refs, collapse = ",\n")

  if (!is.null(robustness_prefix)) {
    rob_long <- paste0("tab:", robustness_prefix, "_long")
    if (main_includes_cs) {
      rob_cs <- paste0("tab:", robustness_prefix, "_cs")
      rob_refs <- paste0("\\cref{", rob_cs, ",", rob_long, "}")
    } else {
      rob_refs <- paste0("\\cref{", rob_long, "}")
    }
  } else {
    rob_refs <- NULL
  }

  # --- Blurb ---
  blurb <- paste0(
    if (!is.null(heterogeneity_prefix)) paste0(
      "Heterogeneity tests assessing whether outcomes within pooled ",
      "constructs show differential treatment effects are in ",
      "\\cref{tab:", heterogeneity_prefix, "}.\n"
    ) else "",
    "Functional form comparisons selecting between linear, ",
    "quadratic, and cubic $\\lambda$ specifications (by AIC) ",
    "are in\n",
    "\\cref{", ff_combined, "}.\n",
    "Main regression outputs for each construct are in\n",
    main_refs_str, ".\n",
    "Each table presents six model specifications: additive ",
    "and full interaction models for continuous, coarsened, ",
    "and factor $\\lambda$.\n",
    "Performance comparisons across the three full interaction ",
    "specifications are in\n",
    "\\cref{", fs_combined, "}.",
    if (!is.null(rob_refs)) paste0(
      "\nRobustness checks examining coefficient stability with ",
      "demographic, preference, and IPW controls are in\n",
      rob_refs, "."
    ) else "",
    if (!is.null(ancova_prefix)) paste0(
      "\nANCOVA results examining domain $\\times$ study interactions ",
      "are in \\cref{tab:", ancova_prefix, "}."
    ) else "",
    if (!is.null(combined_study_prefix)) paste0(
      "\nCombined study analysis comparing cross-sectional and ",
      "longitudinal effects is in \\cref{tab:", combined_study_prefix, "}."
    ) else "",
    "\n",
    if (!is.null(postonly_prefix)) paste0(
      "Only the longitudinal study includes pre-treatment measurement, ",
      "so the main analysis controls for baseline. ",
      "Post-only analysis (without baseline control) including ",
      "the cross-sectional study is also provided as a robustness check."
    ) else ""
  )

  # --- Input lines (with subsubsection structure) ---
  inp <- function(f) {
    paste0("\\input{", table_subdir, "/", f, "}")
  }

  input_lines <- c()

  # Heterogeneity tests (if applicable)
  if (!is.null(heterogeneity_prefix)) {
    input_lines <- c(input_lines,
      "% --- Heterogeneity tests ---",
      inp(heterogeneity_prefix),
      ""
    )
  }

  # Performance Comparisons (functional form + full-spec) - combined side-by-side
  input_lines <- c(input_lines,
    "% --- Functional form comparison (side-by-side) ---",
    inp(paste0(base_prefix, "_functional_form")),
    "",
    "% --- Full-specification performance comparison (side-by-side) ---",
    inp(paste0(base_prefix, "_full_spec")),
    ""
  )

  # Regression Outputs (main regression tables per construct)
  input_lines <- c(input_lines,
    "% --- Main regression tables (per construct) ---"
  )
  for (cn in constructs) {
    # Check if this construct has CS version
    has_cs <- if (!is.null(constructs_with_cs)) {
      cn %in% constructs_with_cs
    } else {
      main_includes_cs
    }
    if (has_cs) {
      input_lines <- c(input_lines,
        inp(paste0(main_reg_prefix, "_", cn, "_cs")),
        inp(paste0(main_reg_prefix, "_", cn, "_long")),
        ""
      )
    } else {
      input_lines <- c(input_lines,
        paste0("% ", construct_labels[[cn]], " is longitudinal-only"),
        inp(paste0(main_reg_prefix, "_", cn, "_long")),
        ""
      )
    }
  }

  # Robustness Checks (if applicable)
  if (!is.null(robustness_prefix)) {
    input_lines <- c(input_lines,
      "% --- Robustness checks ---"
    )
    if (main_includes_cs) {
      input_lines <- c(input_lines,
        inp(paste0(robustness_prefix, "_cs")),
        inp(paste0(robustness_prefix, "_long"))
      )
    } else {
      input_lines <- c(input_lines,
        inp(paste0(robustness_prefix, "_long"))
      )
    }
  }

  # ANCOVA (if applicable)
  if (!is.null(ancova_prefix)) {
    input_lines <- c(input_lines,
      "",
      "% --- ANCOVA ---",
      inp(ancova_prefix)
    )
  }

  # Combined Study Analysis (if applicable)
  if (!is.null(combined_study_prefix)) {
    input_lines <- c(input_lines,
      "",
      "% --- Combined study analysis (cross-sectional vs longitudinal) ---",
      inp(combined_study_prefix)
    )
  }

  # Post-Only Supplementary Section (if applicable)
  if (!is.null(postonly_prefix)) {
    input_lines <- c(input_lines,
      "",
      "% --- Post-only analysis (without baseline control) ---",
      inp(paste0(postonly_prefix, "_functional_form")),
      inp(paste0(postonly_prefix, "_full_spec"))
    )
    for (cn in constructs) {
      input_lines <- c(input_lines,
        inp(paste0(postonly_prefix, "_", cn, "_cs")),
        inp(paste0(postonly_prefix, "_", cn, "_long"))
      )
    }
    input_lines <- c(input_lines,
      inp(paste0(postonly_prefix, "_robustness_cs"))
    )
  }

  # --- Assemble full file ---
  header <- ""

  content <- paste(c(
    header,
    "",
    blurb,
    "",
    paste(input_lines, collapse = "\n")
  ), collapse = "\n")

  # --- Write (to table_coordinators subdirectory) ---
  out_path <- file.path(
    coordinator_dir, paste0(output_filename, ".tex"))
  writeLines(content, out_path)
  cat(sprintf("Wrote parent tex: %s\n", out_path))
  invisible(out_path)
}

# =============================================================================
# Regression Table Generation
# =============================================================================

#' Generate all regression tables for a set of outcomes
#'
#' Detect model family from a model object
#'
#' Returns a human-readable label for the model type:
#' - "Ordinary Least Squares" for lm
#' - "Linear Mixed Effects Model" for lmer
#' - "Logistic Regression" for glm(binomial)
#' - "Generalized Linear Mixed Model" for glmer
#'
#' @param model A fitted model object
#' @return Character string describing the model family
detect_model_family <- function(model) {

  if (is.null(model)) return("Unknown")

  if (inherits(model, "lmerMod")) {
    return("Linear Mixed Effects Model")
  } else if (inherits(model, "glmerMod")) {
    return("Generalized Linear Mixed Model")
  } else if (inherits(model, "glm")) {
    fam <- family(model)$family
    if (fam == "binomial") {
      return("Logistic Regression")
    } else if (fam == "gaussian") {
      return("Ordinary Least Squares")
    } else {
      return(paste0("GLM (", fam, ")"))
    }
  } else if (inherits(model, "lm")) {
    return("Ordinary Least Squares")
  } else {
    return(class(model)[1])
  }
}

#' Creates LaTeX tables for each outcome × study type combination using sjplot_to_latex.
#' Each table has 6 columns: additive/full for continuous, coarsened, and factor lambda.
#'
#' @param mods_cs List of cross-sectional models (one per outcome)
#' @param mods_long List of longitudinal models (one per outcome)
#' @param outcome_vars Character vector of outcome names
#' @param best_specs_cs Data frame with outcome, best_spec columns for CS
#' @param best_specs_long Data frame with outcome, best_spec columns for Long
#' @param table_prefix Prefix for filenames (e.g., "preferences")
#' @param table_dir Output directory
#' @param pred_labels Named vector of predictor labels for sjPlot
#' @param reference_outcomes Named list mapping construct names to reference outcome labels
#'   (for pooled models). E.g., list(reliance = "Behavioural Reliance")
generate_regression_tables <- function(mods_cs, mods_long, outcome_vars,
                                       best_specs_cs, best_specs_long,
                                       table_prefix, table_dir, pred_labels,
                                       reference_outcomes = NULL) {
  n_tables <- 0
  for (outcome in outcome_vars) {
    outcome_title <- format_outcome_label(outcome)

    for (study in c("cs", "long")) {
      # Skip if models for this study type are NULL
      mods_list <- if (study == "cs") mods_cs else mods_long
      if (is.null(mods_list)) next

      mods <- mods_list[[outcome]]
      if (is.null(mods)) next

      specs <- if (study == "cs") best_specs_cs else best_specs_long
      spec_match <- specs$best_spec[specs$outcome == outcome]
      if (length(spec_match) == 0) {
        stop(sprintf("Outcome '%s' not found in best_specs_%s", outcome, study))
      }
      spec_label <- tools::toTitleCase(spec_match[1])
      study_label <- if (study == "cs") "cross_sectional" else "longitudinal"

      model_list <- list(
        mods$additive_continuous, mods$full_continuous,
        mods$additive_coarsened, mods$full_coarsened,
        mods$additive_5level, mods$full_5level
      )

      # Detect model family from first non-null model
      model_family <- "Unknown"
      for (m in model_list) {
        if (!is.null(m)) {
          model_family <- detect_model_family(m)
          break
        }
      }

      # Build caption with optional reference outcome for pooled models
      base_caption <- sprintf("%s --- %s, %s (Best continuous specification: %s)",
                              outcome_title, study_type_caption(study_label),
                              model_family, spec_label)
      if (!is.null(reference_outcomes) && outcome %in% names(reference_outcomes)) {
        ref_label <- reference_outcomes[[outcome]]
        caption <- sprintf("%s. Reference outcome: \\textit{%s}.", base_caption, ref_label)
      } else {
        caption <- base_caption
      }
      # Add abbreviations note
      caption <- paste0(caption, ". Emot.\\ = EmotChat; Pers.\\ = Personalised.")

      sjplot_to_latex(
        models = model_list,
        model_labels = rep(c("Additive", "Full"), 3),
        pred_labels = pred_labels,
        filename = paste0(table_prefix, "_", outcome, "_", study),
        table_dir = table_dir,
        caption = caption,
        dependent_var = outcome_title,
        column_groups = list(
          continuous = list(label = "$\\lambda$ (Continuous)", cols = 1:2),
          coarsened = list(label = "$\\lambda_3$ (Coarsened)", cols = 3:4),
          fiveLevel = list(label = "$\\lambda_5$ (Factor)", cols = 5:6)
        )
      )
      n_tables <- n_tables + 1
    }
  }
  cat(sprintf("Generated %d regression tables\n", n_tables))
}

#' Create LaTeX tabular content (no table wrapper, for embedding)
#'
#' @param perf_list Named list of performance data frames
#' @param construct_labels Optional named vector of display labels
#' @param bold_best Whether to bold the best model
#' @param show_cols Columns to display
#' @param clean_model_names Named vector for model name cleaning
#' @return LaTeX tabular lines (without table wrapper)
create_performance_tabular <- function(
    perf_list,
    construct_labels = NULL,
    bold_best = TRUE,
    show_cols = c("RMSE", "AIC_wt", "AICc_wt", "BIC_wt", "Performance_Score"),
    clean_model_names = NULL) {

  # Return NULL if perf_list is NULL or empty

  if (is.null(perf_list) || length(perf_list) == 0) {
    return(NULL)
  }

  if (is.null(construct_labels)) {
    construct_labels <- setNames(
      gsub("_", " ", tools::toTitleCase(names(perf_list))),
      names(perf_list))
  }

  if (is.null(clean_model_names)) {
    clean_model_names <- c(
      "full_continuous" = "$\\lambda$ (Continuous)",
      "full_coarsened" = "$\\lambda_3$ (Coarsened)",
      "full_5level" = "$\\lambda_5$ (Factor)",
      "linear" = "Linear",
      "quadratic" = "Quadratic",
      "cubic" = "Cubic"
    )
  }

  col_headers <- c("Model")
  for (col in show_cols) {
    header <- switch(col,
      "RMSE" = "RMSE",
      "AIC_wt" = "$w_{AIC}$",
      "AICc_wt" = "$w_{AICc}$",
      "BIC_wt" = "$w_{BIC}$",
      "Performance_Score" = "Perf.",
      "AIC" = "AIC",
      "AICc" = "AICc",
      "BIC" = "BIC",
      gsub("_", " ", col)
    )
    col_headers <- c(col_headers, header)
  }

  n_cols <- length(col_headers)
  col_align <- paste0("l", paste(rep("r", n_cols - 1), collapse = ""))
  header_row <- paste(col_headers, collapse = " & ")

  latex <- c(
    "\\footnotesize",
    paste0("\\begin{tabular}{", col_align, "}"),
    "\\toprule",
    paste0(header_row, " \\\\"),
    "\\midrule"
  )

  construct_names <- names(perf_list)
  for (ci in seq_along(construct_names)) {
    cn <- construct_names[ci]
    cn_label <- if (cn %in% names(construct_labels)) {
      construct_labels[[cn]]
    } else {
      cn
    }
    perf_df <- perf_list[[cn]]

    latex <- c(latex, paste0(
      "\\multicolumn{", n_cols, "}{c}{\\textbf{", cn_label, "}} \\\\"))

    best_idx <- if ("Performance_Score" %in% names(perf_df)) {
      which.max(perf_df$Performance_Score)
    } else if ("AIC_wt" %in% names(perf_df)) {
      which.max(perf_df$AIC_wt)
    } else {
      1
    }

    for (i in seq_len(nrow(perf_df))) {
      model_name <- perf_df$Name[i]
      if (model_name %in% names(clean_model_names)) {
        model_name <- clean_model_names[[model_name]]
      } else {
        model_name <- gsub("_", " ", tools::toTitleCase(model_name))
      }

      row_cells <- c(model_name)
      for (col in show_cols) {
        val <- if (col %in% names(perf_df)) perf_df[[col]][i] else NA
        if (is.na(val)) {
          row_cells <- c(row_cells, "---")
        } else {
          row_cells <- c(row_cells, sprintf("%.3f", val))
        }
      }

      if (bold_best && i == best_idx) {
        row_cells <- sapply(row_cells, function(x) paste0("\\textbf{", x, "}"))
      }
      latex <- c(latex, paste0(paste(row_cells, collapse = " & "), " \\\\"))
    }

    if (ci < length(construct_names)) {
      latex <- c(latex, "\\midrule")
    }
  }

  latex <- c(latex, "\\bottomrule", "\\end{tabular}", "\\normalsize")
  latex
}

#' Generate functional form comparison tables (side-by-side)
#'
#' @param perf_cs List of cross-sectional performance comparisons
#' @param perf_long List of longitudinal performance comparisons
#' @param table_prefix Prefix for filenames
#' @param table_dir Output directory
#' @param task_name Display name (e.g., "Preferences")
generate_functional_form_tables <- function(perf_cs, perf_long,
                                            table_prefix, table_dir, task_name) {
  # Create individual tabular content files
  cs_tabular <- create_performance_tabular(perf_list = perf_cs)
  long_tabular <- create_performance_tabular(perf_list = perf_long)

  # Write to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)

  cs_file <- paste0(table_prefix, "_functional_form_cs")
  long_file <- paste0(table_prefix, "_functional_form_long")

  # Write individual files only if they have content
  if (!is.null(cs_tabular)) {
    writeLines(cs_tabular, file.path(tex_tables_dir, paste0(cs_file, ".tex")))
  }
  if (!is.null(long_tabular)) {
    writeLines(long_tabular, file.path(tex_tables_dir, paste0(long_file, ".tex")))
  }

  # Create combined side-by-side wrapper
  caption_note <- paste0(
    "Functional Form Comparison --- ", task_name,
    ". Best model per construct highlighted in bold. ",
    "$w_{AIC}$, $w_{AICc}$, $w_{BIC}$ = Akaike/BIC weights."
  )

  # Build combined table based on what's available (caption ABOVE tables)
  if (!is.null(cs_tabular) && !is.null(long_tabular)) {
    # Both CS and Long available - side by side
    combined <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption_note, "}"),
      paste0("\\label{tab:", table_prefix, "_functional_form}"),
      "\\begin{minipage}[t]{0.48\\textwidth}",
      "\\centering",
      paste0("\\input{tables/main_studies/tex_tables/", cs_file, "}"),
      paste0("\\subcaption{", study_type_caption("cross_sectional"), "}"),
      "\\end{minipage}",
      "\\hfill",
      "\\begin{minipage}[t]{0.48\\textwidth}",
      "\\centering",
      paste0("\\input{tables/main_studies/tex_tables/", long_file, "}"),
      paste0("\\subcaption{", study_type_caption("longitudinal"), "}"),
      "\\end{minipage}",
      "\\end{table}"
    )
  } else if (!is.null(long_tabular)) {
    # Only Long available
    combined <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption_note, "}"),
      paste0("\\label{tab:", table_prefix, "_functional_form}"),
      paste0("\\input{tables/main_studies/tex_tables/", long_file, "}"),
      "\\end{table}"
    )
  } else if (!is.null(cs_tabular)) {
    # Only CS available
    combined <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption_note, "}"),
      paste0("\\label{tab:", table_prefix, "_functional_form}"),
      paste0("\\input{tables/main_studies/tex_tables/", cs_file, "}"),
      "\\end{table}"
    )
  } else {
    warning("No performance data available for functional form tables")
    return(invisible(NULL))
  }

  output_path <- file.path(tex_tables_dir, paste0(table_prefix, "_functional_form.tex"))
  writeLines(combined, output_path)
  cat("Saved functional form tables to:", output_path, "\n")
}

#' Generate full-spec comparison tables (side-by-side)
#'
#' @param mods_cs List of cross-sectional models
#' @param full_spec_cs Pre-computed cross-sectional comparison (list by outcome)
#' @param full_spec_long Pre-computed longitudinal comparison (list by outcome)
#' @param table_prefix Prefix for filenames
#' @param table_dir Output directory
#' @param task_name Display name
#' @return NULL (writes files)
generate_full_spec_tables <- function(full_spec_cs, full_spec_long,
                                      table_prefix, table_dir, task_name) {
  # Create individual tabular content files
  cs_tabular <- create_performance_tabular(perf_list = full_spec_cs)
  long_tabular <- create_performance_tabular(perf_list = full_spec_long)

  # Write to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)

  cs_file <- paste0(table_prefix, "_full_spec_cs")
  long_file <- paste0(table_prefix, "_full_spec_long")

  # Write individual files only if they have content
  if (!is.null(cs_tabular)) {
    writeLines(cs_tabular, file.path(tex_tables_dir, paste0(cs_file, ".tex")))
  }
  if (!is.null(long_tabular)) {
    writeLines(long_tabular, file.path(tex_tables_dir, paste0(long_file, ".tex")))
  }

  # Create combined side-by-side wrapper
  caption_note <- paste0(
    "Full Model Specification Comparison --- ", task_name,
    ". Best model per construct highlighted in bold. ",
    "$w_{AIC}$, $w_{AICc}$, $w_{BIC}$ = Akaike/BIC weights."
  )

  # Build combined table based on what's available (caption ABOVE tables)
  if (!is.null(cs_tabular) && !is.null(long_tabular)) {
    # Both CS and Long available - side by side
    combined <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption_note, "}"),
      paste0("\\label{tab:", table_prefix, "_full_spec}"),
      "\\begin{minipage}[t]{0.48\\textwidth}",
      "\\centering",
      paste0("\\input{tables/main_studies/tex_tables/", cs_file, "}"),
      paste0("\\subcaption{", study_type_caption("cross_sectional"), "}"),
      "\\end{minipage}",
      "\\hfill",
      "\\begin{minipage}[t]{0.48\\textwidth}",
      "\\centering",
      paste0("\\input{tables/main_studies/tex_tables/", long_file, "}"),
      paste0("\\subcaption{", study_type_caption("longitudinal"), "}"),
      "\\end{minipage}",
      "\\end{table}"
    )
  } else if (!is.null(long_tabular)) {
    # Only Long available
    combined <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption_note, "}"),
      paste0("\\label{tab:", table_prefix, "_full_spec}"),
      paste0("\\input{tables/main_studies/tex_tables/", long_file, "}"),
      "\\end{table}"
    )
  } else if (!is.null(cs_tabular)) {
    # Only CS available
    combined <- c(
      "\\begin{table}[htbp]",
      "\\centering",
      paste0("\\caption{", caption_note, "}"),
      paste0("\\label{tab:", table_prefix, "_full_spec}"),
      paste0("\\input{tables/main_studies/tex_tables/", cs_file, "}"),
      "\\end{table}"
    )
  } else {
    warning("No performance data available for full spec tables")
    return(invisible(NULL))
  }

  output_path <- file.path(tex_tables_dir, paste0(table_prefix, "_full_spec.tex"))
  writeLines(combined, output_path)
  cat("Saved full spec tables to:", output_path, "\n")

  invisible(NULL)
}


# =============================================================================
# HETEROGENEITY TABLE (from regression_utils.R)
# =============================================================================

#' Create LaTeX table for heterogeneity tests
#'
#' @param het_results List of heterogeneity test results
#' @param construct_labels Named vector of display labels
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @param caption Table caption
#' @param label LaTeX label
#' @return LaTeX lines (invisible)
create_heterogeneity_latex <- function(
    het_results,
    construct_labels = NULL,
    table_dir = "outputs/tables/main_studies",
    filename = "heterogeneity_tests",
    caption = "Heterogeneity Tests",
    label = NULL) {

  if (is.null(label)) {
    label <- paste0("tab:", filename)
  }

  # Extract summary data from het_results list
  rows <- list()
  for (key in names(het_results)) {
    hr <- het_results[[key]]

    # Skip entries with missing construct
    if (is.null(hr$construct) || is.na(hr$construct)) {
      warning(sprintf("Skipping heterogeneity result '%s': missing construct", key))
      next
    }

    lr <- hr$lr_test

    # Extract test statistic from row 2 of anova output
    chisq_val <- lr$Chisq[2]
    df_val <- lr$Df[2]
    p_val <- hr$p_value

    cn_label <- if (!is.null(construct_labels) && hr$construct %in% names(construct_labels)) {
      construct_labels[[hr$construct]]
    } else {
      gsub("_", " ", tools::toTitleCase(as.character(hr$construct)))
    }

    study_label <- ifelse(hr$study_type == "cross_sectional",
                          "Cross-Sectional", "Longitudinal")

    decision <- ifelse(hr$heterogeneous, "Heterogeneous", "Homogeneous")

    rows[[length(rows) + 1]] <- list(
      construct = hr$construct,
      construct_label = cn_label,
      study_type = study_label,
      chisq = chisq_val,
      df = df_val,
      p_value = p_val,
      decision = decision
    )
  }

  # Build caption with note
  caption_with_note <- paste0(
    caption,
    ". Likelihood ratio test comparing models with vs.\\ without ",
    "outcome$\\times$treatment interactions. ",
    "A significant test ($p < .05$) indicates that interaction terms ",
    "should be included in the regression specification."
  )

  # Build table
  latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption_with_note, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{llrrl}",
    "\\toprule",
    "Construct & Study Type & $\\chi^2$ ($\\Delta$df) & $p$ & Decision \\\\",
    "\\midrule"
  )

  # Group rows by construct
  prev_construct <- NULL

  for (row in rows) {
    # Add midrule between constructs
    if (!is.null(prev_construct) && row$construct != prev_construct) {
      latex <- c(latex, "\\midrule")
    }

    # Format chi-squared with df
    chisq_str <- sprintf("%.2f (%d)", row$chisq, row$df)

    # Format p-value
    if (row$p_value < 0.001) {
      p_str <- "$< .001$"
    } else {
      p_str <- sprintf("%.3f", row$p_value)
    }

    # Bold if heterogeneous
    decision_str <- if (row$decision == "Heterogeneous") {
      paste0("\\textbf{", row$decision, "}")
    } else {
      row$decision
    }

    latex <- c(latex, paste0(
      row$construct_label, " & ",
      row$study_type, " & ",
      chisq_str, " & ",
      p_str, " & ",
      decision_str, " \\\\"
    ))

    prev_construct <- row$construct
  }

  latex <- c(latex,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    "\\normalsize"
  )

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved heterogeneity table to:", output_path, "\n")

  return(latex)
}


# =============================================================================
# ROBUSTNESS TABLE (from regression_utils.R)
# =============================================================================

#' Create LaTeX robustness table
#'
#' @param wide_df Wide-format robustness data frame
#' @param table_dir Output directory
#' @param filename_prefix Filename prefix
#' @param caption Table caption (base, without study type)
#' @param study_type "cross_sectional" or "longitudinal" for colored label
#' @return Character vector of LaTeX lines
create_robustness_latex_table <- function(wide_df,
                                           table_dir,
                                           filename_prefix = "robustness",
                                           caption = "Robustness Checks",
                                           study_type = NULL) {

  # If table_dir is NULL, skip table creation (caller will combine results)
  if (is.null(table_dir)) {
    return(NULL)
  }

  # Write to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)

  # Add formatted columns
  wide_df$Additive_fmt <- format_coeff_se_p(wide_df$additive_coeff, wide_df$additive_se, wide_df$additive_p)
  wide_df$Full_fmt <- format_coeff_se_p(wide_df$full_coeff, wide_df$full_se, wide_df$full_p)
  wide_df$Demos_fmt <- format_coeff_se_p(wide_df$demos_coeff, wide_df$demos_se, wide_df$demos_p)
  wide_df$Prefs_fmt <- format_coeff_se_p(wide_df$prefs_coeff, wide_df$prefs_se, wide_df$prefs_p)
  wide_df$Weighted_fmt <- format_coeff_se_p(wide_df$weighted_coeff, wide_df$weighted_se, wide_df$weighted_p)

  # Clean predictor labels
  wide_df$clean_predictor <- dplyr::case_when(
    wide_df$variable == "relationship_seeking_categorypos_lambda" ~ "Pos $\\lambda$",
    wide_df$variable == "relationship_seeking_categoryzero_lambda" ~ "Zero $\\lambda$",
    grepl("^lambda_factor", wide_df$variable) ~ gsub("lambda_factor", "$\\\\lambda_{", wide_df$variable) %>%
      gsub("neg", "-", .) %>% gsub("pos", "+", .) %>% paste0("}$"),
    wide_df$variable == "lambda" ~ "$\\lambda$",
    wide_df$variable == "I(lambda^2)" ~ "$\\lambda^2$",
    wide_df$variable == "I(lambda^3)" ~ "$\\lambda^3$",
    wide_df$variable == "domainemotchat" ~ "EmotChat",
    wide_df$variable == "personalisationpersonalised" ~ "Personalised",
    TRUE ~ wide_df$variable
  )

  # Build LaTeX table
  # Add colored study type label if provided
  study_label <- if (!is.null(study_type)) {
    paste0(" --- ", study_type_caption(study_type))
  } else {
    ""
  }

  caption_with_note <- paste0(
    caption, study_label,
    ". Cells show the coefficient estimate with standard error in ",
    "parentheses. Significance is indicated by ",
    "{\\itshape * $p<.05$, ** $p<.01$, *** $p<.001$}. ",
    "Mean $|\\Delta\\beta|$ gives the average absolute deviation of robustness ",
    "coefficients (Full + Demos, Full + Prefs, Full + IPW) from the Full ",
    "model estimate. ",
    "\\% Signif reports the proportion of specifications in which the ",
    "coefficient is statistically significant at $p<.05$."
  )

  n_cols <- 8
  header_row <- paste(
    "Predictor", "Additive", "+ Interactions", "Full + Demos",
    "Full + Prefs", "Full + IPW", "Mean $|\\Delta\\beta|$", "\\% Signif",
    sep = " & "
  )

  # Longtable headers and footers
  first_header <- c(
    paste0("\\caption{", caption_with_note, "}"),
    paste0("\\label{tab:", filename_prefix, "} \\\\"),
    "\\toprule",
    paste0(header_row, " \\\\"),
    "\\midrule",
    "\\endfirsthead"
  )

  continuation_header <- c(
    paste0("\\multicolumn{", n_cols, "}{c}{",
           "\\tablename\\ \\thetable{} -- \\textit{Continued from previous page}",
           "} \\\\"),
    "\\toprule",
    paste0(header_row, " \\\\"),
    "\\midrule",
    "\\endhead"
  )

  continuation_footer <- c(
    "\\midrule",
    paste0("\\multicolumn{", n_cols, "}{r}{\\textit{Continued on next page}} \\\\"),
    "\\endfoot"
  )

  final_footer <- c(
    "\\bottomrule",
    "\\endlastfoot"
  )

  latex_lines <- c(
    "\\footnotesize",
    "\\begin{longtable}{lccccccc}",
    first_header,
    continuation_header,
    continuation_footer,
    final_footer
  )

  current_outcome <- ""

  for (i in seq_len(nrow(wide_df))) {
    row <- wide_df[i, ]

    # Add outcome header if changed (as multicolumn row with gray background)
    if (row$outcome != current_outcome) {
      if (current_outcome != "") {
        latex_lines <- c(latex_lines, "\\addlinespace")
      }
      latex_lines <- c(latex_lines,
        paste0("\\rowcolor{gray!20} \\multicolumn{8}{l}{\\textbf{",
               format_outcome_label(row$outcome), "}} \\\\"))
      current_outcome <- row$outcome
    }

    # Format each column (might be NA)
    fmt_or_dash <- function(x) if (is.na(x) || x == "") "---" else x

    data_line <- paste(
      row$clean_predictor,
      fmt_or_dash(row$Additive_fmt),
      fmt_or_dash(row$Full_fmt),
      fmt_or_dash(row$Demos_fmt),
      fmt_or_dash(row$Prefs_fmt),
      fmt_or_dash(row$Weighted_fmt),
      if (is.na(row$mean_diff)) "---" else sprintf("%.2f", row$mean_diff),
      if (is.na(row$pct_signif)) "---" else paste0(row$pct_signif, "\\%"),
      sep = " & "
    )
    latex_lines <- c(latex_lines, paste0(data_line, " \\\\"))
  }

  latex_lines <- c(latex_lines, "\\end{longtable}")

  # Write to file
  output_path <- file.path(tex_tables_dir, paste0(filename_prefix, ".tex"))
  writeLines(latex_lines, output_path)
  cat("Saved robustness table to:", output_path, "\n")

  return(latex_lines)
}


# =============================================================================
# CONTRAST ROBUSTNESS TABLE (from regression_utils.R)
# =============================================================================

#' Create LaTeX table for contrast-based robustness checks
#'
#' @param results Data frame of contrast results
#' @param construct_order Order of constructs
#' @param construct_labels Named vector of display labels
#' @param has_ipw Whether IPW column is included
#' @param table_dir Output directory
#' @param filename Output filename
#' @param caption Table caption
#' @param label LaTeX label
#' @return LaTeX lines (invisible)
create_contrast_robustness_latex <- function(
    results,
    construct_order,
    construct_labels = NULL,
    has_ipw = FALSE,
    table_dir = "outputs/tables/main_studies",
    filename = "robustness_contrasts",
    caption = "Contrast-Based Robustness Checks",
    label = "tab:robustness_contrasts") {

  if (is.null(construct_labels)) {
    construct_labels <- setNames(
      gsub("_", " ", tools::toTitleCase(construct_order)),
      construct_order)
  }

  # Define spec columns
  spec_cols <- c("Baseline", "+ Demos", "+ Prefs")
  if (has_ipw) spec_cols <- c(spec_cols, "+ IPW")
  n_spec <- length(spec_cols)
  # Total columns: Contrast + specs + Sig Maintained + Max |Delta%|
  n_cols <- 1 + n_spec + 2
  col_align <- paste0("l", paste(rep("c", n_cols - 1), collapse = ""))

  # Build header
  header_specs <- paste(
    sapply(spec_cols, function(s) {
      paste0("\\textbf{", s, "}")
    }),
    collapse = " & ")
  header <- paste0(
    "\\textbf{Contrast} & ", header_specs,
    " & \\textbf{Sig Maint.}",
    " & \\textbf{Max $|\\Delta\\%|$} \\\\")

  # Build caption with note
  caption_with_note <- paste0(
    caption,
    ". Cells show the coefficient estimate with ",
    "$p$-value in parentheses. ",
    "Significance is indicated by ",
    "{\\itshape * $p<.05$, ** $p<.01$, *** $p<.001$}. ",
    "Sig Maint.\\ reports the number of robustness specifications in which ",
    "significance status matches the baseline. ",
    "Max $|\\Delta\\%|$ gives the largest percentage change in the estimate ",
    "relative to the baseline across robustness specifications."
  )

  # Start table
  latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    paste0("\\caption{", caption_with_note, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\footnotesize"),
    paste0("\\begin{tabular}{", col_align, "}"),
    "\\toprule",
    header,
    "\\midrule"
  )

  # Build rows per construct
  for (cn in construct_order) {
    cn_label <- construct_labels[[cn]]
    # Multicolumn header for construct (centered)
    latex <- c(latex, paste0(
      "\\multicolumn{", n_cols, "}{c}{\\textbf{",
      cn_label, "}} \\\\"))

    cn_data <- results[results$construct == cn, ]
    contrast_types <- unique(cn_data$contrast_type)

    for (ct in contrast_types) {
      ct_data <- cn_data[cn_data$contrast_type == ct, ]

      # Get baseline
      baseline_row <- ct_data[ct_data$spec == "Baseline", ]
      if (nrow(baseline_row) == 0) next
      baseline_est <- baseline_row$estimate
      baseline_p <- baseline_row$p.value
      baseline_sig <- baseline_p < 0.05

      # Format each spec cell
      cells <- c()
      for (sp in spec_cols) {
        sp_row <- ct_data[ct_data$spec == sp, ]
        if (nrow(sp_row) > 0) {
          cells <- c(cells, format_est_p(
            sp_row$estimate, sp_row$p.value))
        } else {
          cells <- c(cells, "---")
        }
      }

      # Summary: Sig Maintained
      comparison_specs <- setdiff(spec_cols, "Baseline")
      n_compared <- 0
      n_maintained <- 0
      pct_changes <- c()

      for (sp in comparison_specs) {
        sp_row <- ct_data[ct_data$spec == sp, ]
        if (nrow(sp_row) > 0) {
          n_compared <- n_compared + 1
          sp_sig <- sp_row$p.value < 0.05
          if (sp_sig == baseline_sig) {
            n_maintained <- n_maintained + 1
          }
          if (abs(baseline_est) > 0.001) {
            pct_change <- 100 * abs(
              sp_row$estimate - baseline_est
            ) / abs(baseline_est)
            pct_changes <- c(pct_changes, pct_change)
          }
        }
      }

      sig_cell <- if (n_compared > 0) {
        paste0(n_maintained, "/", n_compared)
      } else { "---" }

      delta_cell <- if (length(pct_changes) > 0) {
        sprintf("%.1f\\%%", max(pct_changes))
      } else { "---" }

      row_str <- paste0(
        ct, " & ",
        paste(cells, collapse = " & "),
        " & ", sig_cell,
        " & ", delta_cell,
        " \\\\")
      latex <- c(latex, row_str)
    }

    # Add midrule between constructs (not after last)
    if (cn != tail(construct_order, 1)) {
      latex <- c(latex, "\\midrule")
    }
  }

  # Close table
  latex <- c(latex,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}")

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved contrast robustness table to:", output_path, "\n")

  return(latex)
}


# =============================================================================
# CONTRAST TABLE FORMATTING (from contrast_utils.R)
# =============================================================================

#' Format contrast data frame to LaTeX tabular
#'
#' @param df Data frame of contrast results
#' @return Character vector of LaTeX tabular lines
format_contrast_df_to_latex <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  # Determine descriptor column
  descriptor_col <- NULL
  for (cand in c("contrast", "coefficient",
                  "personalisation", "domain",
                  "relationship_seeking_category")) {
    if (cand %in% names(df)) { descriptor_col <- cand; break }
  }

  # Determine estimate column
  estimate_col <- intersect(
    c("estimate", "lambda.trend",
      "session_numeric.trend", "week_numeric.trend"),
    names(df))[1]
  if (is.na(estimate_col)) return(NULL)

  # Build output columns as a list, then convert
  cols <- list()

  # Construct
  if ("construct" %in% names(df)) {
    cols$construct <- as.character(df$construct)
  }

  # Descriptor (contrast/coefficient/etc.)
  if (!is.null(descriptor_col)) {
    cols[[descriptor_col]] <- as.character(df[[descriptor_col]])
  }

  # Estimate
  cols[[estimate_col]] <- df[[estimate_col]]

  # 95% CI (from lower.CL and upper.CL if available)
  has_ci <- all(c("lower.CL", "upper.CL") %in% names(df))
  if (has_ci) {
    cols$ci <- sprintf("[%.2f, %.2f]", df$lower.CL, df$upper.CL)
  }

  # Raw p-value
  if ("p.value" %in% names(df)) {
    cols$p.value <- df$p.value
  }

  # Adjusted p-value
  if ("p.value.adj" %in% names(df)) {
    cols$p.value.adj <- df$p.value.adj
  }

  # Significance stars (from p.value.adj)
  if ("p.value.adj" %in% names(df)) {
    p_adj <- as.numeric(df$p.value.adj)
    cols$sig <- ifelse(p_adj < 0.001, "***",
      ifelse(p_adj < 0.01, "**",
      ifelse(p_adj < 0.05, "*", "")))
  }

  # Family-adjusted p-value
  if ("p.value.adj.family" %in% names(df)) {
    cols$p.value.adj.family <- df$p.value.adj.family
  }

  if (length(cols) < 3) return(NULL)
  out_df <- as.data.frame(cols, stringsAsFactors = FALSE)

  # Format numeric values
  for (cn in names(out_df)) {
    if (!is.numeric(out_df[[cn]])) next
    out_df[[cn]] <- if (cn %in% c("p.value", "p.value.adj",
                                    "p.value.adj.family")) {
      ifelse(out_df[[cn]] < 0.001, "$<$.001",
              sprintf("%.3f", out_df[[cn]]))
    } else {
      sprintf("%.3f", out_df[[cn]])
    }
  }

  # Clean variable names in text columns
  text_cols <- c("construct", "contrast", "coefficient",
                  "personalisation", "domain",
                  "relationship_seeking_category")
  for (tc in intersect(text_cols, names(out_df))) {
    out_df[[tc]] <- clean_contrast_varnames(out_df[[tc]])
  }

  # Column headers & alignment
  header_map <- c(
    construct = "Construct", contrast = "Contrast",
    coefficient = "Coefficient",
    personalisation = "Personalisation", domain = "Domain",
    relationship_seeking_category = "RS Category",
    estimate = "Est.", lambda.trend = "Trend",
    session_numeric.trend = "Trend", week_numeric.trend = "Trend",
    ci = "95\\% CI",
    p.value = "$p_{\\text{raw}}$",
    p.value.adj = "$p_{\\text{within}}$",
    sig = "",
    p.value.adj.family = "$p_{\\text{across}}$")
  headers <- sapply(names(out_df), function(x) {
    if (x %in% names(header_map)) header_map[[x]] else x
  })
  aligns <- sapply(names(out_df), function(cn) {
    if (cn %in% text_cols || cn == "sig") "l"
    else if (cn == "ci") "c"
    else "r"
  })

  tab <- c(
    paste0("\\begin{tabular}{", paste(aligns, collapse = ""), "}"),
    "\\toprule",
    paste0(paste(headers, collapse = " & "), " \\\\"),
    "\\midrule")
  for (i in seq_len(nrow(out_df))) {
    tab <- c(tab, paste0(
      paste(sapply(out_df[i, ], as.character), collapse = " & "),
      " \\\\"))
  }
  c(tab, "\\bottomrule", "\\end{tabular}")
}


#' Render a list of table specs as LaTeX
#'
#' @param spec_list List of spec items with title, desc, df
#' @param section_title Section title
#' @return Character vector of LaTeX lines
render_section_latex <- function(spec_list, section_title) {
  out <- c(
    paste0("\\subsubsection*{", to_latex_text(section_title), "}"),
    "")

  for (item in spec_list) {
    tab <- format_contrast_df_to_latex(item$df)
    if (is.null(tab)) next
    title_tex <- to_latex_text(item$title)
    desc_tex <- to_latex_text(item$desc)

    out <- c(out, paste0("\\paragraph{", title_tex, "}"), "")
    if (!is.null(desc_tex) && nchar(desc_tex) > 0) {
      out <- c(out, paste0("\\textit{", desc_tex, "}"), "")
    }
    out <- c(out, "\\begin{center}", "\\footnotesize", tab,
              "\\end{center}", "")
  }
  out
}


# =============================================================================
# SUMMARY TABLE RENDERING (from contrast_utils.R)
# =============================================================================

#' Render summary table to LaTeX longtable with outcome as grouped row headers
#'
#' @param df data.frame from build_*_summary functions (must have 'outcome' column)
#' @param caption table caption (can include LaTeX)
#' @param label LaTeX label
#' @param col_names column header names (excluding 'Outcome' - handled separately)
#' @param outcome_col name of the outcome column (default "outcome")
#' @return character vector of LaTeX lines
render_summary_to_latex <- function(df, caption, label, col_names = NULL,
                                     outcome_col = "outcome") {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Identify data columns (everything except outcome)
  data_cols <- setdiff(names(df), outcome_col)
  ncols <- length(data_cols)

  # Column spec: all left-aligned for text, right for numbers
  col_spec <- paste(rep("l", ncols), collapse = "")

  # Default column names if not provided
  if (is.null(col_names)) {
    col_names <- data_cols
  }

  # Build header
  header <- paste(col_names, collapse = " & ")

  out <- c(
    "\\footnotesize",
    paste0("\\begin{longtable}{@{}", col_spec, "@{}}"),
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "} \\\\"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endfirsthead",
    paste0("\\multicolumn{", ncols, "}{c}{\\tablename\\ \\thetable{} -- continued} \\\\"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endhead",
    "\\midrule",
    paste0("\\multicolumn{", ncols, "}{r}{Continued on next page} \\\\"),
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot"
  )

  # Add data rows grouped by outcome
  outcomes <- unique(df[[outcome_col]])
  for (outcome in outcomes) {
    # Outcome header row with grey background
    outcome_escaped <- escape_latex_text(tools::toTitleCase(as.character(outcome)))
    out <- c(out,
      paste0("\\rowcolor{gray!20} \\multicolumn{", ncols, "}{l}{\\textbf{", outcome_escaped, "}} \\\\")
    )

    # Data rows for this outcome
    outcome_rows <- df[df[[outcome_col]] == outcome, data_cols, drop = FALSE]
    for (i in seq_len(nrow(outcome_rows))) {
      row_data <- sapply(outcome_rows[i, ], function(x) escape_latex_text(as.character(x)))
      out <- c(out, paste0(paste(row_data, collapse = " & "), " \\\\"))
    }
  }

  out <- c(out, "\\end{longtable}", "\\normalsize")
  out
}


#' Render wide robustness table to LaTeX (special format)
#'
#' @param df data.frame with robustness comparisons
#' @param caption table caption
#' @param label LaTeX label
#' @param col_names column header names
#' @param outcome_col name of the outcome column
#' @return character vector of LaTeX lines
render_robustness_to_latex <- function(df, caption, label, col_names,
                                        outcome_col = "outcome") {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  data_cols <- setdiff(names(df), outcome_col)
  ncols <- length(data_cols)
  col_spec <- paste(rep("l", ncols), collapse = "")

  header <- paste(col_names, collapse = " & ")

  out <- c(
    "\\footnotesize",
    paste0("\\begin{longtable}{@{}", col_spec, "@{}}"),
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "} \\\\"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endfirsthead",
    paste0("\\multicolumn{", ncols, "}{c}{\\tablename\\ \\thetable{} -- continued} \\\\"),
    "\\toprule",
    paste0(header, " \\\\"),
    "\\midrule",
    "\\endhead",
    "\\midrule",
    paste0("\\multicolumn{", ncols, "}{r}{Continued on next page} \\\\"),
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot"
  )

  # Group by outcome
  outcomes <- unique(df[[outcome_col]])
  for (outcome in outcomes) {
    outcome_escaped <- escape_latex_text(tools::toTitleCase(as.character(outcome)))
    out <- c(out,
      paste0("\\rowcolor{gray!20} \\multicolumn{", ncols, "}{l}{\\textbf{", outcome_escaped, "}} \\\\")
    )

    outcome_rows <- df[df[[outcome_col]] == outcome, data_cols, drop = FALSE]
    for (i in seq_len(nrow(outcome_rows))) {
      row_data <- sapply(outcome_rows[i, ], function(x) escape_latex_text(as.character(x)))
      out <- c(out, paste0(paste(row_data, collapse = " & "), " \\\\"))
    }
  }

  out <- c(out, "\\end{longtable}", "\\normalsize")
  out
}


# =============================================================================
# VULNERABILITY ANALYSIS TABLES
# =============================================================================

#' Create LaTeX table for vulnerability main effects
#'
#' Long format: one row per predictor-outcome combination
#' Columns: Outcome, Predictor, Estimate [95% CI], p (raw), p (within), p (global)
#'
#' @param main_effects_df Data frame with main effects results
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @return Character vector of LaTeX lines (invisible)
create_vulnerability_main_effects_latex <- function(
    main_effects_df,
    table_dir,
    filename = "vulnerability_main_effects") {

  if (is.null(main_effects_df) || nrow(main_effects_df) == 0) {
    cat("No main effects data to create table\n")
    return(invisible(NULL))
  }

  # Filter to significant effects and arrange
  sig_effects <- main_effects_df %>%
    filter(p_within < 0.05) %>%
    arrange(outcome, p_within)

  if (nrow(sig_effects) == 0) {
    cat("No significant main effects to create table\n")
    return(invisible(NULL))
  }

  # Get predictor labels
  pred_labels <- get_sociodemo_labels()

  # Build caption with note
  caption_note <- paste0(
    "Significant Main Effects of Participant Characteristics on Outcomes ($p_{\\text{within}} < .05$). ",
    "Bold $p_{\\text{global}}$ indicates significance after global FDR correction."
  )

  # Start longtable
  latex <- c(
    "\\footnotesize",
    "\\begin{longtable}{llcrrr}",
    paste0("\\caption{", caption_note, "}"),
    paste0("\\label{tab:", filename, "} \\\\"),
    "\\toprule",
    "Outcome & Predictor & Estimate [95\\% CI] & $p_{\\text{raw}}$ & $p_{\\text{within}}$ & $p_{\\text{global}}$ \\\\",
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\multicolumn{6}{c}{\\textit{\\tablename\\ \\thetable{} -- continued from previous page}} \\\\",
    "\\toprule",
    "Outcome & Predictor & Estimate [95\\% CI] & $p_{\\text{raw}}$ & $p_{\\text{within}}$ & $p_{\\text{global}}$ \\\\",
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

  # Track current outcome for grouping
  current_outcome <- ""

  for (i in seq_len(nrow(sig_effects))) {
    row <- sig_effects[i, ]

    # Clean outcome name
    outcome_clean <- format_outcome_label(row$outcome)

    # Use predefined predictor labels
    predictor_clean <- if (row$predictor %in% names(pred_labels)) {
      pred_labels[row$predictor]
    } else {
      tools::toTitleCase(gsub("_", " ", row$predictor))
    }

    # Format estimate with CI
    est_ci <- sprintf("%.2f [%.2f, %.2f]", row$estimate, row$ci_low, row$ci_high)

    # Format p-values
    format_p <- function(p) {
      if (p < 0.001) return("$<$.001")
      sprintf("%.3f", p)
    }

    p_raw_fmt <- format_p(row$p_raw)
    p_within_fmt <- format_p(row$p_within)
    p_global_fmt <- format_p(row$p_global)

    # Bold p_global if significant
    if (row$p_global < 0.05) {
      p_global_fmt <- paste0("\\textbf{", p_global_fmt, "}")
    }

    # Add outcome header row if changed
    if (row$outcome != current_outcome) {
      if (current_outcome != "") {
        latex <- c(latex, "\\addlinespace")
      }
      latex <- c(latex, paste0(
        "\\multicolumn{6}{l}{\\textbf{", outcome_clean, "}} \\\\"
      ))
      current_outcome <- row$outcome
    }

    # Add data row (indent predictor)
    latex <- c(latex, sprintf(
      "  & %s & %s & %s & %s & %s \\\\",
      predictor_clean, est_ci, p_raw_fmt, p_within_fmt, p_global_fmt
    ))
  }

  # Close longtable (bottomrule already in endlastfoot)
  latex <- c(latex,
             "\\end{longtable}")

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved vulnerability main effects table to:", output_path, "\n")

  invisible(latex)
}


#' Create LaTeX table for vulnerability interaction effects
#'
#' Long format: one row per interaction term-outcome combination
#' Columns: Outcome, Interaction Term, Estimate [95% CI], p (raw), p (within), p (global)
#'
#' @param interactions_df Data frame with interaction results
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @return Character vector of LaTeX lines (invisible)
create_vulnerability_interactions_latex <- function(
    interactions_df,
    table_dir,
    filename = "vulnerability_interactions") {

  if (is.null(interactions_df) || nrow(interactions_df) == 0) {
    cat("No interaction data to create table\n")
    return(invisible(NULL))
  }

  # Filter to significant effects and arrange
  sig_effects <- interactions_df %>%
    filter(p_within < 0.05) %>%
    arrange(outcome, p_within)

  if (nrow(sig_effects) == 0) {
    cat("No significant interactions to create table\n")
    return(invisible(NULL))
  }

  # Get predictor labels
  pred_labels <- get_sociodemo_labels()

  # Build caption with note
  caption_note <- paste0(
    "Significant Interaction Effects: Companionship Condition $\\times$ Participant Characteristics ($p_{\\text{within}} < .05$). ",
    "Bold $p_{\\text{global}}$ indicates significance after global FDR correction. ",
    "Significant interactions indicate differential response to the companionship condition."
  )

  # Start table
  latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption_note, "}"),
    paste0("\\label{tab:", filename, "}"),
    "\\begin{tabular}{llcrrr}",
    "\\toprule",
    "Outcome & Interaction Term & Estimate [95\\% CI] & $p_{\\text{raw}}$ & $p_{\\text{within}}$ & $p_{\\text{global}}$ \\\\",
    "\\midrule"
  )

  # Track current outcome for grouping
  current_outcome <- ""

  for (i in seq_len(nrow(sig_effects))) {
    row <- sig_effects[i, ]

    # Clean outcome name
    outcome_clean <- format_outcome_label(row$outcome)

    # Extract base predictor from interaction term and use predefined labels
    base_pred <- gsub("companionship_condition:", "", row$interaction_term)
    pred_label <- if (base_pred %in% names(pred_labels)) {
      pred_labels[base_pred]
    } else {
      tools::toTitleCase(gsub("_", " ", base_pred))
    }
    term_clean <- paste0("Comp. $\\times$ ", pred_label)

    # Format estimate with CI
    est_ci <- sprintf("%.2f [%.2f, %.2f]", row$estimate, row$ci_low, row$ci_high)

    # Format p-values
    format_p <- function(p) {
      if (p < 0.001) return("$<$.001")
      sprintf("%.3f", p)
    }

    p_raw_fmt <- format_p(row$p_raw)
    p_within_fmt <- format_p(row$p_within)
    p_global_fmt <- format_p(row$p_global)

    # Bold p_global if significant
    if (row$p_global < 0.05) {
      p_global_fmt <- paste0("\\textbf{", p_global_fmt, "}")
    }

    # Add outcome header row if changed
    if (row$outcome != current_outcome) {
      if (current_outcome != "") {
        latex <- c(latex, "\\addlinespace")
      }
      latex <- c(latex, paste0(
        "\\multicolumn{6}{l}{\\textbf{", outcome_clean, "}} \\\\"
      ))
      current_outcome <- row$outcome
    }

    # Add data row (indent interaction term)
    latex <- c(latex, sprintf(
      "  & %s & %s & %s & %s & %s \\\\",
      term_clean, est_ci, p_raw_fmt, p_within_fmt, p_global_fmt
    ))
  }

  # Close table
  latex <- c(latex,
             "\\bottomrule",
             "\\end{tabular}",
             "\\end{table}")

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved vulnerability interactions table to:", output_path, "\n")

  invisible(latex)
}


# =============================================================================
# SIMPLE STATISTICAL TESTS TABLES
# =============================================================================
# These functions format chi-square, Fisher's exact, and t-test results
# as LaTeX tables. Useful for EDA scripts that don't run regressions.
# =============================================================================

#' Create LaTeX table for chi-square/Fisher's test results
#'
#' @param results_df Data frame with columns: outcome, treatment, test_type,
#'   statistic, df, p_value (and optionally p_adj)
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @param caption Table caption
#' @param label LaTeX label (default: tab:filename)
#' @return Character vector of LaTeX lines (invisible)
create_chisq_fisher_latex <- function(
    results_df,
    table_dir,
    filename,
    caption = "Chi-Square and Fisher's Exact Test Results",
    label = NULL) {

  if (is.null(results_df) || nrow(results_df) == 0) {
    cat("No chi-square/Fisher results to create table\n")
    return(invisible(NULL))
  }

  if (is.null(label)) {
    label <- paste0("tab:", filename)
  }

  # Determine if we have adjusted p-values
  has_adj <- "p_adj" %in% names(results_df) || "p_adj_fdr" %in% names(results_df)
  adj_col <- if ("p_adj_fdr" %in% names(results_df)) "p_adj_fdr" else "p_adj"

  # Build column spec
  if (has_adj) {
    col_spec <- "llrrrr"
    header <- "Outcome & Treatment & Test & Statistic (df) & $p$ & $p_{\\text{adj}}$ \\\\"
  } else {
    col_spec <- "llrrr"
    header <- "Outcome & Treatment & Test & Statistic (df) & $p$ \\\\"
  }

  # Build caption with note
  caption_note <- paste0(
    caption,
    ". Bold $p$-values indicate significance at $\\alpha = .05$."
  )
  if (has_adj) {
    caption_note <- paste0(caption_note, " $p_{\\text{adj}}$ = FDR-corrected.")
  }

  # Start table
  latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption_note, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule",
    header,
    "\\midrule"
  )

  # Format p-value helper
  fmt_p <- function(p) {
    if (is.na(p)) return("---")
    if (p < 0.001) return("$<$.001")
    sprintf("%.3f", p)
  }

  # Add rows
  for (i in seq_len(nrow(results_df))) {
    row <- results_df[i, ]

    # Format statistic with df
    if (!is.na(row$statistic) && !is.na(row$df)) {
      stat_str <- sprintf("%.2f (%s)", row$statistic,
                          if (is.numeric(row$df)) sprintf("%d", as.integer(row$df)) else as.character(row$df))
    } else if (!is.na(row$statistic)) {
      stat_str <- sprintf("%.2f", row$statistic)
    } else {
      stat_str <- "---"
    }

    # Format p-values
    p_raw <- fmt_p(row$p_value)
    if (!is.na(row$p_value) && row$p_value < 0.05) {
      p_raw <- paste0("\\textbf{", p_raw, "}")
    }

    # Escape underscores for LaTeX
    outcome_clean <- gsub("_", "\\\\_", row$outcome)
    treatment_clean <- gsub("_", "\\\\_", row$treatment)

    if (has_adj) {
      p_adj_val <- row[[adj_col]]
      p_adj_str <- fmt_p(p_adj_val)
      if (!is.na(p_adj_val) && p_adj_val < 0.05) {
        p_adj_str <- paste0("\\textbf{", p_adj_str, "}")
      }
      latex <- c(latex, sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        outcome_clean, treatment_clean, row$test_type, stat_str, p_raw, p_adj_str
      ))
    } else {
      latex <- c(latex, sprintf(
        "%s & %s & %s & %s & %s \\\\",
        outcome_clean, treatment_clean, row$test_type, stat_str, p_raw
      ))
    }
  }

  # Close table
  latex <- c(latex,
             "\\bottomrule",
             "\\end{tabular}",
             "\\end{table}")

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved chi-square/Fisher table to:", output_path, "\n")

  invisible(latex)
}


#' Nice labels for treatment/outcome variables
get_chisq_nice_labels <- function() {
  c(
    # Treatment variables
    "lambda_factor" = "Multiplier ($\\lambda$)",
    "relationship_seeking_category" = "RS Category",
    "study_id" = "Study",
    # Outcome variables
    "change_sentience_recoded" = "Change in Sentience View",
    "future_view" = "Future View",
    "change_recoded" = "Perceived Change",
    "seeking_companionship_likelihood" = "Seeking Companionship",
    "personalisation" = "Personalisation",
    "domain" = "Domain",
    "goodbye_action" = "Goodbye Action",
    "goodbye_feeling_recoded" = "Goodbye Feeling",
    "change_recoded" = "Perceived Change",
    "relational_change_recoded" = "Relational Change Binary",
    # Continuous outcomes
    "distant_close_change" = "Distant-Close Change",
    "tool_friend_change" = "Tool-Friend Change",
    "will_miss_ai" = "Will Miss AI",
    # Group labels
    "pos_lambda" = "Pos $\\lambda$",
    "neg_lambda" = "Neg $\\lambda$",
    "non-personalised" = "Non-personalised",
    "personalised" = "Personalised",
    "emotchat" = "Emotchat",
    "polchat" = "Polchat"
  )
}

#' Create LaTeX table for chi-square tests with breakdown for significant results
#'
#' @param test_results Raw results from run_standard_treatment_tests()
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @param caption Table caption
#' @param label LaTeX label (default: tab:filename)
#' @return Character vector of LaTeX lines (invisible)
create_chisq_with_breakdown_latex <- function(
    test_results,
    table_dir,
    filename,
    caption = "Chi-Square Tests",
    label = NULL) {

  if (is.null(test_results) || length(test_results) == 0) {
    cat("No chi-square results to create table\n")
    return(invisible(NULL))
  }

  if (is.null(label)) {
    label <- paste0("tab:", filename)
  }

  nice_labels <- get_chisq_nice_labels()


  # Helper to get nice label (escape underscores if not in lookup)
  nice_label <- function(x) {
    x_clean <- as.character(x)
    if (x_clean %in% names(nice_labels)) {
      nice_labels[[x_clean]]
    } else {
      gsub("_", "\\\\_", x_clean)
    }
  }

  # Format p-value helper
  fmt_p <- function(p) {
    if (is.na(p)) return("---")
    if (p < 0.001) return("$<$.001")
    sprintf("%.3f", p)
  }

  # Build caption
  caption_note <- paste0(
    caption,
    ". Bold $p$-values indicate significance at $\\alpha = .05$. ",
    "Breakdowns shown for significant tests."
  )

  # Start table
  latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption_note, "}"),
    paste0("\\label{", label, "}"),
    "\\begin{tabular}{llrrr}",
    "\\toprule",
    "Outcome & Treatment & $\\chi^2$ (df) & $p$ & \\\\",
    "\\midrule"
  )

  # Process each study type
  for (study_type in names(test_results)) {
    study_label <- switch(study_type,
      "cross_study" = "Cross-Study",
      "cross_sectional" = "Cross-Sectional",
      "longitudinal" = "Longitudinal",
      study_type
    )

    study_results <- test_results[[study_type]]
    if (is.null(study_results) || length(study_results) == 0) next

    # Add study header
    latex <- c(latex,
      sprintf("\\multicolumn{5}{l}{\\textit{%s}} \\\\", study_label)
    )

    for (outcome in names(study_results)) {
      for (trt_var in names(study_results[[outcome]])) {
        res <- study_results[[outcome]][[trt_var]]
        if (is.null(res) || is.na(res$p_value)) next

        # Format statistic
        stat_str <- sprintf("%.2f (%d)", res$chi_sq, as.integer(res$df))

        # Format p-value
        p_str <- fmt_p(res$p_value)
        if (!is.na(res$p_value) && res$p_value < 0.05) {
          p_str <- paste0("\\textbf{", p_str, "}")
        }

        # Main row
        latex <- c(latex, sprintf(
          "\\quad %s & %s & %s & %s & \\\\",
          nice_label(outcome), nice_label(trt_var), stat_str, p_str
        ))

        # Add breakdown if significant and available
        if (res$significant && !is.null(res$breakdown)) {
          bd <- res$breakdown

          # Get unique levels (outcome categories)
          levels <- unique(bd$level)

          # Add sub-header row with outcome levels (use commas, not &)
          level_header <- paste(sapply(levels, function(l) {
            gsub("_", " ", as.character(l))
          }), collapse = ", ")
          latex <- c(latex, sprintf(
            "\\quad\\quad & \\multicolumn{4}{l}{\\scriptsize [%s]} \\\\",
            level_header
          ))

          # Add row for each treatment group
          for (grp in unique(bd$group)) {
            grp_data <- bd[bd$group == grp, ]
            cells <- sapply(levels, function(l) {
              row_data <- grp_data[grp_data$level == l, ]
              if (nrow(row_data) > 0) {
                sprintf("%.1f\\%% (n=%d)", row_data$percentage, row_data$n)
              } else {
                "---"
              }
            })
            grp_clean <- gsub("_", " ", as.character(grp))
            latex <- c(latex, sprintf(
              "\\quad\\quad & \\multicolumn{4}{l}{\\scriptsize %s: %s} \\\\",
              grp_clean, paste(cells, collapse = "; ")
            ))
          }
        }
      }
    }
    latex <- c(latex, "\\midrule")
  }

  # Remove last midrule and close table
  latex <- latex[-length(latex)]
  latex <- c(latex,
             "\\bottomrule",
             "\\end{tabular}",
             "\\end{table}")

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved chi-square table with breakdowns to:", output_path, "\n")

  invisible(latex)
}


#' Create LaTeX table for emmeans pairwise contrasts
#'
#' @param pairwise_results List with study types as keys, each containing
#'   $pairwise data frame from emmeans::pairs() summary
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @param caption Table caption
#' @param label LaTeX label (default: tab:filename)
#' @param is_logistic Whether model is logistic (shows OR instead of estimate)
#' @return Character vector of LaTeX lines (invisible)
create_pairwise_latex <- function(
    pairwise_results,
    table_dir,
    filename,
    caption = "Pairwise Contrasts (FDR-adjusted)",
    label = NULL,
    is_logistic = TRUE) {

  if (is.null(pairwise_results) || length(pairwise_results) == 0) {
    cat("No pairwise results to create table\n")
    return(invisible(NULL))
  }

  if (is.null(label)) {
    label <- paste0("tab:", filename)
  }

  # Determine column headers based on model type
  if (is_logistic) {
    col_spec <- "llrrrr"
    header <- "Study & Contrast & OR & SE & $z$ & $p$ \\\\"
    est_label <- "odds.ratio"
  } else {
    col_spec <- "llrrrr"
    header <- "Study & Contrast & Estimate & SE & $t$/$z$ & $p$ \\\\"
    est_label <- "estimate"
  }

  # Build caption with note
  caption_note <- paste0(
    caption,
    ". Bold $p$-values indicate significance at $\\alpha = .05$."
  )

  # Start table
  latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption_note, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule",
    header,
    "\\midrule"
  )

  # Format p-value helper
  fmt_p <- function(p) {
    if (is.na(p)) return("---")
    if (p < 0.001) return("$<$.001")
    sprintf("%.3f", p)
  }

  # Process each study type
  for (study_type in names(pairwise_results)) {
    pw_df <- pairwise_results[[study_type]]$pairwise
    if (is.null(pw_df) || nrow(pw_df) == 0) next

    study_label <- ifelse(study_type == "cross_sectional",
                          "Cross-Sectional", "Longitudinal")

    # Add study separator if not first
    if (study_type != names(pairwise_results)[1]) {
      latex <- c(latex, "\\midrule")
    }

    for (i in seq_len(nrow(pw_df))) {
      row <- pw_df[i, ]

      # Get estimate (OR or estimate depending on model)
      est_col <- if ("odds.ratio" %in% names(row)) "odds.ratio" else "estimate"
      est_val <- if (!is.na(row[[est_col]])) sprintf("%.2f", row[[est_col]]) else "---"

      # Get SE
      se_val <- if (!is.na(row$SE)) sprintf("%.2f", row$SE) else "---"

      # Get z/t ratio
      z_col <- if ("z.ratio" %in% names(row)) "z.ratio" else "t.ratio"
      z_val <- if (!is.na(row[[z_col]])) sprintf("%.2f", row[[z_col]]) else "---"

      # Get p-value
      p_val <- fmt_p(row$p.value)
      if (!is.na(row$p.value) && row$p.value < 0.05) {
        p_val <- paste0("\\textbf{", p_val, "}")
      }

      # Clean contrast name (escape special chars)
      contrast_clean <- gsub("_", "\\\\_", as.character(row$contrast))

      # Only show study label for first row of each study
      study_str <- if (i == 1) study_label else ""

      latex <- c(latex, sprintf(
        "%s & %s & %s & %s & %s & %s \\\\",
        study_str, contrast_clean, est_val, se_val, z_val, p_val
      ))
    }
  }

  # Close table
  latex <- c(latex,
             "\\bottomrule",
             "\\end{tabular}",
             "\\end{table}")

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved pairwise contrasts table to:", output_path, "\n")

  invisible(latex)
}


#' Create LaTeX table for t-test results
#'
#' @param results_df Data frame with columns: study, outcome, treatment (optional),
#'   group1, group2 (for two-sample), n/n1/n2, mean_change/mean1/mean2,
#'   sd_change (optional), t_stat, df, p_value, ci_lower, ci_upper
#' @param test_type Either "one_sample" or "two_sample"
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @param caption Table caption
#' @param label LaTeX label (default: tab:filename)
#' @return Character vector of LaTeX lines (invisible)
create_ttest_latex <- function(
    results_df,
    test_type = c("one_sample", "two_sample"),
    table_dir,
    filename,
    caption = NULL,
    label = NULL) {

  test_type <- match.arg(test_type)

  if (is.null(results_df) || nrow(results_df) == 0) {
    cat("No t-test results to create table\n")
    return(invisible(NULL))
  }

  if (is.null(label)) {
    label <- paste0("tab:", filename)
  }

  if (is.null(caption)) {
    caption <- if (test_type == "one_sample") {
      "One-Sample T-Test Results (Testing Change $\\neq$ 0)"
    } else {
      "Two-Sample T-Test Results"
    }
  }

  # Build table based on test type
  if (test_type == "one_sample") {
    col_spec <- "llrrrrrr"
    header <- "Study & Outcome & $n$ & Mean $\\Delta$ & SD & $t$ & df & $p$ \\\\"

    # Build caption with note
    caption_note <- paste0(
      caption,
      ". Testing whether pre-post change differs from zero. ",
      "Bold $p$-values indicate significance at $\\alpha = .05$."
    )
  } else {
    col_spec <- "llllrrrrrr"
    header <- "Study & Outcome & Group 1 & Group 2 & $\\bar{x}_1$ & $\\bar{x}_2$ & $t$ & df & $p$ \\\\"

    caption_note <- paste0(
      caption,
      ". Comparing means between treatment groups. ",
      "Bold $p$-values indicate significance at $\\alpha = .05$."
    )
  }

  # Start table
  latex <- c(
    "\\begin{table}[H]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption_note, "}"),
    paste0("\\label{", label, "}"),
    paste0("\\begin{tabular}{", col_spec, "}"),
    "\\toprule",
    header,
    "\\midrule"
  )

  # Format p-value helper
  fmt_p <- function(p) {
    if (is.na(p)) return("---")
    if (p < 0.001) return("$<$.001")
    sprintf("%.3f", p)
  }

  # Get nice labels and create helper function
  nice_labels <- get_chisq_nice_labels()
  nice_label <- function(x) {
    x_clean <- as.character(x)
    if (x_clean %in% names(nice_labels)) {
      nice_labels[[x_clean]]
    } else {
      gsub("_", "\\\\_", x_clean)
    }
  }

  # Add rows
  for (i in seq_len(nrow(results_df))) {
    row <- results_df[i, ]

    # Format p-value with bolding
    p_str <- fmt_p(row$p_value)
    if (!is.na(row$p_value) && row$p_value < 0.05) {
      p_str <- paste0("\\textbf{", p_str, "}")
    }

    if (test_type == "one_sample") {
      latex <- c(latex, sprintf(
        "%s & %s & %d & %.2f & %.2f & %.2f & %.0f & %s \\\\",
        if ("study" %in% names(row)) nice_label(row$study) else "---",
        nice_label(row$outcome),
        row$n,
        row$mean_change,
        if ("sd_change" %in% names(row)) row$sd_change else NA,
        row$t_stat,
        row$df,
        p_str
      ))
    } else {
      # Two-sample
      latex <- c(latex, sprintf(
        "%s & %s & %s & %s & %.2f & %.2f & %.2f & %.1f & %s \\\\",
        if ("study" %in% names(row)) nice_label(row$study) else "---",
        nice_label(row$outcome),
        nice_label(row$group1),
        nice_label(row$group2),
        row$mean1,
        row$mean2,
        row$t_stat,
        row$df,
        p_str
      ))
    }
  }

  # Close table
  latex <- c(latex,
             "\\bottomrule",
             "\\end{tabular}",
             "\\end{table}")

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved t-test table to:", output_path, "\n")

  invisible(latex)
}



# =============================================================================
# LATEX TABLE GENERATION FOR PLACKETT-LUCE
# =============================================================================

#' Generate LaTeX table for PlackettLuce models
#'
#' sjPlot does not support PlackettLuce, so this function creates a custom
#' LaTeX table using the worth parameters.
#'
#' @param models List of PlackettLuce models
#' @param model_labels Labels for each model column
#' @param filename Base filename (without .tex)
#' @param table_dir Directory for output
#' @param caption Table caption
#' @param fontsize Font size (default "scriptsize")
#' @return TRUE if successful
generate_plackett_luce_table <- function(models, model_labels, filename, table_dir,
                                         caption, fontsize = "scriptsize",
                                         ref_multiplier = 0) {

  # Create output directory
  tex_dir <- file.path(table_dir, "tex_tables")
  dir.create(tex_dir, recursive = TRUE, showWarnings = FALSE)

  # Get all unique item names (multipliers) - format is "m_X"
  all_item_names <- unique(unlist(lapply(models, function(m) names(coef(m)))))

  # Extract numeric multiplier values and sort
  all_multipliers <- as.numeric(gsub("^m_", "", all_item_names))
  sort_order <- order(all_multipliers)
  all_item_names <- all_item_names[sort_order]
  all_multipliers <- all_multipliers[sort_order]

  n_models <- length(models)
  n_cols <- n_models + 1
  ref_item <- paste0("m_", ref_multiplier)

  # Helper to format p-value stars
  get_stars <- function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return(" \\textsuperscript{***}")
    if (p < 0.01) return(" \\textsuperscript{**}")
    if (p < 0.05) return(" \\textsuperscript{*}")
    return("")
  }

  # Build coefficient rows using qvcalc for CIs
  coef_rows <- list()
  for (item_idx in seq_along(all_item_names)) {
    item_name <- all_item_names[item_idx]
    mult_val <- all_multipliers[item_idx]

    # Format multiplier label
    if (mult_val >= 0) {
      mult_label <- sprintf("$\\lambda$ = +%.1f", mult_val)
    } else {
      mult_label <- sprintf("$\\lambda$ = %.1f", mult_val)
    }

    row_cells <- c(mult_label)

    for (model in models) {
      if (item_name %in% names(coef(model))) {
        # Get qvcalc data
        qv <- qvcalc(model)
        qvf <- qv$qvframe

        # Reparameterize to ref_multiplier as reference
        ref_log_worth <- if (ref_item %in% rownames(qvf)) qvf[ref_item, "estimate"] else 0

        # Shift estimate to new reference (ref_multiplier has log-worth = 0)
        est_raw <- qvf[item_name, "estimate"]
        est <- est_raw - ref_log_worth
        qse <- qvf[item_name, "quasiSE"]
        ci_lower <- est - 1.96 * qse
        ci_upper <- est + 1.96 * qse

        # Z-test for significance (against 0, the reference)
        z <- est / qse
        p <- 2 * pnorm(-abs(z))
        stars <- get_stars(p)

        # Format as makecell with estimate and CI
        # Check if this IS the reference category
        if (item_name == ref_item) {
          cell <- "\\makecell{\\textbf{0.00} \\\\ (ref)}"
        } else {
          cell <- sprintf(
            "\\makecell{\\textbf{%.2f%s} \\\\ (%.2f~--~%.2f)}",
            est, stars, ci_lower, ci_upper
          )
        }
      } else {
        cell <- "~"
      }
      row_cells <- c(row_cells, cell)
    }

    coef_rows[[item_idx]] <- row_cells
  }

  # Build LaTeX content
  col_spec <- paste0("l", paste(rep("c", n_models), collapse = ""))

  latex_lines <- c(
    paste0("\\", fontsize, "{"),
    "\\setlength{\\LTcapwidth}{\\textwidth}",
    "\\renewcommand\\arraystretch{2}",
    "\\setlength\\tabcolsep{0.8mm}",
    paste0("\\begin{longtable}{", col_spec, "}"),
    sprintf("\\caption{\\normalsize{%s}} \\\\", caption),
    "\\hhline{" %+% paste(rep("-", n_cols), collapse = "") %+% "}",
    "\\hhline{" %+% paste(rep("=", n_cols), collapse = "") %+% "}",
    paste0(
      "{\\bfseries ~} & ",
      paste(sprintf("\\multicolumn{1}{c}{\\bfseries %s}", model_labels),
            collapse = " & "),
      " \\\\"
    ),
    "",
    "\\endfirsthead",
    sprintf("\\multicolumn{%d}{c}{\\tablename\\ \\thetable{} -- \\textit{Continued}} \\\\",
            n_cols),
    "\\hhline{" %+% paste(rep("-", n_cols), collapse = "") %+% "}",
    "\\hhline{" %+% paste(rep("=", n_cols), collapse = "") %+% "}",
    paste0(
      "{\\bfseries ~} & ",
      paste(sprintf("\\multicolumn{1}{c}{\\bfseries %s}", model_labels),
            collapse = " & "),
      " \\\\"
    ),
    "\\hline",
    "\\endhead",
    "\\midrule",
    sprintf("\\multicolumn{%d}{r}{\\textit{Continued on next page}} \\\\", n_cols),
    sprintf(
      "\\multicolumn{%d}{r}{\\raggedleft{\\itshape * p{\\textless}0.05~~~** p{\\textless}0.01~~~*** p{\\textless}0.001}} \\\\",
      n_cols
    ),
    "\\endfoot",
    "\\bottomrule",
    sprintf(
      "\\multicolumn{%d}{r}{\\raggedleft{\\itshape * p{\\textless}0.05~~~** p{\\textless}0.01~~~*** p{\\textless}0.001}} \\\\",
      n_cols
    ),
    "\\endlastfoot",
    "\\hline",
    paste0(
      "{\\itshape Multiplier} & ",
      paste(rep("{\\itshape Log-Worth}", n_models), collapse = " & "),
      "\\\\ \\hline"
    ),
    "\\addlinespace[0.5em]"
  )

  # Add coefficient rows
  for (row_cells in coef_rows) {
    latex_lines <- c(latex_lines, paste(row_cells, collapse = " &\n") %+% "\\\\")
  }

  # Add model fit statistics
  latex_lines <- c(latex_lines, " \\hline",
                   sprintf("\\multicolumn{%d}{l}{{\\bfseries Model Fit}} \\\\ \\hline",
                           n_cols))

  # Log-likelihood
  ll_vals <- sapply(models, function(m) sprintf("%.1f", logLik(m)))
  latex_lines <- c(latex_lines,
                   paste(c("Log-Likelihood", ll_vals), collapse = " &\n") %+% "\\\\")

  # AIC
  aic_vals <- sapply(models, function(m) sprintf("%.1f", AIC(m)))
  latex_lines <- c(latex_lines,
                   paste(c("AIC", aic_vals), collapse = " &\n") %+% "\\\\")

  # BIC (calculate manually)
  bic_vals <- sapply(models, function(m) {
    k <- length(coef(m))
    n <- length(m$rankings)
    sprintf("%.1f", -2 * as.numeric(logLik(m)) + k * log(n))
  })
  latex_lines <- c(latex_lines,
                   paste(c("BIC", bic_vals), collapse = " &\n") %+% "\\\\")

  # N observations
  n_vals <- sapply(models, function(m) length(m$rankings))
  latex_lines <- c(latex_lines,
                   paste(c("N (rankings)", n_vals), collapse = " &\n"))

  # Table footer
  latex_lines <- c(latex_lines,
                   sprintf("\\label{tab:%s}", filename),
                   "\\end{longtable}",
                   "}",
                   "\\normalsize"
  )

  # Write file
  output_file <- file.path(tex_dir, paste0(filename, ".tex"))
  writeLines(latex_lines, output_file)

  cat("PlackettLuce table saved to:", output_file, "\n")
  return(invisible(TRUE))
}

#' Generate performance comparison table for calibration models
#'
#' @param comparison_df Performance comparison data frame
#' @param filename Base filename (without .tex)
#' @param table_dir Directory for output
#' @param caption Table caption (optional)
#' @param fontsize Font size (default "scriptsize")
#' @param is_ranking Whether this is for ranking models (affects columns shown)
#' @return TRUE if successful
generate_performance_table_calibration <- function(comparison_df, filename, table_dir,
                                       caption = NULL, fontsize = "scriptsize",
                                       is_ranking = FALSE) {
  # Create output directory
  tex_dir <- file.path(table_dir, "tex_tables")
  dir.create(tex_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(caption)) {
    caption <- "Model performance comparison across specifications"
  }

  # Select columns based on model type
  if (is_ranking) {
    core_cols <- c("Name", "AIC", "AICc", "BIC", "LogLik", "AIC_wt", "AICc_wt", "BIC_wt", "Performance_Score")
  } else {
    core_cols <- c("Name", "RMSE", "AIC_wt", "AICc_wt", "BIC_wt", "Performance_Score")
  }

  # Filter to available columns
  available_cols <- core_cols[core_cols %in% colnames(comparison_df)]
  perf_df <- comparison_df[, available_cols, drop = FALSE]

  # Format numeric columns
  for (col in names(perf_df)) {
    if (is.numeric(perf_df[[col]])) {
      perf_df[[col]] <- sprintf("%.3f", perf_df[[col]])
    }
  }

  # Build LaTeX
  n_cols <- ncol(perf_df)
  col_spec <- paste0("l", paste(rep("c", n_cols - 1), collapse = ""))

  # Header mapping
  header_map <- c(
    "Name" = "Model",
    "RMSE" = "RMSE",
    "AIC" = "AIC",
    "AICc" = "AICc",
    "BIC" = "BIC",
    "LogLik" = "Log-Lik",
    "AIC_wt" = "$w_{AIC}$",
    "AICc_wt" = "$w_{AICc}$",
    "BIC_wt" = "$w_{BIC}$",
    "Performance_Score" = "Perf Score"
  )

  headers <- sapply(names(perf_df), function(x) {
    if (x %in% names(header_map)) header_map[x] else x
  })

  latex_lines <- c(
    paste0("\\", fontsize, "{"),
    paste0("\\begin{longtable}{", col_spec, "}"),
    paste0("\\caption{", caption, "} \\label{tab:", filename, "_perf} \\\\"),
    "\\toprule",
    paste(headers, collapse = " & "),
    " \\\\",
    "\\midrule",
    "\\endfirsthead",
    "",
    "\\bottomrule",
    "\\endlastfoot"
  )

  # Data rows
  for (i in seq_len(nrow(perf_df))) {
    row_vals <- as.character(perf_df[i, ])
    if (i == 1) {
      row_vals <- paste0("\\textbf{", row_vals, "}")
    }
    latex_lines <- c(latex_lines, paste(row_vals, collapse = " & "), " \\\\")
  }

  latex_lines <- c(latex_lines,
    "\\end{longtable}",
    "}",
    "\\normalsize"
  )

  # Write file
  output_file <- file.path(tex_dir, paste0(filename, "_performance.tex"))
  writeLines(latex_lines, output_file)

  cat("Performance table saved to:", output_file, "\n")
  return(invisible(TRUE))
}


# =============================================================================
# DECOUPLING ANALYSIS TABLES
# =============================================================================

#' Create LaTeX summary table for decoupling analysis
#'
#' @param summary_df Data frame with columns: Comparison, Wanting Measure,
#'   Primary p, OR, Sens1 p, Sens2 p, Continuous p, Cohens_d, Primary Sig
#' @param table_dir Output directory
#' @param filename Output filename (without .tex)
#' @param caption Table caption
#' @return Character vector of LaTeX lines (invisible)
create_decoupling_summary_latex <- function(
    summary_df,
    table_dir,
    filename = "decoupling_summary",
    caption = "Decoupling Analysis: Summary of Treatment Effects") {

  if (is.null(summary_df) || nrow(summary_df) == 0) {
    cat("No decoupling results to create table\n")
    return(invisible(NULL))
  }

  # Format p-value helper

  fmt_p <- function(p) {
    if (is.na(p)) return("---")
    if (p < 0.001) return("$<$.001")
    sprintf("%.3f", p)
  }

  # Bold if significant
  fmt_p_bold <- function(p) {
    formatted <- fmt_p(p)
    if (!is.na(p) && p < 0.05) {
      return(paste0("\\textbf{", formatted, "}"))
    }
    formatted
  }

  # Build LaTeX
 latex <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    "\\footnotesize",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{tab:", filename, "}"),
    "\\begin{tabular}{llrrrrrrc}",
    "\\toprule",
    paste0("Comparison & Outcome & $p$ & OR & ",
           "$p_{\\text{sens1}}$ & $p_{\\text{sens2}}$ & ",
           "$p_{\\text{cont}}$ & $d$ & Sig \\\\"),
    "\\midrule"
  )

  # Data rows
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]

    # Format values
    comparison <- gsub("_", " ", row$Comparison)
    wanting <- row$`Wanting Measure`
    p_primary <- fmt_p_bold(row$`Primary p`)
    or_val <- sprintf("%.2f", row$OR)
    p_sens1 <- fmt_p(row$`Sens1 p`)
    p_sens2 <- fmt_p(row$`Sens2 p`)
    p_cont <- fmt_p(row$`Continuous p`)
    d_val <- sprintf("%.2f", row$Cohens_d)
    sig <- ifelse(row$`Primary Sig` == "Yes", "\\checkmark", "")

    latex <- c(latex, sprintf(
      "%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
      comparison, wanting, p_primary, or_val,
      p_sens1, p_sens2, p_cont, d_val, sig
    ))
  }

  # Close table
  latex <- c(latex,
    "\\bottomrule",
    "\\end{tabular}",
    "",
    "\\vspace{2mm}",
    paste0("\\parbox{\\textwidth}{\\scriptsize \\textit{Note.} ",
           "$p$ = primary proportion test; OR = odds ratio for decoupled ",
           "dependency; $p_{\\text{sens1}}$ = excluding near-zero slopes; ",
           "$p_{\\text{sens2}}$ = strong decoupling only; ",
           "$p_{\\text{cont}}$ = continuous t-test on decoupling score; ",
           "$d$ = Cohen's d effect size. Sig = significant at $\\alpha = .05$.}"),
    "\\end{table}"
  )

  # Save to tex_tables subdirectory
  tex_tables_dir <- get_tex_tables_dir(table_dir)
  output_path <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(latex, output_path)
  cat("Saved decoupling summary table to:", output_path, "\n")

  invisible(latex)
}
