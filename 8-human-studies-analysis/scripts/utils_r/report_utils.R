# =============================================================================
# Report Utilities
# =============================================================================
#
# Shared functions for generating markdown report sections.
#
# =============================================================================

#' Generate Data Summary as Markdown
#'
#' Summarizes observations and participants by outcome and study type.
#'
#' @param data_cs_pooled Cross-sectional pooled data (with construct column)
#' @param data_long_pooled Longitudinal pooled data (with construct column)
#' @return Character vector of markdown lines
generate_data_summary_md <- function(data_cs_pooled, data_long_pooled) {
  lines <- c("## Data Summary", "")

  outcomes <- unique(data_cs_pooled$construct)

  for (outcome in outcomes) {
    outcome_label <- gsub("_", " ", tools::toTitleCase(outcome))
    lines <- c(lines, paste0("### ", outcome_label), "")

    # Cross-sectional
    cs_subset <- data_cs_pooled %>% filter(construct == outcome)
    lines <- c(lines, paste0(
      "- **Cross-sectional**: ", nrow(cs_subset), " obs / ",
      n_distinct(cs_subset$ppt_id), " participants"
    ))

    # Longitudinal
    long_subset <- data_long_pooled %>% filter(construct == outcome)
    lines <- c(lines, paste0(
      "- **Longitudinal**: ", nrow(long_subset), " obs / ",
      n_distinct(long_subset$ppt_id), " participants"
    ))

    lines <- c(lines, "")
  }

  c(lines, "---", "")
}


#' Generate Full-Model Specification Comparison as Markdown
#'
#' @param full_spec_cs List of cross-sectional spec comparison data frames
#' @param full_spec_long List of longitudinal spec comparison data frames
#' @param outcomes Character vector of outcome names
#' @return Character vector of markdown lines
generate_full_spec_comparison_md <- function(full_spec_cs, full_spec_long, outcomes) {
  lines <- c(
    "## Full-Model Specification Comparison",
    "",
    "Performance comparison across the three full interaction",
    "specifications (continuous, coarsened, factor $\\lambda$).",
    ""
  )

  for (study_label in c("Cross-Sectional", "Longitudinal")) {
    spec_data <- if (study_label == "Cross-Sectional") full_spec_cs else full_spec_long
    lines <- c(lines,
      paste0("### ", study_label), "",
      "| Outcome | Model | RMSE | AIC wt | AICc wt | BIC wt | Perf. Score |",
      "|---------|-------|------|--------|---------|--------|-------------|"
    )

    for (outcome in outcomes) {
      df <- spec_data[[outcome]]
      outcome_label <- gsub("_", " ", tools::toTitleCase(outcome))
      if (!is.null(df) && nrow(df) > 0) {
        for (r in seq_len(nrow(df))) {
          outcome_cell <- if (r == 1) outcome_label else ""
          fmt <- function(col) {
            if (col %in% names(df)) sprintf("%.3f", df[[col]][r]) else "---"
          }
          lines <- c(lines, sprintf(
            "| %s | %s | %s | %s | %s | %s | %s |",
            outcome_cell, df$Name[r],
            fmt("RMSE"), fmt("AIC_wt"), fmt("AICc_wt"),
            fmt("BIC_wt"), fmt("Performance_Score")
          ))
        }
      }
    }
    lines <- c(lines, "")
  }

  lines <- c(lines, "Full comparison tables exported to LaTeX.", "", "---", "")
  lines
}

#' Generate Best Specifications as Markdown
#'
#' @param best_specs_cs Data frame of cross-sectional best specs
#' @param best_specs_long Data frame of longitudinal best specs
#' @return Character vector of markdown lines
generate_best_specs_md <- function(best_specs_cs, best_specs_long) {
  lines <- c(
    "## Functional Form Comparison",
    "",
    "Best specification (linear, quadratic, cubic) selected by AIC.",
    "",
    "### Cross-Sectional",
    "",
    "| Outcome | Best Spec |",
    "|---------|-----------|"
  )

  for (i in seq_len(nrow(best_specs_cs))) {
    lines <- c(lines, sprintf(
      "| %s | %s |",
      best_specs_cs$outcome[i],
      best_specs_cs$best_spec[i]
    ))
  }

  lines <- c(lines, "", "### Longitudinal", "",
             "| Outcome | Best Spec |",
             "|---------|-----------|")

  for (i in seq_len(nrow(best_specs_long))) {
    lines <- c(lines, sprintf(
      "| %s | %s |",
      best_specs_long$outcome[i],
      best_specs_long$best_spec[i]
    ))
  }

  lines <- c(lines,
    "",
    "Full comparison tables (RMSE, AIC weights, BIC weights,",
    "Performance Score) exported to LaTeX.",
    "",
    "---",
    ""
  )
  lines
}

#' Generate Heterogeneity Tests Summary as Markdown
#'
#' @param heterogeneity_results List of heterogeneity test results
#' @return Character vector of markdown lines
generate_heterogeneity_md <- function(heterogeneity_results) {
  lines <- c(
    "## Heterogeneity Tests",
    "",
    "Tests whether outcomes within pooled constructs show different",
    "treatment effects. A significant test suggests heterogeneity.",
    "",
    "| Construct | Study Type | p-value | Result |",
    "|-----------|------------|---------|--------|"
  )

  for (key in names(heterogeneity_results)) {
    hr <- heterogeneity_results[[key]]
    result_str <- ifelse(hr$heterogeneous, "HETEROGENEOUS", "HOMOGENEOUS")
    study_label <- ifelse(hr$study_type == "cross_sectional",
                          "Cross-Sectional", "Longitudinal")
    lines <- c(lines, sprintf("| %s | %s | %.4f | %s |",
                              hr$construct, study_label, hr$p_value, result_str))
  }

  c(lines, "", "---", "")
}


#' Generate Robustness Analysis Summary as Markdown
#'
#' Summarizes robustness checks matching the LaTeX table format.
#' Shows coefficient (SE) with significance stars for each specification.
#'
#' @param robustness_cs Robustness analysis results for cross-sectional
#' @param robustness_long Robustness analysis results for longitudinal
#' @param outcome_vars Character vector of outcome variable names
#' @param domain_name Character name of the domain (e.g., "preferences")
#' @return Character vector of markdown lines
generate_robustness_summary_md <- function(robustness_cs, robustness_long,
                                           outcome_vars, domain_name) {
  lines <- c(
    "## Robustness Checks",
    "",
    "Robustness analyses test whether treatment effects hold under",
    "alternative specifications. Cells show coefficient (SE) with",
    "significance: *p<.05, **p<.01, ***p<.001.",
    "",
    "**Specifications:**",
    "- **Additive**: Base treatment effects (no interactions)",
    "- **+ Interactions**: Full model with treatment interactions",
    "- **Full + Demos**: Full model + demographic controls",
    "- **Full + Prefs**: Full model + AI pre-treatment pref groups",
    "- **Full + IPW**: Full model with IPW weights (attrition adjustment)",
    ""
  )

  # Helper to format coefficient with significance

  format_coeff_md <- function(coeff, se, p) {
    if (is.na(coeff) || is.na(se)) return("---")
    stars <- if (is.na(p)) "" else if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
    sprintf("%.2f (%.2f)%s", coeff, se, stars)
  }

  # Helper to generate table for a study type
  generate_study_table <- function(robustness_result, study_label) {
    study_lines <- c(paste0("### ", study_label), "")

    if (!is.null(robustness_result) && !is.null(robustness_result$coeffs_wide)) {
      wide_df <- robustness_result$coeffs_wide

      # Create clean summary table with formatted coefficients
      # Header row with nice column names
      header <- "| Outcome | Predictor | Additive | + Interactions | Full + Demos | Full + Prefs | Full + IPW |"
      sep <- "|---|---|---|---|---|---|---|"
      study_lines <- c(study_lines, header, sep)

      # Data rows
      for (i in seq_len(nrow(wide_df))) {
        row <- wide_df[i, ]

        # Format each coefficient column
        additive_fmt <- format_coeff_md(row$additive_coeff, row$additive_se, row$additive_p)
        full_fmt <- format_coeff_md(row$full_coeff, row$full_se, row$full_p)
        demos_fmt <- format_coeff_md(row$demos_coeff, row$demos_se, row$demos_p)
        prefs_fmt <- format_coeff_md(row$prefs_coeff, row$prefs_se, row$prefs_p)
        weighted_fmt <- format_coeff_md(row$weighted_coeff, row$weighted_se, row$weighted_p)

        row_str <- sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                          as.character(row$outcome),
                          as.character(row$variable),
                          additive_fmt, full_fmt, demos_fmt, prefs_fmt, weighted_fmt)
        study_lines <- c(study_lines, row_str)
      }

    } else if (!is.null(robustness_result) && !is.null(robustness_result$coeffs_long)) {
      # Fall back to long format summary
      study_lines <- c(study_lines,
        "See LaTeX tables for detailed coefficient estimates.")
    } else {
      study_lines <- c(study_lines, "See LaTeX tables for detailed results.")
    }

    c(study_lines, "")
  }

  lines <- c(lines,
    generate_study_table(robustness_cs, "Cross-Sectional"),
    generate_study_table(robustness_long, "Longitudinal")
  )

  lines <- c(lines,
    paste0("Full tables: `", domain_name, "_robustness_{cs,long}.tex`"),
    "",
    "---",
    ""
  )

  lines
}

#' Generate Pairwise Comparisons Summary as Markdown
#'
#' Formats pairwise comparison results (emmeans) for the markdown report.
#'
#' @param pairwise_results List with cross_sectional and longitudinal results
#' @param outcome_var Outcome variable name
#' @return Character vector of markdown lines
generate_pairwise_md <- function(pairwise_results, outcome_var) {
  lines <- c(
    "## Pairwise Comparisons",
    "",
    "Estimated marginal means and pairwise contrasts (FDR-adjusted).",
    ""
  )

  for (study_type in c("cross_sectional", "longitudinal")) {
    study_label <- ifelse(study_type == "cross_sectional",
                          "Cross-Sectional", "Longitudinal")
    lines <- c(lines, paste0("### ", study_label), "")

    if (!is.null(pairwise_results[[study_type]])) {
      result <- pairwise_results[[study_type]]

      # Marginal means table
      if (!is.null(result$marginal_means)) {
        mm <- result$marginal_means
        lines <- c(lines, "**Marginal Means:**", "")

        # Check column names - emmeans output varies
        if ("prob" %in% names(mm)) {
          lines <- c(lines,
            "| Lambda | Prob | SE | 95% CI |",
            "|--------|------|-----|--------|"
          )
          for (i in seq_len(nrow(mm))) {
            ci <- sprintf("[%.3f, %.3f]",
              mm$asymp.LCL[i], mm$asymp.UCL[i])
            lines <- c(lines, sprintf("| %s | %.3f | %.3f | %s |",
              mm$lambda_factor[i], mm$prob[i], mm$SE[i], ci))
          }
        } else if ("emmean" %in% names(mm)) {
          lines <- c(lines,
            "| Lambda | Mean | SE | 95% CI |",
            "|--------|------|-----|--------|"
          )
          for (i in seq_len(nrow(mm))) {
            ci <- sprintf("[%.2f, %.2f]", mm$lower.CL[i], mm$upper.CL[i])
            lines <- c(lines, sprintf("| %s | %.2f | %.2f | %s |",
              mm$lambda_factor[i], mm$emmean[i], mm$SE[i], ci))
          }
        }
        lines <- c(lines, "")
      }

      # Pairwise contrasts table
      if (!is.null(result$pairwise)) {
        pw <- result$pairwise
        lines <- c(lines, "**Pairwise Contrasts (FDR-adjusted):**", "")

        if ("odds.ratio" %in% names(pw)) {
          lines <- c(lines,
            "| Contrast | OR | SE | z | p |",
            "|----------|-----|-----|-----|-----|"
          )
          for (i in seq_len(nrow(pw))) {
            sig <- ifelse(pw$p.value[i] < 0.001, "***",
                   ifelse(pw$p.value[i] < 0.01, "**",
                   ifelse(pw$p.value[i] < 0.05, "*", "")))
            lines <- c(lines, sprintf("| %s | %.2f | %.2f | %.2f | %.3f%s |",
              pw$contrast[i], pw$odds.ratio[i], pw$SE[i],
              pw$z.ratio[i], pw$p.value[i], sig))
          }
        } else if ("estimate" %in% names(pw)) {
          lines <- c(lines,
            "| Contrast | Est | SE | t | p |",
            "|----------|-----|-----|-----|-----|"
          )
          for (i in seq_len(nrow(pw))) {
            sig <- ifelse(pw$p.value[i] < 0.001, "***",
                   ifelse(pw$p.value[i] < 0.01, "**",
                   ifelse(pw$p.value[i] < 0.05, "*", "")))
            t_or_z <- if ("t.ratio" %in% names(pw)) pw$t.ratio[i] else pw$z.ratio[i]
            lines <- c(lines, sprintf("| %s | %.2f | %.2f | %.2f | %.3f%s |",
              pw$contrast[i], pw$estimate[i], pw$SE[i],
              t_or_z, pw$p.value[i], sig))
          }
        }
        lines <- c(lines, "")
      }
    } else {
      lines <- c(lines, "No pairwise results available.", "")
    }
  }

  lines <- c(lines, "*p < .05, **p < .01, ***p < .001 (FDR-corrected)", "", "---", "")
  lines
}

#' Format treatment test results as markdown table
#'
#' @param test_results Results from run_treatment_tests or run_standard_treatment_tests
#' @param data_by_study Optional named list of data frames by study type
#'   (e.g., list(cross_sectional = df_cs, longitudinal = df_long)).
#'   If provided, contingency tables with percentages are shown for significant results.
#' @return Character vector of markdown lines
format_treatment_tests_md <- function(test_results, data_by_study = NULL) {
  lines <- c(
    "## Treatment Association Tests",
    "",
    "Chi-squared tests of independence between treatment variables and outcomes.",
    "*p < .05, **p < .01, ***p < .001. † indicates expected cell count < 5.",
    ""
  )

  for (study_type in names(test_results)) {
    study_label <- switch(study_type,
      "cross_study" = "Cross-Study Comparison",
      "cross_sectional" = "Cross-Sectional",
      "longitudinal" = "Longitudinal",
      study_type
    )
    lines <- c(lines, paste0("### ", study_label), "")

    study_results <- test_results[[study_type]]
    study_data <- data_by_study[[study_type]]

    for (outcome in names(study_results)) {
      lines <- c(lines, paste0("**", outcome, ":**"), "")

      outcome_results <- study_results[[outcome]]
      for (res in outcome_results) {
        if (!is.null(res) && is.list(res)) {
          sig <- !is.na(res$p_value) && res$p_value < 0.05
          sig_label <- if (sig) " (SIGNIFICANT)" else ""

          lines <- c(lines,
            sprintf("- %s: χ² = %.2f, df = %d, p = %.4f%s",
              res$treatment,
              ifelse(is.na(res$chi_sq), NA, res$chi_sq),
              ifelse(is.na(res$df), NA, as.integer(res$df)),
              ifelse(is.na(res$p_value), NA, res$p_value),
              sig_label))

          # Add contingency table for significant results if data provided
          if (sig && !is.null(study_data)) {
            trt_var <- res$treatment
            contingency_lines <- format_contingency_table_md(
              study_data, outcome, trt_var)
            lines <- c(lines, contingency_lines)
          }
        }
      }
      lines <- c(lines, "")
    }
  }

  lines <- c(lines, "---", "")
  lines
}

#' Format contingency table as markdown
#'
#' Helper function to create a markdown table showing percentages
#' for each treatment group by outcome category.
#'
#' @param data Data frame
#' @param outcome_var Name of outcome variable
#' @param treatment_var Name of treatment variable
#' @return Character vector of markdown lines
format_contingency_table_md <- function(data, outcome_var, treatment_var) {
  # Calculate percentages
  pct_data <- data %>%
    filter(!is.na(.data[[outcome_var]]) & !is.na(.data[[treatment_var]])) %>%
    group_by(.data[[treatment_var]], .data[[outcome_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(.data[[treatment_var]]) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()

  if (nrow(pct_data) == 0) {
    return(character(0))
  }

  # Get unique categories
  outcome_cats <- unique(pct_data[[outcome_var]])
  treatment_groups <- unique(pct_data[[treatment_var]])

  # Create table header
  header <- paste0("| ", treatment_var, " | ",
    paste(outcome_cats, collapse = " | "), " |")
  sep_line <- paste0("|", paste(rep("---", length(outcome_cats) + 1),
    collapse = "|"), "|")

  lines <- c("", header, sep_line)

  # Create rows

  for (grp in treatment_groups) {
    grp_data <- pct_data %>% filter(.data[[treatment_var]] == grp)
    row_vals <- sapply(outcome_cats, function(cat_val) {
      val_row <- grp_data %>% filter(.data[[outcome_var]] == cat_val)
      if (nrow(val_row) > 0) {
        sprintf("%.1f%% (n=%d)", val_row$pct[1], val_row$n[1])
      } else {
        "0% (n=0)"
      }
    })
    lines <- c(lines, sprintf("| %s | %s |", grp, paste(row_vals, collapse = " | ")))
  }

  c(lines, "")
}

#' Format Logistic Regression as Odds Ratios Markdown
#'
#' Formats a logistic regression model's coefficients as odds ratios
#' for inclusion in a markdown report.
#'
#' @param model Fitted glm model (binomial family)
#' @param model_params Data frame from model_parameters with p_adjusted column
#' @return Character vector of markdown lines
format_logistic_or_md <- function(model, model_params) {
  # Formula
  formula_str <- paste(deparse(formula(model)), collapse = " ")
  lines <- c(paste0("`", formula_str, "`"), "")

  # Get centralized labels for parameter name cleaning
  sociodemo_labels <- get_sociodemo_labels()

  # Create odds ratio table with cleaned parameter names
  or_table <- model_params %>%
    dplyr::mutate(
      # Clean parameter names using centralized labels
      Parameter_clean = dplyr::case_when(
        Parameter %in% names(sociodemo_labels) ~ sociodemo_labels[Parameter],
        TRUE ~ Parameter
      ),
      OR = exp(Coefficient),
      OR_low = exp(Coefficient - 1.96 * SE),
      OR_high = exp(Coefficient + 1.96 * SE),
      OR_formatted = sprintf("%.2f%s", OR, significance),
      CI_formatted = sprintf("[%.2f, %.2f]", OR_low, OR_high)
    )

  lines <- c(lines,
    "| Parameter | Odds Ratio | 95% CI | P-value |",
    "|-----------|------------|--------|---------|"
  )

  for (i in seq_len(nrow(or_table))) {
    p_val <- if ("p" %in% names(or_table)) or_table$p[i] else or_table$p_adjusted[i]
    lines <- c(lines, sprintf(
      "| %s | %s | %s | %.3f |",
      or_table$Parameter_clean[i],
      or_table$OR_formatted[i],
      or_table$CI_formatted[i],
      p_val
    ))
  }

  c(lines, "")
}


#' Generate Output Files Section as Markdown
#'
#' @param domain_name Character name of the domain
#' @return Character vector of markdown lines
generate_output_files_md <- function(task_name) {
  c(
    "## Output Files",
    "",
    sprintf("All outputs use prefix `%s_`.", task_name),
    "",
    "- Figures: `outputs/figures/main_studies/`",
    "- Tables: `outputs/tables/main_studies/`",
    "- Models: `outputs/models/`"
  )
}


# =============================================================================
# Model Coefficient Markdown Functions
# =============================================================================

#' Format coefficient table as markdown string
#'
#' @param model Fitted model object
#' @param outcome_name Name of the outcome variable
#' @param study_type "Cross-Sectional" or "Longitudinal"
#' @return Character vector of markdown lines
format_coefficients_md <- function(model, outcome_name, study_type = "") {
  coeffs <- extract_all_coefficients(model, outcome_name)
  if (is.null(coeffs) || nrow(coeffs) == 0) {
    return(paste0("*No coefficients found for ", outcome_name, "*\n"))
  }

  # Check if model is binary (use odds ratios)
  is_binary <- inherits(model, "glm") &&
    model$family$family %in% c("binomial", "quasibinomial")
  coef_label <- if (is_binary) "Odds Ratio" else "Coefficient"

  # Format formula
  formula_str <- paste(deparse(formula(model)), collapse = " ")

  lines <- c()
  if (study_type != "") {
    lines <- c(lines, paste0("**", study_type, " Model:**"))
  }
  lines <- c(lines, paste0("`", formula_str, "`"), "")

  # Create markdown table
  table_df <- coeffs %>%
    dplyr::select(Parameter_clean, coeff_formatted,
                  ci_formatted, p_raw_formatted) %>%
    dplyr::rename(
      "Parameter" = Parameter_clean,
      !!coef_label := coeff_formatted,
      "95% CI" = ci_formatted,
      "P-value" = p_raw_formatted
    )

  table_md <- knitr::kable(table_df, format = "markdown", align = "lcccc")
  lines <- c(lines, table_md, "")

  return(lines)
}


#' Generate full model coefficients section as markdown
#'
#' @param mods_cross_sectional List of cross-sectional models by outcome
#' @param mods_longitudinal List of longitudinal models by outcome
#' @param outcome_vars Vector of outcome variable names
#' @return Character vector of markdown lines
generate_coefficients_md <- function(mods_cross_sectional,
                                      mods_longitudinal,
                                      outcome_vars) {

  lines <- c(
    "## Model Coefficients",
    "",
    "Fixed effects from fitted models. Binary outcomes show odds ratios.",
    ""
  )

  model_specs <- c("additive_coarsened", "additive_continuous", "full_coarsened", "full_continuous")
  spec_labels <- c(
    "additive_coarsened" = "Additive (lambda_3 Coarsened)",
    "additive_continuous" = "Additive (lambda Continuous)",
    "full_coarsened" = "Full/Interaction (lambda_3 Coarsened)",
    "full_continuous" = "Full/Interaction (lambda Continuous)"
  )

  for (outcome in outcome_vars) {
    lines <- c(lines,
               paste0("### ", tools::toTitleCase(outcome)),
               "")

    for (spec in model_specs) {
      lines <- c(lines,
                 paste0("#### ", spec_labels[spec]),
                 "")

      # Cross-sectional
      if (!is.null(mods_cross_sectional[[outcome]][[spec]])) {
        lines <- c(lines, format_coefficients_md(
          mods_cross_sectional[[outcome]][[spec]],
          outcome,
          "Cross-Sectional"
        ))
      }

      # Longitudinal
      if (!is.null(mods_longitudinal[[outcome]][[spec]])) {
        lines <- c(lines, format_coefficients_md(
          mods_longitudinal[[outcome]][[spec]],
          outcome,
          "Longitudinal"
        ))
      }
    }

    lines <- c(lines, "---", "")
  }

  return(lines)
}

# =============================================================================
# VULNERABILITY ANALYSIS MARKDOWN FUNCTIONS
# =============================================================================

#' Generate markdown table for vulnerability main effects
#'
#' @param main_effects_df Data frame with main effects results
#' @return Character vector of markdown lines
generate_vulnerability_main_effects_md <- function(main_effects_df) {
  lines <- c(
    "## Main Effects",
    "",
    "Main effects of participant characteristics on outcomes.",
    "These show whether characteristics predict outcomes regardless of condition.",
    ""
  )

  if (is.null(main_effects_df) || nrow(main_effects_df) == 0) {
    return(c(lines, "*No main effects results available.*", "", "---", ""))
  }

  # Get predefined labels
  pred_labels <- get_sociodemo_labels()

  # Filter to significant (p_within)
  sig_effects <- main_effects_df %>%
    filter(p_within < 0.05) %>%
    arrange(p_within)

  if (nrow(sig_effects) == 0) {
    return(c(lines, "*No significant main effects at p_within < 0.05.*", "", "---", ""))
  }

  # Count significant at both levels
  n_sig_within <- sum(main_effects_df$p_within < 0.05, na.rm = TRUE)
  n_sig_global <- sum(main_effects_df$p_global < 0.05, na.rm = TRUE)
  n_total <- nrow(main_effects_df)

  lines <- c(lines,
    sprintf("**Significant effects (p_within < 0.05): %d / %d tests**", n_sig_within, n_total),
    sprintf("**Significant effects (p_global < 0.05): %d / %d tests**", n_sig_global, n_total),
    "",
    "| Outcome | Predictor | Estimate [95% CI] | p (raw) | p (within) | p (global) |",
    "|---------|-----------|-------------------|---------|------------|------------|"
  )

  for (i in seq_len(nrow(sig_effects))) {
    row <- sig_effects[i, ]
    # Format outcome using labelling function
    outcome_label <- format_outcome_label(row$outcome)
    # Format predictor using predefined labels
    pred_label <- ifelse(row$predictor %in% names(pred_labels),
                         pred_labels[row$predictor], row$predictor)
    est_ci <- sprintf("%.2f [%.2f, %.2f]", row$estimate, row$ci_low, row$ci_high)
    p_raw_fmt <- ifelse(row$p_raw < 0.001, "<.001", sprintf("%.3f", row$p_raw))
    p_within_fmt <- ifelse(row$p_within < 0.001, "<.001", sprintf("%.3f", row$p_within))
    p_global_fmt <- ifelse(row$p_global < 0.001, "<.001", sprintf("%.3f", row$p_global))
    # Bold p_global if significant
    if (row$p_global < 0.05) {
      p_global_fmt <- paste0("**", p_global_fmt, "**")
    }
    lines <- c(lines, sprintf(
      "| %s | %s | %s | %s | %s | %s |",
      outcome_label, pred_label, est_ci, p_raw_fmt, p_within_fmt, p_global_fmt
    ))
  }

  c(lines, "", "---", "")
}


#' Generate markdown table for vulnerability interaction effects
#'
#' @param interactions_df Data frame with interaction results
#' @return Character vector of markdown lines
generate_vulnerability_interactions_md <- function(interactions_df) {
  lines <- c(
    "## Interaction Effects",
    "",
    "Interaction effects: companionship_condition x participant characteristics.",
    "These identify vulnerable subgroups who respond differently to the companionship condition.",
    ""
  )

  if (is.null(interactions_df) || nrow(interactions_df) == 0) {
    return(c(lines, "*No interaction results available.*", "", "---", ""))
  }

  # Get predefined labels
  pred_labels <- get_sociodemo_labels()

  # Filter to significant (p_within)
  sig_effects <- interactions_df %>%
    filter(p_within < 0.05) %>%
    arrange(p_within)

  if (nrow(sig_effects) == 0) {
    return(c(lines, "*No significant interactions at p_within < 0.05.*", "", "---", ""))
  }

  # Count significant at both levels
  n_sig_within <- sum(interactions_df$p_within < 0.05, na.rm = TRUE)
  n_sig_global <- sum(interactions_df$p_global < 0.05, na.rm = TRUE)
  n_total <- nrow(interactions_df)

  lines <- c(lines,
    sprintf("**Significant interactions (p_within < 0.05): %d / %d tests**", n_sig_within, n_total),
    sprintf("**Significant interactions (p_global < 0.05): %d / %d tests**", n_sig_global, n_total),
    "",
    "| Outcome | Interaction Term | Estimate [95% CI] | p (raw) | p (within) | p (global) |",
    "|---------|------------------|-------------------|---------|------------|------------|"
  )

  for (i in seq_len(nrow(sig_effects))) {
    row <- sig_effects[i, ]
    # Format outcome using labelling function
    outcome_label <- format_outcome_label(row$outcome)
    # Format interaction term - extract predictor and apply label
    int_term <- row$interaction_term
    # Try to extract base predictor from interaction term (format: companionship_condition:predictor)
    base_pred <- gsub("^companionship_condition:", "", int_term)
    if (base_pred %in% names(pred_labels)) {
      int_term_label <- paste0("Companion × ", pred_labels[base_pred])
    } else {
      int_term_label <- int_term
    }
    est_ci <- sprintf("%.2f [%.2f, %.2f]", row$estimate, row$ci_low, row$ci_high)
    p_raw_fmt <- ifelse(row$p_raw < 0.001, "<.001", sprintf("%.3f", row$p_raw))
    p_within_fmt <- ifelse(row$p_within < 0.001, "<.001", sprintf("%.3f", row$p_within))
    p_global_fmt <- ifelse(row$p_global < 0.001, "<.001", sprintf("%.3f", row$p_global))
    # Bold p_global if significant
    if (row$p_global < 0.05) {
      p_global_fmt <- paste0("**", p_global_fmt, "**")
    }
    lines <- c(lines, sprintf(
      "| %s | %s | %s | %s | %s | %s |",
      outcome_label, int_term_label, est_ci, p_raw_fmt, p_within_fmt, p_global_fmt
    ))
  }

  c(lines, "", "---", "")
}


#' Compare n significant at each correction level
#'
#' @param df Data frame with p_raw, p_within, p_global columns
#' @param test_type Label for the type of test (e.g., "Main Effects", "Interactions")
#' @return Character vector of markdown lines
generate_vulnerability_correction_comparison_md <- function(df, test_type) {
  lines <- c(
    sprintf("## Correction Level Comparison: %s", test_type),
    ""
  )

  if (is.null(df) || nrow(df) == 0) {
    return(c(lines, "*No data available.*", "", "---", ""))
  }

  n_total <- nrow(df)
  n_raw <- sum(df$p_raw < 0.05, na.rm = TRUE)
  n_within <- sum(df$p_within < 0.05, na.rm = TRUE)
  n_global <- sum(df$p_global < 0.05, na.rm = TRUE)

  lines <- c(lines,
    "| Correction Level | N Significant | Percentage |",
    "|------------------|---------------|------------|",
    sprintf("| Uncorrected (p < 0.05) | %d | %.1f%% |", n_raw, 100 * n_raw / n_total),
    sprintf("| Within-outcome FDR | %d | %.1f%% |", n_within, 100 * n_within / n_total),
    sprintf("| Global FDR | %d | %.1f%% |", n_global, 100 * n_global / n_total),
    "",
    sprintf("*Total tests: %d*", n_total),
    "",
    "---",
    ""
  )

  lines
}
