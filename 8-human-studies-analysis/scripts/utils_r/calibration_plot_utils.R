# =============================================================================
# Calibration Study Plotting Utilities
# =============================================================================
#
# Visualization functions for calibration study analysis:
# - Rating model effects plots
# - PlackettLuce worth parameter plots
#
# =============================================================================

library(ggplot2)
library(patchwork)
library(ggeffects)

# =============================================================================
# RATING MODEL VISUALIZATION FUNCTIONS
# =============================================================================

#' Create marginal effects plot for a single rating model
#'
#' @param model lmerMod object
#' @param model_type Label for the model type (e.g., "Linear", "Quadratic")
#' @param outcome_label Label for the outcome
#' @param show_ci Whether to show confidence intervals
#' @param y_limits Optional y-axis limits for consistent scaling
#' @return ggplot object
create_rating_effects_plot <- function(model,
                                        model_type = "Linear",
                                        outcome_label = "Predicted Value",
                                        show_ci = TRUE,
                                        y_limits = NULL) {
  # Detect if model uses multiplier_factor (nonlinear) or multiplier (linear/quadratic)
  model_terms <- names(lme4::fixef(model))
  uses_factor <- any(grepl("multiplier_factor", model_terms))

  # Get marginal effects using ggeffects
  if (uses_factor) {
    effects <- ggeffects::ggpredict(model, terms = "multiplier_factor")
    plot_data <- as.data.frame(effects)
    # Convert factor labels back to numeric multiplier values
    label_to_mult <- c("neg1.5" = -1.5, "neg1" = -1, "neg0.5" = -0.5,
                       "zero" = 0, "pos0.5" = 0.5, "pos1" = 1, "pos1.5" = 1.5)
    plot_data$multiplier <- label_to_mult[as.character(plot_data$x)]
  } else {
    effects <- ggeffects::ggpredict(model, terms = "multiplier [all]")
    plot_data <- as.data.frame(effects)
    names(plot_data)[names(plot_data) == "x"] <- "multiplier"
  }

  # Rename columns for consistency
  names(plot_data)[names(plot_data) == "predicted"] <- "estimate"

  # Create plot
  p <- ggplot(plot_data, aes(x = multiplier, y = estimate))

  if (uses_factor) {
    # Nonlinear model: use error bars (discrete points)
    if (show_ci) {
      p <- p + geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                             width = 0.1, color = "#66c2a5", linewidth = 0.8)
    }
    p <- p + geom_point(size = 3, color = "#66c2a5")
  } else {
    # Linear/Quadratic: use ribbon (continuous)
    if (show_ci) {
      p <- p + geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                           alpha = 0.2, fill = "#66c2a5")
    }
    p <- p +
      geom_line(linewidth = 1.2, color = "#66c2a5") +
      geom_point(size = 3, color = "#66c2a5")
  }

  p <- p +
    scale_x_continuous(breaks = seq(-1.5, 1.5, 0.5)) +
    theme_pub() +
    labs(
      title = paste(model_type, "Model"),
      x = "Multiplier (lambda)",
      y = outcome_label
    )

  if (!is.null(y_limits)) {
    p <- p + ylim(y_limits)
  }

  return(p)
}

#' Create all rating effects plots for a set of models
#'
#' @param results List containing models (linear, truncated_linear, etc.)
#' @param outcome_label Label for the outcome
#' @param y_limits Optional y-axis limits. If NULL, calculates shared limits.
#' @return List of ggplot objects
create_all_rating_effects_plots <- function(results,
                                             outcome_label = "Predicted Value",
                                             y_limits = NULL) {
  model_configs <- list(
    list(model = results$linear, type = "Linear"),
    list(model = results$truncated_linear, type = "Truncated Linear"),
    list(model = results$quadratic, type = "Quadratic"),
    list(model = results$truncated_quadratic, type = "Truncated Quadratic"),
    list(model = results$nonlinear, type = "Non-linear")
  )

  # If no y_limits provided, calculate shared limits across all models
  if (is.null(y_limits)) {
    all_predictions <- lapply(model_configs, function(config) {
      model_terms <- names(lme4::fixef(config$model))
      uses_factor <- any(grepl("multiplier_factor", model_terms))
      if (uses_factor) {
        effects <- ggeffects::ggpredict(config$model, terms = "multiplier_factor")
      } else {
        effects <- ggeffects::ggpredict(config$model, terms = "multiplier [all]")
      }
      as.data.frame(effects)
    })

    # Get min/max across all models (including CIs)
    all_lows <- sapply(all_predictions, function(df) min(df$conf.low, na.rm = TRUE))
    all_highs <- sapply(all_predictions, function(df) max(df$conf.high, na.rm = TRUE))
    y_limits <- c(min(all_lows), max(all_highs))
  }

  plots <- purrr::map(model_configs, function(config) {
    create_rating_effects_plot(
      model = config$model,
      model_type = config$type,
      outcome_label = outcome_label,
      y_limits = y_limits
    )
  })

  names(plots) <- c("linear", "truncated_linear", "quadratic",
                    "truncated_quadratic", "nonlinear")

  return(plots)
}

#' Create and save combined rating effects plot using patchwork
#'
#' @param results List containing rating models
#' @param outcome_label Label for the outcome (e.g., "Coherence")
#' @param filename_prefix Prefix for the output filename
#' @param figure_dir Directory to save figures
#' @param save_plot_fn Function to save plots
#' @param y_limits Optional y-axis limits
#' @return Combined ggplot object (invisibly)
create_and_save_rating_effects <- function(results,
                                            outcome_label,
                                            filename_prefix,
                                            figure_dir,
                                            save_plot_fn = NULL,
                                            y_limits = NULL) {
  # Create all plots
  effects <- create_all_rating_effects_plots(results, outcome_label, y_limits)

  # Combine plots using patchwork (2x2 grid for first 4, plus nonlinear below)
  combined_plot <- (effects$linear + effects$truncated_linear) /
    patchwork::plot_spacer() /
    (effects$quadratic + effects$truncated_quadratic) /
    patchwork::plot_spacer() /
    effects$nonlinear +
    patchwork::plot_layout(heights = c(1, 0.1, 1, 0.1, 1.2)) &
    theme(plot.margin = margin(5, 20, 5, 10))

  # Save plot if save function provided
  if (!is.null(save_plot_fn)) {
    filename <- paste0(filename_prefix, "_effects_combined.pdf")
    save_plot_fn(figure_dir, combined_plot, filename, width = 12, height = 14)
  }

  return(invisible(combined_plot))
}

# =============================================================================
# PLACKETT-LUCE VISUALIZATION FUNCTIONS
# =============================================================================

#' Create worth parameter plot for a PlackettLuce model
#'
#' Uses point estimates with error bars (not ribbon) since PlackettLuce
#' estimates discrete item worths, not a continuous function.
#'
#' @param model PlackettLuce model object
#' @param model_type Label for the model type (e.g., "Full", "Truncated")
#' @param outcome_label Label for the outcome (e.g., "Preference")
#' @param ref_multiplier Reference multiplier for log-worth = 0 (default 0)
#' @param color Color for points and error bars
#' @return ggplot object
create_plackett_luce_worth_plot <- function(model,
                                             model_type = "Full",
                                             outcome_label = "Worth",
                                             ref_multiplier = 0,
                                             color = "#66c2a5") {
  # Use qvcalc for proper quasi-variance comparison intervals
  qv <- qvcalc(model)
  qvf <- qv$qvframe

  # Extract multiplier values from rownames (format: "m_X" where X is multiplier)
  multiplier_vals <- as.numeric(gsub("^m_", "", rownames(qvf)))

  # Reparameterize to set ref_multiplier as reference (log-worth = 0)
  ref_item <- paste0("m_", ref_multiplier)
  ref_log_worth <- if (ref_item %in% rownames(qvf)) qvf[ref_item, "estimate"] else 0

  # Create data frame with log-worth relative to new reference
  plot_data <- data.frame(
    multiplier = multiplier_vals,
    log_worth = qvf$estimate - ref_log_worth,  # Shift to new reference
    quasi_se = qvf$quasiSE
  ) %>%
    dplyr::mutate(
      ci_lower = log_worth - 1.96 * quasi_se,
      ci_upper = log_worth + 1.96 * quasi_se
    ) %>%
    dplyr::arrange(multiplier)

  # Create plot with point estimates and error bars (discrete, not continuous)
  p <- ggplot(plot_data, aes(x = multiplier, y = log_worth)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "gray50", alpha = 0.7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.1, color = color, linewidth = 0.8) +
    geom_point(size = 3, color = color) +
    scale_x_continuous(breaks = sort(unique(plot_data$multiplier))) +
    theme_pub() +
    labs(
      title = model_type,
      subtitle = outcome_label,
      x = "Multiplier",
      y = paste0("Log-worth (ref: lambda = ", ref_multiplier, ")")
    )

  return(p)
}

#' Create PlackettLuce worth plots for full and truncated models
#'
#' @param results List containing $full and $truncated models
#' @param outcome_label Label for the outcome
#' @param ref_multiplier Reference multiplier for log-worth = 0
#' @param color Color for points and error bars
#' @return List with $full and $truncated ggplot objects
create_plackett_luce_plots <- function(results,
                                        outcome_label = "Worth",
                                        ref_multiplier = 0,
                                        color = "#66c2a5") {
  plots <- list(
    full = create_plackett_luce_worth_plot(
      model = results$full,
      model_type = "Full Model",
      outcome_label = outcome_label,
      ref_multiplier = ref_multiplier,
      color = color
    ),
    truncated = create_plackett_luce_worth_plot(
      model = results$truncated,
      model_type = "Truncated (excl. +/-1.5)",
      outcome_label = outcome_label,
      ref_multiplier = ref_multiplier,
      color = color
    )
  )

  return(plots)
}

#' Create and save combined PlackettLuce plot (full + truncated side by side)
#'
#' @param results List containing $full and $truncated models
#' @param outcome_label Label for the outcome
#' @param filename_prefix Prefix for output filename
#' @param figure_dir Directory for figures
#' @param save_plot_fn Function to save plots
#' @param ref_multiplier Reference multiplier
#' @param color Color for points and error bars
#' @return Combined ggplot (invisibly)
create_and_save_plackett_luce_plots <- function(results,
                                                 outcome_label,
                                                 filename_prefix,
                                                 figure_dir,
                                                 save_plot_fn = NULL,
                                                 ref_multiplier = 0,
                                                 color = "#66c2a5") {
  # Create both plots
  plots <- create_plackett_luce_plots(results, outcome_label, ref_multiplier,
                                      color = color)

  # Combine side by side using patchwork
  combined_plot <- plots$full + plots$truncated +
    patchwork::plot_layout(ncol = 2) &
    theme(plot.margin = margin(5, 15, 5, 10))

  # Save if function provided
  if (!is.null(save_plot_fn)) {
    filename <- paste0("plackett_luce_", filename_prefix, ".pdf")
    save_plot_fn(figure_dir, combined_plot, filename, width = 12, height = 5)
  }

  return(invisible(combined_plot))
}
