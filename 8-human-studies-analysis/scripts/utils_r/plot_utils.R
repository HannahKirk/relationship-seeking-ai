# plot_utils.R
# Reusable plotting functions for R analyses

library(tidyverse)
library(patchwork)
library(pheatmap)
library(RColorBrewer)

# =============================================================================
# DISTRIBUTION PLOTS
# =============================================================================

#' Create boxplot comparing outcome variables by study
#'
#' @param data Data frame
#' @param outcome_vars Character vector of outcome variable names
#' @param nrow Number of legend rows
#' @return ggplot object
boxplot_by_study <- function(data, outcome_vars, nrow = 1) {
  # Reshape to long format
  data_long <- data %>%
    select(ppt_id, study_id, all_of(outcome_vars)) %>%
    pivot_longer(
      cols = all_of(outcome_vars),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(variable_clean = clean_var_vector(variable, add_newlines = TRUE))

  # Create plot
  ggplot(data_long, aes(x = variable_clean, y = value, fill = study_id)) +
    geom_boxplot(position = position_dodge(width = 0.8), alpha = 0.7) +
    scale_fill_study() +
    labs(x = "", y = "Rating", fill = "Study") +
    theme_pub() +
    guides(fill = guide_legend(nrow = nrow))
}


# =============================================================================
# TRAJECTORY PLOTS
# =============================================================================

#' Create trajectory plot showing outcome changes over time
#'
#' @param data Data frame with timepoint data
#' @param outcome_vars Character vector of outcome variable names
#' @param split_by_multiplier Whether to facet by multiplier
#' @return ggplot object
plot_trajectory <- function(data, outcome_vars, split_by_multiplier = FALSE) {
  # Prepare data for trajectory plot
  trajectory_data <- data %>%
    filter(
      (study_id == "cross-sectional" & timepoint %in% c("pre", "post")) |
        (study_id == "longitudinal")
    ) %>%
    mutate(
      timepoint_combined = case_when(
        study_id == "cross-sectional" & timepoint == "pre" ~ "w0-pre",
        study_id == "cross-sectional" & timepoint == "post" & week_numeric == 0 ~ "w0-post",
        study_id == "cross-sectional" & timepoint == "post" & week_numeric == 4 ~ "w4-post",
        study_id == "longitudinal" & week == "w0" ~ "w0-pre",
        study_id == "longitudinal" & week == "w1" ~ "w1",
        study_id == "longitudinal" & week == "w2" ~ "w2",
        study_id == "longitudinal" & week == "w3" ~ "w3",
        study_id == "longitudinal" & week == "w4" ~ "w4",
        TRUE ~ NA_character_
      ),
      x_position = case_when(
        timepoint_combined == "w0-pre" ~ 0,
        timepoint_combined == "w0-post" ~ 0.5,
        timepoint_combined == "w1" ~ 1,
        timepoint_combined == "w2" ~ 2,
        timepoint_combined == "w3" ~ 3,
        timepoint_combined == "w4" ~ 4,
        timepoint_combined == "w4-post" ~ 4,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(timepoint_combined))

  # Calculate means
  if (split_by_multiplier && "multiplier_factor" %in% names(trajectory_data)) {
    trajectory_means <- map_dfr(outcome_vars, function(var) {
      trajectory_data %>%
        filter(!is.na(.data[[var]])) %>%
        group_by(study_id, timepoint_combined, x_position, multiplier_factor) %>%
        summarise(
          mean_value = mean(.data[[var]], na.rm = TRUE),
          se_value = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
          n = n(),
          .groups = "drop"
        ) %>%
        mutate(outcome = var)
    })
  } else {
    trajectory_means <- map_dfr(outcome_vars, function(var) {
      trajectory_data %>%
        filter(!is.na(.data[[var]])) %>%
        group_by(study_id, timepoint_combined, x_position) %>%
        summarise(
          mean_value = mean(.data[[var]], na.rm = TRUE),
          se_value = sd(.data[[var]], na.rm = TRUE) / sqrt(n()),
          n = n(),
          .groups = "drop"
        ) %>%
        mutate(outcome = var)
    })
  }

  # Clean outcome names
  trajectory_means$outcome_clean <- clean_var_vector(trajectory_means$outcome)

  # Create plot
  p <- ggplot(trajectory_means, aes(x = x_position, y = mean_value, color = study_id)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = mean_value - se_value, ymax = mean_value + se_value),
      width = 0.05, alpha = 0.7
    ) +
    scale_color_study() +
    scale_x_continuous(
      breaks = c(0, 0.5, 1, 2, 3, 4),
      labels = c("w0-pre", "w0-post", "w1", "w2", "w3", "w4")
    ) +
    labs(x = "Timepoint", y = "Mean Rating") +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Add faceting
  if (split_by_multiplier && "multiplier_factor" %in% names(trajectory_means)) {
    p <- p + facet_grid(outcome_clean ~ multiplier_factor, scales = "free_y")
  } else {
    p <- p + facet_wrap(~outcome_clean, scales = "free_y", nrow = 1)
  }

  return(p)
}

# =============================================================================
# SURVEY ITEM PLOTS
# =============================================================================

# Hardcoded item names for anthro attitudes survey (by slider_name)
ANTHRO_ITEM_NAMES <- c(
  # Mental states
  express_preferences = "Preferences",
  think_believe = "Mental language",
  has_emotions = "Emotions (general)",
  happy_to_help = "Emotions (specific)",
  # Relationships
  ai_as_friend = "Friendship",
  express_love = "Affection",
  sexual_content = "Sexual content",
  meaningful_over_humans = "Meaningful bonds",
  # Tone
  rude_to_ai = "Rudeness",
  swear_slang = "Formality",
  funny_offbeat = "Personality",
  give_opinions = "Opinions"
)

#' Create survey item summary plot with means and CIs
#'
#' @param data Data frame with slider_response, slider_category, slider_name columns
#' @param category_mapping Optional named vector mapping categories
#' @return ggplot object
plot_survey_items <- function(data, category_mapping = NULL) {
  # Calculate summary statistics
  plot_data <- data %>%
    group_by(slider_category, slider_name) %>%
    summarise(
      mean_response = mean(slider_response, na.rm = TRUE),
      sd_response = sd(slider_response, na.rm = TRUE),
      n = n(),
      se = sd_response / sqrt(n),
      ci_95 = 1.96 * se,
      .groups = "drop"
    ) %>%
    group_by(slider_category) %>%
    mutate(item_number = row_number()) %>%
    ungroup()

  # Clean category names
  if (!is.null(category_mapping)) {
    plot_data$broad_category <- category_mapping[plot_data$slider_category]
  } else {
    plot_data$broad_category <- gsub("_", " ", tools::toTitleCase(plot_data$slider_category))
  }

  # Map item labels from hardcoded names
  plot_data$item_label <- ANTHRO_ITEM_NAMES[plot_data$slider_name]

  # Create plot
  ggplot(plot_data, aes(x = mean_response, y = item_number)) +
    geom_segment(
      aes(x = 0, xend = 100, y = item_number, yend = item_number),
      color = "lightgray", linetype = "dotted", alpha = 0.7, linewidth = 0.5
    ) +
    geom_errorbarh(
      aes(xmin = mean_response - ci_95, xmax = mean_response + ci_95),
      height = 0.2, color = "black", linewidth = 0.5
    ) +
    geom_point(color = "black", size = 3) +
    geom_vline(xintercept = 50, color = "gray", linetype = "dashed", alpha = 0.25) +
    geom_text(
      aes(x = 50, y = item_number + 0.25, label = item_label),
      hjust = 0.5, vjust = 0, size = 4
    ) +
    facet_wrap(~broad_category, ncol = 1, scales = "free_y") +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = c(0, 50, 100),
      labels = c("Strongly\nDisagree", "Neutral", "Strongly\nAgree")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
    labs(x = NULL, y = NULL) +
    theme_pub() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# =============================================================================
# RANKING PLOTS
# =============================================================================

#' Create topic preference ranking plot
#'
#' @param data_long Long-format data with topic and rank columns
#' @return Combined ggplot object
plot_topic_rankings <- function(data_long) {
  # Determine topic order by mean rank
  topic_order <- data_long %>%
    group_by(topic) %>%
    summarise(mean_rank = mean(rank, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_rank)) %>%
    pull(topic)

  # Plot 1: Mean ranking with 95% CI
  plot_means <- data_long %>%
    group_by(study_id, topic) %>%
    summarise(
      mean_rank = mean(rank, na.rm = TRUE),
      sd_rank = sd(rank, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      se_rank = sd_rank / sqrt(n),
      ci_lower = mean_rank - 1.96 * se_rank,
      ci_upper = mean_rank + 1.96 * se_rank,
      topic = factor(topic, levels = topic_order)
    ) %>%
    ggplot(aes(x = topic, y = mean_rank, color = study_id)) +
    geom_point(size = 4, position = position_dodge(width = 0.5)) +
    geom_errorbar(
      aes(ymin = ci_lower, ymax = ci_upper),
      width = 0.2, position = position_dodge(width = 0.5)
    ) +
    coord_flip() +
    scale_color_study() +
    labs(
      title = "Mean Rank",
      subtitle = "Higher = more preferred (95% CI)",
      x = "Topics",
      y = "Mean Rank"
    ) +
    theme_pub() +
    theme(legend.position = "bottom")

  # Plot 2: Frequency of top rank
  top_rank <- max(data_long$rank, na.rm = TRUE)

  topic_top_pct <- data_long %>%
    filter(rank == top_rank) %>%
    group_by(study_id, topic) %>%
    summarise(n_top = n(), .groups = "drop") %>%
    left_join(
      data_long %>%
        group_by(study_id) %>%
        summarise(total = n_distinct(ppt_id), .groups = "drop"),
      by = "study_id"
    ) %>%
    mutate(
      pct_top = (n_top / total) * 100,
      topic = factor(topic, levels = topic_order)
    )

  plot_top <- topic_top_pct %>%
    ggplot(aes(x = topic, y = pct_top, fill = study_id)) +
    geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
    coord_flip() +
    scale_fill_study() +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    labs(
      title = "Frequency of Top Rank",
      subtitle = "% who ranked topic highest",
      x = "",
      y = "Percentage"
    ) +
    theme_pub() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_blank()
    )

  # Combine
  combined <- plot_means + plot_top +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  return(combined)
}

# =============================================================================
# LONGITUDINAL TREND PLOTS
# =============================================================================

#' Plot raw trends over time by facet variables
#'
#' Creates trajectory plots showing mean outcome values over time,
#' with faceting by treatment arms and/or outcome measures.
#'
#' @param data_long Long-format data with outcome_value and outcome_measure columns
#' @param time_var Name of time variable (e.g., "session_numeric")
#' @param facet_vars Character vector of variables to facet by (e.g., c("relationship_seeking_category", "personalisation"))
#' @param width Plot width
#' @param height Plot height
#' @param span Loess span for smoothed lines
#' @param plot_smoothed If TRUE, show loess smoothed lines; if FALSE, show connected means
#' @param outcome_var Name of outcome variable (default "outcome_value")
#' @return ggplot object (also prints)
plot_raw_trends <- function(data_long, time_var, facet_vars = c("relationship_seeking_category"),
                            width = 18, height = 12, span = 0.75, plot_smoothed = TRUE,
                            outcome_var = "outcome_value") {
  # Create grouping variables for means calculation
  group_vars <- c(time_var, "outcome_measure", facet_vars)

  # Calculate means for plotting
  data_plot <- data_long %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(mean_value = mean(!!sym(outcome_var), na.rm = TRUE), .groups = "drop")

  # Create facet formula
  if (length(facet_vars) == 1) {
    facet_formula <- as.formula(paste("outcome_measure ~", facet_vars[1]))
  } else {
    facet_formula <- as.formula(paste("outcome_measure ~", paste(facet_vars, collapse = " + ")))
  }

  # Create base plot
  p <- ggplot(data_long, aes(x = !!sym(time_var), y = !!sym(outcome_var), color = outcome_measure)) +
    geom_point(data = data_plot, aes(y = mean_value), size = 2)

  # Add either smoothed lines or connected dots
  if (plot_smoothed) {
    p <- p + geom_smooth(aes(fill = outcome_measure), method = "loess", se = TRUE,
                         span = span, alpha = 0.2, linewidth = 1.2)
  } else {
    p <- p + geom_line(data = data_plot, aes(y = mean_value), linewidth = 1.2)
  }

  # Set y-axis label based on outcome variable
  y_label <- if (grepl("delta", outcome_var, ignore.case = TRUE)) {
    "Change from Pre"
  } else {
    "Outcome Value"
  }

  # Complete the plot
  p <- p +
    facet_grid(facet_formula, scales = "free_y") +
    labs(x = "Time", y = y_label, color = "Outcome", fill = "Outcome") +
    theme_pub() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(keywidth = 2),
           fill = guide_legend(keywidth = 2))

  # Set plot size and display
  options(repr.plot.width = width, repr.plot.height = height)
  suppressWarnings(print(p))

  return(invisible(p))
}

#' Plot outcome trajectories by lambda
#'
#' Creates trajectory plots showing mean outcome values over lambda levels.
#'
#' @param data_long Long-format data with outcome_value and outcome_measure columns
#' @param data_cross Optional cross-sectional data for comparison
#' @param plot_study Which study to plot: "longitudinal", "cross_sectional", or "both"
#' @param facet_vars Character vector of variables to facet by
#' @param width Plot width
#' @param height Plot height
#' @param span Loess span
#' @param plot_smoothed If TRUE, show loess smoothed lines
#' @param outcome_var Name of outcome variable
#' @param scales Scales for facet_grid ("free_y", "free", "fixed")
#' @return ggplot object
plot_raw_lambda <- function(data_long, data_cross = NULL, plot_study = "longitudinal",
                            facet_vars = c("outcome_measure"), width = 18, height = 12,
                            span = 0.75, plot_smoothed = TRUE, outcome_var = "outcome_value",
                            scales = "free_y") {
  # Create grouping variables for means calculation
  group_vars <- c("lambda", "outcome_measure", facet_vars)

  # Prepare data based on study type
  if (plot_study == "longitudinal") {
    plot_data <- data_long
    data_plot <- data_long %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(mean_value = mean(!!sym(outcome_var), na.rm = TRUE), .groups = "drop")
  } else if (plot_study == "cross_sectional") {
    if (is.null(data_cross)) stop("data_cross must be provided for cross-sectional plots")
    plot_data <- data_cross
    data_plot <- data_cross %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(mean_value = mean(!!sym(outcome_var), na.rm = TRUE), .groups = "drop")
  } else if (plot_study == "both") {
    if (is.null(data_cross)) stop("data_cross must be provided when plot_study = 'both'")
    data_long_labeled <- data_long %>% mutate(study_type = "Longitudinal")
    data_cross_labeled <- data_cross %>% mutate(study_type = "Cross-sectional")
    plot_data <- bind_rows(data_long_labeled, data_cross_labeled)

    group_vars_both <- c("lambda", "outcome_measure", "study_type", facet_vars)
    data_plot <- plot_data %>%
      group_by(across(all_of(group_vars_both))) %>%
      summarise(mean_value = mean(!!sym(outcome_var), na.rm = TRUE), .groups = "drop")
  }

  # Create facet formula
  if (length(facet_vars) == 1 && facet_vars[1] == "outcome_measure") {
    facet_formula <- as.formula("~ outcome_measure")
  } else if (length(facet_vars) == 1) {
    facet_formula <- as.formula(paste("outcome_measure ~", facet_vars[1]))
  } else {
    other_vars <- facet_vars[facet_vars != "outcome_measure"]
    if (length(other_vars) > 0) {
      facet_formula <- as.formula(paste("outcome_measure ~", paste(other_vars, collapse = " + ")))
    } else {
      facet_formula <- as.formula("~ outcome_measure")
    }
  }

  # Create plot
  if (plot_study == "both") {
    p <- ggplot(plot_data, aes(x = lambda, y = !!sym(outcome_var), color = outcome_measure)) +
      geom_point(data = data_plot, aes(y = mean_value, shape = study_type), size = 2)

    if (plot_smoothed) {
      p <- p + geom_smooth(aes(fill = outcome_measure, linetype = study_type),
                           method = "loess", se = TRUE, span = span, alpha = 0.2, linewidth = 1.2)
    } else {
      p <- p + geom_line(data = data_plot, aes(y = mean_value, linetype = study_type), linewidth = 1.2)
    }

    p <- p + scale_linetype_manual(values = c("Longitudinal" = "solid", "Cross-sectional" = "dashed")) +
      scale_shape_manual(values = c("Longitudinal" = 16, "Cross-sectional" = 17)) +
      labs(linetype = "Study Type", shape = "Study Type")
  } else {
    p <- ggplot(plot_data, aes(x = lambda, y = !!sym(outcome_var), color = outcome_measure)) +
      geom_point(data = data_plot, aes(y = mean_value), size = 2)

    if (plot_smoothed) {
      p <- p + geom_smooth(aes(fill = outcome_measure), method = "loess", se = TRUE,
                           span = span, alpha = 0.2, linewidth = 1.2)
    } else {
      p <- p + geom_line(data = data_plot, aes(y = mean_value), linewidth = 1.2)
    }
  }

  # Use facet_wrap for single facet variable (allows truly independent scales)
  # Use facet_grid for multiple facet variables
  if (length(facet_vars) == 1) {
    p <- p + facet_wrap(facet_formula, scales = scales)
  } else {
    p <- p + facet_grid(facet_formula, scales = scales)
  }

  # Set y-axis label based on outcome variable
  y_label <- if (grepl("delta", outcome_var, ignore.case = TRUE)) {
    "Change from Pre"
  } else {
    "Outcome Value"
  }

  p <- p +
    labs(x = "Lambda (Multiplier)", y = y_label,
         color = "Outcome", fill = "Outcome") +
    theme_pub() +
    theme(legend.position = "bottom") +
    guides(color = guide_legend(keywidth = 2),
           fill = guide_legend(keywidth = 2),
           linetype = guide_legend(keywidth = 2),
           shape = guide_legend(keywidth = 2))

  options(repr.plot.width = width, repr.plot.height = height)
  suppressWarnings(print(p))

  return(invisible(p))
}


# =============================================================================
# PRE-POST CHANGE PLOTS
# =============================================================================

#' Clean single outcome variable name
#'
#' @param outcome_var Single outcome variable name
#' @return Cleaned name
clean_single_outcome <- function(outcome_var) {
  gsub("_", " ", tools::toTitleCase(outcome_var))
}

#' Plot pre-post change with means and error bars or distributions
#'
#' @param data Data frame with outcome and _pre columns
#' @param outcome_vars Character vector of outcome variable names
#' @param facet_var Variable to facet by (default "study_id")
#' @param add_distribution If TRUE, show violin + boxplot; if FALSE, show point + errorbar
#' @return ggplot object
plot_pre_post_change <- function(data, outcome_vars, facet_var = "study_id",
                                  add_distribution = FALSE) {
  # Calculate change scores for each outcome
  data_change <- map_dfr(outcome_vars, ~ {
    outcome_var <- .x
    pre_var <- paste0(.x, "_pre")

    if (outcome_var %in% names(data) && pre_var %in% names(data)) {
      data %>%
        select(all_of(facet_var), post = all_of(outcome_var), pre = all_of(pre_var)) %>%
        mutate(
          outcome = outcome_var,
          change = post - pre,
          !!facet_var := as.character(!!sym(facet_var))
        ) %>%
        filter(!is.na(pre) & !is.na(post) & !is.na(change))
    } else {
      NULL
    }
  })

  if (is.null(data_change) || nrow(data_change) == 0) {
    stop("No valid data found for the specified outcome variables")
  }

  # Calculate summary statistics
  change_stats <- data_change %>%
    group_by(outcome, !!sym(facet_var)) %>%
    summarise(
      mean_change = mean(change, na.rm = TRUE),
      se_change = sd(change, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(!!facet_var := as.character(!!sym(facet_var)))

  # Clean outcome names - preserve original order from outcome_vars
  outcome_levels <- sapply(outcome_vars, clean_single_outcome)

  change_stats <- change_stats %>%
    mutate(
      outcome_clean = sapply(outcome, clean_single_outcome),
      outcome_clean = factor(outcome_clean, levels = outcome_levels)
    )

  data_change <- data_change %>%
    mutate(
      outcome_clean = sapply(outcome, clean_single_outcome),
      outcome_clean = factor(outcome_clean, levels = outcome_levels)
    )

  # Get scales based on facet variable
  if (facet_var == "study_id") {
    fill_scale <- scale_fill_study()
    color_scale <- scale_color_study()
  } else if (facet_var == "domain") {
    fill_scale <- scale_fill_domain()
    color_scale <- scale_color_domain()
  } else {
    fill_scale <- scale_fill_viridis_d()
    color_scale <- scale_color_viridis_d()
  }

  # Create base plot
 p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1)

  if (add_distribution) {
    p <- p +
      geom_violin(
        data = data_change,
        aes(x = !!sym(facet_var), y = change, fill = !!sym(facet_var)),
        alpha = 0.3, show.legend = FALSE
      ) +
      geom_boxplot(
        data = data_change,
        aes(x = !!sym(facet_var), y = change, fill = !!sym(facet_var)),
        width = 0.3, alpha = 0.7, show.legend = FALSE
      ) +
      labs(x = "", y = "Distribution Change\n(Post - Pre)")
  } else {
    p <- p +
      geom_point(
        data = change_stats,
        aes(x = !!sym(facet_var), y = mean_change, fill = !!sym(facet_var)),
        size = 4, shape = 21, color = "white", stroke = 1, show.legend = FALSE
      ) +
      geom_errorbar(
        data = change_stats,
        aes(
          x = !!sym(facet_var), y = mean_change,
          ymin = mean_change - se_change, ymax = mean_change + se_change,
          color = !!sym(facet_var)
        ),
        width = 0.2, linewidth = 1.2, show.legend = FALSE
      ) +
      labs(x = "", y = "Mean Change\n(Post - Pre)")
  }

  # Apply scales
  if (add_distribution) {
    p <- p + fill_scale +
      facet_wrap(~outcome_clean, scales = "free_y", nrow = 1) +
      theme_pub()
  } else {
    p <- p + fill_scale + color_scale +
      facet_wrap(~outcome_clean, scales = "free_y", nrow = 1) +
      theme_pub()
  }

  if (facet_var == "study_id") {
    p <- p + theme(axis.text.x = element_text(size = rel(1)))
  }

  return(p)
}

#' Create combined pre-post change plot (means + distributions)
#'
#' @param data Data frame with outcome and _pre columns
#' @param outcome_vars Character vector of outcome variable names
#' @param facet_var Variable to facet by
#' @param total_height Plot height
#' @param total_width Plot width
#' @param figure_dir Directory to save figure
#' @param save_pdf Whether to save PDF
#' @param filename_prefix Prefix for filename
#' @return Combined patchwork plot
plot_pre_post_change_combined <- function(data, outcome_vars, facet_var = "study_id",
                                           total_height = 10, total_width = 16,
                                           figure_dir = NULL, save_pdf = FALSE,
                                           filename_prefix = "outcome") {
  # Create both plots
  plot1 <- plot_pre_post_change(
    data = data,
    outcome_vars = outcome_vars,
    facet_var = facet_var,
    add_distribution = FALSE
  )

  plot2 <- plot_pre_post_change(
    data = data,
    outcome_vars = outcome_vars,
    facet_var = facet_var,
    add_distribution = TRUE
  )

  # Combine plots
  if (length(outcome_vars) == 1) {
    combined_plot <- plot1 | plot2
  } else {
    combined_plot <- plot1 / plot2
  }

  combined_plot <- combined_plot +
    plot_annotation(
      tag_levels = "A",
      theme = theme(plot.tag = element_text(size = 14, face = "bold"))
    )

  # Save if requested
  if (!is.null(figure_dir) && save_pdf) {
    filename <- paste0(filename_prefix, "_pre_post_combined.pdf")
    ggsave(file.path(figure_dir, "pdf", filename),
           combined_plot, width = total_width, height = total_height)
    ggsave(file.path(figure_dir, "png", gsub(".pdf$", ".png", filename)),
           combined_plot, width = total_width, height = total_height, dpi = 300)
  }

  return(combined_plot)
}

#' Plot pre-post correlations with scatter and regression lines
#'
#' Creates scatter plots showing pre vs post values with regression lines
#' per study, R² and coefficient labels, and diagonal reference line.
#'
#' @param data Data frame with outcome and outcome_pre columns
#' @param outcome_vars Character vector of outcome variable names
#' @param total_height Plot height
#' @param total_width Plot width
#' @param figure_dir Directory to save figure
#' @param save_pdf Whether to save PDF
#' @param filename_prefix Prefix for filename
#' @param nrow Number of rows for facet_wrap
#' @return ggplot object
plot_pre_post_correlations <- function(data, outcome_vars,
                                       total_height = 5, total_width = 14,
                                       figure_dir = NULL, save_pdf = FALSE,
                                       filename_prefix = "pre_post_corr",
                                       nrow = 1) {
  # Calculate correlation stats for each outcome and study combination
  correlation_stats <- map_dfr(outcome_vars, ~ {
    outcome_var <- .x
    pre_var <- paste0(.x, "_pre")

    # Filter the data first to get the actual range
    outcome_data <- data %>%
      filter(!is.na(.data[[outcome_var]]) & !is.na(.data[[pre_var]]))

    # Calculate y-axis limits for this outcome
    y_min <- min(outcome_data[[outcome_var]], na.rm = TRUE)
    y_max <- max(outcome_data[[outcome_var]], na.rm = TRUE)
    y_range <- y_max - y_min

    outcome_data %>%
      group_by(study_id) %>%
      summarise(
        outcome = outcome_var,
        r_squared = cor(
          .data[[outcome_var]], .data[[pre_var]], use = "complete.obs"
        )^2,
        coefficient = {
          model <- lm(.data[[outcome_var]] ~ .data[[pre_var]])
          coef(model)[2]
        },
        .groups = "drop"
      ) %>%
      mutate(
        y_pos = case_when(
          study_id == "cross-sectional" ~ y_min + (0.2 * y_range),
          study_id == "longitudinal" ~ y_min + (0.05 * y_range),
          TRUE ~ y_min + (0.05 * y_range)
        )
      ) %>%
      ungroup()
  })

  # Clean outcome variable names using helper function
  correlation_stats <- correlation_stats %>%
    mutate(
      outcome_clean = sapply(outcome, clean_single_outcome),
      outcome_clean = factor(outcome_clean, levels = unique(outcome_clean))
    )

  # Simple data processing
  data_list <- map(outcome_vars, ~ {
    outcome_var <- .x
    pre_var <- paste0(.x, "_pre")

    if (outcome_var %in% names(data) && pre_var %in% names(data)) {
      data %>%
        select(study_id, post = all_of(outcome_var), pre = all_of(pre_var)) %>%
        mutate(outcome = outcome_var) %>%
        filter(!is.na(pre) & !is.na(post))
    } else {
      NULL
    }
  })

  data_corr_long <- bind_rows(data_list[!map_lgl(data_list, is.null)])

  # Clean outcome variable names using helper function
  data_corr_long <- data_corr_long %>%
    mutate(
      outcome_clean = sapply(outcome, clean_single_outcome),
      outcome_clean = factor(
        outcome_clean,
        levels = unique(sapply(unique(outcome), clean_single_outcome))
      )
    )

  # Create the plot
  p <- ggplot(
    data_corr_long,
    aes(x = pre, y = post, color = study_id, fill = study_id)
  ) +
    geom_point(alpha = 0.05, size = 1.5) +
    geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", color = "gray50", linewidth = 1
    ) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, alpha = 0.2) +
    geom_label(
      data = correlation_stats,
      aes(
        label = sprintf("R^2 == %.2f~beta == %.2f", r_squared, coefficient),
        y = y_pos
      ),
      x = Inf, hjust = 1.1,
      size = 5, fontface = "bold",
      fill = "white",
      alpha = 0.9,
      label.padding = unit(0.25, "lines"),
      label.r = unit(0.15, "lines"),
      show.legend = FALSE,
      parse = TRUE
    ) +
    scale_fill_study() +
    scale_color_study() +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      fill = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    facet_wrap(~outcome_clean, scales = "free", nrow = nrow) +
    labs(
      x = "Pre-intervention Score",
      y = "Post-intervention Score"
    ) +
    theme_pub()

  # Save plot if requested
  if (!is.null(figure_dir) && save_pdf) {
    filename <- paste0(filename_prefix, "_pre_post_corr")
    ggsave(
      file.path(figure_dir, "pdf", paste0(filename, ".pdf")),
      p, width = total_width, height = total_height
    )
    ggsave(
      file.path(figure_dir, "png", paste0(filename, ".png")),
      p, width = total_width, height = total_height, dpi = 300
    )
  }

  return(p)
}

# =============================================================================
# DISTRIBUTION HISTOGRAMS
# =============================================================================

#' Create faceted distribution histograms
#'
#' Creates a faceted histogram plot with outcomes as columns and studies as rows.
#' Uses STUDY_COLORS from plot_config.R for consistent styling.
#'
#' @param data Data frame with study_id column and outcome variables
#' @param outcome_vars Character vector of outcome variable names
#' @param binwidth Histogram bin width (default 10 for 0-100 scale)
#' @param outcome_labels Optional named vector for outcome labels
#' @param scales Scales for facet_grid ("free_y", "free_x", "free", "fixed")
#' @return ggplot object
plot_distributions <- function(data,
                               outcome_vars,
                               binwidth = 10,
                               outcome_labels = NULL,
                               scales = "free_y") {

  # Default outcome labels
  if (is.null(outcome_labels)) {
    outcome_labels <- setNames(
      tools::toTitleCase(outcome_vars),
      outcome_vars
    )
  }

  # Reshape to long format
  dist_data <- data %>%
    dplyr::select(dplyr::all_of(c("study_id", outcome_vars))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(outcome_vars),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      outcome = factor(outcome, levels = outcome_vars, labels = outcome_labels)
    )

  # Create faceted histogram using study_id directly
  p <- ggplot(dist_data, aes(x = value, fill = study_id)) +
    geom_histogram(binwidth = binwidth, color = "white", alpha = 0.85) +
    facet_grid(study_id ~ outcome, scales = scales) +
    scale_fill_study() +
    labs(
      x = "Rating (0-100)",
      y = "Count"
    ) +
    theme_pub() +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )

  return(p)
}


#' Create boxplots comparing outcomes by study
#'
#' Creates side-by-side boxplots for each outcome variable, colored by study.
#' Uses STUDY_COLORS from plot_config.R for consistent styling.
#'
#' @param data Data frame with study_id column and outcome variables
#' @param outcome_vars Character vector of outcome variable names
#' @param outcome_labels Optional named vector for outcome labels
#' @return ggplot object
plot_boxplots <- function(data,
                          outcome_vars,
                          outcome_labels = NULL,
                          free_y = FALSE) {

  # Default outcome labels
  if (is.null(outcome_labels)) {
    outcome_labels <- setNames(
      tools::toTitleCase(gsub("_", " ", outcome_vars)),
      outcome_vars
    )
  }

  # Reshape to long format
  box_data <- data %>%
    dplyr::select(dplyr::all_of(c("study_id", outcome_vars))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(outcome_vars),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      outcome = factor(outcome, levels = outcome_vars, labels = outcome_labels)
    )

  # Create boxplot using study_id directly with scale_fill_study()
  p <- ggplot(box_data, aes(x = study_id, y = value, fill = study_id)) +
    geom_boxplot(outlier.size = 0.8, alpha = 0.85) +
    scale_fill_study() +
    facet_wrap(~ outcome, scales = if (free_y) "free_y" else "fixed", nrow = 1) +
    labs(
      x = NULL,
      y = "Value"
    ) +
    theme_pub() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  return(p)
}


#' Create side-by-side correlation heatmaps using pheatmap
#'
#' Creates correlation heatmaps with dendrograms for cross-sectional
#' and longitudinal data side by side. Can handle single-study cases.
#'
#' @param data_cs Cross-sectional data frame (NULL or empty to skip)
#' @param data_long Longitudinal data frame (NULL or empty to skip)
#' @param outcome_vars Character vector of outcome variable names
#' @param width Plot width
#' @param height Plot height
#' @return Combined patchwork plot
plot_corr_heatmaps_combined <- function(data_cs = NULL, data_long = NULL,
                                        outcome_vars, width = 14, height = 8) {

  # Color palette (reversed RdBu - red for high correlation)
  color_palette <- colorRampPalette(rev(brewer.pal(11, "RdBu")))(100)
  breaks <- seq(-1, 1, length.out = 101)

  # Function to determine text color based on correlation value
  get_text_colors <- function(mat, dark_threshold = 0.5) {
    ifelse(abs(mat) > dark_threshold, "white", "black")
  }

  # Clean variable names
  clean_names <- tools::toTitleCase(outcome_vars)

  # Helper function to create a single heatmap
  create_heatmap <- function(data, title) {
    cor_mat <- cor(data[, outcome_vars], use = "complete.obs")
    rownames(cor_mat) <- clean_names
    colnames(cor_mat) <- clean_names

    # First pass to get clustering order
    p_temp <- pheatmap(
      cor_mat,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      display_numbers = FALSE,
      color = color_palette,
      breaks = breaks,
      silent = TRUE
    )

    # Get ordered text colors
    text_colors <- get_text_colors(cor_mat)
    text_colors_ord <- text_colors[p_temp$tree_row$order, p_temp$tree_col$order]

    # Create final heatmap
    pheatmap(
      cor_mat,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      display_numbers = TRUE,
      number_color = text_colors_ord,
      color = color_palette,
      breaks = breaks,
      fontsize = 12,
      fontsize_number = 10,
      legend = FALSE,
      main = paste0(title, "\n"),
      silent = TRUE
    )
  }

  plots <- list()

  # Check which datasets have data
  has_cs <- !is.null(data_cs) && nrow(data_cs) > 0
  has_long <- !is.null(data_long) && nrow(data_long) > 0

  if (has_cs) {
    plots[["cs"]] <- create_heatmap(data_cs, "Cross-sectional")
  }

  if (has_long) {
    plots[["long"]] <- create_heatmap(data_long, "Longitudinal")
  }

  # Combine with patchwork
  if (has_cs && has_long) {
    combined <- wrap_elements(plots$cs[[4]]) + wrap_elements(plots$long[[4]])
  } else if (has_cs) {
    combined <- wrap_elements(plots$cs[[4]])
  } else if (has_long) {
    combined <- wrap_elements(plots$long[[4]])
  } else {
    stop("At least one of data_cs or data_long must be provided with data")
  }

  return(combined)
}

# =============================================================================
# AFFECT GRID PLOTS
# =============================================================================

#' Plot affect grid with pre/post density contours
#'
#' Creates a 2D affect grid visualization showing the distribution of
#' affect ratings before and after an intervention using density contours.
#' Pre and Post are shown in separate facets for clarity.
#'
#' @param df Data frame with affect_x, affect_y, affect_x_pre, affect_y_pre
#' @param facet_rows Variable(s) for facet rows (e.g., "study_id")
#' @param alpha Contour transparency (default 0.3)
#' @param bins Number of contour bins (default 8)
#' @param show_points Whether to show scatter points (default TRUE)
#' @return ggplot object
plot_affect_grid_overlay <- function(df,
                                     facet_rows = "study_id",
                                     alpha = 0.3,
                                     bins = 8,
                                     show_points = TRUE) {

  # Prepare data: reshape to long format for pre/post
  select_cols <- c("ppt_id", "affect_x_pre", "affect_y_pre")
  if (!is.null(facet_rows)) select_cols <- c(select_cols, facet_rows)

  df_pre <- df %>%
    filter(!is.na(affect_x_pre) & !is.na(affect_y_pre)) %>%
    select(all_of(select_cols)) %>%
    rename(valence = affect_x_pre, arousal = affect_y_pre) %>%
    mutate(timepoint = "Pre")

  select_cols_post <- c("ppt_id", "affect_x", "affect_y")
  if (!is.null(facet_rows)) select_cols_post <- c(select_cols_post, facet_rows)

  df_post <- df %>%
    filter(!is.na(affect_x) & !is.na(affect_y)) %>%
    select(all_of(select_cols_post)) %>%
    rename(valence = affect_x, arousal = affect_y) %>%
    mutate(timepoint = "Post")

  df_long <- bind_rows(df_pre, df_post) %>%
    mutate(timepoint = factor(timepoint, levels = c("Pre", "Post")))

  # Create base plot

  p <- ggplot(df_long, aes(x = valence, y = arousal))

  # Add scatter points underneath contours
  if (show_points) {
    p <- p + geom_point(alpha = 0.03, size = 0.5, shape = 16, color = "gray30")
  }

  # Add filled density contours
  p <- p +
    stat_density_2d(
      aes(fill = after_stat(level)),
      geom = "polygon",
      alpha = alpha,
      bins = bins
    ) +
    geom_density_2d(linewidth = 0.5, alpha = 0.6, color = "gray40")

  # Add softer quadrant lines
  p <- p +
    geom_hline(yintercept = 50, linetype = "dotted", color = "gray70",
               linewidth = 0.3) +
    geom_vline(xintercept = 50, linetype = "dotted", color = "gray70",
               linewidth = 0.3)

  # Use coord_fixed for square grid
  p <- p +
    coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 100)) +
    scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
    scale_fill_viridis_c(
      option = "plasma",
      name = "Density",
      guide = guide_colorbar(barwidth = 15, barheight = 0.5)
    )

  # Add axis labels with affect grid conventions
  p <- p +
    labs(
      x = "Valence\nUnpleasant - Pleasant",
      y = "Arousal\nSleepy - Alert"
    )

  # Add quadrant annotations in margins
  p <- p +
    annotate("text", x = 5, y = 97, label = "STRESS",
             size = 4, color = "black", fontface = "italic",
             hjust = 0, vjust = 1) +
    annotate("text", x = 95, y = 97, label = "EXCITEMENT",
             size = 4, color = "black", fontface = "italic",
             hjust = 1, vjust = 1) +
    annotate("text", x = 5, y = 3, label = "BOREDOM",
             size = 4, color = "black", fontface = "italic",
             hjust = 0, vjust = 0) +
    annotate("text", x = 95, y = 3, label = "RELAXATION",
             size = 4, color = "black", fontface = "italic",
             hjust = 1, vjust = 0)

  # Facet by timepoint and optional row variable
  if (!is.null(facet_rows)) {
    p <- p + facet_grid(
      rows = vars(!!sym(facet_rows)),
      cols = vars(timepoint)
    )
  } else {
    p <- p + facet_wrap(~ timepoint, nrow = 1)
  }

  # Apply theme
  p <- p +
    theme_pub() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      strip.text = element_text(size = 10, face = "bold")
    )

  return(p)
}


# =============================================================================
# CATEGORICAL BAR PLOTS
# =============================================================================

#' Create percentage-based dodged bar plot for categorical responses
#'
#' Creates a horizontal bar plot showing percentage breakdown of categorical
#' responses by a grouping variable. Useful for visualizing survey responses
#' like "change in view" or "future outlook" by study or treatment group.
#'
#' @param data Data frame containing the response and grouping variables
#' @param response_var Name of the categorical response column (e.g., "change_recoded")
#' @param group_var Name of the grouping column (e.g., "study_id", "relationship_seeking_category")
#' @param response_levels Optional ordered factor levels for response variable
#' @param title Optional plot title
#' @param subtitle Optional plot subtitle
#' @param colors Optional named vector of colors for fill
#' @param labels Optional named vector of labels for fill legend
#' @return ggplot object
plot_categorical_by_group <- function(data, response_var, group_var,
                                       response_levels = NULL,
                                       title = NULL, subtitle = NULL,
                                       colors = NULL, labels = NULL) {
  # Calculate percentages by group

plot_data <- data %>%
    filter(!is.na(.data[[response_var]])) %>%
    count(.data[[group_var]], response_value = .data[[response_var]]) %>%
    group_by(.data[[group_var]]) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()

  # Factor response if levels provided
  if (!is.null(response_levels)) {
    plot_data$response_value <- factor(plot_data$response_value, levels = response_levels)
  }

  # Create base plot
  p <- ggplot(plot_data, aes(x = pct, y = response_value, fill = .data[[group_var]])) +
    geom_col(position = position_dodge(width = 0.8), alpha = 0.8, width = 0.7) +
    geom_text(aes(label = sprintf("%.0f%%", pct)),
              position = position_dodge(width = 0.8),
              hjust = -0.1, size = 4, fontface = "bold") +
    labs(title = title, subtitle = subtitle, x = "Percentage", y = NULL, fill = "") +
    theme_pub() +
    theme(legend.position = "bottom") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25)))

  # Apply colors if provided
  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors, labels = labels)
  } else if (group_var == "study_id") {
    p <- p + scale_fill_study()
  }

  return(p)
}

# =============================================================================
# FIXED EFFECTS COEFFICIENT PLOTS
# =============================================================================

#' Create combined topic effects plot with outcome facets
#'
#' Creates a forest plot for topic fixed effects across multiple outcomes,
#' with facets by outcome and domain-colored labels.
#'
#' @param topic_models Named list of models, keyed by outcome name.
#'   Each element should have cross_sectional and/or longitudinal models.
#' @param study_type Which study type to plot: "cross_sectional" or "longitudinal"
#' @param fe_var Fixed effect variable name (default "topic")
#' @param reference_level Reference level to add at zero
#' @param level_domains Named vector mapping levels to domains
#' @param fdr_correct Whether to apply FDR correction
#' @param alpha Significance threshold
#' @return ggplot object
plot_topic_effects_combined <- function(topic_models,
                                        study_type = "cross_sectional",
                                        fe_var = "topic",
                                        reference_level = NULL,
                                        level_domains = NULL,
                                        fdr_correct = TRUE,
                                        alpha = 0.05) {

  # Extract coefficients from a single model using parameters package
  extract_fe_coefs <- function(mod, outcome_name) {
    if (is.null(mod)) return(NULL)

    # Use parameters package for clean extraction
    params <- parameters::model_parameters(mod, effects = "fixed")
    coefs <- data.frame(
      term = params$Parameter,
      estimate = params$Coefficient,
      std_error = params$SE,
      p_value = params$p
    )

    fe_pattern <- paste0("^", fe_var)
    coefs <- coefs %>%
      filter(grepl(fe_pattern, term)) %>%
      mutate(
        level = gsub(fe_var, "", term),
        outcome = outcome_name
      ) %>%
      select(term, level, estimate, std_error, p_value, outcome)

    coefs
  }

  # Extract coefficients for all outcomes
  all_coefs <- list()
  for (outcome_name in names(topic_models)) {
    mod <- topic_models[[outcome_name]][[study_type]]
    coefs <- extract_fe_coefs(mod, tools::toTitleCase(gsub("_", " ", outcome_name)))
    if (!is.null(coefs)) {
      all_coefs[[outcome_name]] <- coefs
    }
  }

  coefs_df <- bind_rows(all_coefs)

  if (nrow(coefs_df) == 0) {
    warning("No coefficients found")
    return(NULL)
  }

  # Apply FDR correction within each outcome
  if (fdr_correct) {
    coefs_df <- coefs_df %>%
      group_by(outcome) %>%
      mutate(p_adj = p.adjust(p_value, method = "fdr")) %>%
      ungroup()
  } else {
    coefs_df <- coefs_df %>%
      mutate(p_adj = p_value)
  }

  # Add significance indicators and CIs
 coefs_df <- coefs_df %>%
    mutate(
      significant = p_adj < alpha,
      ci_lower = estimate - 1.96 * std_error,
      ci_upper = estimate + 1.96 * std_error
    )

  # Add reference level if specified
  if (!is.null(reference_level)) {
    ref_rows <- data.frame(
      term = paste0(fe_var, reference_level),
      level = reference_level,
      estimate = 0,
      std_error = 0,
      p_value = NA,
      outcome = unique(coefs_df$outcome),
      p_adj = NA,
      significant = NA,
      ci_lower = 0,
      ci_upper = 0
    )
    coefs_df <- bind_rows(ref_rows, coefs_df)
  }

  # Order levels by mean coefficient across outcomes (ref will be in middle)
  level_order <- coefs_df %>%
    group_by(level) %>%
    summarise(mean_est = mean(estimate, na.rm = TRUE), .groups = "drop") %>%
    arrange(mean_est) %>%
    pull(level)

  coefs_df <- coefs_df %>%
    mutate(level = factor(level, levels = level_order))

  # Define significance colors
  colors <- c(
    "reference" = "#2166ac",
    "significant" = "#b2182b",
    "non_significant" = "gray50"
  )

  coefs_df <- coefs_df %>%
    mutate(
      color_group = case_when(
        is.na(significant) ~ "reference",
        significant ~ "significant",
        TRUE ~ "non_significant"
      )
    )

  # Create plot
  p <- ggplot(coefs_df, aes(x = estimate, y = level, color = color_group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40",
               linewidth = 0.8) +
    geom_errorbarh(
      aes(xmin = ci_lower, xmax = ci_upper),
      height = 0.2, linewidth = 0.6, alpha = 0.8
    ) +
    geom_point(size = 2) +
    scale_color_manual(
      values = colors,
      name = "Significance",
      labels = c("reference" = "Reference",
                 "significant" = "Significant (FDR < 0.05)",
                 "non_significant" = "Not significant"),
      guide = guide_legend(override.aes = list(shape = 16, size = 3))
    ) +
    facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
    labs(
      x = "Coefficient Estimate",
      y = "Topic"
    ) +
    theme_pub() +
    theme(
      legend.position = "bottom",
      legend.box.margin = margin(t = 10, b = 10),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      strip.text = element_text(size = 11, face = "bold")
    )

  # Add domain-based label coloring if provided
  if (!is.null(level_domains)) {
    domain_colors <- c(
      "emotchat" = "#568810",
      "polchat" = "#1142e5"
    )

    colored_labels <- sapply(level_order, function(lvl) {
      domain <- level_domains[lvl]
      if (!is.na(domain) && domain %in% names(domain_colors)) {
        paste0("<span style='color:", domain_colors[domain], "'>", lvl, "</span>")
      } else {
        lvl
      }
    })

    p <- p +
      scale_y_discrete(labels = colored_labels) +
      theme(axis.text.y = ggtext::element_markdown(size = 11))
  }

  return(p)
}
