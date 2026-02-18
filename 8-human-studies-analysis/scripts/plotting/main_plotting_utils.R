# =============================================================================
# Main Plotting Utilities
# =============================================================================
# Shared plotting functions for main paper figures.
# =============================================================================

# Required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(grDevices)
library(ggeffects)
library(ggbeeswarm)
library(emmeans)
library(ggsignif)
library(jsonlite)

# Set emmeans options globally
emm_options(pbkrtest.limit = 50000)

# =============================================================================
# CONSTANTS
# =============================================================================

# Define consistent colors for treatments across all plots
rs_color <- "red"
domain_color <- "#209b7e"
polchat_color <- "#72c8bd"
pers_color <- "#7708e5"
time_color <- "grey50"

# Define shared point styling for consistency across plots
POINT_SIZE <- 5
POINT_ALPHA <- 0.6
POINT_SHAPE <- 21
POINT_COLOR <- "#ffffff"
POINT_STROKE <- 1
LINE_WIDTH <- 2

# Define text sizes
STRIP_TEXT_SIZE_SMALL <- 14

# Construct name dictionaries for custom labeling
# Long labels for general plots (facets, regular plots)
construct_labels_long <- list(
  "tool_friend" = "Views Their AI as\nFriend (vs Tool)",
  "sentience" = "Belief that AI in\n General is Conscious",
  "seeking_companionship_likelihood" = "Desire for Future AI\nCompanionship and Support",
  "goodbye_action" = "Saying Goodbye to the AI",
  "engagingness" = "Engagingness",
  "likeability" = "Likeability",
  "helpfulness" = "Helpfulness",
  "separation_distress" = "Separation Distress",
  "reliance" = "Reliance",
  "perceived_understanding" = "Perceived Understanding",
  "self_disclosure" = "Self-Disclosure",
  "valence" = "Valence",
  "arousal" = "Arousal",
  "psychosocial_F1" = "Emotional Health",
  "psychosocial_F2" = "Social Health",
  "perceived_sentience" = "Perceived Consciousness",
  "ontological_sentience" = "Ontological Consciousness"
)

# Short labels for forest plots (with line breaks for readability)
construct_labels_short <- list(
  "tool_friend" = "Tool-Friend\nPerceptions",
  "sentience" = "Sentience\nBeliefs",
  "seeking_companionship_likelihood" = "Desire for Future\nAI Companionship",
  "goodbye_action" = "Saying Goodbye\nto the AI",
  "engagingness" = "Engagingness",
  "likeability" = "Likeability",
  "helpfulness" = "Helpfulness",
  "separation_distress" = "Separation\nDistress",
  "reliance" = "Reliance",
  "perceived_understanding" = "Perceived\nUnderstanding",
  "self_disclosure" = "Self-Disclosure",
  "valence" = "Valence",
  "arousal" = "Arousal",
  "psychosocial_F1" = "Emotional Health\n(Factor 1)",
  "psychosocial_F2" = "Social Health\n(Factor 2)",
  "perceived_sentience" = "Perceived\nConsciousness",
  "ontological_sentience" = "Ontological\nConsciousness"
)

# Helper function to format construct labels for general plots (long labels)
format_construct_labels <- function(x) {
  sapply(x, function(label) {
    # Check if custom label exists in long labels dictionary
    if (label %in% names(construct_labels_long)) {
      return(construct_labels_long[[label]])
    } else {
      # Apply default formatting: replace hyphens/underscores with newlines, then title case
      formatted <- gsub("[-_]", "\n", label)
      return(tools::toTitleCase(formatted))
    }
  })
}

# Helper function to format construct labels for forest plots (short labels with line breaks)
format_construct_labels_forest <- function(x) {
  sapply(x, function(label) {
    # Check if custom label exists in short labels dictionary
    if (label %in% names(construct_labels_short)) {
      return(construct_labels_short[[label]])
    } else {
      # Apply default formatting: replace hyphens/underscores with newlines, then title case
      formatted <- gsub("[-_]", "\n", label)
      return(tools::toTitleCase(formatted))
    }
  })
}

# Define time gradient colors
TIME_GRADIENT_COLORS <- c("#0000FF", "#4040FF", "#808080", "#808080", "#808080", "#FF4040", "#FF0000")

# Define enhanced gradient function and session mean colors
create_enhanced_gradient <- function(color1, color2, n_sessions = 20) {
  colorRampPalette(c(color1, color2))(n_sessions)
}

# Session means gradient colors
BLUE_GRADIENT_START <- "#dedefb"
BLUE_GRADIENT_END <- "#13139a"
RED_GRADIENT_START <- "#FFA4A4"
RED_GRADIENT_END <- "#920000"
GRAY_GRADIENT_START <- "#F5F5F5"
GRAY_GRADIENT_END <- "#2F2F2F"

# Define publication-ready theme
theme_pub <- function() {
    theme(
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 18, colour = "grey50"),
        # legend.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(size = 14, colour = "grey50"),
        strip.text = element_text(size = 18, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        legend.position = "bottom",
        plot.margin = margin(10, 10, 10, 10)

    )
}

plot_lambda_means_with_predictions <- function(mods_longitudinal, data_longitudinal,
                                                 construct_names = NULL,
                                                 custom_order = NULL,
                                                 time_var = "session_numeric",
                                                 lambda_seq = seq(-1, 1, by = 0.01),
                                                 lambda_values = c(-1, -0.5, 0, 0.5, 1),
                                                 family_results = NULL,
                                                 ncol = 3,
                                                 width = 15, height = 8,
                                                 jitter_width = 0.15,
                                                 jitter_amount = 0.1,
                                                 free_y = TRUE,
                                                 y_limits = NULL,
                                                 y_breaks = NULL,
                                                 title = "Model Predictions with Time Means",
                                                 # subtitle = paste0("(Blue: ", expression(lambda), "<0, Red: ", expression(lambda), ">0, Gray: ", expression(lambda), "=0 | Light=early, Dark=late sessions)"),
                                                 subtitle = "",
                                                 x_label = expression("Steering Vector Multiplier ("*lambda*")"),
                                                 y_label = "Predicted Mean",
                                                 seed = 42,
                                                 label_endpoints = TRUE,
                                                 legend_position = "none",
                                                 legend_nrow = 1) {
      # -------------------------------------------------------------------------
      # Step 1: Process input data and establish construct ordering
      # -------------------------------------------------------------------------

      # Get construct names if not provided
      if (is.null(construct_names)) {
          construct_names <- names(mods_longitudinal)
      }

      # Use custom order if provided, otherwise use construct_names order
      if (!is.null(custom_order)) {
          # Filter custom_order to only include available constructs
          custom_order <- intersect(custom_order, construct_names)
          construct_order <- custom_order
      } else {
          construct_order <- construct_names
      }

      # -------------------------------------------------------------------------
      # Step 2: Extract polynomial coefficients if family results provided
      # -------------------------------------------------------------------------

      poly_labels <- NULL
      if (!is.null(family_results)) {
          # Helper function to create polynomial labels
          create_poly_label <- function(data) {
              construct_labels <- data %>%
                  group_by(construct) %>%
                  summarise(
                      has_lambda = any(coef_type == "lambda"),
                      has_lambda2 = any(coef_type == "lambda2"),
                      has_lambda3 = any(coef_type == "lambda3"),
                      .groups = "drop"
                  ) %>%
                  mutate(
                      poly_type = case_when(
                          has_lambda3 ~ "cubic",
                          has_lambda2 ~ "quadratic",
                          has_lambda ~ "linear",
                          TRUE ~ "none"
                      )
                  )

              # Create separate lines for each polynomial coefficient (mtext-inspired)
              labels_list <- list()

              for (i in 1:nrow(construct_labels)) {
                  construct_name <- construct_labels$construct[i]
                  poly_type <- construct_labels$poly_type[i]

                  construct_data <- data %>% filter(construct == construct_name)

                  if (poly_type == "cubic") {
                      lambda_data <- construct_data %>% filter(coef_type == "lambda")
                      lambda2_data <- construct_data %>% filter(coef_type == "lambda2")
                      lambda3_data <- construct_data %>% filter(coef_type == "lambda3")

                      # Line 1: lambda coefficient
                      labels_list[[length(labels_list) + 1]] <- tibble(
                          construct = construct_name,
                          label = sprintf("beta[lambda]*' %.2f%s'", lambda_data$estimate, lambda_data$sig_stars),
                          line_order = 1
                      )
                      # Line 2: lambda^2 coefficient
                      labels_list[[length(labels_list) + 1]] <- tibble(
                          construct = construct_name,
                          label = sprintf("beta[lambda^2]*' %.2f%s'", lambda2_data$estimate, lambda2_data$sig_stars),
                          line_order = 2
                      )
                      # Line 3: lambda^3 coefficient
                      labels_list[[length(labels_list) + 1]] <- tibble(
                          construct = construct_name,
                          label = sprintf("beta[lambda^3]*' %.2f%s'", lambda3_data$estimate, lambda3_data$sig_stars),
                          line_order = 3
                      )
                  } else if (poly_type == "quadratic") {
                      lambda_data <- construct_data %>% filter(coef_type == "lambda")
                      lambda2_data <- construct_data %>% filter(coef_type == "lambda2")

                      # Line 1: lambda coefficient
                      labels_list[[length(labels_list) + 1]] <- tibble(
                          construct = construct_name,
                          label = sprintf("beta[lambda]*' %.2f%s'", lambda_data$estimate, lambda_data$sig_stars),
                          line_order = 1
                      )
                      # Line 2: lambda^2 coefficient
                      labels_list[[length(labels_list) + 1]] <- tibble(
                          construct = construct_name,
                          label = sprintf("beta[lambda^2]*' %.2f%s'", lambda2_data$estimate, lambda2_data$sig_stars),
                          line_order = 2
                      )
                  } else if (poly_type == "linear") {
                      lambda_data <- construct_data %>% filter(coef_type == "lambda")

                      # Line 1: lambda coefficient
                      labels_list[[length(labels_list) + 1]] <- tibble(
                          construct = construct_name,
                          label = sprintf("beta[lambda]*' %.2f%s'", lambda_data$estimate, lambda_data$sig_stars),
                          line_order = 1
                      )
                  }
              }

              if (length(labels_list) > 0) {
                  labels_df <- bind_rows(labels_list)
              } else {
                  labels_df <- tibble(construct = character(), label = character(), line_order = numeric())
              }

              return(labels_df)
          }

          # Extract coefficients
          lambda_coefs <- family_results$family_all %>%
              filter(test_id == "coefs_continuous") %>%
              filter(grepl("^lambda$|^I\\(lambda\\^2\\)|^I\\(lambda\\^3\\)", coefficient)) %>%
              select(construct, coefficient, estimate, p.value.adj.family) %>%
              mutate(
                  sig_stars = case_when(
                      p.value.adj.family < 0.001 ~ "***",
                      p.value.adj.family < 0.01 ~ "**",
                      p.value.adj.family < 0.05 ~ "*",
                      TRUE ~ ""
                  ),
                  coef_type = case_when(
                      coefficient == "lambda" ~ "lambda",
                      coefficient == "I(lambda^2)" ~ "lambda2",
                      coefficient == "I(lambda^3)" ~ "lambda3",
                      TRUE ~ "other"
                  )
              ) %>%
              filter(coef_type != "other") %>%
              select(construct, coef_type, estimate, sig_stars)

          # Create polynomial labels
          poly_labels <- create_poly_label(lambda_coefs) %>%
              mutate(construct = factor(construct, levels = construct_order))

          # Filter poly_labels to only include constructs that are in the final construct_order
          # This prevents annotations from showing on facets that won't be displayed
          if (!is.null(custom_order)) {
              poly_labels <- poly_labels %>%
                  filter(construct %in% construct_order)
          }
      }

      # -------------------------------------------------------------------------
      # Step 3: Generate model predictions
      # -------------------------------------------------------------------------

      all_preds_list <- list()
      for (construct_name in construct_order) {
          model <- mods_longitudinal[[construct_name]]$additive_continuous
          preds <- ggemmeans(
              model,
              terms = paste0("lambda [", paste(lambda_seq, collapse = ", "), "]"),
              verbose = FALSE
          )
          preds_df <- as.data.frame(preds) %>%
              rename(lambda = x, predicted = predicted) %>%
              mutate(
                  construct = construct_name,
                  lambda = as.numeric(as.character(lambda))
              )
          all_preds_list[[construct_name]] <- preds_df
      }

      all_preds_df <- bind_rows(all_preds_list) %>%
          mutate(construct = factor(construct, levels = construct_order))

      # -------------------------------------------------------------------------
      # Step 4: Calculate time point means
      # -------------------------------------------------------------------------

      # Convert construct_order to character to avoid factor level issues
      construct_order_char <- as.character(construct_order)

      # Calculate observed time point means with SE
      time_means <- data_longitudinal %>%
          filter(as.character(construct) %in% construct_order_char) %>%
          group_by(construct, lambda, !!sym(time_var)) %>%
          summarise(
              time_mean = mean(outcome_value, na.rm = TRUE),
              time_sd = sd(outcome_value, na.rm = TRUE),
              n_obs = n(),
              time_se = time_sd / sqrt(n_obs),
              .groups = "drop"
          ) %>%
          mutate(
              construct = factor(construct, levels = construct_order),
              lambda_time_key = paste0(lambda, "_", !!sym(time_var))
          )

      # -------------------------------------------------------------------------
      # Step 5: Create enhanced color palettes for session gradients
      # -------------------------------------------------------------------------

      # Check if time_means is empty after filtering
      if (nrow(time_means) == 0) {
          stop("No time points available in the data. Please check your data structure.")
      }

      # Determine time range for gradient mapping
      time_range <- range(time_means[[time_var]], na.rm = TRUE)
      min_time <- time_range[1]
      max_time <- time_range[2]
      n_time_points <- max_time - min_time + 1

      # Additional safety check
      if (!is.finite(n_time_points) || n_time_points <= 0) {
          stop("Invalid time range calculated. Check your time variable data.")
      }

      # Define color mappings for each lambda group
      time_colors <- list()

      # Check if there's only one timepoint - if so, use darkest colors
      if (n_time_points == 1) {
          # Use darkest color from each gradient
          for (lambda_val in lambda_values) {
              for (time_point in min_time:max_time) {
                  key <- paste0(lambda_val, "_", time_point)

                  if (lambda_val %in% c(-1, -0.5)) {
                      time_colors[[key]] <- BLUE_GRADIENT_END
                  } else if (lambda_val %in% c(0.5, 1)) {
                      time_colors[[key]] <- RED_GRADIENT_END
                  } else {
                      time_colors[[key]] <- GRAY_GRADIENT_END
                  }
              }
          }
      } else {
          # Multiple timepoints - use gradient
          blue_gradient <- create_enhanced_gradient(BLUE_GRADIENT_START, BLUE_GRADIENT_END, n_time_points)
          red_gradient <- create_enhanced_gradient(RED_GRADIENT_START, RED_GRADIENT_END, n_time_points)
          gray_gradient <- create_enhanced_gradient(GRAY_GRADIENT_START, GRAY_GRADIENT_END, n_time_points)

          # Assign colors to each lambda-time combination
          for (lambda_val in lambda_values) {
              for (time_point in min_time:max_time) {
                  key <- paste0(lambda_val, "_", time_point)
                  # Map time point to gradient index (1-indexed)
                  gradient_index <- time_point - min_time + 1

                  if (lambda_val %in% c(-1, -0.5)) {
                      time_colors[[key]] <- blue_gradient[gradient_index]
                  } else if (lambda_val %in% c(0.5, 1)) {
                      time_colors[[key]] <- red_gradient[gradient_index]
                  } else {
                      time_colors[[key]] <- gray_gradient[gradient_index]
                  }
              }
          }
      }

      # Convert to named vector
      time_color_vector <- unlist(time_colors)

      # Pre-compute jittered x positions (only jitter if multiple timepoints)
      # and add exact color for error bars to match points
      set.seed(seed)
      if (n_time_points > 1) {
          time_means <- time_means %>%
              mutate(
                  lambda_jittered = lambda + runif(n(), -jitter_amount, jitter_amount),
                  error_bar_color = time_color_vector[lambda_time_key]
              )
      } else {
          time_means <- time_means %>%
              mutate(
                  lambda_jittered = lambda,
                  error_bar_color = time_color_vector[lambda_time_key]
              )
      }

      # Mark first/last timepoints for labeling (only if label_endpoints = TRUE and multiple timepoints)
      # Create labels with just the number (e.g., "1", "20")
      # First timepoint gets dark text (on light dot), last timepoint gets light text (on dark dot)
      # Size 4.5 for two-digit labels, size 5 for one-digit labels
      if (label_endpoints && n_time_points > 1) {
          time_means <- time_means %>%
              mutate(
                  is_labeled = !!sym(time_var) %in% c(min_time, max_time),
                  point_label = ifelse(is_labeled,
                                       as.character(!!sym(time_var)),
                                       NA_character_),
                  label_color = case_when(
                      !!sym(time_var) == min_time ~ "grey30",
                      !!sym(time_var) == max_time ~ "white",
                      TRUE ~ NA_character_
                  ),
                  label_size = ifelse(nchar(point_label) > 1, 4.5, 5)
              )
      } else {
          time_means <- time_means %>%
              mutate(
                  is_labeled = FALSE,
                  point_label = NA_character_,
                  label_color = NA_character_,
                  label_size = NA_real_
              )
      }

      # -------------------------------------------------------------------------
      # Step 6: Create the plot
      # -------------------------------------------------------------------------

      p <- ggplot() +
          # Time means with enhanced color gradients
          # Model prediction ribbon
          geom_ribbon(
              data = all_preds_df,
              aes(x = lambda, ymin = conf.low, ymax = conf.high),
              alpha = 0.3,
              fill = "#9f899f"
          ) +
        # SE error bars for raw means (using pre-jittered x, exact color match)
          geom_linerange(
              data = time_means,
              aes(x = lambda_jittered, ymin = time_mean - time_se,
                  ymax = time_mean + time_se, color = I(error_bar_color)),
              linewidth = 0.5,
              alpha = 0.3,
              show.legend = FALSE
          ) +
        # Unlabeled points (normal size)
        geom_point(
              data = time_means %>% filter(!is_labeled),
              aes(x = lambda_jittered, y = time_mean, fill = lambda_time_key),
              size = POINT_SIZE,
              alpha = POINT_ALPHA,
              shape = POINT_SHAPE,
              color = POINT_COLOR,
              stroke = POINT_STROKE
          ) +
          # Labeled points (larger, for first/last timepoints)
          geom_point(
              data = time_means %>% filter(is_labeled),
              aes(x = lambda_jittered, y = time_mean, fill = lambda_time_key),
              size = POINT_SIZE * 1.6,
              alpha = POINT_ALPHA,
              shape = POINT_SHAPE,
              color = POINT_COLOR,
              stroke = POINT_STROKE
          ) +
          # Labels inside the larger points (just the number)
          # Dark text on light (first) dots, light text on dark (last) dots
          # Size varies: 4.5 for two-digit labels, 5 for one-digit labels
          geom_text(
              data = time_means %>% filter(is_labeled),
              aes(x = lambda_jittered, y = time_mean, label = point_label,
                  color = I(label_color), size = label_size),
              fontface = "bold",
              show.legend = FALSE
          ) +
          scale_size_identity() +
          # Model prediction line with gradient
          geom_line(
              data = all_preds_df,
              aes(x = lambda, y = predicted, color = lambda),
              linewidth = LINE_WIDTH
          ) +
          # Vertical line at lambda = 0
          geom_vline(xintercept = 0, color = "grey", linetype = "dashed", alpha = 0.7, linewidth = 1) +
          scale_color_gradientn(
              colors = TIME_GRADIENT_COLORS,
              values = scales::rescale(c(-1.0, -0.3, -0.1, 0.0, 0.1, 0.3, 1.0)),
              guide = "none"
          ) +
          scale_fill_manual(
              values = time_color_vector,
              guide = "none"
          ) +
          scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1),
                           labels = c("-1", "-0.5", "0", "+0.5", "+1")) +
          facet_wrap(~construct, ncol = ncol, scales = ifelse(free_y, "free_y", "fixed"),
                     labeller = labeller(construct = format_construct_labels)) +
          labs(
              x = x_label,
              y = y_label
              # title = title,
              # subtitle = subtitle
          ) +
          theme_minimal() +
          theme_pub() +
          theme(legend.position = legend_position,
                strip.clip = "off") +
          guides(color = guide_legend(nrow = legend_nrow))

      # Add y-axis limits if specified
      if (!is.null(y_limits)) {
          p <- p + coord_cartesian(ylim = y_limits)
      }

      # Add y-axis breaks if specified
      if (!is.null(y_breaks)) {
          p <- p + scale_y_continuous(breaks = y_breaks)
      }

      # Add polynomial coefficient annotations if available
      if (!is.null(poly_labels)) {
          # Add each line separately with proper vertical positioning
          for (line_num in sort(unique(poly_labels$line_order))) {
              line_data <- poly_labels[poly_labels$line_order == line_num, ]
              # Extra spacing for lines with superscripts
              spacing <- ifelse(line_num == 1, 0, ifelse(line_num == 2, 1.2, 2.6))
              vjust_position <- 1.5 + spacing

              p <- p + geom_text(
                  data = line_data,
                  aes(x = -1.0, y = Inf, label = label),
                  hjust = 0, vjust = vjust_position, size = 6, color = "black",
                  parse = TRUE
              )
          }
      }

      return(p)
  }


# -------------------------------------------------------------------------
# Domain moderation plot for specific constructs
# -------------------------------------------------------------------------

plot_domain_moderation_specific <- function(models_list_long, data_long,
                                           moderation_results_long,
                                           width = 12, height = 6) {
    # Extract interaction test results for longitudinal only
    interaction_long <- moderation_results_long$domain$family_all %>%
        filter(test_id == "interaction_coarsened", construct == "helpfulness") %>%
        select(construct, estimate, p.value.adj.family) %>%
        mutate(
            sig_stars = case_when(
                p.value.adj.family < 0.001 ~ "***",
                p.value.adj.family < 0.01 ~ "**",
                p.value.adj.family < 0.05 ~ "*",
                TRUE ~ ""
            ),
            label = sprintf("p = %.3f%s", p.value.adj.family, sig_stars),
            study = "Longitudinal",
            study_construct = "Longitudinal\nHelpfulness"
        )

    # Generate predictions
    lambda_seq <- seq(-1, 1, by = 0.01)

    # Longitudinal helpfulness predictions
    model_long_help <- models_list_long$helpfulness$full_continuous
    preds_long <- ggemmeans(
        model_long_help,
        terms = c(
            paste0("lambda [", paste(lambda_seq, collapse = ", "), "]"),
            "domain"
        ),
        verbose = FALSE
    )
    preds_long_df <- as.data.frame(preds_long) %>%
        rename(lambda = x, predicted = predicted, domain = group) %>%
        mutate(
            study = "Longitudinal",
            construct = "helpfulness",
            study_construct = "Longitudinal\nHelpfulness"
        )

    # Use only longitudinal predictions
    all_preds_df <- preds_long_df %>%
        mutate(
            study_construct = factor(
                study_construct,
                levels = c("Longitudinal\nHelpfulness")
            )
        )

    # Calculate raw means for longitudinal data only
    raw_means_combined <- data_long %>%
        filter(construct == "helpfulness") %>%
        group_by(lambda, domain) %>%
        summarise(
            mean_value = mean(outcome_value, na.rm = TRUE),
            se = sd(outcome_value, na.rm = TRUE) / sqrt(sum(!is.na(outcome_value))),
            ci_lower = mean_value - 1.96 * se,
            ci_upper = mean_value + 1.96 * se,
            .groups = "drop"
        ) %>%
        mutate(
            study = "Longitudinal",
            construct = "helpfulness",
            study_construct = "Longitudinal\nHelpfulness"
        )

    interaction_labels <- interaction_long %>%
        mutate(
            study_construct = factor(
                study_construct,
                levels = c("Longitudinal\nHelpfulness")
            )
        )

    # Create plot
    p <- ggplot() +
        # Confidence ribbons
        geom_ribbon(
            data = all_preds_df,
            aes(x = lambda, ymin = conf.low, ymax = conf.high, fill = domain),
            alpha = 0.2
        ) +
        # Raw means with error bars
        geom_errorbar(
            data = raw_means_combined,
            aes(x = lambda, ymin = ci_lower, ymax = ci_upper, color = domain),
            width = 0.05, linewidth = 0.6, alpha = 0.4
        ) +
        geom_point(
            data = raw_means_combined,
            aes(x = lambda, y = mean_value, color = domain),
            size = 5, alpha = 0.6
        ) +
        # Predicted lines by domain - polchat (dashed)
        geom_line(
            data = all_preds_df %>% filter(domain == "polchat"),
            aes(x = lambda, y = predicted),
            linewidth = 2.5,
            color = polchat_color,
            linetype = "dashed"
        ) +
        # Predicted lines by domain - emotchat (solid)
        geom_line(
            data = all_preds_df %>% filter(domain == "emotchat"),
            aes(x = lambda, y = predicted),
            linewidth = 2.5,
            color = domain_color,
            linetype = "solid"
        ) +
        # Vertical line at lambda = 0
        geom_vline(
            xintercept = 0,
            linetype = "dashed",
            color = "black",
            alpha = 0.6
        ) +
        # Interaction annotations
        geom_text(
            data = interaction_labels,
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.1, size = 5, color = "black",
            fontface = "bold"
        ) +
        # No faceting needed since only longitudinal data
        # facet_wrap(~study_construct, ncol = 1, scales = "free_y") +
        scale_color_manual(
            values = c("polchat" = polchat_color, "emotchat" = domain_color),
            labels = c("polchat" = "Political", "emotchat" = "Emotional")
        ) +
        scale_fill_manual(
            values = c("polchat" = polchat_color, "emotchat" = domain_color),
            labels = c("polchat" = "Political", "emotchat" = "Emotional")
        ) +
        labs(
            x = expression(lambda),
            y = "",
            title = paste0("Domain x ", expression(lambda), " Interaction (Longitudinal)"),
            subtitle = "Helpfulness ratings across lambda values by domain.",
            color = "Domain",
            fill = "Domain"
        ) +
        theme_minimal() +
        theme_pub() +
        theme(
            legend.position = "bottom",
            legend.key.width = unit(1.5, "cm")
        )

    return(p)
}

# Create a legend plot for treatments
treatment_legend <- function() {
    # Create dummy data for legend
    dummy_data <- data.frame(
        treatment = c("RS", "Emotional Domain", "Pers", "Time"),
        value = c(1, 1, 1, 1),
        color = c(rs_color, domain_color, pers_color, time_color),
        label = c("Relationship Seeking", "Emotional Domain", "Personalisation", "Time")
    )

    # Set factor levels with proper labels
    dummy_data$treatment <- factor(dummy_data$treatment,
                                  levels = c("RS", "Emotional Domain", "Pers", "Time"),
                                  labels = c("Relationship Seeking", "Emotional Domain", "Personalisation", "Time"))

    p <- ggplot(dummy_data, aes(x = treatment, y = value, fill = treatment)) +
        geom_col() +
        scale_fill_manual(
            values = c("Relationship Seeking" = rs_color,
                      "Emotional Domain" = domain_color,
                      "Personalisation" = pers_color,
                      "Time" = time_color),
            name = NULL
        ) +
        theme_void() +
        # Increase size of text
        theme(legend.text = element_text(size = 16), legend.title = element_text(size = 18)) +
        guides(fill = guide_legend(ncol = 4, title.position = "left"))

    return(p)
}

# Study type legend (filled vs outlined bars)
study_legend <- function() {
    dummy_data <- data.frame(
        study = c("Single Exposure", "Month Exposure"),
        value = c(1, 1)
    )

    p <- ggplot(dummy_data, aes(x = study, y = value, fill = study)) +
        geom_col(aes(color = study), linewidth = 1) +
        scale_fill_manual(
            values = c("Month Exposure" = "#000000", "Single Exposure" = "transparent"),
            breaks = c("Single Exposure", "Month Exposure"),
            name = NULL
        ) +
        scale_color_manual(
            values = c("Month Exposure" = "#000000", "Single Exposure" = "#000000"),
            breaks = c("Single Exposure", "Month Exposure"),
            name = NULL
        ) +
        theme_void() +
                theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18, hjust = 0),
        ) +
        guides(
            fill = guide_legend(
            nrow = 1,
            byrow = TRUE,
            title.position = "left",
            override.aes = list(color = "#000000")
            ),
            color = "none"
        )

    return(p)
}

# Forest plot style treatment effects
# Expects data frames with columns: construct, estimate, lower.CL, upper.CL, p.value.adj.family
plot_treatment_effects_forest <- function(rs_results_long, pers_results_long, domain_results_long = NULL, rs_results_cross = NULL, pers_results_cross = NULL, domain_results_cross = NULL, custom_order = NULL, ncol = 1, height = 8, width = 12, title = "Treatment Effects", subtitle = "Pairwise Contrasts from Marginal Means", y_label = "Estimated Treatment Effect", point_size = 3, legend_position = "bottom", legend_nrow = 1, dodge_width = 0.65, convert_odds_ratios = FALSE) {
  # Build data list from provided data frames
  data_list <- list()

  if (!is.null(rs_results_long) && is.data.frame(rs_results_long) && nrow(rs_results_long) > 0) {
    rs_results_long$treatment <- "RS"
    data_list <- c(data_list, list(rs_results_long))
  }

  if (!is.null(pers_results_long) && is.data.frame(pers_results_long) && nrow(pers_results_long) > 0) {
    pers_results_long$treatment <- "Personalisation"
    data_list <- c(data_list, list(pers_results_long))
  }

  if (!is.null(domain_results_long) && is.data.frame(domain_results_long) && nrow(domain_results_long) > 0) {
    domain_results_long$treatment <- "Domain"
    data_list <- c(data_list, list(domain_results_long))
  }

  # Combine all data
  if (length(data_list) == 0) {
    stop("No data provided")
  }

  # Ensure all data frames have consistent columns
  common_cols <- c("contrast", "estimate", "SE", "df", "lower.CL", "upper.CL",
                   "t.ratio", "p.value", "construct", "p.value.adj", "test_id",
                   "p.value.adj.family", "treatment")

  # Only keep common columns from each data frame
  data_list <- lapply(data_list, function(df) {
    available_cols <- intersect(names(df), common_cols)
    df[, available_cols, drop = FALSE]
  })

  data <- do.call(rbind, data_list)
  rownames(data) <- NULL

  # Filter to only include constructs in custom_order if provided
  if (!is.null(custom_order)) {
    data <- data[data$construct %in% custom_order, ]
  }

  # Convert to odds ratios if requested
  if (convert_odds_ratios) {
    data$estimate <- exp(data$estimate)
    data$lower.CL <- exp(data$lower.CL)
    data$upper.CL <- exp(data$upper.CL)

    # Update y_label if it's still the default
    if (y_label == "Estimated Treatment Effect") {
      y_label <- "Estimated Treatment Effect (Odds Ratio)"
    }
  }

  # Add significance stars
  data$stars <- ifelse(data$p.value.adj.family < 0.001, "***",
                      ifelse(data$p.value.adj.family < 0.01, "**",
                            ifelse(data$p.value.adj.family < 0.05, "*", "")))

  # Add transparency based on significance
  data$alpha_val <- ifelse(data$p.value.adj.family > 0.05, 0.3, 1.0)

  # Create construct factor for y-axis positioning (reversed for bottom-to-top display)
  if (!is.null(custom_order)) {
    data$construct <- factor(data$construct, levels = rev(custom_order))
  } else {
    data$construct <- factor(data$construct)
  }

  # Define treatment colors using global color definitions
  treatment_colors <- c("RS" = rs_color, "Personalisation" = pers_color, "Domain" = domain_color)

  # Set treatment factor levels to control dodged position order (Domain, Pers, RS)
  data$treatment <- factor(data$treatment, levels = c("Domain", "Personalisation", "RS"))

  # Forest plot with proper dodging by treatment
  vline_intercept <- ifelse(convert_odds_ratios, 1, 0)
  p <- ggplot(data, aes(x = estimate, y = construct, color = treatment)) +
    geom_vline(xintercept = vline_intercept, linetype = "dashed", color = "black", alpha = 0.7) +

    # Error bars with dodging
    geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL, alpha = alpha_val),
                  width = 0.1, linewidth = 0.8, position = position_dodge(width = dodge_width)) +

    # Points for estimates with dodging
    geom_point(aes(alpha = alpha_val), size = point_size, position = position_dodge(width = dodge_width)) +

    # Significance stars (inherit color from treatment)
    geom_text(aes(label = stars, x = estimate, y = construct, group = treatment),
              size = 7, hjust = 0.5, vjust = -0.03, show.legend = FALSE,
              position = position_dodge(width = dodge_width)) +

    # Colors
    scale_color_manual(
      values = treatment_colors,
      breaks = c("RS", "Personalisation", "Domain"),
      labels = c("RS" = expression("Relationship-Seeking " * lambda * ">0 (vs " * lambda * "<0)"),
                 "Personalisation" = "Personalised (vs Non-Personalised)",
                 "Domain" = "Emotional Domain (vs Political)")
    ) +
    scale_alpha_identity() +

    # Theming
    theme_minimal() +
    theme_pub() +
    theme(legend.position = legend_position,
          axis.text.y = element_text(size = 14),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +

    # Format y-axis labels using custom dictionary with newlines for spaces
    scale_y_discrete(labels = format_construct_labels_forest) +

    # Add custom x-axis breaks for odds ratios (dynamic limits)
    {if (convert_odds_ratios) {
      # Calculate limits dynamically based on data
      x_min <- min(c(data$lower.CL, data$estimate), na.rm = TRUE)
      x_max <- max(c(data$upper.CL, data$estimate), na.rm = TRUE)
      # Add padding
      x_range <- x_max - x_min
      x_min <- x_min - 0.1 * x_range
      x_max <- x_max + 0.1 * x_range
      # Round to nice values
      x_min <- floor(x_min * 20) / 20
      x_max <- ceiling(x_max * 20) / 20
      scale_x_continuous(
        limits = c(x_min, x_max)
      )
    }} +

    labs(
        #  subtitle = subtitle,
        # title = title,
         x = y_label,
         y = NULL,
         color = "Treatment") +

    guides(color = guide_legend(nrow = legend_nrow))

  return(p)
}

# Forest plot style time coefficients
# Expects data frame with columns: construct, coefficient, estimate, lower.CL, upper.CL, p.value.adj.family
plot_time_coeffs_forest <- function(temporal_results_long, time_var = "session_numeric", custom_order = c("engagingness", "likeability", "helpfulness"), ncol = 1, height = 8, width = 12, title = "Treatment × Time Interactions", subtitle = "Main Effects (Grey) and Interaction Effects (Colored)", y_label = NULL, point_size = 3, legend_position = "bottom", legend_nrow = 1, dodge_width = 0.65) {
  # Set dynamic y_label based on time_var if not provided
  if (is.null(y_label)) {
    time_unit <- if (grepl("session", time_var)) "Session" else if (grepl("week", time_var)) "Week" else "Time Unit"
    y_label <- paste0("Rate of Change (per ", time_unit, ")")
  }

  data <- temporal_results_long

  # Extract treatment from coefficient column
  data$treatment <- ifelse(data$coefficient == time_var, "Time",
                          ifelse(grepl(paste0("lambda:", time_var), data$coefficient), "RS",
                                ifelse(grepl(paste0("personalisationpersonalised:", time_var), data$coefficient), "Pers",
                                      ifelse(grepl(paste0("domainemotchat:", time_var), data$coefficient), "Domain", "Other"))))

  # Set treatment order and factor for dodged positioning (Domain, Pers, RS, Time)
  data$treatment <- factor(data$treatment, levels = c("Domain", "Pers", "RS", "Time"))

  # Set construct order (reversed for bottom-to-top display)
  data$construct <- factor(data$construct, levels = rev(custom_order))

  # Add significance stars
  data$stars <- ifelse(data$p.value.adj.family < 0.001, "***",
                      ifelse(data$p.value.adj.family < 0.01, "**",
                            ifelse(data$p.value.adj.family < 0.05, "*", "")))

  # Add transparency based on significance
  data$alpha_val <- ifelse(data$p.value.adj.family > 0.05, 0.3, 1.0)

  # Define treatment colors - grey for Time, colored for treatments
  treatment_colors <- c("Time" = time_color, "RS" = rs_color, "Pers" = pers_color, "Domain" = domain_color)

  # Forest plot with proper dodging by treatment
  p <- ggplot(data, aes(x = estimate, y = construct, color = treatment)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +

    # Error bars with dodging
    geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL, alpha = alpha_val),
                  width = 0.1, linewidth = 0.8, position = position_dodge(width = dodge_width)) +

    # Points for estimates with dodging
    geom_point(aes(alpha = alpha_val), size = point_size, position = position_dodge(width = dodge_width)) +

    # Significance stars (inherit color from treatment)
    geom_text(aes(label = stars, x = estimate, y = construct, group = treatment),
              size = 7, hjust = 0.5, vjust = -0.03, show.legend = FALSE,
              position = position_dodge(width = dodge_width)) +

    # Colors
    scale_color_manual(
      values = treatment_colors,
      breaks = c("RS", "Pers", "Domain", "Time"),
      labels = c("RS" = expression("Relationship-Seeking " * lambda * ">0 (vs " * lambda * "<0)"),
                 "Pers" = "Personalised (vs Non-Personalised)",
                 "Domain" = "Emotional Domain (vs Political)",
                 "Time" = "Time")
    ) +
    scale_alpha_identity() +

    # Theming
    theme_minimal() +
    theme_pub() +
    theme(legend.position = legend_position,
          axis.text.y = element_text(size = 14),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +

    # Format y-axis labels using custom dictionary with newlines for spaces
    scale_y_discrete(labels = format_construct_labels_forest) +

    labs(
    #   title = title,
        #  subtitle = subtitle,
         x = y_label,
         y = NULL,
         color = "Treatment") +
    guides(color = guide_legend(nrow = legend_nrow))

  return(p)
}


## TIME PLOTS ##

# =============================================================================
# HELPER FUNCTION: Get averaged predictions for lambda groups
# =============================================================================

get_temporal_predictions <- function(model, 
                                    terms,
                                    data,
                                    time_var = "session_numeric",
                                    lambda_values = c(-1, -0.5, 0.5, 1)) {
    # Determine time cutpoints based on time_var
    if (time_var == "week_numeric") {
        time_points <- c(1, 2, 3, 4)
    } else if (time_var == "session_numeric") {
        time_points <- c(1, 20)
    } else {
        # Default: use min and max
        time_points <- range(data[[time_var]], na.rm = TRUE)
    }
    
    # Parse terms to identify focal predictors
    has_lambda <- "lambda" %in% terms
    has_personalisation <- "personalisation" %in% terms
    has_domain <- "domain" %in% terms
    
    # Build emmeans formula
    formula_parts <- c()
    at_list <- list()
    
    # Always include time variable
    formula_parts <- c(formula_parts, time_var)
    at_list[[time_var]] <- time_points
    
    # Add lambda if specified
    if (has_lambda) {
        formula_parts <- c(formula_parts, "lambda")
        at_list[["lambda"]] <- lambda_values
    }
    
    # Add personalisation if specified
    if (has_personalisation) {
        formula_parts <- c(formula_parts, "personalisation")
    }
    
    # Add domain if specified
    if (has_domain) {
        formula_parts <- c(formula_parts, "domain")
    }
    
    # Create formula
    emm_formula <- as.formula(paste("~", paste(formula_parts, collapse = " * ")))
    
    # Get emmeans
    emm <- emmeans(model, emm_formula, at = at_list, data = data)
    
    # If lambda is included, average over positive and negative groups
    if (has_lambda) {
        # Determine grouping variables for 'by' argument
        by_vars <- c(time_var)
        if (has_personalisation) by_vars <- c(by_vars, "personalisation")
        if (has_domain) by_vars <- c(by_vars, "domain")
        
        # Create contrast to average lambda groups
        n_lambda <- length(lambda_values)
        n_neg <- sum(lambda_values < 0)
        n_pos <- sum(lambda_values > 0)
        
        # Create weights
        weights_neg <- c(rep(1/n_neg, n_neg), rep(0, n_pos))
        weights_pos <- c(rep(0, n_neg), rep(1/n_pos, n_pos))
        
        preds_averaged <- contrast(emm,
            method = list(
                "Negative_RS" = weights_neg,
                "Positive_RS" = weights_pos
            ),
            by = by_vars)
        
        # Convert to dataframe
        preds_df <- as.data.frame(summary(preds_averaged, infer = TRUE)) %>%
            rename(
                predicted = estimate,
                std.error = SE,
                conf.low = lower.CL,
                conf.high = upper.CL
            )
        
        # Rename columns to match ggemmeans format
        preds_df <- preds_df %>%
            rename(x = !!time_var) %>%
            mutate(
                group = contrast,
                group = recode(group,
                             "Negative_RS" = "neg_lambda",
                             "Positive_RS" = "pos_lambda")
            )
        
        # Add facet column if domain or personalisation exists
        if (has_domain && has_personalisation) {
            preds_df <- preds_df %>%
                mutate(facet = paste(domain, personalisation, sep = " × "))
        } else if (has_domain) {
            preds_df <- preds_df %>%
                rename(facet = domain)
        } else if (has_personalisation) {
            preds_df <- preds_df %>%
                rename(facet = personalisation)
        }
        
    } else {
        # No lambda - just return emmeans as-is
        preds_df <- as.data.frame(summary(emm, infer = TRUE)) %>%
            rename(
                x = !!time_var,
                predicted = emmean,
                std.error = SE,
                conf.low = lower.CL,
                conf.high = upper.CL
            )
        
        # Create group column based on what's available
        if (has_personalisation && has_domain) {
            preds_df <- preds_df %>%
                mutate(
                    group = personalisation,
                    facet = domain
                )
        } else if (has_personalisation) {
            preds_df <- preds_df %>%
                mutate(group = personalisation)
        } else if (has_domain) {
            preds_df <- preds_df %>%
                mutate(group = domain)
        }
    }
    
    # Select and order columns to match ggemmeans format
    final_cols <- c("x", "predicted", "std.error", "conf.low", "conf.high")
    if ("group" %in% names(preds_df)) final_cols <- c(final_cols, "group")
    if ("facet" %in% names(preds_df)) final_cols <- c(final_cols, "facet")
    
    preds_df <- preds_df %>%
        select(any_of(final_cols))
    
    return(preds_df)
}


# =============================================================================
# HELPER FUNCTION: Extract and format slope data based on terms
# =============================================================================

get_slope_data <- function(slope_results, terms, p_value_col = "p_global") {
    # Check if this is JSON format (has 'contrasts' field)
    if ("contrasts" %in% names(slope_results)) {
        return(get_slope_data_from_json(slope_results, terms, p_value_col))
    }

    # Otherwise use legacy RDS format handling
    return(get_slope_data_legacy(slope_results, terms))
}

# Helper for JSON format slope data
get_slope_data_from_json <- function(json_data, terms, p_value_col = "p_global") {
    if (!"lambda" %in% terms) return(NULL)

    # Determine slope_type based on terms
    # Just lambda = 1way, lambda + domain/pers = 2way, all three = 3way
    if ("personalisation" %in% terms && "domain" %in% terms) {
        target_slope_type <- "3way"
    } else if ("personalisation" %in% terms || "domain" %in% terms) {
        target_slope_type <- "2way"
    } else {
        target_slope_type <- "1way"
    }

    # Filter to temporal_slope contrasts with rs_level (neg_λ or pos_λ) and correct slope_type
    slopes <- json_data$contrasts[sapply(json_data$contrasts, function(c) {
        c$test_type == "temporal_slope" &&
        !is.null(c$rs_level) &&
        c$rs_level %in% c("neg_λ", "pos_λ") &&
        !is.null(c$slope_type) &&
        c$slope_type == target_slope_type
    })]

    if (length(slopes) == 0) return(NULL)

    # Convert to data frame
    slope_df <- do.call(rbind, lapply(slopes, function(s) {
        # Get p-value based on preference
        p_val <- switch(p_value_col,
            "p_global" = s$primary$p_global,
            "p_local" = s$primary$p_local,
            "p_raw" = s$primary$p_raw,
            s$primary$p_global
        )
        if (is.null(p_val) || length(p_val) == 0) p_val <- s$primary$p_raw

        data.frame(
            construct = s$outcome,
            lambda_group = ifelse(s$rs_level == "neg_λ", "neg_lambda", "pos_lambda"),
            estimate = s$primary$estimate,
            p.value.adj = p_val,
            stringsAsFactors = FALSE
        )
    }))

    # Add facet_match and slope_text
    slope_df$facet_match <- slope_df$construct
    slope_df$slope_text <- sprintf("%.2f%s", slope_df$estimate,
        ifelse(slope_df$p.value.adj < 0.001, "***",
            ifelse(slope_df$p.value.adj < 0.01, "**",
                ifelse(slope_df$p.value.adj < 0.05, "*", ""))))

    return(slope_df)
}

# Legacy RDS format handling
get_slope_data_legacy <- function(slope_results, terms) {
    slope_df <- NULL

    # Determine which slope dataset to use based on terms
    if ("lambda" %in% terms) {
        if ("personalisation" %in% terms && "domain" %in% terms) {
            # 3-way interaction: rs × personalisation × domain
            if ("slopes_3way" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_3way
            } else if ("slopes_3way_coarsened" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_3way_coarsened
            }
        } else if ("personalisation" %in% terms) {
            # 2-way interaction: rs × personalisation
            if ("slopes_rs_pers" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_rs_pers
            } else if ("slopes_rs_pers_coarsened" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_rs_pers_coarsened
            }
        } else if ("domain" %in% terms) {
            # 2-way interaction: rs × domain
            if ("slopes_rs_domain" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_rs_domain
            } else if ("slopes_rs_domain_coarsened" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_rs_domain_coarsened
            }
        } else {
            # Just lambda: main effect
            if ("slopes_rs" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_rs
            } else if ("slopes_rs_coarsened" %in% names(slope_results)) {
                slope_df <- slope_results$slopes_rs_coarsened
            }
        }

        if (is.null(slope_df)) return(NULL)

        # If we have a contrast column, split it
        if ("contrast" %in% names(slope_df)) {
            # Split contrast column by ":"
            contrast_parts <- strsplit(slope_df$contrast, ":")

            if ("personalisation" %in% terms && "domain" %in% terms) {
                # 3-way: personalisation:domain:lambda
                slope_df$personalisation <- sapply(contrast_parts, function(x) x[1])
                slope_df$domain <- sapply(contrast_parts, function(x) x[2])
                slope_df$lambda_group <- sapply(contrast_parts, function(x) x[3])
            } else if ("personalisation" %in% terms) {
                # 2-way: personalisation:lambda
                slope_df$personalisation <- sapply(contrast_parts, function(x) x[1])
                slope_df$lambda_group <- sapply(contrast_parts, function(x) x[2])
            } else if ("domain" %in% terms) {
                # 2-way: domain:lambda
                slope_df$domain <- sapply(contrast_parts, function(x) x[1])
                slope_df$lambda_group <- sapply(contrast_parts, function(x) x[2])
            } else {
                # Main effect: just lambda
                slope_df$lambda_group <- slope_df$contrast
            }
        } else {
            # No contrast column - already has the right column structure
            if ("relationship_seeking_category" %in% names(slope_df)) {
                slope_df$lambda_group <- slope_df$relationship_seeking_category
            } else if ("lambda" %in% names(slope_df)) {
                # Handle attachment study format where we have lambda column instead of contrast
                slope_df$lambda_group <- ifelse(slope_df$lambda == "neg_lambda", "neg_lambda", "pos_lambda")
            }
        }

        # Clean up lambda group names
        slope_df$lambda_group <- ifelse(slope_df$lambda_group == "neg_lambda", "neg_lambda",
                                       ifelse(slope_df$lambda_group == "pos_lambda", "pos_lambda",
                                             slope_df$lambda_group))

        # Add facet_match column based on terms
        if ("personalisation" %in% terms && "domain" %in% terms) {
            slope_df$facet_match <- paste(slope_df$domain, slope_df$construct, sep = "-")
        } else if ("domain" %in% terms) {
            slope_df$facet_match <- paste(slope_df$domain, slope_df$construct, sep = "-")
        } else if ("personalisation" %in% terms) {
            slope_df$facet_match <- paste(slope_df$personalisation, slope_df$construct, sep = "-")
        } else {
            slope_df$facet_match <- slope_df$construct
        }

        # Determine the correct trend column name
        trend_col <- NULL
        if ("session_numeric.trend" %in% names(slope_df)) {
            trend_col <- "session_numeric.trend"
        } else if ("week_numeric.trend" %in% names(slope_df)) {
            trend_col <- "week_numeric.trend"
        } else if ("lambda.trend" %in% names(slope_df)) {
            trend_col <- "lambda.trend"
        } else if ("estimate" %in% names(slope_df)) {
            # Handle attachment study format where estimate is the slope value
            trend_col <- "estimate"
        }

        # Add slope text with significance stars
        if (!is.null(trend_col)) {
            slope_df$slope_text <- sprintf("%.2f%s", slope_df[[trend_col]],
                                         ifelse(slope_df$p.value.adj < 0.001, "***",
                                               ifelse(slope_df$p.value.adj < 0.01, "**",
                                                     ifelse(slope_df$p.value.adj < 0.05, "*", ""))))
        } else {
            # Fallback - no trend column found
            slope_df$slope_text <- "N/A"
        }

    } else {
        # No lambda in terms - return NULL (no slopes to show)
        return(NULL)
    }

    return(slope_df)
}

# =============================================================================
# PLOT FUNCTION: Plot temporal predictions with colored lines for lambda groups
# =============================================================================

plot_temporal_predictions_colored <- function(mods_longitudinal, data_longitudinal, terms,
                                              slope_results = NULL,
                                              time_var = "session_numeric",
                                              lambda_values = c(-1, -0.5, 0.5, 1),
                                              custom_order = NULL,
                                              add_lines = FALSE,
                                              ncol = 6,
                                              width = 12, height = 8,
                                              title = "Trend Estimates with Session Means",
                                              subtitle = NULL,
                                              legend_nrow = 1,
                                              legend_position = "bottom",
                                              plot_baseline_adjusted_raw_means = FALSE,
                                              plot_delta_from_s1_baseline = FALSE) {
    # Get construct names
    construct_names <- names(mods_longitudinal)

    # Filter to custom order if provided
    if (!is.null(custom_order)) {
        # Filter construct_names to only those in custom_order that also exist in models
        construct_names <- intersect(custom_order, construct_names)
    }

    # Loop through constructs and get predictions
    all_preds <- list()

    for (construct_name in construct_names) {
        # Filter data for this construct
        construct_data <- data_longitudinal %>% filter(construct == construct_name)

        # Get model
        model <- mods_longitudinal[[construct_name]]$full_continuous

        # Get predictions using helper function
        time_preds <- get_temporal_predictions(
            model = model,
            terms = terms,
            data = construct_data,
            time_var = time_var,
            lambda_values = lambda_values
        )

        # Add construct column
        time_preds$construct <- construct_name
        all_preds[[construct_name]] <- time_preds
    }

    # Check if we have any predictions
    if (length(all_preds) == 0) {
        stop("No predictions generated. Check that construct names in custom_order exist in mods_longitudinal.")
    }

    # Combine all predictions
    combined_preds <- do.call(rbind, all_preds)

    # Calculate session-by-session means grouped by lambda sign (exclude lambda = 0)
    if (plot_baseline_adjusted_raw_means) {
        # Calculate model-fitted means (for pre-post regression models)
        time_means_list <- list()

        for (construct_name in construct_names) {
            construct_data <- data_longitudinal %>%
                filter(construct == construct_name, lambda != 0)

            if (nrow(construct_data) > 0) {
                model <- mods_longitudinal[[construct_name]]$full_continuous

                # Get fitted values for each row
                fitted_values <- predict(model, newdata = construct_data)

                # Add fitted values to the data
                construct_data$fitted_value <- fitted_values

                # Calculate means by lambda group and time
                construct_means <- construct_data %>%
                    mutate(
                        lambda_group = case_when(
                            lambda < 0 ~ "neg_lambda",
                            lambda > 0 ~ "pos_lambda"
                        )
                    ) %>%
                    group_by(construct, lambda_group, !!sym(time_var)) %>%
                    summarise(
                        # Use model fitted values as the "adjusted" means
                        session_mean = mean(fitted_value, na.rm = TRUE),
                        session_sd = sd(fitted_value, na.rm = TRUE),
                        n_obs = n(),
                        session_se = session_sd / sqrt(n_obs),
                        .groups = "drop"
                    ) %>%
                    mutate(
                        construct = factor(construct, levels = construct_names),
                        lambda_session_key = paste0(lambda_group, "_", !!sym(time_var))
                    )

                time_means_list[[construct_name]] <- construct_means
            }
        }

        if (length(time_means_list) > 0) {
            time_means <- do.call(rbind, time_means_list)
        } else {
            time_means <- data.frame()
        }
    } else if (plot_delta_from_s1_baseline) {
        # Calculate delta means starting from session 1 baseline
        time_means_list <- list()

        for (construct_name in construct_names) {
            construct_data <- data_longitudinal %>%
                filter(construct == construct_name, lambda != 0)

            if (nrow(construct_data) > 0) {
                # Calculate baseline mean for session 1 (across all lambda values for this construct)
                s1_baseline_mean <- construct_data %>%
                    filter(!!sym(time_var) == 1) %>%
                    summarise(baseline_mean = mean(outcome_value_pre, na.rm = TRUE)) %>%
                    pull(baseline_mean)

                # Calculate means by lambda group and time using delta + baseline
                construct_means <- construct_data %>%
                    mutate(
                        lambda_group = case_when(
                            lambda < 0 ~ "neg_lambda",
                            lambda > 0 ~ "pos_lambda"
                        )
                    ) %>%
                    group_by(construct, lambda_group, !!sym(time_var)) %>%
                    summarise(
                        # Use outcome_value_delta + session 1 baseline mean
                        session_mean = s1_baseline_mean + mean(outcome_value_delta, na.rm = TRUE),
                        session_sd = sd(outcome_value_delta, na.rm = TRUE),
                        n_obs = n(),
                        session_se = session_sd / sqrt(n_obs),
                        .groups = "drop"
                    ) %>%
                    mutate(
                        construct = factor(construct, levels = construct_names),
                        lambda_session_key = paste0(lambda_group, "_", !!sym(time_var))
                    )

                time_means_list[[construct_name]] <- construct_means
            }
        }

        if (length(time_means_list) > 0) {
            time_means <- do.call(rbind, time_means_list)
        } else {
            time_means <- data.frame()
        }
    } else {
        # Calculate raw means (default behavior)
        time_means <- data_longitudinal %>%
            filter(lambda != 0, construct %in% construct_names) %>%
            mutate(
                lambda_group = case_when(
                    lambda < 0 ~ "neg_lambda",
                    lambda > 0 ~ "pos_lambda"
                )
            ) %>%
            group_by(construct, lambda_group, !!sym(time_var)) %>%
            summarise(
                session_mean = mean(outcome_value, na.rm = TRUE),
                session_sd = sd(outcome_value, na.rm = TRUE),
                n_obs = n(),
                session_se = session_sd / sqrt(n_obs),
                .groups = "drop"
            ) %>%
            mutate(
                construct = factor(construct, levels = construct_names),
                lambda_session_key = paste0(lambda_group, "_", !!sym(time_var))
            )
    }

    # Determine time range for gradient mapping
    min_time_point <- min(combined_preds$x, na.rm = TRUE)
    max_time_point <- max(combined_preds$x, na.rm = TRUE)
    n_time_points <- max_time_point - min_time_point + 1

    # Define color mappings for each lambda group
    session_colors <- list()
    blue_gradient <- create_enhanced_gradient(BLUE_GRADIENT_START, BLUE_GRADIENT_END, n_time_points)
    red_gradient <- create_enhanced_gradient(RED_GRADIENT_START, RED_GRADIENT_END, n_time_points)

    # Assign colors to each lambda group-session combination
    lambda_groups <- c("neg_lambda", "pos_lambda")
    for (lambda_group in lambda_groups) {
        for (time_point in min_time_point:max_time_point) {
            key <- paste0(lambda_group, "_", time_point)
            # Map time point to gradient index (1-indexed)
            gradient_index <- time_point - min_time_point + 1

            if (lambda_group == "neg_lambda") {
                session_colors[[key]] <- blue_gradient[gradient_index]
            } else {
                session_colors[[key]] <- red_gradient[gradient_index]
            }
        }
    }

    # Convert to named vector
    session_color_vector <- unlist(session_colors)

    # Check if we have facets
    has_facet <- "facet" %in% names(combined_preds)

    # Handle facets for session means
    if (has_facet) {
        # Add facet information to session means to match combined_preds faceting
        if ("domain" %in% terms) {
            if (plot_baseline_adjusted_raw_means) {
                # Calculate model-fitted means for domain facets
                time_means_list <- list()

                for (construct_name in construct_names) {
                    construct_data <- data_longitudinal %>%
                        filter(construct == construct_name, lambda != 0)

                    if (nrow(construct_data) > 0) {
                        model <- mods_longitudinal[[construct_name]]$full_continuous

                        # Get fitted values for each row
                        fitted_values <- predict(model, newdata = construct_data)

                        # Add fitted values to the data
                        construct_data$fitted_value <- fitted_values

                        # Calculate means by lambda group, domain, and time
                        construct_means <- construct_data %>%
                            mutate(
                                lambda_group = case_when(
                                    lambda < 0 ~ "Non relationship-seeking (λ < 0)",
                                    lambda > 0 ~ "Relationship-seeking (λ > 0)"
                                )
                            ) %>%
                            group_by(construct, lambda_group, domain, !!sym(time_var)) %>%
                            summarise(
                                # Use model fitted values as the "adjusted" means
                                session_mean = mean(fitted_value, na.rm = TRUE),
                                session_sd = sd(fitted_value, na.rm = TRUE),
                                n_obs = n(),
                                session_se = session_sd / sqrt(n_obs),
                                .groups = "drop"
                            ) %>%
                            mutate(
                                construct = factor(construct, levels = construct_names),
                                lambda_session_key = paste0(lambda_group, "_", !!sym(time_var)),
                                facet = domain,
                                facet_combo = paste(domain, construct, sep = "-")
                            )

                        time_means_list[[construct_name]] <- construct_means
                    }
                }

                if (length(time_means_list) > 0) {
                    time_means <- do.call(rbind, time_means_list)
                } else {
                    time_means <- data.frame()
                }
            } else if (plot_delta_from_s1_baseline) {
                # Calculate delta means starting from session 1 baseline for domain facets
                time_means_list <- list()

                for (construct_name in construct_names) {
                    construct_data <- data_longitudinal %>%
                        filter(construct == construct_name, lambda != 0)

                    if (nrow(construct_data) > 0) {
                        # Calculate baseline mean for session 1 by domain (across all lambda values for this construct)
                        s1_baseline_by_domain <- construct_data %>%
                            filter(!!sym(time_var) == 1) %>%
                            group_by(domain) %>%
                            summarise(baseline_mean = mean(outcome_value_pre, na.rm = TRUE), .groups = "drop")

                        # Calculate means by lambda group, domain, and time using delta + baseline
                        construct_means <- construct_data %>%
                            left_join(s1_baseline_by_domain, by = "domain") %>%
                            mutate(
                                lambda_group = case_when(
                                    lambda < 0 ~ "Non relationship-seeking (λ < 0)",
                                    lambda > 0 ~ "Relationship-seeking (λ > 0)"
                                )
                            ) %>%
                            group_by(construct, lambda_group, domain, !!sym(time_var), baseline_mean) %>%
                            summarise(
                                # Use outcome_value_delta + session 1 baseline mean for this domain
                                session_mean = baseline_mean + mean(outcome_value_delta, na.rm = TRUE),
                                session_sd = sd(outcome_value_delta, na.rm = TRUE),
                                n_obs = n(),
                                session_se = session_sd / sqrt(n_obs),
                                .groups = "drop"
                            ) %>%
                            mutate(
                                construct = factor(construct, levels = construct_names),
                                lambda_session_key = paste0(lambda_group, "_", !!sym(time_var)),
                                facet = domain,
                                facet_combo = paste(domain, construct, sep = "-")
                            ) %>%
                            select(-baseline_mean)  # Remove the helper column

                        time_means_list[[construct_name]] <- construct_means
                    }
                }

                if (length(time_means_list) > 0) {
                    time_means <- do.call(rbind, time_means_list)
                } else {
                    time_means <- data.frame()
                }
            } else {
                # Add domain column to session means (raw means)
                time_means <- data_longitudinal %>%
                    filter(lambda != 0) %>%
                    mutate(
                        lambda_group = case_when(
                            lambda < 0 ~ "Non relationship-seeking (λ < 0)",
                            lambda > 0 ~ "Relationship-seeking (λ > 0)"
                        )
                    ) %>%
                    group_by(construct, lambda_group, domain, !!sym(time_var)) %>%
                    summarise(
                        session_mean = mean(outcome_value, na.rm = TRUE),
                        session_sd = sd(outcome_value, na.rm = TRUE),
                        n_obs = n(),
                        session_se = session_sd / sqrt(n_obs),
                        .groups = "drop"
                    ) %>%
                    mutate(
                        construct = factor(construct, levels = construct_names),
                        lambda_session_key = paste0(lambda_group, "_", !!sym(time_var)),
                        facet = domain,
                        facet_combo = paste(domain, construct, sep = "-")
                    )
            }
        }
    }

    # Set factor ordering for proper facet order - KEEP ALL THE ORIGINAL LOGIC
    if (has_facet) {
        # Use custom order if provided, otherwise default
        if (!is.null(custom_order)) {
            # Create facet_combo directly with custom ordering
            combined_preds$facet_combo <- paste(combined_preds$facet, combined_preds$construct, sep = "-")

            # Filter to only include groups that are in custom_order
            combined_preds <- combined_preds[combined_preds$facet_combo %in% custom_order, ]
            combined_preds$facet_combo <- factor(combined_preds$facet_combo, levels = custom_order)

            # Apply same filtering to session means
            if (exists("time_means") && "facet_combo" %in% names(time_means)) {
                time_means <- time_means[time_means$facet_combo %in% custom_order, ]
                time_means$facet_combo <- factor(time_means$facet_combo, levels = custom_order)
            }

            # Sort the data frame by the factor levels to ensure correct ordering
            combined_preds <- combined_preds[order(combined_preds$facet_combo), ]

            # Drop unused factor levels
            combined_preds$facet_combo <- droplevels(combined_preds$facet_combo)
        } else {
            # Default ordering: emotchat/polchat first if they exist, otherwise alphabetical by facet
            unique_facets <- unique(combined_preds$facet)
            unique_constructs <- unique(combined_preds$construct)

            # Order facets: emotchat first if it exists, then polchat, then alphabetical
            if ("emotchat" %in% unique_facets && "polchat" %in% unique_facets) {
                facet_levels <- c("emotchat", "polchat", sort(setdiff(unique_facets, c("emotchat", "polchat"))))
            } else if ("emotchat" %in% unique_facets) {
                facet_levels <- c("emotchat", sort(setdiff(unique_facets, "emotchat")))
            } else if ("polchat" %in% unique_facets) {
                facet_levels <- c("polchat", sort(setdiff(unique_facets, "polchat")))
            } else {
                facet_levels <- sort(unique_facets)
            }

            # Order constructs alphabetically
            construct_levels <- sort(unique_constructs)

            combined_preds$construct <- factor(combined_preds$construct, levels = construct_levels)
            combined_preds$facet <- factor(combined_preds$facet, levels = facet_levels)

            # Create facet_combo with default ordering
            combined_preds$facet_combo <- interaction(combined_preds$facet, combined_preds$construct,
                sep = "-", drop = TRUE
            )
        }
    } else {
        # Just order constructs - use custom order if provided
        if (!is.null(custom_order)) {
            # Create explicitly ordered construct variable
            combined_preds$construct_ordered <- factor(combined_preds$construct, levels = custom_order)
            time_means$construct_ordered <- factor(time_means$construct, levels = custom_order)
        } else {
            # Order constructs alphabetically
            unique_constructs <- unique(combined_preds$construct)
            construct_levels <- sort(unique_constructs)
            combined_preds$construct_ordered <- factor(combined_preds$construct, levels = construct_levels)
            time_means$construct_ordered <- factor(time_means$construct, levels = construct_levels)
        }
    }

    # Set colors for lambda groups
    lambda_colors <- c()
    lambda_colors["neg_lambda"] <- "#0000FF"
    lambda_colors["pos_lambda"] <- "#FF4040"

    # Get slope data if provided
    slope_data <- NULL
    if (!is.null(slope_results)) {
        slope_data <- get_slope_data(slope_results, terms)
        # print(slope_data)

        if (!is.null(slope_data)) {
            # Filter slope data to match construct_names (which is already filtered by custom_order)
            slope_data <- slope_data %>%
                filter(construct %in% construct_names)

            # Use slope data as is (facet_match and slope_text already created in helper)
            slope_annotations <- slope_data %>%
                mutate(color_group = lambda_group)
        }
    }

    # Add error bar color to time_means (matching point fill colors)
    time_means <- time_means %>%
        mutate(error_bar_color = session_color_vector[lambda_session_key])

    # Mark first/last timepoints for labeling (only if multiple timepoints)
    # Create labels with just the number (e.g., "1", "20")
    # First timepoint gets dark text (on light dot), last timepoint gets light text (on dark dot)
    # Size 4.5 for two-digit labels, size 5 for one-digit labels
    if (n_time_points > 1) {
        time_means <- time_means %>%
            mutate(
                is_labeled = !!sym(time_var) %in% c(min_time_point, max_time_point),
                point_label = ifelse(is_labeled,
                                     as.character(!!sym(time_var)),
                                     NA_character_),
                label_color = case_when(
                    !!sym(time_var) == min_time_point ~ "grey30",
                    !!sym(time_var) == max_time_point ~ "white",
                    TRUE ~ NA_character_
                ),
                label_size = ifelse(nchar(point_label) > 1, 4.5, 5)
            )
    } else {
        time_means <- time_means %>%
            mutate(
                is_labeled = FALSE,
                point_label = NA_character_,
                label_color = NA_character_,
                label_size = NA_real_
            )
    }

    # Create plot with colored lines for lambda groups AND session means
    p <- ggplot(combined_preds, aes(x = x, y = predicted, color = group, fill = group)) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
        geom_line(linewidth = LINE_WIDTH)

    # Add connecting lines between mean points if requested (BEFORE points so they go under)
    if (add_lines) {
        # Create a simple mapping for lambda group colors
        lambda_group_colors <- c()
        lambda_group_colors["neg_lambda"] <- "#0000FF"
        lambda_group_colors["pos_lambda"] <- "#FF4040"

        p <- p + geom_line(
            data = time_means,
            aes(x = !!sym(time_var), y = session_mean, color = lambda_group),
            linetype = "dotted",
            linewidth = 1.5,
            inherit.aes = FALSE
        )
    }

    # Add SE error bars for session means (before points so they go under)
    p <- p + geom_linerange(
        data = time_means,
        aes(x = !!sym(time_var), ymin = session_mean - session_se,
            ymax = session_mean + session_se, color = I(error_bar_color)),
        linewidth = 0.5,
        alpha = 0.3,
        inherit.aes = FALSE,
        show.legend = FALSE
    )

    # Add unlabeled session means (normal size)
    p <- p + geom_point(
        data = time_means %>% filter(!is_labeled),
        aes(x = !!sym(time_var), y = session_mean, fill = lambda_session_key),
        size = POINT_SIZE,
        shape = POINT_SHAPE,
        color = POINT_COLOR,
        alpha = POINT_ALPHA,
        stroke = POINT_STROKE,
        inherit.aes = FALSE
    )

    # Add labeled session means (larger, for first/last timepoints)
    p <- p + geom_point(
        data = time_means %>% filter(is_labeled),
        aes(x = !!sym(time_var), y = session_mean, fill = lambda_session_key),
        size = POINT_SIZE * 1.6,
        shape = POINT_SHAPE,
        color = POINT_COLOR,
        alpha = POINT_ALPHA,
        stroke = POINT_STROKE,
        inherit.aes = FALSE
    )

    # Add labels inside the larger points (just the number)
    # Dark text on light (first) dots, light text on dark (last) dots
    # Size varies: 4.5 for two-digit labels, 5 for one-digit labels
    p <- p + geom_text(
        data = time_means %>% filter(is_labeled),
        aes(x = !!sym(time_var), y = session_mean, label = point_label,
            color = I(label_color), size = label_size),
        fontface = "bold",
        inherit.aes = FALSE,
        show.legend = FALSE
    ) +
    scale_size_identity()

    # Add scale_color_manual after all geom layers
    if (add_lines) {
        p <- p + scale_color_manual(
            values = c(lambda_group_colors, lambda_colors),
            name = NULL,
            labels = c("pos_lambda" = expression("Relationship-seeking AI ("~lambda~"> 0)"), "neg_lambda" = expression("Relationship-avoiding AI ("~lambda~"< 0)"))
        )
    } else {
        p <- p + scale_color_manual(
            values = lambda_colors, name = NULL,
            labels = c("pos_lambda" = expression("Relationship-seeking AI ("~lambda~"> 0)"), "neg_lambda" = expression("Relationship-avoiding AI ("~lambda~"< 0)"))
        )
    }

    p <- p +
        scale_fill_manual(
            values = c(session_color_vector, lambda_colors), name = NULL,
            labels = c("pos_lambda" = expression("Relationship-seeking AI ("~lambda~"> 0)"), "neg_lambda" = expression("Relationship-avoiding AI ("~lambda~"< 0)")),
            guide = "none"
        ) +
        labs(
            x = paste("Time (", strsplit(time_var, "_")[[1]][1], ")", sep = ""),
            y = "Predicted Trend"
            # title = title,
            # subtitle = if(is.null(subtitle)) paste0("(Blue: ", expression(lambda), "<0, Red: ", expression(lambda), ">0 | Light=early, Dark=late sessions)") else subtitle
        ) +
        theme_minimal() +
        theme_pub() +
        theme(legend.position = legend_position,
              legend.key.width = unit(2, "cm")) +
        guides(color = guide_legend(nrow = legend_nrow))

    # Facet based on available variables - construct in rows, facet (second term) in columns
    if (has_facet) {
        # Print debug info for custom ordering
        if (!is.null(custom_order)) {
            cat("Custom order provided:", paste(custom_order, collapse = ", "), "\n")
            cat("Unique facet_combo values:", paste(unique(combined_preds$facet_combo), collapse = ", "), "\n")
            cat("Factor levels:", paste(levels(combined_preds$facet_combo), collapse = ", "), "\n")
        }

        # Second term exists: all facets in one row with proper ordering
        p <- p + facet_wrap(~facet_combo,
            ncol = ncol,
            labeller = labeller(facet_combo = function(x) gsub("-", "\n", x))
        )

        # Add slope annotations if available - match to facet_combo
        if (!is.null(slope_data) && exists("slope_annotations")) {
            # Create matching facet_combo for annotations
            slope_annotations$facet_combo <- slope_annotations$facet_match

            # Apply same factor ordering and filtering to slope annotations if custom order used
            if (!is.null(custom_order)) {
                # Filter slope annotations to only include groups that are in custom_order
                slope_annotations <- slope_annotations[slope_annotations$facet_combo %in% custom_order, ]
                slope_annotations$facet_combo <- factor(slope_annotations$facet_combo, levels = custom_order)
            }

            # Add negative lambda annotations at bottom center
            p <- p + geom_text(
                data = subset(slope_annotations, color_group == "neg_lambda"),
                aes(x = mean(combined_preds$x), y = -Inf, label = slope_text),
                hjust = 0.5, vjust = -0.2, size = 8, color = "blue",
                show.legend = FALSE, inherit.aes = FALSE
            )

            # Add positive lambda annotations at top center
            p <- p + geom_text(
                data = subset(slope_annotations, color_group == "pos_lambda"),
                aes(x = mean(combined_preds$x), y = Inf, label = slope_text),
                hjust = 0.5, vjust = 1.2, size = 8, color = "red",
                show.legend = FALSE, inherit.aes = FALSE
            )
        }
    } else {
        # Only lambda: all constructs in one row - USE construct_ordered
        if (!is.null(custom_order)) {
            p <- p + facet_wrap(~construct_ordered, ncol = length(custom_order),
                               labeller = labeller(construct_ordered = format_construct_labels))
        } else {
            p <- p + facet_wrap(~construct_ordered, ncol = length(unique(combined_preds$construct)),
                               labeller = labeller(construct_ordered = format_construct_labels))
        }

        # Add slope annotations if available - match to construct
        if (!is.null(slope_data) && exists("slope_annotations")) {
            # Create construct_ordered for slope annotations to match faceting
            if (!is.null(custom_order)) {
                slope_annotations$construct_ordered <- factor(slope_annotations$construct, levels = custom_order)
            } else {
                unique_constructs <- unique(combined_preds$construct)
                construct_levels <- sort(unique_constructs)
                slope_annotations$construct_ordered <- factor(slope_annotations$construct, levels = construct_levels)
            }

            # Add negative lambda annotations at bottom center
            p <- p + geom_text(
                data = subset(slope_annotations, color_group == "neg_lambda"),
                aes(x = mean(combined_preds$x), y = -Inf, label = slope_text),
                hjust = 0.5, vjust = -0.2, size = 8, color = "blue",
                show.legend = FALSE, inherit.aes = FALSE
            )

            # Add positive lambda annotations at top center
            p <- p + geom_text(
                data = subset(slope_annotations, color_group == "pos_lambda"),
                aes(x = mean(combined_preds$x), y = Inf, label = slope_text),
                hjust = 0.5, vjust = 1.2, size = 8, color = "red",
                show.legend = FALSE, inherit.aes = FALSE
            )
        }
    }

    return(p)
}


# =============================================================================
# ANOVA plotting function for study by domain analysis
# =============================================================================

plot_anova_study_by_domain <- function(data_combined,
                                      construct_filter = c("psychosocial_F1", "psychosocial_F2"),
                                      width = 14, height = 8,
                                      plot_overall = TRUE) {
  # Helper function to convert p-values to stars
  p_to_stars <- function(p) {
    ifelse(p < .001, "***",
      ifelse(p < .01, "**",
        ifelse(p < .05, "*", "ns")
      )
    )
  }

  # Filter data to specified constructs
  if (!is.null(construct_filter)) {
    data_combined <- data_combined %>%
      filter(construct %in% construct_filter)
  }

  # Define domain order: Political, Emotional (no Overall since we removed it)
  domain_order <- c("Political\nDomain", "Emotional\nDomain")

  # Get pooled baseline means for each construct
  baseline_means <- data_combined %>%
    group_by(construct) %>%
    summarise(baseline_mean = mean(outcome_value_pre, na.rm = TRUE), .groups = "drop")

  # Fit ANCOVA models for each construct
  mods_end <- list()
  construct_names <- unique(data_combined$construct)

  for (construct_name in construct_names) {
    construct_data <- data_combined %>% filter(construct == construct_name)
    mods_end[[construct_name]] <- lm(outcome_value ~ outcome_value_pre + domain * study_id,
                                    data = construct_data)
  }

  # Extract estimated marginal means for each construct
  all_plot_data <- list()
  all_contrasts <- list()

  for (construct_name in names(mods_end)) {
    baseline_val <- baseline_means$baseline_mean[baseline_means$construct == construct_name]

    # Get adjusted means at pooled baseline for domain-specific groups
    emm_domain_results <- summary(
      emmeans(mods_end[[construct_name]], ~ domain * study_id,
              at = list(outcome_value_pre = baseline_val)),
      infer = TRUE
    ) %>%
      mutate(
        domain_label = case_when(
          domain == "polchat" ~ "Political\nDomain",
          domain == "emotchat" ~ "Emotional\nDomain",
          TRUE ~ domain
        ),
        study_label = case_when(
          study_id == "longitudinal" ~ "Longitudinal",
          study_id == "cross-sectional" ~ "Cross-sectional",
          TRUE ~ as.character(study_id)
        )
      )

    # Get adjusted means for overall comparisons (marginalizing over domain) - conditional
    if (plot_overall) {
      emm_overall_results <- summary(
        emmeans(mods_end[[construct_name]], ~ study_id,
                at = list(outcome_value_pre = baseline_val)),
        infer = TRUE
      ) %>%
        mutate(
          domain = "overall",
          domain_label = "Overall",
          study_label = case_when(
            study_id == "longitudinal" ~ "Longitudinal",
            study_id == "cross-sectional" ~ "Cross-sectional",
            TRUE ~ as.character(study_id)
          )
        )

      # Combine all results
      combined_emm <- bind_rows(emm_domain_results, emm_overall_results)
    } else {
      # Only use domain results
      combined_emm <- emm_domain_results
    }

    combined_emm <- combined_emm %>%
      mutate(
        outcome = case_when(
          construct_name == "psychosocial_F1" ~ "Emotional Health",
          construct_name == "psychosocial_F2" ~ "Social Health",
          TRUE ~ construct_name
        ),
        domain_label = factor(domain_label, levels = domain_order)
      )

    all_plot_data[[construct_name]] <- combined_emm

    # Perform contrasts - conditional on plot_overall
    if (plot_overall) {
      # Perform overall contrasts (marginalizing over domain)
      contr_overall <- contrast(
        emmeans(mods_end[[construct_name]], ~ study_id),
        "pairwise", adjust = "fdr"
      ) %>%
        summary(infer = TRUE) %>%
        mutate(
          construct = construct_name,
          domain = "overall",
          comparison_type = "overall"
        )

      # Perform contrasts within each domain
      contr_by_domain <- contrast(
        emmeans(mods_end[[construct_name]], ~ study_id | domain),
        "pairwise", adjust = "fdr"
      ) %>%
        summary(infer = TRUE) %>%
        mutate(
          construct = construct_name,
          comparison_type = "by_domain"
        )

      # Combine all contrasts
      all_contrasts[[construct_name]] <- bind_rows(contr_overall, contr_by_domain)
    } else {
      # Only perform contrasts within each domain
      contr_by_domain <- contrast(
        emmeans(mods_end[[construct_name]], ~ study_id | domain),
        "pairwise", adjust = "fdr"
      ) %>%
        summary(infer = TRUE) %>%
        mutate(
          construct = construct_name,
          comparison_type = "by_domain"
        )

      # Only use domain contrasts
      all_contrasts[[construct_name]] <- contr_by_domain
    }
  }

  # Combine plot data
  plot_df <- bind_rows(all_plot_data)

  # Combine contrasts
  combined_contrasts <- bind_rows(all_contrasts)

  # Create bracket function for all comparisons
  mk_brackets <- function(plot_slice, contr_slice) {
    # Define domain positions - conditional on plot_overall
    domains <- domain_order

    # Calculate overall range for consistent padding
    rng <- range(c(plot_slice$lower.CL, plot_slice$upper.CL), na.rm = TRUE)
    pad <- 0.04 * diff(rng)
    y_tickd <- 0.02 * diff(rng)
    y_labd <- 0.04 * diff(rng)

    brackets <- list()

    for (i in seq_along(domains)) {
      domain_name <- domains[i]
      domain_data <- plot_slice %>% filter(domain_label == domain_name)

      if (nrow(domain_data) == 0) next

      # Calculate bracket position for this domain
      # Always position bracket above the bars/curves, and ensure it's above 0 for emotional outcomes
      max_upper <- max(domain_data$upper.CL, na.rm = TRUE)
      y_bracket <- max(max_upper + pad, 0.02)  # Ensure bracket is at least 0.02 above zero
      y_tickd_adj <- -y_tickd  # Ticks go downward from bracket
      y_labd_adj <- y_labd     # Label goes above bracket

      # Get p-value for this domain
      if (domain_name == "Political\nDomain") {
        p_val <- contr_slice %>%
          filter(domain == "polchat") %>%
          pull(p.value)
      } else if (domain_name == "Emotional\nDomain") {
        p_val <- contr_slice %>%
          filter(domain == "emotchat") %>%
          pull(p.value)
      } else {
        # Fallback for any other domain names
        p_val <- 1
      }

      # Handle missing p-values
      if (length(p_val) == 0) p_val <- 1

      # Find the x position for this domain in the plot
      domain_x_pos <- which(unique(plot_slice$domain_label) == domain_name)
      if (length(domain_x_pos) == 0) domain_x_pos <- i

      brackets[[i]] <- tibble(
        outcome = unique(plot_slice$outcome),
        domain_name = domain_name,
        x_start = domain_x_pos - 0.3,  # Position brackets over each domain
        x_end = domain_x_pos + 0.3,
        y = y_bracket,
        y_tick = y_bracket + y_tickd_adj,
        label_y = y_bracket + y_labd_adj,
        p_adj = p_val,
        label = paste0(
          p_to_stars(p_val),
          "  (p = ", formatC(p_val, format = "f", digits = 3), ")"
        )
      )
    }

    bind_rows(brackets)
  }

  # Build brackets for each outcome
  br_df <- list()
  for (outcome_name in unique(plot_df$outcome)) {
    plot_slice <- plot_df %>% filter(outcome == outcome_name)
    contr_slice <- combined_contrasts %>%
      filter(construct == ifelse(outcome_name == "Emotional Health",
                                "psychosocial_F1", "psychosocial_F2"))
    br_df[[outcome_name]] <- mk_brackets(plot_slice, contr_slice)
  }
  br_df <- bind_rows(br_df)

  # Check if br_df has data - if not, create empty dataframe with required columns
  if (nrow(br_df) == 0) {
    br_df <- tibble(
      outcome = character(0),
      domain_name = character(0),
      x_start = numeric(0),
      x_end = numeric(0),
      y = numeric(0),
      y_tick = numeric(0),
      label_y = numeric(0),
      p_adj = numeric(0),
      label = character(0)
    )
  }

  # Add fill and border colors based on domain and study
  plot_df <- plot_df %>%
    mutate(
      # Set border colors based on domain
      border_color = case_when(
        domain_label == "Political\nDomain" ~ polchat_color,
        domain_label == "Emotional\nDomain" ~ domain_color,
        TRUE ~ "#808080"
      ),
      # Set fill colors: transparent for cross-sectional, domain color for longitudinal
      fill_color = case_when(
        study_label == "Cross-sectional" ~ "transparent",
        domain_label == "Political\nDomain" ~ polchat_color,
        domain_label == "Emotional\nDomain" ~ domain_color,
        TRUE ~ "#808080"
      ),
      # Add alpha for significance
      alpha_val = 1.0  # Can be adjusted based on significance if needed
    )

  # Create the dodged bar plot
  p <- ggplot(plot_df, aes(x = domain_label, y = emmean, group = study_label)) +
    geom_hline(yintercept = 0, color = "black", linetype = "solid", alpha = 0.5) +
    geom_col(
      aes(fill = fill_color, color = border_color),
      position = position_dodge(width = 0.8),
      linewidth = 1,
      width = 0.7
    ) +
    geom_errorbar(
      aes(ymin = lower.CL, ymax = upper.CL),
      position = position_dodge(width = 0.8),
      width = 0,
      color = "black",
      linewidth = 0.8
    ) +
    # significance brackets for overall comparison
    geom_segment(
      data = br_df,
      aes(x = x_start, xend = x_end, y = y, yend = y),
      inherit.aes = FALSE,
      color = "black"
    ) +
    geom_segment(
      data = br_df,
      aes(x = x_start, xend = x_start, y = y, yend = y_tick),
      inherit.aes = FALSE,
      color = "black"
    ) +
    geom_segment(
      data = br_df,
      aes(x = x_end, xend = x_end, y = y, yend = y_tick),
      inherit.aes = FALSE,
      color = "black"
    ) +
    geom_text(
      data = br_df,
      aes(x = (x_start + x_end) / 2, y = label_y, label = label),
      vjust = 0, size = 5, inherit.aes = FALSE,
      color = "black"
    ) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_grid(cols = vars(outcome), scales = "free_y") +
    labs(
      # title = "Domain × Study Comparisons of Psychosocial Health",
      x = "Domain",
      y = "Adjusted Post Means"
    ) +
    theme_minimal() +
    theme_pub() +
    theme(
      legend.position = "none"
    )

  # Return comprehensive results
  return(list(
    plot = p,
    contrasts = combined_contrasts,
    plot_data = plot_df,
    brackets = br_df
  ))
}
# =============================================================================
# JSON Contrasts Extraction Utilities
# =============================================================================
# Functions to extract contrasts from JSON files as data frames
# that can be passed directly to plotting functions.
# =============================================================================

load_contrasts_json <- function(json_path) {
  if (!file.exists(json_path)) {
    stop(paste("JSON file not found:", json_path))
  }
  fromJSON(json_path, simplifyDataFrame = FALSE)
}

#' Extract contrasts from JSON as a data frame
extract_contrasts <- function(json_data,
                               outcomes = NULL,
                               test_types = NULL,
                               study_type = NULL,
                               p_value_col = "p_global") {

  contrasts <- json_data$contrasts
  if (length(contrasts) == 0) return(data.frame())

  # Build data frame from contrasts
  rows <- lapply(contrasts, function(c) {
    # Apply filters
    if (!is.null(outcomes) && !(c$outcome %in% outcomes)) return(NULL)
    if (!is.null(test_types) && !(c$test_type %in% test_types)) return(NULL)
    if (!is.null(study_type) && c$study_type != study_type) return(NULL)

    # Get p-value (handle NULL/empty)
    get_val <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else x

    p_adj <- switch(p_value_col,
      "p_global" = get_val(c$primary$p_global),
      "p_local" = get_val(c$primary$p_local),
      "p_raw" = get_val(c$primary$p_raw),
      get_val(c$primary$p_global)
    )
    if (is.na(p_adj)) p_adj <- get_val(c$primary$p_raw)

    data.frame(
      construct = c$outcome,
      contrast = c$test,
      coefficient = c$test,
      estimate = c$primary$estimate,
      SE = get_val(c$primary$se),
      lower.CL = get_val(c$primary$lower_cl),
      upper.CL = get_val(c$primary$upper_cl),
      p.value = get_val(c$primary$p_raw),
      p.value.adj.family = p_adj,
      test_type = c$test_type,
      study_type = c$study_type,
      stringsAsFactors = FALSE
    )
  })

  df <- bind_rows(rows)
  if (nrow(df) == 0) return(data.frame())

  # Add significance stars
  df$stars <- case_when(
    df$p.value.adj.family < 0.001 ~ "***",
    df$p.value.adj.family < 0.01 ~ "**",
    df$p.value.adj.family < 0.05 ~ "*",
    TRUE ~ ""
  )

  df
}

#' Extract RS (Relationship-Seeking) main effects
extract_rs <- function(json_data, outcomes = NULL, study_type = "longitudinal", p_value_col = "p_global") {
  # Use main_effect_RS test_type (or fall back to main_effect for backwards compatibility)
  df <- extract_contrasts(json_data, outcomes, c("main_effect_RS", "main_effect"), study_type, p_value_col)
  if (nrow(df) == 0) return(NULL)
  df %>% filter(grepl("^RS:", contrast))
}

#' Extract Domain main effects
extract_domain <- function(json_data, outcomes = NULL, study_type = "longitudinal", p_value_col = "p_global") {
  # Use main_effect_Domain test_type (or fall back to main_effect for backwards compatibility)
  df <- extract_contrasts(json_data, outcomes, c("main_effect_Domain", "main_effect"), study_type, p_value_col)
  if (nrow(df) == 0) return(NULL)
  df %>% filter(grepl("^Domain:", contrast))
}

#' Extract Personalisation main effects
extract_pers <- function(json_data, outcomes = NULL, study_type = "longitudinal", p_value_col = "p_global") {
  # Use main_effect_Pers test_type (or fall back to main_effect for backwards compatibility)
  df <- extract_contrasts(json_data, outcomes, c("main_effect_Pers", "main_effect"), study_type, p_value_col)
  if (nrow(df) == 0) return(NULL)
  df %>% filter(grepl("^Personalisation:", contrast))
}

#' Extract temporal coefficients (regression coefficients for time effects)
#'
#' Uses temporal_coefficient test_type which contains per-unit-time regression
#' coefficients (e.g., "0.09 per session") rather than emmeans contrasts
#' (e.g., "S20 - S1 = 1.73").
extract_temporal <- function(json_data, outcomes = NULL, time_var = "session_numeric", p_value_col = "p_global") {
  df <- extract_contrasts(json_data, outcomes, "temporal_coefficient", "longitudinal", p_value_col)
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Map coefficient names to format expected by plot_time_coeffs_forest
  df %>% mutate(
    coefficient = case_when(
      grepl("^Coefficient: Time$", contrast) ~ time_var,
      grepl("Relationship-Seeking", contrast) ~ paste0("lambda:", time_var),
      grepl("Personalisation", contrast) ~ paste0("personalisationpersonalised:", time_var),
      grepl("Domain", contrast) ~ paste0("domainemotchat:", time_var),
      TRUE ~ coefficient
    )
  )
}

#' Extract polynomial (dose-response) coefficients
extract_polynomial <- function(json_data, outcomes = NULL, p_value_col = "p_global") {
  df <- extract_contrasts(json_data, outcomes, "dose_response", "longitudinal", p_value_col)
  if (nrow(df) == 0) return(NULL)

  # Map polynomial terms and add metadata for lambda prediction plots
  df %>% mutate(
    coefficient = case_when(
      grepl("λ \\(linear\\)", contrast) ~ "lambda",
      grepl("λ² \\(quadratic\\)", contrast) ~ "I(lambda^2)",
      grepl("λ³ \\(cubic\\)", contrast) ~ "I(lambda^3)",
      TRUE ~ coefficient
    ),
    test_id = "coefs_continuous",
    coef_type = case_when(
      coefficient == "lambda" ~ "lambda",
      coefficient == "I(lambda^2)" ~ "lambda2",
      coefficient == "I(lambda^3)" ~ "lambda3",
      TRUE ~ "other"
    ),
    sig_stars = stars
  )
}

#' Wrap polynomial results for plot_lambda_means_with_predictions
wrap_polynomial <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  list(family_all = df)
}

#' Extract moderation contrasts (RS × Domain, RS × Personalisation interactions)
extract_moderation <- function(json_data, outcomes = NULL, p_value_col = "p_global") {
  df <- extract_contrasts(json_data, outcomes, "moderation", "longitudinal", p_value_col)
  if (nrow(df) == 0) return(NULL)

  # Add test_id based on contrast type
  df %>% mutate(
    test_id = case_when(
      grepl("Domain", contrast) ~ "interaction_coarsened",
      grepl("Personalisation", contrast) ~ "interaction_coarsened",
      TRUE ~ "interaction"
    )
  )
}

#' Wrap moderation results for plot_domain_moderation_specific
wrap_moderation <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Split by moderation type
  domain_df <- df %>% filter(grepl("Domain", contrast))
  pers_df <- df %>% filter(grepl("Personalisation", contrast))

  list(
    domain = list(family_all = domain_df),
    personalisation = list(family_all = pers_df)
  )
}

# =============================================================================
# PLOT FUNCTION: Group comparison bar chart with significance
# =============================================================================

plot_group_comparison <- function(model, group_var,
                                  group_labels = NULL,
                                  title = "Group Comparison",
                                  xlab = NULL, ylab = "Predicted Probability",
                                  width = 8, height = 6,
                                  use_response_scale = TRUE) {

    # Get emmeans - use response scale for logistic models
    is_glm <- inherits(model, "glm") || inherits(model, "glmerMod")
    if (use_response_scale && is_glm && family(model)$family == "binomial") {
        emm <- emmeans(model, as.formula(paste0("~", group_var)), type = "response")
        emm_df <- as.data.frame(summary(emm))
        y_col <- "prob"
        ci_low <- "asymp.LCL"
        ci_high <- "asymp.UCL"
    } else {
        emm <- emmeans(model, as.formula(paste0("~", group_var)))
        emm_df <- as.data.frame(summary(emm))
        y_col <- "emmean"
        ci_low <- "lower.CL"
        ci_high <- "upper.CL"
    }

    # Get contrast
    contrast_result <- contrast(emm, method = "revpairwise")
    contrast_df <- as.data.frame(summary(contrast_result, infer = TRUE))

    # Extract p-value and format
    p_value <- contrast_df$p.value[1]
    p_label <- if (p_value < 0.001) {
        "p < .001***"
    } else if (p_value < 0.01) {
        sprintf("p = %.3f**", p_value)
    } else if (p_value < 0.05) {
        sprintf("p = %.3f*", p_value)
    } else {
        sprintf("p = %.3f", p_value)
    }

    # Rename columns for plotting
    names(emm_df)[1] <- "group"
    emm_df$group <- as.character(emm_df$group)
    emm_df$y_value <- emm_df[[y_col]]
    emm_df$ci_low <- emm_df[[ci_low]]
    emm_df$ci_high <- emm_df[[ci_high]]

    # Apply custom labels if provided
    if (!is.null(group_labels)) {
        emm_df$group <- factor(emm_df$group,
            levels = names(group_labels),
            labels = group_labels
        )
    }

    # Set x-axis label
    if (is.null(xlab)) {
        xlab <- tools::toTitleCase(gsub("_", " ", group_var))
    }

    # Calculate y position for significance bracket
    y_max <- max(emm_df$ci_high) * 1.05
    bracket_y <- y_max + (max(emm_df$ci_high) - min(emm_df$ci_low)) * 0.05

    # Create plot
    p <- ggplot(emm_df, aes(x = group, y = y_value)) +
        geom_col(aes(fill = group), alpha = 0.7, width = 0.6, show.legend = FALSE) +
        geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
            width = 0.0, linewidth = 0.8
        ) +
        geom_text(aes(label = sprintf("%.2f", y_value)),
            vjust = 2.5, size = 6, fontface = "bold"
        ) +
        # Add significance bracket
        geom_signif(
            comparisons = list(c(1, 2)),
            annotations = paste(p_label),
            y_position = bracket_y,
            tip_length = 0.0,
            textsize = 6
        ) +
        scale_fill_manual(values = c("gray30", "gray70")) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
            title = title,
            x = xlab,
            y = ylab
        ) +
        theme_minimal() +
        theme_pub() +
        theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank()
        )

    invisible(list(
        plot = p,
        emmeans = emm_df,
        contrast = contrast_df
    ))
}
