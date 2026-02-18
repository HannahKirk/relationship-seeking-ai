# =============================================================================
# Coarsen and Factor Sociodemographic Variables
# =============================================================================
# Transforms raw sociodemographic variables into grouped/binary factors
# for use in regression analyses.
#
# Usage:
#   source("scripts/utils_r/coarsen_and_factor_sociodemos.R")
#   df <- coarsen_and_factor_sociodemos(df)
#
# Input: Data frame with raw sociodemographic columns
# Output: Data frame with additional coarsened/factored columns
# =============================================================================

library(dplyr)

# =============================================================================
# VARIABLE SPECIFICATIONS (expected values in factor order)
# =============================================================================

VARIABLE_SPECS <- list(
  gender = c("Male", "Female", "Non-binary", "Other", "Prefer not to say"),
  ethnicity = c(
    "White",
    "Black, African, Caribbean or Black British",
    "Asian and Asian British",
    "Mixed and multiple ethnic groups",
    "Hispanic", "Jewish", "Middle Eastern",
    "Other", "Prefer not to say"
  ),
  religion = c(
    "No religion", "Christian", "Muslim",
    "Buddhist", "Hindu", "Jewish", "Sikh", "Spiritual",
    "Other", "Prefer not to say"
  ),
  disability = c(
    "I do not have a disability",
    "I have minor health issues or a mild disability",
    "I have major health issues or a major disability that significantly affects my day-to-day activities, but I am not registered disabled",
    "Yes, and I am registered disabled",
    "Prefer not to say"
  ),
  income = c(
    "<£10k", "£10-20K", "£20-30K", "£30-50K", "£50-100K", ">£100K",
    "Prefer not to say"
  ),
  education = c(
    "No qualifications", "GCSEs or equivalent", "Vocational qualifications",
    "A levels or equivalent", "Undergraduate degree or equivalent",
    "Graduate study", "Prefer not to say"
  ),
  ai_frequency = c("Every day", 
  "Every week", "More than once a month", 
  "Once per month", "Less than once a year", 
  "I have never used a chatbot", "Prefer not to say"))

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Validate column values against spec
check_column_values <- function(data, col, expected) {
  vals <- unique(data[[col]])
  vals <- vals[!is.na(vals)]
  unexpected <- setdiff(vals, expected)
  if (length(unexpected) > 0) {
    stop(sprintf("%s has unexpected values: %s", col, paste(unexpected, collapse = ", ")))
  }
}

# Validate and factor raw column using VARIABLE_SPECS
validate_and_factor_raw <- function(data, col) {
  if (!col %in% names(data)) return(data)
  if (!col %in% names(VARIABLE_SPECS)) return(data)

  expected <- VARIABLE_SPECS[[col]]
  check_column_values(data, col, expected)
  data[[col]] <- factor(data[[col]], levels = expected)
  return(data)
}

# =============================================================================
# MAIN FUNCTION
# =============================================================================

coarsen_and_factor_sociodemos <- function(data) {

  # First: validate and factor all raw variables
  for (var_name in names(VARIABLE_SPECS)) {
    data <- validate_and_factor_raw(data, var_name)
  }

  # Then: apply coarsening transformations (creates _grouped and _binary)
  data <- coarsen_gender(data)
  data <- coarsen_ethnicity(data)
  data <- coarsen_religion(data)
  data <- coarsen_disability(data)
  data <- coarsen_income(data)
  data <- coarsen_education(data)
  data <- coarsen_ai_frequency(data)
  data <- factor_cluster_name(data)

  return(data)
}

# =============================================================================
# INDIVIDUAL COARSENING FUNCTIONS
# =============================================================================

coarsen_gender <- function(data) {
  if (!"gender" %in% names(data)) return(data)

  # Shorten names and group "Other" and "Prefer not to say" into "Non-binary"
  data$gender_grouped <- recode(data$gender,
    "Male" = "Male",
    "Female" = "Female",
    "Other" = "Non-binary",
    "Prefer not to say" = "Prefer not to say",
    "Non-binary" = "Non-binary",
  )

  data$gender_grouped <- factor(data$gender_grouped,
    levels = c("Male", "Female", "Non-binary", "Prefer not to say")
  )

  data$gender_binary <- case_when(
    data$gender_grouped == "Male" ~ "Male",
    data$gender_grouped == "Prefer not to say" ~ "Prefer not to say",
    TRUE ~ "Non-Male"
  )

  data$gender_binary <- factor(data$gender_binary,
    levels = c("Male", "Non-Male", "Prefer not to say")
  )

  return(data)
}

coarsen_ethnicity <- function(data) {
  if (!"ethnicity" %in% names(data)) return(data)
  check_column_values(data, "ethnicity", c("White", "Black, African, Caribbean or Black British", "Asian and Asian British", "Mixed and multiple ethnic groups", "Hispanic", "Jewish", "Middle Eastern", "Other", "Prefer not to say"))

  # Shorten names and group smaller categories into "Other"
  data$ethnicity_grouped <- recode(data$ethnicity,
    "White" = "White",
    "Black, African, Caribbean or Black British" = "Black",
    "Asian and Asian British" = "Asian",
    "Mixed and multiple ethnic groups" = "Mixed",
    "Jewish" = "Other",
    "Hispanic" = "Other",
    "Middle Eastern" = "Other",
    "Prefer not to say" = "Prefer not to say",
    "Other" = "Other",
  )

  data$ethnicity_grouped <- factor(data$ethnicity_grouped,
    levels = c("White", "Black", "Asian", "Mixed", "Other", "Prefer not to say")
  )

  data$ethnicity_binary <- case_when(
    data$ethnicity_grouped == "White" ~ "White",
    data$ethnicity_grouped == "Prefer not to say" ~ "Prefer not to say",
    TRUE ~ "Non-White"
  )

  data$ethnicity_binary <- factor(data$ethnicity_binary,
    levels = c("White", "Non-White", "Prefer not to say")
  )

  return(data)
}

coarsen_religion <- function(data) {
  if (!"religion" %in% names(data)) return(data)
  check_column_values(data, "religion", c("Buddhist", "Christian", "Hindu", "Jewish", "Muslim", "No religion", "Other", "Prefer not to say", "Sikh", "Spiritual"))

  # Group smaller categories into "Other"
  data$religion_grouped <- recode(data$religion,
    "No religion" = "No religion",
    "Christian" = "Christian",
    "Muslim" = "Muslim",
    "Jewish" = "Other",
    "Buddhist" = "Other",
    "Spiritual" = "Other",
    "Sikh" = "Other",
    "Hindu" = "Other",
    "Other" = "Other",
    "Prefer not to say" = "Prefer not to say",
  )

  data$religion_grouped <- factor(data$religion_grouped,
    levels = c("No religion", "Christian", "Muslim", "Other", "Prefer not to say")
  )

  data$religion_binary <- case_when(
    data$religion_grouped == "No religion" ~ "Non-Religious",
    data$religion_grouped == "Prefer not to say" ~ "Prefer not to say",
    TRUE ~ "Religious"
  )

  data$religion_binary <- factor(data$religion_binary,
    levels = c("Non-Religious", "Religious", "Prefer not to say")
  )

  return(data)
}

coarsen_disability <- function(data) {
  if (!"disability" %in% names(data)) return(data)
  check_column_values(data, "disability", c("I do not have a disability", "I have minor health issues or a mild disability", "I have major health issues or a major disability that significantly affects my day-to-day activities, but I am not registered disabled", "Yes, and I am registered disabled", "Prefer not to say"))

  # Shorten names and group
  data$disability_grouped <- recode(data$disability,
    "I do not have a disability" = "No disability",
    "I have minor health issues or a mild disability" = "Minor disability",
    "I have major health issues or a major disability that significantly affects my day-to-day activities, but I am not registered disabled" = "Disability",
    "Yes, and I am registered disabled" = "Disability",
    "Prefer not to say" = "Prefer not to say",
  )

  data$disability_grouped <- factor(data$disability_grouped,
    levels = c("No disability", "Minor disability", "Disability", "Prefer not to say")
  )

  data$disability_binary <- case_when(
    data$disability_grouped == "No disability" ~ "No disability",
    data$disability_grouped == "Prefer not to say" ~ "Prefer not to say",
    TRUE ~ "Has disability"
  )

  data$disability_binary <- factor(data$disability_binary,
    levels = c("No disability", "Has disability", "Prefer not to say")
  )

  return(data)
}

coarsen_income <- function(data) {
  if (!"income" %in% names(data)) return(data)
  check_column_values(data, "income", c("<£10k", "£10-20K", "£20-30K", "£30-50K", "£50-100K", ">£100K", "Prefer not to say"))

  data$income_grouped <- recode(data$income,
    "<£10k" = "Low income (<GBP30K)",
    ">£100K" = "High income (GBP50K+)",
     "£10-20K" = "Low income (<GBP30K)",
     "£20-30K" = "Low income (<GBP30K)",
     "£30-50K" = "Middle income (GBP30-50K)",
     "£50-100K" = "High income (GBP50K+)",
     "Prefer not to say" = "Prefer not to say",
  )


  data$income_grouped <- factor(data$income_grouped,
    levels = c("Low income (<GBP30K)", "Middle income (GBP30-50K)", "High income (GBP50K+)", "Prefer not to say")
  )

  data$income_binary <- case_when(
    data$income_grouped %in% c("Low income (<GBP30K)") ~ "Low Income (<GBP30K)",
    data$income_grouped == "Prefer not to say" ~ "Prefer not to say",
    TRUE ~ "High/Middle Income (GBP30K+)"
  )

  data$income_binary <- factor(data$income_binary,
    levels = c("High/Middle Income (GBP30K+)", "Low Income (<GBP30K)", "Prefer not to say")
  )

  return(data)
}

coarsen_education <- function(data) {
  if (!"education" %in% names(data)) return(data)
  check_column_values(data, "education", c("No qualifications", "GCSEs or equivalent", "Vocational qualifications", "A levels or equivalent", "Undergraduate degree or equivalent", "Graduate study", "Prefer not to say"))

  data$education_grouped <- recode(data$education,
    "No qualifications" = "No qualifications",
    "GCSEs or equivalent" = "GCSEs",
    "Vocational qualifications" = "Vocational qualifications",
    "A levels or equivalent" = "A levels",
    "Undergraduate degree or equivalent" = "Undergraduate degree",
    "Graduate study" = "Graduate study",
    "Prefer not to say" = "Prefer not to say",
  )

  # Education in years mapping
  education_years_mapping <- c(
    "No qualifications" = 10,
    "GCSEs" = 11,
    "Vocational qualifications" = 12,
    "A levels" = 13,
    "Undergraduate degree" = 16,
    "Graduate study" = 18
  )

  data$education_years <- education_years_mapping[data$education_grouped]

 # Impute median for "Prefer not to say"
  median_years <- median(data$education_years, na.rm = TRUE)
  data$education_years[is.na(data$education_years)] <- median_years

  data$education_grouped <- factor(data$education_grouped,
    levels = c(
      "No qualifications", "Vocational qualifications", "GCSEs",
      "A levels", "Undergraduate degree", "Graduate study", "Prefer not to say"
    )
  )

  return(data)
}

coarsen_ai_frequency <- function(data) {
  if (!"ai_frequency" %in% names(data)) return(data)
  check_column_values(data, "ai_frequency", c("I have never used a chatbot", "Less than once a year", "Once per month", "More than once a month", "Every week", "Every day", "Prefer not to say"))

  data$ai_frequency_grouped <- recode(data$ai_frequency,
    "I have never used a chatbot" = "Never",
    "Less than once a year" = "Yearly",
    "Once per month" = "Monthly",
    "More than once a month" = "Monthly",
    "Every week" = "Weekly",
    "Every day" = "Daily",
    "Prefer not to say" = "Prefer not to say",
  )

  data$ai_frequency_grouped <- factor(data$ai_frequency_grouped,
    levels = c("Never", "Yearly", "Monthly", "Weekly", "Daily", "Prefer not to say")
  )

  data$ai_frequency_coarsened <- case_when(
    data$ai_frequency_grouped %in% c("Never", "Yearly") ~ "Non-users",
    data$ai_frequency_grouped == "Monthly" ~ "Moderate users",
    data$ai_frequency_grouped %in% c("Weekly", "Daily") ~ "Heavy users",
    data$ai_frequency_grouped == "Prefer not to say" ~ "Prefer not to say",
    TRUE ~ NA_character_
  )

  data$ai_frequency_coarsened <- factor(data$ai_frequency_coarsened,
    levels = c("Non-users", "Moderate users", "Heavy users", "Prefer not to say")
  )

  return(data)
}

factor_cluster_name <- function(data) {
  if (!"cluster_name" %in% names(data)) return(data)

  # Factor cluster_name with "Anthro Skeptic" as reference level
  data$cluster_name <- factor(
    data$cluster_name,
    levels = c("Anthro Skeptic", "Anthro Enthusiast")
  )

  return(data)
}

# =============================================================================
# PLOTTING FUNCTION: Sociodemographic Distribution Summary
# =============================================================================

plot_sociodemo_distributions <- function(data) {
  library(ggplot2)
  library(tidyr)
  library(patchwork)

  # Use study colors from plot_config.R (must be sourced before calling)
  study_colors <- STUDY_COLORS$study_id

  # Define variable mappings: raw -> grouped -> binary
variable_sets <- list(
    gender = list(raw = "gender", grouped = "gender_grouped", binary = "gender_binary"),
    ethnicity = list(raw = "ethnicity", grouped = "ethnicity_grouped", binary = "ethnicity_binary"),
    religion = list(raw = "religion", grouped = "religion_grouped", binary = "religion_binary"),
    disability = list(raw = "disability", grouped = "disability_grouped", binary = "disability_binary"),
    income = list(raw = "income", grouped = "income_grouped", binary = "income_binary"),
    ai_frequency = list(raw = "ai_frequency", grouped = "ai_frequency_grouped", binary = "ai_frequency_coarsened")
  )

  # Helper to truncate long labels
  truncate_label <- function(x, max_chars = 30) {
    ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 3), "..."), x)
  }

  # Helper to create a single bar panel
  make_bar_panel <- function(data, var_col, title, show_labels = TRUE) {
    if (!var_col %in% names(data)) {
      return(ggplot() + theme_void() + ggtitle(paste(title, "(missing)")))
    }

    # Calculate percentages by study
    plot_data <- data %>%
      filter(!is.na(.data[[var_col]])) %>%
      group_by(study_id, .data[[var_col]]) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(study_id) %>%
      mutate(
        total = sum(n),
        pct = n / total * 100,
        label = sprintf("%.0f%% (%d)", pct, n)
      ) %>%
      ungroup()

    # Rename for plotting and truncate long labels (preserve factor order)
    names(plot_data)[2] <- "category"
    if (is.factor(plot_data$category)) {
      orig_levels <- levels(plot_data$category)
      new_levels <- truncate_label(orig_levels)
      plot_data$category <- factor(
        truncate_label(as.character(plot_data$category)),
        levels = new_levels
      )
    } else {
      plot_data$category <- truncate_label(as.character(plot_data$category))
    }

    p <- ggplot(plot_data, aes(x = category, y = pct, fill = study_id)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      scale_fill_manual(values = study_colors, name = "Study") +
      scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
      coord_flip() +
      labs(title = title, x = NULL, y = "% of sample") +
      theme_pub() +
      theme(
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        plot.title = element_text(size = 13, face = "bold"),
        legend.position = "none"
      )

    if (show_labels) {
      p <- p + geom_text(
        aes(label = label),
        position = position_dodge(width = 0.8),
        hjust = -0.1,
        size = 4
      )
    }

    p
  }

  # Create rows of plots
  plot_rows <- list()

  for (var_name in names(variable_sets)) {
    vars <- variable_sets[[var_name]]

    p_raw <- make_bar_panel(data, vars$raw, paste0(var_name, " (raw)"), show_labels = FALSE)
    p_grouped <- make_bar_panel(data, vars$grouped, paste0(var_name, " (grouped)"))
    p_binary <- make_bar_panel(data, vars$binary, paste0(var_name, " (binary)"))

    plot_rows[[var_name]] <- p_raw + p_grouped + p_binary
  }

  # Combine all rows vertically
  combined_plot <- wrap_plots(plot_rows, ncol = 1)

  # Add shared legend at bottom (only main studies)
  main_study_colors <- study_colors[c("cross-sectional", "longitudinal")]
  legend_plot <- ggplot(
    data.frame(study_id = factor(names(main_study_colors), levels = names(main_study_colors))),
    aes(x = study_id, fill = study_id)
  ) +
    geom_bar() +
    scale_fill_manual(values = main_study_colors, name = "Study") +
    theme_pub() +
    theme(legend.position = "bottom", legend.text = element_text(size = 12))

  legend <- cowplot::get_legend(legend_plot)

  final_plot <- cowplot::plot_grid(
    combined_plot,
    legend,
    ncol = 1,
    rel_heights = c(1, 0.05)
  )

  return(final_plot)
}
