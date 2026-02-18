# plot_config.R
# Global color scheme and theme for all R analyses

library(ggplot2)
library(ggthemes)

# =============================================================================
# COLOR PALETTES
# =============================================================================

STUDY_COLORS <- list(
  # Study ID colors
  study_id = c(
    "calibration" = "#fbbd05",
    "cross-sectional" = "#32bcdd",
    "longitudinal" = "#ff8a67"
  ),

  # Domain colors
  domain = c(
    "polchat" = "#637939",
    "emotchat" = "#b6d75d"
  ),

  # Multiplier colors (coolwarm divergent)
  multiplier_factor = c(
    "neg1" = "#3b4cc0",
    "neg0.5" = "#8db0fe",
    "zero" = "#918f8fff",
    "pos0.5" = "#f4987a",
    "pos1" = "#b40426"
  ),

  # Personalisation colors
  personalisation = c(
    "non-personalised" = "#5d3cb1ff",
    "personalised" = "#bca1f1"
  ),

  # Anthropomorphic category colors
  anthro_category = c(
    "deanthro" = "#3b4cc0",
    "neutral" = "#918f8fff",
    "anthro" = "#b40426"
  )
)

# =============================================================================
# SCALE FUNCTIONS
# =============================================================================

scale_color_study <- function() {
  scale_color_manual(values = STUDY_COLORS$study_id, name = "Study")
}

scale_fill_study <- function() {
  scale_fill_manual(values = STUDY_COLORS$study_id, name = "Study")
}

scale_color_domain <- function() {
  scale_color_manual(values = STUDY_COLORS$domain, name = "Domain")
}

scale_fill_domain <- function() {
  scale_fill_manual(values = STUDY_COLORS$domain, name = "Domain")
}

scale_color_multiplier_factor <- function() {
  scale_color_manual(values = STUDY_COLORS$multiplier_factor, name = "Multiplier")
}

scale_fill_multiplier_factor <- function() {
  scale_fill_manual(values = STUDY_COLORS$multiplier_factor, name = "Multiplier")
}

# =============================================================================
# THEME
# =============================================================================

theme_pub <- function(base_size = 18, base_family = "sans") {
  theme_foundation(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5, margin = margin(0, 0, 20, 0)),
      text = element_text(),
      plot.subtitle = element_text(face = "italic", size = rel(0.9)),
      panel.background = element_rect(colour = NA),
      plot.background = element_rect(colour = NA),
      panel.border = element_rect(colour = NA),
      axis.title = element_text(face = "bold", size = rel(1)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      axis.text = element_text(),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.ticks = element_line(),
      axis.text.x = element_text(angle = 0, size = rel(1.1)),
      axis.text.y = element_text(angle = 0, size = rel(1.1)),
      panel.grid.major = element_line(colour = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.key.size = unit(0.5, "cm"),
      panel.spacing = unit(0.3, "cm"),
      legend.text = element_text(size = rel(1.1)),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "mm"),
      legend.title = element_text(face = "italic"),
      plot.margin = unit(c(t = 2, r = 2, b = 2, l = 3), "mm"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
      strip.text = element_text(face = "bold", size = rel(0.9))
    )
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

save_plot <- function(figure_dir, plot, filename, width, height,
                      save_pdf = TRUE, save_png = TRUE) {
  #' Save plot to PDF and/or PNG

  #' @param figure_dir Base figure directory (should contain pdf/ and png/ subdirs)
  #' @param plot The ggplot object to save
  #' @param filename Filename without extension (e.g., "my_plot")
  #' @param width Width in inches
  #' @param height Height in inches
  #' @param save_pdf Whether to save PDF (default TRUE)
  #' @param save_png Whether to save PNG (default TRUE)

  # Remove extension if provided

  base_name <- sub("\\.(pdf|png)$", "", filename)

  if (save_pdf) {
    pdf_dir <- file.path(figure_dir, "pdf")
    if (!dir.exists(pdf_dir)) dir.create(pdf_dir, recursive = TRUE)
    pdf_path <- file.path(pdf_dir, paste0(base_name, ".pdf"))
    ggsave(pdf_path, plot = plot, width = width, height = height,
           units = "in", dpi = 300)
    cat("Saved:", pdf_path, "\n")
  }

  if (save_png) {
    png_dir <- file.path(figure_dir, "png")
    if (!dir.exists(png_dir)) dir.create(png_dir, recursive = TRUE)
    png_path <- file.path(png_dir, paste0(base_name, ".png"))
    ggsave(png_path, plot = plot, width = width, height = height,
           units = "in", dpi = 300)
    cat("Saved:", png_path, "\n")
  }
}

# Variable name cleaning
CUSTOM_REPLACEMENTS <- c(
  "tool_friend" = "tool-friend",
  "cold_warm" = "cold-warm",
  "impersonal_personal" = "impersonal-personal",
  "insensitive_sensitive" = "insensitive-sensitive",
  "unsociable_sociable" = "unsociable-sociable",
  "robot_human" = "robot-human",
  "personalisation_agreement" = "personalisation",
  "seeking_companionship_likelihood" = "seeking companionship"
)

clean_var_name <- function(var_name, add_newlines = FALSE) {
  if (var_name %in% names(CUSTOM_REPLACEMENTS)) {
    clean_name <- CUSTOM_REPLACEMENTS[[var_name]]
  } else {
    clean_name <- gsub("_", " ", var_name)
  }

  if (add_newlines && nchar(clean_name) > 15) {
    clean_name <- gsub(" ", "\n", clean_name)
  }

  return(clean_name)
}

clean_var_vector <- function(var_vector, add_newlines = FALSE) {
  unique_vars <- unique(var_vector)
  clean_names <- sapply(unique_vars, clean_var_name, add_newlines = add_newlines)

  result <- sapply(var_vector, function(x) {
    idx <- which(unique_vars == x)
    clean_names[idx]
  })

  return(factor(result, levels = clean_names))
}
