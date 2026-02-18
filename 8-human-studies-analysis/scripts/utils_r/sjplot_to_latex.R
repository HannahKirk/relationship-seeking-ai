library(sjPlot)
library(stringr)
library(html2latex)

# We wrote this custom script
# to convert sjPlot HTML tables to LaTeX
# It is based off html2latex
# but includes custom cleaning and formatting steps to produce publication-ready LaTeX tables.
# It is a bit hacky but it works and saves us from having to manually format multiple tables in LaTeX.

# Source labelling utilities for predictor label generation
# This must be sourced before using generate_pred_labels()
if (!exists("get_pred_labels") || !exists("auto_pred_labels")) {
  # Try relative path from project root (most common execution context)
  tryCatch({
    source("scripts/utils_r/labelling_utils.R")
  }, error = function(e) {
    # Fallback: try sourcing from same directory
    script_dir <- dirname(sys.frame(1)$ofile)
    if (!is.null(script_dir) && script_dir != "") {
      source(file.path(script_dir, "labelling_utils.R"))
    } else {
      stop("Cannot find labelling_utils.R. Please source it before sjplot_to_latex.R")
    }
  })
}

#####################################################
# SJPLOT TO LATEX CONVERSION SYSTEM
#####################################################

#' Create regression table from sjPlot and convert to LaTeX
#'
#' @param models List of regression models (lm, glm, lmer, glmer, clm, etc.)
#' @param model_labels Character vector of column labels for each model
#' @param pred_labels Named vector for renaming predictors
#' @param filename Output filename (without .tex extension)
#' @param table_dir Directory to save the LaTeX file
#' @param caption Table caption
#' @param dependent_var Dependent variable description
#' @param column_groups Optional list specifying column grouping structure
#' @param text_replacements Named vector of text replacements to apply (e.g., c("Multiplier" = "$\\lambda$"))
#' @param fontsize Font size for table ("scriptsize", "footnotesize", "small", "normalsize") (default: "scriptsize")
#' @param p.adjust P-value adjustment method (default: "none")
#' @param show.p Show p-values (default: TRUE)
#' @param drop Variables to drop from the table
#' @param debug Save intermediate files for debugging (default: FALSE)
#' @param silent Suppress progress messages (default: FALSE)
#'
#' @return TRUE if successful
sjplot_to_latex <- function(models,
                            model_labels,
                            filename,
                            table_dir,
                            caption,
                            dependent_var,
                            pred_labels = NULL,
                            column_groups = NULL,
                            text_replacements = NULL,
                            fontsize = "footnotesize",
                            p.adjust = "none",
                            show.p = TRUE,
                            drop = NULL,
                            debug = FALSE,
                            silent = FALSE) {
  
  # Validate fontsize
  valid_sizes <- c("tiny", "scriptsize", "footnotesize", "small", "normalsize", "large", "Large", "LARGE", "huge", "Huge")
  if (!fontsize %in% valid_sizes) {
    stop("fontsize must be one of: ", paste(valid_sizes, collapse = ", "))
  }
  
  # Validate inputs
  if (length(models) != length(model_labels)) {
    stop("Length of models (", length(models), ") must match length of model_labels (", length(model_labels), ")")
  }
  
  # Always auto-generate labels, then override with any provided labels
  auto_labels <- generate_pred_labels(models)
  if (!is.null(pred_labels)) {
    # Merge: provided labels take priority
    for (name in names(pred_labels)) {
      auto_labels[name] <- pred_labels[name]
    }
  }
  pred_labels <- auto_labels
  
  # Step 1: Create HTML table with sjPlot
  if (!silent) cat("Step 1: Creating HTML table with sjPlot...\n")
  
  tab <- sjPlot::tab_model(
    models,
    dv.labels = model_labels,
    pred.labels = pred_labels,
    show.ngroups = TRUE,
    show.p = show.p,
    p.style = "stars",
    collapse.ci = TRUE,
    digits = 2,
    p.adjust = p.adjust,
    drop = drop,
    title = caption,
    file = "temp.html"
  )
  
  print(tab)
  # Wait for file creation
  wait_for_file("temp.html", silent = silent)

  # Step 2: Convert HTML to LaTeX
  if (!silent) cat("Step 2: Converting HTML to LaTeX...\n")
  temp_tex <- file.path("temp.tex")
  if (file.exists(temp_tex)) file.remove(temp_tex)
  
  html2pdf(
    filename = "temp.html",
    table_width = 13,
    silent = TRUE,
    style = TRUE,
    build_pdf = FALSE,
    clean = TRUE,
    name_table = paste0("tab:", filename)
  )
  
  wait_for_file(temp_tex, silent = silent)
  
  # Step 3: Load and clean LaTeX
  if (!silent) cat("Step 3: Loading LaTeX content...\n")
  tex_content <- readLines(temp_tex, warn = FALSE, encoding = "UTF-8")
  tex_content <- paste(tex_content, collapse = "\n")
  
  # SAVE DEBUG FILE IN CURRENT DIRECTORY
  if (debug) {
    writeLines(tex_content, "debug_temp.tex")
    if (!silent) cat("Debug: Raw LaTeX saved to debug_temp.tex\n")
  }
  
  # Step 4: Clean and format LaTeX
  if (!silent) cat("Step 4: Cleaning LaTeX content...\n")
  n_cols <- length(model_labels) + 1  # +1 for predictor column
  
  cleaned_tex <- clean_sjplot_latex(
    tex_content = tex_content,
    n_cols = n_cols,
    caption = caption,
    dependent_var = dependent_var,
    model_labels = model_labels,
    column_groups = column_groups,
    text_replacements = text_replacements,
    fontsize = fontsize,
    debug = debug
  )
  
  # Step 5: Save final LaTeX file (to tex_tables subdirectory)
  if (!silent) cat("Step 5: Saving final LaTeX file...\n")
  tex_tables_dir <- file.path(table_dir, "tex_tables")
  dir.create(tex_tables_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(tex_tables_dir, paste0(filename, ".tex"))
  writeLines(cleaned_tex, output_file)
  
  # Clean up temp files
  if (file.exists("temp.html")) file.remove("temp.html")
  if (file.exists("temp.tex")) file.remove("temp.tex")
  
  if (!silent) cat("Table saved to:", output_file, "\n")
  return(invisible(TRUE))
}


#' Generate predictor labels from models
#'
#' Uses auto_pred_labels() from labelling_utils.R to generate human-readable
#' plain text labels. These will be converted to LaTeX math notation by
#' create_default_replacements() after the HTML-to-LaTeX conversion.
#'
#' @param models List of models
#' @param custom_labels Named vector of custom labels to override defaults
#' @param time_var Time variable name for proper formatting (default: "session_numeric")
#'
#' @return Named vector of predictor labels (plain text)
generate_pred_labels <- function(models, custom_labels = NULL, time_var = "session_numeric") {

  # Extract all coefficient names
  all_coeff_names <- c()
  for (model in models) {
    if (!is.null(model)) {
      if (inherits(model, c("lmerMod", "glmerMod", "clmm"))) {
        coeff_names <- names(lme4::fixef(model))
      } else if (inherits(model, "clm")) {
        coeff_names <- names(coef(model))
      } else {
        coeff_names <- names(coef(model))
      }
      all_coeff_names <- c(all_coeff_names, coeff_names)
    }
  }
  all_coeff_names <- unique(all_coeff_names)

  if (length(all_coeff_names) == 0) {
    return(NULL)
  }

  # Get base labels from labelling_utils.R
  base_labels <- get_pred_labels(time_var = time_var)

  # Auto-generate labels for all coefficients using format_predictor_label()
  pred_labels <- auto_pred_labels(all_coeff_names, time_var = time_var, format = "text")

  # Override with explicit base labels where available
  for (name in names(base_labels)) {
    if (name %in% names(pred_labels)) {
      pred_labels[name] <- base_labels[name]
    }
  }

  # Apply custom labels if provided (highest priority)
  if (!is.null(custom_labels)) {
    for (name in names(custom_labels)) {
      if (name %in% names(pred_labels)) {
        pred_labels[name] <- custom_labels[name]
      }
    }
  }

  return(pred_labels)
}

clean_sjplot_latex <- function(tex_content, n_cols, caption, dependent_var, 
                               model_labels = NULL, column_groups = NULL,
                               text_replacements = NULL, fontsize = "footnotesize",
                               debug = FALSE) {
  
  # Extract longtable content
  longtable_pattern <- "\\\\begin\\{longtable\\}.*?\\\\end\\{longtable\\}"
  longtable_content <- str_extract(tex_content, regex(longtable_pattern, dotall = TRUE))
  
  if (is.na(longtable_content)) {
    stop("No longtable environment found in LaTeX content")
  }
  
  # Basic replacements
  longtable_content <- str_replace_all(longtable_content, "p\\{13em\\}", "l")
  longtable_content <- str_replace_all(longtable_content, "m\\{[^}]+\\}", "c")
  longtable_content <- str_replace_all(longtable_content, "&\\s*\n\\s*~\\s*\n", "&~")
  
  # R-squared formatting
  longtable_content <- str_replace_all(
    longtable_content,
    "R\\\\textsuperscript\\{2\\} / R\\\\textsuperscript\\{2\\} adjusted",
    "$R^2/R^2_{adj.}$"
  )
  longtable_content <- str_replace_all(
    longtable_content,
    "Marginal R\\\\textsuperscript\\{2\\} / Conditional R\\\\textsuperscript\\{2\\}",
    "$R^2_{marg.}$ / $R^2_{cond.}$"
  )
  
  # Collapse line-wrapped predictor labels (sjPlot wraps long interaction terms)
  longtable_content <- str_replace_all(longtable_content, fixed(" x\n"), " x ")

  # Apply text replacements: always apply defaults, then user overrides
  all_replacements <- create_default_replacements()
  if (!is.null(text_replacements)) {
    all_replacements <- c(all_replacements, text_replacements)
  }
  for (original_text in names(all_replacements)) {
    replacement_text <- all_replacements[[original_text]]
    longtable_content <- str_replace_all(longtable_content,
                                        fixed(original_text),
                                        replacement_text)
  }
  
  # Convert CI to makecell format FIRST (before removing \centering)
  longtable_content <- convert_ci_to_makecell(longtable_content)
  
  # Remove \centering\arraybslash from table cells
  longtable_content <- str_replace_all(longtable_content, "\\\\centering\\\\arraybslash", "")
  
  # Remove other \centering patterns from table cells
  longtable_content <- str_replace_all(
    longtable_content,
    "\\\\centering(\\s*)\\{\\\\itshape ([^}]+)\\}",
    "{\\\\itshape \\2}"
  )
  longtable_content <- str_replace_all(longtable_content, "\\\\centering\\s*\\{\\\\bfseries", "{\\\\bfseries")
  longtable_content <- str_replace_all(longtable_content, "\\\\centering\\s+", "")
  
  # Create header structure
  header_content <- create_table_header(
    n_cols = n_cols,
    model_labels = model_labels,
    column_groups = column_groups
  )
  
  # Replace complete header with caption and dependent variable
  complete_header <- create_complete_header(
    n_cols = n_cols,
    caption = caption,
    dependent_var = dependent_var,
    header_content = header_content
  )
  
  # Replace begin statement and INSERT our header (using fixed() to avoid
  # regex interpretation of backslashes in complete_header)
  begin_pattern <- "\\\\begin\\{longtable\\}\\{([lrc|]+)\\}"
  col_spec <- str_match(longtable_content, begin_pattern)[,2]
  old_begin <- paste0("\\begin{longtable}{", col_spec, "}")
  new_begin <- paste0("\\begin{longtable}{", col_spec, "}\n", complete_header)
  longtable_content <- str_replace(longtable_content, fixed(old_begin), new_begin)
  
  # Remove the duplicate model labels row but keep Predictors/Estimates row
  # Pattern: \endlastfoot, \hline, model labels row, Predictors/Estimates row, \hline
  old_header_pattern <- "\\\\endlastfoot\\s*\\n\\s*\\\\hline\\s*\\n.*?\\{\\\\bfseries ~\\}.*?\\\\\\\\\\s*\\n(\\{\\\\itshape Predictors\\}.*?\\\\hline)(\\s*\\n)"
  longtable_content <- str_replace(
    longtable_content,
    regex(old_header_pattern, dotall = TRUE),
    "\\\\endlastfoot\n\\\\hline\n\\1\\2\\\\addlinespace[0.5em]\n"
  )
  # Format Random Effects section - LEFT ALIGNED
  random_effects_pattern <- paste0(
    "\\\\multicolumn\\{", n_cols, "\\}\\{[rcl]\\}\\{\\{\\\\bfseries Random Effects\\}\\}\\\\\\\\")
  random_effects_replacement <- paste0(
    " \\\\hline\n\\\\multicolumn{", n_cols, "}{l}{{\\\\bfseries Random Effects}} \\\\\\\\ \\\\hline"
  )
  longtable_content <- str_replace_all(
    longtable_content,
    random_effects_pattern,
    random_effects_replacement
  )
  
  
  # REMOVE DUPLICATE P-VALUE LINES from table body
  # sjPlot inserts center-aligned p-value notes in the body; our footer

  # sections (which use {r} alignment) are intentionally kept.
  # Handles: optional preceding \hline, possible line-wrap in *** p,
  # and optional trailing \\.
  pvalue_line_pattern <- paste0(
    "(\\\\hline\\s*)?",
    "\\\\multicolumn\\{", n_cols, "\\}\\{c\\}\\{",
    "\\\\raggedleft\\{\\\\itshape \\* p\\{\\\\textless\\}0\\.05~~~",
    "\\*\\* p\\{\\\\textless\\}0\\.01~~~",
    "\\*\\*\\*\\s*p\\{\\\\textless\\}0\\.001\\}\\}",
    "(\\\\\\\\)?",
    "\\s*"
  )

  longtable_content <- str_replace_all(
    longtable_content,
    regex(pvalue_line_pattern, dotall = TRUE),
    "\n"
  )
  
  
  # Clean up formatting issues
  longtable_content <- str_replace_all(longtable_content, "\\\\endlastfoot\\\\\\\\", "\\\\endlastfoot")
  longtable_content <- str_replace_all(longtable_content, "\\\\hline\\\\\\\\\\\\", "\\\\hline")
  longtable_content <- str_replace_all(longtable_content, "\\\\\\\\\\\\", "\\\\\\\\")
  longtable_content <- str_replace_all(longtable_content, "\\\\\\\\hline", "\\\\\\\\ \\\\hline")
  longtable_content <- str_replace_all(longtable_content, "\\\\\\\\\\n\\\\label", "\n\\\\label")
  
  # Clean up any double hlines before label
  longtable_content <- str_replace_all(longtable_content, "\\\\hline\\s*\\n\\s*\\\\hline\\s*\\n\\s*\\\\label", "\\\\hline\n\\\\label")
  
  # Wrap in formatting commands
  final_content <- paste(
    paste0("\\", fontsize, "{"),
    "\\setlength{\\LTcapwidth}{\\textwidth}",
    "\\renewcommand\\arraystretch{2}",
    "\\setlength\\tabcolsep{0.8mm}",
    longtable_content,
    "}",
    "\\normalsize",
    sep = "\n"
  )
  
  return(final_content)
}

#' Create table header with optional column groups
#'
#' @param n_cols Number of columns
#' @param model_labels Column labels
#' @param column_groups Optional grouping structure
#'
#' @return Header string
create_table_header <- function(n_cols, model_labels = NULL, column_groups = NULL) {
  # NOTE: Output is used with fixed() string replacement (not regex),
  # so each LaTeX backslash needs exactly one \\ in R source.

  if (!is.null(column_groups)) {
    # Multi-row header with groups
    group_row <- "{\\bfseries ~} & "
    for (group_name in names(column_groups)) {
      group <- column_groups[[group_name]]
      n_group_cols <- length(group$cols)
      group_row <- paste0(
        group_row,
        "\\multicolumn{", n_group_cols, "}{c}{\\bfseries ", group$label, "}"
      )
      if (group_name != names(column_groups)[length(column_groups)]) {
        group_row <- paste0(group_row, " & ")
      }
    }
    group_row <- paste0(group_row, " \\\\\n")

    # Partial hline (skip first column)
    hline <- paste0(
      "\\hhline{~",
      paste(rep("-", n_cols - 1), collapse = ""),
      "}\n"
    )

    # Individual column headers - use multicolumn for each cell
    col_row <- "{\\bfseries ~} & "
    formatted_labels <- c()
    for (i in seq_along(model_labels)) {
      formatted_labels[i] <- paste0("\\multicolumn{1}{c}{\\bfseries ", model_labels[i], "}")
    }
    col_row <- paste0(col_row, paste(formatted_labels, collapse = " & "), " \\\\\n")

    return(paste0(group_row, hline, col_row))

  } else {
    # Simple single-row header
    if (!is.null(model_labels)) {
      formatted_labels <- c()
      for (i in seq_along(model_labels)) {
        formatted_labels[i] <- paste0("\\multicolumn{1}{c}{\\bfseries ", model_labels[i], "}")
      }
      header_row <- paste0(
        "{\\bfseries ~} & ",
        paste(formatted_labels, collapse = " & "),
        " \\\\\n"
      )
      return(header_row)
    } else {
      return("")
    }
  }
}


#' Create complete header with caption, dependent variable, and page breaks
#'
#' @param n_cols Number of columns
#' @param caption Table caption
#' @param dependent_var Dependent variable description
#' @param header_content Header row content
#'
#' @return Complete header string
create_complete_header <- function(n_cols, caption, dependent_var, header_content) {
  # NOTE: Output is used with fixed() string replacement (not regex),
  # so each LaTeX backslash needs exactly one \\ in R source.

  single_hline <- paste0("\\hhline{", paste(rep("-", n_cols), collapse = ""), "}")
  double_hline <- paste0("\\hhline{", paste(rep("=", n_cols), collapse = ""), "}")

  # Main header block WITH caption
  main_header_with_caption <- paste(
    paste0("\\caption{", caption, "} \\\\"),
    single_hline,
    paste0("\\multicolumn{", n_cols, "}{l}{Dependent Variable = \\textit{", dependent_var, "}} \\\\"),
    double_hline,
    header_content,
    sep = "\n"
  )

  # Main header block WITHOUT caption (for continuation pages)
  main_header_no_caption <- paste(
    single_hline,
    paste0("\\multicolumn{", n_cols, "}{l}{Dependent Variable = \\textit{", dependent_var, "}} \\\\"),
    double_hline,
    header_content,
    sep = "\n"
  )

  # First page header (with caption)
  first_header <- paste(
    main_header_with_caption,
    "\\endfirsthead",
    sep = "\n"
  )

  # Continuation header (without caption)
  continuation_header <- paste(
    paste0("\\multicolumn{", n_cols, "}{c}{\\tablename\\ \\thetable{} -- \\textit{Continued from previous page}} \\\\"),
    main_header_no_caption,  # Use version WITHOUT caption
    "\\hline",
    "\\endhead",
    sep = "\n"
  )

  # Footers
  continuation_footer <- paste(
    "\\midrule",
    paste0("\\multicolumn{", n_cols, "}{r}{\\textit{Continued on next page}} \\\\"),
    paste0("\\multicolumn{", n_cols, "}{r}{\\raggedleft{\\itshape * p{\\textless}0.05~~~** p{\\textless}0.01~~~*** p{\\textless}0.001}} \\\\"),
    "\\endfoot",
    sep = "\n"
  )

  final_footer <- paste(
    "\\bottomrule",
    paste0("\\multicolumn{", n_cols, "}{r}{\\raggedleft{\\itshape * p{\\textless}0.05~~~** p{\\textless}0.01~~~*** p{\\textless}0.001}} \\\\"),
    "\\endlastfoot",
    sep = "\n"
  )

  complete_header <- paste(
    first_header,
    continuation_header,
    continuation_footer,
    final_footer,
    sep = "\n"
  )

  return(complete_header)
}


#' Convert confidence intervals to makecell format
convert_ci_to_makecell <- function(tex_content) {
  # Pattern 1: Positive numbers with optional p-values, followed by CI on next line
  pattern1 <- "\\\\centering(?:\\\\arraybslash)?\\s+(\\d+\\.?\\d*)(\\s*\\\\textsuperscript\\{[^}]+\\})?\\s*\n\\s*(\\([^)]+\\))"
  
  # Pattern 2: Negative numbers with {} wrapper, optional p-values, followed by CI on next line
  pattern2 <- "\\\\centering(?:\\\\arraybslash)?\\s+\\{\\}(-?\\d+\\.?\\d*)(\\s*\\\\textsuperscript\\{[^}]+\\})?\\s*\n\\s*(\\([^)]+\\))"
  
  # Pattern 3: Same line format (for cases where they're actually on same line)
  pattern3 <- "\\\\centering(?:\\\\arraybslash)?\\s+(\\d+\\.?\\d*)(\\s*\\\\textsuperscript\\{[^}]+\\})?\\s+(\\([^)]+\\))"
  
  # Pattern 4: Same line format with {} wrapper
  pattern4 <- "\\\\centering(?:\\\\arraybslash)?\\s+\\{\\}(-?\\d+\\.?\\d*)(\\s*\\\\textsuperscript\\{[^}]+\\})?\\s+(\\([^)]+\\))"
  
  # Define replacement function
  replace_function <- function(match_obj) {
    # Extract groups from the match
    full_match <- match_obj[1]
    number <- match_obj[2]
    pvalue <- if(is.na(match_obj[3])) "" else match_obj[3]
    ci <- match_obj[4]
    
    # For patterns with {}, ensure negative sign is present
    if (str_detect(full_match, "\\{\\}") && !str_detect(number, "^-")) {
      number <- paste0("-", number)
    }
    
    # Bold the number and p-value if there are stars
    if (nzchar(pvalue) && str_detect(pvalue, "\\*")) {
      return(paste0("\\makecell{\\textbf{", number, pvalue, "} \\\\ ", ci, "}"))
    } else {
      return(paste0("\\makecell{", number, pvalue, " \\\\ ", ci, "}"))
    }
  }
  
  # Apply all patterns
  result <- tex_content
  
  # Pattern 1
  result <- str_replace_all(result, pattern1, function(x) {
    matches <- str_match(x, pattern1)
    replace_function(matches)
  })
  
  # Pattern 2
  result <- str_replace_all(result, pattern2, function(x) {
    matches <- str_match(x, pattern2)
    replace_function(matches)
  })
  
  # Pattern 3
  result <- str_replace_all(result, pattern3, function(x) {
    matches <- str_match(x, pattern3)
    replace_function(matches)
  })
  
  # Pattern 4
  result <- str_replace_all(result, pattern4, function(x) {
    matches <- str_match(x, pattern4)
    replace_function(matches)
  })
  
  return(result)
}


# Waiting function for file creation and completion
wait_for_file <- function(filepath, timeout = 30, check_content = TRUE, silent = FALSE) {
  start_time <- Sys.time()

  if (!silent) cat("Waiting for file:", filepath, "\n")

  # Wait for file to exist
  while (!file.exists(filepath)) {
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop(paste("Timeout waiting for file:", filepath))
    }
    Sys.sleep(0.1)
  }

  if (check_content) {
    # Additional wait to ensure file writing is complete
    # Check file size stability
    initial_size <- file.size(filepath)
    Sys.sleep(0.5)

    stable_count <- 0
    while (stable_count < 3) { # File size must be stable for 3 consecutive checks
      current_size <- file.size(filepath)
      if (current_size == initial_size && current_size > 0) {
        stable_count <- stable_count + 1
      } else {
        stable_count <- 0
        initial_size <- current_size
      }
      Sys.sleep(0.2)

      if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
        warning(paste("File size not stable within timeout for:", filepath))
        break
      }
    }
  }

  if (!silent) cat("File ready:", filepath, "\n")
  return(TRUE)
}


#####################################################
# HELPER FUNCTIONS FOR REGRESSION TABLE AUTO-DETECTION
#####################################################

#' Detect regression structure for automatic column grouping
#'
#' @param model_labels Character vector of model labels
#'
#' @return List with detected structure information
detect_regression_structure <- function(model_labels) {
  has_study_column <- "Study" %in% model_labels
  is_cross_study_only <- length(model_labels) == 1 && model_labels[1] == "Study"

  # Auto-detect Additive vs Full model pattern
  has_additive_full <- any(grepl("Additive", model_labels)) && any(grepl("Full", model_labels))

  if (has_additive_full && !is_cross_study_only) {
    # Create automatic column groups
    additive_cols <- which(grepl("Additive", model_labels))
    full_cols <- which(grepl("Full", model_labels))

    column_groups <- list(
      additive = list(label = "Additive", cols = additive_cols),
      full = list(label = "Full", cols = full_cols)
    )

    return(list(
      has_study = has_study_column,
      is_cross_study_only = is_cross_study_only,
      column_groups = column_groups
    ))
  }

  return(list(
    has_study = has_study_column,
    is_cross_study_only = is_cross_study_only,
    column_groups = NULL
  ))
}

#' Create default text replacements for regression tables
#'
#' Converts plain-text predictor labels (used by pred_labels.R) into
#' LaTeX math notation after the HTML-to-LaTeX conversion step.
#' Order matters: more specific patterns must come first to avoid
#' partial matches (e.g., "Multiplier Squared" before "Multiplier").
#'
#' @return Named vector of default text replacements
create_default_replacements <- function() {
  c(
    # --- Polynomial terms (most specific first) ---
    "Multiplier Cubed" = "$\\lambda^3$",
    "Multiplier Squared" = "$\\lambda^2$",

    # --- 5-level factor interactions with arms and time ---
    "Multiplier neg1 x Pers" = "$\\lambda_{-1}$ $\\times$ Pers",
    "Multiplier neg1 x EmotChat" = "$\\lambda_{-1}$ $\\times$ EmotChat",
    "Multiplier neg1 x Session" = "$\\lambda_{-1}$ $\\times$ Session",
    "Multiplier neg1 x Week" = "$\\lambda_{-1}$ $\\times$ Week",
    "Multiplier neg0.5 x Pers" = "$\\lambda_{-0.5}$ $\\times$ Pers",
    "Multiplier neg0.5 x EmotChat" = "$\\lambda_{-0.5}$ $\\times$ EmotChat",
    "Multiplier neg0.5 x Session" = "$\\lambda_{-0.5}$ $\\times$ Session",
    "Multiplier neg0.5 x Week" = "$\\lambda_{-0.5}$ $\\times$ Week",
    "Multiplier zero x Pers" = "$\\lambda_0$ $\\times$ Pers",
    "Multiplier zero x EmotChat" = "$\\lambda_0$ $\\times$ EmotChat",
    "Multiplier zero x Session" = "$\\lambda_0$ $\\times$ Session",
    "Multiplier zero x Week" = "$\\lambda_0$ $\\times$ Week",
    "Multiplier pos0.5 x Pers" = "$\\lambda_{+0.5}$ $\\times$ Pers",
    "Multiplier pos0.5 x EmotChat" = "$\\lambda_{+0.5}$ $\\times$ EmotChat",
    "Multiplier pos0.5 x Session" = "$\\lambda_{+0.5}$ $\\times$ Session",
    "Multiplier pos0.5 x Week" = "$\\lambda_{+0.5}$ $\\times$ Week",
    "Multiplier pos1 x Pers" = "$\\lambda_{+1}$ $\\times$ Pers",
    "Multiplier pos1 x EmotChat" = "$\\lambda_{+1}$ $\\times$ EmotChat",
    "Multiplier pos1 x Session" = "$\\lambda_{+1}$ $\\times$ Session",
    "Multiplier pos1 x Week" = "$\\lambda_{+1}$ $\\times$ Week",

    # --- 5-level factor interactions with outcome measures (pooled models) ---
    # Cognitive Reliance
    "Multiplier neg0.5 x Cog. Reliance" = "$\\lambda_{-0.5}$ $\\times$ Cog. Reliance",
    "Multiplier zero x Cog. Reliance" = "$\\lambda_0$ $\\times$ Cog. Reliance",
    "Multiplier pos0.5 x Cog. Reliance" = "$\\lambda_{+0.5}$ $\\times$ Cog. Reliance",
    "Multiplier pos1 x Cog. Reliance" = "$\\lambda_{+1}$ $\\times$ Cog. Reliance",
    # Behavioural Reliance
    "Multiplier neg0.5 x Behav. Reliance" = "$\\lambda_{-0.5}$ $\\times$ Behav. Reliance",
    "Multiplier zero x Behav. Reliance" = "$\\lambda_0$ $\\times$ Behav. Reliance",
    "Multiplier pos0.5 x Behav. Reliance" = "$\\lambda_{+0.5}$ $\\times$ Behav. Reliance",
    "Multiplier pos1 x Behav. Reliance" = "$\\lambda_{+1}$ $\\times$ Behav. Reliance",
    # Responsiveness
    "Multiplier neg0.5 x Responsiveness" = "$\\lambda_{-0.5}$ $\\times$ Responsiveness",
    "Multiplier zero x Responsiveness" = "$\\lambda_0$ $\\times$ Responsiveness",
    "Multiplier pos0.5 x Responsiveness" = "$\\lambda_{+0.5}$ $\\times$ Responsiveness",
    "Multiplier pos1 x Responsiveness" = "$\\lambda_{+1}$ $\\times$ Responsiveness",
    # Understanding
    "Multiplier neg0.5 x Understanding" = "$\\lambda_{-0.5}$ $\\times$ Understanding",
    "Multiplier zero x Understanding" = "$\\lambda_0$ $\\times$ Understanding",
    "Multiplier pos0.5 x Understanding" = "$\\lambda_{+0.5}$ $\\times$ Understanding",
    "Multiplier pos1 x Understanding" = "$\\lambda_{+1}$ $\\times$ Understanding",
    # Connection
    "Multiplier neg0.5 x Connection" = "$\\lambda_{-0.5}$ $\\times$ Connection",
    "Multiplier zero x Connection" = "$\\lambda_0$ $\\times$ Connection",
    "Multiplier pos0.5 x Connection" = "$\\lambda_{+0.5}$ $\\times$ Connection",
    "Multiplier pos1 x Connection" = "$\\lambda_{+1}$ $\\times$ Connection",
    # --- 5-level factor main effects (subscript notation) ---
    "Multiplier neg1" = "$\\lambda_{-1}$",
    "Multiplier neg0.5" = "$\\lambda_{-0.5}$",
    "Multiplier zero" = "$\\lambda_0$",
    "Multiplier pos0.5" = "$\\lambda_{+0.5}$",
    "Multiplier pos1" = "$\\lambda_{+1}$",

    # --- 3-level interactions with arms and time ---
    "Pos Multiplier x Pers" = "$\\lambda_{>0}$ $\\times$ Pers",
    "Pos Multiplier x EmotChat" = "$\\lambda_{>0}$ $\\times$ EmotChat",
    "Pos Multiplier x Session" = "$\\lambda_{>0}$ $\\times$ Session",
    "Pos Multiplier x Week" = "$\\lambda_{>0}$ $\\times$ Week",
    "Zero Multiplier x Pers" = "$\\lambda_0$ $\\times$ Pers",
    "Zero Multiplier x EmotChat" = "$\\lambda_0$ $\\times$ EmotChat",
    "Zero Multiplier x Session" = "$\\lambda_0$ $\\times$ Session",
    "Zero Multiplier x Week" = "$\\lambda_0$ $\\times$ Week",

    # --- 3-level interactions with outcome measures (pooled models) ---
    # Cognitive Reliance
    "Zero Multiplier x Cog. Reliance" = "$\\lambda_0$ $\\times$ Cog. Reliance",
    "Pos Multiplier x Cog. Reliance" = "$\\lambda_{>0}$ $\\times$ Cog. Reliance",
    # Behavioural Reliance
    "Zero Multiplier x Behav. Reliance" = "$\\lambda_0$ $\\times$ Behav. Reliance",
    "Pos Multiplier x Behav. Reliance" = "$\\lambda_{>0}$ $\\times$ Behav. Reliance",
    # Responsiveness
    "Zero Multiplier x Responsiveness" = "$\\lambda_0$ $\\times$ Responsiveness",
    "Pos Multiplier x Responsiveness" = "$\\lambda_{>0}$ $\\times$ Responsiveness",
    # Understanding
    "Zero Multiplier x Understanding" = "$\\lambda_0$ $\\times$ Understanding",
    "Pos Multiplier x Understanding" = "$\\lambda_{>0}$ $\\times$ Understanding",
    # Connection
    "Zero Multiplier x Connection" = "$\\lambda_0$ $\\times$ Connection",
    "Pos Multiplier x Connection" = "$\\lambda_{>0}$ $\\times$ Connection",
    # --- 3-level main effects (subscript notation) ---
    "Pos Multiplier" = "$\\lambda_{>0}$",
    "Zero Multiplier" = "$\\lambda_0$",

    # --- Continuous interactions with arms and time ---
    "Multiplier x Pers" = "$\\lambda$ $\\times$ Pers",
    "Multiplier x EmotChat" = "$\\lambda$ $\\times$ EmotChat",
    "Multiplier x Session" = "$\\lambda$ $\\times$ Session",
    "Multiplier x Week" = "$\\lambda$ $\\times$ Week",

    # --- Continuous interactions with outcome measures (pooled models) ---
    "Multiplier x Cog. Reliance" = "$\\lambda$ $\\times$ Cog. Reliance",
    "Multiplier x Behav. Reliance" = "$\\lambda$ $\\times$ Behav. Reliance",
    "Multiplier x Responsiveness" = "$\\lambda$ $\\times$ Responsiveness",
    "Multiplier x Understanding" = "$\\lambda$ $\\times$ Understanding",
    "Multiplier x Connection" = "$\\lambda$ $\\times$ Connection",
    # --- Continuous main effect (fallback, must be last) ---
    "Multiplier" = "$\\lambda$",

    # --- Random effects: shorten time variable names ---
    "session\\_numeric" = "time",
    "session_numeric" = "time",
    "week\\_numeric" = "time",
    "week_numeric" = "time",

    # --- Catch-all: convert remaining " x " to LaTeX times symbol ---
    " x " = " $\\times$ "
  )
}