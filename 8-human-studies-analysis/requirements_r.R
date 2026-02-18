# R package requirements for 8-human-studies-analysis
# Install with: Rscript requirements_r.R

packages <- c(
  # Core data manipulation
  "tidyverse",
  "jsonlite",
  "reshape2",

  # Mixed effects models
  "lme4",
  "lmerTest",
  "emmeans",
  "parameters",
  "performance",

  # Plotting
  "patchwork",
  "cowplot",
  "ggeffects",
  "ggthemes",
  "ggbeeswarm",
  "ggsignif",
  "ggpattern",
  "gridExtra",
  "pheatmap",
  "RColorBrewer",

  # Tables and reports
  "knitr",
  "kableExtra",
  "broom",
  "sjPlot",
  "html2latex",

  # Statistical analysis
  "psych",
  "PlackettLuce",

  # Parallel processing
  "future.apply"
)

install.packages(packages, repos = "https://cloud.r-project.org")
