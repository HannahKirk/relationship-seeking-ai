"""
Centralized captions for tables and figures.

Import these into analysis scripts to maintain consistency.
"""

# =============================================================================
# TABLE CAPTIONS
# =============================================================================

TABLES = {
    "sociodemographics": (
        "Sociodemographic characteristics of participants across the three studies. "
        "Percentages shown within each study."
    ),
}

# =============================================================================
# FIGURE CAPTIONS
# =============================================================================

FIGURES = {
    "geographic_distribution": (
        "Geographic distribution of participants across UK regions."
    ),
    "factor_loadings": (
        "Factor loadings from exploratory factor analysis of pre-treatment attitude items."
    ),
    "cluster_pca": (
        "PCA projection of participant clusters based on pre-treatment attitudes."
    ),
    "anthro_attitudes_survey": (
        "Distribution of responses to the Societal Attitudes to Anthropomorphism survey."
    ),
    "topic_preferences": (
        "Participant rankings of AI use case preferences (higher rank = more preferred)."
    ),
    "stated_prefs_distributions": (
        "Distribution of pre-treatment stated preferences for AI characteristics."
    ),
    "stated_prefs_correlations": (
        "Correlation matrix of pre-treatment stated preference items."
    ),
}


def get_table_caption(key: str) -> str:
    """Get a table caption by key."""
    return TABLES.get(key, "ADD CAPTION HERE")


def get_figure_caption(key: str) -> str:
    """Get a figure caption by key."""
    return FIGURES.get(key, "ADD CAPTION HERE")
