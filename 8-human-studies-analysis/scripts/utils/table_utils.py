#!/usr/bin/env python3
"""
Table utility functions for human studies analysis.

Includes functions for:
- Multi-study demographic table generation
- HTML and LaTeX table styling
"""

import re
import numpy as np
import pandas as pd


# =============================================================================
# STUDY COLORS
# =============================================================================

STUDY_COLORS = {
    "calibration": "#fbbd05",
    "cross-sectional": "#32bcdd",
    "longitudinal": "#ff8a67",
}

# LaTeX bar command mapping for studies
LATEX_BAR_COMMANDS = {
    "calibration": "calpcb",
    "cross-sectional": "crosspcb",
    "longitudinal": "longpcb",
}


# =============================================================================
# TABLE GENERATION
# =============================================================================


def create_multi_study_demographic_table(data, categorical_vars, study_col="study_id"):
    """
    Create a demographic table with separate count/pct columns for each study.

    Parameters:
    -----------
    data : pd.DataFrame
        The dataset
    categorical_vars : dict
        Dictionary mapping column names to display labels
    study_col : str
        Column name for study grouping

    Returns:
    --------
    tuple: (DataFrame, list of headers, list of count_cols, list of bar_cols)
    """
    # Get unique studies
    studies = sorted(data[study_col].unique())

    # Create column names for each study
    count_cols = [f"Count_{study}" for study in studies]
    bar_cols = [f"Pct_{study}" for study in studies]

    # Create full column list
    columns = ["Category"]
    for study in studies:
        columns.extend([f"Count_{study}", f"Pct_{study}"])

    headers = ["Total Participants"]
    rows = []

    # Total participants row
    total_row = ["Total Participants"]
    for study in studies:
        study_data = data[data[study_col] == study]
        total_row.extend([len(study_data), np.nan])
    rows.append(total_row)

    # Loop through categorical variables
    for col_name, display_label in categorical_vars.items():
        if col_name not in data.columns:
            continue

        headers.append(display_label)

        # Add category header row
        header_row = [display_label] + [np.nan] * (len(studies) * 2)
        rows.append(header_row)

        # Get all unique categories across all studies
        all_categories = set()
        for study in studies:
            study_data = data[data[study_col] == study]
            categories = study_data[col_name].value_counts(dropna=False).index.tolist()
            all_categories.update(categories)

        # Separate special categories
        special_categories = []
        regular_categories = []

        for category in all_categories:
            if pd.isna(category):
                special_categories.append("Missing/NaN")
            elif str(category).lower() in [
                "prefer not to say",
                "other",
                "prefer not to say/other",
            ]:
                special_categories.append(category)
            else:
                regular_categories.append(category)

        # Sort regular categories by total count across all studies
        category_totals = {}
        for category in regular_categories:
            total_count = 0
            for study in studies:
                study_data = data[data[study_col] == study]
                count = (study_data[col_name] == category).sum()
                total_count += count
            category_totals[category] = total_count

        sorted_regular = sorted(
            regular_categories, key=lambda x: category_totals[x], reverse=True
        )

        # Add regular categories
        for category in sorted_regular:
            cat_row = [f"  {category}"]
            for study in studies:
                study_data = data[data[study_col] == study]
                count = (study_data[col_name] == category).sum()
                total = len(study_data[study_data[col_name].notna()])
                pct = count / total if total > 0 else 0
                cat_row.extend([count, pct])
            rows.append(cat_row)

        # Add special categories
        for category in special_categories:
            cat_row = [f"  {category}"]
            for study in studies:
                study_data = data[data[study_col] == study]
                if category == "Missing/NaN":
                    count = study_data[col_name].isna().sum()
                else:
                    count = (study_data[col_name] == category).sum()
                total = len(study_data)
                pct = count / total if total > 0 else 0
                cat_row.extend([count, pct])
            rows.append(cat_row)

    df = pd.DataFrame(rows, columns=columns)
    return df, headers, count_cols, bar_cols


# =============================================================================
# STYLING HELPERS
# =============================================================================


def format_count_and_pct_columns(
    styled_df, count_cols, bar_cols, cost_cols=None, pct=True, nan_mode=""
):
    """Format count and percentage columns in a styled DataFrame."""
    formatting_dict = {}

    for col in count_cols:
        try:
            pd.to_numeric(styled_df.data[col], errors="raise")
            formatting_dict[col] = "{:,.0f}"
        except Exception:
            pass

    if pct:
        for col in bar_cols:
            try:
                pd.to_numeric(styled_df.data[col], errors="raise")
                formatting_dict[col] = "{:.1%}"
            except Exception:
                pass

    if cost_cols is not None:
        for col in cost_cols:
            try:
                pd.to_numeric(styled_df.data[col], errors="raise")
                formatting_dict[col] = "{:,.2f}"
            except Exception:
                pass

    return styled_df.format(formatting_dict, na_rep=nan_mode)


def highlight_rows_html(row, headers):
    """Highlight header rows in HTML tables."""
    if row["Category"] in headers:
        styles = [
            "font-weight: bold; background-color: #2c3d4f; color: white; text-align: left;"
        ] + ["font-weight: bold; background-color: #2c3d4f; color: white;"] * (
            len(row) - 1
        )
    else:
        styles = [""] * len(row)
    return styles


# =============================================================================
# HTML STYLING
# =============================================================================


def apply_custom_styles_to_html_multi(
    df,
    headers,
    category_cols,
    count_cols,
    bar_cols,
    cost_cols=None,
    highlight_row_headers=False,
    highlight_top_header=False,
    nan_mode="",
    study_colors=None,
):
    """Apply custom HTML styles to a multi-study demographic table."""
    if study_colors is None:
        study_colors = STUDY_COLORS

    styled_df = df.style

    # Add bars to each study's 'Pct' columns with study-specific colors
    for col in bar_cols:
        if col in df.columns:
            study_name = col.split("_")[-1] if "_" in col else col
            color = study_colors.get(study_name, "#d16249")
            styled_df = styled_df.bar(
                subset=[col], align="left", vmin=0, vmax=1, color=color, width=70
            )

    # Format columns
    styled_df = format_count_and_pct_columns(
        styled_df,
        count_cols=count_cols,
        bar_cols=bar_cols,
        cost_cols=cost_cols,
        nan_mode=nan_mode,
    )

    # Italicize specific rows
    styled_df = styled_df.map(
        lambda val: (
            "font-style: italic"
            if val in ["Prefer not to say", "Other", "Prefer not to say/Other"]
            else ""
        ),
        subset=category_cols,
    )

    # Apply white bottom border to 'Pct' column cells
    styled_df = styled_df.apply(
        lambda row: [
            "border-bottom: 1px solid white" if col_name in bar_cols else ""
            for col_name in row.index
        ],
        axis=1,
    )

    # Additional custom styles
    shared_tab_properties = [
        {"selector": "th", "props": "text-align: center;"},
        {"selector": "tr:nth-of-type(odd)", "props": "background-color: #f7f7f7;"},
        {"selector": "tr:nth-of-type(even)", "props": "background-color: white;"},
        {
            "selector": ".dataframe",
            "props": [
                ("border-top", "2px solid black"),
                ("border-bottom", "2px solid black"),
            ],
        },
        {"selector": "tr", "props": [("height", "0px")]},
    ]

    if highlight_row_headers and highlight_top_header:
        shared_tab_properties.append(
            {
                "selector": "th",
                "props": [
                    ("background-color", "#2c3d4f"),
                    ("color", "white"),
                    ("font-weight", "bold"),
                    ("text-align", "left"),
                ],
            }
        )
        styled_df = styled_df.apply(highlight_rows_html, headers=headers, axis=1)

    elif not highlight_row_headers and highlight_top_header:
        shared_tab_properties.append(
            {
                "selector": "th",
                "props": [
                    ("background-color", "#2c3d4f"),
                    ("color", "white"),
                    ("font-weight", "bold"),
                    ("text-align", "left"),
                ],
            }
        )

    elif highlight_row_headers and not highlight_top_header:
        shared_tab_properties.append({"selector": "thead", "props": "display: none;"})
        styled_df = styled_df.apply(highlight_rows_html, headers=headers, axis=1)

    styled_df = styled_df.set_table_styles(
        table_styles=shared_tab_properties, overwrite=False
    )
    styled_df = styled_df.hide(axis="index")

    return styled_df


# =============================================================================
# LATEX STYLING
# =============================================================================


def midline_rows_latex_multi(df, header_indices):
    """Insert midrule rows around header rows for LaTeX."""
    new_rows = []
    for index, row in df.iterrows():
        if index in header_indices:
            new_rows.append(["\\midrule"] + [np.nan] * (len(df.columns) - 1))
            new_rows.append(
                [f"\\textbf{{{row['Category']}}}"] + [np.nan] * (len(df.columns) - 1)
            )
            new_rows.append(["\\midrule"] + [np.nan] * (len(df.columns) - 1))
        else:
            new_rows.append(row.values)
    return pd.DataFrame(new_rows, columns=df.columns)


def color_rows_latex_multi(data, header_indices):
    """Apply alternating row colors for LaTeX tables."""
    if header_indices is not None:
        # Split using pandas iloc instead of np.split to avoid swapaxes warning
        split_indices = [0] + list(header_indices) + [len(data)]
        rejoins = []
        for i in range(len(split_indices) - 1):
            sp = data.iloc[split_indices[i]:split_indices[i + 1]].copy()
            sp.index = pd.RangeIndex(len(sp.index))
            if len(sp) > 1:
                s = sp.index % 2 != 0
                s = pd.concat([pd.Series(s)] * data.shape[1], axis=1)
                z = pd.DataFrame(
                    np.where(s, "background-color:white", "background-color: #f2f2f2"),
                    index=sp.index,
                    columns=sp.columns,
                )
                z.iloc[0] = "background-color:white"
            else:
                z = pd.DataFrame(
                    "background-color: white", index=sp.index, columns=sp.columns
                )
            rejoins.append(z)
        colored_data = pd.concat(rejoins, axis=0, ignore_index=True)
    else:
        s = data.index % 2 != 0
        s = pd.concat([pd.Series(s)] * data.shape[1], axis=1)
        colored_data = pd.DataFrame(
            np.where(s, "background-color:#f2f2f2", "background-color: white"),
            index=data.index,
            columns=data.columns,
        )
    return colored_data


def apply_custom_styles_to_latex_multi(
    df,
    headers,
    category_cols,
    count_cols,
    bar_cols,
    cost_cols=None,
    highlight_row_headers=False,
    highlight_top_header=False,
    nan_mode="",
    environment=None,
    bar_commands=None,
    caption="ADD CAPTION HERE",
    label="tab:ADD_LABEL_HERE",
    column_spec=None,
    multicolumn_headers=None,
):
    """Apply custom LaTeX styles to a multi-study demographic table."""
    if bar_commands is None:
        bar_commands = LATEX_BAR_COMMANDS

    df_copy = df.copy()
    orig_cols = df_copy.columns

    # Convert bar columns to latex bar command strings
    for col in bar_cols:
        if col in df_copy.columns:
            study_name = col.split("_")[-1] if "_" in col else col
            bar_command = bar_commands.get(study_name, "pcb")

            df_copy[f"{col}_latex_bar"] = df_copy[col].apply(
                lambda x: (
                    f"\\{bar_command}{{{x*100:.1f}}}\\%" if not pd.isna(x) else np.nan
                )
            )
            df_copy = df_copy.drop(columns=[col])

    df_copy = df_copy.rename(
        columns={
            f"{col}_latex_bar": col
            for col in bar_cols
            if f"{col}_latex_bar" in df_copy.columns
        }
    )
    df_copy = df_copy[orig_cols]

    # Track header rows
    if highlight_row_headers:
        header_indices = []
        for index, row in df_copy.iterrows():
            if row["Category"] in headers and row["Category"] != "Total Participants":
                header_indices.append(index)

        df_copy = midline_rows_latex_multi(df_copy, header_indices)

        trio_header_indices = []
        for index, row in df_copy.iterrows():
            try:
                if ("midrule" in str(row["Category"])) or (
                    "textbf" in str(row["Category"])
                ):
                    trio_header_indices.append(index)
            except Exception:
                pass

        if highlight_top_header:
            trio_header_indices = [0] + trio_header_indices

    # Convert to styled DataFrame
    styled_df = df_copy.style
    styled_df = format_count_and_pct_columns(
        styled_df,
        count_cols=count_cols,
        bar_cols=bar_cols,
        cost_cols=cost_cols,
        pct=False,
        nan_mode=nan_mode,
    )

    styled_df = styled_df.apply(
        lambda row: [
            (
                "font-style: italic"
                if str(val) in ["Prefer not to say", "Other", "Prefer not to say/Other"]
                else ""
            )
            for val in row
        ],
        axis=1,
        subset=category_cols,
    )

    if highlight_row_headers:
        styled_df = styled_df.apply(
            color_rows_latex_multi, header_indices=trio_header_indices, axis=None
        )
    elif highlight_top_header:
        styled_df = styled_df.apply(
            color_rows_latex_multi, header_indices=None, axis=None
        )

    shared_tab_properties = [
        {"selector": "toprule", "props": ":toprule;"},
        {"selector": "midrule", "props": ":midrule;"},
        {"selector": "bottomrule", "props": ":bottomrule;"},
    ]

    if highlight_row_headers and highlight_top_header:
        shared_tab_properties.append(
            {
                "selector": "th",
                "props": [
                    ("color", "black"),
                    ("font-weight", "bold"),
                    ("text-align", "left"),
                ],
            }
        )

    styled_df = styled_df.set_table_styles(
        table_styles=shared_tab_properties, overwrite=True
    )
    styled_df = styled_df.hide(axis="index")

    latex_str = styled_df.to_latex(
        hrules=True,
        convert_css=True,
        environment=environment,
        caption=caption,
        label=label,
    )

    # Clean up LaTeX string
    inside_string = "& {\\cellcolor{white}} " + f"{nan_mode} "
    midrule_replace = (
        "{\\cellcolor{white}} \\midrule "
        + inside_string * (len(df_copy.columns) - 1)
        + "\\\\"
    )
    latex_str = (
        latex_str.replace(midrule_replace, "\\midrule")
        .replace("\\theadnone", "")
        .replace("\\thblack", "")
    )

    latex_str = re.sub(r"({\\\w+(?:\[[^\]]+\])?{[^}]+})\s+", r"\1", latex_str)
    latex_str = re.sub(r"}\s+([^{&\\])", r"}\1", latex_str)
    latex_str = re.sub(r"}\s+&", r"} &", latex_str)

    # Apply custom column spec if provided
    if column_spec is not None:
        # Replace the auto-generated column spec
        latex_str = re.sub(
            r"\\begin{longtable}\{[^}]+\}",
            f"\\\\begin{{longtable}}{{{column_spec}}}",
            latex_str,
        )
        latex_str = re.sub(
            r"\\begin{tabular}\{[^}]+\}",
            f"\\\\begin{{tabular}}{{{column_spec}}}",
            latex_str,
        )

    # Apply multicolumn headers if provided
    if multicolumn_headers is not None:
        # Find and replace the header row
        # Pattern: Category & Count_calibration & Pct_calibration & ...
        header_pattern = r"Category & Count_[^\n]+\\\\"
        replacement = multicolumn_headers + " \\\\"
        if re.search(header_pattern, latex_str):
            # Use a function as replacement to avoid backslash escape issues
            latex_str = re.sub(header_pattern, lambda m: replacement, latex_str)

    # Make "Total Participants" row bold
    latex_str = latex_str.replace(
        "Total Participants",
        "\\textbf{Total Participants}"
    )

    # Make "Continued on next page" italic
    latex_str = latex_str.replace(
        "Continued on next page",
        "\\textit{Continued on next page}"
    )

    return latex_str


# =============================================================================
# MAIN STYLING ENTRY POINT
# =============================================================================


def style_demographic_table(
    df,
    headers,
    category_cols=None,
    count_cols=None,
    bar_cols=None,
    cost_cols=None,
    format="html",
    highlight_mode="rows",
    environment=None,
    nan_mode="",
    study_colors=None,
    caption="ADD CAPTION HERE",
    label="tab:ADD_LABEL_HERE",
    column_spec=None,
    multicolumn_headers=None,
):
    """
    Style a demographic table for HTML or LaTeX output.

    Parameters:
    -----------
    df : pd.DataFrame
        The demographic table from create_multi_study_demographic_table
    headers : list
        List of header row labels
    category_cols : list
        Column names for category columns (default: ["Category"])
    count_cols : list
        Column names for count columns
    bar_cols : list
        Column names for percentage columns (with bars)
    cost_cols : list, optional
        Column names for cost columns
    format : str
        Output format: "html" or "latex"
    highlight_mode : str
        "rows" (highlight category headers), "top" (highlight column headers),
        or "both"
    environment : str, optional
        LaTeX environment (e.g., "longtable")
    nan_mode : str
        String to display for NaN values
    study_colors : dict, optional
        Custom study color mapping

    Returns:
    --------
    Styled DataFrame (HTML) or LaTeX string
    """
    if category_cols is None:
        category_cols = ["Category"]

    highlight_row_headers = highlight_mode in ["rows", "both"]
    highlight_top_header = highlight_mode in ["top", "both"]

    if format == "html":
        return apply_custom_styles_to_html_multi(
            df,
            headers,
            category_cols,
            count_cols,
            bar_cols,
            cost_cols,
            highlight_row_headers,
            highlight_top_header,
            nan_mode=nan_mode,
            study_colors=study_colors,
        )
    elif format == "latex":
        return apply_custom_styles_to_latex_multi(
            df,
            headers,
            category_cols,
            count_cols,
            bar_cols,
            cost_cols,
            highlight_row_headers,
            highlight_top_header,
            nan_mode=nan_mode,
            environment=environment,
            caption=caption,
            label=label,
            column_spec=column_spec,
            multicolumn_headers=multicolumn_headers,
        )
    else:
        raise ValueError(f"Format '{format}' not supported. Use 'html' or 'latex'.")
