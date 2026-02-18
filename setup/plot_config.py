"""
Shared plotting configuration for publication-ready figures.

Matches R's ggplot2 theme_pub style with Pastel2/Set2 color palettes.
"""

import matplotlib.pyplot as plt
import seaborn as sns

# Pastel2 palette (matches R's RColorBrewer)
PASTEL2 = [
    "#b3e2cd",
    "#fdcdac",
    "#cbd5e8",
    "#f4cae4",
    "#e6f5c9",
    "#fff2ae",
    "#f1e2cc",
    "#cccccc",
]

# Set2 palette (matches R's RColorBrewer)
SET2 = [
    "#66c2a5",
    "#fc8d62",
    "#8da0cb",
    "#e78ac3",
    "#a6d854",
    "#ffd92f",
    "#e5c494",
    "#b3b3b3",
]

# Model-specific colors
MODEL_COLORS = {
    "GPT-4o": "mediumpurple",
    "Claude-3.7": "sandybrown",
    "Llama-3.1-70b": "dodgerblue",
    "gpt": "mediumpurple",
    "claude": "sandybrown",
    "llama-3.1-70b": "dodgerblue",
}

# Model display name mapping
MODEL_RENAME = {
    "gpt": "GPT-4o",
    "claude": "Claude-3.7",
    "llama-3.1-70b": "Llama-3.1-70b",
}

# Choice colors (target vs antitarget)
CHOICE_COLORS = {
    "target": "firebrick",
    "antitarget": "steelblue",
    True: "firebrick",
    False: "steelblue",
}


def set_pub_style():
    """Set publication-ready matplotlib style matching R's theme_pub."""
    # Use Arial (has bold variant available, unlike Helvetica Neue .ttc on macOS)
    try:
        import matplotlib.font_manager as fm

        available_fonts = [f.name for f in fm.fontManager.ttflist]
        if "Arial" in available_fonts:
            font_family = "Arial"
        elif "DejaVu Sans" in available_fonts:
            font_family = "DejaVu Sans"
        else:
            font_family = "sans-serif"
    except Exception:
        font_family = "sans-serif"

    plt.rcParams.update(
        {
            # Figure
            "figure.facecolor": "white",
            "figure.dpi": 100,
            # Font - match R's theme_pub (base_size=18)
            "font.family": font_family,
            "font.size": 14,
            # Axes
            "axes.titlesize": 18,
            "axes.titleweight": "bold",
            "axes.labelsize": 16,
            "axes.labelweight": "bold",
            "axes.spines.top": False,
            "axes.spines.right": False,
            "axes.facecolor": "white",
            "axes.edgecolor": "black",
            "axes.linewidth": 1,
            "axes.grid": True,
            "axes.axisbelow": True,
            # Grid
            "grid.color": "#f0f0f0",
            "grid.linewidth": 0.5,
            "grid.alpha": 1.0,
            # Ticks - match R's rel(1.1)
            "xtick.labelsize": 14,
            "ytick.labelsize": 14,
            # Legend - match R's rel(1.1)
            "legend.fontsize": 14,
            "legend.frameon": False,
            "legend.title_fontsize": 16,
            # Figure title (suptitle)
            "figure.titlesize": 18,
            "figure.titleweight": "bold",
            # Save
            "savefig.dpi": 300,
            "savefig.bbox": "tight",
            "savefig.facecolor": "white",
        }
    )

    # Set seaborn palette
    sns.set_palette(SET2)
    print(f"Publication style loaded (font: {font_family})")


def add_legend_bottom(ax, ncol=3):
    """Move legend to bottom, horizontal orientation."""
    ax.legend(
        loc="upper center",
        bbox_to_anchor=(0.5, -0.15),
        ncol=ncol,
        frameon=False,
        fontsize=14,
    )


def save_fig(fig, output_dir, name, formats=None):
    """
    Save figure in multiple formats.

    Args:
        fig: matplotlib figure
        output_dir: Path to output directory
        name: filename without extension
        formats: list of formats (default: ["pdf"])
    """
    if formats is None:
        formats = ["pdf"]

    for fmt in formats:
        fig.savefig(output_dir / f"{name}.{fmt}", bbox_inches="tight", dpi=300)
    print(f"Saved: {name}")
