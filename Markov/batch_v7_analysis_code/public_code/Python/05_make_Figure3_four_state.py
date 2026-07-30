from __future__ import annotations

from pathlib import Path
import os

import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.patches import FancyArrowPatch, FancyBboxPatch
import numpy as np
import pandas as pd


HERE = Path(__file__).resolve().parent
OUTPUT_DIR = Path(os.environ.get("V7_OUTPUT_DIR", HERE))
ABS_PATH = OUTPUT_DIR / "06_CHARLS_IPCW_absolute_transition_probabilities.csv"
DIFF_PATH = OUTPUT_DIR / "25_CHARLS_period_transition_differences_bootstrap_2000.csv"
PNG_PATH = OUTPUT_DIR / "Figure3_CHARLS_four_state_transitions.png"
PDF_PATH = OUTPUT_DIR / "Figure3_CHARLS_four_state_transitions.pdf"
SOURCE_PATH = OUTPUT_DIR / "Figure3_CHARLS_four_state_source_data.csv"

STATE_ORDER = ["Low deficit", "Intermediate deficit", "High deficit"]
DEST_ORDER = ["Low deficit", "Intermediate deficit", "High deficit", "Death"]
STATE_COLORS = {
    "Low deficit": "#2A8C82",
    "Intermediate deficit": "#B4872D",
    "High deficit": "#C95A50",
    "Death": "#6C4A73",
}


def transition_role(origin: str, destination: str) -> str:
    if destination == "Death":
        return "Death"
    i = STATE_ORDER.index(origin)
    j = STATE_ORDER.index(destination)
    if j < i:
        return "Recovery"
    if j == i:
        return "Persistence"
    return "Deterioration"


def prepare_data() -> pd.DataFrame:
    absolute = pd.read_csv(ABS_PATH)
    absolute = absolute.loc[absolute["dimension"].eq("Overall")].copy()
    absolute["probability_pct"] = 100 * absolute["probability"]
    wide = absolute.pivot_table(
        index=["from_state", "from_state_label", "to_state", "to_state_label"],
        columns="period",
        values="probability_pct",
    ).reset_index()

    differences = pd.read_csv(DIFF_PATH)
    differences = differences.loc[differences["dimension"].eq("Overall")].copy()
    keep = [
        "from_state",
        "from_state_label",
        "to_state",
        "to_state_label",
        "percentage_point_difference",
        "conf_low_pp",
        "conf_high_pp",
    ]
    data = wide.merge(differences[keep], on=keep[:4], validate="one_to_one")
    data["role"] = [
        transition_role(origin, destination)
        for origin, destination in zip(
            data["from_state_label"], data["to_state_label"]
        )
    ]
    data["transition"] = (
        data["from_state_label"] + " \u2192 " + data["to_state_label"]
    )
    short = {
        "Low deficit": "Low",
        "Intermediate deficit": "Mid",
        "High deficit": "High",
        "Death": "Death",
    }
    data["transition_short"] = (
        data["from_state_label"].map(short)
        + " \u2192 "
        + data["to_state_label"].map(short)
    )
    data["row_order"] = (
        (data["from_state"] - 1) * 4 + (data["to_state"] - 1)
    )
    data = data.sort_values("row_order").reset_index(drop=True)
    data.to_csv(SOURCE_PATH, index=False, encoding="utf-8-sig")
    return data


def add_panel_label(
    ax: plt.Axes, label: str, x: float = -0.06, y: float = 1.03
) -> None:
    ax.text(
        x,
        y,
        label,
        transform=ax.transAxes,
        fontsize=12,
        fontweight="bold",
        va="bottom",
    )


def state_box(
    ax: plt.Axes,
    xy: tuple[float, float],
    text: str,
    color: str,
    width: float = 0.18,
) -> None:
    x, y = xy
    patch = FancyBboxPatch(
        (x - width / 2, y - 0.09),
        width,
        0.18,
        boxstyle="round,pad=0.018,rounding_size=0.02",
        facecolor="white",
        edgecolor=color,
        linewidth=1.6,
    )
    ax.add_patch(patch)
    ax.text(x, y, text, ha="center", va="center", fontsize=9.2,
            color=color, fontweight="bold")


def arrow(
    ax: plt.Axes,
    start: tuple[float, float],
    end: tuple[float, float],
    color: str,
    rad: float = 0.0,
    linestyle: str = "-",
    linewidth: float = 1.4,
) -> None:
    ax.add_patch(
        FancyArrowPatch(
            start,
            end,
            arrowstyle="-|>",
            mutation_scale=10,
            connectionstyle=f"arc3,rad={rad}",
            color=color,
            linewidth=linewidth,
            linestyle=linestyle,
            shrinkA=3,
            shrinkB=3,
        )
    )


def death_lane(
    ax: plt.Axes,
    source_x: float,
    lane_y: float,
    target_x: float,
    box_top: float,
) -> None:
    color = STATE_COLORS["Death"]
    ax.plot(
        [source_x, source_x, target_x],
        [box_top, lane_y, lane_y],
        color=color,
        linewidth=1.05,
        solid_capstyle="round",
        zorder=1,
    )
    arrow(
        ax,
        (target_x, lane_y),
        (target_x, box_top),
        color,
        rad=0,
        linewidth=1.05,
    )


def draw_state_space(ax: plt.Axes) -> None:
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis("off")
    add_panel_label(ax, "a")
    ax.text(0.02, 0.94, "Four-state health space", fontsize=11,
            fontweight="bold")
    ax.text(0.02, 0.80, "Living-state mobility with death as an absorbing state",
            fontsize=8.5, color="#7A7F85")

    low = (0.18, 0.25)
    intermediate = (0.45, 0.25)
    high = (0.70, 0.25)
    death = (0.91, 0.25)
    state_box(ax, low, "Low deficit", STATE_COLORS["Low deficit"])
    state_box(
        ax,
        intermediate,
        "Intermediate",
        STATE_COLORS["Intermediate deficit"],
        width=0.20,
    )
    state_box(ax, high, "High deficit", STATE_COLORS["High deficit"])
    state_box(ax, death, "Death", STATE_COLORS["Death"], width=0.13)

    # Adjacent living-state mobility stays entirely in the gaps between boxes.
    arrow(ax, (0.285, 0.29), (0.335, 0.29), "#4B4F54")
    arrow(ax, (0.335, 0.21), (0.285, 0.21), "#4B4F54")
    arrow(ax, (0.565, 0.29), (0.595, 0.29), "#4B4F54")
    arrow(ax, (0.595, 0.21), (0.565, 0.21), "#4B4F54")

    # A single labelled double-headed arc makes the non-adjacent low-high
    # transitions legible after the figure is reduced to publication width.
    ax.add_patch(
        FancyArrowPatch(
            (0.27, 0.16),
            (0.61, 0.16),
            arrowstyle="<|-|>",
            mutation_scale=7.5,
            connectionstyle="arc3,rad=0.24",
            linewidth=1.1,
            linestyle=(0, (2, 2)),
            color="#858B91",
            zorder=2,
        )
    )
    ax.text(
        0.44,
        0.015,
        "Direct low–high transitions",
        ha="center",
        va="bottom",
        fontsize=7.0,
        color="#6F757B",
    )

    # Death transitions use separate lanes above the boxes, so no path crosses
    # a state label or another state box.
    box_top = 0.34
    death_lane(ax, low[0], 0.68, 0.865, box_top)
    death_lane(ax, intermediate[0], 0.58, 0.910, box_top)
    death_lane(ax, high[0], 0.48, 0.955, box_top)
    ax.text(0.91, 0.06, "Absorbing", ha="center", fontsize=8,
            color=STATE_COLORS["Death"])


def draw_probability_panel(ax: plt.Axes, data: pd.DataFrame) -> None:
    add_panel_label(ax, "b", x=-0.18, y=1.16)
    ax.set_title(
        "Model-standardised two-year probabilities",
        loc="left",
        fontsize=10.5,
        fontweight="bold",
        y=1.14,
        pad=0,
    )
    ax.text(
        0,
        1.085,
        "Open: pre-expansion; filled: expansion",
        transform=ax.transAxes,
        fontsize=8.4,
        color="#7A7F85",
    )
    y = np.arange(len(data))[::-1]
    for yi, (_, row) in zip(y, data.iterrows()):
        color = STATE_COLORS[row["to_state_label"]]
        ax.scatter(
            row["Pre-expansion"],
            yi,
            s=43,
            marker="o",
            facecolors="white",
            edgecolors=color,
            linewidths=1.2,
            zorder=3,
        )
        ax.scatter(
            row["Expansion"],
            yi,
            s=43,
            marker="o",
            facecolors=color,
            edgecolors=color,
            linewidths=0.8,
            zorder=4,
        )
    ax.set_yticks(y)
    ax.set_yticklabels(data["transition_short"], fontsize=7.2)
    ax.set_xlim(0, 82)
    ax.set_xticks([0, 20, 40, 60, 80])
    ax.set_xlabel("Transition probability (%)", fontsize=8.5)
    ax.tick_params(axis="x", labelsize=7.5)
    ax.spines[["top", "right", "left"]].set_visible(False)
    ax.tick_params(axis="y", length=0)
    ax.grid(axis="x", color="#E3E5E7", linewidth=0.55)
    for boundary in [3.5, 7.5]:
        ax.axhline(boundary, color="#D9DDE1", linewidth=0.8)

    destination_handles = [
        Line2D(
            [0],
            [0],
            marker="o",
            color="none",
            markerfacecolor=STATE_COLORS[state],
            markeredgecolor=STATE_COLORS[state],
            markersize=5.5,
            label=label,
        )
        for state, label in [
            ("Low deficit", "Low destination"),
            ("Intermediate deficit", "Intermediate destination"),
            ("High deficit", "High destination"),
            ("Death", "Death"),
        ]
    ]
    ax.legend(
        handles=destination_handles,
        loc="lower left",
        bbox_to_anchor=(0, 1.015),
        ncol=4,
        frameon=False,
        fontsize=6.9,
        handletextpad=0.35,
        columnspacing=0.85,
        borderaxespad=0,
    )


def draw_difference_panel(ax: plt.Axes, data: pd.DataFrame) -> None:
    add_panel_label(ax, "c", x=-0.06, y=1.16)
    ax.set_title(
        "Expansion-period difference",
        loc="left",
        fontsize=10.5,
        fontweight="bold",
        y=1.14,
        pad=0,
    )
    ax.text(
        0,
        1.085,
        "Point estimate and 95% cluster-bootstrap CI",
        transform=ax.transAxes,
        fontsize=8.4,
        color="#7A7F85",
    )
    y = np.arange(len(data))[::-1]
    for yi, row in zip(y, data.itertuples()):
        color = STATE_COLORS[row.to_state_label]
        estimate = row.percentage_point_difference
        ax.errorbar(
            estimate,
            yi,
            xerr=np.array(
                [[estimate - row.conf_low_pp], [row.conf_high_pp - estimate]]
            ),
            fmt="o",
            color=color,
            markerfacecolor=color,
            markeredgecolor=color,
            markersize=5.0,
            elinewidth=1.25,
            capsize=2.0,
            zorder=3,
        )
    ax.axvline(0, color="#777B80", linewidth=1.1)
    ax.set_yticks(y)
    ax.set_yticklabels([])
    ax.set_xlim(-26, 26)
    ax.set_xticks([-20, -10, 0, 10, 20])
    ax.set_xlabel("Expansion minus pre-expansion (pp)", fontsize=8.5)
    ax.tick_params(axis="x", labelsize=7.5)
    ax.tick_params(axis="y", length=0)
    ax.spines[["top", "right", "left"]].set_visible(False)
    ax.grid(axis="x", color="#E3E5E7", linewidth=0.55)
    for boundary in [3.5, 7.5]:
        ax.axhline(boundary, color="#D9DDE1", linewidth=0.8)


def main() -> None:
    data = prepare_data()
    plt.rcParams.update(
        {
            "font.family": "sans-serif",
            "font.sans-serif": ["Arial", "DejaVu Sans"],
            "axes.edgecolor": "#202124",
            "axes.labelcolor": "#202124",
            "xtick.color": "#202124",
            "ytick.color": "#202124",
            "figure.dpi": 300,
            "savefig.dpi": 300,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
        }
    )
    fig = plt.figure(figsize=(7.2, 6.35), constrained_layout=False)
    grid = fig.add_gridspec(
        2,
        2,
        height_ratios=[1.0, 3.5],
        width_ratios=[1.08, 1.0],
        left=0.165,
        right=0.985,
        top=0.975,
        bottom=0.085,
        wspace=0.20,
        hspace=0.30,
    )
    ax_a = fig.add_subplot(grid[0, :])
    ax_b = fig.add_subplot(grid[1, 0])
    ax_c = fig.add_subplot(grid[1, 1])
    draw_state_space(ax_a)
    draw_probability_panel(ax_b, data)
    draw_difference_panel(ax_c, data)
    fig.savefig(PNG_PATH, dpi=300, facecolor="white")
    fig.savefig(PDF_PATH, facecolor="white")
    print(PNG_PATH)
    print(PDF_PATH)
    print(SOURCE_PATH)


if __name__ == "__main__":
    main()
