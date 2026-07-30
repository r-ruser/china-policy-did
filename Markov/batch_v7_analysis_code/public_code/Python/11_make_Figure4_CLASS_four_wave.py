from pathlib import Path
import os

import matplotlib.pyplot as plt
import pandas as pd


HERE = Path(__file__).resolve().parent
OUTPUT_DIR = Path(os.environ.get("V7_OUTPUT_DIR", HERE))
SOURCE = OUTPUT_DIR / "04_CLASS_four_wave_standardized_ADL_probabilities.csv"
OUT_PNG = OUTPUT_DIR / "Figure4_CLASS_four_wave_ADL_help.png"
OUT_PDF = OUTPUT_DIR / "Figure4_CLASS_four_wave_ADL_help.pdf"
OUT_DATA = OUTPUT_DIR / "Figure4_CLASS_four_wave_ADL_help_source_data.csv"
SUPP_PNG = OUTPUT_DIR / "FigureS2_CHARLS_ADL_validation.png"
SUPP_PDF = OUTPUT_DIR / "FigureS2_CHARLS_ADL_validation.pdf"
CHARLS_VALIDATION = os.environ.get("CHARLS_ADL_VALIDATION_CSV")

COLORS = {
    "Overall": "#263746",
    "65–74 years": "#138A7E",
    "≥75 years": "#C95454",
    "Urban/town": "#138A7E",
    "Rural": "#C28A27",
}


def panel(ax, data, groups, title, letter):
    for source_level, display_label in groups:
        z = data.loc[data["level"].eq(source_level)].sort_values("time")
        x = z["time"].astype(int)
        y = z["events_per_1000"]
        low = y - z["conf_low_per_1000"]
        high = z["conf_high_per_1000"] - y
        ax.errorbar(
            x,
            y,
            yerr=[low, high],
            color=COLORS[display_label],
            marker="o",
            markersize=5.2,
            markeredgewidth=0.8,
            markeredgecolor="white",
            linewidth=1.8,
            elinewidth=1.1,
            capsize=3,
            label=display_label,
            zorder=3,
        )
    ax.set_title(title, loc="left", fontsize=10.5, fontweight="bold", pad=9)
    ax.text(
        -0.12, 1.07, letter, transform=ax.transAxes,
        fontsize=12, fontweight="bold", va="top"
    )
    ax.set_xticks([2016, 2018, 2020, 2023])
    ax.set_xlim(2015.4, 2023.6)
    ax.set_ylim(0, 190)
    ax.set_yticks([0, 50, 100, 150])
    ax.grid(axis="y", color="#D9DEE2", linewidth=0.7)
    ax.spines[["top", "right"]].set_visible(False)
    ax.spines[["left", "bottom"]].set_color("#6F7981")
    ax.tick_params(labelsize=8.5, colors="#37434C")
    if len(groups) > 1:
        ax.legend(
            frameon=False, fontsize=8.2, loc="upper right",
            handlelength=1.7, borderaxespad=0.2
        )


def make_main():
    data = pd.read_csv(SOURCE)
    selected = data[
        data["dimension"].isin(["Overall", "Age", "Residence"])
    ].copy()
    selected.to_csv(OUT_DATA, index=False)

    plt.rcParams.update({
        "font.family": "Arial",
        "axes.titlecolor": "#111111",
        "axes.labelcolor": "#263746",
        "pdf.fonttype": 42,
        "ps.fonttype": 42,
    })
    fig, axes = plt.subplots(1, 3, figsize=(10.8, 3.55), sharey=True)
    panel(
        axes[0],
        selected[selected["dimension"].eq("Overall")],
        [("Overall", "Overall")],
        "Overall",
        "a",
    )
    panel(
        axes[1],
        selected[selected["dimension"].eq("Age")],
        [("65-74", "65–74 years"), ("75+", "≥75 years")],
        "Age group",
        "b",
    )
    panel(
        axes[2],
        selected[selected["dimension"].eq("Residence")],
        [("Urban/town", "Urban/town"), ("Rural", "Rural")],
        "Residence",
        "c",
    )
    axes[0].set_ylabel("ADL-help requirement per 1,000 adults", fontsize=9.5)
    for ax in axes:
        ax.set_xlabel("CLASS wave", fontsize=9.2, labelpad=6)
    fig.subplots_adjust(left=0.075, right=0.985, top=0.88, bottom=0.20, wspace=0.16)
    fig.savefig(OUT_PNG, dpi=400, facecolor="white")
    fig.savefig(OUT_PDF, facecolor="white")
    plt.close(fig)


def make_charls_supplement():
    if not CHARLS_VALIDATION:
        return
    data = pd.read_csv(Path(CHARLS_VALIDATION))
    colors = ["#138A7E", "#C28A27", "#C95454"]
    fig, ax = plt.subplots(figsize=(5.8, 3.45))
    x = range(3)
    y = data["risk_pct"]
    low = y - data["ci_low_pct"]
    high = data["ci_high_pct"] - y
    ax.errorbar(
        x, y, yerr=[low, high], fmt="none",
        ecolor="#6F7981", elinewidth=1.4, capsize=4, zorder=2
    )
    ax.scatter(
        x, y, s=58, c=colors, edgecolors="white",
        linewidths=0.8, zorder=3
    )
    for i, value in enumerate(y):
        ax.text(
            i, data.loc[i, "label_y"], f"{value:.1f}",
            ha="center", va="bottom", fontsize=9, color="#263746"
        )
    ax.set_xticks(list(x), data["state_label"])
    ax.set_ylabel("ADL-help prevalence (%)")
    ax.set_ylim(0, 32)
    ax.set_title(
        "CHARLS ADL-help prevalence by deficit state",
        loc="left", fontsize=10.5, fontweight="bold", pad=9
    )
    ax.grid(axis="y", color="#D9DEE2", linewidth=0.7)
    ax.spines[["top", "right"]].set_visible(False)
    ax.spines[["left", "bottom"]].set_color("#6F7981")
    ax.tick_params(labelsize=8.8, colors="#37434C")
    fig.subplots_adjust(left=0.14, right=0.98, top=0.86, bottom=0.18)
    fig.savefig(SUPP_PNG, dpi=400, facecolor="white")
    fig.savefig(SUPP_PDF, facecolor="white")
    plt.close(fig)


if __name__ == "__main__":
    make_main()
    make_charls_supplement()
    print(OUT_PNG)
    print(SUPP_PNG)
