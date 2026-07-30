from __future__ import annotations

from pathlib import Path
import os

import matplotlib.pyplot as plt
from matplotlib.patches import FancyArrowPatch, Rectangle
import pandas as pd


HERE = Path(__file__).resolve().parent
OUTPUT_DIR = Path(os.environ.get("V7_OUTPUT_DIR", HERE))
PNG = OUTPUT_DIR / "Figure1_three_survey_sample_flow_updated.png"
TIFF = OUTPUT_DIR / "Figure1_three_survey_sample_flow_updated.tiff"
SOURCE = OUTPUT_DIR / "Figure1_three_survey_sample_flow_updated_source_data.csv"

BLACK = "#111111"


def rect_box(
    ax: plt.Axes,
    x: float,
    y: float,
    w: float,
    h: float,
    text: str,
    *,
    fontsize: float = 12.2,
    weight: str = "normal",
) -> None:
    ax.add_patch(
        Rectangle(
            (x, y),
            w,
            h,
            facecolor="white",
            edgecolor=BLACK,
            linewidth=1.1,
        )
    )
    ax.text(
        x + w / 2,
        y + h / 2,
        text,
        ha="center",
        va="center",
        fontsize=fontsize,
        fontweight=weight,
        color=BLACK,
        linespacing=1.28,
    )


def down_arrow(
    ax: plt.Axes, x: float, y_top: float, y_bottom: float
) -> None:
    ax.add_patch(
        FancyArrowPatch(
            (x, y_top),
            (x, y_bottom),
            arrowstyle="-|>",
            mutation_scale=11,
            linewidth=1.05,
            color=BLACK,
        )
    )


def side_arrow(
    ax: plt.Axes, x_left: float, x_right: float, y: float
) -> None:
    ax.add_patch(
        FancyArrowPatch(
            (x_left, y),
            (x_right, y),
            arrowstyle="-|>",
            mutation_scale=10,
            linewidth=1.0,
            color=BLACK,
        )
    )


def title(ax: plt.Axes, x: float, text: str) -> None:
    ax.text(
        x,
        0.965,
        text,
        ha="center",
        va="top",
        fontsize=17.0,
        fontweight="bold",
        color=BLACK,
    )


def draw_cfps(ax: plt.Axes) -> None:
    x, w = 0.012, 0.205
    sx, sw = 0.225, 0.105
    ys = [0.815, 0.665, 0.515, 0.365, 0.215, 0.045]
    hs = [0.105, 0.105, 0.105, 0.105, 0.105, 0.125]
    title(ax, x + w / 2, "CFPS")
    texts = [
        "Older candidate cohort\n"
        "31,915 person-wave records\n8,333 participants",
        "Included: baseline age ≥65 years\n"
        "20,081 records, 5,428 participants",
        "Included: valid self-rated health\n"
        "at baseline in 2014\n5,426 participants",
        "Included: observed in 2018 follow-up\n3,556 participants",
        "Included: valid self-rated health\n"
        "in 2018\n3,509 participants",
        "Main 2014–2018 paired DID\n"
        "3,509 participants\n7,018 person-wave records\n"
        "Pilot: 954  •  Control: 2,555",
    ]
    for y, h, text in zip(ys, hs, texts):
        rect_box(ax, x, y, w, h, text, fontsize=11.35)
    for i in range(len(ys) - 1):
        down_arrow(ax, x + w / 2, ys[i], ys[i + 1] + hs[i + 1])
    exclusions = [
        (ys[1] + hs[1] / 2, "Excluded\n11,834 records\n2,905 participants"),
        (
            ys[2] + hs[2] / 2,
            "Excluded\n2 participants\nMissing baseline\noutcome",
        ),
        (
            ys[3] + hs[3] / 2,
            "Excluded\n1,870 participants\nNot observed\nin 2018",
        ),
        (
            ys[4] + hs[4] / 2,
            "Excluded\n47 participants\nMissing 2018\noutcome",
        ),
    ]
    for yy, text in exclusions:
        side_arrow(ax, x + w, sx, yy)
        rect_box(ax, sx, yy - 0.0475, sw, 0.095, text, fontsize=9.7)


def draw_charls(ax: plt.Axes) -> None:
    x, w = 0.345, 0.205
    sx, sw = 0.558, 0.105
    ys = [0.815, 0.665, 0.515, 0.365, 0.215, 0.045]
    hs = [0.105, 0.105, 0.105, 0.105, 0.105, 0.125]
    title(ax, x + w / 2, "CHARLS")
    texts = [
        "Four-wave frailty-index dataset\n"
        "77,233 wave records\n25,586 participants",
        "Included: age ≥65 years\nat observation\n"
        "23,500 records, 9,602 participants",
        "Included: ≥80% completion of the\n"
        "21-item frailty index\n21,164 records, 9,192 participants",
        "Included: at least two valid waves\n"
        "17,908 wave records\n5,936 participants",
        "Eligible adjacent-wave baselines\n"
        "10,182 transition records\n4,863 participants",
        "Four-state IPCW model\n"
        "8,746 resolved transitions\n698 deaths\n"
        "2,000 participant-cluster bootstraps",
    ]
    for y, h, text in zip(ys, hs, texts):
        rect_box(ax, x, y, w, h, text, fontsize=11.0)
    for i in range(len(ys) - 1):
        down_arrow(ax, x + w / 2, ys[i], ys[i + 1] + hs[i + 1])
    exclusions = [
        (ys[1] + hs[1] / 2, "Excluded\n53,733 records\n15,984 participants"),
        (ys[2] + hs[2] / 2, "Excluded\n2,336 records\n410 participants"),
        (
            ys[4] + hs[4] / 2,
            "Not in transition frame\n1,073 participants",
        ),
        (
            ys[5] + hs[5] * 0.72,
            "Unresolved follow-up\n1,436 records\nHandled with IPCW",
        ),
    ]
    for yy, text in exclusions:
        side_arrow(ax, x + w, sx, yy)
        rect_box(ax, sx, yy - 0.0475, sw, 0.095, text, fontsize=9.45)


def draw_class(ax: plt.Axes) -> None:
    x, w = 0.678, 0.205
    sx, sw = 0.891, 0.097
    ys = [0.815, 0.655, 0.495, 0.295, 0.075]
    hs = [0.115, 0.115, 0.115, 0.155, 0.14]
    title(ax, x + w / 2, "CLASS")
    texts = [
        "Four requested survey releases\n"
        "2016, 2018, 2020 and 2023\n45,958 records",
        "Included: age 65–110 years\n"
        "38,160 records",
        "Included: complete ADL-help outcome,\n"
        "age, residence, education and sex\n33,081 records",
        "Repeated cross-sectional model sample\n"
        "2016: 3,284  •  2018: 9,357\n"
        "2020: 9,987  •  2023: 10,453\n"
        "No cross-wave person linkage",
        "Standardized ADL-help probabilities\n"
        "Probability  •  Percentage points\nEvents per 1,000\n"
        "Secondary 2018 FI validation: n=9,135",
    ]
    for y, h, text in zip(ys, hs, texts):
        rect_box(ax, x, y, w, h, text, fontsize=10.65)
    for i in range(len(ys) - 1):
        down_arrow(ax, x + w / 2, ys[i], ys[i + 1] + hs[i + 1])
    exclusions = [
        (ys[1] + hs[1] / 2, "Excluded\n7,798 records\nOutside age range"),
        (ys[2] + hs[2] / 2, "Excluded\n5,079 records\nIncomplete variables"),
        (
            ys[4] + hs[4] * 0.76,
            "2018 FI validation\nExcluded: 222",
        ),
    ]
    for yy, text in exclusions:
        side_arrow(ax, x + w, sx, yy)
        rect_box(ax, sx, yy - 0.0475, sw, 0.095, text, fontsize=9.2)


def source_data() -> pd.DataFrame:
    rows = [
        ["CFPS", "Candidate cohort", 31915, "person-wave records"],
        ["CFPS", "Candidate cohort", 8333, "participants"],
        ["CFPS", "Age >=65", 20081, "records"],
        ["CFPS", "Age >=65", 5428, "participants"],
        ["CFPS", "Main paired DID", 3509, "participants"],
        ["CFPS", "Main paired DID", 7018, "person-wave records"],
        ["CHARLS", "Four-wave FI dataset", 77233, "wave records"],
        ["CHARLS", "Four-wave FI dataset", 25586, "participants"],
        ["CHARLS", "Eligible transitions", 10182, "transition records"],
        ["CHARLS", "Bootstrap sampling frame", 4863, "participants"],
        ["CHARLS", "Resolved outcomes", 8746, "transitions"],
        ["CHARLS", "Deaths within resolved outcomes", 698, "deaths"],
        ["CHARLS", "Unresolved follow-up", 1436, "transition records"],
        ["CLASS", "Four releases", 45958, "records"],
        ["CLASS", "Age 65-110", 38160, "records"],
        ["CLASS", "Complete model sample", 33081, "records"],
        ["CLASS", "2016 model sample", 3284, "records"],
        ["CLASS", "2018 model sample", 9357, "records"],
        ["CLASS", "2020 model sample", 9987, "records"],
        ["CLASS", "2023 model sample", 10453, "records"],
        ["CLASS", "Secondary 2018 FI validation", 9135, "participants"],
    ]
    return pd.DataFrame(rows, columns=["survey", "stage", "count", "unit"])


def main() -> None:
    plt.rcParams.update(
        {
            "font.family": "sans-serif",
            "font.sans-serif": ["Arial", "DejaVu Sans"],
            "figure.dpi": 300,
            "savefig.dpi": 300,
            "pdf.fonttype": 42,
            "ps.fonttype": 42,
        }
    )
    fig, ax = plt.subplots(figsize=(14, 8.6))
    ax.set_xlim(0, 1)
    ax.set_ylim(0, 1)
    ax.axis("off")
    draw_cfps(ax)
    draw_charls(ax)
    draw_class(ax)
    fig.subplots_adjust(left=0, right=1, top=1, bottom=0)
    fig.savefig(PNG, facecolor="white")
    fig.savefig(TIFF, facecolor="white", dpi=600)
    plt.close(fig)
    source_data().to_csv(SOURCE, index=False, encoding="utf-8-sig")
    print(PNG)
    print(TIFF)
    print(SOURCE)


if __name__ == "__main__":
    main()
