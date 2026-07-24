"""
01_clean_cfps.py
CFPS数据清洗：读取各波次、应用地理交叉对照、匹配试点城市
"""

import pyreadstat
import pandas as pd
import numpy as np
import os
import json

# Paths
PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CFPS_ROOT = "E:/公共数据库/中国数据库/CFPS"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "04_clean_data")
LOG_DIR = os.path.join(PROJECT_ROOT, "10_logs")

os.makedirs(OUTPUT_DIR, exist_ok=True)
os.makedirs(LOG_DIR, exist_ok=True)

print("=" * 60)
print("CFPS Data Cleaning Pipeline")
print("=" * 60)

# ============================================================
# Step 1: Build crosswalk and treat county set
# ============================================================
print("\n[1] Building crosswalk and treat county set...")

xwalk_path = os.path.join(CFPS_ROOT, "1⭐cfps顺序码匹配", "顺序码匹配.dta")
xwalk, _ = pyreadstat.read_dta(xwalk_path)
xwalk['code_str'] = xwalk['code'].apply(lambda x: f'{int(x):06d}')
xwalk['countyid_int'] = xwalk['countyid'].astype(int)
xwalk['prov_code'] = xwalk['code_str'].str[:2].astype(int)
xwalk['city_code'] = (xwalk['code_str'].str[:4] + '00').astype(int)

# Create mapping dict: countyid -> code
county_to_code = dict(zip(xwalk['countyid_int'], xwalk['code'].astype(int)))
county_to_prov = dict(zip(xwalk['countyid_int'], xwalk['prov_code']))
county_to_city = dict(zip(xwalk['countyid_int'], xwalk['city_code']))

print(f"  Crosswalk: {len(xwalk)} counties, {xwalk['prov_code'].nunique()} provinces")

# Read policy table
policy = pd.read_csv(os.path.join(PROJECT_ROOT, "02_policy_mapping", "policy_unit_table.csv"))

# Build treat county set
treat_counties = set()
treat_map = {}
batch_map = {}

for _, row in policy.iterrows():
    pcode = int(row['county_code'])
    plevel = row['policy_level']
    code_str = f'{pcode:06d}'

    if plevel in ['municipal_district', 'county', 'county_level_city']:
        treat_counties.add(pcode)
        treat_map[pcode] = row['policy_unit_id']
        batch_map[pcode] = row['batch']
    elif plevel in ['prefecture_city', 'sub_provincial_city', 'autonomous_prefecture']:
        prefix = code_str[:4]
        matching = xwalk[xwalk['code_str'].str[:4] == prefix]
        for c in matching['code'].astype(int):
            treat_counties.add(c)
            treat_map[c] = row['policy_unit_id']
            batch_map[c] = row['batch']

print(f"  Treat counties: {len(treat_counties)}")

# ============================================================
# Step 2: Read and clean each CFPS wave
# ============================================================
waves_config = [
    (2010, os.path.join(CFPS_ROOT, "cfps数据及清洗代码", "10", "cfps2010adult_201906.dta"), "countyid", None, None),
    (2012, os.path.join(CFPS_ROOT, "cfps数据及清洗代码", "12", "cfps2012adult_201906.dta"), "countyid", None, None),
    (2014, os.path.join(CFPS_ROOT, "cfps数据及清洗代码", "14", "cfps2014adult_201906.dta"), "countyid14", "provcd14", "fid14"),
    (2018, os.path.join(CFPS_ROOT, "cfps数据及清洗代码", "18", "cfps2018person_202012.dta"), "countyid18", "provcd18", "fid18"),
    (2020, os.path.join(CFPS_ROOT, "cfps数据及清洗代码", "20", "cfps2020person_202306.dta"), "countyid20", "provcd20", "fid20"),
]

all_waves = {}

for wave, path, county_var, prov_var, fid_var in waves_config:
    print(f"\n[2] Reading CFPS {wave}...")

    df, meta = pyreadstat.read_dta(path)
    n_raw = len(df)
    print(f"  Raw rows: {n_raw}")

    # Apply crosswalk
    df['countyid_int'] = df[county_var].astype(float).astype('Int64')
    df['admin_code'] = df['countyid_int'].map(county_to_code)
    df['prov_code_calc'] = df['countyid_int'].map(county_to_prov)
    df['city_code_calc'] = df['countyid_int'].map(county_to_city)

    # Keep only matched counties (162 original CFPS counties)
    n_matched = df['admin_code'].notna().sum()
    print(f"  Matched to crosswalk: {n_matched} ({100*n_matched/n_raw:.1f}%)")

    df_matched = df[df['admin_code'].notna()].copy()
    print(f"  After crosswalk filter: {len(df_matched)}")

    # Assign treat status
    df_matched['treat_area'] = df_matched['admin_code'].isin(treat_counties).astype(int)
    df_matched['policy_unit_id'] = df_matched['admin_code'].map(treat_map)
    df_matched['treat_batch'] = df_matched['admin_code'].map(batch_map)

    n_treat = df_matched['treat_area'].sum()
    n_control = len(df_matched) - n_treat
    print(f"  Treat: {n_treat}, Control: {n_control}")

    # Add wave indicator
    df_matched['wave'] = wave

    # Standardize key variables
    # Age
    if wave == 2010:
        df_matched['age'] = df_matched.get('ba004_w2_3', pd.Series(dtype=float))
        df_matched['gender'] = df_matched.get('rgender', df_matched.get('gender', pd.Series(dtype=float)))
    elif wave == 2012:
        df_matched['age'] = df_matched.get('ba002_1', df_matched.get('age', pd.Series(dtype=float)))
        df_matched['gender'] = df_matched.get('rgender', df_matched.get('gender', pd.Series(dtype=float)))
    elif wave == 2014:
        df_matched['age'] = df_matched.get('ba002_1', df_matched.get('age', pd.Series(dtype=float)))
        df_matched['gender'] = df_matched.get('rgender', df_matched.get('gender', pd.Series(dtype=float)))
    elif wave == 2018:
        df_matched['age'] = df_matched.get('ba002_1', df_matched.get('age', pd.Series(dtype=float)))
        df_matched['gender'] = df_matched.get('rgender', df_matched.get('gender', pd.Series(dtype=float)))
    elif wave == 2020:
        df_matched['age'] = df_matched.get('ba002_1', df_matched.get('age', pd.Series(dtype=float)))
        df_matched['gender'] = df_matched.get('rgender', df_matched.get('gender', pd.Series(dtype=float)))

    # Urban/rural
    urban_var = [c for c in df_matched.columns if 'urban' in c.lower() and c != 'urban_base']
    if urban_var:
        df_matched['urban'] = df_matched[urban_var[0]]

    all_waves[wave] = df_matched
    print(f"  Final: {len(df_matched)} rows")

# ============================================================
# Step 3: Output summary statistics
# ============================================================
print("\n[3] Summary statistics...")

summary_rows = []
for wave, df in all_waves.items():
    n_total = len(df)
    n_treat = df['treat_area'].sum()
    n_control = n_total - n_treat
    n_counties = df['admin_code'].nunique()
    n_treat_counties = df[df['treat_area'] == 1]['admin_code'].nunique()
    n_60plus = (df['age'] >= 60).sum() if 'age' in df.columns else np.nan

    summary_rows.append({
        'wave': wave,
        'n_total': n_total,
        'n_treat': int(n_treat),
        'n_control': int(n_control),
        'n_counties': n_counties,
        'n_treat_counties': n_treat_counties,
        'n_60plus': int(n_60plus) if not np.isnan(n_60plus) else None,
    })

summary = pd.DataFrame(summary_rows)
print(summary.to_string(index=False))
summary.to_csv(os.path.join(OUTPUT_DIR, "cfps_wave_summary.csv"), index=False)

# ============================================================
# Step 4: Identify panel individuals (appear in pre and post)
# ============================================================
print("\n[4] Building panel sample...")

pre_waves = {2010, 2012, 2014}
post_waves = {2018, 2020}

# Find individuals in both pre and post
pre_pids = set()
post_pids = set()
for wave, df in all_waves.items():
    if wave in pre_waves:
        pre_pids.update(df['pid'].dropna().unique())
    elif wave in post_waves:
        post_pids.update(df['pid'].dropna().unique())

panel_pids = pre_pids & post_pids
print(f"  Pre-wave individuals: {len(pre_pids)}")
print(f"  Post-wave individuals: {len(post_pids)}")
print(f"  Panel individuals (both): {len(panel_pids)}")

# ============================================================
# Step 5: Save cleaned data
# ============================================================
print("\n[5] Saving cleaned data...")

# Save each wave separately
for wave, df in all_waves.items():
    # Select key columns
    key_cols = ['pid', 'wave', 'admin_code', 'prov_code_calc', 'city_code_calc',
                'treat_area', 'policy_unit_id', 'treat_batch', 'age', 'gender', 'urban']
    existing_cols = [c for c in key_cols if c in df.columns]
    df_out = df[existing_cols].copy()
    df_out.to_parquet(os.path.join(OUTPUT_DIR, f"cfps_{wave}_clean.parquet"), index=False)
    print(f"  Saved cfps_{wave}_clean.parquet: {len(df_out)} rows")

# Save panel sample
panel_dfs = []
for wave, df in all_waves.items():
    df_panel = df[df['pid'].isin(panel_pids)].copy()
    panel_dfs.append(df_panel)

panel_all = pd.concat(panel_dfs, ignore_index=True)
panel_all.to_parquet(os.path.join(OUTPUT_DIR, "cfps_panel_clean.parquet"), index=False)
print(f"  Saved cfps_panel_clean.parquet: {len(panel_all)} rows, {panel_all['pid'].nunique()} individuals")

# ============================================================
# Step 6: Output treated/control counts by wave
# ============================================================
print("\n[6] Treated/control counts by wave and county...")

county_counts = []
for wave, df in all_waves.items():
    for treat_val in [0, 1]:
        subset = df[df['treat_area'] == treat_val]
        n = len(subset)
        n_counties = subset['admin_code'].nunique()
        label = "Treat" if treat_val == 1 else "Control"
        county_counts.append({
            'wave': wave,
            'group': label,
            'n_individuals': n,
            'n_counties': n_counties,
        })

county_df = pd.DataFrame(county_counts)
print(county_df.to_string(index=False))
county_df.to_csv(os.path.join(OUTPUT_DIR, "cfps_treat_control_counts.csv"), index=False)

# ============================================================
# Step 7: Save treat county mapping
# ============================================================
treat_info = []
for code in sorted(treat_counties):
    pid = treat_map.get(code, None)
    batch = batch_map.get(code, None)
    policy_row = policy[policy['policy_unit_id'] == pid]
    pname = policy_row['policy_unit_name_standard'].values[0] if len(policy_row) > 0 else None
    prov = policy_row['province_name'].values[0] if len(policy_row) > 0 else None
    treat_info.append({
        'admin_code': code,
        'policy_unit_id': pid,
        'policy_unit_name': pname,
        'province': prov,
        'batch': batch,
    })

treat_df = pd.DataFrame(treat_info)
treat_df.to_csv(os.path.join(OUTPUT_DIR, "cfps_treat_county_mapping.csv"), index=False)
print(f"\n  Treat county mapping saved: {len(treat_df)} counties")

print("\n" + "=" * 60)
print("CFPS cleaning completed.")
print("=" * 60)
