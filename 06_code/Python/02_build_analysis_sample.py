"""
02_build_analysis_sample.py
使用CFPS清洗文件构建分析样本：地理匹配、处理组分配、变量审计
"""

import pyreadstat
import pandas as pd
import numpy as np
import os

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CFPS_ROOT = "E:/公共数据库/中国数据库/CFPS"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "04_clean_data")
ANALYSIS_DIR = os.path.join(PROJECT_ROOT, "05_analysis_data")
os.makedirs(ANALYSIS_DIR, exist_ok=True)

print("=" * 60)
print("CFPS Analysis Sample Builder")
print("=" * 60)

# ============================================================
# 1. Load crosswalk and build treat county set
# ============================================================
print("\n[1] Loading crosswalk...")

xwalk_path = os.path.join(CFPS_ROOT, "1⭐cfps顺序码匹配", "顺序码匹配.dta")
xwalk, _ = pyreadstat.read_dta(xwalk_path)
xwalk['code_str'] = xwalk['code'].apply(lambda x: f'{int(x):06d}')
county_to_code = dict(zip(xwalk['countyid'].astype(int), xwalk['code'].astype(int)))
county_to_prov = dict(zip(xwalk['countyid'].astype(int), xwalk['code_str'].str[:2].astype(int)))
county_to_city = dict(zip(xwalk['countyid'].astype(int), (xwalk['code_str'].str[:4] + '00').astype(int)))

policy = pd.read_csv(os.path.join(PROJECT_ROOT, "02_policy_mapping", "policy_unit_table.csv"))

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
# 2. Load cleaned person files and apply crosswalk
# ============================================================
print("\n[2] Loading cleaned person files...")

cleaned_files = {
    2010: '中国数据库/CFPS/cfps数据及清洗代码/10/10个人.dta',
    2012: '中国数据库/CFPS/cfps数据及清洗代码/12/12个人.dta',
    2014: '中国数据库/CFPS/cfps数据及清洗代码/14/14个人.dta',
    2018: '中国数据库/CFPS/cfps数据及清洗代码/18/18个人.dta',
    2020: '中国数据库/CFPS/cfps数据及清洗代码/20/20个人.dta',
}

wave_data = {}
for wave, path in cleaned_files.items():
    df, _ = pyreadstat.read_dta(path)

    # Determine county ID variable
    if wave in [2010]:
        county_var = 'countyid'
    elif wave in [2012]:
        county_var = 'countyid'
    elif wave in [2014]:
        county_var = 'countyid14'
    elif wave in [2018]:
        # 2018 cleaned file lacks countyid18; use merged version
        if 'countyid18' not in df.columns:
            merged_path = os.path.join(CFPS_ROOT, 'cfps数据及清洗代码', '18', '18个人_with_countyid.dta')
            if os.path.exists(merged_path):
                df, _ = pyreadstat.read_dta(merged_path)
        county_var = 'countyid18'
    elif wave in [2020]:
        county_var = 'countyid20'

    # Apply crosswalk
    df['countyid_int'] = df[county_var].astype(float).astype('Int64')
    df['admin_code'] = df['countyid_int'].map(county_to_code)
    df['prov_code'] = df['countyid_int'].map(county_to_prov)
    df['city_code'] = df['countyid_int'].map(county_to_city)

    # Keep only crosswalk-matched counties
    df_matched = df[df['admin_code'].notna()].copy()

    # Assign treat status
    df_matched['treat_area'] = df_matched['admin_code'].isin(treat_counties).astype(int)
    df_matched['policy_unit_id'] = df_matched['admin_code'].map(treat_map)
    df_matched['treat_batch'] = df_matched['admin_code'].map(batch_map)

    # Add wave
    df_matched['wave'] = wave

    # Standardize age variable
    if 'age_' in df_matched.columns:
        df_matched['age'] = df_matched['age_']
    elif 'age' in df_matched.columns:
        pass  # already named 'age'

    # Standardize urban variable
    urban_candidates = [c for c in df_matched.columns if 'urban' in c.lower()]
    if urban_candidates:
        df_matched['urban_std'] = df_matched[urban_candidates[0]]

    wave_data[wave] = df_matched
    n_treat = df_matched['treat_area'].sum()
    print(f"  {wave}: {len(df_matched)} rows, treat={int(n_treat)}, control={len(df_matched)-int(n_treat)}")

# ============================================================
# 3. Audit outcome variables across waves
# ============================================================
print("\n[3] Auditing outcome variables...")

outcomes = {
    'health': 'Self-rated health',
    'dw': 'Disability (ADL/IADL)',
    'unhealth': 'Unhealthy status',
    'weak': 'Weakness/frailty',
    'job': 'Employment',
    'wage': 'Wage income',
    'medsure_dum': 'Medical insurance',
    'mar': 'Marital status',
    'gen': 'Gender',
}

print("\n  Variable availability and non-missing counts:")
print(f"  {'Variable':<20} {'2010':>8} {'2012':>8} {'2014':>8} {'2018':>8} {'2020':>8}")
print("  " + "-" * 60)

for var, desc in outcomes.items():
    counts = []
    for wave in [2010, 2012, 2014, 2018, 2020]:
        df = wave_data[wave]
        if var in df.columns:
            n = df[var].notna().sum()
            counts.append(f"{n:>8}")
        else:
            counts.append(f"{'N/A':>8}")
    print(f"  {var:<20} {''.join(counts)}")

# ============================================================
# 4. Check cross-wave comparability of key outcomes
# ============================================================
print("\n[4] Cross-wave comparability...")

for var in ['health', 'dw', 'job']:
    print(f"\n  {var}:")
    for wave in [2010, 2012, 2014, 2018, 2020]:
        df = wave_data[wave]
        if var in df.columns:
            vals = df[var].dropna()
            print(f"    {wave}: n={len(vals)}, unique={vals.nunique()}, "
                  f"mean={vals.mean():.3f}, sample={sorted(vals.unique())[:8]}")

# ============================================================
# 5. Build panel sample
# ============================================================
print("\n[5] Building panel sample...")

pre_waves = {2010, 2012, 2014}
post_waves = {2018, 2020}

pre_pids = set()
post_pids = set()
for wave, df in wave_data.items():
    if wave in pre_waves:
        pre_pids.update(df['pid'].dropna().unique())
    elif wave in post_waves:
        post_pids.update(df['pid'].dropna().unique())

panel_pids = pre_pids & post_pids
print(f"  Pre-wave individuals: {len(pre_pids)}")
print(f"  Post-wave individuals: {len(post_pids)}")
print(f"  Panel individuals: {len(panel_pids)}")

# Build panel dataset with key variables
key_vars = ['pid', 'wave', 'admin_code', 'prov_code', 'city_code',
            'treat_area', 'policy_unit_id', 'treat_batch',
            'age', 'gen', 'health', 'dw', 'unhealth', 'weak',
            'job', 'wage', 'medsure_dum', 'mar', 'urban_std', 'old', 'size']

panel_dfs = []
for wave, df in wave_data.items():
    existing = [v for v in key_vars if v in df.columns]
    df_sub = df[df['pid'].isin(panel_pids)][existing].copy()
    panel_dfs.append(df_sub)

panel = pd.concat(panel_dfs, ignore_index=True)

# Sort for panel structure
panel = panel.sort_values(['pid', 'wave']).reset_index(drop=True)

print(f"  Panel: {len(panel)} rows, {panel['pid'].nunique()} individuals")

# ============================================================
# 6. Sample statistics
# ============================================================
print("\n[6] Sample statistics...")

# By wave and treat
print("\n  N by wave and treat:")
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = panel[panel['wave'] == wave]
    n_treat = sub['treat_area'].sum()
    n_control = len(sub) - n_treat
    print(f"    {wave}: treat={int(n_treat)}, control={int(n_control)}, total={len(sub)}")

# Age distribution
print("\n  Age distribution (60+):")
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = panel[panel['wave'] == wave]
    n_60 = (sub['age'] >= 60).sum()
    n_total = len(sub)
    print(f"    {wave}: {n_60}/{n_total} ({100*n_60/n_total:.1f}%)")

# ============================================================
# 7. Save analysis samples
# ============================================================
print("\n[7] Saving analysis samples...")

# Save full panel
panel.to_parquet(os.path.join(ANALYSIS_DIR, "cfps_panel_analysis.parquet"), index=False)

# Save elderly sample (60+)
elderly = panel[panel['age'] >= 60].copy()
elderly.to_parquet(os.path.join(ANALYSIS_DIR, "cfps_elderly_analysis.parquet"), index=False)
print(f"  Elderly (60+): {len(elderly)} rows, {elderly['pid'].nunique()} individuals")

# Save treat county mapping
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
treat_df.to_csv(os.path.join(ANALYSIS_DIR, "cfps_treat_county_mapping.csv"), index=False)

# Save summary
summary_rows = []
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = panel[panel['wave'] == wave]
    sub_e = elderly[elderly['wave'] == wave] if wave in elderly['wave'].values else pd.DataFrame()
    summary_rows.append({
        'wave': wave,
        'n_total': len(sub),
        'n_treat': int(sub['treat_area'].sum()),
        'n_control': len(sub) - int(sub['treat_area'].sum()),
        'n_counties': sub['admin_code'].nunique(),
        'n_treat_counties': sub[sub['treat_area']==1]['admin_code'].nunique(),
        'n_60plus': len(sub_e),
    })
summary = pd.DataFrame(summary_rows)
summary.to_csv(os.path.join(ANALYSIS_DIR, "cfps_sample_summary.csv"), index=False)
print("\n  Sample summary:")
print(summary.to_string(index=False))

print("\n" + "=" * 60)
print("Analysis sample builder completed.")
print("=" * 60)
