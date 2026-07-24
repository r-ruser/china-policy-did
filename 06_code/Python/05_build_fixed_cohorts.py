"""
05_build_fixed_cohorts.py
构建固定队列：老年健康队列和劳动供给队列
"""

import pyreadstat
import pandas as pd
import numpy as np
import os

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CFPS_ROOT = "E:/公共数据库/中国数据库/CFPS"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "05_analysis_data")
os.makedirs(OUTPUT_DIR, exist_ok=True)

print("=" * 60)
print("Building Fixed Cohorts")
print("=" * 60)

# ============================================================
# 1. Load crosswalk and build treat county set
# ============================================================
print("\n[1] Loading crosswalk...")

xwalk, _ = pyreadstat.read_dta(os.path.join(CFPS_ROOT, "1⭐cfps顺序码匹配", "顺序码匹配.dta"))
xwalk['code_str'] = xwalk['code'].apply(lambda x: f'{int(x):06d}')
county_to_code = dict(zip(xwalk['countyid'].astype(int), xwalk['code'].astype(int)))

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

# Also build policy_assignment_cluster (city-level clustering)
# For prefecture-level cities: all counties in the same prefecture share a cluster
# For direct-controlled municipalities: each district is its own cluster
cluster_map = {}
for _, row in policy.iterrows():
    pcode = int(row['county_code'])
    plevel = row['policy_level']
    code_str = f'{pcode:06d}'
    if plevel in ['municipal_district', 'county', 'county_level_city']:
        cluster_map[pcode] = pcode  # individual policy unit as cluster
    elif plevel in ['prefecture_city', 'sub_provincial_city', 'autonomous_prefecture']:
        prefix = code_str[:4]
        matching = xwalk[xwalk['code_str'].str[:4] == prefix]
        for c in matching['code'].astype(int):
            cluster_map[int(c)] = pcode  # prefecture code as cluster

# ============================================================
# 2. Load all CFPS waves
# ============================================================
print("\n[2] Loading CFPS waves...")

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
    if wave in [2010, 2012]:
        county_var = 'countyid'
    elif wave == 2014:
        county_var = 'countyid14'
    elif wave == 2018:
        if 'countyid18' not in df.columns:
            merged_path = os.path.join(CFPS_ROOT, 'cfps数据及清洗代码', '18', '18个人_with_countyid.dta')
            if os.path.exists(merged_path):
                df, _ = pyreadstat.read_dta(merged_path)
        county_var = 'countyid18'
    elif wave == 2020:
        county_var = 'countyid20'

    # Apply crosswalk
    df['countyid_int'] = df[county_var].astype(float).astype('Int64')
    df['admin_code'] = df['countyid_int'].map(county_to_code)

    # Keep only matched counties
    df = df[df['admin_code'].notna()].copy()

    # Assign treat status
    df['treat'] = df['admin_code'].isin(treat_counties).astype(int)
    df['policy_unit_id'] = df['admin_code'].map(treat_map)
    df['cluster'] = df['admin_code'].map(cluster_map)

    # Wave indicator
    df['wave'] = wave

    # Post indicator (2018+ = 1, 2014- = 0, 2016 deleted)
    df['post'] = (df['wave'] >= 2018).astype(int)

    # DID interaction
    df['did'] = df['treat'] * df['post']

    # Age variable
    age_var = 'age_' if 'age_' in df.columns else 'age'
    df['age_num'] = pd.to_numeric(df[age_var], errors='coerce')

    # Gender (1=Male, 2=Female -> 0/1)
    df['female'] = (df['gen'] == 2).astype(int)

    # SRH: higher = better (1=very poor, 5=very good)
    df['srh'] = pd.to_numeric(df['health'], errors='coerce')
    df['poor_srh'] = (df['srh'] <= 2).astype(int)  # 1=poor/very poor

    # Activity limitation: recode 79 as missing
    df['dw_clean'] = pd.to_numeric(df['dw'], errors='coerce')
    df.loc[df['dw_clean'] == 79, 'dw_clean'] = np.nan
    df['activity_limitation'] = (df['dw_clean'] <= 2).astype(int)  # 1=limited

    # Employment
    df['employed'] = pd.to_numeric(df['job'], errors='coerce')

    # Family ID
    fid_var = f'fid{wave}' if f'fid{wave}' in df.columns else [c for c in df.columns if c.startswith('fid')][0]
    df['fid'] = df[fid_var]

    wave_data[wave] = df
    print(f"  {wave}: {len(df)} rows, treat={df['treat'].sum()}, control={len(df)-df['treat'].sum()}")

# ============================================================
# 3. Build 2014 baseline for fixed cohort definition
# ============================================================
print("\n[3] Building 2014 baseline...")

df14 = wave_data[2014].copy()

# 2014 baseline age
df14['baseline_age'] = df14['age_num']
df14['baseline_srh'] = df14['srh']
df14['baseline_poor_srh'] = df14['poor_srh']
df14['baseline_dw'] = df14['dw_clean']
df14['baseline_female'] = df14['female']

# Family-level: does family have 60+ member in 2014?
family_60plus_2014 = df14[df14['baseline_age'] >= 60].groupby('fid').size()
df14['older_household_2014'] = df14['fid'].map(family_60plus_2014).fillna(0).astype(int)
df14['older_household_2014'] = (df14['older_household_2014'] > 0).astype(int)

# Working age in 2014
df14['working_age_2014'] = ((df14['baseline_age'] >= 18) & (df14['baseline_age'] <= 59)).astype(int)

# Save 2014 baseline
baseline_cols = ['pid', 'fid', 'admin_code', 'treat', 'cluster',
                 'baseline_age', 'baseline_srh', 'baseline_poor_srh', 'baseline_dw',
                 'baseline_female', 'older_household_2014', 'working_age_2014']
df14_base = df14[baseline_cols].copy()
df14_base.to_parquet(os.path.join(OUTPUT_DIR, 'cfps_2014_baseline.parquet'), index=False)
print(f"  2014 baseline: {len(df14_base)} individuals")

# ============================================================
# 4. Build Elderly Health Cohort (baseline 2014 age >= 60)
# ============================================================
print("\n[4] Building Elderly Health Cohort...")

elderly_pids = set(df14[df14['baseline_age'] >= 60]['pid'])
print(f"  Elderly cohort size (2014 age >= 60): {len(elderly_pids)}")

elderly_dfs = []
for wave, df in wave_data.items():
    df_e = df[df['pid'].isin(elderly_pids)].copy()
    # Merge 2014 baseline
    df_e = df_e.merge(df14_base[['pid', 'baseline_age', 'baseline_srh', 'baseline_poor_srh',
                                   'baseline_dw', 'baseline_female', 'older_household_2014']],
                       on='pid', how='left', suffixes=('', '_bl'))
    elderly_dfs.append(df_e)

elderly = pd.concat(elderly_dfs, ignore_index=True)
elderly = elderly.sort_values(['pid', 'wave']).reset_index(drop=True)

print(f"  Elderly cohort panel: {len(elderly)} rows, {elderly['pid'].nunique()} individuals")
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = elderly[elderly['wave'] == wave]
    print(f"    {wave}: {len(sub)} obs, treat={sub['treat'].sum()}, "
          f"poor_srh={sub['poor_srh'].mean():.3f}")

elderly.to_parquet(os.path.join(OUTPUT_DIR, 'cfps_elderly_cohort.parquet'), index=False)

# ============================================================
# 5. Build Labor Supply Cohort (18-59, older household 2014)
# ============================================================
print("\n[5] Building Labor Supply Cohort...")

# Working age individuals in families that had 60+ member in 2014
labor_pids = set(df14[(df14['working_age_2014'] == 1) & (df14['older_household_2014'] == 1)]['pid'])
print(f"  Labor cohort size (18-59, older household 2014): {len(labor_pids)}")

labor_dfs = []
for wave, df in wave_data.items():
    df_l = df[df['pid'].isin(labor_pids)].copy()
    df_l = df_l.merge(df14_base[['pid', 'baseline_age', 'baseline_female', 'older_household_2014']],
                       on='pid', how='left', suffixes=('', '_bl'))
    labor_dfs.append(df_l)

labor = pd.concat(labor_dfs, ignore_index=True)
labor = labor.sort_values(['pid', 'wave']).reset_index(drop=True)

print(f"  Labor cohort panel: {len(labor)} rows, {labor['pid'].nunique()} individuals")
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = labor[labor['wave'] == wave]
    print(f"    {wave}: {len(sub)} obs, treat={sub['treat'].sum()}, "
          f"employed={sub['employed'].mean():.3f}")

labor.to_parquet(os.path.join(OUTPUT_DIR, 'cfps_labor_cohort.parquet'), index=False)

# ============================================================
# 6. Summary
# ============================================================
print("\n[6] Summary...")
print(f"\nElderly Health Cohort:")
print(f"  2014 baseline age >= 60: {len(elderly_pids)}")
print(f"  Panel rows: {len(elderly)}")
print(f"  Treat counties: {elderly[elderly['treat']==1]['admin_code'].nunique()}")

print(f"\nLabor Supply Cohort:")
print(f"  18-59, older household 2014: {len(labor_pids)}")
print(f"  Panel rows: {len(labor)}")
print(f"  Treat counties: {labor[labor['treat']==1]['admin_code'].nunique()}")

print("\n" + "=" * 60)
print("Fixed cohorts built successfully.")
print("=" * 60)
