"""
15_fix_cfps_labor_ddd.py
Fix CFPS labor supply DDD: use full 18-59 sample, not just older households
"""
import pyreadstat
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CFPS_ROOT = "E:/公共数据库/中国数据库/CFPS"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "07_results")

print("=" * 70)
print("CFPS LABOR SUPPLY DDD - FIXED")
print("=" * 70)

# ============================================================
# 1. Load crosswalk and treat counties
# ============================================================
print("\n[1] Loading crosswalk...")
xwalk, _ = pyreadstat.read_dta(os.path.join(CFPS_ROOT, "1⭐cfps顺序码匹配", "顺序码匹配.dta"))
xwalk['code_str'] = xwalk['code'].apply(lambda x: f'{int(x):06d}')
county_to_code = dict(zip(xwalk['countyid'].astype(int), xwalk['code'].astype(int)))

policy = pd.read_csv(os.path.join(PROJECT_ROOT, "02_policy_mapping", "policy_unit_table.csv"))
treat_counties = set()
treat_map = {}
for _, row in policy.iterrows():
    pcode = int(row['county_code'])
    plevel = row['policy_level']
    code_str = f'{pcode:06d}'
    if plevel in ['municipal_district', 'county', 'county_level_city']:
        treat_counties.add(pcode)
        treat_map[pcode] = row['policy_unit_id']
    elif plevel in ['prefecture_city', 'sub_provincial_city', 'autonomous_prefecture']:
        prefix = code_str[:4]
        matching = xwalk[xwalk['code_str'].str[:4] == prefix]
        for c in matching['code'].astype(int):
            treat_counties.add(c)
            treat_map[c] = row['policy_unit_id']

# ============================================================
# 2. Load 2014 baseline for family structure
# ============================================================
print("\n[2] Loading 2014 baseline...")
df14, _ = pyreadstat.read_dta(os.path.join(CFPS_ROOT, 'cfps数据及清洗代码', '14', '14个人.dta'))
df14['countyid_int'] = df14['countyid14'].astype(float).astype('Int64')
df14['admin_code'] = df14['countyid_int'].map(county_to_code)
df14 = df14[df14['admin_code'].notna()].copy()

# Family-level: does family have 60+ member in 2014?
age_2014 = pd.to_numeric(df14.get('age_', df14.get('age')), errors='coerce')
df14['age_2014'] = age_2014
family_60plus = df14[df14['age_2014'] >= 60].groupby('fid14').size()
df14['older_household_2014'] = df14['fid14'].map(family_60plus).fillna(0).astype(int)
df14['older_household_2014'] = (df14['older_household_2014'] > 0).astype(int)

# Working age 18-59 in 2014
df14['working_age_2014'] = ((df14['age_2014'] >= 18) & (df14['age_2014'] <= 59)).astype(int)

# Full working age sample (not restricted to older households)
working_age_pids = set(df14[df14['working_age_2014'] == 1]['pid'])
print(f"  Working age 18-59 in 2014: {len(working_age_pids)}")

# ============================================================
# 3. Load all waves and build panel
# ============================================================
print("\n[3] Loading CFPS waves...")
cleaned_files = {
    2010: '中国数据库/CFPS/cfps数据及清洗代码/10/10个人.dta',
    2012: '中国数据库/CFPS/cfps数据及清洗代码/12/12个人.dta',
    2014: '中国数据库/CFPS/cfps数据及清洗代码/14/14个人.dta',
    2018: '中国数据库/CFPS/cfps数据及清洗代码/18/18个人.dta',
    2020: '中国数据库/CFPS/cfps数据及清洗代码/20/20个人.dta',
}

panel_rows = []
for wave, path in cleaned_files.items():
    df, _ = pyreadstat.read_dta(path)

    # County ID
    if wave in [2010, 2012]:
        county_var = 'countyid'
    elif wave == 2014:
        county_var = 'countyid14'
    elif wave == 2018:
        if 'countyid18' not in df.columns:
            merged = os.path.join(CFPS_ROOT, 'cfps数据及清洗代码', '18', '18个人_with_countyid.dta')
            if os.path.exists(merged):
                df, _ = pyreadstat.read_dta(merged)
        county_var = 'countyid18'
    elif wave == 2020:
        county_var = 'countyid20'

    df['countyid_int'] = df[county_var].astype(float).astype('Int64')
    df['admin_code'] = df['countyid_int'].map(county_to_code)
    df = df[df['admin_code'].notna()].copy()
    df['treat'] = df['admin_code'].isin(treat_counties).astype(int)

    # Working age in this wave
    age_var = 'age_' if 'age_' in df.columns else 'age'
    df['age_num'] = pd.to_numeric(df[age_var], errors='coerce')
    df['working_age'] = ((df['age_num'] >= 18) & (df['age_num'] <= 59)).astype(int)

    # Employment
    df['employed'] = pd.to_numeric(df['job'], errors='coerce')

    # Wave and post
    df['wave'] = wave
    df['post'] = int(wave >= 2018)

    # Merge older_household_2014
    df = df.merge(df14[['pid', 'older_household_2014']], on='pid', how='left', suffixes=('', '_bl'))
    if 'older_household_2014_bl' in df.columns:
        df['older_household_2014'] = df['older_household_2014_bl']
    df['older_household_2014'] = df['older_household_2014'].fillna(0).astype(int)

    # Keep working age only
    df = df[df['working_age'] == 1].copy()

    # DID and DDD variables
    df['did'] = df['treat'] * df['post']
    df['did_oh'] = df['did'] * df['older_household_2014']
    df['treat_oh'] = df['treat'] * df['older_household_2014']
    df['post_oh'] = df['post'] * df['older_household_2014']

    panel_rows.append(df)
    print(f"  {wave}: {len(df)} working-age obs, treat={df['treat'].sum()}, older_hh={df['older_household_2014'].sum()}")

panel = pd.concat(panel_rows, ignore_index=True)
panel = panel[panel['wave'].isin([2010, 2012, 2014, 2018])].copy()

print(f"\n  Panel: {len(panel)} obs, {panel['pid'].nunique()} individuals")
print(f"  Older household 2014: {panel['older_household_2014'].mean():.3f}")

# ============================================================
# 4. DDD: Treat × Post × OlderHousehold2014
# ============================================================
print("\n[4] DDD: Treat × Post × OlderHousehold2014...")

# Model with individual FE
df = panel.copy()
for var in ['did', 'did_oh', 'treat', 'post', 'older_household_2014', 'treat_oh', 'post_oh', 'employed']:
    df[f'{var}_dm'] = df.groupby('pid')[var].transform(lambda x: x - x.mean())

X = sm.add_constant(df[['did_dm', 'did_oh_dm', 'treat_dm', 'post_dm', 'older_household_2014_dm', 'treat_oh_dm', 'post_oh_dm']])
y = df['employed_dm']
model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})

print(f"\n  DDD (individual FE, city-clustered):")
print(f"    DDD (did_oh): {model.params['did_oh_dm']:.4f} (SE={model.bse['did_oh_dm']:.4f}, p={model.pvalues['did_oh_dm']:.4f})")
print(f"    95% CI: [{model.conf_int().loc['did_oh_dm', 0]:.4f}, {model.conf_int().loc['did_oh_dm', 1]:.4f}]")
print(f"    DID (did): {model.params['did_dm']:.4f} (SE={model.bse['did_dm']:.4f}, p={model.pvalues['did_dm']:.4f})")
print(f"    N: {int(model.nobs)}")

# ============================================================
# 5. DDD Event Study
# ============================================================
print("\n[5] DDD Event Study...")
df2 = panel.copy()
ref_wave = 2014
for w in [2010, 2012, 2014, 2018]:
    df2[f'tx_{w}'] = ((df2['treat'] == 1) & (df2['wave'] == w)).astype(int)
    if w == ref_wave:
        df2[f'tx_{w}'] = 0
    df2[f'tx_oh_{w}'] = df2[f'tx_{w}'] * df2['older_household_2014']

for var in ['tx_2010', 'tx_2012', 'tx_2018', 'tx_oh_2010', 'tx_oh_2012', 'tx_oh_2018', 'employed']:
    df2[f'{var}_dm'] = df2.groupby('pid')[var].transform(lambda x: x - x.mean())

X = sm.add_constant(df2[['tx_2010_dm', 'tx_2012_dm', 'tx_2018_dm', 'tx_oh_2010_dm', 'tx_oh_2012_dm', 'tx_oh_2018_dm']])
y = df2['employed_dm']
model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df2['admin_code']})

print(f"\n  DDD Event Study (Treat × Post × OlderHousehold):")
for var, yr in [('tx_oh_2010_dm', '2010'), ('tx_oh_2012_dm', '2012'), ('tx_oh_2018_dm', '2018')]:
    coef = model.params[var]
    se = model.bse[var]
    p = model.pvalues[var]
    ci = model.conf_int().loc[var]
    print(f"    DDD {yr}: {coef:.4f} (SE={se:.4f}, p={p:.4f}, 95CI=[{ci[0]:.4f},{ci[1]:.4f}])")
print(f"    DDD 2014: 0.0000 (reference)")

# Simple DID for comparison
print(f"\n  Simple DID (all working age):")
print(f"    DID: {model.params['tx_2018_dm']:.4f} (SE={model.bse['tx_2018_dm']:.4f}, p={model.pvalues['tx_2018_dm']:.4f})")

# ============================================================
# 6. Sample sizes
# ============================================================
print("\n[6] Sample sizes by wave, treat, older_household...")
for wave in [2010, 2012, 2014, 2018]:
    sub = panel[panel['wave'] == wave]
    for t in [0, 1]:
        for oh in [0, 1]:
            s = sub[(sub['treat'] == t) & (sub['older_household_2014'] == oh)]
            label = f"Treat={t}, OH={oh}"
            print(f"  {wave} {label}: n={len(s)}, emp={s['employed'].mean():.3f}")

print("\n" + "=" * 70)
print("CFPS labor DDD fixed.")
print("=" * 70)
