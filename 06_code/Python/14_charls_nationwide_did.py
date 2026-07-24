"""
14_charls_nationwide_did.py
CHARLS nationwide deployment: Older×Post DID and DDD
"""
import pyreadstat
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CHARLS_PATH = "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "07_results")
os.makedirs(os.path.join(OUTPUT_DIR, "tables"), exist_ok=True)

print("=" * 70)
print("CHARLS NATIONWIDE DEPLOYMENT ANALYSIS")
print("=" * 70)

# ============================================================
# 1. Load and prepare CHARLS data
# ============================================================
print("\n[1] Loading CHARLS data...")
df, meta = pyreadstat.read_dta(CHARLS_PATH)
print(f"  Total: {len(df)} individuals, {len(df.columns)} variables")

# Construct age in 2015
df['age_2015'] = 2015 - df['rabyear']

# ============================================================
# 2. Build analysis sample: age 50-69 in 2015
# ============================================================
print("\n[2] Building analysis sample (age 50-69 in 2015)...")
sample = df[(df['age_2015'] >= 50) & (df['age_2015'] <= 69)].copy()
print(f"  Total: {len(sample)}")
print(f"  50-59: {((sample['age_2015'] >= 50) & (sample['age_2015'] < 60)).sum()}")
print(f"  60-69: {((sample['age_2015'] >= 60) & (sample['age_2015'] <= 69)).sum()}")

# Older2015 indicator
sample['older_2015'] = (sample['age_2015'] >= 60).astype(int)
print(f"  Older2015 (60+): {sample['older_2015'].sum()}")

# ============================================================
# 3. Build panel from harmonized data
# ============================================================
print("\n[3] Building panel...")

# Wave mapping: w1=2011, w2=2013, w3=2015, w4=2018
wave_info = {
    1: {'year': 2011, 'inw': 'inw1', 'post': 0},
    2: {'year': 2013, 'inw': 'inw2', 'post': 0},
    3: {'year': 2015, 'inw': 'inw3', 'post': 0},
    4: {'year': 2018, 'inw': 'inw4', 'post': 1},
}

panel_rows = []
for w, info in wave_info.items():
    inw_col = info['inw']
    sub = sample[sample[inw_col] == 1].copy()
    sub['wave'] = w
    sub['year'] = info['year']
    sub['post'] = info['post']

    # ADL count
    adl_col = f'r{w}adla_c'
    if adl_col in sub.columns:
        sub['adl_count'] = sub[adl_col]
    else:
        sub['adl_count'] = np.nan

    # IADL count (not available in wave 1)
    iadl_col = f'r{w}iadla'
    if iadl_col in sub.columns:
        sub['iadl_count'] = sub[iadl_col]
    else:
        sub['iadl_count'] = np.nan

    # CESD-10
    cesd_col = f'r{w}cesd10'
    if cesd_col in sub.columns:
        sub['cesd10'] = sub[cesd_col]
    else:
        sub['cesd10'] = np.nan

    # Orientation (cognition)
    orient_col = f'r{w}orient'
    if orient_col in sub.columns:
        sub['orientation'] = sub[orient_col]
    else:
        sub['orientation'] = np.nan

    # Any ADL difficulty
    sub['any_adl'] = (sub['adl_count'] >= 1).astype(float)
    sub.loc[sub['adl_count'].isna(), 'any_adl'] = np.nan

    # Any IADL difficulty
    sub['any_iadl'] = (sub['iadl_count'] >= 1).astype(float)
    sub.loc[sub['iadl_count'].isna(), 'any_iadl'] = np.nan

    # HighNeed from 2015 (wave 3)
    if w == 3:
        # Chronic disease count from 2015
        chronic_count = np.zeros(len(sub))
        for dc in ['r3hearte', 'r3stroke', 'r3lunge']:
            if dc in sub.columns:
                chronic_count += (sub[dc] == 1).astype(int).values
        sub['chronic_count_2015'] = chronic_count
        sub['highneed_chronic'] = (sub['chronic_count_2015'] >= 2).astype(int)
        sub['highneed_adl'] = (sub['adl_count'] >= 1).astype(int)
        sub['highneed_iadl'] = (sub['iadl_count'] >= 1).astype(int)
        sub['highneed_combined'] = ((sub['highneed_adl'] == 1) | (sub['highneed_iadl'] == 1)).astype(int)

    # Individual and wave FE dummies
    sub['wave_2013'] = (sub['wave'] == 2).astype(int)
    sub['wave_2015'] = (sub['wave'] == 3).astype(int)
    sub['wave_2018'] = (sub['wave'] == 4).astype(int)

    panel_rows.append(sub)

panel = pd.concat(panel_rows, ignore_index=True)
panel = panel.sort_values(['ID', 'wave']).reset_index(drop=True)

print(f"  Panel: {len(panel)} rows, {panel['ID'].nunique()} individuals")
for w in [1, 2, 3, 4]:
    sub = panel[panel['wave'] == w]
    print(f"    Wave {w} ({sub['year'].iloc[0]}): {len(sub)} obs")

# ============================================================
# 4. DID: Older2015 × Post(2018)
# ============================================================
print("\n[4] DID: Older2015 × Post(2018)...")

# Post = 1 only for wave 4 (2018)
# Reference: wave 3 (2015)
panel_did = panel[panel['wave'].isin([3, 4])].copy()
panel_did['older_x_post'] = panel_did['older_2015'] * panel_did['post']

for outcome, label in [('any_adl', 'Any ADL'), ('cesd10', 'CESD-10'), ('orientation', 'Cognition')]:
    sub = panel_did.dropna(subset=[outcome]).copy()
    sub[outcome] = sub[outcome].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)
    print(f"\n  {label} (n={len(sub)}):")
    # Individual FE + wave FE
    sub['outcome_dm'] = sub.groupby('ID')[outcome].transform(lambda x: x - x.mean())
    sub['older_x_post_dm'] = sub.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())

    X = sm.add_constant(sub[['older_x_post_dm']].astype(float))
    y = sub['outcome_dm'].astype(float)
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
    print(f"    Older×Post: {model.params['older_x_post_dm']:.4f} (SE={model.bse['older_x_post_dm']:.4f}, p={model.pvalues['older_x_post_dm']:.4f})")
    print(f"    95% CI: [{model.conf_int().loc['older_x_post_dm', 0]:.4f}, {model.conf_int().loc['older_x_post_dm', 1]:.4f}]")

# ============================================================
# 5. Event study: Older2015 × Year
# ============================================================
print("\n[5] Event study: Older2015 × Year...")

ref_wave = 3  # 2015
for outcome, label in [('any_adl', 'Any ADL'), ('cesd10', 'CESD-10'), ('orientation', 'Cognition')]:
    df_es = panel.dropna(subset=[outcome]).copy()

    # Create Older × Wave interactions
    for w in [1, 2, 3, 4]:
        df_es[f'ow_{w}'] = (df_es['older_2015'] == 1) & (df_es['wave'] == w)
        if w == ref_wave:
            df_es[f'ow_{w}'] = 0

    interact_vars = [f'ow_{w}' for w in [1, 2, 4]]

    # Demean by individual
    for var in interact_vars + [outcome]:
        df_es[var] = df_es[var].astype(float)
        df_es[f'{var}_dm'] = df_es.groupby('ID')[var].transform(lambda x: x - x.mean())

    X = sm.add_constant(df_es[[f'{v}_dm' for v in interact_vars]].astype(float))
    y = df_es[f'{outcome}_dm'].astype(float)
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df_es['ID']})

    print(f"\n  {label} Event Study:")
    for w, yr in [(1, '2011'), (2, '2013'), (4, '2018')]:
        var = f'ow_{w}_dm'
        coef = model.params[var]
        se = model.bse[var]
        p = model.pvalues[var]
        ci = model.conf_int().loc[var]
        print(f"    {yr}: {coef:.4f} (SE={se:.4f}, p={p:.4f}, 95CI=[{ci[0]:.4f},{ci[1]:.4f}])")
    print(f"    2015: 0.0000 (reference)")

    # Pre-trend tests
    for var, yr in [(f'ow_1_dm', '2011'), (f'ow_2_dm', '2013')]:
        t_stat = model.params[var] / model.bse[var]
        from scipy.stats import t as t_dist
        p_val = 2 * t_dist.sf(abs(t_stat), df=model.df_resid)
        print(f"    Pre-trend {yr}: t={t_stat:.3f}, p={p_val:.4f}")

# ============================================================
# 6. DDD: Older2015 × Post × HighNeed
# ============================================================
print("\n[6] DDD: Older2015 × Post × HighNeed...")
panel_ddd = panel[panel['wave'].isin([3, 4])].copy()

# Merge HighNeed from wave 3
hn_map = panel_ddd[panel_ddd['wave'] == 3][['ID', 'highneed_chronic', 'highneed_adl', 'highneed_iadl', 'highneed_combined']].drop_duplicates()
panel_ddd = panel_ddd.merge(hn_map, on='ID', how='left', suffixes=('', '_bl'))
# Forward-fill HighNeed to wave 4
for hn_var in ['highneed_chronic', 'highneed_adl', 'highneed_iadl', 'highneed_combined']:
    if f'{hn_var}_bl' in panel_ddd.columns:
        panel_ddd[hn_var] = panel_ddd[hn_var].fillna(panel_ddd[f'{hn_var}_bl'])

for hn_var, hn_label in [('highneed_combined', 'Combined(ADL/IADL)'), ('highneed_adl', 'ADL>=1'), ('highneed_chronic', 'Chronic>=2')]:
    sub = panel_ddd.dropna(subset=['any_adl', hn_var]).copy()
    sub['older_post'] = sub['older_2015'] * sub['post']
    sub['older_hn'] = sub['older_2015'] * sub[hn_var]
    sub['post_hn'] = sub['post'] * sub[hn_var]
    sub['ddd'] = sub['older_2015'] * sub['post'] * sub[hn_var]

    # Demean
    for var in ['ddd', 'older_post', 'older_hn', 'post_hn', 'any_adl']:
        sub[var] = sub[var].astype(float)
        sub[f'{var}_dm'] = sub.groupby('ID')[var].transform(lambda x: x - x.mean())

    X = sm.add_constant(sub[['ddd_dm', 'older_post_dm', 'older_hn_dm', 'post_hn_dm']].astype(float))
    y = sub['any_adl_dm'].astype(float)
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})

    print(f"\n  DDD with {hn_label} (n={len(sub)}):")
    print(f"    DDD: {model.params['ddd_dm']:.4f} (SE={model.bse['ddd_dm']:.4f}, p={model.pvalues['ddd_dm']:.4f})")
    print(f"    95% CI: [{model.conf_int().loc['ddd_dm', 0]:.4f}, {model.conf_int().loc['ddd_dm', 1]:.4f}]")

# ============================================================
# 7. Narrow age window (55-69)
# ============================================================
print("\n[7] Narrow age window (55-69)...")
sample_narrow = df[(df['age_2015'] >= 55) & (df['age_2015'] <= 69)].copy()
sample_narrow['older_2015'] = (sample_narrow['age_2015'] >= 60).astype(int)
print(f"  55-59: {((sample_narrow['age_2015'] >= 55) & (sample_narrow['age_2015'] < 60)).sum()}")
print(f"  60-69: {((sample_narrow['age_2015'] >= 60) & (sample_narrow['age_2015'] <= 69)).sum()}")

# Build narrow panel
panel_narrow_rows = []
for w, info in wave_info.items():
    inw_col = info['inw']
    sub = sample_narrow[sample_narrow[inw_col] == 1].copy()
    sub['wave'] = w
    sub['year'] = info['year']
    sub['post'] = info['post']
    adl_col = f'r{w}adla_c'
    if adl_col in sub.columns:
        sub['any_adl'] = (sub[adl_col] >= 1).astype(float)
        sub.loc[sub[adl_col].isna(), 'any_adl'] = np.nan
    cesd_col = f'r{w}cesd10'
    if cesd_col in sub.columns:
        sub['cesd10'] = sub[cesd_col]
    else:
        sub['cesd10'] = np.nan
    panel_narrow_rows.append(sub)

panel_narrow = pd.concat(panel_narrow_rows, ignore_index=True)
panel_narrow_dn = panel_narrow[panel_narrow['wave'].isin([3, 4])].copy()
panel_narrow_dn['older_x_post'] = panel_narrow_dn['older_2015'] * panel_narrow_dn['post']

for outcome, label in [('any_adl', 'Any ADL'), ('cesd10', 'CESD-10')]:
    sub = panel_narrow_dn.dropna(subset=[outcome]).copy()
    sub[outcome] = sub[outcome].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)
    sub['outcome_dm'] = sub.groupby('ID')[outcome].transform(lambda x: x - x.mean())
    sub['oxp_dm'] = sub.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())
    X = sm.add_constant(sub[['oxp_dm']].astype(float))
    y = sub['outcome_dm'].astype(float)
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
    print(f"\n  {label} (55-69, n={len(sub)}):")
    print(f"    Older×Post: {model.params['oxp_dm']:.4f} (SE={model.bse['oxp_dm']:.4f}, p={model.pvalues['oxp_dm']:.4f})")

print("\n" + "=" * 70)
print("CHARLS analysis completed.")
print("=" * 70)
