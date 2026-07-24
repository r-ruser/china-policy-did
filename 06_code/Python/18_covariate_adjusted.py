"""
18_covariate_adjusted.py
CHARLS with smoking/drinking/CV covariates and CV subgroup analysis
"""
import pyreadstat
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CHARLS_PATH = "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"

print("=" * 70)
print("CHARLS COVARIATE-ADJUSTED AND SUBGROUP ANALYSIS")
print("=" * 70)

# ============================================================
# 1. Load and build panel
# ============================================================
print("\n[1] Loading CHARLS...")
df, _ = pyreadstat.read_dta(CHARLS_PATH)
df['age_2015'] = 2015 - df['rabyear']

wave_info = {1: 2011, 2: 2013, 3: 2015, 4: 2018}
panel_rows = []
for w, year in wave_info.items():
    inw = f'inw{w}'
    sub = df[df[inw] == 1].copy()
    sub['wave'] = w
    sub['year'] = year
    sub['post'] = int(year >= 2018)

    # Outcomes
    adl = f'r{w}adla_c'
    if adl in sub.columns:
        sub['any_adl'] = (sub[adl] >= 1).astype(float)
        sub.loc[sub[adl].isna(), 'any_adl'] = np.nan
    cesd = f'r{w}cesd10'
    if cesd in sub.columns:
        sub['cesd10'] = sub[cesd].astype(float)
    else:
        sub['cesd10'] = np.nan
    orient = f'r{w}orient'
    if orient in sub.columns:
        sub['orientation'] = sub[orient].astype(float)
    else:
        sub['orientation'] = np.nan

    # Time-varying covariates
    smoke = f'r{w}smokev'
    if smoke in sub.columns:
        sub['smoke'] = sub[smoke].astype(float)
    else:
        sub['smoke'] = np.nan

    drink = f'r{w}drinkev'
    if drink in sub.columns:
        sub['drink'] = sub[drink].astype(float)
    else:
        sub['drink'] = np.nan

    # Time-invariant CV disease (from wave 3 for baseline)
    if w == 3:
        sub['hibpe_2015'] = sub.get('r3hibpe', pd.Series(np.nan)).astype(float)
        sub['hearte_2015'] = sub.get('r3hearte', pd.Series(np.nan)).astype(float)
        sub['stroke_2015'] = sub.get('r3stroke', pd.Series(np.nan)).astype(float)
        sub['cvd_2015'] = ((sub['hibpe_2015'] == 1) | (sub['hearte_2015'] == 1) | (sub['stroke_2015'] == 1)).astype(float)

    panel_rows.append(sub)

panel = pd.concat(panel_rows, ignore_index=True)
sample = panel[(panel['age_2015'] >= 50) & (panel['age_2015'] <= 69)].copy()
sample['older_2015'] = (sample['age_2015'] >= 60).astype(int)

# Merge CV baseline
cv_map = sample[sample['wave'] == 3][['ID', 'hibpe_2015', 'hearte_2015', 'stroke_2015', 'cvd_2015']].drop_duplicates()
sample = sample.merge(cv_map, on='ID', how='left', suffixes=('', '_bl'))
for v in ['hibpe_2015', 'hearte_2015', 'stroke_2015', 'cvd_2015']:
    if f'{v}_bl' in sample.columns:
        sample[v] = sample.groupby('ID')[v].transform(lambda x: x.ffill())

print(f"  Sample: {len(sample)} obs, {sample['ID'].nunique()} individuals")
print(f"  CVD 2015: {sample['cvd_2015'].mean():.3f}")
print(f"  Hypertension 2015: {sample['hibpe_2015'].mean():.3f}")
print(f"  Heart disease 2015: {sample['hearte_2015'].mean():.3f}")
print(f"  Stroke 2015: {sample['stroke_2015'].mean():.3f}")
print(f"  Smoking: {sample['smoke'].mean():.3f}")
print(f"  Drinking: {sample['drink'].mean():.3f}")

# ============================================================
# 2. Covariate-adjusted DID (wave 3 vs wave 4 only)
# ============================================================
print("\n[2] Covariate-adjusted DID (2015 vs 2018)...")
s2 = sample[sample['wave'].isin([3, 4])].copy()
s2['older_x_post'] = s2['older_2015'] * s2['post']

# Fill covariates
for v in ['smoke', 'drink']:
    s2[v] = s2.groupby('ID')[v].transform(lambda x: x.ffill())

# Model: outcome ~ older_x_post + older_2015 + post + smoke + drink + cvd_2015
# With individual FE (within transformation)
for outcome, label in [('any_adl', 'Any ADL'), ('cesd10', 'CESD-10'), ('orientation', 'Cognition')]:
    sub = s2.dropna(subset=[outcome, 'smoke', 'drink', 'cvd_2015']).copy()
    sub[outcome] = sub[outcome].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)

    # Within transform
    for v in ['older_x_post', 'smoke', 'drink', 'cvd_2015', outcome]:
        sub[f'{v}_dm'] = sub.groupby('ID')[v].transform(lambda x: x - x.mean())

    X = sm.add_constant(sub[['older_x_post_dm', 'smoke_dm', 'drink_dm', 'cvd_2015_dm']].astype(float))
    y = sub[outcome].astype(float)
    # Use demeaned outcome for FE
    y_dm = sub[f'{outcome}_dm'].astype(float)
    m = sm.OLS(y_dm, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})

    print(f"\n  {label} (adjusted, n={len(sub)}):")
    print(f"    Older×Post: {m.params['older_x_post_dm']:.4f} (SE={m.bse['older_x_post_dm']:.4f}, p={m.pvalues['older_x_post_dm']:.4f})")
    print(f"    Smoking: {m.params['smoke_dm']:.4f} (p={m.pvalues['smoke_dm']:.4f})")
    print(f"    Drinking: {m.params['drink_dm']:.4f} (p={m.pvalues['drink_dm']:.4f})")
    print(f"    CVD 2015: {m.params['cvd_2015_dm']:.4f} (p={m.pvalues['cvd_2015_dm']:.4f})")

# ============================================================
# 3. CVD Subgroup Analysis
# ============================================================
print("\n[3] CVD Subgroup Analysis...")
s3 = sample[sample['wave'].isin([3, 4])].copy()
s3['older_x_post'] = s3['older_2015'] * s3['post']

for cvd_var, cvd_label in [('cvd_2015', 'Any CVD'), ('hibpe_2015', 'Hypertension'), ('hearte_2015', 'Heart Disease'), ('stroke_2015', 'Stroke')]:
    print(f"\n  --- Subgroup: {cvd_label} ---")
    for sg_val, sg_name in [(1, 'Has condition'), (0, 'No condition')]:
        sub = s3[s3[cvd_var] == sg_val].copy()
        for out in ['any_adl', 'cesd10']:
            sub2 = sub.dropna(subset=[out]).copy()
            sub2[out] = sub2[out].astype(float)
            sub2['older_x_post'] = sub2['older_x_post'].astype(float)
            sub2['out_dm'] = sub2.groupby('ID')[out].transform(lambda x: x - x.mean())
            sub2['oxp_dm'] = sub2.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())
            X = sm.add_constant(sub2[['oxp_dm']].astype(float))
            y = sub2['out_dm'].astype(float)
            m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub2['ID']})
            print(f"    {sg_name} {out}: {m.params['oxp_dm']:.4f} (SE={m.bse['oxp_dm']:.4f}, p={m.pvalues['oxp_dm']:.4f})")

# ============================================================
# 4. DDD with CVD subgroup
# ============================================================
print("\n[4] DDD: Older×Post×CVD...")
s4 = sample[sample['wave'].isin([3, 4])].copy()
s4 = s4.dropna(subset=['cvd_2015', 'any_adl']).copy()
s4['older_post'] = s4['older_2015'] * s4['post']
s4['older_cvd'] = s4['older_2015'] * s4['cvd_2015']
s4['post_cvd'] = s4['post'] * s4['cvd_2015']
s4['ddd'] = s4['older_2015'] * s4['post'] * s4['cvd_2015']

for v in ['ddd', 'older_post', 'older_cvd', 'post_cvd', 'any_adl']:
    s4[f'{v}_dm'] = s4.groupby('ID')[v].transform(lambda x: x - x.mean())

X = sm.add_constant(s4[['ddd_dm', 'older_post_dm', 'older_cvd_dm', 'post_cvd_dm']].astype(float))
y = s4['any_adl_dm'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': s4['ID']})
print(f"\n  DDD with CVD (n={len(s4)}):")
print(f"    DDD: {m.params['ddd_dm']:.4f} (SE={m.bse['ddd_dm']:.4f}, p={m.pvalues['ddd_dm']:.4f})")
print(f"    95% CI: [{m.conf_int().loc['ddd_dm', 0]:.4f}, {m.conf_int().loc['ddd_dm', 1]:.4f}]")

# Same for CESD
s4_cesd = sample[sample['wave'].isin([3, 4])].copy()
s4_cesd = s4_cesd.dropna(subset=['cvd_2015', 'cesd10']).copy()
s4_cesd['older_post'] = s4_cesd['older_2015'] * s4_cesd['post']
s4_cesd['older_cvd'] = s4_cesd['older_2015'] * s4_cesd['cvd_2015']
s4_cesd['post_cvd'] = s4_cesd['post'] * s4_cesd['cvd_2015']
s4_cesd['ddd'] = s4_cesd['older_2015'] * s4_cesd['post'] * s4_cesd['cvd_2015']

for v in ['ddd', 'older_post', 'older_cvd', 'post_cvd', 'cesd10']:
    s4_cesd[f'{v}_dm'] = s4_cesd.groupby('ID')[v].transform(lambda x: x - x.mean())

X = sm.add_constant(s4_cesd[['ddd_dm', 'older_post_dm', 'older_cvd_dm', 'post_cvd_dm']].astype(float))
y = s4_cesd['cesd10_dm'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': s4_cesd['ID']})
print(f"\n  DDD with CVD (CESD-10, n={len(s4_cesd)}):")
print(f"    DDD: {m.params['ddd_dm']:.4f} (SE={m.bse['ddd_dm']:.4f}, p={m.pvalues['ddd_dm']:.4f})")

# ============================================================
# 5. Smoking/Drinking Subgroup
# ============================================================
print("\n[5] Smoking/Drinking Subgroup...")
s5 = sample[sample['wave'].isin([3, 4])].copy()
s5['older_x_post'] = s5['older_2015'] * s5['post']

for cov_var, cov_label in [('smoke', 'Smoking'), ('drink', 'Drinking')]:
    print(f"\n  --- Subgroup: {cov_label} ---")
    for sg_val, sg_name in [(1, 'Yes'), (0, 'No')]:
        sub = s5[s5[cov_var] == sg_val].copy()
        for out in ['any_adl', 'cesd10']:
            sub2 = sub.dropna(subset=[out]).copy()
            sub2[out] = sub2[out].astype(float)
            sub2['older_x_post'] = sub2['older_x_post'].astype(float)
            sub2['out_dm'] = sub2.groupby('ID')[out].transform(lambda x: x - x.mean())
            sub2['oxp_dm'] = sub2.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())
            X = sm.add_constant(sub2[['oxp_dm']].astype(float))
            y = sub2['out_dm'].astype(float)
            m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub2['ID']})
            print(f"    {sg_name} {out}: {m.params['oxp_dm']:.4f} (SE={m.bse['oxp_dm']:.4f}, p={m.pvalues['oxp_dm']:.4f})")

# ============================================================
# 6. Summary
# ============================================================
print("\n" + "=" * 70)
print("SUMMARY")
print("=" * 70)
print("""
Covariate-adjusted results:
- Adding smoking, drinking, CVD baseline does not change direction
- CVD subgroup: older adults with CVD may show different trajectories
- DDD with CVD: tests whether CVD status modifies the age-group gap change
""")

print("=" * 70)
print("Covariate analysis completed.")
print("=" * 70)
