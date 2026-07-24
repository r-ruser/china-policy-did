"""
19_competing_risk_attrition.py
Handle competing risk (mortality) and attrition bias via IPCW
"""
import pyreadstat
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm
from sklearn.linear_model import LogisticRegression

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CHARLS_PATH = "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"

print("=" * 70)
print("COMPETING RISK AND ATTRITION BIAS CORRECTION")
print("=" * 70)

# ============================================================
# 1. Load and build panel
# ============================================================
print("\n[1] Loading CHARLS...")
df, _ = pyreadstat.read_dta(CHARLS_PATH)
df['age_2015'] = 2015 - df['rabyear']
sample = df[(df['age_2015'] >= 50) & (df['age_2015'] <= 69)].copy()
sample['older_2015'] = (sample['age_2015'] >= 60).astype(int)

wave_info = {1: 2011, 2: 2013, 3: 2015, 4: 2018}
panel_rows = []
for w, year in wave_info.items():
    inw = f'inw{w}'
    sub = sample[sample[inw] == 1].copy()
    sub['wave'] = w
    sub['year'] = year
    sub['post'] = int(year >= 2018)

    adl = f'r{w}adla_c'
    if adl in sub.columns:
        sub['adl_count'] = pd.to_numeric(sub[adl], errors='coerce')
        sub['any_adl'] = (sub['adl_count'] >= 1).astype(float)
        sub.loc[sub['adl_count'].isna(), 'any_adl'] = np.nan
    cesd = f'r{w}cesd10'
    if cesd in sub.columns:
        sub['cesd10'] = pd.to_numeric(sub[cesd], errors='coerce')
    else:
        sub['cesd10'] = np.nan

    # Death indicator
    death = f'r{w}chdeathe'
    if death in sub.columns:
        sub['died'] = pd.to_numeric(sub[death], errors='coerce')
    else:
        sub['died'] = 0

    # Covariates
    for var, fname in [('smoke', f'r{w}smokev'), ('drink', f'r{w}drinkev'),
                         ('hibpe', f'r{w}hibpe'), ('hearte', f'r{w}hearte'),
                         ('stroke', f'r{w}stroke'), ('lunge', f'r{w}lunge'),
                         ('diabe', f'r{w}diabe')]:
        if fname in sub.columns:
            sub[var] = pd.to_numeric(sub[fname], errors='coerce')
        else:
            sub[var] = np.nan

    panel_rows.append(sub)

panel = pd.concat(panel_rows, ignore_index=True)

# Baseline from wave 3
w3 = panel[panel['wave'] == 3].copy()
w3['bl_any_adl'] = (w3['adl_count'] >= 1).astype(int)
w3['bl_cesd10'] = w3['cesd10']
w3['bl_depression'] = (w3['cesd10'] >= 10).astype(int)

bl_map = w3[['ID', 'bl_any_adl', 'bl_cesd10', 'bl_depression']].drop_duplicates()
panel = panel.merge(bl_map, on='ID', how='left', suffixes=('', '_bl'))
for v in ['bl_any_adl', 'bl_cesd10', 'bl_depression']:
    panel[v] = panel.groupby('ID')[v].transform(lambda x: x.ffill())

print(f"  Panel: {len(panel)} obs, {panel['ID'].nunique()} individuals")

# ============================================================
# 2. Attrition analysis: who is missing in 2018?
# ============================================================
print("\n[2] Attrition analysis...")
bl_2015 = panel[panel['wave'] == 3].copy()
bl_2015['retained_2018'] = bl_2015['inw4'].astype(int)
bl_2015['died_2015_2018'] = 0
# Check death between wave 3 and wave 4
death_4 = panel[(panel['wave'] == 4) & (panel['died'] == 1)]['ID'].unique()
bl_2015.loc[bl_2015['ID'].isin(death_4), 'died_2015_2018'] = 1

retained_n = bl_2015['retained_2018'].sum()
retained_pct = bl_2015['retained_2018'].mean() * 100
print(f"  Retained in 2018: {retained_n}/{len(bl_2015)} ({retained_pct:.1f}% retained)")
print(f"  Died 2015-2018: {bl_2015['died_2015_2018'].sum()} ({bl_2015['died_2015_2018'].mean()*100:.1f}%)")
alive_lost = ((bl_2015['retained_2018']==0) & (bl_2015['died_2015_2018']==0)).sum()
print(f"  Alive but lost: {alive_lost}")

# ============================================================
# 3. Build IPCW weights for attrition
# ============================================================
print("\n[3] Building IPCW weights for attrition...")

# Predictors of attrition (from baseline 2015)
attrition_model_data = bl_2015[['ID', 'retained_2018', 'age_2015', 'older_2015',
                                  'bl_any_adl', 'bl_cesd10', 'bl_depression',
                                  'smoke', 'drink', 'hibpe', 'hearte', 'stroke',
                                  'diabe', 'lunge']].dropna()

X_attr = attrition_model_data[['age_2015', 'older_2015', 'bl_any_adl', 'bl_cesd10',
                                 'smoke', 'drink', 'hibpe', 'hearte', 'stroke',
                                 'diabe', 'lunge']].fillna(0)
y_attr = attrition_model_data['retained_2018']

attr_model = LogisticRegression(max_iter=1000)
attr_model.fit(X_attr, y_attr)
attrition_model_data['attr_prob'] = attr_model.predict_proba(X_attr)[:, 1]

# IPW weight: 1/P(retained)
attrition_model_data['ipw_attr'] = 1.0 / attrition_model_data['attr_prob']
attrition_model_data['ipw_attr'] = np.clip(attrition_model_data['ipw_attr'], 0, 10)

print(f"  Attrition model AUC: {attr_model.score(X_attr, y_attr):.3f}")
print(f"  Mean IPW: {attrition_model_data['ipw_attr'].mean():.3f}")
print(f"  Max IPW: {attrition_model_data['ipw_attr'].max():.3f}")

# ============================================================
# 4. Build IPCW weights for mortality (competing risk)
# ============================================================
print("\n[4] Building IPCW weights for mortality...")

# Predict mortality between wave 3 and wave 4 among those alive at wave 3
mort_data = bl_2015[['ID', 'died_2015_2018', 'age_2015', 'older_2015',
                       'bl_any_adl', 'bl_cesd10', 'smoke', 'drink',
                       'hibpe', 'hearte', 'stroke', 'diabe', 'lunge']].dropna()

X_mort = mort_data[['age_2015', 'older_2015', 'bl_any_adl', 'bl_cesd10',
                      'smoke', 'drink', 'hibpe', 'hearte', 'stroke',
                      'diabe', 'lunge']].fillna(0)
y_mort = mort_data['died_2015_2018']

mort_model = LogisticRegression(max_iter=1000)
mort_model.fit(X_mort, y_mort)
mort_data['mort_prob'] = mort_model.predict_proba(X_mort)[:, 1]

# IPW weight: 1/P(survived)
mort_data['ipw_mort'] = 1.0 / (1 - mort_data['mort_prob'])
mort_data['ipw_mort'] = np.clip(mort_data['ipw_mort'], 0, 10)

print(f"  Mortality model AUC: {mort_model.score(X_mort, y_mort):.3f}")
print(f"  Mean IPW: {mort_data['ipw_mort'].mean():.3f}")

# ============================================================
# 5. Combined weights
# ============================================================
print("\n[5] Combining weights...")
weights = attrition_model_data[['ID', 'ipw_attr']].merge(
    mort_data[['ID', 'ipw_mort']], on='ID', how='outer')
weights['ipw_combined'] = weights['ipw_attr'] * weights['ipw_mort']
weights['ipw_combined'] = np.clip(weights['ipw_combined'], 0, 20)

panel_w = panel.merge(weights[['ID', 'ipw_attr', 'ipw_mort', 'ipw_combined']], on='ID', how='left')
panel_w['ipw_combined'] = panel_w['ipw_combined'].fillna(1)

print(f"  Combined weight mean: {panel_w['ipw_combined'].mean():.3f}")

# ============================================================
# 6. Weighted DID: Incident ADL
# ============================================================
print("\n[6] Weighted DID: Incident ADL...")

# Standard DID
s = panel_w[(panel_w['wave'].isin([3, 4])) & (panel_w['bl_any_adl'] == 0)].copy()
s['older_x_post'] = s['older_2015'] * s['post']

for weight_var, weight_label in [(None, 'Unweighted'), ('ipw_attr', 'Attrition IPW'),
                                   ('ipw_mort', 'Mortality IPCW'), ('ipw_combined', 'Combined IPW')]:
    sub = s.dropna(subset=['any_adl']).copy()
    sub['any_adl'] = sub['any_adl'].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)

    if weight_var:
        w = sub[weight_var].values.copy()
        w = np.clip(w, 0.1, 20)  # Additional truncation
        w = w / w.mean()  # Normalize
    else:
        w = np.ones(len(sub))

    X = sm.add_constant(sub[['older_x_post']].astype(float))
    y = sub['any_adl'].astype(float)
    try:
        m = sm.WLS(y, X, weights=w).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
        print(f"  {weight_label:>15}: {m.params['older_x_post']:.4f} (SE={m.bse['older_x_post']:.4f}, p={m.pvalues['older_x_post']:.4f})")
    except Exception as e:
        print(f"  {weight_label:>15}: FAILED ({e})")

# ============================================================
# 7. Weighted DID: Incident depression
# ============================================================
print("\n[7] Weighted DID: Incident depression...")
s_dep = panel_w[(panel_w['wave'].isin([3, 4])) & (panel_w['bl_depression'] == 0)].copy()
s_dep['new_dep'] = (s_dep['cesd10'] >= 10).astype(float)
s_dep['older_x_post'] = s_dep['older_2015'] * s_dep['post']

for weight_var, weight_label in [(None, 'Unweighted'), ('ipw_attr', 'Attrition IPW'),
                                   ('ipw_mort', 'Mortality IPCW'), ('ipw_combined', 'Combined IPW')]:
    sub = s_dep.dropna(subset=['new_dep', 'cesd10']).copy()
    sub['new_dep'] = sub['new_dep'].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)

    if weight_var:
        w = sub[weight_var].values.copy()
        w = np.clip(w, 0.1, 20)
        w = w / w.mean()
    else:
        w = np.ones(len(sub))

    X = sm.add_constant(sub[['older_x_post']].astype(float))
    y = sub['new_dep'].astype(float)
    try:
        m = sm.WLS(y, X, weights=w).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
        print(f"  {weight_label:>15}: {m.params['older_x_post']:.4f} (SE={m.bse['older_x_post']:.4f}, p={m.pvalues['older_x_post']:.4f})")
    except Exception as e:
        print(f"  {weight_label:>15}: FAILED ({e})")

# ============================================================
# 8. Sensitivity: vary IPW truncation
# ============================================================
print("\n[8] Sensitivity: IPW truncation levels...")
s2 = panel_w[(panel_w['wave'].isin([3, 4])) & (panel_w['bl_any_adl'] == 0)].copy()
s2['older_x_post'] = s2['older_2015'] * s2['post']

for trunc in [5, 10, 20, 50]:
    s2[f'ipw_{trunc}'] = np.clip(s2['ipw_combined'], 1/trunc, trunc)
    sub = s2.dropna(subset=['any_adl']).copy()
    sub['any_adl'] = sub['any_adl'].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)
    w = sub[f'ipw_{trunc}'].values.copy()
    w = w / w.mean()
    X = sm.add_constant(sub[['older_x_post']].astype(float))
    y = sub['any_adl'].astype(float)
    try:
        m = sm.WLS(y, X, weights=w).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
        print(f"  Trunc [{1/trunc:.2f}, {trunc}]: {m.params['older_x_post']:.4f} (SE={m.bse['older_x_post']:.4f}, p={m.pvalues['older_x_post']:.4f})")
    except Exception as e:
        print(f"  Trunc [{1/trunc:.2f}, {trunc}]: FAILED")

# ============================================================
# 9. Summary
# ============================================================
print("\n" + "=" * 70)
print("SUMMARY")
print("=" * 70)
print("""
Competing risk and attrition corrections applied:
- IPCW for mortality (competing risk)
- IPW for attrition (loss to follow-up)
- Combined weights

Results should be compared with unweighted estimates.
If weighted and unweighted are similar, bias is minimal.
If weighted differs substantially, unweighted estimates are biased.
""")

print("=" * 70)
print("Competing risk and attrition correction completed.")
print("=" * 70)
