"""
20_corrected_analysis.py
Corrected analysis: exclude baseline outcomes, study onset/change only
"""
import pyreadstat
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CHARLS_PATH = "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "07_results")

print("=" * 70)
print("CORRECTED ANALYSIS: Incident Outcomes Only")
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
        sub['adl_count'] = sub[adl].astype(float)
        sub['any_adl'] = (sub['adl_count'] >= 1).astype(float)
        sub.loc[sub['adl_count'].isna(), 'any_adl'] = np.nan
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

    panel_rows.append(sub)

panel = pd.concat(panel_rows, ignore_index=True)

# ============================================================
# 2. Define baseline outcomes from wave 3 (2015)
# ============================================================
print("\n[2] Defining baseline outcomes...")
w3 = panel[panel['wave'] == 3].copy()
w3['bl_any_adl'] = (w3['adl_count'] >= 1).astype(int)
w3['bl_cesd10'] = w3['cesd10']
w3['bl_depression'] = (w3['cesd10'] >= 10).astype(int)
w3['bl_orientation'] = w3['orientation']
w3['bl_low_cog'] = (w3['orientation'] <= 2).astype(int)

bl_map = w3[['ID', 'bl_any_adl', 'bl_cesd10', 'bl_depression', 'bl_orientation', 'bl_low_cog']].drop_duplicates()
panel = panel.merge(bl_map, on='ID', how='left', suffixes=('', '_bl'))
for v in ['bl_any_adl', 'bl_cesd10', 'bl_depression', 'bl_orientation', 'bl_low_cog']:
    panel[v] = panel.groupby('ID')[v].transform(lambda x: x.ffill())

# Baseline conditions
for v in ['r3hibpe', 'r3hearte', 'r3stroke', 'r3lunge', 'r3diabe']:
    if v in panel.columns:
        panel[v.replace('r3', 'bl_')] = panel[v].astype(float)
        panel[v.replace('r3', 'bl_')] = panel.groupby('ID')[v.replace('r3', 'bl_')].transform(lambda x: x.ffill())

panel['cvd_2015'] = ((panel.get('bl_hibpe', 0) == 1) | (panel.get('bl_hearte', 0) == 1) | (panel.get('bl_stroke', 0) == 1)).astype(float)

# Covariates
for w in range(1, 5):
    for var, fname in [('smoke', f'r{w}smokev'), ('drink', f'r{w}drinkev')]:
        if fname in panel.columns:
            panel.loc[panel['wave'] == w, var] = panel.loc[panel['wave'] == w, fname].astype(float)

print(f"  Baseline ADL: {panel['bl_any_adl'].mean():.3f}")
print(f"  Baseline depression: {panel['bl_depression'].mean():.3f}")
print(f"  Baseline low cognition: {panel['bl_low_cog'].mean():.3f}")

# ============================================================
# 3. Incident ADL analysis
# ============================================================
print("\n[3] Incident ADL (exclude baseline ADL)...")
s_adl = panel[(panel['bl_any_adl'] == 0) & panel['any_adl'].notna()].copy()
s_adl['older_x_post'] = s_adl['older_2015'] * s_adl['post']

# Only waves 3 and 4 for DID
s_adl_34 = s_adl[s_adl['wave'].isin([3, 4])].copy()
s_adl_34['any_adl'] = s_adl_34['any_adl'].astype(float)
s_adl_34['older_x_post'] = s_adl_34['older_x_post'].astype(float)

sub = s_adl_34.copy()
sub['out_dm'] = sub.groupby('ID')['any_adl'].transform(lambda x: x - x.mean())
sub['oxp_dm'] = sub.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())
X = sm.add_constant(sub[['oxp_dm']].astype(float))
y = sub['out_dm'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
print(f"  Incident ADL DID (n={len(sub)}):")
print(f"    Older×Post: {m.params['oxp_dm']:.4f} (SE={m.bse['oxp_dm']:.4f}, p={m.pvalues['oxp_dm']:.4f})")
print(f"    95% CI: [{m.conf_int().loc['oxp_dm', 0]:.4f}, {m.conf_int().loc['oxp_dm', 1]:.4f}]")

# ============================================================
# 4. Incident depression analysis
# ============================================================
print("\n[4] Incident depression (exclude baseline CESD>=10)...")
s_dep = panel[(panel['bl_depression'] == 0) & panel['cesd10'].notna()].copy()
s_dep['new_depression'] = (s_dep['cesd10'] >= 10).astype(float)
s_dep['older_x_post'] = s_dep['older_2015'] * s_dep['post']

s_dep_34 = s_dep[s_dep['wave'].isin([3, 4])].copy()
s_dep_34['new_depression'] = s_dep_34['new_depression'].astype(float)
s_dep_34['older_x_post'] = s_dep_34['older_x_post'].astype(float)

sub = s_dep_34.copy()
sub['out_dm'] = sub.groupby('ID')['new_depression'].transform(lambda x: x - x.mean())
sub['oxp_dm'] = sub.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())
X = sm.add_constant(sub[['oxp_dm']].astype(float))
y = sub['out_dm'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
print(f"  Incident depression DID (n={len(sub)}):")
print(f"    Older×Post: {m.params['oxp_dm']:.4f} (SE={m.bse['oxp_dm']:.4f}, p={m.pvalues['oxp_dm']:.4f})")

# Also: change in CESD score among non-depressed
print("\n  CESD change among non-depressed at baseline:")
cesd_w3 = s_dep[s_dep['wave'] == 3][['ID', 'older_2015', 'cesd10']].copy()
cesd_w3.columns = ['ID', 'older_2015', 'cesd_2015']
cesd_w4 = s_dep[s_dep['wave'] == 4][['ID', 'cesd10']].copy()
cesd_w4.columns = ['ID', 'cesd_2018']
cesd_34 = cesd_w3.merge(cesd_w4, on='ID', how='inner')
cesd_34['cesd_change'] = cesd_34['cesd_2018'] - cesd_34['cesd_2015']
X = sm.add_constant(cesd_34[['older_2015']].astype(float))
y = cesd_34['cesd_change'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': cesd_34['ID']})
print(f"    Older×Post: {m.params['older_2015']:.4f} (SE={m.bse['older_2015']:.4f}, p={m.pvalues['older_2015']:.4f})")

# ============================================================
# 5. Cognition decline (exclude baseline low cognition)
# ============================================================
print("\n[5] Cognition decline (exclude baseline orientation<=2)...")
# Compare wave 3 vs wave 4 orientation among those with baseline >2
cog_w3 = panel[(panel['bl_low_cog'] == 0) & (panel['wave'] == 3)][['ID', 'older_2015', 'orientation']].copy()
cog_w3.columns = ['ID', 'older_2015', 'orient_2015']
cog_w4 = panel[(panel['bl_low_cog'] == 0) & (panel['wave'] == 4)][['ID', 'orientation']].copy()
cog_w4.columns = ['ID', 'orient_2018']
cog_34 = cog_w3.merge(cog_w4, on='ID', how='inner')
cog_34['cog_change'] = cog_34['orient_2018'] - cog_34['orient_2015']
print(f"  Sample: {len(cog_34)}")
print(f"  Mean change: {cog_34['cog_change'].mean():.3f}")
print(f"  Older mean change: {cog_34[cog_34['older_2015']==1]['cog_change'].mean():.3f}")
print(f"  Younger mean change: {cog_34[cog_34['older_2015']==0]['cog_change'].mean():.3f}")
X = sm.add_constant(cog_34[['older_2015']].astype(float))
y = cog_34['cog_change'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': cog_34['ID']})
print(f"  Cognition decline DID:")
print(f"    Older×Post: {m.params['older_2015']:.4f} (SE={m.bse['older_2015']:.4f}, p={m.pvalues['older_2015']:.4f})")

# ============================================================
# 6. DDD with incident ADL
# ============================================================
print("\n[6] DDD: Incident ADL × HighNeed...")
s_ddd_adl = s_adl_34.copy()
# Get baseline conditions from panel (already merged)
bl_condition_cols = ['bl_hibpe', 'bl_hearte', 'bl_stroke', 'bl_diabe', 'bl_lunge']
cvd_2015_vals = ((s_ddd_adl.get('bl_hibpe', 0) == 1) | (s_ddd_adl.get('bl_hearte', 0) == 1) | (s_ddd_adl.get('bl_stroke', 0) == 1)).astype(float)
s_ddd_adl['cvd_2015'] = cvd_2015_vals

for cond_var, cond_label in [('bl_diabe', 'Diabetes'), ('cvd_2015', 'CVD'), ('bl_hearte', 'Heart Disease'), ('bl_hibpe', 'Hypertension'), ('bl_lunge', 'Lung Disease')]:
    sub = s_ddd_adl.dropna(subset=[cond_var]).copy()
    sub['older_post'] = sub['older_2015'] * sub['post']
    sub['older_cond'] = sub['older_2015'] * sub[cond_var]
    sub['post_cond'] = sub['post'] * sub[cond_var]
    sub['ddd'] = sub['older_2015'] * sub['post'] * sub[cond_var]
    for v in ['ddd', 'older_post', 'older_cond', 'post_cond', 'any_adl']:
        sub[v] = sub[v].astype(float)
        sub[f'{v}_dm'] = sub.groupby('ID')[v].transform(lambda x: x - x.mean())
    X = sm.add_constant(sub[['ddd_dm', 'older_post_dm', 'older_cond_dm', 'post_cond_dm']].astype(float))
    y = sub['any_adl_dm'].astype(float)
    m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
    print(f"  DDD {cond_label} → Incident ADL (n={len(sub)}):")
    print(f"    DDD: {m.params['ddd_dm']:.4f} (SE={m.bse['ddd_dm']:.4f}, p={m.pvalues['ddd_dm']:.4f})")

# ============================================================
# 7. Save results for R figure generation
# ============================================================
print("\n[7] Saving results for figure generation...")

# Event study data (incident ADL)
es_rows = []
for w in [1, 2, 3, 4]:
    yr = wave_info[w]
    # Event study: older × wave among baseline ADL=0
    sub_w = panel[(panel['bl_any_adl'] == 0) & (panel['wave'] == w) & panel['any_adl'].notna()].copy()
    older = sub_w[sub_w['older_2015'] == 1]
    younger = sub_w[sub_w['older_2015'] == 0]
    es_rows.append({
        'year': yr, 'older_adl': older['any_adl'].mean(),
        'younger_adl': younger['any_adl'].mean(),
        'n_older': len(older), 'n_younger': len(younger),
    })

es_df = pd.DataFrame(es_rows)
es_df.to_csv(os.path.join(OUTPUT_DIR, "tables", "corrected_event_study_adl.csv"), index=False)
print(f"  Saved event study data: {len(es_df)} waves")

# Subgroup results for forest plot
subgroup_results = []
for cond_var, cond_label in [('bl_diabe', 'Diabetes'), ('cvd_2015', 'Any CVD'), ('bl_hearte', 'Heart Disease'), ('bl_hibpe', 'Hypertension'), ('bl_lunge', 'Lung Disease'), ('bl_stroke', 'Stroke')]:
    for sg_val, sg_name in [(1, 'Has'), (0, 'No')]:
        sub = s_adl_34[s_adl_34[cond_var] == sg_val].copy()
        sub = sub.dropna(subset=['any_adl'])
        sub['any_adl'] = sub['any_adl'].astype(float)
        sub['older_x_post'] = sub['older_x_post'].astype(float)
        sub['out_dm'] = sub.groupby('ID')['any_adl'].transform(lambda x: x - x.mean())
        sub['oxp_dm'] = sub.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())
        X = sm.add_constant(sub[['oxp_dm']].astype(float))
        y = sub['out_dm'].astype(float)
        m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
        subgroup_results.append({
            'subgroup': cond_label, 'level': sg_name,
            'coef': m.params['oxp_dm'], 'se': m.bse['oxp_dm'],
            'ci_lower': m.conf_int().loc['oxp_dm', 0],
            'ci_upper': m.conf_int().loc['oxp_dm', 1],
            'pvalue': m.pvalues['oxp_dm'], 'n': len(sub),
        })

subgroup_df = pd.DataFrame(subgroup_results)
subgroup_df.to_csv(os.path.join(OUTPUT_DIR, "tables", "corrected_subgroup_results.csv"), index=False)
print(f"  Saved subgroup results: {len(subgroup_df)} rows")

print("\n" + "=" * 70)
print("Corrected analysis completed.")
print("=" * 70)
