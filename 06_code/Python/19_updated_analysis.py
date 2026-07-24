"""
19_updated_analysis.py
Updated: exclude baseline depression, add diabetes subgroup
"""
import pyreadstat
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CHARLS_PATH = "E:/公共数据库/7国老年数据库/CHARLS_China/H_CHARLS_D_Data.dta"

print("=" * 70)
print("UPDATED ANALYSIS: Exclude Baseline Depression + Diabetes Subgroup")
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

    # Covariates
    for var, fname in [('smoke', f'r{w}smokev'), ('drink', f'r{w}drinkev')]:
        if fname in sub.columns:
            sub[var] = sub[fname].astype(float)
        else:
            sub[var] = np.nan

    panel_rows.append(sub)

panel = pd.concat(panel_rows, ignore_index=True)
sample = panel[(panel['age_2015'] >= 50) & (panel['age_2015'] <= 69)].copy()
sample['older_2015'] = (sample['age_2015'] >= 60).astype(int)

# Baseline conditions from wave 3
w3 = sample[sample['wave'] == 3].copy()
for v in ['r3hibpe', 'r3hearte', 'r3stroke', 'r3lunge', 'r3diabe']:
    if v in w3.columns:
        w3[v.replace('r3', 'bl_')] = w3[v].astype(float)

bl_map = w3[['ID', 'bl_hibpe', 'bl_hearte', 'bl_stroke', 'bl_lunge', 'bl_diabe']].drop_duplicates()
sample = sample.merge(bl_map, on='ID', how='left', suffixes=('', '_bl'))
for v in ['bl_hibpe', 'bl_hearte', 'bl_stroke', 'bl_lunge', 'bl_diabe']:
    sample[v] = sample.groupby('ID')[v].transform(lambda x: x.ffill())

# Derived variables
sample['cvd_2015'] = ((sample['bl_hibpe'] == 1) | (sample['bl_hearte'] == 1) | (sample['bl_stroke'] == 1)).astype(float)

print(f"  Sample: {len(sample)} obs, {sample['ID'].nunique()} individuals")
print(f"  Baseline depression (CESD-10 wave3): {sample[sample['wave']==3]['cesd10'].mean():.2f}")
print(f"  Diabetes 2015: {sample['bl_diabe'].mean():.3f}")
print(f"  Hypertension 2015: {sample['bl_hibpe'].mean():.3f}")
print(f"  Heart disease 2015: {sample['bl_hearte'].mean():.3f}")
print(f"  Stroke 2015: {sample['bl_stroke'].mean():.3f}")
print(f"  Lung disease 2015: {sample['bl_lunge'].mean():.3f}")

# ============================================================
# 2. DID excluding baseline depression
# ============================================================
print("\n[2] DID excluding baseline depression (2015 vs 2018)...")
s2 = sample[sample['wave'].isin([3, 4])].copy()
s2['older_x_post'] = s2['older_2015'] * s2['post']

# Exclude individuals with baseline depression (CESD-10 >= 10 in wave 3)
bl_dep = sample[sample['wave'] == 3][['ID', 'cesd10']].copy()
bl_dep['dep_2015'] = (bl_dep['cesd10'] >= 10).astype(int)
dep_map = bl_dep[['ID', 'dep_2015']].drop_duplicates()
s2 = s2.merge(dep_map, on='ID', how='left')
s2_nodep = s2[s2['dep_2015'] == 0].copy()

print(f"  Excluded with baseline depression: {(s2['dep_2015'] == 1).sum()} obs")
print(f"  Remaining: {len(s2_nodep)} obs")

for outcome, label in [('any_adl', 'Any ADL'), ('cesd10', 'CESD-10'), ('orientation', 'Cognition')]:
    sub = s2_nodep.dropna(subset=[outcome]).copy()
    sub[outcome] = sub[outcome].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)
    sub['out_dm'] = sub.groupby('ID')[outcome].transform(lambda x: x - x.mean())
    sub['oxp_dm'] = sub.groupby('ID')['older_x_post'].transform(lambda x: x - x.mean())
    X = sm.add_constant(sub[['oxp_dm']].astype(float))
    y = sub['out_dm'].astype(float)
    m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
    print(f"\n  {label} (no baseline depression, n={len(sub)}):")
    print(f"    Older×Post: {m.params['oxp_dm']:.4f} (SE={m.bse['oxp_dm']:.4f}, p={m.pvalues['oxp_dm']:.4f})")

# ============================================================
# 3. CVD subgroups
# ============================================================
print("\n[3] CVD Subgroup Analysis...")
s3 = s2_nodep.copy()

for cvd_var, cvd_label in [('cvd_2015', 'Any CVD'), ('bl_hibpe', 'Hypertension'),
                             ('bl_hearte', 'Heart Disease'), ('bl_stroke', 'Stroke'),
                             ('bl_diabe', 'Diabetes'), ('bl_lunge', 'Lung Disease')]:
    print(f"\n  --- {cvd_label} ---")
    for sg_val, sg_name in [(1, 'Has'), (0, 'No')]:
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
            print(f"    {sg_name:>3} {out:>8}: {m.params['oxp_dm']:>7.4f} (SE={m.bse['oxp_dm']:.4f}, p={m.pvalues['oxp_dm']:.4f})")

# ============================================================
# 4. DDD with each condition
# ============================================================
print("\n[4] DDD: Older×Post×Condition...")
s4 = s2_nodep.copy()

for cond_var, cond_label in [('cvd_2015', 'CVD'), ('bl_diabe', 'Diabetes'),
                               ('bl_hearte', 'Heart Disease'), ('bl_hibpe', 'Hypertension')]:
    sub = s4.dropna(subset=['any_adl', cond_var]).copy()
    sub['older_post'] = sub['older_2015'] * sub['post']
    sub['older_cond'] = sub['older_2015'] * sub[cond_var]
    sub['post_cond'] = sub['post'] * sub[cond_var]
    sub['ddd'] = sub['older_2015'] * sub['post'] * sub[cond_var]
    for v in ['ddd', 'older_post', 'older_cond', 'post_cond', 'any_adl']:
        sub[f'{v}_dm'] = sub.groupby('ID')[v].transform(lambda x: x - x.mean())
    X = sm.add_constant(sub[['ddd_dm', 'older_post_dm', 'older_cond_dm', 'post_cond_dm']].astype(float))
    y = sub['any_adl_dm'].astype(float)
    m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub['ID']})
    print(f"\n  DDD {cond_label} → ADL (n={len(sub)}):")
    print(f"    DDD: {m.params['ddd_dm']:.4f} (SE={m.bse['ddd_dm']:.4f}, p={m.pvalues['ddd_dm']:.4f})")

    # CESD
    sub_c = s4.dropna(subset=['cesd10', cond_var]).copy()
    sub_c['older_post'] = sub_c['older_2015'] * sub_c['post']
    sub_c['older_cond'] = sub_c['older_2015'] * sub_c[cond_var]
    sub_c['post_cond'] = sub_c['post'] * sub_c[cond_var]
    sub_c['ddd'] = sub_c['older_2015'] * sub_c['post'] * sub_c[cond_var]
    for v in ['ddd', 'older_post', 'older_cond', 'post_cond', 'cesd10']:
        sub_c[f'{v}_dm'] = sub_c.groupby('ID')[v].transform(lambda x: x - x.mean())
    X = sm.add_constant(sub_c[['ddd_dm', 'older_post_dm', 'older_cond_dm', 'post_cond_dm']].astype(float))
    y = sub_c['cesd10_dm'].astype(float)
    m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub_c['ID']})
    print(f"    DDD: {m.params['ddd_dm']:.4f} (SE={m.bse['ddd_dm']:.4f}, p={m.pvalues['ddd_dm']:.4f})")

# ============================================================
# 5. Event study excluding baseline depression
# ============================================================
print("\n[5] Event study (no baseline depression)...")
ref_wave = 3
for outcome, label in [('any_adl', 'Any ADL'), ('cesd10', 'CESD-10')]:
    df_es = sample.dropna(subset=[outcome]).copy()
    df_es = df_es.merge(dep_map, on='ID', how='left')
    df_es = df_es[df_es['dep_2015'] == 0].copy()

    for w in [1, 2, 3, 4]:
        df_es[f'ow_{w}'] = ((df_es['older_2015'] == 1) & (df_es['wave'] == w)).astype(float)
        if w == ref_wave:
            df_es[f'ow_{w}'] = 0

    interact_vars = [f'ow_{w}' for w in [1, 2, 4]]
    for var in interact_vars + [outcome]:
        df_es[var] = df_es[var].astype(float)
        df_es[f'{var}_dm'] = df_es.groupby('ID')[var].transform(lambda x: x - x.mean())

    X = sm.add_constant(df_es[[f'{v}_dm' for v in interact_vars]].astype(float))
    y = df_es[f'{outcome}_dm'].astype(float)
    m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df_es['ID']})

    print(f"\n  {label} Event Study (no baseline depression):")
    for w, yr in [(1, '2011'), (2, '2013'), (4, '2018')]:
        var = f'ow_{w}_dm'
        coef = m.params[var]
        se = m.bse[var]
        p = m.pvalues[var]
        print(f"    {yr}: {coef:.4f} (SE={se:.4f}, p={p:.4f})")
    print(f"    2015: 0.0000 (reference)")

print("\n" + "=" * 70)
print("Updated analysis completed.")
print("=" * 70)
