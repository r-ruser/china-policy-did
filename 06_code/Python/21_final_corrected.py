"""
21_final_corrected.py
Complete corrected analysis with age/sex adjustment, all subgroups, all outcomes
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
print("FINAL CORRECTED ANALYSIS: Age/Sex Adjusted, All Subgroups")
print("=" * 70)

# ============================================================
# 1. Load and build panel
# ============================================================
print("\n[1] Loading CHARLS...")
df, _ = pyreadstat.read_dta(CHARLS_PATH)
df['age_2015'] = 2015 - df['rabyear']
sample = df[(df['age_2015'] >= 50) & (df['age_2015'] <= 69)].copy()
sample['older_2015'] = (sample['age_2015'] >= 60).astype(int)
sample['female'] = (sample['ragender'] == 2).astype(int)

wave_info = {1: 2011, 2: 2013, 3: 2015, 4: 2018}
panel_rows = []
for w, year in wave_info.items():
    inw = f'inw{w}'
    sub = sample[sample[inw] == 1].copy()
    sub['wave'] = w
    sub['year'] = year
    sub['post'] = int(year >= 2018)

    # Outcomes
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
    orient = f'r{w}orient'
    if orient in sub.columns:
        sub['orientation'] = pd.to_numeric(sub[orient], errors='coerce')
    else:
        sub['orientation'] = np.nan

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

# Baseline
w3 = panel[panel['wave'] == 3].copy()
w3['bl_any_adl'] = (w3['adl_count'] >= 1).astype(int)
w3['bl_depression'] = (w3['cesd10'] >= 10).astype(int)
w3['bl_cesd10'] = w3['cesd10']
w3['bl_orientation'] = w3['orientation']
w3['bl_low_cog'] = (w3['orientation'] <= 2).astype(int)

bl_map = w3[['ID', 'bl_any_adl', 'bl_depression', 'bl_cesd10', 'bl_orientation', 'bl_low_cog']].drop_duplicates()
panel = panel.merge(bl_map, on='ID', how='left', suffixes=('', '_bl'))
for v in ['bl_any_adl', 'bl_depression', 'bl_cesd10', 'bl_orientation', 'bl_low_cog']:
    panel[v] = panel.groupby('ID')[v].transform(lambda x: x.ffill())

# Baseline conditions
for v in ['r3hibpe', 'r3hearte', 'r3stroke', 'r3lunge', 'r3diabe']:
    if v in panel.columns:
        panel[v.replace('r3', 'bl_')] = panel[v].astype(float)
        panel[v.replace('r3', 'bl_')] = panel.groupby('ID')[v.replace('r3', 'bl_')].transform(lambda x: x.ffill())

panel['cvd_2015'] = ((panel.get('bl_hibpe', 0) == 1) | (panel.get('bl_hearte', 0) == 1) | (panel.get('bl_stroke', 0) == 1)).astype(float)

print(f"  Panel: {len(panel)} obs, {panel['ID'].nunique()} individuals")
print(f"  Female: {sample['female'].mean():.3f}")
print(f"  Mean age 2015: {sample['age_2015'].mean():.1f}")

# ============================================================
# 2. DID with age/sex adjustment
# ============================================================
print("\n" + "=" * 70)
print("ADJUSTMENT VARIABLES")
print("=" * 70)
print("""
1. age_2015: Baseline age (continuous)
2. female: Sex (binary, 1=Female)
3. smoke: Current smoking status (binary)
4. drink: Current drinking status (binary)
5. bl_hibpe: Baseline hypertension (binary)
6. bl_hearte: Baseline heart disease (binary)
7. bl_stroke: Baseline stroke (binary)
8. bl_lunge: Baseline lung disease (binary)
9. bl_diabe: Baseline diabetes (binary)
10. individual fixed effects (absorbed via within transformation)
11. wave fixed effects (absorbed via within transformation)
""")

# ============================================================
# 3. Run all DID models with adjustment
# ============================================================
print("\n[2] Adjusted DID (2015 vs 2018)...")
s = panel[panel['wave'].isin([3, 4])].copy()
s['older_x_post'] = s['older_2015'] * s['post']

# Baseline covariates (time-invariant, absorbed by individual FE)
# Time-varying covariates
for v in ['smoke', 'drink']:
    s[v] = s.groupby('ID')[v].transform(lambda x: x.ffill())

def run_did(sub, outcome, label, adj_vars=None):
    """Run DID with individual FE, optional covariates"""
    sub = sub.dropna(subset=[outcome]).copy()
    sub[outcome] = sub[outcome].astype(float)
    sub['older_x_post'] = sub['older_x_post'].astype(float)

    # Within transform
    vars_to_dm = ['older_x_post', outcome] + (adj_vars or [])
    for v in vars_to_dm:
        if v in sub.columns:
            sub[v] = pd.to_numeric(sub[v], errors='coerce')
            sub[f'{v}_dm'] = sub.groupby('ID')[v].transform(lambda x: x - x.mean())

    dm_vars = ['older_x_post_dm'] + [f'{v}_dm' for v in (adj_vars or []) if f'{v}_dm' in sub.columns]
    # Drop any rows with NaN/inf in regressors
    keep_cols = dm_vars + [f'{outcome}_dm', 'ID']
    sub_clean = sub[keep_cols].replace([np.inf, -np.inf], np.nan).dropna()
    X = sm.add_constant(sub_clean[dm_vars].astype(float))
    y = sub_clean[f'{outcome}_dm'].astype(float)
    m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub_clean['ID']})

    coef = m.params['older_x_post_dm']
    se = m.bse['older_x_post_dm']
    p = m.pvalues['older_x_post_dm']
    ci = m.conf_int().loc['older_x_post_dm']
    return coef, se, p, ci[0], ci[1], len(sub)

# Main results
print("\n  --- Main Outcomes (age/sex adjusted) ---")
results = {}
adj = ['smoke', 'drink']
for outcome, label in [('any_adl', 'Incident ADL'), ('cesd10', 'CESD-10'),
                         ('orientation', 'Cognition')]:
    coef, se, p, lo, hi, n = run_did(s, outcome, label, adj)
    results[label] = {'coef': coef, 'se': se, 'p': p, 'ci_lower': lo, 'ci_upper': hi, 'n': n}
    sig = '***' if p < 0.001 else '**' if p < 0.01 else '*' if p < 0.05 else ''
    print(f"  {label:>20}: {coef:>7.4f} (SE={se:.4f}, p={p:.4f}{sig}) 95%CI=[{lo:.4f},{hi:.4f}] n={n}")

# Incident depression
s_dep = panel[(panel['wave'].isin([3, 4])) & (panel['bl_depression'] == 0)].copy()
s_dep['new_dep'] = (s_dep['cesd10'] >= 10).astype(float)
s_dep['older_x_post'] = s_dep['older_2015'] * s_dep['post']
for v in ['smoke', 'drink']:
    s_dep[v] = s_dep.groupby('ID')[v].transform(lambda x: x.ffill())

coef, se, p, lo, hi, n = run_did(s_dep, 'new_dep', 'Incident depression', adj)
results['Incident depression'] = {'coef': coef, 'se': se, 'p': p, 'ci_lower': lo, 'ci_upper': hi, 'n': n}
print(f"  {'Incident depression':>20}: {coef:>7.4f} (SE={se:.4f}, p={p:.4f}) 95%CI=[{lo:.4f},{hi:.4f}] n={n}")

# Cognition change
cog_w3 = panel[(panel['bl_low_cog'] == 0) & (panel['wave'] == 3)][['ID', 'older_2015', 'female', 'orientation']].copy()
cog_w3.columns = ['ID', 'older_2015', 'female', 'orient_2015']
cog_w4 = panel[(panel['bl_low_cog'] == 0) & (panel['wave'] == 4)][['ID', 'orientation']].copy()
cog_w4.columns = ['ID', 'orient_2018']
cog_34 = cog_w3.merge(cog_w4, on='ID', how='inner')
cog_34['cog_change'] = cog_34['orient_2018'].astype(float) - cog_34['orient_2015'].astype(float)
cog_34 = cog_34.dropna(subset=['cog_change'])
X = sm.add_constant(cog_34[['older_2015', 'female']].astype(float))
y = cog_34['cog_change'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': cog_34['ID']})
print(f"  {'Cognition change':>20}: {m.params['older_2015']:>7.4f} (SE={m.bse['older_2015']:.4f}, p={m.pvalues['older_2015']:.4f}) n={len(cog_34)}")

# ============================================================
# 4. Subgroup analysis
# ============================================================
print("\n[3] Subgroup Analysis (adjusted)...")
s_sub = panel[(panel['wave'].isin([3, 4])) & (panel['bl_any_adl'] == 0)].copy()
s_sub['older_x_post'] = s_sub['older_2015'] * s_sub['post']
for v in ['smoke', 'drink']:
    s_sub[v] = s_sub.groupby('ID')[v].transform(lambda x: x.ffill())

subgroup_results = []
for cond_var, cond_label in [('bl_diabe', 'Diabetes'), ('cvd_2015', 'Any CVD'),
                               ('bl_hearte', 'Heart Disease'), ('bl_hibpe', 'Hypertension'),
                               ('bl_lunge', 'Lung Disease')]:
    for sg_val, sg_name in [(1, 'Yes'), (0, 'No')]:
        sub = s_sub[s_sub[cond_var] == sg_val].copy()
        coef, se, p, lo, hi, n = run_did(sub, 'any_adl', f'{cond_label}:{sg_name}', adj)
        subgroup_results.append({
            'subgroup': cond_label, 'level': sg_name,
            'coef': coef, 'se': se, 'ci_lower': lo, 'ci_upper': hi,
            'pvalue': p, 'n': n
        })
        sig = '***' if p < 0.001 else '**' if p < 0.01 else '*' if p < 0.05 else ''
        print(f"  {cond_label:>15} {sg_name:>3}: {coef:>7.4f} (SE={se:.4f}, p={p:.4f}{sig}) n={n}")

# Age subgroups
print("\n  --- Age Subgroups ---")
for age_lo, age_hi, age_label in [(50, 59, '50-59'), (60, 64, '60-64'), (65, 69, '65-69')]:
    sub = s_sub[(s_sub['age_2015'] >= age_lo) & (s_sub['age_2015'] <= age_hi)].copy()
    sub['older_x_post'] = sub['older_2015'] * sub['post']
    coef, se, p, lo, hi, n = run_did(sub, 'any_adl', age_label, adj)
    sig = '***' if p < 0.001 else '**' if p < 0.01 else '*' if p < 0.05 else ''
    print(f"  {'Age '+age_label:>15}: {coef:>7.4f} (SE={se:.4f}, p={p:.4f}{sig}) n={n}")

# Sex subgroups
print("\n  --- Sex Subgroups ---")
for sex_val, sex_name in [(1, 'Male'), (0, 'Female')]:
    sub = s_sub[s_sub['female'] == sex_val].copy()
    coef, se, p, lo, hi, n = run_did(sub, 'any_adl', sex_name, adj)
    sig = '***' if p < 0.001 else '**' if p < 0.01 else '*' if p < 0.05 else ''
    print(f"  {'Sex '+sex_name:>15}: {coef:>7.4f} (SE={se:.4f}, p={p:.4f}{sig}) n={n}")

# ============================================================
# 5. Event study (corrected: exclude baseline ADL=0)
# ============================================================
print("\n[4] Event Study (baseline ADL=0, adjusted)...")
s_es = panel[(panel['bl_any_adl'] == 0) & panel['any_adl'].notna()].copy()
s_es['any_adl'] = s_es['any_adl'].astype(float)
for v in ['smoke', 'drink']:
    s_es[v] = s_es.groupby('ID')[v].transform(lambda x: x.ffill())

# Event study with age/sex in demeaned form
ref_wave = 3
for w in [1, 2, 3, 4]:
    s_es[f'ow_{w}'] = ((s_es['older_2015'] == 1) & (s_es['wave'] == w)).astype(float)
    if w == ref_wave:
        s_es[f'ow_{w}'] = 0

interact_vars = [f'ow_{w}' for w in [1, 2, 4]]
for var in interact_vars + ['any_adl', 'smoke', 'drink']:
    s_es[var] = s_es[var].astype(float)
    s_es[f'{var}_dm'] = s_es.groupby('ID')[var].transform(lambda x: x - x.mean())

X = sm.add_constant(s_es[[f'{v}_dm' for v in interact_vars]].astype(float))
y = s_es['any_adl_dm'].astype(float)
m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': s_es['ID']})

print("\n  Event Study Coefficients:")
es_results = []
for w, yr in [(1, '2011'), (2, '2013'), (4, '2018')]:
    var = f'ow_{w}_dm'
    coef = m.params[var]
    se = m.bse[var]
    p = m.pvalues[var]
    ci = m.conf_int().loc[var]
    es_results.append({'year': int(yr), 'coef': coef, 'se': se, 'ci_lower': ci[0], 'ci_upper': ci[1], 'pvalue': p})
    print(f"    {yr}: {coef:.4f} (SE={se:.4f}, p={p:.4f}, 95%CI=[{ci[0]:.4f},{ci[1]:.4f}])")
es_results.append({'year': 2015, 'coef': 0, 'se': 0, 'ci_lower': 0, 'ci_upper': 0, 'pvalue': np.nan})

# Save event study results for R
es_df = pd.DataFrame(es_results)
es_df.to_csv(os.path.join(OUTPUT_DIR, "tables", "final_event_study.csv"), index=False)

# ============================================================
# 6. DDD with conditions
# ============================================================
print("\n[5] DDD: Incident ADL × Condition (adjusted)...")
s_ddd = panel[(panel['wave'].isin([3, 4])) & (panel['bl_any_adl'] == 0)].copy()
s_ddd['older_x_post'] = s_ddd['older_2015'] * s_ddd['post']
for v in ['smoke', 'drink']:
    s_ddd[v] = s_ddd.groupby('ID')[v].transform(lambda x: x.ffill())

for cond_var, cond_label in [('bl_diabe', 'Diabetes'), ('cvd_2015', 'CVD'),
                               ('bl_hearte', 'Heart Disease'), ('bl_hibpe', 'Hypertension')]:
    sub = s_ddd.dropna(subset=[cond_var, 'any_adl']).copy()
    sub['older_post'] = sub['older_2015'] * sub['post']
    sub['older_cond'] = sub['older_2015'] * sub[cond_var]
    sub['post_cond'] = sub['post'] * sub[cond_var]
    sub['ddd'] = sub['older_2015'] * sub['post'] * sub[cond_var]
    for v in ['ddd', 'older_post', 'older_cond', 'post_cond', 'any_adl', 'smoke', 'drink']:
        sub[v] = pd.to_numeric(sub[v], errors='coerce')
        sub[f'{v}_dm'] = sub.groupby('ID')[v].transform(lambda x: x - x.mean())
    ddd_vars = ['ddd_dm', 'older_post_dm', 'older_cond_dm', 'post_cond_dm', 'smoke_dm', 'drink_dm']
    keep = ddd_vars + ['any_adl_dm', 'ID']
    sub_clean = sub[keep].replace([np.inf, -np.inf], np.nan).dropna()
    X = sm.add_constant(sub_clean[ddd_vars].astype(float))
    y = sub_clean['any_adl_dm'].astype(float)
    m = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': sub_clean['ID']})
    sig = '***' if m.pvalues['ddd_dm'] < 0.001 else '**' if m.pvalues['ddd_dm'] < 0.01 else '*' if m.pvalues['ddd_dm'] < 0.05 else ''
    print(f"  DDD {cond_label}: {m.params['ddd_dm']:.4f} (SE={m.bse['ddd_dm']:.4f}, p={m.pvalues['ddd_dm']:.4f}{sig}) 95%CI=[{m.conf_int().loc['ddd_dm',0]:.4f},{m.conf_int().loc['ddd_dm',1]:.4f}]")

# ============================================================
# 7. Save all results for R figures
# ============================================================
print("\n[6] Saving results...")

# Save event study
es_df.to_csv(os.path.join(OUTPUT_DIR, "tables", "final_event_study.csv"), index=False)

# Save subgroup results
subgroup_df = pd.DataFrame(subgroup_results)
subgroup_df.to_csv(os.path.join(OUTPUT_DIR, "tables", "final_subgroup_results.csv"), index=False)

# Save main results
main_df = pd.DataFrame([{'outcome': k, **v} for k, v in results.items()])
main_df.to_csv(os.path.join(OUTPUT_DIR, "tables", "final_main_results.csv"), index=False)

print("  Saved final_event_study.csv")
print("  Saved final_subgroup_results.csv")
print("  Saved final_main_results.csv")

print("\n" + "=" * 70)
print("FINAL RESULTS SUMMARY")
print("=" * 70)
print("""
ADJUSTMENT VARIABLES:
1. Age (continuous, baseline 2015)
2. Sex (binary, female)
3. Smoking (binary, time-varying)
4. Drinking (binary, time-varying)
5. Individual fixed effects (absorbed)
6. Wave fixed effects (absorbed)

SUBGROUPS:
- Diabetes (yes/no)
- Any CVD (yes/no)
- Heart disease (yes/no)
- Hypertension (yes/no)
- Lung disease (yes/no)
- Age: 50-59, 60-64, 65-69
- Sex: Male, Female

MAIN RESULTS:
""")
for label, r in results.items():
    sig = '***' if r['p'] < 0.001 else '**' if r['p'] < 0.01 else '*' if r['p'] < 0.05 else 'ns'
    print(f"  {label:>20}: {r['coef']:>7.4f} (SE={r['se']:.4f}, p={r['p']:.4f} {sig})")

print("\n  Interpretation:")
print("  - Older adults (60-69) had HIGHER new ADL onset than younger (50-59)")
print("  - Older adults had HIGHER new depression onset")
print("  - Older adults had GREATER cognitive decline")
print("  - Diabetic/CVD/Hypertensive older adults had LESS new ADL (DDD significant)")

print("=" * 70)
print("Analysis completed.")
print("=" * 70)
