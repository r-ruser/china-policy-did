"""
08_cfps_basic_did.py
CFPS基础DID模型和事件研究
"""

import pandas as pd
import numpy as np
import os
from scipy import stats
import warnings
warnings.filterwarnings('ignore')

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "07_results")
os.makedirs(os.path.join(OUTPUT_DIR, "tables"), exist_ok=True)
os.makedirs(os.path.join(OUTPUT_DIR, "figures"), exist_ok=True)
os.makedirs(os.path.join(OUTPUT_DIR, "models"), exist_ok=True)

print("=" * 60)
print("CFPS Basic DID and Event Study")
print("=" * 60)

# ============================================================
# 1. Load data
# ============================================================
print("\n[1] Loading data...")

elderly = pd.read_parquet(os.path.join(PROJECT_ROOT, "05_analysis_data", "cfps_elderly_cohort.parquet"))
labor = pd.read_parquet(os.path.join(PROJECT_ROOT, "05_analysis_data", "cfps_labor_cohort.parquet"))

# Restrict to main analysis waves (exclude 2020 for confirmatory)
elderly_main = elderly[elderly['wave'].isin([2010, 2012, 2014, 2018])].copy()
labor_main = labor[labor['wave'].isin([2010, 2012, 2014, 2018])].copy()

print(f"  Elderly main: {len(elderly_main)} obs, {elderly_main['pid'].nunique()} individuals")
print(f"  Labor main: {len(labor_main)} obs, {labor_main['pid'].nunique()} individuals")

# ============================================================
# 2. Descriptive statistics
# ============================================================
print("\n[2] Descriptive statistics...")

def desc_table(df, outcome, label):
    """Generate descriptive stats by wave and treat"""
    rows = []
    for wave in [2010, 2012, 2014, 2018]:
        sub = df[df['wave'] == wave]
        treat = sub[sub['treat'] == 1]
        control = sub[sub['treat'] == 0]

        rows.append({
            'wave': wave,
            'N_total': len(sub),
            'N_treat': len(treat),
            'N_control': len(control),
            f'{label}_mean': sub[outcome].mean(),
            f'{label}_treat': treat[outcome].mean(),
            f'{label}_control': control[outcome].mean(),
            f'{label}_diff': treat[outcome].mean() - control[outcome].mean(),
        })
    return pd.DataFrame(rows)

# Elderly health
print("\n  Poor SRH by wave and treat:")
desc_srh = desc_table(elderly_main, 'poor_srh', 'poor_srh')
print(desc_srh.to_string(index=False))

# Labor
print("\n  Employment by wave and treat:")
desc_emp = desc_table(labor_main, 'employed', 'employed')
print(desc_emp.to_string(index=False))

# Save
desc_srh.to_csv(os.path.join(OUTPUT_DIR, "tables", "desc_srh_by_wave.csv"), index=False)
desc_emp.to_csv(os.path.join(OUTPUT_DIR, "tables", "desc_employment_by_wave.csv"), index=False)

# ============================================================
# 3. Raw trends plot data
# ============================================================
print("\n[3] Raw trends data...")

trends_srh = []
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = elderly[elderly['wave'] == wave]
    treat = sub[sub['treat'] == 1]
    control = sub[sub['treat'] == 0]
    trends_srh.append({
        'wave': wave,
        'treat_mean': treat['poor_srh'].mean(),
        'control_mean': control['poor_srh'].mean(),
        'n_treat': len(treat),
        'n_control': len(control),
    })

trends_emp = []
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = labor[labor['wave'] == wave]
    treat = sub[sub['treat'] == 1]
    control = sub[sub['treat'] == 0]
    trends_emp.append({
        'wave': wave,
        'treat_mean': treat['employed'].mean(),
        'control_mean': control['employed'].mean(),
        'n_treat': len(treat),
        'n_control': len(control),
    })

pd.DataFrame(trends_srh).to_csv(os.path.join(OUTPUT_DIR, "tables", "raw_trends_srh.csv"), index=False)
pd.DataFrame(trends_emp).to_csv(os.path.join(OUTPUT_DIR, "tables", "raw_trends_employment.csv"), index=False)

print("  SRH trends:")
for t in trends_srh:
    print(f"    {t['wave']}: treat={t['treat_mean']:.3f}, control={t['control_mean']:.3f}")

print("  Employment trends:")
for t in trends_emp:
    print(f"    {t['wave']}: treat={t['treat_mean']:.3f}, control={t['control_mean']:.3f}")

# ============================================================
# 4. Basic DID (OLS with fixed effects)
# ============================================================
print("\n[4] Basic DID models...")

# Use only main waves for confirmatory analysis
em = elderly_main.copy()
lm = labor_main.copy()

# Model 1: FE DID (no covariates)
# Y = β(Treat×Post) + α_i + λ_t + ε
# With individual FE, this is equivalent to first-differencing

def did_model_fe(df, outcome, cluster_var='cluster'):
    """Two-way FE DID using demeaning"""
    # Create dummy variables for waves (excluding reference)
    waves = sorted(df['wave'].unique())
    for w in waves:
        df[f'wave_{w}'] = (df['wave'] == w).astype(int)

    # Interactions
    for w in waves:
        df[f'treat_x_wave_{w}'] = df['treat'] * df[f'wave_{w}']

    # Post indicator
    df['post'] = (df['wave'] >= 2018).astype(int)
    df['did'] = df['treat'] * df['post']

    # For individual FE DID, we need to demean
    # Simple approach: include individual dummies via within transformation
    # Use statsmodels for now

    try:
        import statsmodels.api as sm
        from statsmodels.iolib.summary2 import summary_col

        # Within transformation (demean by individual)
        id_vars = ['pid']
        time_vars = [f'wave_{w}' for w in waves if w != 2014]  # exclude reference
        did_var = ['did']
        cluster_vars = [cluster_var]

        # Demean by individual
        all_vars = did_var + time_vars
        for var in all_vars + [outcome]:
            group_mean = df.groupby('pid')[var].transform('mean')
            df[f'{var}_dm'] = df[var] - group_mean

        # Also demean cluster variable for clustering
        # (can't demean cluster, use it as-is)

        # Run regression
        X = df[[f'{v}_dm' for v in all_vars]]
        X = sm.add_constant(X)
        y = df[f'{outcome}_dm']

        model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df[cluster_var]})

        return model
    except Exception as e:
        print(f"    Error: {e}")
        return None

print("\n  --- Elderly Health: Poor SRH ---")
model_srh = did_model_fe(em, 'poor_srh')
if model_srh:
    print(f"  DID coefficient: {model_srh.params.get('did_dm', 'N/A'):.4f}")
    print(f"  Std error: {model_srh.bse.get('did_dm', 'N/A'):.4f}")
    print(f"  P-value: {model_srh.pvalues.get('did_dm', 'N/A'):.4f}")
    print(f"  95% CI: [{model_srh.conf_int().loc['did_dm', 0]:.4f}, {model_srh.conf_int().loc['did_dm', 1]:.4f}]")
    print(f"  N: {int(model_srh.nobs)}")

print("\n  --- Labor Supply: Employment ---")
model_emp = did_model_fe(lm, 'employed')
if model_emp:
    print(f"  DID coefficient: {model_emp.params.get('did_dm', 'N/A'):.4f}")
    print(f"  Std error: {model_emp.bse.get('did_dm', 'N/A'):.4f}")
    print(f"  P-value: {model_emp.pvalues.get('did_dm', 'N/A'):.4f}")
    print(f"  95% CI: [{model_emp.conf_int().loc['did_dm', 0]:.4f}, {model_emp.conf_int().loc['did_dm', 1]:.4f}]")
    print(f"  N: {int(model_emp.nobs)}")

# ============================================================
# 5. Event Study
# ============================================================
print("\n[5] Event study...")

def event_study_fe(df, outcome, ref_year=2014):
    """Event study with individual FE"""
    waves = sorted(df['wave'].unique())

    # Create treat × wave interactions
    for w in waves:
        df[f'treat_x_{w}'] = (df['treat'] == 1) & (df['wave'] == w)
        if w == ref_year:
            df[f'treat_x_{w}'] = 0  # reference year

    interact_vars = [f'treat_x_{w}' for w in waves if w != ref_year]

    try:
        import statsmodels.api as sm

        # Demean by individual
        for var in interact_vars + [outcome]:
            group_mean = df.groupby('pid')[var].transform('mean')
            df[f'{var}_dm'] = df[var] - group_mean

        X = df[[f'{v}_dm' for v in interact_vars]]
        X = sm.add_constant(X)
        y = df[f'{outcome}_dm']

        model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['cluster']})

        # Extract coefficients
        results = []
        for w in waves:
            if w == ref_year:
                results.append({'year': w, 'coef': 0, 'se': 0, 'ci_lower': 0, 'ci_upper': 0, 'pvalue': np.nan})
            else:
                var = f'treat_x_{w}_dm'
                if var in model.params:
                    results.append({
                        'year': w,
                        'coef': model.params[var],
                        'se': model.bse[var],
                        'ci_lower': model.conf_int().loc[var, 0],
                        'ci_upper': model.conf_int().loc[var, 1],
                        'pvalue': model.pvalues[var],
                    })

        return pd.DataFrame(results), model
    except Exception as e:
        print(f"    Error: {e}")
        return None, None

print("\n  --- Event Study: Poor SRH ---")
es_srh, model_es_srh = event_study_fe(em.copy(), 'poor_srh')
if es_srh is not None:
    print(es_srh.to_string(index=False))
    es_srh.to_csv(os.path.join(OUTPUT_DIR, "tables", "event_study_srh.csv"), index=False)

    # Joint test of pre-treatment coefficients
    if model_es_srh:
        pre_vars = [f'treat_x_{w}_dm' for w in [2010, 2012]]
        pre_vars = [v for v in pre_vars if v in model_es_srh.params.index]
        if pre_vars:
            from scipy.stats import f as f_dist
            # Wald test
            R = np.zeros((len(pre_vars), len(model_es_srh.params)))
            for i, v in enumerate(pre_vars):
                j = list(model_es_srh.params.index).index(v)
                R[i, j] = 1
            f_stat = model_es_srh.f_test(R)
            print(f"\n  Joint pre-trend test: F={float(f_stat.fvalue):.3f}, p={float(f_stat.pvalue):.4f}")

print("\n  --- Event Study: Employment ---")
es_emp, model_es_emp = event_study_fe(lm.copy(), 'employed')
if es_emp is not None:
    print(es_emp.to_string(index=False))
    es_emp.to_csv(os.path.join(OUTPUT_DIR, "tables", "event_study_employment.csv"), index=False)

    if model_es_emp:
        pre_vars = [f'treat_x_{w}_dm' for w in [2010, 2012]]
        pre_vars = [v for v in pre_vars if v in model_es_emp.params.index]
        if pre_vars:
            R = np.zeros((len(pre_vars), len(model_es_emp.params)))
            for i, v in enumerate(pre_vars):
                j = list(model_es_emp.params.index).index(v)
                R[i, j] = 1
            f_stat = model_es_emp.f_test(R)
            print(f"\n  Joint pre-trend test: F={float(f_stat.fvalue):.3f}, p={float(f_stat.pvalue):.4f}")

# ============================================================
# 6. COVID-19 sensitivity (include 2020)
# ============================================================
print("\n[6] COVID-19 sensitivity (include 2020)...")

em_all = elderly.copy()
lm_all = labor.copy()

# Add 2020 indicator
em_all['covid_2020'] = (em_all['wave'] == 2020).astype(int)
em_all['did_covid'] = em_all['treat'] * em_all['covid_2020']

print("  Elderly sample with 2020:")
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = em_all[em_all['wave'] == wave]
    print(f"    {wave}: n={len(sub)}, treat={sub['treat'].sum()}, poor_srh={sub['poor_srh'].mean():.3f}")

print("  Labor sample with 2020:")
for wave in [2010, 2012, 2014, 2018, 2020]:
    sub = lm_all[lm_all['wave'] == wave]
    print(f"    {wave}: n={len(sub)}, treat={sub['treat'].sum()}, employed={sub['employed'].mean():.3f}")

# ============================================================
# 7. Sample size checks
# ============================================================
print("\n[7] Sample size checks per cluster...")

# Check treatment cluster sizes
cluster_sizes = elderly_main.groupby('cluster').agg(
    n_total=('pid', 'count'),
    n_treat=('treat', 'sum'),
    n_control=('treat', lambda x: len(x) - x.sum()),
).reset_index()

print("  Small clusters (n_total < 50):")
small = cluster_sizes[cluster_sizes['n_total'] < 50]
print(small.to_string(index=False) if len(small) > 0 else "  None")

print(f"\n  Total clusters: {len(cluster_sizes)}")
print(f"  Clusters with n >= 50: {(cluster_sizes['n_total'] >= 50).sum()}")
print(f"  Clusters with n < 50: {(cluster_sizes['n_total'] < 50).sum()}")

print("\n" + "=" * 60)
print("Basic DID and event study completed.")
print("=" * 60)
