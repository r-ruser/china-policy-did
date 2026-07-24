"""
12_full_diagnostics.py
Full diagnostic analysis: SRH (2012-2014-2018), DDD, balancing, synthetic DID
"""
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm
from scipy.spatial.distance import cdist

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "07_results")
os.makedirs(os.path.join(OUTPUT_DIR, "tables"), exist_ok=True)
os.makedirs(os.path.join(OUTPUT_DIR, "figures"), exist_ok=True)

print("=" * 70)
print("FULL DIAGNOSTIC ANALYSIS")
print("=" * 70)

# ============================================================
# LOAD DATA
# ============================================================
print("\n[0] Loading data...")
elderly = pd.read_parquet(os.path.join(PROJECT_ROOT, "05_analysis_data", "cfps_elderly_cohort.parquet"))
labor = pd.read_parquet(os.path.join(PROJECT_ROOT, "05_analysis_data", "cfps_labor_cohort.parquet"))
baseline = pd.read_parquet(os.path.join(PROJECT_ROOT, "05_analysis_data", "cfps_2014_baseline.parquet"))

# ============================================================
# PART A: SRH ANALYSIS (2012-2014-2018, exclude 2010)
# ============================================================
print("\n" + "=" * 70)
print("PART A: SRH ANALYSIS (2012, 2014, 2018)")
print("=" * 70)

# A1. SRH trends 2012-2014-2018
print("\n[A1] SRH trends (2012-2014-2018)...")
em = elderly[elderly['wave'].isin([2012, 2014, 2018])].copy()
em['post'] = (em['wave'] >= 2018).astype(int)

print(f"\n  Sample: {len(em)} obs, {em['pid'].nunique()} individuals")
for wave in [2012, 2014, 2018]:
    for t in [0, 1]:
        sub = em[(em['wave'] == wave) & (em['treat'] == t)]
        label = "Treat" if t == 1 else "Control"
        n = len(sub)
        srh = sub['srh'].mean()
        poor = sub['poor_srh'].mean()
        se = np.sqrt(poor * (1 - poor) / n) if n > 0 else 0
        print(f"  {wave} {label:>8}: n={n:>5}, srh={srh:.3f}, poor_srh={poor:.3f} [{poor-1.96*se:.3f},{poor+1.96*se:.3f}]")

# A2. Basic DID (2012-2014-2018)
print("\n[A2] Basic DID (2012-2014-2018)...")
em['did'] = em['treat'] * em['post']

for outcome, label in [('poor_srh', 'poor_srh'), ('srh', 'srh_score')]:
    df = em.copy()
    df['did'] = df['treat'] * df['post']
    X = sm.add_constant(df[['did', 'post', 'treat']])
    y = df[outcome].astype(float)
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})
    print(f"\n  {label} (city-clustered):")
    print(f"    DID: {model.params['did']:.4f} (SE={model.bse['did']:.4f}, p={model.pvalues['did']:.4f})")
    print(f"    95% CI: [{model.conf_int().loc['did', 0]:.4f}, {model.conf_int().loc['did', 1]:.4f}]")

# A3. Event study (2012-2014-2018)
print("\n[A3] Event study (2012-2014-2018)...")
for outcome, label in [('poor_srh', 'poor_srh'), ('srh', 'srh_score')]:
    df = em.copy()
    ref_year = 2014
    for w in [2012, 2014, 2018]:
        df[f'tx_{w}'] = ((df['treat'] == 1) & (df['wave'] == w)).astype(int)
        if w == ref_year:
            df[f'tx_{w}'] = 0
    interact_vars = ['tx_2012', 'tx_2018']
    for var in interact_vars + [outcome]:
        df[f'{var}_dm'] = df.groupby('pid')[var].transform(lambda x: x - x.mean())
    X = sm.add_constant(df[[f'{v}_dm' for v in interact_vars]])
    y = df[f'{outcome}_dm']
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})
    print(f"\n  {label} Event Study:")
    print(f"    2012: {model.params['tx_2012_dm']:.4f} (SE={model.bse['tx_2012_dm']:.4f}, p={model.pvalues['tx_2012_dm']:.4f})")
    print(f"    2014: 0.0000 (reference)")
    print(f"    2018: {model.params['tx_2018_dm']:.4f} (SE={model.bse['tx_2018_dm']:.4f}, p={model.pvalues['tx_2018_dm']:.4f})")
    # Pre-trend test: test tx_2012_dm = 0
    from scipy.stats import t as t_dist
    t_stat = model.params['tx_2012_dm'] / model.bse['tx_2012_dm']
    p_val = 2 * t_dist.sf(abs(t_stat), df=model.df_resid)
    print(f"    Pre-trend (2012): t={t_stat:.3f}, p={p_val:.4f}")

# ============================================================
# PART B: ELDERLY HEALTH DDD
# ============================================================
print("\n" + "=" * 70)
print("PART B: ELDERLY HEALTH DDD (2012-2014-2018)")
print("=" * 70)

# B1. Define HighNeed
print("\n[B1] Defining HighNeed from 2014 baseline...")
em_bl = em[em['wave'] == 2014].copy()

# a. Age >= 75
em_bl['highneed_age75'] = (em_bl['baseline_age'] >= 75).astype(int)

# b. Chronic disease >= 2 (use dw as proxy if chronic not available)
# Check if chronic disease variable exists
chronic_cols = [c for c in em_bl.columns if 'chronic' in c.lower() or 'disease' in c.lower()]
print(f"  Chronic disease columns: {chronic_cols}")

# Use activity limitation as proxy for high need
em_bl['highneed_dw'] = (em_bl['baseline_dw'] <= 2).astype(int)  # 1=limited

# Combine: highneed = age>=75 OR activity limited
em_bl['highneed_combined'] = ((em_bl['baseline_age'] >= 75) | (em_bl['highneed_dw'] == 1)).astype(int)

print(f"  highneed_age75: {em_bl['highneed_age75'].mean():.3f}")
print(f"  highneed_dw: {em_bl['highneed_dw'].mean():.3f}")
print(f"  highneed_combined: {em_bl['highneed_combined'].mean():.3f}")

# Merge back to panel
highneed_map = em_bl[['pid', 'highneed_age75', 'highneed_dw', 'highneed_combined']].drop_duplicates()
em_ddd = em.merge(highneed_map, on='pid', how='left')

# B2. DDD models
print("\n[B2] DDD models...")
for hn_var, hn_label in [('highneed_age75', 'Age>=75'), ('highneed_dw', 'ActivityLimited'), ('highneed_combined', 'Combined')]:
    df = em_ddd.copy()
    df['did'] = df['treat'] * df['post']
    df['did_hn'] = df['did'] * df[hn_var]
    df['treat_hn'] = df['treat'] * df[hn_var]
    df['post_hn'] = df['post'] * df[hn_var]

    X = sm.add_constant(df[['did', 'did_hn', 'treat', 'post', hn_var, 'treat_hn', 'post_hn']])
    y = df['poor_srh']
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})

    print(f"\n  DDD with {hn_label}:")
    print(f"    DDD (did_hn): {model.params['did_hn']:.4f} (SE={model.bse['did_hn']:.4f}, p={model.pvalues['did_hn']:.4f})")
    print(f"    95% CI: [{model.conf_int().loc['did_hn', 0]:.4f}, {model.conf_int().loc['did_hn', 1]:.4f}]")
    print(f"    DID (did): {model.params['did']:.4f} (SE={model.bse['did']:.4f}, p={model.pvalues['did']:.4f})")

# B3. DDD event study
print("\n[B3] DDD event study (highneed_combined)...")
df = em_ddd.copy()
hn_var = 'highneed_combined'
for w in [2012, 2014, 2018]:
    df[f'tx_{w}'] = ((df['treat'] == 1) & (df['wave'] == w)).astype(int)
    if w == 2014:
        df[f'tx_{w}'] = 0
    df[f'tx_hn_{w}'] = df[f'tx_{w}'] * df[hn_var]

# Demean
for var in ['tx_2012', 'tx_2018', 'tx_hn_2012', 'tx_hn_2018', 'poor_srh']:
    df[f'{var}_dm'] = df.groupby('pid')[var].transform(lambda x: x - x.mean())

X = sm.add_constant(df[['tx_2012_dm', 'tx_2018_dm', 'tx_hn_2012_dm', 'tx_hn_2018_dm']])
y = df['poor_srh_dm']
model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})

print(f"  DDD Event Study (Treat x Post x HighNeed):")
for var, yr in [('tx_hn_2012_dm', '2012'), ('tx_hn_2018_dm', '2018')]:
    coef = model.params[var]
    se = model.bse[var]
    p = model.pvalues[var]
    ci = model.conf_int().loc[var]
    print(f"    {yr}: {coef:.4f} (SE={se:.4f}, p={p:.4f}, 95CI=[{ci[0]:.4f},{ci[1]:.4f}])")

# ============================================================
# PART C: LABOR SUPPLY DDD
# ============================================================
print("\n" + "=" * 70)
print("PART C: LABOR SUPPLY DDD (2010-2012-2014-2018)")
print("=" * 70)

lm = labor[labor['wave'].isin([2010, 2012, 2014, 2018])].copy()
lm['post'] = (lm['wave'] >= 2018).astype(int)

# Merge older_household_2014
lm_ddd = lm.merge(baseline[['pid', 'older_household_2014']], on='pid', how='left', suffixes=('', '_bl'))

print(f"\n  Sample: {len(lm_ddd)} obs, {lm_ddd['pid'].nunique()} individuals")
print(f"  Older household 2014: {lm_ddd['older_household_2014'].mean():.3f}")

# DDD model
df = lm_ddd.copy()
df['did'] = df['treat'] * df['post']
df['did_oh'] = df['did'] * df['older_household_2014']
df['treat_oh'] = df['treat'] * df['older_household_2014']
df['post_oh'] = df['post'] * df['older_household_2014']

X = sm.add_constant(df[['did', 'did_oh', 'treat', 'post', 'older_household_2014', 'treat_oh', 'post_oh']])
y = df['employed']
model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})

print(f"\n  Labor DDD (Treat x Post x OlderHousehold):")
print(f"    DDD (did_oh): {model.params['did_oh']:.4f} (SE={model.bse['did_oh']:.4f}, p={model.pvalues['did_oh']:.4f})")
print(f"    95% CI: [{model.conf_int().loc['did_oh', 0]:.4f}, {model.conf_int().loc['did_oh', 1]:.4f}]")
print(f"    DID (did): {model.params['did']:.4f} (SE={model.bse['did']:.4f}, p={model.pvalues['did']:.4f})")

# DDD event study
print(f"\n  Labor DDD Event Study:")
df2 = lm_ddd.copy()
for w in [2010, 2012, 2014, 2018]:
    df2[f'tx_{w}'] = ((df2['treat'] == 1) & (df2['wave'] == w)).astype(int)
    if w == 2014:
        df2[f'tx_{w}'] = 0
    df2[f'tx_oh_{w}'] = df2[f'tx_{w}'] * df2['older_household_2014']

for var in ['tx_2010', 'tx_2012', 'tx_2018', 'tx_oh_2010', 'tx_oh_2012', 'tx_oh_2018', 'employed']:
    df2[f'{var}_dm'] = df2.groupby('pid')[var].transform(lambda x: x - x.mean())

X = sm.add_constant(df2[['tx_2010_dm', 'tx_2012_dm', 'tx_2018_dm', 'tx_oh_2010_dm', 'tx_oh_2012_dm', 'tx_oh_2018_dm']])
y = df2['employed_dm']
model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df2['admin_code']})

for var, yr in [('tx_oh_2010_dm', '2010'), ('tx_oh_2012_dm', '2012'), ('tx_oh_2018_dm', '2018')]:
    coef = model.params[var]
    se = model.bse[var]
    p = model.pvalues[var]
    ci = model.conf_int().loc[var]
    print(f"    DDD {yr}: {coef:.4f} (SE={se:.4f}, p={p:.4f}, 95CI=[{ci[0]:.4f},{ci[1]:.4f}])")

# ============================================================
# PART D: PRE-TREND BALANCING
# ============================================================
print("\n" + "=" * 70)
print("PART D: PRE-TREND BALANCING AT POLICY UNIT LEVEL")
print("=" * 70)

# Compute city-level baseline characteristics
em12 = elderly[elderly['wave'] == 2012].copy()
em14 = elderly[elderly['wave'] == 2014].copy()

city_base = em14.groupby('admin_code').agg(
    treat=('treat', 'first'),
    poor_srh_2014=('poor_srh', 'mean'),
    srh_2014=('srh', 'mean'),
    n_2014=('pid', 'count'),
    mean_age=('age_num', 'mean'),
    female_pct=('female', 'mean'),
).reset_index()

city_2012 = em12.groupby('admin_code').agg(
    poor_srh_2012=('poor_srh', 'mean'),
    srh_2012=('srh', 'mean'),
).reset_index()

city_base = city_base.merge(city_2012, on='admin_code', how='left')
city_base['srh_change'] = city_base['srh_2014'] - city_base['srh_2012']
city_base['poor_srh_change'] = city_base['poor_srh_2014'] - city_base['poor_srh_2012']

# Standardized differences before balancing
covs = ['poor_srh_2014', 'poor_srh_2012', 'poor_srh_change', 'srh_2014', 'srh_2012', 'srh_change', 'mean_age', 'female_pct', 'n_2014']

def std_diff(treat_vals, control_vals):
    t_mean = np.mean(treat_vals)
    c_mean = np.mean(control_vals)
    pooled_std = np.std(np.concatenate([treat_vals, control_vals]))
    if pooled_std > 0:
        return (t_mean - c_mean) / pooled_std
    return 0

print("\n  Pre-balance standardized differences:")
treat = city_base[city_base['treat'] == 1]
control = city_base[city_base['treat'] == 0]
for v in covs:
    sd = std_diff(treat[v].values, control[v].values)
    print(f"    {v:>20}: {sd:>7.3f}")

# IPW balancing
from sklearn.linear_model import LogisticRegression
z_covs = []
for c in covs:
    std = city_base[c].std()
    city_base[f'{c}_z'] = (city_base[c] - city_base[c].mean()) / std if std > 0 else 0
    z_covs.append(f'{c}_z')

X_prop = city_base[z_covs].fillna(0)
y_prop = city_base['treat']
prop_model = LogisticRegression(max_iter=1000)
prop_model.fit(X_prop, y_prop)
city_base['propensity'] = prop_model.predict_proba(X_prop)[:, 1]
city_base['ipw'] = city_base['treat'] / city_base['propensity'] + (1 - city_base['treat']) / (1 - city_base['propensity'])
city_base['ipw'] = np.clip(city_base['ipw'], 0, 10)

print("\n  Post-balance standardized differences:")
for v in covs:
    t_w = np.average(treat[v].values, weights=np.ones(len(treat)))
    c_w = np.average(control[v].values, weights=city_base[city_base['treat']==0]['ipw'].values)
    pooled_std = city_base[v].std()
    sd = (t_w - c_w) / pooled_std if pooled_std > 0 else 0
    print(f"    {v:>20}: {sd:>7.3f}")

# ============================================================
# PART E: WEIGHTED EVENT STUDY
# ============================================================
print("\n" + "=" * 70)
print("PART E: WEIGHTED EVENT STUDY (2012-2014-2018)")
print("=" * 70)

city_weights = city_base[['admin_code', 'ipw']].drop_duplicates()
em_w = em.merge(city_weights, on='admin_code', how='left')
em_w['ipw'] = em_w['ipw'].fillna(1)

for outcome, label in [('poor_srh', 'poor_srh'), ('srh', 'srh_score')]:
    df = em_w.copy()
    ref_year = 2014
    for w in [2012, 2014, 2018]:
        df[f'tx_{w}'] = ((df['treat'] == 1) & (df['wave'] == w)).astype(int)
        if w == ref_year:
            df[f'tx_{w}'] = 0
    interact_vars = ['tx_2012', 'tx_2018']
    for var in interact_vars + [outcome]:
        df[f'{var}_dm'] = df.groupby('pid')[var].transform(lambda x: x - x.mean())
    X = sm.add_constant(df[[f'{v}_dm' for v in interact_vars]])
    y = df[f'{outcome}_dm']
    model = sm.WLS(y, X, weights=df['ipw']).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})
    print(f"\n  {label} Weighted Event Study:")
    for var, yr in [('tx_2012_dm', '2012'), ('tx_2018_dm', '2018')]:
        coef = model.params[var]
        se = model.bse[var]
        p = model.pvalues[var]
        ci = model.conf_int().loc[var]
        print(f"    {yr}: {coef:.4f} (SE={se:.4f}, p={p:.4f}, 95CI=[{ci[0]:.4f},{ci[1]:.4f}])")

# ============================================================
# PART F: SYNTHETIC DID (Employment, county-level)
# ============================================================
print("\n" + "=" * 70)
print("PART F: SYNTHETIC DID (Employment)")
print("=" * 70)

# Build county-level employment means
county_emp = []
for wave in [2010, 2012, 2014, 2018]:
    sub = labor[labor['wave'] == wave]
    for code in sub['admin_code'].unique():
        csub = sub[sub['admin_code'] == code]
        if len(csub) >= 10:
            county_emp.append({
                'wave': wave,
                'admin_code': code,
                'treat': csub['treat'].iloc[0],
                'employed': csub['employed'].mean(),
                'n': len(csub),
            })

county_df = pd.DataFrame(county_emp)
print(f"\n  Counties with sufficient data: {county_df['admin_code'].nunique()}")
print(f"  Treat counties: {county_df[county_df['treat']==1]['admin_code'].nunique()}")
print(f"  Control counties: {county_df[county_df['treat']==0]['admin_code'].nunique()}")

# Simple synthetic DID using matrix completion approach
# For each treat county, find weighted combination of control counties
# that matches pre-treatment outcomes

treat_codes = county_df[county_df['treat']==1]['admin_code'].unique()
control_codes = county_df[county_df['treat']==0]['admin_code'].unique()

pre_waves = [2010, 2012, 2014]
post_wave = 2018

# Build pre-treatment outcome matrix for controls
control_pre = county_df[county_df['wave'].isin(pre_waves)].pivot_table(
    index='admin_code', columns='wave', values='employed', aggfunc='mean'
)
control_pre = control_pre.reindex(columns=pre_waves)

# For each treat county, find best control match
synth_results = []
for tc in treat_codes:
    treat_pre = county_df[(county_df['admin_code'] == tc) & (county_df['wave'].isin(pre_waves))].set_index('wave')['employed'].reindex(pre_waves).values
    treat_post = county_df[(county_df['admin_code'] == tc) & (county_df['wave'] == post_wave)]['employed'].values

    if len(treat_post) == 0:
        continue

    # Find closest control county
    min_dist = np.inf
    best_cc = None
    for cc in control_codes:
        ctrl_pre = control_pre.loc[cc].values if cc in control_pre.index else None
        if ctrl_pre is not None and not np.any(np.isnan(ctrl_pre)):
            dist = np.sqrt(np.sum((treat_pre - ctrl_pre) ** 2))
            if dist < min_dist:
                min_dist = dist
                best_cc = cc

    if best_cc is not None:
        ctrl_post = county_df[(county_df['admin_code'] == best_cc) & (county_df['wave'] == post_wave)]['employed'].values
        if len(ctrl_post) > 0:
            gap_pre = treat_pre - control_pre.loc[best_cc].values
            gap_post = float(treat_post[0] - ctrl_post[0])
            synth_results.append({
                'treat_code': tc,
                'control_code': best_cc,
                'pre_fit_error': np.mean(np.abs(gap_pre)),
                'post_gap': gap_post,
            })

synth_df = pd.DataFrame(synth_results)
print(f"\n  Matched treat counties: {len(synth_df)}")
print(f"  Mean pre-fit error: {synth_df['pre_fit_error'].mean():.4f}")
print(f"  Mean post gap: {synth_df['post_gap'].mean():.4f}")

# Placebo: run same procedure for control counties
print("\n  Running placebo distribution...")
placebo_gaps = []
np.random.seed(42)
for i in range(500):
    placebo_treat = np.random.choice(control_codes, size=len(treat_codes), replace=False)
    placebo_control = np.setdiff1d(control_codes, placebo_treat)

    placebo_results = []
    for pt in placebo_treat:
        pt_pre = county_df[(county_df['admin_code'] == pt) & (county_df['wave'].isin(pre_waves))].set_index('wave')['employed'].reindex(pre_waves).values
        pt_post = county_df[(county_df['admin_code'] == pt) & (county_df['wave'] == post_wave)]['employed'].values

        if len(pt_post) == 0 or np.any(np.isnan(pt_pre)):
            continue

        min_d = np.inf
        best_p = None
        for pc in placebo_control:
            pc_pre = county_df[(county_df['admin_code'] == pc) & (county_df['wave'].isin(pre_waves))].set_index('wave')['employed'].reindex(pre_waves).values
            if not np.any(np.isnan(pc_pre)):
                d = np.sqrt(np.sum((pt_pre - pc_pre) ** 2))
                if d < min_d:
                    min_d = d
                    best_p = pc

        if best_p is not None:
            pc_post = county_df[(county_df['admin_code'] == best_p) & (county_df['wave'] == post_wave)]['employed'].values
            if len(pc_post) > 0:
                placebo_gaps.append(float(pt_post[0] - pc_post[0]))

if placebo_gaps:
    p_value = np.mean(np.array(placebo_gaps) >= synth_df['post_gap'].mean())
    print(f"  Placebo p-value: {p_value:.4f}")
    print(f"  Real gap: {synth_df['post_gap'].mean():.4f}")
    print(f"  Placebo mean: {np.mean(placebo_gaps):.4f}")
    print(f"  Placebo 95% CI: [{np.percentile(placebo_gaps, 2.5):.4f}, {np.percentile(placebo_gaps, 97.5):.4f}]")

# ============================================================
# PART G: FINAL ASSESSMENT
# ============================================================
print("\n" + "=" * 70)
print("PART G: FINAL ASSESSMENT")
print("=" * 70)
print("""
CHECKLIST:
1. 2010 excluded from SRH: YES
2. SRH trends 2012-2014-2018: REPORTED
3. DDD with HighNeed: RUN
4. Pre-trend balancing: DONE
5. Weighted event study: DONE
6. Synthetic DID: DONE
7. Parallel trends assessment: See results above

CAUSAL INTERPRETATION CRITERIA:
- If weighted event study pre-trends are close to 0: LIMITED causal interpretation
- If DDD pre-trends are close to 0: LIMITED causal interpretation
- If neither: NO causal interpretation, descriptive only
""")

print("=" * 70)
print("Full diagnostic analysis completed.")
print("=" * 70)
