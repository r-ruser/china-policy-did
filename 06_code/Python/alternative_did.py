"""
alternative_did.py
Alternative identification strategies
"""
import pandas as pd
import numpy as np
import os
import statsmodels.api as sm

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "07_results")

print("=" * 60)
print("Alternative Identification Strategies")
print("=" * 60)

elderly = pd.read_parquet(os.path.join(PROJECT_ROOT, "05_analysis_data", "cfps_elderly_cohort.parquet"))
labor = pd.read_parquet(os.path.join(PROJECT_ROOT, "05_analysis_data", "cfps_labor_cohort.parquet"))

em = elderly[elderly['wave'].isin([2010, 2012, 2014, 2018])].copy()
lm = labor[labor['wave'].isin([2010, 2012, 2014, 2018])].copy()
em['post'] = (em['wave'] >= 2018).astype(int)
lm['post'] = (lm['wave'] >= 2018).astype(int)

# ============================================================
# 1. DID with city-clustered SE
# ============================================================
print("\n[1] DID with city-clustered SE...")
for label, df, outcome in [('SRH', em, 'poor_srh'), ('Employment', lm, 'employed')]:
    df = df.copy()
    df['did'] = df['treat'] * df['post']
    X = sm.add_constant(df[['did', 'post', 'treat']])
    y = df[outcome]
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})
    print(f"\n  {label} (city-clustered):")
    print(f"    DID: {model.params['did']:.4f} (SE={model.bse['did']:.4f}, p={model.pvalues['did']:.4f})")
    print(f"    95% CI: [{model.conf_int().loc['did', 0]:.4f}, {model.conf_int().loc['did', 1]:.4f}]")

# ============================================================
# 2. DID with individual FE + city-clustered SE
# ============================================================
print("\n[2] DID with individual FE + city-clustered SE...")
for label, df, outcome in [('SRH', em, 'poor_srh'), ('Employment', lm, 'employed')]:
    df = df.copy()
    df['did'] = df['treat'] * df['post']
    df['outcome_dm'] = df.groupby('pid')[outcome].transform(lambda x: x - x.mean())
    df['did_dm'] = df.groupby('pid')['did'].transform(lambda x: x - x.mean())
    df['post_dm'] = df.groupby('pid')['post'].transform(lambda x: x - x.mean())
    df['treat_dm'] = df.groupby('pid')['treat'].transform(lambda x: x - x.mean())
    X = sm.add_constant(df[['did_dm', 'post_dm', 'treat_dm']])
    y = df['outcome_dm']
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})
    print(f"\n  {label} (individual FE, city-clustered):")
    print(f"    DID: {model.params['did_dm']:.4f} (SE={model.bse['did_dm']:.4f}, p={model.pvalues['did_dm']:.4f})")

# ============================================================
# 3. Event study with city-clustered SE
# ============================================================
print("\n[3] Event study with city-clustered SE...")
for label, df, outcome in [('SRH', em, 'poor_srh'), ('Employment', lm, 'employed')]:
    df = df.copy()
    ref_year = 2014
    waves = [2010, 2012, 2014, 2018]
    for w in waves:
        df[f'treat_x_{w}'] = ((df['treat'] == 1) & (df['wave'] == w)).astype(int)
        if w == ref_year:
            df[f'treat_x_{w}'] = 0
    interact_vars = [f'treat_x_{w}' for w in waves if w != ref_year]
    for var in interact_vars + [outcome]:
        df[f'{var}_dm'] = df.groupby('pid')[var].transform(lambda x: x - x.mean())
    X = sm.add_constant(df[[f'{v}_dm' for v in interact_vars]])
    y = df[f'{outcome}_dm']
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df['admin_code']})
    print(f"\n  {label} Event Study:")
    for w in waves:
        if w == ref_year:
            print(f"    {w}: 0.0000 (reference)")
        else:
            var = f'treat_x_{w}_dm'
            coef = model.params[var]
            se = model.bse[var]
            p = model.pvalues[var]
            ci = model.conf_int().loc[var]
            print(f"    {w}: {coef:.4f} (SE={se:.4f}, p={p:.4f}, 95CI=[{ci[0]:.4f},{ci[1]:.4f}])")

# ============================================================
# 4. IPW-weighted DID
# ============================================================
print("\n[4] IPW-weighted DID...")
em14 = em[em['wave'] == 2014].copy()
em12 = em[em['wave'] == 2012].copy()
city_2014 = em14.groupby('admin_code').agg(
    treat=('treat', 'first'), poor_srh_2014=('poor_srh', 'mean'),
    srh_2014=('srh', 'mean'), n_2014=('pid', 'count'),
    mean_age=('age_num', 'mean'), female_pct=('female', 'mean'),
).reset_index()
city_2012 = em12.groupby('admin_code').agg(
    poor_srh_2012=('poor_srh', 'mean'), srh_2012=('srh', 'mean'),
).reset_index()
city_base = city_2014.merge(city_2012, on='admin_code', how='left')

from sklearn.linear_model import LogisticRegression
covs = ['poor_srh_2014', 'srh_2012', 'mean_age', 'female_pct', 'n_2014']
for c in covs:
    std = city_base[c].std()
    city_base[f'{c}_z'] = (city_base[c] - city_base[c].mean()) / std if std > 0 else 0
X_prop = city_base[[f'{c}_z' for c in covs]].fillna(0)
y_prop = city_base['treat']
prop_model = LogisticRegression(max_iter=1000)
prop_model.fit(X_prop, y_prop)
city_base['propensity'] = prop_model.predict_proba(X_prop)[:, 1]
city_base['ipw'] = city_base['treat'] / city_base['propensity'] + (1 - city_base['treat']) / (1 - city_base['propensity'])
city_base['ipw'] = np.clip(city_base['ipw'], 0, 10)

city_weights = city_base[['admin_code', 'ipw']].drop_duplicates()
em_w = em.merge(city_weights, on='admin_code', how='left')
em_w['ipw'] = em_w['ipw'].fillna(1)
lm_w = lm.merge(city_weights, on='admin_code', how='left')
lm_w['ipw'] = lm_w['ipw'].fillna(1)

for label, df_w, outcome in [('SRH', em_w, 'poor_srh'), ('Employment', lm_w, 'employed')]:
    df_w = df_w.copy()
    df_w['did'] = df_w['treat'] * df_w['post']
    X = sm.add_constant(df_w[['did', 'post', 'treat']])
    y = df_w[outcome]
    model = sm.WLS(y, X, weights=df_w['ipw']).fit(cov_type='cluster', cov_kwds={'groups': df_w['admin_code']})
    print(f"\n  {label} IPW-weighted DID:")
    print(f"    DID: {model.params['did']:.4f} (SE={model.bse['did']:.4f}, p={model.pvalues['did']:.4f})")

# ============================================================
# 5. Pre-change matched DID
# ============================================================
print("\n[5] Pre-change matched DID...")
city_pre = em12.groupby('admin_code').agg(
    treat=('treat', 'first'), srh_2012=('srh', 'mean'),
).reset_index()
city_pre = city_pre.merge(
    em14.groupby('admin_code').agg(srh_2014=('srh', 'mean')).reset_index(), on='admin_code')
city_pre['srh_change_pre'] = city_pre['srh_2014'] - city_pre['srh_2012']

treat_cities = city_pre[city_pre['treat']==1][['admin_code', 'srh_2012', 'srh_change_pre']]
control_cities = city_pre[city_pre['treat']==0][['admin_code', 'srh_2012', 'srh_change_pre']]

matched_pairs = []
for _, t_row in treat_cities.iterrows():
    dists = np.sqrt((control_cities['srh_2012'] - t_row['srh_2012'])**2 +
                     (control_cities['srh_change_pre'] - t_row['srh_change_pre'])**2)
    nearest_idx = dists.idxmin()
    matched_pairs.append({
        'treat_code': t_row['admin_code'],
        'control_code': control_cities.loc[nearest_idx, 'admin_code'],
        'distance': dists.min(),
    })
matched_df = pd.DataFrame(matched_pairs)
print(f"  Matched pairs: {len(matched_df)}")

matched_codes = set(matched_df['treat_code']) | set(matched_df['control_code'])
em_matched = em[em['admin_code'].isin(matched_codes)].copy()
lm_matched = lm[lm['admin_code'].isin(matched_codes)].copy()

for label, df_m, outcome in [('SRH', em_matched, 'poor_srh'), ('Employment', lm_matched, 'employed')]:
    df_m = df_m.copy()
    df_m['did'] = df_m['treat'] * df_m['post']
    X = sm.add_constant(df_m[['did', 'post', 'treat']])
    y = df_m[outcome]
    model = sm.OLS(y, X).fit(cov_type='cluster', cov_kwds={'groups': df_m['admin_code']})
    print(f"\n  {label} matched DID:")
    print(f"    DID: {model.params['did']:.4f} (SE={model.bse['did']:.4f}, p={model.pvalues['did']:.4f})")

print("\n" + "=" * 60)
print("All alternative identification completed.")
print("=" * 60)
