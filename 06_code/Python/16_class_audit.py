"""
16_class_audit.py
CLASS database audit: variable availability, cohort construction
"""
import pyreadstat
import pandas as pd
import numpy as np
import os

PROJECT_ROOT = "E:/公共数据库/中国数据库/医养结合政策DID_CHFS_CFPS"
CLASS_ROOT = "E:/公共数据库/中国数据库/CLASS数据全"
OUTPUT_DIR = os.path.join(PROJECT_ROOT, "07_results")

print("=" * 70)
print("CLASS DATABASE AUDIT")
print("=" * 70)

# ============================================================
# 1. Check available files
# ============================================================
print("\n[1] Available CLASS files...")
stata_dir = os.path.join(CLASS_ROOT, "两种格式", "STATA")
for f in sorted(os.listdir(stata_dir)):
    if f.endswith('.dta'):
        fp = os.path.join(stata_dir, f)
        size = os.path.getsize(fp) / 1e6
        print(f"  {f} ({size:.1f} MB)")

# ============================================================
# 2. Read each wave
# ============================================================
waves = {
    2014: os.path.join(stata_dir, '2014class数据_发布版.dta'),
    2016: os.path.join(stata_dir, '2016class-individual-发布版.dta'),
    2018: os.path.join(stata_dir, 'CLASS2018-cleaned release.dta'),
    2020: os.path.join(stata_dir, 'individual -2020  cleaned for user.sav'),
    2023: os.path.join(stata_dir, '2023_individual_release_weighted .dta'),
}

wave_data = {}
for wave, path in waves.items():
    print(f"\n[2.{wave}] Reading CLASS {wave}...")
    try:
        if path.endswith('.sav'):
            df, meta = pyreadstat.read_sav(path)
        else:
            df, meta = pyreadstat.read_dta(path)
        print(f"  Shape: {df.shape}")
        print(f"  Columns: {len(df.columns)}")

        # Check for geographic variables
        geo_cols = [c for c in df.columns if any(k in c.lower() for k in ['province', 'city', 'county', 'district', 'region', 'area', 'urban', 'code', 'id', 's03', 'rid', 'pid'])]
        print(f"  Geographic/ID columns: {geo_cols[:15]}")

        # Check for health variables
        health_cols = [c for c in df.columns if any(k in c.lower() for k in ['health', 'adl', 'iadl', 'chronic', 'depress', 'cesd', 'care', 'medical', 'hospital', 'doctor', 'nursing'])]
        print(f"  Health columns: {health_cols[:20]}")

        wave_data[wave] = (df, meta)
    except Exception as e:
        print(f"  Error: {e}")

# ============================================================
# 3. Detailed variable check for 2014 and 2018
# ============================================================
for wave in [2014, 2018]:
    if wave in wave_data:
        df, meta = wave_data[wave]
        print(f"\n[3.{wave}] CLASS {wave} variable detail...")

        # Search for ADL/IADL
        adl_cols = [c for c in df.columns if any(k in c.lower() for k in ['adl', 'iadl', 'daily', 'activity', 'dressing', 'bathing', 'eating'])]
        print(f"  ADL/IADL: {adl_cols[:15]}")

        # Search for depression
        dep_cols = [c for c in df.columns if any(k in c.lower() for k in ['depress', 'cesd', 'phq', 'mental', 'mhi'])]
        print(f"  Depression: {dep_cols[:15]}")

        # Search for chronic disease
        chronic_cols = [c for c in df.columns if any(k in c.lower() for k in ['chronic', 'disease', 'hypert', 'diabet', 'heart', 'stroke'])]
        print(f"  Chronic: {chronic_cols[:15]}")

        # Search for care
        care_cols = [c for c in df.columns if any(k in c.lower() for k in ['care', 'nurse', 'help', 'formal', 'informal', 'caregiv'])]
        print(f"  Care: {care_cols[:15]}")

        # Search for medical utilization
        med_cols = [c for c in df.columns if any(k in c.lower() for k in ['hospital', 'doctor', 'visit', 'med', 'outpat', 'inpat'])]
        print(f"  Medical: {med_cols[:15]}")

        # Print first 60 column names
        print(f"  First 60 columns: {list(df.columns)[:60]}")

# ============================================================
# 4. Variable labels for key variables in 2014
# ============================================================
if 2014 in wave_data:
    df, meta = wave_data[2014]
    print(f"\n[4] CLASS 2014 variable labels (health-related)...")
    for var, label in meta.column_names_to_labels.items():
        if label and any(k in str(label).lower() for k in ['health', 'adl', 'iadl', 'chronic', 'depress', 'care', 'medical', 'hospital', 'daily', 'activity', 'disease']):
            if var in df.columns:
                vals = df[var].dropna()
                if len(vals) > 0 and vals.nunique() <= 15:
                    print(f"  {var}: '{label}' values={sorted(vals.unique())[:10]}")

# ============================================================
# 5. ID variables
# ============================================================
print("\n[5] ID variables across waves...")
for wave, (df, meta) in wave_data.items():
    id_cols = [c for c in df.columns if any(k in c.lower() for k in ['pid', 'rid', 'id'])]
    print(f"  {wave}: {id_cols[:10]}")

print("\n" + "=" * 70)
print("CLASS audit completed.")
print("=" * 70)
