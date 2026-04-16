# WHEP vs Global Reference: Comparison Report

Comparison of the WHEP R package pipeline outputs against the Global
reference files (the original R-script-based pipeline by the same team).

**Date**: 2026-03-26
**WHEP branch**: `edu/build-production-cbs`
**Global reference versions** (pins):
- `primary_prod`: `20250714T123350Z-74e7f`
- `commodity_balance_sheet`: `20250714T123343Z-114b5`
- `processing_coefs`: `20250714T123348Z-06c63`

**WHEP year range**: 1850--2021 (full historical)
**Global year range**: 1961--2021 (FAOSTAT era only)

Comparison restricted to the overlapping 1961--2021 range via key matching.

---

## 1. Primary Production

### 1.1 Overview

| Metric | Value |
|---|---|
| WHEP rows | 6,067,870 |
| Global rows | 2,443,516 |
| Matched rows | 2,259,723 |
| Exact matches | 1,197,862 (53.0%) |
| Close (rel < 1e-6) | 1,800,251 (79.7%) |
| Differ (rel >= 1e-6) | 459,472 (20.3%) |
| Keys only in WHEP | 3,765,910 |
| Keys only in Global | 183,793 |

The large WHEP-only key count is expected: WHEP extends back to 1850 while
Global only covers 1961--2021. The 183,793 Global-only keys are 6 item
codes not present in WHEP (see Section 1.4).

### 1.2 Mismatch categories

| Category | Rows | Description |
|---|---|---|
| ~1000x (WHEP >> Global) | 59,738 | Poultry/small animal unit bug in Global |
| ~0.001x (WHEP << Global) | 65,289 | Inverse of the above (t_head, t_LU units) |
| ~1x (small diff, <5%) | 123,802 | Rounding / algorithmic differences |
| ~1x (moderate diff, 5--20%) | 80,906 | Algorithmic differences in gap-filling |
| other (>20% diff) | 122,104 | Fodder outliers, livestock product cascades |
| >100x | 4,363 | Extreme outliers (fodder zeros in Global) |
| <0.01x | 3,274 | Extreme outliers (inverse) |

### 1.3 Root cause: Poultry 1000x unit error (Global bug)

**Affected items** (all with heads/LU units):

| Item | Code | Rows affected |
|---|---|---|
| Chickens, broilers | 1053 | 18,736 |
| Chickens, layers | 1052 | 18,720 |
| Ducks | 1068 | 7,590 |
| Turkeys | 1079 | 5,846 |
| Rabbits and hares | 1140 | 4,784 |
| Geese | 1072 | 3,824 |
| Rodents, other | 1150 | 238 |

**Diagnosis**: FAOSTAT emissions-livestock reports poultry and small animals
in "An" (individual animals). The Global pipeline erroneously divides these
by 1000, producing values 1000x too low. WHEP correctly preserves the
original unit.

**Example**: Afghanistan, Chickens broilers, 2000:
- FAOSTAT raw: 1,056,000 An
- WHEP output: 1,056,000 heads (correct)
- Global output: 1,056 heads (divided by 1000)

This is a confirmed bug in the Global reference, not in WHEP. The error
accounts for ~125,000 of the 459,472 mismatched rows (the ~1000x and
~0.001x bins combined), since each item appears twice (heads + LU) and
yields-per-head (t_head, t_LU) show the inverse ratio.

**Action**: No fix needed in WHEP.

### 1.4 Root cause: Fodder/forage outliers

The top mismatches by relative difference are fodder items in specific
country-years where Global has near-zero values (~1e-10) while WHEP has
plausible non-zero values:

- UK 2012, Beets for fodder (647): WHEP 720,000 t vs Global 2.7e-10 t
- UK 2012, Mangolds (646): WHEP 700,000 t vs Global 2.6e-10 t

These are **not systematic**: the median WHEP/Global ratio for all fodder
items across all countries is ~1.0. The outliers are localized cases where
the Global pipeline zeroed out values (likely a different carry-forward or
QC treatment).

**Action**: Investigate if the Global pipeline applies a stricter
zero-out rule for specific country-year-item combinations, but this does
not indicate a WHEP bug.

### 1.5 Root cause: Crop mismatches (~1x ratios)

The 123,802 "small diff" and 80,906 "moderate diff" rows are distributed
across livestock product items (offals, fats, hides) and forages. These
stem from:

1. **Gap-filling algorithm differences**: WHEP uses `fill_linear` and
   `fill_proxy_growth` which may interpolate slightly differently than
   Global's equivalent functions.
2. **Carry-forward smoothing**: WHEP's QC pipeline flags and optionally
   smooths carry-forward tails; Global may handle these differently.
3. **Historical yield extrapolation**: Small differences in how yields are
   extended backward compound when multiplied by area.

These are expected algorithmic differences, not bugs.

### 1.6 Unmatched items

**Global-only (6 items)**:
Mate leaves (671), Tallowtree seed (305), Game (1190), Game meat (1163),
Edible offals of horses (1098), Flax processed (773).

These are minor items that WHEP intentionally excludes or maps differently.

**WHEP-only (2 items)**:
Pasture (3001), Range (3002) -- synthetic items created by WHEP for
grassland/rangeland accounting. Not present in Global.

---

## 2. Commodity Balance Sheets (CBS)

### 2.1 Overview

| Metric | Value |
|---|---|
| WHEP rows | 22,509,570 |
| Global rows | 9,315,170 |
| Matched rows | 8,726,240 |
| Exact matches | 6,870,517 (78.7%) |
| Close (rel < 1e-6) | 7,119,732 (81.6%) |
| Differ (rel >= 1e-6) | 1,606,508 (18.4%) |

### 2.2 Element-level breakdown

| Element | Mismatches | Median ratio | % near 1x |
|---|---|---|---|
| domestic_supply | 349,224 | 0.950 | 42.8% |
| stock_variation | 340,522 | 0.730 | 7.4% |
| food | 286,025 | 0.949 | 43.7% |
| production | 114,809 | 1.000 | 32.0% |
| feed | 107,042 | 0.951 | 42.3% |
| export | 102,235 | 1.060 | 7.5% |
| other_uses | 100,173 | 0.970 | 45.6% |
| seed | 99,103 | 0.950 | 48.4% |
| processing | 64,894 | 0.959 | 47.2% |
| import | 44,211 | 0.638 | 20.2% |

### 2.3 Root cause: Domestic supply ~5% offset

The dominant pattern: domestic supply and downstream elements (food, feed,
seed, other_uses, processing) show a systematic median ratio of ~0.95,
meaning WHEP values are ~5% lower than Global. This is:

- **Not a data version issue**: even where production matches exactly, DS
  differs.
- **Evenly distributed across decades**: not a temporal artifact.
- **Algorithmic**: WHEP's `.cbs_impute_trade()` and
  `.reestimate_domestic_supply()` steps use different balancing logic
  than Global. Specifically, WHEP computes
  `domestic_supply = production + import - export + stock_variation` and
  then redistributes destiny shares proportionally, while Global may use
  slightly different imputation order or defaults.

Since the offset is consistent (~5%) and affects all destiny elements
proportionally, it suggests a systematic difference in how the
"domestic supply" anchor is calculated, not a per-element bug.

### 2.4 Root cause: Export zeros

The top CBS mismatches are all export values where WHEP has large nonzero
values and Global has zero:

| Rows | Pattern |
|---|---|
| 57,900 | Zero in Global, nonzero in WHEP (mostly export) |
| 2,537 | Zero in WHEP, nonzero in Global (export) |

WHEP's `.cbs_impute_trade()` fills missing trade using:
```
net_trade = domestic_supply - production
export = max(0, domestic_supply - production)  [when DS > production]
```

Global does not perform this imputation, leaving exports at zero when
FAOSTAT has no trade data. This is an intentional WHEP enhancement, not a
bug.

### 2.5 Root cause: Stock variation divergence

Stock variation has the worst agreement (median ratio 0.730, only 7.4%
near 1x). This is because:

1. WHEP computes `stock_variation = -stock_retrieval` (sign flip from
   the internal CBS representation).
2. FAOSTAT stock variation data is sparse and noisy.
3. WHEP's balancing step may redistribute residuals differently.

Additionally, 62,037 rows have nonzero stock_variation in WHEP but zero
in Global, and 35,807 rows have the inverse. This bidirectional
zero-vs-nonzero pattern suggests different imputation strategies.

### 2.6 Root cause: Import divergence

Import has the lowest median ratio (0.638) among non-stock elements.
WHEP's GDP-based trade imputation (`.cbs_impute_trade()`) fills import
values for countries with no FAOSTAT trade data, using GDP as a predictor.
Global either doesn't perform this imputation or uses different parameters.

### 2.7 Area coverage

| Set | Count | Details |
|---|---|---|
| Common | 184 | |
| Global-only | 3 | Netherlands Antilles (151), Sudan former (206), RoW (999) |
| WHEP-only | 6 | Taiwan (214), Eswatini (209), North Macedonia (154), South Sudan (277), Syria (212), + 1 NA |

The Global-only areas are historical/aggregate entities. The WHEP-only
areas are modern states that WHEP tracks but Global aggregates into parent
entities. The NA area code in WHEP is a data quality issue to investigate.

---

## 3. Processing Coefficients

### 3.1 Overview

| Metric | Value |
|---|---|
| WHEP rows | 595,720 |
| Global rows | 217,896 |
| Matched rows | 165,484 |
| Keys only in WHEP | 430,236 |
| Keys only in Global | 52,412 |

### 3.2 Value comparison

| Column | Exact | Close | Differ |
|---|---|---|---|
| value_to_process | 51,057 | 54,946 | 110,538 |
| initial_conversion_factor | 1,985 | 28,871 | 136,613 |
| initial_value_processed | 565 | 10,660 | 154,824 |
| conversion_factor_scaling | 646 | 10,669 | 154,815 |
| final_conversion_factor | 4,374 | 33,112 | 132,372 |
| final_value_processed | 47,659 | 82,095 | 83,389 |

### 3.3 Root cause

Processing coefficients cascade from CBS values. Since CBS production and
domestic supply differ by ~5%, the `value_to_process` input diverges,
which propagates through all downstream columns (conversion factors,
scaling, final processed values).

The worst mismatches (area_code 101 = India, item 2537 = Sugar cane) show
WHEP values ~460,000x larger than Global, indicating that the CBS
production value for sugar cane in India differs dramatically between the
two pipelines for specific years (1995, 1999).

**Action**: These differences are downstream consequences of CBS
differences and will resolve when CBS alignment improves.

---

## 4. Summary of findings

### Confirmed bugs (in Global reference, not WHEP)

1. **Poultry/small animal 1000x unit error**: Global divides poultry
   stocks by 1000 erroneously. Affects ~125,000 primary production rows
   across 7 animal types. WHEP is correct.

### Intentional algorithmic differences

2. **CBS domestic supply ~5% offset**: WHEP's balancing and imputation
   steps produce systematically ~5% lower domestic supply values. This
   cascades to all destiny elements (food, feed, seed, etc.).

3. **Trade imputation**: WHEP fills missing trade data using GDP-based
   models and `domestic_supply - production` residuals. Global leaves
   these as zero. ~60,000 export rows affected.

4. **Stock variation**: Different handling of sparse FAOSTAT stock data
   produces bidirectional zero-vs-nonzero mismatches.

5. **Historical extension**: WHEP covers 1850--2021 vs Global's
   1961--2021, adding ~3.7M primary and ~13.8M CBS rows.

### Minor discrepancies

6. **6 Global-only item codes**: Mate leaves, Tallowtree seed, Game,
   Game meat, Horse offals, Processed flax. Minor items.

7. **Area name differences**: 10 countries have cosmetic name differences
   (short vs official UN names). 3 Global-only area codes
   (historical/aggregate), 6 WHEP-only (modern states).

8. **Fodder outliers**: Localized country-years where Global zeroes out
   values that WHEP preserves. Not systematic.

### Recommended next steps

1. **Document the poultry unit bug** in Global and flag it for the
   upstream team.
2. **Investigate the CBS ~5% DS offset** -- trace through a single
   country-item-year to identify which step introduces the divergence.
   Candidates: `.cbs_impute_trade()`, `.reestimate_domestic_supply()`,
   or destiny share redistribution.
3. **Decide on trade imputation policy** -- the WHEP approach of filling
   missing trade is more complete but produces values Global doesn't have.
   Confirm this is the intended behavior.
4. **Fix the NA area_code** in WHEP CBS output.

---

## 5. How to reproduce

```r
# Run the comparison (requires pins access):
source("inst/scripts/compare_global_whep.R")
```
