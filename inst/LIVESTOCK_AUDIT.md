# Livestock Emissions Module — Audit Report

## Date: 2025-07-16

## Branch: `edu/livestock-emissions`

---

## 1. Overview

The livestock emissions module implements IPCC 2019 Refinement (Vol 4,
Ch 10) methodologies for estimating greenhouse gas emissions from
livestock. It supports both **Tier 1** (emission-factor based) and
**Tier 2** (energy-based) approaches for enteric CH₄, manure CH₄,
and manure N₂O.

### Source Files

| File | Lines | Purpose |
|---|---|---|
| `R/livestock_energy.R` | 522 | Tier 2 Gross Energy (Eq 10.3–10.16) |
| `R/livestock_enteric.R` | 269 | Enteric CH₄ Tier 1 & 2 |
| `R/livestock_manure.R` | 569 | Manure CH₄ (Tier 1 & 2) and N₂O |
| `R/livestock_emissions.R` | 126 | Main dispatcher / orchestration |
| `R/livestock_cohorts.R` | 107 | Cohort & production system distribution |
| `R/livestock_uncertainty.R` | 91 | Uncertainty bounds |

### Key Design Principles

- **No hardcoded values**: all coefficients come from internal
  package data tables (`R/sysdata.rda`).
- **Regional/global fallbacks**: cattle EFs use regional values when
  `region` or `iso3` is provided, falling back to Global averages.
- **Automatic tier selection**: `calculate_livestock_emissions()`
  detects whether Tier 2 prerequisites (`cohort` + `weight`/`iso3`)
  are present and selects the appropriate tier.
- **Modular pipeline**: each function adds columns and can be
  composed freely (e.g. `estimate_energy_demand() |>
  calc_enteric_ch4_tier2()`).

---

## 2. Architecture & Data Flow

### Tier 2 Pipeline

```
Input data (species, cohort, weight, heads, ...)
  │
  ├─ estimate_energy_demand()
  │    ├─ .get_general_species()       → species_gen
  │    ├─ .get_subcategory()           → Dairy / Non-Dairy / All
  │    ├─ .join_weights()              → gleam_animal_weights
  │    ├─ .join_energy_coefs()         → ipcc_tier2_energy_coefs
  │    ├─ .ensure_production_cols()    → ensure optional cols exist
  │    ├─ .join_production_defaults()  → livestock_production_defaults
  │    ├─ .set_activity_coef()         → Ca (pasture/feedlot)
  │    ├─ .join_feed_characteristics() → DE%, Ym, CP%
  │    ├─ .join_temperature_adjustment()
  │    ├─ calc_energy_maintenance()    → NEm (Eq 10.3)
  │    ├─ calc_energy_activity()       → NEa (Eq 10.4)
  │    ├─ calc_energy_lactation()      → NEl (Eq 10.8/10.9/NRC)
  │    ├─ calc_energy_wool()           → NEwool (Eq 10.6)
  │    ├─ calc_energy_work()           → NEwork (Eq 10.11)
  │    ├─ calc_energy_pregnancy()      → NEp (Eq 10.13)
  │    ├─ calc_energy_growth()         → NEg (Eq 10.6)
  │    └─ estimate_gross_energy()      → GE (Eq 10.16), REM, REG
  │
  ├─ calc_enteric_ch4_tier2()
  │    ├─ .join_ym_values()            → Ym from Table 10.12
  │    └─ CH4 = GE * (Ym/100) * 365 / 55.65
  │
  └─ Manure pathway
       ├─ calc_manure_ch4_tier2()
       │    ├─ .calc_volatile_solids() → VS (Eq 10.24)
       │    ├─ .join_bo_values()       → Bo from Table 10.16
       │    ├─ .join_mcf_values()      → MCF from Table 10.17
       │    └─ CH4 = VS * 365 * Bo * 0.67 * MCF * heads
       │
       └─ calc_manure_n2o()
            ├─ .calc_n_excretion()     → Nex (Eq 10.31–10.33)
            ├─ .calc_direct_n2o()      → EF3 from Table 10.21
            └─ .calc_indirect_n2o()    → FracGas, FracLeach
```

### Tier 1 Pipeline

```
Input data (species, heads)
  │
  ├─ calc_enteric_ch4_tier1()
  │    ├─ Cattle → Table 10.10 (regional/global)
  │    └─ Others (incl. Buffalo) → Table 10.11
  │
  └─ calc_manure_ch4_tier1()
       ├─ Cattle → Table 10.14 (regional/global)
       └─ Others (incl. Buffalo) → Table 10.15
```

---

## 3. Module Details

### 3.1 Energy Demand (`livestock_energy.R`)

#### Exported Functions

| Function | IPCC Reference | Formula |
|---|---|---|
| `calc_energy_maintenance()` | Eq 10.3 | NEm = Cfi × W^0.75 × (1 + temp_adj) |
| `calc_energy_activity()` | Eq 10.4 | NEa = Ca × NEm + walk_cost × W × dist |
| `calc_energy_lactation()` | Eq 10.8/10.9 | NEl = Milk × (1.47 + 0.40 × Fat%) |
| `calc_energy_wool()` | Eq 10.6 | NEwool = EVwool × wool_kg_yr / 365 |
| `calc_energy_work()` | Eq 10.11 | NEwork = 0.10 × NEm × hours |
| `calc_energy_pregnancy()` | Eq 10.13 | NEp = Cp × NEm × pregnant_frac |
| `calc_energy_growth()` | Eq 10.6 | NEg = WG × energy_content_gain |
| `estimate_gross_energy()` | Eq 10.16 | GE = (NE_m/REM + NE_g/REG)/(DE%/100) |
| `estimate_energy_demand()` | — | Full pipeline combining all above |

#### Enhanced Lactation (NRC 2001)

When `protein_percent` and `lactose_percent` are available, uses:

```
NEl = Milk × (0.389 × Fat + 0.229 × Prot + 0.165 × Lact)
```

For sheep/goats without detailed composition: `NEl = Milk × 4.6`

#### REM / REG Calculations

```
REM = 1.123 - (4.092e-3 × DE%) + (1.126e-5 × DE%²) - (25.4 / DE%)
REG = 1.164 - (5.160e-3 × DE%) + (1.308e-5 × DE%²) - (37.4 / DE%)
```

#### Private Helpers

- `.get_general_species()`: Maps species string to general category
  (Cattle, Buffalo, Sheep, Goats, Swine, Poultry, Horses, Camels,
  Mules and Asses).
- `.get_subcategory()`: Returns "Dairy", "Non-Dairy", or "All".
- `.join_production_defaults()`: Joins `livestock_production_defaults`
  using a `defaults_key` mapping (e.g. "Beef Cattle" → "Other Cattle")
  so that all cattle variants find their correct defaults.
- `.set_activity_coef()`: Sets `Ca` based on production system
  (pasture vs feedlot).
- `.join_feed_characteristics()`: Joins `feed_characteristics` table
  for DE%, Ym, CP% by diet quality.
- `.join_temperature_adjustment()`: Applies temperature-based
  metabolic adjustment.

---

### 3.2 Enteric CH₄ (`livestock_enteric.R`)

#### Tier 1

- **Cattle only** (species_gen == "Cattle"): Uses
  `ipcc_2019_enteric_ef_cattle` (Table 10.10), with regional values
  and Global fallback.
- **All other species** (including Buffalo): Uses
  `ipcc_2019_enteric_ef_other` (Table 10.11).
- Subcategory aggregation: e.g. "Swine" averages "Swine - Market"
  and "Swine - Breeding" EFs.

| Species | EF (kg CH₄/head/yr) | Source |
|---|---|---|
| Dairy Cattle (Global) | 80 | Table 10.10 |
| Other Cattle (Global) | 47 | Table 10.10 |
| Buffalo | 55 | Table 10.11 |
| Sheep | 8 | Table 10.11 |
| Goats | 5 | Table 10.11 |
| Swine (avg) | 1.5 | Table 10.11 |
| Poultry | 0 | Not applicable |
| Horses | 18 | Table 10.11 |
| Camels | 46 | Table 10.11 |
| Mules and Asses | 10 | Table 10.11 |

#### Tier 2

```
CH4_per_head = GE × (Ym/100) × 365 / energy_content_ch4
```

Where `energy_content_ch4` = 55.65 MJ/kg CH₄ and Ym comes from
`ipcc_tier2_ym_values` (Table 10.12), joined by species_gen +
subcategory + diet_quality.

---

### 3.3 Manure Emissions (`livestock_manure.R`)

#### Manure CH₄ — Tier 1

Same routing logic as enteric:
- **Cattle** → `ipcc_2019_manure_ch4_ef_cattle` (Table 10.14)
- **Others** (incl. Buffalo) → `ipcc_2019_manure_ch4_ef_other`
  (Table 10.15)

| Species | EF (kg CH₄/head/yr) | Source |
|---|---|---|
| Dairy Cattle (Global) | 36 | Table 10.14 |
| Other Cattle (Global) | 2 | Table 10.14 |
| Buffalo | 2 | Table 10.15 |
| Sheep | 0.19 | Table 10.15 |
| Goats | 0.13 | Table 10.15 |
| Swine (avg) | 6 | Table 10.15 |
| Horses | 1.64 | Table 10.15 |
| Camels | 1.92 | Table 10.15 |
| Mules and Asses | 0.9 | Table 10.15 |

#### Manure CH₄ — Tier 2

```
VS = GE × (1 - DE%/100) + (UE × GE)    (Eq 10.24, all ÷ 18.45)
CH4/head = VS × 365 × Bo × 0.67 × ΣMCFj
```

Where:
- Bo from `ipcc_tier2_bo_values` (Table 10.16)
- MCF from `ipcc_2019_mcf_manure` (Table 10.17), weighted by MMS
  distribution when regional data is available
- UE = 0.04 (urinary energy fraction)

#### Manure N₂O

**N Excretion (Eq 10.31–10.33):**

```
N_intake = (GE / 18.45) × (CP% / 100) / 6.25
Nex = N_intake × (1 - N_retention) × 365    [kgN/head/yr]
```

N retention fractions from `ipcc_tier2_n_retention` (differentiated
by species; default 0.07).

**Direct N₂O (Eq 10.25):**

```
N2O_direct = Σ(Nex × MMS_share × EF3) × (44/28)
```

EF3 from `ipcc_2019_n2o_ef_direct` (Table 10.21). Without regional
MMS distribution, uses pasture-only default.

**Indirect N₂O (Eq 10.26–10.28):**

```
N2O_indirect_vol = Nex × FracGas × EF4 × (44/28)
N2O_indirect_leach = Nex × FracLeach × EF5 × (44/28)
```

Parameters from `indirect_n2o_ef` table.

---

### 3.4 Orchestration (`livestock_emissions.R`)

| Function | Behavior |
|---|---|
| `calculate_enteric_ch4(data, tier)` | Auto-selects tier; forced via `tier=1` or `tier=2` |
| `calculate_manure_emissions(data, tier)` | Tier 2: CH₄ + N₂O; Tier 1: CH₄ only |
| `calculate_livestock_emissions(data, tier)` | Runs both enteric + manure |

**Tier resolution** (`.resolve_tier()`): Tier 2 requires `cohort`
column AND either `weight` or `iso3`. Otherwise defaults to Tier 1.

---

### 3.5 Cohort Distribution (`livestock_cohorts.R`)

`calculate_cohorts_systems()` distributes a total herd across GLEAM
cohorts and production systems using:
- `gleam_livestock_categories`: cohort fractions by species
- GLEAM S.6 tables (e.g. `gleam_field_operation_ef`, `gleam_mechanization_levels`): system shares by species + region

---

### 3.6 Uncertainty (`livestock_uncertainty.R`)

`calculate_uncertainty_bounds()` applies IPCC uncertainty ranges
(±%) from `uncertainty_ranges` table to produce `lower` and `upper`
bound columns.

---

## 4. Coefficient Tables

All tables are stored in `R/sysdata.rda` and built from
`data-raw/livestock_coefficients.R`.

| Table | Rows | Key Columns | Source |
|---|---|---|---|
| `ipcc_tier2_energy_coefs` | 6 | species_gen, subcategory, cfi, energy_content_gain | IPCC Table 10.4 |
| `ipcc_tier2_ym_values` | 16 | species_gen, subcategory, diet_quality, ym_percent | IPCC Table 10.12 |
| `ipcc_tier2_bo_values` | 12 | species_gen, subcategory, bo_m3_kg_vs | IPCC Table 10.16 |
| `ipcc_2019_enteric_ef_cattle` | 20 | region, category, ef_cattle | IPCC Table 10.10 |
| `ipcc_2019_enteric_ef_other` | 9 | species, ef_other | IPCC Table 10.11 |
| `ipcc_2019_manure_ch4_ef_cattle` | varies | region, category, ef_ch4 | IPCC Table 10.14 |
| `ipcc_2019_manure_ch4_ef_other` | 10 | species, ef_ch4 | IPCC Table 10.15 |
| `ipcc_2019_n2o_ef_direct` | 18 | system, ef_kg_n2o_n_per_kg_n | IPCC Table 10.21 |
| `ipcc_2019_mcf_manure` | varies | climate, system, mcf | IPCC Table 10.17 |
| `ipcc_tier2_n_retention` | 10 | category, n_retention_frac | IPCC Table 10.20 |
| `ipcc_tier2_manure_ash` | 9 | category, ash_fraction | IPCC Table 10.A.9 |
| `indirect_n2o_ef` | 4 | parameter, value | IPCC Table 10.22 |
| `feed_characteristics` | 3 | diet_quality, de_percent, ym, cp_percent | IPCC / GLEAM |
| `temperature_adjustment` | 3 | temp_range, adjustment | IPCC Table 10.A.2 |
| `livestock_production_defaults` | 10 | category, weight, milk, fat%, etc. | GLEAM 3.0 |
| `livestock_constants` | 8 | Named values | IPCC Ch 10 |
| `grazing_energy_coefs` | varies | parameter, value | NRC 2001 |
| `uncertainty_ranges` | varies | species, source, lower/upper_pct | IPCC Table 3.2 |

### Livestock Constants

| Constant | Value | Unit | Source |
|---|---|---|---|
| `energy_content_ch4_mj_kg` | 55.65 | MJ/kg | IPCC Eq 10.21 |
| `ch4_density_kg_m3` | 0.67 | kg/m³ | IPCC Table 10.A.8 |
| `vs_energy_content_mj_kg` | 18.45 | MJ/kg DM | IPCC Eq 10.24 |
| `n_to_n2o` | 1.571 (44/28) | — | Molecular weight ratio |
| `days_in_year` | 365 | days | — |
| `default_de_percent` | 65 | % | IPCC default |
| `default_ue_fraction` | 0.04 | fraction | IPCC Eq 10.24 |
| `ev_wool_mj_kg` | 24.0 | MJ/kg | IPCC Eq 10.6 |

---

## 5. Bugs Found & Fixed

### Bug 1 (Medium): Buffalo Enteric/Manure EF Routing

**Problem:** Buffalo was routed through the cattle-specific IPCC
tables (Table 10.10 for enteric, Table 10.14 for manure) instead of
the "other livestock" tables (Table 10.11, Table 10.15).

The filter `species_gen %in% c("Cattle", "Buffalo")` in both
`.join_enteric_ef_tier1()` and `.join_manure_ch4_ef_tier1()` grouped
Buffalo with Cattle. This caused Buffalo to receive the "Other
Cattle" Global EF of 47 kg CH₄/head/yr instead of the correct
Buffalo-specific EF of 55.

For manure, the result was coincidentally correct (Buffalo=2 via
Table 10.15 matches "Other Cattle"=2 via Table 10.14 Global), but
the routing was wrong and would produce incorrect results with
regional data.

**Fix:** Changed both filters to `species_gen == "Cattle"` only.
Buffalo now correctly flows through the "other" path and retrieves
its species-specific EFs from Tables 10.11 and 10.15.

**Files modified:**
- `R/livestock_enteric.R` — `.join_enteric_ef_tier1()`
- `R/livestock_manure.R` — `.join_manure_ch4_ef_tier1()`

---

### Bug 2 (High): N₂O Missing Annualization (×365)

**Problem:** Nitrogen excretion (Nex) was calculated as
kgN/head/**day** but all downstream N₂O formulas (direct, indirect)
treated it as an annual value. This caused N₂O emissions to be
underestimated by a factor of 365.

Before fix: Nex = 0.33 → N₂O_total = 0.74 (per 100 heads/day).
After fix: Nex = 120.6 → N₂O_total = 270.1 (per 100 heads/year).

The corrected Nex of ~121 kgN/head/yr aligns with IPCC Table 10.19
(North America dairy: ~100–140 kgN/head/yr).

**Fix:** Added `* livestock_constants$days_in_year` to the Nex
calculation in `.calc_n_excretion()`.

**File modified:** `R/livestock_manure.R`

---

### Bug 3 (Low): Beef Cattle Production Defaults Join Failure

**Problem:** The `livestock_production_defaults` table uses
"Other Cattle" as the category name, but the species string
"Beef Cattle" did not match. This caused the `left_join` in
`.join_production_defaults()` to return NA for all default columns
(including `weight_gain_kg_day = 0.5`), meaning beef cattle without
explicit weight gain data defaulted to 0 (no growth energy).

**Fix:** Added a `defaults_key` column via `dplyr::case_when()`:
- "Dairy Cattle" species → "Dairy Cattle"
- Other cattle (including "Beef Cattle") → "Other Cattle"
- All other species → `species_gen`

The join now operates on `defaults_key` instead of raw `species`.

**File modified:** `R/livestock_energy.R`

---

## 6. Numerical Validation

### Tier 2 — Dairy Cattle Reference Case

Input: species="Dairy Cattle", cohort="Adult Female", weight=600 kg,
milk=20 kg/day, fat=4.0%, diet_quality="High", heads=100.

| Parameter | Value | Unit | Validation |
|---|---|---|---|
| NEm | 46.8 | MJ/day | Cfi=0.386 × 600^0.75 |
| NEa | 7.96 | MJ/day | Ca=0.17 × NEm |
| NEl | 61.78 | MJ/day | 20 × (1.47 + 0.40×4.0) |
| NEp | 4.68 | MJ/day | Cp=0.10 × NEm × frac |
| NEg | 0 | MJ/day | No weight gain for dairy |
| DE% | 75 | % | High diet quality |
| REM | 0.5408 | — | Eq 10.14 |
| REG | 0.3519 | — | Eq 10.15 |
| **GE** | **297.7** | **MJ/day** | Eq 10.16 |
| Ym | 6.5 | % | Table 10.12 |
| **Enteric CH₄/head** | **126.9** | **kg/yr** | GE×Ym/100×365/55.65 |
| VS | 4.16 | kg DM/day | Eq 10.24 |
| Bo | 0.24 | m³/kg VS | Table 10.16 |
| **Manure CH₄/head** | **17.6** | **kg/yr** | VS×365×Bo×0.67×MCF |
| Nex | 120.6 | kgN/head/yr | Eq 10.31 |
| **N₂O direct** | **1.90** | **kg/head/yr** | Eq 10.25 |
| **N₂O total** | **2.70** | **kg/head/yr** | Direct + indirect |

### Tier 2 — Beef Cattle Reference Case

Input: species="Beef Cattle", cohort="Adult Male", weight=500 kg,
diet_quality="Medium", heads=200.

| Parameter | Value | Unit | Note |
|---|---|---|---|
| Weight gain | 0.5 | kg/day | From production defaults |
| NEg | 11.0 | MJ/day | 0.5 × 22.0 energy_content_gain |
| GE | 162.5 | MJ/day | Lower than dairy (no milk) |
| Enteric CH₄/head | 77.1 | kg/yr | — |

### Tier 1 — All 10 Species Validation

| Species | Enteric EF | Manure EF | Source |
|---|---|---|---|
| Dairy Cattle | 80 | 36 | Tables 10.10, 10.14 (Global) |
| Beef Cattle | 47 | 2 | Tables 10.10, 10.14 (Global) |
| Buffalo | 55 | 2 | Tables 10.11, 10.15 |
| Sheep | 8 | 0.19 | Table 10.11, 10.15 |
| Goats | 5 | 0.13 | Table 10.11, 10.15 |
| Swine (avg) | 1.5 | 6 | Table 10.11, 10.15 |
| Poultry | 0 | 0.025 | Table 10.11, 10.15 |
| Horses | 18 | 1.64 | Table 10.11, 10.15 |
| Camels | 46 | 1.92 | Table 10.11, 10.15 |
| Mules and Asses | 10 | 0.9 | Table 10.11, 10.15 |

All EFs verified against IPCC 2019 Refinement tables.

---

## 7. Test Coverage

### Test Files

| File | Tests | Description |
|---|---|---|
| `test_livestock_energy.R` | 18 | All NE components, GE pipeline, defaults |
| `test_livestock_enteric.R` | 11 | Tier 1 EFs, Buffalo routing, Tier 2 range |
| `test_livestock_manure.R` | 15 | CH₄ Tier 1/2, N₂O, Nex annualization |
| `test_livestock_emissions.R` | 6 | Auto-tier selection, forced tiers |
| `test_livestock_cohorts.R` | 3 | Cohort distribution |
| `test_livestock_uncertainty.R` | 3 | Uncertainty bounds |
| **Total** | **56** | |

### Helper Fixtures (`helper_livestock.R`)

| Fixture | Purpose |
|---|---|
| `dairy_tier2_fixture()` | 600 kg dairy cow, 20 kg/day milk, 4% fat |
| `beef_tier2_fixture()` | 500 kg beef, 0.5 kg/day gain, medium diet |
| `tier1_fixture()` | 5 species: Dairy, Beef, Sheep, Swine, Horses |
| `single_tier1_fixture()` | Single species for isolation tests |

### Regression Tests Added

- **Buffalo enteric EF = 55**: Explicit test that Buffalo routes
  through Table 10.11 (not cattle Table 10.10).
- **Buffalo manure EF = 2**: Explicit test for Table 10.15 routing.
- **Nex annualized range 50–200**: Validates Nex is in kgN/head/yr
  (not daily).
- **Beef cattle default weight gain = 0.5**: Validates production
  defaults join works for "Beef Cattle" species string.

---

## 8. Remaining Refinements

Items from `LIVESTOCK_REFINEMENTS.md` reviewed against current
implementation:

### Already Implemented

| Refinement | Status |
|---|---|
| NEl with protein/lactose (NRC 2001) | ✅ Implemented |
| Species-specific energy_content_gain | ✅ In ipcc_tier2_energy_coefs |
| Dynamic DE/REM from diet quality | ✅ Via feed_characteristics |
| Grazing distance activity energy | ✅ Walking cost × weight × km |
| Temperature adjustment | ✅ Via temperature_adjustment table |

### Still Pending

| Refinement | Priority | Description |
|---|---|---|
| Dynamic Ym from diet composition | Low | Calculate Ym from NDF% instead of table lookup |
| Species-specific GE content of feed | Medium | Currently fixed at 18.45 MJ/kg DM for all |
| Regional MMS distributions | Medium | Implemented but untested with real regional data |
| Terrain-based activity adjustment | Low | Flat vs hilly pasture distinction |

---

## 9. IPCC Equation Reference

| Equation | IPCC Ref | Implementation |
|---|---|---|
| NEm | Eq 10.3 | `calc_energy_maintenance()` |
| NEa | Eq 10.4 | `calc_energy_activity()` |
| NEg | Eq 10.6 | `calc_energy_growth()` |
| NEwool | Eq 10.6 | `calc_energy_wool()` |
| NEl (cattle) | Eq 10.8 | `calc_energy_lactation()` |
| NEl (small rum.) | Eq 10.9 | `calc_energy_lactation()` |
| NEwork | Eq 10.11 | `calc_energy_work()` |
| NEp | Eq 10.13 | `calc_energy_pregnancy()` |
| REM | Eq 10.14 | `.calc_rem()` |
| REG | Eq 10.15 | `.calc_reg()` |
| GE | Eq 10.16 | `estimate_gross_energy()` |
| Enteric CH₄ (T2) | Eq 10.21 | `calc_enteric_ch4_tier2()` |
| VS | Eq 10.24 | `.calc_volatile_solids()` |
| Manure CH₄ (T2) | Eq 10.22 | `calc_manure_ch4_tier2()` |
| Nex | Eq 10.31–33 | `.calc_n_excretion()` |
| Direct N₂O | Eq 10.25 | `.calc_direct_n2o()` |
| Indirect N₂O (vol) | Eq 10.26 | `.calc_indirect_n2o()` |
| Indirect N₂O (leach) | Eq 10.28 | `.calc_indirect_n2o()` |
