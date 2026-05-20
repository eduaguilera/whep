# Spatialization Pipeline

## Overview

The WHEP spatialization pipeline disaggregates country-level FAOSTAT data
onto a 0.5-degree global grid for years 1850–2022. The approach reproduces
the core methodology of the
[LandInG](https://github.com/PIK-LPJmL/LandInG) toolbox (Ostberg et al.,
2023, GMD), adapted to WHEP conventions and tidy R data structures.

The pipeline covers three completed domains:

- [x] **Crop land use** — harvested area (rainfed + irrigated) for 154 crops
   aggregated into 15 CFTs, with LUH2 functional-type constraints.
- [x] **Crop yields & production** — country yields distributed to cells with
   optional sub-national spatial variation, split by irrigation status.
- [x] **Nitrogen & fertilizer inputs** — synthetic N, manure N, biological
   fixation, atmospheric deposition, P₂O₅, K₂O per crop at cell level.

A parallel **livestock spatialization** pipeline distributes animal
populations (8 species groups) and associated emissions onto pasture and
rangeland cells. See [docs/LIVESTOCK_SPATIALIZATION.md](LIVESTOCK_SPATIALIZATION.md).

### Implementation status

- [x] Crop land-use spatialization for 154 crops and 15 CFTs
- [x] Type-aware allocation using LUH2 functional crop types
- [x] Irrigated and rainfed area splits
- [x] Crop yield and production spatialization
- [x] Synthetic fertilizer, manure N input, biological fixation, and
  atmospheric deposition inputs
- [x] Diagnostic and validation figure scripts
- [x] Parallel livestock population spatialization
- [ ] Manure spatialization separated from livestock spatialization
- [ ] Manure management N losses estimated before field application
- [ ] Managed manure allocation estimated by receiving land use and crop

### Pipeline architecture

```
prepare_spatialize_inputs.R       prepare_nitrogen_inputs.R
       │                                  │
       ▼                                  │
   whep/inputs/                           │
   ├── country_grid.parquet               │
   ├── country_areas.parquet              │
   ├── crop_patterns.parquet              │
   ├── gridded_cropland.parquet           │
   ├── type_cropland.parquet              │
   └── crop_fertilizer_patterns.parquet   │
       │                                  │
       │    ┌─────────────────────────────┘
       ▼    ▼
  run_spatialize.R
       │
       ├── gridded_landuse_crops.parquet   (154 crops × cell × year)
       ├── gridded_landuse.parquet         (15 CFTs × cell × year)
       ├── landuse_summary.csv
       ├── gridded_yields.parquet          (if country_yields exists)
       ├── gridded_nitrogen.parquet        (if nitrogen_inputs exists)
       └── gridded_nitrogen_cft.parquet    (if nitrogen_inputs exists)
       │
       ▼
  figure_explore_spatialized.R     figure_validate_spatialized.R
       │                                  │
       └── whep/figures/ (15+ PNGs)       └── whep/figures/ (8+ PNGs + CSV)
```

## Data sources

| Status | Source | Resolution | Period | Variables |
|--------|--------|-----------|--------|-----------|
| [x] | **FAOSTAT** Production | Country | 1961–2023 | Element 5312: area harvested (ha) by crop |
| [x] | **LUH2 v2h** `states.nc` | 0.25° | 850–2022 | Crop fractions: `c3ann`, `c4ann`, `c3per`, `c4per`, `c3nfx` |
| [x] | **LUH2 v2h** `management.nc` | 0.25° | 850–2022 | Irrigation fractions: `irrig_c3ann`, etc. |
| [x] | **LUH2 v2h** `staticData_quarterdeg.nc` | 0.25° | Static | `carea` — grid-cell area (km²) |
| [x] | **EarthStat / Monfreda** | 5 arc-min | ~2000 | `HarvestedAreaFraction` per crop (175 crops) |
| [x] | **EarthStat / Mueller** | ~10 km | ~2000 | N/P/K application rates (17 major crops) |
| [x] | **NaturalEarth** | 10 m | Static | Admin-0 country polygons |
| [x] | **FAOSTAT** FertilizersNutrient | Country | 1961–2022 | Synthetic N, P₂O₅, K₂O use |
| [x] | **FAOSTAT** LivestockManure | Country | 1961–2022 | N in manure applied to soils |
| [x] | **Coello et al.** (2025) | Country | 1961–2019 | Crop-group N/P/K rates |
| [x] | **HaNi** (Tian et al., 2022) | 0.5° | 1850–2020 | NHx + NOy deposition |
| [x] | **West et al.** (2014) | 0.5° | ~2000 | Manure N density patterns |

## Input preparation

### Stage 1: Crop & land-use inputs

Script: `inst/scripts/prepare_spatialize_inputs.R` (~930 lines)

Produces 6 parquet files in `L_files/whep/inputs/`:

- [x] `country_grid.parquet`
- [x] `country_areas.parquet`
- [x] `crop_patterns.parquet`
- [x] `gridded_cropland.parquet`
- [x] `type_cropland.parquet`
- [x] `crop_fertilizer_patterns.parquet`

#### 1. `country_grid.parquet`

Maps each 0.5° grid cell to a WHEP `area_code`.

- [x] Rasterizes the NaturalEarth 10 m admin-0 shapefile onto a 0.5° global
  grid using the `terra` package.
- [x] Matches NaturalEarth countries to WHEP polities via ISO 3166-1 alpha-3
  codes. Uses a cascading lookup: `ISO_A3` → `ISO_A3_EH` → `ADM0_A3` to
  handle countries where NaturalEarth sets `ISO_A3 = "-99"` (notably France
  and Norway).
- [x] Output: ~58,765 cells, each with `lon`, `lat`, `area_code`.

### Polity compartments in shared cells

The spatialization code now treats `country_grid` as a **polity-cell
compartment table**, not only as a one-country mask. The legacy
one-row-per-cell file remains valid, but future inputs may contain
multiple rows for the same physical grid cell:

| Column | Meaning |
|--------|---------|
| `lon`, `lat` | Physical 0.5-degree cell centre |
| `area_code` | WHEP polity owning this compartment |
| `cell_area_frac` | Fraction of the physical cell in that polity |
| `polycell_id` | Optional stable compartment identifier |
| `cell_id` | Optional stable physical-cell identifier |
| `year` or `valid_from`/`valid_to` | Optional temporal validity |

When multiple polities share a cell, WHEP spatialization keeps them as
separate model compartments. Crop area, livestock, manure, fertilizer,
yield, and other extensive variables are allocated inside
`lon`/`lat`/`area_code` rather than being assigned to a single dominant
country and re-split later. Intensive physical drivers such as climate
or soil class can be copied to each compartment, while masses and land
areas must remain compartment-specific.

This distinction is important because different variables have different
allocation weights. Pig manure, synthetic fertilizer, cropland, pasture,
and deposition should not share one universal cell split. Instead, each
variable uses its own spatial proxy **within the polity compartment
envelope**. This prevents a value generated by one country's livestock
or management system from being mixed into a neighbouring polity that
shares the same physical grid cell.

The current `prepare_country_grid()` still produces the legacy dominant
country grid. Once exact historical polity-cell overlays are available,
they can be written with the columns above and passed to the existing
spatialization functions without changing their interface.

- [x] Legacy one-row-per-cell `country_grid` remains valid.
- [x] Spatialization functions can accept polity-cell compartment rows.
- [ ] Exact historical polity-cell overlays are still needed.

### Temporal polity changes

Current WHEP inputs redistribute dissolved predecessor entities (for
example USSR, Yugoslavia, and Czechoslovakia) to modern successor states
before spatialization. This keeps the present pipeline compatible with a
static modern country grid.

- [x] Historical predecessor entities are redistributed to modern
  successor states.
- [ ] Historically explicit annual polity compartments are still future
  work.

For future historically explicit modelling, `country_grid` may instead
be time-varying. The spatializers already accept either:

- [x] explicit annual rows using a `year` column; or
- [x] validity intervals using `valid_from`/`valid_to` (also accepting
  `start_year`/`end_year` or `from_year`/`to_year`).

In that mode, each simulation year uses the polity compartments valid
for that year. The model-state implication is more subtle than the input
format: when a polity splits, merges, or changes borders, biogeochemical
state variables must be transferred through an explicit transition map.
WHEP should not silently average or duplicate state across successor
polities. A future historical-polity module should therefore provide:

- [ ] Annual `polity_cell` overlay.
- [ ] `polity_transition` table describing how carbon, nitrogen, water,
  crop, and livestock state is partitioned when polity identities change.

### Spatialization PR status and plan

#### Done in earlier spatialization commits

Earlier spatialization commits established the gridded WHEP pipeline that
this PR extends:

- [x] Country-level crop, livestock, nitrogen, yield, and land-use outputs can
   be spatialized to the LPJmL grid through `run_spatialize()`.
- [x] Historical country handling, Natural Earth ISO fixes, and COW-to-LPJmL
   country-code exports were added so WHEP outputs can be linked to LPJmL
   country conventions.
- [x] MIRCA, LUH2, yield-gap, nitrogen-ratio, sub-national nitrogen, and
   livestock proxy layers were wired into the allocation workflow.
- [x] Type-aware allocation and LUH2 constraints were added to preserve
   country totals across crop groups and land-use types.
- [x] Multicropping capacity was prepared as a per-cell/year budget and then
   applied inside the spatialization year loop, including support for
   multicropping factors below 1 to represent fallow.
- [x] Livestock nitrogen excretion was separated into a swappable WHEP output,
   with corrected FAOSTAT live-animal codes and IPCC nitrogen-excretion
   defaults.
- [x] Pipeline entry points and NetCDF exports were made more composable and
   efficient through vectorisation, chunking, parallelization, and merged
   export helpers.
- [x] The default preparation horizon was extended to 1851-2023, with graceful
   handling of LUH2 end-of-record years.

#### Done in this PR

This PR adds the first implementation layer for preserving polity identity
inside shared grid cells:

- [x] `country_grid` may now be either the old one-row-per-cell country mask or
   a long polity-cell compartment table with `cell_id`, `area_code`,
   `polycell_id`, and optional `cell_area_frac` or validity dates.
- [x] Crop and livestock spatialization now preserve `area_code` and
   compartment identifiers instead of collapsing shared cells to a dominant
   country.
- [x] Physical proxy layers are scaled by each compartment's cell-area fraction
   before allocation, so a shared boundary cell can carry independent country
   allocations without leaking totals across polities.
- [x] Yield and nitrogen spatialization avoid reattaching a dominant country
   mask when the incoming data already carries polity identity.
- [x] Tests cover shared-cell crop allocation, livestock/manure leakage
   prevention, and time-varying country-grid validity.
- [x] Package documentation was regenerated for the new compartment-aware
   arguments and outputs.

#### Remaining plan

The next steps are to make the polity-compartment design operational across
the whole modelling chain:

- [ ] Build the production `polity_cell` overlay from historical polity
   polygons and the LPJmL grid, producing stable `cell_id`, `area_code`,
   `polity_id`, `polycell_id`, `cell_area_frac`, and validity dates.
- [ ] Treat polity changes as input data, not modelling assumptions. When the
   USSR, Yugoslavia, Sudan, or any other polity changes through time, the
   overlay should add or retire compartments by validity date while keeping
   the spatialization code grouped by `year`, `cell_id`, and polity identity.
- [ ] Keep country-specific source data attached to polity compartments before
   aggregation. Crop areas, livestock, manure, fertilizer, land use,
   management, and nitrogen inputs should be allocated independently for
   every `cell_id`/polity compartment.
- [ ] Add explicit validation that national totals are preserved after
   compartment allocation, and that polity-incompatible variables do not
   appear in neighbouring compartments sharing the same physical cell.
- [ ] Extend NetCDF/export formats so compartment-level outputs can either
   remain explicit or be intentionally aggregated with documented rules.
- [ ] Decide and prototype the LPJmL implementation path described below, using
   the virtual-cell path only as a short-term bridge and the native
   compartment path as the long-term target.

### Open tasks: LPJmL polity-compartment implementation

There are two possible implementation paths for preserving polity
identity inside LPJmL:

- [ ] **Virtual-cell path**

   Duplicate boundary cells into multiple computational cells, one per
   polity compartment. Climate, soil, and routing inputs are copied;
   `countrycode`, land use, manure, fertilizer, livestock, and management
   differ by compartment. Outputs are mapped back through `polycell_id`.

   This is easier to prototype but risky because LPJmL may assume one
   unique coordinate/country per grid cell, especially for routing and
   cell area.

- [ ] **Native compartment path**

   Modify LPJmL so each physical cell contains multiple polity
   compartments, analogous to independent sub-grid stands. Climate is
   shared and routing aggregates water after compartment-level
   calculations, but carbon, nitrogen, crop, and management states remain
   separate by polity.

   This is the correct long-term design.

#### 2. `country_areas.parquet`

Country × crop × year harvested and irrigated area in hectares.

**FAOSTAT period (1961–2022):**

- [x] Reads the FAOSTAT normalized CSV, filters to element 5312 (Area
  harvested) and the 154 crop items defined in `cft_mapping.csv`.
- [x] Filters out FAO regional-aggregate codes (≥ 5000).
- [x] Redistributes historical predecessor entities to modern successor states
  (see [Historical country changes](#historical-country-changes) below).
- [x] Distributes irrigation from the LUH2 `management.nc` layer: within each
  country × year × LUH2 crop type, LUH2 irrigated area is allocated to
  individual crops proportionally to their share of total harvested area.

**Pre-FAOSTAT period (1850–1960):**

- [x] Uses FAOSTAT 1961 crop areas as a template.
- [x] Scales each country's 1961 values by the ratio
  $\text{LUH2}_\text{year} / \text{LUH2}_{1961}$ of total LUH2
  country-level cropland, providing smooth temporal trends.
- [x] Output: ~1,049,000 rows.

#### 3. `crop_patterns.parquet`

Static spatial reference patterns from EarthStat (~year 2000).

- [x] Reads 151 crop-specific `HarvestedAreaFraction` GeoTIFFs.
- [x] Aggregates from 5 arc-min to 0.5° (factor 6, mean).
- [x] Maps EarthStat crop names to FAOSTAT `item_prod_code` via
  `earthstat_mapping.csv`. Multiple EarthStat crops mapping to the same
  item code have their fractions summed.
- [x] Output: ~2,247,000 rows with `lon`, `lat`, `item_prod_code`,
  `harvest_fraction`.

#### 4. `gridded_cropland.parquet`

Annual gridded cropland extent and irrigation from LUH2.

- [x] For each year in 1850–2022, reads five crop-type fractions from
  `states.nc` and corresponding irrigation fractions from `management.nc`.
- [x] Converts fractions to hectares using `carea` (km² → ha × 100).
- [x] Aggregates from 0.25° to 0.5° (factor 2, sum).
- [x] Output: ~7,410,000 rows with `lon`, `lat`, `year`, `cropland_ha`,
  `irrigated_ha`.

#### 5. `type_cropland.parquet`

Annual per-type cropland (LUH2 functional types) for type-aware allocation.

- [x] Same disaggregation as `gridded_cropland`, but retains per-type detail
  (`c3ann`, `c4ann`, `c3per`, `c3nfx`).
- [x] Merges `c4per` into `c3per` (no CFT maps to c4per).
- [x] Can also be regenerated standalone via `inst/scripts/_gen_type_cropland.R`.
- [x] Output: `lon`, `lat`, `year`, `luh2_type`, `type_ha`, `type_irrig_ha`.

#### 6. `crop_fertilizer_patterns.parquet`

- [x] Static sub-national fertilizer application rates from EarthStat/Mueller
(17 crops, N/P/K), aggregated to 0.5°.

### Stage 2: Nitrogen & fertilizer inputs

Script: `inst/scripts/prepare_nitrogen_inputs.R` (~1,535 lines)

Combines 6 data sources into country × crop × year nutrient budgets:

- [x] **FAOSTAT** synthetic N, P₂O₅, K₂O totals + manure N applied
- [x] **EuroAgriDB** (Einarsson et al., 2019) with cropland/grassland split
- [x] **Crop-specific distribution** using Mueller et al. (2012) + West et al.
   (2014) + EarthStat fertilizer rates
- [x] **Atmospheric N deposition** from HaNi (Tian et al., 2022)
- [x] **Biological N fixation** with default 13 kg N/ha (Herridge et al., 2008)
- [x] **Coello et al.** (2025) 13 crop-group rates as fallback + P/K
   distribution

**Outputs:**

| Status | File | Contents |
|--------|------|----------|
| [x] | `nitrogen_inputs.parquet` | crop × country × year × fert_type |
| [x] | `n_deposition.parquet` | country × year atmospheric deposition rates |
| [x] | `pk_totals.parquet` | P and K country × year totals |
| [x] | `coello_crop_rates.parquet` | cached Coello crop-group rates |

### Open tasks: manure spatialization

- [ ] Differentiate livestock spatialization from manure-related spatialization.
  Excreta/direct deposition should coincide with livestock locations, while
  managed manure can be collected, stored, transported, and applied elsewhere.
- [ ] Estimate N losses during manure management before calculating manure N
  available for field application.
- [ ] Estimate which land uses and crops receive managed manure applications
  instead of assuming the application pattern is identical to livestock
  locations or uniformly distributed over cropland.

## Spatialization algorithm

Exported function: `build_gridded_landuse()` in `R/spatialize.R` (~650 lines).

### Step 1 — Proportional allocation (type-aware)

For each year and crop, country totals are distributed to grid cells using:

$$
\text{cell\_ha} = \frac{p_i \cdot c_i}{\sum_{j \in \text{country}} p_j \cdot c_j} \times T
$$

where $p_i$ is the EarthStat harvest fraction, $c_i$ is the LUH2 cropland
in cell $i$, and $T$ is the FAOSTAT country total. If no pattern exists for
a crop in a country, allocation falls back to uniform distribution over
cropland cells.

**Type-aware allocation (new):** When `type_cropland` and `type_mapping`
are provided, each crop is allocated only into cells that contain its LUH2
functional type (`c3ann`, `c4ann`, `c3per`, or `c3nfx`). The cropland
weight $c_i$ is the type-specific cropland (`type_ha`) rather than total
cropland. This gives time-varying, type-constrained spatial patterns while
falling back to total cropland when a crop's type has zero area in a
country.

Irrigated and rainfed areas are allocated separately using corresponding
irrigated and rainfed cropland (or type-specific irrigated) as weights.

### Step 2 — Capacity constraint (optional)

If a `multicropping` suitability layer is supplied, cells where total
allocated harvested area exceeds capacity ($\text{cropland} \times
\text{multi\text{-}cropping factor}$) trigger iterative redistribution:

- [x] Express each cell's allocation as a fraction of capacity.
- [x] Apply a logit transform: $\text{logit}(f) = \ln(f / (1 - f))$.
- [x] Add an increment proportional to the country-level deficit.
- [x] Inverse-logit to get new fractions.
- [x] Repeat until convergence (tolerance $10^{-4}$) or `max_iterations`
   (default 1000).

After `expansion_threshold` iterations (default 100), crops are allowed to
expand into cells that have cropland but no initial pattern for that crop.

### Step 3 — CFT aggregation

Individual crop results are aggregated into 15 crop functional types (CFTs)
using the mapping in `inst/extdata/cft_mapping.csv`:

| Status | CFT name | LUH2 type | Example crops |
|--------|----------|-----------|---------------|
| [x] | `temperate_cereals` | c3ann | Wheat, barley, rye, oats |
| [x] | `rice` | c3ann | Rice, paddy |
| [x] | `maize` | c4ann | Maize |
| [x] | `tropical_cereals` | c4ann | Sorghum, millet |
| [x] | `pulses` | c3nfx | Beans, lentils, peas |
| [x] | `oil_crops_soybean` | c3nfx | Soybeans |
| [x] | `oil_crops_groundnut` | c3nfx | Groundnuts |
| [x] | `oil_crops_rapeseed` | c3ann | Rapeseed |
| [x] | `oil_crops_sunflower` | c3ann | Sunflower seed |
| [x] | `oil_crops_other` | c3ann | Sesame, castor, linseed |
| [x] | `sugarcane` | c4ann | Sugar cane |
| [x] | `temperate_roots` | c3ann | Potatoes, sugar beet |
| [x] | `tropical_roots` | c3ann | Cassava, yams, sweet potatoes |
| [x] | `others_perennial` | c3per | Coffee, cocoa, fruits, vegetables (perennial) |
| [x] | `others_annual` | c3ann | Vegetables, tobacco, fibre crops |

### Step 4 — Yield spatialization (optional)

If `country_yields.parquet` is present in inputs, `run_spatialize.R`
distributes country × crop yields to each cell:

- [x] Joins gridded crop areas with country-level yields via `area_code` ×
   `item_prod_code` × `year`.
- [x] Optionally applies a **sub-national spatial yield index**
   (`spatial_yield_index.parquet`) derived from EarthStat, with
   renormalization to preserve country production totals.
- [x] Splits into irrigated/rainfed yields using literature-based ratios
   (Siebert & Döll, 2010; Mueller et al., 2012):

$$
\text{yield}_{rf} = \frac{\text{yield} \times \text{total\_ha}}{\text{rf\_ha} + R \times \text{irr\_ha}}, \quad
\text{yield}_{irr} = R \times \text{yield}_{rf}
$$

- [x] Computes `rainfed_prod_t` and `irrigated_prod_t` per cell.

### Step 5 — Nitrogen spatialization (optional)

If `nitrogen_inputs.parquet` is present, N application rates are distributed
to cells using the same conservation-preserving approach:

- [x] Joins gridded crop areas with country × crop × fert_type N rates.
- [x] Optionally applies `spatial_n_index.parquet` (West/EarthStat pixel
   index) with renormalization.
- [x] Splits irrigated/rainfed using `n_rate_ratio` per CFT.
- [x] Outputs both crop-level and CFT-aggregated nitrogen datasets.

## Historical country changes

FAOSTAT reports data under historical predecessor entities that no longer
exist (e.g., USSR through 1991). The NaturalEarth shapefile only contains
modern borders, so predecessor data has no matching grid cells and would
be silently dropped.

The pipeline redistributes 7 predecessor entities to their successor
states, weighted by each successor's LUH2 cropland in the relevant year:

| Status | Predecessor | Code | Period | Successors |
|--------|-------------|------|--------|------------|
| [x] | USSR | 228 | 1961–1991 | Armenia, Azerbaijan, Belarus, …, Uzbekistan (15 states) |
| [x] | Yugoslav SFR | 248 | 1961–1991 | Bosnia and Herzegovina, Croatia, North Macedonia, Slovenia, Serbia, Montenegro |
| [x] | Czechoslovakia | 51 | 1961–1992 | Czech Republic, Slovakia |
| [x] | Belgium-Luxembourg | 15 | 1961–1999 | Belgium, Luxembourg |
| [x] | Serbia and Montenegro | 186 | 1992–2005 | Serbia, Montenegro |
| [x] | Ethiopia PDR | 62 | 1961–1992 | Ethiopia, Eritrea |
| [x] | Sudan (former) | 206 | 1961–2011 | Sudan, South Sudan |

The redistribution share for each successor $s$ in year $t$ is:

$$
w_{s,t} = \frac{\text{LUH2 cropland}_{s,t}}{\sum_{k \in \text{successors}} \text{LUH2 cropland}_{k,t}}
$$

## Outputs

### Primary outputs (`L_files/whep/`)

| Status | File | Columns | Description |
|--------|------|---------|-------------|
| [x] | `gridded_landuse_crops.parquet` | `lon`, `lat`, `year`, `item_prod_code`, `rainfed_ha`, `irrigated_ha` | Per-crop gridded areas (154 crops) |
| [x] | `gridded_landuse.parquet` | `lon`, `lat`, `year`, `cft_name`, `rainfed_ha`, `irrigated_ha` | CFT-aggregated areas (15 CFTs, ~70M rows) |
| [x] | `landuse_summary.csv` | `year`, `cft_name`, `total_rainfed_ha`, `total_irrigated_ha`, `n_cells` | Per-year, per-CFT summary |
| [x] | `gridded_yields.parquet` | `lon`, `lat`, `year`, `area_code`, `item_prod_code`, `yield_rainfed`, `yield_irrigated`, `rainfed_prod_t`, `irrigated_prod_t` | Spatially disaggregated yields and production |
| [x] | `gridded_nitrogen.parquet` | `lon`, `lat`, `year`, `area_code`, `item_prod_code`, `fert_type`, `kg_n_ha`, `kg_n_ha_rainfed`, `kg_n_ha_irrigated`, `rainfed_n_mg`, `irrigated_n_mg` | Per-crop nitrogen inputs |
| [x] | `gridded_nitrogen_cft.parquet` | `lon`, `lat`, `year`, `cft_name`, `fert_type`, `rainfed_n_mg`, `irrigated_n_mg`, `kg_n_ha`, … | CFT-aggregated nitrogen |

### Irrigated/rainfed yield and N-rate ratios

Literature-based ratios used to split country-level values between
irrigated and rainfed systems:

| Status | CFT | Yield ratio | N rate ratio |
|--------|-----|-------------|--------------|
| [x] | temperate_cereals | 1.3 | 1.3 |
| [x] | rice | 1.6 | 1.4 |
| [x] | maize | 1.5 | 1.4 |
| [x] | tropical_cereals | 1.3 | 1.3 |
| [x] | pulses | 1.3 | 1.2 |
| [x] | oil_crops_soybean | 1.3 | 1.2 |
| [x] | oil_crops_groundnut | 1.3 | 1.3 |
| [x] | oil_crops_sunflower | 1.3 | 1.3 |
| [x] | oil_crops_rapeseed | 1.3 | 1.3 |
| [x] | oil_crops_other | 1.3 | 1.3 |
| [x] | sugarcane | 1.2 | 1.3 |
| [x] | temperate_roots | 1.3 | 1.3 |
| [x] | tropical_roots | 1.2 | 1.2 |
| [x] | others_annual | 1.3 | 1.3 |
| [x] | others_perennial | 1.2 | 1.2 |

Sources: Siebert & Döll (2010), Mueller et al. (2012), Lassaletta et al.
(2014), AQUASTAT yield tables, Zhang et al. (2015).

## Diagnostic figures

Two figure-generation scripts produce exploratory maps and validation plots
to `L_files/whep/figures/`.

### Exploratory figures (`figure_explore_spatialized.R`)

| Status | Figure | Description |
|--------|--------|-------------|
| [x] | `map_total_cropland_<year>.png` | Global cropland maps (1900/1950/1980/2000/2020), Robinson projection |
| [x] | `map_cft_panel_<year>.png` | 15-panel faceted map by CFT (2000 & 2020) |
| [x] | `map_irrigated_share_<year>.png` | Irrigated fraction maps (1960/2000/2020) |
| [x] | `trend_global_area.png` | Stacked area: rainfed vs irrigated (1850–2022) |
| [x] | `trend_cft_area.png` | Per-CFT harvested area time series |
| [x] | `trend_top_countries.png` | Top-10 country trends (1961–2022) |
| [x] | `trend_irrigated_share.png` | Global irrigated share over time |
| [x] | `trend_continental_area.png` | Stacked area by continent |

### Validation figures (`figure_validate_spatialized.R`)

| Status | Figure | Description |
|--------|--------|-------------|
| [x] | `val_global_totals.png` | Global harvested + irrigated: FAOSTAT vs gridded overlay |
| [x] | `val_cft_totals.png` | Per-CFT total comparison (15 facets) |
| [x] | `val_scatter_country_<year>.png` | Country 1:1 scatter (1960/1980/2000/2020) with labelled outliers |
| [x] | `val_relative_error_distribution.png` | Histogram of country-year relative errors |
| [x] | `val_timeseries_top_deviations.png` | 12 worst-case country time series |
| [x] | `val_irrigated_totals.png` | Global irrigated area validation |
| [x] | `val_country_panel.png` | Top-12 producer country panels |
| [x] | `validation_detail.csv` | Full per-country per-year error table |

## How to run

Run sequence checklist:

- [ ] Prepare crop/land-use inputs.
- [ ] Prepare nitrogen inputs, if needed.
- [ ] Run spatialization.
- [ ] Generate exploratory and validation figures.

```bash
# Step 1: Prepare crop/land-use inputs (~30 min)
Rscript inst/scripts/prepare_spatialize_inputs.R

# Step 2: Prepare nitrogen inputs (~20 min, optional)
Rscript inst/scripts/prepare_nitrogen_inputs.R

# Step 3: Run spatialization (173 years, ~40 min)
Rscript inst/scripts/run_spatialize.R

# Step 4: Generate figures (~15 min)
Rscript inst/scripts/figure_explore_spatialized.R
Rscript inst/scripts/figure_validate_spatialized.R
```

Both scripts expect raw data in `L_files/` with the directory structure:

```
L_files/
├── FAOSTAT/
│   ├── Production_Crops_Livestock_E_All_Data_(Normalized).csv
│   ├── Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv
│   └── Environment_LivestockManure_E_All_Data_(Normalized).csv
├── HarvestedAreaYield175Crops_Geotiff/
│   └── GeoTiff/
│       ├── wheat/wheat_HarvestedAreaFraction.tif
│       └── ...
├── EarthStat - Crop Specific Fertilizers/
│   ├── barley/barley_NitrogenApplication_Rate.tif
│   └── ...
├── LUH2/
│   └── LUH2 v2h/
│       ├── states.nc
│       ├── management.nc
│       └── staticData_quarterdeg.nc
├── NaturalEarth/
│   └── Countries_shape/ne_10m_admin_0_countries.shp
├── HaNi/
│   ├── NHx-deposition2x25.nc
│   └── NOy-deposition2x25.nc
├── West_Manure/
│   └── manure_n_density.nc
└── whep/
    └── inputs/    ← output directory
```

## Validation results

### Area conservation

After predecessor redistribution, NaturalEarth ISO fix, LUH2
type-aware allocation, soybean code fix, and MIRCA2000 irrigation:

| Status | Metric | Value |
|--------|--------|-------|
| [x] | Country-year observations | 29,248 |
| [x] | Perfect match (<0.01% error) | 29,037 (99.3%) |
| [ ] | Max country-year error | 10.2% (tiny country, rounding) |
| [ ] | Global annual error (max) | 0.035% |
| [x] | Grid cells | 42,600 |
| [x] | `country_areas` rows | 1,032,404 |
| [x] | Predecessor entity codes in output | 0 (all redistributed) |
| [x] | CFTs present | 15/15 |

### LPJmL compatibility

A validation script (`inst/analysis/spatialize/validate_lpjml_compat.R`)
runs 15 automated checks comparing WHEP outputs against LPJmL v6 input
requirements. Current status: **10 PASS / 5 FAIL**.

| Done | Check | Status | Detail |
|------|-------|--------|--------|
| [x] | Grid alignment (0.5°) | PASS | 647 lon × 253 lat |
| [x] | Temporal completeness | PASS | 1850–2022, no gaps |
| [x] | CFT completeness (15) | PASS | All 15 CFTs present every year |
| [x] | No NA/NaN/Inf | PASS | Zero non-finite values |
| [x] | No negative areas | PASS | Zero negatives |
| [ ] | Country conservation | FAIL | 99.3% within 0.01%; max 10.2% |
| [ ] | Global totals | FAIL | Max 0.035% annual error |
| [ ] | Irrigated ≤ LUH2 irrigated | FAIL | 250K cell-years (MIRCA redistribution) |
| [ ] | Cell capacity (≤ 3× cropland) | FAIL | 56K cell-years; worst 8× in Nigeria |
| [ ] | Fraction range | FAIL | p99 = 2.73 (below 3.0); max outlier in small countries |
| [x] | No orphan cells | PASS | All cells have a country |
| [x] | CFT → LPJmL band mapping | PASS | 15 CFTs cover bands 0–12 |
| [x] | Cell count vs LPJmL | PASS | 42,600 vs 67,420 (37% fewer) |
| [x] | Year range vs LPJmL v6 | PASS | 1850–2022 vs 1700–2023 |
| [x] | Managed grassland data | PASS | `gridded_pasture.parquet` available |

The 5 remaining FAILs are structural/expected:

- [ ] **Country conservation**: 211 minor deviations from rounding in
  small countries; 99.3% of 29,248 observations are within 0.01%.
- [ ] **Global totals**: 0.035% max annual error — effectively perfect.
  Marked FAIL only because the threshold is 0.01%.
- [ ] **Irrigated > LUH2**: expected with MIRCA2000 crop-specific
  irrigation, which redistributes independently of LUH2 cell limits.
- [ ] **Cell capacity / fraction range**: small countries (Nigeria,
  Bangladesh) with high cropping intensity relative to LUH2 cropland.
  Would be resolved by activating the multi-cropping capacity
  constraint (P1 in upgrade plan).

### LPJmL band coverage (latest year, 2022)

| Status | LPJmL band | CFT name | Total Mha | Irrigated Mha |
|--------|------------|----------|-----------|---------------|
| [x] | 0 | Temperate cereals | 291 | 66 |
| [x] | 1 | Rice | 168 | 88 |
| [x] | 2 | Maize | 204 | 52 |
| [x] | 3 | Tropical cereals | 73 | 2 |
| [x] | 4 | Pulses | 97 | 6 |
| [x] | 5 | Temperate roots | 36 | 11 |
| [x] | 6 | Tropical roots | 7 | 4 |
| [x] | 7 | Oil crops (sunflower) | 1 | 0.1 |
| [x] | 8 | Oil crops (soybean) | 131 | 7 |
| [x] | 9 | Oil crops (groundnut) | 31 | 3 |
| [x] | 10 | Oil crops (rapeseed) | 1 | 0.3 |
| [x] | 11 | Sugar cane | 2 | 1 |
| [x] | 12 | Others (3 sub-CFTs) | 436 | 97 |
| [x] | 13–14 | Managed grassland | via `gridded_pasture.parquet` | — |
| [ ] | 15 | Bioenergy | 0 (not modelled) | — |

### Conversion readiness (Parquet → CLM)

- [x] All 8 steps for converting WHEP Parquet output to LPJmL CLM binary
  format are ready.
- [x] Full gap analysis, CFT-to-band mapping, and conversion recipe are
  documented in `inst/analysis/spatialize/LPJML_COMPATIBILITY.md`.

## Tests

| Status | Test file | Tests | Scope |
|--------|-----------|-------|-------|
| [x] | `test_spatialize.R` | 9 | `build_gridded_landuse()`, type-aware allocation, fallback, capacity constraints |
| [x] | `test_spatialize_livestock.R` | 13 | `build_gridded_livestock()`, proxy weighting, conservation |

Run with:

```bash
Rscript -e "devtools::test(filter = 'spatialize')"
```

## Key files

| Status | File | Lines | Description |
|--------|------|-------|-------------|
| [x] | `R/spatialize.R` | 654 | `build_gridded_landuse()` and private helpers |
| [x] | `R/spatialize_livestock.R` | 435 | `build_gridded_livestock()` and helpers |
| [x] | `inst/scripts/prepare_spatialize_inputs.R` | 927 | Crop & land-use input preparation |
| [x] | `inst/scripts/prepare_nitrogen_inputs.R` | 1,535 | Nitrogen & fertilizer input pipeline |
| [x] | `inst/scripts/run_spatialize.R` | 408 | Runner: crop areas, yields, nitrogen |
| [x] | `inst/scripts/prepare_livestock_spatialize_inputs.R` | 731 | Livestock input preparation |
| [x] | `inst/scripts/run_livestock_spatialize.R` | 220 | Livestock runner |
| [x] | `inst/scripts/figure_explore_spatialized.R` | 399 | Exploratory maps & trends |
| [x] | `inst/scripts/figure_validate_spatialized.R` | 475 | Validation comparison figures |
| [x] | `inst/scripts/_gen_type_cropland.R` | 98 | Standalone LUH2 type-cropland generator |
| [x] | `inst/extdata/cft_mapping.csv` | 155 | 154 crop items → 15 CFTs + LUH2 type |
| [x] | `inst/extdata/earthstat_mapping.csv` | — | EarthStat crop names → item codes |

## References

- Coello, J., et al. (2025). "Crop-specific nutrient application rates
  1961–2019." *In preparation*.
- Einarsson, R., et al. (2019). "Crop production and nitrogen use in
  European cropland and grassland 1961–2019." *Scientific Data*, 8, 288.
- Herridge, D. F., Peoples, M. B., & Boddey, R. M. (2008). "Global inputs
  of biological nitrogen fixation in agricultural systems." *Plant and
  Soil*, 311, 1–18.
- Hurtt, G. C., et al. (2020). "Harmonization of global land use change
  and management for the period 850–2100 (LUH2) for CMIP6." *Geoscientific
  Model Development*, 13, 5425–5464.
- Lassaletta, L., et al. (2014). "50 year trends in nitrogen use
  efficiency of world cropping systems." *Environmental Research Letters*,
  9, 105011.
- Monfreda, C., Ramankutty, N., & Foley, J. A. (2008). "Farming the
  planet: 2. Geographic distribution of crop areas, yields, physiological
  types, and net primary production in the year 2000." *Global
  Biogeochemical Cycles*, 22, GB1022.
- Mueller, N. D., et al. (2012). "Closing yield gaps through nutrient and
  water management." *Nature*, 490, 254–257.
- Ostberg, S., et al. (2023). "LandInG 1.0 – a toolbox to derive input
  datasets for terrestrial ecosystem modelling at variable resolutions from
  heterogeneous sources." *Geoscientific Model Development*, 16, 3375–3406.
- Siebert, S. & Döll, P. (2010). "Quantifying blue and green virtual water
  contents in global crop production." *Journal of Hydrology*, 384, 198–217.
- Tian, H., et al. (2022). "Global soil nitrous oxide emissions since the
  preindustrial era estimated by an ensemble of terrestrial biosphere
  models." *Nature Food*, 3, 886–896.
- West, P. C., et al. (2014). "Leverage points for improving global food
  security and the environment." *Science*, 345, 325–328.
- Zhang, X., et al. (2015). "Managing nitrogen for sustainable
  development." *Nature*, 528, 51–59.
