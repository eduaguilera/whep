# Spatialization Pipeline — Next Major Upgrade Plan

**Date:** 2025-01 (updated 2025-02)  
**Branch:** `edu/build-production-cbs`  
**Status:** Active — Phase 1 complete, Phase 2 in progress  

## Current state summary

The spatialization pipeline is functional end-to-end:

- **Crop land use**: 154 crops → 15 CFTs, 0.5° global, 1850–2022, type-aware
  LUH2 allocation, 100% area conservation verified.
- **Yields & production**: Country yields spatialized with optional
  sub-national index, irrigated/rainfed split.
- **Nitrogen & fertilizer**: 6 data sources, crop-specific N/P/K rates,
  irrigated/rainfed split.
- **Livestock**: 8 species groups, pasture/rangeland proxy weighting, enteric
  CH₄ + manure emissions spatialized.
- **Diagnostics**: 15 exploratory figures, 7 validation figures + CSV.

### Known limitations

| Area | Limitation | Status |
|------|-----------|--------|
| Crop patterns | Static EarthStat ~2000 patterns; no temporal evolution | Open |
| Multi-cropping | `multicropping` parameter exists but always NULL | Open (needs GAEZ data) |
| Irrigation | ~~LUH2 irrigation only~~ → MIRCA2000 crop-specific irrigation integrated | **✅ Resolved (P4)** |
| Yields | ~~Spatially uniform within countries~~ → EarthStat sub-national index | **✅ Resolved (P2)** |
| Livestock density | GLW3 `density_grid` parameter exists but not integrated | Open (needs GLW3 data) |
| Livestock validation | ~~No validation figures~~ → 8 explore + 7 validation figures | **✅ Resolved (P6)** |
| Performance | Full run ~40 min; 113M-row output; no parallel processing | Open |
| Forward projections | LUH2 SSP scenario support not implemented | Open |

---

## Upgrade priorities

### Priority 1 — Multi-cropping suitability layer

**Goal:** Activate the existing capacity-constraint mechanism.

The logit-based redistribution algorithm is fully implemented and tested in
`R/spatialize.R` (`.apply_capacity_constraint()`,
`.redistribute_single_country()`). What's missing is the suitability input.

**Data source options:**

| Dataset | Resolution | Coverage | Pros | Cons |
|---------|-----------|----------|------|------|
| **GAEZ v4** (FAO/IIASA) | 5 arc-min | Global, per crop | Official FAO, crop-specific | Large download, complex format |
| **Ray & Foley (2013)** | 5 arc-min | Global | Published, simple | Dated (~2008 data) |
| **Waha et al. (2020)** | 0.5° | Global | LPJmL-based, matches LandInG | Model-dependent |

**Implementation steps:**

1. Download and rasterize the chosen multi-cropping suitability dataset.
2. Aggregate to 0.5° grid (max suitability factor per cell).
3. Add preparation step to `prepare_spatialize_inputs.R` → output
   `multicropping.parquet` with columns `lon`, `lat`, `suitability`.
4. Pass to `build_gridded_landuse(multicropping = ...)` in
   `run_spatialize.R`.
5. Validate: check that no cell exceeds physical capacity while country
   totals are preserved.

**Effort:** ~2 days (data prep + integration + validation).

---

### Priority 2 — Sub-national yield patterns ✅ COMPLETE

**Goal:** Replace spatially-uniform country yields with sub-national
variation using EarthStat or SPAM yield data.

**Current state:** COMPLETE. `prepare_yield_inputs.R` reads 169 EarthStat
crop rasters, aggregates to 0.5°, and produces `country_yields.parquet`
(395,022 rows) + `spatial_yield_index.parquet` (1,848,676 pixels, mean
0.78, sd 0.42, range [0.1, 5.0]). Fully integrated in `run_spatialize.R`;
`gridded_yields.parquet` = 113.7 M rows.

**Data source options:**

| Dataset | Resolution | Crops | Period |
|---------|-----------|-------|--------|
| **EarthStat** (Monfreda 2008) | 5 arc-min | 175 crops | ~2000 |
| **MapSPAM 2020v1** (IFPRI) | 5 arc-min | 42 crops | 2020 |
| **GAEZ v4** attainable yield | 5 arc-min | 12 crops | Multiple |

**Implementation steps:**

1. Read EarthStat `*_YieldPerHectare.tif` for each crop (already partially
   mapped via `earthstat_mapping.csv`).
2. Aggregate to 0.5° (weighted mean by area fraction).
3. Compute `yield_index = pixel_yield / country_mean_yield` per crop ×
   country.
4. Save as `spatial_yield_index.parquet` (`lon`, `lat`, `item_prod_code`,
   `yield_index`).
5. Integration in `run_spatialize.R` is already coded — just needs the file.

**Effort:** ~1–2 days.

---

### Priority 3 — GLW3 livestock density integration

**Goal:** Use species-specific animal density grids to improve livestock
spatial allocation beyond pasture-area proxies.

**Data source:** Gridded Livestock of the World v3 (Gilbert et al., 2018)
— 10 km resolution, 6 species (cattle, buffaloes, sheep, goats, pigs,
chickens).

**Current state:** `R/spatialize_livestock.R` already has full GLW3 support
in `.build_proxy_weight()` — it checks for a `density_grid` tibble and
uses it as weight when available, falling back to pasture-only weighting.

**Implementation steps:**

1. Download GLW3 GeoTIFFs from
   <https://www.fao.org/livestock-systems/global-distributions/en/>.
2. Aggregate to 0.5° (sum of heads per cell).
3. Map GLW3 species to WHEP species groups.
4. Save as `glw3_density.parquet` (`lon`, `lat`, `species_group`,
   `density_heads`).
5. Load in `run_livestock_spatialize.R` and pass as `density_grid`.
6. Compare distributions with/without GLW3 weights.

**Effort:** ~2 days.

---

### Priority 4 — Crop-specific irrigation patterns (MIRCA2000) ✅ COMPLETE

**Goal:** Replace LUH2 irrigation (5 functional types only) with
crop-specific irrigated area patterns.

**Current state:** COMPLETE. `prepare_mirca_inputs.R` reads all 26 MIRCA2000
binary grids (5 arc-min, 4320×2160×12 months), aggregates peak-month
areas to 0.5°, and maps to 131 FAOSTAT crop codes via `mirca_mapping.csv`
+ `cft_mapping.csv`.

Outputs:
- `mirca_irrigation_country.parquet`: 22,791 rows (174 countries × 131
  crops) with `irrig_frac` per crop × country.
- `mirca_irrigation_patterns.parquet`: 7,668,015 rows of pixel-level data.
- Global totals: 492.8 Mha irrigated + 1,654.5 Mha rainfed = 2,147.3 Mha.

Integration: `prepare_spatialize_inputs.R` automatically detects
`mirca_irrigation_country.parquet` and uses MIRCA crop-specific fractions
to distribute irrigated area. Crops without MIRCA data fall back to LUH2
type-proportional allocation. `country_areas.parquet` regenerated: 17.2%
global irrigation fraction, 87.3% of records have irrigation > 0.

Key irrigation fractions: Rice 63%, Date palm 86%, Sugarcane 54%, Citrus
56%, Cotton 44%, Wheat 30%, Maize 23%, Cocoa/Oil palm/Cassava ~0%.

**Effort:** Completed in ~3 days as estimated.

---

### Priority 5 — Temporal pattern evolution

**Goal:** Move beyond static ~year-2000 EarthStat patterns to decade-
specific crop distributions.

**Approaches:**

**Option A — MapSPAM multi-year:**

MapSPAM has releases for 2000, 2005, 2010, 2017, 2020. Interpolate
patterns between baseline years.

**Option B — LUH2-adjusted patterns:**

Scale static EarthStat patterns by LUH2 cropland changes:

$$
p_{i,t} = p_{i,2000} \times \frac{\text{LUH2 cropland}_{i,t}}{\text{LUH2 cropland}_{i,2000}}
$$

This is partially implicit in the current algorithm (cropland enters as
weight), but making it explicit would allow better handling of frontier
expansion.

**Option C — Machine-learning reconstruction:**

Train on MapSPAM snapshots to predict inter-temporal patterns using
covariates (climate, irrigation, population).

**Recommendation:** Start with Option B (cheapest), later adopt Option A
when MapSPAM licence allows.

**Effort:** 2–5 days depending on option.

---

### Priority 6 — Livestock validation & diagnostics ✅ COMPLETE

**Goal:** Create validation figures for the livestock pipeline analogous to
the crop validation suite.

**Current state:** COMPLETE.

`figure_explore_livestock.R` — 8 figures:
- Maps: livestock heads 1900/1960, enteric CH₄ 1960/2000
- Trends: global livestock, species-level, top countries, emissions,
  continental breakdown

`figure_validate_livestock.R` — 7 figures + CSV:
- Global/species totals, country-level scatter plots (1960/1980/2000),
  error distribution, top deviations, emissions totals, country panel
- **Result: 125,846 observations → 100% perfect match** (<0.01% deviation)
- `validation_livestock_detail.csv` saved for downstream use

**Effort:** Completed in ~1 day.

---

### Priority 7 — Performance optimization

**Goal:** Reduce runtime from ~40 minutes to <15 minutes.

**Approaches:**

| Approach | Expected speedup | Complexity |
|----------|-----------------|------------|
| **Parallelize years** with `future` + `furrr` | 3–4× (8 cores) | Low |
| **Arrow-native joins** (replace dplyr joins on large tables with arrow) | 1.5–2× | Medium |
| **Pre-filter patterns** (drop zero-harvest cells before join) | 1.3× | Low |
| **Chunk I/O** (write per-decade parquet partitions) | Memory savings | Medium |

**Implementation steps:**

1. Profile with `profvis` to identify bottlenecks.
2. Implement `future::plan(multisession)` in year loop.
3. Partition output by decade for lazy reading downstream.
4. Benchmark on standard hardware.

**Effort:** ~2–3 days.

---

### Priority 8 — LUH2 SSP forward projections

**Goal:** Extend spatialization from 2022 into 2023–2100 using LUH2 v2f
scenario data.

**Data:** LUH2 provides `states.nc` and `management.nc` for SSP1-2.6,
SSP2-4.5, SSP3-7.0, SSP5-8.5 (2015–2100).

**Implementation steps:**

1. Download LUH2 v2f scenario files.
2. Extend `prepare_spatialize_inputs.R` to optionally read future scenario
   files and produce `gridded_cropland_ssp*.parquet`.
3. Use latest available FAOSTAT year as crop-mix template (like pre-1961
   backfill but forward).
4. Add `scenario` parameter to `build_gridded_landuse()`.
5. Create comparison figures across scenarios.

**Effort:** ~3–4 days.

---

## Implementation roadmap

| Phase | Priorities | Target | Status |
|-------|-----------|--------|--------|
| **Phase 1** — Quick wins | P2 (yield index), P3 (GLW3), P6 (livestock figures) | Q1 2025 | ✅ P2+P6 done; P3 deferred (no GLW3 data) |
| **Phase 2** — Algorithm upgrades | P1 (multi-cropping), P4 (MIRCA irrigation) | Q2 2025 | ✅ P4 done; P1 deferred (no GAEZ data) |
| **Phase 3** — Performance & scale | P7 (optimization), P5 (temporal patterns) | Q3 2025 | Not started |
| **Phase 4** — Future scenarios | P8 (SSP projections) | Q4 2025 | Not started |

## Data download checklist

| Dataset | URL | Size | Needed for |
|---------|-----|------|------------|
| GAEZ v4 suitability | <https://gaez.fao.org/> | ~2 GB | P1 |
| EarthStat yields | <https://www.earthstat.org/> | ~1 GB | P2 |
| MapSPAM 2020v1 | <https://www.mapspam.info/> | ~3 GB | P2, P5 |
| GLW3 density grids | <https://www.fao.org/livestock-systems/> | ~500 MB | P3 |
| MIRCA2000 | <https://www.uni-frankfurt.de/45218023/> | ~200 MB | P4 |
| LUH2 v2f scenarios | <https://luh.umd.edu/> | ~4 GB | P8 |

## References

- Gilbert, M., et al. (2018). "Global distribution data for cattle,
  buffaloes, horses, sheep, goats, pigs, chickens and ducks in 2010."
  *Scientific Data*, 5, 180227.
- Portmann, F. T., Siebert, S., & Döll, P. (2010). "MIRCA2000 — Global
  monthly irrigated and rainfed crop areas around the year 2000."
  *Global Biogeochemical Cycles*, 24, GB1011.
- Ray, D. K. & Foley, J. A. (2013). "Increasing global crop harvest
  frequency." *Global Environmental Change*, 23, 261–271.
- Waha, K., et al. (2020). "Multiple cropping systems of the world and
  the potential for increasing cropping intensity." *Global Environmental
  Change*, 64, 102131.
- You, L., et al. (2020). "Spatial Production Allocation Model (SPAM)
  2017 v1.0." <https://doi.org/10.7910/DVN/FSSKBW>.
