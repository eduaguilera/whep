# Changelog

## whep (development version)

- **The polity a row belongs to is now carried from where it is resolved
  instead of re-derived at the end of every output.**
  `.aggregate_to_polities()` has always resolved the bucket’s polity in
  order to label the fold and then discarded the code, leaving the ~70
  call sites of `.add_reporting_polity_columns()` to resolve it a second
  time from the same crosswalk. The fold now emits `polity_area_code`,
  `reporting_polity_code`, `reporting_polity_name` and
  `reporting_polity_has_geometry` — the published names, so no new
  vocabulary and no schema change — and the tail helper keeps a carried
  identity rather than resolving it again. It keeps it only after
  checking it: the identity must still match the key it sits next to (a
  bucket code resolves to itself, so a re-keyed frame fails that test),
  and the distinct `(area_code, year)` pairs are re-resolved and
  compared, which costs a fraction of the full resolution it replaces.
  Two non-`NA` answers for one key now warn instead of one of them being
  published silently. **No published value changes**:
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  (6,310,390 rows) and
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  (2,098,818 rows) are identical before and after, column for column.

- **[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
  now says which direction a stand-in fell in, and the two directions
  are not the same defect.** The new `gap_kind` column takes
  `"polity_ended"` (the polity had ended by the row’s year, so the value
  covers a territory that entity no longer describes — whep#414’s case)
  or `"polity_not_started"` (the polity begins later, which is mostly
  WHEP’s documented pre-1961 back-cast onto the anchor-year territory).
  No published value changes; this is a diagnostic gaining a column.
  Measured on a real full-range
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  (6.3M rows), 7,247 rows — 0.115% — are attributed to a polity that was
  not live in the row’s year, and they split **3,285 rows across 3
  areas** `"polity_ended"` against **3,962 rows across 16 areas**
  `"polity_not_started"`. Bucket 206 is 2,938 of the ended ones, 89%;
  the other two, FAOSTAT area 178 Eritrea (123 rows, `ERI-1889-1952`)
  and area 273 Montenegro (224 rows, `MNE-1913-1918`), were not
  previously named anywhere. The classification is read at the year the
  resolver actually matched on, not the row’s year, because
  `backcast_anchor` floors the lookup year: a pre-anchor row is matched
  as 1961 and can land on a polity that had already ended by then. That
  is exactly 165 rows of areas 178 and 273, which the raw-year
  comparison a caller could write for itself would label
  `"polity_not_started"` instead.

- **The `area` label a country carries through the commodity-balance
  build no longer depends on row order.** `.select_best_source()`
  reduced the long CBS input to one human-readable `area` per numeric
  code by keeping whichever row came first. `area` is the *periodized*
  polity name and a code legitimately changes it at a period boundary,
  so one code offers several labels over a multi-year build: on a real
  1850-2023 run, 75 of the 216 codes carry more than one (up to four),
  and shuffling the input rows flipped the label for 13 of them. The
  label is also a join key — a second `area` vocabulary for one bucket
  once dropped 702,166 rows
  ([\#382](https://github.com/eduaguilera/whep/issues/382)) — so nothing
  pinned a key the build depends on. The pick is now a stated total
  order: the source that reports the code earliest in the order
  `.assemble_cbs_sources()` binds them in, that source’s earliest year,
  then the label alphabetically. **No published value changes**: the
  rule reproduces all 216 of today’s labels exactly, which is
  deliberate, because that same label is what the pre-1962 proxy fill
  reads a polity out of
  ([\#698](https://github.com/eduaguilera/whep/issues/698)) and changing
  it would silently redistribute which countries find a population and
  land proxy. Fixes
  [\#580](https://github.com/eduaguilera/whep/issues/580).

- **Every join that keys on a territory but not on a year is now
  classified, and the list can only shrink.** A key of `area_code` with
  no `year` spans every period of a territory’s history, so it asserts
  that the area means one thing for all time. Usually that is right – 57
  of the package’s 163 territorial joins carry no year, and nearly all
  are a single-year scope, a table with no time dimension (a
  coefficient, a single-vintage map, a grid mask), an identity lookup or
  a diagnostic – but nothing said which, so a decision and an oversight
  looked alike. `.territorial_join_baseline()` now records the verdict
  and the reason for each, and `test_join_audit.R` fails when a new
  year-free territorial join appears unclassified, when a classified one
  disappears without its entry, or when a further join starts keying on
  the `area` label. Classifying them turned up one real defect, filed as
  [\#698](https://github.com/eduaguilera/whep/issues/698) with its
  measurement rather than fixed here, because removing it needs
  [\#493](https://github.com/eduaguilera/whep/issues/493)’s decision
  first. No published value changes
  ([\#669](https://github.com/eduaguilera/whep/issues/669)).

- **Four documented examples could not say which territory their rows
  belong to.** `build_supply_use(example = TRUE)` shipped a row with no
  `area_code` at all (an epsilon `3.33e-14` husbandry use) and
  `get_feed_intake(example = TRUE)` two more, so their polity columns
  came out `NA`; `build_feed_intake_local(example = TRUE)` and
  `build_grass_natural_carbon_inputs(example = TRUE)` keyed cells by an
  ISO-3166 numeric code (724 Spain, 300 Greece) where the FAOSTAT area
  code belongs, which resolves to nothing – and one sibling row’s ISO
  code for Argentina, 32, is FAOSTAT’s Cameroon, so a cell in the pampas
  was labelled Cameroon. The two feed fixtures also predated the
  redistribute-feed migration and showed a 10% feed loss the current
  allocator cannot produce. All four fixtures are now sampled from real
  builds (the gridded ones keyed by the code the cell grid actually
  assigns: 203, 84, 9), and `build_supply_use(example = TRUE)` now
  covers all five documented process groups instead of three. **No
  published value changes** – these are documentation fixtures, not
  pipeline outputs
  ([\#417](https://github.com/eduaguilera/whep/issues/417)).

- **[`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md)
  can now use the region-specific MMS shares, via the new
  `mms_source = "region_specific"`. The default is unchanged, so no
  published value moves.** `regional_mms_distribution` ships 33 rows: 18
  for `region == "Global"` and 15 for North America (cattle, swine),
  Western Europe (cattle) and Latin America (cattle). The function
  filtered the table to `"Global"` unconditionally and never read the
  excretion’s `territory`, so those 15 rows were unreachable and every
  territory got the global split — a global default, not a drop and not
  a silent zero. The territory is now resolved to its IPCC region
  through the same GLEAM lookup the emission-factor tables use
  (`.gleam_region_of()`, whep#465), with the Global rows as the fallback
  for every region and species the table does not cover.

  **What flipping the default would move**, measured on the real 2020
  national chain (90.06 Mt excreted N, 195 territories): 66 territories
  and 5.40 Mt N (6.0% of the excreted nitrogen) change management
  system. The in-situ grazing stream falls from 41.36 to 40.71 Mt N
  (−1.6%) and the collected stream rises correspondingly; applied N
  moves −0.24%, volatilized N +0.94%, leached N −4.67%, direct N2O-N
  +0.30% and indirect N2O-N +0.84%. Rows, keys and territories are
  identical between the two sources, and mass is conserved per input row
  under both. The default stays `"regional_default"` because the
  region-specific rows are a coarse four-pair table (`data-raw`
  documents them as “GLEAM 3.0 / FAO statistics (simplified)”) whose
  provenance has not been verified against a published GLEAM table;
  whether they are better than the global average is the maintainer’s
  call ([\#466](https://github.com/eduaguilera/whep/issues/466)).

- **[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  and
  [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
  now name the reporting areas their `country_grid` cannot represent at
  all**, once per call, with the national total at stake. The existing
  diagnostics fire per (country, crop) per year and per (species, year),
  so a country the grid has no cell for anywhere was reported as one
  more line in a list that already names 178 codes. On today’s pinned
  centroid grid at 2015 the new warning reads 18 reporting areas
  carrying 0.109 Mha — all island or city states. Substitute the
  fractional crosswalk and it reads 20 areas carrying 28.90 Mha, because
  that parquet still keys Ethiopia `62` and Sudan `206` where the
  centroid grid and today’s `regions.csv` use `238` and `276`. No
  published value changes: this is a diagnostic only
  ([\#461](https://github.com/eduaguilera/whep/issues/461)).

- **[`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
  gains the `country_grid` override**, `"centroid"` (default, today’s
  `spatialize-country-grid` pin) or `"fraction"`
  (`cell_polity_fraction.parquet`, which splits each border cell by
  fractional coverage instead of giving it whole to one polity). The
  engines already read `polity_frac` as `cell_area_frac`, so this is
  data wiring, not an engine change, and the resolved choice is recorded
  in `run_metadata.yaml`. The default is unchanged, so no published
  value moves. Measured at 2015, the alternative moves 6,828 of 7,557
  (country, crop) cell-share vectors, by a median L1 of 0.060 and a
  harvested-area-weighted mean of 0.040, and raises the compartments
  receiving an allocation from 33,614 to 36,226
  ([\#461](https://github.com/eduaguilera/whep/issues/461)).

- **[`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
  reported bucket 206 as a three-way fold in all 65 years and called its
  label an extent mismatch; both were wrong
  ([\#414](https://github.com/eduaguilera/whep/issues/414)).** No
  published value changes — this is a diagnostic and the warning it
  drives.

  The fold runs **2012-2025, not 1961-2025**. Measured on the FAOSTAT
  production pin, area 206 “Sudan (former)” carries 13,759 rows over
  1961-2011 and areas 276 Sudan / 277 South Sudan carry 3,467 and 2,170
  rows over 2012-2024: the three never report in the same year. The
  year-aware resolver answers for every `(area_code, year)` pair
  regardless, standing in with the nearest period, and counting those
  stand-ins invented two members in every pre-secession year. A member
  now counts only when its polity is in span **and** the upstream map
  reports the area that year, which takes the report from 65 rows to 14.

  The label is **not** an extent mismatch. Bucket 206 resolves to
  `SUD-1956-2011`, whose published `successor` set is exactly
  `SDN-2011-2025; SSD-2011-2025` — the two polities the bucket folds —
  so that polity’s territory *is* the sum. What is wrong is the period:
  it had ended. That is now its own class, `"predecessor"`, and
  `"partial"` is reserved for a label covering less than the value does.
  No bucket is `"partial"` today. The build-time warning says which of
  the two a bucket has instead of asserting the wrong one.

  The open decision in
  [\#414](https://github.com/eduaguilera/whep/issues/414) is unchanged
  and unmade: no **live** polity means “Sudan and South Sudan”. Minting
  one upstream is proposed in lbm364dl/whep-polities#139; un-folding the
  two areas instead is costed in
  [\#680](https://github.com/eduaguilera/whep/issues/680).

- **[`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
  can now charge a single crop’s water, and the per-CFT
  consumptive-water cubes are readable at all.**
  [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  gains `"cft_consump_water_b"` / `"cft_consump_water_g"`, and
  `build_water_balance(bands = )` restricts the consumptive-water and
  `cft_nir` terms to named crop-functional-type bands, e.g.
  `bands = "rainfed grassland"` to charge a grazing footprint the
  grassland water alone rather than every crop in the cell. Bands are
  selected by the `band_name` the file itself carries, never by index,
  so a run configured with a different band set aborts instead of
  silently charging the wrong crop. `bands = NULL` (the default) totals
  every band, so existing callers are unaffected. Three fixes were
  needed to get there, each of which would have produced wrong numbers
  rather than an error:

  - The `cft_nir` map entry named `mcft_nir.nc` holding a monthly
    `cft_nir` variable. **No WHEP run has ever written that file**: all
    nine runs, 5.9.7 and 6.1.1 alike, write `cft_nir.nc` holding annual
    `nir`. Reading it would simply have failed; nothing called it yet.
  - The reader assumed twelve time steps per year for every variable.
    The per-CFT consumptive-water cubes are annual (`nstep` 1, mm/yr),
    so their time axis was decoded as months, mapping year *y* to year
    1901 + (y-1901)/12 and slicing the wrong years out of the file
    entirely.
  - `ncvar_get()` drops length-1 dimensions, so slicing one year out of
    an annual per-CFT cube returned a 3-D slab whose *band* axis was
    then decoded as *time* — scrambling crops into years. Now read with
    `collapse_degen = FALSE`. Monthly cubes never hit this, because a
    one-year slice is still twelve steps.

- **New
  [`polity_identity_conventions()`](https://eduaguilera.github.io/whep/reference/polity_identity_conventions.md)
  states, per object, what territorial identity a WHEP table with no
  year dimension carries
  ([\#671](https://github.com/eduaguilera/whep/issues/671)).** A polity
  code is year-scoped, so for a year-less object “attach the polity
  code” has no single answer: measured on the deployed
  `spatialize-country-grid` pin, 52,420 of its 58,795 cells (89.2%) sit
  under an `area_code` that `polity_area_crosswalk` maps to more than
  one polity over time, and 33 of `mueller_synthetic_n`’s 156 `iso3c`
  labels, 37 of `crops_manure_n`’s 184 `ISO` labels and 38 of
  `gleam_geographic_hierarchy`’s 204 `iso3` labels name a *different*
  polity at 1961 than at 2020. The register records which of
  [\#458](https://github.com/eduaguilera/whep/issues/458)’s three
  answers each object takes — present-day polity, polity-period rows, or
  deliberately identity-free — and the new
  `tests/testthat/test_territorial_identity.R` checks each claim against
  the object it is made about, so a year-less territory-keyed dataset
  can no longer arrive without one. No published value moves; nothing
  but the register and its guards is added.

  Two things it makes visible. `regions_full` and `polities_cats` really
  do carry the present-day polity, in `reporting_polity_code`, and it is
  exactly what `add_polity_code(year_column = NULL)` resolves for all
  272 and 198 rows respectively — now asserted rather than assumed. And
  their column literally named `polity_code` is **not** a polity code:
  none of its 271 values appears in `polities`, because it is a legacy
  ISO3-like prefix, which is pinned so the two vocabularies cannot be
  quietly conflated.

- **[`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
  and
  [`get_soc_climate_drivers()`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md)
  now say when a cell-year is attributed to a polity that did not yet
  exist, and can refuse to do it
  ([\#462](https://github.com/eduaguilera/whep/issues/462)).** The
  cell-polity crosswalk is a present-day rasterization with no year
  dimension, while polity validity is year-scoped, so a cell labelled
  `area_code` 52 carried that label in 1901 as readily as in 2009 and
  the polity resolution silently substituted the nearest period,
  `AZE-1991-2025`. Measured on the deployed
  `cell_polity_fraction.parquet` over the 1901-2009 LPJmL run: **1,948
  of 19,838 `(area_code, year)` keys, 21 of 182 area codes, 14,761 of
  58,791 cells** — the post-Soviet and post-Yugoslav successors plus
  South Sudan. Both functions gain `polity_validity`: `"keep"` (default)
  is the previous behaviour plus a warning naming the rows, years and
  area codes; `"flag"` adds the per-row logical
  `reporting_polity_out_of_span`; `"drop"` removes those rows. **No
  published value changes on the default**, and `"flag"` is numerically
  identical to it. `"drop"` removes 20.4% of the run’s cell-years and
  makes South Sudan disappear from it entirely, which is why it is
  opt-in.

- **Year-scoped builds no longer drop split-species slaughter counts.**
  `.compute_stock_shares()` read the livestock stock series scoped to
  the caller’s window, but those shares are carried along the year axis
  precisely because the `faostat-emissions-livestock` pin lags QCL
  slaughter by 1-2 years. A narrow window left the carry-forward nothing
  to fill from, and the join in `.split_slaughter_by_shares()` then
  dropped the slaughter row entirely. At 2010 that was 2 Singapore rows
  (Pigs, Hogs); `slaughtered_heads` now agrees exactly with the
  full-range build instead of by 4.7e-06. The stock series is read over
  its full span; full-range output is unchanged
  ([\#665](https://github.com/eduaguilera/whep/issues/665)).

- **[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  is about a quarter faster, with output unchanged to the last bit.**
  The RothC/HSOC climate modifier is now computed for every cell-year at
  once instead of once per (cell, year, land use) – roughly 1.2e6
  separate calls over five years, each of which allocated a list and
  accumulated over twelve months. The deficit recurrence is sequential
  over months but independent across cells, so the loop inverts.
  Measured on `years = 1901:1905`: 820.4 s to 612.1 s. Peak memory is
  unaffected.

  The per-group path stays in place as the reference and still runs for
  models that do not use this modifier. The two agree exactly, not
  approximately: [`identical()`](https://rdrr.io/r/base/identical.html)
  holds across all 1,166,220 rows and 17 columns of the five-year build,
  so no result changes
  ([\#630](https://github.com/eduaguilera/whep/issues/630)).

- **`build_energy_co2_extension(unclassified = "historical_region")`
  prices the dissolved federations instead of losing them
  ([\#553](https://github.com/eduaguilera/whep/issues/553)).** Measured
  on the real
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  output (6,305,656 rows, 1850-2023), 569.4 Mt of meat carcass
  production — 3.33% of all of it, and 15.2% of the world’s 1961 tonnage
  — gets no energy intensity and leaves the extension, because
  `gleam_geographic_hierarchy` is a present-day country table with no
  row for the USSR, Belgium-Luxembourg, Czechoslovakia, the Yugoslav SFR
  or Serbia and Montenegro. Those five are now 99.998% of the loss:
  since the Rest-of-World fold was lifted
  ([\#628](https://github.com/eduaguilera/whep/issues/628)) bucket 999
  no longer contributes to it at all. The new treatment groups them by
  running GLEAM’s own scheme rules on the OECD and EU membership they
  themselves held while they existed — Belgium and Luxembourg were OECD
  founding members and EEC founders, so Belgium-Luxembourg is OECD/EU
  27; no successor of the other four was in either body before the
  entity dissolved, so they are non-OECD, non-EU. Rows carry
  `method_energy = "GLEAM_3.0_energy_meat_historical_region"`, and the
  option is a superset of `"polity_region"`.

  **No published value changes**: the default `unclassified = "drop"` is
  bit-identical on the full real input (181,831 rows,
  `sum(impact_u) = 6.530863856531e12` before and after,
  [`identical()`](https://rdrr.io/r/base/identical.html) TRUE). Opting
  in adds 1,190 rows and 7 areas, moves no shared row by any amount, and
  raises total energy CO2e by **+2.40%** over 1850-2023 — **+12.0% in
  1961**, +11.3% in 1990, +0.26% in 2000 and 0% from 2010 on.

- **`polity_area_crosswalk$mapping_status` now uses the value it
  documented but never shipped, and the confidence of a mapping is
  documented as the pair `mapping_status` x `mapping_source`.**
  `not_a_reporting_area` sat below `matched` in the build’s `case_when`,
  so it could only fire for a row with neither an `area_code` nor a
  `polity_code` — no such row exists, and it shipped on 0 of 596 rows.
  The 20 rows it was written for (Aland, Saint Barthelemy, Guernsey,
  Jersey, the Isle of Man and Sint Maarten, which `regions_full` carries
  without a FAOSTAT code, plus the six regional aggregate polities)
  match a polity and so read `matched`, indistinguishable from a real
  area mapping even though they carry `NA` in both `area_code` and
  `polity_area_code` and no consumer can join to them. Status counts
  move from manual 27 / matched 568 / unmapped 1 to manual 27 / matched
  548 / not_a_reporting_area 20 / unmapped 1. No `polity_code`,
  `polity_area_code` or any other column moves, and no code in the
  package filters the crosswalk on `mapping_status == "matched"`, so no
  published number changes. A consumer that does filter that way loses
  20 unjoinable rows.

  `mapping_status` says whether a polity was found, not how far to trust
  it: `matched` covers a curated hit in upstream’s FAOSTAT map (233
  rows), a prefix-inferred historical period (247), a prefix guess for
  an area the map never mentions (6) and the FABIO Rest-of-World fold
  (62). `mapping_source` already separates those and is non-`NA` on
  every row, so the fix for
  [\#544](https://github.com/eduaguilera/whep/issues/544) is to document
  the pair rather than add a third vocabulary that would duplicate it
  ([\#544](https://github.com/eduaguilera/whep/issues/544)).

- **`get_polity_geometries(polity_codes = )` now returns a usable `sf`
  object in a session that has not loaded `sf`.** The row subset ran
  through `[.data.frame` whenever the suggested `sf` namespace was not
  loaded, which keeps class `sf` and `attr(, "sf_column")` but strips
  `sfc` off the column they point at; the result passed every cheap
  structural check and then aborted inside the first `sf` call,
  complaining about a column nobody had renamed. The function now loads
  `sf` before subsetting, and aborts with class `whep_sf_required` if
  `sf` is not installed instead of returning the broken object. No
  published values change: the argument-less call is untouched, and both
  in-package callers use it.

- **[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  and
  [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
  take an `area_key`, and say when their output cannot join a national
  table.** The spatialize chain allocates on the raw reporting codes its
  `country_areas` and `country_grid` are keyed on, while whep’s
  polity-keyed national tables are aggregated on `polity_area_code`. A
  reporting code that is not itself a bucket therefore left every output
  row carrying two territorial keys that disagree — `area_code = 276`
  beside `polity_area_code = 206` — so whether a consumer joined on one
  or the other decided whether Sudan existed in its result
  ([\#582](https://github.com/eduaguilera/whep/issues/582)). Measured
  against the deployed pins, `country_grid` holds 831 such cells under 2
  codes (276 Sudan, 277 South Sudan) and `country_areas` 0.64% of its
  harvested area; the other six codes the issue listed are no longer
  off-bucket, because
  [\#628](https://github.com/eduaguilera/whep/issues/628) gave Syria,
  North Macedonia, Eswatini, Equatorial Guinea, New Caledonia and
  Palestine their own published codes. The default `area_key = "grid"`
  is unchanged bit-for-bit and now warns naming the codes that cannot
  join; `"polity_area"` re-keys the output on the bucket before the
  polity columns are attached, so the two keys agree in every row. **No
  published value changes** unless `"polity_area"` is asked for: on a
  2020 Sudan/South Sudan run it conserved 21,894,526 ha and 230.7 M head
  exactly, kept the row count, and moved 13,447 crop rows and 3,671
  livestock rows from a key no national table carries onto `206`. Under
  `"polity_area"` the raw code is carried, not replaced, as
  `grid_area_code`, the shape
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  adopted in [\#579](https://github.com/eduaguilera/whep/issues/579).
  [`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
  accepts `area_key` in `overrides`.

- **[`estimate_energy_demand()`](https://eduaguilera.github.io/whep/reference/estimate_energy_demand.md)
  now warns when `work_hours_day` is supplied without a work
  coefficient.** `whep` ships `cw = 0` for every species, so draft work
  is opt-in per call via `work_coef` — passing only the hours produced
  `ne_work = 0` with no indication that the input had been ignored
  ([\#210](https://github.com/eduaguilera/whep/issues/210)). The numbers
  are unchanged; only the silence is. Hours filled in from
  `livestock_production_defaults` never warn, since several species
  carry a non-zero default and warning about those would fire on
  ordinary runs.

- **The FABIO comparison’s EU aggregate is derived, and now covers the
  dissolved predecessors.** `inst/scripts/compare_fabio_footprints.R`
  carried a 28-element ISO3 literal for EU28. It is now built by
  `.eu_aggregate_iso3()` from the published `regions_full$EU27` flag
  plus `GBR`, the one membership fact no table in the package states,
  selected through the new `WHEP_EU_AGGREGATE` environment variable
  (`"eu28_territory"`, the default, `"eu27_territory"`, `"eu28_states"`,
  `"eu27_states"`). The literal omitted `BLX` (Belgium-Luxembourg) and
  `CSK` (Czechoslovakia), under which FABIO *and* WHEP’s own CBS both
  book Belgium, Luxembourg, Czechia and Slovakia before those
  successions, so all four read as exactly zero in the 1986 benchmark
  year on both sides of the comparison and normally in 2000 and 2013.
  **This moves a published number:** the FABIO EU land footprint for
  1986 goes from 210.4 Mha to 222.7 Mha (+12.3 Mha, +5.9%); 2000 and
  2013 are bit-identical, because the predecessors carry no demand
  there. `WHEP_EU_AGGREGATE=eu28_states` reproduces the old list, and
  the old numbers, exactly. Whether the comparison should report EU28 or
  EU27 at all is left open
  ([\#421](https://github.com/eduaguilera/whep/issues/421)).

- **[`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  and
  [`build_n_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_percapita.md)
  now name the areas they drop for having no population denominator.**
  Both inner-join the
  [`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
  table, so an area the `gdp-population` pin does not cover was absent
  from their per-capita output rather than wrong in it, and nothing said
  so. Measured on a real `get_wide_cbs(years = c(2010, 2015, 2021))`
  plus the real pin,
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  silently lost **16 areas over 43 area-years** — Bhutan, Comoros, New
  Caledonia, Tonga, Micronesia, Seychelles, the Faroe Islands, bucket
  999 and others — carrying 0.0304% of the food protein in range. They
  are still dropped (no denominator is invented) but each is now named
  in a warning, with the share of the quantity that leaves with it.
  `options(whep.warn_missing_population = FALSE)` silences it. **No
  published value changes**: with the warning suppressed a real
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  run is [`identical()`](https://rdrr.io/r/base/identical.html) to the
  one before this change.

  This closes [\#543](https://github.com/eduaguilera/whep/issues/543),
  whose measurement it also corrects. That issue reported the area-999
  denominator as covering 6 of its 62 territories and so overstating
  every per-capita quantity keyed on 999 by 15-43%. Since the
  Rest-of-World un-fold
  ([\#628](https://github.com/eduaguilera/whep/issues/628)) that is no
  longer the shape of the defect: the 6 covered members have their own
  area codes,
  [`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
  emits **no 999 row at all**, and CBS 999 carries **zero food** in
  2010, 2015 and 2021 — so nothing per-capita is keyed on 999 and
  nothing is overstated. Routing the pin’s five continental “Other”
  residuals into 999, the fix the issue proposed first, would now be
  wrong: it would give a bucket with no food a denominator of 5.9 M
  people (2010) and attribute Reunion’s, Greenland’s and New Caledonia’s
  population to a code that no longer carries their food.

- **LUH2 land is no longer discarded when one aggregation bucket holds
  two territories, and the pre-1962 yield back-cast no longer mixes
  countries that share a polity label.** Two sites keyed on the `area`
  *label* where an `area_code` was available, which is whep#632’s defect
  at two further sites.

  1.  The LUH2 area bridge paired each bucket’s code with its *member’s*
      name, so bucket 206 reached grassland construction as two rows —
      “Sudan (former)” and “South Sudan” — under one `area_code`;
      `.dedup_production()` reads that as competing sources and kept
      one, dropping the other’s pasture. The bucket now carries one
      label derived from its own code, so the two are summed.
  2.  The `t_ha` proxy-growth fill grouped its series on the label,
      which is wrong both ways: “Rest of World” covers 62 reporting
      `area_code`s, so growth rates were taken between different
      countries, and the label is year-aware, so one country’s own
      series was cut in two at every periodization boundary (`area_code`
      79 is “Germany (divided, 1949-1990)” through 1989 and “Germany”
      from 1990). **Published values move**: `ha` +1.04% (+5.56e9
      ha-years, all of it bucket 206’s recovered pasture over 1850-2022)
      and `t_ha` -0.036% (464 cross-country fills removed, 5 real ones
      gained); `tonnes`, `heads`, `LU`, `t_head`, `t_LU` and
      `slaughtered_heads` are bit-identical. Comparing the Rest-of-World
      fold against the default un-folded build over its 62 areas, `ha`
      goes from 2.38x to 0.99x and `tonnes` from 0.59x to 0.82x, with
      the whole remainder in the pre-1962 back-cast and the observed
      1962-2023 era conserved to 0.04%
      ([\#633](https://github.com/eduaguilera/whep/issues/633)).

- **EU AgriDB fodder now reaches Austria and the United Kingdom.**
  `.read_fodder_euadb()` resolves the source’s `Region` through
  `regions_full$ADB_Region`, and that column had a key for 26 of the
  pin’s 28 regions: `AT` and `GB` were missing, so 2030 rows (8.8% of
  the input, 1961-2019) resolved to no area and were discarded without a
  message. Those two countries had their fodder estimated from
  dry-matter yields (`source = "DM_yield_estimate"`) while their 26 EU
  peers used the source. Adding the two keys moves published values for
  area codes 11 and 229 only, and for no other area: harvested-area
  totals over 1850-2023 rise 6.4% (Austria) and 14.0% (United Kingdom),
  fodder tonnage 33.4% and 65.7%; the global harvested-area total moves
  +0.07% and global tonnage +1.2%. Fodder production is copied
  one-for-one into `feed` by `.primary_to_cbs()`, so those areas’ `feed`
  moves by the same tonnage. A region the source adds in future that
  `regions_full` does not key now raises a warning naming it, instead of
  vanishing ([\#585](https://github.com/eduaguilera/whep/issues/585)).

- **The last two ad-hoc country-label joins in the spatialization script
  are gone ([\#576](https://github.com/eduaguilera/whep/issues/576)).**
  `inst/scripts/prepare_spatialize_all.R` matched
  [`whep::crops_manure_n`](https://eduaguilera.github.io/whep/reference/crops_manure_n.md)
  on a raw `iso3c` join and
  [`whep::lassaletta_grassland_share`](https://eduaguilera.github.io/whep/reference/lassaletta_grassland_share.md)
  on a country *name*. The manure reader now goes through
  [`whep::polity_label_aliases`](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
  like the Mueller reader does, read at the vintage of its own labels
  rather than at Mueller’s circa-2000 base year: `crops_manure_n` names
  Serbia, Montenegro and South Sudan separately and names no Serbia and
  Montenegro, Sudan (former), Czechoslovakia or Zaire, so its vocabulary
  is post-2011, and every year from 2011 on maps all 183 of its country
  labels exactly as the retired join did. **No published value
  changes**: same 31,476 rows, same 183 area codes, maximum difference 0
  Mg. West et al.’s `RoW` aggregate is still dropped rather than equated
  with WHEP’s residual bucket 999, which since
  [\#628](https://github.com/eduaguilera/whep/issues/628) means
  something else.

  The grassland-share reader gains a `grass_share_route` argument on
  `prepare_nitrogen_inputs()` and `prepare_spatialize_all()`, recorded
  in `nitrogen_inputs.parquet` as `method_grass_share`. The default,
  `"area_name"`, is the existing name join and is byte-identical to it.
  `"alias_map"` resolves each label at its own row’s year instead: 6,633
  rows against 6,370 and 137 area codes against 130, gaining China, Cote
  d’Ivoire, DPRepublic of Korea, Cape Verde, Swaziland, Ethiopia PDR,
  Belgium-Luxemburg and Occupied Palestinian Territory, and losing South
  Sudan and the years in which Yugoslav SFR, Czechoslovakia, Viet Nam
  and Botswana had no polity. Which route is right is an open question
  ([\#576](https://github.com/eduaguilera/whep/issues/576)); nothing
  switches by itself.

- **[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  and
  [`get_processing_coefs()`](https://eduaguilera.github.io/whep/reference/get_processing_coefs.md)
  take a `years` argument.** A scoped request now builds only that
  window instead of building 1850-2023 and discarding the rest. Measured
  for 2010: wide CBS 256 s / 23.3 GB peak to 12.6 s / 6.8 GB, primary
  production 168 s / 14.7 GB to 29.5 s / 2.9 GB. The full wide-CBS build
  peaking above 16 GB is what had been failing the r-universe check on
  macOS and Linux. `years = NULL` is unchanged in every respect,
  including its cache slot, so existing callers keep today’s behaviour
  and today’s numbers. The primary-production to CBS to
  processing-coefficient chain and its cache keys are now shared with
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md),
  which previously carried a private copy
  ([\#367](https://github.com/eduaguilera/whep/issues/367)).

  A scoped window is close to, but not identical with, building the full
  range and filtering. Against the full range at 2010, wide-CBS quantity
  totals agree to 3.8e-04 and primary-production totals to 3.0e-04, with
  `ha`, `t_ha`, `LU` and `heads` exact. The residual sits in `import`
  and in the livestock ratios
  ([\#625](https://github.com/eduaguilera/whep/issues/625)). Use
  `years = NULL` when exact agreement with the published series matters.

- **Year-scoped production builds no longer drop every forage crop.**
  Fodder rows are synthesised across the whole year axis —
  `.fill_fodder_gaps()` takes the union of (area, item) groups over all
  years and interpolates between them — so a narrow window silently lost
  all six forage items (`Forage and silage, *`, `Cabbage for fodder`,
  `Forage products`). At 2010 that was 137 rows, **1.16% of production
  tonnes, 1.85% of `t_ha` and 1.36% of wide-CBS `feed`**, and it
  affected `build_io_model(years = )` on every release that had it. The
  fodder chain now runs over the full span and trims afterwards.
  Full-range output is unchanged
  ([\#623](https://github.com/eduaguilera/whep/issues/623)).

- **Livestock stocks are split on the area CODE, not the area label
  ([\#589](https://github.com/eduaguilera/whep/issues/589)).**
  `.split_stock_share()` divides a parent item’s production across its
  sub-items in proportion to their stocks, grouped by
  `(year, area, item_prod_code)`. When several reporting areas share one
  label the group spans all of them, the share denominator sums across
  areas, and each area keeps only its own fraction. That became live
  when the Rest-of-World fold was lifted: `.unfold_rest_of_world()`
  promotes `polity_area_code` but leaves `polity_code`/`polity_name`
  alone, so all 13 reporting members came out with their own `area_code`
  and the shared label `"Rest of World"`. Measured: Syria’s 2000
  livestock read **3,408,857** head against **38,048,415** after the
  fix, with fractional animals (`1227745.45`) as the visible symptom of
  a share that should have been 1. `slaughtered_heads` was never
  affected, because it does not pass through this splitter — which is
  what made the defect look like a unit-conversion bug.

  The stock join, the carry-forward and the row-count grouping are
  re-keyed the same way. Globally this moves `heads` **+0.22%** and `LU`
  +0.13%; `ha`, `tonnes` and `slaughtered_heads` are bit-identical,
  because only areas sharing a label were ever affected.

- **[`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
  no longer depends on the order its rows arrive in.** Without `.by`, it
  never sorted: carrying a value forward or backward and the
  `value_smooth_window` moving average are all positional, so an
  unsorted input filled the wrong way round. On a 2015-2020 series
  anchored at 2016 and 2019, reversing the rows swapped the two carry
  labels, and interleaving them left both outer gaps unfilled and moved
  two interpolated values. Both paths now sort by `.by` and then
  `time_col` first, and **rows come back in that order** — the grouped
  path already did through `setkeyv()`, the ungrouped one now matches
  it. Grouped output is unchanged for already-sorted input, which is
  every caller inside the package. Three further gaps in the same file
  are closed. A `value_smooth_window` that leaves a group with no valid
  anchor (gaps one year apart, or a window wider than the group) aborted
  with `missing value where TRUE/FALSE needed`; both paths now share one
  filling core, leave those gaps as `"Gap not filled"`, and cannot
  diverge again.
  [`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
  used to trust a `.whep_sorted_by` attribute it had stamped on a
  previous call, which a `setorderv()` in between does not clear, so a
  reordered data.table was filled in the wrong direction and came back
  carrying a `sorted` key its rows did not obey; the sort is now
  verified against the rows. And in
  [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md),
  the documented weighted proxy syntax (`"gdp:region[population]"`)
  aborted in `setnames()` on every call, so it had never run; with that
  fixed, its weights are lagged before the rows without a growth rate
  are dropped, which is what makes them the previous period’s weights
  rather than the previous surviving row’s.

- **One unvaluable 1:n split no longer erases observed data.**
  [`harmonize_interpolate()`](https://eduaguilera.github.io/whep/reference/harmonize_interpolate.md)
  summed the split 1:n contributions together with the already-correct
  `"simple"` component using [`sum()`](https://rdrr.io/r/base/sum.html)
  without `na.rm`, so a single contribution with a missing `value`, or
  with a share that could neither be computed nor interpolated (every
  year of the group totalling zero makes the shares `NaN`), turned the
  whole harmonized `(item_code, year)` cell into `NA`/`NaN` — including
  the observed values summed into it. Unvaluable contributions are now
  dropped with a warning naming the affected cells, and the observed
  values survive. Published values change only where the old output was
  `NA`/`NaN`: such a cell now holds its observed `"simple"` sum, or
  disappears if it had none. No cell that was a number before changes.

- **[`build_cbs_prices()`](https://eduaguilera.github.io/whep/reference/build_cbs_prices.md)
  no longer drops crop residues into an NA bucket.** The residue routing
  in `.add_residue_prices()` read `Herb_Woody == "Woody"` inside a
  nested `fifelse()`, so every item whose herbaceous/woody habit is
  missing got `NA` as its residue item. Those rows were pooled into one
  `NA`-keyed group and then dropped, and the pool mixed the mass and
  value of unrelated items on the way. On the real
  `faostat-trade-bilateral` pin (1986-2021) that silently discarded **72
  rows** (36 years x 2 elements) of residue value. Residues are now
  generated only for primary crops and grassland — processed and animal
  products never had a crop residue — and a crop with no recorded habit
  takes the herbaceous default, reported in a warning naming the items
  (currently Cottonseed, Palm kernels and Palm Oil, whose `Name` is
  unset in `items_prod_full_raw.csv`). **Published values move for one
  item, `Other crop residues` (2106)**: its tonnage basis grows by 15.0%
  on average (2020 exports 345.6 Mt to 401.9 Mt) and its price shifts by
  -2.6% on average (2020 exports -1.2%, largest single move +6.8%).
  `Straw` (2105), `Firewood` (2107) and every non-residue item are
  unchanged to the digit.

- **[`calculate_soc_dynamics()`](https://eduaguilera.github.io/whep/reference/calculate_soc_dynamics.md)
  returns one schema for all five SOC models.** It used to hand back
  whichever shape the selected model happened to produce: `hsoc` (the
  default) came back long as `pool` / `year` / `stock_mgc_ha` /
  `rate_mgc_ha` with **no** `soc_total`, while `rothc`, `icbm`, `amg`
  and `century` came back wide with `soc_total` and their own mutually
  exclusive pool columns (`dpm`/`rpm`/`bio`/`hum`/`iom`, `y`/`o`,
  `ca`/`cs`, `str`/`met`/`act`/`slw`/`pas`) — no two of the five agreed,
  so a caller had to branch on `model`. The selector now reshapes
  whichever model ran to the long schema `year`, `pool`, `stock_mgc_ha`,
  `soc_total`, `method_soc`: pool detail is kept, the model-specific
  part sits in the values of `pool` instead of in column names, and the
  five runs of a sensitivity analysis stack with a plain
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
  Total-only callers read `dplyr::distinct(out, year, soc_total)`.
  [`calculate_soc_hsoc()`](https://eduaguilera.github.io/whep/reference/calculate_soc_hsoc.md)
  itself is now wide like its four siblings (`year`, `fresh`, `humus`,
  `iom`, `soc_total`) and no longer returns the per-pool `rate_mgc_ha`,
  which was exactly the forward annual difference of `stock_mgc_ha` and
  is recoverable from it. **No published value changes**: every pool
  stock and every
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  equilibrium is bit-identical before and after (checked across nine
  HSOC parameterisations and the spin-up of all five models).

- **WHEP now models the Rest-of-World reporting members in their own
  right.** FABIO folds 61 FAOSTAT reporting areas into its single
  `Rest of World` column, and `polity_area_code` inherited that fold, so
  any territory outside FABIO’s 192-country layout was published as
  `ROW`. FABIO’s layout is a methodology this package compares against,
  not a constraint on which territories it represents, and the country
  set is WHEP’s own decision
  ([\#459](https://github.com/eduaguilera/whep/issues/459)).

  The fold was also not doing what its name suggests. Of the 61 members
  only about a third report anything; the rest contribute no rows, so
  folding them is arithmetically a no-op. Everything the bucket carried
  came from the members that DO file returns, and the fold discarded
  whose data it was — Syria’s production was published as “Rest of
  World” despite Syria filing its own FAOSTAT returns. Promotion is
  therefore self-limiting: an area with no rows is unaffected either
  way, so no hand-maintained list of “which ones to promote” is needed.

  Measured on two full-range
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  builds (1850-2023): the published area count goes **195 → 216**, and
  21 territories become standalone — Bermuda, Cayman Islands, Cook
  Islands, Equatorial Guinea, Faroe Islands, French Guiana, Greenland,
  Guadeloupe, Martinique, New Caledonia, North Macedonia, Niue, Réunion,
  Eswatini, Syria, Palestine and five more. Global totals move by at
  most **0.99%** (`stock_addition`), every other column inside 0.4%.
  **Bucket 999 survives** as a genuine residual for the territories that
  report nothing, shrinking from 15,507 rows to 516.

  `options(whep.unfold_rest_of_world = "none")` restores the fold, which
  is what reproducing a number published before this change requires; it
  warns on every crosswalk read, because such a run no longer matches
  the published series. The `"successor_state"` folds (Sudan/South Sudan
  into bucket 206) are untouched — those are territorial identities, not
  a FABIO convention, and remain the subject of
  [\#414](https://github.com/eduaguilera/whep/issues/414).

  An earlier measurement in
  [\#419](https://github.com/eduaguilera/whep/issues/419) put this
  change at up to 13.7x on `feed`. That comparison predates the
  `dcast()` duplicate-key fix
  ([\#425](https://github.com/eduaguilera/whep/issues/425)/#429) and
  does not reproduce;
  [\#555](https://github.com/eduaguilera/whep/issues/555) re-measured it
  at 1.0000.

- [`create_typologies_of_josette()`](https://eduaguilera.github.io/whep/reference/create_typologies_of_josette.md)
  and
  [`create_typologies_grafs_spain()`](https://eduaguilera.github.io/whep/reference/create_typologies_grafs_spain.md)
  gained an `example = FALSE` argument, so both now have runnable
  examples like the rest of the package’s remote-data functions. Their
  documented `@return` was also wrong and is corrected:
  [`create_typologies_of_josette()`](https://eduaguilera.github.io/whep/reference/create_typologies_of_josette.md)
  returns a named list of three tibbles plus a `ggplot`, not a single
  tibble, and
  [`create_typologies_grafs_spain()`](https://eduaguilera.github.io/whep/reference/create_typologies_grafs_spain.md)
  returns `Province_name` and `Typologie` for `map_year` only, not a
  seven-column series over all years. No published value changes — the
  only new code path is the `example = TRUE` early return.

- **An aggregation bucket now sums, and comes out under one name.** The
  reader aggregation grouped rows by the member’s polity **name** as
  well as by `polity_area_code`, so a bucket folding members that
  resolve to different polities was never actually summed: it came back
  as several rows under one `area_code`, carrying different `area`
  labels. That is live on the shipped crosswalk, not hypothetical —
  bucket 206 “Sudan (former)” folds FAOSTAT areas 276 Sudan and 277
  South Sudan, which resolve to two polities from 2012 on. Measured over
  the real pins, four sources came out split: `faostat-fbs-new` (2,056
  duplicate `(area_code, year, item, element, unit)` keys),
  `faostat-trade-totals` (3,739), `faostat-production` (2,000) and
  `faostat-emissions-livestock` (144). The label is now derived after
  the sum from the **bucket’s own** code — the same polity
  [`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
  reports and the reporting columns resolve — so one `area_code` has one
  `area` in one year. Each reader’s total is **unchanged to the digit**
  and its row count falls by exactly its duplicate-key count.
  **Published values do move, for bucket 206 only**, because the
  duplicated keys were mishandled downstream in both directions:
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
  changes 1,673 of 6,170,595 keys, all in 2012-2023, and against the raw
  pin the new value is the right one — bucket 206 goats in 2018 were
  14,449,249 head (South Sudan alone, Sudan’s 40,846,000 dropped) and
  are now 55,295,249, while 2019 sugar cane was 10,898,000 t against
  5,449,000 t reported and is now 5,449,000 t. On a real 2005-2020
  [`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
  the effect is 678 changed keys, 559 of them area 206; every other area
  moves by **43.4 t in total across 119 keys** (largest single move 3.91
  t, 4e-9% of the build). Element totals over that range move by 1.79%
  on `stock_variation` and by less than 0.03% on everything else.
  `reporting_polity_code` for bucket 206 is `SUD-1956-2011` before and
  after.

- `polity_end_year` / `end_year` is now read as **exclusive**
  everywhere, which is the convention upstream `whep-polities`
  publishes: a successor’s `start_year` equals its predecessor’s
  `end_year`, and 240 of the 245 FAOSTAT-map rows in
  `polity_area_crosswalk` carry `polity_end_year == map_year_end + 1`.
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  used to join on `polity_end_year >= year`, so a period answered for
  one year past its end. Over the 1961-2024 grid for all 266 crosswalk
  areas that put **7** area-years on their period’s end year, of which
  **3 landed in a state that had already dissolved** and still read
  `"matched"`/`"manual"`: 1993 Czechoslovakia (`F51-1947-1993`), 2006
  Serbia and Montenegro (`SCG-1992-2006`) and 1992 Yugoslav SFR
  (`F248-1991-1992`). Those three now report `"out_of_span"`. The other
  four are years the upstream map explicitly declares the area reports
  (`map_year_end`, inclusive), and the resolver keeps them: a reported
  year is never dropped for being one past a polity’s end. **No
  published value moves**: over 1850-2024 x 266 areas,
  `polity_area_code` is unchanged on every row, the resolved-row count
  is unchanged (46,336 with the default back-cast anchor), and the only
  `polity_code` that moves is area 273 Montenegro in 1962, from
  `MNE-1913-1918` to `MNE-2006-2025` – a nearest-period stand-in either
  way, now landing on the nearer period.
  [`build_constant_territory_series()`](https://eduaguilera.github.io/whep/reference/build_constant_territory_series.md)
  reads the same convention, so a dissolved polity no longer sits on top
  of its successors in the hand-over year (238 polities carried a
  polygon in 1993 on the old reading against 236, and 453 extra active
  polity-years over 1850-2024), where each grid cell goes to exactly one
  target and the predecessor was capturing the ones its successors
  should have received. Note that `ref_year = 2025` now aborts: the
  vintage’s open periods carry 2025 as their exclusive end, so they stop
  at 2024.

- `inst/scripts/prepare_spatialize_all.R` no longer repairs
  `mueller_synthetic_n`’s FAO-style legacy ISO codes with a
  hand-maintained 14-entry `recode()` list. The mapping now comes from
  [`whep::polity_label_aliases`](https://eduaguilera.github.io/whep/reference/polity_label_aliases.md)
  through
  [`resolve_polity_label()`](https://eduaguilera.github.io/whep/reference/resolve_polity_label.md),
  bridged back to the country grid’s numeric `area_code` through the
  polity’s `iso3_code` and the same `regions.csv` lookup the grid is
  rasterised from. **No published values change**: the resulting
  `crop_synthetic` table is byte-identical, 5,043 rows resolving to the
  same 156 area codes with a maximum rate difference of 0. Four of the
  14 list entries (`BHA`, `BAR`, `DMI`, `STL`) named codes the dataset
  never uses.

- New
  [`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
  reports the rows of a built table whose `reporting_polity_code` is a
  nearest-period stand-in, i.e. a polity that did not exist in that
  row’s year.
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  has always reported these as `mapping_status == "out_of_span"`, but
  the reporting-column boundary every area-keyed output crosses dropped
  that column, so the documented uncertainty was invisible in published
  data. Measured on the real FAOSTAT production path
  (`.read_input("faostat-production")` aggregated to polities,
  1961-2024), **5,637 of 3,011,912 rows (0.19%)** are stand-ins, all of
  them bucket 206 “Sudan (former)” over 2012-2024 on `SUD-1956-2011`; on
  `faostat-fbs-old` it is 972 of 5,331,877 (0.018%), the same bucket
  over 2012-2013. Across the whole crosswalk over 1961-2023 it is 922 of
  16,658 resolved area-years in 28 areas, in both directions. **No
  published value or column changes**: the new function is a separate
  query, and carrying the signal on the outputs themselves is opt-in
  through `options(whep.polity_mapping_status = "flag")` for a logical
  `reporting_polity_out_of_span`, or `"status"` for the full
  `reporting_mapping_status`. The default, `"none"`, is today’s schema.
  Which of the two to adopt as the default is an open decision
  ([\#545](https://github.com/eduaguilera/whep/issues/545)).

- [`build_energy_co2_extension()`](https://eduaguilera.github.io/whep/reference/build_energy_co2_extension.md)
  gains a third `unclassified` treatment, `"polity_region"`, for the
  **live** reporting areas `gleam_geographic_hierarchy` has no row for.
  On today’s crosswalk that is Nauru (area 148) and Tuvalu (227): they
  exist, report under their own area codes, and their meat production
  left the extension unpriced. `"polity_region"` groups them by running
  GLEAM’s own scheme rules on the continent their polity carries – no
  grouping label is added to the package – so Tuvalu now lands on
  `"Least developed countries"`, the classification `.energy_ldc_iso3()`
  already asserted for TUV while joining against a table with no TUV
  row. Those rows are labelled `"GLEAM_3.0_energy_meat_polity_region"`
  in `method_energy`. **No published value changes**: the default is
  still `"drop"`, and the full 1850-2023 build is bit-identical under
  both `"drop"` and `"global_mean"`. Measured, for the decision:
  `"polity_region"` adds 366 rows and 2 areas (61,149 to 61,515), moves
  no existing row by any amount, and raises total energy CO2e by
  0.0000155%; it puts Nauru at 288,502 kg CO2e and Tuvalu at 424,851 kg
  over 1961-2023, against 664,412 and 1,775,719 under `"global_mean"`.
  Whether the default should move is left open in whep#415.

- [`folded_reporting_areas()`](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)
  no longer calls all 61 Rest-of-World folds a FABIO convention, because
  for four of them it is not one. FABIO’s own published region list –
  `io_codes.csv` of the v1.1 release (Zenodo record 2577067), 192 areas
  x 125 commodities, the file `inst/scripts/compare_fabio.R` already
  downloads – gives **153 New Caledonia, 154 North Macedonia, 209
  Eswatini and 212 Syria** each their own commodity block, distinct from
  area 999 `RoW`; the FABIO source repository marks all four
  `current == TRUE`, which is exactly the flag its `replace_RoW()` keeps
  out of bucket 999. `regions_full` nonetheless gives them `fabio_code`
  999 while flagging them `cbs` `TRUE`, and Syria is the single largest
  contributor to the fold (24,426 `faostat-production` rows). Those four
  now come back as a third `fold_kind`, `"cbs_reporter_folded"`,
  separating them from the 57 folds FABIO does make; a new
  `options(whep.unfold_rest_of_world = "cbs_reporters")` promotes only
  those four, alongside the existing `TRUE` (equivalently `"all"`) for
  all 61. **No published value changes**: `fabio_code`,
  `polity_area_code` and every polity label are untouched on the default
  path, which the suite pins against the committed crosswalk. Whether to
  correct `regions_full` is an open decision
  ([\#556](https://github.com/eduaguilera/whep/issues/556)); doing so is
  the numeric un-fold of
  [\#563](https://github.com/eduaguilera/whep/issues/563)’s option 3,
  and must not be done at the polity level alone
  ([\#480](https://github.com/eduaguilera/whep/issues/480), reverted in
  [\#561](https://github.com/eduaguilera/whep/issues/561)).

- [`build_feed_demand()`](https://eduaguilera.github.io/whep/reference/build_feed_demand.md)
  gains `region_fallback`, which decides how a reporting bucket the
  crosswalk leaves without a Bouwman feed region gets one. Rest of World
  (`area_code` 999) folds 62 FAOSTAT reporting areas, 58 of which have a
  region of their own, and kept none of them; every region-keyed join
  therefore missed and the bucket’s feed demand went nowhere. The new
  default, `"member_mix"`, splits the bucket across its members’ regions
  weighted by the livestock those members carry (Middle East 0.69,
  Southern Africa 0.18, Oceania 0.045, Eastern Europe 0.045, then five
  smaller regions). `"none"` restores the previous behaviour. **This
  moves published values, for area 999 only.** Measured over a full
  1850-2023
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md):
  with `by = "feed_type"` the mix gains 5,500 keys and 926,327,446 t of
  dry matter (world total +0.151%) where 808,638,528 t had been dropped
  outright, and no key that existed before changes by more than 1e-6 t;
  at `by = "category"` area 999 goes from 808,638,528 to 926,327,446 t
  (+14.6%) with `demand_tier = "ipcc"`, and from 0 to 2,035,462,034 t
  with `demand_tier = "fcr"`. All 191 other areas are bit-identical in
  both tiers. The five continent residuals `901`-`905` stay unmapped on
  purpose: they span several Bouwman regions each and carry no
  production row at all.

- [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  gains `area_key`, choosing which code the shared cell-area grid every
  gridded consumer keys on. The grid is rasterized from present-day
  polygons through `regions.csv`, so its `area_code` is a raw
  reporting-area code, not the `polity_area_code` bucket every
  polity-keyed national table in whep is aggregated on. On the deployed
  `cell_polity_fraction` parquet **12 of its 182 codes (819 cells) are
  not a bucket** – Syria, Palestine, Eswatini, Equatorial Guinea, North
  Macedonia, New Caledonia, Western Sahara, Andorra, Liechtenstein and
  San Marino (all folded into `999` Rest of World), `62` Ethiopia PDR
  (into `238`) and `277` South Sudan (into `206`) – so their cells match
  nothing on either side of the join. Measured against real 2010
  harvested area, **21.09 Mha of national cropland, 1.525% of the world
  total, cannot be placed on any grid cell**; 15.30 Mha of that is the
  whole of Ethiopia and 5.67 Mha is Rest of World.
  `area_key = "polity_area"` re-keys the grid on the bucket and cuts
  that to 0.12 Mha (0.009%). **No published value changes**: the default
  `"grid"` reproduces today’s output bit-for-bit and only adds a warning
  naming the codes, because switching moves every gridded consumer’s
  territorial attribution at once. Whether it should become the default
  is issue [\#460](https://github.com/eduaguilera/whep/issues/460).

- `polities` and `polity_area_crosswalk` are re-synced against upstream
  `whep-polities` at `eb02dcb` (740 rows to **749**), which retired or
  superseded **14** codes this package had been treating as live and
  published a replacement for each. The user-visible consequence is that
  `reporting_polity_code` values change: `ROW-1850-2023` becomes
  `ROW-1850-2025`, the six regional buckets
  `RAFR/RASI/REUR/RNAM/ROCE-1850-2021` and `RLAM-1850-2013` become
  `-1850-2025`, and `CAN-1948-2025` becomes `CAN-1949-2025`.
  Newfoundland acceded on 31 March **1949**, so calendar 1948 now
  resolves to pre-accession Canada (`CAN-1886-1949`, 9,379,600 km2)
  instead of post-accession Canada (9,774,537 km2) – a 394,937 km2
  correction visible only where the back-cast anchor is off,
  i.e. historical trade sources reported under their own borders. The
  bucket extensions recover **88 previously unresolvable area-years**
  over 1961-2024 (`RLAM` alone had lost 2014-2024), of which 20 fall
  inside the default `1850:2023` build range. **No published value is
  expected to move**: `polity_area_code`, the numeric bucket every
  matrix workflow aggregates on, is byte-identical for all 267 reporting
  areas, and the recovered area-years are either year 2024 (outside the
  default range) or areas 901-906, which are WHEP reporting labels no
  source dataset carries. That is a crosswalk-level measurement, not a
  full-pipeline one.

- [`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
  now reports the `area_code` rows that are aggregates of several
  territories, alongside the message it already emitted for the dropped
  regional residuals. `area_code` is `polity_area_code`, a bucket rather
  than an identity, so with the real `gdp-population` pin eight ISO3
  codes fold into two rows: 999 “Rest of World” (Syria, North Macedonia,
  Palestine, Eswatini, Equatorial Guinea, French Guiana) and, from 2012,
  206 “Sudan (former)” (Sudan + South Sudan). That is 0.35% of the
  population over 1850-2021 and 1.05% in 2015, against the 0.07% the
  existing message covered. The fold is deliberate – those are the codes
  the commodity balances are keyed on, so a finer key would leave their
  food supply with no denominator – and the `@return` documentation now
  says a row is an area code rather than a country. **No published value
  changes**: the output of a full real-pin read is byte-identical before
  and after (28,255 rows, 530,970,330,534 person-years).

- [`build_energy_co2_extension()`](https://eduaguilera.github.io/whep/reference/build_energy_co2_extension.md)
  now **reports the meat production it cannot price** instead of
  dropping it in silence. Reporting areas with no row in
  `gleam_geographic_hierarchy` get no energy intensity, so their carcass
  production used to leave the extension without a word: measured on the
  full FAOSTAT production input, that is **595 Mt of carcass weight,
  3.48% of 1850-2023** and **15.3% of 1961**, over eight areas – the
  USSR (436.8 Mt), Belgium-Luxembourg (43.9), Czechoslovakia (38.1), the
  Yugoslav SFR (37.8), the Rest-of-World bucket 999 (25.5), Serbia and
  Montenegro (12.8), Tuvalu and Nauru. A warning now names them with
  their tonnage and share. A new `unclassified` argument selects the
  treatment: `"drop"` (default) keeps the historical behaviour, and
  `"global_mean"` prices those areas at the unweighted world mean of the
  published GLEAM factors, marking the affected rows
  `"GLEAM_3.0_energy_meat_global_mean"` in `method_energy`. **No
  published value changes on the default path** (verified bit-identical
  on the full input); `"global_mean"` raises total energy CO2e by 4.4%
  over 1850-2023, 14.3% in 1961 and 0.17% in 2023. Which treatment is
  right is an open decision
  ([\#492](https://github.com/eduaguilera/whep/issues/492)).

- New
  [`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
  reports every FABIO reporting bucket (`polity_area_code`) that folds
  more than one polity in a year, and says whether the polity the bucket
  itself resolves to covers the fold (`"aggregate"`), covers only part
  of it (`"partial"`), or is absent (`"unlabelled"`). Exactly one bucket
  in the shipped crosswalk is `"partial"`: 206, which folds FAOSTAT
  areas 276 Sudan and 277 South Sudan while no live polity means “Sudan
  and South Sudan”. Measured on real FAOSTAT production for 2015, that
  bucket carries 53,124,088 t for Sudan plus 14,876,146 t for South
  Sudan – 21.9% of the bucket – under one polity label.
  `.aggregate_to_polities()` now warns when it builds such a bucket;
  silence it with `options(whep.warn_polity_folds = FALSE)`. **No
  published value changes:** the fold, the numeric bucket and every
  polity label are exactly as before, and the only new behaviour is the
  warning and the new function.

- The FABIO Rest-of-World fold is now **reported instead of silent**,
  and the measurement that was blocking a decision on it has been
  redone. New
  [`folded_reporting_areas()`](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)
  lists every reporting area whose `polity_area_code` is not its own
  `area_code`: 61 areas folded into Rest of World, of which 14 carry
  observed data (Syria 24,426 `faostat-production` rows, Eswatini
  12,196, Réunion 11,970, Palestine 9,606, the Faroe Islands 2,458 and
  nine more, 130,103 rows in total), plus 3 successor-state folds (62
  into 238, 276 and 277 into 206). `.aggregate_to_polities()` now warns
  per source, naming the areas and the rows it folded, because these
  areas resolve with `mapping_status == "matched"` and so no coverage
  count could show them. **No published value changes.**
  `options(whep.unfold_rest_of_world = TRUE)` promotes each member to
  its own code for sensitivity work; it warns on every crosswalk read
  and is not a production mode. Measured on a full-range
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  (1850-2023), promoting all 61 members moves global totals by at most
  1.2% (`stock_addition`) and under 0.1% for `feed`, `production` and
  `processing` — the 13.7x feed inflation recorded in issue
  [\#419](https://github.com/eduaguilera/whep/issues/419) does not
  reproduce, because that comparison predates the `dcast()`
  duplicate-key fix in `.select_best_source()`
  ([\#425](https://github.com/eduaguilera/whep/issues/425)).

- [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
  gains `federation_land`, controlling how the pre-1962 LUH2 back-cast
  reaches an area whose territory is a dissolved federation. LUH2 land
  use is keyed on present-day ISO3, so 15 Belgium-Luxembourg, 51
  Czechoslovakia, 228 USSR and 248 Yugoslav SFR have no land record of
  their own and their pre-1962 production has never been back-cast at
  all – 14.3% of 1961-62 FAOSTAT production tonnage, USSR alone 12.2%.
  `federation_land = "successor_union"` rebuilds each federation’s land
  series as the sum of its successor states’ LUH2 land, resolved from
  the `successor` relation published in `polities`, and reduces the
  unmatched areas from 4 to 1 (only Belgium-Luxembourg, which upstream
  publishes no successor for). **No published value changes by
  default**: `"none"` keeps current behaviour. Measured on a 1850-1965
  build, `"successor_union"` raises global pre-1962 production tonnage
  by 13.9% (1850) to 19.4% (1960), moves exactly three area codes (51,
  228, 248) and moves no row at or after 1961; it also closes the hard
  0-to-704 Mt discontinuity USSR had at the 1961 splice (1960/1961 now
  differ by 1.2%).

- `polity_area_crosswalk` now takes its area-to-polity mapping from
  **upstream’s published map** (`faostat_area_polity_map.csv`, read via
  `WHEP_POLITIES_FAOSTAT_MAP`, 281 rows over 228 FAOSTAT area codes)
  instead of inferring it from the polity-code string with
  `sub("-.*", "", polity_code)`. The build aborts if the map is absent
  rather than falling back silently. Seven reporting areas gain a
  mapping no prefix could reach – Djibouti (72) had resolved to
  **nothing**, and areas 7, 20, 181, 237, 249 and 251 reach
  `ANG-1905-1975`, `BEC-1885-1966`, `SRH-1953-1964`, `F237-1954-1975`,
  `F249-1918-1990` and `NRH-1953-1964`. Area 15 resolves to
  `BLX-1850-1999` rather than `BLX-1921-1999`, and area 206 “Sudan
  (former)” to `SUD-1956-2011` rather than standing in on post-secession
  `SDN-2011-2025`. Prefix inference is kept, labelled in a new
  `mapping_source` column, only where the map is silent: seven areas it
  does not cover (351 and 901-906) and periods outside the spans it
  declares, which is what keeps pre-1961 history resolvable for sources
  reported under their own borders. The four-part codes that used to
  enter through the prefix collapse (`AZE-SSR-1920-1991`,
  `IDN-BLB/JVM/OTH-1949-1951`, `MMR-LWR-1852-1885`) are gone, taking
  crosswalk `subnational` rows from 6 to 3 and ambiguous `(area, year)`
  resolutions from 199 to 86. **This moves published values and no
  magnitude comparison has been run.**

- `polities` is refreshed from upstream, 603 rows to **740**, because
  the published map names 43 polity codes the old snapshot did not
  contain. This is the refresh
  [\#485](https://github.com/eduaguilera/whep/issues/485) drafted.

- The manure/nutrient chain now documents **one vocabulary for
  `territory`**: a stringified `area_code`, what
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  emits and what the pipeline has always passed. The `@examples` of
  [`estimate_n_excretion()`](https://eduaguilera.github.io/whep/reference/estimate_n_excretion.md),
  [`split_manure_management()`](https://eduaguilera.github.io/whep/reference/split_manure_management.md),
  [`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md),
  [`allocate_manure_to_land()`](https://eduaguilera.github.io/whep/reference/allocate_manure_to_land.md),
  [`allocate_manure_transport()`](https://eduaguilera.github.io/whep/reference/allocate_manure_transport.md)
  and
  [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)
  used ISO literals instead (`"ESP"`, and `"ES"`, which the chain’s own
  resolver rejects outright). Passing an `iso3c` still resolves, as a
  bridge for existing fixtures, but now warns: it can only answer with
  `polity_area_code`, a FABIO aggregation bucket, which for 62 of the
  257 ISO3 codes in `regions_full` is not that territory’s own code (61
  land on 999, Rest of World; `"SSD"` lands on 206, Sudan (former),
  where the numeric form `"277"` keeps South Sudan). No published value
  changes: the pipeline itself never took the ISO3 branch.

- `urban_n_reference` now carries a **`polity_code`** column
  (`"ESP-1800-2025"`) alongside its numeric `area_code`, so the
  benchmark series names the territory it measures instead of only the
  FAOSTAT aggregation bucket `203`. The code is resolved per benchmark
  year against the polity active in that year. Additive: `area_code` and
  every measured value are unchanged, and no exported function reads
  this dataset at runtime. This sets the convention for the other
  territory-keyed coefficient tables.

- [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  no longer presents a **nearest-period stand-in as a real match**. When
  no mapped period covers a row’s year the row still resolves to the
  nearest period of the same area, but `mapping_status` now reports
  `"out_of_span"` instead of inheriting the crosswalk’s
  `"matched"`/`"manual"`, so a figure attributed to a polity that did
  not exist in that year is visible. Over the FAOSTAT era this covers
  993 of 16638 resolved area-years across 36 areas, in both directions:
  FAOSTAT area 206 “Sudan (former)” for 1961-2010 resolved to
  `SDN-2011-2025` (post-secession Sudan, which excludes the territory
  those figures cover) and area 51 Czechoslovakia for 1994-2023 resolved
  to `F51-1947-1993`, a state that had dissolved. Relabelling only: no
  `polity_code` assignment changes (0 of 16638), and no exported table
  carries `mapping_status`, so no published value moves.

- **Crop-residue feed-use fractions are live again.** The
  `residue_feed_fraction` coefficient table’s region column was named
  `region_hanpp` but held UN M49 sub-regions, and
  `calculate_residue_destinies(method = "krausmann_regional")` joined it
  against a `region_hanpp` column the pipeline filled from
  `regions_full$region_HANPP`. The two vocabularies share no label, so
  the join matched nothing and every polity silently took the `"Global"`
  default of `0.20` — a table spanning `0.05` to `0.45`, dead in full.
  The column is renamed to `region_un_sub` (values unchanged, apart from
  `South-Eastern Asia` -\> `South-eastern Asia` to match
  `regions_full`), and the method now requires a `region_un_sub` input
  instead of `region_hanpp`; 230 of 261 areas receive a region-specific
  fraction, the rest (Micronesia, Polynesia, RoW and areas with no M49
  sub-region) keep the `0.20` fallback. This **moves published values**:
  `residue_feed_dm_t` and `residue_burn_dm_t` change, and with them
  [`build_residue_feed_avail()`](https://eduaguilera.github.io/whep/reference/build_residue_feed_avail.md)
  and the nitrogen balance’s `used_residue_n_t` / `burnt_residue_n_t`.
  `residue_soil_dm_t` and their sum do not change (neither depends on
  `feed_use_fraction`), so
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)’s
  residue carbon is unaffected.

- The pre-1962 commodity-balance fills now key their **proxies on the
  polity** rather than on an area name. Three name vocabularies met at
  that join: the frame carries the periodized `polity_name` (FAO area 3
  arrives as `"Albania (1913-2025)"`), the gdp/population pin carries
  its own labels (`"Albania"`) and the LUH2 land table the crosswalk’s
  static `area_name`. 57 of the pin’s 196 names (8,263 rows, 27.8%) and
  96 of the LUH2 labels (41.7% of land rows) were names no builder
  emits, so those territories silently kept their gaps. **This moves
  published values**: proxy coverage of the pre-1962 frame’s (year,
  polity) cells rises from 13,664 to 18,480 of 22,624 for population (43
  polities gain a proxy, none lose one) and from 402 to 567 of 606 for
  agricultural land over 1900-1902 (55 gain, none lose). Aggregates that
  are only reached by folding other territories into them (Rest of
  World, 999) are still left without a proxy: what an aggregate’s proxy
  should be is an open methodological question
  ([\#493](https://github.com/eduaguilera/whep/issues/493)).

- Every area-keyed exported output now carries the **reporting-polity
  columns** (`polity_area_code`, `reporting_polity_code`,
  `reporting_polity_name`, `reporting_polity_has_geometry`), so a caller
  can tell which territory a row belongs to, and whether it has a
  polygon, without re-joining the crosswalk. This widens the contract
  from 10 exports to 35, covering the balances
  ([`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md),
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md),
  [`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md),
  …), the footprint extensions
  ([`get_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/get_crop_land_extension.md),
  [`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md),
  …) and the gridded readers
  ([`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md),
  [`get_soc_climate_drivers()`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md)).
  The columns are additive: no existing column or value changes.
  [`get_faostat_data()`](https://eduaguilera.github.io/whep/reference/get_faostat_data.md)
  keeps returning raw FAOSTAT area names (it is the pre-resolution
  reader), and
  [`build_grazing_feed_footprint()`](https://eduaguilera.github.io/whep/reference/build_grazing_feed_footprint.md)/[`build_land_balance_footprint()`](https://eduaguilera.github.io/whep/reference/build_land_balance_footprint.md)
  are aggregated over time and have no year to resolve a polity against.
  [`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)
  now returns the numeric WHEP `area_code` rather than the character
  territory key its manure-transport reuse works in; an ISO3 input still
  resolves through the same checked resolver as the manure path.

- Add gridded soil **water, carbon and nitrogen balances** (0.5° cell ×
  polity fragment).
  [`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
  closes the annual cell water budget from LPJmL hydrology and exposes
  drainage (for N leaching) plus footprint terms;
  [`get_soc_climate_drivers()`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md)
  emits the monthly SOC climate drivers from CRU.
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  runs historical SOC dynamics (equilibrium init, LUH2-driven land-use
  march, LUC C-transfer) and derives ΔSON via asymmetric C:N;
  `calculate_soc_dynamics(model = c("hsoc","rothc","icbm","amg", "century"))`
  wraps the five SOC models (default `"hsoc"`), fed by
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md).
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  assembles inputs
  ([`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md),
  incl. [`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)/[`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md))
  minus the selectable losses
  ([`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)/[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)/
  [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md),
  with the process-based MANNER NH3 model) into surplus, NUE and
  GWP/CO2e indicators. New readers:
  [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md),
  [`read_cru_climate()`](https://eduaguilera.github.io/whep/reference/read_cru_climate.md),
  [`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md),
  [`read_hyde_population()`](https://eduaguilera.github.io/whep/reference/read_hyde_population.md),
  [`read_lpjml_wind()`](https://eduaguilera.github.io/whep/reference/read_lpjml_wind.md),
  [`read_soil_ph()`](https://eduaguilera.github.io/whep/reference/read_soil_ph.md),
  [`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md)
  (per-cell field capacity, wilting point and porosity from HWSD
  texture, feeding the ICBM SOC moisture modifier). New coefficient
  datasets for SOC turnover, humification, C:N, N2O EFs, MANNER and
  denitrification, the crop growth-stage soil-cover curve
  (`soc_soil_cover_curve`), and the USDA texture-class soil hydraulic
  properties (`soil_hydraulic_by_texture`) with the HWSD texture-code
  crosswalk (`hwsd_texture_usda`).

- Add
  [`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md)
  to complete tibbles from typed zero-row prototypes, with safe casts,
  scalar defaults, deterministic ordering, and explicit extra-column
  handling.

- Add
  [`decompose_weighted_ratio()`](https://eduaguilera.github.io/whep/reference/decompose_weighted_ratio.md)
  for exact Kitagawa/Shapley, additive LMDI, and sequential-polar
  decomposition of changing aggregate ratios.

- [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  and
  [`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
  gain a `method = c("mass", "value")` argument for co-product
  allocation. `"value"` splits a multi-output process’s inputs (and the
  pressures embodied in them) across its products by economic value
  (mass times export price from
  [`build_cbs_prices()`](https://eduaguilera.github.io/whep/reference/build_cbs_prices.md))
  instead of mass, falling back to mass for any process whose
  co-products lack prices. The default `"mass"` preserves previous
  results ([\#100](https://github.com/eduaguilera/whep/issues/100)).

- Add
  [`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md):
  aggregate the IPCC enteric and manure emissions pipeline into a
  greenhouse-gas footprint extension (kg CO2e per `area_code` x
  `item_cbs_code`), with selectable IPCC tier and GWP100 standard, ready
  to feed
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).
  `inst/scripts/footprints.R` gains a `WHEP_FOOTPRINT_PRESSURE` switch
  to trace GHG instead of land
  ([\#100](https://github.com/eduaguilera/whep/issues/100)).

- `calculate_livestock_emissions(tier = 1)` now also computes manure N2O
  (direct and indirect) from IPCC default per-head excretion rates, so
  the default Tier-1 GHG footprint covers enteric CH4 + manure CH4 +
  manure N2O rather than methane alone
  ([\#100](https://github.com/eduaguilera/whep/issues/100)).

- Add
  [`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md):
  IPCC 2019 Tier 1 nitrous-oxide from nitrogen applied to managed soils
  – synthetic fertiliser (F_SN), applied manure (F_ON, FAOSTAT “Manure
  applied to soils”), and above-ground crop residues (F_CR, residue dry
  matter times Table 11.1a N content, net of removal) – as a CO2e
  footprint extension keyed by `(area_code, item_cbs_code)`. F_SN and
  F_ON are FAOSTAT country totals allocated to crops by harvested area.
  Direct (EF1), indirect volatilisation (FracGASF for synthetic,
  FracGASM for manure; residues excluded) and leaching per Ch. 11; EFs
  verified against the IPCC source. Grazing deposition (F_PRP) and
  below-ground residue N are documented follow-ups
  ([\#100](https://github.com/eduaguilera/whep/issues/100)).

- Add
  [`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
  and
  [`align_extension()`](https://eduaguilera.github.io/whep/reference/align_extension.md):
  trace a long-format extension table through the supply chain end to
  end, de-duplicating the per-year alignment glue previously repeated
  across the footprint driver scripts
  ([\#100](https://github.com/eduaguilera/whep/issues/100)).

- Fix non-dairy cattle being misclassified as dairy in the livestock
  emission factors: a case-insensitive `"dairy"` match also matched the
  `"Cattle, non-dairy"` item name, so beef cattle received the much
  larger dairy enteric and manure factors. Non-dairy cattle CH4 (Tier 1
  and Tier 2) is now classified correctly.

- Fix
  [`prepare_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/prepare_livestock_emissions.md)
  failing on real production data when `live_anim_code` is supplied as
  an integer.

- Fix
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  not passing the now-required `feed_mode` argument to the
  feed-redistribution step.

- Fix mojibake area labels in `regions_full`, `polities_cats` and
  `polity_area_crosswalk`: Curaçao, Côte d’Ivoire and “Netherlands
  Antilles / Curaçao” shipped with their accented letters read as pairs
  of Latin-1 characters. The vendored harmonization CSVs are now
  repaired on read in `data-raw`
  ([\#399](https://github.com/eduaguilera/whep/issues/399)).

- `polities_cats` is now derived from `regions_full` rather than
  vendored as a second hand-maintained copy of the same 39 columns, so
  the two can no longer drift. They had: 17 columns disagreed over the
  198 shared area codes, and 95 of the differing cells were the literal
  string `"0"` in `eia`, `iea` and eleven `region_*` columns where
  `regions_full` leaves `NA`. Those 95 cells are now `NA`; the row set,
  row order, column names and column types are unchanged, and the
  deliberate fold of Bhutan into `RASI` and Comoros into `RAFR` is kept
  as an explicit override
  ([\#406](https://github.com/eduaguilera/whep/issues/406)).

- [`consolidate_sources()`](https://eduaguilera.github.io/whep/reference/consolidate_sources.md)
  gains two opt-in `tie_break` options for panels whose sources report
  exact zeros or several quality variants of one cell.
  `coverage = "positive"` counts the coverage tie-break over strictly
  positive values instead of non-missing ones, so a zero-padded series
  no longer wins on inflated coverage; `quality_variants = TRUE`
  collapses a source’s several `quality_col` variants of a cell to its
  best-ranked one instead of aborting. Both default to the previous
  behaviour ([\#139](https://github.com/eduaguilera/whep/issues/139)).

- [`get_faostat_data()`](https://eduaguilera.github.io/whep/reference/get_faostat_data.md)
  no longer attaches and then unloads `FAOSTAT` to make
  [`FAOSTAT::fillCountryCode()`](https://rdrr.io/pkg/FAOSTAT/man/fillCountryCode.html)
  see its lazily loaded `FAOcountryProfile`. The ISO3 lookup now loads
  that dataset explicitly and matches area names itself, reproducing
  `fillCountryCode()`’s rule (exact match against the six profile name
  columns, unresolved when several profile rows match). Verified
  identical on all 232 FAOSTAT area names in `regions_full` and
  upstream’s `faostat_area_polity_map`: 215 resolve, 0 differences. Two
  side effects are gone – the user’s `FAOSTAT` session state is left
  alone, and rows keep their input order instead of being sorted by area
  name by an internal [`merge()`](https://rdrr.io/r/base/merge.html)
  ([\#520](https://github.com/eduaguilera/whep/issues/520)).

- `citation("whep")` now returns two entries – the package itself,
  carrying its CRAN DOI and all five authors, and the FABIO paper the
  model builds on – where it returned only the generated `DESCRIPTION`
  default before. The package entry takes its year from
  `Date/Publication` rather than a hardcoded one. The machine-readable
  equivalents, `CITATION.cff` and `codemeta.json`, ship alongside it,
  and the package gained a [code of
  conduct](https://ropensci.org/code-of-conduct/) and a link from the
  README to the contributing guide. Groundwork for rOpenSci peer review
  ([\#75](https://github.com/eduaguilera/whep/issues/75)).

- The HWSD readers now say which column a local `hwsd_data.csv` is
  missing.
  [`read_soil_ph()`](https://eduaguilera.github.io/whep/reference/read_soil_ph.md),
  [`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md)
  and the soil-carbon clay driver check the extract against the columns
  they are about to read and abort naming the absent ones plus the
  script that re-exports a complete extract, where a partial extract
  previously surfaced as a `dplyr` error
  (`Column t_clay not found in .data`) that read as a code fault rather
  than a stale input. `inst/scripts/export_hwsd_attributes.R` now
  exports `t_clay`, so a re-run produces an extract the clay driver can
  read. No published value changes: a complete extract is read exactly
  as before ([\#596](https://github.com/eduaguilera/whep/issues/596)).

- **[`propagate_fp_uncertainty()`](https://eduaguilera.github.io/whep/reference/propagate_fp_uncertainty.md)
  no longer reseeds the calling session.** Given
  `options = list(seed = )` it called
  [`set.seed()`](https://rdrr.io/r/base/Random.html) and left it set, so
  every random number drawn afterwards depended on having made the call,
  and in a session that had not yet used the RNG it created
  `.Random.seed` where there was none. The seed is now scoped to the
  call and the previous RNG state (or its absence) is restored on
  return. Seeded results are bit-identical to before; unseeded runs
  still consume the caller’s stream, so consecutive unseeded runs remain
  independent draws
  ([\#188](https://github.com/eduaguilera/whep/issues/188)).

- A failed Natural Earth download now reports how to recover instead of
  dying on its own error message. The abort interpolated the layer URL
  as `{.url {.natural_earth_url(layer)}}`, and cli \>= 3.4.0 reads a
  [`{}`](https://rdrr.io/r/base/Paren.html) expression starting with a
  dot as a style name, so the branch raised `Invalid cli literal` and
  the instructions never reached the user. The province typologies
  ([`create_typologies_grafs_spain()`](https://eduaguilera.github.io/whep/reference/create_typologies_grafs_spain.md),
  [`create_typologies_of_josette()`](https://eduaguilera.github.io/whep/reference/create_typologies_of_josette.md))
  are the callers that reach it. No published value changes
  ([\#594](https://github.com/eduaguilera/whep/issues/594)).

## whep 0.3.0

CRAN release: 2026-03-03

- Add
  [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
  and
  [`calculate_lmdi()`](https://eduaguilera.github.io/whep/reference/calculate_lmdi.md)
  ([@jinfama](https://github.com/jinfama),
  [\#65](https://github.com/eduaguilera/whep/issues/65)).
- Build datasets for GRAFS model in Spain
  ([@AliceBeckmann](https://github.com/AliceBeckmann),
  [\#18](https://github.com/eduaguilera/whep/issues/18)).
- Add harmonization functions
  ([@justin-morgan-csic](https://github.com/justin-morgan-csic),
  [\#66](https://github.com/eduaguilera/whep/issues/66)).

## whep 0.2.0

CRAN release: 2025-10-15

- Add gapfilling functions
  [`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md),
  [`fill_sum()`](https://eduaguilera.github.io/whep/reference/fill_sum.md)
  ([@eduaguilera](https://github.com/eduaguilera),
  [\#11](https://github.com/eduaguilera/whep/issues/11)).
- Now examples can’t fail because of unavailable Internet resources
  ([\#58](https://github.com/eduaguilera/whep/issues/58)).

## whep 0.1.0

CRAN release: 2025-07-25

- Work in Progress FABIO model implementation:
  - Build supply-use tables
    ([`build_supply_use()`](https://eduaguilera.github.io/whep/reference/build_supply_use.md))
    ([\#17](https://github.com/eduaguilera/whep/issues/17)).
  - Balance bilateral trade
    ([`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md))
    ([\#8](https://github.com/eduaguilera/whep/issues/8),
    [\#9](https://github.com/eduaguilera/whep/issues/9)).
- Create article `Follow the workflow` for new contributors
  ([\#1](https://github.com/eduaguilera/whep/issues/1),
  [\#2](https://github.com/eduaguilera/whep/issues/2),
  [\#29](https://github.com/eduaguilera/whep/issues/29)).
- Download large datasets with
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
  and `pins` package
  ([\#29](https://github.com/eduaguilera/whep/issues/29),
  [\#43](https://github.com/eduaguilera/whep/issues/43)).
- Get raw FAOSTAT data with
  [`get_faostat_data()`](https://eduaguilera.github.io/whep/reference/get_faostat_data.md)
  wrapper ([\#3](https://github.com/eduaguilera/whep/issues/3)).
- Initial CRAN submission.
