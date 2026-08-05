# whep (development version)

* `folded_reporting_areas()` no longer calls all 61 Rest-of-World folds a FABIO
  convention, because for four of them it is not one. FABIO's own published
  region list -- `io_codes.csv` of the v1.1 release (Zenodo record 2577067),
  192 areas x 125 commodities, the file `inst/scripts/compare_fabio.R` already
  downloads -- gives **153 New Caledonia, 154 North Macedonia, 209 Eswatini and
  212 Syria** each their own commodity block, distinct from area 999 `RoW`; the
  FABIO source repository marks all four `current == TRUE`, which is exactly the
  flag its `replace_RoW()` keeps out of bucket 999. `regions_full` nonetheless
  gives them `fabio_code` 999 while flagging them `cbs` `TRUE`, and Syria is the
  single largest contributor to the fold (24,426 `faostat-production` rows).
  Those four now come back as a third `fold_kind`, `"cbs_reporter_folded"`,
  separating them from the 57 folds FABIO does make; a new
  `options(whep.unfold_rest_of_world = "cbs_reporters")` promotes only those
  four, alongside the existing `TRUE` (equivalently `"all"`) for all 61. **No
  published value changes**: `fabio_code`, `polity_area_code` and every polity
  label are untouched on the default path, which the suite pins against the
  committed crosswalk. Whether to correct `regions_full` is an open decision
  (#556); doing so is the numeric un-fold of #563's option 3, and must not be
  done at the polity level alone (#480, reverted in #561).
* `polities` and `polity_area_crosswalk` are re-synced against upstream
  `whep-polities` at `eb02dcb` (740 rows to **749**), which retired or superseded
  **14** codes this package had been treating as live and published a replacement
  for each. The user-visible consequence is that `reporting_polity_code` values
  change: `ROW-1850-2023` becomes `ROW-1850-2025`, the six regional buckets
  `RAFR/RASI/REUR/RNAM/ROCE-1850-2021` and `RLAM-1850-2013` become `-1850-2025`,
  and `CAN-1948-2025` becomes `CAN-1949-2025`. Newfoundland acceded on 31 March
  **1949**, so calendar 1948 now resolves to pre-accession Canada
  (`CAN-1886-1949`, 9,379,600 km2) instead of post-accession Canada
  (9,774,537 km2) -- a 394,937 km2 correction visible only where the back-cast
  anchor is off, i.e. historical trade sources reported under their own borders.
  The bucket extensions recover **88 previously unresolvable area-years** over
  1961-2024 (`RLAM` alone had lost 2014-2024), of which 20 fall inside the
  default `1850:2023` build range. **No published value is expected to move**:
  `polity_area_code`, the numeric bucket every matrix workflow aggregates on, is
  byte-identical for all 267 reporting areas, and the recovered area-years are
  either year 2024 (outside the default range) or areas 901-906, which are WHEP
  reporting labels no source dataset carries. That is a crosswalk-level
  measurement, not a full-pipeline one.
* `read_population()` now reports the `area_code` rows that are aggregates of
  several territories, alongside the message it already emitted for the dropped
  regional residuals. `area_code` is `polity_area_code`, a bucket rather than an
  identity, so with the real `gdp-population` pin eight ISO3 codes fold into two
  rows: 999 "Rest of World" (Syria, North Macedonia, Palestine, Eswatini,
  Equatorial Guinea, French Guiana) and, from 2012, 206 "Sudan (former)"
  (Sudan + South Sudan). That is 0.35% of the population over 1850-2021 and
  1.05% in 2015, against the 0.07% the existing message covered. The fold is
  deliberate -- those are the codes the commodity balances are keyed on, so a
  finer key would leave their food supply with no denominator -- and the
  `@return` documentation now says a row is an area code rather than a country.
  **No published value changes**: the output of a full real-pin read is
  byte-identical before and after (28,255 rows, 530,970,330,534 person-years).
* `build_energy_co2_extension()` now **reports the meat production it cannot
  price** instead of dropping it in silence. Reporting areas with no row in
  `gleam_geographic_hierarchy` get no energy intensity, so their carcass
  production used to leave the extension without a word: measured on the full
  FAOSTAT production input, that is **595 Mt of carcass weight, 3.48% of
  1850-2023** and **15.3% of 1961**, over eight areas -- the USSR (436.8 Mt),
  Belgium-Luxembourg (43.9), Czechoslovakia (38.1), the Yugoslav SFR (37.8), the
  Rest-of-World bucket 999 (25.5), Serbia and Montenegro (12.8), Tuvalu and
  Nauru. A warning now names them with their tonnage and share. A new
  `unclassified` argument selects the treatment: `"drop"` (default) keeps the
  historical behaviour, and `"global_mean"` prices those areas at the unweighted
  world mean of the published GLEAM factors, marking the affected rows
  `"GLEAM_3.0_energy_meat_global_mean"` in `method_energy`. **No published value
  changes on the default path** (verified bit-identical on the full input);
  `"global_mean"` raises total energy CO2e by 4.4% over 1850-2023, 14.3% in 1961
  and 0.17% in 2023. Which treatment is right is an open decision (#492).
* New `polity_bucket_coverage()` reports every FABIO reporting bucket
  (`polity_area_code`) that folds more than one polity in a year, and says
  whether the polity the bucket itself resolves to covers the fold
  (`"aggregate"`), covers only part of it (`"partial"`), or is absent
  (`"unlabelled"`). Exactly one bucket in the shipped crosswalk is `"partial"`:
  206, which folds FAOSTAT areas 276 Sudan and 277 South Sudan while no live
  polity means "Sudan and South Sudan". Measured on real FAOSTAT production for
  2015, that bucket carries 53,124,088 t for Sudan plus 14,876,146 t for South
  Sudan -- 21.9% of the bucket -- under one polity label.
  `.aggregate_to_polities()` now warns when it builds such a bucket; silence it
  with `options(whep.warn_polity_folds = FALSE)`. **No published value changes:**
  the fold, the numeric bucket and every polity label are exactly as before, and
  the only new behaviour is the warning and the new function.
* The FABIO Rest-of-World fold is now **reported instead of silent**, and the
  measurement that was blocking a decision on it has been redone. New
  `folded_reporting_areas()` lists every reporting area whose `polity_area_code`
  is not its own `area_code`: 61 areas folded into Rest of World, of which 14
  carry observed data (Syria 24,426 `faostat-production` rows, Eswatini 12,196,
  Réunion 11,970, Palestine 9,606, the Faroe Islands 2,458 and nine more, 130,103
  rows in total), plus 3 successor-state folds (62 into 238, 276 and 277 into
  206). `.aggregate_to_polities()` now warns per source, naming the areas and the
  rows it folded, because these areas resolve with `mapping_status == "matched"`
  and so no coverage count could show them. **No published value changes.**
  `options(whep.unfold_rest_of_world = TRUE)` promotes each member to its own
  code for sensitivity work; it warns on every crosswalk read and is not a
  production mode. Measured on a full-range `get_wide_cbs()` (1850-2023),
  promoting all 61 members moves global totals by at most 1.2%
  (`stock_addition`) and under 0.1% for `feed`, `production` and `processing` —
  the 13.7x feed inflation recorded in issue #419 does not reproduce, because
  that comparison predates the `dcast()` duplicate-key fix in
  `.select_best_source()` (#425).

* `polity_area_crosswalk` now takes its area-to-polity mapping from
  **upstream's published map** (`faostat_area_polity_map.csv`, read via
  `WHEP_POLITIES_FAOSTAT_MAP`, 281 rows over 228 FAOSTAT area codes) instead of
  inferring it from the polity-code string with `sub("-.*", "", polity_code)`.
  The build aborts if the map is absent rather than falling back silently. Seven
  reporting areas gain a mapping no prefix could reach -- Djibouti (72) had
  resolved to **nothing**, and areas 7, 20, 181, 237, 249 and 251 reach
  `ANG-1905-1975`, `BEC-1885-1966`, `SRH-1953-1964`, `F237-1954-1975`,
  `F249-1918-1990` and `NRH-1953-1964`. Area 15 resolves to `BLX-1850-1999`
  rather than `BLX-1921-1999`, and area 206 "Sudan (former)" to `SUD-1956-2011`
  rather than standing in on post-secession `SDN-2011-2025`. Prefix inference is
  kept, labelled in a new `mapping_source` column, only where the map is silent:
  seven areas it does not cover (351 and 901-906) and periods outside the spans
  it declares, which is what keeps pre-1961 history resolvable for sources
  reported under their own borders. The four-part codes that used to enter
  through the prefix collapse (`AZE-SSR-1920-1991`, `IDN-BLB/JVM/OTH-1949-1951`,
  `MMR-LWR-1852-1885`) are gone, taking crosswalk `subnational` rows from 6 to 3
  and ambiguous `(area, year)` resolutions from 199 to 86. **This moves published
  values and no magnitude comparison has been run.**
* `polities` is refreshed from upstream, 603 rows to **740**, because the
  published map names 43 polity codes the old snapshot did not contain. This is
  the refresh #485 drafted.

* The manure/nutrient chain now documents **one vocabulary for `territory`**: a
  stringified `area_code`, what `redistribute_feed()` emits and what the
  pipeline has always passed. The `@examples` of `estimate_n_excretion()`,
  `split_manure_management()`, `apply_management_losses()`,
  `allocate_manure_to_land()`, `allocate_manure_transport()` and
  `build_livestock_nutrient_flows()` used ISO literals instead (`"ESP"`, and
  `"ES"`, which the chain's own resolver rejects outright). Passing an `iso3c`
  still resolves, as a bridge for existing fixtures, but now warns: it can only
  answer with `polity_area_code`, a FABIO aggregation bucket, which for 62 of
  the 257 ISO3 codes in `regions_full` is not that territory's own code (61 land
  on 999, Rest of World; `"SSD"` lands on 206, Sudan (former), where the numeric
  form `"277"` keeps South Sudan). No published value changes: the pipeline
  itself never took the ISO3 branch.

* `urban_n_reference` now carries a **`polity_code`** column
  (`"ESP-1800-2025"`) alongside its numeric `area_code`, so the benchmark series
  names the territory it measures instead of only the FAOSTAT aggregation bucket
  `203`. The code is resolved per benchmark year against the polity active in
  that year. Additive: `area_code` and every measured value are unchanged, and
  no exported function reads this dataset at runtime. This sets the convention
  for the other territory-keyed coefficient tables.

* `add_polity_code()` no longer presents a **nearest-period stand-in as a real
  match**. When no mapped period covers a row's year the row still resolves to
  the nearest period of the same area, but `mapping_status` now reports
  `"out_of_span"` instead of inheriting the crosswalk's `"matched"`/`"manual"`,
  so a figure attributed to a polity that did not exist in that year is
  visible. Over the FAOSTAT era this covers 993 of 16638 resolved area-years
  across 36 areas, in both directions: FAOSTAT area 206 "Sudan (former)" for
  1961-2010 resolved to `SDN-2011-2025` (post-secession Sudan, which excludes
  the territory those figures cover) and area 51 Czechoslovakia for 1994-2023
  resolved to `F51-1947-1993`, a state that had dissolved. Relabelling only:
  no `polity_code` assignment changes (0 of 16638), and no exported table
  carries `mapping_status`, so no published value moves.
* **Crop-residue feed-use fractions are live again.** The
  `residue_feed_fraction` coefficient table's region column was named
  `region_hanpp` but held UN M49 sub-regions, and
  `calculate_residue_destinies(method = "krausmann_regional")` joined it against
  a `region_hanpp` column the pipeline filled from `regions_full$region_HANPP`.
  The two vocabularies share no label, so the join matched nothing and every
  polity silently took the `"Global"` default of `0.20` — a table spanning
  `0.05` to `0.45`, dead in full. The column is renamed to `region_un_sub`
  (values unchanged, apart from `South-Eastern Asia` -> `South-eastern Asia` to
  match `regions_full`), and the method now requires a `region_un_sub` input
  instead of `region_hanpp`; 230 of 261 areas receive a region-specific
  fraction, the rest (Micronesia, Polynesia, RoW and areas with no M49
  sub-region) keep the `0.20` fallback. This **moves published values**:
  `residue_feed_dm_t` and `residue_burn_dm_t` change, and with them
  `build_residue_feed_avail()` and the nitrogen balance's `used_residue_n_t` /
  `burnt_residue_n_t`. `residue_soil_dm_t` and their sum do not change (neither
  depends on `feed_use_fraction`), so `build_soil_carbon_inputs()`'s residue
  carbon is unaffected.
* Every area-keyed exported output now carries the **reporting-polity columns**
  (`polity_area_code`, `reporting_polity_code`, `reporting_polity_name`,
  `reporting_polity_has_geometry`), so a caller can tell which territory a row
  belongs to, and whether it has a polygon, without re-joining the crosswalk.
  This widens the contract from 10 exports to 35, covering the balances
  (`build_nitrogen_balance()`, `build_carbon_balance()`, `build_water_balance()`,
  ...), the footprint extensions (`get_crop_land_extension()`,
  `build_livestock_ghg_extension()`, ...) and the gridded readers
  (`read_luh2_landuse()`, `get_soc_climate_drivers()`). The columns are
  additive: no existing column or value changes. `get_faostat_data()` keeps
  returning raw FAOSTAT area names (it is the pre-resolution reader), and
  `build_grazing_feed_footprint()`/`build_land_balance_footprint()` are
  aggregated over time and have no year to resolve a polity against.
  `build_urban_n()` now returns the numeric WHEP `area_code` rather than the
  character territory key its manure-transport reuse works in; an ISO3 input
  still resolves through the same checked resolver as the manure path.
* Add gridded soil **water, carbon and nitrogen balances** (0.5° cell × polity
  fragment). `build_water_balance()` closes the annual cell water budget from
  LPJmL hydrology and exposes drainage (for N leaching) plus footprint terms;
  `get_soc_climate_drivers()` emits the monthly SOC climate drivers from CRU.
  `build_carbon_balance()` runs historical SOC dynamics (equilibrium init,
  LUH2-driven land-use march, LUC C-transfer) and derives ΔSON via asymmetric
  C:N; `calculate_soc_dynamics(model = c("hsoc","rothc","icbm","amg",
  "century"))` wraps the five SOC models (default `"hsoc"`), fed by
  `build_soil_carbon_inputs()`. `build_nitrogen_balance()` assembles inputs
  (`build_n_inputs()`, incl. `build_n_deposition()`/`build_urban_n()`) minus the
  selectable losses (`calculate_nh3()`/`calculate_soil_n2o()`/
  `calculate_n_leaching()`, with the process-based MANNER NH3 model) into
  surplus, NUE and GWP/CO2e indicators. New readers: `read_lpjml_hydrology()`,
  `read_cru_climate()`, `read_luh2_landuse()`, `read_hyde_population()`,
  `read_lpjml_wind()`, `read_soil_ph()`, `read_soil_hydraulic()` (per-cell
  field capacity, wilting point and porosity from HWSD texture, feeding the
  ICBM SOC moisture modifier). New coefficient datasets for SOC turnover,
  humification, C:N, N2O EFs, MANNER and denitrification, the crop
  growth-stage soil-cover curve (`soc_soil_cover_curve`), and the USDA
  texture-class soil hydraulic properties (`soil_hydraulic_by_texture`) with
  the HWSD texture-code crosswalk (`hwsd_texture_usda`).

* Add `ensure_columns()` to complete tibbles from typed zero-row prototypes,
  with safe casts, scalar defaults, deterministic ordering, and explicit
  extra-column handling.
* Add `decompose_weighted_ratio()` for exact Kitagawa/Shapley, additive LMDI,
  and sequential-polar decomposition of changing aggregate ratios.
* `build_io_model()` and `build_footprint()` gain a `method = c("mass",
  "value")` argument for co-product allocation. `"value"` splits a multi-output
  process's inputs (and the pressures embodied in them) across its products by
  economic value (mass times export price from `build_cbs_prices()`) instead of
  mass, falling back to mass for any process whose co-products lack prices. The
  default `"mass"` preserves previous results (#100).

* Add `build_livestock_ghg_extension()`: aggregate the IPCC enteric and manure
  emissions pipeline into a greenhouse-gas footprint extension (kg CO2e per
  `area_code` x `item_cbs_code`), with selectable IPCC tier and GWP100 standard,
  ready to feed `compute_footprint()`. `inst/scripts/footprints.R` gains a
  `WHEP_FOOTPRINT_PRESSURE` switch to trace GHG instead of land (#100).
* `calculate_livestock_emissions(tier = 1)` now also computes manure N2O
  (direct and indirect) from IPCC default per-head excretion rates, so the
  default Tier-1 GHG footprint covers enteric CH4 + manure CH4 + manure N2O
  rather than methane alone (#100).
* Add `build_crop_soil_n2o_extension()`: IPCC 2019 Tier 1 nitrous-oxide from
  nitrogen applied to managed soils -- synthetic fertiliser (F_SN), applied
  manure (F_ON, FAOSTAT "Manure applied to soils"), and above-ground crop
  residues (F_CR, residue dry matter times Table 11.1a N content, net of
  removal) -- as a CO2e footprint extension keyed by `(area_code,
  item_cbs_code)`. F_SN and F_ON are FAOSTAT country totals allocated to crops
  by harvested area. Direct (EF1), indirect volatilisation (FracGASF for
  synthetic, FracGASM for manure; residues excluded) and leaching per Ch. 11;
  EFs verified against the IPCC source. Grazing deposition (F_PRP) and
  below-ground residue N are documented follow-ups (#100).
* Add `build_footprint()` and `align_extension()`: trace a long-format
  extension table through the supply chain end to end, de-duplicating the
  per-year alignment glue previously repeated across the footprint driver
  scripts (#100).
* Fix non-dairy cattle being misclassified as dairy in the livestock emission
  factors: a case-insensitive `"dairy"` match also matched the `"Cattle,
  non-dairy"` item name, so beef cattle received the much larger dairy enteric
  and manure factors. Non-dairy cattle CH4 (Tier 1 and Tier 2) is now classified
  correctly.
* Fix `prepare_livestock_emissions()` failing on real production data when
  `live_anim_code` is supplied as an integer.
* Fix `build_io_model()` not passing the now-required `feed_mode` argument to
  the feed-redistribution step.
* Fix mojibake area labels in `regions_full`, `polities_cats` and
  `polity_area_crosswalk`: Curaçao, Côte d'Ivoire and "Netherlands Antilles /
  Curaçao" shipped with their accented letters read as pairs of Latin-1
  characters. The vendored harmonization CSVs are now repaired on read in
  `data-raw` (#399).
* `polities_cats` is now derived from `regions_full` rather than vendored as a
  second hand-maintained copy of the same 39 columns, so the two can no longer
  drift. They had: 17 columns disagreed over the 198 shared area codes, and 95
  of the differing cells were the literal string `"0"` in `eia`, `iea` and
  eleven `region_*` columns where `regions_full` leaves `NA`. Those 95 cells are
  now `NA`; the row set, row order, column names and column types are unchanged,
  and the deliberate fold of Bhutan into `RASI` and Comoros into `RAFR` is kept
  as an explicit override (#406).
* `consolidate_sources()` gains two opt-in `tie_break` options for panels whose
  sources report exact zeros or several quality variants of one cell.
  `coverage = "positive"` counts the coverage tie-break over strictly positive
  values instead of non-missing ones, so a zero-padded series no longer wins on
  inflated coverage; `quality_variants = TRUE` collapses a source's several
  `quality_col` variants of a cell to its best-ranked one instead of aborting.
  Both default to the previous behaviour (#139).
* `get_faostat_data()` no longer attaches and then unloads `FAOSTAT` to make
  `FAOSTAT::fillCountryCode()` see its lazily loaded `FAOcountryProfile`. The
  ISO3 lookup now loads that dataset explicitly and matches area names itself,
  reproducing `fillCountryCode()`'s rule (exact match against the six profile
  name columns, unresolved when several profile rows match). Verified identical
  on all 232 FAOSTAT area names in `regions_full` and upstream's
  `faostat_area_polity_map`: 215 resolve, 0 differences. Two side effects are
  gone -- the user's `FAOSTAT` session state is left alone, and rows keep their
  input order instead of being sorted by area name by an internal `merge()`
  (#520).

# whep 0.3.0

* Add `fill_proxy_growth()` and `calculate_lmdi()` (@jinfama, #65).
* Build datasets for GRAFS model in Spain (@AliceBeckmann, #18).
* Add harmonization functions (@justin-morgan-csic, #66).

# whep 0.2.0

* Add gapfilling functions `fill_linear()`, `fill_sum()` (@eduaguilera, #11).
* Now examples can't fail because of unavailable Internet resources (#58).

# whep 0.1.0

* Work in Progress FABIO model implementation:
  - Build supply-use tables (`build_supply_use()`) (#17).
  - Balance bilateral trade (`get_bilateral_trade()`) (#8, #9).
* Create article `Follow the workflow` for new contributors (#1, #2, #29).
* Download large datasets with `whep_read_file()` and `pins` package (#29, #43).
* Get raw FAOSTAT data with `get_faostat_data()` wrapper (#3).
* Initial CRAN submission.
