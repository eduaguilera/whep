# whep (development version)

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
