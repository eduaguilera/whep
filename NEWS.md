# whep (development version)

* Missing `iso3_code` and `cow_code` in `polities` are now real `NA` rather than
  the literal string `"NA"`. The GeoPackage round-trip writes missing text as
  `"NA"`, so 79 rows of `iso3_code` and 185 of `cow_code` looked present:
  `is.na(iso3_code)` found 3 missing codes when 82 were missing, and any
  `!is.na(iso3)` guard treated those rows as carrying a valid ISO3. Converted at
  the read for every character column, since `"NA"` is not a legitimate value for
  any of them (Namibia is `NAM`).

* New `resolve_polity_label()` and the `polity_label_aliases` dataset resolve a
  source's country **label** to a polity. `add_polity_code()` handles numeric
  FAOSTAT area codes, and nothing handled labels — so datasets carrying them had
  no supported path to a polity: `mueller_synthetic_n$iso3c` holds FAO-style
  legacy codes (`"BZE"`, `"ROM"`, `"ZAR"`) and
  `lassaletta_grassland_share$Country` holds name variants (`"Cape Verde"`,
  `"Swaziland"`). Resolution is source- and year-aware, since the same label can
  mean different things per source and its referent changes over time (`"Cape
  Verde"` is the Portuguese colony before 1975 and Cabo Verde after). The mapping
  is a copy of the contract published by whep-polities, not a lookup built here.

* Pre-independence FAOSTAT years no longer resolve to the modern polity. The
  crosswalk mapped each reporting area to a single polity prefix, and upstream
  gives the colonial and modern polities of seven chains different prefixes
  (Angola `ANG-1905-1975` then `AGO-1975-2025`, Sudan `SUD` then `SDN`, Zimbabwe
  `SRH` then `ZWE`, Botswana `BEC`/`BWA`, Zambia `NRH`/`ZMB`, Viet Nam `F237`,
  Yemen `F249`). The colonial polity was unreachable, so `add_polity_code()` fell
  back to the modern one: 1965 Angola resolved to "Angola (independent,
  1975-2025)", and 1970 Sudan to post-secession `SDN-2011-2025`, which excludes
  the South Sudanese territory those reports covered. `manual_area_prefixes` now
  lists both prefixes per area. 118 area-years change attribution, all of which
  previously had NO span-covering polity.

* `.iso3_to_fao_area_code()` no longer breaks an ambiguous ISO3 on row order. An
  ISO3 can name two FAOSTAT reporting areas because the pre-split entity is kept
  beside its successor (`ETH` is both 62 "Ethiopia PDR" and 238 "Ethiopia"), and
  `unique(bridge, by = "iso3c")` was resolving `ETH` to the **dissolved** area 62
  for every year. The tie is now broken on the polities database — prefer the
  area that is its polity's `polity_area_code` — and the function aborts rather
  than guessing if that leaves a choice.

* `.filter_dissolved_countries()` in `build_production()` no longer hardcodes
  FAOSTAT area codes with bare year cutoffs. The bounds now live in
  `.production_reporting_windows`, which separates the two things the integer
  version conflated: **polity existence** (Slovakia is `SVK-1993-2025`, so a
  1970 Slovakia row is attributed to a polity that did not exist) and **FAOSTAT
  reporting convention** (Belgium is `BEL-1831-2025`, but the source files it
  inside Belgium-Luxembourg until 1999, so admitting Belgium rows earlier would
  double-count). Only the first is derivable from the polities database, which
  is why the table is not replaced by a join — but the polity span now bounds
  every window, checked by `test_polity_reporting_windows.R`. That check found
  two missing lower bounds: the old filter admitted Czechoslovakia before 1918
  and Belgium-Luxembourg before 1850, which matters now that `historical_data`
  feeds pre-1961 rows through the same path. No row that previously survived is
  readmitted.

* **Breaking:** the `polity_code` column of `regions_full` and `polities_cats`
  is renamed `polity_prefix`. It never held polity codes — all 271 values were
  bare ISO3-shaped family prefixes (`"AFG"`, `"ROW"`), so joining it to
  `polities`, whose codes are periodized (`"AFG-1919-2025"`), silently matched
  nothing. Join on `reporting_polity_code` instead, which carries the real
  code. `inst/extdata/harmonization/regions_full.csv` and `polities_cats.csv`
  are renamed to match, and `test_polity_prefix_naming.R` now fails if either
  column regains the misleading name.

* `regions_full` and `polities_cats` are rebuilt against the current
  whep-polities database. The committed copies were stale and 12 area codes
  still resolved to superseded polities — 8 of them explicitly retired
  upstream (Angola to `AGO-1816-2025` rather than `AGO-1975-2025`, Brazil to
  the collapsed row rather than the post-Acre split).

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
