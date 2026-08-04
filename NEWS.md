# whep (development version)

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
