# Changelog

## whep (development version)

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

- [`consolidate_sources()`](https://eduaguilera.github.io/whep/reference/consolidate_sources.md)
  gains two opt-in `tie_break` options for panels whose sources report
  exact zeros or several quality variants of one cell.
  `coverage = "positive"` counts the coverage tie-break over strictly
  positive values instead of non-missing ones, so a zero-padded series
  no longer wins on inflated coverage; `quality_variants = TRUE`
  collapses a source’s several `quality_col` variants of a cell to its
  best-ranked one instead of aborting. Both default to the previous
  behaviour ([\#139](https://github.com/eduaguilera/whep/issues/139)).

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
