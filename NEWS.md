# whep (development version)

* The base production dataset is now keyed by the period-specific WHEP
  polity: `build_primary_production()`/`get_primary_production()` return
  `year, polity_code, item..., unit, value, source`, with no FABIO
  Rest-of-World aggregation baked in and no source-specific identifiers
  (the FAOSTAT `area_code` is internal only). Each source is matched to
  polities on its own path: FAOSTAT-era and back-cast rows via the curated
  alias crosswalk at the 1961 anchor, genuine historical rows
  (`historical_*` sources) to their vintage polity (a 1900 France row maps
  to `FRA-1871-1919`, not the modern republic), and LUH2 land — present-day
  boundaries back-cast in time — to the present-day polity (so e.g. South
  Sudan's grassland is never attributed to a defunct predecessor). The
  collapse to the closed FABIO region list moved into the new exported
  `collapse_to_fabio_regions()`, applied explicitly at the CBS/IO matrix
  boundary, and accepts both area-keyed and polity-keyed input. Derived
  polity metadata for area-keyed tables is re-joinable on demand via the
  new exported `add_reporting_polity_columns()` (#120).

* `polity_area_crosswalk` is now built from the curated alias table
  maintained in the whep-polities repository (see `data-raw/table_mappings.R`
  and the `WHEP_POLITIES_ALIASES` env var) instead of a prefix-matching
  heuristic over `regions_full`; `regions_full` is kept only for
  `fabio_code`/`cbs`/region metadata. Identifiable territories that the old
  crosswalk folded into the Rest-of-World polity (Syria, North Macedonia,
  Eswatini, French Guiana, Palestine, micro-states, ...) keep their own
  polity, and former-entity reporting areas map to their former polity
  (Sudan (former) to the undivided-Sudan polity, never the rump
  successor) (#120).

* Fix a commodity-balance double count: CBS source selection keys on area name
  plus code, and inputs previously carried two different names for the same
  `area_code` (the crosswalk polity name from raw-input aggregation vs the
  FAOSTAT area name from production). Both series survived selection and were
  summed, roughly doubling CBS values for every area whose polity name differs
  from its FAOSTAT name (China, Türkiye, Iran, Bolivia, Taiwan, ...). All CBS
  inputs now share the canonical crosswalk area name, so source selection
  merges them correctly (#120).

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
