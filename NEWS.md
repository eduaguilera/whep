# whep (development version)

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
  synthetic fertiliser applied to managed soils, allocated to crops by harvested
  area, as a CO2e footprint extension keyed by `(area_code, item_cbs_code)`.
  Direct (EF1) plus indirect (volatilisation, leaching) per Ch. 11. Applied
  manure and crop-residue N are documented follow-ups (#100).
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
