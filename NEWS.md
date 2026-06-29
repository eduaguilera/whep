# whep (development version)

* Add `build_livestock_ghg_extension()`: aggregate the IPCC enteric and manure
  emissions pipeline into a greenhouse-gas footprint extension (kg CO2e per
  `area_code` x `item_cbs_code`), with selectable IPCC tier and GWP100 standard,
  ready to feed `compute_footprint()`. `inst/scripts/footprints.R` gains a
  `WHEP_FOOTPRINT_PRESSURE` switch to trace GHG instead of land (#100).
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
