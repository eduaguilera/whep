# Tests for the soil-balance coefficient datasets (Module B, Task B1):
# soc_turnover_params, amg_h_by_input_type, soil_cn_ratios.

test_that("soil_cn_ratios has the expected Cropland/Conventional ratios", {
  row <- whep::soil_cn_ratios |>
    dplyr::filter(
      cropland_class == "Cropland",
      management == "Conventional"
    )

  testthat::expect_equal(nrow(row), 1L)
  testthat::expect_equal(dplyr::pull(row, cn_mineralization), 8)
  testthat::expect_equal(dplyr::pull(row, cn_sequestration), 11)
})

test_that("soil_cn_ratios covers all class-management combinations", {
  combos <- whep::soil_cn_ratios |>
    dplyr::distinct(cropland_class, management)

  testthat::expect_equal(nrow(combos), 4L)
  pointblank::expect_col_vals_in_set(
    whep::soil_cn_ratios,
    columns = "management",
    set = c("Conventional", "Organic")
  )
})

test_that("soil_cn_ratios Organic rows ship but are not selectable here", {
  # Issue 809. Both halves are pinned deliberately, because documenting an
  # intention in prose does not stop the next person changing it by accident:
  #
  #   1. the Organic rows DO ship -- retiring them would be a data change
  #   2. and nothing in this package selects them -- build_carbon_balance() has
  #      no management dimension, so an argument for it could only be set
  #      world-wide, and running the whole world as organic is not a meaningful
  #      request. They are reference values for downstream consumers.
  #
  # If management is ever exposed, this test fails and has to be edited on
  # purpose -- which is the point.
  organic <- whep::soil_cn_ratios |>
    dplyr::filter(.data$management == "Organic")
  testthat::expect_equal(nrow(organic), 2L)
  testthat::expect_setequal(
    organic$cropland_class,
    c("Cropland", "NonCropland")
  )

  # The only reader of this table filters to Conventional, so every row it
  # returns is Conventional and the Organic pair is unreachable.
  lookup <- whep:::.cb_cn_lookup()
  testthat::expect_equal(nrow(lookup), 2L)
  testthat::expect_setequal(lookup$cropland_class, c("Cropland", "NonCropland"))
  conventional <- whep::soil_cn_ratios |>
    dplyr::filter(.data$management == "Conventional")
  testthat::expect_equal(
    lookup$cn_mineralization[order(lookup$cropland_class)],
    conventional$cn_mineralization[order(conventional$cropland_class)]
  )
})

test_that("soc_turnover_params has the HSOC fresh and humus rates", {
  fresh <- whep::soc_turnover_params |>
    dplyr::filter(
      model == "hsoc",
      component == "fresh",
      parameter == "decomposition_rate"
    ) |>
    dplyr::pull(value)

  humus <- whep::soc_turnover_params |>
    dplyr::filter(
      model == "hsoc",
      component == "humus",
      parameter == "decomposition_rate"
    ) |>
    dplyr::pull(value)

  testthat::expect_equal(fresh, 0.48)
  testthat::expect_equal(humus, 0.02)
})

test_that("soc_turnover_params namespaces all five models", {
  models <- whep::soc_turnover_params |>
    dplyr::distinct(model) |>
    dplyr::pull(model)

  testthat::expect_setequal(
    models,
    c("hsoc", "rothc", "icbm", "amg", "century", "lpjml")
  )
})

test_that("amg_h_by_input_type default fallthrough is 0.15", {
  default_h <- whep::amg_h_by_input_type |>
    dplyr::filter(input_type == "default") |>
    dplyr::pull(h)

  testthat::expect_equal(default_h, 0.15)
})

test_that("amg_h_by_input_type values match the published h table", {
  lookup <- whep::amg_h_by_input_type |>
    dplyr::select(input_type, h) |>
    tibble::deframe()

  testthat::expect_equal(lookup[["green_manure"]], 0.20)
  testthat::expect_equal(lookup[["mineral_manure"]], 0.30)
  testthat::expect_equal(lookup[["none"]], 0.10)
  testthat::expect_equal(lookup[["manure"]], 0.40)
  testthat::expect_equal(lookup[["residue"]], 0.13)
  testthat::expect_equal(lookup[["mineral"]], 0.13)
})

test_that("residue_humification has the expected columns", {
  pointblank::expect_col_exists(
    whep::residue_humification,
    columns = c("input_type", "humified_fraction", "description")
  )
})

test_that("residue_humification transcribes the source anchors", {
  lookup <- whep::residue_humification |>
    dplyr::select(input_type, humified_fraction) |>
    tibble::deframe()

  # Weed/grass aboveground = the source table's "Grass"
  # Residue_humified_kgC_kgC.
  testthat::expect_equal(
    lookup[["weed"]],
    0.1153466666666667,
    tolerance = 1e-6
  )
  # Crop residue (herbaceous aboveground) shares the same coefficient.
  testthat::expect_equal(
    lookup[["crop_residue"]],
    0.1153466666666667,
    tolerance = 1e-6
  )
  # Root (belowground herbaceous) coefficient.
  testthat::expect_equal(
    lookup[["root"]],
    0.1782459207459208,
    tolerance = 1e-6
  )
  # Excreta / manure average over species.
  testthat::expect_equal(
    lookup[["excreta"]],
    0.2543363636363637,
    tolerance = 1e-6
  )
})

test_that("residue_humification fractions are valid proportions", {
  pointblank::expect_col_vals_between(
    whep::residue_humification,
    columns = "humified_fraction",
    left = 0,
    right = 1
  )
})

# Module C (Task C1) nitrogen-loss coefficient datasets.

test_that("n2o_efs_disaggregated has the expected columns", {
  pointblank::expect_col_exists(
    whep::n2o_efs_disaggregated,
    columns = c("irrig_type", "climate", "ef")
  )
})

test_that("n2o_efs_disaggregated transcribes the Cayuela 2017 anchors", {
  ef <- function(.irrig, .climate) {
    whep::n2o_efs_disaggregated |>
      dplyr::filter(irrig_type == .irrig, climate == .climate) |>
      dplyr::pull(ef)
  }

  testthat::expect_equal(ef("Tier_1", "ATL"), 0.0100)
  testthat::expect_equal(ef("Rainfed", "MED"), 0.0027)
  testthat::expect_equal(ef("Drip", "MED"), 0.0051)
  testthat::expect_equal(ef("Sprinkler", "MED"), 0.0091)
  testthat::expect_equal(ef("Flooded", "MED"), 0.0019)
  testthat::expect_equal(ef("Med_average", "MED"), 0.0050)
})

test_that("fertiliser_n2o_modifiers transcribes the MF anchors", {
  pointblank::expect_col_exists(
    whep::fertiliser_n2o_modifiers,
    columns = c("fert_type", "climate", "mf", "source")
  )

  mf <- function(.fert, .climate) {
    whep::fertiliser_n2o_modifiers |>
      dplyr::filter(fert_type == .fert, climate == .climate) |>
      dplyr::pull(mf)
  }

  testthat::expect_equal(mf("Synthetic", "MED"), 1.00)
  testthat::expect_equal(mf("Solid", "MED"), 0.38)
  testthat::expect_equal(mf("Liquid", "MED"), 1.70)
  testthat::expect_equal(mf("Excreta_cattle_monog", "MED"), 0.20)
  testthat::expect_equal(mf("Synthetic", "ATL"), 1.60)
  # The MED solid-organic streams are the DOI-verified 0.00, not NA (a missing
  # value would make calculate_soil_n2o(method = "aguilera") return NA).
  testthat::expect_equal(mf("Recycling", "MED"), 0.00)
  testthat::expect_equal(mf("SOM", "MED"), 0.00)
  testthat::expect_equal(mf("Human", "MED"), 0.00)
})

test_that("meisinger_denitrification has the expected columns and size", {
  pointblank::expect_col_exists(
    whep::meisinger_denitrification,
    columns = c(
      "fert_cat",
      "tillage",
      "som_content",
      "climate_cat",
      "drainage_rate",
      "denit_share",
      "climate"
    )
  )
  testthat::expect_equal(nrow(whep::meisinger_denitrification), 108L)
})

test_that("meisinger_denitrification transcribes spot values", {
  # Waterlogged (drainage None) denitrifies the whole surplus.
  none_rows <- whep::meisinger_denitrification |>
    dplyr::filter(drainage_rate == "None") |>
    dplyr::pull(denit_share)
  testthat::expect_true(all(none_rows == 1))

  # Synthetic / Tillage / Low SOM / Semiarid / Very_high drainage = 0.02.
  anchor <- whep::meisinger_denitrification |>
    dplyr::filter(
      fert_cat == "Synthetic",
      tillage == "Tillage",
      som_content == "Low",
      climate_cat == "Semiarid",
      drainage_rate == "Very_high"
    ) |>
    dplyr::pull(denit_share)
  testthat::expect_equal(anchor, 0.02)

  # Manure / Humid / High SOM / Very_low drainage = 0.95.
  manure <- whep::meisinger_denitrification |>
    dplyr::filter(
      fert_cat == "Manure",
      som_content == "High",
      climate_cat == "Humid",
      drainage_rate == "Very_low"
    ) |>
    dplyr::pull(denit_share)
  testthat::expect_equal(manure, 0.95)
})

test_that("drainage_ranges has the expected bins", {
  pointblank::expect_col_exists(
    whep::drainage_ranges,
    columns = c("drainage_rate", "s_min", "s_max")
  )

  very_high <- whep::drainage_ranges |>
    dplyr::filter(drainage_rate == "Very_high")
  testthat::expect_equal(dplyr::pull(very_high, s_min), 1000)
  testthat::expect_equal(dplyr::pull(very_high, s_max), 3000)

  none_bin <- whep::drainage_ranges |>
    dplyr::filter(drainage_rate == "None")
  testthat::expect_equal(dplyr::pull(none_bin, s_min), -0.1)
  testthat::expect_equal(dplyr::pull(none_bin, s_max), 0.1)
})

test_that("subsoil_no3_reduction transcribes the NO3 reduction anchors", {
  pointblank::expect_col_exists(
    whep::subsoil_no3_reduction,
    columns = c("fert_type", "climate", "irrig_cat", "no3_red")
  )

  no3 <- function(.fert, .climate, .irrig) {
    whep::subsoil_no3_reduction |>
      dplyr::filter(
        fert_type == .fert,
        climate == .climate,
        irrig_cat == .irrig
      ) |>
      dplyr::pull(no3_red)
  }

  testthat::expect_equal(no3("Synthetic", "MED", "Rainfed"), 0.20)
  testthat::expect_equal(no3("Synthetic", "ATL", "Irrigated"), 0.60)
  testthat::expect_equal(no3("Excreta_cattle_monog", "ATL", "Irrigated"), 0.75)
  # BNF inherits the Excreta_cattle_monog reduction fractions across all four
  # climate/irrigation bins (the verified source override), not the raw sheet
  # values (0.35/0.55/0.65/0.70).
  testthat::expect_equal(no3("BNF", "MED", "Rainfed"), 0.40)
  testthat::expect_equal(no3("BNF", "MED", "Irrigated"), 0.65)
  testthat::expect_equal(no3("BNF", "ATL", "Rainfed"), 0.70)
  testthat::expect_equal(no3("BNF", "ATL", "Irrigated"), 0.75)
})

test_that("manner_params transcribes the MANNER factor anchors", {
  pointblank::expect_col_exists(
    whep::manner_params,
    columns = c("category", "key", "sub_key", "factor")
  )

  max_nh3 <- function(.fert) {
    whep::manner_params |>
      dplyr::filter(category == "max_nh3", key == .fert) |>
      dplyr::pull(factor)
  }
  testthat::expect_equal(max_nh3("Urea"), 0.45)
  testthat::expect_equal(max_nh3("AN"), 0.04)
  testthat::expect_equal(max_nh3("CAN"), 0.04)
  testthat::expect_equal(max_nh3("AS"), 0.45)

  # AS at acidic pH attenuates strongly.
  as_acid <- whep::manner_params |>
    dplyr::filter(category == "ph", key == "AS", sub_key == "pH<7") |>
    dplyr::pull(factor)
  testthat::expect_equal(as_acid, 0.09)

  # Broadcast technique is the unattenuated reference.
  broadcast <- whep::manner_params |>
    dplyr::filter(category == "technique", key == "Broadcast") |>
    dplyr::pull(factor)
  testthat::expect_equal(broadcast, 1)

  strongwind <- whep::manner_params |>
    dplyr::filter(category == "windspeed", key == "strongwind") |>
    dplyr::pull(factor)
  testthat::expect_equal(strongwind, 1.6)
})

test_that("n_attenuation_constants transcribes A_CN and indirect EFs", {
  pointblank::expect_col_exists(
    whep::n_attenuation_constants,
    columns = c("constant", "value", "description")
  )

  lookup <- whep::n_attenuation_constants |>
    dplyr::select(constant, value) |>
    tibble::deframe()

  testthat::expect_equal(lookup[["a_cn_min_cn"]], 15)
  testthat::expect_equal(lookup[["a_cn_span"]], 120)
  testthat::expect_equal(lookup[["a_cn_span_other"]], 60)
  testthat::expect_equal(lookup[["a_cn_max"]], 0.98)
  testthat::expect_equal(lookup[["ef5_no3_to_n2o"]], 0.011)
  testthat::expect_equal(lookup[["ef4_nh3_to_n2o_atl"]], 0.014)
  testthat::expect_equal(lookup[["nh3_frac_synthetic"]], 0.11)
  testthat::expect_equal(lookup[["nh3_frac_organic"]], 0.21)
  # SWAT ANION_EXCL default (I/O documentation 2012, ch. 22, p. 304) and the
  # IMAGE-GNM Arrhenius constants (Beusen et al. 2015, eq. 13, p. 4052).
  testthat::expect_equal(lookup[["swat_anion_exclusion"]], 0.5)
  testthat::expect_equal(lookup[["beusen_arrhenius_factor"]], 7.94e12)
  testthat::expect_equal(lookup[["beusen_activation_energy"]], 74830)
  testthat::expect_equal(lookup[["molar_gas_constant"]], 8.3144)
  # IMAGE-GNM root-zone rules: the 1 m depth of eq. 14 (p. 4052) and the
  # authors' code bounds (n_soil_denitrification.py lines 97, 179-182,
  # 377-378).
  testthat::expect_equal(lookup[["image_gnm_tawc_depth_m"]], 1)
  testthat::expect_equal(lookup[["image_gnm_crop_max_residence_yr"]], 1)
  testthat::expect_equal(lookup[["image_gnm_no_drainage_residence_yr"]], 100)
  testthat::expect_equal(lookup[["image_gnm_max_temperature_effect"]], 1)
  # The authors' minimal_soil_infiltration (parameters_history_100.ini line
  # 204, mm/yr), below which the excess water of eq. 5 is 0
  # (n_soil_denitrification.py lines 341-344).
  testthat::expect_equal(lookup[["image_gnm_min_runoff_mm"]], 3)
  # One tolerance below 0 for a water flux, shared by the Meisinger drainage
  # check and the IMAGE-GNM runoff check; it must stay the lower edge of
  # Meisinger's None drainage bin, or a drainage could pass the check and
  # match no bin.
  testthat::expect_equal(lookup[["water_flux_floor_mm"]], -0.1)
  testthat::expect_identical(
    lookup[["water_flux_floor_mm"]],
    min(whep::drainage_ranges$s_min)
  )
  # IMAGE-GNM surface runoff: f_cal of eq. 9 (p. 4052; C_sro,N of Table 3,
  # p. 4056) and the erosion slope factor of the authors' code
  # (n_erosion.py line 50).
  testthat::expect_equal(lookup[["image_gnm_runoff_calibration"]], 0.3)
  testthat::expect_equal(lookup[["image_gnm_erosion_slope_offset"]], -1.5)
  testthat::expect_equal(lookup[["image_gnm_erosion_slope_range"]], 17)
  testthat::expect_equal(lookup[["image_gnm_erosion_slope_intercept"]], 2.3)
  testthat::expect_equal(lookup[["image_gnm_erosion_slope_rate"]], 6.1)
  # SOM per unit SOC: 2 (SOM 50% C, Pribyl 2010) by default and the
  # conventional 1.724 (58% C) selectable; the HWSD topsoil depth option of
  # the root zone.
  testthat::expect_equal(lookup[["som_soc_factor_pribyl"]], 2)
  testthat::expect_equal(lookup[["som_soc_factor_conventional"]], 1.724)
  testthat::expect_equal(lookup[["root_depth_topsoil_cm"]], 30)
})

test_that("image_gnm_runoff_factors transcribes the authors' runoff inputs", {
  x <- whep::image_gnm_runoff_factors
  pointblank::expect_col_exists(
    x,
    columns = c("factor", "class", "value", "unit", "source")
  )
  # Land-use factors of Table 3 (p. 4056), the authors' texture_erosion.vat,
  # their soil-loss factors (parameters_history_100.ini lines 112-114) and
  # the erosion C:N ratios (p. 4052).
  expected <- tibble::tribble(
    ~factor,           ~class,      ~value,
    "runoff_land_use", "Cropland",  1.0,
    "runoff_land_use", "Grassland", 0.25,
    "runoff_land_use", "Natural",   0.125,
    "erosion_texture", "coarse",    0.5,
    "erosion_texture", "medium",    1.5,
    "erosion_texture", "fine",      1.5,
    "erosion_texture", "very_fine", 0.5,
    "erosion_texture", "organic",   0.5,
    "soil_loss",       "Cropland",  1151,
    "soil_loss",       "Grassland", 99,
    "soil_loss",       "Natural",   29,
    "erosion_cn",      "Cropland",  12,
    "erosion_cn",      "Grassland", 14,
    "erosion_cn",      "Natural",   14
  )
  testthat::expect_equal(dplyr::select(x, "factor", "class", "value"), expected)
  testthat::expect_false(vctrs::vec_duplicate_any(x[c("factor", "class")]))
  pointblank::expect_col_vals_not_null(x, c("unit", "source"))
  testthat::expect_true(all(
    stringr::str_detect(
      x$source[x$factor == "runoff_land_use"],
      "Table 3, p\\. 4056"
    )
  ))
  testthat::expect_true(all(
    stringr::str_detect(
      x$source[x$factor == "erosion_texture"],
      "texture_erosion\\.vat"
    )
  ))
  testthat::expect_true(all(
    stringr::str_detect(
      x$source[x$factor == "soil_loss"],
      "parameters_history_100\\.ini"
    )
  ))
  # Every texture class Table 2 reads has an erosion factor, and every land
  # use of eq. 11 a runoff, soil loss and C:N factor.
  gnm <- whep::image_gnm_leaching_factors
  testthat::expect_setequal(
    x$class[x$factor == "erosion_texture"],
    gnm$class[gnm$factor == "texture"]
  )
  for (f in c("runoff_land_use", "soil_loss", "erosion_cn")) {
    testthat::expect_setequal(
      x$class[x$factor == f],
      gnm$class[gnm$factor == "land_use"]
    )
  }
})

test_that("image_gnm_leaching_factors transcribes Beusen et al. Table 2", {
  x <- whep::image_gnm_leaching_factors
  pointblank::expect_col_exists(
    x,
    columns = c(
      "factor",
      "class",
      "label",
      "soc_min_pct",
      "soc_max_pct",
      "value",
      "source"
    )
  )
  # Table 2, p. 4053, as printed; the two well-drained FAO classes the table
  # does not name take 0 from the authors' code; f_landuse from p. 4052.
  expected <- tibble::tribble(
    ~factor,    ~class,                         ~value,
    "texture",  "coarse",                       0.0,
    "texture",  "medium",                       0.1,
    "texture",  "fine",                         0.2,
    "texture",  "very_fine",                    0.3,
    "texture",  "organic",                      0.0,
    "drainage", "excessively_drained",          0.0,
    "drainage", "moderately_well_drained",      0.1,
    "drainage", "imperfectly_drained",          0.2,
    "drainage", "poorly_drained",               0.3,
    "drainage", "very_poorly_drained",          0.4,
    "drainage", "well_drained",                 0.0,
    "drainage", "somewhat_excessively_drained", 0.0,
    "soc",      "below_1_pct",                  0.0,
    "soc",      "1_to_3_pct",                   0.1,
    "soc",      "3_to_6_pct",                   0.2,
    "soc",      "6_to_50_pct",                  0.3,
    "soc",      "organic",                      0.3,
    "land_use", "Cropland",                     1.0,
    "land_use", "Grassland",                    0.36,
    "land_use", "Natural",                      0.36
  )
  testthat::expect_equal(dplyr::select(x, "factor", "class", "value"), expected)
  soc <- dplyr::filter(x, .data$factor == "soc")
  testthat::expect_equal(soc$soc_min_pct, c(0, 1, 3, 6, 50))
  testthat::expect_equal(soc$soc_max_pct, c(1, 3, 6, 50, Inf))
  testthat::expect_true(all(is.na(x$soc_min_pct[x$factor != "soc"])))
  testthat::expect_identical(
    x$label[x$factor %in% c("texture", "drainage", "soc")],
    c(
      "Coarse",
      "Medium",
      "Fine",
      "Very fine",
      "Organic",
      "Excessively well drained",
      "Moderate well drained",
      "Imperfectly drained",
      "Poorly drained",
      "Very poorly drained",
      NA,
      NA,
      "<1%",
      "1-3%",
      "3-6%",
      "6-50%",
      "Organic"
    )
  )
  from_code <- x$class %in% c("well_drained", "somewhat_excessively_drained")
  testthat::expect_true(all(
    stringr::str_detect(x$source[from_code], "n_soil_denitrification\\.py")
  ))
  testthat::expect_true(all(
    stringr::str_detect(
      x$source[x$factor != "land_use" & !from_code],
      "Table 2, p\\. 4053"
    )
  ))
  testthat::expect_true(all(
    stringr::str_detect(x$source[x$factor == "land_use"], "p\\. 4052")
  ))
  testthat::expect_false(vctrs::vec_duplicate_any(x[c("factor", "class")]))
})

test_that("ipcc_2019_soil_n_ef transcribes every Tier 1 factor", {
  pointblank::expect_col_exists(
    whep::ipcc_2019_soil_n_ef,
    columns = c("parameter", "disaggregation", "value", "unit", "source")
  )
  testthat::expect_equal(nrow(whep::ipcc_2019_soil_n_ef), 18L)
  pointblank::expect_col_vals_not_null(whep::ipcc_2019_soil_n_ef, "value")

  expected <- tibble::tribble(
    ~parameter, ~disaggregation, ~expected,
    "EF1", "aggregated", 0.010,
    "EF1", "synthetic_wet", 0.016,
    "EF1", "other_wet", 0.006,
    "EF1", "all_dry", 0.005,
    "EF1FR", "aggregated", 0.004,
    "EF1FR", "continuous_flooding", 0.003,
    "EF1FR", "single_multiple_drainage", 0.005,
    "EF3PRP_CPP", "aggregated", 0.004,
    "EF3PRP_CPP", "wet", 0.006,
    "EF3PRP_CPP", "dry", 0.002,
    "EF3PRP_SO", "aggregated", 0.003,
    "EF4", "aggregated", 0.010,
    "EF4", "wet", 0.014,
    "EF4", "dry", 0.005,
    "EF5", "aggregated", 0.011,
    "FracLEACH_H", "wet", 0.24,
    "FracLEACH_H", "dry", 0,
    "FracLEACH_H", "dry_irrigated_non_drip", 0.24
  )

  joined <- expected |>
    dplyr::inner_join(
      whep::ipcc_2019_soil_n_ef,
      by = c("parameter", "disaggregation")
    )
  testthat::expect_equal(nrow(joined), 18L)
  testthat::expect_equal(joined$value, joined$expected)

  x <- whep::ipcc_2019_soil_n_ef
  testthat::expect_equal(
    x$unit,
    dplyr::case_when(
      x$parameter == "FracLEACH_H" ~ "kg N per kg N",
      TRUE ~ "kg N2O-N per kg N"
    )
  )
})

test_that("ipcc_2019_soil_n_ef agrees with indirect_n2o_ef", {
  indirect <- whep::indirect_n2o_ef |>
    dplyr::filter(edition == "ipcc_2019")

  ef4 <- whep::ipcc_2019_soil_n_ef |>
    dplyr::filter(parameter == "EF4", disaggregation == "aggregated") |>
    dplyr::pull(value)
  testthat::expect_equal(
    ef4,
    indirect |>
      dplyr::filter(parameter == "ef4_volatilization") |>
      dplyr::pull(value)
  )

  ef5 <- whep::ipcc_2019_soil_n_ef |>
    dplyr::filter(parameter == "EF5", disaggregation == "aggregated") |>
    dplyr::pull(value)
  testthat::expect_equal(
    ef5,
    indirect |>
      dplyr::filter(parameter == "ef5_leaching") |>
      dplyr::pull(value)
  )

  frac_leach <- whep::ipcc_2019_soil_n_ef |>
    dplyr::filter(parameter == "FracLEACH_H", disaggregation == "wet") |>
    dplyr::pull(value)
  testthat::expect_equal(
    frac_leach,
    indirect |>
      dplyr::filter(parameter == "frac_leach") |>
      dplyr::pull(value)
  )
})

# Module C (Task C3) human-population nitrogen benchmark series.

testthat::test_that("human_n_reference keys its area by FAOSTAT code", {
  # whep#401: the vendored CSV labels the series with the ISO3 string "ESP",
  # but the column is named area_code, which everywhere else in this package
  # means the numeric FAOSTAT code. The other half of this same derivation,
  # data-raw/build_human_kgn_cap.R, keeps the series' country by filtering
  # on area code 203L, so while the built dataset held "ESP" the two steps keyed one
  # concept two ways and this benchmark could not be joined to any area-keyed
  # table without an undocumented hand conversion. The build now resolves the
  # label through polity_area_crosswalk. This test fails on the string form:
  # is.integer("ESP") is FALSE.
  x <- whep::human_n_reference

  pointblank::expect_col_exists(x, c("area_code", "year", "human_n_gg"))
  testthat::expect_true(is.integer(x$area_code))
  testthat::expect_setequal(x$area_code, 203L)

  # Resolved, not hard-coded: the code has to be the one the crosswalk gives
  # for ESP and it has to reach a polity, otherwise the fix would only have
  # swapped one unjoinable value for another.
  spain <- whep::polity_area_crosswalk |>
    dplyr::filter(area_iso3c == "ESP")
  testthat::expect_setequal(unique(x$area_code), unique(spain$area_code))
  testthat::expect_true(all(!is.na(spain$polity_code)))

  # The measured series is untouched by the identifier fix: the same ten
  # benchmark years and the same Gg N as the vendored CSV.
  testthat::expect_equal(nrow(x), 10L)
  testthat::expect_equal(min(x$year), 1860)
  testthat::expect_equal(max(x$year), 2022)
  testthat::expect_equal(x$human_n_gg[x$year == 1860], 6.97)
  testthat::expect_equal(x$human_n_gg[x$year == 2022], 61.29)
})

testthat::test_that("human_n_reference names its territory by polity code", {
  # whep#495 (epic whep#458): resolving the ISO3 label to 203 made the series
  # joinable, and 203 is still a FABIO aggregation bucket rather than the
  # identity of a territory. Every place in WHEP that identifies a territory has
  # to carry a polity code string, so this 10-row coefficient table names Spain
  # as ESP-1800-2025 and not only as 203. This test fails on the pre-#495 build:
  # the dataset had exactly three columns, none of them a polity code.
  x <- whep::human_n_reference

  pointblank::expect_col_exists(x, "polity_code")
  testthat::expect_true(is.character(x$polity_code))
  testthat::expect_true(all(!is.na(x$polity_code)))
  # Shape, not just presence: a bare "ESP" or a stringified 203 would satisfy
  # "a character column called polity_code" and would not be a polity code. The
  # prefix runs 3 to 5 characters across whep::polities (ESP, AUSA, RSFSR).
  testthat::expect_true(all(grepl(
    "^[A-Z0-9]{3,5}-[0-9]{4}-[0-9]{4}$",
    x$polity_code
  )))

  # ATTACHED, not substituted (whep#424's choice for the 25 area-keyed
  # exports): the numeric key external consumers may already join on survives.
  pointblank::expect_col_exists(x, c("area_code", "year", "human_n_gg"))
  testthat::expect_true(is.integer(x$area_code))

  # Re-derive the expected code independently of the build script: for each
  # benchmark year, the ESP polity whose active span covers that year. If the
  # build had written a literal or matched on the wrong year, the two would
  # disagree. Spain happens to be one continuous polity over 1860-2022, so this
  # currently expects one distinct code -- the check is written per row anyway,
  # because the point of the column is that it can differ by year.
  #
  # `polity_end_year` is EXCLUSIVE at a succession and inclusive only at an open
  # end (#577), so the containment test is `>`, widened by one year for a polity
  # nothing succeeds. This re-derivation used to read the bound inclusively; it
  # was the fifth site to do so and was inert only because ESP is a single
  # interval (#565). It is still spelled out here rather than delegated to
  # `whep:::.iso3_year_to_polity_code()`, so it stays independent of the code the
  # builder used.
  expected <- whep::polity_area_crosswalk |>
    dplyr::filter(area_iso3c == "ESP", !is.na(polity_code)) |>
    dplyr::distinct(polity_code, polity_start_year, polity_end_year)
  # DA-24's convention: `polity_end_year` is EXCLUSIVE at a succession and
  # INCLUSIVE at the open end. Read through the package's own
  # `.open_polity_codes()` rather than re-derived here, so the test cannot
  # drift from the rule the resolvers apply.
  covered_to <- expected$polity_end_year +
    (expected$polity_code %in% whep:::.open_polity_codes())
  resolved <- vapply(
    x$year,
    function(yr) {
      hit <- expected$polity_code[
        expected$polity_start_year <= yr & covered_to > yr
      ]
      if (length(hit) == 1L) hit else NA_character_
    },
    character(1)
  )
  testthat::expect_equal(x$polity_code, resolved)

  # And it has to be a polity that exists, not a well-formed string: a code
  # absent from whep::polities is unusable by anything downstream.
  testthat::expect_true(all(x$polity_code %in% whep::polities$polity_code))

  # The measured series is untouched by the added identifier: same ten
  # benchmark years, same Gg N.
  testthat::expect_equal(nrow(x), 10L)
  testthat::expect_equal(x$human_n_gg[x$year == 1860], 6.97)
  testthat::expect_equal(x$human_n_gg[x$year == 2022], 61.29)
})

testthat::test_that("the former dataset names still load, unchanged", {
  # Kept for one release under their former names and column names, so a
  # caller reading them sees exactly the rows it saw before the rename.
  testthat::expect_identical(
    whep::urban_n_reference,
    dplyr::rename(whep::human_n_reference, urban_n_gg = "human_n_gg")
  )
  testthat::expect_equal(
    whep::urban_kgn_cap_reference,
    dplyr::rename(
      whep::human_kgn_cap_reference,
      urban_kgn_cap = "human_kgn_cap"
    ),
    ignore_attr = TRUE
  )
})

# ---- the extdata CSVs must be well-formed CSV ---------------------------

testthat::test_that("every balances CSV parses to a rectangle", {
  # soc_turnover_params.csv shipped one description containing an unquoted
  # comma. utils::read.csv() split it into a 7th field on a 6-column header,
  # returning 48 rows with the description truncated at "dimensionless",
  # while readr - which data-raw/ uses - returned 47 rows with it intact. The
  # .rda was therefore right and the CSV it is built from was not, and the
  # freshness gate could not see it because both sides ran the same reader.
  dir <- system.file("extdata", "balances", package = "whep")
  testthat::skip_if(dir == "")
  for (f in list.files(dir, pattern = "[.]csv$", full.names = TRUE)) {
    # Cross-READER, not self: two read.csv() calls on one file derive their
    # column count from the same header and the same first lines, so they
    # agree for every possible input including a corrupt one. Only comparing
    # the reader that broke against the reader data-raw/ uses can see an
    # unquoted comma, which spills a surplus field onto a NEW ROW under
    # read.csv()'s fill = TRUE while readr keeps the record intact.
    testthat::skip_if_not_installed("readr")
    full <- utils::read.csv(f, check.names = FALSE)
    tidy <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
    testthat::expect_equal(
      ncol(full),
      ncol(tidy),
      label = paste(basename(f), "column count agrees between readers")
    )
    testthat::expect_equal(
      nrow(full),
      nrow(tidy),
      label = paste(basename(f), "row count agrees between readers")
    )
    # A ragged row shows up as an all-NA trailing column or as a row whose
    # last field is NA where the file has text.
    testthat::expect_false(
      any(vapply(full, \(col) all(is.na(col)), logical(1))),
      label = paste(basename(f), "has no all-NA column")
    )
  }
})

testthat::test_that("soc_turnover_params reads identically through two readers", {
  f <- system.file(
    "extdata",
    "balances",
    "soc_turnover_params.csv",
    package = "whep"
  )
  testthat::skip_if(f == "")
  testthat::skip_if_not_installed("readr")

  base_r <- utils::read.csv(f, stringsAsFactors = FALSE)
  tidy <- as.data.frame(readr::read_csv(f, show_col_types = FALSE))

  testthat::expect_equal(nrow(base_r), nrow(tidy))
  testthat::expect_equal(names(base_r), names(tidy))
  testthat::expect_equal(base_r$description, tidy$description)
  # And the shipped .rda must match both, not just one of them.
  testthat::expect_equal(nrow(whep::soc_turnover_params), nrow(base_r))
})

testthat::test_that("human_kgn_cap_total_reference is rebased on its total", {
  # The per-TOTAL-inhabitant rate divides the same calibration nitrogen as
  # human_kgn_cap_reference by the calibration WPP total population, which
  # the table carries so the rebasing is checkable here without the WPP file.
  total <- whep::human_kgn_cap_total_reference
  pointblank::expect_col_exists(
    total,
    c("year", "human_kgn_cap", "calibration_population")
  )
  joined <- dplyr::inner_join(
    total,
    whep::human_n_reference,
    by = "year"
  )
  # Every human_n_reference benchmark WPP covers (1950 on) has a row, and no
  # row lacks its numerator.
  testthat::expect_equal(nrow(joined), nrow(total))
  testthat::expect_setequal(
    total$year,
    whep::human_n_reference$year[whep::human_n_reference$year >= 1950]
  )
  testthat::expect_equal(
    joined$human_kgn_cap * joined$calibration_population,
    joined$human_n_gg * 1e6,
    tolerance = 1e-12
  )
  # A total population is larger than its urban part, so per total inhabitant
  # is the smaller rate in every year both series cover.
  both <- dplyr::inner_join(
    total,
    whep::human_kgn_cap_reference,
    by = "year",
    suffix = c("_total", "_urban")
  )
  testthat::expect_equal(nrow(both), nrow(total))
  testthat::expect_true(all(
    both$human_kgn_cap_total < both$human_kgn_cap_urban
  ))
})

testthat::test_that("the renamed Human level keeps the former Urban coefficients", {
  # The fert_type key was renamed from "Urban" to "Human" in both tables the
  # loss cascade looks up. A key the cascade emits with no row would be an NA
  # modifier (aborted) or an NA reduction (aborted), so pin both that the
  # values are the ones "Urban" carried and that nothing still says "Urban".
  mf <- whep::fertiliser_n2o_modifiers
  testthat::expect_false("Urban" %in% mf$fert_type)
  human_mf <- mf[mf$fert_type == "Human", ]
  testthat::expect_equal(
    human_mf$mf[match(c("MED", "ATL"), human_mf$climate)],
    c(0.00, 0.60)
  )
  red <- whep::subsoil_no3_reduction
  testthat::expect_false("Urban" %in% red$fert_type)
  human_red <- red[red$fert_type == "Human", ]
  key <- paste(human_red$climate, human_red$irrig_cat)
  testthat::expect_equal(
    human_red$no3_red[match(
      c("MED Rainfed", "MED Irrigated", "ATL Rainfed", "ATL Irrigated"),
      key
    )],
    c(0.40, 0.65, 0.70, 0.75)
  )
  # Every fert_type build_nitrogen_balance() sends to the direct-N2O lookup
  # resolves to a modifier in both climates. "Excreta" carries its species
  # as a share and is priced as the blend of the two species rows, so those
  # are the rows it needs.
  sent <- whep:::.nb_loss_fert_type(c(
    "excreta",
    "manure_liquid",
    "manure_solid",
    "som_mineralization",
    "synthetic",
    "human",
    "recycling"
  ))
  testthat::expect_true("Excreta" %in% sent)
  looked_up <- c(
    setdiff(sent, "Excreta"),
    "Excreta_cattle_monog",
    "Excreta_other"
  )
  grid <- tidyr::crossing(fert_type = looked_up, climate = c("MED", "ATL"))
  joined <- dplyr::left_join(grid, mf, by = c("fert_type", "climate"))
  testthat::expect_false(anyNA(joined$mf))
  blend <- whep:::.soil_n2o_ef_mf_aguilera(tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type, ~excreta_cpp_share,
    1, "Excreta", "MED", "Rainfed", 0.5,
    1, "Excreta", "ATL", "Tier_1", 0.5
  ))
  testthat::expect_false(anyNA(blend))
})

test_that("fao56_root_depth has unique, known production items", {
  x <- whep::fao56_root_depth
  pointblank::expect_col_exists(
    x,
    columns = c(
      "item_prod_code",
      "fao56_crop",
      "depth_min_m",
      "depth_max_m",
      "method_root_depth",
      "rationale",
      "source"
    )
  )
  testthat::expect_equal(nrow(x), 305L)
  testthat::expect_false(anyDuplicated(x$item_prod_code) > 0L)
  # items_prod_full omits the aggregate fibre-equivalent item 17530, so the
  # complete production-item list the table covers is items_prod.
  testthat::expect_setequal(x$item_prod_code, whep::items_prod$item_prod_code)
  testthat::expect_true(
    all(
      as.character(setdiff(x$item_prod_code, 17530)) %in%
        whep::items_prod_full$item_prod_code
    )
  )
})

test_that("fao56_root_depth ranges are ordered and typed by method", {
  x <- whep::fao56_root_depth
  pointblank::expect_col_vals_in_set(
    x,
    columns = "method_root_depth",
    set = c("direct", "direct_multi", "proxy", "none")
  )
  testthat::expect_equal(
    x |>
      dplyr::count(method_root_depth) |>
      dplyr::arrange(method_root_depth) |>
      dplyr::pull(n),
    c(94L, 18L, 117L, 76L)
  )
  with_depth <- dplyr::filter(x, method_root_depth != "none")
  testthat::expect_false(anyNA(with_depth$depth_min_m))
  testthat::expect_false(anyNA(with_depth$depth_max_m))
  testthat::expect_true(
    all(with_depth$depth_min_m <= with_depth$depth_max_m)
  )
  none <- dplyr::filter(x, method_root_depth == "none")
  testthat::expect_true(all(is.na(none$depth_min_m)))
  testthat::expect_true(all(is.na(none$depth_max_m)))
})

test_that("fao56_root_depth proxy rows state their rationale", {
  proxy <- whep::fao56_root_depth |>
    dplyr::filter(method_root_depth == "proxy")
  testthat::expect_gt(nrow(proxy), 0L)
  testthat::expect_false(anyNA(proxy$rationale))
  testthat::expect_true(all(nzchar(trimws(proxy$rationale))))
})

test_that("fao56_root_depth transcribes the FAO-56 Table 22 ranges", {
  expected <- tibble::tribble(
    ~item_prod_code, ~depth_min_m, ~depth_max_m,
    44L, 1.0, 1.5,
    116L, 0.4, 0.6,
    27L, 0.5, 1.0,
    56L, 1.0, 1.7,
    15L, 1.0, 1.8
  )
  joined <- expected |>
    dplyr::inner_join(
      whep::fao56_root_depth,
      by = "item_prod_code",
      suffix = c("_expected", "")
    )
  testthat::expect_equal(nrow(joined), 5L)
  testthat::expect_equal(joined$depth_min_m, joined$depth_min_m_expected)
  testthat::expect_equal(joined$depth_max_m, joined$depth_max_m_expected)
})
