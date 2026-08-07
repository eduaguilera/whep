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
    c("hsoc", "rothc", "icbm", "amg", "century")
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

test_that("residue_humification transcribes the Spain_Hist anchors", {
  lookup <- whep::residue_humification |>
    dplyr::select(input_type, humified_fraction) |>
    tibble::deframe()

  # Weed/grass aboveground = Spain_Hist "Grass" Residue_humified_kgC_kgC.
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
  testthat::expect_equal(mf("Urban", "MED"), 0.00)
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
  testthat::expect_equal(lookup[["ef4_nh3_to_n2o_atl"]], 0.016)
  testthat::expect_equal(lookup[["nh3_frac_synthetic"]], 0.11)
  testthat::expect_equal(lookup[["nh3_frac_organic"]], 0.21)
})

# Module C (Task C3) urban nitrogen benchmark series.

testthat::test_that("urban_n_reference keys its area by FAOSTAT code", {
  # whep#401: the vendored CSV labels the series with the ISO3 string "ESP",
  # but the column is named area_code, which everywhere else in this package
  # means the numeric FAOSTAT code. The other half of this same derivation,
  # data-raw/build_urban_kgn_cap.R, keeps Spain by filtering on area code
  # 203L, so while the built dataset held "ESP" the two steps keyed one
  # concept two ways and this benchmark could not be joined to any area-keyed
  # table without an undocumented hand conversion. The build now resolves the
  # label through polity_area_crosswalk. This test fails on the string form:
  # is.integer("ESP") is FALSE.
  x <- whep::urban_n_reference

  pointblank::expect_col_exists(x, c("area_code", "year", "urban_n_gg"))
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
  testthat::expect_equal(x$urban_n_gg[x$year == 1860], 6.97)
  testthat::expect_equal(x$urban_n_gg[x$year == 2022], 61.29)
})

testthat::test_that("urban_n_reference names its territory by polity code", {
  # whep#495 (epic whep#458): resolving the ISO3 label to 203 made the series
  # joinable, and 203 is still a FABIO aggregation bucket rather than the
  # identity of a territory. Every place in WHEP that identifies a territory has
  # to carry a polity code string, so this 10-row coefficient table names Spain
  # as ESP-1800-2025 and not only as 203. This test fails on the pre-#495 build:
  # the dataset had exactly three columns, none of them a polity code.
  x <- whep::urban_n_reference

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
  pointblank::expect_col_exists(x, c("area_code", "year", "urban_n_gg"))
  testthat::expect_true(is.integer(x$area_code))

  # Re-derive the expected code independently of the build script: for each
  # benchmark year, the ESP polity whose active span covers that year. If the
  # build had written a literal or matched on the wrong year, the two would
  # disagree. Spain happens to be one continuous polity over 1860-2022, so this
  # currently expects one distinct code -- the check is written per row anyway,
  # because the point of the column is that it can differ by year.
  expected <- whep::polity_area_crosswalk |>
    dplyr::filter(area_iso3c == "ESP", !is.na(polity_code)) |>
    dplyr::distinct(polity_code, polity_start_year, polity_end_year)
  # DA-24's convention, re-derived here rather than imported: `polity_end_year`
  # is EXCLUSIVE at a succession and INCLUSIVE at the open end, so an interval
  # ending where the crosswalk's coverage ends, with no later-starting interval
  # of the same area after it, covers its own terminal year.
  open_ended <- expected$polity_end_year == max(expected$polity_end_year) &
    expected$polity_start_year >= max(expected$polity_start_year)
  resolved <- vapply(
    x$year,
    function(yr) {
      hit <- expected$polity_code[
        expected$polity_start_year <= yr &
          (yr < expected$polity_end_year |
            (open_ended & yr == expected$polity_end_year))
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
  testthat::expect_equal(x$urban_n_gg[x$year == 1860], 6.97)
  testthat::expect_equal(x$urban_n_gg[x$year == 2022], 61.29)
})
