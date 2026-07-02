# Helper fixtures --------------------------------------------------------------

# BNF: minimal calculate_bnf() input (one crop, one cell-year).
.nbi_bnf_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~item_prod_code,
    ~crop_npp_n_t,
    ~product_n_t,
    ~weed_npp_n_t,
    ~land_use,
    ~legumes_seeded,
    ~seeded_cover_crop_share,
    ~area_ha,
    0.25,
    50.25,
    10L,
    2010L,
    "176",
    10,
    5,
    4,
    "Cropland",
    0,
    0,
    40
  )
}

# NPP N: minimal calculate_npp_carbon_nitrogen() input (one crop, one
# cell-year), item_prod_code 15 (wheat) matches whep::items_prod_full so
# calculate_bnf()/npp joins resolve.
.nbi_npp_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~item_prod_code,
    ~item_cbs_code,
    ~product_dm_t,
    ~residue_dm_t,
    ~root_dm_t,
    0.25,
    50.25,
    10L,
    2010L,
    "15",
    2511L,
    87.9,
    135.75,
    30,
  )
}

# Livestock intake: the redistribute_feed() realised-intake contract expected
# by build_livestock_nutrient_flows(); territory is area_code cast to
# character (the redistribute_feed() convention), sub_territory a "lon_lat"
# cell id.
.nbi_livestock_intake <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~item_cbs_code,
    ~feed_quality,
    ~intake_dm_t,
    2010L,
    "10",
    "0.25_50.25",
    "Cattle_milk",
    2513L,
    "high_quality",
    200,
    2010L,
    "10",
    "0.25_50.25",
    "Cattle_milk",
    NA,
    "grass",
    600
  )
}

.nbi_gridded <- function() {
  list(
    crops = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~crop,
      ~manure_n_receptivity,
      ~crop_n_cap,
      2010L,
      "10",
      "0.25_50.25",
      "barley",
      6,
      200
    ),
    grass = tibble::tribble(
      ~year,
      ~territory,
      ~sub_territory,
      ~grass_n_cap,
      2010L,
      "10",
      "0.25_50.25",
      50
    )
  )
}

.nbi_nhx <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~value_g,
    0.25,
    50.25,
    2010L,
    2000000000
  )
}

.nbi_noy <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~value_g,
    0.25,
    50.25,
    2010L,
    1000000000
  )
}

.nbi_urban_population <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~urban_pop,
    0.25,
    50.25,
    2010L,
    30898536
  )
}

.nbi_cropland_ha <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~cropland_ha,
    0.25,
    50.25,
    10L,
    2010L,
    1000
  )
}

.nbi_cell_polity <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~polity_frac,
    ~cell_area_ha,
    0.25,
    50.25,
    10L,
    1,
    3000
  )
}

.nbi_carbon_balance <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~land_use,
    ~year,
    ~area_ha,
    ~son_change_kgn_ha,
    0.25,
    50.25,
    10L,
    "Cropland",
    2010L,
    50,
    12, # mineralization: positive input
    0.25,
    50.25,
    10L,
    "Grassland",
    2010L,
    50,
    -5 # immobilization: must be clamped out
  )
}

.nbi_primary_prod <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~unit,
    ~value,
    2010L,
    10L,
    2511L,
    "ha",
    700,
    2010L,
    10L,
    2807L,
    "ha",
    300
  )
}

.nbi_fertilizer <- function() {
  tibble::tribble(
    ~Year,
    ~`Area Code`,
    ~Element,
    ~Item,
    ~Value,
    2010L,
    10L,
    "Agricultural Use",
    "Nutrient nitrogen N (total)",
    100
  )
}

.nbi_crop_patterns <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~item_prod_code,
    ~harvest_fraction,
    0.25,
    50.25,
    15L,
    0.6, # wheat
    0.25,
    50.25,
    27L,
    0.2 # rice
  )
}

.nbi_type_cropland <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~year,
    ~luh2_type,
    ~type_ha,
    0.25,
    50.25,
    2010L,
    "c3ann",
    1000
  )
}

.nbi_full_data <- function() {
  list(
    bnf_input = .nbi_bnf_input(),
    npp_n_input = .nbi_npp_input(),
    livestock_intake = .nbi_livestock_intake(),
    gridded = .nbi_gridded(),
    nhx = .nbi_nhx(),
    noy = .nbi_noy(),
    urban_population = .nbi_urban_population(),
    cropland_ha = .nbi_cropland_ha(),
    cell_polity = .nbi_cell_polity(),
    carbon_balance = .nbi_carbon_balance(),
    primary_prod = .nbi_primary_prod(),
    fertilizer = .nbi_fertilizer(),
    crop_patterns = .nbi_crop_patterns(),
    type_cropland = .nbi_type_cropland()
  )
}

# Tests ------------------------------------------------------------------------

testthat::test_that("all seven implemented fert_type values are present", {
  out <- whep::build_n_inputs(data = .nbi_full_data())

  expected <- c(
    "bnf",
    "recycling",
    "manure_solid",
    "manure_liquid",
    "excreta",
    "deposition",
    "urban",
    "som_mineralization",
    "synthetic"
  )
  testthat::expect_true(all(expected %in% out$fert_type))
})

testthat::test_that("schema is complete at grid resolution", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "item_cbs_code",
      "year",
      "fert_type",
      "n_input_t"
    )
  )
})

testthat::test_that("synthetic gridded output re-aggregates to the polity total", {
  out <- whep::build_n_inputs(
    data = .nbi_full_data(),
    resolution = "grid"
  )
  synthetic <- out[out$fert_type == "synthetic", ]
  testthat::expect_true(nrow(synthetic) > 0)
  testthat::expect_equal(sum(synthetic$n_input_t), 100, tolerance = 1e-6)
})

testthat::test_that("SOM term clamps negative son_change_kgn_ha out", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  som <- out[out$fert_type == "som_mineralization", ]
  testthat::expect_true(nrow(som) > 0)
  testthat::expect_true(all(som$n_input_t > 0))
  # The Grassland row (rate -5, immobilization) must not appear at all.
  expected_t <- 12 * 50 * 1 / 1000
  testthat::expect_equal(sum(som$n_input_t), expected_t, tolerance = 1e-6)
})

testthat::test_that("SOM term uses the NA_integer_ non-crop-specific sentinel", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  som <- out[out$fert_type == "som_mineralization", ]
  testthat::expect_true(all(is.na(som$item_cbs_code)))
})

testthat::test_that("deposition and urban use the NA_integer_ sentinel", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  dep <- out[out$fert_type == "deposition", ]
  urb <- out[out$fert_type == "urban", ]
  testthat::expect_true(nrow(dep) > 0 && all(is.na(dep$item_cbs_code)))
  testthat::expect_true(nrow(urb) > 0 && all(is.na(urb$item_cbs_code)))
})

testthat::test_that("polity resolution is the cell-summed aggregate of grid", {
  grid <- whep::build_n_inputs(data = .nbi_full_data(), resolution = "grid")
  polity <- whep::build_n_inputs(data = .nbi_full_data(), resolution = "polity")

  expected <- grid |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t),
      .by = c("area_code", "item_cbs_code", "year", "fert_type")
    ) |>
    dplyr::arrange(
      .data$area_code,
      .data$item_cbs_code,
      .data$year,
      .data$fert_type
    )
  got <- polity |>
    dplyr::arrange(
      .data$area_code,
      .data$item_cbs_code,
      .data$year,
      .data$fert_type
    )

  testthat::expect_equal(
    sum(got$n_input_t),
    sum(expected$n_input_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(nrow(got), nrow(expected))
})

testthat::test_that("manure_type maps to manure_solid/manure_liquid/excreta", {
  out <- whep::build_n_inputs(data = .nbi_full_data())
  manure <- out[
    out$fert_type %in% c("manure_solid", "manure_liquid", "excreta"),
  ]
  testthat::expect_true(nrow(manure) > 0)
  # area_code resolved from the manure engine's territory string.
  testthat::expect_true(all(manure$area_code == 10L))
})

testthat::test_that("years filters the assembled output", {
  data <- .nbi_full_data()
  out <- whep::build_n_inputs(years = 2010L, data = data)
  testthat::expect_true(all(out$year == 2010L))
})

testthat::test_that("example fixture is schema-complete", {
  out <- whep::build_n_inputs(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "area_code",
      "item_cbs_code",
      "year",
      "fert_type",
      "n_input_t"
    )
  )
  expected_present <- c(
    "bnf",
    "synthetic",
    "deposition",
    "urban",
    "som_mineralization"
  )
  testthat::expect_true(all(expected_present %in% out$fert_type))
})

testthat::test_that("resolution argument is validated", {
  testthat::expect_error(
    whep::build_n_inputs(resolution = "province", data = .nbi_full_data()),
    "resolution"
  )
})
