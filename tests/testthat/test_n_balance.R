# Tests for R/n_balance.R (Module C, Task C7): build_nitrogen_balance().

# Helper fixtures --------------------------------------------------------------

.nb_bnf_input <- function() {
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

.nb_npp_input <- function() {
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

.nb_livestock_intake <- function() {
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

.nb_gridded <- function() {
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

.nb_nhx <- function() {
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

.nb_noy <- function() {
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

.nb_urban_population <- function() {
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

.nb_cropland_ha <- function() {
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

.nb_cell_polity <- function() {
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

.nb_carbon_balance <- function() {
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
    -0.4 # negative son_change_kgn_ha means SOM sequestration is positive
  )
}

.nb_primary_prod <- function() {
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
    700
  )
}

.nb_fertilizer <- function() {
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

.nb_crop_patterns <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~item_prod_code,
    ~harvest_fraction,
    0.25,
    50.25,
    15L,
    1
  )
}

.nb_type_cropland <- function() {
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

.nb_residue_destiny_input <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~item_prod_code,
    ~residue_dm_t,
    ~region_krausmann,
    ~region_hanpp,
    0.25,
    50.25,
    10L,
    2511L,
    2010L,
    "15",
    135.75,
    "Western Europe",
    "Western Europe"
  )
}

.nb_full_data <- function() {
  list(
    bnf_input = .nb_bnf_input(),
    npp_n_input = .nb_npp_input(),
    livestock_intake = .nb_livestock_intake(),
    gridded = .nb_gridded(),
    nhx = .nb_nhx(),
    noy = .nb_noy(),
    urban_population = .nb_urban_population(),
    cropland_ha = .nb_cropland_ha(),
    cell_polity = .nb_cell_polity(),
    carbon_balance = .nb_carbon_balance(),
    primary_prod = .nb_primary_prod(),
    fertilizer = .nb_fertilizer(),
    crop_patterns = .nb_crop_patterns(),
    type_cropland = .nb_type_cropland(),
    residue_destiny_input = .nb_residue_destiny_input()
  )
}

# Joins loss/leaching drivers for every item_cbs_code present in the
# assembled n_inputs, so the join in build_nitrogen_balance() never drops a
# row for a driver mismatch (the tests use method = "ipcc"/"ipcc2019"/
# "ipcc_fracleach", the simplest methods, to keep driver requirements small).
.nb_data_with_drivers <- function() {
  data <- .nb_full_data()
  n_inputs <- whep::build_n_inputs(data = data)
  item_codes <- unique(n_inputs$item_cbs_code)
  fert_types <- c(
    "Excreta_other",
    "Liquid",
    "Solid",
    "SOM",
    "Synthetic",
    "Urban",
    "Recycling"
  )
  data$n_balance_drivers <- tidyr::expand_grid(
    tibble::tibble(
      lon = 0.25,
      lat = 50.25,
      area_code = 10L,
      item_cbs_code = item_codes,
      year = 2010L
    ),
    fert_type = fert_types
  ) |>
    dplyr::mutate(climate = "MED", irrig_type = "Rainfed")
  data$n_balance_leaching_drivers <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = item_codes,
    year = 2010L,
    climate = "MED",
    irrig_cat = "Rainfed",
    land_use = "Cropland",
    cn_input = NA_real_,
    tillage = "Not_specified",
    som_share = 0.03
  )
  data$drainage_mm <- 400
  data
}

.nb_run <- function(data = .nb_data_with_drivers(), resolution = "grid") {
  whep::build_nitrogen_balance(
    methods = list(
      nh3 = "ipcc",
      n2o = "ipcc2019",
      leaching = "ipcc_fracleach"
    ),
    resolution = resolution,
    data = data
  )
}

# Tests --------------------------------------------------------------------

testthat::test_that("balance closes: n_balance_t equals input minus output", {
  out <- .nb_run()
  testthat::expect_true(nrow(out) > 0)
  testthat::expect_true(all(
    abs(out$n_balance_t - (out$n_input_full_t - out$n_output_full_t)) < 1e-6
  ))
})

testthat::test_that("surplus_t is never negative", {
  out <- .nb_run()
  testthat::expect_true(all(out$surplus_t >= 0))
})

testthat::test_that("example fixture is schema-complete", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("nue_std", "nue_som", "nue_useful", "surplus_share", "method_nh3")
  )
})

testthat::test_that("example fixture closes and has non-negative surplus", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  testthat::expect_true(all(
    abs(out$n_balance_t - (out$n_input_full_t - out$n_output_full_t)) < 1e-6
  ))
  testthat::expect_true(all(out$surplus_t >= 0))
})

testthat::test_that("the N-limitation SOM cap engages and recomputes every downstream value", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")

  # Directly test the private two-pass sequence on a constructed scenario:
  # n_balance_t < 0 (deficit) and som_sequestration_n_t > 0 before the cap.
  before_cap <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    n_input_full_t = 50,
    n_input_full_nosom_t = 45,
    n_input_std_t = 45,
    n_input_som_t = 48,
    n_input_for_n2o_t = 40,
    prod_n_t = 20,
    used_residue_n_t = 5,
    burnt_residue_n_t = 2,
    grazed_weeds_n_t = 3,
    som_sequestration_n_t = 10, # would push n_output_full_t above 50+
    nh3_n_t = 30
  )
  pass1 <- whep:::.nb_indicators_pass1(before_cap)
  # n_output_full_t sums to 70 (20 + 5 + 2 + 3 + 30 + 10) against an input
  # of 50, so n_balance_t is negative.
  testthat::expect_lt(pass1$n_balance_t, 0)
  testthat::expect_gt(pass1$som_sequestration_n_t, 0)

  capped <- whep:::.nb_cap_som(pass1)
  # Capped som is pmax(0, 10 + n_balance_t), i.e. 0 here, strictly smaller
  # than the uncapped value of 10.
  testthat::expect_lt(capped$som_sequestration_n_t, pass1$som_sequestration_n_t)
  testthat::expect_equal(capped$som_sequestration_n_t, 0)

  # Recompute every downstream output/balance value from the capped SOM (the
  # SAME .nb_output_aggregates()/.nb_balance() helpers the real pipeline
  # uses), confirming nothing downstream is left stale.
  recomputed <- capped |>
    whep:::.nb_output_aggregates() |>
    whep:::.nb_balance()
  expected_output_full <- 20 + 5 + 2 + 3 + 30 + 0
  testthat::expect_equal(recomputed$n_output_full_t, expected_output_full)
  testthat::expect_equal(
    recomputed$n_balance_t,
    50 - expected_output_full,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    recomputed$surplus_t,
    pmax(0, 50 - expected_output_full)
  )
  # The stale pre-cap n_output_full_t (70) must not survive anywhere.
  testthat::expect_false(isTRUE(all.equal(recomputed$n_output_full_t, 70)))
})

testthat::test_that("resolution = \"polity\" re-aggregates resolution = \"grid\"", {
  data <- .nb_data_with_drivers()
  grid <- .nb_run(data, resolution = "grid")
  polity <- .nb_run(data, resolution = "polity")

  testthat::expect_true(nrow(polity) > 0)
  # Mass-conserving input/output totals must match exactly between
  # resolutions (both were built from the same single grid cell).
  testthat::expect_equal(
    sum(polity$n_input_full_t),
    sum(grid$n_input_full_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    sum(polity$prod_n_t),
    sum(grid$prod_n_t),
    tolerance = 1e-6
  )
  testthat::expect_equal(
    sum(polity$n_output_full_t),
    sum(grid$n_output_full_t),
    tolerance = 1e-6
  )
})

testthat::test_that("NUE denominators are not accidentally collapsed", {
  x <- tibble::tibble(
    n_input_std_t = 100,
    n_input_full_t = 90, # deliberately != n_input_std_t and n_input_som_t
    n_input_som_t = 80,
    prod_n_t = 40,
    n_output_residues_t = 50,
    n_output_som_t = 45,
    n_output_useful_t = 60,
    n_output_full_t = 70
  )
  out <- whep:::.nb_nue(x)

  testthat::expect_equal(out$nue_std, 40 / 100)
  testthat::expect_equal(out$nue_residues, 50 / 100)
  # nue_som and nue_useful divide by n_input_full_t (90), NOT n_input_std_t
  # (100) or n_input_som_t (80).
  testthat::expect_equal(out$nue_som, 45 / 90)
  testthat::expect_equal(out$nue_useful, 60 / 90)
  # nue_full divides by n_input_som_t (80), a THIRD distinct denominator.
  testthat::expect_equal(out$nue_full, 70 / 80)
  testthat::expect_false(isTRUE(all.equal(out$nue_som, 45 / 80)))
  testthat::expect_false(isTRUE(all.equal(out$nue_full, 70 / 90)))
})

testthat::test_that("method_nh3/method_soil_n2o/method_leaching are stamped", {
  out <- .nb_run()
  testthat::expect_true(all(out$method_nh3 == "ipcc"))
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019"))
  testthat::expect_true(all(out$method_leaching == "ipcc_fracleach"))
})

testthat::test_that("resolution argument is validated", {
  testthat::expect_error(
    whep::build_nitrogen_balance(resolution = "province", data = list()),
    "resolution"
  )
})

testthat::test_that("gwp argument is validated and total_gwp_co2e_kg is non-negative", {
  out <- .nb_run()
  testthat::expect_true(all(out$total_gwp_co2e_kg >= 0))
  testthat::expect_error(
    whep::build_nitrogen_balance(gwp = "ar99", data = list())
  )
})
