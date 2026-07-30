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
    ~area_ha,
    ~product_dm_t,
    ~residue_dm_t,
    ~root_dm_t,
    0.25,
    50.25,
    10L,
    2010L,
    "15",
    2511L,
    40,
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

.nb_ag_land_support <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~land_use, ~area_ha,
    0.25, 50.25, 10L, 2511L, 2010L, "cropland", 1000,
    0.25, 50.25, 10L, 3000L, 2010L, "grassland", 2000
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
    ag_land_support = .nb_ag_land_support(),
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

testthat::test_that("calculate_npp_carbon_nitrogen runs once per build_nitrogen_balance call", {
  # build_n_inputs()'s "recycling" term and this function's own prod_n_t
  # term both need calculate_npp_carbon_nitrogen()'s result; a prior
  # version recomputed it twice despite an .n_balance_npp() cache helper
  # because the cache was never populated before the first call site ran.
  # Prepare the fixture data BEFORE mocking: .nb_data_with_drivers() makes
  # its own build_n_inputs() call to derive driver rows, which is a
  # legitimate, separate call outside build_nitrogen_balance() and must
  # not be counted here.
  data <- .nb_data_with_drivers()
  call_count <- 0
  real_fn <- whep::calculate_npp_carbon_nitrogen
  testthat::local_mocked_bindings(
    calculate_npp_carbon_nitrogen = function(...) {
      call_count <<- call_count + 1
      real_fn(...)
    },
    .package = "whep"
  )
  .nb_run(data = data)
  testthat::expect_equal(call_count, 1L)
})

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

testthat::test_that("output-only production keys are preserved", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  inputs <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    n_input_full_t = 1
  )
  npp <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2513L,
    year = 2010L,
    product_n_t = 2
  )

  out <- whep:::.nb_add_prod_n(inputs, list(.npp_cache = npp), key)

  testthat::expect_setequal(out$item_cbs_code, c(2511L, 2513L))
  testthat::expect_equal(out$prod_n_t[out$item_cbs_code == 2513L], 2)
  testthat::expect_equal(out$n_input_full_t[out$item_cbs_code == 2513L], 0)
})

testthat::test_that("output-only residue-destiny keys are preserved", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  inputs <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2807L,
    year = 2010L,
    n_input_full_t = 1
  )
  data <- list(residue_destiny_input = .nb_residue_destiny_input())

  out <- whep:::.nb_add_residue_destiny(inputs, data, key)

  testthat::expect_setequal(out$item_cbs_code, c(2511L, 2807L))
  testthat::expect_equal(out$n_input_full_t[out$item_cbs_code == 2511L], 0)
  testthat::expect_false(anyNA(
    out$used_residue_n_t[out$item_cbs_code == 2511L]
  ))
})

testthat::test_that("duplicate loss-driver keys abort instead of duplicating emissions", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  n_inputs <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L,
    fert_type = "synthetic",
    n_input_t = 10
  )
  drivers <- tibble::tibble(
    lon = c(0.25, 0.25),
    lat = c(50.25, 50.25),
    area_code = c(10L, 10L),
    item_cbs_code = c(2511L, 2511L),
    year = c(2010L, 2010L),
    fert_type = c("Synthetic", "Synthetic"),
    climate = c("MED", "MED")
  )

  testthat::expect_error(
    whep:::.nb_loss_rows(n_inputs, key, list(n_balance_drivers = drivers))
  )
})

testthat::test_that("a balance with no loss-relevant inputs gets zero losses", {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  balance <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2010L
  )
  n_inputs <- dplyr::mutate(balance, fert_type = "bnf", n_input_t = 10)
  methods <- list(
    nh3 = "manner",
    n2o = "ipcc2019",
    leaching = "meisinger_drainage"
  )

  out <- whep:::.nb_losses(balance, n_inputs, key, methods, list())

  testthat::expect_equal(out$nh3_n_t, 0)
  testthat::expect_equal(out$n2o_direct_n_t, 0)
  testthat::expect_equal(out$n2o_indirect_nh3_n_t, 0)
})

testthat::test_that("loss methods are validated even when no loss rows exist", {
  testthat::expect_error(whep:::.nb_methods(list(nh3 = "not_a_method")))
  testthat::expect_error(whep:::.nb_methods(list(n2o = "not_a_method")))
  testthat::expect_error(whep:::.nb_methods(list(leaching = "not_a_method")))
  testthat::expect_error(whep:::.nb_methods(list(nh_3 = "ipcc")))
})

testthat::test_that("example fixture is schema-complete", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("nue_std", "nue_som", "nue_useful", "surplus_share", "method_nh3")
  )
})

testthat::test_that("example fixture carries a positive area_ha keyed to the crop", {
  out <- whep::build_nitrogen_balance(example = TRUE)
  pointblank::expect_col_exists(out, "area_ha")
  # area_ha is explicit physical agricultural support, keyed to the crop.
  testthat::expect_true(all(out$area_ha > 0))
  testthat::expect_true(rlang::has_name(out, "item_cbs_code"))
})

testthat::test_that("build_nitrogen_balance emits per-crop area_ha on the grid key", {
  # Explicit agricultural support supplies the physical crop area (1000 ha
  # for item_cbs 2511). It must survive on the grid row so boundary pressure
  # is not computed from harvested-area proxies.
  out <- .nb_run()
  pointblank::expect_col_exists(out, "area_ha")
  crop <- dplyr::filter(out, !is.na(item_cbs_code), item_cbs_code == 2511L)
  testthat::expect_equal(nrow(crop), 1L)
  testthat::expect_equal(crop$area_ha, 1000)
  testthat::expect_true(all(crop$area_ha > 0))
})

testthat::test_that("resolution = \"polity\" sums area_ha per key over cells", {
  data <- .nb_data_with_drivers()
  grid <- .nb_run(data, resolution = "grid")
  polity <- .nb_run(data, resolution = "polity")
  pointblank::expect_col_exists(polity, "area_ha")
  # Single grid cell, so physical crop area is conserved across resolutions.
  testthat::expect_equal(
    sum(polity$area_ha),
    sum(grid$area_ha),
    tolerance = 1e-6
  )
  crop_polity <- dplyr::filter(
    polity,
    !is.na(item_cbs_code),
    item_cbs_code == 2511L
  )
  testthat::expect_equal(crop_polity$area_ha, 1000)
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

testthat::test_that("zero-input balance rows have undefined, not infinite, ratios", {
  balance <- tibble::tibble(
    n_input_full_t = 0,
    n_output_full_t = 2
  ) |>
    whep:::.nb_balance()
  nue <- tibble::tibble(
    n_input_std_t = 0,
    n_input_full_t = 0,
    n_input_som_t = 0,
    prod_n_t = 2,
    n_output_residues_t = 2,
    n_output_som_t = 2,
    n_output_useful_t = 2,
    n_output_full_t = 2
  ) |>
    whep:::.nb_nue()

  testthat::expect_true(is.na(balance$surplus_share))
  testthat::expect_true(all(is.na(dplyr::select(
    nue,
    dplyr::starts_with("nue_")
  ))))
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

testthat::test_that("total_gwp_co2e_kg matches the 44/28 x GWP x 1000 formula", {
  # .nb_gwp() sums the three N2O-N terms (kg... actually tonnes N), converts
  # N2O-N -> N2O via the 44/28 molecular-mass ratio (whep:::.soil_n2o_factors()
  # $n_to_n2o), applies the N2O GWP factor from whep:::.ghg_gwp_factors(gwp),
  # then scales tonnes -> kilograms (x1000). Pulling both factors from their
  # actual source (rather than hardcoding guessed numbers) means this test
  # fails if either constant, or the x1000 scaling, silently drifts.
  n_to_n2o <- whep:::.soil_n2o_factors()$n_to_n2o
  gwp_n2o_ar6 <- whep:::.ghg_gwp_factors("ar6")[["n2o"]]
  gwp_n2o_ar5 <- whep:::.ghg_gwp_factors("ar5")[["n2o"]]

  x <- tibble::tibble(
    n2o_direct_n_t = 2,
    n2o_indirect_nh3_n_t = 0.5,
    n2o_indirect_no3_n_t = 0.3
  )
  total_n2o_n_t <- 2 + 0.5 + 0.3

  out_ar6 <- whep:::.nb_gwp(x, gwp = "ar6")
  expected_ar6 <- total_n2o_n_t * n_to_n2o * gwp_n2o_ar6 * 1000
  testthat::expect_equal(
    out_ar6$total_gwp_co2e_kg,
    expected_ar6,
    tolerance = 1e-9
  )

  out_ar5 <- whep:::.nb_gwp(x, gwp = "ar5")
  expected_ar5 <- total_n2o_n_t * n_to_n2o * gwp_n2o_ar5 * 1000
  testthat::expect_equal(
    out_ar5$total_gwp_co2e_kg,
    expected_ar5,
    tolerance = 1e-9
  )
  testthat::expect_false(isTRUE(all.equal(expected_ar6, expected_ar5)))
})

testthat::test_that("SOM sequestration is kept when no NA-item input row exists", {
  # A cell in net carbon GAIN (son_change_kgn_ha < 0) emits no
  # som_mineralization input row; if it also lacks deposition/urban N, x has
  # no NA-item row. A left join would drop the sequestration output entirely;
  # the full-join merge must keep it (as its own NA-item row).
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  x <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 2511L, # a real crop row only; NO NA-item row present
    year = 2010L,
    prod_n_t = 40,
    used_residue_n_t = 5,
    burnt_residue_n_t = 2,
    grazed_weeds_n_t = 0,
    n_input_full_t = 100
  )
  cb <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    land_use = "Cropland",
    year = 2010L,
    area_ha = 1000,
    son_change_kgn_ha = -0.4 # negative => sequestration = 0.4 * 1000 / 1000
  )
  out <- whep:::.nb_add_som_sequestration(x, list(carbon_balance = cb), key)
  testthat::expect_equal(sum(out$som_sequestration_n_t), 0.4)
  # The sequestration lands on its own NA-item row, not the crop row.
  na_row <- out[is.na(out$item_cbs_code), ]
  testthat::expect_equal(nrow(na_row), 1L)
  testthat::expect_equal(na_row$som_sequestration_n_t, 0.4)
  # Input aggregates carried onto the output-only row default to 0, not NA.
  testthat::expect_false(anyNA(out$n_input_full_t))
  testthat::expect_false(anyNA(out$prod_n_t))
})

testthat::test_that("grazed weeds key the grass sentinel (3000L), joining grass rows", {
  # build_n_inputs()'s manure engine keys grass rows to item_cbs_code 3000L;
  # grazed weeds must use the SAME sentinel so they attach to the grass row
  # rather than an NA code that never matches it.
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  x <- tibble::tibble(
    lon = 0.25,
    lat = 50.25,
    area_code = 10L,
    item_cbs_code = 3000L, # grass row from the manure engine; NO NA-item row
    year = 2010L,
    prod_n_t = 0,
    used_residue_n_t = 0,
    burnt_residue_n_t = 0,
    n_input_full_t = 20
  )
  intake <- tibble::tibble(
    year = 2010L,
    territory = "10",
    sub_territory = "0.25_50.25",
    feed_quality = "grass",
    intake_dm_t = 600
  )
  weed_coef <- whep::whep_coef_table("weed_coefs")$residue_n_kgdm_weed
  out <- whep:::.nb_add_grazed_weeds(x, list(livestock_intake = intake), key)
  # 600 * weed_coef of grazed-weeds N, attached to the single grass row
  # (no phantom NA row created).
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$item_cbs_code, 3000L)
  testthat::expect_equal(out$grazed_weeds_n_t, 600 * weed_coef)
})

testthat::test_that("duplicate n_balance_leaching_drivers keys abort the join", {
  # A caller-supplied leaching-drivers table with a duplicate balance key
  # would fan x out and misalign the balance-key-aligned drainage_mm vector;
  # the many-to-one join must abort at the source instead.
  key <- c("area_code", "item_cbs_code", "year")
  x <- tibble::tibble(
    area_code = c(10L, 10L),
    item_cbs_code = c(2511L, 2513L),
    year = 2010L
  )
  drivers_dup <- tibble::tibble(
    area_code = 10L,
    item_cbs_code = c(2511L, 2511L, 2513L), # 2511 duplicated
    year = 2010L,
    climate = "MED"
  )
  testthat::expect_error(
    whep:::.nb_leaching_join(x, drivers_dup, key),
    "match at most 1 row"
  )
  # A unique table joins without error (one row per balance key).
  drivers_ok <- tibble::tibble(
    area_code = 10L,
    item_cbs_code = c(2511L, 2513L),
    year = 2010L,
    climate = "MED"
  )
  testthat::expect_equal(nrow(whep:::.nb_leaching_join(x, drivers_ok, key)), 2L)
})
