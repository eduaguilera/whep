# A shared 2x2-cell grid. The surplus fixture carries one crop above the cell's
# critical value, one below it, and crops across all four cells so the grid ->
# polity aggregation can be checked.
.nbx_surplus_fixture <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~n_input_std_t,
    ~surplus_kgn_ha,
    0.25, 0.25, 1L, 2511L, 2010L, 100, 12, 80,
    0.25, 0.25, 1L, 2513L, 2010L, 50, 4, 30,
    0.75, 0.25, 1L, 2511L, 2010L, 200, 30, 90,
    0.25, 0.75, 1L, 2511L, 2010L, 40, 6, 60,
    0.75, 0.75, 1L, 2555L, 2010L, 10, 3, 150
  )
}

.nbx_critical_surplus_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~value,
    0.25, 0.25, 50,
    0.75, 0.25, 120,
    0.25, 0.75, 40,
    0.75, 0.75, 100
  )
}

testthat::test_that("a crop above critical exceeds by the expected share", {
  out <- whep::build_n_boundary_exceedance(
    .nbx_surplus_fixture(),
    .nbx_critical_surplus_fixture(),
    resolution = "grid"
  )
  row <- dplyr::filter(
    out,
    .data$lon == 0.25,
    .data$lat == 0.25,
    .data$item_cbs_code == 2511L
  )
  # actual 80, critical 50 -> share 0.375, over 100 ha.
  testthat::expect_equal(row$exceed_share, 0.375)
  testthat::expect_equal(row$exceedance_kgn_ha, 30)
  testthat::expect_equal(row$within_boundary_kgn_ha, 50)
  testthat::expect_equal(row$exceedance_n_t, 3)
  testthat::expect_equal(row$within_boundary_n_t, 5)
  testthat::expect_equal(row$actual_n_t, 8)
})

testthat::test_that("a crop below critical has zero exceedance", {
  out <- whep::build_n_boundary_exceedance(
    .nbx_surplus_fixture(),
    .nbx_critical_surplus_fixture(),
    resolution = "grid"
  )
  row <- dplyr::filter(
    out,
    .data$lon == 0.25,
    .data$lat == 0.25,
    .data$item_cbs_code == 2513L
  )
  # actual 30 < critical 50 -> no exceedance, all within boundary.
  testthat::expect_equal(row$exceed_share, 0)
  testthat::expect_equal(row$exceedance_kgn_ha, 0)
  testthat::expect_equal(row$exceedance_n_t, 0)
  testthat::expect_equal(row$within_boundary_kgn_ha, 30)
})

testthat::test_that("exceedance and within_boundary conserve the actual", {
  out <- whep::build_n_boundary_exceedance(
    .nbx_surplus_fixture(),
    .nbx_critical_surplus_fixture(),
    resolution = "grid"
  )
  testthat::expect_equal(
    out$exceedance_kgn_ha + out$within_boundary_kgn_ha,
    out$actual_kgn_ha
  )
  testthat::expect_equal(
    out$exceedance_n_t + out$within_boundary_n_t,
    out$actual_n_t
  )
  testthat::expect_true(all(out$exceed_share >= 0 & out$exceed_share <= 1))
})

testthat::test_that("grid resolution retains the per-crop key", {
  surplus <- .nbx_surplus_fixture()
  out <- whep::build_n_boundary_exceedance(
    surplus,
    .nbx_critical_surplus_fixture(),
    resolution = "grid"
  )
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "item_cbs_code", "year")
  )
  testthat::expect_equal(nrow(out), nrow(surplus))
  keyed <- dplyr::distinct(
    out,
    .data$lon,
    .data$lat,
    .data$item_cbs_code,
    .data$year
  )
  testthat::expect_equal(nrow(keyed), nrow(surplus))
  testthat::expect_true(all(out$metric == "surplus"))
  testthat::expect_true(all(out$land_use == "all"))
  testthat::expect_true(all(out$method_boundary == "surplus"))
})

testthat::test_that("the input metric compares per-hectare nitrogen input", {
  critical_input <- tibble::tribble(
    ~lon, ~lat, ~value,
    0.25, 0.25, 80,
    0.75, 0.25, 200,
    0.25, 0.75, 100,
    0.75, 0.75, 250
  )
  out <- whep::build_n_boundary_exceedance(
    .nbx_surplus_fixture(),
    critical_input,
    metric = "input",
    resolution = "grid"
  )
  row <- dplyr::filter(
    out,
    .data$lon == 0.25,
    .data$lat == 0.25,
    .data$item_cbs_code == 2511L
  )
  # input 12 t over 100 ha -> 120 kg N/ha vs critical input 80.
  testthat::expect_equal(row$actual_kgn_ha, 120)
  testthat::expect_equal(row$exceed_share, (120 - 80) / 120)
  testthat::expect_true(all(out$metric == "input"))
})

testthat::test_that("polity resolution sums the mass terms over cells", {
  out <- whep::build_n_boundary_exceedance(
    .nbx_surplus_fixture(),
    .nbx_critical_surplus_fixture(),
    resolution = "polity"
  )
  pointblank::expect_col_exists(out, c("area_code", "item_cbs_code", "year"))
  testthat::expect_false(rlang::has_name(out, "lon"))
  crop <- dplyr::filter(out, .data$item_cbs_code == 2511L)
  # crop 2511: cells (80/100ha over crit 50), (90/200ha under crit 120),
  # (60/40ha over crit 40).
  # actual mass = 8 + 18 + 2.4; exceedance = 3 + 0 + (20 * 40 / 1000).
  testthat::expect_equal(crop$actual_n_t, 8 + 18 + 2.4)
  testthat::expect_equal(crop$exceedance_n_t, 3 + 0 + 0.8)
  testthat::expect_equal(
    crop$exceedance_n_t + crop$within_boundary_n_t,
    crop$actual_n_t
  )
})

testthat::test_that("image_region falls back to polity with a note", {
  testthat::expect_warning(
    out <- whep::build_n_boundary_exceedance(
      .nbx_surplus_fixture(),
      .nbx_critical_surplus_fixture(),
      resolution = "image_region"
    ),
    "IMAGE-region|polity"
  )
  pointblank::expect_col_exists(out, c("area_code", "item_cbs_code", "year"))
})

testthat::test_that("build_n_boundary_exceedance(example = TRUE) runs", {
  out <- whep::build_n_boundary_exceedance(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  pointblank::expect_col_exists(
    out,
    c("item_cbs_code", "exceedance_n_t", "within_boundary_n_t", "actual_n_t")
  )
  testthat::expect_equal(
    out$exceedance_n_t + out$within_boundary_n_t,
    out$actual_n_t
  )
})

# A NEGATIVE critical surplus is real: 1796 of 28881 cells (6.2%) of the
# Schulte-Uebbing gridded critical N surplus are below zero (to -396 kg N/ha),
# meaning the cell tolerates no positive surplus at all. The share must clamp
# to 1 (all of the pressure is exceedance, none within boundary) rather than
# exceeding 1 and driving within_boundary negative.
testthat::test_that("a negative critical value clamps the share to 1", {
  surplus <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~year, ~area_ha, ~surplus_kgn_ha,
    1, 1, 10L, 2511L, 2000L, 100, 50
  )
  critical <- tibble::tibble(lon = 1, lat = 1, value = -100)
  out <- whep::build_n_boundary_exceedance(surplus, critical)
  testthat::expect_equal(out$exceed_share, 1)
  testthat::expect_equal(out$exceedance_kgn_ha, 50)
  testthat::expect_equal(out$within_boundary_kgn_ha, 0)
  # The decomposition still partitions the pressure exactly.
  testthat::expect_equal(
    out$exceedance_n_t + out$within_boundary_n_t,
    out$actual_n_t
  )
  testthat::expect_true(out$exceed_share >= 0 && out$exceed_share <= 1)
})

testthat::test_that("shares stay in [0, 1] across a critical-value sweep", {
  surplus <- tibble::tibble(
    lon = 1:5,
    lat = 1,
    area_code = 10L,
    item_cbs_code = 2511L,
    year = 2000L,
    area_ha = 100,
    surplus_kgn_ha = 40
  )
  critical <- tibble::tibble(lon = 1:5, lat = 1, value = c(-396, -1, 0, 20, 80))
  out <- whep::build_n_boundary_exceedance(surplus, critical)
  testthat::expect_true(all(out$exceed_share >= 0 & out$exceed_share <= 1))
  testthat::expect_true(all(out$within_boundary_kgn_ha >= 0))
  testthat::expect_true(all(out$exceedance_kgn_ha <= out$actual_kgn_ha))
})
