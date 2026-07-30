# Shared 2x2-cell balance fixture. Cell (0.25,0.25) has groundwater as the
# tighter water load, cell (0.75,0.25) has surface water as the tighter one, so
# binding_water_medium can be checked both ways. The crops span cells so the
# grid -> polity aggregation can be checked.
.npb_balance_fixture <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~nh3_n_t,
    ~no3_n_t,
    0.25, 0.25, 1L, 2511L, 2010L, 100, 4.0, 2.0,
    0.25, 0.25, 1L, 2513L, 2010L, 50, 0.5, 3.0,
    0.75, 0.25, 1L, 2511L, 2010L, 200, 2.0, 8.0,
    0.25, 0.75, 1L, 2511L, 2010L, 40, 1.0, 1.0,
    0.75, 0.75, 1L, 2555L, 2010L, 10, 0.3, 0.5
  )
}

.npb_loads_fixture <- function() {
  list(
    crit_nh3_emission = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 25,
      0.75, 0.25, 30,
      0.25, 0.75, 20,
      0.75, 0.75, 100
    ) |>
      dplyr::mutate(critical_var = "crit_nh3_emission"),
    crit_leaching_gw = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 30,
      0.75, 0.25, 50,
      0.25, 0.75, 45,
      0.75, 0.75, 60
    ) |>
      dplyr::mutate(critical_var = "crit_leaching_gw"),
    crit_load_sw = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 40,
      0.75, 0.25, 20,
      0.25, 0.75, 55,
      0.75, 0.75, 70
    ) |>
      dplyr::mutate(critical_var = "crit_load_sw")
  )
}

.npb_row <- function(out, cell_lon, item) {
  dplyr::filter(
    out,
    .data$lon == cell_lon,
    .data$lat == 0.25,
    .data$item_cbs_code == item
  )
}

testthat::test_that("air pressure exceeds the critical ammonia by the share", {
  out <- whep::build_n_pathway_exceedance(
    .npb_balance_fixture(),
    .npb_loads_fixture(),
    resolution = "grid"
  )
  row <- .npb_row(out, 0.25, 2511L)
  # nh3 4 t over 100 ha -> 40 kg N/ha vs critical 25 -> share 0.375.
  testthat::expect_equal(row$actual_air_kgn_ha, 40)
  testthat::expect_equal(row$exceed_share_air, 0.375)
  testthat::expect_equal(row$exceedance_air_kgn_ha, 15)
  testthat::expect_equal(row$within_air_kgn_ha, 25)
  testthat::expect_equal(row$exceedance_air_n_t, 1.5)
  testthat::expect_equal(row$within_air_n_t, 2.5)
  testthat::expect_equal(row$actual_air_n_t, 4)
})

testthat::test_that("the tighter water load binds per cell", {
  out <- whep::build_n_pathway_exceedance(
    .npb_balance_fixture(),
    .npb_loads_fixture(),
    resolution = "grid"
  )
  gw_cell <- .npb_row(out, 0.25, 2511L)
  sw_cell <- .npb_row(out, 0.75, 2511L)
  # (0.25,0.25): min(gw 30, sw 40) = 30, groundwater binds.
  testthat::expect_equal(gw_cell$critical_water_kgn_ha, 30)
  testthat::expect_equal(gw_cell$binding_water_medium, "groundwater")
  # (0.75,0.25): min(gw 50, sw 20) = 20, surface water binds.
  testthat::expect_equal(sw_cell$critical_water_kgn_ha, 20)
  testthat::expect_equal(sw_cell$binding_water_medium, "surface_water")
  # (0.75,0.25): no3 8 t over 200 ha -> 40 kg N/ha vs critical 20 -> share 0.5.
  testthat::expect_equal(sw_cell$actual_water_kgn_ha, 40)
  testthat::expect_equal(sw_cell$exceed_share_water, 0.5)
})

testthat::test_that("binding_boundary picks the higher-exceedance medium", {
  out <- whep::build_n_pathway_exceedance(
    .npb_balance_fixture(),
    .npb_loads_fixture(),
    resolution = "grid"
  )
  # (0.25,0.25) crop 2511: air share 0.375, water 20<30 so 0 -> air binds.
  testthat::expect_equal(.npb_row(out, 0.25, 2511L)$binding_boundary, "air")
  # (0.75,0.25) crop 2511: air 10<30 so 0, water share 0.5 -> water binds.
  testthat::expect_equal(.npb_row(out, 0.75, 2511L)$binding_boundary, "water")
  # (0.75,0.75) crop 2555: neither medium exceeds -> none.
  none_row <- dplyr::filter(out, .data$item_cbs_code == 2555L)
  testthat::expect_equal(none_row$binding_boundary, "none")
})

testthat::test_that("a zero-area (non-crop) row gets NA binding_boundary, not 'both'", {
  # Regression: rows with area_ha 0/NA (the per-cell deposition / urban / SOM
  # non-crop terms) have NA per-hectare exceedance shares, which fail every
  # case_when comparison and used to fall through to the "both" default. They
  # must be NA -- a row with no agricultural area has no binding medium. Keep
  # the row even when it lies outside all three critical-load rasters.
  balance <- dplyr::bind_rows(
    .npb_balance_fixture(),
    tibble::tibble(
      lon = 9.25,
      lat = 9.25,
      area_code = 1L,
      item_cbs_code = NA_integer_,
      year = 2010L,
      area_ha = 0,
      nh3_n_t = 5,
      no3_n_t = 5
    )
  )
  out <- whep::build_n_pathway_exceedance(
    balance,
    .npb_loads_fixture(),
    resolution = "grid"
  )
  na_row <- dplyr::filter(out, is.na(.data$item_cbs_code))
  testthat::expect_equal(nrow(na_row), 1L)
  testthat::expect_true(is.na(na_row$binding_boundary))
})

testthat::test_that("missing pathway-load coverage aborts", {
  loads <- .npb_loads_fixture()
  loads$crit_load_sw <- dplyr::filter(
    loads$crit_load_sw,
    .data$lon != 0.75 | .data$lat != 0.75
  )
  testthat::expect_error(
    whep::build_n_pathway_exceedance(
      .npb_balance_fixture(),
      loads,
      resolution = "grid"
    ),
    "Critical-layer coverage is incomplete|critical_sw_kgn_ha"
  )
})

testthat::test_that("each medium conserves and shares stay in [0, 1]", {
  out <- whep::build_n_pathway_exceedance(
    .npb_balance_fixture(),
    .npb_loads_fixture(),
    resolution = "grid"
  )
  testthat::expect_equal(
    out$exceedance_air_kgn_ha + out$within_air_kgn_ha,
    out$actual_air_kgn_ha
  )
  testthat::expect_equal(
    out$exceedance_air_n_t + out$within_air_n_t,
    out$actual_air_n_t
  )
  testthat::expect_equal(
    out$exceedance_water_kgn_ha + out$within_water_kgn_ha,
    out$actual_water_kgn_ha
  )
  testthat::expect_equal(
    out$exceedance_water_n_t + out$within_water_n_t,
    out$actual_water_n_t
  )
  testthat::expect_true(
    all(out$exceed_share_air >= 0 & out$exceed_share_air <= 1)
  )
  testthat::expect_true(
    all(out$exceed_share_water >= 0 & out$exceed_share_water <= 1)
  )
})

testthat::test_that("grid resolution retains the per-crop key and stamps", {
  balance <- .npb_balance_fixture()
  out <- whep::build_n_pathway_exceedance(
    balance,
    .npb_loads_fixture(),
    resolution = "grid"
  )
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "item_cbs_code", "year")
  )
  testthat::expect_equal(nrow(out), nrow(balance))
  keyed <- dplyr::distinct(
    out,
    .data$lon,
    .data$lat,
    .data$item_cbs_code,
    .data$year
  )
  testthat::expect_equal(nrow(keyed), nrow(balance))
  testthat::expect_true(all(out$nh3_source == "soil"))
  testthat::expect_true(all(out$method_boundary == "pathway"))
})

testthat::test_that("total_agricultural adds manure ammonia to the air", {
  manure <- tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~manure_mgmt_nh3_n_t,
    0.25, 0.25, 1L, 2511L, 2010L, 1.0
  )
  soil <- whep::build_n_pathway_exceedance(
    .npb_balance_fixture(),
    .npb_loads_fixture(),
    nh3_source = "soil",
    resolution = "grid"
  )
  total <- whep::build_n_pathway_exceedance(
    .npb_balance_fixture(),
    .npb_loads_fixture(),
    nh3_source = "total_agricultural",
    data = list(manure_mgmt_nh3_n_t = manure),
    resolution = "grid"
  )
  # (0.25,0.25) crop 2511: 4 t + 1 t manure over 100 ha -> 50 kg N/ha (was 40).
  testthat::expect_equal(.npb_row(soil, 0.25, 2511L)$actual_air_kgn_ha, 40)
  testthat::expect_equal(.npb_row(total, 0.25, 2511L)$actual_air_kgn_ha, 50)
  testthat::expect_gt(
    .npb_row(total, 0.25, 2511L)$exceed_share_air,
    .npb_row(soil, 0.25, 2511L)$exceed_share_air
  )
  testthat::expect_true(all(total$nh3_source == "total_agricultural"))
})

testthat::test_that("total_agricultural without manure aborts", {
  testthat::expect_error(
    whep::build_n_pathway_exceedance(
      .npb_balance_fixture(),
      .npb_loads_fixture(),
      nh3_source = "total_agricultural",
      resolution = "grid"
    ),
    "manure-management|total_agricultural"
  )
})

testthat::test_that("polity resolution sums the per-medium mass terms", {
  out <- whep::build_n_pathway_exceedance(
    .npb_balance_fixture(),
    .npb_loads_fixture(),
    resolution = "polity"
  )
  pointblank::expect_col_exists(out, c("area_code", "item_cbs_code", "year"))
  testthat::expect_false(rlang::has_name(out, "lon"))
  crop <- dplyr::filter(out, .data$item_cbs_code == 2511L)
  # crop 2511 air mass: 4 (0.25,0.25) + 2 (0.75,0.25) + 1 (0.25,0.75) = 7.
  testthat::expect_equal(crop$actual_air_n_t, 7)
  testthat::expect_equal(
    crop$exceedance_air_n_t + crop$within_air_n_t,
    crop$actual_air_n_t
  )
  testthat::expect_equal(
    crop$exceedance_water_n_t + crop$within_water_n_t,
    crop$actual_water_n_t
  )
})

testthat::test_that("build_n_pathway_exceedance(example = TRUE) runs", {
  out <- whep::build_n_pathway_exceedance(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  pointblank::expect_col_exists(
    out,
    c(
      "item_cbs_code",
      "exceed_share_air",
      "exceed_share_water",
      "binding_boundary",
      "binding_water_medium"
    )
  )
  testthat::expect_equal(
    out$exceedance_air_n_t + out$within_air_n_t,
    out$actual_air_n_t
  )
  testthat::expect_equal(
    out$exceedance_water_n_t + out$within_water_n_t,
    out$actual_water_n_t
  )
})
