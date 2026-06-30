testthat::test_that("build_water_balance closes the water budget exactly", {
  wb <- whep::build_water_balance(example = TRUE)

  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$runoff_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 1e-6))

  pointblank::expect_col_exists(
    wb,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "drainage_mm",
      "aet_mm",
      "water_input_mm",
      "method_water"
    )
  )
})

testthat::test_that("method_water records the aet, drainage and bg methods", {
  wb <- whep::build_water_balance(example = TRUE)
  testthat::expect_true(all(
    wb$method_water == "aet:components|drain:seepage|bg:cft_native"
  ))
})

testthat::test_that("residual drainage still closes the 4-term budget", {
  wb <- whep::build_water_balance(
    method = list(drainage = "residual"),
    example = TRUE
  )
  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$runoff_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 1e-6))
  testthat::expect_true(all(
    wb$method_water == "aet:components|drain:residual|bg:cft_native"
  ))
})

testthat::test_that("get_soc_climate_drivers returns monthly climate drivers", {
  drv <- whep::get_soc_climate_drivers(example = TRUE)
  pointblank::expect_col_exists(
    drv,
    c(
      "lon",
      "lat",
      "area_code",
      "year",
      "month",
      "temp_c",
      "swc_topsoil",
      "clay_pct"
    )
  )
  pointblank::expect_col_vals_between(drv, "month", 1, 12)
})

testthat::test_that("polity resolution aggregates by year and area_code", {
  grid <- whep::build_water_balance(resolution = "grid", example = TRUE)
  pol <- whep::build_water_balance(resolution = "polity", example = TRUE)

  pointblank::expect_col_exists(pol, c("year", "area_code"))
  testthat::expect_false(rlang::has_name(pol, "lon"))
  testthat::expect_equal(
    nrow(pol),
    nrow(dplyr::distinct(grid, year, area_code))
  )
})

testthat::test_that("an invalid drainage method is rejected", {
  testthat::expect_error(
    whep::build_water_balance(method = list(drainage = "nonsense")),
    "drainage"
  )
})

# ---- Real-path closure test (exercises read -> compute -> blue_green ->
# finalise on synthetic per-cell MONTHLY inputs; NOT the fixture). The synthetic
# inputs are constructed so the 4-term budget closes exactly, i.e. precipitation
# plus irrigation equals AET plus runoff plus seepage plus the storage change.

# Build synthetic monthly LPJmL-style inputs for `n_cells` cells x 12 months x
# one year. Soil-water saturation drops linearly Jan -> Dec so dStorage != 0 and
# the storage term participates. prec is solved so the 4-term budget closes.
.wb_synthetic_monthly <- function() {
  cells <- tibble::tribble(
    ~lon, ~lat, ~area_code,
    9.25, 47.75, 11L,
    -55.25, -12.25, 21L
  )
  porosity <- 0.4
  thickness_mm <- c(200, 300, 500, 1000, 1000, 10000)
  swc_jan <- c(0.50, 0.45, 0.40, 0.35, 0.30, 0.25)
  swc_dec <- c(0.40, 0.38, 0.34, 0.31, 0.28, 0.24)
  d_storage_mm <- sum((swc_dec - swc_jan) * thickness_mm * porosity)

  months <- 1:12
  flux <- tidyr::expand_grid(cells, month = months) |>
    dplyr::mutate(
      year = 2000L,
      transp = 40 + lon * 0,
      evap = 15,
      interc = 5,
      irrig = 8,
      runoff = 12,
      seepage = 10
    )
  aet_annual <- (40 + 15 + 5) * 12
  irrig_annual <- 8 * 12
  runoff_annual <- 12 * 12
  seepage_annual <- 10 * 12
  water_input_annual <- aet_annual +
    runoff_annual +
    seepage_annual +
    d_storage_mm
  prec_monthly <- (water_input_annual - irrig_annual) / 12
  flux <- dplyr::mutate(flux, prec = prec_monthly)

  swc <- tidyr::expand_grid(
    cells,
    month = months,
    layer = seq_along(swc_jan)
  ) |>
    dplyr::mutate(
      year = 2000L,
      value = swc_jan[layer] +
        (swc_dec[layer] - swc_jan[layer]) * (month - 1) / 11
    )

  to_long <- function(var) {
    dplyr::select(flux, lon, lat, year, month, value = dplyr::all_of(var))
  }
  cell_polity <- dplyr::mutate(cells, polity_frac = 1, cell_area_ha = 30000)
  list(
    inputs = list(
      transp = to_long("transp"),
      evap = to_long("evap"),
      interc = to_long("interc"),
      prec = to_long("prec"),
      irrig = to_long("irrig"),
      runoff = to_long("runoff"),
      seepage = to_long("seepage"),
      swc = swc,
      cell_polity = cell_polity
    ),
    water_input_annual = water_input_annual
  )
}

testthat::test_that("real-path 4-term budget closes within 1% (runoff included)", {
  syn <- .wb_synthetic_monthly()
  wb <- suppressWarnings(
    whep::build_water_balance(data = syn$inputs, example = FALSE)
  )

  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$runoff_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 0.01 * wb$water_input_mm))
  testthat::expect_true(all(wb$runoff_mm > 0))
  pointblank::expect_col_exists(wb, c("runoff_mm", "drainage_mm"))
})

testthat::test_that("real-path method_water records the blue_green choice", {
  syn <- .wb_synthetic_monthly()
  wb <- suppressWarnings(
    whep::build_water_balance(data = syn$inputs, example = FALSE)
  )
  testthat::expect_true(all(stringr::str_detect(wb$method_water, "bg:")))
})

testthat::test_that("cft_native without per-CFT data warns and falls back", {
  syn <- .wb_synthetic_monthly()
  testthat::expect_warning(
    wb <- whep::build_water_balance(data = syn$inputs, example = FALSE),
    "per-CFT consumptive water"
  )
  testthat::expect_true(all(
    stringr::str_detect(wb$method_water, "bg:irrig_share_fallback")
  ))
})

testthat::test_that("cft_native uses per-CFT consumptive water when supplied", {
  syn <- .wb_synthetic_monthly()
  cells <- dplyr::distinct(syn$inputs$prec, lon, lat, year)
  syn$inputs$cft_consump_water_b <- dplyr::mutate(cells, value = 120)
  syn$inputs$cft_consump_water_g <- dplyr::mutate(cells, value = 280)

  wb <- whep::build_water_balance(data = syn$inputs, example = FALSE)
  testthat::expect_true(all(stringr::str_detect(
    wb$method_water,
    "bg:cft_native"
  )))
  # blue share = 120 / (120 + 280) = 0.3 of total AET.
  testthat::expect_equal(wb$aet_blue_mm, wb$aet_mm * 0.3, tolerance = 1e-8)
})

testthat::test_that("real-path rejects an invalid drainage method", {
  syn <- .wb_synthetic_monthly()
  testthat::expect_error(
    whep::build_water_balance(
      method = list(drainage = "nonsense"),
      data = syn$inputs
    ),
    "drainage"
  )
})
