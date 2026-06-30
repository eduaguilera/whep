testthat::test_that("build_water_balance closes the water budget exactly", {
  wb <- whep::build_water_balance(example = TRUE)

  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$drainage_mm + wb$soil_water_change_mm)
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

testthat::test_that("method_water records the chosen aet and drainage methods", {
  wb <- whep::build_water_balance(example = TRUE)
  testthat::expect_true(all(wb$method_water == "aet:components|drain:seepage"))
})

testthat::test_that("runoff_resid drainage still closes the budget", {
  wb <- whep::build_water_balance(
    method = list(drainage = "runoff_resid"),
    example = TRUE
  )
  resid <- wb$water_input_mm -
    (wb$aet_mm + wb$drainage_mm + wb$soil_water_change_mm)
  testthat::expect_true(all(abs(resid) < 1e-6))
  testthat::expect_true(all(
    wb$method_water == "aet:components|drain:runoff_resid"
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
