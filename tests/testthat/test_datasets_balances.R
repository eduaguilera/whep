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
