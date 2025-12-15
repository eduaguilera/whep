testthat::test_that("prepare_luh2_land_use aggregates LUH2 land areas", {
  stock_area_full <- tibble::tribble(
    ~Year, ~area, ~area_code, ~Land_Use, ~Area_Mha,
    1850, "A", 1, "c3ann", 1,
    1850, "A", 1, "pastr", 2,
    1850, "A", 1, "urban", 10,
    1851, "A", 1, "c3ann", 1.5,
    1851, "A", 1, "range", 0.5
  )

  land_use <- prepare_luh2_land_use(
    stock_area_full,
    cropland_varnames = c("c3ann"),
    pasture_varnames = c("pastr", "range")
  )

  land_use |>
    pointblank::expect_col_exists(c("year", "Cropland", "Pasture",
      "Agriland")) |>
    pointblank::expect_col_vals_equal(
      Cropland,
      c(1e6, 1.5e6)
    ) |>
    pointblank::expect_col_vals_equal(
      Pasture,
      c(2e6, 0.5e6)
    ) |>
    pointblank::expect_col_vals_equal(
      Agriland,
      c(3e6, 2e6)
    )
})

testthat::test_that("create_grassland_production returns pasture and range", {
  stock_area_full <- tibble::tribble(
    ~Year, ~area, ~area_code, ~Land_Use, ~Area_Mha,
    1850, "A", 1, "pastr", 2,
    1850, "A", 1, "range", 1
  )

  grassland <- create_grassland_production(stock_area_full)

  grassland |>
    pointblank::expect_col_exists(c("item_prod", "item_prod_code",
      "item_cbs_code", "unit", "value")) |>
    pointblank::expect_col_vals_in_set(item_prod, c("Pasture", "range")) |>
    pointblank::expect_col_vals_equal(unit, c("ha", "ha")) |>
    pointblank::expect_col_vals_equal(value, c(2e6, 1e6))
})

testthat::test_that("filter_country_temporal_validity removes invalid years", {
  data <- tibble::tribble(
    ~year, ~area, ~area_code, ~value,
    1992, "Czechoslovakia", 1, 1,
    1993, "Czechoslovakia", 1, 1,
    1992, "Czech Republic", 2, 1,
    1993, "Czech Republic", 2, 1
  )

  filtered <- filter_country_temporal_validity(data)

  filtered |>
    dplyr::arrange(area, year) |>
    testthat::expect_equal(
      tibble::tribble(
        ~year, ~area, ~area_code, ~value,
        1992, "Czechoslovakia", 1, 1,
        1993, "Czech Republic", 2, 1
      )
    )
})

testthat::test_that("finalize_primary_production averages duplicates", {
  data <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~live_anim_code,
      ~unit, ~value,
    2000, 1, 10, 100, NA, "tonnes", 1,
    2000, 1, 10, 100, NA, "tonnes", 3
  )

  out <- finalize_primary_production(data)

  out |>
    testthat::expect_equal(
      tibble::tribble(
        ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~live_anim_code,
          ~unit, ~value,
        2000, 1, 10, 100, NA, "tonnes", 2
      )
    )
})
