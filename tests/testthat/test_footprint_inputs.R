testthat::test_that("get_land_fp_production example returns expected structure", {
  result <- get_land_fp_production(example = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  pointblank::expect_col_exists(
    result,
    c(
      "year",
      "area_code",
      "item_cbs_code",
      "impact",
      "element",
      "origin",
      "group",
      "impact_u"
    )
  )
  testthat::expect_type(result$year, "integer")
  testthat::expect_type(result$area_code, "integer")
  testthat::expect_type(result$item_cbs_code, "integer")
  pointblank::expect_col_vals_in_set(
    result,
    impact,
    set = "Land"
  )
  pointblank::expect_col_vals_in_set(
    result,
    origin,
    set = "Production"
  )
})


testthat::test_that("get_land_fp_production filters and cleans land_fp", {
  local_mocked_bindings(
    whep_read_file = function(...) {
      tibble::tribble(
          ~year, ~area, ~item_code, ~Impact,
          ~element, ~Origin, ~group, ~impact_u,
          2020, "Spain", 2511, "Land",
          "Cropland", "Production", "Crops", 10,
          2020, "Spain", 2511, "Water",
          "Blue", "Production", "Crops", 2,
          2020, "Spain", 2511, "Land",
          "Cropland", "Import", "Crops", 3
        )
    },
    add_area_code = function(data, name_column, code_column) {
      data[[code_column]] <- dplyr::if_else(
        data[[name_column]] == "Spain",
        203L,
        NA_integer_
      )
      data
    }
  )

  result <- get_land_fp_production()

  testthat::expect_equal(nrow(result), 1)
  pointblank::expect_col_vals_in_set(
    result,
    impact,
    set = "Land"
  )
  pointblank::expect_col_vals_in_set(
    result,
    origin,
    set = "Production"
  )
  testthat::expect_type(result$year, "integer")
  testthat::expect_type(result$area_code, "integer")
  testthat::expect_type(result$item_cbs_code, "integer")
  testthat::expect_equal(result$area_code[[1]], 203L)
})
