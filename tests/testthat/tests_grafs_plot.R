library(dplyr)
library(testthat)

test_that(".create_land_df returns correct structure and values", {
  n_balance_mock <- tibble::tibble(
    Province_name = c("A", "A", "B", "B"),
    Year = c(2000, 2000, 2001, 2001),
    Irrig_cat = c("Irrigated", "Rainfed", "Irrigated", "Rainfed"),
    LandUse = c("Cropland", "Cropland", "Forest_low", "Forest_high"),
    Name_biomass = c("Wheat", "Apple", "Oak", "Plum"),
    Area_ygpit_ha = c(100, 200, 300, 400),
    Prod_MgN = c(10, 20, 30, 40),
    UsedResidue_MgN = c(1, 2, 3, 4),
    GrazedWeeds_MgN = c(0.5, 1, 1.5, 2)
  )

  assign("whep_read_file", function(x) n_balance_mock, envir = .GlobalEnv)

  df <- .create_land_df()

  expect_s3_class(df, "tbl_df")
  expect_true(all(
    c("province", "year", "label", "data", "align") %in% colnames(df)
  ))
  expect_true(all(df$align == "R"))
  expect_true(nrow(df) > 0)
  expect_true(all(df$data >= 0))
})
