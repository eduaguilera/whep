.soil_n2o_fixture <- function() {
  fertilizer <- tibble::tribble(
    ~Element, ~Item, ~Year, ~`Area Code`, ~Value,
    "Agricultural Use", "Nutrient nitrogen N (total)", 2010, 10, 100000,
    "Agricultural Use", "Nutrient phosphate P2O5 (total)", 2010, 10, 5000
  )
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2010L, 10L, 2511L, "ha", 7000,
    2010L, 10L, 2513L, "ha", 3000,
    2010L, 10L, 3000L, "ha", 9999,
    2010L, 10L, 2511L, "tonnes", 42
  )
  # empty residues by default, so the synthetic split is isolated
  primary_residues <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    item_cbs_code_crop = integer(),
    item_cbs_code_residue = integer(),
    value = numeric()
  )
  list(
    fertilizer = fertilizer,
    primary_prod = primary_prod,
    primary_residues = primary_residues
  )
}

testthat::test_that("crop/soil N2O example has expected structure", {
  result <- whep::build_crop_soil_n2o_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_soil_n2o")
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
})

testthat::test_that("synthetic N is split across crops by harvested area", {
  f <- .soil_n2o_fixture()
  result <- whep::build_crop_soil_n2o_extension(
    data = f
  )

  testthat::expect_setequal(result$item_cbs_code, c(2511L, 2513L))
  testthat::expect_true(all(result$method_soil_n2o == "IPCC_2019_Tier1_AR6"))

  # synthetic factor: (EF1 + FracGASF*EF4 + FracLEACH*EF5) * 44/28 * 1000 * GWP
  per_t_n <- (0.010 + 0.11 * 0.010 + 0.24 * 0.011) * (44 / 28) * 1000 * 273
  testthat::expect_equal(sum(result$impact_u), 100000 * per_t_n)
  wheat <- result$impact_u[result$item_cbs_code == 2511L]
  barley <- result$impact_u[result$item_cbs_code == 2513L]
  testthat::expect_equal(wheat / (wheat + barley), 0.7)
})

testthat::test_that("residue N adds via the leaching-only factor", {
  f <- .soil_n2o_fixture()
  f$fertilizer$Value <- 0 # isolate the residue contribution
  f$primary_residues <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_crop, ~item_cbs_code_residue, ~value,
    2010L, 10L, 2511L, 2105L, 1000
  )
  result <- whep::build_crop_soil_n2o_extension(
    residue_removed_frac = 0.45,
    data = f
  )

  # residue factor excludes volatilisation (Eq 11.9): EF1 + FracLEACH*EF5
  n_t <- 1000 * 0.006 * (1 - 0.45) # residue DM * N_AG(wheat) * (1 - removed)
  expected <- n_t * (0.010 + 0.24 * 0.011) * (44 / 28) * 1000 * 273
  testthat::expect_equal(
    result$impact_u[result$item_cbs_code == 2511L],
    expected
  )
})

testthat::test_that("GWP standard rescales soil N2O proportionally", {
  f <- .soil_n2o_fixture()
  ar6 <- whep::build_crop_soil_n2o_extension(gwp = "ar6", data = f)
  ar5 <- whep::build_crop_soil_n2o_extension(gwp = "ar5", data = f)
  testthat::expect_equal(sum(ar5$impact_u) / sum(ar6$impact_u), 265 / 273)
})

testthat::test_that("residue_removed_frac is validated", {
  f <- .soil_n2o_fixture()
  testthat::expect_error(
    whep::build_crop_soil_n2o_extension(residue_removed_frac = 1, data = f),
    "residue_removed_frac"
  )
})
