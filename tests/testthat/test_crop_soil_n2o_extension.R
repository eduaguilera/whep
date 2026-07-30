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
  # empty residues + manure by default, so the synthetic split is isolated
  primary_residues <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    item_cbs_code_crop = integer(),
    item_cbs_code_residue = integer(),
    value = numeric()
  )
  manure <- tibble::tibble(
    Item = character(),
    Element = character(),
    Year = integer(),
    `Area Code` = integer(),
    Value = numeric()
  )
  list(
    fertilizer = fertilizer,
    primary_prod = primary_prod,
    primary_residues = primary_residues,
    manure = manure
  )
}

testthat::test_that("crop/soil N2O example has expected structure", {
  result <- whep::build_crop_soil_n2o_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c(
      "year",
      "area_code",
      "item_cbs_code",
      "impact_u",
      "method_soil_n2o",
      "method_synthetic"
    )
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
})

testthat::test_that("synthetic N is split across crops by harvested area", {
  # Coello is the default crop split; this test pins the old uniform
  # behaviour, which is now the explicitly-selected "area_share" method
  # (REGRESSION requirement (c): area_share reproduces the harvested-area
  # split). The area_code here exists in whep::coello_synthetic_n, so the
  # default would rate-weight; forcing area_share isolates the 0.7 split.
  f <- .soil_n2o_fixture()
  f$synthetic_method <- "area_share"
  result <- whep::build_crop_soil_n2o_extension(
    data = f
  )

  testthat::expect_setequal(result$item_cbs_code, c(2511L, 2513L))
  testthat::expect_true(all(result$method_soil_n2o == "IPCC_2019_Tier1_AR6"))
  testthat::expect_true(all(result$method_synthetic == "area_share"))

  # synthetic factor: (EF1 + FracGASF*EF4 + FracLEACH*EF5) * 44/28 * 1000 * GWP
  per_t_n <- (0.010 + 0.11 * 0.010 + 0.24 * 0.011) * (44 / 28) * 1000 * 273
  testthat::expect_equal(sum(result$impact_u), 100000 * per_t_n)
  wheat <- result$impact_u[result$item_cbs_code == 2511L]
  barley <- result$impact_u[result$item_cbs_code == 2513L]
  testthat::expect_equal(wheat / (wheat + barley), 0.7)
})

testthat::test_that("soil-N2O synthetic split follows Coello rates", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2010L, 10L, 2511L, "ha", 100, # wheat
    2010L, 10L, 2514L, "ha", 100 # maize
  )
  fertilizer <- tibble::tribble(
    ~Element, ~Item, ~Year, ~`Area Code`, ~Value,
    "Agricultural Use", "Nutrient nitrogen N (total)", 2010L, 10L, 1000
  )
  coello <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~kg_n_ha,
    2010L, 10L, 2511L, 150,
    2010L, 10L, 2514L, 50
  )
  res <- whep::build_crop_soil_n2o_extension(
    data = list(
      primary_prod = primary_prod,
      fertilizer = fertilizer,
      coello_rates = coello,
      synthetic_method = "coello",
      manure = tibble::tibble(
        Item = character(),
        Element = character(),
        Year = integer(),
        `Area Code` = integer(),
        Value = double()
      ),
      primary_residues = tibble::tibble(
        year = integer(),
        area_code = integer(),
        item_cbs_code_crop = integer(),
        value = double()
      )
    )
  )
  wheat <- res$impact_u[res$item_cbs_code == 2511L]
  maize <- res$impact_u[res$item_cbs_code == 2514L]
  testthat::expect_gt(wheat, maize) # Coello wheat rate 3x maize
  testthat::expect_equal(wheat / (wheat + maize), 0.75, tolerance = 1e-8)
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

testthat::test_that("applied manure adds via the FracGASM volatilisation factor", {
  f <- .soil_n2o_fixture()
  f$fertilizer$Value <- 0 # isolate the manure contribution
  f$manure <- tibble::tribble(
    ~Item, ~Element, ~Year, ~`Area Code`, ~Value,
    "All Animals", "Manure applied to soils (N content)", 2010, 10, 1e7
  )
  result <- whep::build_crop_soil_n2o_extension(data = f)

  # FAOSTAT value is kg N -> /1000 = tonnes N, split 70/30 by area
  # manure factor: EF1 + FracGASM*EF4 + FracLEACH*EF5
  per_t_n <- (0.010 + 0.21 * 0.010 + 0.24 * 0.011) * (44 / 28) * 1000 * 273
  testthat::expect_equal(sum(result$impact_u), (1e7 / 1000) * per_t_n)
  wheat <- result$impact_u[result$item_cbs_code == 2511L]
  testthat::expect_equal(wheat / sum(result$impact_u), 0.7)
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
