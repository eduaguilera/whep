.ghg_prod_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 961L, "heads", 1000000,
    2000L, 10L, 976L, "heads", 5000000,
    2000L, 100L, 960L, "heads", 300000
  )
}

testthat::test_that("build_livestock_ghg_extension example has expected structure", {
  result <- whep::build_livestock_ghg_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_ghg")
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
  testthat::expect_true(all(result$method_ghg == "IPCC_2019_Tier1_AR6"))
})

testthat::test_that("Tier 1 keys output by live-animal sector and labels the method", {
  result <- whep::build_livestock_ghg_extension(
    tier = 1,
    gwp = "ar6",
    data = list(primary_prod = .ghg_prod_fixture())
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_ghg")
  )
  testthat::expect_setequal(result$item_cbs_code, c(960L, 961L, 976L))
  testthat::expect_true(all(result$impact_u > 0))
  testthat::expect_true(all(result$method_ghg == "IPCC_2019_Tier1_AR6"))
})

testthat::test_that("Tier 1 emissions scale linearly with head counts", {
  base <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = .ghg_prod_fixture())
  )
  doubled <- .ghg_prod_fixture() |>
    dplyr::mutate(value = value * 2)
  result <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = doubled)
  )

  joined <- dplyr::inner_join(
    base,
    result,
    by = c("year", "area_code", "item_cbs_code"),
    suffix = c("_base", "_double")
  )
  testthat::expect_equal(joined$impact_u_double, joined$impact_u_base * 2)
})

testthat::test_that("GWP standard rescales the footprint", {
  ar6 <- whep::build_livestock_ghg_extension(
    gwp = "ar6",
    data = list(primary_prod = .ghg_prod_fixture())
  )
  ar5 <- whep::build_livestock_ghg_extension(
    gwp = "ar5",
    data = list(primary_prod = .ghg_prod_fixture())
  )

  joined <- dplyr::inner_join(
    ar6,
    ar5,
    by = c("year", "area_code", "item_cbs_code"),
    suffix = c("_ar6", "_ar5")
  )
  ratio <- joined$impact_u_ar5 / joined$impact_u_ar6
  # Each sector's CO2e blends CH4 (AR5 28 vs AR6 27) and N2O (AR5 265 vs
  # AR6 273), so the ratio sits between the two gas ratios and is not 1.
  testthat::expect_true(all(
    ratio >= 265 / 273 - 1e-9 & ratio <= 28 / 27 + 1e-9
  ))
  testthat::expect_true(any(abs(ratio - 1) > 1e-6))
  testthat::expect_true(all(ar5$method_ghg == "IPCC_2019_Tier1_AR5"))
})

testthat::test_that("rows sharing a sector are summed", {
  prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 961L, "heads", 400000,
    2000L, 10L, 961L, "heads", 600000
  )
  combined <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = prod)
  )
  single <- whep::build_livestock_ghg_extension(
    data = list(
      primary_prod = dplyr::tibble(
        year = 2000L,
        area_code = 10L,
        item_cbs_code = 961L,
        unit = "heads",
        value = 1000000
      )
    )
  )

  testthat::expect_equal(nrow(combined), 1L)
  testthat::expect_equal(combined$impact_u, single$impact_u)
})

testthat::test_that("Tier 2 drops rows it cannot resolve instead of emitting NA", {
  result <- suppressWarnings(
    whep::build_livestock_ghg_extension(
      tier = 2,
      data = list(primary_prod = .ghg_prod_fixture())
    )
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_ghg")
  )
  testthat::expect_false(any(is.na(result$impact_u)))
})

testthat::test_that("tier must be 1 or 2", {
  testthat::expect_error(
    whep::build_livestock_ghg_extension(
      tier = 3,
      data = list(primary_prod = .ghg_prod_fixture())
    ),
    "tier"
  )
})
