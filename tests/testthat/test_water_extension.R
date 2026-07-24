testthat::test_that("build_water_extension example has expected structure", {
  result <- whep::build_water_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_water")
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
})

# Shared inputs: maize (two prod items aggregating to one CBS), a crop without a
# coefficient, cattle heads, grazed pasture and rotational fallow.
.water_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~unit, ~value,
    2000L, 2L, 56L, 2514L, "tonnes", 100,
    2000L, 2L, 56L, 2514L, "tonnes", 50,
    2000L, 2L, 15L, 2511L, "tonnes", 200,
    2000L, 2L, 866L, 961L, "heads", 10,
    2000L, 2L, NA_integer_, 3000L, "ha", 1000,
    2000L, 2L, NA_integer_, 3003L, "ha", 5000
  )
}

.water_crop_coef <- function() {
  tibble::tribble(
    ~crop_code, ~country_code, ~year, ~wfg_m3_t, ~wfb_cr_m3_t, ~wfb_i_m3_t,
    56L, 2L, 2000L, 300, 1, 2000
  )
}

testthat::test_that("blue water sums crop irrigation and livestock per head", {
  result <- whep::build_water_extension(
    component = "blue",
    data = list(
      primary_prod = .water_primary_prod(),
      crop_water = .water_crop_coef(),
      livestock_water = tibble::tribble(
        ~item_cbs_code, ~m3_per_head,
        961L, 25
      )
    )
  )

  # Maize blue = (wfb_i + wfb_cr) x aggregated production = 2001 * 150.
  maize <- dplyr::filter(result, .data$item_cbs_code == 2514L)
  testthat::expect_equal(maize$impact_u, 300150)
  # Cattle blue = m3_per_head x heads = 25 * 10.
  cattle <- dplyr::filter(result, .data$item_cbs_code == 961L)
  testthat::expect_equal(cattle$impact_u, 250)
  # Wheat has no coefficient and grazing is green-only: neither appears.
  testthat::expect_false(any(result$item_cbs_code %in% c(2511L, 3000L)))
  testthat::expect_true(all(result$method_water == "WFN_blue"))
})

testthat::test_that("green water sums crop rainfed and grazing per hectare", {
  result <- whep::build_water_extension(
    component = "green",
    data = list(
      primary_prod = .water_primary_prod(),
      crop_water = .water_crop_coef(),
      grazing_water = tibble::tribble(
        ~area_code, ~m3_per_ha,
        2L, 1500
      )
    )
  )

  # Maize green = wfg x aggregated production = 300 * 150.
  maize <- dplyr::filter(result, .data$item_cbs_code == 2514L)
  testthat::expect_equal(maize$impact_u, 45000)
  # Pasture green = m3_per_ha x grazed area = 1500 * 1000; fallow (3003) excluded.
  pasture <- dplyr::filter(result, .data$item_cbs_code == 3000L)
  testthat::expect_equal(pasture$impact_u, 1.5e6)
  testthat::expect_false(any(result$item_cbs_code %in% c(961L, 3003L)))
  testthat::expect_true(all(result$method_water == "WFN_green"))
})

testthat::test_that("build_water_extension rejects an unknown component", {
  testthat::expect_error(
    whep::build_water_extension(component = "grey"),
    "component"
  )
})
