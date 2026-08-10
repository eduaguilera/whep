testthat::test_that("build_water_extension example has expected structure", {
  result <- whep::build_water_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c(
      "year",
      "area_code",
      "item_cbs_code",
      "impact_u",
      "method_water",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry"
    )
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
})

# Shared inputs: maize (two prod items aggregating to one CBS), a crop without a
# coefficient, cattle heads, grazed pasture and rotational fallow.
# Carries both area codes, as get_primary_production() does: coefficients join
# on the legacy area_code, output is keyed on polity_area_code.
.water_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~unit, ~value,
    2000L, 2L, 56L, 2514L, "tonnes", 100,
    2000L, 2L, 56L, 2514L, "tonnes", 50,
    2000L, 2L, 15L, 2511L, "tonnes", 200,
    2000L, 2L, 866L, 961L, "heads", 10,
    2000L, 2L, NA_integer_, 3000L, "ha", 1000,
    2000L, 2L, NA_integer_, 3003L, "ha", 5000
  ) |>
    dplyr::mutate(polity_area_code = .data$area_code)
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
        ~year, ~area_code, ~m3_per_ha,
        2000L, 2L, 1500,
        2001L, 2L, 9999
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

testthat::test_that("water is summed onto the polity, not the legacy area", {
  # Two legacy reporting areas (a split territory) sharing one polity bucket.
  # Their coefficients differ, which is the point: an intensity cannot be
  # averaged across them without a weight, but the resulting cubic metres add.
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~polity_area_code, ~item_prod_code, ~item_cbs_code,
    ~unit, ~value,
    2000L, 2L, 2L, 56L, 2514L, "tonnes", 100,
    2000L, 277L, 2L, 56L, 2514L, "tonnes", 400
  )
  crop_coef <- tibble::tribble(
    ~crop_code, ~country_code, ~year, ~wfg_m3_t, ~wfb_cr_m3_t, ~wfb_i_m3_t,
    56L, 2L, 2000L, 300, 0, 1000,
    56L, 277L, 2000L, 300, 0, 10
  )

  result <- whep::build_water_extension(
    component = "blue",
    data = list(
      primary_prod = primary_prod,
      crop_water = crop_coef,
      livestock_water = tibble::tribble(
        ~item_cbs_code, ~m3_per_head,
        961L, 25
      )
    )
  )

  # One row for the shared polity, not one per legacy area.
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$area_code, 2L)
  # 100 t x 1000 m3/t + 400 t x 10 m3/t = 104,000 m3. A mean intensity would
  # have given 505 m3/t x 500 t = 252,500 m3 instead.
  testthat::expect_equal(result$impact_u, 104000, tolerance = 1e-8)
})

testthat::test_that("grazing water uses the coefficient of the row's own year", {
  # The 2001 coefficient is deliberately far from 2000's: if the join ignored
  # the year, 2000 pasture would pick up the wrong one (or duplicate rows).
  prod <- tibble::tribble(
    ~year, ~area_code, ~polity_area_code, ~item_prod_code, ~item_cbs_code,
    ~unit, ~value,
    2000L, 2L, 2L, NA_integer_, 3000L, "ha", 1000,
    2001L, 2L, 2L, NA_integer_, 3000L, "ha", 1000
  )
  coef <- tibble::tribble(
    ~year, ~area_code, ~m3_per_ha,
    2000L, 2L, 1500,
    2001L, 2L, 4000
  )

  result <- whep::build_water_extension(
    component = "green",
    data = list(
      primary_prod = prod,
      crop_water = .water_crop_coef(),
      grazing_water = coef
    )
  ) |>
    dplyr::arrange(year)

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_equal(result$impact_u, c(1.5e6, 4.0e6))
})
