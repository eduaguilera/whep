testthat::test_that("build_labour_extension example has expected structure", {
  result <- whep::build_labour_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_labour")
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
})

# Shared inputs: wheat (two harvested-area rows aggregating to one CBS item), a
# crop without a coefficient, cattle heads and grazed pasture (neither is crop
# harvested area, so neither contributes labour).
.labour_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~unit, ~value,
    2000L, 2L, 15L, 2511L, "ha", 100,
    2000L, 2L, 15L, 2511L, "ha", 50,
    2000L, 2L, 56L, 2514L, "ha", 200,
    2000L, 2L, 866L, 961L, "heads", 10,
    2000L, 2L, NA_integer_, 3000L, "ha", 5000
  )
}

.labour_crop_coef <- function() {
  tibble::tribble(
    ~crop_code,
    ~country_code,
    ~year,
    ~area_harvested_ha,
    ~hours_total,
    ~hours_child,
    15L,
    2L,
    2000L,
    100,
    30000,
    600
  )
}

testthat::test_that("total labour scales hours per hectare by whep area", {
  result <- whep::build_labour_extension(
    component = "total",
    data = list(
      primary_prod = .labour_primary_prod(),
      labour_crop = .labour_crop_coef()
    )
  )

  # Wheat total = (hours_total / source area) x whep area = 300 * 150.
  wheat <- dplyr::filter(result, .data$item_cbs_code == 2511L)
  testthat::expect_equal(wheat$impact_u, 45000)
  # Maize has no coefficient; cattle and grazed pasture are not crop area.
  testthat::expect_false(any(result$item_cbs_code %in% c(2514L, 961L, 3000L)))
  testthat::expect_true(all(result$method_labour == "GLD_total"))
})

testthat::test_that("component selects the matching hours tier", {
  result <- whep::build_labour_extension(
    component = "child",
    data = list(
      primary_prod = .labour_primary_prod(),
      labour_crop = .labour_crop_coef()
    )
  )

  # Wheat child = (hours_child / source area) x whep area = 6 * 150.
  wheat <- dplyr::filter(result, .data$item_cbs_code == 2511L)
  testthat::expect_equal(wheat$impact_u, 900)
  testthat::expect_true(all(result$method_labour == "GLD_child"))
})

testthat::test_that("build_labour_extension rejects an unknown component", {
  testthat::expect_error(
    whep::build_labour_extension(component = "managerial"),
    "component"
  )
})
