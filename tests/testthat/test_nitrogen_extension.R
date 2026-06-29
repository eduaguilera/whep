testthat::test_that("build_nitrogen_extension example has expected structure", {
  result <- whep::build_nitrogen_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_nitrogen")
  )
  testthat::expect_true(all(result$method_nitrogen == "soil_n_surplus"))
})

# Shared crop inputs: wheat (non-legume, has production and area -> removal but
# no fixation), soyabeans (legume -> fixation and removal), and barley with an
# area row only (-> input allocation but no production, removal or fixation).
.nitrogen_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~unit, ~value,
    2000L, 2L, 15L, 2511L, "tonnes", 100,
    2000L, 2L, 15L, 2511L, "ha", 50,
    2000L, 2L, 236L, 2555L, "tonnes", 80,
    2000L, 2L, 236L, 2555L, "ha", 40,
    2000L, 2L, 44L, 2513L, "ha", 110
  )
}

.nitrogen_fertilizer <- function() {
  tibble::tribble(
    ~Year, ~`Area Code`, ~Element, ~Item, ~Value,
    2000L, 2L, "Agricultural Use", "Nutrient nitrogen N (total)", 20
  )
}

.nitrogen_manure <- function() {
  tibble::tribble(
    ~Year, ~`Area Code`, ~Element, ~Item, ~Value,
    2000L, 2L, "Manure applied to soils (N content)", "All Animals", 10000
  )
}

testthat::test_that("surplus sums soil-N inputs minus removal per sector", {
  result <- whep::build_nitrogen_extension(
    method = "surplus",
    data = list(
      primary_prod = .nitrogen_primary_prod(),
      fertilizer = .nitrogen_fertilizer(),
      manure = .nitrogen_manure(),
      primary_residues = tibble::tribble(
        ~year, ~area_code, ~item_cbs_code_crop, ~value,
        2000L, 2L, 2511L, 30000
      )
    )
  )

  # Barley has an area row only: no production (no removal), no fixation and no
  # residue, so its surplus is purely the area-allocated fertiliser and manure.
  # Area share = 110 / (50 + 40 + 110) = 0.55; inputs = (20 + 10) t * 0.55.
  barley <- dplyr::filter(result, .data$item_cbs_code == 2513L)
  testthat::expect_equal(barley$impact_u, (20 + 10) * 0.55 * 1000)
  # Wheat and soyabeans both carry inputs and product removal.
  testthat::expect_true(all(c(2511L, 2555L) %in% result$item_cbs_code))
  # Net soil-N mining is floored to zero, so every reported load is positive.
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
  testthat::expect_true(all(result$method_nitrogen == "soil_n_surplus"))
})

testthat::test_that("bnf covers legumes only", {
  result <- whep::build_nitrogen_extension(
    method = "bnf",
    data = list(primary_prod = .nitrogen_primary_prod())
  )

  # Only the legume (soyabeans, 2555) fixes nitrogen; the cereals do not appear.
  testthat::expect_setequal(result$item_cbs_code, 2555L)
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
  testthat::expect_true(all(result$method_nitrogen == "biological_n_fixation"))
})

testthat::test_that("build_nitrogen_extension rejects an unknown method", {
  testthat::expect_error(
    whep::build_nitrogen_extension(method = "leaching"),
    "method"
  )
})

testthat::test_that("build_nitrogen_extension rejects a bad removed fraction", {
  testthat::expect_error(
    whep::build_nitrogen_extension(residue_removed_frac = 1.2),
    "residue_removed_frac"
  )
})
