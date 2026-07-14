# A tiny two-plus-item biomass_coefs fixture exercising each protein coalesce
# branch: Wheat carries Edible_N_kgFM, Maize only N_kgN_kgFM, Soybeans only
# Product_kgN_kgDM * Product_kgDM_kgFM, and Spices no protein at all (dropped).
# Wheat additionally carries an edible-portion gross energy so the energy
# coalesce (edible -> whole product) is exercised too.
.food_coefs <- function() {
  tibble::tribble(
    ~Name_biomass,
    ~Edible_N_kgFM,
    ~N_kgN_kgFM,
    ~Product_kgN_kgDM,
    ~Product_kgDM_kgFM,
    ~GE_product_edible_portion_MJ_kgFM,
    ~GE_product_MJ_kgFM,
    "Wheat", 0.02, 0.019, 0.020, 0.87, 13.0, 14.0,
    "Maize", NA_real_, 0.016, 0.018, 0.88, NA_real_, 15.0,
    "Soybeans", NA_real_, NA_real_, 0.050, 0.90, NA_real_, 16.0,
    "Spices", NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 8.0
  )
}

.food_items <- function() {
  tibble::tribble(
    ~item_cbs_code, ~Name_biomass,
    2511L, "Wheat",
    2513L, "Maize",
    2555L, "Soybeans",
    9999L, "Spices"
  )
}

# Area 10 mixes all three matched crops (sum + every coalesce branch); area 20
# isolates the N_kgN_kgFM branch; area 30 isolates the product-N branch.
.food_cbs <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~food_t,
    2010L, 10L, 2511L, 1000,
    2010L, 10L, 2513L, 500,
    2010L, 10L, 2555L, 200,
    2010L, 20L, 2513L, 600,
    2010L, 30L, 2555L, 400
  )
}

.food_pop <- function() {
  tibble::tribble(
    ~year, ~area_code, ~population,
    2010L, 10L, 10000,
    2010L, 20L, 8000,
    2010L, 30L, 4000
  )
}

.food_data <- function(cbs = .food_cbs()) {
  list(
    cbs_food = cbs,
    population = .food_pop(),
    biomass_coefs = .food_coefs(),
    items_full = .food_items()
  )
}

testthat::test_that("whep_native protein is sum(food_t * N * 6.25) per cap", {
  out <- whep::build_food_supply(method = "whep_native", data = .food_data())
  a10 <- dplyr::filter(out, area_code == 10)
  expected <- (1000 * 0.02 + 500 * 0.016 + 200 * (0.050 * 0.90)) *
    6.25 *
    1e6 /
    10000 /
    365
  testthat::expect_equal(a10$protein_g_cap_day, expected)
  # Realistic magnitude (tens of g protein per cap per day).
  testthat::expect_gt(a10$protein_g_cap_day, 40)
  testthat::expect_lt(a10$protein_g_cap_day, 90)
})

testthat::test_that("protein coalesce falls back N then product-N per item", {
  out <- whep::build_food_supply(data = .food_data())
  # Area 20: Edible_N is NA so the N_kgN_kgFM value (0.016) must be used.
  a20 <- dplyr::filter(out, area_code == 20)
  testthat::expect_equal(
    a20$protein_g_cap_day,
    600 * 0.016 * 6.25 * 1e6 / 8000 / 365
  )
  # Area 30: only Product_kgN_kgDM * Product_kgDM_kgFM is available (0.045).
  a30 <- dplyr::filter(out, area_code == 30)
  testthat::expect_equal(
    a30$protein_g_cap_day,
    400 * (0.050 * 0.90) * 6.25 * 1e6 / 4000 / 365
  )
})

testthat::test_that("energy uses MJ / 0.004184 and the edible->whole coalesce", {
  out <- whep::build_food_supply(data = .food_data())
  a10 <- dplyr::filter(out, area_code == 10)
  # Wheat uses the edible gross energy (13); Maize and Soybeans the whole one.
  expected <- (1000 * 13.0 + 500 * 15.0 + 200 * 16.0) *
    1000 /
    0.004184 /
    10000 /
    365
  testthat::expect_equal(a10$energy_kcal_cap_day, expected)
})

testthat::test_that("an unmatched food item warns and is excluded", {
  cbs <- dplyr::bind_rows(
    .food_cbs(),
    tibble::tibble(
      year = 2010L,
      area_code = 10L,
      item_cbs_code = 9999L,
      food_t = 300
    )
  )
  testthat::expect_warning(
    out <- whep::build_food_supply(data = .food_data(cbs)),
    "Excluding 1 food item"
  )
  # Spices (9999) has no protein coefficient, so area-10 protein is unchanged
  # from the matched-only fixture (the item is excluded, not zero-filled).
  matched <- whep::build_food_supply(data = .food_data())
  testthat::expect_equal(
    dplyr::filter(out, area_code == 10)$protein_g_cap_day,
    dplyr::filter(matched, area_code == 10)$protein_g_cap_day
  )
})

testthat::test_that("faostat_fbs passes the injected supply through", {
  fbs <- tibble::tribble(
    ~year, ~area_code, ~protein_g_cap_day, ~energy_kcal_cap_day, ~population,
    2010L, 10L, 80.0, 2900.0, 1e6,
    2010L, 32L, 55.0, 2400.0, 5e5
  )
  out <- whep::build_food_supply(
    method = "faostat_fbs",
    data = list(fbs_supply = fbs)
  )
  testthat::expect_equal(out, fbs)
})

testthat::test_that("build_food_supply rejects an unknown method", {
  testthat::expect_error(
    whep::build_food_supply(method = "not_a_method", data = list()),
    "arg_match|must be one of|not_a_method"
  )
})

testthat::test_that("build_food_supply aborts on a missing input column", {
  bad <- .food_data()
  bad$cbs_food <- dplyr::select(bad$cbs_food, -"food_t")
  testthat::expect_error(
    whep::build_food_supply(data = bad),
    "food_t"
  )
})

testthat::test_that("build_food_supply(example = TRUE) has the contract shape", {
  out <- whep::build_food_supply(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  pointblank::expect_col_exists(
    out,
    c(
      "year",
      "area_code",
      "protein_g_cap_day",
      "energy_kcal_cap_day",
      "population"
    )
  )
})
