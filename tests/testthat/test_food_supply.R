# A tiny biomass_coefs fixture exercising each protein branch: Wheat carries
# N_kgN_kgFM and is fully edible, Maize carries N_kgN_kgFM with an inedible
# fraction (so the edible scaling bites), Soybeans only
# Product_kgN_kgDM * Product_kgDM_kgFM, and Spices no nitrogen at all (dropped).
# Wheat additionally carries an edible-portion gross energy so the energy
# coalesce (edible -> whole product) is exercised too.
# Edible_N_kgFM is deliberately absent: it is empty in the real coefficients and
# build_food_supply() no longer reads it (#361).
.food_coefs <- function() {
  tibble::tribble(
    ~Name_biomass,
    ~N_kgN_kgFM,
    ~Product_kgN_kgDM,
    ~Product_kgDM_kgFM,
    ~Edible_portion,
    ~GE_product_edible_portion_MJ_kgFM,
    ~GE_product_MJ_kgFM,
    "Wheat", 0.019, 0.020, 0.87, 1.0, 13.0, 14.0,
    "Maize", 0.016, 0.018, 0.88, 0.75, NA_real_, 15.0,
    "Soybeans", NA_real_, 0.050, 0.90, 0.80, NA_real_, 16.0,
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

# Area 10 mixes all three matched crops (sum + every branch); area 20 isolates
# the N_kgN_kgFM branch with an inedible fraction; area 30 isolates the
# product-nitrogen branch.
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

# Protein grams per capita per day from summed nitrogen tonnes.
.food_g_day <- function(nitrogen_t, population) {
  nitrogen_t * 6.25 * 1e6 / population / 365
}

testthat::test_that("edible_portion scales nitrogen by the edible fraction", {
  out <- whep::build_food_supply(data = .food_data())
  a10 <- dplyr::filter(out, area_code == 10)
  expected <- .food_g_day(
    1000 * 0.019 * 1.0 + 500 * 0.016 * 0.75 + 200 * (0.050 * 0.90) * 0.80,
    10000
  )
  testthat::expect_equal(a10$protein_g_cap_day, expected)
  # Realistic magnitude (tens of g protein per cap per day).
  testthat::expect_gt(a10$protein_g_cap_day, 20)
  testthat::expect_lt(a10$protein_g_cap_day, 90)
})

testthat::test_that("whole_commodity reproduces the pre-#361 behaviour", {
  out <- whep::build_food_supply(
    data = .food_data(),
    protein_basis = "whole_commodity"
  )
  a10 <- dplyr::filter(out, area_code == 10)
  expected <- .food_g_day(
    1000 * 0.019 + 500 * 0.016 + 200 * (0.050 * 0.90),
    10000
  )
  testthat::expect_equal(a10$protein_g_cap_day, expected)
})

testthat::test_that("edible_portion never exceeds whole_commodity", {
  edible <- whep::build_food_supply(data = .food_data())
  whole <- whep::build_food_supply(
    data = .food_data(),
    protein_basis = "whole_commodity"
  )
  # Every fixture item has Edible_portion <= 1, so scaling can only reduce.
  testthat::expect_true(
    all(edible$protein_g_cap_day <= whole$protein_g_cap_day)
  )
  # Area 20 (Maize, Edible_portion 0.75) must strictly drop.
  testthat::expect_lt(
    dplyr::filter(edible, area_code == 20)$protein_g_cap_day,
    dplyr::filter(whole, area_code == 20)$protein_g_cap_day
  )
})

testthat::test_that("product_nitrogen ignores N_kgN_kgFM", {
  out <- whep::build_food_supply(
    data = .food_data(),
    protein_basis = "product_nitrogen"
  )
  # Area 20 is Maize: the agronomic product nitrogen (0.018 * 0.88), not the
  # 0.016 edible value, scaled by the 0.75 edible fraction.
  a20 <- dplyr::filter(out, area_code == 20)
  testthat::expect_equal(
    a20$protein_g_cap_day,
    .food_g_day(600 * (0.018 * 0.88) * 0.75, 8000)
  )
})

testthat::test_that("nitrogen falls back to product N when N_kgN_kgFM is NA", {
  out <- whep::build_food_supply(data = .food_data())
  # Area 30 is Soybeans: only Product_kgN_kgDM * Product_kgDM_kgFM exists,
  # scaled by the 0.80 edible fraction.
  a30 <- dplyr::filter(out, area_code == 30)
  testthat::expect_equal(
    a30$protein_g_cap_day,
    .food_g_day(400 * (0.050 * 0.90) * 0.80, 4000)
  )
})

testthat::test_that("a missing Edible_portion counts as fully edible", {
  data <- .food_data()
  data$biomass_coefs$Edible_portion <- NA_real_
  scaled <- whep::build_food_supply(data = data)
  whole <- whep::build_food_supply(
    data = .food_data(),
    protein_basis = "whole_commodity"
  )
  testthat::expect_equal(scaled$protein_g_cap_day, whole$protein_g_cap_day)
})

testthat::test_that("energy uses MJ / 0.004184 and the edible->whole coalesce", {
  out <- whep::build_food_supply(data = .food_data())
  a10 <- dplyr::filter(out, area_code == 10)
  # Wheat uses the edible gross energy (13); Maize and Soybeans the whole one.
  # Energy is not edible-scaled.
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
  # Spices (9999) has no nitrogen coefficient, so area-10 protein is unchanged
  # from the matched-only fixture (the item is excluded, not zero-filled).
  matched <- whep::build_food_supply(data = .food_data())
  testthat::expect_equal(
    dplyr::filter(out, area_code == 10)$protein_g_cap_day,
    dplyr::filter(matched, area_code == 10)$protein_g_cap_day
  )
})

testthat::test_that("an area with food but no population is named, not silent", {
  # The inner join in .food_per_capita() makes such an area vanish from the
  # output rather than appear wrong in it, which is why the loss has to be
  # reported here (#543). Area 18 is Bhutan, one of the 15 real areas the
  # gdp-population pin gives no denominator to.
  cbs <- dplyr::bind_rows(
    .food_cbs(),
    tibble::tibble(
      year = 2010L,
      area_code = 18L,
      item_cbs_code = 2511L,
      food_t = 800
    )
  )
  testthat::expect_warning(
    out <- whep::build_food_supply(data = .food_data(cbs)),
    "Bhutan \\(18, 1 area-year\\)"
  )
  # Reported AND still dropped: the warning is about visibility, and does not
  # invent a denominator.
  testthat::expect_false(18L %in% out$area_code)
  testthat::expect_setequal(out$area_code, c(10L, 20L, 30L))
})

testthat::test_that("the food-supply coverage warning can be switched off", {
  withr::local_options(whep.warn_missing_population = FALSE)
  cbs <- dplyr::bind_rows(
    .food_cbs(),
    tibble::tibble(
      year = 2010L,
      area_code = 18L,
      item_cbs_code = 2511L,
      food_t = 800
    )
  )
  testthat::expect_no_warning(whep::build_food_supply(data = .food_data(cbs)))
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
  testthat::expect_equal(
    dplyr::select(out, dplyr::all_of(names(fbs))),
    fbs
  )
  testthat::expect_true(all(out$method_food_supply == "faostat_fbs"))
  # The protein basis does not apply to a pass-through supply.
  testthat::expect_true(all(is.na(out$method_protein_basis)))
})

testthat::test_that("the chosen protein basis is recorded in the output", {
  bases <- c("edible_portion", "whole_commodity", "product_nitrogen")
  recorded <- purrr::map_chr(bases, function(basis) {
    out <- whep::build_food_supply(
      data = .food_data(),
      protein_basis = basis
    )
    unique(out$method_protein_basis)
  })
  testthat::expect_equal(recorded, bases)
})

testthat::test_that("build_food_supply rejects an unknown method", {
  testthat::expect_error(
    whep::build_food_supply(method = "not_a_method", data = list()),
    "arg_match|must be one of|not_a_method"
  )
})

testthat::test_that("build_food_supply rejects an unknown protein basis", {
  testthat::expect_error(
    whep::build_food_supply(data = .food_data(), protein_basis = "nope"),
    "arg_match|must be one of|nope"
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

testthat::test_that("build_food_supply aborts on a missing coefficient", {
  bad <- .food_data()
  bad$biomass_coefs <- dplyr::select(bad$biomass_coefs, -"Edible_portion")
  testthat::expect_error(
    whep::build_food_supply(data = bad),
    "Edible_portion"
  )
})

testthat::test_that("the packaged coefficients give the shipped protein", {
  # Every other arithmetic test here injects its own biomass_coefs fixture, so
  # a change to the PACKAGED coefficients would break none of them -- which is
  # exactly the surface #500 moves. This test runs the shipped lookup over
  # whep::biomass_coefs and whep::items_full and pins the four items that
  # dominate the WHEP-vs-FAOSTAT residual, so a coefficient edit has to be
  # deliberate and has to state its effect.
  lookup <- whep:::.food_nutrition_lookup(
    whep::items_full,
    whep::biomass_coefs,
    "edible_portion"
  )
  # which() rather than a bare logical: items_full carries one row with an NA
  # item_cbs_code, and NA == code subsets in an NA element rather than dropping
  # the row.
  protein_of <- function(code) {
    unique(lookup$protein_frac_kgfm[which(lookup$item_cbs_code == code)])
  }

  # kg protein per kg fresh matter = N_kgN_kgFM * 6.25 * Edible_portion.
  testthat::expect_equal(protein_of(2511), 0.11844577745690253) # Wheat
  testthat::expect_equal(protein_of(2807), 0.0743119266055046) # Rice, milled
  testthat::expect_equal(protein_of(2551), 0.2) # Nuts -> Almonds (#500)
  testthat::expect_equal(protein_of(2848), 0.033) # Milk excl. Butter
})

testthat::test_that("every food item resolves to one coefficient row", {
  # .food_nutrition_lookup() de-duplicates on Name_biomass because duplicate
  # names would fan out and double-count food_t. Pin that the bridge really is
  # one row per item, so a future items_full edit cannot silently reintroduce
  # the fan-out.
  lookup <- whep:::.food_nutrition_lookup(
    whep::items_full,
    whep::biomass_coefs,
    "edible_portion"
  )
  testthat::expect_equal(
    nrow(lookup),
    dplyr::n_distinct(lookup$item_cbs_code)
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
      "population",
      "method_food_supply",
      "method_protein_basis"
    )
  )
})
