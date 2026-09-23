# test_livestock_emissions.R --------------------------------------------------

# calculate_enteric_ch4 --------------------------------------------------------

testthat::test_that("auto tier selects Tier 2 when GE available", {
  result <- dairy_tier2_fixture() |>
    calculate_enteric_ch4()

  result |>
    pointblank::expect_col_exists("enteric_ch4_tier2")
})

testthat::test_that("auto tier selects Tier 1 when GE missing", {
  result <- single_tier1_fixture("Sheep", 100) |>
    calculate_enteric_ch4()

  result |>
    pointblank::expect_col_exists("enteric_ch4_tier1")
})

testthat::test_that("forced tier 1 uses Tier 1 even with GE", {
  result <- dairy_tier2_fixture() |>
    calculate_enteric_ch4(tier = 1)

  result |>
    pointblank::expect_col_exists("enteric_ch4_tier1")
})

# calculate_manure_emissions ---------------------------------------------------

testthat::test_that("calculate_manure_emissions includes CH4 and N2O", {
  result <- dairy_tier2_fixture() |>
    calculate_manure_emissions()

  result |>
    pointblank::expect_col_exists(
      c("manure_ch4_tier2", "manure_n2o_total")
    )
})

testthat::test_that("Tier 1 manure returns CH4 only", {
  result <- single_tier1_fixture("Sheep", 100) |>
    calculate_manure_emissions()

  result |>
    pointblank::expect_col_exists("manure_ch4_tier1")
})

# calculate_livestock_emissions ------------------------------------------------

testthat::test_that("full pipeline returns enteric + manure", {
  result <- dairy_tier2_fixture() |>
    calculate_livestock_emissions()

  testthat::expect_true(
    "enteric_ch4_tier2" %in%
      names(result) ||
      "enteric_ch4_tier1" %in% names(result)
  )
})

# Tier 2 species coverage (whep#1028) ------------------------------------------

.mixed_tier2_fixture <- function() {
  tibble::tribble(
    ~species,           ~cohort,        ~weight, ~diet_quality, ~heads,
    "Horses",           NA_character_,       NA, "Medium",        200,
    "Dairy Cattle",     "Adult Female",     600, "High",          100,
    "Pigs",             NA_character_,       NA, "Medium",       1000,
    "Chickens, layers", NA_character_,       NA, "Medium",       5000
  ) |>
    dplyr::mutate(milk_yield_kg_day = c(NA, 20, NA, NA))
}

testthat::test_that("Tier 2 gives uncovered species their Tier 1 value", {
  data <- .mixed_tier2_fixture()
  testthat::expect_message(
    tier2 <- whep::calculate_livestock_emissions(data, tier = 2),
    class = "whep_tier2_uncovered"
  )
  # Tier 1 puts cattle rows first, so align it on the species before comparing.
  tier1 <- whep::calculate_livestock_emissions(data, tier = 1)
  tier1 <- tier1[match(data$species, tier1$species), ]
  cattle_only <- whep::calculate_livestock_emissions(data[2, ], tier = 2)

  # Row order and count are preserved.
  testthat::expect_equal(tier2$species, data$species)
  tier2 |>
    pointblank::expect_col_vals_not_null("enteric_ch4_tier2") |>
    pointblank::expect_col_vals_not_null("manure_ch4_tier2") |>
    pointblank::expect_col_vals_not_null("manure_n2o_total")
  testthat::expect_equal(
    tier2$method_enteric,
    c("IPCC_2019_Tier1", "IPCC_2019_Tier2", rep("IPCC_2019_Tier1", 2))
  )
  uncovered <- c(1, 3, 4)
  testthat::expect_equal(
    tier2$enteric_ch4_tier2[uncovered],
    tier1$enteric_ch4_tier1[uncovered]
  )
  testthat::expect_equal(
    tier2$manure_ch4_tier2[uncovered],
    tier1$manure_ch4_tier1[uncovered]
  )
  testthat::expect_equal(
    tier2$manure_n2o_total[uncovered],
    tier1$manure_n2o_total[uncovered]
  )
  testthat::expect_true(all(
    tier2$method_manure_n2o[uncovered] == "IPCC_2019_Tier1"
  ))
  # The covered species is untouched by the split.
  testthat::expect_equal(
    tier2$enteric_ch4_tier2[2],
    cattle_only$enteric_ch4_tier2
  )
  testthat::expect_equal(
    tier2$manure_n2o_total[2],
    cattle_only$manure_n2o_total
  )
})

testthat::test_that("a covered-only herd stays silent at Tier 2", {
  testthat::expect_no_message(
    whep::calculate_livestock_emissions(dairy_tier2_fixture(), tier = 2)
  )
})

testthat::test_that("an all-uncovered herd needs no Tier 2 chain at all", {
  data <- .mixed_tier2_fixture()[c(1, 3), ]
  result <- suppressMessages(
    whep::calculate_livestock_emissions(data, tier = 2)
  )
  testthat::expect_true(all(result$method_enteric == "IPCC_2019_Tier1"))
  testthat::expect_true(all(result$enteric_ch4_tier2 > 0))
})

testthat::test_that("leave_na keeps the NA, and warns naming species", {
  testthat::expect_warning(
    result <- whep::calculate_livestock_emissions(
      .mixed_tier2_fixture(),
      tier = 2,
      options = list(tier2_uncovered = "leave_na")
    ),
    class = "whep_tier2_uncovered"
  )
  testthat::expect_true(all(is.na(result$enteric_ch4_tier2[c(1, 3, 4)])))
  testthat::expect_false(is.na(result$enteric_ch4_tier2[2]))
})

testthat::test_that("abort names the uncovered species", {
  err <- rlang::catch_cnd(
    whep::calculate_livestock_emissions(
      .mixed_tier2_fixture(),
      tier = 2,
      options = list(tier2_uncovered = "abort")
    ),
    classes = "error"
  )
  testthat::expect_s3_class(err, "whep_tier2_uncovered")
  msg <- conditionMessage(err)
  testthat::expect_match(msg, "Horses", fixed = TRUE)
  testthat::expect_match(msg, "Swine", fixed = TRUE)
  testthat::expect_match(msg, "Poultry", fixed = TRUE)
})

testthat::test_that("the enteric and manure wrappers cover the same way", {
  data <- .mixed_tier2_fixture()
  enteric <- suppressMessages(whep::calculate_enteric_ch4(data, tier = 2))
  manure <- suppressMessages(whep::calculate_manure_emissions(data, tier = 2))
  full <- suppressMessages(whep::calculate_livestock_emissions(data, tier = 2))

  testthat::expect_equal(enteric$enteric_ch4_tier2, full$enteric_ch4_tier2)
  testthat::expect_equal(manure$manure_ch4_tier2, full$manure_ch4_tier2)
  testthat::expect_equal(manure$manure_n2o_total, full$manure_n2o_total)
  testthat::expect_error(
    whep::calculate_enteric_ch4(
      data,
      tier = 2,
      options = list(tier2_uncovered = "abort")
    ),
    class = "whep_tier2_uncovered"
  )
})
