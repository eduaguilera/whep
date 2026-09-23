# test_livestock_cohorts.R ------------------------------------------------------

testthat::test_that("calculate_cohorts_systems expands rows", {
  input <- tibble::tibble(
    species = "Dairy Cattle",
    heads = 1000
  )
  result <- calculate_cohorts_systems(input)

  # Should have more rows than input (expanded by systems/cohorts)
  testthat::expect_gt(nrow(result), nrow(input))
})

testthat::test_that("cohort heads sum to original heads", {
  input <- tibble::tibble(
    species = "Dairy Cattle",
    heads = 1000
  )
  result <- calculate_cohorts_systems(input)

  total_heads <- sum(result$cohort_heads, na.rm = TRUE)
  testthat::expect_equal(total_heads, 1000, tolerance = 1)
})

testthat::test_that("result has required columns", {
  input <- tibble::tibble(
    species = "Sheep",
    heads = 500
  )
  result <- calculate_cohorts_systems(input)

  result |>
    pointblank::expect_col_exists(
      c("system", "cohort", "cohort_heads")
    )
})

testthat::test_that("dairy commodity routes only to the Dairy system", {
  # Regression for #109: the cohort split must follow the commodity, not just
  # the general species. A "Cattle, dairy" herd is entirely dairy-system cohorts.
  result <- tibble::tibble(species = "Cattle, dairy", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), "Dairy")
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("non-dairy commodity routes only to the Beef system", {
  result <- tibble::tibble(species = "Cattle, non-dairy", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), "Beef")
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("single-commodity species keep the generic system blend", {
  # "Buffalo" is one commodity for all buffalo, so it must not collapse to a
  # single system the way the cattle dairy/non-dairy commodities do.
  result <- tibble::tibble(species = "Buffalo", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), c("Dairy", "Other"))
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("market swine route only to the Fattening system", {
  # whep#1107: FAOSTAT reports swine as two disjoint stock items, 1049
  # "Swine, market" (animals_codes name "Pigs") and 1051 "Swine, breeding"
  # ("Hogs"). Once both reach production the herd is already split, so the
  # assumed Breeding 0.15 / Fattening 0.85 blend must not be applied on top:
  # that would book 15% of the reported market herd as breeding as well.
  result <- tibble::tibble(species = "Pigs", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), "Fattening")
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("breeding swine route only to the Breeding system", {
  result <- tibble::tibble(species = "Hogs", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), "Breeding")
  testthat::expect_setequal(unique(result$cohort), c("Sows", "Boars"))
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("the two swine items cover the herd exactly once", {
  # The invariant the split has to satisfy: the breeding cohorts of the two
  # items together hold the reported breeding herd and nothing else. Before
  # whep#1107 the market herd alone produced a breeding cohort of fifteen per
  # cent of its own head count, and the reported breeding herd was absent.
  result <- tibble::tribble(
    ~species, ~heads,
    "Pigs",      900,
    "Hogs",      100
  ) |>
    whep::calculate_cohorts_systems()

  breeding <- result |>
    dplyr::filter(.data$system == "Breeding") |>
    dplyr::pull(.data$cohort_heads) |>
    sum()

  testthat::expect_equal(breeding, 100, tolerance = 1)
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("supplied system_shares bypass commodity routing", {
  custom <- tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Cattle", "Dairy", 0.5,
    "Cattle", "Beef", 0.5
  )
  result <- tibble::tibble(species = "Cattle, dairy", heads = 1000) |>
    whep::calculate_cohorts_systems(system_shares = custom)

  testthat::expect_setequal(unique(result$system), c("Dairy", "Beef"))
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("chicken layers and broilers route to their own system", {
  # whep#1194: production splits the FAOSTAT chicken stock into 1052
  # "Chickens, layers" and 1053 "Chickens, broilers" by the reported
  # emissions-domain stock items, exactly as it does for swine. Applying the
  # assumed Layers 0.50 / Broilers 0.50 blend on top booked half of each
  # reported flock in the other system.
  result <- tibble::tribble(
    ~species,             ~heads,
    "Chickens, layers",      300,
    "Chickens, broilers",    700
  ) |>
    whep::calculate_cohorts_systems()

  by_system <- result |>
    dplyr::summarise(
      heads = sum(.data$cohort_heads),
      .by = c("species", "system")
    ) |>
    dplyr::arrange(.data$species)

  testthat::expect_equal(
    by_system,
    tibble::tribble(
      ~species,             ~system,    ~heads,
      "Chickens, broilers", "Broilers",    700,
      "Chickens, layers",   "Layers",      300
    )
  )
})

testthat::test_that("method_system_share records where each split came from", {
  # whep#1194: the default shares are unsourced, so every expanded row says
  # whether its system split was reported (the commodity names the system),
  # assumed (WHEP's unverified default blend) or supplied by the caller.
  result <- tibble::tribble(
    ~species,           ~heads,
    "Cattle, dairy",       100,
    "Chickens, layers",    100,
    "Sheep",               100,
    "Ducks",               100
  ) |>
    whep::calculate_cohorts_systems()

  methods <- result |>
    dplyr::distinct(.data$species, .data$method_system_share) |>
    dplyr::arrange(.data$species)

  testthat::expect_equal(
    methods,
    tibble::tribble(
      ~species,           ~method_system_share,
      "Cattle, dairy",    "reported",
      "Chickens, layers", "reported",
      "Ducks",            "assumed",
      "Sheep",            "assumed"
    )
  )

  custom <- tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Sheep",      "Meat",  1
  )
  supplied <- tibble::tibble(species = "Sheep", heads = 100) |>
    whep::calculate_cohorts_systems(system_shares = custom)
  testthat::expect_setequal(supplied$method_system_share, "supplied")
})
