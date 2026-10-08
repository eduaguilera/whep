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

testthat::test_that("species with no GLEAM cohorts keep their whole herd", {
  # whep#1028: horses, asses, mules and camels have no production-system or
  # cohort rows in the GLEAM tables. The expansion used to leave each of them
  # one row with `cohort_heads = NA`, so every downstream count
  # (`.animal_count()`) lost the whole herd. A species the tables cannot split
  # stays one row holding all of its heads.
  herd <- tibble::tribble(
    ~species,  ~heads,
    "Horses",    1000,
    "Asses",      200,
    "Mules",       50,
    "Camels",     300,
    "Sheep",      500
  )
  result <- whep::calculate_cohorts_systems(herd)

  result |>
    pointblank::expect_col_vals_not_null("cohort_heads") |>
    pointblank::expect_col_vals_not_null("cohort_fraction")
  totals <- result |>
    dplyr::summarise(cohort_heads = sum(cohort_heads), .by = species)
  testthat::expect_equal(
    totals$cohort_heads[match(herd$species, totals$species)],
    herd$heads
  )
  unsplit <- dplyr::filter(result, species != "Sheep")
  testthat::expect_equal(nrow(unsplit), 4L)
  testthat::expect_true(all(unsplit$cohort_fraction == 1))
})

testthat::test_that("a species absent from supplied shares keeps its herd", {
  custom <- tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Cattle", "Dairy", 0.5,
    "Cattle", "Beef", 0.5
  )
  result <- tibble::tibble(species = c("Cattle", "Sheep"), heads = 1000) |>
    whep::calculate_cohorts_systems(system_shares = custom)

  testthat::expect_equal(sum(result$cohort_heads), 2000, tolerance = 1)
})

# whep#1472: milk is what milked females give, so cohort expansion moves a
# herd's milk onto the dairy system's "Adult Female" and conserves it.
test_that("cohort expansion puts a herd's milk on the milked cohort", {
  herd <- tibble::tribble(
    ~species,  ~heads, ~milk_yield_kg_day,
    "Sheep",   1000,   0.5,
    "Goats",   400,    NA
  )
  result <- calculate_cohorts_systems(herd)
  sheep <- dplyr::filter(result, species == "Sheep")
  milked <- sheep$system == "Dairy" & sheep$cohort == "Adult Female"

  expect_equal(sum(milked), 1L)
  expect_equal(
    sheep$milk_yield_kg_day[milked],
    0.5 / sheep$cohort_fraction[milked]
  )
  expect_true(all(sheep$milk_yield_kg_day[!milked] == 0))
  expect_equal(sum(sheep$cohort_heads * sheep$milk_yield_kg_day), 1000 * 0.5)
  expect_equal(
    sort(unique(sheep$method_milk_yield)),
    c("milked_cohort", "not_milked_cohort")
  )
  # A herd with no milk yield is not given one.
  goats <- dplyr::filter(result, species == "Goats")
  expect_true(all(is.na(goats$milk_yield_kg_day)))
  expect_true(all(is.na(goats$method_milk_yield)))
  expect_false(rlang::has_name(result, ".herd_row"))
})

test_that("milk follows supplied system shares and keeps its total", {
  shares <- tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Buffalo",    "Dairy", 0.9,
    "Buffalo",    "Other", 0.1
  )
  result <- tibble::tibble(
    species = "Buffalo",
    heads = 200,
    milk_yield_kg_day = 3
  ) |>
    calculate_cohorts_systems(system_shares = shares)

  expect_equal(sum(result$cohort_heads * result$milk_yield_kg_day), 200 * 3)
  expect_equal(
    result$milk_yield_kg_day[result$cohort == "Adult Female"],
    3 / (0.9 * 0.5)
  )
})

test_that("a species with no dairy system keeps the milk it was given", {
  # Pigs have cohorts but no dairy system, so there is nothing to move the
  # column onto; it is left as the caller supplied it.
  result <- tibble::tibble(
    species = "Pigs",
    heads = 100,
    milk_yield_kg_day = 1
  ) |>
    calculate_cohorts_systems()

  expect_true(all(result$milk_yield_kg_day == 1))
  expect_true(all(result$method_milk_yield == "whole_herd"))
})

test_that("a milked species whose herd has no milked cohort aborts", {
  shares <- tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Goats",      "Meat",  1
  )
  goats <- tibble::tibble(species = "Goats", heads = 100, milk_yield_kg_day = 1)

  expect_error(
    calculate_cohorts_systems(goats, system_shares = shares),
    class = "whep_milk_without_milked_cohort"
  )
  # No milk, nothing to strand.
  expect_no_error(
    calculate_cohorts_systems(
      dplyr::mutate(goats, milk_yield_kg_day = 0),
      system_shares = shares
    )
  )
})
