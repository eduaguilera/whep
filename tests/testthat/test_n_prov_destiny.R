# Tests for n_prov_destiny.R functions
testthat::local_edition(3)

# .remove_seeds_from_system
testthat::test_that(".remove_seeds_from_system subtracts seed and applies 50% cap", {
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Area_ygpit_ha, ~LandUse,
    2000, "A", "Wheat", 100, "Cropland",
    2000, "B", "Wheat", 10,  "Cropland"
  )

  pie_seed <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Seed", 220
  )

  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~production_fm,
    2000, "A", "Wheat", 300,
    2000, "B", "Wheat", 30
  )

  res <- .remove_seeds_from_system(npp, pie_seed, prod)

  summed <- res |>
    dplyr::group_by(Province_name) |>
    dplyr::summarise(production_fm = sum(production_fm), .groups = "drop")

  testthat::expect_equal(
    summed$production_fm[summed$Province_name == "A"],
    150
  )

  testthat::expect_equal(
    summed$production_fm[summed$Province_name == "B"],
    15
  )
})

test_that("remove_seeds_from_system caps seeds at 50% when seeds exceed production", {
  npp <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Area_ygpit_ha, ~LandUse,
    2000, "A", "Wheat", 1, "Cropland"
  )

  pie_seed <- tibble::tribble(
    ~Year, ~Item, ~Element, ~Destiny, ~Value_destiny,
    2000, "Wheat", "Domestic_supply", "Seed", 1000
  )

  prod <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~production_fm,
    2000, "A", "Wheat", 10
  )

  out <- .remove_seeds_from_system(npp, pie_seed, prod)

  expect_equal(out$production_fm, 5)
})


# .split_import_consumption
testthat::test_that(".split_import_consumption limits imports and splits feed correctly", {
  local_vs_import <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Box,
    ~local_consumption, ~import_consumption,
    ~food_share, ~feed_share, ~other_uses_share,
    2000, "A", "Wheat",    "Cropland", 50, 30, 0.6, 0.3, 0.1,
    2000, "A", "FishProd", "Fish",      20, 10, 0.5, 0.4, 0.1
  )

  feed_share_rum_mono <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~share_rum, ~share_mono,
    2000, "A", "Wheat",    0.7, 0.3,
    2000, "A", "FishProd", 0.6, 0.4
  )

  out <- .split_import_consumption(local_vs_import, feed_share_rum_mono)

  # Wheat: food (pmin applies)
  wheat_food <- out |>
    dplyr::filter(Item == "Wheat", Destiny == "population_food")

  testthat::expect_equal(sum(wheat_food$MgN), 18, tolerance = 1e-12)
  testthat::expect_equal(unique(wheat_food$Origin), "Outside")
  testthat::expect_true(all(is.na(wheat_food$Irrig_cat)))

  # Wheat: feed split
  wheat_feed <- out |>
    dplyr::filter(
      Item == "Wheat",
      Destiny %in% c("livestock_rum", "livestock_mono")
    ) |>
    dplyr::group_by(Destiny) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    tibble::deframe()

  testthat::expect_equal(
    unname(wheat_feed["livestock_rum"]),
    9 * 0.7,
    tolerance = 1e-12
  )

  testthat::expect_equal(
    unname(wheat_feed["livestock_mono"]),
    9 * 0.3,
    tolerance = 1e-12
  )

  # Fish: food (no pmin)
  fish_food <- out |>
    dplyr::filter(Item == "FishProd", Destiny == "population_food") |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    dplyr::pull(MgN)

  testthat::expect_equal(fish_food, 10 * 0.5, tolerance = 1e-12)
})

# .add_n_soil_inputs
testthat::test_that(".add_n_soil_inputs pivots soil inputs and preserves totals", {
  base <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat,
    2000, "A", "Wheat", "irrig"
  )

  soil_inputs <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland", 1, 2, 3, 4, 5
  )

  out <- .add_n_soil_inputs(base, soil_inputs)

  sums <- out |>
    dplyr::filter(Item == "Wheat") |>
    dplyr::group_by(Origin) |>
    dplyr::summarise(MgN = sum(MgN), .groups = "drop") |>
    tibble::deframe()

  testthat::expect_setequal(
    names(sums),
    c("Deposition", "Fixation", "Synthetic", "Livestock", "People")
  )

  testthat::expect_equal(
    unname(sums[c(
      "Deposition",
      "Fixation",
      "Synthetic",
      "Livestock",
      "People"
    )]),
    c(1, 2, 3, 4, 5)
  )
})

test_that("add_n_soil_inputs does not create duplicate flows", {
  base <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat,
    2000, "A", "Wheat", "irrig"
  )

  soil_inputs <- tibble::tribble(
    ~Year, ~Province_name, ~Item, ~Irrig_cat, ~Box,
    ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    2000, "A", "Wheat", "irrig", "Cropland", 1, 2, 3, 4, 5
  )

  out <- .add_n_soil_inputs(base, soil_inputs)

  expect_false(any(duplicated(
    out |> dplyr::select(Year, Province_name, Item, Origin, Destiny)
  )))
})
