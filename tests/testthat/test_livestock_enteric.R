# test_livestock_enteric.R ----------------------------------------------------

# .calc_enteric_ch4_tier1 -------------------------------------------------------

testthat::test_that("Tier 1 returns expected columns", {
  result <- tier1_fixture() |>
    whep:::.calc_enteric_ch4_tier1()

  result |>
    pointblank::expect_col_exists(
      c("enteric_ef_kgch4", "enteric_ch4_tier1")
    )
})

testthat::test_that("Tier 1 EF matches IPCC Table 10.10/10.11", {
  result <- tibble::tribble(
    ~species,       ~heads,
    "Dairy Cattle",   1,
    "Sheep",          1,
    "Horses",         1
  ) |>
    whep:::.calc_enteric_ch4_tier1()

  efs <- result |> dplyr::pull(enteric_ef_kgch4)

  # Dairy (Global): 80, Sheep: 8, Horses: 18
  testthat::expect_equal(efs, c(80, 8, 18))
})

testthat::test_that("Tier 1 handles Swine subcategory aggregation", {
  result <- single_tier1_fixture("Swine", 1) |>
    whep:::.calc_enteric_ch4_tier1()

  ef <- result |> dplyr::pull(enteric_ef_kgch4)
  # Average of Swine-Market (1.5) and Swine-Breeding (1.5)
  testthat::expect_equal(ef, 1.5)
})

testthat::test_that("Tier 1 total equals heads * EF", {
  result <- single_tier1_fixture("Sheep", 5000) |>
    whep:::.calc_enteric_ch4_tier1()

  ef <- result |> dplyr::pull(enteric_ef_kgch4)
  total <- result |> dplyr::pull(enteric_ch4_tier1)
  testthat::expect_equal(total, ef * 5000)
})

testthat::test_that("Tier 1 all 10 species have non-NA EFs", {
  all_species <- tibble::tibble(
    species = c(
      "Dairy Cattle",
      "Beef Cattle",
      "Buffalo",
      "Sheep",
      "Goats",
      "Swine",
      "Poultry",
      "Horses",
      "Camels",
      "Mules and Asses"
    ),
    heads = rep(1, 10)
  )
  result <- all_species |>
    whep:::.calc_enteric_ch4_tier1()

  result |>
    pointblank::expect_col_vals_not_null("enteric_ef_kgch4")
})

testthat::test_that("Tier 1 Buffalo uses Table 10.11 EF", {
  result <- single_tier1_fixture("Buffalo", 1) |>
    whep:::.calc_enteric_ch4_tier1()

  ef <- result |> dplyr::pull(enteric_ef_kgch4)
  # IPCC Table 10.11: Buffalo = 55
  testthat::expect_equal(ef, 55)
})

# .calc_enteric_ch4_tier2 -------------------------------------------------------

testthat::test_that("Tier 2 enteric is in IPCC range for dairy", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_enteric_ch4_tier2()

  per_head <- result |> dplyr::pull(enteric_ch4_per_head)
  # IPCC Table 10.10 reference: ~117-128 kg CH4/head/yr
  testthat::expect_gt(per_head, 80)
  testthat::expect_lt(per_head, 180)
})

testthat::test_that("Tier 2 total equals heads * per_head", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_enteric_ch4_tier2()

  per_head <- result |> dplyr::pull(enteric_ch4_per_head)
  total <- result |> dplyr::pull(enteric_ch4_tier2)
  heads <- result |> dplyr::pull(heads)
  testthat::expect_equal(total, per_head * heads)
})

testthat::test_that("Tier 2 adds Method_Enteric column", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_enteric_ch4_tier2()

  result |>
    pointblank::expect_col_exists("Method_Enteric") |>
    pointblank::expect_col_vals_in_set(
      "Method_Enteric",
      c("IPCC_2019_Tier2")
    )
})
