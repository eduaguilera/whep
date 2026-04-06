# test_livestock_manure.R ------------------------------------------------------

# .calc_manure_ch4_tier1 --------------------------------------------------------

testthat::test_that("Manure Tier 1 returns expected columns", {
  result <- tier1_fixture() |>
    whep:::.calc_manure_ch4_tier1()

  result |>
    pointblank::expect_col_exists(
      c("manure_ef_kgch4", "manure_ch4_tier1")
    )
})

testthat::test_that("Manure Tier 1 EFs match IPCC tables", {
  result <- tibble::tribble(
    ~species,       ~heads,
    "Dairy Cattle",   1,
    "Sheep",          1,
    "Horses",         1
  ) |>
    whep:::.calc_manure_ch4_tier1()

  efs <- result |> dplyr::pull(manure_ef_kgch4)
  # Dairy (Global): 36, Sheep: 0.19, Horses: 1.64
  testthat::expect_equal(efs, c(36, 0.19, 1.64))
})

testthat::test_that("Manure Tier 1 Swine subcategory aggregation", {
  result <- single_tier1_fixture("Swine", 1) |>
    whep:::.calc_manure_ch4_tier1()

  ef <- result |> dplyr::pull(manure_ef_kgch4)
  # Average of Market (6) and Breeding (6)
  testthat::expect_equal(ef, 6)
})

testthat::test_that("Manure Tier 1 Poultry exact subcategory match", {
  result <- single_tier1_fixture(
    "Poultry - Broilers",
    1
  ) |>
    whep:::.calc_manure_ch4_tier1()

  ef <- result |> dplyr::pull(manure_ef_kgch4)
  # Exact match: Poultry - Broilers = 0.02
  testthat::expect_equal(ef, 0.02)
})

testthat::test_that("Manure Tier 1 all species non-NA", {
  all_species <- tibble::tibble(
    species = c(
      "Dairy Cattle",
      "Beef Cattle",
      "Buffalo",
      "Sheep",
      "Goats",
      "Swine",
      "Horses",
      "Camels",
      "Mules and Asses"
    ),
    heads = rep(1, 9)
  )
  result <- all_species |>
    whep:::.calc_manure_ch4_tier1()

  result |>
    pointblank::expect_col_vals_not_null("manure_ef_kgch4")
})

# .calc_manure_ch4_tier2 --------------------------------------------------------

testthat::test_that("Manure Tier 2 returns expected columns", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_ch4_tier2()

  result |>
    pointblank::expect_col_exists(
      c(
        "VS",
        "Bo",
        "weighted_mcf",
        "manure_ch4_per_head",
        "manure_ch4_tier2"
      )
    )
})

testthat::test_that("Manure Tier 2 CH4 is reasonable for dairy", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_ch4_tier2()

  per_head <- result |> dplyr::pull(manure_ch4_per_head)
  # Typical range: 5-50 kg CH4/head/yr
  testthat::expect_gt(per_head, 1)
  testthat::expect_lt(per_head, 100)
})

testthat::test_that("VS is positive", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_ch4_tier2()

  vs <- result |> dplyr::pull(VS)
  testthat::expect_gt(vs, 0)
})

# .calc_manure_n2o --------------------------------------------------------------

testthat::test_that("N2O calculation returns expected columns", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_n2o()

  result |>
    pointblank::expect_col_exists(
      c(
        "Nex",
        "manure_n2o_direct",
        "manure_n2o_indirect",
        "manure_n2o_total"
      )
    )
})

testthat::test_that("N2O total = direct + indirect", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_n2o()

  direct <- result |> dplyr::pull(manure_n2o_direct)
  indirect <- result |> dplyr::pull(manure_n2o_indirect)
  total <- result |> dplyr::pull(manure_n2o_total)

  testthat::expect_equal(total, direct + indirect)
})

testthat::test_that("Nex is positive for dairy cattle", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_n2o()

  nex <- result |> dplyr::pull(Nex)
  testthat::expect_gt(nex, 0)
})

testthat::test_that("Nex is annualized (kgN/head/yr)", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_n2o()

  nex <- result |> dplyr::pull(Nex)
  # IPCC Table 10.19: N. America dairy ~100-140 kgN/head/yr.
  testthat::expect_gt(nex, 50)
  testthat::expect_lt(nex, 200)
})

testthat::test_that("Manure Tier 1 Buffalo uses Table 10.15 EF", {
  result <- single_tier1_fixture("Buffalo", 1) |>
    whep:::.calc_manure_ch4_tier1()

  ef <- result |> dplyr::pull(manure_ef_kgch4)
  # IPCC Table 10.15: Buffalo = 2
  testthat::expect_equal(ef, 2)
})
