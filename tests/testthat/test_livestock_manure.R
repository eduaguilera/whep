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

testthat::test_that("Manure Tier 1 uses regional EF when iso3 is supplied", {
  result <- tibble::tribble(
    ~species, ~heads, ~iso3,
    "Dairy Cattle", 1, "DEU"
  ) |>
    whep:::.calc_manure_ch4_tier1()

  ef <- result |> dplyr::pull(manure_ef_kgch4)
  # DEU -> Western Europe dairy cattle, climate-averaged: mean(31, 39) = 35.
  # The Global fallback would instead give 36.
  testthat::expect_equal(ef, 35)
  testthat::expect_false("region" %in% names(result))
})

# .calc_manure_ch4_tier2 --------------------------------------------------------

testthat::test_that("Manure Tier 2 returns expected columns", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_ch4_tier2()

  result |>
    pointblank::expect_col_exists(
      c(
        "volatile_solids",
        "methane_potential",
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

  vs <- result |> dplyr::pull(volatile_solids)
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
        "n_excretion",
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

  nex <- result |> dplyr::pull(n_excretion)
  testthat::expect_gt(nex, 0)
})

testthat::test_that("Nex is annualized (kgN/head/yr)", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_manure_n2o()

  nex <- result |> dplyr::pull(n_excretion)
  # IPCC Table 10.19: N. America dairy ~100-140 kgN/head/yr.
  testthat::expect_gt(nex, 50)
  testthat::expect_lt(nex, 200)
})

testthat::test_that("Manure Tier 2 scales cohort rows by cohort_heads", {
  # Regression for #106: like enteric CH4, manure CH4 and N2O totals must scale
  # by the cohort's own head count, not the national `heads` each expanded row
  # still carries.
  result <- tibble::tibble(
    species = "Cattle, dairy",
    heads = 1000,
    iso3 = "DEU",
    milk_yield_kg_day = 20,
    diet_quality = "High"
  ) |>
    whep::calculate_cohorts_systems() |>
    whep::estimate_energy_demand() |>
    whep:::.calc_manure_ch4_tier2() |>
    whep:::.calc_manure_n2o()

  testthat::expect_equal(
    result$manure_ch4_tier2,
    result$cohort_heads * result$manure_ch4_per_head
  )
  # Aggregated to the herd, manure N2O stays in a realistic per-head range; it
  # was inflated by the cohort count (~11x) when scaled by national heads.
  per_head_n2o <- sum(result$manure_n2o_total) / 1000
  testthat::expect_gt(per_head_n2o, 0.5)
  testthat::expect_lt(per_head_n2o, 10)
})

testthat::test_that("Manure Tier 1 Buffalo uses Table 10.15 EF", {
  result <- single_tier1_fixture("Buffalo", 1) |>
    whep:::.calc_manure_ch4_tier1()

  ef <- result |> dplyr::pull(manure_ef_kgch4)
  # IPCC Table 10.15: Buffalo = 2
  testthat::expect_equal(ef, 2)
})

# .calc_volatile_solids ---------------------------------------------------------

testthat::test_that("Volatile solids match IPCC 2019 Eq 10.24 (#160)", {
  # Eq 10.24: VS = GE * [(1 - DE/100) + UE] * (1 - ASH/100) / 18.45.
  # UE (urinary energy fraction of GE) is additive, not scaled by DE.
  # GE = 200, DE = 65 %, UE = 0.04, ASH = 8 % (Cattle), factor = 18.45:
  #   200 * (0.35 + 0.04) * 0.92 / 18.45 = 3.889431.
  result <- tibble::tribble(
    ~species,       ~species_gen, ~gross_energy, ~de_percent,
    "Dairy Cattle", "Cattle",     200,           65
  ) |>
    whep:::.calc_volatile_solids()

  expected <- 200 * (1 - 65 / 100 + 0.04) * (1 - 8 / 100) / 18.45
  testthat::expect_equal(result$volatile_solids, expected)
  testthat::expect_equal(result$volatile_solids, 3.889431, tolerance = 1e-6)
  # The pre-fix formula scaled UE by DE, giving ~3.7503; guard against it.
  buggy <- 200 * (1 - 65 / 100 + 0.04 * 65 / 100) * (1 - 8 / 100) / 18.45
  testthat::expect_false(isTRUE(all.equal(result$volatile_solids, buggy)))
})

# .calc_weighted_mcf ------------------------------------------------------------

testthat::test_that("Weighted MCF falls back to Global MMS mix (#201)", {
  # "Africa" has no region-specific rows in regional_mms_distribution, so the
  # Global Cattle distribution must be used instead of the flat 2% default.
  # Global Cattle mix x Temperate MCF (Table 10.17):
  #   0.50*1.5 + 0.30*4.0 + 0.15*35.0 + 0.05*0.5 = 7.225 % -> 0.07225.
  result <- tibble::tribble(
    ~species_gen, ~region,  ~climate_zone,
    "Cattle",     "Africa", "Temperate"
  ) |>
    whep:::.calc_weighted_mcf()

  testthat::expect_equal(result$weighted_mcf, 0.07225)
  # Must not collapse to the flat 2% (0.02) default.
  testthat::expect_false(isTRUE(all.equal(result$weighted_mcf, 0.02)))
})

# .calc_direct_n2o --------------------------------------------------------------

testthat::test_that("Direct N2O falls back to Global MMS mix (#201)", {
  # A region without region-specific MMS rows ("Africa") must reuse the Global
  # distribution, giving the same weighted EF3 as an explicit "Global" region,
  # not the flat pasture default (EF3 = 0.005).
  base <- tibble::tribble(
    ~species_gen, ~n_excretion, ~heads,
    "Cattle",     100,          10
  )

  africa <- base |>
    dplyr::mutate(region = "Africa") |>
    whep:::.calc_direct_n2o() |>
    dplyr::pull(manure_n2o_direct)
  global <- base |>
    dplyr::mutate(region = "Global") |>
    whep:::.calc_direct_n2o() |>
    dplyr::pull(manure_n2o_direct)

  testthat::expect_equal(africa, global)
  flat_default <- 10 * 100 * 0.005 * (44 / 28)
  testthat::expect_false(isTRUE(all.equal(africa, flat_default)))
})
