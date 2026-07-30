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

testthat::test_that("Tier 1 uses regional EF when iso3 is supplied", {
  result <- tibble::tribble(
    ~species, ~heads, ~iso3,
    "Beef Cattle", 1, "DEU",
    "Dairy Cattle", 1, "DEU"
  ) |>
    whep:::.calc_enteric_ch4_tier1()

  efs <- result |> dplyr::pull(enteric_ef_kgch4)
  # DEU -> Western Europe (IPCC Table 10.10): Other Cattle 57, Dairy 117.
  # The Global fallback would instead give 47 and 80.
  testthat::expect_equal(efs, c(57, 117))
  testthat::expect_false("region" %in% names(result))
})

# .add_ipcc_region --------------------------------------------------------------

testthat::test_that("IPCC region crosswalk covers all GLEAM regions", {
  # Regression for #268: four crosswalk keys ("Russia",
  # "Near East and North Africa", "East and Southeast Asia",
  # "Latin America and Caribbean") never matched the real gleam_region
  # values, leaving 90/204 countries with region = NA.
  hierarchy <- whep::gleam_geographic_hierarchy |>
    dplyr::distinct(iso3)

  result <- whep:::.add_ipcc_region(hierarchy)

  # Antarctica (ATF, SGS) has no IPCC EF region and is the only allowed gap.
  unmapped <- result |>
    dplyr::filter(is.na(region)) |>
    dplyr::pull(iso3)
  testthat::expect_setequal(unmapped, c("ATF", "SGS"))
})

testthat::test_that("IPCC region resolves representative countries", {
  result <- tibble::tribble(
    ~iso3,
    "RUS",
    "SAU",
    "CHN",
    "BRA"
  ) |>
    whep:::.add_ipcc_region()

  testthat::expect_equal(
    result$region,
    c("Eastern Europe", "Middle East", "Asia", "Latin America")
  )
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

testthat::test_that("Tier 2 scales cohort rows by cohort_heads, not heads", {
  # Regression for #106: each expanded cohort row still carries the national
  # `heads`, so scaling by `heads` and summing over cohorts inflated the herd
  # total by the cohort count (~20x for cattle).
  expanded <- tibble::tibble(
    species = "Cattle, dairy",
    heads = 1000,
    iso3 = "DEU",
    milk_yield_kg_day = 20,
    diet_quality = "High"
  ) |>
    whep::calculate_cohorts_systems()

  result <- expanded |>
    whep::estimate_energy_demand() |>
    whep:::.calc_enteric_ch4_tier2()

  # The per-row total uses cohort_heads, not the national heads it carries.
  testthat::expect_equal(
    result$enteric_ch4_tier2,
    result$cohort_heads * result$enteric_ch4_per_head
  )

  # Aggregated to the herd, the per-head figure stays in the IPCC range
  # (it was ~1,300 before the fix).
  per_head <- sum(result$enteric_ch4_tier2) / 1000
  testthat::expect_gt(per_head, 50)
  testthat::expect_lt(per_head, 200)
})

testthat::test_that("Tier 2 adds Method_Enteric column", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    whep:::.calc_enteric_ch4_tier2()

  result |>
    pointblank::expect_col_exists("method_enteric") |>
    pointblank::expect_col_vals_in_set(
      "method_enteric",
      c("IPCC_2019_Tier2")
    )
})
