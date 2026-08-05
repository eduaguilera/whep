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

testthat::test_that("dissolved states get a real region, not the Global EF", {
  # Regression for #465. `gleam_geographic_hierarchy` lists only the 204 modern
  # sovereign states, so every dissolved reporting area resolved to region = NA
  # and silently took the Global emission factor. MEASURED on `origin/main`, all
  # six of these areas returned the Global dairy EF of 80 kg CH4/head/yr; they
  # are the only areas with no GLEAM row that carry livestock heads in the
  # FAOSTAT production input (USSR 10.2e9 head-years, Yugoslav SFR 6.7e8,
  # Czechoslovakia 4.1e8, Belgium-Luxembourg 3.0e8, Serbia and Montenegro
  # 1.1e8, plus the RoW aggregate at 1.45e9 which is asserted separately below).
  areas <- tibble::tribble(
    ~area_code, ~species,
    15L, "Dairy Cattle", # Belgium-Luxembourg -> Western Europe
    51L, "Dairy Cattle", # Czechoslovakia     -> Eastern Europe
    151L, "Dairy Cattle", # Netherlands Antilles -> Central & South America
    186L, "Dairy Cattle", # Serbia and Montenegro -> Western Europe
    228L, "Dairy Cattle", # USSR -> Russian Federation, IPCC Eastern Europe
    248L, "Dairy Cattle" # Yugoslav SFR -> Western Europe
  ) |>
    dplyr::mutate(heads = 1)

  result <- areas |>
    whep:::.calc_enteric_ch4_tier1()

  # IPCC Table 10.10 dairy: Western Europe 117, Eastern Europe 99,
  # Latin America 72. None of them is the Global 80.
  testthat::expect_equal(
    result$enteric_ef_kgch4,
    c(117, 99, 72, 117, 99, 117)
  )
})

testthat::test_that("worldwide residual aggregates keep the Global EF", {
  # `polity_area_code` 999 (Rest of World) spans every continent -- it also
  # absorbs some 40 dependent territories such as Bermuda and Guam -- so the
  # Global average is the right factor for it and it is deliberately left out
  # of the #465 override table. This pins that as a decision, not an oversight.
  result <- tibble::tibble(
    area_code = 999L,
    species = "Dairy Cattle",
    heads = 1
  ) |>
    whep:::.calc_enteric_ch4_tier1()

  testthat::expect_equal(result$enteric_ef_kgch4, 80)
})

testthat::test_that("the region resolves from polity_area_code without iso3", {
  # The polity-keyed leg tested on a FIXTURE: every frame the shipped pipeline
  # produces carries `iso3` as well, because `prepare_livestock_emissions()`
  # attaches both, so an assertion over real data could not tell the two legs
  # apart. Dropping `iso3` here forces the resolution through
  # `polity_area_code` alone, which is what #465 asked for.
  result <- tibble::tibble(polity_area_code = c(228L, 51L, 999L)) |>
    whep:::.add_ipcc_region()

  testthat::expect_equal(
    result$region,
    c("Eastern Europe", "Eastern Europe", NA_character_)
  )
})

testthat::test_that("every override region is a key the IPCC crosswalk knows", {
  # An override value that does not match a `gleam_region` shipped in
  # `gleam_geographic_hierarchy` would crosswalk to region = NA and put the row
  # straight back on the Global fallback it was added to escape -- exactly the
  # failure mode #268 found in the IPCC crosswalk itself.
  overrides <- whep:::.gleam_region_overrides()
  known <- unique(whep::gleam_geographic_hierarchy$gleam_region)
  testthat::expect_true(all(overrides$gleam_region %in% known))

  resolved <- tibble::tibble(polity_area_code = overrides$polity_area_code) |>
    whep:::.add_ipcc_region()
  testthat::expect_false(anyNA(resolved$region))
})

testthat::test_that("the USSR region choice cannot move a published number", {
  # The USSR is the one override whose successor states span several GLEAM
  # regions, so its assignment is a judgement call. It is a SAFE one, and this
  # test is why: the IPCC crosswalk sends both candidate regions to the same
  # IPCC region, and `gleam_animal_weights` (the only other consumer of a GLEAM
  # region in the emission pipeline) ships rows for neither, so both fall back
  # to the same Global weights. If a future coefficient refresh adds either
  # region to `gleam_animal_weights`, this test fails and the choice becomes a
  # real decision that needs the project owner.
  candidates <- tibble::tibble(
    gleam_region = c(
      "Russian Federation",
      "Eastern Europe"
    )
  )
  ipcc <- tibble::tibble(iso3 = c("RUS", "UKR")) |>
    whep:::.add_ipcc_region()
  testthat::expect_equal(ipcc$region, c("Eastern Europe", "Eastern Europe"))

  weight_regions <- unique(whep::gleam_animal_weights$region)
  testthat::expect_equal(
    intersect(candidates$gleam_region, weight_regions),
    character()
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

testthat::test_that(".join_ym gives sheep a single Ym regardless of weight or diet quality", {
  # Regression for #250: IPCC 2019 Table 10.13 gives a single Ym = 6.7% for
  # all sheep "irrespective of feed quality" -- there is no <75kg/>=75kg
  # split or 4.7% value in the source. Real-world sheep (~45 kg average)
  # were previously routed to the fabricated 4.7% branch.
  data <- tibble::tribble(
    ~species_gen, ~weight, ~diet_quality,
    "Sheep",       45,      "Medium",
    "Sheep",       90,      "Medium",
    "Sheep",       NA,      "Low",
    "Sheep",       45,      "High"
  )

  result <- whep:::.join_ym(data)

  testthat::expect_equal(result$ym_factor, rep(6.7, 4))
})
