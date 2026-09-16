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

testthat::test_that(".calc_weighted_mcf reaches the Latin America MMS mix, not the Global default", {
  # Regression for #174: regional_mms_distribution previously used region
  # label "Latin America and Caribbean", which never matched the "Latin
  # America" region produced by .add_ipcc_region(). The four LAC-specific
  # rows were dead data, and every Latin American row silently fell back to
  # region == NA handling (coalesce to a flat MCF of 2.0%).
  data <- tibble::tribble(
    ~species_gen, ~region,          ~method_manure_ch4,
    "Cattle",      "Latin America", "x",
    "Cattle",      "Global",        "x"
  )

  result <- whep:::.calc_weighted_mcf(data)

  lac_mcf <- result |>
    dplyr::filter(region == "Latin America") |>
    dplyr::pull(weighted_mcf)
  global_mcf <- result |>
    dplyr::filter(region == "Global") |>
    dplyr::pull(weighted_mcf)

  # Both must resolve to the real regional_mms_distribution mix (not the
  # coalesce() fallback of mcf = 2.0/fraction = 1.0, which would give 0.02),
  # and Latin America's MMS mix must differ from the Global default mix.
  testthat::expect_false(isTRUE(all.equal(lac_mcf, 0.02)))
  testthat::expect_false(isTRUE(all.equal(lac_mcf, global_mcf)))
})

testthat::test_that("regional_mms_distribution region labels match the IPCC region vocabulary", {
  # Regression for #174: a referential check that every non-Global region in
  # regional_mms_distribution is a region .add_ipcc_region() can actually
  # produce (catches the whole "hardcoded label that never joins" class).
  valid_regions <- c(
    "North America",
    "Eastern Europe",
    "Western Europe",
    "Middle East",
    "Asia",
    "Oceania",
    "Indian Subcontinent",
    "Latin America",
    "Africa"
  )

  present_regions <- regional_mms_distribution |>
    dplyr::filter(region != "Global") |>
    dplyr::pull(region) |>
    unique()

  testthat::expect_true(all(present_regions %in% valid_regions))
})

testthat::test_that(".join_bo gives Camels their own Bo, distinct from Buffalo", {
  # Regression for #251: Camels previously copied Buffalo's Bo (0.10).
  # IPCC 2019 Refinement Table 10.16a: Camels = 0.26, Buffalo = 0.10.
  data <- tibble::tribble(
    ~species,   ~species_gen,
    "Camels",   "Camels",
    "Buffalo",  "Buffalo"
  )

  result <- whep:::.join_bo(data)

  camel_bo <- result |>
    dplyr::filter(species == "Camels") |>
    dplyr::pull(methane_potential)
  buffalo_bo <- result |>
    dplyr::filter(species == "Buffalo") |>
    dplyr::pull(methane_potential)

  testthat::expect_equal(camel_bo, 0.26)
  testthat::expect_equal(buffalo_bo, 0.10)
  testthat::expect_false(isTRUE(all.equal(camel_bo, buffalo_bo)))
})

# EF3 vocabulary and the silent 0.005 default (#950) ---------------------------

testthat::test_that("Global poultry direct N2O uses the litter EF3 (#950)", {
  result <- tibble::tribble(
    ~species_gen, ~n_excretion, ~heads, ~region,
    "Poultry",    1,            1,      "Global"
  ) |>
    whep:::.calc_direct_n2o()

  # Global poultry MMS split: 0.80 "Poultry Manure" + 0.20 "Solid Storage".
  # ipcc_2019_n2o_ef_direct carries no "Poultry Manure" row, so the label-only
  # join left 0.80 of the split on the 0.005 "Other" coalesce default and the
  # weighted EF3 came out a flat 0.005. Resolved through .manure_ef3() the
  # deep-litter row applies: 0.80 * 0.001 + 0.20 * 0.005 = 0.0018.
  testthat::expect_equal(result$manure_n2o_direct, 0.0018 * (44 / 28))
  testthat::expect_false(
    isTRUE(all.equal(result$manure_n2o_direct, 0.005 * (44 / 28)))
  )
})

testthat::test_that("every shipped MMS label resolves an EF3 and an MCF (#950)", {
  # Referential invariant, the check that would have caught #950: every label
  # regional_mms_distribution can hand the manure engine must resolve both a
  # direct-N2O EF3 and a methane conversion factor in all three IPCC zones.
  labels <- unique(whep::regional_mms_distribution$mms_type)

  testthat::expect_equal(
    sort(intersect(labels, whep:::.manure_ef3()$mms_type)),
    sort(labels)
  )

  grid <- tidyr::expand_grid(
    mms_type = labels,
    climate_zone = c("Cool", "Temperate", "Warm")
  )
  matched <- dplyr::semi_join(
    grid,
    whep::climate_mcf,
    by = c("mms_type", "climate_zone")
  )
  testthat::expect_equal(nrow(matched), nrow(grid))
})

testthat::test_that("an MMS label with no EF3 aborts instead of taking 0.005", {
  testthat::local_mocked_bindings(
    .mms_global_shares = function() {
      tibble::tribble(
        ~species, ~mms_type,          ~fraction,
        "Cattle", "Composting - Bin", 1
      )
    },
    .package = "whep"
  )
  data <- tibble::tribble(
    ~species_gen, ~n_excretion, ~heads,
    "Cattle",     1,            1
  )

  testthat::expect_error(
    whep:::.calc_direct_n2o(data),
    class = "whep_missing_ef3"
  )
})

testthat::test_that("an MMS label with no MCF row aborts instead of taking 2%", {
  # "Anaerobic Digester" exists in climate_mcf only with climate_zone "All",
  # so a Temperate lookup misses it and used to coalesce to a flat 2.0 percent.
  testthat::local_mocked_bindings(
    .mms_global_shares = function() {
      tibble::tribble(
        ~species, ~mms_type,            ~fraction,
        "Cattle", "Anaerobic Digester", 1
      )
    },
    .package = "whep"
  )
  data <- tibble::tribble(
    ~species_gen, ~method_manure_ch4,
    "Cattle",     "IPCC_2019_Tier2"
  )

  testthat::expect_error(
    whep:::.calc_weighted_mcf(data),
    class = "whep_missing_mcf"
  )
})

testthat::test_that("a species with no MMS distribution aborts", {
  data <- tibble::tribble(
    ~species_gen, ~n_excretion, ~heads,
    "Rabbits",    1,            1
  )

  testthat::expect_error(
    whep:::.calc_direct_n2o(data),
    class = "whep_missing_mms_species"
  )
})

# Tier 2 region and climate zone (#949) ----------------------------------------

testthat::test_that("Tier 2 manure resolves the IPCC region on request (#949)", {
  data <- tibble::tibble(
    species = "Cattle, dairy",
    heads = 1000,
    iso3 = "USA",
    milk_yield_kg_day = 20,
    diet_quality = "High"
  ) |>
    whep::calculate_cohorts_systems()

  default <- whep::calculate_manure_emissions(data, tier = 2)
  regional <- whep::calculate_manure_emissions(
    data,
    tier = 2,
    options = list(mms_region = "resolve")
  )

  testthat::expect_false("region" %in% names(default))
  testthat::expect_equal(unique(regional$region), "North America")
  # Global cattle mix x Temperate MCF (Table 10.17):
  #   0.50*1.5 + 0.30*4.0 + 0.15*35.0 + 0.05*0.5 = 7.225 percent.
  # North America's mix is 0.40 Liquid/Slurry, 0.30 Solid Storage,
  # 0.25 Pasture, 0.05 Daily Spread:
  #   0.40*35.0 + 0.30*4.0 + 0.25*1.5 + 0.05*0.5 = 15.6 percent.
  testthat::expect_equal(unique(default$weighted_mcf), 0.07225)
  testthat::expect_equal(unique(regional$weighted_mcf), 0.156)
  testthat::expect_equal(unique(default$method_mms), "regional_default")
  testthat::expect_equal(unique(regional$method_mms), "region_specific")
})

testthat::test_that("a region request with no area key warns and is recorded", {
  data <- dairy_tier2_fixture()

  testthat::expect_warning(
    out <- whep::calculate_manure_emissions(
      data,
      tier = 2,
      options = list(mms_region = "resolve")
    ),
    class = "whep_no_region_key"
  )
  testthat::expect_false("region" %in% names(out))
  testthat::expect_equal(unique(out$method_mms), "regional_default")
})

testthat::test_that("mms_region 'global' ignores a region column", {
  base <- tibble::tribble(
    ~species_gen, ~n_excretion, ~heads, ~region,
    "Cattle",     100,          10,     "North America"
  )

  regional <- whep:::.calc_direct_n2o(base)
  forced <- whep:::.calc_direct_n2o(base, options = list(mms_region = "global"))
  global <- base |>
    dplyr::mutate(region = "Global") |>
    whep:::.calc_direct_n2o()

  testthat::expect_equal(forced$manure_n2o_direct, global$manure_n2o_direct)
  testthat::expect_false(
    isTRUE(all.equal(regional$manure_n2o_direct, global$manure_n2o_direct))
  )
  testthat::expect_equal(unique(forced$method_mms), "regional_default")
})

testthat::test_that("the assumed climate zone is selectable and recorded", {
  data <- tibble::tribble(
    ~species_gen, ~method_manure_ch4,
    "Cattle",     "IPCC_2019_Tier2"
  )

  warm <- whep:::.calc_weighted_mcf(
    data,
    options = list(assumed_climate_zone = "Warm")
  )
  cool <- whep:::.calc_weighted_mcf(
    data,
    options = list(assumed_climate_zone = "Cool")
  )

  # Global cattle mix x Warm MCF:
  #   0.50*2.0 + 0.30*5.0 + 0.15*80.0 + 0.05*1.0 = 14.55 percent.
  # x Cool MCF: 0.50*1.0 + 0.30*2.0 + 0.15*17.0 + 0.05*0.1 = 3.655 percent.
  testthat::expect_equal(warm$weighted_mcf, 0.1455)
  testthat::expect_equal(cool$weighted_mcf, 0.03655)
  testthat::expect_match(warm$method_manure_ch4, "climate_assumed_warm")
  testthat::expect_match(cool$method_manure_ch4, "climate_assumed_cool")
})

testthat::test_that("climate_source 'from_data' needs a climate_zone column", {
  data <- tibble::tribble(
    ~species_gen, ~method_manure_ch4,
    "Cattle",     "IPCC_2019_Tier2"
  )

  testthat::expect_error(
    whep:::.calc_weighted_mcf(
      data,
      options = list(climate_source = "from_data")
    ),
    class = "whep_missing_climate_zone"
  )

  supplied <- data |>
    dplyr::mutate(climate_zone = "Warm") |>
    whep:::.calc_weighted_mcf(options = list(climate_source = "from_data"))
  testthat::expect_equal(supplied$weighted_mcf, 0.1455)
  testthat::expect_match(supplied$method_manure_ch4, "climate_from_data")
})

testthat::test_that("an unknown manure option name aborts", {
  testthat::expect_error(
    whep::calculate_manure_emissions(
      single_tier1_fixture(),
      tier = 1,
      options = list(mms_regoin = "resolve")
    ),
    class = "whep_manure_options"
  )
})
# .calc_weighted_mcf: declare the climate assumption, never hide it -----------

testthat::test_that("a missing climate zone is assumed, stamped, not refused", {
  # The absent-column case has always assumed Temperate and stamped it. A hole
  # inside a supplied column is the same absence, so it takes the same declared
  # assumption -- refusing it would exclude an animal whose manure exists.
  data <- tibble::tibble(
    species = c("Dairy Cattle", "Dairy Cattle"),
    species_gen = "Cattle",
    subcategory = "Dairy",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2",
    climate_zone = c("Warm", NA_character_)
  )

  testthat::expect_warning(
    result <- whep:::.calc_weighted_mcf(data),
    "climate_zone"
  )

  testthat::expect_false(anyNA(result$weighted_mcf))
  # Only the unresolved row is stamped: the measured row stays measured.
  testthat::expect_equal(
    grepl("climate_assumed_temperate", result$method_manure_ch4),
    c(FALSE, TRUE)
  )
  # And the assumed row really took the Temperate mix, not the Warm one.
  temperate <- data |>
    dplyr::mutate(climate_zone = "Temperate") |>
    whep:::.calc_weighted_mcf()
  testthat::expect_equal(result$weighted_mcf[2], temperate$weighted_mcf[2])
})

testthat::test_that("an absent climate column keeps its declared assumption", {
  data <- tibble::tibble(
    species = "Dairy Cattle",
    species_gen = "Cattle",
    subcategory = "Dairy",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2"
  )

  result <- whep:::.calc_weighted_mcf(data)

  testthat::expect_true(
    grepl("climate_assumed_temperate", result$method_manure_ch4)
  )
})


testthat::test_that("an unknown climate zone aborts instead of taking 2%", {
  # `climate_mcf` keys Cool, Temperate and Warm. Any other non-NA label found
  # no MCF row and was handed the 2% liquid/slurry default one line below the
  # NA guard -- silently, with no warning and no method stamp. `cell_climate`
  # is a caller-supplied argument, so any label can arrive here.
  data <- tibble::tibble(
    species = "Cattle, dairy",
    species_gen = "Cattle",
    subcategory = "Dairy",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2",
    climate_zone = "Boreal"
  )

  msg <- tryCatch(
    {
      whep:::.calc_weighted_mcf(data)
      "<no error was raised>"
    },
    error = conditionMessage
  )
  testthat::expect_match(msg, "Boreal", fixed = TRUE)
  testthat::expect_match(msg, "climate_zone", fixed = TRUE)
})


testthat::test_that("the three keyed zones all resolve without a default", {
  # The complement of the test above: every zone `climate_mcf` keys must go
  # through, or the new guard would be rejecting valid input.
  data <- tibble::tibble(
    species = "Cattle, dairy",
    species_gen = "Cattle",
    subcategory = "Dairy",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2",
    climate_zone = c("Cool", "Temperate", "Warm")
  )

  result <- whep:::.calc_weighted_mcf(data)
  testthat::expect_equal(nrow(result), 3L)
  testthat::expect_false(any(is.na(result$weighted_mcf)))
  # A real weighted MCF, not the 2% default standing in for one.
  testthat::expect_false(all(result$weighted_mcf == 0.02))
})


testthat::test_that("the residual FAOSTAT species takes a declared split", {
  # "Animals live nes" is one of the four species WHEP's livestock vocabulary
  # carries and `regional_mms_distribution` omits. It argues no neighbour --
  # it is a residual category with no single husbandry -- so it takes the
  # minor-livestock fallback and says so in `method_manure_ch4`.
  data <- tibble::tibble(
    species = "Animals live nes",
    species_gen = "Animals live nes",
    subcategory = "All",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2",
    climate_zone = "Temperate"
  )

  testthat::expect_warning(
    result <- whep:::.calc_weighted_mcf(data),
    "Animals live nes"
  )

  # All of it deposited where it falls, so Temperate pasture MCF = 1.5 % ->
  # 0.015.
  pasture_mcf <- climate_mcf |>
    dplyr::filter(
      mms_type == "Pasture/Range/Paddock",
      climate_zone == "Temperate"
    ) |>
    dplyr::pull(mcf_percent)
  testthat::expect_equal(result$weighted_mcf, pasture_mcf / 100)
  testthat::expect_match(result$method_manure_ch4, "mms_assumed_pasture")
})


testthat::test_that("a species outside the argued list still aborts", {
  # The complement of the test above, and the line between an absent quantity
  # and an absent contract. The four species the fill covers are named; a
  # species nobody has argued a husbandry for is a vocabulary defect a
  # maintainer can fix, so it aborts rather than being given an invented split.
  data <- tibble::tibble(
    species = "Llamas",
    species_gen = "Llamas",
    subcategory = "All",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2",
    climate_zone = "Temperate"
  )

  testthat::expect_error(
    whep:::.calc_weighted_mcf(data),
    class = "whep_missing_mms_species"
  )
})


testthat::test_that("rabbits take the poultry MMS split, and say so", {
  # `livestock_mapping.csv`'s species_group == "other" (Rabbits and hares,
  # Rodents other, Animals live nes) is what actually reaches this. Rabbits and
  # cavies are caged small stock whose manure is collected dry, so the shipped
  # Poultry split is the argued neighbour -- not a bare 2 %.
  data <- tibble::tibble(
    species = "Rabbits and hares",
    species_gen = "Rabbits and hares",
    subcategory = "All",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2",
    climate_zone = "Temperate"
  )

  result <- suppressWarnings(whep:::.calc_weighted_mcf(data))

  poultry <- tibble::tibble(
    species = "Chickens",
    species_gen = "Poultry",
    subcategory = "All",
    heads = 100,
    method_manure_ch4 = "IPCC_2019_Tier2",
    climate_zone = "Temperate"
  ) |>
    whep:::.calc_weighted_mcf()

  testthat::expect_equal(result$weighted_mcf, poultry$weighted_mcf)
  testthat::expect_match(result$method_manure_ch4, "mms_assumed_poultry")
  # The poultry row is measured, so it carries no assumption stamp at all.
  testthat::expect_false(grepl("assumed", poultry$method_manure_ch4))
})


testthat::test_that("Tier 2 manure CH4 resolves for a species IPCC omits", {
  # Regression for the abort this replaces: one unmapped species used to take
  # the whole run down, including every species that WAS resolvable. With the
  # energy balance supplied by the caller, both rows now produce a number.
  data <- tibble::tibble(
    species = c("Dairy Cattle", "Rabbits and hares"),
    species_gen = c("Cattle", "Rabbits and hares"),
    subcategory = "All",
    heads = c(100, 100),
    gross_energy = c(250, 2),
    de_percent = 65,
    climate_zone = "Temperate",
    method_manure_ch4 = "IPCC_2019_Tier2"
  )

  result <- suppressWarnings(whep:::.calc_manure_ch4_tier2(data))

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_false(anyNA(result$manure_ch4_tier2))
  testthat::expect_true(all(result$manure_ch4_tier2 > 0))
  # Every assumption the unmapped row took is filterable from its method stamp.
  testthat::expect_false(grepl("assumed", result$method_manure_ch4[1]))
  testthat::expect_match(result$method_manure_ch4[2], "mms_assumed_poultry")
  testthat::expect_match(result$method_manure_ch4[2], "bo_assumed_")
  testthat::expect_match(result$method_manure_ch4[2], "ash_assumed_")
})


testthat::test_that("a row with no weighted MCF aborts, never passes NA", {
  msg <- tryCatch(
    {
      whep:::.check_weighted_mcf(tibble::tibble(weighted_mcf = NA_real_))
      "<no error was raised>"
    },
    error = conditionMessage
  )
  testthat::expect_match(msg, "weighted_mcf", fixed = TRUE)
})


# Declared assumptions for species no IPCC table covers -----------------------

testthat::test_that(".join_bo declares an assumed Bo, never invents one", {
  # `ipcc_tier2_bo_values` covers twelve categories. Anything else used to take
  # a bare 0.18 -- Other Cattle's Bo -- with nothing recording that it was not
  # measured.
  data <- tibble::tribble(
    ~species,            ~species_gen,        ~method_manure_ch4,
    "Dairy Cattle",      "Cattle",            "IPCC_2019_Tier2",
    "Rabbits and hares", "Rabbits and hares", "IPCC_2019_Tier2",
    "Llamas",            "Llamas",            "IPCC_2019_Tier2"
  )

  result <- suppressWarnings(whep:::.join_bo(data))

  horses_bo <- ipcc_tier2_bo_values |>
    dplyr::filter(category == "Horses") |>
    dplyr::pull(bo_m3_kg_vs)
  testthat::expect_equal(result$methane_potential[1], 0.24)
  testthat::expect_equal(result$methane_potential[2], horses_bo)
  testthat::expect_equal(
    result$methane_potential[3],
    mean(ipcc_tier2_bo_values$bo_m3_kg_vs)
  )
  # Measured rows stay unstamped; assumed rows are filterable.
  testthat::expect_equal(
    grepl("bo_assumed_", result$method_manure_ch4),
    c(FALSE, TRUE, TRUE)
  )
  # And the old bare 0.18 is not what an unmapped species gets any more.
  testthat::expect_false(any(result$methane_potential[2:3] == 0.18))
})


testthat::test_that("volatile solids declare an assumed ash content", {
  data <- tibble::tribble(
    ~species,            ~species_gen,        ~gross_energy, ~de_percent,
    "Dairy Cattle",      "Cattle",            200,           65,
    "Rabbits and hares", "Rabbits and hares", 2,             65,
    "Llamas",            "Llamas",            5,             65
  )

  result <- suppressWarnings(whep:::.calc_volatile_solids(data))

  horses_ash <- ipcc_tier2_manure_ash |>
    dplyr::filter(category == "Horses") |>
    dplyr::pull(ash_percent)
  testthat::expect_equal(
    result$ash_percent,
    c(8, horses_ash, mean(ipcc_tier2_manure_ash$ash_percent))
  )
  testthat::expect_equal(
    grepl("ash_assumed_", result$method_manure_ch4),
    c(FALSE, TRUE, TRUE)
  )
})


testthat::test_that("an absent diet declares the crude protein it assumed", {
  # 12 % is not a free-standing number: it is the Medium diet's crude protein
  # in `feed_characteristics`. Say so, and stamp the row that took it.
  data <- tibble::tribble(
    ~species,       ~species_gen, ~gross_energy, ~method_manure_n2o,
    "Dairy Cattle", "Cattle",     250,           "IPCC_2019_Tier2"
  )

  result <- suppressWarnings(whep:::.calc_n_excretion(data))

  medium_cp <- feed_characteristics |>
    dplyr::filter(diet_quality == "Medium") |>
    dplyr::pull(cp_percent)
  testthat::expect_equal(result$cp_percent, medium_cp)
  testthat::expect_match(result$method_manure_n2o, "cp_assumed_medium_diet")
})


testthat::test_that("a supplied diet leaves the crude protein unstamped", {
  data <- tibble::tibble(
    species = "Dairy Cattle",
    species_gen = "Cattle",
    gross_energy = 250,
    diet_quality = "High",
    method_manure_n2o = "IPCC_2019_Tier2"
  )

  result <- whep:::.calc_n_excretion(data)

  testthat::expect_equal(result$cp_percent, 16)
  testthat::expect_equal(result$method_manure_n2o, "IPCC_2019_Tier2")
})


testthat::test_that("swine and poultry reach their own N retention, not 0.07", {
  # `.get_bo_category()` keys swine and poultry by subcategory
  # ("Swine - Market", "Poultry - Layers") but `ipcc_tier2_n_retention` keys
  # them by species, so the join never matched and both took the invented 0.07
  # -- inflating their nitrogen excretion by (1-0.07)/(1-0.30) = 33 %.
  data <- tibble::tibble(
    species = c("Swine", "Chickens, layers", "Dairy Cattle"),
    species_gen = c("Swine", "Poultry", "Cattle"),
    gross_energy = c(100, 10, 250),
    diet_quality = "Medium",
    method_manure_n2o = "IPCC_2019_Tier2"
  )

  result <- whep:::.calc_n_excretion(data)

  testthat::expect_equal(result$n_retention_frac, c(0.30, 0.30, 0.20))
  testthat::expect_false(any(grepl("assumed", result$method_manure_n2o)))
})


testthat::test_that("an unmapped species declares its assumed N retention", {
  data <- tibble::tribble(
    ~species,            ~species_gen,        ~gross_energy, ~diet_quality,
    "Rabbits and hares", "Rabbits and hares", 2,             "Medium",
    "Llamas",            "Llamas",            5,             "Medium"
  )

  result <- suppressWarnings(whep:::.calc_n_excretion(data))

  poultry_ret <- ipcc_tier2_n_retention |>
    dplyr::filter(category == "Poultry") |>
    dplyr::pull(n_retention_frac)
  testthat::expect_equal(result$n_retention_frac[1], poultry_ret)
  testthat::expect_equal(
    result$n_retention_frac[2],
    mean(ipcc_tier2_n_retention$n_retention_frac)
  )
  testthat::expect_true(all(
    grepl("n_retention_assumed_", result$method_manure_n2o)
  ))
})


# Direct N2O: a sourced EF3, never an invented one ----------------------------

testthat::test_that("a frame with no region still weights over the MMS split", {
  # The pasture-only path this replaces put the whole excreted N of every Tier 2
  # row on the pasture EF3 of 0.010, liquid slurry and poultry litter included
  # (whep#949). No region is not no distribution: with none, the Global split
  # applies and is weighted like any other.
  data <- tibble::tibble(
    species = "Dairy Cattle",
    species_gen = "Cattle",
    n_excretion = 100,
    heads = 10,
    method_manure_n2o = "IPCC_2019_Tier2"
  )

  result <- whep:::.calc_direct_n2o(data)

  # Global cattle split x .manure_ef3():
  #   0.50*0.010 + 0.30*0.005 + 0.15*0.002 + 0.05*0.010 = 0.00730.
  testthat::expect_equal(
    result$manure_n2o_direct,
    10 * 100 * 0.0073 * (44 / 28)
  )
  # And emphatically not the flat pasture factor it used to take.
  testthat::expect_false(
    isTRUE(all.equal(result$manure_n2o_direct, 10 * 100 * 0.010 * (44 / 28)))
  )
  testthat::expect_false(grepl("ef3_pasture_only", result$method_manure_n2o))
})


testthat::test_that("a fully keyed MMS mix carries no EF3 assumption stamp", {
  data <- tibble::tibble(
    species = "Dairy Cattle",
    species_gen = "Cattle",
    n_excretion = 100,
    heads = 10,
    region = "Global",
    method_manure_n2o = "IPCC_2019_Tier1"
  )

  result <- whep:::.calc_direct_n2o(data)

  testthat::expect_false(grepl("assumed", result$method_manure_n2o))
  testthat::expect_false(grepl("pasture_only", result$method_manure_n2o))
})


testthat::test_that("an unmapped species reaches direct N2O, and declares it", {
  data <- tibble::tibble(
    species = "Rabbits and hares",
    species_gen = "Rabbits and hares",
    n_excretion = 1,
    heads = 100,
    region = "Global",
    method_manure_n2o = "IPCC_2019_Tier1"
  )

  result <- suppressWarnings(whep:::.calc_direct_n2o(data))

  testthat::expect_false(is.na(result$manure_n2o_direct))
  testthat::expect_gt(result$manure_n2o_direct, 0)
  testthat::expect_match(result$method_manure_n2o, "mms_assumed_poultry")
})


testthat::test_that("a row with no weighted EF3 aborts, never passes NA", {
  msg <- tryCatch(
    {
      whep:::.check_weighted_ef3(tibble::tibble(weighted_ef3 = NA_real_))
      "<no error was raised>"
    },
    error = conditionMessage
  )
  testthat::expect_match(msg, "weighted_ef3", fixed = TRUE)
})
