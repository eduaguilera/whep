.ghg_prod_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 961L, "heads", 1000000,
    2000L, 10L, 976L, "heads", 5000000,
    2000L, 100L, 960L, "heads", 300000
  )
}

testthat::test_that("build_livestock_ghg_extension example has expected structure", {
  result <- whep::build_livestock_ghg_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c(
      "year",
      "area_code",
      "item_cbs_code",
      "impact_u",
      "method_ghg",
      "method_mms",
      "method_manure_ch4"
    )
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
  testthat::expect_true(all(result$method_ghg == "IPCC_2019_Tier1_AR6"))
})

testthat::test_that("Tier 1 keys output by live-animal sector and labels the method", {
  result <- whep::build_livestock_ghg_extension(
    tier = 1,
    gwp = "ar6",
    data = list(primary_prod = .ghg_prod_fixture())
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_ghg")
  )
  testthat::expect_setequal(result$item_cbs_code, c(960L, 961L, 976L))
  testthat::expect_true(all(result$impact_u > 0))
  testthat::expect_true(all(result$method_ghg == "IPCC_2019_Tier1_AR6"))
})

testthat::test_that("Tier 1 emissions scale linearly with head counts", {
  base <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = .ghg_prod_fixture())
  )
  doubled <- .ghg_prod_fixture() |>
    dplyr::mutate(value = value * 2)
  result <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = doubled)
  )

  joined <- dplyr::inner_join(
    base,
    result,
    by = c("year", "area_code", "item_cbs_code"),
    suffix = c("_base", "_double")
  )
  testthat::expect_equal(joined$impact_u_double, joined$impact_u_base * 2)
})

testthat::test_that("GWP standard rescales the footprint", {
  ar6 <- whep::build_livestock_ghg_extension(
    gwp = "ar6",
    data = list(primary_prod = .ghg_prod_fixture())
  )
  ar5 <- whep::build_livestock_ghg_extension(
    gwp = "ar5",
    data = list(primary_prod = .ghg_prod_fixture())
  )

  joined <- dplyr::inner_join(
    ar6,
    ar5,
    by = c("year", "area_code", "item_cbs_code"),
    suffix = c("_ar6", "_ar5")
  )
  ratio <- joined$impact_u_ar5 / joined$impact_u_ar6
  # Each sector's CO2e blends CH4 (AR5 28 vs AR6 27) and N2O (AR5 265 vs
  # AR6 273), so the ratio sits between the two gas ratios and is not 1.
  testthat::expect_true(all(
    ratio >= 265 / 273 - 1e-9 & ratio <= 28 / 27 + 1e-9
  ))
  testthat::expect_true(any(abs(ratio - 1) > 1e-6))
  testthat::expect_true(all(ar5$method_ghg == "IPCC_2019_Tier1_AR5"))
})

testthat::test_that("rows sharing a sector are summed", {
  prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 961L, "heads", 400000,
    2000L, 10L, 961L, "heads", 600000
  )
  combined <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = prod)
  )
  single <- whep::build_livestock_ghg_extension(
    data = list(
      primary_prod = dplyr::tibble(
        year = 2000L,
        area_code = 10L,
        item_cbs_code = 961L,
        unit = "heads",
        value = 1000000
      )
    )
  )

  testthat::expect_equal(nrow(combined), 1L)
  testthat::expect_equal(combined$impact_u, single$impact_u)
})

testthat::test_that("Tier 2 drops rows it cannot resolve instead of emitting NA", {
  result <- suppressWarnings(
    whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "uniform_medium",
      data = list(primary_prod = .ghg_prod_fixture())
    )
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_ghg")
  )
  testthat::expect_false(any(is.na(result$impact_u)))
})

testthat::test_that("tier must be 1 or 2", {
  testthat::expect_error(
    whep::build_livestock_ghg_extension(
      tier = 3,
      data = list(primary_prod = .ghg_prod_fixture())
    ),
    "tier"
  )
})

testthat::test_that(".warn_dropped_ghg distinguishes a systematic species gap from partial data gaps", {
  # Regression for #191: Swine/Poultry/Horses/Camels/Mules & Asses have no
  # Tier 2 energy coefficients at all (ipcc_tier2_energy_coefs only covers
  # Cattle, Buffalo, Sheep, Goats), so every row of those species silently
  # got gross_energy = NA and was dropped with a misleading message implying
  # a per-row data gap. The warning must now name the affected species and
  # say explicitly this is a systematic coefficient gap, separate from the
  # existing generic message for genuine per-row missing-data cases.
  emissions <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~species_gen, ~enteric_ch4_tier2, ~manure_ch4_tier2, ~manure_n2o_total,
    2000, 1, 1, "Cattle", 10, 5, 2,
    2000, 1, 2, "Cattle", NA_real_, NA_real_, NA_real_,
    2000, 1, 3, "Swine", NA_real_, NA_real_, NA_real_,
    2000, 1, 4, "Poultry", NA_real_, NA_real_, NA_real_
  )

  warnings_caught <- character()
  withCallingHandlers(
    whep:::.ghg_co2e_extension(emissions, tier = 2, gwp = "ar6"),
    warning = function(w) {
      warnings_caught <<- c(warnings_caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  systematic_warning <- warnings_caught[
    grepl("Swine", warnings_caught, fixed = TRUE)
  ]
  partial_warning <- warnings_caught[
    grepl("unresolved emissions", warnings_caught, fixed = TRUE)
  ]

  testthat::expect_length(systematic_warning, 1)
  testthat::expect_match(systematic_warning, "Swine", fixed = TRUE)
  testthat::expect_match(systematic_warning, "Poultry", fixed = TRUE)
  testthat::expect_match(systematic_warning, "no Tier 2 coefficients")
  testthat::expect_match(systematic_warning, "systematic gap")
  testthat::expect_length(partial_warning, 1)
  testthat::expect_false(grepl("Swine", partial_warning, fixed = TRUE))
})

testthat::test_that("Tier 2 warns explicitly when a real species has no energy coefficients", {
  # End-to-end: item_cbs_code 1049 (pig meat) maps to species_gen "Swine" via
  # prepare_livestock_emissions(), which has no Tier 2 Cfi row.
  prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 1049L, "heads", 100000
  )

  testthat::expect_warning(
    result <- whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "uniform_medium",
      data = list(primary_prod = prod)
    ),
    "no Tier 2 coefficients"
  )

  testthat::expect_equal(nrow(result), 0L)
})

# .sum_emission_cols: an absent gas is not a zero gas --------------------------

testthat::test_that("an absent emission column aborts instead of summing 0", {
  emissions <- tibble::tibble(enteric_ch4_tier1 = 1e6)

  testthat::expect_error(
    whep:::.sum_emission_cols(
      emissions,
      c(
        "enteric_ch4_tier1",
        "manure_ch4_tier1"
      )
    ),
    "missing column"
  )
})

testthat::test_that("present columns still sum row-wise with NA propagating", {
  emissions <- tibble::tibble(
    enteric_ch4_tier1 = c(1, 2, NA),
    manure_ch4_tier1 = c(10, 20, 30)
  )

  testthat::expect_equal(
    whep:::.sum_emission_cols(
      emissions,
      c(
        "enteric_ch4_tier1",
        "manure_ch4_tier1"
      )
    ),
    c(11, 22, NA)
  )
})

# method_diet -----------------------------------------------------------------

testthat::test_that("Tier 2 records which diet method produced the numbers", {
  result <- suppressWarnings(
    whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "uniform_medium",
      data = list(primary_prod = .ghg_prod_fixture())
    )
  )

  testthat::expect_true(all(
    result$method_ghg == "IPCC_2019_Tier2_AR6_diet_uniform_medium"
  ))
})

testthat::test_that("Tier 1 carries no diet dimension in its label", {
  result <- whep::build_livestock_ghg_extension(
    tier = 1,
    method_diet = "uniform_medium",
    data = list(primary_prod = .ghg_prod_fixture())
  )

  testthat::expect_true(all(result$method_ghg == "IPCC_2019_Tier1_AR6"))
})

testthat::test_that("an unknown diet method is rejected", {
  # Named rungs only: an unrecognised one must abort on the argument itself,
  # before anything is built. Asserting a bare error on a *valid* rung would
  # pass on the missing-feed-intake abort below and never test this at all.
  testthat::expect_error(
    whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "per_cel_feed",
      data = list(primary_prod = .ghg_prod_fixture())
    ),
    "must be one of"
  )
})

testthat::test_that("Tier 2 refuses to rebuild the feed intake behind you", {
  testthat::expect_error(
    whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "national_feed",
      data = list(primary_prod = .ghg_prod_fixture())
    ),
    "feed-intake table"
  )
})

# options passthrough (#1029) -------------------------------------------------

testthat::test_that("the #1029 numbers survive under mcf_source as_shipped", {
  # Regression lock for #1029: threading `options` from the extension down to
  # calculate_livestock_emissions() must not move a published value. The
  # expected figures were produced by the pre-passthrough code (origin/main at
  # 6c8bf0d2) on this same fixture, at both tiers. Tier 2 is now asked for
  # `mcf_source = "as_shipped"` explicitly, because whep#1022 moved the
  # default onto the 2019 Refinement; the lock still proves the passthrough
  # itself moves nothing, which is what #1029 was about.
  expected_tier1 <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~impact_u,
    10L, 961L, 1845198000,
    10L, 976L, 1472445000,
    100L, 960L, 916724250
  )
  # Sheep (976) is 2.53% above the figure #1029 locked, and only sheep. The
  # pre-passthrough code reached `.join_ym()` with a `diet_quality` column that
  # `estimate_energy_demand()` had already created and filled with `NA`, so
  # `.join_ym()`'s own "Medium" default -- guarded on the column being absent,
  # not empty -- never fired, the Ym join missed, and
  # `coalesce(ym_percent, 6.5)` handed every unmatched species the cattle Ym.
  # Sheep ship 6.7 in `ipcc_tier2_ym_values`, so their enteric CH4 rises by
  # exactly 6.7/6.5 and nothing else moves: cattle (961, 960) are already 6.5,
  # and manure CH4 and N2O are untouched. See `.assume_missing_diet()`.
  expected_tier2 <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~impact_u,
    10L, 961L, 2255369539.5477095,
    10L, 976L, 1600495292.4821796,
    100L, 960L, 492717625.4361503
  )

  tier1 <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = .ghg_prod_fixture())
  )
  # `uniform_medium` is the rung that reproduces the inline IPCC "Medium" diet
  # this lock was measured under; the default moved to `per_cell_feed` when the
  # diet ladder was named, and the fixture carries no cells to resolve one from.
  tier2 <- suppressWarnings(
    whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "uniform_medium",
      options = list(mcf_source = "as_shipped"),
      data = list(primary_prod = .ghg_prod_fixture())
    )
  )

  testthat::expect_equal(
    dplyr::arrange(
      dplyr::select(tier1, area_code, item_cbs_code, impact_u),
      area_code,
      item_cbs_code
    ),
    dplyr::arrange(expected_tier1, area_code, item_cbs_code)
  )
  testthat::expect_equal(
    dplyr::arrange(
      dplyr::select(tier2, area_code, item_cbs_code, impact_u),
      area_code,
      item_cbs_code
    ),
    dplyr::arrange(expected_tier2, area_code, item_cbs_code)
  )
  # The defaults the manure engine actually took, recorded per sector.
  testthat::expect_true(all(tier1$method_mms == "region_specific"))
  testthat::expect_true(all(tier1$method_manure_ch4 == "IPCC_2019_Tier1"))
  testthat::expect_true(all(
    tier2$method_manure_ch4 ==
      "IPCC_2019_Tier2; climate_assumed_temperate; mcf_as_shipped"
  ))
})

testthat::test_that("the shipped Tier 2 default is the 2019 Refinement", {
  # whep#1022 flipped `mcf_source` to `"ipcc_2019"`. These are the numbers the
  # extension publishes now, on the same fixture as the #1029 lock above, so
  # the two sit side by side and the size of the move is readable: cattle 961
  # +0.13 percent, sheep 976 -1.75 percent, cattle 960 +0.17 percent. Sheep
  # move most because they are 100 percent pasture, whose MCF the Refinement
  # cuts from 1.5 to 0.47 percent; the cattle sectors barely move because the
  # pasture cut is nearly cancelled by a higher liquid/slurry factor.
  expected <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~impact_u,
    10L, 961L, 2258337147.8284378,
    10L, 976L, 1572410786.0715780,
    100L, 960L, 493564355.41606408
  )
  tier2 <- suppressWarnings(
    whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "uniform_medium",
      data = list(primary_prod = .ghg_prod_fixture())
    )
  )

  testthat::expect_equal(
    dplyr::arrange(
      dplyr::select(tier2, area_code, item_cbs_code, impact_u),
      area_code,
      item_cbs_code
    ),
    dplyr::arrange(expected, area_code, item_cbs_code)
  )
  testthat::expect_true(all(
    tier2$method_manure_ch4 ==
      "IPCC_2019_Tier2; climate_assumed_temperate; mcf_ipcc_2019"
  ))
})

testthat::test_that("assumed_climate_zone reaches the manure kernel", {
  # The point of #1029: an option handed to the extension must change what the
  # kernel computes, not just be accepted. The climate zone sets the methane
  # conversion factor, so Cool <= Temperate <= Warm sector by sector -- a
  # passthrough that silently dropped `options` would leave all three equal.
  #
  # The bound is not strict under the default `mcf_source = "ipcc_2019"`:
  # the Refinement gives pasture/range/paddock a single 0.47 percent for every
  # zone, so a species that is 100 percent pasture (sheep, goats, camels,
  # mules and asses in `regional_mms_distribution`) has a climate-invariant
  # manure MCF and its sector is equal across all three. Sector 976 is sheep
  # and is exactly that case; the cattle sectors still increase strictly, so
  # at least one strict increase is asserted to keep the test able to fail on
  # a dropped passthrough.
  run_zone <- function(zone) {
    suppressWarnings(
      whep::build_livestock_ghg_extension(
        tier = 2,
        method_diet = "uniform_medium",
        options = list(assumed_climate_zone = zone),
        data = list(primary_prod = .ghg_prod_fixture())
      )
    ) |>
      dplyr::arrange(area_code, item_cbs_code)
  }
  cool <- run_zone("Cool")
  temperate <- run_zone("Temperate")
  warm <- run_zone("Warm")

  testthat::expect_true(all(cool$impact_u <= temperate$impact_u))
  testthat::expect_true(all(temperate$impact_u <= warm$impact_u))
  testthat::expect_true(any(cool$impact_u < temperate$impact_u))
  testthat::expect_true(any(temperate$impact_u < warm$impact_u))
  # The pasture-only sector is the one the Refinement makes zone-invariant.
  sheep <- function(x) x$impact_u[x$item_cbs_code == 976L]
  testthat::expect_equal(sheep(cool), sheep(warm))
  # Under the shipped table it did vary, so this is the edition talking.
  shipped_zone <- function(zone) {
    suppressWarnings(
      whep::build_livestock_ghg_extension(
        tier = 2,
        method_diet = "uniform_medium",
        options = list(
          assumed_climate_zone = zone,
          mcf_source = "as_shipped"
        ),
        data = list(primary_prod = .ghg_prod_fixture())
      )
    ) |>
      dplyr::arrange(area_code, item_cbs_code)
  }
  testthat::expect_false(isTRUE(all.equal(
    sheep(shipped_zone("Cool")),
    sheep(shipped_zone("Warm"))
  )))
  testthat::expect_true(all(
    warm$method_manure_ch4 ==
      "IPCC_2019_Tier2; climate_assumed_warm; mcf_ipcc_2019"
  ))
  testthat::expect_true(all(
    cool$method_manure_ch4 ==
      "IPCC_2019_Tier2; climate_assumed_cool; mcf_ipcc_2019"
  ))
  # Temperate is the default, so asking for it explicitly changes nothing.
  default <- suppressWarnings(
    whep::build_livestock_ghg_extension(
      tier = 2,
      method_diet = "uniform_medium",
      data = list(primary_prod = .ghg_prod_fixture())
    )
  ) |>
    dplyr::arrange(area_code, item_cbs_code)
  testthat::expect_equal(temperate, default)
})

testthat::test_that("mms_region reaches the manure kernel", {
  # `method_mms` is stamped inside the kernel's MMS resolver, so the flip can
  # only happen if the option travelled the whole way down. The shipped
  # `regional_mms_distribution` gives these species the same split in every
  # region, so the emission totals are unmoved here; the recorded method is
  # what says which split was taken.
  default <- whep::build_livestock_ghg_extension(
    data = list(primary_prod = .ghg_prod_fixture())
  )
  global <- whep::build_livestock_ghg_extension(
    options = list(mms_region = "global"),
    data = list(primary_prod = .ghg_prod_fixture())
  )

  testthat::expect_true(all(default$method_mms == "region_specific"))
  testthat::expect_true(all(global$method_mms == "regional_default"))
})

testthat::test_that("an unknown option aborts before the production read", {
  testthat::local_mocked_bindings(
    get_primary_production = function(...) {
      cli::cli_abort("The reader must not be reached.")
    }
  )

  testthat::expect_error(
    whep::build_livestock_ghg_extension(options = list(mms_regoin = "global")),
    class = "whep_manure_options"
  )
})
