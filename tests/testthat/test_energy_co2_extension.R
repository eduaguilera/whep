# USA (area_code 231) production: carcass tonnes per meat group plus the
# slaughtered head counts used to attribute each group to its live-animal
# sectors. iso3 USA resolves to GLEAM energy factors for every meat species.
.energy_prod_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 231L, 2731L, "tonnes", 1e7,
    2000L, 231L, 961L, "slaughtered_heads", 3e7,
    2000L, 231L, 946L, "slaughtered_heads", 1e6,
    2000L, 231L, 2732L, "tonnes", 2e5,
    2000L, 231L, 976L, "slaughtered_heads", 5e6,
    2000L, 231L, 1016L, "slaughtered_heads", 1e6,
    2000L, 231L, 2733L, "tonnes", 7e6,
    2000L, 231L, 1049L, "slaughtered_heads", 9e7,
    2000L, 231L, 1051L, "slaughtered_heads", 1e7,
    2000L, 231L, 2734L, "tonnes", 1.5e7,
    2000L, 231L, 1053L, "slaughtered_heads", 8e9
  )
}

testthat::test_that("example has the expected structure", {
  result <- whep::build_energy_co2_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_energy")
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
  testthat::expect_true(all(result$method_energy == "GLEAM_3.0_energy_meat"))
})

testthat::test_that("output is keyed by the meat live-animal sectors", {
  result <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_energy")
  )
  testthat::expect_setequal(
    result$item_cbs_code,
    c(961L, 946L, 976L, 1016L, 1049L, 1051L, 1053L)
  )
  testthat::expect_true(all(result$impact_u > 0))
  testthat::expect_false(any(is.na(result$impact_u)))
  testthat::expect_true(all(result$method_energy == "GLEAM_3.0_energy_meat"))
})

testthat::test_that("milk and egg sectors get no energy CO2 (meat only)", {
  prod <- .energy_prod_fixture() |>
    dplyr::bind_rows(
      tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
        2000L, 231L, 960L, "slaughtered_heads", 5e6,
        2000L, 231L, 1052L, "slaughtered_heads", 1e8
      )
    )
  result <- whep::build_energy_co2_extension(
    data = list(primary_prod = prod)
  )

  testthat::expect_false(any(result$item_cbs_code %in% c(960L, 1052L)))
})

testthat::test_that("emissions scale linearly with carcass production", {
  base <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )
  doubled <- .energy_prod_fixture() |>
    dplyr::mutate(
      value = dplyr::if_else(unit == "tonnes", value * 2, value)
    )
  result <- whep::build_energy_co2_extension(
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

testthat::test_that("a group is split across its sectors by slaughtered heads", {
  result <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )

  cattle <- result$impact_u[result$item_cbs_code == 961L]
  buffalo <- result$impact_u[result$item_cbs_code == 946L]
  # Same group factor and dressing, so the ratio is the head-count ratio (30:1).
  testthat::expect_equal(cattle / buffalo, 30)
})

testthat::test_that("areas GLEAM cannot classify are named, not dropped mutely", {
  # `gleam_geographic_hierarchy` is the country universe of the whole extension,
  # so an area with no row there gets no grouping, hence no `ef_total`, and the
  # intensity join in `.energy_co2e_by_group()` used to discard its production
  # without a word: a Tuvalu-only (area 227) build returned zero rows and raised
  # nothing. Tuvalu is the sharpest case because `.energy_ldc_iso3()` asserts TUV
  # is least-developed, i.e. the file claims a GLEAM grouping for a country the
  # table it joins against cannot represent. The two names are asserted rather
  # than only the count: whep#415 needs to know WHICH areas to resolve, and the
  # list moves with the crosswalk (Bermuda, Guam and Palau were in it until they
  # were folded into FABIO bucket 999, so they no longer report as themselves).
  testthat::expect_warning(
    hierarchy <- .energy_hierarchy(),
    "GLEAM cannot classify"
  )
  # The warning is a statement about the crosswalk, not about any one build, so
  # under the default it must not change what the country universe contains.
  testthat::expect_setequal(hierarchy$iso3, gleam_geographic_hierarchy$iso3)

  areas <- testthat::capture_warnings(.energy_hierarchy())
  testthat::expect_match(areas, "Nauru", all = FALSE)
  testthat::expect_match(areas, "Tuvalu", all = FALSE)
})

testthat::test_that("Bermuda, Guam and Palau cannot be reached individually", {
  # whep#415 names five live areas; three of them no longer report as themselves.
  # Measured on the real `get_primary_production()` output (6,170,595 rows, 194
  # distinct reporting areas): area codes 17, 88 and 180 carry ZERO rows, so no
  # treatment on this code path can reach them -- bucket 999 carries their
  # production. This pins the crosswalk state that makes that true, so if
  # whep#419 ever unfolds them the omission stops being silent.
  folded <- whep::polity_area_crosswalk |>
    tibble::as_tibble() |>
    dplyr::filter(.data$area_code %in% c(17L, 88L, 180L)) |>
    dplyr::distinct(.data$area_code, .data$polity_area_code, .data$polity_type)

  testthat::expect_equal(nrow(folded), 3L)
  testthat::expect_true(all(folded$polity_area_code == 999L))
  testthat::expect_true(all(folded$polity_type == "aggregate"))
  # ... which is exactly why the warning above does not name them.
  testthat::expect_false(any(
    c(17L, 88L, 180L) %in% .areas_gleam_cannot_group()$area_code
  ))
})

testthat::test_that("polity_region groups an omitted area like its GLEAM peers", {
  # The point of deriving rather than tabulating: no grouping label is typed in
  # here. Nauru and Tuvalu go through the same `case_when()` as the 204 published
  # countries, on the continent their polity carries, so each label they get must
  # already be one GLEAM's own non-OECD countries of that continent carry.
  extended <- suppressMessages(
    .energy_country_grouping(.energy_hierarchy("polity_region"))
  )
  derived <- dplyr::filter(extended, .data$ef_scope == "polity_region")
  testthat::expect_setequal(derived$iso3, c("NRU", "TUV"))

  hierarchy <- tibble::as_tibble(whep::gleam_geographic_hierarchy)
  peers <- extended |>
    dplyr::filter(
      .data$ef_scope == "country",
      .data$iso3 %in%
        hierarchy$iso3[
          hierarchy$continent == "Oceania" & hierarchy$oecd == 0
        ]
    )
  testthat::expect_setequal(derived$region5, unique(peers$region5))
  testthat::expect_setequal(derived$detailed15, unique(peers$detailed15))
  testthat::expect_equal(unique(peers$detailed15), "Non-OECD Pacific")

  # `development3` is a property of the country, not the continent, and comes
  # from `.energy_ldc_iso3()`. That resolves the contradiction whep#415 named:
  # this file asserted TUV was least-developed while joining against a table
  # that had no row for it at all.
  testthat::expect_equal(
    derived$development3[derived$iso3 == "TUV"],
    "Least developed countries"
  )
  testthat::expect_equal(derived$development3[derived$iso3 == "NRU"], "Others")

  # The GLEAM region comes from the merged whep#465 override table rather than a
  # second copy of that decision, and only feeds the dressing fraction.
  testthat::expect_true(
    all(.energy_polity_hierarchy_rows()$gleam_region == "Oceania")
  )
})

testthat::test_that("only continents that settle every scheme are derived", {
  # `detailed15` splits Asia into "Middle East" and "Asia" on `faostat_region`,
  # which `polity_area_crosswalk` does not carry, so an Asian area would have to
  # be guessed onto one side. It stays unpriced and keeps being reported instead.
  testthat::expect_false("Asia" %in% .energy_scheme_continents())
  testthat::expect_setequal(
    .energy_scheme_continents(),
    c("Africa", "Americas", "Europe", "Oceania")
  )
})

testthat::test_that("an area GLEAM omits can be neither OECD nor EU27", {
  # This is what lets the derived rows set `oecd = 0` and `eu27 = 0` rather than
  # look a flag up somewhere: both memberships are complete in GLEAM's own table,
  # so an iso3 absent from it belongs to neither. Pinned because a coefficient
  # refresh that dropped a member would make the assumption false silently.
  hierarchy <- tibble::as_tibble(whep::gleam_geographic_hierarchy)
  oecd_members <- c(
    "AUS",
    "AUT",
    "BEL",
    "CAN",
    "CHL",
    "COL",
    "CRI",
    "CZE",
    "DNK",
    "EST",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "ISL",
    "IRL",
    "ISR",
    "ITA",
    "JPN",
    "KOR",
    "LVA",
    "LTU",
    "LUX",
    "MEX",
    "NLD",
    "NZL",
    "NOR",
    "POL",
    "PRT",
    "SVK",
    "SVN",
    "ESP",
    "SWE",
    "CHE",
    "TUR",
    "GBR",
    "USA"
  )
  testthat::expect_length(setdiff(oecd_members, hierarchy$iso3), 0L)
  testthat::expect_length(
    setdiff(oecd_members, hierarchy$iso3[hierarchy$oecd == 1]),
    0L
  )
  testthat::expect_equal(sum(hierarchy$eu27), 27L)

  testthat::expect_true(all(.energy_polity_hierarchy_rows()$eu27 == 0L))
  testthat::expect_true(all(.energy_polity_hierarchy_rows()$oecd == 0L))
  testthat::expect_length(
    intersect(.energy_polity_hierarchy_rows()$iso3, hierarchy$iso3),
    0L
  )
})

# Tuvalu (area 227) is one of the two live areas GLEAM omits, and its production
# is real: 4,448 t of pig and 2,120 t of poultry carcass over 1961-2023 in the
# FAOSTAT input.
.energy_omitted_area_fixture <- function() {
  .energy_prod_fixture() |>
    dplyr::bind_rows(
      tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
        2000L, 227L, 2733L, "tonnes", 70,
        2000L, 227L, 1049L, "slaughtered_heads", 2000,
        2000L, 227L, 1051L, "slaughtered_heads", 200
      )
    )
}

testthat::test_that("polity_region recovers a live omitted area, and says so", {
  # whep#415's option 1, opt-in so nothing moves without consent. It must
  # recover Tuvalu, label those rows, and leave every GLEAM-classified area
  # bit-identical -- structure included, not just the totals.
  base <- suppressWarnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_omitted_area_fixture())
    )
  )
  result <- suppressMessages(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_omitted_area_fixture()),
      unclassified = "polity_region"
    )
  )

  testthat::expect_false(227L %in% base$area_code)
  tuvalu <- dplyr::filter(result, .data$area_code == 227L)
  testthat::expect_setequal(tuvalu$item_cbs_code, c(1049L, 1051L))
  testthat::expect_true(all(tuvalu$impact_u > 0))
  testthat::expect_true(
    all(tuvalu$method_energy == "GLEAM_3.0_energy_meat_polity_region")
  )

  # Structure, not only values: rows are added and none removed or re-keyed.
  usa <- dplyr::filter(result, .data$area_code == 231L)
  testthat::expect_equal(usa, dplyr::filter(base, .data$area_code == 231L))
  testthat::expect_equal(nrow(result), nrow(base) + nrow(tuvalu))
  testthat::expect_setequal(
    setdiff(result$area_code, base$area_code),
    227L
  )
  testthat::expect_length(setdiff(base$area_code, result$area_code), 0L)
})


testthat::test_that("area -> iso3 needs no tie-break across polity periods", {
  # `.energy_area_iso3()` used to reuse `.current_area_lookup()`, which exists to
  # pick one "best current" polity per area_code for a different purpose, purely
  # to read `area_iso3c` off the winning row -- riding on an unstated invariant.
  # The invariant is real (checked below), so the projection can be taken off the
  # crosswalk directly; this test pins the invariant so the simpler projection
  # cannot start silently picking an arbitrary iso3 if it ever breaks.
  per_area <- whep::polity_area_crosswalk |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::summarise(
      n_iso3 = dplyr::n_distinct(.data$area_iso3c),
      .by = "area_code"
    )
  testthat::expect_equal(max(per_area$n_iso3), 1L)

  area2iso <- .energy_area_iso3()
  testthat::expect_named(area2iso, c("area_code", "iso3"))
  testthat::expect_equal(anyDuplicated(area2iso$area_code), 0L)
  testthat::expect_false(any(is.na(area2iso$iso3)))
  testthat::expect_equal(area2iso$iso3[area2iso$area_code == 231L], "USA")
  # Statistical aggregates with no iso3 (351 "China", which double-counts its
  # components) stay out, as they did under the old lookup's unmapped filter.
  testthat::expect_false(351L %in% area2iso$area_code)
})

testthat::test_that("only the gleam method is available", {
  testthat::expect_error(
    whep::build_energy_co2_extension(method = "fao"),
    "should be"
  )
})

# Area 999 is the "Rest of World" reporting bucket: 62 FAOSTAT area codes fold
# into it, and it has no row in `gleam_geographic_hierarchy`, so it gets no
# energy intensity. Its carcass tonnage here is a fifth of the USA's 32.2 Mt,
# which makes the reported share exactly one sixth of the input.
.energy_unpriced_fixture <- function() {
  .energy_prod_fixture() |>
    dplyr::bind_rows(
      tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
        2000L, 999L, 2731L, "tonnes", 6.44e6,
        2000L, 999L, 961L, "slaughtered_heads", 3e6,
        2000L, 999L, 946L, "slaughtered_heads", 1e6
      )
    )
}

testthat::test_that("unpriceable meat production is reported with its size", {
  # The loss is real but was invisible: the intensity join leaves `ef_total` NA,
  # `.energy_allocate_to_sectors()` sums that away to zero, and the
  # `impact_u > 0` filter then removes the rows. Nothing said so. Measured on
  # the full FAOSTAT input, 595 Mt of carcass production (3.48% of 1850-2023,
  # 15% of 1961) left the extension this way, 25.5 Mt through bucket 999.
  msgs <- testthat::capture_warnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_unpriced_fixture())
    )
  )
  reported <- msgs[grepl("has no row for", msgs)]

  testthat::expect_length(reported, 1L)
  testthat::expect_match(reported, "RoW")
  # Size published, not just the fact: 6.44 Mt of 32.44 Mt of carcass tonnage.
  testthat::expect_match(reported, "6.4 Mt", fixed = TRUE)
  testthat::expect_match(reported, "16.67%", fixed = TRUE)
  testthat::expect_match(reported, "unclassified", fixed = TRUE)
})

testthat::test_that("the default still drops it, and taints nothing else", {
  # Status quo pin: `unclassified = "drop"` must leave the published numbers
  # exactly where they were, so adding unpriceable production to the input can
  # neither add rows nor move any other area's value.
  base <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )
  with_bucket <- suppressWarnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_unpriced_fixture())
    )
  )

  testthat::expect_false(999L %in% with_bucket$area_code)
  testthat::expect_equal(with_bucket, base)
})

testthat::test_that("global_mean prices the bucket and says so per row", {
  # The alternative treatment from whep#492, opt-in so nothing moves without
  # consent. It must recover the bucket, label those rows as world-mean, and
  # leave every classifiable area bit-identical.
  base <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )
  result <- suppressMessages(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_unpriced_fixture()),
      unclassified = "global_mean"
    )
  )

  bucket <- dplyr::filter(result, area_code == 999L)
  testthat::expect_setequal(bucket$item_cbs_code, c(961L, 946L))
  testthat::expect_true(all(bucket$impact_u > 0))
  testthat::expect_true(
    all(bucket$method_energy == "GLEAM_3.0_energy_meat_global_mean")
  )

  usa <- dplyr::filter(result, area_code == 231L)
  testthat::expect_true(all(usa$method_energy == "GLEAM_3.0_energy_meat"))
  testthat::expect_equal(usa, base)
})

testthat::test_that("the world-mean intensity is the mean of GLEAM's factors", {
  # No new coefficient enters the package: the world mean is the unweighted mean
  # of `gleam_energy_use_ef` over the groupings of the same scheme the country
  # factors use, recomputed here straight from the dataset.
  expected_bovine <- mean(
    whep::gleam_energy_use_ef$emission_factor[
      whep::gleam_energy_use_ef$species == "cattle" &
        whep::gleam_energy_use_ef$energy_type == "embedded" &
        whep::gleam_energy_use_ef$denominator == "lw" &
        whep::gleam_energy_use_ef$herd == "non_dairy"
    ]
  ) +
    mean(
      whep::gleam_energy_use_ef$emission_factor[
        whep::gleam_energy_use_ef$species == "large_ruminants" &
          whep::gleam_energy_use_ef$energy_type == "direct" &
          whep::gleam_energy_use_ef$denominator == "lw" &
          whep::gleam_energy_use_ef$herd == "non_dairy"
      ]
    )
  global <- .energy_global_intensity()

  testthat::expect_setequal(
    global$grp,
    c("bovine", "mutton_goat", "pig", "poultry")
  )
  testthat::expect_equal(
    global$ef_global[global$grp == "bovine"],
    expected_bovine
  )
  # A world mean must sit inside the spread of the country factors it averages,
  # otherwise the collapse is wrong rather than merely coarse.
  country <- suppressWarnings(.energy_intensity_by_country()) |>
    dplyr::summarise(
      lo = min(ef_total),
      hi = max(ef_total),
      .by = "grp"
    ) |>
    dplyr::inner_join(global, by = "grp")
  testthat::expect_true(all(country$ef_global >= country$lo))
  testthat::expect_true(all(country$ef_global <= country$hi))
})

testthat::test_that("polity_region leaves the aggregate buckets alone", {
  # Its scope is the live self-reporting areas only. Bucket 999 is not one, so
  # it must still drop and still be reported -- that is whep#492's decision, and
  # this treatment must not quietly pre-empt it.
  msgs <- testthat::capture_warnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_unpriced_fixture()),
      unclassified = "polity_region"
    )
  )
  result <- suppressMessages(suppressWarnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_unpriced_fixture()),
      unclassified = "polity_region"
    )
  ))

  testthat::expect_false(999L %in% result$area_code)
  testthat::expect_match(msgs, "has no row for", all = FALSE)
  testthat::expect_match(msgs, "RoW", all = FALSE)
})

testthat::test_that("unclassified only takes the documented values", {
  testthat::expect_error(
    whep::build_energy_co2_extension(unclassified = "zero"),
    class = "rlang_error"
  )
})
