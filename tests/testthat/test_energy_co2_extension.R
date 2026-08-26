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

# Spain (203) alongside the USA (231), in two years, so every meat group recurs
# on both sides of the group -> sector fan-out. Both areas are classified by
# GLEAM, so neither is reported as unpriced meat.
.energy_multi_area_fixture <- function() {
  both <- .energy_prod_fixture() |>
    dplyr::bind_rows(dplyr::mutate(.energy_prod_fixture(), area_code = 203L))
  dplyr::bind_rows(both, dplyr::mutate(both, year = 2001L))
}

testthat::test_that("the sector fan-out is not reported as unexpected", {
  # The join is many-to-many by design, so only the extension's own warnings
  # (unpriced meat, missing slaughter shares) should ever reach a caller.
  testthat::expect_no_warning(
    result <- whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_multi_area_fixture())
    ),
    class = "dplyr_warning_join_relationship_many_to_many"
  )

  # The fan-out spreads each group without duplicating an output key...
  testthat::expect_equal(
    nrow(dplyr::distinct(result, year, area_code, item_cbs_code)),
    nrow(result)
  )
  # ...and it leaves each country-year exactly where it was on its own.
  single <- whep::build_energy_co2_extension(
    data = list(primary_prod = .energy_prod_fixture())
  )
  usa_2000 <- dplyr::filter(result, year == 2000L, area_code == 231L)
  testthat::expect_equal(nrow(usa_2000), nrow(single))
  testthat::expect_equal(sum(usa_2000$impact_u), sum(single$impact_u))
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

testthat::test_that("the crosswalk read follows the Rest-of-World unfold", {
  # whep#646. This file used to read the shipped `polity_area_crosswalk` object
  # directly, while every other consumer reads it through `.polity_crosswalk()`,
  # the one place `.unfold_rest_of_world()` is applied. The gate the energy
  # helpers use is `area_code == polity_area_code`, which is exactly the column
  # the unfold moves, so the two reads disagree about which areas report as
  # themselves for all 61 Rest-of-World members.
  #
  # Bermuda (17), Guam (88) and Palau (180) are the three whep#415 named. On the
  # shipped table they still carry `polity_area_code == 999`; under the default
  # `whep.unfold_rest_of_world = "all"` (whep#628) they carry their own code.
  shipped <- whep::polity_area_crosswalk |>
    tibble::as_tibble() |>
    dplyr::filter(.data$area_code %in% c(17L, 88L, 180L)) |>
    dplyr::distinct(.data$area_code, .data$polity_area_code)
  testthat::expect_equal(nrow(shipped), 3L)
  testthat::expect_true(all(shipped$polity_area_code == 999L))

  gaps <- .energy_self_reporting_gaps()
  testthat::expect_true(all(c(17L, 88L, 180L) %in% gaps$area_code))

  # ... and re-folding takes them straight back out, which a read of the shipped
  # table could not do either: it is invariant to the option.
  withr::local_options(whep.unfold_rest_of_world = "none")
  refolded <- suppressWarnings(.energy_self_reporting_gaps())
  testthat::expect_false(any(c(17L, 88L, 180L) %in% refolded$area_code))
})

testthat::test_that("Bermuda, Guam and Palau are grouped like their peers", {
  # THIS TEST ASSERTED THE OPPOSITE until whep#717. They reported as themselves
  # (previous test) but `.unfold_rest_of_world()` promoted `polity_area_code`
  # ONLY, so their polity was still the Rest-of-World aggregate and the
  # crosswalk gave that polity `continent = "World"`, which is not one of the
  # four continents the GLEAM scheme rules can settle. The `polity_type` filter
  # in `.areas_gleam_cannot_group()` therefore dropped them one line after
  # whep#628 let them in, and the `polity_region` treatment could not group
  # them even if the warning named them. That was whep#646's second half, and
  # giving a promoted member its own polity resolves it.
  rows <- .energy_self_reporting_gaps() |>
    dplyr::filter(.data$area_code %in% c(17L, 88L, 180L))

  # Four rows over three areas: Bermuda carries two upstream periods, which is
  # itself the year-aware identity whep#717 gives a promoted member.
  testthat::expect_equal(nrow(rows), 4L)
  testthat::expect_setequal(rows$area_code, c(17L, 88L, 180L))
  testthat::expect_false(any(rows$polity_type == "aggregate"))
  testthat::expect_false(any(rows$continent == "World"))
  testthat::expect_true(all(
    .energy_gleam_continent(rows$continent) %in% .energy_scheme_continents()
  ))

  # Bermuda and Palau are live, so they join whep#415's `polity_region`
  # treatment. Guam does NOT: its only upstream period `GUM-1898-1950` ended in
  # 1950, so it is a dissolved entity by the same derived test every other one
  # passes. Splitting the three that way is the crosswalk speaking, not a list
  # typed in here.
  live <- .areas_gleam_cannot_group()$area_code
  testthat::expect_true(all(c(17L, 180L) %in% live))
  testthat::expect_false(88L %in% live)
  testthat::expect_true(88L %in% .energy_dissolved_areas()$area_code)

  grouped <- suppressMessages(.energy_hierarchy("polity_region"))
  testthat::expect_true(all(c("BMU", "PLW") %in% grouped$iso3))

  # And Guam is STILL not priced, because `historical_region` needs the OECD/EU
  # membership the entity held while it existed and
  # `.energy_dissolved_membership()` records none for it. That is the right
  # answer, not a gap to fill here: inventing a membership for a colonial
  # administration would be exactly the manufactured value this package forbids.
  historical <- suppressMessages(.energy_hierarchy("historical_region"))
  testthat::expect_false(any(c("GUM", "CXR", "SHN") %in% historical$iso3))
  testthat::expect_equal(nrow(.energy_dissolved_rows()), 6L)
})

testthat::test_that("the unfold moves what the energy file reads", {
  # THE OPPOSITE CLAIM, AND IT IS THE POINT. This guard was written while the
  # promotion was numeric only, asserting that the three crosswalk-fed surfaces
  # were identical under both fold states so that "the first time a promoted
  # Rest-of-World member starts mattering, this fails instead of the change
  # being silent". whep#717 is that time: 31 members now carry a territory, and
  # 16 live plus 3 dissolved ones are GLEAM omissions the extension can group.
  #
  # So the guard is inverted rather than deleted -- the sets must differ by
  # EXACTLY the promoted members, and re-folding must put every one of them
  # back. A change that moved anything else, or that moved these silently in
  # only one direction, still fails here.
  gaps <- .areas_gleam_cannot_group()
  dissolved <- .energy_dissolved_areas()
  iso3 <- .energy_area_iso3()

  promoted <- whep::row_promotion_status()
  own <- promoted$area_code[promoted$status == "own_polity"]

  withr::local_options(whep.unfold_rest_of_world = "none")
  refolded_gaps <- suppressWarnings(.areas_gleam_cannot_group())
  refolded_dissolved <- suppressWarnings(.energy_dissolved_areas())

  testthat::expect_setequal(
    setdiff(gaps$area_code, refolded_gaps$area_code),
    c(
      5L,
      6L,
      17L,
      22L,
      36L,
      65L,
      82L,
      85L,
      94L,
      125L,
      140L,
      142L,
      161L,
      163L,
      172L,
      180L,
      190L,
      192L,
      218L,
      224L,
      239L,
      240L,
      258L,
      270L,
      279L,
      281L
    )
  )
  # 164 joined on 2026-08-13, the same shape as the three below it: the Pacific Islands
  # Trust Territory now resolves to TTPI-1947-1994, a period that ENDED, where the fold
  # had hidden it behind ROW-1850-2025's open end. FAOSTAT stops reporting area 164 in
  # 1990 and TTPI covers to 1993, so the gap this creates is latent -- no data falls in it.
  testthat::expect_setequal(
    setdiff(dissolved$area_code, refolded_dissolved$area_code),
    c(42L, 88L, 164L, 187L)
  )
  # Everything added is a promoted member, and re-folding removes nothing else.
  testthat::expect_true(all(
    setdiff(gaps$area_code, refolded_gaps$area_code) %in% own
  ))
  testthat::expect_true(all(
    setdiff(dissolved$area_code, refolded_dissolved$area_code) %in% own
  ))
  testthat::expect_length(setdiff(refolded_gaps$area_code, gaps$area_code), 0L)
  testthat::expect_length(
    setdiff(refolded_dissolved$area_code, dissolved$area_code),
    0L
  )
  # The ISO3 lookup is keyed on the bucket, which whep#628 already moved, so it
  # is the one surface the territorial half leaves alone.
  testthat::expect_equal(suppressWarnings(.energy_area_iso3()), iso3)
})

testthat::test_that("polity_region groups an omitted area like its GLEAM peers", {
  # The point of deriving rather than tabulating: no grouping label is typed in
  # here. Each derived area goes through the same `case_when()` as the 204
  # published countries, on the continent its polity carries, so every label it
  # gets must already be one GLEAM's own non-OECD countries of that continent
  # carry.
  #
  # It used to be two areas on one continent, which whep#717 took to 16 across
  # four -- so the peer comparison is made PER CONTINENT rather than against
  # Oceania's labels. Comparing the pooled sets would pass on a derivation that
  # gave Greenland the Pacific's labels.
  extended <- suppressMessages(
    .energy_country_grouping(.energy_hierarchy("polity_region"))
  )
  derived <- dplyr::filter(extended, .data$ef_scope == "polity_region")
  gaps <- .areas_gleam_cannot_group()
  testthat::expect_setequal(derived$iso3, gaps$area_iso3c)
  # 16 -> 28 on 2026-08-13: sixteen Rest-of-World members gained their own polity upstream
  # (whep-polities #209/#210/#212), so sixteen more areas get polity_region treatment instead
  # of the bucket's. The setequal above is the invariant; this count follows it.
  testthat::expect_equal(nrow(derived), 28L)

  # The peer set is taken on GLEAM's OWN continent vocabulary, which is what the
  # scheme rules are written against: the crosswalk splits the Americas in two
  # and `.energy_gleam_continent()` merges them back.
  hierarchy <- tibble::as_tibble(whep::gleam_geographic_hierarchy)
  polity_rows <- .energy_polity_hierarchy_rows()
  continent <- polity_rows$continent[match(derived$iso3, polity_rows$iso3)]
  testthat::expect_false(any(is.na(continent)))
  for (cont in unique(continent)) {
    peers <- extended |>
      dplyr::filter(
        .data$ef_scope == "country",
        .data$iso3 %in%
          hierarchy$iso3[
            hierarchy$continent == cont &
              hierarchy$oecd == 0 &
              hierarchy$eu27 == 0
          ]
      )
    # SUBSET, not setequal. `detailed15` carves the Russian Federation out of
    # Europe as its own group, so Europe's non-OECD peers do not share ONE
    # label and demanding the derived areas exhaust the peer set would assert
    # that some derived area must be Russia. What must hold is the other
    # direction: no derived area may carry a label its continent's peers do not
    # already carry, which is what "derived, not typed in" means.
    testthat::expect_gt(nrow(peers), 0L)
    testthat::expect_true(all(
      derived$region5[continent == cont] %in% peers$region5
    ))
    testthat::expect_true(all(
      derived$detailed15[continent == cont] %in% peers$detailed15
    ))
  }
  # Oceania is still the case the original whep#415 pair sat in, and there the
  # stronger equality holds.
  testthat::expect_equal(
    unique(derived$detailed15[continent == "Oceania"]),
    "Non-OECD Pacific"
  )

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
  # second copy of that decision, and only feeds the dressing fraction. That
  # table covers eight areas, and the 14 whep#717 added to this treatment are
  # not among them -- so they carry `NA` and take the GLOBAL-MEAN dressing
  # fraction, which `.energy_join_dressing()` documents as the fallback for a
  # row with no region. Filling those 14 in here would be inventing a regional
  # assignment; leaving them NA is the honest answer and it is asserted, not
  # left to chance.
  polity_rows <- .energy_polity_hierarchy_rows()
  named <- polity_rows$gleam_region[!is.na(polity_rows$gleam_region)]
  testthat::expect_true(all(
    named %in% whep::gleam_geographic_hierarchy$gleam_region
  ))
  testthat::expect_equal(
    polity_rows$gleam_region[polity_rows$iso3 %in% c("NRU", "TUV")],
    c("Oceania", "Oceania")
  )
  testthat::expect_setequal(
    polity_rows$iso3[is.na(polity_rows$gleam_region)],
    setdiff(polity_rows$iso3, c("NRU", "TUV"))
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

testthat::test_that("carcass with no slaughter heads is not dropped", {
  # Bovine carcass output but zero slaughtered-head rows for the group.
  prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 231L, 2731L, "tonnes", 1e7
  )
  testthat::expect_warning(
    result <- whep::build_energy_co2_extension(
      data = list(primary_prod = prod)
    ),
    "slaughtered-head"
  )

  # The group's CO2e survives and is split equally across both sectors.
  testthat::expect_setequal(result$item_cbs_code, c(961L, 946L))
  testthat::expect_true(all(result$impact_u > 0))
  testthat::expect_false(any(is.na(result$impact_u)))
  testthat::expect_equal(
    result$impact_u[result$item_cbs_code == 961L],
    result$impact_u[result$item_cbs_code == 946L]
  )
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

# ---- whep#553: the dissolved entities --------------------------------------

# The USSR (area 228) is 437 Mt of the 569 Mt of carcass production that no
# GLEAM grouping can price, measured on the real `get_primary_production()`
# output. Belgium-Luxembourg (area 15) is the one dissolved entity that was
# itself OECD and EU while it reported, so the two are fixtured together.
.energy_dissolved_fixture <- function() {
  .energy_prod_fixture() |>
    dplyr::bind_rows(
      tibble::tribble(
        ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
        1980L, 228L, 2731L, "tonnes", 6.5e6,
        1980L, 228L, 961L, "slaughtered_heads", 3e7,
        1980L, 228L, 946L, "slaughtered_heads", 1e6,
        1980L, 15L, 2733L, "tonnes", 7e5,
        1980L, 15L, 1049L, "slaughtered_heads", 8e6,
        1980L, 15L, 1051L, "slaughtered_heads", 1e5
      )
    )
}

testthat::test_that("the dissolved set is derived, not typed in", {
  # What separates a dissolved entity from the live omissions and from the
  # aggregate buckets is a property of the crosswalk, not a list: every polity
  # period a dissolved area carries ended before the crosswalk's open end.
  dissolved <- .energy_dissolved_areas()

  # 42 Christmas Island, 88 Guam and 187 Saint Helena joined the six original
  # ones with whep#717. They are Rest-of-World members whose own upstream
  # period ended -- `CXR-1946-1958`, `GUM-1898-1950`, `SHN-1834-1967` -- which
  # the fold hid behind `ROW-1850-2025`, a period that runs to the open end and
  # so never looks dissolved. That upstream has no live successor period for
  # them is a coverage gap `polity_coverage_gaps()` now reports.
  # 164 Pacific Islands Trust Territory joined on 2026-08-13, by the same mechanism: it now
  # resolves to TTPI-1947-1994, whose period ended in 1994.
  testthat::expect_setequal(
    dissolved$area_code,
    c(15L, 42L, 51L, 88L, 151L, 164L, 186L, 187L, 228L, 248L)
  )
  # The live omissions stay with whep#415's treatment ...
  testthat::expect_false(any(c(148L, 227L) %in% dissolved$area_code))
  # ... and the aggregate buckets stay with whep#492's, because their periods
  # run to the open end rather than stopping at a dissolution.
  testthat::expect_false(
    any(c(901L, 902L, 903L, 904L, 905L, 906L, 999L) %in% dissolved$area_code)
  )
  open_end <- max(whep::polity_area_crosswalk$polity_end_year, na.rm = TRUE)
  testthat::expect_true(all(dissolved$last_year < open_end))
  # One row per area, so the hierarchy cannot gain a duplicate iso3 and
  # duplicate every production row it joins to.
  testthat::expect_equal(anyDuplicated(dissolved$area_code), 0L)
  testthat::expect_equal(
    anyDuplicated(.energy_dissolved_rows()$iso3),
    0L
  )
  hierarchy <- suppressMessages(suppressWarnings(
    .energy_hierarchy("historical_region")
  ))
  testthat::expect_equal(anyDuplicated(hierarchy$iso3), 0L)
})

testthat::test_that("a dissolved entity is grouped as it was, not as now", {
  # The whep#553 decision, made visible: memberships are taken as of the
  # entity's own existence. Belgium and Luxembourg both signed the OECD
  # Convention in 1960 and both are EEC founders, so Belgium-Luxembourg reports
  # its whole 1961-1999 span as an OECD/EU member and must land exactly where
  # Belgium and Luxembourg do. Czechoslovakia's successors joined the OECD in
  # 1995 and 2000 and the EU in 2004 -- all after the 1992 dissolution -- so it
  # must NOT inherit their present-day groupings.
  grouping <- suppressMessages(suppressWarnings(
    .energy_country_grouping(.energy_hierarchy("historical_region"))
  ))
  at <- function(code) {
    as.list(dplyr::filter(grouping, .data$iso3 == code))
  }

  testthat::expect_equal(at("BLX")$development3, at("BEL")$development3)
  testthat::expect_equal(at("BLX")$region5, at("BEL")$region5)
  testthat::expect_equal(at("BLX")$detailed15, at("BEL")$detailed15)
  testthat::expect_equal(at("BLX")$detailed15, at("LUX")$detailed15)
  testthat::expect_equal(at("BLX")$detailed15, "EU 27")

  testthat::expect_equal(at("CSK")$development3, "Others")
  testthat::expect_equal(at("CSK")$detailed15, "Non-OECD Europe")
  testthat::expect_false(identical(at("CSK")$detailed15, at("CZE")$detailed15))
  testthat::expect_false(identical(at("CSK")$detailed15, at("SVK")$detailed15))
  # The Yugoslav successors were outside both bodies while the federation
  # existed and Serbia still is, so there the two readings agree.
  testthat::expect_equal(at("YUG")$detailed15, at("SRB")$detailed15)
  testthat::expect_equal(at("SCG")$detailed15, at("SRB")$detailed15)
  # The Netherlands Antilles is the reason the crosswalk's continent has to be
  # translated: it says "North America" where GLEAM's rules say "Americas".
  testthat::expect_equal(.energy_gleam_continent("North America"), "Americas")
  testthat::expect_equal(.energy_gleam_continent("South America"), "Americas")
  testthat::expect_equal(.energy_gleam_continent("Europe"), "Europe")
  # Everything else, NA and the empty build included, comes back untouched:
  # the translation must not turn an unclassifiable continent into a match.
  testthat::expect_equal(.energy_gleam_continent(NA_character_), NA_character_)
  testthat::expect_equal(.energy_gleam_continent("bogus"), "bogus")
  testthat::expect_equal(.energy_gleam_continent(character()), character())
  testthat::expect_equal(at("ANT")$region5, at("CUB")$region5)
})

testthat::test_that("the method label names the scope that produced a row", {
  # Pinned because whep#850 rewrote the label off the deprecated
  # dplyr::case_match(): these strings are published in method_energy.
  label <- "GLEAM_3.0_energy_meat"

  testthat::expect_equal(
    .energy_method_label(
      "gleam",
      c("global", "polity_region", "historical_region", "country")
    ),
    c(
      paste0(label, "_global_mean"),
      paste0(label, "_polity_region"),
      paste0(label, "_historical_region"),
      label
    )
  )
  # The default scope, an unknown scope and NA all read as the plain country
  # factor, which is what a row with no derived treatment carries.
  testthat::expect_equal(.energy_method_label("gleam"), label)
  testthat::expect_equal(.energy_method_label("gleam", NA_character_), label)
  testthat::expect_equal(
    .energy_method_label("gleam", character()),
    character()
  )
})

testthat::test_that("historical_region recovers the dissolved entities", {
  # whep#553's fix, opt-in so nothing moves without consent: the areas must come
  # back with their own factors, be labelled as such, and leave every
  # GLEAM-classified area bit-identical -- structure included.
  base <- suppressWarnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_dissolved_fixture())
    )
  )
  result <- suppressWarnings(suppressMessages(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_dissolved_fixture()),
      unclassified = "historical_region"
    )
  ))

  testthat::expect_false(any(c(15L, 228L) %in% base$area_code))
  recovered <- dplyr::filter(result, .data$area_code %in% c(15L, 228L))
  testthat::expect_setequal(
    recovered$item_cbs_code,
    c(961L, 946L, 1049L, 1051L)
  )
  testthat::expect_true(all(recovered$impact_u > 0))
  testthat::expect_true(all(
    recovered$method_energy == "GLEAM_3.0_energy_meat_historical_region"
  ))

  usa <- dplyr::filter(result, .data$area_code == 231L)
  testthat::expect_equal(usa, dplyr::filter(base, .data$area_code == 231L))
  testthat::expect_equal(nrow(result), nrow(base) + nrow(recovered))
  testthat::expect_setequal(
    setdiff(result$area_code, base$area_code),
    c(15L, 228L)
  )
  testthat::expect_length(setdiff(base$area_code, result$area_code), 0L)
  # One area_code, one label: recovering an area must not split its key.
  testthat::expect_equal(
    max(
      dplyr::summarise(
        result,
        n = dplyr::n_distinct(.data$polity_area_code),
        .by = "area_code"
      )$n
    ),
    1L
  )
})

testthat::test_that("the default still drops the dissolved entities", {
  # Status quo pin: `unclassified = "drop"` publishes the same numbers it did
  # before whep#553, so adding dissolved-entity production can neither add rows
  # nor move another area's value.
  base <- suppressWarnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_prod_fixture())
    )
  )
  with_dissolved <- suppressWarnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_dissolved_fixture())
    )
  )

  testthat::expect_equal(with_dissolved, base)
  reported <- testthat::capture_warnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_dissolved_fixture())
    )
  )
  testthat::expect_match(reported, "USSR", all = FALSE)
})

testthat::test_that("historical_region keeps the live areas on whep#415", {
  # It is a superset of `polity_region`, not a replacement: Tuvalu must still be
  # grouped from the crosswalk and still carry that label, so the two decisions
  # stay distinguishable row by row.
  prod <- dplyr::bind_rows(
    .energy_omitted_area_fixture(),
    dplyr::filter(.energy_dissolved_fixture(), .data$area_code == 228L)
  )
  result <- suppressWarnings(suppressMessages(
    whep::build_energy_co2_extension(
      data = list(primary_prod = prod),
      unclassified = "historical_region"
    )
  ))

  testthat::expect_true(all(
    result$method_energy[result$area_code == 227L] ==
      "GLEAM_3.0_energy_meat_polity_region"
  ))
  testthat::expect_true(all(
    result$method_energy[result$area_code == 228L] ==
      "GLEAM_3.0_energy_meat_historical_region"
  ))
  testthat::expect_true(all(
    result$method_energy[result$area_code == 231L] == "GLEAM_3.0_energy_meat"
  ))
})

testthat::test_that("an uncovered dissolved entity stays unpriced", {
  # The membership table is the whep#553 decision itself, so an entity it does
  # not cover must fall out of the join and keep being reported, rather than
  # defaulting to non-OECD/non-EU zeros nobody decided on.
  testthat::local_mocked_bindings(
    .energy_dissolved_membership = function() {
      tibble::tibble(
        polity_area_code = 51L,
        oecd = 0L,
        eu27 = 0L
      )
    }
  )
  rows <- .energy_dissolved_rows()
  testthat::expect_setequal(rows$iso3, "CSK")

  msgs <- testthat::capture_warnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_dissolved_fixture()),
      unclassified = "historical_region"
    )
  )
  result <- suppressMessages(suppressWarnings(
    whep::build_energy_co2_extension(
      data = list(primary_prod = .energy_dissolved_fixture()),
      unclassified = "historical_region"
    )
  ))
  testthat::expect_false(any(c(15L, 228L) %in% result$area_code))
  testthat::expect_match(msgs, "has no row for", all = FALSE)
  testthat::expect_match(msgs, "USSR", all = FALSE)
})
