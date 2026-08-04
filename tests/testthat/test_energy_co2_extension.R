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
    grouping <- .energy_country_grouping(),
    "GLEAM cannot classify"
  )
  # The warning is a statement about the crosswalk, not about any one build, so
  # it must not change what the grouping itself contains.
  testthat::expect_setequal(grouping$iso3, gleam_geographic_hierarchy$iso3)

  areas <- testthat::capture_warnings(.energy_country_grouping())
  testthat::expect_match(areas, "Nauru", all = FALSE)
  testthat::expect_match(areas, "Tuvalu", all = FALSE)
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
