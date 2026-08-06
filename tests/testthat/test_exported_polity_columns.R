# Every area-keyed export must say WHICH TERRITORY its rows belong to.
#
# A row keyed only by `area_code` is attributable but not self-describing: the
# caller has to re-join the crosswalk to learn whose territory it is, and to
# learn whether that territory has a polygon. whep#424 censused this. Of the
# exports that take `example = TRUE` and return an area-keyed frame, 10 carried
# the reporting-polity columns and 22 did not; re-running the census on `main`
# after that measurement gave 28 without, because six exports added since then
# joined the gap. PR #382 pinned the 22 as a deliberate boundary. This test is
# the inversion of that pin: the contract is now that an area-keyed export
# carries the columns, and the exceptions are three named functions, each with a
# reason that this test checks rather than trusts.
#
# It is pinned by IDENTITY, not by count: "3 exceptions" would still pass if one
# export silently lost its columns while another gained them. And it deliberately
# does NOT skip on CI -- a gate that only runs on a developer's laptop is how the
# boundary drifted by six exports in the first place. All 73 example paths are
# hardcoded fixtures or pure computation over them, no network and no rasters, so
# running them all costs seconds.

# Exported functions that can be run cheaply, via their `example` argument.
.exports_with_example <- function() {
  ns <- asNamespace("whep")
  ex <- getNamespaceExports("whep")
  fns <- ex[vapply(ex, function(n) is.function(get(n, envir = ns)), logical(1))]
  sort(fns[vapply(
    fns,
    function(n) "example" %in% names(formals(get(n, envir = ns))),
    logical(1)
  )])
}

.run_example <- function(name) {
  tryCatch(
    suppressWarnings(suppressMessages(
      do.call(getExportedValue("whep", name), list(example = TRUE))
    )),
    error = function(e) NULL
  )
}

# "with_polity" / "without_polity" / "not_area_keyed" / "not_a_frame", using the
# same area-column patterns and the same polity-column test as the whep#424
# census, so the numbers in this file are comparable to the ones in the issue.
.classify_export <- function(name) {
  out <- .run_example(name)
  if (is.null(out)) {
    return("unavailable")
  }
  frame <- tryCatch(as.data.frame(out), error = function(e) NULL)
  if (is.null(frame)) {
    return("not_a_frame")
  }
  nm <- names(frame)
  area <- grepl(
    "^(area|area_code|area_name|country|iso3c?|region|sub_territory)$",
    nm,
    ignore.case = TRUE
  )
  if (!any(area)) {
    return("not_area_keyed")
  }
  if (any(grepl("polity", nm, ignore.case = TRUE))) {
    "with_polity"
  } else {
    "without_polity"
  }
}

testthat::test_that("area-keyed exports carry the reporting-polity columns", {
  fns <- .exports_with_example()
  # Non-vacuous: a tiny export set would make every assertion below meaningless.
  testthat::expect_gt(length(fns), 50L)

  kinds <- vapply(fns, .classify_export, character(1))
  # No export's example path may error: an unavailable one would silently drop
  # out of both sets below and hide a regression.
  testthat::expect_setequal(intersect(kinds, "unavailable"), character())

  without <- sort(names(kinds)[kinds == "without_polity"])
  testthat::expect_setequal(
    without,
    c(
      # Pre-resolution reader: returns FAOSTAT's own `area` NAME ("Portugal"),
      # not an area code, because resolving is the caller's job.
      "get_faostat_data",
      # Footprints aggregated over time. They have no `year`, so resolving a
      # polity means CHOOSING one, which is a modelling decision and not
      # plumbing. Left out until that convention is decided.
      "build_grazing_feed_footprint",
      "build_land_balance_footprint"
    )
  )

  # And the exports that must carry them, by name, so one cannot quietly lose
  # its columns while a new export joins the exception list. These are the 25
  # widened for whep#424 plus the 10 that already complied.
  with_polity <- names(kinds)[kinds == "with_polity"]
  testthat::expect_true(all(
    c(
      "build_ag_land_support",
      "build_carbon_balance",
      "build_carbon_inputs",
      "build_crop_soil_n2o_extension",
      "build_energy_co2_extension",
      "build_feed_demand",
      "build_feed_intake_local",
      "build_food_supply",
      "build_grass_natural_carbon_inputs",
      "build_grassland_land_extension",
      "build_livestock_ghg_extension",
      "build_n_boundary_exceedance",
      "build_n_deposition",
      "build_n_inputs",
      "build_n_pathway_exceedance",
      "build_n_percapita",
      "build_nitrogen_balance",
      "build_primary_production",
      "build_soil_carbon_inputs",
      "build_supply_use",
      "build_urban_n",
      "build_water_balance",
      "calculate_n_surplus",
      "get_arable_permanent_land",
      "get_crop_land_extension",
      "get_soc_climate_drivers",
      "get_wide_cbs",
      "read_luh2_landuse"
    ) %in%
      with_polity
  ))
})

testthat::test_that("the attached polity columns are populated, not just present", {
  # A left join that matched nothing would satisfy the name check above while
  # telling the caller nothing, which is the failure mode of attaching columns
  # with the wrong code column or an incompatible code type. So check the
  # payload: the full four-column set, and at least one resolved polity code.
  # `build_urban_n` is in this list on purpose: its fixtures used to key cells
  # by the string "ESP", which cannot resolve against the numeric crosswalk.
  cols <- c(
    "polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry"
  )
  for (nm in c(
    "build_nitrogen_balance",
    "build_water_balance",
    "build_urban_n",
    "get_arable_permanent_land",
    "read_luh2_landuse",
    "get_crop_land_extension"
  )) {
    out <- .run_example(nm)
    testthat::expect_false(is.null(out), info = nm)
    pointblank::expect_col_exists(out, dplyr::all_of(cols))
    testthat::expect_true(
      any(!is.na(out$reporting_polity_code)),
      info = paste(nm, "resolved no polity for any row")
    )
  }
})

testthat::test_that("the mapping-status switch is off, and adds one column", {
  # THE RECONCILIATION whep#545 needs. The pins above say the polity columns are
  # PRESENT; this one says the default set is exactly those four, so the
  # `out_of_span` signal cannot arrive on ~100 exported outputs without an
  # explicit decision -- and that when it is asked for, it arrives as exactly one
  # extra column rather than as a wider re-shaping.
  cols <- c(
    "polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry"
  )
  for (nm in c(
    "build_primary_production",
    "get_wide_cbs",
    "get_arable_permanent_land"
  )) {
    base <- .run_example(nm)
    testthat::expect_false(is.null(base), info = nm)
    testthat::expect_setequal(grep("polity", names(base), value = TRUE), cols)

    flagged <- withr::with_options(
      list(whep.polity_mapping_status = "flag"),
      .run_example(nm)
    )
    testthat::expect_equal(
      setdiff(names(flagged), names(base)),
      "reporting_polity_out_of_span",
      info = nm
    )
    status <- withr::with_options(
      list(whep.polity_mapping_status = "status"),
      .run_example(nm)
    )
    testthat::expect_equal(
      setdiff(names(status), names(base)),
      "reporting_mapping_status",
      info = nm
    )
    # Nothing else moves: same rows, same values in the shared columns.
    testthat::expect_equal(
      as.data.frame(flagged[names(base)]),
      as.data.frame(base),
      info = nm
    )
  }
})

testthat::test_that("the three carve-outs are carved out for the stated reason", {
  # The reasons are the load-bearing part of the exception list, so they are
  # asserted, not asserted-in-a-comment. If a `year` column ever appears on the
  # footprints, or `get_faostat_data()` starts returning a resolved area code,
  # this fails and the exception should be revisited.
  faostat <- .run_example("get_faostat_data")
  testthat::expect_true(rlang::has_name(faostat, "area"))
  testthat::expect_false(rlang::has_name(faostat, "area_code"))
  testthat::expect_type(faostat$area, "character")

  for (nm in c(
    "build_grazing_feed_footprint",
    "build_land_balance_footprint"
  )) {
    out <- .run_example(nm)
    testthat::expect_true(rlang::has_name(out, "area_code"), info = nm)
    testthat::expect_false(rlang::has_name(out, "year"), info = nm)
  }
})
