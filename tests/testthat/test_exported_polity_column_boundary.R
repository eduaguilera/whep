# Which exported functions attach polity columns, and which only inherit a polity-derived key?
#
# `check_like_ci.R` gates that the EIGHT primary builders document their polity columns, and
# `test_builder_polity_columns_populated.R` gates that five of them emit those columns
# populated. Neither says anything about the other exports, and "not covered" is the state in
# which a boundary drifts without anyone deciding to move it.
#
# Censused: of 172 exports, 64 take `example = TRUE` and can be run cheaply; 32 of those return
# an area-keyed frame. **10 carry polity columns, 22 do not.** The 22 are not broken -- their
# `area_code` is the polity-DERIVED key, because `get_primary_production()` emits
# `polity_area_code` AS its `area_code`, so their rows are attributable. They are simply not
# self-describing: a consumer of `build_nitrogen_balance()` must re-join the crosswalk to learn
# whose territory a row is.
#
# Whether they SHOULD carry the columns is whep#424 -- four columns on 21 public outputs, which
# widens the documented contract from 8 functions to 29 and the documentation gate with it. One
# of the 22 should certainly NOT: `get_faostat_data()` returns `area` as a raw FAOSTAT NAME
# ("Portugal"), because it is the pre-resolution reader and resolving is the caller's job. A
# sweep that mechanically added columns everywhere would get that one wrong.
#
# So this pins the boundary rather than asserting either side is right. If #424 is implemented
# the list shrinks and this test fails, which is the intended signal; if a new export quietly
# joins the 22, that surfaces as a change rather than as silence.
#
# Runs on the example path only, which is what `example = TRUE` exists for and keeps this fast.

.area_keyed_exports <- function() {
  ns <- asNamespace("whep")
  ex <- getNamespaceExports("whep")
  fns <- ex[vapply(ex, function(n) is.function(get(n, envir = ns)), logical(1))]
  sort(fns[vapply(
    fns,
    function(n) "example" %in% names(formals(get(n, envir = ns))),
    logical(1)
  )])
}

.classify_export <- function(name) {
  out <- tryCatch(
    suppressWarnings(suppressMessages(do.call(name, list(example = TRUE)))),
    error = function(e) NULL
  )
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

testthat::test_that("the polity-column boundary among exports is where whep#424 says", {
  testthat::skip_on_ci()

  fns <- .area_keyed_exports()
  # Non-vacuous: a tiny export set would make every count below meaningless.
  testthat::expect_gt(length(fns), 50L)

  kinds <- vapply(fns, .classify_export, character(1))

  with_polity <- sort(names(kinds)[kinds == "with_polity"])
  without <- sort(names(kinds)[kinds == "without_polity"])

  # Pinned by identity, not by count: 22 -> 23 says nothing about which export joined.
  testthat::expect_setequal(
    without,
    c(
      "build_carbon_balance",
      "build_carbon_inputs",
      "build_crop_soil_n2o_extension",
      "build_energy_co2_extension",
      "build_feed_demand",
      "build_feed_intake_local",
      "build_grass_natural_carbon_inputs",
      "build_grassland_land_extension",
      "build_grazing_feed_footprint",
      "build_land_balance_footprint",
      "build_livestock_ghg_extension",
      "build_n_deposition",
      "build_n_inputs",
      "build_nitrogen_balance",
      "build_soil_carbon_inputs",
      "build_urban_n",
      "build_water_balance",
      "get_arable_permanent_land",
      "get_crop_land_extension",
      "get_faostat_data",
      "get_soc_climate_drivers",
      "read_luh2_landuse"
    )
  )

  # And the ten that do, so an export cannot lose its columns unnoticed either.
  testthat::expect_gte(length(with_polity), 10L)
  for (nm in c(
    "get_wide_cbs",
    "build_primary_production",
    "build_supply_use"
  )) {
    testthat::expect_true(
      nm %in% with_polity,
      info = paste(nm, "is a primary builder and must carry polity columns")
    )
  }
})

testthat::test_that("the year needed to resolve them is present for all but two", {
  # The cost of whep#424, measured rather than asserted: `.add_reporting_polity_columns()`
  # resolves a polity, and resolution needs a year. 19 of the 21 candidates have one, so they
  # are a one-line pipe each. Two do not -- `build_grazing_feed_footprint` and
  # `build_land_balance_footprint` return `area_code, item_cbs_code, value, method`, a footprint
  # aggregated over time -- so attaching a reporting polity there means CHOOSING a year, which
  # is a modelling decision and not plumbing.
  #
  # Asserted so the split survives: it is what lets the cheap 19 proceed without waiting on the
  # interesting 2.
  testthat::skip_on_ci()

  yearless <- character()
  for (nm in c(
    "build_grazing_feed_footprint",
    "build_land_balance_footprint"
  )) {
    out <- tryCatch(
      suppressWarnings(suppressMessages(do.call(nm, list(example = TRUE)))),
      error = function(e) NULL
    )
    testthat::skip_if(is.null(out), paste(nm, "example unavailable"))
    if (!any(grepl("^year$", names(as.data.frame(out)), ignore.case = TRUE))) {
      yearless <- c(yearless, nm)
    }
  }
  testthat::expect_setequal(
    yearless,
    c("build_grazing_feed_footprint", "build_land_balance_footprint")
  )
})
