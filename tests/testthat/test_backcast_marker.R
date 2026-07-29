# add_polity_code() records which anchor produced its codes, and
# build_constant_territory_series() reads that to catch one silently-wrong combination: strict
# data-year matching on a series that is back-cast onto anchor-year territory.
#
# Why a marker at all. The reallocation spreads each value over its polity's own extent, so it
# needs the polity to name the territory the value was reported for. Under the default anchor that
# holds; under `backcast_anchor = -Inf` a 1900 row of a back-cast series carries a 1900-era polity
# while the value still describes 1961 borders, and the reallocation returns plausible numbers from
# the wrong extent. The two cases are indistinguishable from the data — which is why documenting
# the hazard was the first step and recording provenance is the second.
#
# An attribute, not a column: it is a fact about the resolution rather than about any row. It
# survives filter/mutate/select/rename/arrange and is dropped by summarise, so the failure mode is
# the safe one — no marker means no claim, which means no warning rather than a false one.

testthat::test_that("add_polity_code records the anchor it used", {
  default <- add_polity_code(data.frame(
    area_code = 10L,
    year = c(1900L, 1990L)
  ))
  strict <- add_polity_code(
    data.frame(area_code = 10L, year = c(1900L, 1990L)),
    backcast_anchor = -Inf
  )
  testthat::expect_equal(attr(default, "whep_backcast_anchor"), 1961L)
  testthat::expect_equal(attr(strict, "whep_backcast_anchor"), -Inf)
})

testthat::test_that("every polity-assigning path records the anchor", {
  # Set inside .add_polity_columns_dt(), the single point where the anchor is applied, so all seven
  # call sites inherit it rather than six of them silently omitting it. That matters because the
  # realistic use of the check is a constant-territory series of PRODUCTION, and production does not
  # go through add_polity_code() — it goes through the reporting-column helper.
  a <- function(x) attr(x, "whep_backcast_anchor", exact = TRUE)
  testthat::expect_equal(a(get_primary_production(example = TRUE)), 1961L)
  testthat::expect_equal(a(get_wide_cbs(example = TRUE)), 1961L)
  testthat::expect_equal(a(build_detailed_trade(example = TRUE)), 1961L)
})

testthat::test_that("the marker survives the verbs a caller puts between the two calls", {
  resolved <- add_polity_code(data.frame(
    area_code = 10L,
    year = c(1900L, 1990L)
  ))
  keep <- function(x) attr(x, "whep_backcast_anchor", exact = TRUE)
  testthat::expect_equal(
    keep(dplyr::filter(resolved, .data$year > 1800L)),
    1961L
  )
  testthat::expect_equal(keep(dplyr::mutate(resolved, value = 1)), 1961L)
  testthat::expect_equal(keep(dplyr::arrange(resolved, .data$year)), 1961L)
  testthat::expect_equal(keep(as.data.frame(resolved)), 1961L)
})

testthat::test_that("reallocation warns only for the unsafe combination", {
  # The marker check runs before the required-column validation, so omitting `value` exercises the
  # warning and then aborts immediately. That keeps the test to milliseconds instead of running a
  # full dasymetric reallocation, which is what a first version of this test did — it ran past a
  # nine-minute timeout for four assertions about a warning.
  frame <- function(anchor, years) {
    d <- data.frame(
      year = years,
      polity_code = rep("AUS-1901-2025", length(years))
    )
    if (!is.null(anchor)) {
      attr(d, "whep_backcast_anchor") <- anchor
    }
    d
  }
  quiet_error <- function(d) {
    tryCatch(
      build_constant_territory_series(d, ref_year = 2000L, verbose = FALSE),
      error = function(e) invisible(NULL)
    )
  }

  # Unsafe: strict resolution AND pre-anchor years present.
  testthat::expect_warning(
    quiet_error(frame(-Inf, c(1900L, 1990L))),
    "backcast_anchor"
  )

  # Safe in three distinct ways, each of which must stay silent:
  #   default resolution; strict but no pre-anchor rows; no marker at all.
  for (d in list(
    frame(1961L, c(1900L, 1990L)),
    frame(-Inf, c(1990L, 2000L)),
    frame(NULL, c(1900L, 1990L))
  )) {
    warned <- FALSE
    withCallingHandlers(
      quiet_error(d),
      warning = function(w) {
        if (grepl("backcast_anchor", conditionMessage(w))) {
          warned <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    )
    testthat::expect_false(warned)
  }
})
