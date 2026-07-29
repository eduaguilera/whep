# The companion test, test_polity_output_coverage.R, checks that nine named public
# functions carry reporting-polity columns. Nine is a LIST, and a list cannot notice
# a tenth function.
#
# Discovering them instead found that 22 more exported functions emit area-keyed
# output — the whole nitrogen, carbon and water balance family (`build_nitrogen_
# balance`, `build_carbon_balance`, `build_water_balance`, `build_n_inputs`,
# `build_urban_n`, `get_arable_permanent_land`, `read_luh2_landuse` and more) — none
# of which carries a `polity_code` column.
#
# That turns out to be a consistency gap rather than a correctness one: their
# `area_code` values ARE polity area codes, already resolved upstream in the
# pipeline. So this test asserts the property that actually matters and is cheap to
# check — an area-keyed public output must be keyed on POLITY areas, never on raw
# FAOSTAT reporting areas — and it discovers its subjects rather than listing them,
# so a new function is covered the day it is exported.

area_keyed_outputs <- function() {
  fns <- Filter(
    function(f) {
      fn <- tryCatch(get(f, envir = asNamespace("whep")), error = function(e) {
        NULL
      })
      is.function(fn) && "example" %in% names(formals(fn))
    },
    getNamespaceExports("whep")
  )
  out <- list()
  for (f in sort(fns)) {
    # Some examples need pins or local data that CI does not have; those simply are
    # not part of the sample rather than failing the test.
    res <- tryCatch(
      suppressWarnings(suppressMessages(
        do.call(get(f, envir = asNamespace("whep")), list(example = TRUE))
      )),
      error = function(e) NULL
    )
    if (!is.data.frame(res) || !"area_code" %in% names(res)) {
      next
    }
    out[[f]] <- res
  }
  out
}

test_that("every area-keyed public output is keyed on polity areas, not raw FAOSTAT areas", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  polity_areas <- unique(stats::na.omit(as.integer(cw$polity_area_code)))
  # Codes that exist as a FAOSTAT reporting area but are NEVER a polity area code.
  # Seeing one of these in a public output means the row was never collapsed to its
  # polity — the failure that had the two grassland sources keyed on different bases.
  raw_only <- setdiff(
    unique(stats::na.omit(as.integer(cw$area_code))),
    polity_areas
  )

  outputs <- area_keyed_outputs()
  testthat::skip_if(length(outputs) == 0, "no example outputs available")

  offenders <- character(0)
  for (nm in names(outputs)) {
    codes <- suppressWarnings(unique(as.integer(outputs[[nm]]$area_code)))
    codes <- codes[!is.na(codes)]
    bad <- intersect(codes, raw_only)
    if (length(bad) > 0) {
      offenders <- c(
        offenders,
        sprintf(
          "%s emits raw FAOSTAT area code(s) %s",
          nm,
          paste(utils::head(sort(bad), 5), collapse = ", ")
        )
      )
    }
  }
  expect_equal(
    length(offenders),
    0L,
    info = paste(offenders, collapse = "; ")
  )
})

test_that("the sample is not empty, so the check above cannot pass vacuously", {
  # A discovery-based test that discovers nothing passes for free, so the sample size
  # is pinned: a change that breaks the discovery fails loudly instead of going quiet.
  # 31 area-keyed outputs were found when this was written, in about a second — the
  # `example = TRUE` paths read bundled files under inst/extdata rather than pins, so
  # the count does not depend on data CI lacks. The floor is set a little below 31 to
  # tolerate an example being retired without a false alarm.
  outputs <- area_keyed_outputs()
  expect_gte(length(outputs), 25L)
})
