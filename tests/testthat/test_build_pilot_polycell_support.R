# The pilot script's guards, read without running the script -------------
#
# `inst/scripts/build_pilot_polycell_support.R` ends in `.bps_main()`, so it
# cannot be sourced: sourcing runs the build and aborts on the environment
# variables it requires. The same technique `test_data_raw_freshness.R` uses
# on `data-raw/` applies here -- parse the file and evaluate only the named
# top-level definitions -- so a guard can be called on its own.

.bps_assigned_name <- function(expr) {
  assigned <- is.call(expr) &&
    identical(as.character(expr[[1]]), "<-") &&
    is.name(expr[[2]])
  if (assigned) rlang::as_name(expr[[2]]) else NA_character_
}

.bps_definitions <- function(defs) {
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "build_pilot_polycell_support.R"
  )
  testthat::skip_if_not(
    file.exists(path),
    "inst/scripts is not in this build (^inst/scripts$ is .Rbuildignore'd)."
  )
  exprs <- as.list(parse(path))
  env <- new.env(parent = globalenv())
  wanted <- exprs[purrr::map_chr(exprs, .bps_assigned_name) %in% defs]
  purrr::walk(wanted, eval, envir = env)
  env
}

testthat::test_that("the subset gate names only the side that is wrong", {
  # A REFUSAL THAT ASSERTS NOTHING IS WORSE THAN NO REFUSAL. Both bullets
  # were emitted unconditionally, so the side with no content rendered as
  # "Unexpected: ." -- an `x` bullet claiming a second problem that does not
  # exist. Measured on a real Argentina build, where
  # `ARG-SANTACRUZ-1955-2025` is invalid under the spherical engine and so
  # receives no polycell: the operator's first line was the empty one.
  env <- .bps_definitions(".bps_subset_gate")
  err <- testthat::expect_error(
    env$.bps_subset_gate(
      c("A-1900-2000", "B-1900-2000"),
      data.frame(polity_code = "A-1900-2000", stringsAsFactors = FALSE)
    )
  )
  refusal <- conditionMessage(err)
  testthat::expect_match(refusal, "B-1900-2000")
  testthat::expect_no_match(refusal, "Unexpected")

  # And the mirror case, so the fix cannot be a blanket drop of one bullet.
  err2 <- testthat::expect_error(
    env$.bps_subset_gate(
      "A-1900-2000",
      data.frame(
        polity_code = c("A-1900-2000", "C-1900-2000"),
        stringsAsFactors = FALSE
      )
    )
  )
  refusal2 <- conditionMessage(err2)
  testthat::expect_match(refusal2, "C-1900-2000")
  testthat::expect_no_match(refusal2, "Missing")
})
