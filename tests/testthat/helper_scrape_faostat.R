# Helpers for FAOSTAT scraping tests -------------------------------------------

#' Skip when package:FAOSTAT is attached.
#'
#' The ISO3 tests assert that `.populate_iso3_code()` works with prefixed
#' access only (#520). An attached FAOSTAT would put its lazily loaded
#' `FAOcountryProfile` on the search path and hide the very failure the tests
#' guard against, so probe the search path instead of the CI environment.
skip_if_faostat_attached <- function() {
  if ("package:FAOSTAT" %in% search()) {
    testthat::skip("package:FAOSTAT is attached; unattached path not testable")
  }
  invisible()
}
