# Helpers for FAOSTAT scraping tests -------------------------------------------

#' Skip when package:FAOSTAT is attached.
#'
#' The ISO3 tests assert that `.populate_iso3_code()` resolves without any
#' dependence on the search path (#520, #541). An attached FAOSTAT would put
#' its lazily loaded `FAOcountryProfile` there and hide a regression that went
#' back to reading it, so probe the search path instead of the CI environment.
skip_if_faostat_attached <- function() {
  if ("package:FAOSTAT" %in% search()) {
    testthat::skip("package:FAOSTAT is attached; unattached path not testable")
  }
  invisible()
}
