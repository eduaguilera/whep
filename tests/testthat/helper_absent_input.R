# The test shape whep#1034 asks every supplied-check to ship with.
#
# Its subject is the INADEQUACY OF THE IDENTITY, not the identity. Each of the
# three defects behind whep#1034 satisfied its own consistency check perfectly
# while an input was missing, so a test that only asserts the identity is
# evidence of nothing. This helper asserts both halves at once: that the
# identity still holds on a vacuously filled input, and that the guard fires
# anyway.
#
# `identity` and `guard` are promises. They are forced here, in the caller's
# environment, so the fixture is built once in the test and used twice.
expect_supplied_guard <- function(
  identity,
  guard,
  class = "whep_absent_input",
  condition = c("error", "warning")
) {
  condition <- match.arg(condition)
  testthat::expect_true(
    identity,
    label = "the identity holds on a vacuously filled input"
  )
  if (identical(condition, "error")) {
    testthat::expect_error(guard, class = class)
  } else {
    testthat::expect_warning(guard, class = class)
  }
}
