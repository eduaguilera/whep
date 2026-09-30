# The test shape whep#1073 asks every lattice guard to ship with, and the
# sibling of the whep#1034 supplied-guard helper.
#
# Its subject is the INADEQUACY OF THE AGGREGATE, not the aggregate. An absent
# row leaves nothing NA, removes no column and breaks no identity, so a test
# that only asserts the identity is evidence of nothing at all: it passes just
# as happily over eleven months as over twelve. This helper asserts both halves
# at once -- that the aggregate is still perfectly well-formed over the
# incomplete lattice, and that the guard fires anyway.
#
# `well_formed` and `guard` are promises, forced here in the caller's
# environment, so the short-lattice fixture is built once and used twice.
expect_lattice_guard <- function(
  well_formed,
  guard,
  class = "whep_incomplete_lattice",
  condition = c("error", "warning")
) {
  condition <- match.arg(condition)
  testthat::expect_true(
    well_formed,
    label = "the aggregate is well-formed over the incomplete lattice"
  )
  if (identical(condition, "error")) {
    testthat::expect_error(guard, class = class)
  } else {
    testthat::expect_warning(guard, class = class)
  }
}

# A cell-month lattice with `n_cells` cells and one year, from which `drop`
# names the months to remove for the FIRST cell only -- so the incomplete
# groups are a strict subset and a test can assert that the complete ones came
# through untouched.
.lattice_month_fixture <- function(n_cells = 2L, drop = integer()) {
  cells <- tibble::tibble(
    lon = seq(0.25, by = 0.5, length.out = n_cells),
    lat = 0.25
  )
  full <- tidyr::expand_grid(cells, year = 2000L, month = 1:12)
  dplyr::mutate(full, value = month) |>
    dplyr::filter(!(lon == cells$lon[[1L]] & month %in% drop))
}

# The lattice condition out of `expr`, unwrapped from any chain a purrr::map()
# or a pipeline wrapped it in. testthat's expect_error(class = ) already walks
# the chain; catch_cnd() does not, and a `purrr_error_indexed` parent is what
# every wired site produces.
.lattice_cnd <- function(expr, class = "whep_incomplete_lattice") {
  cnd <- rlang::catch_cnd(expr, classes = "error")
  while (!is.null(cnd) && !inherits(cnd, class)) {
    cnd <- cnd$parent
  }
  cnd
}
