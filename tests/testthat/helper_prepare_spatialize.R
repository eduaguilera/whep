# The helpers exercised by test_prepare_nitrogen.R and
# test_prepare_spatialize_irrigation.R live at script scope in
# inst/scripts/prepare_spatialize_all.R rather than in R/, so those tests have
# to source the script before they can see them.
#
# The script's path depends on the layout. A source checkout keeps
# `inst/scripts/`; an installed package flattens `inst/` onto the package root,
# so `inst/scripts/` does not exist under `R CMD check`, on CRAN or on
# r-universe. Both test files used to resolve the source-checkout path only,
# which meant that under `R CMD check` `file.exists()` was FALSE, nothing was
# sourced, and all 11 tests fell through their `exists(<helper>)` guards as
# skips -- reported as passes, never actually run on any check platform.
# `system.file()` resolves both layouts (pkgload shims it for a source
# checkout), so the tests now run everywhere.
#
# Sourcing is wrapped rather than left to abort because the script attaches
# ncdf4 at top level and ncdf4 is Suggests-only: where it is missing the script
# genuinely cannot load, and the callers' `exists()` guards should skip instead
# of erroring the whole file. That is why those guards stay.
.source_prepare_spatialize <- function() {
  path <- system.file(
    "scripts",
    "prepare_spatialize_all.R",
    package = "whep"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      "prepare_spatialize_all.R"
    )
  }
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }
  ok <- tryCatch(
    {
      sys.source(path, envir = topenv())
      TRUE
    },
    error = function(e) FALSE
  )
  invisible(ok)
}
