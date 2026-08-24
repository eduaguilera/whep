# The helpers exercised by test_prepare_nitrogen.R and
# test_prepare_spatialize_irrigation.R live at script scope in
# inst/scripts/prepare_spatialize_all.R rather than in R/, so those tests have
# to source the script before they can see them.
#
# There are three layouts, and only two of them have the script at all:
#
#   1. a source checkout, where it sits at `inst/scripts/`;
#   2. a package installed straight from the checkout (`R CMD INSTALL .`),
#      where `inst/` is flattened onto the package root and `system.file()`
#      finds it -- `.Rbuildignore` does not apply to this route;
#   3. a package built into a tarball first, which is what `R CMD check`,
#      r-universe and CRAN all do. `^inst/scripts$` is in `.Rbuildignore`, so
#      the script is not in the tarball and therefore not in the installed
#      package. Nothing can source it there.
#
# whep#402 read the CI skips as a path bug, and 30ef8fc9 fixed the path, which
# bought layout 2 but not layout 3 -- so its claim that the tests now run under
# check was wrong, and they still skip on every check platform. The path
# resolution is kept because it is correct as far as it goes; what changes here
# is that the unavailable case is reported for what it is instead of looking
# like an ordinary resource gate. See test_prepare_nitrogen.R for the
# assertion, and #402 for the two ways to close the gap for real (ship the
# script, or move the helpers into R/) -- both are the maintainer's call.
#
# Returns "" when no layout has the script, so callers can tell "absent" from
# "present but broken". `system.file()` is tried first because pkgload shims it
# for a source checkout; the literal path is the fallback for a plain
# `testthat::test_dir()` against a checkout with no pkgload attached.
.prepare_spatialize_path <- function() {
  path <- system.file(
    "scripts",
    "prepare_spatialize_all.R",
    package = "whep"
  )
  if (nzchar(path) && file.exists(path)) {
    return(path)
  }
  path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "scripts",
    "prepare_spatialize_all.R"
  )
  if (file.exists(path)) {
    return(path)
  }
  ""
}

# Sourcing is wrapped rather than left to abort because the script attaches
# ncdf4 at top level and ncdf4 is Suggests-only: where it is missing the script
# genuinely cannot load, and the callers' `exists()` guards should skip instead
# of erroring the whole file. That is why those guards stay.
.source_prepare_spatialize <- function() {
  path <- .prepare_spatialize_path()
  if (!nzchar(path)) {
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

# The one reason string for "the script is not in this build", so the skip
# names its cause rather than leaving the reader to guess.
.skip_no_prepare_spatialize <- function() {
  testthat::skip(paste(
    "inst/scripts/prepare_spatialize_all.R is not in the built package",
    "(`^inst/scripts$` is in .Rbuildignore), so the script-scope helpers",
    "cannot be reached from a tarball install -- whep#402."
  ))
}

# The per-test guard. It replaces a bare
# `skip_if_not(exists(<nm>, mode = "function"))`, which reported every cause as
# the same anonymous "is not TRUE" and so made an absent script, an absent
# ncdf4 and a genuinely missing helper indistinguishable in the CI skip list.
# Here the first two skip with their own reason and the third fails, because
# once the script is present and loadable a helper it defines cannot be missing.
.need_spatialize_helper <- function(nm) {
  if (!nzchar(.prepare_spatialize_path())) {
    .skip_no_prepare_spatialize()
  }
  testthat::skip_if_not_installed("ncdf4")
  if (!exists(nm, envir = parent.frame(), mode = "function")) {
    testthat::fail(paste0(
      "`",
      nm,
      "()` is not defined after sourcing ",
      "inst/scripts/prepare_spatialize_all.R, which is present and loadable. ",
      "Either it was renamed or removed there, or the sourcing is broken."
    ))
  }
  invisible(TRUE)
}
