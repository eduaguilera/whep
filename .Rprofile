# R reads a working-directory .Rprofile INSTEAD of ~/.Rprofile, never both, so
# a tracked one here silently replaces the user profile -- the same trap
# .Renviron sprang in #456. On CI that is not merely a convenience lost:
# r-lib/actions' setup-r delivers `use-public-rspm: true` by writing
# `options(repos = c(RSPM = ..., CRAN = ...))` into ~/.Rprofile, and every
# workflow step runs Rscript with this directory as the working directory. With
# the RSPM entry shadowed, pak resolved source packages from cran.rstudio.com
# and BUILT all 141 dependencies -- arrow, sf, terra, igraph and the rest --
# turning a cold dependency install on ubuntu from ~2 minutes into 19.5, which
# is what pushed the R-CMD-check leg through its timeout (#1102).
#
# So chain-source the user profile first: this file must add to it, not replace
# it. No tryCatch -- a broken ~/.Rprofile aborts an R session started in any
# other directory, and hiding that here is how the shadowing went unnoticed for
# so long. The path comparison is what stops a session started in the home
# directory from sourcing itself.
local({
  user_profile <- path.expand("~/.Rprofile")
  if (
    file.exists(user_profile) &&
      !identical(
        normalizePath(user_profile, mustWork = FALSE),
        normalizePath(".Rprofile", mustWork = FALSE)
      )
  ) {
    source(user_profile)
  }
})

# Developer convenience: attach the package on session start so a plain
# `Rscript` at the repo root already has every function available.
#
# The load is wrapped because load_all() -> pkgload:::load_imports() asserts
# that every DESCRIPTION `Imports:` entry is already installed. CI installs
# dependencies by running Rscript with the repo root as the working directory,
# so R sources this file BEFORE the missing package can be installed: an
# unguarded load_all() makes the dependency-install step of every workflow fail
# the moment a new Import lands (#616). The failure is reported rather than
# swallowed -- a silent try() would let a developer believe the package is
# loaded when it is not.
if (requireNamespace("devtools", quietly = TRUE)) {
  tryCatch(
    devtools::load_all(),
    error = function(e) {
      message(
        "whep .Rprofile: devtools::load_all() failed, package NOT loaded: ",
        conditionMessage(e),
        "\nInstall the missing dependencies (e.g. pak::pak('.')) and restart."
      )
    }
  )
}

# Suppresses the "unable to verify current time" NOTE in R CMD check, which
# inherits this session's environment. Set here rather than in a .Renviron: R
# reads a working-directory .Renviron INSTEAD of ~/.Renviron, never both, so a
# tracked one hides every WHEP_* path variable the local rasters need (#456).
Sys.setenv(`_R_CHECK_SYSTEM_CLOCK_` = 0)
