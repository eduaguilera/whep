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
