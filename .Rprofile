if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

# Suppresses the "unable to verify current time" NOTE in R CMD check, which
# inherits this session's environment. Set here rather than in a .Renviron: R
# reads a working-directory .Renviron INSTEAD of ~/.Renviron, never both, so a
# tracked one hides every WHEP_* path variable the local rasters need (#456).
Sys.setenv(`_R_CHECK_SYSTEM_CLOCK_` = 0)
