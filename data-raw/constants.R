# This code is only for generating the internal data in case it's needed again
# The actual internal data (which can be used throughout the package) is stored
# in `R/sysdata.rda` and is directly available to developers when using
# `devtools::load_all()`. If you want to add more constants, rerun this file.

k_tonnes_per_livestock_unit <- 0.65

usethis::use_data(
  k_tonnes_per_livestock_unit,
  internal = TRUE,
  overwrite = TRUE
)
