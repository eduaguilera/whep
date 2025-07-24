# This code is only for generating the internal data in case it's needed again
# The actual internal data (which can be used throughout the package) is stored
# in `R/sysdata.rda` and is directly available to developers when using
# `devtools::load_all()`. If you want to add more constants, rerun this file.

k_tonnes_per_livestock_unit <- 0.65

# Saving only for transparency
polities_inputs_path <- here::here("data-raw", "polities_inputs")

k_faostat_regions <- .clean_faostat_regions()
k_faostat_regions |>
  readr::write_csv(file.path(polities_inputs_path, "faostat_regions.csv"))

k_unstats_m49 <- .clean_unstats_m49()
k_unstats_m49 |>
  readr::write_csv(file.path(polities_inputs_path, "unstats_m49.csv"))

usethis::use_data(
  k_tonnes_per_livestock_unit,
  k_faostat_regions,
  k_unstats_m49,
  internal = TRUE,
  overwrite = TRUE
)
