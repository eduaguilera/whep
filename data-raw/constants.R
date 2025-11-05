# This code is only for generating the internal data in case it's needed again
# The actual internal data (which can be used throughout the package) is stored
# in `R/sysdata.rda` and is directly available to developers when using
# `devtools::load_all()`. If you want to add more constants, rerun this file.

k_tonnes_per_livestock_unit <- 0.65

# Saving only for transparency
polities_inputs_path <- here::here("data-raw", "polities_inputs")

k_source_faostat <- "faostat"
k_source_m49 <- "m49"
k_source_federico_tena <- "federico_tena"
k_source_cshapes <- "cshapes"
k_source_whep <- "whep"

k_faostat_regions <- .clean_faostat_regions()
k_faostat_regions |>
  readr::write_csv(file.path(polities_inputs_path, "faostat_regions.csv"))

k_unstats_m49 <- .clean_unstats_m49()
k_unstats_m49 |>
  readr::write_csv(file.path(polities_inputs_path, "unstats_m49.csv"))

k_historical_m49 <- .clean_historical_m49()
k_historical_m49 |>
  readr::write_csv(file.path(polities_inputs_path, "historical_m49.csv"))

k_federico_tena_polities <- .clean_federico_tena_polities()
k_federico_tena_polities |>
  readr::write_csv(
    file.path(polities_inputs_path, "federico_tena_polities.csv")
  )

k_cshapes <- .clean_cshapes()
k_cshapes |>
  # Can't affort also saving geometry, slow and too large for repository
  sf::st_set_geometry(NULL) |>
  readr::write_csv(
    file.path(polities_inputs_path, "cshapes.csv")
  )

k_polity_common_names <- polities_inputs_path |>
  fs::path("common_names.csv") |>
  readr::read_csv()

k_polity_names_codes <- polities_inputs_path |>
  fs::path("polity_names_codes.csv") |>
  readr::read_csv()

k_polity_alias_table <- polities_inputs_path |>
  fs::path("alias_manual.csv") |>
  readr::read_csv() |>
  .expand_alias_table()

k_polity_first_year <- 1800
k_polity_last_year <- 2025

usethis::use_data(
  k_tonnes_per_livestock_unit,
  k_faostat_regions,
  k_unstats_m49,
  k_historical_m49,
  k_federico_tena_polities,
  k_cshapes,
  k_polity_alias_table,
  k_polity_common_names,
  k_polity_names_codes,
  k_polity_first_year,
  k_polity_last_year,
  k_source_faostat,
  k_source_m49,
  k_source_federico_tena,
  k_source_cshapes,
  k_source_whep,
  internal = TRUE,
  overwrite = TRUE
)
