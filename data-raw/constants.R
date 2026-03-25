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
k_source_gadm <- "gadm"

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

gadm_path <- fs::path(polities_inputs_path, "gadm_geometries.gpkg")
k_gadm <- if (file.exists(gadm_path)) {
  sf::sf_use_s2(FALSE)
  sf::st_read(gadm_path, quiet = TRUE) |>
    sf::st_make_valid() |>
    sf::st_transform(crs = 4326) |>
    # Simplify to ~1 km tolerance to keep sysdata.rda manageable
    # (full-detail geometries are in the GPKG for scripts that need them)
    sf::st_simplify(dTolerance = 0.01, preserveTopology = TRUE)
} else {
  tibble::tibble(polity_name = character(), geometry = sf::st_sfc())
}

k_whep_polity_fixes <- polities_inputs_path |>
  fs::path("whep_fixes.csv") |>
  readr::read_csv()

k_polity_common_names <- polities_inputs_path |>
  fs::path("common_names.csv") |>
  readr::read_csv()

k_polity_codes <- polities_inputs_path |>
  fs::path("polity_codes.csv") |>
  readr::read_csv()

k_polity_first_year <- 1800
k_polity_last_year <- 2025

usethis::use_data(
  k_tonnes_per_livestock_unit,
  k_faostat_regions,
  k_unstats_m49,
  k_historical_m49,
  k_federico_tena_polities,
  k_cshapes,
  k_gadm,
  k_whep_polity_fixes,
  k_polity_common_names,
  k_polity_codes,
  k_polity_first_year,
  k_polity_last_year,
  k_source_faostat,
  k_source_m49,
  k_source_federico_tena,
  k_source_cshapes,
  k_source_whep,
  k_source_gadm,
  internal = TRUE,
  overwrite = TRUE
)
