# Do the registered `spatialize-country-areas`, `spatialize-crop-patterns`
# and `spatialize-multicropping` pins still carry the harvested area that
# production publishes and `cft_mapping.csv` maps? (issue #1367)
#
# The three pins are frozen builds of Sections 2, 3 and 4b of
# `inst/scripts/prepare_spatialize_all.R`. Section 2 keeps only the
# production rows whose `item_prod_code` `cft_mapping.csv` lists, and Section 3
# keys each EarthStat raster on the code `earthstat_mapping.csv` gives it. A fix
# to either mapping, or to production, moves nothing until the pins are rebuilt:
# after whep#1292 moved coconut, linum, hemp and kapok onto the codes that hold
# their area, the registered country-areas pin still had no row for any of them
# in any year, so 13.9 Mha of 2010 harvested area never reached the grid.
#
# Checks, per year:
#
# 1. every mapped code production gives harvested area has rows in
#    `country_areas`, and no code is there that production does not carry;
# 2. world harvested area per mapped code matches production;
# 3. every code in `country_areas` that `earthstat_mapping.csv` gives a raster
#    has a non-zero crop pattern, which is what `build_gridded_landuse()` needs
#    to place it (a code without one is warned and dropped). Codes with no
#    EarthStat raster at all (dry chillies 689, leeks 407, ...; issue #877)
#    are a data limitation, listed but not failed;
# 4. irrigated area is non-missing and never exceeds harvested area;
# 5. `multicropping` covers every year `country_areas` does.
#
# ## Usage
#
#   Rscript --no-init-file validation/spatialize_crop_pins.R
#
#   VAL_SCP_YEARS      optional; comma-separated years. Default 2010,2020.
#   VAL_SCP_TOLERANCE  optional; relative tolerance on check 2. Default 1e-6.
#   VAL_SCP_DIR        optional; a directory holding candidate
#                      `country_areas.parquet`, `crop_patterns.parquet` and
#                      `multicropping.parquet` to check instead of the
#                      registered pins (a rebuild, before it is uploaded).
#
# Exits non-zero when any check fails, printing every table either way.

devtools::load_all(".", quiet = TRUE)

years <- as.integer(strsplit(Sys.getenv("VAL_SCP_YEARS", "2010,2020"), ",")[[
  1
]])
tolerance <- as.numeric(Sys.getenv("VAL_SCP_TOLERANCE", "1e-6"))
candidate_dir <- Sys.getenv("VAL_SCP_DIR", "")

.read_candidate <- function(file_name, alias) {
  if (nzchar(candidate_dir)) {
    cli::cli_alert_info("Reading {.path {file.path(candidate_dir, file_name)}}")
    return(nanoparquet::read_parquet(file.path(candidate_dir, file_name)))
  }
  cli::cli_alert_info("Reading registered pin {.val {alias}}")
  whep_read_file(alias)
}

mapping <- readr::read_csv(
  system.file("extdata", "cft_mapping.csv", package = "whep"),
  show_col_types = FALSE
)

production <- build_primary_production(
  start_year = min(years),
  end_year = max(years)
) |>
  dplyr::filter(
    .data$unit == "ha",
    .data$year %in% years,
    .data$value > 0,
    as.integer(.data$item_prod_code) %in% mapping$item_prod_code
  ) |>
  dplyr::summarise(
    production_ha = sum(.data$value),
    .by = c("year", "item_prod_code")
  ) |>
  dplyr::mutate(item_prod_code = as.integer(.data$item_prod_code))

country_areas <- .read_candidate(
  "country_areas.parquet",
  "spatialize-country-areas"
) |>
  tibble::as_tibble() |>
  dplyr::mutate(item_prod_code = as.integer(.data$item_prod_code))
crop_patterns <- .read_candidate(
  "crop_patterns.parquet",
  "spatialize-crop-patterns"
)
multicropping <- .read_candidate(
  "multicropping.parquet",
  "spatialize-multicropping"
)

# Checks 1 and 2: per-code world area, pin against production.
pin_items <- country_areas |>
  dplyr::filter(.data$year %in% years) |>
  dplyr::summarise(
    pin_ha = sum(.data$harvested_area_ha),
    .by = c("year", "item_prod_code")
  )
items <- dplyr::full_join(
  production,
  pin_items,
  by = c("year", "item_prod_code")
) |>
  dplyr::mutate(relative_difference = .data$pin_ha / .data$production_ha - 1)

missing_codes <- dplyr::filter(items, is.na(.data$pin_ha))
extra_codes <- dplyr::filter(items, is.na(.data$production_ha))
off_codes <- dplyr::filter(
  items,
  !is.na(.data$relative_difference),
  abs(.data$relative_difference) > tolerance
)

world <- items |>
  dplyr::summarise(
    production_mha = sum(.data$production_ha, na.rm = TRUE) / 1e6,
    pin_mha = sum(.data$pin_ha, na.rm = TRUE) / 1e6,
    .by = "year"
  ) |>
  dplyr::left_join(
    country_areas |>
      dplyr::filter(.data$year %in% years) |>
      dplyr::summarise(
        pin_irrigated_mha = sum(.data$irrigated_area_ha) / 1e6,
        .by = "year"
      ),
    by = "year"
  )
cli::cli_h2("World harvested area, mapped codes")
print(as.data.frame(world), digits = 10)

cli::cli_h2("Mapped codes production carries that the pin lacks")
print(as.data.frame(missing_codes), digits = 10)
cli::cli_h2("Codes in the pin that production does not carry")
print(as.data.frame(extra_codes), digits = 10)
cli::cli_h2("Codes whose world area differs from production")
print(as.data.frame(off_codes), digits = 10)

# Check 3: a pattern for every code the areas carry.
patterned <- crop_patterns |>
  dplyr::filter(.data$harvest_fraction > 0) |>
  dplyr::distinct(.data$item_prod_code) |>
  dplyr::pull() |>
  as.integer()
rastered <- readr::read_csv(
  system.file("extdata", "earthstat_mapping.csv", package = "whep"),
  show_col_types = FALSE
) |>
  dplyr::filter(!is.na(.data$item_prod_code)) |>
  dplyr::pull("item_prod_code") |>
  as.integer()
no_pattern <- pin_items |>
  dplyr::filter(!.data$item_prod_code %in% patterned)
unpatterned <- dplyr::filter(no_pattern, .data$item_prod_code %in% rastered)
cli::cli_h2("Codes with an EarthStat raster but no crop pattern")
print(as.data.frame(unpatterned), digits = 10)
cli::cli_h2("Codes with no EarthStat raster at all (not failed)")
print(
  as.data.frame(dplyr::filter(no_pattern, !.data$item_prod_code %in% rastered)),
  digits = 10
)

# Check 4: irrigation is present and bounded.
bad_irrigation <- country_areas |>
  dplyr::filter(
    is.na(.data$irrigated_area_ha) |
      .data$irrigated_area_ha > .data$harvested_area_ha * (1 + 1e-9)
  )

# Check 5: multicropping spans the same years.
mc_gap <- setdiff(unique(country_areas$year), unique(multicropping$year))

failures <- c(
  missing = nrow(missing_codes),
  extra = nrow(extra_codes),
  off = nrow(off_codes),
  unpatterned = nrow(unpatterned),
  bad_irrigation = nrow(bad_irrigation),
  multicropping_years = length(mc_gap)
)
cli::cli_h2("Summary")
print(failures)
if (any(failures > 0L)) {
  cli::cli_abort(
    "Spatialize crop pins fail {sum(failures > 0L)} of
     {length(failures)} checks: {.val {names(failures)[failures > 0L]}}."
  )
}
cli::cli_alert_success("All spatialize crop-pin checks pass.")
