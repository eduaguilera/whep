# -----------------------------------------------------------------------
# prepare_mirca_inputs.R
#
# Reads MIRCA2000 monthly irrigated/rainfed crop area grids and
# produces two outputs:
#
#   1. mirca_irrigation_country.parquet
#      Country-level irrigated fraction per FAOSTAT crop.
#      Columns: area_code, item_prod_code, irrig_frac
#      Used by prepare_spatialize_inputs.R to distribute country
#      irrigated area across individual crops.
#
#   2. mirca_irrigation_patterns.parquet
#      Gridded crop-specific irrigated fraction at 0.5 degrees.
#      Columns: lon, lat, item_prod_code, irrig_ha, rainfed_ha
#      Can be used for spatial irrigation validation.
#
# Data source:
#   MIRCA2000 v1.1 (Portmann, Siebert & Döll, 2010)
#   Monthly irrigated and rainfed crop areas around the year 2000.
#   26 crop classes, 5 arc-min resolution.
#   doi:10.1029/2008GB003435
#
# File format:
#   Binary files, 4320 x 2160 x 12 months, 4-byte float, little-endian.
#   Files are either raw .flt (crops 1-4, in subdirectories) or
#   gzip-compressed .flt.gz (crops 5-26).
#
# MIRCA-to-FAOSTAT mapping:
#   Crops 1-16, 19-23 map 1:1 to a FAOSTAT item_prod_code.
#   Crops 17 (Pulses), 18 (Citrus), 24 (Others perennial),
#   25 (Fodder grasses), 26 (Others annual) map to groups of
#   FAOSTAT codes via mirca_mapping.csv.
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Configuration ===================================================

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"
mirca_dir <- file.path(l_files_dir, "Irrigation maps_CIRCA-2000")
output_dir <- file.path(l_files_dir, "whep", "inputs")
target_res <- 0.5

# MIRCA grid dimensions (5 arc-min)
NCOLS <- 4320L
NROWS <- 2160L
NMONTHS <- 12L
NCELLS <- NCOLS * NROWS

# ==== Helper functions ================================================

.find_extdata_file <- function(filename) {
  local_path <- file.path("inst", "extdata", filename)
  if (file.exists(local_path)) return(local_path)
  pkg_path <- system.file("extdata", filename, package = "whep")
  if (nchar(pkg_path) > 0) return(pkg_path)
  cli::cli_abort("Cannot find {filename} in inst/extdata or package.")
}

#' Read one MIRCA binary file (12-month growing area grid).
#'
#' Each file has 4320 x 2160 cells x 12 months = 111,974,400 float values.
#' Returns annual maximum (max across 12 months) as a terra SpatRaster.
#' @noRd
.read_mirca_binary <- function(fpath) {
  is_gz <- grepl("\\.gz$", fpath)

  if (is_gz) {
    con <- gzfile(fpath, "rb")
    on.exit(close(con))
    raw_vals <- readBin(con, "double", n = NCELLS * NMONTHS, size = 4L)
  } else {
    raw_vals <- readBin(fpath, "double", n = NCELLS * NMONTHS, size = 4L)
  }

  if (length(raw_vals) != NCELLS * NMONTHS) {
    cli::cli_abort(
      "Expected {NCELLS * NMONTHS} values, got {length(raw_vals)} from {fpath}"
    )
  }

  # Take max across 12 months per cell (memory-efficient)
  # Read values as 12 monthly layers in sequence
  annual <- raw_vals[seq_len(NCELLS)]  # month 1
  for (m in 2:NMONTHS) {
    idx <- ((m - 1L) * NCELLS + 1L):(m * NCELLS)
    annual <- pmax(annual, raw_vals[idx])
  }
  rm(raw_vals)

  # Create raster (N→S, W→E, matching standard global grid)
  r <- terra::rast(
    nrows = NROWS, ncols = NCOLS,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90
  )
  terra::values(r) <- annual
  rm(annual)
  r
}

#' Find the MIRCA binary file for a given crop and type.
#'
#' Handles the messy directory layout: crops 1-4 are in subdirectories
#' (sometimes with ~ suffix), crops 5-26 are .flt.gz at root level.
#' @noRd
.find_mirca_file <- function(mirca_dir, crop_num, type) {
  # type is "irrigated" or "rainfed"
  fname <- sprintf("crop_%02d_%s_12", crop_num, type)

  # Option 1: gzipped at root level
  gz_path <- file.path(mirca_dir, paste0(fname, ".flt.gz"))
  if (file.exists(gz_path)) return(gz_path)

  # Option 2: uncompressed in subdirectory
  # Pattern: subdir/crop_XX_type_12.flt/crop_XX_type_12.flt
  sub_dir <- file.path(mirca_dir, type, paste0(fname, ".flt"))
  sub_file <- file.path(sub_dir, paste0(fname, ".flt"))
  if (file.exists(sub_file)) return(sub_file)

  # Option 3: subdirectory with ~ suffix (crop_01)
  sub_dir_tilde <- file.path(mirca_dir, type, paste0(fname, ".flt~"))
  sub_file_tilde <- file.path(sub_dir_tilde, paste0(fname, ".flt"))
  if (file.exists(sub_file_tilde)) return(sub_file_tilde)

  # Option 4: standalone file in subdirectory
  standalone <- file.path(mirca_dir, type, paste0(fname, ".flt"))
  if (file.exists(standalone)) return(standalone)

  # Also check rainfed/crop_01_rainfed_12/ and crop_01_rainfed_12.flt
  alt_dir <- file.path(mirca_dir, type, fname)
  alt_file <- file.path(alt_dir, paste0(fname, ".flt"))
  if (file.exists(alt_file)) return(alt_file)

  NULL
}

#' Aggregate a 5 arc-min raster to target resolution (sum of ha).
#' @noRd
.aggregate_mirca <- function(r, target_res) {
  agg_factor <- as.integer(target_res / (5 / 60))
  terra::aggregate(r, fact = agg_factor, fun = "sum", na.rm = TRUE)
}

#' Convert raster to tibble with lon, lat, value.
#' @noRd
.raster_to_tibble <- function(r, value_name) {
  coords <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  tibble::tibble(
    lon = round(coords[, 1], 2),
    lat = round(coords[, 2], 2),
    value = terra::values(r)[, 1]
  ) |>
    rlang::set_names(c("lon", "lat", value_name))
}

# ==== Read MIRCA mapping =============================================

read_mirca_mapping <- function() {
  mirca_map <- readr::read_csv(
    .find_extdata_file("mirca_mapping.csv"),
    show_col_types = FALSE
  )

  # Read CFT mapping to expand group crops to individual FAOSTAT codes
  cft_map <- readr::read_csv(
    .find_extdata_file("cft_mapping.csv"),
    show_col_types = FALSE
  )
  cft_map <- dplyr::select(cft_map, item_prod_code, cft_name)

  # Build expansion for group crops
  # mirca_group values: pulses, citrus, others_perennial, fodder,
  #   others_annual
  # Map to cft_name in cft_mapping.csv where possible.
  # "fodder" (MIRCA 25) has no FAOSTAT equivalent — dropped.

  # Citrus codes (FAOSTAT): direct list of citrus item codes
  citrus_codes <- c(490L, 495L, 497L, 507L, 512L)

  group_to_cft <- tibble::tribble(
    ~mirca_group,       ~cft_name,
    "pulses",           "pulses",
    "others_perennial", "others_perennial",
    "others_annual",    "others_annual"
  )

  # Direct 1:1 mappings (mirca crops with item_prod_code)
  direct <- mirca_map |>
    dplyr::filter(!is.na(item_prod_code)) |>
    dplyr::select(mirca_class, item_prod_code)

  # Citrus group: expand to citrus FAOSTAT codes only
  citrus_class <- mirca_map$mirca_class[mirca_map$mirca_group == "citrus" & !is.na(mirca_map$mirca_group)]
  citrus_expanded <- tibble::tibble(
    mirca_class = rep(citrus_class, length(citrus_codes)),
    item_prod_code = citrus_codes
  )

  # Other group mappings via cft_name
  group_rows <- mirca_map |>
    dplyr::filter(is.na(item_prod_code), !is.na(mirca_group)) |>
    dplyr::filter(mirca_group %in% group_to_cft$mirca_group) |>
    dplyr::select(mirca_class, mirca_group) |>
    dplyr::inner_join(group_to_cft, by = "mirca_group") |>
    dplyr::inner_join(cft_map, by = "cft_name", relationship = "many-to-many") |>
    dplyr::select(mirca_class, item_prod_code)

  dplyr::bind_rows(direct, citrus_expanded, group_rows)
}

# ==== Main processing =================================================

cli::cli_h1("MIRCA2000 irrigation pattern preparation")

# Read country grid for country assignment
country_grid_path <- file.path(output_dir, "country_grid.parquet")
if (!file.exists(country_grid_path)) {
  cli::cli_abort("country_grid.parquet not found. Run prepare_spatialize_inputs.R first.")
}
country_grid <- nanoparquet::read_parquet(country_grid_path)
cli::cli_alert_info("Loaded country_grid: {nrow(country_grid)} cells")

# Read MIRCA mapping
mirca_to_fao <- read_mirca_mapping()
cli::cli_alert_info(
  "MIRCA mapping: {n_distinct(mirca_to_fao$mirca_class)} MIRCA classes -> {nrow(mirca_to_fao)} FAOSTAT codes"
)

# Process each MIRCA crop class
cli::cli_h2("Reading MIRCA2000 binary grids")

all_crops <- list()

for (crop_num in 1:26) {
  irrig_path <- .find_mirca_file(mirca_dir, crop_num, "irrigated")
  rain_path <- .find_mirca_file(mirca_dir, crop_num, "rainfed")

  if (is.null(irrig_path) || is.null(rain_path)) {
    cli::cli_alert_warning(
      "Crop {crop_num}: missing files (irrig={!is.null(irrig_path)}, rain={!is.null(rain_path)})"
    )
    next
  }

  cli::cli_alert_info("Crop {sprintf('%02d', crop_num)}: reading...")

  # Read and aggregate
  irrig_r <- .read_mirca_binary(irrig_path) |>
    .aggregate_mirca(target_res)
  rain_r <- .read_mirca_binary(rain_path) |>
    .aggregate_mirca(target_res)

  # Convert to tibbles and free rasters immediately
  irrig_tbl <- .raster_to_tibble(irrig_r, "irrig_ha") |>
    filter(!is.na(irrig_ha), irrig_ha > 0)
  rm(irrig_r)
  rain_tbl <- .raster_to_tibble(rain_r, "rainfed_ha") |>
    filter(!is.na(rainfed_ha), rainfed_ha > 0)
  rm(rain_r)
  gc(verbose = FALSE)

  # Combine irrigated + rainfed
  crop_tbl <- full_join(irrig_tbl, rain_tbl, by = c("lon", "lat")) |>
    mutate(
      irrig_ha = if_else(is.na(irrig_ha), 0, irrig_ha),
      rainfed_ha = if_else(is.na(rainfed_ha), 0, rainfed_ha),
      mirca_class = crop_num
    ) |>
    filter(irrig_ha > 0 | rainfed_ha > 0)

  all_crops[[crop_num]] <- crop_tbl

  cli::cli_alert_success(
    "Crop {sprintf('%02d', crop_num)}: {nrow(crop_tbl)} cells ({sum(crop_tbl$irrig_ha > 0)} irrigated)"
  )
}

cli::cli_h2("Building country-level irrigation fractions")

# Combine all crops
gridded_mirca <- bind_rows(all_crops)
cli::cli_alert_info("Total gridded MIRCA: {nrow(gridded_mirca)} crop-cell observations")

# Assign countries
gridded_mirca <- gridded_mirca |>
  inner_join(country_grid, by = c("lon", "lat"))

# Map MIRCA classes to FAOSTAT codes
gridded_fao <- gridded_mirca |>
  inner_join(mirca_to_fao, by = "mirca_class", relationship = "many-to-many")

# ---- Output 1: Country-level irrigation fractions --------------------
# Aggregate to country × FAOSTAT crop
country_irrig <- gridded_fao |>
  summarize(
    total_irrig_ha = sum(irrig_ha),
    total_rainfed_ha = sum(rainfed_ha),
    .by = c(area_code, item_prod_code)
  ) |>
  mutate(
    total_ha = total_irrig_ha + total_rainfed_ha,
    irrig_frac = if_else(total_ha > 0, total_irrig_ha / total_ha, 0)
  ) |>
  select(area_code, item_prod_code, irrig_frac)

# Save
mirca_country_path <- file.path(output_dir, "mirca_irrigation_country.parquet")
nanoparquet::write_parquet(country_irrig, mirca_country_path)
cli::cli_alert_success(
  "mirca_irrigation_country: {nrow(country_irrig)} rows ({n_distinct(country_irrig$area_code)} countries x {n_distinct(country_irrig$item_prod_code)} crops)"
)

# ---- Output 2: Gridded irrigation patterns ---------------------------
# Save per-cell irrigation data for spatial validation
gridded_patterns <- gridded_fao |>
  summarize(
    irrig_ha = sum(irrig_ha),
    rainfed_ha = sum(rainfed_ha),
    .by = c(lon, lat, item_prod_code)
  )

mirca_patterns_path <- file.path(output_dir, "mirca_irrigation_patterns.parquet")
nanoparquet::write_parquet(gridded_patterns, mirca_patterns_path)
cli::cli_alert_success(
  "mirca_irrigation_patterns: {nrow(gridded_patterns)} rows"
)

# ---- Summary statistics ----------------------------------------------
cli::cli_h2("Summary")

# Global irrigated fraction by MIRCA crop
mirca_names <- readr::read_csv(
  .find_extdata_file("mirca_mapping.csv"),
  show_col_types = FALSE
) |>
  select(mirca_class, mirca_name)

global_summary <- gridded_mirca |>
  inner_join(mirca_names, by = "mirca_class") |>
  summarize(
    irrig_mha = sum(irrig_ha) / 1e6,
    rainfed_mha = sum(rainfed_ha) / 1e6,
    irrig_pct = sum(irrig_ha) / (sum(irrig_ha) + sum(rainfed_ha)) * 100,
    .by = c(mirca_class, mirca_name)
  ) |>
  arrange(mirca_class)

cli::cli_alert_info("Global irrigated area by MIRCA crop (Mha):")
for (i in seq_len(nrow(global_summary))) {
  row <- global_summary[i, ]
  cli::cli_alert(
    "  {sprintf('%02d', row$mirca_class)} {row$mirca_name}: {round(row$irrig_mha, 1)} Mha irrigated ({round(row$irrig_pct, 0)}%), {round(row$rainfed_mha, 1)} Mha rainfed"
  )
}

total_irrig <- sum(global_summary$irrig_mha)
total_rainfed <- sum(global_summary$rainfed_mha)
cli::cli_alert_success(
  "Total: {round(total_irrig, 1)} Mha irrigated + {round(total_rainfed, 1)} Mha rainfed = {round(total_irrig + total_rainfed, 1)} Mha"
)

cli::cli_alert_success("MIRCA2000 preparation complete")
