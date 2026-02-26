# -----------------------------------------------------------------------
# prepare_mirca_irrigation.R
#
# Reads MIRCA2000 monthly growing area grids (Portmann et al., 2010) to
# compute crop-specific irrigated fractions at country level and at 0.5°
# grid resolution.
#
# MIRCA2000 provides: irrigated + rainfed growing area (ha) for 26 crop
# classes at 5 arc-minute, 12 months. We sum across months (annual max
# or sum depending on interpretation) and aggregate:
#   1. Country-level irrigated fraction per crop class
#   2. Gridded 0.5° irrigated area fraction per crop class
#
# This can replace or supplement the LUH2-based irrigation allocation in
# prepare_spatialize_inputs.R. LUH2 only has 5 broad crop types (~15 CFTs
# share those 5 types), while MIRCA has 26 crop-specific classes.
#
# Data source: MIRCA2000 v1.1 (Portmann et al., 2010)
#   http://www2.uni-frankfurt.de/45218023/MIRCA
#   Local path: L_files/Irrigation maps_CIRCA-2000/
#
# Outputs (to L_files/whep/inputs/):
#   - mirca_irrigation_country.parquet
#     (area_code, item_prod_code, irrigated_ha, rainfed_ha, irrig_frac)
#   - mirca_irrigation_grid.parquet
#     (lon, lat, item_prod_code, irrigated_ha, rainfed_ha)
#
# Requires: MIRCA binary .flt.gz files, country_grid.parquet, terra
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Configuration ====================================================

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"
output_dir <- file.path(l_files_dir, "whep", "inputs")

mirca_dir <- file.path(l_files_dir, "Irrigation maps_CIRCA-2000")
target_res <- 0.5  # Output resolution in degrees

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# ==== MIRCA → FAOSTAT mapping ==========================================

.read_mirca_mapping <- function() {
  mirca_map <- readr::read_csv(
    system.file("extdata", "mirca_mapping.csv", package = "whep"),
    show_col_types = FALSE
  )

  cft_map <- readr::read_csv(
    system.file("extdata", "cft_mapping.csv", package = "whep"),
    show_col_types = FALSE
  )

  # For MIRCA classes with direct item_prod_code → 1:1 mapping
  direct <- mirca_map |>
    filter(!is.na(item_prod_code)) |>
    select(mirca_class, item_prod_code)

  # For group classes (pulses, citrus, others_perennial, others_annual,
  # fodder): distribute the MIRCA irrigation signal to all matching
  # item_prod_codes via cft_name
  group_map <- mirca_map |>
    filter(is.na(item_prod_code), !is.na(mirca_group))

  group_items <- group_map |>
    left_join(
      cft_map |> select(item_prod_code, cft_name),
      by = c("mirca_group" = "cft_name"),
      relationship = "many-to-many"
    ) |>
    # Remove items that already have a direct MIRCA mapping
    filter(!item_prod_code %in% direct$item_prod_code) |>
    select(mirca_class, item_prod_code)

  # Fodder grasses → no matching items in cft_mapping (skip)
  group_items <- filter(group_items, !is.na(item_prod_code))

  bind_rows(direct, group_items) |>
    arrange(mirca_class, item_prod_code)
}


# ==== Read a single MIRCA binary .flt file =============================

.read_mirca_flt <- function(path, ncols = 4320L, nrows = 2160L,
                            nmonths = 12L) {
  # .flt files are raw 4-byte floats, ncols × nrows × 12 months
  # Cell order: North→South, West→East. Each "layer" is one month.

  if (grepl("\\.gz$", path)) {
    con <- gzfile(path, "rb")
  } else {
    con <- file(path, "rb")
  }
  on.exit(close(con))

  n <- ncols * nrows * nmonths
  vals <- readBin(con, what = numeric(), n = n, size = 4L, endian = "little")

  if (length(vals) != n) {
    cli::cli_warn(
      "Expected {n} values but read {length(vals)} from {basename(path)}"
    )
  }

  # Reshape to 3D array: [row, col, month]
  arr <- array(vals, dim = c(ncols, nrows, nmonths))
  # Transpose first two dims so rows = latitude (N→S), cols = longitude (W→E)
  arr <- aperm(arr, c(2, 1, 3))

  # Sum across months → annual total growing area (hectares)
  annual <- apply(arr, c(1, 2), sum, na.rm = TRUE)

  # Convert to terra SpatRaster
  r <- terra::rast(
    nrows = nrows, ncols = ncols,
    xmin = -180, xmax = 180, ymin = -90, ymax = 90,
    vals = as.vector(t(annual))  # terra expects row-major from top
  )
  r
}


# ==== Aggregate one MIRCA crop class to 0.5° ===========================

.process_mirca_crop <- function(mirca_class, mirca_dir, target_res) {
  # Find irrigated and rainfed files
  # Crops 01-04 may be in subdirectories irrigated/ and rainfed/
  irr_file <- list.files(
    mirca_dir, pattern = sprintf("crop_%02d_irrigated", mirca_class),
    recursive = TRUE, full.names = TRUE
  )
  rf_file <- list.files(
    mirca_dir, pattern = sprintf("crop_%02d_rainfed", mirca_class),
    recursive = TRUE, full.names = TRUE
  )

  if (length(irr_file) == 0 || length(rf_file) == 0) {
    cli::cli_warn("MIRCA class {mirca_class}: files not found")
    return(NULL)
  }

  # Take the first match (prefer .flt.gz, then .flt)
  irr_file <- irr_file[1]
  rf_file <- rf_file[1]

  irr_rast <- .read_mirca_flt(irr_file)
  rf_rast <- .read_mirca_flt(rf_file)

  # Aggregate from 5 arcmin to target_res (0.5° = factor 6)
  agg_factor <- round(target_res / (5 / 60))
  irr_agg <- terra::aggregate(irr_rast, fact = agg_factor, fun = "sum",
                               na.rm = TRUE)
  rf_agg <- terra::aggregate(rf_rast, fact = agg_factor, fun = "sum",
                              na.rm = TRUE)

  # Convert to tibble
  coords <- terra::xyFromCell(irr_agg, 1:terra::ncell(irr_agg))
  tibble::tibble(
    lon = coords[, 1],
    lat = coords[, 2],
    mirca_class = mirca_class,
    irrigated_ha = as.vector(terra::values(irr_agg)),
    rainfed_ha = as.vector(terra::values(rf_agg))
  ) |>
    filter(irrigated_ha > 0 | rainfed_ha > 0)
}


# ==== Main execution ==================================================

cli::cli_h1("Preparing MIRCA2000 irrigation data")

if (!dir.exists(mirca_dir)) {
  cli::cli_abort("MIRCA2000 directory not found: {mirca_dir}")
}

# Load mapping
mirca_items <- .read_mirca_mapping()
unique_classes <- sort(unique(mirca_items$mirca_class))
cli::cli_alert_info(
  "Processing {length(unique_classes)} MIRCA crop classes → ",
  "{n_distinct(mirca_items$item_prod_code)} item_prod_codes"
)

# Load country grid
country_grid <- nanoparquet::read_parquet(
  file.path(output_dir, "country_grid.parquet")
)

# Process each MIRCA class
cli::cli_progress_bar(
  "Reading MIRCA crops",
  total = length(unique_classes)
)

all_crops <- list()
for (mc in unique_classes) {
  cli::cli_progress_update()
  result <- .process_mirca_crop(mc, mirca_dir, target_res)
  if (!is.null(result)) {
    all_crops[[length(all_crops) + 1]] <- result
  }
}
cli::cli_progress_done()

mirca_grid_raw <- bind_rows(all_crops)
cli::cli_alert_success(
  "Read {n_distinct(mirca_grid_raw$mirca_class)} MIRCA classes, ",
  "{nrow(mirca_grid_raw)} grid cells total"
)

# ==== Map MIRCA classes to item_prod_codes ============================

# For direct 1:1 mappings: straightforward join
# For group mappings (1 MIRCA class → many items): distribute uniformly

mirca_grid <- mirca_grid_raw |>
  inner_join(mirca_items, by = "mirca_class", relationship = "many-to-many") |>
  # For group classes, count items per class per cell to distribute
  mutate(
    n_items = n(),
    .by = c("lon", "lat", "mirca_class")
  ) |>
  mutate(
    irrigated_ha = irrigated_ha / n_items,
    rainfed_ha = rainfed_ha / n_items
  ) |>
  # Aggregate if multiple MIRCA classes map to same item (shouldn't happen)
  summarize(
    irrigated_ha = sum(irrigated_ha),
    rainfed_ha = sum(rainfed_ha),
    .by = c("lon", "lat", "item_prod_code")
  )

# ==== Grid-level output ================================================

nanoparquet::write_parquet(
  mirca_grid,
  file.path(output_dir, "mirca_irrigation_grid.parquet")
)

sz_grid <- round(
  file.size(file.path(output_dir, "mirca_irrigation_grid.parquet")) /
    1024 / 1024, 1
)
cli::cli_alert_success(
  "mirca_irrigation_grid.parquet: {nrow(mirca_grid)} rows ({sz_grid} MB)"
)

# ==== Country-level output =============================================

mirca_country <- mirca_grid |>
  inner_join(country_grid, by = c("lon", "lat")) |>
  summarize(
    irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
    rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
    .by = c("area_code", "item_prod_code")
  ) |>
  mutate(
    total_ha = irrigated_ha + rainfed_ha,
    irrig_frac = if_else(total_ha > 0, irrigated_ha / total_ha, 0)
  ) |>
  filter(total_ha > 0) |>
  arrange(area_code, item_prod_code)

nanoparquet::write_parquet(
  mirca_country,
  file.path(output_dir, "mirca_irrigation_country.parquet")
)

n_items <- n_distinct(mirca_country$item_prod_code)
n_ctry <- n_distinct(mirca_country$area_code)
mean_frac <- round(
  weighted.mean(mirca_country$irrig_frac, mirca_country$total_ha), 3
)

cli::cli_h1("Summary")
cli::cli_alert_success(
  "mirca_irrigation_country.parquet: {nrow(mirca_country)} rows"
)
cli::cli_alert_info(
  "  {n_items} crops × {n_ctry} countries"
)
cli::cli_alert_info(
  "  Area-weighted mean irrigated fraction: {mean_frac}"
)
cli::cli_alert_info(
  "  Use mirca_irrigation_country.parquet in prepare_spatialize_inputs.R"
)
cli::cli_alert_info(
  "  to replace LUH2-proportional irrigation with crop-specific fractions."
)
