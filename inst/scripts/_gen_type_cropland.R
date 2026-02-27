# -----------------------------------------------------------------------
# _gen_type_cropland.R
#
# Quick helper to generate ONLY type_cropland.parquet from LUH2 data.
# Standalone — does not source the full prepare_spatialize_inputs.R.
#
# Output: L_files/whep/inputs/type_cropland.parquet
#   Columns: lon, lat, year, luh2_type, type_ha, type_irrig_ha
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
output_dir <- file.path(l_files_dir, "whep", "inputs")
luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
year_range <- 1850L:2022L
target_res <- 0.5

.raster_to_tibble <- function(r, value_name) {
  coords <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  tibble::tibble(
    lon = round(coords[, 1], 2),
    lat = round(coords[, 2], 2),
    value = terra::values(r)[, 1]
  ) |>
    rlang::set_names(c("lon", "lat", value_name))
}

.read_luh2_variables <- function(nc_path, var_names, time_idx) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  lat <- ncdf4::ncvar_get(nc, "lat")
  n_lon <- length(ncdf4::ncvar_get(nc, "lon"))
  n_lat <- length(lat)
  lat_desc <- lat[1] > lat[length(lat)]
  purrr::map(var_names, \(vname) {
    vals <- ncdf4::ncvar_get(nc, vname,
      start = c(1, 1, time_idx), count = c(n_lon, n_lat, 1))
    r <- terra::rast(t(vals), extent = terra::ext(-180, 180, -90, 90))
    if (!lat_desc) r <- terra::flip(r, direction = "vertical")
    r[is.nan(terra::values(r))] <- 0
    r[is.na(terra::values(r))] <- 0
    r
  }) |> rlang::set_names(var_names)
}

cli::cli_h1("Generating type_cropland.parquet")

# Read cell area
static_path <- file.path(luh2_dir, "staticData_quarterdeg.nc")
nc <- ncdf4::nc_open(static_path)
lat <- ncdf4::ncvar_get(nc, "lat")
vals <- ncdf4::ncvar_get(nc, "carea")
ncdf4::nc_close(nc)
carea_r <- terra::rast(t(vals), extent = terra::ext(-180, 180, -90, 90))
if (lat[1] < lat[length(lat)]) {
  carea_r <- terra::flip(carea_r, direction = "vertical")
}
carea_ha <- carea_r * 100 # km^2 -> ha

crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")
states_path <- file.path(luh2_dir, "states.nc")
mgmt_path <- file.path(luh2_dir, "management.nc")
agg_factor <- as.integer(target_res / 0.25)

cli::cli_alert_info("Processing {length(year_range)} years")

by_type <- purrr::map(year_range, \(yr) {
  if (yr %% 10 == 0) cli::cli_alert("Year {yr}")
  time_idx <- yr - 850L + 1L
  crop_r <- .read_luh2_variables(states_path, crop_vars, time_idx)
  irrig_r <- .read_luh2_variables(mgmt_path,
    paste0("irrig_", crop_vars), time_idx)

  purrr::map(crop_vars, \(cv) {
    iv <- paste0("irrig_", cv)
    type_ha_r <- terra::aggregate(
      crop_r[[cv]] * carea_ha, fact = agg_factor,
      fun = "sum", na.rm = TRUE)
    type_ir_r <- terra::aggregate(
      irrig_r[[iv]] * crop_r[[cv]] * carea_ha, fact = agg_factor,
      fun = "sum", na.rm = TRUE)
    ha_tbl <- .raster_to_tibble(type_ha_r, "type_ha")
    ir_tbl <- .raster_to_tibble(type_ir_r, "type_irrig_ha")
    dplyr::left_join(ha_tbl, ir_tbl, by = c("lon", "lat")) |>
      dplyr::filter(!is.na(type_ha), type_ha > 0) |>
      dplyr::mutate(
        type_irrig_ha = dplyr::if_else(
          is.na(type_irrig_ha), 0, type_irrig_ha),
        luh2_type = cv, year = yr)
  }) |> dplyr::bind_rows()
}) |>
  dplyr::bind_rows() |>
  # Merge c4per into c3per (no CFT maps to c4per)
  dplyr::mutate(
    luh2_type = dplyr::if_else(luh2_type == "c4per", "c3per", luh2_type)
  ) |>
  dplyr::summarise(
    type_ha = sum(type_ha),
    type_irrig_ha = sum(type_irrig_ha),
    .by = c(lon, lat, year, luh2_type)
  )

type_path <- file.path(output_dir, "type_cropland.parquet")
nanoparquet::write_parquet(by_type, type_path)
cli::cli_alert_success(
  "type_cropland: {nrow(by_type)} rows, {dplyr::n_distinct(by_type$luh2_type)} types"
)
cli::cli_alert_success("Saved to {type_path}")
