# -----------------------------------------------------------------------
# prepare_spatialize_all.R
#
# Consolidated script that prepares all inputs for the WHEP
# spatialization pipeline and runs both crop and livestock
# spatialization.
#
# Merges logic from 13 individual scripts into a single pipeline with
# 12 sections, each wrapped as a callable function.  Can be sourced
# (to load functions without auto-running) or executed directly.
#
# Requires: pass the L_files directory path to main() when calling it.
# Reference N datasets are bundled as package data or fetched via pins.
#
# Data sources (see individual section headers for full citations):
#   - FAOSTAT Production/Emissions/Fertilizers (FAO, 2024)
#   - LUH2 v2h states/management/static (Hurtt et al., 2020)
#   - EarthStat HarvestedAreaYield175Crops (Monfreda et al., 2008)
#   - EarthStat Crop Specific Fertilizers (Mueller et al., 2012)
#   - MIRCA2000 irrigated/rainfed areas (Portmann et al., 2010)
#   - NaturalEarth ne_10m_admin_0_countries (v5.1)
#   - West et al. (2014) Manure N application rates
#   - HWSD v1.21 soil texture and pH
#   - WorldClim/SRTM elevation via geodata package
#   - GRanD v1.1 dam database
#   - GLWD lakes/rivers
#   - DRT/DDM30 drainage routing
#   - Coello et al. (2025) crop-group fertilization
#   - HaNi atmospheric N deposition (Tian et al., 2022)
#
# Outputs (to L_files/whep/inputs/ and L_files/whep/):
#   country_grid, country_areas, crop_patterns,
#   crop_fertilizer_patterns, gridded_cropland, type_cropland,
#   mirca_irrigation_country, mirca_irrigation_grid,
#   mirca_irrigation_patterns, livestock_country_data,
#   gridded_pasture, manure_pattern, country_yields,
#   spatial_yield_index, nitrogen_inputs, n_deposition,
#   pk_totals, coello_crop_rates, spatial_n_index,
#   gridded_west_manure, gridded_earthstat_synth,
#   elevation, reservoirs, lakes_rivers, drainage, soil,
#   gridded_landuse_crops,
#   lpjml_inputs/gadm/, lpjml_inputs/soil/, lpjml_inputs/river_routing/,
#   lpjml_inputs/lakes_rivers/, lpjml_inputs/nitrogen/ (NC: static fields),
#   lpjml_inputs/landuse/ (NC: landuse, fert_N, manure_N, yields),
#   lpjml_inputs/livestock/ (NC: heads, enteric_ch4, manure vars)
#
# Usage:
#   - Rscript this file to run every section in order.
#   - Source the file to make the section functions available in an
#     interactive R session, then call them one by one.
#   - After this script completes, run run_lpjml.R to execute the model.
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(ncdf4)

# ==== Shared helpers ====================================================

# ---- Grid and NetCDF helpers -------------------------------------------

make_target_grid <- function() {
  lon <- seq(-179.75, 179.75, by = 0.5)
  lat <- seq(83.75, -59.25, by = -0.5)
  list(
    lon = lon,
    lat = lat,
    lon_key = sprintf("%.2f", lon),
    lat_key = sprintf("%.2f", lat),
    nlon = length(lon),
    nlat = length(lat)
  )
}

cell_area_ha_by_lat <- function(lat_vals) {
  r <- 6371000
  dlon <- 0.5 * pi / 180
  lat_n <- (lat_vals + 0.25) * pi / 180
  lat_s <- (lat_vals - 0.25) * pi / 180
  r * r * dlon * (sin(lat_n) - sin(lat_s)) / 10000
}

coord_to_rowcol <- function(dt, grid) {
  nlat <- length(grid$lat)
  nlon <- length(grid$lon)
  dt[, row := round((83.75 - lat) / 0.5) + 1L]
  dt[, col := round((lon + 179.75) / 0.5) + 1L]
  dt[row >= 1L & row <= nlat & col >= 1L & col <= nlon]
}

.pft_nc_create <- function(
  path,
  var_name,
  long_name,
  units,
  grid,
  pft_vals,
  years
) {
  npft <- length(pft_vals)
  dlon <- ncdim_def("longitude", "degrees_east", vals = grid$lon)
  dlat <- ncdim_def("latitude", "degrees_north", vals = grid$lat)
  dpft <- ncdim_def("pft", "plant functional types", vals = pft_vals)
  dtime <- ncdim_def("time", "years since 0000-1-1 0:0:0", vals = years)
  v <- ncvar_def(
    var_name,
    units,
    list(dlon, dlat, dpft, dtime),
    missval = -1.175494e+38,
    longname = long_name,
    prec = "float",
    compression = 1,
    chunksizes = c(grid$nlon, grid$nlat, npft, 1L)
  )
  nc <- nc_create(path, vars = list(v), force_v4 = TRUE)
  list(nc = nc, var = v)
}

.pft_nc_write_chunk <- function(
  nc_info,
  data_dt,
  yr_chunk,
  all_years,
  grid,
  npft
) {
  m4 <- array(0, dim = c(grid$nlon, grid$nlat, npft, length(yr_chunk)))
  if (nrow(data_dt) > 0L) {
    ti <- match(data_dt$year, yr_chunk)
    pi <- data_dt$pft
    flat <- (ti - 1L) *
      grid$nlon *
      grid$nlat *
      npft +
      (pi - 1L) * grid$nlon * grid$nlat +
      (data_dt$row - 1L) * grid$nlon +
      data_dt$col
    m4[flat] <- data_dt$value
  }
  t_start <- match(yr_chunk[1L], all_years)
  ncvar_put(
    nc_info$nc,
    nc_info$var,
    m4,
    start = c(1L, 1L, 1L, t_start),
    count = c(grid$nlon, grid$nlat, npft, length(yr_chunk))
  )
}

.nc_finalise <- function(nc_info, creator = "WHEP prepare_spatialize_all.R") {
  ncatt_put(nc_info$nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc_info$nc, 0, "created_by", creator)
  ncatt_put(nc_info$nc, 0, "created_date", as.character(Sys.time()))
  nc_close(nc_info$nc)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

.mem_available_mb <- function() {
  meminfo <- "/proc/meminfo"
  if (!file.exists(meminfo)) {
    return(NA_real_)
  }
  lines <- readLines(meminfo, warn = FALSE)
  available <- grep("^MemAvailable:", lines, value = TRUE)
  if (length(available) == 0L) {
    return(NA_real_)
  }
  as.numeric(gsub("[^0-9]", "", available[[1L]])) / 1024
}

.auto_spatialize_workers <- function(max_years, worker_mb_default = 4500) {
  cores <- parallel::detectCores(logical = TRUE) %||% 1L
  mem_mb <- .mem_available_mb()
  mem_fraction <- as.numeric(Sys.getenv(
    "WHEP_SPATIALIZE_MEM_FRACTION",
    unset = "0.70"
  ))
  worker_mb <- as.numeric(Sys.getenv(
    "WHEP_SPATIALIZE_WORKER_MB",
    unset = as.character(worker_mb_default)
  ))
  core_limit <- max(1L, cores - 1L)
  mem_limit <- if (is.finite(mem_mb)) {
    max(1L, floor((mem_mb * mem_fraction) / worker_mb))
  } else {
    1L
  }
  max(1L, min(max_years, core_limit, mem_limit))
}

new_slice <- function(nlon, nlat, fill = 0) {
  matrix(fill, nrow = nlon, ncol = nlat)
}

lat_idx_of <- function(lat) as.integer(round((83.75 - lat) / 0.5))
lon_idx_of <- function(lon) as.integer(round((lon + 179.75) / 0.5))
flat_of <- function(lon, lat, nlon_g) lat_idx_of(lat) * nlon_g + lon_idx_of(lon)

haver_m <- function(lo1, la1, lo2, la2) {
  r <- 6371000
  p1 <- la1 * pi / 180
  p2 <- la2 * pi / 180
  dp <- (la2 - la1) * pi / 180
  dl <- (lo2 - lo1) * pi / 180
  a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
  2 * r * asin(pmin(sqrt(a), 1))
}

write_nc_2d <- function(
  path,
  var_name,
  long_name,
  units,
  values_lon_lat,
  lon,
  lat,
  prec = "float",
  missval = -1.175494e+38
) {
  dlon <- ncdim_def("longitude", "degrees_east", vals = lon)
  dlat <- ncdim_def("latitude", "degrees_north", vals = lat)
  v <- ncvar_def(
    name = var_name,
    units = units,
    dim = list(dlon, dlat),
    missval = missval,
    longname = long_name,
    prec = prec,
    compression = 1
  )
  nc <- nc_create(path, vars = list(v), force_v4 = TRUE)
  on.exit(nc_close(nc), add = TRUE)
  ncvar_put(
    nc,
    v,
    values_lon_lat,
    start = c(1, 1),
    count = c(length(lon), length(lat))
  )
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "created_by", "WHEP prepare_spatialize_all.R")
  ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
}

.write_dep_monthly <- function(
  out_path,
  var_name,
  value_col,
  grid,
  dep_monthly,
  time_vals
) {
  ntime <- length(time_vals)
  nlon <- grid$nlon
  nlat <- grid$nlat
  dlon <- ncdim_def("longitude", "degrees_east", vals = grid$lon)
  dlat <- ncdim_def("latitude", "degrees_north", vals = grid$lat)
  dtime <- ncdim_def(
    "time",
    "months since 0000-1-1 0:0:0",
    vals = time_vals,
    unlim = FALSE
  )
  v <- ncvar_def(
    name = var_name,
    units = "g/m2/day",
    dim = list(dlon, dlat, dtime),
    missval = -1.175494e+38,
    longname = sprintf("%s deposition (WHEP/HaNi)", var_name),
    prec = "float",
    compression = 1
  )
  cli::cli_alert_info("Building {var_name} array ({ntime} months)...")
  m3 <- array(0, dim = c(nlon, nlat, ntime))
  ti_idx <- match(dep_monthly$time_idx, time_vals)
  valid <- !is.na(ti_idx)
  if (any(valid)) {
    sub <- dep_monthly[valid, ]
    flat <- (ti_idx[valid] - 1L) * nlon * nlat + (sub$row - 1L) * nlon + sub$col
    m3[flat] <- sub[[value_col]]
  }
  cli::cli_alert_info("Writing {var_name} NetCDF...")
  nc <- nc_create(out_path, vars = list(v), force_v4 = TRUE)
  on.exit(nc_close(nc), add = TRUE)
  ncvar_put(nc, v, m3)
  ncatt_put(nc, 0, "Conventions", "CF-1.8")
  ncatt_put(nc, 0, "created_by", "WHEP prepare_spatialize_all.R")
  ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
}

.filter_years <- function(dt, export_years) {
  if (!is.null(export_years) && "year" %in% names(dt)) {
    dt <- dt[dt$year %in% export_years, ]
  }
  dt
}

.downstream_vec <- function(start, nextcell, max_steps = 60L) {
  out <- integer(max_steps)
  n <- 0L
  cur <- nextcell[start]
  while (cur > 0L && n < max_steps) {
    n <- n + 1L
    out[n] <- cur
    cur <- nextcell[cur]
  }
  out[seq_len(n)]
}

.upstream_vec <- function(start, parents_list, max_steps = 60L) {
  out <- integer(max_steps)
  n <- 0L
  q <- parents_list[[start]]
  while (length(q) > 0L && n < max_steps) {
    cur <- q[1L]
    q <- q[-1L]
    n <- n + 1L
    out[n] <- cur
    q <- c(q, parents_list[[cur]])
  }
  out[seq_len(n)]
}

cft_to_pft <- c(
  temperate_cereals = 1L,
  rice = 2L,
  maize = 3L,
  tropical_cereals = 4L,
  pulses = 5L,
  temperate_roots = 6L,
  tropical_roots = 7L,
  oil_crops_sunflower = 8L,
  oil_crops_soybean = 9L,
  oil_crops_groundnut = 10L,
  oil_crops_rapeseed = 11L,
  sugarcane = 12L,
  others_annual = 13L,
  others_perennial = 13L,
  oil_crops_other = 13L
)

# ---- Find extdata file (inst/ or installed package) --------------------
.find_extdata_file <- function(filename) {
  local_path <- file.path("inst", "extdata", filename)
  if (file.exists(local_path)) {
    return(local_path)
  }
  pkg_path <- system.file("extdata", filename, package = "whep")
  if (nchar(pkg_path) > 0) {
    return(pkg_path)
  }
  cli::cli_abort("Cannot find {.file {filename}} in inst/extdata.")
}

# ---- Raster to tibble --------------------------------------------------
.raster_to_tibble <- function(r, value_name) {
  coords <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  vals <- terra::values(r)[, 1]
  tibble::tibble(
    lon = round(coords[, 1], 2),
    lat = round(coords[, 2], 2),
    !!value_name := vals
  )
}

# ---- Read CFT mapping ---------------------------------------------------
.read_cft_mapping <- function() {
  .find_extdata_file("cft_mapping.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

# ---- Read EarthStat mapping ---------------------------------------------
# Rows whose `item_prod_code` is NA correspond to EarthStat raster
# layers (fodder grasses, alfalfa, clover, etc.) that have no direct
# FAOSTAT QCL item. Downstream consumers usually filter with
# `dplyr::filter(!is.na(item_prod_code))`.
.read_earthstat_mapping <- function() {
  .find_extdata_file("earthstat_mapping.csv") |>
    readr::read_csv(
      show_col_types = FALSE,
      col_types = readr::cols(
        earthstat_name = readr::col_character(),
        item_prod_code = readr::col_integer(),
        item_prod_name = readr::col_character()
      )
    )
}

# ---- EarthStat Crop Specific Fertilizer mapping (17 crops) ---------------
.earthstat_fertilizer_mapping <- function() {
  tibble::tribble(
    ~earthstat_fert_name, ~item_prod_code,
    "barley",      44L,
    "cassava",    340L,
    "cotton",     274L,
    "groundnut",  242L,
    "maize",       56L,
    "millet",      79L,
    "oilpalm",    217L,
    "potato",     328L,
    "rapeseed",   223L,
    "rice",        27L,
    "rye",         71L,
    "sorghum",     83L,
    "soybean",    236L,
    "sugarbeet",  157L,
    "sugarcane",  780L,
    "sunflower",  222L,
    "wheat",       15L
  )
}

# ---- Read one EarthStat HarvestedAreaFraction crop ----------------------
.read_one_earthstat_crop <- function(
  earthstat_dir,
  crop_name,
  item_prod_code,
  target_res
) {
  tif_path <- file.path(
    earthstat_dir,
    crop_name,
    paste0(crop_name, "_HarvestedAreaFraction.tif")
  )
  if (!file.exists(tif_path)) {
    cli::cli_alert_warning("Missing: {tif_path}")
    return(tibble::tibble())
  }
  r <- terra::rast(tif_path)
  agg_factor <- as.integer(target_res / (5 / 60))
  r_agg <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
  .raster_to_tibble(r_agg, "harvest_fraction") |>
    dplyr::filter(!is.na(harvest_fraction), harvest_fraction > 0) |>
    dplyr::mutate(item_prod_code = item_prod_code)
}

# ---- Read one EarthStat fertilizer raster (N, P, or K) ------------------
.read_one_earthstat_fertilizer <- function(
  fert_dir,
  crop_name,
  item_prod_code,
  nutrient,
  target_res
) {
  tif_name <- paste0(crop_name, "_", nutrient, "Application_Rate.tif")
  tif_path <- file.path(fert_dir, paste0("Fertilizer_", crop_name), tif_name)
  if (!file.exists(tif_path)) {
    return(tibble::tibble())
  }
  r <- terra::rast(tif_path)
  src_res <- terra::res(r)[1]
  agg_factor <- max(1L, as.integer(round(target_res / src_res)))
  if (agg_factor > 1) {
    r_agg <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
  } else {
    r_agg <- r
  }
  col_name <- paste0("kg_", tolower(substr(nutrient, 1, 1)), "_ha")
  .raster_to_tibble(r_agg, col_name) |>
    dplyr::filter(!is.na(.data[[col_name]]), .data[[col_name]] > 0) |>
    dplyr::mutate(item_prod_code = item_prod_code)
}

# ---- Read one EarthStat YieldPerHectare raster ---------------------------
.read_one_earthstat_yield <- function(
  earthstat_dir,
  crop_name,
  item_prod_code,
  target_res
) {
  tif_path <- file.path(
    earthstat_dir,
    crop_name,
    paste0(crop_name, "_YieldPerHectare.tif")
  )
  if (!file.exists(tif_path)) {
    return(tibble::tibble())
  }
  r <- terra::rast(tif_path)
  agg_factor <- as.integer(target_res / (5 / 60))
  r_agg <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
  .raster_to_tibble(r_agg, "yield_t_ha") |>
    dplyr::filter(!is.na(yield_t_ha), yield_t_ha > 0) |>
    dplyr::mutate(item_prod_code = item_prod_code)
}

# ---- LUH2 static variable reader ----------------------------------------
.read_luh2_static <- function(luh2_dir, var_name) {
  static_path <- file.path(luh2_dir, "staticData_quarterdeg.nc")
  nc <- ncdf4::nc_open(static_path)
  on.exit(ncdf4::nc_close(nc))
  lat <- ncdf4::ncvar_get(nc, "lat")
  vals <- ncdf4::ncvar_get(nc, var_name)
  r <- terra::rast(t(vals), extent = terra::ext(-180, 180, -90, 90))
  if (lat[1] < lat[length(lat)]) {
    r <- terra::flip(r, direction = "vertical")
  }
  r
}

.read_luh2_carea <- function(luh2_dir) {
  .read_luh2_static(luh2_dir, "carea")
}

# ---- LUH2 single-variable reader (for livestock/pasture) -----------------
.read_luh2_variable <- function(nc_path, varname, time_idx) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  lat <- ncdf4::ncvar_get(nc, "lat")
  time_idx <- min(time_idx, nc$dim$time$len)
  vals <- ncdf4::ncvar_get(
    nc,
    varname,
    start = c(1, 1, time_idx),
    count = c(-1, -1, 1)
  )
  vals[is.na(vals)] <- 0
  r <- terra::rast(t(vals), extent = terra::ext(-180, 180, -90, 90))
  if (lat[1] < lat[length(lat)]) {
    r <- terra::flip(r, direction = "vertical")
  }
  r
}

# ---- LUH2 multi-variable reader (for cropland types) --------------------
.luh2_time_len <- function(nc_path) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  nc$dim$time$len
}

.read_luh2_variables <- function(nc_path, var_names, time_idx) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  n_lon <- length(lon)
  n_lat <- length(lat)
  lat_desc <- lat[1] > lat[length(lat)]
  time_idx <- min(time_idx, nc$dim$time$len)
  purrr::map(var_names, \(vname) {
    vals <- ncdf4::ncvar_get(
      nc,
      vname,
      start = c(1, 1, time_idx),
      count = c(n_lon, n_lat, 1)
    )
    r <- terra::rast(t(vals), extent = terra::ext(-180, 180, -90, 90))
    if (!lat_desc) {
      r <- terra::flip(r, direction = "vertical")
    }
    r[is.nan(terra::values(r))] <- 0
    r[is.na(terra::values(r))] <- 0
    r
  }) |>
    rlang::set_names(var_names)
}

# ---- Read LUH2 cropland for one year (per-type + total) ------------------
.read_luh2_year <- function(luh2_dir, yr, crop_vars, carea_rast, target_res) {
  states_path <- file.path(luh2_dir, "states.nc")
  mgmt_path <- file.path(luh2_dir, "management.nc")
  irrig_vars <- paste0("irrig_", crop_vars)

  # LUH2 v2h time axis: index 1 = year 850 CE. The historical record
  # currently ends at 2022 (1173 time steps). For requested years past
  # the LUH2 record (e.g. 2023+ in releases where FAOSTAT extends further
  # than LUH2), reuse the most recent LUH2 slice so the prep does not
  # abort. .read_luh2_variables is given the clamped index.
  time_idx_raw <- yr - 850L + 1L
  luh2_time_len <- .luh2_time_len(states_path)
  time_idx <- min(time_idx_raw, luh2_time_len)
  if (time_idx_raw > luh2_time_len) {
    cli::cli_warn(c(
      "LUH2 v2h covers up to year {850L + luh2_time_len - 1L}.",
      "i" = "Year {yr} requested; reusing the {850L + luh2_time_len - 1L} slice."
    ))
  }

  crop_rasters <- .read_luh2_variables(states_path, crop_vars, time_idx)
  irrig_rasters <- .read_luh2_variables(mgmt_path, irrig_vars, time_idx)

  agg_factor <- as.integer(target_res / 0.25)
  carea_ha <- carea_rast * 100

  type_rows <- purrr::map(crop_vars, \(cv) {
    iv <- paste0("irrig_", cv)
    type_ha_r <- crop_rasters[[cv]] * carea_ha
    type_ir_r <- irrig_rasters[[iv]] * crop_rasters[[cv]] * carea_ha
    type_ha_r <- terra::aggregate(
      type_ha_r,
      fact = agg_factor,
      fun = "sum",
      na.rm = TRUE
    )
    type_ir_r <- terra::aggregate(
      type_ir_r,
      fact = agg_factor,
      fun = "sum",
      na.rm = TRUE
    )
    ha_tbl <- .raster_to_tibble(type_ha_r, "type_ha")
    ir_tbl <- .raster_to_tibble(type_ir_r, "type_irrig_ha")
    dplyr::left_join(ha_tbl, ir_tbl, by = c("lon", "lat")) |>
      dplyr::filter(!is.na(type_ha), type_ha > 0) |>
      dplyr::mutate(
        type_irrig_ha = dplyr::if_else(is.na(type_irrig_ha), 0, type_irrig_ha),
        luh2_type = cv,
        year = yr
      )
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      luh2_type = dplyr::if_else(luh2_type == "c4per", "c3per", luh2_type)
    ) |>
    dplyr::summarise(
      type_ha = sum(type_ha),
      type_irrig_ha = sum(type_irrig_ha),
      .by = c(lon, lat, year, luh2_type)
    )

  total_rows <- type_rows |>
    dplyr::summarise(
      cropland_ha = sum(type_ha),
      irrigated_ha = sum(type_irrig_ha),
      .by = c(lon, lat, year)
    )

  list(total = total_rows, by_type = type_rows)
}

# ---- LUH2 country-level cropland + irrigation totals ---------------------
.luh2_country_totals <- function(
  luh2_dir,
  year_range,
  country_grid,
  target_res
) {
  cli::cli_alert_info("Computing country-level cropland & irrigation from LUH2")

  crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")
  irrig_vars <- paste0("irrig_", crop_vars)
  states_path <- file.path(luh2_dir, "states.nc")
  mgmt_path <- file.path(luh2_dir, "management.nc")
  carea_ha_r <- .read_luh2_carea(luh2_dir) * 100
  agg_factor <- as.integer(target_res / 0.25)

  n_workers <- .auto_spatialize_workers(
    length(year_range),
    worker_mb_default = 9000
  )
  cli::cli_alert_info("LUH2 country-total workers: {n_workers}")
  per_year_fn <- \(yr) {
    if (yr %% 10 == 0) {
      cli::cli_alert("LUH2 country totals: year {yr}")
    }
    time_idx <- yr - 850L + 1L
    crop_r <- .read_luh2_variables(states_path, crop_vars, time_idx)
    irrig_r <- .read_luh2_variables(mgmt_path, irrig_vars, time_idx)
    purrr::map(crop_vars, \(cv) {
      iv <- paste0("irrig_", cv)
      crop_ha_r <- crop_r[[cv]] * carea_ha_r
      irrig_ha_r <- irrig_r[[iv]] * crop_r[[cv]] * carea_ha_r
      crop_agg <- terra::aggregate(
        crop_ha_r,
        fact = agg_factor,
        fun = "sum",
        na.rm = TRUE
      )
      irrig_agg <- terra::aggregate(
        irrig_ha_r,
        fact = agg_factor,
        fun = "sum",
        na.rm = TRUE
      )
      crop_tbl <- .raster_to_tibble(crop_agg, "crop_ha")
      irrig_tbl <- .raster_to_tibble(irrig_agg, "irrig_ha")
      dplyr::left_join(crop_tbl, irrig_tbl, by = c("lon", "lat")) |>
        dplyr::filter(!is.na(.data$crop_ha), .data$crop_ha > 0) |>
        dplyr::mutate(
          irrig_ha = dplyr::if_else(is.na(.data$irrig_ha), 0, .data$irrig_ha),
          luh2_type = cv
        )
    }) |>
      dplyr::bind_rows() |>
      dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
      dplyr::summarise(
        crop_ha = sum(.data$crop_ha),
        irrig_ha = sum(.data$irrig_ha),
        .by = c("area_code", "luh2_type")
      ) |>
      dplyr::mutate(year = yr)
  }
  per_year_results <- if (.Platform$OS.type == "windows") {
    lapply(year_range, per_year_fn)
  } else {
    parallel::mclapply(year_range, per_year_fn, mc.cores = n_workers)
  }
  per_year_results |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      luh2_type = dplyr::if_else(
        .data$luh2_type == "c4per",
        "c3per",
        .data$luh2_type
      )
    ) |>
    dplyr::summarise(
      crop_ha = sum(.data$crop_ha),
      irrig_ha = sum(.data$irrig_ha),
      .by = c("year", "area_code", "luh2_type")
    )
}

# ---- Predecessor -> successor country mappings ---------------------------
.predecessor_successors <- function() {
  tibble::tribble(
    ~predecessor_code, ~successor_code,
    228L,   1L,    228L,  52L,    228L,  57L,    228L,  63L,
    228L,  73L,    228L, 108L,    228L, 113L,    228L, 119L,
    228L, 126L,    228L, 146L,    228L, 185L,    228L, 208L,
    228L, 213L,    228L, 230L,    228L, 235L,
    248L,  80L,    248L,  98L,    248L, 154L,
    248L, 198L,    248L, 272L,    248L, 273L,
     51L, 167L,     51L, 199L,
     15L, 255L,     15L, 256L,
    186L, 272L,    186L, 273L,
     62L, 238L,     62L, 178L,
    206L, 276L,    206L, 277L
  )
}

# ---- Redistribute predecessor data to successors -------------------------
.redistribute_predecessors <- function(crop_areas, luh2_totals) {
  pred_map <- .predecessor_successors()
  pred_codes <- unique(pred_map$predecessor_code)
  pred_rows <- crop_areas |>
    dplyr::filter(.data$area_code %in% pred_codes)
  if (nrow(pred_rows) == 0) {
    return(crop_areas)
  }

  other_rows <- crop_areas |>
    dplyr::filter(!.data$area_code %in% pred_codes)

  luh2_ctry <- luh2_totals |>
    dplyr::summarise(crop_ha = sum(.data$crop_ha), .by = c("year", "area_code"))

  redistributed <- pred_rows |>
    dplyr::inner_join(
      pred_map,
      by = c("area_code" = "predecessor_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      luh2_ctry,
      by = c("year", "successor_code" = "area_code")
    ) |>
    dplyr::mutate(
      crop_ha = dplyr::if_else(is.na(.data$crop_ha), 0, .data$crop_ha)
    ) |>
    dplyr::mutate(
      group_total = sum(.data$crop_ha),
      share = dplyr::if_else(
        .data$group_total > 0,
        .data$crop_ha / .data$group_total,
        1 / dplyr::n()
      ),
      .by = c("year", "area_code", "item_prod_code")
    ) |>
    dplyr::mutate(
      harvested_area_ha = .data$harvested_area_ha * .data$share,
      area_code = .data$successor_code
    ) |>
    dplyr::filter(.data$harvested_area_ha > 0) |>
    dplyr::select("year", "area_code", "item_prod_code", "harvested_area_ha")

  n_pred <- length(unique(pred_rows$area_code))
  n_succ <- length(unique(redistributed$area_code))
  cli::cli_alert_success(
    "Redistributed {n_pred} predecessor entities to {n_succ} successor states"
  )

  dplyr::bind_rows(other_rows, redistributed)
}

# ---- Save parquet helper -------------------------------------------------
.save_parquet <- function(data, dir, name) {
  path <- file.path(dir, paste0(name, ".parquet"))
  nanoparquet::write_parquet(data, path)
  sz <- round(file.size(path) / 1024 / 1024, 1)
  cli::cli_alert_success("{name}.parquet: {nrow(data)} rows ({sz} MB)")
  invisible(path)
}

# ---- Load or cache production data ----------------------------------------
.load_or_cache_production <- function(output_dir, year_range) {
  prod_cache <- file.path(output_dir, ".prod_cache.parquet")
  if (file.exists(prod_cache)) {
    cached <- nanoparquet::read_parquet(prod_cache)
    cached_years <- sort(unique(as.integer(cached$year)))
    if (
      min(year_range) >= min(cached_years) &&
        max(year_range) <= max(cached_years)
    ) {
      cli::cli_alert_info("Reading cached production data")
      return(cached)
    }
    cli::cli_alert_info(
      "Cache covers {min(cached_years)}-{max(cached_years)}, \\
      rebuilding for {min(year_range)}-{max(year_range)}"
    )
  }
  prod <- whep::build_primary_production(
    start_year = min(year_range),
    end_year = max(year_range)
  )
  nanoparquet::write_parquet(prod, prod_cache)
  cli::cli_alert_info("Cached production data for reuse")
  prod
}


# ==== Section 1: Country grid ============================================
#
# Rasterize NaturalEarth to a 0.5-degree grid with WHEP area_code.

prepare_country_grid <- function(l_files_dir, target_res) {
  cli::cli_h2("Section 1: Country grid")

  shp_path <- file.path(
    l_files_dir,
    "NaturalEarth",
    "Countries_shape",
    "ne_10m_admin_0_countries.shp"
  )
  countries <- terra::vect(shp_path)
  polities <- whep::polities

  iso_raw <- as.character(countries$ISO_A3)
  iso_eh <- as.character(countries$ISO_A3_EH)
  iso_adm <- as.character(countries$ADM0_A3)
  iso3c <- dplyr::if_else(
    iso_raw != "-99",
    iso_raw,
    dplyr::if_else(iso_eh != "-99", iso_eh, iso_adm)
  )

  ne_data <- tibble::tibble(iso3c = iso3c) |>
    dplyr::left_join(
      dplyr::select(polities, "iso3c", "area_code"),
      by = "iso3c"
    )
  countries$area_code <- ne_data$area_code

  ref <- terra::rast(
    resolution = target_res,
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90
  )

  cli::cli_alert_info("Rasterizing shapefile to {target_res}-degree grid")
  grid_rast <- terra::rasterize(countries, ref, field = "area_code")

  result <- .raster_to_tibble(grid_rast, "area_code") |>
    dplyr::filter(!is.na(area_code)) |>
    dplyr::mutate(area_code = as.integer(area_code))

  cli::cli_alert_success("country_grid: {nrow(result)} cells")
  result
}


# ==== Section 2: Country areas ===========================================
#
# FAOSTAT harvested area + LUH2/MIRCA irrigation allocation.

prepare_country_areas <- function(
  l_files_dir,
  year_range,
  country_grid,
  target_res,
  prod = NULL
) {
  cli::cli_h2("Section 2: Country areas")

  cft_mapping <- .read_cft_mapping()

  if (is.null(prod)) {
    prod <- whep::build_primary_production(
      start_year = min(year_range),
      end_year = max(year_range)
    )
  }

  crop_areas <- prod |>
    dplyr::filter(
      .data$unit == "ha",
      as.integer(.data$item_prod_code) %in% cft_mapping$item_prod_code,
      .data$year %in% year_range
    ) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      harvested_area_ha = .data$value
    ) |>
    dplyr::filter(.data$harvested_area_ha > 0)

  cli::cli_alert_success(
    "{nrow(crop_areas)} crop-area rows from production pipeline"
  )

  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  luh2_totals <- .luh2_country_totals(
    luh2_dir,
    year_range,
    country_grid,
    target_res
  )

  crop_areas <- .redistribute_predecessors(crop_areas, luh2_totals)

  type_map <- dplyr::select(cft_mapping, "item_prod_code", "luh2_type")
  crop_areas <- crop_areas |>
    dplyr::left_join(type_map, by = "item_prod_code")

  luh2_irrig <- luh2_totals |>
    dplyr::select("year", "area_code", "luh2_type", "irrig_ha")

  mirca_file <- file.path(
    l_files_dir,
    "whep",
    "inputs",
    "mirca_irrigation_country.parquet"
  )
  has_mirca <- file.exists(mirca_file)

  if (has_mirca) {
    cli::cli_alert_info("Using MIRCA2000 crop-specific irrigation fractions")
    mirca <- nanoparquet::read_parquet(mirca_file) |>
      dplyr::select("area_code", "item_prod_code", "irrig_frac")

    luh2_total_irrig <- luh2_totals |>
      dplyr::summarize(
        total_irrig_ha = sum(.data$irrig_ha, na.rm = TRUE),
        .by = c("year", "area_code")
      )

    crop_areas <- crop_areas |>
      dplyr::left_join(mirca, by = c("area_code", "item_prod_code")) |>
      dplyr::left_join(luh2_total_irrig, by = c("year", "area_code"))

    crop_areas <- crop_areas |>
      dplyr::mutate(
        mirca_irrig_raw = dplyr::if_else(
          !is.na(.data$irrig_frac),
          .data$irrig_frac * .data$harvested_area_ha,
          NA_real_
        ),
        mirca_total = sum(.data$mirca_irrig_raw, na.rm = TRUE),
        .by = c("year", "area_code")
      ) |>
      dplyr::mutate(
        total_irrig_ha = dplyr::if_else(
          is.na(.data$total_irrig_ha),
          0,
          .data$total_irrig_ha
        ),
        mirca_scale = dplyr::if_else(
          .data$mirca_total > 0,
          .data$total_irrig_ha / .data$mirca_total,
          1
        ),
        irrigated_area_ha = dplyr::case_when(
          !is.na(.data$mirca_irrig_raw) ~
            .data$mirca_irrig_raw * .data$mirca_scale,
          TRUE ~ NA_real_
        )
      )

    needs_fallback <- is.na(crop_areas$irrigated_area_ha)
    if (any(needs_fallback)) {
      fallback <- crop_areas |>
        dplyr::filter(is.na(.data$irrigated_area_ha)) |>
        dplyr::left_join(
          luh2_irrig,
          by = c("year", "area_code", "luh2_type")
        ) |>
        dplyr::mutate(
          irrig_ha = dplyr::if_else(is.na(.data$irrig_ha), 0, .data$irrig_ha),
          cft_total = sum(.data$harvested_area_ha),
          crop_share = .data$harvested_area_ha / .data$cft_total,
          irrigated_area_ha = .data$crop_share * .data$irrig_ha,
          .by = c("year", "area_code", "luh2_type")
        )
      crop_areas$irrigated_area_ha[needs_fallback] <-
        fallback$irrigated_area_ha
    }
  } else {
    cli::cli_alert_info(
      "MIRCA not found -- using LUH2-proportional irrigation allocation"
    )
    crop_areas <- crop_areas |>
      dplyr::mutate(
        cft_total_ha = sum(.data$harvested_area_ha),
        crop_share = .data$harvested_area_ha / .data$cft_total_ha,
        .by = c("year", "area_code", "luh2_type")
      ) |>
      dplyr::left_join(luh2_irrig, by = c("year", "area_code", "luh2_type")) |>
      dplyr::mutate(
        irrig_ha = dplyr::if_else(is.na(.data$irrig_ha), 0, .data$irrig_ha),
        irrigated_area_ha = .data$crop_share * .data$irrig_ha
      )
  }

  crop_areas |>
    dplyr::mutate(
      irrigated_area_ha = dplyr::if_else(
        is.na(.data$irrigated_area_ha),
        0,
        .data$irrigated_area_ha
      ),
      irrigated_area_ha = pmin(
        .data$irrigated_area_ha,
        .data$harvested_area_ha
      )
    ) |>
    dplyr::select(
      "year",
      "area_code",
      "item_prod_code",
      "harvested_area_ha",
      "irrigated_area_ha"
    )
}


# ==== Section 3: Crop patterns (EarthStat) ================================

prepare_crop_patterns <- function(l_files_dir, target_res) {
  cli::cli_h2("Section 3: EarthStat crop patterns")

  earthstat_dir <- file.path(
    l_files_dir,
    "HarvestedAreaYield175Crops_Geotiff",
    "GeoTiff"
  )
  xwalk <- .read_earthstat_mapping() |>
    dplyr::filter(!is.na(item_prod_code))

  cli::cli_alert_info(
    "Processing {nrow(xwalk)} crops from EarthStat in parallel"
  )

  read_one_pattern <- function(i) {
    .read_one_earthstat_crop(
      earthstat_dir,
      xwalk$earthstat_name[i],
      xwalk$item_prod_code[i],
      target_res
    )
  }
  n_workers <- min(nrow(xwalk), max(1L, parallel::detectCores() - 1L))
  parts <- if (.Platform$OS.type == "windows") {
    lapply(seq_len(nrow(xwalk)), read_one_pattern)
  } else {
    parallel::mclapply(
      seq_len(nrow(xwalk)),
      read_one_pattern,
      mc.cores = n_workers
    )
  }
  dplyr::bind_rows(parts) |>
    dplyr::summarise(
      harvest_fraction = sum(harvest_fraction),
      .by = c(lon, lat, item_prod_code)
    )
}


# ==== Section 3b: Crop fertilizer patterns ================================

prepare_crop_fert_patterns <- function(l_files_dir, target_res) {
  cli::cli_h2("Section 3b: EarthStat fertilizer rates")

  fert_dir <- file.path(l_files_dir, "EarthStat - Crop Specific Fertilizers")
  if (!dir.exists(fert_dir)) {
    cli::cli_alert_warning("EarthStat fertilizer dir not found: {fert_dir}")
    return(NULL)
  }

  fert_map <- .earthstat_fertilizer_mapping()
  nutrients <- c("Nitrogen", "Phosphorus", "Potassium")
  cli::cli_alert_info(
    "Processing {nrow(fert_map)} crops x {length(nutrients)} nutrients"
  )

  tasks <- tidyr::expand_grid(
    nutrient = nutrients,
    crop_idx = seq_len(nrow(fert_map))
  )
  n_workers <- min(
    nrow(tasks),
    max(1L, parallel::detectCores() - 1L)
  )
  read_one <- function(k) {
    .read_one_earthstat_fertilizer(
      fert_dir,
      fert_map$earthstat_fert_name[tasks$crop_idx[k]],
      fert_map$item_prod_code[tasks$crop_idx[k]],
      tasks$nutrient[k],
      target_res
    )
  }
  all_results <- if (.Platform$OS.type == "windows") {
    lapply(seq_len(nrow(tasks)), read_one)
  } else {
    parallel::mclapply(
      seq_len(nrow(tasks)),
      read_one,
      mc.cores = n_workers
    )
  }
  results <- split(all_results, tasks$nutrient)
  results <- purrr::map(results, dplyr::bind_rows)
  results <- rlang::set_names(
    results[nutrients],
    c("n", "p", "k")
  )

  combined <- results$n |>
    dplyr::full_join(results$p, by = c("lon", "lat", "item_prod_code")) |>
    dplyr::full_join(results$k, by = c("lon", "lat", "item_prod_code")) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("kg_"), \(x) {
        dplyr::if_else(is.na(x), 0, x)
      })
    )

  n_crops <- dplyr::n_distinct(combined$item_prod_code)
  n_cells <- dplyr::n_distinct(paste(combined$lon, combined$lat))
  cli::cli_alert_success(
    "Fertilizer rates: {n_crops} crops, {n_cells} cells, {nrow(combined)} rows"
  )
  combined
}


# ==== Section 4: Gridded cropland (LUH2) =================================
#
# Includes type_cropland.parquet generation (from _gen_type_cropland.R).

prepare_gridded_cropland <- function(
  l_files_dir,
  year_range,
  target_res,
  output_dir = NULL
) {
  cli::cli_h2("Section 4: LUH2 gridded cropland + type_cropland")

  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  carea_rast <- .read_luh2_carea(luh2_dir)
  crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")

  cli::cli_alert_info("Processing {length(year_range)} years of LUH2 data")

  n_workers <- .auto_spatialize_workers(
    length(year_range),
    worker_mb_default = 9000
  )
  cli::cli_alert_info("LUH2 gridded-cropland workers: {n_workers}")
  per_year_fn <- \(yr) {
    if (yr %% 10 == 0) {
      cli::cli_alert("Processing year {yr}")
    }
    .read_luh2_year(luh2_dir, yr, crop_vars, carea_rast, target_res)
  }
  all_years <- if (.Platform$OS.type == "windows") {
    lapply(year_range, per_year_fn)
  } else {
    parallel::mclapply(year_range, per_year_fn, mc.cores = n_workers)
  }

  total <- purrr::map(all_years, "total") |> dplyr::bind_rows()
  by_type <- purrr::map(all_years, "by_type") |> dplyr::bind_rows()

  if (!is.null(output_dir)) {
    .save_parquet(by_type, output_dir, "type_cropland")
  }

  cli::cli_alert_success(
    "gridded_cropland: {nrow(total)} rows, type_cropland: {nrow(by_type)} rows"
  )
  total
}


# ---- MIRCA post-processing: expand to FAO codes and aggregate --------------
.mirca_expand_aggregate <- function(gridded_mirca, mirca_to_fao) {
  dt <- data.table::as.data.table(gridded_mirca)
  map_dt <- data.table::as.data.table(mirca_to_fao)
  expanded <- map_dt[
    dt,
    on = "mirca_class",
    allow.cartesian = TRUE,
    nomatch = NULL
  ]

  country <- expanded[,
    .(
      total_irrig_ha = sum(irrig_ha),
      total_rainfed_ha = sum(rainfed_ha)
    ),
    by = .(area_code, item_prod_code)
  ]
  country[, total_ha := total_irrig_ha + total_rainfed_ha]
  country[,
    irrig_frac := data.table::fifelse(
      total_ha > 0,
      total_irrig_ha / total_ha,
      0
    )
  ]
  country[, c("total_irrig_ha", "total_rainfed_ha", "total_ha") := NULL]

  patterns <- expanded[,
    .(irrig_ha = sum(irrig_ha), rainfed_ha = sum(rainfed_ha)),
    by = .(lon, lat, item_prod_code)
  ]

  list(
    country = tibble::as_tibble(country),
    patterns = tibble::as_tibble(patterns)
  )
}

# ---- MIRCA binary reader ---------------------------------------------------
.mirca_read_binary <- function(fpath, ncols, nrows, nmonths) {
  ncells <- ncols * nrows
  is_gz <- grepl("\\.gz$", fpath)
  if (is_gz) {
    con <- gzfile(fpath, "rb")
    on.exit(close(con))
    raw_vals <- readBin(con, "double", n = ncells * nmonths, size = 4L)
  } else {
    raw_vals <- readBin(fpath, "double", n = ncells * nmonths, size = 4L)
  }
  if (length(raw_vals) != ncells * nmonths) {
    cli::cli_abort(
      "Expected {ncells * nmonths} values, got {length(raw_vals)} from {fpath}"
    )
  }
  annual <- raw_vals[seq_len(ncells)]
  for (m in 2:nmonths) {
    idx <- ((m - 1L) * ncells + 1L):(m * ncells)
    annual <- pmax(annual, raw_vals[idx])
  }
  r <- terra::rast(
    nrows = nrows,
    ncols = ncols,
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90
  )
  terra::values(r) <- annual
  r
}

# ---- MIRCA file finder -----------------------------------------------------
.mirca_find_file <- function(mirca_dir, crop_num, type) {
  fname <- sprintf("crop_%02d_%s_12", crop_num, type)
  gz_path <- file.path(mirca_dir, paste0(fname, ".flt.gz"))
  if (file.exists(gz_path)) {
    return(gz_path)
  }
  sub_dir <- file.path(mirca_dir, type, paste0(fname, ".flt"))
  sub_file <- file.path(sub_dir, paste0(fname, ".flt"))
  if (file.exists(sub_file)) {
    return(sub_file)
  }
  sub_dir_tilde <- file.path(mirca_dir, type, paste0(fname, ".flt~"))
  sub_file_tilde <- file.path(sub_dir_tilde, paste0(fname, ".flt"))
  if (file.exists(sub_file_tilde)) {
    return(sub_file_tilde)
  }
  standalone <- file.path(mirca_dir, type, paste0(fname, ".flt"))
  if (file.exists(standalone)) {
    return(standalone)
  }
  alt_dir <- file.path(mirca_dir, type, fname)
  alt_file <- file.path(alt_dir, paste0(fname, ".flt"))
  if (file.exists(alt_file)) {
    return(alt_file)
  }
  NULL
}

# ---- MIRCA raster aggregator -----------------------------------------------
.mirca_aggregate <- function(r, target_res) {
  agg_factor <- as.integer(target_res / (5 / 60))
  terra::aggregate(r, fact = agg_factor, fun = "sum", na.rm = TRUE)
}

# ---- Process one MIRCA crop class ------------------------------------------
.mirca_process_crop <- function(crop_num, mirca_dir, target_res, dims) {
  irrig_path <- .mirca_find_file(mirca_dir, crop_num, "irrigated")
  rain_path <- .mirca_find_file(mirca_dir, crop_num, "rainfed")
  if (is.null(irrig_path) || is.null(rain_path)) {
    cli::cli_alert_warning("Crop {crop_num}: missing files")
    return(NULL)
  }
  irrig_r <- .mirca_read_binary(
    irrig_path,
    dims$ncols,
    dims$nrows,
    dims$nmonths
  ) |>
    .mirca_aggregate(target_res)
  rain_r <- .mirca_read_binary(
    rain_path,
    dims$ncols,
    dims$nrows,
    dims$nmonths
  ) |>
    .mirca_aggregate(target_res)
  irrig_tbl <- .raster_to_tibble(irrig_r, "irrig_ha") |>
    dplyr::filter(!is.na(irrig_ha), irrig_ha > 0)
  rain_tbl <- .raster_to_tibble(rain_r, "rainfed_ha") |>
    dplyr::filter(!is.na(rainfed_ha), rainfed_ha > 0)
  dplyr::full_join(irrig_tbl, rain_tbl, by = c("lon", "lat")) |>
    dplyr::mutate(
      irrig_ha = dplyr::if_else(is.na(irrig_ha), 0, irrig_ha),
      rainfed_ha = dplyr::if_else(is.na(rainfed_ha), 0, rainfed_ha),
      mirca_class = crop_num
    ) |>
    dplyr::filter(irrig_ha > 0 | rainfed_ha > 0)
}

# ==== Section 5: MIRCA irrigation =========================================
#
# Processes MIRCA2000 binary grids to produce country-level and gridded
# irrigation fractions.  Merges logic from prepare_mirca_inputs.R and
# prepare_mirca_irrigation.R.

prepare_mirca_irrigation <- function(
  l_files_dir,
  output_dir,
  country_grid,
  target_res
) {
  cli::cli_h2("Section 5: MIRCA2000 irrigation")

  mirca_dir <- file.path(l_files_dir, "Irrigation maps_CIRCA-2000")
  if (!dir.exists(mirca_dir)) {
    cli::cli_alert_warning("MIRCA2000 directory not found: {mirca_dir}")
    return(NULL)
  }

  dims <- list(ncols = 4320L, nrows = 2160L, nmonths = 12L)

  # Read MIRCA mapping
  mirca_map <- readr::read_csv(
    .find_extdata_file("mirca_mapping.csv"),
    show_col_types = FALSE
  )
  cft_map <- readr::read_csv(
    .find_extdata_file("cft_mapping.csv"),
    show_col_types = FALSE
  )

  # Build MIRCA -> FAOSTAT expansion
  direct <- mirca_map |>
    dplyr::filter(!is.na(item_prod_code)) |>
    dplyr::select(mirca_class, item_prod_code)

  citrus_codes <- c(490L, 495L, 497L, 507L, 512L)
  citrus_class <- mirca_map$mirca_class[
    mirca_map$mirca_group == "citrus" & !is.na(mirca_map$mirca_group)
  ]
  citrus_expanded <- tibble::tibble(
    mirca_class = rep(citrus_class, length(citrus_codes)),
    item_prod_code = citrus_codes
  )

  # MIRCA "pulses", "others_perennial", "others_annual" are catch-alls
  # that must expand to every item_prod_code not already assigned to
  # a specific MIRCA class. We derive the buckets from the cft_map's
  # luh2_type so the expansion stays consistent with the WHEP
  # taxonomy (whose cft_name is now granular).
  direct_codes <- direct$item_prod_code
  pulses_ipc <- cft_map$item_prod_code[cft_map$cft_name == "pulses"]
  perennial_ipc <- cft_map$item_prod_code[
    cft_map$luh2_type == "c3per" &
      !cft_map$item_prod_code %in% c(direct_codes, citrus_codes)
  ]
  annual_ipc <- cft_map$item_prod_code[
    cft_map$luh2_type %in%
      c("c3ann", "c4ann", "c3nfx") &
      !cft_map$item_prod_code %in% c(direct_codes, pulses_ipc)
  ]
  group_lookup <- list(
    pulses = pulses_ipc,
    others_perennial = perennial_ipc,
    others_annual = annual_ipc
  )

  group_rows <- mirca_map |>
    dplyr::filter(is.na(item_prod_code), !is.na(mirca_group)) |>
    dplyr::filter(mirca_group %in% names(group_lookup)) |>
    dplyr::select(mirca_class, mirca_group)
  group_rows <- purrr::pmap_dfr(group_rows, function(mirca_class, mirca_group) {
    tibble::tibble(
      mirca_class = mirca_class,
      item_prod_code = group_lookup[[mirca_group]]
    )
  })

  mirca_to_fao <- dplyr::bind_rows(direct, citrus_expanded, group_rows)
  cli::cli_alert_info(
    "MIRCA mapping: {n_distinct(mirca_to_fao$mirca_class)} classes -> {nrow(mirca_to_fao)} codes"
  )

  # Process all 26 MIRCA crop classes in parallel
  Sys.setenv(R_PROFILE_USER = nullfile())
  on.exit(Sys.unsetenv("R_PROFILE_USER"), add = TRUE)
  old_plan <- suppressMessages(future::plan(future::multisession))
  on.exit(future::plan(old_plan), add = TRUE)

  cli::cli_alert_info("Processing 26 MIRCA crops in parallel...")
  all_crops <- suppressMessages(furrr::future_map(
    1:26,
    \(crop_num) .mirca_process_crop(crop_num, mirca_dir, target_res, dims),
    .options = furrr::furrr_options(seed = NULL)
  )) |>
    purrr::compact()

  gridded_mirca <- dplyr::bind_rows(all_crops) |>
    dplyr::inner_join(country_grid, by = c("lon", "lat"))

  agg <- .mirca_expand_aggregate(gridded_mirca, mirca_to_fao)

  .save_parquet(agg$country, output_dir, "mirca_irrigation_country")
  .save_parquet(agg$patterns, output_dir, "mirca_irrigation_patterns")

  cli::cli_alert_success("MIRCA2000 preparation complete")
  invisible(list(
    country = agg$country,
    patterns = agg$patterns
  ))
}


# ==== Section 6: Yield inputs =============================================
#
# Country-level yields from FAOSTAT + EarthStat spatial yield index.

prepare_yield_inputs <- function(
  l_files_dir,
  output_dir,
  target_res,
  year_range = 1961L:2022L,
  prod = NULL
) {
  cli::cli_h2("Section 6: Yield inputs")

  cft_mapping <- .read_cft_mapping()

  if (is.null(prod)) {
    prod <- .load_or_cache_production(output_dir, year_range)
  }

  # Country yields from production/area

  production <- prod |>
    dplyr::filter(
      .data$unit == "tonnes",
      as.integer(.data$item_prod_code) %in% cft_mapping$item_prod_code,
      .data$year %in% year_range
    ) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      production_t = .data$value
    ) |>
    dplyr::filter(production_t > 0)

  area_harvested <- prod |>
    dplyr::filter(
      .data$unit == "ha",
      as.integer(.data$item_prod_code) %in% cft_mapping$item_prod_code,
      .data$year %in% year_range
    ) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_prod_code = as.integer(.data$item_prod_code),
      harvested_area_ha = .data$value
    ) |>
    dplyr::filter(harvested_area_ha > 0)

  country_yields <- production |>
    dplyr::inner_join(
      area_harvested,
      by = c("year", "area_code", "item_prod_code")
    ) |>
    dplyr::mutate(yield_t_ha = production_t / harvested_area_ha) |>
    dplyr::filter(is.finite(yield_t_ha), yield_t_ha > 0) |>
    dplyr::select(year, area_code, item_prod_code, yield_t_ha)

  .save_parquet(country_yields, output_dir, "country_yields")

  # Spatial yield index from EarthStat
  earthstat_dir <- file.path(
    l_files_dir,
    "HarvestedAreaYield175Crops_Geotiff",
    "GeoTiff"
  )
  xwalk <- .read_earthstat_mapping() |> dplyr::filter(!is.na(item_prod_code))

  cli::cli_alert_info(
    "Reading yield rasters for {nrow(xwalk)} EarthStat crops in parallel..."
  )
  read_one_yield <- function(i) {
    .read_one_earthstat_yield(
      earthstat_dir,
      xwalk$earthstat_name[i],
      xwalk$item_prod_code[i],
      target_res
    )
  }
  n_workers <- min(nrow(xwalk), max(1L, parallel::detectCores() - 1L))
  raw_yields <- if (.Platform$OS.type == "windows") {
    lapply(seq_len(nrow(xwalk)), read_one_yield)
  } else {
    parallel::mclapply(
      seq_len(nrow(xwalk)),
      read_one_yield,
      mc.cores = n_workers
    )
  }
  raw_yields <- dplyr::bind_rows(raw_yields)

  raw_yields <- raw_yields |>
    dplyr::summarise(
      yield_t_ha = mean(yield_t_ha, na.rm = TRUE),
      .by = c(lon, lat, item_prod_code)
    )

  country_grid <- nanoparquet::read_parquet(
    file.path(output_dir, "country_grid.parquet")
  )
  crop_patterns <- nanoparquet::read_parquet(
    file.path(output_dir, "crop_patterns.parquet")
  )

  yields_with_country <- raw_yields |>
    dplyr::inner_join(country_grid, by = c("lon", "lat"))

  yields_weighted <- yields_with_country |>
    dplyr::left_join(crop_patterns, by = c("lon", "lat", "item_prod_code")) |>
    dplyr::mutate(weight = dplyr::coalesce(harvest_fraction, 1.0))

  country_mean_yields <- yields_weighted |>
    dplyr::summarise(
      country_mean = sum(yield_t_ha * weight, na.rm = TRUE) /
        sum(weight, na.rm = TRUE),
      .by = c(area_code, item_prod_code)
    ) |>
    dplyr::filter(country_mean > 0)

  spatial_yield_index <- yields_with_country |>
    dplyr::inner_join(
      country_mean_yields,
      by = c("area_code", "item_prod_code")
    ) |>
    dplyr::mutate(
      spatial_yield_index = yield_t_ha / country_mean
    ) |>
    dplyr::select(lon, lat, item_prod_code, spatial_yield_index) |>
    dplyr::mutate(
      spatial_yield_index = pmin(pmax(spatial_yield_index, 0.1), 5.0)
    )

  .save_parquet(spatial_yield_index, output_dir, "spatial_yield_index")

  cli::cli_alert_success("Yield inputs complete")
  invisible(list(
    country_yields = country_yields,
    spatial_yield_index = spatial_yield_index
  ))
}


# ==== Section 7 helpers ===================================================
#
# Country-year totals for Synthetic / Manure / Grassland_excretion N
# consumed by prepare_nitrogen_inputs(). Defined at file scope so they
# stay testable in isolation. Manure-applied and grassland-excretion are
# derived from WHEP's own livestock pipeline (heads x IPCC NEx) split
# by constant per-country shares calibrated against FAOSTAT 1961-1965;
# FAOSTAT manure-applied is no longer consumed directly. Synthetic N is
# FAOSTAT for 1961+ and Smil (2001) for 1913-1960.

.faostat_synth_country_total <- function(regions) {
  whep::whep_read_file("faostat-fertilizer-nutrients") |>
    dplyr::filter(
      Element == "Agricultural Use",
      Item == "Nutrient nitrogen N (total)"
    ) |>
    dplyr::transmute(
      area_code = as.integer(`Area Code`),
      year = as.integer(Year),
      mg_n = Value
    ) |>
    dplyr::inner_join(
      dplyr::select(regions, area_code, area_name),
      by = "area_code"
    ) |>
    dplyr::filter(!is.na(mg_n), mg_n >= 0)
}

.faostat_manure_country_long <- function(regions) {
  elements <- c(
    applied = "Manure applied to soils (N content)",
    pasture = "Manure left on pasture (N content)",
    mms = "Manure management (manure treated, N content)",
    excreted = "Amount excreted in manure (N content)"
  )
  whep::whep_read_file("faostat-emissions-livestock") |>
    dplyr::filter(Item == "All Animals", Element %in% elements) |>
    dplyr::transmute(
      area_code = as.integer(`Area Code`),
      year = as.integer(Year),
      element = names(elements)[match(Element, elements)],
      mg_n = Value / 1000
    ) |>
    dplyr::inner_join(
      dplyr::select(regions, area_code, area_name),
      by = "area_code"
    ) |>
    dplyr::filter(!is.na(mg_n), mg_n >= 0)
}

.faostat_manure_shares_const <- function(
  manure_long,
  share_window = 1961L:1965L
) {
  manure_long |>
    dplyr::filter(year %in% share_window) |>
    tidyr::pivot_wider(names_from = element, values_from = mg_n) |>
    dplyr::filter(!is.na(excreted), excreted > 0) |>
    dplyr::mutate(
      applied_share = dplyr::coalesce(applied, 0) / excreted,
      pasture_share = dplyr::coalesce(pasture, 0) / excreted
    ) |>
    dplyr::summarise(
      applied_share = mean(applied_share, na.rm = TRUE),
      pasture_share = mean(pasture_share, na.rm = TRUE),
      .by = c(area_code, area_name)
    )
}

.livestock_manure_n_country <- function(prod, regions) {
  livestock_mapping <- readr::read_csv(
    .find_extdata_file("livestock_mapping.csv"),
    show_col_types = FALSE
  )
  stocks <- prod |>
    dplyr::filter(
      unit == "heads",
      as.integer(item_prod_code) %in% livestock_mapping$item_code
    ) |>
    dplyr::transmute(
      area_code = as.integer(area_code),
      item_code = as.integer(item_prod_code),
      year = as.integer(year),
      heads = value
    ) |>
    dplyr::filter(!is.na(area_code), heads > 0) |>
    dplyr::inner_join(
      dplyr::select(livestock_mapping, item_code, species_group),
      by = "item_code"
    ) |>
    dplyr::summarise(
      heads = sum(heads, na.rm = TRUE),
      .by = c(year, area_code, species_group)
    )
  nex <- .compute_default_n_excretion(
    dplyr::distinct(stocks, year, area_code, species_group)
  )
  stocks |>
    dplyr::inner_join(
      dplyr::select(nex, year, area_code, species_group, nex_kg_n_head),
      by = c("year", "area_code", "species_group")
    ) |>
    dplyr::mutate(manure_n_mg = heads * nex_kg_n_head / 1000) |>
    dplyr::summarise(
      mg_n_excreted = sum(manure_n_mg, na.rm = TRUE),
      .by = c(year, area_code)
    ) |>
    dplyr::inner_join(
      dplyr::select(regions, area_code, area_name),
      by = "area_code"
    )
}

.livestock_manure_split <- function(excreted_country, shares_const) {
  excreted_country |>
    dplyr::inner_join(
      dplyr::select(shares_const, area_code, applied_share, pasture_share),
      by = "area_code"
    ) |>
    dplyr::transmute(
      year,
      area_code,
      area_name,
      Manure = mg_n_excreted * applied_share,
      Grassland_excretion = mg_n_excreted * pasture_share
    ) |>
    tidyr::pivot_longer(
      c("Manure", "Grassland_excretion"),
      names_to = "fert_type",
      values_to = "mg_n"
    ) |>
    dplyr::filter(!is.na(mg_n), mg_n >= 0)
}

.smil_global_yearly <- function(first_year = 1913L, last_year = 1960L) {
  anchors <- whep::smil_2001_synthetic_n_global |>
    dplyr::transmute(
      year = as.integer(year),
      global_mg_n = global_kt_n * 1000
    )
  tibble::tibble(year = seq.int(first_year, last_year)) |>
    dplyr::left_join(anchors, by = "year") |>
    whep::fill_linear(global_mg_n, time_col = year)
}

.smil_country_share <- function(
  synth_country_faostat,
  share_window = 1961L:1965L
) {
  smil_anchor <- .smil_global_yearly(
    first_year = min(share_window),
    last_year = max(share_window)
  )
  smil_window_avg <- mean(smil_anchor$global_mg_n, na.rm = TRUE)
  synth_country_faostat |>
    dplyr::filter(year %in% share_window, mg_n > 0) |>
    dplyr::summarise(
      country_avg_mg_n = mean(mg_n, na.rm = TRUE),
      .by = c(area_code, area_name)
    ) |>
    dplyr::mutate(country_share = country_avg_mg_n / smil_window_avg) |>
    dplyr::select(area_code, area_name, country_share)
}

.smil_synth_pre_1961 <- function(
  synth_country_faostat,
  share_window = 1961L:1965L,
  first_year = 1913L
) {
  smil_yearly <- .smil_global_yearly(first_year, 1960L)
  country_share <- .smil_country_share(synth_country_faostat, share_window)
  tidyr::expand_grid(smil_yearly, country_share) |>
    dplyr::mutate(mg_n = global_mg_n * country_share) |>
    dplyr::transmute(year, area_code, area_name, mg_n) |>
    dplyr::filter(!is.na(mg_n), mg_n > 0)
}


# ==== Section 7: Nitrogen inputs ==========================================
#
# N/P/K inputs from FAOSTAT + spatial distribution.
# Reference datasets are bundled as package data or fetched via pins.
# Pre-FAOSTAT (pre-1961) synthetic N: Smil (2001) global anchors scaled by
# 1961-1965 country shares. Manure (applied + pasture): livestock heads x
# IPCC NEx, split by constant per-country shares calibrated to FAOSTAT
# 1961-1965.

prepare_nitrogen_inputs <- function(
  l_files_dir,
  output_dir,
  year_range,
  prod = NULL
) {
  cli::cli_h2("Section 7: Nitrogen / fertilizer inputs")

  regions <- readr::read_csv(
    system.file("extdata", "regions.csv", package = "whep"),
    show_col_types = FALSE
  )
  items_prod <- readr::read_csv(
    system.file("extdata", "items_prod.csv", package = "whep"),
    show_col_types = FALSE
  )

  # ---- 7a. N totals: synth (FAOSTAT 1961+ + Smil 1913-1960) and
  #          manure (livestock heads x NEx, split by constant shares) ----
  .read_n_totals_local <- function(regions, prod) {
    synth_faostat <- .faostat_synth_country_total(regions)
    synth_smil <- .smil_synth_pre_1961(synth_faostat)
    synth <- dplyr::bind_rows(synth_faostat, synth_smil) |>
      dplyr::mutate(fert_type = "Synthetic") |>
      dplyr::select(year, area_code, area_name, fert_type, mg_n)

    manure_long <- .faostat_manure_country_long(regions)
    shares_const <- .faostat_manure_shares_const(manure_long)
    excreted_country <- .livestock_manure_n_country(prod, regions)
    manure_lu <- .livestock_manure_split(excreted_country, shares_const) |>
      dplyr::select(year, area_code, area_name, fert_type, mg_n)

    dplyr::bind_rows(synth, manure_lu) |>
      dplyr::filter(!is.na(mg_n), mg_n >= 0)
  }

  # ---- 7b. P/K totals (via pin) ----
  .read_faostat_pk_totals_local <- function(regions) {
    whep::whep_read_file("faostat-fertilizer-nutrients") |>
      dplyr::filter(
        Element == "Agricultural Use",
        Item %in%
          c("Nutrient phosphate P2O5 (total)", "Nutrient potash K2O (total)")
      ) |>
      dplyr::transmute(
        area_code = `Area Code`,
        year = as.integer(Year),
        nutrient = dplyr::if_else(grepl("phosphate", Item), "P", "K"),
        mg_nutrient = Value
      ) |>
      dplyr::inner_join(
        regions |> dplyr::select(area_code, area_name),
        by = "area_code"
      ) |>
      dplyr::filter(!is.na(mg_nutrient), mg_nutrient >= 0)
  }

  # ---- 7c. Cropland/Grassland split (EuroAgriDB via pins) ----
  .read_crop_grass_split_local <- function(regions) {
    synth_eu <- whep::whep_read_file("eu-agridb-synthetic-fertilizer") |>
      filter(Symbol %in% c("Q_C", "Q_PG")) |>
      mutate(
        land_use = if_else(Symbol == "Q_C", "Cropland", "Grassland"),
        fert_type = "Synthetic"
      )
    manure_eu <- whep::whep_read_file("eu-agridb-manure-flows") |>
      filter(Symbol %in% c("A_C", "A_PG")) |>
      mutate(
        land_use = if_else(Symbol == "A_C", "Cropland", "Grassland"),
        fert_type = "Manure"
      )
    euadb <- bind_rows(synth_eu, manure_eu) |>
      rename(iso2c = Region) |>
      mutate(mg_n_lu = Value * 1000) |>
      left_join(
        regions |>
          select(iso2c, area_code, area_name),
        by = "iso2c"
      ) |>
      filter(!is.na(area_code)) |>
      select(
        year = Year,
        area_code,
        area_name,
        land_use,
        fert_type,
        mg_n_lu
      ) |>
      mutate(
        lu_share_eu = mg_n_lu / sum(mg_n_lu),
        .by = c(year, area_code, fert_type)
      )

    lass <- whep::lassaletta_grassland_share |>
      rename(lassaletta_name = Country) |>
      left_join(
        select(regions, iso3c, area_code, area_name),
        by = c("lassaletta_name" = "area_name")
      ) |>
      filter(!is.na(area_code)) |>
      select(year, area_code, grass_share)

    list(euadb = euadb, lassaletta = lass)
  }

  # ---- 7d. Crop-specific base-year rates (from package data) ----
  .read_crop_base_rates_local <- function(regions) {
    crop_manure <- whep::crops_manure_n |>
      rename(crop_name = Crop_name, iso3c = ISO, manure_mg_n = Manure_N_Mg) |>
      left_join(select(regions, iso3c, area_code), by = "iso3c") |>
      filter(!is.na(area_code)) |>
      summarize(manure_mg_n = sum(manure_mg_n), .by = c(area_code, crop_name))

    crop_synthetic <- whep::mueller_synthetic_n |>
      rename(crop_name = crop_process) |>
      mutate(
        iso3c = recode(
          iso3c,
          "SRM" = "SCG",
          "GUA" = "GTM",
          "BZE" = "BLZ",
          "COS" = "CRI",
          "ELS" = "SLV",
          "HAI" = "HTI",
          "HON" = "HND",
          "ROM" = "ROU",
          "TRI" = "TTO",
          "ZAR" = "COD",
          "BHA" = "BHS",
          "BAR" = "BRB",
          "DMI" = "DMA",
          "STL" = "LCA"
        )
      ) |>
      left_join(select(regions, iso3c, area_code), by = "iso3c") |>
      filter(!is.na(area_code), !is.na(rate_value)) |>
      summarize(
        kg_n_ha_synth = mean(rate_value, na.rm = TRUE),
        .by = c(area_code, crop_name)
      )
    list(crop_manure = crop_manure, crop_synthetic = crop_synthetic)
  }

  # ---- 7e. West gridded manure ----
  .read_west_manure_local <- function(
    l_files_dir,
    country_grid,
    items_prod,
    target_res,
    output_dir
  ) {
    west_dir <- file.path(l_files_dir, "Manure_Westetal2014", "N")
    if (!dir.exists(west_dir)) {
      return(NULL)
    }

    crop_map <- tibble::tribble(
      ~west_crop,    ~item_prod_code,
      "barley",      44L, "cassava",    340L, "cotton",     274L,
      "groundnut",  242L, "maize",       56L, "millet",      79L,
      "oilpalm",    217L, "potato",     328L, "rapeseed",   223L,
      "rice",        27L, "rye",         71L, "sorghum",     83L,
      "soybean",    236L, "sugarbeet",  157L, "sugarcane",  780L,
      "sunflower",  222L, "wheat",       15L
    )

    .read_one_west <- function(i) {
      crop <- crop_map$west_crop[i]
      code <- crop_map$item_prod_code[i]
      nc_path <- file.path(
        west_dir,
        paste0("NappliedperHA", crop, "Westetal.nc")
      )
      if (!file.exists(nc_path)) {
        return(tibble::tibble())
      }
      tryCatch(
        {
          r <- terra::rast(nc_path)
          src_res <- terra::res(r)[1]
          agg_factor <- max(1L, as.integer(round(target_res / src_res)))
          if (agg_factor > 1) {
            r_agg <- terra::aggregate(
              r,
              fact = agg_factor,
              fun = "mean",
              na.rm = TRUE
            )
          } else {
            r_agg <- r
          }
          coords <- terra::xyFromCell(r_agg, seq_len(terra::ncell(r_agg)))
          tibble::tibble(
            lon = round(coords[, 1], 2),
            lat = round(coords[, 2], 2),
            kg_n_ha_manure = terra::values(r_agg)[, 1]
          ) |>
            dplyr::filter(!is.na(kg_n_ha_manure), kg_n_ha_manure > 0) |>
            dplyr::mutate(item_prod_code = code)
        },
        error = \(e) tibble::tibble()
      )
    }
    n_workers <- min(
      nrow(crop_map),
      max(1L, parallel::detectCores() - 1L)
    )
    raw_results <- if (.Platform$OS.type == "windows") {
      lapply(seq_len(nrow(crop_map)), .read_one_west)
    } else {
      parallel::mclapply(
        seq_len(nrow(crop_map)),
        .read_one_west,
        mc.cores = n_workers
      )
    }
    results <- dplyr::bind_rows(raw_results)

    if (nrow(results) == 0) {
      return(NULL)
    }

    results_geo <- results |>
      dplyr::inner_join(country_grid, by = c("lon", "lat"))

    nanoparquet::write_parquet(
      dplyr::select(
        results_geo,
        lon,
        lat,
        area_code,
        item_prod_code,
        kg_n_ha_manure
      ),
      file.path(output_dir, "gridded_west_manure.parquet")
    )

    results_geo |>
      dplyr::summarize(
        kg_n_ha_manure_west = mean(kg_n_ha_manure, na.rm = TRUE),
        .by = c("area_code", "item_prod_code")
      ) |>
      dplyr::left_join(
        dplyr::select(items_prod, item_prod_code, item_prod_name),
        by = "item_prod_code"
      ) |>
      dplyr::rename(crop_name = item_prod_name) |>
      dplyr::filter(!is.na(crop_name))
  }

  # ---- 7f. EarthStat country N rates ----
  .read_earthstat_rates_local <- function(
    l_files_dir,
    country_grid,
    items_prod,
    target_res,
    output_dir
  ) {
    fert_dir <- file.path(l_files_dir, "EarthStat - Crop Specific Fertilizers")
    if (!dir.exists(fert_dir)) {
      return(NULL)
    }

    crop_map <- .earthstat_fertilizer_mapping()

    .read_one_es <- function(i) {
      crop <- crop_map$earthstat_fert_name[i]
      code <- crop_map$item_prod_code[i]
      tif_path <- file.path(
        fert_dir,
        paste0("Fertilizer_", crop),
        paste0(crop, "_NitrogenApplication_Rate.tif")
      )
      if (!file.exists(tif_path)) {
        return(tibble::tibble())
      }
      tryCatch(
        {
          r <- terra::rast(tif_path)
          src_res <- terra::res(r)[1]
          agg_factor <- max(1L, as.integer(round(target_res / src_res)))
          if (agg_factor > 1) {
            r_agg <- terra::aggregate(
              r,
              fact = agg_factor,
              fun = "mean",
              na.rm = TRUE
            )
          } else {
            r_agg <- r
          }
          coords <- terra::xyFromCell(r_agg, seq_len(terra::ncell(r_agg)))
          tibble::tibble(
            lon = round(coords[, 1], 2),
            lat = round(coords[, 2], 2),
            kg_n_ha_synth_es = terra::values(r_agg)[, 1]
          ) |>
            dplyr::filter(!is.na(kg_n_ha_synth_es), kg_n_ha_synth_es > 0) |>
            dplyr::mutate(item_prod_code = code)
        },
        error = \(e) tibble::tibble()
      )
    }
    n_workers <- min(
      nrow(crop_map),
      max(1L, parallel::detectCores() - 1L)
    )
    raw_results <- if (.Platform$OS.type == "windows") {
      lapply(seq_len(nrow(crop_map)), .read_one_es)
    } else {
      parallel::mclapply(
        seq_len(nrow(crop_map)),
        .read_one_es,
        mc.cores = n_workers
      )
    }
    results <- dplyr::bind_rows(raw_results)

    if (nrow(results) == 0) {
      return(NULL)
    }

    results_geo <- results |>
      dplyr::inner_join(country_grid, by = c("lon", "lat"))

    nanoparquet::write_parquet(
      dplyr::select(
        results_geo,
        lon,
        lat,
        area_code,
        item_prod_code,
        kg_n_ha_synth_es
      ),
      file.path(output_dir, "gridded_earthstat_synth.parquet")
    )

    results_geo |>
      dplyr::summarize(
        kg_n_ha_synth_es = mean(kg_n_ha_synth_es, na.rm = TRUE),
        .by = c("area_code", "item_prod_code")
      ) |>
      dplyr::left_join(
        dplyr::select(items_prod, item_prod_code, item_prod_name),
        by = "item_prod_code"
      ) |>
      dplyr::rename(crop_name = item_prod_name) |>
      dplyr::filter(!is.na(crop_name))
  }

  # ---- 7g. Spatial N rate index ----
  .compute_spatial_n_index_local <- function(output_dir) {
    west_path <- file.path(output_dir, "gridded_west_manure.parquet")
    es_path <- file.path(output_dir, "gridded_earthstat_synth.parquet")
    parts <- list()

    if (file.exists(west_path)) {
      west <- nanoparquet::read_parquet(west_path)
      west_means <- west |>
        dplyr::summarize(
          country_mean = mean(kg_n_ha_manure, na.rm = TRUE),
          .by = c("area_code", "item_prod_code")
        ) |>
        dplyr::filter(country_mean > 0)
      west_idx <- west |>
        dplyr::inner_join(west_means, by = c("area_code", "item_prod_code")) |>
        dplyr::mutate(
          spatial_n_index = kg_n_ha_manure / country_mean,
          fert_type = "Manure"
        ) |>
        dplyr::select(lon, lat, item_prod_code, fert_type, spatial_n_index)
      parts <- c(parts, list(west_idx))
    }

    if (file.exists(es_path)) {
      es <- nanoparquet::read_parquet(es_path)
      es_means <- es |>
        dplyr::summarize(
          country_mean = mean(kg_n_ha_synth_es, na.rm = TRUE),
          .by = c("area_code", "item_prod_code")
        ) |>
        dplyr::filter(country_mean > 0)
      es_idx <- es |>
        dplyr::inner_join(es_means, by = c("area_code", "item_prod_code")) |>
        dplyr::mutate(
          spatial_n_index = kg_n_ha_synth_es / country_mean,
          fert_type = "Synthetic"
        ) |>
        dplyr::select(lon, lat, item_prod_code, fert_type, spatial_n_index)
      parts <- c(parts, list(es_idx))
    }

    if (length(parts) == 0) {
      return(NULL)
    }
    spatial_idx <- dplyr::bind_rows(parts) |>
      dplyr::mutate(spatial_n_index = pmax(0.1, pmin(10, spatial_n_index)))
    .save_parquet(spatial_idx, output_dir, "spatial_n_index")
    spatial_idx
  }

  # ---- 7h. Coello et al. 2025 ----
  .prepare_coello_inputs_local <- function(l_files_dir, regions, output_dir) {
    coello_file <- file.path(output_dir, "coello_crop_rates.parquet")
    if (file.exists(coello_file)) {
      return(nanoparquet::read_parquet(coello_file))
    }
    coello_dir <- file.path(l_files_dir, "Coello2025")
    csv_file <- file.path(coello_dir, "Prediction_corrected.csv")
    if (!file.exists(csv_file)) {
      return(NULL)
    }

    raw <- data.table::fread(csv_file) |> tibble::as_tibble()
    coello <- raw |>
      transmute(
        area_code = as.integer(FAOStat_area_code),
        coello_crop_code = as.character(Crop_Code),
        year = as.integer(Year),
        kg_n_ha_coello = as.numeric(predicted_N_avg_app_cor),
        kg_p2o5_ha_coello = as.numeric(predicted_P2O5_avg_app_cor),
        kg_k2o_ha_coello = as.numeric(predicted_K2O_avg_app_cor)
      ) |>
      filter(!is.na(area_code), !is.na(year))

    mapping_file <- .find_extdata_file("coello_mapping.csv")
    if (!file.exists(mapping_file)) {
      return(coello)
    }

    coello_map <- readr::read_csv(mapping_file, show_col_types = FALSE) |>
      select(item_prod_code, coello_crop_code, coello_crop_name)

    items_ref <- readr::read_csv(
      system.file("extdata", "items_prod.csv", package = "whep"),
      show_col_types = FALSE
    ) |>
      select(item_prod_code, item_prod_name)

    coello_items <- coello |>
      inner_join(
        coello_map,
        by = "coello_crop_code",
        relationship = "many-to-many"
      ) |>
      left_join(select(regions, area_code, area_name), by = "area_code") |>
      filter(!is.na(area_name)) |>
      mutate(
        kg_n_ha_coello = pmax(kg_n_ha_coello, 0),
        kg_p2o5_ha_coello = pmax(kg_p2o5_ha_coello, 0),
        kg_k2o_ha_coello = pmax(kg_k2o_ha_coello, 0)
      ) |>
      left_join(items_ref, by = "item_prod_code") |>
      select(
        year,
        area_code,
        area_name,
        item_prod_code,
        crop_name = item_prod_name,
        coello_crop_code,
        coello_crop_name,
        kg_n_ha_coello,
        kg_p2o5_ha_coello,
        kg_k2o_ha_coello
      )

    .save_parquet(coello_items, output_dir, "coello_crop_rates")
    coello_items
  }

  # ---- 7i. N deposition ----
  .prepare_n_deposition_local <- function(l_files_dir, regions, output_dir) {
    dep_file <- file.path(output_dir, "n_deposition.parquet")
    if (file.exists(dep_file)) {
      dep <- nanoparquet::read_parquet(dep_file)
      if (all(c("nhx", "noy") %in% names(dep))) {
        cli::cli_alert_info(
          "Reading pre-computed deposition with NHx/NOy split"
        )
        return(dep)
      }
      cli::cli_alert_info(
        "Cached deposition lacks NHx/NOy split; re-extracting..."
      )
    }
    dep <- .extract_hani_deposition(l_files_dir, regions)
    if (!is.null(dep)) {
      .save_parquet(dep, output_dir, "n_deposition")
      return(dep)
    }
    cli::cli_alert_warning(
      "N deposition data not available (run download_nitrogen.R)"
    )
    NULL
  }

  .extract_hani_deposition <- function(l_files_dir, regions) {
    hani_dir <- file.path(l_files_dir, "HaNi")
    if (!dir.exists(hani_dir)) {
      return(NULL)
    }
    zip_files <- list.files(hani_dir, pattern = "\\.zip$", full.names = TRUE)
    if (length(zip_files) == 0) {
      return(NULL)
    }
    cli::cli_alert(
      "Extracting N deposition from HaNi rasters to 0.5-degree grid..."
    )
    if (!requireNamespace("terra", quietly = TRUE)) {
      install.packages("terra", quiet = TRUE)
    }

    # 0.5-degree target grid matching WHEP
    target <- terra::rast(
      resolution = 0.5,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90
    )
    agg_factor <- 6L # 5 arcmin -> 30 arcmin

    .extract_one_hani <- function(zip_path) {
      tmpd <- tempfile()
      dir.create(tmpd)
      on.exit(unlink(tmpd, recursive = TRUE), add = TRUE)
      utils::unzip(zip_path, exdir = tmpd)
      nc_files <- list.files(tmpd, pattern = "\\.nc$", full.names = TRUE)
      if (length(nc_files) == 0) {
        return(tibble::tibble())
      }
      r <- terra::rast(nc_files)
      # Aggregate to 0.5° by averaging 5-arcmin cells
      r_05 <- terra::aggregate(r, fact = agg_factor, fun = "mean", na.rm = TRUE)
      vals <- terra::as.data.frame(r_05, xy = TRUE)
      vals$file <- basename(zip_path)
      tibble::as_tibble(vals)
    }

    dep_raw <- lapply(zip_files, .extract_one_hani) |> dplyr::bind_rows()

    if (nrow(dep_raw) == 0) {
      return(NULL)
    }

    dep <- dep_raw |>
      tidyr::pivot_longer(
        dplyr::starts_with("ndep_"),
        names_to = "layer",
        values_to = "value"
      ) |>
      dplyr::mutate(
        parameter = gsub(".*ndep_(\\w+)\\.zip", "\\1", file),
        layer_num = as.integer(gsub("ndep_.*_(\\d+).*", "\\1", layer)),
        year = 1850L + layer_num,
        value = value / 10000000
      ) |>
      dplyr::filter(!is.na(value), !is.na(year)) |>
      dplyr::select(x, y, year, parameter, value) |>
      tidyr::pivot_wider(names_from = parameter, values_from = value) |>
      dplyr::mutate(
        deposit_kg_n_ha = nhx + noy,
        lon = round(x, 2),
        lat = round(y, 2)
      ) |>
      dplyr::filter(!is.na(nhx) | !is.na(noy)) |>
      dplyr::select(lon, lat, year, deposit_kg_n_ha, nhx, noy)

    cli::cli_alert_success(
      "HaNi deposition: {dplyr::n_distinct(paste(dep$lon, dep$lat))} cells, ",
      "{min(dep$year)}–{max(dep$year)}"
    )
    dep
  }

  # ---- Execute nitrogen pipeline ----
  cli::cli_alert_info("Running nitrogen input pipeline")

  if (is.null(prod)) {
    prod <- .load_or_cache_production(output_dir, year_range)
  }

  # Load crop areas
  crop_areas_file <- file.path(output_dir, "country_areas.parquet")
  crop_areas <- NULL
  if (file.exists(crop_areas_file)) {
    crop_areas_raw <- nanoparquet::read_parquet(crop_areas_file)
    crop_areas <- crop_areas_raw |>
      left_join(select(regions, area_code, area_name), by = "area_code") |>
      left_join(
        items_prod |> select(item_prod_code, item_prod_name),
        by = "item_prod_code"
      ) |>
      transmute(
        year,
        area_code,
        area_name,
        crop_name = item_prod_name,
        area_ha = harvested_area_ha
      ) |>
      filter(!is.na(area_ha), area_ha > 0)
  }

  n_totals <- .read_n_totals_local(regions, prod)
  pk_totals <- .read_faostat_pk_totals_local(regions)
  lu_split <- .read_crop_grass_split_local(regions)
  base_rates <- .read_crop_base_rates_local(regions)

  # Spatial rate enhancements
  country_grid_file <- file.path(output_dir, "country_grid.parquet")
  if (file.exists(country_grid_file)) {
    country_grid <- nanoparquet::read_parquet(country_grid_file)
    base_rates$west_manure_rates <- .read_west_manure_local(
      l_files_dir,
      country_grid,
      items_prod,
      0.5,
      output_dir
    )
    base_rates$earthstat_synth_rates <- .read_earthstat_rates_local(
      l_files_dir,
      country_grid,
      items_prod,
      0.5,
      output_dir
    )
    .compute_spatial_n_index_local(output_dir)
  }

  coello_rates <- .prepare_coello_inputs_local(l_files_dir, regions, output_dir)
  if (!is.null(coello_rates)) {
    base_rates$coello_rates <- coello_rates
  }

  n_deposition <- .prepare_n_deposition_local(l_files_dir, regions, output_dir)

  # Distribute N among crops (full algorithm from prepare_nitrogen_inputs.R)
  n_crops <- NULL
  nsbnf <- NULL
  pk_crop <- NULL

  if (!is.null(crop_areas) && nrow(crop_areas) > 0) {
    # Distribute N to crops using the three-layer approach
    n_crops <- .distribute_n_to_crops(
      n_totals,
      lu_split,
      base_rates,
      crop_areas,
      regions
    )
    # NSBNF
    nsbnf_rate <- 13
    nsbnf <- crop_areas |>
      filter(!is.na(area_ha), area_ha > 0) |>
      mutate(
        fert_type = "NSBNF",
        kg_n_ha = nsbnf_rate,
        mg_n = kg_n_ha * area_ha / 1000,
        land_use = "Cropland"
      ) |>
      select(
        year,
        area_code,
        area_name,
        crop_name,
        land_use,
        fert_type,
        area_ha,
        mg_n,
        kg_n_ha
      )
    # P/K via Coello
    if (!is.null(coello_rates) && !is.null(pk_totals)) {
      pk_crop_raw <- crop_areas |>
        filter(!is.na(area_ha), area_ha > 0) |>
        left_join(
          coello_rates |>
            select(
              year,
              area_code,
              crop_name,
              kg_p2o5_ha_coello,
              kg_k2o_ha_coello
            ),
          by = c("year", "area_code", "crop_name")
        ) |>
        filter(!is.na(kg_p2o5_ha_coello) | !is.na(kg_k2o_ha_coello))
      if (nrow(pk_crop_raw) > 0) {
        pk_ref <- pk_totals |> filter(!is.na(mg_nutrient), mg_nutrient > 0)
        pk_p <- pk_crop_raw |>
          filter(!is.na(kg_p2o5_ha_coello)) |>
          mutate(
            mg_raw = kg_p2o5_ha_coello * area_ha / 1000,
            mg_raw_total = sum(mg_raw, na.rm = TRUE),
            .by = c("year", "area_code")
          ) |>
          left_join(
            pk_ref |>
              filter(nutrient == "P") |>
              select(year, area_code, mg_total = mg_nutrient),
            by = c("year", "area_code")
          ) |>
          filter(!is.na(mg_total), mg_raw_total > 0) |>
          mutate(
            scaling = mg_total / mg_raw_total,
            kg_ha = kg_p2o5_ha_coello * scaling,
            mg = kg_ha * area_ha / 1000,
            fert_type = "Synthetic_P2O5",
            land_use = "Cropland"
          ) |>
          select(
            year,
            area_code,
            area_name,
            crop_name,
            land_use,
            fert_type,
            area_ha,
            mg_n = mg,
            kg_n_ha = kg_ha
          )
        pk_k <- pk_crop_raw |>
          filter(!is.na(kg_k2o_ha_coello)) |>
          mutate(
            mg_raw = kg_k2o_ha_coello * area_ha / 1000,
            mg_raw_total = sum(mg_raw, na.rm = TRUE),
            .by = c("year", "area_code")
          ) |>
          left_join(
            pk_ref |>
              filter(nutrient == "K") |>
              select(year, area_code, mg_total = mg_nutrient),
            by = c("year", "area_code")
          ) |>
          filter(!is.na(mg_total), mg_raw_total > 0) |>
          mutate(
            scaling = mg_total / mg_raw_total,
            kg_ha = kg_k2o_ha_coello * scaling,
            mg = kg_ha * area_ha / 1000,
            fert_type = "Synthetic_K2O",
            land_use = "Cropland"
          ) |>
          select(
            year,
            area_code,
            area_name,
            crop_name,
            land_use,
            fert_type,
            area_ha,
            mg_n = mg,
            kg_n_ha = kg_ha
          )
        pk_crop <- bind_rows(pk_p, pk_k)
      }
    }
  }

  # Combine and save
  all_parts <- list(n_crops, nsbnf, pk_crop)
  all_parts <- all_parts[!sapply(all_parts, is.null)]
  if (length(all_parts) > 0) {
    target_year <- max(as.integer(year_range))
    nitrogen_inputs <- bind_rows(all_parts) |>
      .fill_n_inputs_to_target_year(target_year) |>
      arrange(year, area_code, crop_name, fert_type)
    .save_parquet(nitrogen_inputs, output_dir, "nitrogen_inputs")
    cli::cli_alert_success(
      "Nitrogen: {n_distinct(nitrogen_inputs$crop_name)} crops, {n_distinct(nitrogen_inputs$fert_type)} types"
    )
  }
  if (!is.null(pk_totals) && nrow(pk_totals) > 0) {
    .save_parquet(pk_totals, output_dir, "pk_totals")
  }

  cli::cli_alert_success("Nitrogen inputs complete")
  invisible(NULL)
}

# ---- N distribution helper (from prepare_nitrogen_inputs.R section 5) ----
.distribute_n_to_crops <- function(
  n_totals,
  lu_split,
  base_rates,
  crop_areas,
  regions
) {
  euadb <- lu_split$euadb
  lass <- lu_split$lassaletta
  crop_manure <- base_rates$crop_manure
  crop_synthetic <- base_rates$crop_synthetic
  west_rates <- base_rates$west_manure_rates
  es_rates <- base_rates$earthstat_synth_rates
  coello <- base_rates$coello_rates

  # LU split
  n_by_lu <- n_totals |>
    cross_join(tibble(land_use = c("Cropland", "Grassland"))) |>
    left_join(
      euadb |> select(year, area_code, land_use, fert_type, lu_share_eu),
      by = c("year", "area_code", "land_use", "fert_type")
    )

  if (!is.null(lass)) {
    n_by_lu <- n_by_lu |> left_join(lass, by = c("year", "area_code"))
  } else {
    n_by_lu <- n_by_lu |> mutate(grass_share = NA_real_)
  }

  # Manure is already livestock-applied-to-soil only (.read_n_totals_local
  # routes the pasture share into Grassland_excretion) so it must not be
  # re-split here. Synthetic still uses EuroAgriDB/Lassaletta where
  # available; pre-1961 Smil synth falls through to the 100% cropland
  # default (no Lassaletta coverage that far back).
  n_by_lu <- n_by_lu |>
    mutate(
      lu_share = case_when(
        fert_type == "Grassland_excretion" & land_use == "Grassland" ~ 1.0,
        fert_type == "Grassland_excretion" & land_use == "Cropland" ~ 0.0,
        fert_type == "Manure" & land_use == "Cropland" ~ 1.0,
        fert_type == "Manure" & land_use == "Grassland" ~ 0.0,
        !is.na(lu_share_eu) ~ lu_share_eu,
        !is.na(grass_share) & land_use == "Grassland" ~ grass_share,
        !is.na(grass_share) & land_use == "Cropland" ~ 1 - grass_share,
        land_use == "Cropland" ~ 1.0,
        TRUE ~ 0.0
      ),
      mg_n_lu = mg_n * lu_share
    ) |>
    select(year, area_code, area_name, fert_type, land_use, mg_n, mg_n_lu)

  if (is.null(crop_areas) || nrow(crop_areas) == 0) {
    return(n_by_lu)
  }

  cropland_n <- n_by_lu |>
    filter(land_use == "Cropland", fert_type %in% c("Synthetic", "Manure"))
  rate_limit <- 4

  # Layer 1: Base-year rates
  crop_ids <- crop_areas |> distinct(area_code, crop_name)
  base_tbl <- crop_ids |>
    cross_join(tibble(fert_type = c("Synthetic", "Manure")))

  if (!is.null(crop_synthetic)) {
    base_tbl <- base_tbl |>
      left_join(
        crop_synthetic |>
          summarize(
            kg_n_ha_synth = mean(kg_n_ha_synth, na.rm = TRUE),
            .by = c(area_code, crop_name)
          ),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$kg_n_ha_synth <- NA_real_
  }
  if (!is.null(crop_manure)) {
    base_tbl <- base_tbl |>
      left_join(
        crop_manure |> select(area_code, crop_name, manure_mg_n),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$manure_mg_n <- NA_real_
  }
  if (!is.null(west_rates)) {
    base_tbl <- base_tbl |>
      left_join(
        west_rates |> select(area_code, crop_name, kg_n_ha_manure_west),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$kg_n_ha_manure_west <- NA_real_
  }
  if (!is.null(es_rates)) {
    base_tbl <- base_tbl |>
      left_join(
        es_rates |> select(area_code, crop_name, kg_n_ha_synth_es),
        by = c("area_code", "crop_name")
      )
  } else {
    base_tbl$kg_n_ha_synth_es <- NA_real_
  }

  base_year <- 2000L
  base_area <- crop_areas |>
    filter(
      year >= base_year - 2L,
      year <= base_year + 2L,
      !is.na(area_ha),
      area_ha > 0
    ) |>
    summarize(area_ha_base = median(area_ha), .by = c(area_code, crop_name))

  base_tbl <- base_tbl |>
    left_join(base_area, by = c("area_code", "crop_name")) |>
    mutate(
      kg_n_ha_base = case_when(
        fert_type == "Manure" & !is.na(manure_mg_n) & !is.na(area_ha_base) ~
          manure_mg_n * 1000 / area_ha_base,
        fert_type == "Manure" & !is.na(kg_n_ha_manure_west) ~
          kg_n_ha_manure_west,
        fert_type == "Synthetic" & !is.na(kg_n_ha_synth) ~
          kg_n_ha_synth,
        fert_type == "Synthetic" & !is.na(kg_n_ha_synth_es) ~
          kg_n_ha_synth_es,
        TRUE ~ NA_real_
      )
    ) |>
    mutate(
      kg_n_ha_global_crop = median(kg_n_ha_base, na.rm = TRUE),
      .by = c("crop_name", "fert_type")
    ) |>
    mutate(
      kg_n_ha_country_med = median(kg_n_ha_base, na.rm = TRUE),
      .by = c("area_code", "fert_type")
    ) |>
    mutate(
      kg_n_ha_global = median(kg_n_ha_base, na.rm = TRUE),
      .by = "fert_type"
    ) |>
    mutate(
      kg_n_ha_capped = case_when(
        is.na(kg_n_ha_base) ~ NA_real_,
        kg_n_ha_base == 0 ~ kg_n_ha_country_med / rate_limit,
        kg_n_ha_base > kg_n_ha_country_med * rate_limit ~
          kg_n_ha_country_med * rate_limit,
        TRUE ~ kg_n_ha_base
      ),
      kg_n_ha_ref = coalesce(
        kg_n_ha_capped,
        kg_n_ha_country_med,
        kg_n_ha_global_crop,
        kg_n_ha_global
      )
    )

  base_ref <- base_tbl |>
    select(area_code, crop_name, fert_type, kg_n_ha_ref)

  # Layer 2: Coello temporal index
  coello_idx <- NULL
  if (!is.null(coello)) {
    mapping_file <- .find_extdata_file("coello_mapping.csv")
    if (file.exists(mapping_file)) {
      coello_map <- readr::read_csv(mapping_file, show_col_types = FALSE) |>
        select(item_prod_code, coello_crop_code)
      coello_group <- coello |>
        distinct(year, area_code, coello_crop_code, kg_n_ha_coello)
      coello_base <- coello_group |>
        filter(year >= base_year - 2L, year <= base_year + 2L) |>
        summarize(
          kg_n_ha_coello_ref = median(kg_n_ha_coello, na.rm = TRUE),
          .by = c(area_code, coello_crop_code)
        ) |>
        filter(!is.na(kg_n_ha_coello_ref), kg_n_ha_coello_ref > 0)
      coello_idx <- coello_group |>
        inner_join(coello_base, by = c("area_code", "coello_crop_code")) |>
        mutate(coello_temporal_idx = kg_n_ha_coello / kg_n_ha_coello_ref) |>
        select(year, area_code, coello_crop_code, coello_temporal_idx)
      items_ref <- readr::read_csv(
        system.file("extdata", "items_prod.csv", package = "whep"),
        show_col_types = FALSE
      ) |>
        select(item_prod_code, item_prod_name)
      item_group <- coello_map |>
        left_join(items_ref, by = "item_prod_code") |>
        select(crop_name = item_prod_name, coello_crop_code)
    }
  }

  # Combine
  n_crop <- crop_areas |>
    filter(!is.na(area_ha), area_ha > 0) |>
    left_join(
      cropland_n |> select(year, area_code, fert_type, land_use, mg_n_lu),
      by = c("year", "area_code"),
      relationship = "many-to-many"
    ) |>
    filter(!is.na(mg_n_lu)) |>
    left_join(base_ref, by = c("area_code", "crop_name", "fert_type"))

  if (!is.null(coello_idx)) {
    n_crop <- n_crop |>
      left_join(item_group, by = "crop_name") |>
      left_join(coello_idx, by = c("year", "area_code", "coello_crop_code")) |>
      mutate(
        kg_n_ha_trended = kg_n_ha_ref * coalesce(coello_temporal_idx, 1.0)
      ) |>
      select(-coello_crop_code, -coello_temporal_idx)
  } else {
    n_crop <- n_crop |>
      mutate(kg_n_ha_trended = kg_n_ha_ref)
  }

  # Layer 3: Scale to country totals
  n_crop <- n_crop |>
    mutate(
      mg_n_raw = kg_n_ha_trended * area_ha / 1000,
      mg_n_raw_total = sum(mg_n_raw, na.rm = TRUE),
      scaling = if_else(mg_n_raw_total > 0, mg_n_lu / mg_n_raw_total, 0),
      kg_n_ha = kg_n_ha_trended * scaling,
      mg_n_crop = kg_n_ha * area_ha / 1000,
      .by = c("year", "area_code", "fert_type")
    )

  n_crop |>
    select(
      year,
      area_code,
      area_name,
      crop_name,
      land_use,
      fert_type,
      area_ha,
      mg_n = mg_n_crop,
      kg_n_ha
    )
}

# Forward-fill the combined nitrogen_inputs table to a target year so
# the downstream LPJmL NetCDF writer never sees terminal gaps. Currently
# Synthetic / P / K trail behind FAOSTAT's last observed year; this
# extrapolates per (area_code, crop_name, fert_type, land_use) using
# whep::fill_linear on MgN and area_ha. kg_n_ha is re-derived from the
# filled MgN / area pair.
.fill_n_inputs_to_target_year <- function(nitrogen_inputs, target_year) {
  if (nrow(nitrogen_inputs) == 0L) {
    return(nitrogen_inputs)
  }
  key_cols <- c("area_code", "area_name", "crop_name", "land_use", "fert_type")
  year_seq <- seq.int(min(nitrogen_inputs$year), target_year)
  # Carry-forward only: linear extrapolation past the last observed
  # year quickly produces implausible fertilizer levels, and carrying
  # the first observation backward would propagate a much later level
  # into the pre-Haber-Bosch / pre-FAOSTAT period.
  nitrogen_inputs |>
    tidyr::complete(
      year = year_seq,
      tidyr::nesting(!!!rlang::syms(key_cols))
    ) |>
    whep::fill_linear(
      mg_n,
      time_col = year,
      .by = key_cols,
      fill_backward = FALSE
    ) |>
    whep::fill_linear(
      area_ha,
      time_col = year,
      .by = key_cols,
      fill_backward = FALSE
    ) |>
    dplyr::filter(!is.na(mg_n), !is.na(area_ha), area_ha > 0) |>
    dplyr::mutate(kg_n_ha = mg_n * 1000 / area_ha) |>
    dplyr::select(
      year,
      area_code,
      area_name,
      crop_name,
      land_use,
      fert_type,
      area_ha,
      mg_n,
      kg_n_ha
    )
}


# ==== Section 4b: Multi-cropping factor ====================================
#
# Purpose
# -------
# Disaggregating FAOSTAT country-level harvested area to a 0.5 degree
# grid runs into the multi-cropping problem: in regions where land is
# cropped more than once a year (rice double-cropping, cereal-pulse
# rotations, etc.), country harvested area exceeds country physical
# cropland. The spatializer's capacity-constraint path
# (.apply_capacity_constraint() in R/spatialize.R) re-allocates excess
# area between cells within a country, but it needs an explicit budget
# per cell: how much harvested area each cell can hold, expressed as
# a multiple of its physical cropland area. This function builds that
# budget for every cell and every year of the 1850-2022 series.
#
# Method
# ------
# The per-hectare budget combines two distinct phenomena:
#
#   budget_per_ha(country, year) =
#       (1 - fallow_share)         # share of cropland actually cropped this year
#     * cropping_intensity         # cycles per cropped hectare, always >= 1
#
# We currently do not have an independent fallow_share series we trust
# for the full 1850-2022 range, so the two phenomena stay fused in a
# single country-year factor equal to their product:
#
#   mc_factor(country, year) =
#     sum_{herbaceous items} harvested_area_ha       (from country_areas)
#     ---------------------------------------------
#     sum_{herbaceous LUH2 types} physical_ha       (from type_cropland)
#
# When mc_factor > 1, multi-cropping dominates (e.g. monsoon Asia
# rice double-cropping). When mc_factor < 1, fallow dominates
# (e.g. Sahel rotational fallow, US set-aside, dryland biennial wheat).
# When mc_factor = 1, either nothing is happening or the two cancel.
# Pre-FAOSTAT extension years (1850-1960) give mc ~ 1.0 because the
# LUH2-driven country_areas extension encodes neither phenomenon.
#
# "Herbaceous" = LUH2 types c3ann + c4ann + c3nfx (annuals + N-fixing
# annuals). Woody / perennial cropland (c3per, c4per) is excluded from
# both numerator and denominator because multi-cropping is an
# annual-crop phenomenon; perennials occupy land continuously.
#
# Bounds: capped at 3.0 (the documented maximum cropping intensity in
# MIRCA2000 and GAEZ -- triple cropping). No lower bound: fallow
# countries legitimately have mc < 1, and the spatializer's logit
# redistribution handles the resulting tighter per-cell cap correctly.
#
# Upgrade path
# ------------
# When a defensible fallow_share(country, year) series becomes
# available, prepare_multicropping should be split into two outputs:
# fallow_share and cropping_intensity. The product remains
# mc_factor, so the spatializer's cap math does not change; the
# value is that the two phenomena become individually inspectable
# (visualisation, separate spatial fallow modelling, sensitivity
# analysis). Plumbing such an input through this function would mean
# adding a fallow_share argument; the function would compute
# cropping_intensity = harvested / (physical * (1 - fallow_share))
# and the parquet would gain explicit fallow_share + cropping_intensity
# columns alongside mc_rainfed and mc_irrigated.
#
# Spatial expansion
# -----------------
# The country-year factor is broadcast uniformly to every cell of the
# country for that year (mc_rainfed = mc_irrigated = mc_factor). The
# spatializer's logit redistribution (.apply_capacity_constraint and
# its .redistribute_* helpers) then handles within-country reallocation
# when the crop-pattern-derived initial allocation puts more harvested
# area in a cell than the country budget allows.
#
# Inputs and outputs
# ------------------
# Reads country_areas.parquet, type_cropland.parquet, country_grid.parquet,
# and gridded_cropland.parquet from output_dir (all produced by earlier
# sections of this pipeline). Writes inputs/multicropping.parquet with
# columns lon, lat, year, mc_rainfed, mc_irrigated.

prepare_multicropping <- function(l_files_dir, output_dir) {
  cli::cli_h2("Section 4b: Multi-cropping factor")

  cft_mapping <- readr::read_csv(
    .find_extdata_file("cft_mapping.csv"),
    show_col_types = FALSE
  )
  herb_types <- c("c3ann", "c4ann", "c3nfx")
  herb_items <- cft_mapping |>
    dplyr::filter(luh2_type %in% herb_types) |>
    dplyr::pull(item_prod_code)
  cli::cli_alert_info(
    "{length(herb_items)} herbaceous items \\
    ({length(herb_types)} LUH2 types)"
  )

  ca <- nanoparquet::read_parquet(
    file.path(output_dir, "country_areas.parquet")
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(item_prod_code %in% herb_items)

  num <- ca |>
    dplyr::summarise(
      harvested_ha = sum(harvested_area_ha, na.rm = TRUE),
      .by = c(year, area_code)
    )

  tc <- nanoparquet::read_parquet(
    file.path(output_dir, "type_cropland.parquet")
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(luh2_type %in% herb_types)

  cg <- nanoparquet::read_parquet(
    file.path(output_dir, "country_grid.parquet")
  ) |>
    tibble::as_tibble()

  den <- tc |>
    dplyr::inner_join(cg, by = c("lon", "lat")) |>
    dplyr::summarise(
      physical_ha = sum(type_ha, na.rm = TRUE),
      .by = c(year, area_code)
    )

  mc_country <- num |>
    dplyr::inner_join(den, by = c("year", "area_code")) |>
    dplyr::mutate(
      mc_factor = dplyr::case_when(
        physical_ha < 1 ~ 1.0,
        TRUE ~ pmin(harvested_ha / physical_ha, 3.0)
      )
    )

  gc <- nanoparquet::read_parquet(
    file.path(output_dir, "gridded_cropland.parquet")
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(cropland_ha > 0) |>
    dplyr::select(lon, lat, year, cropland_ha)

  mc_cell <- gc |>
    dplyr::inner_join(cg, by = c("lon", "lat")) |>
    dplyr::inner_join(
      dplyr::select(mc_country, year, area_code, mc_factor),
      by = c("year", "area_code")
    ) |>
    dplyr::mutate(
      mc_rainfed = mc_factor,
      mc_irrigated = mc_factor
    ) |>
    dplyr::select(lon, lat, year, mc_rainfed, mc_irrigated)

  .save_parquet(mc_cell, output_dir, "multicropping")

  invisible(mc_cell)
}


# ==== Section 8: Livestock inputs ==========================================
#
# Livestock stocks, emissions, gridded pasture, manure pattern. The
# per-animal nitrogen-excretion rate (nex_kg_n_head) used to derive
# country-level manure-N (manure_n_mg) is a separate WHEP output, see
# .compute_default_n_excretion() below.

# --- N excretion (default = IPCC 2019 Tier 1, regional for cattle) --------
#
# Purpose
# -------
# Country x year x species_group N excretion rate (kg N per head per year)
# used to translate livestock heads into manure-N flows. This is the
# Tier 1 default; downstream modules (notably .calc_n_excretion() in
# R/livestock_manure.R, which derives N excretion from gross energy
# and dietary crude protein) can provide a more sophisticated Tier 2
# series. To override the default, pass a tibble with columns
# (year, area_code, species_group, nex_kg_n_head) as the nex_source
# argument of prepare_livestock_inputs().
#
# Method
# ------
# Looks up each species_group in the IPCC 2019 Refinement Vol 4 Ch 10
# Table 10.19 (whep::ipcc_2019_n_excretion):
#   - cattle_dairy   uses regional 'Dairy Cattle' values via the
#     country -> region_krausmann -> IPCC NEx region mapping below.
#   - cattle_non_dairy uses regional 'Other Cattle' values via the
#     same mapping.
#   - All other species_groups use Global values from the same table
#     because the IPCC table itself only provides regional values for
#     the two cattle categories.
#   - species_group 'other' (rabbits, rodents, animals nes) has no
#     IPCC entry and falls back to a small constant 2 kg N/head/year.
#
# The Krausmann -> IPCC region table below covers all 11 Krausmann
# regions; countries with no Krausmann classification fall back to
# Global. Output is materialised to L_files/whep/outputs/n_excretion.parquet
# so it can be inspected and reused outside this prep step.

.species_to_ipcc_2019_category <- function() {
  tibble::tribble(
    ~species_group,      ~ipcc_category,        ~use_regional,
    "cattle_dairy",      "Dairy Cattle",        TRUE,
    "cattle_non_dairy",  "Other Cattle",        TRUE,
    "buffalo",           "Buffalo",             FALSE,
    "sheep_goats",       "Sheep",               FALSE,
    "pigs",              "Swine - Market",      FALSE,
    "chickens_layers",   "Poultry - Layers",    FALSE,
    "chickens_broilers", "Poultry - Broilers",  FALSE,
    "poultry",           "Poultry - Broilers",  FALSE,
    "equines",           "Horses",              FALSE,
    "camels",            "Camels",              FALSE,
    "other",             NA_character_,         FALSE
  )
}

.krausmann_to_ipcc_nex_region <- function() {
  tibble::tribble(
    ~region_krausmann,                       ~ipcc_region,
    "Australia and Oceania",                 "Oceania",
    "Central Asia and Russian Federation",   "Eastern Europe",
    "Eastern and South Eastern Europe",      "Eastern Europe",
    "Eastern Asia",                          "Asia",
    "Latin America and the Caribbean",       "Latin America",
    "Northern Africa and Western Asia",      "Middle East",
    "Northern America",                      "North America",
    "Southeastern Asia",                     "Asia",
    "Southern Asia",                         "Indian Subcontinent",
    "Subsaharan Africa",                     "Africa",
    "Western Europe",                        "Western Europe"
  )
}

.compute_default_n_excretion <- function(keys) {
  species_to_ipcc <- .species_to_ipcc_2019_category()
  krausmann_to_ipcc <- .krausmann_to_ipcc_nex_region()

  area_to_region <- whep::regions_full |>
    dplyr::select(area_code = code, region_krausmann) |>
    dplyr::distinct() |>
    dplyr::left_join(krausmann_to_ipcc, by = "region_krausmann") |>
    dplyr::mutate(
      ipcc_region = dplyr::coalesce(ipcc_region, "Global")
    ) |>
    dplyr::select(area_code, ipcc_region)

  nex_lookup <- whep::ipcc_2019_n_excretion |>
    dplyr::rename(
      ipcc_region_lkp = region,
      ipcc_category = category,
      nex_lookup = nex_kg_n_head_yr
    )

  result <- keys |>
    dplyr::left_join(species_to_ipcc, by = "species_group") |>
    dplyr::left_join(area_to_region, by = "area_code") |>
    dplyr::mutate(
      ipcc_region_lkp = dplyr::if_else(
        use_regional & !is.na(ipcc_region),
        ipcc_region,
        "Global"
      )
    ) |>
    dplyr::left_join(
      nex_lookup,
      by = c("ipcc_region_lkp", "ipcc_category")
    ) |>
    dplyr::mutate(
      source = dplyr::case_when(
        !is.na(nex_lookup) & use_regional ~
          "ipcc_2019_tier1_regional",
        !is.na(nex_lookup) ~ "ipcc_2019_tier1_global",
        TRUE ~ "fallback_other_2kg"
      ),
      nex_kg_n_head = dplyr::coalesce(nex_lookup, 2.0)
    ) |>
    dplyr::select(year, area_code, species_group, nex_kg_n_head, source)

  result
}

.resolve_n_excretion <- function(nex_source, keys) {
  if (is.null(nex_source)) {
    return(.compute_default_n_excretion(keys))
  }
  required <- c("year", "area_code", "species_group", "nex_kg_n_head")
  missing <- setdiff(required, names(nex_source))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.arg nex_source} is missing required column{?s}:",
      "x" = "{.val {missing}}."
    ))
  }
  out <- nex_source
  if (!"source" %in% names(out)) {
    out <- dplyr::mutate(out, source = "user_supplied")
  }
  out |>
    dplyr::select(year, area_code, species_group, nex_kg_n_head, source)
}


prepare_livestock_inputs <- function(
  l_files_dir,
  output_dir,
  year_range,
  target_res,
  prod = NULL,
  nex_source = NULL
) {
  cli::cli_h2("Section 8: Livestock inputs")

  outputs_dir <- file.path(l_files_dir, "whep", "outputs")
  if (!dir.exists(outputs_dir)) {
    dir.create(outputs_dir, recursive = TRUE)
  }

  mapping_path <- .find_extdata_file("livestock_mapping.csv")
  livestock_mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)

  # --- Stocks from build_primary_production ---
  if (is.null(prod)) {
    prod <- .load_or_cache_production(output_dir, year_range)
  }

  stocks <- prod |>
    filter(
      .data$unit == "heads",
      as.integer(.data$item_prod_code) %in% livestock_mapping$item_code
    ) |>
    transmute(
      area_code = as.integer(.data$area_code),
      item_code = as.integer(.data$item_prod_code),
      year = as.integer(.data$year),
      heads = .data$value
    ) |>
    filter(!is.na(.data$area_code), .data$heads > 0) |>
    inner_join(
      select(livestock_mapping, item_code, species_group),
      by = "item_code"
    )

  stocks_grouped <- stocks |>
    summarise(
      heads = sum(heads, na.rm = TRUE),
      .by = c(year, area_code, species_group)
    )

  # --- N excretion (separate WHEP output, materialised to outputs/) ---
  nex_tbl <- .resolve_n_excretion(
    nex_source = nex_source,
    keys = dplyr::distinct(stocks_grouped, year, area_code, species_group)
  )
  .save_parquet(nex_tbl, outputs_dir, "n_excretion")

  stocks_grouped <- stocks_grouped |>
    dplyr::left_join(
      dplyr::select(nex_tbl, year, area_code, species_group, nex_kg_n_head),
      by = c("year", "area_code", "species_group")
    )

  # --- Emissions from pin ---
  emi_raw <- tryCatch(
    {
      dt <- whep:::.read_input("faostat-emissions-livestock")
      tibble::as_tibble(dt)
    },
    error = function(e) NULL
  )

  emissions <- NULL
  if (!is.null(emi_raw)) {
    emi_species_map <- tribble(
      ~emi_item, ~species_group,
      "Cattle, dairy", "cattle_dairy",
      "Cattle, non-dairy", "cattle_non_dairy",
      "Buffalo", "buffalo",
      "Sheep", "sheep_goats",
      "Goats", "sheep_goats",
      "Swine, market", "pigs",
      "Swine, breeding", "pigs",
      "Chickens, layers", "chickens_layers",
      "Chickens, broilers", "chickens_broilers",
      "Ducks", "poultry",
      "Turkeys", "poultry",
      "Horses", "equines",
      "Asses", "equines",
      "Mules and hinnies", "equines",
      "Mules and Asses", "equines",
      "Camels", "camels",
      "Camels and Llamas", "camels"
    )

    enteric_ch4 <- emi_raw |>
      filter(Element == "Enteric fermentation (Emissions CH4)") |>
      transmute(
        area_code = as.integer(`Area Code`),
        year = as.integer(Year),
        emi_item = Item,
        enteric_ch4_kt = Value
      ) |>
      filter(!is.na(area_code), area_code < 5000L) |>
      inner_join(emi_species_map, by = "emi_item") |>
      summarise(
        enteric_ch4_kt = sum(enteric_ch4_kt, na.rm = TRUE),
        .by = c(year, area_code, species_group)
      )

    manure_ch4 <- emi_raw |>
      filter(Element == "Manure management (Emissions CH4)") |>
      transmute(
        area_code = as.integer(`Area Code`),
        year = as.integer(Year),
        emi_item = Item,
        manure_ch4_kt = Value
      ) |>
      filter(!is.na(area_code), area_code < 5000L) |>
      inner_join(emi_species_map, by = "emi_item") |>
      summarise(
        manure_ch4_kt = sum(manure_ch4_kt, na.rm = TRUE),
        .by = c(year, area_code, species_group)
      )

    manure_n2o <- emi_raw |>
      filter(Element == "Manure management (Emissions N2O)") |>
      transmute(
        area_code = as.integer(`Area Code`),
        year = as.integer(Year),
        emi_item = Item,
        manure_n2o_kt = Value
      ) |>
      filter(!is.na(area_code), area_code < 5000L) |>
      inner_join(emi_species_map, by = "emi_item") |>
      summarise(
        manure_n2o_kt = sum(manure_n2o_kt, na.rm = TRUE),
        .by = c(year, area_code, species_group)
      )

    emissions <- enteric_ch4 |>
      full_join(manure_ch4, by = c("year", "area_code", "species_group")) |>
      full_join(manure_n2o, by = c("year", "area_code", "species_group")) |>
      mutate(
        enteric_ch4_kt = if_else(is.na(enteric_ch4_kt), 0, enteric_ch4_kt),
        manure_ch4_kt = if_else(is.na(manure_ch4_kt), 0, manure_ch4_kt),
        manure_n2o_kt = if_else(is.na(manure_n2o_kt), 0, manure_n2o_kt)
      )
  }

  # --- Merge stocks + emissions ---
  livestock_country <- stocks_grouped |>
    mutate(manure_n_mg = heads * nex_kg_n_head / 1000)

  if (!is.null(emissions)) {
    livestock_country <- livestock_country |>
      left_join(emissions, by = c("year", "area_code", "species_group")) |>
      mutate(
        enteric_ch4_kt = if_else(is.na(enteric_ch4_kt), 0, enteric_ch4_kt),
        manure_ch4_kt = if_else(is.na(manure_ch4_kt), 0, manure_ch4_kt),
        manure_n2o_kt = if_else(is.na(manure_n2o_kt), 0, manure_n2o_kt)
      )
  } else {
    livestock_country <- livestock_country |>
      mutate(enteric_ch4_kt = 0, manure_ch4_kt = 0, manure_n2o_kt = 0)
  }

  livestock_out <- livestock_country |>
    select(
      year,
      area_code,
      species_group,
      heads,
      enteric_ch4_kt,
      manure_ch4_kt,
      manure_n2o_kt,
      manure_n_mg
    )
  .save_parquet(livestock_out, output_dir, "livestock_country_data")

  # --- Gridded pasture from LUH2 ---
  cli::cli_alert_info("Preparing gridded pasture (LUH2)")
  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  states_path <- file.path(luh2_dir, "states.nc")
  carea_ha <- .read_luh2_carea(luh2_dir) * 100
  agg_factor <- as.integer(target_res / 0.25)

  pasture_years <- sort(intersect(unique(livestock_country$year), year_range))
  n_workers <- .auto_spatialize_workers(
    length(pasture_years),
    worker_mb_default = 9000
  )
  cli::cli_alert_info("LUH2 pasture workers: {n_workers}")
  per_year_pasture <- function(yr) {
    time_idx <- yr - 850L + 1L
    pastr_r <- .read_luh2_variable(states_path, "pastr", time_idx)
    range_r <- .read_luh2_variable(states_path, "range", time_idx)
    pastr_ha <- terra::aggregate(
      pastr_r * carea_ha,
      fact = agg_factor,
      fun = "sum",
      na.rm = TRUE
    )
    range_ha <- terra::aggregate(
      range_r * carea_ha,
      fact = agg_factor,
      fun = "sum",
      na.rm = TRUE
    )
    p_tbl <- .raster_to_tibble(pastr_ha, "pasture_ha")
    r_tbl <- .raster_to_tibble(range_ha, "rangeland_ha")
    left_join(p_tbl, r_tbl, by = c("lon", "lat")) |>
      mutate(
        pasture_ha = if_else(is.na(pasture_ha), 0, pasture_ha),
        rangeland_ha = if_else(is.na(rangeland_ha), 0, rangeland_ha)
      ) |>
      filter(pasture_ha > 0 | rangeland_ha > 0) |>
      mutate(year = yr)
  }
  pasture_list <- if (.Platform$OS.type == "windows") {
    lapply(pasture_years, per_year_pasture)
  } else {
    parallel::mclapply(pasture_years, per_year_pasture, mc.cores = n_workers)
  }

  gridded_pasture <- bind_rows(pasture_list)
  .save_parquet(gridded_pasture, output_dir, "gridded_pasture")

  # --- Manure pattern (West et al. 2014) ---
  cli::cli_alert_info("Preparing manure intensity pattern (West et al. 2014)")
  west_dir <- file.path(l_files_dir, "Manure_Westetal2014", "N")
  if (dir.exists(west_dir)) {
    nc_files <- list.files(west_dir, pattern = "\\.nc$", full.names = TRUE)
    manure_total <- NULL
    for (nc_path in nc_files) {
      r <- terra::rast(nc_path)
      agg_f <- round(target_res / terra::res(r)[1])
      if (agg_f > 1) {
        r <- terra::aggregate(r, fact = agg_f, fun = "mean", na.rm = TRUE)
      }
      if (is.null(manure_total)) {
        manure_total <- r
      } else {
        if (terra::compareGeom(manure_total, r, stopOnError = FALSE)) {
          manure_total <- manure_total + r
        } else {
          r_aligned <- terra::resample(r, manure_total, method = "bilinear")
          manure_total <- manure_total + r_aligned
        }
      }
    }
    manure_tbl <- .raster_to_tibble(manure_total, "manure_intensity") |>
      filter(!is.na(manure_intensity), manure_intensity > 0)
    max_intensity <- max(manure_tbl$manure_intensity, na.rm = TRUE)
    if (max_intensity > 0) {
      manure_tbl <- manure_tbl |>
        mutate(manure_intensity = manure_intensity / max_intensity)
    }
    .save_parquet(manure_tbl, output_dir, "manure_pattern")
  } else {
    cli::cli_alert_warning(
      "West et al. 2014 not found at {west_dir} -- skipping manure pattern"
    )
  }

  cli::cli_alert_success("Livestock inputs complete")
  invisible(livestock_out)
}


# ==== Section 9: Hydrology + Soil =========================================

prepare_hydrology_inputs <- function(l_files_dir, output_dir, lpjml_out_dir) {
  cli::cli_h2("Section 9a: Hydrology inputs")

  country_grid <- nanoparquet::read_parquet(
    file.path(output_dir, "country_grid.parquet")
  )
  grid <- make_target_grid()
  grand_dir <- file.path(l_files_dir, "GIS", "Global dams")
  glwd_dir <- file.path(l_files_dir, "GLWD")
  drainage_paths <- c(
    file.path(l_files_dir, "DRT", "DRT_half_FDR_globe.asc"),
    file.path(l_files_dir, "DDM30", "ddm30.asc")
  )

  # ---- Elevation ----
  .prepare_elevation <- function(country_grid, l_files_dir, output_dir) {
    cli::cli_alert_info("Elevation")
    elev_path <- file.path(
      l_files_dir,
      "WorldClim",
      "elevation",
      "wc2.1_10m_elev.tif"
    )
    if (!file.exists(elev_path)) {
      cli::cli_abort(
        "Elevation raster not found at {elev_path}. Run download_all() first."
      )
    }
    elev_10m <- terra::rast(elev_path)
    elev_30m <- terra::aggregate(
      elev_10m,
      fact = 3,
      fun = "median",
      na.rm = TRUE
    )
    grid_coords <- as.matrix(country_grid[, c("lon", "lat")])
    cell_ids <- terra::cellFromXY(elev_30m, grid_coords)
    elev_vals <- terra::values(elev_30m)[cell_ids]
    n_na <- sum(is.na(elev_vals))
    if (n_na > 0) {
      na_idx <- which(is.na(elev_vals))
      for (i in na_idx) {
        lon_i <- grid_coords[i, 1]
        lat_i <- grid_coords[i, 2]
        for (radius in seq_len(50)) {
          nearby <- which(
            abs(grid_coords[, 1] - lon_i) <= radius * 0.5 &
              abs(grid_coords[, 2] - lat_i) <= radius * 0.5 &
              !is.na(elev_vals)
          )
          if (length(nearby) > 0) {
            dists <- sqrt(
              (grid_coords[nearby, 1] - lon_i)^2 +
                (grid_coords[nearby, 2] - lat_i)^2
            )
            weights <- 1 / pmax(dists, 0.01)
            elev_vals[i] <- round(
              stats::weighted.mean(elev_vals[nearby], weights)
            )
            break
          }
        }
      }
    }
    result <- country_grid |>
      select(lon, lat) |>
      mutate(elevation_m = pmax(as.integer(round(elev_vals)), 0L))
    .save_parquet(result, output_dir, "elevation")
  }

  # ---- Reservoirs ----
  .prepare_reservoirs <- function(country_grid, grand_dir, output_dir) {
    cli::cli_alert_info("Reservoirs (GRanD)")
    shp_path <- file.path(grand_dir, "GRanD_dams_v1_1.shp")
    if (!file.exists(shp_path)) {
      cli::cli_alert_warning("GRanD not found -- skipping reservoirs")
      return(invisible(NULL))
    }
    dams_v <- terra::vect(shp_path)
    dams <- as.data.frame(dams_v) |>
      tibble::as_tibble() |>
      select(
        grand_id = GRAND_ID,
        dam_name = DAM_NAME,
        year = YEAR,
        cap_mcm = CAP_MCM,
        area_skm = AREA_SKM,
        catch_skm = CATCH_SKM,
        main_use = MAIN_USE,
        lon_dam = LONG_DD,
        lat_dam = LAT_DD
      ) |>
      mutate(
        year = if_else(year < 0, NA_integer_, as.integer(year)),
        cap_mcm = if_else(cap_mcm < 0, NA_real_, cap_mcm),
        area_skm = if_else(area_skm < 0, NA_real_, area_skm),
        catch_skm = if_else(catch_skm < 0, NA_real_, catch_skm),
        lon = round(floor(lon_dam / 0.5) * 0.5 + 0.25, 2),
        lat = round(floor(lat_dam / 0.5) * 0.5 + 0.25, 2),
        purpose_code = if_else(main_use == "Irrigation", 1L, 2L)
      )
    cell_reservoirs <- dams |>
      filter(!is.na(cap_mcm), cap_mcm > 0) |>
      summarise(
        n_dams = n(),
        total_cap_mcm = sum(cap_mcm, na.rm = TRUE),
        total_area_skm = sum(area_skm, na.rm = TRUE),
        min_year = suppressWarnings(min(year, na.rm = TRUE)),
        mean_catch_skm = mean(catch_skm, na.rm = TRUE),
        purpose_code = purpose_code[which.max(cap_mcm)],
        .by = c(lon, lat)
      ) |>
      mutate(min_year = if_else(is.finite(min_year), min_year, NA_integer_))
    result <- country_grid |>
      select(lon, lat) |>
      left_join(cell_reservoirs, by = c("lon", "lat")) |>
      mutate(
        across(
          c(
            n_dams,
            total_cap_mcm,
            total_area_skm,
            min_year,
            mean_catch_skm,
            purpose_code
          ),
          ~ replace(.x, is.na(.x), 0)
        ),
        n_dams = as.integer(n_dams),
        purpose_code = as.integer(purpose_code)
      )
    .save_parquet(result, output_dir, "reservoirs")
  }

  # ---- Lakes & Rivers ----
  .prepare_lakes_rivers <- function(
    country_grid,
    glwd_dir,
    lpjml_out_dir,
    grid
  ) {
    cli::cli_alert_info("Lakes & Rivers (GLWD)")
    glwd_path <- NULL
    glwd_version <- NULL
    v2_dir <- file.path(glwd_dir, "GLWD_v2")
    .find_v2_tif <- function(pattern) {
      list.files(v2_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
    }
    v2_tifs <- Filter(
      Negate(function(p) grepl("50pct", p)),
      c(
        .find_v2_tif("main_class.*\\.tif$"),
        .find_v2_tif("dominant.*\\.tif$"),
        .find_v2_tif("combined.*\\.tif$")
      )
    )
    if (length(v2_tifs) > 0) {
      glwd_path <- v2_tifs[1]
      glwd_version <- "v2"
    }
    if (is.null(glwd_path)) {
      glwd3_path <- file.path(glwd_dir, "glwd_3", "hdr.adf")
      if (!file.exists(glwd3_path)) {
        glwd3_path <- file.path(glwd_dir, "glwd_3.tif")
      }
      if (file.exists(glwd3_path)) {
        glwd_path <- glwd3_path
        glwd_version <- "v1"
      }
    }
    if (is.null(glwd_path)) {
      cli::cli_alert_warning("GLWD data not found -- skipping")
      return(invisible(NULL))
    }
    glwd <- terra::rast(glwd_path)
    src_res <- terra::res(glwd)
    agg_factor <- round(0.5 / src_res[1])
    if (glwd_version == "v2") {
      lake_classes <- c(1, 2, 3)
      river_classes <- 7
    } else {
      lake_classes <- 1
      river_classes <- 3
    }
    lake_rcl <- cbind(lake_classes, rep(1, length(lake_classes)))
    lake_mask <- terra::classify(glwd, lake_rcl, others = NA)
    lake_frac <- terra::aggregate(
      lake_mask,
      fact = agg_factor,
      fun = "mean",
      na.rm = TRUE
    )
    river_rcl <- cbind(river_classes, rep(1, length(river_classes)))
    river_mask <- terra::classify(glwd, river_rcl, others = NA)
    river_frac <- terra::aggregate(
      river_mask,
      fact = agg_factor,
      fun = "mean",
      na.rm = TRUE
    )
    grid_coords <- as.matrix(country_grid[, c("lon", "lat")])
    lake_vals <- terra::values(lake_frac)[
      terra::cellFromXY(lake_frac, grid_coords)
    ]
    river_vals <- terra::values(river_frac)[
      terra::cellFromXY(river_frac, grid_coords)
    ]
    lr_tbl <- country_grid |>
      select(lon, lat) |>
      mutate(
        lake_fraction = round(replace(lake_vals, is.na(lake_vals), 0), 6),
        river_fraction = round(replace(river_vals, is.na(river_vals), 0), 6)
      )
    lr_dt <- coord_to_rowcol(data.table::as.data.table(lr_tbl), grid)
    m_lakes <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_lakes[cbind(lr_dt$col, lr_dt$row)] <- pmin(
      1,
      pmax(0, lr_dt$lake_fraction + lr_dt$river_fraction)
    )
    dir.create(
      file.path(lpjml_out_dir, "lakes_rivers"),
      recursive = TRUE,
      showWarnings = FALSE
    )
    write_nc_2d(
      file.path(
        lpjml_out_dir,
        "lakes_rivers",
        "glwd_lakes_and_rivers_30arcmin.nc"
      ),
      "lakes",
      "Lake and river fraction",
      "1",
      m_lakes,
      grid$lon,
      grid$lat,
      prec = "float"
    )
  }

  # ---- Drainage ----
  .prepare_drainage <- function(country_grid, drainage_paths) {
    cli::cli_alert_info("Drainage Routing")
    drainage_path <- NULL
    drainage_type <- NULL
    for (p in drainage_paths) {
      if (file.exists(p)) {
        drainage_path <- p
        drainage_type <- if (grepl("DRT|drt", p, ignore.case = TRUE)) {
          "DRT"
        } else {
          "DDM30"
        }
        break
      }
    }
    if (is.null(drainage_path)) {
      cli::cli_alert_warning("No drainage direction file found -- skipping")
      return(invisible(NULL))
    }
    ddm <- terra::rast(drainage_path)
    grid_coords <- as.matrix(country_grid[, c("lon", "lat")])
    cell_ids <- terra::cellFromXY(ddm, grid_coords)
    flow_dir_vals <- terra::values(ddm)[cell_ids]
    if (drainage_type == "DRT") {
      esri_to_ddm <- c(
        `1` = 1L,
        `2` = 2L,
        `4` = 3L,
        `8` = 4L,
        `16` = 5L,
        `32` = 6L,
        `64` = 7L,
        `128` = 8L
      )
      flow_dir_vals <- esri_to_ddm[as.character(as.integer(flow_dir_vals))]
      flow_dir_vals[is.na(flow_dir_vals)] <- 0L
    }
    result <- country_grid |>
      select(lon, lat) |>
      mutate(
        flow_direction = as.integer(
          replace(flow_dir_vals, is.na(flow_dir_vals), 0L)
        )
      )
    result
  }

  .prepare_elevation(country_grid, l_files_dir, output_dir)
  .prepare_reservoirs(country_grid, grand_dir, output_dir)
  .prepare_lakes_rivers(country_grid, glwd_dir, lpjml_out_dir, grid)
  drainage_dt <- .prepare_drainage(country_grid, drainage_paths)

  cli::cli_alert_success("Hydrology inputs complete")
  invisible(drainage_dt)
}

prepare_soil_inputs <- function(
  l_files_dir,
  output_dir,
  lpjml_out_dir,
  target_res = 0.5
) {
  cli::cli_h2("Section 9b: Soil inputs (HWSD)")

  hwsd_dir <- file.path(l_files_dir, "HWSD")

  .read_hwsd_attributes_local <- function(hwsd_dir) {
    csv_path <- file.path(hwsd_dir, "hwsd_data.csv")
    if (!file.exists(csv_path)) {
      cli::cli_abort("HWSD CSV not found at {csv_path}")
    }
    readr::read_csv(csv_path, show_col_types = FALSE)
  }

  # HWSD v2 USDA texture codes → LPJmL soil type codes (1-13)
  # HWSD2: 1=Clay(heavy), 2=Silty clay, 3=Clay(light), 4=Silty clay loam,
  #   5=Clay loam, 6=Silt, 7=Silt loam, 8=Sandy clay, 9=Loam,
  #   10=Sandy clay loam, 11=Sandy loam, 12=Loamy sand, 13=Sand
  # LPJmL: 1=clay, 2=silty clay, 3=sandy clay, 4=clay loam,
  #   5=silty clay loam, 6=sandy clay loam, 7=loam, 8=silt loam,
  #   9=sandy loam, 10=silt, 11=loamy sand, 12=sand, 13=rock and ice
  .hwsd2_to_lpjml_tex <- c(
    "1" = 1L, # Clay(heavy) → clay
    "2" = 2L, # Silty clay
    "3" = 1L, # Clay(light) → clay
    "4" = 5L, # Silty clay loam
    "5" = 4L, # Clay loam
    "6" = 10L, # Silt
    "7" = 8L, # Silt loam
    "8" = 3L, # Sandy clay
    "9" = 7L, # Loam
    "10" = 6L, # Sandy clay loam
    "11" = 9L, # Sandy loam
    "12" = 11L, # Loamy sand
    "13" = 12L # Sand
  )

  .derive_dominant_soil <- function(hwsd_attr) {
    hwsd_attr <- hwsd_attr |>
      mutate(
        t_usda_tex_class = .hwsd2_to_lpjml_tex[as.character(t_usda_tex)]
      )
    soils <- hwsd_attr |> filter(!is.na(t_usda_tex_class))
    dom_tex <- soils |>
      summarise(
        tex_share = sum(share, na.rm = TRUE),
        .by = c(mu_global, t_usda_tex_class)
      ) |>
      slice_max(tex_share, n = 1, with_ties = FALSE, by = mu_global) |>
      select(mu_global, t_usda_tex_class)
    ph_data <- soils |>
      inner_join(dom_tex, by = c("mu_global", "t_usda_tex_class")) |>
      slice_max(share, n = 1, with_ties = FALSE, by = mu_global) |>
      select(mu_global, t_ph_h2o)
    dom_tex |>
      left_join(ph_data, by = "mu_global") |>
      mutate(t_ph_h2o = if_else(is.na(t_ph_h2o), 7.0, t_ph_h2o))
  }

  .aggregate_hwsd <- function(hwsd_dir, mu_soils, target_res) {
    hwsd_path <- file.path(hwsd_dir, "hwsd.bil")
    if (!file.exists(hwsd_path)) {
      cli::cli_abort("HWSD raster not found at {hwsd_path}")
    }
    hwsd_rast <- terra::rast(hwsd_path)
    agg_factor <- as.integer(target_res / terra::res(hwsd_rast)[1])

    # Reclassify mu_global IDs -> texture code and pH directly on the raster,
    # then aggregate spatially. Both ops run in terra's C++ layer.
    rcl_tex <- as.matrix(mu_soils[, c("mu_global", "t_usda_tex_class")])
    rcl_ph <- as.matrix(mu_soils[, c("mu_global", "t_ph_h2o")])

    tex_rast <- terra::classify(hwsd_rast, rcl_tex, others = NA)
    ph_rast <- terra::classify(hwsd_rast, rcl_ph, others = NA)

    tex_coarse <- terra::aggregate(
      tex_rast,
      fact = agg_factor,
      fun = "modal",
      na.rm = TRUE
    )
    ph_coarse <- terra::aggregate(
      ph_rast,
      fact = agg_factor,
      fun = "mean",
      na.rm = TRUE
    )

    tex_df <- terra::as.data.frame(tex_coarse, xy = TRUE, na.rm = TRUE)
    ph_df <- terra::as.data.frame(ph_coarse, xy = TRUE, na.rm = TRUE)
    names(tex_df) <- c("lon", "lat", "soil_texture_code")
    names(ph_df) <- c("lon", "lat", "soil_ph")

    dplyr::inner_join(
      tibble::as_tibble(tex_df),
      tibble::as_tibble(ph_df),
      by = c("lon", "lat")
    ) |>
      dplyr::mutate(
        lon = round(lon, 2),
        lat = round(lat, 2),
        soil_texture_code = as.integer(soil_texture_code),
        soil_ph = round(soil_ph, 2)
      )
  }

  .gapfill_soil <- function(soil_grid, country_grid, max_search = 100L) {
    missing <- country_grid |>
      anti_join(soil_grid, by = c("lon", "lat"))
    if (nrow(missing) == 0) {
      return(soil_grid)
    }
    cli::cli_alert_info("Gap-filling {nrow(missing)} soil cells...")
    soil_lookup <- soil_grid |>
      select(lon, lat, soil_texture_code, soil_ph)
    filled <- vector("list", nrow(missing))
    for (i in seq_len(nrow(missing))) {
      m_lon <- missing$lon[i]
      m_lat <- missing$lat[i]
      found <- FALSE
      for (radius in seq_len(max_search)) {
        neighbours <- soil_lookup |>
          filter(
            abs(lon - m_lon) <= radius * 0.5,
            abs(lat - m_lat) <= radius * 0.5
          )
        if (nrow(neighbours) > 0) {
          neighbours <- neighbours |>
            mutate(
              dist = sqrt((lon - m_lon)^2 + (lat - m_lat)^2),
              dist = pmax(dist, 0.01),
              w = 1.0 / dist^2
            )
          dom_tex <- neighbours |>
            summarise(total_w = sum(w), .by = soil_texture_code) |>
            slice_max(total_w, n = 1, with_ties = FALSE)
          ph_val <- neighbours |>
            filter(soil_texture_code == dom_tex$soil_texture_code[1]) |>
            summarise(ph = stats::weighted.mean(soil_ph, w = w)) |>
            pull(ph)
          filled[[i]] <- tibble::tibble(
            lon = m_lon,
            lat = m_lat,
            soil_texture_code = dom_tex$soil_texture_code[1],
            soil_ph = round(ph_val, 2)
          )
          found <- TRUE
          break
        }
      }
      if (!found) {
        filled[[i]] <- tibble::tibble(
          lon = m_lon,
          lat = m_lat,
          soil_texture_code = 0L,
          soil_ph = 7.0
        )
      }
    }
    bind_rows(soil_grid, bind_rows(filled))
  }

  # Execute soil pipeline
  hwsd_attr <- .read_hwsd_attributes_local(hwsd_dir)
  mu_soils <- .derive_dominant_soil(hwsd_attr)
  soil_grid <- .aggregate_hwsd(hwsd_dir, mu_soils, target_res)

  country_grid <- nanoparquet::read_parquet(
    file.path(output_dir, "country_grid.parquet")
  )
  soil_grid <- .gapfill_soil(soil_grid, country_grid)
  soil_grid <- soil_grid |>
    inner_join(country_grid |> select(lon, lat), by = c("lon", "lat"))

  grid <- make_target_grid()
  soil_dt <- coord_to_rowcol(data.table::as.data.table(soil_grid), grid)
  dir.create(
    file.path(lpjml_out_dir, "soil"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  m_soil_type <- new_slice(grid$nlon, grid$nlat, fill = NA_real_)
  m_soil_type[cbind(soil_dt$col, soil_dt$row)] <- as.numeric(
    soil_dt$soil_texture_code
  )
  write_nc_2d(
    file.path(lpjml_out_dir, "soil", "soil_30arcmin_13_types.nc"),
    "soil_type",
    "USDA soil texture class (1-13)",
    "index",
    m_soil_type,
    grid$lon,
    grid$lat,
    prec = "float"
  )

  m_soil_ph <- new_slice(grid$nlon, grid$nlat, fill = NA_real_)
  m_soil_ph[cbind(soil_dt$col, soil_dt$row)] <- as.numeric(soil_dt$soil_ph)
  write_nc_2d(
    file.path(lpjml_out_dir, "soil", "soil_pH_30arcmin.nc"),
    "soil_pH",
    "Top-soil pH",
    "pH",
    m_soil_ph,
    grid$lon,
    grid$lat,
    prec = "float"
  )

  cli::cli_alert_success("Soil inputs complete")
}


# ==== Section 10a: Write static LPJmL inputs to NetCDF ====================

write_lpjml_static_inputs <- function(
  input_dir,
  lpjml_out_dir,
  drainage_dt = NULL,
  export_years = NULL,
  climate_dir = NULL
) {
  cli::cli_h2("Section 10a: Write static LPJmL inputs to NetCDF")

  lpjml_dirs <- c(
    "climate",
    "gadm",
    "lakes_rivers",
    "landuse",
    "nitrogen",
    "river_routing",
    "soil"
  )
  for (d in lpjml_dirs) {
    dir.create(
      file.path(lpjml_out_dir, d),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  grid <- make_target_grid()
  nlon_g <- grid$nlon
  nlat_g <- grid$nlat

  ddm_dlon <- c(0, 0.5, 0.5, 0, -0.5, -0.5, -0.5, 0, 0.5)
  ddm_dlat <- c(0, 0, -0.5, -0.5, -0.5, 0, 0.5, 0.5, 0.5)
  search_radius_m <- 75000

  # ---- 1) GADM-like static grids ------------------------------------------
  country_grid_file <- file.path(input_dir, "country_grid.parquet")
  if (file.exists(country_grid_file)) {
    cli::cli_alert_info("GADM: writing countrycode / coord / landfrac")
    cg <- data.table::as.data.table(
      nanoparquet::read_parquet(country_grid_file)
    )
    cg <- coord_to_rowcol(cg, grid)

    cow_path <- system.file("extdata", "cow_to_lpjml.csv", package = "whep")
    cow_lpjml <- data.table::as.data.table(read.csv(cow_path))
    cg <- cow_lpjml[cg, on = "area_code"]
    n_unmatched <- sum(is.na(cg$lpjml_code))
    if (n_unmatched > 0L) {
      cli::cli_warn(
        "{n_unmatched} cells with area_code not in cow_to_lpjml.csv (set to 0)"
      )
    }
    cg_matched <- cg[!is.na(cg$lpjml_code), ]

    m_country <- new_slice(grid$nlon, grid$nlat, fill = 0L)
    m_country[cbind(cg_matched$col, cg_matched$row)] <- cg_matched$lpjml_code
    write_nc_2d(
      file.path(lpjml_out_dir, "gadm", "cow_gadm_30arcmin.nc"),
      "countrycode",
      "LPJmL country index (per managepar.h) mapped to grid",
      "index",
      m_country,
      grid$lon,
      grid$lat,
      prec = "integer",
      missval = -9999L
    )

    m_coord <- new_slice(grid$nlon, grid$nlat, fill = 0L)
    ord <- order(cg$row, cg$col)
    m_coord[cbind(cg$col[ord], cg$row[ord])] <- seq_len(nrow(cg))
    write_nc_2d(
      file.path(lpjml_out_dir, "gadm", "grid_gadm_30arcmin.nc"),
      "coord",
      "Land-cell index on LPJmL grid",
      "index",
      m_coord,
      grid$lon,
      grid$lat,
      prec = "integer",
      missval = -9999L
    )

    m_landfrac <- new_slice(grid$nlon, grid$nlat, fill = 0)
    m_landfrac[cbind(cg$col, cg$row)] <- 1
    write_nc_2d(
      file.path(lpjml_out_dir, "gadm", "landfrac_gadm_30arcmin.nc"),
      "landfrac",
      "Land fraction (binary from country grid)",
      "1",
      m_landfrac,
      grid$lon,
      grid$lat,
      prec = "float"
    )
    cli::cli_alert_success("GADM: done")
  } else {
    cli::cli_warn("GADM: {.path {country_grid_file}} not found — skipping")
  }

  # ---- 3) Drainage routing ------------------------------------------------
  if (!is.null(drainage_dt)) {
    cli::cli_alert_info("Drainage: building river_routing.nc")
    dr <- data.table::as.data.table(drainage_dt)
    dr[, flat_idx := flat_of(lon, lat, nlon_g)]
    land_flat <- dr$flat_idx
    valid <- !is.na(dr$flow_direction) & dr$flow_direction > 0L
    dr[, nx_lon := lon]
    dr[, nx_lat := lat]
    dr[valid, nx_lon := round((lon + ddm_dlon[flow_direction + 1L]) * 2) / 2]
    dr[valid, nx_lat := round((lat + ddm_dlat[flow_direction + 1L]) * 2) / 2]
    dr[, nx_flat := -1L]
    dr[valid, nx_flat := flat_of(nx_lon, nx_lat, nlon_g)]
    dr[valid & !(nx_flat %in% land_flat), nx_flat := -1L]
    dr[, riverlen := 0]
    dr[nx_flat != -1L, riverlen := haver_m(lon, lat, nx_lon, nx_lat)]

    m_index <- matrix(-9999L, nrow = nlon_g, ncol = nlat_g)
    m_riverlen <- matrix(0, nrow = nlon_g, ncol = nlat_g)
    dr_rc <- coord_to_rowcol(dr, grid)
    m_index[cbind(dr_rc$col, dr_rc$row)] <- dr_rc$nx_flat
    m_riverlen[cbind(dr_rc$col, dr_rc$row)] <- dr_rc$riverlen

    out_drain <- file.path(lpjml_out_dir, "river_routing", "river_routing.nc")
    dlon_dim <- ncdim_def("longitude", "degrees_east", grid$lon)
    dlat_dim <- ncdim_def("latitude", "degrees_north", grid$lat)
    v_index <- ncvar_def(
      "index",
      "1",
      list(dlon_dim, dlat_dim),
      missval = -9999L,
      longname = "Flat 2D index of downstream cell",
      prec = "integer",
      compression = 1
    )
    v_riverlen <- ncvar_def(
      "riverlen",
      "m",
      list(dlon_dim, dlat_dim),
      missval = -9999,
      longname = "River length to downstream cell (m)",
      prec = "float",
      compression = 1
    )
    nc <- nc_create(
      out_drain,
      vars = list(v_index, v_riverlen),
      force_v4 = TRUE
    )
    ncvar_put(nc, v_index, m_index)
    ncvar_put(nc, v_riverlen, m_riverlen)
    ncatt_put(nc, 0, "Conventions", "CF-1.8")
    ncatt_put(nc, 0, "created_by", "WHEP prepare_spatialize_all.R")
    ncatt_put(nc, 0, "created_date", as.character(Sys.time()))
    nc_close(nc)
    cli::cli_alert_success(
      "Drainage: {sum(dr$nx_flat != -1L)} routed, {sum(dr$nx_flat == -1L)} sinks"
    )
  } else {
    cli::cli_warn("Drainage: drainage_dt not provided — skipping")
  }

  # ---- 4) Irrigation neighbour index --------------------------------------
  if (!is.null(drainage_dt) && file.exists(country_grid_file)) {
    cli::cli_alert_info(
      "Neighbour irrig: parallel search (radius {search_radius_m} m)..."
    )
    dr_n <- data.table::as.data.table(drainage_dt)
    cgi <- data.table::as.data.table(
      nanoparquet::read_parquet(country_grid_file)
    )
    cgi <- dr_n[cgi, on = c("lon", "lat")]
    cgi[, cell := .I]
    n_cells <- nrow(cgi)

    cgi[, flat_idx := flat_of(lon, lat, nlon_g)]
    lon_all <- cgi$lon
    lat_all <- cgi$lat
    flat_all <- cgi$flat_idx

    grid_lookup <- matrix(-1L, nrow = nlon_g, ncol = nlat_g)
    grid_lookup[cbind(lon_idx_of(lon_all) + 1L, lat_idx_of(lat_all) + 1L)] <-
      cgi$cell

    nextcell <- integer(n_cells)
    ddir <- cgi[!is.na(flow_direction) & flow_direction > 0L]
    nx_lon <- round((ddir$lon + ddm_dlon[ddir$flow_direction + 1L]) * 2) / 2
    nx_lat <- round((ddir$lat + ddm_dlat[ddir$flow_direction + 1L]) * 2) / 2
    nx_loi <- lon_idx_of(nx_lon) + 1L
    nx_li <- lat_idx_of(nx_lat) + 1L
    in_bounds <- nx_loi >= 1L & nx_loi <= nlon_g & nx_li >= 1L & nx_li <= nlat_g
    nx_cell <- integer(nrow(ddir))
    nx_cell[in_bounds] <- grid_lookup[cbind(
      nx_loi[in_bounds],
      nx_li[in_bounds]
    )]
    nextcell[ddir$cell] <- pmax(nx_cell, 0L)

    in_deg <- tabulate(nextcell[nextcell > 0L], nbins = n_cells)
    upstream_count <- rep(1L, n_cells)
    queue <- which(in_deg == 0L)
    qi <- 1L
    while (qi <= length(queue)) {
      ci <- queue[qi]
      qi <- qi + 1L
      parent <- nextcell[ci]
      if (parent > 0L) {
        upstream_count[parent] <- upstream_count[parent] + upstream_count[ci]
        in_deg[parent] <- in_deg[parent] - 1L
        if (in_deg[parent] == 0L) queue <- c(queue, parent)
      }
    }

    parents_list <- vector("list", n_cells)
    valid_nc <- which(nextcell > 0L)
    tmp <- split(valid_nc, nextcell[valid_nc])
    parents_list[as.integer(names(tmp))] <- tmp

    search_cells <- ceiling(search_radius_m / (111111 * 0.5)) + 1L
    ncores <- parallel::detectCores(logical = FALSE) %||% 1L
    neighbour_flat <- unlist(
      parallel::mclapply(
        seq_len(n_cells),
        function(i) {
          loi0 <- lon_idx_of(lon_all[i]) + 1L
          li0 <- lat_idx_of(lat_all[i]) + 1L
          loi_r <- seq(
            max(1L, loi0 - search_cells),
            min(nlon_g, loi0 + search_cells)
          )
          li_r <- seq(
            max(1L, li0 - search_cells),
            min(nlat_g, li0 + search_cells)
          )
          cands <- as.integer(grid_lookup[loi_r, li_r])
          cands <- cands[cands > 0L & cands != i]
          if (length(cands) == 0L) {
            return(flat_all[i])
          }
          dist_m <- haver_m(
            lon_all[i],
            lat_all[i],
            lon_all[cands],
            lat_all[cands]
          )
          keep <- dist_m <= search_radius_m
          cands <- cands[keep]
          dist_m <- dist_m[keep]
          if (length(cands) == 0L) {
            return(flat_all[i])
          }
          excl <- c(
            .downstream_vec(i, nextcell),
            .upstream_vec(i, parents_list)
          )
          cands <- cands[!cands %in% excl]
          if (length(cands) == 0L) {
            return(flat_all[i])
          }
          dist_m <- haver_m(
            lon_all[i],
            lat_all[i],
            lon_all[cands],
            lat_all[cands]
          )
          best <- cands[which.max(upstream_count[cands] / dist_m^2)]
          flat_all[best]
        },
        mc.cores = ncores
      ),
      use.names = FALSE
    )

    m_neighbour <- matrix(-9999L, nrow = nlon_g, ncol = nlat_g)
    m_neighbour[cbind(lon_idx_of(lon_all) + 1L, lat_idx_of(lat_all) + 1L)] <-
      neighbour_flat
    out_neighbour <- file.path(
      lpjml_out_dir,
      "river_routing",
      "neighbour_irrig_30arcmin_75000m_radius_exclude_downstream_exclude_upstream_idw.nc"
    )
    v_neigh <- ncvar_def(
      "neighbour",
      "index",
      list(dlon_dim, dlat_dim),
      missval = -9999L,
      longname = "Flat 2D index of irrigation neighbour cell",
      prec = "integer",
      compression = 1
    )
    nc_neigh <- nc_create(out_neighbour, vars = list(v_neigh), force_v4 = TRUE)
    ncvar_put(nc_neigh, v_neigh, m_neighbour)
    ncatt_put(nc_neigh, 0, "Conventions", "CF-1.8")
    ncatt_put(nc_neigh, 0, "search_radius_m", search_radius_m)
    ncatt_put(nc_neigh, 0, "created_by", "WHEP prepare_spatialize_all.R")
    ncatt_put(nc_neigh, 0, "created_date", as.character(Sys.time()))
    nc_close(nc_neigh)
    cli::cli_alert_success(
      "Neighbour irrig: {sum(neighbour_flat != flat_all)} non-self assignments"
    )
  } else {
    cli::cli_warn(
      "Neighbour irrig: missing drainage or country_grid — skipping"
    )
  }

  # ---- 8) NHx / NOy deposition --------------------------------------------
  dep_file <- file.path(input_dir, "n_deposition.parquet")
  if (file.exists(dep_file)) {
    cli::cli_alert_info("Deposition: reading {.path {dep_file}}")
    dep <- .filter_years(
      data.table::as.data.table(nanoparquet::read_parquet(dep_file)),
      export_years
    )
    has_coords <- "lon" %in% names(dep)
    if (has_coords) {
      dep_cells <- coord_to_rowcol(dep, grid)
    } else {
      cg_dep <- data.table::as.data.table(
        nanoparquet::read_parquet(country_grid_file)
      )
      cg_dep <- coord_to_rowcol(cg_dep, grid)
      dep_cells <- merge(
        dep,
        cg_dep[, .(area_code, row, col)],
        by = "area_code",
        allow.cartesian = TRUE
      )
    }
    has_split <- all(c("nhx", "noy") %in% names(dep_cells))
    if (has_split) {
      dep_cells[, value_nhx := nhx * 0.1 / 365]
      dep_cells[, value_noy := noy * 0.1 / 365]
    } else {
      dep_cells[, value_nhx := deposit_kg_n_ha * 0.1 / 365 * 0.5]
      dep_cells[, value_noy := deposit_kg_n_ha * 0.1 / 365 * 0.5]
      cli::cli_warn(
        "nhx/noy columns absent in deposition — using 50/50 fallback"
      )
    }
    years_d <- sort(unique(dep_cells$year))
    month_grid <- data.table::CJ(year = years_d, month = 1L:12L)
    dep_monthly <- merge(
      month_grid,
      dep_cells,
      by = "year",
      allow.cartesian = TRUE
    )
    dep_monthly[, time_idx := (year * 12L) + (month - 1L)]
    time_vals <- sort(unique(dep_monthly$time_idx))

    out_nhx <- file.path(
      lpjml_out_dir,
      "nitrogen",
      sprintf(
        "ndep_nhx_whep_annual_%d_%d.nc4",
        min(years_d),
        max(years_d)
      )
    )
    out_noy <- file.path(
      lpjml_out_dir,
      "nitrogen",
      sprintf(
        "ndep_noy_whep_annual_%d_%d.nc4",
        min(years_d),
        max(years_d)
      )
    )
    cli::cli_alert_info("Deposition: writing NHx -> {.path {out_nhx}}")
    .write_dep_monthly(
      out_nhx,
      "nhx",
      "value_nhx",
      grid,
      dep_monthly,
      time_vals
    )
    cli::cli_alert_info("Deposition: writing NOy -> {.path {out_noy}}")
    .write_dep_monthly(
      out_noy,
      "noy",
      "value_noy",
      grid,
      dep_monthly,
      time_vals
    )
    cli::cli_alert_success("Deposition: done")
  } else {
    cli::cli_warn("Deposition: {.path {dep_file}} not found — skipping")
  }

  # ---- 9) Climate (symlinks) ----------------------------------------------
  climate_mapping <- list(
    temp = "cru_ts_3_10.1901.2009.tmp.dat.nc",
    prec = "cru_ts_3_10_01.1901.2009.pre.dat.nc",
    cloud = "cru_ts_3_10.1901.2009.cld.dat.nc",
    wind = "wind_gswp3-w5e5_1901_2016_monthly.nc",
    co2 = "historical_CO2_annual_1765_2018.txt",
    wetdays = "cru_ts3.20.1901.2011.wet.dat.nc"
  )
  if (!is.null(climate_dir) && dir.exists(climate_dir)) {
    for (nm in names(climate_mapping)) {
      src_path <- file.path(climate_dir, climate_mapping[[nm]])
      dst_path <- file.path(lpjml_out_dir, "climate", climate_mapping[[nm]])
      if (file.exists(src_path)) {
        if (!file.exists(dst_path)) {
          file.symlink(src_path, dst_path)
        }
        cli::cli_alert_success("Climate: symlinked {nm}")
      } else {
        cli::cli_warn("Climate: {nm} not found in {.path {climate_dir}}")
      }
    }
  } else {
    cli::cli_alert_info(
      "Climate: no climate_dir provided — use LPJmL climate inputs separately"
    )
  }

  cli::cli_alert_success("Static LPJmL inputs complete")
}

# ==== Section 10: Run crop spatialization ==================================

# ---- Per-year yield spatialization ----------------------------------------
.spatialize_yields_chunk <- function(
  crops_with_country,
  country_yields,
  spatial_yield_idx,
  item_ratios
) {
  gridded_y <- data.table::as.data.table(country_yields)[
    data.table::as.data.table(crops_with_country),
    on = .(year, area_code, item_prod_code),
    nomatch = NULL
  ]

  if (!is.null(spatial_yield_idx)) {
    sy <- data.table::as.data.table(spatial_yield_idx)
    gridded_y[
      sy,
      spatial_yield_index := i.spatial_yield_index,
      on = .(lon, lat, item_prod_code)
    ]
    gridded_y[is.na(spatial_yield_index), spatial_yield_index := 1]
    gridded_y[, total_ha := rainfed_ha + irrigated_ha]
    gridded_y[,
      `:=`(
        weighted_sum = sum(spatial_yield_index * total_ha, na.rm = TRUE),
        ha_sum = sum(total_ha, na.rm = TRUE)
      ),
      by = .(year, area_code, item_prod_code)
    ]
    gridded_y[,
      renorm := data.table::fifelse(weighted_sum > 0, ha_sum / weighted_sum, 1)
    ]
    gridded_y[, yield_t_ha := yield_t_ha * spatial_yield_index * renorm]
    gridded_y[,
      c(
        "spatial_yield_index",
        "total_ha",
        "weighted_sum",
        "ha_sum",
        "renorm"
      ) := NULL
    ]
  }

  ir <- data.table::as.data.table(
    dplyr::select(item_ratios, item_prod_code, yield_ratio)
  )
  gridded_y[ir, yield_ratio := i.yield_ratio, on = "item_prod_code"]
  gridded_y[is.na(yield_ratio), yield_ratio := 1]
  gridded_y[, `:=`(
    total_ha = rainfed_ha + irrigated_ha,
    denom = rainfed_ha + yield_ratio * irrigated_ha
  )]
  gridded_y[,
    yield_rainfed := data.table::fifelse(
      denom > 0,
      yield_t_ha * total_ha / denom,
      yield_t_ha
    )
  ]
  gridded_y[, yield_irrigated := yield_ratio * yield_rainfed]
  gridded_y[,
    c("yield_t_ha", "yield_ratio", "total_ha", "denom") := NULL
  ]
  gridded_y
}

# ---- Chunk-level nitrogen spatialization ----------------------------------
# Downstream .write_nitrogen_nc_chunks only consumes kg_n_ha (mean per cell),
# so we skip the rainfed/irrigated split computed in earlier versions.
.spatialize_nitrogen_chunk <- function(
  crops_with_country,
  n_rates,
  spatial_n_idx,
  item_ratios
) {
  # Downstream only uses Synthetic and Manure; filter before the cartesian
  # to avoid carrying ~2-3× extra rows through the join and groupbys.
  nr <- data.table::as.data.table(n_rates)
  nr <- nr[fert_type %in% c("Synthetic", "Manure")]
  gridded_n <- nr[
    data.table::as.data.table(crops_with_country),
    on = .(year, area_code, item_prod_code),
    allow.cartesian = TRUE,
    nomatch = NULL
  ]

  if (!is.null(spatial_n_idx)) {
    sni <- data.table::as.data.table(spatial_n_idx)
    gridded_n[
      sni,
      spatial_n_index := i.spatial_n_index,
      on = .(lon, lat, item_prod_code, fert_type)
    ]
    gridded_n[is.na(spatial_n_index), spatial_n_index := 1]
    gridded_n[, total_ha := rainfed_ha + irrigated_ha]
    gridded_n[,
      `:=`(
        weighted_sum = sum(spatial_n_index * total_ha, na.rm = TRUE),
        ha_sum = sum(total_ha, na.rm = TRUE)
      ),
      by = .(year, area_code, item_prod_code, fert_type)
    ]
    gridded_n[,
      renorm := data.table::fifelse(weighted_sum > 0, ha_sum / weighted_sum, 1)
    ]
    gridded_n[, kg_n_ha := kg_n_ha * spatial_n_index * renorm]
    gridded_n[,
      c(
        "spatial_n_index",
        "total_ha",
        "weighted_sum",
        "ha_sum",
        "renorm"
      ) := NULL
    ]
  }

  gridded_n
}

.write_lu_nc_chunk <- function(
  nc_lu,
  cft_chunk,
  chunk_years,
  all_years,
  grid,
  row_area_ha
) {
  lu <- data.table::as.data.table(cft_chunk)
  lu[, base_pft := as.integer(cft_to_pft[cft_name])]
  lu <- lu[!is.na(base_pft)]
  lu <- coord_to_rowcol(lu, grid)
  lu[, cell_area_ha := row_area_ha[row]]
  lu[, rainfed_frac := pmin(1, pmax(0, rainfed_ha / cell_area_ha))]
  lu[, irrigated_frac := pmin(1, pmax(0, irrigated_ha / cell_area_ha))]
  rf <- lu[,
    .(value = sum(rainfed_frac, na.rm = TRUE)),
    by = .(year, pft = base_pft, row, col)
  ]
  ir <- lu[,
    .(value = sum(irrigated_frac, na.rm = TRUE)),
    by = .(year, pft = base_pft + 16L, row, col)
  ]
  out <- rbind(rf, ir)
  out[, value := pmin(1, pmax(0, value))]
  .pft_nc_write_chunk(nc_lu, out, chunk_years, all_years, grid, 32L)
}

.write_nitrogen_nc_chunks <- function(
  nc_syn,
  nc_man,
  chunk_years,
  all_years,
  grid,
  n_dt,
  cft_map_dt
) {
  ng <- data.table::as.data.table(n_dt)
  ng <- coord_to_rowcol(ng, grid)
  agg <- ng[,
    .(value = mean(kg_n_ha, na.rm = TRUE)),
    by = .(year, pft, fert_type, row, col)
  ]
  # Duplicate PFTs 1-16 into 17-32 for irrigated bands (LPJmL expects 32 bands)
  # Convert kgN/ha -> g/m2 (LPJmL expects g/m2)
  rf <- agg[, .(year, pft, row, col, value = value * 0.1, fert_type)]
  ir <- agg[, .(
    year,
    pft = pft + 16L,
    row,
    col,
    value = value * 0.1,
    fert_type
  )]
  both <- rbind(rf, ir)
  .pft_nc_write_chunk(
    nc_syn,
    both[fert_type == "Synthetic", !"fert_type"],
    chunk_years,
    all_years,
    grid,
    32L
  )
  .pft_nc_write_chunk(
    nc_man,
    both[fert_type == "Manure", !"fert_type"],
    chunk_years,
    all_years,
    grid,
    32L
  )
}

.write_yields_nc_chunk <- function(
  nc_yld,
  chunk_years,
  all_years,
  grid,
  yld_dt,
  cft_map_dt
) {
  yld <- data.table::as.data.table(yld_dt)
  if (nrow(yld) == 0L) {
    return(invisible(NULL))
  }
  yld <- coord_to_rowcol(yld, grid)
  yld[, w := rainfed_ha + irrigated_ha]
  agg <- yld[
    w > 0,
    .(
      rf_num = sum(yield_rainfed * w, na.rm = TRUE),
      ir_num = sum(yield_irrigated * w, na.rm = TRUE),
      w_sum = sum(w, na.rm = TRUE)
    ),
    by = .(year, pft, row, col)
  ]
  rf <- agg[, .(year, pft, row, col, value = rf_num / w_sum)]
  ir <- agg[, .(year, pft = pft + 16L, row, col, value = ir_num / w_sum)]
  rf[is.nan(value), value := 0]
  ir[is.nan(value), value := 0]
  .pft_nc_write_chunk(nc_yld, rbind(rf, ir), chunk_years, all_years, grid, 32L)
}

run_crop_spatialize <- function(
  run_dir,
  input_dir,
  year_range,
  lpjml_out_dir = file.path(run_dir, "lpjml_inputs"),
  n_workers = NULL
) {
  cli::cli_h2("Section 10: Crop spatialization")

  country_grid <- nanoparquet::read_parquet(
    file.path(input_dir, "country_grid.parquet")
  )
  country_areas <- nanoparquet::read_parquet(
    file.path(input_dir, "country_areas.parquet")
  ) |>
    dplyr::filter(year %in% year_range)

  crop_patterns <- nanoparquet::read_parquet(
    file.path(input_dir, "crop_patterns.parquet")
  )
  gridded_cropland <- nanoparquet::read_parquet(
    file.path(input_dir, "gridded_cropland.parquet")
  ) |>
    dplyr::filter(year %in% year_range)

  type_cl_path <- file.path(input_dir, "type_cropland.parquet")
  type_cropland <- NULL
  if (file.exists(type_cl_path)) {
    type_cropland <- nanoparquet::read_parquet(type_cl_path) |>
      dplyr::filter(year %in% year_range)
  }

  mc_path <- file.path(input_dir, "multicropping.parquet")
  multicropping <- NULL
  if (file.exists(mc_path)) {
    multicropping <- nanoparquet::read_parquet(mc_path) |>
      dplyr::filter(year %in% year_range)
    cli::cli_alert_info(
      "multicropping: {nrow(multicropping)} rows loaded"
    )
  }

  cft_mapping <- .read_cft_mapping()
  mapped_items <- cft_mapping$item_prod_code
  country_areas <- dplyr::filter(
    country_areas,
    item_prod_code %in% mapped_items
  )
  crop_patterns <- dplyr::filter(
    crop_patterns,
    item_prod_code %in% mapped_items
  )

  cli::cli_alert_info(
    "Years: {min(year_range)}-{max(year_range)}"
  )

  irrig_rf_ratios <- tibble::tribble(
    ~cft_name,              ~yield_ratio, ~n_rate_ratio,
    "temperate_cereals",    1.3,          1.3,
    "rice",                 1.6,          1.4,
    "maize",                1.5,          1.4,
    "tropical_cereals",     1.3,          1.3,
    "pulses",               1.3,          1.2,
    "oil_crops_soybean",    1.3,          1.2,
    "oil_crops_groundnut",  1.3,          1.3,
    "oil_crops_sunflower",  1.3,          1.3,
    "oil_crops_rapeseed",   1.3,          1.3,
    "oil_crops_other",      1.3,          1.3,
    "sugarcane",            1.2,          1.3,
    "temperate_roots",      1.3,          1.3,
    "tropical_roots",       1.2,          1.2,
    "others_annual",        1.3,          1.3,
    "others_perennial",     1.2,          1.2
  )

  item_ratios <- cft_mapping |>
    dplyr::select(item_prod_code, cft_name) |>
    dplyr::inner_join(irrig_rf_ratios, by = "cft_name")

  yields_file <- file.path(input_dir, "country_yields.parquet")
  yield_idx_file <- file.path(input_dir, "spatial_yield_index.parquet")
  has_yields <- file.exists(yields_file)
  country_yields <- if (has_yields) {
    nanoparquet::read_parquet(yields_file)
  }
  spatial_yield_idx <- if (has_yields && file.exists(yield_idx_file)) {
    nanoparquet::read_parquet(yield_idx_file)
  } else {
    NULL
  }

  n_inputs_file <- file.path(input_dir, "nitrogen_inputs.parquet")
  has_nitrogen <- file.exists(n_inputs_file)
  n_rates <- NULL
  spatial_n_idx <- NULL
  if (has_nitrogen) {
    n_inputs <- nanoparquet::read_parquet(n_inputs_file)
    items_prod <- readr::read_csv(
      system.file("extdata", "items_prod.csv", package = "whep"),
      show_col_types = FALSE
    )
    n_rates <- n_inputs |>
      dplyr::filter(!is.na(kg_n_ha), kg_n_ha > 0) |>
      dplyr::summarize(
        kg_n_ha = sum(kg_n_ha, na.rm = TRUE),
        .by = c(year, area_code, crop_name, fert_type)
      ) |>
      dplyr::left_join(
        dplyr::select(items_prod, item_prod_code, item_prod_name),
        by = c("crop_name" = "item_prod_name")
      ) |>
      dplyr::filter(!is.na(item_prod_code))
    spatial_idx_file <- file.path(input_dir, "spatial_n_index.parquet")
    if (file.exists(spatial_idx_file)) {
      spatial_n_idx <- nanoparquet::read_parquet(spatial_idx_file)
    }
  }

  years <- sort(unique(country_areas$year))
  years <- years[years %in% year_range]
  if (is.null(n_workers)) {
    n_workers <- .auto_spatialize_workers(
      length(years),
      worker_mb_default = 15000
    )
  }

  grid <- make_target_grid()
  row_area_ha <- cell_area_ha_by_lat(grid$lat)
  cft_map_dt <- data.table::as.data.table(
    dplyr::select(cft_mapping, item_prod_code, cft_name)
  )
  lu_dir <- file.path(lpjml_out_dir, "landuse")
  dir.create(lu_dir, recursive = TRUE, showWarnings = FALSE)
  yr_label <- sprintf("%d-%d", min(years), max(years))

  nc_lu <- .pft_nc_create(
    file.path(
      lu_dir,
      paste0("cft_default_cft_aggregation_30min_", yr_label, ".nc")
    ),
    "landuse",
    "Land use fraction",
    "1",
    grid,
    1:32,
    years
  )
  nc_syn <- if (has_nitrogen) {
    .pft_nc_create(
      file.path(
        lu_dir,
        paste0("fert_N_default_cft_aggregation_30min_", yr_label, ".nc")
      ),
      "fertilizer_nr",
      "Synthetic fertilizer nitrogen rate",
      "g/m2",
      grid,
      1:32,
      years
    )
  }
  nc_man <- if (has_nitrogen) {
    .pft_nc_create(
      file.path(
        lu_dir,
        paste0("manure_N_default_cft_aggregation_30min_", yr_label, ".nc")
      ),
      "manure_nr",
      "Manure nitrogen rate",
      "g/m2",
      grid,
      1:32,
      years
    )
  }
  nc_yld <- if (has_yields) {
    .pft_nc_create(
      file.path(lu_dir, paste0("yields_cft_30min_", yr_label, ".nc")),
      "yield_t_ha",
      "Crop yield (rainfed PFTs 1-16, irrigated 17-32)",
      "t ha-1 yr-1",
      grid,
      1:32,
      years
    )
  }

  chunk_size <- as.integer(Sys.getenv(
    "WHEP_SPATIALIZE_CHUNK_SIZE",
    unset = as.character(max(1L, min(30L, n_workers * 2L)))
  ))
  chunk_size <- max(1L, chunk_size)
  cli::cli_alert_info(
    "Crop spatialization workers: {n_workers}; chunk size: {chunk_size}"
  )
  n_chunks <- ceiling(length(years) / chunk_size)
  year_chunks <- split(years, ceiling(seq_along(years) / chunk_size))

  t_start <- proc.time()
  summary_rows <- vector("list", n_chunks)

  for (i in seq_along(year_chunks)) {
    chunk_years <- year_chunks[[i]]
    cli::cli_alert(
      "Chunk {i}/{n_chunks}: years {min(chunk_years)}-{max(chunk_years)}"
    )

    .step <- function(label, expr) {
      t0 <- proc.time()[["elapsed"]]
      out <- eval.parent(substitute(expr))
      dt <- round(proc.time()[["elapsed"]] - t0, 2)
      cli::cli_alert_info("  [{dt}s] {label}")
      out
    }

    crops_chunk <- .step(
      "build_gridded_landuse",
      whep::build_gridded_landuse(
        country_areas = dplyr::filter(country_areas, year %in% chunk_years),
        crop_patterns = crop_patterns,
        gridded_cropland = dplyr::filter(
          gridded_cropland,
          year %in% chunk_years
        ),
        country_grid = country_grid,
        config = list(
          type_cropland = if (!is.null(type_cropland)) {
            dplyr::filter(type_cropland, year %in% chunk_years)
          },
          type_mapping = cft_mapping,
          multicropping = if (!is.null(multicropping)) {
            dplyr::filter(multicropping, year %in% chunk_years)
          },
          n_workers = min(n_workers, length(chunk_years))
        )
      )
    )
    cli::cli_alert_info("    → {nrow(crops_chunk)} crop-cell rows")

    # Attach cft_name and pft once on crops_chunk; both the landuse CFT
    # aggregation and the nitrogen/yields helpers reuse it downstream.
    crops_chunk <- .step("attach cft_name + pft", {
      cc <- data.table::as.data.table(crops_chunk)
      cc[cft_map_dt, cft_name := i.cft_name, on = "item_prod_code"]
      cc[, pft := cft_to_pft[cft_name]]
      cc[!is.na(pft)]
    })

    cft_chunk <- .step(
      "aggregate to CFT",
      crops_chunk[,
        .(
          rainfed_ha = sum(rainfed_ha),
          irrigated_ha = sum(irrigated_ha)
        ),
        by = .(lon, lat, year, cft_name)
      ]
    )

    # Normalize to physical cropland before writing LPJmL landuse.
    # The spatializer may have allocated harvested area > cropland
    # (multicropping). LPJmL expects physical managed-area fractions,
    # so we scale each cell proportionally. Relative crop shares are
    # preserved; only the absolute hectares are clipped to cropland.
    cft_chunk_norm <- .step("normalize to cropland", {
      gc_chunk <- data.table::as.data.table(
        dplyr::filter(gridded_cropland, year %in% chunk_years)
      )
      dt <- data.table::as.data.table(cft_chunk)
      dt[gc_chunk, cropland_ha := i.cropland_ha, on = .(lon, lat, year)]
      dt[,
        total_ha := sum(rainfed_ha + irrigated_ha, na.rm = TRUE),
        by = .(lon, lat, year)
      ]
      dt[,
        scale := data.table::fifelse(
          !is.na(cropland_ha) & total_ha > cropland_ha & total_ha > 0,
          cropland_ha / total_ha,
          1
        )
      ]
      dt[, `:=`(
        rainfed_ha = rainfed_ha * scale,
        irrigated_ha = irrigated_ha * scale,
        total_ha = NULL,
        scale = NULL,
        cropland_ha = NULL
      )]
      dt
    })

    n_cft_chunk <- nrow(cft_chunk_norm)
    .step(
      paste0("write landuse NC (", n_cft_chunk, " CFT rows)"),
      .write_lu_nc_chunk(
        nc_lu,
        cft_chunk_norm,
        chunk_years,
        years,
        grid,
        row_area_ha
      )
    )
    summary_rows[[i]] <- dplyr::summarise(
      data.table::setDF(cft_chunk_norm),
      total_rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
      total_irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
      n_cells = dplyr::n(),
      .by = c(year, cft_name)
    )
    rm(cft_chunk, cft_chunk_norm)

    crops_with_country <- .step("attach area_code", {
      cwc <- data.table::as.data.table(crops_chunk)
      if (!"area_code" %in% names(cwc)) {
        cg_keys <- data.table::as.data.table(
          dplyr::select(country_grid, lon, lat, area_code)
        )
        cwc[cg_keys, area_code := i.area_code, on = .(lon, lat)]
      }
      cwc[!is.na(area_code)]
    })
    rm(crops_chunk)

    if (has_nitrogen) {
      n_chunk <- .step(
        "spatialize nitrogen",
        .spatialize_nitrogen_chunk(
          crops_with_country,
          n_rates = dplyr::filter(n_rates, year %in% chunk_years),
          spatial_n_idx = spatial_n_idx,
          item_ratios = item_ratios
        )
      )
      .step(
        "write nitrogen NC",
        .write_nitrogen_nc_chunks(
          nc_syn,
          nc_man,
          chunk_years,
          years,
          grid,
          n_chunk,
          cft_map_dt
        )
      )
      rm(n_chunk)
    }
    if (has_yields) {
      yields_chunk <- .step(
        "spatialize yields",
        .spatialize_yields_chunk(
          crops_with_country,
          country_yields = dplyr::filter(country_yields, year %in% chunk_years),
          spatial_yield_idx = spatial_yield_idx,
          item_ratios = item_ratios
        )
      )
      .step(
        "write yields NC",
        .write_yields_nc_chunk(
          nc_yld,
          chunk_years,
          years,
          grid,
          yields_chunk,
          cft_map_dt
        )
      )
      rm(yields_chunk)
    }

    rm(crops_with_country)
    mem_mb <- round(sum(gc()[, 2]), 0)
    cli::cli_alert_success(
      "Chunk {i} saved: cft={n_cft_chunk} rows, mem={mem_mb} MB"
    )
  }

  elapsed <- (proc.time() - t_start)[["elapsed"]]
  cli::cli_alert_success(
    "Individual crops done in {round(elapsed / 60, 1)} minutes"
  )

  tryCatch(
    {
      .nc_finalise(nc_lu)
      if (has_nitrogen) {
        .nc_finalise(nc_syn)
      }
      if (has_nitrogen) {
        .nc_finalise(nc_man)
      }
      if (has_yields) .nc_finalise(nc_yld)
    },
    error = function(e) cli::cli_warn("NC finalise error: {e$message}")
  )

  summary_tbl <- dplyr::bind_rows(summary_rows) |>
    dplyr::arrange(year, cft_name)
  readr::write_csv(summary_tbl, file.path(run_dir, "landuse_summary.csv"))

  cli::cli_alert_success("Crop spatialization complete")
  invisible(NULL)
}


# ==== Section 11: Run livestock spatialization ==============================

run_livestock_spatialize <- function(
  run_dir,
  input_dir,
  lpjml_out_dir = file.path(run_dir, "lpjml_inputs")
) {
  cli::cli_h2("Section 11: Livestock spatialization")

  livestock_data <- nanoparquet::read_parquet(
    file.path(input_dir, "livestock_country_data.parquet")
  )
  gridded_pasture <- nanoparquet::read_parquet(
    file.path(input_dir, "gridded_pasture.parquet")
  )
  gridded_cropland <- nanoparquet::read_parquet(
    file.path(input_dir, "gridded_cropland.parquet")
  )
  country_grid <- nanoparquet::read_parquet(
    file.path(input_dir, "country_grid.parquet")
  )

  mapping_path <- .find_extdata_file("livestock_mapping.csv")
  livestock_mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)
  species_proxy <- livestock_mapping |>
    dplyr::distinct(species_group, spatial_proxy)

  manure_pattern_path <- file.path(input_dir, "manure_pattern.parquet")
  manure_pattern <- NULL
  if (file.exists(manure_pattern_path)) {
    manure_pattern <- nanoparquet::read_parquet(manure_pattern_path)
  }

  years <- sort(unique(gridded_pasture$year))
  livestock_data <- dplyr::filter(livestock_data, year %in% years)

  grid <- make_target_grid()
  species_groups <- sort(unique(livestock_data$species_group))
  n_species <- length(species_groups)
  species_idx <- stats::setNames(seq_len(n_species), species_groups)

  ls_dir <- file.path(lpjml_out_dir, "livestock")
  dir.create(ls_dir, recursive = TRUE, showWarnings = FALSE)
  yr_label <- sprintf("%d-%d", min(years), max(years))

  ls_vars <- c(
    "heads",
    "enteric_ch4_kt",
    "manure_ch4_kt",
    "manure_n2o_kt",
    "manure_n_mg"
  )
  ls_units <- c(
    "1000 heads",
    "kt CH4 yr-1",
    "kt CH4 yr-1",
    "kt N2O yr-1",
    "Mg N yr-1"
  )
  nc_ls <- stats::setNames(
    purrr::map2(ls_vars, ls_units, \(v, u) {
      .pft_nc_create(
        file.path(ls_dir, paste0("livestock_", v, "_", yr_label, ".nc")),
        v,
        paste("Gridded livestock", v),
        u,
        grid,
        seq_len(n_species),
        years
      )
    }),
    ls_vars
  )
  readr::write_csv(
    tibble::tibble(
      index = seq_len(n_species),
      species_group = species_groups
    ),
    file.path(ls_dir, "livestock_species_index.csv")
  )

  chunk_size <- 20L
  year_chunks <- split(years, ceiling(seq_along(years) / chunk_size))

  cli::cli_alert_info(
    "Processing {length(years)} years in {length(year_chunks)} chunks"
  )

  summary_rows <- vector("list", length(year_chunks))
  grid_heads_rows <- vector("list", length(year_chunks))

  for (i in seq_along(year_chunks)) {
    chunk_years <- year_chunks[[i]]
    cli::cli_alert(
      "  Chunk {i}/{length(year_chunks)}: {min(chunk_years)}-{max(chunk_years)}"
    )
    chunk_result <- whep::build_gridded_livestock(
      livestock_data = dplyr::filter(livestock_data, year %in% chunk_years),
      gridded_pasture = dplyr::filter(gridded_pasture, year %in% chunk_years),
      gridded_cropland = dplyr::filter(gridded_cropland, year %in% chunk_years),
      country_grid = country_grid,
      species_proxy = species_proxy,
      manure_pattern = manure_pattern,
      glw_density = NULL
    )
    ls_dt <- coord_to_rowcol(data.table::as.data.table(chunk_result), grid)
    ls_dt[, species_pft := as.integer(species_idx[species_group])]

    for (v in ls_vars) {
      d <- ls_dt[, .(year, pft = species_pft, row, col, value = get(v))]
      .pft_nc_write_chunk(
        nc_ls[[v]],
        d,
        chunk_years,
        years,
        grid,
        n_species
      )
    }

    summary_rows[[i]] <- dplyr::summarise(
      chunk_result,
      total_heads = round(sum(heads, na.rm = TRUE)),
      enteric_ch4_kt = round(sum(enteric_ch4_kt, na.rm = TRUE), 2),
      manure_ch4_kt = round(sum(manure_ch4_kt, na.rm = TRUE), 2),
      manure_n2o_kt = round(sum(manure_n2o_kt, na.rm = TRUE), 2),
      manure_n_mg = round(sum(manure_n_mg, na.rm = TRUE), 1),
      n_cells = dplyr::n_distinct(paste(lon, lat)),
      .by = c(year, species_group)
    )
    grid_heads_rows[[i]] <- dplyr::summarise(
      chunk_result,
      grid_heads = sum(heads, na.rm = TRUE),
      .by = year
    )
    rm(chunk_result, ls_dt)
    gc()
  }

  tryCatch(
    purrr::walk(nc_ls, .nc_finalise),
    error = function(e) cli::cli_warn("NC finalise error: {e$message}")
  )

  country_totals <- dplyr::summarise(
    livestock_data,
    total_heads = sum(heads, na.rm = TRUE),
    .by = year
  )
  grid_totals <- dplyr::bind_rows(grid_heads_rows)
  check <- dplyr::inner_join(country_totals, grid_totals, by = "year") |>
    dplyr::mutate(heads_ratio = grid_heads / total_heads)
  heads_range <- range(check$heads_ratio, na.rm = TRUE)
  cli::cli_alert_info(
    "Heads conservation ratio: {round(heads_range[1], 4)} - {round(heads_range[2], 4)}"
  )

  summary_tbl <- dplyr::bind_rows(summary_rows) |>
    dplyr::arrange(year, species_group)
  readr::write_csv(summary_tbl, file.path(run_dir, "livestock_summary.csv"))

  cli::cli_alert_success("Livestock spatialization complete")
  invisible(NULL)
}


# ==== Main execution ===================================================

prepare_spatialize_all <- function(
  l_files_dir = "LPJmL_inputs",
  year_range = 1851:2023,
  target_res = 0.5,
  climate_dir = NULL
) {
  # For a quick test run use: year_range = 2000:2001
  #
  # Note on year_range upper bound: FAOSTAT QCL (faostat-production pin)
  # currently carries data through 2024; LUH2 v2h states.nc + management.nc
  # cover 850-2022, so years > 2022 reuse the 2022 LUH2 slice (handled by
  # the LUH2 reader). FAOSTAT-side data dominates the 2023+ values.
  cli::cli_h1("WHEP Spatialization Pipeline")

  if (!dir.exists(l_files_dir)) {
    cli::cli_abort("Directory does not exist: {.path {l_files_dir}}")
  }
  l_files_dir <- normalizePath(l_files_dir, mustWork = FALSE)
  output_dir <- file.path(l_files_dir, "whep", "inputs")
  run_dir <- file.path(l_files_dir, "whep")
  lpjml_out_dir <- file.path(run_dir, "lpjml_inputs")

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Cache production data (shared by crops + livestock)
  prod <- .load_or_cache_production(output_dir, year_range)

  # Section 1: Country grid
  country_grid <- prepare_country_grid(l_files_dir, target_res)
  .save_parquet(country_grid, output_dir, "country_grid")

  # Section 5: MIRCA irrigation (run before country_areas so MIRCA
  # is available on the first pass)
  prepare_mirca_irrigation(l_files_dir, output_dir, country_grid, target_res)

  # Section 2: Country areas
  country_areas <- prepare_country_areas(
    l_files_dir,
    year_range,
    country_grid,
    target_res,
    prod
  )
  .save_parquet(country_areas, output_dir, "country_areas")

  # Section 3: Crop patterns
  crop_patterns <- prepare_crop_patterns(l_files_dir, target_res)
  .save_parquet(crop_patterns, output_dir, "crop_patterns")

  # Section 3b: Crop fertilizer patterns
  crop_fert <- prepare_crop_fert_patterns(l_files_dir, target_res)
  if (!is.null(crop_fert)) {
    .save_parquet(crop_fert, output_dir, "crop_fertilizer_patterns")
  }

  # Section 4: Gridded cropland + type_cropland
  gridded_cropland <- prepare_gridded_cropland(
    l_files_dir,
    year_range,
    target_res,
    output_dir = output_dir
  )
  .save_parquet(gridded_cropland, output_dir, "gridded_cropland")

  # Section 4b: Multi-cropping factor (needs country_areas + type_cropland)
  prepare_multicropping(l_files_dir, output_dir)

  # Section 6: Yield inputs
  prepare_yield_inputs(
    l_files_dir,
    output_dir,
    target_res,
    year_range = year_range,
    prod = prod
  )

  # Section 7: Nitrogen inputs
  prepare_nitrogen_inputs(l_files_dir, output_dir, year_range, prod = prod)

  # Section 8: Livestock inputs
  prepare_livestock_inputs(
    l_files_dir,
    output_dir,
    year_range,
    target_res,
    prod = prod
  )

  # Section 9: Hydrology + Soil — write NC directly, no intermediary parquets
  drainage_dt <- prepare_hydrology_inputs(
    l_files_dir,
    output_dir,
    lpjml_out_dir
  )
  prepare_soil_inputs(l_files_dir, output_dir, lpjml_out_dir, target_res)

  # Section 10a: Write static LPJmL inputs (GADM, routing, deposition, climate)
  write_lpjml_static_inputs(
    output_dir,
    lpjml_out_dir,
    drainage_dt,
    export_years = year_range,
    climate_dir = climate_dir
  )

  # Section 10: Run crop spatialization
  run_crop_spatialize(run_dir, output_dir, year_range, lpjml_out_dir)

  # Section 11: Run livestock spatialization
  run_livestock_spatialize(run_dir, output_dir, lpjml_out_dir)

  cli::cli_alert_success("Pipeline complete")
}

# Run if executed directly (not just sourced)
if (sys.nframe() == 0L) {
  stop(
    "Call prepare_spatialize_all() with the path to your L_files directory, e.g.:\n",
    "  prepare_spatialize_all(\"/path/to/L_files\")"
  )
}
