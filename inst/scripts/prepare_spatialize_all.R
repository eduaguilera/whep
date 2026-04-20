# -----------------------------------------------------------------------
# prepare_spatialize_all.R
#
# Consolidated script that prepares all inputs for the WHEP
# spatialization pipeline and runs both crop and livestock
# spatialization.
#
# Merges logic from 13 individual scripts into a single pipeline with
# 11 sections, each wrapped as a callable function.  Can be sourced
# (to load functions without auto-running) or executed directly.
#
# Requires: WHEP_L_FILES_DIR environment variable pointing to L_files.
# Optional: WHEP_GLOBAL_DIR for Global repo (nitrogen inputs only).
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
#   gridded_landuse_crops, gridded_landuse, gridded_yields,
#   gridded_nitrogen, gridded_livestock_emissions
#
# Usage:
#   - Rscript this file to run every section in order.
#   - Source the file to make the section functions available in an
#     interactive R session, then call them one by one.
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Configuration ====================================================

l_files_dir <- whep:::.get_l_files_dir()
output_dir <- file.path(l_files_dir, "whep", "inputs")
run_dir <- file.path(l_files_dir, "whep")
year_range <- 1850L:2022L
target_res <- 0.5

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


# ==== Shared helpers ====================================================

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
.read_luh2_variables <- function(nc_path, var_names, time_idx) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  n_lon <- length(lon)
  n_lat <- length(lat)
  lat_desc <- lat[1] > lat[length(lat)]
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
  time_idx <- yr - 850L + 1L
  states_path <- file.path(luh2_dir, "states.nc")
  mgmt_path <- file.path(luh2_dir, "management.nc")
  irrig_vars <- paste0("irrig_", crop_vars)

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

  purrr::map(year_range, \(yr) {
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
  }) |>
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
    cli::cli_alert_info("Reading cached production data")
    return(nanoparquet::read_parquet(prod_cache))
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

  cli::cli_alert_info("Processing {nrow(xwalk)} crops from EarthStat")

  purrr::imap(
    rlang::set_names(xwalk$earthstat_name, xwalk$earthstat_name),
    \(crop_name, idx) {
      code <- xwalk$item_prod_code[xwalk$earthstat_name == crop_name]
      .read_one_earthstat_crop(earthstat_dir, crop_name, code, target_res)
    }
  ) |>
    dplyr::bind_rows() |>
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

  results <- purrr::map(nutrients, \(nutrient) {
    purrr::map2(
      fert_map$earthstat_fert_name,
      fert_map$item_prod_code,
      \(crop_name, code) {
        .read_one_earthstat_fertilizer(
          fert_dir,
          crop_name,
          code,
          nutrient,
          target_res
        )
      }
    ) |>
      dplyr::bind_rows()
  }) |>
    rlang::set_names(c("n", "p", "k"))

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

  all_years <- purrr::map(year_range, \(yr) {
    if (yr %% 10 == 0) {
      cli::cli_alert("Processing year {yr}")
    }
    .read_luh2_year(luh2_dir, yr, crop_vars, carea_rast, target_res)
  })

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

  ncols <- 4320L
  nrows <- 2160L
  nmonths <- 12L
  ncells <- ncols * nrows

  .read_mirca_binary_local <- function(fpath) {
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
    rm(raw_vals)
    r <- terra::rast(
      nrows = nrows,
      ncols = ncols,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90
    )
    terra::values(r) <- annual
    rm(annual)
    r
  }

  .find_mirca_file_local <- function(mirca_dir, crop_num, type) {
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

  .aggregate_mirca_local <- function(r, target_res) {
    agg_factor <- as.integer(target_res / (5 / 60))
    terra::aggregate(r, fact = agg_factor, fun = "sum", na.rm = TRUE)
  }

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

  # Process each MIRCA crop class
  all_crops <- list()
  for (crop_num in 1:26) {
    irrig_path <- .find_mirca_file_local(mirca_dir, crop_num, "irrigated")
    rain_path <- .find_mirca_file_local(mirca_dir, crop_num, "rainfed")
    if (is.null(irrig_path) || is.null(rain_path)) {
      cli::cli_alert_warning(
        "Crop {crop_num}: missing files"
      )
      next
    }
    cli::cli_alert_info("Crop {sprintf('%02d', crop_num)}: reading...")
    irrig_r <- .read_mirca_binary_local(irrig_path) |>
      .aggregate_mirca_local(target_res)
    rain_r <- .read_mirca_binary_local(rain_path) |>
      .aggregate_mirca_local(target_res)

    irrig_tbl <- .raster_to_tibble(irrig_r, "irrig_ha") |>
      filter(!is.na(irrig_ha), irrig_ha > 0)
    rm(irrig_r)
    rain_tbl <- .raster_to_tibble(rain_r, "rainfed_ha") |>
      filter(!is.na(rainfed_ha), rainfed_ha > 0)
    rm(rain_r)
    gc(verbose = FALSE)

    crop_tbl <- full_join(irrig_tbl, rain_tbl, by = c("lon", "lat")) |>
      mutate(
        irrig_ha = if_else(is.na(irrig_ha), 0, irrig_ha),
        rainfed_ha = if_else(is.na(rainfed_ha), 0, rainfed_ha),
        mirca_class = crop_num
      ) |>
      filter(irrig_ha > 0 | rainfed_ha > 0)
    all_crops[[crop_num]] <- crop_tbl
  }

  gridded_mirca <- bind_rows(all_crops) |>
    inner_join(country_grid, by = c("lon", "lat"))

  gridded_fao <- gridded_mirca |>
    inner_join(mirca_to_fao, by = "mirca_class", relationship = "many-to-many")

  # Output 1: Country-level irrigation fractions
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

  .save_parquet(country_irrig, output_dir, "mirca_irrigation_country")

  # Output 2: Gridded irrigation patterns
  gridded_patterns <- gridded_fao |>
    summarize(
      irrig_ha = sum(irrig_ha),
      rainfed_ha = sum(rainfed_ha),
      .by = c(lon, lat, item_prod_code)
    )

  .save_parquet(gridded_patterns, output_dir, "mirca_irrigation_patterns")

  cli::cli_alert_success("MIRCA2000 preparation complete")
  invisible(list(
    country = country_irrig,
    patterns = gridded_patterns
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
    "Reading yield rasters for {nrow(xwalk)} EarthStat crops..."
  )
  raw_yields <- purrr::pmap(
    list(
      crop_name = xwalk$earthstat_name,
      item_prod_code = xwalk$item_prod_code
    ),
    \(crop_name, item_prod_code) {
      .read_one_earthstat_yield(
        earthstat_dir,
        crop_name,
        item_prod_code,
        target_res
      )
    },
    .progress = TRUE
  ) |>
    dplyr::bind_rows()

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


# ==== Section 7: Nitrogen inputs ==========================================
#
# N/P/K inputs from FAOSTAT + spatial distribution.
# Requires WHEP_GLOBAL_DIR for some data sources.

prepare_nitrogen_inputs <- function(
  l_files_dir,
  output_dir,
  year_range,
  prod = NULL
) {
  cli::cli_h2("Section 7: Nitrogen / fertilizer inputs")

  global_dir <- Sys.getenv("WHEP_GLOBAL_DIR", unset = "")
  if (!nchar(global_dir) || !dir.exists(global_dir)) {
    cli::cli_alert_warning(
      "WHEP_GLOBAL_DIR not set or not found -- skipping nitrogen inputs"
    )
    return(invisible(NULL))
  }

  regions <- readr::read_csv(
    system.file("extdata", "regions.csv", package = "whep"),
    show_col_types = FALSE
  )
  items_prod <- readr::read_csv(
    system.file("extdata", "items_prod.csv", package = "whep"),
    show_col_types = FALSE
  )

  # ---- 7a. FAOSTAT N totals ----
  .read_faostat_totals_local <- function(l_files_dir, regions) {
    fert_file <- file.path(
      l_files_dir,
      "FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv"
    )
    manure_file <- file.path(
      l_files_dir,
      "FAOSTAT/Environment_LivestockManure_E_All_Data_(Normalized).csv"
    )
    if (!file.exists(fert_file) || !file.exists(manure_file)) {
      cli::cli_abort("FAOSTAT N files not found in {l_files_dir}/FAOSTAT/")
    }

    fert_raw <- data.table::fread(fert_file) |>
      tibble::as_tibble() |>
      filter(
        Element == "Agricultural Use",
        Item == "Nutrient nitrogen N (total)"
      ) |>
      transmute(
        area_code = `Area Code`,
        year = as.integer(Year),
        fert_type = "Synthetic",
        mg_n = Value
      )

    manure_raw <- data.table::fread(manure_file) |> tibble::as_tibble()

    manure_applied <- manure_raw |>
      filter(
        Element == "Manure applied to soils (N content)",
        Item == "All Animals"
      ) |>
      transmute(
        area_code = `Area Code`,
        year = as.integer(Year),
        fert_type = "Manure",
        mg_n = Value / 1000
      )

    manure_pasture <- manure_raw |>
      filter(
        Element == "Manure left on pasture (N content)",
        Item == "All Animals"
      ) |>
      transmute(
        area_code = `Area Code`,
        year = as.integer(Year),
        fert_type = "Grassland_excretion",
        mg_n = Value / 1000
      )

    bind_rows(fert_raw, manure_applied, manure_pasture) |>
      inner_join(select(regions, area_code, area_name), by = "area_code") |>
      filter(!is.na(mg_n), mg_n >= 0)
  }

  # ---- 7b. P/K totals ----
  .read_faostat_pk_totals_local <- function(l_files_dir, regions) {
    fert_file <- file.path(
      l_files_dir,
      "FAOSTAT/Inputs_FertilizersNutrient_E_All_Data_(Normalized).csv"
    )
    if (!file.exists(fert_file)) {
      return(NULL)
    }
    data.table::fread(fert_file) |>
      tibble::as_tibble() |>
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

  # ---- 7c. Cropland/Grassland split ----
  .read_crop_grass_split_local <- function(l_files_dir, regions) {
    euadb_dir <- file.path(
      l_files_dir,
      "EuropeAgriDB-v1.0-results/main_results/tables"
    )
    euadb <- NULL
    if (dir.exists(euadb_dir)) {
      synth_eu <- data.table::fread(
        file.path(euadb_dir, "synthetic_fertilizer.csv")
      ) |>
        tibble::as_tibble() |>
        filter(Symbol %in% c("Q_C", "Q_PG")) |>
        mutate(
          land_use = if_else(Symbol == "Q_C", "Cropland", "Grassland"),
          fert_type = "Synthetic"
        )
      manure_eu <- data.table::fread(
        file.path(euadb_dir, "manure_flows.csv")
      ) |>
        tibble::as_tibble() |>
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
            select(iso3c, area_code, area_name) |>
            mutate(iso2c = substr(iso3c, 1, 2)),
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
    }

    lass_file <- file.path(
      global_dir,
      "input",
      "Synthetic_N_Grassland_share.csv"
    )
    lass <- NULL
    if (file.exists(lass_file)) {
      lass_raw <- data.table::fread(lass_file, header = TRUE) |>
        tibble::as_tibble()
      year_cols <- grep("^(X?\\d{4})$", names(lass_raw), value = TRUE)
      lass <- lass_raw |>
        tidyr::pivot_longer(
          all_of(year_cols),
          names_to = "year",
          values_to = "grass_share_pct"
        ) |>
        mutate(
          year = as.integer(gsub("X", "", year)),
          grass_share = grass_share_pct / 100
        ) |>
        rename(lassaletta_name = Country) |>
        left_join(
          select(regions, iso3c, area_code, area_name),
          by = c("lassaletta_name" = "area_name")
        ) |>
        filter(!is.na(area_code)) |>
        select(year, area_code, grass_share)
    }

    list(euadb = euadb, lassaletta = lass)
  }

  # ---- 7d. Crop-specific base-year rates ----
  .read_crop_base_rates_local <- function(l_files_dir, global_dir, regions) {
    manure_file <- file.path(global_dir, "input", "Crops_manure_N.csv")
    crop_manure <- NULL
    if (file.exists(manure_file)) {
      crop_manure <- data.table::fread(manure_file) |>
        tibble::as_tibble() |>
        rename(crop_name = Crop_name, iso3c = ISO, manure_mg_n = Manure_N_Mg) |>
        left_join(select(regions, iso3c, area_code), by = "iso3c") |>
        filter(!is.na(area_code)) |>
        summarize(manure_mg_n = sum(manure_mg_n), .by = c(area_code, crop_name))
    }

    synth_file <- file.path(
      global_dir,
      "input",
      "Nfertilizationmatrix_filled_nodup.xlsx"
    )
    crop_synthetic <- NULL
    if (file.exists(synth_file)) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        install.packages("openxlsx", quiet = TRUE)
      }
      synth_raw <- openxlsx::read.xlsx(
        synth_file,
        sheet = "N fertilization",
        cols = c(1, 2, 5:197),
        rows = c(2:64),
        na.strings = "NaN",
        check.names = TRUE
      )
      crop_synthetic <- synth_raw |>
        tidyr::pivot_longer(
          -c(Proc.Code, Process),
          names_to = "iso3c",
          values_to = "kg_n_ha_synth"
        ) |>
        rename(proc_code = Proc.Code, crop_name = Process) |>
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
        filter(!is.na(area_code), !is.na(kg_n_ha_synth))
    }
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

    results <- purrr::map(seq_len(nrow(crop_map)), \(i) {
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
    }) |>
      dplyr::bind_rows()

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

    results <- purrr::map(seq_len(nrow(crop_map)), \(i) {
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
    }) |>
      dplyr::bind_rows()

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
      return(nanoparquet::read_parquet(dep_file))
    }
    global_dep <- file.path(
      l_files_dir,
      "Global/output/Global_N_deposition.csv"
    )
    if (file.exists(global_dep)) {
      dep_raw <- data.table::fread(global_dep) |> tibble::as_tibble()
      if ("Deposit_kgNha" %in% names(dep_raw)) {
        dep <- dep_raw |>
          select(year = Year, iso3c = ISO3, deposit_kg_n_ha = Deposit_kgNha) |>
          left_join(select(regions, iso3c, area_code), by = "iso3c") |>
          filter(!is.na(area_code), !is.na(deposit_kg_n_ha)) |>
          select(year, area_code, deposit_kg_n_ha)
        .save_parquet(dep, output_dir, "n_deposition")
        return(dep)
      }
    }
    cli::cli_alert_warning("N deposition data not available")
    NULL
  }

  # ---- Execute nitrogen pipeline ----
  cli::cli_alert_info("Running nitrogen input pipeline")

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

  n_totals <- .read_faostat_totals_local(l_files_dir, regions)
  pk_totals <- .read_faostat_pk_totals_local(l_files_dir, regions)
  lu_split <- .read_crop_grass_split_local(l_files_dir, regions)
  base_rates <- .read_crop_base_rates_local(l_files_dir, global_dir, regions)

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
  n_dep_crop <- NULL
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
    # Deposition
    if (!is.null(n_deposition)) {
      n_dep_crop <- crop_areas |>
        left_join(n_deposition, by = c("year", "area_code")) |>
        filter(!is.na(deposit_kg_n_ha)) |>
        mutate(
          fert_type = "Deposition",
          kg_n_ha = deposit_kg_n_ha,
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
    }
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
  all_parts <- list(n_crops, nsbnf, n_dep_crop, pk_crop)
  all_parts <- all_parts[!sapply(all_parts, is.null)]
  if (length(all_parts) > 0) {
    nitrogen_inputs <- bind_rows(all_parts) |>
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
    ) |>
    left_join(lass, by = c("year", "area_code")) |>
    mutate(
      lu_share = case_when(
        fert_type == "Grassland_excretion" & land_use == "Grassland" ~ 1.0,
        fert_type == "Grassland_excretion" & land_use == "Cropland" ~ 0.0,
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


# ==== Section 8: Livestock inputs ==========================================
#
# Livestock stocks, emissions, gridded pasture, manure pattern.

prepare_livestock_inputs <- function(
  l_files_dir,
  output_dir,
  year_range,
  target_res,
  prod = NULL
) {
  cli::cli_h2("Section 8: Livestock inputs")

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
      select(livestock_mapping, item_code, species_group, nex_kg_n_head),
      by = "item_code"
    )

  stocks_grouped <- stocks |>
    summarise(
      heads = sum(heads, na.rm = TRUE),
      nex_kg_n_head = sum(heads * nex_kg_n_head) / sum(heads),
      .by = c(year, area_code, species_group)
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
      "Cattle, dairy", "cattle",
      "Cattle, non-dairy", "cattle",
      "Buffalo", "buffalo",
      "Sheep", "sheep_goats",
      "Goats", "sheep_goats",
      "Swine, market", "pigs",
      "Swine, breeding", "pigs",
      "Chickens, layers", "poultry",
      "Chickens, broilers", "poultry",
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

  pasture_years <- sort(unique(livestock_country$year))
  pasture_list <- list()
  for (i in seq_along(pasture_years)) {
    yr <- pasture_years[i]
    if (yr %% 50 == 0 || yr == min(pasture_years)) {
      cli::cli_alert("  Year {yr} ({i}/{length(pasture_years)})")
    }
    time_idx <- yr - 850L + 1L
    pastr_r <- .read_luh2_variable(states_path, "pastr", time_idx)
    range_r <- .read_luh2_variable(states_path, "range", time_idx)
    pastr_ha <- pastr_r * carea_ha
    range_ha <- range_r * carea_ha
    pastr_ha <- terra::aggregate(
      pastr_ha,
      fact = agg_factor,
      fun = "sum",
      na.rm = TRUE
    )
    range_ha <- terra::aggregate(
      range_ha,
      fact = agg_factor,
      fun = "sum",
      na.rm = TRUE
    )
    p_tbl <- .raster_to_tibble(pastr_ha, "pasture_ha")
    r_tbl <- .raster_to_tibble(range_ha, "rangeland_ha")
    yr_tbl <- left_join(p_tbl, r_tbl, by = c("lon", "lat")) |>
      mutate(
        pasture_ha = if_else(is.na(pasture_ha), 0, pasture_ha),
        rangeland_ha = if_else(is.na(rangeland_ha), 0, rangeland_ha)
      ) |>
      filter(pasture_ha > 0 | rangeland_ha > 0) |>
      mutate(year = yr)
    pasture_list[[i]] <- yr_tbl
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

prepare_hydrology_inputs <- function(l_files_dir, output_dir) {
  cli::cli_h2("Section 9a: Hydrology inputs")

  country_grid <- nanoparquet::read_parquet(
    file.path(output_dir, "country_grid.parquet")
  )
  grand_dir <- file.path(l_files_dir, "GIS", "Global dams")
  glwd_dir <- file.path(l_files_dir, "GLWD")
  drainage_paths <- c(
    file.path(l_files_dir, "DRT", "DRT_half_FDR_globe.asc"),
    file.path(l_files_dir, "DDM30", "ddm30.asc")
  )

  # ---- Elevation ----
  .prepare_elevation <- function(country_grid, output_dir) {
    cli::cli_alert_info("Elevation")
    if (!requireNamespace("geodata", quietly = TRUE)) {
      install.packages("geodata", quiet = TRUE)
    }
    elev_10m <- geodata::elevation_global(
      res = 10,
      path = file.path(output_dir, ".cache")
    )
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
        min_year = min(year, na.rm = TRUE),
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
  .prepare_lakes_rivers <- function(country_grid, glwd_dir, output_dir) {
    cli::cli_alert_info("Lakes & Rivers (GLWD)")
    glwd_path <- NULL
    glwd_version <- NULL
    v2_dir <- file.path(glwd_dir, "GLWD_v2")
    v2_tifs <- list.files(
      v2_dir,
      pattern = "dominant.*\\.tif$",
      recursive = TRUE,
      full.names = TRUE
    )
    if (length(v2_tifs) == 0) {
      v2_tifs <- list.files(
        v2_dir,
        pattern = "combined.*\\.tif$",
        recursive = TRUE,
        full.names = TRUE
      )
    }
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
    lake_mask <- terra::classify(glwd, lake_rcl, othersNA = TRUE)
    lake_frac <- terra::aggregate(
      lake_mask,
      fact = agg_factor,
      fun = "mean",
      na.rm = TRUE
    )
    river_rcl <- cbind(river_classes, rep(1, length(river_classes)))
    river_mask <- terra::classify(glwd, river_rcl, othersNA = TRUE)
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
    result <- country_grid |>
      select(lon, lat) |>
      mutate(
        lake_fraction = round(replace(lake_vals, is.na(lake_vals), 0), 6),
        river_fraction = round(replace(river_vals, is.na(river_vals), 0), 6)
      )
    .save_parquet(result, output_dir, "lakes_rivers")
  }

  # ---- Drainage ----
  .prepare_drainage <- function(country_grid, drainage_paths, output_dir) {
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
    .save_parquet(result, output_dir, "drainage")
  }

  .prepare_elevation(country_grid, output_dir)
  .prepare_reservoirs(country_grid, grand_dir, output_dir)
  .prepare_lakes_rivers(country_grid, glwd_dir, output_dir)
  .prepare_drainage(country_grid, drainage_paths, output_dir)

  cli::cli_alert_success("Hydrology inputs complete")
}

prepare_soil_inputs <- function(l_files_dir, output_dir, target_res = 0.5) {
  cli::cli_h2("Section 9b: Soil inputs (HWSD)")

  hwsd_dir <- file.path(l_files_dir, "HWSD")

  .read_hwsd_attributes_local <- function(hwsd_dir) {
    db_path <- file.path(hwsd_dir, "HWSD.SQLite")
    if (!file.exists(db_path)) {
      cli::cli_abort("HWSD SQLite database not found at {db_path}")
    }
    db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(db), add = TRUE)
    DBI::dbGetQuery(
      db,
      "
      SELECT mu_global, issoil, share,
             t_sand, t_silt, t_clay,
             t_usda_tex_class, t_ph_h2o, su_sym90
      FROM hwsd_data
    "
    ) |>
      tibble::as_tibble()
  }

  .derive_dominant_soil <- function(hwsd_attr) {
    hwsd_attr <- hwsd_attr |>
      mutate(
        t_usda_tex_class = case_when(
          su_sym90 == "DS" & is.na(t_usda_tex_class) ~ 13L,
          TRUE ~ as.integer(t_usda_tex_class)
        )
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
    target_rast <- terra::rast(
      resolution = target_res,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90
    )
    agg_factor <- as.integer(target_res / terra::res(hwsd_rast)[1])
    n_target_rows <- terra::nrow(target_rast)
    n_target_cols <- terra::ncol(target_rast)
    results <- vector("list", n_target_rows)

    for (row_i in seq_len(n_target_rows)) {
      src_row_start <- (row_i - 1L) * agg_factor + 1L
      strip <- terra::values(
        hwsd_rast,
        row = src_row_start,
        nrows = agg_factor
      )[, 1]
      strip_mat <- matrix(strip, nrow = agg_factor, byrow = TRUE)
      lat <- 90.0 - (row_i - 0.5) * target_res
      row_results <- vector("list", n_target_cols)

      for (col_j in seq_len(n_target_cols)) {
        src_col_start <- (col_j - 1L) * agg_factor + 1L
        src_col_end <- col_j * agg_factor
        block <- strip_mat[, src_col_start:src_col_end]
        mu_ids <- as.integer(block)
        mu_ids <- mu_ids[mu_ids > 0L & !is.na(mu_ids)]
        if (length(mu_ids) == 0L) {
          next
        }
        lon <- -180.0 + (col_j - 0.5) * target_res
        mu_counts <- tibble::tibble(mu_global = mu_ids) |>
          count(mu_global, name = "n_cells")
        mu_soil <- mu_counts |>
          inner_join(mu_soils, by = "mu_global")
        if (nrow(mu_soil) == 0L) {
          next
        }
        dom <- mu_soil |>
          summarise(total_cells = sum(n_cells), .by = t_usda_tex_class) |>
          slice_max(total_cells, n = 1, with_ties = FALSE)
        ph_val <- mu_soil |>
          filter(t_usda_tex_class == dom$t_usda_tex_class[1]) |>
          summarise(
            ph = stats::weighted.mean(t_ph_h2o, w = n_cells, na.rm = TRUE)
          ) |>
          pull(ph)
        row_results[[col_j]] <- tibble::tibble(
          lon = lon,
          lat = lat,
          soil_texture_code = dom$t_usda_tex_class[1],
          soil_ph = round(ph_val, 2)
        )
      }
      results[[row_i]] <- bind_rows(row_results)
      if (row_i %% 36 == 0) {
        cli::cli_alert("  Row {row_i}/{n_target_rows} ({lat} lat)")
      }
    }
    bind_rows(results)
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

  .save_parquet(soil_grid, output_dir, "soil")
  cli::cli_alert_success("Soil inputs complete")
  invisible(soil_grid)
}


# ==== Section 10: Run crop spatialization ==================================

run_crop_spatialize <- function(run_dir, input_dir) {
  cli::cli_h2("Section 10: Crop spatialization")

  year_range <- 1850L:2022L

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

  t_start <- proc.time()

  result_crops <- whep::build_gridded_landuse(
    country_areas = country_areas,
    crop_patterns = crop_patterns,
    gridded_cropland = gridded_cropland,
    country_grid = country_grid,
    cft_mapping = NULL,
    type_cropland = type_cropland,
    type_mapping = cft_mapping
  )

  elapsed <- (proc.time() - t_start)[["elapsed"]]
  cli::cli_alert_success(
    "Individual crops done in {round(elapsed / 60, 1)} minutes"
  )

  # Save individual crop output
  crop_path <- file.path(run_dir, "gridded_landuse_crops.parquet")
  nanoparquet::write_parquet(result_crops, crop_path)

  # CFT aggregation
  result <- result_crops |>
    dplyr::inner_join(
      dplyr::select(cft_mapping, item_prod_code, cft_name),
      by = "item_prod_code"
    ) |>
    dplyr::summarise(
      rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
      irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
      .by = c(lon, lat, year, cft_name)
    )

  out_path <- file.path(run_dir, "gridded_landuse.parquet")
  nanoparquet::write_parquet(result, out_path)

  # Summary CSV
  summary_tbl <- result |>
    dplyr::summarise(
      total_rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
      total_irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
      n_cells = dplyr::n_distinct(paste(lon, lat)),
      .by = c(year, cft_name)
    ) |>
    dplyr::arrange(year, cft_name)
  readr::write_csv(summary_tbl, file.path(run_dir, "landuse_summary.csv"))

  # Irrigated/rainfed ratio tables
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

  # Yield spatialization
  yields_file <- file.path(input_dir, "country_yields.parquet")
  yield_idx_file <- file.path(input_dir, "spatial_yield_index.parquet")

  if (file.exists(yields_file)) {
    cli::cli_alert_info("Spatializing crop yields")
    country_yields <- nanoparquet::read_parquet(yields_file)
    spatial_yield_idx <- NULL
    if (file.exists(yield_idx_file)) {
      spatial_yield_idx <- nanoparquet::read_parquet(yield_idx_file)
    }

    gridded_y <- result_crops |>
      dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
      dplyr::inner_join(
        country_yields,
        by = c("year", "area_code", "item_prod_code")
      )

    if (!is.null(spatial_yield_idx)) {
      gridded_y <- gridded_y |>
        dplyr::left_join(
          spatial_yield_idx,
          by = c("lon", "lat", "item_prod_code")
        ) |>
        dplyr::mutate(
          spatial_yield_index = dplyr::coalesce(spatial_yield_index, 1.0)
        ) |>
        dplyr::mutate(
          total_ha = rainfed_ha + irrigated_ha,
          weighted_sum = sum(spatial_yield_index * total_ha, na.rm = TRUE),
          ha_sum = sum(total_ha, na.rm = TRUE),
          renorm = dplyr::if_else(
            weighted_sum > 0,
            ha_sum / weighted_sum,
            1.0
          ),
          yield_t_ha = yield_t_ha * spatial_yield_index * renorm,
          .by = c("year", "area_code", "item_prod_code")
        ) |>
        dplyr::select(
          -spatial_yield_index,
          -total_ha,
          -weighted_sum,
          -ha_sum,
          -renorm
        )
    }

    gridded_y <- gridded_y |>
      dplyr::left_join(
        dplyr::select(item_ratios, item_prod_code, yield_ratio),
        by = "item_prod_code"
      ) |>
      dplyr::mutate(
        yield_ratio = dplyr::coalesce(yield_ratio, 1.0),
        total_ha = rainfed_ha + irrigated_ha,
        denom = rainfed_ha + yield_ratio * irrigated_ha,
        yield_rainfed = dplyr::if_else(
          denom > 0,
          yield_t_ha * total_ha / denom,
          yield_t_ha
        ),
        yield_irrigated = yield_ratio * yield_rainfed,
        rainfed_prod_t = yield_rainfed * rainfed_ha,
        irrigated_prod_t = yield_irrigated * irrigated_ha
      ) |>
      dplyr::select(
        lon,
        lat,
        year,
        area_code,
        item_prod_code,
        rainfed_ha,
        irrigated_ha,
        yield_rainfed,
        yield_irrigated,
        rainfed_prod_t,
        irrigated_prod_t
      )

    nanoparquet::write_parquet(
      gridded_y,
      file.path(run_dir, "gridded_yields.parquet")
    )
    cli::cli_alert_success("Gridded yields: {nrow(gridded_y)} rows")
  }

  # Nitrogen spatialization
  n_inputs_file <- file.path(input_dir, "nitrogen_inputs.parquet")
  if (file.exists(n_inputs_file)) {
    cli::cli_alert_info("Spatializing nitrogen inputs")
    n_inputs <- nanoparquet::read_parquet(n_inputs_file)
    spatial_idx_file <- file.path(input_dir, "spatial_n_index.parquet")
    spatial_n_idx <- NULL
    if (file.exists(spatial_idx_file)) {
      spatial_n_idx <- nanoparquet::read_parquet(spatial_idx_file)
    }

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

    gridded_n <- result_crops |>
      dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
      dplyr::inner_join(n_rates, by = c("year", "area_code", "item_prod_code"))

    if (!is.null(spatial_n_idx)) {
      gridded_n <- gridded_n |>
        dplyr::left_join(
          spatial_n_idx,
          by = c("lon", "lat", "item_prod_code", "fert_type")
        ) |>
        dplyr::mutate(
          spatial_n_index = dplyr::coalesce(spatial_n_index, 1.0)
        ) |>
        dplyr::mutate(
          total_ha = rainfed_ha + irrigated_ha,
          weighted_sum = sum(spatial_n_index * total_ha, na.rm = TRUE),
          ha_sum = sum(total_ha, na.rm = TRUE),
          renorm = dplyr::if_else(
            weighted_sum > 0,
            ha_sum / weighted_sum,
            1.0
          ),
          kg_n_ha = kg_n_ha * spatial_n_index * renorm,
          .by = c("year", "area_code", "item_prod_code", "fert_type")
        ) |>
        dplyr::select(
          -spatial_n_index,
          -total_ha,
          -weighted_sum,
          -ha_sum,
          -renorm
        )
    }

    gridded_n <- gridded_n |>
      dplyr::left_join(
        dplyr::select(item_ratios, item_prod_code, n_rate_ratio),
        by = "item_prod_code"
      ) |>
      dplyr::mutate(
        n_rate_ratio = dplyr::coalesce(n_rate_ratio, 1.0),
        total_ha = rainfed_ha + irrigated_ha,
        denom = rainfed_ha + n_rate_ratio * irrigated_ha,
        kg_n_ha_rainfed = dplyr::if_else(
          denom > 0,
          kg_n_ha * total_ha / denom,
          kg_n_ha
        ),
        kg_n_ha_irrigated = n_rate_ratio * kg_n_ha_rainfed,
        rainfed_n_mg = rainfed_ha * kg_n_ha_rainfed / 1000,
        irrigated_n_mg = irrigated_ha * kg_n_ha_irrigated / 1000
      ) |>
      dplyr::select(
        lon,
        lat,
        year,
        area_code,
        item_prod_code,
        fert_type,
        kg_n_ha,
        kg_n_ha_rainfed,
        kg_n_ha_irrigated,
        rainfed_ha,
        irrigated_ha,
        rainfed_n_mg,
        irrigated_n_mg
      )

    nanoparquet::write_parquet(
      gridded_n,
      file.path(run_dir, "gridded_nitrogen.parquet")
    )
    cli::cli_alert_success("Gridded nitrogen: {nrow(gridded_n)} rows")
  }

  cli::cli_alert_success("Crop spatialization complete")
  invisible(result_crops)
}


# ==== Section 11: Run livestock spatialization ==============================

run_livestock_spatialize <- function(run_dir, input_dir) {
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
  species_proxy <- livestock_mapping |> distinct(species_group, spatial_proxy)

  manure_pattern_path <- file.path(input_dir, "manure_pattern.parquet")
  manure_pattern <- NULL
  if (file.exists(manure_pattern_path)) {
    manure_pattern <- nanoparquet::read_parquet(manure_pattern_path)
  }

  # Process in annual chunks
  years <- sort(unique(livestock_data$year))
  chunk_size <- 20L
  year_chunks <- split(years, ceiling(seq_along(years) / chunk_size))

  cli::cli_alert_info(
    "Processing {length(years)} years in {length(year_chunks)} chunks"
  )

  result_list <- list()
  for (i in seq_along(year_chunks)) {
    chunk_years <- year_chunks[[i]]
    cli::cli_alert(
      "  Chunk {i}/{length(year_chunks)}: {min(chunk_years)}-{max(chunk_years)}"
    )
    chunk_result <- whep::build_gridded_livestock(
      livestock_data = filter(livestock_data, year %in% chunk_years),
      gridded_pasture = filter(gridded_pasture, year %in% chunk_years),
      gridded_cropland = filter(gridded_cropland, year %in% chunk_years),
      country_grid = country_grid,
      species_proxy = species_proxy,
      manure_pattern = manure_pattern,
      glw_density = NULL
    )
    result_list[[i]] <- chunk_result
  }

  gridded_livestock <- bind_rows(result_list)

  # Conservation check
  country_totals <- livestock_data |>
    summarise(total_heads = sum(heads, na.rm = TRUE), .by = year)
  grid_totals <- gridded_livestock |>
    summarise(grid_heads = sum(heads, na.rm = TRUE), .by = year)
  check <- inner_join(country_totals, grid_totals, by = "year") |>
    mutate(heads_ratio = grid_heads / total_heads)
  heads_range <- range(check$heads_ratio, na.rm = TRUE)
  cli::cli_alert_info(
    "Heads conservation ratio: {round(heads_range[1], 4)} - {round(heads_range[2], 4)}"
  )

  # Save
  nanoparquet::write_parquet(
    gridded_livestock,
    file.path(run_dir, "gridded_livestock_emissions.parquet")
  )

  summary_tbl <- gridded_livestock |>
    summarise(
      total_heads = round(sum(heads, na.rm = TRUE)),
      enteric_ch4_kt = round(sum(enteric_ch4_kt, na.rm = TRUE), 2),
      manure_ch4_kt = round(sum(manure_ch4_kt, na.rm = TRUE), 2),
      manure_n2o_kt = round(sum(manure_n2o_kt, na.rm = TRUE), 2),
      manure_n_mg = round(sum(manure_n_mg, na.rm = TRUE), 1),
      n_cells = n_distinct(paste(lon, lat)),
      .by = c(year, species_group)
    ) |>
    arrange(year, species_group)
  readr::write_csv(summary_tbl, file.path(run_dir, "livestock_summary.csv"))

  cli::cli_alert_success("Livestock spatialization complete")
  invisible(gridded_livestock)
}


# ==== Main execution ===================================================

main <- function() {
  cli::cli_h1("WHEP Spatialization Pipeline")

  # Cache production data (shared by crops + livestock)
  prod <- .load_or_cache_production(output_dir, year_range)

  # Section 1: Country grid
  country_grid <- prepare_country_grid(l_files_dir, target_res)
  .save_parquet(country_grid, output_dir, "country_grid")

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

  # Section 5: MIRCA irrigation
  prepare_mirca_irrigation(l_files_dir, output_dir, country_grid, target_res)

  # Re-run country_areas with MIRCA if it was just generated
  mirca_path <- file.path(output_dir, "mirca_irrigation_country.parquet")
  if (file.exists(mirca_path)) {
    cli::cli_alert_info("Re-generating country_areas with MIRCA irrigation")
    country_areas <- prepare_country_areas(
      l_files_dir,
      year_range,
      country_grid,
      target_res,
      prod
    )
    .save_parquet(country_areas, output_dir, "country_areas")
  }

  # Section 6: Yield inputs
  prepare_yield_inputs(
    l_files_dir,
    output_dir,
    target_res,
    year_range = 1961L:2022L,
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

  # Section 9: Hydrology + Soil
  prepare_hydrology_inputs(l_files_dir, output_dir)
  prepare_soil_inputs(l_files_dir, output_dir, target_res)

  # Section 10: Run crop spatialization
  run_crop_spatialize(run_dir, output_dir)

  # Section 11: Run livestock spatialization
  run_livestock_spatialize(run_dir, output_dir)

  cli::cli_alert_success("Pipeline complete")
}

# Run if executed directly (not just sourced)
if (sys.nframe() == 0L) {
  main()
}
