# -----------------------------------------------------------------------
# prepare_spatialize_inputs.R
#
# Reads raw spatial data from L_files and prepares the four input
# tibbles required by build_gridded_landuse(). Each output is saved
# as parquet to L_files/whep/inputs/.
#
# Raw data expected in L_files:
#   FAOSTAT, EarthStat, LUH2 v2h, and NaturalEarth shapefiles.
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Private helpers =================================================

.find_extdata_file <- function(filename) {
  local_path <- file.path("inst", "extdata", filename)
  if (file.exists(local_path)) return(local_path)
  pkg_path <- system.file("extdata", filename, package = "whep")
  if (nchar(pkg_path) > 0) return(pkg_path)
  cli::cli_abort("Cannot find {filename} in inst/extdata or package.")
}

.read_cft_mapping <- function() {
  .find_extdata_file("cft_mapping.csv") |>
    readr::read_csv(show_col_types = FALSE)
}

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

.raster_to_tibble <- function(r, value_name) {
  coords <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  tibble::tibble(
    lon = round(coords[, 1], 2),
    lat = round(coords[, 2], 2),
    value = terra::values(r)[, 1]
  ) |>
    rlang::set_names(c("lon", "lat", value_name))
}

.read_one_earthstat_crop <- function(earthstat_dir, crop_name,
                                     item_prod_code, target_res) {
  tif_path <- file.path(
    earthstat_dir, crop_name,
    paste0(crop_name, "_HarvestedAreaFraction.tif")
  )

  if (!file.exists(tif_path)) {
    cli::cli_alert_warning("Missing: {tif_path}")
    return(tibble::tibble())
  }

  r <- terra::rast(tif_path)

  # Aggregate 5 arc-min to target_res (0.5 deg -> factor = 6)
  agg_factor <- as.integer(target_res / (5 / 60))
  r_agg <- terra::aggregate(
    r, fact = agg_factor, fun = "mean", na.rm = TRUE
  )

  .raster_to_tibble(r_agg, "harvest_fraction") |>
    dplyr::filter(
      !is.na(harvest_fraction),
      harvest_fraction > 0
    ) |>
    dplyr::mutate(item_prod_code = item_prod_code)
}

.read_luh2_static <- function(luh2_dir, var_name) {
  static_path <- file.path(luh2_dir, "staticData_quarterdeg.nc")
  nc <- ncdf4::nc_open(static_path)
  on.exit(ncdf4::nc_close(nc))

  lat <- ncdf4::ncvar_get(nc, "lat")
  vals <- ncdf4::ncvar_get(nc, var_name)

  # t(vals) -> rows = lat, cols = lon. When lat is descending
  # (90 to -90), row 1 = north, which matches terra convention.
  r <- terra::rast(
    t(vals),
    extent = terra::ext(-180, 180, -90, 90)
  )
  if (lat[1] < lat[length(lat)]) {
    r <- terra::flip(r, direction = "vertical")
  }
  r
}

.read_luh2_carea <- function(luh2_dir) {
  .read_luh2_static(luh2_dir, "carea")
}

# CFT-to-LUH2 crop functional type mapping
.cft_to_luh2_mapping <- function() {
  tibble::tribble(
    ~cft_name,             ~luh2_type,
    "temperate_cereals",   "c3ann",
    "rice",                "c3ann",
    "oil_crops_rapeseed",  "c3ann",
    "oil_crops_sunflower", "c3ann",
    "oil_crops_other",     "c3ann",
    "others_annual",       "c3ann",
    "temperate_roots",     "c3ann",
    "tropical_roots",      "c3ann",
    "maize",               "c4ann",
    "tropical_cereals",    "c4ann",
    "sugarcane",           "c4ann",
    "others_perennial",    "c3per",
    "oil_crops_soybean",   "c3nfx",
    "oil_crops_groundnut", "c3nfx",
    "pulses",              "c3nfx"
  )
}

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
      nc, vname,
      start = c(1, 1, time_idx),
      count = c(n_lon, n_lat, 1)
    )
    r <- terra::rast(
      t(vals),
      extent = terra::ext(-180, 180, -90, 90)
    )
    if (!lat_desc) {
      r <- terra::flip(r, direction = "vertical")
    }
    r[is.nan(terra::values(r))] <- 0
    r[is.na(terra::values(r))] <- 0
    r
  }) |>
    rlang::set_names(var_names)
}

.read_luh2_year <- function(luh2_dir, yr, crop_vars,
                            carea_rast, target_res) {
  time_idx <- yr - 850L + 1L

  states_path <- file.path(luh2_dir, "states.nc")
  mgmt_path <- file.path(luh2_dir, "management.nc")
  irrig_vars <- paste0("irrig_", crop_vars)

  crop_rasters <- .read_luh2_variables(
    states_path, crop_vars, time_idx
  )
  irrig_rasters <- .read_luh2_variables(
    mgmt_path, irrig_vars, time_idx
  )

  # Total cropland = sum of crop fractions * cell area
  cropland_frac <- Reduce(`+`, crop_rasters)
  irrig_frac <- Reduce(`+`, purrr::map2(
    irrig_rasters, crop_rasters,
    \(irrig, crop) irrig * crop
  ))

  # Convert fractions to hectares (carea km^2 * 100 = ha)
  cropland_ha <- cropland_frac * carea_rast * 100
  irrigated_ha <- irrig_frac * carea_rast * 100

  # Aggregate 0.25 deg -> target_res
  agg_factor <- as.integer(target_res / 0.25)
  cropland_ha <- terra::aggregate(
    cropland_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
  )
  irrigated_ha <- terra::aggregate(
    irrigated_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
  )

  cropland_tbl <- .raster_to_tibble(cropland_ha, "cropland_ha")
  irrigated_tbl <- .raster_to_tibble(irrigated_ha, "irrigated_ha")

  cropland_tbl |>
    dplyr::left_join(irrigated_tbl, by = c("lon", "lat")) |>
    dplyr::filter(!is.na(cropland_ha), cropland_ha > 0) |>
    dplyr::mutate(
      year = yr,
      irrigated_ha = dplyr::if_else(
        is.na(irrigated_ha), 0, irrigated_ha
      )
    )
}

# ==== Preparation functions ===========================================

# ---- 1. Country grid ------------------------------------------------
# Rasterize NaturalEarth to a 0.5-degree grid with WHEP area_code.

prepare_country_grid <- function(l_files_dir, target_res) {
  cli::cli_h1("Preparing country grid")

  shp_path <- file.path(
    l_files_dir, "NaturalEarth", "Countries_shape",
    "ne_10m_admin_0_countries.shp"
  )
  countries <- terra::vect(shp_path)

  polities <- whep::polities
  ne_data <- tibble::tibble(
    iso3c = as.character(countries$ISO_A3)
  ) |>
    dplyr::left_join(
      dplyr::select(polities, "iso3c", "area_code"),
      by = "iso3c"
    )

  countries$area_code <- ne_data$area_code

  ref <- terra::rast(
    resolution = target_res,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90
  )

  cli::cli_alert_info(
    "Rasterizing shapefile to {target_res}-degree grid"
  )
  grid_rast <- terra::rasterize(
    countries, ref, field = "area_code"
  )

  .raster_to_tibble(grid_rast, "area_code") |>
    dplyr::filter(!is.na(area_code)) |>
    dplyr::mutate(area_code = as.integer(area_code))
}

# ---- 2. Country areas (FAOSTAT + LUH2 irrigation) ------------------
# Read FAOSTAT production CSV (element 5312: Area harvested) and
# derive per-crop irrigated area from LUH2 management layer.

.luh2_country_irrig <- function(
  luh2_dir,
  year_range,
  country_grid,
  target_res
) {
  cli::cli_alert_info(
    "Computing country-level irrigation from LUH2"
  )

  crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")
  irrig_vars <- paste0("irrig_", crop_vars)
  states_path <- file.path(luh2_dir, "states.nc")
  mgmt_path <- file.path(luh2_dir, "management.nc")

  # Cell area in hectares (km2 * 100) at 0.25 deg
  carea_ha_r <- .read_luh2_carea(luh2_dir) * 100
  agg_factor <- as.integer(target_res / 0.25)

  purrr::map(year_range, \(yr) {
    if (yr %% 10 == 0) {
      cli::cli_alert("LUH2 irrigation: year {yr}")
    }
    time_idx <- yr - 850L + 1L

    crop_r <- .read_luh2_variables(
      states_path, crop_vars, time_idx
    )
    irrig_r <- .read_luh2_variables(
      mgmt_path, irrig_vars, time_idx
    )

    # Per-type irrigated ha at 0.25deg, aggregate to target_res
    purrr::map(crop_vars, \(cv) {
      iv <- paste0("irrig_", cv)
      irrig_ha_r <- irrig_r[[iv]] * crop_r[[cv]] * carea_ha_r
      irrig_agg <- terra::aggregate(
        irrig_ha_r, fact = agg_factor,
        fun = "sum", na.rm = TRUE
      )
      .raster_to_tibble(irrig_agg, "irrig_ha") |>
        dplyr::filter(
          !is.na(.data$irrig_ha), .data$irrig_ha > 0
        ) |>
        dplyr::mutate(luh2_type = cv)
    }) |>
      dplyr::bind_rows() |>
      # Join with NaturalEarth country grid for area_code
      dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
      dplyr::summarise(
        irrig_ha = sum(.data$irrig_ha),
        .by = c("area_code", "luh2_type")
      ) |>
      dplyr::mutate(year = yr)
  }) |>
    dplyr::bind_rows() |>
    # Merge c4per into c3per (both map to others_perennial)
    dplyr::mutate(
      luh2_type = dplyr::if_else(
        .data$luh2_type == "c4per", "c3per", .data$luh2_type
      )
    ) |>
    dplyr::summarise(
      irrig_ha = sum(.data$irrig_ha),
      .by = c("year", "area_code", "luh2_type")
    )
}

prepare_country_areas <- function(
  l_files_dir, year_range, country_grid, target_res
) {
  cli::cli_h1("Preparing FAOSTAT country areas")

  csv_path <- file.path(
    l_files_dir, "FAOSTAT",
    "Production_Crops_Livestock_E_All_Data_(Normalized).csv"
  )

  cli::cli_alert_info(
    "Reading FAOSTAT CSV (this may take a moment)"
  )
  fao_raw <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      `Area Code` = readr::col_integer(),
      `Item Code` = readr::col_integer(),
      `Element Code` = readr::col_integer(),
      Year = readr::col_integer(),
      Value = readr::col_double(),
      .default = readr::col_character()
    ),
    show_col_types = FALSE
  )

  element_area_harvested <- 5312L
  cft_mapping <- .read_cft_mapping()

  crop_areas <- fao_raw |>
    dplyr::filter(
      .data$`Element Code` == element_area_harvested,
      .data$Year %in% year_range,
      .data$`Item Code` %in% cft_mapping$item_prod_code
    ) |>
    dplyr::transmute(
      year = .data$Year,
      area_code = .data$`Area Code`,
      item_prod_code = .data$`Item Code`,
      harvested_area_ha = dplyr::if_else(
        is.na(Value), 0, Value
      )
    ) |>
    dplyr::filter(harvested_area_ha > 0)

  # Add irrigation from LUH2: per-CFT country-level irrigated
  # area distributed to individual crops by harvested-area share
  # within each country-year-CFT group.
  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  luh2_irrig <- .luh2_country_irrig(
    luh2_dir, year_range, country_grid, target_res
  )

  cft_luh2 <- .cft_to_luh2_mapping()

  crop_areas |>
    # Map each FAOSTAT item to its CFT and LUH2 type
    dplyr::left_join(
      dplyr::select(cft_mapping, item_prod_code, cft_name),
      by = "item_prod_code"
    ) |>
    dplyr::left_join(
      dplyr::select(cft_luh2, "cft_name", "luh2_type"),
      by = "cft_name"
    ) |>
    # Within each country-year-LUH2 type, compute crop share
    dplyr::mutate(
      cft_total_ha = sum(.data$harvested_area_ha),
      crop_share = .data$harvested_area_ha /
        .data$cft_total_ha,
      .by = c("year", "area_code", "luh2_type")
    ) |>
    # Join with LUH2 per-type irrigation
    dplyr::left_join(
      luh2_irrig,
      by = c("year", "area_code", "luh2_type")
    ) |>
    dplyr::mutate(
      irrig_ha = dplyr::if_else(
        is.na(.data$irrig_ha), 0, .data$irrig_ha
      ),
      irrigated_area_ha = .data$crop_share * .data$irrig_ha,
      # Cap irrigation at harvested area
      irrigated_area_ha = pmin(
        .data$irrigated_area_ha, .data$harvested_area_ha
      )
    ) |>
    dplyr::select(
      year, area_code, item_prod_code,
      harvested_area_ha, irrigated_area_ha
    )
}

# ---- 3. Crop patterns (EarthStat / Monfreda) ------------------------
# Read HarvestedAreaFraction GeoTIFFs, aggregate to 0.5 degrees.

prepare_crop_patterns <- function(l_files_dir, target_res) {
  cli::cli_h1("Preparing EarthStat crop patterns")

  earthstat_dir <- file.path(
    l_files_dir,
    "HarvestedAreaYield175Crops_Geotiff", "GeoTiff"
  )
  xwalk <- .read_earthstat_mapping() |>
    dplyr::filter(!is.na(item_prod_code))

  cli::cli_alert_info("Processing {nrow(xwalk)} crops from EarthStat")

  purrr::imap(
    rlang::set_names(
      xwalk$earthstat_name, xwalk$earthstat_name
    ),
    \(crop_name, idx) {
      code <- xwalk$item_prod_code[
        xwalk$earthstat_name == crop_name
      ]
      .read_one_earthstat_crop(
        earthstat_dir, crop_name, code, target_res
      )
    }
  ) |>
    dplyr::bind_rows() |>
    # Multiple EarthStat crops can map to the same
    # item_prod_code. Sum their fractions.
    dplyr::summarise(
      harvest_fraction = sum(harvest_fraction),
      .by = c(lon, lat, item_prod_code)
    )
}

# ---- 4. Gridded cropland (LUH2) -------------------------------------
# Read LUH2 crop fractions + irrigation, convert to hectares.

prepare_gridded_cropland <- function(l_files_dir, year_range,
                                     target_res) {
  cli::cli_h1("Preparing LUH2 gridded cropland")


  luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
  carea_rast <- .read_luh2_carea(luh2_dir)

  crop_vars <- c("c3ann", "c4ann", "c3per", "c4per", "c3nfx")

  cli::cli_alert_info("Processing {length(year_range)} years of LUH2 data")

  purrr::map(year_range, \(yr) {
    if (yr %% 10 == 0) {
      cli::cli_alert("Processing year {yr}")
    }
    .read_luh2_year(
      luh2_dir, yr, crop_vars, carea_rast, target_res
    )
  }) |>
    dplyr::bind_rows()
}

# ==== Main execution ==================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
output_dir <- file.path(l_files_dir, "whep", "inputs")
year_range <- 1961L:2015L
target_res <- 0.5

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cli::cli_h1("Preparing spatialize inputs")
cli::cli_alert_info("Output directory: {output_dir}")

country_grid <- prepare_country_grid(l_files_dir, target_res)
nanoparquet::write_parquet(
  country_grid,
  file.path(output_dir, "country_grid.parquet")
)
cli::cli_alert_success(
  "country_grid: {nrow(country_grid)} cells saved"
)

country_areas <- prepare_country_areas(
  l_files_dir, year_range, country_grid, target_res
)
nanoparquet::write_parquet(
  country_areas,
  file.path(output_dir, "country_areas.parquet")
)
cli::cli_alert_success(
  "country_areas: {nrow(country_areas)} rows saved"
)

crop_patterns <- prepare_crop_patterns(l_files_dir, target_res)
nanoparquet::write_parquet(
  crop_patterns,
  file.path(output_dir, "crop_patterns.parquet")
)
cli::cli_alert_success(
  "crop_patterns: {nrow(crop_patterns)} rows saved"
)

gridded_cropland <- prepare_gridded_cropland(
  l_files_dir, year_range, target_res
)
nanoparquet::write_parquet(
  gridded_cropland,
  file.path(output_dir, "gridded_cropland.parquet")
)
cli::cli_alert_success(
  "gridded_cropland: {nrow(gridded_cropland)} rows saved"
)

cli::cli_alert_success("All inputs prepared in {output_dir}")
