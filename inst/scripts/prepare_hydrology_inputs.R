# -----------------------------------------------------------------------
# prepare_hydrology_inputs.R
#
# Generates hydrology-related inputs for LPJmL at 0.5-degree resolution:
#
#   1. elevation.parquet  — Median elevation (m) per cell
#      Source: WorldClim/SRTM via `geodata` package (10 arcmin, aggregated
#      to 30 arcmin with block-median). Land-only; ocean cells in the grid
#      are gap-filled using nearest-neighbour expansion.
#
#   2. reservoirs.parquet — Dam/reservoir attributes per grid cell
#      Source: GRanD v1.1 shapefile from L_files/GIS/Global dams/.
#      Each dam is assigned to the 0.5-degree cell whose centre is nearest.
#      Fields per cell: number of dams, total capacity (MCM), first
#      commissioning year, area (km²), dominant purpose.
#
#   3. lakes_rivers.parquet — Lake and river cell fractions (optional)
#      Sources (in order of preference):
#        a) GLWD v2 (Lehner et al., 2025) — 15 arcsec, 33 classes,
#           CC-BY 4.0 from Figshare. Run download_hydrology_data.R.
#        b) GLWD Level 3 raster (30 arcsec, legacy format).
#           Download from https://www.worldwildlife.org/pages/
#           global-lakes-and-wetlands-database → L_files/GLWD/glwd_3/.
#
#   4. drainage.parquet — River routing directions (optional)
#      Sources (in order of preference):
#        a) DRT 0.5-degree (Wu et al., 2012) — freely available.
#           Run download_hydrology_data.R to fetch automatically.
#        b) DDM30 (Döll & Lehner, 2002) — request from
#           https://www.uni-frankfurt.de/45218101/DDM30.
#
# The grid is taken from country_grid.parquet (58765 land cells).
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== 1. Elevation ====================================================

#' Download and aggregate elevation to 0.5 degrees.
#'
#' Uses the `geodata` package to fetch 10-arcmin WorldClim/SRTM elevation,
#' then aggregates by factor 3 (10' × 3 = 30') using block median.
#' Extracts values for the 58765-cell grid and gap-fills any NA cells
#' with expanding nearest-neighbour search.
.prepare_elevation <- function(country_grid, output_dir) {
  cli::cli_h2("Elevation")

  # -- Ensure geodata is available ---
  if (!requireNamespace("geodata", quietly = TRUE)) {
    cli::cli_alert_info("Installing {.pkg geodata} from CRAN...")
    install.packages("geodata", quiet = TRUE)
  }

  # -- Download 10-arcmin elevation (SRTM/WorldClim) ---
  cli::cli_alert("Downloading 10-arcmin elevation via {.pkg geodata}...")
  elev_10m <- geodata::elevation_global(
    res  = 10,
    path = file.path(output_dir, ".cache")
  )
  cli::cli_alert_success(paste0(
    "Source raster: ", terra::nrow(elev_10m), " x ", terra::ncol(elev_10m),
    ", res = ", paste(round(terra::res(elev_10m), 4), collapse = " x ")
  ))

  # -- Aggregate to 30 arcmin (0.5 degree) ---
  cli::cli_alert("Aggregating to 0.5-degree (factor 3, block median)...")
  elev_30m <- terra::aggregate(elev_10m, fact = 3, fun = "median", na.rm = TRUE)
  cli::cli_alert_success(
    "Aggregated raster: {terra::nrow(elev_30m)} x {terra::ncol(elev_30m)}"
  )

  # -- Extract for grid cells ---
  grid_coords <- as.matrix(country_grid[, c("lon", "lat")])
  cell_ids <- terra::cellFromXY(elev_30m, grid_coords)
  elev_vals <- terra::values(elev_30m)[cell_ids]

  n_na <- sum(is.na(elev_vals))
  cli::cli_alert_info(
    "Extracted {length(elev_vals)} cells, {n_na} with NA"
  )

  # -- Gap-fill NA cells using nearest-neighbour from raster ---
  if (n_na > 0) {
    cli::cli_alert("Gap-filling {n_na} NA cells (nearest neighbour)...")
    na_idx <- which(is.na(elev_vals))
    filled_count <- 0L

    for (i in na_idx) {
      lon_i <- grid_coords[i, 1]
      lat_i <- grid_coords[i, 2]

      # Expanding search: check cells at increasing distances
      for (radius in seq_len(50)) {
        # Get all grid cells within radius steps
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
          # Use inverse-distance weighted mean
          weights <- 1 / pmax(dists, 0.01)
          elev_vals[i] <- round(
            stats::weighted.mean(elev_vals[nearby], weights)
          )
          filled_count <- filled_count + 1L
          break
        }
      }
    }
    cli::cli_alert_success("Gap-filled {filled_count} / {n_na} cells")
  }

  # Elevation is integer in LPJmL
  result <- country_grid |>
    select(lon, lat) |>
    mutate(elevation_m = as.integer(round(elev_vals)))

  # Clamp below-sea-level to 0 to match LandInG convention
  result <- result |>
    mutate(elevation_m = pmax(elevation_m, 0L))

  nanoparquet::write_parquet(
    result,
    file.path(output_dir, "elevation.parquet")
  )
  cli::cli_alert_success(
    "elevation: {nrow(result)} cells saved ({min(result$elevation_m)} \u2013 {max(result$elevation_m)} m)"
  )
  result
}


# ==== 2. Reservoirs (GRanD) ===========================================

#' Read GRanD v1.1 dam shapefile and assign each dam to the 0.5-degree
#' grid cell whose centre is nearest. Dams with missing year or capacity
#' are flagged. Multiple dams in one cell are aggregated.
#'
#' LPJmL reservoir input fields per cell:
#'   year       — commissioning year (earliest dam in cell)
#'   capacity   — total storage capacity in million cubic metres (MCM)
#'   area       — sum of reservoir surface areas (km²)
#'   inst_cap   — installed hydroelectric capacity (MW, if available)
#'   purpose    — 1 = irrigation, 2 = other (from MAIN_USE)
.prepare_reservoirs <- function(country_grid, grand_dir, output_dir) {
  cli::cli_h2("Reservoirs (GRanD)")

  shp_path <- file.path(grand_dir, "GRanD_dams_v1_1.shp")
  if (!file.exists(shp_path)) {
    cli::cli_alert_warning(
      "GRanD shapefile not found at {shp_path} — skipping reservoirs"
    )
    return(invisible(NULL))
  }

  # -- Read dam attributes ---
  dams_v <- terra::vect(shp_path)
  dams <- as.data.frame(dams_v) |>
    tibble::as_tibble() |>
    select(
      grand_id  = GRAND_ID,
      dam_name  = DAM_NAME,
      year      = YEAR,
      cap_mcm   = CAP_MCM,
      area_skm  = AREA_SKM,
      catch_skm = CATCH_SKM,
      main_use  = MAIN_USE,
      lon_dam   = LONG_DD,
      lat_dam   = LAT_DD
    )
  cli::cli_alert_info("GRanD: {nrow(dams)} dams read")

  # -- Clean: -99 = missing in GRanD ---
  dams <- dams |>
    mutate(
      year    = if_else(year < 0, NA_integer_, as.integer(year)),
      cap_mcm = if_else(cap_mcm < 0, NA_real_, cap_mcm),
      area_skm = if_else(area_skm < 0, NA_real_, area_skm),
      catch_skm = if_else(catch_skm < 0, NA_real_, catch_skm)
    )

  n_valid_cap <- sum(!is.na(dams$cap_mcm))
  n_valid_yr <- sum(!is.na(dams$year))
  cli::cli_alert_info(
    "Valid capacity: {n_valid_cap}, valid year: {n_valid_yr}"
  )

  # -- Assign each dam to nearest 0.5-degree cell ---
  dams <- dams |>
    mutate(
      lon = round(round(lon_dam / 0.5) * 0.5 + 0.25, 2) - 0.25,
      lat = round(round(lat_dam / 0.5) * 0.5 + 0.25, 2) - 0.25
    )
  # Snap to actual grid-cell centres: round to nearest 0.5° cell centre
  # Cell centres are at -179.75, -179.25, ..., 179.75 for lon
  dams <- dams |>
    mutate(
      lon = round(floor(lon_dam / 0.5) * 0.5 + 0.25, 2),
      lat = round(floor(lat_dam / 0.5) * 0.5 + 0.25, 2)
    )

  # -- Map purpose to LPJmL code: 1 = irrigation, 2 = other ---
  dams <- dams |>
    mutate(
      purpose_code = if_else(main_use == "Irrigation", 1L, 2L)
    )

  # -- Aggregate per 0.5-degree cell ---
  cell_reservoirs <- dams |>
    filter(!is.na(cap_mcm), cap_mcm > 0) |>
    summarise(
      n_dams          = n(),
      total_cap_mcm   = sum(cap_mcm, na.rm = TRUE),
      total_area_skm  = sum(area_skm, na.rm = TRUE),
      min_year        = min(year, na.rm = TRUE),
      mean_catch_skm  = mean(catch_skm, na.rm = TRUE),
      # Dominant purpose: pick the purpose of the dam with largest capacity
      purpose_code    = purpose_code[which.max(cap_mcm)],
      .by = c(lon, lat)
    ) |>
    # Fix Inf from min() when all years NA
    mutate(min_year = if_else(is.finite(min_year), min_year, NA_integer_))

  # -- Join to grid (keep only cells that are in our land grid) ---
  result <- country_grid |>
    select(lon, lat) |>
    left_join(cell_reservoirs, by = c("lon", "lat")) |>
    mutate(
      across(
        c(n_dams, total_cap_mcm, total_area_skm, min_year,
          mean_catch_skm, purpose_code),
        ~ replace(.x, is.na(.x), 0)
      ),
      n_dams = as.integer(n_dams),
      purpose_code = as.integer(purpose_code)
    )

  # Some dams may fall outside our land grid
  n_assigned <- sum(result$n_dams > 0)
  n_outside <- nrow(cell_reservoirs) - n_assigned
  cli::cli_alert_info(
    "Grid cells with dams: {n_assigned} ({sum(result$n_dams)} dams total)"
  )
  if (n_outside > 0) {
    cli::cli_alert_warning(
      "{n_outside} cell(s) with dams fall outside the land grid"
    )
  }

  nanoparquet::write_parquet(
    result,
    file.path(output_dir, "reservoirs.parquet")
  )
  total_cap <- round(sum(result$total_cap_mcm))
  cli::cli_alert_success(
    "reservoirs: {nrow(result)} cells saved, {n_assigned} with dams, total capacity = {total_cap} MCM"
  )
  result
}


# ==== 3. Lakes & Rivers (GLWD) ========================================

#' Process GLWD data to compute lake and river cell fractions.
#'
#' Supports two versions:
#'   - GLWD v2 (Lehner et al., 2025): 15 arcsec, 33 classes. Uses
#'     the combined dominant-type GeoTIFF. Lake classes: 1-3,
#'     River class: 7.
#'   - GLWD v1 Level 3 (legacy): 30 arcsec. Lake class: 1,
#'     River class: 3.
#'
#' For LPJmL, we need the fraction of each 0.5-degree cell covered by:
#'   - Lakes (freshwater lakes/ponds)
#'   - Rivers
#' (Reservoirs are handled separately via GRanD.)
.prepare_lakes_rivers <- function(country_grid, glwd_dir, output_dir) {
  cli::cli_h2("Lakes & Rivers (GLWD)")

  # -- Find GLWD data: prefer v2, fall back to v1 ---
  glwd_version <- NULL
  glwd_path <- NULL

  # GLWD v2: combined classes GeoTIFF
  v2_dir <- file.path(glwd_dir, "GLWD_v2")
  v2_tifs <- list.files(
    v2_dir, pattern = "dominant.*\\.tif$",
    recursive = TRUE, full.names = TRUE
  )
  if (length(v2_tifs) == 0) {
    v2_tifs <- list.files(
      v2_dir, pattern = "combined.*\\.tif$",
      recursive = TRUE, full.names = TRUE
    )
  }
  if (length(v2_tifs) > 0) {
    glwd_path <- v2_tifs[1]
    glwd_version <- "v2"
    cli::cli_alert_info("Using GLWD v2: {glwd_path}")
  }

  # GLWD v1 Level 3: ArcInfo grid or GeoTIFF
  if (is.null(glwd_path)) {
    glwd3_path <- file.path(glwd_dir, "glwd_3", "hdr.adf")
    if (!file.exists(glwd3_path)) {
      glwd3_path <- file.path(glwd_dir, "glwd_3.tif")
    }
    if (file.exists(glwd3_path)) {
      glwd_path <- glwd3_path
      glwd_version <- "v1"
      cli::cli_alert_info("Using GLWD v1 Level 3: {glwd_path}")
    }
  }

  if (is.null(glwd_path)) {
    cli::cli_alert_warning(
      "GLWD data not found at {glwd_dir} — skipping"
    )
    cli::cli_alert_info(
      "Run download_hydrology_data.R to get GLWD v2 (CC-BY, Figshare)"
    )
    return(invisible(NULL))
  }

  cli::cli_alert("Reading GLWD raster...")
  glwd <- terra::rast(glwd_path)
  src_res <- terra::res(glwd)
  agg_factor <- round(0.5 / src_res[1])
  cli::cli_alert_info(paste0(
    "GLWD resolution: ", paste(round(src_res, 5), collapse = " x "),
    ", aggregation factor: ", agg_factor
  ))

  # -- Define lake and river classes by version ---
  if (glwd_version == "v2") {
    # GLWD v2 dominant type classes:
    # 1 = Large Lake, 2 = Small Lake, 3 = Pond,
    # 4 = Reservoir, 5 = Dam, 6 = Canal,
    # 7 = River, 8-33 = wetland types
    lake_classes <- c(1, 2, 3)
    river_classes <- 7
    cli::cli_alert_info(
      "v2 lake classes: {paste(lake_classes, collapse = ',')}, ",
      "river: {paste(river_classes, collapse = ',')}"
    )
  } else {
    # GLWD v1: 1 = Lake, 2 = Reservoir, 3 = River
    lake_classes <- 1
    river_classes <- 3
  }

  # -- Create binary masks ---
  cli::cli_alert("Computing lake fraction...")
  lake_rcl <- cbind(lake_classes, rep(1, length(lake_classes)))
  lake_mask <- terra::classify(glwd, lake_rcl, othersNA = TRUE)
  lake_frac <- terra::aggregate(
    lake_mask, fact = agg_factor, fun = "mean", na.rm = TRUE
  )

  cli::cli_alert("Computing river fraction...")
  river_rcl <- cbind(river_classes, rep(1, length(river_classes)))
  river_mask <- terra::classify(glwd, river_rcl, othersNA = TRUE)
  river_frac <- terra::aggregate(
    river_mask, fact = agg_factor, fun = "mean", na.rm = TRUE
  )

  # -- Extract for grid cells ---
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
      lake_fraction  = round(replace(lake_vals, is.na(lake_vals), 0), 6),
      river_fraction = round(replace(river_vals, is.na(river_vals), 0), 6)
    )

  n_lake_cells <- sum(result$lake_fraction > 0)
  n_river_cells <- sum(result$river_fraction > 0)

  nanoparquet::write_parquet(
    result,
    file.path(output_dir, "lakes_rivers.parquet")
  )
  cli::cli_alert_success(
    "lakes_rivers: {nrow(result)} cells saved ({n_lake_cells} with lakes, {n_river_cells} with rivers)"
  )
  result
}


# ==== 4. Drainage Routing ==============================================

#' Process drainage direction map at 0.5-degree resolution.
#'
#' Flow direction encodings:
#'   - DDM30: D8 convention 1=E, 2=SE, 3=S, 4=SW, 5=W, 6=NW, 7=N,
#'     8=NE. 0 = ocean/sink, 9 = inland sink.
#'   - DRT: ArcGIS/ESRI convention powers of 2: 1=E, 2=SE, 4=S,
#'     8=SW, 16=W, 32=NW, 64=N, 128=NE. 0 = sink/undefined.
#'     Converted to DDM30 convention internally.
.prepare_drainage <- function(country_grid, drainage_paths,
                              output_dir) {
  cli::cli_h2("Drainage Routing")

  # -- Find drainage file: DDM30 or DRT ---
  drainage_path <- NULL
  drainage_type <- NULL

  for (p in drainage_paths) {
    if (file.exists(p)) {
      drainage_path <- p
      if (grepl("DRT|drt", p, ignore.case = TRUE)) {
        drainage_type <- "DRT"
      } else {
        drainage_type <- "DDM30"
      }
      break
    }
  }

  if (is.null(drainage_path)) {
    cli::cli_alert_warning(
      "No drainage direction file found — skipping"
    )
    cli::cli_alert_info(
      "Run download_hydrology_data.R to get the DRT flow direction"
    )
    return(invisible(NULL))
  }

  cli::cli_alert("Reading {drainage_type} raster: {drainage_path}")
  ddm <- terra::rast(drainage_path)
  ddm_res <- paste(round(terra::res(ddm), 4), collapse = " x ")
  cli::cli_alert_info(
    "DDM30 resolution: {ddm_res}, dims: {terra::nrow(ddm)} x {terra::ncol(ddm)}"
  )

  # -- Extract for grid cells ---
  grid_coords <- as.matrix(country_grid[, c("lon", "lat")])
  cell_ids <- terra::cellFromXY(ddm, grid_coords)
  flow_dir_vals <- terra::values(ddm)[cell_ids]

  # -- Convert DRT (ESRI powers-of-2) to DDM30 (1-8) convention ---
  if (drainage_type == "DRT") {
    cli::cli_alert("Converting DRT (ESRI) to DDM30 direction codes...")
    # ESRI: 1=E, 2=SE, 4=S, 8=SW, 16=W, 32=NW, 64=N, 128=NE
    # DDM30: 1=E, 2=SE, 3=S, 4=SW, 5=W, 6=NW, 7=N, 8=NE
    esri_to_ddm <- c(
      `1` = 1L, `2` = 2L, `4` = 3L, `8` = 4L,
      `16` = 5L, `32` = 6L, `64` = 7L, `128` = 8L
    )
    flow_dir_vals <- esri_to_ddm[
      as.character(as.integer(flow_dir_vals))
    ]
    flow_dir_vals[is.na(flow_dir_vals)] <- 0L
  }

  result <- country_grid |>
    select(lon, lat) |>
    mutate(
      flow_direction = as.integer(replace(flow_dir_vals,
                                          is.na(flow_dir_vals), 0L))
    )

  n_routed <- sum(result$flow_direction > 0 & result$flow_direction <= 8)
  n_sink <- sum(result$flow_direction == 0 | result$flow_direction == 9)

  nanoparquet::write_parquet(
    result,
    file.path(output_dir, "drainage.parquet")
  )
  cli::cli_alert_success(
    "drainage: {nrow(result)} cells saved ({n_routed} routed, {n_sink} sinks)"
  )
  result
}


# ==== Main execution ==================================================

l_files_dir <- "WHEP_L_FILES_DIR_PLACEHOLDER"
output_dir  <- file.path(l_files_dir, "whep", "inputs")
grand_dir   <- file.path(l_files_dir, "GIS", "Global dams")
glwd_dir    <- file.path(l_files_dir, "GLWD")
# Drainage: try DRT first (freely downloadable), then DDM30 (request)
drainage_paths <- c(
  file.path(l_files_dir, "DRT", "DRT_half_FDR_globe.asc"),
  file.path(l_files_dir, "DDM30", "ddm30.asc")
)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -- Read reference grid ---
country_grid <- nanoparquet::read_parquet(
  file.path(output_dir, "country_grid.parquet")
)
cli::cli_h1("Preparing hydrology inputs for LPJmL")
cli::cli_alert_info("Reference grid: {nrow(country_grid)} cells")
cli::cli_alert_info("Output directory: {output_dir}")

# -- 1. Elevation ---
elevation <- .prepare_elevation(country_grid, output_dir)

# -- 2. Reservoirs ---
reservoirs <- .prepare_reservoirs(country_grid, grand_dir, output_dir)

# -- 3. Lakes & Rivers ---
lakes_rivers <- .prepare_lakes_rivers(country_grid, glwd_dir, output_dir)

# -- 4. Drainage ---
drainage <- .prepare_drainage(country_grid, drainage_paths, output_dir)

# -- Summary ---
cli::cli_h1("Summary")
outputs <- c("elevation", "reservoirs", "lakes_rivers", "drainage")
for (out_name in outputs) {
  out_file <- file.path(output_dir, paste0(out_name, ".parquet"))
  if (file.exists(out_file)) {
    sz <- round(file.size(out_file) / 1024)
    cli::cli_alert_success("{out_name}.parquet ({sz} KB)")
  } else {
    cli::cli_alert_warning("{out_name}.parquet — not generated (see above)")
  }
}
cli::cli_alert_success("Done!")
