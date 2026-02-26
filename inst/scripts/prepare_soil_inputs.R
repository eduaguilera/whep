# -----------------------------------------------------------------------
# prepare_soil_inputs.R
#
# Reads the Harmonized World Soil Database (HWSD v1.21) raster and
# attribute database from L_files and aggregates soil texture class
# and pH to the 0.5-degree grid used by the spatialization pipeline.
#
# Method (following Ostberg et al. 2023, LandInG toolbox):
#   1. Read HWSD raster (30 arcsec mapping-unit IDs).
#   2. Join with SQLite attribute table to get texture class and pH
#      for the dominant soil in each mapping unit.
#   3. Aggregate to 0.5 degrees by selecting the dominant USDA
#      texture class (largest area share among 30-arcsec source
#      cells in each target cell).
#   4. Derive soil pH from the dominant soil unit belonging to the
#      dominant texture class in each 0.5-degree cell.
#   5. Gap-fill missing cells using an expanding search window with
#      inverse-distance weighting.
#   6. Save as parquet to L_files/whep/inputs/soil.parquet.
#
# Raw data expected in L_files:
#   HWSD/hwsd.bil   — HWSD raster (21600 x 43200, 16-bit, BIL)
#   HWSD/HWSD.SQLite — HWSD attribute database
#
# Output columns:
#   lon, lat          — 0.5-degree cell centres
#   soil_texture_code — USDA texture class (1-13)
#   soil_ph           — Top-soil pH (H2O)
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Private helpers =================================================

#' Read HWSD attribute data from SQLite.
#' Returns one row per mapping-unit soil-unit combination with:
#'   mu_global, issoil, share, t_usda_tex_class, t_ph_h2o, su_sym90
.read_hwsd_attributes <- function(hwsd_dir) {
  db_path <- file.path(hwsd_dir, "HWSD.SQLite")
  if (!file.exists(db_path)) {
    cli::cli_abort("HWSD SQLite database not found at {db_path}")
  }
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  DBI::dbGetQuery(db, "
    SELECT mu_global, issoil, share,
           t_sand, t_silt, t_clay,
           t_usda_tex_class, t_ph_h2o,
           su_sym90
    FROM hwsd_data
  ") |>
    tibble::as_tibble()
}

#' For each mapping unit, find the dominant soil texture class
#' and derive associated pH.
#'
#' Strategy:
#'  - Filter to actual soil units (issoil == 1, non-NA texture).
#'  - Within each MU, sum the share per texture class.
#'  - Pick the texture class with the largest combined share.
#'  - For pH: pick the soil unit with the largest individual share
#'    within the dominant texture class.
#'  - For "Rock outcrops" (su_sym90 = "RK"), "Glaciers" (GI),
#'    and "Dunes & shifting sands" (DS): treat DS as sand (13),
#'    others as non-soil by default.
.derive_dominant_soil_per_mu <- function(hwsd_attr) {
  # Handle special classes: Dunes → sand (texture code 13)
  hwsd_attr <- hwsd_attr |>
    mutate(
      t_usda_tex_class = case_when(
        su_sym90 == "DS" & is.na(t_usda_tex_class) ~ 13L,
        TRUE ~ as.integer(t_usda_tex_class)
      )
    )

  # Filter to rows with valid soil texture

  soils <- hwsd_attr |>
    filter(!is.na(t_usda_tex_class))

  # Step 1: dominant texture per MU (by total share)
  dom_tex <- soils |>
    summarise(
      tex_share = sum(share, na.rm = TRUE),
      .by = c(mu_global, t_usda_tex_class)
    ) |>
    slice_max(tex_share, n = 1, with_ties = FALSE, by = mu_global) |>
    select(mu_global, t_usda_tex_class)

  # Step 2: for pH, pick the soil unit with the largest share within
  # the dominant texture class
  ph_data <- soils |>
    inner_join(dom_tex, by = c("mu_global", "t_usda_tex_class")) |>
    slice_max(share, n = 1, with_ties = FALSE, by = mu_global) |>
    select(mu_global, t_ph_h2o)

  # Combine
  dom_tex |>
    left_join(ph_data, by = "mu_global") |>
    # Default pH for special classes (RK, GI, DS without pH)
    mutate(t_ph_h2o = if_else(is.na(t_ph_h2o), 7.0, t_ph_h2o))
}

#' Read the HWSD raster and aggregate to target resolution.
#'
#' For each 0.5-degree cell:
#'   - Count how many 30-arcsec source cells belong to each MU.
#'   - Join to dominant-soil-per-MU table.
#'   - Group by texture class and sum counts.
#'   - Pick the texture class with the maximum count.
#'   - pH is the weighted-mean pH of all MUs having the dominant
#'     texture class.
.aggregate_hwsd_to_grid <- function(hwsd_dir, mu_soils, target_res) {
  hwsd_path <- file.path(hwsd_dir, "hwsd.bil")
  if (!file.exists(hwsd_path)) {
    cli::cli_abort("HWSD raster not found at {hwsd_path}")
  }

  cli::cli_alert("Reading HWSD raster...")
  hwsd_rast <- terra::rast(hwsd_path)

  # Create target grid at 0.5 degrees
  target_rast <- terra::rast(
    resolution = target_res,
    xmin = -180, xmax = 180,
    ymin = -90, ymax = 90
  )

  cli::cli_alert("Aggregating mapping units to {target_res}-degree grid...")

  # The aggregation factor: 0.5 / (1/120) = 60
  agg_factor <- as.integer(target_res / terra::res(hwsd_rast)[1])
  cli::cli_alert_info("Aggregation factor: {agg_factor}x{agg_factor}")

  # Strategy: iterate over rows of the 0.5° grid to avoid loading

  # the entire raster into memory. Each 0.5° row contains
  # `agg_factor` source rows.
  n_target_rows <- terra::nrow(target_rast)
  n_target_cols <- terra::ncol(target_rast)

  results <- vector("list", n_target_rows)

  for (row_i in seq_len(n_target_rows)) {
    # Row of source cells corresponding to this target row
    src_row_start <- (row_i - 1L) * agg_factor + 1L
    src_row_end <- row_i * agg_factor

    # Read the strip from the HWSD raster
    strip <- terra::values(
      hwsd_rast,
      row = src_row_start,
      nrows = agg_factor
    )[, 1]

    # Reshape into a matrix [agg_factor rows x full source cols]
    strip_mat <- matrix(strip, nrow = agg_factor, byrow = TRUE)

    # Latitude of this target row centre
    lat <- 90.0 - (row_i - 0.5) * target_res

    row_results <- vector("list", n_target_cols)

    for (col_j in seq_len(n_target_cols)) {
      src_col_start <- (col_j - 1L) * agg_factor + 1L
      src_col_end <- col_j * agg_factor

      # Extract the block of source MU IDs
      block <- strip_mat[, src_col_start:src_col_end]
      mu_ids <- as.integer(block)
      mu_ids <- mu_ids[mu_ids > 0L & !is.na(mu_ids)]

      if (length(mu_ids) == 0L) next

      lon <- -180.0 + (col_j - 0.5) * target_res

      # Count occurrences of each MU
      mu_counts <- tibble::tibble(mu_global = mu_ids) |>
        count(mu_global, name = "n_cells")

      # Join to soil attributes
      mu_soil <- mu_counts |>
        inner_join(mu_soils, by = "mu_global")

      if (nrow(mu_soil) == 0L) next

      # Dominant texture: sum n_cells by texture class, pick max
      dom <- mu_soil |>
        summarise(
          total_cells = sum(n_cells),
          .by = t_usda_tex_class
        ) |>
        slice_max(total_cells, n = 1, with_ties = FALSE)

      # pH: weighted mean of MUs with dominant texture
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

#' Gap-fill cells that have land (in country_grid) but no soil data
#' from HWSD. Uses expanding search window with nearest-neighbor fill.
.gapfill_soil <- function(soil_grid, country_grid, max_search = 100L) {
  # Find cells needing gap-fill (present in country_grid but missing

  # in soil_grid)
  missing <- country_grid |>
    anti_join(soil_grid, by = c("lon", "lat"))

  if (nrow(missing) == 0) {
    cli::cli_alert_success("No gap-filling needed")
    return(soil_grid)
  }

  cli::cli_alert_info("Gap-filling {nrow(missing)} cells...")

  # Build a lookup from soil_grid
  soil_lookup <- soil_grid |>
    select(lon, lat, soil_texture_code, soil_ph)

  filled <- vector("list", nrow(missing))

  for (i in seq_len(nrow(missing))) {
    m_lon <- missing$lon[i]
    m_lat <- missing$lat[i]

    found <- FALSE
    for (radius in seq_len(max_search)) {
      # Cells within the search window
      neighbours <- soil_lookup |>
        filter(
          abs(lon - m_lon) <= radius * 0.5,
          abs(lat - m_lat) <= radius * 0.5
        )

      if (nrow(neighbours) > 0) {
        # Inverse-distance weighting
        neighbours <- neighbours |>
          mutate(
            dist = sqrt((lon - m_lon)^2 + (lat - m_lat)^2),
            dist = pmax(dist, 0.01),
            w = 1.0 / dist^2
          )

        # Dominant texture by total weight
        dom_tex <- neighbours |>
          summarise(total_w = sum(w), .by = soil_texture_code) |>
          slice_max(total_w, n = 1, with_ties = FALSE)

        # pH: weighted mean from cells with dominant texture
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
      # Assign default if nothing found (shouldn't happen often)
      filled[[i]] <- tibble::tibble(
        lon = m_lon,
        lat = m_lat,
        soil_texture_code = 0L,
        soil_ph = 7.0
      )
    }
  }

  filled_df <- bind_rows(filled)
  cli::cli_alert_success(
    "Gap-filled {nrow(filled_df)} cells ({sum(filled_df$soil_texture_code == 0)} unfilled)"
  )

  bind_rows(soil_grid, filled_df)
}

# ==== Main function ===================================================

prepare_soil <- function(l_files_dir, target_res = 0.5) {
  hwsd_dir <- file.path(l_files_dir, "HWSD")

  cli::cli_h2("Step 1: Reading HWSD attribute database")
  hwsd_attr <- .read_hwsd_attributes(hwsd_dir)
  cli::cli_alert_info(
    "{nrow(hwsd_attr)} soil records, {length(unique(hwsd_attr$mu_global))} mapping units"
  )

  cli::cli_h2("Step 2: Deriving dominant soil per mapping unit")
  mu_soils <- .derive_dominant_soil_per_mu(hwsd_attr)
  cli::cli_alert_info(
    "{nrow(mu_soils)} mapping units with valid texture"
  )

  cli::cli_h2("Step 3: Aggregating HWSD raster to {target_res}-degree grid")
  soil_grid <- .aggregate_hwsd_to_grid(hwsd_dir, mu_soils, target_res)
  cli::cli_alert_info(
    "{nrow(soil_grid)} cells with soil data"
  )

  cli::cli_h2("Step 4: Gap-filling against country_grid")
  input_dir <- file.path(l_files_dir, "whep", "inputs")
  country_grid <- nanoparquet::read_parquet(
    file.path(input_dir, "country_grid.parquet")
  )
  soil_grid <- .gapfill_soil(soil_grid, country_grid)

  # Keep only cells in the country_grid
  soil_grid <- soil_grid |>
    inner_join(
      country_grid |> select(lon, lat),
      by = c("lon", "lat")
    )

  soil_grid
}

# ==== Main execution ==================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
output_dir <- file.path(l_files_dir, "whep", "inputs")
target_res <- 0.5

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

cli::cli_h1("Preparing soil inputs from HWSD")
cli::cli_alert_info("Output directory: {output_dir}")

soil <- prepare_soil(l_files_dir, target_res)

nanoparquet::write_parquet(
  soil,
  file.path(output_dir, "soil.parquet")
)
cli::cli_alert_success(
  "soil: {nrow(soil)} cells saved to {output_dir}/soil.parquet"
)

# Summary statistics
tex_summary <- soil |>
  count(soil_texture_code) |>
  arrange(desc(n))
cli::cli_h2("Texture class distribution")
print(as.data.frame(tex_summary))
cat("\n")
cli::cli_alert_info(
  "pH range: {round(min(soil$soil_ph), 2)} - {round(max(soil$soil_ph), 2)}"
)
cli::cli_alert_success("Done!")
