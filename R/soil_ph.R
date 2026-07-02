# Gridded soil pH from HWSD (Harmonized World Soil Database), promoted from
# the tested prepare_soil_inputs() pipeline in
# inst/scripts/prepare_spatialize_all.R: the attribute reader, dominant-soil
# selector, raster aggregator and gap-filler helpers.
#
# CONFIRMED HWSD FACTS (source pipeline inspected; do not re-guess):
# - hwsd_data.csv (attribute table) holds one row per HWSD soil map unit
#   (mu_global) x texture-class share, with columns mu_global, t_usda_tex
#   (HWSD2 USDA texture code, 1-13), share (percent of the map unit covered
#   by that texture class) and t_ph_h2o (topsoil pH in water for that
#   texture class; may be NA).
# - hwsd.bil is the accompanying raster (ESRI .bil), whose cell values are
#   mu_global map-unit IDs. Soil pH is not read from the raster directly:
#   the raster is reclassified (mu_global -> pH of that unit's DOMINANT
#   texture class) and then spatially aggregated with terra.
# - Dominant texture per map unit = texture class with the largest summed
#   `share`; that class's own pH is used (not an area-weighted pH across
#   all classes). Missing pH defaults to 7.0 (neutral), matching the
#   existing pipeline's own documented fallback.
# - Soil pH is a static property of the HWSD map (no time dimension): this
#   reader returns no `year` column, the same convention used for other
#   static coefficient tables (e.g. `whep::soil_cn_ratios`).
# - Local dev data dir is read from Sys.getenv("WHEP_HWSD_DIR"); never
#   hardcode an absolute path in committed code.

#' Read gridded soil pH onto WHEP's grid.
#'
#' @description
#' Reads the HWSD (Harmonized World Soil Database) soil map unit attribute
#' table and raster, derives each map unit's pH from its dominant USDA
#' texture class, and aggregates the result to WHEP's 0.5-degree grid by
#' averaging the native HWSD cells inside each 0.5-degree block. Soil pH is
#' a static HWSD property: the result has no `year` column. When
#' `data$cell_polity` is supplied, cells present in that target grid but
#' missing from the aggregated HWSD grid are gap-filled from the nearest
#' available neighbour; otherwise gap-filling is skipped and the returned
#' grid only covers cells where HWSD itself has data.
#'
#' @param hwsd_dir Path to the directory holding `hwsd_data.csv` and
#'   `hwsd.bil`. Defaults to `Sys.getenv("WHEP_HWSD_DIR")`.
#' @param data Optional named list of pre-loaded inputs: `cell_polity`
#'   (`lon`, `lat`, at minimum), used as the target grid for gap-filling.
#'   When absent, gap-filling is skipped (documented fallback above).
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `soil_ph`.
#' @export
#' @examples
#' read_soil_ph(example = TRUE)
read_soil_ph <- function(hwsd_dir = NULL, data = list(), example = FALSE) {
  if (isTRUE(example)) {
    return(.example_soil_ph())
  }
  rlang::check_installed("terra")
  dir <- .resolve_hwsd_dir(hwsd_dir)
  mu_soils <- .read_hwsd_attributes_local(dir) |> .derive_dominant_soil()
  soil_grid <- .aggregate_hwsd(dir, mu_soils, target_res = 0.5)
  if (is.null(data$cell_polity)) {
    return(soil_grid)
  }
  .gapfill_soil(soil_grid, data$cell_polity)
}

# ---- Private helpers --------------------------------------------------

# Resolve the HWSD data directory from the argument, else the env var.
.resolve_hwsd_dir <- function(hwsd_dir) {
  resolved <- hwsd_dir %||% Sys.getenv("WHEP_HWSD_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No HWSD soil directory available.",
      i = "Pass {.arg hwsd_dir} or set {.envvar WHEP_HWSD_DIR}."
    ))
  }
  resolved
}

# Read the HWSD map-unit x texture-class attribute table.
.read_hwsd_attributes_local <- function(hwsd_dir) {
  csv_path <- file.path(hwsd_dir, "hwsd_data.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort("HWSD CSV not found at {.file {csv_path}}.")
  }
  readr::read_csv(csv_path, show_col_types = FALSE)
}

# For each map unit, pick the pH of its dominant (largest-share) USDA
# texture class, defaulting missing pH to 7.0 (neutral).
.derive_dominant_soil <- function(hwsd_attr) {
  soils <- hwsd_attr |> dplyr::filter(!is.na(.data$t_usda_tex))
  dom_tex <- soils |>
    dplyr::summarise(
      tex_share = sum(.data$share, na.rm = TRUE),
      .by = c("mu_global", "t_usda_tex")
    ) |>
    dplyr::slice_max(
      .data$tex_share,
      n = 1,
      with_ties = FALSE,
      by = "mu_global"
    ) |>
    dplyr::select("mu_global", "t_usda_tex")
  ph_data <- soils |>
    dplyr::inner_join(dom_tex, by = c("mu_global", "t_usda_tex")) |>
    dplyr::slice_max(
      .data$share,
      n = 1,
      with_ties = FALSE,
      by = "mu_global"
    ) |>
    dplyr::select("mu_global", "t_ph_h2o")
  dom_tex |>
    dplyr::left_join(ph_data, by = "mu_global") |>
    dplyr::mutate(
      t_ph_h2o = dplyr::if_else(is.na(.data$t_ph_h2o), 7.0, .data$t_ph_h2o)
    ) |>
    dplyr::select("mu_global", "t_ph_h2o")
}

# Reclassify the HWSD map-unit raster to per-cell pH and spatially
# aggregate to WHEP's 0.5-degree grid (mean of native cells per block).
.aggregate_hwsd <- function(hwsd_dir, mu_soils, target_res) {
  hwsd_path <- file.path(hwsd_dir, "hwsd.bil")
  if (!file.exists(hwsd_path)) {
    cli::cli_abort("HWSD raster not found at {.file {hwsd_path}}.")
  }
  hwsd_rast <- terra::rast(hwsd_path)
  agg_factor <- as.integer(target_res / terra::res(hwsd_rast)[1])

  rcl_ph <- as.matrix(mu_soils[, c("mu_global", "t_ph_h2o")])
  ph_rast <- terra::classify(hwsd_rast, rcl_ph, others = NA)
  ph_coarse <- terra::aggregate(
    ph_rast,
    fact = agg_factor,
    fun = "mean",
    na.rm = TRUE
  )

  ph_df <- terra::as.data.frame(ph_coarse, xy = TRUE, na.rm = TRUE)
  names(ph_df) <- c("lon", "lat", "soil_ph")
  tibble::as_tibble(ph_df) |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      soil_ph = round(.data$soil_ph, 2)
    )
}

# Gap-fill cells present in the target grid but missing from the aggregated
# HWSD grid, from the nearest available neighbour (inverse-distance-squared
# weighted mean pH), searching outward in 0.5-degree rings up to
# `max_search` rings.
.gapfill_soil <- function(soil_grid, country_grid, max_search = 100L) {
  missing <- country_grid |>
    dplyr::select("lon", "lat") |>
    dplyr::anti_join(soil_grid, by = c("lon", "lat"))
  if (nrow(missing) == 0) {
    return(soil_grid)
  }
  cli::cli_alert_info("Gap-filling {nrow(missing)} soil pH cells...")
  filled <- purrr::map2(
    missing$lon,
    missing$lat,
    .fill_soil_cell,
    soil_grid = soil_grid,
    max_search = max_search
  )
  dplyr::bind_rows(soil_grid, dplyr::bind_rows(filled))
}

# Fill one missing cell's pH from its nearest available HWSD neighbours,
# searching outward in 0.5-degree rings; falls back to pH 7.0 when nothing
# is found within `max_search` rings.
.fill_soil_cell <- function(m_lon, m_lat, soil_grid, max_search) {
  for (radius in seq_len(max_search)) {
    neighbours <- soil_grid |>
      dplyr::filter(
        abs(.data$lon - m_lon) <= radius * 0.5,
        abs(.data$lat - m_lat) <= radius * 0.5
      )
    if (nrow(neighbours) > 0) {
      ph_val <- neighbours |>
        dplyr::mutate(
          dist = pmax(
            sqrt((.data$lon - m_lon)^2 + (.data$lat - m_lat)^2),
            0.01
          ),
          w = 1.0 / .data$dist^2
        ) |>
        dplyr::summarise(
          ph = stats::weighted.mean(.data$soil_ph, w = .data$w)
        ) |>
        dplyr::pull("ph")
      return(tibble::tibble(
        lon = m_lon,
        lat = m_lat,
        soil_ph = round(ph_val, 2)
      ))
    }
  }
  tibble::tibble(lon = m_lon, lat = m_lat, soil_ph = 7.0)
}

# Toy fixture for a runnable example (one cell).
.example_soil_ph <- function() {
  tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.8
  )
}
