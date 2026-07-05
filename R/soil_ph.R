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
#' `data$cell_polity` is supplied, the native HWSD raster is first cropped to
#' that grid's extent before reclassification (so a regional caller never
#' materialises or reclassifies the full-resolution global raster), and cells
#' present in that target grid but missing from the aggregated HWSD grid are
#' gap-filled from the nearest available neighbour; otherwise cropping and
#' gap-filling are both skipped and the returned grid covers every cell where
#' HWSD itself has data.
#'
#' @param hwsd_dir Path to the directory holding `hwsd_data.csv` and
#'   `hwsd.bil`. Defaults to `Sys.getenv("WHEP_HWSD_DIR")`.
#' @param data Optional named list of pre-loaded inputs: `cell_polity`
#'   (`lon`, `lat`, at minimum), used both to crop the HWSD raster to the
#'   region of interest before reclassification and as the target grid for
#'   gap-filling. When absent, cropping and gap-filling are both skipped
#'   (documented fallback above).
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
  soil_grid <- .aggregate_hwsd(
    dir,
    mu_soils,
    target_res = 0.5,
    target_grid = data$cell_polity,
    value_col = "t_ph_h2o",
    out_col = "soil_ph"
  )
  if (is.null(data$cell_polity)) {
    return(soil_grid)
  }
  .gapfill_soil(soil_grid, data$cell_polity)
}

#' Read gridded soil hydraulic properties from HWSD onto WHEP's grid.
#'
#' @description
#' Reads the HWSD (Harmonized World Soil Database) soil map unit attribute
#' table and raster, resolves each map unit's dominant USDA texture class,
#' looks up that class's volumetric field capacity, wilting point and porosity
#' from [soil_hydraulic_by_texture] (via the [hwsd_texture_usda] code
#' crosswalk), and aggregates each property to WHEP's 0.5-degree grid by
#' averaging the native HWSD cells inside each 0.5-degree block. These are the
#' per-cell soil hydraulic drivers the ICBM soil-carbon moisture modifier
#' consumes. Soil texture is a static HWSD property: the result has no `year`
#' column. Cropping to `data$cell_polity` follows the same regional-crop path
#' as [read_soil_ph()]; missing cells are gap-filled from the nearest
#' available neighbour when a target grid is supplied.
#'
#' @param hwsd_dir Path to the directory holding `hwsd_data.csv` and
#'   `hwsd.bil`. Defaults to `Sys.getenv("WHEP_HWSD_DIR")`.
#' @param data Optional named list of pre-loaded inputs: `cell_polity`
#'   (`lon`, `lat`, at minimum), used both to crop the HWSD raster and as the
#'   gap-filling target grid.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `t_field` (volumetric field capacity),
#'   `t_wilt` (volumetric wilting point) and `porosity`, each a fraction.
#' @export
#' @examples
#' read_soil_hydraulic(example = TRUE)
read_soil_hydraulic <- function(
  hwsd_dir = NULL,
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_soil_hydraulic())
  }
  rlang::check_installed("terra")
  dir <- .resolve_hwsd_dir(hwsd_dir)
  mu_hyd <- .read_hwsd_attributes_local(dir) |> .derive_map_unit_hydraulic()
  grid <- .aggregate_hwsd_hydraulic(dir, mu_hyd, data$cell_polity)
  if (is.null(data$cell_polity)) {
    return(grid)
  }
  .gapfill_soil_hydraulic(grid, data$cell_polity)
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

# For each map unit, pick the dominant (largest summed share) USDA texture
# class code. Shared by the pH and soil-hydraulic derivations.
.derive_dominant_texture <- function(hwsd_attr) {
  hwsd_attr |>
    dplyr::filter(!is.na(.data$t_usda_tex)) |>
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
}

# For each map unit, pick the pH of its dominant (largest-share) USDA
# texture class, defaulting missing pH to 7.0 (neutral).
.derive_dominant_soil <- function(hwsd_attr) {
  soils <- hwsd_attr |> dplyr::filter(!is.na(.data$t_usda_tex))
  dom_tex <- .derive_dominant_texture(soils)
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

# Per map unit, the volumetric field capacity, wilting point and porosity of
# its dominant USDA texture class, from soil_hydraulic_by_texture keyed via the
# hwsd_texture_usda code crosswalk. Map units whose dominant code is not in the
# crosswalk (e.g. HWSD rock/ice) drop out, so they aggregate to NA and are
# gap-filled downstream.
.derive_map_unit_hydraulic <- function(hwsd_attr) {
  hwsd_attr |>
    .derive_dominant_texture() |>
    dplyr::inner_join(whep::hwsd_texture_usda, by = "t_usda_tex") |>
    dplyr::inner_join(
      whep::soil_hydraulic_by_texture,
      by = "usda_texture_class"
    ) |>
    dplyr::transmute(
      .data$mu_global,
      t_field = .data$field_capacity,
      t_wilt = .data$wilting_point,
      .data$porosity
    )
}

# Aggregate the three per-map-unit hydraulic columns to the 0.5-degree grid,
# reusing .aggregate_hwsd() (crop -> classify -> mean-aggregate) once per
# column and joining the results on the cell key.
.aggregate_hwsd_hydraulic <- function(hwsd_dir, mu_hyd, target_grid) {
  cols <- c("t_field", "t_wilt", "porosity")
  grids <- purrr::map(cols, function(col) {
    .aggregate_hwsd(
      hwsd_dir,
      mu_hyd[, c("mu_global", col)],
      target_res = 0.5,
      target_grid = target_grid,
      value_col = col,
      out_col = col
    )
  })
  purrr::reduce(grids, dplyr::inner_join, by = c("lon", "lat"))
}

# Gap-fill cells in the target grid missing from the aggregated hydraulic grid
# from the nearest available neighbour, one property at a time via the shared
# .gapfill_soil() (which fills a single value column), then rejoin.
.gapfill_soil_hydraulic <- function(grid, country_grid) {
  cols <- c("t_field", "t_wilt", "porosity")
  filled <- purrr::map(cols, function(col) {
    single <- dplyr::rename(grid, soil_ph = dplyr::all_of(col))
    single <- dplyr::select(single, "lon", "lat", "soil_ph")
    .gapfill_soil(single, country_grid) |>
      dplyr::rename("{col}" := "soil_ph")
  })
  purrr::reduce(filled, dplyr::inner_join, by = c("lon", "lat"))
}

# Reclassify the HWSD map-unit raster to a per-cell numeric attribute
# (`value_col` of `mu_soils`) and spatially aggregate to WHEP's 0.5-degree
# grid (mean of native cells per block), returning it as `out_col`. When
# `target_grid` (a `lon`/`lat` tibble) is supplied, the raster is first
# cropped to that grid's bounding box (padded half a target cell on each
# side) BEFORE the expensive terra::classify(), so a regional caller never
# materialises or reclassifies the full-resolution global HWSD raster (which
# otherwise exhausts memory and crashes the R session). Shared by the soil-pH
# and soil-hydraulic-property readers.
.aggregate_hwsd <- function(
  hwsd_dir,
  mu_soils,
  target_res,
  target_grid = NULL,
  value_col = "t_ph_h2o",
  out_col = "soil_ph"
) {
  hwsd_path <- file.path(hwsd_dir, "hwsd.bil")
  if (!file.exists(hwsd_path)) {
    cli::cli_abort("HWSD raster not found at {.file {hwsd_path}}.")
  }
  hwsd_rast <- .crop_to_target(terra::rast(hwsd_path), target_grid, target_res)
  agg_factor <- as.integer(target_res / terra::res(hwsd_rast)[1])

  rcl <- as.matrix(mu_soils[, c("mu_global", value_col)])
  val_rast <- terra::classify(hwsd_rast, rcl, others = NA)
  val_coarse <- terra::aggregate(
    val_rast,
    fact = agg_factor,
    fun = "mean",
    na.rm = TRUE
  )

  val_df <- terra::as.data.frame(val_coarse, xy = TRUE, na.rm = TRUE)
  names(val_df) <- c("lon", "lat", out_col)
  tibble::as_tibble(val_df) |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      "{out_col}" := round(.data[[out_col]], 2)
    )
}

# Crop the native HWSD raster to a target grid's bounding box, padded half a
# target cell on each side. Returns the raster unchanged when no target grid
# is supplied (the documented global path).
.crop_to_target <- function(rast, target_grid, target_res) {
  if (is.null(target_grid)) {
    return(rast)
  }
  pad <- target_res / 2
  extent <- terra::ext(
    min(target_grid$lon) - pad,
    max(target_grid$lon) + pad,
    min(target_grid$lat) - pad,
    max(target_grid$lat) + pad
  )
  terra::crop(rast, extent)
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

# Toy fixture for a runnable example (one cell, loam-class hydraulics).
.example_soil_hydraulic <- function() {
  tibble::tribble(
    ~lon, ~lat, ~t_field, ~t_wilt, ~porosity,
    -0.25, -0.25, 0.29, 0.14, 0.43
  )
}
