# Gridded critical-nitrogen boundary exceedance, surplus mode (SJOS-N Module 2,
# Task 2.2, Mode A). Compares each crop's per-hectare nitrogen surplus (or
# per-hectare nitrogen input) in a grid cell to that cell's per-hectare
# critical value from Schulte-Uebbing et al. (2022), decomposing the crop's
# nitrogen into the part within the boundary and the part exceeding it. The
# comparison is per crop (item_cbs_code): the cell's single critical value
# broadcasts to every crop sharing the cell (locked plan decision 14), a
# documented per-cell environmental-limit simplification. By construction
# exceedance + within_boundary == actual (conservation), with the exceedance
# share in [0, 1]. resolution = "grid" retains the per-crop table the footprint
# (Module 4) needs; polity / country / image_region sum the mass terms over
# cells as additional aggregate outputs, never replacing the per-crop grid
# table.

#' Build the surplus-mode critical-nitrogen boundary exceedance.
#'
#' @description
#' Compares a [calculate_n_surplus()] output against a
#' [read_critical_n()] critical-nitrogen layer, per grid cell and per crop.
#' For `metric = "surplus"` each crop's per-hectare surplus (`surplus_kgn_ha`)
#' is compared to the cell's critical nitrogen surplus; for `metric = "input"`
#' each crop's per-hectare nitrogen input (`n_input_std_t` per `area_ha`) is
#' compared to the cell's critical nitrogen input. The cell's single critical
#' value broadcasts to every crop sharing the cell. Each crop's nitrogen is
#' split into an `exceedance` part (above the boundary) and a `within_boundary`
#' part, as both a per-hectare intensity and a mass, with the exceedance share
#' `exceed_share = (actual - critical) / actual` when the crop is above the
#' critical value and `0` otherwise (and `0` when the crop is at or below zero).
#' `exceedance + within_boundary == actual` holds by construction.
#' `resolution = "grid"` keeps the full per-crop grid key; `"polity"` and
#' `"country"` sum the mass terms over cells to `area_code`, `item_cbs_code`,
#' `year`; `"image_region"` sums to IMAGE region when `cell_polity` supplies an
#' `image_region` column, else falls back to the polity aggregate with a note.
#'
#' @param surplus A [calculate_n_surplus()] output keyed by `lon`, `lat`,
#'   `area_code`, `item_cbs_code`, `year`, carrying `area_ha` and, for
#'   `metric = "surplus"`, `surplus_kgn_ha`, or for `metric = "input"`,
#'   `n_input_std_t`.
#' @param critical A [read_critical_n()] output (`lon`, `lat`, `value` in
#'   kg N per hectare), the critical nitrogen surplus for `metric = "surplus"`
#'   or the critical nitrogen input for `metric = "input"`.
#' @param land_use Land-use scope the `critical` layer was read for, `"all"`
#'   (default) or `"ara"`; a provenance stamp on the output.
#' @param resolution Output grain: `"grid"` (default, per crop per cell),
#'   `"polity"` or `"country"` (per crop per country) or `"image_region"`.
#' @param metric Which pressure to compare: `"surplus"` (default, per-hectare
#'   surplus vs critical surplus) or `"input"` (per-hectare nitrogen input vs
#'   critical input).
#' @param cell_polity Optional crosswalk carrying `area_code` and
#'   `image_region`, used only for `resolution = "image_region"`. Defaults to
#'   `NULL`.
#' @param example If `TRUE`, return a small fixture instead of computing.
#'   Defaults to `FALSE`.
#' @return For `resolution = "grid"`, a tibble keyed `lon`, `lat`, `area_code`,
#'   `item_cbs_code`, `year` with `area_ha`, `critical_kgn_ha`,
#'   `actual_kgn_ha`, `exceed_share`, `exceedance_kgn_ha`,
#'   `within_boundary_kgn_ha`, the mass terms `exceedance_n_t`,
#'   `within_boundary_n_t`, `actual_n_t`, and the `metric`, `land_use`,
#'   `method_boundary` stamps. For the aggregate resolutions, the grouping key
#'   with the summed mass terms and the same stamps.
#' @export
#' @examples
#' build_n_boundary_exceedance(example = TRUE)
build_n_boundary_exceedance <- function(
  surplus,
  critical,
  land_use = c("all", "ara"),
  resolution = c("grid", "polity", "country", "image_region"),
  metric = c("surplus", "input"),
  cell_polity = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_boundary_exceedance())
  }
  land_use <- rlang::arg_match(land_use)
  resolution <- rlang::arg_match(resolution)
  metric <- rlang::arg_match(metric)
  .check_columns(surplus, .nbx_surplus_required(metric), "surplus")
  .check_columns(critical, c("lon", "lat", "value"), "critical")
  surplus |>
    .nbx_grid(critical, metric, land_use) |>
    .nbx_resolve(resolution, cell_polity)
}

# ---- Private helpers -------------------------------------------------------

# Columns the surplus input must carry for each metric.
.nbx_surplus_required <- function(metric) {
  base <- c("lon", "lat", "area_code", "item_cbs_code", "year", "area_ha")
  extra <- if (metric == "input") "n_input_std_t" else "surplus_kgn_ha"
  c(base, extra)
}

# Per-crop per-cell decomposition: join the cell's critical value to every
# crop in the cell, derive the actual per-hectare pressure, split it into the
# exceedance and within-boundary parts (intensity and mass), and stamp the
# provenance.
.nbx_grid <- function(surplus, critical, metric, land_use) {
  crit <- dplyr::transmute(
    critical,
    lon = .data$lon,
    lat = .data$lat,
    critical_kgn_ha = .data$value
  )
  surplus |>
    dplyr::inner_join(crit, by = c("lon", "lat")) |>
    .nbx_actual(metric) |>
    .nbx_decompose() |>
    .nbx_stamp(metric, land_use)
}

# The per-hectare pressure being compared: the surplus rate directly, or the
# nitrogen input converted to a rate from its mass and harvested area.
.nbx_actual <- function(joined, metric) {
  if (metric == "input") {
    return(dplyr::mutate(
      joined,
      actual_kgn_ha = dplyr::if_else(
        .data$area_ha > 0,
        .data$n_input_std_t * 1000 / .data$area_ha,
        NA_real_
      )
    ))
  }
  dplyr::mutate(joined, actual_kgn_ha = .data$surplus_kgn_ha)
}

# Split each crop's per-hectare nitrogen into the exceedance and
# within-boundary parts (intensity and mass). exceed_share is 0 when the crop
# is at or below zero or below the critical value, else (actual - critical) /
# actual, so exceedance + within_boundary == actual.
.nbx_decompose <- function(x) {
  dplyr::mutate(
    x,
    exceed_share = dplyr::if_else(
      .data$actual_kgn_ha <= 0 | .data$actual_kgn_ha < .data$critical_kgn_ha,
      0,
      (.data$actual_kgn_ha - .data$critical_kgn_ha) / .data$actual_kgn_ha
    ),
    exceedance_kgn_ha = .data$actual_kgn_ha * .data$exceed_share,
    within_boundary_kgn_ha = .data$actual_kgn_ha * (1 - .data$exceed_share),
    exceedance_n_t = .data$exceedance_kgn_ha * .data$area_ha / 1000,
    within_boundary_n_t = .data$within_boundary_kgn_ha * .data$area_ha / 1000,
    actual_n_t = .data$actual_kgn_ha * .data$area_ha / 1000
  )
}

# Stamp the metric, land-use scope and boundary mode.
.nbx_stamp <- function(x, metric, land_use) {
  dplyr::mutate(
    x,
    metric = metric,
    land_use = land_use,
    method_boundary = "surplus"
  )
}

# Return the requested resolution: the per-crop grid table, the polity/country
# aggregate (identical grouping here), or the IMAGE-region aggregate.
.nbx_resolve <- function(grid, resolution, cell_polity) {
  if (resolution == "grid") {
    return(.nbx_grid_cols(grid))
  }
  if (resolution == "image_region") {
    return(.nbx_image_region(grid, cell_polity))
  }
  .nbx_aggregate(grid, c("area_code", "item_cbs_code", "year"))
}

# The per-crop grid table Module 4 consumes.
.nbx_grid_cols <- function(grid) {
  dplyr::select(
    grid,
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "area_ha",
    "critical_kgn_ha",
    "actual_kgn_ha",
    "exceed_share",
    "exceedance_kgn_ha",
    "within_boundary_kgn_ha",
    "exceedance_n_t",
    "within_boundary_n_t",
    "actual_n_t",
    "metric",
    "land_use",
    "method_boundary"
  )
}

# Sum the mass terms over cells to the requested key, carrying the constant
# provenance stamps.
.nbx_aggregate <- function(grid, key) {
  dplyr::summarise(
    grid,
    exceedance_n_t = sum(.data$exceedance_n_t, na.rm = TRUE),
    within_boundary_n_t = sum(.data$within_boundary_n_t, na.rm = TRUE),
    actual_n_t = sum(.data$actual_n_t, na.rm = TRUE),
    metric = dplyr::first(.data$metric),
    land_use = dplyr::first(.data$land_use),
    method_boundary = dplyr::first(.data$method_boundary),
    .by = dplyr::all_of(key)
  )
}

# Aggregate to IMAGE region when cell_polity carries the crosswalk, else fall
# back to the polity aggregate with a note (the area_code -> IMAGE-region map
# is not available in this environment).
.nbx_image_region <- function(grid, cell_polity) {
  if (is.null(cell_polity) || !rlang::has_name(cell_polity, "image_region")) {
    cli::cli_warn(c(
      "!" = "No IMAGE-region crosswalk available; returning the polity
             aggregate instead.",
      "i" = "Supply {.arg cell_polity} with an {.field image_region} column to
             aggregate by IMAGE region."
    ))
    return(.nbx_aggregate(grid, c("area_code", "item_cbs_code", "year")))
  }
  map <- dplyr::distinct(cell_polity, .data$area_code, .data$image_region)
  grid |>
    dplyr::left_join(map, by = "area_code") |>
    .nbx_aggregate(c("image_region", "item_cbs_code", "year"))
}
