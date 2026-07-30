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
#'   or the critical nitrogen input for `metric = "input"`. Every retained
#'   positive-area cell must have a non-missing critical value; incomplete
#'   coverage aborts instead of silently dropping the cell.
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
#'   `within_boundary_n_t`, `actual_n_t`, `production_n_t` (the harvest-removal
#'   nitrogen the footprint's `"production"` category traces, carried through
#'   only when the `surplus` input supplies it), and the `metric`, `land_use`,
#'   `method_boundary` stamps. For the aggregate resolutions, the grouping key
#'   with the summed mass terms and the same stamps.
#' @export
#' @examples
#' build_n_boundary_exceedance(example = TRUE)
build_n_boundary_exceedance <- function(
  surplus,
  critical,
  land_use = c("ara", "all"),
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
  .check_columns(
    critical,
    c(
      "lon",
      "lat",
      "value",
      "critical_var",
      "critical_land_use"
    ),
    "critical"
  )
  .nbx_validate_critical(critical, metric, land_use)
  surplus |>
    .nbx_filter_land_use(land_use) |>
    .nbx_grid(critical, metric, land_use) |>
    .nbx_resolve(resolution, cell_polity)
}

# Fail at the comparison boundary when the supplied critical layer does not
# match the requested pressure or land-use scope. Provenance is part of the
# data contract, not a caller-supplied label.
.nbx_validate_critical <- function(critical, metric, land_use) {
  expected_var <- if (metric == "input") {
    "critical_n_input"
  } else {
    "critical_n_surplus"
  }
  vars <- unique(critical$critical_var[!is.na(critical$critical_var)])
  scopes <- unique(
    critical$critical_land_use[!is.na(critical$critical_land_use)]
  )
  if (!identical(vars, expected_var)) {
    cli::cli_abort(c(
      "The critical layer does not match {.arg metric = {metric}}.",
      i = "Expected {.val {expected_var}}; found {.val {vars}}."
    ))
  }
  if (!identical(scopes, land_use)) {
    cli::cli_abort(c(
      "The critical layer does not match {.arg land_use = {land_use}}.",
      i = "Found critical land-use scope {.val {scopes}}."
    ))
  }
  invisible(critical)
}

# ---- Private helpers -------------------------------------------------------

# Columns the surplus input must carry for each metric.
.nbx_surplus_required <- function(metric) {
  base <- c("lon", "lat", "area_code", "item_cbs_code", "year", "area_ha")
  extra <- if (metric == "input") "n_input_std_t" else "surplus_kgn_ha"
  c(base, extra)
}

# Apply the land-use selector to the WHEP item key rather than merely stamping
# it on the result. CBS 3000/3002/3003 are grass items. The robust historical
# default is arable/crop land only; `all` retains all WHEP grassland as an
# explicit sensitivity and must not be interpreted as a measured
# intensive-grassland reconstruction.
.nbx_filter_land_use <- function(surplus, land_use) {
  grass <- c(3000L, 3002L, 3003L)
  attributed <- dplyr::filter(surplus, !is.na(.data$item_cbs_code))
  if (land_use == "ara") {
    return(dplyr::filter(
      attributed,
      !.data$item_cbs_code %in% grass
    ))
  }
  attributed
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
    .n_join_critical_complete(
      crit,
      value_col = "critical_kgn_ha",
      source = "critical"
    ) |>
    .nbx_actual(metric) |>
    .nbx_decompose() |>
    .nbx_stamp(metric, land_use)
}

# Attach one critical layer without losing unmatched rows. A missing critical
# value is fatal for rows with positive agricultural area because their
# boundary split cannot be computed. Zero/non-positive-area rows are retained
# with NA boundary results: they carry no evaluable per-hectare pressure, but
# must not disappear merely because they fall outside the critical raster.
.n_join_critical_complete <- function(x, critical, value_col, source) {
  joined <- dplyr::left_join(
    x,
    critical,
    by = c("lon", "lat"),
    relationship = "many-to-one"
  )
  uncovered <- is.finite(joined$area_ha) &
    joined$area_ha > 0 &
    is.na(joined[[value_col]])
  if (!any(uncovered)) {
    return(joined)
  }
  cells <- joined[uncovered, c("lon", "lat"), drop = FALSE] |>
    dplyr::distinct()
  first_cells <- utils::head(
    sprintf("(%s, %s)", cells$lon, cells$lat),
    5L
  )
  missing_message <- sprintf(
    "%s positive-area row(s) in %s cell(s) lack a non-missing value from %s.",
    sum(uncovered),
    nrow(cells),
    source
  )
  cells_message <- sprintf(
    "First uncovered cell(s): %s.",
    paste(first_cells, collapse = ", ")
  )
  cli::cli_abort(c(
    "Critical-layer coverage is incomplete.",
    "x" = missing_message,
    "i" = cells_message
  ))
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

# Shared exceed-share formula for both boundary modes: the surplus mode (Mode
# A, this file) and the pathway mode (Mode B, R/n_pathway_boundary.R). The
# fraction of a per-hectare pressure that lies above the critical value: 0 when
# the pressure is at or below zero or below the critical value, else
# (actual - critical) / actual, so (actual * share) and (actual * (1 - share))
# partition the pressure exactly (conservation). Kept in one place so the two
# modes cannot drift to subtly different formulas.
#
# The share is CLAMPED to [0, 1]. A critical value can be NEGATIVE in the real
# Schulte-Uebbing gridded critical N surplus (1796 of 28881 agricultural cells,
# 6.2%, down to -396 kg N/ha): those cells are so sensitive that the safe
# surplus is a net removal, so no positive surplus at all is tolerable. Without
# the clamp, (actual - critical)/actual exceeds 1 there, which would make
# exceedance larger than the pressure itself and drive within_boundary
# NEGATIVE, breaking the decomposition. Clamping assigns the whole pressure to
# exceedance (share 1, within 0), which is the correct reading: none of it is
# within the boundary. The overshoot MAGNITUDE beyond a negative critical is
# actual - critical, a different quantity from this within/exceedance split.
.n_exceed_split <- function(actual, critical) {
  raw <- dplyr::if_else(
    actual <= 0 | actual < critical,
    0,
    (actual - critical) / actual
  )
  pmin(1, pmax(0, raw))
}

# Split each crop's per-hectare nitrogen into the exceedance and
# within-boundary parts (intensity and mass) using the shared exceed-share
# formula, so exceedance + within_boundary == actual.
.nbx_decompose <- function(x) {
  dplyr::mutate(
    x,
    # A nitrogen deficit is retained on the upstream surplus table, but is not
    # a negative environmental pressure or footprint extension.
    actual_kgn_ha = pmax(.data$actual_kgn_ha, 0),
    exceed_share = .n_exceed_split(
      .data$actual_kgn_ha,
      .data$critical_kgn_ha
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
    dplyr::any_of("production_n_t"),
    "metric",
    "land_use",
    "method_boundary"
  )
}

# Sum the mass terms over cells to the requested key, carrying the constant
# provenance stamps.
.nbx_aggregate <- function(grid, key) {
  mass_cols <- intersect(
    c(
      "exceedance_n_t",
      "within_boundary_n_t",
      "actual_n_t",
      "production_n_t"
    ),
    names(grid)
  )
  dplyr::summarise(
    grid,
    dplyr::across(dplyr::all_of(mass_cols), .sum_if_any),
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
