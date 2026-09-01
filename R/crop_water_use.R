# Per-crop, per-month applied irrigation (Module B / whep#916). The first
# builder on the cft_airrig_month output: water placed on the crop that
# received it, in the month it was received.

#' Build per-crop monthly applied irrigation from an LPJmL run.
#'
#' @description
#' Applied irrigation per cell, crop and month, from `cft_airrig_month.nc`
#' (first written by the 2026-09-01 v2 run), joined with each crop's stand
#' area fraction from `cftfrac.nc`. This is the disaggregated layer a
#' per-crop water footprint needs; the cell-level water budget stays with
#' [build_water_balance()], which reports whole-cell terms.
#'
#' Both unit conventions are returned side by side, because both are needed
#' and confusing them is the characteristic error of per-CFT data:
#' `airrig_stand_mm` is the depth applied per square metre of THE CROP'S OWN
#' STAND (an irrigation intensity), and `airrig_cell_mm` is the same water as
#' a whole-cell depth (`airrig_stand_mm * stand_frac`), which is what sums --
#' over crops it reproduces the crop-less `irrig` cube to 0.999.
#'
#' Rows are crop stands present in the cell (`stand_frac > 0`), rainfed bands
#' included: LPJmL books paddy water on *rainfed rice* (36% of the
#' stand-weighted applied total at July 2010), so dropping rainfed bands
#' drops real water.
#'
#' @param resolution `"grid"` (default, per cell) or `"polity"` (aggregated
#'   to `area_code`, `airrig_stand_mm` area-weighted by stand area,
#'   `airrig_cell_mm` by cell area).
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the run covers.
#' @param run_dir Path to the LPJmL run output directory. `NULL` (default)
#'   uses `WHEP_LPJML_RUN_DIR`.
#' @param data Named list of pre-loaded inputs, each falling back to its
#'   reader when absent: `airrig_month` (per cell, band and month,
#'   `read_lpjml_hydrology("cft_airrig_month")` output), `stand_frac` (per
#'   cell, band and year, `read_lpjml_hydrology("stand_frac", monthly =
#'   FALSE)` output) and `country_grid` (the cell-polity support).
#' @param example If `TRUE`, return a small fixture instead of reading a run.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `cell_area_frac`, `year`,
#'   `month`, `band`,
#'   `band_name`, `stand_frac`, `airrig_stand_mm` and `airrig_cell_mm` at
#'   `"grid"` resolution (aggregated over cells at `"polity"`), plus the
#'   polity columns below.
#'
#'   A border cell shared by several polities appears ONCE PER POLITY, with
#'   `cell_area_frac` carrying that polity's share of the cell. The mm
#'   columns are densities and do not split; the AREA does. Aggregating over
#'   cells therefore weights by `cell_area_frac` (times stand or cell area),
#'   and summing rows without it double-counts every border cell.
#' @inheritSection whep_polity_columns Polity columns
#' @source LPJmL run outputs `cft_airrig_month.nc` and `cftfrac.nc`; see
#'   Schaphoff, S. et al. (2018). LPJmL4 - a dynamic global vegetation model
#'   with managed land - Part 1: Model description. *Geoscientific Model
#'   Development*, 11, 1343-1375. \doi{10.5194/gmd-11-1343-2018}.
#' @export
#' @examples
#' build_crop_water_use(example = TRUE)
build_crop_water_use <- function(
  resolution = c("grid", "polity"),
  years = NULL,
  run_dir = NULL,
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_crop_water_use())
  }
  airrig <- data$airrig_month %||%
    read_lpjml_hydrology("cft_airrig_month", run_dir = run_dir, years = years)
  stand_frac <- data$stand_frac %||%
    read_lpjml_hydrology(
      "stand_frac",
      run_dir = run_dir,
      years = years,
      monthly = FALSE
    )
  country_grid <- data$country_grid %||% .carbon_cell_support()
  .cwu_assemble(airrig, stand_frac) |>
    .cwu_attach_polity(country_grid) |>
    .cwu_finalise(resolution) |>
    .add_reporting_polity_columns()
}

# Attach the cell-polity support, KEEPING cell_area_frac: unlike the grass
# builder's attachment, the polity aggregation here splits a border cell's
# water between the polities sharing it in proportion to their share of the
# cell, so the fraction is load-bearing, not decorative.
.cwu_attach_polity <- function(cells, country_grid) {
  cg <- .normalize_carbon_support(country_grid) |>
    dplyr::select("lon", "lat", "area_code", dplyr::any_of("cell_area_frac")) |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2))
  if (!rlang::has_name(cg, "cell_area_frac")) {
    cg$cell_area_frac <- 1
  }
  cells |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2)) |>
    # Deliberately many-to-many: many band-month rows per cell on the left,
    # and a border cell appears once per polity sharing it on the right.
    dplyr::inner_join(
      cg,
      by = c("lon", "lat"),
      relationship = "many-to-many"
    )
}

# Join the monthly irrigation onto the annual stand fractions and derive both
# unit conventions. Stands absent from a cell (stand_frac 0 or missing) are
# dropped: a crop with no area can receive no water, and the v1 run's
# single-band defect aside, cft_airrig_month is zero off-stand anyway.
.cwu_assemble <- function(airrig, stand_frac) {
  .check_columns(
    airrig,
    c("lon", "lat", "year", "month", "band", "band_name", "value"),
    "data$airrig_month"
  )
  .check_columns(
    stand_frac,
    c("lon", "lat", "year", "band_name", "value"),
    "data$stand_frac"
  )
  airrig |>
    dplyr::filter(is.finite(.data$value)) |>
    dplyr::rename(airrig_stand_mm = "value") |>
    dplyr::inner_join(
      stand_frac |>
        dplyr::filter(is.finite(.data$value), .data$value > 0) |>
        dplyr::select("lon", "lat", "year", "band_name", stand_frac = "value"),
      by = c("lon", "lat", "year", "band_name")
    ) |>
    dplyr::mutate(
      airrig_cell_mm = .data$airrig_stand_mm * .data$stand_frac
    )
}

# Polity aggregation: the intensity is weighted by the stand area it applies
# to, the cell depth by the cell area it is spread over. cell_area_frac from
# the support scales both, so a border cell's water is split between the
# polities sharing it exactly as its land is.
.cwu_finalise <- function(cells, resolution) {
  if (identical(resolution, "grid")) {
    return(dplyr::select(
      cells,
      "lon",
      "lat",
      "area_code",
      "cell_area_frac",
      "year",
      "month",
      "band",
      "band_name",
      "stand_frac",
      "airrig_stand_mm",
      "airrig_cell_mm"
    ))
  }
  cells |>
    dplyr::mutate(
      cell_w = dplyr::coalesce(.data$cell_area_frac, 1),
      stand_w = .data$cell_w * .data$stand_frac
    ) |>
    dplyr::summarise(
      airrig_stand_mm = stats::weighted.mean(
        .data$airrig_stand_mm,
        .data$stand_w
      ),
      airrig_cell_mm = stats::weighted.mean(.data$airrig_cell_mm, .data$cell_w),
      .by = c("area_code", "year", "month", "band", "band_name")
    )
}

# Toy fixture: one cell, one irrigated crop, watered in the summer months.
.example_crop_water_use <- function() {
  tibble::tibble(
    lon = -3.75,
    lat = 40.25,
    area_code = 203L,
    year = 2010L,
    month = 1:12,
    band = 19L,
    band_name = "irrigated maize",
    cell_area_frac = 1,
    stand_frac = 0.2,
    airrig_stand_mm = c(0, 0, 0, 0, 30, 80, 120, 90, 20, 0, 0, 0),
    airrig_cell_mm = c(0, 0, 0, 0, 6, 16, 24, 18, 4, 0, 0, 0)
  ) |>
    .add_reporting_polity_columns()
}
