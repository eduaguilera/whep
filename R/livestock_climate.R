# Per-cell climate zone for the IPCC livestock emission models.
#
# CONFIRMED FACTS (checked in this repo; do not re-derive):
# - CRU TS `tmp` sits on the SAME global 0.5-degree grid the livestock
#   spatializer uses (lon -179.75..179.75, lat -89.75..89.75), so cell centres
#   match `spatialize-country-grid` exactly and no regridding is needed.
# - `.climate_zone_from_mat()` (R/manure_to_soil_coefs.R) already holds the
#   verified IPCC 2006 GL Vol.4 Ch.3 decision-tree cuts (Cool <= 10 degC,
#   Temperate 10-18 degC, Warm > 18 degC) that `climate_mcf` is keyed on. It is
#   called from here, never forked.
# - `climate_mcf` carries three zones (Cool / Temperate / Warm) plus "All".
#   whep does NOT ship the ten-zone IPCC MCF table; do not build against one.
# - Monthly CRU is never materialised: each year is reduced to a mean annual
#   temperature inside the read loop. Annual MAT is ~67k land cells x 122
#   years; the monthly table is twelve times that and is the memory cliff.

#' Build the per-cell IPCC climate zone from CRU mean annual temperature.
#'
#' @description
#' Reduce CRU TS monthly near-surface temperature to a mean annual temperature
#' (MAT) per 0.5-degree land cell and year, and classify each cell into the
#' IPCC manure-management climate zone (`"Cool"`, `"Temperate"`, `"Warm"`) that
#' [climate_mcf] is keyed on. This is the per-cell replacement for the single
#' hardcoded `"Temperate"` zone the manure CH4 model falls back to when no zone
#' is supplied.
#'
#' CRU TS covers 1901 onwards, so years outside its span cannot be measured.
#' They are not dropped and not silently given a measured year's value: the
#' 30-year climatology at the nearest end of the record is held constant across
#' them and stamped in `method_climate_zone` with its own label (for example
#' `"climatology_1901_1930"`), so a reader can tell a backcast zone from a
#' measured one without reading this code. Holding an early-record climatology
#' constant backwards is an assumption, not an observation.
#'
#' @param years Integer vector of calendar years to return. `NULL` (default)
#'   returns every year the CRU record covers. Years before the record are
#'   backcast and years after it are forward-held, both from the nearest
#'   30-year climatology.
#' @param cru_dir Path to the CRU TS NetCDF directory. Defaults to
#'   `Sys.getenv("WHEP_CRU_DIR")`; aborts when neither is set. Ignored when
#'   `data` is supplied.
#' @param data Optional pre-read tibble of annual means (`lon`, `lat`, `year`,
#'   `mean_annual_temp_c`) used in place of reading CRU, for testing. The years
#'   it carries are treated as the measured record, so the backcast and
#'   forward-hold logic still applies to `years` outside them.
#' @param example If `TRUE`, return a small fixture instead of reading CRU
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with one row per cell and year:
#' - `lon`, `lat`: Cell centre coordinates (0.5-degree grid).
#' - `year`: Calendar year.
#' - `mean_annual_temp_c`: Mean annual near-surface air temperature (degrees
#'   Celsius), the mean of the twelve monthly CRU values.
#' - `climate_zone`: IPCC zone, `"Cool"`, `"Temperate"` or `"Warm"`.
#' - `method_climate_zone`: `"cru_ts_annual"` for a measured year, or
#'   `"climatology_<first>_<last>"` for a held climatology.
#'
#' @source CRU TS (Climatic Research Unit, University of East Anglia; Harris,
#'   Osborn & Jones 2020, Scientific Data,
#'   \doi{10.1038/s41597-020-0453-3}). Zone cuts: IPCC 2006 Guidelines Vol.4
#'   Ch.3, adopted by the 2019 Refinement Vol.4 Ch.10.
#' @export
#'
#' @examples
#' build_cell_climate_zone(example = TRUE)
build_cell_climate_zone <- function(
  years = NULL,
  cru_dir = NULL,
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_cell_climate_zone())
  }
  if (is.null(data)) {
    dir <- .resolve_cru_dir(cru_dir)
    covered <- .cru_covered_years(dir)
    plan <- .climate_year_plan(.resolve_climate_years(years, covered), covered)
    annual <- .cru_annual_mat(dir, plan$read_years)
  } else {
    annual <- .check_supplied_mat(data)
    covered <- sort(unique(annual$year))
    plan <- .climate_year_plan(.resolve_climate_years(years, covered), covered)
    annual <- dplyr::filter(annual, year %in% plan$read_years)
  }
  .assemble_cell_climate(annual, plan) |>
    .stamp_climate_zone()
}

# Private helpers ----

# Calendar years present on the CRU `tmp` time axis. Metadata only: the file is
# opened and closed without reading a single data slab.
.cru_covered_years <- function(cru_dir) {
  rlang::check_installed("ncdf4")
  nc <- ncdf4::nc_open(.cru_file("tmp", cru_dir))
  on.exit(ncdf4::nc_close(nc))
  sort(unique(.cru_time_stamps(nc)$year))
}

# The years to return: everything the record covers when the caller named none.
.resolve_climate_years <- function(years, covered) {
  if (is.null(years)) {
    return(covered)
  }
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) {
    cli::cli_abort("{.arg years} must name at least one year.")
  }
  years
}

# Split the requested years into measured, backcast and forward-held sets, and
# name the climatology window each held set is taken from. A requested year that
# falls INSIDE the record's span but is absent from it is an interior hole,
# which CRU does not have: that can only mean a truncated or wrong file, so it
# aborts rather than being quietly climatologised.
.climate_year_plan <- function(years, covered) {
  measured <- intersect(years, covered)
  back <- years[years < min(covered)]
  forward <- years[years > max(covered)]
  interior <- setdiff(years, c(measured, back, forward))
  if (length(interior) > 0L) {
    cli::cli_abort(c(
      "The CRU record has no data for {length(interior)} requested
       year{?s} inside its own span: {.val {interior}}.",
      i = "Expected a continuous record over
           {.val {min(covered)}}-{.val {max(covered)}}."
    ))
  }
  back_window <- utils::head(covered, 30L)
  forward_window <- utils::tail(covered, 30L)
  list(
    measured = measured,
    back = back,
    forward = forward,
    back_window = back_window,
    forward_window = forward_window,
    read_years = sort(unique(c(
      measured,
      if (length(back) > 0L) back_window,
      if (length(forward) > 0L) forward_window
    )))
  )
}

# Mean annual temperature per land cell for each requested year, reduced inside
# the read loop so only one year of monthly slabs is ever in memory.
.cru_annual_mat <- function(cru_dir, years) {
  rlang::check_installed("ncdf4")
  nc <- ncdf4::nc_open(.cru_file("tmp", cru_dir))
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  stamps <- .cru_time_stamps(nc)
  purrr::map(
    years,
    \(yr) .cru_year_mean(nc, lon, lat, stamps, yr),
    .progress = length(years) > 5L
  ) |>
    data.table::rbindlist() |>
    tibble::as_tibble()
}

# One year: read its twelve monthly slabs, average them per cell, and keep only
# cells with a complete year. A partial cell would carry a seasonal bias into
# the annual mean and hence into the zone cut.
.cru_year_mean <- function(nc, lon, lat, stamps, yr) {
  idx <- which(stamps$year == yr)
  months <- purrr::map(
    idx,
    \(ti) .cru_slice_to_long(nc, "tmp", lon, lat, ti, yr, stamps$month[ti])
  )
  dt <- data.table::rbindlist(months)
  out <- dt[,
    list(mean_annual_temp_c = mean(value), n_months = .N),
    by = list(lon, lat)
  ]
  .drop_partial_cru_years(out, yr)
}

# Drop cells whose year is not complete, saying how many went.
.drop_partial_cru_years <- function(annual, yr) {
  partial <- sum(annual$n_months < 12L)
  if (partial > 0L) {
    cli::cli_warn(
      "Dropping {partial} cell{?s} with fewer than 12 CRU months in {yr}."
    )
  }
  annual[annual$n_months == 12L, ][, list(
    lon,
    lat,
    year = yr,
    mean_annual_temp_c
  )]
}

# Stitch the measured years together with the held climatologies.
.assemble_cell_climate <- function(annual, plan) {
  parts <- list(
    annual |>
      dplyr::filter(year %in% plan$measured) |>
      dplyr::mutate(method_climate_zone = "cru_ts_annual"),
    .held_climatology(annual, plan$back_window, plan$back),
    .held_climatology(annual, plan$forward_window, plan$forward)
  )
  dplyr::bind_rows(parts) |>
    dplyr::arrange(year, lon, lat)
}

# The per-cell mean over a climatology window, held constant across `years`.
# The label carries the window's own span so a backcast year is never confusable
# with a measured one.
.held_climatology <- function(annual, window, years) {
  if (length(years) == 0L) {
    return(NULL)
  }
  label <- paste0("climatology_", min(window), "_", max(window))
  annual |>
    dplyr::filter(year %in% window) |>
    dplyr::summarise(
      mean_annual_temp_c = mean(mean_annual_temp_c),
      .by = c(lon, lat)
    ) |>
    dplyr::cross_join(tibble::tibble(year = years)) |>
    dplyr::mutate(method_climate_zone = label)
}

# Attach the IPCC zone, reusing the verified cuts rather than restating them.
.stamp_climate_zone <- function(cells) {
  cells |>
    dplyr::mutate(climate_zone = .climate_zone_from_mat(mean_annual_temp_c)) |>
    dplyr::select(
      lon,
      lat,
      year,
      mean_annual_temp_c,
      climate_zone,
      method_climate_zone
    )
}

# Validate a caller-supplied annual-mean table before it stands in for CRU.
.check_supplied_mat <- function(data) {
  data <- tibble::as_tibble(data)
  required <- c("lon", "lat", "year", "mean_annual_temp_c")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg data} is missing required column{?s}: {.field {missing}}."
    )
  }
  if (anyNA(data$mean_annual_temp_c)) {
    cli::cli_abort(c(
      "{.arg data} carries {sum(is.na(data$mean_annual_temp_c))} missing
       {.field mean_annual_temp_c} value{?s}.",
      i = "A missing temperature would become a missing climate zone, which
           the manure CH4 model cannot resolve."
    ))
  }
  dplyr::mutate(data, year = as.integer(year))
}
