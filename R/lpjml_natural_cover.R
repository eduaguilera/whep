# Natural-land soil cover from LPJmL's foliar projective cover (Module B).
# Read separately from the hydrology cube because fpc.nc is a PFT-banded
# annual output, not one of the monthly water variables.

#' Read natural-land soil cover from an LPJmL run.
#'
#' @description
#' Per-cell, per-year vegetated cover of the natural stand, from LPJmL's
#' foliar projective cover (`fpc.nc`). Band 1 of that file is the natural
#' stand FRACTION -- how much of the cell is natural -- and bands 2 to 15 are
#' the fourteen natural PFTs' projective cover WITHIN that stand. The cover of
#' natural land is therefore the sum of bands 2 to 15, capped at 1, and never
#' band 1, which is an area share and a different quantity.
#'
#' Supply the result as `data$natural_cover` to [build_carbon_balance()] to
#' replace [soc_soil_cover_curve]'s constant 0.85 for the natural class.
#'
#' @section Why it matters where it does:
#' The constant is close on the global mean -- measured cover runs 0.858 in
#' 1901 to 0.884 in 2023 -- and wrong in the distribution: the median natural
#' cell is fully covered at 1.000 while the 5th percentile is bare at roughly
#' zero. The RothC plant-retainment term is `0.6 + 0.4 * (1 - cover)`, so a
#' near-bare cell moves from 0.66 to 1.00, decomposes half again as fast, and
#' loses about a third of its equilibrium carbon. Those arid cells are exactly
#' where the model sits furthest from observation. The effect on the global
#' mean is about 1.8%; per cell it spans 0.66 to 1.10 times.
#'
#' Managed grassland has no FPC band -- `fpc.nc` covers the natural stand
#' only -- so it necessarily stays on the curve.
#'
#' @param run_dir Path to the LPJmL run output directory. `NULL` (default)
#'   uses `WHEP_LPJML_RUN_DIR`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the file covers.
#' @param first_year Calendar year of the file's first time step. `NULL`
#'   (default) reads it from the file's own `time` axis.
#' @param example If `TRUE`, return a small fixture instead of reading a run.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `natural_stand_frac` and
#'   `natural_cover` (both fractions in 0-1).
#' @source Schaphoff, S. et al. (2018). LPJmL4 - a dynamic global vegetation
#'   model with managed land - Part 1: Model description. *Geoscientific Model
#'   Development*, 11, 1343-1375. \doi{10.5194/gmd-11-1343-2018}.
#' @export
#' @examples
#' read_lpjml_natural_cover(example = TRUE)
read_lpjml_natural_cover <- function(
  run_dir = NULL,
  years = NULL,
  first_year = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_lpjml_natural_cover())
  }
  rlang::check_installed("ncdf4")
  path <- file.path(.resolve_run_dir(run_dir), "fpc.nc")
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "LPJmL foliar-cover file not found: {.file {path}}.",
      i = "It is the {.val fpc} output; add it to the run's output list."
    ))
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  .fpc_natural_cover(nc, years, first_year)
}

# Band 1 is the stand fraction and bands 2..n the PFTs within it. Summing
# from band 2 is the whole point: including band 1 would add an area share to
# a cover fraction. Coexisting PFTs can push the sum marginally above 1, so it
# is capped.
.fpc_natural_cover <- function(nc, years, first_year) {
  first_year <- .lpjml_resolve_first_year(nc, first_year, "fpc.nc")
  bands <- as.character(ncdf4::ncvar_get(nc, "NamePFT"))
  if (length(bands) < 2L) {
    cli::cli_abort(
      "{.val fpc} needs a stand-fraction band plus at least one PFT band."
    )
  }
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  # Captured before the tibble: inside it, `length(lon)` would resolve to the
  # lon COLUMN already defined, not the axis, expanding the grid to 55 million
  # rows.
  nlon <- length(lon)
  nlat <- length(lat)
  var <- setdiff(
    names(nc$var),
    c("lon_bnds", "lat_bnds", "time_bnds", "NamePFT")
  )
  var <- var[which.max(purrr::map_int(var, \(v) length(nc$var[[v]]$size)))]
  steps <- nc$dim$time$len
  keep <- .fpc_year_index(years, first_year, steps)
  purrr::list_rbind(purrr::map(keep, \(i) {
    a <- ncdf4::ncvar_get(
      nc,
      var,
      start = c(1, 1, 1, i),
      count = c(nlon, nlat, length(bands), 1)
    )
    tibble::tibble(
      lon = rep(lon, times = nlat),
      lat = rep(lat, each = nlon),
      year = first_year + i - 1L,
      natural_stand_frac = as.vector(a[,, 1L]),
      natural_cover = as.vector(
        pmin(apply(a[,, -1L, drop = FALSE], c(1, 2), sum), 1)
      )
    ) |>
      dplyr::filter(is.finite(.data$natural_cover))
  }))
}

# Time indices for the requested years, refusing a year the file cannot hold
# rather than reading whatever sits at that offset.
.fpc_year_index <- function(years, first_year, steps) {
  if (is.null(years)) {
    return(seq_len(steps))
  }
  idx <- as.integer(years) - first_year + 1L
  bad <- years[idx < 1L | idx > steps]
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Year{?s} {.val {bad}} {?is/are} outside the file's coverage.",
      i = "It holds {steps} step{?s} from {first_year}."
    ))
  }
  idx
}

# Toy fixture for a runnable example: two cells, one fully vegetated and one
# near-bare, which is the contrast the constant 0.85 cannot represent.
.example_lpjml_natural_cover <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~natural_stand_frac, ~natural_cover,
    -0.25, 5.25, 2010L, 0.98, 0.99,
    12.25, 18.75, 2010L, 0.95, 0.04
  )
}
