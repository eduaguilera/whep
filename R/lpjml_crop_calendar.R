# Cropland soil cover from LPJmL's crop calendar (Module B). Read separately
# from the hydrology cube because sdate/hdate are annual per-CFT day-of-year
# outputs, not monthly water variables.

#' Read monthly cropland soil cover from an LPJmL run's crop calendar.
#'
#' @description
#' Per-cell, per-month share of a cell's cropped area that is under a growing
#' crop, derived from LPJmL's sowing (`sdate.nc`) and harvest (`hdate.nc`)
#' dates and area-weighted by `cftfrac.nc`. Supply the result as
#' `data$cropland_cover` to [build_carbon_balance()] to place the cropland
#' soil-cover season where the crops actually are.
#'
#' @section Why the temperature proxy is not good enough:
#' Without a calendar, [soc_soil_cover_curve] is anchored to each cell-year's
#' warmest month as a stand-in for peak canopy. Measured against this run at
#' 2010 over 18,548 cropland cells, the real area-weighted crop mid-season
#' falls in the warmest month in only **5.2%** of them, within one month in
#' 22.6%, and **three or more months away in 51.0%** (median absolute offset
#' three months). Winter cereals, Mediterranean systems and irrigated
#' dry-season crops all grow away from the temperature peak, so the proxy puts
#' modelled full canopy over real fallow and modelled bare soil over the real
#' crop. It is a timing error rather than a level one: the curve's annual mean
#' cover is 0.254 against the calendar's 0.343.
#'
#' @section What the calendar does not cover:
#' `sdate`/`hdate` carry 24 bands - the twelve named crops, rainfed and
#' irrigated - while `cftfrac` carries 32. The `others` and managed-grassland
#' bands have no calendar, which is **1.2% of cropped area**; cells are
#' returned with the cover of the area that does have one, and a cell whose
#' cropped area is entirely `others` yields no row rather than a guess.
#'
#' Band mapping is by NAME, never by index: only 12 of the 24 line up, because
#' `sdate` band 13 is `"irrigated temperate cereals"` where `cftfrac` band 13
#' is `"rainfed others"`.
#'
#' @param run_dir Path to the LPJmL run output directory. `NULL` (default)
#'   uses `WHEP_LPJML_RUN_DIR`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the files cover.
#' @param first_year Calendar year of the files' first time step. `NULL`
#'   (default) reads it from the file's own `time` axis.
#' @param example If `TRUE`, return a small fixture instead of reading a run.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `month` and `cropland_cover`
#'   (a fraction in 0-1).
#' @source Schaphoff, S. et al. (2018). LPJmL4 - a dynamic global vegetation
#'   model with managed land - Part 1: Model description. *Geoscientific Model
#'   Development*, 11, 1343-1375. \doi{10.5194/gmd-11-1343-2018}.
#' @export
#' @examples
#' read_lpjml_crop_cover(example = TRUE)
read_lpjml_crop_cover <- function(
  run_dir = NULL,
  years = NULL,
  first_year = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_lpjml_crop_cover())
  }
  rlang::check_installed("ncdf4")
  dir <- .resolve_run_dir(run_dir)
  paths <- .crop_cover_paths(dir)
  nc <- purrr::map(paths, ncdf4::nc_open)
  on.exit(purrr::walk(nc, ncdf4::nc_close))
  first_year <- .lpjml_resolve_first_year(nc$sdate, first_year, "sdate.nc")
  .crop_cover_build(nc, years, first_year)
}

# All three files or none: the cover is an area-weighted mean over crops, so
# the calendar without the stand fractions cannot be weighted.
.crop_cover_paths <- function(dir) {
  files <- c(sdate = "sdate.nc", hdate = "hdate.nc", cftfrac = "cftfrac.nc")
  paths <- file.path(dir, files)
  names(paths) <- names(files)
  absent <- unname(files[!file.exists(paths)])
  if (length(absent) > 0L) {
    cli::cli_abort(c(
      "Crop-calendar inputs not found in {.path {dir}}: {.file {absent}}.",
      i = "{.val sdate} and {.val hdate} were first written on 2026-08-27;
           no earlier run carries a crop calendar."
    ))
  }
  paths
}

.crop_cover_build <- function(nc, years, first_year) {
  cal_bands <- .lpjml_band_names(nc$sdate)
  cft_bands <- .lpjml_band_names(nc$cftfrac)
  band <- match(cal_bands, cft_bands)
  if (anyNA(band)) {
    cli::cli_abort(c(
      "Crop-calendar band{?s} absent from {.file cftfrac.nc}:
       {.val {cal_bands[is.na(band)]}}.",
      i = "Bands are matched by name; only 12 of 24 align by index."
    ))
  }
  lon <- ncdf4::ncvar_get(nc$sdate, "lon")
  lat <- ncdf4::ncvar_get(nc$sdate, "lat")
  keep <- .fpc_year_index(years, first_year, nc$sdate$dim$time$len)
  purrr::list_rbind(purrr::map(keep, \(i) {
    .crop_cover_year(nc, band, i, first_year + i - 1L, lon, lat)
  }))
}

# Band names of a per-CFT output.
.lpjml_band_names <- function(nc) {
  trimws(as.character(ncdf4::ncvar_get(nc, "NamePFT")))
}

# One year: accumulate covered and cropped area over the calendar's bands,
# then divide. Kept as a matrix accumulation rather than a long join because
# a 720 x 277 x 24 x 12 long table is 55 million rows before the reduction.
.crop_cover_year <- function(nc, band, index, year, lon, lat) {
  n <- c(length(lon), length(lat))
  slab <- function(h, var, b) {
    ncdf4::ncvar_get(h, var, c(1, 1, b, index), c(n[1], n[2], 1, 1))
  }
  covered <- array(0, c(n[1], n[2], 12L))
  cropped <- array(0, n)
  for (b in seq_along(band)) {
    area <- slab(nc$cftfrac, "CFTfrac", band[b])
    area[!is.finite(area) | area < 0] <- 0
    sow <- slab(nc$sdate, "sdate", b)
    harvest <- slab(nc$hdate, "hdate", b)
    live <- is.finite(sow) & is.finite(harvest) & sow > 0 & harvest > 0
    cropped <- cropped + area
    for (m in seq_len(12L)) {
      inside <- .crop_month_inside(sow, harvest, m)
      covered[,, m] <- covered[,, m] + area * (live & inside)
    }
  }
  purrr::list_rbind(purrr::map(seq_len(12L), \(m) {
    tibble::tibble(
      lon = rep(lon, times = n[2]),
      lat = rep(lat, each = n[1]),
      year = as.integer(year),
      month = as.integer(m),
      cropped = as.vector(cropped),
      cropland_cover = as.vector(covered[,, m])
    )
  })) |>
    dplyr::filter(.data$cropped > 0) |>
    dplyr::mutate(cropland_cover = .data$cropland_cover / .data$cropped) |>
    dplyr::select(-"cropped")
}

# Is a month's mid-point inside the growing season?
#
# 41.7% of cells at 2010 have hdate < sdate because the crop is sown in one
# calendar year and harvested in the next, so the season wraps. Treating that
# as an empty interval would book every winter cereal as permanently bare.
# Written as logical algebra rather than with `if_else()`, which rejects a
# matrix condition: the real read passes 720 x 277 grids, while a vector of
# cells is what a unit test naturally supplies. Both work here.
.crop_month_inside <- function(sow, harvest, month) {
  day <- round((month - 0.5) * 365 / 12)
  wraps <- harvest < sow
  (!wraps & day >= sow & day <= harvest) |
    (wraps & (day >= sow | day <= harvest))
}

# Toy fixture: one cell, a summer crop covered May to September.
.example_lpjml_crop_cover <- function() {
  tibble::tibble(
    lon = 0.25,
    lat = 45.25,
    year = 2010L,
    month = 1:12,
    cropland_cover = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0)
  )
}
