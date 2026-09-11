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
#' @param by `"cropland"` (default) pools every calendar band into one cover
#'   per cell and month. `"regime"` keeps the rainfed and the irrigated bands
#'   apart -- a band is irrigated when its name starts with `irrigated` --
#'   and adds `regime` and `cropped_frac`, the regime's share of the cell, so
#'   a consumer can pool the two back by area. Rainfed and irrigated stands
#'   of one crop sow and harvest on different dates; the per-regime layer is
#'   what the herbaceous rainfed and irrigated crop groups of
#'   [build_carbon_balance()] (`crop_groups = list(method = "spain_hist")`)
#'   read, each from its own bands, while plain cropland reads the pool.
#' @param example If `TRUE`, return a small fixture instead of reading a run.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `month` and `cropland_cover`
#'   (a fraction in 0-1); with `by = "regime"`, also `regime` (`"rainfed"` or
#'   `"irrigated"`) and `cropped_frac`, and up to two rows per cell-month.
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
  by = c("cropland", "regime"),
  example = FALSE
) {
  by <- rlang::arg_match(by)
  if (isTRUE(example)) {
    return(.example_lpjml_crop_cover(by))
  }
  rlang::check_installed("ncdf4")
  dir <- .resolve_run_dir(run_dir)
  paths <- .crop_cover_paths(dir)
  nc <- purrr::map(paths, ncdf4::nc_open)
  on.exit(purrr::walk(nc, ncdf4::nc_close))
  first_year <- .lpjml_resolve_first_year(nc$sdate, first_year, "sdate.nc")
  .crop_cover_build(nc, years, first_year, by)
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

.crop_cover_build <- function(nc, years, first_year, by = "cropland") {
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
  group <- if (by == "regime") {
    .crop_band_regime(cal_bands)
  } else {
    rep("cropland", length(band))
  }
  lon <- ncdf4::ncvar_get(nc$sdate, "lon")
  lat <- ncdf4::ncvar_get(nc$sdate, "lat")
  keep <- .fpc_year_index(years, first_year, nc$sdate$dim$time$len)
  purrr::list_rbind(purrr::map(keep, \(i) {
    .crop_cover_year(nc, band, group, i, first_year + i - 1L, lon, lat, by)
  }))
}

# The irrigation regime of a calendar band, from its name: LPJmL names the
# irrigated stands "irrigated <crop>" and the rainfed ones "rainfed <crop>"
# (older runs drop the rainfed prefix, so the rule is "irrigated or not").
.crop_band_regime <- function(band_names) {
  dplyr::if_else(
    stringr::str_starts(band_names, "irrigated"),
    "irrigated",
    "rainfed"
  )
}

# Band names of a per-CFT output.
.lpjml_band_names <- function(nc) {
  trimws(as.character(ncdf4::ncvar_get(nc, "NamePFT")))
}

# One year: accumulate covered and cropped area over the calendar's bands,
# then divide. Kept as a matrix accumulation rather than a long join because
# a 720 x 277 x 24 x 12 long table is 55 million rows before the reduction.
.crop_cover_year <- function(nc, band, group, index, year, lon, lat, by) {
  n <- c(length(lon), length(lat))
  slab <- .crop_cover_slab(nc, index, n)
  acc <- .crop_cover_accumulate(slab, band, group, n)
  .crop_cover_rows(acc, year, lon, lat, by)
}

# A reader of one band of one year from the three open files, so the
# accumulation below can be driven by in-memory arrays in a test.
.crop_cover_slab <- function(nc, index, n) {
  vars <- c(cftfrac = "CFTfrac", sdate = "sdate", hdate = "hdate")
  function(file, b) {
    ncdf4::ncvar_get(
      nc[[file]],
      vars[[file]],
      c(1, 1, b, index),
      c(n[1], n[2], 1, 1)
    )
  }
}

# Covered and cropped area per group of bands (one group when pooled, one per
# irrigation regime otherwise). `slab(file, b)` returns the n[1] x n[2] grid
# of band `b` of `file`; `band[b]` is the cftfrac band the b-th calendar band
# maps to, matched by name upstream.
.crop_cover_accumulate <- function(slab, band, group, n) {
  groups <- rlang::set_names(unique(group))
  covered <- purrr::map(groups, \(g) array(0, c(n[1], n[2], 12L)))
  cropped <- purrr::map(groups, \(g) array(0, n))
  for (b in seq_along(band)) {
    g <- group[b]
    area <- slab("cftfrac", band[b])
    area[!is.finite(area) | area < 0] <- 0
    sow <- slab("sdate", b)
    harvest <- slab("hdate", b)
    live <- is.finite(sow) & is.finite(harvest) & sow > 0 & harvest > 0
    cropped[[g]] <- cropped[[g]] + area
    for (m in seq_len(12L)) {
      inside <- .crop_month_inside(sow, harvest, m)
      covered[[g]][,, m] <- covered[[g]][,, m] + area * (live & inside)
    }
  }
  list(covered = covered, cropped = cropped)
}

# Long rows from the accumulation: cover = covered / cropped, cells with no
# cropped area dropped. Pooled output keeps the historical five columns; the
# per-regime output adds `regime` and `cropped_frac` (the regime's share of
# the cell) so a consumer can pool the regimes back by area.
.crop_cover_rows <- function(acc, year, lon, lat, by) {
  n <- c(length(lon), length(lat))
  rows <- purrr::list_rbind(purrr::map(names(acc$cropped), \(g) {
    purrr::list_rbind(purrr::map(seq_len(12L), \(m) {
      tibble::tibble(
        lon = rep(lon, times = n[2]),
        lat = rep(lat, each = n[1]),
        year = as.integer(year),
        month = as.integer(m),
        regime = g,
        cropped = as.vector(acc$cropped[[g]]),
        cropland_cover = as.vector(acc$covered[[g]][,, m])
      )
    }))
  })) |>
    dplyr::filter(.data$cropped > 0) |>
    dplyr::mutate(cropland_cover = .data$cropland_cover / .data$cropped)
  if (by == "regime") {
    dplyr::rename(rows, cropped_frac = "cropped")
  } else {
    dplyr::select(rows, -"regime", -"cropped")
  }
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

# Toy fixture: one cell, a summer crop covered May to September. Per regime,
# that summer crop is the rainfed stand on 30% of the cell and an irrigated
# winter crop (November to February) sits on 10%.
.example_lpjml_crop_cover <- function(by = "cropland") {
  pooled <- tibble::tibble(
    lon = 0.25,
    lat = 45.25,
    year = 2010L,
    month = 1:12,
    cropland_cover = c(0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0)
  )
  if (by != "regime") {
    return(pooled)
  }
  dplyr::bind_rows(
    dplyr::mutate(pooled, regime = "rainfed", cropped_frac = 0.3),
    dplyr::mutate(
      pooled,
      regime = "irrigated",
      cropped_frac = 0.1,
      cropland_cover = c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1)
    )
  ) |>
    dplyr::relocate("regime", "cropped_frac", .after = "month")
}
