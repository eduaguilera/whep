# LPJmL irrigated and rainfed crop yields per cell, crop and year: the ratio
# that splits a crop row's synthetic N, production N and residue removals
# between its rainfed and irrigated parts (#1233, plan decision D12).
#
# CONFIRMED LPJmL FACTS (run inspected 2026-09-24; do not re-guess):
# - pft_harvestc.nc holds var "harvestc", "harvested carbon excluding
#   residuals", gC/m2/yr, annual, 32 CFT bands named in NamePFT (the same 32
#   names, in the same order, as cftfrac.nc). It is already a density per m2
#   of the band's OWN STAND, not of the cell, so the harvest per unit crop
#   area is pft_harvestc itself and must NOT be divided by cftfrac again.
#   Measured on global_1750-2023_spinup_300_our_inputs_lpjml611_preindustrial
#   _v2 at 2010 against the cell total in harvestc.nc:
#     * managed-grassland-only cells (4,590; no crop stand, so no residue
#       term): sum over bands of pft_harvestc x cftfrac equals harvestc.nc,
#       p5 = p50 = p95 = 1.0000. The per-cell reading (plain band sum) gives
#       median 1.50 and p95 26.0 on the same cells.
#     * all 33,230 cells with harvestc.nc > 1 gC/m2: the stand-weighted sum
#       is median 0.82 of the cell total and never above it (p95 1.000),
#       because harvestc.nc is "including residuals" -- the residue LPJmL
#       removes ("fixed_residue_remove") is in the cell total and not in
#       pft_harvestc. The plain band sum is median 31x, p95 459x.
#     * within each crop band, the log-log slope of pft_harvestc on cftfrac
#       across cells is between -0.19 and +0.44 (27 bands with harvest), not
#       the 1 a per-cell density would show.
#   This is the same per-stand convention R/lpjml_npp.R verified for pft_npp
#   and R/lpjml_hydrology.R for the per-CFT water cubes.
# - A band with zero stand fraction has no yield: pft_harvestc is exactly 0
#   there (2010: 0 cells carry harvest on a zero-area band), which is "no
#   stand", not "a stand that yielded nothing". Such bands are NA here. A
#   band WITH area and zero harvest (2010: 10,924 of 405,943 band-cells, crop
#   failure or no maturity) is a real zero and stays 0.
# - Climate before 1901: the run's CRU TS 4.09 forcing starts in 1901, and
#   LPJmL recycles the 1901-1930 window (shuffled, undetrended) for
#   1750-1900 (run README "Known gaps"; config fix_climate_interval
#   1901-1930, fix_climate_shuffle true). Land use, CO2, fertiliser and
#   deposition stay historical (landuse file 1750-2023; fertiliser, manure
#   and deposition from 1851). So pre-1901 yields are real model output of a
#   real land-use state under a recycled weather year, not a held climatology
#   of yields; `method_regime_yield` says so rather than hiding it.
# - Rainfed rice is watered: LPJmL books paddy water on the RAINFED rice band
#   too (cft_nir and cft_airrig_month both carry band 2; see
#   read_lpjml_hydrology()). Its harvest is therefore a paddy harvest, and the
#   rice irrigated:rainfed ratio is compressed toward 1 relative to a truly
#   unwatered rainfed stand. Documented, not corrected: which is right for
#   upland versus lowland rice is a science question outside this reader.
# - Run dir from Sys.getenv("WHEP_LPJML_RUN_DIR"); never hardcode a path.

#' Read LPJmL irrigated and rainfed crop yields per cell.
#'
#' @description
#' Derives the harvest per unit crop area of the rainfed and the irrigated
#' stand of each crop in each 0.5-degree cell and year from a finished LPJmL
#' run, and maps the LPJmL crop functional types (CFTs) onto WHEP production
#' items. It is the yield ratio that weights the rainfed/irrigated split of
#' a crop row's synthetic nitrogen, production nitrogen and residue removals
#' toward the more productive hectares.
#'
#' The harvest is `pft_harvestc` (harvested carbon excluding residues), which
#' LPJmL already writes per square metre of the band's own stand; it is not
#' divided by the stand fraction again. The evidence is in the header of
#' `R/lpjml_regime_yield.R`: weighted by `cftfrac`, the bands reproduce the
#' cell total in `harvestc.nc` exactly on grassland-only cells, while their
#' plain sum overshoots it 1.5 to 26 times.
#'
#' @section Which crops carry a yield:
#' Only production items mapped in [cft_mapping] to one of LPJmL's twelve
#' crop-specific CFTs (temperate and tropical cereals, rice, maize, pulses,
#' temperate and tropical roots, sugarcane, and the soybean, groundnut,
#' sunflower and rapeseed oil crops) get rows: 40 items. The other 113 mapped
#' items sit on LPJmL's `"others"` catch-all stand (fruits, vegetables, nuts,
#' fibres, stimulants, oil palm, cotton...), whose yield is that of a
#' composite rather than of the crop, and items absent from [cft_mapping]
#' (fodder crops among them) have no band at all. By default neither gets a
#' row, so a join against this layer leaves them without a yield, and the
#' caller has to decide what to use instead.
#'
#' `include_others = TRUE` adds the `"others"` stand as a crop of its own,
#' expanded to the items [cft_mapping] puts on it. Its yield is the composite
#' stand's, not the item's. Plan decision D15 uses it as the year-to-year
#' anomaly source for crops without a crop-specific CFT, where only the ratio
#' of the two regimes' yields matters; see [build_regime_yield_ratio()].
#'
#' The natural key is `item_prod_code`: each production item maps to one
#' CFT, while four `item_cbs_code`s mix CFTs across their production items
#' (2520 "Cereals, Other", temperate and tropical cereals; 2534 "Roots,
#' Other", tropical roots and an unmapped item; 2537 "Sugar beet", temperate
#' roots and an unmapped item; 2605 "Vegetables, Other", where only green
#' maize has a band). `item_cbs_code` is carried for joining, not as a key.
#'
#' @section Zero-area stands:
#' A regime whose stand fraction is zero in a cell has no harvest to divide,
#' so its yield is `NA`, not zero. A cell-crop-year gets a row when at least
#' one of the two stands has area. A stand with area and no harvest (a crop
#' that failed or never matured) keeps its real zero.
#'
#' @section Years before 1901:
#' The run's climate forcing starts in 1901, and LPJmL recycles the
#' 1901-1930 window for 1750-1900 while land use and CO2 stay historical.
#' Those years are stamped `"lpjml_band_harvest_recycled_climate"` in
#' `method_regime_yield`; later years `"lpjml_band_harvest"`.
#'
#' @section Rainfed rice:
#' LPJmL waters paddy rice on the rainfed stand too (see
#' [read_lpjml_hydrology()]), so rainfed rice yields are paddy yields and the
#' rice irrigated:rainfed ratio sits closer to one than for an unwatered
#' upland stand.
#'
#' @param years Optional integer vector of calendar years. `NULL` reads every
#'   year the run's `pft_harvestc.nc` carries. A year the run does not cover
#'   aborts, naming the coverage it has.
#' @param run_dir Path to a finished LPJmL run output directory holding
#'   `pft_harvestc.nc` and `cftfrac.nc`. `NULL` (default) uses
#'   `WHEP_LPJML_RUN_DIR`. There is no pinned copy of this layer yet, so with
#'   neither the reader aborts rather than reading anything else.
#' @param data Optional list used in place of reading a run, for testing:
#'   `harvestc` (as [read_lpjml_npp()] returns it for `"harvestc"`) and
#'   `stand_frac` (as [read_lpjml_hydrology()] returns it for
#'   `"stand_frac"`).
#' @param include_others If `TRUE`, also return LPJmL's `"others"` catch-all
#'   stand, expanded to the items [cft_mapping] puts on it. Defaults to
#'   `FALSE`, the crop-specific stands only.
#' @param example If `TRUE`, return a small fixture instead of reading a
#'   run. Defaults to `FALSE`.
#' @return A tibble with one row per cell, production item and year:
#'   - `lon`, `lat`: cell centre, 0.5-degree grid.
#'   - `year`: calendar year.
#'   - `item_prod_code`, `item_cbs_code`: the WHEP production item and its
#'     commodity balance item.
#'   - `lpjml_crop`: the LPJmL CFT the item's yield comes from.
#'   - `yield_rainfed`, `yield_irrigated`: harvested carbon, excluding
#'     residues, in grams of carbon per square metre of that regime's own
#'     stand per year; `NA` where the regime has no stand in the cell.
#'   - `method_regime_yield`: how the yields were obtained (see *Years before
#'     1901*).
#' @source LPJmL 6.1.1 (Potsdam Institute for Climate Impact Research), fork
#'   `lbm364dl/LPJmL` at git hash `e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605`
#'   (as stamped in the output metadata), run
#'   `global_1750-2023_spinup_300_our_inputs_lpjml611_preindustrial_v2`,
#'   outputs `pft_harvestc.nc` and `cftfrac.nc`.
#' @export
#' @examples
#' read_lpjml_regime_yield(example = TRUE)
read_lpjml_regime_yield <- function(
  years = NULL,
  run_dir = NULL,
  data = NULL,
  include_others = FALSE,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_lpjml_regime_yield())
  }
  .lrg_crop_yield(years, run_dir, data, include_others) |>
    .lrg_expand_items(include_others)
}

# -- Private helpers ----------------------------------------------------------

# Per cell, LPJmL crop and year: the rainfed and irrigated per-stand yields,
# and the two stands' fractions of the cell (`stand_frac_*`, which weight the
# yields when they are aggregated over cells; `NA` where a stand is absent).
# This CFT grain is the compact form of the layer (twelve crops, or thirteen
# with "others", rather than forty items); the item expansion is a pure join
# on package data, so a pinned copy of this layer should hold this grain and
# expand on read.
.lrg_crop_yield <- function(
  years = NULL,
  run_dir = NULL,
  data = NULL,
  include_others = FALSE
) {
  if (!is.null(data)) {
    out <- .lrg_band_yield(data$harvestc, data$stand_frac, include_others)
    return(.filter_years_if_present(out, years))
  }
  run_dir <- .lrg_resolve_run_dir(run_dir)
  years <- years %||% .lrg_run_years(run_dir)
  # One year at a time: a year of cftfrac is 6.4 million band-cells, and the
  # whole 274-year record at once would not fit in memory.
  purrr::map(as.integer(years), function(year) {
    .lrg_read_year(run_dir, year, include_others)
  }) |>
    dplyr::bind_rows()
}

.lrg_read_year <- function(run_dir, year, include_others = FALSE) {
  harvestc <- read_lpjml_npp("harvestc", years = year, run_dir = run_dir)
  stand_frac <- read_lpjml_hydrology(
    "stand_frac",
    run_dir = run_dir,
    years = year
  )
  .lrg_band_yield(harvestc, stand_frac, include_others)
}

# Join the per-stand harvest onto the stand fractions by band NAME (never by
# band position), keep the stands that have area, and spread the two regimes
# of each crop side by side.
.lrg_band_yield <- function(harvestc, stand_frac, include_others = FALSE) {
  bands <- .lrg_crop_bands(include_others)
  .lrg_check_bands(harvestc$name_pft, stand_frac$band_name, bands)
  harvest <- harvestc |>
    dplyr::filter(.data$name_pft %in% bands$band_name) |>
    dplyr::select("lon", "lat", "year", band_name = "name_pft", "value") |>
    dplyr::rename(harvest = "value")
  stands <- stand_frac |>
    dplyr::filter(
      .data$band_name %in% bands$band_name,
      is.finite(.data$value),
      .data$value > 0
    ) |>
    dplyr::select("lon", "lat", "year", "band_name", stand_frac = "value") |>
    dplyr::left_join(harvest, by = c("lon", "lat", "year", "band_name"))
  .lrg_check_harvest_present(stands)
  stands |>
    dplyr::inner_join(bands, by = "band_name") |>
    .lrg_spread_regimes()
}

# One row per cell, crop and year with the two regimes' yields side by side.
# A regime with no stand in the cell has no row going in and comes out NA.
.lrg_spread_regimes <- function(stands) {
  stands |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "lpjml_crop",
      "regime",
      yield = "harvest",
      "stand_frac"
    ) |>
    tidyr::pivot_wider(
      names_from = "regime",
      values_from = c("yield", "stand_frac"),
      names_glue = "{.value}_{regime}"
    ) |>
    dplyr::mutate(method_regime_yield = .lrg_method(.data$year)) |>
    ensure_columns(.lrg_crop_prototype(), extra = "drop")
}

.lrg_crop_prototype <- function() {
  tibble::tibble(
    lon = double(),
    lat = double(),
    year = integer(),
    lpjml_crop = character(),
    yield_rainfed = double(),
    yield_irrigated = double(),
    stand_frac_rainfed = double(),
    stand_frac_irrigated = double(),
    method_regime_yield = character()
  )
}

# Attach every production item that maps to each LPJmL crop.
.lrg_expand_items <- function(crop_yield, include_others = FALSE) {
  crop_yield |>
    dplyr::inner_join(
      .lrg_item_bands(include_others),
      by = "lpjml_crop",
      relationship = "many-to-many"
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "item_prod_code",
      "item_cbs_code",
      "lpjml_crop",
      "yield_rainfed",
      "yield_irrigated",
      "method_regime_yield"
    )
}

# The rainfed and irrigated band of each crop-specific LPJmL CFT, read from
# the band vocabulary in inst/extdata/lpjml_cft_bands.csv. Grassland and the
# two bioenergy stands are not crops, and "others" is LPJmL's catch-all stand,
# whose yield is that of a composite rather than of any one item; it is kept
# only on request (`include_others`), for the regime anomaly of D15.
.lrg_crop_bands <- function(include_others = FALSE) {
  path <- system.file("extdata", "lpjml_cft_bands.csv", package = "whep")
  excluded <- .lrg_non_crop_bands()
  if (isTRUE(include_others)) {
    excluded <- setdiff(excluded, "others")
  }
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::filter(
      .data$output == "pft_harvestc",
      !.data$crop %in% excluded
    ) |>
    dplyr::transmute(
      band_name = .data$band_name,
      regime = .data$regime,
      lpjml_crop = .data$crop
    )
}

.lrg_non_crop_bands <- function() {
  c("grassland", "biomass grass", "biomass tree", "others")
}

# Production items -> LPJmL crop, via cft_mapping's `cft_lpjml` (underscored,
# "temperate_cereals") against the band vocabulary's crop ("temperate
# cereals"), plus each item's commodity balance code.
.lrg_item_bands <- function(include_others = FALSE) {
  crops <- unique(.lrg_crop_bands(include_others)$lpjml_crop)
  cbs <- whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(!is.na(.data$item_prod_code)) |>
    dplyr::distinct()
  whep::cft_mapping |>
    dplyr::transmute(
      item_prod_code = as.integer(.data$item_prod_code),
      lpjml_crop = stringr::str_replace_all(.data$cft_lpjml, "_", " ")
    ) |>
    dplyr::filter(.data$lpjml_crop %in% crops) |>
    dplyr::left_join(cbs, by = "item_prod_code") |>
    dplyr::select("item_prod_code", "item_cbs_code", "lpjml_crop")
}

# Every crop band must be present, by name, in both inputs. A run configured
# with a different band set then fails here rather than silently leaving a
# crop without a regime.
.lrg_check_bands <- function(harvest_names, stand_names, bands) {
  missing_h <- setdiff(bands$band_name, harvest_names)
  missing_s <- setdiff(bands$band_name, stand_names)
  if (length(missing_h) == 0L && length(missing_s) == 0L) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "The LPJmL run does not carry every crop band this layer needs.",
      x = "Absent from {.file pft_harvestc.nc}: {.val {missing_h}}.",
      x = "Absent from {.file cftfrac.nc}: {.val {missing_s}}.",
      i = "Bands are matched by name against
           {.file inst/extdata/lpjml_cft_bands.csv}."
    ),
    class = "whep_lpjml_regime_yield_bands"
  )
}

# A stand with area but no harvest value means the two files do not describe
# the same cells; refuse rather than turn the gap into a missing yield.
.lrg_check_harvest_present <- function(stands) {
  n_gap <- sum(!is.finite(stands$harvest))
  if (n_gap == 0L) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "{n_gap} crop stand{?s} with area in {.file cftfrac.nc} ha{?s/ve} no
       finite harvest in {.file pft_harvestc.nc}.",
      i = "The two outputs should cover the same cells; check they come from
           the same run."
    ),
    class = "whep_lpjml_regime_yield_gap"
  )
}

# First year of the run's observed climate forcing (CRU TS 4.09 starts in
# 1901); earlier years run on the recycled 1901-1930 window.
.lrg_climate_first_year <- function() {
  1901L
}

.lrg_method <- function(year) {
  dplyr::if_else(
    year < .lrg_climate_first_year(),
    "lpjml_band_harvest_recycled_climate",
    "lpjml_band_harvest"
  )
}

# The run directory, or an abort naming what to set. There is deliberately no
# pin fallback: no `lpjml-crop-regime-yield` pin has been published, and a
# pin cannot be registered in whep_inputs.csv without its board URL and
# version.
.lrg_resolve_run_dir <- function(run_dir) {
  resolved <- run_dir %||% Sys.getenv("WHEP_LPJML_RUN_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(
      c(
        "No LPJmL run to derive the regime yields from.",
        i = "Pass {.arg run_dir} or set {.envvar WHEP_LPJML_RUN_DIR} to a
             finished LPJmL run's output directory.",
        i = "No pinned {.val lpjml-crop-regime-yield} layer is published
             yet, so there is nothing else to read."
      ),
      class = "whep_lpjml_regime_yield_no_run"
    )
  }
  files <- file.path(resolved, c("pft_harvestc.nc", "cftfrac.nc"))
  absent <- basename(files[!file.exists(files)])
  if (length(absent) > 0L) {
    cli::cli_abort(
      c(
        "The LPJmL run lacks {.file {absent}}.",
        i = "Run directory: {.file {resolved}}.",
        i = "Both {.file pft_harvestc.nc} and {.file cftfrac.nc} are needed;
             there is no substitute for either."
      ),
      class = "whep_lpjml_regime_yield_no_run"
    )
  }
  resolved
}

# Every calendar year pft_harvestc.nc carries, read from its own time axis.
.lrg_run_years <- function(run_dir) {
  rlang::check_installed("ncdf4")
  nc <- ncdf4::nc_open(file.path(run_dir, "pft_harvestc.nc"))
  on.exit(ncdf4::nc_close(nc))
  first_year <- .lpjml_resolve_first_year(nc, NULL, "pft_harvestc.nc")
  first_year + seq_len(nc$dim[["time"]]$len) - 1L
}
