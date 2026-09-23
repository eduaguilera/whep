# Per-CFT consumptive water and net irrigation requirement, kept per band.
#
# build_water_balance() reads the same three per-CFT LPJmL cubes and sums
# their bands to a whole-cell depth before anything can use them, so the crop
# identity is gone by the time a consumer sees `blue_consump_mm` (#916). This
# builder applies the identical stand-fraction weighting
# (.wb_band_consump(), shared with build_water_balance()) and stops one step
# short of that sum. Summed over bands it reproduces build_water_balance()'s
# three per-CFT columns exactly; that identity is what the tests pin.
#
# It stays at LPJmL CFT grain on purpose. Attributing a band to WHEP items
# through cft_mapping is many-to-one (35 of the 58 crop CBS commodities fall
# into "others"), and how to split a band between the items sharing it is a
# methodological choice this function does not make.

#' Build per-crop consumptive water from an LPJmL run, one row per band.
#'
#' @description
#' Returns the LPJmL per-crop-functional-type (CFT) consumptive blue and green
#' water and net irrigation requirement **per band**, rather than summed to the
#' cell as [build_water_balance()] reports them. Each value is the band's
#' water expressed as a depth over the whole cell (mm/yr): the per-stand
#' density LPJmL writes, multiplied by the band's stand fraction. That is the
#' same weighting [build_water_balance()] applies before summing, so summing
#' this output over bands per cell-year returns its `blue_consump_mm`,
#' `green_consump_mm` and `cft_nir_mm` exactly.
#'
#' It is the annual, consumptive counterpart of [build_crop_water_use()],
#' which keeps the *applied* irrigation (`cft_airrig_month`) per crop and
#' month. The two read different LPJmL outputs and answer different
#' questions (water evaporated by a crop versus water delivered to it), and
#' share the band vocabulary and the `crop_group` column.
#'
#' The crop dimension is LPJmL's, not WHEP's: a run distinguishes about 16
#' CFTs (each rainfed and irrigated), and many WHEP items share one CFT --
#' wheat and barley are both `temperate cereals`, and most fruit, vegetable,
#' fibre and stimulant crops fall into `others`. Water is therefore resolved
#' **per CFT band**, never per WHEP item. Mapping a band to items (for
#' example through `cft_mapping`) and deciding how to split it among the
#' items that share it are left to the caller; neither is done here.
#'
#' Two cautions carried over from the reader. On an LPJmL 6.x run without the
#' green/blue fix (lbm364dl/LPJmL#3) the blue/green split of the consumptive
#' cubes is unusable; the same data check [build_water_balance()] runs warns
#' when rainfed bands carry blue water (class `whep_rainfed_blue_water`),
#' while `blue + green` stays valid. And `cft_nir_mm` is the net irrigation
#' *requirement*, not the gross water applied; which of the two a footprint
#' should charge is a methodological choice.
#'
#' @param resolution `"grid"` (per cell and band, depths in mm/yr, default)
#'   or `"polity"` (per `area_code`, `year` and band, volumes in m3/yr).
#' @param years Optional integer vector of calendar years to read. `NULL`
#'   reads every year the files carry; each per-CFT cube is about 3 GB for a
#'   full run, so restrict it when reading from disk.
#' @param bands Optional character vector of band names (e.g.
#'   `"rainfed grassland"`) to keep. `NULL` keeps every band. Matched on the
#'   `band_name` the file carries, as in [build_water_balance()]; an unknown
#'   name aborts.
#' @param data Optional named list of pre-loaded inputs, each falling back to
#'   [read_lpjml_hydrology()] (restricted to `years`) when absent:
#'   `cft_consump_water_b`, `cft_consump_water_g` and `cft_nir` (each `lon`,
#'   `lat`, `year`, `band` and/or `band_name`, `value` mm/yr per stand),
#'   `stand_frac` (the same key, `value` the stand fraction of the cell) and,
#'   for `resolution = "polity"` only, the required `cell_polity` crosswalk
#'   (`lon`, `lat`, `area_code`, `polity_frac`, `cell_area_ha`).
#' @param example If `TRUE`, run on a small built-in fixture instead of
#'   reading data. Defaults to `FALSE`.
#' @return A tibble. For `resolution = "grid"`: `lon`, `lat`, `year`, `band`
#'   (when the input carries it), `band_name`, `crop_group` (the soil-carbon
#'   crop group of the band, as in [build_crop_water_use()]: `NA` for the
#'   `others`, grassland and bioenergy bands), `stand_frac`, and `blue_consump_mm`, `green_consump_mm`
#'   and `cft_nir_mm` (mm/yr over the whole cell; `NA` where that cube does
#'   not carry the band). Divide by `stand_frac` for a per-hectare-of-crop
#'   intensity. For `resolution = "polity"`: `year`, `area_code`, the band
#'   columns, `stand_area_ha`, and `blue_consump_m3`, `green_consump_m3` and
#'   `cft_nir_m3`, each the depth times `polity_frac * cell_area_ha` (the
#'   weight [build_water_balance()] uses for its polity means) summed over
#'   the polity's cells, plus the polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_crop_water_consumption(example = TRUE)
#' build_crop_water_consumption(resolution = "polity", example = TRUE)
build_crop_water_consumption <- function(
  resolution = c("grid", "polity"),
  years = NULL,
  bands = NULL,
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    data <- .example_cft_water_inputs()
  }
  data <- .cwc_read_inputs(data, years)
  grid <- .cwc_grid(data, bands)
  if (resolution == "grid") {
    return(grid)
  }
  .cwc_aggregate_polity(grid, data$cell_polity) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers --------------------------------------------------

# Names of the per-CFT water cubes and the output column each becomes.
.cwc_cubes <- function() {
  c(
    cft_consump_water_b = "blue_consump_mm",
    cft_consump_water_g = "green_consump_mm",
    cft_nir = "cft_nir_mm"
  )
}

# Fill each absent cube from the reader, then the stand fractions for the
# years those cubes cover.
.cwc_read_inputs <- function(data, years) {
  for_read <- setdiff(names(.cwc_cubes()), names(data))
  read <- purrr::map(
    rlang::set_names(for_read),
    \(var) read_lpjml_hydrology(var, years = years, monthly = FALSE)
  )
  data <- c(data, read)
  data$stand_frac <- .wb_stand_frac(data)
  data
}

# Weight each cube per band and join the three on the cell-year-band key.
.cwc_grid <- function(data, bands) {
  .wb_warn_rainfed_blue(data$cft_consump_water_b, data$cft_consump_water_g)
  cubes <- .cwc_cubes()
  parts <- purrr::imap(cubes, function(out_col, var) {
    data[[var]] |>
      .wb_filter_bands(bands) |>
      .wb_band_consump(out_col, data$stand_frac)
  })
  parts <- purrr::compact(parts)
  if (length(parts) == 0L) {
    cli::cli_abort("No per-CFT water cube supplied or readable.")
  }
  purrr::reduce(parts, .cwc_join_bands) |>
    .cwc_complete_columns(cubes) |>
    .cwc_label_bands()
}

# Full join two per-band parts. The stand fraction is the same weight on both
# sides (one stand_frac table), so it is coalesced rather than joined on.
.cwc_join_bands <- function(x, y) {
  key <- intersect(
    c("lon", "lat", "year", "band", "band_name"),
    intersect(names(x), names(y))
  )
  dplyr::full_join(x, y, by = key, suffix = c("", ".y")) |>
    dplyr::mutate(
      stand_frac = dplyr::coalesce(.data$stand_frac, .data$stand_frac.y)
    ) |>
    dplyr::select(-"stand_frac.y")
}

# Add an all-NA column for every cube that was not supplied, so the output
# schema does not depend on which inputs were present.
.cwc_complete_columns <- function(grid, cubes) {
  prototype <- tibble::as_tibble(
    rlang::set_names(rep(list(double()), length(cubes)), unname(cubes))
  )
  ensure_columns(grid, prototype) |>
    dplyr::relocate(dplyr::all_of(unname(cubes)), .after = "stand_frac")
}

# The soil-carbon crop group of each band, in build_crop_water_use()'s
# vocabulary so the two per-crop water layers share it.
.cwc_label_bands <- function(grid) {
  if (!rlang::has_name(grid, "band_name")) {
    grid$band_name <- NA_character_
  }
  grid |>
    dplyr::mutate(crop_group = .cwu_band_group(.data$band_name)) |>
    dplyr::relocate("crop_group", .after = "band_name") |>
    tibble::as_tibble()
}

# Aggregate the per-band depths to per-polity volumes: mm over a cell times
# its polity-allocated hectares, times 10 (1 mm over 1 ha is 10 m3).
.cwc_aggregate_polity <- function(grid, crosswalk) {
  crosswalk <- .wb_require_input(
    crosswalk,
    "cell_polity",
    c("lon", "lat", "area_code", "polity_frac", "cell_area_ha")
  )
  keys <- intersect(c("band", "band_name", "crop_group"), names(grid))
  grid |>
    dplyr::inner_join(crosswalk, by = c("lon", "lat")) |>
    dplyr::mutate(weight_ha = .data$polity_frac * .data$cell_area_ha) |>
    dplyr::summarise(
      stand_area_ha = sum(.data$stand_frac * .data$weight_ha),
      blue_consump_m3 = .cwc_volume(.data$blue_consump_mm, .data$weight_ha),
      green_consump_m3 = .cwc_volume(.data$green_consump_mm, .data$weight_ha),
      cft_nir_m3 = .cwc_volume(.data$cft_nir_mm, .data$weight_ha),
      .by = dplyr::all_of(c("year", "area_code", keys))
    )
}

# Sum of depth x area in m3, NA (never 0) when no depth is available, so an
# absent cube stays visibly absent after aggregation.
.cwc_volume <- function(depth_mm, weight_ha) {
  if (all(is.na(depth_mm))) {
    return(NA_real_)
  }
  sum(depth_mm * weight_ha * 10, na.rm = TRUE)
}
