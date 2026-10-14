# Native producer for the gridded agricultural land support that the nitrogen
# balance allocates its non-crop-specific inputs over (deposition, urban N, SOM
# mineralization, and manure assigned to Cropland without a landing crop).
#
# Before this file the support table was a caller-supplied input with no
# producer anywhere in the package, so build_n_inputs(resolution = "grid")
# aborted on any real input set. Both sides are assembled from readers the
# gridded nitrogen pipeline already depends on:
#
# - CROPLAND: type_cropland.parquet gives each cell's PHYSICAL cropland hectares
#   per year (type_ha summed over the five LUH2 cropland classes; the parquet
#   carries crop classes only, never pasture -- see prepare_spatialize_all.R
#   Section 4). The crop composition of each polycell and year is the crop
#   allocation the nitrogen inputs and the soil carbon inputs place every
#   crop by (R/crop_allocation.R, whep#1002): each crop's harvested hectares
#   in the polycell, its allocation weight times its national FAOSTAT
#   harvested area. The composition is normalised to sum to 1 within the
#   polycell and then multiplied by the physical hectares, so the cropland
#   area is split among its crops and NEVER inflated: harvested area can sum
#   above the physical area under multicropping, which would over-credit
#   deposition if used as an area directly.
# - GRASSLAND: gridded_pasture.parquet's pasture_ha + rangeland_ha, the same
#   contract spatialize_livestock() already consumes, all carried on CBS 3000.
#   No intensive/extensive split is inferred, matching the decision recorded in
#   R/n_balance_inputs.R. read_luh2_landuse()'s grassland class is the
#   alternative: verified to agree to the hectare for 2010 (3208.5 Mha both
#   ways), but it stops at 2015 where gridded_pasture runs to 2023 alongside the
#   cropland surface, so it is selectable rather than the default.
#
# Both sides are split across border polities by the SAME cell_polity crosswalk
# (read_luh2_landuse() accepts polity_frac through .normalize_country_grid()),
# so a cell's cropland and grassland cannot land in different polities.
#
# A grassland source that runs short of the cropland surface leaves years with
# cropland support and no grassland support. Those years are warned about and
# carry cropland-only support rather than being dropped or back-filled with a
# fabricated grassland area.

#' Build the gridded agricultural land support.
#'
#' @description
#' Assembles the physical agricultural land support that [build_n_inputs()]
#' allocates its non-crop-specific nitrogen terms over: per grid cell, polity,
#' year and CBS item, the hectares of agricultural land available to receive
#' nitrogen. Cropland hectares come from the LUH2-derived `type_cropland`
#' surface, split among crops by the harvested-area composition of the crop
#' allocation the nitrogen and soil carbon inputs share (the spatialization
#' engine's placement of each crop, whep#1002), normalised within each
#' polycell, so the physical cropland area is apportioned rather than
#' inflated by multicropping. Grassland hectares come
#' from [read_luh2_landuse()]'s gridded grassland class and are all carried on
#' CBS 3000, with no intensive/extensive split inferred. Both sides are split
#' across border polities by the same `cell_polity` crosswalk.
#'
#' Years with cropland but no grassland coverage (a grassland source that runs
#' short of the cropland surface, as `"luh2"` does after 2015) keep their
#' cropland support and raise a warning naming the affected years; supply
#' `data$grassland_ha` to cover them.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the cropland surface covers.
#' @param grassland Grassland-support source. `"gridded_pasture"` (default) is
#'   the prepared per-cell `pasture_ha` + `rangeland_ha` surface, which shares
#'   the cropland surface's grid and 1851-2023 span. `"luh2"` reads the same
#'   LUH2 classes through [read_luh2_landuse()] and agrees with it where they
#'   overlap, but stops at 2015. `"none"` returns cropland-only support, an
#'   explicit choice rather than a silent gap.
#' @inheritParams build_water_balance
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `cell_polity` (the [build_cell_polity()] crosswalk), `type_cropland`
#'   (`lon`, `lat`, `year`, `luh2_type`, `type_ha`), `crop_allocation` (the
#'   shared crop allocation: a list of `weights` and `fallback`, built from
#'   the pinned spatialization inputs when absent), `harvested_area`
#'   (`area_code`, `item_prod_code`, `year`, `faostat_area_ha`; from
#'   [get_primary_production()] when absent), `gridded_pasture` (`lon`,
#'   `lat`, `year`, `pasture_ha`, `rangeland_ha`), `states`
#'   ([read_luh2_landuse()]'s raw LUH2 states) and `grassland_ha` (`lon`,
#'   `lat`, `area_code`, `year`, `area_ha`, bypassing the grassland read
#'   entirely). Each falls back to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with `lon`, `lat`, `area_code`, `item_cbs_code`, `year`,
#'   `land_use` (`"cropland"` or `"grassland"`) and positive `area_ha`, plus the
#'   polity columns below, plus `reporting_polity_out_of_span` when
#'   `polity_validity = "flag"`.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_ag_land_support(example = TRUE)
build_ag_land_support <- function(
  years = NULL,
  grassland = c("gridded_pasture", "luh2", "none"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
) {
  grassland <- rlang::arg_match(grassland)
  polity_validity <- rlang::arg_match(polity_validity)
  if (isTRUE(example)) {
    return(.resolve_polity_validity(
      .example_ag_land_support(),
      polity_validity
    ))
  }
  .crop_alloc_refuse_patterns(data, "build_ag_land_support")
  cell_polity <- data$cell_polity %||% build_cell_polity()
  .check_columns(
    cell_polity,
    c("lon", "lat", "area_code", "polity_frac"),
    "cell_polity"
  )
  cropland <- .als_cropland_support(data, cell_polity, years)
  .als_finalise(
    cropland,
    .als_grassland_support(data, cell_polity, cropland, grassland)
  ) |>
    .resolve_polity_validity(polity_validity)
}

# ---- Cropland support ------------------------------------------------------

# Physical cropland hectares per cell-year, split across border polities by
# polity_frac and apportioned among each polycell's crops by the crop
# allocation's harvested-area composition.
.als_cropland_support <- function(data, cell_polity, years) {
  cropland_ha <- .als_type_cropland(data, years)
  composition <- .als_crop_composition(
    data,
    cell_polity,
    sort(unique(cropland_ha$year))
  )
  polycells <- cropland_ha |>
    dplyr::inner_join(
      dplyr::select(cell_polity, "lon", "lat", "area_code", "polity_frac"),
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(area_code = as.integer(.data$area_code))
  .als_warn_uncovered(polycells, composition)
  polycells |>
    dplyr::inner_join(
      composition,
      by = c("lon", "lat", "area_code", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = as.integer(.data$area_code),
      item_cbs_code = .data$item_cbs_code,
      year = .data$year,
      land_use = "cropland",
      area_ha = .data$type_ha * .data$crop_frac * .data$polity_frac
    )
}

# Per-cell PHYSICAL cropland hectares, summed over the LUH2 cropland classes.
# Mirrors .n_cropland_ha() (R/n_balance_spatialize.R) but treats years = NULL as
# "every year the surface covers" instead of filtering to an empty table.
.als_type_cropland <- function(data, years) {
  raw <- data$type_cropland %||% .n_read_grid_input("type_cropland", years)
  .check_columns(
    raw,
    c("lon", "lat", "year", "luh2_type", "type_ha"),
    "type_cropland"
  )
  raw <- tibble::as_tibble(raw)
  if (!is.null(years)) {
    raw <- dplyr::filter(raw, .data$year %in% years)
  }
  raw |>
    dplyr::summarise(
      type_ha = sum(.data$type_ha, na.rm = TRUE),
      .by = c(lon, lat, year)
    ) |>
    dplyr::filter(is.finite(.data$type_ha), .data$type_ha > 0)
}

# Each polycell's crop composition as shares summing to 1 per year: the
# harvested hectares the crop allocation puts there (its weight times the
# polity-crop's national harvested area; a crop the allocation does not place
# spread over the polity's cropland by the fallback weight), pooled to
# commodity balance items. Harvested hectares are used only to apportion the
# physical hectares, never as an area themselves.
.als_crop_composition <- function(data, cell_polity, years) {
  allocation <- .crop_allocation(
    years,
    .normalize_carbon_support(cell_polity, "cell_polity"),
    data
  )
  national <- .als_harvested_area(data, years)
  placed <- allocation$weights |>
    dplyr::inner_join(
      national,
      by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$item_prod_code,
      .data$year,
      harvested_ha = .data$area_weight * .data$faostat_area_ha
    )
  moved <- national |>
    dplyr::anti_join(
      allocation$weights,
      by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::inner_join(
      allocation$fallback,
      by = c("area_code", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$item_prod_code,
      .data$year,
      harvested_ha = .data$cropland_weight * .data$faostat_area_ha
    )
  dplyr::bind_rows(placed, moved) |>
    dplyr::inner_join(.als_crop_item_codes(), by = "item_prod_code") |>
    dplyr::summarise(
      harvested_ha = sum(.data$harvested_ha),
      .by = c("lon", "lat", "area_code", "year", "item_cbs_code")
    ) |>
    dplyr::filter(is.finite(.data$harvested_ha), .data$harvested_ha > 0) |>
    dplyr::mutate(
      crop_frac = .data$harvested_ha / sum(.data$harvested_ha),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "item_cbs_code",
      "crop_frac"
    )
}

# The FAOSTAT national harvested area per polity, production item and year,
# the area every allocation weight is scaled to (.sci_harvested_area()).
.als_harvested_area <- function(data, years) {
  raw <- data$harvested_area %||%
    .sci_harvested_area(
      data$primary_prod %||% get_primary_production(years = years)
    )
  .check_columns(
    raw,
    c("area_code", "item_prod_code", "year", "faostat_area_ha"),
    "harvested_area"
  )
  tibble::as_tibble(raw) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      year = as.integer(.data$year),
      faostat_area_ha = as.numeric(.data$faostat_area_ha)
    ) |>
    dplyr::filter(
      .data$year %in% years,
      is.finite(.data$faostat_area_ha),
      .data$faostat_area_ha > 0
    )
}

# Every CBS-mapped crop production code, via the same items_prod_full crosswalk
# .n_item_prod_codes() uses. Grass CBS items are excluded: grassland support is
# assembled from the LUH2 grassland class, not from the crop-pattern raster.
.als_crop_item_codes <- function() {
  grass <- c(3000L, 3002L, 3003L)
  whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(
      !is.na(.data$item_prod_code),
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% grass
    ) |>
    dplyr::distinct(.data$item_prod_code, .data$item_cbs_code)
}

# Polycells carrying cropland hectares that the crop allocation places no crop
# in. Their nitrogen cannot be attributed, so they are named rather than
# dropped in silence.
.als_warn_uncovered <- function(cropland_ha, composition) {
  key <- c("lon", "lat", "area_code", "year")
  uncovered <- dplyr::anti_join(
    cropland_ha,
    dplyr::distinct(composition, dplyr::pick(dplyr::all_of(key))),
    by = key
  )
  if (nrow(uncovered) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "!" = "{nrow(uncovered)} polycell-year{?s}
           ({round(sum(uncovered$type_ha * uncovered$polity_frac), 1)} ha)
           have cropland but no crop in the crop allocation; they carry no
           cropland support.",
    "i" = "Supply {.field data$crop_allocation} covering {.field lon}/{.field
           lat} {unique(uncovered$lon)[1]}/{unique(uncovered$lat)[1]} and
           similar cells, or {.field data$ag_land_support} directly."
  ))
  invisible(NULL)
}

# ---- Grassland support -----------------------------------------------------

# LUH2 gridded grassland (pastr + range), all carried on CBS 3000. An injected
# grassland_ha bypasses the LUH2 read; grassland = "none" is an explicit
# cropland-only choice.
.als_grassland_support <- function(data, cell_polity, cropland, grassland) {
  if (grassland == "none") {
    return(.als_empty())
  }
  years <- sort(unique(cropland$year))
  raw <- data$grassland_ha %||%
    .als_read_grassland(data, cell_polity, years, grassland)
  if (is.null(raw) || nrow(raw) == 0L) {
    return(.als_empty())
  }
  .check_columns(
    raw,
    c("lon", "lat", "area_code", "year", "area_ha"),
    "grassland_ha"
  )
  .als_warn_year_gap(years, sort(unique(raw$year)))
  .als_grassland_rows(raw, years)
}

# Shape a gridded grassland layer onto the support contract. All pasture and
# rangeland is carried on CBS 3000: no intensive/extensive class is inferred.
.als_grassland_rows <- function(raw, years) {
  raw |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = as.integer(.data$area_code),
      item_cbs_code = 3000L,
      year = .data$year,
      land_use = "grassland",
      area_ha = .data$area_ha
    )
}

.als_read_grassland <- function(data, cell_polity, years, grassland) {
  if (grassland == "gridded_pasture") {
    return(.als_read_gridded_pasture(data, cell_polity, years))
  }
  .als_read_luh2_grassland(data, cell_polity, years)
}

# The prepared per-cell pasture + rangeland surface, the same contract
# spatialize_livestock() already consumes. It shares the cropland surface's grid
# and 1851-2023 span, so a full-period run needs no grassland back-fill. Border
# cells are split by the SAME cell_polity crosswalk the cropland side uses.
.als_read_gridded_pasture <- function(data, cell_polity, years) {
  raw <- data$gridded_pasture %||% .n_read_grid_input("gridded_pasture", years)
  .check_columns(
    raw,
    c("lon", "lat", "year", "pasture_ha", "rangeland_ha"),
    "gridded_pasture"
  )
  tibble::as_tibble(raw) |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::inner_join(
      dplyr::select(cell_polity, "lon", "lat", "area_code", "polity_frac"),
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      year = .data$year,
      area_ha = (.data$pasture_ha + .data$rangeland_ha) * .data$polity_frac
    )
}

# Read the gridded LUH2 grassland class for the years LUH2 actually covers,
# splitting border cells by the SAME cell_polity crosswalk the cropland side
# uses (.normalize_country_grid() accepts polity_frac).
#
# `area_basis` is pinned to `"luh2_fraction"` ON PURPOSE: LUH2's own grassland
# hectares, split between a cell's polities by the support's share. The
# cropland half is LUH2's cropland hectares split by `polity_frac` the same
# way, so both halves share one land definition (the mismatch AM-29 recorded).
#
# The SPLIT has migrated. C7 moved the carbon path onto the polycell's measured
# land (DA-26), and the gridded nitrogen driver now follows it (whep#1196):
# `build_cell_polity(year = )` returns `polity_frac` as the polity's share of
# the cell's measured land, equal to the carbon path's `cell_area_frac`, so
# both halves of this support are split on measured land whenever that
# support is passed. `"polycell_land"` stays out: it would rescale the
# grassland hectares onto the polycell's own land while the cropland half
# keeps LUH2's, putting the two halves back on two definitions.
.als_read_luh2_grassland <- function(data, cell_polity, years) {
  covered <- .als_luh2_years(data, years)
  if (length(covered) == 0L) {
    return(NULL)
  }
  read_luh2_landuse(
    resolution = "grid",
    years = covered,
    area_basis = "luh2_fraction",
    data = list(states = data$states, country_grid = cell_polity)
  ) |>
    dplyr::filter(.data$land_use == "grassland") |>
    dplyr::select("lon", "lat", "area_code", "year", "area_ha")
}

# The requested years LUH2 can actually serve, taken from the injected states or
# from the local states.nc time axis -- never from a hardcoded end year. With
# neither available the request is passed through so the reader itself reports
# the missing input.
.als_luh2_years <- function(data, years) {
  if (!is.null(data$states)) {
    return(intersect(years, unique(data$states$year)))
  }
  nc_path <- file.path(.luh2_states_dir(), "states.nc")
  if (!file.exists(nc_path)) {
    return(years)
  }
  intersect(years, .luh2_nc_years(nc_path))
}

# Years with cropland support but no grassland support (LUH2 v2h ends before the
# cropland surface does). Named rather than back-filled with a fabricated area.
.als_warn_year_gap <- function(requested, covered) {
  missing <- setdiff(requested, covered)
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "!" = "No gridded grassland support for {length(missing)} year{?s}
           ({min(missing)}-{max(missing)}), which carry cropland support only.",
    "i" = "LUH2 v2h ends before the cropland surface does. Supply
           {.field data$grassland_ha} to cover those years."
  ))
  invisible(NULL)
}

# ---- Shared -----------------------------------------------------------------

# Sum the two sides onto the support key and drop non-positive area, the
# contract .ni_land_support() (R/n_balance_inputs.R) validates.
.als_finalise <- function(cropland, grassland) {
  dplyr::bind_rows(cropland, grassland) |>
    dplyr::filter(is.finite(.data$area_ha), .data$area_ha > 0) |>
    dplyr::summarise(
      area_ha = sum(.data$area_ha),
      .by = c(lon, lat, area_code, item_cbs_code, year, land_use)
    ) |>
    dplyr::arrange(
      .data$year,
      .data$area_code,
      .data$lon,
      .data$lat,
      .data$item_cbs_code
    )
}

.als_empty <- function() {
  tibble::tibble(
    lon = double(),
    lat = double(),
    area_code = integer(),
    item_cbs_code = integer(),
    year = integer(),
    land_use = character(),
    area_ha = double()
  )
}

# A real build_ag_land_support() run over two cells, one of them shared 60/40
# between two polities: 1000 physical cropland ha split 75/25 by a 0.9/0.3
# wheat/barley pattern, and the LUH2 grassland fraction of each cell.
.example_ag_land_support <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~land_use,
    ~area_ha,
    0.25,
    50.25,
    10L,
    2511L,
    2010L,
    "cropland",
    750,
    0.25,
    50.25,
    10L,
    2513L,
    2010L,
    "cropland",
    250,
    0.25,
    50.25,
    10L,
    3000L,
    2010L,
    "grassland",
    1976.55,
    0.75,
    50.25,
    10L,
    2511L,
    2010L,
    "cropland",
    300,
    0.75,
    50.25,
    10L,
    3000L,
    2010L,
    "grassland",
    2371.86,
    0.75,
    50.25,
    20L,
    2511L,
    2010L,
    "cropland",
    200,
    0.75,
    50.25,
    20L,
    3000L,
    2010L,
    "grassland",
    1581.24
  ) |>
    .add_reporting_polity_columns()
}
