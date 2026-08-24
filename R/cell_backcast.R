# The pre-1962 back-cast estimates production as `tonnes = ha * t_ha`, and both
# existing methods measure the `ha` half on a NATIONAL land series that
# `fill_proxy_growth()` then walks backwards one year at a time. That walk is
# the failure mode: a single year the series cannot measure breaks the chain,
# and every earlier year of that series is lost with it. Measured on `main`,
# `land_method = "historical_polity"` loses 517,935 pre-1962 keys over 71 areas
# that way -- Hungary stops at 1920, Syria at 1946, Israel at 1948, all of them
# one-year gaps in the upstream polity periods (whep-polities#252) rather than
# facts about those countries.
#
# This file removes the chain instead of patching the gaps. A grid cell does not
# change shape and LUH2 carries a cropland value for it in every year, so a
# crop's area in a cell can be referenced straight to 1961:
#
#   area_c,i(Y) = area_c,i(1961) * cropland_c(Y) / cropland_c(1961)
#
# Stepping that year by year telescopes -- 1959 = 1960 * L(1959)/L(1960) =
# 1961 * L(1959)/L(1961) -- so the walk and the single ratio are the same
# arithmetic wherever the series is complete, and `test_cell_backcast.R` asserts
# it. Where the series is NOT complete they differ, and that difference is the
# whole point: the single ratio loses only the year it cannot measure.
#
# Aggregating the cells to a polygon is then a separate step that chains
# nothing, so the territory may change year to year without any of it
# propagating backwards. The cell-to-polity intersection is NOT measured here;
# it is read from the polycell support (`read_polycell_support()`, whep#619),
# the same conserving table `build_historical_land_areas()` uses.
#
# WHAT THIS ASSUMES, stated once here and again in the roxygen: within a cell, a
# crop's share of that cell's cropland is CONSTANT over time. The 1961
# allocation decides which cells grow which crop, and every earlier year
# rescales that same pattern. It is the same single-vintage caveat
# `crop_patterns` already carries -- EarthStat/Monfreda is a circa-2000
# snapshot -- extended backwards along the cropland series.

#' Build a crop-specific pre-1962 land proxy from gridded cells
#'
#' @description
#' Emit the crop-by-crop land series the cell-level pre-1962 back-cast
#' consumes -- `year`, `area_code`, `item_prod_code` and `cropland_mha` -- by
#' scaling each grid cell's 1961 harvested area by that cell's own LUH2
#' cropland ratio and summing the cells into the polity each `area_code`
#' resolved to in that year.
#'
#' For crop `i` in cell `c`:
#' `area(c, i, Y) = area(c, i, 1961) * cropland(c, Y) / cropland(c, 1961)`.
#' Nothing is chained through intermediate years, so a year whose territory
#' cannot be resolved costs that year alone instead of every year before it.
#'
#' @section What this assumes:
#' Within a cell, a crop's share of that cell's cropland is held **constant
#' over time**. The 1961 allocation decides which cells grow which crop and
#' every earlier year rescales that same pattern; a crop cannot appear in a
#' cell it did not occupy in 1961, nor leave one it did. This extends the
#' single-vintage caveat `crop_patterns` already carries -- EarthStat/Monfreda
#' (Monfreda et al. 2008, doi:10.1029/2007GB002947) is a circa-2000 snapshot,
#' so 1850 crop geography is that snapshot rescaled, not a reconstruction of
#' it.
#'
#' A cell whose 1961 cropland is zero has no ratio to give. Such cells carry no
#' 1961 harvested area in the first place unless the allocation's expansion
#' step put some there, and they are dropped with a count rather than divided
#' by zero.
#'
#' @section What it cannot reach:
#' Two coverage limits, both measured rather than assumed, and both inherited
#' from the layers this reads rather than introduced here:
#'
#' * **1850.** The prepared `spatialize-gridded-cropland` layer starts at 1851,
#'   so the ratio has no numerator for 1850 and that year gets no cell-level
#'   back-cast. The function warns naming the years, and the seam aborts rather
#'   than shortening the series silently. Regenerating that layer from 1850 is
#'   what fixes it; filling the year from raw LUH2 instead would mix two
#'   aggregation bases inside one ratio series.
#' * **Crops with no cells.** A crop absent from `crop_patterns` is allocated
#'   nowhere in 1961, so it has no 1961 anchor and no series. On the
#'   `spatialize-crop-patterns` vintage current at whep#761 that included
#'   **barley**, 5.7% of the world's 1961 harvested area, because
#'   `earthstat_mapping.csv` had no `barley` row. The crosswalk is fixed, but
#'   the pin has to be regenerated before this method can reach those crops.
#'
#' Measured on a real 1851-1961 build, the 1961 cells re-aggregate to 943.9 Mha
#' against the 950.6 Mha allocated into them: 0.70% of the anchor sits in cells
#' that no polity of that year claims.
#'
#' @param years Integer vector of calendar years to measure. Defaults to
#'   `1850:1961`, the span the back-cast uses.
#' @param data Named list of pre-loaded inputs bypassing the readers, for
#'   tests: `gridded_1961` (`lon`, `lat`, `item_prod_code`, `harvested_ha`),
#'   `cell_cropland` (`lon`, `lat`, `year`, `cropland_ha`), `polity_areas`
#'   (`year`, `area_code`, `polity_code`) and `cover` (`polity_code`, `lon`,
#'   `lat`, `frac`). Each falls back to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @returns A tibble with columns `year`, `area_code`, `polity_code`,
#'   `item_prod_code` and `cropland_mha`. `area_code` is the
#'   `polity_area_code` aggregation bucket, the key `.read_land_areas()` and
#'   [build_historical_land_areas()] both emit, so the result joins to the
#'   back-cast seam unchanged. `polity_code` names the territory each year was
#'   measured on, semicolon-separated where a bucket holds more than one polity
#'   in a year.
#'
#' @seealso [build_historical_land_areas()], which measures the same territory
#'   for the non-crop half of the back-cast.
#'
#' @export
#'
#' @examples
#' build_cell_crop_land(example = TRUE)
build_cell_crop_land <- function(
  years = 1850:1961,
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_cell_crop_land())
  }
  years <- sort(unique(as.integer(years)))
  data <- data %||% list()
  polity_areas <- data$polity_areas %||% .polity_area_by_year(years)
  cover <- data$cover %||%
    .polity_cell_cover(unique(polity_areas$polity_code), data$support)
  scaled <- .cell_crop_scale(
    data$gridded_1961 %||% .gridded_harvest_1961(),
    data$cell_cropland %||% .cell_cropland_series(years),
    years
  )
  purrr::map(
    years,
    \(yr) .crop_land_in_polygons(scaled[year == yr], polity_areas, cover)
  ) |>
    data.table::rbindlist(use.names = TRUE) |>
    .label_land_polities(polity_areas) |>
    tibble::as_tibble()
}

# --- The back-cast seam ------------------------------------------------------

# `NULL` unless the cell method is requested, so the two established methods
# keep reading exactly what they read before. The table is STATIC -- it depends
# only on the LUH2 vintage, the crop patterns, the polycell support and the
# polities snapshot -- and building it spatializes 1961 and then rescales
# ~1.3 million cell-crop rows for every back-cast year, so it belongs in a pin
# with `data-raw/cell_crop_land.R` as its generator, exactly as
# `historical-land-areas` does.
.cell_crop_land_wide <- function(land_method, years) {
  if (land_method != "cell_polity") {
    return(NULL)
  }
  back_cast <- years[years < 1962L]
  if (length(back_cast) == 0L) {
    return(NULL)
  }
  .read_cell_crop_land(back_cast)
}

# Local parquet first so a development build can be used before the pin is
# published, then the pin. Aborts naming both rather than quietly recomputing a
# tens-of-minutes table inside a pipeline run.
.read_cell_crop_land <- function(years) {
  path <- Sys.getenv("WHEP_CELL_CROP_LAND_PATH", "")
  land <- if (nzchar(path)) {
    if (!file.exists(path)) {
      cli::cli_abort("Cell crop land table not found at {.file {path}}.")
    }
    data.table::as.data.table(nanoparquet::read_parquet(path))
  } else {
    .read_input("cell-crop-land", years = years, year_col = "year")
  }
  land <- land[year %in% years]
  .check_cell_crop_years(land, years)
  tibble::as_tibble(land[, .(year, area_code, item_prod_code, cropland_mha)])
}

.check_cell_crop_years <- function(land, years) {
  missing <- setdiff(years, unique(land$year))
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "The cell crop land table does not cover {length(missing)} requested
     year{?s}: {.val {utils::head(missing, 5)}}.",
    "i" = "Regenerate it with {.file data-raw/cell_crop_land.R} and point
           {.envvar WHEP_CELL_CROP_LAND_PATH} at the parquet, or publish it as
           the {.val cell-crop-land} pin."
  ))
}

# --- Private helpers ---------------------------------------------------------

# The 1961 harvested area of every crop in every cell, collapsed over the
# reporting codes the allocation ran on: WHICH polity holds a cell is decided
# here by the polycell support, not by the grid the engine allocated into, so
# the grid's own `area_code` would be a second, disagreeing answer.
.gridded_harvest_1961 <- function() {
  cft <- .read_packaged_cft_mapping()
  grid <- build_cell_polity() |>
    dplyr::select("lon", "lat", "area_code", "polity_frac")
  gridded <- build_gridded_landuse(
    country_areas = dplyr::filter(
      whep_read_file("spatialize-country-areas"),
      .data$item_prod_code %in% cft$item_prod_code
    ),
    crop_patterns = dplyr::filter(
      whep_read_file("spatialize-crop-patterns"),
      .data$item_prod_code %in% cft$item_prod_code
    ),
    gridded_cropland = whep_read_file("spatialize-gridded-cropland"),
    country_grid = grid,
    config = list(
      type_cropland = whep_read_file("spatialize-type-cropland"),
      type_mapping = cft,
      years = 1961L
    )
  )
  data.table::as.data.table(gridded)[,
    .(harvested_ha = sum(rainfed_ha + irrigated_ha, na.rm = TRUE)),
    by = .(lon, lat, item_prod_code)
  ]
}

# Per-cell cropland for every requested year, from the same prepared LUH2 layer
# the allocation itself ran on, so numerator and denominator of the ratio are
# the same quantity measured the same way.
.cell_cropland_series <- function(years) {
  cropland <- data.table::as.data.table(
    whep_read_file("spatialize-gridded-cropland")
  )
  out <- cropland[year %in% c(years, 1961L), .(lon, lat, year, cropland_ha)]
  missing <- setdiff(years, unique(out$year))
  if (length(missing) > 0L) {
    cli::cli_warn(c(
      "!" = "{.val spatialize-gridded-cropland} carries no cropland for
        {length(missing)} requested year{?s}: {.val {utils::head(missing, 5)}}.",
      "i" = "Those years get no cell-level back-cast."
    ))
  }
  out
}

# area(c, i, Y) = area(c, i, 1961) * cropland(c, Y) / cropland(c, 1961).
#
# The join is on the cell, so a crop can only ever be rescaled by the cropland
# of the cell it sits in. Cells whose 1961 cropland is zero or missing have no
# ratio and are dropped, counted rather than silently divided by zero.
.cell_crop_scale <- function(gridded_1961, cell_cropland, years) {
  gridded_1961 <- data.table::as.data.table(gridded_1961)
  cropland <- data.table::as.data.table(cell_cropland)
  base <- cropland[year == 1961L, .(lon, lat, cropland_1961 = cropland_ha)]
  base <- base[is.finite(cropland_1961) & cropland_1961 > 0]
  anchor <- merge(gridded_1961, base, by = c("lon", "lat"), sort = FALSE)
  .warn_cells_without_1961(gridded_1961, anchor)
  ratio <- merge(
    cropland[year %in% years, .(lon, lat, year, cropland_ha)],
    base,
    by = c("lon", "lat"),
    sort = FALSE
  )
  ratio[, scale := cropland_ha / cropland_1961]
  merge(
    anchor[, .(lon, lat, item_prod_code, harvested_ha)],
    ratio[, .(lon, lat, year, scale)],
    by = c("lon", "lat"),
    allow.cartesian = TRUE,
    sort = FALSE
  )[, .(lon, lat, year, item_prod_code, area_ha = harvested_ha * scale)]
}

.warn_cells_without_1961 <- function(gridded_1961, anchor) {
  lost <- sum(gridded_1961$harvested_ha) - sum(anchor$harvested_ha)
  if (lost <= 0) {
    return(invisible(NULL))
  }
  cells <- data.table::uniqueN(gridded_1961[, .(lon, lat)]) -
    data.table::uniqueN(anchor[, .(lon, lat)])
  cli::cli_warn(c(
    "!" = "{cells} cell{?s} hold 1961 harvested area but no 1961 cropland;
      {round(lost)} ha {?has/have} no ratio to back-cast with.",
    "i" = "The allocation's expansion step can place a crop in a cell the
           cropland layer calls empty."
  ))
  invisible(NULL)
}

# Sum the rescaled cell areas into each bucket for ONE year, sharing every cell
# among the polities that cover it in proportion to the covered fraction
# renormalised to one per cell -- the rule `.land_in_polygons()` already uses,
# applied crop by crop.
.crop_land_in_polygons <- function(cells, polity_areas, cover) {
  yr <- cells$year[1]
  live <- data.table::as.data.table(polity_areas)[year == yr]
  if (nrow(live) == 0L || nrow(cells) == 0L) {
    return(.empty_cell_crop_land())
  }
  merge(
    cells,
    .polity_cell_shares(live, cover),
    by = c("lon", "lat"),
    allow.cartesian = TRUE
  )[,
    .(cropland_mha = sum(area_ha * share) / 1e6),
    by = .(year, area_code, item_prod_code)
  ]
}

.empty_cell_crop_land <- function() {
  data.table::data.table(
    year = integer(0),
    area_code = integer(0),
    item_prod_code = numeric(0),
    cropland_mha = numeric(0)
  )
}
