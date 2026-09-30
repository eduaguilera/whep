#' Build the native grassland land extension.
#'
#' @description
#' Produce a grassland land extension keyed by `(year, area_code,
#' item_cbs_code)`, replacing the grassland rows that used to come from the
#' external `land_fp` pin.
#'
#' Two area sources are available, selected with `source`:
#' - `"luh2"` (default): permanent and temporary grassland area (item_cbs 3000
#'   and 3002, LUH2 pasture and rangeland) taken from
#'   [build_primary_production()]. This shares the gridded LUH2 land-use basis
#'   used by the crop land extensions and by livestock spatialisation.
#'   Rotational fallow (item_cbs 3003) is excluded because the
#'   `cropgrids_fallow` crop extension already attributes fallow to crops, so
#'   counting it here too would double count it.
#' - `"faostat_pasture"`: FAOSTAT "Permanent meadows and pastures" area (Land
#'   Use item 6655), the statistics-based basis comparable to most published
#'   footprint studies.
#'
#' Two metrics are available, selected with `grassland_metric`:
#' - `"occupation"` (default): the full grassland area is charged as occupied
#'   land.
#' - `"active_grazing"`: grassland is capped at the area implied by actual
#'   grazing intake (the `"grass"` feed in [get_feed_intake()]) divided by a
#'   usable grass yield, so ungrazed or marginal rangeland is not charged.
#'
#' @param source Grassland area source, `"luh2"` (default) or
#'   `"faostat_pasture"`.
#' @param grassland_metric Grassland land metric, `"occupation"` (default) or
#'   `"active_grazing"`.
#' @param usable_grass_yield_dm_t_ha Usable grass yield in dry-matter tonnes per
#'   hectare, used only by `"active_grazing"`. Defaults to `2.06`.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (for `source = "luh2"`), `landuse` (the `faostat-landuse`
#'   pin, for `source = "faostat_pasture"`) and `feed_intake` (for
#'   `grassland_metric = "active_grazing"`). Each falls back to its reader
#'   ([get_primary_production()], [whep_read_file()], [get_feed_intake()]) when
#'   absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (grassland area in hectares) and `method_grassland` (the chosen
#'   metric), plus the polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' build_grassland_land_extension(example = TRUE)
build_grassland_land_extension <- function(
  source = c("luh2", "faostat_pasture"),
  grassland_metric = c("occupation", "active_grazing"),
  usable_grass_yield_dm_t_ha = 2.06,
  data = list(),
  example = FALSE
) {
  source <- match.arg(source)
  grassland_metric <- match.arg(grassland_metric)
  if (isTRUE(example)) {
    return(.example_grassland_extension())
  }

  occupation <- switch(
    source,
    luh2 = .grassland_occupation_luh2(data$primary_prod),
    faostat_pasture = .grassland_occupation_faostat(data$landuse)
  )
  if (grassland_metric == "occupation") {
    return(
      dplyr::mutate(occupation, method_grassland = "occupation") |>
        .add_reporting_polity_columns()
    )
  }

  .check_usable_grass_yield(usable_grass_yield_dm_t_ha)
  feed_intake <- if (is.null(data$feed_intake)) {
    get_feed_intake()
  } else {
    data$feed_intake
  }
  .grassland_active_grazing(
    occupation,
    feed_intake,
    usable_grass_yield_dm_t_ha
  ) |>
    .add_reporting_polity_columns()
}

# LUH2 grassland area (hectares) from primary production, excluding rotational
# fallow (item_cbs 3003) which the crop fallow extension attributes to crops.
.grassland_occupation_luh2 <- function(primary_prod = NULL) {
  if (is.null(primary_prod)) {
    primary_prod <- get_primary_production()
  }
  grass_items <- setdiff(.grass_item_cbs(), .fallow_item_cbs())
  primary_prod |>
    dplyr::filter(
      .data$unit == "ha",
      !is.na(.data$item_cbs_code),
      .data$item_cbs_code %in% grass_items
    ) |>
    dplyr::summarise(
      impact_u = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    )
}

# FAOSTAT "Permanent meadows and pastures" area (item 6655), filtered live from
# the faostat-landuse pin (Value is in 1000 ha). Raw FAOSTAT reporting areas are
# mapped to WHEP polities through polity_area_crosswalk and collapsed to
# polity_area_code, the same country basis the LUH2 grassland source already
# uses. Statistical aggregates that overlap their components (e.g. FAOSTAT
# "China" 351, "World", continents) are unmapped in the crosswalk and dropped
# here, so they cannot double-count. This replaces an earlier `!is.na(iso3c)`
# heuristic on regions_full, which reinvented aggregate detection and left the
# two grassland sources keyed on different area codes (raw FAOSTAT vs polity).
#
# The two labels this filter selects on are a VOCABULARY the pin owns, not a
# contract WHEP controls. If a pin revision renames either, the filter matches
# no rows, the extension comes back empty, and every footprint built on it is
# short its whole grassland term with no complaint anywhere -- the shape of
# whep#1016. So the labels are asserted before they are used (whep#1034).
.grassland_occupation_faostat <- function(landuse = NULL) {
  if (is.null(landuse)) {
    landuse <- whep_read_file("faostat-landuse")
  }
  landuse <- .check_landuse_labels(landuse)
  landuse |>
    dplyr::filter(.data[["Item Code"]] == 6655, .data$Element == "Area") |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      area_code = as.integer(.data[["Area Code"]]),
      item_cbs_code = 3000L,
      impact_u = round(.data$Value * 1000, 1)
    ) |>
    dplyr::filter(!is.na(.data$impact_u), .data$impact_u > 0) |>
    .grassland_faostat_to_polities()
}

.check_landuse_labels <- function(landuse) {
  remedy <- c(
    i = "Check the {.val faostat-landuse} pin: the label was renamed, or the
         version frozen in {.file inst/extdata/whep_inputs.csv} is a different
         FAOSTAT domain."
  )
  check_labels_supplied(landuse, "Element", "Area", details = remedy)
  check_labels_supplied(landuse, "Item Code", 6655, details = remedy)
}

# Collapse raw FAOSTAT reporting areas to WHEP polity_area_code via the canonical
# crosswalk, dropping unmapped statistical aggregates. Keeps the extension grain
# (year, area_code, item_cbs_code); area_code is now a polity_area_code, matching
# the LUH2 source.
.grassland_faostat_to_polities <- function(occupation) {
  occupation |>
    add_polity_code(code_column = "area_code", year_column = "year") |>
    dplyr::filter(!is.na(.data$polity_code)) |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c(year, polity_area_code, item_cbs_code)
    ) |>
    dplyr::rename(area_code = polity_area_code)
}

.fallow_item_cbs <- function() {
  whep::items_full |>
    dplyr::filter(.data$item_cbs == "Fallow") |>
    dplyr::pull(.data$item_cbs_code) |>
    unique() |>
    as.integer()
}

# Cap grassland area at the area implied by grazing intake and usable yield.
# Grass intake from get_feed_intake() is booked under permanent grassland
# (item_cbs 3000) only, while occupation spans all grass items (e.g. 3002
# temporary grassland). Capping must therefore compare total grazing intake
# against total grassland area per year/area, then split the capped total back
# across items in proportion to their occupation, otherwise items with no
# matching intake row (like 3002) would be silently zeroed.
.grassland_active_grazing <- function(
  occupation,
  feed_intake,
  usable_grass_yield_dm_t_ha
) {
  intake <- feed_intake |>
    dplyr::filter(.data$feed_type == "grass") |>
    dplyr::summarise(
      grazing_intake_dm_t = sum(.data$intake_dry_matter, na.rm = TRUE),
      .by = c(year, area_code)
    ) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code)
    )

  joined <- occupation |>
    dplyr::mutate(
      occupation_total = sum(.data$impact_u, na.rm = TRUE),
      .by = c(year, area_code)
    ) |>
    dplyr::left_join(intake, by = c("year", "area_code"))

  # One country with grassland and no grazing is an observation, and the
  # zero-fill below is the right answer for it. NO country with any grazing at
  # all is an absent input: the cap is then zero everywhere and the whole
  # extension ships as zero, while non-negativity, the cap inequality and
  # conservation all still hold, because zero satisfies them (whep#1034). The
  # join is checked rather than `intake`, so a partial match still passes.
  check_inputs_supplied(
    joined,
    c("grass feed intake" = "grazing_intake_dm_t"),
    details = c(
      i = "No row of {.arg feed_intake} joined a grassland row, so
           {.code grassland_metric = \"active_grazing\"} would charge zero
           hectares everywhere.",
      i = "Check the {.field feed_type} vocabulary of {.fn get_feed_intake}
           and that its {.field year}/{.field area_code} keys match the
           grassland source."
    )
  )

  joined |>
    dplyr::mutate(
      grazing_intake_dm_t = tidyr::replace_na(.data$grazing_intake_dm_t, 0),
      capped_total = pmin(
        .data$occupation_total,
        .data$grazing_intake_dm_t / usable_grass_yield_dm_t_ha,
        na.rm = TRUE
      ),
      impact_u = dplyr::if_else(
        .data$occupation_total > 0,
        .data$impact_u * .data$capped_total / .data$occupation_total,
        0
      ),
      method_grassland = "active_grazing"
    ) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u, method_grassland)
}

.check_usable_grass_yield <- function(usable_grass_yield_dm_t_ha) {
  if (
    !is.numeric(usable_grass_yield_dm_t_ha) ||
      length(usable_grass_yield_dm_t_ha) != 1L ||
      is.na(usable_grass_yield_dm_t_ha) ||
      usable_grass_yield_dm_t_ha <= 0
  ) {
    cli::cli_abort(
      "{.arg usable_grass_yield_dm_t_ha} must be one positive number."
    )
  }
}
