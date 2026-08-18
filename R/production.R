#' Primary items production
#'
#' @description
#' Get amount of crops, livestock and livestock products.
#'
#' @param years Optional integer vector of years to build. When `NULL`
#'   (default) the whole series is built. Supplying a window builds only that
#'   range rather than building 1850-2023 and discarding the rest, and caches it
#'   under a window-specific key.
#'
#'   A window is not guaranteed to reproduce the full-range result value for
#'   value, because some steps look across the years present. Measured for 2010
#'   against the full range, `area_code`-level totals agree exactly for `ha`,
#'   `t_ha`, `LU` and `heads`; the largest disagreement is 3.0e-04, in the
#'   livestock ratios (`t_head`, `t_LU`) and `slaughtered_heads`, tracked in
#'   issue #625. Use `NULL` when exact agreement with the published series
#'   matters.
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
#'
#' @returns
#' A tibble with the item production data.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: Legacy numeric reporting area code.
#' - `polity_area_code`: Numeric WHEP reporting polity code used for matrix
#'    workflows. This currently matches `area_code`.
#' - `reporting_polity_code`: WHEP polity code for the reporting polygon.
#' - `reporting_polity_name`: WHEP polity name for the reporting polygon.
#' - `reporting_polity_has_geometry`: Whether the reporting polity has a
#'    polygon in the WHEP polity database.
#' - `item_prod_code`: FAOSTAT internal code for each produced item.
#' - `item_cbs_code`: FAOSTAT internal code for each commodity balance sheet
#'    item. The commodity balance sheet contains an aggregated version of
#'    production items. This field is the code for the corresponding
#'    aggregated item.
#' - `live_anim_code`: Commodity balance sheet code for the type of livestock
#'    that produces the livestock product. It can be:
#'    - `NA`: The entry is not a livestock product.
#'    - Non-`NA`: The code for the livestock type. The name can also be
#'    retrieved by using `add_item_cbs_name()`.
#' - `unit`: Measurement unit for the data. Here, keep in mind three groups of
#'    items: crops (e.g. `Apples and products`, `Beans`...), livestock (e.g.
#'    `Cattle, dairy`, `Goats`...) and livestock products (e.g. `Poultry
#'    Meat`, `Offals, Edible`...). Then the unit can be one of:
#'    - `tonnes`: Available for crops and livestock products.
#'    - `ha`: Hectares, available for crops.
#'    - `t_ha`: Tonnes per hectare, available for crops.
#'    - `heads`: Number of animals (stocks), available for livestock.
#'    - `slaughtered_heads`: Number of animals slaughtered, available
#'      for livestock.
#'    - `LU`: Standard Livestock Unit measure, available for livestock.
#'    - `t_head`: tonnes per head, available for livestock products.
#'    - `t_LU`: tonnes per Livestock Unit, available for livestock products.
#' - `value`: The amount of item produced, measured in `unit`.
#'
#' @export
#'
#' @examples
#' get_primary_production(example = TRUE)
get_primary_production <- function(years = NULL, example = FALSE) {
  if (example) {
    return(.ex_get_primary_prod())
  }
  build_years <- .build_years(years)
  .cached_primary_prod(build_years) |>
    .filter_years(build_years)
}

#' Crop residue items
#'
#' @description
#' Get type and amount of residue produced for each crop production item.
#'
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
#'
#' @returns
#' A tibble with the crop residue data.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `item_cbs_code_crop`: FAOSTAT internal code for each commodity balance
#'    sheet item. This is the crop that is generating the residue.
#' - `item_cbs_code_residue`: FAOSTAT internal code for each commodity balance
#'    sheet item. This is the obtained residue. In the commodity balance sheet,
#'    this can be three different items right now:
#'    - `2105`: `Straw`
#'    - `2106`: `Other crop residues`
#'    - `2107`: `Firewood`
#'
#'    These are actually not FAOSTAT defined items, but custom defined by us.
#'    When necessary, FAOSTAT codes are extended for our needs.
#' - `value`: The amount of residue produced, measured in tonnes.
#'
#' @export
#'
#' @examples
#' get_primary_residues(example = TRUE)
get_primary_residues <- function(example = FALSE) {
  if (example) {
    return(.example_get_primary_residues())
  }

  "crop_residues" |>
    whep_read_file() |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(product_residue == "Residue") |>
    add_area_code(name_column = "area") |>
    .warn_residues_no_area() |>
    add_item_cbs_code(
      name_column = "item_cbs_crop",
      code_column = "item_cbs_code_crop"
    ) |>
    add_item_cbs_code(
      name_column = "item_cbs",
      code_column = "item_cbs_code_residue"
    ) |>
    dplyr::summarise(
      value = sum(prod_ygpit_mg),
      .by = c(year, area_code, item_cbs_code_crop, item_cbs_code_residue)
    ) |>
    dplyr::filter(value > 0) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code_crop,
      item_cbs_code_residue,
      value
    ) |>
    .use_crop_process_cbs_item() |>
    .add_reporting_polity_columns()
}

# Say when a residue row cannot be attributed to any area, instead of emitting
# it silently.
#
# `add_area_code()` resolves this source by NAME -- it is the only builder that
# does -- and leaves `area_code` as NA where no name matches. Those rows then
# travel all the way to the output with NA polity columns and reach
# `build_supply_use()` from there. Measured on the current pin: 44,985 of 475,688
# rows (9.5%) have no area code, over 14 labels and years 1961-2021, and 3,937
# rows of `get_primary_residues()`'s own output carry NA polity columns as a
# result. Every one of the 14 is a common short form of an area the crosswalk
# holds under a FAOSTAT long form -- "Tanzania" against "United Republic of
# Tanzania", "Netherlands" against "Netherlands (Kingdom of the)" -- so the codes
# are reachable and the spellings are not.
#
# Nothing said so. Every other unattributable-row path in this package names
# itself; this was the exception, and it is the origin of the gap, so tracing it
# from downstream took a full-range run instead of reading a warning.
#
# Reports rather than drops. The rows stay in the output exactly as before,
# because whether an unattributable residue row should be dropped is a modelling
# question and this is a diagnostic. Repairing the name-based join itself is a
# separate change.
.warn_residues_no_area <- function(dt) {
  if (!all(c("area", "area_code", "year") %in% names(dt))) {
    return(dt)
  }
  missing <- is.na(dt$area_code)
  if (!any(missing)) {
    return(dt)
  }
  # No cli pluralisation markers. `{?s}` keys on "the" quantity, and this message
  # interpolates a count and a vector of labels, so cli cannot decide which and
  # aborts with "length(object) == 1 is not TRUE". Plain wording cannot fail.
  n_missing <- sum(missing)
  labels <- sort(unique(as.character(dt$area[missing])))
  n_labels <- length(labels)
  first_year <- min(dt$year[missing], na.rm = TRUE)
  last_year <- max(dt$year[missing], na.rm = TRUE)
  cli::cli_warn(c(
    "!" = "{n_missing} crop-residue rows resolved to no area, so their polity
       columns stay NA and any join on {.field reporting_polity_code} drops
       them.",
    "i" = "{n_labels} unresolved labels over {first_year}-{last_year}:
       {.val {labels}}"
  ))
  dt
}

# TODO: This is dirty, revisit when we build the data here directly.
# Keep crop residue rows keyed to the crop production process item.
.use_crop_process_cbs_item <- function(crop_residues) {
  crop_residues
}
