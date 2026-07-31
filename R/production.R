#' Primary items production
#'
#' @description
#' Get amount of crops, livestock and livestock products.
#'
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
get_primary_production <- function(example = FALSE) {
  if (example) {
    return(.ex_get_primary_prod())
  }
  .cache_get("primary_prod", build_primary_production())
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
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' get_primary_residues(example = TRUE)
# Say when a residue row cannot be attributed to any area, instead of emitting it silently.
#
# `add_area_code()` resolves this source by NAME -- it is the only builder that does -- and
# leaves `area_code` as `NA` where no name matches. Those rows then travel all the way to the
# output with NA polity columns, and downstream they reach `build_supply_use()`: measured at
# full range, 160 residue rows carry no area code and exactly 160 of `build_supply_use()`'s
# 10,118,408 rows have NA polity columns. They are the same rows.
#
# Nothing said so. The CBS path warns about the 24 of them that reach its own join, but this
# builder -- the origin -- was silent, so tracing the supply-use gap took a full-range smoke run
# instead of reading a warning. Every other unattributable-row path in this package now names
# itself; this was the exception.
#
# Reports rather than drops: the rows stay in the output exactly as before, because whether an
# unattributable residue row should be dropped is a modelling question and this is a diagnostic.
# The remainder today is the `TAN-1922-1964` code/column disagreement that upstream baselines
# rather than guesses at, which is why the years are named.
.warn_residues_no_area <- function(dt) {
  if (!"area_code" %in% names(dt)) {
    return(dt)
  }
  missing <- is.na(dt$area_code)
  if (!any(missing)) {
    return(dt)
  }
  years <- sort(unique(dt$year[missing]))
  labels <- if ("area" %in% names(dt)) {
    sort(unique(as.character(dt$area[missing])))
  } else {
    character()
  }
  # No cli pluralisation markers. `{?s}` keys on "the" quantity, and this message interpolates
  # a count AND a vector of years, so cli cannot decide which -- it aborts with
  # "length(object) == 1 is not TRUE". Plain wording costs nothing and cannot fail.
  n_missing <- sum(missing)
  cli::cli_warn(c(
    "!" = "{n_missing} crop-residue rows resolved to no area, so their polity columns are
       NA and any join on {.field reporting_polity_code} drops them.",
    "i" = "affected years: {.val {years}}",
    "i" = if (length(labels) > 0L) {
      "unresolved labels: {.val {utils::head(labels, 6)}}"
    } else {
      "no area label column survives to this point, so the labels cannot be named"
    }
  ))
  dt
}

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

# TODO: This is dirty, revisit when we build the data here directly.
# Keep crop residue rows keyed to the crop production process item.
.use_crop_process_cbs_item <- function(crop_residues) {
  crop_residues
}
