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
#' - `source`: Where the value came from, e.g. `"FAOSTAT_prod"`,
#'    `"EuropeAgriDB"`, `"LUH2_cropland"`, `"imputed_yield"`.
#' - `fao_flag`: FAOSTAT's observation-status code for the value (`"A"`
#'    official, `"E"` estimated, `"I"` imputed, `"M"`, `"X"`), or `NA` where
#'    the number is not one FAOSTAT published under a flag. It describes the
#'    value rather than the item or area, so it is `NA` on WHEP's computed
#'    yields, on the livestock-unit conversions, and on every gap-filled or
#'    back-cast row. See [build_primary_production()] for the full rule.
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
#' @inheritSection whep_read_file The two batch pins on the build path
#'
#' @export
#'
#' @examples
#' get_primary_residues(example = TRUE)
get_primary_residues <- function(example = FALSE) {
  if (example) {
    return(.example_get_primary_residues())
  }

  # The `crop_residues` pin is predecessor-pipeline output, not a curated
  # input: its `Product` rows equal the `primary_prod` pin's tonnes to the last
  # digit, and the year-varying residue ratio behind its `Residue` rows is not
  # in this repository. See the pin-batch section above for the measurement,
  # and note that this is where the predecessor's production series enters the
  # commodity balance (#1054).
  "crop_residues" |>
    whep_read_file() |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(product_residue == "Residue") |>
    add_area_code(name_column = "area") |>
    .residue_area_from_polity() |>
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
      # whep#167: a single NA `prod_ygpit_mg` sibling otherwise poisons the
      # whole group sum to NA, which `filter(value > 0)` below then silently
      # drops -- erasing real, non-NA residue rows along with the missing one.
      value = sum(prod_ygpit_mg, na.rm = TRUE),
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

# Resolve a residue area label the NAME join could not, through the polity the
# label names.
#
# `add_area_code()` matches the crosswalk's canonical area names exactly, and
# this pin -- the only source resolved by name -- spells 14 of its 185 labels in
# the common short form: "Tanzania" against "United Republic of Tanzania",
# "Turkey" against "Turkiye", "Netherlands" against "Netherlands (Kingdom of
# the)". Measured on the current pin, those 14 labels are 44,985 of 475,688 rows
# (9.5%) and 16,651,046,476 t of residue dry matter (5.08%), and every one of
# them then took a missing-value path through the rest of the package:
# `.read_crop_residues()` drops a row that reaches no polity, and
# `calculate_residue_destinies()` gives a row with no `region_krausmann` a
# recovery rate of 0, so the whole residue is booked to soil with nothing
# recovered, nothing fed and nothing burned (whep#1175, whep#684).
#
# NOTHING IS INVENTED HERE. The label goes through `resolve_polity_label()` --
# WHEP's curated alias table, regenerated together with `polities` from one
# upstream revision -- and the polity it names is mapped back to the single area
# that reports it. All 14 resolve, and none of the 14 target codes is already
# carried by another label in the pin, so no country is counted twice.
#
# It is asked per (label, year) because a label's referent moves. "Tanzania" is
# TZA-1964-2025 from 1964 on but the pre-union TZA-1961-1964 before it, and no
# FAOSTAT area reports Tanganyika: those 150 rows (23,413,534 t, 0.14% of the
# gap) keep `NA` rather than being booked to the United Republic, and
# `.warn_residues_no_area()` below names what is left.
#
# Only rows the name join left `NA` are touched, so the 171 labels that already
# resolve keep exactly the code they had.
.residue_area_from_polity <- function(dt) {
  if (!all(c("area", "area_code", "year") %in% names(dt))) {
    return(dt)
  }
  unresolved <- is.na(dt$area_code)
  if (!any(unresolved)) {
    return(dt)
  }
  keys <- tibble::tibble(
    .residue_label = as.character(dt$area[unresolved]),
    .residue_year = as.integer(dt$year[unresolved])
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      polity_code = resolve_polity_label(
        .data$.residue_label,
        year = .data$.residue_year
      )
    ) |>
    dplyr::left_join(.unique_polity_area(), by = "polity_code") |>
    dplyr::filter(!is.na(.data$area_code_from_polity)) |>
    dplyr::select(-"polity_code")
  if (nrow(keys) == 0L) {
    return(dt)
  }
  out <- dt |>
    dplyr::mutate(
      .residue_label = as.character(.data$area),
      .residue_year = as.integer(.data$year)
    ) |>
    dplyr::left_join(keys, by = c(".residue_label", ".residue_year")) |>
    dplyr::mutate(
      area_code = dplyr::coalesce(
        .data$area_code,
        .data$area_code_from_polity
      )
    )
  .inform_residue_area_route(
    dt$area[unresolved],
    out$area_code[unresolved]
  )
  dplyr::select(
    out,
    -".residue_label",
    -".residue_year",
    -"area_code_from_polity"
  )
}

# The one area that reports each polity. A polity several areas map to resolves
# to none: in the shipped crosswalk that is only the Rest-of-World bucket
# ROW-1850-2025, which 15 areas share, and picking one of them would be a guess.
.unique_polity_area <- function() {
  .current_area_lookup(include_unmapped = TRUE) |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$polity_code), !is.na(.data$area_code)) |>
    dplyr::distinct(.data$polity_code, .data$area_code) |>
    dplyr::filter(dplyr::n() == 1L, .by = "polity_code") |>
    dplyr::transmute(
      polity_code = .data$polity_code,
      area_code_from_polity = as.integer(.data$area_code)
    )
}

# Changing where a row's area comes from is a change of attribution, so say it.
# No cli pluralisation markers, for the reason `.warn_residues_no_area()` gives.
.inform_residue_area_route <- function(labels, codes) {
  gained <- !is.na(codes)
  if (!any(gained)) {
    return(invisible(NULL))
  }
  n_rows <- sum(gained)
  named <- sort(unique(as.character(labels[gained])))
  n_labels <- length(named)
  cli::cli_inform(c(
    "v" = "{n_rows} crop-residue rows took their area code from the polity
       their label names, because no canonical area name matched it.",
    "i" = "{n_labels} labels resolved this way: {.val {named}}"
  ))
  invisible(NULL)
}

# Say when a residue row cannot be attributed to any area, instead of emitting
# it silently.
#
# `add_area_code()` resolves this source by NAME -- it is the only builder that
# does -- and leaves `area_code` as NA where no name matches.
# `.residue_area_from_polity()` above now recovers the 14 short-form labels that
# caused nearly all of it; what reaches here is what neither route resolves.
# Those rows travel all the way to the output with NA polity columns and reach
# `build_supply_use()` from there, so the gap stays named rather than silent.
#
# Nothing said so before whep#684. Every other unattributable-row path in this
# package names itself; this was the exception, and it is the origin of the gap,
# so tracing it from downstream took a full-range run instead of reading a
# warning.
#
# Reports rather than drops. The rows stay in the output, because whether an
# unattributable residue row should be dropped is a modelling question and this
# is a diagnostic.
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
