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
#' @param cereal_residue How cereal residue is estimated. One of:
#'   - `"ipcc"` (default): the IPCC yield-dependent line, above-ground residue
#'     dry matter = slope x grain dry-matter yield + intercept, through
#'     [calculate_crop_residues()] with its modern-variety correction.
#'   - `"ensemble"`: the same estimator's mean of that line and the static
#'     [biomass_coefs] ratio, as the soil carbon and nitrogen chain uses it.
#'   - `"ratio"`: the static [biomass_coefs] ratio alone.
#'   - `"wirsenius"`: the predecessor's model, Wirsenius (2000) Table 3.16
#'     regional ratios, applied directly on Wirsenius's own region membership.
#'
#'   See the "Cereal residue" section. Other crops are not affected.
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
#' - `value`: The amount of residue produced -- all of it, before any is
#'    recovered from the field -- in tonnes of **fresh matter**, like every
#'    other commodity-balance quantity.
#' - `value_dm`: The same residue in tonnes of **dry matter**: each crop's
#'    fresh residue times its own residue dry-matter content,
#'    `Residue_kgDM_kgFM` in [biomass_coefs], summed per row. `NA` where a
#'    crop with residue mass carries no such coefficient, so the gap stays
#'    visible rather than reading as zero.
#' - `method_residue`: how the row's residue was estimated: the
#'    `cereal_residue` method for a cereal, `"pin"` for every other crop.
#'
#' The pin's residue quantities are fresh matter. Across its crops the ratio of
#' pinned residue to product tracks the fresh-matter residue:product ratio
#' `kg_residue_kg_product_FM` of [biomass_coefs] (about 0.8 of it for nearly
#' every crop), not the residue's dry-matter content, which runs from 0.13
#' (tomato) to 1.0 (rapeseed) (whep#1215). Use `value_dm` wherever a quantity
#' is defined per unit of dry matter, such as a residue nitrogen content.
#'
#' The pin does not hold the residue produced but the share of it recovered
#' from the field: the predecessor pipeline multiplied each residue row by the
#' legacy recovery rate of its region and crop category (`recovery_rates` in
#' `residue_recovery.csv`, see [calculate_residue_destinies()]) before writing
#' it. Every reader of `value` treats it as the whole residue and applies a
#' recovery rate of its own, so `value` is the pin divided by that same rate
#' (whep#1195). The division is exact: WHEP ships the rate the predecessor
#' used, and on the real pin the result equals the predecessor's gross residue
#' on all 427,584 rows where both exist. Where that rate is 0 (fodder crops
#' in seven of the eight regions; roots and tubers, cassava, sugar beet and
#' dry beans in West Europe and in North America and Oceania) the
#' pin holds 0 and the gross residue cannot be recovered from it, so those
#' crops carry no residue here, as before.
#'
#' The ratio behind the pin's gross residue is the fresh-matter
#' residue:product ratio of [biomass_coefs], scaled by the region's
#' residue:product ratio relative to West Europe (`residue_dm_product_dm` in
#' the same table, Wirsenius (2000) Table 3.16) and by a harvest-index change
#' factor per region and year. That factor is what makes the ratio vary by
#' year. The predecessor's table carries it at eight anchor years from 1910 to
#' 2000, interpolates linearly between them and holds 2000 constant after it,
#' falling for instance from 1.10 in 1962 to 1.00 in 2000 in East Europe. Its
#' code attributes the table to Krausmann et al. (2013), *PNAS* 110:10324,
#' Table M2 (assumed, unverified), and WHEP does not ship it.
#'
#' The predecessor looked the regional ratio up by `regions_full$region_HANPP`,
#' which carries Wirsenius's eight region names but not his membership
#' (Wirsenius 2000, Table 3.1): Southeast Asia, Russia, Belarus, the Caucasus
#' and Sudan took another region's ratio (whep#1430). For every crop that
#' reads the pin, the ratio is re-keyed here on Wirsenius's own membership,
#' through the (HANPP region, UN M49 sub-region) pairs listed in
#' `residue_feed_regions.csv`. That lowers world non-cereal residue dry
#' matter by 0.2% in 2010.
#'
#' @section Cereal residue:
#' Cereals are the one part of the base with a published global series:
#' Smerald, Rahimi & Scheer (2023), *A global dataset for the production and
#' usage of cereal residues in the period 1997-2021*, Scientific Data 10:685,
#' \doi{10.1038/s41597-023-02587-0}, the mean of three methods (constant
#' regional ratios, and two yield-dependent ones). The pin's model put world
#' cereal residue 16.1% above that mean over 1997-2021 (3899 against 3357 Tg
#' dry matter), and 25% above their constant-ratio method in every year with
#' the same grain production (whep#1448). Besides the membership above, two
#' things caused it. The West Europe anchor presumes that [biomass_coefs]
#' holds the West Europe ratio, and for cereals it does not (wheat 1.34,
#' barley 1.18, sorghum 1.70 and maize 0.96 kg dry matter per kg grain dry
#' matter, against Wirsenius's 1.0, 1.0, 1.2 and 1.2). And Wirsenius's ratios
#' are early-1990s harvest indices, held constant after 2000 while yields
#' rose.
#'
#' Cereal residue is therefore estimated from the pin's own production and
#' harvested area (its `Product` rows, which equal the `primary_prod` pin)
#' with [calculate_crop_residues()], the estimator the soil carbon and
#' nitrogen chain already uses. Its modern-variety harvest-index correction
#' applies, keyed on the HANPP region its adoption table is written in. The
#' `"ipcc"` line is IPCC (2019), *2019 Refinement to the 2006 IPCC Guidelines
#' for National Greenhouse Gas Inventories*, Vol. 4, Ch. 11, Table 11.2
#' (p. 11.19), whose cereal slopes and intercepts are those of IPCC (2006)
#' Table 11.2. Measured against Smerald et al. (`validation/residue_base_dm.R`),
#' world cereal residue in dry matter over 1997-2021 and in 2010 is:
#'
#' | `cereal_residue` | 1997-2021 mean, Tg | vs Smerald | years outside band | 2010, Tg |
#' |---|--:|--:|--:|--:|
#' | `"ipcc"` | 3385 | +0.8% | 0 of 25 | 3333 |
#' | `"ensemble"` | 3059 | -8.9% | 5 of 25 | 3008 |
#' | `"ratio"` | 2732 | -18.6% | 25 of 25 | 2684 |
#' | `"wirsenius"` | 3656 | +8.9% | 9 of 25 | 3598 |
#' | the pin's model | 3899 | +16.1% | 16 of 25 | 3826 |
#'
#' The band is the spread of Smerald et al.'s three methods, widened by 5%;
#' their 2010 mean is 3334 Tg. `"ipcc"` matches the world total, but per crop
#' it books maize about 19% and sorghum about 20% below their mean and wheat
#' about 23% above it (2010 and 2020). `"wirsenius"` keeps the pin's
#' harvest-index factor and corrects the anchor by applying Table 3.16
#' directly: residue dry matter is grain dry matter times the ratio of the
#' area's Wirsenius region.
#'
#' @inheritSection whep_read_file The batch pin on the build path
#'
#' @export
#'
#' @examples
#' get_primary_residues(example = TRUE)
get_primary_residues <- function(
  example = FALSE,
  cereal_residue = c("ipcc", "ensemble", "ratio", "wirsenius")
) {
  cereal_residue <- rlang::arg_match(cereal_residue)
  if (example) {
    return(.example_get_primary_residues())
  }

  # The `crop_residues` pin is predecessor-pipeline output, not a curated
  # input: its `Product` rows equal the `primary_prod` pin's tonnes to the last
  # digit, and its `Residue` rows are the RECOVERED residue of the
  # predecessor's harvest-index model, which `.residue_gross_from_recovered()`
  # turns back into the residue produced (#1195). See the pin-batch section
  # above for the measurement, and note that this is where the predecessor's
  # production series enters the commodity balance (#1054).
  # `.residue_ratio_corrected()` then corrects that model's ratio (#1448).
  # Cereal residue is recomputed from the pin's `Product` rows, so both kinds
  # of row resolve their area here, once.
  pin <- "crop_residues" |>
    whep_read_file() |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(product_residue %in% c("Residue", "Product")) |>
    add_area_code(name_column = "area") |>
    .residue_area_from_polity()
  pin |>
    dplyr::filter(product_residue == "Residue") |>
    .warn_residues_no_area() |>
    .residue_gross_from_recovered() |>
    .residue_ratio_corrected(
      products = dplyr::filter(pin, product_residue == "Product"),
      method = cereal_residue
    ) |>
    add_item_cbs_code(
      name_column = "item_cbs_crop",
      code_column = "item_cbs_code_crop"
    ) |>
    add_item_cbs_code(
      name_column = "item_cbs",
      code_column = "item_cbs_code_residue"
    ) |>
    .add_residue_dm_content() |>
    dplyr::summarise(
      # whep#167: a single NA `prod_ygpit_mg` sibling otherwise poisons the
      # whole group sum to NA, which `filter(value > 0)` below then silently
      # drops -- erasing real, non-NA residue rows along with the missing one.
      value = sum(prod_ygpit_mg, na.rm = TRUE),
      value_dm = .sum_residue_dm(prod_ygpit_mg, residue_kgdm_kgfm),
      .by = c(
        year,
        area_code,
        item_cbs_code_crop,
        item_cbs_code_residue,
        method_residue
      )
    ) |>
    dplyr::filter(value > 0) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code_crop,
      item_cbs_code_residue,
      value,
      value_dm,
      method_residue
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
# (9.5%) and 16,651,046,476 t of residue fresh matter (5.08%), and every one of
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

# Turn the pin's RECOVERED residue back into the residue the crop produced.
#
# The pin's `Residue` rows are not gross residue. The predecessor pipeline
# (`Global/R/crop_npp.r`, then `afsetools::residue_use()`) wrote each one as
# the product, times the `kg_residue_kg_product_FM` ratio of `biomass_coefs`,
# times a harvest-index change factor for the region and year, times the
# region's `residue_dm_product_dm` over West Europe's for the category, times
# `Use_Share` -- the legacy recovery rate of the region and category. Every
# consumer of `value` reads it as the whole residue: `calculate_residue_
# destinies()` multiplies it by a recovery rate again, and the soil-N2O path
# takes its unremoved share. So the recovery was applied twice (whep#1195).
#
# Dividing by that same rate undoes it EXACTLY, because WHEP ships the rate the
# predecessor used: `recovery_rates` in `residue_recovery.csv` equals its
# `residue_krausmann$Recovery_rates` in all 160 cells, keyed the same way --
# `items_prod_full$Cat_Krausmann` by production item and
# `regions_full$region_HANPP` by area. Measured on the real pin, the result
# equals the predecessor's gross residue on all 427,584 comparable rows at a
# relative tolerance of 1e-6. Nothing is estimated here.
#
# Where the legacy rate is 0 (31,492 pin rows, all of them 0 in the pin) the
# gross residue was never written, so it cannot be recovered from the pin: the
# row stays 0 and is dropped below, exactly as before. A row with no area has
# no region, so its recovery cannot be undone either; it keeps the pin's figure
# and stays visible with `NA` polity columns, which every consumer that joins
# on an area already drops (`.warn_residues_no_area()` names it).
#
# A row WITH an area that reaches no rate, or a positive residue where the rate
# is 0, means the pin was not written by the rule this inverts. Passing it on
# would book recovered residue as gross, so it aborts.
.residue_gross_from_recovered <- function(dt) {
  if (!rlang::has_name(dt, "item_prod")) {
    cli::cli_abort(
      "The crop-residue table has no {.field item_prod} column, so the
       recovery rate its residue already carries cannot be undone.",
      class = "whep_residue_pin_recovery"
    )
  }
  rated <- dplyr::left_join(
    dt,
    .residue_pin_recovery_rates(dt),
    by = c("item_prod", "area_code"),
    relationship = "many-to-one"
  )
  .check_pin_recovery(rated)
  rated |>
    dplyr::mutate(
      prod_ygpit_mg = dplyr::case_when(
        is.na(.data$area_code) ~ .data$prod_ygpit_mg,
        .data$pin_recovery_rate > 0 ~ .data$prod_ygpit_mg /
          .data$pin_recovery_rate,
        .default = .data$prod_ygpit_mg
      )
    ) |>
    dplyr::select(-"pin_recovery_rate")
}

# The legacy recovery rate behind each (item_prod, area_code) pair the pin
# carries. The pin names its crops, so the production item is resolved through
# `add_item_prod_code()` and everything after it joins on codes.
#
# Keyed on `regions_full$region_HANPP` ON PURPOSE, and it must stay so. That
# membership files Southeast Asia, Russia and the Caucasus under the wrong
# Wirsenius region (whep#1430), but it is the membership the predecessor
# WROTE the pin with, so it is the only one that inverts it exactly. Correcting
# the membership belongs to the forward recovery rate and residue ratio, never
# to this undo.
.residue_pin_recovery_rates <- function(dt) {
  categories <- whep::items_prod_full |>
    dplyr::distinct(
      item_prod_code = as.character(.data$item_prod_code),
      cat_krausmann = .data$Cat_Krausmann
    )
  regions <- whep::regions_full |>
    dplyr::filter(!is.na(.data$code)) |>
    dplyr::distinct(
      area_code = as.integer(.data$code),
      region_krausmann = .data$region_HANPP
    )
  dt |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::distinct(.data$item_prod, .data$area_code) |>
    add_item_prod_code(name_column = "item_prod") |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::left_join(categories, by = "item_prod_code") |>
    dplyr::left_join(regions, by = "area_code") |>
    dplyr::left_join(
      .residue_recovery_rates("legacy"),
      by = c("cat_krausmann", "region_krausmann")
    ) |>
    dplyr::select(
      "item_prod",
      "area_code",
      pin_recovery_rate = "recovery_rates"
    )
}

# No cli pluralisation markers, for the reason `.warn_residues_no_area()`
# gives.
.check_pin_recovery <- function(rated) {
  has_area <- !is.na(rated$area_code)
  no_rate <- has_area & is.na(rated$pin_recovery_rate)
  zero_rate_mass <- has_area &
    !no_rate &
    rated$pin_recovery_rate == 0 &
    !is.na(rated$prod_ygpit_mg) &
    rated$prod_ygpit_mg > 0
  if (!any(no_rate) && !any(zero_rate_mass)) {
    return(invisible(NULL))
  }
  bad <- no_rate | zero_rate_mass
  crops <- sort(unique(as.character(rated$item_prod[bad])))
  n_bad <- sum(bad)
  cli::cli_abort(
    c(
      "{n_bad} crop-residue rows cannot be turned back into gross residue.",
      "i" = "Their crop or region reaches no legacy recovery rate, or the rate
        is 0 while the pin holds residue: {.val {crops}}.",
      "i" = "The pin's residue is recovered residue (whep#1195); without the
        rate it was recovered at, it cannot be read as the residue produced."
    ),
    class = "whep_residue_pin_recovery"
  )
}

# Attach each pin row's residue dry-matter content (kg DM per kg fresh
# residue), keyed on the row's own `name_biomass`, so the conversion follows
# the crop that produced the residue and not the CBS residue item it is booked
# to. "Other crop residues" (2106) mixes vegetable haulm at 0.13-0.30 with
# pulse straw at 0.9; a single coefficient for the item cannot convert it
# (whep#1215).
#
# Joined many-to-one on purpose: `biomass_coefs` repeats a few livestock names,
# and a crop name that ever matched two different contents would be a guess,
# so it aborts rather than duplicating residue mass.
.add_residue_dm_content <- function(dt, biomass_coefs = whep::biomass_coefs) {
  if (!rlang::has_name(dt, "name_biomass")) {
    cli::cli_abort(
      "The crop-residue table has no {.field name_biomass} column, so its
       residue cannot be converted to dry matter."
    )
  }
  coefs <- biomass_coefs |>
    tibble::as_tibble() |>
    dplyr::filter(.data$Name_biomass %in% dt$name_biomass) |>
    dplyr::distinct(
      name_biomass = .data$Name_biomass,
      residue_kgdm_kgfm = .data$Residue_kgDM_kgFM
    )
  dt |>
    dplyr::left_join(
      coefs,
      by = "name_biomass",
      relationship = "many-to-one"
    ) |>
    .warn_residue_no_dm()
}

# Name the residue mass that has no dry-matter content, instead of letting it
# vanish from `value_dm`. No cli pluralisation markers, for the reason
# `.warn_residues_no_area()` gives.
.warn_residue_no_dm <- function(dt) {
  gap <- is.na(dt$residue_kgdm_kgfm) &
    !is.na(dt$prod_ygpit_mg) &
    dt$prod_ygpit_mg > 0
  if (!any(gap)) {
    return(dt)
  }
  mass_mt <- round(sum(dt$prod_ygpit_mg[gap]) / 1e6, 3)
  names_gap <- sort(unique(as.character(dt$name_biomass[gap])))
  cli::cli_warn(c(
    "!" = "{mass_mt} Mt of crop residue has no residue dry-matter content in
       {.field biomass_coefs}, so {.field value_dm} is NA for it.",
    "i" = "Biomass names without {.field Residue_kgDM_kgFM}:
       {.val {names_gap}}"
  ))
  dt
}

# Dry matter of one residue group. A missing fresh mass is ignored, as in
# `value` (whep#167); a missing dry-matter content on real mass is not, and
# makes the group NA.
.sum_residue_dm <- function(fresh_t, kgdm_kgfm) {
  has_mass <- !is.na(fresh_t) & fresh_t > 0
  if (any(has_mass & is.na(kgdm_kgfm))) {
    return(NA_real_)
  }
  sum(fresh_t[has_mass] * kgdm_kgfm[has_mass])
}

# TODO: This is dirty, revisit when we build the data here directly.
# Keep crop residue rows keyed to the crop production process item.
.use_crop_process_cbs_item <- function(crop_residues) {
  crop_residues
}
