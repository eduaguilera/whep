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
#' Oil palm residue is re-anchored rather than taken from the pin
#' (whep#1424). The pin books 35.8 t of residue dry matter per harvested
#' hectare at 2020, more than an oil palm plantation's whole above-ground net
#' primary production, fruit included: 81% of 17.3 Mg C, so about 30 t DM at
#' the 0.47 kg C per kg DM of [biomass_coefs], per hectare and year in the
#' industrial plantation of Wakhid, Hirano, Dariah & Agus (2022), *Mires and
#' Peat* 28:02, doi:10.19189/map.2021.snpg.sta.2288. In Malaysia it is 2.2 to 2.9
#' times the country's whole solid oil palm biomass, mill residues included.
#' The anchor is Malaysia's field residue: fronds (46 Mt) and trunks (14 Mt)
#' of dry matter on 4.85 Mha planted in 2010, Exhibit 1 and p. 10 of Agensi
#' Inovasi Malaysia (2013), *National Biomass Strategy 2020*, version 2.0
#' (FAOLEX mal228571). Mill residues (empty fruit bunches, fibre, shells) are
#' part of the fruit bunch and are not field residue, so they are left out.
#'
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
#' @param oil_palm_residue How oil palm residue (fronds and trunks) is
#'   obtained. One of:
#'   - `"per_product"` (default): 60 Mt DM / 83.09 Mt of fresh fruit bunches
#'     = 0.722 t DM per tonne of fruit, Malaysia's 2010 fronds and trunks over
#'     its 2010 FAOSTAT production. It keeps the pin's production-times-ratio
#'     form and is the cautious rule where the per-hectare one is weakest:
#'     low-yield, semi-wild palm systems such as Nigeria's (2.6 t of fruit per
#'     hectare against Malaysia's ~17), which `"per_hectare"` would give twice
#'     the pin's residue. West African semi-wild systems stay uncertain under
#'     either rule and would need their own source. Maintainer decision
#'     (whep#1424).
#'   - `"per_hectare"`: 60 Mt DM / 4.85 Mha = 12.37 t DM per harvested
#'     hectare, the same 2010 anchor. It treats frond and trunk production as
#'     a property of the standing palm, and the rate barely moves between the
#'     strategy's two Malaysian years (12.26 t DM/ha at 2012, from its "over 83
#'     million dry tonnes" times "about 75 percent" on FAOSTAT's 5.08 Mha),
#'     where the per-tonne ratio drifts (0.656 at 2012).
#'   - `"pin"`: the pin as published, 3.8 t fresh residue per tonne of fruit.
#'
#'   The anchored dry matter is converted to fresh mass with the oil palm
#'   `Residue_kgDM_kgFM` of [biomass_coefs], so `value_dm` is the anchor
#'   exactly. The choice is recorded in `method_residue`.
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
#' - `value`: The amount of residue produced, in tonnes of **fresh matter**,
#'    like every other commodity-balance quantity.
#' - `value_dm`: The same residue in tonnes of **dry matter**: each crop's
#'    fresh residue times its own residue dry-matter content,
#'    `Residue_kgDM_kgFM` in [biomass_coefs], summed per row. `NA` where a
#'    crop with residue mass carries no such coefficient, so the gap stays
#'    visible rather than reading as zero.
#' - `method_residue`: Where the residue quantity comes from: `"pin"` for the
#'    `crop_residues` pin, or `"malaysia_nbs_per_hectare"` /
#'    `"malaysia_nbs_per_product"` for oil palm re-anchored by
#'    `oil_palm_residue`.
#'
#' The pin's residue quantities are fresh matter. Across its crops the ratio of
#' pinned residue to product tracks the fresh-matter residue:product ratio
#' `kg_residue_kg_product_FM` of [biomass_coefs] (about 0.8 of it for nearly
#' every crop), not the residue's dry-matter content, which runs from 0.13
#' (tomato) to 1.0 (rapeseed) (whep#1215). Use `value_dm` wherever a quantity
#' is defined per unit of dry matter, such as a residue nitrogen content.
#'
#' @inheritSection whep_read_file The batch pin on the build path
#'
#' @export
#'
#' @examples
#' get_primary_residues(example = TRUE)
get_primary_residues <- function(
  example = FALSE,
  oil_palm_residue = c("per_product", "per_hectare", "pin")
) {
  oil_palm_residue <- rlang::arg_match(oil_palm_residue)
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
    .anchor_oil_palm_residue(oil_palm_residue) |>
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
# Replace the pin's oil palm residue with the Malaysian fronds-and-trunks
# anchor (whep#1424), and stamp every row with where its residue comes from.
#
# Works on the pin before its `Product` rows are dropped, because the anchor
# needs the fruit row's harvested hectares or tonnes. The sources and the
# derivation of both rates are in the `oil_palm_residue` documentation of
# `get_primary_residues()`. The anchored quantity is dry matter; it is stored
# as fresh mass through the same `Residue_kgDM_kgFM` that
# `.add_residue_dm_content()` applies later, so `value_dm` comes out equal to
# the anchor.
.anchor_oil_palm_residue <- function(pin, method) {
  pin <- dplyr::mutate(pin, method_residue = "pin")
  if (method == "pin" || !rlang::has_name(pin, "name_biomass")) {
    return(pin)
  }
  is_palm <- pin$name_biomass %in% "Oil palm"
  if (!any(is_palm)) {
    return(pin)
  }
  palm <- .oil_palm_anchored_residue(pin[is_palm, ], method)
  dplyr::bind_rows(pin[!is_palm, ], palm)
}

.oil_palm_anchored_residue <- function(palm, method) {
  # Malaysia, 2010: fronds 46 + trunks 14 Mt DM (National Biomass Strategy
  # 2020, Exhibit 1) on 4.85 Mha planted (p. 10), or over FAOSTAT's 83,090,935
  # t of fresh fruit bunches.
  rate <- switch(
    method,
    per_hectare = 60e6 / 4.85e6,
    per_product = 60e6 / 83090935
  )
  stamp <- paste0("malaysia_nbs_", method)
  basis <- if (method == "per_hectare") "area_ygpit_ha" else "prod_ygpit_mg"
  kgdm <- .oil_palm_residue_kgdm()
  fruit <- .oil_palm_fruit_basis(palm, basis)
  residue <- dplyr::filter(palm, .data$product_residue == "Residue")
  .check_oil_palm_fruit(residue, fruit)
  residue |>
    dplyr::left_join(
      fruit,
      by = c("year", "area"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      prod_ygpit_mg = .data$fruit_basis * rate / kgdm,
      method_residue = stamp
    ) |>
    dplyr::select(-"fruit_basis") |>
    dplyr::bind_rows(dplyr::filter(palm, .data$product_residue != "Residue"))
}

.oil_palm_fruit_basis <- function(palm, basis) {
  if (!rlang::has_name(palm, basis)) {
    cli::cli_abort(
      c(
        "The crop-residue pin has no {.field {basis}} column, which the oil
         palm residue anchor needs.",
        i = "Use {.code oil_palm_residue = \"pin\"} to keep the pin's own
             oil palm residue."
      ),
      class = "whep_oil_palm_no_product"
    )
  }
  palm |>
    dplyr::filter(.data$product_residue == "Product") |>
    dplyr::summarise(
      fruit_basis = sum(.data[[basis]]),
      .by = c("year", "area")
    )
}

# A residue row with no fruit row would come out `NA` here, then sum to zero
# and be dropped by `get_primary_residues()`: an absent input turned into no
# residue. Refuse instead.
.check_oil_palm_fruit <- function(residue, fruit) {
  orphan <- dplyr::anti_join(residue, fruit, by = c("year", "area"))
  if (nrow(orphan) == 0L) {
    return(invisible(NULL))
  }
  labels <- unique(paste(orphan$area, orphan$year))
  cli::cli_abort(
    c(
      "{nrow(orphan)} oil palm residue row{?s} ha{?s/ve} no fruit row to
       anchor on.",
      i = "Area and year: {.val {labels}}.",
      i = "Use {.code oil_palm_residue = \"pin\"} to keep the pin's own oil
           palm residue."
    ),
    class = "whep_oil_palm_no_product"
  )
}

.oil_palm_residue_kgdm <- function(biomass_coefs = whep::biomass_coefs) {
  kgdm <- biomass_coefs$Residue_kgDM_kgFM[
    biomass_coefs$Name_biomass %in% "Oil palm"
  ]
  if (length(kgdm) != 1L || is.na(kgdm) || kgdm <= 0) {
    cli::cli_abort(
      "{.field biomass_coefs} has no single positive oil palm
       {.field Residue_kgDM_kgFM}, which the oil palm residue anchor needs."
    )
  }
  kgdm
}

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
