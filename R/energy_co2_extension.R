#' Build the livestock energy-use CO2 footprint extension (meat only).
#'
#' @description
#' Aggregate GLEAM 3.0 on-farm (direct) and feed-production (embedded) energy
#' use into a footprint extension keyed by `(year, area_code, item_cbs_code)`,
#' expressed in kilograms of carbon-dioxide equivalent (CO2e). This is the
#' energy slice of the livestock greenhouse-gas basket and is designed to be
#' summed with [build_livestock_ghg_extension()] (enteric and manure CH4/N2O),
#' which keys on the same live-animal sectors.
#'
#' The GLEAM energy emission factors are expressed per kilogram of **live
#' weight** (see [gleam_energy_use_ef]), which is well defined for meat but not
#' for milk or eggs, so the extension covers **meat only**: bovine
#' (`item_cbs_code` 961 non-dairy cattle and 946 buffalo), sheep (976) and goat
#' (1016), pig (1049 and 1051) and broiler-chicken (1053) meat. Milk and eggs
#' keep their CH4/N2O but get no energy CO2.
#'
#' For each meat group the live weight produced is recovered from FAOSTAT
#' carcass production divided by a GLEAM dressing fraction
#' ([gleam_dressing_percentages]), multiplied by a per-country energy intensity
#' (embedded + direct), and then attributed to the contributing live-animal
#' sectors in proportion to their slaughtered head counts. Because GLEAM reports
#' its factors by production system and climate zone but the package has no
#' country-level system or climate shares, the intensities are collapsed to one
#' value per country by an unweighted mean across systems and climate zones;
#' this choice is recorded in `method_energy`.
#'
#' `gleam_geographic_hierarchy` is the country universe of the whole extension,
#' so a reporting area absent from it gets no energy intensity and its meat
#' production leaves the extension. That affects both the aggregate reporting
#' buckets (`polity_area_code` 999 "Rest of World" and the continental residuals
#' 901-906) and the dissolved entities GLEAM's present-day country table cannot
#' carry (USSR, Czechoslovakia, Yugoslavia, Belgium-Luxembourg, Serbia and
#' Montenegro). The size of that loss is now **reported** on every build rather
#' than left to be inferred, and `unclassified = "global_mean"` prices those
#' areas at the world-mean GLEAM intensity instead of losing them. The default
#' keeps the historical behaviour; see whep#492.
#'
#' @param method Estimation method. Only `"gleam"` (default), the GLEAM 3.0
#'   per-live-weight factors, is currently available.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output). It falls back to
#'   its reader when absent.
#' @param unclassified How to treat reporting areas `gleam_geographic_hierarchy`
#'   has no row for, and which therefore get no country energy intensity.
#'   `"drop"` (default) keeps the historical behaviour: their meat production
#'   leaves the extension, and a warning says how much. `"global_mean"` prices
#'   them at the unweighted world mean of the published GLEAM factors instead,
#'   marking those rows `"GLEAM_3.0_energy_meat_global_mean"` in
#'   `method_energy`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (energy-use emissions in kilograms CO2e) and `method_energy`
#'   (`"GLEAM_3.0_energy_meat"`, or `"GLEAM_3.0_energy_meat_global_mean"` for
#'   rows priced at the world mean), plus the polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' build_energy_co2_extension(example = TRUE)
build_energy_co2_extension <- function(
  method = c("gleam"),
  data = list(),
  unclassified = c("drop", "global_mean"),
  example = FALSE
) {
  method <- match.arg(method)
  unclassified <- rlang::arg_match(unclassified)
  if (isTRUE(example)) {
    return(.example_energy_co2_extension())
  }

  primary_prod <- if (is.null(data$primary_prod)) {
    get_primary_production()
  } else {
    data$primary_prod
  }

  intensity <- .energy_intensity_by_country()
  primary_prod |>
    .energy_co2e_by_group(intensity, unclassified) |>
    .energy_allocate_to_sectors(primary_prod) |>
    .energy_finalise_extension(method) |>
    .add_reporting_polity_columns()
}

# ---- meat-group definitions ----------------------------------------------

# One row per meat group: the carcass `item_cbs_code`, the GLEAM species to
# read embedded and direct energy factors from, the herd (shared by both
# stages) and the embedded grouping scheme (direct is always the detailed-15
# scheme).
.energy_meat_groups <- function() {
  tibble::tribble(
    ~grp, ~meat_code, ~emb_species, ~dir_species, ~herd, ~emb_scheme,
    "bovine", 2731L, "cattle", "large_ruminants", "non_dairy", "development3",
    "mutton_goat", 2732L, "small_ruminants", "small_ruminants", "non_dairy",
    "development3",
    "pig", 2733L, "pigs", "pigs", NA_character_, "region5",
    "poultry", 2734L, "chickens", "chickens", "broilers", "region5"
  )
}

# The live-animal sectors that share each meat group's CO2e, in proportion to
# slaughtered head counts (same energy intensity, so this only sets the key).
.energy_sector_map <- function() {
  tibble::tribble(
    ~grp, ~item_cbs_code,
    "bovine", 961L,
    "bovine", 946L,
    "mutton_goat", 976L,
    "mutton_goat", 1016L,
    "pig", 1049L,
    "pig", 1051L,
    "poultry", 1053L
  )
}

# ---- country -> grouping crosswalk ----------------------------------------

# UN list of Least Developed Countries (2021), used by the GLEAM
# "development3" grouping (OECD / least developed / others).
.energy_ldc_iso3 <- function() {
  c(
    "AFG",
    "AGO",
    "BGD",
    "BEN",
    "BTN",
    "BFA",
    "BDI",
    "KHM",
    "CAF",
    "TCD",
    "COM",
    "COD",
    "DJI",
    "ERI",
    "ETH",
    "GMB",
    "GIN",
    "GNB",
    "HTI",
    "KIR",
    "LAO",
    "LSO",
    "LBR",
    "MDG",
    "MWI",
    "MLI",
    "MRT",
    "MOZ",
    "MMR",
    "NPL",
    "NER",
    "RWA",
    "STP",
    "SEN",
    "SLE",
    "SLB",
    "SOM",
    "SSD",
    "SDN",
    "TLS",
    "TGO",
    "TUV",
    "UGA",
    "TZA",
    "YEM",
    "ZMB"
  )
}

# Say which reporting areas the GLEAM schemes cannot classify, because the
# alternative is nothing at all.
#
# `gleam_geographic_hierarchy` defines the country universe for the whole energy
# extension: all three schemes below are derived from it, so an area absent from
# that table gets no row -- not a wrong group, no group -- and the intensity join
# in `.energy_co2e_by_group()` then loses its production. How much is lost is
# reported per build by `.report_unpriced_meat()`; this warning is about the
# crosswalk gap itself and fires whether or not the area produces anything.
#
# Only areas WHEP treats as a polity in their own right are worth naming.
# Dissolved entities such as SUN and YUG are absent from a present-day table by
# construction, the regional buckets RAFR to ROW are aggregates a country table
# should not carry, and territories folded into a FABIO bucket are represented by
# that bucket rather than by themselves. What remains on the current crosswalk is
# Nauru and Tuvalu: they exist today, report under their own area codes, and are
# unclassifiable. Tuvalu is the sharpest case -- `.energy_ldc_iso3()` lists TUV
# as least-developed, so this file asserts a classification for a country the
# table it joins against cannot represent.
#
# whep#415 lists Bermuda, Guam and Palau alongside those two. They are no longer
# named here because the crosswalk now folds all three into FABIO bucket 999, so
# they are no longer self-reporting; their raw area codes 17, 88 and 180 still
# carry an iso3 GLEAM has no row for. Bucket 999 carries their production
# instead, and it has no GLEAM row either -- that loss is whep#492 and is
# reported by size in `.report_unpriced_meat()`.
#
# This WARNS rather than assigning a group. Which GLEAM region an area belongs to
# is a modelling decision tracked in whep#415, and the source table is not
# edited either: it is parsed from the GLEAM Excel workbook, so adding rows would
# make this package's copy diverge from the published source with nothing
# recording it.
.warn_areas_gleam_cannot_group <- function() {
  # `area_code == polity_area_code` keeps the areas that report as themselves,
  # dropping those aggregated into a FABIO bucket under another code.
  gaps <- polity_area_crosswalk |>
    tibble::as_tibble() |>
    dplyr::filter(
      !is.na(.data$area_code),
      !is.na(.data$area_iso3c),
      .data$area_code == .data$polity_area_code,
      .data$polity_type != "aggregate",
      !is.na(.data$polity_end_year),
      .data$polity_end_year >= 2020,
      !.data$area_iso3c %in% gleam_geographic_hierarchy$iso3
    ) |>
    dplyr::distinct(.data$area_code, .data$area_name)
  if (nrow(gaps) == 0L) {
    return(invisible(NULL))
  }
  n_gaps <- nrow(gaps)
  areas <- sort(gaps$area_name)
  cli::cli_warn(c(
    "!" = "GLEAM cannot classify {n_gaps} live reporting area{?s}: no row in
       {.field gleam_geographic_hierarchy}, so the energy extension drops their
       production.",
    "i" = "Areas: {.val {areas}}.",
    "i" = "Assigning them a GLEAM region is a modelling decision; see whep#415."
  ))
  invisible(NULL)
}

# Map each country to its grouping under each of the three GLEAM schemes.
.energy_country_grouping <- function() {
  ldc <- .energy_ldc_iso3()
  .warn_areas_gleam_cannot_group()
  gleam_geographic_hierarchy |>
    dplyr::transmute(
      iso3 = .data$iso3,
      development3 = dplyr::case_when(
        .data$oecd == 1 ~ "OECD",
        .data$iso3 %in% ldc ~ "Least developed countries",
        .default = "Others"
      ),
      region5 = dplyr::case_when(
        .data$oecd == 1 ~ "OECD",
        .data$continent == "Africa" ~ "Africa",
        .data$continent == "Americas" ~
          "Non-OECD Latin America and the Caribbean",
        .data$continent == "Europe" ~ "Non-OECD Europe",
        .default = "Non-OECD Asia"
      ),
      detailed15 = dplyr::case_when(
        .data$iso3 == "USA" ~ "United States",
        .data$iso3 == "CAN" ~ "Canada",
        .data$iso3 == "AUS" ~ "Australia",
        .data$iso3 == "JPN" ~ "Japan",
        .data$iso3 == "KOR" ~ "South Korea",
        .data$iso3 == "NZL" ~ "New Zealand",
        .data$iso3 == "RUS" ~ "Russian Federation",
        .data$eu27 == 1 ~ "EU 27",
        .data$iso3 %in% c("TUR", "ISR") ~ "OECD Europe",
        .data$oecd == 1 & .data$continent == "Europe" ~ "OECD Europe",
        .data$continent == "Europe" ~ "Non-OECD Europe",
        .data$continent == "Americas" ~ "Latin America and the Caribbean",
        .data$continent == "Africa" ~ "Africa",
        .data$faostat_region == "Western Asia" ~ "Middle East",
        .data$continent == "Asia" ~ "Asia",
        .data$continent == "Oceania" ~ "Non-OECD Pacific",
        .default = "Asia"
      )
    )
}

# ---- energy intensity per (iso3, group) -----------------------------------

# Total live-weight energy intensity (kg CO2e / kg LW) for every meat group and
# country: embedded + direct, each collapsed across system and climate.
.energy_intensity_by_country <- function() {
  xwalk <- .energy_country_grouping()
  .energy_meat_groups() |>
    purrr::pmap(function(grp, emb_species, dir_species, herd, emb_scheme, ...) {
      emb <- .energy_mean_factor(emb_species, herd, "embedded") |>
        .energy_join_scheme(xwalk, emb_scheme, "ef_embedded")
      dir <- .energy_mean_factor(dir_species, herd, "direct") |>
        .energy_join_scheme(xwalk, "detailed15", "ef_direct")
      emb |>
        dplyr::full_join(dir, by = "iso3") |>
        dplyr::mutate(
          grp = grp,
          ef_total = dplyr::coalesce(.data$ef_embedded, 0) +
            dplyr::coalesce(.data$ef_direct, 0)
        )
    }) |>
    purrr::list_rbind() |>
    dplyr::select("iso3", "grp", "ef_total")
}

# World-mean live-weight energy intensity per meat group (kg CO2e / kg LW): the
# unweighted mean of the published GLEAM factors over that species' groupings,
# embedded plus direct.
#
# This is the same unweighted collapse the country factors already apply over
# production systems and climate zones (see the function documentation), taken
# one dimension further because the package has no production shares to weight
# groupings by either. It is the direct analogue of the global-mean dressing
# fraction `.energy_join_dressing()` already falls back to, and introduces no
# value that is not in `gleam_energy_use_ef`.
#
# Used only for `unclassified = "global_mean"`; nothing reaches it by default.
.energy_global_intensity <- function() {
  .energy_meat_groups() |>
    purrr::pmap(function(grp, emb_species, dir_species, herd, ...) {
      emb <- .energy_mean_factor(emb_species, herd, "embedded")
      dir <- .energy_mean_factor(dir_species, herd, "direct")
      tibble::tibble(grp = grp, ef_global = mean(emb$ef) + mean(dir$ef))
    }) |>
    purrr::list_rbind()
}

# Mean live-weight emission factor per GLEAM grouping for one species/herd and
# energy stage (collapsing the system and climate dimensions).
.energy_mean_factor <- function(species_f, herd_f, etype) {
  out <- gleam_energy_use_ef |>
    dplyr::filter(
      .data$species == species_f,
      .data$energy_type == etype,
      .data$denominator == "lw"
    )
  if (!is.na(herd_f)) {
    out <- dplyr::filter(out, .data$herd == herd_f)
  }
  out |>
    dplyr::summarise(ef = mean(.data$emission_factor), .by = "grouping")
}

# Join a per-grouping factor onto the country crosswalk via one scheme column,
# renaming the factor to `value_col`.
.energy_join_scheme <- function(factors, xwalk, scheme, value_col) {
  xwalk |>
    dplyr::select("iso3", grouping = dplyr::all_of(scheme)) |>
    dplyr::left_join(factors, by = "grouping") |>
    dplyr::transmute(.data$iso3, !!value_col := .data$ef)
}

# ---- carcass -> live weight -> CO2e ---------------------------------------

# Carcass production (tonnes) per meat group converted to live weight and to
# CO2e, keyed by (year, area_code, grp).
.energy_co2e_by_group <- function(
  primary_prod,
  intensity,
  unclassified = "drop"
) {
  groups <- .energy_meat_groups()
  area2iso <- .energy_area_iso3()
  dressing <- .energy_dressing_by_group()

  primary_prod |>
    dplyr::filter(
      .data$item_cbs_code %in% groups$meat_code,
      .data$unit == "tonnes"
    ) |>
    dplyr::inner_join(
      dplyr::select(groups, "meat_code", "grp"),
      by = c("item_cbs_code" = "meat_code")
    ) |>
    dplyr::summarise(
      carcass_t = sum(.data$value, na.rm = TRUE),
      .by = c("year", "area_code", "grp")
    ) |>
    dplyr::inner_join(area2iso, by = "area_code") |>
    .energy_join_dressing(dressing) |>
    dplyr::left_join(intensity, by = c("iso3", "grp")) |>
    .energy_price_unclassified(unclassified) |>
    dplyr::mutate(
      co2e_kg = .data$carcass_t * 1000 / .data$dressing * .data$ef_total
    ) |>
    dplyr::select("year", "area_code", "grp", "co2e_kg", "ef_scope")
}

# Report the meat production no GLEAM grouping can price -- and, when the caller
# asks for it, price it at the world mean instead of losing it (whep#492).
#
# `ef_scope` records which factor each row used, so the coarser treatment is
# never a silent fallback: it rides through `.energy_allocate_to_sectors()` -- a
# property of the (year, area, group) intensity row, and each `item_cbs_code`
# belongs to exactly one group, so grouping on it there cannot split a key --
# and ends up in `method_energy`.
.energy_price_unclassified <- function(priced, unclassified) {
  .report_unpriced_meat(priced, unclassified)
  if (identical(unclassified, "drop")) {
    return(dplyr::mutate(priced, ef_scope = "country"))
  }
  priced |>
    dplyr::left_join(.energy_global_intensity(), by = "grp") |>
    dplyr::mutate(
      ef_scope = dplyr::if_else(is.na(.data$ef_total), "global", "country"),
      ef_total = dplyr::coalesce(.data$ef_total, .data$ef_global)
    )
}

# Say how much meat production the intensity join cannot price, and how much of
# the input that is. Without this the loss is invisible: the rows keep an NA
# `ef_total`, `.energy_allocate_to_sectors()` turns that into a zero, and
# `.energy_finalise_extension()`'s `impact_u > 0` filter then removes them.
.report_unpriced_meat <- function(priced, unclassified) {
  unpriced <- dplyr::filter(priced, is.na(.data$ef_total))
  if (nrow(unpriced) == 0L) {
    return(invisible(NULL))
  }
  .warn_unpriced_meat(
    .energy_unpriced_summary(unpriced),
    sum(priced$carcass_t, na.rm = TRUE),
    unclassified
  )
}

# One row per unpriceable reporting area, largest producer first.
.energy_unpriced_summary <- function(unpriced) {
  unpriced |>
    dplyr::summarise(
      carcass_t = sum(.data$carcass_t, na.rm = TRUE),
      .by = c("area_code", "iso3")
    ) |>
    dplyr::arrange(dplyr::desc(.data$carcass_t)) |>
    add_area_name() |>
    dplyr::mutate(
      area_name = dplyr::coalesce(
        .data$area_name,
        as.character(.data$area_code)
      )
    )
}

.warn_unpriced_meat <- function(unpriced, total_t, unclassified) {
  n_areas <- nrow(unpriced)
  share <- .energy_share_label(sum(unpriced$carcass_t), total_t)
  largest <- .energy_unpriced_labels(unpriced)
  if (identical(unclassified, "drop")) {
    cli::cli_warn(c(
      "!" = "{.field gleam_geographic_hierarchy} has no row for {n_areas}
         reporting area{?s}, so {share} of the meat carcass production in this
         input gets no energy intensity and leaves the extension.",
      "i" = "Largest: {largest}.",
      "i" = "Set {.arg unclassified} to {.val global_mean} to price them at the
         world-mean GLEAM intensity instead. Which treatment is right is a
         modelling decision; see whep#492."
    ))
    return(invisible(NULL))
  }
  cli::cli_inform(c(
    "i" = "{.field gleam_geographic_hierarchy} has no row for {n_areas}
       reporting area{?s}: {share} of the meat carcass production in this input
       is priced at the world-mean GLEAM intensity, not a country factor.",
    "i" = "Largest: {largest}."
  ))
  invisible(NULL)
}

# "USSR (436.8 Mt); Belgium-Luxembourg (43.9 Mt); ...", largest first and capped
# so the message stays readable. Pre-collapsed rather than handed to cli as a
# vector, so the trailing count does not read as one more area name.
.energy_unpriced_labels <- function(unpriced, max_areas = 6L) {
  shown <- dplyr::slice_head(unpriced, n = max_areas)
  labels <- paste0(
    shown$area_name,
    " (",
    .energy_megatonnes(shown$carcass_t),
    ")"
  )
  omitted <- nrow(unpriced) - nrow(shown)
  if (omitted == 0L) {
    return(paste(labels, collapse = "; "))
  }
  paste0(paste(labels, collapse = "; "), "; and ", omitted, " smaller")
}

.energy_megatonnes <- function(tonnes) {
  paste(format(round(tonnes / 1e6, 1), trim = TRUE), "Mt")
}

.energy_share_label <- function(part_t, total_t) {
  pct <- if (total_t > 0) 100 * part_t / total_t else NA_real_
  paste0(
    .energy_megatonnes(part_t),
    " (",
    format(round(pct, 2), trim = TRUE),
    "%)"
  )
}

# Attach a dressing fraction per (grp, country) with a global-mean fallback.
.energy_join_dressing <- function(data, dressing) {
  global <- dressing |>
    dplyr::summarise(dressing_g = mean(.data$dressing), .by = "grp")
  iso2reg <- gleam_geographic_hierarchy |>
    dplyr::select("iso3", reg = "gleam_region")
  data |>
    dplyr::left_join(iso2reg, by = "iso3") |>
    dplyr::left_join(dressing, by = c("grp", "reg")) |>
    dplyr::left_join(global, by = "grp") |>
    dplyr::mutate(dressing = dplyr::coalesce(.data$dressing, .data$dressing_g))
}

# Mean dressing fraction (carcass / live weight) per meat group and GLEAM
# region, from the species/system breakdown in `gleam_dressing_percentages`.
.energy_dressing_by_group <- function() {
  abbrev <- .energy_region_abbrev()
  gleam_dressing_percentages |>
    dplyr::mutate(
      grp = dplyr::case_when(
        .data$species == "Cattle" & .data$production_system == "Beef" ~
          "bovine",
        .data$species %in% c("Sheep", "Goats") ~ "mutton_goat",
        .data$species == "Pigs" ~ "pig",
        .data$species == "Chicken" &
          .data$production_system == "Broilers" ~ "poultry",
        .default = NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data$grp)) |>
    dplyr::mutate(
      reg = dplyr::coalesce(abbrev[.data$gleam_region], .data$gleam_region)
    ) |>
    dplyr::summarise(
      dressing = mean(.data$dressing_percent, na.rm = TRUE) / 100,
      .by = c("grp", "reg")
    )
}

# GLEAM region abbreviation -> full name used in `gleam_geographic_hierarchy`.
.energy_region_abbrev <- function() {
  c(
    "NA" = "North America",
    "RUS" = "Russian Federation",
    "WE" = "Western Europe",
    "EE" = "Eastern Europe",
    "NENA" = "West Asia & Northern Africa",
    "ESEA" = "East Asia",
    "OCE" = "Oceania",
    "SA" = "South Asia",
    "LAC" = "Central & South America",
    "SSA" = "Sub-Saharan Africa"
  )
}

# Reporting area -> iso3 for the GLEAM joins. `area_iso3c` is a property of the
# reporting area, constant across the polity periods sharing an `area_code`
# (checked: one distinct value for each of the 265 mapped codes), so this needs
# no tie-breaking between those periods.
.energy_area_iso3 <- function() {
  polity_area_crosswalk |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$area_iso3c)) |>
    dplyr::distinct(area_code = .data$area_code, iso3 = .data$area_iso3c)
}

# ---- attribution to live-animal sectors -----------------------------------

# Spread each (year, area_code, grp) CO2e across its live-animal sectors in
# proportion to slaughtered head counts.
.energy_allocate_to_sectors <- function(co2e, primary_prod) {
  shares <- .energy_slaughter_shares(primary_prod)
  co2e |>
    dplyr::inner_join(shares, by = c("year", "area_code", "grp")) |>
    dplyr::mutate(impact_u = .data$co2e_kg * .data$share) |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c("year", "area_code", "item_cbs_code", "ef_scope")
    )
}

.energy_slaughter_shares <- function(primary_prod) {
  sector_map <- .energy_sector_map()
  primary_prod |>
    dplyr::filter(
      .data$unit == "slaughtered_heads",
      .data$item_cbs_code %in% sector_map$item_cbs_code
    ) |>
    dplyr::inner_join(sector_map, by = "item_cbs_code") |>
    dplyr::summarise(
      heads = sum(.data$value, na.rm = TRUE),
      .by = c("year", "area_code", "grp", "item_cbs_code")
    ) |>
    dplyr::mutate(
      share = .data$heads / sum(.data$heads),
      .by = c("year", "area_code", "grp")
    ) |>
    dplyr::select("year", "area_code", "grp", "item_cbs_code", "share")
}

# ---- finalise --------------------------------------------------------------

.energy_finalise_extension <- function(extension, method) {
  extension |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      method_energy = .energy_method_label(method, .data$ef_scope)
    ) |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      "impact_u",
      "method_energy"
    )
}

# Per row, because `unclassified = "global_mean"` prices only the areas GLEAM
# cannot classify at the world mean and leaves every other row on its country
# factor: a single label for the whole build would hide which is which.
.energy_method_label <- function(method, ef_scope = "country") {
  label <- switch(method, gleam = "GLEAM_3.0_energy_meat")
  dplyr::if_else(
    ef_scope == "global",
    paste0(label, "_global_mean"),
    label
  )
}
