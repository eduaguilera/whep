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
#' @param method Estimation method. Only `"gleam"` (default), the GLEAM 3.0
#'   per-live-weight factors, is currently available.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output). It falls back to
#'   its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (energy-use emissions in kilograms CO2e) and `method_energy`
#'   (e.g. `"GLEAM_3.0_energy_meat"`).
#'
#' @export
#'
#' @examples
#' build_energy_co2_extension(example = TRUE)
build_energy_co2_extension <- function(
  method = c("gleam"),
  data = list(),
  example = FALSE
) {
  method <- match.arg(method)
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
    .energy_co2e_by_group(intensity) |>
    .energy_allocate_to_sectors(primary_prod) |>
    .energy_finalise_extension(method)
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

# Map each country to its grouping under each of the three GLEAM schemes.
.energy_country_grouping <- function() {
  ldc <- .energy_ldc_iso3()
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
.energy_co2e_by_group <- function(primary_prod, intensity) {
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
    dplyr::mutate(
      co2e_kg = .data$carcass_t * 1000 / .data$dressing * .data$ef_total
    ) |>
    dplyr::select("year", "area_code", "grp", "co2e_kg")
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

.energy_area_iso3 <- function() {
  .current_area_lookup(include_unmapped = FALSE) |>
    tibble::as_tibble() |>
    dplyr::select("area_code", iso3 = "area_iso3c") |>
    dplyr::filter(!is.na(.data$iso3)) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
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
      .by = c("year", "area_code", "item_cbs_code")
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
      method_energy = .energy_method_label(method)
    ) |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      "impact_u",
      "method_energy"
    )
}

.energy_method_label <- function(method) {
  switch(method, gleam = "GLEAM_3.0_energy_meat")
}
