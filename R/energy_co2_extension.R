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
#' this choice is recorded in `method_energy`. A meat group with carcass output
#' but no slaughtered-head counts keeps its energy CO2e, split equally across
#' the group's live-animal sectors, and triggers a warning.
#'
#' `gleam_geographic_hierarchy` is the country universe of the whole extension,
#' so a reporting area absent from it gets no energy intensity and its meat
#' production leaves the extension. That affects both the aggregate reporting
#' buckets (`polity_area_code` 999 "Rest of World" and the continental residuals
#' 901-906) and the dissolved entities GLEAM's present-day country table cannot
#' carry (USSR, Czechoslovakia, Yugoslavia, Belgium-Luxembourg, Serbia and
#' Montenegro). The size of that loss is now **reported** on every build rather
#' than left to be inferred, and three opt-in treatments recover it instead of
#' losing it: `unclassified = "polity_region"` groups the **live** reporting
#' areas GLEAM omits (today Nauru and Tuvalu) from the polity crosswalk,
#' `unclassified = "historical_region"` additionally groups the **dissolved**
#' entities from the membership they themselves held while they existed, and
#' `unclassified = "global_mean"` prices **every** unclassifiable area at the
#' world-mean GLEAM intensity. The default keeps the historical behaviour; see
#' whep#415, whep#492 and whep#553.
#'
#' @param method Estimation method. Only `"gleam"` (default), the GLEAM 3.0
#'   per-live-weight factors, is currently available.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output). It falls back to
#'   its reader when absent.
#' @param unclassified How to treat reporting areas `gleam_geographic_hierarchy`
#'   has no row for, and which therefore get no country energy intensity.
#'   `"drop"` (default) keeps the historical behaviour: their meat production
#'   leaves the extension, and a warning says how much. `"polity_region"` gives
#'   the **live, self-reporting** ones among them a grouping derived from their
#'   polity in `polity_area_crosswalk`, running GLEAM's own scheme rules on that
#'   continent, and marks those rows `"GLEAM_3.0_energy_meat_polity_region"`;
#'   the aggregate buckets and dissolved entities still drop.
#'   `"historical_region"` does everything `"polity_region"` does and also
#'   groups the **dissolved** entities (USSR, Czechoslovakia, the Yugoslav SFR,
#'   Belgium-Luxembourg, Serbia and Montenegro, the Netherlands Antilles) from
#'   the OECD and EU membership they held while they existed, marking those rows
#'   `"GLEAM_3.0_energy_meat_historical_region"`.
#'   `"global_mean"` instead prices every unclassifiable area at the unweighted
#'   world mean of the published GLEAM factors, marking those rows
#'   `"GLEAM_3.0_energy_meat_global_mean"`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (energy-use emissions in kilograms CO2e) and `method_energy`
#'   (`"GLEAM_3.0_energy_meat"`, `"GLEAM_3.0_energy_meat_polity_region"` for
#'   rows grouped from the polity crosswalk,
#'   `"GLEAM_3.0_energy_meat_historical_region"` for dissolved entities grouped
#'   from their own era's memberships, or
#'   `"GLEAM_3.0_energy_meat_global_mean"` for rows priced at the world mean),
#'   plus the polity columns below.
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
  unclassified = c("drop", "polity_region", "historical_region", "global_mean"),
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

  hierarchy <- .energy_hierarchy(unclassified)
  intensity <- .energy_intensity_by_country(hierarchy)
  primary_prod |>
    .energy_co2e_by_group(intensity, unclassified, hierarchy) |>
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
# that bucket rather than by themselves. That used to leave Nauru and Tuvalu
# alone. Tuvalu is the sharpest case -- `.energy_ldc_iso3()` lists TUV as
# least-developed, so this file asserted a classification for a country the
# table it joins against cannot represent.
#
# whep#415 lists Bermuda, Guam and Palau alongside those two, and it took two
# separate crosswalk fixes to reach them. whep#628 promoted a Rest-of-World
# member's `polity_area_code`, so all three began reporting under their own area
# codes and `.energy_self_reporting_gaps()` picked them up -- and then the
# `polity_type` filter below dropped them again, because promotion left their
# POLITY as the ROW aggregate, on `continent = "World"`, which is not in
# `.energy_scheme_continents()`. whep#717 promoted the territorial identity too,
# and the live gap set went from 2 areas to 16.
#
# The default treatment is still `"drop"`, so this is a wider WARNING rather
# than a different number: what moved is which areas `"polity_region"` and
# `"historical_region"` can reach when a caller asks for them.
#
# The source table is not edited: it is parsed from the GLEAM Excel workbook, so
# adding rows would make this package's copy diverge from the published source
# with nothing recording it. Instead `unclassified = "polity_region"` derives the
# grouping consumer-side, from the polity crosswalk, and the default still only
# warns.
.areas_gleam_cannot_group <- function() {
  .energy_self_reporting_gaps() |>
    dplyr::filter(
      .data$polity_type != "aggregate",
      !is.na(.data$polity_end_year),
      .data$polity_end_year >= 2020
    ) |>
    dplyr::distinct(
      .data$area_code,
      .data$area_name,
      .data$area_iso3c,
      .data$polity_area_code,
      .data$continent
    )
}

# The crosswalk AS THE REST OF THE PIPELINE SEES IT.
#
# `.polity_crosswalk()` is the one place `.unfold_rest_of_world()` is applied
# (`R/polities.R`), so reading the shipped `polity_area_crosswalk` object here
# would give this file a fold state nothing else has used since whep#628 made
# `whep.unfold_rest_of_world = "all"` the default. That matters precisely
# because the gate below is `area_code == polity_area_code`, which is the one
# column the unfold moves: on the shipped table every Rest-of-World member still
# carries 999 and so never counts as reporting as itself. See whep#646.
#
# `as.data.frame()` before `as_tibble()` drops the data.table self-reference
# `.polity_crosswalk()` returns, which would otherwise ride along on every frame
# derived from it.
.energy_crosswalk <- function() {
  .polity_crosswalk() |>
    as.data.frame() |>
    tibble::as_tibble()
}

# The reporting areas that report as THEMSELVES and have no row in
# `gleam_geographic_hierarchy`. Both the live gap above and the dissolved
# entities further down are filters on this set, and each used to spell the gate
# out separately against a different read of the crosswalk.
#
# `area_code == polity_area_code` keeps the areas that report as themselves,
# dropping those aggregated into a FABIO bucket under another code.
.energy_self_reporting_gaps <- function() {
  .energy_crosswalk() |>
    dplyr::filter(
      !is.na(.data$area_code),
      !is.na(.data$area_iso3c),
      .data$area_code == .data$polity_area_code,
      !.data$area_iso3c %in% gleam_geographic_hierarchy$iso3
    )
}

.warn_areas_gleam_cannot_group <- function(unclassified = "drop") {
  gaps <- .areas_gleam_cannot_group()
  if (nrow(gaps) == 0L) {
    return(invisible(NULL))
  }
  n_gaps <- nrow(gaps)
  areas <- sort(gaps$area_name)
  if (unclassified %in% .energy_derived_treatments()) {
    cli::cli_inform(c(
      "i" = "{n_gaps} live reporting area{?s} {?has/have} no row in
         {.field gleam_geographic_hierarchy} and {?is/are} grouped from the
         polity crosswalk instead: {.val {areas}}.",
      "i" = "Those rows are labelled
         {.val GLEAM_3.0_energy_meat_polity_region}; see whep#415."
    ))
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "!" = "GLEAM cannot classify {n_gaps} live reporting area{?s}: no row in
       {.field gleam_geographic_hierarchy}, so the energy extension drops their
       production.",
    "i" = "Areas: {.val {areas}}.",
    "i" = "Set {.arg unclassified} to {.val polity_region} to group them from
       the polity crosswalk instead. Which treatment is right is a modelling
       decision; see whep#415."
  ))
  invisible(NULL)
}

# The two treatments that DERIVE a grouping for an area GLEAM omits, rather than
# dropping it or averaging the world. `"historical_region"` is a superset of
# `"polity_region"`: it adds the dissolved entities to the live areas.
.energy_derived_treatments <- function() {
  c("polity_region", "historical_region")
}

.inform_dissolved_grouped <- function() {
  rows <- .energy_dissolved_rows()
  if (nrow(rows) == 0L) {
    return(invisible(NULL))
  }
  n <- nrow(rows)
  cli::cli_inform(c(
    "i" = "{n} dissolved reporting entit{?y/ies} {?is/are} grouped from the
       OECD and EU membership {?it/they} held while {?it/they} existed:
       {.val {sort(rows$iso3)}}.",
    "i" = "Those rows are labelled
       {.val GLEAM_3.0_energy_meat_historical_region}; see whep#553."
  ))
  invisible(NULL)
}

# The country universe the whole extension is derived from: GLEAM's own table,
# plus -- only when the caller asks for it -- one row per live reporting area
# GLEAM omits, built from the polity crosswalk, and under
# `"historical_region"` one row per dissolved entity as well.
#
# `ef_scope` rides with each country so `method_energy` can say afterwards which
# rows were grouped from the crosswalk rather than read off GLEAM's table.
.energy_hierarchy <- function(unclassified = "drop") {
  .warn_areas_gleam_cannot_group(unclassified)
  hierarchy <- gleam_geographic_hierarchy |>
    tibble::as_tibble() |>
    dplyr::mutate(ef_scope = "country")
  if (!unclassified %in% .energy_derived_treatments()) {
    return(hierarchy)
  }
  derived <- .energy_polity_hierarchy_rows()
  if (identical(unclassified, "historical_region")) {
    .inform_dissolved_grouped()
    derived <- dplyr::bind_rows(derived, .energy_dissolved_rows())
  }
  dplyr::bind_rows(hierarchy, derived)
}

# Rows shaped like `gleam_geographic_hierarchy` for the live reporting areas it
# has no row for, so `.energy_country_grouping()` can run GLEAM's OWN scheme
# rules on them instead of a grouping typed in here. No group label is invented:
# each of the three schemes below is evaluated by the same `case_when()` the 204
# published countries go through.
#
# The three inputs those rules need:
#
# * `continent` comes from the polity crosswalk. Only the continents for which
#   `continent` alone settles all three schemes are eligible -- see
#   `.energy_scheme_continents()`.
# * `oecd` and `eu27` are 0 by construction: all 38 OECD members and all 27 EU27
#   members have a row in `gleam_geographic_hierarchy`, pinned by a test, so an
#   iso3 absent from that table belongs to neither.
# * `gleam_region`, which only feeds the dressing fraction, is taken from
#   `.gleam_region_overrides()` -- the same merged whep#465 decision the Tier-1
#   livestock EFs already use for these territories, rather than a second copy of
#   it. An area with no override keeps `NA` and falls back to the world-mean
#   dressing, exactly as an unknown region already did.
#
# `development3` therefore resolves per country from `.energy_ldc_iso3()`, which
# is what makes Tuvalu land on "Least developed countries" -- the classification
# this file already asserted for TUV while joining against a table that had no
# row for it.
.energy_polity_hierarchy_rows <- function() {
  overrides <- .gleam_region_overrides()
  .areas_gleam_cannot_group() |>
    dplyr::mutate(continent = .energy_gleam_continent(.data$continent)) |>
    dplyr::filter(.data$continent %in% .energy_scheme_continents()) |>
    dplyr::transmute(
      iso3 = .data$area_iso3c,
      continent = .data$continent,
      faostat_region = NA_character_,
      gleam_region = overrides$gleam_region[
        match(.data$polity_area_code, overrides$polity_area_code)
      ],
      eu27 = 0L,
      oecd = 0L,
      ef_scope = "polity_region"
    )
}

# The continents for which `continent` on its own settles all three GLEAM
# schemes once `oecd` and `eu27` are 0. Asia is deliberately absent:
# `detailed15` splits it into "Middle East" and "Asia" on `faostat_region`,
# which `polity_area_crosswalk` does not carry, and guessing which side an area
# falls on is exactly the kind of invented value this package forbids. No live
# unclassifiable area is in Asia today, so the exclusion costs nothing now; if
# one appears it stays unpriced and keeps being reported rather than being
# quietly mis-grouped.
.energy_scheme_continents <- function() {
  c("Africa", "Americas", "Europe", "Oceania")
}

# `polity_area_crosswalk` splits the Americas into "North America" and
# "South America"; `gleam_geographic_hierarchy`, whose vocabulary the scheme
# rules above are written against, has one "Americas". Without this, a crosswalk
# row for an American area would match no branch of `region5` and fall through
# its `.default` to "Non-OECD Asia". MEASURED: no area reaching either derived
# treatment today is in the Americas with production (the live pair is Nauru and
# Tuvalu, both Oceania; the Netherlands Antilles is dissolved and produces no
# meat), so this maps a latent mis-grouping, not a live one.
.energy_gleam_continent <- function(continent) {
  dplyr::case_match(
    continent,
    c("North America", "South America") ~ "Americas",
    .default = continent
  )
}

# ---- dissolved entities ----------------------------------------------------

# The self-reporting areas GLEAM omits that no longer exist: every polity period
# the crosswalk gives them ended before the crosswalk's open end. That is what
# separates them from the live omissions (Nauru, Tuvalu) and from the aggregate
# buckets 901-906 and 999, whose periods all run to the open end. Derived rather
# than listed so a crosswalk revision cannot leave a hardcoded list behind.
#
# `open_end` is read off the same unfolded crosswalk the gate uses, so the two
# cannot come from tables in different fold states (whep#646).
.energy_dissolved_areas <- function() {
  open_end <- max(.energy_crosswalk()$polity_end_year, na.rm = TRUE)
  .energy_self_reporting_gaps() |>
    dplyr::filter(!is.na(.data$polity_end_year)) |>
    dplyr::summarise(
      last_year = max(.data$polity_end_year),
      .by = c(
        "area_code",
        "area_name",
        "area_iso3c",
        "polity_area_code",
        "continent"
      )
    ) |>
    dplyr::filter(.data$last_year < open_end)
}

# The OECD and EU membership each dissolved entity held WHILE IT EXISTED. This
# is the whep#553 decision, and it is deliberately contemporaneous rather than
# inherited from the successor states as they stand today.
#
# Every value is a membership date, not an estimate:
#
# * Belgium and Luxembourg both signed the OECD Convention on 14 December 1960
#   and are founding members (in force 30 September 1961), and both are founding
#   members of the EEC (Treaty of Rome 1957, in force 1 January 1958). So
#   Belgium-Luxembourg was OECD and EU for the whole 1961-1999 span it reports.
# * No successor of Czechoslovakia, the USSR, the Yugoslav SFR or Serbia and
#   Montenegro was in either body during the entity's own lifetime: Czechia
#   joined the OECD on 21 December 1995 and Slovakia on 14 December 2000, both
#   after the 1992 dissolution, and both joined the EU on 1 May 2004; Slovenia
#   joined the OECD in 2010; no Soviet successor is an OECD member. So those
#   four are non-OECD and non-EU throughout, with no weighting between
#   successors needed -- the successors agree, they were all outside.
# * The Netherlands Antilles never held either membership in its own right: the
#   OECD member is the Kingdom of the Netherlands, and its Caribbean territories
#   are EU Overseas Countries and Territories, associated with the Union but
#   outside its territory.
#
# Taking the successors' PRESENT-DAY flags instead would make Czechoslovakia
# OECD and EU 27 in 1990, which is anachronistic; that alternative is quantified
# in whep#553 and is the open question there. An entity with no row here stays
# unpriced and keeps being reported, rather than defaulting to zeros.
.energy_dissolved_membership <- function() {
  tibble::tribble(
    ~polity_area_code, ~oecd, ~eu27,
    15L,               1L,    1L, # Belgium-Luxembourg
    51L,               0L,    0L, # Czechoslovakia
    151L,              0L,    0L, # Netherlands Antilles
    186L,              0L,    0L, # Serbia and Montenegro
    228L,              0L,    0L, # USSR
    248L,              0L,    0L # Yugoslav SFR
  )
}

# Rows shaped like `gleam_geographic_hierarchy` for the dissolved entities, so
# `.energy_country_grouping()` runs GLEAM's OWN scheme rules on them. As with
# the live areas, no grouping label is typed in here: only the three inputs
# those rules read are supplied -- the continent from the crosswalk, the
# memberships above, and the GLEAM region from the merged whep#465 override
# table (which only feeds the dressing fraction). An entity the membership
# table does not cover is dropped by the inner join and stays reported.
.energy_dissolved_rows <- function() {
  overrides <- .gleam_region_overrides()
  .energy_dissolved_areas() |>
    dplyr::inner_join(
      .energy_dissolved_membership(),
      by = "polity_area_code"
    ) |>
    dplyr::mutate(continent = .energy_gleam_continent(.data$continent)) |>
    dplyr::filter(.data$continent %in% .energy_scheme_continents()) |>
    dplyr::transmute(
      iso3 = .data$area_iso3c,
      continent = .data$continent,
      faostat_region = NA_character_,
      gleam_region = overrides$gleam_region[
        match(.data$polity_area_code, overrides$polity_area_code)
      ],
      eu27 = .data$eu27,
      oecd = .data$oecd,
      ef_scope = "historical_region"
    )
}

# Map each country to its grouping under each of the three GLEAM schemes.
.energy_country_grouping <- function(hierarchy = .energy_hierarchy()) {
  ldc <- .energy_ldc_iso3()
  hierarchy |>
    dplyr::transmute(
      iso3 = .data$iso3,
      ef_scope = .data$ef_scope,
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
.energy_intensity_by_country <- function(hierarchy = .energy_hierarchy()) {
  xwalk <- .energy_country_grouping(hierarchy)
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
    dplyr::left_join(
      dplyr::distinct(xwalk, .data$iso3, .data$ef_scope),
      by = "iso3"
    ) |>
    dplyr::select("iso3", "grp", "ef_total", "ef_scope")
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
  unclassified = "drop",
  hierarchy = .energy_hierarchy(unclassified)
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
    .energy_join_dressing(dressing, hierarchy) |>
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
  global <- identical(unclassified, "global_mean")
  # `ef_scope` is NA exactly where the intensity join found no country row, so
  # it names the treatment those rows get: dropped, or priced at the world mean.
  priced <- dplyr::mutate(
    priced,
    ef_scope = dplyr::coalesce(
      .data$ef_scope,
      if (global) "global" else "country"
    )
  )
  if (!global) {
    return(priced)
  }
  priced |>
    dplyr::left_join(.energy_global_intensity(), by = "grp") |>
    dplyr::mutate(ef_total = dplyr::coalesce(.data$ef_total, .data$ef_global))
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
  if (!identical(unclassified, "global_mean")) {
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
# `hierarchy` rather than `gleam_geographic_hierarchy` so that an area grouped
# from the polity crosswalk also takes its region's dressing fraction; without a
# region it keeps the global-mean fallback it already had.
.energy_join_dressing <- function(
  data,
  dressing,
  hierarchy = .energy_hierarchy()
) {
  global <- dressing |>
    dplyr::summarise(dressing_g = mean(.data$dressing), .by = "grp")
  iso2reg <- hierarchy |>
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
          .data$production_system == "Broilers" ~
          "poultry",
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
#
# Neither column is touched by the Rest-of-World unfold, so this reads the same
# either way; it goes through `.energy_crosswalk()` anyway so that no helper in
# this file has its own private idea of what the crosswalk is (whep#646).
.energy_area_iso3 <- function() {
  .energy_crosswalk() |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$area_iso3c)) |>
    dplyr::distinct(area_code = .data$area_code, iso3 = .data$area_iso3c)
}

# ---- attribution to live-animal sectors -----------------------------------

# Spread each (year, area_code, grp) CO2e across its live-animal sectors in
# proportion to slaughtered head counts. Every group is expanded to its full
# sector set from the sector map (via a left join to head counts) so a
# country-year is never silently dropped for lacking `slaughtered_heads` rows;
# when a group has no head counts its CO2e is split equally across its sectors.
.energy_allocate_to_sectors <- function(co2e, primary_prod) {
  heads <- .energy_slaughter_heads(primary_prod)
  allocated <- co2e |>
    # The fan-out is the point of this join: one meat group feeds several
    # live-animal sectors. `co2e` is one row per (year, area_code, grp) and the
    # map is one row per (grp, item_cbs_code), so each group recurs on both
    # sides -- many-to-many by design, not a duplicated key (whep#647).
    dplyr::inner_join(
      .energy_sector_map(),
      by = "grp",
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      heads,
      by = c("year", "area_code", "grp", "item_cbs_code")
    ) |>
    .energy_apply_head_shares()
  .energy_warn_missing_shares(allocated)
  allocated |>
    dplyr::mutate(impact_u = .data$co2e_kg * .data$share) |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c("year", "area_code", "item_cbs_code", "ef_scope")
    )
}

# Slaughtered head counts per (year, area_code, grp, item_cbs_code); may be
# absent for a group when `primary_prod` has no matching rows.
.energy_slaughter_heads <- function(primary_prod) {
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
    )
}

# Within each (year, area_code, grp), turn head counts into allocation shares.
# When the group has no positive head count, fall back to an equal split across
# its sectors (this also guards the `0 / 0 = NaN` share case).
.energy_apply_head_shares <- function(allocated) {
  allocated |>
    dplyr::mutate(
      total_heads = sum(.data$heads, na.rm = TRUE),
      share = dplyr::if_else(
        .data$total_heads > 0,
        dplyr::coalesce(.data$heads, 0) / .data$total_heads,
        1 / dplyr::n()
      ),
      .by = c("year", "area_code", "grp")
    )
}

# Warn once when any group lacked head counts and used the equal-split fallback.
.energy_warn_missing_shares <- function(allocated) {
  missing <- allocated |>
    dplyr::filter(.data$total_heads == 0) |>
    dplyr::distinct(.data$year, .data$area_code, .data$grp)
  n <- nrow(missing)
  if (n > 0) {
    cli::cli_warn(c(
      "!" = "{n} country-year meat group{?s} lack slaughtered-head counts.",
      "i" = "Their energy CO2e was split equally across the group's sectors."
    ))
  }
  invisible(allocated)
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

# Per row, because the non-default treatments touch only the areas GLEAM cannot
# classify and leave every other row on its country factor: a single label for
# the whole build would hide which is which.
.energy_method_label <- function(method, ef_scope = "country") {
  label <- switch(method, gleam = "GLEAM_3.0_energy_meat")
  dplyr::case_match(
    ef_scope,
    "global" ~ paste0(label, "_global_mean"),
    "polity_region" ~ paste0(label, "_polity_region"),
    "historical_region" ~ paste0(label, "_historical_region"),
    .default = label
  )
}
