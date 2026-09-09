#' IPCC 2019 Tier 1 enteric CH4.
#' @noRd
.calc_enteric_ch4_tier1 <- function(data) {
  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species),
      method_enteric = "IPCC_2019_Tier1"
    )

  # Determine best EF table
  cattle_ef <- ipcc_2019_enteric_ef_cattle |>
    dplyr::rename(
      ef_cattle = ef_kg_head_yr
    )
  other_ef <- ipcc_2019_enteric_ef_other |>
    dplyr::rename(
      ef_other = ef_kg_head_yr
    )

  data <- .join_enteric_ef_tier1(data, cattle_ef, other_ef)

  n_animals <- .animal_count(data)
  data |>
    dplyr::mutate(
      enteric_ch4_tier1 = n_animals * enteric_ef_kgch4
    ) |>
    dplyr::select(-dplyr::any_of(c("ef_cattle", "ef_other")))
}

#' IPCC 2019 Tier 2 enteric CH4.
#' @noRd
.calc_enteric_ch4_tier2 <- function(data) {
  if (!rlang::has_name(data, "gross_energy")) {
    cli::cli_abort(
      "{.fun .calc_enteric_ch4_tier2} requires {.var gross_energy}. \\
       Run {.fun estimate_energy_demand} first."
    )
  }

  # The method label is set before the Ym join, not after it: `.join_ym()`
  # stamps the diet assumption onto it, and a later assignment would erase it.
  data <- data |>
    dplyr::mutate(
      species_gen = dplyr::coalesce(
        species_gen,
        .get_general_species(species)
      ),
      method_enteric = "IPCC_2019_Tier2"
    )

  data <- .join_ym(data)

  energy_conversion <- livestock_constants$energy_content_ch4_mj_kg

  n_animals <- .animal_count(data)
  data |>
    dplyr::mutate(
      enteric_ch4_per_head = gross_energy *
        (ym_factor / 100) *
        365 /
        energy_conversion,
      enteric_ch4_tier2 = n_animals * enteric_ch4_per_head
    )
}

# Private helpers ----

#' Join Tier 1 enteric EF (cattle tables have regional detail).
#' @noRd
.join_enteric_ef_tier1 <- function(data, cattle_ef, other_ef) {
  region_added <- !rlang::has_name(data, "region") &&
    .has_gleam_region_key(data)
  if (region_added) {
    data <- .add_ipcc_region(data)
  }

  # Buffalo uses Table 10.11 (other), not Table 10.10 (cattle)
  cattle_rows <- data |>
    dplyr::filter(species_gen == "Cattle")

  other_rows <- data |>
    dplyr::filter(species_gen != "Cattle")

  if (nrow(cattle_rows) > 0) {
    cattle_rows <- .join_cattle_ef(cattle_rows, cattle_ef)
  }

  if (nrow(other_rows) > 0) {
    other_rows <- .join_other_ef(other_rows, other_ef)
  }

  out <- dplyr::bind_rows(cattle_rows, other_rows) |>
    dplyr::rename(
      enteric_ef_kgch4 = dplyr::any_of(
        c("ef_cattle", "ef_other", "enteric_ef_kgch4")
      )
    )
  if (region_added) {
    out <- out |> dplyr::select(-dplyr::any_of("region"))
  }
  out
}

#' Derive the IPCC Tier-1 EF region from the row's territory.
#'
#' The IPCC 2019 Refinement EF tables (10.10, 10.14) use their own region
#' taxonomy, distinct from the GLEAM regions in gleam_geographic_hierarchy.
#' The gleam_region keys below match the values actually shipped in
#' gleam_geographic_hierarchy. Most GLEAM regions map directly; two are
#' judgement calls: GLEAM "Russian Federation" to IPCC "Eastern Europe" and
#' "West Asia & Northern Africa" to "Middle East". "Antarctica" has no IPCC
#' EF region and is intentionally left unmapped (no livestock countries).
#' @noRd
.add_ipcc_region <- function(data) {
  crosswalk <- tibble::tribble(
    ~gleam_region,
    ~region,
    "North America",
    "North America",
    "Russian Federation",
    "Eastern Europe",
    "Western Europe",
    "Western Europe",
    "Eastern Europe",
    "Eastern Europe",
    "West Asia & Northern Africa",
    "Middle East",
    "East Asia",
    "Asia",
    "Oceania",
    "Oceania",
    "South Asia",
    "Indian Subcontinent",
    "Central & South America",
    "Latin America",
    "Sub-Saharan Africa",
    "Africa"
  )
  gleam <- .gleam_region_of(data)
  data |>
    dplyr::mutate(
      region = crosswalk$region[match(gleam, crosswalk$gleam_region)]
    )
}

#' TRUE when the frame carries any key the GLEAM-region lookup can use.
#'
#' Before whep#465 the EF joins gated on `iso3` alone, so a frame keyed only by
#' its territory (`area_code` / `polity_area_code`) skipped the regional tables
#' entirely and took the Global default.
#' @noRd
.has_gleam_region_key <- function(data) {
  any(rlang::has_name(data, c("iso3", "polity_area_code", "area_code")))
}

#' GLEAM region per row, resolved from the row's territory.
#'
#' The ISO3 leg is tried first so present-day polities resolve to exactly the
#' region they resolved to before whep#465; the polity-keyed overrides only fill
#' rows the ISO3 leg left empty, which is every territory whose ISO3 is missing
#' from `gleam_geographic_hierarchy`'s 204 modern sovereign states.
#'
#' `.has_gleam_region_key()` accepts `area_code`, so the ISO3 leg reads an
#' `iso3` column when there is one and otherwise derives the ISO3 from
#' `area_code` (whep#678). Without that middle leg an `area_code`-only frame
#' matched nothing but the dissolved-federation overrides -- MEASURED: 8 of the
#' 266 reporting areas -- and every other row silently took the Global
#' emission factor, which is the failure mode whep#465 was opened to remove.
#' @noRd
.gleam_region_of <- function(data) {
  hierarchy <- gleam_geographic_hierarchy
  region <- hierarchy$gleam_region[match(.gleam_iso3_of(data), hierarchy$iso3)]

  key <- .polity_area_key_of(data)
  if (is.null(key)) {
    return(region)
  }
  overrides <- .gleam_region_overrides()
  dplyr::coalesce(
    region,
    overrides$gleam_region[match(key, overrides$polity_area_code)]
  )
}

#' The ISO3 of each row: an explicit `iso3` column, else derived from
#' `area_code`.
#'
#' The `area_code` -> ISO3 direction needs no tie-break:
#' `.current_area_lookup()` is unique by `area_code`, so one code names one
#' ISO3. (The reverse direction is the ambiguous one, and is what
#' `.iso3_area_code_bridge()` exists for.)
#' `include_unmapped = TRUE` keeps `.polity_area_key_of()`'s key space, so the
#' two legs see the same set of areas.
#'
#' An explicit `iso3` wins over the derived one so a caller that already carries
#' a harmonised ISO3 (e.g. `prepare_livestock_emissions()`) resolves exactly as
#' before. MEASURED: no reporting area whose derived ISO3 resolves against
#' `gleam_geographic_hierarchy` also has a row in `.gleam_region_overrides()`,
#' so the middle leg cannot steal a row from the overrides.
#' @noRd
.gleam_iso3_of <- function(data) {
  explicit <- if (rlang::has_name(data, "iso3")) {
    as.character(data$iso3)
  } else {
    rep(NA_character_, nrow(data))
  }
  if (!rlang::has_name(data, "area_code")) {
    return(explicit)
  }
  lookup <- .current_area_lookup(include_unmapped = TRUE)
  dplyr::coalesce(
    explicit,
    as.character(lookup$area_iso3c)[
      match(as.integer(data$area_code), lookup$area_code)
    ]
  )
}

#' The `polity_area_code` of each row, derived from `area_code` when absent.
#' @noRd
.polity_area_key_of <- function(data) {
  if (rlang::has_name(data, "polity_area_code")) {
    return(as.integer(data$polity_area_code))
  }
  if (!rlang::has_name(data, "area_code")) {
    return(NULL)
  }
  lookup <- .current_area_lookup(include_unmapped = TRUE)
  as.integer(lookup$polity_area_code)[
    match(as.integer(data$area_code), lookup$area_code)
  ]
}

#' GLEAM regions for polities that `gleam_geographic_hierarchy` does not list.
#'
#' `gleam_geographic_hierarchy` covers the 204 modern sovereign states, so every
#' dissolved state and every ISO3 outside that list fell through to the Global
#' emission factor. MEASURED on the FAOSTAT production input: the reporting
#' areas that actually carry livestock heads and resolved to no GLEAM region
#' were USSR (10.2e9 head-years), RoW (1.45e9), Yugoslav SFR (6.7e8),
#' Czechoslovakia (4.1e8), Belgium-Luxembourg (3.0e8) and Serbia and Montenegro
#' (1.1e8).
#'
#' Keyed on `polity_area_code` rather than `polity_code` because the periodised
#' code is not stable for these entities (area 228 alone carries ten USSR
#' periods, area 51 four Czechoslovak ones) while `polity_area_code` is a
#' function of the reporting area, so one row per territory suffices.
#'
#' Each region below is the one GLEAM itself assigns to ALL of the entity's
#' successor or member territories, so no weighting is involved:
#' BEL/LUX are both Western Europe; CZE/SVK are both Eastern Europe; the six
#' Yugoslav successors (HRV, SVN, SRB, BIH, MKD, MNE) are all Western Europe,
#' as are SRB/MNE for Serbia and Montenegro; GLEAM places the whole Caribbean
#' in Central & South America; and Nauru and Tuvalu are single Oceanian
#' territories GLEAM simply omits.
#'
#' The USSR is the one entity whose successors span four GLEAM regions
#' (Russian Federation, Eastern Europe, Western Europe for the Baltics, West
#' Asia & Northern Africa for Central Asia and the Caucasus). It takes the
#' region of its largest successor. MEASURED: the choice between
#' "Russian Federation" and "Eastern Europe" cannot change any published
#' number, because the IPCC crosswalk above sends both to IPCC
#' "Eastern Europe", and `gleam_animal_weights` ships rows for neither, so both
#' fall back to the same Global weights.
#'
#' Aggregates whose members span several GLEAM regions are deliberately absent
#' and keep the Global default: `polity_area_code` 999 (Rest of World, which
#' also absorbs some 40 dependent territories) is a worldwide residual for
#' which the Global average is the right answer, while the continent-scoped
#' residuals 901-906 (RAFR, RASI, REUR, RLAM, RNAM, ROCE) would each need an
#' owner decision. MEASURED: none of 901-906 carries a single livestock-head
#' row in the FAOSTAT production input, so that decision changes nothing here.
#' @noRd
.gleam_region_overrides <- function() {
  tibble::tribble(
    ~polity_area_code, ~gleam_region,
    15L,               "Western Europe", # Belgium-Luxembourg
    51L,               "Eastern Europe", # Czechoslovakia
    148L,              "Oceania", # Nauru
    151L,              "Central & South America", # Netherlands Antilles
    186L,              "Western Europe", # Serbia and Montenegro
    227L,              "Oceania", # Tuvalu
    228L,              "Russian Federation", # USSR
    248L,              "Western Europe" # Yugoslav SFR
  )
}

#' Join cattle-specific enteric EFs with regional fallback.
#' @noRd
.join_cattle_ef <- function(cattle_rows, cattle_ef) {
  cattle_category <- dplyr::case_when(
    .is_dairy(cattle_rows$species) ~ "Dairy Cattle",
    TRUE ~ "Other Cattle"
  )
  cattle_rows <- cattle_rows |>
    dplyr::mutate(cattle_category = cattle_category)

  if (rlang::has_name(cattle_rows, "region")) {
    cattle_rows <- cattle_rows |>
      dplyr::left_join(
        cattle_ef,
        by = c(
          "region" = "region",
          "cattle_category" = "category"
        )
      )
    # Global fallback for missing regions
    missing <- is.na(cattle_rows$ef_cattle)
    if (any(missing)) {
      global_ef <- cattle_ef |>
        dplyr::filter(region == "Global") |>
        dplyr::select(category, ef_global = ef_cattle)
      cattle_rows <- cattle_rows |>
        dplyr::left_join(
          global_ef,
          by = c("cattle_category" = "category")
        ) |>
        dplyr::mutate(
          ef_cattle = dplyr::coalesce(ef_cattle, ef_global)
        ) |>
        dplyr::select(-ef_global)
    }
  } else {
    global_ef <- cattle_ef |>
      dplyr::filter(region == "Global") |>
      dplyr::select(category, ef_cattle)
    cattle_rows <- cattle_rows |>
      dplyr::left_join(
        global_ef,
        by = c("cattle_category" = "category")
      )
  }

  cattle_rows |>
    dplyr::rename(enteric_ef_kgch4 = ef_cattle) |>
    dplyr::select(-cattle_category)
}

#' Join non-cattle enteric EFs.
#' Handles subcategories (e.g. "Swine - Market") by
#' matching on prefix when exact match fails.
#' @noRd
.join_other_ef <- function(other_rows, other_ef) {
  ef_tbl <- other_ef |>
    dplyr::select(category, ef_other)

  # Create aggregated EF for species with subcategories
  ef_agg <- ef_tbl |>
    dplyr::mutate(
      species_base = stringr::str_extract(
        category,
        "^[^-]+"
      ) |>
        stringr::str_trim()
    ) |>
    dplyr::summarise(
      ef_agg = mean(ef_other, na.rm = TRUE),
      .by = species_base
    )

  other_rows |>
    dplyr::left_join(
      ef_tbl,
      by = c("species_gen" = "category")
    ) |>
    dplyr::left_join(
      ef_agg,
      by = c("species_gen" = "species_base")
    ) |>
    dplyr::mutate(
      enteric_ef_kgch4 = dplyr::coalesce(ef_other, ef_agg)
    ) |>
    dplyr::select(-ef_other, -ef_agg)
}

#' Join Ym values from IPCC 2019 Table 10.13 with feed situation mapping.
#'
#' The Ym itself is never defaulted: a species with no Tier 2 Ym row keeps `NA`.
#' The former `coalesce(ym_percent, 6.5)` handed every unmatched species the
#' cattle/buffalo Ym of IPCC Table 10.13 as a bare number, which is wrong for
#' horses, mules, asses, swine and poultry -- IPCC defines no Tier 2 Ym for them
#' at all.
#'
#' A row with no diet does keep its methane, under a declared assumption: see
#' [.assume_missing_diet()]. Callers that want the diet chosen deliberately have
#' the `method_diet` ladder on [build_gridded_livestock_emissions()] and
#' [build_livestock_ghg_extension()], which resolves it before this point.
#' @noRd
.join_ym <- function(data) {
  ym_tbl <- ipcc_tier2_ym_values
  data <- .assume_missing_diet(data)

  # Ensure system column exists
  if (!rlang::has_name(data, "system")) {
    data <- data |> dplyr::mutate(system = NA_character_)
  }

  # Map diet_quality to feed_situation
  data <- data |>
    dplyr::mutate(
      feed_situation = dplyr::case_when(
        !is.na(system) & system == "Feedlot" ~ "Feedlot",
        TRUE ~ diet_quality
      )
    )

  data |>
    dplyr::left_join(
      ym_tbl,
      by = c(
        "species_gen" = "category",
        "feed_situation"
      )
    ) |>
    dplyr::rename(ym_factor = ym_percent) |>
    .warn_species_without_ym() |>
    dplyr::select(-dplyr::any_of("feed_situation"))
}

#' Declare the diet assumed for a row that carries none.
#'
#' A missing diet is not the `"Medium"` diet, but refusing the row excludes an
#' animal whose methane exists, so the row keeps its emissions and says which
#' diet it was given. `estimate_energy_demand()` already makes exactly this
#' assumption one step upstream -- an absent diet leaves `de_percent` at
#' `livestock_constants$default_de_percent`, which is the Medium diet's DE% --
#' so aborting here refused a gross energy that had already been computed under
#' the same assumption.
#'
#' MEASURED: the assumption moves no Ym. `ipcc_tier2_ym_values` gives one Ym per
#' species irrespective of diet (cattle and buffalo 6.5, sheep 6.7, goats 5.5,
#' camels 5.0); only the `"Feedlot"` situation differs, and that comes from
#' `system`, not from `diet_quality`. Where the diet does bite is DE% upstream,
#' which is why the stamp matters more than the number.
#'
#' Callers that want the choice made deliberately have the `method_diet` ladder,
#' which resolves the diet before this point and never reaches here.
#' @noRd
.assume_missing_diet <- function(data) {
  if (!rlang::has_name(data, "diet_quality")) {
    data <- dplyr::mutate(data, diet_quality = NA_character_)
  }
  gap <- is.na(data$diet_quality)
  if (!any(gap)) {
    return(data)
  }
  assumed <- .assumed_diet_quality()
  cli::cli_warn(c(
    "!" = "{sum(gap)} row{?s} {?has/have} no {.field diet_quality}.",
    i = "Assumed the IPCC {.val {assumed}} diet and stamped it in
         {.field method_enteric}.",
    i = "Resolve it with the {.arg method_diet} ladder, e.g.
         {.val national_feed}, to choose it deliberately."
  ))
  data$diet_quality[gap] <- assumed
  .stamp_assumption(data, "method_enteric", "diet_assumed_medium", gap)
}

#' The diet assumed when a row carries none.
#' @noRd
.assumed_diet_quality <- function() {
  "Medium"
}

#' Warn where a resolvable animal has no Tier 2 Ym.
#'
#' Only rows whose gross energy did resolve are worth reporting: a row with no
#' gross energy is already unresolved for other reasons and is reported by the
#' caller. A row with gross energy but no Ym is the case the old bare 6.5
#' default hid.
#' @noRd
.warn_species_without_ym <- function(data) {
  if (!rlang::has_name(data, "gross_energy")) {
    return(data)
  }
  affected <- data |>
    dplyr::filter(!is.na(gross_energy), is.na(ym_factor)) |>
    dplyr::distinct(species_gen) |>
    dplyr::pull(species_gen)
  if (length(affected) == 0L) {
    return(data)
  }
  cli::cli_warn(c(
    "!" = "No Tier 2 methane conversion factor (Ym) for
      {.val {affected}}: their enteric CH4 is {.val NA}, not zero.",
    i = "IPCC 2019 Vol.4 Ch.10 Table 10.13 defines Ym for ruminants and
         camels only; use Tier 1 for the others."
  ))
  data
}
