#' Build the crop/soil N2O extension.
#'
#' @description
#' Estimate IPCC 2019 Tier 1 nitrous-oxide emissions from nitrogen applied to
#' managed soils, as a footprint extension keyed by `(year, area_code,
#' item_cbs_code)` in kilograms of carbon-dioxide equivalent (CO2e). This is the
#' soil-N2O analogue of [build_livestock_ghg_extension()] and feeds
#' [build_footprint()] / [compute_footprint()] the same way.
#'
#' Three nitrogen inputs to soil are included:
#' - **Synthetic fertiliser** (F_SN): FAOSTAT reports it only as a country total
#'   (tonnes N per `area_code` per year), so it is allocated to crops by the
#'   Coello 2025 rate-weighted, FAOSTAT-conserving crop share (default; the
#'   national total is preserved), or by harvested-area share when
#'   `synthetic_method = "area_share"`.
#' - **Applied manure** (F_ON): FAOSTAT "Manure applied to soils (N content)"
#'   country total, allocated to crops by harvested area (Coello is a
#'   synthetic-N rate basis only).
#' - **Crop residues** (F_CR): the dry matter of above-ground residues returned
#'   to soil (from [get_primary_residues()], net of the removed fraction) times
#'   the crop's residue nitrogen content (IPCC 2019 Table 11.1a).
#'
#' Both country totals are read under raw FAOSTAT `Area Code` values and
#' harmonised to the whep polity `area_code` through [polity_area_crosswalk]
#' before they are split to crops, so reporting units that FABIO folds into one
#' bucket are summed rather than dropped (Sudan 276 + South Sudan 277 to 206,
#' Ethiopia PDR 62 to 238, the small territories that fold into "rest of world"
#' to 999). FAOSTAT rows that are not territories carry no polity and are
#' dropped: 5000 "World", the continent, region, EU-27, OECD and income-group
#' rollups, and the "China" aggregate 351, which overlaps 41/96/128/214. This
#' is a documented exception rather than a coverage gap, since a rollup has no
#' crop shares of its own and would double count its members.
#'
#' N2O is then estimated with IPCC 2019 Refinement (Vol 4, Ch 11) Tier 1
#' factors (climate-aggregated): direct `EF1 = 0.010`; indirect via
#' volatilisation `EF4 = 0.010` applied to the volatilised fraction
#' (`FracGASF = 0.11` for synthetic, `FracGASM = 0.21` for manure; crop residues
#' do not volatilise, Eq 11.9); indirect via leaching `FracLEACH = 0.24` times
#' `EF5 = 0.011`. N2O-N is converted to N2O by 44/28 and to CO2e with the chosen
#' GWP100.
#'
#' Manure deposited by grazing animals (F_PRP, which uses the grazing EF3 on
#' pasture) and below-ground residue N are further Tier 1 inputs not yet
#' included.
#'
#' @param gwp 100-year global warming potential standard for N2O, `"ar6"`
#'   (default, 273), `"ar5"` (265) or `"ar4"` (298).
#' @param residue_removed_frac Fraction of above-ground crop residue removed
#'   from the field (for feed, fuel or construction) and therefore not returned
#'   to soil. Defaults to `0.45`, a global mid-range value; country-specific
#'   removal (`gleam_fracremove`) is a future refinement.
#' @param synthetic_method Synthetic-N crop allocation method, `"coello"` or
#'   `"area_share"`. When `NULL` (default), uses
#'   `data$synthetic_method %||% "coello"` for backwards compatibility.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` ([get_primary_production()], for harvested area),
#'   `fertilizer` (the `faostat-fertilizer-nutrients` pin), `manure` (the
#'   `faostat-emissions-livestock` pin) and `primary_residues`
#'   ([get_primary_residues()]). Each falls back to its reader when absent.
#'   `synthetic_method` selects the synthetic-N crop split, `"coello"`
#'   (default; Coello 2025 rate-weighted, FAOSTAT-conserving) or
#'   `"area_share"`; `coello_rates` overrides the rate table (shaped like
#'   [coello_synthetic_n]), defaulting to `whep::coello_synthetic_n`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (soil N2O in kilograms CO2e) and `method_soil_n2o`, plus the
#'   polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' build_crop_soil_n2o_extension(example = TRUE)
build_crop_soil_n2o_extension <- function(
  gwp = c("ar6", "ar5", "ar4"),
  residue_removed_frac = 0.45,
  synthetic_method = NULL,
  data = list(),
  example = FALSE
) {
  gwp <- match.arg(gwp)
  .check_removed_frac(residue_removed_frac)
  if (isTRUE(example)) {
    return(.example_soil_n2o_extension())
  }
  if (!is.null(synthetic_method)) {
    data$synthetic_method <- rlang::arg_match(
      synthetic_method,
      c("coello", "area_share")
    )
  }

  primary_prod <- if (is.null(data$primary_prod)) {
    get_primary_production()
  } else {
    data$primary_prod
  }
  fertilizer <- if (is.null(data$fertilizer)) {
    whep_read_file("faostat-fertilizer-nutrients")
  } else {
    data$fertilizer
  }
  manure <- if (is.null(data$manure)) {
    whep_read_file("faostat-emissions-livestock")
  } else {
    data$manure
  }
  primary_residues <- if (is.null(data$primary_residues)) {
    get_primary_residues()
  } else {
    data$primary_residues
  }

  area_shares <- .crop_area_shares(primary_prod)
  synth_shares <- .n_synthetic_crop_shares(
    primary_prod,
    data$synthetic_method %||% "coello",
    data$coello_rates
  )
  synthetic <- .synthetic_n_inputs(fertilizer, synth_shares)
  manure_n <- .manure_n_inputs(manure, area_shares)
  residue <- .residue_n_inputs(primary_residues, residue_removed_frac)
  .soil_n2o_co2e(synthetic, manure_n, residue, gwp) |>
    .add_reporting_polity_columns()
}

# Synthetic fertiliser N (tonnes N) per crop: country total split by area share.
.synthetic_n_inputs <- function(fertilizer, shares) {
  shares |>
    dplyr::inner_join(
      .synthetic_n_country(fertilizer),
      by = c("year", "area_code")
    ) |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      n_t = .data$synthetic_n_t * .data$area_share,
      method_synthetic = .data$method_synthetic
    )
}

# Applied-manure N (tonnes N) per crop: FAOSTAT country total split by area share.
.manure_n_inputs <- function(manure, shares) {
  shares |>
    dplyr::inner_join(
      .manure_applied_n_country(manure),
      by = c("year", "area_code")
    ) |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      n_t = .data$manure_applied_n_t * .data$area_share
    )
}

# Country-total manure N applied to soils (tonnes N) from the FAOSTAT emissions
# pin (reported in kg N as "Manure applied to soils (N content)").
.manure_applied_n_country <- function(manure) {
  manure |>
    dplyr::filter(
      .data$Item == "All Animals",
      .data$Element == "Manure applied to soils (N content)"
    ) |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      area_code = as.integer(.data[["Area Code"]]),
      manure_applied_n_t = .data$Value / 1000
    ) |>
    dplyr::filter(
      !is.na(.data$manure_applied_n_t),
      .data$manure_applied_n_t >= 0
    ) |>
    .n_country_to_polity("manure_applied_n_t")
}

# Country-total synthetic fertiliser N (tonnes N) from the FAOSTAT pin.
.synthetic_n_country <- function(fertilizer) {
  fertilizer |>
    dplyr::filter(
      .data$Element == "Agricultural Use",
      .data$Item == "Nutrient nitrogen N (total)"
    ) |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      area_code = as.integer(.data[["Area Code"]]),
      synthetic_n_t = .data$Value
    ) |>
    dplyr::filter(!is.na(.data$synthetic_n_t), .data$synthetic_n_t >= 0) |>
    .n_country_to_polity("synthetic_n_t")
}

# Re-key a raw-FAOSTAT country total onto the WHEP polity vocabulary, the
# `area_code` every other whep table speaks. The crop shares these totals are
# split by descend from get_primary_production(), which is aggregated to
# `polity_area_code` by .aggregate_to_polities(); joining a literal FAOSTAT
# `Area Code` against that either misses the join or lands on the wrong country.
# Reporting units that FABIO folds into one bucket are summed, exactly as
# .aggregate_to_polities() sums them: Sudan 276 + South Sudan 277 -> 206,
# Ethiopia PDR 62 -> 238, and the ~20 small territories that fold into "rest of
# world" -> 999. Measured on the 1961-2023 pins against a crop-share key set
# rebuilt the same way get_primary_production() builds it, this recovers 103
# country-years for each of the two totals, +0.234% of the joinable synthetic-N
# mass and +0.278% of the joinable applied-manure mass.
#
# `polity_area_code` is functionally determined by `area_code` (0 of the 550
# crosswalk rows disagree), so the bridge needs no year predicate; checked
# against the year-aware .add_polity_columns_dt() path over the real pins, the
# two agree to 0 on all 9883 synthetic and 11076 manure keys.
#
# Documented exception: the 40 aggregate/non-territory reporting codes in these
# two pins get no polity and are dropped -- 5000 "World", the 51xx-58xx
# continent, region, EU-27, OECD and income-group rollups, plus "China" 351,
# which overlaps 41/96/128/214. This costs no coverage. Those codes are absent
# from the crop shares too, so the inner join already discarded them, and there
# is no output resolution that could carry them: both of
# spatialize_country_n_to_crops()'s resolutions need a crop share. Keeping them
# would double count the members they roll up.
.n_country_to_polity <- function(totals, value_col) {
  # .polity_crosswalk(), not whep::polity_area_crosswalk. The dataset is the
  # pre-unfold mapping: 98 of its rows still fold into polity_area_code 999
  # (Rest of World) and it carries 203 polities, where the function applies
  # .unfold_rest_of_world() and carries 264. Reading the dataset here rebuilt a
  # Rest-of-World bucket that production no longer has, so the resulting country
  # totals had no crop-area shares to be allocated onto and
  # spatialize_country_n_to_crops() aborted on them (#446).
  bridge <- .polity_crosswalk() |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$polity_code)) |>
    dplyr::distinct(
      area_code = as.integer(.data$area_code),
      polity_area_code = as.integer(.data$polity_area_code)
    )
  totals |>
    dplyr::inner_join(bridge, by = "area_code") |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_col), sum),
      .by = c(year, polity_area_code)
    ) |>
    dplyr::rename(area_code = "polity_area_code")
}

# Each crop's share of national harvested cropland area per year (grassland
# excluded), used to split the country fertiliser total across crops. A thin
# call-through to the promoted, shared implementation in
# R/n_balance_spatialize.R (Task C6), reused by build_crop_soil_n2o_extension()
# and by spatialize_country_n_to_crops().
.crop_area_shares <- function(primary_prod) {
  .n_crop_area_shares(primary_prod)
}

# Above-ground crop-residue N returned to soil (tonnes N) per crop: residue dry
# matter (net of the removed fraction) times residue N content.
.residue_n_inputs <- function(primary_residues, removed_frac) {
  n_content <- .crop_residue_n_content()
  primary_residues |>
    dplyr::summarise(
      residue_dm_t = sum(.data$value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code_crop)
    ) |>
    dplyr::rename(item_cbs_code = item_cbs_code_crop) |>
    dplyr::left_join(n_content, by = "item_cbs_code") |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      n_t = .data$residue_dm_t *
        dplyr::coalesce(.data$n_ag, 0.008) *
        (1 - removed_frac)
    )
}

# Apply the IPCC 2019 Tier 1 soil-N2O factors to each N-input stream and sum to
# (year, area_code, item_cbs_code). Synthetic fertiliser N volatilises and
# leaches; residue N only leaches (Eq 11.9), so the two get different factors.
.soil_n2o_co2e <- function(synthetic, manure, residue, gwp) {
  ef <- .soil_n2o_factors()
  gwp_n2o <- .ghg_gwp_factors(gwp)[["n2o"]]
  to_co2e <- ef$n_to_n2o * 1000 * gwp_n2o
  # Synthetic fertiliser volatilises via FracGASF, organic manure via FracGASM;
  # crop residues do not volatilise (Eq 11.9). All three leach.
  factor_synthetic <- (ef$ef1 +
    ef$frac_gasf * ef$ef4 +
    ef$frac_leach * ef$ef5) *
    to_co2e
  factor_manure <- (ef$ef1 + ef$frac_gasm * ef$ef4 + ef$frac_leach * ef$ef5) *
    to_co2e
  factor_residue <- (ef$ef1 + ef$frac_leach * ef$ef5) * to_co2e

  dplyr::bind_rows(
    dplyr::mutate(synthetic, impact_u = .data$n_t * factor_synthetic),
    dplyr::mutate(
      manure,
      impact_u = .data$n_t * factor_manure,
      method_synthetic = NA_character_
    ),
    dplyr::mutate(
      residue,
      impact_u = .data$n_t * factor_residue,
      method_synthetic = NA_character_
    )
  ) |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      method_synthetic = {
        methods <- sort(unique(
          .data$method_synthetic[!is.na(.data$method_synthetic)]
        ))
        if (length(methods) == 0L) {
          NA_character_
        } else {
          paste(
            methods,
            collapse = "|"
          )
        }
      },
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::mutate(method_soil_n2o = paste0("IPCC_2019_Tier1_", toupper(gwp))) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      impact_u,
      method_soil_n2o,
      method_synthetic
    )
}

# IPCC 2019 Refinement (Vol 4, Ch 11) Tier 1 managed-soil N2O factors,
# climate-aggregated. Verified against Tables 11.1 and 11.3.
.soil_n2o_factors <- function() {
  list(
    ef1 = 0.010,
    frac_gasf = 0.11,
    frac_gasm = 0.21,
    frac_leach = 0.24,
    ef4 = 0.010,
    ef5 = 0.011,
    n_to_n2o = 44 / 28
  )
}

# Above-ground residue N content (kg N per kg dry matter) by crop item, from
# IPCC 2019 Table 11.1a. Crops not listed fall back to the generic 0.008.
.crop_residue_n_content <- function() {
  tibble::tribble(
    ~item_cbs_code, ~n_ag,
    2511L, 0.006, # wheat
    2513L, 0.007, # barley
    2514L, 0.006, # maize
    2515L, 0.005, # rye
    2516L, 0.007, # oats
    2517L, 0.007, # millet
    2518L, 0.007, # sorghum
    2520L, 0.006, # other cereals
    2807L, 0.007, # rice
    2546L, 0.008, # beans
    2547L, 0.008, # peas
    2549L, 0.008, # other pulses
    2555L, 0.008, # soyabeans
    2552L, 0.016, # groundnuts
    2531L, 0.019, # potatoes
    2532L, 0.019, # cassava
    2533L, 0.019 # sweet potatoes
  )
}

.check_removed_frac <- function(removed_frac) {
  if (
    !is.numeric(removed_frac) ||
      length(removed_frac) != 1L ||
      is.na(removed_frac) ||
      removed_frac < 0 ||
      removed_frac >= 1
  ) {
    cli::cli_abort("{.arg residue_removed_frac} must be in [0, 1).")
  }
}
