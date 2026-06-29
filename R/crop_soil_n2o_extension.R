#' Build the crop/soil N2O extension (synthetic fertiliser).
#'
#' @description
#' Estimate IPCC 2019 Tier 1 nitrous-oxide emissions from synthetic nitrogen
#' fertiliser applied to managed soils, as a footprint extension keyed by
#' `(year, area_code, item_cbs_code)` in kilograms of carbon-dioxide equivalent
#' (CO2e). This is the soil-N2O analogue of [build_livestock_ghg_extension()]
#' and feeds [build_footprint()] / [compute_footprint()] the same way.
#'
#' Synthetic fertiliser nitrogen is reported by FAOSTAT only as a country total
#' (tonnes N per `area_code` per year). It is allocated to crops in proportion
#' to each crop's harvested area within the country-year (from
#' [get_primary_production()]), so a crop with a larger share of national
#' cropland carries a larger share of the fertiliser N. N2O is then estimated
#' with IPCC 2019 Refinement (Vol 4, Ch 11) Tier 1 factors:
#' - direct: `EF1 = 0.010` kg N2O-N per kg N (Table 11.1, climate-aggregated),
#' - indirect via volatilisation: `FracGASF = 0.11` times `EF4 = 0.010`,
#' - indirect via leaching/runoff: `FracLEACH = 0.24` times `EF5 = 0.011`
#'   (Table 11.3),
#'
#' summed and converted N2O-N to N2O by 44/28, then to CO2e with the chosen
#' GWP100 (see [build_livestock_ghg_extension()] for the `gwp` options).
#'
#' This first cut covers **synthetic fertiliser** only -- the dominant managed-
#' soil N2O source. Applied manure (F_ON) and crop-residue N (F_CR) are further
#' Tier 1 inputs not yet included; they require a manure-to-soil allocation and
#' the residue-N balance respectively, and are tracked as follow-ups.
#'
#' @param gwp 100-year global warming potential standard for N2O, `"ar6"`
#'   (default, 273), `"ar5"` (265) or `"ar4"` (298).
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output, for harvested area)
#'   and `fertilizer` (the `faostat-fertilizer-nutrients` pin). Each falls back
#'   to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (synthetic-fertiliser soil N2O in kilograms CO2e) and
#'   `method_soil_n2o`.
#'
#' @export
#'
#' @examples
#' build_crop_soil_n2o_extension(example = TRUE)
build_crop_soil_n2o_extension <- function(
  gwp = c("ar6", "ar5", "ar4"),
  data = list(),
  example = FALSE
) {
  gwp <- match.arg(gwp)
  if (isTRUE(example)) {
    return(.example_soil_n2o_extension())
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

  synthetic <- .synthetic_n_country(fertilizer)
  shares <- .crop_area_shares(primary_prod)
  .synthetic_soil_n2o(shares, synthetic, gwp)
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
    dplyr::filter(!is.na(.data$synthetic_n_t), .data$synthetic_n_t >= 0)
}

# Each crop's share of national harvested cropland area per year (grassland
# excluded), used to split the country fertiliser total across crops.
.crop_area_shares <- function(primary_prod) {
  grass <- c(3000L, 3002L, 3003L)
  primary_prod |>
    dplyr::filter(
      .data$unit == "ha",
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% grass,
      .data$value > 0
    ) |>
    dplyr::summarise(
      area_ha = sum(.data$value),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::mutate(
      area_share = .data$area_ha / sum(.data$area_ha),
      .by = c(year, area_code)
    ) |>
    dplyr::select(year, area_code, item_cbs_code, area_share)
}

# Allocate synthetic N to crops and apply the IPCC 2019 Tier 1 soil-N2O factors.
.synthetic_soil_n2o <- function(shares, synthetic, gwp) {
  ef <- .soil_n2o_factors()
  n2o_n_per_t_n <- ef$ef1 +
    ef$frac_gasf * ef$ef4 +
    ef$frac_leach * ef$ef5
  gwp_n2o <- .ghg_gwp_factors(gwp)[["n2o"]]

  shares |>
    dplyr::inner_join(synthetic, by = c("year", "area_code")) |>
    dplyr::mutate(
      impact_u = .data$synthetic_n_t *
        .data$area_share *
        n2o_n_per_t_n *
        ef$n_to_n2o *
        1000 *
        gwp_n2o,
      method_soil_n2o = paste0("IPCC_2019_Tier1_synthetic_", toupper(gwp))
    ) |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u, method_soil_n2o)
}

# IPCC 2019 Refinement (Vol 4, Ch 11) Tier 1 managed-soil N2O factors,
# climate-aggregated. Verified against Tables 11.1 and 11.3.
.soil_n2o_factors <- function() {
  list(
    ef1 = 0.010,
    frac_gasf = 0.11,
    frac_leach = 0.24,
    ef4 = 0.010,
    ef5 = 0.011,
    n_to_n2o = 44 / 28
  )
}
