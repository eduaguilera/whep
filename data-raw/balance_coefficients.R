# Build the soil-balance coefficient datasets (Module B, Task B1).
#
# Reads the structural CSVs under inst/extdata/balances/ and saves them
# as package datasets:
#   - soc_turnover_params: per-model SOC turnover parameters (HSOC, RothC,
#     ICBM, AMG, Century) in long form.
#   - amg_h_by_input_type: AMG humification coefficient h by carbon input
#     type, with matching order and regex pattern.
#   - soil_cn_ratios: soil carbon-to-nitrogen ratios used to convert SOC
#     change to net nitrogen mineralization / sequestration.
#   - residue_humification: per-input-type fraction of carbon stabilised
#     directly into humus (HSOC effective humification fraction).

soc_turnover_params <- here::here(
  "inst",
  "extdata",
  "balances",
  "soc_turnover_params.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

amg_h_by_input_type <- here::here(
  "inst",
  "extdata",
  "balances",
  "amg_h_by_input_type.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

soil_cn_ratios <- here::here(
  "inst",
  "extdata",
  "balances",
  "soil_cn_ratios.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

residue_humification <- here::here(
  "inst",
  "extdata",
  "balances",
  "residue_humification.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

# Module B (Task B3 / T24) generic land-use monthly soil-cover curve feeding the
# RothC/HSOC cover factor (crop growth-stage canopy for cropland, perennial
# cover for grassland/natural). See R/datasets_balances.R @source for provenance.
soc_soil_cover_curve <- here::here(
  "inst",
  "extdata",
  "balances",
  "soc_soil_cover_curve.csv"
) |>
  readr::read_csv(show_col_types = FALSE) |>
  dplyr::mutate(months_from_peak = as.integer(months_from_peak))

# Module C (Task C1) nitrogen-loss coefficient datasets.
.read_balance_csv <- function(name) {
  here::here("inst", "extdata", "balances", name) |>
    readr::read_csv(show_col_types = FALSE)
}

n2o_efs_disaggregated <- .read_balance_csv("n2o_efs_disaggregated.csv")
fertiliser_n2o_modifiers <- .read_balance_csv("fertiliser_n2o_modifiers.csv")
meisinger_denitrification <- .read_balance_csv("meisinger_denitrification.csv")
drainage_ranges <- .read_balance_csv("drainage_ranges.csv")
subsoil_no3_reduction <- .read_balance_csv("subsoil_no3_reduction.csv")
manner_params <- .read_balance_csv("manner_params.csv")
n_attenuation_constants <- .read_balance_csv("n_attenuation_constants.csv")

# Resolve an ISO3 area label to the numeric FAOSTAT area code every other
# area_code in this package uses. Aborts on an ISO3 the crosswalk does not
# know, and on one that names two areas (ETH is both 62, Ethiopia PDR, and
# 238; SDN is both 206, former Sudan, and 276), where taking whichever row
# came first would silently date the series.
.iso3_to_area_code <- function(iso3) {
  if (!exists("polity_area_crosswalk")) {
    load(here::here("data", "polity_area_crosswalk.rda"))
  }
  codes <- polity_area_crosswalk |>
    dplyr::filter(.data$area_iso3c %in% iso3, !is.na(.data$area_code)) |>
    dplyr::distinct(.data$area_iso3c, .data$area_code)

  unknown <- setdiff(iso3, codes$area_iso3c)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Cannot resolve an ISO3 area label to a FAOSTAT area code.",
      "x" = "Not in polity_area_crosswalk: {.val {unknown}}."
    ))
  }
  ambiguous <- unique(codes$area_iso3c[duplicated(codes$area_iso3c)])
  if (length(ambiguous) > 0) {
    cli::cli_abort(c(
      "An ISO3 area label maps to more than one FAOSTAT area code.",
      "x" = "Ambiguous: {.val {ambiguous}}.",
      "i" = "Label the source rows with the era they cover."
    ))
  }

  as.integer(codes$area_code[match(iso3, codes$area_iso3c)])
}

# Module C (Task C3) urban nitrogen coefficient datasets. urban_n_reference is
# the raw Spain_Hist benchmark series (see R/datasets_balances.R @source for
# provenance). urban_kgn_cap_reference is the DERIVED per-capita rate; it is
# NOT recomputed here (see data-raw/build_urban_kgn_cap.R for how it was
# built and how to regenerate it against real HYDE data).
#
# The vendored urban_n_reference.csv labels its area with the ISO3 string
# "ESP", while every other area_code in this package is the numeric FAOSTAT
# code -- the other half of this same derivation,
# data-raw/build_urban_kgn_cap.R, keeps Spain by filtering on area code 203L.
# One concept keyed two ways, so the series could not be joined to any
# area-keyed table without a hand conversion and the column name gave no hint
# that one was needed (whep#401). The label is resolved here rather than
# rewritten in the vendored CSV, which would diverge from its source, and
# looked up in the crosswalk rather than written as a literal 203, so a
# renamed or re-coded territory becomes a build error instead of a wrong join.
urban_n_reference <- .read_balance_csv("urban_n_reference.csv") |>
  dplyr::mutate(area_code = .iso3_to_area_code(.data$area_code))
urban_kgn_cap_reference <- .read_balance_csv("urban_kgn_cap_reference.csv")

# Module C (Task C4) MANNER process-based ammonia-volatilisation coefficient
# datasets, complementing manner_params (see R/datasets_balances.R @source
# for provenance).
manner_rate_factor <- .read_balance_csv("manner_rate_factor.csv")
manner_rain_factor <- .read_balance_csv("manner_rain_factor.csv")
manner_incorporation_factor <- .read_balance_csv(
  "manner_incorporation_factor.csv"
)
manure_inorganic_n <- .read_balance_csv("manure_inorganic_n.csv")

# Module C (Task C4 follow-up) gross-assumption default for MANNER's
# technique/incorporation_delay_h drivers, used where real per-cell/per-era
# application-technique survey data does not exist (everywhere right now).
# See calculate_manner_nh3_default()'s @details in R/manner_model.R for the
# reasoning; this is a deliberate, permanent placeholder, not a temporary
# stopgap pending a specific dataset.
manner_default_technique_mix <- .read_balance_csv(
  "manner_default_technique_mix.csv"
)

# Module C (Task C5) nitrogen-loss coefficient datasets: IPCC 2006 Tier 1
# direct soil N2O emission factors (complementing n2o_efs_disaggregated's
# Cayuela/IPCC-2019 factors) and the soil organic matter content bins used
# by the Meisinger denitrification lookup.
n2o_efs_ipcc2006 <- .read_balance_csv("n2o_efs_ipcc2006.csv")
som_ranges <- .read_balance_csv("som_ranges.csv")

# Module B (Task B3 / T10b) soil-hydraulic property table by USDA texture
# class (Cosby et al. 1984 class averages) and the HWSD2 topsoil-texture-code
# to USDA-class-name crosswalk, feeding get_soc_climate_drivers()'s per-cell
# ICBM moisture drivers (t_field, t_wilt, porosity). See R/datasets_balances.R
# @source for provenance.
soil_hydraulic_by_texture <- .read_balance_csv("soil_hydraulic_by_texture.csv")
hwsd_texture_usda <- .read_balance_csv("hwsd_texture_usda.csv") |>
  dplyr::mutate(t_usda_tex = as.integer(t_usda_tex))

usethis::use_data(
  soc_turnover_params,
  amg_h_by_input_type,
  soil_cn_ratios,
  residue_humification,
  soc_soil_cover_curve,
  n2o_efs_disaggregated,
  fertiliser_n2o_modifiers,
  meisinger_denitrification,
  drainage_ranges,
  subsoil_no3_reduction,
  manner_params,
  n_attenuation_constants,
  urban_n_reference,
  urban_kgn_cap_reference,
  manner_rate_factor,
  manner_rain_factor,
  manner_incorporation_factor,
  manure_inorganic_n,
  manner_default_technique_mix,
  n2o_efs_ipcc2006,
  som_ranges,
  soil_hydraulic_by_texture,
  hwsd_texture_usda,
  overwrite = TRUE
)
