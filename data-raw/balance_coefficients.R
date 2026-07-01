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

# Module C (Task C3) urban nitrogen coefficient datasets. urban_n_reference is
# the raw Spain_Hist benchmark series (see R/datasets_balances.R @source for
# provenance). urban_kgn_cap_reference is the DERIVED per-capita rate; it is
# NOT recomputed here (see data-raw/build_urban_kgn_cap.R for how it was
# built and how to regenerate it against real HYDE data).
urban_n_reference <- .read_balance_csv("urban_n_reference.csv")
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

usethis::use_data(
  soc_turnover_params,
  amg_h_by_input_type,
  soil_cn_ratios,
  residue_humification,
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
  overwrite = TRUE
)
