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

usethis::use_data(
  soc_turnover_params,
  amg_h_by_input_type,
  soil_cn_ratios,
  residue_humification,
  overwrite = TRUE
)
