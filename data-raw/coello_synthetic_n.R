# Builds whep::coello_synthetic_n: crop-specific synthetic-N application
# rates (kg N/ha) by (year, area_code, item_cbs_code), 1961-2023.
#
# Source: Coello et al. (2025) "A global gridded crop-specific
# fertilization dataset from 1961 to 2019", Sci Data 12:40,
# doi:10.1038/s41597-024-04215-x (Author Correction
# 10.1038/s41597-025-04591-y). The column predicted_N_avg_app_cor is the
# CORRECTED average synthetic-N application rate in kg N/ha (the quantity the
# Author Correction revised); this is the same column and unit the package's
# existing offline ingestion (.prepare_coello_inputs_local() in
# inst/scripts/prepare_spatialize_all.R) reads as kg_n_ha_coello, confirming
# the semantics. Coello's 13 crop groups are crosswalked to CBS items via
# inst/extdata/coello_mapping.csv + whep::items_prod_full. Native years
# 1961-2019 are carried forward to 2023 with whep::fill_linear (rate held at
# the 2019 value; decision 6).
#
# Data-quality safeguard (user decision 2026-07-12): Coello's corrected column
# carries model-extrapolation outliers concentrated in a few small areas (e.g.
# FAOSTAT area 99, ~1966-1975, up to ~37000 kg N/ha; ~0.5% of rows exceed
# 1000 kg/ha). Rates above implausible_kg_n_ha (1000 kg N/ha, well above any
# real agronomic rate) are treated as MISSING here, not capped: the missing
# (crop, area, year) then follow the builder's existing missing-rate handling
# -- temporal interpolation/carry-forward of the crop's own plausible rates
# where available, else the .n_crop_rate_shares() fallback (which imputes the
# (year, area_code) mean rate in a covered year, or plain area weights in a
# fully-uncovered one). Conservation is unaffected either way: the downstream
# rate-weighted crop share normalises to 1 within each (year, area_code), so
# the FAOSTAT national synthetic-N total is always conserved; the safeguard
# only prevents an artifact from skewing the relative per-crop split within its
# area (decision 14, per-crop granularity).
#
# The raw CSV lives off-repo under L_files (never committed; >50 MB rule).
# Read via WHEP_COELLO_DIR with a documented home fallback and base read.csv
# (data.table::fread can segfault via Git Bash on this machine).

pkgload::load_all(".", quiet = TRUE)

coello_dir <- Sys.getenv(
  "WHEP_COELLO_DIR",
  file.path(path.expand("~"), "OneDrive", "L_files", "Coello2025")
)
csv_file <- file.path(coello_dir, "Prediction_corrected.csv")
if (!file.exists(csv_file)) {
  cli::cli_abort(c(
    "Coello CSV not found at {.path {csv_file}}.",
    i = "Set {.envvar WHEP_COELLO_DIR} to the Coello2025 folder."
  ))
}

raw <- utils::read.csv(csv_file, stringsAsFactors = FALSE) |>
  tibble::as_tibble()

# Rates above this are Coello model-extrapolation artifacts, not real
# agronomic rates; treated as missing (see header safeguard).
implausible_kg_n_ha <- 1000

coello_group <- raw |>
  dplyr::transmute(
    year = as.integer(Year),
    area_code = as.integer(FAOStat_area_code),
    coello_crop_code = as.character(Crop_Code),
    kg_n_ha = dplyr::if_else(
      as.numeric(predicted_N_avg_app_cor) > implausible_kg_n_ha,
      NA_real_,
      pmax(as.numeric(predicted_N_avg_app_cor), 0)
    )
  ) |>
  dplyr::filter(!is.na(year), !is.na(area_code), !is.na(kg_n_ha))

mapping <- readr::read_csv(
  here::here("inst", "extdata", "coello_mapping.csv"),
  show_col_types = FALSE
) |>
  dplyr::transmute(
    item_prod_code = as.integer(item_prod_code),
    coello_crop_code = as.character(coello_crop_code)
  )

prod_to_cbs <- whep::items_prod_full |>
  dplyr::transmute(
    item_prod_code = as.integer(item_prod_code),
    item_cbs_code = as.integer(item_cbs_code)
  ) |>
  dplyr::filter(!is.na(item_prod_code), !is.na(item_cbs_code)) |>
  dplyr::distinct()

coello_synthetic_n <- coello_group |>
  dplyr::inner_join(
    mapping,
    by = "coello_crop_code",
    relationship = "many-to-many"
  ) |>
  dplyr::inner_join(prod_to_cbs, by = "item_prod_code") |>
  dplyr::summarise(
    kg_n_ha = mean(kg_n_ha, na.rm = TRUE),
    .by = c(year, area_code, item_cbs_code)
  ) |>
  tidyr::complete(
    tidyr::nesting(area_code, item_cbs_code),
    year = 1961:2023
  ) |>
  whep::fill_linear(
    value_col = kg_n_ha,
    time_col = year,
    interpolate = TRUE,
    fill_forward = TRUE,
    fill_backward = FALSE,
    .by = c("area_code", "item_cbs_code")
  ) |>
  dplyr::filter(!is.na(kg_n_ha)) |>
  dplyr::transmute(
    year = as.integer(year),
    area_code = as.integer(area_code),
    item_cbs_code = as.integer(item_cbs_code),
    kg_n_ha = kg_n_ha
  ) |>
  dplyr::arrange(area_code, item_cbs_code, year)

usethis::use_data(coello_synthetic_n, overwrite = TRUE, compress = "xz")
