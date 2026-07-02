# Shared Spain_Hist local-data helpers for real-data port-fidelity tests
# (skip_on_ci(), degrade gracefully when the local Spain_Hist checkout or its
# heavy L-files output directory are absent). Adapted from
# test_carbon_balance.R's "Spain_Hist port-fidelity validation" section
# (task 2C-4/C8): the two env-var resolvers below are copied verbatim so
# every fidelity test (Module B's carbon balance, Module C's nitrogen
# balance) shares one source of truth for where the local reference data
# lives, without a cross-test-file sourcing dependency (testthat only
# guarantees `helper_*.R` files load before every `test_*.R` file).

# Locate the Spain_Hist repo from an env var, falling back to the sibling
# repo layout. Never hardcode the absolute user path in tracked code.
.spain_hist_dir <- function() {
  d <- Sys.getenv("SPAIN_HIST_DIR", "")
  if (nzchar(d)) {
    return(d)
  }
  file.path(dirname(here::here()), "Spain_Hist")
}

# Spain_Hist L-files output directory (the heavy intermediate SOC/N-balance
# inputs live off-repo). From an env var, falling back to the documented
# local XL_files path.
.spain_hist_l_dir <- function() {
  Sys.getenv("SPAIN_HIST_L_DIR", "C:/XL_files/Spain_Hist_L/output")
}

# Province -> Climate ("ATL"/"MED") lookup, read read-only from Spain_Hist's
# own Province_Codes.xlsx "Codes" sheet (the exact source
# Classify_climate_cats() in n_fun.r:343-356 uses). Returns NULL when the
# file is absent so callers can skip_if() rather than fail.
.spain_province_climate <- function() {
  f <- file.path(.spain_hist_dir(), "input", "Province_Codes.xlsx")
  if (!file.exists(f)) {
    return(NULL)
  }
  openxlsx::read.xlsx(f, sheet = "Codes") |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$Province_name), !is.na(.data$Climate)) |>
    dplyr::select("Province_name", "Climate")
}
