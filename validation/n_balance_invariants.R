# Mass-balance invariants for the Spain nitrogen balance pin.
#
# Both checks would have caught the stale `n_balance_ygpit_all_old` Excreta
# graft that inflated manure N by ~46% and zeroed 2022-2023 (see the commit
# that removed it). Run against the live pins:
#
#   Rscript --vanilla validation/n_balance_invariants.R
#
# Exits non-zero if an invariant fails, so it can be wired into CI once the
# Spain pins are reachable from the runner.

pkgload::load_all(quiet = TRUE)

failures <- character()

report <- function(ok, label, detail = "") {
  cli::cli_alert(paste0(if (ok) "PASS  " else "FAIL  ", label))
  if (nzchar(detail)) {
    cli::cli_alert_info(detail)
  }
  if (!ok) {
    failures <<- c(failures, label)
  }
}

n_balance <- whep_read_file("n_balance_ygpit_all")

# ---- 1. The pin's own input identity ---------------------------------------
# Excreta is one of seven N input streams; they must sum to N_input_std.
streams <- c(
  "Synthetic",
  "Excreta",
  "Solid",
  "Liquid",
  "Urban",
  "BNF",
  "Deposition"
)

recomputed <- n_balance |>
  dplyr::select(dplyr::all_of(streams)) |>
  rowSums(na.rm = TRUE)

rel_gap <- abs(recomputed - n_balance$N_input_std) /
  pmax(abs(n_balance$N_input_std), 1)

report(
  max(rel_gap, na.rm = TRUE) < 1e-6,
  "sum(N input streams) == N_input_std",
  sprintf("max relative gap %.3g", max(rel_gap, na.rm = TRUE))
)

# ---- 2. Manure cannot exceed livestock excretion ---------------------------
# Excreta + Solid + Liquid is manure N reaching soils. It must stay below
# total livestock excretion, since housing and storage lose N as NH3.
manure_by_year <- n_balance |>
  dplyr::summarise(
    manure = sum(Excreta + Solid + Liquid, na.rm = TRUE),
    .by = Year
  )

excretion_by_year <- whep_read_file("n_excretion_ygs") |>
  dplyr::summarise(excretion = sum(N_excr_MgN, na.rm = TRUE), .by = Year)

ratios <- manure_by_year |>
  dplyr::inner_join(excretion_by_year, by = "Year") |>
  dplyr::mutate(ratio = manure / excretion)

worst <- ratios |> dplyr::slice_max(ratio, n = 1)

report(
  all(ratios$ratio <= 1, na.rm = TRUE),
  "manure N <= livestock excretion N, every year",
  sprintf(
    "worst year %s at ratio %.2f (%d of %d years exceed 1)",
    worst$Year,
    worst$ratio,
    sum(ratios$ratio > 1, na.rm = TRUE),
    nrow(ratios)
  )
)

# ---- 3. No year silently loses Excreta ------------------------------------
# A join against a shorter table used to zero the most recent years.
zero_years <- n_balance |>
  dplyr::summarise(excreta = sum(Excreta, na.rm = TRUE), .by = Year) |>
  dplyr::filter(excreta == 0)

report(
  nrow(zero_years) == 0,
  "no year has zero total Excreta",
  if (nrow(zero_years) > 0) {
    paste("zero years:", paste(zero_years$Year, collapse = ", "))
  } else {
    "all years carry Excreta"
  }
)

if (length(failures) > 0) {
  cli::cli_abort("{length(failures)} invariant{?s} failed: {failures}")
}
cli::cli_alert_success("All n_balance invariants passed.")
