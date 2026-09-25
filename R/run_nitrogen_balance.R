# The nitrogen-balance driver's blocking policy
# (inst/scripts/run_nitrogen_balance.R), factored out of the script so it is
# unit-testable: `inst/scripts/` is `.Rbuildignore`d and no test can source
# it directly (the same reason .n_drop_uncelled_fertilizer() lives in
# R/n_balance_spatialize.R rather than in the driver itself).

# Stages the driver tolerates failing: build_n_inputs() answers their
# absence with an empty term rather than an error, so the balance still
# builds, just without that term. carbon_balance is the only one: it is the
# driver's heaviest stage (minutes of runtime, a 537 MB LPJmL pin, and every
# gridded LUH2/HWSD/climate reader build_carbon_balance() touches), and its
# own som_mineralization stream already treats "nothing to report" as a real
# observation rather than only a symptom -- .n_inputs_som() keeps
# son_change_kgn_ha > 0 rows only (R/n_balance_inputs.R), so a cell-year with
# no net mineralization contributes zero whether carbon_balance ran or not.
#
# livestock_intake used to be tolerated for the same shape of reason
# (whep#1025 made get_wide_cbs() unreachable), but that was fixed by
# fdcdf7e2 and the chain has since run to completion (whep#1289): a failure
# is now a real defect, not an expected gap, and must block the balance
# rather than silently zero-filling the manure and intake terms.
.nbd_tolerated_stages <- function() {
  c("carbon_balance")
}

# The report rows that stop the run: a FAILED stage that is not tolerated.
# The single source of truth behind the driver's "5b. Terms this run does
# NOT carry" / blocker split, so a change to the policy is made once and is
# provable by a test rather than only by reading the script.
.nbd_blocking_failures <- function(
  report,
  tolerated = .nbd_tolerated_stages()
) {
  dplyr::filter(
    report,
    .data$status == "FAIL",
    !.data$input %in% tolerated
  )
}

# The report rows the balance runs WITHOUT: a tolerated stage that did not
# succeed, or any stage skipped on request (WHEP_NBD_SKIP_HEAVY). A skip is not
# a failure, so it does not block, but it is not an absence to hide either:
# a skipped livestock_intake empties the manure and intake terms exactly as a
# failed one would, and the "5b" section must say so.
.nbd_carried_gaps <- function(report, tolerated = .nbd_tolerated_stages()) {
  dplyr::filter(
    report,
    .data$status != "ok",
    .data$input %in% tolerated | .data$status == "skip"
  )
}

# The balance terms that are zero when a given stage is missing, for the
# "5b" message. Stages not listed contribute no term of their own.
.nbd_gap_terms <- function(inputs) {
  terms <- c(
    carbon_balance = "som_mineralization",
    livestock_intake = "manure and livestock intake"
  )
  unname(terms[intersect(inputs, names(terms))])
}
