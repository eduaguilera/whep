# Gridded nitrogen-balance plausibility check (internal + boundary archetype).
#
# The offline suite drives every step of
#
#   build_nitrogen_balance() -> calculate_n_surplus()
#                            -> build_n_boundary_exceedance()
#
# from injected fixtures, so it proves the arithmetic and says nothing about
# whether a real global build assembles or lands anywhere near the literature.
# That is issue #446's whole point, and it is why three separate blockers sat
# undetected behind a green suite. This script is the regression net the issue
# asks for: it takes a REAL run's output and checks the things a fixture
# cannot.
#
# It does not build anything itself -- the assembly is minutes and reads local
# rasters and pins. Produce the input once with the driver:
#
#   WHEP_NBD_OUT=validation/cache/findings/n_balance_2010.rds \
#     Rscript --no-init-file inst/scripts/run_nitrogen_balance.R 2010 grid
#
#   WHEP_NBD_RESULT=validation/cache/findings/n_balance_2010.rds \
#     Rscript --no-init-file validation/n_balance_gridded.R
#
# Exits non-zero if a check fails.
#
# ## What is checked, and why each one
#
# 1. The harvest-removal surplus is loss-free. `calculate_n_surplus()`'s
#    default method is net inputs minus harvested exports, all four terms
#    computed before the loss cascade runs. That matters operationally: the
#    cascade still has no global per-cell drivers (#359), so a driver must feed
#    it a placeholder `climate`. If the identity holds, the surplus is immune
#    to that placeholder; if it ever stops holding, every surplus quoted from a
#    placeholder run silently becomes a function of an invented value.
#
# 2. No cell-crop-year is duplicated. The balance key is the grain the
#    boundary comparison and the footprint both join on.
#
# 3. The global standard nitrogen input sits inside 50-300 Tg N/yr. Wide, on
#    purpose: it is a "the chain is wrong" tripwire, not a calibration.
#
# 4. WHEP's arable surplus against the Schulte-Uebbing et al. (2022) CRITICAL
#    arable surplus, integrated over the same archive's own source areas. This
#    is measured from the archive the package already reads (Zenodo record
#    6395016), not quoted from a paper, so it cannot go stale against a number
#    nobody can re-derive. It is a ratio, reported and recorded rather than
#    judged: the expected ratio is well above 1 (the world exceeds the
#    boundary), and how far above is exactly what #446 wants a human to look
#    at.
#
# Every measured value is also compared BIDIRECTIONALLY against
# `gt_n_balance_gridded.json`, which is committed. A number that falls is as
# loud as one that rises -- a term quietly dropping out of the assembly moves
# the total down, and that is the failure this chain has actually had.

suppressMessages(pkgload::load_all(quiet = TRUE))

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

result_path <- Sys.getenv("WHEP_NBD_RESULT")
if (!nzchar(result_path) || !file.exists(result_path)) {
  cli::cli_abort(c(
    "Set {.envvar WHEP_NBD_RESULT} to a driver result file.",
    i = "Produce one with {.envvar WHEP_NBD_OUT} and
         {.file inst/scripts/run_nitrogen_balance.R}; see this file's header."
  ))
}
result <- readRDS(result_path)
balance <- result$balance
surplus <- result$surplus
if (is.null(balance) || is.null(surplus)) {
  cli::cli_abort(
    "The driver result carries no balance or surplus; read its blocker table."
  )
}

# ---- 1. the surplus does not depend on the loss cascade --------------------
recomputed <- surplus$n_input_std_t -
  (surplus$prod_n_t + surplus$used_residue_n_t + surplus$grazed_weeds_n_t)
surplus_gap <- max(abs(recomputed - surplus$surplus_n_t), na.rm = TRUE)

report(
  surplus_gap < 1e-6,
  "harvest-removal surplus is computed from pre-loss terms only",
  sprintf("worst absolute gap %.3g t N", surplus_gap)
)

# ---- 2. the balance key is a key -------------------------------------------
key <- intersect(
  c("lon", "lat", "area_code", "item_cbs_code", "year"),
  names(balance)
)
duplicated_keys <- sum(duplicated(balance[key]))

report(
  duplicated_keys == 0L,
  "the balance key is unique",
  sprintf(
    "%d duplicated row(s) over %s",
    duplicated_keys,
    paste(key, collapse = "/")
  )
)

# ---- 3. the global input is physically plausible ---------------------------
input_tg <- sum(balance$n_input_std_t, na.rm = TRUE) / 1e6
surplus_tg <- sum(surplus$surplus_n_t, na.rm = TRUE) / 1e6

report(
  input_tg > 50 && input_tg < 300,
  "global standard N input is inside 50-300 Tg N/yr",
  sprintf("%.1f Tg N/yr (surplus %.1f Tg N/yr)", input_tg, surplus_tg)
)

# ---- 4. against the archive's own critical arable surplus ------------------
# critical_n_surplus is kg N/ha/yr on the archive's own deposited source area,
# so the global boundary total is the area-weighted integral of the layer --
# derived here, not copied from the paper.
critical <- read_critical_n(
  var = "critical_n_surplus",
  threshold = "mi",
  land_use = "ara"
)
critical_tg <- sum(
  critical$value * critical$source_area_ha,
  na.rm = TRUE
) /
  1e9
ratio <- surplus_tg / critical_tg

report(
  is.finite(ratio) && ratio > 0,
  "WHEP surplus compares to the critical arable surplus",
  sprintf(
    "critical %.1f Tg N/yr; WHEP/critical = %.2f",
    critical_tg,
    ratio
  )
)

# ---- the committed tripwire -------------------------------------------------
baseline_path <- file.path("validation", "gt_n_balance_gridded.json")
measured <- list(
  year = result$year,
  input_tg = input_tg,
  surplus_tg = surplus_tg,
  critical_tg = critical_tg,
  surplus_over_critical = ratio,
  balance_rows = nrow(balance)
)

if (file.exists(baseline_path)) {
  baseline <- jsonlite::fromJSON(baseline_path)
  moved <- purrr::keep(
    names(measured),
    \(nm) {
      recorded <- baseline$measured[[nm]]
      !is.null(recorded) &&
        !isTRUE(all.equal(
          as.numeric(recorded),
          as.numeric(measured[[nm]]),
          tolerance = 1e-6
        ))
    }
  )
  report(
    length(moved) == 0L,
    "the recorded state has not moved",
    if (length(moved) == 0L) {
      "every recorded figure reproduces"
    } else {
      paste(
        purrr::map_chr(
          moved,
          \(nm) {
            sprintf(
              "%s %s -> %s",
              nm,
              signif(as.numeric(baseline$measured[[nm]]), 6),
              signif(as.numeric(measured[[nm]]), 6)
            )
          }
        ),
        collapse = "; "
      )
    }
  )
} else {
  cli::cli_alert_info("No baseline at {.file {baseline_path}}; measured only.")
}

cli::cli_h2("Measured")
print(as.data.frame(measured))

if (length(failures) > 0) {
  cli::cli_abort("{length(failures)} check{?s} failed: {failures}")
}
cli::cli_alert_success("All gridded nitrogen-balance checks passed.")
