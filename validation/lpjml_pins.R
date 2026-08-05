# Guards the four WHEP input pins that carry LPJmL model output, so a pin swap
# cannot change published numbers silently.
#
# WHY THIS EXISTS
#
# Repointing these pins from LPJmL 5.9.7 to 6.1.1 raised natural-land carbon
# input ~31% -- a change that moves every downstream SOC number -- and
# `validate_all.R` ran clean straight through it. It had to: every variable it
# sweeps reads FAOSTAT, GAEZ, MapSPAM or USDA PSD, and none reads an LPJmL pin
# (see the note at the top of `variables.R`). So the largest input change of the
# migration was invisible to the harness. See #559.
#
# WHAT THIS IS, AND IS NOT
#
# It is NOT an external validation: there is no independent observational
# product for "LPJmL's grass NPP", so nothing here can say the model is right.
# It is a CONTRACT and REGRESSION check, in three tiers, cheapest first:
#
#   1. CONTRACT   required columns, row count and year span -- the cheapest
#                 signal that a pin is not the layer its consumers expect.
#   2. INVARIANT  physical impossibility, not expectation: a fractional
#                 saturation outside [0, 1], a negative carbon density, a
#                 monthly rainfall above any observed value. These hold for any
#                 model version, so they never need updating and they catch the
#                 corruptions that a row count cannot.
#   3. BASELINE   recorded magnitudes in `gt_lpjml_pins.json`, compared with a
#                 tolerance. This is the tier that makes a pin swap loud: it
#                 fails by design when the pins change, and the failure is the
#                 signal to look at what moved and then re-record.
#
# Tier 3 is deliberately a tripwire rather than a truth claim. Re-record it with
# `--record` when a pin change is intended and understood, and say in the commit
# why the numbers moved.
#
# Usage:
#   Rscript validation/lpjml_pins.R            # check against the baseline
#   Rscript validation/lpjml_pins.R --record   # rewrite the baseline
#
# Magnitudes use a fixed year window (see COMPARE_YEARS) so the check is fast
# and deterministic; row counts and schema cover the whole pin.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
})

BASELINE_PATH <- "validation/gt_lpjml_pins.json"

# One decade is enough to detect a magnitude shift, and reading the full span of
# the hydrology pin would mean 86.8e6 rows for no extra information.
COMPARE_YEARS <- 2000:2010

# Fractional tolerance on a recorded mean before it is flagged.
#
# Tiny on purpose. These are deterministic model outputs, not measurements:
# re-reading the same pin gives the same number, so the tolerance has to absorb
# only floating-point summation order, not sampling noise. A loose tolerance
# defeats the whole tier -- at 2% the real 5.9.7 -> 6.1.1 hydrology shift (1.7%)
# slipped through undetected while the larger carbon shifts were caught, which
# is exactly the silent pass this script exists to prevent.
MAGNITUDE_TOL <- 1e-5

# The four pins, their required columns, and the value column each is judged on.
#
# `bounds` are IMPOSSIBILITY limits, not expected ranges. swc_topsoil is a
# fractional saturation so it cannot leave [0, 1]. The carbon densities cannot
# be negative, and their ceilings sit far above any real value (tropical forest
# NPP is ~15-25 Mg C/ha/yr, so 100 is absurd rather than merely high). Monthly
# precipitation has been observed near 2500 mm, so 10000 is unreachable. A
# violation is corruption, not a model difference.
PIN_SPECS <- list(
  list(
    alias = "lpjml-grass-availability",
    columns = c("lon", "lat", "year", "grass_npp_gc_m2", "grass_avail_dm_t_ha"),
    value = "grass_avail_dm_t_ha",
    bounds = list(grass_avail_dm_t_ha = c(0, 100), grass_npp_gc_m2 = c(0, 5000))
  ),
  list(
    alias = "lpjml-grass-productivity",
    columns = c("lon", "lat", "year", "grass_npp"),
    value = "grass_npp",
    bounds = list(grass_npp = c(0, 5000))
  ),
  list(
    alias = "lpjml-grass-natural-net-c",
    columns = c("lon", "lat", "year", "land_use", "npp_c_mgc_ha_yr"),
    value = "npp_c_mgc_ha_yr",
    bounds = list(npp_c_mgc_ha_yr = c(0, 100))
  ),
  list(
    alias = "lpjml-soc-hydrology",
    columns = c(
      "lon",
      "lat",
      "year",
      "month",
      "swc_topsoil",
      "prec_mm",
      "irrig_mm"
    ),
    value = "swc_topsoil",
    bounds = list(
      swc_topsoil = c(0, 1),
      prec_mm = c(0, 10000),
      irrig_mm = c(0, 10000)
    )
  )
)

main <- function() {
  record <- "--record" %in% commandArgs(trailingOnly = TRUE)
  observed <- lapply(PIN_SPECS, measure_pin)
  names(observed) <- vapply(PIN_SPECS, function(s) s$alias, character(1))

  if (record) {
    write_baseline(observed)
    return(invisible(NULL))
  }
  report(observed, read_baseline())
}

# ---- Measuring -------------------------------------------------------------

# Everything this check needs from one pin.
#
# Read through whep_read_file() rather than off a path, because the point is to
# check what WHEP would actually load -- the registry version included -- not a
# file that happens to be on disk. That costs a full read of the 86.8e6-row
# hydrology pin, which is the price of using the supported interface instead of
# reaching into the pins cache layout.
measure_pin <- function(spec) {
  data <- whep::whep_read_file(spec$alias, type = "parquet") |>
    tibble::as_tibble()

  missing <- setdiff(spec$columns, names(data))
  if (length(missing) > 0L) {
    return(list(
      alias = spec$alias,
      fatal = sprintf("missing columns: %s", paste(missing, collapse = ", ")),
      n_rows = nrow(data)
    ))
  }

  info <- list(num_rows = nrow(data))
  window <- dplyr::filter(data, .data$year %in% COMPARE_YEARS)
  values <- window[[spec$value]]
  values <- values[is.finite(values)]

  list(
    alias = spec$alias,
    fatal = NULL,
    n_rows = info$num_rows,
    first_year = min(data$year),
    last_year = max(data$year),
    n_window = length(values),
    mean = mean(values),
    median = stats::median(values),
    violations = bound_violations(window, spec$bounds)
  )
}

# Rows outside the impossibility limits, per column. Counted rather than
# stopped at the first one: how many and in which column is what tells you
# whether it is one bad cell or a broken layer.
bound_violations <- function(data, bounds) {
  out <- lapply(names(bounds), function(column) {
    limit <- bounds[[column]]
    values <- data[[column]]
    finite <- values[is.finite(values)]
    tibble::tibble(
      column = column,
      below = sum(finite < limit[[1L]]),
      above = sum(finite > limit[[2L]]),
      n_missing = sum(!is.finite(values))
    )
  })
  dplyr::bind_rows(out)
}

# ---- Baseline --------------------------------------------------------------

read_baseline <- function() {
  if (!file.exists(BASELINE_PATH)) {
    cli::cli_abort(c(
      "No baseline at {.path {BASELINE_PATH}}.",
      i = "Record one with {.code Rscript validation/lpjml_pins.R --record}."
    ))
  }
  jsonlite::fromJSON(BASELINE_PATH, simplifyVector = FALSE)
}

write_baseline <- function(observed) {
  payload <- list(
    note = paste(
      "Recorded magnitudes for the LPJmL-derived pins. A mismatch is not",
      "automatically a fault: it is the intended alarm when the pins are",
      "repointed at a different LPJmL run. Re-record deliberately and say in",
      "the commit message why the numbers moved."
    ),
    compare_years = range(COMPARE_YEARS),
    pins = lapply(observed, function(o) {
      list(
        n_rows = o$n_rows,
        first_year = o$first_year,
        last_year = o$last_year,
        mean = o$mean,
        median = o$median
      )
    })
  )
  # digits = 12, because the default rounds to 4 decimals and that alone would
  # cap detectable change at ~1e-4 regardless of the tolerance above.
  jsonlite::write_json(
    payload,
    BASELINE_PATH,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 12
  )
  cli::cli_alert_success(
    "Recorded {length(observed)} pins to {.path {BASELINE_PATH}}."
  )
}

# ---- Reporting -------------------------------------------------------------

report <- function(observed, baseline) {
  cli::cli_h1("LPJmL-derived pin checks")
  rows <- lapply(observed, function(o) check_one(o, baseline$pins[[o$alias]]))
  table <- dplyr::bind_rows(rows)
  print(as.data.frame(table), row.names = FALSE)

  failed <- dplyr::filter(table, .data$verdict != "ok")
  cat("\n")
  if (nrow(failed) == 0L) {
    cli::cli_alert_success(
      "All {nrow(table)} pins match contract, invariants and baseline."
    )
    return(invisible(NULL))
  }
  cli::cli_alert_danger("{nrow(failed)} of {nrow(table)} pins deviate:")
  for (i in seq_len(nrow(failed))) {
    cli::cli_alert_warning("{failed$pin[[i]]}: {failed$detail[[i]]}")
  }
  cli::cli_alert_info(
    "If the pins were repointed on purpose, re-record with
     {.code Rscript validation/lpjml_pins.R --record}."
  )
  invisible(NULL)
}

check_one <- function(observed, expected) {
  base <- tibble::tibble(pin = observed$alias, n_rows = observed$n_rows)

  if (!is.null(observed$fatal)) {
    return(dplyr::mutate(base, verdict = "SCHEMA", detail = observed$fatal))
  }
  if (is.null(expected)) {
    return(dplyr::mutate(
      base,
      verdict = "NEW",
      detail = "not in the baseline; record it"
    ))
  }

  problems <- c(
    row_problem(observed, expected),
    span_problem(observed, expected),
    invariant_problem(observed),
    magnitude_problem(observed, expected)
  )
  dplyr::mutate(
    base,
    verdict = if (length(problems) == 0L) "ok" else "DEVIATES",
    detail = if (length(problems) == 0L) {
      sprintf("mean %.4g", observed$mean)
    } else {
      paste(problems, collapse = "; ")
    }
  )
}

row_problem <- function(observed, expected) {
  if (identical(as.numeric(observed$n_rows), as.numeric(expected$n_rows))) {
    return(character())
  }
  sprintf("rows %d vs recorded %d", observed$n_rows, expected$n_rows)
}

span_problem <- function(observed, expected) {
  if (
    identical(
      as.numeric(observed$first_year),
      as.numeric(expected$first_year)
    ) &&
      identical(as.numeric(observed$last_year), as.numeric(expected$last_year))
  ) {
    return(character())
  }
  sprintf(
    "span %d-%d vs recorded %d-%d",
    observed$first_year,
    observed$last_year,
    expected$first_year,
    expected$last_year
  )
}

# Impossibility violations are reported separately from baseline drift, because
# they mean something different: drift is a changed model, a violation is a
# broken layer.
invariant_problem <- function(observed) {
  bad <- dplyr::filter(observed$violations, .data$below > 0 | .data$above > 0)
  if (nrow(bad) == 0L) {
    return(character())
  }
  sprintf(
    "IMPOSSIBLE VALUES: %s",
    paste(
      sprintf("%s %d below/%d above", bad$column, bad$below, bad$above),
      collapse = ", "
    )
  )
}

magnitude_problem <- function(observed, expected) {
  ratio <- observed$mean / expected$mean
  if (abs(ratio - 1) <= MAGNITUDE_TOL) {
    return(character())
  }
  sprintf(
    "mean %.4g vs recorded %.4g (%.3fx)",
    observed$mean,
    expected$mean,
    ratio
  )
}

if (sys.nframe() == 0L) {
  main()
}
