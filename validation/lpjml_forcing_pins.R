# Guards the six climate-FORCING pins -- the NetCDF grids that feed INTO LPJmL.
#
# WHY THIS EXISTS, AND WHY IT IS SEPARATE FROM lpjml_pins.R
#
# `validation/lpjml_pins.R` guards the four pins carrying LPJmL model OUTPUT and
# deliberately excludes these, on the reasoning that forcing does not change
# with the model version. That reasoning is sound for its tier 3 (recorded
# magnitudes as a model-swap tripwire) and wrong for its tier 2: forcing can
# still be CORRUPT, and #824 is the proof. `lpjml-rsds-era5-2017-2023` ships
# 1,823,843 negative shortwave values. #536 fixed the script that builds it and
# nobody rebuilt the artifact, so every consumer still receives them.
#
# A negative downwelling flux is an impossibility, not an expectation, so it is
# exactly tier-2 material. Nothing in the repo could see it: the output-pin
# script excludes these pins by design, and `test_data_raw_freshness.R` gates
# `data/*.rda` against `data-raw/`, not a pin against its generating script.
#
# WHAT IT CHECKS, cheapest first, mirroring its sibling's tiers:
#
#   1. CONTRACT   the named variable is present and its grid has the recorded
#                 shape. A pin silently regridded is not the layer its
#                 consumers expect.
#   2. INVARIANT  impossibility bounds. A downwelling radiative flux and a wind
#                 speed cannot be negative, and the ceilings sit far above any
#                 physical value (see BOUNDS). These hold for every version of
#                 every forcing dataset, so they never need updating.
#   3. KNOWN STATE  `gt_lpjml_forcing_pins.json` records the negative count per
#                 pin. This is what makes the check usable TODAY, while #824 is
#                 open: the rsds-era5 violation is recorded rather than
#                 suppressed, so the script is green on the known-bad state and
#                 loud about anything new.
#
# The tier-3 comparison is BIDIRECTIONAL on purpose. A count that RISES is a new
# corruption. A count that FALLS means somebody rebuilt the pin -- which is the
# event #824 exists because nobody noticed -- and that must also stop the check
# and demand a re-record, so the fix gets written down instead of quietly
# changing what consumers receive.
#
# Usage:
#   Rscript validation/lpjml_forcing_pins.R            # check
#   Rscript validation/lpjml_forcing_pins.R --record   # rewrite the baseline
#
# Every value of every grid is read, one time step at a time: the whole point is
# a count, and a sampled count cannot distinguish 1.8e6 from 1.8e6 + 1. The
# ISIMIP pins are 1428 steps of 720x360, the ERA5 pins 84 of 1440x721.

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
  library(dplyr)
})

BASELINE_PATH <- "validation/gt_lpjml_forcing_pins.json" # nolint: object_name_linter.

# Impossibility limits, not expected ranges.
#
# The lower bound is 0 and it is INCLUSIVE. `lpjml-rsds-isimip-1901-2019` has a
# minimum of exactly 0 -- night -- so a positivity test would fail on a clean
# pin. #536's clamp must land on 0 for the same reason: do not nudge to epsilon.
#
# Ceilings are absurd rather than merely high, so a violation is corruption and
# never a climate argument. The solar constant is ~1361 W/m2 at the top of the
# atmosphere, which a monthly-mean surface flux cannot approach; downwelling
# longwave runs ~150-450 W/m2; the highest reliably measured surface wind gust
# is ~113 m/s and these are monthly means.
# nolint start: object_name_linter.
BOUNDS <- list(
  rsds = c(0, 1500),
  rlds = c(0, 1000),
  wind = c(0, 100)
)
# nolint end

# The six forcing pins and the variable each is judged on.
#
# The variable is named, never taken by index. A NetCDF file carries variables
# that are not the data -- these pins ship a dimensionless `crs`, and
# `names(nc$var)[[1]]` returns it for the wind pins, which is how a first pass
# at this measurement reported a spurious error. The same reason
# `.wb_filter_bands()` gives for reading band identity by name.
# nolint start: object_name_linter.
PIN_SPECS <- list(
  list(alias = "lpjml-rsds-era5-2017-2023", variable = "rsds"),
  list(alias = "lpjml-rlds-era5-2017-2023", variable = "rlds"),
  list(alias = "lpjml-wind-era5-2017-2023", variable = "wind"),
  list(alias = "lpjml-rsds-isimip-1901-2019", variable = "rsds"),
  list(alias = "lpjml-rlds-isimip-1901-2019", variable = "rlds"),
  list(alias = "lpjml-wind-isimip-1901-2019", variable = "wind")
)
# nolint end

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

# Read through whep_read_file() rather than off a path, so this measures what a
# consumer receives -- registry version included -- and not a file that happens
# to sit in the pins cache.
measure_pin <- function(spec) {
  handle <- whep::whep_read_file(spec$alias, type = "nc")
  nc <- if (inherits(handle, "ncdf4")) handle else ncdf4::nc_open(handle)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  if (!spec$variable %in% names(nc$var)) {
    return(list(
      alias = spec$alias,
      fatal = sprintf(
        "variable %s absent; file carries: %s",
        spec$variable,
        paste(names(nc$var), collapse = ", ")
      )
    ))
  }

  limit <- BOUNDS[[spec$variable]]
  dims <- vapply(nc$var[[spec$variable]]$dim, function(d) d$len, integer(1))
  names(dims) <- vapply(
    nc$var[[spec$variable]]$dim,
    function(d) d$name,
    character(1)
  )
  time_axis <- which(names(dims) == "time")
  if (length(time_axis) != 1L) {
    return(list(
      alias = spec$alias,
      fatal = sprintf(
        "expected exactly one time dimension, found dims: %s",
        paste(names(dims), collapse = ", ")
      )
    ))
  }

  totals <- sweep_time_steps(nc, spec$variable, dims, time_axis, limit)

  list(
    alias = spec$alias,
    fatal = NULL,
    variable = spec$variable,
    dims = paste(dims, collapse = "x"),
    n_steps = dims[[time_axis]],
    below = totals$below,
    above = totals$above,
    bad_steps = totals$bad_steps,
    n_missing = totals$n_missing,
    minimum = totals$minimum,
    maximum = totals$maximum
  )
}

# One time step at a time: the ERA5 grids are 1440x721 and holding 84 or 1428 of
# them at once is needless. Counts are accumulated, so the answer is exact.
#
# `n_missing` is counted rather than skipped silently. `!is.finite()` covers NA,
# NaN and the infinities together, and each would be a different corruption --
# but none of them is a bound violation, and folding them into `below` would let
# an all-NaN grid read as a clean one.
sweep_time_steps <- function(nc, variable, dims, time_axis, limit) {
  below <- 0
  above <- 0
  bad_steps <- 0L
  n_missing <- 0
  minimum <- Inf
  maximum <- -Inf

  for (i in seq_len(dims[[time_axis]])) {
    start <- rep(1L, length(dims))
    count <- as.integer(dims)
    start[[time_axis]] <- i
    count[[time_axis]] <- 1L
    values <- ncdf4::ncvar_get(nc, variable, start = start, count = count)

    finite <- values[is.finite(values)]
    n_missing <- n_missing + (length(values) - length(finite))
    if (length(finite) == 0L) {
      next
    }
    step_below <- sum(finite < limit[[1L]])
    step_above <- sum(finite > limit[[2L]])
    below <- below + step_below
    above <- above + step_above
    if (step_below > 0 || step_above > 0) {
      bad_steps <- bad_steps + 1L
    }
    minimum <- min(minimum, min(finite))
    maximum <- max(maximum, max(finite))
  }

  list(
    below = below,
    above = above,
    bad_steps = bad_steps,
    n_missing = n_missing,
    minimum = minimum,
    maximum = maximum
  )
}

# ---- Baseline --------------------------------------------------------------

read_baseline <- function() {
  if (!file.exists(BASELINE_PATH)) {
    cli::cli_abort(c(
      "No baseline at {.path {BASELINE_PATH}}.",
      i = "Record one with
         {.code Rscript validation/lpjml_forcing_pins.R --record}."
    ))
  }
  jsonlite::fromJSON(BASELINE_PATH, simplifyVector = FALSE)
}

write_baseline <- function(observed) {
  payload <- list(
    note = paste(
      "Recorded state of the climate-forcing pins. `below` is the count of",
      "values under the impossibility floor: nonzero entries are KNOWN,",
      "FILED defects, not accepted behaviour. The comparison is",
      "bidirectional -- a count that falls means the pin was rebuilt and must",
      "be re-recorded, with the issue closed in the same commit."
    ),
    known_defects = list(
      "lpjml-rsds-era5-2017-2023" = "#824 (builder fixed in #536, pin not rebuilt)"
    ),
    pins = lapply(observed, function(o) {
      list(
        variable = o$variable,
        dims = o$dims,
        below = o$below,
        above = o$above,
        bad_steps = o$bad_steps,
        n_missing = o$n_missing,
        minimum = o$minimum,
        maximum = o$maximum
      )
    })
  )
  jsonlite::write_json(
    payload,
    BASELINE_PATH,
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = 12
  )
  cli::cli_alert_success(
    "Recorded {length(observed)} forcing pins to {.path {BASELINE_PATH}}."
  )
}

# ---- Reporting -------------------------------------------------------------

report <- function(observed, baseline) {
  cli::cli_h1("Climate-forcing pin checks")
  rows <- lapply(observed, function(o) check_one(o, baseline$pins[[o$alias]]))
  table <- dplyr::bind_rows(rows)
  print(as.data.frame(table), row.names = FALSE)

  failed <- dplyr::filter(table, !.data$verdict %in% c("ok", "KNOWN"))
  cat("\n")
  if (nrow(failed) == 0L) {
    known <- dplyr::filter(table, .data$verdict == "KNOWN")
    cli::cli_alert_success(
      "All {nrow(table)} forcing pins match contract and recorded state."
    )
    for (i in seq_len(nrow(known))) {
      cli::cli_alert_info("{known$pin[[i]]}: {known$detail[[i]]}")
    }
    return(invisible(NULL))
  }
  cli::cli_alert_danger("{nrow(failed)} of {nrow(table)} forcing pins deviate:")
  for (i in seq_len(nrow(failed))) {
    cli::cli_alert_warning("{failed$pin[[i]]}: {failed$detail[[i]]}")
  }
  cli::cli_alert_info(
    "A count that FELL means the pin was rebuilt: re-record with
     {.code Rscript validation/lpjml_forcing_pins.R --record} and close the
     issue in the same commit."
  )
  invisible(NULL)
}

check_one <- function(observed, expected) {
  base <- tibble::tibble(pin = observed$alias)

  if (!is.null(observed$fatal)) {
    return(dplyr::mutate(base, verdict = "SCHEMA", detail = observed$fatal))
  }
  base <- dplyr::mutate(
    base,
    dims = observed$dims,
    below = observed$below,
    above = observed$above
  )
  if (is.null(expected)) {
    return(dplyr::mutate(
      base,
      verdict = "NEW",
      detail = "not in the baseline; record it"
    ))
  }

  problems <- c(
    dims_problem(observed, expected),
    count_problem(observed, expected, "below"),
    count_problem(observed, expected, "above"),
    count_problem(observed, expected, "n_missing")
  )
  if (length(problems) > 0L) {
    return(dplyr::mutate(
      base,
      verdict = "DEVIATES",
      detail = paste(problems, collapse = "; ")
    ))
  }
  if (observed$below > 0 || observed$above > 0) {
    return(dplyr::mutate(
      base,
      verdict = "KNOWN",
      detail = sprintf(
        "%d impossible values, unchanged (min %.6g, max %.6g)",
        observed$below + observed$above,
        observed$minimum,
        observed$maximum
      )
    ))
  }
  dplyr::mutate(
    base,
    verdict = "ok",
    detail = sprintf("min %.6g, max %.6g", observed$minimum, observed$maximum)
  )
}

dims_problem <- function(observed, expected) {
  if (identical(observed$dims, expected$dims)) {
    return(character())
  }
  sprintf("grid %s vs recorded %s", observed$dims, expected$dims)
}

# Bidirectional by design: see the header. A drop is as much a signal as a rise,
# so this compares for equality rather than for an exceedance.
count_problem <- function(observed, expected, field) {
  seen <- as.numeric(observed[[field]])
  want <- as.numeric(expected[[field]])
  if (identical(seen, want)) {
    return(character())
  }
  sprintf(
    "%s %s vs recorded %s (%s)",
    field,
    format(seen, big.mark = ","),
    format(want, big.mark = ","),
    if (seen > want) "ROSE: new corruption" else "FELL: pin rebuilt?"
  )
}

if (sys.nframe() == 0L) {
  main()
}
