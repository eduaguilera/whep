# Checks a finished LPJmL run's GLOBAL fluxes for equilibration and against
# observational benchmarks, and diffs two runs.
#
# LPJmL writes one row per simulated year to globalflux_spinup.csv, covering
# the spinup AND the transient period, which makes this the cheapest honest
# view of a run: no gridded output has to be read at all.
#
# Three things it answers:
#
# 1. DID THE SPINUP EQUILIBRATE? A run whose pools are still drifting is not
#    a steady state, and every flux read off it inherits that drift. Checked
#    per pool (carbon, nitrogen) rather than globally, because they equilibrate
#    on very different timescales -- carbon settles within a couple of hundred
#    years while nitrogen can still be draining well past that, so a single
#    verdict would hide the one that matters.
#
# 2. ARE THE TRANSIENT FLUXES PLAUSIBLE? Compared against published
#    observational estimates (see BENCHMARKS). Note that global GPP is
#    genuinely contested: satellite-optical products give 120-140 PgC/yr
#    while carbonyl-sulfide and 18O constraints give 150-175, and the gap is
#    concentrated in tropical rainforest. A model can therefore only be said
#    to agree with ONE of those families, so this reports which.
#
# 3. WHAT CHANGED BETWEEN TWO RUNS? Side by side, absolute and relative.
#
# Usage:
#   Rscript validation/lpjml_globalflux.R <run_dir> [baseline_dir] [from,to]
#
# Example:
#   Rscript validation/lpjml_globalflux.R \
#     /path/to/611/output/scenario_1 /path/to/597/output/scenario_1 2000,2010

suppressMessages(library(dplyr))

# Published observational estimates. Every entry carries its source; none of
# these numbers may be adjusted to suit a run.
#
# GPP is reported as three separate families rather than one range because
# they disagree by ~30%: the carbonyl-sulfide constraint of 157 +/- 8.5
# PgC/yr (Lai et al. 2024, doi:10.1038/s41586-024-08050-3) is consistent
# with the 18O constraint of 150-175 but well above satellite-optical
# products at 120-140, a gap those authors attribute to underestimated
# tropical productivity.
BENCHMARKS <- list(
  gpp_cos = list(
    label = "GPP, carbonyl sulfide (Lai 2024)",
    low = 148.5,
    high = 165.5
  ),
  gpp_o18 = list(label = "GPP, 18O constraint", low = 150, high = 175),
  gpp_satellite = list(
    label = "GPP, satellite-optical products",
    low = 120,
    high = 140
  ),
  firec = list(label = "Fire C emissions, GFED4s", low = 2.1, high = 2.2)
)

# Pools whose drift decides whether the spinup equilibrated, and the fraction
# of the pool per year below which drift is negligible.
#
# 0.1%/yr is the threshold, not something tighter: spinup recycles a fixed
# climate period, so the fast pools genuinely oscillate from year to year and
# a tighter tolerance just labels every pool "drifting" regardless of the run,
# which is a verdict that carries no information.
EQUILIBRIUM_POOLS <- c("SoilC", "LitC", "VegC", "SoilN", "VegN")
EQUILIBRIUM_DRIFT_TOL <- 1e-3
EQUILIBRIUM_WINDOW <- 20L

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L) {
    cat("usage: lpjml_globalflux.R <run_dir> [baseline_dir] [from,to]\n")
    return(invisible(NULL))
  }
  run_dir <- args[[1L]]
  baseline_dir <- if (length(args) >= 2L && nzchar(args[[2L]])) {
    args[[2L]]
  } else {
    NULL
  }
  window <- if (length(args) >= 3L) {
    as.integer(strsplit(args[[3L]], ",")[[1L]])
  } else {
    c(2000L, 2010L)
  }

  run <- read_globalflux(run_dir)
  report_equilibration(run, label = "run")
  report_benchmarks(run, window)

  if (!is.null(baseline_dir)) {
    base <- read_globalflux(baseline_dir)
    report_equilibration(base, label = "baseline")
    report_delta(run, base, window)
  }
  invisible(NULL)
}

# ---- Reading ----------------------------------------------------------------

# The first data line of the CSV is a units row, not a year, so it is peeled
# off and kept: units are read from the file rather than hardcoded, because
# they differ between the carbon, water and nitrogen blocks and a wrong label
# is worse than none.
read_globalflux <- function(run_dir) {
  path <- file.path(run_dir, "globalflux_spinup.csv")
  if (!file.exists(path)) {
    stop("no globalflux_spinup.csv in ", run_dir, call. = FALSE)
  }
  raw <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  units <- vapply(raw[1L, ], as.character, character(1L))
  data <- raw[-1L, , drop = FALSE]
  for (column in names(data)) {
    data[[column]] <- suppressWarnings(as.numeric(data[[column]]))
  }
  # A run still in progress has a partial final line.
  data <- data[!is.na(data$Year), , drop = FALSE]
  list(data = tibble::as_tibble(data), units = units, dir = run_dir)
}

# Total evapotranspiration. Kept as a helper because the three components are
# reported separately and summing the wrong subset is an easy silent error.
total_et <- function(data) {
  data$transp + data$evap + data$interc
}

# ---- Reports ----------------------------------------------------------------

# Per-pool drift over the last 20 spinup years, as a fraction of the pool per
# year. Reported per pool because carbon and nitrogen equilibrate on different
# timescales and a single number would mask a still-draining nitrogen pool.
report_equilibration <- function(flux, label) {
  header(paste0("SPINUP EQUILIBRATION (", label, ")"))
  spinup <- dplyr::filter(flux$data, Year < 1901L)
  if (nrow(spinup) < 21L) {
    cat("  fewer than 21 spinup years present; skipping\n")
    return(invisible(NULL))
  }
  # Two adjacent windows rather than a slope through one: spinup recycles a
  # fixed climate, so a within-window slope partly measures where in that
  # cycle the window happens to fall. Comparing consecutive 20-year MEANS
  # averages that cycle out and leaves the secular drift.
  window <- EQUILIBRIUM_WINDOW
  if (nrow(spinup) < 2L * window + 1L) {
    window <- nrow(spinup) %/% 2L
  }
  recent <- utils::tail(spinup, window)
  earlier <- utils::tail(utils::head(spinup, -window), window)
  cat(sprintf(
    "  %d spinup years (%d..%d); drift = change between the last two %d-year means\n\n",
    nrow(spinup),
    min(spinup$Year),
    max(spinup$Year),
    window
  ))

  rows <- lapply(EQUILIBRIUM_POOLS, function(pool) {
    if (!pool %in% names(recent)) {
      return(NULL)
    }
    now <- mean(recent[[pool]])
    before <- mean(earlier[[pool]])
    per_year <- (now - before) / window
    frac <- abs(per_year) / now
    tibble::tibble(
      pool = pool,
      mean = now,
      drift_per_yr = per_year,
      frac_per_yr = frac,
      verdict = if (frac < EQUILIBRIUM_DRIFT_TOL) "equilibrated" else "DRIFTING"
    )
  })
  print(as.data.frame(bind_rows(rows)), digits = 3, row.names = FALSE)

  # At nitrogen steady state, losses balance inputs. This ratio is the
  # standard diagnostic and is unitless, so it is comparable across runs.
  if (all(c("nlosses", "ninflux") %in% names(recent))) {
    ratio <- sum(recent$nlosses) / sum(recent$ninflux)
    cat(sprintf(
      "\n  nlosses/ninflux over the last %d spinup years: %.3f (1.0 = steady)\n",
      window,
      ratio
    ))
  }
}

report_benchmarks <- function(flux, window) {
  header(sprintf(
    "TRANSIENT FLUXES %d-%d vs OBSERVATIONS",
    window[[1L]],
    window[[2L]]
  ))
  data <- dplyr::filter(flux$data, Year >= window[[1L]], Year <= window[[2L]])
  if (nrow(data) == 0L) {
    cat("  no years in that window (run may still be in spinup)\n")
    return(invisible(NULL))
  }
  gpp <- mean(data$GPP)
  cat(sprintf(
    "  GPP      %7.1f %s\n  NPP      %7.1f  (CUE %.2f)\n  firec    %7.2f %s\n  harvestc %7.2f %s\n  ET       %7.1f %s\n\n",
    gpp,
    flux$units[["GPP"]],
    mean(data$NPP),
    mean(data$NPP) / gpp,
    mean(data$firec),
    flux$units[["firec"]],
    mean(data$harvestc),
    flux$units[["harvestc"]],
    mean(total_et(data)),
    flux$units[["transp"]]
  ))
  for (key in names(BENCHMARKS)) {
    spec <- BENCHMARKS[[key]]
    value <- if (startsWith(key, "gpp")) gpp else mean(data$firec)
    inside <- value >= spec$low && value <= spec$high
    cat(sprintf(
      "  %-34s %6.1f - %6.1f   %s\n",
      spec$label,
      spec$low,
      spec$high,
      if (inside) "CONSISTENT" else "outside"
    ))
  }
}

report_delta <- function(run, base, window) {
  header(sprintf("RUN vs BASELINE, %d-%d", window[[1L]], window[[2L]]))
  pick <- function(flux) {
    d <- dplyr::filter(flux$data, Year >= window[[1L]], Year <= window[[2L]])
    c(
      GPP = mean(d$GPP),
      NPP = mean(d$NPP),
      RH = mean(d$RH),
      NEP = mean(d$NEP),
      firec = mean(d$firec),
      harvestc = mean(d$harvestc),
      ET = mean(total_et(d)),
      discharge = mean(d$discharge),
      SoilC = mean(d$SoilC),
      VegC = mean(d$VegC)
    )
  }
  new <- pick(run)
  old <- pick(base)
  out <- tibble::tibble(
    variable = names(new),
    baseline = as.numeric(old),
    run = as.numeric(new),
    ratio = as.numeric(new) / as.numeric(old)
  )
  print(as.data.frame(out), digits = 4, row.names = FALSE)
}

header <- function(text) {
  cat("\n", strrep("=", 74), "\n", text, "\n", strrep("=", 74), "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
