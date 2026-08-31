# Session-level cache for expensive build pipeline results.
#
# The full build pipeline (primary production -> CBS -> processing
# coefficients) takes several minutes.  This cache stores the
# intermediate tibbles so that repeated calls within the same R
# session (e.g. build_io_model() calling both build_supply_use()
# and get_wide_cbs()) reuse already-computed results.
#
# Use whep_clear_cache() to force a fresh rebuild.

.build_cache <- new.env(parent = emptyenv())

#' Clear the build pipeline cache
#'
#' @description
#' Removes cached results from [build_primary_production()],
#' [build_commodity_balances()], and [build_processing_coefs()]
#' so that the next call rebuilds from scratch.
#'
#' @return Invisible `NULL`.
#' @export
#'
#' @examples
#' whep_clear_cache()
whep_clear_cache <- function() {
  rm(list = ls(.build_cache), envir = .build_cache)
  cli::cli_alert_success("Build cache cleared.")
  invisible(NULL)
}

# Retrieve a cached value or compute and store it.
# key: character name for the cache slot
# expr: expression to evaluate if not cached (must be a call, not a symbol)
#
# The hit test is `exists()`, not `!is.null(.build_cache[[key]])`: assigning
# NULL into an environment removes the binding rather than storing it
# (`?assign`), so a NULL result was indistinguishable from an empty slot and
# was recomputed on every call (whep#172). `exists(inherits = FALSE)` and
# `get0()` see the binding regardless of what it holds.
.cache_get <- function(key, expr) {
  if (exists(key, envir = .build_cache, inherits = FALSE)) {
    cli::cli_alert_info("Using cached {key}.")
    return(get0(key, envir = .build_cache, inherits = FALSE))
  }
  result <- expr
  assign(key, result, envir = .build_cache)
  result
}

# --- Year-scoped cache keys --------------------------------------------------

# Cache slot name for a year-scoped build. A NULL window keeps the bare key, so
# callers asking for the full range share one slot and nothing that pre-dates
# year scoping changes behaviour. Without the window in the key, a request for
# 2000-2003 would be served to a caller that asked for everything (cf. #243).
.cache_key <- function(key, years) {
  if (is.null(years)) {
    return(key)
  }
  paste(
    key,
    min(years, na.rm = TRUE),
    max(years, na.rm = TRUE),
    sep = "__"
  )
}

# Collapse a requested year window to the contiguous range the builds work on.
.build_years <- function(years) {
  if (is.null(years)) {
    return(NULL)
  }
  seq.int(min(years, na.rm = TRUE), max(years, na.rm = TRUE))
}

# Widen a year window to the context the CBS build needs, for two reasons.
#
# The trade and stock imputation looks at neighbouring years, so a bare window
# leaves `stock_addition` and `import` visibly off. Measured at 2010 against the
# full-range build, the largest relative error across the wide-CBS quantity
# columns falls from 9.2e-03 with no margin to 5.2e-04 at +/-3, 3.8e-04 at +/-5
# and 2.1e-04 at +/-10. `import` bottoms out at 2.1e-04 (identical at +/-5 and
# +/-10), so that is the achievable floor and more margin only buys build time.
# +/-5 sits past the knee and keeps a scoped build several times cheaper than
# the full one.
#
# On top of that, FBS_New is the reference series and the 2010-2013 overlap is
# what splices the old series onto it (see .reestimate_domestic_supply), so a
# request reaching 2013 must build 2011 too or the splice silently changes.
#
# The pre-1961 back-cast needs no guard here: .read_production() already widens
# its own reads (see R/build_production.R) and trims afterwards.
#
# The margin is NOT a general remedy for the year axis, and whep#833 is where
# that stops being a detail. Two fills inside `.fix_cbs()` decide whether a
# processing output exists at all -- `.correct_processed()`'s `scaling_raw` and
# `.interpolate_destiny_shares()`'s `dest_share` -- and both carry a single
# anchor across the whole series. Measured at 2010, the anchors the full-range
# build uses for the 44 keys the two builds disagree on sit 7 to 49 years away:
# a margin of 10 still leaves 16 of them broken, 20 leaves 6, and only ~50
# closes them all. So the choice is between building the CBS over the full span
# (exact, and it costs the scoped build its saving: 254 s against 35 s for the
# `.fix_cbs()` chain measured at 2010) and bounding how far those fills may
# carry (cheap, but it moves full-range published values too). Both remedies
# are open in whep#833, and the recorded budget in
# validation/gt_year_scoping.json holds the measured divergence until one of
# them is taken.
.context_margin <- 5L

# The first year the series covers, matching the `start_year` default of
# build_primary_production() and build_commodity_balances(). The margin is
# clamped to it so widening never asks a build for years that precede the data.
.whep_first_year <- 1850L

.context_years <- function(years, margin = .context_margin) {
  if (is.null(years)) {
    return(years)
  }
  start_year <- max(min(years, na.rm = TRUE) - margin, .whep_first_year)
  end_year <- max(years, na.rm = TRUE) + margin
  if (end_year >= 2013L) {
    start_year <- min(start_year, 2011L)
  }
  seq.int(start_year, end_year)
}

# --- The shared build chain -------------------------------------------------

# All three build functions already accept start_year/end_year. These wrappers
# only translate a year vector into that pair, so a scoped request stops
# rebuilding 1850-2023 and discarding it.
.build_primary_prod_years <- function(years) {
  if (is.null(years)) {
    return(build_primary_production())
  }
  build_primary_production(
    start_year = min(years, na.rm = TRUE),
    end_year = max(years, na.rm = TRUE)
  )
}

.build_cbs_years <- function(primary_prod, years, context_years = years) {
  if (is.null(years)) {
    return(build_commodity_balances(primary_prod))
  }
  build_commodity_balances(
    primary_prod,
    start_year = min(context_years, na.rm = TRUE),
    end_year = max(context_years, na.rm = TRUE)
  ) |>
    .filter_years(years)
}

.build_proc_coefs_years <- function(cbs_built, years) {
  if (is.null(years)) {
    return(build_processing_coefs(cbs_built))
  }
  build_processing_coefs(
    cbs_built,
    start_year = min(years, na.rm = TRUE),
    end_year = max(years, na.rm = TRUE)
  )
}

# Primary production, cached under its context window. Callers that need the
# requested window only should .filter_years() the result themselves.
.cached_primary_prod <- function(years) {
  .cache_get(
    .cache_key("primary_prod", years),
    .build_primary_prod_years(years)
  )
}

# The long CBS built from primary production, cached under the requested
# window. This is the single copy of the wiring that get_wide_cbs(),
# get_processing_coefs() and build_io_model() all share.
.cached_cbs_built <- function(years) {
  primary_prod <- .cached_primary_prod(.context_years(years))
  .cache_get(.cache_key("cbs_built", years), {
    cli::cli_h1("Building commodity balance sheets")
    .build_cbs_years(primary_prod, years, .context_years(years))
  })
}
