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
#
# `method` qualifies the slot the same way for a build method that changes the
# result. A NULL method adds nothing, so the slots a default build uses are the
# ones it has always used; any other method gets its own slot, because serving
# one method's CBS to a caller that asked for the other is silent and
# unrecoverable (whep#762).
.cache_key <- function(key, years, method = NULL) {
  key <- paste(c(key, method), collapse = "__")
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

# The cache-slot qualifier for a CBS build method. `"none"` is the default and
# returns NULL, so a default build keeps its existing slot names.
.cbs_cache_method <- function(trade_recovery) {
  if (identical(trade_recovery, "none")) {
    return(NULL)
  }
  trade_recovery
}

# Collapse a requested year window to the contiguous range the builds work on.
.build_years <- function(years) {
  if (is.null(years)) {
    return(NULL)
  }
  seq.int(min(years, na.rm = TRUE), max(years, na.rm = TRUE))
}

# The first year the series covers, matching the `start_year` default of
# build_primary_production() and build_commodity_balances().
.whep_first_year <- 1850L

# --- Why a scoped CBS is cut from the full-range one (whep#833) --------------
#
# A year window asks for a subset, so the contract is an identity:
#
#     get_wide_cbs(years = Y) == get_wide_cbs() |> filter(year %in% Y)
#
# The CBS chain cannot be built over a window and keep it. Two fills inside
# `.fix_cbs()` carry a single observation across the whole year axis, however
# far away it is, and both decide whether a processing output EXISTS:
#
# * `.correct_processed()`'s `scaling_raw`: with no anchor in the frame the
#   scaling collapses to 0 and the output row is deleted. Italy's Ricebran Oil
#   at 2010 rests on its only observation, 1961.
# * `.interpolate_destiny_shares()`'s `dest_share`: with no anchor in the frame
#   the key falls back to the world-average split, takes a `processing` share
#   it never reported, and `.cbs_second_processed_round()` manufactures the
#   oil and cake that crush implies (Malta coconuts, anchored in 1990-1994).
#
# Measured at 2010 on the old wiring (a +/-5-year margin around the window),
# that was 14 keys lost and 30 invented, with anchors 7 to 49 years away; a
# margin of 20 still left 6 broken and only ~50 closed them all. The trade and
# stock imputation reads neighbouring years too, which is what the margin was
# for (9.2e-03 relative error with none, 3.8e-04 at +/-5).
#
# No finite margin is safe, because how far a fill reaches is set by the data.
# So a scoped request builds the full-range CBS once, caches it under the
# full-range slot, and filters it: exact by construction, the same answer
# whep#834 reached for the production yield chain. The price is that a scoped
# CBS costs what a full one does (the `.fix_cbs()` chain was measured at 35 s
# scoped against 254 s full-range at 2010), paid once per session, after which
# every window is a filter of the cached build.
#
# The alternative is to bound how far those two fills may carry. It keeps
# scoped builds cheap but moves full-range published values, so it is a
# science decision left open in whep#833.

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

# The long CBS built from primary production, cached under the trade-recovery
# method. This is the single copy of the wiring that get_wide_cbs(),
# get_processing_coefs() and build_io_model() all share, so the method has to
# travel with it: a build under one method served to a caller that asked for
# the other would be invisible downstream.
#
# A scoped request is cut from the full-range build, never built on its own
# window: see "Why a scoped CBS is cut from the full-range one" above.
.cached_cbs_built <- function(years, trade_recovery = "none") {
  key <- .cache_key("cbs_built", NULL, .cbs_cache_method(trade_recovery))
  full <- .cache_get(key, {
    primary_prod <- .cached_cbs_primary_prod()
    cli::cli_h1("Building commodity balance sheets")
    build_commodity_balances(primary_prod, trade_recovery = trade_recovery)
  })
  if (is.null(years)) {
    return(full)
  }
  dplyr::filter(full, .data$year %in% years)
}

# The primary production the CBS chain is built on: the full range, for the
# same reason the CBS is (whep#833). Callers that pair a scoped CBS with its
# production take it from here too, so a session builds production once.
.cached_cbs_primary_prod <- function() {
  .cached_primary_prod(NULL)
}
