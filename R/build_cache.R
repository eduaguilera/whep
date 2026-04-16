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
.cache_get <- function(key, expr) {
  if (!is.null(.build_cache[[key]])) {
    cli::cli_alert_info("Using cached {key}.")
    return(.build_cache[[key]])
  }
  result <- expr
  .build_cache[[key]] <- result
  result
}
