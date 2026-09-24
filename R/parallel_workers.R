#' Resolve how many parallel workers a forked call may use.
#'
#' @description
#' Every forked-parallel call site in the package has to satisfy the same
#' two constraints, so they are resolved here once instead of being
#' re-derived per site.
#'
#' `parallel::mclapply()` forks, which is unavailable on Windows, so it
#' degenerates to a serial run there and the worker count must be one.
#'
#' `parallel:::.check_ncores()` aborts as soon as more than two processes
#' are requested while the environment variable `_R_CHECK_LIMIT_CORES_`
#' holds any value other than `"false"`. `R CMD check --as-cran` sets it,
#' and so do CRAN's own machines, whose policy caps a package at two
#' cores. A call site that ignores it turns an ordinary check run into a
#' hard error on any host with more than four cores.
#'
#' @param requested Number of workers asked for, or `NULL` to derive a
#'   default from `parallel::detectCores()`.
#' @return A single positive integer: `1` on Windows, at most `2` while
#'   the check core limit is in force, and otherwise `requested` (or half
#'   the detected cores when `requested` is `NULL`).
#' @noRd
.parallel_workers <- function(requested = NULL) {
  if (.is_windows()) {
    return(1L)
  }
  n <- if (is.null(requested)) {
    parallel::detectCores() %/% 2L
  } else {
    suppressWarnings(as.integer(requested))
  }
  # detectCores() is documented to return NA when it cannot tell.
  if (length(n) != 1L || is.na(n) || n < 1L) {
    n <- 1L
  }
  if (.core_limit_in_force()) {
    n <- min(n, 2L)
  }
  as.integer(n)
}

# TRUE while a core limit applies. Same test as parallel's own private
# guard: the variable counts as set unless it is empty or reads "false",
# case-insensitively.
.core_limit_in_force <- function() {
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  nzchar(chk) && chk != "false"
}

# Wrapped so the no-fork branch stays reachable from a test on Linux.
.is_windows <- function() {
  .Platform$OS.type == "windows"
}
