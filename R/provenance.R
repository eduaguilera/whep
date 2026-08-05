#' Record provenance for a reproducible result.
#'
#' @description
#' Capture the information needed to regenerate a result: the
#' package version (the code), the R version, the pinned versions
#' of the input datasets used, and a timestamp. Attach the record
#' to an output with [attach_provenance()] so any number can be
#' traced back to the exact inputs and code that produced it.
#'
#' Unknown aliases are an error rather than a silent omission, so a
#' provenance record never quietly drops an input it could not
#' resolve.
#'
#' @param aliases Optional character vector of input aliases to
#'   record. When `NULL` (default), every registered input is
#'   recorded.
#' @param inputs Tibble of registered inputs with `alias` and
#'   `version` columns. Defaults to [whep_inputs].
#' @param recorded_at Timestamp for the record. Defaults to the
#'   current time; pass a fixed value for reproducible output.
#'
#' @return A tibble with one row per recorded input:
#'   - `recorded_at`: When the record was made.
#'   - `whep_version`: Installed package version (the code).
#'   - `r_version`: R version.
#'   - `input_alias`: Input dataset alias.
#'   - `input_version`: Pinned version of that input.
#'
#' @export
#'
#' @examples
#' prov <- record_provenance(
#'   aliases = "bilateral_trade",
#'   recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
#' )
#' prov
record_provenance <- function(
  aliases = NULL,
  inputs = whep::whep_inputs,
  recorded_at = Sys.time()
) {
  .require_cols(inputs, c("alias", "version"), "inputs")
  used <- .select_provenance_inputs(inputs, aliases)
  n <- nrow(used)
  tibble::tibble(
    recorded_at = rep(recorded_at, n),
    whep_version = rep(as.character(utils::packageVersion("whep")), n),
    r_version = rep(as.character(getRversion()), n),
    input_alias = used$alias,
    input_version = used$version
  )
}

#' Attach a provenance record to a result.
#'
#' @description
#' Store a provenance record (from [record_provenance()]) on an
#' object as an attribute, so the result travels together with its
#' lineage. Retrieve it later with [get_provenance()].
#'
#' @param x Any R object, typically a result tibble.
#' @param provenance Provenance tibble from [record_provenance()].
#'
#' @return `x`, unchanged except for an added `whep_provenance`
#'   attribute.
#'
#' @export
#'
#' @examples
#' prov <- record_provenance(
#'   aliases = "bilateral_trade",
#'   recorded_at = as.POSIXct("2026-01-01", tz = "UTC")
#' )
#' result <- attach_provenance(tibble::tibble(value = 1), prov)
#' get_provenance(result)
attach_provenance <- function(x, provenance) {
  attr(x, "whep_provenance") <- provenance
  x
}

#' Retrieve a result's provenance record.
#'
#' @description
#' Return the provenance record attached by [attach_provenance()],
#' or `NULL` when none is present.
#'
#' @param x An object that may carry a `whep_provenance` attribute.
#'
#' @return The provenance tibble, or `NULL`.
#'
#' @export
#'
#' @examples
#' get_provenance(tibble::tibble(value = 1))
get_provenance <- function(x) {
  attr(x, "whep_provenance")
}

# --- Helpers ---

.select_provenance_inputs <- function(inputs, aliases) {
  if (is.null(aliases)) {
    return(inputs)
  }
  missing <- setdiff(aliases, inputs$alias)
  if (length(missing) > 0) {
    cli::cli_abort("Unknown input alias{?es}: {.val {missing}}.")
  }
  inputs[match(aliases, inputs$alias), , drop = FALSE]
}
