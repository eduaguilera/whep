#' Describe the scope of a footprint result.
#'
#' @description
#' Build a machine-readable scope record for a footprint: what is
#' measured, in which units, by which method, and under which
#' system boundary, allocation rule, data vintage and known
#' limitations. This is the ISO 14044 goal-and-scope made into an
#' attachable data object rather than prose, so a footprint result
#' always travels with the assumptions behind it.
#'
#' Attach it to a result with [attach_scope()] and read it back
#' with [get_scope()].
#'
#' @param stressor What is measured, e.g. `"cropland"`.
#' @param units Units of the footprint value, e.g. `"ha"`.
#' @param method Estimation method used, e.g. `"FABIO-MRIO"`. This
#'   mirrors the multi-method `method_<quantity>` columns recorded
#'   elsewhere in the package.
#' @param details Optional named list overriding any of:
#'   `boundary` (system boundary), `allocation` (allocation rule),
#'   `vintage` (data years) and `limitations` (free text).
#'
#' @return A one-row tibble with columns `stressor`, `units`,
#'   `method`, `boundary`, `allocation`, `vintage` and
#'   `limitations`.
#'
#' @export
#'
#' @examples
#' footprint_scope(
#'   stressor = "cropland",
#'   units = "ha",
#'   method = "FABIO-MRIO",
#'   details = list(vintage = "1850-2023", allocation = "mass")
#' )
footprint_scope <- function(
  stressor,
  units,
  method,
  details = list()
) {
  .validate_string1(stressor, "stressor")
  .validate_string1(units, "units")
  .validate_string1(method, "method")
  defaults <- list(
    boundary = "cradle-to-farm-gate",
    allocation = "mass",
    vintage = NA_character_,
    limitations = NA_character_
  )
  details <- .merge_scope_details(defaults, details)
  tibble::tibble(
    stressor = stressor,
    units = units,
    method = method,
    boundary = details$boundary,
    allocation = details$allocation,
    vintage = as.character(details$vintage),
    limitations = as.character(details$limitations)
  )
}

#' Attach a scope record to a result.
#'
#' @description
#' Store a scope record (from [footprint_scope()]) on an object as
#' an attribute, so the result carries its goal-and-scope with it.
#'
#' @param x Any R object, typically a footprint tibble.
#' @param scope Scope tibble from [footprint_scope()].
#'
#' @return `x`, unchanged except for an added `whep_scope`
#'   attribute.
#'
#' @export
#'
#' @examples
#' scope <- footprint_scope("cropland", "ha", "FABIO-MRIO")
#' result <- attach_scope(tibble::tibble(value = 1), scope)
#' get_scope(result)
attach_scope <- function(x, scope) {
  attr(x, "whep_scope") <- scope
  x
}

#' Retrieve a result's scope record.
#'
#' @description
#' Return the scope record attached by [attach_scope()], or `NULL`
#' when none is present.
#'
#' @param x An object that may carry a `whep_scope` attribute.
#'
#' @return The scope tibble, or `NULL`.
#'
#' @export
#'
#' @examples
#' get_scope(tibble::tibble(value = 1))
get_scope <- function(x) {
  attr(x, "whep_scope")
}

# --- Helpers ---

.merge_scope_details <- function(defaults, details) {
  if (!is.list(details)) {
    cli::cli_abort("{.arg details} must be a named list.")
  }
  unknown <- setdiff(names(details), names(defaults))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown {.arg details} field{?s}: {.field {unknown}}.")
  }
  utils::modifyList(defaults, details)
}

.validate_string1 <- function(value, arg) {
  if (
    !is.character(value) ||
      length(value) != 1 ||
      is.na(value) ||
      !nzchar(value)
  ) {
    cli::cli_abort("{.arg {arg}} must be one non-empty string.")
  }
}
