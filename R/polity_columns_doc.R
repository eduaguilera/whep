#' Polity columns on WHEP outputs
#'
#' @description
#' Shared description of the reporting-polity columns that WHEP's area-keyed
#' outputs attach. Documented once and inherited, because many functions emit
#' these and one copy of the description per function is how half of them come
#' to say something different.
#'
#' @section Polity columns:
#' Every area-keyed output carries the polity its `area_code` resolves to in
#' that row's year:
#'
#' - `polity_area_code`: The numeric key rows are AGGREGATED on, for the matrix
#'   workflows. It is a bucket, not an identity: use `reporting_polity_code` to
#'   say which territory a row belongs to.
#' - `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It is
#'   year-aware, so the same `area_code` resolves to different polities in
#'   different years, which is the point of the crosswalk.
#' - `reporting_polity_name`: Its name. It can differ from the area's own name
#'   where the area folds into an aggregate.
#' - `reporting_polity_has_geometry`: Whether the polity has a polygon in the
#'   WHEP polity database, for callers that need to map or intersect it. `FALSE`
#'   is a documented gap upstream, not an error.
#'
#' Rows whose `area_code` resolves to no polity keep the columns with `NA`
#' rather than being dropped, so a gap is visible instead of silent.
#'
#' Rows before the back-cast anchor year resolve to the polity live in that
#' anchor year rather than to the polity live in the row's own year, because
#' WHEP's pre-anchor series are back-cast onto the anchor-year territory. See
#' [add_polity_code()] for the reasoning.
#'
#' @name whep_polity_columns
#' @keywords internal
NULL
