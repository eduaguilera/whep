#' Polity columns on WHEP outputs
#'
#' @description
#' Shared description of the polity columns that WHEP builders attach to their
#' output. Documented once and inherited, because eight functions emit these and
#' eight copies of one description is how half of them come to say something
#' different — the `polities_cats` and `regions_full` column lists drifted apart
#' exactly that way.
#'
#' @section Polity columns:
#' Every area-keyed output carries the polity its `area_code` resolves to in that
#' row's year:
#'
#' - `polity_area_code`: the numeric key rows are AGGREGATED on. Not the same as
#'   the legacy `area_code`, and not the same as FABIO's `fabio_code`: 17 areas
#'   keep their own key while FABIO folds them into rest-of-world, so grouping on
#'   one reproduces WHEP and grouping on the other reproduces FABIO.
#' - `reporting_polity_code`: the polity itself, e.g. `RUS-2014-2025`. Year-aware —
#'   the same `area_code` resolves to different polities in different years, which
#'   is the point of the crosswalk.
#' - `reporting_polity_name`: its name. Differs from an area's own name where the
#'   area folds into an aggregate; Bermuda reads "Latin America Other".
#' - `reporting_polity_has_geometry`: whether a polygon is attached, for callers
#'   that need to map or intersect. `FALSE` is a documented gap upstream, not an
#'   error.
#'
#' Pre-1961 rows resolve to the polity live in the back-cast anchor year rather
#' than to the polity live in the row's own year. See [add_polity_code()] for why,
#' and for the `whep_backcast_anchor` attribute that records it.
#'
#' Rows whose area resolves to no polity are dropped before this point, with the
#' reason reported: a FAOSTAT regional group, a deliberate non-mapping such as 351
#' China, or — the case that warns — an area code the project does not know.
#'
#' @name whep_polity_columns
#' @keywords internal
NULL
