# Reporting-area folds -------------------------------------------------------
#
# A fold is the one misattribution that leaves NO trace in the usual coverage
# reports. `polity_area_code` is the numeric key the builds aggregate on --
# `get_primary_production()` emits it AS its `area_code` and `build_trade.R`
# assigns `area_code := polity_area_code` outright -- so an area whose
# `polity_area_code` is not its own `area_code` has every row it reports summed
# into another bucket. Nothing is dropped, nothing is unresolved, and the
# crosswalk reports `mapping_status == "matched"`, which is why "0 rows
# unresolved" says nothing about it (#419).

#' List the reporting areas whose data is folded into another area code
#'
#' @description
#' A FAOSTAT reporting area is *folded* when [polity_area_crosswalk] gives it a
#' `polity_area_code` that is not its own `area_code`. Every row the area
#' reports is then summed into that other bucket, so the area disappears from
#' WHEP output without a single row being dropped or left unresolved. This lists
#' those areas, because the coverage reports cannot: a fold resolves perfectly
#' well, only to a territory that did not report the data.
#'
#' Two kinds exist, and they are not equally defensible:
#'
#' - `"fabio_rest_of_world"`: FABIO collapses the area into its single
#'   Rest-of-World row (`polity_area_code` 999, `ROW-1850-2023`). Most such
#'   areas report nothing, but several report substantial data of their own:
#'   Syria, Eswatini, New Caledonia, North Macedonia, Reunion, Guadeloupe,
#'   Palestine, the Faroe Islands. Their observed values are attributed to Rest
#'   of World.
#' - `"successor_state"`: the area is summed into the bucket of the state that
#'   succeeded it, which is a deliberate territorial identity rather than a
#'   loss: FAOSTAT area 62 "Ethiopia PDR" into 238 Ethiopia, and areas 276 Sudan
#'   and 277 South Sudan into 206 Sudan (former).
#'
#' Whether to lift the Rest-of-World fold is an open decision recorded in issue
#' 419; this function only makes the current state visible and changes nothing.
#' A build also warns, naming the areas and the row counts it actually folded.
#'
#' @section Measuring the alternative:
#' `options(whep.unfold_rest_of_world = TRUE)` promotes every Rest-of-World
#' member to its own `polity_area_code` for the whole pipeline, which is the
#' experiment the decision needs. It is **off by default and not a production
#' mode**: published WHEP values assume the fold, so every read of the crosswalk
#' warns while it is set. The `"successor_state"` folds are never lifted by it,
#' since those are territorial identities rather than a FABIO convention.
#'
#' Measured with it on a full-range `get_wide_cbs()` (1850-2023, all 61 members
#' promoted), global totals move by at most 1.2% (`stock_addition`) and by less
#' than 0.1% for `feed`, `production` and `processing`. An earlier measurement
#' recorded in issue 419 reported up to 13.7x; that comparison predates the
#' `dcast()` duplicate-key fix in `.select_best_source()` (issue 425) and does
#' not reproduce.
#'
#' @param crosswalk Crosswalk to inspect. Defaults to [polity_area_crosswalk].
#'
#' @returns A tibble with one row per folded reporting area, ordered by
#'   `area_code`:
#' - `area_code`: The reporting area whose data is folded away.
#' - `area_name`, `area_iso3c`: Its name and ISO3-like code.
#' - `polity_area_code`: The bucket its rows are summed into.
#' - `polity_code`, `polity_name`: The polity the fold attributes them to.
#' - `fold_kind`: `"fabio_rest_of_world"` or `"successor_state"`.
#'
#' @export
#'
#' @examples
#' folded <- folded_reporting_areas()
#' nrow(folded)
#' head(folded[folded$fold_kind == "successor_state", ], 4)
folded_reporting_areas <- function(crosswalk = NULL) {
  cw <- crosswalk %||% .polity_crosswalk()
  required <- c("area_code", "polity_area_code", "fabio_code")
  missing <- required[!rlang::has_name(cw, required)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg crosswalk} is missing {.field {missing}}."
    )
  }

  tibble::as_tibble(cw) |>
    dplyr::filter(
      !is.na(.data$area_code),
      !is.na(.data$polity_area_code),
      .data$area_code != .data$polity_area_code
    ) |>
    dplyr::mutate(
      fold_kind = dplyr::if_else(
        !is.na(.data$fabio_code) & .data$fabio_code == 999L,
        "fabio_rest_of_world",
        "successor_state"
      )
    ) |>
    dplyr::distinct(
      .data$area_code,
      .data$area_name,
      .data$area_iso3c,
      .data$polity_area_code,
      .data$polity_code,
      .data$polity_name,
      .data$fold_kind
    ) |>
    dplyr::arrange(.data$area_code, .data$polity_code)
}

# Report the folds that a single read ACTUALLY exercised, with row counts.
#
# `folded_reporting_areas()` lists which areas the crosswalk folds; this says
# which of them carried data in the source being aggregated, and how much. That
# is the number the fold hides: the crosswalk names 64 folded areas, but only 14
# of them have anything to fold, and until this warned nothing in a build
# distinguished the two. Warn rather than inform, because the whole defect is
# that the attribution was reportable and unreported.
.warn_folded_areas <- function(dt, source_label = NULL) {
  needed <- c("area_code", "polity_area_code")
  if (!all(needed %in% names(dt)) || nrow(dt) == 0L) {
    return(invisible(NULL))
  }
  folded <- .summarise_folded_rows(dt)
  if (nrow(folded) == 0L) {
    return(invisible(folded))
  }

  where <- if (is.null(source_label)) "this source" else source_label
  # One bullet per area, not a cli-truncated vector: naming them all IS the fix.
  bullets <- stats::setNames(folded$label, rep("*", nrow(folded)))
  cli::cli_warn(c(
    "!" = "{nrow(folded)} reporting area{?s} in {where}
           {cli::qty(nrow(folded))}{?is/are} folded into another
           {.field polity_area_code}, so {sum(folded$rows)} observed row{?s}
           {?is/are} attributed to a polity that did not report {?it/them}.",
    bullets,
    "i" = "Nothing is dropped and nothing is unresolved, so the coverage
           counts cannot show this. See {.fun folded_reporting_areas}."
  ))
  invisible(folded)
}

.summarise_folded_rows <- function(dt) {
  has_name <- "area_name" %in% names(dt)
  folded <- dt[
    !is.na(polity_area_code) &
      !is.na(area_code) &
      area_code != polity_area_code,
    .(rows = .N),
    by = c("area_code", "polity_area_code", if (has_name) "area_name")
  ]
  if (nrow(folded) == 0L) {
    return(folded)
  }
  data.table::setorderv(folded, "rows", order = -1L)
  shown <- if (has_name) folded$area_name else as.character(folded$area_code)
  folded[,
    label := sprintf(
      "%s (%d) -> %d (n = %d)",
      shown,
      area_code,
      polity_area_code,
      rows
    )
  ]
  folded
}

# The alternative to the fold, selectable and OFF by default.
#
# Lifting the fold is a modelling decision, not a bug fix, so it is not made
# here: `whep.unfold_rest_of_world` promotes each Rest-of-World member to its own
# `polity_area_code`, which is the experiment #419 exists to inform, and the
# default leaves every published number exactly where it is. Every consumer goes
# through `.polity_crosswalk()`, so one switch covers the whole pipeline instead
# of 30 call sites disagreeing.
.unfold_rest_of_world_option <- function() {
  isTRUE(getOption("whep.unfold_rest_of_world", FALSE))
}

.unfold_rest_of_world <- function(crosswalk) {
  if (!.unfold_rest_of_world_option()) {
    return(crosswalk)
  }
  promoted <- !is.na(crosswalk$fabio_code) &
    crosswalk$fabio_code == 999L &
    !is.na(crosswalk$area_code) &
    crosswalk$area_code != 999L
  if (!any(promoted)) {
    return(crosswalk)
  }
  crosswalk[promoted, polity_area_code := area_code]
  # Warned on EVERY read rather than once per session: the crosswalk is read
  # dozens of times in a build, and a run whose numbers do not match anything
  # published should be impossible to mistake for one that does. Session-level
  # "once" state would also make the warning untestable.
  cli::cli_warn(c(
    "!" = "{.code options(whep.unfold_rest_of_world = TRUE)} is set:
           {sum(promoted)} crosswalk row{?s} promoted out of the FABIO
           Rest-of-World bucket.",
    "i" = "Published WHEP values assume the fold. This is a sensitivity setting
           for issue 419, not a supported production mode."
  ))
  crosswalk
}
