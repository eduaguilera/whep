#' Report reporting buckets whose polity covers less than the bucket
#'
#' @description
#' `polity_area_code` is an aggregation bucket, not an identity: FABIO folds
#' several FAOSTAT reporting areas into one numeric code, and WHEP's builds sum
#' them under it. `reporting_polity_code` is then resolved from that bucket
#' code, so a bucket folding more than one live territory can end up labelled
#' with a polity covering only part of what the value covers.
#'
#' This lists every `(polity_area_code, year)` that folds more than one polity
#' and classifies whether the bucket's own label covers the fold:
#'
#' - `"aggregate"`: the bucket resolves to an aggregate polity (Rest of World,
#'   Belgium-Luxembourg, the FAOSTAT combined-reporting entities), whose name
#'   and polygon already mean the union of its members. Honest.
#' - `"partial"`: the bucket sums several territories but is labelled with a
#'   single-territory polity, so the value and its polity describe different
#'   extents. This is the defect.
#' - `"unlabelled"`: the bucket code resolves to no polity, so rows carry `NA`
#'   and the gap is at least visible rather than wrong.
#'
#' The known `"partial"` case is bucket 206, which folds FAOSTAT areas 276
#' Sudan and 277 South Sudan after the 2011 secession while no live polity
#' means "Sudan and South Sudan" (whep#414). `.aggregate_to_polities()` warns
#' when it builds such a bucket; set `options(whep.warn_polity_folds = FALSE)`
#' to silence that warning.
#'
#' The polity reported here as the bucket's own is also the `area` label the
#' builds attach to the summed row, and the one the reporting columns resolve.
#' A bucket carries one label whatever its members resolve to, because `area`
#' is a join key and a bucket under two labels stops summing (whep#563).
#'
#' @param years Integer vector of years to classify. Defaults to the FAOSTAT
#'   reporting era, 1961 to 2025. Years before the back-cast anchor resolve to
#'   the anchor-year territory, so they classify identically to 1961.
#'
#' @returns A tibble with one row per folded `(polity_area_code, year)`, with
#'   the folded member polities, the polity the bucket itself resolves to, and
#'   the `coverage` classification. Zero rows means no bucket folds more than
#'   one polity in the requested years.
#' @export
#' @examples
#' polity_bucket_coverage(years = 2015L)
polity_bucket_coverage <- function(years = NULL) {
  resolved <- .resolve_all_area_years(.coverage_years(years))
  .fold_members(resolved) |>
    dplyr::left_join(
      .fold_bucket_labels(resolved),
      by = c("polity_area_code", "year")
    ) |>
    dplyr::mutate(
      coverage = dplyr::case_when(
        is.na(.data$bucket_polity_code) ~ "unlabelled",
        !is.na(.data$bucket_polity_type) &
          .data$bucket_polity_type == "aggregate" ~ "aggregate",
        TRUE ~ "partial"
      )
    ) |>
    dplyr::arrange(.data$polity_area_code, .data$year)
}

# ---- helpers ----------------------------------------------------------------

.coverage_years <- function(years) {
  if (is.null(years)) {
    return(1961L:2025L)
  }
  out <- sort(unique(as.integer(years[!is.na(years)])))
  if (length(out) == 0L) {
    cli::cli_abort("{.arg years} must contain at least one non-missing year.")
  }
  out
}

# Resolve every mapped reporting area for every requested year, through the
# same helper the builds use, so this reports what they actually get rather
# than a second, subtly different reading of the crosswalk.
.resolve_all_area_years <- function(years) {
  crosswalk <- .polity_crosswalk(include_unmapped = FALSE)
  areas <- sort(unique(stats::na.omit(crosswalk$area_code)))
  grid <- data.table::CJ(area_code = areas, year = years)
  .add_polity_columns_dt(
    grid,
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$polity_code))
}

.fold_members <- function(resolved) {
  resolved |>
    dplyr::summarise(
      n_member_polities = dplyr::n_distinct(.data$polity_code),
      member_polity_codes = paste(
        sort(unique(.data$polity_code)),
        collapse = ", "
      ),
      member_area_codes = paste(sort(unique(.data$area_code)), collapse = ", "),
      .by = c("polity_area_code", "year")
    ) |>
    dplyr::filter(.data$n_member_polities > 1L)
}

# The label a folded bucket carries is whatever its own numeric code resolves
# to, because `.aggregate_to_polities()` renames `polity_area_code` onto
# `area_code` and the reporting columns are resolved from that.
.fold_bucket_labels <- function(resolved) {
  resolved |>
    dplyr::filter(.data$area_code == .data$polity_area_code) |>
    dplyr::transmute(
      polity_area_code = .data$polity_area_code,
      year = .data$year,
      bucket_polity_code = .data$polity_code,
      bucket_polity_name = .data$polity_name,
      bucket_mapping_status = .data$mapping_status
    ) |>
    dplyr::left_join(
      .polity_type_lookup(),
      by = c("bucket_polity_code" = "polity_code")
    )
}

.polity_type_lookup <- function() {
  .polity_crosswalk(include_unmapped = FALSE) |>
    tibble::as_tibble() |>
    dplyr::distinct(
      polity_code = .data$polity_code,
      bucket_polity_type = .data$polity_type
    )
}

# Warn where a build has just summed several territories into one bucket whose
# polity names only part of them. Wired into `.aggregate_to_polities()`, which
# is where the sum is created; the reporting-column helper runs on ~100 outputs
# per build and warning there would drown the message it is trying to send.
.warn_partial_bucket_polities <- function(dt) {
  if (!isTRUE(getOption("whep.warn_polity_folds", TRUE))) {
    return(invisible(NULL))
  }
  buckets <- unique(stats::na.omit(dt$polity_area_code))
  years <- unique(stats::na.omit(dt$year))
  if (length(buckets) == 0L || length(years) == 0L) {
    return(invisible(NULL))
  }
  partial <- polity_bucket_coverage(years = years) |>
    dplyr::filter(
      .data$coverage == "partial",
      .data$polity_area_code %in% buckets
    )
  if (nrow(partial) > 0L) {
    .warn_bucket_coverage(partial)
  }
  invisible(NULL)
}

.warn_bucket_coverage <- function(partial) {
  folds <- partial |>
    dplyr::summarise(
      year_range = paste0(min(.data$year), "-", max(.data$year)),
      .by = c(
        "polity_area_code",
        "member_polity_codes",
        "bucket_polity_code"
      )
    )
  n <- nrow(folds)
  bullets <- paste0(
    "Bucket ",
    folds$polity_area_code,
    " (",
    folds$year_range,
    ") sums ",
    folds$member_polity_codes,
    " but is labelled ",
    folds$bucket_polity_code,
    "."
  )
  cli::cli_warn(c(
    "!" = paste(
      "{n} reporting bucket{?s} {?sums/sum} more than one territory but",
      "{?carries/carry} a single-territory polity."
    ),
    rlang::set_names(bullets, "*"),
    "i" = paste(
      "The value and its {.field reporting_polity_code} describe different",
      "extents. See {.fn polity_bucket_coverage}."
    ),
    "i" = "Silence with {.code options(whep.warn_polity_folds = FALSE)}."
  ))
}

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
  # Shares `whep.warn_polity_folds` with `.warn_partial_bucket_polities()`, which
  # fires at the same site. The two report different halves of one phenomenon --
  # that one names the bucket whose label covers only part of what it sums, this
  # one names the areas whose rows were moved into it -- so a caller silencing
  # fold diagnostics means both, and one option is less surprising than two.
  if (!isTRUE(getOption("whep.warn_polity_folds", TRUE))) {
    return(invisible(NULL))
  }
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

# The `area` label an aggregation bucket carries, one per (bucket, year).
#
# A bucket is a numeric key that several reporting areas are summed into, so its
# label has to be a property of the BUCKET. Taking it from a member row instead
# -- which is what grouping by `polity_name` did -- means a bucket whose members
# resolve to different polities comes out under several labels, and the sum the
# bucket exists to produce never happens (whep#563, the defect that forced the
# revert of whep#480 in whep#561). Resolving the bucket's own code is also what
# `polity_bucket_coverage()` documents as the label a fold carries, and what
# `add_reporting_polity_columns()` resolves downstream, so this makes the
# aggregator agree with both rather than inventing a third rule.
#
# `dt` must already carry the polity columns, i.e. be past
# `.add_polity_columns_dt()`.
.bucket_area_labels <- function(dt) {
  # The member label is only a fallback, for a bucket whose own code resolves to
  # no polity in that year (an aggregate whose period has ended). Deterministic
  # by lowest `area_code` so it cannot depend on row order or on which member
  # happens to report.
  members <- dt[
    !is.na(polity_area_code),
    .(member_name = polity_name[which.min(area_code)]),
    by = c("polity_area_code", "year")
  ]
  if (nrow(members) == 0L) {
    return(members[, .(polity_area_code, year, area = character(0))])
  }
  resolved <- .add_polity_columns_dt(
    data.table::data.table(
      area_code = members$polity_area_code,
      year = members$year
    ),
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  )
  members[, area := data.table::fcoalesce(resolved$polity_name, member_name)]
  members[, member_name := NULL]
  members
}

# Attach the bucket labels to an aggregated table and rename it to the
# `area_code` / `area` pair the rest of the pipeline expects, in the column
# order the grouped key already had.
.apply_bucket_area_labels <- function(dt, labels) {
  # An update-join, not a merge: it cannot drop or reorder a row, so the label
  # is provably an annotation rather than a second filter.
  dt[labels, on = c("polity_area_code", "year"), area := i.area]
  data.table::setnames(dt, "polity_area_code", "area_code")
  data.table::setcolorder(dt, c("year", "area_code", "area"))
  dt
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
