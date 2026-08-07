#' Report reporting buckets that sum more than one territory
#'
#' @description
#' `polity_area_code` is an aggregation bucket, not an identity: FABIO folds
#' several FAOSTAT reporting areas into one numeric code, and WHEP's builds sum
#' them under it. `reporting_polity_code` is then resolved from that bucket
#' code, so a bucket folding more than one live territory can end up labelled
#' with a polity that covers only part of what the value covers, or that covers
#' the whole of it but in a period that has ended.
#'
#' This lists every `(polity_area_code, year)` that folds more than one polity
#' and classifies whether the bucket's own label covers the fold:
#'
#' - `"aggregate"`: the bucket resolves to an aggregate polity (Rest of World,
#'   Belgium-Luxembourg, the FAOSTAT combined-reporting entities), whose name
#'   and polygon already mean the union of its members. Honest.
#' - `"predecessor"`: the bucket is labelled with a polity that has **ended**,
#'   and whose published `successor` set is exactly the set of polities the
#'   bucket folds. The extent is right — that predecessor's territory is the
#'   union of its successors — but the period is not, so a consumer filtering
#'   polities by span drops the rows.
#' - `"partial"`: the bucket sums several territories but is labelled with a
#'   polity covering only part of them, so the value and its polity describe
#'   different extents. This is the worst case, and no bucket is in it today.
#' - `"unlabelled"`: the bucket code resolves to no polity, so rows carry `NA`
#'   and the gap is at least visible rather than wrong.
#'
#' An area counts as a member only in the years it **reports**: its polity must
#' be in span, and the upstream FAOSTAT map must report the area that year. A
#' year-aware lookup answers every `(area_code, year)` pair regardless, standing
#' in with the nearest period, so asking it about an area that does not report
#' in that year invents a member. FAOSTAT reports area 206 for 1961-2011 and
#' areas 276/277 for 2012-2024, never in the same year, so counting the stand-ins
#' reported bucket 206 as a three-way fold in all 65 years rather than a two-way
#' fold in the 14 it is one (whep#414).
#'
#' Bucket 206 is the one fold reported today, `"predecessor"` from 2012: it sums
#' FAOSTAT areas 276 Sudan and 277 South Sudan and is labelled `SUD-1956-2011`,
#' whose successors are exactly `SDN-2011-2025` and `SSD-2011-2025`. No **live**
#' polity means "Sudan and South Sudan"; whether to mint one upstream, or to
#' stop folding the two areas, is the open decision in whep#414. The un-fold is
#' costed in whep#680 — it moves nothing outside the region and loses 4.2% of
#' the region's own tonnage, so it is not a switch-flip.
#' `.aggregate_to_polities()` warns when it builds such a bucket; set
#' `options(whep.warn_polity_folds = FALSE)` to silence that warning.
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
        .bucket_is_predecessor(
          .data$bucket_polity_code,
          .data$bucket_mapping_status,
          .data$member_polity_codes
        ) ~ "predecessor",
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
    dplyr::filter(!is.na(.data$polity_code)) |>
    dplyr::left_join(.area_first_reported_year(), by = "area_code")
}

# The first year the upstream FAOSTAT map reports each area at all.
#
# The resolver bounds a period BELOW by `polity_start_year`, not by the map's
# reporting years, so it answers "which polity would area 276 be in 2011?" with
# `SDN-2011-2025` even though FAOSTAT does not report area 276 before 2012.
# That is right for the resolver -- a row that exists must resolve -- and wrong
# for this diagnostic, which asks which areas a bucket actually sums.
.area_first_reported_year <- function() {
  crosswalk <- .polity_crosswalk(include_unmapped = FALSE)
  empty <- tibble::tibble(
    area_code = integer(0),
    first_reported_year = integer(0)
  )
  if (!rlang::has_name(crosswalk, "map_year_start")) {
    return(empty)
  }
  tibble::as_tibble(crosswalk) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$map_year_start)) |>
    dplyr::summarise(
      first_reported_year = min(.data$map_year_start),
      .by = "area_code"
    )
}

# Only an area that REPORTS in the year is folded in that year.
#
# `.add_polity_columns_dt()` answers every `(area_code, year)` pair, standing in
# with the nearest period and reporting `out_of_span`. So resolving all three
# Sudan areas for 1990 returns SUD-1956-2011 (the one FAOSTAT actually reports)
# plus SDN-2011-2025 and SSD-2011-2025 as stand-ins, and counting those made the
# bucket look like a three-way fold in a year where only one area reports at
# all. Two bounds are needed because the resolver applies neither for this
# purpose: `out_of_span` drops a stand-in above a period, and the upstream map's
# first reported year drops one below it -- area 276 resolves to SDN-2011-2025
# from 2011 because that polity starts then, while FAOSTAT begins reporting the
# area in 2012.
#
# Measured on the FAOSTAT production pin, the reporting spans do not overlap:
# area 206 carries 13,759 rows over 1961-2011, area 276 carries 3,467 over
# 2012-2024 and area 277 carries 2,170 over 2012-2024.
.in_span_members <- function(resolved) {
  resolved |>
    dplyr::filter(
      is.na(.data$mapping_status) | .data$mapping_status != "out_of_span",
      is.na(.data$first_reported_year) |
        .data$year >= .data$first_reported_year
    )
}

.fold_members <- function(resolved) {
  .in_span_members(resolved) |>
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

# TRUE where the bucket's label is the folded members' shared predecessor.
#
# `SUD-1956-2011` labels bucket 206 from 2012 only because the year-aware lookup
# has nothing later to offer, so the label is flagged `out_of_span`. That reads
# as a defect until the extent is checked: the polities database publishes
# `SUD-1956-2011`'s successors as `SDN-2011-2025; SSD-2011-2025`, which is
# exactly the member set the bucket folds, so the label's territory IS the sum.
# Requiring both conditions keeps the class narrow -- a live polity has no
# successors yet, and an ended one whose successors are only some of the members
# stays `"partial"`.
.bucket_is_predecessor <- function(bucket_code, bucket_status, member_codes) {
  ended <- !is.na(bucket_status) & bucket_status == "out_of_span"
  successors <- .polity_successor_keys()
  key <- successors$successor_key[match(bucket_code, successors$polity_code)]
  ended & !is.na(key) & key == member_codes
}

# One row per polity, with its published successors written the same way
# `.fold_members()` writes a member set, so the two compare as plain strings.
.polity_successor_keys <- function() {
  polities <- whep::polities
  tibble::tibble(
    polity_code = as.character(polities$polity_code),
    successor_key = .successor_key(polities$successor)
  ) |>
    dplyr::filter(!is.na(.data$successor_key))
}

.successor_key <- function(successor) {
  successor |>
    stringr::str_split(";") |>
    purrr::map_chr(\(codes) {
      codes <- sort(unique(stringr::str_trim(codes[!is.na(codes)])))
      if (length(codes) == 0L) NA_character_ else paste(codes, collapse = ", ")
    })
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
  flagged <- polity_bucket_coverage(years = years) |>
    dplyr::filter(
      .data$coverage %in% c("partial", "predecessor"),
      .data$polity_area_code %in% buckets
    )
  if (nrow(flagged) > 0L) {
    .warn_bucket_coverage(flagged)
  }
  invisible(NULL)
}

.warn_bucket_coverage <- function(flagged) {
  folds <- flagged |>
    dplyr::summarise(
      year_range = paste0(min(.data$year), "-", max(.data$year)),
      .by = c(
        "polity_area_code",
        "member_polity_codes",
        "bucket_polity_code",
        "coverage"
      )
    )
  n <- nrow(folds)
  cli::cli_warn(c(
    "!" = paste(
      "{n} reporting bucket{?s} {?sums/sum} more than one territory under",
      "one polity."
    ),
    rlang::set_names(.bucket_coverage_bullets(folds), "*"),
    "i" = paste(
      "A {.val predecessor} label has the right extent but has ended; a",
      "{.val partial} one covers less than the value does. See",
      "{.fn polity_bucket_coverage}."
    ),
    "i" = "Silence with {.code options(whep.warn_polity_folds = FALSE)}."
  ))
}

# Say which of the two defects each bucket has, because they need different
# answers: a `"partial"` label is arithmetically wrong about the territory, a
# `"predecessor"` one names the right territory in a period that has ended.
.bucket_coverage_bullets <- function(folds) {
  verdict <- dplyr::if_else(
    folds$coverage == "predecessor",
    ", their ended predecessor.",
    ", which covers less."
  )
  paste0(
    "Bucket ",
    folds$polity_area_code,
    " (",
    folds$year_range,
    ") sums ",
    folds$member_polity_codes,
    " but is labelled ",
    folds$bucket_polity_code,
    verdict
  )
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
#' Three kinds exist, and they are not equally defensible:
#'
#' - `"fabio_rest_of_world"`: FABIO collapses the area into its single
#'   Rest-of-World row (`polity_area_code` 999, `ROW-1850-2025`) because its own
#'   region list does not enumerate the area either. 57 areas, all flagged
#'   `cbs` `FALSE` in [regions_full]. Several still report substantial data of
#'   their own -- Reunion, Guadeloupe, Palestine, the Faroe Islands -- which is
#'   attributed to Rest of World.
#' - `"cbs_reporter_folded"`: the area is flagged `cbs` `TRUE`, so
#'   [regions_full] says it has a commodity balance sheet of its own, and it is
#'   folded into 999 anyway. Four areas: 153 New Caledonia, 154 North Macedonia,
#'   209 Eswatini and 212 Syria, the last being the largest single contributor
#'   to the fold. **FABIO does not fold these**: its published region list
#'   enumerates all four as regions in their own right (see the section below),
#'   so this fold is WHEP's, not a FABIO convention, and the `"fabio"` label the
#'   other 57 carry does not apply.
#' - `"successor_state"`: the area is summed into the bucket of the state that
#'   succeeded it, which is a deliberate territorial identity rather than a
#'   loss: FAOSTAT area 62 "Ethiopia PDR" into 238 Ethiopia, and areas 276 Sudan
#'   and 277 South Sudan into 206 Sudan (former).
#'
#' Whether to lift the Rest-of-World fold is an open decision recorded in issue
#' 419; this function only makes the current state visible and changes nothing.
#' A build also warns, naming the areas and the row counts it actually folded.
#'
#' @section What FABIO's own region list says:
#' FABIO (Bruckner et al. 2019) publishes the region list it uses, and it
#' contains all four `"cbs_reporter_folded"` areas as regions of their own:
#'
#' - `io_codes.csv` of the FABIO v1.1 release (Zenodo record 2577067, the file
#'   `inst/scripts/compare_fabio.R` already downloads) enumerates 192 areas x
#'   125 commodities. Areas 153, 154, 209 and 212 each have their own 125-row
#'   block, distinct from area 999 `RoW`.
#' - The FABIO source repository
#'   (<https://github.com/fineprint-global/fabio>) folds an area into Rest of
#'   World exactly when it is absent from `inst/regions_full.csv` with
#'   `current == TRUE`. All four carry `current` `TRUE` there, and the 192
#'   codes that file flags `cbs` `TRUE` are precisely the 192 areas of
#'   `io_codes.csv`.
#'
#' So `fabio_code == 999` for these four is a statement WHEP makes, not one
#' FABIO makes. Correcting it in `regions_full` would move published values,
#' because `polity_area_code` is derived from `fabio_code`, so the contradiction
#' is left standing and reported here instead (issue 556).
#'
#' @section The Rest-of-World fold is no longer applied:
#' WHEP models every reporting member of bucket 999 in its own right. FABIO's
#' 192-country layout is a methodology this package compares against, not a
#' constraint on which territories it represents, and the choice of country set
#' is WHEP's to make (issue 459).
#'
#' That matters because the fold was never doing what its name suggests. Of the
#' 61 members, only about a third report anything at all; the rest contribute no
#' rows and folding them is arithmetically a no-op. Everything the bucket
#' actually carried came from the members that DO file returns -- Syria,
#' Eswatini, North Macedonia, New Caledonia, the Faroe Islands, Palestine,
#' Greenland and the like -- and folding them discarded whose data it was. So
#' promotion is self-limiting: an area with no rows is unaffected either way.
#'
#' Bucket 999 survives as a genuine residual for the territories that report
#' nothing. Measured on a full-range `get_wide_cbs()` (1850-2023), promotion
#' takes the published area count from 195 to 216 and moves global totals by at
#' most 0.99% (`stock_addition`), with every other column inside 0.4%.
#'
#' `options(whep.unfold_rest_of_world = "none")` restores the fold, which is
#' what reproducing a number published before this change requires. Because that
#' no longer matches the published series, every read of the crosswalk warns
#' while it is set. `"cbs_reporters"` re-folds all but the four
#' `"cbs_reporter_folded"` areas and warns for the same reason. The
#' `"successor_state"` folds are never lifted by any mode, since those are
#' territorial identities rather than a FABIO convention.
#'
#' An earlier measurement recorded in issue 419 reported this change at up to
#' 13.7x on `feed`; that comparison predates the `dcast()` duplicate-key fix in
#' `.select_best_source()` (issue 425) and does not reproduce.
#'
#' @param crosswalk Crosswalk to inspect. Defaults to [polity_area_crosswalk].
#'
#' @returns A tibble with one row per folded reporting area, ordered by
#'   `area_code`:
#' - `area_code`: The reporting area whose data is folded away.
#' - `area_name`, `area_iso3c`: Its name and ISO3-like code.
#' - `polity_area_code`: The bucket its rows are summed into.
#' - `polity_code`, `polity_name`: The polity the fold attributes them to.
#' - `fold_kind`: `"fabio_rest_of_world"`, `"cbs_reporter_folded"` or
#'   `"successor_state"`.
#'
#' @references
#' Bruckner, M., Wood, R., Moran, D., Kuschnig, N., Wieland, H., Maus, V.,
#' Borner, J. (2019). FABIO - The Construction of the Food and Agriculture
#' Input-Output Model. Environmental Science & Technology 53(19), 11302-11312.
#' \doi{10.1021/acs.est.9b03554}
#'
#' @export
#'
#' @examples
#' folded <- folded_reporting_areas()
#' nrow(folded)
#' head(folded[folded$fold_kind == "successor_state", ], 4)
#' folded[folded$fold_kind == "cbs_reporter_folded", ]
folded_reporting_areas <- function(crosswalk = NULL) {
  cw <- crosswalk %||% .polity_crosswalk()
  required <- c("area_code", "polity_area_code", "fabio_code", "cbs")
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
      fold_kind = dplyr::case_when(
        is.na(.data$fabio_code) | .data$fabio_code != 999L ~ "successor_state",
        .data$cbs %in% TRUE ~ "cbs_reporter_folded",
        TRUE ~ "fabio_rest_of_world"
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
#
# `"cbs_reporters"` is the narrower experiment #556 asks for: promote only the
# four areas FABIO's own region list enumerates as regions of their own, and
# leave the 57 folds FABIO agrees with alone.
.unfold_rest_of_world_modes <- function() {
  c("none", "all", "cbs_reporters")
}

.unfold_rest_of_world_mode <- function() {
  value <- getOption("whep.unfold_rest_of_world", "all")
  if (isTRUE(value)) {
    return("all")
  }
  if (is.null(value) || isFALSE(value)) {
    return("none")
  }
  modes <- .unfold_rest_of_world_modes()
  if (is.character(value) && length(value) == 1L && value %in% modes) {
    return(value)
  }
  cli::cli_abort(c(
    "{.code options(whep.unfold_rest_of_world = )} must be {.val TRUE},
     {.val FALSE} or one of {.val {modes}}.",
    "x" = "Got {.val {value}}."
  ))
}

# The ONE predicate deciding which members a mode promotes. `regions_full` and
# the crosswalk both state the fold, and a promotion once survived being
# withdrawn because only one of them was rebuilt (#419), so the two call sites
# share this rather than each spelling the condition out.
.rest_of_world_members <- function(areas, mode) {
  in_bucket <- !is.na(areas$fabio_code) &
    areas$fabio_code == 999L &
    !is.na(areas$area_code) &
    areas$area_code != 999L
  if (mode == "none") {
    return(rep(FALSE, length(in_bucket)))
  }
  if (mode == "all") {
    return(in_bucket)
  }
  in_bucket & areas$cbs %in% TRUE
}

.unfold_rest_of_world <- function(crosswalk) {
  mode <- .unfold_rest_of_world_mode()
  promoted <- .rest_of_world_members(crosswalk, mode)
  # The WARNING FOLLOWS THE DEFAULT, and the default is now `"all"`. It used to
  # fire whenever anything was promoted, because the fold was what WHEP
  # published; now promotion IS what WHEP publishes, so the thing worth warning
  # about is the opposite -- a run that re-folds and therefore does not match
  # anything published.
  #
  # Warned on EVERY read rather than once per session: the crosswalk is read
  # dozens of times in a build, and a run whose numbers do not match the
  # published series should be impossible to mistake for one that does.
  # Session-level "once" state would also make the warning untestable.
  if (mode != "all") {
    cli::cli_warn(c(
      "!" = "{.code whep.unfold_rest_of_world} is set to {.val {mode}}:
             the FABIO Rest-of-World fold is being applied to
             {sum(!promoted & .rest_of_world_members(crosswalk, 'all'))}
             reporting area{?s} that WHEP models in their own right.",
      "i" = "Published WHEP values do NOT fold them. This is a sensitivity
             setting for issues 419 and 556, not the production mode."
    ))
  }
  if (!any(promoted)) {
    return(crosswalk)
  }
  crosswalk[promoted, polity_area_code := area_code]
  crosswalk
}

# `regions_full` states the fold a second time, keyed on `code` rather than
# `area_code`, so the same predicate is applied to a renamed view of it.
.unfold_regions_full <- function(regions) {
  mode <- .unfold_rest_of_world_mode()
  if (mode == "none") {
    return(regions)
  }
  promoted <- .rest_of_world_members(
    tibble::tibble(
      fabio_code = as.integer(regions$fabio_code),
      area_code = as.integer(regions$code),
      cbs = regions$cbs
    ),
    mode
  )
  regions |>
    dplyr::mutate(
      polity_area_code = dplyr::if_else(
        promoted,
        as.integer(.data$code),
        as.integer(.data$polity_area_code)
      )
    )
}
