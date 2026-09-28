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
#' Bucket 206 is the one fold reported today, and it is `"aggregate"` for
#' 2012-2025: it sums FAOSTAT areas 276 Sudan and 277 South Sudan and resolves
#' to `F206-2011-2025` "Sudan and South Sudan (combined reporting)", the
#' aggregate whep#860 wired in. It read `"predecessor"` before that, labelled
#' with `SUD-1956-2011`, a polity that had ended.
#'
#' So the labelling is settled and only the regionalisation is still open:
#' whether to keep folding the two successors at all is whep#680, and
#' `options(whep.unfold_predecessor_bucket = "all")` is the switch that
#' promotes them. It is off by default because it is not mass-neutral — see
#' [folded_reporting_areas()] for what it withdraws and why.
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
    dplyr::left_join(.area_reported_year_bounds(), by = "area_code")
}

# The years the upstream FAOSTAT map reports each area at all, both ends.
#
# The resolver bounds a period BELOW by `polity_start_year`, not by the map's
# reporting years, so it answers "which polity would area 276 be in 2011?" with
# `SDN-2011-2025` even though FAOSTAT does not report area 276 before 2012.
# That is right for the resolver -- a row that exists must resolve -- and wrong
# for this diagnostic, which asks which areas a bucket actually sums.
#
# BOTH BOUNDS COME FROM THE MAP NOW, and the upper one used to arrive by
# accident. Area 206 stops reporting in 2011, and what dropped it from bucket
# 206's member set for 2012 onward was `mapping_status == "out_of_span"` --
# true only while the bucket had nothing later than `SUD-1956-2011` to resolve
# to. whep#860 gives it `F206-2011-2025` from 2012, which resolves `matched`,
# so the area re-entered its own bucket's member set and the fold read as three
# polities (its own aggregate label plus the two successors) instead of two.
# The documented rule was always "the upstream map must report the area that
# year"; only half of it was implemented.
.area_reported_year_bounds <- function() {
  crosswalk <- .polity_crosswalk(include_unmapped = FALSE)
  empty <- tibble::tibble(
    area_code = integer(0),
    first_reported_year = integer(0),
    last_reported_year = integer(0)
  )
  if (!rlang::has_name(crosswalk, "map_year_start")) {
    return(empty)
  }
  tibble::as_tibble(crosswalk) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$map_year_start)) |>
    dplyr::summarise(
      first_reported_year = min(.data$map_year_start),
      last_reported_year = max(.data$map_year_end),
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
# all. Three bounds are needed because the resolver applies none of them for
# this purpose: `out_of_span` drops a stand-in on a period that never resolves,
# and the upstream map's reporting years drop one below and one above it -- area
# 276 resolves to SDN-2011-2025 from 2011 because that polity starts then, while
# FAOSTAT begins reporting the area in 2012, and area 206 resolves to
# F206-2011-2025 from 2012 as the BUCKET's label while FAOSTAT stopped reporting
# the AREA in 2011.
#
# Measured on the FAOSTAT production pin, the reporting spans do not overlap:
# area 206 carries 13,759 rows over 1961-2011, area 276 carries 3,467 over
# 2012-2024 and area 277 carries 2,170 over 2012-2024.
.in_span_members <- function(resolved) {
  resolved |>
    dplyr::filter(
      is.na(.data$mapping_status) | .data$mapping_status != "out_of_span",
      is.na(.data$first_reported_year) |
        .data$year >= .data$first_reported_year,
      is.na(.data$last_reported_year) |
        .data$year <= .data$last_reported_year
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
#' @section The predecessor-bucket fold, and the switch that lifts it:
#' `"predecessor_bucket"` is the other direction and a separate switch. FAOSTAT
#' retired area 206 "Sudan (former)" at the 2011 secession and reports 276 Sudan
#' and 277 South Sudan from 2012, so here the *bucket* is the dead code and its
#' members are live -- the reverse of 62 Ethiopia PDR folding into its live
#' successor 238, which stays a `"successor_state"` fold and is never lifted.
#'
#' WHEP publishes the fold. `options(whep.unfold_predecessor_bucket = "all")`
#' promotes the successors and warns on every crosswalk read, because unlike the
#' Rest-of-World promotion it is **not** mass-neutral. Measured on
#' `build_primary_production(2015, 2015)` it moves nothing at all outside areas
#' 206, 276 and 277 -- 48,678 rows, no key present in only one run, no matched
#' value differing -- and inside the region it withdraws exactly one series:
#' item 651 Forage products, 1,432,940 t (4.22% of the region's tonnage) and
#' 208,350 ha (0.25%). Heads, livestock units and slaughtered heads are
#' conserved; item 1052 Chickens, layers splits exactly, 9,439,000 head to
#' Sudan and 4,679,716 to South Sudan.
#'
#' That tonnage is not reported data that stops being joined. It is
#' `DM_yield_estimate_carried_forward`, WHEP's own extrapolation of the
#' `faostat-production-old` fodder series for area 206, a source that carries
#' no row for 276 or 277 in any year; the fold is what keeps bucket 206 a live
#' key to extrapolate onto after FAOSTAT retired it in 2011. Restoring it under
#' the promotion would need
#' a rule for apportioning a predecessor's series between its successors, which
#' this package has no source for. Whether to publish the promotion is issue
#' 680.
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
#' - `fold_kind`: `"fabio_rest_of_world"`, `"cbs_reporter_folded"`,
#'   `"predecessor_bucket"` or `"successor_state"`.
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
#' folded[folded$fold_kind == "predecessor_bucket", ]
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
        .data$polity_area_code %in% .predecessor_bucket_codes(cw) ~
          "predecessor_bucket",
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

#' Report which Rest-of-World members report under their own territory
#'
#' @description
#' Promoting a member of the FABIO Rest-of-World bucket has two halves: the
#' numeric one, which stops its rows being summed into `polity_area_code` 999,
#' and the territorial one, which lets it publish under its own year-scoped
#' `reporting_polity_code` instead of the bucket's `ROW-1850-2025`. Only the
#' first is unconditional. The second needs upstream to name a polity for the
#' area in `faostat_area_polity_map.csv`, and for 30 of the 61 members it does
#' not (whep#717).
#'
#' Without this, a member still on `ROW-1850-2025` is indistinguishable from the
#' genuine residual: both publish an aggregate polity on the continent `"World"`
#' with no geometry, and nothing says which is which. This names them, and says
#' for each what is missing.
#'
#' @section The statuses:
#' - `"own_polity"`: promoted, and publishing under its own polity. 31 members.
#' - `"polity_unmapped"`: promoted, still on `ROW-1850-2025`, but a live
#'   non-aggregate polity carrying the area's ISO3 **does** exist upstream. All
#'   that is missing is a row of the FAOSTAT area map naming it for this area,
#'   so this is the actionable list to send upstream. 6 members: 22 Aruba,
#'   71 French Southern and Antarctic Territories, 94 Holy See, 218 Tokelau,
#'   243 Wallis and Futuna, 271 South Georgia and the South Sandwich Islands.
#' - `"no_polity"`: promoted, still on `ROW-1850-2025`, and upstream has no
#'   polity for the territory at all. 24 members. Three of them are not
#'   territories and must never acquire one -- 252 `"Unspecified"`,
#'   254 `"Others (adjustment)"` and the 999 bucket itself, which is excluded
#'   here because it is the residual rather than a member of it.
#' - `"folded"`: this run is re-folding the member, so it is summed into bucket
#'   999 and carries the bucket's polity. Only under
#'   `options(whep.unfold_rest_of_world = "none")` or `"cbs_reporters"`.
#'
#' The ISO3 test behind `"polity_unmapped"` decides **nothing**: it reports that
#' upstream holds a polity this package cannot reach, which is a gap in the map,
#' not a mapping. Resolving an area to a polity by matching ISO3 downstream is
#' the defect whep#711 removed, and #717 argues explicitly against re-minting a
#' territorial identity here.
#'
#' @param crosswalk Crosswalk to inspect. Defaults to the crosswalk this run
#'   resolves through, so the answer reflects
#'   `options(whep.unfold_rest_of_world)`.
#'
#' @returns A tibble with one row per Rest-of-World member, ordered by
#'   `area_code`:
#' - `area_code`, `area_name`, `area_iso3c`: The member.
#' - `cbs`: Whether [regions_full] flags it as a commodity-balance reporter,
#'   which is what `"cbs_reporters"` promotes.
#' - `status`: One of the four above.
#' - `n_periods`: How many polity periods it resolves through.
#' - `polity_codes`: Those periods, comma-separated.
#'
#' @seealso [folded_reporting_areas()] for the areas whose rows are summed into
#'   another area's code, and [polity_mapping_provenance()] for which authority
#'   a resolved row rests on.
#' @export
#' @examples
#' status <- row_promotion_status()
#' table(status$status)
#' status[status$status == "polity_unmapped", c("area_code", "area_name")]
row_promotion_status <- function(crosswalk = NULL) {
  cw <- tibble::as_tibble(crosswalk %||% .polity_crosswalk())
  required <- c(
    "area_code",
    "area_name",
    "area_iso3c",
    "cbs",
    "fabio_code",
    "polity_code",
    "polity_area_code"
  )
  missing <- required[!rlang::has_name(cw, required)]
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg crosswalk} is missing {.field {missing}}.")
  }
  bucket_polity <- unique(cw$polity_code[cw$area_code %in% 999L])
  cw |>
    dplyr::filter(
      !is.na(.data$area_code),
      !is.na(.data$fabio_code),
      .data$fabio_code == 999L,
      .data$area_code != 999L
    ) |>
    dplyr::summarise(
      cbs = any(.data$cbs %in% TRUE),
      promoted = all(.data$area_code == .data$polity_area_code),
      own_polity = !any(.data$polity_code %in% bucket_polity),
      n_periods = dplyr::n(),
      polity_codes = paste(sort(unique(.data$polity_code)), collapse = ", "),
      .by = c("area_code", "area_name", "area_iso3c")
    ) |>
    dplyr::mutate(
      status = dplyr::case_when(
        !.data$promoted ~ "folded",
        .data$own_polity ~ "own_polity",
        .data$area_iso3c %in% .iso3_with_own_polity() ~ "polity_unmapped",
        TRUE ~ "no_polity"
      )
    ) |>
    dplyr::select(!c("promoted", "own_polity")) |>
    dplyr::relocate("status", .after = "cbs") |>
    dplyr::arrange(.data$area_code)
}

# The ISO3 codes upstream holds a real territorial polity for, applying the same
# two filters `data-raw/table_mappings.R` applies when it builds the crosswalk's
# resolution candidates: retired and superseded polities are not candidates, and
# an aggregate is not a territory.
#
# Used ONLY to classify a member the map does not name, never to resolve one.
# The distinction matters: "upstream has a polity for ESH but no map row reaches
# it" is a report about the map, whereas resolving an area to a polity because
# their ISO3 strings agree is the inference whep#711 deleted.
.iso3_with_own_polity <- function() {
  live <- is.na(polities$wiki_status) |
    !polities$wiki_status %in% c("retired", "superseded")
  territorial <- !is.na(polities$polity_type) &
    polities$polity_type != "aggregate"
  unique(stats::na.omit(polities$iso3_code[live & territorial]))
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

# The identity an aggregation bucket carries, one row per (bucket, year): the
# `area` label AND the polity the label came from.
#
# A bucket is a numeric key that several reporting areas are summed into, so its
# identity has to be a property of the BUCKET. Taking it from a member row
# instead -- which is what grouping by `polity_name` did -- means a bucket whose
# members resolve to different polities comes out under several labels, and the
# sum the bucket exists to produce never happens (whep#563, the defect that
# forced the revert of whep#480 in whep#561). Resolving the bucket's own code is
# also what `polity_bucket_coverage()` documents as the label a fold carries, and
# what `.add_reporting_polity_columns()` resolves downstream, so this makes the
# aggregator agree with both rather than inventing a third rule.
#
# The polity columns come out under their PUBLISHED names, from the same
# resolution that produced the label. The fold has always had the polity code in
# hand here and then thrown it away, leaving ~100 outputs to re-derive it at the
# tail (whep#670); emitting it under the published names means no second
# vocabulary and no second resolution to disagree with.
#
# `dt` must already carry the polity columns, i.e. be past
# `.add_polity_columns_dt()`.
.bucket_area_labels <- function(dt) {
  # The member label is only a fallback, for a bucket whose own code resolves to
  # no polity in that year (an aggregate whose period has ended). Deterministic
  # by lowest `area_code` so it cannot depend on row order or on which member
  # happens to report. It is a fallback for the LABEL only: the polity columns
  # stay NA there, exactly as the downstream resolution leaves them, because a
  # member's polity is not the bucket's identity.
  members <- dt[
    !is.na(polity_area_code),
    .(member_name = polity_name[which.min(area_code)]),
    by = c("polity_area_code", "year")
  ]
  if (nrow(members) == 0L) {
    return(members[, .(
      polity_area_code = polity_area_code,
      year = year,
      area = character(0),
      reporting_polity_area_code = integer(0),
      reporting_polity_code = character(0),
      reporting_polity_name = character(0),
      reporting_polity_has_geometry = logical(0)
    )])
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
  members[, `:=`(
    area = data.table::fcoalesce(resolved$polity_name, member_name),
    reporting_polity_area_code = resolved$polity_area_code,
    reporting_polity_code = resolved$polity_code,
    reporting_polity_name = resolved$polity_name,
    reporting_polity_has_geometry = resolved$has_geometry
  )]
  members[, member_name := NULL]
  members
}

# The bucket identity columns `.bucket_area_labels()` attaches. Every one of
# them is what `.add_reporting_polity_columns()` publishes for the same key,
# `reporting_polity_area_code` included: a bucket whose own code resolves to no
# polity in that year gets NA there, and copying the bucket code in instead
# would be the one value on which the two paths disagree.
.bucket_identity_cols <- function() {
  c(
    "area",
    "reporting_polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry"
  )
}

# Attach the bucket identity to an aggregated table and rename it to the
# `area_code` / `area` pair the rest of the pipeline expects, in the column
# order the grouped key already had.
#
# `polity_area_code` survives the rename, as the bucket's own resolution rather
# than as a copy of the key, so an aggregated frame satisfies
# `polity_area_code == area_code` wherever the bucket resolves at all -- the
# fixed-point property that tells `.add_reporting_polity_columns()` the carried
# identity was resolved for the key the frame still has.
.apply_bucket_area_labels <- function(dt, labels) {
  cols <- .bucket_identity_cols()
  # An update-join, not a merge: it cannot drop or reorder a row, so the identity
  # is provably an annotation rather than a second filter.
  dt[
    labels,
    on = c("polity_area_code", "year"),
    (cols) := mget(paste0("i.", cols))
  ]
  data.table::setnames(dt, "polity_area_code", "area_code")
  dt[, polity_area_code := reporting_polity_area_code]
  dt[, reporting_polity_area_code := NULL]
  # The `year` / `area_code` / `area` prefix is left exactly where it was and the
  # identity columns trail the frame, so a consumer of an aggregated table sees
  # columns added and nothing moved. `.add_reporting_polity_columns()` puts them
  # in their published position on the way out, as it always has.
  data.table::setcolorder(dt, c("year", "area_code", "area"))
  dt
}

# Which Rest-of-World members are modelled in their own right. Every consumer
# goes through `.polity_crosswalk()`, so one switch covers the whole pipeline
# instead of 30 call sites disagreeing.
#
# A promotion has two halves, and each mode does both or neither:
#
#   the NUMERIC half        `polity_area_code := area_code`, so the member's
#                           rows stop being summed into bucket 999.
#   the TERRITORIAL half    the member publishes under its own year-scoped
#                           polity code, polity name and geometry flag instead
#                           of the bucket's `ROW-1850-2025`, wherever upstream
#                           names one (#717).
#
# The numeric half landed alone in #628, which is what left 62 areas reporting
# as themselves and identified as an aggregate on the continent "World".
#
#   "all"            the default and what WHEP publishes: every member of
#                    bucket 999 is promoted.
#   "none"           re-folds everything, restoring the crosswalk exactly as it
#                    was -- the fold rows are still in the shipped table, so
#                    this reproduces a number published under the fold. Warns.
#   "cbs_reporters"  the narrower experiment #556 asks for: promote only the
#                    four areas FABIO's own region list enumerates as regions of
#                    their own, and leave the 57 folds FABIO agrees with alone.
#                    Those four are promoted in both halves; the rest keep the
#                    bucket's code AND the bucket's polity. Warns.
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
  promoted_areas <- unique(crosswalk$area_code[promoted])
  out <- .select_row_identity_rows(crosswalk, promoted_areas)
  if (length(promoted_areas) == 0L) {
    return(out)
  }
  out[area_code %in% promoted_areas, polity_area_code := area_code]
  out
}

# WHICH OF THE TWO ANSWERS A REST-OF-WORLD MEMBER GETS.
#
# The crosswalk carries both (see `data-raw/table_mappings.R`): a
# `"fabio_row_fold"` row saying `ROW-1850-2025` over the whole span, and, where
# upstream names one, `"fabio_row_promoted"` rows saying the real polity, one per
# period the upstream FAOSTAT map declares. Exactly one of the two survives per
# area, which is what makes this a choice rather than a fallback: leaving the
# fold row in would let `ROW-1850-2025` -- a period covering 1850-2025 -- answer
# for any year the promoted rows do not reach, so an area whose upstream periods
# stop in 1950 would quietly go back to being Rest of World for 1961 onward
# instead of reporting the coverage gap it has.
#
# An area with no promoted rows keeps its fold row and stays on the bucket's
# polity. That is the 31 members upstream names nowhere in the FAOSTAT map, and
# it is deliberate: minting a territorial identity for them here is exactly what
# issue 717 argues WHEP must not do. `row_promotion_status()` reports them.
#
# A caller-supplied or mocked crosswalk with no `mapping_source` column carries
# neither kind of row, so there is nothing to choose between and the frame is
# returned untouched.
.select_row_identity_rows <- function(crosswalk, promoted_areas) {
  if (!rlang::has_name(crosswalk, "mapping_source")) {
    return(crosswalk)
  }
  source <- crosswalk$mapping_source
  is_promoted_row <- !is.na(source) & source == "fabio_row_promoted"
  is_fold_row <- !is.na(source) & source == "fabio_row_fold"
  in_promoted <- crosswalk$area_code %in% promoted_areas
  has_own_identity <- crosswalk$area_code %in%
    crosswalk$area_code[is_promoted_row]
  drop <- (is_promoted_row & !in_promoted) |
    (is_fold_row & in_promoted & has_own_identity)
  if (!any(drop)) {
    return(crosswalk)
  }
  crosswalk[!drop]
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

# -- The predecessor-bucket un-fold (whep#680) ---------------------------------
#
# The other fold in the table, and the opposite shape to Rest of World. FAOSTAT
# retired area 206 "Sudan (former)" at the 2011 secession and reports 276 Sudan
# and 277 South Sudan from 2012; WHEP sums both successors back into the
# predecessor's code, because that is FABIO's region. So bucket 206 is a live
# key for two territories whose own reporting codes are live too.
#
# `"none"` is the default and is what WHEP publishes. `"all"` promotes the
# successors, and it is a SENSITIVITY setting rather than a second production
# mode, because it is not mass-neutral and the two series it drops are
# imputations rather than reported data (see below). Which one WHEP should
# publish is issue 680's open modelling decision; both readings are implemented
# so that it can be measured rather than argued, and neither is a fallback for
# the other.
#
# What `"all"` withdraws, measured on `build_primary_production(2015, 2015)`, is
# ONE series: item 651 Forage products, 1,432,940 t (-4.22% of the region's
# tonnage) and 208,350 ha (-0.25%). Heads, livestock units and slaughtered heads
# are conserved to the rounding, and outside areas 206/276/277 not one of the
# 48,678 rows moves.
#
# That tonnage is not a reported value that stops being joined. It is
# `DM_yield_estimate_carried_forward`, WHEP's own extrapolation of the
# `faostat-production-old` fodder series for area 206 -- a source holding
# 1961-2013 and carrying NO row for 276 or 277 at any year. Folded, that
# predecessor series is carried forward into years FAOSTAT has not reported
# area 206 since 2011, because the fold
# keeps bucket 206 a live key; promoted, the successors have no fodder source at
# all. Restoring it under the promotion would mean apportioning a predecessor's
# series between its successors, which needs a share rule this package has no
# source for and must not invent.
#
# The other series whep#680 costed, item 1052 Chickens, layers, is no longer
# affected: the promotion splits its 14,118,716 head exactly, 9,439,000 to
# Sudan and 4,679,716 to South Sudan. whep#1050's `.restore_unproduced_stocks()`
# re-emits a reported herd whose products are unreported, which is what used to
# make South Sudan's share depend on Sudan's egg tonnage.
.predecessor_unfold_modes <- function() {
  c("none", "all")
}

.predecessor_unfold_mode <- function() {
  value <- getOption("whep.unfold_predecessor_bucket", "none")
  if (isTRUE(value)) {
    return("all")
  }
  if (is.null(value) || isFALSE(value)) {
    return("none")
  }
  modes <- .predecessor_unfold_modes()
  if (is.character(value) && length(value) == 1L && value %in% modes) {
    return(value)
  }
  cli::cli_abort(c(
    "{.code options(whep.unfold_predecessor_bucket = )} must be {.val TRUE},
     {.val FALSE} or one of {.val {modes}}.",
    "x" = "Got {.val {value}}."
  ))
}

# The buckets whose own reporting area stopped reporting BEFORE the areas folded
# into them did -- the folds where the bucket is the predecessor and its members
# are the successors. Derived from the upstream map windows the crosswalk
# carries, never enumerated: area 276 reports to 2024 and bucket 206 to 2011, so
# 206 qualifies; area 62 (Ethiopia PDR) reports to 1992 and its bucket 238 to
# 2024, so 238 does not -- there the retired area folds into its live successor,
# which is a territorial identity and is never lifted.
.predecessor_bucket_codes <- function(crosswalk = NULL) {
  cw <- tibble::as_tibble(crosswalk %||% whep::polity_area_crosswalk)
  needed <- c("area_code", "polity_area_code", "fabio_code", "map_year_end")
  if (!all(rlang::has_name(cw, needed))) {
    return(integer(0L))
  }
  last <- cw |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$map_year_end)) |>
    dplyr::summarise(
      last_year = max(.data$map_year_end),
      .by = "area_code"
    ) |>
    dplyr::mutate(area_code = as.integer(.data$area_code))
  cw |>
    dplyr::filter(
      !is.na(.data$area_code),
      !is.na(.data$polity_area_code),
      .data$area_code != .data$polity_area_code,
      is.na(.data$fabio_code) | .data$fabio_code != 999L
    ) |>
    dplyr::distinct(
      area_code = as.integer(.data$area_code),
      polity_area_code = as.integer(.data$polity_area_code)
    ) |>
    dplyr::inner_join(last, by = "area_code") |>
    dplyr::inner_join(
      dplyr::rename(
        last,
        polity_area_code = "area_code",
        bucket_last_year = "last_year"
      ),
      by = "polity_area_code"
    ) |>
    dplyr::filter(.data$last_year > .data$bucket_last_year) |>
    dplyr::pull("polity_area_code") |>
    unique() |>
    sort()
}

# The ONE predicate deciding which members a mode promotes, shared by the
# crosswalk and the `regions_full` call sites for the same reason
# `.rest_of_world_members()` is: the fold is stated twice and a promotion that
# reaches only one of the two tables leaves the two lookups disagreeing (#419).
.predecessor_bucket_members <- function(areas, mode) {
  n <- length(areas$area_code)
  if (mode == "none") {
    return(rep(FALSE, n))
  }
  !is.na(areas$area_code) &
    !is.na(areas$polity_area_code) &
    areas$area_code != areas$polity_area_code &
    areas$polity_area_code %in% .predecessor_bucket_codes()
}

.unfold_predecessor_bucket <- function(crosswalk) {
  mode <- .predecessor_unfold_mode()
  promoted <- .predecessor_bucket_members(
    list(
      area_code = as.integer(crosswalk$area_code),
      polity_area_code = as.integer(crosswalk$polity_area_code)
    ),
    mode
  )
  if (!any(promoted)) {
    return(crosswalk)
  }
  # Warned on every read, exactly as the Rest-of-World unfold warns whenever it
  # is not in its published mode: a run whose numbers do not match the published
  # series must be impossible to mistake for one that does.
  areas <- sort(unique(as.integer(crosswalk$area_code[promoted])))
  buckets <- sort(unique(as.integer(crosswalk$polity_area_code[promoted])))
  # `qty()` pinned and the codes held in variables: a bare integer vector next
  # to a plural marker makes cli read the quantity off the codes and abort
  # inside its own message (#618, #621).
  cli::cli_warn(c(
    "!" = "{.code whep.unfold_predecessor_bucket} is set to {.val {mode}}:
           {cli::qty(length(areas))}reporting area{?s} {.val {areas}}
           {cli::qty(length(areas))}{?is/are} promoted out of
           {cli::qty(length(buckets))}predecessor bucket{?s} {.val {buckets}}.",
    "i" = "Published WHEP values keep that fold. This is the sensitivity
           setting for issue 680, not the production mode."
  ))
  crosswalk[which(promoted), polity_area_code := area_code]
  crosswalk
}

# `regions_full` states the fold a second time, keyed on `code`.
.unfold_predecessor_regions <- function(regions) {
  mode <- .predecessor_unfold_mode()
  promoted <- .predecessor_bucket_members(
    list(
      area_code = as.integer(regions$code),
      polity_area_code = as.integer(regions$polity_area_code)
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

# -- Area-vintage mismatch (whep#884) ------------------------------------------

# The years WHEP's own area vocabulary reports each `area_code`, taken from the
# upstream FAOSTAT map the crosswalk carries. One row per area, widest window
# across that area's polity periods, so an area whose polity changes mid-series
# keeps one window rather than one per period.
.area_reporting_windows <- function() {
  empty <- data.table::data.table(
    area_code = integer(0),
    window_start = integer(0),
    window_end = integer(0)
  )
  crosswalk <- .polity_crosswalk(include_unmapped = FALSE)
  needed <- c("area_code", "map_year_start", "map_year_end")
  if (!all(needed %in% names(crosswalk))) {
    return(empty)
  }
  crosswalk[
    !is.na(area_code) & !is.na(map_year_start) & !is.na(map_year_end),
    .(
      window_start = min(map_year_start),
      window_end = max(map_year_end)
    ),
    by = .(area_code = as.integer(area_code))
  ]
}

# `(area_code, year)` pairs a source keys to an area that reports NO territory
# in that year -- a vintage mismatch between the source's area codes and WHEP's.
#
# A fold is not a mismatch, and that is the whole difficulty. FAOSTAT reports
# areas 276/277 from 2012 and WHEP sums them into bucket 206, whose own window
# ends 2011; it reports 62 (Ethiopia PDR) until 1992 and sums it into bucket 238,
# whose window starts 1993. Both look off-window on the bucket's own window while
# nothing is wrong, so the test is not "is the year inside this area's window"
# but "does ANY reporting area land on this bucket that year" -- which is false
# only when the bucket carries a territory nothing reports under that code then.
# `folded_reporting_areas()` is the report for the folds themselves.
#
# The shape this catches is the opposite one: FishStat reports Belgium as area
# 255 from 1976, while every FAOSTAT product reports that territory as
# Belgium-Luxembourg (15) until 1999 and only splits 255/256 at 2000. Nothing
# reports bucket 255 before 2000, so those 459 rows (6,948.4 kt) can never join
# a CBS keyed 15, and creating the 255 row instead would put two overlapping
# reporting areas on one territory-year (whep#884).
#
# Measured over every CBS-relevant pin, FishStat's pre-2000 Belgium is the only
# off-window area-year in any of them.
#
# `(polity_area_code, year)` pairs some reporting area actually reports, i.e.
# the buckets that carry a territory that year. Built from the areas whose own
# reporting window contains the year, resolved through the same year-aware
# helper the builds use, so a fold counts as reported in the years its members
# report -- bucket 238 is reported in 1961-1992 by area 62 and in 1993-2024 by
# area 238 itself.
.reported_bucket_years <- function(years) {
  windows <- .area_reporting_windows()
  grid <- data.table::CJ(
    area_code = windows$area_code,
    year = as.integer(years)
  )
  grid <- merge(grid, windows, by = "area_code")
  grid <- grid[year >= window_start & year <= window_end]
  resolved <- .add_polity_columns_dt(
    grid,
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  )
  unique(resolved[
    !is.na(polity_area_code),
    .(area_code = as.integer(polity_area_code), year = as.integer(year))
  ])
}

.off_window_area_keys <- function(dt) {
  empty <- data.table::data.table(
    area_code = integer(0),
    year = integer(0),
    rows = integer(0),
    window_start = integer(0),
    window_end = integer(0)
  )
  if (!all(c("area_code", "year") %in% names(dt)) || nrow(dt) == 0L) {
    return(empty)
  }
  observed <- data.table::as.data.table(dt)[
    !is.na(area_code) & !is.na(year),
    .(rows = .N),
    by = .(area_code = as.integer(area_code), year = as.integer(year))
  ]
  windows <- .area_reporting_windows()
  if (nrow(windows) == 0L) {
    return(empty)
  }
  off <- merge(observed, windows, by = "area_code")
  off <- off[year < window_start | year > window_end]
  if (nrow(off) == 0L) {
    return(empty)
  }
  # Cheap because it is asked only about the years a candidate row is in.
  off[
    !.reported_bucket_years(sort(unique(off$year))),
    on = c("area_code", "year")
  ]
}

# One row per off-window area, for a message.
.off_window_area_years <- function(dt) {
  off <- .off_window_area_keys(dt)
  if (nrow(off) == 0L) {
    return(off[, .(
      area_code,
      window_start,
      window_end,
      year_min = year,
      year_max = year,
      years = rows,
      rows
    )])
  }
  off[,
    .(
      year_min = min(year),
      year_max = max(year),
      years = data.table::uniqueN(year),
      rows = sum(rows)
    ),
    by = .(area_code, window_start, window_end)
  ][order(-rows)]
}

# Warn rather than inform, for the same reason `.warn_folded_areas()` does: the
# rows are silently unjoinable downstream, and the mismatch is reportable at the
# point the codes are read.
.warn_off_window_area_years <- function(dt, source_label = NULL) {
  if (!isTRUE(getOption("whep.warn_area_vintage", TRUE))) {
    return(invisible(NULL))
  }
  off <- .off_window_area_years(dt)
  if (nrow(off) == 0L) {
    return(invisible(off))
  }
  where <- if (is.null(source_label)) "this source" else source_label
  # One bullet per area, not a cli-truncated vector: naming them all IS the fix.
  bullets <- stats::setNames(
    sprintf(
      "Area %d reports %d-%d, but %s has %d-%d (%d row%s)",
      off$area_code,
      off$window_start,
      off$window_end,
      where,
      off$year_min,
      off$year_max,
      off$rows,
      ifelse(off$rows == 1L, "", "s")
    ),
    rep("*", nrow(off))
  )
  n_off <- nrow(off)
  cli::cli_warn(c(
    "!" = "{n_off} reporting area{?s} in {where} carr{?ies/y} rows outside the
           years WHEP's area vocabulary reports {cli::qty(n_off)}{?it/them}.",
    bullets,
    "i" = "The territory reports under a different {.field area_code} in those
           years, so these rows cannot join a CBS keyed on it, and creating
           that key would duplicate the territory (whep#884).",
    "i" = "Silence with {.code options(whep.warn_area_vintage = FALSE)}."
  ))
  invisible(off)
}

# A row must never be CREATED for an area-year the vocabulary does not report:
# that IS the duplicated territory, not a warning about one. Callers that create
# CBS rows from a trade record drop these first, so reaching here means a
# restriction was lifted without the filter (whep#884).
.abort_if_off_window_areas <- function(dt, what = "row") {
  off <- .off_window_area_years(dt)
  if (nrow(off) == 0L) {
    return(invisible(dt))
  }
  areas <- paste(off$area_code, collapse = ", ")
  n_rows <- sum(off$rows)
  cli::cli_abort(
    c(
      "{n_rows} {what}{?s} would be created for {nrow(off)} area-year
       {cli::qty(nrow(off))}bucket{?s} the area vocabulary does not report.",
      "x" = "Area{?s} {areas}.",
      "i" = "The territory already reports under another {.field area_code} in
             those years, so the created rows would duplicate it (whep#884)."
    ),
    class = "whep_error_off_window_area_year"
  )
}
