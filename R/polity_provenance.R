#' Report which authority a row's territorial identity rests on
#'
#' @description
#' [polity_area_crosswalk] is not the upstream FAOSTAT-to-polity map. It is that
#' map plus rows this package manufactures, and its `mapping_source` column says
#' which. Measured on the shipped snapshot, of 648 crosswalk rows:
#'
#' - `"upstream_map"` (245 rows): a row of `faostat_area_polity_map.csv` in
#'   `eduaguilera/whep-polities`. Upstream's statement about the territory.
#' - `"fabio_row_promoted"` (52): the same file, for the 47 areas the FABIO
#'   Rest-of-World fold used to shadow. Equally upstream's statement, and kept
#'   separate only because `.unfold_rest_of_world()` chooses between it and the
#'   fold row (whep#717). The two together consume the map's 297 rows exactly
#'   once.
#' - `"prefix_outside_map"` (263) and `"prefix_fallback"` (26): WHEP's own
#'   ISO3-prefix match, built in `data-raw/table_mappings.R`. No upstream
#'   authority. A prefix match can only ever produce an ISO3-family guess, so it
#'   cannot express the statements the pre-1961 era actually needs -- Turkey
#'   before 1913 is the Ottoman Empire, Pakistan before 1947 is British India --
#'   whose target stem differs from the source's (whep#740).
#' - `"fabio_row_fold"` (62): WHEP's own Rest-of-World bucket. Legitimately
#'   WHEP's to decide, but the territory still has no upstream row of its own.
#'   In the default mode only the 31 members upstream names nowhere resolve
#'   through one; see [row_promotion_status()].
#'
#' Counting crosswalk rows overstates the exposure, because most manufactured
#' rows are never picked. This reports the provenance of the **resolution**: for
#' each `(area_code, year)`, the class of the crosswalk row that
#' [add_polity_code()] actually selected, which is what a published value rests
#' on. Measured over the crosswalk's own 1850-2025 grid, 259 of the 263
#' `"prefix_outside_map"` rows are the resolution of no `(area_code, year)` at
#' all: the back-cast anchor floors every lookup at `backcast_anchor`, so the
#' pre-1961 era resolves through whatever answers the anchor year rather than
#' through the historical periods the prefix rule invented for it.
#'
#' The same measurement is what whep#717 moved: 10,912 of those `(area_code,
#' year)` resolutions used to rest on `"fabio_row_fold"`, and 5,456 of them --
#' every year of the 31 members upstream names -- now rest on the upstream map
#' instead.
#'
#' @section Authority:
#' `authority` collapses `mapping_source` to the question "who said so":
#'
#' - `"upstream"`: `"upstream_map"` or `"fabio_row_promoted"`.
#' - `"whep_prefix"`: `"prefix_outside_map"` or `"prefix_fallback"` -- a WHEP
#'   guess, and the population whep#740 asks to delete rather than replace.
#' - `"whep_bucket"`: `"fabio_row_fold"` -- WHEP's own documented bucket.
#' - `"unresolved"`: the area resolves to no polity, so nothing was said.
#'
#' An unrecognised `mapping_source` aborts rather than being folded into one of
#' these, so a new class of manufactured row cannot arrive already classified.
#'
#' @param table A data frame carrying an area-code column, and a year column if
#'   the resolution is to be year-aware. `NULL` (the default) reports over the
#'   crosswalk's own `(area_code, year)` grid instead, one row per pair, which
#'   is the provenance of the mapping rather than of a build.
#' @param code_column Name of the column holding numeric area codes.
#' @param year_column Name of the column holding years. Set to `NULL`, or leave
#'   it absent from `table`, to use the current/default mapping.
#' @param backcast_anchor First year of reported (non-back-cast) FAOSTAT data;
#'   passed to the same resolution [add_polity_code()] documents. Set to `-Inf`
#'   to resolve strictly by data year, which is what makes the manufactured
#'   pre-1961 periods live.
#'
#' @returns A tibble with one row per resolved `(area_code, year)`, ordered by
#'   area code and year, carrying `area_code`, `year`, `polity_code`,
#'   `mapping_source`, `authority`, `mapping_status` and `n_rows`, the number of
#'   rows of `table` that pair carries (always 1 when `table` is `NULL`).
#'
#' @seealso [add_polity_code()] for the resolution itself,
#'   [polity_coverage_gaps()] for the rows whose polity is a nearest-period
#'   stand-in, and [polity_bucket_coverage()] for the buckets that sum more than
#'   one territory.
#' @export
#' @examples
#' # Area 238 Ethiopia is the one reporting area whose published identity rests
#' # on a manufactured row: its pre-1993 years resolve to `ETH-1952-1993`, a
#' # period no upstream map row declares.
#' polity_mapping_provenance(
#'   tibble::tibble(area_code = 238L, year = c(1900L, 2000L), value = 1)
#' )
#'
#' # The headline is one summarise away.
#' polity_mapping_provenance(
#'   tibble::tibble(area_code = c(11L, 238L), year = 1990L)
#' ) |>
#'   dplyr::summarise(n_rows = sum(n_rows), .by = authority)
polity_mapping_provenance <- function(
  table = NULL,
  code_column = "area_code",
  year_column = "year",
  backcast_anchor = 1961L
) {
  keys <- .provenance_keys(table, code_column, year_column)
  resolved <- .add_polity_columns_dt(
    keys,
    code_col = "area_code",
    year_col = if (rlang::has_name(keys, "year")) "year" else NULL,
    include_unmapped = TRUE,
    backcast_anchor = backcast_anchor
  ) |>
    tibble::as_tibble()
  if (!rlang::has_name(resolved, "year")) {
    resolved <- dplyr::mutate(resolved, year = NA_integer_)
  }
  .attach_mapping_source(resolved) |>
    dplyr::mutate(authority = .mapping_authority(.data$mapping_source)) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      .by = c(
        "area_code",
        "year",
        "polity_code",
        "mapping_source",
        "authority",
        "mapping_status"
      )
    ) |>
    dplyr::arrange(.data$area_code, .data$year)
}

# ---- helpers ----------------------------------------------------------------

# The two key columns the resolution needs, renamed to the names the resolver
# uses, or the crosswalk's own grid when the caller has no table.
.provenance_keys <- function(table, code_column, year_column) {
  if (is.null(table)) {
    return(.provenance_grid())
  }
  if (!rlang::has_name(table, code_column)) {
    cli::cli_abort(
      "Column {.field {code_column}} is required for {.arg table}."
    )
  }
  year_col <- if (
    !is.null(year_column) && rlang::has_name(table, year_column)
  ) {
    year_column
  } else {
    NULL
  }
  tibble::as_tibble(table) |>
    dplyr::select(dplyr::all_of(c(code_column, year_col))) |>
    dplyr::rename_with(
      \(nm) c("area_code", "year")[match(nm, c(code_column, year_col))]
    )
}

# Every reporting area the crosswalk names, over every year WHEP publishes.
#
# The lower bound is the package's own first year rather than the crosswalk's,
# so the grid covers the back-cast era the manufactured periods were built for;
# the upper bound is the open-period sentinel, read from the data for the same
# reason `.reporting_era_years()` reads it (a literal stops covering the newest
# year the next time the snapshot moves).
.provenance_grid <- function() {
  crosswalk <- .polity_crosswalk(include_unmapped = TRUE)
  areas <- sort(unique(stats::na.omit(crosswalk$area_code)))
  years <- seq.int(
    .whep_first_year,
    max(as.integer(crosswalk$polity_end_year), na.rm = TRUE)
  )
  tidyr::expand_grid(
    area_code = as.integer(areas),
    year = as.integer(years)
  )
}

# The provenance of the row the resolver picked, read off that row.
#
# Keyed on the crosswalk row's own identity -- the area plus the polity PERIOD --
# rather than on the area alone, because an area carries several periods and
# they need not share a source. The key is unique in the shipped crosswalk
# (0 duplicate `(area_code, polity_code, polity_start_year, polity_end_year)`
# tuples), so this cannot multiply rows.
.attach_mapping_source <- function(resolved) {
  sources <- .polity_crosswalk(include_unmapped = TRUE) |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$polity_code)) |>
    dplyr::select(
      "area_code",
      "polity_code",
      "polity_start_year",
      "polity_end_year",
      "mapping_source"
    )
  resolved |>
    dplyr::left_join(
      sources,
      by = c(
        "area_code",
        "polity_code",
        "polity_start_year",
        "polity_end_year"
      )
    )
}

# Who said so, for each class of crosswalk row. Adding a class here is a
# statement that WHEP may manufacture rows of that kind, so a new
# `mapping_source` aborts instead of landing in whichever bucket looks nearest.
.mapping_source_authority <- function() {
  tibble::tribble(
    ~mapping_source,      ~authority,
    "upstream_map",       "upstream",
    "fabio_row_promoted", "upstream",
    "prefix_outside_map", "whep_prefix",
    "prefix_fallback",    "whep_prefix",
    "fabio_row_fold",     "whep_bucket"
  )
}

.mapping_authority <- function(mapping_source) {
  known <- .mapping_source_authority()
  unknown <- setdiff(
    unique(mapping_source[!is.na(mapping_source)]),
    known$mapping_source
  )
  if (length(unknown) > 0L) {
    # No cli pluralisation markers: `{?s}` needs a quantity in the bullet it
    # sits in, and the second bullet interpolates none, which aborts cli itself
    # with "Cannot pluralize without a quantity" instead of reporting the value.
    cli::cli_abort(c(
      "Unrecognised {.field mapping_source} in the crosswalk:
       {.val {unknown}}.",
      "i" = "Classify each one in {.fn .mapping_source_authority}. A new class
             of crosswalk row is a new statement about who decided the
             territory, not a detail."
    ))
  }
  out <- known$authority[match(mapping_source, known$mapping_source)]
  dplyr::coalesce(out, "unresolved")
}
