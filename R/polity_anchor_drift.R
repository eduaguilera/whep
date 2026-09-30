#' Find back-cast rows whose polity label and growth proxy describe different
#' territories
#'
#' @description
#' A pre-1962 WHEP row is a reconstruction with **two territorial references**
#' and they need not agree (whep#748):
#'
#' * its **level** is the area's reported value at the back-cast anchor, walked
#'   backwards by [fill_proxy_growth()], so it describes the territory that
#'   area had in `backcast_anchor`. That is the territory
#'   `reporting_polity_code` names, because [add_polity_code()] floors the
#'   polity lookup at the same anchor;
#' * its **year-on-year movement** is a ratio of LUH2 land, and under
#'   `build_primary_production(land_method = "present_day")` the `luh2-areas`
#'   pin is keyed on **present-day ISO3**, so the movement describes the
#'   territory that area has today.
#'
#' Wherever a territory changed after the anchor those are different polygons.
#' Nothing in a built table says so: [polity_coverage_gaps()] answers the
#' neighbouring question "was this polity live in the row's year", which is
#' true of far more rows and says nothing about whether the label's extent is
#' the extent the value was computed on. This reports the disagreement itself,
#' for a table that has already been built, so it can be sized without
#' re-deriving the crosswalk.
#'
#' No row is an error and none is dropped from any build. `drift_kind` says
#' what kind of disagreement a pair carries:
#'
#' - `"entity"`: the anchor polity and the reference polity are different
#'   entities, e.g. area 181 is labelled `SRH-1953-1964` (Southern Rhodesia)
#'   while its LUH2 movement is Zimbabwe's. The label can then overlap a
#'   sibling area's own published rows.
#' - `"interval"`: the same entity in two vintages, e.g. area 238 is labelled
#'   `ETH-1952-1993`, which includes Eritrea, while its movement is
#'   `ETH-1993-2025`, which does not. The entity name matches and the polygon
#'   does not, which is the harder class to notice.
#' - `"unmapped_reference"`: the area resolves to no polity at
#'   `reference_year` at all, so what the movement describes cannot be named.
#'
#' A pair whose anchor polity is itself `NA` is **not** reported: that row has
#' no label to disagree with, and it is [polity_mapping_provenance()] and
#' [polity_coverage_gaps()] that account for it.
#'
#' `data_year_polity_code` is reported alongside, resolved unfloored, because
#' it is the third reference in play: `build_primary_production(land_method =
#' "historical_polity")` measures the `ha` half inside that polity's polygon
#' (see [build_historical_land_areas()]), so under that method the movement
#' describes `data_year_polity_code` rather than `reference_polity_code`.
#'
#' @section Only the back-cast era is reported:
#' A row at or after `backcast_anchor` carries a value its own year's territory
#' reported, resolved at its own year, so its label and its level agree by
#' construction and it is not a disagreement this function has anything to say
#' about. Such rows are filtered out, which is why a `table` holding no
#' pre-anchor year returns zero rows. A missing year column aborts rather than
#' returning zero rows for the same reason: an empty answer must mean "no
#' drift", never "nothing was supplied to look at".
#'
#' @param table A data frame carrying an area-code column and a year column.
#' @param code_column Name of the column holding numeric area codes. The
#'   column may hold either a FAOSTAT `area_code` or the `polity_area_code`
#'   bucket that published outputs are keyed by; both resolve through the same
#'   lookup.
#' @param year_column Name of the column holding years. Required: the whole
#'   question is year-dependent.
#' @param backcast_anchor First year of reported (non-back-cast) FAOSTAT data;
#'   passed to the same resolution [add_polity_code()] documents, and the year
#'   the level is anchored at.
#' @param reference_year The year whose territory the growth proxy describes.
#'   Defaults to `2023L`, [build_primary_production()]'s default `end_year`,
#'   i.e. the last year the published artifact covers, which is the vintage the
#'   present-day ISO3 keying of the `luh2-areas` pin resolves to. Measured on
#'   the shipped crosswalk the answer is identical for every reference year
#'   from 2014 to 2024, so the default is not load-bearing.
#'
#' @returns A tibble with one row per drifting `(area_code, year)`, ordered by
#'   area code and year, carrying `area_code`, `year`, `anchor_polity_code`,
#'   `anchor_polity_name`, `data_year_polity_code`, `reference_polity_code`,
#'   `reference_polity_name`, `drift_kind` and `n_rows`, the number of rows of
#'   `table` that pair carries. Zero rows means every pre-anchor row's label
#'   describes the same territory its growth proxy does.
#'
#' @seealso [polity_coverage_gaps()] for rows attributed to a polity not live
#'   in their year, [polity_mapping_provenance()] for which authority a row's
#'   identity rests on, and [polity_bucket_coverage()] for buckets that sum
#'   more than one territory.
#' @export
#' @examples
#' # Area 238 Ethiopia is the documented case: the 1850 row is labelled
#' # `ETH-1952-1993`, which includes Eritrea, while its LUH2 growth proxy is
#' # keyed on present-day `ETH`, which does not.
#' polity_anchor_drift(
#'   tibble::tibble(
#'     area_code = c(238L, 238L, 11L),
#'     year = c(1850L, 2000L, 1850L),
#'     value = 1
#'   )
#' )
polity_anchor_drift <- function(
  table,
  code_column = "area_code",
  year_column = "year",
  backcast_anchor = 1961L,
  reference_year = 2023L
) {
  pairs <- .anchor_drift_pairs(table, code_column, year_column, backcast_anchor)
  if (nrow(pairs) == 0L) {
    return(.anchor_drift_empty())
  }
  anchored <- .anchor_drift_resolve(pairs, backcast_anchor)
  data_year <- .anchor_drift_resolve(pairs, -Inf)
  reference <- .anchor_drift_reference(pairs, reference_year)

  tibble::tibble(
    area_code = pairs$area_code,
    year = pairs$year,
    anchor_polity_code = anchored$polity_code,
    anchor_polity_name = anchored$polity_name,
    data_year_polity_code = data_year$polity_code,
    reference_polity_code = reference$polity_code,
    reference_polity_name = reference$polity_name,
    n_rows = pairs$n_rows
  ) |>
    dplyr::filter(
      !is.na(.data$anchor_polity_code),
      !.anchor_drift_same(
        .data$anchor_polity_code,
        .data$reference_polity_code
      )
    ) |>
    dplyr::mutate(
      drift_kind = .anchor_drift_kind(
        .data$anchor_polity_code,
        .data$reference_polity_code
      ),
      .before = "n_rows"
    ) |>
    dplyr::arrange(.data$area_code, .data$year)
}

# ---- helpers ----------------------------------------------------------------

# The distinct pre-anchor `(area_code, year)` pairs of `table`, with the number
# of rows each carries.
#
# The year column is required rather than optional. Resolving without a year
# would give every pair the current mapping, which agrees with itself by
# construction: the function would return zero rows and read as "no
# disagreement" for a table that was never asked the question.
.anchor_drift_pairs <- function(
  table,
  code_column,
  year_column,
  backcast_anchor
) {
  if (!rlang::has_name(table, code_column)) {
    cli::cli_abort(
      "Column {.field {code_column}} is required for {.arg table}.",
      class = "whep_anchor_drift_no_code"
    )
  }
  if (is.null(year_column) || !rlang::has_name(table, year_column)) {
    named <- if (is.null(year_column)) "year" else year_column
    cli::cli_abort(
      c(
        "Column {.field {named}} is required for {.arg table}.",
        x = "The disagreement this reports exists only before
             {.arg backcast_anchor}, so it cannot be read off a table with no
             year.",
        i = "Resolving without a year would return zero rows, which reads as
             {.q no drift} rather than as {.q nothing was supplied}."
      ),
      class = "whep_anchor_drift_no_year"
    )
  }
  tibble::as_tibble(table) |>
    dplyr::transmute(
      area_code = as.integer(.data[[code_column]]),
      year = as.integer(.data[[year_column]])
    ) |>
    dplyr::filter(!is.na(.data$year), .data$year < backcast_anchor) |>
    dplyr::summarise(n_rows = dplyr::n(), .by = c("area_code", "year")) |>
    dplyr::arrange(.data$area_code, .data$year)
}

# The polity each pair resolves to, in `pairs` order and one row per pair.
.anchor_drift_resolve <- function(pairs, backcast_anchor) {
  pairs |>
    dplyr::select("area_code", "year") |>
    data.table::as.data.table() |>
    .add_polity_columns_dt(
      code_col = "area_code",
      year_col = "year",
      include_unmapped = TRUE,
      backcast_anchor = backcast_anchor
    ) |>
    tibble::as_tibble() |>
    dplyr::select("polity_code", "polity_name")
}

# The polity each pair's area resolves to at `reference_year`, i.e. the
# territory the present-day ISO3 keying of the growth proxy describes.
#
# The same pairs are re-resolved with their year replaced, rather than resolved
# once per area and joined back: that join would be keyed on `area_code` with
# no year, which is precisely the year-blind territorial key
# `.territorial_joins()` exists to catch, and the resolution is cheap enough
# that there is nothing to buy by writing one.
.anchor_drift_reference <- function(pairs, reference_year) {
  pairs |>
    dplyr::transmute(
      area_code = .data$area_code,
      year = as.integer(reference_year)
    ) |>
    .anchor_drift_resolve(-Inf)
}

# Two polity codes describe the same territory only when they are the same
# code. `NA` on either side is a disagreement that can be named rather than a
# match, so it is not folded into equality here.
.anchor_drift_same <- function(anchor, reference) {
  !is.na(reference) & anchor == reference
}

# The polity family a code belongs to: its identity with the `-start-end`
# interval suffix removed, the same stem `resolve_polity_lineage()` keys its
# sibling lookup on.
.anchor_drift_family <- function(code) {
  stringr::str_remove(code, "-[0-9]+-[0-9]+$")
}

.anchor_drift_kind <- function(anchor, reference) {
  dplyr::case_when(
    is.na(reference) ~ "unmapped_reference",
    .anchor_drift_family(anchor) == .anchor_drift_family(reference) ~
      "interval",
    .default = "entity"
  )
}

.anchor_drift_empty <- function() {
  tibble::tibble(
    area_code = integer(0),
    year = integer(0),
    anchor_polity_code = character(0),
    anchor_polity_name = character(0),
    data_year_polity_code = character(0),
    reference_polity_code = character(0),
    reference_polity_name = character(0),
    drift_kind = character(0),
    n_rows = integer(0)
  )
}
