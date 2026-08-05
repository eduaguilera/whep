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
