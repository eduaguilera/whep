# Subnational agricultural statistics for the United States from the USDA
# NASS Quick Stats bulk dumps.
#
# CONFIRMED NASS FACTS (the 2026-09-02 dumps qs.crops_20260902.txt.gz and
# qs.animals_products_20260902.txt.gz were inspected directly; do not
# re-guess these):
# - The dumps are gzipped, tab-separated, CRLF-terminated, and carry 39
#   uppercase columns, SOURCE_DESC ... VALUE, CV_%. Both domains share the
#   identical column set.
# - No field is ever quoted (zero `"` bytes in 128,603 sampled rows), so the
#   reader disables quoting; a lone `"` would otherwise be read as opening a
#   quoted field and swallow every row up to the next one.
# - The CV_% column holds a literal NUL byte (0x00) on rows with no
#   coefficient of variation.
# - Those two facts together rule out `readr::read_tsv_chunked()`, which the
#   plan for this reader originally named: called with `quote = ""` on a
#   file containing NUL bytes, its legacy tokenizer SILENTLY DROPS rows --
#   88 of the 181-row test fixture survive instead of all 181, and the one
#   reported problem is a spurious "closing quote at end of file". Dropping
#   `quote = ""` reads all 181, at the price of reinstating the quote
#   hazard. So the streaming is done here instead: `readLines(skipNul =
#   TRUE)` takes the NUL bytes out of the stream a chunk at a time and each
#   chunk is parsed by `readr::read_tsv()`, whose vroom tokenizer honours
#   `quote = ""` correctly. `tests/testthat/test_admin_stats_nass.R` pins
#   the full fixture row count so a regression here cannot be silent.
# - VALUE carries thousands separators ("2,236,000") and, where a figure is
#   not published, a parenthesised code instead of a number.
# - The names change daily (the date stamp is the publication date), so the
#   dump on disk carries its own vintage and inst/scripts/download/
#   download_nass.R discovers the newest names from the listing page.
# - AGG_LEVEL_DESC separates the geographies: "STATE" (FIPS 01-56 plus 98
#   OTHER STATES), "COUNTY", "NATIONAL" (FIPS 99, STATE_NAME "US TOTAL"),
#   and several others this reader never returns. US TOTAL is therefore not
#   a state row that has to be dropped -- it is a different aggregation
#   level, and .nass_national_check() is what reads it.
# - The same SHORT_DESC appears under SOURCE_DESC "SURVEY" and "CENSUS",
#   under DOMAIN_DESC "TOTAL" and a dozen breakdown domains, and under
#   in-season forecast reference periods ("YEAR - AUG FORECAST"). All three
#   filters are load-bearing: without them one state-year returns several
#   rows that are not the annual survey estimate.
# - Licence: CC0 1.0 Universal (catalog.data.gov entry for the Quick Stats
#   agricultural database), so the dumps carry no redistribution condition.

#' Read United States state or county agricultural statistics from NASS
#'
#' @description
#' Reads one USDA NASS Quick Stats bulk dump from disk and returns the
#' requested series as source-native admin-statistics rows: one row per
#' reporting unit, item, indicator and year, with values converted to WHEP
#' units and every non-numeric NASS value code preserved.
#'
#' The dump is streamed a chunk of lines at a time and filtered chunk by
#' chunk, so the ~8 GB flat file is never materialised. Only the fifteen
#' columns this reader uses are parsed, and a gzipped dump is read without
#' being extracted first.
#'
#' Identifiers stay exactly as NASS published them: no item is mapped to a
#' WHEP `item_prod_code` and no unit is resolved to a polity. Both happen
#' downstream, from the source-native keys returned here.
#'
#' @section Series selection:
#' NASS keys a series on `SHORT_DESC`, which folds the commodity, its class,
#' the production practice and the statistic into one string
#' (`"CORN, GRAIN - ACRES HARVESTED"`). `short_desc` is therefore the item
#' selector, and its default covers the field crops and livestock species
#' the subnational spatialization needs. The authoritative item-to-series
#' table is built separately; pass `short_desc` to read anything else.
#'
#' Rows are kept only where `SOURCE_DESC` is `"SURVEY"` (not the
#' quinquennial census), `DOMAIN_DESC` is `"TOTAL"` (not a breakdown by
#' farm size, sales or NAICS class), and `PRODN_PRACTICE_DESC` is
#' `"ALL PRODUCTION PRACTICES"` (not the irrigated or organic split).
#'
#' @section Reference periods:
#' Crop area is annual (`FREQ_DESC == "ANNUAL"`) and its final estimate
#' carries `REFERENCE_PERIOD_DESC == "YEAR"`; the in-season forecasts
#' (`"YEAR - AUG FORECAST"` and siblings) are excluded by that default.
#' Livestock inventories are `FREQ_DESC == "POINT IN TIME"` and the default
#' reference period is `"FIRST OF JAN"`.
#'
#' One species does not follow that default: NASS publishes the annual hog
#' inventory as of **1 December**, so `"HOGS - INVENTORY"` has no
#' `"FIRST OF JAN"` rows at all and returns nothing under the default. That
#' is deliberately not patched here -- which hog reference period stands for
#' the annual inventory is a vocabulary decision, not a reading one -- but
#' it is never silent: any requested `short_desc` that matches no row warns
#' by name, and `reference_period` selects a different one.
#'
#' @param domain Which NASS dump to read, `"crops"` or `"animals"`.
#' @param nass_dir Directory holding the Quick Stats dumps, as
#'   `inst/scripts/download/download_nass.R` fills it. Defaults to
#'   `Sys.getenv("WHEP_NASS_DIR")`.
#' @param short_desc Character vector of NASS `SHORT_DESC` series to keep.
#'   `NULL` uses the documented default for `domain`.
#' @param reference_period Character vector of `REFERENCE_PERIOD_DESC`
#'   values to keep. `NULL` uses `"YEAR"` for crops and `"FIRST OF JAN"`
#'   for animals.
#' @param agg_level Aggregation level to return, `"STATE"` (grain
#'   `"admin1"`) or `"COUNTY"` (grain `"admin2"`). County rows are for
#'   within-unit validation, which the state constraint leaves untouched.
#' @param example If `TRUE`, return a small fixture instead of reading a
#'   dump. Defaults to `FALSE`.
#'
#' @return A tibble with one row per unit, item, indicator and year:
#'
#' - `source`: always `"USDA_NASS"`.
#' - `source_native_unit_id`, `source_native_unit_name`: the state FIPS code
#'   and `STATE_NAME` verbatim at `"STATE"` grain; the five-digit state plus
#'   county FIPS code and `"<state>, <county>"` at `"COUNTY"` grain. FIPS
#'   `"98"` is NASS's own residual unit, `"OTHER STATES"`, an observed value
#'   covering the states too few to publish separately.
#' - `source_native_item_code`: always `NA`; the dumps carry no item code.
#' - `source_native_item_name`: `SHORT_DESC` verbatim, the string that keys
#'   the series.
#' - `indicator_used`: `"area_harvested"`, `"area_planted_or_sown"`,
#'   `"production"` or `"yield"` for crop rows, `NA` for head counts.
#' - `quantity`: `"area"`, `"production"`, `"yield"` or `"heads"`.
#' - `year`: calendar year.
#' - `value`: the published value in WHEP units, `NA` where NASS published a
#'   code instead of a number.
#' - `value_unit`: `"ha"`, `"tonnes"` or `"heads"`.
#' - `value_flag`: `"residual"` on an OTHER STATES row, the NASS value code
#'   verbatim (`"(D)"`, `"(S)"`, `"(Z)"`, `"(NA)"`, `"(X)"`) on a row with
#'   no published number, both joined by `"; "` where both apply, and `NA`
#'   on a clean row.
#' - `grain`: `"admin1"` for state rows, `"admin2"` for county rows.
#' - `nuts_version`: always `NA`; the United States has no NUTS geography.
#' - `source_version`: the dump's own date stamp, `"YYYYMMDD"`, taken from
#'   its file name.
#' - `recorded_at`: ISO 8601 UTC time the dump was read.
#'
#' @source USDA National Agricultural Statistics Service, Quick Stats bulk
#'   downloads (<https://www.nass.usda.gov/datasets/>), files
#'   `qs.crops_YYYYMMDD.txt.gz` and `qs.animals_products_YYYYMMDD.txt.gz`.
#'   Licence CC0 1.0 Universal. Verified 2026-09-02.
#'
#' @export
#'
#' @examples
#' read_admin_stats_nass(example = TRUE)
read_admin_stats_nass <- function(
  domain = c("crops", "animals"),
  nass_dir = NULL,
  short_desc = NULL,
  reference_period = NULL,
  agg_level = c("STATE", "COUNTY"),
  example = FALSE
) {
  if (example) {
    return(.example_admin_stats_nass())
  }
  domain <- rlang::arg_match(domain)
  agg_level <- rlang::arg_match(agg_level)
  series <- short_desc %||% .nass_default_series(domain)
  period <- reference_period %||% .nass_default_period(domain)
  dump <- .fetch_nass_dump(domain, nass_dir)

  kept <- .nass_scan_dump(dump$path, series, agg_level) |>
    .nass_common_filters(series, period, .nass_domain_freq(domain)) |>
    .nass_warn_unmatched(series)
  .nass_assert_agg_level(kept, agg_level)

  .nass_convert(kept) |>
    .nass_shape_rows(dump$source_version, agg_level)
}

# --- Series, period and unit vocabularies --------------------------------

# The series the subnational spatialization needs, one SHORT_DESC each, all
# verified present in the 2026-09-02 dumps. Deliberately not a package
# dataset: the authoritative item-to-series table is built downstream, and
# duplicating it here would give two sources of truth.
.nass_default_series <- function(domain) {
  switch(
    domain,
    crops = c(
      "CORN, GRAIN - ACRES HARVESTED",
      "WHEAT - ACRES HARVESTED",
      "SOYBEANS - ACRES HARVESTED",
      "BARLEY - ACRES HARVESTED",
      "OATS - ACRES HARVESTED"
    ),
    animals = c(
      "CATTLE, INCL CALVES - INVENTORY",
      "CATTLE, COWS, MILK - INVENTORY",
      "HOGS - INVENTORY",
      "SHEEP, INCL LAMBS - INVENTORY"
    )
  )
}

.nass_default_period <- function(domain) {
  switch(domain, crops = "YEAR", animals = "FIRST OF JAN")
}

# FREQ_DESC is redundant with an exact reference period but guards against a
# reference-period label being reused at another frequency.
.nass_domain_freq <- function(domain) {
  switch(domain, crops = "ANNUAL", animals = "POINT IN TIME")
}

# NASS's statistic category, which is what says whether a row is an area, a
# production, a yield or a head count. The reader refuses anything else
# rather than guessing an indicator for it.
.nass_indicator_spec <- function() {
  tibble::tribble(
    ~statisticcat_desc, ~indicator_used,        ~quantity,
    "AREA HARVESTED",   "area_harvested",       "area",
    "AREA PLANTED",     "area_planted_or_sown", "area",
    "PRODUCTION",       "production",           "production",
    "YIELD",            "yield",                "yield",
    "INVENTORY",        NA_character_,          "heads"
  )
}

# Conversions to WHEP units. Only the exact, definition-based factors are
# listed; everything else NASS publishes (bushels, hundredweight, pounds,
# and every per-acre yield unit) needs a crop-specific density or milling
# factor, which belongs to the item vocabulary and not to a reader.
#
# 1 international acre = 4046.8564224 m2 exactly = 0.40468564224 ha, from
# the 1959 international yard and pound agreement (NIST Special Publication
# 811, appendix B.6). The 0.404686 often quoted is that value to six
# decimals.
# 1 short ton = 2000 lb and 1 lb = 0.45359237 kg exactly (same source), so
# 1 short ton = 0.90718474 tonnes.
.nass_unit_spec <- function() {
  tibble::tribble(
    ~unit_desc, ~value_unit, ~to_whep,
    "ACRES",    "ha",        0.40468564224,
    "HEAD",     "heads",     1,
    "TONS",     "tonnes",    0.90718474
  )
}

# --- Locating a dump -----------------------------------------------------

# The only file-system entry point; tests stub it.
.fetch_nass_dump <- function(domain, nass_dir) {
  dir <- .resolve_nass_dir(nass_dir)
  pattern <- .nass_dump_pattern(domain)
  files <- list.files(dir, pattern = pattern)
  if (length(files) == 0) {
    cli::cli_abort(c(
      "No NASS {.val {domain}} dump in {.file {dir}}.",
      i = "Expected a file matching {.val {pattern}}.",
      i = "Run {.file inst/scripts/download/download_nass.R} to fetch the
           newest one."
    ))
  }
  # The stamps are zero-padded YYYYMMDD, so the newest is the lexical
  # maximum. `max()` rather than `which.max()`, which only accepts a
  # character vector from R 4.4.0 and this package supports R 4.1.0.
  stamps <- .nass_dump_stamp(files)
  newest <- which(stamps == max(stamps))[[1]]
  list(
    path = file.path(dir, files[[newest]]),
    source_version = stamps[[newest]]
  )
}

.resolve_nass_dir <- function(nass_dir) {
  resolved <- nass_dir %||% Sys.getenv("WHEP_NASS_DIR")
  if (!.has_path(resolved) || !dir.exists(resolved)) {
    cli::cli_abort(c(
      "No NASS Quick Stats directory available.",
      i = "Pass {.arg nass_dir} or set {.envvar WHEP_NASS_DIR}.",
      i = "{.file inst/scripts/download/download_nass.R} fills it."
    ))
  }
  resolved
}

# The published names are qs.crops_YYYYMMDD.txt.gz and
# qs.animals_products_YYYYMMDD.txt.gz. An already-extracted .txt is accepted
# too, because readr reads either transparently.
.nass_dump_pattern <- function(domain) {
  stem <- switch(domain, crops = "crops", animals = "animals_products")
  paste0("^qs\\.", stem, "_[0-9]{8}\\.txt(\\.gz)?$")
}

.nass_dump_stamp <- function(files) {
  stringr::str_extract(files, "[0-9]{8}(?=\\.txt)")
}

# --- Streaming read and row selection ------------------------------------

# Lines per chunk. A dump row is ~350 bytes, so 250,000 lines is a working
# set of roughly 90 MB per chunk on a dump of ~35 million rows.
.nass_chunk_size <- function() {
  250000L
}

# Only the columns this reader uses are parsed; cols_only() drops the other
# 24, CV_% and its embedded NUL bytes among them. STATE_FIPS_CODE and
# COUNTY_CODE are character because their leading zeros are significant.
.nass_col_types <- function() {
  readr::cols_only(
    SOURCE_DESC = readr::col_character(),
    PRODN_PRACTICE_DESC = readr::col_character(),
    STATISTICCAT_DESC = readr::col_character(),
    UNIT_DESC = readr::col_character(),
    SHORT_DESC = readr::col_character(),
    DOMAIN_DESC = readr::col_character(),
    AGG_LEVEL_DESC = readr::col_character(),
    STATE_FIPS_CODE = readr::col_character(),
    STATE_NAME = readr::col_character(),
    COUNTY_CODE = readr::col_character(),
    COUNTY_NAME = readr::col_character(),
    YEAR = readr::col_integer(),
    FREQ_DESC = readr::col_character(),
    REFERENCE_PERIOD_DESC = readr::col_character(),
    VALUE = readr::col_character()
  )
}

# Discards each chunk down to the wanted series and geography before the
# next one is read, so peak memory is one chunk plus the kept rows. The
# loop is the streaming itself and cannot be vectorised: each pass has to
# finish before the connection yields the next lines. `gzfile()` reads a
# gzipped and a plain dump alike, and `skipNul` takes the CV_% NUL bytes
# out of the stream (see the file header for why that matters).
.nass_scan_dump <- function(path, series, agg_levels) {
  con <- gzfile(path, "rt")
  on.exit(close(con))
  header <- readLines(con, n = 1L, skipNul = TRUE)
  if (length(header) == 0) {
    cli::cli_abort("NASS dump {.file {path}} is empty.")
  }
  chunks <- list()
  repeat {
    lines <- readLines(con, n = .nass_chunk_size(), skipNul = TRUE)
    if (length(lines) == 0) {
      break
    }
    chunks[[length(chunks) + 1L]] <-
      .nass_parse_chunk(header, lines, series, agg_levels)
  }
  if (length(chunks) == 0) {
    return(.nass_parse_chunk(header, character(), series, agg_levels))
  }
  purrr::list_rbind(chunks)
}

# `series` and `agg_levels` are each either a vector to keep or `NULL`,
# which keeps everything -- the whole-file read the fixture test needs.
.nass_parse_chunk <- function(header, lines, series, agg_levels) {
  parsed <- readr::read_tsv(
    I(c(header, lines)),
    col_types = .nass_col_types(),
    quote = "",
    na = character(),
    progress = FALSE
  )
  if (!is.null(series)) {
    parsed <- dplyr::filter(parsed, SHORT_DESC %in% series)
  }
  if (!is.null(agg_levels)) {
    parsed <- dplyr::filter(parsed, AGG_LEVEL_DESC %in% agg_levels)
  }
  parsed
}

.nass_common_filters <- function(x, series, period, freq) {
  dplyr::filter(
    x,
    SHORT_DESC %in% series,
    SOURCE_DESC == "SURVEY",
    DOMAIN_DESC == "TOTAL",
    PRODN_PRACTICE_DESC == "ALL PRODUCTION PRACTICES",
    FREQ_DESC %in% freq,
    REFERENCE_PERIOD_DESC %in% period
  )
}

# A requested series that survives no row is the failure this reader most
# has to avoid making silent: NASS publishes the hog inventory on a
# December reference period, so the January default drops the species
# whole. Naming the series is what turns that into a visible outcome.
.nass_warn_unmatched <- function(x, series) {
  missing <- setdiff(series, unique(x$SHORT_DESC))
  if (length(missing) > 0) {
    cli::cli_warn(c(
      "{length(missing)} requested NASS series matched no row:
       {.val {missing}}.",
      i = "Check {.arg reference_period}; NASS publishes some inventories
           on a December reference date, not {.val FIRST OF JAN}."
    ))
  }
  x
}

# US TOTAL is a NATIONAL row, never a STATE one, so a FIPS 99 row arriving
# at state grain means the dump's aggregation semantics changed and the
# state sum would double-count the country.
.nass_assert_agg_level <- function(x, agg_level) {
  intruders <- x |>
    dplyr::filter(AGG_LEVEL_DESC == "STATE", STATE_FIPS_CODE == "99")
  if (nrow(intruders) > 0) {
    cli::cli_abort(
      "{nrow(intruders)} NASS row{?s} carry FIPS 99 (US TOTAL) at
       {.val STATE} aggregation level, where only states belong."
    )
  }
  invisible(x)
}

# --- Value conversion ----------------------------------------------------

.nass_convert <- function(x) {
  x |>
    .nass_add_indicator() |>
    .nass_add_unit() |>
    dplyr::mutate(
      value = .nass_parse_number(VALUE) * to_whep,
      value_flag = .nass_value_code(VALUE)
    )
}

.nass_add_indicator <- function(x) {
  spec <- .nass_indicator_spec()
  unknown <- setdiff(unique(x$STATISTICCAT_DESC), spec$statisticcat_desc)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "NASS statistic categor{?y/ies} {.val {unknown}} {?is/are} not mapped
       to a WHEP indicator.",
      i = "Mapped categories are {.val {spec$statisticcat_desc}}."
    ))
  }
  dplyr::left_join(
    x,
    spec,
    by = c(STATISTICCAT_DESC = "statisticcat_desc")
  )
}

.nass_add_unit <- function(x) {
  spec <- .nass_unit_spec()
  unknown <- setdiff(unique(x$UNIT_DESC), spec$unit_desc)
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "NASS unit{?s} {.val {unknown}} cannot be converted to a WHEP unit.",
      i = "This reader converts only {.val {spec$unit_desc}}.",
      i = "Bushels, hundredweight and per-acre yields need a crop-specific
           factor, which belongs to the item vocabulary, not to a reader."
    ))
  }
  dplyr::left_join(x, spec, by = c(UNIT_DESC = "unit_desc"))
}

# NASS writes thousands separators into VALUE and, where a figure is not
# published, a parenthesised code in its place. Anything that is not a
# number after the separators come out is kept verbatim as the flag, so a
# code this reader has not seen is preserved rather than silently lost.
.nass_parse_number <- function(x) {
  cleaned <- stringr::str_remove_all(stringr::str_trim(x), ",")
  suppressWarnings(as.numeric(cleaned))
}

.nass_value_code <- function(x) {
  trimmed <- stringr::str_trim(x)
  dplyr::if_else(
    is.na(.nass_parse_number(x)) & nzchar(trimmed),
    trimmed,
    NA_character_
  )
}

# --- Output shape --------------------------------------------------------

.nass_shape_rows <- function(x, source_version, agg_level) {
  n <- nrow(x)
  tibble::tibble(
    source = rep("USDA_NASS", n),
    source_native_unit_id = .nass_unit_id(x, agg_level),
    source_native_unit_name = .nass_unit_name(x, agg_level),
    source_native_item_code = rep(NA_character_, n),
    source_native_item_name = x$SHORT_DESC,
    indicator_used = x$indicator_used,
    quantity = x$quantity,
    year = x$YEAR,
    value = x$value,
    value_unit = x$value_unit,
    value_flag = .nass_row_flag(x),
    grain = rep(.nass_grain(agg_level), n),
    nuts_version = rep(NA_character_, n),
    source_version = rep(source_version, n),
    recorded_at = rep(.format_evidence_stamp(Sys.time()), n)
  ) |>
    dplyr::arrange(
      .data$year,
      .data$source_native_item_name,
      .data$source_native_unit_id
    )
}

.nass_grain <- function(agg_level) {
  switch(agg_level, STATE = "admin1", COUNTY = "admin2")
}

# A county is identified by the five-digit state-plus-county FIPS code, the
# state code alone being ambiguous across states.
.nass_unit_id <- function(x, agg_level) {
  if (agg_level == "STATE") {
    return(x$STATE_FIPS_CODE)
  }
  paste0(x$STATE_FIPS_CODE, x$COUNTY_CODE)
}

.nass_unit_name <- function(x, agg_level) {
  if (agg_level == "STATE") {
    return(x$STATE_NAME)
  }
  paste0(x$STATE_NAME, ", ", x$COUNTY_NAME)
}

# NASS's OTHER STATES row is a residual with an observed value: it holds the
# states too few to publish separately. It is kept, and marked, because it
# carries mass the reporting units do not.
.nass_row_flag <- function(x) {
  residual <- dplyr::if_else(
    x$STATE_FIPS_CODE == "98",
    "residual",
    NA_character_
  )
  purrr::map2_chr(residual, x$value_flag, .nass_join_flags)
}

.nass_join_flags <- function(residual, code) {
  flags <- c(residual, code)
  flags <- flags[!is.na(flags)]
  if (length(flags) == 0) {
    return(NA_character_)
  }
  stringr::str_c(flags, collapse = "; ")
}

# --- In-file national check ----------------------------------------------

# The dump publishes its own US TOTAL at AGG_LEVEL_DESC "NATIONAL", so the
# state rows can be checked against the country without leaving the file.
# The result is a diagnostic, not a correction: a ratio well below one means
# the states NASS publishes separately do not cover the national total, and
# the OTHER STATES residual is where the rest sits.
.nass_national_check <- function(
  domain = c("crops", "animals"),
  nass_dir = NULL,
  short_desc = NULL,
  reference_period = NULL
) {
  domain <- rlang::arg_match(domain)
  series <- short_desc %||% .nass_default_series(domain)
  period <- reference_period %||% .nass_default_period(domain)
  dump <- .fetch_nass_dump(domain, nass_dir)

  converted <- .nass_scan_dump(dump$path, series, c("STATE", "NATIONAL")) |>
    .nass_common_filters(series, period, .nass_domain_freq(domain)) |>
    .nass_convert()

  dplyr::full_join(
    .nass_national_side(converted),
    .nass_state_side(converted),
    by = c("source_native_item_name", "year", "quantity", "value_unit")
  ) |>
    dplyr::mutate(ratio = state_sum / national_value) |>
    dplyr::arrange(.data$source_native_item_name, .data$year)
}

.nass_national_side <- function(x) {
  x |>
    dplyr::filter(AGG_LEVEL_DESC == "NATIONAL") |>
    dplyr::summarise(
      national_value = sum(.data$value, na.rm = TRUE),
      n_national = dplyr::n(),
      .by = c("SHORT_DESC", "YEAR", "quantity", "value_unit")
    ) |>
    dplyr::rename(source_native_item_name = "SHORT_DESC", year = "YEAR")
}

.nass_state_side <- function(x) {
  x |>
    dplyr::filter(AGG_LEVEL_DESC == "STATE") |>
    dplyr::summarise(
      state_sum = sum(.data$value, na.rm = TRUE),
      n_states = dplyr::n(),
      n_suppressed = sum(is.na(.data$value)),
      n_residual = sum(.data$STATE_FIPS_CODE == "98"),
      .by = c("SHORT_DESC", "YEAR", "quantity", "value_unit")
    ) |>
    dplyr::rename(source_native_item_name = "SHORT_DESC", year = "YEAR")
}
