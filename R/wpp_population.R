# UN World Population Prospects population, by age and sex.
#
# Two consumers, one reader. build_protein_requirement() needs population BY AGE
# AND SEX, which the `gdp-population` pin does not carry at all. And that pin
# covers 190 of the 256 area codes the crosswalk resolves, so 16 areas with
# commodity-balance food have no denominator and vanish from every per-capita
# output (#644); WPP covers 14 of them individually.
#
# Mechanism. CLAUDE.md prefers a verified on-demand download over a pin for
# third-party data, because a pin adds an uncheckable second copy (#457). WPP
# fits that shape except in one respect: UN DESA publishes no checksum and no
# DOI for these CSVs, unlike the Zenodo records read_critical_n() and
# read_luh2_landuse() verify against. So the manifest below is WHEP-RECORDED,
# not publisher-published. It detects a silent upstream revision; it does not
# verify the file against the publisher. That is weaker than the rule intends
# and is stated here rather than glossed.

#' Read UN World Population Prospects population.
#'
#' @description
#' Reads the UN WPP 2024 medium-variant population by five-year age group and
#' sex, resolved to WHEP area codes. `by = "total"` sums to one row per country
#' and year; `by = "age_sex"` keeps the age and sex detail that
#' [build_protein_requirement()] needs.
#'
#' The file is resolved in order: the `dir` argument, then the `WHEP_WPP_DIR`
#' environment variable, then a cache under `rappdirs::user_cache_dir("whep")`,
#' downloading it on first use. Population is converted from the thousands WPP
#' publishes to persons.
#'
#' Only `Country/Area` locations with an ISO3 code are kept, so WPP's regional
#' and income-group aggregates cannot leak into a per-country denominator.
#'
#' @param years Optional integer vector of years to keep. `NULL` (default)
#'   keeps every year in the file.
#' @param by Output grain: `"total"` (default, one row per `year` and
#'   `area_code`) or `"age_sex"` (adds `age_start`, `age_span` and `sex`).
#' @param data Optional pre-read WPP table, bypassing the file entirely. Used by
#'   the tests so the whole path stays offline.
#' @param dir Optional directory holding `WPP2024_PopulationByAge5GroupSex_Medium.csv.gz`.
#' @return A tibble with `year`, `area_code`, `population` (persons) and, for
#'   `by = "age_sex"`, `age_start`, `age_span` and `sex` (`"m"` / `"f"`).
#' @export
#' @examples
#' read_wpp_population(
#'   by = "age_sex",
#'   data = tibble::tribble(
#'     ~ISO3_code, ~LocTypeName,   ~Time, ~AgeGrpStart, ~AgeGrpSpan,
#'     ~PopMale, ~PopFemale,
#'     "ESP",      "Country/Area", 2010L, 0L,           5L,
#'     1170.5,    1103.2
#'   )
#' )
read_wpp_population <- function(
  years = NULL,
  by = c("total", "age_sex"),
  data = NULL,
  dir = NULL
) {
  by <- rlang::arg_match(by)
  raw <- data %||% .wpp_read_file(dir)
  parsed <- .wpp_parse(raw, years)
  if (by == "total") {
    return(.wpp_totals(parsed))
  }
  parsed
}

# ---- Private helpers -------------------------------------------------------

.wpp_file_name <- function() {
  "WPP2024_PopulationByAge5GroupSex_Medium.csv.gz"
}

.wpp_url <- function() {
  paste0(
    "https://population.un.org/wpp/assets/Excel%20Files/",
    "1_Indicator%20(Standard)/CSV_FILES/",
    .wpp_file_name()
  )
}

# WHEP-recorded, not publisher-published. See the file header.
.wpp_bytes <- function() 29948947
.wpp_md5 <- function() "17913a00b69876fce197e2d0cea90447"

.wpp_cache_dir <- function() {
  file.path(rappdirs::user_cache_dir("whep"), "wpp")
}

.resolve_wpp_dir <- function(dir = NULL) {
  resolved <- dir %||% Sys.getenv("WHEP_WPP_DIR")
  if (.has_path(resolved)) {
    return(resolved)
  }
  .wpp_cache_dir()
}

# Read the CSV, downloading it on first use. `download` is injected so the
# cache-hit and failure paths test without touching the network.
.wpp_read_file <- function(dir = NULL, download = .wpp_download) {
  resolved <- .resolve_wpp_dir(dir)
  path <- file.path(resolved, .wpp_file_name())
  if (!.wpp_manifest_ok(path)) {
    path <- download(resolved)
  }
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

.wpp_manifest_ok <- function(path) {
  file.exists(path) &&
    identical(
      as.numeric(unname(file.info(path)$size)),
      as.numeric(.wpp_bytes())
    ) &&
    identical(unname(tools::md5sum(path)), .wpp_md5())
}

.wpp_download <- function(dir, fetch = .wpp_fetch) {
  path <- file.path(dir, .wpp_file_name())
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_alert_info("Downloading UN WPP 2024 population (29 MB)...")
  fetch(.wpp_url(), path)
  if (!.wpp_manifest_ok(path)) {
    unlink(path)
    # Bind before interpolating: a `{}` expression starting with a dot is read
    # as a cli style, not as an expression, in cli >= 3.4.0 (#621).
    expected_bytes <- .wpp_bytes()
    expected_md5 <- .wpp_md5()
    cli::cli_abort(c(
      "The downloaded WPP file does not match WHEP's recorded manifest.",
      i = "Expected {.val {expected_bytes}} bytes, MD5 {.val {expected_md5}}.",
      i = "UN DESA publishes no checksum, so this manifest is WHEP's own: a
           mismatch means the upstream file was revised, not necessarily that
           the download failed.",
      i = "Set {.envvar WHEP_WPP_DIR} to a directory holding a known-good copy
           to bypass the download."
    ))
  }
  path
}

.wpp_fetch <- function(url, path) {
  old <- options(timeout = max(600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  utils::download.file(url, path, mode = "wb", quiet = TRUE)
  invisible(path)
}

# WPP publishes population in thousands, male and female in separate columns,
# and mixes country rows with regional and income-group aggregates. Keep only
# Country/Area rows carrying an ISO3 code, then pivot the two sex columns long.
.wpp_parse <- function(raw, years) {
  needed <- c(
    "ISO3_code",
    "LocTypeName",
    "Time",
    "AgeGrpStart",
    "AgeGrpSpan",
    "PopMale",
    "PopFemale"
  )
  .check_columns(raw, needed, "the WPP table")
  out <- tibble::as_tibble(raw) |>
    dplyr::filter(
      .data$LocTypeName == "Country/Area",
      !is.na(.data$ISO3_code),
      .data$ISO3_code != ""
    ) |>
    dplyr::transmute(
      year = as.integer(.data$Time),
      iso3c = as.character(.data$ISO3_code),
      age_start = as.integer(.data$AgeGrpStart),
      # The open-ended top group carries a negative or missing span; it is one
      # year of age for weighting purposes, and the requirement is flat across
      # adult ages anyway.
      age_span = dplyr::if_else(
        is.na(as.integer(.data$AgeGrpSpan)) | as.integer(.data$AgeGrpSpan) < 1L,
        1L,
        as.integer(.data$AgeGrpSpan)
      ),
      male = as.numeric(.data$PopMale) * 1000,
      female = as.numeric(.data$PopFemale) * 1000
    )
  if (!is.null(years)) {
    out <- dplyr::filter(out, .data$year %in% years)
  }
  out |>
    tidyr::pivot_longer(
      c("male", "female"),
      names_to = "sex",
      values_to = "population"
    ) |>
    dplyr::mutate(
      sex = dplyr::if_else(.data$sex == "male", "m", "f"),
      area_code = .iso3c_to_area_code(.data$iso3c)
    ) |>
    dplyr::filter(is.finite(.data$population), .data$population > 0) |>
    dplyr::select(
      "year",
      "area_code",
      "iso3c",
      "age_start",
      "age_span",
      "sex",
      "population"
    )
}

.wpp_totals <- function(parsed) {
  parsed |>
    dplyr::summarise(
      population = sum(.data$population),
      .by = c("year", "area_code", "iso3c")
    )
}
