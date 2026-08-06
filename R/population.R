# National population on WHEP area codes, the denominator every per-capita
# quantity in the package needs (the SJOS-N nourishment axis via
# build_food_supply(), and build_n_percapita()).
#
# The `gdp-population` pin is keyed by ISO3 in a column named `area_code`,
# which is NOT the numeric area_code the rest of the package uses, and its
# `pop` column is in THOUSANDS. Both conversions were the reason the nourishment
# axis had never been run on real data (#450): every consumer wants
# (year, area_code, population) with a numeric code and persons.
#
# The pin also carries five regional residual aggregates (RAFR "Africa Other",
# RASI, REUR, RLAM, ROCE). They are not countries and have no numeric code, so
# they are dropped -- with a message naming them and their population, because a
# silent drop here is the difference between a world total and a countries-only
# total.
#
# The second regrouping is larger and is deliberate: `polity_area_code` is a
# bucket, not an identity, so several ISO3 codes share one code and are summed
# into one row. In the real pin that is Syria, North Macedonia, Palestine,
# Eswatini, Equatorial Guinea and French Guiana on 999 "Rest of World", plus
# Sudan and South Sudan on 206 "Sudan (former)" from 2012 on. Those buckets are
# what the commodity balances carry, so folding is what lets the per-capita
# join find a denominator at all -- but it means a row is not always one
# country, which is why `.pop_report_folded()` names it (#482).

#' Read national population on WHEP area codes.
#'
#' @description
#' Reads the `gdp-population` pin and returns population per country and year on
#' the numeric `area_code` the rest of the package uses. The pin is keyed by
#' ISO3 (in a column confusingly also called `area_code`) and reports population
#' in thousands; both are converted here, so consumers such as
#' [build_food_supply()] and [build_n_percapita()] get the
#' `year`/`area_code`/`population` contract they document.
#'
#' Regional residual aggregates in the pin (`RAFR`, `RASI`, `REUR`, `RLAM`,
#' `ROCE`) are not countries and carry no numeric code. They are dropped and
#' reported rather than silently discarded, since their omission is what makes
#' the result a countries-only total rather than a world total.
#'
#' `area_code` here is `polity_area_code`, which is a **bucket, not an
#' identity**: several ISO3 codes can share one code, and this function sums
#' them, so some rows are aggregates of more than one territory. With the real
#' pin, code 999 "Rest of World" carries Syria, North Macedonia, Palestine,
#' Eswatini, Equatorial Guinea and French Guiana, and code 206 "Sudan (former)"
#' carries Sudan plus South Sudan from 2012 on. The fold is required, not
#' accidental -- those are the codes the commodity balances are keyed on, so a
#' finer key would leave their food supply with no population denominator -- and
#' every folded row is named in a message. The polity columns say the same
#' thing: 999 resolves to `ROW-1850-2025` "Rest of World", and 206 to
#' `SUD-1956-2011`, the pre-secession territory its two members together cover.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the pin covers.
#' @param data Optional named list of pre-loaded inputs to avoid the pin read:
#'   `gdp_population` (the raw pin, with `Year`, `area_code` as ISO3, `pop` in
#'   thousands). Falls back to [whep_read_file()] when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with `year`, `area_code` and `population` (persons), one row
#'   per area code and year, sorted by year then area code, plus the polity
#'   columns below. A row is one country in the common case, but `area_code` is
#'   an aggregation bucket: rows on 999 ("Rest of World") and, from 2012,
#'   206 ("Sudan (former)") are sums over several territories rather than a
#'   single country.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' read_population(example = TRUE)
read_population <- function(years = NULL, data = list(), example = FALSE) {
  if (isTRUE(example)) {
    return(.example_population())
  }
  raw <- data$gdp_population %||% whep_read_file("gdp-population")
  .check_columns(raw, c("Year", "area_code", "pop"), "gdp_population")
  parsed <- .pop_parse(raw, years)
  .pop_report_unmapped(parsed)
  .pop_report_folded(parsed)
  parsed |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::summarise(
      population = sum(.data$population),
      .by = c("year", "area_code")
    ) |>
    dplyr::arrange(.data$year, .data$area_code) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# Parse the pin onto the package contract: ISO3 -> numeric area_code, thousands
# -> persons. Non-positive and missing populations are dropped; they are not a
# meaningful denominator and would produce Inf per-capita supply.
.pop_parse <- function(raw, years) {
  out <- tibble::as_tibble(raw) |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      iso3c = as.character(.data$area_code),
      population = as.numeric(.data$pop) * 1000
    ) |>
    dplyr::filter(
      !is.na(.data$year),
      is.finite(.data$population),
      .data$population > 0
    )
  if (!is.null(years)) {
    out <- dplyr::filter(out, .data$year %in% years)
  }
  dplyr::mutate(out, area_code = .iso3c_to_area_code(.data$iso3c))
}

# Name the ISO3 codes that carry population but no numeric area_code. These are
# the pin's regional residuals; anything else appearing here is a genuine
# crosswalk gap worth knowing about.
.pop_report_unmapped <- function(parsed) {
  unmapped <- dplyr::filter(parsed, is.na(.data$area_code))
  if (nrow(unmapped) == 0L) {
    return(invisible(NULL))
  }
  codes <- sort(unique(unmapped$iso3c))
  share <- sum(unmapped$population) / sum(parsed$population)
  cli::cli_inform(c(
    i = "Dropped {length(codes)} population code{?s} with no numeric
         {.field area_code}: {.val {codes}}.",
    i = "{round(100 * share, 2)}% of the population in range; the result is a
         countries-only total, not a world total."
  ))
  invisible(NULL)
}

# Name the `area_code` buckets that receive more than one ISO3 code, so that a
# reader is told which output rows are aggregates. This is a deliberate
# aggregation, not a crosswalk gap: `polity_area_code` is the key the commodity
# balances are keyed on, where 999 and (post-2011) 206 are single rows too, so a
# finer key would leave their food supply with no denominator. What would be
# wrong is leaving it unsaid, since `@return` otherwise reads as one country per
# row.
.pop_report_folded <- function(parsed) {
  cells <- .pop_folded_cells(parsed)
  if (nrow(cells) == 0L) {
    return(invisible(NULL))
  }
  buckets <- .pop_folded_buckets(cells)
  share <- sum(cells$population) / sum(parsed$population)
  cli::cli_inform(c(
    i = "Folded {sum(buckets$n_iso3)} ISO3 code{?s} into {nrow(buckets)}
         aggregate {.field area_code} row{?s}: {buckets$label}.",
    i = "{round(100 * share, 2)}% of the population in range sits on a row that
         sums several territories rather than one country."
  ))
  invisible(NULL)
}

# The rows the fold actually merges, counted per year: a bucket with a single
# member in a given year is that country's own row, so 206 only counts from
# 2012, when South Sudan joins Sudan in it.
.pop_folded_cells <- function(parsed) {
  parsed |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::mutate(
      n_iso3 = dplyr::n_distinct(.data$iso3c),
      .by = c("year", "area_code")
    ) |>
    dplyr::filter(.data$n_iso3 > 1L)
}

# One row per area code that the fold merges, naming its members over the whole
# range in hand, for the message.
.pop_folded_buckets <- function(cells) {
  cells |>
    dplyr::summarise(
      n_iso3 = dplyr::n_distinct(.data$iso3c),
      codes = paste(sort(unique(.data$iso3c)), collapse = " + "),
      .by = "area_code"
    ) |>
    dplyr::arrange(.data$area_code) |>
    dplyr::mutate(label = paste0(.data$area_code, " (", .data$codes, ")"))
}

# A real read_population() slice: the five largest countries in 2010, with the
# pin's thousands already converted to persons.
.example_population <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~population,
    2010L,
    41L,
    1348191400,
    2010L,
    100L,
    1240613600,
    2010L,
    231L,
    311182800,
    2010L,
    101L,
    244016200,
    2010L,
    21L,
    196353500
  ) |>
    .add_reporting_polity_columns()
}
