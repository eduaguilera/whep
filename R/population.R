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
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the pin covers.
#' @param data Optional named list of pre-loaded inputs to avoid the pin read:
#'   `gdp_population` (the raw pin, with `Year`, `area_code` as ISO3, `pop` in
#'   thousands). Falls back to [whep_read_file()] when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with `year`, `area_code` and `population` (persons), one row
#'   per country-year, sorted by year then area code.
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
  parsed |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::summarise(
      population = sum(.data$population),
      .by = c("year", "area_code")
    ) |>
    dplyr::arrange(.data$year, .data$area_code)
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
  )
}
