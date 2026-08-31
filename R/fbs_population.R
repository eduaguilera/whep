# National population as FAOSTAT itself publishes it, read from the Food
# Balance Sheet pins WHEP already reads for the food NUMERATOR.
#
# WHY A THIRD SOURCE EXISTS AT ALL. The two population sources WHEP had are both
# keyed on a PRESENT-DAY ISO3 code: the `gdp-population` pin has an ISO3 column,
# and UN WPP publishes `Country/Area` rows carrying one. A WHEP area code names
# a territory in the years it reported, so a dissolved federation has no key in
# either -- that is what `R/population_reach.R` measures and what leaves area
# 186 Serbia and Montenegro (1992-2005) and area 151 Netherlands Antilles
# (1961-2010) with food and no denominator (#862, #787).
#
# The FAOSTAT Food Balance Sheets are keyed on the FAOSTAT AREA CODE instead,
# which is the same key space the commodity balances are built from, and FAOSTAT
# keeps a dissolved area alive for the years it reported. So item 2501
# "Population", element 511 "Total Population - Both sexes", carries area 186 for
# exactly 1992-2005 and area 151 for exactly 1961-2010: no successor walk, no
# ISO3 vocabulary, no second copy of anything -- these are the same two pins
# `build_commodity_balances()` already reads.
#
# CONFIRMED FACTS about the two pins (inspected; do not re-guess):
# - `faostat-fbs-old` carries item 2501 / element 511 for 217 areas, 1961-2013,
#   in "1000 persons"; `faostat-fbs-new` for 213 areas, 2010-2023, in "1000 No".
#   Both are thousands, so both are multiplied by 1000 here.
# - FAOSTAT's regional and grouping aggregates (`World`, `Africa`,
#   `European Union`, `Least Developed Countries`, ...) all carry area codes
#   >= 5000 and resolve to no polity, so they drop out with the rest of the
#   package's area resolution rather than needing their own filter. Measured on
#   the real pins, every unresolved area is one of those 42 aggregates.
# - Bucketed onto `polity_area_code` the year-aware way, NO bucket-year receives
#   more than one FAOSTAT area, so nothing is double counted: 62 Ethiopia PDR
#   (to 1992) and 238 Ethiopia (from 1993) both land on 238 but never in the
#   same year, and 276 Sudan (2012-2013) lands on 206 in years 206 itself does
#   not report. South Sudan (277) has no population row in either pin at all,
#   so bucket 206 from 2012 is Sudan-north alone here, unlike the
#   `gdp-population` pin, which sums `SDN + SSD` onto it.
# - The two vintages disagree, sometimes by a lot, where they overlap: FAOSTAT
#   area 272 Serbia in 2010 is 9,647,000 in `faostat-fbs-old` and 7,395,860 in
#   `faostat-fbs-new`, because the old vintage's Serbia includes Kosovo and the
#   new one does not. `faostat-fbs-new` therefore wins an overlapping
#   (year, area), which is the same order `.cbs_source_rank()` gives the two
#   pins for the numerator.
#
# THIS BUILDS NO DEFAULT. `read_population()` still defaults to the
# `gdp-population` pin. This reader is the fill for
# `population_source = "pin_wpp_fbs_fallback"`, and which denominator a
# dissolved federation SHOULD get is a science decision the maintainer has not
# made: for area 186 in 2000 FAOSTAT says 10,801,000 while a UN WPP 2024
# territorial sum (`SRB + MNE + XKX`) says 10,104,000, 6.5% apart, and the
# `SRB + MNE` sum the successor walk can actually reach today says 8,311,000,
# 23% apart (#863). See `read_population()`.

#' Read FAOSTAT Food Balance Sheet population on WHEP area codes.
#'
#' @description
#' Reads item 2501 "Population", element 511 "Total Population - Both sexes",
#' from the `faostat-fbs-old` and `faostat-fbs-new` pins — the same two pins
#' [build_commodity_balances()] reads for the food itself — and returns
#' population per area and year on the numeric `area_code` the rest of the
#' package uses. FAOSTAT publishes it in thousands; it is converted to persons
#' here.
#'
#' This is the one population source WHEP has that is **not** keyed on a
#' present-day ISO3 code. It is keyed on the FAOSTAT area code, the same key
#' space the commodity balances are built from, and FAOSTAT keeps a dissolved
#' reporting area alive for the years it reported. So it covers the territories
#' [read_population()] and [read_wpp_population()] structurally cannot: area 186
#' Serbia and Montenegro for exactly 1992–2005 and area 151 Netherlands
#' Antilles for exactly 1961–2010, the two largest holes
#' [population_source_reach()] reports (#862, #787).
#'
#' The two pins overlap over 2010–2013 and disagree there, sometimes sharply:
#' FAOSTAT area 272 Serbia in 2010 is 9,647,000 in `faostat-fbs-old` against
#' 7,395,860 in `faostat-fbs-new`, because the old vintage's Serbia includes
#' Kosovo and the new one does not. `faostat-fbs-new` wins an overlapping
#' `(year, area_code)`, which is the order `build_commodity_balances()` gives
#' the same two pins for the numerator.
#'
#' FAOSTAT's regional and grouping aggregates (`World`, `Africa`,
#' `European Union`, `Least Developed Countries` and 38 others) carry area codes
#' at or above 5000, resolve to no polity, and are dropped, so they cannot leak
#' into a per-country denominator.
#'
#' `area_code` is `polity_area_code`, a **bucket, not an identity**, resolved
#' year by year exactly as the commodity balances resolve it. On the real pins no
#' bucket-year receives more than one FAOSTAT area, so no row here sums two
#' territories — but note South Sudan (277) has no population row in either pin,
#' so bucket 206 from 2012 is Sudan alone, where the `gdp-population` pin sums
#' `SDN + SSD` onto it.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the pins cover.
#' @param data Optional named list of pre-loaded raw pins to avoid the pin read,
#'   `fbs_old` and/or `fbs_new`, each in the pins' own long FAOSTAT layout
#'   (`Area Code`, `Item Code`, `Element Code`, `Year`, `Value`). Falls back to
#'   [whep_read_file()] for whichever is absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with `year`, `area_code`, `population` (persons) and
#'   `source_pop` (`"FAOSTAT FBS old"` or `"FAOSTAT FBS new"`, naming which pin
#'   the row came from), one row per area code and year, sorted by year then
#'   area code, plus the polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' read_fbs_population(example = TRUE)
read_fbs_population <- function(years = NULL, data = list(), example = FALSE) {
  if (isTRUE(example)) {
    return(.example_fbs_population())
  }
  old <- data$fbs_old %||% whep_read_file("faostat-fbs-old")
  new <- data$fbs_new %||% whep_read_file("faostat-fbs-new")
  dplyr::bind_rows(
    .fbs_pop_parse(old, years, "FAOSTAT FBS old", 2L),
    .fbs_pop_parse(new, years, "FAOSTAT FBS new", 1L)
  ) |>
    .fbs_pop_prefer_new() |>
    dplyr::arrange(.data$year, .data$area_code) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# One pin, reduced to the population item and resolved onto the polity bucket
# the year-aware way -- the same resolution the commodity balances use for the
# numerator, so the two agree on which bucket a FAOSTAT area belongs to in a
# given year. `rank` orders the two vintages for `.fbs_pop_prefer_new()`.
.fbs_pop_parse <- function(raw, years, label, rank) {
  needed <- c("Area Code", "Item Code", "Element Code", "Year", "Value")
  .check_columns(raw, needed, "the FAOSTAT FBS table")
  out <- tibble::as_tibble(raw) |>
    dplyr::filter(
      as.integer(.data[["Item Code"]]) == .fbs_population_item(),
      as.integer(.data[["Element Code"]]) == .fbs_population_element()
    ) |>
    dplyr::transmute(
      year = as.integer(.data[["Year"]]),
      area_code = as.integer(.data[["Area Code"]]),
      # FAOSTAT publishes this item in thousands in both pins ("1000 persons"
      # in the old one, "1000 No" in the new one).
      population = as.numeric(.data[["Value"]]) * 1000,
      source_pop = .env$label,
      source_rank = .env$rank
    ) |>
    dplyr::filter(
      !is.na(.data$year),
      !is.na(.data$area_code),
      is.finite(.data$population),
      .data$population > 0
    )
  if (!is.null(years)) {
    out <- dplyr::filter(out, .data$year %in% years)
  }
  .fbs_pop_bucket(out)
}

# FAOSTAT area code -> `polity_area_code`, year by year. Areas that resolve to
# no polity are FAOSTAT's own regional and grouping aggregates and are dropped;
# summing them into a denominator would double count every member.
.fbs_pop_bucket <- function(parsed) {
  parsed |>
    add_polity_code(code_column = "area_code", year_column = "year") |>
    dplyr::filter(!is.na(.data$polity_area_code)) |>
    dplyr::summarise(
      population = sum(.data$population),
      .by = c("year", "polity_area_code", "source_pop", "source_rank")
    ) |>
    dplyr::rename(area_code = "polity_area_code") |>
    dplyr::mutate(area_code = as.integer(.data$area_code))
}

# The two vintages overlap over 2010-2013 and disagree there. Keep the newer
# pin's row, matching the order `.cbs_source_rank()` gives them on the food
# side, so a per-capita ratio is not built from two vintages of the same year.
.fbs_pop_prefer_new <- function(stacked) {
  stacked |>
    dplyr::slice_min(
      .data$source_rank,
      n = 1L,
      by = c("year", "area_code"),
      with_ties = FALSE
    ) |>
    dplyr::select(-"source_rank")
}

.fbs_population_item <- function() 2501L

.fbs_population_element <- function() 511L

# A real read_fbs_population() slice: area 186 Serbia and Montenegro, the
# territory neither ISO3-keyed source can reach, plus one ordinary area as a
# control.
.example_fbs_population <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~population,
    ~source_pop,
    1992L,
    186L,
    10429000,
    "FAOSTAT FBS old",
    2000L,
    186L,
    10801000,
    "FAOSTAT FBS old",
    2005L,
    186L,
    10471000,
    "FAOSTAT FBS old",
    2010L,
    203L,
    46840470,
    "FAOSTAT FBS new"
  ) |>
    .add_reporting_polity_columns()
}
