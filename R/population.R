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
# The second regrouping is deliberate: `polity_area_code` is a bucket, not an
# identity, so several ISO3 codes can share one code and are summed into one
# row. Since WHEP models the reporting members of bucket 999 in their own right
# (#628), the only fold the real pin still exercises by default is Sudan plus
# South Sudan on 206 "Sudan (former)" from 2012 on; restoring the Rest-of-World
# fold with `options(whep.unfold_rest_of_world = "none")` puts Syria, North
# Macedonia, Palestine, Eswatini, Equatorial Guinea and French Guiana back on
# 999. A row is therefore not always one country, which is why
# `.pop_report_folded()` names it (#482).
#
# COVERAGE, which is the other half of the same story (#543). The pin does not
# reach every area WHEP models: measured on the real pin, 190 of the 256 area
# codes the crosswalk resolves get a population row and 66 get none. The two
# per-capita consumers -- `build_food_supply()` and `build_n_percapita()` --
# inner-join this table, so an area with no row does not come out understated,
# it does not come out at all. `.warn_missing_population()` lives here and is
# called from both, because the omission is a property of the denominator and
# neither `.pop_report_unmapped()` (which measures its drops against WORLD
# population, 0.07%) nor `.pop_report_folded()` can see which uncovered area
# actually reports food.

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
#' them, so some rows are aggregates of more than one territory. WHEP models the
#' reporting members of bucket 999 in their own right, so with the real pin and
#' the default options the only such row is code 206 "Sudan (former)", carrying
#' Sudan plus South Sudan from 2012 on; it resolves to `SUD-1956-2011`, the
#' pre-secession territory its two members together cover. Under
#' `options(whep.unfold_rest_of_world = "none")` code 999 folds Syria, North
#' Macedonia, Palestine, Eswatini, Equatorial Guinea and French Guiana as well.
#' Every folded row is named in a message.
#'
#' The pin does not cover every area WHEP models, and that is a bigger gap than
#' the fold. On the real pin 190 of the 256 area codes the crosswalk resolves
#' get a population row; the 66 that do not include Bhutan, Comoros, New
#' Caledonia and the Faroe Islands, all of which the commodity balances do give
#' food to. [build_food_supply()] and [build_n_percapita()] inner-join this
#' table, so those areas are absent from their per-capita output rather than
#' wrong in it. Both warn and name them instead of dropping them silently;
#' `options(whep.warn_missing_population = FALSE)` silences that warning.
#'
#' `population_source = "pin_wpp_fallback"` fills the country-years the pin
#' does not reach from [read_wpp_population()], and **only** those: the pin wins
#' wherever both have a value, so turning it on cannot move a denominator that
#' was already published. On the real inputs it adds 44 areas the pin has no row
#' for at all — Réunion, Bhutan, Comoros, Western Sahara, New Caledonia, the
#' French overseas departments and the small island states — and 4,755
#' country-years inside the pin's own year span. Filled rows are stamped
#' `source_pop = "UN WPP 2024"`.
#'
#' It is a gap-filler and not a replacement, because the two sources disagree
#' where they overlap: across 12,309 shared country-years they differ by a
#' median 0.64%, a 95th percentile of 4.4% and a maximum of 81%. That is why
#' `"pin"` remains the default.
#'
#' Neither source can reach an area whose territory no longer exists, because
#' both are keyed on a present-day ISO3 code. [population_source_reach()]
#' reports which areas that leaves out and whether the polities database's
#' `successor` relation could stand in for them. Against UN WPP 2024's
#' vocabulary, exactly one reporting area outside the Rest-of-World bucket is
#' unreachable by either route: area 151 Netherlands Antilles, `ANT-1961-2010`,
#' which carries commodity-balance food in every year from 1961 to 2010 and for
#' which upstream publishes no successor at all (#787). Reachable is not the
#' same as safe to sum — see that function and the note at the top of
#' `R/population_reach.R`.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the pin covers.
#' @param data Optional named list of pre-loaded inputs to avoid the pin read:
#'   `gdp_population` (the raw pin, with `Year`, `area_code` as ISO3, `pop` in
#'   thousands) and `wpp_population` (a [read_wpp_population()] output). Falls
#'   back to [whep_read_file()] when absent.
#' @param population_source `"pin"` (default, the `gdp-population` pin alone) or
#'   `"pin_wpp_fallback"`, which additionally fills country-years the pin does
#'   not cover from UN WPP.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with `year`, `area_code`, `population` (persons) and
#'   `source_pop`, one row per area code and year, sorted by year then area
#'   code, plus the polity columns below. `source_pop` carries the pin's own
#'   vocabulary (`"Original"`, `"Linear interpolation"`, `"First value carried
#'   backwards"`), joined with `" + "` when a bucket sums ISO3 codes of
#'   differing provenance, or `"UN WPP 2024"` for a fallback-filled row. A row
#'   is one country in the common case, but `area_code` is
#'   an aggregation bucket: rows from 2012 on 206 ("Sudan (former)") are sums
#'   over several territories rather than a single country, as are rows on 999
#'   ("Rest of World") when the Rest-of-World fold is restored.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' read_population(example = TRUE)
read_population <- function(
  years = NULL,
  data = list(),
  population_source = c("pin", "pin_wpp_fallback"),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_population())
  }
  population_source <- rlang::arg_match(population_source)
  raw <- data$gdp_population %||% whep_read_file("gdp-population")
  .check_columns(raw, c("Year", "area_code", "pop"), "gdp_population")
  parsed <- .pop_parse(raw, years)
  .pop_report_unmapped(parsed)
  .pop_report_folded(parsed)
  parsed |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::summarise(
      population = sum(.data$population),
      source_pop = .pop_collapse_source(.data$source_pop),
      .by = c("year", "area_code")
    ) |>
    .pop_fill_from_wpp(population_source, data$wpp_population, years) |>
    dplyr::arrange(.data$year, .data$area_code) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# Parse the pin onto the package contract: ISO3 -> numeric area_code, thousands
# -> persons. Non-positive and missing populations are dropped; they are not a
# meaningful denominator and would produce Inf per-capita supply.
.pop_parse <- function(raw, years) {
  # The pin's own provenance vocabulary ("Original", "Linear interpolation",
  # "First value carried backwards"). Older or injected inputs may not carry
  # it, in which case every row is simply "pin".
  raw <- tibble::as_tibble(raw)
  source_pop <- if (rlang::has_name(raw, "Source_pop")) {
    as.character(raw$Source_pop)
  } else {
    rep("pin", nrow(raw))
  }
  out <- raw |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      iso3c = as.character(.data$area_code),
      population = as.numeric(.data$pop) * 1000,
      source_pop = .env$source_pop
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

# A bucket row can sum several ISO3 codes whose provenance differs, so the
# summary keeps every distinct value rather than the first. Reporting one would
# hide that half a row is interpolated.
.pop_collapse_source <- function(source_pop) {
  paste(sort(unique(source_pop)), collapse = " + ")
}

# `pin_wpp_fallback`: fill ONLY the country-years the pin does not reach, from
# the UN WPP reader, stamped in the pin's own vocabulary so a filled row can
# never be mistaken for a pinned one. The pin always wins where both have a
# value, so turning this on cannot move a denominator that was already
# published -- it can only add one that was missing. On the real inputs that is
# 44 areas the pin has no row for at all (Reunion, Bhutan, Comoros, Western
# Sahara, New Caledonia, the French overseas departments and the small island
# states) and 4,755 country-years inside the pin's own year span.
#
# The two sources are NOT interchangeable where they overlap: across 12,309
# shared country-years they differ by a median 0.64%, a 95th percentile of
# 4.4%, and up to 81%. That is the reason this is a gap-filler and not a
# replacement, and the reason the default stays `"pin"`.
.pop_fill_from_wpp <- function(pinned, population_source, wpp, years) {
  if (population_source == "pin") {
    return(pinned)
  }
  wpp <- wpp %||% read_wpp_population(years = years)
  .check_columns(
    wpp,
    c("year", "area_code", "population"),
    "data$wpp_population"
  )
  fill <- wpp |>
    dplyr::filter(!is.na(.data$area_code)) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      population = as.numeric(.data$population),
      source_pop = "UN WPP 2024"
    ) |>
    dplyr::anti_join(pinned, by = c("year", "area_code"))
  if (!is.null(years)) {
    fill <- dplyr::filter(fill, .data$year %in% years)
  }
  dplyr::bind_rows(pinned, fill)
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

# ---- The denominator a per-capita divide could not find --------------------

# Warn about the country-years a per-capita divide drops for lack of a
# population row, naming the areas and the share of the quantity that goes with
# them.
#
# `build_food_supply()` and `build_n_percapita()` both inner-join the population
# table, so an uncovered area is not understated -- it is absent, and the output
# looks complete. Until this warned, the only diagnostics were the two messages
# above, and neither can see the omission: `.pop_report_unmapped()` measures the
# five continental residuals it drops against WORLD population (0.07%), which is
# true and says nothing about whether an area that reports food has a
# denominator (#543).
#
# Warn rather than inform, for the same reason `.warn_folded_areas()` does: the
# whole defect is that the loss was reportable and unreported.
.warn_missing_population <- function(agg, population, mass_col, quantity) {
  if (!isTRUE(getOption("whep.warn_missing_population", TRUE))) {
    return(invisible(NULL))
  }
  dropped <- .missing_denominator_areas(agg, population, mass_col)
  if (nrow(dropped) == 0L) {
    return(invisible(dropped))
  }
  total <- sum(agg[[mass_col]], na.rm = TRUE)
  share <- if (total > 0) sum(dropped$mass) / total else NA_real_
  cli::cli_warn(c(
    "!" = "{nrow(dropped)} area{?s} carrying {quantity}
           {cli::qty(nrow(dropped))}{?has/have} no population
           row, so {cli::qty(sum(dropped$area_years))}
           {sum(dropped$area_years)} area-year{?s} {?is/are} dropped from the
           per-capita output rather than reported.",
    stats::setNames(dropped$label, rep("*", nrow(dropped))),
    "i" = "{.val {signif(100 * share, 3)}}% of the {quantity} in range goes
           with them. See {.fun read_population} for what the denominator
           covers."
  ))
  invisible(dropped)
}

# The areas of `agg` with no (year, area_code) match in `population`, heaviest
# first, one row per area with the quantity and the area-year count it takes
# with it. Keyed on `area_code`, never on a name: the same bucket can carry
# different labels across years (#589).
.missing_denominator_areas <- function(agg, population, mass_col) {
  agg |>
    dplyr::anti_join(
      dplyr::distinct(population, .data$year, .data$area_code),
      by = c("year", "area_code")
    ) |>
    dplyr::summarise(
      mass = sum(.data[[mass_col]], na.rm = TRUE),
      area_years = dplyr::n(),
      .by = "area_code"
    ) |>
    dplyr::arrange(dplyr::desc(.data$mass)) |>
    .missing_denominator_labels()
}

# One "Name (code, n area-years)" bullet per dropped area. The name is attached
# here, at the reporting stage, and only for the message.
.missing_denominator_labels <- function(dropped) {
  if (nrow(dropped) == 0L) {
    return(dplyr::mutate(dropped, label = character(0)))
  }
  dropped |>
    add_area_name() |>
    dplyr::mutate(
      label = paste0(
        dplyr::coalesce(.data$area_name, "unnamed area"),
        " (",
        .data$area_code,
        ", ",
        .data$area_years,
        " area-year",
        dplyr::if_else(.data$area_years == 1L, "", "s"),
        ")"
      )
    )
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
