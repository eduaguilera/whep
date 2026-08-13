# Population protein requirement for the SJOS-N nourishment ("just") axis.
#
# The axis compares a national per-capita protein supply against a floor. That
# floor has to be a POPULATION requirement, and WHO/FAO/UNU TRS 935 is explicit
# that an individual safe intake level cannot serve as one:
#
#   p.41  "reference intake or safe intake levels defined as above for
#          individuals have been incorrectly applied to populations."
#   p.241 "For a population, a safe population intake cannot be defined as a
#          simple function of the mean requirement."
#
# So this function returns the population-weighted AVERAGE requirement, which is
# the anchor the report names ("the practically useful measure is the average
# requirement ... 0.66 g protein/kg per day", p.41). The margin that turns an
# average requirement into a supply threshold is applied downstream, once, over
# the convolution of requirement and intake variability -- not here.
#
# Using a flat adult value instead is not neutral: children need far less
# protein in absolute terms (17.1 g/day at ages 4-6 against 46 g/day for a 55 kg
# adult), so a flat floor overstates the requirement of every population and
# overstates it most in the youngest, which are also the poorest.

#' Build the population protein requirement.
#'
#' @description
#' Weights the WHO/FAO/UNU TRS 935 per-class protein requirements by a
#' population's age and sex structure, giving the mean requirement of an average
#' member of that population in grams of protein per day.
#'
#' The default `requirement = "average"` uses the class AVERAGE (median)
#' requirement, which is the anchor TRS 935 names for population use. The
#' alternative `"safe"` uses the class safe level, the 97.5th percentile of the
#' individual requirement distribution; it is offered for continuity with
#' analyses built on the safe level, but TRS 935 calls that application to
#' populations incorrect, and it double-counts the requirement margin whenever a
#' dispersion allowance is also applied downstream.
#'
#' Population rows are supplied as age groups. Each group is expanded to the
#' single years of age it spans, which are assumed uniformly distributed within
#' the group; on UN WPP 2024 data the difference between five-year groups and
#' single-year data is at most 0.105 g/cap/day (0.3%).
#'
#' @param data Named list of injected inputs. `population_age` is required:
#'   `year`, `area_code`, `age_start`, `age_span`, `sex` (`"m"` / `"f"`) and
#'   `population`. `protein_requirement` overrides the packaged coefficient
#'   table.
#' @param requirement Which TRS 935 column to weight: `"average"` (default, the
#'   class average requirement) or `"safe"` (the class safe level).
#' @return A tibble keyed by `year`, `area_code` with `requirement_g_cap_day`,
#'   `population`, `method_requirement`, plus the polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_protein_requirement(
#'   data = list(
#'     population_age = tibble::tribble(
#'       ~year, ~area_code, ~age_start, ~age_span, ~sex, ~population,
#'       2010L, 10L,        0L,         5L,        "m",  1000,
#'       2010L, 10L,        0L,         5L,        "f",  1000,
#'       2010L, 10L,        20L,        5L,        "m",  3000,
#'       2010L, 10L,        20L,        5L,        "f",  3000
#'     )
#'   )
#' )
build_protein_requirement <- function(
  data = list(),
  requirement = c("average", "safe")
) {
  requirement <- rlang::arg_match(requirement)
  population <- data$population_age
  .check_columns(
    population,
    c("year", "area_code", "age_start", "age_span", "sex", "population"),
    "data$population_age"
  )
  coefs <- data$protein_requirement %||%
    whep::whep_coef_table("protein_requirement")
  .pr_validate_sex(population)

  by_year <- .pr_requirement_by_year(coefs, requirement)
  population |>
    .pr_group_requirement(by_year) |>
    .pr_weight(requirement) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# Sex must be the two codes the coefficient table is keyed on. Anything else
# would silently drop rows in the join below, losing population without saying
# so.
.pr_validate_sex <- function(population) {
  bad <- setdiff(unique(population$sex), c("m", "f"))
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "{.arg data$population_age} has unexpected {.field sex} value{?s}
       {.val {bad}}.",
      "i" = "Expected {.val m} and {.val f}."
    ))
  }
}

# One requirement per single year of age and sex, expanded from the TRS 935
# classes over their integer year-of-age bounds.
#
# Two details the bounds encode. TRS 935 resolves infancy sub-annually, so its
# 1 and 1.5 classes both fall inside year-of-age 1 and are AVERAGED there
# rather than one of them silently winning a de-duplication. And the adult row
# runs to 120 because the report states the adult requirement per kg is the
# same "at all ages" above 18 (p.242 section 14.2), so no age may fall through
# to NA -- a missing year would poison every population group spanning it.
.pr_requirement_by_year <- function(coefs, requirement) {
  column <- if (requirement == "average") {
    "avg_req_g_day"
  } else {
    "safe_req_g_day"
  }
  coefs |>
    dplyr::mutate(
      age = purrr::map2(
        .data$year_from,
        .data$year_to,
        \(from, to) seq(from, to)
      ),
      requirement_g_day = .data[[column]]
    ) |>
    tidyr::unnest("age") |>
    dplyr::summarise(
      requirement_g_day = mean(.data$requirement_g_day),
      .by = c("age", "sex")
    )
}

# Mean requirement over the single years a population age group spans. The
# years inside a group are weighted equally, which is the uniform-distribution
# assumption documented in the roxygen.
.pr_group_requirement <- function(population, by_year) {
  population |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::mutate(
      age = purrr::map2(
        .data$age_start,
        .data$age_span,
        \(from, span) seq(from, from + max(span, 1L) - 1L)
      )
    ) |>
    tidyr::unnest("age") |>
    dplyr::left_join(by_year, by = c("age", "sex")) |>
    .pr_check_ages_covered() |>
    dplyr::summarise(
      year = dplyr::first(.data$year),
      area_code = dplyr::first(.data$area_code),
      population = dplyr::first(.data$population),
      requirement_g_day = mean(.data$requirement_g_day),
      .by = ".row"
    )
}

# Every single year of age a population covers must resolve to a requirement.
# An uncovered year would average to NA and take its whole age group with it,
# which surfaces far downstream as a missing country rather than as a coverage
# gap in the coefficient table.
.pr_check_ages_covered <- function(expanded) {
  missing <- sort(unique(expanded$age[is.na(expanded$requirement_g_day)]))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "No protein requirement for age{?s} {.val {missing}}.",
      "i" = "The coefficient table must cover every year of age present in
             {.arg data$population_age}."
    ))
  }
  expanded
}

# Population-weighted mean requirement per country-year.
.pr_weight <- function(groups, requirement) {
  groups |>
    dplyr::summarise(
      requirement_g_cap_day = stats::weighted.mean(
        .data$requirement_g_day,
        w = .data$population
      ),
      population = sum(.data$population),
      .by = c("year", "area_code")
    ) |>
    dplyr::mutate(method_requirement = requirement)
}
