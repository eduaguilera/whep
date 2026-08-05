# Between-country nourishment inequality for the SJOS-N "just" axis (Module 3,
# Task 3.4). calculate_food_gini() reduces a per-capita food-supply series to a
# per-year, population-weighted Gini coefficient of the between-country
# distribution (the Global sjos_food.R Score / 1 - sum(Score) construction). It
# is a DIAGNOSTIC of how unequally supply is shared, not a rescaling of the
# per-country nourishment score. disaggregate_ussr() splits the pre-1992
# aggregate USSR ("Russian Federation") supply row into successor states by
# their 1992 population shares, so the early series carries the successor states
# rather than one lumped polity (the Global sjos_n.R USSR bind_rows).
#
# Integration note: ussr_shares (the ussr_area_code -> successor_area_code
# population-share lookup) is injected as a small package-data / integration
# input; deriving it from a 1992 population table is a wiring step out of scope
# for this fixture-tested function.

#' Between-country population-weighted Gini of per-capita food supply.
#'
#' @description
#' Reduces a per-capita nourishment series to a per-year, between-country,
#' population-weighted Gini coefficient, a diagnostic of how unequally
#' per-capita supply is shared across countries (it does not rescale the
#' per-country nourishment score). Within each `year`, countries are sorted in
#' descending per-capita supply; each country's population fraction `pop_frac`
#' and its share of the total supply mass `value_frac` (per-capita supply times
#' population, over the world total) give a score `value_frac * (pop_frac + 2 *
#' richer_frac)`, where `richer_frac` is the cumulative population fraction of
#' the strictly richer countries. The Gini is `1 - sum(score)` over the
#' countries in that year, `0` under a perfectly equal distribution and rising
#' toward `1` as supply concentrates. Protein per-capita supply is the SJOS-N
#' nourishment axis, so it is the default; passing a different `value_col` (for
#' example `energy_kcal_cap_day`) takes the Gini of that axis instead. Rows with
#' a missing supply or population value are dropped before the computation.
#'
#' @param x A tibble with a `year` column, the per-capita supply column named by
#'   `value_col` and the population column named by `pop_col` (for example a
#'   [build_food_supply()] output).
#' @param value_col The unquoted per-capita supply column whose between-country
#'   inequality is measured. Defaults to `protein_g_cap_day`.
#' @param pop_col The unquoted population column used as the inequality weight.
#'   Defaults to `population`.
#' @return A tibble keyed by `year` with the `gini` coefficient.
#' @export
#' @examples
#' calculate_food_gini(
#'   tibble::tribble(
#'     ~year, ~area_code, ~protein_g_cap_day, ~population,
#'     2000L, 10L, 40, 100,
#'     2000L, 20L, 40, 100,
#'     2000L, 30L, 40, 100
#'   )
#' )
calculate_food_gini <- function(
  x,
  value_col = protein_g_cap_day,
  pop_col = population
) {
  x |>
    dplyr::filter(!is.na({{ value_col }}), !is.na({{ pop_col }})) |>
    dplyr::arrange(.data$year, dplyr::desc({{ value_col }})) |>
    dplyr::mutate(
      score = .food_gini_score({{ value_col }}, {{ pop_col }}),
      .by = year
    ) |>
    dplyr::summarise(gini = 1 - sum(.data$score), .by = year)
}

#' Split the pre-1992 aggregate USSR supply into successor states.
#'
#' @description
#' For years before `cutoff_year`, replaces the single aggregate USSR row
#' (the `area_code` listed as `ussr_area_code` in `ussr_shares`) with one row
#' per successor state, inheriting the aggregate's per-capita supply values and
#' scaling its population by the successor's 1992 population share, so the split
#' conserves the aggregate population and each successor carries the aggregate
#' per-capita supply. Rows at or after `cutoff_year`, and all non-USSR rows,
#' pass through unchanged. This mirrors the Global SJOS-N pre-1992 USSR
#' disaggregation, which distributes the Russian Federation Food Balance Sheet
#' supply across the ex-USSR states by their 1992 population weights.
#'
#' @param x A tibble with `year`, `area_code` and `population` columns (plus any
#'   per-capita supply columns to inherit), for example a [build_food_supply()]
#'   output.
#' @param ussr_shares A lookup tibble with `ussr_area_code` (the aggregate USSR
#'   area code), `successor_area_code` (a successor state's area code) and
#'   `pop_share` (the successor's 1992 population share, summing to one per
#'   aggregate). Injected as a package-data / integration input.
#' @param cutoff_year The first year for which successor states report
#'   separately; the split applies to years strictly before it. Defaults to
#'   `1992L`.
#' @return `x` with the pre-cutoff aggregate USSR rows replaced by their
#'   successor-state rows.
#' @export
#' @examples
#' disaggregate_ussr(
#'   tibble::tribble(
#'     ~year, ~area_code, ~protein_g_cap_day, ~population,
#'     1990L, 228L, 50, 100,
#'     1995L, 228L, 55, 120
#'   ),
#'   tibble::tribble(
#'     ~ussr_area_code, ~successor_area_code, ~pop_share,
#'     228L, 1L, 0.6,
#'     228L, 2L, 0.4
#'   )
#' )
disaggregate_ussr <- function(x, ussr_shares, cutoff_year = 1992L) {
  .check_columns(x, c("year", "area_code", "population"), "x")
  .check_columns(
    ussr_shares,
    c("ussr_area_code", "successor_area_code", "pop_share"),
    "ussr_shares"
  )
  ussr_codes <- unique(ussr_shares$ussr_area_code)
  to_split <- dplyr::filter(
    x,
    .data$year < cutoff_year,
    .data$area_code %in% ussr_codes
  )
  kept <- dplyr::filter(
    x,
    !(.data$year < cutoff_year & .data$area_code %in% ussr_codes)
  )
  dplyr::bind_rows(kept, .ussr_split_rows(to_split, ussr_shares))
}

# ---- Private helpers -------------------------------------------------------

# The per-country Gini score for one year's countries, already sorted in
# descending per-capita supply: population fraction, supply-mass fraction and
# the cumulative population fraction of the strictly richer countries combine
# into value_frac * (pop_frac + 2 * richer_frac). Summing and subtracting from
# one yields the Gini for the year.
.food_gini_score <- function(value, pop) {
  pop_frac <- pop / sum(pop)
  value_abs <- value * pop
  value_frac <- value_abs / sum(value_abs)
  richer_frac <- cumsum(pop_frac) - pop_frac
  value_frac * (pop_frac + 2 * richer_frac)
}

# Expand the aggregate USSR rows into successor-state rows: match each aggregate
# to its successors, adopt the successor area code, scale the population by the
# successor share (per-capita supply columns are inherited unchanged) and drop
# the lookup helper columns.
.ussr_split_rows <- function(to_split, ussr_shares) {
  to_split |>
    dplyr::rename(ussr_area_code = area_code) |>
    dplyr::inner_join(
      ussr_shares,
      by = "ussr_area_code",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      area_code = .data$successor_area_code,
      population = .data$population * .data$pop_share
    ) |>
    dplyr::select(
      -"ussr_area_code",
      -"successor_area_code",
      -"pop_share"
    )
}
