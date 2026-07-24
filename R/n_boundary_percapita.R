# Per-capita reactive-nitrogen boundary axis for the SJOS-N scatter (Module 3,
# Task 3.5). build_n_boundary_percapita() normalizes each country's total
# anthropogenic reactive-N per capita against the world per-capita planetary
# boundary (the Global comdat_global.R Normalize_impacts piecewise, capped at
# 6), scales it by the agri-food-system share of the boundary (afs_share,
# locked-plan decision 8, flagged provisional in Global), and joins it to the
# nourishment normalization to produce the environ_health_n scatter (x =
# nourishment norm, y = boundary norm, point size = population).
#
# Units. n_percapita_kg is country total anthropogenic reactive N per capita in
# kg N/cap/yr (synthetic + BNF, Campbell framing), already divided by
# population. The world per-capita boundary converts the Tg N/yr planetary
# limits (boundary_low 60, boundary_high 125) to kg N/cap/yr as
# boundary_Tg * 1e9 / world_population, where 1e9 is Tg N -> kg N and
# world_population is nourishment$population summed per year (WHEP population is
# absolute persons; Global's kg-per-thousand-persons 1e6 factor does not apply
# here). world_population from the food-supply set approximates the true world
# population (the set of countries with supply data).
#
# Integration note: n_percapita is produced by build_n_percapita()
# (R/n_percapita.R), which aggregates build_n_inputs()'s synthetic + BNF terms
# to a national total and divides by population; it can also be injected
# directly. This function only normalizes and joins, so it stays fixture-tested.

#' Build the per-capita nitrogen-boundary versus nourishment scatter.
#'
#' @description
#' Normalizes each country's total anthropogenic reactive-nitrogen per capita
#' against the world per-capita planetary-nitrogen boundary and joins it to the
#' nourishment normalization, yielding the safe-and-just nitrogen scatter (one
#' point per country-year: nourishment adequacy on the x axis, boundary
#' pressure on the y axis, population as the point weight). The world
#' per-capita boundary converts the Tg N/yr limits in `params` (`boundary_low`,
#' `boundary_high`) to kg N/cap/yr by dividing by the world population
#' (`nourishment` population summed per year). The normalization is the Global
#' piecewise: `n_percapita_kg / low_pc` below the lower bound, `1 +
#' (n_percapita_kg - low_pc) / (high_pc - low_pc)` within the band, and `min(1 +
#' n_percapita_kg / high_pc, 6)` above the upper bound. The result is then
#' scaled by `afs_share`, the agri-food-system share of the boundary (a
#' parameter, default 0.8, flagged provisional).
#'
#' @param n_percapita A tibble keyed by `year`, `area_code` with
#'   `n_percapita_kg`, the country total anthropogenic reactive N per capita
#'   (kg N/cap/yr, synthetic plus biological fixation), from
#'   [build_n_percapita()] or injected directly.
#' @param nourishment A tibble keyed by `year`, `area_code` with `value_norm`
#'   (the nourishment normalization, for example a [normalize_nourishment()]
#'   output) and `population` (absolute persons), whose per-year population sum
#'   sets the world per-capita boundary.
#' @param params Boundary parameters, defaulting to [n_boundary_params], read
#'   for the `boundary_low` and `boundary_high` Tg N/yr limits.
#' @param afs_share The agri-food-system share of the planetary boundary applied
#'   to the normalized boundary pressure. Defaults to `0.8` (provisional).
#' @return A tibble keyed by `year`, `area_code` with `nourish_norm` (the
#'   nourishment normalization), `boundary_norm` (the afs-scaled per-capita
#'   boundary normalization) and `population`.
#' @export
#' @examples
#' build_n_boundary_percapita(
#'   n_percapita = tibble::tribble(
#'     ~year, ~area_code, ~n_percapita_kg,
#'     2000L, 10L, 5,
#'     2000L, 20L, 15
#'   ),
#'   nourishment = tibble::tribble(
#'     ~year, ~area_code, ~value_norm, ~population,
#'     2000L, 10L, 0.8, 3e9,
#'     2000L, 20L, 1.5, 3e9
#'   )
#' )
build_n_boundary_percapita <- function(
  n_percapita,
  nourishment,
  params = NULL,
  afs_share = 0.8
) {
  params <- params %||% whep::n_boundary_params
  .check_columns(
    n_percapita,
    c("year", "area_code", "n_percapita_kg"),
    "n_percapita"
  )
  .check_columns(
    nourishment,
    c("year", "area_code", "value_norm", "population"),
    "nourishment"
  )
  n_percapita |>
    dplyr::left_join(
      .n_boundary_world_bounds(nourishment, params),
      by = "year"
    ) |>
    dplyr::mutate(
      boundary_norm = .n_boundary_normalize(
        .data$n_percapita_kg,
        .data$low_pc,
        .data$high_pc
      ) *
        afs_share
    ) |>
    .n_boundary_join_nourishment(nourishment)
}

# ---- Private helpers -------------------------------------------------------

# World per-capita boundary per year: the Tg N/yr low/high limits converted to
# kg N/cap/yr by 1e9 (Tg N -> kg N) over the year's world population (the
# nourishment population sum).
.n_boundary_world_bounds <- function(nourishment, params) {
  low_tg <- .n_boundary_param(params, "boundary_low")
  high_tg <- .n_boundary_param(params, "boundary_high")
  nourishment |>
    dplyr::summarise(world_pop = sum(.data$population), .by = year) |>
    dplyr::mutate(
      low_pc = low_tg * 1e9 / .data$world_pop,
      high_pc = high_tg * 1e9 / .data$world_pop
    ) |>
    dplyr::select("year", "low_pc", "high_pc")
}

# Pull one boundary parameter value from the long-form params tibble.
.n_boundary_param <- function(params, name) {
  params |>
    dplyr::filter(.data$parameter == name) |>
    dplyr::pull(.data$value)
}

# The Global Normalize_impacts piecewise: below the lower per-capita bound the
# pressure scales toward 1, the band maps onto [1, 2], and above the upper
# bound it grows past 2 up to a hard cap of 6.
.n_boundary_normalize <- function(value, low_pc, high_pc) {
  dplyr::case_when(
    value < low_pc ~ value / low_pc,
    value > high_pc ~ pmin(1 + value / high_pc, 6),
    .default = 1 + (value - low_pc) / (high_pc - low_pc)
  )
}

# Inner-join the normalized boundary pressure to the nourishment normalization,
# keeping only country-years present on both axes, and shape the scatter.
.n_boundary_join_nourishment <- function(boundary, nourishment) {
  boundary |>
    dplyr::inner_join(
      dplyr::select(
        nourishment,
        "year",
        "area_code",
        "value_norm",
        "population"
      ),
      by = c("year", "area_code")
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      nourish_norm = .data$value_norm,
      boundary_norm = .data$boundary_norm,
      population = .data$population
    )
}
