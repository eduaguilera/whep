# Country per-capita anthropogenic reactive-nitrogen axis (SJOS-N Module 3).
# build_n_percapita() aggregates a build_n_inputs() long output to the total
# anthropogenic reactive nitrogen entering each country's agricultural land,
# then divides by population to give the kg N/cap/yr that
# build_n_boundary_percapita() (R/n_boundary_percapita.R) normalizes against
# the world per-capita planetary-nitrogen boundary. This closes the injection
# seam that function's file note flagged: its n_percapita input was previously
# supplied by the caller rather than derived from the N inputs.
#
# Framing. "synthetic_bnf" (default, the Campbell / Global framing) scales
# synthetic fertiliser to total agricultural reactive N with the packaged
# `syn_tot_agri_ratio`, then adds biological nitrogen fixation. Recycled or
# internal terms (manure, atmospheric
# deposition, urban/human N, soil-organic-matter mineralization) are excluded
# because they are not new fixation of reactive nitrogen. Other framings (e.g.
# adding atmospheric deposition or manure) can be added as further `framing`
# choices; each maps to its own set of fert_type terms.
#
# Units. n_input_t is tonnes N; multiplying by 1000 converts to kg N, and
# dividing by population (absolute persons) gives kg N/cap/yr.

#' Build country per-capita anthropogenic reactive nitrogen.
#'
#' @description
#' Aggregates a [build_n_inputs()] long-format nitrogen-input tibble to the
#' total anthropogenic reactive nitrogen entering each country's agricultural
#' land and divides by population, giving the per-capita reactive nitrogen
#' (kg N/cap/yr) that [build_n_boundary_percapita()] consumes as its
#' `n_percapita` input. The default `"synthetic_bnf"` framing (the Campbell /
#' Global framing) sums the synthetic-fertiliser and biological-nitrogen-
#' fixation input terms using `synthetic * syn_tot_agri_ratio + BNF`, the
#' locked Campbell / Global framing; recycled or internal terms (manure,
#' deposition, urban, soil-organic-matter mineralization) are excluded. Any
#' finer grid key (`lon`, `lat`, `item_cbs_code`) is aggregated away to the
#' country total, and country-years without a matching population row are
#' dropped. The chosen framing is stamped on every row.
#'
#' @param n_inputs A [build_n_inputs()] long-format output with `fert_type`,
#'   `n_input_t` and the `year`, `area_code` keys (finer grid keys such as
#'   `lon`/`lat`/`item_cbs_code` are summed away).
#' @param population A tibble keyed by `year`, `area_code` with `population`
#'   (absolute persons).
#' @param framing How the total anthropogenic reactive nitrogen is defined.
#'   `"synthetic_bnf"` (default) scales the `"synthetic"` term by
#'   `syn_tot_agri_ratio` and adds the `"bnf"` term; other framings can be added.
#' @param params Boundary parameters, defaulting to [n_boundary_params], used
#'   here for `syn_tot_agri_ratio`.
#' @param example If `TRUE`, return a small fixture instead of computing from
#'   `n_inputs`/`population`. Defaults to `FALSE`.
#' @return A tibble keyed by `year`, `area_code` with `n_percapita_kg`, the
#'   country total anthropogenic reactive nitrogen per capita (kg N/cap/yr),
#'   and `framing`, the anthropogenic definition it was computed under, plus the
#'   polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_n_percapita(example = TRUE)
build_n_percapita <- function(
  n_inputs,
  population,
  framing = c("synthetic_bnf"),
  params = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_percapita())
  }
  framing <- rlang::arg_match(framing)
  params <- params %||% whep::n_boundary_params
  .check_columns(
    n_inputs,
    c("year", "area_code", "fert_type", "n_input_t"),
    "n_inputs"
  )
  .check_columns(population, c("year", "area_code", "population"), "population")
  n_inputs |>
    .n_percapita_anthropogenic(framing, params) |>
    .n_percapita_per_capita(population) |>
    dplyr::mutate(framing = .env$framing) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# The fert_type terms that make up the anthropogenic reactive-nitrogen total
# for a framing. "synthetic_bnf" is synthetic fertiliser plus biological
# fixation (the Campbell / Global new-reactive-N proxy).
.n_percapita_fert_types <- function(framing) {
  switch(
    framing,
    synthetic_bnf = c("synthetic", "bnf")
  )
}

# Sum the framing's fert_type terms to the country total anthropogenic reactive
# nitrogen (tonnes N) per (year, area_code), collapsing any finer grid key.
.n_percapita_anthropogenic <- function(n_inputs, framing, params) {
  fert_types <- .n_percapita_fert_types(framing)
  totals <- n_inputs |>
    dplyr::filter(.data$fert_type %in% fert_types) |>
    dplyr::summarise(
      n_input_t = .sum_if_any(.data$n_input_t),
      .by = c("year", "area_code", "fert_type")
    ) |>
    tidyr::pivot_wider(
      names_from = "fert_type",
      values_from = "n_input_t",
      values_fill = 0
    )
  for (term in fert_types) {
    if (!rlang::has_name(totals, term)) {
      totals[[term]] <- 0
    }
  }
  ratio <- .n_boundary_param(params, "syn_tot_agri_ratio")
  dplyr::transmute(
    totals,
    year = .data$year,
    area_code = .data$area_code,
    anthropogenic_n_t = .data$synthetic * ratio + .data$bnf
  )
}

# Divide the country total (converted tonnes N -> kg N) by population to give
# kg N/cap/yr. The inner join drops country-years lacking a population row (no
# per-capita denominator), matching build_n_boundary_percapita()'s own join.
.n_percapita_per_capita <- function(anthropogenic, population) {
  invalid <- dplyr::filter(
    population,
    !is.finite(.data$population) | .data$population <= 0
  )
  if (nrow(invalid) > 0L) {
    cli::cli_abort(
      "{.arg population} must contain finite, strictly positive denominators."
    )
  }
  anthropogenic |>
    dplyr::inner_join(
      dplyr::select(population, "year", "area_code", "population"),
      by = c("year", "area_code")
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      n_percapita_kg = .data$anthropogenic_n_t * 1000 / .data$population
    )
}

# Toy fixture for a runnable example: two countries' per-capita reactive N,
# carrying the same framing stamp the computed path emits.
.example_n_percapita <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~n_percapita_kg,
    ~framing,
    2000L,
    10L,
    8.5,
    "synthetic_bnf",
    2000L,
    20L,
    22,
    "synthetic_bnf"
  ) |>
    .add_reporting_polity_columns()
}
