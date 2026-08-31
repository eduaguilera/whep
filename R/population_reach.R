# Can a population source keyed on PRESENT-DAY ISO3 reach the territory a WHEP
# area code names?
#
# Every population source WHEP has is keyed that way. The `gdp-population` pin
# is an ISO3 column, and UN WPP publishes `Country/Area` rows carrying an ISO3
# code. A WHEP area code, by contrast, names a territory in the years it
# reported -- and some of those territories no longer exist, so no present-day
# ISO3 stands for them. Area 151 Netherlands Antilles is the case #787 is about:
# UN WPP 2024 carries no `ANT` record in any year, and it never will.
#
# The package already knows how to walk past a dissolved territory:
# `.successor_iso3_map()` resolves a polity to the present-day ISO3 codes of the
# states that replaced it, transitively, from the `successor` column the
# polities database publishes. `build_primary_production()` uses it for
# `federation_land = "successor_union"`. This file asks the same question of a
# population source and reports the answer per area code, so that "which areas
# can a denominator reach, and why not" is measured rather than remembered
# (#643, #644, #787).
#
# IT REPORTS THE REACH; IT DOES NOT BUILD THE DENOMINATOR, and the difference is
# deliberate. Measured against the `gdp-population` pin's own figure for the
# five dissolved aggregates the pin does cover, a successor sum over UN WPP 2024
# agrees to 0.05% for Czechoslovakia (51), 0.10% for Belgium-Luxembourg (15),
# 0.70% for Bechuanaland (20) and 1.54% for the USSR (228) -- and is 17.5% SHORT
# for the Yugoslav SFR (248).
#
# PART of that shortfall is Kosovo, and its mechanism is worth stating because
# it is not a missing edge. The walk stops a branch as soon as it lands in the
# vocabulary, which is right, and the branch lands on `SRB-2006-2008` -- whose
# name upstream is "Serbia (including Kosovo)" and whose `iso3_code` is `SRB`.
# WPP's `SRB` is Serbia EXCLUDING Kosovo, which the CSV publishes separately as
# `XKX`. So the walk returns a code that is correct as an identifier and 1.8-2.0
# million people short as a territory.
#
# Only part, though, and the split was measured for #863. In 1990 the pin's
# figure for area 248 is 26,487,416; the six ISO3 codes the walk reaches sum to
# 21,840,518 (the 17.5%); WPP's own `XKX` row adds 1,973,790, which closes 42%
# of the gap and no more. The remaining 2,673,108 -- 10.1% of the pin -- is the
# pin's Yugoslav level disagreeing with WPP's seven territories, and the other
# four aggregates agree to 1.5%, so it is specific to this series. Attributing
# the whole 17.5% to Kosovo, as this file used to, overstates it.
#
# One more hop would not repair the Kosovo half either, for two reasons that
# stack. `KOS-2008-2025` IS published as a successor of `SRB-2006-2008` -- so
# the edge exists, contrary to what this file used to say -- but it carries no
# `area_code` and calls itself `KOS`, not `XKX`; and `read_wpp_population()`
# drops `XKX` before any vocabulary is built from it, precisely because no
# numeric `area_code` resolves it. Kosovo therefore needs an `area_code`, which
# is crosswalk work, not walk work.
#
# The shape is general, and `.successor_code_reuse()` censuses it: ten polities
# upstream publishes are partitions whose parent keeps one part's ISO3. Sudan
# (1956-2011) carrying `SDN` is the largest by population and reads `"direct"`,
# where nothing hints at it at all -- `extent_exceeds_iso3` is the column that
# now says so.
#
# The relation is a TERRITORIAL succession, which is what land needs; it is not
# a partition of a population source's own geography, which is what a
# denominator needs, and the two coincide only where the source's ISO3 happens
# to cover the same ground as the polity's. Filling a denominator from it would
# replace an absence the package already warns about with a number wrong by an
# amount nobody would see.

#' Report which areas a present-day-ISO3 population source can reach.
#'
#' @description
#' Every population source WHEP reads is keyed on a present-day ISO3 code: the
#' `gdp-population` pin and UN WPP 2024 both are. A WHEP `area_code` names a
#' territory in the years it reported, and for a dissolved territory no
#' present-day ISO3 stands for it, so the source has no row and the area drops
#' out of every per-capita output (see [read_population()] and
#' [build_food_supply()]).
#'
#' This reports, for each reporting period of each area code, whether the
#' vocabulary in `iso3_codes` reaches it:
#'
#'  * `"direct"` — the period's own ISO3 is in the vocabulary.
#'  * `"successor"` — it is not, but the `successor` relation the polities
#'    database publishes leads, transitively, to ISO3 codes that are. Those
#'    codes are returned in `iso3_reached`.
#'  * `"unreachable"` — neither. No arrangement of the source's rows supplies
#'    this area.
#'
#' Measured against UN WPP 2024's 237 `Country/Area` ISO3 codes, 14 reporting
#' periods are not `"direct"` and five are `"unreachable"` — `ATF`, `PCN`,
#' `SGS`, `NFK` and `CXR`, every one of them inside the Rest-of-World bucket
#' 999. `ANT-1961-2010`, area 151 Netherlands Antilles, was the last
#' `"unreachable"` outside that bucket (#787); the 2026-08-25 upstream re-sync
#' gave it `CUW-2010-2025; SXM-2010-2025; BES-2010-2025`, so it now reads
#' `"successor"` by lookup rather than by anything hardcoded here.
#'
#' `"successor"` says the ISO3 codes exist, **not** that summing them is a safe
#' denominator. In general it is not: a successor sum for the Yugoslav SFR falls
#' 17.5% short of the `gdp-population` pin's own figure for the same aggregate,
#' of which Kosovo is 42% and a level disagreement between the pin and WPP on
#' that series is the rest. Use this to see what a source can cover, and read
#' the note at the top of `R/population_reach.R` before turning any of it into a
#' population.
#'
#' `extent_exceeds_iso3` marks the periods where the codes reported are
#' **narrower than the territory**, because the walk stopped on a polity that
#' reuses one of its own parts' ISO3 — `SRB-2006-2008` "Serbia (including
#' Kosovo)" carrying `SRB`, `SUD-1956-2011` carrying `SDN` (#863). It is
#' orthogonal to `reach`: a `"direct"` period can be flagged, and that is the
#' case worth knowing about, since nothing else about the row hints at it.
#'
#' Rows on `area_code` 999 describe the **members** of the Rest-of-World fold
#' bucket rather than the bucket itself, since each member is a crosswalk period
#' of its own.
#'
#' @param iso3_codes Character vector of the present-day ISO3 codes the
#'   population source publishes, e.g. `unique(read_wpp_population()$iso3c)`.
#' @param crosswalk Optional crosswalk overriding [polity_area_crosswalk], for
#'   testing.
#' @return A tibble with one row per `area_code` and `polity_code` reporting
#'   period, sorted by area code then first reported year: `area_code`,
#'   `polity_code`, `polity_name`, `map_year_start`, `map_year_end`,
#'   `own_iso3` (the period's own ISO3), `reach` (`"direct"` / `"successor"` /
#'   `"unreachable"`), `n_iso3`, `iso3_reached` (the ISO3 codes standing in
#'   for the area, `NA` when unreachable) and `extent_exceeds_iso3` (`TRUE`
#'   where those codes cover less ground than the period's territory).
#' @export
#' @examples
#' population_source_reach(c("BEL", "LUX", "CZE", "SVK", "CUW"))
population_source_reach <- function(iso3_codes, crosswalk = NULL) {
  if (!is.character(iso3_codes) || length(iso3_codes) == 0L) {
    cli::cli_abort(
      "{.arg iso3_codes} must be a non-empty character vector of ISO3 codes.",
      class = "whep_bad_iso3_vocabulary"
    )
  }
  available <- unique(iso3_codes[!is.na(iso3_codes)])
  periods <- .reporting_periods(crosswalk)
  stops <- .successor_stop_map(periods$polity_code, available)
  iso3 <- .polity_iso3_lookup()
  reached <- purrr::map(stops, \(nodes) sort(unique(unname(iso3[nodes]))))
  narrower <- .successor_code_reuse()$polity_code
  periods |>
    dplyr::mutate(
      # The walk stops a branch as soon as it lands in the vocabulary, so a
      # period whose own ISO3 is available resolves to itself. That single code
      # is the answer for a direct reach, and the successor codes are the answer
      # for the rest.
      n_iso3 = as.integer(lengths(reached[.data$polity_code])),
      iso3_reached = .reach_iso3_label(.data$polity_code, reached),
      reach = dplyr::case_when(
        !is.na(.data$own_iso3) & .data$own_iso3 %in% available ~ "direct",
        .data$n_iso3 > 0L ~ "successor",
        .default = "unreachable"
      ),
      # Reached is not the same extent: a stop on a polity that reuses one of
      # its own parts' ISO3 answers with a code narrower than the territory,
      # and on the `"direct"` branch it looks certain (#863).
      extent_exceeds_iso3 = purrr::map_lgl(
        stops[.data$polity_code],
        \(nodes) any(nodes %in% narrower)
      )
    ) |>
    dplyr::relocate("reach", .before = "n_iso3") |>
    dplyr::arrange(.data$area_code, .data$map_year_start, .data$polity_code)
}

# ---- Private helpers -------------------------------------------------------

# One row per (area code, polity) period the crosswalk declares REPORTING years
# for. `map_year_start` is the authority on when an area reports under a period
# (see `.polity_join_end_year()`), and a period nothing reports under cannot
# want a denominator, so crosswalk rows with no map span are dropped. One pair
# carries two spans; take the union, so the output key stays
# (area_code, polity_code).
.reporting_periods <- function(crosswalk = NULL) {
  (crosswalk %||% polity_area_crosswalk) |>
    tibble::as_tibble() |>
    dplyr::filter(
      !is.na(.data$polity_area_code),
      !is.na(.data$polity_code),
      !is.na(.data$map_year_start)
    ) |>
    dplyr::mutate(area_code = as.integer(.data$polity_area_code)) |>
    dplyr::summarise(
      polity_name = dplyr::first(.data$polity_name),
      own_iso3 = as.character(dplyr::first(.data$iso3_code)),
      map_year_start = as.integer(min(.data$map_year_start)),
      map_year_end = as.integer(max(.data$map_year_end)),
      .by = c("area_code", "polity_code")
    ) |>
    dplyr::relocate("area_code", "polity_code", "polity_name")
}

.reach_iso3_label <- function(polity_code, reached) {
  label <- vapply(
    polity_code,
    \(code) paste(reached[[code]], collapse = ", "),
    character(1),
    USE.NAMES = FALSE
  )
  dplyr::na_if(label, "")
}
