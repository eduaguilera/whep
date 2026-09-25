# Synthetic 1890-1910 seam fixture ---------------------------------------------
#
# The "reduction" fixture the plan's seam section and T29's synthetic leg
# need (plans/2026-09-01-subnational-spatialization.md, T28 and T29): one
# container, three units, extents that move in opposite directions, and
# observed shares that are EXACTLY proportional to the extent at every
# observed year.
#
# That proportionality is what makes two invariants checkable at once:
#
#   - the pure-LUH2 reduction -- with `s_v(t0)` proportional to `E_v(t0)`,
#     `s_u(t0) * E_u(t) / E_u(t0)` renormalised is `E_u(t) / sum_v E_v(t)`,
#     the share vector LUH2 alone would give;
#   - seam-agnosticism -- moving `t0` then moves only `treatment`, because
#     every anchor year yields the same back-cast vector.
#
# Extents in hectares, with k counting years from the first: S1 rises by
# five a year from 100 to 200, S2 falls by five a year from 300 to 200,
# and S3 holds at 200 throughout. The container total is therefore 600 in
# every year and no unit is ever zero.
# Observations start at 1900, leaving 1890:1899 for the back-cast.

.seam1890_years <- function() {
  1890:1910
}

.seam1890_first_observed <- function() {
  1900L
}

#' The three-unit extent table, in [aggregate_unit_extent()]'s shape.
#'
#' @return A tibble: `area_code`, `level_polity_code`, `level`, `year`,
#'   `extent_ha`, `extent_basis`.
#' @noRd
.seam1890_extent <- function() {
  years <- .seam1890_years()
  k <- years - min(years)
  tibble::tibble(
    area_code = 910L,
    level_polity_code = rep(c("S1", "S2", "S3"), each = length(years)),
    level = 1L,
    year = rep(as.integer(years), times = 3),
    extent_ha = c(100 + 5 * k, 300 - 5 * k, rep(200, length(years))),
    extent_basis = "cropland_ha"
  )
}

#' The observed shares from `.seam1890_first_observed()` onwards.
#'
#' Each unit's share is its own extent over the container's, so the
#' observed vector is the pure-extent vector by construction.
#'
#' @return A tibble in the [admin_shares_schema()] column set.
#' @noRd
.seam1890_shares <- function() {
  .seam1890_extent() |>
    dplyr::filter(year >= .seam1890_first_observed()) |>
    dplyr::mutate(share = extent_ha / sum(extent_ha), .by = year) |>
    dplyr::mutate(
      item_prod_code = 15L,
      indicator_used = "area_harvested",
      value = extent_ha,
      source = "fixture",
      tier = 1L,
      grain = "admin1",
      concept_break = FALSE,
      nuts_version = NA_character_,
      source_native_id = level_polity_code,
      source_native_name = level_polity_code,
      source_id = "fixture",
      source_version = NA_character_,
      recorded_at = "2026-01-01T00:00:00Z",
      treatment_year = "observed",
      value_flag = NA_character_
    ) |>
    dplyr::select(
      area_code,
      level_polity_code,
      level,
      item_prod_code,
      indicator_used,
      year,
      value,
      share,
      source,
      tier,
      grain,
      concept_break,
      nuts_version,
      source_native_id,
      source_native_name,
      source_id,
      source_version,
      recorded_at,
      treatment_year,
      value_flag
    )
}

#' The share vector the extent alone implies, for every year.
#'
#' The target of the pure-LUH2 reduction invariant.
#'
#' @return A tibble: `level_polity_code`, `year`, `luh2_share`.
#' @noRd
.seam1890_luh2_shares <- function() {
  .seam1890_extent() |>
    dplyr::mutate(luh2_share = extent_ha / sum(extent_ha), .by = year) |>
    dplyr::select(level_polity_code, year, luh2_share)
}
