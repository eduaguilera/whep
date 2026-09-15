# Level-1 (granted-depth) fixtures ----------------------------------------------
#
# The synthetic granted-depth fixture every later engine task (T07, T12, T13,
# T15b, T17, T28) develops against, per
# plans/2026-09-01-subnational-spatialization.md T37. Two countries:
#
#   - A (area_code 900L): granted depth 1. No level-0 row for A itself is
#     emitted -- decision 10 keys a granted-depth compartment on the
#     CONTAINER's area_code plus level_polity_code, never on a container-only
#     row. Two level-1 units:
#       - A1 "A-A1-1900-2100": full-series edge, three cells (one held alone,
#         one straddled with A2 and bordered by B, one held alone with no
#         crop-pattern row for item 44 -- the "zero-pattern cell" T31(b)
#         needs evidence for).
#       - A2 "A-A2-1975-2100": edge starts mid-series at 1975 (T07's temporal
#         test fixture), two cells (the straddled one, one held alone).
#   - B (area_code 901L): granted depth 0 (today's default), two cells, one
#     of which is the same physical cell A1/A2 straddle -- the "A1/A2/B
#     border cell" the plan asks for, satisfied by three compartments in one
#     physical cell rather than a fourth cell.
#
# Six physical cells, eight country_grid rows (the straddling cell carries
# three compartments). `cell_id` follows `.pcs_cell_id()`
# (R/polycell_support.R:956): `(klon + 360) * 1000 + (klat + 180)` with
# `klon = (lon - 0.25) / 0.5`, `klat = (lat - 0.25) / 0.5`. It is used only to
# build `polycell_id` and is not itself a kept column, matching the plan's
# column list for `.level1_country_grid()`.
#
# Physical cells (lon, lat -> cell_id):
#   cell 1 (10.25, 40.25) -> 380260 : A1 alone
#   cell 2 (10.75, 40.25) -> 381260 : A1 + A2 + B (straddle + border)
#   cell 3 (11.25, 40.25) -> 382260 : A1 alone, no item 44 pattern
#   cell 4 (11.75, 40.25) -> 383260 : A2 alone
#   cell 5 (12.25, 40.25) -> 384260 : B alone
#   cell 6 (10.25, 40.75) -> 380261 : A2 alone (second cell)
#
# `polycell_id` follows the `paste0(polity_code, "@", cell_id)` convention at
# `R/polycell_support.R:1562`, using `level_polity_code %||% area_code` in
# place of `polity_code` (a level-0 row has no `level_polity_code`);
# implemented with `dplyr::coalesce()` because `%||%` is not vectorised over
# a column.
#
# Consumers (plan Execution DAG): T07 (edge-validity gate), T12
# (level-aware loading + decision-10 assertions (a) and (b)), T13 (regime
# recording), T15b, T17, T28 (seam formula core).

#' The level-1 synthetic country grid (decision 10 shape).
#'
#' @return A tibble: `lon`, `lat`, `area_code` (integer, the CONTAINER code:
#'   900L for every A1/A2 row, 901L for B), `level_polity_code` (character,
#'   `NA` at level 0), `level` (integer, `0L` or `1L`), `cell_area_frac`
#'   (double, this compartment's share of the physical cell on the land
#'   basis), `polycell_id` (character), `start_year`, `end_year` (integer,
#'   the compartment's edge-validity interval; start inclusive, end exclusive
#'   at a succession / inclusive at the open end, the
#'   `.filter_country_grid_year()` convention,
#'   R/spatialize_compartments.R:324-373).
#' @noRd
.level1_country_grid <- function() {
  raw <- tibble::tribble(
    ~lon,   ~lat,   ~area_code, ~level_polity_code, ~level, ~cell_area_frac, ~cell_id, ~start_year, ~end_year,
    10.25, 40.25,         900L, "A-A1-1900-2100",       1L,             1.0,   380260L,       1900L,      2100L,
    10.75, 40.25,         900L, "A-A1-1900-2100",       1L,             0.3,   381260L,       1900L,      2100L,
    10.75, 40.25,         900L, "A-A2-1975-2100",       1L,             0.5,   381260L,       1975L,      2100L,
    10.75, 40.25,         901L, NA_character_,          0L,             0.2,   381260L,       1850L,      2100L,
    11.25, 40.25,         900L, "A-A1-1900-2100",       1L,             1.0,   382260L,       1900L,      2100L,
    11.75, 40.25,         900L, "A-A2-1975-2100",       1L,             1.0,   383260L,       1975L,      2100L,
    12.25, 40.25,         901L, NA_character_,          0L,             1.0,   384260L,       1850L,      2100L,
    10.25, 40.75,         900L, "A-A2-1975-2100",       1L,             1.0,   380261L,       1975L,      2100L
  )
  raw |>
    dplyr::mutate(
      polycell_id = paste0(
        dplyr::coalesce(level_polity_code, as.character(area_code)),
        "@",
        cell_id
      )
    ) |>
    dplyr::select(
      lon,
      lat,
      area_code,
      level_polity_code,
      level,
      cell_area_frac,
      polycell_id,
      start_year,
      end_year
    )
}

#' Country A's level-0 share of each physical cell it touches.
#'
#' The level-0 share a `level = 0L` A row WOULD have carried, kept apart
#' because decision 10 forbids emitting one ("for every granted country, no
#' container-keyed row exists anywhere", plan Architecture section
#' "Compartment key, allocation layer, output grain (decision 10)"). Used to
#' check assertion (b): A1 + A2's `cell_area_frac` sum to THIS value per
#' cell, not to 1 -- cell (10.75, 40.25) also carries B's 0.2, so A's own
#' share there is 0.8, not 1.
#'
#' @return A tibble: `lon`, `lat`, `area_code` (900L throughout),
#'   `cell_area_frac`.
#' @noRd
.level1_level0_shares <- function() {
  tibble::tribble(
    ~lon,   ~lat,   ~area_code, ~cell_area_frac,
    10.25, 40.25,         900L,             1.0,
    10.75, 40.25,         900L,             0.8,
    11.25, 40.25,         900L,             1.0,
    11.75, 40.25,         900L,             1.0,
    10.25, 40.75,         900L,             1.0
  )
}

#' Level-1 fixture crop patterns (Monfreda shape, two crops).
#'
#' Item 44's pattern is ABSENT at cell 3 (11.25, 40.25) -- the zero-pattern
#' cell A1 owns with reported area but no positive Monfreda cell, the T31(b)
#' evidence case (plan Phase 0, T18a note on the zero-pattern-unit count).
#'
#' @return A tibble: `lon`, `lat`, `item_prod_code`, `harvest_fraction`.
#' @noRd
.level1_crop_patterns <- function() {
  tibble::tribble(
    ~lon,   ~lat,   ~item_prod_code, ~harvest_fraction,
    10.25, 40.25,               15L,               0.6,
    10.25, 40.25,               44L,               0.2,
    10.75, 40.25,               15L,               0.5,
    10.75, 40.25,               44L,               0.3,
    11.25, 40.25,               15L,               0.7,
    11.75, 40.25,               15L,               0.4,
    11.75, 40.25,               44L,               0.5,
    12.25, 40.25,               15L,               0.3,
    12.25, 40.25,               44L,               0.2,
    10.25, 40.75,               15L,               0.5,
    10.25, 40.75,               44L,               0.3
  )
}

#' Level-1 fixture gridded cropland extent, 3 years straddling A2's edge.
#'
#' Years 1974:1976 bracket A2's edge start (1975), matching
#' `.level1_country_grid()` and `.level1_admin_shares()`.
#'
#' @return A tibble: `lon`, `lat`, `year`, `cropland_ha`, in the column
#'   shape [build_gridded_landuse()]'s `gridded_cropland` argument reads.
#' @noRd
.level1_gridded_cropland <- function() {
  cells <- tibble::tribble(
    ~lon,   ~lat,   ~cropland_ha,
    10.25, 40.25,           1000,
    10.75, 40.25,           1200,
    11.25, 40.25,            800,
    11.75, 40.25,            900,
    12.25, 40.25,           1500,
    10.25, 40.75,            700
  )
  tidyr::crossing(cells, year = c(1974L, 1975L, 1976L)) |>
    dplyr::select(lon, lat, year, cropland_ha)
}

#' Level-1 fixture gridded pasture extent, 3 years straddling A2's edge.
#'
#' @return A tibble: `lon`, `lat`, `year`, `pasture_ha`, `rangeland_ha`, in
#'   the column shape [build_gridded_livestock()]'s `gridded_pasture`
#'   argument reads.
#' @noRd
.level1_gridded_pasture <- function() {
  cells <- tibble::tribble(
    ~lon,   ~lat,   ~pasture_ha, ~rangeland_ha,
    10.25, 40.25,           200,           100,
    10.75, 40.25,           150,           250,
    11.25, 40.25,           100,            50,
    11.75, 40.25,           180,           120,
    12.25, 40.25,           220,            80,
    10.25, 40.75,            90,            60
  )
  tidyr::crossing(cells, year = c(1974L, 1975L, 1976L)) |>
    dplyr::select(lon, lat, year, pasture_ha, rangeland_ha)
}

#' National harvested-area totals [build_gridded_landuse()] takes.
#'
#' @return A tibble: `year`, `area_code`, `item_prod_code`,
#'   `harvested_area_ha`, in the shape `build_gridded_landuse()`'s
#'   `country_areas` argument reads (R/spatialize.R roxygen).
#' @noRd
.level1_country_areas <- function() {
  tibble::tribble(
    ~year,  ~area_code, ~item_prod_code, ~harvested_area_ha,
    1974L,        900L,             15L,               1000,
    1974L,        900L,             44L,                400,
    1974L,        901L,             15L,                300,
    1974L,        901L,             44L,                150,
    1975L,        900L,             15L,               1050,
    1975L,        900L,             44L,                420,
    1975L,        901L,             15L,                310,
    1975L,        901L,             44L,                155,
    1976L,        900L,             15L,               1100,
    1976L,        900L,             44L,                440,
    1976L,        901L,             15L,                320,
    1976L,        901L,             44L,                160
  )
}

#' The T33-schema admin-shares fixture for container A, coverage change 1975.
#'
#' A1 reports alone in 1974; A2's edge starts 1975
#' (`.level1_country_grid()`'s `"A-A2-1975-2100"`), so 1975-1976 carry two
#' reporting units per crop -- the coverage-change year T25's seam list has
#' to see. `share` is COMPUTED per sibling group, not hand-typed, so it is
#' exact rather than rounded, and the schema is checked, not assumed: see
#' `tests/testthat/test_helper_level1_grid.R`
#' (`whep:::ensure_admin_shares()`).
#'
#' @return A tibble conforming to `admin_shares_schema()` (R/admin_shares.R).
#' @noRd
.level1_admin_shares <- function() {
  raw <- tibble::tribble(
    ~year,  ~level_polity_code, ~item_prod_code, ~value,
    1974L,  "A-A1-1900-2100",               15L,     600,
    1974L,  "A-A1-1900-2100",               44L,     250,
    1975L,  "A-A1-1900-2100",               15L,     650,
    1975L,  "A-A2-1975-2100",               15L,     450,
    1975L,  "A-A1-1900-2100",               44L,     270,
    1975L,  "A-A2-1975-2100",               44L,     180,
    1976L,  "A-A1-1900-2100",               15L,     680,
    1976L,  "A-A2-1975-2100",               15L,     470,
    1976L,  "A-A1-1900-2100",               44L,     280,
    1976L,  "A-A2-1975-2100",               44L,     190
  )

  raw |>
    dplyr::mutate(
      area_code = 900L,
      level = 1L,
      indicator_used = "area_harvested",
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
    dplyr::mutate(
      share = value / sum(value),
      .by = c(area_code, level, item_prod_code, indicator_used, year)
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

#' Toy example: the level-1 country grid only (~8 rows).
#'
#' @return See [.level1_country_grid()].
#' @noRd
# dispatcher: move to R/toy_examples.R
.example_level1_country_grid <- function() {
  .level1_country_grid()
}
