# Allocation at a containment depth (whep#1000 T12 and T13), and the two
# level-0 support vintages (whep#1000 T39).
#
# FOR THE INTEGRATION PASS, T39's part of this file adds NO new export, NO new
# NSE symbol (every added helper uses the `.data` pronoun) and NO new
# `R/toy_examples.R` fixture -- its `@examples` addition runs the real reader
# on the inline `tibble` already in the block, offline and in milliseconds.
# `devtools::document()` must still be re-run: the roxygen of
# `read_level_country_grid()` here and of `run_spatialize()` in
# R/run_spatialize.R both changed, so `man/read_level_country_grid.Rd` and
# `man/run_spatialize.Rd` are stale. `_pkgdown.yml` needs no entry.
#
# FOR THE INTEGRATION PASS -- NSE symbols this file needs in the
# `utils::globalVariables()` block at the end of `R/utils.R`, none of which is
# declared yet (verified with `lintr::object_usage_linter()`):
#
#   admin_sum, allocated_ha, basis, candidate, candidate_share, cell_ha,
#   cropland_irrigated_ha, cropland_rainfed_ha, denominator, difference_frac,
#   difference_ha, discrepancy_frac, discrepancy_ha, irrigated_raw_ha,
#   irrigated_target_ha, irrigation_basis, irrigation_clipped_ha, is_reporter,
#   land_area, method_crop_alloc, n_areas_here, n_cells, n_units_here,
#   n_units_reporting, national_total_ha, reports, reports_value,
#   residual_share, share_reported, shares_foreign, shares_sibling, target_ha,
#   weight_area, weight_basis, weight_irrigated, weight_rainfed
#
# FOR THE INTEGRATION PASS -- the pre-PR review fixes add NO further NSE
# symbol beyond the list above: every helper they introduce reads its columns
# through the `.data` pronoun (`dropped_ha`, `claimed_share`) or through
# string-valued `.by` / `dplyr::select()` keys (`epoch`).
#
# `allocate_level_crops()` and `build_level_crop_targets()` are new exports and
# need NAMESPACE, `man/` and a `_pkgdown.yml` reference entry. Both examples
# run the real functions on inline `tibble::tribble()` fixtures in a fraction
# of a second, with no pin, no network and no `WHEP_*` path, so neither needs
# an `example = FALSE` fixture in `R/toy_examples.R`.
#
# THE KEY THIS FILE IS BUILT ON is the composite `(area_code,
# level_polity_code)`: the container's reporting code plus the unit's polity
# code, `NA` at level 0. `allocate_level_crops()`'s roxygen states in full
# where it is carried, where the engine had to learn it, and the two places
# that keep the container key on purpose. Read that section before changing
# any grouping here or in `R/spatialize.R`.

#' Read the cell-to-polity country grid at a containment depth
#'
#' @description
#' The spatialization engines allocate a national total into the compartments
#' of a *country grid*: one row per physical 0.5-degree cell and territorial
#' unit, carrying that unit's share of the cell. `level = 0L` is today's grid,
#' one row per cell and reporting `area_code`. `level >= 1L` resolves the
#' polycell support one containment step deeper, so a cell held by a province
#' arrives as a province-keyed row instead of being folded into its country.
#'
#' Under `grid_vintage = "snapshot_2015"` level 0 is served by the unchanged
#' `.read_polycell_country_grid()` path, so such a run is bit-for-bit what it
#' was. Deeper levels bypass that path entirely -- in particular they never
#' reach `.carbon_support_to_area_code()`, whose fold is what deletes a
#' province.
#'
#' @section Which vintage of the support level 0 is read at:
#' The support is a table of cell-to-polity claims with validity intervals, so
#' "which polity holds this cell" has an answer per year. Two answers are
#' selectable and neither is ever a fallback for the other:
#'
#' \describe{
#'   \item{`"snapshot_2015"`}{One reference year, `.carbon_support_year()`,
#'     folded onto reporting `area_code`s -- the carbon path's DA-28 / whep#549
#'     choice that migrating the territorial EXTENT does not migrate the
#'     ATTRIBUTION. Every year of a run is allocated into the SAME present-day
#'     geography.}
#'   \item{`"year_aware"`}{The support in its own interval grain, keyed and
#'     folded per epoch, so `.filter_country_grid_year()` selects the polities
#'     valid in each simulation year -- the same treatment a granted depth
#'     gets. A cell held by a polity that has no reporting code in that year,
#'     or by no polity at all, is then absent for that year.}
#' }
#'
#' The two are different geographies, not two precisions of one, and the
#' difference is large before about 1990: on the `20260825T102349Z-1a0eb`
#' support the year-aware read covers 41,597 cells and 7,942 Mha of land at
#' 1851 against the snapshot's 66,709 cells and 12,926 Mha. Of that year's
#' LUH2 cropland, 41.6% is taken away from the country the snapshot gives it
#' to and 21.5% is given to a country the snapshot does not -- the two
#' directions are reported separately, because a metric that scores only the
#' losses cannot see a share that absorbs one country's land into another.
#' 101.8 Mha of the loss falls in cells no polity claims in 1851 and is
#' attributed to nobody. At 2015 the two vintages agree exactly, which is the
#' check that fails if the year-aware share is renormalised.
#'
#' Those figures are the `20260825T102349Z-1a0eb` support's, not the
#' `20260827T190201Z-f82a2` one's: that later pin books every hectare of
#' inland water and ice as land (whep#1010), which moves the cell count and
#' the land, though not the argument.
#' `run_spatialize()` records the resolved value in `run_metadata.yaml` and in
#' a `method_grid_vintage` column on every output it writes.
#'
#' `"snapshot_2015"` is still the default, against whep#1000 T31(j)'s stated
#' preference, because WHEP's national tables are on a **constant-territory**
#' basis and the support is on a **historical-polity** one. In 1961
#' `country_areas` reports the Russian Federation, Kazakhstan and Ukraine
#' where the year-aware grid offers only the USSR, so 17.2% of the world's
#' harvested area has no cell to land in and is dropped whole;
#' `validation/spatialize_grid_vintage.R` measures it per year. Reconciling
#' the two bases is a lineage step on the national side, not a grid setting.
#'
#' @section Which rows count as a level:
#' A polity is at depth *d* when the containment edge ([polity_containment])
#' places it *d* steps inside a container that is not itself contained. An edge
#' is admitted only when its container resolves, through
#' [polity_area_crosswalk], to a single reporting `area_code` **and** is not of
#' `polity_type` `"aggregate"`. An aggregate's reporting code is a matrix
#' *bucket*: `999` alone pools 62 territories, so filing a province under one
#' would attribute it to Rest of World rather than to a country. Dropped edges
#' are counted and named, never skipped in silence.
#'
#' @section How the share of the cell is measured:
#' `cell_area_frac` is the unit's share of the **physical cell on the land
#' basis** -- `land_area_ha / cell_land_ha`, the same basis
#' `.carbon_attach_land_share()` uses at level 0, with the cell's whole
#' measured land as the denominator. Everything the fraction later splits
#' (LUH2 class areas, crop-pattern hectares) is already land-only, so dividing
#' by the whole cell would subtract the water twice. Because the support is
#' read in its interval grain, the denominator is evaluated per interval-start
#' year: the cell's land partition genuinely differs between epochs, and taking
#' one denominator across all of them would count a cell once per epoch.
#'
#' Two support shapes are recognised, and which one is in hand is **detected,
#' never assumed**:
#'
#' \describe{
#'   \item{replacement}{The support carries the units *instead of* their
#'     container (the shape a level-tagged pin delivers). `land_area_ha` is
#'     already the unit's absolute land, so the share is taken directly. A
#'     container row surviving beside its own members in one cell is refused:
#'     both claim the same ground, and choosing one silently is exactly the
#'     double count this epic exists to remove.}
#'   \item{nested}{The support carries a within-container share column
#'     (`container_area_frac`, `container_frac` or `parent_frac`) alongside the
#'     container's own row. The unit's share of the cell is then the
#'     composition `f_unit x f_container`, with `f_container` measured from the
#'     container's own row on the same land basis.}
#' }
#'
#' @param level Containment depth, a non-negative whole number. `0L` (default)
#'   is today's cell-to-`area_code` grid.
#' @param support Polycell support table in the [build_polycell_support()]
#'   grain, overriding [read_polycell_support()]. Not read at `level = 0L`
#'   with `grid_vintage = "snapshot_2015"`, which resolves the support itself.
#' @param containment Containment edge table in the [polity_containment]
#'   schema, overriding the packaged one. Only read at `level >= 1L`.
#' @param reference_year Optional year to snapshot the grid at, applied with
#'   the package's own validity predicate. `NULL` (default) keeps the interval
#'   grain, which is what the engines' per-year filter expects. Not read at
#'   `level = 0L` with `grid_vintage = "snapshot_2015"`, whose reference year
#'   is fixed at `.carbon_support_year()`.
#' @param grid_vintage Which vintage of the cell-to-polity support a level-0
#'   grid is read at, `"snapshot_2015"` (default) or `"year_aware"`. See
#'   *Which vintage of the support level 0 is read at*, which states why the
#'   snapshot is still the default. Not read at `level >= 1L`, which is
#'   year-aware by construction; snapshot a depth with `reference_year`.
#' @param containers Optional integer vector of container reporting
#'   `area_code`s the depth read is scoped to -- the containers a run grants a
#'   depth. `NULL` (default) reads every admitted edge. Only read at
#'   `level >= 1L`. See *Which containers a depth is read for*.
#' @param double_claim Which clashes the double-claim gate refuses, one of
#'   `"co_presence"` (default) or `"measured"`. See *Which double claims are
#'   refused*. Only read at `level >= 1L`.
#'
#' @section Which double claims are refused:
#' A support that carries a container's own row beside its units' rows in one
#' cell claims that ground twice: `cell_land_ha` is the sum over the cell, so
#' every unit's share is divided by a total counting the same ground twice,
#' and nothing downstream can see it because the container's national total
#' still reconciles. The two rules are alternatives, never a fallback.
#'
#' \describe{
#'   \item{`"co_presence"`}{The default, and fail-closed: a container row
#'     found beside its own units is refused, full stop. It is the strict
#'     rule because this table carries no geometry -- whether the two
#'     polygons overlap cannot be read off it.}
#'   \item{`"measured"`}{Refuses only a container the cell's own area
#'     REFUTES: at least one shared cell-epoch whose whole claimed territory
#'     exceeds `cell_area_ha`. A cell cannot hold more territory than it has,
#'     so an excess is proof; but the proof is ONE-SIDED, which is why this
#'     is the weaker rule and not the default. A cell half of which is ocean
#'     can hold a duplicate under its own area and show no excess. Every
#'     clash it passes over is warned about by container and cell-epoch
#'     count.}
#' }
#'
#' On the `20260827T190201Z-f82a2` support the measurement separates the two
#' cleanly: over the ten containers the shipped edge table admits at depth 1,
#' 3,285 of 3,965 clashing cell-epochs over-claim by 529.03 Mha in total, and
#' the whole of that sits in eight containers whose units are genuinely
#' nested -- Alaska inside the USA alone over-claims 150 Mha over 1,156 of its
#' 1,372 shared cells. The two that over-claim nothing are Ryukyu inside Japan
#' 1895-1945 (8 cells, 0 Mha) and Singapore inside Malaysia (1 cell, 0 Mha),
#' whose ground in the shared cell is disjoint. The decision is taken per
#' CONTAINER rather than per cell for that reason: a nested container's
#' coastal cells hide their own duplicate, while its interior cells prove the
#' nesting outright.
#'
#' @section Which containers a depth is read for:
#' Without `containers`, every edge [polity_containment] admits at this depth
#' is read, whoever the run is for. On the shipped edge table that is ten
#' containers with support rows -- Alaska inside the USA, three 1949
#' Indonesian units, Manchuria inside three Chinese epochs, Serbia inside
#' three Yugoslav ones, Singapore inside Malaysia -- and each of them is
#' carried in the polycell support BESIDE its own units, so
#' `.level_check_no_double_claim()` refuses the read. The refusal is right:
#' continuing would divide every unit's share by a cell land total that
#' counts the same ground twice. It is simply not the Japanese run's
#' business.
#'
#' `containers` scopes the edge set to the containers asked for, and that is
#' all it does. **Scoping is not suppressing**: the double-claim gate, the
#' aggregate-container refusal and the support-empty abort all still run, on
#' the scoped edges, so a granted container whose own row survives beside its
#' units aborts exactly as before. A container named here that no edge places
#' at this depth aborts with class `whep_level_container_not_admitted` rather
#' than returning a grid that silently lacks it.
#'
#' @return A `tibble` with `lon`, `lat`, `area_code` (integer, the container's
#'   reporting code), `level_polity_code` (character, `NA` at level 0),
#'   `level` (integer), `cell_area_frac`, `polycell_id`, `start_year`,
#'   `end_year`, `cell_area_ha` and `land_area_ha`. At level 0 with
#'   `grid_vintage = "snapshot_2015"` the six columns `.carbon_cell_support()`
#'   returns are passed through unchanged; with `"year_aware"` those six
#'   arrive alongside `start_year` and `end_year`.
#'
#' @seealso [build_allocation_layer()], [read_polycell_support()].
#' @export
#'
#' @examples
#' # Offline: a two-cell support for one Japanese prefecture, with the
#' # containment edge that puts it inside Japan. No pin and no network.
#' support <- tibble::tribble(
#'   ~polycell_id, ~cell_id, ~lon, ~lat, ~polity_code,
#'   "JPN-AICHI-1871-2025@1", 1L, 137.25, 35.25, "JPN-AICHI-1871-2025",
#'   "JPN-AICHI-1871-2025@2", 2L, 137.75, 35.25, "JPN-AICHI-1871-2025"
#' ) |>
#'   dplyr::mutate(
#'     area_code = 110L,
#'     start_year = 1952L,
#'     end_year = 2025L,
#'     cell_area_ha = 3000,
#'     land_area_ha = c(1200, 2000)
#'   )
#' containment <- tibble::tibble(
#'   member_code = "JPN-AICHI-1871-2025",
#'   container_code = "JPN-1952-2025",
#'   start_year = 1952L,
#'   end_year = 2025L,
#'   basis = "prefecture inside JPN-1952-2025 for those years"
#' )
#' read_level_country_grid(
#'   level = 1L,
#'   support = support,
#'   containment = containment
#' )
#'
#' # The same read scoped to the containers a run grants a depth. Japan's
#' # reporting code is 110, so the edge above is kept and any other
#' # container's is left at level 0.
#' read_level_country_grid(
#'   level = 1L,
#'   support = support,
#'   containment = containment,
#'   containers = 110L
#' )
#'
#' # The same support read at level 0. The default vintage is the 2015
#' # snapshot, which resolves the support itself and refuses one; the
#' # year-aware read takes this one and keeps its validity intervals.
#' read_level_country_grid(
#'   level = 0L,
#'   support = support,
#'   grid_vintage = "year_aware"
#' )
read_level_country_grid <- function(
  level = 0L,
  support = NULL,
  containment = NULL,
  reference_year = NULL,
  grid_vintage = c("snapshot_2015", "year_aware"),
  containers = NULL,
  double_claim = c("co_presence", "measured")
) {
  level <- .check_grid_level(level)
  containers <- .level_check_containers(containers)
  double_claim <- rlang::arg_match(double_claim)
  # Whether the caller SUPPLIED a vintage, which `arg_match()` cannot say once
  # it has resolved one: the unevaluated default is the whole vocabulary, so
  # its length is the question. Only used to decide whether a depth read has
  # something to report, never to change what it returns.
  vintage_given <- length(grid_vintage) == 1L
  grid_vintage <- rlang::arg_match(grid_vintage)
  if (level == 0L) {
    .refuse_level0_containers(containers)
    return(.level0_country_grid(
      support,
      containment,
      reference_year,
      grid_vintage
    ))
  }
  .inform_deep_vintage(level, vintage_given)
  support <- support %||% read_polycell_support()
  support <- .level_support_intervals(tibble::as_tibble(support))
  containment <- tibble::as_tibble(containment %||% whep::polity_containment)
  edges <- .level_scope_containers(
    .level_admit_edges(containment, level),
    containers
  )
  units <- .level_support_units(support, edges, level)
  grid <- .level_attach_cell_share(units, support, double_claim)
  if (!is.null(reference_year)) {
    grid <- .filter_country_grid_year(grid, as.integer(reference_year))
  }
  cli::cli_alert_info(
    "country_grid: polycell support at level {level}, {nrow(grid)} \\
     compartment{?s} over \\
     {dplyr::n_distinct(paste(grid$lon, grid$lat))} cell{?s} in \\
     {dplyr::n_distinct(grid$area_code)} container{?s}."
  )
  grid
}

#' Choose an allocation depth per country and assert no cell is claimed twice
#'
#' @description
#' Decision 10's allocation layer: ONE country grid in which each country is
#' represented at exactly one depth. A container listed in `granted` arrives as
#' its granted-depth units and as nothing else; every other country arrives as
#' its level-0 row. The engines see a single grid and need no depth logic.
#'
#' @section The two-part assertion:
#' \describe{
#'   \item{(a), a one-sided abort}{The shares of one physical cell may never
#'     EXCEED 1, within `1e-8`, **at any one year**: the sum is taken over the
#'     rows valid together, once per epoch the layer carries, and a layer with
#'     no validity intervals is one epoch and therefore the unconditional sum
#'     T37 agreed on. Summing ACROSS epochs was the defect: two successive
#'     polities each hold their cell whole and never coexist, so every
#'     succession of the year-aware level-0 read -- the USSR, Yugoslavia,
#'     Sudan, Czechoslovakia -- summed to 2 and no year-aware grid could be
#'     combined with a granted depth. A sum above 1 is ground claimed twice --
#'     a container kept beside its own units -- and is invisible downstream
#'     because every national total still reconciles, so it aborts with class
#'     `whep_alloc_layer_not_partition`. A sum BELOW 1 is not an error: it is
#'     land inside the cell that no reporting polity claims, the same
#'     unclaimed ground the year-aware level-0 read leaves out, and it is
#'     reported rather than refused (see the unclaimed-land section). A
#'     compartment holding two different shares of one cell IN ONE EPOCH is a
#'     contradiction rather than a share that moves, and is refused with class
#'     `whep_alloc_layer_varying_share`; a share that differs between two
#'     DISJOINT intervals of one compartment is now read at each of them,
#'     which is what the year-aware level-0 read produces on real data.}
#'   \item{(b), a diagnostic}{For every granted country: no container-keyed row
#'     survives anywhere, and per cell AND EPOCH the unit shares reproduce the
#'     country's level-0 share **in that same epoch**. Both sides are swept
#'     together per (cell, container), so neither is ever compared against a
#'     total taken across every epoch of the other. The epochs read are those
#'     of the container's own depth in the cell -- from its first unit row to
#'     its last, gaps inside it included -- because outside that span the
#'     layer holds no units for it at all and the 2015 snapshot claims every
#'     year, which is the vintage question open at T31(j) rather than anything
#'     the units did; land the layer then leaves unclaimed is reported by the
#'     other attribute. A container with no unit row in a cell at all is
#'     reported over its level-0 span. Rows that fail are RETURNED, in the
#'     `"ragged_coverage"` attribute, and are never back-filled with the
#'     container -- back-filling would restore exactly the fold this feature
#'     removes. (b) failing is not by itself a defect: level 0 is a fixed 2015
#'     snapshot while granted depths are year-filtered by edge validity, and
#'     how the two coexist is open at T31(j).}
#' }
#'
#' @section Unclaimed land:
#' A cell whose shares fall short of 1 holds land that no reporting polity
#' claims. That is a real state of the grid and not corruption: on the
#' `20260825T102349Z-1a0eb` polycell support, the level-0 grid at the 2015
#' snapshot has 37 such cells out of 66,709 -- the worst claimed to 0.0116 at
#' (20.75, 42.75) -- and the year-aware read leaves whole cells out on top of
#' that, which is what `validation/spatialize_grid_vintage.R` sizes per year.
#' Aborting on the shortfall would stop every granted-depth run on real data;
#' ignoring it would hide land that no national total accounts for. So it is
#' measured and returned, ONE ROW PER (CELL, EPOCH) that falls short, in the
#' `"unclaimed_land"` attribute: `lon`, `lat`, `start_year`, `end_year`
#' (`NA` for a bound the layer never gave), `claimed_share`,
#' `unclaimed_share`, and `unclaimed_ha`. A caller reads that attribute to
#' learn how much land is unclaimed, where, AND WHEN.
#'
#' The epoch is in the report because a cell is not one claim: it is short in
#' the years it is short. Judging each cell at its fullest epoch instead --
#' the moment the double-claim check has to read -- forgives a cell short only
#' before one of its holders existed, but by the same line of code it also
#' hides a cell that goes short in a LATER epoch, which is what a granted
#' depth ending before its container produces, and it turned an abort into
#' silence. So every epoch is read.
#'
#' WHICH YEARS ARE READ is a second question, and reading each cell between
#' its own first claim and its last answered it wrongly. A depth stops
#' covering its container either by holding a smaller share or by holding NO
#' ROW -- which is what a real deep grid produces, since a unit has no rows
#' outside its validity -- and in the second case the cell's last claim IS the
#' depth's end, so the window closed exactly where the silence began. On the
#' `20260825T102349Z-1a0eb` support, a depth for `area_code` 33 that
#' reproduces its level-0 shares exactly but stops in 1990 left 7,810 of the
#' 8,109 cells it still holds after 1990 in neither report -- 826.5 Mha of
#' land, each cell counted once -- and 0 ragged rows. It is now 0 cells.
#'
#' The window is therefore the union of the layer's span in the cell and the
#' level-0 span of the granted containers holding it: the same rule at both
#' ends, so a depth that starts late is caught like one that ends early, and a
#' cell the depth never reaches at all is read over the level-0 span alone.
#' It widens only to years level 0 itself states -- a row carrying no validity
#' interval (the 2015 snapshot, which has no time dimension) gives no bound to
#' fall short of, and the window then stops at the layer's own.
#'
#' The ragged-coverage report does NOT widen with it, and the asymmetry is
#' deliberate. This report asks what the LAYER claims, where absence is a fact
#' whatever `grid0`'s vintage; assertion (b) COMPARES two grids, and outside a
#' container's depth span it would be comparing year-scoped units against a
#' level 0 that may claim every year by having no time dimension at all --
#' the open vintage question at T31(j), not a defect of the units. So the
#' land a stopped depth leaves is reported here, once, rather than twice or
#' not at all.
#'
#' `unclaimed_ha` is `NA` where the layer carries no `land_area_ha` at all --
#' never 0, which would deny the shortfall. For an epoch no compartment
#' claims, the shortfall is instead read from `base`: `grid0`'s own row for
#' the granted container, in that same cell and on its own interval, which
#' `.level_cell_segments(shares, base)` already receives `base` for and which
#' carries a measurement rather than an absence. `unclaimed_ha` is `NA` there
#' only when `base` itself gives no matching land for the epoch -- no `base`
#' was supplied (a direct caller of the internal `.level_unclaimed_ha()`), or
#' the container's own row carries no `land_area_ha` either. Every hectare
#' figure this function reports assumes `land_area_ha` on each row already
#' equals `cell_area_frac` times the cell's whole land; that is a contract on
#' the CALLER's `grid0` and `grid_deep`, not something re-derived or checked
#' here, so a layer whose two disagree reports the column's stated value. The
#' attribute is always present; it is a zero-row tibble when every cell is
#' fully claimed in every epoch, and also on the no-grant path, which returns
#' `grid0` unexamined.
#'
#' @param grid0 The level-0 country grid, e.g. `read_level_country_grid(0L)`.
#' @param grid_deep A granted-depth country grid, e.g.
#'   `read_level_country_grid(1L)`. May also carry level-0 rows for countries
#'   that are not granted; they are ignored here and taken from `grid0`.
#' @param granted A tibble with `area_code` (integer container code) and
#'   `level` (integer depth), one row per container -- the granted rows of the
#'   T25 coverage report. `NULL` or a zero-row tibble returns `grid0`.
#'
#' @return `grid0`-shaped `tibble` with `level_polity_code` and `level`,
#'   carrying the ragged-coverage diagnostic in its `"ragged_coverage"`
#'   attribute and the unclaimed-land report in its `"unclaimed_land"`
#'   attribute (each a zero-row tibble when nothing was found).
#'
#' @seealso [read_level_country_grid()].
#' @export
#'
#' @examples
#' grid0 <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code, ~cell_area_frac,
#'   10.25, 40.25,       900L,             0.6,
#'   10.25, 40.25,       901L,             0.4
#' )
#' grid_deep <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
#'   10.25, 40.25,       900L, "A-A1-1900-2100",      1L,             0.25,
#'   10.25, 40.25,       900L, "A-A2-1900-2100",      1L,             0.35,
#'   10.25, 40.25,       901L, NA_character_,         0L,             0.4
#' )
#' granted <- tibble::tibble(area_code = 900L, level = 1L)
#' build_allocation_layer(grid0, grid_deep, granted)
build_allocation_layer <- function(grid0, grid_deep, granted) {
  .check_columns(grid0, c("lon", "lat", "area_code"), "grid0")
  grid0 <- .level_ensure_level_cols(grid0, 0L, "grid0")
  granted <- .level_check_granted(granted)
  if (nrow(granted) == 0L) {
    cli::cli_inform(
      "No container is granted a depth; the allocation layer is {.arg grid0}."
    )
    return(.level_attach_diagnostics(
      grid0,
      .level_ragged_prototype(),
      .level_unclaimed_prototype()
    ))
  }
  .check_columns(grid_deep, c("lon", "lat", "area_code"), "grid_deep")
  grid_deep <- .level_ensure_level_cols(grid_deep, NULL, "grid_deep")
  deep <- dplyr::semi_join(grid_deep, granted, by = c("area_code", "level"))
  .level_check_supply(deep, granted)
  base <- dplyr::filter(grid0, !(area_code %in% granted$area_code))
  layer <- dplyr::bind_rows(base, deep)
  # The interval columns are filled in before the shares are taken, never on
  # the returned layer: a row with no validity is valid always, and the sweep
  # below needs both bounds present to place its events.
  shares <- .level_compartment_shares(.level_support_intervals(layer))
  base <- .level_granted_base(grid0, granted)
  segments <- .level_cell_segments(shares, base)
  has_land <- rlang::has_name(segments, "claimed_ha")
  unclaimed <- .assert_layer_partition(segments) |>
    .level_unclaimed_ha(has_land, base)
  .level_warn_unclaimed(unclaimed, has_land)
  ragged <- .level_ragged_coverage(shares, base, granted)
  .level_warn_ragged(ragged)
  .level_attach_diagnostics(layer, ragged, unclaimed)
}

#' Zero-row prototype of the run's admin-coverage report
#'
#' @description
#' The schema of `admin_coverage.csv`, which [run_spatialize()] writes beside
#' `run_metadata.yaml` whenever a run requests a depth. One row per container x
#' item x year, saying which admin source constrained it and at what grain. The
#' populated table is produced by the T25 resolver; until a run is wired to one,
#' the file is written with this header and no rows, so a reader can tell "no
#' coverage was granted" from "the file was never written".
#'
#' It IS the resolver's own coverage schema, returned rather than restated:
#' this function had a second, hand-written list of names (`source`, `tier`,
#' `grain`, and a character `not_shipped`) where [resolve_admin_shares()]
#' emits `resolved_source`, `resolved_tier`, `resolved_grain` and a logical,
#' so handing the resolver's table to the writer aborted on three missing
#' columns, and the writer's subset would have dropped the resolution audit
#' trail (`resolved_indicator`, `resolved_nuts_version`, `resolution_rule`)
#' from the file.
#'
#' @return A zero-row `tibble` with the report's columns.
#' @export
#'
#' @examples
#' admin_coverage_prototype()
admin_coverage_prototype <- function() {
  .admin_coverage_prototype()
}

# --- Level validation --------------------------------------------------------

# One place decides what a level is, so the loader, the reader and the override
# key cannot disagree about whether `1` and `1L` are the same request.
.check_grid_level <- function(level, arg = "level") {
  if (is.null(level)) {
    return(0L)
  }
  ok <- is.numeric(level) &&
    length(level) == 1L &&
    !is.na(level) &&
    level >= 0 &&
    level == round(level)
  if (!ok) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a single non-negative whole number.",
      x = "Got {.val {level}}."
    ))
  }
  as.integer(level)
}

# The two level-0 support vintages. One place decides the vocabulary so the
# reader, the loader and the override key cannot drift apart, and so the
# `method_grid_vintage` column can only ever hold a value this names.
#
# THE DEFAULT IS THE 2015 SNAPSHOT, AND THAT IS A DELIBERATE HOLD.
# whep#1000, decision T31 item j, asked for the year-aware read as the
# default. The T39 measurement (`validation/spatialize_grid_vintage.R`, run
# against the `20260827T190201Z-f82a2` support) says what that costs on
# today's inputs, and it is not a refinement:
#
#   * `country_areas` is on a CONSTANT-TERRITORY basis -- it reports the
#     Russian Federation, Kazakhstan, Ukraine and Belarus in 1961 -- while the
#     support is on a HISTORICAL-POLITY basis and offers only the USSR that
#     year. A year-aware grid therefore has no cell for those reporting codes,
#     and `.warn_grid_missing_reporters()` drops their WHOLE national total:
#     164.2 Mha, 17.2% of the world's harvested area, in 1961; 165.1 Mha,
#     15.0%, in 1981. It cuts the other way too -- the livestock table still
#     reports the USSR in 1991, where the year-aware grid has already moved to
#     the successors, losing 1.57 billion head, 9.7% of the world's.
#   * Run end to end, that hole is what the gridded output loses. Over the
#     six largest-area crops at 1961 (section C of the same script) the
#     year-aware grid returns 438.1 Mha of gridded harvested area against the
#     snapshot's 544.9 Mha: 106.8 Mha, 19.6%, dropped outright, with a further
#     3.3 Mha reattributed between countries. At 2011 the two totals agree to
#     the last hectare and 1.5 Mha changes hands.
#   * Reconciling the two bases needs a lineage step on the NATIONAL side
#     (aggregate successors onto the historical polity, or split its cells
#     onto them), which is a separate piece of work and a science decision.
#
# So the vintage is selectable now and the measurement exists; flipping the
# default is one word here and in `.spatialize_presets()`, and belongs with
# that lineage step rather than before it.
.grid_vintages <- function() {
  c("snapshot_2015", "year_aware")
}

.check_grid_vintage <- function(grid_vintage, arg = "grid_vintage") {
  rlang::arg_match0(
    grid_vintage %||% .grid_vintages()[[1L]],
    .grid_vintages(),
    arg_nm = arg
  )
}

# --- Level 0, at either vintage ----------------------------------------------

# The two level-0 vintages, dispatched in one place so the snapshot branch is
# reachable only by an explicit request and stays the code that ran before
# whep#1000 T39 -- not a re-implementation of it.
.level0_country_grid <- function(
  support,
  containment,
  reference_year,
  grid_vintage
) {
  if (grid_vintage == "snapshot_2015") {
    .refuse_level0_arguments(support, containment, reference_year)
    return(.read_polycell_country_grid())
  }
  .refuse_level0_containment(containment)
  grid <- .polycell_grid_year_aware(support)
  if (!is.null(reference_year)) {
    grid <- .filter_country_grid_year(grid, as.integer(reference_year))
  }
  cli::cli_alert_info(
    "country_grid: polycell support, year-aware, {nrow(grid)} \\
     compartment{?s} over \\
     {dplyr::n_distinct(paste(grid$lon, grid$lat))} cell{?s} in \\
     {dplyr::n_distinct(grid$start_year)} epoch{?s}."
  )
  grid
}

# The snapshot vintage is the carbon path's own, taken at
# `.carbon_support_year()` through `.carbon_cell_support()`. Accepting a
# support, an edge table or a year here and then ignoring them is how a run
# ends up believing it read one thing while it read another, so they are
# refused rather than dropped.
.refuse_level0_arguments <- function(support, containment, reference_year) {
  given <- c(
    support = !is.null(support),
    containment = !is.null(containment),
    reference_year = !is.null(reference_year)
  )
  if (!any(given)) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg {names(given)[given]}} {?is/are} not read at {.code level = 0}
     with {.code grid_vintage = \"snapshot_2015\"}.",
    x = "That vintage is the carbon path's own snapshot at
         {.code .carbon_support_year()}, resolved by
         {.fn .read_polycell_country_grid}.",
    i = "Use {.code grid_vintage = \"year_aware\"} to supply your own support
         and reference year, or {.code level >= 1} to supply edges too."
  ))
}

# The containers a depth read is scoped to. Whole `area_code`s, because that
# is the space `granted`, `constraint_exclude` and every reporting join
# already speak; a container polity code would key the scope on an identity
# the run never states.
.level_check_containers <- function(containers, arg = "containers") {
  if (is.null(containers)) {
    return(NULL)
  }
  ok <- is.numeric(containers) &&
    length(containers) > 0L &&
    !anyNA(containers) &&
    all(containers == trunc(containers))
  if (!ok) {
    cli::cli_abort(c(
      "{.arg {arg}} must be a non-empty vector of whole
       {.field area_code}s, or {.val {NULL}}.",
      i = "For example {.code {arg} = 110L}, Japan."
    ))
  }
  sort(unique(as.integer(containers)))
}

# Level 0 keys cells on the reporting `area_code` directly and admits no edge,
# so there is nothing for a container scope to select. Refused rather than
# ignored, for the same reason `containment` is.
.refuse_level0_containers <- function(containers) {
  if (is.null(containers)) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg containers} is not read at {.code level = 0}.",
    x = "It scopes the containment edges a DEPTH is resolved against; level 0
         resolves none.",
    i = "Pass {.code level >= 1} to grant a depth to
         {.val {containers}}."
  ))
}

# SCOPING IS NOT SUPPRESSING. This drops the edges of containers the run did
# not ask for, and nothing else: the aggregate refusal, the support-empty
# abort and the double-claim gate all still run afterwards, on what is left.
# A run for Japan should not have to answer for Alaska sitting inside the USA
# in a support that carries both -- but if Japan's own container row survives
# beside its prefectures, that is Japan's defect and it still aborts.
.level_scope_containers <- function(edges, containers) {
  present <- sort(unique(edges$container_area_code))
  if (is.null(containers)) {
    cli::cli_inform(
      "No {.arg containers} scope: every admitted containment edge is read
       ({length(present)} container{?s})."
    )
    return(edges)
  }
  absent <- setdiff(containers, present)
  if (length(absent) > 0L) {
    cli::cli_abort(
      c(
        "{length(absent)} scoped container{?s} {?has/have} no admitted
         containment edge at this depth.",
        x = "{.field area_code}{?s}: {.val {absent}}.",
        i = "Admitted here: {.val {present}}."
      ),
      class = "whep_level_container_not_admitted"
    )
  }
  dropped <- setdiff(present, containers)
  if (length(dropped) > 0L) {
    cli::cli_inform(
      "Scoped to {length(containers)} container{?s}; {length(dropped)}
       other{?s} stay at level 0: {.val {dropped}}."
    )
  }
  dplyr::filter(edges, .data$container_area_code %in% containers)
}

# The containment edge names a member's container, which is what a DEPTH is
# resolved against; level 0 has no member. Refused rather than ignored for the
# same reason as above.
.refuse_level0_containment <- function(containment) {
  if (is.null(containment)) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg containment} is not read at {.code level = 0}.",
    x = "A containment edge resolves a member's container; level 0 keys cells
         on the reporting {.field area_code} directly.",
    i = "Pass {.code level >= 1} to resolve a depth against the edge."
  ))
}

# A granted depth is year-aware by construction: it keys cells on polities the
# containment edge validates per year, and the whole point of T12 is that the
# fold to one reference year is what deletes a province. So `grid_vintage` has
# nothing to select at depth and is not read -- said out loud, and recorded as
# `"year_aware"` by `.grid_vintage_method()`, rather than left for a reader of
# `run_metadata.yaml` to discover that the run's stated vintage was the one it
# ignored. Aborting instead would make every depth run fail under the shipped
# default, which is the level-0 snapshot.
.inform_deep_vintage <- function(level, vintage_given) {
  if (!isTRUE(vintage_given)) {
    return(invisible(NULL))
  }
  cli::cli_inform(
    "{.arg grid_vintage} is not read at {.code level = {level}}: a granted
     depth is year-aware by construction."
  )
  invisible(NULL)
}

# Level 0 in the support's own interval grain: the same four operations the
# snapshot does, IN THE SAME ORDER -- re-key onto the reporting vocabulary,
# take the cell's land, drop what the vocabulary cannot express, fold the
# polities sharing a code inside one cell -- carried out per EPOCH instead of
# at one reference year.
#
# THE ORDER IS THE WHOLE POINT, and it is `.carbon_support_to_area_code()`'s:
# the denominator is the cell's WHOLE measured land, taken BEFORE any row is
# dropped. Taking it after renormalises the survivors over a smaller cell and
# hands an unkeyable polity's hectares to whoever else holds the cell -- on the
# `20260825T102349Z-1a0eb` support, 17,655 cells carry such a row. That
# absorption is invisible to a partition check, because renormalising is
# exactly what forces the shares to sum to 1; what catches it is comparing the
# kept shares against the cell's whole land, which is what the two vintages
# then agree on at 2015. Measured on that support, the level-0 read has 72,099
# rows valid at 2015; taking the denominator after the drop moves some of them
# away from the snapshot the same year has to reproduce, and hands 2015 LUH2
# cropland to countries the snapshot does not give it to -- a pure GAIN, which
# a metric scoring only losses reports as a flat zero. The counts and hectares
# once quoted here were measured on `20260827T190201Z-f82a2`, the pin that
# books inland water and ice as land (whep#1010); the rejected implementation
# has not been re-run on the sound pin, so the claim is left qualitative
# rather than carried on a number known to be off its basis.
#
# Taking one denominator across the whole interval-grain table would instead
# count each cell once per epoch, which is the trap `.level_cell_land()` was
# written for; this reuses that helper rather than restating the rule.
.polycell_grid_year_aware <- function(support = NULL) {
  support <- (support %||% read_polycell_support()) |>
    tibble::as_tibble() |>
    .level_support_intervals()
  .check_columns(
    support,
    c("lon", "lat", "area_code", "cell_area_ha", "land_area_ha"),
    "support"
  )
  support <- .carbon_rekey_area_code(support)
  .level0_check_epochs(support)
  cell_land <- .level_cell_land(support, unique(support$start_year))
  support |>
    .carbon_drop_unkeyed() |>
    .level0_fold_epochs() |>
    dplyr::left_join(cell_land, by = c("lon", "lat", "start_year")) |>
    .level0_attach_share()
}

# The per-epoch denominator is looked up by interval-START year, which is exact
# only where the intervals inside one cell either coincide or are disjoint:
# then every row valid at a start year covers the same epoch, and the cell's
# land is constant across it. Where two intervals overlap without coinciding,
# the row whose interval starts alone gets a denominator taken over fewer rows
# than its neighbour's, and the cell is claimed more than once with nothing
# downstream able to see it.
#
# The shipped support has no such cell, but `support` is an exported argument
# and the snapshot path -- which reads one reference year and never faces the
# question -- refuses a caller's table outright rather than resolving it. So
# this refuses too. Only the level-0 read is guarded: at a granted depth the
# nested support shape carries a container's own row BESIDE its members'
# (`.level_share_nested()` needs exactly that), where overlap is the design.
.level0_check_epochs <- function(support) {
  clash <- support |>
    dplyr::distinct(.data$lon, .data$lat, .data$start_year, .data$end_year) |>
    dplyr::arrange(
      .data$lon,
      .data$lat,
      .data$start_year,
      .data$end_year
    ) |>
    dplyr::mutate(
      prev_start = dplyr::lag(.data$start_year),
      prev_end = dplyr::lag(.data$end_year),
      .by = c("lon", "lat")
    ) |>
    dplyr::filter(!is.na(.data$prev_end), .data$start_year < .data$prev_end)
  if (nrow(clash) == 0L) {
    return(invisible(NULL))
  }
  worst <- clash[1L, , drop = FALSE]
  cli::cli_abort(
    c(
      "{nrow(clash)} cell{?s} hold validity intervals that overlap without
       coinciding.",
      x = "First: ({worst$lon}, {worst$lat}) holds
           {worst$prev_start}-{worst$prev_end} beside
           {worst$start_year}-{worst$end_year}.",
      i = "The year-aware land share is taken per interval-start year, so a
           partial overlap gives the two rows different denominators and the
           cell is claimed more than once.",
      i = "Split the intervals so that within a cell they coincide or are
           disjoint, or read the support at one year with
           {.code grid_vintage = \"snapshot_2015\"}."
    ),
    class = "whep_level0_overlapping_epochs"
  )
}

# `.carbon_fold_area_code()`'s fold with the epoch in the key: two polities
# sharing a reporting code inside one cell are summed only where their validity
# intervals coincide, so a successor is not added to its own predecessor.
.level0_fold_epochs <- function(support) {
  folded <- support |>
    dplyr::summarise(
      cell_area_ha = dplyr::first(.data$cell_area_ha),
      land_area_ha = sum(.data$land_area_ha),
      n_polities = dplyr::n(),
      .by = c("lon", "lat", "area_code", "start_year", "end_year")
    )
  .carbon_warn_fold(folded, support)
  dplyr::select(folded, -"n_polities")
}

# `.carbon_attach_land_share()` at epoch grain, and the same treatment of a
# cell with no measured land: dropped, because a zero denominator has no share
# to take, and reported rather than divided.
.level0_attach_share <- function(grid) {
  dry <- dplyr::filter(
    grid,
    is.na(.data$cell_land_ha) | .data$cell_land_ha <= 0
  )
  if (nrow(dry) > 0L) {
    cli::cli_warn(
      "{dplyr::n_distinct(dry$lon, dry$lat)} cell{?s} hold no land in at
       least one epoch and carry no allocation; dropped from the grid."
    )
  }
  grid |>
    dplyr::filter(!is.na(.data$cell_land_ha), .data$cell_land_ha > 0) |>
    dplyr::mutate(cell_area_frac = .data$land_area_ha / .data$cell_land_ha) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "cell_area_ha",
      "land_area_ha",
      "cell_area_frac",
      "start_year",
      "end_year"
    )
}

# --- The containment edge ----------------------------------------------------

# Admit the edges whose member sits at exactly `level` steps inside a container
# the reporting vocabulary can name.
.level_admit_edges <- function(containment, level) {
  .check_columns(
    containment,
    c("member_code", "container_code", "start_year", "end_year"),
    "containment"
  )
  edges <- containment |>
    dplyr::mutate(
      start_year = as.integer(start_year),
      end_year = as.integer(end_year)
    ) |>
    dplyr::filter(!is.na(member_code), !is.na(container_code))
  depth <- .level_edge_depth(edges)
  edges <- edges |>
    dplyr::mutate(level = depth[match(member_code, names(depth))]) |>
    dplyr::filter(level == .env$level)
  if (nrow(edges) == 0L) {
    cli::cli_abort(
      c(
        "No containment edge places a polity at depth {level}.",
        i = "Depths present: {.val {sort(unique(depth))}}."
      ),
      class = "whep_level_no_edges"
    )
  }
  .level_resolve_containers(edges)
}

# Depth by walking the edge set: a member whose container is itself a member is
# one step deeper. Bounded by the number of distinct members, so a cycle in the
# edge table aborts instead of looping.
.level_edge_depth <- function(edges) {
  members <- unique(edges$member_code)
  depth <- rep(NA_integer_, length(members))
  names(depth) <- members
  # Depth is a property of the MEMBER, so the first container found for it
  # decides: a member listed inside two containers at different depths would be
  # at two depths at once, which the vocabulary does not express.
  parent <- edges$container_code[match(members, edges$member_code)]
  # A container that is never itself a member is the root the walk stops on.
  depth[!(parent %in% members)] <- 1L
  for (step in seq_along(members)) {
    if (!anyNA(depth)) {
      break
    }
    depth[is.na(depth)] <- depth[parent[is.na(depth)]] + 1L
  }
  if (anyNA(depth)) {
    cli::cli_abort(c(
      "{sum(is.na(depth))} containment edge{?s} form a cycle.",
      x = "Member{?s}: {.val {names(depth)[is.na(depth)]}}.",
      i = "A member cannot contain, directly or transitively, its container."
    ))
  }
  depth
}

# The container's reporting `area_code`, resolved through the package's own
# polity lookup and NEVER by name. An aggregate container is refused: its
# reporting code is a matrix bucket (999 pools 62 territories), so a unit filed
# under one would be attributed to Rest of World.
.level_resolve_containers <- function(edges) {
  types <- .level_polity_types()
  edges <- edges |>
    dplyr::mutate(
      container_area_code = .polity_reporting_area_code(container_code),
      polity_type = types$polity_type[match(container_code, types$polity_code)]
    )
  drop_type <- edges$polity_type %in% "aggregate"
  drop_code <- is.na(edges$container_area_code)
  if (any(drop_type | drop_code)) {
    .level_warn_dropped_edges(edges, drop_type, drop_code)
  }
  kept <- edges[!(drop_type | drop_code), , drop = FALSE]
  if (nrow(kept) == 0L) {
    cli::cli_abort(
      "No containment edge has a container the reporting vocabulary names.",
      class = "whep_level_no_edges"
    )
  }
  dplyr::select(
    kept,
    "member_code",
    "container_code",
    "container_area_code",
    "level",
    "start_year",
    "end_year"
  )
}

.level_polity_types <- function() {
  types <- whep::polity_area_crosswalk |>
    dplyr::distinct(polity_code, polity_type)
  dup <- unique(types$polity_code[duplicated(types$polity_code)])
  if (length(dup) > 0L) {
    cli::cli_abort(c(
      "{.field polity_area_crosswalk} gives {length(dup)} polit{?y/ies} more
       than one {.field polity_type}.",
      x = "{.val {dup}}."
    ))
  }
  types
}

.level_warn_dropped_edges <- function(edges, drop_type, drop_code) {
  aggregates <- unique(edges$container_code[drop_type])
  unnamed <- unique(edges$container_code[drop_code & !drop_type])
  cli::cli_warn(c(
    "!" = "{sum(drop_type | drop_code)} containment edge{?s} {?is/are} not
           admitted at this depth.",
    "*" = "{length(aggregates)} aggregate container{?s}: {.val {aggregates}}.",
    "*" = "{length(unnamed)} container{?s} with no single reporting
           {.field area_code}: {.val {unnamed}}.",
    i = "Their members stay folded into whatever level 0 reports."
  ))
}

# --- Support rows at a depth -------------------------------------------------

# The support rows of the admitted members, intersected with the edge's own
# validity. A member may sit inside several successive containers (Japan's
# prefectures span four Japanese epochs), so one support row can yield one row
# per overlapping edge; that is the point, since the container -- and therefore
# the reporting code the row is filed under -- is what changes.
.level_support_units <- function(support, edges, level) {
  .check_columns(
    support,
    c("lon", "lat", "polity_code", "cell_area_ha", "land_area_ha"),
    "support"
  )
  units <- support |>
    dplyr::inner_join(
      edges,
      by = dplyr::join_by(polity_code == member_code),
      relationship = "many-to-many",
      suffix = c("", "_edge")
    ) |>
    dplyr::mutate(
      start_year = pmax(start_year, start_year_edge),
      end_year = pmin(end_year, end_year_edge)
    ) |>
    dplyr::filter(start_year < end_year)
  if (nrow(units) == 0L) {
    cli::cli_abort(
      c(
        "The support carries no cells for any polity at depth {level}.",
        i = "{dplyr::n_distinct(edges$member_code)} member{?s} were looked
             for; the support holds
             {dplyr::n_distinct(support$polity_code)} polit{?y/ies}.",
        i = "Regenerate the polycell support so it carries the granted
             depth; see {.fn build_polycell_support}."
      ),
      class = "whep_level_support_empty"
    )
  }
  units
}

# The bounds a row with no validity interval is given. One place decides them,
# so the sweep that places events and the reports that read them back cannot
# disagree about which year means "no bound was given".
.level_open_interval <- function() {
  c(start = -2147483647L, end = 2147483647L)
}

# A bound the layer never gave is REPORTED as `NA`, not as the sentinel the
# sweep needs: a reader of the unclaimed-land or ragged-coverage attribute is
# not asked to recognise 2147483647 as "no end".
.level_open_to_na <- function(years) {
  dplyr::if_else(
    years %in% .level_open_interval(),
    NA_integer_,
    as.integer(years)
  )
}

# A support with no validity interval is a single-epoch table; give it the open
# interval rather than treating a missing bound as year zero.
.level_support_intervals <- function(support) {
  open <- .level_open_interval()
  if (!rlang::has_name(support, "start_year")) {
    support$start_year <- NA_integer_
  }
  if (!rlang::has_name(support, "end_year")) {
    support$end_year <- NA_integer_
  }
  support |>
    dplyr::mutate(
      start_year = dplyr::coalesce(as.integer(start_year), open[["start"]]),
      end_year = dplyr::coalesce(as.integer(end_year), open[["end"]])
    )
}

# --- The share of the physical cell ------------------------------------------

.level_attach_cell_share <- function(units, support, double_claim) {
  nested_col <- intersect(.level_container_frac_cols(), names(support))
  if (length(nested_col) > 0L) {
    return(.level_share_nested(units, support, nested_col[[1L]]))
  }
  .level_check_no_double_claim(units, support, double_claim)
  cell_land <- .level_cell_land(support, unique(units$start_year))
  units |>
    dplyr::left_join(cell_land, by = c("lon", "lat", "start_year")) |>
    dplyr::mutate(cell_area_frac = land_area_ha / cell_land_ha) |>
    .level_finish_grid()
}

#' Column names that hold a unit's share of its CONTAINER, not of the cell.
#'
#' Present only on a support that delivers within-container partitions. The
#' share of the physical cell is then the composition `f_unit x f_container`,
#' never the column itself: read as a cell share it would hand a province the
#' whole of its country's cell.
#' @noRd
.level_container_frac_cols <- function() {
  c("container_area_frac", "container_frac", "parent_frac")
}

# Nested support: the container keeps its own row (that is where `f_container`
# is measured) and the unit's column is a share of it.
.level_share_nested <- function(units, support, frac_col) {
  years <- unique(units$start_year)
  cell_land <- .level_cell_land(support, years)
  container_land <- .level_container_land(support, years)
  out <- units |>
    dplyr::left_join(cell_land, by = c("lon", "lat", "start_year")) |>
    dplyr::left_join(
      container_land,
      by = c("lon", "lat", "container_code", "start_year")
    )
  if (anyNA(out$container_land_ha)) {
    cli::cli_abort(
      c(
        "{sum(is.na(out$container_land_ha))} unit row{?s} have no container
         row in the same cell.",
        x = "A within-container share ({.field {frac_col}}) can only be
             composed where the container's own land is measured.",
        i = "Either supply the container rows or publish absolute
             {.field land_area_ha} per unit."
      ),
      class = "whep_level_container_missing"
    )
  }
  cli::cli_inform(
    "Support carries {.field {frac_col}}: composing the unit's share of its
     container with the container's share of the cell."
  )
  out |>
    dplyr::mutate(
      cell_area_frac = .data[[frac_col]] *
        (container_land_ha / cell_land_ha)
    ) |>
    .level_finish_grid()
}

# The cell's whole measured land, evaluated once per interval-start year the
# unit set uses. `.carbon_attach_land_share()` takes the same denominator at one
# reference year; in interval grain it has to be taken per epoch, because
# summing an interval-grain table whole counts each cell once per epoch.
.level_cell_land <- function(support, years) {
  purrr::map(
    sort(unique(years)),
    \(yr) {
      .filter_country_grid_year(support, yr) |>
        dplyr::summarise(
          cell_land_ha = sum(land_area_ha, na.rm = TRUE),
          .by = c("lon", "lat")
        ) |>
        dplyr::mutate(start_year = yr)
    }
  ) |>
    purrr::list_rbind()
}

# The container's own measured land per cell, taken on the same per-epoch basis
# as `.level_cell_land()`. Only the nested shape needs it.
.level_container_land <- function(support, years) {
  purrr::map(
    sort(unique(years)),
    \(yr) {
      .filter_country_grid_year(support, yr) |>
        dplyr::summarise(
          container_land_ha = sum(land_area_ha, na.rm = TRUE),
          .by = c("lon", "lat", "polity_code")
        ) |>
        dplyr::rename(container_code = "polity_code") |>
        dplyr::mutate(start_year = yr)
    }
  ) |>
    purrr::list_rbind()
}

# A container row surviving beside its own members in one cell claims that
# ground twice. Nothing downstream can see it -- the container's national total
# still reconciles -- so it is refused here rather than resolved by picking one.
# This is the consumer-side half of the T07 gate.
#
# WHICH CLASHES ARE REFUSED is `double_claim`'s question, and the two rules
# are alternatives, never a fallback:
#
# - `"co_presence"` (default) refuses every clash. It is the fail-closed rule
#   because this table cannot see geometry: a container beside its own units
#   halves those units' shares wherever the two describe the same ground, and
#   nothing downstream can tell that they do.
# - `"measured"` refuses only a container whose clash the cell's own area
#   REFUTES -- at least one shared cell-epoch holding more territory than the
#   cell has. That is a one-sided proof and is therefore weaker, not stronger:
#   a cell half of which is ocean can hide a duplicate under its own area. A
#   clash it passes over is warned about, never silent.
.level_check_no_double_claim <- function(
  units,
  support,
  double_claim = .level_double_claim_rules()
) {
  double_claim <- rlang::arg_match(double_claim)
  clash <- units |>
    dplyr::select("lon", "lat", "container_code", "start_year", "end_year") |>
    dplyr::distinct() |>
    dplyr::inner_join(
      dplyr::select(
        support,
        "lon",
        "lat",
        container_code = "polity_code",
        "land_area_ha",
        con_start = "start_year",
        con_end = "end_year"
      ),
      by = c("lon", "lat", "container_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(start_year < .data$con_end, .data$con_start < end_year)
  if (nrow(clash) == 0L) {
    return(invisible(NULL))
  }
  over <- .level_double_claim_excess(clash, support)
  if (identical(double_claim, "measured")) {
    clash <- .level_double_claim_refuted(clash, over)
  }
  if (nrow(clash) == 0L) {
    return(invisible(NULL))
  }
  .abort_double_claim(clash, over, double_claim)
}

.level_double_claim_rules <- function() {
  c("co_presence", "measured")
}

.abort_double_claim <- function(clash, over, double_claim) {
  codes <- sort(unique(clash$container_code))
  cli::cli_abort(
    c(
      "{nrow(clash)} container-cell row{?s} over
       {dplyr::n_distinct(clash$lon, clash$lat)} distinct cell{?s} carry a
       container row beside its own units
       ({round(sum(clash$land_area_ha, na.rm = TRUE) / 1e6, 2)} Mha of
       container land).",
      x = "Container{?s}: {.val {codes}}.",
      i = .level_double_claim_measure(over, codes),
      i = "The two claim the same ground, so the cell's land no longer
           partitions. A level-tagged support returns the units INSTEAD of
           their container; see the T07 package-wide level gate.",
      i = "Scope the read with {.arg containers} where the clash is in a
           container this run does not grant; the rule in force is
           {.code double_claim = {.val {double_claim}}}."
    ),
    class = "whep_level_support_double_claim"
  )
}

# HOW MUCH OF THE CO-PRESENCE IS A MEASURED OVER-CLAIM. Reported beside every
# refusal, and under `double_claim = "measured"` it also decides which
# containers are refused.
#
# What the table can see is whether a cell's whole claimed territory exceeds
# the cell's own area, which is a one-sided proof: a cell cannot hold more
# territory than it has. On the `20260827T190201Z-f82a2` support that
# separates the eight containers whose units are genuinely nested (Alaska in
# the USA over-claims 150 Mha over 1,156 of its 1,372 shared cells) from the
# two whose ground in the shared cell is disjoint and over-claims nothing
# (Ryukyu in Japan 1895-1945, 8 cells; Singapore in Malaysia, 1 cell). It is
# one-sided, not a verdict: a zero is co-presence the cell's own area cannot
# refute, not proof the ground is disjoint.
#
# `NULL` where the support carries no `cell_area_ha` to measure against, which
# is a state `"measured"` refuses to guess at.
.level_double_claim_excess <- function(clash, support) {
  if (!rlang::has_name(support, "cell_area_ha")) {
    return(NULL)
  }
  territory <- if (rlang::has_name(support, "polity_area_ha")) {
    "polity_area_ha"
  } else {
    "land_area_ha"
  }
  clash |>
    dplyr::distinct(
      .data$lon,
      .data$lat,
      .data$container_code,
      .data$start_year,
      .data$end_year
    ) |>
    dplyr::inner_join(
      dplyr::select(
        support,
        "lon",
        "lat",
        "cell_area_ha",
        claimed_ha = dplyr::all_of(territory),
        sup_start = "start_year",
        sup_end = "end_year"
      ),
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$start_year < .data$sup_end,
      .data$sup_start < .data$end_year
    ) |>
    dplyr::summarise(
      excess_ha = max(
        0,
        sum(.data$claimed_ha, na.rm = TRUE) -
          dplyr::first(.data$cell_area_ha)
      ),
      .by = c("lon", "lat", "container_code", "start_year", "end_year")
    )
}

# The clash rows `"measured"` still refuses: those of a container the cell's
# own area refutes SOMEWHERE. The decision is per CONTAINER and not per cell
# on purpose -- a coastal cell can hide its own duplicate under its area, so
# judging cell by cell would pass exactly the cells a nested container is
# hardest to see in, while its interior cells prove the nesting outright.
.level_double_claim_refuted <- function(clash, over) {
  if (is.null(over)) {
    cli::cli_abort(c(
      "{.code double_claim = \"measured\"} needs the support's
       {.field cell_area_ha} to measure the over-claim against.",
      i = "Supply it, or judge on co-presence with
           {.code double_claim = \"co_presence\"}."
    ))
  }
  refuted <- over |>
    dplyr::summarise(
      any_over = any(.data$excess_ha > 0),
      excess_mha = sum(.data$excess_ha) / 1e6,
      n_epochs = dplyr::n(),
      .by = "container_code"
    )
  passed <- dplyr::filter(refuted, !.data$any_over)
  if (nrow(passed) > 0L) {
    cli::cli_warn(c(
      "!" = "{nrow(passed)} container{?s} {?sits/sit} beside {?its/their} own
             units in {sum(passed$n_epochs)} cell-epoch{?s} that the cell's
             own area does not refute; {.code double_claim = \"measured\"}
             lets {?it/them} through.",
      "*" = "{.val {sort(passed$container_code)}}.",
      i = "A zero over-claim is co-presence the cell's area cannot refute,
           not proof the ground is disjoint."
    ))
  }
  dplyr::semi_join(
    clash,
    dplyr::filter(refuted, .data$any_over),
    by = "container_code"
  )
}

.level_double_claim_measure <- function(over, codes = NULL) {
  if (is.null(over) || nrow(over) == 0L) {
    return(
      "The support carries no {.field cell_area_ha}, so the over-claim
            was not measured."
    )
  }
  if (!is.null(codes)) {
    over <- dplyr::filter(over, .data$container_code %in% codes)
  }
  paste0(
    "Measured over-claim: ",
    sum(over$excess_ha > 0),
    " of ",
    nrow(over),
    " clashing cell-epochs hold more territory than the cell has (",
    round(sum(over$excess_ha) / 1e6, 2),
    " Mha). A zero is co-presence the cell's own area cannot refute, not
     proof the ground is disjoint."
  )
}

.level_finish_grid <- function(grid) {
  bad <- which(
    is.na(grid$cell_area_frac) |
      grid$cell_area_frac < -1e-8 |
      grid$cell_area_frac > 1 + 1e-8
  )
  if (length(bad) > 0L) {
    # The sole bound on a share that multiplies every downstream hectare.
    # `build_allocation_layer()` catches an over-claim independently, but
    # `NA` sums to `NA` and is dropped by its `> 1 + tol` filter, and a
    # negative share does not merely sum BELOW 1: paired with a genuine
    # over-claim in the same cell and epoch it CANCELS it, so the sum reads
    # as a perfect partition while one compartment claims more than the
    # whole cell. `.level_check_finite_share()` refuses both `NA` and a
    # negative share on that path for exactly that reason; this bound
    # refuses them here too, on the shares `read_level_country_grid()`
    # computes for itself.
    cli::cli_abort(
      c(
        "{length(bad)} compartment{?s} have a share outside {.code [0, 1]}.",
        x = "Range: {.val {range(grid$cell_area_frac[bad])}}.",
        i = "A cell whose measured land is zero has no share to take."
      ),
      class = "whep_level_share_out_of_range"
    )
  }
  grid |>
    dplyr::mutate(
      area_code = as.integer(container_area_code),
      level_polity_code = as.character(polity_code),
      level = as.integer(level),
      polycell_id = .level_polycell_id(grid)
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "level_polity_code",
      "level",
      "cell_area_frac",
      "polycell_id",
      "start_year",
      "end_year",
      dplyr::any_of(c("cell_area_ha", "land_area_ha"))
    )
}

# The support's own identifier where it has one, else the producer's
# `polity_code@cell_id` convention (`R/polycell_support.R:1562`).
.level_polycell_id <- function(grid) {
  if (rlang::has_name(grid, "polycell_id")) {
    return(as.character(grid$polycell_id))
  }
  cell <- if (rlang::has_name(grid, "cell_id")) {
    grid$cell_id
  } else {
    paste0(grid$lon, "_", grid$lat)
  }
  paste0(grid$polity_code, "@", cell)
}

# --- The allocation layer ----------------------------------------------------

.level_ensure_level_cols <- function(grid, default_level, arg) {
  grid <- tibble::as_tibble(grid)
  if (!rlang::has_name(grid, "level_polity_code")) {
    grid$level_polity_code <- NA_character_
  }
  if (!rlang::has_name(grid, "level")) {
    if (is.null(default_level)) {
      cli::cli_abort(c(
        "{.arg {arg}} carries no {.field level} column.",
        i = "A granted-depth grid must declare the depth its rows are at;
             {.fn read_level_country_grid} writes it."
      ))
    }
    grid$level <- default_level
  }
  frac_col <- intersect(.polity_share_cols(), names(grid))
  if (length(frac_col) == 0L) {
    .abort_missing_polity_share(grid, arg)
  }
  grid |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      level = as.integer(level),
      level_polity_code = as.character(level_polity_code),
      cell_area_frac = as.numeric(.data[[frac_col[[1L]]]])
    )
}

.level_check_granted <- function(granted) {
  if (is.null(granted)) {
    return(tibble::tibble(area_code = integer(), level = integer()))
  }
  .check_columns(granted, c("area_code", "level"), "granted")
  granted <- granted |>
    tibble::as_tibble() |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      level = as.integer(level)
    ) |>
    dplyr::select("area_code", "level")
  if (any(granted$level < 1L, na.rm = TRUE) || anyNA(granted)) {
    cli::cli_abort(c(
      "{.arg granted} must give every container a depth of at least 1.",
      i = "A container at level 0 is simply absent from {.arg granted}."
    ))
  }
  dup <- unique(granted$area_code[duplicated(granted$area_code)])
  if (length(dup) > 0L) {
    cli::cli_abort(c(
      "{.arg granted} names {length(dup)} container{?s} twice.",
      x = "{.val {dup}}.",
      i = "A country is allocated at exactly one depth."
    ))
  }
  granted
}

# A grant with no rows to honour it would silently leave the country out of the
# layer altogether, which is worse than allocating it at level 0.
.level_check_supply <- function(deep, granted) {
  supplied <- unique(deep$area_code)
  missing <- setdiff(granted$area_code, supplied)
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{length(missing)} granted container{?s} have no rows in
       {.arg grid_deep} at the granted depth.",
      x = "area_code{?s}: {.val {missing}}.",
      i = "Withdraw the grant or supply the depth; the country must not
           vanish from the layer."
    ),
    class = "whep_alloc_layer_no_supply"
  )
}

# A SHARE THE SWEEP CANNOT READ DOES NOT FAIL ASSERTION (a); IT DISABLES IT.
# `cumsum()` carries an `NA` into every later segment of its cell, and both
# bounds of the sweep -- `claimed_share > 1 + tol` and `claimed_share <
# 1 - tol` -- drop an `NA` row, so a single missing share silently exempts a
# whole cell from the check the layer exists to pass, in the direction that
# aborts. `Inf` reached the partition abort instead and was reported there as
# a cell claimed twice, which sends the reader to the wrong fix. So the share
# is required to be FINITE where the sweep reads it, and says which rows are
# not. `grid0` and `grid_deep` are the caller's tables here, not necessarily
# ones `.level_finish_grid()` has already bounded.
#
# A NEGATIVE SHARE DOES NOT FAIL ASSERTION (a) EITHER; IT HIDES A FAILURE.
# `cumsum()` sums it in unchanged, so it never disables the check the way an
# `NA` does -- but paired with a genuine over-claim in the SAME cell and
# epoch it CANCELS the excess: one compartment claiming 1.4 of a cell beside
# another claiming -0.4 sums to exactly 1 and reads as a perfect partition,
# though one of the two claims 140% of the ground. So the share is required
# to be NON-NEGATIVE at the same check that already requires it finite.
.level_check_finite_share <- function(layer, tol = 1e-8) {
  bad <- which(
    !is.finite(layer$cell_area_frac) | layer$cell_area_frac < -tol
  )
  if (length(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- utils::head(bad, 3L)
  shown <- paste0("row ", worst, ": ", layer$cell_area_frac[worst])
  cli::cli_abort(
    c(
      "{length(bad)} row{?s} of the allocation layer carry a
       {.field cell_area_frac} that is not finite or is negative.",
      x = "{.val {shown}}.",
      i = "An {.val {NA}} share is not a failed assertion but a suspended
           one: it propagates through the cell's running total and both
           bounds then drop the row, so the cell is never checked at all.",
      i = "A negative share is not suspended, it is WRONG: it can cancel a
           genuine over-claim elsewhere in the same cell and epoch, so the
           aggregate sum passes while one compartment claims more than the
           whole cell."
    ),
    class = "whep_alloc_layer_share_not_finite"
  )
}

# One row per (cell, compartment, VALIDITY INTERVAL). A compartment repeated
# with the same interval contributes its share ONCE, and two rows of one
# compartment-interval carrying DIFFERENT shares are a contradiction in the
# layer as given -- not a share that moves -- so they are refused rather than
# resolved by picking one. The interval is in the key because the sweep in
# `.level_cell_segments()` reads it: a share that genuinely differs between two
# disjoint intervals of one compartment is what the year-aware level-0 read
# produces, and summing those two as if they coexisted is the defect the sweep
# exists to remove. The compartment's land rides along on the same basis, so
# the unclaimed report can be stated in hectares without a second fold.
.level_compartment_shares <- function(layer, tol = 1e-8) {
  .level_check_finite_share(layer, tol)
  shares <- layer |>
    dplyr::summarise(
      share = dplyr::first(cell_area_frac),
      share_span = max(cell_area_frac) - min(cell_area_frac),
      n_rows = dplyr::n(),
      dplyr::across(dplyr::any_of("land_area_ha"), dplyr::first),
      .by = c(
        "lon",
        "lat",
        "area_code",
        "level_polity_code",
        "start_year",
        "end_year"
      )
    )
  varying <- dplyr::filter(shares, share_span > tol)
  if (nrow(varying) > 0L) {
    cli::cli_abort(
      c(
        "{nrow(varying)} compartment{?s} hold two different shares of one
         cell in one validity interval.",
        x = "Worst span: {.val {max(varying$share_span)}}.",
        i = "One compartment holds one share of one cell at one time; two
             rows saying otherwise cannot be summed and cannot be chosen
             between. A share that differs between DISJOINT intervals is
             read at each of them and is not this."
      ),
      class = "whep_alloc_layer_varying_share"
    )
  }
  dplyr::select(
    shares,
    "lon",
    "lat",
    "area_code",
    "level_polity_code",
    "start_year",
    "end_year",
    "share",
    dplyr::any_of("land_area_ha")
  )
}

# THE PARTITION HOLDS PER (CELL, EPOCH), NOT PER CELL. The layer's rows carry
# validity intervals, and two compartments whose intervals never overlap never
# claim the same ground: a cell held whole by the USSR and then whole by its
# successor sums to 2 over the layer as passed, and every succession of the
# year-aware level-0 read is such a cell. Summing across epochs made that
# vintage unusable with any granted depth.
#
# EVERY epoch, by an INTERVAL SWEEP: each compartment contributes `+share` at
# its start year and `-share` at its end, and the running sum over a cell's
# events is what that cell holds between one event and the next. The state is
# read after EVERY event of a year, so a succession boundary counts the
# successor and not both, and a zero-length interval counts neither. That
# reading does not depend on how events falling on one year are ordered among
# themselves, which is why no order is imposed: the peak-based sweep needed
# ends before starts, because it maximised over the part-way states inside a
# year as well, where a start read before an end shows a claim nothing holds.
# A layer whose intervals are all open has one epoch per cell, and its claim
# is then the unconditional sum T37 agreed on -- unchanged.
#
# ALL the segments, not the fullest one. Detecting "claimed twice" needs only
# the peak, but the unclaimed report has to say how much is unclaimed AND WHEN,
# and a cell judged in its fullest epoch alone is silent about a cell that goes
# short in a later one -- which is the direction a granted depth ending before
# its container produces. The peak is recovered from the segments where it is
# wanted, rather than the segments being thrown away to keep it.
#
# It is a sweep and not a per-epoch re-filter because the shipped support
# carries about 150 distinct interval-start years over 438,000 compartments:
# filtering the layer once per epoch is that product, and does not finish. The
# segments cost no more than the peak did -- there are two events per
# compartment either way.
.level_cell_segments <- function(shares, base) {
  has_land <- rlang::has_name(shares, "land_area_ha")
  land <- if (has_land) .level_measured_land(shares) else 0
  n <- nrow(shares)
  segments <- dplyr::bind_rows(
    tibble::tibble(
      lon = rep(shares$lon, 2L),
      lat = rep(shares$lat, 2L),
      at = c(shares$start_year, shares$end_year),
      d_share = c(shares$share, -shares$share),
      d_land = c(rep_len(land, n), -rep_len(land, n))
    ),
    .level_window_events(shares, base)
  ) |>
    dplyr::arrange(.data$lon, .data$lat, .data$at) |>
    dplyr::mutate(
      claimed_share = cumsum(.data$d_share),
      claimed_ha = cumsum(.data$d_land),
      .by = c("lon", "lat")
    ) |>
    .level_last_state(
      c("lon", "lat"),
      c("claimed_share", "claimed_ha")
    ) |>
    .level_close_segments(c("lon", "lat"))
  # "0 ha" and "not measured" are different statements, so a layer with no
  # land column must not come back carrying a hectare figure of zero.
  if (has_land) segments else dplyr::select(segments, -"claimed_ha")
}

# The granted containers' own rows of `grid0`, in interval grain. Both
# diagnostics read them: assertion (b) compares the units against them, and
# the sweep takes the years they cover as the outer edge of the window it
# reads the cell over. One function, so the two cannot disagree about which
# rows level 0 contributes.
.level_granted_base <- function(grid0, granted) {
  grid0 |>
    dplyr::filter(.data$area_code %in% granted$area_code) |>
    .level_support_intervals()
}

# THE YEARS A CELL IS READ OVER, and the fix to the silence a granted depth
# that simply STOPS used to produce. The sweep's own events end at the cell's
# last claim, so a window drawn from the layer alone ends exactly where the
# silence starts: a depth ending in 1950 in a cell nobody else holds leaves
# the layer with no row after 1949, the last segment closes at 1950, and
# every later year -- 838.6 Mha of it for area_code 33 alone on the shipped
# support -- is unclaimed and unreported. A depth that ends by SHRINKING was
# reported and one that ends by ABSENCE was not, though both say the same
# thing and absence is what a real deep grid produces, because a unit has no
# rows outside its own validity.
#
# So the window is the union of the layer's span in the cell and the level-0
# span of the granted containers holding it. ONE RULE AT BOTH ENDS: a depth
# that starts late is the same defect mirrored, and a cell the depth never
# reaches at all has no layer span, so it is read over the level-0 span
# alone. What the window does NOT do is invent a bound: it widens only to
# years level 0 itself states.
#
# A BOUND LEVEL 0 NEVER GAVE IS NOT ONE THE LAYER CAN FALL SHORT OF. The 2015
# snapshot carries no time dimension, so `.level_support_intervals()` gives
# its rows the open interval; extending to that would report a leading and a
# trailing unclaimed epoch for every granted cell of that vintage, which is
# the T31(j) question of how a year-scoped depth and a year-free level 0
# coexist, not a shortfall. The open sentinel is therefore dropped rather
# than extended to, at both ends.
#
# The events carry no share and no land, so they move no running total: they
# add only the event YEARS the sweep would otherwise never see. They are
# placed strictly OUTSIDE the layer's own span, never inside it, so no
# segment the layer already describes is split in two.
.level_window_events <- function(shares, base) {
  span <- dplyr::summarise(
    shares,
    lo = min(.data$start_year),
    hi = max(.data$end_year),
    .by = c("lon", "lat")
  )
  open <- .level_open_interval()
  lead <- base |>
    dplyr::filter(.data$start_year != open[["start"]]) |>
    .level_cell_bound("start_year", min) |>
    dplyr::left_join(span, by = c("lon", "lat")) |>
    dplyr::filter(dplyr::coalesce(.data$at < .data$lo, TRUE))
  trail <- base |>
    dplyr::filter(.data$end_year != open[["end"]]) |>
    .level_cell_bound("end_year", max) |>
    dplyr::left_join(span, by = c("lon", "lat")) |>
    dplyr::filter(dplyr::coalesce(.data$at > .data$hi, TRUE))
  dplyr::bind_rows(lead, trail) |>
    dplyr::mutate(d_share = 0, d_land = 0) |>
    dplyr::select("lon", "lat", "at", "d_share", "d_land")
}

# One bound per cell, on a frame that may hold no rows at all: with every
# bound open -- the 2015-snapshot vintage -- the filter above leaves nothing,
# and dplyr type-probes the aggregate on the empty frame, where `min()` warns
# and returns `Inf`. The early return is that case, not a convenience.
.level_cell_bound <- function(rows, col, reduce) {
  if (nrow(rows) == 0L) {
    return(tibble::tibble(lon = numeric(), lat = numeric(), at = integer()))
  }
  dplyr::summarise(
    rows,
    at = reduce(.data[[col]]),
    .by = c("lon", "lat")
  )
}

# A layer carrying `land_area_ha` on SOME rows only under-counts every hectare
# figure taken from it, silently: a row with no land reads as 0 ha claimed
# while its share still counts, so the shortfall is scaled by a denominator
# smaller than the land actually claimed. "The column is there" and "the
# column is filled" are different statements, and only the second one makes
# the hectares mean anything.
.level_measured_land <- function(shares) {
  n_open <- sum(is.na(shares$land_area_ha))
  if (n_open > 0L) {
    cli::cli_warn(c(
      "!" = "{n_open} compartment{?s} of the allocation layer carry no
             {.field land_area_ha}.",
      i = "Their share still counts, so the hectares in the
           {.field unclaimed_land} attribute are taken over the land that IS
           measured and under-state the shortfall."
    ))
  }
  dplyr::coalesce(shares$land_area_ha, 0)
}

# The state that HOLDS from one event year to the next is the one left after
# EVERY event of that year, so a succession boundary counts the successor and
# not both, and a zero-length interval counts neither. The frame is already
# sorted, so that state is the last row of each (group, year) run and a run
# boundary finds it in one pass: a grouped reduction instead would run over
# the 400,000-odd (cell, container) groups a real layer carries and cost
# several times the sweep it closes.
.level_last_state <- function(states, keys, cols) {
  run <- do.call(
    dplyr::consecutive_id,
    unname(as.list(states[c(keys, "at")]))
  )
  states |>
    dplyr::filter(dplyr::coalesce(run != dplyr::lead(run), TRUE)) |>
    dplyr::select(dplyr::all_of(c(keys, "at", cols)))
}

# The state read after every event of one year holds until the next event, so
# a group's epochs are the elementary intervals between its own event years.
# The last state is dropped: after a group's final event nothing it carries is
# open and there is no next year to close a segment at. Which years the group
# HAS events in is the caller's question, not this one's:
# `.level_window_events()` adds the bounds level 0 states for a cell before
# the sweep runs, so the segment after a stopped depth exists to be dropped
# from, and the peak-based report's other failure -- forgiving a shortfall in
# an early epoch and hiding one in a late epoch, by one line of code -- is
# gone either way.
.level_close_segments <- function(states, keys) {
  states |>
    dplyr::mutate(
      start_year = .data$at,
      end_year = dplyr::lead(.data$at),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::filter(!is.na(.data$end_year)) |>
    dplyr::select(-"at")
}

# Assertion (a), one-sided: a cell may never be claimed twice, but it may be
# claimed only in part. EITHER level-0 vintage drops the polycells the
# reporting vocabulary cannot name -- `.carbon_cell_support()` at 2015, and the
# year-aware read per epoch -- while keeping the cell's whole land as the
# denominator, so real cells sum below 1 before any depth is granted. That is a
# property of `grid0`, not of the grant, and it is land nobody reports rather
# than land the layer lost. Returns the shortfall for the caller to report.
.assert_layer_partition <- function(segments, tol = 1e-8) {
  over <- dplyr::filter(segments, .data$claimed_share > 1 + tol)
  if (nrow(over) > 0L) {
    .abort_layer_over_claimed(over)
  }
  segments |>
    dplyr::filter(.data$claimed_share < 1 - tol) |>
    dplyr::mutate(unclaimed_share = 1 - .data$claimed_share) |>
    dplyr::arrange(dplyr::desc(.data$unclaimed_share))
}

# How an epoch reads in a message. The sweep needs a sentinel for a bound the
# layer never gave; a person reading the warning needs the words.
.level_epoch_text <- function(start_year, end_year) {
  dplyr::case_when(
    is.na(start_year) & is.na(end_year) ~ "every year the layer describes",
    is.na(start_year) ~ paste("everything up to", end_year),
    is.na(end_year) ~ paste("everything from", start_year),
    .default = paste0(start_year, "-", end_year)
  )
}

.abort_layer_over_claimed <- function(over) {
  worst <- over[which.max(over$claimed_share), , drop = FALSE]
  epoch <- .level_epoch_text(
    .level_open_to_na(worst$start_year),
    .level_open_to_na(worst$end_year)
  )
  cli::cli_abort(
    c(
      "{nrow(over)} (cell, epoch) row{?s} of the allocation layer are
       claimed twice.",
      x = "Worst: ({worst$lon}, {worst$lat}) sums to
           {.val {worst$claimed_share}}, above 1, over {epoch}.",
      i = "A sum above 1 is ground claimed twice -- a container kept beside
           its own units, or one compartment whose own validity intervals
           overlap without coinciding -- and it leaves every national total
           reconciling either way. A sum below 1 is permitted: it is land
           no reporting polity claims, returned in the
           {.field unclaimed_land} attribute."
    ),
    class = "whep_alloc_layer_not_partition"
  )
}

# The reference land for a (cell, epoch) SEGMENT NOBODY CURRENTLY CLAIMS.
# `base` is `.level_granted_base()`'s output -- `grid0`'s own rows for the
# granted containers, each on its own validity interval -- and the widened
# window is built from those SAME rows (`.level_window_events()`), so a
# segment outside the layer's own span cannot hold a claim from any OTHER
# compartment either: if one did, the layer's own span would already reach
# it and no widening would have been needed. Summing the `base` rows valid
# across the segment's interval therefore reads the cell's whole land there,
# on the SAME interval it is reported over -- a measurement that exists,
# never one carried forward from a different epoch. `NA` where `base` gives
# no matching row: no `base` was passed at all, or the row it has there
# carries no `land_area_ha`.
.level_base_land_gap <- function(unclaimed, base) {
  if (
    is.null(base) ||
      nrow(unclaimed) == 0L ||
      !rlang::has_name(base, "land_area_ha")
  ) {
    return(rep(NA_real_, nrow(unclaimed)))
  }
  keyed <- dplyr::mutate(unclaimed, row_id = dplyr::row_number())
  gap <- keyed |>
    dplyr::select("lon", "lat", "start_year", "end_year", "row_id") |>
    dplyr::inner_join(
      dplyr::select(
        base,
        "lon",
        "lat",
        "start_year",
        "end_year",
        "land_area_ha"
      ),
      by = c("lon", "lat"),
      suffix = c("", "_base"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$start_year < .data$end_year_base,
      .data$start_year_base < .data$end_year
    ) |>
    dplyr::summarise(
      gap_ha = sum(.data$land_area_ha, na.rm = TRUE),
      .by = "row_id"
    )
  dplyr::left_join(keyed, gap, by = "row_id")$gap_ha
}

# The cell's whole measured land is not a column of the layer, but it is
# recoverable wherever the claimed land is: `claimed_share` is the fraction of
# the cell's land that the claimed rows hold, so the shortfall in hectares is
# `claimed_ha * unclaimed_share / claimed_share`. Both come off the same
# epoch's rows, so a compartment repeated across its own validity intervals
# contributes its land ONCE and the share and the hectares describe the same
# moment.
#
# "NOBODY CLAIMS IT" IS `claimed_share <= tol`, NOT `== 0`, and the
# proportional formula above cannot price it: the segment after a cell's
# last claim is what the running total is left at once every claim has been
# closed, and `cumsum()` over doubles does not cancel to exactly 0
# (`0.1 + 0.2 + 0.7` less the same three leaves 2.2e-16), so
# `claimed_ha / claimed_share` there is a ratio of two rounding errors -- 0,
# which denies the shortfall, or a plausible hectare figure that is pure
# noise. `.level_base_land_gap()` prices it instead, from `base`, which is
# what `.level_cell_segments(shares, base)` already receives `base` for.
#
# FOUR STATES, NOT TWO. A layer with no land column cannot put a magnitude on
# any shortfall. Where something IS claimed, the proportional formula prices
# the rest. Where nothing is claimed, `base` prices it where `base` covers
# that cell and epoch; where it does not, the shortfall is `NA`, never 0 and
# never `NaN` -- the peak-based formula this replaced returned `NaN`, which
# `is.na()` accepts, so a layer carrying land was reported as carrying none.
.level_unclaimed_ha <- function(unclaimed, has_land, base = NULL, tol = 1e-8) {
  if (nrow(unclaimed) == 0L || !has_land) {
    return(.level_unclaimed_out(dplyr::mutate(
      unclaimed,
      unclaimed_ha = NA_real_
    )))
  }
  gap_ha <- .level_base_land_gap(unclaimed, base)
  unclaimed |>
    dplyr::mutate(
      unclaimed_ha = dplyr::if_else(
        .data$claimed_share > tol,
        .data$claimed_ha * .data$unclaimed_share / .data$claimed_share,
        gap_ha
      )
    ) |>
    .level_unclaimed_out()
}

.level_unclaimed_out <- function(unclaimed) {
  unclaimed |>
    dplyr::mutate(
      start_year = .level_open_to_na(.data$start_year),
      end_year = .level_open_to_na(.data$end_year)
    ) |>
    dplyr::select(
      dplyr::all_of(
        names(.level_unclaimed_prototype())
      )
    )
}

.level_unclaimed_prototype <- function() {
  tibble::tibble(
    lon = numeric(),
    lat = numeric(),
    start_year = integer(),
    end_year = integer(),
    claimed_share = numeric(),
    unclaimed_share = numeric(),
    unclaimed_ha = numeric()
  )
}

.level_warn_unclaimed <- function(unclaimed, has_land) {
  if (nrow(unclaimed) == 0L) {
    return(invisible(NULL))
  }
  worst <- unclaimed[1L, , drop = FALSE]
  epoch <- .level_epoch_text(worst$start_year, worst$end_year)
  cells <- dplyr::n_distinct(unclaimed$lon, unclaimed$lat)
  total <- .level_unclaimed_total(unclaimed, has_land)
  cli::cli_warn(c(
    "!" = "{nrow(unclaimed)} (cell, epoch) row{?s} of the allocation layer
           hold land no reporting polity claims, over {cells} cell{?s}.",
    "*" = "Worst: ({worst$lon}, {worst$lat}), {.val {worst$unclaimed_share}}
           of the cell's land unclaimed over {epoch}; {total}.",
    i = "Returned in the {.field unclaimed_land} attribute. This is a
         property of the level-0 grid, not of the grant, and it is NOT an
         error; it is land outside every national total."
  ))
}

# "0 Mha", "not measured" and "not computable" are three different statements.
# Reporting zero for either of the last two would hide the shortfall exactly as
# refusing to report it would, and calling a layer that carries land one that
# does not is worse still: it sends the reader to fix the wrong thing.
#
# AND THE SUM OVER THE ROWS IS NOT AN AREA. The report is one row per (cell,
# EPOCH), so a cell short in several epochs contributes its land once per
# epoch -- median 1 row per cell on the shipped support and up to 14 -- and
# summing the column gives hectare-epochs: 6182.97 Mha where the land
# involved is 2951.11. It sat beside a documented per-year figure, where a
# reader compares it against global land, and "over all such rows" was not
# enough to stop that. The headline is therefore the LAND: each cell counted
# once, at the epoch it is shortest in, which is the most of that cell any
# one year leaves unclaimed. The rows themselves carry the per-epoch detail
# for anyone who wants it summed differently.
.level_unclaimed_total <- function(unclaimed, has_land) {
  if (!has_land) {
    return("no land column on the layer, so no hectares")
  }
  n_open <- sum(is.na(unclaimed$unclaimed_ha))
  measured <- dplyr::filter(unclaimed, !is.na(.data$unclaimed_ha))
  # One bound per cell, on a frame that may hold NO row at all: when every
  # unclaimed row carries `NA` hectares -- `base` gave none of them a
  # measurement -- the filter above leaves nothing, and dplyr type-probes the
  # aggregate on the empty frame, where `max()` warns and returns `-Inf`. The
  # early return is that case, the same one `.level_cell_bound()` guards
  # against 200-odd lines above; printing "0 Mha of land" here would deny a
  # shortfall that was never measured at all, exactly what the header above
  # this function forbids.
  if (nrow(measured) == 0L) {
    return(paste0(
      "no epoch's shortfall could be measured, over ",
      cli::pluralize("{n_open} row{?s}"),
      " claimed by nobody, whose land the layer never measures"
    ))
  }
  worst <- measured |>
    dplyr::summarise(ha = max(.data$unclaimed_ha), .by = c("lon", "lat"))
  total <- paste0(
    round(sum(worst$ha) / 1e6, 2),
    " Mha of land, each cell counted once at its worst epoch"
  )
  if (n_open == 0L) {
    return(total)
  }
  paste0(
    total,
    ", plus ",
    cli::pluralize("{n_open} row{?s}"),
    " claimed by nobody, whose land the layer never measures"
  )
}

# Assertion (b), a diagnostic and never a repair.
#
# BOTH SIDES ON ONE GRAIN. The units and the level-0 shares are swept together,
# per (cell, container), so each side is read over the same elementary interval
# and never one against a total taken across every epoch of the other. Reading
# the units at one moment per CELL was the defect: a container whose units
# exactly reproduce its level-0 share in every year was reported ragged
# whenever the cell's fullest moment fell in a neighbour's epoch, and 84,522
# (cell, container) pairs of the year-aware level-0 read carry more than one
# epoch.
.level_ragged_coverage <- function(shares, base, granted, tol = 1e-8) {
  units <- dplyr::filter(shares, .data$area_code %in% granted$area_code)
  .level_container_segments(units, base) |>
    dplyr::filter(.data$in_window) |>
    dplyr::mutate(
      difference = .data$unit_share - .data$level0_share,
      reason = .level_ragged_reason(
        .data$n_container_rows,
        .data$n_units,
        .data$n_level0_rows,
        .data$difference,
        tol
      )
    ) |>
    dplyr::filter(!is.na(.data$reason)) |>
    .level_ragged_out()
}

# The running totals each side of assertion (b) contributes to the sweep.
.level_ragged_sums <- function() {
  c(
    "unit_share",
    "n_units",
    "n_container_rows",
    "level0_share",
    "n_level0_rows",
    "u_started",
    "b_started"
  )
}

# The same sweep as `.level_cell_segments()`, keyed on (cell, CONTAINER) and
# carrying both sides at once, so a segment states the container's unit share
# and its level-0 share over one and the same interval.
.level_container_segments <- function(units, base) {
  dplyr::bind_rows(
    .level_unit_events(units),
    .level_base_events(base)
  ) |>
    dplyr::arrange(.data$lon, .data$lat, .data$area_code, .data$at) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(.level_ragged_sums()), cumsum),
      .by = c("lon", "lat", "area_code")
    ) |>
    .level_last_state(
      c("lon", "lat", "area_code"),
      .level_ragged_sums()
    ) |>
    .level_ragged_window() |>
    .level_close_segments(c("lon", "lat", "area_code"))
}

# A granted container's rows of the layer as sweep events. `u_started` counts
# starts only, so its running total is how many of the container's unit rows
# the cell has seen begin, and the group's last value is how many there are.
.level_unit_events <- function(units) {
  starts <- tibble::tibble(
    lon = units$lon,
    lat = units$lat,
    area_code = units$area_code,
    at = units$start_year,
    unit_share = units$share,
    n_units = as.integer(!is.na(units$level_polity_code)),
    n_container_rows = as.integer(is.na(units$level_polity_code)),
    level0_share = 0,
    n_level0_rows = 0L,
    u_started = 1L,
    b_started = 0L
  )
  ends <- starts |>
    dplyr::mutate(
      at = units$end_year,
      dplyr::across(
        dplyr::all_of(c("unit_share", "n_units", "n_container_rows")),
        \(v) -v
      ),
      u_started = 0L
    )
  dplyr::bind_rows(starts, ends)
}

# The level-0 side of the same sweep: the container's own share of the cell,
# in whatever validity intervals `grid0` carries. A `grid0` with none is one
# epoch spanning the whole layer, which is the 2015 snapshot's shape.
.level_base_events <- function(base) {
  starts <- tibble::tibble(
    lon = base$lon,
    lat = base$lat,
    area_code = base$area_code,
    at = base$start_year,
    unit_share = 0,
    n_units = 0L,
    n_container_rows = 0L,
    level0_share = base$cell_area_frac,
    n_level0_rows = 1L,
    u_started = 0L,
    b_started = 1L
  )
  ends <- starts |>
    dplyr::mutate(
      at = base$end_year,
      dplyr::across(
        dplyr::all_of(c("level0_share", "n_level0_rows")),
        \(v) -v
      ),
      b_started = 0L
    )
  dplyr::bind_rows(starts, ends)
}

# THE EPOCHS ASSERTION (b) IS READ OVER. A granted container's DEPTH in a cell
# spans from its first unit row to its last, gaps inside it included. Outside
# that span the layer holds no units for the container at all, and comparing a
# year-scoped depth against a level-0 grid carrying no time dimension there is
# the open question at T31(j) rather than anything the units did: the 2015
# snapshot claims every year, so every granted cell would otherwise report a
# ragged epoch before its units began and another after they ended. What that
# leaves out is not lost -- a cell the layer does not fully claim in those
# years is reported by the unclaimed-land attribute, whose question it is.
# Where the container has NO unit row in the cell at all, its level-0 span is
# read instead, so a cell its depth never reaches is still reported.
.level_ragged_window <- function(states) {
  states |>
    dplyr::mutate(
      u_total = max(.data$u_started),
      b_total = max(.data$b_started),
      .by = c("lon", "lat", "area_code")
    ) |>
    dplyr::mutate(
      in_window = dplyr::if_else(
        .data$u_total > 0L,
        .data$u_started > 0L &
          .data$u_started -
            .data$n_units -
            .data$n_container_rows <
            .data$u_total,
        .data$b_started > 0L &
          .data$b_started - .data$n_level0_rows < .data$b_total
      )
    )
}

.level_ragged_out <- function(ragged) {
  ragged |>
    dplyr::mutate(
      start_year = .level_open_to_na(.data$start_year),
      end_year = .level_open_to_na(.data$end_year)
    ) |>
    dplyr::select(dplyr::all_of(names(.level_ragged_prototype())))
}

# A container's depth window can contain an epoch in which the container is
# not in the cell AT ALL -- it leaves and comes back, which the year-aware
# level-0 read has 2,632 (cell, container, epoch) rows of. Both sides are then
# empty and they AGREE, so there is nothing to report: `no_unit_rows` states
# that level 0 puts the container in the cell and the depth gives it no unit
# there, which is false of an epoch level 0 does not put it in either. A
# diagnostic row whose two sides agree is noise, and enough of it hides the
# rows that do not.
.level_ragged_reason <- function(
  n_container_rows,
  n_units,
  n_level0_rows,
  difference,
  tol
) {
  dplyr::case_when(
    n_container_rows > 0L ~ "container_row_present",
    n_units == 0L & n_level0_rows == 0L ~ NA_character_,
    n_units == 0L ~ "no_unit_rows",
    n_level0_rows == 0L ~ "unit_outside_level0",
    abs(difference) > tol ~ "unit_share_mismatch",
    .default = NA_character_
  )
}

.level_ragged_prototype <- function() {
  tibble::tibble(
    lon = numeric(),
    lat = numeric(),
    area_code = integer(),
    start_year = integer(),
    end_year = integer(),
    n_units = integer(),
    n_container_rows = integer(),
    unit_share = numeric(),
    level0_share = numeric(),
    difference = numeric(),
    reason = character()
  )
}

.level_warn_ragged <- function(ragged) {
  if (nrow(ragged) == 0L) {
    return(invisible(NULL))
  }
  counts <- table(ragged$reason)
  cells <- dplyr::n_distinct(ragged$lon, ragged$lat)
  cli::cli_warn(c(
    "!" = "{nrow(ragged)} (cell, container, epoch) row{?s} of the allocation
           layer are ragged: a granted country's units do not reproduce its
           level-0 share. {cells} cell{?s} carry one.",
    "*" = "{paste(names(counts), unname(counts), sep = ': ',
             collapse = '; ')}.",
    i = "Returned in the {.field ragged_coverage} attribute. They are NOT
         back-filled with the container -- that would restore the fold."
  ))
}

.level_attach_diagnostics <- function(layer, ragged, unclaimed) {
  attr(layer, "ragged_coverage") <- ragged
  attr(layer, "unclaimed_land") <- unclaimed
  layer
}

# --- Output grain ------------------------------------------------------------

# Decision 10's output grain, applied AFTER the engine returns rather than
# inside its year loop: the engine allocates at the granted depth, and what the
# caller gets back is a choice about reporting, not about allocation.
#
# The default folds granted-depth rows onto the container, so
# `(lon, lat, area_code, item_prod_code, year)` stays unique and the schema
# equals `main`'s. A result that carries no `level_polity_code` -- every level-0
# run -- is returned IDENTICALLY, which is what keeps the default path
# bit-for-bit.
.level_fold_output <- function(
  result,
  output_level = 0L,
  value_cols = c("rainfed_ha", "irrigated_ha")
) {
  output_level <- .check_grid_level(output_level, "output_level")
  if (!rlang::has_name(result, "level_polity_code")) {
    return(result)
  }
  if (output_level > 0L) {
    return(result)
  }
  value_cols <- intersect(value_cols, names(result))
  drop_cols <- c("level_polity_code", "level", "polycell_id", "cell_id")
  group_cols <- setdiff(names(result), c(value_cols, drop_cols))
  folded <- result |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(value_cols), \(x) sum(x, na.rm = TRUE)),
      .by = dplyr::all_of(group_cols)
    )
  cli::cli_inform(
    "Output grain: folded {nrow(result)} granted-depth row{?s} onto
     {nrow(folded)} container-keyed row{?s}."
  )
  .level_check_output_key(folded, group_cols)
  folded
}

.level_check_output_key <- function(folded, group_cols) {
  key <- intersect(
    c("lon", "lat", "area_code", "item_prod_code", "cft_name", "year"),
    group_cols
  )
  dup <- sum(duplicated(folded[key]))
  if (dup > 0L) {
    cli::cli_abort(c(
      "The container-keyed output has {dup} duplicated key row{?s}.",
      x = "Key: {.field {key}}.",
      i = "A column that varies within a container's units survived the
           fold; declare it as a value column or summarise it first."
    ))
  }
  invisible(NULL)
}

# --- The run's coverage report -----------------------------------------------

.write_admin_coverage <- function(out_dir, coverage = NULL) {
  coverage <- coverage %||% admin_coverage_prototype()
  missing <- setdiff(names(admin_coverage_prototype()), names(coverage))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.arg coverage} is missing {length(missing)} column{?s}.",
      x = "{.field {missing}}.",
      i = "Its schema is {.fn admin_coverage_prototype}."
    ))
  }
  path <- file.path(out_dir, "admin_coverage.csv")
  # Subset to the prototype's columns so the file's schema is the same in every
  # run: a missing column is refused above, an extra one is a resolver
  # diagnostic that does not belong in the run's report.
  data.table::fwrite(
    coverage[names(admin_coverage_prototype())],
    path
  )
  path
}

# --- Real-data comparison (verification surface (ii)) ------------------------

# Compare two spatialization outputs by their sorted contents, at a stated
# tolerance. Values are rounded before hashing, because a bit-level hash of two
# sums that followed different input orders differs at the last ulp and would
# report every run as changed; the row keys are compared exactly.
#
# Used by the `WHEP_SPATIALIZE_OUT_DIR` leg in
# `tests/testthat/test_spatialize_levels.R`, and unit-tested offline on two
# small tibbles so the comparison itself is verified even where the real
# outputs are not available.
.compare_spatialize_outputs <- function(
  new,
  reference,
  key_cols = c("year", "area_code", "lon", "lat", "item_prod_code"),
  value_cols = c("rainfed_ha", "irrigated_ha"),
  tolerance = 1e-9
) {
  key_cols <- intersect(key_cols, intersect(names(new), names(reference)))
  value_cols <- intersect(value_cols, intersect(names(new), names(reference)))
  digest_new <- .spatialize_content_hash(new, key_cols, value_cols, tolerance)
  digest_ref <- .spatialize_content_hash(
    reference,
    key_cols,
    value_cols,
    tolerance
  )
  list(
    identical = identical(digest_new$hash, digest_ref$hash),
    rows_new = nrow(new),
    rows_reference = nrow(reference),
    hash_new = digest_new$hash,
    hash_reference = digest_ref$hash,
    max_abs_diff = .spatialize_max_diff(
      digest_new$data,
      digest_ref$data,
      key_cols,
      value_cols
    ),
    key_cols = key_cols,
    value_cols = value_cols,
    tolerance = tolerance
  )
}

.spatialize_content_hash <- function(data, key_cols, value_cols, tolerance) {
  digits <- max(0L, as.integer(round(-log10(tolerance))))
  slim <- data |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(c(key_cols, value_cols))) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(value_cols), \(x) round(x, digits))
    ) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(key_cols)))
  list(hash = rlang::hash(as.data.frame(slim)), data = slim)
}

.spatialize_max_diff <- function(new, reference, key_cols, value_cols) {
  if (nrow(new) != nrow(reference)) {
    return(NA_real_)
  }
  joined <- dplyr::inner_join(
    new,
    reference,
    by = key_cols,
    suffix = c("_new", "_ref")
  )
  if (nrow(joined) != nrow(new)) {
    return(NA_real_)
  }
  diffs <- purrr::map_dbl(value_cols, \(cl) {
    max(abs(joined[[paste0(cl, "_new")]] - joined[[paste0(cl, "_ref")]]))
  })
  max(c(0, diffs))
}

# Peak memory of the current session, in MB. `gc()`'s per-row max-used megabyte
# column (the sixth) is the only peak figure base R exposes on every platform,
# so the floor recorded in the PR body is this and is LABELLED as such: it is
# the R heap high-water mark, not the process RSS the operating system reports,
# and it never falls until `gc(reset = TRUE)`.
.spatialize_peak_mb <- function() {
  usage <- gc(reset = FALSE, full = TRUE)
  round(sum(usage[, 6L]), 1)
}

# --- Two-level crop allocation (whep#1000 T13) -------------------------------

#' Allocate national crop areas at a granted containment depth
#'
#' @description
#' The two-level allocation: a national total is split across the
#' administrative units of its container by reported harvested-**area**
#' shares, and each unit's target is then spread across that unit's cells
#' by the gridded pattern the engine already uses, under one capacity
#' ceiling, in ONE pass per country. Items with no statistics travel in the
#' same pass on pattern-implied unit shares, so a constrained country is
#' allocated at one grain and a cell's capacity is shared by everything in
#' it.
#'
#' @section The composite key and where it must be carried:
#' A granted-depth compartment is identified by **`(area_code,
#' level_polity_code)`**: the container's integer reporting code, which
#' every `area_code`-typed join, [add_area_name()] and the `area_key`
#' switch keep working on, plus the unit's polity code from the containment
#' edge (`NA` at level 0). The pair travels through the whole chain, and
#' these are the places that had to learn it:
#'
#' \describe{
#'   \item{`.compartment_id_cols()`}{already appends `level_polity_code`
#'     (T12), so every capacity, redistribution and CFT grouping keyed on
#'     it followed for free.}
#'   \item{The engine's national-table join and share denominators}{
#'     `.spatialize_year()` joined `country_areas` to the grid on
#'     `(area_code, item_prod_code)` and formed `rf_pot_sum` /
#'     `ir_pot_sum` / `rainfed_sum` / `irrigated_sum` on the same pair.
#'     Both now use `.alloc_target_cols()`, the grain the NATIONAL TABLE is
#'     keyed at. That is the load-bearing distinction: the key comes from
#'     the targets, never from the grid, because a unit-keyed grid under a
#'     container-keyed national table is the pattern-implied case and must
#'     stay one national total.}
#'   \item{The LUH2 type-potential fallback}{`type_pot` decides per group
#'     whether a crop has any of its LUH2 type to sit in; grouped on the
#'     container it would let one unit's type cropland keep another unit
#'     out of the whole-group fallback.}
#'   \item{`.redistribute_country_dt()`}{`.crop_group` was `item_prod_code`
#'     alone. It is now the target grain inside the country, so the logit
#'     passes and the final rescale conserve each (unit, item). This is
#'     the grouping error the machine criterion pins: with the item alone,
#'     a unit whose cells are too small has its excess pushed across the
#'     border into a sibling, the two units' targets swap, and every
#'     national total still reconciles.}
#'   \item{`.warn_unallocated_crops()`}{reports at the target grain, so a
#'     unit that cannot place its target is visible instead of being
#'     averaged into its container's success.}
#'   \item{The capacity ceiling}{`.capacity_bases()` keys the unit
#'     multi-cropping factor on `(area_code, level_polity_code)`.}
#' }
#'
#' Two places do **not** carry it, for stated reasons.
#' `.redistribute_countries_dt()` still iterates `area_code`: it is a
#' memory bound on the per-country working set, and the compartment key is
#' inside each subset, so splitting further would only make the chunks
#' smaller. `.compartment_interval_groups()` stays on `(lon, lat,
#' area_code)` because `polycell_id` -- and, at depth, the unit code --
#' changes at a succession, which is exactly what its open-ended-interval
#' test must see across.
#'
#' @section What binds, and what is only measured:
#' \itemize{
#'   \item The national total binds. Admin statistics set the within-country
#'     shape and nothing else; a yield or production row never binds
#'     (decision T31(i)) and is dropped, counted, by
#'     [build_level_crop_targets()].
#'   \item The reported share binds inside the country. Where a unit's
#'     target does not fit its cells, the target wins and the ceiling gives
#'     way; the excess is measured per unit, at both multi-cropping bases,
#'     and returned in `breach` (decisions T31(c), T31(g)).
#'   \item Partial coverage raises a RESIDUAL pseudo-unit carrying
#'     `national - admin_sum`, supported by the non-reporting units' cells
#'     only. With complete coverage there is no residual: the reported units
#'     rescale proportionally and the leftover is a diagnostic, refused only
#'     beyond both tolerances (decisions T31(a), T31(d)).
#' }
#'
#' @param country_areas National crop areas, as
#'   [build_gridded_landuse()] takes them: `year`, `area_code`,
#'   `item_prod_code`, `harvested_area_ha` and optionally
#'   `irrigated_area_ha`. Keyed on the CONTAINER; the unit split is this
#'   function's job. A negative `harvested_area_ha` is refused first, as
#'   `whep_alloc_negative_national`: the irrigation check would otherwise
#'   read it as a row with more irrigated than harvested area and say so.
#' @param crop_patterns Per-cell crop pattern, as
#'   [build_gridded_landuse()] takes it.
#' @param gridded_cropland Per-cell cropland extent, as
#'   [build_gridded_landuse()] takes it.
#' @param allocation_layer One country grid choosing a depth per country,
#'   from [build_allocation_layer()]: granted countries arrive as their
#'   units, every other country as its level-0 row.
#' @param admin_shares Resolved admin shares -- the `shares` element of
#'   [resolve_admin_shares()], optionally completed by
#'   [backcast_admin_shares()]. `NULL` allocates every item on
#'   pattern-implied unit shares.
#' @param config Named list. Every [build_gridded_landuse()] key is
#'   forwarded (and validated there); `pattern_extension` is fixed to
#'   `"granted_units"` by decision T31(b). This function's own keys are
#'   `tolerance_relative` (0.10), `tolerance_absolute` (1000 ha) and
#'   `conservation_tolerance` (1e-6, relative).
#'
#' @return A list of seven tibbles:
#'
#' - `allocation`: the gridded rows, at unit grain, exactly the
#'   [build_gridded_landuse()] schema plus `level_polity_code`.
#' - `targets`: one row per `(year, area_code, level_polity_code,
#'   item_prod_code)` with `share`, `target_ha`, `irrigated_target_ha`,
#'   `rainfed_target_ha`, `irrigation_clipped_ha` and the regime,
#'   `method_crop_alloc`.
#' - `coverage`: one row per `(year, area_code, item_prod_code)` with
#'   `n_units`, `n_units_reporting`, `coverage`, `admin_sum`,
#'   `residual_target_ha`, `discrepancy_ha` and the denominator `basis`.
#' - `breach`: the capacity excess per `(year, area_code,
#'   level_polity_code, item_prod_code, mc_basis)`.
#' - `straddle`: per unit and item, `n_cells`, `straddle_sibling`,
#'   `straddle_foreign` and `cell_limited`.
#' - `conservation`: allocated against target, at container and unit grain.
#'   The container's target is the NATIONAL total, so hectares that became no
#'   unit's target are visible here as well as in `coverage$dropped_ha`; the
#'   unit rows are warned about only where their container reconciles, which
#'   is the sibling-absorption failure they exist to see.
#' - `bridges`: per `(area_code, item_prod_code, treatment)`, how many years
#'   of the shares were not observed and the longest contiguous run of them,
#'   so a 60-year LUH2 bridge is visible rather than merely legal (decision
#'   T31(e)).
#'
#' @seealso [build_level_crop_targets()], [build_allocation_layer()],
#'   [build_gridded_landuse()].
#' @export
#'
#' @examples
#' # Two units of container 1 (Armenia), one cell each, one crop.
#' layer <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
#'   0.25, 50.25,         1L, "A-A1-1900-2100",       1L,               1,
#'   0.75, 50.25,         1L, "A-A2-1900-2100",       1L,               1
#' )
#' shares <- tibble::tribble(
#'   ~area_code, ~level_polity_code, ~item_prod_code, ~year, ~value,
#'           1L, "A-A1-1900-2100",               15L, 2000L,     150,
#'           1L, "A-A2-1900-2100",               15L, 2000L,     100
#' ) |>
#'   dplyr::mutate(level = 1L, indicator_used = "area_harvested")
#' out <- allocate_level_crops(
#'   country_areas = tibble::tibble(
#'     year = 2000L, area_code = 1L, item_prod_code = 15L,
#'     harvested_area_ha = 250
#'   ),
#'   crop_patterns = tibble::tibble(
#'     lon = c(0.25, 0.75), lat = 50.25, item_prod_code = 15L,
#'     harvest_fraction = c(0.5, 0.5)
#'   ),
#'   gridded_cropland = tibble::tibble(
#'     lon = c(0.25, 0.75), lat = 50.25, year = 2000L,
#'     cropland_ha = c(1000, 1000)
#'   ),
#'   allocation_layer = layer,
#'   admin_shares = shares
#' )
#' out$targets[c("level_polity_code", "target_ha", "method_crop_alloc")]
allocate_level_crops <- function(
  country_areas,
  crop_patterns,
  gridded_cropland,
  allocation_layer,
  admin_shares = NULL,
  config = list()
) {
  .check_columns(
    country_areas,
    c("year", "area_code", "item_prod_code", "harvested_area_ha"),
    "country_areas"
  )
  layer <- .normalize_country_grid(allocation_layer, "allocation_layer")
  .alloc_check_layer(layer)
  split <- .alloc_split_config(config)
  national <- .ensure_irrigation_cols(tibble::as_tibble(country_areas))
  # The sign check runs FIRST, because the irrigation check reads a negative
  # harvested area as a row with more irrigated than harvested area and
  # refuses it in those words -- describing an input the caller never gave.
  .alloc_check_negative_national(national)
  .check_irrigation_within_area(national)
  national <- .alloc_filter_years(national, split$engine$years)
  weights <- .alloc_unit_weights(
    national,
    crop_patterns,
    gridded_cropland,
    layer,
    split$engine
  )
  .alloc_warn_unweighted(national, weights)
  built <- build_level_crop_targets(
    national,
    weights,
    admin_shares,
    tolerance_relative = split$own$tolerance_relative,
    tolerance_absolute = split$own$tolerance_absolute
  )
  .alloc_warn_dropped(built$coverage)
  parts <- .alloc_run_engine(
    .alloc_engine_areas(built$targets),
    crop_patterns,
    gridded_cropland,
    layer,
    split$engine
  )
  list(
    allocation = parts$allocation,
    targets = built$targets,
    coverage = built$coverage,
    breach = .alloc_unit_breach(parts$breach),
    straddle = .alloc_straddle(parts$allocation, layer),
    conservation = .alloc_conservation(
      parts$allocation,
      built$targets,
      split$own$conservation_tolerance
    ),
    bridges = .alloc_bridge_report(admin_shares)
  )
}

#' Split a national crop total across the units of its container
#'
#' @description
#' The first half of [allocate_level_crops()], exposed on its own because
#' the reconciliation diagnostics read it: it turns national totals plus
#' resolved admin shares into one target per `(year, container, unit,
#' item)`, and says on every row which regime produced it.
#'
#' @section How the denominator is chosen:
#' Per `(year, area_code, item_prod_code)`, writing `D` for the
#' denominator each unit's reported value is divided by:
#'
#' \describe{
#'   \item{`"pattern"`}{No unit reports. Every unit's share is its own
#'     pattern weight over the group's, `method_crop_alloc =
#'     "pattern_implied"` -- or `"pattern_national"` where the container is
#'     at level 0 and is therefore its own single unit.}
#'   \item{`"admin_sum"`}{Every unit of the layer reports a value:
#'     complete coverage, so `D = admin_sum` and the reported units rescale
#'     proportionally onto the national total. The leftover is a
#'     diagnostic; it is refused only when it breaks BOTH tolerances
#'     (decision T31(d)).}
#'   \item{`"residual"`}{Some units report a value: `D = max(admin_sum,
#'     national)`, so a reported value binds in absolute hectares while
#'     `admin_sum` is below the national total, and rescales down when it
#'     is above. The residual, `1 - admin_sum / D` of the total, is split
#'     across the units that reported NO absolute area, and never touches a
#'     unit that did (decision T31(a)). Those units' pattern weights split
#'     it -- unless every one of them declares a share, in which case the
#'     declared shares do, because that is what a declared share states.}
#'   \item{`"share_normalised"`}{The rows carry a `share` and no `value`,
#'     which is what [backcast_admin_shares()] produces: the shares already
#'     sum to 1 over the units they cover, so they are used as they stand
#'     and a unit outside them gets zero. `method_crop_alloc =
#'     "admin_backcast_luh2"`.}
#' }
#'
#' `method_crop_alloc` therefore takes one of `"pattern_national"` (a
#' container the layer holds at level 0, which is its own single unit),
#' `"pattern_implied"`, `"admin_area_shares"`, `"admin_backcast_luh2"`,
#' `"admin_residual"` (a unit carrying its share of the residual on the
#' weights that split it -- whether because it reported nothing, or because
#' it declared a share the split did not run on) and `"unallocated"` (a unit
#' whose target is 0: the shares do not cover it and no residual reaches it,
#' or the group had neither pattern weight nor cropland for anything to be
#' split on, `coverage$weight_basis == "none"`). The column names what placed
#' the hectares, so a unit whose declared share was displaced by the pattern
#' weights is `"admin_residual"` and not `"admin_area_shares"`, and a group
#' the pattern never ran on is `"unallocated"` and not `"pattern_implied"`.
#'
#' A group whose reported values are all zero while the national total is
#' positive cannot set a shape. Under complete coverage every share is 0,
#' the hectares are counted in `coverage$dropped_ha`, and the T31(d)
#' refusal fires on any group where that matters (the discrepancy is then
#' 100% of the national total). Where that leaves NO positive target at all,
#' [allocate_level_crops()] returns the empty allocation beside this
#' coverage rather than calling an engine with nothing to place.
#'
#' An admin-share row carrying no `level_polity_code` names no unit -- that
#' is what [resolve_admin_units()] leaves on a unit it could not resolve --
#' and is dropped, counted and named before the split. `NA` is the layer's
#' own value for "this container is at level 0 and IS its own unit", so such
#' a row would otherwise bind the container's own row and publish one
#' unresolved province as the country's complete subnational evidence.
#'
#' @section What counts as a report:
#' One predicate. A unit reports when it carries a `value`, a `share`, or
#' both, and `coverage$n_units_reporting` counts exactly the units that then
#' bind. `reports_value` -- an ABSOLUTE area -- is a narrower question, and
#' it decides two things only: which denominator the group uses, and which
#' units the residual is spread over. So a unit declaring a share and no
#' value is a reporter, and it takes a slice of the residual the valued
#' units left. What sizes that slice is a property of the candidate SET,
#' not of the unit: where every residual candidate declares a share, those
#' shares split the residual (`coverage$weight_basis` is then
#' `"declared"`) and the declared share binds; where only some do, the
#' pattern weights split it, because a declared fraction and a hectare of
#' potential are not the same quantity. In that second case the unit is
#' allocated exactly as a silent one is, and `method_crop_alloc` says so.
#'
#' A negative reported value, a negative declared share and a negative
#' derived target are all refused (classes `whep_alloc_negative_value` and
#' `whep_alloc_negative_target`); [allocate_level_crops()] refuses a
#' negative national total ahead of both, as
#' `whep_alloc_negative_national`. A negative target is dropped by the
#' engine's positivity filter, after which the remaining units divide the
#' whole national total between them: the container over-allocates and
#' nothing downstream can see the row that is not there.
#'
#' @section Irrigation:
#' The unit's irrigated target is the national irrigated area split by the
#' engine's own `ir_potential` aggregated per unit, so the two steps
#' compose; where a group has no irrigated potential the split falls to the
#' units' irrigated cropland, and where there is none of that either the
#' irrigated area is unplaceable and is reported as such, because the
#' engine would drop it too. The rainfed target is the remainder, and the
#' irrigated target is clipped at the unit's area target so the remainder
#' is never negative -- the engine computes `harvested - irrigated` with no
#' clipping and lets a negative row through its output filter. Clipped
#' hectares are reported per unit in `irrigation_clipped_ha` and are NOT
#' redistributed to units with room: that would be an allocation rule
#' nothing has decided.
#'
#' @param country_areas National crop areas keyed on the container; see
#'   [allocate_level_crops()].
#' @param unit_weights Per-unit pattern weights, from
#'   `.alloc_unit_weights()`: `year`, `area_code`, `level_polity_code`,
#'   `item_prod_code`, `weight_rainfed`, `weight_irrigated`,
#'   `cropland_rainfed_ha`, `cropland_irrigated_ha`, `n_cells`.
#' @param admin_shares Resolved admin shares, or `NULL`.
#' @param tolerance_relative Relative discrepancy above which a
#'   complete-coverage group is refused, `0.10` by decision T31(d).
#' @param tolerance_absolute Absolute discrepancy, in hectares, above which
#'   the same group is refused, `1000` by decision T31(d). BOTH must be
#'   breached.
#'
#' @return A list of two tibbles, `targets` and `coverage`; see
#'   [allocate_level_crops()] for their columns.
#'
#' @export
#'
#' @examples
#' weights <- tibble::tibble(
#'   year = 2000L,
#'   area_code = 1L,
#'   level_polity_code = c("A1", "A2"),
#'   item_prod_code = 15L,
#'   weight_rainfed = c(400, 600),
#'   weight_irrigated = 0,
#'   cropland_rainfed_ha = c(800, 1200),
#'   cropland_irrigated_ha = 0,
#'   n_cells = 1L
#' )
#' national <- tibble::tibble(
#'   year = 2000L, area_code = 1L, item_prod_code = 15L,
#'   harvested_area_ha = 1000
#' )
#' build_level_crop_targets(national, weights)$targets
build_level_crop_targets <- function(
  country_areas,
  unit_weights,
  admin_shares = NULL,
  tolerance_relative = 0.1,
  tolerance_absolute = 1000
) {
  .check_columns(
    unit_weights,
    c(
      "year",
      "area_code",
      "level_polity_code",
      "item_prod_code",
      "weight_rainfed",
      "weight_irrigated"
    ),
    "unit_weights"
  )
  national <- .ensure_irrigation_cols(tibble::as_tibble(country_areas))
  shares <- .alloc_prepare_shares(admin_shares)
  placeable <- unit_weights |>
    tibble::as_tibble() |>
    dplyr::inner_join(
      national,
      by = c("year", "area_code", "item_prod_code"),
      relationship = "many-to-one"
    )
  .alloc_warn_unmatched_shares(shares, placeable)
  rows <- placeable |>
    dplyr::left_join(
      shares,
      by = c("year", "area_code", "level_polity_code", "item_prod_code"),
      relationship = "many-to-one",
      # `.alloc_prepare_shares()` has already removed every unresolved row, so
      # this cannot bind `NA` to the level-0 container's `NA`. Said here too:
      # a share row that names no unit must never match one, and dplyr's
      # default is that it does.
      na_matches = "never"
    ) |>
    .alloc_group_state() |>
    .alloc_area_shares()
  # Before the irrigation split, not after: the split clips the irrigated
  # target at the area target, so a negative area target makes it report
  # hectares "clipped" that only exist because the target is impossible.
  .alloc_check_negative_targets(rows)
  rows <- .alloc_irrigation_split(rows)
  .alloc_refuse_discrepancy(rows, tolerance_relative, tolerance_absolute)
  list(targets = .alloc_targets_out(rows), coverage = .alloc_coverage(rows))
}

# --- Configuration -----------------------------------------------------------

.alloc_own_defaults <- function() {
  list(
    tolerance_relative = 0.1,
    tolerance_absolute = 1000,
    conservation_tolerance = 1e-6
  )
}

# This function's own keys are peeled off before the rest goes to the
# engine, which refuses a key it does not know. `pattern_extension` is not
# among either: decision T31(b) fixes it at `"granted_units"` here.
.alloc_split_config <- function(config) {
  if (!is.list(config) || (length(config) > 0L && is.null(names(config)))) {
    cli::cli_abort("{.arg config} must be a named list.")
  }
  own <- utils::modifyList(
    .alloc_own_defaults(),
    config[intersect(names(config), names(.alloc_own_defaults()))]
  )
  engine <- config[setdiff(names(config), names(.alloc_own_defaults()))]
  if (!is.null(engine$cft_mapping)) {
    cli::cli_abort(c(
      "{.arg cft_mapping} is not allocated through {.fn allocate_level_crops}.",
      i = "The diagnostics it returns are keyed on {.field item_prod_code};
           aggregate the {.field allocation} to CFTs afterwards."
    ))
  }
  if (!is.null(engine$pattern_extension)) {
    cli::cli_abort(
      "{.arg pattern_extension} is fixed at {.val granted_units} here."
    )
  }
  engine <- utils::modifyList(.landuse_config_defaults(), engine)
  engine$pattern_extension <- "granted_units"
  list(own = own, engine = engine)
}

.alloc_check_layer <- function(layer) {
  granted <- rlang::has_name(layer, "level_polity_code") &&
    any(!is.na(layer$level_polity_code))
  if (granted) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg allocation_layer} grants no country a depth.",
      x = "No row carries a {.field level_polity_code}.",
      i = "A layer with nothing granted is a level-0 grid; allocate it with
           {.fn build_gridded_landuse}, which is the unchanged path."
    ),
    class = "whep_alloc_no_granted_depth"
  )
}

.alloc_filter_years <- function(national, years) {
  if (is.null(years)) {
    return(national)
  }
  dplyr::filter(national, year %in% as.integer(years))
}

# --- Unit weights: the engine's own pre-capacity potential -------------------

# The weights the unit split uses are computed with the ENGINE's helpers on
# the ENGINE's inputs, not by a second implementation of the potential: the
# plan's irrigation rule ("the engine's `ir_potential`, aggregated per
# unit") only composes if the two are the same quantity.
.alloc_unit_weights <- function(
  national,
  crop_patterns,
  gridded_cropland,
  layer,
  engine
) {
  ctx <- list(
    national = national,
    crop_patterns = crop_patterns,
    cropland = .ensure_gridded_irrigation(gridded_cropland),
    layer = layer,
    engine = engine,
    type_lookup = .alloc_type_lookup(engine)
  )
  years <- sort(unique(as.integer(national$year)))
  purrr::map(years, \(yr) .alloc_weights_one_year(yr, ctx)) |>
    purrr::list_rbind()
}

.alloc_type_lookup <- function(engine) {
  if (is.null(engine$type_cropland) || is.null(engine$type_mapping)) {
    return(NULL)
  }
  engine$type_mapping |>
    dplyr::select("item_prod_code", "luh2_type") |>
    dplyr::distinct()
}

.alloc_weights_one_year <- function(yr, ctx) {
  grid_yr <- .filter_country_grid_year(ctx$layer, yr)
  if (nrow(grid_yr) == 0L) {
    cli::cli_abort("No {.arg allocation_layer} rows valid for year {yr}.")
  }
  ca <- dplyr::filter(ctx$national, year == yr)
  base <- .build_base_grid_cp(grid_yr, ctx$crop_patterns, ctx$type_lookup) |>
    .extend_base_grid_pattern(
      grid_yr,
      ca,
      list(pattern_extension = "granted_units")
    )
  .spatialize_cell_potential(
    base,
    dplyr::filter(ctx$cropland, year == yr),
    .alloc_type_cropland_year(ctx$engine$type_cropland, yr),
    .alloc_unit_item_cols(),
    yr
  ) |>
    tibble::as_tibble() |>
    dplyr::summarise(
      weight_rainfed = sum(harvest_fraction * rainfed_ha, na.rm = TRUE),
      weight_irrigated = sum(harvest_fraction * irrigated_ha, na.rm = TRUE),
      cropland_rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
      cropland_irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
      n_cells = dplyr::n(),
      .by = dplyr::all_of(.alloc_unit_item_cols())
    ) |>
    dplyr::mutate(year = yr, .before = 1L)
}

.alloc_type_cropland_year <- function(type_cropland, yr) {
  if (is.null(type_cropland)) {
    return(NULL)
  }
  dplyr::filter(type_cropland, year == yr)
}

.alloc_unit_item_cols <- function() {
  c("area_code", "level_polity_code", "item_prod_code")
}

.alloc_group_cols <- function() {
  c("year", "area_code", "item_prod_code")
}

# --- Admin shares ------------------------------------------------------------

# Only an AREA indicator binds (decision T31(i)): a production or yield row
# is dropped and counted, never used as a share, because a share of a ratio
# is not a share and production-implied shares are a T14 diagnostic.
.alloc_prepare_shares <- function(admin_shares) {
  proto <- tibble::tibble(
    year = integer(),
    area_code = integer(),
    level_polity_code = character(),
    item_prod_code = integer(),
    value = numeric(),
    share_reported = numeric(),
    treatment = character()
  )
  if (is.null(admin_shares) || nrow(admin_shares) == 0L) {
    return(proto)
  }
  .check_columns(
    admin_shares,
    c("year", "area_code", "level_polity_code", "item_prod_code"),
    "admin_shares"
  )
  rows <- tibble::as_tibble(admin_shares)
  rows <- .alloc_drop_non_area(rows)
  rows <- rows |>
    dplyr::mutate(
      year = as.integer(year),
      area_code = as.integer(area_code),
      item_prod_code = as.integer(item_prod_code),
      value = if (rlang::has_name(rows, "value")) {
        as.numeric(value)
      } else {
        NA_real_
      },
      share_reported = if (rlang::has_name(rows, "share")) {
        as.numeric(share)
      } else {
        NA_real_
      },
      treatment = if (rlang::has_name(rows, "treatment")) {
        as.character(treatment)
      } else {
        "observed"
      }
    ) |>
    dplyr::select(dplyr::all_of(names(proto)))
  rows <- .alloc_drop_unresolved(rows)
  .alloc_check_share_key(rows)
  .alloc_check_negative_values(rows)
  rows
}

# THE TWO MEANINGS OF A MISSING `level_polity_code`, separated where they meet.
# In the allocation layer it says "this container is at level 0 and IS its own
# single unit"; in the admin-share contract it says "`resolve_admin_units()`
# could not resolve this unit to a polity, and the row is kept visible rather
# than dropped" (R/admin_shares.R). The target join matches `NA` to `NA`, so an
# unresolved province bound its country's own level-0 row: one row became the
# country's complete subnational evidence, `.alloc_warn_unmatched_shares()`
# stayed silent because the code DID match, and the group read as
# `admin_area_shares` with full coverage. At a value near the national total
# that publishes one province as the whole country; at a small one it aborts
# the run on a discrepancy no province reported.
#
# The fix is at the root and not at the join: after this, `NA` never reaches
# the allocation from the share side, so the value carries exactly one meaning
# for the rest of the file. The rows are dropped rather than refused, which is
# what the sibling case (a unit the layer does not carry) already does, and
# they are named, because dropping them lowers the group's coverage.
.alloc_drop_unresolved <- function(rows) {
  unresolved <- dplyr::filter(rows, is.na(level_polity_code))
  if (nrow(unresolved) == 0L) {
    return(rows)
  }
  codes <- sort(unique(unresolved$area_code))
  cli::cli_warn(c(
    "{nrow(unresolved)} admin-share row{?s} carry no
     {.field level_polity_code} and so name no unit.",
    "x" = "{length(codes)} container{?s}: {.val {utils::head(codes, 5L)}}.",
    i = "A missing unit code means the layer's level-0 container, so such a
         row would bind the container's own row as though a province had
         reported it. Resolve the unit codes with {.fn resolve_admin_units}
         first; until then the group is treated as reporting fewer units."
  ))
  dplyr::filter(rows, !is.na(level_polity_code))
}

# FAIL CLOSED ON A NEGATIVE REPORTED AREA. A negative value makes a negative
# unit target; `.alloc_engine_areas()` then drops that row on its
# `target_ha > 0` filter, and the units that remain divide the whole national
# total between them -- so the container allocates MORE than its target (600
# ha against 500 on a two-unit fixture) while every surviving row looks
# ordinary and no conservation check can see the row that is not there.
# "Negative harvested area" has no reading, so it is refused where it enters
# rather than repaired somewhere downstream.
.alloc_check_negative_values <- function(rows) {
  bad <- dplyr::filter(
    rows,
    dplyr::coalesce(value, 0) < 0 | dplyr::coalesce(share_reported, 0) < 0
  )
  if (nrow(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- bad[
    which.min(pmin(
      dplyr::coalesce(bad$value, 0),
      dplyr::coalesce(bad$share_reported, 0)
    )),
    ,
    drop = FALSE
  ]
  cli::cli_abort(
    c(
      "{nrow(bad)} {.arg admin_shares} row{?s} report a negative area or
       share.",
      x = "Worst: unit {.val {worst$level_polity_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           value {.val {worst$value}}, share
           {.val {worst$share_reported}}.",
      i = "A negative target is dropped by the engine's positivity filter,
           and the remaining units then split the whole national total, so
           the container over-allocates with nothing left to show it."
    ),
    class = "whep_alloc_negative_value"
  )
}

# The same failure reached from the other side: a negative NATIONAL total, or
# any arithmetic that produces one, makes a negative unit target that the
# engine's positivity filter drops just as silently.
.alloc_check_negative_targets <- function(rows) {
  bad <- dplyr::filter(rows, target_ha < 0)
  if (nrow(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- bad[which.min(bad$target_ha), , drop = FALSE]
  cli::cli_abort(
    c(
      "{nrow(bad)} unit-item target{?s} are negative.",
      x = "Worst: unit {.val {worst$level_polity_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           {.val {worst$target_ha}} ha from a national
           {.val {worst$harvested_area_ha}} ha.",
      i = "The engine drops a non-positive target, so the units that remain
           would divide the whole national total between them."
    ),
    class = "whep_alloc_negative_target"
  )
}

# And the input that reaches it, refused where the caller can still see it as
# an input. `.alloc_check_negative_targets()` runs on DERIVED targets, after
# the split, so through [allocate_level_crops()] the irrigation gate spoke
# first and named the wrong quantity. The two are not redundant: this one
# says the national total is impossible, that one says the split produced an
# impossible unit target from a national total that was not.
.alloc_check_negative_national <- function(national) {
  bad <- dplyr::filter(national, harvested_area_ha < 0)
  if (nrow(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- bad[which.min(bad$harvested_area_ha), , drop = FALSE]
  cli::cli_abort(
    c(
      "{nrow(bad)} {.arg country_areas} national total{?s} are negative.",
      x = "Worst: area_code {.val {worst$area_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           {.val {worst$harvested_area_ha}} ha harvested.",
      i = "There is no share of a negative area to allocate: every unit
           target it produces is negative, and the engine's positivity
           filter drops them all."
    ),
    class = "whep_alloc_negative_national"
  )
}

.alloc_drop_non_area <- function(rows) {
  if (!rlang::has_name(rows, "indicator_used")) {
    return(rows)
  }
  area_indicators <- c(
    "area_harvested",
    "area_planted_or_sown",
    "area_main",
    "area_cultivated"
  )
  keep <- rows$indicator_used %in% area_indicators
  if (all(keep)) {
    return(rows)
  }
  dropped <- sort(unique(rows$indicator_used[!keep]))
  cli::cli_warn(c(
    "{sum(!keep)} admin-share row{?s} do not carry an area indicator and
     cannot bind an allocation.",
    "x" = "Dropped indicator{?s}: {.val {dropped}}.",
    i = "Decision T31(i): the anchor is the first observed AREA year;
         production stays a reconciliation diagnostic."
  ))
  rows[keep, , drop = FALSE]
}

.alloc_check_share_key <- function(rows) {
  key <- c("year", "area_code", "level_polity_code", "item_prod_code")
  dup <- sum(duplicated(rows[key]))
  if (dup == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg admin_shares} has {dup} row{?s} repeating a
       (year, container, unit, item) key.",
      i = "Resolve the sources first with {.fn resolve_admin_shares}: two
           rows for one unit-year would each claim the same share."
    ),
    class = "whep_alloc_duplicate_shares"
  )
}


# An admin row whose unit the layer cannot reach would vanish in the join
# that builds the targets, taking its evidence with it: the group would
# then look like one with fewer reporters, `admin_sum` would fall, and a
# residual would be raised to cover a unit that did report. Reported, never
# silently dropped.
.alloc_warn_unmatched_shares <- function(shares, placeable) {
  key <- c("year", "area_code", "level_polity_code", "item_prod_code")
  orphan <- dplyr::anti_join(shares, placeable, by = key)
  if (nrow(orphan) == 0L) {
    return(invisible(NULL))
  }
  units <- sort(unique(orphan$level_polity_code))
  cli::cli_warn(c(
    "{nrow(orphan)} admin-share row{?s} name a unit the allocation layer
     does not carry for that container, item and year.",
    "x" = "{length(units)} unit{?s}: {.val {utils::head(units, 5L)}}.",
    i = "Their values do not enter {.field admin_sum}, so the group is
         treated as reporting fewer units. Regenerate the layer at the
         granted depth, or resolve the unit codes."
  ))
}

# The national totals no unit can receive: the layer holds no cell for that
# container, or the container's cells carry neither the crop's pattern nor
# any cropland to spread it over. `build_gridded_landuse()` warns about the
# same mass one step later; saying it here names the target that was never
# built rather than the allocation that came out empty.
.alloc_warn_unweighted <- function(national, weights) {
  key <- c("year", "area_code", "item_prod_code")
  orphan <- national |>
    dplyr::filter(harvested_area_ha > 0) |>
    dplyr::anti_join(weights, by = key)
  if (nrow(orphan) == 0L) {
    return(invisible(NULL))
  }
  codes <- sort(unique(orphan$area_code))
  cli::cli_warn(c(
    "{nrow(orphan)} (container, item, year) national total{?s} have no unit
     to receive them; {round(sum(orphan$harvested_area_ha))} ha unplaced.",
    "x" = "{length(codes)} area_code{?s}: {.val {utils::head(codes, 5L)}}."
  ))
}

# The hectares that became NO unit's target. `.alloc_warn_unweighted()` cannot
# see them: it anti-joins on (year, container, item), and a group whose units
# all carry a weights row OF ZEROS matches that key and passes. The engine
# cannot see them either, because `.alloc_engine_areas()` filters
# `target_ha > 0` and the group never reaches it -- so
# `build_gridded_landuse()`'s own "no allocatable grid cell" warning, which
# fires on exactly this mass one step later on the national path, never fires
# here. `coverage$dropped_ha` recorded it and nothing in the package read it.
#
# The threshold is relative as well as absolute so that float noise in
# `1 - sum(share)` cannot raise a warning about a hectare that was placed.
.alloc_warn_dropped <- function(coverage, tol = 1e-6) {
  dropped <- dplyr::filter(
    coverage,
    .data$national_total_ha > 0,
    .data$dropped_ha > pmax(tol, 1e-9 * .data$national_total_ha)
  )
  if (nrow(dropped) == 0L) {
    return(invisible(NULL))
  }
  codes <- sort(unique(dropped$area_code))
  cli::cli_warn(c(
    "{nrow(dropped)} (container, item, year) group{?s} turned part of a
     national total into no unit target; {round(sum(dropped$dropped_ha))} ha
     dropped.",
    "x" = "{length(codes)} area_code{?s}: {.val {utils::head(codes, 5L)}}.",
    i = "The hectares are in {.field coverage$dropped_ha}, and
         {.field coverage$weight_basis} says whether the units had any
         pattern or cropland to receive them."
  ))
}

# --- The split ---------------------------------------------------------------

# ONE PREDICATE FOR "THIS UNIT REPORTED". `reports` is what `coverage`
# counts and what `is_reporter` means, because they were two different
# predicates and the difference was silent: a unit declaring a SHARE and no
# value counted towards `coverage` while `is_reporter` (on `reports_value`)
# treated it as a non-reporter, so its declared shape was replaced by the
# frozen pattern weights. Decision 8 says a declared share binds, so it is a
# report on both counts; `reports_value` survives only where the question is
# specifically whether an ABSOLUTE area was stated, which is what selects the
# denominator and the residual candidates.
.alloc_group_state <- function(rows) {
  rows |>
    dplyr::mutate(
      reports_value = !is.na(value),
      reports = !is.na(share_reported) | !is.na(value)
    ) |>
    dplyr::mutate(
      n_units = dplyr::n(),
      n_units_reporting = sum(reports),
      n_units_valued = sum(reports_value),
      admin_sum = sum(value[reports_value]),
      share_sum = sum(share_reported[reports & !reports_value]),
      .by = dplyr::all_of(.alloc_group_cols())
    ) |>
    dplyr::mutate(
      basis = dplyr::case_when(
        n_units_reporting == 0L ~ "pattern",
        n_units_valued == 0L ~ "share_normalised",
        n_units_valued == n_units ~ "admin_sum",
        .default = "residual"
      ),
      denominator = dplyr::case_when(
        basis == "admin_sum" ~ admin_sum,
        basis == "residual" ~ pmax(admin_sum, harvested_area_ha),
        .default = NA_real_
      ),
      is_reporter = dplyr::if_else(basis == "pattern", FALSE, reports)
    )
}

# The weight of a unit inside the set the residual (or the whole total, under
# `"pattern"`) is split over: the units that stated no ABSOLUTE area. Where
# every one of them declares a share, those shares set the split, because a
# declared share is a statement about exactly this shape and decision 8 says
# it binds; where only some do, the declared shares cannot define a split of
# the whole set and the pattern weights do it instead -- a declared share and
# a pattern weight are not the same quantity and adding them would compare
# hectares of potential with a fraction. `weight_rainfed + weight_irrigated`
# is the engine's own pre-capacity potential; where the whole candidate set
# has none the split falls to the units' cropland, which is the same uniform
# branch the engine takes inside a unit (decision T31(b)), and where there is
# no cropland either the group is unplaceable and its share is zero.
.alloc_area_shares <- function(rows) {
  rows |>
    dplyr::mutate(
      weight_area = weight_rainfed + weight_irrigated,
      land_area = cropland_rainfed_ha + cropland_irrigated_ha,
      candidate = !reports_value
    ) |>
    dplyr::mutate(
      candidate_weight = sum(weight_area[candidate]),
      candidate_land = sum(land_area[candidate]),
      candidate_declared = sum(share_reported[candidate], na.rm = TRUE),
      n_candidates = sum(candidate),
      n_declared = sum(candidate & !is.na(share_reported)),
      .by = dplyr::all_of(.alloc_group_cols())
    ) |>
    dplyr::mutate(
      weight_basis = dplyr::case_when(
        n_candidates == 0L ~ NA_character_,
        n_declared == n_candidates & candidate_declared > 0 ~ "declared",
        candidate_weight > 0 ~ "pattern",
        candidate_land > 0 ~ "cropland",
        .default = "none"
      ),
      candidate_share = dplyr::case_when(
        !candidate ~ 0,
        weight_basis %in% "declared" ~
          .alloc_safe_ratio(share_reported, candidate_declared),
        weight_basis %in% "pattern" ~ weight_area / candidate_weight,
        weight_basis %in% "cropland" ~ land_area / candidate_land,
        .default = 0
      ),
      residual_share = dplyr::if_else(
        basis == "residual",
        1 - admin_sum / denominator,
        0
      ),
      share = .alloc_unit_share(
        basis,
        is_reporter,
        reports_value,
        value,
        share_reported,
        denominator,
        share_sum,
        residual_share,
        candidate_share
      ),
      method_crop_alloc = .alloc_method(
        basis,
        is_reporter,
        reports_value,
        weight_basis,
        treatment
      ),
      target_ha = share * harvested_area_ha
    )
}

# `dplyr::case_when()` evaluates every branch, so a zero denominator is
# guarded rather than relied on to be unreachable: `0 / 0` in an unselected
# branch still produces the NaN that would silently become a target.
#
# The `!reports_value` branch is the one a mixed group needs: a unit that
# declared a share and no absolute area is a reporter, so it never falls to
# the frozen pattern, and it takes its declared slice of the residual the
# valued units left. Its `candidate_share` is that declared share, from
# `.alloc_area_shares()`.
.alloc_unit_share <- function(
  basis,
  is_reporter,
  reports_value,
  value,
  share_reported,
  denominator,
  share_sum,
  residual_share,
  candidate_share
) {
  dplyr::case_when(
    basis == "pattern" ~ candidate_share,
    !is_reporter & basis == "residual" ~ residual_share * candidate_share,
    !is_reporter ~ 0,
    basis == "share_normalised" ~
      .alloc_safe_ratio(share_reported, share_sum),
    !reports_value ~ residual_share * candidate_share,
    .default = .alloc_safe_ratio(value, denominator)
  )
}

.alloc_safe_ratio <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, 0)
}

# `method_crop_alloc` names what actually placed the hectares, so a unit
# whose declared share the split never used must not be named for it.
.alloc_method <- function(
  basis,
  is_reporter,
  reports_value,
  weight_basis,
  treatment
) {
  observed <- is.na(treatment) | treatment == "observed"
  declared <- weight_basis %in% "declared"
  dplyr::case_when(
    # No weight and no cropland anywhere in the group: every share is 0 and
    # the pattern regime never ran, so naming the row for it would credit
    # the LUH2 pattern with hectares nothing placed. `weight_basis` is the
    # only column that separates this from a real pattern split, and it is
    # group-grain, so a consumer of `targets` alone could not.
    basis == "pattern" & weight_basis %in% "none" ~ "unallocated",
    basis == "pattern" ~ "pattern_implied",
    !is_reporter & basis == "residual" ~ "admin_residual",
    # A unit the group's own shares do not cover, where there is no residual
    # to give it either: it receives nothing, and saying "pattern_implied"
    # would claim a regime it never ran.
    !is_reporter ~ "unallocated",
    # A reporter that declared a share and no ABSOLUTE area takes a slice of
    # the residual, and its declared share sets that slice only where every
    # residual candidate declares one. Where only some do, the frozen
    # pattern weights split it instead: the row was then allocated exactly
    # as a silent unit's was, so it carries the same name. Either
    # "admin_area_shares" or "admin_backcast_luh2" here would credit a
    # declared share for hectares the pattern placed.
    basis == "residual" & !reports_value & !declared ~ "admin_residual",
    basis == "share_normalised" ~ "admin_backcast_luh2",
    !observed ~ "admin_backcast_luh2",
    .default = "admin_area_shares"
  )
}

# A container the layer holds at level 0 is its own single unit, and its
# "pattern-implied" share is 1: the regime it is actually running is the
# unchanged national one, so it is named as such.
.alloc_mark_level0 <- function(rows) {
  dplyr::mutate(
    rows,
    method_crop_alloc = dplyr::if_else(
      is.na(level_polity_code) & method_crop_alloc == "pattern_implied",
      "pattern_national",
      method_crop_alloc
    )
  )
}

.alloc_irrigation_split <- function(rows) {
  rows |>
    dplyr::mutate(
      ir_weight_sum = sum(weight_irrigated),
      ir_land_sum = sum(cropland_irrigated_ha),
      .by = dplyr::all_of(.alloc_group_cols())
    ) |>
    dplyr::mutate(
      irrigation_basis = dplyr::case_when(
        ir_weight_sum > 0 ~ "pattern",
        ir_land_sum > 0 ~ "cropland",
        .default = "none"
      ),
      irrigated_raw_ha = dplyr::case_when(
        irrigation_basis == "pattern" ~
          irrigated_area_ha * weight_irrigated / ir_weight_sum,
        irrigation_basis == "cropland" ~
          irrigated_area_ha * cropland_irrigated_ha / ir_land_sum,
        .default = 0
      ),
      irrigated_target_ha = pmin(irrigated_raw_ha, target_ha),
      irrigation_clipped_ha = irrigated_raw_ha - irrigated_target_ha,
      rainfed_target_ha = target_ha - irrigated_target_ha
    ) |>
    .alloc_mark_level0() |>
    .alloc_warn_clipped()
}

.alloc_warn_clipped <- function(rows) {
  clipped <- dplyr::filter(rows, irrigation_clipped_ha > 1e-9)
  if (nrow(clipped) == 0L) {
    return(rows)
  }
  cli::cli_warn(c(
    "{nrow(clipped)} unit-item target{?s} carry more irrigated than
     harvested area; {round(sum(clipped$irrigation_clipped_ha))} ha clipped.",
    "x" = "{dplyr::n_distinct(clipped$area_code)} container{?s} affected.",
    i = "The clipped hectares are reported in
         {.field irrigation_clipped_ha} and are NOT moved to units with
         room: which unit should receive them is not decided."
  ))
  rows
}

# Decision T31(d): refuse only where coverage is COMPLETE, and only when
# both the relative and the absolute tolerance are breached. Under partial
# coverage the difference is the residual pseudo-unit's own target and is
# not a discrepancy at all.
.alloc_refuse_discrepancy <- function(rows, relative, absolute) {
  groups <- rows |>
    dplyr::filter(basis == "admin_sum") |>
    dplyr::distinct(
      dplyr::pick(dplyr::all_of(.alloc_group_cols())),
      harvested_area_ha,
      admin_sum
    ) |>
    dplyr::mutate(
      discrepancy_ha = harvested_area_ha - admin_sum,
      discrepancy_frac = dplyr::if_else(
        harvested_area_ha > 0,
        discrepancy_ha / harvested_area_ha,
        NA_real_
      )
    ) |>
    dplyr::filter(
      abs(discrepancy_frac) > relative,
      abs(discrepancy_ha) > absolute
    )
  if (nrow(groups) == 0L) {
    return(invisible(NULL))
  }
  worst <- groups[which.max(abs(groups$discrepancy_ha)), , drop = FALSE]
  cli::cli_abort(
    c(
      "{nrow(groups)} (container, item, year) group{?s} report a complete
       set of units whose areas do not add up to the national total.",
      x = "Worst: area_code {.val {worst$area_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           {round(worst$admin_sum)} ha reported against
           {round(worst$harvested_area_ha)} national
           ({round(100 * worst$discrepancy_frac, 1)}%).",
      i = "Refused because BOTH tolerances are breached
           ({.val {relative}} relative and {.val {absolute}} ha absolute,
           decision T31(d)). Under partial coverage the same difference
           would be the residual pseudo-unit's target."
    ),
    class = "whep_alloc_admin_discrepancy"
  )
}

.alloc_targets_out <- function(rows) {
  rows |>
    dplyr::select(
      "year",
      "area_code",
      "level_polity_code",
      "item_prod_code",
      "share",
      "target_ha",
      "irrigated_target_ha",
      "rainfed_target_ha",
      "irrigation_clipped_ha",
      "method_crop_alloc",
      national_total_ha = "harvested_area_ha",
      dplyr::any_of("n_cells")
    ) |>
    dplyr::arrange(year, area_code, item_prod_code, level_polity_code)
}

.alloc_coverage <- function(rows) {
  rows |>
    dplyr::summarise(
      n_units = dplyr::first(n_units),
      n_units_reporting = dplyr::first(n_units_reporting),
      coverage = dplyr::first(n_units_reporting) / dplyr::first(n_units),
      basis = dplyr::first(basis),
      weight_basis = dplyr::first(weight_basis),
      irrigation_basis = dplyr::first(irrigation_basis),
      national_total_ha = dplyr::first(harvested_area_ha),
      admin_sum = dplyr::first(admin_sum),
      residual_target_ha = dplyr::first(residual_share) *
        dplyr::first(harvested_area_ha),
      allocated_share = sum(share),
      dropped_ha = (1 - sum(share)) * dplyr::first(harvested_area_ha),
      irrigation_unplaceable_ha = sum(irrigation_clipped_ha) +
        dplyr::first(irrigated_area_ha) *
          (dplyr::first(irrigation_basis) == "none"),
      .by = dplyr::all_of(.alloc_group_cols())
    ) |>
    dplyr::mutate(
      discrepancy_ha = national_total_ha - admin_sum,
      discrepancy_frac = dplyr::if_else(
        national_total_ha > 0,
        discrepancy_ha / national_total_ha,
        NA_real_
      )
    ) |>
    dplyr::arrange(year, area_code, item_prod_code)
}

# The engine, or the documented empty result when nothing is left to place.
#
# `.gridded_landuse_parts()` maps over the years its `country_areas` carries,
# so a table with no positive target yields no year, no part, and a
# zero-COLUMN bind whose polity step then aborts on a missing `area_code`.
# That abort is `build_gridded_landuse()`'s and is left exactly where it is;
# reaching it from here would make the outcome `build_level_crop_targets()`
# documents -- under complete coverage every reported value zero, every share
# 0, the hectares counted in `coverage$dropped_ha` -- unreachable through the
# driver. So the call returns the empty allocation together with the coverage
# and target rows that say why it is empty.
.alloc_run_engine <- function(areas, patterns, cropland, layer, engine) {
  if (nrow(areas) > 0L) {
    return(.gridded_landuse_parts(areas, patterns, cropland, layer, engine))
  }
  cli::cli_inform(
    "No unit-item target is positive, so the allocation is empty; the
     hectares are counted in {.field coverage$dropped_ha}."
  )
  list(
    allocation = .alloc_empty_allocation(engine$area_key),
    breach = tibble::tibble()
  )
}

# The engine's own output columns at zero rows. Built through the same two
# tail steps `.gridded_landuse_parts()` ends with, so the polity columns are
# whatever they are there rather than a second list of names;
# `test_spatialize_levels.R` pins it against a real run of the same driver.
.alloc_empty_allocation <- function(area_key) {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    level_polity_code = character(),
    lon = numeric(),
    lat = numeric(),
    item_prod_code = integer(),
    rainfed_ha = numeric(),
    irrigated_ha = numeric()
  ) |>
    .spatialize_apply_area_key(
      area_key,
      c("rainfed_ha", "irrigated_ha")
    ) |>
    .add_reporting_polity_columns() |>
    tibble::as_tibble()
}

.alloc_engine_areas <- function(targets) {
  targets |>
    dplyr::filter(target_ha > 0 | irrigated_target_ha > 0) |>
    dplyr::select(
      "year",
      "area_code",
      "level_polity_code",
      "item_prod_code",
      harvested_area_ha = "target_ha",
      irrigated_area_ha = "irrigated_target_ha"
    )
}

# --- Diagnostics -------------------------------------------------------------

# The engine measures the breach per compartment-cell and item; the unit is
# the sum over its cells. Both multi-cropping bases survive the sum, and
# `in_force` says which one the redistribution actually ran against.
.alloc_unit_breach <- function(breach) {
  keys <- c(
    "year",
    "area_code",
    "level_polity_code",
    "item_prod_code",
    "mc_basis",
    "in_force"
  )
  if (nrow(breach) == 0L) {
    return(tibble::tibble(
      year = integer(),
      area_code = integer(),
      level_polity_code = character(),
      item_prod_code = integer(),
      mc_basis = character(),
      in_force = logical(),
      rf_over_ha = numeric(),
      ir_over_ha = numeric(),
      over_ha = numeric(),
      n_cells = integer()
    ))
  }
  breach |>
    dplyr::summarise(
      rf_over_ha = sum(rf_over),
      ir_over_ha = sum(ir_over),
      over_ha = sum(rf_over) + sum(ir_over),
      n_cells = dplyr::n_distinct(paste(lon, lat)),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::arrange(year, area_code, level_polity_code, item_prod_code)
}

# Per unit and item: how much of its target sits in cells it shares with a
# sibling unit of the same container, and with another country. A unit-level
# score re-aggregated through the same crosswalk cannot see misallocation
# INSIDE a shared cell, so this is what says how much of the unit is exposed
# to that blindness. `cell_limited` marks a unit with fewer than 4 cells,
# where the gridded pattern has almost no freedom left.
.alloc_straddle <- function(allocation, layer) {
  cells <- layer |>
    dplyr::distinct(lon, lat, area_code, level_polity_code) |>
    # A SIBLING IS A UNIT OF THE SAME CONTAINER, so the count that decides it
    # has to carry `area_code`. Keyed on the cell alone, a unit sharing a cell
    # with another COUNTRY scored as sibling straddle with no sibling anywhere
    # in the layer, and the two columns -- whose whole purpose is to separate
    # those two exposures -- returned the same number.
    dplyr::mutate(
      n_units_here = dplyr::n_distinct(level_polity_code),
      .by = c("lon", "lat", "area_code")
    ) |>
    dplyr::mutate(
      n_areas_here = dplyr::n_distinct(area_code),
      .by = c("lon", "lat")
    ) |>
    dplyr::mutate(
      shares_sibling = n_units_here > 1L,
      shares_foreign = n_areas_here > 1L
    )
  unit_cells <- dplyr::summarise(
    cells,
    n_cells = dplyr::n(),
    .by = c("area_code", "level_polity_code")
  )
  allocation |>
    dplyr::mutate(cell_ha = rainfed_ha + irrigated_ha) |>
    dplyr::left_join(
      dplyr::select(
        cells,
        "lon",
        "lat",
        "area_code",
        "level_polity_code",
        "shares_sibling",
        "shares_foreign"
      ),
      by = c("lon", "lat", "area_code", "level_polity_code")
    ) |>
    dplyr::summarise(
      allocated_ha = sum(cell_ha),
      straddle_sibling = .alloc_share_of(cell_ha, shares_sibling),
      straddle_foreign = .alloc_share_of(cell_ha, shares_foreign),
      .by = c("year", "area_code", "level_polity_code", "item_prod_code")
    ) |>
    dplyr::left_join(
      unit_cells,
      by = c("area_code", "level_polity_code")
    ) |>
    dplyr::mutate(cell_limited = n_cells < 4L) |>
    dplyr::arrange(year, area_code, level_polity_code, item_prod_code)
}

.alloc_share_of <- function(value, flag) {
  total <- sum(value)
  if (!isTRUE(total > 0)) {
    return(NA_real_)
  }
  sum(value[!is.na(flag) & flag]) / total
}

# Conservation at both grains, because they fail differently: a unit target
# that cannot be placed leaves the unit short while the container still
# reconciles if a sibling absorbed it, and a container short by the same
# hectares tells you the mass left the country altogether.
#
# THE CONTAINER IS MEASURED AGAINST THE NATIONAL TOTAL, never against the sum
# of the unit targets. Summing the unit targets makes the check circular: a
# hectare the unit split never turned into a target is missing from both sides
# at once, so 90% of a national crop area could leave the country with the
# table reporting a difference of exactly 0 and nothing warning. The mass is
# the same one `coverage$dropped_ha` records, and the two now agree.
.alloc_conservation <- function(allocation, targets, tolerance) {
  by_unit <- allocation |>
    dplyr::summarise(
      allocated_ha = sum(rainfed_ha + irrigated_ha),
      .by = c("year", "area_code", "level_polity_code", "item_prod_code")
    ) |>
    dplyr::full_join(
      dplyr::select(
        targets,
        "year",
        "area_code",
        "level_polity_code",
        "item_prod_code",
        "target_ha"
      ),
      by = c("year", "area_code", "level_polity_code", "item_prod_code")
    ) |>
    dplyr::mutate(grain = "unit")
  by_container <- by_unit |>
    dplyr::summarise(
      allocated_ha = sum(allocated_ha, na.rm = TRUE),
      .by = c("year", "area_code", "item_prod_code")
    ) |>
    dplyr::full_join(
      .alloc_national_totals(targets),
      by = c("year", "area_code", "item_prod_code")
    ) |>
    dplyr::mutate(grain = "container", level_polity_code = NA_character_)
  out <- dplyr::bind_rows(by_container, by_unit) |>
    dplyr::mutate(
      allocated_ha = dplyr::coalesce(allocated_ha, 0),
      target_ha = dplyr::coalesce(target_ha, 0),
      difference_ha = dplyr::coalesce(allocated_ha, 0) -
        dplyr::coalesce(target_ha, 0)
    ) |>
    dplyr::mutate(
      difference_frac = dplyr::if_else(
        target_ha > 0,
        difference_ha / target_ha,
        NA_real_
      )
    ) |>
    dplyr::arrange(grain, year, area_code, item_prod_code)
  .alloc_warn_conservation(out, tolerance)
  out
}

# The container's target, taken from the national total the split started
# from. `national_total_ha` is constant inside a (year, container, item) on the
# public path -- it comes from a many-to-one join with `country_areas` -- and
# that is CHECKED here rather than trusted: the whole point of the container
# grain is that its target is not the circular sum of the unit targets, so the
# one input it does rest on may not be whichever value happens to sort first.
.alloc_national_totals <- function(targets) {
  if (!rlang::has_name(targets, "national_total_ha")) {
    cli::cli_abort(c(
      "{.arg targets} carries no {.field national_total_ha}.",
      i = "The container's conservation target is the national total, not
           the sum of the unit targets; without it a hectare that became no
           unit's target would be missing from both sides at once."
    ))
  }
  totals <- targets |>
    dplyr::summarise(
      target_ha = dplyr::first(national_total_ha),
      n_totals = dplyr::n_distinct(national_total_ha),
      .by = c("year", "area_code", "item_prod_code")
    )
  .alloc_check_one_total(totals)
  dplyr::select(totals, -"n_totals")
}

.alloc_check_one_total <- function(totals) {
  bad <- dplyr::filter(totals, .data$n_totals > 1L)
  if (nrow(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- bad[1L, , drop = FALSE]
  cli::cli_abort(
    c(
      "{nrow(bad)} (year, container, item) group{?s} carry more than one
       {.field national_total_ha}.",
      x = "First: area_code {.val {worst$area_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           {worst$n_totals} distinct totals.",
      i = "The container's conservation target IS that total. Picking one of
           several would set the target the whole check is measured against
           from whichever row sorted first."
    ),
    class = "whep_alloc_national_total_varies"
  )
}

# The container rows are the headline; the unit rows are warned about ONLY
# where their container reconciles, because that is the failure they exist to
# see -- a sibling absorbing another unit's target leaves every national total
# right -- and repeating a container's own shortfall once per unit would bury
# it.
.alloc_warn_conservation <- function(conservation, tolerance) {
  containers <- .alloc_conservation_bad(conservation, "container", tolerance)
  .alloc_warn_conservation_at(containers, "container", tolerance)
  units <- .alloc_conservation_bad(conservation, "unit", tolerance) |>
    dplyr::anti_join(
      containers,
      by = c("year", "area_code", "item_prod_code")
    )
  .alloc_warn_conservation_at(units, "unit", tolerance)
}

# `difference_frac` is `NA` where the target is 0, and a relative tolerance has
# nothing to test there -- but hectares allocated against no target at all is
# the loudest breach of the two, not a case to fall through. Coalescing the
# `NA` to 0 excused it; the absolute half above (`difference_ha`) is what
# decides it instead.
.alloc_conservation_bad <- function(conservation, want, tolerance) {
  conservation |>
    dplyr::filter(
      grain == want,
      abs(difference_ha) > 1e-6,
      is.na(difference_frac) | abs(difference_frac) > tolerance
    )
}

.alloc_warn_conservation_at <- function(bad, grain_name, tolerance) {
  if (nrow(bad) == 0L) {
    return(invisible(NULL))
  }
  worst <- bad[which.max(abs(bad$difference_ha)), , drop = FALSE]
  if (grain_name == "unit") {
    return(.alloc_warn_unit_conservation(bad, worst, tolerance))
  }
  # The difference is SIGNED and both signs are reachable, so the wording
  # states the magnitude and lets the number carry the direction. Saying
  # "allocate less than their target" described one direction as if it were
  # the only one, and an over-allocation -- the shape a dropped negative
  # target produces -- would have been reported in words that denied it.
  cli::cli_warn(c(
    "{nrow(bad)} (container, item, year) group{?s} allocate a total
     differing from their target by more than {.val {tolerance}} relative,
     or hold hectares against no target at all.",
    "x" = "Worst: area_code {.val {worst$area_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           {round(worst$allocated_ha)} ha allocated against
           {round(worst$target_ha)} ha targeted
           ({round(worst$difference_ha)} ha).",
    i = "Negative is a target the engine could not place, which it names
         itself; positive is more allocated than was asked for, which no
         path should reach."
  ))
}

# A unit off its target inside a container that is exactly on its own: the
# hectares did not leave the country, they went to the wrong unit of it. No
# conservation check at the container grain can see that, which is why the
# unit rows are computed at all -- they were computed and then never read.
.alloc_warn_unit_conservation <- function(bad, worst, tolerance) {
  cli::cli_warn(c(
    "{nrow(bad)} (unit, item, year) row{?s} miss their target by more than
     {.val {tolerance}} relative -- or hold hectares against no target at all
     -- inside a container that reconciles.",
    "x" = "Worst: unit {.val {worst$level_polity_code}}, item
           {.val {worst$item_prod_code}}, {.val {worst$year}} --
           {round(worst$allocated_ha)} ha allocated against
           {round(worst$target_ha)} ha targeted
           ({round(worst$difference_ha)} ha).",
    i = "A sibling unit absorbed the difference, so every national total
         still reconciles and only the unit grain shows it."
  ))
}

# The longest run of consecutive years a series was carried rather than
# observed, per (container, item) and treatment. Decision T31(e) admits a
# LUH2 bridge of any length, so the length is what makes a 60-year bridge
# visible rather than merely legal.
.alloc_bridge_report <- function(admin_shares) {
  proto <- tibble::tibble(
    area_code = integer(),
    item_prod_code = integer(),
    treatment = character(),
    n_years = integer(),
    longest_run = integer()
  )
  if (
    is.null(admin_shares) ||
      nrow(admin_shares) == 0L ||
      !rlang::has_name(admin_shares, "treatment")
  ) {
    return(proto)
  }
  admin_shares |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(treatment), treatment != "observed") |>
    dplyr::distinct(area_code, item_prod_code, treatment, year) |>
    dplyr::arrange(area_code, item_prod_code, treatment, year) |>
    dplyr::mutate(
      run_id = cumsum(c(TRUE, diff(year) != 1L)),
      .by = c("area_code", "item_prod_code", "treatment")
    ) |>
    dplyr::summarise(
      run_length = dplyr::n(),
      .by = c("area_code", "item_prod_code", "treatment", "run_id")
    ) |>
    dplyr::summarise(
      # `max(c(0L, x))`, not `max(x)`: `dplyr::summarise()` evaluates the
      # expression once on a zero-row slice to type the column, and a bare
      # `max()` there warns and returns `-Inf`.
      n_years = sum(run_length),
      longest_run = max(c(0L, run_length)),
      .by = c("area_code", "item_prod_code", "treatment")
    ) |>
    dplyr::arrange(area_code, item_prod_code, treatment)
}
