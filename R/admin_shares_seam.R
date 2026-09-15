#' Aggregate a gridded land quantity onto administrative units
#'
#' @description
#' Sum a per-cell land quantity onto the compartments of a granted-depth
#' country grid, giving the per-unit extent `E_u(t)` the seam back-cast
#' modulates shares with:
#'
#' \deqn{E_u(t) = \sum_c E_c(t) \times f(c, u)}
#'
#' where `f(c, u)` is `cell_area_frac`, the unit's share of the physical
#' cell on the land basis. The gridded tables are the ones already in
#' memory for the spatialization engine, so no LUH2 archive is re-read
#' here: [read_luh2_landuse()] is never called, and the identity between
#' the extent that modulates a share and the extent that weights cells
#' within a unit holds by construction rather than by assertion. What the
#' construction cannot prove is which LUH2 vintage the caller's tables
#' came from; the prep-side provenance stamp on those tables carries that.
#'
#' @section Mirroring the engine's land quantity:
#' `R/spatialize.R:337-405` builds the crop weight from
#' `cropland_ha = i.cropland_ha * cell_area_frac`; a type-aware run
#' replaces that with `type_ha * cell_area_frac` joined on
#' `(lon, lat, luh2_type)`, zeroes cells with no row for the crop's type,
#' and restores total cropland for a whole `(area_code, item_prod_code)`
#' group whose type potential `sum(harvest_fraction * cropland_ha)` is not
#' positive. `R/spatialize_livestock.R:358-405` builds the livestock
#' weight from `pasture_ha + rangeland_ha` (`pasture`), `rangeland_ha`
#' (`rangeland`), `cropland_ha` (`cropland`) or
#' `0.5 * (pasture_ha + rangeland_ha) + 0.5 * cropland_ha` (`mixed`), also
#' times `cell_area_frac`.
#'
#' This function mirrors the `cell_area_frac` multiplication and the
#' compartment sum. Which land column stands for a given item or species
#' group is the caller's choice, declared through `quantity`: a character
#' vector applies one weighted column set to every row, a lookup table
#' applies a different set per key. The engine's whole-group fallback to
#' total cropland turns on `harvest_fraction`, a within-unit weight rather
#' than a land extent, so the caller evaluates it and passes the resulting
#' per-item choice in the lookup; the choice comes back on `extent_basis`,
#' so the land quantity behind each row is recorded rather than inferred.
#'
#' @section Territory basis:
#' `country_grid` is used exactly as given, unfiltered by edge validity.
#' That is the t0-geometry convention of the plan's seam section: shares
#' and `E_u` are defined on the t0 unit set with t0 geometry held fixed,
#' so a unit whose containment edge starts after a back-cast year still
#' contributes its t0 cells at that year. Pass the layer already filtered
#' to the t0 reference year (`.filter_country_grid_year()`) when the t0
#' geometry differs from the delivered layer.
#'
#' @param gridded A tibble of per-cell land quantities keyed on `lon`,
#'   `lat`, `year`, plus any extra key columns named in `extent_by`, plus
#'   the numeric columns named by `quantity`.
#' @param country_grid A tibble of compartments: `lon`, `lat`,
#'   `area_code`, `level_polity_code`, `level`, `cell_area_frac`. One row
#'   per `(lon, lat, area_code, level_polity_code)`; an interval-grained
#'   layer must be filtered to one geometry first.
#' @param quantity Either a character vector of `gridded` columns summed
#'   with weight 1, or a lookup table with a `quantity` column of column
#'   names, an optional numeric `weight` column (default 1) and the
#'   `extent_by` key columns the lookup is keyed on. Several rows per key
#'   sum several columns.
#' @param extent_by Character vector of extra key columns of the output.
#'   Each must be a column of `gridded` (carried through the aggregation)
#'   or a key column of a `quantity` lookup (which selects columns per
#'   key), never both.
#' @param container_frac Optional tibble `lon`, `lat`, `area_code`,
#'   `cell_area_frac` giving the container's own share of each cell on the
#'   same land basis. Required by `residual_code`, ignored without it.
#' @param residual_code Optional `level_polity_code` for a residual
#'   pseudo-unit, whose extent is
#'   `E_res(t) = E_container(t) - sum_u E_u(t)`. `NULL` (default) emits no
#'   residual row, which is the right shape unless the allocation runs
#'   under the residual policy.
#' @param tolerance Relative tolerance below which a negative residual
#'   extent counts as floating-point noise and is clamped to zero. A
#'   larger negative residual aborts: it means the unit fractions exceed
#'   the container's.
#'
#' @return A tibble: `area_code`, `level_polity_code`, `level`, the
#'   `extent_by` columns, `year`, `extent_ha` and `extent_basis` (the
#'   `weight * column` terms behind the row, `+`-joined in a stable
#'   order). One row per unit, key and year for which at least one
#'   compartment of that unit met a gridded cell.
#'
#' @export
#'
#' @examples
#' grid <- tibble::tibble(
#'   lon = c(10.25, 10.75, 10.75),
#'   lat = 40.25,
#'   area_code = 900L,
#'   level_polity_code = c("A1", "A1", "A2"),
#'   level = 1L,
#'   cell_area_frac = c(1, 0.3, 0.5)
#' )
#' cells <- tibble::tibble(
#'   lon = c(10.25, 10.75),
#'   lat = 40.25,
#'   year = 1900L,
#'   cropland_ha = c(1000, 1200)
#' )
#' # A1 = 1000 * 1 + 1200 * 0.3 = 1360; A2 = 1200 * 0.5 = 600.
#' aggregate_unit_extent(cells, grid, "cropland_ha")
aggregate_unit_extent <- function(
  gridded,
  country_grid,
  quantity,
  extent_by = NULL,
  container_frac = NULL,
  residual_code = NULL,
  tolerance = 1e-8
) {
  spec <- .aue_spec(quantity, extent_by, gridded)
  .aue_validate(gridded, country_grid, spec, container_frac, residual_code)
  cells <- .aue_cell_quantity(gridded, spec)
  units <- .aue_sum_to_units(cells, country_grid, spec)
  if (is.null(residual_code)) {
    return(units)
  }
  .aue_add_residual(
    units,
    cells,
    container_frac,
    spec,
    residual_code,
    tolerance
  )
}

#' Flag implausible year-on-year jumps in a per-unit extent
#'
#' @description
#' Run [check_series_jumps()] on `extent_ha` per unit series, the check the
#' plan's seam section asks for before an extent modulates a share. WHEP
#' repairs LUH2's isolated single-year cropland collapses nationally
#' (`.fix_luh2_crop_collapse()`, `R/build_production.R:627-695`); the same
#' collapse reaches a unit series unrepaired, where it would propagate
#' into every earlier back-cast year through the growth chain.
#'
#' The scan covers whatever years `extent` carries.
#' [backcast_admin_shares()] calls it on the back-cast window alone --
#' `t0` and everything before it, the years the modulation actually reads
#' -- so a jump after the seam is not a reason to refuse a back-cast.
#'
#' @param extent A per-unit extent table as returned by
#'   [aggregate_unit_extent()]. Every column that is not `year`,
#'   `extent_ha` or `extent_basis` identifies the series.
#' @param ratio_bounds Length-2 numeric `c(low, high)` plausible band for
#'   the ratio of consecutive years, passed to [check_series_jumps()],
#'   whose own default it inherits.
#' @param min_value Minimum extent both members of a pair must exceed to
#'   be flagged, passed to [check_series_jumps()].
#' @param verbose Logical, passed to [check_series_jumps()]. Default
#'   `FALSE`, so a guard inside a back-cast stays quiet.
#'
#' @return The [check_series_jumps()] flags tibble: the series key
#'   columns, `year`, `prev_value`, `value`, `ratio` and `allowlisted`.
#'
#' @export
#'
#' @examples
#' extent <- tibble::tibble(
#'   area_code = 900L,
#'   level_polity_code = "A1",
#'   level = 1L,
#'   year = 1900:1903,
#'   extent_ha = c(1000, 1010, 5, 1020),
#'   extent_basis = "cropland_ha"
#' )
#' check_extent_jumps(extent)
check_extent_jumps <- function(
  extent,
  ratio_bounds = c(0.55, 1.6),
  min_value = 0,
  verbose = FALSE
) {
  .require_cols(extent, c("year", "extent_ha"), "extent")
  check_series_jumps(
    extent,
    value_col = extent_ha,
    time_col = year,
    .by = .backcast_extent_keys(extent),
    ratio_bounds = ratio_bounds,
    min_value = min_value,
    verbose = verbose
  )
}

#' Back-cast admin shares across the seam
#'
#' @description
#' Complete an admin-shares table to the full year range of its extent and
#' fill the pre-seam years by modulating each unit's seam-year share with
#' its own land extent, the plan's decision-7 back-cast:
#'
#' \deqn{\tilde{s}_u(t) = s_u(t_0) \frac{E_u(t)}{E_u(t_0)}, \qquad
#'       s_u(t) = \frac{\tilde{s}_u(t)}{\sum_v \tilde{s}_v(t)}}
#'
#' The first step is [fill_proxy_growth()] with the extent as proxy, the
#' same machinery `.fill_pre_faostat()` (`R/build_production.R:3008`) uses
#' to back-cast the national totals these shares split, so the two
#' compose. The second is a per-year renormalisation over the `t0` unit
#' set, the residual pseudo-unit included when the shares carry one.
#'
#' Nothing here is a fallback: a year the rules cannot resolve is refused
#' with a diagnostic and keeps `share = NA`, never a carried value.
#'
#' Two things the plan's seam section names are deliberately **not** here.
#' Pre-seam irrigation needs no separate treatment: the unit irrigated
#' target is built from the engine's own annual `ir_potential`
#' (`R/spatialize.R:420`), whose LUH2 `irrigated_ha` varies every year,
#' pre-seam years included, so it moves with the same extent that
#' modulates the area share. And the within-unit weight *regime* per
#' `(unit, item, year)` is the allocation's to record, not this
#' function's: `treatment` here says how a share was obtained, while the
#' regime says which weight then spread it inside the unit.
#'
#' @section Territory basis:
#' Shares and `E_u` are defined on the `t0` unit set with `t0` geometry
#' held fixed, the [read_luh2_landuse()] snapshot convention and the
#' analogue of `mapping_status == "backcast_anchor"`. Every row produced
#' that way carries `treatment = "backcast_t0_geometry"`, so a unit whose
#' containment edge starts after a back-cast year still gets a row at that
#' year, on its `t0` cells.
#'
#' Translating those rows onto the partition valid at `t` through the
#' containment-edge lineage -- an earlier unit that later split taking the
#' sum of its `t0`-descendants' modulated shares -- happens before the
#' rows enter the engine and is **not implemented here**. It is a
#' follow-up, because the relation it needs is succession (which unit
#' became which), while `polity_containment` carries containment
#' (`member_code`, `container_code`, `start_year`, `end_year`, `basis`)
#' only: nothing in the delivered data says a unit starting in one year
#' descends from one that ended the year before. Until it lands, a
#' container whose partition changed inside the back-cast window is
#' back-cast on its `t0` partition, which `treatment` states on every row.
#'
#' @section Zero cases:
#' Post-processing applied after the fill, which itself leaves such
#' positions `NA` (`.fg_fill_backward_vec()` keeps a fill only where the
#' result is finite and positive). The plan's four cases, with precedence
#' (a) over (b):
#'
#' - (a) `E_u(t0) == 0` with `s_u(t0) > 0`: hold `s_u(t0)` **outside** the
#'   renormalisation, siblings renormalised to `1 - s_u(t0)`. Holding it
#'   inside would anti-modulate the unit. Counter `zero_extent_t0_held`.
#' - (b) `E_u(t) == 0` at a back-cast year of a unit not held by (a):
#'   share `0` there, its mass going to the siblings through the
#'   renormalisation. Counter `zero_extent_year_zeroed`.
#' - (c) the renormalisable sum is zero while there is mass to distribute:
#'   the year is refused for that series and all its units left `NA`.
#'   Counter `zero_sum_year_refused`.
#' - (d) `E_u(t0) == 0` with no positive `s_u(t0)`: nothing to hold and no
#'   mass to move, so the unit contributes `0`. Counter
#'   `zero_extent_t0_no_share`.
#'
#' Case (b) counts every zero-extent back-cast year, including the ones
#' inside a year (c) then refuses: both statements are true of such a
#' year, and (c) is what decides the output.
#'
#' A zero year also breaks the growth chain: `fill_proxy_growth()`
#' telescopes a product of consecutive growth factors, so it cannot reach
#' the years beyond the break and leaves them `NA` even though the ratio
#' above is perfectly well defined there. Those positions are completed
#' from that ratio directly -- the same expression, not a second method --
#' and every one is counted (`direct_ratio_repair`). Where the chain is
#' intact the two agree to machine precision, which the tests assert.
#'
#' @section Interior gaps:
#' The fill window ends at `t0`, so no gap between two observed years is
#' ever filled: such rows are completed, left `NA` and counted
#' (`interior_gap_refused`, `trailing_gap_refused`). That is the
#' placeholder default until T31(e) decides the rule; it is recorded on
#' every row in `gap_rule` and in `settings`. `max_gap` and
#' `max_gap_linear` are passed straight to [fill_proxy_growth()] so that
#' decision needs no API change. Today `max_gap` bounds the back-cast run
#' itself -- a longer run is left unfilled and counted
#' (`max_gap_exceeded`), because `fill_proxy_growth()`'s gap test governs
#' the leading run as well as the interior ones -- while `max_gap_linear`
#' cannot bite, there being no interior run inside the window.
#'
#' @param shares An admin-shares table (see [admin_shares_schema()]).
#'   Minimum columns: `area_code`, `level`, `item_prod_code`,
#'   `indicator_used`, `level_polity_code`, `year`, `share`,
#'   `treatment_year`. Every other contract column present is carried onto
#'   the produced rows from their anchor row.
#' @param extent A per-unit extent table from [aggregate_unit_extent()].
#'   Its year range is the range the shares are completed to.
#' @param seam Optional tibble overriding `t0` per series: `area_code`,
#'   `item_prod_code`, `t0`, optionally `level`. An override is the
#'   temporal hold-out lever: every year below the given `t0` becomes a
#'   back-cast year even where an observation exists, and the superseded
#'   observations are counted (`seam_supersedes_observed`).
#' @param binding_indicator The indicator that binds, `"area_harvested"`
#'   by the plan's decision 8. `t0` is the series' first observed year
#'   carrying it; a series with none is refused (`no_binding_anchor`).
#' @param settings Named list of tuning knobs, each defaulting as below.
#'   An unknown key aborts, and what the call ran with comes back in the
#'   returned `settings` tibble:
#'
#'   - `max_gap` (`Inf`): passed to [fill_proxy_growth()], its own
#'     default.
#'   - `max_gap_linear` (`0`): passed to [fill_proxy_growth()]; a
#'     placeholder pending T31(e), see the interior-gaps section.
#'   - `zero_policy` (`"hold_outside"`): how case (a) is handled. Only
#'     `"hold_outside"` is signed off (decision 9); the key exists so a
#'     later decision can add a value without changing call sites.
#'   - `repair` (`FALSE`): on a flagged extent jump, `TRUE` applies the
#'     unit-keyed analogue of `.fix_luh2_crop_collapse()` and refuses only
#'     what stays flagged; `FALSE`, pending T31, refuses the affected
#'     series outright. Either way the choice is in `settings` and the
#'     outcome in `diagnostics`.
#'   - `ratio_bounds` (`c(0.55, 1.6)`): plausible band for
#'     [check_extent_jumps()].
#'   - `collapse_ratio` (`0.02`): fraction of the adjacent-year mean below
#'     which a year counts as an isolated collapse, as in
#'     `.fix_luh2_crop_collapse()` (`R/build_production.R:629-630`).
#'   - `min_neighbour_ha` (`100`): both neighbours must exceed this for a
#'     collapse to be repaired -- that function's
#'     `min_neighbor_mha = 0.001` Mha expressed in hectares.
#'   - `tolerance` (`1e-9`): absolute tolerance for the share and extent
#'     arithmetic.
#'
#' @return A list of four tibbles:
#'   - `shares`: the input rows plus the completed year set for the `t0`
#'     unit sets, with two added columns. `treatment` is `"observed"` for
#'     a row that arrived in `shares`, `"backcast_t0_geometry"` for a row
#'     this function produced, `"luh2_clamped"` where `t0` lies beyond the
#'     extent's last year so `E_u(t0)` is the clamped last slice, and `NA`
#'     for a completed row nothing filled, whose reason is in
#'     `diagnostics`. `gap_rule` records the interior-gap setting. A
#'     produced row carries its anchor's provenance columns, `value = NA`
#'     (no reported value exists) and `treatment_year = NA` (reserved for
#'     the interior-gap rule), so produced rows deliberately sit outside
#'     the closed [admin_shares_schema()]; the `"observed"` subset on the
#'     contract columns still conforms.
#'   - `diagnostics`: one row per event, with the series and unit keys,
#'     `year` where the event is year-specific, `diagnostic` from a closed
#'     vocabulary and free-text `detail`.
#'   - `counters`: every vocabulary member with its count, zeros included.
#'   - `settings`: the one-row record of what this call was run with.
#'
#' @export
#'
#' @examples
#' shares <- tibble::tibble(
#'   area_code = 900L,
#'   level = 1L,
#'   item_prod_code = 15L,
#'   indicator_used = "area_harvested",
#'   level_polity_code = c("A1", "A2"),
#'   year = 1902L,
#'   share = c(0.6, 0.4),
#'   treatment_year = "observed"
#' )
#' extent <- tibble::tibble(
#'   area_code = 900L,
#'   level_polity_code = rep(c("A1", "A2"), each = 3),
#'   level = 1L,
#'   year = rep(1900:1902, times = 2),
#'   extent_ha = c(800, 900, 1000, 600, 550, 500)
#' )
#' out <- backcast_admin_shares(shares, extent)
#' out$shares[, c("level_polity_code", "year", "share", "treatment")]
backcast_admin_shares <- function(
  shares,
  extent,
  seam = NULL,
  binding_indicator = "area_harvested",
  settings = list()
) {
  config <- .backcast_config(settings)
  repair <- config$repair
  tolerance <- config$tolerance
  .backcast_validate(shares, extent, binding_indicator, repair)
  anchors <- .backcast_resolve_t0(shares, seam, binding_indicator)
  guard <- .backcast_guard_extent(
    extent,
    anchors$t0,
    repair,
    config$ratio_bounds,
    config$collapse_ratio,
    config$min_neighbour_ha
  )
  panel <- .backcast_build_panel(
    shares,
    guard$extent,
    guard$t0,
    extent,
    binding_indicator
  )
  filled <- .backcast_run_fill(
    panel$data,
    config$max_gap,
    config$max_gap_linear,
    tolerance
  )
  zeroed <- .backcast_zero_cases(filled$data, tolerance)
  final <- .backcast_renormalise(zeroed$data, tolerance)
  diag <- dplyr::bind_rows(
    anchors$diag,
    guard$diag,
    panel$diag,
    filled$diag,
    zeroed$diag,
    final$diag,
    .backcast_gap_diag(final$data, shares)
  )
  record <- .backcast_settings(config, binding_indicator)
  .backcast_assemble(shares, final$data, diag, record)
}

# --- Private helpers: unit extent ---------------------------------------------

# The series key columns of an extent table: everything that is not the
# time column, the value, or the value's provenance label.
.backcast_extent_keys <- function(extent) {
  setdiff(names(extent), c("year", "extent_ha", "extent_basis"))
}

# Rows of `data` that repeat a key, as a count table. One helper so every
# duplicate guard here reports the same way.
.backcast_duplicate_keys <- function(data, keys) {
  data |>
    dplyr::summarise(n = dplyr::n(), .by = dplyr::all_of(keys)) |>
    dplyr::filter(n > 1)
}

.backcast_distinct_keys <- function(data, keys) {
  data |>
    dplyr::select(dplyr::all_of(keys)) |>
    dplyr::distinct()
}

# Normalise `quantity` into a lookup of one row per (key, column), and
# split `extent_by` into keys carried from `gridded` and keys the lookup
# itself supplies.
.aue_spec <- function(quantity, extent_by, gridded) {
  extent_by <- extent_by %||% character(0)
  map <- .aue_quantity_map(quantity)
  lookup_keys <- setdiff(names(map), c("quantity", "weight"))
  bad <- setdiff(lookup_keys, extent_by)
  if (length(bad) > 0) {
    cli::cli_abort(
      "{.arg quantity} key column{?s} {.field {bad}} {?is/are} not in
       {.arg extent_by}."
    )
  }
  in_both <- intersect(lookup_keys, names(gridded))
  if (length(in_both) > 0) {
    cli::cli_abort(
      "{.field {in_both}} {?is/are} both a {.arg quantity} lookup key and a
       {.arg gridded} column; a key comes from one or the other."
    )
  }
  grid_keys <- setdiff(extent_by, lookup_keys)
  list(
    map = map,
    grid_keys = grid_keys,
    lookup_keys = lookup_keys,
    out_keys = c(grid_keys, lookup_keys)
  )
}

# `quantity` as a tibble of (quantity, weight) plus any lookup keys.
.aue_quantity_map <- function(quantity) {
  if (is.character(quantity)) {
    if (length(quantity) == 0 || anyNA(quantity)) {
      cli::cli_abort("{.arg quantity} must name at least one column.")
    }
    return(tibble::tibble(quantity = quantity, weight = 1))
  }
  if (!is.data.frame(quantity)) {
    cli::cli_abort(
      "{.arg quantity} must be a character vector or a lookup table."
    )
  }
  .require_cols(quantity, "quantity", "quantity")
  map <- tibble::as_tibble(quantity)
  if (!rlang::has_name(map, "weight")) {
    map$weight <- 1
  }
  if (!is.numeric(map$weight) || anyNA(map$weight)) {
    cli::cli_abort("{.arg quantity}'s {.field weight} must be numeric.")
  }
  map
}

.aue_validate <- function(
  gridded,
  country_grid,
  spec,
  container_frac,
  residual_code
) {
  qcols <- unique(spec$map$quantity)
  .require_cols(
    gridded,
    c("lon", "lat", "year", spec$grid_keys, qcols),
    "gridded"
  )
  .require_cols(
    country_grid,
    c(
      "lon",
      "lat",
      "area_code",
      "level_polity_code",
      "level",
      "cell_area_frac"
    ),
    "country_grid"
  )
  .aue_check_grid_unique(country_grid)
  if (is.null(residual_code)) {
    return(invisible(NULL))
  }
  .aue_check_residual(country_grid, container_frac, residual_code)
}

.aue_check_residual <- function(country_grid, container_frac, residual_code) {
  if (is.null(container_frac)) {
    cli::cli_abort(
      "{.arg residual_code} needs {.arg container_frac}: the residual is
       {.code E_container - sum_u E_u}, so the container's own cell shares
       must be supplied."
    )
  }
  .require_cols(
    container_frac,
    c("lon", "lat", "area_code", "cell_area_frac"),
    "container_frac"
  )
  if (residual_code %in% country_grid$level_polity_code) {
    cli::cli_abort(
      "{.arg residual_code} {.val {residual_code}} is already a unit of
       {.arg country_grid}."
    )
  }
}

.aue_check_grid_unique <- function(country_grid) {
  keys <- c("lon", "lat", "area_code", "level_polity_code")
  dup <- .backcast_duplicate_keys(country_grid, keys)
  if (nrow(dup) > 0) {
    cli::cli_abort(
      c(
        "{.arg country_grid} has {nrow(dup)} compartment{?s} with more than
         one row.",
        "i" = "Filter an interval-grained layer to one geometry first
               ({.fn .filter_country_grid_year})."
      )
    )
  }
}

# Per-cell quantity: melt the named columns, attach the lookup's weights
# and keys, and sum back to one value per cell, year and output key.
.aue_cell_quantity <- function(gridded, spec) {
  qcols <- unique(spec$map$quantity)
  id <- c("lon", "lat", "year", spec$grid_keys)
  dt <- data.table::as.data.table(gridded)[, c(id, qcols), with = FALSE]
  long <- data.table::melt(
    dt,
    id.vars = id,
    measure.vars = qcols,
    variable.name = "quantity",
    value.name = ".cell_value",
    variable.factor = FALSE
  )
  .aue_abort_on_na(long, qcols)
  map <- data.table::as.data.table(spec$map)
  joined <- map[long, on = "quantity", allow.cartesian = TRUE, nomatch = NULL]
  joined[, .cell_value := .cell_value * weight]
  keys <- c("lon", "lat", "year", spec$out_keys)
  joined[, list(.cell_value = sum(.cell_value)), by = keys]
}

.aue_abort_on_na <- function(long, qcols) {
  n_na <- sum(is.na(long$.cell_value))
  if (n_na == 0) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.arg gridded} has {n_na} missing value{?s} in {.field {qcols}}.",
      "i" = "Absence and zero are different quantities here; zero-fill
             deliberately before aggregating."
    )
  )
}

.aue_sum_to_units <- function(cells, country_grid, spec) {
  keep <- c(
    "lon",
    "lat",
    "area_code",
    "level_polity_code",
    "level",
    "cell_area_frac"
  )
  cg <- data.table::as.data.table(country_grid)[, keep, with = FALSE]
  joined <- cells[
    cg,
    on = list(lon, lat),
    allow.cartesian = TRUE,
    nomatch = NULL
  ]
  joined[, .unit_ha := .cell_value * cell_area_frac]
  by_cols <- .aue_out_key_order(spec)
  out <- joined[, list(extent_ha = sum(.unit_ha)), by = by_cols]
  tibble::as_tibble(out) |>
    .aue_attach_basis(spec) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(by_cols)))
}

.aue_out_key_order <- function(spec) {
  c("area_code", "level_polity_code", "level", spec$out_keys, "year")
}

# The `weight * column` terms behind every row, in a stable order, so the
# land quantity a row was built from is recorded, not inferred. One label
# per lookup key, because a lookup gives different keys different bases.
.aue_basis_table <- function(spec) {
  labelled <- spec$map |>
    dplyr::mutate(
      .term = dplyr::if_else(
        weight == 1,
        quantity,
        paste0(weight, "*", quantity)
      )
    )
  if (length(spec$lookup_keys) == 0) {
    return(tibble::tibble(extent_basis = .aue_join_terms(labelled$.term)))
  }
  labelled |>
    dplyr::summarise(
      extent_basis = .aue_join_terms(.term),
      .by = dplyr::all_of(spec$lookup_keys)
    )
}

.aue_join_terms <- function(terms) {
  paste(sort(unique(terms)), collapse = "+")
}

.aue_attach_basis <- function(df, spec) {
  basis <- .aue_basis_table(spec)
  if (length(spec$lookup_keys) == 0) {
    return(dplyr::mutate(df, extent_basis = basis$extent_basis[[1L]]))
  }
  dplyr::left_join(df, basis, by = spec$lookup_keys)
}

.aue_add_residual <- function(
  units,
  cells,
  container_frac,
  spec,
  residual_code,
  tolerance
) {
  by_cols <- c("area_code", spec$out_keys, "year")
  container <- .aue_container_extent(cells, container_frac, by_cols)
  unit_sum <- units |>
    dplyr::summarise(
      .unit_ha = sum(extent_ha),
      level = .aue_one_level(level),
      .by = dplyr::all_of(by_cols)
    )
  residual <- container |>
    dplyr::inner_join(unit_sum, by = by_cols) |>
    dplyr::mutate(.residual_ha = extent_ha - .unit_ha)
  .aue_abort_on_negative(residual, tolerance)
  residual |>
    dplyr::mutate(
      extent_ha = pmax(.residual_ha, 0),
      level_polity_code = residual_code
    ) |>
    .aue_attach_basis(spec) |>
    dplyr::select(dplyr::all_of(names(units))) |>
    dplyr::bind_rows(units) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(.aue_out_key_order(spec))))
}

.aue_container_extent <- function(cells, container_frac, by_cols) {
  cf <- data.table::as.data.table(container_frac)[,
    list(lon, lat, area_code, cell_area_frac)
  ]
  joined <- cells[
    cf,
    on = list(lon, lat),
    allow.cartesian = TRUE,
    nomatch = NULL
  ]
  joined[, .unit_ha := .cell_value * cell_area_frac]
  tibble::as_tibble(joined[, list(extent_ha = sum(.unit_ha)), by = by_cols])
}

.aue_abort_on_negative <- function(residual, tolerance) {
  bad <- dplyr::filter(
    residual,
    .residual_ha < -tolerance * pmax(extent_ha, 1)
  )
  if (nrow(bad) == 0) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{nrow(bad)} residual extent{?s} {?is/are} negative beyond
       {.arg tolerance}.",
      "i" = "The unit fractions exceed the container's in
             {.val {unique(bad$area_code)}}."
    )
  )
}

# The one depth a container's units sit at. Read inside the aggregation
# that already keys on the year, rather than joined back on `area_code`
# alone, so the residual adds no year-free territorial join (whep#669,
# `R/join_audit.R`).
.aue_one_level <- function(level) {
  depths <- unique(level)
  if (length(depths) > 1) {
    cli::cli_abort(
      "This container's units sit at {length(depths)} different
       {.field level}s; a residual has no single depth there."
    )
  }
  depths
}

# --- Private helpers: seam keys and validation --------------------------------

.backcast_series_key <- function() {
  c("area_code", "level", "item_prod_code")
}

.backcast_unit_key <- function() {
  c("area_code", "level", "item_prod_code", "level_polity_code")
}

.backcast_row_key <- function() {
  c(.backcast_unit_key(), "indicator_used", "year")
}

.backcast_required_cols <- function() {
  c(.backcast_row_key(), "share", "treatment_year")
}

.backcast_validate <- function(shares, extent, binding_indicator, repair) {
  .require_cols(shares, .backcast_required_cols(), "shares")
  .require_cols(
    extent,
    c("area_code", "level_polity_code", "level", "year", "extent_ha"),
    "extent"
  )
  if (!rlang::is_string(binding_indicator)) {
    cli::cli_abort("{.arg binding_indicator} must be a single string.")
  }
  if (!rlang::is_bool(repair)) {
    cli::cli_abort("{.arg repair} must be {.code TRUE} or {.code FALSE}.")
  }
  if (nrow(extent) == 0) {
    cli::cli_abort(
      "{.arg extent} is empty: there is no year range to complete onto."
    )
  }
  taken <- intersect(
    c("treatment", "gap_rule", "extent_ha", "extent_basis"),
    names(shares)
  )
  if (length(taken) > 0) {
    cli::cli_abort(
      "{.arg shares} already carries {.field {taken}}, which this function
       writes or reads from {.arg extent}."
    )
  }
  dup <- .backcast_duplicate_keys(shares, .backcast_row_key())
  if (nrow(dup) > 0) {
    cli::cli_abort("{.arg shares} repeats {nrow(dup)} row key{?s}.")
  }
}

# --- Private helpers: seam anchors --------------------------------------------

# `t0` per series: the first observed year carrying the binding indicator,
# or `seam`'s override. A series with neither is refused.
.backcast_resolve_t0 <- function(shares, seam, binding_indicator) {
  observed <- .backcast_observed(shares, binding_indicator)
  auto <- .backcast_first_years(observed)
  t0 <- .backcast_apply_seam(auto, seam, shares)
  ok <- .backcast_t0_is_observed(t0, observed)
  bad <- dplyr::anti_join(t0, ok, by = .backcast_series_key())
  anchorless <- dplyr::anti_join(
    .backcast_distinct_keys(shares, .backcast_series_key()),
    auto,
    by = .backcast_series_key()
  )
  diag <- dplyr::bind_rows(
    .backcast_diag_rows(
      anchorless,
      "no_binding_anchor",
      paste0("no observed ", binding_indicator, " row")
    ),
    .backcast_diag_rows(bad, "seam_t0_not_observed", paste0("t0 = ", bad$.t0))
  )
  list(t0 = dplyr::semi_join(t0, ok, by = .backcast_series_key()), diag = diag)
}

# The first observed year per series. Branches on the empty case because
# `min()` on no rows warns and returns `Inf`.
.backcast_first_years <- function(observed) {
  keys <- .backcast_series_key()
  if (nrow(observed) == 0) {
    return(dplyr::mutate(
      .backcast_distinct_keys(observed, keys),
      .t0 = integer(0)
    ))
  }
  dplyr::summarise(observed, .t0 = min(year), .by = dplyr::all_of(keys))
}

.backcast_observed <- function(shares, binding_indicator) {
  shares |>
    dplyr::filter(
      indicator_used == binding_indicator,
      treatment_year == "observed",
      !is.na(share)
    )
}

.backcast_apply_seam <- function(auto, seam, shares) {
  if (is.null(seam)) {
    return(auto)
  }
  .require_cols(seam, c("area_code", "item_prod_code", "t0"), "seam")
  keys <- intersect(names(seam), .backcast_series_key())
  overrides <- seam |>
    dplyr::select(dplyr::all_of(c(keys, "t0"))) |>
    dplyr::rename(.t0_new = t0)
  dup <- .backcast_duplicate_keys(overrides, keys)
  if (nrow(dup) > 0) {
    cli::cli_abort(
      "{.arg seam} gives {nrow(dup)} series more than one {.field t0}."
    )
  }
  .backcast_distinct_keys(shares, .backcast_series_key()) |>
    dplyr::left_join(auto, by = .backcast_series_key()) |>
    dplyr::left_join(overrides, by = keys) |>
    dplyr::mutate(.t0 = dplyr::coalesce(.t0_new, .t0)) |>
    dplyr::filter(!is.na(.t0)) |>
    dplyr::select(dplyr::all_of(c(.backcast_series_key(), ".t0")))
}

# A `t0` anchors only where that year really carries observed shares.
.backcast_t0_is_observed <- function(t0, observed) {
  anchor_years <- observed |>
    .backcast_distinct_keys(c(.backcast_series_key(), "year")) |>
    dplyr::rename(.t0 = year)
  t0 |>
    dplyr::inner_join(anchor_years, by = c(.backcast_series_key(), ".t0")) |>
    .backcast_distinct_keys(.backcast_series_key())
}

# --- Private helpers: extent guard --------------------------------------------

# Scan the per-unit extent over the back-cast window, then repair or
# refuse per `repair`, as the plan's seam section requires.
.backcast_guard_extent <- function(
  extent,
  t0,
  repair,
  ratio_bounds,
  collapse_ratio,
  min_neighbour_ha
) {
  window <- .backcast_scan_window(extent, t0)
  if (nrow(window) == 0) {
    return(list(
      extent = extent,
      t0 = t0,
      diag = .backcast_diag_rows(NULL, "", "")
    ))
  }
  flags <- check_extent_jumps(window, ratio_bounds = ratio_bounds)
  if (nrow(flags) == 0) {
    return(list(
      extent = extent,
      t0 = t0,
      diag = .backcast_diag_rows(NULL, "", "")
    ))
  }
  if (!repair) {
    return(.backcast_refuse_flagged(extent, flags, t0, "not repaired"))
  }
  fixed <- .repair_extent_collapse(extent, collapse_ratio, min_neighbour_ha)
  again <- check_extent_jumps(
    .backcast_scan_window(fixed$extent, t0),
    ratio_bounds = ratio_bounds
  )
  out <- .backcast_refuse_flagged(
    fixed$extent,
    again,
    t0,
    "repair insufficient"
  )
  list(
    extent = out$extent,
    t0 = out$t0,
    diag = dplyr::bind_rows(fixed$diag, out$diag)
  )
}

# The years an extent series is scanned over: `t0` and everything before
# it, which is exactly what the back-cast modulates with.
.backcast_scan_window <- function(extent, t0) {
  keys <- intersect(names(extent), .backcast_series_key())
  if (nrow(t0) == 0 || nrow(extent) == 0) {
    return(extent[0, , drop = FALSE])
  }
  bounds <- dplyr::summarise(t0, .t0 = max(.t0), .by = dplyr::all_of(keys))
  extent |>
    dplyr::inner_join(bounds, by = keys) |>
    dplyr::filter(year <= .t0) |>
    dplyr::select(-.t0)
}

# A flagged unit poisons the renormalisation of every sibling, so the
# whole series is refused rather than that one unit.
.backcast_refuse_flagged <- function(extent, flags, t0, why) {
  if (nrow(flags) == 0) {
    return(list(
      extent = extent,
      t0 = t0,
      diag = .backcast_diag_rows(NULL, "", "")
    ))
  }
  keys <- intersect(names(flags), .backcast_series_key())
  refused <- dplyr::semi_join(
    t0,
    .backcast_distinct_keys(flags, keys),
    by = keys
  )
  list(
    extent = extent,
    t0 = dplyr::anti_join(t0, refused, by = .backcast_series_key()),
    diag = .backcast_diag_rows(
      flags,
      "extent_jump_refused",
      paste0(why, ", ratio ", signif(flags$ratio, 4))
    )
  )
}

#' Unit-keyed analogue of `.fix_luh2_crop_collapse()`.
#'
#' Replaces an isolated single-year extent collapse with the mean of its
#' neighbours, on the rule of `R/build_production.R:627-695`: both
#' neighbours above `min_neighbour_ha` (0.001 Mha = 100 ha there) and the
#' year below `collapse_ratio` (0.02 there) of their mean.
#' @noRd
.repair_extent_collapse <- function(extent, collapse_ratio, min_neighbour_ha) {
  by_cols <- .backcast_extent_keys(extent)
  marked <- extent |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c(by_cols, "year")))) |>
    dplyr::mutate(
      .prev_ha = dplyr::lag(extent_ha),
      .next_ha = dplyr::lead(extent_ha),
      .by = dplyr::all_of(by_cols)
    ) |>
    dplyr::mutate(.neighbour_ha = (.prev_ha + .next_ha) / 2)
  bad <- !is.na(marked$.neighbour_ha) &
    marked$.prev_ha > min_neighbour_ha &
    marked$.next_ha > min_neighbour_ha &
    marked$extent_ha < marked$.neighbour_ha * collapse_ratio
  bad[is.na(bad)] <- FALSE
  repaired <- marked |>
    dplyr::mutate(extent_ha = dplyr::if_else(bad, .neighbour_ha, extent_ha)) |>
    dplyr::select(-.prev_ha, -.next_ha, -.neighbour_ha)
  list(
    extent = repaired,
    diag = .backcast_diag_rows(
      marked[bad, , drop = FALSE],
      "extent_jump_repaired",
      "adjacent-year mean"
    )
  )
}

# --- Private helpers: the completed panel -------------------------------------

# The completed year set for every t0 unit set, carrying each unit's t0
# share, the provenance produced rows inherit, and its extent per year.
.backcast_build_panel <- function(
  shares,
  extent,
  t0,
  extent_full,
  binding_indicator
) {
  years <- .backcast_year_set(extent_full)
  anchors <- .backcast_anchor_rows(
    .backcast_observed(shares, binding_indicator),
    .backcast_clamp_t0(t0, max(years))
  )
  panel <- anchors$data |>
    dplyr::cross_join(tibble::tibble(year = years)) |>
    .backcast_join_extent(extent)
  list(
    data = panel,
    diag = dplyr::bind_rows(
      anchors$diag,
      .backcast_clamp_diag(anchors$data),
      .backcast_supersede_diag(shares, panel)
    )
  )
}

.backcast_year_set <- function(extent) {
  seq(as.integer(min(extent$year)), as.integer(max(extent$year)))
}

# `.t0_ext` is the year whose extent stands in for `E_u(t0)`: `t0`
# itself, or the extent's last year when `t0` lies beyond it.
.backcast_clamp_t0 <- function(t0, last_year) {
  t0 |>
    dplyr::mutate(
      .t0_clamped = .t0 > last_year,
      .t0_ext = pmin(.t0, last_year)
    )
}

# One row per unit of the t0 unit set. A unit unresolved at t0 has no
# identity to back-cast, so its whole series is refused.
.backcast_anchor_rows <- function(observed, t0) {
  rows <- observed |>
    dplyr::inner_join(t0, by = .backcast_series_key()) |>
    dplyr::filter(year == .t0) |>
    dplyr::mutate(.share_t0 = share)
  dup <- .backcast_duplicate_keys(rows, .backcast_unit_key())
  if (nrow(dup) > 0) {
    cli::cli_abort(
      "{nrow(dup)} unit{?s} anchor{?s/} more than one row at their
       {.field t0}."
    )
  }
  unresolved <- rows |>
    dplyr::filter(is.na(level_polity_code)) |>
    .backcast_distinct_keys(.backcast_series_key())
  dropped <- c("year", "share", "treatment_year", "value", "value_flag")
  list(
    data = rows |>
      dplyr::anti_join(unresolved, by = .backcast_series_key()) |>
      dplyr::select(-dplyr::any_of(dropped)),
    diag = .backcast_diag_rows(
      unresolved,
      "unresolved_unit_at_t0",
      "level_polity_code is NA at t0"
    )
  )
}

# Attach the per-year extent and the extent at the anchor year, and mark
# which rows the back-cast owns. `.n_back` counts the rows
# `fill_proxy_growth()` has to fill -- the anchor year excluded, clamped
# or not -- so `max_gap` means there exactly what it means there.
.backcast_join_extent <- function(panel, extent) {
  join_cols <- setdiff(
    intersect(names(extent), names(panel)),
    c("extent_ha", "extent_basis")
  )
  slim <- dplyr::select(extent, dplyr::all_of(c(join_cols, "extent_ha")))
  dup <- .backcast_duplicate_keys(slim, join_cols)
  if (nrow(dup) > 0) {
    cli::cli_abort(
      c(
        "{.arg extent} has {nrow(dup)} unit-year{?s} with more than one
         row.",
        "i" = "It is keyed on {.field {setdiff(names(extent), join_cols)}},
               which {.arg shares} does not carry."
      )
    )
  }
  at_t0 <- slim |>
    dplyr::rename(.t0_ext = year, .extent_t0 = extent_ha)
  panel |>
    dplyr::left_join(slim, by = join_cols) |>
    dplyr::left_join(at_t0, by = c(setdiff(join_cols, "year"), ".t0_ext")) |>
    dplyr::mutate(
      .is_back = year < .t0,
      .n_back = sum(year < .t0 & year < .t0_ext),
      .by = dplyr::all_of(.backcast_unit_key())
    )
}

.backcast_clamp_diag <- function(anchors) {
  .backcast_diag_rows(
    dplyr::filter(anchors, .t0_clamped),
    "luh2_clamped_t0",
    "t0 beyond the extent's last year"
  )
}

# A `seam` override turns observed years below `t0` into back-cast years;
# the observations they replace are counted, never dropped silently.
.backcast_supersede_diag <- function(shares, panel) {
  hit <- panel |>
    dplyr::filter(.is_back) |>
    dplyr::semi_join(shares, by = .backcast_row_key()) |>
    dplyr::select(dplyr::all_of(.backcast_row_key()))
  .backcast_diag_rows(hit, "seam_supersedes_observed", "back-cast wins")
}

# --- Private helpers: the fill ------------------------------------------------

# `fill_proxy_growth()` on the back-cast window, then the plan's own
# direct ratio wherever the growth chain could not express it.
.backcast_run_fill <- function(panel, max_gap, max_gap_linear, tolerance) {
  window <- dplyr::filter(panel, year <= .t0_ext)
  filled <- .backcast_call_fill(window, max_gap, max_gap_linear)
  out <- panel |>
    dplyr::left_join(filled, by = c(.backcast_unit_key(), "year")) |>
    dplyr::mutate(
      .direct = dplyr::if_else(
        !is.na(extent_ha) &
          extent_ha > tolerance &
          !is.na(.extent_t0) &
          .extent_t0 > tolerance,
        .share_t0 * extent_ha / .extent_t0,
        NA_real_
      ),
      .tilde = dplyr::if_else(
        .n_back <= max_gap,
        dplyr::coalesce(.filled, .direct),
        NA_real_
      )
    )
  list(data = out, diag = .backcast_fill_diag(out, max_gap))
}

.backcast_call_fill <- function(window, max_gap, max_gap_linear) {
  by_cols <- .backcast_unit_key()
  input <- window |>
    dplyr::mutate(
      share = dplyr::if_else(year == .t0_ext, .share_t0, NA_real_)
    ) |>
    dplyr::select(dplyr::all_of(c(by_cols, "year", "share", "extent_ha")))
  fill_proxy_growth(
    input,
    value_col = share,
    proxy_col = "extent_ha",
    time_col = year,
    .by = by_cols,
    max_gap = max_gap,
    max_gap_linear = max_gap_linear,
    verbose = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(c(by_cols, "year")), .filled = share)
}

.backcast_fill_diag <- function(panel, max_gap) {
  back <- dplyr::filter(panel, .is_back)
  blocked <- dplyr::filter(back, .n_back > max_gap)
  dplyr::bind_rows(
    .backcast_diag_rows(
      blocked,
      "max_gap_exceeded",
      paste0("run of ", blocked$.n_back, " years")
    ),
    .backcast_diag_rows(
      dplyr::filter(back, is.na(extent_ha), .n_back <= max_gap),
      "missing_extent",
      "no extent row for this unit-year"
    ),
    .backcast_diag_rows(
      dplyr::filter(back, is.na(.filled), !is.na(.direct), .n_back <= max_gap),
      "direct_ratio_repair",
      "growth chain broken; plan formula applied directly"
    )
  )
}

# --- Private helpers: the zero cases ------------------------------------------

.backcast_zero_cases <- function(panel, tolerance) {
  marked <- panel |>
    dplyr::mutate(
      .held = !is.na(.extent_t0) &
        .extent_t0 <= tolerance &
        !is.na(.share_t0) &
        .share_t0 > tolerance,
      .no_share = !is.na(.extent_t0) &
        .extent_t0 <= tolerance &
        !is.na(.share_t0) &
        .share_t0 <= tolerance,
      .zeroed = .is_back &
        !is.na(extent_ha) &
        extent_ha <= tolerance &
        !is.na(.extent_t0) &
        .extent_t0 > tolerance
    ) |>
    dplyr::mutate(
      .tilde = dplyr::case_when(
        .is_back & .held ~ .share_t0,
        .is_back & .no_share ~ 0,
        .zeroed ~ 0,
        TRUE ~ .tilde
      )
    )
  list(data = marked, diag = .backcast_zero_diag(marked))
}

.backcast_zero_diag <- function(panel) {
  units <- panel |>
    dplyr::select(
      dplyr::all_of(.backcast_unit_key()),
      .extent_t0,
      .share_t0,
      .held,
      .no_share
    ) |>
    dplyr::distinct()
  dplyr::bind_rows(
    .backcast_diag_rows(
      dplyr::filter(units, .held),
      "zero_extent_t0_held",
      "E_u(t0) = 0 with a positive t0 share"
    ),
    .backcast_diag_rows(
      dplyr::filter(units, .no_share),
      "zero_extent_t0_no_share",
      "E_u(t0) = 0 with no positive t0 share"
    ),
    .backcast_diag_rows(
      dplyr::filter(panel, .zeroed, !.held),
      "zero_extent_year_zeroed",
      "E_u(t) = 0"
    )
  )
}

# --- Private helpers: renormalisation -----------------------------------------

# Per year, over the t0 unit set: held units keep their t0 share and the
# siblings are renormalised to what is left.
.backcast_renormalise <- function(panel, tolerance) {
  scored <- panel |>
    dplyr::mutate(
      .hold_sum = sum(dplyr::if_else(.held, .tilde, 0), na.rm = TRUE),
      .sib_sum = sum(dplyr::if_else(.held, NA_real_, .tilde), na.rm = TRUE),
      .n_na = sum(!.held & is.na(.tilde)),
      .by = dplyr::all_of(c(.backcast_series_key(), "year"))
    ) |>
    dplyr::mutate(
      .target = 1 - .hold_sum,
      .refuse_why = .backcast_refusal(
        .is_back,
        .n_na,
        1 - .hold_sum,
        .sib_sum,
        tolerance
      )
    )
  final <- scored |>
    dplyr::mutate(
      share = dplyr::case_when(
        !.is_back ~ NA_real_,
        !is.na(.refuse_why) ~ NA_real_,
        .held ~ .tilde,
        .sib_sum > tolerance ~ .tilde * .target / .sib_sum,
        TRUE ~ 0
      )
    )
  list(data = final, diag = .backcast_refusal_diag(final))
}

# Why a back-cast year cannot be renormalised, or `NA` when it can.
.backcast_refusal <- function(is_back, n_na, target, sib_sum, tolerance) {
  dplyr::case_when(
    !is_back ~ NA_character_,
    n_na > 0 ~ "incomplete_unit_set",
    target < -tolerance ~ "held_share_exceeds_one",
    target > tolerance & sib_sum <= tolerance ~ "zero_sum_year_refused",
    TRUE ~ NA_character_
  )
}

.backcast_refusal_diag <- function(panel) {
  refused <- panel |>
    dplyr::filter(.is_back, !is.na(.refuse_why)) |>
    dplyr::select(
      dplyr::all_of(c(.backcast_series_key(), "year")),
      .refuse_why
    ) |>
    dplyr::distinct()
  if (nrow(refused) == 0) {
    return(.backcast_diag_rows(NULL, "", ""))
  }
  purrr::map(
    unique(refused$.refuse_why),
    function(why) {
      .backcast_diag_rows(
        dplyr::filter(refused, .refuse_why == why),
        why,
        "year refused for the whole series"
      )
    }
  ) |>
    purrr::list_rbind()
}

# --- Private helpers: assembly ------------------------------------------------

# The input rows, minus what the back-cast supersedes, plus the produced
# and completed rows, with `treatment` and `gap_rule`.
.backcast_assemble <- function(shares, panel, diag, settings) {
  back <- dplyr::filter(panel, .is_back)
  above <- panel |>
    dplyr::filter(!.is_back) |>
    dplyr::anti_join(shares, by = .backcast_row_key())
  produced <- .backcast_produced_rows(
    dplyr::bind_rows(back, above),
    settings,
    c(names(shares), "treatment", "gap_rule")
  )
  kept <- shares |>
    dplyr::anti_join(back, by = .backcast_row_key()) |>
    dplyr::mutate(treatment = "observed", gap_rule = settings$gap_rule)
  list(
    shares = dplyr::bind_rows(kept, produced) |>
      dplyr::arrange(dplyr::pick(dplyr::all_of(.backcast_row_key()))),
    diagnostics = dplyr::arrange(diag, diagnostic),
    counters = .backcast_counters(diag),
    settings = settings
  )
}

.backcast_produced_rows <- function(panel, settings, keep_cols) {
  panel |>
    dplyr::mutate(
      treatment = dplyr::case_when(
        is.na(share) ~ NA_character_,
        .t0_clamped ~ "luh2_clamped",
        TRUE ~ "backcast_t0_geometry"
      ),
      gap_rule = settings$gap_rule
    ) |>
    dplyr::select(dplyr::any_of(keep_cols))
}

# The last observed year per unit series. Branches on the empty case
# because `max()` on no rows warns and returns `-Inf`.
.backcast_last_years <- function(observed) {
  keys <- .backcast_unit_key()
  if (nrow(observed) == 0) {
    return(dplyr::mutate(
      .backcast_distinct_keys(observed, keys),
      .last = integer(0)
    ))
  }
  dplyr::summarise(observed, .last = max(year), .by = dplyr::all_of(keys))
}

# Interior and trailing gaps: completed rows above `t0` that no
# observation filled, refused under the placeholder gap rule.
.backcast_gap_diag <- function(panel, shares) {
  above <- panel |>
    dplyr::filter(!.is_back) |>
    dplyr::anti_join(shares, by = .backcast_row_key())
  if (nrow(above) == 0) {
    return(.backcast_diag_rows(NULL, "", ""))
  }
  last <- .backcast_last_years(dplyr::filter(shares, !is.na(share)))
  split <- dplyr::left_join(above, last, by = .backcast_unit_key())
  dplyr::bind_rows(
    .backcast_diag_rows(
      dplyr::filter(split, !is.na(.last), year < .last),
      "interior_gap_refused",
      "between observed years"
    ),
    .backcast_diag_rows(
      dplyr::filter(split, is.na(.last) | year >= .last),
      "trailing_gap_refused",
      "after the last observed year"
    )
  )
}

# --- Private helpers: diagnostics ---------------------------------------------

.backcast_diag_vocabulary <- function() {
  c(
    "no_binding_anchor",
    "seam_t0_not_observed",
    "seam_supersedes_observed",
    "unresolved_unit_at_t0",
    "luh2_clamped_t0",
    "missing_extent",
    "extent_jump_repaired",
    "extent_jump_refused",
    "max_gap_exceeded",
    "direct_ratio_repair",
    "zero_extent_t0_held",
    "zero_extent_t0_no_share",
    "zero_extent_year_zeroed",
    "zero_sum_year_refused",
    "held_share_exceeds_one",
    "incomplete_unit_set",
    "interior_gap_refused",
    "trailing_gap_refused"
  )
}

.backcast_diag_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    level_polity_code = character(),
    year = integer(),
    diagnostic = character(),
    detail = character()
  )
}

# Diagnostic rows built from whatever key columns `rows` happens to carry.
.backcast_diag_rows <- function(rows, diagnostic, detail) {
  if (is.null(rows) || nrow(rows) == 0) {
    return(.backcast_diag_prototype())
  }
  tibble::tibble(
    area_code = .backcast_diag_col(rows, "area_code", NA_integer_),
    level = .backcast_diag_col(rows, "level", NA_integer_),
    item_prod_code = .backcast_diag_col(rows, "item_prod_code", NA_integer_),
    level_polity_code = .backcast_diag_col(
      rows,
      "level_polity_code",
      NA_character_
    ),
    year = .backcast_diag_col(rows, "year", NA_integer_),
    diagnostic = diagnostic,
    detail = as.character(detail)
  )
}

.backcast_diag_col <- function(rows, nm, empty) {
  if (!rlang::has_name(rows, nm)) {
    return(rep(empty, nrow(rows)))
  }
  if (is.character(empty)) {
    return(as.character(rows[[nm]]))
  }
  as.integer(rows[[nm]])
}

.backcast_counters <- function(diag) {
  tibble::tibble(diagnostic = .backcast_diag_vocabulary()) |>
    dplyr::left_join(dplyr::count(diag, diagnostic), by = "diagnostic") |>
    dplyr::mutate(n = dplyr::coalesce(n, 0L))
}

.backcast_settings <- function(config, binding_indicator) {
  tibble::tibble(
    gap_rule = "refuse_interior",
    max_gap = as.numeric(config$max_gap),
    max_gap_linear = as.numeric(config$max_gap_linear),
    zero_policy = config$zero_policy,
    repair = config$repair,
    binding_indicator = binding_indicator,
    ratio_bounds_lo = config$ratio_bounds[[1]],
    ratio_bounds_hi = config$ratio_bounds[[2]],
    collapse_ratio = config$collapse_ratio,
    min_neighbour_ha = config$min_neighbour_ha
  )
}

# The knobs of the three sub-steps, bundled rather than spread over eight
# formals (CLAUDE.md: group related arguments into named lists, the way
# `allocate_level_crops()` takes its `config`). This is the one place the
# defaults are written down, and `.backcast_settings()` records what a
# call actually ran with.
.backcast_defaults <- function() {
  list(
    max_gap = Inf,
    max_gap_linear = 0,
    zero_policy = "hold_outside",
    repair = FALSE,
    ratio_bounds = c(0.55, 1.6),
    collapse_ratio = 0.02,
    min_neighbour_ha = 100,
    tolerance = 1e-9
  )
}

# An unknown key is a typo, and a silently ignored typo is a run the
# caller believes carried a setting it never had.
.backcast_config <- function(settings) {
  defaults <- .backcast_defaults()
  named <- is.list(settings) &&
    (length(settings) == 0L || !is.null(names(settings)))
  if (!named) {
    cli::cli_abort("{.arg settings} must be a named list.")
  }
  unknown <- setdiff(names(settings), names(defaults))
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "Unknown {.arg settings} key{?s}: {.field {unknown}}.",
      "i" = "Known key{?s}: {.field {names(defaults)}}."
    ))
  }
  config <- utils::modifyList(defaults, settings)
  config$zero_policy <- rlang::arg_match0(
    config$zero_policy,
    "hold_outside",
    arg_nm = "settings$zero_policy"
  )
  config
}
