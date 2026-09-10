# FOR THE INTEGRATION PASS (whep#1000 T13) -- NSE symbols this file gained and
# that the `utils::globalVariables()` block at the end of `R/utils.R` does not
# yet declare (verified with `lintr::object_usage_linter()`):
#
#   i.unit_mc_ir, i.unit_mc_rf, ir_share, rf_share, unit_ir, unit_ir_land,
#   unit_mc_ir, unit_mc_rf, unit_rf, unit_rf_land
#
# `i.unit_mc_rf` and `i.unit_mc_ir` are data.table join-side names and follow
# the file's existing `i.cropland_ha` / `i.rf_capacity` usages, which are not
# declared either. No export and no example changes here: the new behaviour is
# reached through `config` keys and through `allocate_level_crops()`.

#' Build gridded landuse dataset
#'
#' @description
#' Disaggregate country-level FAOSTAT crop harvested areas to a 0.5-degree
#' grid. This reproduces the core spatialization workflow of the
#' [LandInG](https://github.com/PIK-LPJmL/LandInG) toolbox, adapted to
#' WHEP conventions and tidy data structures.
#'
#' The algorithm follows three main steps:
#' 1. Each crop's country total is distributed to grid cells proportionally
#'    to a spatial reference pattern (e.g. Monfreda) weighted by gridded
#'    cropland extent (e.g. LUH2/HYDE).
#' 2. If total allocated harvested area in any cell exceeds its capacity
#'    (cropland times multi-cropping suitability), excess is iteratively
#'    redistributed using a logit-based transformation.
#' 3. Individual crops are aggregated into crop functional types (CFTs).
#'
#' @param country_areas A tibble with country-level crop harvested areas.
#'   Expected columns:
#'   - `year`: Integer year.
#'   - `area_code`: Country code (numeric, matching WHEP polities).
#'   - `item_prod_code`: FAOSTAT item code for the crop.
#'   - `harvested_area_ha`: Total harvested area in hectares.
#'   - `irrigated_area_ha`: Irrigated harvested area in hectares
#'     (optional, defaults to 0).
#' @param crop_patterns A tibble with per-cell spatial crop patterns.
#'   Expected columns:
#'   - `lon`: Longitude of cell centre.
#'   - `lat`: Latitude of cell centre.
#'   - `item_prod_code`: FAOSTAT item code.
#'   - `harvest_fraction`: Cropping intensity (Monfreda area divided
#'     by reference cropland).
#' @param gridded_cropland A tibble with per-cell cropland extent.
#'   Expected columns:
#'   - `lon`: Longitude of cell centre.
#'   - `lat`: Latitude of cell centre.
#'   - `year`: Integer year.
#'   - `cropland_ha`: Total cropland area in hectares.
#'   - `irrigated_ha`: Irrigated cropland in hectares (optional,
#'     defaults to 0).
#' @param country_grid A tibble mapping grid cells to countries.
#'   Expected columns:
#'   - `lon`: Longitude of cell centre.
#'   - `lat`: Latitude of cell centre.
#'   - `area_code`: Country code.
#'   - `cell_area_frac` (or `polity_frac`, `area_frac`, `country_frac`):
#'     This polity compartment's share of the physical cell, a partition
#'     summing to 1 over the polities that overlap the cell. Required: a
#'     grid carrying no share is refused, because defaulting it to 1 gives
#'     a border cell wholly to one polity. Pass 1 only where the polity
#'     does own the whole cell. A land fraction (`landfrac`) is a
#'     different quantity and is refused rather than reinterpreted.
#'   Optional columns:
#'   - `polycell_id`, `cell_id`: Stable compartment/cell identifiers
#'     preserved in outputs when present.
#'   - `year` or validity intervals (`valid_from`/`valid_to`,
#'     `start_year`/`end_year`, `from_year`/`to_year`) for historical,
#'     time-varying polity overlays. The start bound is inclusive; the end
#'     bound is **exclusive at a succession** and **inclusive at the open
#'     end**, so 2014 selects `"RUS-2014-2025"` and not `"RUS-1991-2014"`,
#'     while 2025 still selects `"RUS-2014-2025"` because no later interval of
#'     that compartment follows it. See [polities] for the full rule.
#' @param config Named list of optional extras. Unknown keys raise
#'   an error. Recognised keys:
#'   - `years`: Integer vector of years to spatialize. If `NULL`
#'     (default), all years present in `country_areas` are processed.
#'     When supplied, `country_areas`, `gridded_cropland`, and
#'     `type_cropland` are filtered to this set before processing.
#'   - `cft_mapping`: A tibble mapping FAOSTAT items to CFT names
#'     (`item_prod_code`, `cft_name`). If `NULL`, no CFT aggregation
#'     is performed and individual crop results are returned.
#'   - `type_cropland`: A tibble with per-cell, per-year, per-type
#'     cropland (`lon`, `lat`, `year`, `luh2_type`, `type_ha`,
#'     `type_irrig_ha`). When provided alongside `type_mapping`,
#'     each crop is allocated only into cells containing its LUH2
#'     type. If `NULL`, falls back to total cropland.
#'   - `type_mapping`: A tibble (`item_prod_code`, `luh2_type`) that
#'     maps each crop to its LUH2 type. If `NULL`, type-aware
#'     allocation is disabled even when `type_cropland` is provided.
#'   - `multicropping`: A tibble with per-cell multi-cropping
#'     suitability factors. Required columns: `lon`, `lat`,
#'     `mc_rainfed`, `mc_irrigated`. An optional `year` column
#'     keys factors to year (one row per cell per year); when
#'     present, the table is filtered to the current year before
#'     the capacity constraint is applied. When absent, the table
#'     is treated as a static spatial layer applied to every year.
#'     If `NULL` (default), the capacity constraint still runs with
#'     `mc_rainfed = mc_irrigated = 1` (harvested area capped at
#'     physical cropland).
#'   - `max_iterations`: Maximum iterations for the redistribution
#'     loop. Default: `1000L`.
#'   - `expansion_threshold`: Iteration number after which crops are
#'     allowed to expand into cells without an existing pattern.
#'     Default: `100L`.
#'   - `area_key`: Which area code the output is keyed on, `"grid"`
#'     (default) or `"polity_area"`. See *Which area code the output is
#'     keyed on*.
#'   - `mc_factor`: Which multi-cropping factor sets the capacity
#'     ceiling of a granted-depth compartment, `"unit"` (default) or
#'     `"national"`. See *The capacity ceiling and its breach*. A grid
#'     with no `level_polity_code` has no unit, so the two are the same
#'     table there and a level-0 run is unaffected.
#'   - `pattern_extension`: `"none"` (default) or `"granted_units"`.
#'     Under `"granted_units"`, a granted-depth compartment carrying no
#'     `crop_patterns` row for an item the national table gives it a
#'     target for gains one at `harvest_fraction = 0`, so the crop is
#'     placed uniformly over that unit's cropland of its LUH2 type
#'     instead of being dropped. Level-0 rows are never extended.
#'
#' @return A tibble with gridded crop (or CFT) harvested areas.
#'   Columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `year`: Integer year.
#'   - `area_code`: WHEP polity code for this cell compartment.
#'   - `polity_area_code`, `reporting_polity_code`,
#'     `reporting_polity_name`, `reporting_polity_has_geometry`: Polity
#'     metadata for `area_code`.
#'   - `grid_area_code`: Only under `area_key = "polity_area"`; the
#'     reporting code the engine allocated on.
#'   - `polycell_id`, `cell_id`: Preserved when supplied in
#'     `country_grid`.
#'   - `crop_name` or `cft_name`: Crop or CFT identifier.
#'   - `rainfed_ha`: Rainfed harvested area in the cell.
#'   - `irrigated_ha`: Irrigated harvested area in the cell.
#'
#'   The per-compartment capacity breach is a second table and is
#'   therefore **not** returned here: this function's return value is a
#'   contract two in-package consumers and every `run_spatialize()` caller
#'   already depend on, and an attribute would ride through every dplyr
#'   verb into comparisons that are about the allocation. Allocate at a
#'   granted depth with [allocate_level_crops()], which returns the breach
#'   beside the allocation. See *The capacity ceiling and its breach*.
#'
#' @section The capacity ceiling and its breach:
#' The ceiling is soft: `.redistribute_country_dt()` rescales every crop
#' back to its target after the logit passes, so where a compartment's
#' cells cannot hold the target the target wins and the ceiling gives
#' way. That excess is measured rather than absorbed, and returned as the
#' `breach` table of [allocate_level_crops()]: one row per compartment,
#' cell, item and `mc_basis`, with `rf_over` and `ir_over` in hectares,
#' which T14's reconciliation consumes as a table and never as warning
#' text. A cell's ceiling
#' is shared by every item in it, so the cell's excess is attributed to
#' items **pro rata by their allocated area** -- no other split is
#' available, and summing the rows of a cell recovers its excess exactly.
#'
#' `mc_basis` records which multi-cropping factor the row was scored
#' against. Rows carrying a `level_polity_code` are scored twice: at
#' `"national"`, the factor the `multicropping` layer supplies (which is
#' a national figure broadcast to every cell, so it inflates a unit's
#' breach by construction), and at `"unit"`, the unit's own implied
#' intensity -- its whole allocated harvested area over the same physical
#' cropland the ceiling uses, floored at 1 because a factor below 1 would
#' forbid single cropping on land that exists. `in_force` marks the basis
#' `mc_factor` selected, which is the one the redistribution ran against.
#'
#' The unit factor makes a unit's *aggregate* ceiling equal its own
#' allocation by construction, so a breach at `"unit"` is always a
#' within-unit concentration, never a shortfall of the unit as a whole;
#' that is the difference the pair of bases exists to show. The plan
#' phrases the unit factor as unit harvested over unit *herbaceous* LUH2
#' cropland; the ceiling carries no LUH2 type split, so it is computed
#' over the same cropland the ceiling itself multiplies, and a
#' herbaceous-only denominator would leave the two inconsistent.
#'
#' @section Which area code the output is keyed on:
#' The chain allocates *from* a national table keyed on `area_code` and
#' *into* a `country_grid` keyed the same way, so both sides speak the raw
#' reporting vocabulary the grid was rasterized in. WHEP's polity-keyed
#' national tables are aggregated on `polity_area_code` instead, a bucket
#' that a reporting code need not equal: `276` Sudan and `277` South Sudan
#' both fall in bucket `206`. Every such output row therefore carries two
#' territorial keys that disagree, and whether a consumer joins on
#' `area_code` or on `polity_area_code` decides whether Sudan exists in its
#' result (whep#582).
#'
#' `area_key` selects which of the two the output carries. It is not a
#' fallback: `"grid"` is the default, reproduces today's codes
#' bit-for-bit, and warns naming the codes that cannot join;
#' `"polity_area"` resolves each code to its bucket through
#' [polity_area_crosswalk] before the polity columns are attached, so
#' `area_code` and `polity_area_code` agree in every row. It respects
#' `options(whep.unfold_rest_of_world)` (see [folded_reporting_areas()]),
#' so the output and the national tables agree about where a Rest-of-World
#' member's rows belong.
#'
#' Under `"polity_area"` the raw reporting code is **carried, not
#' replaced**: the output gains `grid_area_code`, joined with `+` where two
#' reporting areas of one bucket meet in a cell and their rows collapse. So
#' the fold stays recoverable at the join rather than baked into the output,
#' the shape [build_cell_polity()] adopted for the same reason (whep#579).
#'
#' @section Methodology:
#' This function reimplements the spatial crop allocation from the LandInG
#' toolbox (Ostberg et al. 2023, doi:10.5194/gmd-16-3375-2023) with the
#' following extensions:
#' \itemize{
#'   \item LUH2 crop-functional-type constraints (\code{type_cropland} +
#'     \code{type_mapping} parameters) restrict each crop to cells
#'     containing its LUH2 type (c3ann, c4ann, c3per, c3nfx). LandInG
#'     allocates to total cropland without type constraints.
#'   \item MIRCA2000 crop-specific irrigated fractions (Portmann et al.
#'     2010) for irrigation distribution, falling back to
#'     LUH2-proportional allocation.
#' }
#'
#' @section Data sources:
#' \itemize{
#'   \item Country areas: FAOSTAT QCL via \code{\link{build_primary_production}}
#'   \item Crop patterns: EarthStat / Monfreda et al. (2008)
#'   \item Gridded cropland: LUH2 v2h (Hurtt et al. 2020)
#'   \item Irrigation: MIRCA2000 (Portmann et al. 2010) + LUH2
#' }
#'
#' @export
#'
#' @examples
#' # Minimal example with toy data
#' country_areas <- tibble::tribble(
#'   ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
#'   2000L, 1L, 15L, 1000
#' )
#' crop_patterns <- tibble::tribble(
#'   ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
#'   0.25, 50.25, 15L, 0.6,
#'   0.75, 50.25, 15L, 0.4
#' )
#' gridded_cropland <- tibble::tribble(
#'   ~lon, ~lat, ~year, ~cropland_ha,
#'   0.25, 50.25, 2000L, 800,
#'   0.75, 50.25, 2000L, 500
#' )
#' country_grid <- tibble::tribble(
#'   ~lon, ~lat, ~area_code, ~cell_area_frac,
#'   0.25, 50.25, 1L, 1,
#'   0.75, 50.25, 1L, 1
#' )
#' build_gridded_landuse(
#'   country_areas, crop_patterns, gridded_cropland, country_grid,
#'   config = list(years = 2000L)
#' )
build_gridded_landuse <- function(
  country_areas,
  crop_patterns,
  gridded_cropland,
  country_grid,
  config = list()
) {
  .gridded_landuse_parts(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid,
    config
  )$allocation
}

#' The engine, returning its diagnostics beside the allocation.
#'
#' `build_gridded_landuse()` is this function's `allocation` element and
#' nothing else, so the exported return value keeps the exact shape,
#' classes and attributes it has always had; `allocate_level_crops()` calls
#' this one and returns `breach` as a table of its own.
#' @noRd
.gridded_landuse_parts <- function(
  country_areas,
  crop_patterns,
  gridded_cropland,
  country_grid,
  config = list()
) {
  .validate_landuse_inputs(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )
  country_grid <- .normalize_country_grid(country_grid)
  config <- .resolve_landuse_config(config)
  years <- config$years
  cft_mapping <- config$cft_mapping
  type_cropland <- config$type_cropland
  type_mapping <- config$type_mapping
  multicropping <- config$multicropping
  max_iterations <- config$max_iterations
  expansion_threshold <- config$expansion_threshold

  country_areas <- .ensure_irrigation_cols(country_areas)
  # DELIBERATELY UNGUARDED AT LEVEL 0. `.check_irrigation_within_area()` runs
  # only where the national table states UNIT targets -- the granted-depth
  # path this epic adds, where nothing is pinned and no caller exists yet.
  # Applying it to a container-keyed table would turn inputs that produce
  # output today into an abort, which is a fail-loud-vs-continue change on a
  # published path: a science decision, not a mechanical one, and out of this
  # task's scope. `allocate_level_crops()` runs the same check on the
  # container-keyed table it is handed, before it splits it into units.
  if (rlang::has_name(country_areas, "level_polity_code")) {
    .check_irrigation_within_area(country_areas)
  }
  gridded_cropland <- .ensure_gridded_irrigation(gridded_cropland)

  if (!is.null(years)) {
    years <- sort(unique(as.integer(years)))
    filtered <- .filter_landuse_years(
      years,
      country_areas,
      gridded_cropland,
      type_cropland
    )
    country_areas <- filtered$country_areas
    gridded_cropland <- filtered$gridded_cropland
    type_cropland <- filtered$type_cropland
  }

  .warn_grid_missing_reporters(
    country_areas,
    country_grid,
    "harvested_area_ha",
    "ha of harvested area"
  )

  # Build type lookup: item_prod_code -> luh2_type
  type_lookup <- NULL
  if (!is.null(type_cropland) && !is.null(type_mapping)) {
    type_lookup <- dplyr::select(
      type_mapping,
      item_prod_code,
      luh2_type
    ) |>
      dplyr::distinct()
  }

  years <- sort(unique(country_areas$year))
  n_workers <- config$n_workers

  # Year-invariant work — done once, shared across the year loop.
  # Cartesian: cells × crops. Per-year work just joins cropland_ha onto this.
  country_grid_is_dynamic <- .country_grid_is_dynamic(
    country_grid
  )
  base_grid_cp <- if (country_grid_is_dynamic) {
    NULL
  } else {
    .build_base_grid_cp(country_grid, crop_patterns, type_lookup)
  }

  opts <- list(
    max_iterations = max_iterations,
    expansion_threshold = expansion_threshold,
    mc_factor = config$mc_factor,
    pattern_extension = config$pattern_extension
  )

  .spatialize_one <- function(yr) {
    country_grid_yr <- .filter_country_grid_year(country_grid, yr)
    if (nrow(country_grid_yr) == 0L) {
      cli::cli_abort("No {.arg country_grid} rows valid for year {yr}.")
    }
    .spatialize_year(
      yr,
      country_areas = dplyr::filter(country_areas, year == yr),
      base_grid_cp = if (country_grid_is_dynamic) {
        .build_base_grid_cp(
          country_grid_yr,
          crop_patterns,
          type_lookup
        )
      } else {
        base_grid_cp
      },
      cropland = dplyr::filter(gridded_cropland, year == yr),
      country_grid = country_grid_yr,
      type_cropland_yr = if (!is.null(type_cropland)) {
        dplyr::filter(type_cropland, year == yr)
      },
      multicropping = multicropping,
      opts = opts
    )
  }

  parts <- if (n_workers > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(years, .spatialize_one, mc.cores = n_workers)
  } else {
    purrr::map(years, .spatialize_one)
  }
  result <- data.table::rbindlist(
    purrr::map(parts, "allocation"),
    fill = TRUE
  )
  breach <- data.table::rbindlist(purrr::map(parts, "breach"), fill = TRUE)
  rm(parts)

  if (!is.null(cft_mapping)) {
    result <- .aggregate_to_cft(result, cft_mapping)
  }

  out <- tibble::as_tibble(result) |>
    .spatialize_apply_area_key(
      config$area_key,
      c("rainfed_ha", "irrigated_ha")
    ) |>
    .add_reporting_polity_columns()
  list(allocation = out, breach = tibble::as_tibble(breach))
}

# --- Private helpers ----------------------------------------------------------

#' Spatialize a single year using data.table for all crops at once.
#'
#' Returns a list of two tibbles: `allocation`, the year's gridded rows,
#' and `breach`, the per-compartment capacity excess at both
#' multi-cropping bases.
#' @noRd
.spatialize_year <- function(
  yr,
  country_areas,
  base_grid_cp,
  cropland,
  country_grid,
  type_cropland_yr = NULL,
  multicropping,
  opts
) {
  t_alloc0 <- proc.time()[["elapsed"]]

  ca <- data.table::as.data.table(country_areas)
  # THE ALLOCATION KEY. Every denominator, join and target below groups on
  # exactly the grain the NATIONAL TABLE is keyed at -- the container alone,
  # or the container and its unit -- never on the grid's. A unit-keyed grid
  # under a container-keyed national table is the pattern-implied split and
  # must stay one national total; grouping on the grid there would hand every
  # unit the container's whole total.
  alloc_cols <- .alloc_target_cols(ca, base_grid_cp)
  opts$alloc_cols <- alloc_cols
  base_grid_cp <- .extend_base_grid_pattern(
    base_grid_cp,
    country_grid,
    ca,
    opts
  )
  grid_cp <- .spatialize_cell_potential(
    base_grid_cp,
    cropland,
    type_cropland_yr,
    alloc_cols,
    yr
  )

  # THE ALLOCATION KEY IS SPELLED OUT IN BOTH GRAINS, ON PURPOSE.
  # `.alloc_target_cols()` decides which grain is in force; the join and the
  # share denominators below then write that grain as a LITERAL `on =` /
  # `by =` instead of passing the vector. The territorial join audit
  # (`R/join_audit.R`) reads keys exactly as written, so `on = alloc_cols`
  # resolves to `<dynamic>` and takes the engine's core allocation join --
  # the most important territorial grouping in the spatialization -- out of
  # the gate that exists to watch it. `test_join_audit.R` pins both spellings
  # against `.alloc_target_cols()`, so they cannot drift away from it.
  unit_keyed <- "level_polity_code" %in% alloc_cols

  # Join country_areas (cartesian: each country-crop gets its cells)
  dat <- if (unit_keyed) {
    grid_cp[
      ca,
      on = .(area_code, level_polity_code, item_prod_code),
      nomatch = NA
    ]
  } else {
    grid_cp[ca, on = .(area_code, item_prod_code), nomatch = NA]
  }
  dat <- dat[!is.na(harvested_area_ha)]

  # Compute allocation for ALL (country, crop) pairs in one pass
  dat[, `:=`(
    rf_potential = harvest_fraction * rainfed_ha,
    ir_potential = harvest_fraction * irrigated_ha
  )]
  if (unit_keyed) {
    dat[,
      `:=`(
        rf_pot_sum = sum(rf_potential, na.rm = TRUE),
        ir_pot_sum = sum(ir_potential, na.rm = TRUE),
        rainfed_sum = sum(rainfed_ha, na.rm = TRUE),
        irrigated_sum = sum(irrigated_ha, na.rm = TRUE)
      ),
      by = .(area_code, level_polity_code, item_prod_code)
    ]
  } else {
    dat[,
      `:=`(
        rf_pot_sum = sum(rf_potential, na.rm = TRUE),
        ir_pot_sum = sum(ir_potential, na.rm = TRUE),
        rainfed_sum = sum(rainfed_ha, na.rm = TRUE),
        irrigated_sum = sum(irrigated_ha, na.rm = TRUE)
      ),
      by = .(area_code, item_prod_code)
    ]
  }
  dat[, `:=`(
    rf_uniform = data.table::fifelse(
      rainfed_sum > 0,
      rainfed_ha / rainfed_sum,
      0
    ),
    ir_uniform = data.table::fifelse(
      irrigated_sum > 0,
      irrigated_ha / irrigated_sum,
      0
    )
  )]
  dat[, rainfed_target := harvested_area_ha - irrigated_area_ha]
  dat[, `:=`(
    allocated_rf = data.table::fifelse(
      rf_pot_sum > 0,
      rf_potential / rf_pot_sum * rainfed_target,
      rf_uniform * rainfed_target
    ),
    allocated_ir = data.table::fifelse(
      ir_pot_sum > 0,
      ir_potential / ir_pot_sum * irrigated_area_ha,
      ir_uniform * irrigated_area_ha
    )
  )]

  # Surface (country, crop) pairs whose national area cannot be allocated
  # (no matching grid cell / only zero-cropland cells) before the filter
  # below silently drops them.
  .warn_unallocated_crops(dat, yr, alloc_cols)

  result <- dat[
    allocated_rf > 0 | allocated_ir > 0,
    c(
      .compartment_id_cols(dat),
      "lon",
      "lat",
      "item_prod_code",
      "allocated_rf",
      "allocated_ir"
    ),
    with = FALSE
  ]
  data.table::setnames(
    result,
    c("allocated_rf", "allocated_ir"),
    c("rainfed_ha", "irrigated_ha")
  )

  t_alloc <- round(proc.time()[["elapsed"]] - t_alloc0, 2)

  # Capacity constraint (keep dplyr version for now; can be dt-optimised later)
  multicropping_yr <- multicropping
  if (!is.null(multicropping_yr) && "year" %in% names(multicropping_yr)) {
    multicropping_yr <- dplyr::filter(multicropping_yr, year == yr) |>
      dplyr::select(-year)
  }
  constrained <- .apply_capacity_constraint(
    result,
    cropland,
    country_grid,
    multicropping_yr,
    opts
  )
  result <- dplyr::mutate(constrained$allocated, year = yr, .before = 1L)

  t_cap <- round(proc.time()[["elapsed"]] - t_alloc0 - t_alloc, 2)
  cli::cli_alert(
    "  Year {yr}: {nrow(result)} rows (alloc {t_alloc}s, cap {t_cap}s)"
  )

  list(
    allocation = result,
    breach = dplyr::mutate(constrained$breach, year = yr, .before = 1L)
  )
}

#' The grain the national table is keyed at, which is the allocation key.
#'
#' A `country_areas` carrying `level_polity_code` states a UNIT target, so
#' every share denominator, capacity target and redistribution group below
#' is that unit's; one without states a container target spread by the
#' pattern, which is the pattern-implied split of decision T31(f). The grid
#' does not decide this: a unit-keyed grid under a container-keyed national
#' table is the pattern-implied case, and grouping on the grid there would
#' give every unit the container's whole total.
#' @noRd
.alloc_target_cols <- function(country_areas, grid) {
  base <- c("area_code", "item_prod_code")
  if (!rlang::has_name(country_areas, "level_polity_code")) {
    return(base)
  }
  if (!rlang::has_name(grid, "level_polity_code")) {
    cli::cli_abort(c(
      "{.arg country_areas} carries unit targets
       ({.field level_polity_code}) but {.arg country_grid} has no unit to
       place them in.",
      i = "Allocate at the granted depth with
           {.fn read_level_country_grid} and {.fn build_allocation_layer},
           or drop the unit key from the national table."
    ))
  }
  c("area_code", "level_polity_code", "item_prod_code")
}

#' Attach cropland to the static base grid and apply the LUH2 type split.
#'
#' Extracted from `.spatialize_year()` unchanged except for the grouping
#' key, so the pre-capacity potential the unit-target builder reads
#' (`.alloc_unit_weights()`) is the SAME quantity the engine allocates on,
#' rather than a second implementation of it.
#' @noRd
.spatialize_cell_potential <- function(
  base_grid_cp,
  cropland,
  type_cropland_yr,
  group_cols,
  yr
) {
  cl <- data.table::as.data.table(cropland)
  # Per-year: copy the static base (cells × crops) and attach cropland.
  # `.build_base_grid_cp()` has already dropped the rows no compartment claims,
  # so a surviving NA share would be an unkeyed allocation reaching the engine.
  grid_cp <- data.table::copy(base_grid_cp)
  if (anyNA(grid_cp$cell_area_frac)) {
    cli::cli_abort(
      "{sum(is.na(grid_cp$cell_area_frac))} compartment{?s} in year {yr} have
       no {.field cell_area_frac}."
    )
  }
  grid_cp[
    cl,
    `:=`(
      cropland_ha = i.cropland_ha * cell_area_frac,
      irrigated_ha = i.irrigated_ha * cell_area_frac
    ),
    on = .(lon, lat)
  ]
  grid_cp[, rainfed_ha := cropland_ha - irrigated_ha]

  use_type_aware <- !is.null(type_cropland_yr) &&
    "luh2_type" %in% names(grid_cp)
  if (!use_type_aware) {
    return(grid_cp)
  }
  .spatialize_type_cropland(grid_cp, type_cropland_yr, group_cols)
}

#' Replace cropland with its LUH2-type slice, with the whole-group fallback.
#' @noRd
.spatialize_type_cropland <- function(grid_cp, type_cropland_yr, group_cols) {
  # Preserve original LUH2 total cropland so the fallback path below
  # can restore it for (country, crop) groups with no type potential.
  grid_cp[, `:=`(
    .orig_cropland_ha = cropland_ha,
    .orig_irrigated_ha = irrigated_ha,
    .orig_rainfed_ha = rainfed_ha
  )]
  tc <- data.table::as.data.table(type_cropland_yr)
  grid_cp_tc <- tc[grid_cp, on = .(lon, lat, luh2_type), nomatch = NA]

  # Compute potential for type-aware crops
  grid_cp_tc[
    !is.na(type_ha),
    `:=`(
      cropland_ha = type_ha * cell_area_frac,
      irrigated_ha = type_irrig_ha * cell_area_frac,
      rainfed_ha = (type_ha - type_irrig_ha) * cell_area_frac
    )
  ]

  # `type_cropland` is stored sparse: cells lacking a crop's LUH2 type have
  # no row and join to `type_ha = NA`. Zero those so a crop cannot be placed
  # in a cell lacking its type. Otherwise they would keep the inherited total
  # cropland and both leak allocation and inflate `type_pot`, masking the
  # whole-group fallback below.
  grid_cp_tc[
    is.na(type_ha),
    `:=`(
      cropland_ha = 0,
      irrigated_ha = 0,
      rainfed_ha = 0
    )
  ]

  # Check which allocation group has type potential; fallback where zero.
  # Both grains written out, for the reason `.spatialize_year()` gives where
  # it forms the same key.
  if ("level_polity_code" %in% group_cols) {
    grid_cp_tc[,
      type_pot := sum(harvest_fraction * cropland_ha, na.rm = TRUE),
      by = .(area_code, level_polity_code, item_prod_code)
    ]
  } else {
    grid_cp_tc[,
      type_pot := sum(harvest_fraction * cropland_ha, na.rm = TRUE),
      by = .(area_code, item_prod_code)
    ]
  }
  grid_cp_tc[
    type_pot <= 0,
    `:=`(
      cropland_ha = .orig_cropland_ha,
      irrigated_ha = .orig_irrigated_ha,
      rainfed_ha = .orig_rainfed_ha
    )
  ]
  grid_cp_tc[, `:=`(
    type_pot = NULL,
    luh2_type = NULL,
    .orig_cropland_ha = NULL,
    .orig_irrigated_ha = NULL,
    .orig_rainfed_ha = NULL
  )]
  grid_cp_tc
}

#' Give a granted unit a zero-pattern row for every item it must place.
#'
#' Decision T31(b). `.build_base_grid_cp()` is `crop_patterns`-keyed, so a
#' unit whose cells carry no Monfreda pattern for a crop has NO engine row
#' for it and its target is warned and dropped -- at province grain the
#' common case, and the signature this feature exists to remove. A row at
#' `harvest_fraction = 0` puts the unit back in its own group, where the
#' engine's existing uniform branch (`rf_uniform`, reached when the group's
#' potential is zero) spreads the target over that unit's cropland of the
#' crop's LUH2 type.
#'
#' Only compartments carrying a `level_polity_code` are extended, so no
#' level-0 country's allocation moves; and only items the national table
#' actually gives that compartment's container a target for.
#' @noRd
.extend_base_grid_pattern <- function(base_grid_cp, country_grid, ca, opts) {
  if (opts$pattern_extension == "none") {
    return(base_grid_cp)
  }
  if (!rlang::has_name(country_grid, "level_polity_code")) {
    return(base_grid_cp)
  }
  grid <- data.table::as.data.table(country_grid)
  units <- grid[!is.na(level_polity_code)]
  if (nrow(units) == 0L) {
    return(base_grid_cp)
  }
  # Taken from the GRID, never from `base_grid_cp`: the base is
  # `crop_patterns`-keyed, so a unit whose cells carry no pattern row for any
  # item at all is absent from it entirely, and extending what survives there
  # would reach every unit except the one that needs it most.
  cell_cols <- .compartment_cell_cols(units)
  keep <- setdiff(
    names(units),
    c("item_prod_code", "luh2_type", "harvest_fraction")
  )
  cells <- unique(units[, keep, with = FALSE])
  cells[, harvest_fraction := 0]
  wanted <- unique(data.table::as.data.table(ca)[,
    c("area_code", "item_prod_code"),
    with = FALSE
  ])
  filled <- cells[wanted, on = "area_code", allow.cartesian = TRUE]
  filled <- filled[!is.na(lon)]
  have <- unique(base_grid_cp[,
    c(cell_cols, "item_prod_code"),
    with = FALSE
  ])
  filled <- filled[!have, on = c(cell_cols, "item_prod_code")]
  if (nrow(filled) == 0L) {
    return(base_grid_cp)
  }
  if (rlang::has_name(base_grid_cp, "luh2_type")) {
    types <- unique(base_grid_cp[
      !is.na(luh2_type),
      c("item_prod_code", "luh2_type"),
      with = FALSE
    ])
    filled[types, luh2_type := i.luh2_type, on = "item_prod_code"]
  }
  cli::cli_inform(
    "Pattern extension: {nrow(filled)} zero-pattern compartment-item row{?s}
     added for {dplyr::n_distinct(filled$level_polity_code)} granted unit{?s}."
  )
  out <- data.table::rbindlist(
    list(base_grid_cp, filled),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setkey(out, lon, lat)
  out
}

#' Refuse a national table whose irrigated area exceeds its harvested area.
#'
#' The engine computes `rainfed_target := harvested - irrigated` with no
#' clipping, so such a row allocates a NEGATIVE rainfed area which the
#' output filter (`allocated_rf > 0 | allocated_ir > 0`) then keeps
#' whenever the irrigated part is positive. It has no physical reading:
#' irrigated harvested area is part of harvested area, not additional to
#' it. `.cap_national_irrigation()` in `inst/scripts/prepare_spatialize_all.R`
#' caps a country's irrigation at its national irrigated total, which is a
#' different budget and does not prevent this.
#' @noRd
.check_irrigation_within_area <- function(country_areas, tolerance = 1e-6) {
  over <- which(
    country_areas$irrigated_area_ha >
      country_areas$harvested_area_ha + tolerance
  )
  if (length(over) == 0L) {
    return(invisible(NULL))
  }
  worst <- over[[which.max(
    country_areas$irrigated_area_ha[over] -
      country_areas$harvested_area_ha[over]
  )]]
  cli::cli_abort(
    c(
      "{length(over)} {.arg country_areas} row{?s} have more irrigated than
       harvested area.",
      x = "Worst: area_code {.val {country_areas$area_code[worst]}}, item
           {.val {country_areas$item_prod_code[worst]}},
           {.val {country_areas$irrigated_area_ha[worst]}} irrigated of
           {.val {country_areas$harvested_area_ha[worst]}} harvested.",
      i = "Irrigated harvested area is part of harvested area. Cap it before
           allocating; the unit-target builder floors the rainfed remainder
           at zero and reports the clipped hectares."
    ),
    class = "whep_spatialize_irrigation_over_area"
  )
}

.landuse_config_defaults <- function() {
  list(
    years = NULL,
    cft_mapping = NULL,
    type_cropland = NULL,
    type_mapping = NULL,
    multicropping = NULL,
    max_iterations = 1000L,
    expansion_threshold = 100L,
    n_workers = 1L,
    area_key = "grid",
    mc_factor = "unit",
    pattern_extension = "none"
  )
}

.resolve_landuse_config <- function(config) {
  defaults <- .landuse_config_defaults()
  if (
    !is.list(config) ||
      (length(config) > 0L && is.null(names(config)))
  ) {
    cli::cli_abort("{.arg config} must be a named list.")
  }
  unknown <- setdiff(names(config), names(defaults))
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{length(unknown)} unknown {.arg config} entr{?y/ies} for \\
      {.fn build_gridded_landuse}:",
      "x" = "{.val {unknown}}.",
      "i" = "Known: {.val {names(defaults)}}."
    ))
  }
  config <- utils::modifyList(defaults, config)
  config$area_key <- .resolve_spatialize_area_key(config$area_key)
  config$mc_factor <- rlang::arg_match0(
    config$mc_factor,
    c("unit", "national"),
    arg_nm = "mc_factor"
  )
  config$pattern_extension <- rlang::arg_match0(
    config$pattern_extension,
    c("none", "granted_units"),
    arg_nm = "pattern_extension"
  )
  config
}

#' Validate that required columns exist.
#' @noRd
.validate_landuse_inputs <- function(
  country_areas,
  crop_patterns,
  gridded_cropland,
  country_grid
) {
  .check_columns(
    country_areas,
    c("year", "area_code", "item_prod_code", "harvested_area_ha"),
    "country_areas"
  )
  .check_columns(
    crop_patterns,
    c("lon", "lat", "item_prod_code", "harvest_fraction"),
    "crop_patterns"
  )
  .check_columns(
    gridded_cropland,
    c("lon", "lat", "year", "cropland_ha"),
    "gridded_cropland"
  )
  .check_columns(
    country_grid,
    c("lon", "lat", "area_code"),
    "country_grid"
  )
}

#' Check that a tibble contains required columns.
#' @noRd
.check_columns <- function(data, required_cols, arg_name) {
  missing <- required_cols[!rlang::has_name(data, required_cols)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing columns in {.arg {arg_name}}:",
      "x" = "Column{?s} not found: {.field {missing}}."
    ))
  }
}

#' Ensure irrigation columns exist with default zeros.
#' @noRd
.ensure_irrigation_cols <- function(country_areas) {
  if (!rlang::has_name(country_areas, "irrigated_area_ha")) {
    country_areas <- dplyr::mutate(
      country_areas,
      irrigated_area_ha = 0
    )
  }
  country_areas
}

#' Ensure gridded irrigation column exists.
#' @noRd
.ensure_gridded_irrigation <- function(gridded_cropland) {
  if (!rlang::has_name(gridded_cropland, "irrigated_ha")) {
    gridded_cropland <- dplyr::mutate(
      gridded_cropland,
      irrigated_ha = 0
    )
  }
  gridded_cropland
}

#' Apply multi-cropping capacity constraints.
#'
#' If total harvested area in any cell exceeds cropland times
#' multi-cropping suitability, iteratively redistribute using a
#' logit-based transformation. See Ostberg et al. (2023, GMD) for
#' details.
#' @noRd
.apply_capacity_constraint <- function(
  allocated,
  cropland,
  country_grid,
  multicropping,
  opts
) {
  allocated_dt <- data.table::as.data.table(allocated)
  capacity <- .capacity_bases(
    cropland,
    country_grid,
    multicropping,
    allocated_dt,
    opts$mc_factor
  )
  capacity_join_cols <- .compartment_join_cols(
    allocated_dt,
    capacity$in_force,
    "allocated",
    "country_grid"
  )
  cell_cols <- .compartment_cell_cols(allocated_dt)
  cell_sums <- allocated_dt[,
    list(
      total_rf = sum(rainfed_ha, na.rm = TRUE),
      total_ir = sum(irrigated_ha, na.rm = TRUE)
    ),
    by = cell_cols
  ]
  overloaded <- .capacity_overloaded(
    capacity$in_force,
    cell_sums,
    capacity_join_cols
  )

  if (nrow(overloaded) == 0L) {
    # Nothing exceeded the ceiling in force, so nothing was rescaled and the
    # in-force breach is empty by construction. The ALTERNATIVE basis can
    # still be breached, and that is the comparison decision T31(g) asks
    # for, so it is measured -- but only where a granted depth makes a
    # second basis exist, which keeps the level-0 fast path untouched.
    return(list(
      allocated = allocated,
      breach = .capacity_breach_all(
        allocated_dt,
        capacity,
        capacity_join_cols,
        skip_in_force = TRUE
      )
    ))
  }

  # Find which countries need redistribution
  countries_to_fix <- unique(overloaded$area_code)

  fixed <- .redistribute_countries_dt(
    allocated_dt,
    capacity$in_force,
    countries_to_fix,
    opts
  )

  out_cols <- unique(c(
    .compartment_id_cols(allocated_dt),
    "lon",
    "lat",
    "item_prod_code",
    "rainfed_ha",
    "irrigated_ha"
  ))
  stable <- allocated_dt[
    !(area_code %in% countries_to_fix),
    ..out_cols
  ]

  out <- data.table::rbindlist(
    list(stable, fixed),
    use.names = TRUE,
    fill = TRUE
  )
  breach <- .capacity_breach_all(out, capacity, capacity_join_cols)
  .warn_capacity_breach(
    .capacity_cell_breach(out, capacity$in_force, capacity_join_cols)
  )
  list(allocated = tibble::as_tibble(out), breach = breach)
}

#' Per-compartment capacity at both multi-cropping bases.
#'
#' The physical cropland layer is shared by all polity compartments in a
#' cell, then clipped to each compartment's geographic envelope via
#' `cell_area_frac`. `national` multiplies it by the `multicropping`
#' layer's factor as supplied; `unit` replaces that factor, for
#' granted-depth compartments only, by the unit's own implied intensity.
#' Both are returned whenever the second exists, because decision T31(g)
#' adopts the unit factor and asks for the breach at both.
#' @noRd
.capacity_bases <- function(
  cropland,
  country_grid,
  multicropping,
  allocated_dt,
  mc_factor
) {
  country_cols <- .compartment_id_cols(country_grid)
  country_lookup <- data.table::as.data.table(country_grid)[,
    unique(c(country_cols, "lon", "lat", "cell_area_frac")),
    with = FALSE
  ]
  cropland_dt <- data.table::as.data.table(cropland)
  base <- cropland_dt[country_lookup, on = .(lon, lat), nomatch = 0L]
  if (is.null(multicropping)) {
    base[, `:=`(mc_rainfed = 1, mc_irrigated = 1)]
  } else {
    # Left join: a cropland cell missing from the multicropping layer must
    # stay in the table (not be dropped), defaulting to a multicropping
    # factor of 1 rather than falling through to the Inf/unconstrained
    # default further down (#223).
    mc_dt <- data.table::as.data.table(multicropping)
    base <- mc_dt[base, on = .(lon, lat)]
    base[is.na(mc_rainfed), mc_rainfed := 1]
    base[is.na(mc_irrigated), mc_irrigated := 1]
  }
  national <- .capacity_from_factors(base, country_lookup)
  unit_base <- .unit_mc_factors(base, allocated_dt)
  if (is.null(unit_base)) {
    return(list(
      in_force = national,
      in_force_name = "national",
      bases = list(national = national)
    ))
  }
  unit <- .capacity_from_factors(unit_base, country_lookup)
  list(
    in_force = if (mc_factor == "unit") unit else national,
    in_force_name = mc_factor,
    bases = list(national = national, unit = unit)
  )
}

.capacity_from_factors <- function(base, country_lookup) {
  out <- data.table::copy(base)
  out[, `:=`(
    rf_capacity = (cropland_ha - irrigated_ha) * cell_area_frac * mc_rainfed,
    ir_capacity = irrigated_ha * cell_area_frac * mc_irrigated
  )]
  out[,
    c(.compartment_cell_cols(country_lookup), "rf_capacity", "ir_capacity"),
    with = FALSE
  ]
}

#' The unit's own implied multi-cropping factor (decision T31(g)).
#'
#' A unit's whole allocated harvested area over the same physical cropland
#' the ceiling multiplies, floored at 1: below 1 the factor would forbid
#' single cropping on land that exists, which is not a capacity statement
#' but an observation about how little of the unit is sown. A unit with no
#' cropland of that water regime keeps 1, since no multiplier makes
#' capacity out of zero land. `NULL` when no compartment carries a unit,
#' which is every level-0 grid.
#' @noRd
.unit_mc_factors <- function(base, allocated_dt) {
  if (
    !rlang::has_name(base, "level_polity_code") ||
      !rlang::has_name(allocated_dt, "level_polity_code") ||
      all(is.na(base$level_polity_code))
  ) {
    return(NULL)
  }
  unit_cols <- c("area_code", "level_polity_code")
  land <- base[,
    list(
      unit_rf_land = sum((cropland_ha - irrigated_ha) * cell_area_frac),
      unit_ir_land = sum(irrigated_ha * cell_area_frac)
    ),
    by = unit_cols
  ]
  sown <- allocated_dt[,
    list(
      unit_rf = sum(rainfed_ha, na.rm = TRUE),
      unit_ir = sum(irrigated_ha, na.rm = TRUE)
    ),
    by = unit_cols
  ]
  factors <- land[sown, on = unit_cols, nomatch = NA]
  factors[, `:=`(
    unit_mc_rf = .safe_intensity(unit_rf, unit_rf_land),
    unit_mc_ir = .safe_intensity(unit_ir, unit_ir_land)
  )]
  out <- data.table::copy(base)
  out[
    factors,
    `:=`(unit_mc_rf = i.unit_mc_rf, unit_mc_ir = i.unit_mc_ir),
    on = unit_cols
  ]
  out[
    !is.na(level_polity_code) & !is.na(unit_mc_rf),
    `:=`(mc_rainfed = unit_mc_rf, mc_irrigated = unit_mc_ir)
  ]
  out[, `:=`(unit_mc_rf = NULL, unit_mc_ir = NULL)]
  out
}

.safe_intensity <- function(sown, land) {
  data.table::fifelse(is.finite(sown / land), pmax(sown / land, 1), 1)
}

.capacity_overloaded <- function(capacity, cell_sums, join_cols) {
  over <- capacity[cell_sums, on = join_cols, nomatch = 0L]
  over[
    total_rf > rf_capacity + 1e-4 |
      total_ir > ir_capacity + 1e-4
  ]
}

#' Measure every polycell left above its capacity ceiling, at both bases.
#'
#' `.redistribute_country_dt()` rescales each crop back to its target after
#' the logit passes, so when a compartment's cells are collectively too small
#' the target wins and the per-cell ceiling gives way. That is a soft
#' ceiling, and moving the land denominator onto the polycell makes it bite
#' more often, so the breach is measured and RETURNED rather than absorbed
#' or left in a warning string: which invariant should yield is the caller's
#' decision (T14 consumes this table), and it needs the magnitude to make it.
#' @noRd
.capacity_breach_all <- function(
  allocated,
  capacity,
  join_cols,
  skip_in_force = FALSE
) {
  bases <- capacity$bases
  if (skip_in_force) {
    bases <- bases[setdiff(names(bases), capacity$in_force_name)]
  }
  purrr::imap(
    bases,
    \(cap, nm) {
      .capacity_breach_table(allocated, cap, join_cols) |>
        dplyr::mutate(
          mc_basis = nm,
          in_force = identical(nm, capacity$in_force_name)
        )
    }
  ) |>
    purrr::list_rbind()
}

#' One basis' breach, attributed to items pro rata by allocated area.
#'
#' A cell's ceiling is shared by every item allocated into it, so no split
#' of its excess between them is derivable; pro rata by allocated area is
#' the only attribution that sums back to the compartment's own excess
#' exactly, and that identity is what makes the item-keyed table safe to
#' aggregate.
#' @noRd
.capacity_breach_table <- function(allocated, capacity, join_cols) {
  breach <- .capacity_cell_breach(allocated, capacity, join_cols)
  keep <- c(join_cols, "item_prod_code", "rf_over", "ir_over")
  if (nrow(breach) == 0L) {
    empty <- allocated[0L, c(join_cols, "item_prod_code"), with = FALSE]
    empty[, `:=`(rf_over = 0, ir_over = 0)]
    return(tibble::as_tibble(empty))
  }
  rows <- allocated[breach, on = join_cols, nomatch = 0L]
  rows[, `:=`(
    rf_share = data.table::fifelse(total_rf > 0, rainfed_ha / total_rf, 0),
    ir_share = data.table::fifelse(total_ir > 0, irrigated_ha / total_ir, 0)
  )]
  rows[, `:=`(
    rf_over = rf_over * rf_share,
    ir_over = ir_over * ir_share
  )]
  tibble::as_tibble(rows[, keep, with = FALSE])
}

#' Every polycell left above one basis' ceiling, before item attribution.
#'
#' The ceiling is a property of the CELL, so this is the grain the breach is
#' measured at and the grain the warning reports; the item split is applied
#' on top of it, for the returned table only.
#' @noRd
.capacity_cell_breach <- function(allocated, capacity, join_cols) {
  tolerance <- 1e-4
  sums <- allocated[,
    list(
      total_rf = sum(rainfed_ha, na.rm = TRUE),
      total_ir = sum(irrigated_ha, na.rm = TRUE)
    ),
    by = join_cols
  ]
  breach <- capacity[sums, on = join_cols, nomatch = 0L]
  breach[, `:=`(
    rf_over = pmax(total_rf - rf_capacity, 0),
    ir_over = pmax(total_ir - ir_capacity, 0)
  )]
  breach[rf_over > tolerance | ir_over > tolerance]
}

#' Report the breach in force, naming the grain whose total was preserved.
#'
#' Takes the PER-POLYCELL breach, not the item-attributed table: `worst` is
#' the largest single-regime excess of one cell, exactly the figure this
#' warning has always carried. Re-deriving it from the attributed table
#' would take `max()` over per-cell sums of both regimes instead, which is a
#' different statistic and moved the printed number (12670 -> 12808 ha on
#' the level-0 two-country fixture) without anything having decided that it
#' should.
#' @noRd
.warn_capacity_breach <- function(breach) {
  if (nrow(breach) == 0L) {
    return(invisible(NULL))
  }
  excess <- sum(breach$rf_over, na.rm = TRUE) +
    sum(breach$ir_over, na.rm = TRUE)
  worst <- max(c(breach$rf_over, breach$ir_over), na.rm = TRUE)
  codes <- sort(unique(breach$area_code))
  grain <- if (
    rlang::has_name(breach, "level_polity_code") &&
      any(!is.na(breach$level_polity_code))
  ) {
    "unit"
  } else {
    "national"
  }
  cli::cli_warn(c(
    "{nrow(breach)} polycell{?s} hold more harvested area than their capacity;
     {round(excess)} ha over, worst {round(worst)} ha.",
    "x" = "{length(codes)} area_code{?s}: {.val {codes}}.",
    i = "The {grain} total was preserved and the per-cell ceiling gave way."
  ))
}

#' Redistribute excess harvested area for overloaded countries.
#'
#' data.table rewrite: processes one country at a time so temporary
#' redistribution columns stay bounded by one country's crop-cell rows.
#' @noRd
.redistribute_countries_dt <- function(
  allocated,
  capacity,
  countries,
  opts
) {
  join_cols <- .compartment_join_cols(
    allocated,
    capacity,
    "allocated",
    "capacity"
  )

  out_cols <- unique(c(
    .compartment_id_cols(allocated),
    "lon",
    "lat",
    "item_prod_code",
    "rainfed_ha",
    "irrigated_ha"
  ))

  data.table::setindexv(allocated, "area_code")
  data.table::setindexv(capacity, "area_code")
  data.table::setindexv(capacity, join_cols)

  fixed <- vector("list", length(countries))
  for (idx in seq_along(countries)) {
    country <- countries[[idx]]
    work <- allocated[.(country), on = "area_code", nomatch = 0L]
    work <- data.table::copy(work)
    capacity_country <- capacity[.(country), on = "area_code", nomatch = 0L]
    work[
      capacity_country,
      `:=`(
        rf_capacity = i.rf_capacity,
        ir_capacity = i.ir_capacity
      ),
      on = join_cols
    ]

    fixed[[idx]] <- .redistribute_country_dt(work, opts)[, ..out_cols]
  }

  data.table::rbindlist(fixed, use.names = TRUE, fill = TRUE)
}

#' Redistribute for one country using vectorized logit updates.
#'
#' `.crop_group` is the ALLOCATION TARGET's grain inside the country, not
#' the item alone: under a unit-keyed national table the target that binds
#' is the unit's, so the logit passes and the final rescale must conserve
#' each (unit, item) and never move hectares across a unit border. Grouping
#' on the item alone there pools the units of a country, and a unit whose
#' cells are too small has its excess pushed into a sibling that reported a
#' smaller area -- the two units' targets swap, silently, with every
#' national total still reconciling.
#' @noRd
.redistribute_country_dt <- function(work, opts) {
  tolerance <- 1e-4
  max_iterations <- opts$max_iterations
  cell_cols <- .compartment_cell_cols(work)
  target_cols <- setdiff(opts$alloc_cols, "area_code")

  work[, .cell_group := .GRP, by = cell_cols]
  work[, .crop_group := .GRP, by = target_cols]

  cell_capacity <- work[,
    .(
      rf_capacity = data.table::first(rf_capacity),
      ir_capacity = data.table::first(ir_capacity)
    ),
    keyby = .cell_group
  ]
  per_crop_target <- work[,
    .(
      target_rf = sum(rainfed_ha, na.rm = TRUE),
      target_ir = sum(irrigated_ha, na.rm = TRUE)
    ),
    keyby = .crop_group
  ]

  cell_group <- work$.cell_group
  crop_group <- work$.crop_group
  rf_capacity <- cell_capacity$rf_capacity
  ir_capacity <- cell_capacity$ir_capacity
  rf_capacity[is.na(rf_capacity)] <- Inf
  ir_capacity[is.na(ir_capacity)] <- Inf
  target_rf <- per_crop_target$target_rf
  target_ir <- per_crop_target$target_ir
  rainfed_vec <- work$rainfed_ha
  irrigated_vec <- work$irrigated_ha
  n_cells <- length(rf_capacity)
  n_crops <- length(target_rf)

  for (iter in seq_len(max_iterations)) {
    total_rf <- .sum_by_group(cell_group, rainfed_vec, n_cells)
    total_ir <- .sum_by_group(cell_group, irrigated_vec, n_cells)
    rf_excess <- pmax(total_rf - rf_capacity, 0)
    ir_excess <- pmax(total_ir - ir_capacity, 0)

    max_excess <- max(
      max(rf_excess, na.rm = TRUE),
      max(ir_excess, na.rm = TRUE),
      na.rm = TRUE
    )
    if (!is.finite(max_excess)) {
      max_excess <- 0
    }

    if (max_excess <= tolerance) {
      break
    }

    pass_target_ir <- .sum_by_group(crop_group, irrigated_vec, n_crops)
    over_ir <- ir_excess > 0 & total_ir > 0
    if (any(over_ir, na.rm = TRUE)) {
      ir_scale <- rep(1, n_cells)
      ir_scale[over_ir] <- ir_capacity[over_ir] / total_ir[over_ir]
      irrigated_vec <- irrigated_vec * ir_scale[cell_group]
    }

    current_ir <- .sum_by_group(
      crop_group,
      irrigated_vec,
      n_crops
    )
    increment_ir <- numeric(n_crops)
    grow_ir <- current_ir > 0 & pass_target_ir > 0
    increment_ir[grow_ir] <- (pass_target_ir[grow_ir] - current_ir[grow_ir]) /
      current_ir[grow_ir]
    ir_idx <- which(
      !over_ir[cell_group] &
        increment_ir[crop_group] != 0 &
        ir_capacity[cell_group] > 0 &
        is.finite(ir_capacity[cell_group])
    )
    if (length(ir_idx) > 0L) {
      frac <- pmax(
        pmin(irrigated_vec[ir_idx] / ir_capacity[cell_group[ir_idx]], 1 - 1e-6),
        1e-6
      )
      irrigated_vec[ir_idx] <- .logistic(
        .logit(frac) + increment_ir[crop_group[ir_idx]]
      ) *
        ir_capacity[cell_group[ir_idx]]
    }

    pass_target_rf <- .sum_by_group(crop_group, rainfed_vec, n_crops)
    over_rf <- rf_excess > 0 & total_rf > 0
    if (any(over_rf, na.rm = TRUE)) {
      rf_scale <- rep(1, n_cells)
      rf_scale[over_rf] <- rf_capacity[over_rf] / total_rf[over_rf]
      rainfed_vec <- rainfed_vec * rf_scale[cell_group]
    }

    current_rf <- .sum_by_group(
      crop_group,
      rainfed_vec,
      n_crops
    )
    increment_rf <- numeric(n_crops)
    grow_rf <- current_rf > 0 & pass_target_rf > 0
    increment_rf[grow_rf] <- (pass_target_rf[grow_rf] - current_rf[grow_rf]) /
      current_rf[grow_rf]
    rf_idx <- which(
      !over_rf[cell_group] &
        increment_rf[crop_group] != 0 &
        rf_capacity[cell_group] > 0 &
        is.finite(rf_capacity[cell_group])
    )
    if (length(rf_idx) > 0L) {
      frac <- pmax(
        pmin(rainfed_vec[rf_idx] / rf_capacity[cell_group[rf_idx]], 1 - 1e-6),
        1e-6
      )
      rainfed_vec[rf_idx] <- .logistic(
        .logit(frac) + increment_rf[crop_group[rf_idx]]
      ) *
        rf_capacity[cell_group[rf_idx]]
    }
  }

  current_rf <- .sum_by_group(crop_group, rainfed_vec, n_crops)
  current_ir <- .sum_by_group(crop_group, irrigated_vec, n_crops)
  rf_scale <- rep(1, n_crops)
  ir_scale <- rep(1, n_crops)
  scale_rf <- current_rf > 0 & target_rf > 0
  scale_ir <- current_ir > 0 & target_ir > 0
  rf_scale[scale_rf] <- target_rf[scale_rf] / current_rf[scale_rf]
  ir_scale[scale_ir] <- target_ir[scale_ir] / current_ir[scale_ir]

  work[, `:=`(
    rainfed_ha = rainfed_vec * rf_scale[crop_group],
    irrigated_ha = irrigated_vec * ir_scale[crop_group],
    .cell_group = NULL,
    .crop_group = NULL
  )]

  work
}

#' Sum numeric values by precomputed positive integer group.
#' @noRd
.sum_by_group <- function(group, weights, n_groups) {
  if (anyNA(weights)) {
    weights[is.na(weights)] <- 0
  }
  summed <- rowsum(weights, group, reorder = FALSE)
  out <- numeric(n_groups)
  out[as.integer(rownames(summed))] <- as.numeric(summed[, 1L])
  out
}

#' Logit transformation.
#' @noRd
.logit <- function(p) {
  log(p / (1 - p))
}

#' Logistic (inverse logit) transformation.
#' @noRd
.logistic <- function(x) {
  1 / (1 + exp(-x))
}

#' Filter year-keyed inputs to the requested years.
#'
#' Warns if any requested year is absent from `country_areas` and
#' filters all three year-keyed inputs accordingly. Returns a list
#' with the filtered tibbles.
#' @noRd
.filter_landuse_years <- function(
  years,
  country_areas,
  gridded_cropland,
  type_cropland
) {
  available <- unique(as.integer(country_areas$year))
  missing_years <- setdiff(years, available)
  if (length(missing_years) > 0L) {
    cli::cli_warn(c(
      "{length(missing_years)} requested year{?s} not found in \\
       {.arg country_areas}:",
      "x" = "{.val {missing_years}}."
    ))
  }
  country_areas <- dplyr::filter(country_areas, year %in% years)
  gridded_cropland <- dplyr::filter(gridded_cropland, year %in% years)
  if (!is.null(type_cropland)) {
    type_cropland <- dplyr::filter(type_cropland, year %in% years)
  }
  list(
    country_areas = country_areas,
    gridded_cropland = gridded_cropland,
    type_cropland = type_cropland
  )
}

#' Warn about country-crops whose national area cannot be allocated.
#'
#' A (country, crop) with a positive harvested area but no matching grid
#' cell (or only zero-cropland cells) receives a zero allocation and would
#' otherwise be dropped silently, leaking its national total. Detect these
#' and warn with the count, leaked area, and identities.
#' @noRd
.warn_unallocated_crops <- function(dat, yr, alloc_cols) {
  # Both grains written out, for the reason `.spatialize_year()` gives where
  # it forms the same key.
  leaked <- if ("level_polity_code" %in% alloc_cols) {
    dat[,
      list(
        national_area = harvested_area_ha[1L],
        allocated = sum(allocated_rf + allocated_ir, na.rm = TRUE)
      ),
      by = .(area_code, level_polity_code, item_prod_code)
    ]
  } else {
    dat[,
      list(
        national_area = harvested_area_ha[1L],
        allocated = sum(allocated_rf + allocated_ir, na.rm = TRUE)
      ),
      by = .(area_code, item_prod_code)
    ]
  }
  leaked <- leaked[national_area > 0 & allocated <= 0]
  if (nrow(leaked) == 0L) {
    return(invisible(NULL))
  }
  # `codes` is integer, so the plural marker must follow an explicit scalar
  # count: cli's make_quantity() errors on a numeric vector of length > 1.
  codes <- sort(unique(leaked$area_code))
  grain <- if ("level_polity_code" %in% alloc_cols) {
    "(country, unit, crop)"
  } else {
    "(country, crop)"
  }
  # `cli::qty()` restates the quantity immediately before `{?s}`. Without it
  # the interpolated `{grain}` sits between the count and the plural marker,
  # and cli pluralises on `grain` -- a length-1 string -- so every message
  # read "258 (country, crop) pair". Measured against `main`, which had the
  # count adjacent to the marker and pluralised correctly.
  cli::cli_warn(c(
    "{nrow(leaked)} {grain} {cli::qty(nrow(leaked))}pair{?s} in year {yr} \\
     have national harvested area but no allocatable grid cell; \\
     {round(sum(leaked$national_area))} ha dropped:",
    "x" = "{length(codes)} area_code{?s}: {.val {codes}}."
  ))
}

#' Aggregate crop-level results to CFT level.
#' @noRd
.aggregate_to_cft <- function(data, cft_mapping) {
  .assert_unique_cft_mapping(cft_mapping)
  group_cols <- unique(c(
    .compartment_id_cols(data),
    "lon",
    "lat",
    "year",
    "cft_name"
  ))
  data |>
    dplyr::inner_join(
      dplyr::select(cft_mapping, item_prod_code, cft_name),
      by = "item_prod_code"
    ) |>
    dplyr::summarise(
      rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
      irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
      .by = dplyr::all_of(group_cols)
    )
}

#' Guard a CFT mapping table against fan-out on its join key.
#'
#' `item_prod_code` is the key every `cft_mapping` join uses (here and in
#' `.write_landuse_outputs()` in `R/run_spatialize.R`). A second row for the
#' same code would duplicate every matching crop-cell row and double-count
#' its hectares in the CFT total (#224), so abort loudly instead of silently
#' `distinct()`-ing the table -- a repeated code is a data defect to fix at
#' the source, not one to paper over.
#' @noRd
.assert_unique_cft_mapping <- function(cft_mapping) {
  dupes <- unique(
    cft_mapping$item_prod_code[duplicated(cft_mapping$item_prod_code)]
  )
  if (length(dupes) > 0L) {
    cli::cli_abort(c(
      "{.arg cft_mapping} must have one row per \\
       {.field item_prod_code}.",
      i = "Duplicated code{?s}: {.val {dupes}}."
    ))
  }
  invisible(cft_mapping)
}

# `.normalize_to_cropland()` and `.get_area_code_from_grid()` were removed in
# C8 (AM-5 risk 26). Both were unreachable -- no caller anywhere in the package
# -- and both were keyed on the PHYSICAL cell, so they are exactly what a
# future reader reaches for when totals exceed cropland:
#   * `.normalize_to_cropland()` grouped its scale factor on `(lon, lat, year)`,
#     so one polity's allocation was scaled down by its neighbour's overload;
#   * `.get_area_code_from_grid()` `left_join`ed on `(lon, lat)` and then
#     `pull`ed, which returns MORE values than rows under a multi-row grid and
#     silently misaligns every downstream column.
# Reinstate neither. The polycell-keyed equivalents are
# `.apply_capacity_constraint()` (with `.warn_capacity_breach()`) and
# `.compartment_id_cols()` carried through from `country_grid`.
