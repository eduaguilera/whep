#' Run the gridded land-use spatialization pipeline
#'
#' @description
#' Wrapper around [build_gridded_landuse()] that resolves a named
#' preset (`"lpjml"` or `"whep"`) into a consistent bundle of input
#' files, engine flags, and output paths. Use this to produce two
#' comparable outputs from the same prepared parquet inputs:
#' an LPJmL/LandInG-faithful run (for cell-by-cell comparison against
#' LPJmL inputs) and the full WHEP run (all historical years,
#' LUH2 type-aware allocation).
#'
#' Presets can be combined with per-flag `overrides` to produce any
#' intermediate configuration; the resolved configuration is written
#' next to the outputs as `run_metadata.yaml` for traceability.
#'
#' @param preset One of `"lpjml"` or `"whep"`. Selects a default
#'   bundle of engine flags and input choices. See *Presets*.
#' @param years Integer vector of years to spatialize. If `NULL`,
#'   the preset default is used: for `"lpjml"` a 10-year benchmark
#'   sequence (`seq(1850L, 2020L, by = 10L)`), intersected with the
#'   years available in `country_areas`; for `"whep"` all years
#'   present in `country_areas`.
#' @param components Character vector selecting which engines to run.
#'   Defaults to `c("landuse", "livestock")`. Pass a subset to run
#'   only one (e.g. `"landuse"`). Unknown entries raise an error.
#' @param overrides Named list of flags that override the preset.
#'   Unknown keys raise an error. Recognised entries:
#'   - `use_type_constraint` (logical): enable/disable LUH2
#'     type-aware allocation.
#'   - `aggregate_to_cft` (logical, default `TRUE`): write a
#'     CFT-aggregated parquet alongside the crop-level output.
#'   - `max_iterations`, `expansion_threshold`: forwarded to the
#'     landuse engine.
#'   - `cft_target`: one of `"whep"` (default for
#'     `preset = "whep"`) or `"lpjml"` (default for
#'     `preset = "lpjml"`). Selects which column of
#'     [cft_mapping] drives CFT aggregation: `cft_name`
#'     (granular 33-class WHEP taxonomy) or `cft_lpjml`
#'     (12 LPJmL crop CFTs + single `others` bucket).
#'   - `area_key`: one of `"grid"` (default) or `"polity_area"`,
#'     forwarded to both engines. See [build_gridded_landuse()]'s
#'     *Which area code the output is keyed on*.
#'   - `country_grid`: which cell-to-polity crosswalk the engines
#'     allocate into, `"polycell"` (default), `"centroid"` or
#'     `"fraction"`. See *Which cell-to-polity crosswalk*.
#'   - `grid_vintage`: which vintage of the polycell support the level-0
#'     grid is read at, `"snapshot_2015"` (default) or `"year_aware"`. See
#'     [read_level_country_grid()]'s *Which vintage of the support level 0 is
#'     read at*. The two are different geographies, not two precisions of one:
#'     `"snapshot_2015"` allocates every year of a run into the present-day
#'     cell-to-country map, `"year_aware"` into the map valid that year. The
#'     snapshot remains the default because the national tables are on a
#'     constant-territory basis the historical support cannot receive: see
#'     `.grid_vintages()` and `validation/spatialize_grid_vintage.R`.
#'     Recorded in `run_metadata.yaml` and per row in `method_grid_vintage`.
#'     Not read under `country_grid = "centroid"` or `"fraction"`, which carry
#'     no validity interval at all and are recorded as `"static_crosswalk"`.
#'   - `level` (integer, default `0L`): containment depth the grid is
#'     resolved at. `0L` is today's cell-to-`area_code` grid; `1L` and
#'     deeper key the cells on admin units through
#'     [read_level_country_grid()], which only the `"polycell"`
#'     crosswalk supports.
#'   - `granted_containers` (default `NULL`): the container `area_code`s the
#'     run grants a depth, e.g. `110L` for Japan. **Required whenever
#'     `level > 0`** and refused at `level = 0`; a depth run must say whose
#'     depth it is, because that decides which containment edges
#'     [read_level_country_grid()] reads, which admin-share rows constrain
#'     the run, and which cells [build_allocation_layer()] takes from the
#'     deep grid rather than from level 0. See *What a granted depth runs*.
#'   - `double_claim`: which clashes the depth read's double-claim gate
#'     refuses, `"co_presence"` (default) or `"measured"`, forwarded to
#'     [read_level_country_grid()]. The default is the fail-closed rule.
#'   - `output_level` (integer, default `0L`): grain of the crop output.
#'     `0L` sums granted-depth rows back onto the container, so
#'     `(lon, lat, area_code, item_prod_code, year)` stays unique and the
#'     schema equals a level-0 run's; a positive value returns unit-grain
#'     rows carrying `level_polity_code`. It may not exceed `level`.
#'   - `constraint_exclude` (default `NULL`): container x year ranges to
#'     hold out of the admin constraint, as a list named by `area_code`,
#'     such as `list("840" = 1961:1989)`. That is the key
#'     [resolve_admin_shares()] reads, and it is checked here against the
#'     same rule, so a hold-out this run records is one the resolver can
#'     honour. Recorded in `run_metadata.yaml` and consumed by the
#'     admin-shares resolver; it does not by itself change a run that has
#'     no admin constraint wired.
#'   - `livestock_proxy`: one of `"luh2"` (default) or `"glw3"`, forwarded
#'     to [build_gridded_livestock()]'s `proxy_method`. Under `"glw3"` the
#'     density table is read with [read_glw_density()], which needs a
#'     `WHEP_GLW3_DIR` tree and aborts without one; under `"luh2"` it is
#'     not read at all.
#'   - `livestock_glw_variant`: which GLW3 product a `"glw3"` run
#'     allocates on, `"DA"` (default, the dasymetric rasters) or `"AW"`
#'     (the areal-weighted ones), forwarded to [read_glw_density()]'s
#'     `variant`. They are different within-country geographies, so the
#'     resolved value is recorded twice: in `run_metadata.yaml` with the
#'     rest of the config, and per row in the output's
#'     `method_livestock_proxy` as `"glw3_da"` or `"glw3_aw"`. Ignored
#'     under `livestock_proxy = "luh2"`, which reads no raster.
#' @param paths Named list of filesystem paths. Recognised entries:
#'   - `l_files_dir`: path to the `L_files` root, for local prepared inputs.
#'   - `input_dir`: directory holding the prepared input parquets. If `NULL`
#'     and `l_files_dir` is unset, the pinned WHEP spatialization inputs are
#'     used.
#'   - `out_dir`: output directory. If `NULL`, defaults to
#'     `<l_files_dir>/whep/spatialize/<preset>` when `l_files_dir` is supplied,
#'     otherwise to a session temporary directory (suffixed with `_custom` when
#'     `overrides` is non-empty). Created if missing.
#'
#' @return Invisibly, a named list with `preset`, `components`, `cft_target`,
#'   resolved `config`, `years`, `out_dir`, `output_paths`, and `admin` -- the
#'   resolver's coverage report and the constraint summary at a granted depth,
#'   `NULL` otherwise.
#'
#' @section Presets:
#' \describe{
#'   \item{`lpjml`}{LandInG-faithful configuration: no LUH2
#'     type-aware allocation (`use_type_constraint = FALSE`) and a
#'     short default year sample suited to comparison against
#'     LPJmL inputs.}
#'   \item{`whep`}{Full WHEP configuration: LUH2 type-aware
#'     allocation (`use_type_constraint = TRUE`) and the full
#'     historical year range present in `country_areas`.}
#' }
#'
#' @section Inputs read from `input_dir`:
#' Landuse (`components` contains `"landuse"`):
#' \itemize{
#'   \item `country_areas.parquet`
#'   \item `crop_patterns.parquet`
#'   \item `gridded_cropland.parquet`
#'   \item `country_grid.parquet`
#'   \item `type_cropland.parquet` (required when
#'     `use_type_constraint = TRUE`).
#' }
#' Livestock (`components` contains `"livestock"`):
#' \itemize{
#'   \item `livestock_country_data.parquet`
#'   \item `gridded_pasture.parquet`
#'   \item `gridded_cropland.parquet`, `country_grid.parquet`
#'   \item `manure_pattern.parquet` (optional, enables
#'     manure-intensity weighting if present).
#'   \item `livestock_mapping.csv` from the installed package.
#'   \item The GLW3 rasters under `WHEP_GLW3_DIR`, read only when
#'     `livestock_proxy = "glw3"` (see [read_glw_density()]).
#' }
#'
#' @section What a granted depth runs:
#' With `level > 0` the landuse step does not call [build_gridded_landuse()]
#' on a level-0 grid. It runs the subnational chain, in this order, and each
#' step aborts naming what it is missing rather than continuing on the
#' level-0 pattern:
#'
#' \enumerate{
#'   \item [read_level_country_grid()] twice -- level 0 for the ungranted
#'     countries, `level` scoped to `granted_containers` for the granted ones
#'     -- and [build_allocation_layer()] to assert the two partition every
#'     cell. Both halves are read year-aware, which is what
#'     `method_grid_vintage` has always recorded for a depth run.
#'   \item [read_admin_shares()], scoped to `granted_containers`. A granted
#'     container with no admin row aborts with class
#'     `whep_run_admin_container_absent`; an unregistered pin with
#'     `whep_run_no_admin_shares`.
#'   \item [resolve_admin_units()], turning each source's native identifier
#'     into a polity code under the code system that identifier belongs to.
#'     Rows resolving to nothing are dropped and counted; a source resolving
#'     to nothing at all aborts with `whep_run_admin_unresolved`, and a
#'     source whose code system is undeclared with
#'     `whep_run_admin_code_system`.
#'   \item [resolve_admin_shares()], applying indicator and source precedence
#'     and honouring `constraint_exclude`. A constraint sharing no
#'     `level_polity_code` with the layer aborts with
#'     `whep_run_admin_layer_mismatch`, because a run whose constraint
#'     matched nothing is indistinguishable from an unconstrained one in
#'     every output it writes.
#'   \item [allocate_level_crops()], splitting each national total across the
#'     container's units and spreading each unit's target over that unit's
#'     cells.
#'   \item [reconcile_admin_allocation()] and [seam_gate()], written beside
#'     the parquets as the run's own audit trail.
#' }
#'
#' What actually happened is recorded in `run_metadata.yaml` under
#' `admin_constraint` -- the resolved, dropped and held-out row counts, the
#' units constrained, the `method_crop_alloc` tally over the targets, the
#' share basis the gate judged on, and the gate's verdict -- and per row in
#' the targets' own `method_crop_alloc`. A level-0 run records
#' `admin_constraint: none`.
#'
#' @section Which cell-to-polity crosswalk:
#' The producer builds two crosswalks from the same polygons.
#' `"centroid"` is the deployed `spatialize-country-grid` pin: one
#' `area_code` per 0.5-degree cell, winner-take-all at a border, no share
#' column, so a whole border cell goes to a single polity. `"fraction"` is
#' `cell_polity_fraction.parquet`, which splits each border cell by
#' fractional coverage; the engines already read its `polity_frac` as
#' `cell_area_frac`, so no engine change is involved.
#'
#' They are alternatives, never a fallback. The fractional parquet used to
#' carry a different area vocabulary from the centroid grid — it keyed
#' Ethiopia `62` and Sudan `206` where today's `regions.csv` uses `238` and
#' `276`, so substituting it dropped both countries entirely (whep#461).
#' Regenerating it closed that gap: the two grids now carry the same 178 area
#' codes, it is published as the `spatialize-cell-polity-fraction` pin so no
#' user has to rebuild it, and [build_cell_polity()] refuses a copy still
#' holding a retired code instead of deleting the countries silently
#' (whep#694). It still cannot
#' rescue a polity smaller than a cell, because its producer restricts it to
#' the cells the centroid grid already has, and it drops 4 of those cells,
#' whose only land is a sliver covering the 0.5-degree cell centre but no
#' 1/12-degree subcell centre. Whichever is selected,
#' `build_gridded_landuse()` and
#' `build_gridded_livestock()` now warn once per call naming every reporting
#' area the chosen grid has no cell for and the national total at stake.
#'
#' @section Outputs written to `out_dir`:
#' Every parquet below carries `method_grid_vintage`, the geography the run
#' allocated into: `"year_aware"`, `"snapshot_2015"` or `"static_crosswalk"`.
#' \itemize{
#'   \item `gridded_landuse_crops.parquet` — crop-level output.
#'   \item `gridded_landuse.parquet` — CFT-aggregated output
#'     (when `aggregate_to_cft = TRUE`).
#'   \item `gridded_livestock_emissions.parquet` — gridded
#'     livestock stocks and emissions (when livestock component
#'     selected).
#'   \item `run_metadata.yaml` — resolved preset, components,
#'     flags, years, timestamp, package version, and the resolved
#'     `method_grid_vintage`.
#'   \item `admin_coverage.csv` — which admin source constrained each
#'     container x item x year, at what tier, grain and depth. Written
#'     only when `level > 0`, with its header and no rows where no
#'     coverage is granted. See [admin_coverage_prototype()].
#'   \item Eleven further CSVs, written only when `level > 0` and the
#'     landuse component ran: `admin_targets.csv` (one row per unit, item and
#'     year with its share, target and `method_crop_alloc`),
#'     `admin_group_coverage.csv`, `admin_conservation.csv`,
#'     `admin_breach.csv`, `admin_reconciliation.csv`,
#'     `admin_reconciliation_units.csv`, `admin_unit_cropland.csv`,
#'     `admin_seams.csv` and the three seam-gate tiers
#'     `admin_seam_gate_a.csv`, `admin_seam_gate_b.csv`,
#'     `admin_seam_gate_c.csv`.
#' }
#'
#' @seealso [build_gridded_landuse()].
#'
#' @export
#'
#' @examples
#' # Dispatch to the engine with a filtered year range (offline
#' # example; normally called against prepared parquet inputs).
#' country_areas <- tibble::tribble(
#'   ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
#'   1999L,         1L,             15L,                500,
#'   2000L,         1L,             15L,               1000
#' )
#' crop_patterns <- tibble::tribble(
#'   ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
#'    0.25, 50.25,             15L,               0.6,
#'    0.75, 50.25,             15L,               0.4
#' )
#' gridded_cropland <- tibble::tribble(
#'   ~lon,  ~lat,  ~year, ~cropland_ha,
#'    0.25, 50.25, 1999L,          800,
#'    0.75, 50.25, 1999L,          500,
#'    0.25, 50.25, 2000L,          800,
#'    0.75, 50.25, 2000L,          500
#' )
#' country_grid <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code, ~cell_area_frac,
#'    0.25, 50.25,         1L,               1,
#'    0.75, 50.25,         1L,               1
#' )
#' build_gridded_landuse(
#'   country_areas, crop_patterns, gridded_cropland, country_grid,
#'   config = list(years = 2000L)
#' )
run_spatialize <- function(
  preset = c("lpjml", "whep"),
  years = NULL,
  components = c("landuse", "livestock"),
  overrides = list(),
  paths = list()
) {
  preset <- match.arg(preset)
  components <- .validate_components(components)
  .validate_overrides(overrides)
  .validate_paths(paths)
  cft_target <- .resolve_cft_target(overrides$cft_target, preset)
  # Engine overrides = overrides minus cft_target (which affects
  # aggregation target, not engine flags).
  engine_overrides <- overrides[setdiff(names(overrides), "cft_target")]
  config <- .resolve_spatialize_config(preset, engine_overrides)
  config <- .validate_level_config(config, components)

  resolved <- .resolve_paths(paths, preset, overrides)
  input_dir <- resolved$input_dir
  out_dir <- resolved$out_dir
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  .warn_if_out_dir_occupied(out_dir)

  resolved_years <- if (!is.null(years)) {
    sort(unique(as.integer(years)))
  }
  output_paths <- list()
  admin <- NULL

  if ("landuse" %in% components) {
    step <- .run_landuse_step(
      preset,
      years,
      resolved_years,
      config,
      cft_target,
      input_dir,
      out_dir
    )
    resolved_years <- step$years
    output_paths <- c(output_paths, step$paths)
    admin <- step$admin
  }

  if ("livestock" %in% components) {
    step <- .run_livestock_step(
      preset,
      years,
      resolved_years,
      config,
      input_dir,
      out_dir
    )
    resolved_years <- step$years
    output_paths <- c(output_paths, step$paths)
  }

  .write_run_metadata(
    out_dir,
    preset,
    resolved_years,
    components,
    cft_target,
    config,
    overrides,
    input_dir,
    admin
  )
  if (config$level > 0L) {
    output_paths$admin_coverage <- .write_admin_coverage(
      out_dir,
      admin$coverage
    )
  }

  cli::cli_alert_success(
    "Spatialize complete: {.path {out_dir}}"
  )
  invisible(list(
    preset = preset,
    components = components,
    cft_target = cft_target,
    config = config,
    years = resolved_years,
    out_dir = out_dir,
    output_paths = output_paths,
    admin = admin
  ))
}

# --- Private helpers --------------------------------------------------------

.run_landuse_step <- function(
  preset,
  years,
  resolved_years,
  config,
  cft_target,
  input_dir,
  out_dir
) {
  lu_inputs <- .load_landuse_inputs(input_dir, config)
  if (is.null(resolved_years)) {
    resolved_years <- .resolve_years(years, preset, lu_inputs$country_areas)
  }
  cli::cli_h2("Running spatialize: landuse")
  cli::cli_alert_info(
    "Preset: {.val {preset}} | years: {length(resolved_years)} \\
    ({min(resolved_years)}-{max(resolved_years)}) | \\
    type-aware: {.val {config$use_type_constraint}}"
  )
  if (config$level > 0L) {
    return(.run_landuse_depth(
      lu_inputs,
      resolved_years,
      config,
      cft_target,
      out_dir
    ))
  }
  result_crops <- build_gridded_landuse(
    country_areas = lu_inputs$country_areas,
    crop_patterns = lu_inputs$crop_patterns,
    gridded_cropland = lu_inputs$gridded_cropland,
    country_grid = lu_inputs$country_grid,
    config = list(
      type_cropland = lu_inputs$type_cropland,
      type_mapping = lu_inputs$type_mapping,
      multicropping = lu_inputs$multicropping,
      years = resolved_years,
      max_iterations = config$max_iterations,
      expansion_threshold = config$expansion_threshold,
      area_key = config$area_key
    )
  )
  # Decision 10's output grain is applied HERE, after the engine and outside
  # its year loop: the engine allocates at the granted depth, and what is
  # written is a reporting choice. At level 0 the result carries no
  # `level_polity_code` and `.level_fold_output()` returns it identically, so
  # the default path is bit-for-bit what it was.
  result_crops <- .level_fold_output(result_crops, config$output_level)
  list(
    years = resolved_years,
    paths = .write_landuse_outputs(
      result_crops,
      lu_inputs$cft_mapping,
      out_dir,
      config,
      cft_target = cft_target
    )
  )
}

.run_livestock_step <- function(
  preset,
  years,
  resolved_years,
  config,
  input_dir,
  out_dir
) {
  ls_inputs <- .load_livestock_inputs(input_dir, config)
  if (is.null(resolved_years)) {
    resolved_years <- .resolve_years(
      years,
      preset,
      dplyr::select(ls_inputs$livestock_data, year)
    )
  }
  cli::cli_h2("Running spatialize: livestock")
  cli::cli_alert_info(
    "Preset: {.val {preset}} | years: {length(resolved_years)} \\
    ({min(resolved_years)}-{max(resolved_years)})"
  )
  gridded_livestock <- build_gridded_livestock(
    livestock_data = ls_inputs$livestock_data,
    gridded_pasture = ls_inputs$gridded_pasture,
    gridded_cropland = ls_inputs$gridded_cropland,
    country_grid = ls_inputs$country_grid,
    species_proxy = ls_inputs$species_proxy,
    manure_pattern = ls_inputs$manure_pattern,
    glw_density = ls_inputs$glw_density,
    years = resolved_years,
    proxy_method = config$livestock_proxy,
    area_key = config$area_key
  )
  list(
    years = resolved_years,
    paths = .write_livestock_outputs(gridded_livestock, out_dir, config)
  )
}

.spatialize_presets <- function() {
  list(
    lpjml = list(
      use_type_constraint = FALSE,
      aggregate_to_cft = TRUE,
      max_iterations = 1000L,
      expansion_threshold = 100L,
      area_key = "grid",
      country_grid = "polycell",
      grid_vintage = "snapshot_2015",
      level = 0L,
      output_level = 0L,
      granted_containers = NULL,
      double_claim = "co_presence",
      constraint_exclude = NULL,
      livestock_proxy = "luh2",
      livestock_glw_variant = "DA"
    ),
    whep = list(
      use_type_constraint = TRUE,
      aggregate_to_cft = TRUE,
      max_iterations = 1000L,
      expansion_threshold = 100L,
      area_key = "grid",
      country_grid = "polycell",
      grid_vintage = "snapshot_2015",
      level = 0L,
      output_level = 0L,
      granted_containers = NULL,
      double_claim = "co_presence",
      constraint_exclude = NULL,
      livestock_proxy = "luh2",
      livestock_glw_variant = "DA"
    )
  )
}

.known_override_keys <- function() {
  c(
    "use_type_constraint",
    "aggregate_to_cft",
    "max_iterations",
    "expansion_threshold",
    "cft_target",
    "area_key",
    "country_grid",
    "grid_vintage",
    "level",
    "output_level",
    "granted_containers",
    "double_claim",
    "constraint_exclude",
    "livestock_proxy",
    "livestock_glw_variant"
  )
}

.known_path_keys <- function() {
  c("input_dir", "out_dir", "l_files_dir")
}

# Which GLW3 product a `"glw3"` run allocates on. Validated here rather
# than left to `read_glw_density()`'s own `arg_match()` so the abort names
# the override key the user set, not the argument it was forwarded to.
#
# `"DA"` is the default because the dasymetric product is the more
# rigorous of the two: it redistributes the census counts with
# high-resolution covariates, where the areal-weighted one spreads them
# evenly over the reporting unit's suitable land. It was the hardwired
# choice before this key existed, so the default reproduces every run made
# until now (whep#1000, wave-7 review finding 5).
.check_glw_run_variant <- function(variant) {
  rlang::arg_match0(
    variant %||% "DA",
    c("DA", "AW"),
    arg_nm = "livestock_glw_variant"
  )
}

.validate_paths <- function(paths) {
  if (length(paths) == 0L) {
    return(invisible(NULL))
  }
  if (is.null(names(paths)) || any(names(paths) == "")) {
    cli::cli_abort("{.arg paths} must be a fully named list.")
  }
  unknown <- setdiff(names(paths), .known_path_keys())
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{length(unknown)} unknown {.arg paths} entr{?y/ies}:",
      "x" = "{.val {unknown}}.",
      "i" = "Known: {.val {(.known_path_keys())}}."
    ))
  }
  invisible(NULL)
}

.resolve_paths <- function(paths, preset, overrides) {
  l_files_dir <- paths$l_files_dir
  if (!is.null(l_files_dir) && !dir.exists(l_files_dir)) {
    cli::cli_abort(c(
      "{.arg paths$l_files_dir} must be an existing directory.",
      "x" = "{.path {l_files_dir}} does not exist."
    ))
  }
  input_dir <- if (!is.null(paths$input_dir)) {
    paths$input_dir
  } else if (!is.null(l_files_dir)) {
    file.path(l_files_dir, "whep", "inputs")
  } else {
    NULL
  }
  out_dir <- if (is.null(paths$out_dir)) {
    if (!is.null(l_files_dir)) {
      .default_spatialize_out_dir(l_files_dir, preset, overrides)
    } else {
      .default_pin_spat_out(preset, overrides)
    }
  } else {
    paths$out_dir
  }
  list(
    l_files_dir = l_files_dir,
    input_dir = input_dir,
    out_dir = out_dir
  )
}

.known_components <- function() {
  c("landuse", "livestock")
}

.validate_components <- function(components) {
  if (length(components) == 0L) {
    cli::cli_abort("{.arg components} must not be empty.")
  }
  unknown <- setdiff(components, .known_components())
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{length(unknown)} unknown {.arg components} \\
      entr{?y/ies}: {.val {unknown}}.",
      "i" = "Known: {.val {(.known_components())}}."
    ))
  }
  unique(components)
}

.validate_overrides <- function(overrides) {
  if (length(overrides) == 0L) {
    return(invisible(NULL))
  }
  if (is.null(names(overrides)) || any(names(overrides) == "")) {
    cli::cli_abort("{.arg overrides} must be a fully named list.")
  }
  unknown <- setdiff(names(overrides), .known_override_keys())
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{length(unknown)} unknown {.arg overrides} entr{?y/ies}:",
      "x" = "{.val {unknown}}.",
      "i" = "Known: {.val {(.known_override_keys())}}."
    ))
  }
  invisible(NULL)
}

.resolve_spatialize_config <- function(preset, overrides) {
  defaults <- .spatialize_presets()[[preset]]
  utils::modifyList(defaults, overrides)
}

# Normalise and cross-check the depth keys once, before any input is read
# (whep#1000 T12). `modifyList()` will happily take `level = 1.5` or an
# `output_level` finer than the depth actually allocated, and both would only
# surface as a wrong output grain hours into a run.
.validate_level_config <- function(config, components) {
  config$level <- .check_grid_level(config$level, "overrides$level")
  config$output_level <- .check_grid_level(
    config$output_level,
    "overrides$output_level"
  )
  if (config$output_level > config$level) {
    cli::cli_abort(c(
      "{.code output_level = {config$output_level}} is finer than
       {.code level = {config$level}}.",
      i = "The output cannot be reported at a depth the run did not
           allocate at."
    ))
  }
  config <- .validate_granted_containers(config)
  config$double_claim <- rlang::arg_match0(
    config$double_claim %||% "co_presence",
    c("co_presence", "measured"),
    arg_nm = "overrides$double_claim"
  )
  config$grid_vintage <- .check_grid_vintage(
    config$grid_vintage,
    "overrides$grid_vintage"
  )
  config$livestock_proxy <- rlang::arg_match0(
    config$livestock_proxy %||% "luh2",
    c("luh2", "glw3"),
    arg_nm = "overrides$livestock_proxy"
  )
  # Resolved even under `"luh2"`, which reads no raster: `run_metadata.yaml`
  # then records the same key for every run, and a typo is caught when the
  # run is configured rather than after the landuse component has already
  # spent its hours.
  config$livestock_glw_variant <- .check_glw_run_variant(
    config$livestock_glw_variant
  )
  # Assigned through `[` so an empty hold-out stays a recorded `NULL` key
  # rather than disappearing from the config -- `$<- NULL` deletes the element,
  # and `run_metadata.yaml` would then not say the run had no hold-out.
  config["constraint_exclude"] <- list(
    .check_constraint_exclude(config$constraint_exclude)
  )
  # The livestock engine has no output-grain step yet: T15b owns the two-level
  # livestock allocation, so a depth run writes unit-grain livestock rows while
  # the crop output is folded onto the container. Said out loud rather than
  # left for a consumer to discover from a row count.
  if (config$level > 0L && "livestock" %in% components) {
    cli::cli_warn(c(
      "!" = "Livestock output stays at the grain the grid carries;
             {.code output_level} governs the crop output only.",
      i = "Two-level livestock allocation and its fold are not wired yet
           (whep#1000 T15b)."
    ))
  }
  config
}

# WHICH CONTAINERS THE RUN GRANTS A DEPTH, and the reason the key is required
# rather than defaulted. Until whep#1000 T40 there was no call site for the
# admin machinery at all, so `level = 1L` allocated FAOSTAT national totals on
# the level-0 pattern and ignored every subnational row -- a run that believed
# it was constrained and was not. The fix is not a better default: a depth run
# has to say whose depth it is, because the answer decides which containment
# edges are read, which admin rows constrain, and which cells the layer takes
# from the deep grid rather than from level 0. So a granted depth with no
# containers aborts, and containers with no depth abort too.
.validate_granted_containers <- function(config) {
  granted <- .level_check_containers(
    config$granted_containers,
    "overrides$granted_containers"
  )
  if (config$level > 0L && is.null(granted)) {
    cli::cli_abort(c(
      "{.code level = {config$level}} grants a depth to no container.",
      x = "{.arg overrides$granted_containers} is unset.",
      i = "Name the container {.field area_code}s the run constrains, e.g.
           {.code overrides = list(level = 1L, granted_containers = 110L)}.",
      i = "Without them a depth run would allocate national totals on the
           level-0 pattern and ignore every subnational row."
    ))
  }
  if (config$level == 0L && !is.null(granted)) {
    cli::cli_abort(c(
      "{.arg overrides$granted_containers} names {length(granted)}
       container{?s} but {.code level = 0}.",
      x = "Level 0 resolves no containment edge, so nothing is granted.",
      i = "Set {.code level >= 1} to grant them a depth."
    ))
  }
  # Assigned through `[` so an empty grant stays a recorded `NULL` key rather
  # than disappearing from the config, exactly as `constraint_exclude` does.
  config["granted_containers"] <- list(granted)
  config
}

# `constraint_exclude` is a per-container year hold-out, consumed by the
# admin-shares resolver. Its SHAPE is checked here so a typo is refused at the
# door instead of silently constraining a country the run meant to exclude.
#
# The keys are `area_code`, and the check that they are is the resolver's own
# `.parse_exclude_years()` rather than a copy of its rule: this gate used to
# accept an ISO3 name, so `list(USA = 1961:1989)` passed the door, was written
# into `run_metadata.yaml` as the run's hold-out, and then aborted in
# `resolve_admin_shares()` -- a recorded hold-out its only consumer cannot
# read. The parsed rows are discarded here; the resolver builds them again
# from the same list when the constraint is actually wired.
.check_constraint_exclude <- function(exclude) {
  if (is.null(exclude) || length(exclude) == 0L) {
    return(NULL)
  }
  named <- !is.null(names(exclude)) && all(nzchar(names(exclude)))
  years_ok <- all(purrr::map_lgl(
    exclude,
    \(x) is.numeric(x) && length(x) > 0L && !anyNA(x)
  ))
  if (!is.list(exclude) || !named || !years_ok) {
    cli::cli_abort(c(
      "{.arg overrides$constraint_exclude} must be a fully named list of
       year vectors.",
      i = "For example {.code list(\"840\" = 1961:1989)}."
    ))
  }
  .parse_exclude_years(exclude)
  purrr::map(exclude, \(x) sort(unique(as.integer(x))))
}

.resolve_years <- function(years, preset, country_areas) {
  available <- sort(unique(as.integer(country_areas$year)))
  if (!is.null(years)) {
    return(sort(unique(as.integer(years))))
  }
  if (preset == "lpjml") {
    picked <- intersect(.benchmark_years(), available)
    if (length(picked) == 0L) {
      picked <- available
    }
    return(as.integer(picked))
  }
  available
}

.benchmark_years <- function() {
  as.integer(seq(1850L, 2020L, by = 10L))
}

.warn_if_out_dir_occupied <- function(out_dir) {
  existing <- list.files(
    out_dir,
    pattern = "\\.parquet$",
    full.names = FALSE
  )
  if (length(existing) > 0L) {
    cli::cli_warn(c(
      "Output directory {.path {out_dir}} already contains \\
      {length(existing)} parquet file{?s}; they will be overwritten.",
      "i" = "Existing: {.file {head(existing, 4)}}\\
      {if (length(existing) > 4L) ' (...)' else ''}"
    ))
  }
  invisible(NULL)
}

.resolve_cft_target <- function(cft_target, preset) {
  if (is.null(cft_target)) {
    cft_target <- if (preset == "lpjml") "lpjml" else "whep"
  }
  cft_target <- match.arg(cft_target, c("whep", "lpjml"))
  cft_target
}

.default_spatialize_out_dir <- function(l_files_dir, preset, overrides) {
  base <- file.path(l_files_dir, "whep", "spatialize", preset)
  if (length(overrides) == 0L) {
    return(base)
  }
  paste0(base, "_custom")
}

.default_pin_spat_out <- function(preset, overrides) {
  base <- file.path(tempdir(), "whep_spatialize", preset)
  if (length(overrides) == 0L) {
    return(base)
  }
  paste0(base, "_custom")
}

.load_landuse_inputs <- function(input_dir, config) {
  cft_mapping <- .read_packaged_cft_mapping()
  mapped_items <- cft_mapping$item_prod_code

  country_areas <- .read_spatial_input(
    input_dir,
    "country_areas.parquet",
    .spatial_input_aliases()[["country_areas"]]
  ) |>
    dplyr::filter(item_prod_code %in% mapped_items)
  crop_patterns <- .read_spatial_input(
    input_dir,
    "crop_patterns.parquet",
    .spatial_input_aliases()[["crop_patterns"]]
  ) |>
    dplyr::filter(item_prod_code %in% mapped_items)
  gridded_cropland <- .read_spatial_input(
    input_dir,
    "gridded_cropland.parquet",
    .spatial_input_aliases()[["gridded_cropland"]]
  )
  country_grid <- .load_country_grid(
    input_dir,
    config$country_grid,
    config$level,
    config$grid_vintage,
    config$granted_containers,
    config$double_claim
  )

  type_cropland <- NULL
  type_mapping <- NULL
  if (isTRUE(config$use_type_constraint)) {
    type_cropland <- .read_spatial_input(
      input_dir,
      "type_cropland.parquet",
      .spatial_input_aliases()[["type_cropland"]]
    )
    type_mapping <- cft_mapping
  }

  multicropping <- .read_spatial_input(
    input_dir,
    "multicropping.parquet",
    .spatial_input_aliases()[["multicropping"]],
    required = FALSE
  )
  if (!is.null(multicropping)) {
    cli::cli_alert_info(
      "multicropping: {nrow(multicropping)} rows loaded"
    )
  }

  list(
    country_areas = country_areas,
    crop_patterns = crop_patterns,
    gridded_cropland = gridded_cropland,
    country_grid = country_grid,
    type_cropland = type_cropland,
    type_mapping = type_mapping,
    multicropping = multicropping,
    cft_mapping = cft_mapping
  )
}

.read_packaged_cft_mapping <- function() {
  cft_mapping <- whep::cft_mapping
  .assert_unique_cft_mapping(cft_mapping)
  cft_mapping
}

.load_livestock_inputs <- function(input_dir, config = list()) {
  livestock_data <- .read_spatial_input(
    input_dir,
    "livestock_country_data.parquet",
    .spatial_input_aliases()[["livestock_country_data"]]
  )
  gridded_pasture <- .read_spatial_input(
    input_dir,
    "gridded_pasture.parquet",
    .spatial_input_aliases()[["gridded_pasture"]]
  )
  gridded_cropland <- .read_spatial_input(
    input_dir,
    "gridded_cropland.parquet",
    .spatial_input_aliases()[["gridded_cropland"]]
  )
  country_grid <- .load_country_grid(
    input_dir,
    config$country_grid,
    config$level,
    config$grid_vintage,
    config$granted_containers,
    config$double_claim
  )

  species_proxy <- .read_livestock_mapping()

  manure_pattern <- .read_spatial_input(
    input_dir,
    "manure_pattern.parquet",
    .spatial_input_aliases()[["manure_pattern"]],
    required = FALSE
  )

  # Only under the method that allocates on it. GLW3 is an env-var-gated
  # local raster set, not one of this directory's parquets, so `input_dir`
  # is not consulted -- `read_glw_density()` resolves `WHEP_GLW3_DIR` and
  # aborts naming its download script when unset. Loading it unconditionally
  # would make every default `"luh2"` run depend on a tree it never reads.
  glw_density <- NULL
  if (identical(config$livestock_proxy, "glw3")) {
    glw_density <- read_glw_density(
      variant = .check_glw_run_variant(config$livestock_glw_variant)
    )
  }

  list(
    livestock_data = livestock_data,
    gridded_pasture = gridded_pasture,
    gridded_cropland = gridded_cropland,
    country_grid = country_grid,
    species_proxy = species_proxy,
    manure_pattern = manure_pattern,
    glw_density = glw_density
  )
}

# Which cell-to-polity crosswalk the engines allocate into.
#
# `"polycell"` is the default: `build_polycell_support()`'s measured territory,
# resolved to one row per cell and `area_code` and carrying the polycell's
# share of the cell's LAND as `cell_area_frac`. It is the only one of the three
# whose share is a geodesic measurement rather than a subcell count, and the
# only one keyed on a polity identity before it is folded to a reporting code.
#
# `"centroid"` is the deployed `spatialize-country-grid` pin: one `area_code`
# per 0.5-degree cell, winner-take-all at a border, and NO share column at all.
# Since C8 that is refused rather than defaulted to 1 -- giving a whole border
# cell to one polity is the defect this epic exists to remove -- so this source
# now aborts in `.normalize_country_grid()` unless the parquet in hand happens
# to carry a share. It is kept selectable to reproduce a published run, not
# because it is usable.
#
# `"fraction"` is the fractional-coverage crosswalk `build_cell_polity()`
# reads, whose `polity_frac` is a share quantised to 1/36 of a cell. The
# deployed parquet used to be a DIFFERENT vintage of the
# `iso3c -> area_code` lookup, so substituting it deleted every reporting area
# whose code it did not carry -- 27.1 Mha of harvested area on Ethiopia and
# Sudan alone (whep#461). It was regenerated in whep#694 and now carries
# exactly the centroid grid's 178 codes; `build_cell_polity()` aborts on a
# copy that still holds a retired one, and
# `.warn_grid_missing_reporters()` still reports whatever a grid cannot
# represent before it reaches an output.
#
# The three are alternatives, never a fallback: a run asked for one crosswalk
# must fail rather than quietly allocate into another.
.load_country_grid <- function(
  input_dir,
  source = NULL,
  level = 0L,
  grid_vintage = "snapshot_2015",
  granted_containers = NULL,
  double_claim = "co_presence"
) {
  if (is.null(source)) {
    source <- "polycell"
  }
  source <- rlang::arg_match0(
    source,
    c("polycell", "centroid", "fraction"),
    arg_nm = "country_grid"
  )
  level <- .check_grid_level(level)
  grid_vintage <- .check_grid_vintage(grid_vintage)
  if (source == "polycell") {
    # The vintage is forwarded at level 0 only. A granted depth does not read
    # it, and passing it there would make every depth run report a key it
    # ignored; `.grid_vintage_method()` records what the grid actually is.
    if (level > 0L) {
      return(.load_allocation_layer(level, granted_containers, double_claim))
    }
    return(read_level_country_grid(
      level = level,
      grid_vintage = grid_vintage
    ))
  }
  .inform_static_crosswalk(source, grid_vintage)
  # Only the polycell support is keyed on a polity identity, so it is the only
  # crosswalk a containment depth can be resolved against. The centroid grid
  # holds one `area_code` per cell and the fractional one is built from the same
  # reporting vocabulary; asking either for a depth and quietly getting level 0
  # would put a constrained run on an unconstrained grid.
  if (level > 0L) {
    cli::cli_abort(c(
      "{.arg country_grid} {.val {source}} carries no containment depth.",
      x = "{.code level = {level}} was requested.",
      i = "Depth is resolved against the polycell support's
           {.field polity_code}; use {.code country_grid = \"polycell\"}."
    ))
  }
  if (source == "centroid") {
    return(.read_spatial_input(
      input_dir,
      "country_grid.parquet",
      .spatial_input_aliases()[["country_grid"]]
    ))
  }
  .read_fraction_country_grid(input_dir)
}

# The centroid grid and the fractional crosswalk carry no validity interval at
# all -- they are single-vintage rasterizations of one polity snapshot -- so
# `grid_vintage` has nothing to select there. Said out loud, and recorded as
# its own `method_grid_vintage` value, rather than letting a run report a
# vintage it did not read.
.inform_static_crosswalk <- function(source, grid_vintage) {
  recorded <- .static_crosswalk_vintage()
  cli::cli_inform(c(
    i = "{.arg country_grid} {.val {source}} carries no validity interval;
         {.arg grid_vintage} {.val {grid_vintage}} is not read.",
    i = "The run records {.field method_grid_vintage} = {.val {recorded}}."
  ))
  invisible(NULL)
}

# What a run allocated on, as one closed vocabulary: the two support vintages
# plus the static crosswalks, which are neither.
.static_crosswalk_vintage <- function() {
  "static_crosswalk"
}

.grid_vintage_method <- function(source, grid_vintage, level = 0L) {
  if (!identical(source %||% "polycell", "polycell")) {
    return(.static_crosswalk_vintage())
  }
  if (.check_grid_level(level) > 0L) {
    return("year_aware")
  }
  .check_grid_vintage(grid_vintage)
}

# The polycell support resolved to the spatialization's grain. It is read
# through the carbon path's own helper so that the two consumers of this table
# cannot end up on different reference years or different folds: the year, the
# `area_code` collapse and the land-share denominator are all decided once, in
# `.carbon_cell_support()`.
#
# `input_dir` is deliberately not consulted. The support is a pin, not one of
# the spatialization parquets, so there is no per-directory copy of it to mix
# in -- and `read_polycell_support()` already names the environment variable to
# point at a local build.
.read_polycell_country_grid <- function() {
  grid <- .carbon_cell_support()
  cli::cli_alert_info(
    "country_grid: polycell support, {nrow(grid)} compartment{?s} over \\
     {dplyr::n_distinct(paste(grid$lon, grid$lat))} cell{?s}"
  )
  grid
}

# The fractional crosswalk is whatever `build_cell_polity()` reads, so it is
# resolved the same way. An `input_dir` that does not hold it aborts rather than
# falling back to the pin or to `WHEP_POLITY_FRACTION_PATH`: a run asked for one
# directory's inputs must not silently mix in another's. With no `input_dir` it
# resolves like any other call -- the `spatialize-cell-polity-fraction` pin by
# default, the env var when set as an override (whep#694).
.read_fraction_country_grid <- function(input_dir) {
  path <- NULL
  if (!is.null(input_dir)) {
    path <- file.path(input_dir, "cell_polity_fraction.parquet")
    if (!file.exists(path)) {
      cli::cli_abort(c(
        "Missing required spatialization input in {.path {input_dir}}:",
        "x" = "{.file cell_polity_fraction.parquet}.",
        "i" = "It is only needed for
           {.code overrides = list(country_grid = \"fraction\")}."
      ))
    }
  }
  grid <- build_cell_polity(polity_fraction_path = path) |>
    dplyr::select(lon, lat, area_code, polity_frac)
  cli::cli_alert_info(
    "country_grid: fractional crosswalk, {nrow(grid)} compartment{?s} over \\
     {dplyr::n_distinct(paste(grid$lon, grid$lat))} cell{?s}"
  )
  grid
}

.spatial_input_aliases <- function() {
  c(
    country_areas = "spatialize-country-areas",
    crop_patterns = "spatialize-crop-patterns",
    gridded_cropland = "spatialize-gridded-cropland",
    country_grid = "spatialize-country-grid",
    type_cropland = "spatialize-type-cropland",
    multicropping = "spatialize-multicropping",
    livestock_country_data = "spatialize-livestock-country-data",
    gridded_pasture = "spatialize-gridded-pasture",
    manure_pattern = "spatialize-manure-pattern"
  )
}

.read_spatial_input <- function(input_dir, file_name, alias, required = TRUE) {
  if (!is.null(input_dir)) {
    path <- file.path(input_dir, file_name)
    if (!file.exists(path)) {
      if (isTRUE(required)) {
        cli::cli_abort(c(
          "Missing required spatialization input in {.path {input_dir}}:",
          "x" = "{.file {file_name}}."
        ))
      }
      return(NULL)
    }
    return(nanoparquet::read_parquet(path))
  }

  tryCatch(
    whep_read_file(alias),
    error = function(e) {
      if (!isTRUE(required)) {
        return(NULL)
      }
      cli::cli_abort(c(
        "Could not read pinned spatialization input {.val {alias}}.",
        i = "Provide a local {.arg input_dir} with {.file {file_name}} to use
          locally prepared inputs instead.",
        "Caused by" = conditionMessage(e)
      ))
    }
  )
}

.read_livestock_mapping <- function() {
  path <- system.file(
    "extdata",
    "livestock_mapping.csv",
    package = "whep"
  )
  if (!nzchar(path)) {
    return(NULL)
  }
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::distinct(species_group, spatial_proxy)
}

.write_landuse_outputs <- function(
  result_crops,
  cft_mapping,
  out_dir,
  config,
  cft_target = "whep"
) {
  paths <- list()
  crop_path <- file.path(out_dir, "gridded_landuse_crops.parquet")
  write_parquet_checked(.stamp_grid_vintage(result_crops, config), crop_path)
  paths$landuse_crops <- crop_path

  if (isTRUE(config$aggregate_to_cft)) {
    # Pick the aggregation column: cft_name (granular WHEP) or
    # cft_lpjml (LPJmL-compatible 12 crops + 'others').
    agg_col <- if (cft_target == "lpjml") "cft_lpjml" else "cft_name"
    if (!rlang::has_name(cft_mapping, agg_col)) {
      cli::cli_abort(
        "cft_mapping is missing the {.field {agg_col}} column."
      )
    }
    .assert_unique_cft_mapping(cft_mapping)
    group_cols <- unique(c(
      .compartment_id_cols(result_crops),
      "lon",
      "lat",
      "year",
      "cft_name"
    ))
    cft_result <- result_crops |>
      dplyr::inner_join(
        dplyr::select(
          cft_mapping,
          item_prod_code,
          cft_name = dplyr::all_of(agg_col)
        ),
        by = "item_prod_code"
      ) |>
      dplyr::summarise(
        rainfed_ha = sum(rainfed_ha, na.rm = TRUE),
        irrigated_ha = sum(irrigated_ha, na.rm = TRUE),
        .by = dplyr::all_of(group_cols)
      )
    cft_path <- file.path(out_dir, "gridded_landuse.parquet")
    write_parquet_checked(.stamp_grid_vintage(cft_result, config), cft_path)
    paths$landuse_cft <- cft_path
  }

  paths
}

.write_livestock_outputs <- function(gridded_livestock, out_dir, config) {
  path <- file.path(out_dir, "gridded_livestock_emissions.parquet")
  write_parquet_checked(.stamp_grid_vintage(gridded_livestock, config), path)
  list(livestock = path)
}

# WHICH GEOGRAPHY THE ROW WAS ALLOCATED INTO, written on every output this
# function produces. It is stamped here rather than inside the engines because
# only the run knows which crosswalk and vintage were resolved: a caller
# handing `build_gridded_landuse()` a grid directly has not said, and inventing
# a label for it would be worse than not carrying one.
#
# Constant within a run by construction -- one grid feeds both engines -- so it
# is a run-level provenance column, not a per-row measurement, and the CFT
# aggregation neither groups on it nor needs to.
.stamp_grid_vintage <- function(out, config) {
  dplyr::mutate(
    out,
    method_grid_vintage = .grid_vintage_method(
      config$country_grid,
      config$grid_vintage,
      config$level
    )
  )
}

.write_run_metadata <- function(
  out_dir,
  preset,
  years,
  components,
  cft_target,
  config,
  overrides,
  input_dir,
  admin = NULL
) {
  meta <- list(
    preset = preset,
    components = components,
    cft_target = cft_target,
    timestamp = format(
      Sys.time(),
      "%Y-%m-%dT%H:%M:%S%z",
      tz = "UTC"
    ),
    package_version = as.character(utils::packageVersion("whep")),
    method_grid_vintage = .grid_vintage_method(
      config$country_grid,
      config$grid_vintage,
      config$level
    ),
    input_source = if (is.null(input_dir)) "pins" else "directory",
    input_dir = input_dir,
    years = as.integer(years),
    config = config,
    overrides = overrides,
    # WHETHER THE RUN WAS ACTUALLY CONSTRAINED, which the config alone cannot
    # say: `level = 1L` is what was asked for, this is what happened. `"none"`
    # on every level-0 run, and on a depth run whose components exclude the
    # landuse engine -- the only path a depth takes with no crop constraint,
    # and it says so rather than leaving the key absent.
    admin_constraint = admin$summary %||% "none"
  )
  yaml::write_yaml(meta, file.path(out_dir, "run_metadata.yaml"))
}

# --- The granted-depth run --------------------------------------------------
#
# THE CALL GRAPH A DEPTH RUN TAKES, and why it exists. Until whep#1000 T40
# every one of `build_allocation_layer()`, `resolve_admin_shares()`,
# `allocate_level_crops()`, `reconcile_admin_allocation()` and `seam_gate()`
# had NO call site outside its own tests. `overrides$level` was validated and
# written into `run_metadata.yaml`, and then nothing read it: a `level = 1L`
# run allocated FAOSTAT national totals on the level-0 pattern and ignored
# every subnational row, while its metadata said it had a depth. What follows
# is the sequence that was missing, in order:
#
#   1. `.load_allocation_layer` reads the grid twice, level 0 for the
#      ungranted countries and the granted depth scoped to its containers,
#      and combines them with `build_allocation_layer`.
#   2. `.run_landuse_depth` calls `.admin_constraint`, which reads the pin
#      with `read_admin_shares`, scopes it in `.admin_scope_containers`,
#      resolves its units in `.admin_resolve_units` through
#      `resolve_admin_units`, and applies precedence with
#      `resolve_admin_shares`.
#   3. `allocate_level_crops` splits each national total across the units.
#   4. `.admin_diagnostics` measures the run with `unit_cropland_extent`,
#      `reconcile_admin_allocation` and `seam_gate`.
#   5. `.level_fold_output` writes the usual parquets at the asked-for grain
#      and `.write_admin_outputs` writes the audit trail beside them.
#
# Every step aborts naming what it is missing rather than continuing on the
# level-0 pattern, because continuing silently is the defect this section
# exists to remove.

# The layer the engines allocate into: the granted containers at their depth,
# everybody else at level 0, asserted to partition every cell.
#
# BOTH HALVES ARE READ YEAR-AWARE. The granted half has no choice -- a depth
# is resolved against containment edges the package validates per year -- and
# reading the base half at the 2015 snapshot instead would put two geographies
# in one layer and compare them in `build_allocation_layer()`'s own assertion
# (b). So `grid_vintage` is not read here, which is what
# `.grid_vintage_method()` has always recorded for a depth run and what every
# output row's `method_grid_vintage` says.
.load_allocation_layer <- function(level, granted_containers, double_claim) {
  granted <- tibble::tibble(
    area_code = .level_check_containers(
      granted_containers,
      "overrides$granted_containers"
    ),
    level = as.integer(level)
  )
  cli::cli_alert_info(
    "Allocation layer: {nrow(granted)} container{?s} granted level {level}
     ({.val {granted$area_code}}); every other country stays at level 0."
  )
  grid0 <- read_level_country_grid(
    level = 0L,
    grid_vintage = "year_aware"
  )
  grid_deep <- read_level_country_grid(
    level = level,
    containers = granted$area_code,
    double_claim = double_claim
  )
  layer <- build_allocation_layer(grid0, grid_deep, granted)
  .inform_layer_diagnostics(layer)
  layer
}

# The two attributes `build_allocation_layer()` rides on the layer are what
# says how much of the grid the depth actually covers. They are warned about
# there; here they are stated once more with their totals, because a run log
# is where an operator looks.
.inform_layer_diagnostics <- function(layer) {
  ragged <- attr(layer, "ragged_coverage")
  unclaimed <- attr(layer, "unclaimed_land")
  cli::cli_alert_info(
    "Layer: {nrow(layer)} compartment{?s}; {nrow(ragged)} ragged-coverage
     row{?s}; {nrow(unclaimed)} unclaimed cell-epoch{?s}."
  )
  invisible(NULL)
}

# The landuse step at a granted depth. Same inputs, same engine, but the
# national totals are split across administrative units first.
.run_landuse_depth <- function(
  lu_inputs,
  resolved_years,
  config,
  cft_target,
  out_dir
) {
  constraint <- .admin_constraint(config, lu_inputs$country_grid)
  allocated <- allocate_level_crops(
    country_areas = lu_inputs$country_areas,
    crop_patterns = lu_inputs$crop_patterns,
    gridded_cropland = lu_inputs$gridded_cropland,
    allocation_layer = lu_inputs$country_grid,
    admin_shares = constraint$shares,
    config = list(
      type_cropland = lu_inputs$type_cropland,
      type_mapping = lu_inputs$type_mapping,
      multicropping = lu_inputs$multicropping,
      years = resolved_years,
      max_iterations = config$max_iterations,
      expansion_threshold = config$expansion_threshold,
      area_key = config$area_key
    )
  )
  diagnostics <- .admin_diagnostics(
    allocated,
    constraint,
    lu_inputs,
    resolved_years
  )
  result_crops <- .level_fold_output(
    allocated$allocation,
    config$output_level
  )
  paths <- .write_landuse_outputs(
    result_crops,
    lu_inputs$cft_mapping,
    out_dir,
    config,
    cft_target = cft_target
  )
  list(
    years = resolved_years,
    paths = c(
      paths,
      .write_admin_outputs(out_dir, constraint, allocated, diagnostics)
    ),
    admin = .admin_run_record(constraint, allocated, diagnostics)
  )
}

# --- The admin constraint ---------------------------------------------------

# Read, scope, resolve to polities, resolve precedence. Four steps, each of
# which aborts on an empty result naming what it looked for: a depth run that
# reached the engine with no admin row would allocate on pattern-implied unit
# shares and never say so, which is indistinguishable from an unconstrained
# run in every output it writes.
.admin_constraint <- function(config, layer) {
  read <- .admin_read_shares()
  scoped <- .admin_scope_containers(read$shares, config$granted_containers)
  units <- .admin_resolve_units(scoped)
  resolved <- resolve_admin_shares(
    units$shares,
    constraint_exclude = config$constraint_exclude,
    not_shipped = read$not_shipped
  )
  .admin_check_resolved(resolved, config$granted_containers)
  .admin_check_layer_units(resolved$shares, layer)
  # THE CONTRACT'S COLUMN IS `treatment_year`; every consumer downstream reads
  # `treatment`. Renamed once, here, rather than in each of the three
  # consumers: `reconcile_admin_allocation()` silently emits an empty bridge
  # report without it, and `seam_gate()` treats every row as not observed, so
  # a run would report "no carried year" for a reason that is a column name.
  resolved$shares <- dplyr::mutate(
    resolved$shares,
    treatment = dplyr::coalesce(as.character(treatment_year), "observed")
  )
  cli::cli_alert_info(
    "Admin constraint: {nrow(resolved$shares)} winning row{?s} over
     {nrow(resolved$coverage)} container-item-year group{?s};
     {nrow(resolved$dropped)} dropped, {nrow(resolved$excluded)} held out,
     {nrow(resolved$seams)} seam{?s}."
  )
  c(resolved, list(unit_resolution = units$diagnostics))
}

.admin_read_shares <- function() {
  read <- read_admin_shares()
  if (nrow(read$shares) == 0L) {
    cli::cli_abort(
      c(
        "The admin constraint has no rows to read.",
        x = "The {.val admin-shares} pin is not registered in
             {.field whep_inputs}, or it holds no rows.",
        i = "A granted depth is constrained by administrative statistics;
             register the pin or run at {.code level = 0}."
      ),
      class = "whep_run_no_admin_shares"
    )
  }
  read
}

.admin_scope_containers <- function(shares, granted_containers) {
  rows <- dplyr::filter(shares, area_code %in% granted_containers)
  absent <- setdiff(granted_containers, unique(rows$area_code))
  if (length(absent) > 0L) {
    cli::cli_abort(
      c(
        "{length(absent)} granted container{?s} have no admin-share row.",
        x = "{.field area_code}{?s}: {.val {absent}}.",
        i = "The pin covers {.val {sort(unique(shares$area_code))}}.",
        i = "Withdraw the grant or ship the statistics; a granted container
             with no evidence is allocated on the gridded pattern alone."
      ),
      class = "whep_run_admin_container_absent"
    )
  }
  cli::cli_alert_info(
    "Admin shares: {nrow(rows)} row{?s} for
     {dplyr::n_distinct(rows$area_code)} granted container{?s}, from
     {nrow(shares)} in the pin."
  )
  rows
}

# The load-time resolution of source-native identifiers to polity codes. The
# pin stores what the source said, so this happens on every read rather than
# at pin-build time; see `R/admin_shares_polities.R`.
.admin_resolve_units <- function(shares) {
  resolved <- resolve_admin_units(
    dplyr::rename(shares, source_native_unit_id = "source_native_id"),
    .admin_code_systems_for(shares$source)
  )
  rows <- resolved$rows |>
    dplyr::rename(source_native_id = "source_native_unit_id") |>
    dplyr::select(-"alias_source")
  kept <- dplyr::filter(rows, !is.na(level_polity_code))
  if (nrow(kept) == 0L) {
    cli::cli_abort(
      c(
        "No admin-share row resolved to a polity.",
        x = "{nrow(rows)} row{?s} over
             {dplyr::n_distinct(rows$source)} source{?s} resolved to
             {.val {NA}}.",
        i = "The alias rows for these code systems are a whep-polities
             deliverable; see {.fn resolve_admin_units}.",
        i = "Unresolved identifiers include
             {.val {utils::head(sort(unique(rows$source_native_id)), 5L)}}."
      ),
      class = "whep_run_admin_unresolved"
    )
  }
  if (nrow(kept) < nrow(rows)) {
    cli::cli_warn(c(
      "!" = "{nrow(rows) - nrow(kept)} admin-share row{?s} resolved to no
             polity and are dropped from the constraint.",
      i = "They lower the coverage of the groups they belonged to; see the
           unit-resolution diagnostics in {.file run_metadata.yaml}."
    ))
  }
  list(shares = kept, diagnostics = resolved$diagnostics)
}

# WHICH CODE SYSTEM EACH SOURCE'S IDENTIFIERS BELONG TO. "21" is a NUTS code,
# an IBGE UF and a NASS FIPS code at once, so the slug carries the system and
# a row cannot be resolved without one. The five in-house families are read
# off `.admin_family_aliases()` rather than restated, so a family added to the
# registry cannot go missing here; the public products are named because each
# one is a separate publication with its own key space.
.admin_code_systems_for <- function(source) {
  families <- .admin_family_aliases()
  known <- c(
    stats::setNames(
      paste0("whep-lab-", sub("^admin-stats-", "", families)),
      families
    ),
    .admin_public_code_systems()
  )
  system <- unname(known[as.character(source)])
  if (anyNA(system)) {
    unknown <- sort(unique(as.character(source)[is.na(system)]))
    cli::cli_abort(
      c(
        "{length(unknown)} admin-share source{?s} {?names/name} no code
         system.",
        x = "{.val {unknown}}.",
        i = "Every source's identifiers belong to one code system; declare
             it beside the others in {.fn .admin_code_systems_for}.",
        i = "Known: {.val {sort(names(known))}}."
      ),
      class = "whep_run_admin_code_system"
    )
  }
  system
}

.admin_public_code_systems <- function() {
  c(
    USDA_NASS = "usda-nass-fips",
    IBGE_PAM = "ibge-uf",
    IBGE_PPM = "ibge-uf",
    Eurostat_apro_cpshr = "eurostat-nuts",
    Eurostat_apro_cpnhr_h = "eurostat-nuts",
    Eurostat_apro_mt_ls_r = "eurostat-nuts",
    Eurostat_ef_lsk_poultry = "eurostat-nuts",
    JRC_subnational_crops = "jrc-nuts"
  )
}

.admin_check_resolved <- function(resolved, granted_containers) {
  absent <- setdiff(granted_containers, unique(resolved$shares$area_code))
  if (length(absent) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{length(absent)} granted container{?s} lost every admin-share row in
       resolution.",
      x = "{.field area_code}{?s}: {.val {absent}}.",
      i = "The rows were dropped by indicator precedence, by source
           precedence, or by {.arg constraint_exclude}.",
      i = "Withdraw the grant or narrow the hold-out; a granted container
           with no surviving evidence is allocated on the gridded pattern."
    ),
    class = "whep_run_admin_container_absent"
  )
}

# THE JOIN THAT MUST NOT BE EMPTY. The shares are keyed on
# `level_polity_code` and so is the layer; if the two vocabularies do not
# meet, every group falls back to pattern-implied unit shares and the run
# looks exactly like an unconstrained one. `allocate_level_crops()` warns
# about unmatched shares, but a run whose constraint matched NOTHING is not a
# warning, it is the defect this whole section exists to remove.
.admin_check_layer_units <- function(shares, layer) {
  in_layer <- intersect(
    unique(shares$level_polity_code),
    unique(layer$level_polity_code)
  )
  if (length(in_layer) > 0L) {
    cli::cli_alert_info(
      "Constraint meets the layer on {length(in_layer)} unit{?s}."
    )
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "No resolved admin unit appears in the allocation layer.",
      x = "{dplyr::n_distinct(shares$level_polity_code)} share unit{?s} and
           {dplyr::n_distinct(layer$level_polity_code)} layer unit{?s} share
           no {.field level_polity_code}.",
      i = "Shares name
           {.val {utils::head(sort(unique(shares$level_polity_code)), 3L)}};
           the layer names
           {.val {utils::head(sort(unique(layer$level_polity_code)), 3L)}}.",
      i = "The polycell support and the alias table must resolve to the same
           polity vocabulary."
    ),
    class = "whep_run_admin_layer_mismatch"
  )
}

# --- Reconciliation and the seam gate ---------------------------------------

.admin_diagnostics <- function(allocated, constraint, lu_inputs, years) {
  # Scoped to the run's own years. The gridded cropland spans the whole
  # historical range whatever a run asks for, and an extent table describing
  # years the run never allocated is not this run's diagnostic.
  cropland <- unit_cropland_extent(
    lu_inputs$country_grid,
    dplyr::filter(lu_inputs$gridded_cropland, year %in% as.integer(years))
  )
  # `mc_national` is deliberately not supplied: the run's multi-cropping layer
  # is a PER-CELL suitability factor (`mc_rainfed`, `mc_irrigated`), not the
  # national factor this diagnostic flags an intensity against. Inventing one
  # from it would publish a ceiling nothing measured, so the flag stays `NA`
  # and says so.
  reconciliation <- reconcile_admin_allocation(
    allocated,
    admin_shares = constraint$shares,
    intensity = list(unit_cropland = cropland)
  )
  gate <- .admin_seam_gate(constraint)
  list(
    unit_cropland = cropland,
    reconciliation = reconciliation,
    gate = gate$gate,
    method_admin_share = gate$method_admin_share
  )
}

# The seam gate judges the shares the run was constrained by, at the years the
# resolver's own seam list marks. The gate reads a `share` and a `treatment`;
# the contract's rows carry `treatment_year` and, for a value-shipping family,
# no share at all. Both are supplied here, and NEITHER is invented:
# `treatment` is `treatment_year` renamed, and a missing share is the source's
# own value over the group's total -- the same `value_share` the gate itself
# forms in `.sg_anchor_value_share()` to check a reported share against.
# Which basis each row used is recorded in `run_metadata.yaml`.
.admin_seam_gate <- function(constraint) {
  shares <- .admin_gate_shares(constraint$shares)
  .admin_check_gate_key(shares)
  .warn_gate_identity_vacuous(shares)
  list(
    gate = seam_gate(shares, constraint$seams),
    method_admin_share = .admin_share_basis_counts(shares)
  )
}

# WHAT TIER A CANNOT SEE ON A VALUE-SHIPPING FAMILY, said out loud rather than
# left in a passing verdict. Tier A checks a reported share against the same
# share re-derived from the source's own values; where the family ships no
# share, the one supplied above IS that re-derivation, so the check compares a
# quantity with itself and passes by construction. Tier B is unaffected -- it
# reads the year-on-year movement of the shares, which is a real series
# whatever produced it -- and so is every allocation the run makes.
.warn_gate_identity_vacuous <- function(shares) {
  derived <- shares$share_basis == "value_normalised"
  if (!any(derived)) {
    return(invisible(NULL))
  }
  series <- dplyr::n_distinct(shares$item_prod_code[derived])
  cli::cli_warn(c(
    "!" = "{sum(derived)} share{?s} over {series} item{?s} were derived from
           the source's own values, so tier A's share-against-value identity
           holds by construction there and proves nothing.",
    i = "Tier B, which reads the year-on-year movement, is unaffected.",
    i = "Recorded as {.field method_admin_share = \"value_normalised\"} in
         {.file run_metadata.yaml}."
  ))
}

.admin_gate_shares <- function(shares) {
  dplyr::mutate(
    shares,
    share_basis = .admin_share_basis(share, value),
    share = dplyr::if_else(
      .data$share_basis == "value_normalised",
      value / sum(value, na.rm = TRUE),
      share
    ),
    .by = c("area_code", "level", "item_prod_code", "year")
  )
}

# One basis per group, never per row: mixing a reported share with a
# value-derived one inside a group would give a "share sum" adding two
# different denominators.
.admin_share_basis <- function(share, value) {
  if (length(share) > 0L && !anyNA(share)) {
    return(rep("reported", length(share)))
  }
  usable <- length(value) > 0L &&
    !anyNA(value) &&
    sum(value, na.rm = TRUE) > 0
  rep(if (usable) "value_normalised" else "unavailable", length(share))
}

.admin_share_basis_counts <- function(shares) {
  counts <- table(shares$share_basis)
  as.list(stats::setNames(as.integer(counts), names(counts)))
}

.admin_check_gate_key <- function(shares) {
  key <- c("area_code", "level", "item_prod_code", "level_polity_code", "year")
  dup <- sum(duplicated(shares[key]))
  if (dup == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "The resolved shares repeat {dup} unit-year row{?s}.",
    x = "Key: {.field {key}}.",
    i = "One resolved indicator binds per year; a union of indicators is not
         a gateable series."
  ))
}

# --- What a depth run writes ------------------------------------------------

# The reconciliation and the gate are the evidence that the constraint bound,
# so they are written beside the parquets rather than left inside the returned
# object: a run nobody can audit afterwards has not been verified.
.admin_run_files <- function() {
  list(
    admin_targets = "admin_targets.csv",
    admin_group_coverage = "admin_group_coverage.csv",
    admin_conservation = "admin_conservation.csv",
    admin_breach = "admin_breach.csv",
    admin_reconciliation = "admin_reconciliation.csv",
    admin_reconciliation_units = "admin_reconciliation_units.csv",
    admin_unit_cropland = "admin_unit_cropland.csv",
    admin_seams = "admin_seams.csv",
    admin_seam_gate_a = "admin_seam_gate_a.csv",
    admin_seam_gate_b = "admin_seam_gate_b.csv",
    admin_seam_gate_c = "admin_seam_gate_c.csv"
  )
}

.write_admin_outputs <- function(out_dir, constraint, allocated, diagnostics) {
  files <- .admin_run_files()
  tables <- list(
    admin_targets = allocated$targets,
    admin_group_coverage = allocated$coverage,
    admin_conservation = allocated$conservation,
    admin_breach = allocated$breach,
    admin_reconciliation = diagnostics$reconciliation$groups,
    admin_reconciliation_units = diagnostics$reconciliation$units,
    admin_unit_cropland = diagnostics$unit_cropland,
    admin_seams = constraint$seams,
    admin_seam_gate_a = diagnostics$gate$tier_a,
    admin_seam_gate_b = diagnostics$gate$tier_b,
    admin_seam_gate_c = diagnostics$gate$tier_c
  )
  paths <- purrr::imap(
    tables,
    \(x, nm) {
      path <- file.path(out_dir, files[[nm]])
      data.table::fwrite(x, path)
      path
    }
  )
  cli::cli_alert_success(
    "Wrote {length(paths)} admin diagnostic{?s} to {.path {out_dir}}."
  )
  paths
}

# WHAT THE RUN SAYS ABOUT ITSELF. `run_metadata.yaml` already carried the
# resolved config; a depth run must also say whether the constraint actually
# bound, because the config alone cannot -- it says what was asked for.
.admin_run_record <- function(constraint, allocated, diagnostics) {
  list(
    coverage = constraint$coverage,
    summary = list(
      n_shares_resolved = nrow(constraint$shares),
      n_shares_dropped = nrow(constraint$dropped),
      n_shares_excluded = nrow(constraint$excluded),
      n_seams = nrow(constraint$seams),
      n_units_constrained = dplyr::n_distinct(
        constraint$shares$level_polity_code
      ),
      method_admin_share = diagnostics$method_admin_share,
      method_crop_alloc = .admin_method_counts(allocated$targets),
      seam_gate_verdict = as.list(diagnostics$gate$verdict),
      n_groups_beyond_tolerance = sum(
        diagnostics$reconciliation$groups$beyond_tolerance,
        na.rm = TRUE
      )
    )
  )
}

.admin_method_counts <- function(targets) {
  if (is.null(targets) || nrow(targets) == 0L) {
    return(list())
  }
  counts <- table(targets$method_crop_alloc)
  as.list(stats::setNames(as.integer(counts), names(counts)))
}
