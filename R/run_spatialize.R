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
#'   Recognised entries:
#'   - `use_type_constraint` (logical): enable/disable LUH2
#'     type-aware allocation.
#'   - `aggregate_to_cft` (logical, default `TRUE`): write a
#'     CFT-aggregated parquet alongside the crop-level output.
#'   - `max_iterations`, `expansion_threshold`: forwarded to the
#'     landuse engine.
#' @param cft_target One of `"whep"` (default for `preset = "whep"`)
#'   or `"lpjml"` (default for `preset = "lpjml"`). Selects which
#'   column of `cft_mapping.csv` to use for CFT-level aggregation:
#'   `cft_name` (granular 33-class WHEP taxonomy) or `cft_lpjml`
#'   (12 LPJmL crop CFTs + single `others` bucket, matching LPJmL
#'   v6 band layout).
#' @param input_dir Directory holding the prepared input parquets.
#'   If `NULL`, defaults to `<l_files_dir>/whep/inputs`.
#' @param out_dir Output directory. If `NULL`, defaults to
#'   `<l_files_dir>/whep/spatialize/<preset>` (suffixed with
#'   `_custom` when `overrides` is non-empty). Created if missing.
#' @param l_files_dir Optional path to the `L_files` directory.
#'   If `NULL`, falls back to the `WHEP_L_FILES_DIR` environment
#'   variable.
#'
#' @return Invisibly, a named list with `preset`, resolved `config`,
#'   `years`, `out_dir`, and `output_paths`.
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
#' }
#'
#' @section Outputs written to `out_dir`:
#' \itemize{
#'   \item `gridded_landuse_crops.parquet` — crop-level output.
#'   \item `gridded_landuse.parquet` — CFT-aggregated output
#'     (when `aggregate_to_cft = TRUE`).
#'   \item `gridded_livestock_emissions.parquet` — gridded
#'     livestock stocks and emissions (when livestock component
#'     selected).
#'   \item `run_metadata.yaml` — resolved preset, components,
#'     flags, years, timestamp, and package version.
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
#'   ~lon,  ~lat, ~area_code,
#'    0.25, 50.25,         1L,
#'    0.75, 50.25,         1L
#' )
#' build_gridded_landuse(
#'   country_areas, crop_patterns, gridded_cropland, country_grid,
#'   years = 2000L
#' )
run_spatialize <- function(
  preset = c("lpjml", "whep"),
  years = NULL,
  components = c("landuse", "livestock"),
  overrides = list(),
  cft_target = NULL,
  input_dir = NULL,
  out_dir = NULL,
  l_files_dir = NULL
) {
  preset <- match.arg(preset)
  components <- .validate_components(components)
  cft_target <- .resolve_cft_target(cft_target, preset)
  .validate_overrides(overrides)
  config <- .resolve_spatialize_config(preset, overrides)

  l_files_dir <- .get_l_files_dir(l_files_dir)
  if (is.null(input_dir)) {
    input_dir <- file.path(l_files_dir, "whep", "inputs")
  }
  if (is.null(out_dir)) {
    out_dir <- .default_spatialize_out_dir(
      l_files_dir,
      preset,
      overrides
    )
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  output_paths <- list()
  resolved_years <- if (!is.null(years)) {
    sort(unique(as.integer(years)))
  }

  if ("landuse" %in% components) {
    lu_inputs <- .load_landuse_inputs(input_dir, config)
    if (is.null(resolved_years)) {
      resolved_years <- .resolve_years(
        years,
        preset,
        lu_inputs$country_areas
      )
    }
    cli::cli_h2("Running spatialize: landuse")
    cli::cli_alert_info(
      "Preset: {.val {preset}} | years: {length(resolved_years)} \\
      ({min(resolved_years)}-{max(resolved_years)}) | \\
      type-aware: {.val {config$use_type_constraint}}"
    )
    result_crops <- build_gridded_landuse(
      country_areas = lu_inputs$country_areas,
      crop_patterns = lu_inputs$crop_patterns,
      gridded_cropland = lu_inputs$gridded_cropland,
      country_grid = lu_inputs$country_grid,
      cft_mapping = NULL,
      type_cropland = lu_inputs$type_cropland,
      type_mapping = lu_inputs$type_mapping,
      years = resolved_years,
      max_iterations = config$max_iterations,
      expansion_threshold = config$expansion_threshold
    )
    output_paths <- c(
      output_paths,
      .write_landuse_outputs(
        result_crops,
        lu_inputs$cft_mapping,
        out_dir,
        config,
        cft_target = cft_target
      )
    )
  }

  if ("livestock" %in% components) {
    ls_inputs <- .load_livestock_inputs(input_dir)
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
      years = resolved_years
    )
    output_paths <- c(
      output_paths,
      .write_livestock_outputs(gridded_livestock, out_dir)
    )
  }

  .write_run_metadata(
    out_dir,
    preset,
    resolved_years,
    components,
    cft_target,
    config,
    overrides,
    input_dir
  )

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
    output_paths = output_paths
  ))
}

# --- Private helpers --------------------------------------------------------

.spatialize_presets <- function() {
  list(
    lpjml = list(
      use_type_constraint = FALSE,
      aggregate_to_cft = TRUE,
      max_iterations = 1000L,
      expansion_threshold = 100L
    ),
    whep = list(
      use_type_constraint = TRUE,
      aggregate_to_cft = TRUE,
      max_iterations = 1000L,
      expansion_threshold = 100L
    )
  )
}

.known_override_keys <- function() {
  c(
    "use_type_constraint",
    "aggregate_to_cft",
    "max_iterations",
    "expansion_threshold"
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

.load_landuse_inputs <- function(input_dir, config) {
  required <- c(
    "country_areas.parquet",
    "crop_patterns.parquet",
    "gridded_cropland.parquet",
    "country_grid.parquet"
  )
  missing_files <- required[
    !file.exists(file.path(input_dir, required))
  ]
  if (length(missing_files) > 0L) {
    cli::cli_abort(c(
      "Missing required input{?s} in {.path {input_dir}}:",
      "x" = "{.file {missing_files}}."
    ))
  }

  cft_mapping <- .read_packaged_cft_mapping()
  mapped_items <- cft_mapping$item_prod_code

  country_areas <- nanoparquet::read_parquet(
    file.path(input_dir, "country_areas.parquet")
  ) |>
    dplyr::filter(item_prod_code %in% mapped_items)
  crop_patterns <- nanoparquet::read_parquet(
    file.path(input_dir, "crop_patterns.parquet")
  ) |>
    dplyr::filter(item_prod_code %in% mapped_items)
  gridded_cropland <- nanoparquet::read_parquet(
    file.path(input_dir, "gridded_cropland.parquet")
  )
  country_grid <- nanoparquet::read_parquet(
    file.path(input_dir, "country_grid.parquet")
  )

  type_cropland <- NULL
  type_mapping <- NULL
  if (isTRUE(config$use_type_constraint)) {
    type_cl_path <- file.path(input_dir, "type_cropland.parquet")
    if (!file.exists(type_cl_path)) {
      cli::cli_abort(c(
        "{.file type_cropland.parquet} not found in \\
        {.path {input_dir}}.",
        "i" = "Required when {.code use_type_constraint = TRUE}."
      ))
    }
    type_cropland <- nanoparquet::read_parquet(type_cl_path)
    type_mapping <- cft_mapping
  }

  list(
    country_areas = country_areas,
    crop_patterns = crop_patterns,
    gridded_cropland = gridded_cropland,
    country_grid = country_grid,
    type_cropland = type_cropland,
    type_mapping = type_mapping,
    cft_mapping = cft_mapping
  )
}

.read_packaged_cft_mapping <- function() {
  path <- system.file("extdata", "cft_mapping.csv", package = "whep")
  if (!nzchar(path)) {
    cli::cli_abort(
      "{.file cft_mapping.csv} not found in installed package."
    )
  }
  readr::read_csv(path, show_col_types = FALSE)
}

.load_livestock_inputs <- function(input_dir) {
  required <- c(
    "livestock_country_data.parquet",
    "gridded_pasture.parquet",
    "gridded_cropland.parquet",
    "country_grid.parquet"
  )
  missing_files <- required[
    !file.exists(file.path(input_dir, required))
  ]
  if (length(missing_files) > 0L) {
    cli::cli_abort(c(
      "Missing required livestock input{?s} in \\
      {.path {input_dir}}:",
      "x" = "{.file {missing_files}}."
    ))
  }

  livestock_data <- nanoparquet::read_parquet(
    file.path(input_dir, "livestock_country_data.parquet")
  )
  gridded_pasture <- nanoparquet::read_parquet(
    file.path(input_dir, "gridded_pasture.parquet")
  )
  gridded_cropland <- nanoparquet::read_parquet(
    file.path(input_dir, "gridded_cropland.parquet")
  )
  country_grid <- nanoparquet::read_parquet(
    file.path(input_dir, "country_grid.parquet")
  )

  species_proxy <- .read_livestock_mapping()

  manure_pattern <- NULL
  manure_path <- file.path(input_dir, "manure_pattern.parquet")
  if (file.exists(manure_path)) {
    manure_pattern <- nanoparquet::read_parquet(manure_path)
  }

  list(
    livestock_data = livestock_data,
    gridded_pasture = gridded_pasture,
    gridded_cropland = gridded_cropland,
    country_grid = country_grid,
    species_proxy = species_proxy,
    manure_pattern = manure_pattern
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
  nanoparquet::write_parquet(result_crops, crop_path)
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
        .by = c(lon, lat, year, cft_name)
      )
    cft_path <- file.path(out_dir, "gridded_landuse.parquet")
    nanoparquet::write_parquet(cft_result, cft_path)
    paths$landuse_cft <- cft_path
  }

  paths
}

.write_livestock_outputs <- function(gridded_livestock, out_dir) {
  path <- file.path(out_dir, "gridded_livestock_emissions.parquet")
  nanoparquet::write_parquet(gridded_livestock, path)
  list(livestock = path)
}

.write_run_metadata <- function(
  out_dir,
  preset,
  years,
  components,
  cft_target,
  config,
  overrides,
  input_dir
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
    input_dir = input_dir,
    years = as.integer(years),
    config = config,
    overrides = overrides
  )
  yaml::write_yaml(meta, file.path(out_dir, "run_metadata.yaml"))
}
