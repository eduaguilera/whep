#' Build native grassland evapotranspiration from an LPJmL run.
#'
#' @description
#' Preserve green and blue water consumption from the named rainfed and
#' irrigated grassland stands. The native stand-depth outputs
#' `cft_consump_water_g.nc` and `cft_consump_water_b.nc` are weighted by
#' `cftfrac.nc` and WHEP's spherical 0.5-degree cell areas. One millimetre
#' over one hectare is ten cubic metres. Depth units alone do not establish
#' the spatial basis: a supplied run configuration must declare
#' `grid_scaled = false`. Its SHA-256 fingerprint is retained in the output.
#'
#' This is grassland-domain ET before attribution to consumed feed. It is
#' not maximum grazing yield, animal drinking water or a livestock footprint.
#' Use [calculate_grazing_water()] to select an attribution basis explicitly.
#' Native blue ET remains blue; rainfed stand names do not reclassify water.
#' No country codes or present-day territorial assignments are inferred.
#'
#' All three cubes must share their recorded model version, commit and run
#' history. Their native annual axes must be consecutive on the noleap
#' calendar, measured in days since January 1. Both grassland bands and every
#' requested year must be supplied. Positional reads require native variable
#' dimension order `lon`, `lat`, `pft`, `time`, with degree coordinates.
#' Zero-area stands retain their native missing depth and have zero volume;
#' missing area fractions or missing depth on a positive-area stand abort.
#' Ocean/ice cells with no supplied fraction in either grass band and no
#' supplied ET are outside the returned domain. Injected cubes define their
#' own cell population; agreement between cubes does not independently prove
#' coverage of any target territory. Provenance labels and a supplied config
#' identify the inputs; they cannot prove that a config was never edited.
#'
#' @param run_dir LPJmL output directory. If `NULL`, use
#'   `WHEP_LPJML_RUN_DIR`. Not read when complete `data` is supplied.
#' @param years Integer calendar years. Required for run reads, which load
#'   only these years and the two named grass bands. When `data` is supplied,
#'   `NULL` takes the years from its stand-fraction cube.
#' @param config Path to the run's resolved JSON configuration. Required for
#'   run reads because the NetCDF headers do not record `grid_scaled`.
#'   Only explicit `false` is supported; cell-scaled water aborts.
#' @param data Complete named list of `green`, `blue` and `stand_frac` tibbles
#'   keyed by `lon`, `lat`, `year`, `band_name`, with numeric `value`.
#'   `vintage` has one row per input with `input`, `units`, `lpjml_version`,
#'   `lpjml_git_hash`, `lpjml_run`, `spatial_basis` and `source_basis`.
#'   Water uses `mm/yr` and spatial basis `"stand"`; fractions are
#'   dimensionless (`""` or `"1"`) with basis `"cell_fraction"`.
#'   Nonempty source-basis labels identify the evidence for those assertions.
#'   No missing list member is silently fetched from another input source.
#' @param example Return a small actual-readback fixture without reading a
#'   run. Defaults to `FALSE`.
#' @return A tibble per cell, year, named grass stand and water component,
#'   with `stand_frac`, `cell_area_ha`, `grass_area_ha`, native `water_mm`,
#'   extensive `grass_water_m3`, input-status and source/basis/run metadata.
#'   Area and stand-fraction columns repeat across water components; count
#'   those areas once. `water_scenario` identifies the supplied run, not a
#'   hypothetical maximum-yield reference experiment.
#' @export
#' @examples
#' build_grazing_water_lpjml(example = TRUE)
build_grazing_water_lpjml <- function(
  run_dir = NULL,
  years = NULL,
  config = NULL,
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_grazing_water_lpjml())
  }
  if (is.null(data)) {
    .gwlp_years(years, years)
    data <- .gwlp_read(run_dir, years, config)
  }
  .gwlp_require(data, "stand_frac")
  .gwlp_require(data$stand_frac, "year")
  years <- years %||% sort(unique(data$stand_frac$year))
  .gwlp_assemble(data, years)
}

.gwlp_read <- function(run_dir, years, config) {
  basis <- .gwlp_config(config)
  run_dir <- .resolve_run_dir(run_dir)
  vars <- c(
    green = "cft_consump_water_g",
    blue = "cft_consump_water_b",
    stand_frac = "stand_frac"
  )
  parts <- purrr::map(vars, .gwlp_read_one, run_dir, years, basis)
  if (!all(purrr::map_lgl(parts, .gwlp_same_run, parts[[1L]]$attrs))) {
    .gwlp_abort(
      "Native cubes have different source, commit or run history.",
      "vintage"
    )
  }
  cubes <- purrr::map(parts, "data")
  purrr::walk(cubes[c("green", "blue")], .gwlp_coverage, cubes$stand_frac)
  land <- cubes$stand_frac |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::distinct(.data$lon, .data$lat, .data$year)
  cubes <- purrr::map(cubes, .gwlp_land, land)
  cubes$vintage <- dplyr::bind_rows(purrr::map(parts, "vintage"), .id = "input")
  .gwlp_vintage(cubes$vintage)
  cubes
}

.gwlp_config <- function(config) {
  if (
    !is.character(config) ||
      length(config) != 1L ||
      is.na(config) ||
      !file.exists(config)
  ) {
    .gwlp_abort(
      "Supply the run's resolved JSON config to establish its area basis."
    )
  }
  data <- jsonlite::fromJSON(config)
  if (!identical(data$grid_scaled, FALSE)) {
    .gwlp_abort(
      "Native stand-depth reading requires explicit grid_scaled = false."
    )
  }
  paste0("grid_scaled=false; config SHA-256 ", unname(tools::sha256sum(config)))
}

.gwlp_read_one <- function(var, run_dir, years, basis) {
  spec <- dplyr::filter(.hydro_var_map(), .data$var == .env$var)
  path <- file.path(run_dir, spec$file)
  rlang::check_installed("ncdf4")
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  .gwlp_check_axis(nc)
  variable <- .hydro_resolve_var(nc, spec$netcdf_var, path)
  .gwlp_check_dimensions(nc, variable)
  first <- .lpjml_resolve_first_year(nc, NULL, basename(path))
  .hydro_check_coverage(nc, first, years, path, steps_per_year = 1L)
  attrs <- purrr::map(
    c(source = "source", GIT_hash = "GIT_hash", history = "history"),
    .lid_attr,
    attrs = ncdf4::ncatt_get(nc, 0)
  )
  labels <- unlist(attrs, use.names = FALSE)
  if (anyNA(labels) || any(!nzchar(stringr::str_trim(labels)))) {
    .gwlp_abort(
      "Each native cube needs source, commit and run-history metadata.",
      "vintage"
    )
  }
  raw <- .read_lpjml_bands(path, variable, .gwlp_bands(), years, first) |>
    tibble::as_tibble() |>
    dplyr::rename(band_name = "band")
  list(
    data = raw,
    attrs = attrs,
    vintage = .gwlp_file_vintage(
      attrs,
      nc$var[[variable]]$units,
      list(var = var, run_dir = run_dir, basis = basis)
    )
  )
}

.gwlp_check_dimensions <- function(nc, variable) {
  dims <- vapply(nc$var[[variable]]$dim, `[[`, character(1), "name")
  if (
    !identical(dims, c("lon", "lat", "pft", "time")) ||
      !identical(nc$dim$lon$units, "degrees_east") ||
      !identical(nc$dim$lat$units, "degrees_north")
  ) {
    .gwlp_abort(
      "Native ET needs lon/lat/pft/time dimensions and degree coordinates."
    )
  }
}

.gwlp_check_axis <- function(nc) {
  time <- nc$dim$time
  calendar <- ncdf4::ncatt_get(nc, "time", "calendar")$value
  dated <- stringr::str_detect(
    time$units %||% "",
    "^days since [0-9]{3,4}-0?1-0?1(?: 0:0:0)?$"
  )
  values <- time$vals
  if (
    !isTRUE(dated) ||
      !calendar %in% c("noleap", "365_day") ||
      !length(values) ||
      any(!is.finite(values)) ||
      any(diff(values) <= 0) ||
      any(diff(floor(values / 365)) != 1)
  ) {
    .gwlp_abort(
      "Native ET requires a consecutive annual noleap days-since axis.",
      "coverage"
    )
  }
}

.gwlp_same_run <- function(part, attrs) {
  identical(part$attrs, attrs)
}

.gwlp_file_vintage <- function(attrs, units, context) {
  tibble::tibble(
    units = units,
    lpjml_version = attrs$source,
    lpjml_git_hash = attrs$GIT_hash,
    lpjml_run = .lid_run_label(context$run_dir, attrs$history),
    spatial_basis = if (context$var == "stand_frac") {
      "cell_fraction"
    } else {
      "stand"
    },
    source_basis = context$basis
  )
}

.gwlp_land <- function(data, land) {
  outside <- dplyr::anti_join(data, land, by = c("lon", "lat", "year"))
  if (any(!is.na(outside$value))) {
    .gwlp_abort(
      "Native values exist outside the supplied stand-fraction domain.",
      "coverage"
    )
  }
  dplyr::semi_join(data, land, by = c("lon", "lat", "year"))
}

# Native grass-stand ET, before any biomass or economic attribution. Keys
# use the file's band names, never positional band indices or country names.
.gwlp_assemble <- function(data, years) {
  inputs <- c("green", "blue", "stand_frac")
  .gwlp_require(data, c(inputs, "vintage"))
  vintage <- .gwlp_vintage(data$vintage)
  cubes <- purrr::map(data[inputs], .gwlp_cube, years = years)
  purrr::walk(cubes[c("green", "blue")], .gwlp_coverage, cubes$stand_frac)
  stands <- dplyr::rename(cubes$stand_frac, stand_frac = "value")
  .gwlp_fractions(stands)
  dplyr::bind_rows(cubes[c("green", "blue")], .id = "component") |>
    dplyr::rename(water_mm = "value") |>
    dplyr::left_join(stands, by = .gwlp_keys(), relationship = "many-to-one") |>
    .gwlp_volumes(vintage) |>
    dplyr::arrange(
      .data$lon,
      .data$lat,
      .data$year,
      .data$band_name,
      .data$component
    )
}

.gwlp_keys <- function() {
  c("lon", "lat", "year", "band_name")
}

.gwlp_bands <- function() {
  c("rainfed grassland", "irrigated grassland")
}

.gwlp_cube <- function(data, years) {
  keys <- .gwlp_keys()
  .gwlp_require(data, c(keys, "value"))
  data <- data |>
    dplyr::select(dplyr::all_of(c(keys, "value"))) |>
    dplyr::filter(.data$band_name %in% .gwlp_bands())
  if (!nrow(data)) {
    .gwlp_abort("Each input must supply both grassland bands.", "coverage")
  }
  numeric_cols <- c("lon", "lat", "year", "value")
  if (!all(vapply(data[numeric_cols], is.numeric, logical(1)))) {
    .gwlp_abort("Coordinates, years and cube values must be numeric.")
  }
  .gwlp_grid(data)
  .gwlp_years(data$year, years)
  data <- dplyr::filter(data, .data$year %in% years)
  if (anyDuplicated(data[keys])) {
    .gwlp_abort("Grassland cube keys must be unique.")
  }
  counts <- dplyr::count(data, .data$lon, .data$lat, .data$year)
  if (any(counts$n != length(.gwlp_bands()))) {
    .gwlp_abort("Each cell-year needs both grassland bands.", "coverage")
  }
  coverage <- dplyr::count(counts, .data$lon, .data$lat)
  if (any(coverage$n != length(unique(years)))) {
    .gwlp_abort(
      "Each supplied cell must cover every requested year.",
      "coverage"
    )
  }
  data
}

.gwlp_grid <- function(data) {
  coords <- c(data$lon, data$lat)
  if (
    any(!is.finite(coords)) ||
      any(abs(data$lon) > 179.75 | abs(data$lat) > 89.75) ||
      any((coords - 0.25) %% 0.5 != 0)
  ) {
    .gwlp_abort("Input coordinates must be centres of WHEP's 0.5-degree grid.")
  }
}

.gwlp_years <- function(available, requested) {
  if (
    !is.numeric(requested) ||
      !length(requested) ||
      any(!is.finite(requested) | requested != trunc(requested)) ||
      any(!is.finite(available) | available != trunc(available))
  ) {
    .gwlp_abort("Supply finite integer calendar years.")
  }
  if (length(setdiff(requested, available))) {
    .gwlp_abort("Each input must cover every requested year.", "coverage")
  }
}

.gwlp_coverage <- function(water, stands) {
  keys <- .gwlp_keys()
  if (
    nrow(dplyr::anti_join(stands, water, by = keys)) ||
      nrow(dplyr::anti_join(water, stands, by = keys))
  ) {
    .gwlp_abort(
      "Water and stand-fraction cell-year-band coverage differs.",
      "coverage"
    )
  }
}

.gwlp_fractions <- function(data) {
  if (
    any(!is.finite(data$stand_frac) | data$stand_frac < 0 | data$stand_frac > 1)
  ) {
    .gwlp_abort("Each grass stand fraction must be supplied and within [0, 1].")
  }
  totals <- dplyr::summarise(
    data,
    total = sum(.data$stand_frac),
    .by = c("lon", "lat", "year")
  )
  if (any(totals$total > 1)) {
    .gwlp_abort("Combined grass stands cannot exceed their cell's area.")
  }
}

.gwlp_vintage <- function(data) {
  fields <- c("lpjml_version", "lpjml_git_hash", "lpjml_run")
  .gwlp_require(data, c("input", "units", fields), "vintage")
  if (
    nrow(data) != 3L ||
      !setequal(data$input, c("green", "blue", "stand_frac"))
  ) {
    .gwlp_abort(
      "Supply one provenance record for each of the three cubes.",
      "vintage"
    )
  }
  labels <- unlist(data[fields], use.names = FALSE)
  if (
    any(is.na(labels) | !nzchar(stringr::str_trim(labels))) ||
      nrow(dplyr::distinct(data, dplyr::across(dplyr::all_of(fields)))) != 1L
  ) {
    .gwlp_abort(
      "All cubes need the same supplied version, commit and run.",
      "vintage"
    )
  }
  water <- data$input %in% c("green", "blue")
  if (
    anyNA(data$units) ||
      any(data$units[water] != "mm/yr") ||
      any(!data$units[!water] %in% c("", "1"))
  ) {
    .gwlp_abort(
      "Water cubes must use mm/yr and stand fractions be dimensionless."
    )
  }
  .gwlp_spatial_basis(data)
  dplyr::select(
    data,
    dplyr::all_of(c(
      "input",
      fields,
      "spatial_basis",
      "source_basis"
    ))
  )
}

.gwlp_spatial_basis <- function(data) {
  .gwlp_require(data, c("spatial_basis", "source_basis"))
  expected <- ifelse(data$input == "stand_frac", "cell_fraction", "stand")
  if (
    anyNA(data$spatial_basis) ||
      any(data$spatial_basis != expected) ||
      anyNA(data$source_basis) ||
      any(!nzchar(stringr::str_trim(data$source_basis)))
  ) {
    .gwlp_abort(
      "Identify sourced stand-based ET and cell-based area fractions."
    )
  }
}

.gwlp_volumes <- function(data, vintage) {
  bad <- !is.na(data$water_mm) &
    (!is.finite(data$water_mm) | data$water_mm < 0)
  if (any(bad)) {
    .gwlp_abort("Supplied ET depths must be finite and nonnegative.")
  }
  if (any(data$stand_frac > 0 & is.na(data$water_mm))) {
    .gwlp_abort("A positive-area grass stand has no ET depth.", "coverage")
  }
  data |>
    dplyr::mutate(
      cell_area_ha = .cell_area_ha_lat(.data$lat),
      grass_area_ha = .data$stand_frac * .data$cell_area_ha,
      # 1 mm over 1 ha is 10 m3. A supplied zero-area stand has zero volume
      # even when LPJmL does not report an intensive depth for that stand.
      grass_water_m3 = dplyr::if_else(
        .data$stand_frac == 0,
        0,
        .data$water_mm * .data$grass_area_ha * 10
      ),
      water_input_status = dplyr::if_else(
        .data$stand_frac == 0,
        "no_grass_stand",
        "supplied"
      ),
      source_water = "LPJmL_native_grassland_ET"
    ) |>
    .gwlp_provenance(vintage)
}

.gwlp_provenance <- function(data, vintage) {
  water <- vintage |>
    dplyr::filter(.data$input != "stand_frac") |>
    dplyr::rename(
      component = "input",
      water_input_basis = "spatial_basis",
      source_water_basis = "source_basis"
    )
  data |>
    dplyr::left_join(water, by = "component", relationship = "many-to-one") |>
    dplyr::mutate(
      water_scenario = .data$lpjml_run,
      source_fraction_basis = vintage$source_basis[
        vintage$input == "stand_frac"
      ]
    )
}

.gwlp_require <- function(data, columns, kind = "input") {
  missing <- columns[!rlang::has_name(data, columns)]
  if (length(missing)) {
    .gwlp_abort(
      "Missing grazing-water input field{?s}: {.field {missing}}.",
      kind
    )
  }
}

.gwlp_abort <- function(message, kind = "input") {
  cli::cli_abort(
    message,
    class = paste0("whep_grazing_water_", kind),
    .envir = parent.frame()
  )
}
