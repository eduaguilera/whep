# The native runner consumes one producer generation, including its geography.
# A checksum is an integrity check; territorial evidence remains the source
# adapter's responsibility and its revisions travel with the output.
.check_polity_run_mode <- function(config, components) {
  if (!identical(config$geographic_key, "polity")) {
    return(invisible(NULL))
  }
  valid <- identical(components, "landuse") &&
    config$level == 0L &&
    config$output_level == 0L &&
    identical(config$area_key, "grid") &&
    identical(config$country_grid, "polycell") &&
    identical(config$grid_vintage, "year_aware")
  if (!isTRUE(valid)) {
    cli::cli_abort(
      "Polity inputs require landuse alone, level zero, grid area keys and
       year-aware polycell geography.",
      class = "whep_polity_run_mode"
    )
  }
}

.check_polity_run_dir <- function(config, input_dir) {
  if (
    identical(config$geographic_key, "polity") &&
      (is.null(input_dir) || !dir.exists(input_dir))
  ) {
    cli::cli_abort(
      "Polity mode requires a prepared native input directory.",
      class = "whep_polity_run_mode"
    )
  }
}

.polity_input_files <- function() {
  c(
    country_areas = "polity_country_areas.parquet",
    country_grid = "polity_country_grid.parquet",
    crop_patterns = "crop_patterns.parquet",
    gridded_cropland = "gridded_cropland.parquet",
    type_cropland = "type_cropland.parquet"
  )
}

.read_polity_input_manifest <- function(input_dir) {
  rlang::check_installed("jsonlite")
  path <- file.path(input_dir, "polity_input_manifest.json")
  if (is.null(input_dir) || !file.exists(path)) {
    cli::cli_abort(
      "Native prepared inputs require polity_input_manifest.json.",
      class = "whep_polity_input_manifest"
    )
  }
  manifest <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(error) {
      cli::cli_abort(
        "Native input manifest is not valid JSON.",
        class = "whep_polity_input_manifest",
        parent = error
      )
    }
  )
  .check_polity_manifest(manifest)
  files <- manifest$files
  paths <- file.path(input_dir, names(files))
  if (
    any(!file.exists(paths) | dir.exists(paths)) ||
      !identical(unname(tools::md5sum(paths)), unname(unlist(files)))
  ) {
    cli::cli_abort(
      "Native input files are missing or do not match their manifest checksums.",
      class = "whep_polity_input_checksum"
    )
  }
  manifest
}

.polity_manifest_strings <- function(values, required) {
  is.list(values) &&
    !anyDuplicated(names(values)) &&
    all(required %in% names(values)) &&
    all(vapply(
      values[required],
      function(x) {
        is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
      },
      logical(1)
    ))
}

.check_polity_manifest <- function(manifest) {
  revisions <- c("production", "mapping", "support", "luh2", "crop_patterns")
  if (is.list(manifest) && identical(manifest$method_irrigation, "mirca")) {
    revisions <- c(revisions, "mirca")
  }
  valid <- is.list(manifest) &&
    !anyDuplicated(names(manifest)) &&
    identical(manifest$schema_version, 1L) &&
    identical(manifest$geographic_key, "polity") &&
    identical(manifest$territorial_basis, "source_reported") &&
    (identical(manifest$method_irrigation, "mirca") ||
      identical(manifest$method_irrigation, "luh2")) &&
    .polity_manifest_strings(manifest$input_revisions, revisions) &&
    .polity_manifest_strings(manifest$files, unname(.polity_input_files())) &&
    setequal(names(manifest$files), unname(.polity_input_files()))
  if (!isTRUE(valid)) {
    cli::cli_abort(
      "Native manifest requires schema 1, source-reported polity identity,
       an irrigation method, input revisions and the five prepared files.",
      class = "whep_polity_input_manifest"
    )
  }
}

.load_polity_crop_inputs <- function(input_dir, config) {
  manifest <- .read_polity_input_manifest(input_dir)
  inputs <- purrr::map(.polity_input_files(), function(filename) {
    nanoparquet::read_parquet(file.path(input_dir, filename)) |>
      tibble::as_tibble()
  })
  .check_native_prepared_tables(inputs, manifest)
  cft_mapping <- .read_packaged_cft_mapping()
  if (
    !all(inputs$country_areas$item_prod_code %in% cft_mapping$item_prod_code)
  ) {
    cli::cli_abort(
      "Native targets contain crops absent from the packaged CFT mapping.",
      class = "whep_polity_input_schema"
    )
  }
  type_mapping <- inputs$country_areas |>
    dplyr::distinct(.data$item_prod_code, .data$luh2_type)
  if (anyDuplicated(type_mapping$item_prod_code)) {
    cli::cli_abort(
      "Native target crops require an unambiguous prepared LUH2 type.",
      class = "whep_polity_input_schema"
    )
  }
  inputs$type_mapping <- if (isTRUE(config$use_type_constraint)) type_mapping
  if (!isTRUE(config$use_type_constraint)) {
    inputs["type_cropland"] <- list(NULL)
  }
  inputs["multicropping"] <- list(NULL)
  inputs["polity_support"] <- list(NULL)
  inputs$cft_mapping <- cft_mapping
  inputs$manifest <- manifest
  inputs
}

.check_native_prepared_tables <- function(inputs, manifest) {
  targets <- inputs$country_areas
  .check_columns(
    targets,
    c(
      "year",
      "polity_code",
      "item_prod_code",
      "harvested_area_ha",
      "irrigated_area_ha",
      "source",
      "source_area_code",
      "territorial_basis",
      "luh2_type",
      "method_irrigation"
    ),
    "native country areas"
  )
  .check_polity_target_amounts(targets)
  .check_polity_input_years(targets$year)
  .check_polity_target_basis(targets, manifest)
  .check_source_target_strings(
    targets,
    c("polity_code", "source", "source_area_code", "luh2_type")
  )
  if (
    anyNA(targets$territorial_basis) ||
      any(targets$territorial_basis != "source_reported") ||
      "area_code" %in% names(targets) ||
      "area_code" %in% names(inputs$country_grid) ||
      anyDuplicated(targets[c("year", "polity_code", "item_prod_code")])
  ) {
    cli::cli_abort(
      "Native prepared targets need unique source-reported polity identities.",
      class = "whep_polity_input_schema"
    )
  }
  grid <- .polity_grid_year_aware(inputs$country_grid)
  if (!identical(grid$cell_area_frac, inputs$country_grid$cell_area_frac)) {
    cli::cli_abort(
      "Native grid fractions disagree with its complete partition areas.",
      class = "whep_polity_input_schema"
    )
  }
  .check_polity_pattern_coverage(targets, grid, inputs$crop_patterns)
  .check_polity_prepared_water(inputs)
}

.attach_polity_input_source <- function(out, targets) {
  keys <- c("year", "polity_code", "item_prod_code")
  metadata <- setdiff(
    names(targets),
    c(names(out), "harvested_area_ha", "irrigated_area_ha")
  )
  dplyr::left_join(
    out,
    dplyr::select(targets, dplyr::all_of(c(keys, metadata))),
    by = c("year", "polity_code", "item_prod_code"),
    relationship = "many-to-one"
  )
}

.write_polity_run_manifest <- function(manifest, out_dir) {
  jsonlite::write_json(
    manifest,
    file.path(out_dir, "polity_input_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
}


.check_polity_prepared_water <- function(inputs) {
  cells <- inputs$gridded_cropland
  .check_polity_cells(
    cells,
    c("year", "lon", "lat"),
    c("cropland_ha", "irrigated_ha")
  )
  .check_polity_input_years(cells$year)
  capacity <- purrr::map(sort(unique(cells$year)), function(yr) {
    live <- .filter_country_grid_year(inputs$country_grid, yr)
    dplyr::filter(cells, .data$year == yr) |>
      dplyr::inner_join(
        dplyr::select(live, "lon", "lat", "polity_code", "cell_area_frac"),
        by = c("lon", "lat"),
        relationship = "many-to-many"
      ) |>
      dplyr::summarise(
        .irrig_capacity = sum(.data$irrigated_ha * .data$cell_area_frac),
        .by = c("year", "polity_code")
      )
  }) |>
    dplyr::bind_rows()
  positive <- inputs$country_areas |>
    dplyr::filter(.data$irrigated_area_ha > 0) |>
    dplyr::left_join(
      capacity,
      by = c("year", "polity_code"),
      relationship = "many-to-one"
    )
  if (anyNA(positive$.irrig_capacity) || any(positive$.irrig_capacity <= 0)) {
    cli::cli_abort(
      "Positive irrigation targets require supplied physical irrigation
       on their own polity's cells in the target year.",
      class = "whep_polity_input_irrigation"
    )
  }
}


.check_polity_target_amounts <- function(targets) {
  columns <- c("harvested_area_ha", "irrigated_area_ha")
  valid <- vapply(
    targets[columns],
    function(x) {
      is.numeric(x) && all(is.finite(x)) && all(x >= 0)
    },
    logical(1)
  )
  if (
    !all(valid) ||
      any(targets$irrigated_area_ha > targets$harvested_area_ha)
  ) {
    cli::cli_abort(
      "Native targets require finite nonnegative harvested and irrigated
       hectares, with irrigation no larger than harvested area.",
      class = "whep_polity_input_schema"
    )
  }
}

.check_polity_target_basis <- function(targets, manifest) {
  columns <- c("start_year", "end_year")
  if (!all(rlang::has_name(targets, columns))) {
    cli::cli_abort(
      "Native targets require explicit source mapping start and end years.",
      class = "whep_polity_input_schema"
    )
  }
  valid <- vapply(
    targets[columns],
    function(x) {
      is.numeric(x) && all(is.finite(x)) && all(x == trunc(x))
    },
    logical(1)
  )
  method <- switch(
    manifest$method_irrigation,
    luh2 = "luh2_proportional",
    mirca = "mirca_scaled_luh2"
  )
  if (
    !all(valid) ||
      any(targets$start_year >= targets$end_year) ||
      anyNA(targets$method_irrigation) ||
      any(targets$method_irrigation != method) ||
      any(targets$year < targets$start_year | targets$year >= targets$end_year)
  ) {
    cli::cli_abort(
      "Native targets must agree with the manifest irrigation method and
       fall within their explicit half-open source mapping intervals.",
      class = "whep_polity_input_schema"
    )
  }
}
