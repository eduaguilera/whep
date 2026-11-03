#' Prepare crop inputs on source reporting polities
#'
#' @description
#' Resolve original source observations before geographic aggregation, then
#' prepare physical crop inputs for polity-native allocation. Source codes
#' are provenance; all territorial matching uses the resolved polity code.
#' This does not relabel the legacy prepared country-area pin, redistribute
#' predecessor observations, or reconstruct historical crop composition.
#'
#' @param data Named list containing `production`, `mapping`, `support`,
#'   `luh2`, `type_mapping`, `crop_patterns` and `input_revisions`.
#'   Production requires `source`, character `source_area_code`, integer
#'   `year`, `item_prod_code`, `harvested_area_ha` and `territorial_basis`
#'   equal to `"source_reported"`. This basis describes the source territory,
#'   not whether its numeric value was measured or estimated; retain source
#'   observation flags as additional columns. Already aggregated or backcast
#'   production must not be presented as original source observations.
#'   Optional numeric `area_code` and `polity_area_code` metadata are retained
#'   as `input_area_code` and `input_polity_area_code` in prepared outputs;
#'   they cannot become geography keys.
#'   The source-specific mapping requires `source`, `source_area_code`,
#'   `start_year`, `end_year` (exclusive) and `polity_code`. Its territorial
#'   correctness must be established by the source adapter; a unique match
#'   alone is not documentary evidence. Support is a complete partition from
#'   [read_polycell_support()]. LUH2 cells require `year`, `lon`, `lat`,
#'   `luh2_type`, `crop_ha` and `irrig_ha`, including supplied zeros for every
#'   required type in every cell. The type mapping requires `item_prod_code`
#'   and `luh2_type`. Crop patterns have the [build_gridded_landuse()] schema.
#'   `input_revisions` supplies nonempty revision identifiers for `production`,
#'   `mapping`, `support`, `luh2`, `crop_patterns` and, when used, `mirca`.
#'   MIRCA requires physical `lon`, `lat`, `item_prod_code`, `irrig_ha` and
#'   `rainfed_ha`, before any polity join. Patterns are applied to each year's
#'   reporting footprint; this is a spatial-pattern estimate, not an annual
#'   MIRCA observation.
#' @param method Irrigation estimation method. `"mirca"` uses the ratio of
#'   weighted MIRCA irrigated to total hectares per polity and crop, then
#'   scales the crop targets to the polity's LUH2 irrigated area. `"luh2"`
#'   distributes each type's LUH2 irrigation among crops in proportion to
#'   their harvested areas. Both retain the legacy per-crop harvested-area
#'   cap; capping can leave physical irrigation unallocated. Missing inputs
#'   never trigger a switch of method. The chosen method is recorded.
#' @param output_dir Optional directory in which to write native parquet
#'   inputs and their checksum manifest. Default `NULL` returns the inputs
#'   without writing them. Files use distinct native country-area/grid names.
#' @param source_format Either `"source_records"` for the schema above, or
#'   `"faostat"` to extract harvested hectares from an unaggregated FAOSTAT
#'   production table. The latter requires `Area Code`, `Year`, `Item Code`,
#'   `Element`, `Unit`, `Value` and `Flag`; only `Area harvested` records in
#'   `ha` for the supplied crop-type mapping are selected. No geographic rows
#'   are silently excluded: restrict the raw input to the intended reporting
#'   scope explicitly, and unmapped territories fail. Supply the authoritative
#'   whep-polities FAOSTAT map with `area_code`, `year_start`, `year_end`
#'   (inclusive) and `polity_code`. The adapter converts inclusive endpoints
#'   to the native exclusive contract and retains FAOSTAT observation flags.
#' @return A named list of prepared tables and a provenance manifest.
#' @export
#' @examples
#' # A synthetic source observation with a supplied physical irrigation zero.
#' inputs <- list(
#'   production = tibble::tibble(
#'     source = "example", source_area_code = "region", year = 1900L,
#'     item_prod_code = 15L, harvested_area_ha = 30,
#'     territorial_basis = "source_reported"
#'   ),
#'   mapping = tibble::tibble(
#'     source = "example", source_area_code = "region",
#'     start_year = 1800L, end_year = 1950L, polity_code = "AAA-1800-1950"
#'   ),
#'   support = tibble::tibble(
#'     lon = 0.25, lat = 0.25, polity_code = "AAA-1800-1950",
#'     start_year = 1800L, end_year = 1950L,
#'     cell_area_ha = 100, land_area_ha = 100
#'   ),
#'   luh2 = tibble::tibble(
#'     year = 1900L, lon = 0.25, lat = 0.25,
#'     luh2_type = "c3ann", crop_ha = 100, irrig_ha = 0
#'   ),
#'   type_mapping = tibble::tibble(item_prod_code = 15L, luh2_type = "c3ann"),
#'   crop_patterns = tibble::tibble(
#'     lon = 0.25, lat = 0.25, item_prod_code = 15L, harvest_fraction = 1
#'   ),
#'   input_revisions = list(
#'     production = "synthetic", mapping = "synthetic", support = "synthetic",
#'     luh2 = "synthetic", crop_patterns = "synthetic"
#'   )
#' )
#' prepare_polity_crop_inputs(inputs, method = "luh2")
prepare_polity_crop_inputs <- function(
  data,
  method = c("mirca", "luh2"),
  output_dir = NULL,
  source_format = c("source_records", "faostat")
) {
  method <- rlang::arg_match(method)
  source_format <- rlang::arg_match(source_format)
  .check_polity_preparation(data, method)
  if (source_format == "faostat") {
    data <- .polity_faostat_inputs(data)
  }
  targets <- if (method == "luh2") {
    .prepare_polity_luh2(data)
  } else {
    .prepare_polity_mirca(data)
  }
  inputs <- .prepared_polity_tables(data, targets, method)
  inputs$manifest$source_format <- source_format
  if (!is.null(output_dir)) {
    inputs$manifest <- .write_polity_crop_inputs(inputs, output_dir)
  }
  inputs
}

.polity_faostat_inputs <- function(data) {
  .check_columns(
    data$production,
    c("Area Code", "Year", "Item Code", "Element", "Unit", "Value", "Flag"),
    "production"
  )
  .check_columns(
    data$mapping,
    c("area_code", "year_start", "year_end", "polity_code"),
    "mapping"
  )
  .check_columns(
    data$type_mapping,
    c("item_prod_code", "luh2_type"),
    "type_mapping"
  )
  data$production <- data$production |>
    dplyr::filter(
      .data$Element == "Area harvested",
      .data$Unit == "ha",
      .data[["Item Code"]] %in% data$type_mapping$item_prod_code
    ) |>
    dplyr::transmute(
      source = "FAOSTAT_prod",
      source_area_code = as.character(.data[["Area Code"]]),
      year = .data$Year,
      item_prod_code = as.integer(.data[["Item Code"]]),
      harvested_area_ha = .data$Value,
      fao_flag = .data$Flag,
      territorial_basis = "source_reported"
    )
  # Upstream scripts/write_faostat_area_map.py documents inclusive year_end
  # and a reporting-era-only map. Never extend it to a pre-observation anchor.
  data$mapping <- data$mapping |>
    dplyr::transmute(
      source = "FAOSTAT_prod",
      source_area_code = as.character(.data$area_code),
      start_year = .data$year_start,
      end_year = .data$year_end + 1,
      polity_code = .data$polity_code
    )
  data
}

.check_polity_preparation <- function(data, method) {
  fields <- c(
    "production",
    "mapping",
    "support",
    "luh2",
    "type_mapping",
    "crop_patterns",
    "input_revisions"
  )
  revisions <- c("production", "mapping", "support", "luh2", "crop_patterns")
  if (method == "mirca") {
    fields <- c(fields, "mirca")
    revisions <- c(revisions, "mirca")
  }
  if (!is.list(data) || !all(fields %in% names(data))) {
    cli::cli_abort(
      "Missing native preparation inputs: {.val {fields}}.",
      class = "whep_polity_preparation"
    )
  }
  refs <- data$input_revisions
  if (!is.list(refs) || !all(revisions %in% names(refs))) {
    cli::cli_abort(
      "Native preparation requires explicit input revisions.",
      class = "whep_polity_preparation"
    )
  }
  valid <- vapply(
    refs[revisions],
    function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
    },
    logical(1)
  )
  if (!all(valid)) {
    cli::cli_abort(
      "Input revisions must be nonempty strings.",
      class = "whep_polity_preparation"
    )
  }
}

.prepare_polity_mirca <- function(data) {
  targets <- .polity_source_targets(data$production, data$mapping) |>
    .attach_polity_crop_type(data$type_mapping)
  if (!all(targets$item_prod_code %in% data$mirca$item_prod_code)) {
    cli::cli_abort(
      "MIRCA lacks a required target crop; choose a method explicitly.",
      class = "whep_polity_preparation"
    )
  }
  land <- .polity_luh2_totals(
    data$luh2,
    data$support,
    unique(data$type_mapping$luh2_type),
    unique(targets$polity_code)
  ) |>
    dplyr::summarise(
      total_irrig_ha = sum(.data$irrig_ha),
      .by = c("year", "polity_code")
    )
  mirca <- .polity_mirca_totals(
    data$mirca,
    data$support,
    unique(targets$year),
    unique(targets$polity_code)
  )$totals
  .allocate_polity_mirca(targets, land, mirca)
}

.allocate_polity_mirca <- function(targets, land, mirca) {
  rows <- targets |>
    dplyr::left_join(
      land,
      by = c("year", "polity_code"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      dplyr::select(
        mirca,
        "year",
        "polity_code",
        "item_prod_code",
        "irrig_frac"
      ),
      by = c("year", "polity_code", "item_prod_code"),
      relationship = "many-to-one"
    )
  if (
    anyNA(rows$total_irrig_ha) ||
      any(is.na(rows$irrig_frac) & rows$harvested_area_ha > 0)
  ) {
    cli::cli_abort(
      "MIRCA irrigation basis is missing for a crop target.",
      class = "whep_polity_preparation"
    )
  }
  rows <- rows |>
    dplyr::mutate(
      .raw_irrig = dplyr::if_else(
        .data$harvested_area_ha == 0,
        0,
        .data$harvested_area_ha * .data$irrig_frac
      ),
      .raw_total = sum(.data$.raw_irrig),
      .by = c("year", "polity_code")
    )
  if (any(rows$.raw_total == 0 & rows$total_irrig_ha > 0)) {
    cli::cli_abort(
      "MIRCA provides no weights for positive LUH2 irrigation.",
      class = "whep_polity_preparation"
    )
  }
  rows |>
    dplyr::mutate(
      irrigated_area_ha = dplyr::if_else(
        .data$.raw_total > 0,
        pmin(
          .data$harvested_area_ha,
          .data$.raw_irrig * .data$total_irrig_ha / .data$.raw_total
        ),
        0
      ),
      method_irrigation = "mirca_scaled_luh2"
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "total_irrig_ha",
        "irrig_frac",
        ".raw_irrig",
        ".raw_total"
      ))
    )
}

.prepared_polity_tables <- function(data, targets, method) {
  .check_polity_cells(
    data$crop_patterns,
    c("lon", "lat", "item_prod_code"),
    "harvest_fraction"
  )
  grid <- .polity_grid_year_aware(data$support)
  .check_polity_pattern_coverage(targets, grid, data$crop_patterns)
  list(
    country_areas = .native_target_metadata(targets),
    country_grid = grid,
    crop_patterns = data$crop_patterns,
    gridded_cropland = dplyr::summarise(
      data$luh2,
      cropland_ha = sum(.data$crop_ha),
      irrigated_ha = sum(.data$irrig_ha),
      .by = c("year", "lon", "lat")
    ),
    type_cropland = dplyr::transmute(
      data$luh2,
      year = .data$year,
      lon = .data$lon,
      lat = .data$lat,
      luh2_type = .data$luh2_type,
      type_ha = .data$crop_ha,
      type_irrig_ha = .data$irrig_ha
    ),
    manifest = list(
      schema_version = 1L,
      geographic_key = "polity",
      territorial_basis = "source_reported",
      method_irrigation = method,
      input_revisions = data$input_revisions
    )
  )
}

.native_target_metadata <- function(targets) {
  columns <- intersect(c("area_code", "polity_area_code"), names(targets))
  if (length(columns) == 0L) {
    return(targets)
  }
  renamed <- paste0("input_", columns)
  if (any(renamed %in% names(targets))) {
    cli::cli_abort(
      "Source numeric metadata has conflicting input-prefixed names.",
      class = "whep_polity_preparation"
    )
  }
  dplyr::rename(targets, !!!stats::setNames(columns, renamed))
}

.write_polity_crop_inputs <- function(inputs, output_dir) {
  rlang::check_installed("nanoparquet")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  tables <- c(
    "country_areas",
    "country_grid",
    "crop_patterns",
    "gridded_cropland",
    "type_cropland"
  )
  filenames <- c(
    "polity_country_areas.parquet",
    "polity_country_grid.parquet",
    "crop_patterns.parquet",
    "gridded_cropland.parquet",
    "type_cropland.parquet"
  )
  paths <- file.path(output_dir, filenames)
  purrr::walk2(inputs[tables], paths, nanoparquet::write_parquet)
  manifest <- inputs$manifest
  manifest$files <- as.list(stats::setNames(
    unname(tools::md5sum(paths)),
    filenames
  ))
  jsonlite::write_json(
    manifest,
    file.path(output_dir, "polity_input_manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  manifest
}

# Global crop presence is insufficient: the source pattern must intersect the
# target's own reporting territory in that year. Supplied zero-pattern rows
# retain the engine's declared behavior; absent rows must not drop a target.
.check_polity_pattern_coverage <- function(targets, grid, patterns) {
  positive <- dplyr::filter(targets, .data$harvested_area_ha > 0)
  missing <- purrr::map_dfr(sort(unique(positive$year)), function(yr) {
    available <- .filter_country_grid_year(grid, yr) |>
      dplyr::filter(.data$cell_area_frac > 0) |>
      dplyr::inner_join(
        dplyr::select(patterns, "lon", "lat", "item_prod_code"),
        by = c("lon", "lat"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(year = yr) |>
      dplyr::distinct(.data$year, .data$polity_code, .data$item_prod_code)
    positive |>
      dplyr::filter(.data$year == yr) |>
      dplyr::anti_join(
        available,
        by = c("year", "polity_code", "item_prod_code")
      )
  })
  if (nrow(missing) > 0L) {
    cli::cli_abort(
      c(
        "Crop allocation patterns lack target polity/year coverage.",
        "x" = "{nrow(missing)} positive crop target{?s} lack local patterns.",
        "i" = "Resolve the source gap or select an explicit estimation method."
      ),
      class = "whep_polity_preparation"
    )
  }
  invisible(NULL)
}
