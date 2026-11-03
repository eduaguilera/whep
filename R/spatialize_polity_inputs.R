# Physical cells remain independent of source reporting codes. Shares come
# from the complete partition before any requested polity is selected.
.polity_luh2_totals <- function(
  cells,
  support,
  required_types,
  polity_codes = NULL
) {
  cells <- tibble::as_tibble(cells)
  keys <- c("year", "lon", "lat", "luh2_type")
  .check_polity_cells(cells, keys, c("crop_ha", "irrig_ha"))
  .check_polity_input_years(cells$year)
  .check_polity_cell_classes(cells, "luh2_type", required_types)
  if (any(cells$irrig_ha > cells$crop_ha)) {
    cli::cli_abort(
      "LUH2 irrigated area exceeds supplied crop area.",
      class = "whep_polity_input_measure"
    )
  }
  grid <- .polity_grid_year_aware(support)
  totals <- purrr::map(sort(unique(cells$year)), function(yr) {
    annual <- dplyr::filter(cells, .data$year == yr)
    .polity_weight_cells(annual, grid, yr) |>
      dplyr::summarise(
        crop_ha = sum(.data$crop_ha * .data$cell_area_frac),
        irrig_ha = sum(.data$irrig_ha * .data$cell_area_frac),
        .by = c("year", "polity_code", "luh2_type")
      )
  }) |>
    dplyr::bind_rows()
  .select_input_polities(totals, polity_codes)
}

.polity_mirca_totals <- function(
  patterns,
  support,
  years,
  polity_codes = NULL
) {
  patterns <- tibble::as_tibble(patterns)
  keys <- c("lon", "lat", "item_prod_code")
  .check_polity_cells(patterns, keys, c("irrig_ha", "rainfed_ha"))
  .check_polity_input_years(years)
  .check_polity_cell_classes(
    patterns,
    "item_prod_code",
    unique(patterns$item_prod_code)
  )
  grid <- .polity_grid_year_aware(support)
  totals <- purrr::map(sort(unique(years)), function(yr) {
    .polity_weight_cells(patterns, grid, yr) |>
      dplyr::mutate(year = yr) |>
      dplyr::summarise(
        irrig_ha = sum(.data$irrig_ha * .data$cell_area_frac),
        rainfed_ha = sum(.data$rainfed_ha * .data$cell_area_frac),
        .by = c("year", "polity_code", "item_prod_code")
      ) |>
      dplyr::mutate(
        # An explicitly supplied zero crop has no irrigation fraction.
        irrig_frac = dplyr::if_else(
          .data$irrig_ha + .data$rainfed_ha > 0,
          .data$irrig_ha / (.data$irrig_ha + .data$rainfed_ha),
          NA_real_
        )
      ) |>
      dplyr::relocate("year")
  }) |>
    dplyr::bind_rows()
  list(
    patterns = patterns,
    totals = .select_input_polities(totals, polity_codes)
  )
}

.check_polity_input_years <- function(years) {
  if (
    !is.numeric(years) ||
      length(years) == 0L ||
      any(!is.finite(years)) ||
      any(years != floor(years))
  ) {
    cli::cli_abort(
      "Physical preparation requires finite whole calendar years.",
      class = "whep_polity_input_year"
    )
  }
}

.check_polity_cells <- function(cells, keys, measures) {
  .check_columns(cells, c(keys, measures), "cells")
  valid <- vapply(
    cells[c("lon", "lat", measures)],
    function(x) {
      is.numeric(x) && all(is.finite(x))
    },
    logical(1)
  )
  if (
    nrow(cells) == 0L ||
      !all(valid) ||
      any(vapply(cells[measures], function(x) any(x < 0), logical(1)))
  ) {
    cli::cli_abort(
      "Physical cells require supplied finite, nonnegative hectare measures.",
      class = "whep_polity_input_measure"
    )
  }
  if (anyNA(cells[keys])) {
    cli::cli_abort(
      "Physical cell keys cannot be missing.",
      class = "whep_polity_input_class"
    )
  }
  if (anyDuplicated(cells[keys])) {
    cli::cli_abort(
      "Physical inputs repeat a cell and class key.",
      class = "whep_polity_input_duplicate"
    )
  }
  reserved <- c("polity_code", "cell_area_frac", "start_year", "end_year")
  if (any(reserved %in% names(cells))) {
    cli::cli_abort(
      "Physical inputs must precede polity support joins.",
      class = "whep_polity_input_schema"
    )
  }
}

.check_polity_cell_classes <- function(cells, column, required) {
  groups <- intersect(c("year", "lon", "lat"), names(cells))
  counts <- cells |>
    dplyr::summarise(n_class = dplyr::n(), .by = dplyr::all_of(groups))
  if (
    length(required) == 0L ||
      anyNA(required) ||
      any(!nzchar(trimws(as.character(required)))) ||
      anyDuplicated(required) ||
      !setequal(cells[[column]], required) ||
      any(counts$n_class != length(required))
  ) {
    cli::cli_abort(
      "Each physical cell must supply every required class, including zeros.",
      class = "whep_polity_input_class"
    )
  }
}

.polity_weight_cells <- function(cells, grid, year) {
  # Strict half-open periods also guard the final support endpoint.
  live <- dplyr::filter(
    grid,
    .data$start_year <= year,
    year < .data$end_year
  )
  cell_keys <- c("lon", "lat")
  source_cells <- dplyr::distinct(cells, .data$lon, .data$lat)
  grid_cells <- dplyr::distinct(live, .data$lon, .data$lat)
  missing_support <- dplyr::anti_join(source_cells, grid_cells, by = cell_keys)
  missing_input <- dplyr::anti_join(grid_cells, source_cells, by = cell_keys)
  if (
    nrow(live) == 0L || nrow(missing_support) > 0L || nrow(missing_input) > 0L
  ) {
    cli::cli_abort(
      "Physical cells and polity support must cover the same cells in {year}.",
      class = "whep_polity_input_support"
    )
  }
  dplyr::inner_join(
    cells,
    dplyr::select(live, "lon", "lat", "polity_code", "cell_area_frac"),
    by = c("lon", "lat"),
    relationship = "many-to-many"
  )
}

.select_input_polities <- function(totals, polity_codes) {
  if (is.null(polity_codes)) {
    return(totals)
  }
  if (
    !is.character(polity_codes) ||
      length(polity_codes) == 0L ||
      anyNA(polity_codes) ||
      any(!nzchar(trimws(polity_codes))) ||
      !all(polity_codes %in% totals$polity_code)
  ) {
    cli::cli_abort(
      "Requested polities must have physical input support.",
      class = "whep_polity_input_support"
    )
  }
  dplyr::filter(totals, .data$polity_code %in% polity_codes)
}
