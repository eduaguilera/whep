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
#' @param cft_mapping A tibble mapping FAOSTAT items to CFT names.
#'   Expected columns:
#'   - `item_prod_code`: FAOSTAT item code.
#'   - `cft_name`: Target crop functional type name.
#'   If `NULL`, no CFT aggregation is performed and individual crop
#'   results are returned.
#' @param type_cropland A tibble with per-cell, per-year, per-type
#'   cropland. When provided alongside `type_mapping`, each crop is
#'   allocated only into cells that contain its LUH2
#'   crop-functional type, giving time-varying, type-constrained
#'   allocation.
#'   Expected columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `year`: Integer year.
#'   - `luh2_type`: LUH2 crop type (`c3ann`, `c4ann`, `c3per`,
#'     or `c3nfx`).
#'   - `type_ha`: Cropland hectares for this type.
#'   - `type_irrig_ha`: Irrigated hectares for this type.
#'   If `NULL`, falls back to total cropland (original behaviour).
#' @param type_mapping A tibble mapping each crop to its LUH2 type.
#'   Expected columns:
#'   - `item_prod_code`: FAOSTAT item code.
#'   - `luh2_type`: LUH2 crop type.
#'   If `NULL`, type-aware allocation is disabled even when
#'   `type_cropland` is provided.
#' @param multicropping A tibble with per-cell multi-cropping
#'   suitability factors. Expected columns:
#'   - `lon`, `lat`: Cell coordinates.
#'   - `mc_rainfed`: Rainfed suitability factor (default 1).
#'   - `mc_irrigated`: Irrigated suitability factor (default 1).
#'   If `NULL`, no capacity constraint is applied.
#' @param max_iterations Maximum iterations for the redistribution
#'   loop. Default: `1000L`.
#' @param expansion_threshold Iteration number after which crops are
#'   allowed to expand into cells without an existing pattern.
#'   Default: `100L`.
#'
#' @return A tibble with gridded crop (or CFT) harvested areas.
#'   Columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `year`: Integer year.
#'   - `crop_name` or `cft_name`: Crop or CFT identifier.
#'   - `rainfed_ha`: Rainfed harvested area in the cell.
#'   - `irrigated_ha`: Irrigated harvested area in the cell.
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
#'   2000L,         1L,             15L,            1000
#' )
#' crop_patterns <- tibble::tribble(
#'   ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
#'    0.25, 50.25,             15L,               0.6,
#'    0.75, 50.25,             15L,               0.4
#' )
#' gridded_cropland <- tibble::tribble(
#'   ~lon,  ~lat,  ~year, ~cropland_ha,
#'    0.25, 50.25, 2000L,          800,
#'    0.75, 50.25, 2000L,          500
#' )
#' country_grid <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code,
#'    0.25, 50.25,         1L,
#'    0.75, 50.25,         1L
#' )
#' build_gridded_landuse(
#'   country_areas, crop_patterns, gridded_cropland, country_grid
#' )
build_gridded_landuse <- function(
  country_areas,
  crop_patterns,
  gridded_cropland,
  country_grid,
  cft_mapping = NULL,
  type_cropland = NULL,
  type_mapping = NULL,
  multicropping = NULL,
  max_iterations = 1000L,
  expansion_threshold = 100L
) {
  .validate_landuse_inputs(
    country_areas, crop_patterns,
    gridded_cropland, country_grid
  )

  country_areas <- .ensure_irrigation_cols(country_areas)
  gridded_cropland <- .ensure_gridded_irrigation(gridded_cropland)

  # Build type lookup: item_prod_code -> luh2_type
  type_lookup <- NULL
  if (!is.null(type_cropland) && !is.null(type_mapping)) {
    type_lookup <- dplyr::select(
      type_mapping, item_prod_code, luh2_type
    ) |>
      dplyr::distinct()
    cli::cli_alert_info(
      "Type-aware allocation: {dplyr::n_distinct(type_lookup$luh2_type)} LUH2 types"
    )
  }

  years <- sort(unique(country_areas$year))

  result <- purrr::map(years, \(yr) {
    .spatialize_year(
      yr,
      country_areas = dplyr::filter(country_areas, year == yr),
      crop_patterns = crop_patterns,
      cropland = dplyr::filter(gridded_cropland, year == yr),
      country_grid = country_grid,
      type_cropland_yr = if (!is.null(type_cropland)) {
        dplyr::filter(type_cropland, year == yr)
      },
      type_lookup = type_lookup,
      multicropping = multicropping,
      max_iterations = max_iterations,
      expansion_threshold = expansion_threshold
    )
  }) |>
    dplyr::bind_rows()

  if (!is.null(cft_mapping)) {
    result <- .aggregate_to_cft(result, cft_mapping)
  }

  result
}

# --- Private helpers ----------------------------------------------------------

#' Spatialize a single year.
#' @noRd
.spatialize_year <- function(
  yr,
  country_areas,
  crop_patterns,
  cropland,
  country_grid,
  type_cropland_yr = NULL,
  type_lookup = NULL,
  multicropping,
  max_iterations,
  expansion_threshold
) {
  crops <- unique(country_areas$item_prod_code)

  purrr::map(crops, \(crop_code) {
    # Determine this crop's LUH2 type (if type-aware)
    crop_type <- NULL
    crop_type_cl <- NULL
    if (!is.null(type_lookup) && !is.null(type_cropland_yr)) {
      crop_type <- type_lookup$luh2_type[
        type_lookup$item_prod_code == crop_code
      ]
      if (length(crop_type) == 1L) {
        crop_type_cl <- dplyr::filter(
          type_cropland_yr, luh2_type == crop_type
        )
      } else {
        crop_type <- NULL
      }
    }

    .spatialize_crop(
      yr = yr,
      crop_code = crop_code,
      country_areas = dplyr::filter(
        country_areas, item_prod_code == crop_code
      ),
      crop_pattern = dplyr::filter(
        crop_patterns, item_prod_code == crop_code
      ),
      cropland = cropland,
      country_grid = country_grid,
      type_cropland = crop_type_cl
    )
  }) |>
    dplyr::bind_rows() |>
    .apply_capacity_constraint(
      cropland, country_grid, multicropping,
      max_iterations, expansion_threshold
    ) |>
    dplyr::mutate(year = yr, .before = 1L)
}

#' Spatialize one crop across all countries for a given year.
#' @noRd
.spatialize_crop <- function(
  yr,
  crop_code,
  country_areas,
  crop_pattern,
  cropland,
  country_grid,
  type_cropland = NULL
) {
  use_type <- !is.null(type_cropland) && nrow(type_cropland) > 0L

  if (use_type) {
    # Type-aware: use type-specific cropland
    grid <- country_grid |>
      dplyr::inner_join(
        dplyr::select(
          type_cropland,
          lon, lat, type_ha, type_irrig_ha
        ),
        by = c("lon", "lat")
      ) |>
      dplyr::left_join(
        dplyr::select(crop_pattern, lon, lat, harvest_fraction),
        by = c("lon", "lat")
      ) |>
      dplyr::mutate(
        harvest_fraction = dplyr::if_else(
          is.na(harvest_fraction), 0, harvest_fraction
        ),
        cropland_ha = type_ha,
        irrigated_ha = type_irrig_ha,
        rainfed_ha = type_ha - type_irrig_ha
      )

    # Check if this type has any potential in the needed countries
    needed <- unique(country_areas$area_code)
    grid_has <- grid |>
      dplyr::filter(area_code %in% needed) |>
      dplyr::summarise(
        total_pot = sum(harvest_fraction * cropland_ha)
      ) |>
      dplyr::pull(total_pot)

    if (length(grid_has) == 0L || grid_has <= 0) {
      # Fall back to total cropland for this crop
      use_type <- FALSE
    }
  }

  if (!use_type) {
    # Original behaviour: total cropland
    grid <- country_grid |>
      dplyr::inner_join(cropland, by = c("lon", "lat")) |>
      dplyr::left_join(
        dplyr::select(crop_pattern, lon, lat, harvest_fraction),
        by = c("lon", "lat")
      ) |>
      dplyr::mutate(
        harvest_fraction = dplyr::if_else(
          is.na(harvest_fraction), 0, harvest_fraction
        ),
        rainfed_ha = cropland_ha - irrigated_ha
      )
  }

  grid |>
    dplyr::inner_join(
      dplyr::select(
        country_areas,
        area_code, harvested_area_ha, irrigated_area_ha
      ),
      by = "area_code"
    ) |>
    .allocate_country_area() |>
    dplyr::mutate(item_prod_code = crop_code) |>
    dplyr::select(
      lon, lat, item_prod_code,
      rainfed_ha = allocated_rf,
      irrigated_ha = allocated_ir
    )
}

#' Proportionally allocate country total to grid cells.
#'
#' For each country, distributes the total harvested area to cells
#' using: `cell_ha = pattern * cropland * (total / sum(pattern *
#' cropland))`. Falls back to uniform distribution over cropland if
#' no pattern exists.
#' @noRd
.allocate_country_area <- function(grid) {
  grid |>
    dplyr::mutate(
      # Weighted potential: pattern x cropland
      rf_potential = harvest_fraction * rainfed_ha,
      ir_potential = harvest_fraction * irrigated_ha,
      # Country sums of potential
      rf_pot_sum = sum(rf_potential),
      ir_pot_sum = sum(ir_potential),
      # Fallback: uniform over cropland
      rf_uniform = rainfed_ha / sum(rainfed_ha),
      ir_uniform = irrigated_ha / sum(irrigated_ha),
      # Replace NaN from 0/0
      rf_uniform = dplyr::if_else(
        is.nan(rf_uniform), 0, rf_uniform
      ),
      ir_uniform = dplyr::if_else(
        is.nan(ir_uniform), 0, ir_uniform
      ),
      .by = area_code
    ) |>
    dplyr::mutate(
      rainfed_target = harvested_area_ha - irrigated_area_ha,
      allocated_rf = dplyr::if_else(
        rf_pot_sum > 0,
        rf_potential / rf_pot_sum * rainfed_target,
        rf_uniform * rainfed_target
      ),
      allocated_ir = dplyr::if_else(
        ir_pot_sum > 0,
        ir_potential / ir_pot_sum * irrigated_area_ha,
        ir_uniform * irrigated_area_ha
      )
    )
}

#' Validate that required columns exist.
#' @noRd
.validate_landuse_inputs <- function(
  country_areas, crop_patterns,
  gridded_cropland, country_grid
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
  max_iterations,
  expansion_threshold
) {
  if (is.null(multicropping)) {
    return(allocated)
  }

  # Build per-cell capacity
  capacity <- cropland |>
    dplyr::inner_join(multicropping, by = c("lon", "lat")) |>
    dplyr::mutate(
      rainfed_ha = cropland_ha - irrigated_ha,
      rf_capacity = rainfed_ha * mc_rainfed,
      ir_capacity = irrigated_ha * mc_irrigated
    ) |>
    dplyr::select(lon, lat, rf_capacity, ir_capacity)

  # Compute per-cell sums
  cell_sums <- allocated |>
    dplyr::summarise(
      total_rf = sum(rainfed_ha),
      total_ir = sum(irrigated_ha),
      .by = c(lon, lat)
    )

  overloaded <- cell_sums |>
    dplyr::inner_join(capacity, by = c("lon", "lat")) |>
    dplyr::filter(
      total_rf > rf_capacity + 1e-4 |
        total_ir > ir_capacity + 1e-4
    )

  if (nrow(overloaded) == 0L) {
    return(allocated)
  }

  # Find which countries need redistribution
  countries_to_fix <- overloaded |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
    dplyr::pull(area_code) |>
    unique()

  # Split into fixable vs stable

  to_fix <- dplyr::filter(
    allocated,
    .get_area_code_from_grid(allocated, country_grid) %in%
      countries_to_fix
  )
  stable <- dplyr::filter(
    allocated,
    !(.get_area_code_from_grid(allocated, country_grid) %in%
        countries_to_fix)
  )

  fixed <- .redistribute_countries(
    to_fix, capacity, country_grid,
    max_iterations, expansion_threshold
  )

  dplyr::bind_rows(stable, fixed)
}

#' Get area_code for allocated cells via country_grid lookup.
#' @noRd
.get_area_code_from_grid <- function(allocated, country_grid) {
  lookup <- dplyr::select(country_grid, lon, lat, area_code)
  allocated |>
    dplyr::left_join(lookup, by = c("lon", "lat")) |>
    dplyr::pull(area_code)
}

#' Redistribute excess harvested area for overloaded countries.
#'
#' Uses the logit-increment-logistic approach from LandInG:
#' 1. Express cell allocation as fraction of capacity.
#' 2. Apply logit transform.
#' 3. Add an increment proportional to the country deficit.
#' 4. Inverse logit to get new fraction.
#' 5. Repeat until convergence or max iterations.
#' @noRd
.redistribute_countries <- function(
  allocated,
  capacity,
  country_grid,
  max_iterations,
  expansion_threshold
) {
  # Add area_code and capacity info
  work <- allocated |>
    dplyr::left_join(
      dplyr::select(country_grid, lon, lat, area_code),
      by = c("lon", "lat")
    ) |>
    dplyr::left_join(capacity, by = c("lon", "lat"))

  countries <- unique(work$area_code)

  purrr::map(countries, \(ctry) {
    ctry_data <- dplyr::filter(work, area_code == ctry)
    .redistribute_single_country(
      ctry_data, max_iterations, expansion_threshold
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(
      lon, lat, item_prod_code,
      rainfed_ha, irrigated_ha
    )
}

#' Redistribute for one country using logit transformation.
#' @noRd
.redistribute_single_country <- function(
  data,
  max_iterations,
  expansion_threshold
) {
  tolerance <- 1e-4

  for (iter in seq_len(max_iterations)) {
    # Check convergence: per-cell sums vs capacity
    cell_check <- data |>
      dplyr::summarise(
        total_rf = sum(rainfed_ha),
        total_ir = sum(irrigated_ha),
        .by = c(lon, lat, rf_capacity, ir_capacity)
      ) |>
      dplyr::mutate(
        rf_excess = pmax(total_rf - rf_capacity, 0),
        ir_excess = pmax(total_ir - ir_capacity, 0)
      )

    max_excess <- max(
      max(cell_check$rf_excess),
      max(cell_check$ir_excess)
    )

    if (max_excess <= tolerance) break

    # Apply logit redistribution for irrigated
    data <- .logit_redistribute(
      data, cell_check, "irrigated_ha", "ir_capacity",
      "total_ir", "ir_excess"
    )
    # Apply logit redistribution for rainfed
    data <- .logit_redistribute(
      data, cell_check, "rainfed_ha", "rf_capacity",
      "total_rf", "rf_excess"
    )
  }

  data
}

#' One pass of logit-based redistribution.
#'
#' For cells over capacity, scale down proportionally. For cells
#' under capacity, increase via logit transform with an increment
#' proportional to the country-wide shortfall.
#' @noRd
.logit_redistribute <- function(
  data,
  cell_check,
  ha_col,
  cap_col,
  total_col,
  excess_col
) {
  # Compute per-crop target sums
  targets <- data |>
    dplyr::summarise(
      target = sum(.data[[ha_col]]),
      .by = item_prod_code
    )

  # Scale down overloaded cells
  over <- cell_check |>
    dplyr::filter(.data[[excess_col]] > 0) |>
    dplyr::mutate(
      scale_factor = .data[[cap_col]] / .data[[total_col]]
    ) |>
    dplyr::select(lon, lat, scale_factor)

  if (nrow(over) > 0L) {
    data <- data |>
      dplyr::left_join(over, by = c("lon", "lat")) |>
      dplyr::mutate(
        !!ha_col := dplyr::if_else(
          !is.na(scale_factor),
          .data[[ha_col]] * scale_factor,
          .data[[ha_col]]
        )
      ) |>
      dplyr::select(-scale_factor)
  }

  # Compute current sums after scaling
  current <- data |>
    dplyr::summarise(
      current_sum = sum(.data[[ha_col]]),
      .by = item_prod_code
    )

  # Compute increment per crop
  increments <- targets |>
    dplyr::inner_join(current, by = "item_prod_code") |>
    dplyr::mutate(
      deficit = target - current_sum,
      increment = dplyr::if_else(
        current_sum > 0,
        deficit / current_sum,
        0
      )
    ) |>
    dplyr::select(item_prod_code, increment)

  # Apply logit-increment to non-overloaded cells
  under <- cell_check |>
    dplyr::filter(.data[[excess_col]] <= 0) |>
    dplyr::select(lon, lat)

  data |>
    dplyr::left_join(increments, by = "item_prod_code") |>
    dplyr::mutate(
      is_under = paste(lon, lat) %in% paste(under$lon, under$lat),
      frac = dplyr::if_else(
        .data[[cap_col]] > 0,
        .data[[ha_col]] / .data[[cap_col]],
        0
      ),
      # Clamp fraction to (epsilon, 1 - epsilon)
      frac = pmax(pmin(frac, 1 - 1e-6), 1e-6),
      !!ha_col := dplyr::if_else(
        is_under & increment != 0,
        .logistic(
          .logit(frac) + increment
        ) * .data[[cap_col]],
        .data[[ha_col]]
      )
    ) |>
    dplyr::select(-increment, -is_under, -frac)
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

#' Aggregate crop-level results to CFT level.
#' @noRd
.aggregate_to_cft <- function(data, cft_mapping) {
  data |>
    dplyr::inner_join(
      dplyr::select(cft_mapping, item_prod_code, cft_name),
      by = "item_prod_code"
    ) |>
    dplyr::summarise(
      rainfed_ha = sum(rainfed_ha),
      irrigated_ha = sum(irrigated_ha),
      .by = c(lon, lat, year, cft_name)
    )
}
