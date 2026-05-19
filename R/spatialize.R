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
#'   Optional columns:
#'   - `cell_area_frac` (or `area_frac`): Fraction of the physical cell
#'     belonging to this polity compartment. Defaults to 1.
#'   - `polycell_id`, `cell_id`: Stable compartment/cell identifiers
#'     preserved in outputs when present.
#'   - `year` or validity intervals (`valid_from`/`valid_to`,
#'     `start_year`/`end_year`, `from_year`/`to_year`) for historical,
#'     time-varying polity overlays.
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
#'   - `max_iterations`: Maximum iterations for the redistribution
#'     loop. Default: `1000L`.
#'   - `expansion_threshold`: Iteration number after which crops are
#'     allowed to expand into cells without an existing pattern.
#'     Default: `100L`.
#'
#' @return A tibble with gridded crop (or CFT) harvested areas.
#'   Columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `year`: Integer year.
#'   - `area_code`: WHEP polity code for this cell compartment.
#'   - `polycell_id`, `cell_id`: Preserved when supplied in
#'     `country_grid`.
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
  .validate_landuse_inputs(
    country_areas,
    crop_patterns,
    gridded_cropland,
    country_grid
  )
  country_grid <- .spatialize_prepare_country_grid(country_grid)
  config <- .resolve_landuse_config(config)
  years <- config$years
  cft_mapping <- config$cft_mapping
  type_cropland <- config$type_cropland
  type_mapping <- config$type_mapping
  multicropping <- config$multicropping
  max_iterations <- config$max_iterations
  expansion_threshold <- config$expansion_threshold

  country_areas <- .ensure_irrigation_cols(country_areas)
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

  # Build type lookup: item_prod_code -> luh2_type
  type_lookup <- NULL
  if (!is.null(type_cropland) && !is.null(type_mapping)) {
    type_lookup <- dplyr::select(
      type_mapping,
      item_prod_code,
      luh2_type
    ) |>
      dplyr::distinct()
    # nolint: object_usage_linter
    type_count <- dplyr::n_distinct(type_lookup$luh2_type)
    # message printed once per build_gridded_landuse call; caller logs progress
  }

  years <- sort(unique(country_areas$year))
  n_workers <- config$n_workers

  # Year-invariant work — done once, shared across the year loop.
  # Cartesian: cells × crops. Per-year work just joins cropland_ha onto this.
  country_grid_is_dynamic <- .spatialize_country_grid_is_time_varying(
    country_grid
  )
  base_grid_cp <- if (country_grid_is_dynamic) {
    NULL
  } else {
    .spatialize_build_base_grid_cp(country_grid, crop_patterns, type_lookup)
  }

  .spatialize_one <- function(yr) {
    country_grid_yr <- .spatialize_filter_country_grid_year(country_grid, yr)
    if (nrow(country_grid_yr) == 0L) {
      cli::cli_abort("No {.arg country_grid} rows valid for year {yr}.")
    }
    .spatialize_year(
      yr,
      country_areas = dplyr::filter(country_areas, year == yr),
      base_grid_cp = if (country_grid_is_dynamic) {
        .spatialize_build_base_grid_cp(
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
      max_iterations = max_iterations,
      expansion_threshold = expansion_threshold
    )
  }

  parts <- if (n_workers > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(years, .spatialize_one, mc.cores = n_workers)
  } else {
    purrr::map(years, .spatialize_one)
  }
  result <- data.table::rbindlist(parts, fill = TRUE)
  rm(parts)

  if (!is.null(cft_mapping)) {
    result <- .aggregate_to_cft(result, cft_mapping)
  }

  tibble::as_tibble(result)
}

# --- Private helpers ----------------------------------------------------------

#' Spatialize a single year using data.table for all crops at once.
#' @noRd
.spatialize_year <- function(
  yr,
  country_areas,
  base_grid_cp,
  cropland,
  country_grid,
  type_cropland_yr = NULL,
  multicropping,
  max_iterations,
  expansion_threshold
) {
  t_alloc0 <- proc.time()[["elapsed"]]

  cl <- data.table::as.data.table(cropland)
  ca <- data.table::as.data.table(country_areas)

  # Per-year: copy the static base (cells × crops) and attach cropland.
  grid_cp <- data.table::copy(base_grid_cp)
  grid_cp[,
    cell_area_frac := data.table::fifelse(
      is.na(cell_area_frac),
      1,
      cell_area_frac
    )
  ]
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

  # Type-aware: replace cropland with type-specific where applicable
  if (use_type_aware) {
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

    # Check which (country, crop) have type potential; fallback where zero
    grid_cp_tc[,
      type_pot := sum(harvest_fraction * cropland_ha, na.rm = TRUE),
      by = .(area_code, item_prod_code)
    ]
    grid_cp_tc[
      type_pot <= 0,
      `:=`(
        cropland_ha = .orig_cropland_ha,
        irrigated_ha = .orig_irrigated_ha,
        rainfed_ha = .orig_rainfed_ha
      )
    ]

    grid_cp <- grid_cp_tc
    grid_cp[, `:=`(
      type_pot = NULL,
      luh2_type = NULL,
      .orig_cropland_ha = NULL,
      .orig_irrigated_ha = NULL,
      .orig_rainfed_ha = NULL
    )]
  }

  # Join country_areas (cartesian: each country-crop gets its cells)
  dat <- grid_cp[ca, on = .(area_code, item_prod_code), nomatch = NA]
  dat <- dat[!is.na(harvested_area_ha)]

  # Compute allocation for ALL (country, crop) pairs in one pass
  dat[, `:=`(
    rf_potential = harvest_fraction * rainfed_ha,
    ir_potential = harvest_fraction * irrigated_ha
  )]
  dat[,
    `:=`(
      rf_pot_sum = sum(rf_potential, na.rm = TRUE),
      ir_pot_sum = sum(ir_potential, na.rm = TRUE),
      rainfed_sum = sum(rainfed_ha, na.rm = TRUE),
      irrigated_sum = sum(irrigated_ha, na.rm = TRUE)
    ),
    by = .(area_code, item_prod_code)
  ]
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

  result <- dat[
    allocated_rf > 0 | allocated_ir > 0,
    c(
      .spatialize_compartment_id_cols(dat),
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
  result <- result |>
    .apply_capacity_constraint(
      cropland,
      country_grid,
      multicropping_yr,
      max_iterations,
      expansion_threshold
    ) |>
    dplyr::mutate(year = yr, .before = 1L)

  t_cap <- round(proc.time()[["elapsed"]] - t_alloc0 - t_alloc, 2)
  cli::cli_alert(
    "  Year {yr}: {nrow(result)} rows (alloc {t_alloc}s, cap {t_cap}s)"
  )

  result
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
    n_workers = 1L
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
  utils::modifyList(defaults, config)
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
  max_iterations,
  expansion_threshold
) {
  if (is.null(multicropping)) {
    return(allocated)
  }

  country_cols <- .spatialize_compartment_id_cols(country_grid)
  country_lookup <- country_grid |>
    dplyr::select(
      dplyr::any_of(country_cols),
      lon,
      lat,
      cell_area_frac
    )

  # Build per-compartment capacity. The physical cropland layer is shared
  # by all polity compartments in a cell, then clipped to each compartment's
  # geographic envelope via cell_area_frac.
  capacity <- cropland |>
    dplyr::inner_join(country_lookup, by = c("lon", "lat")) |>
    dplyr::inner_join(multicropping, by = c("lon", "lat")) |>
    dplyr::mutate(
      rainfed_ha = (cropland_ha - irrigated_ha) * cell_area_frac,
      irrigated_ha = irrigated_ha * cell_area_frac,
      rf_capacity = rainfed_ha * mc_rainfed,
      ir_capacity = irrigated_ha * mc_irrigated
    ) |>
    dplyr::select(
      dplyr::all_of(.spatialize_compartment_cell_cols(country_lookup)),
      rf_capacity,
      ir_capacity
    )

  cell_cols <- .spatialize_compartment_cell_cols(allocated)
  capacity_join_cols <- intersect(cell_cols, names(capacity))

  # Compute per-cell sums
  cell_sums <- allocated |>
    dplyr::summarise(
      total_rf = sum(rainfed_ha, na.rm = TRUE),
      total_ir = sum(irrigated_ha, na.rm = TRUE),
      .by = dplyr::all_of(cell_cols)
    )

  overloaded <- cell_sums |>
    dplyr::inner_join(capacity, by = capacity_join_cols) |>
    dplyr::filter(
      total_rf > rf_capacity + 1e-4 |
        total_ir > ir_capacity + 1e-4
    )

  if (nrow(overloaded) == 0L) {
    return(allocated)
  }

  # Find which countries need redistribution
  countries_to_fix <- overloaded |>
    dplyr::pull(area_code) |>
    unique()

  # Split into fixable vs stable

  to_fix <- dplyr::filter(
    allocated,
    .data$area_code %in% countries_to_fix
  )
  stable <- dplyr::filter(
    allocated,
    !(.data$area_code %in% countries_to_fix)
  )

  fixed <- .redistribute_countries(
    to_fix,
    capacity,
    country_grid,
    max_iterations,
    expansion_threshold
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
  join_cols <- intersect(
    .spatialize_compartment_cell_cols(allocated),
    names(capacity)
  )

  # Add capacity info
  work <- allocated |>
    dplyr::left_join(capacity, by = join_cols)

  countries <- unique(work$area_code)

  purrr::map(countries, \(ctry) {
    ctry_data <- dplyr::filter(work, area_code == ctry)
    .redistribute_single_country(
      ctry_data,
      max_iterations,
      expansion_threshold
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(
      dplyr::any_of(.spatialize_compartment_id_cols(work)),
      lon,
      lat,
      item_prod_code,
      rainfed_ha,
      irrigated_ha
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
  cell_cols <- .spatialize_compartment_cell_cols(data)

  # Capture per-crop targets BEFORE the logit loop. The loop's exit
  # criterion ("no cell over cap") says nothing about whether per-crop
  # country totals are preserved -- the scale-down + logit-growth
  # iteration is approximate, so totals can drift well below target.
  # The final renormalisation step (below) restores per-crop country
  # totals exactly. Without it, runs lose ~20% of FAOSTAT area
  # uniformly across countries.
  per_crop_target <- data |>
    dplyr::summarise(
      target_rf = sum(rainfed_ha, na.rm = TRUE),
      target_ir = sum(irrigated_ha, na.rm = TRUE),
      .by = item_prod_code
    )

  for (iter in seq_len(max_iterations)) {
    # Check convergence: per-cell sums vs capacity
    cell_check <- data |>
      dplyr::summarise(
        total_rf = sum(rainfed_ha, na.rm = TRUE),
        total_ir = sum(irrigated_ha, na.rm = TRUE),
        .by = dplyr::all_of(c(cell_cols, "rf_capacity", "ir_capacity"))
      ) |>
      dplyr::mutate(
        rf_excess = pmax(total_rf - rf_capacity, 0),
        ir_excess = pmax(total_ir - ir_capacity, 0)
      )

    max_excess <- max(
      max(cell_check$rf_excess),
      max(cell_check$ir_excess)
    )

    if (max_excess <= tolerance) {
      break
    }

    # Apply logit redistribution for irrigated
    data <- .logit_redistribute(
      data,
      cell_check,
      "irrigated_ha",
      "ir_capacity",
      "total_ir",
      "ir_excess",
      cell_cols
    )
    # Apply logit redistribution for rainfed
    data <- .logit_redistribute(
      data,
      cell_check,
      "rainfed_ha",
      "rf_capacity",
      "total_rf",
      "rf_excess",
      cell_cols
    )
  }

  # Final per-crop renormalisation: per-crop country totals must equal
  # the pre-loop target (which is the FAOSTAT harvested area for that
  # country-crop pair, by construction of the upstream allocation).
  # This may push a few cells slightly above their physical-area cap;
  # the trade-off is intentional because FAOSTAT country totals are
  # authoritative.
  data |>
    dplyr::left_join(per_crop_target, by = "item_prod_code") |>
    dplyr::mutate(
      current_rf = sum(rainfed_ha, na.rm = TRUE),
      current_ir = sum(irrigated_ha, na.rm = TRUE),
      .by = item_prod_code
    ) |>
    dplyr::mutate(
      rf_scale = dplyr::if_else(
        current_rf > 0 & target_rf > 0,
        target_rf / current_rf,
        1
      ),
      ir_scale = dplyr::if_else(
        current_ir > 0 & target_ir > 0,
        target_ir / current_ir,
        1
      ),
      rainfed_ha = rainfed_ha * rf_scale,
      irrigated_ha = irrigated_ha * ir_scale
    ) |>
    dplyr::select(
      -target_rf,
      -target_ir,
      -current_rf,
      -current_ir,
      -rf_scale,
      -ir_scale
    )
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
  excess_col,
  cell_cols
) {
  # Compute per-crop target sums
  targets <- data |>
    dplyr::summarise(
      target = sum(.data[[ha_col]], na.rm = TRUE),
      .by = item_prod_code
    )

  # Scale down overloaded cells
  over <- cell_check |>
    dplyr::filter(.data[[excess_col]] > 0) |>
    dplyr::mutate(
      scale_factor = .data[[cap_col]] / .data[[total_col]]
    ) |>
    dplyr::select(dplyr::all_of(cell_cols), scale_factor)

  if (nrow(over) > 0L) {
    data <- data |>
      dplyr::left_join(over, by = cell_cols) |>
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
      current_sum = sum(.data[[ha_col]], na.rm = TRUE),
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
    dplyr::select(dplyr::all_of(cell_cols)) |>
    dplyr::mutate(.is_under = TRUE)

  data |>
    dplyr::left_join(increments, by = "item_prod_code") |>
    dplyr::left_join(under, by = cell_cols) |>
    dplyr::mutate(
      is_under = dplyr::coalesce(.data$.is_under, FALSE),
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
        ) *
          .data[[cap_col]],
        .data[[ha_col]]
      )
    ) |>
    dplyr::select(-increment, -is_under, -frac, -.is_under)
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

#' Aggregate crop-level results to CFT level.
#' @noRd
.aggregate_to_cft <- function(data, cft_mapping) {
  group_cols <- unique(c(
    .spatialize_compartment_id_cols(data),
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

#' Scale allocated hectares down so total per cell never exceeds physical cropland.
#' @noRd
.normalize_to_cropland <- function(data, gridded_cropland) {
  dt <- data.table::as.data.table(data)
  gc <- data.table::as.data.table(
    dplyr::select(gridded_cropland, lon, lat, year, cropland_ha)
  )
  dt[gc, cropland_ha := i.cropland_ha, on = .(lon, lat, year)]
  dt[,
    total_ha := sum(rainfed_ha + irrigated_ha, na.rm = TRUE),
    by = .(lon, lat, year)
  ]
  dt[,
    scale := data.table::fifelse(
      !is.na(cropland_ha) & total_ha > cropland_ha & total_ha > 0,
      cropland_ha / total_ha,
      1
    )
  ]
  dt[, `:=`(
    rainfed_ha = rainfed_ha * scale,
    irrigated_ha = irrigated_ha * scale
  )]
  dt[, `:=`(total_ha = NULL, scale = NULL, cropland_ha = NULL)]
  tibble::as_tibble(dt)
}
