#' Normalise a country grid for polity-compartment spatialization.
#' @noRd
.spatialize_prepare_country_grid <- function(country_grid) {
  .check_columns(country_grid, c("lon", "lat", "area_code"), "country_grid")

  country_grid <- tibble::as_tibble(country_grid)
  frac_col <- intersect(
    c("cell_area_frac", "area_frac", "country_frac", "landfrac"),
    names(country_grid)
  )

  if (length(frac_col) == 0L) {
    country_grid <- dplyr::mutate(country_grid, cell_area_frac = 1)
  } else if (frac_col[[1L]] != "cell_area_frac") {
    country_grid <- dplyr::mutate(
      country_grid,
      cell_area_frac = .data[[frac_col[[1L]]]]
    )
  }

  country_grid |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      cell_area_frac = as.numeric(cell_area_frac),
      cell_area_frac = dplyr::coalesce(cell_area_frac, 1),
      cell_area_frac = pmin(pmax(cell_area_frac, 0), 1)
    )
}

#' Columns that identify a polity compartment within a physical cell.
#' @noRd
.spatialize_compartment_id_cols <- function(data) {
  intersect(c("polycell_id", "cell_id", "area_code"), names(data))
}

#' Join/grouping columns for a compartment-resolved cell.
#' @noRd
.spatialize_compartment_cell_cols <- function(data) {
  unique(c(.spatialize_compartment_id_cols(data), "lon", "lat"))
}

#' Detect whether a country grid changes through time.
#' @noRd
.spatialize_country_grid_is_time_varying <- function(country_grid) {
  any(c(
    "year",
    "valid_from", "valid_to",
    "start_year", "end_year",
    "from_year", "to_year"
  ) %in% names(country_grid))
}

#' Select the polity-cell rows valid for one simulation year.
#' @noRd
.spatialize_filter_country_grid_year <- function(country_grid, yr) {
  if ("year" %in% names(country_grid)) {
    return(dplyr::filter(country_grid, .data$year == yr))
  }

  start_col <- intersect(
    c("valid_from", "start_year", "from_year"),
    names(country_grid)
  )
  end_col <- intersect(
    c("valid_to", "end_year", "to_year"),
    names(country_grid)
  )

  if (length(start_col) > 0L || length(end_col) > 0L) {
    start_vals <- if (length(start_col) > 0L) {
      country_grid[[start_col[[1L]]]]
    } else {
      -Inf
    }
    end_vals <- if (length(end_col) > 0L) {
      country_grid[[end_col[[1L]]]]
    } else {
      Inf
    }
    start_vals[is.na(start_vals)] <- -Inf
    end_vals[is.na(end_vals)] <- Inf
    return(country_grid[yr >= start_vals & yr <= end_vals, , drop = FALSE])
  }

  country_grid
}

#' Build the static crop-pattern by country-compartment table.
#' @noRd
.spatialize_build_base_grid_cp <- function(
  country_grid,
  crop_patterns,
  type_lookup = NULL
) {
  cg_dt <- data.table::as.data.table(country_grid)
  cp_dt <- data.table::as.data.table(crop_patterns)
  base_grid_cp <- cg_dt[cp_dt, on = .(lon, lat), allow.cartesian = TRUE]
  base_grid_cp[,
    harvest_fraction := data.table::fifelse(
      is.na(harvest_fraction),
      0,
      harvest_fraction
    )
  ]
  if (!is.null(type_lookup)) {
    tlu_dt <- data.table::as.data.table(type_lookup)
    base_grid_cp[tlu_dt, luh2_type := i.luh2_type, on = .(item_prod_code)]
  }
  data.table::setkey(base_grid_cp, lon, lat)
  base_grid_cp
}
