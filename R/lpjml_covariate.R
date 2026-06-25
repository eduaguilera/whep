#' Build an LPJmL/WHEP spatial covariate function
#'
#' @description
#' Creates a `function(centroids_sf, year)` suitable for APIs that accept a
#' spatial covariate density, such as [build_constant_territory_series()]. The
#' covariate is built from prepared WHEP/LPJmL spatialization inputs, either
#' from a local `input_dir` or from pinned WHEP spatialization inputs when
#' `input_dir = NULL`.
#'
#' @param input_dir Directory holding prepared WHEP/LPJmL input parquets. If
#'   `NULL`, the function reads pinned spatialization inputs via
#'   [whep_read_file()].
#' @param years Integer vector of years to retain. If `NULL`, all years present
#'   in the required input are used.
#' @param weighting Either `"total_cropland"`, `"crop_pattern"`, or a custom
#'   `function(centroids_sf, year)`. A custom function is returned unchanged.
#' @param item_prod_code FAOSTAT/WHEP production item code. Required when
#'   `weighting = "crop_pattern"`.
#' @param cft_mapping Mapping from WHEP item codes to CFT/LUH2 types. Defaults
#'   to [cft_mapping].
#'
#' @returns A function `function(centroids_sf, year)` returning non-negative
#'   density values aligned to `centroids_sf`.
#'
#' @export
#'
#' @examples
#' # A custom covariate function is returned unchanged, ready to plug into
#' # build_constant_territory_series(). Here a uniform (area-weighting) density:
#' uniform <- function(centroids_sf, year) rep(1, nrow(centroids_sf))
#' covariate <- make_lpjml_covariate(weighting = uniform)
#' identical(covariate, uniform)
#'
#' # The "total_cropland" and "crop_pattern" modes instead read prepared
#' # WHEP/LPJmL spatialization parquets from `input_dir` (or pinned inputs).
make_lpjml_covariate <- function(
  input_dir = NULL,
  years = NULL,
  weighting = c("total_cropland", "crop_pattern"),
  item_prod_code = NULL,
  cft_mapping = whep::cft_mapping
) {
  if (is.function(weighting)) {
    return(weighting)
  }
  if (!is.character(weighting) || length(weighting) != 1L) {
    cli::cli_abort(
      "{.arg weighting} must be {.val total_cropland}, {.val crop_pattern}, or a function."
    )
  }
  weighting <- match.arg(weighting, c("total_cropland", "crop_pattern"))
  years <- .lpjml_normalize_years(years)

  if (weighting == "total_cropland") {
    surface <- .lpjml_read_total_cropland(input_dir, years)
    return(.lpjml_surface_covariate(
      surface,
      value_col = "cropland_ha",
      label = "lpjml_total_cropland"
    ))
  }

  surface <- .lpjml_read_crop_pattern_surface(
    input_dir,
    years,
    item_prod_code,
    cft_mapping
  )
  .lpjml_surface_covariate(
    surface,
    value_col = "crop_pattern_ha",
    label = .lpjml_surface_label(surface, "lpjml_crop_pattern")
  )
}

.lpjml_normalize_years <- function(years) {
  if (is.null(years)) {
    return(NULL)
  }
  sort(unique(as.integer(years)))
}

.lpjml_read_total_cropland <- function(input_dir, years) {
  cropland <- .lpjml_read_spatial_input(
    input_dir,
    "gridded_cropland.parquet",
    .lpjml_spatial_input_aliases()[["gridded_cropland"]]
  ) |>
    dplyr::select("lon", "lat", "year", "cropland_ha")

  if (!is.null(years)) {
    cropland <- dplyr::filter(cropland, .data$year %in% years)
  }
  .lpjml_abort_if_empty(cropland, "gridded_cropland", years)
  cropland
}

.lpjml_read_crop_pattern_surface <- function(
  input_dir,
  years,
  item_prod_code,
  cft_mapping
) {
  if (is.null(item_prod_code)) {
    cli::cli_abort(
      "{.arg item_prod_code} is required when {.code weighting = \"crop_pattern\"}."
    )
  }
  item_prod_code <- as.integer(item_prod_code)

  item_map <- cft_mapping[cft_mapping$item_prod_code == item_prod_code, ]
  if (nrow(item_map) != 1L) {
    cli::cli_abort(
      "Expected exactly one {.arg cft_mapping} row for {.arg item_prod_code} = {item_prod_code}; found {nrow(item_map)}."
    )
  }
  luh2_type <- item_map$luh2_type[[1]]
  if (is.na(luh2_type) || !nzchar(luh2_type)) {
    cli::cli_abort(
      "{.arg cft_mapping} has no {.field luh2_type} for {.arg item_prod_code} = {item_prod_code}."
    )
  }

  type_cropland <- .lpjml_read_spatial_input(
    input_dir,
    "type_cropland.parquet",
    .lpjml_spatial_input_aliases()[["type_cropland"]]
  ) |>
    dplyr::filter(.data$luh2_type == !!luh2_type) |>
    dplyr::select("lon", "lat", "year", "type_ha")

  if (!is.null(years)) {
    type_cropland <- dplyr::filter(type_cropland, .data$year %in% years)
  }
  .lpjml_abort_if_empty(type_cropland, "type_cropland", years)

  crop_pattern <- .lpjml_read_spatial_input(
    input_dir,
    "crop_patterns.parquet",
    .lpjml_spatial_input_aliases()[["crop_patterns"]]
  ) |>
    dplyr::filter(.data$item_prod_code == !!item_prod_code) |>
    dplyr::select("lon", "lat", "harvest_fraction")
  .lpjml_abort_if_empty(crop_pattern, "crop_patterns", NULL)

  dt <- data.table::as.data.table(type_cropland)
  pattern <- data.table::as.data.table(crop_pattern)
  dt[pattern, harvest_fraction := i.harvest_fraction, on = .(lon, lat)]
  dt[is.na(harvest_fraction), harvest_fraction := 0]
  dt[, crop_pattern_ha := type_ha * harvest_fraction]

  out <- tibble::as_tibble(dt[, .(lon, lat, year, crop_pattern_ha)])
  item_name <- if ("item_prod_name" %in% names(item_map)) {
    item_map$item_prod_name[[1]]
  } else {
    item_prod_code
  }
  attr(out, "label") <- paste0(
    "lpjml_",
    item_name,
    "_",
    luh2_type,
    "_crop_pattern"
  )
  out
}

.lpjml_surface_label <- function(surface, default) {
  label <- attr(surface, "label", exact = TRUE)
  if (is.null(label)) {
    default
  } else {
    label
  }
}

.lpjml_surface_covariate <- function(surface, value_col, label = value_col) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg sf} is required to evaluate LPJmL covariates."
    )
  }
  .lpjml_check_surface(surface, value_col, label)

  surface <- as.data.frame(surface)
  surface <- surface[!is.na(surface[[value_col]]) & surface[[value_col]] > 0, ]
  .lpjml_abort_if_empty(surface, label, NULL)

  grid <- .lpjml_grid_spec(surface)
  surface$lon_i <- .lpjml_grid_index(
    surface$lon,
    grid$lon_min,
    grid$lon_max,
    grid$lon_res
  )
  surface$lat_i <- .lpjml_grid_index(
    surface$lat,
    grid$lat_min,
    grid$lat_max,
    grid$lat_res
  )
  surface$density <- pmax(
    surface[[value_col]] /
      .lpjml_grid_cell_area_ha(surface$lat, grid$lon_res, grid$lat_res),
    0
  )

  dt <- data.table::as.data.table(surface[, c(
    "year",
    "lon_i",
    "lat_i",
    "density"
  )])
  dt <- dt[,
    .(density = sum(density, na.rm = TRUE)),
    by = .(year, lon_i, lat_i)
  ]
  data.table::setkey(dt, year, lon_i, lat_i)
  available_years <- sort(unique(surface$year))

  fn <- function(centroids_sf, year) {
    coords <- sf::st_coordinates(sf::st_transform(centroids_sf, 4326))
    nearest_year <- available_years[which.min(abs(available_years - year))]
    query <- data.table::data.table(
      year = as.integer(nearest_year),
      lon_i = .lpjml_grid_index(
        coords[, "X"],
        grid$lon_min,
        grid$lon_max,
        grid$lon_res
      ),
      lat_i = .lpjml_grid_index(
        coords[, "Y"],
        grid$lat_min,
        grid$lat_max,
        grid$lat_res
      )
    )
    values <- dt[query, on = .(year, lon_i, lat_i)]$density
    values[is.na(values)] <- 0
    values
  }
  attr(fn, "label") <- label
  fn
}

.lpjml_check_surface <- function(surface, value_col, label) {
  required <- c("lon", "lat", "year", value_col)
  missing <- setdiff(required, names(surface))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "LPJmL covariate surface {.val {label}} is missing required column{?s}:",
      "x" = "{.field {missing}}."
    ))
  }
  invisible(NULL)
}

.lpjml_abort_if_empty <- function(data, name, years) {
  if (nrow(data) > 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "No rows available for LPJmL covariate input {.val {name}}.",
    "i" = if (is.null(years)) {
      "Check that the required spatialization input is available."
    } else {
      "Requested years: {.val {years}}."
    }
  ))
}

.lpjml_grid_spec <- function(surface) {
  lon_vals <- sort(unique(surface$lon))
  lat_vals <- sort(unique(surface$lat))
  if (length(lon_vals) < 2L || length(lat_vals) < 2L) {
    cli::cli_abort(
      "LPJmL covariate surface must contain at least two longitude and latitude values to infer grid resolution."
    )
  }
  list(
    lon_res = min(diff(lon_vals)),
    lat_res = min(diff(lat_vals)),
    lon_min = min(lon_vals),
    lat_min = min(lat_vals),
    lon_max = max(lon_vals),
    lat_max = max(lat_vals)
  )
}

.lpjml_grid_index <- function(x, min_value, max_value, resolution) {
  outside <- x < (min_value - resolution / 2) |
    x > (max_value + resolution / 2)
  index <- as.integer(round((x - min_value) / resolution))
  index[outside | is.na(index)] <- NA_integer_
  index
}

.lpjml_grid_cell_area_ha <- function(lat, lon_res, lat_res) {
  radius_m <- 6371008.8
  dlon <- lon_res * pi / 180
  lat1 <- (lat - lat_res / 2) * pi / 180
  lat2 <- (lat + lat_res / 2) * pi / 180
  abs(radius_m^2 * dlon * (sin(lat2) - sin(lat1))) / 10000
}

.lpjml_read_spatial_input <- function(input_dir, file_name, alias) {
  if (!is.null(input_dir)) {
    path <- file.path(input_dir, file_name)
    if (!file.exists(path)) {
      cli::cli_abort(c(
        "Missing required LPJmL spatial input in {.path {input_dir}}:",
        "x" = "{.file {file_name}}."
      ))
    }
    return(tibble::as_tibble(nanoparquet::read_parquet(path)))
  }

  tryCatch(
    whep_read_file(alias),
    error = function(e) {
      cli::cli_abort(c(
        "Could not read pinned LPJmL spatial input {.val {alias}}.",
        "i" = "Provide a local {.arg input_dir} with {.file {file_name}} to use locally prepared inputs instead.",
        "Caused by" = conditionMessage(e)
      ))
    }
  )
}

.lpjml_spatial_input_aliases <- function() {
  c(
    crop_patterns = "spatialize-crop-patterns",
    gridded_cropland = "spatialize-gridded-cropland",
    type_cropland = "spatialize-type-cropland"
  )
}
