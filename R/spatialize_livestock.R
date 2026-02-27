#' Build gridded livestock dataset
#'
#' @description
#' Disaggregate country-level FAOSTAT livestock stocks and emissions to a
#' 0.5-degree grid. Each species group uses a tailored spatial proxy:
#'
#' - **Ruminants** (cattle, buffalo, sheep/goats, equines): LUH2
#'   managed pasture (`pastr`) plus rangeland (`range`), optionally
#'   weighted by a static manure-intensity reference (West et al. 2014).
#' - **Confined animals** (pigs, poultry): LUH2 aggregate cropland,
#'   reflecting intensive farming co-location with crop production.
#' - **Range specialists** (camels): LUH2 rangeland only.
#' - **Mixed** (other animals): 50/50 blend of pasture and cropland.
#'
#' For each country, year, and species group the function distributes the
#' national total proportionally to cell-level proxy weights:
#'
#' \deqn{\text{cell} = \frac{w_i}{\sum_{j \in \text{country}} w_j}
#'       \times T_{\text{country}}}
#'
#' where \eqn{w_i} is the proxy weight in cell \eqn{i} (land-use
#' hectares times optional reference-pattern intensity) and \eqn{T} is
#' the country total (heads or emissions).
#'
#' ## Data sources and references
#'
#' | Source | Use |
#' |--------|-----|
#' | FAOSTAT Production_Livestock (FAO 2024) | Country-level heads |
#' | FAOSTAT Emissions_livestock (FAO 2024) | Enteric CH\u2084, manure CH\u2084/N\u2082O |
#' | LUH2 v2h (Hurtt et al. 2020) | Time-varying pasture + cropland |
#' | West et al. (2014) | Static manure-N intensity reference |
#' | GLW3 (Gilbert et al. 2018) | Species-specific density (optional) |
#' | IPCC 2006/2019 | N-excretion rates, emission factors |
#'
#' @param livestock_data A tibble with country-level livestock data.
#'   Required columns:
#'   - `year`: Integer year.
#'   - `area_code`: Country code (WHEP polities).
#'   - `species_group`: Livestock functional-type name (e.g.
#'     `"cattle"`, `"pigs"`, `"poultry"`).
#'   - `heads`: Live animal count (number of head).
#'   Any additional numeric columns (e.g. `enteric_ch4_kt`,
#'   `manure_ch4_kt`, `manure_n2o_kt`, `manure_n_mg`) are
#'   distributed to the grid using the same proportional weights
#'   as `heads`.
#' @param gridded_pasture A tibble with annual gridded pasture extent.
#'   Required columns:
#'   - `lon`, `lat`: Cell centre coordinates (0.5 degree).
#'   - `year`: Integer year.
#'   - `pasture_ha`: Managed pasture area in hectares (LUH2 `pastr`).
#'   - `rangeland_ha`: Rangeland area in hectares (LUH2 `range`).
#' @param gridded_cropland A tibble with annual gridded cropland extent.
#'   Required columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `year`: Integer year.
#'   - `cropland_ha`: Total cropland area in hectares.
#' @param country_grid A tibble mapping grid cells to countries.
#'   Required columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `area_code`: Country code.
#' @param species_proxy A tibble mapping each `species_group` to its
#'   spatial proxy type: `"pasture"`, `"cropland"`, `"rangeland"`, or
#'   `"mixed"`.
#'   Required columns:
#'   - `species_group`: Group name (must match `livestock_data`).
#'   - `spatial_proxy`: One of `"pasture"`, `"cropland"`,
#'     `"rangeland"`, or `"mixed"`.
#'   If `NULL`, a default mapping is used (see Details).
#' @param manure_pattern A tibble with static manure-intensity
#'   weights (e.g. from West et al. 2014). Optional.
#'   Expected columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `manure_intensity`: Relative intensity (kg N per ha or
#'     similar). Values are used multiplicatively with the
#'     land-use proxy.
#'   If `NULL`, land-use weights are used alone.
#' @param glw_density A tibble with species-specific gridded
#'   livestock density from GLW3 (Gilbert et al. 2018). Optional.
#'   Expected columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `species_group`: Must match `livestock_data`.
#'   - `density`: Heads per cell (reference year ~2010).
#'   If provided, this **replaces** the LUH2-based proxy for the
#'   matching groups, while still being scaled by LUH2 time trends.
#'   If `NULL`, LUH2 proxies are used for all groups.
#'
#' @return A tibble with gridded livestock data. Columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `year`: Integer year.
#'   - `species_group`: Livestock functional type.
#'   - `heads`: Allocated live animal count.
#'   - Any additional numeric columns from `livestock_data`
#'     (e.g. `enteric_ch4_kt`, `manure_ch4_kt`).
#'
#' @export
#'
#' @examples
#' # Minimal example with toy data
#' livestock_data <- tibble::tribble(
#'   ~year, ~area_code, ~species_group, ~heads,
#'   2000L,         1L,       "cattle",   5000
#' )
#' gridded_pasture <- tibble::tribble(
#'   ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
#'    0.25, 50.25, 2000L,         600,           200,
#'    0.75, 50.25, 2000L,         400,           100
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
#' build_gridded_livestock(
#'   livestock_data, gridded_pasture, gridded_cropland, country_grid
#' )
build_gridded_livestock <- function(
  livestock_data,
  gridded_pasture,
  gridded_cropland,
  country_grid,
  species_proxy = NULL,
  manure_pattern = NULL,
  glw_density = NULL
) {
  .validate_livestock_inputs(
    livestock_data, gridded_pasture, gridded_cropland, country_grid
  )

  if (is.null(species_proxy)) {
    species_proxy <- .default_species_proxy()
  }

  # Identify numeric value columns to distribute (beyond grouping keys)
  key_cols <- c("year", "area_code", "species_group")
  numeric_cols <- setdiff(
    names(livestock_data)[
      vapply(livestock_data, is.numeric, logical(1))
    ],
    key_cols
  )
  if (length(numeric_cols) == 0L) {
    cli::cli_abort("No numeric columns found in {.arg livestock_data}.")
  }

  years <- sort(unique(livestock_data$year))
  groups <- sort(unique(livestock_data$species_group))

  cli::cli_alert_info(
    "Spatializing {length(groups)} groups over {length(years)} years"
  )

  result <- purrr::map(years, \(yr) {
    .spatialize_livestock_year(
      yr = yr,
      livestock_yr = dplyr::filter(livestock_data, year == yr),
      pasture_yr = dplyr::filter(gridded_pasture, year == yr),
      cropland_yr = dplyr::filter(gridded_cropland, year == yr),
      country_grid = country_grid,
      species_proxy = species_proxy,
      manure_pattern = manure_pattern,
      glw_density = glw_density,
      numeric_cols = numeric_cols
    )
  }, .progress = length(years) > 5L) |>
    dplyr::bind_rows()

  result
}


# --- Private helpers : livestock spatialization ---------------------------

#' Default species-group → spatial-proxy mapping.
#' @noRd
.default_species_proxy <- function() {
  tibble::tribble(
    ~species_group,  ~spatial_proxy,
    "cattle",        "pasture",
    "buffalo",       "pasture",
    "sheep_goats",   "pasture",
    "equines",       "pasture",
    "camels",        "rangeland",
    "pigs",          "cropland",
    "poultry",       "cropland",
    "other",         "mixed"
  )
}


#' Build proxy weight grid for one proxy type.
#'
#' Returns a tibble with `lon`, `lat`, `area_code`, `weight` for
#' every grid cell that has the relevant land-use type.
#' @noRd
.build_proxy_grid <- function(
  proxy_type,
  pasture_yr,
  cropland_yr,
  country_grid,
  manure_pattern
) {
  grid <- switch(
    proxy_type,
    pasture = {
      pasture_yr |>
        dplyr::transmute(
          lon, lat,
          weight = pasture_ha + rangeland_ha
        )
    },
    rangeland = {
      pasture_yr |>
        dplyr::transmute(
          lon, lat,
          weight = rangeland_ha
        )
    },
    cropland = {
      cropland_yr |>
        dplyr::transmute(
          lon, lat,
          weight = cropland_ha
        )
    },
    mixed = {
      # Combine pasture+rangeland and cropland 50/50
      p <- pasture_yr |>
        dplyr::transmute(lon, lat, p_wt = pasture_ha + rangeland_ha)
      c <- cropland_yr |>
        dplyr::transmute(lon, lat, c_wt = cropland_ha)
      dplyr::full_join(p, c, by = c("lon", "lat")) |>
        dplyr::mutate(
          p_wt = dplyr::if_else(is.na(p_wt), 0, p_wt),
          c_wt = dplyr::if_else(is.na(c_wt), 0, c_wt),
          weight = 0.5 * p_wt + 0.5 * c_wt
        ) |>
        dplyr::select(lon, lat, weight)
    },
    cli::cli_abort("Unknown proxy type: {.val {proxy_type}}")
  )

  # Join country assignment

  grid <- grid |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
    dplyr::filter(weight > 0)

  # Optionally multiply by manure-intensity reference pattern
  if (!is.null(manure_pattern)) {
    grid <- grid |>
      dplyr::left_join(
        dplyr::select(manure_pattern, lon, lat, manure_intensity),
        by = c("lon", "lat")
      ) |>
      dplyr::mutate(
        manure_intensity = dplyr::if_else(
          is.na(manure_intensity), 0, manure_intensity
        ),
        # Blend: 70% land-use, 30% manure-reference (avoid zero-out)
        weight = weight * (0.7 + 0.3 * manure_intensity /
          max(manure_intensity, na.rm = TRUE))
      )
  }

  grid
}


#' Build proxy weight grid from GLW3 species-specific density,
#' scaled by LUH2 time trend.
#' @noRd
.build_glw_proxy_grid <- function(
  group,
  glw_density,
  pasture_yr,
  cropland_yr,
  country_grid,
  proxy_type
) {
  glw_grp <- dplyr::filter(glw_density, species_group == group)
  if (nrow(glw_grp) == 0L) {
    return(NULL)
  }

  # Get current land-use for temporal scaling
  lu <- switch(
    proxy_type,
    pasture = ,
    rangeland = {
      pasture_yr |>
        dplyr::transmute(
          lon, lat,
          lu_now = pasture_ha + rangeland_ha
        )
    },
    cropland = {
      cropland_yr |>
        dplyr::transmute(lon, lat, lu_now = cropland_ha)
    },
    mixed = {
      p <- pasture_yr |>
        dplyr::transmute(lon, lat, p_wt = pasture_ha + rangeland_ha)
      c <- cropland_yr |>
        dplyr::transmute(lon, lat, c_wt = cropland_ha)
      dplyr::full_join(p, c, by = c("lon", "lat")) |>
        dplyr::mutate(
          lu_now = dplyr::if_else(is.na(p_wt), 0, p_wt) +
            dplyr::if_else(is.na(c_wt), 0, c_wt)
        ) |>
        dplyr::select(lon, lat, lu_now)
    }
  )

  # GLW density as reference weight, scaled by land-use presence
  glw_grp |>
    dplyr::select(lon, lat, density) |>
    dplyr::inner_join(lu, by = c("lon", "lat")) |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
    dplyr::mutate(
      # Where LU is zero now, the cell gets zero even if GLW has density
      weight = density * dplyr::if_else(lu_now > 0, 1, 0)
    ) |>
    dplyr::filter(weight > 0) |>
    dplyr::select(lon, lat, area_code, weight)
}


#' Spatialize all species groups for a single year.
#' @noRd
.spatialize_livestock_year <- function(
  yr,
  livestock_yr,
  pasture_yr,
  cropland_yr,
  country_grid,
  species_proxy,
  manure_pattern,
  glw_density,
  numeric_cols
) {
  groups <- unique(livestock_yr$species_group)

  purrr::map(groups, \(grp) {
    grp_data <- dplyr::filter(livestock_yr, species_group == grp)

    # Determine proxy type for this group
    proxy_row <- dplyr::filter(species_proxy, species_group == grp)
    proxy_type <- if (nrow(proxy_row) > 0) {
      proxy_row$spatial_proxy[1]
    } else {
      "pasture"  # fallback
    }

    # Try GLW3 first if available
    proxy_grid <- NULL
    if (!is.null(glw_density)) {
      proxy_grid <- .build_glw_proxy_grid(
        grp, glw_density, pasture_yr, cropland_yr,
        country_grid, proxy_type
      )
    }

    # Fall back to LUH2-based proxy
    if (is.null(proxy_grid) || nrow(proxy_grid) == 0L) {
      proxy_grid <- .build_proxy_grid(
        proxy_type, pasture_yr, cropland_yr,
        country_grid, manure_pattern
      )
    }

    if (nrow(proxy_grid) == 0L) {
      return(tibble::tibble())
    }

    .allocate_livestock_to_grid(
      grp_data, proxy_grid, numeric_cols
    ) |>
      dplyr::mutate(
        year = yr,
        species_group = grp,
        .before = 1L
      )
  }) |>
    dplyr::bind_rows()
}


#' Proportionally allocate country totals to grid cells.
#'
#' For each country, distributes every numeric column using the
#' same cell weights (proxy area or density).
#' @noRd
.allocate_livestock_to_grid <- function(
  country_data,
  proxy_grid,
  numeric_cols
) {
  # Get countries that have data
  needed <- unique(country_data$area_code)

  # Add country weight sums
  grid <- proxy_grid |>
    dplyr::filter(area_code %in% needed) |>
    dplyr::mutate(
      weight_sum = sum(weight),
      share = dplyr::if_else(
        weight_sum > 0, weight / weight_sum, 0
      ),
      .by = area_code
    )

  # Join country totals
  join_cols <- dplyr::select(
    country_data,
    area_code, dplyr::all_of(numeric_cols)
  )

  grid <- grid |>
    dplyr::inner_join(join_cols, by = "area_code")

  # Distribute each numeric column proportionally
  for (col in numeric_cols) {
    grid <- grid |>
      dplyr::mutate(
        !!col := .data[[col]] * share
      )
  }

  grid |>
    dplyr::select(
      lon, lat,
      dplyr::all_of(numeric_cols)
    )
}


#' Validate inputs for build_gridded_livestock.
#' @noRd
.validate_livestock_inputs <- function(
  livestock_data,
  gridded_pasture,
  gridded_cropland,
  country_grid
) {
  .check_columns(
    livestock_data,
    c("year", "area_code", "species_group"),
    "livestock_data"
  )
  .check_columns(
    gridded_pasture,
    c("lon", "lat", "year", "pasture_ha", "rangeland_ha"),
    "gridded_pasture"
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
