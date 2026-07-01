# Gridded yearly land-use-class areas for the historical carbon balance, read
# from the LUH2 v2h "states" product (Hurtt et al. 2020 GMD). LUH2 v2h reports
# the FRACTION (0..1) of each grid cell occupied by each of 12 subgrid land-use
# states, natively at 0.25 degrees over 850-2015. The 12 states are aggregated
# here into the four carbon-balance classes (cropland, grassland, natural,
# urban) and converted to areas via the spherical 0.5-degree cell area; finer
# native cells are area-aggregated to the 0.5-degree grid. The output matches
# the land_use input contract of build_carbon_balance(): (lon, lat, area_code,
# year, land_use, fraction, area_ha) with lowercase class names.
#
# NOTE ON THE PIN PAYLOAD (verified 2026-06-30): the registered pin
# "luh2_v2h_states" currently ships a serialized HDF5/NetCDF blob, not a tidy
# table: its ".parquet" member is a single STRING column holding the chunked
# HDF byte stream, and its ".csv" member is the HDF5 file itself (with a
# CRLF-mangled signature). Neither reads as a tabular LUH2 grid through the
# tabular readers. The states->classes mapping and the LUH2 v2h spec (12 states,
# fractions 0..1, 0.25-degree native, 850-2015) are taken from Hurtt et al.
# (2020). .luh2_read_states() decodes the pin once re-uploaded as a clean
# NetCDF (or a tidy parquet with lon/lat/year/state/fraction); until then it
# errors and callers must pass data$states.

#' Read gridded yearly LUH2 land-use-class fractions and areas.
#'
#' @description
#' Read the LUH2 v2h gridded land-use "states" product and aggregate its 12
#' subgrid states into the four carbon-balance classes (cropland, grassland,
#' natural, urban). Per cell-year-class the `fraction` is the sum of the member
#' states' grid-cell fractions (0..1); `area_ha` is that fraction times the
#' spherical 0.5-degree cell area. At `resolution = "polity"` the areas are
#' summed to each overlapping polity via the country grid; a border cell keeps
#' every polity it overlaps.
#'
#' @param resolution `"grid"` (default, per cell and class) or `"polity"`
#'   (aggregated to `area_code` per year and class).
#' @param years Optional integer vector of calendar years to keep. `NULL` keeps
#'   every year present in the source.
#' @param data Named list of pre-loaded inputs bypassing the pin read: `states`
#'   (raw per-cell-year-state fractions with `lon`, `lat`, `year`, `land_use`,
#'   `fraction`) and `country_grid` (`lon`, `lat`, `area_code`,
#'   `cell_area_frac`). Each falls back to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble with columns `lon`, `lat`, `area_code`, `year`, `land_use`,
#'   `fraction` and `area_ha` at `"grid"` resolution; at `"polity"` resolution
#'   `lon` and `lat` are dropped and `area_ha` is summed per
#'   `(area_code, year, land_use)`.
#' @source LUH2 v2h, Hurtt, G. C. et al. (2020). Harmonization of global land
#'   use change and management for the period 850-2100 (LUH2) for CMIP6.
#'   Geoscientific Model Development 13, 5425-5464. \doi{10.5194/gmd-13-5425-2020}.
#' @export
#' @examples
#' read_luh2_landuse(example = TRUE)
read_luh2_landuse <- function(
  resolution = c("grid", "polity"),
  years = NULL,
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_luh2_landuse())
  }
  resolution <- rlang::arg_match(resolution)
  data <- data %||% list()
  states <- data$states %||% .luh2_read_states_source(years = years)
  if (!is.null(years)) {
    states <- dplyr::filter(states, .data$year %in% years)
  }
  country_grid <- data$country_grid %||% .luh2_read_country_grid()

  grid <- states |>
    .luh2_map_classes() |>
    dplyr::mutate(area_ha = .data$fraction * .luh2_cell_area_ha(.data$lat))

  .luh2_to_polity(grid, country_grid, resolution)
}

# Aggregate the 12 LUH2 states into the four lowercase carbon-balance classes,
# summing member-state fractions per (cell, year, class).
.luh2_map_classes <- function(states) {
  classes <- .luh2_class_lookup()
  unknown <- setdiff(unique(states$land_use), classes$state)
  if (length(unknown) > 0L) {
    cli::cli_warn("Unmapped LUH2 state{?s} dropped: {unknown}.")
  }
  states |>
    dplyr::inner_join(classes, by = c("land_use" = "state")) |>
    dplyr::summarise(
      fraction = sum(.data$fraction, na.rm = TRUE),
      .by = c("lon", "lat", "year", "class")
    ) |>
    dplyr::rename(land_use = "class")
}

# The LUH2 v2h state -> carbon-balance class lookup (Hurtt et al. 2020).
.luh2_class_lookup <- function() {
  tibble::tribble(
    ~state, ~class,
    "c3ann", "cropland",
    "c4ann", "cropland",
    "c3per", "cropland",
    "c4per", "cropland",
    "c3nfx", "cropland",
    "pastr", "grassland",
    "range", "grassland",
    "primf", "natural",
    "secdf", "natural",
    "primn", "natural",
    "secdn", "natural",
    "urban", "urban"
  )
}

# Spherical area (ha) of a 0.5-degree grid cell centred at latitude `lat`:
# R^2 * dlon * (sin(lat + dlat/2) - sin(lat - dlat/2)). A cell at the equator is
# ~3091 km2 = 309100 ha, shrinking with cos(lat).
.luh2_cell_area_ha <- function(lat) {
  earth_radius_m <- 6371000
  half_step_rad <- 0.25 * pi / 180
  lon_step_rad <- 0.5 * pi / 180
  band <- sin(lat * pi / 180 + half_step_rad) -
    sin(lat * pi / 180 - half_step_rad)
  earth_radius_m^2 * lon_step_rad * band / 1e4
}

# Assign each cell to its overlapping polities via the country grid and, for
# resolution "polity", sum area_ha to (area_code, year, land_use). A border
# cell keeps every polity it overlaps (area split by cell_area_frac).
.luh2_to_polity <- function(grid, country_grid, resolution) {
  cg <- .normalize_country_grid(country_grid) |>
    dplyr::select("lon", "lat", "area_code", "cell_area_frac")

  joined <- grid |>
    dplyr::inner_join(cg, by = c("lon", "lat")) |>
    dplyr::mutate(area_ha = .data$area_ha * .data$cell_area_frac)

  if (resolution == "grid") {
    return(
      joined |>
        dplyr::select(
          "lon",
          "lat",
          "area_code",
          "year",
          "land_use",
          "fraction",
          "area_ha"
        ) |>
        tibble::as_tibble()
    )
  }

  joined |>
    dplyr::summarise(
      area_ha = sum(.data$area_ha, na.rm = TRUE),
      .by = c("area_code", "year", "land_use")
    ) |>
    tibble::as_tibble()
}

# -- States source dispatch ---------------------------------------------------

# Choose the states source: the clean LOCAL LUH2 v2h states.nc when present
# (the registered pin payload is corrupted, see file header), otherwise fall
# back to the pin reader. The directory comes from the WHEP_LUH2_DIR env var.
.luh2_read_states_source <- function(years = NULL) {
  nc_path <- file.path(.luh2_states_dir(), "states.nc")
  if (file.exists(nc_path)) {
    return(.luh2_read_states_nc(nc_path, years = years))
  }
  .luh2_read_states(years = years)
}

.luh2_states_dir <- function() {
  Sys.getenv("WHEP_LUH2_DIR", "C:/XL_files/LUH2/LUH2 v2h")
}

# -- Local states.nc reader ---------------------------------------------------

# Read the 12 LUH2 v2h state fractions for the requested years from the clean
# local states.nc and area-aggregate the 0.25-degree native grid to the
# 0.5-degree carbon grid. Returns long (lon, lat, year, land_use, fraction) on
# the 0.5-degree cell centres. LUH2 v2h time index 1 = year 850 CE.
.luh2_read_states_nc <- function(nc_path, years = NULL) {
  years <- years %||% seq_len(.luh2_time_len_nc(nc_path))
  purrr::map_dfr(years, \(yr) .luh2_read_states_nc_year(nc_path, yr))
}

# Read one year's 12 states from states.nc and aggregate to 0.5 degrees.
.luh2_read_states_nc_year <- function(nc_path, year) {
  vars <- .luh2_class_lookup()$state
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  lat <- ncdf4::ncvar_get(nc, "lat")
  time_idx <- year - 850L + 1L
  time_len <- nc$dim$time$len
  if (time_idx < 1L || time_idx > time_len) {
    cli::cli_abort(
      "Year {year} outside LUH2 v2h range {850L}-{850L + time_len - 1L}."
    )
  }
  lon <- ncdf4::ncvar_get(nc, "lon")
  long <- purrr::map_dfr(vars, \(v) {
    .luh2_slice_to_cells(
      ncdf4::ncvar_get(
        nc,
        v,
        start = c(1L, 1L, time_idx),
        count = c(-1L, -1L, 1L)
      ),
      lon,
      lat,
      v
    )
  })
  .luh2_aggregate_half_degree(long, year)
}

# Turn a native [lon, lat] fraction slice into a long table of native cells,
# dropping NA (ocean) fractions, tagged with the state name. Ocean sub-cells
# carry no cropland and must contribute 0 to the aggregate: dropping them here
# is corrected by dividing by the FULL 0.5-degree cell area downstream (not by
# the summed weights of the present sub-cells only).
.luh2_slice_to_cells <- function(vals, lon, lat, state) {
  # vals is the [lon, lat] slice from ncvar_get: as.vector() runs lon fastest,
  # so the coordinate table must run lon fastest too. CJ varies its LAST
  # argument fastest, hence CJ(lat, lon).
  dt <- data.table::CJ(lat = lat, lon = lon, sorted = FALSE)
  dt[, fraction := as.vector(vals)]
  dt <- dt[!is.na(fraction)]
  dt[, land_use := state]
  tibble::as_tibble(dt)
}

# Area-aggregate native 0.25-degree fractions to the 0.5-degree carbon grid.
# Per (0.5-cell, state) the aggregated fraction is the native-area-weighted
# cropland/grass/etc. area summed over the member native sub-cells divided by
# the FULL 0.5-degree cell area, so ocean (dropped-NA) sub-cells contribute 0.
# This conserves the native land area exactly.
.luh2_aggregate_half_degree <- function(long, year) {
  dt <- data.table::as.data.table(long)
  dt[, lon5 := floor((lon + 180) / 0.5) * 0.5 - 180 + 0.25]
  dt[, lat5 := floor((lat + 90) / 0.5) * 0.5 - 90 + 0.25]
  dt[, native_area := .luh2_native_cell_area_ha(lat)]
  agg <- dt[,
    .(state_area = sum(fraction * native_area)),
    by = .(lon = lon5, lat = lat5, land_use)
  ]
  agg[, fraction := state_area / .luh2_cell_area_ha(lat)]
  agg[, year := as.integer(year)]
  tibble::as_tibble(agg[, .(lon, lat, year, land_use, fraction)])
}

# Spherical area (ha) of a native 0.25-degree LUH2 cell centred at latitude
# `lat`. Four of these tile one 0.5-degree carbon cell.
.luh2_native_cell_area_ha <- function(lat) {
  earth_radius_m <- 6371000
  half_step_rad <- 0.125 * pi / 180
  lon_step_rad <- 0.25 * pi / 180
  band <- sin(lat * pi / 180 + half_step_rad) -
    sin(lat * pi / 180 - half_step_rad)
  earth_radius_m^2 * lon_step_rad * band / 1e4
}

.luh2_time_len_nc <- function(nc_path) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  nc$dim$time$len
}

# -- Pin readers --------------------------------------------------------------

# Decode the LUH2 v2h states pin into per-cell-year-state fractions. The current
# pin payload is a serialized NetCDF blob (see file header); until it is
# re-uploaded as a clean NetCDF or a tidy parquet this errors and callers must
# inject data$states.
.luh2_read_states <- function(years = NULL) {
  raw <- tryCatch(
    whep_read_file("luh2_v2h_states"),
    error = function(e) {
      cli::cli_abort(c(
        "Could not read the {.val luh2_v2h_states} pin.",
        i = "Pass {.code data$states} (per-cell-year-state fractions).",
        "Caused by" = conditionMessage(e)
      ))
    }
  )
  .luh2_tidy_states(raw, years)
}

# Reshape a raw LUH2 states table into long (lon, lat, year, land_use,
# fraction). Accepts either already-long input or one column per state.
.luh2_tidy_states <- function(raw, years = NULL) {
  raw <- tibble::as_tibble(raw)
  if (rlang::has_name(raw, "fraction") && rlang::has_name(raw, "land_use")) {
    long <- raw
  } else {
    states <- intersect(.luh2_class_lookup()$state, names(raw))
    if (length(states) == 0L) {
      cli::cli_abort("No LUH2 state columns found in the pinned data.")
    }
    long <- tidyr::pivot_longer(
      raw,
      cols = dplyr::all_of(states),
      names_to = "land_use",
      values_to = "fraction"
    )
  }
  if (!is.null(years)) {
    long <- dplyr::filter(long, .data$year %in% years)
  }
  dplyr::select(long, "lon", "lat", "year", "land_use", "fraction")
}

.luh2_read_country_grid <- function() {
  whep_read_file("spatialize-country-grid")
}
