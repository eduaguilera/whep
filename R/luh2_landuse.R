# Gridded yearly land-use-class areas for the historical carbon balance, read
# from the LUH2 v2h "states" product (Hurtt et al. 2020 GMD). LUH2 v2h reports
# the FRACTION (0..1) of each grid cell occupied by each of 12 subgrid land-use
# states, natively at 0.25 degrees from 850 CE on. The 12 states are aggregated
# here into the four carbon-balance classes (cropland, grassland, natural,
# urban) and converted to areas via the spherical 0.5-degree cell area; finer
# native cells are area-aggregated to the 0.5-degree grid. The output matches
# the land_use input contract of build_carbon_balance(): (lon, lat, area_code,
# year, land_use, fraction, area_ha) with lowercase class names.
#
# NOTE ON THE PIN PAYLOAD (verified 2026-08-04): the registered pin
# "luh2_v2h_states" ships ONE NetCDF member, "states.nc" (6.7 GB), so it must be
# fetched with type = "nc" (a path read lazily by ncdf4), never as a table. The
# superseded first version (20251006T152247Z-942cc) held the serialized HDF5
# byte stream in a ".parquet"/".csv" pair, which is what made the pin look
# unreadable; the registered version (20260701T083449Z-582d8) is a clean NetCDF.
#
# The pinned NetCDF is the LUH2-GCB2022 vintage (source_id
# "UofMD-landState-LUH2-GCB2022", 1173 yearly steps, 850-2022), NOT the base
# v2h release (1166 steps, 850-2015). Both trees exist in the wild and give
# different results, so the vintage actually read is recorded on the output via
# attach_provenance() rather than assumed. .luh2_nc_years() derives the calendar
# span from the file's own time axis, so either vintage reads correctly.
#
# Fetching the pin emits "NAs introduced by coercion: 6657587367 is out of
# integer range" from yaml via pins: the payload's byte size overflows R's
# integer, so the download progress bar loses its total. Benign, upstream, and
# not a sign the payload is bad.

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
#' The states grid is read from the registered `luh2_v2h_states` pin. The
#' `WHEP_LUH2_DIR` environment variable is only a fallback, used when the pin
#' cannot be fetched. Either way the vintage actually read (the NetCDF
#' `source_id`, e.g. `"UofMD-landState-LUH2-GCB2022"`) is recorded on the result
#' with [attach_provenance()], because the base v2h release and the annual
#' Global Carbon Budget variants cover different years and do not agree.
#'
#' @param resolution `"grid"` (default, per cell and class) or `"polity"`
#'   (aggregated to `area_code` per year and class).
#' @param years Optional integer vector of calendar years to keep. `NULL` keeps
#'   every year present in the source.
#' @param states_source Which states source to read: `"pin"` (default, the
#'   registered `luh2_v2h_states` pin, falling back to `WHEP_LUH2_DIR` with a
#'   warning when the pin cannot be fetched) or `"local"` for `WHEP_LUH2_DIR`
#'   only. Recorded in the provenance record's `input_origin`.
#' @param data Named list of pre-loaded inputs bypassing the pin read: `states`
#'   (raw per-cell-year-state fractions with `lon`, `lat`, `year`, `land_use`,
#'   `fraction`) and `country_grid` (`lon`, `lat`, `area_code`,
#'   `cell_area_frac`). Each falls back to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble with columns `lon`, `lat`, `area_code`, `year`, `land_use`,
#'   `fraction` and `area_ha` at `"grid"` resolution; at `"polity"` resolution
#'   `lon` and `lat` are dropped and `area_ha` is summed per
#'   `(area_code, year, land_use)`. When the states grid was read from a NetCDF,
#'   a provenance record naming the vintage is attached; read it back with
#'   [get_provenance()].
#' @source LUH2 v2h, Hurtt, G. C. et al. (2020). Harmonization of global land
#'   use change and management for the period 850-2100 (LUH2) for CMIP6.
#'   Geoscientific Model Development 13, 5425-5464. \doi{10.5194/gmd-13-5425-2020}.
#'   The pinned payload is the Global Carbon Budget vintage of that release:
#'   Chini, L. et al. (2021). Land-use harmonization datasets for annual global
#'   carbon budgets. Earth System Science Data 13, 4175-4189.
#'   \doi{10.5194/essd-13-4175-2021}.
#' @export
#' @examples
#' read_luh2_landuse(example = TRUE)
read_luh2_landuse <- function(
  resolution = c("grid", "polity"),
  years = NULL,
  states_source = c("pin", "local"),
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_luh2_landuse())
  }
  resolution <- rlang::arg_match(resolution)
  states_source <- rlang::arg_match(states_source)
  data <- data %||% list()
  states <- data$states %||%
    .luh2_read_states_source(years = years, states_source = states_source)
  if (!is.null(years)) {
    states <- dplyr::filter(states, .data$year %in% years)
  }
  country_grid <- data$country_grid %||% .luh2_read_country_grid()

  grid <- states |>
    .luh2_map_classes() |>
    dplyr::mutate(area_ha = .data$fraction * .luh2_cell_area_ha(.data$lat))

  .luh2_to_polity(grid, country_grid, resolution) |>
    attach_provenance(get_provenance(states))
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

# Choose the states source: the registered pin, falling back to a local LUH2
# tree only when the pin cannot be fetched. Pin-first is deliberate: the pin
# names its vintage, whereas WHEP_LUH2_DIR may point at any of the v2h trees in
# circulation, which cover different years and do not agree. states_source =
# "local" asks for the local tree outright, so it is a selectable source and not
# only a rescue path.
.luh2_read_states_source <- function(years = NULL, states_source = "pin") {
  if (states_source == "local") {
    return(.luh2_read_states_local(years = years))
  }
  states <- tryCatch(
    .luh2_read_states(years = years),
    error = function(e) {
      cli::cli_warn(c(
        "Could not read the {.val luh2_v2h_states} pin.",
        i = "Falling back to {.envvar WHEP_LUH2_DIR}.",
        "Caused by" = conditionMessage(e)
      ))
      NULL
    }
  )
  states %||% .luh2_read_states_local(years = years)
}

# The local fallback tree, WHEP_LUH2_DIR/states.nc. Aborts with the env-var
# instruction when unset or absent, so a failed pin read is never silently
# downgraded to an empty result.
.luh2_read_states_local <- function(years = NULL) {
  states_dir <- .luh2_states_dir()
  nc_path <- file.path(states_dir, "states.nc")
  if (!.has_path(states_dir) || !file.exists(nc_path)) {
    cli::cli_abort(c(
      "No LUH2 v2h states source is available.",
      i = "Set {.envvar WHEP_LUH2_DIR} to a directory holding
           {.file states.nc}, or pass {.code data$states}."
    ))
  }
  .luh2_read_states_nc(nc_path, years = years, origin = "local")
}

.luh2_states_dir <- function() {
  Sys.getenv("WHEP_LUH2_DIR", "")
}

# -- states.nc reader (pin payload and local fallback alike) ------------------

# Read the 12 LUH2 v2h state fractions for the requested years from a states.nc
# and area-aggregate the 0.25-degree native grid to the 0.5-degree carbon grid.
# Returns long (lon, lat, year, land_use, fraction) on the 0.5-degree cell
# centres, carrying the vintage record. LUH2 v2h time index 1 = year 850 CE.
.luh2_read_states_nc <- function(nc_path, years = NULL, origin = "local") {
  provenance <- .luh2_states_provenance(nc_path, origin)
  vintage <- provenance$input_source_id
  span <- paste(
    provenance$input_first_year,
    provenance$input_last_year,
    sep = "-"
  )
  cli::cli_alert_info("LUH2 v2h states ({origin}): {.val {vintage}}, {span}.")
  years <- years %||% .luh2_nc_years(nc_path)
  purrr::map_dfr(years, \(yr) .luh2_read_states_nc_year(nc_path, yr)) |>
    attach_provenance(provenance)
}

# Record which LUH2 product a states.nc actually is, extending the
# record_provenance() schema. The base v2h release (850-2015) and the annual
# Global Carbon Budget variants (850-2022 for GCB2022) are different products
# that reproduce different residual statistics, so a result must carry the one
# that produced it instead of citing the base release by assumption. The pinned
# version is only claimed when the pin was the source that was read.
.luh2_states_provenance <- function(nc_path, origin) {
  years <- .luh2_nc_years(nc_path)
  record_provenance(aliases = "luh2_v2h_states") |>
    dplyr::mutate(
      input_version = dplyr::if_else(
        origin == "pin",
        .data$input_version,
        NA_character_
      ),
      input_origin = origin,
      input_source_id = .luh2_nc_source_id(nc_path),
      input_first_year = min(years),
      input_last_year = max(years)
    )
}

# The LUH2 vintage a states.nc declares in its CF global attributes. The
# CMIP6-style releases carry "source_id" ("UofMD-landState-LUH2-GCB2022"); older
# trees only set "dataset_version_number" or "source".
.luh2_nc_source_id <- function(nc_path) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  named <- ncdf4::ncatt_get(nc, 0)[
    c("source_id", "dataset_version_number", "source")
  ]
  found <- purrr::detect(named, \(x) is.character(x) && nzchar(x))
  found %||% NA_character_
}

# Full calendar-year sequence the states.nc covers. LUH2 v2h time index 1 =
# year 850 CE, so the series spans 850 .. 850 + time_len - 1 (2015 for the base
# v2h release, 2022 for the pinned GCB2022 vintage). Derived from time_len, not
# a hardcoded end year, so every vintage reads correctly.
.luh2_nc_years <- function(nc_path) {
  seq(850L, 850L + .luh2_time_len_nc(nc_path) - 1L)
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

# Decode the LUH2 v2h states pin into per-cell-year-state fractions. The
# registered payload is a single NetCDF member, so it must be fetched with
# type = "nc" (a path, read lazily by ncdf4) rather than as a table -- asking for
# the default "parquet" is what made this pin look unreadable. A tidy tabular
# payload is still decoded, so re-uploading the pin in either shape needs no
# reader change.
.luh2_read_states <- function(years = NULL) {
  nc_path <- tryCatch(
    whep_read_file("luh2_v2h_states", type = "nc"),
    error = function(e) NULL
  )
  if (!is.null(nc_path)) {
    return(.luh2_read_states_nc(nc_path, years = years, origin = "pin"))
  }
  # A tabular payload carries no CF attributes, so only the pin version can be
  # recorded -- there is no source_id to name the vintage with.
  whep_read_file("luh2_v2h_states") |>
    .luh2_tidy_states(years) |>
    attach_provenance(record_provenance(aliases = "luh2_v2h_states"))
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
