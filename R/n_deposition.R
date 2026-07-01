# Gridded atmospheric nitrogen deposition from HaNi (Tian et al. 2022,
# doi:10.5194/essd-14-4551-2022).
#
# CONFIRMED HaNi FACTS (local file inspected; do not re-guess):
# - ndep_nhx.nc / ndep_noy.nc, each one variable (ndep_nhx / ndep_noy) on a
#   5-arcmin native grid: lon[4320] x lat[2160] x time[171].
# - Units are grams N deposited to land WITHIN the native cell (an extensive
#   mass, not a density) -- long_name "N{HX,OY}-N deposition to land within
#   the grid cell". Aggregating to WHEP's 0.5-degree grid must therefore SUM
#   the 6x6 fine cells per 0.5-degree block (mass is additive), never average
#   them: averaging grams-per-fine-cell and dividing by one fixed area
#   constant (as an earlier exploratory script did) silently mis-weights every
#   cell away from that reference latitude, because a 5-arcmin cell's true
#   area shrinks by cos(lat) toward the poles.
# - time units "years since 1850-01-01 00:00:00", calendar noleap, so time
#   index i (0-based) is calendar year 1850 + i.
# - Local dev data dir is read from Sys.getenv("WHEP_HANI_DIR"); never
#   hardcode an absolute path in committed code.

#' Read a HaNi atmospheric nitrogen deposition species onto WHEP's grid.
#'
#' @description
#' Reads one HaNi NHx or NOy deposition NetCDF (native 5-arcmin grid, total
#' grams N deposited per native cell per year) and aggregates it to WHEP's
#' 0.5-degree grid by summing the 6x6 fine cells inside each 0.5-degree
#' block, since the source quantity is an extensive mass. Returns the summed
#' mass per 0.5-degree cell; converting to a per-hectare rate needs the true
#' cell area and is done downstream by [build_n_deposition()].
#'
#' @param species Which HaNi species to read, `"nhx"` or `"noy"`.
#' @param hani_dir Path to the directory holding `ndep_nhx.nc` and
#'   `ndep_noy.nc`. Defaults to `Sys.getenv("WHEP_HANI_DIR")`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   reads every year present in the file.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `value_g` (total grams N
#'   deposited in the 0.5-degree cell that year).
#' @export
#' @examples
#' read_n_deposition(example = TRUE)
read_n_deposition <- function(
  species = c("nhx", "noy"),
  hani_dir = NULL,
  years = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_hani_species())
  }
  species <- rlang::arg_match(species)
  rlang::check_installed("ncdf4")
  path <- file.path(
    .resolve_hani_dir(hani_dir),
    paste0("ndep_", species, ".nc")
  )
  .read_hani_nc(path, paste0("ndep_", species), years)
}

#' Build gridded atmospheric nitrogen deposition inputs.
#'
#' @description
#' Combines HaNi NHx and NOy deposition into a total nitrogen deposition rate
#' per WHEP grid cell, using the true latitude-dependent 0.5-degree cell area
#' (from the cell-polity crosswalk) to convert the deposited mass into a
#' per-hectare rate, and the crosswalk's polity land-area share to derive the
#' absolute mass a polity receives.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year the inputs cover.
#' @param data Optional named list of pre-loaded inputs: `nhx` and `noy`
#'   (each `lon`, `lat`, `year`, `value_g`, falling back to
#'   [read_n_deposition()] when absent) and `cell_polity` (`lon`, `lat`,
#'   `area_code`, `polity_frac`, `cell_area_ha`, required).
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`,
#'   `deposition_kgn_ha`, `deposition_n_t` and `method_deposition`.
#' @export
#' @examples
#' build_n_deposition(example = TRUE)
build_n_deposition <- function(years = NULL, data = list(), example = FALSE) {
  if (isTRUE(example)) {
    return(.example_n_deposition())
  }
  nhx <- data$nhx %||% read_n_deposition("nhx", years = years)
  noy <- data$noy %||% read_n_deposition("noy", years = years)
  polity <- .wb_require_input(
    data$cell_polity,
    "cell_polity",
    c("area_code", "polity_frac", "cell_area_ha")
  )
  .nd_assemble(nhx, noy, polity)
}

# ---- Private helpers --------------------------------------------------

# Resolve the HaNi data directory from the argument, else the env var.
.resolve_hani_dir <- function(hani_dir) {
  resolved <- hani_dir %||% Sys.getenv("WHEP_HANI_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No HaNi deposition directory available.",
      i = "Pass {.arg hani_dir} or set {.envvar WHEP_HANI_DIR}."
    ))
  }
  resolved
}

# Read one HaNi NetCDF, aggregating the native 5-arcmin grid to WHEP's
# 0.5-degree grid by summing the 36 fine cells per block (mass-conservative;
# see the file-header note on why this must be a sum, not a mean). Reads and
# aggregates one requested year at a time to bound memory on the ~9.3M-cell
# native grid.
.read_hani_nc <- function(path, netcdf_var, years) {
  if (!file.exists(path)) {
    cli::cli_abort("HaNi deposition file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  first_year <- 1850L
  n_time <- nc$dim[["time"]]$len
  available_years <- first_year + seq_len(n_time) - 1L
  time_idx <- if (is.null(years)) {
    seq_len(n_time)
  } else {
    which(available_years %in% years)
  }
  layers <- lapply(time_idx, function(ti) {
    .read_hani_year(nc, netcdf_var, lon, lat, ti, available_years[ti])
  })
  data.table::rbindlist(layers) |> tibble::as_tibble()
}

# Read one native-grid time slab and block-sum it to the 0.5-degree grid.
.read_hani_year <- function(nc, netcdf_var, lon, lat, time_idx, year) {
  slab <- ncdf4::ncvar_get(
    nc,
    netcdf_var,
    start = c(1, 1, time_idx),
    count = c(-1, -1, 1)
  )
  dt <- data.table::data.table(
    lon_block = .hani_block_center(lon)[rep(
      seq_along(lon),
      times = length(lat)
    )],
    lat_block = rep(.hani_block_center(lat), each = length(lon)),
    value_g = as.vector(slab)
  )
  dt[,
    .(year = year, value_g = sum(value_g, na.rm = TRUE)),
    by = .(lon = lon_block, lat = lat_block)
  ]
}

# Map native-grid coordinates to their enclosing 0.5-degree cell center.
.hani_block_center <- function(coord) {
  floor(coord / 0.5) * 0.5 + 0.25
}

# Combine NHx + NOy mass, convert to a per-hectare rate using the crosswalk's
# true cell area, and derive the polity-share mass.
.nd_assemble <- function(nhx, noy, polity) {
  total <- dplyr::full_join(
    nhx,
    noy,
    by = c("lon", "lat", "year"),
    suffix = c("_nhx", "_noy")
  ) |>
    dplyr::mutate(
      value_g_total = dplyr::coalesce(.data$value_g_nhx, 0) +
        dplyr::coalesce(.data$value_g_noy, 0)
    )
  dplyr::inner_join(total, polity, by = c("lon", "lat")) |>
    dplyr::mutate(
      deposition_kgn_ha = .data$value_g_total / 1000 / .data$cell_area_ha,
      deposition_n_t = .data$deposition_kgn_ha *
        .data$cell_area_ha *
        .data$polity_frac /
        1000,
      method_deposition = "hani"
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "deposition_kgn_ha",
      "deposition_n_t",
      "method_deposition"
    )
}

# Toy fixture for a runnable example (one native-grid deposition species).
.example_hani_species <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~value_g,
    -0.25, -0.25, 2020L, 30800000
  )
}

# Toy fixture for the runnable example (one cell, one polity, one year).
.example_n_deposition <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~deposition_kgn_ha, ~deposition_n_t,
    ~method_deposition,
    -0.25, -0.25, 1L, 2020L, 15, 46.2, "hani"
  )
}
