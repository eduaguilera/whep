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
#' to convert the deposited mass into a per-hectare rate, and a polity share of
#' the cell to derive the absolute mass a polity receives.
#'
#' The cell's deposited mass is split across the polities holding the cell in
#' proportion to `polity_area_ha`, the geodesic territory each holds in it, as
#' [build_polycell_support()] measures it. The transitional alternative is
#' `polity_frac`, the subcell-count share [build_cell_polity()] carries, which
#' is quantised to 1/36 of a cell; it stays selectable so the two partitions
#' can be compared, and it is what a support table carrying no
#' `polity_area_ha` is split by. Either way the split is a share of the cell,
#' so the source mass is redistributed and never created or destroyed.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year the inputs cover.
#' @param data Optional named list of pre-loaded inputs: `nhx` and `noy`
#'   (each `lon`, `lat`, `year`, `value_g`, falling back to
#'   [read_n_deposition()] when absent) and `cell_polity` (`lon`, `lat`,
#'   `area_code`, `cell_area_ha` and the `split` key column, required).
#' @param split Which polity share splits the cell's deposited mass:
#'   `"auto"` (default) takes `polity_area_ha` when the support carries it and
#'   `polity_frac` otherwise, `"polity_area_ha"` and `"polity_frac"` demand
#'   that key and abort when it is absent. The resolved key is recorded in the
#'   `method_polity_split` output column, so a table's split is readable from
#'   the table.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `area_code`, `year`,
#'   `deposition_kgn_ha`, `deposition_n_t`, `method_deposition` and
#'   `method_polity_split`, plus the polity columns below.
#'
#'   `deposition_kgn_ha` is the whole-cell mean rate: the cell's total mass
#'   over its whole area, so every polity of a cell carries the same rate and
#'   the rate is **not** conserved on re-aggregation. Only `deposition_n_t` is
#'   a mass.
#'
#'   Rows are keyed on `area_code`. `build_polycell_support()` keys on
#'   `polity_code` and does not derive the reporting vocabulary (DA-23), and
#'   `polity_area_crosswalk` folds distinct polities into one `area_code`, so a
#'   support table must be converted to one row per cell and `area_code`
#'   **before** it is passed here. That conversion is refused rather than
#'   performed silently.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_n_deposition(example = TRUE)
build_n_deposition <- function(
  years = NULL,
  data = list(),
  split = c("auto", "polity_area_ha", "polity_frac"),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_n_deposition())
  }
  split <- rlang::arg_match(split)
  nhx <- data$nhx %||% read_n_deposition("nhx", years = years)
  noy <- data$noy %||% read_n_deposition("noy", years = years)
  nhx <- .nd_filter_years(nhx, years)
  noy <- .nd_filter_years(noy, years)
  polity <- .wb_require_input(data$cell_polity, "cell_polity", "area_code")
  key <- .nd_resolve_split(polity, split)
  .nd_assemble(nhx, noy, .nd_polity_share(polity, key), key) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers --------------------------------------------------

.nd_filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% years)
}

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
  if (length(time_idx) == 0L) {
    return(tibble::tibble(
      lon = double(),
      lat = double(),
      year = integer(),
      value_g = double()
    ))
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

# Validate the support and attach `polity_share`, the fraction of the cell's
# deposited mass each row takes, under the key `.nd_resolve_split()` settled on.
.nd_polity_share <- function(support, key) {
  .check_columns(support, c(key, "cell_area_ha"), "cell_polity")
  if (key == "polity_area_ha") {
    return(.nd_area_share(support))
  }
  # The transitional key. `polity_frac` is used exactly as supplied, with no
  # renormalisation, because the crosswalk already is a partition and a support
  # that is not must lose mass visibly rather than be repaired here.
  dplyr::mutate(support, polity_share = .data$polity_frac)
}

# "auto" takes the finest partition the support carries. An explicit choice is
# never silently downgraded: naming a key the support lacks aborts, so a caller
# that means to split geodesically cannot be handed crosswalk numbers instead.
.nd_resolve_split <- function(support, split) {
  if (split != "auto") {
    return(split)
  }
  if (rlang::has_name(support, "polity_area_ha")) {
    "polity_area_ha"
  } else {
    "polity_frac"
  }
}

# The geodesic split: each polity takes the cell's mass in proportion to the
# territory it holds in that cell (DA-10). Dividing by the cell's own total,
# never by the polity's own area, is what keeps this a partition -- a share per
# polycell's own hectares would let every polity of a shared cell recover the
# whole cell mass, emitting it once per polity.
.nd_area_share <- function(support) {
  .nd_check_area_values(support)
  .nd_check_area_key(support)
  support |>
    dplyr::mutate(
      polity_share = .data$polity_area_ha / sum(.data$polity_area_ha),
      .by = c("lon", "lat")
    )
}

# A cell whose territory sums to zero has no partition, and dividing by it
# would hand every polity of that cell an `NaN` share that later sums away to
# nothing. An `NA` area is worse still: it would silently delete one polity's
# claim while leaving the others' shares looking like a complete partition.
.nd_check_area_values <- function(support) {
  area <- support$polity_area_ha
  if (!is.numeric(area) || anyNA(area) || any(!is.finite(area) | area < 0)) {
    cli::cli_abort(
      "{.field cell_polity$polity_area_ha} must be finite and non-negative."
    )
  }
  empty <- dplyr::summarise(
    support,
    total = sum(.data$polity_area_ha),
    .by = c("lon", "lat")
  )
  if (any(empty$total <= 0)) {
    cli::cli_abort(c(
      "{sum(empty$total <= 0)} cell{?s} hold no territory to split.",
      i = "A cell with {.code sum(polity_area_ha) == 0} has no partition."
    ))
  }
}

# `build_polycell_support()` keys on `polity_code`, and `polity_area_crosswalk`
# folds distinct polities into one `area_code` (Sudan and South Sudan share
# 206) or leaves it `NA`. Deposition rows are keyed on `area_code`, so folding
# here would silently merge two territories' shares or pour a cell's mass into
# an unjoinable `NA` bucket. The conversion belongs at the caller's boundary
# (DA-23), so it is refused rather than performed.
.nd_check_area_key <- function(support) {
  dup <- support |>
    dplyr::count(.data$lon, .data$lat, .data$area_code, name = "n_rows") |>
    dplyr::filter(.data$n_rows > 1L | is.na(.data$area_code))
  if (nrow(dup) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{.arg cell_polity} must hold one row per cell and {.field area_code}.",
    x = "{nrow(dup)} cell-{.field area_code} group{?s} {?is/are} duplicated
         or {.val NA}.",
    i = "Convert {.field polity_code} to {.field area_code} before calling;
         deposition rows are keyed on {.field area_code} and will not fold
         two polities into one silently."
  ))
}

# Combine NHx + NOy mass, convert to a per-hectare rate using the true cell
# area, and split that mass across the cell's polities by `polity_share`.
.nd_assemble <- function(nhx, noy, polity, key) {
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
      # `cell_area_ha` divides here and multiplies straight back, so it
      # cancels and the whole-cell area over-count cannot reach the mass. The
      # round trip is kept, not simplified away, so that substituting a land
      # area on one side alone stays a visible break rather than a silent
      # ~10% fall in every deposition total.
      deposition_n_t = .data$deposition_kgn_ha *
        .data$cell_area_ha *
        .data$polity_share /
        1000,
      method_deposition = "hani",
      method_polity_split = key
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "deposition_kgn_ha",
      "deposition_n_t",
      "method_deposition",
      "method_polity_split"
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
    ~method_deposition, ~method_polity_split,
    -0.25, -0.25, 1L, 2020L, 15, 46.2, "hani", "polity_area_ha"
  ) |>
    .add_reporting_polity_columns()
}
