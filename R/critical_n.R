# Gridded critical-nitrogen layers from Schulte-Uebbing et al. (2022),
# doi:10.1038/s41586-022-05158-2 (data archive Zenodo
# doi:10.5281/zenodo.6395016, CC-BY-4.0).
#
# CONFIRMED FORMAT (archive inspected; see plans/sjos_n_critical_n_format.md):
# - Every layer is an ESRI ASCII grid (.asc): 6 header lines
#   (ncols nrows xllcorner yllcorner cellsize NODATA_value) then the value
#   matrix, row 1 = north (ymax). Global 0.5-degree grid, 720 x 360, EPSG:4326.
# - Units kg N ha-1 yr-1 (year-2010 snapshot), except binding_threshold which
#   is a categorical 1-8 code of the medium(s) that bind per cell.
# - Read via a small base-R parser (no terra dependency): the tested paths
#   (example, data=) never touch the filesystem, and the real read is pure
#   base R + tibble, so there is no data.table Git-Bash segfault surface.

#' Read a Schulte-Uebbing gridded critical-nitrogen layer.
#'
#' @description
#' Reads one 0.5-degree gridded critical-nitrogen layer from the
#' Schulte-Uebbing et al. (2022) archive (doi:10.5281/zenodo.6395016) onto
#' WHEP's grid: the critical nitrogen surplus, the critical nitrogen input,
#' the exceedance of the critical surplus, the three medium-specific critical
#' losses (ammonia emission, groundwater leaching, surface-water load), or the
#' pre-computed binding-threshold map. Values are in kg N per hectare per year
#' (a categorical 1-8 impact code for `binding_threshold`). The critical
#' surplus, input and exceedance are selectable by `threshold` (minimum of all
#' media, surface water, groundwater or deposition) and `land_use` (all
#' agricultural land, arable only, or intensively managed grassland); the three
#' critical losses and the binding threshold ignore `threshold`. The archive
#' directory is read from the `WHEP_CRITICAL_N_DIR` environment variable.
#'
#' @param var Which critical-nitrogen layer to read: one of
#'   `"critical_n_surplus"`, `"critical_n_input"`, `"exceedance"`,
#'   `"crit_nh3_emission"`, `"crit_leaching_gw"`, `"crit_load_sw"` or
#'   `"binding_threshold"`.
#' @param threshold Impact threshold selecting the critical value: `"mi"`
#'   (minimum across media, the collapsed boundary), `"sw"` (surface-water
#'   eutrophication), `"gw"` (groundwater nitrate) or `"de"` (atmospheric or
#'   terrestrial deposition). Ignored by the critical-loss and
#'   binding-threshold layers.
#' @param land_use Land-use scope: `"all"` (arable plus intensively managed
#'   grassland), `"ara"` (arable only) or `"igl"` (intensively managed
#'   grassland). Ignored by the critical-loss layers (`crit_nh3_emission`,
#'   `crit_leaching_gw`, `crit_load_sw`), which have a single land-use-agnostic
#'   file; used by the binding threshold and the surplus/input/exceedance
#'   layers.
#' @param dir Optional path to the archive directory, overriding
#'   `WHEP_CRITICAL_N_DIR`. Defaults to `NULL`.
#' @param data Optional pre-read tibble (`lon`, `lat`, `value`) returned
#'   directly instead of reading the archive, for tests and injection.
#'   Defaults to `NULL`.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat` (0.5-degree cell centres) and `value`
#'   (kg N per hectare per year; a categorical impact code for
#'   `binding_threshold`). NODATA cells are dropped.
#' @export
#' @examples
#' read_critical_n(example = TRUE)
read_critical_n <- function(
  var = c(
    "critical_n_surplus",
    "critical_n_input",
    "exceedance",
    "crit_nh3_emission",
    "crit_leaching_gw",
    "crit_load_sw",
    "binding_threshold"
  ),
  threshold = c("mi", "sw", "gw", "de"),
  land_use = c("all", "ara", "igl"),
  dir = NULL,
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_critical_n())
  }
  var <- rlang::arg_match(var)
  threshold <- rlang::arg_match(threshold)
  land_use <- rlang::arg_match(land_use)
  grid <- data %||%
    .read_critical_n_file(
      .resolve_critical_n_dir(dir),
      var,
      threshold,
      land_use
    )
  .critical_n_finalize(grid)
}

# ---- Private helpers --------------------------------------------------

# Resolve the critical-nitrogen archive directory from the argument, else the
# env var.
.resolve_critical_n_dir <- function(dir = NULL) {
  resolved <- dir %||% Sys.getenv("WHEP_CRITICAL_N_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No critical-nitrogen archive directory available.",
      i = "Pass {.arg dir} or set {.envvar WHEP_CRITICAL_N_DIR} to the
           extracted Zenodo archive (doi:10.5281/zenodo.6395016)."
    ))
  }
  resolved
}

# Read one critical-nitrogen layer from the extracted archive.
.read_critical_n_file <- function(dir, var, threshold, land_use) {
  spec <- .critical_n_var_spec(var, threshold, land_use)
  path <- file.path(
    dir,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances",
    "Output_files",
    spec$subdir,
    spec$file
  )
  .read_esri_asc(path)
}

# Map a layer + selectors to its archive subdirectory and .asc filename.
.critical_n_var_spec <- function(var, threshold, land_use) {
  suffix <- paste0(threshold, "_", land_use, "_ph.asc")
  switch(
    var,
    critical_n_surplus = list(
      subdir = "Critical N surpluses",
      file = paste0("nsur_crit_", suffix)
    ),
    critical_n_input = list(
      subdir = "Critical N inputs",
      file = paste0("nin_crit_", suffix)
    ),
    exceedance = list(
      subdir = "Exeedance of critical N surpluses",
      file = paste0("exc_nsur_crit_", suffix)
    ),
    crit_nh3_emission = list(
      subdir = "Critical losses",
      file = "nem_crit_ph.asc"
    ),
    crit_leaching_gw = list(
      subdir = "Critical losses",
      file = "nle_crit_ph.asc"
    ),
    crit_load_sw = list(
      subdir = "Critical losses",
      file = "nload_crit_ph.asc"
    ),
    binding_threshold = list(
      subdir = "Threshold exceedance by impact",
      file = paste0("threshold_exc_", land_use, ".asc")
    )
  )
}

# Parse an ESRI ASCII grid to a lon/lat/value tibble at cell centres,
# dropping NODATA cells.
.read_esri_asc <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Critical-nitrogen grid file not found: {.file {path}}.")
  }
  header <- .read_asc_header(path)
  values <- scan(path, skip = 6, quiet = TRUE)
  .asc_to_grid(values, header)
}

# Read the 6-line ESRI header into a named numeric vector.
.read_asc_header <- function(path) {
  lines <- readLines(path, n = 6)
  parts <- stringr::str_split(
    stringr::str_trim(lines),
    "\\s+",
    simplify = TRUE
  )
  vals <- as.numeric(parts[, 2])
  names(vals) <- stringr::str_to_lower(parts[, 1])
  vals
}

# Expand a row-major (north-first) value vector to cell-centre coordinates.
.asc_to_grid <- function(values, header) {
  ncols <- header[["ncols"]]
  nrows <- header[["nrows"]]
  cell <- header[["cellsize"]]
  col <- rep(seq_len(ncols), times = nrows)
  row <- rep(seq_len(nrows), each = ncols)
  tibble::tibble(
    lon = header[["xllcorner"]] + (col - 0.5) * cell,
    lat = header[["yllcorner"]] + (nrows - row + 0.5) * cell,
    value = values
  ) |>
    dplyr::filter(.data$value != header[["nodata_value"]])
}

# Coerce any critical-nitrogen grid to the lon/lat/value output schema.
.critical_n_finalize <- function(grid) {
  if (!all(rlang::has_name(grid, c("lon", "lat", "value")))) {
    cli::cli_abort(
      "Critical-nitrogen grid needs columns {.field lon}, {.field lat} and
       {.field value}."
    )
  }
  grid |>
    dplyr::select("lon", "lat", "value") |>
    tibble::as_tibble()
}
