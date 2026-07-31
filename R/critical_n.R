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
#' directory comes from `dir`, else the `WHEP_CRITICAL_N_DIR` environment
#' variable, else a local cache that is populated by downloading the archive
#' from Zenodo on first use (see `dir`).
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
#'   `WHEP_CRITICAL_N_DIR`. Defaults to `NULL`, in which case the archive is
#'   resolved as: `dir`, then `WHEP_CRITICAL_N_DIR`, then a local cache under
#'   `rappdirs::user_cache_dir("whep")`. When the cache is empty the 18.4 MB
#'   CC-BY-4.0 Zenodo archive is downloaded, verified against its published
#'   MD5 and unpacked there on first use, so a plain `read_critical_n()` call
#'   works with nothing configured. Unpacking needs a 7-Zip extractor: the
#'   `archive` package (system libarchive) or a `7z` binary on `PATH`. With
#'   neither, the call aborts after the download, naming the command to run.
#' @param data Optional pre-read tibble (`lon`, `lat`, `value`) returned
#'   directly instead of reading the archive, for tests and injection.
#'   Defaults to `NULL`.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat` (0.5-degree cell centres), `value`
#'   (kg N per hectare per year; a categorical impact code for
#'   `binding_threshold`) and retained layer provenance: `critical_var`,
#'   `critical_threshold`, `critical_land_use`, `critical_year` and
#'   `critical_source`. NODATA cells are dropped.
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
  var <- rlang::arg_match(var)
  threshold <- rlang::arg_match(threshold)
  land_use <- rlang::arg_match(land_use)
  grid <- if (isTRUE(example)) {
    .example_critical_n()
  } else {
    data %||%
      .read_critical_n_file(
        .resolve_critical_n_dir(dir),
        var,
        threshold,
        land_use
      )
  }
  .critical_n_finalize(grid, var, threshold, land_use)
}

# ---- Private helpers --------------------------------------------------

# Resolve the critical-nitrogen archive directory: an explicit argument, else
# the env var, else the local cache (populated by fetching the Zenodo archive
# on first use, the same download-on-demand pattern .provinces_shapefile()
# uses in R/natural_earth.R). The archive is 18.4 MB and CC-BY-4.0, so it is
# small enough and freely enough licensed to fetch rather than require by hand.
.resolve_critical_n_dir <- function(dir = NULL) {
  resolved <- dir %||% Sys.getenv("WHEP_CRITICAL_N_DIR")
  if (.has_path(resolved)) {
    return(resolved)
  }
  .critn_cached_dir()
}

.critn_cache_dir <- function() {
  file.path(rappdirs::user_cache_dir("whep"), "critical_n")
}

# The single .7z asset of Zenodo record 6395016 (Schulte-Uebbing et al. 2022,
# CC-BY-4.0) and its published MD5, both read off the record's API metadata.
.critn_archive_url <- function() {
  paste0(
    "https://zenodo.org/api/records/6395016/files/",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances.7z/content"
  )
}

.critn_archive_md5 <- function() "d6b4bf88e9b140bd25a147396e371733"

# The single top-level directory the archive unpacks into. Its presence under
# <cache>/extracted is what .read_critical_n_file() then reads through, so it
# doubles as the cache-hit marker.
.critn_archive_root <- function() {
  "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances"
}

# Return the cache directory holding the extracted archive, downloading and
# unpacking it on first use. `download`/`extract` are injected so the cache-hit
# and failure paths are testable without touching the network.
.critn_cached_dir <- function(
  dir = .critn_cache_dir(),
  download = .critn_download,
  extract = .critn_extract
) {
  if (dir.exists(file.path(dir, "extracted", .critn_archive_root()))) {
    return(dir)
  }
  archive <- download(dir)
  extract(archive, file.path(dir, "extracted"))
  if (!dir.exists(file.path(dir, "extracted", .critn_archive_root()))) {
    cli::cli_abort(c(
      "The critical-nitrogen archive did not unpack as expected.",
      i = "Expected {.path {file.path('extracted', .critn_archive_root())}}
           under {.path {dir}}."
    ))
  }
  dir
}

# Fetch the .7z into the cache and verify it against the published MD5. An
# already-downloaded file that still matches is reused, so a failed extraction
# does not re-download 18.4 MB.
.critn_download <- function(dir) {
  path <- file.path(dir, "critical_n_archive.7z")
  if (.critn_md5_ok(path)) {
    return(path)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_alert_info(
    "Downloading the critical-nitrogen archive (18.4 MB) from Zenodo..."
  )
  ok <- tryCatch(
    utils::download.file(
      .critn_archive_url(),
      path,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(e) e
  )
  if (inherits(ok, "error") || !.critn_md5_ok(path)) {
    cli::cli_abort(c(
      "Could not download the critical-nitrogen archive.",
      x = if (inherits(ok, "error")) {
        conditionMessage(ok)
      } else {
        "The downloaded file does not match the published MD5."
      },
      i = "Download {.url {.critn_archive_url()}} by hand, extract it, and
           point {.envvar WHEP_CRITICAL_N_DIR} at the result."
    ))
  }
  path
}

.critn_md5_ok <- function(path) {
  file.exists(path) &&
    identical(unname(tools::md5sum(path)), .critn_archive_md5())
}

# Unpack the .7z with whatever extractor the machine has: the archive package
# (libarchive) first, then a 7-Zip binary on PATH. Neither is a hard dependency
# of the package, so when both are missing this aborts -- but only after the
# archive is already downloaded and checksum-verified, so the message can name
# the one command left to run.
.critn_extract <- function(archive, exdir) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  if (rlang::is_installed("archive")) {
    archive::archive_extract(archive, dir = exdir)
    return(invisible(exdir))
  }
  bin <- .critn_7z_binary()
  if (!is.null(bin)) {
    status <- system2(
      bin,
      c("x", "-y", shQuote(archive), paste0("-o", exdir)),
      stdout = FALSE,
      stderr = FALSE
    )
    if (identical(as.integer(status), 0L)) {
      return(invisible(exdir))
    }
  }
  cli::cli_abort(c(
    "No 7-Zip extractor available for the critical-nitrogen archive.",
    i = "It is already downloaded and MD5-verified at {.path {archive}}.",
    i = "Run {.code 7z x {basename(archive)} -o{basename(exdir)}} there, or
         install the {.pkg archive} R package (needs system libarchive), or
         set {.envvar WHEP_CRITICAL_N_DIR} to an archive extracted elsewhere."
  ))
}

.critn_7z_binary <- function() {
  found <- Sys.which(c("7z", "7za", "7zr", "7zz"))
  found <- found[nzchar(found)]
  if (length(found) == 0L) NULL else unname(found[[1L]])
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

# Coerce any critical-nitrogen grid to the output schema while retaining the
# selectors that identify the physical layer. Dropping these fields permits a
# critical-input/arable grid to be silently relabelled as surplus/all
# downstream.
.critical_n_finalize <- function(grid, var, threshold, land_use) {
  if (!all(rlang::has_name(grid, c("lon", "lat", "value")))) {
    cli::cli_abort(
      "Critical-nitrogen grid needs columns {.field lon}, {.field lat} and
       {.field value}."
    )
  }
  grid |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      value = .data$value,
      critical_var = var,
      critical_threshold = dplyr::if_else(
        var %in% c("critical_n_surplus", "critical_n_input", "exceedance"),
        threshold,
        NA_character_
      ),
      critical_land_use = dplyr::if_else(
        var %in%
          c(
            "critical_n_surplus",
            "critical_n_input",
            "exceedance",
            "binding_threshold"
          ),
        land_use,
        NA_character_
      ),
      critical_year = 2010L,
      critical_source = "Schulte-Uebbing et al. (2022)"
    ) |>
    tibble::as_tibble()
}
