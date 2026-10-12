# IMAGE-GNM's two 0.5-degree surface runoff layers, from the supplement of
# Beusen et al. (2015), Geosci. Model Dev. 8:4045-4067
# (doi:10.5194/gmd-8-4045-2015-supplement, CC BY 3.0 per its title page).
#
# - water_input/pcrglobwb_100/fQsro.map: f_qsro, the fraction of total runoff
#   that runs off the surface, f_qsro(slope) f_qsro(texture) of eqs. 3-4
#   (p. 4049). The authors' N model reads this grid (parameters_history_100.ini
#   line 51; N-model.py line 203) and never computes eq. 4 itself.
# - fix_input/slope.map: the median slope in m/km of each 0.5-degree cell,
#   read by the erosion term (n_erosion.py lines 20-21).
#
# Both are ESRI ASCII grids, 720 x 360 at 0.5 degrees from -180, -90, row 1
# north. fQsro.map has a NODATA line (-1); slope.map has none, so every value
# of it is data. The paper's eq. 4 applied to slope.map does not reproduce
# fQsro.map (the medians differ by a factor of about 1.6 on gentle slopes),
# so the fraction is read, never recomputed.
#
# Third-party data with a stable DOI: downloaded on demand, verified against
# the checksums below and cached under rappdirs::user_cache_dir("whep"), with
# WHEP_IMAGE_GNM_DIR as the override. No WHEP-built layer derives from it, so
# nothing here belongs in a pin.

#' Read IMAGE-GNM's surface runoff fraction and slope layers.
#'
#' @description
#' Reads the two 0.5-degree layers IMAGE-GNM's surface runoff nitrogen needs,
#' as its authors published them with Beusen et al. (2015)
#' (doi:10.5194/gmd-8-4045-2015-supplement):
#'
#' - `surface_runoff_fraction`: `f_qsro`, the share of total runoff that runs
#'   off the surface, the product `f_qsro(slope) f_qsro(texture)` of eqs. 3-4
#'   (p. 4049), the median over the 90 m cells of each 0.5-degree cell. Their
#'   model reads it from `water_input/pcrglobwb_100/fQsro.map` rather than
#'   computing eq. 4, and it is read here the same way; the land-use factor
#'   of eq. 3 is applied by [calculate_n_leaching()].
#' - `slope_m_per_km`: the median slope (m/km) of each cell, from
#'   `fix_input/slope.map`, which their erosion term reads.
#'
#' Only cells where `fQsro.map` holds a value (its land mask) are returned.
#' A balance row on a cell outside it gets no value from a join and
#' [calculate_n_leaching()] aborts there rather than run nothing off.
#'
#' The supplement is resolved from `dir`, else the `WHEP_IMAGE_GNM_DIR`
#' environment variable, else a local cache populated on first use: the
#' 59.7 MB archive is downloaded from its DOI, verified against its SHA-256
#' and the two layers unpacked. The archive's publisher prints no checksum,
#' so the SHA-256 is the one WHEP recorded on 2026-10-12;
#' `inst/scripts/download/download_image_gnm.R` fetches the same archive
#' into a directory for `WHEP_IMAGE_GNM_DIR`.
#'
#' @param dir Optional directory holding the unpacked supplement (the folder
#'   that contains `GMD_beusen_supplement/`), overriding
#'   `WHEP_IMAGE_GNM_DIR`. Defaults to `NULL`.
#' @param verify If `TRUE` (default), each layer is checked against its
#'   recorded size and SHA-256 before it is read.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat` (0.5-degree cell centres),
#'   `surface_runoff_fraction` (0-1), `slope_m_per_km` and
#'   `source_image_gnm`, the supplement's DOI.
#' @source Beusen, A. H. W., Van Beek, L. P. H., Bouwman, A. F., Mogollon,
#'   J. M. and Middelburg, J. J. (2015). Coupling global models for
#'   hydrology and nutrient loading to simulate nitrogen and phosphorus
#'   retention in surface water - description of IMAGE-GNM and analysis of
#'   performance. Geosci. Model Dev. 8, 4045-4067.
#'   \doi{10.5194/gmd-8-4045-2015}; supplement
#'   \doi{10.5194/gmd-8-4045-2015-supplement}.
#' @export
#' @examples
#' read_image_gnm_runoff(example = TRUE)
read_image_gnm_runoff <- function(dir = NULL, verify = TRUE, example = FALSE) {
  if (isTRUE(example)) {
    return(.example_image_gnm_runoff())
  }
  root <- file.path(.resolve_image_gnm_dir(dir), .gnm_archive_root())
  if (isTRUE(verify)) {
    .gnm_verify_layers(root)
  }
  fraction <- .gnm_read_layer(root, "fraction")
  slope <- .gnm_read_layer(root, "slope")
  .gnm_check_same_grid(fraction$header, slope$header)
  fraction$cells |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::rename(surface_runoff_fraction = "value") |>
    dplyr::left_join(
      dplyr::rename(slope$cells, slope_m_per_km = "value"),
      by = c("lon", "lat")
    ) |>
    dplyr::mutate(source_image_gnm = .gnm_source_doi())
}

# ---- Private helpers: the IMAGE-GNM supplement ------------------------------

.gnm_source_doi <- function() "doi:10.5194/gmd-8-4045-2015-supplement"

# The DOI resolves to the archive itself on the publisher's site.
.gnm_archive_url <- function() {
  "https://doi.org/10.5194/gmd-8-4045-2015-supplement"
}

# Recorded on 2026-10-12 from the archive the DOI serves (Last-Modified
# 2020-06-23); the publisher prints no checksum of its own.
.gnm_archive_sha256 <- function() {
  "6824ddb8f0ad760c96980fc0ab4a07cec16163fc547acc3b9785337d07743f43"
}

.gnm_archive_bytes <- function() 59659953

.gnm_archive_root <- function() "GMD_beusen_supplement"

# The two layers read, relative to the archive root, with the size and
# SHA-256 of the files the recorded archive unpacks.
.gnm_layer_manifest <- function() {
  tibble::tribble(
    ~layer, ~relative_path, ~bytes, ~sha256,
    "fraction", "water_input/pcrglobwb_100/fQsro.map", 1206507,
    "e488c5832865913f02f7ef14a937bed43f2cdc68defd8704e8d6e2e65771d910",
    "slope", "fix_input/slope.map", 3888816,
    "d403365eea10fae7f52155dd1e3582946fceec2046df73c53f7a27884a38ec2d"
  )
}

.resolve_image_gnm_dir <- function(dir = NULL) {
  resolved <- dir %||% Sys.getenv("WHEP_IMAGE_GNM_DIR")
  if (.has_path(resolved)) {
    return(resolved)
  }
  .gnm_cached_dir()
}

.gnm_cache_dir <- function() {
  file.path(rappdirs::user_cache_dir("whep"), "image_gnm")
}

.gnm_layers_present <- function(dir) {
  paths <- file.path(
    dir,
    .gnm_archive_root(),
    .gnm_layer_manifest()$relative_path
  )
  all(file.exists(paths))
}

# The cache directory holding the unpacked layers, downloading and unpacking
# the archive on first use. `download` and `extract` are injected so the
# cache-hit and failure paths are testable without the network.
.gnm_cached_dir <- function(
  dir = .gnm_cache_dir(),
  download = .gnm_download,
  extract = .gnm_extract
) {
  if (.gnm_layers_present(dir)) {
    return(dir)
  }
  archive <- download(dir)
  extract(archive, dir)
  if (!.gnm_layers_present(dir)) {
    cli::cli_abort(
      c(
        "The IMAGE-GNM supplement did not unpack its runoff layers.",
        i = "Expected {.path {(.gnm_layer_manifest()$relative_path)}} under
             {.path {file.path(dir, .gnm_archive_root())}}."
      ),
      class = "whep_image_gnm_source"
    )
  }
  dir
}

# Fetch the archive into `dir` and verify it. A file already there that
# matches is reused.
.gnm_download <- function(dir) {
  path <- file.path(dir, "gmd-8-4045-2015-supplement.zip")
  if (.gnm_archive_ok(path)) {
    return(path)
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  cli::cli_alert_info(
    "Downloading the IMAGE-GNM supplement (59.7 MB) from its DOI..."
  )
  url <- .gnm_archive_url()
  .gnm_fetch(url, path)
  if (!.gnm_archive_ok(path)) {
    cli::cli_abort(
      c(
        "The downloaded IMAGE-GNM supplement does not match its recorded
         SHA-256.",
        i = "Download {.url {url}} by hand, unzip it, and point
             {.envvar WHEP_IMAGE_GNM_DIR} at the folder holding
             {.path {(.gnm_archive_root())}}."
      ),
      class = "whep_image_gnm_source"
    )
  }
  path
}

# The network call alone, so a test can replace it.
.gnm_fetch <- function(url, path) {
  withr::local_options(timeout = max(600, getOption("timeout")))
  utils::download.file(url, path, mode = "wb", quiet = TRUE)
  invisible(path)
}

.gnm_archive_ok <- function(path) {
  file.exists(path) &&
    identical(as.numeric(file.info(path)$size), .gnm_archive_bytes()) &&
    identical(unname(tools::sha256sum(path)), .gnm_archive_sha256())
}

# Unpack only the two layers the reader uses.
.gnm_extract <- function(archive, dir) {
  entries <- file.path(.gnm_archive_root(), .gnm_layer_manifest()$relative_path)
  utils::unzip(archive, files = entries, exdir = dir)
  invisible(dir)
}

.gnm_verify_layers <- function(root) {
  manifest <- .gnm_layer_manifest()
  paths <- file.path(root, manifest$relative_path)
  .gnm_check_layer_files(paths)
  ok <- as.numeric(file.info(paths)$size) == manifest$bytes &
    unname(tools::sha256sum(paths)) == manifest$sha256
  if (!all(ok)) {
    cli::cli_abort(
      c(
        "{sum(!ok)} IMAGE-GNM layer{?s} failed content verification:
         {.path {manifest$relative_path[!ok]}}.",
        i = "Use the unmodified supplement of Beusen et al. (2015),
             {.url {(.gnm_archive_url())}}."
      ),
      class = "whep_image_gnm_source"
    )
  }
  invisible(TRUE)
}

.gnm_check_layer_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "IMAGE-GNM layer file{?s} not found: {.path {missing}}.",
        i = "Point {.arg dir} or {.envvar WHEP_IMAGE_GNM_DIR} at the folder
             holding {.path {(.gnm_archive_root())}}, or leave both unset to
             download the supplement."
      ),
      class = c("whep_image_gnm_source", "whep_absent_input")
    )
  }
  invisible(paths)
}

# One layer as its header and a lon/lat/value tibble at cell centres, with
# the NODATA value, where the header declares one, read as missing.
.gnm_read_layer <- function(root, layer) {
  manifest <- .gnm_layer_manifest()
  path <- file.path(root, manifest$relative_path[manifest$layer == layer])
  .gnm_check_layer_files(path)
  header <- .gnm_asc_header(path)
  values <- scan(path, skip = length(header), quiet = TRUE)
  nodata <- header["nodata_value"]
  if (!is.na(nodata)) {
    values[values == nodata] <- NA_real_
  }
  list(header = header, cells = .gnm_asc_cells(values, header))
}

# The ESRI header: the leading lines that open with a header keyword. The
# authors' slope.map has five (no NODATA line), fQsro.map six.
.gnm_asc_header <- function(path) {
  keys <- c(
    "ncols",
    "nrows",
    "xllcorner",
    "yllcorner",
    "cellsize",
    "nodata_value"
  )
  parts <- readLines(path, n = 6L) |>
    stringr::str_trim() |>
    stringr::str_split_fixed("\\s+", 2L)
  first <- stringr::str_to_lower(parts[, 1])
  n_header <- match(FALSE, first %in% keys, nomatch = 7L) - 1L
  rlang::set_names(
    as.numeric(parts[seq_len(n_header), 2]),
    first[seq_len(n_header)]
  )
}

# Expand a row-major, north-first value vector to cell-centre coordinates.
.gnm_asc_cells <- function(values, header) {
  ncols <- header[["ncols"]]
  nrows <- header[["nrows"]]
  cell <- header[["cellsize"]]
  col <- rep(seq_len(ncols), times = nrows)
  row <- rep(seq_len(nrows), each = ncols)
  tibble::tibble(
    lon = header[["xllcorner"]] + (col - 0.5) * cell,
    lat = header[["yllcorner"]] + (nrows - row + 0.5) * cell,
    value = values
  )
}

# The two layers are joined cell by cell, so they must share one grid.
.gnm_check_same_grid <- function(a, b) {
  keys <- c("ncols", "nrows", "xllcorner", "yllcorner", "cellsize")
  if (!isTRUE(all.equal(a[keys], b[keys]))) {
    cli::cli_abort(
      "The IMAGE-GNM runoff fraction and slope layers are on different
       grids.",
      class = "whep_image_gnm_source"
    )
  }
  invisible(TRUE)
}
