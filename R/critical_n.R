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
#' @param verify_source If `TRUE` (default), real archive reads verify the
#'   selected critical raster and its source-area/IMAGE support rasters against
#'   the package's versioned content manifest before parsing. Ignored for
#'   `data` and `example` injection.
#' @return A tibble with `lon`, `lat` (0.5-degree cell centres), `value`
#'   (kg N per hectare per year; a categorical impact code for
#'   `binding_threshold`) and retained layer provenance: `critical_var`,
#'   `critical_threshold`, `critical_land_use`, `critical_year` and
#'   `critical_source`, canonical integer `cell_id`/row/column keys, deposited
#'   `source_area_ha`, IMAGE-region membership, DOI/version and archive checksum.
#'   NODATA cells are dropped.
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
  example = FALSE,
  verify_source = TRUE
) {
  var <- rlang::arg_match(var)
  threshold <- rlang::arg_match(threshold)
  land_use <- rlang::arg_match(land_use)
  resolved_dir <- NULL
  grid <- if (isTRUE(example)) {
    .example_critical_n()
  } else if (!is.null(data)) {
    data
  } else {
    resolved_dir <- .resolve_critical_n_dir(dir)
    if (
      isTRUE(verify_source) &&
        var %in% c("critical_n_surplus", "critical_n_input") &&
        .critn_has_source_geometry(resolved_dir, var, threshold, land_use)
    ) {
      .critn_verify_selected(resolved_dir, var, threshold, land_use)
    }
    .read_critical_n_file(resolved_dir, var, threshold, land_use)
  }
  if (
    !is.null(resolved_dir) &&
      var %in% c("critical_n_surplus", "critical_n_input")
  ) {
    grid <- .critical_n_attach_support(grid, resolved_dir, land_use)
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

.critn_archive_sha256 <- function() {
  "74dc623f86b97c11be3269f762f6577e637559c0969000ae5b89ed6d53cacf91"
}

.critn_archive_bytes <- function() 18376996

.critn_source_doi <- function() "10.5281/zenodo.6395016"

.critn_source_version <- function() "1.0"

# The single top-level directory the archive unpacks into. Its presence under
# <cache>/extracted is what .read_critical_n_file() then reads through, so it
# doubles as the cache-hit marker.
.critn_archive_root <- function() {
  "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances"
}

.critn_root_path <- function(dir) {
  file.path(dir, "extracted", .critn_archive_root())
}

.critn_manifest <- function() {
  path <- system.file(
    "extdata",
    "critical_n_source_manifest.csv",
    package = "whep"
  )
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "critical_n_source_manifest.csv")
  }
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE) |>
    tibble::as_tibble()
}

.critn_selected_paths <- function(var, threshold, land_use) {
  spec <- .critical_n_var_spec(var, threshold, land_use)
  paths <- c(
    file.path("Output_files", spec$subdir, spec$file),
    "Input_files/a_crop.asc",
    "Input_files/a_gr_int.asc",
    "Input_files/image_region28.asc"
  )
  chartr("\\", "/", paths)
}

.critn_verify_selected <- function(dir, var, threshold, land_use) {
  wanted <- .critn_selected_paths(var, threshold, land_use)
  manifest <- .critn_manifest()
  expected <- dplyr::filter(manifest, .data$relative_path %in% .env$wanted)
  if (!setequal(expected$relative_path, wanted)) {
    cli::cli_abort("The critical-N source manifest is incomplete.")
  }
  root <- .critn_root_path(dir)
  purrr::pwalk(
    list(expected$relative_path, expected$bytes, expected$md5, expected$sha256),
    \(relative_path, bytes, md5, sha256) {
      .critn_verify_one(
        file.path(root, relative_path),
        relative_path,
        bytes,
        md5,
        sha256
      )
    }
  )
  invisible(TRUE)
}

.critn_verify_one <- function(path, relative_path, bytes, md5, sha256) {
  if (!file.exists(path)) {
    cli::cli_abort("Manifest-pinned source file is missing: {.file {path}}.")
  }
  matches <- identical(
    as.numeric(unname(file.info(path)$size)),
    as.numeric(bytes)
  ) &&
    identical(unname(tools::md5sum(path)), md5) &&
    identical(unname(tools::sha256sum(path)), sha256)
  if (!matches) {
    cli::cli_abort(c(
      "A Schulte-Uebbing source raster failed content verification.",
      x = "File: {.file {relative_path}}.",
      i = "Use the unmodified Zenodo record 6395016 archive."
    ))
  }
  invisible(TRUE)
}

.critn_has_source_geometry <- function(dir, var, threshold, land_use) {
  spec <- .critical_n_var_spec(var, threshold, land_use)
  path <- file.path(
    .critn_root_path(dir),
    "Output_files",
    spec$subdir,
    spec$file
  )
  if (!file.exists(path)) {
    return(FALSE)
  }
  header <- .read_asc_header(path)
  isTRUE(all.equal(unname(header[["ncols"]]), 720)) &&
    isTRUE(all.equal(unname(header[["nrows"]]), 360)) &&
    isTRUE(all.equal(unname(header[["xllcorner"]]), -180)) &&
    isTRUE(all.equal(unname(header[["yllcorner"]]), -90)) &&
    isTRUE(all.equal(unname(header[["cellsize"]]), 0.5))
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
  # Held in a local: cli reads `{.critn_archive_url()}` as a style, not a
  # substitution, and aborts inside its own error message.
  url <- .critn_archive_url()
  ok <- tryCatch(
    utils::download.file(
      url,
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
      i = "Download {.url {url}} by hand, extract it, and
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
    return(.critn_extract_archive(archive, exdir))
  }
  bin <- .critn_7z_binary()
  if (!is.null(bin) && .critn_extract_7z(archive, exdir, bin)) {
    return(invisible(exdir))
  }
  cli::cli_abort(c(
    "No 7-Zip extractor available for the critical-nitrogen archive.",
    i = "It is already downloaded and MD5-verified at {.path {archive}}.",
    i = "Run {.code 7z x {basename(archive)} -o{basename(exdir)}} there, or
         install the {.pkg archive} R package (needs system libarchive), or
         set {.envvar WHEP_CRITICAL_N_DIR} to an archive extracted elsewhere."
  ))
}

# The libarchive back-end. Split out so a test can drive it directly instead
# of having to arrange for `archive` to be the extractor .critn_extract()
# happens to pick.
.critn_extract_archive <- function(archive, exdir) {
  archive::archive_extract(archive, dir = exdir)
  invisible(exdir)
}

# The 7-Zip binary back-end; TRUE when the binary reported success. Both paths
# are quoted: an unquoted `-o` output path splits at the first space, and 7-Zip
# then reads the tail as a member filter, extracts nothing and still exits 0.
.critn_extract_7z <- function(archive, exdir, bin = .critn_7z_binary()) {
  status <- system2(
    bin,
    c("x", "-y", shQuote(archive), paste0("-o", shQuote(exdir))),
    stdout = FALSE,
    stderr = FALSE
  )
  identical(as.integer(status), 0L)
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
    .critn_archive_root(),
    "Output_files",
    spec$subdir,
    spec$file
  )
  .read_esri_asc(path)
}

.critical_n_attach_support <- function(grid, dir, land_use) {
  root <- .critn_root_path(dir)
  area <- .critical_n_source_area(root, land_use)
  image <- .critn_image_region(root)
  keyed <- .nbx_add_cell_key(grid, "deposited critical-N raster")
  keyed |>
    dplyr::left_join(area, by = "cell_id", relationship = "many-to-one") |>
    dplyr::left_join(image, by = "cell_id", relationship = "many-to-one") |>
    dplyr::mutate(image_region = as.integer(.data$image_region))
}

# The IMAGE region of each archive cell (`Input_files/image_region28.asc`),
# keyed on the canonical cell id: `cell_id`, `image_region` (double, as read).
.critn_image_region <- function(root) {
  .read_esri_asc(file.path(
    root,
    "Input_files",
    "image_region28.asc"
  )) |>
    dplyr::rename(image_region = value) |>
    .nbx_add_cell_key("deposited IMAGE-region raster") |>
    dplyr::select("cell_id", "image_region")
}

.critical_n_source_area <- function(root, land_use) {
  read_area <- function(file) {
    .read_esri_asc(file.path(root, "Input_files", file)) |>
      .nbx_add_cell_key("deposited source-area raster") |>
      dplyr::transmute(cell_id = .data$cell_id, source_area_ha = .data$value)
  }
  crop <- read_area("a_crop.asc")
  if (land_use == "ara") {
    return(crop)
  }
  grass <- read_area("a_gr_int.asc")
  if (land_use == "igl") {
    return(grass)
  }
  dplyr::full_join(
    dplyr::transmute(
      crop,
      cell_id = .data$cell_id,
      crop_ha = .data$source_area_ha
    ),
    dplyr::transmute(
      grass,
      cell_id = .data$cell_id,
      grass_ha = .data$source_area_ha
    ),
    by = "cell_id",
    relationship = "one-to-one"
  ) |>
    dplyr::transmute(
      cell_id = .data$cell_id,
      source_area_ha = dplyr::coalesce(.data$crop_ha, 0) +
        dplyr::coalesce(.data$grass_ha, 0)
    )
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
  if (!rlang::has_name(grid, "cell_id")) {
    grid <- .nbx_add_cell_key(grid, "critical-N grid")
  }
  if (!rlang::has_name(grid, "source_area_ha")) {
    grid$source_area_ha <- NA_real_
  }
  if (!rlang::has_name(grid, "image_region")) {
    grid$image_region <- NA_integer_
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
      critical_source = "Schulte-Uebbing et al. (2022)",
      cell_id = .data$cell_id,
      source_row = .data$source_row,
      source_col = .data$source_col,
      source_area_ha = .data$source_area_ha,
      image_region = as.integer(.data$image_region),
      critical_source_doi = .critn_source_doi(),
      critical_source_version = .critn_source_version(),
      archive_md5 = .critn_archive_md5()
    ) |>
    tibble::as_tibble()
}

# ---- IMAGE 2010 extensive-grassland N budget ---------------------------

# Total agricultural NH3 emission per cell, Schulte-Uebbing, L. F., Beusen,
# A. H. W., Bouwman, A. F. & de Vries, W. (2022), Nature,
# doi:10.1038/s41586-022-05158-2, Supplementary Information, Supplementary
# Table 4 row 5:
#   NH3,tot = NH3,fer_ara + NH3,fer_igl + NH3,spr_ara + NH3,spr_igl +
#             NH3,spr_egl + NH3,graz_igl + NH3,graz_egl + NH3,stor
# The archive (Zenodo doi:10.5281/zenodo.6395016 v1.0, `Input_files/`) ships
# whole-cell NH3 layers and their per-land-use parts. Measured over all 66,222
# cells with data (2026-09-24), kg N per cell per year:
# - nh3_graz == nh3_graz_int + nh3_graz_ext: max cell difference 0 kg,
#   6.8801 Tg both ways;
# - nh3_spread_fe == _crops + _grass_int + _grass_ext: max cell difference
#   1.2e-10 kg, 13.6298 Tg both ways; nh3_spread_fe_grass_ext is zero in
#   every cell, consistent with row 5 having no NH3,fer_egl term;
# - nh3_spread_man == _crops + _grass_int + _grass_ext: max cell difference
#   2.9e-11 kg, 6.7914 Tg both ways.
# The whole-cell sum below therefore equals the row-5 sum of parts to within
# 3.7e-9 kg per cell (37.7443 Tg globally, of which nh3_stor 10.4430 Tg), and
# the whole-cell layers are used. A missing layer value stays NA.
.critical_n_nh3_tot <- function(graz, spread_fe, spread_man, stor) {
  graz + spread_fe + spread_man + stor
}

# Read one grassland-intensity archive raster and key it to the canonical
# WHEP grid, keeping only cell_id and the renamed value column.
.critn_grassland_layer <- function(root, file, value_col) {
  .read_esri_asc(file.path(root, "Input_files", paste0(file, ".asc"))) |>
    .nbx_add_cell_key(paste0("deposited ", file, " raster")) |>
    dplyr::transmute(cell_id = .data$cell_id, !!value_col := .data$value)
}

# The grassland-layers base: total cell area, keyed, with lon/lat retained
# for the final output.
.critn_grassland_base <- function(root) {
  .read_esri_asc(file.path(root, "Input_files", "a_tot.asc")) |>
    .nbx_add_cell_key("deposited a_tot raster") |>
    dplyr::transmute(
      cell_id = .data$cell_id,
      lon = .data$lon,
      lat = .data$lat,
      a_tot_ha = .data$value
    )
}

# The 12 non-base archive rasters this reader joins onto the base layer,
# and the output column each maps to.
.critn_grassland_specs <- function() {
  list(
    list(file = "a_crop", col = "a_crop_ha"),
    list(file = "a_gr_int", col = "a_gr_int_ha"),
    list(file = "a_gr_ext", col = "a_gr_ext_ha"),
    list(file = "n_man_eff_grass_int", col = "manure_int_n_kg"),
    list(file = "n_man_eff_grass_ext", col = "manure_ext_n_kg"),
    list(file = "nfix_grass_ext", col = "fix_ext_n_kg"),
    list(file = "n_up_grass_ext", col = "uptake_ext_n_kg"),
    list(file = "ndep", col = "ndep_n_kg"),
    list(file = "nh3_graz", col = "nh3_graz_n_kg"),
    list(file = "nh3_spread_fe", col = "nh3_spread_fe_n_kg"),
    list(file = "nh3_spread_man", col = "nh3_spread_man_n_kg"),
    list(file = "nh3_stor", col = "nh3_stor_n_kg")
  )
}

# Abort if any cell carries both intensive and extensive IMAGE 2010
# grassland: the two are mutually exclusive land-use classes.
.critn_grassland_check_mixed <- function(layers) {
  mixed <- layers$a_gr_int_ha > 0 & layers$a_gr_ext_ha > 0
  n_mixed <- sum(mixed)
  if (n_mixed > 0) {
    cli::cli_abort(
      "{n_mixed} cell{?s} ha{?s/ve} both intensive and extensive
       IMAGE 2010 grassland.",
      class = "whep_critn_mixed_grassland"
    )
  }
  invisible(layers)
}

# Assemble the 13 archive input layers needed to classify and budget
# grassland by intensity: cell areas, manure/fixation/uptake/deposition/NH3
# flows, and the derived IMAGE 2010 intensive/extensive class. One row per
# cell present in the a_tot layer (.read_esri_asc() already drops NODATA
# cells, so no extra filtering is needed for "a_tot non-missing").
.critical_n_grassland_layers <- function(root) {
  layers <- purrr::reduce(
    .critn_grassland_specs(),
    \(acc, spec) {
      dplyr::left_join(
        acc,
        .critn_grassland_layer(root, spec$file, spec$col),
        by = "cell_id",
        relationship = "one-to-one"
      )
    },
    .init = .critn_grassland_base(root)
  ) |>
    dplyr::mutate(
      # Areas: an unreached class in a cell means none of that class is
      # present -- a structural zero, not an absent measurement. Flow
      # columns are left untouched and stay NA when the source had none.
      a_crop_ha = dplyr::coalesce(.data$a_crop_ha, 0),
      a_gr_int_ha = dplyr::coalesce(.data$a_gr_int_ha, 0),
      a_gr_ext_ha = dplyr::coalesce(.data$a_gr_ext_ha, 0),
      nh3_tot_n_kg = .critical_n_nh3_tot(
        .data$nh3_graz_n_kg,
        .data$nh3_spread_fe_n_kg,
        .data$nh3_spread_man_n_kg,
        .data$nh3_stor_n_kg
      )
    )
  .critn_grassland_check_mixed(layers)
  layers |>
    dplyr::mutate(
      image_class_2010 = dplyr::case_when(
        .data$a_gr_int_ha > 0 ~ "intensive",
        .data$a_gr_ext_ha > 0 ~ "extensive",
        .default = NA_character_
      )
    ) |>
    dplyr::select(
      "cell_id",
      "lon",
      "lat",
      "a_tot_ha",
      "a_crop_ha",
      "a_gr_int_ha",
      "a_gr_ext_ha",
      "manure_int_n_kg",
      "manure_ext_n_kg",
      "fix_ext_n_kg",
      "uptake_ext_n_kg",
      "ndep_n_kg",
      "nh3_tot_n_kg",
      "image_class_2010"
    ) |>
    tibble::as_tibble()
}

# IMAGE 2010 N input and surplus on extensively managed grassland per cell:
# the budget Schulte-Uebbing et al. (2022) hold constant inside each cell's
# critical load. Nature, Supplementary Information, Supplementary Table 4:
#   row 6   Ndep,corr = MAX(Ndep, NH3,tot)            (NH3,tot: row 5 above)
#   row 3   f_egl     = a_egl / a_tot
#   row 17  Ndep_egl  = Ndep,corr * f_egl
#   row 21  Nin_egl   = Nman_egl + Nfix_egl + Ndep_egl
# (no synthetic fertiliser on extensive grassland: Supplementary Table 3 lists
# Nfer for ara and igl only), and SI Eq. 6, surplus = input - uptake. Archive:
# Zenodo doi:10.5281/zenodo.6395016 v1.0 (`.critn_source_doi()`).
#
# `layers` holds one row per cell: `cell_id`, areas `a_tot_ha`, `a_gr_ext_ha`
# (ha per cell) and flows `manure_ext_n_kg`, `fix_ext_n_kg`,
# `uptake_ext_n_kg`, `ndep_n_kg`, `nh3_tot_n_kg` (kg N per cell per year).
# Rates are per ha of extensive grassland and NA where there is none. A
# missing flow on a cell with extensive grassland aborts: it is a reader
# defect, never a zero.
#
# Measured on the archive (whole-cell NH3,tot): the max() takes the NH3,tot
# branch in 1,431 of the 27,360 cells with extensive grassland (Ndep 1.0011 Tg
# -> 1.7552 Tg there; whole-cell deposition on those 27,360 cells 38.8827 Tg
# -> 39.6367 Tg) and in 5,513 of all 66,222 cells (81.8754 Tg -> 89.7992 Tg).
# Resulting global extensive budget: input 47.28 Tg (manure 24.39, fixation
# 11.71, deposition 11.19), uptake 32.95 Tg, surplus 14.33 Tg on 2,342 Mha.
.critical_n_extensive_budget <- function(layers) {
  .check_columns(layers, .critn_budget_columns(), "layers")
  .critn_budget_validate(layers)
  layers |>
    dplyr::mutate(
      has_ext = !is.na(.data$a_gr_ext_ha) & .data$a_gr_ext_ha > 0,
      dep_corr = pmax(.data$ndep_n_kg, .data$nh3_tot_n_kg),
      dep_ext = dplyr::if_else(
        .data$a_gr_ext_ha == 0,
        0,
        .data$dep_corr * .data$a_gr_ext_ha / .data$a_tot_ha
      ),
      ext_input_n_kg = .data$manure_ext_n_kg +
        .data$fix_ext_n_kg +
        .data$dep_ext,
      ext_surplus_n_kg = .data$ext_input_n_kg - .data$uptake_ext_n_kg,
      ext_input_kgn_ha = dplyr::if_else(
        .data$has_ext,
        .data$ext_input_n_kg / .data$a_gr_ext_ha,
        NA_real_
      ),
      ext_surplus_kgn_ha = dplyr::if_else(
        .data$has_ext,
        .data$ext_surplus_n_kg / .data$a_gr_ext_ha,
        NA_real_
      )
    ) |>
    dplyr::select(
      "cell_id",
      "ext_input_n_kg",
      "ext_surplus_n_kg",
      "ext_input_kgn_ha",
      "ext_surplus_kgn_ha"
    ) |>
    tibble::as_tibble()
}

.critn_budget_columns <- function() {
  c(
    "cell_id",
    "a_tot_ha",
    "a_gr_ext_ha",
    .critn_budget_flows()
  )
}

.critn_budget_flows <- function() {
  c(
    "manure_ext_n_kg",
    "fix_ext_n_kg",
    "uptake_ext_n_kg",
    "ndep_n_kg",
    "nh3_tot_n_kg"
  )
}

.critn_budget_validate <- function(layers) {
  has_ext <- !is.na(layers$a_gr_ext_ha) & layers$a_gr_ext_ha > 0
  .critn_budget_check_area(layers, has_ext)
  .critn_budget_check_flows(layers, has_ext)
  .critn_budget_check_orphans(layers)
  # An all-zero uptake layer passes the NA checks above yet makes every
  # extensive surplus equal its input: guard that the layer was supplied.
  check_inputs_supplied(
    layers[has_ext, , drop = FALSE],
    c(uptake = "uptake_ext_n_kg", manure = "manure_ext_n_kg")
  )
  invisible(layers)
}

# f_egl must be a share of the cell: a missing or smaller total area would
# turn the deposition share into NA, Inf or more than the cell receives.
.critn_budget_check_area <- function(layers, has_ext) {
  bad <- has_ext &
    (is.na(layers$a_tot_ha) | layers$a_tot_ha < layers$a_gr_ext_ha)
  n_bad <- sum(bad)
  if (n_bad > 0) {
    ids <- utils::head(layers$cell_id[bad], 5)
    cli::cli_abort(
      c(
        "Extensive grassland exceeds the cell area in {n_bad} cell{?s}.",
        x = "{.field a_tot_ha} is missing or below {.field a_gr_ext_ha}.",
        i = "{cli::qty(length(ids))}Cell{?s}: {.val {ids}}."
      ),
      class = "whep_critn_budget_bad_area"
    )
  }
  invisible(TRUE)
}

.critn_budget_check_flows <- function(layers, has_ext) {
  core <- c("manure_ext_n_kg", "fix_ext_n_kg", "uptake_ext_n_kg")
  empty <- has_ext & rowSums(!is.na(as.matrix(layers[core]))) == 0
  n_empty <- sum(empty)
  if (n_empty > 0) {
    cli::cli_abort(
      c(
        "{n_empty} cell{?s} with extensive grassland ha{?s/ve} no manure,
         fixation or uptake value.",
        i = "An absent IMAGE layer is a reader defect, not a zero."
      ),
      class = "whep_critn_budget_missing_flow"
    )
  }
  flows <- .critn_budget_flows()
  n_na <- vapply(
    flows,
    \(col) sum(has_ext & is.na(layers[[col]])),
    integer(1)
  )
  if (any(n_na > 0)) {
    detail <- paste0(flows[n_na > 0], ": ", n_na[n_na > 0], " cell(s)")
    cli::cli_abort(
      c(
        "Missing N flow on cells with extensive grassland.",
        rlang::set_names(detail, rep("x", length(detail))),
        i = "An absent flow is a reader defect, not a zero."
      ),
      class = "whep_critn_budget_missing_flow"
    )
  }
  invisible(TRUE)
}

# A flow booked on a cell with no extensive grassland has no area to carry
# its rate, so it would vanish from any rate-times-area product downstream.
# The archive has none (0 cells for manure, fixation and uptake).
.critn_budget_check_orphans <- function(layers) {
  no_ext <- !is.na(layers$a_gr_ext_ha) & layers$a_gr_ext_ha == 0
  core <- as.matrix(
    layers[c("manure_ext_n_kg", "fix_ext_n_kg", "uptake_ext_n_kg")]
  )
  orphan <- no_ext & rowSums(!is.na(core) & core != 0) > 0
  n_orphan <- sum(orphan)
  if (n_orphan > 0) {
    ids <- utils::head(layers$cell_id[orphan], 5)
    cli::cli_abort(
      c(
        "{n_orphan} cell{?s} without extensive grassland carr{?ies/y}
         extensive manure, fixation or uptake N.",
        i = "{cli::qty(length(ids))}Cell{?s}: {.val {ids}}."
      ),
      class = "whep_critn_budget_flow_without_area"
    )
  }
  invisible(TRUE)
}
