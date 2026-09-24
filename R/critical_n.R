# Gridded critical-nitrogen layers from Schulte-Uebbing et al. (2022),
# doi:10.1038/s41586-022-05158-2 (data archive Zenodo
# doi:10.5281/zenodo.6395016, CC-BY-4.0).
#
# CONFIRMED FORMAT (archive inspected; see plans/sjos_n_critical_n_format.md):
# - Every layer is an ESRI ASCII grid (.asc): 6 header lines
#   (ncols nrows xllcorner yllcorner cellsize NODATA_value) then the value
#   matrix, row 1 = north (ymax). Global 0.5-degree grid, 720 x 360, EPSG:4326.
# - Units kg N ha-1 yr-1 (year-2010 snapshot), except the "Threshold
#   exceedance by impact" layer (threshold_exc_<land_use>.asc, read as
#   var = "threshold_exceedance"), a categorical 1-8 code of which thresholds
#   are EXCEEDED in the cell. It does not say which threshold binds; that is
#   the argmin of the three threshold-specific critical surpluses, derived by
#   build_critical_n_binding().
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
#' threshold-exceedance map. Values are in kg N per hectare per year (a
#' categorical 1-8 impact code for `threshold_exceedance`). The critical
#' surplus, input and exceedance are selectable by `threshold` (minimum of all
#' media, surface water, groundwater or deposition) and `land_use` (all
#' agricultural land, arable only, or intensively managed grassland); the three
#' critical losses and the threshold-exceedance map ignore `threshold`.
#'
#' The threshold-exceedance map (the archive's "Threshold exceedance by
#' impact" folder, `threshold_exc_<land_use>.asc`) records which thresholds are
#' exceeded in a cell, not which one binds. The threshold that binds, the one
#' with the lowest critical surplus, is derived by
#' [build_critical_n_binding()]. The layer was formerly requested as
#' `var = "binding_threshold"`; that name still reads it, with a deprecation
#' warning, and the result is stamped `critical_var = "threshold_exceedance"`.
#'
#' The archive
#' directory comes from `dir`, else the `WHEP_CRITICAL_N_DIR` environment
#' variable, else a local cache that is populated by downloading the archive
#' from Zenodo on first use (see `dir`).
#'
#' @param var Which critical-nitrogen layer to read: one of
#'   `"critical_n_surplus"`, `"critical_n_input"`, `"exceedance"`,
#'   `"crit_nh3_emission"`, `"crit_leaching_gw"`, `"crit_load_sw"` or
#'   `"threshold_exceedance"`. `"binding_threshold"` is a deprecated alias of
#'   `"threshold_exceedance"`.
#' @param threshold Impact threshold selecting the critical value: `"mi"`
#'   (minimum across media, the collapsed boundary), `"sw"` (surface-water
#'   eutrophication), `"gw"` (groundwater nitrate) or `"de"` (atmospheric or
#'   terrestrial deposition). Ignored by the critical-loss and
#'   threshold-exceedance layers.
#' @param land_use Land-use scope: `"all"` (arable plus intensively managed
#'   grassland), `"ara"` (arable only) or `"igl"` (intensively managed
#'   grassland). Ignored by the critical-loss layers (`crit_nh3_emission`,
#'   `crit_leaching_gw`, `crit_load_sw`), which have a single land-use-agnostic
#'   file; used by the threshold-exceedance map and the
#'   surplus/input/exceedance layers.
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
#'   `threshold_exceedance`) and retained layer provenance: `critical_var`,
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
    "threshold_exceedance",
    "binding_threshold"
  ),
  threshold = c("mi", "sw", "gw", "de"),
  land_use = c("all", "ara", "igl"),
  dir = NULL,
  data = NULL,
  example = FALSE,
  verify_source = TRUE
) {
  var <- .critn_resolve_var(rlang::arg_match(var))
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

#' Derive the binding critical-nitrogen threshold per cell.
#'
#' @description
#' Identifies, for every 0.5-degree cell, the impact threshold whose critical
#' nitrogen surplus is lowest among the three threshold-specific surfaces of
#' Schulte-Uebbing et al. (2022): atmospheric deposition (`"de"`), groundwater
#' nitrate (`"gw"`) and surface-water eutrophication (`"sw"`). That threshold
#' is the one that binds. The archive's own threshold-exceedance map
#' ([read_critical_n()] with `var = "threshold_exceedance"`) is a different
#' quantity: it records which thresholds the 2010 surplus exceeds.
#'
#' `binding_threshold` is `"deposition"`, `"groundwater"`,
#' `"surface_water"`, a two-way tie, `"yield_potential_cap"` or
#' `"non_agricultural_floor"`. A two-way tie is recorded explicitly by joining
#' both tied thresholds with `+` in that fixed order, for example
#' `"groundwater+surface_water"`. Ties are exact equalities of the deposited
#' values: on the real archive no surface lies within 1e-6 kg N/ha of the cell
#' minimum without equalling it, so a tolerance would change nothing. A cell
#' missing from any of the three surfaces gets `NA`.
#'
#' A cell where all three critical surpluses are equal has no environmental
#' threshold that binds on its own. Schulte-Uebbing et al. (2022, Nature 610,
#' Methods) set such a threshold-independent value by one of two rules, and
#' the label names which one:
#'
#' * `"non_agricultural_floor"`: all three thresholds are exceeded (each
#'   threshold-specific exceedance of the critical surplus is positive) or the
#'   tied critical surplus is negative. The source's "Aggregation to regional
#'   and planetary boundaries" section states: "Where N losses from
#'   non-agricultural sources alone exceeded thresholds, critical N inputs
#'   from fertilizer and manure were set to zero." The critical input is then
#'   the fixation and deposition left over, the same for every threshold.
#' * `"yield_potential_cap"`: every other tie. Step 4 of the source's Methods
#'   reads: "for areas with no threshold exceedance, cut off critical inputs
#'   and surplus at a maximum value, set to the input level required to obtain
#'   crop yield potentials", with `Nin(crit,max) = Nup(Yp) / NUE(act)`, where
#'   `Nup(Yp)` is crop nitrogen uptake at potential yield and `NUE(act)` the
#'   current nitrogen use efficiency, "capped ... at 0.8".
#'
#' A tie whose critical surplus is not negative and whose exceedance is
#' missing on any threshold cannot be assigned to either rule and gets `NA`.
#' On the real archive the three critical inputs are identical in every tied
#' cell. The two labels count 293 floor and 8,895 cap cells of 28,573 for
#' `"ara"`, 293 and 9,138 of 28,881 for `"all"`, and 0 and 1,727 of 11,740
#' for `"igl"`. The split matches the archive's threshold-exceedance map
#' exactly: every floor cell carries code 8 and every cap cell code 1. The 293
#' floor cells include 3 with a negative critical surplus (minimum -21.78
#' kg N/ha), and their median critical input is 38.7 kg N/ha against 90.7 for
#' the `"all"` cap cells.
#'
#' When the deposited minimum-of-all-media surface (`"mi"`) is supplied,
#' `binding_matches_mi` reports whether it equals the lowest of the three
#' threshold-specific surpluses. Where it does not, the relation between
#' `"mi"` and the three surfaces is undetermined: `"mi"` differs from
#' `min(de, gw, sw)` there and the source text does not explain why. On the
#' real archive this happens in 1,623 of 28,573 cells for `"ara"` (maximum
#' gap 159.5 kg N/ha), 1,540 of 28,881 for `"all"` (159.5 kg N/ha) and 1,480
#' of 11,740 for `"igl"` (479.1 kg N/ha), with `"mi"` above the minimum in
#' some cells and below it in others. `binding_threshold` still names the
#' argmin of the three surfaces in those cells.
#'
#' @param critical Optional named list of [read_critical_n()] critical-surplus
#'   layers (`var = "critical_n_surplus"`) with elements `de`, `gw` and `sw`,
#'   and optionally `mi`, each stamped with its own threshold and with
#'   `land_use`. When `NULL` (default) the four layers are read from the
#'   archive with [read_critical_n()], together with `exceedance`.
#' @param exceedance Named list of [read_critical_n()] exceedance layers
#'   (`var = "exceedance"`) with elements `de`, `gw` and `sw`, used only to
#'   tell the two kinds of three-way tie apart. Required when `critical` is
#'   supplied; read from the archive when both are `NULL` (default).
#' @param land_use Land-use scope: `"all"`, `"ara"` or `"igl"`, as in
#'   [read_critical_n()]. Supplied layers must carry this scope.
#' @param dir Optional archive directory passed to [read_critical_n()] when
#'   `critical` is `NULL`.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with one row per cell: `cell_id`, `lon`, `lat`,
#'   `critical_land_use`, the three threshold-specific critical surpluses
#'   `critical_de_kgn_ha`, `critical_gw_kgn_ha`, `critical_sw_kgn_ha` (kg N per
#'   hectare per year), their exceedances `exceedance_de_kgn_ha`,
#'   `exceedance_gw_kgn_ha`, `exceedance_sw_kgn_ha`, the minimum critical
#'   surplus `binding_critical_kgn_ha`, the `binding_threshold` label, the
#'   deposited `critical_mi_kgn_ha` and the logical `binding_matches_mi` (both
#'   `NA` when `mi` is not supplied).
#' @export
#' @examples
#' build_critical_n_binding(example = TRUE)
build_critical_n_binding <- function(
  critical = NULL,
  exceedance = NULL,
  land_use = c("all", "ara", "igl"),
  dir = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_critical_n_binding())
  }
  land_use <- rlang::arg_match(land_use)
  if (is.null(critical)) {
    critical <- .critn_read_thresholds(land_use, dir)
    exceedance <- exceedance %||%
      .critn_read_thresholds(land_use, dir, "exceedance")
  }
  .critn_validate_thresholds(critical, land_use)
  .critn_validate_exceedance(exceedance, land_use)
  dplyr::full_join(
    .critn_threshold_wide(critical, "critical"),
    .critn_threshold_wide(exceedance, "exceedance"),
    by = c("cell_id", "lon", "lat"),
    relationship = "one-to-one"
  ) |>
    dplyr::arrange(.data$cell_id) |>
    .critn_label_binding() |>
    dplyr::mutate(critical_land_use = .env$land_use, .after = "lat")
}

# ---- Private helpers --------------------------------------------------

# "binding_threshold" named the threshold-exceedance map, which records the
# thresholds a cell EXCEEDS, not the one that binds. The old name keeps working
# so existing calls do not break, but it warns and is stamped with the new one.
.critn_resolve_var <- function(var) {
  if (var != "binding_threshold") {
    return(var)
  }
  cli::cli_warn(
    c(
      "{.code var = \"binding_threshold\"} is deprecated.",
      i = "The layer records which thresholds are exceeded, not which binds;
           request it as {.code var = \"threshold_exceedance\"}.",
      i = "The binding threshold itself comes from
           {.fn build_critical_n_binding}."
    ),
    class = "whep_critn_var_deprecated"
  )
  "threshold_exceedance"
}

.critn_threshold_names <- function() {
  c(de = "deposition", gw = "groundwater", sw = "surface_water")
}

.critn_read_thresholds <- function(
  land_use,
  dir,
  var = "critical_n_surplus"
) {
  thresholds <- if (var == "exceedance") {
    c("de", "gw", "sw")
  } else {
    c("de", "gw", "sw", "mi")
  }
  purrr::map(
    rlang::set_names(thresholds),
    \(threshold) {
      read_critical_n(
        var,
        threshold = threshold,
        land_use = land_use,
        dir = dir
      )
    }
  )
}

# The exceedance layers only classify three-way ties, but without them a tie
# cannot be assigned to either source rule, so their absence is refused here
# rather than turned into a guessed label.
.critn_validate_exceedance <- function(exceedance, land_use) {
  if (
    !is.list(exceedance) ||
      !all(c("de", "gw", "sw") %in% names(exceedance))
  ) {
    cli::cli_abort(c(
      "{.arg exceedance} must be a named list with elements {.val de},
       {.val gw} and {.val sw}.",
      i = "Read each with {.code read_critical_n(\"exceedance\", threshold)};
           they tell a yield-potential cap from a non-agricultural floor."
    ))
  }
  purrr::iwalk(
    exceedance[c("de", "gw", "sw")],
    \(layer, threshold) {
      .critn_validate_layer(layer, threshold, land_use, "exceedance")
    }
  )
  invisible(TRUE)
}

.critn_validate_thresholds <- function(critical, land_use) {
  if (!is.list(critical) || !all(c("de", "gw", "sw") %in% names(critical))) {
    cli::cli_abort(
      "{.arg critical} must be a named list with elements {.val de},
       {.val gw} and {.val sw}, and optionally {.val mi}."
    )
  }
  purrr::iwalk(
    critical[intersect(c("de", "gw", "sw", "mi"), names(critical))],
    \(layer, threshold) .critn_validate_layer(layer, threshold, land_use)
  )
  invisible(TRUE)
}

# Each element must be the expected layer (the critical SURPLUS, or its
# exceedance) of its own threshold and of the requested land use: a
# critical-input or wrong-scope grid would otherwise be compared silently.
.critn_validate_layer <- function(
  layer,
  threshold,
  land_use,
  var = "critical_n_surplus"
) {
  .check_columns(
    layer,
    c(
      "lon",
      "lat",
      "value",
      "critical_var",
      "critical_threshold",
      "critical_land_use"
    ),
    paste0(var, "$", threshold)
  )
  stamps <- list(
    critical_var = var,
    critical_threshold = threshold,
    critical_land_use = land_use
  )
  purrr::iwalk(stamps, \(expected, col) {
    found <- unique(layer[[col]])
    if (!identical(found, expected)) {
      cli::cli_abort(c(
        "{.field {var}${threshold}} is not the expected layer.",
        i = "Expected {.field {col}} {.val {expected}}; found {.val {found}}."
      ))
    }
  })
  invisible(TRUE)
}

.critn_threshold_wide <- function(layers, prefix) {
  layers[intersect(c("de", "gw", "sw", "mi"), names(layers))] |>
    purrr::imap(\(layer, threshold) {
      layer |>
        .nbx_add_cell_key(paste0(prefix, "$", threshold)) |>
        dplyr::select("cell_id", "lon", "lat", value = "value") |>
        dplyr::rename_with(
          \(x) paste0(prefix, "_", threshold, "_kgn_ha"),
          "value"
        )
    }) |>
    purrr::reduce(
      \(x, y) {
        dplyr::full_join(
          x,
          y,
          by = c("cell_id", "lon", "lat"),
          relationship = "one-to-one"
        )
      }
    ) |>
    dplyr::arrange(.data$cell_id)
}

# Argmin of the three threshold-specific critical surpluses, with a two-way tie
# named in the fixed deposition/groundwater/surface_water order. All three
# equal means no environmental threshold binds on its own; the tie is then
# assigned to the source rule that produced it (see the roxygen above).
.critn_label_binding <- function(wide) {
  if (!rlang::has_name(wide, "critical_mi_kgn_ha")) {
    wide$critical_mi_kgn_ha <- NA_real_
  }
  de <- wide$critical_de_kgn_ha
  gw <- wide$critical_gw_kgn_ha
  sw <- wide$critical_sw_kgn_ha
  low <- pmin(de, gw, sw)
  tags <- paste0(.critn_threshold_names(), "+")
  label <- paste0(
    dplyr::if_else(de == low, tags[[1L]], ""),
    dplyr::if_else(gw == low, tags[[2L]], ""),
    dplyr::if_else(sw == low, tags[[3L]], "")
  )
  wide |>
    dplyr::mutate(
      binding_critical_kgn_ha = .env$low,
      binding_threshold = dplyr::if_else(
        .env$de == .env$gw & .env$gw == .env$sw,
        .critn_tie_rule(
          .env$low,
          .data$exceedance_de_kgn_ha,
          .data$exceedance_gw_kgn_ha,
          .data$exceedance_sw_kgn_ha
        ),
        stringr::str_remove(.env$label, "\\+$")
      ),
      binding_matches_mi = .data$critical_mi_kgn_ha == .env$low
    ) |>
    dplyr::select(
      "cell_id",
      "lon",
      "lat",
      "critical_de_kgn_ha",
      "critical_gw_kgn_ha",
      "critical_sw_kgn_ha",
      "exceedance_de_kgn_ha",
      "exceedance_gw_kgn_ha",
      "exceedance_sw_kgn_ha",
      "binding_critical_kgn_ha",
      "binding_threshold",
      "critical_mi_kgn_ha",
      "binding_matches_mi"
    )
}

# Which source rule set a three-way tie. A negative tied surplus can only come
# from zeroed fertilizer and manure (the non-agricultural floor); otherwise the
# floor applies where every threshold is exceeded and the yield-potential cap
# where none is. A non-negative tie with any exceedance missing stays NA.
.critn_tie_rule <- function(tied, exc_de, exc_gw, exc_sw) {
  all_exceeded <- exc_de > 0 & exc_gw > 0 & exc_sw > 0
  dplyr::case_when(
    tied < 0 ~ "non_agricultural_floor",
    is.na(all_exceeded) ~ NA_character_,
    all_exceeded ~ "non_agricultural_floor",
    .default = "yield_potential_cap"
  )
}

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
  image <- .read_esri_asc(file.path(
    root,
    "Input_files",
    "image_region28.asc"
  )) |>
    dplyr::rename(image_region = value) |>
    .nbx_add_cell_key("deposited IMAGE-region raster") |>
    dplyr::select("cell_id", "image_region")
  keyed <- .nbx_add_cell_key(grid, "deposited critical-N raster")
  keyed |>
    dplyr::left_join(area, by = "cell_id", relationship = "many-to-one") |>
    dplyr::left_join(image, by = "cell_id", relationship = "many-to-one") |>
    dplyr::mutate(image_region = as.integer(.data$image_region))
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
    threshold_exceedance = list(
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
            "threshold_exceedance"
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
