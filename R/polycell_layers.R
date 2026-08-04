# Input layers for the polycell support table (plan
# `plans/2026-08-03-polycell-spatial-support.md`, DA-6, DA-9, DA-17).
#
# Every reader here resolves its path from an environment variable and aborts
# with an instruction when it is unset, so no absolute machine path is baked
# into the package.

#' Read the GLWD inland-water fraction on the 0.5-degree grid
#'
#' @description
#' Reads the LPJmL `glwd_lakes_and_rivers_30arcmin.clm` layer and its companion
#' `grid.clm`, returning the Global Lakes and Wetlands Database surface-water
#' fraction of each 0.5-degree cell. `water_frac` is a fraction of the **whole**
#' cell (the file's own `unit` is `"1"`), so the water area of a cell is
#' `water_frac * cell_area_ha`.
#'
#' This is the inland-water source for [build_polycell_support()]. It is
#' preferred over `ne_10m_lakes`, which carries roughly half of global inland
#' water and omits the Caspian Sea entirely.
#'
#' @param dir Directory holding `grid.clm` and the water file. Defaults to
#'   `Sys.getenv("WHEP_LPJML_INPUT_DIR")`.
#' @param file Name of the water CLM file inside `dir`.
#'
#' @return A `tibble` with `lon`, `lat` and `water_frac`, one row per cell of
#'   the CLM grid.
#' @export
#'
#' @source
#'   Ostberg, S., Mueller, C., Heinke, J. and Schaphoff, S. (2023). LandInG 1.0:
#'   a toolbox to derive input datasets for terrestrial ecosystem modelling at
#'   variable resolutions from heterogeneous sources. *Geoscientific Model
#'   Development* 16, 3375-3406. \doi{10.5194/gmd-16-3375-2023}
#'
#' @examples
#' # Requires WHEP_LPJML_INPUT_DIR to be set; not run without it.
#' if (nzchar(Sys.getenv("WHEP_LPJML_INPUT_DIR"))) {
#'   read_glwd_water()
#' }
read_glwd_water <- function(
  dir = NULL,
  file = "glwd_lakes_and_rivers_30arcmin.clm"
) {
  dir <- .whep_layer_dir(dir, "WHEP_LPJML_INPUT_DIR", "the LPJmL CLM inputs")
  grid <- .read_clm(file.path(dir, "grid.clm"), expect_bands = 2L)
  water <- .read_clm(file.path(dir, file), expect_bands = 1L)
  if (nrow(grid) != nrow(water)) {
    cli::cli_abort(c(
      "{.file {file}} and {.file grid.clm} hold different cell counts.",
      i = "grid: {nrow(grid)} cells; water: {nrow(water)} cells."
    ))
  }
  tibble::tibble(
    lon = grid$band_1,
    lat = grid$band_2,
    water_frac = water$band_1
  )
}

#' Read the Natural Earth glaciated-areas ice layer
#'
#' @description
#' Reads `ne_10m_glaciated_areas`, the ice source for
#' [build_polycell_support()]. A few features are invalid under the spherical
#' `s2` engine and GEOS-level `sf::st_make_valid()` does not repair them, so
#' `sf::st_area()` aborts with "Loop 0 is not valid". Those features are
#' repaired under the planar engine, and any that remain invalid are measured
#' with `terra::expanse()`, which does not go through `s2`, and reported rather
#' than silently dropped.
#'
#' The layer is a coarse present-day snapshot, so ice area does **not** vary
#' historically. That is acceptable only while ice is a reporting category
#' rather than a driver.
#'
#' @param dir Directory holding the shapefile. Defaults to
#'   `Sys.getenv("WHEP_NATURALEARTH_DIR")`, under which the layer is expected at
#'   `ne_10m_glaciated_areas/ne_10m_glaciated_areas.shp`.
#'
#' @return An `sf` table of glaciated polygons in WGS84, carrying a
#'   `s2_repaired` logical column. The `"unrepaired"` attribute is a `tibble` of
#'   the features that stayed `s2`-invalid, with their `terra::expanse()` area.
#' @export
#'
#' @examples
#' # Requires WHEP_NATURALEARTH_DIR to be set; not run without it.
#' if (nzchar(Sys.getenv("WHEP_NATURALEARTH_DIR"))) {
#'   read_glaciated_areas()
#' }
read_glaciated_areas <- function(dir = NULL) {
  rlang::check_installed(c("sf", "terra"))
  dir <- .whep_layer_dir(dir, "WHEP_NATURALEARTH_DIR", "the Natural Earth data")
  path <- file.path(
    dir,
    "ne_10m_glaciated_areas",
    "ne_10m_glaciated_areas.shp"
  )
  if (!file.exists(path)) {
    cli::cli_abort("Ice layer not found at {.file {path}}.")
  }
  path |>
    sf::read_sf() |>
    sf::st_transform(4326) |>
    .repair_s2_polygons()
}

#' Read the LUH2 terrestrial-area validation layer
#'
#' @description
#' Reads `staticData_quarterdeg.nc` and returns `(1 - icwtr) * carea` summed to
#' the 0.5-degree grid: the terrestrial area LUH2 itself implies. This is the
#' DA-5 validation layer for [build_polycell_support()] and is never a
#' production mask, because `icwtr` includes the ocean as well as ice and inland
#' water, and because LUH2 misses small islands its own 0.25-degree mask calls
#' sea.
#'
#' @param vintage Which LUH2 tree to read. `"GCB2022"` (default) is the
#'   `UofMD-landState-LUH2-GCB2022` release under
#'   `Sys.getenv("WHEP_LUH2_DIR")`; `"v2h"` is the base release under
#'   `Sys.getenv("WHEP_LUH2_V2H_DIR")`.
#' @param dir Directory holding `staticData_quarterdeg.nc`, overriding the
#'   vintage's environment variable.
#'
#' @return A `tibble` with `lon`, `lat` and `terrestrial_ha` on the 0.5-degree
#'   grid, carrying the vintage in its `"luh2_vintage"` attribute.
#' @export
#'
#' @examples
#' # Requires WHEP_LUH2_DIR to be set; not run without it.
#' if (nzchar(Sys.getenv("WHEP_LUH2_DIR"))) {
#'   read_luh2_terrestrial(vintage = "GCB2022")
#' }
read_luh2_terrestrial <- function(vintage = c("GCB2022", "v2h"), dir = NULL) {
  rlang::check_installed("ncdf4")
  vintage <- rlang::arg_match(vintage)
  dir <- .whep_layer_dir(dir, .luh2_vintage_var(vintage), "the LUH2 tree")
  out <- .read_luh2_static(file.path(dir, "staticData_quarterdeg.nc"))
  attr(out, "luh2_vintage") <- vintage
  out
}

#' Read the polycell support table from its registered pin
#'
#' @description
#' Resolves the versioned `polycell_support` input (DA-17), preferring a local
#' parquet named by `Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH")` so a development
#' build can be used before it is published.
#'
#' @param path Optional path to a local parquet, overriding the environment
#'   variable and the pin.
#' @param version Pin version, passed to [whep_read_file()]. `NULL` takes the
#'   version frozen in [whep_inputs].
#'
#' @return A `tibble` in the [build_polycell_support()] grain.
#' @export
#'
#' @examples
#' # Requires WHEP_POLYCELL_SUPPORT_PATH or a published pin; not run without it.
#' if (nzchar(Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH"))) {
#'   read_polycell_support()
#' }
read_polycell_support <- function(path = NULL, version = NULL) {
  path <- path %||% Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH", "")
  if (nzchar(path)) {
    if (!file.exists(path)) {
      cli::cli_abort("Polycell support table not found at {.file {path}}.")
    }
    return(tibble::as_tibble(nanoparquet::read_parquet(path)))
  }
  tryCatch(
    whep_read_file("polycell_support", version = version),
    error = function(e) {
      cli::cli_abort(
        c(
          "The {.val polycell_support} pin has no published version yet.",
          i = "Point {.envvar WHEP_POLYCELL_SUPPORT_PATH} at a local parquet
               written by {.fn build_polycell_support}, or publish the pin with
               {.file inst/scripts/prepare_upload.R} and freeze its version in
               {.file inst/extdata/whep_inputs.csv}."
        ),
        parent = e
      )
    }
  )
}

# -- Path resolution ----------------------------------------------------------

.whep_layer_dir <- function(dir, env_var, what) {
  dir <- dir %||% Sys.getenv(env_var, "")
  if (!nzchar(dir)) {
    cli::cli_abort(c(
      "Set {.envvar {env_var}} to the directory holding {what}.",
      i = "Pass {.arg dir} to override it for one call."
    ))
  }
  if (!dir.exists(dir)) {
    cli::cli_abort("{.envvar {env_var}} points at a missing directory: {dir}.")
  }
  dir
}

.luh2_vintage_var <- function(vintage) {
  if (identical(vintage, "v2h")) "WHEP_LUH2_V2H_DIR" else "WHEP_LUH2_DIR"
}

# -- LPJmL CLM reader ---------------------------------------------------------
#
# WHEP had no CLM reader before this: every other LPJmL path in R/ reads
# NetCDF. A CLM file is LPJmL's own binary, opening with a magic string and a
# fixed-width header whose length, value type, scaling and endianness are
# restated by the companion `.json` sidecar. The sidecar is the authority --
# the header layout is not re-derived here, because guessing it wrong shifts
# every value by a few bytes and still returns plausible numbers.

.read_clm <- function(path, expect_bands = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("CLM file not found at {.file {path}}.")
  }
  meta <- .read_clm_meta(path)
  if (!is.null(expect_bands) && meta$nbands != expect_bands) {
    cli::cli_abort(
      "{.file {basename(path)}} has {meta$nbands} band{?s}, expected
       {expect_bands}."
    )
  }
  .read_clm_payload(path, meta) |>
    matrix(nrow = meta$nbands) |>
    t() |>
    tibble::as_tibble(.name_repair = \(x) paste0("band_", seq_along(x)))
}

.read_clm_meta <- function(path) {
  .check_clm_magic(path)
  json <- paste0(path, ".json")
  if (!file.exists(json)) {
    cli::cli_abort(c(
      "{.file {basename(path)}} has no {.file .json} sidecar.",
      i = "The sidecar states the header length, value type, scaling and
           endianness; without it the payload cannot be located safely."
    ))
  }
  meta <- .clm_meta_from_json(json)
  if (!meta$datatype %in% names(.clm_types())) {
    cli::cli_abort("Unsupported CLM datatype {.val {meta$datatype}}.")
  }
  meta
}

.check_clm_magic <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  magic <- rawToChar(readBin(con, "raw", n = 7L))
  if (!stringr::str_detect(magic, "^LPJ")) {
    cli::cli_abort(
      "{.file {basename(path)}} does not open with an LPJmL CLM magic string."
    )
  }
  invisible(magic)
}

.clm_meta_from_json <- function(json) {
  rlang::check_installed("jsonlite")
  spec <- jsonlite::read_json(json, simplifyVector = TRUE)
  list(
    offset = as.integer(spec$offset),
    ncell = as.integer(spec$ncell),
    nbands = as.integer(spec$nbands),
    nstep = as.integer(spec$nstep %||% 1L),
    nyear = as.integer(spec$nyear %||% 1L),
    scalar = as.numeric(spec$scalar),
    datatype = as.character(spec$datatype),
    endian = if (isTRUE(spec$bigendian)) "big" else "little"
  )
}

.clm_types <- function() {
  list(
    byte = list(what = "integer", size = 1L, signed = FALSE),
    short = list(what = "integer", size = 2L, signed = TRUE),
    int = list(what = "integer", size = 4L, signed = TRUE),
    float = list(what = "double", size = 4L, signed = TRUE),
    double = list(what = "double", size = 8L, signed = TRUE)
  )
}

.read_clm_payload <- function(path, meta) {
  spec <- .clm_types()[[meta$datatype]]
  n <- meta$ncell * meta$nbands * meta$nstep * meta$nyear
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, "raw", n = meta$offset)
  raw <- readBin(
    con,
    spec$what,
    n = n,
    size = spec$size,
    signed = spec$signed,
    endian = meta$endian
  )
  if (length(raw) != n) {
    cli::cli_abort(
      "{.file {basename(path)}} holds {length(raw)} values, expected {n}."
    )
  }
  raw * meta$scalar
}

# -- s2 validity --------------------------------------------------------------

# Repair polygons that the spherical engine rejects. GEOS-level make_valid
# splits the self-intersections into pieces s2 usually accepts; whatever is
# still invalid is kept but excluded from the s2 geometry so one bad loop
# cannot abort the whole producer, and its geodesic area is measured with
# `terra::expanse()` instead. This is not hypothetical on either layer: three
# `ne_10m_glaciated_areas` features and seven shipped polity polygons are
# s2-invalid and `sf::st_area()` aborts on all of them; the planar repair
# recovers four of the seven polities, leaving three unusable.
.repair_s2_polygons <- function(x) {
  fixed <- .s2_repair(sf::st_geometry(x))
  sf::st_geometry(x) <- fixed$geom
  x$s2_repaired <- fixed$status == "repaired"
  ok <- fixed$status != "invalid"
  attr(x, "unrepaired") <- .unrepaired_areas(x, ok)
  x[ok, ]
}

# Classify each geometry as `ok`, `repaired` or `invalid`, repairing in place.
.s2_repair <- function(geom) {
  status <- rep("ok", length(geom))
  ok <- .s2_valid(geom)
  if (any(!ok)) {
    geom[!ok] <- .make_valid_planar(geom[!ok])
    status[!ok] <- dplyr::if_else(.s2_valid(geom[!ok]), "repaired", "invalid")
  }
  list(geom = geom, status = status)
}

# `sf::st_is_valid()` under s2 returns FALSE where `sf::st_area()` aborts, and
# it does so an order of magnitude faster than probing each feature.
.s2_valid <- function(geom) {
  ok <- sf::st_is_valid(geom)
  !is.na(ok) & ok
}

.make_valid_planar <- function(geom) {
  old <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))
  sf::st_make_valid(geom)
}

.unrepaired_areas <- function(x, ok) {
  if (all(ok)) {
    return(tibble::tibble(feature = integer(), area_ha = double()))
  }
  bad <- which(!ok)
  cli::cli_warn(c(
    "{length(bad)} ice feature{?s} stayed invalid under the spherical engine.",
    i = "Their area is measured with {.fn terra::expanse} and reported in the
         {.val unrepaired} attribute, not silently dropped."
  ))
  tibble::tibble(
    feature = bad,
    area_ha = terra::expanse(terra::vect(x[bad, ]), unit = "m") / 1e4
  )
}

# -- LUH2 static --------------------------------------------------------------

.read_luh2_static <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("LUH2 static file not found at {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  # `ncvar_get()` returns 1-D arrays, and a tibble column that is a 1-D array
  # is not a plain vector: it silently breaks `dplyr::case_when()` downstream
  # in anything built from this table. Coerce once, here.
  tidyr::expand_grid(
    lat = as.numeric(ncdf4::ncvar_get(nc, "lat")),
    lon = as.numeric(ncdf4::ncvar_get(nc, "lon"))
  ) |>
    dplyr::mutate(
      icwtr = as.vector(ncdf4::ncvar_get(nc, "icwtr")),
      carea_ha = as.vector(ncdf4::ncvar_get(nc, "carea")) * 100
    ) |>
    .luh2_static_to_half_degree()
}

# The native grid is 0.25 degrees; the polycell grid is 0.5, so four native
# cells are summed. Cells whose `icwtr` is NA are ocean in LUH2's own mask and
# contribute nothing.
.luh2_static_to_half_degree <- function(native) {
  native |>
    dplyr::filter(!is.na(.data$icwtr), !is.na(.data$carea_ha)) |>
    dplyr::mutate(
      lon = .half_degree_centre(.data$lon),
      lat = .half_degree_centre(.data$lat),
      terrestrial_ha = (1 - .data$icwtr) * .data$carea_ha
    ) |>
    dplyr::summarise(
      terrestrial_ha = sum(.data$terrestrial_ha),
      .by = c("lon", "lat")
    ) |>
    dplyr::arrange(.data$lon, .data$lat)
}

.half_degree_centre <- function(x) {
  floor((x + 180) / 0.5) * 0.5 - 180 + 0.25
}
