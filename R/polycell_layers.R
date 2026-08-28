# Input layers for the polycell support table (plan
# `plans/2026-08-03-polycell-spatial-support.md`, DA-6, DA-9, DA-17).
#
# Every reader here resolves its path from an environment variable and aborts
# with an instruction when it is unset, so no absolute machine path is baked
# into the package.

#' Read the GLWD inland-water fraction on the 0.5-degree grid
#'
#' @description
#' Returns the Global Lakes and Wetlands Database surface-water fraction of each
#' 0.5-degree cell. `water_frac` is a fraction of the **whole** cell, so the
#' water area of a cell is `water_frac * cell_area_ha`.
#'
#' This is the inland-water source for [build_polycell_support()]. It is
#' preferred over `ne_10m_lakes`, which carries roughly half of global inland
#' water and omits the Caspian Sea entirely.
#'
#' The fraction is derived from the GLWD rasters
#' `inst/scripts/download/download_hydrology.R` fetches from the published
#' figshare DOI, through [glwd_water_fraction()] -- the same derivation
#' `inst/scripts/prepare_spatialize_all.R` uses to write LPJmL's own
#' `lakes_rivers` input, so the two cannot drift.
#'
#' Until WHEP settled on GLWD v2 this read LPJmL's
#' `glwd_lakes_and_rivers_30arcmin.clm` and a companion `grid.clm` instead.
#' That pair is derived from GLWD **v1**, no script in this repository produces
#' it, and it gives 2.4759 Mkm2 of inland water over the 67,420-cell CRU land
#' mask against v2's 3.2480 Mkm2. Any figure quoted against the old layer has
#' to be re-measured rather than carried across.
#'
#' @param dir Directory holding `GLWD/`, as `download_hydrology.R` lays it out.
#'   Defaults to `Sys.getenv("WHEP_LPJML_INPUT_DIR")`.
#'
#' @return A `tibble` with `lon`, `lat` and `water_frac`, one row per cell the
#'   rasters cover.
#' @export
#'
#' @source
#'   Lehner, B., Anand, M., Fluet-Chouinard, E. et al. (2025). Mapping the
#'   world's inland surface waters: an update to the Global Lakes and Wetlands
#'   Database (GLWD v2). *Earth System Science Data* 17, 2277-2329.
#'   \doi{10.5194/essd-17-2277-2025}
#'
#' @examples
#' # Requires WHEP_LPJML_INPUT_DIR to be set; not run without it.
#' if (nzchar(Sys.getenv("WHEP_LPJML_INPUT_DIR"))) {
#'   read_glwd_water()
#' }
read_glwd_water <- function(dir = NULL) {
  dir <- .whep_layer_dir(dir, "WHEP_LPJML_INPUT_DIR", "the GLWD download")
  glwd_water_fraction(file.path(dir, "GLWD"))
}

#' Derive the lake-and-river fraction of each 0.5-degree cell from GLWD
#'
#' @description
#' Aggregates the Global Lakes and Wetlands Database rasters to the 0.5-degree
#' grid as a fraction of the whole cell. This is the one implementation of that
#' derivation in WHEP: [read_glwd_water()] calls it for
#' [build_polycell_support()], and `inst/scripts/prepare_spatialize_all.R`
#' calls it to write LPJmL's `lakes_rivers` input. It used to live only in that
#' script, so the polycell producer read a hand-made `.clm` artefact of an
#' LPJmL run instead and the two answers were free to diverge.
#'
#' @section Which classes count as inland water:
#' GLWD v2 is a **33-class wetland map**, not a water fraction, so a subset has
#' to be chosen and the choice is a judgement rather than a lookup. Taken here:
#' lakes as classes 1-3 (freshwater lake, saline lake, reservoir) and rivers as
#' class 7 (small streams). Everything else -- palustrine and riverine wetland,
#' peatland, mangrove, saltmarsh, rice paddies -- is **excluded**: those are
#' land that is wet, not surface water, and `build_polycell_support()` books
#' them under `land_area_ha`.
#'
#' Under GLWD v1 the equivalent classes are 1 (lakes) and 3 (rivers). The two
#' vintages are not interchangeable and give totals about 20% apart; see
#' [read_glwd_water()].
#'
#' Class membership is multiplied by the companion `area_pct` raster where one
#' is present, so a partially covered source pixel contributes its own fraction
#' rather than counting whole.
#'
#' @param glwd_dir Directory holding the GLWD rasters, as
#'   `inst/scripts/download/download_hydrology.R` lays them out: `GLWD_v2/` for
#'   v2, or `glwd_3/hdr.adf` / `glwd_3.tif` for v1.
#' @param cells Optional `tibble` of `lon`/`lat` cell centres to sample at.
#'   `NULL` (default) returns every cell of the 0.5-degree grid the rasters
#'   cover.
#'
#' @return A `tibble` with `lon`, `lat` and `water_frac`, carrying a
#'   `"glwd_version"` attribute of `"v1"` or `"v2"`.
#' @export
#'
#' @examples
#' # Requires the GLWD download; not run without it.
#' if (nzchar(Sys.getenv("WHEP_LPJML_INPUT_DIR"))) {
#'   glwd_water_fraction(
#'     file.path(Sys.getenv("WHEP_LPJML_INPUT_DIR"), "GLWD")
#'   )
#' }
glwd_water_fraction <- function(glwd_dir, cells = NULL) {
  rlang::check_installed("terra")
  src <- .glwd_source(glwd_dir)
  classes <- .glwd_water_classes(src$version)

  glwd <- terra::rast(src$path)
  # The class raster is categorical, so it is reclassified to a 0/1 membership
  # mask and only then weighted and averaged. Aggregating class CODES would
  # average the code numbers, which means nothing.
  frac <- .glwd_class_fraction(glwd, classes, .glwd_area_pct(src))
  out <- .glwd_sample(frac, cells)
  attr(out, "glwd_version") <- src$version
  out
}

# v2 is preferred when present: it is what `download_hydrology.R` fetches. The
# 50pct variant is excluded deliberately -- it is a thresholded map, not the
# class map this derivation reclassifies.
.glwd_source <- function(glwd_dir) {
  v2_dir <- file.path(glwd_dir, "GLWD_v2")
  v2 <- Filter(
    \(p) !grepl("50pct", p),
    list.files(
      v2_dir,
      pattern = "(main_class|dominant|combined).*\\.tif$",
      recursive = TRUE,
      full.names = TRUE
    )
  )
  if (length(v2) > 0L) {
    return(list(path = v2[[1L]], version = "v2", dir = v2_dir))
  }
  v1 <- c(
    file.path(glwd_dir, "glwd_3", "hdr.adf"),
    file.path(glwd_dir, "glwd_3.tif")
  )
  v1 <- v1[file.exists(v1)]
  if (length(v1) > 0L) {
    return(list(path = v1[[1L]], version = "v1", dir = glwd_dir))
  }
  cli::cli_abort(c(
    "No GLWD raster found under {.file {glwd_dir}}.",
    i = "Fetch it with {.file inst/scripts/download/download_hydrology.R}."
  ))
}

# Snap a raster-derived coordinate onto WHEP's canonical half-degree centre,
# `k * 0.5 + 0.25`, which is how `.pcs_cells_sf()` forms it.
#
# `terra::xyFromCell()` accumulates float error walking out from the raster
# origin, so a centre it reports as -130.25 is -130.24999999999994. That prints
# identically and compares FALSE, and the water layer joins to the polycells on
# `c("lon", "lat")` -- so without this the join matched 36 of 720 longitudes,
# `inland_water_ha` came out 0.00 Mha worldwide, and every hectare of inland
# water was silently booked as land. Nothing errored: a missing water row is
# legitimately "this cell is dry".
.glwd_snap <- function(x) {
  floor(x / 0.5) * 0.5 + 0.25
}

.glwd_water_classes <- function(version) {
  if (version == "v2") {
    return(c(lake = 1L, lake = 2L, lake = 3L, river = 7L))
  }
  c(lake = 1L, river = 3L)
}

.glwd_area_pct <- function(src) {
  if (src$version != "v2") {
    return(NULL)
  }
  pct <- list.files(
    src$dir,
    pattern = "area_pct.*\\.tif$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(pct) == 0L) {
    return(NULL)
  }
  terra::rast(pct[[1L]]) / 100
}

.glwd_class_fraction <- function(glwd, classes, area_pct) {
  mask <- terra::classify(
    glwd,
    cbind(unname(classes), rep(1, length(classes))),
    others = 0
  )
  if (!is.null(area_pct)) {
    mask <- mask * area_pct
  }
  # NA is DRY, not absent. The raster carries no data over ocean, and the
  # fraction being formed is a fraction of the WHOLE 0.5-degree cell -- so a
  # half-ocean coastal cell must divide its water by the whole cell, not by the
  # land part alone.
  #
  # Averaging with `na.rm = TRUE` instead (which this derivation did while it
  # lived in `inst/scripts/prepare_spatialize_all.R`) takes the mean over the
  # land pixels only and then multiplies by the full cell area downstream,
  # inflating every coastal cell. Measured globally against GLWD's own
  # `area_ha_x10` raster, which never passes through this aggregation at all:
  # 3.6066 Mkm2 that way against 3.3903 Mkm2 measured directly, +6.4%. Treating
  # NA as dry gives 3.3814 Mkm2, -0.26% -- the residual is cells straddling the
  # raster edge, not the mask.
  mask <- terra::classify(mask, cbind(NA, 0))
  factor <- round(0.5 / terra::res(glwd)[[1L]])
  terra::aggregate(mask, fact = factor, fun = "mean", na.rm = FALSE)
}

# Water is a fraction, so a cell the raster does not cover is dry rather than
# unknown: `NA` here would propagate into `land_area_ha` as `NA` instead of
# leaving the cell wholly land.
.glwd_sample <- function(frac, cells) {
  if (is.null(cells)) {
    xy <- terra::xyFromCell(frac, seq_len(terra::ncell(frac)))
    values <- terra::values(frac)[, 1L]
    return(tibble::tibble(
      lon = .glwd_snap(xy[, 1L]),
      lat = .glwd_snap(xy[, 2L]),
      water_frac = pmin(1, pmax(0, dplyr::coalesce(values, 0)))
    ))
  }
  .pcs_require_cols(cells, c("lon", "lat"), "cells")
  # A caller's own coordinates are returned unchanged: they are the key the
  # caller will join on, and snapping them would be this function deciding
  # what grid the caller is using.
  values <- terra::values(frac)[
    terra::cellFromXY(frac, as.matrix(cells[, c("lon", "lat")])),
    1L
  ]
  tibble::tibble(
    lon = cells$lon,
    lat = cells$lat,
    water_frac = pmin(1, pmax(0, dplyr::coalesce(values, 0)))
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
#' A support table may carry a second, **non-partitioning** layer: the
#' aggregate polities of [build_polycell_support()]`(aggregates =
#' "overlap_layer")`, whose polygons cover their members' and therefore claim
#' ground twice. This returns the partition alone unless asked otherwise, so a
#' consumer that never heard of the layer cannot pick a row of it up by
#' accident.
#'
#' @param path Optional path to a local parquet, overriding the environment
#'   variable and the pin.
#' @param version Pin version, passed to [whep_read_file()]. `NULL` takes the
#'   version frozen in [whep_inputs].
#' @param role Which layer to return. `"partition"` (default) is the rows that
#'   partition each cell -- every row of a table built with the default
#'   `aggregates = "exclude"`, and every row of any table published before
#'   whep#803. `"overlap"` is the aggregate layer alone, for a consumer that
#'   needs the territory of a reporting bucket whose only polity is an
#'   aggregate; it aborts rather than returning nothing when the table carries
#'   no such layer. `"all"` returns both and is only correct where the two are
#'   kept apart afterwards -- summing across them double-counts every member an
#'   aggregate covers.
#'
#' @return A `tibble` in the [build_polycell_support()] grain.
#' @export
#'
#' @examples
#' # Requires WHEP_POLYCELL_SUPPORT_PATH or a published pin; not run without it.
#' if (nzchar(Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH"))) {
#'   read_polycell_support()
#' }
read_polycell_support <- function(
  path = NULL,
  version = NULL,
  role = c("partition", "overlap", "all")
) {
  role <- rlang::arg_match(role)
  path <- path %||% Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH", "")
  if (nzchar(path)) {
    if (!file.exists(path)) {
      cli::cli_abort("Polycell support table not found at {.file {path}}.")
    }
    support <- tibble::as_tibble(nanoparquet::read_parquet(path))
    return(.polycell_support_role(support, role))
  }
  support <- tryCatch(
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
  .warn_polycell_vintage(support)
  .polycell_support_role(support, role)
}

# The pin is a BUILD ARTEFACT of `whep::polities`, and merge order cannot keep
# the two in step: a PR that re-syncs the vocabulary and a PR that regenerates
# the pin are both correct in isolation and land against different snapshots.
# That has now happened three times (whep#890, whep#905, whep#908), and each
# time it was found by a consumer tripping over a missing territory rather than
# by anything checking.
#
# So the reader says so. It compares the pin's polity set against what
# `build_polycell_support()` would emit from today's vocabulary, and names the
# territories that would be absent. It warns rather than aborts because a stale
# pin is usable -- the gap is small and peripheral by construction, since a
# newly minted polity is usually a small territory -- and because aborting would
# make every downstream reader fail on a data refresh nobody has run yet.
.warn_polycell_vintage <- function(support) {
  prepared <- tryCatch(
    .pcs_prepare_polities(whep::polities),
    error = function(e) NULL
  )
  if (is.null(prepared) || !rlang::has_name(support, "polity_code")) {
    return(invisible(support))
  }
  # Only polities that COULD have cells. Fourteen live polities carry no polygon
  # at all (`polygon_status == "unassigned"`, e.g. CAN-1800-1866, PRY-1811-1870),
  # so the producer cannot emit cells for them and never will until upstream
  # draws one. Comparing against the whole prepared set would warn about those
  # fourteen on every read, forever -- a guard that cries wolf is worse than no
  # guard, because the real staleness then arrives inside a warning people have
  # learned to skip. Measured: regenerating against an up-to-date vocabulary
  # recovers 2 polities and leaves exactly these 14.
  can_have_cells <- prepared$polity_code[
    !(prepared$polity_code %in% .polities_without_polygon())
  ]
  missing <- setdiff(unique(can_have_cells), unique(support$polity_code))
  if (length(missing) == 0L) {
    return(invisible(support))
  }
  cli::cli_warn(c(
    "!" = "The {.val polycell_support} pin is behind {.code whep::polities}:
           {length(missing)} polit{?y/ies} in the vocabulary {?has/have} no
           cells in the pin.",
    "*" = "{.val {sort(missing)}}",
    "i" = "Regenerate with {.fn build_polycell_support} and re-upload; see
           {.file data-raw/} and {.file inst/scripts/prepare_upload.R}."
  ))
  invisible(support)
}

# The default is the PARTITION, and that is the whole consumer-side contract of
# whep#803. The producer may now emit aggregate polities, whose polygons cover
# their members', and every consumer that reads this table sums hectares over
# it. Defaulting to everything would turn admitting an aggregate
# into a silent double count of every member it covers, in a caller that never
# asked for one.
#
# A table with no `support_role` column is not a failure: it is every polycell
# published before whep#803, and every row of it partitions. It answers
# `"partition"` and `"all"` identically and truthfully. What it cannot answer is
# `"overlap"`, and that ABORTS rather than returning zero rows -- a consumer
# asking for a bucket's territory and silently receiving none would report the
# bucket as having no land, which is the very failure whep#803 exists to fix.
.polycell_support_role <- function(support, role) {
  if (identical(role, "all")) {
    return(support)
  }
  roles <- support[["support_role"]] %||% rep("partition", nrow(support))
  if (identical(role, "partition")) {
    return(support[!(roles %in% "overlap"), , drop = FALSE])
  }
  if (!any(roles %in% "overlap")) {
    cli::cli_abort(
      c(
        "This polycell support carries no {.val overlap} layer.",
        i = "Rebuild it with {.code build_polycell_support(aggregates =
           \"overlap_layer\")}, which clips the aggregate polities too, or read
           it with {.code role = \"partition\"}."
      ),
      class = "whep_polycell_no_overlap_layer"
    )
  }
  support[roles %in% "overlap", , drop = FALSE]
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

# -- s2 validity --------------------------------------------------------------

# Repair polygons that the spherical engine rejects. GEOS-level make_valid
# splits the self-intersections into pieces s2 usually accepts; whatever is
# still invalid is kept but excluded from the s2 geometry so one bad loop
# cannot abort the whole producer, and its geodesic area is measured with
# `terra::expanse()` instead. This is not hypothetical on either layer: four of
# the 1,886 `ne_10m_glaciated_areas` features and seven shipped polity polygons
# are s2-invalid and `sf::st_area()` aborts on all of them. The planar repair
# recovers three of the four ice features and four of the seven polities,
# leaving one ice feature and three polities unusable.
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


# Live polities with no polygon, read from the vocabulary rather than listed, so
# it shrinks by itself as upstream draws them (whep-polities#155, #3).
.polities_without_polygon <- function() {
  pol <- sf::st_drop_geometry(whep::polities)
  keep <- is.na(pol$has_geometry) |
    !pol$has_geometry |
    (!is.na(pol$polygon_status) & pol$polygon_status == "unassigned")
  unique(pol$polity_code[keep])
}
