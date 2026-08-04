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
# WHERE states.nc COMES FROM (issue #457, settled 2026-08-04): the reference
# payload is the states asset of Zenodo record 15556812 (LUH2-GCB2022, CC-BY-4.0,
# doi:10.5281/zenodo.15556812), 6,657,587,367 bytes, MD5
# 411ef3d657c3108942954c895f658a17. It is fetched on demand into
# rappdirs::user_cache_dir("whep")/luh2 and verified against that MD5. The
# retired "luh2_v2h_states" SACO pin was a byte-identical mirror of this same
# file (confirmed by MD5), so the pin added a second, unversioned copy with no
# checksum and no stated reason -- see the PR for #457.
#
# luh.umd.edu serves the same bytes at LUH2/LUH2_GCB_2022/states.nc, but its TLS
# chain does not verify (hence the -k workaround in
# inst/scripts/download/download_luh2.R), which is why Zenodo is the source of
# record and where the MD5 comes from.
#
# The reference vintage is LUH2-GCB2022 (source_id
# "UofMD-landState-LUH2-GCB2022", 1173 yearly steps, 850-2022), NOT the base v2h
# release (1166 steps, 850-2015, and a different byte size). Both trees exist in
# the wild and give different results, so the vintage actually read is recorded
# on the output via attach_provenance(), and a local WHEP_LUH2_DIR tree that is
# not the reference vintage warns. .luh2_nc_years() derives the calendar span
# from the file's own time axis, so either vintage reads correctly.

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
#' The states grid comes from a `WHEP_LUH2_DIR` tree when there is one, else the
#' reference LUH2-GCB2022 `states.nc` is downloaded on demand from Zenodo
#' (doi:10.5281/zenodo.15556812, CC-BY-4.0), verified against its published MD5
#' and cached. Whichever is read, the vintage (the NetCDF `source_id`, e.g.
#' `"UofMD-landState-LUH2-GCB2022"`) is recorded on the result with
#' [attach_provenance()], and a local tree that is not the reference vintage
#' warns: the base v2h release and the annual Global Carbon Budget variants cover
#' different years and do not agree.
#'
#' @param resolution `"grid"` (default, per cell and class) or `"polity"`
#'   (aggregated to `area_code` per year and class).
#' @param years Optional integer vector of calendar years to keep. `NULL` keeps
#'   every year present in the source.
#' @param states_source Which states source to read: `"auto"` (default, a
#'   `WHEP_LUH2_DIR` tree when present, else the Zenodo download), `"local"`
#'   (`WHEP_LUH2_DIR` only, an error without it) or `"zenodo"` (the
#'   checksum-verified reference vintage only, ignoring any local tree). Recorded
#'   in the provenance record's `input_origin`.
#' @param data Named list of pre-loaded inputs bypassing the readers: `states`
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
#'   The reference payload is the Global Carbon Budget vintage of that release:
#'   Chini, L. et al. (2021). Land-use harmonization datasets for annual global
#'   carbon budgets. Earth System Science Data 13, 4175-4189.
#'   \doi{10.5194/essd-13-4175-2021}. Data: LUH2-GCB2022,
#'   \doi{10.5281/zenodo.15556812} (CC-BY-4.0).
#' @export
#' @examples
#' read_luh2_landuse(example = TRUE)
read_luh2_landuse <- function(
  resolution = c("grid", "polity"),
  years = NULL,
  states_source = c("auto", "local", "zenodo"),
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

# Choose the states source. "auto" prefers a local WHEP_LUH2_DIR tree, because a
# local file costs nothing and its vintage is identified from the NetCDF's own
# source_id, then falls back to the Zenodo cache. "zenodo" ignores the local tree
# and insists on the checksum-verified reference vintage, which is the lever for
# a reproducible run; "local" insists on the local tree and aborts without one.
.luh2_read_states_source <- function(years = NULL, states_source = "auto") {
  local_nc <- if (states_source == "zenodo") NULL else .luh2_local_states_nc()
  if (!is.null(local_nc)) {
    return(.luh2_read_states_nc(local_nc, years = years, origin = "local"))
  }
  if (states_source == "local") {
    cli::cli_abort(c(
      "No local LUH2 states tree is available.",
      i = "Set {.envvar WHEP_LUH2_DIR} to a directory holding
           {.file states.nc}, or use {.code states_source = \"zenodo\"}."
    ))
  }
  .luh2_read_states_nc(.luh2_zenodo_states(), years = years, origin = "zenodo")
}

# WHEP_LUH2_DIR/states.nc when it exists, else NULL. An unset variable must not
# resolve to a bare "states.nc" in the working directory.
.luh2_local_states_nc <- function() {
  states_dir <- .luh2_states_dir()
  if (!.has_path(states_dir)) {
    return(NULL)
  }
  nc_path <- file.path(states_dir, "states.nc")
  if (file.exists(nc_path)) nc_path else NULL
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
  .luh2_warn_off_vintage(provenance)
  years <- years %||% .luh2_nc_years(nc_path)
  purrr::map_dfr(years, \(yr) .luh2_read_states_nc_year(nc_path, yr)) |>
    attach_provenance(provenance)
}

# Record which LUH2 product a states.nc actually is, in the record_provenance()
# column shape so get_provenance() reads the same either way. The base v2h
# release (850-2015) and the annual Global Carbon Budget variants (850-2022 for
# GCB2022) are different products that reproduce different residual statistics,
# so a result must carry the one that produced it instead of citing the base
# release by assumption. `input_version` names the Zenodo record only for the
# checksum-verified download; a local tree is identified by its own source_id.
.luh2_states_provenance <- function(nc_path, origin) {
  years <- .luh2_nc_years(nc_path)
  tibble::tibble(
    recorded_at = Sys.time(),
    whep_version = as.character(utils::packageVersion("whep")),
    r_version = as.character(getRversion()),
    input_alias = "luh2_states",
    input_version = if (origin == "zenodo") {
      .luh2_states_doi()
    } else {
      NA_character_
    },
    input_origin = origin,
    input_source_id = .luh2_nc_source_id(nc_path),
    input_first_year = min(years),
    input_last_year = max(years)
  )
}

.luh2_states_doi <- function() "10.5281/zenodo.15556812"

# Warn when a local tree is not the reference vintage. Not an error: pointing
# WHEP_LUH2_DIR at the base v2h release is a legitimate choice, it just has to be
# a visible one, since that is the substitution issue #457 could not detect.
.luh2_warn_off_vintage <- function(provenance) {
  if (provenance$input_origin != "local") {
    return(invisible(NULL))
  }
  found <- provenance$input_source_id
  reference <- .luh2_reference_source_id()
  if (identical(found, reference)) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{.envvar WHEP_LUH2_DIR} holds {.val {found}}, not the reference vintage
     {.val {reference}}.",
    i = "Results are not comparable across vintages. Use
         {.code states_source = \"zenodo\"} for the verified reference."
  ))
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

# -- Zenodo states cache ------------------------------------------------------

# Path to the reference states.nc, downloading it into the WHEP cache on first
# use. Same download-on-demand pattern read_critical_n() and
# .provinces_shapefile() use, with the size raising the stakes: 6.7 GB, fetched
# once and kept. `download` is injected so the cache-hit path is testable
# without touching the network.
.luh2_zenodo_states <- function(
  dir = .luh2_cache_dir(),
  download = .luh2_download_states
) {
  path <- file.path(dir, "states.nc")
  if (.luh2_cached_size_ok(path)) {
    return(path)
  }
  download(path)
}

.luh2_cache_dir <- function() {
  file.path(rappdirs::user_cache_dir("whep"), "luh2")
}

# The states asset of Zenodo record 15556812 (LUH2-GCB2022, Chini et al. 2021,
# CC-BY-4.0), its published MD5 and byte size, all read off the record's API
# metadata. luh.umd.edu serves the same bytes at LUH2/LUH2_GCB_2022/states.nc,
# but over a chain that fails verification, so Zenodo is the source of record.
.luh2_states_url <- function() {
  paste0(
    "https://zenodo.org/api/records/15556812/files/",
    "states4.nc/content"
  )
}

.luh2_states_md5 <- function() "411ef3d657c3108942954c895f658a17"

.luh2_states_bytes <- function() 6657587367

# The vintage the reference payload is, for comparison against a local tree.
.luh2_reference_source_id <- function() "UofMD-landState-LUH2-GCB2022"

# A cache hit is checked by byte size, not by MD5: re-hashing 6.7 GB would cost
# ~20 s on every read. The size catches the realistic failure (a truncated
# download); the full MD5 runs once, when the download completes.
.luh2_cached_size_ok <- function(path) {
  file.exists(path) && file.size(path) == .luh2_states_bytes()
}

# Fetch states.nc into the cache and verify it against the published MD5. A
# partial file is discarded rather than kept, so a failed download cannot leave
# something that later passes the size check. `fetch` is injected so the timeout
# and failure handling are testable without moving 6.7 GB.
.luh2_download_states <- function(path, fetch = .luh2_fetch_states) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # R's download timeout defaults to 60 s, which aborts a 6.7 GB fetch long
  # before it can finish. Raise it for the duration and put it back after.
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, .luh2_download_timeout()))
  cli::cli_alert_info(
    "Downloading LUH2-GCB2022 states.nc (6.7 GB) from Zenodo; this is cached
     at {.path {dirname(path)}} and fetched only once."
  )
  ok <- tryCatch(fetch(path), error = function(e) e)
  .luh2_verify_download(path, ok)
  path
}

# 4 h, enough for 6.7 GB on a link slow enough to be painful but not broken.
.luh2_download_timeout <- function() 14400

.luh2_fetch_states <- function(path) {
  utils::download.file(.luh2_states_url(), path, mode = "wb", quiet = FALSE)
}

# Verify the fetched payload, removing it when it does not match so the next call
# re-downloads instead of reading a corrupt grid.
.luh2_verify_download <- function(path, download_result) {
  failed <- inherits(download_result, "error")
  digest <- if (failed) NA_character_ else unname(tools::md5sum(path))
  if (failed || !identical(digest, .luh2_states_md5())) {
    unlink(path)
    # cli >= 3.4 reads a leading dot in {} as a style, so the URL has to reach
    # the message through a local binding rather than a dotted call.
    url <- .luh2_states_url()
    cli::cli_abort(c(
      "Could not download the LUH2-GCB2022 states grid.",
      x = if (failed) {
        conditionMessage(download_result)
      } else {
        "The downloaded file does not match the published MD5."
      },
      i = "Download {.url {url}} by hand and point {.envvar WHEP_LUH2_DIR} at
           the directory holding it, or run
           {.file inst/scripts/download/download_luh2.R}."
    ))
  }
  cli::cli_alert_success("LUH2-GCB2022 states.nc verified against its MD5.")
  invisible(path)
}

.luh2_read_country_grid <- function() {
  whep_read_file("spatialize-country-grid")
}
