# Gridded soil pH from HWSD (Harmonized World Soil Database), promoted from
# the tested prepare_soil_inputs() pipeline in
# inst/scripts/prepare_spatialize_all.R: the attribute reader, dominant-soil
# selector, raster aggregator and gap-filler helpers.
#
# CONFIRMED HWSD FACTS (source pipeline inspected; do not re-guess):
# - hwsd_data.csv (attribute table) holds one row per HWSD soil map unit
#   (mu_global) x texture-class share, with columns mu_global, t_usda_tex
#   (HWSD2 USDA texture code, 1-13), share (percent of the map unit covered
#   by that texture class) and t_ph_h2o (topsoil pH in water for that
#   texture class; may be NA).
# - hwsd.bil is the accompanying raster (ESRI .bil), whose cell values are
#   mu_global map-unit IDs. Soil pH is not read from the raster directly:
#   the raster is reclassified (mu_global -> pH of that unit's DOMINANT
#   texture class) and then spatially aggregated with terra.
# - Dominant texture per map unit = texture class with the largest summed
#   `share`; that class's own pH is used (not an area-weighted pH across
#   all classes). Missing pH defaults to 7.0 (neutral), matching the
#   existing pipeline's own documented fallback.
# - Soil pH is a static property of the HWSD map (no time dimension): this
#   reader returns no `year` column, the same convention used for other
#   static coefficient tables (e.g. `whep::soil_cn_ratios`).
# - Local dev data dir is read from Sys.getenv("WHEP_HWSD_DIR"); never
#   hardcode an absolute path in committed code.
#
# EXEMPT from the `polity_validity` year-check (whep#675). These readers take
# `data$cell_polity` as a spatial EXTENT and gap-fill target only: they never
# join a territory onto a year, and their output carries neither `year` nor
# `area_code`. There is therefore no (area_code, year) pair that could name a
# polity which did not exist -- a soil property is about the place, not the
# state, which is #671's option 3. The same holds for `.cb_hwsd_clay()` in
# R/carbon_balance.R, which reuses `.aggregate_hwsd()` the same way.

#' Read gridded soil pH onto WHEP's grid.
#'
#' @description
#' Reads the HWSD (Harmonized World Soil Database) soil map unit attribute
#' table and raster, derives each map unit's pH from its dominant USDA
#' texture class, and aggregates the result to WHEP's 0.5-degree grid by
#' averaging the native HWSD cells inside each 0.5-degree block. Soil pH is
#' a static HWSD property: the result has no `year` column. When
#' `data$cell_polity` is supplied, the native HWSD raster is first cropped to
#' that grid's extent before reclassification (so a regional caller never
#' materialises or reclassifies the full-resolution global raster), and cells
#' present in that target grid but missing from the aggregated HWSD grid are
#' gap-filled from the nearest available neighbour; otherwise cropping and
#' gap-filling are both skipped and the returned grid covers every cell where
#' HWSD itself has data.
#'
#' @param hwsd_dir Path to the directory holding `hwsd_data.csv` and
#'   `hwsd.bil`. Defaults to `Sys.getenv("WHEP_HWSD_DIR")`.
#' @section Caching:
#' Aggregating the HWSD raster to the 0.5-degree grid takes about an hour per
#' pass, and its result depends only on the archive and the target grid, so it
#' is cached under `rappdirs::user_cache_dir("whep")`. The cache key covers the
#' archive's raster and header (size and modification time), the resolution,
#' the requested columns, the target grid's cells and the derived map-unit
#' values, plus an algorithm version that is bumped whenever a change would
#' move the numbers. Set `WHEP_HWSD_CACHE_DIR` to relocate it; the test suite
#' points it at a temporary directory so a fixture-derived grid can never reach
#' a real cache.
#' @param data Optional named list of pre-loaded inputs: `cell_polity`
#'   (`lon`, `lat`, at minimum), used both to crop the HWSD raster to the
#'   region of interest before reclassification and as the target grid for
#'   gap-filling. When absent, cropping and gap-filling are both skipped
#'   (documented fallback above).
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `soil_ph`.
#' @export
#' @examples
#' read_soil_ph(example = TRUE)
read_soil_ph <- function(hwsd_dir = NULL, data = list(), example = FALSE) {
  if (isTRUE(example)) {
    return(.example_soil_ph())
  }
  rlang::check_installed("terra")
  dir <- .resolve_hwsd_dir(hwsd_dir)
  mu_soils <- .read_hwsd_attributes_local(dir, required = .hwsd_ph_columns()) |>
    .derive_dominant_soil()
  soil_grid <- .aggregate_hwsd(
    dir,
    mu_soils,
    target_res = 0.5,
    target_grid = data$cell_polity,
    value_col = "t_ph_h2o",
    out_col = "soil_ph"
  )
  if (is.null(data$cell_polity)) {
    return(soil_grid)
  }
  .gapfill_soil(soil_grid, data$cell_polity)
}

#' Read gridded soil hydraulic properties from HWSD onto WHEP's grid.
#'
#' @description
#' Reads the HWSD (Harmonized World Soil Database) soil map unit attribute
#' table and raster, resolves each map unit's dominant USDA texture class,
#' looks up that class's volumetric field capacity, wilting point and porosity
#' from [soil_hydraulic_by_texture] (via the [hwsd_texture_usda] code
#' crosswalk), and aggregates each property to WHEP's 0.5-degree grid by
#' averaging the native HWSD cells inside each 0.5-degree block. These are the
#' per-cell soil hydraulic drivers the ICBM soil-carbon moisture modifier
#' consumes. Soil texture is a static HWSD property: the result has no `year`
#' column. Cropping to `data$cell_polity` follows the same regional-crop path
#' as [read_soil_ph()]; missing cells are gap-filled from the nearest
#' available neighbour when a target grid is supplied.
#'
#' @param hwsd_dir Path to the directory holding `hwsd_data.csv` and
#'   `hwsd.bil`. Defaults to `Sys.getenv("WHEP_HWSD_DIR")`. Supplying it
#'   derives the grid locally rather than reading the pin.
#' @param source Where the grid comes from. `"auto"` (default) reads the
#'   published pin unless `hwsd_dir` is given; `"pin"` always reads it;
#'   `"local"` always derives it from an HWSD archive. The pin is preferred so
#'   that every user shares one vintage: HWSD exists in two incompatible
#'   versions (v1.2 topsoil 0-30 cm, HWSD2 D1 0-20 cm) and the carbon balance
#'   reports 0-30 cm.
#' @param version Pin version, passed to [whep_read_file()]. `NULL` takes the
#'   version frozen in [`whep_inputs`].
#' @section Caching:
#' Aggregating the HWSD raster to the 0.5-degree grid takes about an hour per
#' pass, and its result depends only on the archive and the target grid, so it
#' is cached under `rappdirs::user_cache_dir("whep")`. The cache key covers the
#' archive's raster and header (size and modification time), the resolution,
#' the requested columns, the target grid's cells and the derived map-unit
#' values, plus an algorithm version that is bumped whenever a change would
#' move the numbers. Set `WHEP_HWSD_CACHE_DIR` to relocate it; the test suite
#' points it at a temporary directory so a fixture-derived grid can never reach
#' a real cache.
#' @param data Optional named list of pre-loaded inputs: `cell_polity`
#'   (`lon`, `lat`, at minimum), used both to crop the HWSD raster and as the
#'   gap-filling target grid.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `t_field` (volumetric field capacity),
#'   `t_wilt` (volumetric wilting point) and `porosity`, each a fraction.
#' @export
#' @examples
#' read_soil_hydraulic(example = TRUE)
read_soil_hydraulic <- function(
  hwsd_dir = NULL,
  data = list(),
  source = c("auto", "pin", "local"),
  version = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_soil_hydraulic())
  }
  counts <- .resolve_hwsd_grid(
    alias = .hwsd_texture_pin(),
    cols = paste0("n_", .hwsd_texture_classes()),
    derive = .derive_hwsd_texture_counts,
    hwsd_dir = hwsd_dir,
    data_grid = data$texture_counts,
    source = source,
    version = version,
    target_grid = data$cell_polity
  )
  # The three hydraulic columns are computed HERE, from whichever
  # `soil_hydraulic_by_texture` the installed package carries, so a revision of
  # that table reaches pinned and local users alike.
  grid <- .hydraulic_from_class_counts(counts)
  if (is.null(data$cell_polity)) {
    return(grid)
  }
  .gapfill_soil_hydraulic(grid, data$cell_polity)
}

# Aggregate the per-class pixel counts from a local HWSD archive. Kept apart
# from the reader so the pin branch and the local branch join at exactly one
# point, `.resolve_hwsd_grid()`.
.derive_hwsd_texture_counts <- function(dir, target_grid) {
  rlang::check_installed("terra")
  mu_class <- .read_hwsd_attributes_local(
    dir,
    required = .hwsd_texture_columns()
  ) |>
    .derive_dominant_texture() |>
    dplyr::inner_join(whep::hwsd_texture_usda, by = "t_usda_tex") |>
    dplyr::select("mu_global", "usda_texture_class") |>
    dplyr::distinct()
  .aggregate_hwsd_classes(dir, mu_class, target_grid = target_grid)
}

#' Read observed topsoil organic carbon from HWSD onto WHEP's grid.
#'
#' @description
#' Read the observed **0-30 cm** soil organic carbon stock per 0.5-degree
#' cell from the HWSD (Harmonized World Soil Database) map-unit attribute
#' table and raster. Each map unit's stock is the share-weighted mean over
#' its soil components of `t_oc * bulk_density * 30 * (1 - t_gravel / 100)`,
#' and the map units are then averaged over the native HWSD cells inside
#' each 0.5-degree block by the same aggregation [read_soil_ph()] uses.
#'
#' This exists to be a **benchmark**, not a model input: nothing in the
#' carbon pipeline consumes it. HWSD version 1.2's topsoil is 0-30 cm,
#' exactly the layer [build_carbon_balance()] reports (see its Soil depth
#' section), so it is the only observational anchor available at WHEP's own
#' modelled depth -- and it comes out of the archive the carbon balance
#' already reads for clay, so it needs no new download. Note that HWSD**2**,
#' which `inst/scripts/download/download_hwsd.R` fetches, layers its topsoil
#' as D1 = 0-20 cm instead; the two are not interchangeable (whep#851).
#'
#' @param bulk_density Which HWSD bulk density to use. `"measured"`
#'   (default) takes `t_bulk_density`, falling back to
#'   `t_ref_bulk_density` where it is absent; `"reference"` takes
#'   `t_ref_bulk_density` alone. The default is not cosmetic:
#'   `t_ref_bulk_density` is derived from texture and so knows nothing about
#'   organic matter, and over the 1,375 map units with `t_oc` above 6% it
#'   averages 1.33 against a measured 0.37, which would inflate a peat
#'   soil's carbon stock roughly 3.6-fold. Recorded in `method_soc_obs`.
#' @inheritParams read_soil_ph
#' @return A tibble with `lon`, `lat`, `soc_obs_mgc_ha` (0-30 cm soil organic
#'   carbon, Mg C per ha) and `method_soc_obs`.
#' @source FAO/IIASA/ISRIC/ISSCAS/JRC (2012). *Harmonized World Soil Database
#'   version 1.2*. FAO, Rome and IIASA, Laxenburg -- topsoil defined as
#'   0-30 cm. Stock equation and the bulk-density caveat: Hiederer, R. &
#'   Koechy, M. (2011). *Global Soil Organic Carbon Estimates and the
#'   Harmonized World Soil Database*. EUR 25225 EN, Publications Office of
#'   the European Union, 79 pp.
#' @export
#' @examples
#' read_hwsd_topsoil_soc(example = TRUE)
read_hwsd_topsoil_soc <- function(
  hwsd_dir = NULL,
  bulk_density = c("measured", "reference"),
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_hwsd_topsoil_soc())
  }
  bulk_density <- rlang::arg_match(bulk_density)
  rlang::check_installed("terra")
  dir <- .resolve_hwsd_dir(hwsd_dir)
  mu_soc <- .read_hwsd_attributes_local(
    dir,
    required = .hwsd_soc_columns(bulk_density)
  ) |>
    .derive_map_unit_soc(bulk_density)
  .aggregate_hwsd(
    dir,
    mu_soc,
    target_res = 0.5,
    target_grid = data$cell_polity,
    value_col = "soc_obs_mgc_ha",
    out_col = "soc_obs_mgc_ha"
  ) |>
    dplyr::mutate(method_soc_obs = bulk_density)
}

# ---- Private helpers --------------------------------------------------

# The hwsd_data.csv columns each HWSD reader needs, named once so a caller's
# column contract and a test's skip guard cannot drift apart.
.hwsd_ph_columns <- function() {
  c("mu_global", "share", "t_usda_tex", "t_ph_h2o")
}

.hwsd_texture_columns <- function() {
  c("mu_global", "share", "t_usda_tex")
}

# Topsoil clay fraction (% weight), HWSD field T_CLAY: FAO/IIASA/ISRIC/ISSCAS/
# JRC (2012) "Harmonized World Soil Database version 1.2", attribute database
# field list ("T_CLAY: Topsoil Clay Fraction, % wt.").
.hwsd_clay_columns <- function() {
  c("mu_global", "share", "t_clay")
}

# Topsoil organic carbon (T_OC, % weight), bulk density and gravel content,
# the HWSD fields the 0-30 cm carbon benchmark needs. t_bulk_density is
# required only by the "measured" method, so a "reference" run is not
# refused over a column it never reads.
.hwsd_soc_columns <- function(bulk_density = "measured") {
  base <- c(
    "mu_global",
    "share",
    "t_oc",
    "t_ref_bulk_density",
    "t_gravel",
    "topsoil_depth_cm"
  )
  if (bulk_density == "measured") c(base, "t_bulk_density") else base
}

# Per-map-unit topsoil carbon stock (MgC/ha), share-weighted over the map
# unit's soil components. HWSD 1.2's topsoil is 0-30 cm, so the stock is
#   t_oc / 100 * bulk [kg/m3] * 0.3 [m] * (1 - gravel)   kg C / m2
# which, with bulk in g/cm3 and 1 kg/m2 = 10 Mg/ha, collapses to
#   t_oc * bulk * depth * (1 - gravel)                   Mg C / ha
#
# The depth is READ FROM THE FILE (`topsoil_depth_cm`), not assumed: HWSD
# v1.2's topsoil is 0-30 cm and HWSD2's D1 is 0-20 cm, and both producers
# write this same artifact. Hardcoding 30 would overstate an HWSD2 extract
# by half (whep#851).
# (Hiederer & Koechy 2011). A component with no carbon or no density reports
# nothing rather than zero, so it is dropped from its map unit's mean
# instead of dragging it down.
.derive_map_unit_soc <- function(attrs, bulk_density) {
  attrs |>
    dplyr::mutate(
      bulk = if (bulk_density == "measured") {
        dplyr::coalesce(.data$t_bulk_density, .data$t_ref_bulk_density)
      } else {
        .data$t_ref_bulk_density
      },
      gravel = dplyr::coalesce(pmin(pmax(.data$t_gravel, 0), 100), 0) / 100
    ) |>
    dplyr::filter(!is.na(.data$t_oc), !is.na(.data$bulk)) |>
    dplyr::mutate(
      soc_obs_mgc_ha = .data$t_oc *
        .data$bulk *
        .data$topsoil_depth_cm *
        (1 - .data$gravel)
    ) |>
    dplyr::summarise(
      soc_obs_mgc_ha = stats::weighted.mean(
        .data$soc_obs_mgc_ha,
        .data$share
      ),
      .by = "mu_global"
    )
}
# Resolve the HWSD data directory from the argument, else the env var.
.resolve_hwsd_dir <- function(hwsd_dir) {
  resolved <- hwsd_dir %||% Sys.getenv("WHEP_HWSD_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No HWSD soil directory available.",
      i = "Pass {.arg hwsd_dir} or set {.envvar WHEP_HWSD_DIR}."
    ))
  }
  resolved
}

# Read the HWSD map-unit x texture-class attribute table, checking it carries
# the columns the caller is about to read. hwsd_data.csv is derived locally
# (inst/scripts/export_hwsd_attributes.R or download/download_hwsd.R), so a
# partial extract is an ordinary state: without this contract a missing column
# surfaced as a dplyr "Column `t_clay` not found in `.data`" error, which names
# a tidyselect internal instead of the stale extract (whep#596).
.read_hwsd_attributes_local <- function(hwsd_dir, required = character()) {
  csv_path <- file.path(hwsd_dir, "hwsd_data.csv")
  if (!file.exists(csv_path)) {
    cli::cli_abort("HWSD CSV not found at {.file {csv_path}}.")
  }
  absent <- .hwsd_missing_columns(hwsd_dir, required)
  if (length(absent) > 0) {
    cli::cli_abort(c(
      "The HWSD extract at {.file {csv_path}} lacks the column{?s}
       {.field {absent}}.",
      i = "Re-export it with
           {.path inst/scripts/export_hwsd_attributes.R}, which writes every
           column the HWSD readers need."
    ))
  }
  readr::read_csv(csv_path, show_col_types = FALSE)
}

# Which of `required` a local HWSD extract does not carry, or "hwsd_data.csv"
# when the extract itself is absent. Reads the header only, so a test's skip
# guard can call it cheaply and state the same precondition the reader
# enforces instead of drifting from it.
.hwsd_missing_columns <- function(hwsd_dir, required) {
  csv_path <- file.path(hwsd_dir, "hwsd_data.csv")
  if (!.has_path(hwsd_dir) || !file.exists(csv_path)) {
    return("hwsd_data.csv")
  }
  header <- readr::read_csv(csv_path, n_max = 0, show_col_types = FALSE)
  required[!rlang::has_name(header, required)]
}

# For each map unit, pick the dominant (largest summed share) USDA texture
# class code. Shared by the pH and soil-hydraulic derivations.
.derive_dominant_texture <- function(hwsd_attr) {
  hwsd_attr |>
    dplyr::filter(!is.na(.data$t_usda_tex)) |>
    dplyr::summarise(
      tex_share = sum(.data$share, na.rm = TRUE),
      .by = c("mu_global", "t_usda_tex")
    ) |>
    dplyr::slice_max(
      .data$tex_share,
      n = 1,
      with_ties = FALSE,
      by = "mu_global"
    ) |>
    dplyr::select("mu_global", "t_usda_tex")
}

# For each map unit, pick the pH of its dominant (largest-share) USDA
# texture class, defaulting missing pH to 7.0 (neutral).
.derive_dominant_soil <- function(hwsd_attr) {
  soils <- hwsd_attr |> dplyr::filter(!is.na(.data$t_usda_tex))
  dom_tex <- .derive_dominant_texture(soils)
  ph_data <- soils |>
    dplyr::inner_join(dom_tex, by = c("mu_global", "t_usda_tex")) |>
    dplyr::slice_max(
      .data$share,
      n = 1,
      with_ties = FALSE,
      by = "mu_global"
    ) |>
    dplyr::select("mu_global", "t_ph_h2o")
  dom_tex |>
    dplyr::left_join(ph_data, by = "mu_global") |>
    dplyr::mutate(
      t_ph_h2o = dplyr::if_else(is.na(.data$t_ph_h2o), 7.0, .data$t_ph_h2o)
    ) |>
    dplyr::select("mu_global", "t_ph_h2o")
}

# Per map unit, the volumetric field capacity, wilting point and porosity of
# its dominant USDA texture class, from soil_hydraulic_by_texture keyed via the
# hwsd_texture_usda code crosswalk. Map units whose dominant code is not in the
# crosswalk (e.g. HWSD rock/ice) drop out, so they aggregate to NA and are
# gap-filled downstream.
# Also retained as the oracle: the map-unit -> dominant class -> class-constant
# lookup that `.hydraulic_from_class_counts()` now performs after aggregation
# rather than before it. See the note on `.aggregate_hwsd_hydraulic()`.
.derive_map_unit_hydraulic <- function(hwsd_attr) {
  hwsd_attr |>
    .derive_dominant_texture() |>
    dplyr::inner_join(whep::hwsd_texture_usda, by = "t_usda_tex") |>
    dplyr::inner_join(
      whep::soil_hydraulic_by_texture,
      by = "usda_texture_class"
    ) |>
    dplyr::transmute(
      .data$mu_global,
      t_field = .data$field_capacity,
      t_wilt = .data$wilting_point,
      .data$porosity
    )
}

# ---- Pinned derived grids ---------------------------------------------------

# WHEP publishes the two derived HWSD grids so that nobody has to aggregate an
# 11 GB archive (about an hour per pass) and -- the reason that matters more --
# so that everyone reads ONE vintage. The archive itself stays a third-party
# download; what is pinned is the WHEP-built product of it.
.hwsd_texture_pin <- function() {
  "hwsd-texture-class-grid"
}

.hwsd_clay_pin <- function() {
  "hwsd-clay-grid"
}

# Read one of those pins, aborting in a way that names BOTH routes. Modelled on
# `.read_lpjml_pin()` (R/feed_lpjml.R) and `.read_cell_polity_pin()`
# (R/n_balance_spatialize.R); deliberately duplicated rather than generalised,
# so a change here cannot reach the LPJmL path.
.read_hwsd_pin <- function(alias, version = NULL) {
  tryCatch(
    whep_read_file(alias, version = version),
    error = function(e) {
      cli::cli_abort(
        c(
          "Could not read the pinned {.val {alias}} grid.",
          i = "Either fetch the pin (network access required), or derive the
               grid locally with {.code source = \"local\"} and
               {.envvar WHEP_HWSD_DIR} pointing at an HWSD archive
               ({.path inst/scripts/download/download_hwsd.R} fetches one).",
          x = conditionMessage(e)
        ),
        call = NULL
      )
    }
  )
}

# Restrict a global pinned grid to the caller's area of interest using the SAME
# padded bounding box the local path crops to (`.hwsd_target_extent()` pads by
# half a target cell), never by target-cell membership.
#
# That distinction is not cosmetic. `.gapfill_soil()` fills a cell that has no
# aggregated neighbour from a CONSTANT -- pH 7.0, or loam hydraulics -- so if
# the pin route handed back a smaller neighbour pool than the local route, the
# two would disagree only at the edges, only for gap-filled cells, and would
# look like plausible soil either way.
.hwsd_crop_to_target <- function(grid, target_grid, target_res = 0.5) {
  if (is.null(target_grid)) {
    return(grid)
  }
  # An NA coordinate aborts the LOCAL route at `terra::ext()`. Tolerating it
  # here (with na.rm) made the two routes disagree in the worst direction:
  # the pin route kept the NA rows, `.gapfill_soil()` found no neighbour for a
  # coordinate that does not exist, and stamped them with its loam CONSTANT --
  # plausible soil at a place that is not on the map. Refuse it on both routes.
  bad <- is.na(target_grid$lon) | is.na(target_grid$lat)
  if (any(bad)) {
    cli::cli_abort(c(
      "{sum(bad)} target cell{?s} ha{?s/ve} a missing coordinate.",
      i = "A cell with no {.field lon}/{.field lat} cannot be gap-filled from
           its neighbours and would take the fallback constant instead."
    ))
  }
  pad <- target_res / 2
  dplyr::filter(
    grid,
    .data$lon >= min(target_grid$lon) - pad,
    .data$lon <= max(target_grid$lon) + pad,
    .data$lat >= min(target_grid$lat) - pad,
    .data$lat <= max(target_grid$lat) + pad
  )
}

# Both routes are checked the same way, so a pin cannot carry something the
# local path would have refused.
.check_hwsd_grid <- function(grid, cols, source_label, target_res = 0.5) {
  .check_columns(grid, c("lon", "lat", cols), source_label)
  if (nrow(grid) == 0L) {
    cli::cli_abort(
      "{.val {source_label}} carries no cells."
    )
  }
  # A repeated cell multiplies through `.gapfill_soil_hydraulic()`, whose three
  # per-property results are recombined with an inner join: two duplicate rows
  # become eight, each mixing t_field, t_wilt and porosity taken from DIFFERENT
  # source cells. That produced t_wilt above t_field -- physically impossible
  # soil -- from a grid that passed every other check.
  dup <- duplicated(grid[c("lon", "lat")])
  if (any(dup)) {
    cli::cli_abort(c(
      "{.val {source_label}} repeats {sum(dup)} cell{?s}.",
      i = "Each (lon, lat) must appear once; duplicates recombine across
           properties and can yield t_wilt above t_field."
    ))
  }
  off <- abs((grid$lon - target_res / 2) %% target_res) > 1e-6 |
    abs((grid$lat - target_res / 2) %% target_res) > 1e-6
  if (any(off)) {
    cli::cli_abort(c(
      "{.val {source_label}} is not on {.val {target_res}}-degree centres.",
      i = "{sum(off)} cell{?s} off-grid, e.g. lon {.val {grid$lon[off][1]}},
           lat {.val {grid$lat[off][1]}}."
    ))
  }
  tibble::as_tibble(grid)
}

# One resolution point for both grids, so they cannot drift apart.
#
# Order: an explicitly supplied table wins; an explicit `hwsd_dir` or
# `source = "local"` derives locally; otherwise the pin. Pin-first is the
# deliberate choice and it differs from `.gn_can_read_run()`, which lets
# WHEP_LPJML_RUN_DIR outrank its pin. The reason: an LPJmL run is the user's
# OWN model output and no two runs are meant to agree, whereas the HWSD grids
# are a deterministic product of a third-party archive that exists in two
# incompatible versions (v1.2 topsoil 0-30 cm; HWSD2 D1 0-20 cm, whep#851).
# Reading one shared vintage is the point of publishing them, so a set
# WHEP_HWSD_DIR alone does not silently opt a user out -- it is said out loud
# instead, and `source = "local"` opts out explicitly.
.resolve_hwsd_grid <- function(
  alias,
  cols,
  derive,
  hwsd_dir = NULL,
  data_grid = NULL,
  source = c("auto", "pin", "local"),
  version = NULL,
  target_grid = NULL
) {
  source <- rlang::arg_match(source)
  if (!is.null(data_grid)) {
    return(.check_hwsd_grid(data_grid, cols, "supplied grid"))
  }
  local <- source == "local" || (source == "auto" && !is.null(hwsd_dir))
  if (source == "pin" && !is.null(hwsd_dir)) {
    cli::cli_abort(
      "{.arg hwsd_dir} derives locally; it cannot be combined with
       {.code source = \"pin\"}."
    )
  }
  if (local) {
    return(.check_hwsd_grid(
      derive(.resolve_hwsd_dir(hwsd_dir), target_grid),
      cols,
      "locally derived grid"
    ))
  }
  if (source == "auto" && nzchar(Sys.getenv("WHEP_HWSD_DIR"))) {
    cli::cli_inform(c(
      i = "Reading the pinned {.val {alias}} even though
           {.envvar WHEP_HWSD_DIR} is set, so every user shares one vintage.",
      i = "Pass {.code source = \"local\"} to derive it from your archive."
    ))
  }
  .read_hwsd_pin(alias, version) |>
    .hwsd_crop_to_target(target_grid) |>
    .check_hwsd_grid(cols, alias)
}

# The USDA texture classes, in one fixed order, so a counts grid's columns
# mean the same thing wherever it was produced.
.hwsd_texture_classes <- function() {
  sort(unique(whep::soil_hydraulic_by_texture$usda_texture_class))
}

# Per-cell pixel COUNTS in each USDA texture class.
#
# This, not the hydraulic values, is what belongs in a published artifact.
# `t_field`, `t_wilt` and `porosity` are not HWSD quantities: each map unit
# resolves to a dominant texture class and then to that class's CONSTANTS from
# `whep::soil_hydraulic_by_texture`. Publishing the values would freeze that
# coefficient table, and `whep::hwsd_texture_usda` with it, inside a data
# artifact -- so a later revision of either would move every local user while
# pinned users stayed frozen, silently. That is exactly what this package
# forbids for its LPJmL pins, which "hold ONLY LPJmL-derived quantities".
#
# Nothing is lost by carrying counts instead. A pixel's value depends on the
# pixel only through its class and the aggregation is a plain mean, so
#
#   mean = sum_k (n_k * v_k) / sum_k n_k
#
# reproduces the values exactly (measured: max absolute difference 4e-14 over a
# real window). See `.hydraulic_from_class_counts()`.
.aggregate_hwsd_classes <- function(
  hwsd_dir,
  mu_class,
  target_res = 0.5,
  target_grid = NULL
) {
  classes <- .hwsd_texture_classes()
  idx <- match(mu_class$usda_texture_class, classes)
  keep <- !is.na(idx)
  cols <- rlang::set_names(
    rep("class_index", length(classes)),
    paste0("n_", classes)
  )
  .aggregate_hwsd_multi(
    hwsd_dir,
    tibble::tibble(
      mu_global = mu_class$mu_global[keep],
      class_index = idx[keep]
    ),
    target_res = target_res,
    target_grid = target_grid,
    cols = cols,
    reduce = "counts"
  )
}

# Turn a per-cell class-counts grid into the three hydraulic columns, using
# whichever `soil_hydraulic_by_texture` the INSTALLED package carries -- which
# is the whole point of pinning counts rather than values.
#
# The 2-decimal quantisation is kept deliberately, because the previous route
# (aggregate the values, then round) applied it and this branch is not the
# place to revisit it. The counts are exact integers, so the precision is now
# free and dropping the rounding would be more faithful -- but it would move
# EVERY cell by up to 0.005, where keeping it moves at most ~0.05% of cells by
# one quantisation step. Measured globally over 65,794 cells, the number that
# can flip is 32 (t_field), 38 (t_wilt) and 25 (porosity): exactly those whose
# unrounded value lands on a .xx5 boundary, where the two routes' 1e-16
# difference in representation sends `round()` opposite ways. No
# reimplementation can make a tie fall the same side twice.
.hydraulic_from_class_counts <- function(
  counts,
  coef = whep::soil_hydraulic_by_texture
) {
  classes <- .hwsd_texture_classes()
  ncols <- paste0("n_", classes)
  .check_columns(counts, c("lon", "lat", ncols), "texture class counts")
  at <- match(classes, coef$usda_texture_class)
  m <- as.matrix(counts[, ncols])
  m[is.na(m)] <- 0
  total <- rowSums(m)
  if (any(total <= 0)) {
    cli::cli_abort(
      "{sum(total <= 0)} cell{?s} carry no classified HWSD pixel."
    )
  }
  tibble::tibble(
    lon = counts$lon,
    lat = counts$lat,
    t_field = round(as.vector(m %*% coef$field_capacity[at]) / total, 2),
    t_wilt = round(as.vector(m %*% coef$wilting_point[at]) / total, 2),
    porosity = round(as.vector(m %*% coef$porosity[at]) / total, 2)
  )
}

# RETAINED AS THE ORACLE, not as live code. `read_soil_hydraulic()` no longer
# calls this: it aggregates per-class pixel COUNTS and multiplies by the
# coefficient table, so the coefficients stay in code instead of being frozen
# into a published grid. This function is the previous route, kept because
# `test_soil_ph.R` checks the counts route reproduces it exactly. Deleting it
# would delete the only independent reference that check has.
#
# Aggregate the three per-map-unit hydraulic columns to the 0.5-degree grid,
# reusing .aggregate_hwsd() (crop -> classify -> mean-aggregate) once per
# column and joining the results on the cell key.
.aggregate_hwsd_hydraulic <- function(hwsd_dir, mu_hyd, target_grid) {
  cols <- c(t_field = "t_field", t_wilt = "t_wilt", porosity = "porosity")
  .aggregate_hwsd_multi(
    hwsd_dir,
    mu_hyd,
    target_res = 0.5,
    target_grid = target_grid,
    cols = cols
  )
}

# Gap-fill cells in the target grid missing from the aggregated hydraulic grid
# from the nearest available neighbour, one property at a time via the shared
# .gapfill_soil() (which fills a single value column), then rejoin. Each
# property's no-neighbour fallback is its central-texture (loam) reference from
# soil_hydraulic_by_texture, never the pH reader's neutral 7.0 (which is
# impossible for a volumetric fraction in (0, 1) and would corrupt the ICBM
# moisture modifier downstream).
.gapfill_soil_hydraulic <- function(grid, country_grid) {
  loam <- whep::soil_hydraulic_by_texture |>
    dplyr::filter(.data$usda_texture_class == "loam")
  fallbacks <- c(
    t_field = loam$field_capacity,
    t_wilt = loam$wilting_point,
    porosity = loam$porosity
  )
  filled <- purrr::imap(fallbacks, function(fallback, col) {
    single <- dplyr::rename(grid, soil_ph = dplyr::all_of(col))
    single <- dplyr::select(single, "lon", "lat", "soil_ph")
    .gapfill_soil(
      single,
      country_grid,
      fallback = fallback,
      label = "soil hydraulic"
    ) |>
      dplyr::rename("{col}" := "soil_ph")
  })
  purrr::reduce(filled, dplyr::inner_join, by = c("lon", "lat"))
}

# Reclassify the HWSD map-unit raster to a per-cell numeric attribute
# (`value_col` of `mu_soils`) and spatially aggregate to WHEP's 0.5-degree
# grid (mean of native cells per block), returning it as `out_col`. When
# `target_grid` (a `lon`/`lat` tibble) is supplied, the raster is first
# cropped to that grid's bounding box (padded half a target cell on each
# side) BEFORE the expensive terra::classify(), so a regional caller never
# materialises or reclassifies the full-resolution global HWSD raster (which
# otherwise exhausts memory and crashes the R session). Shared by the soil-pH
# and soil-hydraulic-property readers.
.aggregate_hwsd <- function(
  hwsd_dir,
  mu_soils,
  target_res,
  target_grid = NULL,
  value_col = "t_ph_h2o",
  out_col = "soil_ph"
) {
  .aggregate_hwsd_multi(
    hwsd_dir,
    mu_soils,
    target_res,
    target_grid,
    rlang::set_names(value_col, out_col)
  )
}

# Aggregate one or more map-unit attributes onto the target grid in a SINGLE
# banded pass over the raster. `cols` is a named character vector mapping each
# output column to its `mu_soils` attribute.
#
# Each band is cropped ONCE -- the crop is the part that touches the ~11 GB
# file on disk -- and every attribute is then classified and mean-aggregated
# from that one in-memory band. `.aggregate_hwsd_hydraulic()` previously called
# the single-column entry point once per property, so the whole raster was read
# three times to produce three reclassifications of the very same pixels, and
# `.cb_hwsd_clay()` read it a fourth time for clay. On a machine reading its
# soil texture locally that was the dominant cost of a gridded carbon balance:
# `.socd_soil_hydraulic()` alone ran over 35 minutes.
#
# The result is unchanged. Per-column NA dropping and the inner join between
# columns are both kept; joining within a band and binding bands afterwards is
# the same set as binding first and joining after, because each cell belongs to
# exactly one band and the bands are disjoint.
.aggregate_hwsd_multi <- function(
  hwsd_dir,
  mu_soils,
  target_res,
  target_grid = NULL,
  cols = c(soil_ph = "t_ph_h2o"),
  reduce = c("mean", "counts")
) {
  reduce <- rlang::arg_match(reduce)
  hwsd_path <- file.path(hwsd_dir, "hwsd.bil")
  if (!file.exists(hwsd_path)) {
    cli::cli_abort("HWSD raster not found at {.file {hwsd_path}}.")
  }
  # terra::rast() only opens the file; the pixels stay on disk until a band asks
  # for them. Cropping the whole grid up front would pull all ~11 GB into memory
  # before any aggregation happens, which is the cost this banding avoids.
  key <- .hwsd_cache_key(
    hwsd_path,
    target_res,
    cols,
    target_grid,
    mu_soils,
    reduce
  )
  cached <- .hwsd_cache_read(key)
  if (!is.null(cached)) {
    return(cached)
  }
  src <- terra::rast(hwsd_path)
  extent <- .hwsd_target_extent(src, target_grid, target_res)
  # For counts every output column shares ONE reclassification -- map unit to
  # class index -- so the band is classified once and then counted per class,
  # rather than classified once per column.
  rcls <- if (reduce == "counts") {
    list(as.matrix(mu_soils[, c("mu_global", "class_index")]))
  } else {
    purrr::map(cols, \(col) as.matrix(mu_soils[, c("mu_global", col)]))
  }
  out <- purrr::map(
    .hwsd_band_extents(extent, target_res),
    \(band) {
      .hwsd_band_values(
        src,
        band,
        rcls,
        target_res,
        reduce,
        names(cols)
      )
    }
  ) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()
  # An empty grid must never be cached, and never be returned. Downstream,
  # `.gapfill_soil()` fills a cell with no neighbour from a CONSTANT -- pH 7.0,
  # or loam hydraulics -- so a zero-row aggregation does not surface as a gap,
  # it surfaces as plausible soil covering the whole world. Caching would turn
  # that from a one-off into a permanent answer.
  if (nrow(out) == 0L) {
    cli::cli_abort(c(
      "Aggregating HWSD produced no cells.",
      i = "Check the raster at {.file {hwsd_path}} and the target grid."
    ))
  }
  .hwsd_cache_write(key, out)
  out
}

# Derived HWSD grids are cached on disk because they are a PURE FUNCTION of the
# archive and the target grid, and are otherwise recomputed from an ~11 GB
# raster on every build, in every session, for every year -- even though soil
# texture does not vary with any of them. Same directory the verified downloads
# already use.
.hwsd_cache_dir <- function() {
  # `WHEP_HWSD_CACHE_DIR` exists so the test suite can point this somewhere
  # disposable. Without it every test that aggregates a fixture HWSD would
  # write into the user's real cache, and -- worse than the litter -- a later
  # test could then read a cached grid instead of exercising the aggregation
  # it means to test.
  override <- Sys.getenv("WHEP_HWSD_CACHE_DIR")
  if (nzchar(override)) {
    return(override)
  }
  file.path(rappdirs::user_cache_dir("whep"), "hwsd")
}

# Everything the aggregation depends on goes into the key, so a cache hit can
# only happen when recomputing would give the same answer: the archive's
# identity (a replaced or re-downloaded HWSD changes size or mtime), the
# resolution, the attribute mapping, the target grid's own cells, and
# `mu_soils` itself -- which covers the DERIVED attribute values, so a change
# to `.derive_map_unit_hydraulic()` invalidates every grid it fed.
# Bumped whenever anything that CHANGES the aggregated numbers changes: the
# band height, the rounding, the aggregation function or its `na.rm`, the
# `others = NA` classify policy, or a terra upgrade that moves a value. The
# archive's own mtime cannot stand in for this -- an algorithm change leaves
# every user's archive untouched, so without a version here they would be
# served grids computed by the OLD code indefinitely, with the right schema and
# plausible values. Same shape as the #384 stale-`.rda` failure.
.hwsd_cache_algo_version <- function() {
  "2026-09-08.1"
}

.hwsd_cache_key <- function(
  hwsd_path,
  target_res,
  cols,
  target_grid,
  mu_soils,
  reduce = "mean"
) {
  # The `.hdr` sidecar carries the grid geometry terra reads (rows, columns,
  # corner coordinates, pixel type, byte order). A corrected or re-extracted
  # header with an untouched `.bil` changes every aggregated cell, so both
  # files are identified, not just the one holding the pixels.
  files <- c(hwsd_path, sub("\\.bil$", ".hdr", hwsd_path))
  info <- file.info(files)
  # An unstattable path yields NA size and mtime, which hash happily and make
  # every such directory share one key. The grid is always recomputable, so
  # refusing is free; serving another archive's grid is not.
  bad <- files[is.na(info$size) | is.na(info$mtime)]
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Cannot fingerprint the HWSD archive for caching.",
      i = "Could not stat {.file {bad}}."
    ))
  }
  rlang::hash(list(
    algo = .hwsd_cache_algo_version(),
    reduce = reduce,
    size = info$size,
    mtime = info$mtime,
    target_res = target_res,
    cols = cols,
    grid = if (is.null(target_grid)) {
      NULL
    } else {
      dplyr::arrange(
        dplyr::distinct(target_grid[c("lon", "lat")]),
        .data$lon,
        .data$lat
      )
    },
    mu_soils = mu_soils
  ))
}

# A corrupt or half-written cache file must never fail a build: the grid is
# always recomputable, so an unreadable entry is treated as a miss.
.hwsd_cache_read <- function(key) {
  path <- file.path(.hwsd_cache_dir(), paste0(key, ".parquet"))
  if (!file.exists(path)) {
    return(NULL)
  }
  out <- tryCatch(
    tibble::as_tibble(nanoparquet::read_parquet(path)),
    error = function(e) NULL
  )
  # A zero-row entry is treated as a MISS rather than a hit, for the same
  # reason the producer refuses to write one: an empty grid is indistinguishable
  # downstream from soil that happens to be constant everywhere.
  if (is.null(out) || nrow(out) == 0L) {
    return(NULL)
  }
  out
}

# Written via a temporary file and renamed, so a build interrupted mid-write
# cannot leave a truncated grid behind under a key that claims to be complete.
# A cache that cannot be written (read-only home, full disk) is not an error.
.hwsd_cache_write <- function(key, value) {
  dir <- .hwsd_cache_dir()
  path <- file.path(dir, paste0(key, ".parquet"))
  tryCatch(
    {
      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }
      tmp <- paste0(path, ".tmp-", Sys.getpid())
      nanoparquet::write_parquet(value, tmp)
      file.rename(tmp, path)
    },
    error = function(e) {
      cli::cli_warn(
        "Could not cache the derived HWSD grid: {conditionMessage(e)}"
      )
    }
  )
  invisible(value)
}

# The extent to aggregate over: the target grid's bounding box padded by half a
# target cell, or the whole raster when no target grid is given.
.hwsd_target_extent <- function(src, target_grid, target_res) {
  if (is.null(target_grid)) {
    return(terra::ext(src))
  }
  pad <- target_res / 2
  terra::ext(
    min(target_grid$lon) - pad,
    max(target_grid$lon) + pad,
    min(target_grid$lat) - pad,
    max(target_grid$lat) + pad
  )
}

# The whole-number factor that aggregates `src_res` up to `target_res`. Round,
# never truncate, and refuse a ratio that is not whole rather than silently
# accepting one.
#
# `as.integer()` truncates, and the ratio does not arrive exact. The EHdr
# sidecar stores the cell size to 15 significant digits, so a resolution that
# is an exact ratio in memory comes back perturbed: at one sixth of a degree
# the header holds 0.166666666666667, `terra::res()` reads back
# 0.16666666666666699, and 0.5 over that is 2.99999999999999423, which
# truncates to 2. The whole world is then aggregated over 2x2 blocks where 3x3
# is meant -- a complete, plausible grid at the wrong support, with no warning,
# no NA and no failed check. HWSD's own header rounds the other way
# (0.5 / 0.00833333333333332975 = 60.0000000000000284, truncating to the
# correct 60), so the aggregation is right today by luck, not by design, and a
# re-download whose header rounded up would flip it.
#
# The tolerance is relative and has fourteen orders of magnitude of clearance
# on both sides: the header round trip moves the ratio by ~1e-16 relative,
# while the smallest genuine mismatch -- a factor out by one source pixel --
# moves it by 1/factor, which is 1.7e-2 at HWSD's factor of 60.
.hwsd_agg_factor <- function(target_res, src_res) {
  ratio <- target_res / src_res
  whole <- round(ratio)
  residue <- ratio - whole
  if (whole < 1 || abs(residue) > .hwsd_agg_tolerance() * whole) {
    cli::cli_abort(c(
      "Target resolution is not a whole multiple of the source resolution.",
      "i" = "Target {.val {target_res}} over source {.val {src_res}} is
             {.val {ratio}}.",
      "i" = "Nearest whole factor {.val {whole}}, residue {.val {residue}}."
    ))
  }
  as.integer(whole)
}

# Relative tolerance on the aggregation ratio's distance from a whole number.
.hwsd_agg_tolerance <- function() {
  1e-6
}
# Split an extent into latitude bands, each a whole number of target rows tall.
# Whole target rows is what makes this safe: every aggregated cell's source
# pixels then lie inside exactly one band, so banding cannot change a single
# aggregated value.
.hwsd_band_extents <- function(extent, target_res) {
  n_rows <- as.integer(round((extent$ymax - extent$ymin) / target_res))
  starts <- seq(0L, max(n_rows - 1L, 0L), by = .hwsd_band_rows())
  purrr::map(starts, function(start) {
    rows <- min(.hwsd_band_rows(), n_rows - start)
    terra::ext(
      extent$xmin,
      extent$xmax,
      extent$ymax - (start + rows) * target_res,
      extent$ymax - start * target_res
    )
  })
}

# Target rows per band. 32 keeps a global 0.5-degree pass near 2 GB; the value
# only trades peak memory against the number of passes, never the result.
.hwsd_band_rows <- function() {
  32L
}

# Classify and mean-aggregate one latitude band for every requested attribute,
# releasing its full-resolution intermediates before the next band allocates
# its own. `rcls` is a named list of reclassification matrices; the band is
# cropped once and reused for all of them, which is the whole point -- the crop
# is what reads the raster off disk.
.hwsd_band_values <- function(
  src,
  band,
  rcls,
  target_res,
  reduce = "mean",
  out_cols = names(rcls)
) {
  sub <- terra::crop(src, band)
  agg_factor <- .hwsd_agg_factor(target_res, terra::res(sub)[1])
  if (reduce == "counts") {
    out <- .hwsd_band_counts(sub, rcls[[1]], out_cols, agg_factor)
    rm(sub)
    invisible(gc(full = TRUE))
    return(out)
  }
  # Collected per COLUMN, not once per band: `classified` is a
  # full-resolution raster of the band, so leaving three of them uncollected
  # would triple the peak the 32-row band height was chosen against. Banding
  # exists because an unbanded crop exhausted memory, so tripling the band's
  # peak is the wrong direction.
  parts <- purrr::imap(
    rcls,
    \(rcl, out_col) .hwsd_band_column(sub, rcl, out_col, agg_factor)
  )
  rm(sub)
  invisible(gc(full = TRUE))
  purrr::reduce(parts, dplyr::inner_join, by = c("lon", "lat"))
}

# Count, per aggregated cell, how many native HWSD pixels fall in each USDA
# texture class. The band is classified ONCE to a class index and then summed
# per class, so the expensive step is paid once rather than twelve times.
#
# `na.rm = TRUE` on the sum matches the mean path's treatment: a pixel whose
# map unit is absent from the crosswalk is classified NA and counted by no
# class, exactly as it contributed to no mean.
.hwsd_band_counts <- function(sub, rcl, out_cols, agg_factor) {
  classified <- terra::classify(sub, rcl, others = NA)
  parts <- purrr::imap(
    rlang::set_names(seq_along(out_cols), out_cols),
    \(k, nm) {
      coarse <- terra::aggregate(
        classified == k,
        fact = agg_factor,
        fun = "sum",
        na.rm = TRUE
      )
      values <- terra::as.data.frame(coarse, xy = TRUE, na.rm = FALSE)
      names(values) <- c("lon", "lat", nm)
      rm(coarse)
      invisible(gc(full = TRUE))
      dplyr::mutate(
        values,
        lon = round(.data$lon, 2),
        lat = round(.data$lat, 2)
      )
    }
  )
  rm(classified)
  invisible(gc(full = TRUE))
  out <- purrr::reduce(parts, dplyr::full_join, by = c("lon", "lat"))
  # A cell with no classified pixel at all carries no texture and would divide
  # by zero downstream, so it is dropped here rather than travelling as zeros.
  total <- rowSums(as.matrix(out[, out_cols]), na.rm = TRUE)
  out[total > 0, , drop = FALSE]
}

# One attribute of one already-cropped band, rounded exactly as the
# single-column reader rounded it so the join key and the values are
# unchanged.
.hwsd_band_column <- function(sub, rcl, out_col, agg_factor) {
  classified <- terra::classify(sub, rcl, others = NA)
  coarse <- terra::aggregate(
    classified,
    fact = agg_factor,
    fun = "mean",
    na.rm = TRUE
  )
  values <- terra::as.data.frame(coarse, xy = TRUE, na.rm = TRUE)
  names(values) <- c("lon", "lat", out_col)
  rm(classified, coarse)
  invisible(gc(full = TRUE))
  values |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      "{out_col}" := round(.data[[out_col]], 2)
    )
}

# Gap-fill cells present in the target grid but missing from the aggregated
# HWSD grid, from the nearest available neighbour (inverse-distance-squared
# weighted mean of the `soil_ph` value column), searching outward in
# 0.5-degree rings up to `max_search` rings. `fallback` is the domain-neutral
# value used when no neighbour exists within the search window (pH 7.0 for the
# pH reader; a central-texture reference for each hydraulic property), and
# `label` names the quantity in the progress message so the shared helper does
# not mislabel a hydraulic gap-fill as a pH one.
.gapfill_soil <- function(
  soil_grid,
  country_grid,
  max_search = 100L,
  fallback = 7.0,
  label = "soil pH"
) {
  missing <- country_grid |>
    dplyr::select("lon", "lat") |>
    # Border cells occur once per overlapping polity in the country grid, but
    # soil properties are cell-level. Fill each coordinate once or the three
    # hydraulic-property joins multiply duplicate rows cartesianly.
    dplyr::distinct() |>
    dplyr::anti_join(soil_grid, by = c("lon", "lat"))
  if (nrow(missing) == 0) {
    return(soil_grid)
  }
  cli::cli_alert_info("Gap-filling {nrow(missing)} {label} cells...")
  filled <- purrr::map2(
    missing$lon,
    missing$lat,
    .fill_soil_cell,
    soil_grid = soil_grid,
    max_search = max_search,
    fallback = fallback
  )
  dplyr::bind_rows(soil_grid, dplyr::bind_rows(filled))
}

# Fill one missing cell's value from its nearest available HWSD neighbours,
# searching outward in 0.5-degree rings; falls back to `fallback` when nothing
# is found within `max_search` rings.
.fill_soil_cell <- function(m_lon, m_lat, soil_grid, max_search, fallback) {
  for (radius in seq_len(max_search)) {
    neighbours <- soil_grid |>
      dplyr::filter(
        abs(.data$lon - m_lon) <= radius * 0.5,
        abs(.data$lat - m_lat) <= radius * 0.5
      )
    if (nrow(neighbours) > 0) {
      ph_val <- neighbours |>
        dplyr::mutate(
          dist = pmax(
            sqrt((.data$lon - m_lon)^2 + (.data$lat - m_lat)^2),
            0.01
          ),
          w = 1.0 / .data$dist^2
        ) |>
        dplyr::summarise(
          ph = stats::weighted.mean(.data$soil_ph, w = .data$w)
        ) |>
        dplyr::pull("ph")
      return(tibble::tibble(
        lon = m_lon,
        lat = m_lat,
        soil_ph = round(ph_val, 2)
      ))
    }
  }
  tibble::tibble(lon = m_lon, lat = m_lat, soil_ph = fallback)
}

# Toy fixture for a runnable example (one cell).
.example_soil_ph <- function() {
  tibble::tribble(
    ~lon, ~lat, ~soil_ph,
    -0.25, -0.25, 6.8
  )
}

# Toy fixture for a runnable example (one cell, a mid-range mineral soil).
.example_hwsd_topsoil_soc <- function() {
  tibble::tribble(
    ~lon,  ~lat,  ~soc_obs_mgc_ha, ~method_soc_obs,
    -0.25, -0.25, 41.6,            "measured"
  )
}
# Toy fixture for a runnable example (one cell, loam-class hydraulics).
.example_soil_hydraulic <- function() {
  tibble::tribble(
    ~lon, ~lat, ~t_field, ~t_wilt, ~porosity,
    -0.25, -0.25, 0.29, 0.14, 0.43
  )
}
