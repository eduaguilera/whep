# -----------------------------------------------------------------------
# build_pilot_polycell_support.R
#
# Build a publication-grade polycell support table for one subnational pilot
# -- by default Japan's 46 prefecture polities -- WITH the inland-water and ice
# layers, and report what those layers change against the layer-less build.
#
# Why this script exists. `build_polycell_support()` takes `water` and `ice`
# as OPTIONAL arguments and zero-fills them when they are absent, so a support
# built without them books every lake and every glacier as land while
# `polity_area_ha == land_area_ha + inland_water_ha + ice_area_ha` still holds
# perfectly. That is whep#885, and it is how the `polycell_support` pin version
# 20260827T190201Z-f82a2 came to carry 533 Mha of lakes and ice as land
# (whep#1010). An identity that holds by construction cannot detect a missing
# input, so this script asserts the layers were SUPPLIED -- non-zero rows, a
# recorded feature count, a recorded raster fingerprint -- and aborts rather
# than quietly producing a land-only table.
#
# What it reports:
#   L   the two input layers, with the provenance of each.
#   B   the layer-less baseline build, which is what a pilot gets today.
#   W   the build with water and ice, and the delta in all four area columns.
#   C   the per-cell and per-polity effect of the layers.
#   A   the supplied-input assertions, which are the point of the script.
#
# Run (from the WHEP repository root):
#   Rscript inst/scripts/build_pilot_polycell_support.R
#
# Inputs, all resolved from environment variables (never hardcode the path):
#   WHEP_LPJML_INPUT_DIR   the parent of GLWD/, as
#                          inst/scripts/download/download_hydrology.R lays it
#                          out. REQUIRED: inland water is not optional here.
#   WHEP_NATURALEARTH_DIR  ne_10m_glaciated_areas/, as
#                          inst/scripts/download/download_naturalearth.R lays
#                          it out. REQUIRED, for the same reason. Ice may
#                          legitimately measure zero over a pilot; "the layer
#                          was read and found nothing" and "no layer was read"
#                          are different states and only the first is allowed.
#   WHEP_PILOT_OUT_DIR     where the support parquet and its manifest are
#                          written. REQUIRED.
#   WHEP_PILOT_POLITY_CODES  optional comma-separated polity codes. Unset
#                          selects every live subnational polity of
#                          WHEP_PILOT_ISO3 (default JPN).
#   WHEP_PILOT_ISO3        optional ISO3 of the pilot country, default "JPN".
#
# Note for anyone whose environment variables look unset: R reads `.Renviron`
# in the working directory INSTEAD of `~/.Renviron`, never both (whep#456), so
# a `.Renviron` at the repository root would hide every one of these.
# -----------------------------------------------------------------------

.bps_h <- function(x) cli::cli_h2(x)

.bps_env <- function(name) {
  value <- Sys.getenv(name, "")
  if (nzchar(value)) value else NULL
}

.bps_require_env <- function(name, what) {
  value <- .bps_env(name)
  if (is.null(value)) {
    cli::cli_abort(c(
      "{.envvar {name}} is unset, so {what} cannot be read.",
      i = "This script refuses to build a support without it: a zero-filled
           layer is indistinguishable from a genuinely dry one (whep#885)."
    ))
  }
  value
}

# ---- The pilot subset -------------------------------------------------------

# Read the repository snapshot rather than an installed namespace, so the
# geometries measured are the ones this checkout ships.
.bps_polities <- function(path = file.path("data", "polities.rda")) {
  if (!file.exists(path)) {
    cli::cli_abort(
      "Cannot load the polity snapshot: {.file {path}} is absent. Run this
       script from the WHEP repository root."
    )
  }
  store <- new.env(parent = emptyenv())
  objects <- load(path, envir = store)
  if (!identical(objects, "polities") || !inherits(store$polities, "sf")) {
    cli::cli_abort(
      "{.file {path}} must contain exactly one {.cls sf} object named
       {.val polities}; found {.val {objects}}."
    )
  }
  store$polities
}

# The subset is resolved BEFORE the build and held against it afterwards.
# Deriving the expected membership from the returned support is circular: an
# empty build then expects zero and reports success.
.bps_pilot_codes <- function(polities) {
  requested <- .bps_env("WHEP_PILOT_POLITY_CODES")
  if (!is.null(requested)) {
    codes <- sort(stringr::str_trim(stringr::str_split_1(requested, ",")))
    attr(codes, "selection") <- "WHEP_PILOT_POLITY_CODES"
    return(codes)
  }
  iso3 <- .bps_env("WHEP_PILOT_ISO3") %||% "JPN"
  attributes <- sf::st_drop_geometry(polities)
  keep <- attributes$iso3_code %in%
    iso3 &
    attributes$polity_type %in% "subnational" &
    attributes$has_geometry
  if (!any(keep)) {
    cli::cli_abort(
      "No live subnational polity of {.val {iso3}} carries a geometry."
    )
  }
  codes <- sort(attributes$polity_code[keep])
  attr(codes, "selection") <- paste("all subnational polities of", iso3)
  codes
}

# The automatic selector takes EVERY subnational polity of the country, and
# that set is not always the set the pilot's statistics cover. Japan's is not:
# `whep::polities` carries 46 prefectures valid 1871-2025 plus RYU-1937-1945,
# a proxy-polygon Okinawa live for eight years that the 46-to-46
# `admin-stats-japan` alias map has no unit for. Including it also splits every
# interval it touches into 1871-1937 / 1937-1945 / 1945-2025. So the scope is
# printed rather than assumed, and a polity whose interval or polygon differs
# from the majority is called out for the caller to accept or to exclude with
# WHEP_PILOT_POLITY_CODES.
.bps_report_scope <- function(geometries) {
  scope <- sf::st_drop_geometry(geometries) |>
    dplyr::select(dplyr::all_of(c(
      "polity_code",
      "start_year",
      "end_year",
      "polygon_status",
      "wiki_status"
    )))
  span <- paste(scope$start_year, scope$end_year, sep = "-")
  majority <- names(sort(table(span), decreasing = TRUE))[[1L]]
  odd <- scope[span != majority | scope$polygon_status != "assigned", ]
  cli::cli_alert_info(
    "Validity spans: {.val {sort(unique(span))}};
     polygon status: {.val {sort(unique(scope$polygon_status))}}."
  )
  if (nrow(odd) == 0L) {
    cli::cli_alert_success("Every selected polity shares one span and polygon.")
    return(invisible(scope))
  }
  cli::cli_alert_warning(
    "{nrow(odd)} selected polit{?y/ies} do not match the majority span
     {.val {majority}} or carry a non-assigned polygon. Exclude them with
     {.envvar WHEP_PILOT_POLITY_CODES} if the pilot's statistics do not
     cover them."
  )
  print(as.data.frame(odd), row.names = FALSE)
  invisible(scope)
}

.bps_geometries <- function(polities, codes) {
  keep <- polities$polity_code %in% codes
  missing <- setdiff(codes, polities$polity_code[keep])
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Requested polities are not in the snapshot: {.val {missing}}."
    )
  }
  attributes <- sf::st_drop_geometry(polities)[keep, , drop = FALSE]
  sf::st_sf(attributes, geometry = sf::st_geometry(polities)[keep])
}

.bps_subset_gate <- function(codes, support) {
  got <- sort(unique(as.character(support$polity_code)))
  missing <- setdiff(codes, got)
  unexpected <- setdiff(got, codes)
  if (length(missing) > 0L || length(unexpected) > 0L) {
    cli::cli_abort(c(
      "The built support does not match the requested pilot subset.",
      "x" = "Missing: {.val {missing}}.",
      "x" = "Unexpected: {.val {unexpected}}."
    ))
  }
  cli::cli_alert_success(
    "Subset identity matches all {length(codes)} polities."
  )
}

# ---- Input layers -----------------------------------------------------------

# The GLWD derivation walks a 33,600 x 86,400 raster and takes about eight
# minutes, which makes an interactive rebuild of the pilot painful. It is cached
# on a fingerprint of the source rasters themselves -- name, size and mtime --
# so a re-downloaded or re-extracted GLWD invalidates the cache rather than
# being silently read from a stale copy.
.bps_glwd_fingerprint <- function(input_dir) {
  files <- list.files(
    file.path(input_dir, "GLWD"),
    pattern = "\\.tif$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0L) {
    cli::cli_abort(
      "No GLWD GeoTIFF under {.file {file.path(input_dir, 'GLWD')}}. Fetch it
       with {.file inst/scripts/download/download_hydrology.R}."
    )
  }
  info <- file.info(files)
  paste(
    basename(files),
    info$size,
    format(info$mtime, "%Y-%m-%dT%H:%M:%S"),
    sep = ":",
    collapse = " | "
  )
}

.bps_water_cache_path <- function(out_dir) {
  file.path(out_dir, "glwd_v2_water_frac_halfdeg.parquet")
}

.bps_read_water_cache <- function(cache, fingerprint) {
  stamp <- paste0(cache, ".fingerprint.txt")
  if (!file.exists(cache) || !file.exists(stamp)) {
    return(NULL)
  }
  if (!identical(readLines(stamp, warn = FALSE)[[1L]], fingerprint)) {
    cli::cli_alert_warning("GLWD cache is stale; re-deriving from the rasters.")
    return(NULL)
  }
  cli::cli_alert_info("GLWD: reusing the cache at {.file {cache}}.")
  tibble::as_tibble(nanoparquet::read_parquet(cache))
}

.bps_water <- function(input_dir, out_dir) {
  fingerprint <- .bps_glwd_fingerprint(input_dir)
  cache <- .bps_water_cache_path(out_dir)
  water <- .bps_read_water_cache(cache, fingerprint)
  if (is.null(water)) {
    cli::cli_alert("Deriving the GLWD v2 water fraction (about 8 minutes)...")
    water <- whep::read_glwd_water(input_dir)
    nanoparquet::write_parquet(water, cache)
    writeLines(fingerprint, paste0(cache, ".fingerprint.txt"))
  }
  .bps_report_water(water, fingerprint)
  water
}

.bps_report_water <- function(water, fingerprint) {
  cell_ha <- whep:::.cell_area_ha_lat(water$lat)
  cli::cli_alert_info(
    "GLWD v2: {nrow(water)} cells, {sum(water$water_frac > 0)} wet,
     {round(sum(water$water_frac * cell_ha) / 1e8, 4)} Mkm2 global inland
     water."
  )
  cli::cli_alert_info("GLWD source fingerprint: {fingerprint}")
}

.bps_ice <- function(ne_dir) {
  ice <- whep::read_glaciated_areas(ne_dir)
  cli::cli_alert_info(
    "ne_10m_glaciated_areas: {nrow(ice)} usable features,
     {sum(ice$s2_repaired)} repaired planar-side,
     {nrow(attr(ice, 'unrepaired'))} still s2-invalid."
  )
  ice
}

# ---- Builds -----------------------------------------------------------------

.bps_build <- function(geometries, water, ice, label) {
  cli::cli_alert("Building the {label} support...")
  started <- Sys.time()
  support <- whep::build_polycell_support(
    geometries = geometries,
    water = water,
    ice = ice
  )
  cli::cli_alert_success(
    "{label}: {nrow(support)} interval rows,
     {dplyr::n_distinct(support$polycell_id)} polycells,
     {dplyr::n_distinct(support$cell_id)} cells,
     {round(as.numeric(difftime(Sys.time(), started, units = 'secs')), 1)} s."
  )
  support
}

.bps_totals <- function(support) {
  tibble::tibble(
    polity_area_ha = sum(support$polity_area_ha),
    land_area_ha = sum(support$land_area_ha),
    inland_water_ha = sum(support$inland_water_ha),
    ice_area_ha = sum(support$ice_area_ha)
  )
}

.bps_report_totals <- function(baseline, layered) {
  .bps_h("W: what the layers change")
  before <- .bps_totals(baseline)
  after <- .bps_totals(layered)
  comparison <- tibble::tibble(
    quantity = names(before),
    layer_less_ha = as.numeric(before[1L, ]),
    with_layers_ha = as.numeric(after[1L, ])
  ) |>
    dplyr::mutate(
      delta_ha = .data$with_layers_ha - .data$layer_less_ha,
      delta_pct = dplyr::if_else(
        .data$layer_less_ha > 0,
        100 * .data$delta_ha / .data$layer_less_ha,
        NA_real_
      )
    )
  print(as.data.frame(comparison), digits = 10, row.names = FALSE)
  invisible(comparison)
}

# The identity is checked on BOTH builds. It holds on the layer-less one too,
# which is exactly why it cannot be the evidence that the layers arrived.
.bps_identity <- function(support, label) {
  residual <- support$land_area_ha +
    support$inland_water_ha +
    support$ice_area_ha -
    support$polity_area_ha
  cli::cli_text(
    "{label}: max relative residual
     {signif(max(abs(residual) / support$polity_area_ha), 3)};
     negative land rows {sum(support$land_area_ha < 0)};
     negative water rows {sum(support$inland_water_ha < 0)}."
  )
}

# ---- The supplied-input assertions ------------------------------------------

# `polity_area_ha == land + water + ice` is satisfied by zero. These are the
# checks a zero-filled layer FAILS.
.bps_assert_supplied <- function(support, water, ice) {
  .bps_h("A: the layers were supplied, not zero-filled")
  wet <- sum(support$inland_water_ha > 0)
  if (wet == 0L) {
    cli::cli_abort(c(
      "Not one polycell carries inland water.",
      i = "Either the water layer did not reach the build or the join missed
           every cell; both look identical in the area identity (whep#885)."
    ))
  }
  cli::cli_alert_success(
    "Water: {wet} of {nrow(support)} polycell intervals carry
     {round(sum(support$inland_water_ha) / 1e3, 1)} kha of inland water."
  )
  .bps_assert_ice_read(support, ice)
  .bps_assert_water_reaches(support, water)
}

# Ice may honestly be zero over a pilot. What may NOT be zero is the evidence
# that the layer was read: the feature count and the pilot's own bounding-box
# intersection are recorded so a future zero is attributable.
.bps_assert_ice_read <- function(support, ice) {
  if (nrow(ice) == 0L) {
    cli::cli_abort("The ice layer read back zero features.")
  }
  ice_ha <- sum(support$ice_area_ha)
  cli::cli_alert_success(
    "Ice: layer read with {nrow(ice)} features; the pilot intersects
     {round(ice_ha, 3)} ha of it on
     {sum(support$ice_area_ha > 0)} polycell intervals."
  )
  if (ice_ha == 0) {
    cli::cli_alert_info(
      "Zero ice here is a MEASURED zero, not an absent layer: the glaciated
       polygons were read and none of them reaches the pilot's polycells."
    )
  }
}

# A water layer that covers the wrong grid joins to nothing and books every
# lake as land in silence -- the `.glwd_snap()` defect. So the overlap between
# the layer's cells and the pilot's own cells is measured explicitly.
.bps_assert_water_reaches <- function(support, water) {
  cells <- dplyr::distinct(support, .data$lon, .data$lat)
  matched <- dplyr::semi_join(cells, water, by = c("lon", "lat"))
  if (nrow(matched) < nrow(cells)) {
    cli::cli_alert_warning(
      "{nrow(cells) - nrow(matched)} of {nrow(cells)} pilot cells are outside
       the water layer's coverage and are treated as dry."
    )
  }
  cli::cli_alert_success(
    "Water grid: {nrow(matched)} of {nrow(cells)} pilot cells matched a water
     row."
  )
}

# ---- Per-cell and per-polity effect -----------------------------------------

.bps_cell_effect <- function(baseline, layered) {
  .bps_h("C: the per-cell effect")
  joined <- .bps_join_builds(baseline, layered) |>
    dplyr::summarise(
      land_before_ha = sum(.data$land_before_ha),
      land_after_ha = sum(.data$land_after_ha),
      water_ha = sum(.data$inland_water_ha),
      ice_ha = sum(.data$ice_area_ha),
      .by = c("cell_id", "lon", "lat")
    ) |>
    dplyr::mutate(
      land_lost_pct = 100 *
        (.data$land_before_ha - .data$land_after_ha) /
        .data$land_before_ha
    )
  affected <- dplyr::filter(joined, .data$land_lost_pct > 0)
  cli::cli_text(
    "{nrow(affected)} of {nrow(joined)} cells lose land to the layers;
     median loss {round(stats::median(affected$land_lost_pct), 3)}%,
     max {round(max(affected$land_lost_pct), 3)}%."
  )
  print(
    utils::head(
      as.data.frame(dplyr::arrange(affected, dplyr::desc(.data$land_lost_pct))),
      10
    ),
    digits = 6,
    row.names = FALSE
  )
  invisible(joined)
}

.bps_join_builds <- function(baseline, layered) {
  key <- c("polycell_id", "start_year", "end_year")
  before <- dplyr::select(
    baseline,
    dplyr::all_of(key),
    land_before_ha = "land_area_ha"
  )
  after <- dplyr::select(
    layered,
    dplyr::all_of(c(key, "cell_id", "lon", "lat", "polity_code")),
    land_after_ha = "land_area_ha",
    dplyr::all_of(c("inland_water_ha", "ice_area_ha"))
  )
  joined <- dplyr::inner_join(after, before, by = key)
  if (nrow(joined) != nrow(after) || nrow(joined) != nrow(baseline)) {
    cli::cli_abort(
      "The two builds do not share a polycell key: {nrow(baseline)} baseline,
       {nrow(after)} layered, {nrow(joined)} joined."
    )
  }
  joined
}

.bps_polity_effect <- function(baseline, layered) {
  .bps_h("C: the per-polity effect")
  effect <- .bps_join_builds(baseline, layered) |>
    dplyr::summarise(
      land_before_ha = sum(.data$land_before_ha),
      land_after_ha = sum(.data$land_after_ha),
      water_ha = sum(.data$inland_water_ha),
      .by = "polity_code"
    ) |>
    dplyr::mutate(
      land_lost_pct = 100 *
        (.data$land_before_ha - .data$land_after_ha) /
        .data$land_before_ha
    ) |>
    dplyr::arrange(dplyr::desc(.data$water_ha))
  cli::cli_text(
    "{sum(effect$water_ha > 0)} of {nrow(effect)} polities carry inland water."
  )
  print(utils::head(as.data.frame(effect), 10), digits = 6, row.names = FALSE)
  invisible(effect)
}

# ---- Diagnostics the build rides as attributes ------------------------------

# Two of `build_polycell_support()`'s diagnostics say something about the pilot
# rather than about the layers, and both are silent warnings otherwise: cells
# claimed by more territory than they hold (adjacent polygons that overlap),
# and cells one of the two sides reaches and the other does not.
.bps_diagnostics <- function(support) {
  .bps_h("D: the build's own diagnostics")
  overlap <- attr(support, "overlap")
  if (is.null(overlap) || nrow(overlap) == 0L) {
    cli::cli_alert_success("No cell holds more territory than the cell.")
  } else {
    cli::cli_alert_warning(
      "{nrow(overlap)} cell-interval{?s} hold more territory than the cell:
       {round(sum(overlap$excess_ha), 1)} ha excess
       ({signif(100 * sum(overlap$excess_ha) / sum(support$polity_area_ha), 3)}%
       of the pilot), worst {round(max(overlap$excess_ha), 1)} ha,
       up to {max(overlap$polities)} polities in one cell. Adjacent polygons
       overlap; this is a property of the polity table, not of the layers."
    )
  }
  .bps_water_sides(support)
}

# `water_cell_without_polycell` is expected in bulk here -- the water layer is
# global and the support is one country -- so the two sides are counted apart.
# Only the other side would mean water the pilot cannot see.
.bps_water_sides <- function(support) {
  unmatched <- attr(support, "water_unmatched")
  if (is.null(unmatched) || nrow(unmatched) == 0L) {
    cli::cli_alert_success("Water layer and polycells share every cell.")
    return(invisible(NULL))
  }
  counts <- table(unmatched$side)
  missed <- unname(counts["polycell_cell_without_water"])
  cli::cli_alert_info(
    "Water/polycell cell mismatch: {sum(counts)} rows,
     {dplyr::coalesce(missed, 0L)} of them polycell cells the water layer does
     not cover; the rest are the global layer's cells outside the pilot."
  )
  invisible(counts)
}

# ---- Output -----------------------------------------------------------------

# The diagnostics ride as attributes, which parquet cannot carry, so the table
# goes out twice: a parquet `read_polycell_support()` can be pointed at, and an
# RDS that keeps the attributes for anyone auditing the build.
.bps_write <- function(support, out_dir, codes, water_fingerprint, ice) {
  parquet <- file.path(out_dir, "pilot_polycell_support.parquet")
  rds <- file.path(out_dir, "pilot_polycell_support.rds")
  nanoparquet::write_parquet(support, parquet)
  saveRDS(support, rds)
  manifest <- .bps_manifest(support, codes, water_fingerprint, ice)
  utils::write.csv(
    manifest,
    file.path(out_dir, "pilot_polycell_support_manifest.csv"),
    row.names = FALSE
  )
  cli::cli_alert_success(
    "Wrote {.file {parquet}} ({round(file.size(parquet) / 1024)} kB),
     {.file {rds}} and the manifest beside them."
  )
  cli::cli_alert_info(
    "Point {.envvar WHEP_POLYCELL_SUPPORT_PATH} at the parquet to run the
     pilot against it."
  )
  invisible(parquet)
}

.bps_manifest <- function(support, codes, water_fingerprint, ice) {
  tibble::tribble(
    ~field, ~value,
    "built_at", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    "polities", paste(codes, collapse = " "),
    "n_polities", as.character(length(codes)),
    "selection", attr(codes, "selection") %||% "unrecorded",
    "n_interval_rows", as.character(nrow(support)),
    "n_polycells", as.character(dplyr::n_distinct(support$polycell_id)),
    "n_cells", as.character(dplyr::n_distinct(support$cell_id)),
    "polity_area_ha", format(sum(support$polity_area_ha), digits = 15),
    "land_area_ha", format(sum(support$land_area_ha), digits = 15),
    "inland_water_ha", format(sum(support$inland_water_ha), digits = 15),
    "ice_area_ha", format(sum(support$ice_area_ha), digits = 15),
    "water_source", "GLWD v2 combined classes, classes 1-3 and 7",
    "water_fingerprint", water_fingerprint,
    "ice_source", "ne_10m_glaciated_areas",
    "ice_features", as.character(nrow(ice)),
    "polities_rda_blob", .bps_polities_blob(),
    "runtime", .bps_runtime()
  )
}

.bps_polities_blob <- function(path = file.path("data", "polities.rda")) {
  git <- Sys.which("git")
  if (!nzchar(git)) {
    return(NA_character_)
  }
  blob <- suppressWarnings(system2(
    git,
    c("hash-object", path),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (length(blob) != 1L || !grepl("^[0-9a-f]{40}$", blob)) {
    return(NA_character_)
  }
  blob[[1L]]
}

.bps_runtime <- function() {
  paste0(
    "R ",
    R.version$major,
    ".",
    R.version$minor,
    "; sf ",
    utils::packageVersion("sf"),
    "; s2 ",
    utils::packageVersion("s2"),
    "; terra ",
    utils::packageVersion("terra"),
    "; GEOS ",
    sf::sf_extSoftVersion()[["GEOS"]]
  )
}

# ---- Run --------------------------------------------------------------------

.bps_main <- function() {
  rlang::check_installed(c("sf", "terra", "nanoparquet"))
  input_dir <- .bps_require_env("WHEP_LPJML_INPUT_DIR", "the GLWD water layer")
  ne_dir <- .bps_require_env("WHEP_NATURALEARTH_DIR", "the glaciated-area ice")
  out_dir <- .bps_require_env("WHEP_PILOT_OUT_DIR", "the pilot output")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  .bps_h("L: the pilot subset and its input layers")
  polities <- .bps_polities()
  codes <- .bps_pilot_codes(polities)
  cli::cli_alert_info(
    "Pilot subset: {length(codes)} polit{?y/ies}
     ({attr(codes, 'selection')})."
  )
  geometries <- .bps_geometries(polities, codes)
  .bps_report_scope(geometries)
  water <- .bps_water(input_dir, out_dir)
  ice <- .bps_ice(ne_dir)

  .bps_h("B: the layer-less baseline")
  baseline <- .bps_build(geometries, NULL, NULL, "layer-less")
  layered <- .bps_build(geometries, water, ice, "water + ice")
  .bps_subset_gate(codes, layered)
  .bps_identity(baseline, "layer-less")
  .bps_identity(layered, "water + ice")

  .bps_report_totals(baseline, layered)
  .bps_assert_supplied(layered, water, ice)
  .bps_cell_effect(baseline, layered)
  .bps_polity_effect(baseline, layered)
  .bps_diagnostics(layered)
  .bps_write(layered, out_dir, codes, .bps_glwd_fingerprint(input_dir), ice)
  cli::cli_alert_success("Done.")
  invisible(layered)
}

.bps_main()
