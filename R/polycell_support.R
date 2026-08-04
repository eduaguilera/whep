#' Build the polycell spatial support table
#'
#' @description
#' Produce the canonical spatial support unit for WHEP: the **polycell**, the
#' intersection of a 0.5-degree grid cell with a polity over that polity's
#' validity interval. Each row carries the polity's territory in the cell,
#' decomposed into three separately addressable categories
#' (`land_area_ha + inland_water_ha + ice_area_ha = polity_area_ha`), so that
#' aggregating polycells to a polity changes no absolute value and no quantity
#' crosses a border it does not belong to.
#'
#' Areas are geodesic, computed with `sf::st_area()` on unprojected WGS84
#' longitude/latitude with spherical geometry (`s2`) enabled, matching WHEP's
#' own spherical cell-area convention. The territory is the union of live real
#' polities; land that falls in no live polity is emitted in the `"unassigned"`
#' attribute rather than renormalised away.
#'
#' @param years Integer vector of calendar years to resolve.
#' @param geometries An `sf` table of polity geometries with at least
#'   `polity_code`, `start_year` and `end_year` columns; defaults to
#'   [get_polity_geometries()]. `start_year` is inclusive and `end_year` is
#'   exclusive. Optional `wiki_status`, `polity_type` and `polygon_status`
#'   columns are honoured. Provide this argument to inject a fixture or an
#'   alternative geometry source.
#' @param water Optional per-cell `tibble` of inland water with columns `lon`,
#'   `lat` and `water_frac`, where `water_frac` is a fraction of the whole
#'   0.5-degree cell (GLWD convention). The water area is
#'   `water_frac * cell_area_ha`, apportioned across the cell's polycells pro
#'   rata by `polity_area_ha`.
#' @param ice Optional `sf`/`sfc` polygon layer of glaciated area (e.g.
#'   `ne_10m_glaciated_areas`); subtracted per polycell by exact geodesic
#'   intersection.
#' @param data A named list of optional auxiliary layers. `data$luh2` is the
#'   DA-5 validation layer: a `tibble` with `lon`, `lat` and `terrestrial_ha`
#'   used only to reconcile the intersected land and emit the unassigned
#'   diagnostic. It never masks land. When it is omitted the reconciliation is
#'   not run and no `"unassigned"` attribute is attached.
#' @param example If `TRUE`, return a small hard-coded example table instead of
#'   running the geometry engine, so the documented example runs without `sf`
#'   or remote data.
#'
#' @return A `tibble`, one row per polycell-year, whose columns are a superset
#'   of `polycell_id`, `cell_id`, `lon`, `lat`, `polity_code`, `area_code`,
#'   `year`, `cell_area_ha`, `polity_area_ha`, `land_area_ha`,
#'   `inland_water_ha`, `ice_area_ha`, `geometry_source`, `polygon_status`,
#'   `split_method` and `coverage_status`. When `data$luh2` is supplied, the
#'   `"unassigned"` attribute (`attr(result, "unassigned")`) is a `tibble` of
#'   land present in the LUH2 validation layer but claimed by no live polity,
#'   carrying `unassigned_land_ha`.
#' @export
#'
#' @examples
#' build_polycell_support(example = TRUE)
build_polycell_support <- function(
  years,
  geometries = NULL,
  water = NULL,
  ice = NULL,
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.ex_build_polycell_support())
  }
  rlang::check_installed("sf")
  old_s2 <- sf::sf_use_s2()
  withr::defer(suppressMessages(sf::sf_use_s2(old_s2)))
  suppressMessages(sf::sf_use_s2(TRUE))

  if (is.null(geometries)) {
    geometries <- get_polity_geometries()
  }
  polities <- .pcs_prepare_polities(geometries)
  ice_union <- .pcs_prepare_ice(ice)

  parts <- .pcs_all_polycells(as.integer(years), polities)
  if (length(parts) == 0L) {
    return(.pcs_attach_unassigned(.pcs_empty_output(), data$luh2))
  }

  do.call(rbind, parts) |>
    .pcs_add_ice(ice_union) |>
    .pcs_add_water(water) |>
    .pcs_finalize() |>
    .pcs_attach_unassigned(data$luh2)
}

# The unassigned diagnostic is the DA-5 / S-A9 reconciliation against the LUH2
# validation layer, so it is emitted only when `data$luh2` is supplied. Keeping
# it off the result otherwise also stops dplyr, which copies user attributes
# onto every derived frame, from carrying an empty diagnostic through unrelated
# `arrange()`/`select()` calls in consumers.
.pcs_attach_unassigned <- function(out, luh2) {
  if (is.null(luh2)) {
    return(out)
  }
  attr(out, "unassigned") <- .pcs_unassigned(out, luh2)
  out
}

# Output grain of plan A1, one row per polycell-year.
.pcs_output_cols <- function() {
  c(
    "polycell_id",
    "cell_id",
    "lon",
    "lat",
    "polity_code",
    "area_code",
    "year",
    "cell_area_ha",
    "polity_area_ha",
    "land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "geometry_source",
    "polygon_status",
    "split_method",
    "coverage_status"
  )
}

# Normalise the geometry source: keep the columns the producer reads, coerce
# geometry to WGS84, and drop dead and aggregate rows NA-explicitly (DA-7, EA7).
.pcs_prepare_polities <- function(geometries) {
  if (!inherits(geometries, "sf")) {
    cli::cli_abort("{.arg geometries} must be an {.cls sf} table.")
  }
  needed <- c("polity_code", "start_year", "end_year")
  missing <- setdiff(needed, names(geometries))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg geometries} is missing column{?s}: {.field {missing}}."
    )
  }
  attrs <- sf::st_drop_geometry(geometries)
  out <- sf::st_sf(
    polity_code = as.character(attrs$polity_code),
    start_year = as.integer(attrs$start_year),
    end_year = as.integer(attrs$end_year),
    wiki_status = .pcs_col(attrs, "wiki_status", NA_character_),
    polity_type = .pcs_col(attrs, "polity_type", NA_character_),
    polygon_status = .pcs_col(attrs, "polygon_status", NA_character_),
    area_code = .pcs_col(attrs, "area_code", NA_integer_),
    geometry = .pcs_geom_4326(sf::st_geometry(geometries))
  )
  out[.pcs_live_rows(out), ]
}

# EA7's live fencepost: `%in%` is FALSE for NA, so `!(x %in% dead)` KEEPS NA
# rows, unlike `dplyr::filter(x != dead)`, which silently drops them. Empty
# geometries cannot host area and are dropped too.
.pcs_live_rows <- function(polities) {
  !(polities$wiki_status %in% c("retired", "superseded")) &
    !(polities$polity_type %in% "aggregate") &
    !sf::st_is_empty(polities)
}

.pcs_col <- function(df, nm, default) {
  if (rlang::has_name(df, nm)) df[[nm]] else rep(default, nrow(df))
}

.pcs_geom_4326 <- function(geom) {
  crs <- sf::st_crs(geom)
  if (is.na(crs)) {
    return(sf::st_set_crs(geom, 4326))
  }
  if (crs == sf::st_crs(4326)) {
    return(geom)
  }
  sf::st_transform(geom, 4326)
}

# All polycell sf parts across every requested year, one part per polity-year.
.pcs_all_polycells <- function(years, polities) {
  years |>
    purrr::map(\(yr) {
      valid <- .pcs_valid_polities(polities, yr)
      purrr::map(
        seq_len(nrow(valid)),
        \(i) .pcs_polity_polycells(valid[i, ], yr)
      )
    }) |>
    purrr::list_flatten() |>
    purrr::compact()
}

# Year resolution reads the COLUMNS, never the code (DA-2): `start_year`
# inclusive, `end_year` exclusive. `which()` drops any NA-bounded row.
.pcs_valid_polities <- function(polities, yr) {
  polities[
    which(polities$start_year <= yr & yr < polities$end_year),
  ]
}

# Intersect one polity's polygon with the local 0.5-degree grid geodesically.
.pcs_polity_polycells <- function(polity_row, yr) {
  geom <- sf::st_geometry(polity_row)
  if (sf::st_is_empty(geom)[[1L]]) {
    return(NULL)
  }
  cells <- .pcs_cells_sf(.pcs_candidate_cells(sf::st_bbox(geom)))
  if (is.null(cells)) {
    return(NULL)
  }
  sf::st_agr(cells) <- "constant"
  inter <- .pcs_positive_area(sf::st_intersection(cells, geom))
  if (nrow(inter) == 0L) {
    return(NULL)
  }
  inter$cell_id <- (inter$klon + 720L) * 1000L + (inter$klat + 360L)
  inter$year <- yr
  inter$polity_code <- polity_row$polity_code
  inter$area_code <- polity_row$area_code
  inter$polygon_status <- polity_row$polygon_status
  inter[, c(
    "cell_id",
    "lon",
    "lat",
    "cell_area_ha",
    "polity_area_ha",
    "polity_code",
    "area_code",
    "polygon_status",
    "year"
  )]
}

# Geodesic area of each clipped piece; a piece touching a cell edge intersects
# in a zero-area line, which is dropped.
.pcs_positive_area <- function(inter) {
  inter$polity_area_ha <- as.numeric(sf::st_area(inter)) / 1e4
  inter[inter$polity_area_ha > 1e-9, ]
}

# Candidate cell indices covering a bounding box. Cells that only touch the box
# on an edge are enumerated and later dropped by the zero-area filter.
.pcs_candidate_cells <- function(bb) {
  tidyr::expand_grid(
    klon = .pcs_k(bb[["xmin"]]):.pcs_k(bb[["xmax"]]),
    klat = .pcs_k(bb[["ymin"]]):.pcs_k(bb[["ymax"]])
  )
}

# Integer cell index; the cell centre is `k * 0.5 + 0.25`, which sits on the
# canonical `(coord + 180) %% 0.5 == 0.25` half-degree convention.
.pcs_k <- function(x) {
  as.integer(floor(x / 0.5))
}

.pcs_cells_sf <- function(idx) {
  if (nrow(idx) == 0L) {
    return(NULL)
  }
  lon <- idx$klon * 0.5 + 0.25
  lat <- idx$klat * 0.5 + 0.25
  cells <- sf::st_sf(
    klon = idx$klon,
    klat = idx$klat,
    lon = lon,
    lat = lat,
    geometry = sf::st_sfc(purrr::map2(lon, lat, .pcs_cell_poly), crs = 4326)
  )
  cells$cell_area_ha <- as.numeric(sf::st_area(cells)) / 1e4
  cells
}

.pcs_cell_poly <- function(lon, lat) {
  sf::st_polygon(list(cbind(
    c(lon - 0.25, lon + 0.25, lon + 0.25, lon - 0.25, lon - 0.25),
    c(lat - 0.25, lat - 0.25, lat + 0.25, lat + 0.25, lat - 0.25)
  )))
}

# Prepare the DA-6 ice layer: repair s2-invalid features and union them.
.pcs_prepare_ice <- function(ice) {
  if (is.null(ice)) {
    return(NULL)
  }
  geom <- .pcs_geom_4326(sf::st_geometry(ice))
  # EA9: some `ne_10m_glaciated_areas` features are s2-invalid, and neither GEOS
  # nor s2 `st_make_valid()` alone repairs every one, so s2 `st_area()` /
  # `st_intersection()` abort with "Loop 0 is not valid". Repairing under the
  # planar (GEOS) engine splits the self-intersections into s2-valid pieces;
  # any feature that is still s2-invalid afterwards is dropped with a warning so
  # one bad loop cannot crash the whole producer. The injected fixture ice is
  # clean, so no test in the suite exercises this branch.
  geom <- .pcs_geos_make_valid(geom)
  ok <- sf::st_is_valid(geom)
  ok[is.na(ok)] <- FALSE
  if (any(!ok)) {
    cli::cli_warn(
      "Dropping {sum(!ok)} s2-invalid ice feature{?s} that could not be
       repaired (EA9)."
    )
    geom <- geom[ok]
  }
  if (length(geom) == 0L) {
    return(NULL)
  }
  sf::st_union(geom)
}

.pcs_geos_make_valid <- function(geom) {
  old <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(old)))
  suppressMessages(sf::sf_use_s2(FALSE))
  sf::st_make_valid(geom)
}

# Subtract ice per polycell by exact geodesic intersection, then drop geometry.
.pcs_add_ice <- function(polycells_sf, ice_union) {
  ice_area_ha <- rep(0, nrow(polycells_sf))
  if (!is.null(ice_union)) {
    polycells_sf$.pc_row <- seq_len(nrow(polycells_sf))
    sub <- polycells_sf[".pc_row"]
    sf::st_agr(sub) <- "constant"
    hit <- sf::st_intersection(sub, ice_union)
    if (nrow(hit) > 0L) {
      hit_ha <- as.numeric(sf::st_area(hit)) / 1e4
      agg <- tapply(hit_ha, hit$.pc_row, sum)
      ice_area_ha[as.integer(names(agg))] <- as.numeric(agg)
    }
    polycells_sf$.pc_row <- NULL
  }
  out <- tibble::as_tibble(sf::st_drop_geometry(polycells_sf))
  out$ice_area_ha <- ice_area_ha
  out
}

# Apportion GLWD per-cell water across the cell's polycells pro rata by
# `polity_area_ha` (DA-6). `water_frac` multiplies the whole cell area.
.pcs_add_water <- function(polycells, water) {
  if (is.null(water) || nrow(water) == 0L) {
    polycells$inland_water_ha <- 0
    return(polycells)
  }
  .pcs_check_water(water)
  cell_water <- polycells |>
    dplyr::distinct(lon, lat, cell_area_ha) |>
    dplyr::left_join(
      dplyr::distinct(dplyr::select(water, lon, lat, water_frac)),
      by = c("lon", "lat")
    ) |>
    dplyr::mutate(
      water_area_cell_ha = dplyr::coalesce(water_frac, 0) * cell_area_ha
    ) |>
    dplyr::select(lon, lat, water_area_cell_ha)
  polycells |>
    dplyr::left_join(cell_water, by = c("lon", "lat")) |>
    dplyr::mutate(
      inland_water_ha = water_area_cell_ha *
        polity_area_ha /
        sum(polity_area_ha),
      .by = c("year", "cell_id")
    ) |>
    dplyr::select(-water_area_cell_ha)
}

.pcs_check_water <- function(water) {
  missing <- setdiff(c("lon", "lat", "water_frac"), names(water))
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg water} is missing column{?s}: {.field {missing}}.")
  }
}

# Compose the three area categories, the polycell id and the reporting columns.
.pcs_finalize <- function(polycells) {
  polycells |>
    dplyr::mutate(
      polycell_id = paste0(polity_code, "@", cell_id),
      land_area_ha = polity_area_ha - inland_water_ha - ice_area_ha,
      geometry_source = "polity_geometries",
      split_method = "polygon_intersection"
    ) |>
    dplyr::mutate(
      coverage_status = dplyr::if_else(
        sum(polity_area_ha) >= cell_area_ha * (1 - 1e-6),
        "complete",
        "partial"
      ),
      .by = c("year", "cell_id")
    ) |>
    dplyr::select(dplyr::all_of(.pcs_output_cols()))
}

# DA-5 / S-A11 reconciliation: land present in LUH2 but claimed by no live
# polity is emitted, never renormalised into the polities.
.pcs_unassigned <- function(polycells, luh2) {
  if (is.null(luh2) || nrow(luh2) == 0L) {
    return(.pcs_empty_unassigned())
  }
  missing <- setdiff(c("lon", "lat", "terrestrial_ha"), names(luh2))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.code data$luh2} is missing column{?s}: {.field {missing}}."
    )
  }
  claimed <- polycells |>
    dplyr::summarise(
      claimed_land_ha = sum(land_area_ha),
      .by = c("year", "lon", "lat")
    )
  dplyr::full_join(
    claimed,
    .pcs_luh2_by_year(luh2, sort(unique(polycells$year))),
    by = c("year", "lon", "lat")
  ) |>
    dplyr::mutate(
      claimed_land_ha = dplyr::coalesce(claimed_land_ha, 0),
      terrestrial_ha = dplyr::coalesce(terrestrial_ha, 0),
      unassigned_land_ha = pmax(terrestrial_ha - claimed_land_ha, 0)
    ) |>
    dplyr::filter(unassigned_land_ha > 1e-9) |>
    tibble::as_tibble()
}

# Broadcast a year-agnostic LUH2 layer over the resolved years, or keep its own
# year column when present.
.pcs_luh2_by_year <- function(luh2, years) {
  if (rlang::has_name(luh2, "year")) {
    return(dplyr::select(luh2, year, lon, lat, terrestrial_ha))
  }
  tidyr::expand_grid(
    year = years,
    dplyr::distinct(dplyr::select(luh2, lon, lat, terrestrial_ha))
  )
}

.pcs_empty_output <- function() {
  tibble::tibble(
    polycell_id = character(),
    cell_id = integer(),
    lon = double(),
    lat = double(),
    polity_code = character(),
    area_code = integer(),
    year = integer(),
    cell_area_ha = double(),
    polity_area_ha = double(),
    land_area_ha = double(),
    inland_water_ha = double(),
    ice_area_ha = double(),
    geometry_source = character(),
    polygon_status = character(),
    split_method = character(),
    coverage_status = character()
  )
}

.pcs_empty_unassigned <- function() {
  tibble::tibble(
    year = integer(),
    lon = double(),
    lat = double(),
    claimed_land_ha = double(),
    terrestrial_ha = double(),
    unassigned_land_ha = double()
  )
}
