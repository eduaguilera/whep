#' Build the polycell spatial support table
#'
#' @description
#' Produce WHEP's canonical spatial support unit, the **polycell**: a
#' 0.5-degree grid cell intersected with a polity over that polity's validity
#' interval. Each row carries the polity's territory in the cell decomposed into
#' three separately addressable categories,
#' `polity_area_ha = land_area_ha + inland_water_ha + ice_area_ha`, so that
#' aggregating polycells to a polity changes no absolute value and no quantity
#' crosses a border it does not belong to.
#'
#' Areas are geodesic, from `sf::st_area()` on unprojected WGS84 with spherical
#' (`s2`) geometry, matching WHEP's own spherical convention; `cell_area_ha`
#' keeps the package formula so it stays bit-identical to
#' [build_cell_polity()]. Territory is the union of live real polities; land
#' claimed by no live polity is emitted in the `"unassigned"` attribute rather
#' than renormalised away.
#'
#' The default grain is **interval-keyed**: one row per polycell per interval
#' over which every area is constant, carrying `start_year` and `end_year`
#' (`end_year` exclusive). Supply `years` to expand to one row per
#' polycell-year, which is what [expand_polycell_years()] does on demand. No
#' area varies within an interval, so the interval grain is the form to store.
#'
#' @param years Optional integer vector of calendar years. `NULL` (default)
#'   returns the interval-keyed grain; a vector expands to one row per
#'   polycell-year and adds a `year` column.
#' @param geometries An `sf` table of polity geometries with at least
#'   `polity_code`, `start_year` and `end_year`; defaults to
#'   [get_polity_geometries()]. `start_year` is inclusive, `end_year`
#'   exclusive, and neither is ever parsed out of `polity_code`. Optional
#'   `wiki_status`, `polity_type`, `polygon_status` and `area_code` columns are
#'   honoured.
#' @param water Optional per-cell `tibble` of inland water with `lon`, `lat`
#'   and `water_frac`, a fraction of the **whole** cell, as
#'   [read_glwd_water()] returns it.
#' @param ice Optional `sf` polygon layer of glaciated area, as
#'   [read_glaciated_areas()] returns it, subtracted per polycell by exact
#'   geodesic intersection.
#' @param data Optional named list of auxiliary layers: `luh2` the validation
#'   layer (`lon`, `lat`, `terrestrial_ha`, e.g. [read_luh2_terrestrial()]);
#'   `crosswalk` the deployed [build_cell_polity()] table carrying the
#'   transitional `polity_frac`; `producer_crosswalk` a freshly built
#'   `build_cell_polity_fraction()` table; and `crosswalk_year`, the year whose
#'   polycells the crosswalk's present-day geometry describes (default 2015).
#' @param example If `TRUE`, return a small hard-coded table instead of running
#'   the geometry engine, so the documented example needs no `sf` and no
#'   external data.
#'
#' @return A `tibble` whose columns are a superset of `polycell_id`, `cell_id`,
#'   `lon`, `lat`, `polity_code`, `area_code`, `start_year`, `end_year`,
#'   `cell_area_ha`, `polity_area_ha`, `land_area_ha`, `inland_water_ha`,
#'   `ice_area_ha`, `geometry_source`, `polygon_status`, `split_method`,
#'   `coverage_status`, `luh2_vintage` and the transitional `polity_frac`, plus
#'   `year` when `years` is supplied. Diagnostics ride as attributes:
#'   `"unassigned"` (land in the validation layer claimed by no live polity),
#'   `"coverage"` (every live polity interval and whether it had a polygon),
#'   `"overlap"` (cells holding more territory than the cell, because two
#'   polities were handed the same polygon), `"water_excess"` (inland water
#'   clamped to the polycell's territory), `"footprints"` and
#'   `"footprint_diff"` (the deployed crosswalk, the current producer and the
#'   polycell footprint, reconciled at `data$crosswalk_year`).
#'
#'   `"overlap"`, `"water_excess"` and `"unassigned"` are **interval-grain**,
#'   like the table itself: they carry `start_year`, and one cell contributes a
#'   row per interval. Summing them without first filtering to the interval
#'   covering the year of interest counts the same cell once per epoch. On the
#'   shipped polities that is the difference between 1,342 clamped polycells
#'   over all epochs and 94 in 2015.
#' @export
#'
#' @examples
#' build_polycell_support(example = TRUE)
build_polycell_support <- function(
  years = NULL,
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

  geometries <- geometries %||% get_polity_geometries()
  polities <- .pcs_prepare_polities(geometries)
  support <- polities |>
    .pcs_intersect_grid() |>
    .pcs_add_ice(.pcs_prepare_ice(ice)) |>
    .pcs_split_intervals() |>
    .pcs_add_water(water) |>
    .pcs_finalize(.pcs_geometry_source(geometries), data)

  support |>
    .pcs_attach_diagnostics(polities, data, water) |>
    .pcs_expand(years)
}

#' Expand the interval-keyed polycell support to one row per year
#'
#' @description
#' Repeats every polycell interval over the calendar years it covers, adding a
#' `year` column. `start_year` is inclusive and `end_year` exclusive, so a year
#' resolves to exactly one interval per polycell and a boundary year is never
#' counted twice.
#'
#' @param support A [build_polycell_support()] table in the interval grain.
#' @param years Integer vector of calendar years.
#'
#' @return A `tibble` with one row per polycell-year, `year` placed after
#'   `area_code`.
#' @export
#'
#' @examples
#' expand_polycell_years(build_polycell_support(example = TRUE), 2015L)
expand_polycell_years <- function(support, years) {
  .pcs_require_cols(support, c("start_year", "end_year"), "support")
  years <- as.integer(years)
  years |>
    purrr::map(\(yr) {
      support |>
        dplyr::filter(.data$start_year <= yr, yr < .data$end_year) |>
        dplyr::mutate(year = yr, .after = "area_code")
    }) |>
    dplyr::bind_rows()
}

#' View the polycell support as today's cell-polity crosswalk
#'
#' @description
#' Returns the transitional shim: the `lon`, `lat`, `area_code`, `polity_frac`
#' and `cell_area_ha` columns a consumer of [build_cell_polity()] reads today.
#' Under the interim geometry this reproduces that crosswalk bit-for-bit, so
#' consumers migrate to the polycell columns one at a time instead of all on
#' the commit that reshapes the shared object. The shim is a transition device
#' with a scheduled removal, not a permanent contract.
#'
#' @param support A [build_polycell_support()] table built with
#'   `data$crosswalk` supplied.
#'
#' @return A `tibble` with `lon`, `lat`, `area_code`, `polity_frac` and
#'   `cell_area_ha`.
#' @export
#'
#' @examples
#' polycell_shim_view(build_polycell_support(example = TRUE))
polycell_shim_view <- function(support) {
  .pcs_require_cols(
    support,
    c("lon", "lat", "area_code", "polity_frac", "cell_area_ha"),
    "support"
  )
  out <- support |>
    dplyr::filter(!is.na(.data$polity_frac)) |>
    dplyr::distinct(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$polity_frac,
      .data$cell_area_ha
    )
  # The shim is a drop-in for `build_cell_polity()`, so it must not carry the
  # producer's diagnostics: dplyr copies user attributes onto every derived
  # frame, and a consumer comparing its result would see them.
  .pcs_strip_diagnostics(out)
}

# -- Geometry source ----------------------------------------------------------

.pcs_geometry_source <- function(geometries) {
  attr(geometries, "geometry_source") %||% "whep::polities"
}

# Normalise the geometry source: keep the columns the producer reads, coerce to
# WGS84 and drop dead and aggregate rows NA-explicitly. `%in%` is FALSE for NA,
# so `!(x %in% dead)` KEEPS an NA row, unlike `dplyr::filter(x != dead)`, which
# silently drops it. Exclusion needs positive evidence.
.pcs_prepare_polities <- function(geometries) {
  if (!inherits(geometries, "sf")) {
    cli::cli_abort("{.arg geometries} must be an {.cls sf} table.")
  }
  .pcs_require_cols(
    geometries,
    c("polity_code", "start_year", "end_year"),
    "geometries"
  )
  attrs <- sf::st_drop_geometry(geometries)
  usable <- .pcs_usable_geometry(sf::st_geometry(geometries))
  out <- sf::st_sf(
    polity_code = as.character(attrs$polity_code),
    start_year = as.integer(attrs$start_year),
    end_year = as.integer(attrs$end_year),
    polygon_status = .pcs_col(attrs, "polygon_status", NA_character_),
    area_code = .pcs_area_code(attrs),
    coverage_status = usable$coverage_status,
    geometry = usable$geom
  )
  live <- !(.pcs_col(attrs, "wiki_status", NA_character_) %in%
    c("retired", "superseded")) &
    !(.pcs_col(attrs, "polity_type", NA_character_) %in% "aggregate")
  out[live, ]
}

# How usable each polity polygon is, recorded on every polycell it produces and
# in the "coverage" diagnostic, so a missing or unusable geometry is never a
# silent zero area. `no_geometry` and `s2_invalid` polities produce no polycell
# at all; the shipped table holds 23 of the first and 7 of the second, and a
# further set are repaired planar-side before the spherical engine sees them.
.pcs_usable_geometry <- function(geom) {
  empty <- sf::st_is_empty(geom)
  fixed <- .s2_repair(.pcs_geom_4326(geom))
  status <- dplyr::case_when(
    empty ~ "no_geometry",
    fixed$status == "repaired" ~ "s2_repaired",
    fixed$status == "invalid" ~ "s2_invalid",
    .default = "has_geometry"
  )
  list(geom = fixed$geom, coverage_status = status)
}

# `area_code` is a label, resolved from the periodized crosswalk rather than
# invented. It stays NA where the crosswalk has no entry for the polity.
.pcs_area_code <- function(attrs) {
  if (rlang::has_name(attrs, "area_code")) {
    return(as.integer(attrs$area_code))
  }
  lookup <- whep::polity_area_crosswalk |>
    dplyr::distinct(.data$polity_code, .data$polity_area_code) |>
    dplyr::filter(!is.na(.data$polity_area_code)) |>
    dplyr::distinct(.data$polity_code, .keep_all = TRUE)
  as.integer(
    lookup$polity_area_code[match(attrs$polity_code, lookup$polity_code)]
  )
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

# -- The intersection ---------------------------------------------------------

# One geodesic intersection per polity interval, not per polity-year: no area
# depends on the year inside an interval, so a per-year loop would repeat
# identical work and emit identical rows.
.pcs_intersect_grid <- function(polities) {
  rows <- which(
    polities$coverage_status %in% c("has_geometry", "s2_repaired")
  )
  .pcs_warn_unusable(polities, rows)
  parts <- rows |>
    purrr::map(\(i) .pcs_polity_cells(polities[i, ])) |>
    purrr::compact()
  if (length(parts) == 0L) {
    return(NULL)
  }
  do.call(rbind, parts)
}

# A polity that carries no usable polygon receives no polycell at all, which is
# the one failure mode that would otherwise look exactly like a polity with no
# territory. It is named here and listed in the "coverage" diagnostic.
.pcs_warn_unusable <- function(polities, rows) {
  # `x[-integer(0)]` returns nothing rather than everything, so the complement
  # is taken explicitly: otherwise a run where EVERY polity is unusable would
  # be the one run that warns about none of them.
  dropped <- polities$polity_code[setdiff(seq_len(nrow(polities)), rows)]
  if (length(dropped) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{length(dropped)} live polit{?y/ies} carr{?ies/y} no usable polygon and
     receive{?s/} no polycell.",
    i = "See the {.val coverage} attribute; codes: {.val {dropped}}."
  ))
}

.pcs_polity_cells <- function(polity_row) {
  geom <- sf::st_geometry(polity_row)
  cells <- .pcs_cells_sf(.pcs_candidate_cells(sf::st_bbox(geom)))
  cells <- cells[lengths(sf::st_intersects(cells, geom)) > 0L, ]
  if (nrow(cells) == 0L) {
    return(NULL)
  }
  sf::st_agr(cells) <- "constant"
  inter <- .pcs_valid_pieces(sf::st_intersection(cells, geom))
  inter$polity_area_ha <- as.numeric(sf::st_area(inter)) / 1e4
  inter <- inter[inter$polity_area_ha > .pcs_area_floor_ha(), ]
  if (nrow(inter) == 0L) {
    return(NULL)
  }
  .pcs_label_cells(inter, sf::st_drop_geometry(polity_row))
}

.pcs_label_cells <- function(inter, attrs) {
  inter$polity_code <- attrs$polity_code
  inter$start_year <- attrs$start_year
  inter$end_year <- attrs$end_year
  inter$area_code <- attrs$area_code
  inter$polygon_status <- attrs$polygon_status
  inter$coverage_status <- attrs$coverage_status
  inter
}

# The spherical engine can emit a clipped piece carrying a duplicate vertex and
# then refuse to read its own output back: it happens on 8 of Russia's 12,730
# pieces, every one on the antimeridian, and `sf::st_area()` aborts with
# "Loop 0 is not valid: Edge 1 is degenerate". Repairing planar-side removes the
# zero-length edge; measured over the affected pieces the area is unchanged
# (714,451.732 ha either way), because a zero-length edge carries none.
.pcs_valid_pieces <- function(inter) {
  fixed <- .s2_repair(sf::st_geometry(inter))
  sf::st_geometry(inter) <- fixed$geom
  inter[fixed$status != "invalid", ]
}

# Two polygons that only touch intersect in a line, whose area is zero. The
# floor is 0.01 m2, far below any real polycell and far above float noise.
.pcs_area_floor_ha <- function() {
  1e-6
}

# Candidate cells covering a bounding box. Cells that merely touch it are
# enumerated here and removed by the area floor above.
.pcs_candidate_cells <- function(bb) {
  tidyr::expand_grid(
    klon = .pcs_k(bb[["xmin"]], 720L):.pcs_k(bb[["xmax"]], 720L),
    klat = .pcs_k(bb[["ymin"]], 360L):.pcs_k(bb[["ymax"]], 360L)
  )
}

# Integer cell index; the cell centre is `k * 0.5 + 0.25`, which sits on the
# canonical `(coord + 180) %% 0.5 == 0.25` half-degree convention. The index is
# clamped so a bounding box touching a pole or the antimeridian cannot ask for
# a cell off the grid.
.pcs_k <- function(x, n) {
  min(max(as.integer(floor(x / 0.5)), -n / 2L), n / 2L - 1L)
}

# The cell rectangles are NOT densified along parallels. Under s2 a lon/lat
# rectangle's east-west edge is a great circle, so a narrow polity whose north
# or south edge lies ON a cell boundary is clipped by a slightly different
# curve and loses 5.2e-5 (0.025 degrees wide) to 2.7e-4 (0.25 degrees wide) of
# its area at latitude 45. Densifying along parallels removes that, but it
# costs more than it buys: measured here, a polity polygon spanning the full
# cell -- which is what a border running along a parallel such as the 49th
# gives, and what a polity containing whole cells gives -- reproduces its own
# area to 8e-15 against a raw cell and loses 3.6e-4 against a densified one,
# because only its north edge is then clipped. Raw edges are also the same
# convention the polity polygons themselves are stored in, so the engine
# matches the data. The residual bias is bounded, one-sided and confined to
# polygons narrower than a cell whose edge coincides with a grid line; the
# whole-cell area it is measured against agrees with the parallel-bounded
# formula LUH2's `carea` uses to 1.2e-7, so it does not shift the DA-5
# reconciliation.
.pcs_cells_sf <- function(idx) {
  lon <- idx$klon * 0.5 + 0.25
  lat <- idx$klat * 0.5 + 0.25
  sf::st_sf(
    cell_id = .pcs_cell_id(idx$klon, idx$klat),
    lon = lon,
    lat = lat,
    cell_area_ha = .cell_area_ha_lat(lat),
    geometry = sf::st_sfc(purrr::map2(lon, lat, .pcs_cell_poly), crs = 4326)
  )
}

.pcs_cell_id <- function(klon, klat) {
  (klon + 360L) * 1000L + (klat + 180L)
}

.pcs_cell_poly <- function(lon, lat) {
  sf::st_polygon(list(cbind(
    c(lon - 0.25, lon + 0.25, lon + 0.25, lon - 0.25, lon - 0.25),
    c(lat - 0.25, lat - 0.25, lat + 0.25, lat + 0.25, lat - 0.25)
  )))
}

# -- Ice ----------------------------------------------------------------------

# The layer is unioned so overlapping ice features cannot be counted twice. The
# union is repaired before use for the same reason the clipped pieces are: s2
# emits a duplicate vertex in its own output and then refuses to read it back.
.pcs_prepare_ice <- function(ice) {
  if (is.null(ice)) {
    return(NULL)
  }
  geom <- .pcs_geom_4326(sf::st_geometry(ice))
  if (length(geom) == 0L) {
    return(NULL)
  }
  fixed <- .s2_repair(sf::st_union(geom))
  if (any(fixed$status == "invalid")) {
    cli::cli_abort("The {.arg ice} layer does not union into a usable polygon.")
  }
  fixed$geom
}

# Subtract ice per polycell by exact geodesic intersection, then drop geometry:
# ice depends on the polygon pair alone, never on the year, so it is resolved
# before the intervals are split.
.pcs_add_ice <- function(polycells_sf, ice_union) {
  if (is.null(polycells_sf)) {
    return(.pcs_empty_pieces())
  }
  ice_area_ha <- rep(0, nrow(polycells_sf))
  if (!is.null(ice_union)) {
    ice_area_ha <- .pcs_ice_areas(polycells_sf, ice_union)
  }
  out <- tibble::as_tibble(sf::st_drop_geometry(polycells_sf))
  out$ice_area_ha <- ice_area_ha
  out
}

.pcs_ice_areas <- function(polycells_sf, ice_union) {
  out <- rep(0, nrow(polycells_sf))
  sub <- polycells_sf["cell_id"]
  sub$piece <- seq_len(nrow(sub))
  # The indexed predicate is orders of magnitude cheaper than the clip, and ice
  # touches a small minority of polycells, so only those are clipped.
  sub <- sub[lengths(sf::st_intersects(sub, ice_union)) > 0L, ]
  if (nrow(sub) == 0L) {
    return(out)
  }
  sf::st_agr(sub) <- "constant"
  hit <- .pcs_valid_pieces(sf::st_intersection(sub, ice_union))
  if (nrow(hit) == 0L) {
    return(out)
  }
  areas <- tapply(as.numeric(sf::st_area(hit)) / 1e4, hit$piece, sum)
  out[as.integer(names(areas))] <- as.numeric(areas)
  out
}

# -- Interval algebra ---------------------------------------------------------

# The set of polities sharing a cell changes over time, and the inland-water
# apportionment depends on that set, so an interval is only constant-area if it
# is split wherever a co-occupant appears or leaves. Splitting on the cell's own
# breakpoints makes every emitted interval atomic, which is what lets the grain
# be interval-keyed instead of per-year.
.pcs_split_intervals <- function(pieces) {
  if (nrow(pieces) == 0L) {
    return(pieces)
  }
  keys <- c("cell_id", "polity_code", "start_year", "end_year")
  pieces |>
    dplyr::inner_join(
      .pcs_breakpoints(pieces),
      by = "cell_id",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$breakpoint >= .data$start_year,
      .data$breakpoint < .data$end_year
    ) |>
    dplyr::arrange(.data$cell_id, .data$polity_code, .data$breakpoint) |>
    dplyr::mutate(
      next_break = dplyr::lead(.data$breakpoint),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::mutate(
      start_year = .data$breakpoint,
      end_year = dplyr::coalesce(.data$next_break, .data$end_year)
    ) |>
    dplyr::select(-"breakpoint", -"next_break")
}

.pcs_breakpoints <- function(pieces) {
  pieces |>
    dplyr::distinct(.data$cell_id, .data$start_year, .data$end_year) |>
    tidyr::pivot_longer(
      c("start_year", "end_year"),
      values_to = "breakpoint"
    ) |>
    dplyr::distinct(.data$cell_id, .data$breakpoint)
}

# -- Inland water -------------------------------------------------------------

# The layer gives water as a fraction of the WHOLE cell, and it is INLAND
# water: lakes and rivers lie on land by definition, never on ocean, so all of
# a cell's water belongs to its territory and is apportioned across that cell's
# polycells pro rata. Summing the polycells therefore recovers
# `water_frac * cell_area_ha` exactly. Where the water layer's own land mask
# disagrees with the polity polygons and the apportioned water would exceed a
# polycell's territory, it is capped and the excess is emitted, so
# `land_area_ha` can never go negative and the disagreement stays visible.
.pcs_add_water <- function(pieces, water) {
  if (is.null(water) || nrow(water) == 0L || nrow(pieces) == 0L) {
    pieces$inland_water_ha <- rep(0, nrow(pieces))
    pieces$water_excess_ha <- rep(0, nrow(pieces))
    return(pieces)
  }
  .pcs_require_cols(water, c("lon", "lat", "water_frac"), "water")
  pieces |>
    dplyr::left_join(
      dplyr::distinct(water, .data$lon, .data$lat, .data$water_frac),
      by = c("lon", "lat")
    ) |>
    dplyr::mutate(
      water_pro_rata_ha = dplyr::coalesce(.data$water_frac, 0) *
        .data$cell_area_ha *
        .data$polity_area_ha /
        sum(.data$polity_area_ha),
      .by = c("cell_id", "start_year")
    ) |>
    dplyr::mutate(
      inland_water_ha = pmin(
        .data$water_pro_rata_ha,
        .data$polity_area_ha - .data$ice_area_ha
      ),
      water_excess_ha = .data$water_pro_rata_ha - .data$inland_water_ha
    ) |>
    dplyr::select(-"water_frac", -"water_pro_rata_ha")
}

# -- Assembly -----------------------------------------------------------------

.pcs_finalize <- function(pieces, geometry_source, data) {
  pieces |>
    dplyr::mutate(
      polycell_id = paste0(.data$polity_code, "@", .data$cell_id),
      # The DA-19 clamp makes `polity - water - ice >= 0` exactly, but where it
      # binds the subtraction re-derives a value that is already zero and
      # float64 returns it as -1e-13 ha on 8 of 564,304 real polycells. The
      # floor removes that representation artefact only: it can never mask a
      # real negative, because the clamp has already bounded the terms.
      land_area_ha = pmax(
        .data$polity_area_ha - .data$inland_water_ha - .data$ice_area_ha,
        0
      ),
      geometry_source = geometry_source,
      luh2_vintage = .pcs_luh2_vintage(data$luh2)
    ) |>
    .pcs_add_split_method() |>
    .pcs_add_shim(data) |>
    dplyr::select(
      dplyr::all_of(.pcs_output_cols()),
      dplyr::everything()
    )
}

.pcs_add_split_method <- function(pieces) {
  pieces |>
    dplyr::mutate(
      split_method = dplyr::if_else(
        dplyr::n() > 1L & sum(.data$inland_water_ha) > 0,
        "polygon_intersection+water_pro_rata",
        "polygon_intersection"
      ),
      .by = c("cell_id", "start_year")
    )
}

.pcs_luh2_vintage <- function(luh2) {
  as.character(attr(luh2, "luh2_vintage") %||% NA_character_)
}

.pcs_output_cols <- function() {
  c(
    "polycell_id",
    "cell_id",
    "lon",
    "lat",
    "polity_code",
    "area_code",
    "start_year",
    "end_year",
    "cell_area_ha",
    "polity_area_ha",
    "land_area_ha",
    "inland_water_ha",
    "ice_area_ha",
    "geometry_source",
    "polygon_status",
    "split_method",
    "coverage_status",
    "luh2_vintage",
    "polity_frac"
  )
}

# -- The transitional shim ----------------------------------------------------

# The crosswalk is a present-day product with no epochs, so its `polity_frac`
# is attached only to the intervals covering the year it describes. Everything
# else carries NA, and `polycell_shim_view()` drops those rows, which is what
# makes the shim reproduce the crosswalk exactly instead of approximately.
.pcs_add_shim <- function(pieces, data) {
  crosswalk <- data$crosswalk
  if (is.null(crosswalk)) {
    pieces$polity_frac <- rep(NA_real_, nrow(pieces))
    return(pieces)
  }
  .pcs_require_cols(
    crosswalk,
    c("lon", "lat", "area_code", "polity_frac"),
    "data$crosswalk"
  )
  yr <- as.integer(data$crosswalk_year %||% 2015L)
  pieces |>
    dplyr::left_join(
      dplyr::distinct(
        crosswalk,
        .data$lon,
        .data$lat,
        .data$area_code,
        .data$polity_frac
      ),
      by = c("lon", "lat", "area_code")
    ) |>
    dplyr::mutate(
      polity_frac = dplyr::if_else(
        .data$start_year <= yr & yr < .data$end_year,
        .data$polity_frac,
        NA_real_
      )
    ) |>
    .pcs_append_crosswalk_only(crosswalk, yr)
}

# Crosswalk rows the intersection did not reproduce are appended so the shim
# view stays row-complete: an unmigrated consumer must see exactly the rows it
# sees today, and a missing row is a silent change of its result.
.pcs_append_crosswalk_only <- function(pieces, crosswalk, yr) {
  extra <- crosswalk |>
    dplyr::distinct(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$polity_frac
    ) |>
    dplyr::anti_join(
      dplyr::filter(pieces, !is.na(.data$polity_frac)),
      by = c("lon", "lat", "area_code")
    )
  if (nrow(extra) == 0L) {
    return(pieces)
  }
  dplyr::bind_rows(
    pieces,
    dplyr::mutate(
      extra,
      cell_area_ha = .cell_area_ha_lat(.data$lat),
      start_year = yr,
      end_year = yr + 1L,
      coverage_status = "crosswalk_only",
      split_method = "crosswalk_only"
    )
  )
}

# -- Diagnostics --------------------------------------------------------------

# Each diagnostic is attached only when its input is present or it has
# something to report. dplyr copies user attributes onto every derived frame,
# so an unconditional attribute would ride through a consumer's unrelated
# `arrange()` or `select()` and turn up in comparisons that have nothing to do
# with it.
.pcs_attach_diagnostics <- function(support, polities, data, water) {
  coverage <- .pcs_coverage(polities)
  if (any(coverage$coverage_status != "has_geometry")) {
    attr(support, "coverage") <- coverage
  }
  overlap <- .pcs_overlap(support)
  if (nrow(overlap) > 0L) {
    attr(support, "overlap") <- overlap
    .pcs_warn_overlap(overlap)
  }
  if (!is.null(water)) {
    attr(support, "water_excess") <- .pcs_water_excess(support)
  }
  if (!is.null(data$crosswalk) || !is.null(data$producer_crosswalk)) {
    attr(support, "footprints") <- .pcs_footprints(support, data)
    attr(support, "footprint_diff") <- .pcs_footprint_diff(support, data)
  }
  if (!is.null(data$luh2)) {
    attr(support, "unassigned") <- .pcs_unassigned(support, data$luh2)
  }
  support
}

# Two live polities can be handed the SAME polygon by the geometry source, and
# then their polycells both claim the whole of a cell: on the shipped table
# GNQ-1968-2025 and STP-1800-2025 each take all of cell (10.25, 1.75) in 2015,
# and colonial-era IDN/IND/PAK share one 1800 polygon. That is a defect in the
# polygons, not in the intersection, and deciding who owns the ground is a
# territorial judgement this producer must not make. It is emitted instead, so
# the double count is visible where it lands rather than buried in a total.
.pcs_overlap <- function(support) {
  support |>
    dplyr::filter(.data$coverage_status != "crosswalk_only") |>
    dplyr::summarise(
      territory_ha = sum(.data$polity_area_ha),
      polities = dplyr::n(),
      .by = c("cell_id", "lon", "lat", "start_year", "cell_area_ha")
    ) |>
    dplyr::filter(
      .data$territory_ha > .data$cell_area_ha * (1 + .pcs_cell_tolerance())
    ) |>
    dplyr::mutate(excess_ha = .data$territory_ha - .data$cell_area_ha)
}

.pcs_warn_overlap <- function(overlap) {
  cli::cli_warn(c(
    "{nrow(overlap)} cell-interval{?s} hold{?s/} more territory than the cell.",
    i = "Overlapping polity polygons; see the {.val overlap} attribute.
         Excess: {round(sum(overlap$excess_ha) / 1e6, 2)} Mha."
  ))
}

# The whole-cell tolerance: s2 and the package's parallel-bounded cell formula
# agree to <= 9.5e-6 relative over latitudes 0-85, so 1e-4 accepts either
# spherical convention while still rejecting a genuine overlap.
.pcs_cell_tolerance <- function() {
  1e-4
}

.pcs_coverage <- function(polities) {
  polities |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::select(
      "polity_code",
      "start_year",
      "end_year",
      "polygon_status",
      "coverage_status"
    )
}

.pcs_water_excess <- function(support) {
  cols <- c(
    "polycell_id",
    "cell_id",
    "polity_code",
    "start_year",
    "polity_area_ha",
    "water_excess_ha"
  )
  if (!rlang::has_name(support, "water_excess_ha")) {
    support$water_excess_ha <- rep(0, nrow(support))
  }
  support |>
    dplyr::filter(.data$water_excess_ha > .pcs_area_floor_ha()) |>
    dplyr::select(dplyr::all_of(cols))
}

# Both footprints, side by side: the deployed crosswalk every published WHEP
# number was computed from, the crosswalk today's producer would rebuild, and
# the polycell intersection. Picking one silently would make the migration's
# movement and the restriction's movement inseparable.
.pcs_footprints <- function(support, data) {
  sources <- list(
    deployed_crosswalk = data$crosswalk,
    producer_crosswalk = data$producer_crosswalk,
    polycell = .pcs_polycell_footprint(support, data)
  )
  sources |>
    purrr::compact() |>
    purrr::imap(\(x, nm) .pcs_footprint_row(x, nm)) |>
    dplyr::bind_rows()
}

.pcs_footprint_row <- function(x, nm) {
  tibble::tibble(
    footprint = nm,
    rows = nrow(x),
    cells = nrow(dplyr::distinct(x, .data$lon, .data$lat)),
    area_codes = dplyr::n_distinct(x$area_code)
  )
}

# Both crosswalks are present-day products with no epochs, so the polycell
# footprint is taken at the same year. Comparing every historical interval
# against them would count a cell once per epoch and make the reconciliation
# meaningless: on the shipped table that is 129,047 rows against 68,527.
.pcs_polycell_footprint <- function(support, data) {
  yr <- as.integer(data$crosswalk_year %||% 2015L)
  support |>
    dplyr::filter(
      .data$coverage_status != "crosswalk_only",
      .data$start_year <= yr,
      yr < .data$end_year
    ) |>
    dplyr::distinct(.data$lon, .data$lat, .data$area_code)
}

.pcs_footprint_diff <- function(support, data) {
  members <- list(
    deployed_crosswalk = data$crosswalk,
    producer_crosswalk = data$producer_crosswalk,
    polycell = .pcs_polycell_footprint(support, data)
  ) |>
    purrr::compact() |>
    purrr::imap(\(x, nm) {
      x |>
        dplyr::distinct(.data$lon, .data$lat, .data$area_code) |>
        dplyr::mutate(footprint = nm, present = TRUE)
    }) |>
    dplyr::bind_rows()
  if (nrow(members) == 0L) {
    return(members)
  }
  members |>
    tidyr::pivot_wider(
      names_from = "footprint",
      values_from = "present",
      values_fill = FALSE
    ) |>
    .pcs_keep_disagreements()
}

.pcs_keep_disagreements <- function(wide) {
  flags <- setdiff(names(wide), c("lon", "lat", "area_code"))
  wide |>
    dplyr::filter(rowSums(dplyr::pick(dplyr::all_of(flags))) < length(flags))
}

# Land present in the validation layer but claimed by no live polity is
# emitted, never renormalised into the polities: an unexplained gap that could
# be either a geometry error or discarded unclaimed land is unattributable.
.pcs_unassigned <- function(support, luh2) {
  .pcs_require_cols(luh2, c("lon", "lat", "terrestrial_ha"), "data$luh2")
  claimed <- support |>
    dplyr::filter(.data$coverage_status != "crosswalk_only") |>
    dplyr::summarise(
      claimed_land_ha = sum(.data$land_area_ha),
      .by = c("lon", "lat", "start_year", "end_year")
    )
  luh2 |>
    dplyr::distinct(.data$lon, .data$lat, .data$terrestrial_ha) |>
    dplyr::full_join(claimed, by = c("lon", "lat")) |>
    dplyr::mutate(
      claimed_land_ha = dplyr::coalesce(.data$claimed_land_ha, 0),
      terrestrial_ha = dplyr::coalesce(.data$terrestrial_ha, 0),
      unassigned_land_ha = pmax(
        .data$terrestrial_ha - .data$claimed_land_ha,
        0
      )
    ) |>
    dplyr::filter(.data$unassigned_land_ha > .pcs_area_floor_ha()) |>
    tibble::as_tibble()
}

# -- Shared helpers -----------------------------------------------------------

.pcs_expand <- function(support, years) {
  if (is.null(years)) {
    return(support)
  }
  out <- expand_polycell_years(support, years)
  .pcs_copy_diagnostics(out, support)
}

.pcs_copy_diagnostics <- function(out, support) {
  purrr::reduce(.pcs_diagnostic_names(), .init = out, \(acc, nm) {
    attr(acc, nm) <- attr(support, nm)
    acc
  })
}

.pcs_strip_diagnostics <- function(out) {
  purrr::reduce(.pcs_diagnostic_names(), .init = out, \(acc, nm) {
    attr(acc, nm) <- NULL
    acc
  })
}

.pcs_diagnostic_names <- function() {
  c(
    "coverage",
    "overlap",
    "water_excess",
    "footprints",
    "footprint_diff",
    "unassigned"
  )
}

.pcs_require_cols <- function(x, cols, arg) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0L) {
    cli::cli_abort("{.arg {arg}} is missing column{?s}: {.field {missing}}.")
  }
  invisible(x)
}

.pcs_empty_pieces <- function() {
  tibble::tibble(
    cell_id = integer(),
    lon = double(),
    lat = double(),
    cell_area_ha = double(),
    polity_area_ha = double(),
    polity_code = character(),
    start_year = integer(),
    end_year = integer(),
    area_code = integer(),
    polygon_status = character(),
    coverage_status = character(),
    ice_area_ha = double()
  )
}
