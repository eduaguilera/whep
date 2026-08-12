# The pre-1962 back-cast estimates production as `tonnes = ha * t_ha`. The yield
# half is already historical: `.fill_yields()` back-casts `t_ha` against the
# `international-yields` pin, 1,058,295 usable pre-1962 observations over
# 1840-1961. The area half is not. It comes from the `luh2-areas` pin, which is
# LUH2 land pre-aggregated to PRESENT-DAY ISO3, so a row labelled with the 1961
# entity is measured on the borders that entity has today.
#
# This file measures the area half on each year's own borders instead, by
# summing gridded LUH2 inside the polygon of the polity that `area_code`
# resolves to IN THAT YEAR, resolved unfloored (`backcast_anchor = -Inf`, the
# switch `.resolve_hist_trade_polities()` already uses for genuinely historical
# trade sources). It adds no information -- every pre-1961 year still derives
# from the 1961 anchor, a land reconstruction and the yield series -- it frames
# the same information on the right territory.
#
# `.add_federation_land_rows()` is the precedent: it already rewrites the land
# table before this same seam, for the same reason (LUH2 keyed on present-day
# ISO3 cannot answer for a dissolved federation). This is that intervention
# generalised from total dissolutions to every territorial change, and it
# subsumes it -- Czechoslovakia has a polygon, so it needs no successor union.

#' Build a pre-1962 land series measured on each year's own borders
#'
#' @description
#' Emit the land table the pre-1962 production back-cast consumes -- `year`,
#' `area_code`, `Cropland`, `Pasture` and `agriland`, all in Mha -- with the
#' hectares summed from gridded LUH2 inside the polygon of the polity that
#' `area_code` resolved to in that year, instead of inside present-day borders.
#'
#' A cell's land is shared among the polities whose polygons cover it, in
#' proportion to the covered fraction renormalised to one per cell, which is the
#' rule `build_cell_polity_fraction()` already uses. Renormalising matters:
#' LUH2's state fractions are fractions of the whole cell and already discount
#' open water, so weighting them by a raw coastal cell's land share would
#' discount it twice and lose 12-15% of the land of an island or heavily coastal
#' country.
#'
#' `fill_proxy_growth()` consumes only this series' year-on-year ratios, so at a
#' handover year a ratio taken between two different polygons would turn a
#' territorial change into growth and compound it down the whole back-cast. With
#' `boundary_step = "relink"` (the default) the previous year is re-measured
#' inside the **incoming** polity's polygon before the ratio is taken, so the
#' change stays a level step at the boundary. On Ethiopia in 1952, when Eritrea
#' joins, that is the difference between a spurious +7.0% and the real +1.9%.
#'
#' This reads gridded LUH2 for every requested year and is minutes-to-tens-of-
#' minutes of work, so it belongs in a `data-raw/` materialisation step, not in
#' a test or an example.
#'
#' @param years Integer vector of calendar years to measure. Defaults to
#'   `1850:1961`, the span the back-cast uses.
#' @param boundary_step How a year-on-year ratio is taken across a change of
#'   territory. `"relink"` (default) re-measures the previous year inside the
#'   incoming polity's polygon; `"none"` takes the ratio between the two
#'   polygons as measured, which is what makes a border change look like growth.
#' @param data Named list of pre-loaded inputs bypassing the readers, for tests:
#'   `polity_areas` (`year`, `area_code`, `polity_code`), `cover`
#'   (`polity_code`, `lon`, `lat`, `frac`) and `cell_areas` (`year`, `lon`,
#'   `lat`, `land_use`, `area_ha`). Each falls back to its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @returns A tibble with columns `year`, `area_code`, `polity_code`,
#'   `Cropland`, `Pasture` and `agriland`. `area_code` is the
#'   `polity_area_code` aggregation bucket, the same key `.read_land_areas()`
#'   emits, so the result is a drop-in for it at the back-cast seam.
#'   `polity_code` names the territory each year was measured on, and is
#'   semicolon-separated where a bucket holds more than one polity in a year.
#'
#' @export
#'
#' @examples
#' build_historical_land_areas(example = TRUE)
build_historical_land_areas <- function(
  years = 1850:1961,
  boundary_step = c("relink", "none"),
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_historical_land_areas())
  }
  boundary_step <- rlang::arg_match(boundary_step)
  years <- sort(unique(as.integer(years)))
  data <- data %||% list()
  polity_areas <- data$polity_areas %||% .polity_area_by_year(years)
  cover <- data$cover %||%
    .polity_cell_cover(unique(polity_areas$polity_code))
  .warn_land_without_geometry(polity_areas, cover)

  purrr::map(
    years,
    \(yr) .measure_land_year(yr, polity_areas, cover, data$cell_areas)
  ) |>
    data.table::rbindlist(use.names = TRUE) |>
    .chain_link_land(boundary_step) |>
    .label_land_polities(polity_areas)
}

# Name the territory each row was measured on. Keyed on (year, area_code), so
# the label is the one that year and not a present-day stand-in.
.label_land_polities <- function(wide, polity_areas) {
  labels <- polity_areas |>
    tibble::as_tibble() |>
    dplyr::summarise(
      polity_code = paste(sort(unique(.data$polity_code)), collapse = "; "),
      .by = c("year", "area_code")
    )
  wide |>
    dplyr::left_join(labels, by = c("year", "area_code")) |>
    dplyr::relocate("polity_code", .after = "area_code")
}

# The polity every reporting bucket resolves to in every requested year, with
# the back-cast floor switched OFF. Floored (the pipeline default), 1900 Ethiopia
# resolves to the 1961 entity and is measured on its borders; unfloored it
# resolves to ETH-1897-1902 and is measured on that empire's borders, which is
# the whole point of this file.
#
# A bucket can hold more than one polity in one year -- bucket 206 is Sudan
# (former) and holds both Sudan and South Sudan once they split -- so the result
# is deliberately long, and the caller sums land over the bucket's polities.
.polity_area_by_year <- function(years) {
  areas <- unique(.current_area_lookup(include_unmapped = FALSE)$area_code)
  request <- data.table::CJ(
    area_code = areas[!is.na(areas)],
    year = as.integer(years)
  )
  resolved <- .add_polity_columns_dt(
    request,
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE,
    backcast_anchor = -Inf
  )
  out <- unique(resolved[
    !is.na(polity_code) & !is.na(polity_area_code),
    .(
      year,
      area_code = as.integer(polity_area_code),
      polity_code,
      mapping_status
    )
  ])
  data.table::setorder(out, year, area_code, polity_code)
  .keep_measurable_polities(out)
}

# A polygon can only be measured for a bucket when it really is that bucket's
# territory in that year. Two things break that, and both are silent:
#
# 1. `.add_polity_columns_dt()` falls back to the NEAREST period when no period
#    covers the year, stamping `mapping_status = "out_of_span"`. In 1961 that
#    hands bucket 167 (Czechia) and bucket 185 (the Russian Federation)
#    polygons for states that did not exist, sitting inside Czechoslovakia's and
#    the USSR's. Two live polygons then claim the same cells and split them.
# 2. Most reporting areas have no polity of their own before FAOSTAT begins and
#    resolve to the residual `ROW-1850-2025` -- 62 buckets do in 1961 alone. Each
#    then took 1/62 of every ROW cell, which is what shrank Belgium-Luxembourg
#    to 0.026 Mha of cropland.
#
# Neither is a territory, so neither is measured: the bucket gets no land that
# year and its production is simply not back-cast, exactly as an area with no
# LUH2 match is treated today. A residual standing in for 62 areas cannot be
# any one of their borders, and saying so is the point of the whole exercise.
#
# `out_of_span` is the only status excluded, because it is the only one that
# means "no period covers this year". `manual` is a real, hand-checked mapping
# and carries three of the four dissolved federations -- Czechoslovakia, the
# USSR and Yugoslavia -- which this method reaches WITHOUT
# `federation_land = "successor_union"`, because each has a polygon of its own.
.keep_measurable_polities <- function(resolved) {
  claims <- resolved[,
    .(n_buckets = data.table::uniqueN(area_code)),
    by = .(year, polity_code)
  ]
  out <- merge(resolved, claims, by = c("year", "polity_code"), sort = FALSE)
  keep <- out[
    !(mapping_status %in% c("out_of_span", "unmapped")) & n_buckets == 1L
  ]
  .inform_unmeasurable_buckets(out, keep)
  data.table::setorder(keep, year, area_code, polity_code)
  keep[, .(year, area_code, polity_code)]
}

.inform_unmeasurable_buckets <- function(resolved, keep) {
  lost <- setdiff(unique(resolved$area_code), unique(keep$area_code))
  partial <- keep[, .(n = data.table::uniqueN(year)), by = area_code]
  full <- data.table::uniqueN(keep$year)
  cli::cli_inform(c(
    "i" = "{length(unique(keep$area_code))} bucket{?s} can be measured on
      {?its/their} own borders; {length(lost)} cannot in any year and
      {sum(partial$n < full)} only in some.",
    "i" = "A bucket is skipped where the resolver falls back out of span, or
      where its polity is a residual standing in for several areas. Its
      pre-1962 production is then not back-cast."
  ))
  invisible(NULL)
}

# The fraction of each 0.5-degree LUH2 cell that each polity's polygon covers.
#
# This is the one join in this file keyed on a territorial column without a
# year, and it is genuinely time-invariant: a polity code already names its own
# period (`ETH-1952-1993`), so its polygon cannot depend on a year. It is
# classified in `.territorial_join_baseline()` on exactly that ground.
.polity_cell_cover <- function(polity_codes) {
  .assert_polygon_packages()
  polygons <- get_polity_geometries(polity_codes)
  polygons <- polygons[!sf::st_is_empty(sf::st_geometry(polygons)), ]
  if (nrow(polygons) == 0L) {
    cli::cli_abort("No polity in {.arg polity_codes} carries a polygon.")
  }
  cli::cli_progress_step(
    "Rasterising {nrow(polygons)} polity polygon{?s} onto the LUH2 grid"
  )
  template <- .luh2_cell_template()
  extracted <- terra::extract(
    template,
    terra::vect(polygons[, "polity_code"]),
    exact = TRUE
  )
  data.table::setDT(extracted)
  extracted[, polity_code := polygons$polity_code[ID]]
  extracted <- extracted[
    !is.na(cell) & fraction > 0,
    .(frac = sum(fraction)),
    by = .(polity_code, cell)
  ]
  merge(extracted, .luh2_cell_lookup(template), by = "cell", sort = FALSE)[,
    .(polity_code, lon, lat, frac)
  ]
}

# A 0.5-degree global raster whose cell centres are the LUH2 cell centres this
# package reports (-179.75 .. 179.75, 89.75 .. -89.75), carrying the cell index
# so `terra::extract()` returns something joinable back to lon/lat.
.luh2_cell_template <- function() {
  template <- terra::rast(
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90,
    resolution = 0.5,
    crs = "EPSG:4326"
  )
  terra::values(template) <- seq_len(terra::ncell(template))
  names(template) <- "cell"
  template
}

.luh2_cell_lookup <- function(template) {
  centres <- terra::xyFromCell(template, seq_len(terra::ncell(template)))
  data.table::data.table(
    cell = seq_len(terra::ncell(template)),
    lon = centres[, 1],
    lat = centres[, 2]
  )
}

# Land measured for one year, twice: `land_now` inside the polygons live THAT
# year, and `land_next` inside the polygons live the FOLLOWING year. The second
# is what makes the boundary rule possible -- it is the previous year of the
# following year's territory, so `.chain_link_land()` can take every ratio
# between two measurements of the same polygon.
#
# `yr`, not `year`: inside a data.table `[` an argument named after a column is
# shadowed by the column, so `polity_areas[year == year]` silently keeps every
# row. That defect made the polity set never change, which made every boundary
# step disappear -- `test_historical_land_areas.R` is what caught it.
.measure_land_year <- function(yr, polity_areas, cover, cell_areas = NULL) {
  areas <- .luh2_year_cell_areas(yr, cell_areas)
  live <- data.table::as.data.table(polity_areas)[year == yr]
  now <- .land_in_polygons(areas, live, cover)
  data.table::setnames(now, "land_mha", "land_now")
  now[, year := yr]
  ahead <- data.table::as.data.table(polity_areas)[year == yr + 1L]
  if (nrow(ahead) == 0L) {
    return(now[, land_next := land_now][])
  }
  nxt <- .land_in_polygons(areas, ahead, cover)
  data.table::setnames(nxt, "land_mha", "land_next")
  nxt[, year := yr]
  merge(now, nxt, by = c("year", "area_code", "land_use"), all = TRUE)
}

# Sum gridded land into each bucket, sharing every cell among the polygons that
# cover it in proportion to the covered fraction, renormalised to one per cell.
.land_in_polygons <- function(cell_areas, live, cover) {
  shares <- merge(
    data.table::as.data.table(live)[, .(area_code, polity_code)],
    cover,
    by = "polity_code",
    allow.cartesian = TRUE
  )
  shares <- unique(shares, by = c("area_code", "lon", "lat"))
  shares[, share := frac / sum(frac), by = .(lon, lat)]
  merge(
    cell_areas,
    shares[, .(area_code, lon, lat, share)],
    by = c("lon", "lat"),
    allow.cartesian = TRUE
  )[,
    .(land_mha = sum(area_ha * share) / 1e6),
    by = .(area_code, land_use)
  ]
}

# Per-cell LUH2 class areas for one year, UNCLIPPED by the present-day country
# grid. read_luh2_landuse() clips to it, which is what this file exists to stop
# doing, so the raw states are aggregated here instead.
.luh2_year_cell_areas <- function(yr, cell_areas = NULL) {
  if (!is.null(cell_areas)) {
    out <- data.table::as.data.table(cell_areas)
    return(out[year == yr, .(lon, lat, land_use, area_ha)])
  }
  states <- .luh2_read_states_source(years = yr, states_source = "auto")
  out <- data.table::as.data.table(.luh2_map_classes(states))
  out[
    land_use %in% c("cropland", "grassland"),
    .(lon, lat, land_use, area_ha = fraction * .luh2_cell_area_ha(lat))
  ]
}

# Turn the per-year measurements into one series per bucket whose consecutive
# ratios are all taken between two measurements of the SAME polygon.
#
# `fill_proxy_growth()` reads nothing but those ratios, so re-integrating the
# corrected ratios from the last year backwards is the level-step rule stated as
# a series: L*(last) = land_now(last), and L*(y-1) = L*(y) * land_next(y-1) /
# land_now(y). Where the territory does not change, land_next == land_now and
# the series is the plain measurement.
.chain_link_land <- function(measured, boundary_step) {
  data.table::setorder(measured, area_code, land_use, year)
  if (boundary_step == "none") {
    measured[, land_mha := land_now]
  } else {
    measured[,
      log_ratio := c(0, log(utils::head(land_next, -1) / land_now[-1])),
      by = .(area_code, land_use)
    ]
    measured[,
      land_mha := utils::tail(land_now, 1) *
        exp(cumsum(log_ratio)[.N] - cumsum(log_ratio)),
      by = .(area_code, land_use)
    ]
  }
  .land_series_to_wide(measured)
}

.land_series_to_wide <- function(measured) {
  measured |>
    tibble::as_tibble() |>
    dplyr::mutate(
      land_use = dplyr::if_else(
        .data$land_use == "cropland",
        "Cropland",
        "Pasture"
      )
    ) |>
    dplyr::select("year", "area_code", "land_use", "land_mha") |>
    tidyr::pivot_wider(names_from = "land_use", values_from = "land_mha") |>
    ensure_columns(
      tibble::tibble(
        year = integer(),
        area_code = integer(),
        Cropland = double(),
        Pasture = double()
      ),
      defaults = list(Cropland = 0, Pasture = 0)
    ) |>
    dplyr::mutate(agriland = .data$Cropland + .data$Pasture) |>
    dplyr::arrange(.data$year, .data$area_code)
}

# A polity with data and no polygon cannot be measured, and because the series
# is chain-linked a hole also stops every earlier year of that bucket from being
# reached. Say so with the codes, rather than emitting a shorter series and
# letting the loss look like a year range.
.warn_land_without_geometry <- function(polity_areas, cover) {
  missing <- setdiff(
    unique(polity_areas$polity_code),
    unique(cover$polity_code)
  )
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  affected <- unique(
    polity_areas$area_code[polity_areas$polity_code %in% missing]
  )
  cli::cli_warn(c(
    "!" = "{length(missing)} polit{?y/ies} reachable from a reporting area have
      no polygon, so {length(affected)} bucket{?s} cannot be measured in every
      year: {.val {utils::head(missing, 5)}}.",
    "i" = "The series is chain-linked, so a hole also cuts off every earlier
      year of that bucket. See whep-polities#155."
  ))
  invisible(NULL)
}

.assert_polygon_packages <- function() {
  missing <- c("sf", "terra")[
    !vapply(c("sf", "terra"), .namespace_available, logical(1))
  ]
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "Package{?s} {.pkg {missing}} {?is/are} required to measure land inside
         polity polygons.",
        i = "Install {?it/them}, or use the present-day land series."
      ),
      class = "whep_sf_required"
    )
  }
  invisible(NULL)
}

.namespace_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
