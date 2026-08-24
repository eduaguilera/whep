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
#
# WHICH cells a polity holds is NOT decided here. whep#776 rasterised the
# polygons itself with `terra::extract(exact = TRUE)`; whep#619 had already
# shipped the same intersection as the package's canonical spatial support, the
# polycell -- geodesic `sf::st_area()` on s2, keyed on validity intervals,
# conserving by construction, and unable to hand one cell to two polities twice
# over. This file now reads that table instead of building a second answer to
# the same question (whep#800).

#' Build a pre-1962 land series measured on each year's own borders
#'
#' @description
#' Emit the land table the pre-1962 production back-cast consumes -- `year`,
#' `area_code`, `Cropland`, `Pasture` and `agriland`, all in Mha -- with the
#' hectares summed from gridded LUH2 inside the polygon of the polity that
#' `area_code` resolved to in that year, instead of inside present-day borders.
#'
#' The cell-by-polity intersection is not measured here: it is read from the
#' polycell support table ([read_polycell_support()]), which is that
#' intersection measured geodesically on s2 and keyed on each polity's validity
#' interval.
#'
#' A cell's land is shared among the polities whose territory covers it, in
#' proportion to that territory renormalised to one per cell, which is the rule
#' `build_cell_polity_fraction()` already uses. Renormalising matters: LUH2's
#' state fractions are fractions of the whole cell and already discount open
#' water, so weighting them by a raw coastal cell's land share would discount it
#' twice and lose 12-15% of the land of an island or heavily coastal country.
#'
#' `fill_proxy_growth()` consumes only this series' year-on-year ratios, so a
#' change of territory can only reach the back-cast as a ratio. What that ratio
#' should be is a real choice, and `boundary_step` makes it:
#'
#' * `"relink"` (default) re-measures the previous year inside the **incoming**
#'   polity's polygon before taking the ratio, so only within-territory growth
#'   is ever used and annexing a province never moves the back-cast. On Ethiopia
#'   in 1952, when Eritrea joins, that is +1.9% instead of +8.0%.
#' * `"level_step"` takes the ratio between the two polygons as measured, so the
#'   territorial change passes through as a level step and the 1850 row is
#'   scaled to the smaller empire it is labelled with. That is the reframing the
#'   whole method exists for; it is also the option most exposed to a bad
#'   polygon, because an artefact of the polity database then compounds down the
#'   back-cast exactly as a real annexation would.
#'
#' Measured over 1850-1961 against the present-day series, 18.0% of back-cast
#' crop tonnage at 1850 sits between the two rules, falling to 0.07% by 1960.
#'
#' This reads gridded LUH2 for every requested year and is minutes-to-tens-of-
#' minutes of work, so it belongs in a `data-raw/` materialisation step, not in
#' a test or an example.
#'
#' @param years Integer vector of calendar years to measure. Defaults to
#'   `1850:1961`, the span the back-cast uses.
#' @param boundary_step How a year-on-year ratio is taken across a change of
#'   territory: `"level_step"` (default) or `"relink"`. They answer different
#'   questions and differ by up to 18% of back-cast tonnage, so the choice is
#'   the method, not a tuning knob -- see the description.
#'
#'   `"level_step"` lets the series step when the territory changes, because a
#'   different polity is a different thing being measured. That is what a
#'   per-polity series means, and it is why this function exists: on Ethiopia it
#'   puts 1850 cropland at 1.52 Mha against the present-day 3.22, dropping the
#'   land Menelik annexed in the 1880s-90s that the area never held in 1850.
#'
#'   `"relink"` re-measures the previous year inside the incoming polity's
#'   polygon so a change of territory never appears as growth. That suits a
#'   FIXED-territory series, where the step is an artefact. It is NOT the
#'   conservative choice here: because `fill_proxy_growth()` consumes only
#'   ratios, suppressing that channel also suppresses the correction, and
#'   Ethiopia's 1850 comes back to 3.24 Mha -- within 0.6% of the present-day
#'   figure this method exists to replace (whep#761).
#' @param data Named list of pre-loaded inputs bypassing the readers, for tests:
#'   `polity_areas` (`year`, `area_code`, `polity_code`), `support` (a
#'   [read_polycell_support()] table), `cover` (`polity_code`, `lon`, `lat`,
#'   `frac`, which is `support` already reduced to one weight per cell) and
#'   `cell_areas` (`year`, `lon`, `lat`, `land_use`, `area_ha`). Each falls back
#'   to its reader when absent.
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
  boundary_step = c("level_step", "relink"),
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
    .polity_cell_cover(unique(polity_areas$polity_code), data$support)
  .warn_land_without_polycell(polity_areas, cover)

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

# How much of each 0.5-degree LUH2 cell each polity holds, read off the polycell
# support table rather than measured here.
#
# WHICH COLUMN IS THE WEIGHT is a decision, and it is measured rather than
# assumed. It is `polity_area_ha`, the polity's whole territory in the cell, and
# NOT `land_area_ha`:
#
#   * `.land_in_polygons()` renormalises the weights to one per cell, and
#     `build_polycell_support()` apportions a cell's inland water across its
#     polycells PRO RATA BY `polity_area_ha`. Within a cell the water is
#     therefore a common factor that cancels exactly in that renormalisation, so
#     subtracting it changes nothing -- except where the cap that keeps
#     `land_area_ha` non-negative bites, and there it does something bad. 1,502
#     polycells covering 62.4 Mha of territory have their whole territory
#     consumed by apportioned water and carry `land_area_ha == 0`: Canada on the
#     Great Lakes and Hudson Bay (12.7 Mha over its periods) and the USSR on the
#     Caspian and Arctic shores (~3 Mha per period) are the largest. Weighting
#     by land drops their claim on those cells outright and hands the cell to a
#     neighbour -- the 12-15% coastal loss whep#776 fought, arriving through a
#     different door. Measured over 1850-1961 it moves Eritrea by up to 23.5%,
#     Switzerland 21.7%, Mali 20.7% and Eswatini 12.1%, and takes two
#     bucket-years to NA outright.
#   * Permanent ice is the one component that does NOT cancel, and it is the
#     small one. `polity_area_ha - ice_area_ha` is a one-expression alternative
#     that moves no bucket-year by as much as 1% -- its largest single-bucket
#     maximum over 1850-1961 is 0.52% on Switzerland, and 246 of 16,125
#     bucket-years move by more than 0.1%. It would also discount 1850 land with
#     a present-day glacier outline, so it is left in.
#
# `polity_area_ha` is also EXACTLY time-invariant per (cell, polity): a maximum
# relative standard deviation of 0 over the 33,433 (cell, polity) pairs the
# shipped table splits into more than one interval, because it is pure geometry.
# That is what lets one cover serve every year, and it is why
# `.land_in_polygons()`'s join on `polity_code` alone really is the
# time-invariant join `.territorial_join_baseline()` classifies it as.
# `land_area_ha` is not time-invariant -- it moves with the water apportionment
# as a cell's set of claimants changes -- so a single cover built on it would
# mix bases across intervals.
.polity_cell_cover <- function(polity_codes, support = NULL) {
  support <- data.table::as.data.table(support %||% read_polycell_support())
  held <- support[polity_code %in% polity_codes & polity_area_ha > 0]
  if (nrow(held) == 0L) {
    cli::cli_abort(c(
      "No polity in {.arg polity_codes} has a row in the polycell support.",
      i = "The support is keyed on {.field polity_code}. None of
           {.val {utils::head(polity_codes, 3)}} appears in it, which usually
           means it was built against a different {.code polities} vintage;
           regenerate it with {.fn build_polycell_support}."
    ))
  }
  # `max()`, not `unique()`: successive intervals of one polycell repeat the
  # same geometry, and reducing them cannot be allowed to emit the polity twice
  # in one cell, which would double its weight there.
  held[, .(frac = max(polity_area_ha)), by = .(polity_code, lon, lat)]
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
  merge(
    cell_areas,
    .polity_cell_shares(live, cover),
    by = c("lon", "lat"),
    allow.cartesian = TRUE
  )[,
    .(land_mha = sum(area_ha * share) / 1e6),
    by = .(area_code, land_use)
  ]
}

# Each live polity's share of each cell it covers, renormalised to one per cell.
# Extracted so the crop-level aggregation in `cell_backcast.R` reuses this exact
# table instead of building a second answer to the same question -- and so the
# year-free `polity_code` join `.territorial_join_baseline()` classifies stays
# ONE join with one owner rather than multiplying with each new consumer.
.polity_cell_shares <- function(live, cover) {
  shares <- merge(
    data.table::as.data.table(live)[, .(area_code, polity_code)],
    cover,
    by = "polity_code",
    allow.cartesian = TRUE
  )
  shares <- unique(shares, by = c("area_code", "lon", "lat"))
  shares[, share := frac / sum(frac), by = .(lon, lat)]
  shares[, .(area_code, lon, lat, share)]
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
# the series is the plain measurement. It also makes every ratio immune to a
# CONSTANT overlap between two polygons, because both sides of it are measured
# with the same year's polity set and a constant factor cancels.
.chain_link_land <- function(measured, boundary_step) {
  data.table::setorder(measured, area_code, land_use, year)
  if (boundary_step == "level_step") {
    measured[, land_mha := land_now]
  } else {
    measured[,
      log_ratio := .safe_log_ratio(land_next, land_now),
      by = .(area_code, land_use)
    ]
    measured[,
      land_mha := utils::tail(land_now, 1) * exp(.suffix_sum(log_ratio)),
      by = .(area_code, land_use)
    ]
  }
  .land_series_to_wide(measured)
}

# log( land_next(y-1) / land_now(y) ), NA where either side is missing or zero.
.safe_log_ratio <- function(land_next, land_now) {
  ratio <- c(NA_real_, utils::head(land_next, -1) / land_now[-1])
  out <- log(ratio)
  out[!is.finite(out)] <- NA_real_
  out[1] <- 0
  out
}

# `sum(x[(k + 1):n])` per position, accumulated from the END so a break stops
# only the years BEFORE it. `cumsum(x)[n] - cumsum(x)` is the same arithmetic
# and the wrong NA behaviour: one unmeasurable year in the middle would make
# `cumsum(x)[n]` NA and wipe out the bucket's whole 1850-1961 series instead of
# just the part the break cuts off. Four polities have no polygon
# (whep-polities#155), so this is reachable, not hypothetical.
.suffix_sum <- function(x) {
  rev(cumsum(rev(c(x[-1], 0))))
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

# A polity with data and no polycell cannot be measured, and because the series
# is chain-linked a hole also stops every earlier year of that bucket from being
# reached. Say so with the codes, rather than emitting a shorter series and
# letting the loss look like a year range.
#
# Two disjoint reasons land here and the message keeps them apart, because only
# one of them is this package's to fix. A polity has no polycell either because
# it has no polygon at all (`polygon_status = "unassigned"`, whep-polities#155)
# or because `build_polycell_support()` excludes it: it drops
# `polity_type == "aggregate"`, since an aggregate's polygon overlaps its
# members' and the support has to be a partition. Ten of the polities the
# pre-1962 resolver reaches are aggregates, `BLX-1850-1999` among them.
.warn_land_without_polycell <- function(polity_areas, cover) {
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
      no polycell, so {length(affected)} bucket{?s} cannot be measured in every
      year: {.val {utils::head(missing, 5)}}.",
    "i" = "A polity has none when it carries no polygon
      (whep-polities#155) or when {.fn build_polycell_support} excludes it,
      which it does for every {.val aggregate}.",
    "i" = "The series is chain-linked, so a hole also cuts off every earlier
      year of that bucket."
  ))
  invisible(NULL)
}
