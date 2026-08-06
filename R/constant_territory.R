# nolint start: object_length_linter.
#' Build a constant-territory time series for a reference year's boundaries
#'
#' @description
#' Estimates a time series of a quantity over a **fixed** set of territorial
#' boundaries — the polities active in `ref_year` — from data reported under
#' the *changing* historical boundaries of each data year.
#'
#' Country borders change over time, so there is no raw constant-territory
#' series: a 1900 figure for "Austria-Hungary" is not a figure for present-day
#' Austria. This function estimates one by spatial reallocation (dasymetric
#' areal interpolation):
#'
#' 1. For each data `year`, the value reported by each source polity is spread
#'    over **that polity's own extent for that year** across a regular grid,
#'    weighted by a `covariate` density (e.g. gridded cropland or population;
#'    uniform = plain areal weighting).
#' 2. The grid is then re-aggregated to the `ref_year` target boundaries: a
#'    target's estimate is the sum of grid mass falling inside it.
#' 3. Target territory **not covered** by any source with data in that year is
#'    *imputed* — its grid cells still carry covariate mass, so they are filled
#'    at a donor intensity (value per unit covariate) rather than left at zero.
#'    The fraction of a target's covariate mass that had to be imputed is
#'    reported as `imputed_share`, an honest confidence signal.
#'
#' The estimate is only as good as the `covariate`: supply the same gridded
#' surface used elsewhere in WHEP spatialization (cropland for crop output,
#' population for demographic series, livestock density for animals). With
#' `covariate = NULL` the method reduces to area-weighted areal interpolation.
#'
#' @param data A data frame of reported values with columns:
#'   - `year`: integer data year.
#'   - `polity_code`: the source polity that reported the value (must be active
#'     in `year` and carry a polygon).
#'   - `value`: numeric value (summed if a polity appears more than once).
#' @param ref_year Integer. Target boundaries are the polities active in this
#'   year, under the validity convention described for `polities`: a polity
#'   does not answer for the year its successor takes over. The same reading
#'   selects the sources of each data year.
#' @param polities An `sf` of polity polygons with `polity_code`, `start_year`,
#'   `end_year` and geometry. Defaults to [get_polity_geometries()].
#'   `start_year` is inclusive; `end_year` is **exclusive at a succession** and
#'   **inclusive at the open end**. So 2014 resolves to `"RUS-2014-2025"`,
#'   never to `"RUS-1991-2014"`, while 2025 still resolves to
#'   `"RUS-2014-2025"` because nothing succeeds it. An interval counts as open
#'   when it ends on the last year the supplied table covers and no
#'   later-starting interval of the same polity follows it. Where the table
#'   still carries overlapping intervals for one polity, the interval starting
#'   on the resolved year wins, then the latest-starting one.
#' @param covariate `NULL` (uniform density, i.e. area weighting) or a function
#'   `function(centroids_sf, year) -> numeric` returning a non-negative density
#'   per grid-cell centroid (centroids are supplied in `crs_equal_area`).
#' @param resolution Grid cell size, in metres of `crs_equal_area`. Default
#'   25000 (25 km). Smaller is more accurate but slower.
#' @param donor Gap-imputation rule: `"regional"` (default) fills uncovered
#'   target cells at the region-wide value-per-covariate intensity of the
#'   sources with data that year; `"none"` leaves them at zero (covered-only).
#' @param crs_equal_area EPSG code of an equal-area CRS used for gridding and
#'   areas. Default 6933 (NSIDC EASE-Grid 2.0 Global).
#' @param max_cells Safety cap on grid cells per year (default 2e6). Aborts if
#'   the source/target extent would exceed it (usually a stray continent-scale
#'   target); restrict `polities`, coarsen `resolution`, or raise this.
#' @param verbose Logical; emit progress/warnings.
#'
#' @return A tibble, one row per (`ref_year`-target, data `year`):
#'   - `target_polity_code`, `year`
#'   - `value`: constant-territory estimate (`covered + imputed`)
#'   - `covered`: mass from cells overlapping a source with data
#'   - `imputed`: mass added for uncovered cells
#'   - `imputed_share`: covariate fraction imputed (0 = fully observed)
#'   - `n_sources`: number of source polities contributing that year
#'   - `unallocated`: total reported value from sources that could not be placed
#'     on the grid that year (constant within a year; 0 when all were placed).
#'     Kept out of `covered`/`imputed` so it is neither smeared nor lost.
#'
#' @examples
#' # Self-contained toy: two adjacent square polities. Only "P1" reports a
#' # value in 1900, so when the series is rebuilt onto the boundaries active in
#' # `ref_year` 2000 (both polities), "P2" is imputed from "P1"'s intensity.
#' make_square <- function(xmin, ymin, side) {
#'   sf::st_polygon(list(rbind(
#'     c(xmin, ymin),
#'     c(xmin + side, ymin),
#'     c(xmin + side, ymin + side),
#'     c(xmin, ymin + side),
#'     c(xmin, ymin)
#'   )))
#' }
#' polities <- sf::st_sf(
#'   polity_code = c("P1", "P2"),
#'   start_year = c(1800L, 1800L),
#'   end_year = c(2025L, 2025L),
#'   geometry = sf::st_sfc(
#'     make_square(0, 0, 2),
#'     make_square(2, 0, 2),
#'     crs = 4326
#'   )
#' )
#' reported <- tibble::tibble(
#'   year = 1900L,
#'   polity_code = "P1",
#'   value = 100
#' )
#' build_constant_territory_series(
#'   reported,
#'   ref_year = 2000,
#'   polities = polities,
#'   resolution = 50000,
#'   verbose = FALSE
#' )
#' @export
build_constant_territory_series <- function(
  data,
  ref_year,
  polities = NULL,
  covariate = NULL,
  resolution = 25000,
  donor = c("regional", "none"),
  crs_equal_area = 6933,
  max_cells = 2e6,
  verbose = TRUE
) {
  donor <- match.arg(donor)
  required <- c("year", "polity_code", "value")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    cli::cli_abort("`data` is missing column{?s}: {.field {missing}}.")
  }
  if (!is.null(covariate) && !is.function(covariate)) {
    cli::cli_abort(
      "`covariate` must be NULL or a function(centroids_sf, year)."
    )
  }

  if (is.null(polities)) {
    polities <- get_polity_geometries()
  }
  polities <- polities[, c("polity_code", "start_year", "end_year")]
  # only polities that actually carry a polygon can host or receive mass
  polities <- polities[!sf::st_is_empty(polities), ]
  polities <- sf::st_make_valid(sf::st_transform(polities, crs_equal_area))

  # `end_year` is EXCLUSIVE at a succession (see [polities]), so such a period
  # covers `start_year:(end_year - 1)`. Reading it inclusively made a polity and
  # its successors all active in the hand-over year -- 238 polities carry a
  # polygon in 1993 on that reading, Czechoslovakia on top of Czechia and
  # Slovakia, and 453 extra active polity-years over 1850-2024. Since
  # `.assign_polity()` gives each cell exactly one source and one target, the
  # dissolved predecessor was capturing the cells its successors should have
  # received. `.active_polities()` below applies that rule together with DA-24's
  # open end and the same-polity dedupe.
  #
  # The open-end flag is computed ONCE on the whole table: whether an interval is
  # succeeded is a property of the table, and the per-year source subset below
  # would hide the successor and reopen a genuinely dissolved epoch.
  open_ended <- .open_ended_intervals(
    polities$start_year,
    polities$end_year,
    .polity_family(polities$polity_code)
  )

  target <- .active_polities(polities, ref_year, open_ended = open_ended)
  if (nrow(target) == 0) {
    covered <- if (nrow(polities) == 0L) {
      "none, since no supplied polity carries a polygon"
    } else {
      paste0(min(polities$start_year), "-", max(polities$end_year))
    }
    cli::cli_abort(c(
      "No polities with a polygon are active in `ref_year` = {ref_year}.",
      "i" = "`end_year` is exclusive at a succession, so an interval ending in {ref_year} does not cover it unless nothing succeeds it. Covered years: {covered}."
    ))
  }

  data <- data[!is.na(data$value), required]
  years <- sort(unique(data$year))
  results <- vector("list", length(years))

  for (k in seq_along(years)) {
    y <- years[k]
    dy <- data[data$year == y, ]
    # Restrict to the reported polities BEFORE resolving the year: the caller
    # named these codes, so a same-polity tie must never discard one of them in
    # favour of an interval nobody reported.
    reported <- polities$polity_code %in% dy$polity_code
    src <- .active_polities(
      polities[reported, ],
      y,
      open_ended = open_ended[reported]
    )

    if (nrow(src) == 0) {
      if (verbose) {
        cli::cli_warn(
          "Year {y}: no source polity with a polygon is active for the reported data; skipped."
        )
      }
      next
    }
    # report data polities that cannot be placed (no active polygon this year)
    unplaced <- setdiff(unique(dy$polity_code), src$polity_code)
    if (length(unplaced) && verbose) {
      cli::cli_warn(
        "Year {y}: {length(unplaced)} reported polit{?y/ies} have no active polygon and are dropped: {.val {unplaced}}."
      )
    }

    vmap <- tapply(dy$value, dy$polity_code, sum, na.rm = TRUE)

    # ---- grid the region covering sources + targets ----
    region <- sf::st_as_sfc(sf::st_bbox(
      sf::st_union(c(sf::st_geometry(src), sf::st_geometry(target)))
    ))
    # guard: a far-flung source/target (e.g. a continent-spanning target) would
    # blow the bounding box up into a multi-million-cell grid and hang. Fail
    # fast with actionable advice instead.
    n_est <- as.numeric(sf::st_area(region)) / (resolution^2)
    if (n_est > max_cells) {
      cli::cli_abort(c(
        "Year {y}: the source/target extent implies ~{prettyNum(round(n_est), big.mark = ',')} grid cells (> `max_cells` = {prettyNum(max_cells, big.mark = ',')}).",
        "i" = "Restrict `polities`/`data` to the relevant region, use a coarser `resolution`, or raise `max_cells`."
      ))
    }
    cells <- sf::st_make_grid(region, cellsize = resolution, what = "polygons")
    cells <- sf::st_sf(cell_id = seq_along(cells), geometry = cells)
    centroids <- sf::st_sf(
      cell_id = cells$cell_id,
      geometry = sf::st_centroid(sf::st_geometry(cells))
    )
    base <- data.frame(
      cell_id = cells$cell_id,
      cell_area = as.numeric(sf::st_area(cells))
    )

    # covariate density per cell (uniform if NULL)
    if (is.null(covariate)) {
      dens <- rep(1, nrow(base))
    } else {
      dens <- as.numeric(covariate(centroids, y))
      if (length(dens) != nrow(base)) {
        cli::cli_abort(
          "`covariate` returned {length(dens)} values; expected {nrow(base)}."
        )
      }
      dens[is.na(dens) | dens < 0] <- 0
    }
    base$w <- base$cell_area * dens

    # assign each cell to a source polity (year y) and a target polity (ref_year)
    base$src <- .assign_polity(centroids, src)
    base$tgt <- .assign_polity(centroids, target)

    # ---- source intensities: value per unit covariate over each source's extent ----
    has_data <- !is.na(base$src) & base$src %in% names(vmap)
    denom <- tapply(base$w[has_data], base$src[has_data], sum)
    # a source with data but zero gridded weight (too small for the grid, or no
    # covariate density over its extent) cannot be placed. Keep it out of the
    # intensity map, the covered set and the donor pool so its value is neither
    # smeared over gap cells nor silently lost; report it as `unallocated`.
    placeable <- names(denom)[denom > 0]
    starved <- setdiff(names(vmap), placeable)
    unallocated <- sum(vmap[starved], na.rm = TRUE)
    if (length(starved) && verbose) {
      cli::cli_warn(
        "Year {y}: {length(starved)} source{?s} smaller than the grid resolution; refine `resolution` to capture {.val {starved}}."
      )
    }
    intensity <- vmap[placeable] / denom[placeable] # per placeable source
    has_data <- has_data & base$src %in% placeable
    base$e <- ifelse(has_data, base$w * intensity[base$src], NA_real_)

    # ---- donor intensity for uncovered target cells ----
    tot_value <- sum(vmap[placeable], na.rm = TRUE) # value actually distributed
    tot_w_data <- sum(base$w[has_data], na.rm = TRUE)
    i_donor <- if (donor == "regional" && tot_w_data > 0) {
      tot_value / tot_w_data
    } else {
      0
    }

    # ---- re-aggregate to target boundaries ----
    tcells <- base[!is.na(base$tgt), ]
    if (nrow(tcells) == 0) {
      next
    }
    agg <- lapply(split(tcells, tcells$tgt), function(g) {
      w_total <- sum(g$w, na.rm = TRUE)
      covered <- sum(g$e, na.rm = TRUE)
      gap_w <- sum(g$w[is.na(g$e)], na.rm = TRUE)
      imputed <- gap_w * i_donor
      data.frame(
        target_polity_code = g$tgt[1],
        value = covered + imputed,
        covered = covered,
        imputed = imputed,
        imputed_share = if (w_total > 0) gap_w / w_total else NA_real_
      )
    })
    df <- do.call(rbind, agg)
    df$year <- y
    df$n_sources <- length(placeable)
    df$unallocated <- unallocated
    results[[k]] <- df

    if (verbose) {
      cli::cli_progress_step(
        "Year {y}: {nrow(df)} target{?s}, {length(placeable)} source{?s}, mean imputed_share {round(mean(df$imputed_share, na.rm=TRUE), 3)}",
        .auto_close = TRUE
      )
    }
  }

  out <- do.call(rbind, results)
  if (is.null(out)) {
    return(tibble::tibble(
      target_polity_code = character(),
      year = integer(),
      value = double(),
      covered = double(),
      imputed = double(),
      imputed_share = double(),
      n_sources = integer(),
      unallocated = double()
    ))
  }
  rownames(out) <- NULL
  tibble::as_tibble(out[, c(
    "target_polity_code",
    "year",
    "value",
    "covered",
    "imputed",
    "imputed_share",
    "n_sources",
    "unallocated"
  )])
}
# nolint end

# The epoch-independent part of a polity code: "RUS-1991-2014" -> "RUS",
# "AZE-SSR-1920-1991" -> "AZE-SSR". Only the trailing year pair is stripped, and
# no date is ever read from here: `start_year`/`end_year` are authoritative
# because 2 of the 749 codes disagree with their own columns (`NNG-1949-1963`
# ends in 1969, `TAN-1922-1964` in 1961).
.polity_family <- function(polity_code) {
  stringr::str_remove(polity_code, "-\\d+-\\d+$")
}

# THE CONVENTION (DA-24), stated once here and referred to from every other
# resolver: `start_year` is inclusive; `end_year` is EXCLUSIVE at a succession
# and INCLUSIVE at the open end.
#
# Exclusive at a succession, because 2014 belongs to "RUS-2014-2025" and never
# to "RUS-1991-2014". An inclusive end bound makes both epochs active on every
# boundary year, and `.assign_polity()` then hands every cell to whichever
# sorts first -- always the dissolved predecessor -- so the successor receives
# no row at all.
#
# Inclusive at the open end, because an interval nothing succeeds ends where
# the table's coverage ends, not at a boundary between two epochs: no
# double-count is possible there, and a uniformly exclusive read would make the
# current year unrepresentable (all 229 open intervals end in 2025, so 2024
# would be the last covered year).
.open_ended_intervals <- function(start_year, end_year, group) {
  open <- rep(FALSE, length(end_year))
  if (length(end_year) == 0L || all(is.na(end_year))) {
    return(open)
  }
  # Both conditions come from the data, never from a hardcoded year, so the
  # rule follows upstream when the intervals are extended. Only intervals that
  # REACH the domain end can succeed one another there, so a sibling starting
  # later but ending earlier is not a successor and must not punch a one-year
  # hole in a polity that is otherwise continuous.
  at_end <- which(!is.na(end_year) & end_year == max(end_year, na.rm = TRUE))
  keys <- as.character(group)[at_end]
  # A row whose group is unknown keeps the strict exclusive read.
  at_end <- at_end[!is.na(keys)]
  keys <- keys[!is.na(keys)]
  if (length(at_end) == 0L) {
    return(open)
  }
  # The successor condition is load-bearing: 8 polities in the shipped 749-row
  # table carry two intervals ending on the domain end (`AGO-1816-2025` beside
  # `AGO-1975-2025`; also ARG, BLZ, BRA, CAN, GRC, IRQ, ROU), so 237 intervals
  # end there but only 229 are open. A bare "end_year is the maximum" test would
  # open BOTH of each pair and count the terminal year twice. It is also the
  # test that keeps agreeing with upstream's own `successor` column as the
  # horizon moves. With no group repeated, every interval
  # here is its own group maximum, so the branch below is exactly the general
  # case and skips a group-wise pass that costs ~1.3 s on a 70k-row grid.
  if (anyDuplicated(keys) == 0L) {
    open[at_end] <- TRUE
    return(open)
  }
  starts <- start_year[at_end]
  starts[is.na(starts)] <- -Inf
  open[at_end] <- starts >= tapply(starts, keys, max)[keys]
  open
}

# TRUE where an interval covers `yr` under the convention above. `group` is
# evaluated lazily and only touched on a terminal year, so callers may pass an
# expression that is expensive to build. `open_ended` short-circuits that
# derivation with a flag computed elsewhere.
.covers_year <- function(start_year, end_year, group, yr, open_ended = NULL) {
  covered <- start_year <= yr & yr < end_year
  at_end <- !is.na(end_year) & end_year == yr
  if (!any(at_end)) {
    return(covered)
  }
  if (is.null(open_ended)) {
    open_ended <- .open_ended_intervals(start_year, end_year, group)
  }
  covered | (start_year <= yr & at_end & open_ended)
}

# Rows of `polities` valid in `yr` under the convention above. `open_ended` is
# the open-end flag of the FULL table, aligned to `polities` rows; pass it
# whenever `polities` is a subset, because whether an interval is succeeded is
# a property of the whole table and a subset can hide the successor. Left NULL,
# it is derived from `polities` itself.
# Where the table still carries overlapping intervals for one polity, keep a
# single interval per polity, tie-broken on the latest start exactly as
# `.add_polity_columns_dt()` does in `R/polities.R`. The dedupe is not
# hypothetical housekeeping: `get_polity_geometries()` returns every row
# regardless of `wiki_status`, and on the shipped 749-row table 703 rows carry a
# polygon and 2,134 polity-years across 23 families have two intervals of one
# polity active at once (`GRC-1830-1913` alongside `GRC-1881-1913`,
# `PER-1825-1909` alongside `PER-1825-1884`). Without the dedupe both come back
# and `.assign_polity()` keeps whichever sorts first, which is always the
# dissolved predecessor because `X-a-b` precedes `X-b-c` lexically; the
# successor then gets no row at all and every cell outside the predecessor's
# smaller extent is dropped, so mass is lost and not merely relabelled.
# Restricting to live, non-aggregate rows leaves 0 such polity-years on this
# vintage (it was 86 across MNE and PER on the 740-row one), so the overlap is
# now entirely between a live interval and a retired or superseded one.
.active_polities <- function(polities, yr, open_ended = NULL) {
  covers <- .covers_year(
    polities$start_year,
    polities$end_year,
    .polity_family(polities$polity_code),
    yr,
    open_ended = open_ended
  )
  active <- polities[which(covers), ]
  if (nrow(active) < 2L) {
    return(active)
  }
  family <- .polity_family(active$polity_code)
  # `exact_start` is redundant under the filter above -- `start_year <= yr`
  # makes an exact start the maximum start -- and is kept because it states the
  # boundary-year rule the tests assert, not as live logic. `R/polities.R`
  # dropped its own copy for the same reason (there it was not merely redundant
  # but unreachable, the non-equi join having overwritten `join_start_year`), so
  # the surviving decision on both paths is the latest start.
  exact_start <- !is.na(active$start_year) & active$start_year == yr
  ranked <- order(family, !exact_start, -active$start_year)
  active[ranked, ][!duplicated(family[ranked]), ]
}

# Assign each centroid to the polity whose polygon contains it. Returns a
# character vector aligned to `centroids` order (NA where no polygon, first
# match where polygons overlap). The code is coerced to character so that all
# downstream keying is by name, never by numeric position (see issue #209).
.assign_polity <- function(centroids, polys) {
  centroids$.cid <- seq_len(nrow(centroids))
  j <- sf::st_join(
    centroids[, ".cid"],
    polys["polity_code"],
    join = sf::st_within,
    left = TRUE
  )
  j <- sf::st_drop_geometry(j)
  j <- j[!duplicated(j$.cid), ] # keep first match on overlap
  j <- j[order(j$.cid), ]
  as.character(j$polity_code)
}
