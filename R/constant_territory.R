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
#'   year (`start_year <= ref_year <= end_year`).
#' @param polities An `sf` of polity polygons with `polity_code`, `start_year`,
#'   `end_year` and geometry. Defaults to [get_polity_geometries()].
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
#'
#' @examples
#' \dontrun{
#' # Present-day Austria's wheat, back through the Austria-Hungary years,
#' # weighted by gridded cropland:
#' series <- build_constant_territory_series(
#'   data = reported_wheat,            # year, polity_code, value
#'   ref_year = 2020,
#'   covariate = cropland_density_fn   # samples LUH cropland at cell centroids
#' )
#' }
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
    cli::cli_abort("`covariate` must be NULL or a function(centroids_sf, year).")
  }

  if (is.null(polities)) {
    polities <- get_polity_geometries()
  }
  polities <- polities[, c("polity_code", "start_year", "end_year")]
  # only polities that actually carry a polygon can host or receive mass
  polities <- polities[!sf::st_is_empty(polities), ]
  polities <- sf::st_make_valid(sf::st_transform(polities, crs_equal_area))

  .active <- function(yr) {
    polities[polities$start_year <= yr & polities$end_year >= yr, ]
  }

  target <- .active(ref_year)
  if (nrow(target) == 0) {
    cli::cli_abort("No polities with a polygon are active in `ref_year` = {ref_year}.")
  }

  data <- data[!is.na(data$value), required]
  years <- sort(unique(data$year))
  results <- vector("list", length(years))

  for (k in seq_along(years)) {
    y <- years[k]
    dy <- data[data$year == y, ]
    src <- .active(y)
    src <- src[src$polity_code %in% dy$polity_code, ]

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
        cli::cli_abort("`covariate` returned {length(dens)} values; expected {nrow(base)}.")
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
    # a source with data but zero gridded weight (too small for the grid) is lost
    starved <- setdiff(names(vmap), names(denom[denom > 0]))
    if (length(starved) && verbose) {
      cli::cli_warn(
        "Year {y}: {length(starved)} source{?s} smaller than the grid resolution; refine `resolution` to capture {.val {starved}}."
      )
    }
    intensity <- vmap[names(denom)] / denom            # per source polity
    base$e <- ifelse(has_data, base$w * intensity[base$src], NA_real_)

    # ---- donor intensity for uncovered target cells ----
    tot_value <- sum(vmap[names(denom)], na.rm = TRUE)  # value actually distributed
    tot_w_data <- sum(base$w[has_data], na.rm = TRUE)
    i_donor <- if (donor == "regional" && tot_w_data > 0) tot_value / tot_w_data else 0

    # ---- re-aggregate to target boundaries ----
    tcells <- base[!is.na(base$tgt), ]
    if (nrow(tcells) == 0) next
    agg <- lapply(split(tcells, tcells$tgt), function(g) {
      W <- sum(g$w, na.rm = TRUE)
      covered <- sum(g$e, na.rm = TRUE)
      gap_w <- sum(g$w[is.na(g$e)], na.rm = TRUE)
      imputed <- gap_w * i_donor
      data.frame(
        target_polity_code = g$tgt[1],
        value = covered + imputed,
        covered = covered,
        imputed = imputed,
        imputed_share = if (W > 0) gap_w / W else NA_real_
      )
    })
    df <- do.call(rbind, agg)
    df$year <- y
    df$n_sources <- length(denom[denom > 0])
    results[[k]] <- df

    if (verbose) {
      cli::cli_progress_step(
        "Year {y}: {nrow(df)} target{?s}, {length(denom[denom>0])} source{?s}, mean imputed_share {round(mean(df$imputed_share, na.rm=TRUE), 3)}",
        .auto_close = TRUE
      )
    }
  }

  out <- do.call(rbind, results)
  if (is.null(out)) {
    return(tibble::tibble(
      target_polity_code = character(), year = integer(), value = double(),
      covered = double(), imputed = double(), imputed_share = double(),
      n_sources = integer()
    ))
  }
  rownames(out) <- NULL
  tibble::as_tibble(out[, c(
    "target_polity_code", "year", "value", "covered", "imputed",
    "imputed_share", "n_sources"
  )])
}

# Assign each centroid to the polity whose polygon contains it. Returns a
# character vector aligned to `centroids` order (NA where no polygon, first
# match where polygons overlap).
.assign_polity <- function(centroids, polys) {
  centroids$.cid <- seq_len(nrow(centroids))
  j <- sf::st_join(
    centroids[, ".cid"],
    polys["polity_code"],
    join = sf::st_within,
    left = TRUE
  )
  j <- sf::st_drop_geometry(j)
  j <- j[!duplicated(j$.cid), ]            # keep first match on overlap
  j <- j[order(j$.cid), ]
  j$polity_code
}
