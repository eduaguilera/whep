# S-A6 acceptance note -- what polycell keying does and does not deliver.
#
# ACHIEVED in the spatialization engines. Every ALLOCATION is keyed on the
# polycell: the crop engine forms its share denominators over
# `(area_code, item_prod_code)` and the livestock engine over `area_code`, so a
# national total is spread only across cells that polity actually holds, and a
# cell shared by two polities delivers to each only what its own compartment
# carries. Capacity, redistribution and CFT aggregation group on
# `.compartment_cell_cols()`, and `.compartment_join_cols()` now ABORTS rather
# than silently falling back to a `(lon, lat)` join, which across a polycell
# grid is many-to-many.
#
# IRREDUCIBLE, and stated rather than papered over. Every DRIVER layer -- LUH2
# cropland, LUH2 pasture/rangeland, GLW3 density, grass NPP, the manure
# reference pattern -- exists only per physical cell, so it is joined on
# `(lon, lat)` and split by `cell_area_frac`. Two polycells in one cell
# therefore share one cropland density, one grass productivity and one GLW3
# density. This is the driver-resolution limit the unit was never going to fix
# (plan "What the polycell does not fix"); it is a resolution loss, NOT
# cross-border contamination, because the share denominator is per polity.
# Q-S2's GLW3 case is the literal instance: `density * cell_area_frac` splits a
# cell's heads pro rata, and pro rata is the only estimator available at 0.5
# degrees. Do not read S-A6 as closing this.
#
# STILL OPEN, deliberately: the capacity ceiling is soft. See
# `.warn_capacity_breach()`.

#' Names that carry the polity's share of a physical cell.
#'
#' A partition of one cell across the polities that overlap it, summing to 1.
#' `cell_area_frac` is canonical; the others are what live producers already
#' write for the SAME quantity (`build_cell_polity()` writes `polity_frac`).
#' Precedence is the listed order, so an explicit `cell_area_frac` wins.
#' @noRd
.polity_share_cols <- function() {
  c("cell_area_frac", "polity_frac", "area_frac", "country_frac")
}

#' Names that look like a cell fraction but are a different quantity.
#'
#' `landfrac` was in the alias list until C8. It is the LPJmL/GADM **land**
#' fraction of a cell: one value per cell, identical for every polity in it,
#' and not a partition -- read as a share it hands each polity of a border cell
#' the same fraction, so the cell is delivered once per polity instead of once.
#' Refusing these is the other half of S-A5: aborting on a *missing* share is
#' not enough if a different quantity can still be silently reinterpreted.
#' @noRd
.non_share_frac_cols <- function() {
  c("landfrac", "land_frac", "land_fraction", "cell_land_frac", "icwtr")
}

#' Normalise a country grid for polity-compartment spatialization.
#'
#' S-A5. A grid with no polity share is REFUSED. It used to default to
#' `cell_area_frac = 1`, which is exactly how the centroid crosswalk
#' (`lon`, `lat`, `area_code` only) gave a border cell wholly to one polity
#' while every conservation check still passed (EA4).
#' @noRd
.normalize_country_grid <- function(country_grid, arg = "country_grid") {
  .check_columns(country_grid, c("lon", "lat", "area_code"), arg)

  country_grid <- tibble::as_tibble(country_grid)
  frac_col <- intersect(.polity_share_cols(), names(country_grid))
  if (length(frac_col) == 0L) {
    .abort_missing_polity_share(country_grid, arg)
  }
  if (frac_col[[1L]] != "cell_area_frac") {
    country_grid <- dplyr::mutate(
      country_grid,
      cell_area_frac = .data[[frac_col[[1L]]]]
    )
  }

  country_grid |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      cell_area_frac = as.numeric(cell_area_frac)
    ) |>
    .check_polity_share(frac_col[[1L]], arg)
}

#' Refuse a grid that carries no polity share (S-A5).
#' @noRd
.abort_missing_polity_share <- function(country_grid, arg) {
  wrong <- intersect(.non_share_frac_cols(), names(country_grid))
  accepted <- .polity_share_cols()
  detail <- if (length(wrong) > 0L) {
    "{.field {wrong}} {?is/are} present but {?is/are} not a polity share: a
     land fraction is one value per CELL, identical for every polity in it, so
     it does not partition the cell between them."
  } else {
    "None of {.field {accepted}} is present."
  }
  cli::cli_abort(c(
    "{.arg {arg}} carries no polity share, so an allocation on it would be
     keyed on the physical cell rather than on the polycell.",
    x = detail,
    i = "A centroid-resolved crosswalk defaulted to {.code cell_area_frac = 1}
         and gave a border cell WHOLLY to one polity, silently. Supply an
         explicit share; use {.code 1} only where the polity owns the whole
         cell."
  ))
}

#' Refuse a share this function cannot interpret as a share.
#'
#' `NA` used to become 1 and out-of-range values were clamped, both silent. The
#' range test also catches an absolute area substituted for a normalised
#' weight, which is the substitution C5 existed to detect.
#' @noRd
.check_polity_share <- function(country_grid, source_col, arg) {
  share <- country_grid$cell_area_frac
  tol <- 1e-8
  n_na <- sum(is.na(share))
  if (n_na > 0L) {
    cli::cli_abort(c(
      "{.arg {arg}} has {n_na} row{?s} whose {.field {source_col}} is
       {.val {NA}}.",
      x = "A missing share used to become 1, handing the whole cell to that
           polity.",
      i = "Emit the measured share, or drop the row upstream where the loss
           can be reported."
    ))
  }
  bad <- which(share < -tol | share > 1 + tol)
  if (length(bad) > 0L) {
    rng <- range(share[bad])
    cli::cli_abort(c(
      "{.arg {arg}} has {length(bad)} row{?s} whose {.field {source_col}} is
       outside {.code [0, 1]} (range {.val {rng}}).",
      x = "A polity share is a fraction of one cell; values above 1 are the
           signature of an absolute area used as a weight.",
      i = "Divide by the cell's own total before passing it in."
    ))
  }
  # Assigned directly rather than through `mutate()`: `share` is a local, and a
  # `country_grid` that happened to carry a column of the same name would win
  # the data mask.
  country_grid$cell_area_frac <- pmin(pmax(share, 0), 1)
  country_grid
}

#' Columns that identify a polity compartment within a physical cell.
#' @noRd
.compartment_id_cols <- function(data) {
  intersect(c("polycell_id", "cell_id", "area_code"), names(data))
}

#' Join/grouping columns for a compartment-resolved cell.
#' @noRd
.compartment_cell_cols <- function(data) {
  unique(c(.compartment_id_cols(data), "lon", "lat"))
}

#' Compartment join key, checked against the table being joined to (S-A6).
#'
#' `intersect()` alone silently degrades to `(lon, lat)` when the other side
#' has lost a compartment id, and `(lon, lat)` across a polycell grid is
#' many-to-many: one polity's capacity would be applied to its neighbour's
#' allocation, and the row count would grow, with no warning.
#' @noRd
.compartment_join_cols <- function(x, y, x_arg, y_arg) {
  cols <- .compartment_cell_cols(x)
  missing <- setdiff(cols, names(y))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.arg {y_arg}} cannot be joined to {.arg {x_arg}} on the polycell.",
      x = "Missing compartment key column{?s}: {.field {missing}}.",
      i = "Joining on {.field lon}/{.field lat} alone is many-to-many across a
           polycell grid and would mix polities inside one cell."
    ))
  }
  cols
}

#' Detect whether a country grid changes through time.
#' @noRd
.country_grid_is_dynamic <- function(country_grid) {
  any(
    c(
      "year",
      "valid_from",
      "valid_to",
      "start_year",
      "end_year",
      "from_year",
      "to_year"
    ) %in%
      names(country_grid)
  )
}

#' Select the polity-cell rows valid for one simulation year.
#' @noRd
.filter_country_grid_year <- function(country_grid, yr) {
  if ("year" %in% names(country_grid)) {
    return(dplyr::filter(country_grid, .data$year == yr))
  }

  start_col <- intersect(
    c("valid_from", "start_year", "from_year"),
    names(country_grid)
  )
  end_col <- intersect(
    c("valid_to", "end_year", "to_year"),
    names(country_grid)
  )

  if (length(start_col) > 0L || length(end_col) > 0L) {
    start_vals <- if (length(start_col) > 0L) {
      country_grid[[start_col[[1L]]]]
    } else {
      -Inf
    }
    end_vals <- if (length(end_col) > 0L) {
      country_grid[[end_col[[1L]]]]
    } else {
      Inf
    }
    start_vals[is.na(start_vals)] <- -Inf
    end_vals[is.na(end_vals)] <- Inf
    # A missing bound arrives as a scalar; make both full length so the
    # open-end test below is never handed a vector shorter than its grouping.
    start_vals <- rep_len(start_vals, nrow(country_grid))
    end_vals <- rep_len(end_vals, nrow(country_grid))
    # The start bound is inclusive; the end bound is EXCLUSIVE at a succession
    # and INCLUSIVE at the open end, matching the `polities` convention stated
    # in full above `.open_ended_intervals()` in `R/constant_territory.R`. 2014
    # selects "RUS-2014-2025", not "RUS-1991-2014" -- an inclusive end bound
    # would return both epochs on every boundary year and double-count the cell
    # -- while 2025 still selects "RUS-2014-2025", because nothing succeeds it.
    covers <- .covers_year(
      start_vals,
      end_vals,
      .compartment_interval_groups(country_grid),
      yr
    )
    return(country_grid[which(covers), , drop = FALSE])
  }

  country_grid
}

#' Group key identifying successive intervals of one polity compartment.
#'
#' Built from the physical cell and the reporting area, deliberately NOT from
#' `.compartment_id_cols()`: DA-2 makes `polycell_id` a function of the polity
#' code, so it changes at every succession and would file an interval and its
#' own successor under different keys, leaving the open-end test blind.
#' @noRd
.compartment_interval_groups <- function(country_grid) {
  cols <- intersect(c("lon", "lat", "area_code"), names(country_grid))
  if (length(cols) == 0L) {
    return(rep("", nrow(country_grid)))
  }
  keys <- lapply(cols, \(cl) country_grid[[cl]])
  do.call(paste, c(keys, list(sep = "\r")))
}

#' Build the static crop-pattern by country-compartment table.
#' @noRd
.build_base_grid_cp <- function(
  country_grid,
  crop_patterns,
  type_lookup = NULL
) {
  cg_dt <- data.table::as.data.table(country_grid)
  cp_dt <- data.table::as.data.table(crop_patterns)
  base_grid_cp <- cg_dt[cp_dt, on = .(lon, lat), allow.cartesian = TRUE]
  base_grid_cp <- .drop_uncompartmented_cells(base_grid_cp)
  base_grid_cp[,
    harvest_fraction := data.table::fifelse(
      is.na(harvest_fraction),
      0,
      harvest_fraction
    )
  ]
  if (!is.null(type_lookup)) {
    tlu_dt <- data.table::as.data.table(type_lookup)
    base_grid_cp[tlu_dt, luh2_type := i.luh2_type, on = .(item_prod_code)]
  }
  data.table::setkey(base_grid_cp, lon, lat)
  base_grid_cp
}

#' Drop pattern cells that no polity compartment claims (S-A6).
#'
#' `.build_base_grid_cp()` keeps every `crop_patterns` cell, so a cell absent
#' from `country_grid` arrives with `area_code` and `cell_area_frac` both `NA`.
#' The `NA` share used to be replaced by 1 -- a whole-cell allocation to a
#' polity that does not exist, reachable whenever `country_areas` itself
#' carries an `NA` `area_code`. Unkeyable rows are dropped and counted instead.
#' A keyed row with an `NA` share cannot occur after `.check_polity_share()`,
#' so it is an invariant breach, not a coverage gap.
#' @noRd
.drop_uncompartmented_cells <- function(base_grid_cp) {
  unkeyed <- is.na(base_grid_cp$area_code)
  if (any(is.na(base_grid_cp$cell_area_frac) & !unkeyed)) {
    cli::cli_abort(
      "A keyed compartment has an {.val {NA}} {.field cell_area_frac}."
    )
  }
  if (!any(unkeyed)) {
    return(base_grid_cp)
  }
  n_cells <- nrow(unique(base_grid_cp[unkeyed, list(lon, lat)]))
  cli::cli_warn(c(
    "{n_cells} {.arg crop_patterns} cell{?s} are in no polity compartment and
     carry no allocation.",
    i = "They used to be given a whole-cell share with an {.val {NA}}
         {.field area_code}."
  ))
  base_grid_cp[!unkeyed]
}
