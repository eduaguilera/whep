# Floor the LPJmL land-use fractions at float32 resolution (whep#985).
#
# The landuse NetCDF that `inst/scripts/prepare_spatialize_all.R` writes is
# float32 (`prec = "float"`). Its values are per-cell band fractions built in
# double precision from `build_gridded_landuse()` allocations, and a filter of
# `value > 0` there is not a tolerance: anything that underflows toward zero
# without reaching it survives, and the cast to float32 then carries it into
# the denormal range (the pre-whep#1088 file's smallest entry was 1.401e-45,
# `FLT_TRUE_MIN`). whep#1088 removed the denormals that came from EarthStat's
# own pattern residue, but not the tail of the allocation itself, which also
# comes from tiny LUH2 `cropland_ha`, tiny national areas and the logit
# capacity redistribution.
#
# The floor is taken from float32 itself, not chosen:
#
# * `FLT_EPSILON = 2^-23` (1.1920929e-07) is the spacing of float32 values
#   relative to their magnitude (24 significand bits). A band fraction below
#   `cell_total * FLT_EPSILON` lies past the last significant bit of the
#   cell's own land-use total: added to that total in float32 it cannot change
#   it, so the file cannot tell it apart from zero at the scale of the cell it
#   describes. This mirrors the `max * FLT_EPSILON` argument
#   `.crop_pattern_signal_floor()` makes for the EarthStat rasters.
# * `FLT_MIN = 2^-126` (1.1754944e-38) is the smallest NORMAL float32. Below
#   it a value is denormal: it has lost significand bits and is slow on some
#   hardware. This catches a cell whose whole total is so small that the
#   relative floor alone would keep a denormal.
#
# Dropped mass is folded back into the same cell's surviving bands, rescaled
# so the cell's total (in double precision, before the float32 cast) is
# exactly what it was. The rescale factor differs from 1 by less than
# `n_bands * FLT_EPSILON`, i.e. by at most a few float32 ULPs. A cell with no
# band left -- every band below `FLT_MIN` -- is dropped outright; its lost
# area is below `32 * FLT_MIN` of a cell, some 1e-31 ha.
#
# What this floor deliberately does NOT do is the hectare floor whep#985 also
# proposes (drop allocations below 1 ha, ~4e-6 of a cell). That decides what
# counts as a real crop stand in LPJmL, has defensible alternatives, and is
# left to the maintainer.

#' Float32 constants the land-use floor is derived from.
#' @noRd
.float32_epsilon <- function() {
  2^-23
}

#' @noRd
.float32_min_normal <- function() {
  2^-126
}

#' Drop land-use fractions that float32 cannot resolve, conserving each cell.
#'
#' @param lu A data.table with one row per band fraction: key columns named
#'   in `cell_cols` and a double `value` column.
#' @param method `"float32_resolution"` (default) drops fractions below
#'   `cell_total * FLT_EPSILON` or below `FLT_MIN`; `"denormal"` drops only
#'   those below `FLT_MIN`; `"none"` returns `lu` unchanged.
#' @param cell_cols Columns identifying one cell's land-use vector.
#' @return A data.table with the same columns, fewer rows, and every cell's
#'   `sum(value)` unchanged to double precision.
#' @noRd
.floor_landuse_fractions <- function(
  lu,
  method = c("float32_resolution", "denormal", "none"),
  cell_cols = c("year", "row", "col")
) {
  method <- rlang::arg_match(method)
  .check_landuse_floor_input(lu, cell_cols)
  if (method == "none" || nrow(lu) == 0L) {
    return(lu)
  }
  dt <- data.table::copy(data.table::as.data.table(lu))
  dt[, cell_total_ := sum(value), by = cell_cols]
  rel <- if (method == "float32_resolution") .float32_epsilon() else 0
  dt[,
    keep_ := value >= .float32_min_normal() & value >= cell_total_ * rel
  ]
  dt[, kept_total_ := sum(value[keep_]), by = cell_cols]
  out <- dt[keep_ == TRUE]
  out[, value := value * (cell_total_ / kept_total_)]
  out[, c("cell_total_", "keep_", "kept_total_") := NULL]
  out[]
}

#' @noRd
.check_landuse_floor_input <- function(lu, cell_cols) {
  missing <- setdiff(c(cell_cols, "value"), names(lu))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg lu} is missing column{?s} {.field {missing}}.",
      class = "whep_landuse_floor_columns"
    )
  }
  if (any(!is.finite(lu$value)) || any(lu$value < 0)) {
    cli::cli_abort(
      "Column {.field value} of {.arg lu} must be finite and non-negative.",
      class = "whep_landuse_floor_values"
    )
  }
  invisible(lu)
}
