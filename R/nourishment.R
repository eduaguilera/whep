# Nourishment normalization for the SJOS-N "just" axis (Module 3, Task 3.3).
# Maps a per-capita nourishment supply onto the Global Intake_normalization
# score and classifies it Under / Adequate / Over. Protein is the SJOS-N
# nourishment axis (default value_col = protein_g_cap_day, thresholds from
# whep::nourishment_thresholds: floor 62.1, ceiling 85.05 g/cap/day).
#
# PROTEIN AND ENERGY ARE NOT INTERCHANGEABLE HERE, and this file used to imply
# they were. The arithmetic is shared, the bases are not:
#
#   - The energy bounds (2300 / 2900 kcal/cap/day) carry no source at all. They
#     are labelled `inherited_unsourced` in the shipped table, as are the
#     protein ceiling (63) and the 1.35 factor behind both protein bounds. Only
#     the protein floor is cited (whep#753).
#   - WHEP's own energy column is GROSS combustion energy while a dietary
#     kcal/cap/day threshold is metabolisable energy, so passing
#     `energy_kcal_cap_day` from build_food_supply() through these bounds
#     compares two different quantities.
#
# So the energy path is available for a caller who supplies their own
# metabolisable series and their own bounds; it is not a second axis WHEP
# publishes, and nothing in the package uses it.

#' Normalize and classify per-capita nourishment.
#'
#' @description
#' Adds a piecewise-normalized nourishment score `value_norm` and its
#' Under / Adequate / Over class `nourish`. The normalization (the Global
#' `Intake_normalization`) is `value / floor` below the floor, `1 + value /
#' ceiling` above the ceiling and `1 + (value - floor) / (ceiling - floor)` in
#' between, so the score is below 1 under the floor, exactly 1 at the floor,
#' between 1 and 2 across the adequate band and 2 or above at or past the
#' ceiling. The class is `"Under"` when `value_norm < 1`, `"Adequate"` when
#' `value_norm < 2` and `"Over"` otherwise. Protein is the SJOS-N nourishment
#' axis, so the defaults are the protein floor and ceiling (62.1 and 85.05
#' g/cap/day) from [nourishment_thresholds].
#'
#' Of those two defaults only the underlying 46 g/cap/day floor is sourced
#' (WHO/FAO/UNU TRS 935 Table 46, the safe intake of a 55 kg adult, itself a
#' 97.5th-percentile individual level rather than a population one). The 63
#' ceiling and the 1.35 factor that lifts both to a supply basis carry no
#' source; `nourishment_thresholds$provenance` says so per row.
#' [build_nourishment_band()] is the sourced replacement for both bounds and is
#' not wired in here yet.
#'
#' Passing `value_col = energy_kcal_cap_day` runs the same arithmetic on a
#' different quantity and is **not** a second WHEP axis: the packaged energy
#' bounds are unsourced, and WHEP's energy column is gross combustion energy
#' where a dietary threshold is metabolisable. Supply your own bounds and your
#' own metabolisable series if you want that comparison.
#'
#' @param x A tibble carrying the per-capita nourishment column named by
#'   `value_col` (for example a [build_food_supply()] output).
#' @param value_col The unquoted nourishment column to normalize. Defaults to
#'   `protein_g_cap_day`.
#' @param thresholds Optional named `floor` and `ceiling` (a named numeric
#'   vector or list). When `NULL` (default) the protein floor and ceiling from
#'   [nourishment_thresholds] are used.
#' @return `x` with `value_norm` (numeric score) and `nourish` (`"Under"`,
#'   `"Adequate"` or `"Over"`) added.
#' @export
#' @examples
#' normalize_nourishment(
#'   tibble::tribble(
#'     ~area_code, ~protein_g_cap_day,
#'     10L, 30,
#'     20L, 70,
#'     30L, 100
#'   )
#' )
normalize_nourishment <- function(
  x,
  value_col = protein_g_cap_day,
  thresholds = NULL
) {
  bounds <- .nourish_bounds(thresholds)
  x |>
    dplyr::mutate(
      value_norm = .nourish_normalize(
        {{ value_col }},
        bounds$floor,
        bounds$ceiling
      ),
      nourish = .nourish_classify(.data$value_norm)
    )
}

# ---- Private helpers -------------------------------------------------------

# Resolve the floor / ceiling, defaulting to the protein bounds from
# whep::nourishment_thresholds when the caller supplies none.
.nourish_bounds <- function(thresholds) {
  thresholds <- thresholds %||% .nourish_protein_bounds()
  if (
    !rlang::has_name(thresholds, "floor") ||
      !rlang::has_name(thresholds, "ceiling")
  ) {
    cli::cli_abort(c(
      "{.arg thresholds} must be named with {.field floor} and
       {.field ceiling}.",
      "i" = "Received name{?s}: {.val {names(thresholds)}}."
    ))
  }
  list(floor = thresholds[["floor"]], ceiling = thresholds[["ceiling"]])
}

# The default protein floor (62.1) and ceiling (85.05) g/cap/day, read from the
# packaged nourishment thresholds.
.nourish_protein_bounds <- function() {
  nt <- whep::nourishment_thresholds
  floor_val <- nt |>
    dplyr::filter(.data$metric == "protein", .data$bound == "floor") |>
    dplyr::pull(.data$value)
  ceiling_val <- nt |>
    dplyr::filter(.data$metric == "protein", .data$bound == "ceiling") |>
    dplyr::pull(.data$value)
  c(floor = floor_val, ceiling = ceiling_val)
}

# Piecewise Intake_normalization: below the floor scales toward 1, the adequate
# band maps linearly onto [1, 2], and above the ceiling grows past 2.
.nourish_normalize <- function(value, floor_val, ceiling_val) {
  dplyr::case_when(
    value < floor_val ~ value / floor_val,
    value > ceiling_val ~ 1 + value / ceiling_val,
    .default = 1 + (value - floor_val) / (ceiling_val - floor_val)
  )
}

# Under below a score of 1, Adequate below 2, Over at or above 2.
.nourish_classify <- function(value_norm) {
  dplyr::case_when(
    is.na(value_norm) ~ NA_character_,
    value_norm < 1 ~ "Under",
    value_norm < 2 ~ "Adequate",
    .default = "Over"
  )
}
