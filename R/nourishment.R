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
#' @param thresholds Either a named `floor`/`ceiling` pair applied to every row
#'   (a named numeric vector or list), or a **data frame of per-country-year
#'   bounds** keyed by `year` and `area_code` with either
#'   `floor_g_cap_day`/`ceiling_g_cap_day` or `floor`/`ceiling` — so a
#'   [build_nourishment_band()] output passes straight through. A row that
#'   matches no band is classified `NA` and named in a warning, never silently
#'   given the flat default. When `NULL` (default) the flat protein bounds from
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
  if (is.data.frame(thresholds)) {
    return(
      x |>
        .nourish_join_bounds(thresholds) |>
        dplyr::mutate(
          value_norm = .nourish_normalize(
            {{ value_col }},
            .data$.nourish_floor,
            .data$.nourish_ceiling
          ),
          nourish = .nourish_classify(.data$value_norm)
        ) |>
        dplyr::select(-".nourish_floor", -".nourish_ceiling")
    )
  }
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

# A per-country-year band instead of one scalar pair. The arithmetic below is
# already vectorised, so the only work is joining the bounds onto the rows and
# refusing to let a row that finds none pass as if it had.
.nourish_join_bounds <- function(x, thresholds) {
  bounds <- .nourish_band_columns(thresholds)
  .check_columns(x, c("year", "area_code"), "x")
  # A band carrying two rows for one country-year would not error here: it
  # would DUPLICATE the country, once per candidate band, and every headcount
  # and class downstream would count it twice. build_nourishment_band() output
  # is unique by construction, but this argument takes any data frame, so the
  # same guard the band applies to its own inputs applies here.
  .nb_check_unique(bounds, "thresholds")
  joined <- dplyr::left_join(x, bounds, by = c("year", "area_code"))
  .nourish_warn_unbanded(joined)
  joined
}

# The band's own column names, or the short pair, so a caller can pass a
# build_nourishment_band() output straight through.
.nourish_band_columns <- function(thresholds) {
  .check_columns(thresholds, c("year", "area_code"), "thresholds")
  pairs <- list(
    c("floor_g_cap_day", "ceiling_g_cap_day"),
    c("floor", "ceiling")
  )
  for (nm in pairs) {
    if (all(rlang::has_name(thresholds, nm))) {
      return(tibble::tibble(
        year = thresholds$year,
        area_code = thresholds$area_code,
        .nourish_floor = thresholds[[nm[1]]],
        .nourish_ceiling = thresholds[[nm[2]]]
      ))
    }
  }
  cli::cli_abort(c(
    "A data-frame {.arg thresholds} needs a floor and a ceiling column.",
    i = "Expected {.field floor_g_cap_day}/{.field ceiling_g_cap_day} or
         {.field floor}/{.field ceiling}."
  ))
}

# A row with no band gets NA, not the flat default. Silently falling back would
# mix two threshold vintages inside one classification.
#
# EITHER bound missing is enough. A row matched to a band whose ceiling is NA
# also scores NA, and testing only the floor would let that one through
# unreported -- the country would simply vanish from the classification.
.nourish_warn_unbanded <- function(joined) {
  missing <- dplyr::filter(
    joined,
    is.na(.data$.nourish_floor) | is.na(.data$.nourish_ceiling)
  )
  if (nrow(missing) == 0L) {
    return(invisible())
  }
  areas <- unique(missing$area_code)
  cli::cli_warn(c(
    "!" = "{nrow(missing)} row{?s} have no threshold band, so their
           {.field nourish} class is {.val {NA}}.",
    "i" = "Area code{cli::qty(length(areas))}{?s}: {areas}."
  ))
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
