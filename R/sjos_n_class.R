# The 2-way Safe and Just Operating Space for nitrogen classification (SJOS-N
# Module 4, Task 4.1). Crosses the ecological boundary side (Within_boundary /
# Exceedance) with the nourishment "just" side (Under / Adequate / Over) into
# the six ordered whep::sjos_levels. The boundary side is decided per crop
# (item_cbs_code) from a build_n_boundary_exceedance(resolution = "country")
# output (a crop-country-year exceeds when its exceedance mass is positive,
# matching Global's Bound == "Exceedance_Surplus" split, Global/R/sjos_n.r:364).
# The nourishment side is a single class per country-year (normalize_nourishment
# output) broadcast to every crop of that country-year. Kept per item_cbs so the
# classification lines up with the per-crop footprint intensity Module 4 traces
# (locked plan decision 14).

#' Classify crops into the 2-way SJOS-N safe-and-just space.
#'
#' @description
#' Crosses the ecological boundary side with the nourishment side into the six
#' ordered [sjos_levels]. Per crop (`item_cbs_code`), the boundary side is
#' `"Exceedance"` when the crop-country-year's `exceedance_n_t` is positive and
#' `"Within_boundary"` otherwise (the all-zero and missing cases fall to
#' `"Within_boundary"`). The country's nourishment class (`nourish`, from
#' [normalize_nourishment()]) is joined by `year` and `area_code` and broadcast
#' to each of its crops. The classification `paste(boundary_side, nourish)` is
#' one of `"Within_boundary Under"` ... `"Exceedance Over"`, returned as a factor
#' with all six `sjos_levels$level` levels. This reproduces Global's 2-way remap
#' (`Global/R/sjos_n.r:363`) at the per-`item_cbs` granularity Module 4's
#' footprint needs.
#'
#' @param exceedance A [build_n_boundary_exceedance()] output at
#'   `resolution = "country"`, keyed by `year`, `area_code`, `item_cbs_code`
#'   with the mass terms `exceedance_n_t`, `within_boundary_n_t`, `actual_n_t`.
#' @param nourishment A [normalize_nourishment()] output carrying `year`,
#'   `area_code` and the `nourish` class (`"Under"` / `"Adequate"` / `"Over"`),
#'   one row per country-year.
#' @param level_col The unquoted name for the classification column. Defaults to
#'   `sjos_class`.
#' @return A tibble keyed by `year`, `area_code`, `item_cbs_code` with the mass
#'   terms `exceedance_n_t`, `within_boundary_n_t`, `actual_n_t`, the joined
#'   `nourish` class, the `boundary_side` and the classification column (a factor
#'   over `sjos_levels$level`, named by `level_col`).
#' @export
#' @examples
#' classify_sjos_n(
#'   exceedance = tibble::tribble(
#'     ~year,
#'     ~area_code,
#'     ~item_cbs_code,
#'     ~exceedance_n_t,
#'     ~within_boundary_n_t,
#'     ~actual_n_t,
#'     2010L, 10L, 2511L, 5, 3, 8,
#'     2010L, 10L, 2513L, 0, 4, 4
#'   ),
#'   nourishment = tibble::tribble(
#'     ~year, ~area_code, ~nourish,
#'     2010L, 10L, "Over"
#'   )
#' )
classify_sjos_n <- function(exceedance, nourishment, level_col = sjos_class) {
  .check_columns(
    exceedance,
    c(
      "year",
      "area_code",
      "item_cbs_code",
      "exceedance_n_t",
      "within_boundary_n_t",
      "actual_n_t"
    ),
    "exceedance"
  )
  .check_columns(nourishment, c("year", "area_code", "nourish"), "nourishment")
  exceedance |>
    .sjos_n_boundary_side() |>
    .sjos_n_join_nourish(nourishment) |>
    dplyr::mutate(
      {{ level_col }} := factor(
        paste(.data$boundary_side, .data$nourish),
        levels = whep::sjos_levels$level
      )
    ) |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      "exceedance_n_t",
      "within_boundary_n_t",
      "actual_n_t",
      "nourish",
      "boundary_side",
      {{ level_col }}
    )
}

# ---- Private helpers -------------------------------------------------------

# Boundary side per crop: "Exceedance" when the crop's exceedance mass is
# positive, else "Within_boundary" (the all-zero and missing cases guard to
# "Within_boundary").
.sjos_n_boundary_side <- function(exceedance) {
  dplyr::mutate(
    exceedance,
    boundary_side = dplyr::if_else(
      !is.na(.data$exceedance_n_t) & .data$exceedance_n_t > 0,
      "Exceedance",
      "Within_boundary"
    )
  )
}

# Broadcast the country-year nourishment class to each crop, joining the single
# nourish class per (year, area_code) onto every crop row.
.sjos_n_join_nourish <- function(x, nourishment) {
  nourish <- dplyr::distinct(
    nourishment,
    .data$year,
    .data$area_code,
    .data$nourish
  )
  dplyr::left_join(x, nourish, by = c("year", "area_code"))
}
