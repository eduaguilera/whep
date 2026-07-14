# The embodied-nitrogen footprint extension (SJOS-N Module 4, Task 4.2). Turns a
# build_n_boundary_exceedance(resolution = "country") output into a
# build_footprint() extension tibble (year, area_code, item_cbs_code, impact_u,
# method_n_exceedance), carrying one of the three per-crop nitrogen categories as
# the footprint intensity: the exceedance mass, the within-boundary mass, or the
# crop's total actual surplus. These are Global's Impact_prod_all u_FU categories
# (Global/R/sjos_n.r:349) fed into the trade footprint. Kept per item_cbs so the
# footprint can be traced (locked plan decision 14).

#' Build the embodied-nitrogen footprint extension.
#'
#' @description
#' Selects one nitrogen category from a
#' [build_n_boundary_exceedance()] country-resolution output into the
#' `impact_u` column of a [build_footprint()] extension: `"exceedance"` (the
#' default) carries `exceedance_n_t`, `"within_boundary"` carries
#' `within_boundary_n_t`, and `"production"` carries `actual_n_t` (the crop's
#' total surplus). The chosen category is stamped in `method_n_exceedance`.
#'
#' The three categories are traced as three separate extension passes (one call
#' per category), not one signed impact: the footprint framework carries a single
#' non-negative `impact_u`, so the within-boundary and exceedance parts are run
#' as distinct passes and compared afterwards, never combined into one
#' signed intensity.
#'
#' The per-crop (`item_cbs_code`) granularity is preserved so the footprint can
#' be traced to origin (locked plan decision 14). Rows with a missing key are
#' dropped defensively; zero-impact crops are kept because they still consume
#' trade.
#'
#' @param exceedance A [build_n_boundary_exceedance()] output at
#'   `resolution = "country"`, keyed by `year`, `area_code`, `item_cbs_code`
#'   with the mass terms `exceedance_n_t`, `within_boundary_n_t`, `actual_n_t`.
#' @param category Which nitrogen mass to carry into `impact_u`: `"exceedance"`
#'   (default), `"within_boundary"` or `"production"`. Validated with
#'   [rlang::arg_match()].
#' @return A tibble with the [build_footprint()] extension contract columns
#'   `year`, `area_code`, `item_cbs_code`, `impact_u` (tonnes N) and
#'   `method_n_exceedance` (the chosen category).
#' @export
#' @examples
#' build_n_exceedance_extension(
#'   tibble::tribble(
#'     ~year,
#'     ~area_code,
#'     ~item_cbs_code,
#'     ~exceedance_n_t,
#'     ~within_boundary_n_t,
#'     ~actual_n_t,
#'     2010L, 10L, 2511L, 5, 3, 8,
#'     2010L, 10L, 2513L, 0, 4, 4
#'   ),
#'   category = "exceedance"
#' )
build_n_exceedance_extension <- function(
  exceedance,
  category = c("exceedance", "within_boundary", "production")
) {
  category <- rlang::arg_match(category)
  .check_columns(
    exceedance,
    c("year", "area_code", "item_cbs_code", .nex_category_col(category)),
    "exceedance"
  )
  exceedance |>
    .nex_select_impact(category) |>
    .nex_drop_bad_keys() |>
    dplyr::mutate(method_n_exceedance = category) |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      "impact_u",
      "method_n_exceedance"
    )
}

# ---- Private helpers -------------------------------------------------------

# Map the requested category to its source mass column in the exceedance input.
.nex_category_col <- function(category) {
  switch(
    category,
    exceedance = "exceedance_n_t",
    within_boundary = "within_boundary_n_t",
    production = "actual_n_t"
  )
}

# Copy the chosen category's mass into the footprint intensity column impact_u.
.nex_select_impact <- function(exceedance, category) {
  dplyr::mutate(
    exceedance,
    impact_u = .data[[.nex_category_col(category)]]
  )
}

# Drop rows whose footprint key is incomplete, keeping zero-impact crops (they
# still consume trade).
.nex_drop_bad_keys <- function(x) {
  dplyr::filter(
    x,
    !is.na(.data$year),
    !is.na(.data$area_code),
    !is.na(.data$item_cbs_code)
  )
}
