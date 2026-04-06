#' Calculate uncertainty bounds for livestock emissions.
#'
#' @description
#' Applies IPCC uncertainty ranges to emission estimates.
#' Multipliers sourced from `uncertainty_ranges` table
#' (no hardcoded values).
#'
#' @param data Dataframe with emission columns from
#'   `calculate_livestock_emissions()`.
#'
#' @return Dataframe with added `_lower` and `_upper` columns
#'   for each emission estimate.
#' @export
#'
#' @examples
#' \dontrun{
#'   tibble::tibble(
#'     species = "Dairy Cattle",
#'     cohort = "Adult Female",
#'     heads = 1000,
#'     weight = 600,
#'     diet_quality = "High",
#'     milk_yield_kg_day = 20
#'   ) |>
#'     calculate_livestock_emissions() |>
#'     calculate_uncertainty_bounds()
#' }
calculate_uncertainty_bounds <- function(data) {
  unc <- uncertainty_ranges

  emis_cols <- .get_emission_columns(data)

  if (length(emis_cols) == 0) {
    cli::cli_warn(
      "No emission columns found. \\
       Run {.fun calculate_livestock_emissions} first."
    )
    return(data)
  }

  purrr::reduce(
    emis_cols,
    \(data, col_info) {
      bounds <- .get_uncertainty_bounds(unc, col_info$param)
      data |>
        dplyr::mutate(
          !!paste0(col_info$col, "_lower") := .data[[col_info$col]] *
            bounds$lower,
          !!paste0(col_info$col, "_upper") := .data[[col_info$col]] *
            bounds$upper
        )
    },
    .init = data
  )
}

# Private helpers ----

#' Map emission columns to uncertainty parameters.
#' @noRd
.get_emission_columns <- function(data) {
  col_map <- list(
    list(col = "enteric_ch4_tier1", param = "Ym"),
    list(col = "enteric_ch4_tier2", param = "Ym"),
    list(col = "manure_ch4_tier1", param = "MCF"),
    list(col = "manure_ch4_tier2", param = "MCF"),
    list(col = "manure_n2o_direct", param = "EF_N2O"),
    list(col = "manure_n2o_indirect", param = "EF_N2O"),
    list(col = "manure_n2o_total", param = "EF_N2O")
  )

  purrr::keep(col_map, ~ .x$col %in% names(data))
}

#' Get lower/upper multipliers from uncertainty table.
#' @noRd
.get_uncertainty_bounds <- function(unc, param) {
  row <- unc |>
    dplyr::filter(parameter == param)

  if (nrow(row) == 0) {
    return(list(lower = 0.80, upper = 1.20))
  }

  list(
    lower = row$lower_mult[1],
    upper = row$upper_mult[1]
  )
}
