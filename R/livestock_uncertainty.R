#' Calculate Uncertainty Bounds for Livestock Emissions
#'
#' @description
#' Applies uncertainty ranges to emission estimates based on IPCC guidance.
#' Provides lower and upper bounds for confidence intervals.
#'
#' @param data Dataframe with emission estimates
#' @param confidence_level Confidence level (default 0.95 for 95% CI)
#'
#' @return Dataframe with added uncertainty columns
#' @export
#'
#' @examples
#' \dontrun{
#'   results <- calculate_livestock_emissions(data, method = "tier2")
#'   results_with_uncertainty <- calculate_uncertainty_bounds(results)
#' }
calculate_uncertainty_bounds <- function(data, confidence_level = 0.95) {
  
  # Load uncertainty ranges
  load_coefs <- function() {
    if (!exists("uncertainty_ranges", envir = parent.frame(2))) {
      if (file.exists("data/livestock_coefs.rda")) {
        load("data/livestock_coefs.rda", envir = parent.frame(2))
      } else {
        stop("Coefficient tables not found.")
      }
    }
  }
  load_coefs()
  
  # Calculate z-score for confidence level
  z <- qnorm((1 + confidence_level) / 2)
  
  # Apply uncertainty to emissions
  data <- data |>
    dplyr::mutate(
      # Enteric CH4 uncertainty (±15% from Ym uncertainty)
      enteric_ch4_lower = dplyr::if_else(
        !is.na(enteric_ch4_tier2),
        enteric_ch4_tier2 * 0.85,
        enteric_ch4_tier1 * 0.85
      ),
      enteric_ch4_upper = dplyr::if_else(
        !is.na(enteric_ch4_tier2),
        enteric_ch4_tier2 * 1.15,
        enteric_ch4_tier1 * 1.15
      ),
      
      # Manure CH4 uncertainty (±30% from MCF uncertainty)
      manure_ch4_lower = dplyr::if_else(
        !is.na(manure_ch4_tier2),
        manure_ch4_tier2 * 0.70,
        manure_ch4_tier1 * 0.70
      ),
      manure_ch4_upper = dplyr::if_else(
        !is.na(manure_ch4_tier2),
        manure_ch4_tier2 * 1.30,
        manure_ch4_tier1 * 1.30
      ),
      
      # Manure N2O uncertainty (-50% to +100% from EF uncertainty)
      manure_n2o_lower = dplyr::if_else(
        !is.na(manure_n2o_tier2),
        manure_n2o_tier2 * 0.50,
        manure_n2o_tier1 * 0.50
      ),
      manure_n2o_upper = dplyr::if_else(
        !is.na(manure_n2o_tier2),
        manure_n2o_tier2 * 2.00,
        manure_n2o_tier1 * 2.00
      )
    )
  
  return(data)
}
