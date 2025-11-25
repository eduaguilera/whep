#' Calculate Enteric Methane Emissions
#'
#' @description
#' Calculates enteric CH4 emissions using IPCC Tier 1 (fixed Emission Factors) or
#' Tier 2 (based on Gross Energy intake and Methane Conversion Factor Ym).
#' References: IPCC 2019, Vol 4, Chapter 10, Eq 10.21 (Tier 2).
#'
#' @param data Dataframe.
#' @param method "tier1" or "tier2".
#' @param tier1_coefs Dataframe with `species`, `ef_enteric`.
#' @param tier2_coefs Dataframe with `species`, `Ym`.
#' @param constants List with `energy_content_ch4`.
#'
#' @return Dataframe with added `enteric_ch4_tonnes`.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- tibble::tibble(
#'     species = "Cattle", heads = 100, GE = 200
#'   )
#'   # Tier 1
#'   tier1_coefs <- tibble::tibble(species = "Cattle", ef_enteric = 50)
#'   calculate_enteric_ch4(data, method = "tier1", tier1_coefs = tier1_coefs)
#'
#'   # Tier 2
#'   tier2_coefs <- tibble::tibble(species = "Cattle", Ym = 6.5)
#'   constants <- list(days_in_year = 365, energy_content_ch4 = 55.65)
#'   calculate_enteric_ch4(data, method = "tier2", tier2_coefs = tier2_coefs, constants = constants)
#' }
calculate_enteric_ch4 <- function(data, method = "tier2", tier1_coefs = NULL, tier2_coefs = NULL, constants = NULL) {
  
  if (method == "tier1") {
    # Tier 1: Emissions = Heads * EF
    data <- data |>
      dplyr::left_join(tier1_coefs, by = "species") |>
      dplyr::mutate(
        ef_enteric = dplyr::coalesce(ef_enteric, 0),
        enteric_ch4_tonnes = (heads * ef_enteric) / 1000
      )
    
  } else if (method == "tier2") {
    # Tier 2: Emissions = GE * Ym ...
    
    # Join Tier 2 Methane Params (Ym)
    # If Ym already exists in data (e.g. specific to system), we keep it
    if (!"Ym" %in% names(data)) {
       coefs_to_join <- tier2_coefs |> dplyr::select(species, Ym)
       data <- data |> dplyr::left_join(coefs_to_join, by = "species")
    }
    
    data <- data |>
      dplyr::mutate(
        Ym = dplyr::coalesce(Ym, 6.5), # Should ideally come from table, but robust fallback
        # EF = (GE * (Ym/100) * 365) / 55.65
        ef_enteric = (GE * (Ym / 100) * constants$days_in_year) / constants$energy_content_ch4,
        enteric_ch4_tonnes = (heads * ef_enteric) / 1000
      )
  }
  
  return(data)
}
