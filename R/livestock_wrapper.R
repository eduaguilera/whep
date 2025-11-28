#' Calculate Livestock Emissions (High-Level Wrapper)
#'
#' @description
#' Orchestrates the calculation of livestock emissions using IPCC and GLEAM methodologies.
#' Supports Tier 1 (fixed emission factors) and Tier 2 (energy-based calculations).
#' All coefficient tables are loaded internally by the component functions.
#'
#' @param data Input dataframe with columns: `year`, `geog_id`, `species`, `heads`.
#'   Optional columns: `iso3`, `region`, `climate`, `system`, `weight`, `milk_yield_kg_day`, etc.
#' @param method Method to use: "tier1", "tier2", or "all" (character).
#'   - "tier1": IPCC Tier 1 with fixed emission factors
#'   - "tier2": IPCC Tier 2 with energy-based calculations
#'   - "all": Run both methods and return list of results
#'
#' @return 
#' If method = "all": Named list with elements "tier1", "tier2"
#' Otherwise: Dataframe with emission columns for the specified method
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- tibble::tibble(
#'     year = 2020,
#'     geog_id = "USA",
#'     iso3 = "USA",
#'     species = "Dairy Cattle",
#'     heads = 1000
#'   )
#'   
#'   # Run all methods
#'   results <- calculate_livestock_emissions(data, method = "all")
#'   
#'   # Run specific method
#'   tier1_results <- calculate_livestock_emissions(data, method = "tier1")
#' }
calculate_livestock_emissions <- function(data, method = "all") {
  
  results <- list()
  
  # ===== TIER 1 =====
  if (method %in% c("tier1", "all")) {
    message("\n=== Running IPCC Tier 1 ===")
    
    t1_data <- data |>
      calculate_enteric_ch4(method = "tier1") |>
      calculate_manure_emissions(method = "tier1") |>
      dplyr::rename(
        enteric_ch4_tier1 = enteric_ch4_tonnes,
        manure_ch4_tier1 = manure_ch4_tonnes,
        manure_n2o_tier1 = manure_n2o_tonnes
      )
    
    results[["tier1"]] <- t1_data
    message("✓ Tier 1 complete")
  }
  
  # ===== TIER 2 =====
  if (method %in% c("tier2", "all")) {
    message("\n=== Running IPCC Tier 2 ===")
    
    # For Tier 2, we need cohort information
    # If not present, we'll use a default cohort
    if (!"cohort" %in% names(data)) {
      data <- data |> dplyr::mutate(
        cohort = dplyr::case_when(
          grepl("Cattle|Buffalo", species, ignore.case = TRUE) ~ "Adult Female",
          grepl("Swine|Pig", species, ignore.case = TRUE) ~ "Fattening",
          TRUE ~ "Adult"
        )
      )
    }
    
    t2_data <- data |>
      estimate_energy_demand() |>
      calculate_enteric_ch4(method = "tier2") |>
      calculate_manure_emissions(method = "tier2") |>
      dplyr::rename(
        enteric_ch4_tier2 = enteric_ch4_tonnes,
        manure_ch4_tier2 = manure_ch4_tonnes,
        manure_n2o_tier2 = manure_n2o_tonnes
      )
    
    results[["tier2"]] <- t2_data
    message("✓ Tier 2 complete")
  }
  
  # Return results
  if (method == "all") {
    message("\n=== All methods complete ===")
    return(results)
  } else {
    return(results[[method]])
  }
}
