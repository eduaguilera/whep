#' Calculate Livestock Emissions (High-Level Wrapper)
#'
#' @description
#' Orchestrates the calculation of livestock emissions.
#' Loads default coefficients from package data if not provided.
#' This function integrates the GLEAM 3.0 herd structure (cohorts/systems) and
#' IPCC 2019 Tier 2 energy and emissions calculations.
#'
#' @param data Input dataframe.
#' @param method Method to use ("tier1", "tier2", "all").
#' @param coefs_list Optional list of coefficients to override defaults.
#'
#' @return Dataframe with emission columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Requires internal package data to be available or passed in coefs_list
#'   data <- tibble::tibble(
#'     year = 2020, geog_id = "USA", species = "Cattle", heads = 1000
#'   )
#'   # calculate_livestock_emissions(data, method = "tier1")
#' }
calculate_livestock_emissions <- function(data, method = "all", coefs_list = NULL) {
  
  # Load defaults if not provided
  if (is.null(coefs_list)) {
    if (!exists("ipcc_tier1_enteric")) {
        if (file.exists("data/livestock_coefs.rda")) {
            load("data/livestock_coefs.rda", envir = environment())
        }
    }
  } else {
    list2env(coefs_list, envir = environment())
  }
  
  results <- list()
  
  # Run Tier 1
  if (method %in% c("tier1", "all")) {
    message("Running Tier 1...")
    t1_data <- data |>
      calculate_enteric_ch4(method = "tier1", tier1_coefs = ipcc_tier1_enteric) |>
      calculate_manure_emissions(method = "tier1", tier1_coefs = ipcc_tier1_manure) |>
      dplyr::rename(
        enteric_ch4_tier1 = enteric_ch4_tonnes,
        manure_ch4_tier1 = manure_ch4_tonnes,
        manure_n2o_tier1 = manure_n2o_tonnes
      )
    results[["tier1"]] <- t1_data
  }
  
  # Run Tier 2
  if (method %in% c("tier2", "all")) {
    message("Running Tier 2...")
    
    # 1. Cohorts and Systems
    d_cohorts <- calculate_cohorts_systems(data, gleam_default_cohort_shares, gleam_default_system_shares)
    
    # 1b. Join Production Defaults (if available)
    if (exists("production_defaults")) {
      # Only join columns that are not already in data
      cols_to_add <- setdiff(names(production_defaults), names(d_cohorts))
      if (length(cols_to_add) > 0) {
        # We need 'species' for joining
        cols_to_select <- c("species", cols_to_add)
        d_cohorts <- d_cohorts |>
          dplyr::left_join(production_defaults |> dplyr::select(dplyr::all_of(cols_to_select)), by = "species")
      }
    }
    
    # 2. Energy
    d_energy <- estimate_energy_demand(d_cohorts, ipcc_tier2_energy, livestock_weights, feed_params)
    
    # 3. Emissions
    t2_data <- d_energy |>
      calculate_enteric_ch4(method = "tier2", tier2_coefs = ipcc_tier2_methane, constants = livestock_constants) |>
      calculate_manure_emissions(
        method = "tier2", 
        tier2_coefs = ipcc_tier2_methane, 
        manure_params = manure_params, 
        feed_data = feed_params, 
        mms_shares = if(exists("gleam_mms_shares")) gleam_mms_shares else NULL,
        mms_coefs = if(exists("ipcc_mms_coefs")) ipcc_mms_coefs else NULL,
        constants = livestock_constants
      ) |>
      dplyr::rename(
        enteric_ch4_tier2 = enteric_ch4_tonnes,
        manure_ch4_tier2 = manure_ch4_tonnes,
        manure_n2o_tier2 = manure_n2o_tonnes
      )
    results[["tier2"]] <- t2_data
  }
  
  if (method == "all") {
    return(results)
  } else {
    return(results[[method]])
  }
}
