#' Calculate Livestock Emissions
#'
#' @description
#' Main wrapper function to calculate livestock emissions (enteric CH4, manure CH4, manure N2O).
#' Supports granular method selection for different IPCC versions and tiers.
#' 
#' **Available Methods**:
#' - `"tier1_2006"`: IPCC 2006 Tier 1 (fixed emission factors)
#' - `"tier1_2019"`: IPCC 2019 Tier 1 (fixed emission factors, climate-specific)
#' - `"tier2_2006"`: IPCC 2006 Tier 2 (energy-based, fixed Ym, temperature-based MCF)
#' - `"tier2_2019"`: IPCC 2019 Tier 2 (energy-based, dynamic Ym, climate-based MCF) **[DEFAULT]**
#' - `"all"`: Runs all methods and returns combined results with suffixed columns
#' 
#' **Output Columns**:
#' - `Method_Energy`, `Method_Enteric`, `Method_Manure`: Track which assumptions/fallbacks were applied
#' - `enteric_ch4_tonnes`, `manure_ch4_tonnes`, `manure_n2o_tonnes`: Emission results
#' - `GE`, `VS`, `Nex`: Intermediate calculation results (Tier 2 only)
#'
#' @param data Dataframe with livestock data. See individual function documentation for required columns.
#' @param method Method selection (default "tier2_2019"). See description for options.
#' @param include_indirect Logical, include indirect N2O emissions (default TRUE).
#' @param ym_interpolation Ym interpolation method: "continuous" (default) or "categorical".
#'
#' @return Dataframe with emission results and Method tracking columns.
#' @export
calculate_livestock_emissions <- function(data, method = "tier2_2019", include_indirect = TRUE, ym_interpolation = "continuous") {
  
  if (method == "all") {
    # Run all methods and combine
    methods <- c("tier1_2006", "tier1_2019", "tier2_2006", "tier2_2019")
    
    results <- data
    
    for (m in methods) {
      # Calculate for specific method
      res <- calculate_livestock_emissions(data, method = m, include_indirect = include_indirect, ym_interpolation = ym_interpolation)
      
      # Select emission columns and rename with suffix
      suffix <- paste0("_", m)
      cols_to_keep <- c("enteric_ch4_tonnes", "manure_ch4_tonnes", "manure_n2o_tonnes", "GE", "VS", "Nex")
      
      # Only keep columns that exist in result
      cols_to_keep <- intersect(cols_to_keep, names(res))
      
      res_subset <- res |>
        dplyr::select(dplyr::all_of(cols_to_keep)) |>
        dplyr::rename_with(~paste0(., suffix), dplyr::all_of(cols_to_keep))
      
      # Bind columns (assuming row order is preserved, which it should be)
      results <- dplyr::bind_cols(results, res_subset)
    }
    
    return(results)
  }
  
  # Parse method
  if (method == "tier1_2006") {
    calc_method <- "tier1"
    tier1_ver <- "2006"
    ym_opt <- "ipcc2006" # Irrelevant for Tier 1
    mcf_opt <- "temperature" # Irrelevant for Tier 1
  } else if (method == "tier1_2019") {
    calc_method <- "tier1"
    tier1_ver <- "2019"
    ym_opt <- "ipcc2019"
    mcf_opt <- "climate_zone"
  } else if (method == "tier2_2006") {
    calc_method <- "tier2"
    tier1_ver <- "2006" # Fallback if needed
    ym_opt <- "ipcc2006" # Fixed Ym
    mcf_opt <- "temperature" # Temperature based MCF
  } else if (method == "tier2_2019") {
    calc_method <- "tier2"
    tier1_ver <- "2019"
    ym_opt <- "ipcc2019" # Dynamic Ym
    mcf_opt <- "climate_zone" # Climate based MCF
  } else {
    stop("Unknown method: ", method)
  }
  
  # 1. Estimate Energy (Only needed for Tier 2, but good to have)
  if (calc_method == "tier2") {
    data <- estimate_energy_demand(data, method = "ipcc2019") # Energy equations are mostly 2019 refined
  }
  
  # 2. Enteric Methane
  data <- calculate_enteric_ch4(
    data, 
    method = calc_method, 
    tier1_version = tier1_ver, 
    ym_method = ym_opt,
    ym_interpolation = ym_interpolation
  )
  
  # 3. Manure Emissions
  data <- calculate_manure_emissions(
    data, 
    method = calc_method, 
    tier1_version = tier1_ver, 
    mcf_method = mcf_opt,
    include_indirect = include_indirect
  )
  
  return(data)
}
