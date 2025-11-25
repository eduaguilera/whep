#' Calculate Manure Management Emissions
#'
#' @description
#' Calculates manure CH4 and N2O emissions using IPCC Tier 1 or Tier 2 methods.
#' For Tier 2 CH4: Uses Volatile Solids (VS) and Methane Producing Potential (Bo) (IPCC 2019 Eq 10.23).
#' For Tier 2 N2O: Calculates Nitrogen Excretion (Nex) based on intake (Eq 10.30) and estimates emissions
#' from different Manure Management Systems (MMS).
#'
#' @param data Dataframe with `species`, `heads`, `GE`, `weight`, `weight_gain_kg_day`, `milk_yield_kg_day`.
#' @param method "tier1" or "tier2".
#' @param tier1_coefs Dataframe with `species`, `ef_manure_ch4`, `ef_manure_n2o`.
#' @param tier2_coefs Dataframe with `species`, `Bo`, `MCF_temp`.
#' @param manure_params Dataframe with `species`, `Nex_kg_head_yr`, `EF_n2o_direct`.
#' @param feed_data Dataframe with `species`, `UE_frac`, `DE_percent`, `Ash_content`, `CP_percent`.
#' @param mms_shares Dataframe with `species`, `system`, `mms`, `share`. Optional.
#' @param mms_coefs Dataframe with `mms`, `MCF`, `EF_n2o`. Optional.
#' @param constants List with `vs_energy_content`, `ch4_density`, `n2o_n_conversion`, `protein_n_content`.
#'
#' @return Dataframe with added `manure_ch4_tonnes`, `manure_n2o_tonnes`.
#' @export
#'
#' @examples
#' \dontrun{
#'   # ...
#' }
calculate_manure_emissions <- function(data, method = "tier2", tier1_coefs = NULL, tier2_coefs = NULL, manure_params = NULL, feed_data = NULL, mms_shares = NULL, mms_coefs = NULL, constants = NULL) {
  
  if (method == "tier1") {
    # Tier 1
    data <- data |>
      dplyr::left_join(tier1_coefs, by = "species") |>
      dplyr::mutate(
        ef_manure_ch4 = dplyr::coalesce(ef_manure_ch4, 0),
        ef_manure_n2o = dplyr::coalesce(ef_manure_n2o, 0),
        
        manure_ch4_tonnes = (heads * ef_manure_ch4) / 1000,
        manure_n2o_tonnes = (heads * ef_manure_n2o) / 1000
      )
      
  } else if (method == "tier2") {
    # Tier 2
    
    # Join Manure Params (Nex defaults, EF_n2o defaults)
    data <- data |>
      dplyr::left_join(manure_params, by = "species")
      
    # Join Feed Params (UE, DE, Ash, CP)
    if (!"UE_frac" %in% names(data)) {
        data <- data |> dplyr::left_join(feed_data, by = "species")
    }

    # Join Tier 2 Methane Params (Bo, MCF)
    coefs_to_join <- tier2_coefs |> dplyr::select(species, Bo, MCF_temp)
    data <- data |>
      dplyr::left_join(coefs_to_join, by = "species")
      
    data <- data |>
      dplyr::mutate(
        # VS Calculation (IPCC 2019 Eq 10.24)
        VS = (GE * (1 - DE_percent/100) + (UE_frac * GE)) * ((1 - Ash_content) / constants$vs_energy_content),
        
        # Nitrogen Excretion (Nex) Calculation (IPCC 2019 Eq 10.30)
        DMI = GE / 18.45, # Assuming 18.45 MJ/kg DM for feed GE content
        CP_percent = if ("CP_percent" %in% names(.)) dplyr::coalesce(CP_percent, 15) else 15, # Default 15% CP
        N_intake = DMI * (CP_percent / 100) / 6.25,
        
        # N_retention (simplified)
        N_retention_frac = dplyr::case_when(
            species %in% c("Cattle", "Buffalo") ~ 0.20, # Placeholder
            species %in% c("Sheep", "Goats") ~ 0.10,
            TRUE ~ 0.05
        ),
        
        Nex_calc = N_intake * (1 - N_retention_frac) * constants$days_in_year, # kg N/head/year
        
        # Use calculated Nex if possible, else default
        Nex = dplyr::coalesce(Nex_calc, Nex_kg_head_yr),
        
        # Default Bo
        Bo = dplyr::coalesce(Bo, 0.24)
      )
      
    # MMS Logic
    if (!is.null(mms_shares) && !is.null(mms_coefs)) {
      # Distribute into MMS
      # Check if system column exists
      if (!"system" %in% names(data)) {
        warning("System column missing for MMS calculation. Using defaults.")
        data <- data |>
          dplyr::mutate(
             MCF = dplyr::coalesce(MCF_temp, 0.10),
             ef_manure_ch4 = VS * constants$days_in_year * Bo * constants$ch4_density * MCF,
             manure_ch4_tonnes = (heads * ef_manure_ch4) / 1000,
             ef_manure_n2o = Nex * EF_n2o_direct * constants$n2o_n_conversion,
             manure_n2o_tonnes = (heads * ef_manure_n2o) / 1000
          )
      } else {
        # Expand data by MMS
        data_mms <- data |>
          dplyr::left_join(mms_shares, by = c("species", "system"))
          
        # Join MMS Coefs
        data_mms <- data_mms |>
          dplyr::left_join(mms_coefs, by = "mms")
          
        # Calculate Emissions per MMS
        data_mms <- data_mms |>
          dplyr::mutate(
            share = dplyr::coalesce(share, 1), # Default to 1 if no share found
            MCF = dplyr::coalesce(MCF, MCF_temp, 0.10), # Use MMS MCF, else fallback
            EF_n2o = dplyr::coalesce(EF_n2o, EF_n2o_direct, 0.005),
            
            # CH4
            ef_manure_ch4_mms = VS * constants$days_in_year * Bo * constants$ch4_density * MCF,
            manure_ch4_tonnes_mms = (heads * share * ef_manure_ch4_mms) / 1000,
            
            # N2O
            ef_manure_n2o_mms = Nex * EF_n2o * constants$n2o_n_conversion,
            manure_n2o_tonnes_mms = (heads * share * ef_manure_n2o_mms) / 1000
          )
          
        # Summarize back
        group_cols <- names(data)
        
        data <- data_mms |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
          dplyr::summarise(
            manure_ch4_tonnes = sum(manure_ch4_tonnes_mms, na.rm = TRUE),
            manure_n2o_tonnes = sum(manure_n2o_tonnes_mms, na.rm = TRUE),
            .groups = "drop"
          )
      }
      
    } else {
      # Fallback to simple Tier 2 (single MCF/EF)
      data <- data |>
        dplyr::mutate(
          MCF = dplyr::coalesce(MCF_temp, 0.10),
          ef_manure_ch4 = VS * constants$days_in_year * Bo * constants$ch4_density * MCF,
          manure_ch4_tonnes = (heads * ef_manure_ch4) / 1000,
          
          ef_manure_n2o = Nex * EF_n2o_direct * constants$n2o_n_conversion,
          manure_n2o_tonnes = (heads * ef_manure_n2o) / 1000
        )
    }
  }
  
  return(data)
}
