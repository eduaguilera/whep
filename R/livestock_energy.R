#' Estimate Energy Demand (Gross Energy) - Tier 2
#'
#' @description
#' Calculates Gross Energy (GE) intake based on IPCC 2019 Tier 2 equations (Vol 4, Chap 10).
#' It estimates Net Energy for Maintenance (NEm), Activity (NEa), Lactation (NEl),
#' Work (NEwork), Pregnancy (NEp), and Growth (NEg) to derive the total Gross Energy (GE).
#'
#' @param data A dataframe with `species`, `cohort`, `heads`. Optional columns:
#'   `weight`, `milk_yield_kg_day`, `fat_percent`, `weight_gain_kg_day`,
#'   `work_hours_day`, `pregnant_fraction`.
#' @param energy_coefs A dataframe with `species` (and optionally `cohort`) and coefficients:
#'   `Cfi` (Maintenance), `Ca_stall`, `Ca_pasture`, `Ca_grazing` (Activity),
#'   `Cp_ratio` (Pregnancy), `Cwork_ratio` (Work), `Cl_base`, `Cl_fat` (Lactation).
#' @param weight_data A dataframe with `species`, `cohort`, `weight_kg`. Used if `weight` missing in data.
#' @param feed_data A dataframe with `species`, `DE_percent`, `UE_frac`, `REM_ratio`.
#'
#' @return Dataframe with added `GE` (MJ/day) and intermediate NE components.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- tibble::tibble(
#'     species = "Cattle",
#'     cohort = "Adult Female",
#'     heads = 100,
#'     weight = 500,
#'     milk_yield_kg_day = 15,
#'     fat_percent = 4.0
#'   )
#'   # ... coefs ...
#'   estimate_energy_demand(data, energy_coefs, weight_data, feed_data)
#' }
estimate_energy_demand <- function(data, energy_coefs, weight_data, feed_data) {
  
  # 1. Ensure weight column exists
  if (!"weight" %in% names(data)) {
    data <- data |>
      dplyr::left_join(weight_data, by = c("species", "cohort")) |>
      dplyr::rename(weight = weight_kg)
  } else {
    data <- data |>
      dplyr::left_join(weight_data, by = c("species", "cohort")) |>
      dplyr::mutate(weight = dplyr::coalesce(weight, weight_kg)) |>
      dplyr::select(-weight_kg)
  }
  
  # 2. Join energy coefficients
  # Try joining by species and cohort first, then fallback to species only if needed
  # For simplicity here, we join by species and assume coefs are species-level or data has specific columns
  data <- data |>
    dplyr::left_join(energy_coefs, by = "species")
    
  # 3. Join feed parameters
  data <- data |>
    dplyr::left_join(feed_data, by = "species")
  
  # 4. Set defaults for missing production/activity columns
  data <- data |>
    dplyr::mutate(
      milk_yield_kg_day = if ("milk_yield_kg_day" %in% names(.)) dplyr::coalesce(milk_yield_kg_day, 0) else 0,
      fat_percent = if ("fat_percent" %in% names(.)) dplyr::coalesce(fat_percent, 4.0) else 4.0, # Default 4% fat
      weight_gain_kg_day = if ("weight_gain_kg_day" %in% names(.)) dplyr::coalesce(weight_gain_kg_day, 0) else 0,
      work_hours_day = if ("work_hours_day" %in% names(.)) dplyr::coalesce(work_hours_day, 0) else 0,
      pregnant_fraction = if ("pregnant_fraction" %in% names(.)) dplyr::coalesce(pregnant_fraction, 0) else 0,
      
      # Activity coefficients based on system if available, else default
      # Assuming 'system' column exists from calculate_cohorts_systems
      Ca = dplyr::case_when(
        "system" %in% names(.) & system == "Stall" ~ dplyr::coalesce(Ca_stall, 0),
        "system" %in% names(.) & system == "Pasture" ~ dplyr::coalesce(Ca_pasture, 0.17),
        "system" %in% names(.) & system == "Grazing" ~ dplyr::coalesce(Ca_grazing, 0.36),
        TRUE ~ dplyr::coalesce(Ca_pasture, 0.17) # Default
      )
    )

  # 5. Calculate Energy Requirements
  data <- data |>
    dplyr::mutate(
      # Net Energy for Maintenance (NEm) = Cfi * weight^0.75
      # Cfi defaults: Cattle 0.386 (Dairy), 0.322 (Other). 
      # We assume Cfi is in energy_coefs.
      NEm = Cfi * weight^0.75,
      
      # Net Energy for Activity (NEa) = Ca * NEm
      NEa = Ca * NEm,
      
      # Net Energy for Lactation (NEl)
      # Eq 10.8: Milk * (1.47 + 0.40 * Fat)
      # Using coefs if provided, else IPCC defaults
      Cl_base = if ("Cl_base" %in% names(.)) dplyr::coalesce(Cl_base, 1.47) else 1.47,
      Cl_fat = if ("Cl_fat" %in% names(.)) dplyr::coalesce(Cl_fat, 0.40) else 0.40,
      NEl = milk_yield_kg_day * (Cl_base + Cl_fat * fat_percent),
      
      # Net Energy for Work (NEwork)
      # Eq 10.11: 0.10 * NEm * Hours
      Cwork_ratio = if ("Cwork_ratio" %in% names(.)) dplyr::coalesce(Cwork_ratio, 0.10) else 0.10,
      NEwork = Cwork_ratio * NEm * work_hours_day,
      
      # Net Energy for Pregnancy (NEp)
      # Eq 10.13: Cpregnancy * NEm (for pregnant animals)
      # Applied to the fraction of the cohort that is pregnant
      Cp_ratio = if ("Cp_ratio" %in% names(.)) dplyr::coalesce(Cp_ratio, 0.10) else 0.10,
      NEp = Cp_ratio * NEm * pregnant_fraction,
      
      # Net Energy for Growth (NEg)
      # Simplified: Gain * Energy_Density
      # IPCC Eq 10.6 is complex. We'll use a simplified coefficient Cg (MJ/kg gain)
      # Default approx 22 MJ/kg for cattle if not provided
      Cg = if ("Cg" %in% names(.)) dplyr::coalesce(Cg, 22) else 22,
      NEg = weight_gain_kg_day * Cg,
      
      # Total Net Energy
      NE = NEm + NEa + NEl + NEwork + NEp + NEg,
      
      # Gross Energy (GE) = NE / (REM) / (DE/100)
      # REM (Ratio of Net Energy available in diet for maintenance to Digestible Energy)
      # IPCC Eq 10.14: REM = 1.123 - 4.092e-3 * DE% + 1.126e-5 * (DE%)^2 - 25.4/DE%
      # Or use provided REM_ratio
      REM = if ("REM_ratio" %in% names(.)) dplyr::coalesce(REM_ratio, 0.50) else 0.50,
      
      GE = (NE / REM) / (DE_percent / 100)
    )
  
  return(data)
}
