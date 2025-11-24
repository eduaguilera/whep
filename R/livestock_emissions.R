#' Calculate Cohorts and Systems Distribution
#'
#' @description
#' Distributes livestock head counts into cohorts and production systems using provided shares.
#' This step is essential for applying GLEAM 3.0 methodology, which differentiates emissions
#' factors by animal cohort (e.g., adult females, calves) and production system (e.g., grassland, mixed).
#'
#' @param data A dataframe containing at least `year`, `geog_id`, `species`, and `heads`.
#' @param cohort_shares A dataframe with columns `species`, `cohort`, `share`.
#' @param system_shares A dataframe with columns `species`, `system`, `share` (and optionally `geog_id`).
#'
#' @return A dataframe with added columns `cohort`, `system`, and updated `heads`.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Dummy data
#'   data <- tibble::tibble(
#'     year = 2020,
#'     geog_id = "USA",
#'     species = "Cattle",
#'     heads = 1000
#'   )
#'   cohort_shares <- tibble::tibble(
#'     species = "Cattle",
#'     cohort = c("Adult Female", "Adult Male", "Calf"),
#'     share = c(0.5, 0.3, 0.2)
#'   )
#'   system_shares <- tibble::tibble(
#'     species = "Cattle",
#'     system = c("Grassland", "Mixed"),
#'     share = c(0.6, 0.4)
#'   )
#'   calculate_cohorts_systems(data, cohort_shares, system_shares)
#' }
#'
#' @importFrom dplyr left_join mutate select filter group_by summarize rename
calculate_cohorts_systems <- function(data, cohort_shares, system_shares) {
  
  # 1. Join System Shares
  if ("geog_id" %in% names(system_shares)) {
    data_sys <- data |>
      dplyr::left_join(system_shares, by = c("geog_id", "species"))
  } else {
    data_sys <- data |>
      dplyr::left_join(system_shares, by = "species")
  }
  
  data_sys <- data_sys |>
    dplyr::mutate(heads_system = heads * share) |>
    dplyr::select(-heads, -share) |>
    dplyr::rename(heads = heads_system)
  
  # 2. Join Cohort Shares
  data_cohort <- data_sys |>
    dplyr::left_join(cohort_shares, by = "species", relationship = "many-to-many")
  
  data_final <- data_cohort |>
    dplyr::mutate(heads_cohort = heads * share) |>
    dplyr::select(-heads, -share) |>
    dplyr::rename(heads = heads_cohort)
  
  return(data_final)
}

#' Estimate Energy Demand (Gross Energy) - Tier 2
#'
#' @description
#' Calculates Gross Energy (GE) intake based on IPCC 2019 Tier 2 equations (Vol 4, Chap 10).
#' It estimates Net Energy for Maintenance (NEm), Activity (NEa), and other requirements
#' to derive the total Gross Energy (GE) intake per head per day.
#'
#' @param data A dataframe with `species`, `cohort`, `heads`, `weight` (optional).
#' @param energy_coefs A dataframe with `species`, `Cfi`, `Ca_pasture`, `Cp`, etc.
#' @param weight_data A dataframe with `species`, `cohort`, `weight_kg`.
#' @param feed_data A dataframe with `species`, `DE_percent`, `UE_frac`, `REM_ratio`.
#'
#' @return Dataframe with added `GE` (MJ/day).
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- tibble::tibble(
#'     species = "Cattle",
#'     cohort = "Adult Female",
#'     heads = 100,
#'     weight = 500
#'   )
#'   energy_coefs <- tibble::tibble(
#'     species = "Cattle",
#'     Cfi = 0.3, Ca_pasture = 0.17, Cp = 0
#'   )
#'   weight_data <- tibble::tibble(
#'     species = "Cattle", cohort = "Adult Female", weight_kg = 500
#'   )
#'   feed_data <- tibble::tibble(
#'     species = "Cattle", DE_percent = 60, UE_frac = 0.04, REM_ratio = 0.5
#'   )
#'   estimate_energy_demand(data, energy_coefs, weight_data, feed_data)
#' }
estimate_energy_demand <- function(data, energy_coefs, weight_data, feed_data) {
  
  # Ensure weight column exists or join from weight_data
  if (!"weight" %in% names(data)) {
    data <- data |>
      dplyr::left_join(weight_data, by = c("species", "cohort")) |>
      dplyr::rename(weight = weight_kg)
  } else {
    # Fill missing weights
    data <- data |>
      dplyr::left_join(weight_data, by = c("species", "cohort")) |>
      dplyr::mutate(weight = dplyr::coalesce(weight, weight_kg)) |>
      dplyr::select(-weight_kg)
  }
  
  # Join energy coefficients
  data <- data |>
    dplyr::left_join(energy_coefs, by = "species")
    
  # Join feed parameters
  data <- data |>
    dplyr::left_join(feed_data, by = "species")
  
  # Calculate GE
  data <- data |>
    dplyr::mutate(
      # Net Energy for Maintenance (NEm) = Cfi * weight^0.75
      NEm = Cfi * weight^0.75,
      
      # Net Energy for Activity (NEa) = Ca * NEm
      # Simplified: using Ca_pasture from table
      NEa = Ca_pasture * NEm,
      
      # Total Net Energy (Simplified)
      NE = NEm + NEa,
      
      # Gross Energy (GE) = NE / (REM) / (DE/100)
      GE = (NE / REM_ratio) / (DE_percent / 100)
    )
  
  return(data)
}

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
    # Select only Ym from coefs
    coefs_to_join <- tier2_coefs |> dplyr::select(species, Ym)
    
    data <- data |>
      dplyr::left_join(coefs_to_join, by = "species") |>
      dplyr::mutate(
        Ym = dplyr::coalesce(Ym, 6.5), # Should ideally come from table, but robust fallback
        # EF = (GE * (Ym/100) * 365) / 55.65
        ef_enteric = (GE * (Ym / 100) * constants$days_in_year) / constants$energy_content_ch4,
        enteric_ch4_tonnes = (heads * ef_enteric) / 1000
      )
  }
  
  return(data)
}

#' Calculate Manure Management Emissions
#'
#' @description
#' Calculates manure CH4 and N2O emissions using IPCC Tier 1 or Tier 2 methods.
#' For Tier 2 CH4: Uses Volatile Solids (VS) and Methane Producing Potential (Bo) (IPCC 2019 Eq 10.23, 10.24).
#' For N2O: Uses Nitrogen Excretion (Nex) and direct emission factors.
#'
#' @param data Dataframe.
#' @param method "tier1" or "tier2".
#' @param tier1_coefs Dataframe with `species`, `ef_manure_ch4`, `ef_manure_n2o`.
#' @param tier2_coefs Dataframe with `species`, `Bo`, `MCF_temp`.
#' @param manure_params Dataframe with `species`, `Nex_kg_head_yr`, `EF_n2o_direct`.
#' @param feed_data Dataframe with `species`, `UE_frac`, `DE_percent`, `Ash_content`.
#' @param constants List with `vs_energy_content`, `ch4_density`, `n2o_n_conversion`.
#'
#' @return Dataframe with added `manure_ch4_tonnes`, `manure_n2o_tonnes`.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- tibble::tibble(
#'     species = "Cattle", heads = 100, GE = 200
#'   )
#'   # Tier 2
#'   tier2_coefs <- tibble::tibble(species = "Cattle", Bo = 0.24, MCF_temp = 0.1)
#'   manure_params <- tibble::tibble(
#'     species = "Cattle", Nex_kg_head_yr = 50, EF_n2o_direct = 0.005
#'   )
#'   feed_data <- tibble::tibble(
#'     species = "Cattle", UE_frac = 0.04, DE_percent = 60, Ash_content = 0.08
#'   )
#'   constants <- list(
#'     vs_energy_content = 18.45, days_in_year = 365, ch4_density = 0.67,
#'     n2o_n_conversion = 44/28
#'   )
#'   calculate_manure_emissions(
#'     data, method = "tier2", tier2_coefs = tier2_coefs,
#'     manure_params = manure_params, feed_data = feed_data, constants = constants
#'   )
#' }
calculate_manure_emissions <- function(data, method = "tier2", tier1_coefs = NULL, tier2_coefs = NULL, manure_params = NULL, feed_data = NULL, constants = NULL) {
  
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
    
    # Join Manure Params (Nex, EF_n2o)
    data <- data |>
      dplyr::left_join(manure_params, by = "species")
      
    # Join Feed Params if not already present (UE, DE, Ash)
    # Check if columns exist, if not join
    if (!"UE_frac" %in% names(data)) {
        data <- data |> dplyr::left_join(feed_data, by = "species")
    }

    # Join Tier 2 Methane Params (Bo, MCF)
    coefs_to_join <- tier2_coefs |> dplyr::select(species, Bo, MCF_temp)
    data <- data |>
      dplyr::left_join(coefs_to_join, by = "species")
      
    data <- data |>
      dplyr::mutate(
        # VS Calculation
        # VS = [GE * (1 - DE/100) + (UE * GE)] * [(1-Ash)/18.45] ? 
        # IPCC 2019 Eq 10.24: VS = [GE * (1 - DE/100) + (UE * GE)] / 18.45  (Ash ignored in simplified or handled elsewhere?)
        # Actually Eq 10.24 is: VS = [GE * (1 - DE%/100) + (UE * GE)] * [ (1 - Ash/100) / 18.45 ]
        # We'll use the Ash content from feed_data
        
        VS = (GE * (1 - DE_percent/100) + (UE_frac * GE)) * ((1 - Ash_content) / constants$vs_energy_content),
        
        # Manure CH4
        # EF = VS * 365 * Bo * 0.67 * MCF
        MCF = dplyr::coalesce(MCF_temp, 0.10),
        Bo = dplyr::coalesce(Bo, 0.24),
        
        ef_manure_ch4 = VS * constants$days_in_year * Bo * constants$ch4_density * MCF,
        manure_ch4_tonnes = (heads * ef_manure_ch4) / 1000,
        
        # Manure N2O
        # Direct N2O = Nex * EF * (44/28)
        ef_manure_n2o = Nex_kg_head_yr * EF_n2o_direct * constants$n2o_n_conversion,
        manure_n2o_tonnes = (heads * ef_manure_n2o) / 1000
      )
  }
  
  return(data)
}

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
    
    # 2. Energy
    d_energy <- estimate_energy_demand(d_cohorts, ipcc_tier2_energy, livestock_weights, feed_params)
    
    # 3. Emissions
    t2_data <- d_energy |>
      calculate_enteric_ch4(method = "tier2", tier2_coefs = ipcc_tier2_methane, constants = livestock_constants) |>
      calculate_manure_emissions(method = "tier2", tier2_coefs = ipcc_tier2_methane, manure_params = manure_params, feed_data = feed_params, constants = livestock_constants) |>
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
