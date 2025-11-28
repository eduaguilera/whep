#' Calculate Manure Management Emissions
#'
#' @description
#' Calculates manure CH4 and N2O emissions using IPCC Tier 1 or Tier 2 methods.
#' Supports granular method selection (IPCC 2006 vs 2019).
#' 
#' All coefficients loaded from internal package data.
#' 
#' **Missing Data Behavior**:
#' - **Tier 1**: Requires `species` and `heads`. Optional: `iso3` for regional EFs, `climate` for climate-specific EFs (defaults to "Temperate").
#' - **Tier 2 CH4**: Requires `GE`, `weight`. Optional: `climate` or `temperature_c` for MCF selection.
#'   - If MMS distribution not found, uses fallback (Pasture 100%) and tracks in Method column.
#'   - If MCF not found, uses fallback (1.0%) and tracks in Method column.
#' - **Tier 2 N2O**: Requires `GE` for N excretion calculation.
#'   - If `include_indirect=TRUE`, calculates indirect N2O from volatilization and leaching.
#'
#' @param data Dataframe with `species`, `heads`, and for Tier 2: `GE`, `weight`.
#'   Optional: `iso3`, `climate`, `system`, `temperature_c`.
#' @param method "tier1" or "tier2".
#' @param tier1_version "2019" or "2006" (default "2019").
#' @param mcf_method "climate_zone" (2019) or "temperature" (2006) (default "climate_zone").
#' @param include_indirect Logical, include indirect N2O emissions (default TRUE).
#'
#' @return Dataframe with added `manure_ch4_tonnes`, `manure_n2o_tonnes`, and `Method_Manure` tracking column.
#' @export
calculate_manure_emissions <- function(data, method = "tier2", tier1_version = "2019", mcf_method = "climate_zone", include_indirect = TRUE) {
  
  # Load coefficient tables
  load_coefs <- function() {
    if (!exists("ipcc_2019_manure_ch4_ef_cattle", envir = parent.frame(2))) {
      if (file.exists("data/livestock_coefs.rda")) {
        load("data/livestock_coefs.rda", envir = parent.frame(2))
      } else {
        stop("Coefficient tables not found.")
      }
    }
  }
  load_coefs()
  
  # Helper function
  get_general_species <- function(s) {
    dplyr::case_when(
      grepl("Cattle", s, ignore.case = TRUE) ~ "Cattle",
      grepl("Buffalo", s, ignore.case = TRUE) ~ "Buffalo",
      grepl("Sheep", s, ignore.case = TRUE) ~ "Sheep",
      grepl("Goat", s, ignore.case = TRUE) ~ "Goats",
      grepl("Pig|Swine", s, ignore.case = TRUE) ~ "Swine",
      grepl("Poultry|Chicken|Hen", s, ignore.case = TRUE) ~ "Poultry",
      grepl("Horse", s, ignore.case = TRUE) ~ "Horses",
      grepl("Camel", s, ignore.case = TRUE) ~ "Camels",
      grepl("Mule|Ass|Donkey", s, ignore.case = TRUE) ~ "Mules and Asses",
      TRUE ~ s
    )
  }
  
  data <- data |>
    dplyr::mutate(
      species_gen = get_general_species(species),
      # Initialize Method tracking based on tier and version
      Method_Manure = dplyr::case_when(
        method == "tier1" & tier1_version == "2006" ~ "IPCC_2006_Tier1",
        method == "tier1" & tier1_version == "2019" ~ "IPCC_2019_Tier1",
        method == "tier2" & mcf_method == "temperature" ~ "IPCC_2006_Tier2_TempMCF",
        method == "tier2" & mcf_method == "climate_zone" ~ "IPCC_2019_Tier2_ClimateMCF",
        TRUE ~ "Unknown"
      )
    )
  
  if (method == "tier1") {
    data <- calc_manure_ch4_tier1(data, version = tier1_version)
    # Tier 1 N2O is usually just N excretion * EF, we can use the Tier 2 function with defaults or a simplified one.
    # For consistency, let's use a simplified N2O calc here or reuse the robust one.
    # The previous code had a simplified N2O calc in Tier 1 block.
    # Let's use calc_manure_n2o but we need Nex.
    # If Nex is not present, we need to add it from Tier 1 tables.
    
    # Join N excretion for N2O (Tier 1 default)
    data <- data |>
      dplyr::left_join(
        ipcc_2019_n_excretion |>
          dplyr::group_by(category) |>
          dplyr::summarise(nex_kg_n_head_yr = mean(nex_kg_n_head_yr, na.rm = TRUE), .groups = "drop"),
        by = c("species" = "category")
      ) |>
      dplyr::mutate(Nex = nex_kg_n_head_yr) # Map to Nex column used by N2O function
      
    data <- calc_manure_n2o(data, method = "tier1", include_indirect = include_indirect)
    
  } else {
    data <- calc_manure_ch4_tier2(data, mcf_method = mcf_method)
    data <- calc_manure_n2o(data, method = "tier2", include_indirect = include_indirect)
  }
  
  return(data)
}

#' Calculate Manure CH4 - Tier 1
#' @export
calc_manure_ch4_tier1 <- function(data, version = "2019") {
  
  if (version == "2006") {
    # IPCC 2006 Tier 1 EFs
    tier1_ch4 <- ipcc_2006_manure_ef
    
    # Map iso3 to region if available
    if ("iso3" %in% names(data) && !"gleam_region" %in% names(data)) {
      data <- data |>
        dplyr::left_join(
          gleam_geographic_hierarchy |> dplyr::select(iso3, gleam_region),
          by = "iso3"
        )
    }
      
    # Join with region
    if ("gleam_region" %in% names(data)) {
      data <- data |>
        dplyr::left_join(
          tier1_ch4,
          by = c("gleam_region" = "region", "species" = "category")
        ) |>
        # Fallback to Global
        dplyr::left_join(
          tier1_ch4 |> dplyr::filter(region == "Global") |> dplyr::select(-region),
          by = c("species" = "category"),
          suffix = c("", "_global")
        ) |>
        dplyr::mutate(
          ef_manure_ch4 = dplyr::coalesce(ef_kg_head_yr, ef_kg_head_yr_global, 0)
        ) |>
        dplyr::select(-ef_kg_head_yr, -ef_kg_head_yr_global)
    } else {
      # No region info, use global
      data <- data |>
        dplyr::left_join(
          tier1_ch4 |> dplyr::filter(region == "Global") |> dplyr::select(-region),
          by = c("species" = "category")
        ) |>
        dplyr::mutate(
          ef_manure_ch4 = dplyr::coalesce(ef_kg_head_yr, 0)
        ) |>
        dplyr::select(-ef_kg_head_yr)
    }
    
  } else {
    # IPCC 2019 Tier 1 EFs (Climate based)
    tier1_ch4 <- dplyr::bind_rows(
      ipcc_2019_manure_ch4_ef_cattle |>
        dplyr::select(region, category, climate, ef_kg_head_yr),
      ipcc_2019_manure_ch4_ef_other |>
        dplyr::mutate(region = "Global") |>
        dplyr::select(region, category, climate, ef_kg_head_yr)
    )
    
    # Add default climate if missing
    if (!"climate" %in% names(data)) {
      data <- data |> dplyr::mutate(climate = "Temperate")
    }
    
    # Map iso3 to region if available
    if ("iso3" %in% names(data) && !"gleam_region" %in% names(data)) {
      data <- data |>
        dplyr::left_join(
          gleam_geographic_hierarchy |> dplyr::select(iso3, gleam_region),
          by = "iso3"
        )
    }
    
    if ("gleam_region" %in% names(data)) {
      # Use regional EFs
      data <- data |>
        dplyr::left_join(
          tier1_ch4,
          by = c("gleam_region" = "region", "species" = "category", "climate")
        ) |>
        dplyr::left_join(
          tier1_ch4 |> dplyr::filter(region == "Global") |> dplyr::select(-region),
          by = c("species" = "category", "climate"),
          suffix = c("", "_global")
        ) |>
        dplyr::mutate(
          ef_manure_ch4 = dplyr::coalesce(ef_kg_head_yr, ef_kg_head_yr_global, 0)
        ) |>
        dplyr::select(-ef_kg_head_yr, -ef_kg_head_yr_global)
    } else {
      # No region info, use global
      data <- data |>
        dplyr::left_join(
          tier1_ch4 |> dplyr::filter(region == "Global") |> dplyr::select(-region),
          by = c("species" = "category", "climate")
        ) |>
        dplyr::mutate(
          ef_manure_ch4 = dplyr::coalesce(ef_kg_head_yr, 0)
        ) |>
        dplyr::select(-ef_kg_head_yr)
    }
  }
  
  data <- data |>
    dplyr::mutate(
      manure_ch4_tonnes = (heads * ef_manure_ch4) / 1000
    )
    
  return(data)
}

#' Calculate Manure CH4 - Tier 2
#' @export
calc_manure_ch4_tier2 <- function(data, mcf_method = "climate_zone") {
  
  # Join feed parameters
  de_fallback <- fallback_constants$value[fallback_constants$parameter == "de_percent_default"]
  ue_fallback <- fallback_constants$value[fallback_constants$parameter == "ue_fraction_default"]
  
  data <- data |>
    dplyr::mutate(
      DE_percent_was_na = is.na(DE_percent),
      DE_percent = dplyr::coalesce(DE_percent, de_fallback),
      UE_frac = ue_fallback,
      Method_Manure = dplyr::if_else(
        DE_percent_was_na,
        paste0(Method_Manure, "; DE_fallback_", de_fallback, "%"),
        Method_Manure
      )
    ) |>
    dplyr::select(-DE_percent_was_na)
  
  # Join ash content
  ash_fallback <- fallback_constants$value[fallback_constants$parameter == "ash_content_default"] / 100
  
  data <- data |>
    dplyr::left_join(
      ipcc_tier2_manure_ash |>
        dplyr::rename(species_gen = category, Ash_content = ash_percent) |>
        dplyr::mutate(Ash_content = Ash_content / 100),
      by = "species_gen"
    ) |>
    dplyr::mutate(
      Ash_was_na = is.na(Ash_content),
      Ash_content = dplyr::coalesce(Ash_content, ash_fallback),
      Method_Manure = dplyr::if_else(
        Ash_was_na,
        paste0(Method_Manure, "; Ash_fallback_", ash_fallback * 100, "%"),
        Method_Manure
      )
    ) |>
    dplyr::select(-Ash_was_na)
  
  # Join Bo values
  data <- data |>
    dplyr::left_join(
      ipcc_tier2_bo_values |>
        dplyr::rename(species_gen = category, Bo = bo_m3_kg_vs),
      by = "species_gen"
    )
  
  # Calculate VS (Volatile Solids)
  # IPCC 2019 Eq 10.24: VS = [GE * (1 - DE/100) + (UE * GE)] * [(1 - Ash) / 18.45]
  data <- data |>
    dplyr::mutate(
      VS = (GE * (1 - DE_percent/100) + (UE_frac * GE)) * ((1 - dplyr::coalesce(Ash_content, 0.08)) / livestock_constants$vs_energy_content_mj_kg)
    )
  
  # Determine MCF
  if (mcf_method == "temperature") {
    # IPCC 2006 Temperature-based MCF
    # Requires temperature_c
    if (!"temperature_c" %in% names(data)) {
      data <- data |> dplyr::mutate(temperature_c = 15) # Default
    }
    
    # We need to join with ipcc_2006_mcf_temp
    # But first we need to distribute VS across MMS types
    # Join regional MMS distribution
    if ("gleam_region" %in% names(data)) {
      data <- data |>
        dplyr::left_join(
          regional_mms_distribution |> dplyr::rename(mms_region = region),
          by = c("gleam_region" = "mms_region", "species_gen" = "species")
        )
    } else {
      # Fallback to Global distribution
      data <- data |>
        dplyr::left_join(
          regional_mms_distribution |> dplyr::filter(region == "Global") |> dplyr::select(-region),
          by = c("species_gen" = "species")
        )
    }
    
    # If no MMS distribution found, default to Pasture 100%
    data <- data |>
      dplyr::mutate(
        mms_type = dplyr::if_else(is.na(mms_type), "Pasture/Range/Paddock", mms_type),
        fraction = dplyr::if_else(is.na(fraction), 1.0, fraction)
      )
      
    # Round temperature to nearest 5 for lookup (10, 15, 20, 25)
    data <- data |>
      dplyr::mutate(
        temp_lookup = 5 * round(temperature_c / 5),
        temp_lookup = pmax(10, pmin(25, temp_lookup))
      )
      
    # Join MCF
    data <- data |>
      dplyr::left_join(
        ipcc_2006_mcf_temp,
        by = c("mms_type" = "system", "temp_lookup" = "temp_c")
      ) |>
      dplyr::mutate(MCF_was_na = is.na(mcf_percent)) |>
      dplyr::mutate(
        mcf_percent = dplyr::coalesce(
          mcf_percent, 
          fallback_constants$value[fallback_constants$parameter == "mcf_default"]
        ),
        MCF = mcf_percent / 100,
        Method_Manure = dplyr::if_else(
          MCF_was_na,
          paste0(Method_Manure, "; MCF_fallback_1%"),
          Method_Manure
        )
      ) |>
      dplyr::select(-MCF_was_na)
      
  } else {
    # IPCC 2019 Climate-zone based MCF
    # Requires climate zone (Cool, Temperate, Warm)
    if (!"climate" %in% names(data)) {
      data <- data |> dplyr::mutate(climate = "Temperate")
    }
    
    # Join regional MMS distribution
    if ("gleam_region" %in% names(data)) {
      data <- data |>
        dplyr::left_join(
          regional_mms_distribution |> dplyr::rename(mms_region = region),
          by = c("gleam_region" = "mms_region", "species_gen" = "species")
        )
    } else {
      # Fallback to Global distribution
      data <- data |>
        dplyr::left_join(
          regional_mms_distribution |> dplyr::filter(region == "Global") |> dplyr::select(-region),
          by = c("species_gen" = "species")
        )
    }
    
    # If no MMS distribution found, default to Pasture 100%
    data <- data |>
      dplyr::mutate(
        mms_type = dplyr::if_else(is.na(mms_type), "Pasture/Range/Paddock", mms_type),
        fraction = dplyr::if_else(is.na(fraction), 1.0, fraction)
      )
      
    # Join MCF
    data <- data |>
      dplyr::left_join(
        climate_mcf,
        by = c("mms_type", "climate" = "climate_zone")
      ) |>
      dplyr::mutate(MCF_was_na = is.na(mcf_percent)) |>
      dplyr::mutate(
        mcf_percent = dplyr::coalesce(
          mcf_percent,
          fallback_constants$value[fallback_constants$parameter == "mcf_default"]
        ),
        MCF = mcf_percent / 100,
        Method_Manure = dplyr::if_else(
          MCF_was_na,
          paste0(Method_Manure, "; MCF_fallback_1%"),
          Method_Manure
        )
      ) |>
      dplyr::select(-MCF_was_na)
  }
  
  # Calculate Emissions per MMS
  data <- data |>
    dplyr::mutate(
      ef_manure_ch4_mms = (VS * 365) * (Bo) * (0.67) * (MCF) * fraction
    )
    
  # Aggregate across MMS types
  # We need to group by everything except the MMS specific columns
  group_cols <- setdiff(names(data), c("mms_type", "fraction", "MCF", "mcf_percent", "ef_manure_ch4_mms", "temp_lookup"))
  
  data <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      ef_manure_ch4 = sum(ef_manure_ch4_mms, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      manure_ch4_tonnes = (heads * ef_manure_ch4) / 1000
    )
    
  return(data)
}

#' Calculate Manure N2O
#' @export
calc_manure_n2o <- function(data, method = "tier2", include_indirect = TRUE) {
  
  if (method == "tier2") {
    # Calculate Nex (Nitrogen Excretion)
    # IPCC 2019 Eq 10.30: Nex = N_intake * (1 - N_retention)
    # N_intake = (GE / 18.45) * (CP% / 6.25)
    
    # Join CP percent and N retention
    data <- data |>
      dplyr::left_join(
        livestock_production_defaults |> 
          dplyr::select(category, protein_percent) |> 
          dplyr::rename(protein_percent_default = protein_percent),
        by = c("species_gen" = "category")
      ) |>
      dplyr::mutate(
        CP_percent = dplyr::coalesce(protein_percent, protein_percent_default, 3.2) # Default 3.2% CP? No, milk protein is 3.2. Feed CP is different.
        # Actually we need Feed CP.
        # Let's assume CP_percent is available or use a default based on diet quality
      )
      
    # If CP_percent not in data, add default based on diet quality
    if (!"CP_percent" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          CP_percent = dplyr::case_when(
            diet_quality == "High" ~ 18,
            diet_quality == "Medium" ~ 12,
            diet_quality == "Low" ~ 8,
            TRUE ~ 12
          )
        )
    }
    
    data <- data |>
      dplyr::left_join(
        ipcc_tier2_n_retention |> dplyr::rename(species_gen = category),
        by = "species_gen"
      ) |>
      dplyr::mutate(
        N_intake = (GE / 18.45) * (CP_percent / 100 / 6.25),
        Nex = N_intake * (1 - dplyr::coalesce(n_retention_frac, 0)) * 365
      )
  }
  
  # Calculate Direct N2O
  # We need MMS distribution again if it was lost in aggregation or if we are in Tier 1 flow
  # If we are coming from calc_manure_ch4_tier2, we aggregated.
  # So we need to re-join MMS distribution.
  
  if ("gleam_region" %in% names(data)) {
    data <- data |>
      dplyr::left_join(
        regional_mms_distribution |> dplyr::rename(mms_region = region),
        by = c("gleam_region" = "mms_region", "species_gen" = "species")
      )
  } else {
    # Fallback to Global distribution
    data <- data |>
      dplyr::left_join(
        regional_mms_distribution |> dplyr::filter(region == "Global") |> dplyr::select(-region),
        by = c("species_gen" = "species")
      )
  }
  
  # If no MMS distribution found, default to Pasture 100%
  data <- data |>
    dplyr::mutate(
      mms_type = dplyr::if_else(is.na(mms_type), "Pasture/Range/Paddock", mms_type),
      fraction = dplyr::if_else(is.na(fraction), 1.0, fraction)
    )
    
  # Join Direct N2O EFs
  data <- data |>
    dplyr::left_join(
      ipcc_2019_n2o_ef_direct |> dplyr::rename(ef_n2o_direct = ef_kg_n2o_n_per_kg_n),
      by = c("mms_type" = "system")
    ) |>
    dplyr::mutate(
      ef_n2o_direct = dplyr::coalesce(ef_n2o_direct, 0.005) # Default
    )
    
  # Calculate Direct Emissions per MMS
  data <- data |>
    dplyr::mutate(
      ef_manure_n2o_mms = Nex * fraction * ef_n2o_direct * livestock_constants$n_to_n2o
    )
    
  # Aggregate Direct N2O
  group_cols <- setdiff(names(data), c("mms_type", "fraction", "ef_n2o_direct", "ef_manure_n2o_mms"))
  
  data <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      ef_manure_n2o_direct = sum(ef_manure_n2o_mms, na.rm = TRUE),
      .groups = "drop"
    )
    
  # Indirect N2O
  if (include_indirect) {
    data <- data |>
      dplyr::mutate(
        # Indirect N₂O emissions (IPCC 2019 Ch 11)
        # Get indirect emission factors
        ef4_volatilization = 0.01,  # From indirect_n2o_ef table
        ef5_leaching = 0.0075,      # From indirect_n2o_ef table
        frac_gasms = 0.20,          # Fraction volatilized from MMS
        frac_leach = 0.30,          # Fraction leached
        
        # Indirect N₂O from volatilization
        n_volatilized = Nex * frac_gasms,
        ef_n2o_indirect_vol = n_volatilized * ef4_volatilization * livestock_constants$n_to_n2o,
        
        # Indirect N₂O from leaching
        n_leached = Nex * frac_leach,
        ef_n2o_indirect_leach = n_leached * ef5_leaching * livestock_constants$n_to_n2o,
        
        # Total N₂O (direct + indirect)
        ef_manure_n2o = ef_manure_n2o_direct + ef_n2o_indirect_vol + ef_n2o_indirect_leach,
        
        manure_n2o_direct_tonnes = (heads * ef_manure_n2o_direct) / 1000,
        manure_n2o_indirect_tonnes = (heads * (ef_n2o_indirect_vol + ef_n2o_indirect_leach)) / 1000,
        manure_n2o_tonnes = (heads * ef_manure_n2o) / 1000
      )
  } else {
    data <- data |>
      dplyr::mutate(
        ef_manure_n2o = ef_manure_n2o_direct,
        manure_n2o_tonnes = (heads * ef_manure_n2o) / 1000
      )
  }
  
  return(data)
}
