#' Calculate Enteric Methane Emissions
#'
#' @description
#' Calculates enteric CH4 emissions using IPCC Tier 1 (fixed Emission Factors) or
#' Tier 2 (based on Gross Energy intake and Methane Conversion Factor Ym).
#' Supports granular method selection (IPCC 2006 vs 2019).
#' 
#' All coefficients loaded from internal package data.
#' 
#' **Missing Data Behavior**:
#' - **Tier 1**: Requires `species` and `heads`. Optional: `iso3` for regional EFs (otherwise uses Global).
#' - **Tier 2**: Requires `GE` (Gross Energy) from energy calculation. 
#'   - If `diet_quality` is missing, uses fallback Ym (6.5%) and tracks in Method column.
#'   - If `GE` is NA, returns NA for enteric CH4.
#'
#' @param data Dataframe with `species`, `heads`, and for Tier 2: `GE`.
#'   Optional: `iso3` for regional Tier 1 factors, `diet_quality` for Tier 2 Ym.
#' @param method "tier1" or "tier2".
#' @param tier1_version "2019" or "2006" (default "2019").
#' @param ym_method "ipcc2019" (dynamic) or "ipcc2006" (fixed) (default "ipcc2019").
#' @param ym_interpolation "continuous" (default) or "categorical" for Ym calculation.
#'
#' @return Dataframe with added `enteric_ch4_tonnes` and `Method_Enteric` tracking column.
#' @export
calculate_enteric_ch4 <- function(data, method = "tier2", tier1_version = "2019", ym_method = "ipcc2019", ym_interpolation = "continuous") {
  
  # Load coefficient tables
  load_coefs <- function() {
    if (!exists("ipcc_2019_enteric_ef_cattle", envir = parent.frame(2))) {
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
      Method_Enteric = dplyr::case_when(
        method == "tier1" & tier1_version == "2006" ~ "IPCC_2006_Tier1",
        method == "tier1" & tier1_version == "2019" ~ "IPCC_2019_Tier1",
        method == "tier2" & ym_method == "ipcc2006" ~ "IPCC_2006_Tier2_FixedYm",
        method == "tier2" & ym_method == "ipcc2019" ~ "IPCC_2019_Tier2_DynamicYm",
        TRUE ~ "Unknown"
      )
    )
  
  if (method == "tier1") {
    return(calc_enteric_ch4_tier1(data, version = tier1_version))
  } else {
    return(calc_enteric_ch4_tier2(data, ym_method = ym_method, ym_interpolation = ym_interpolation))
  }
}

#' Calculate Enteric CH4 - Tier 1
#' @export
calc_enteric_ch4_tier1 <- function(data, version = "2019") {
  
  # Select EF table based on version
  if (version == "2006") {
    tier1_ef <- ipcc_2006_enteric_ef
  } else {
    # 2019 Combined
    tier1_ef <- dplyr::bind_rows(
      ipcc_2019_enteric_ef_cattle |>
        dplyr::select(region, category, ef_kg_head_yr),
      ipcc_2019_enteric_ef_other |>
        dplyr::mutate(region = "Global") |>
        dplyr::select(region, category, ef_kg_head_yr)
    )
  }
  
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
        tier1_ef,
        by = c("gleam_region" = "region", "species" = "category")
      ) |>
      # Fallback to Global
      dplyr::left_join(
        tier1_ef |> dplyr::filter(region == "Global") |> dplyr::select(-region),
        by = c("species" = "category"),
        suffix = c("", "_global")
      ) |>
      dplyr::mutate(
        ef_enteric = dplyr::coalesce(ef_kg_head_yr, ef_kg_head_yr_global, 0),
        enteric_ch4_tonnes = (heads * ef_enteric) / 1000
      ) |>
      dplyr::select(-ef_kg_head_yr, -ef_kg_head_yr_global, -ef_enteric)
  } else {
    # No region info, use global
    data <- data |>
      dplyr::left_join(
        tier1_ef |> dplyr::filter(region == "Global") |> dplyr::select(-region),
        by = c("species" = "category")
      ) |>
      dplyr::mutate(
        ef_enteric = dplyr::coalesce(ef_kg_head_yr, 0),
        enteric_ch4_tonnes = (heads * ef_enteric) / 1000
      ) |>
      dplyr::select(-ef_kg_head_yr, -ef_enteric)
  }
  
  return(data)
}

#' Calculate Enteric CH4 - Tier 2
#' 
#' @param data Input dataframe
#' @param ym_method Method for Ym calculation ("ipcc2019" or "ipcc2006")
#' @param ym_interpolation Interpolation method for Ym values:
#'   - "continuous" (DEFAULT): Linear interpolation based on DE% for smooth time series.
#'     **DEVIATION FROM IPCC**: This approach interpolates between IPCC categorical values
#'     to avoid discontinuous jumps when diet quality changes gradually.
#'   - "categorical": Strict IPCC categorical approach (High/Medium/Low diet quality).
#' @export
calc_enteric_ch4_tier2 <- function(data, ym_method = "ipcc2019", ym_interpolation = "continuous") {
  
  # Prepare Ym table (pivot to wide manually to avoid tidyr dependency)
  ym_high <- ipcc_tier2_ym_values |> 
    dplyr::filter(diet_quality == "High") |> 
    dplyr::select(category, ym_percent) |> 
    dplyr::rename(ym_high = ym_percent)
  
  ym_medium <- ipcc_tier2_ym_values |> 
    dplyr::filter(diet_quality == "Medium") |> 
    dplyr::select(category, ym_percent) |> 
    dplyr::rename(ym_medium = ym_percent)
  
  ym_low <- ipcc_tier2_ym_values |> 
    dplyr::filter(diet_quality == "Low") |> 
    dplyr::select(category, ym_percent) |> 
    dplyr::rename(ym_low = ym_percent)
  
  ym_wide <- ym_high |>
    dplyr::full_join(ym_medium, by = "category") |>
    dplyr::full_join(ym_low, by = "category") |>
    dplyr::rename(species_gen = category)
  
  # Join Ym values
  data <- data |>
    dplyr::left_join(
      ym_wide,
      by = "species_gen"
    )
  
  # Calculate Ym
  if (ym_method == "ipcc2019") {
    # Dynamic Ym based on DE and NDF (IPCC 2019 Eq 10.21)
    # Requires DE_percent and NDF_percent (from feed_characteristics)
    # If not present, fallback to table values
    
    # Ensure feed characteristics are joined if not already
    if (!"de_percent" %in% names(data) & "diet_quality" %in% names(data)) {
      data <- data |>
        dplyr::left_join(
          feed_characteristics |> dplyr::select(diet_quality, de_percent, ndf_percent),
          by = "diet_quality"
        )
    }
    
    if (ym_interpolation == "continuous") {
      # CONTINUOUS INTERPOLATION (NEW DEFAULT)
      # Linear interpolation between Ym values based on DE%
      # High: 75% DE, Medium: 65% DE, Low: 55% DE
      data <- data |>
        dplyr::mutate(
          Ym = dplyr::case_when(
            # If DE% >= 70: interpolate between Medium and High
            de_percent >= 70 ~ ym_medium + (ym_high - ym_medium) * ((de_percent - 65) / (75 - 65)),
            # If DE% between 60-70: interpolate between Low and Medium
            de_percent >= 60 & de_percent < 70 ~ ym_low + (ym_medium - ym_low) * ((de_percent - 55) / (65 - 55)),
            # If DE% < 60: use Low value (floor)
            de_percent < 60 ~ ym_low,
            # Fallback if DE% is NA but diet_quality is available
            diet_quality == "High" ~ ym_high,
            diet_quality == "Medium" ~ ym_medium,
            diet_quality == "Low" ~ ym_low,
            TRUE ~ (ym_high + ym_low) / 2
          ),
          Method_Enteric = dplyr::if_else(
            !is.na(de_percent),
            paste0(Method_Enteric, "; Ym_continuous_interp"),
            Method_Enteric
          )
        )
    } else {
      # CATEGORICAL APPROACH (IPCC STRICT)
      data <- data |>
        dplyr::mutate(
          Ym = dplyr::case_when(
            diet_quality == "High" ~ ym_high,
            diet_quality == "Low" ~ ym_low,
            diet_quality == "Medium" ~ ym_medium,
            TRUE ~ (ym_high + ym_low) / 2
          ),
          Method_Enteric = paste0(Method_Enteric, "; Ym_categorical")
        )
    }
    
    # Apply fallback if Ym is still NA
    ym_fallback <- fallback_constants$value[fallback_constants$parameter == "ym_default"]
    data <- data |>
      dplyr::mutate(
        Ym_was_na = is.na(Ym),
        Ym = dplyr::coalesce(Ym, ym_fallback),
        Method_Enteric = dplyr::if_else(
          Ym_was_na,
          paste0(Method_Enteric, "; Ym_fallback_", ym_fallback, "%"),
          Method_Enteric
        )
      ) |>
      dplyr::select(-Ym_was_na)
  } else {
    # Fixed Ym (IPCC 2006 style - mostly fixed per species)
    # We'll use the average of high/low as a fixed value
    ym_fallback <- fallback_constants$value[fallback_constants$parameter == "ym_default"]
    data <- data |>
      dplyr::mutate(
        Ym = (ym_high + ym_low) / 2,
        Ym_was_na = is.na(Ym),
        Ym = dplyr::coalesce(Ym, ym_fallback),
        Method_Enteric = dplyr::if_else(
          Ym_was_na,
          paste0(Method_Enteric, "; Ym_fallback_", ym_fallback, "%"),
          Method_Enteric
        )
      ) |>
      dplyr::select(-Ym_was_na)
  }
  
  # Calculate Emissions
  data <- data |>
    dplyr::mutate(
      # Eq 10.21: CH4 (kg/yr) = (GE * (Ym/100) * 365) / 55.65
      ef_enteric_tier2 = (GE * (Ym / 100) * livestock_constants$days_in_year) / livestock_constants$energy_content_ch4_mj_kg,
      enteric_ch4_tonnes = (heads * ef_enteric_tier2) / 1000
    )
  
  return(data)
}
