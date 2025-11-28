#' Estimate Energy Demand (Gross Energy) - Tier 2
#'
#' @description
#' Calculates Gross Energy (GE) intake based on IPCC 2019 Tier 2 equations (Vol 4, Chap 10).
#' It estimates Net Energy for Maintenance (NEm), Activity (NEa), Lactation (NEl),
#' Work (NEwork), Pregnancy (NEp), and Growth (NEg) to derive the total Gross Energy (GE).
#' 
#' All coefficients are loaded from internal package data tables.
#' 
#' **Missing Data Behavior**:
#' - `weight`: If missing, uses GLEAM regional defaults (or global if region unavailable). Returns NA if no default exists.
#' - `temperature_c`: If missing, assumes thermoneutral (15°C). Method column will indicate "temp_assumed".
#' - `diet_quality`: If missing, returns NA for GE (cannot calculate without feed characteristics).
#' - `grazing_distance_km`: If missing, assumes 0 km (no grazing energy cost).
#' - `milk_yield_kg_day`, `work_hours_day`, `pregnant_fraction`, `weight_gain_kg_day`, `wool_production_kg_yr`: If missing, assumes 0 (no energy requirement for that component).
#' - `system`: If missing, assumes "Pasture" for activity coefficient.
#'
#' @param data A dataframe with `species`, `cohort`, `heads`, and optionally `iso3`.
#'   Optional production columns: `weight`, `milk_yield_kg_day`, `fat_percent`, 
#'   `weight_gain_kg_day`, `work_hours_day`, `pregnant_fraction`, `temperature_c`, `diet_quality`, `grazing_distance_km`.
#' @param method Method for calculation (default "ipcc2019").
#'
#' @return Dataframe with added `GE` (MJ/day), intermediate NE components, and `Method_Energy` tracking column.
#' @export
estimate_energy_demand <- function(data, method = "ipcc2019") {
  
  # Load coefficient tables from package data
  load_coefs <- function() {
    if (!exists("ipcc_tier2_energy_coefs", envir = parent.frame(2))) {
      if (file.exists("data/livestock_coefs.rda")) {
        load("data/livestock_coefs.rda", envir = parent.frame(2))
      } else {
        stop("Coefficient tables not found. Please ensure data/livestock_coefs.rda exists.")
      }
    }
  }
  load_coefs()
  
  # Helper function to map specific species to general categories
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
  
  # Add general species column for joining
  data <- data |>
    dplyr::mutate(
      species_gen = get_general_species(species),
      # Initialize Method tracking
      Method_Energy = "IPCC_2019_Tier2"
    )
  
  # 1. Join animal weights
  # Try regional mapping if iso3 is available
  if ("iso3" %in% names(data)) {
    # Map iso3 to GLEAM region
    data <- data |>
      dplyr::left_join(
        gleam_geographic_hierarchy |> dplyr::select(iso3, gleam_region),
        by = "iso3"
      )
    
    # Join weights with regional preference
    data <- data |>
      dplyr::left_join(
        gleam_animal_weights |>
          dplyr::group_by(region, species, cohort) |>
          dplyr::summarise(weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop"),
        by = c("gleam_region" = "region", "species_gen" = "species", "cohort")
      ) |>
      # Fallback to global average if regional not found
      dplyr::left_join(
        gleam_animal_weights |>
          dplyr::filter(region == "Global") |>
          dplyr::group_by(species, cohort) |>
          dplyr::summarise(weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop") |>
          dplyr::rename(weight_kg_global = weight_kg),
        by = c("species_gen" = "species", "cohort"),
        suffix = c("_regional", "_global2")
      ) |>
      dplyr::mutate(
        weight = dplyr::coalesce(weight_kg, weight_kg_global)
      ) |>
      dplyr::select(-dplyr::any_of(c("weight_kg", "weight_kg_global")))
  } else {
    # No iso3, use global average
    data <- data |>
      dplyr::left_join(
        gleam_animal_weights |>
          dplyr::group_by(species, cohort) |>
          dplyr::summarise(weight_kg = mean(weight_kg, na.rm = TRUE), .groups = "drop"),
        by = c("species_gen" = "species", "cohort")
      ) |>
      dplyr::mutate(weight = dplyr::coalesce(weight, weight_kg)) |>
      dplyr::select(-weight_kg)
  }
  
  # 2. Join energy coefficients
  data <- data |>
    dplyr::left_join(
      ipcc_tier2_energy_coefs |> 
        dplyr::rename(species_gen = category),
      by = "species_gen"
    )
  
  # Add missing columns if they don't exist (so join creates suffixes)
  if (!"milk_yield_kg_day" %in% names(data)) data <- data |> dplyr::mutate(milk_yield_kg_day = NA_real_)
  if (!"fat_percent" %in% names(data)) data <- data |> dplyr::mutate(fat_percent = NA_real_)
  if (!"protein_percent" %in% names(data)) data <- data |> dplyr::mutate(protein_percent = NA_real_)
  if (!"lactose_percent" %in% names(data)) data <- data |> dplyr::mutate(lactose_percent = NA_real_)
  if (!"weight_gain_kg_day" %in% names(data)) data <- data |> dplyr::mutate(weight_gain_kg_day = NA_real_)
  if (!"work_hours_day" %in% names(data)) data <- data |> dplyr::mutate(work_hours_day = NA_real_)
  if (!"pregnant_fraction" %in% names(data)) data <- data |> dplyr::mutate(pregnant_fraction = NA_real_)
  if (!"wool_production_kg_yr" %in% names(data)) data <- data |> dplyr::mutate(wool_production_kg_yr = NA_real_)

  # 3. Join production defaults for missing values
  data <- data |>
    dplyr::left_join(
      livestock_production_defaults |>
        dplyr::rename(species_gen = category),
      by = "species_gen",
      suffix = c("", "_default")
    ) |>
    dplyr::mutate(
      milk_yield_kg_day = dplyr::coalesce(milk_yield_kg_day, 0),
      fat_percent = dplyr::coalesce(fat_percent, fat_percent_default, 4.0),
      protein_percent = dplyr::coalesce(protein_percent, protein_percent_default, 3.2),
      lactose_percent = dplyr::coalesce(lactose_percent, lactose_percent_default, 4.85),
      weight_gain_kg_day = dplyr::coalesce(weight_gain_kg_day, weight_gain_kg_day_default, 0),
      work_hours_day = dplyr::coalesce(work_hours_day, work_hours_day_default, 0),
      pregnant_fraction = dplyr::coalesce(pregnant_fraction, pregnant_fraction_default, 0)
    ) |>
    dplyr::select(-dplyr::ends_with("_default"))
  
  # 4. Determine activity coefficient based on system
  # Default to pasture if system not specified
  if (!"system" %in% names(data)) {
    data <- data |> dplyr::mutate(Ca = ca_pasture)
  } else {
    data <- data |>
      dplyr::mutate(
        Ca = dplyr::case_when(
          system == "Feedlot" ~ ca_feedlot,
          TRUE ~ ca_pasture
        )
      )
  }
  
  # Add grazing distance if not present
  if (!"grazing_distance_km" %in% names(data)) {
    data <- data |> dplyr::mutate(grazing_distance_km = 0)
  }
  
  # Check for diet quality - REQUIRED for GE calculation
  if (!"diet_quality" %in% names(data)) {
    warning("diet_quality not provided. GE calculation will return NA. Please provide diet_quality (High/Medium/Low) for accurate results.")
    data <- data |> 
      dplyr::mutate(
        diet_quality = NA_character_,
        Method_Energy = paste0(Method_Energy, "; diet_quality_MISSING")
      )
  }
  
  # Add temperature if not present (default to thermoneutral)
  if (!"temperature_c" %in% names(data)) {
    data <- data |> 
      dplyr::mutate(
        temperature_c = 15,
        Method_Energy = paste0(Method_Energy, "; temp_assumed_15C")
      )
  }
  
  # Join feed characteristics (required for GE calculation)
  data <- data |>
    dplyr::left_join(
      feed_characteristics,
      by = "diet_quality"
    )
  
  # Determine temperature adjustment factor using coefficient table
  data <- data |>
    dplyr::left_join(
      temperature_adjustment |> 
        dplyr::select(temp_min, temp_max, adjustment_factor),
      by = character()
    ) |>
    dplyr::filter(
      temperature_c >= temp_min & temperature_c < temp_max
    ) |>
    dplyr::group_by(dplyr::across(-c(temp_min, temp_max, adjustment_factor))) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename(temp_adjustment = adjustment_factor) |>
    dplyr::select(-temp_min, -temp_max) |>
    dplyr::mutate(
      Method_Energy = dplyr::if_else(
        temp_adjustment != 0,
        paste0(Method_Energy, "; temp_stress_applied"),
        Method_Energy
      )
    )

  # 5. Calculate Energy Requirements (Modular)
  data <- calc_energy_maintenance(data)
  data <- calc_energy_activity(data)
  data <- calc_energy_lactation(data)
  data <- calc_energy_wool(data)
  data <- calc_energy_work(data)
  data <- calc_energy_pregnancy(data)
  data <- calc_energy_growth(data)
  
  # Total Net Energy
  data <- data |>
    dplyr::mutate(
      NE = NEm + NEa + NEl + NEwool + NEwork + NEp + NEg
    )
    
  # Gross Energy
  data <- estimate_gross_energy(data)
  
  return(data)
}

#' Calculate Net Energy for Maintenance (NEm)
#' @export
calc_energy_maintenance <- function(data) {
  data |>
    dplyr::mutate(
      # Net Energy for Maintenance (NEm) = Cfi * weight^0.75 * (1 + temp_adjustment)
      NEm_base = cfi_mj_day_kg075 * weight^0.75,
      NEm = NEm_base * (1 + temp_adjustment)
    )
}

#' Calculate Net Energy for Activity (NEa)
#' @export
calc_energy_activity <- function(data) {
  # Get walking energy cost from coefficient table
  walking_cost <- grazing_energy_coefs$value_mj_kg_km[grazing_energy_coefs$parameter == "walking_energy_cost"]
  
  data |>
    dplyr::mutate(
      # Net Energy for Activity (NEa) = Ca * NEm + Walking Energy
      # Walking Energy from NRC 2001: 0.0019 MJ/kg BW per km
      NEa = Ca * NEm + (walking_cost * weight * grazing_distance_km)
    )
}

#' Calculate Net Energy for Lactation (NEl)
#' @export
calc_energy_lactation <- function(data) {
  data |>
    dplyr::mutate(
      # IPCC 2006/2019 Equation 10.8 (Cattle/Buffalo): NEl = Milk × (1.47 + 0.40 × Fat%)
      # IPCC 2006/2019 Equation 10.9 (Sheep): NEl = Milk × 4.6 (default EVmilk)
      # Note: Can use enhanced NRC equation if protein/lactose data available
      NEl = dplyr::case_when(
        # If protein and lactose are available, use enhanced NRC equation (more accurate)
        !is.na(protein_percent) & !is.na(lactose_percent) & protein_percent > 0 & lactose_percent > 0 ~
          milk_yield_kg_day * (0.389 * fat_percent + 0.229 * protein_percent + 0.165 * lactose_percent),
        # For sheep, use IPCC Eq 10.9
        grepl("Sheep", species_gen, ignore.case = TRUE) ~
          milk_yield_kg_day * 4.6,
        # Otherwise use IPCC Eq 10.8 (standard)
        TRUE ~ milk_yield_kg_day * (1.47 + 0.40 * fat_percent)
      )
    )
}

#' Calculate Net Energy for Wool (NEwool)
#' @export
calc_energy_wool <- function(data) {
  data |>
    dplyr::mutate(
      # Net Energy for Wool Production (NEwool) - IPCC Equation 10.12
      # Only for sheep: NEwool = Wool × Cwool
      # Default Cwool = 24 MJ/kg wool (IPCC 2006)
      NEwool = dplyr::if_else(
        grepl("Sheep", species_gen, ignore.case = TRUE) & !is.na(cl),
        dplyr::coalesce(wool_production_kg_yr, 0) / 365 * cl * NEm,  # cl is Cwool coefficient
        0
      )
    )
}

#' Calculate Net Energy for Work (NEwork)
#' @export
calc_energy_work <- function(data) {
  data |>
    dplyr::mutate(
      # Eq 10.11: Cw * NEm * Hours
      NEwork = cw * NEm * work_hours_day
    )
}

#' Calculate Net Energy for Pregnancy (NEp)
#' @export
calc_energy_pregnancy <- function(data) {
  data |>
    dplyr::mutate(
      # Eq 10.13: Cp * NEm (for pregnant animals)
      NEp = cp * NEm * pregnant_fraction
    )
}

#' Calculate Net Energy for Growth (NEg)
#' @export
calc_energy_growth <- function(data) {
  data |>
    dplyr::mutate(
      # Eq 10.6: Gain * Energy_Content_Gain
      NEg = weight_gain_kg_day * dplyr::coalesce(energy_content_gain_mj_kg, 22)
    )
}

#' Estimate Gross Energy (GE)
#' @export
estimate_gross_energy <- function(data) {
  data |>
    dplyr::mutate(
      # Use dynamic DE and REM
      DE_percent = dplyr::coalesce(de_percent, livestock_constants$default_de_percent),
      
      # REM Calculation (IPCC 2019 Eq 10.24 for ruminants)
      # REM = 1.123 - (4.092e-3 * DE) + (1.126e-5 * DE^2) - (25.4 / DE)
      REM = 1.123 - (4.092e-3 * DE_percent) + (1.126e-5 * DE_percent^2) - (25.4 / DE_percent),
      # Clamp REM to reasonable values (0.4-0.7) just in case
      REM = pmax(0.4, pmin(0.7, REM)),
      
      GE = (NE / REM) / (DE_percent / 100)
    )
}
