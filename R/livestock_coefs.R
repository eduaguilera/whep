# Livestock Coefficient Tables Documentation
# 
# This file provides roxygen2 documentation for all livestock coefficient tables
# following the same format as table_mappings.R

# GLEAM Excel Supplement Tables ----

#' GLEAM Crop Residue Parameters
#'
#' Parameters for calculating crop residues and dry matter content.
#'
#' @format
#' A dataframe with crop-specific parameters for residue calculations.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.1
#' https://doi.org/10.1088/1748-9326/aad4d8
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' head(gleam_crop_residue_params)
#' }
"gleam_crop_residue_params"

#' GLEAM Country and Regional Groupings
#'
#' Maps countries (ISO3 codes) to GLEAM regions, FAOSTAT regions, and other classifications.
#'
#' @format
#' A dataframe where each row corresponds to one country.
#' It contains the following columns:
#' - `iso3`: ISO3 country code (character)
#' - `country`: Country name (character)
#' - `continent`: Continent (character)
#' - `faostat_region`: FAOSTAT regional classification (character)
#' - `gleam_region`: GLEAM regional classification (character)
#' - `eu27`: EU27 membership indicator (numeric, 0 or 1)
#' - `oecd`: OECD membership indicator (numeric, 0 or 1)
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.A1-S.A2
#' https://doi.org/10.1088/1748-9326/aad4d8
#'
#' @usage
#' Used for regional aggregation and mapping countries to GLEAM regions.
#' Essential for joining regional-specific parameters using iso3 codes.
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' head(gleam_geographic_hierarchy)
#' # Map USA to GLEAM region
#' gleam_geographic_hierarchy |> filter(iso3 == "USA")
#' }
"gleam_geographic_hierarchy"

#' GLEAM Feed Composition
#'
#' Nutritional composition of different feed items.
#'
#' @format
#' A dataframe where each row corresponds to one feed item.
#' Columns include feed item identifier and nutritional parameters (DM, CP, ME, etc.).
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.2
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' head(gleam_feed_composition)
#' }
"gleam_feed_composition"

#' GLEAM Feed Digestibility
#'
#' Digestibility coefficients for different feed types.
#'
#' @format
#' A dataframe with digestibility parameters by feed type.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.3
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' head(gleam_feed_digestibility)
#' }
"gleam_feed_digestibility"

#' GLEAM Feed Conversion Ratios
#'
#' Feed conversion efficiency parameters.
#'
#' @format
#' A dataframe with feed conversion ratios by species and system.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.4
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' head(gleam_feed_conversion_ratios)
#' }
"gleam_feed_conversion_ratios"

#' GLEAM Production System Parameters
#'
#' Parameters for different livestock production systems by region.
#' Merged from Tables S.6.1 to S.6.9.
#'
#' @format
#' A dataframe with system-specific parameters.
#' Includes a `source_table` column indicating the original table (e.g., "tab_s61").
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.6.1-S.6.9
#'
#' @usage
#' Used in production system characterization and emissions calculations.
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' head(gleam_production_system_params)
#' unique(gleam_production_system_params$source_table)
#' }
"gleam_production_system_params"

#' GLEAM Manure Management System Parameters
#'
#' Parameters for manure management systems.
#' Merged from Tables S.7.1 to S.7.7.
#'
#' @format
#' A dataframe with manure management parameters.
#' Includes a `source_table` column indicating the original table (e.g., "tab_s71").
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.7.1-S.7.7
#'
#' @usage
#' Used in manure CH4 and N2O emissions calculations.
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' head(gleam_manure_system_params)
#' }
"gleam_manure_system_params"

#' GLEAM Dressing Percentages
#'
#' Carcass dressing percentages by species, cohort, and region.
#'
#' @format
#' A dataframe where each row corresponds to a species-cohort-region combination.
#' Columns include:
#' - `species`: Livestock species
#' - `cohort`: Animal cohort
#' - `region`: Geographic region
#' - `value`: Dressing percentage
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.9.1
#'
#' @examples
#'
#' @format
#' A tibble with columns: `species`, `production_system`, `cohort`, `description`.
#'
#' @source FAO (2018) GLEAM 3.0 Model Description, Table 2.1, Pages 12-13
#' https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Model_description.pdf
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' gleam_livestock_categories
#' }
"gleam_livestock_categories"

#' GLEAM Feed Categories
#'
#' Classification of feed types used in GLEAM.
#'
#' @format
#' A tibble with columns: `feed_category`, `feed_type`, `description`.
#'
#' @source FAO (2018) GLEAM 3.0 Model Description, Table 3.1, Pages 18-20
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' gleam_feed_categories
#' }
"gleam_feed_categories"

#' GLEAM Enteric Fermentation Parameters
#'
#' Methane conversion factors (Ym) by species and production system.
#'
#' @format
#' A tibble with columns: `species`, `system`, `ym_percent`, `notes`.
#'
#' @source FAO (2018) GLEAM 3.0 Model Description, Table 4.1, Pages 25-27
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' gleam_enteric_params
#' }
"gleam_enteric_params"

#' GLEAM Manure Management System Shares
#'
#' Regional shares of different manure management systems.
#'
#' @format
#' A tibble with columns: `region`, `species`, `system`, `mms`, `share_percent`.
#'
#' @source FAO (2018) GLEAM 3.0 Model Description, Table 4.2, Pages 30-32
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' gleam_mms_shares
#' }
"gleam_mms_shares"

#' GLEAM Animal Weights
#'
#' Default animal weights by region, species, system, and cohort.
#'
#' @format
#' A tibble with columns: `region`, `species`, `system`, `cohort`, `weight_kg`.
#'
#' @source FAO (2018) GLEAM 3.0 Model Description, Table 5.1, Pages 35-37
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' gleam_animal_weights
#' }
"gleam_animal_weights"

#' GLEAM Milk Production Parameters
#'
#' Milk production levels and lactation periods by region and species.
#'
#' @format
#' A tibble with columns: `region`, `species`, `system`, `milk_kg_head_yr`, `lactation_days`.
#'
#' @source FAO (2018) GLEAM 3.0 Model Description, Table 6.1, Pages 40-42
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' gleam_milk_production
#' }
"gleam_milk_production"

# IPCC 2019 Tables ----

#' IPCC 2019 Enteric Fermentation Emission Factors - Cattle
#'
#' Tier 1 default emission factors for enteric fermentation from cattle.
#'
#' @format
#' A tibble with columns: `region`, `category`, `ef_kg_head_yr`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.10
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_2019_enteric_ef_cattle
#' }
"ipcc_2019_enteric_ef_cattle"

#' IPCC 2019 Enteric Fermentation Emission Factors - Other Livestock
#'
#' Tier 1 default emission factors for enteric fermentation from non-cattle species.
#'
#' @format
#' A tibble with columns: `category`, `ef_kg_head_yr`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.11
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_2019_enteric_ef_other
#' }
"ipcc_2019_enteric_ef_other"

#' IPCC 2019 Manure Management CH4 Emission Factors - Cattle
#'
#' Tier 1 default emission factors for CH4 from manure management for cattle.
#'
#' @format
#' A tibble with columns: `region`, `category`, `climate`, `ef_kg_head_yr`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.14
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_2019_manure_ch4_ef_cattle
#' }
"ipcc_2019_manure_ch4_ef_cattle"

#' IPCC 2019 Manure Management CH4 Emission Factors - Other Livestock
#'
#' Tier 1 default emission factors for CH4 from manure management for non-cattle species.
#'
#' @format
#' A tibble with columns: `category`, `climate`, `ef_kg_head_yr`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.14
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_2019_manure_ch4_ef_other
#' }
"ipcc_2019_manure_ch4_ef_other"

#' IPCC 2019 Methane Conversion Factors (MCF)
#'
#' MCF values for different manure management systems and climates.
#'
#' @format
#' A tibble with columns: `system`, `climate`, `mcf_percent`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.17
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_2019_mcf_manure
#' }
"ipcc_2019_mcf_manure"

#' IPCC 2019 Nitrogen Excretion Rates
#'
#' Default annual N excretion rates by livestock category and region.
#'
#' @format
#' A tibble with columns: `region`, `category`, `nex_kg_n_head_yr`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.19
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_2019_n_excretion
#' }
"ipcc_2019_n_excretion"

#' IPCC 2019 Direct N2O Emission Factors
#'
#' Direct N2O-N emission factors for different manure management systems.
#'
#' @format
#' A tibble with columns: `system`, `ef_kg_n2o_n_per_kg_n`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.21
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_2019_n2o_ef_direct
#' }
"ipcc_2019_n2o_ef_direct"

# IPCC Tier 2 Parameters ----

#' IPCC Tier 2 Energy Coefficients
#'
#' Coefficients for calculating gross energy (GE) intake using IPCC Tier 2 approach.
#'
#' @format
#' A tibble with columns: `category`, `cfi_mj_day_kg075`, `ca_pasture`, etc.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_tier2_energy_coefs
#' }
"ipcc_tier2_energy_coefs"

#' IPCC Tier 2 Methane Conversion Factors (Ym)
#'
#' Ym values representing the percentage of gross energy converted to CH4.
#'
#' @format
#' A tibble with columns: `category`, `diet_quality`, `ym_percent`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_tier2_ym_values
#' }
"ipcc_tier2_ym_values"

#' IPCC Tier 2 Maximum Methane Producing Capacity (Bo)
#'
#' Bo values in m³ CH4 per kg of volatile solids excreted.
#'
#' @format
#' A tibble with columns: `category`, `bo_m3_kg_vs`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_tier2_bo_values
#' }
"ipcc_tier2_bo_values"

#' IPCC Tier 2 Ash Content of Manure
#'
#' Default ash content percentages for calculating volatile solids.
#'
#' @format
#' A tibble with columns: `category`, `ash_percent`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_tier2_manure_ash
#' }
"ipcc_tier2_manure_ash"

#' IPCC Tier 2 Nitrogen Retention Fractions
#'
#' Fraction of nitrogen intake retained by the animal.
#'
#' @format
#' A tibble with columns: `category`, `n_retention_frac`.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10, Table 10.20
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' ipcc_tier2_n_retention
#' }
"ipcc_tier2_n_retention"

#' Livestock Production Defaults
#'
#' Default values for production parameters when specific data is missing.
#' Includes fat percentage, weight gain, work hours, pregnancy fraction, and CP percent.
#'
#' @format
#' A tibble with columns: `category`, `fat_percent`, `weight_gain_kg_day`, 
#' `work_hours_day`, `pregnant_fraction`, `cp_percent`.
#'
#' @source Derived from IPCC 2019 defaults and expert judgement for fallbacks.
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' livestock_production_defaults
#' }
"livestock_production_defaults"

# Physical Constants ----

#' Physical and Conversion Constants
#'
#' Physical constants used throughout emissions calculations.
#'
#' @format
#' A named list containing energy contents, densities, and conversion factors.
#'
#' @source IPCC (2019) 2019 Refinement, Volume 4, Chapter 10
#'
#' @examples
#' \dontrun{
#' load("data/livestock_coefs.rda")
#' livestock_constants
#' }
"livestock_constants"
