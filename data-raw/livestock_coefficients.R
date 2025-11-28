# Livestock Coefficients Data Extraction
# 
# This script extracts and documents all coefficient tables needed for livestock
# emissions calculations following GLEAM 3.0 and IPCC 2019/2006 methodologies.
#
# Sources:
# - GLEAM 3.0: MacLeod et al. (2018) https://doi.org/10.1088/1748-9326/aad4d8
#   Supplement S1: GLEAM_3.0_Supplement_S1.xlsx
# - IPCC 2019: 2019 Refinement to the 2006 IPCC Guidelines
#   Volume 4, Chapter 10: Emissions from Livestock and Manure Management
# - IPCC 2006: 2006 IPCC Guidelines for National Greenhouse Gas Inventories
#   Volume 4, Chapter 10

# Load packages
if (file.exists("renv/activate.R")) source("renv/activate.R")
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)

# Helper Functions ----

#' Clean column names
clean_names <- function(names) {
  names |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^[:alnum:]_]", "") |>
    tolower() |>
    make.unique(sep = "_")
}

# GLEAM Data Extraction ----

#' Extract GLEAM Tables from Supplement S1
#' 
#' @description
#' Extracts all tables from GLEAM 3.0 Supplement S1 Excel file.
#' Tables are returned as-is with minimal processing to preserve original data.
#' 
#' @param path Path to GLEAM Excel file
#' @return Named list of dataframes
#' 
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1
#' https://doi.org/10.1088/1748-9326/aad4d8
extract_gleam_tables <- function(path) {
  message("Extracting GLEAM tables from: ", path)
  
  # Create a temp copy to avoid file lock issues
  temp_file <- tempfile(fileext = ".xlsx")
  file.copy(path, temp_file)
  on.exit(unlink(temp_file))
  
  path_to_read <- temp_file
  
  tryCatch({
    sheets <- openxlsx::getSheetNames(path_to_read)
    tables <- list()
    
    for (sheet in sheets) {
      if (sheet == "Table of contents") next
      
      tryCatch({
        # Read entire sheet
        df <- openxlsx::read.xlsx(path_to_read, sheet = sheet, colNames = TRUE)
        
        # Clean column names
        names(df) <- clean_names(names(df))
        
        # Store with cleaned sheet name
        clean_sheet <- clean_names(sheet)
        tables[[clean_sheet]] <- df
        
        message("  Extracted: ", sheet)
      }, error = function(e) {
        warning("  Failed to extract ", sheet, ": ", e$message)
      })
    }
    
    return(tables)
  }, error = function(e) {
    warning("Failed to open GLEAM file: ", e$message)
    warning("Proceeding without GLEAM tables. Please check file permissions.")
    return(list())
  })
}

# GLEAM PDF Tables Generation ----

#' Generate GLEAM Model Tables from PDF Documentation
#' 
#' @description
#' Manually transcribed tables from GLEAM 3.0 Model Description PDF.
#' These complement the Excel supplement tables.
#' 
#' @return Named list of dataframes
#' 
#' @source FAO (2018) GLEAM 3.0 Model Description
#' https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Model_description.pdf
generate_gleam_pdf_tables <- function() {
  list(
    # Table 2.1: Livestock Categories and Herd Structures
    # Page 12-13
    gleam_livestock_categories = tibble::tribble(
      ~species, ~production_system, ~cohort, ~description,
      "Cattle", "Dairy", "Adult Female", "Milking cows",
      "Cattle", "Dairy", "Adult Male", "Bulls",
      "Cattle", "Dairy", "Replacement Female", "Heifers",
      "Cattle", "Dairy", "Replacement Male", "Young bulls",
      "Cattle", "Dairy", "Surplus Female", "Culled heifers",
      "Cattle", "Dairy", "Surplus Male", "Calves for meat",
      "Cattle", "Beef", "Adult Female", "Breeding cows",
      "Cattle", "Beef", "Adult Male", "Bulls",
      "Cattle", "Beef", "Replacement Female", "Heifers",
      "Cattle", "Beef", "Replacement Male", "Young bulls",
      "Cattle", "Beef", "Fattening", "Fattening cattle",
      "Buffalo", "Dairy", "Adult Female", "Milking buffalo",
      "Buffalo", "Dairy", "Replacement", "Young buffalo",
      "Buffalo", "Other", "Adult", "Draft buffalo",
      "Sheep", "Dairy", "Adult Female", "Milking ewes",
      "Sheep", "Dairy", "Replacement", "Young ewes",
      "Sheep", "Meat", "Adult Female", "Breeding ewes",
      "Sheep", "Meat", "Fattening", "Lambs",
      "Goats", "Dairy", "Adult Female", "Milking goats",
      "Goats", "Dairy", "Replacement", "Young goats",
      "Goats", "Meat", "Adult Female", "Breeding goats",
      "Goats", "Meat", "Fattening", "Kids",
      "Pigs", "Breeding", "Sows", "Breeding sows",
      "Pigs", "Breeding", "Boars", "Breeding boars",
      "Pigs", "Fattening", "Fattening", "Fattening pigs",
      "Poultry", "Layers", "Layers", "Laying hens",
      "Poultry", "Broilers", "Broilers", "Meat chickens"
    ),
    
    # Table 3.1: Feed Categories
    # Page 18-20
    gleam_feed_categories = tibble::tribble(
      ~feed_category, ~feed_type, ~description,
      "Grass", "Pasture", "Grazed grass and fodder",
      "Crop residues", "Residues", "Straw, stovers, husks",
      "Concentrates", "Crops", "Grains, oilseeds, pulses",
      "Fodder crops", "Crops", "Cultivated fodder",
      "Processed feeds", "Industrial", "Brans, meals, cakes",
      "Animal products", "Animal", "Milk, fish meal"
    ),
    
    # Table 4.1: Enteric Fermentation Parameters
    # Page 25-27
    gleam_enteric_params = tibble::tribble(
      ~species, ~system, ~ym_percent, ~notes,
      "Cattle", "Grazing", 6.5, "IPCC default",
      "Cattle", "Mixed", 6.5, "IPCC default",
      "Cattle", "Feedlot", 3.0, "High concentrate diet",
      "Buffalo", "Grazing", 6.5, "IPCC default",
      "Buffalo", "Mixed", 6.5, "IPCC default",
      "Sheep", "Grazing", 6.5, "IPCC default",
      "Sheep", "Mixed", 6.5, "IPCC default",
      "Goats", "Grazing", 5.5, "IPCC default",
      "Goats", "Mixed", 5.5, "IPCC default",
      "Pigs", "All", 0.0, "Negligible enteric CH4"
    ),
    
    # Table 4.2: Manure Management System Shares by Region
    # Page 30-32
    gleam_mms_shares = tibble::tribble(
      ~region, ~species, ~system, ~mms, ~share_percent,
      "Western Europe", "Cattle", "Dairy", "Liquid/Slurry", 60,
      "Western Europe", "Cattle", "Dairy", "Solid Storage", 30,
      "Western Europe", "Cattle", "Dairy", "Pasture", 10,
      "Western Europe", "Cattle", "Beef", "Pasture", 70,
      "Western Europe", "Cattle", "Beef", "Solid Storage", 30,
      "Sub-Saharan Africa", "Cattle", "All", "Pasture", 90,
      "Sub-Saharan Africa", "Cattle", "All", "Daily Spread", 10,
      "Latin America", "Cattle", "All", "Pasture", 95,
      "Latin America", "Cattle", "All", "Solid Storage", 5,
      "South Asia", "Cattle", "All", "Daily Spread", 60,
      "South Asia", "Cattle", "All", "Solid Storage", 30,
      "South Asia", "Cattle", "All", "Pasture", 10,
      "East Asia", "Pigs", "All", "Liquid/Slurry", 70,
      "East Asia", "Pigs", "All", "Solid Storage", 30
    ),
    
    # Table 5.1: Default Animal Weights
    # Page 35-37
    gleam_animal_weights = tibble::tribble(
      ~region, ~species, ~system, ~cohort, ~weight_kg,
      "Western Europe", "Cattle", "Dairy", "Adult Female", 650,
      "Western Europe", "Cattle", "Dairy", "Adult Male", 1000,
      "Western Europe", "Cattle", "Beef", "Adult Female", 600,
      "Western Europe", "Cattle", "Beef", "Fattening", 400,
      "North America", "Cattle", "Dairy", "Adult Female", 680, # High production
      "North America", "Cattle", "Dairy", "Adult Male", 1000,
      "North America", "Cattle", "Beef", "Adult Female", 550,
      "North America", "Cattle", "Beef", "Fattening", 450,
      "Sub-Saharan Africa", "Cattle", "All", "Adult Female", 250,
      "Sub-Saharan Africa", "Cattle", "All", "Adult Male", 350,
      "South Asia", "Cattle", "Dairy", "Adult Female", 350,
      "South Asia", "Buffalo", "Dairy", "Adult Female", 450,
      "Latin America", "Cattle", "Beef", "Adult Female", 450,
      "Latin America", "Cattle", "Beef", "Fattening", 350,
      "Global", "Cattle", "All", "Adult Female", 400, # Fallback
      "Global", "Cattle", "All", "Adult Male", 600, # Fallback
      "Global", "Cattle", "All", "Fattening", 300, # Fallback
      "Global", "Sheep", "All", "Adult", 45,
      "Global", "Goats", "All", "Adult", 40,
      "Global", "Pigs", "Fattening", "Fattening", 50,
      "Global", "Pigs", "Breeding", "Sows", 200
    ),
    
    # Table 6.1: Milk Production Parameters
    # Page 40-42
    gleam_milk_production = tibble::tribble(
      ~region, ~species, ~system, ~milk_kg_head_yr, ~lactation_days,
      "Western Europe", "Cattle", "Dairy", 7500, 305,
      "North America", "Cattle", "Dairy", 9500, 305,
      "Oceania", "Cattle", "Dairy", 5500, 270,
      "Latin America", "Cattle", "Dairy", 2500, 240,
      "Sub-Saharan Africa", "Cattle", "Dairy", 800, 180,
      "South Asia", "Cattle", "Dairy", 1500, 240,
      "South Asia", "Buffalo", "Dairy", 1800, 270,
      "Western Europe", "Sheep", "Dairy", 200, 180,
      "Western Europe", "Goats", "Dairy", 450, 240
    )
  )
}

# IPCC Tables Generation ----

#' Generate IPCC 2019 Tables
#' 
#' @description
#' Creates dataframes for all relevant IPCC 2019 Refinement tables.
#' 
#' @return Named list of dataframes
#' 
#' @source IPCC 2019 Refinement, Volume 4, Chapter 10
#' https://www.ipcc-nggip.iges.or.jp/public/2019rf/index.html
generate_ipcc_2019_tables <- function() {
  list(
    # Table 10.10: Enteric Fermentation - Cattle
    # Used in: Tier 1 enteric CH4 calculations
    table_10_10 = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr,
      "North America", "Dairy Cattle", 128,
      "North America", "Other Cattle", 53,
      "Western Europe", "Dairy Cattle", 117,
      "Western Europe", "Other Cattle", 57,
      "Eastern Europe", "Dairy Cattle", 99,
      "Eastern Europe", "Other Cattle", 58,
      "Oceania", "Dairy Cattle", 90,
      "Oceania", "Other Cattle", 60,
      "Latin America", "Dairy Cattle", 72,
      "Latin America", "Other Cattle", 56,
      "Asia", "Dairy Cattle", 68,
      "Asia", "Other Cattle", 47,
      "Africa", "Dairy Cattle", 46,
      "Africa", "Other Cattle", 31,
      "Middle East", "Dairy Cattle", 46,
      "Middle East", "Other Cattle", 31,
      "Indian Subcontinent", "Dairy Cattle", 68,
      "Indian Subcontinent", "Other Cattle", 47
    ),
    
    # Table 10.11: Enteric Fermentation - Other Livestock
    # Used in: Tier 1 enteric CH4 calculations
    table_10_11 = tibble::tribble(
      ~category, ~ef_kg_head_yr,
      "Buffalo", 55,
      "Sheep", 8,
      "Goats", 5,
      "Camels", 46,
      "Horses", 18,
      "Mules and Asses", 10,
      "Swine - Market", 1.5,
      "Swine - Breeding", 1.5,
      "Poultry", 0
    ),
    
    # Table 10.14: Manure Management CH4 - Cattle
    # Used in: Tier 1 manure CH4 calculations
    table_10_14_cattle = tibble::tribble(
      ~region, ~category, ~climate, ~ef_kg_head_yr,
      "North America", "Dairy Cattle", "Cool", 27,
      "North America", "Dairy Cattle", "Temperate", 42,
      "North America", "Dairy Cattle", "Warm", 60,
      "North America", "Other Cattle", "Cool", 2,
      "North America", "Other Cattle", "Temperate", 3,
      "North America", "Other Cattle", "Warm", 4,
      "Western Europe", "Dairy Cattle", "Cool", 31,
      "Western Europe", "Dairy Cattle", "Temperate", 39,
      "Western Europe", "Other Cattle", "Cool", 1,
      "Western Europe", "Other Cattle", "Temperate", 1,
      "Eastern Europe", "Dairy Cattle", "Cool", 21,
      "Eastern Europe", "Dairy Cattle", "Temperate", 23,
      "Eastern Europe", "Other Cattle", "Cool", 1,
      "Eastern Europe", "Other Cattle", "Temperate", 1,
      "Oceania", "Dairy Cattle", "Temperate", 24,
      "Oceania", "Other Cattle", "Temperate", 1,
      "Latin America", "Dairy Cattle", "Warm", 47,
      "Latin America", "Other Cattle", "Warm", 1,
      "Africa", "Dairy Cattle", "Warm", 31,
      "Africa", "Other Cattle", "Warm", 1,
      "Middle East", "Dairy Cattle", "Warm", 31,
      "Middle East", "Other Cattle", "Warm", 1,
      "Asia", "Dairy Cattle", "Warm", 39,
      "Asia", "Other Cattle", "Warm", 2
    ),
    
    # Table 10.14: Manure Management CH4 - Other Livestock
    # Used in: Tier 1 manure CH4 calculations
    table_10_14_other = tibble::tribble(
      ~category, ~climate, ~ef_kg_head_yr,
      "Buffalo", "All", 2,
      "Sheep", "All", 0.19,
      "Goats", "All", 0.13,
      "Swine - Market", "All", 6,
      "Swine - Breeding", "All", 6,
      "Poultry - Broilers", "All", 0.02,
      "Poultry - Layers", "All", 0.03,
      "Horses", "All", 1.64,
      "Mules and Asses", "All", 0.90
    ),
    
    # Table 10.17: MCF by Manure Management System and Climate
    # Used in: Tier 2 manure CH4 calculations
    table_10_17 = tibble::tribble(
      ~system, ~climate, ~mcf_percent,
      "Pasture/Range/Paddock", "Cool", 1.0,
      "Pasture/Range/Paddock", "Temperate", 1.5,
      "Pasture/Range/Paddock", "Warm", 2.0,
      "Daily Spread", "Cool", 0.1,
      "Daily Spread", "Temperate", 0.5,
      "Daily Spread", "Warm", 1.0,
      "Solid Storage", "Cool", 2.0,
      "Solid Storage", "Temperate", 4.0,
      "Solid Storage", "Warm", 5.0,
      "Dry Lot", "Cool", 1.5,
      "Dry Lot", "Temperate", 2.5,
      "Dry Lot", "Warm", 4.0,
      "Liquid/Slurry - No Crust", "Cool", 17.0,
      "Liquid/Slurry - No Crust", "Temperate", 24.0,
      "Liquid/Slurry - No Crust", "Warm", 45.0,
      "Liquid/Slurry - With Crust", "Cool", 10.0,
      "Liquid/Slurry - With Crust", "Temperate", 15.0,
      "Liquid/Slurry - With Crust", "Warm", 27.0,
      "Uncovered Anaerobic Lagoon", "Cool", 66.0,
      "Uncovered Anaerobic Lagoon", "Temperate", 73.0,
      "Uncovered Anaerobic Lagoon", "Warm", 80.0,
      "Pit Storage - <1 month", "Cool", 10.0,
      "Pit Storage - <1 month", "Temperate", 15.0,
      "Pit Storage - <1 month", "Warm", 27.0,
      "Pit Storage - >1 month", "Cool", 17.0,
      "Pit Storage - >1 month", "Temperate", 24.0,
      "Pit Storage - >1 month", "Warm", 45.0,
      "Anaerobic Digester", "All", 0.0,
      "Burned for Fuel", "All", 0.0,
      "Composting - Intensive", "All", 0.5,
      "Composting - Passive", "All", 1.5,
      "Poultry Manure - High Rise", "All", 1.5,
      "Poultry Manure - Deep Litter", "All", 1.5
    ),
    
    # Table 10.19: N Excretion Rates
    # Used in: Tier 2 N2O calculations
    table_10_19 = tibble::tribble(
      ~region, ~category, ~nex_kg_n_head_yr,
      "North America", "Dairy Cattle", 105,
      "North America", "Other Cattle", 56,
      "Western Europe", "Dairy Cattle", 100,
      "Western Europe", "Other Cattle", 50,
      "Eastern Europe", "Dairy Cattle", 80,
      "Eastern Europe", "Other Cattle", 50,
      "Oceania", "Dairy Cattle", 80,
      "Oceania", "Other Cattle", 40,
      "Latin America", "Dairy Cattle", 50,
      "Latin America", "Other Cattle", 40,
      "Africa", "Dairy Cattle", 40,
      "Africa", "Other Cattle", 30,
      "Middle East", "Dairy Cattle", 40,
      "Middle East", "Other Cattle", 30,
      "Asia", "Dairy Cattle", 50,
      "Asia", "Other Cattle", 40,
      "Global", "Buffalo", 55,
      "Global", "Sheep", 12,
      "Global", "Goats", 12,
      "Global", "Swine - Market", 15,
      "Global", "Swine - Breeding", 18,
      "Global", "Horses", 50,
      "Global", "Mules and Asses", 35,
      "Global", "Poultry - Broilers", 0.6,
      "Global", "Poultry - Layers", 0.8
    ),
    
    # Table 10.21: Direct N2O Emission Factors
    # Used in: Tier 1 and Tier 2 N2O calculations
    table_10_21 = tibble::tribble(
      ~system, ~ef_kg_n2o_n_per_kg_n,
      "Liquid/Slurry", 0.002,
      "Solid Storage and Drylot", 0.005,
      "Pasture/Range/Paddock", 0.01,
      "Daily Spread", 0.01,
      "Anaerobic Lagoon", 0.001,
      "Anaerobic Digester", 0.0,
      "Poultry Manure", 0.001,
      "Other", 0.005
    )
  )
}

#' Generate IPCC 2006 Tables
#' 
#' @description
#' Tables from IPCC 2006 Guidelines, Vol 4, Chapter 10.
#' Used for "Tier 1 IPCC 2006" and "Tier 2 IPCC 2006" methods.
#' 
#' @return Named list of dataframes
#' @source IPCC 2006 Guidelines
generate_ipcc_2006_tables <- function() {
  list(
    # Tier 1 Enteric Fermentation Emission Factors (2006)
    # Table 10.11
    ipcc_2006_enteric_ef = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr,
      "North America", "Dairy Cattle", 128,
      "North America", "Other Cattle", 53,
      "Western Europe", "Dairy Cattle", 117,
      "Western Europe", "Other Cattle", 57,
      "Eastern Europe", "Dairy Cattle", 99,
      "Eastern Europe", "Other Cattle", 58,
      "Oceania", "Dairy Cattle", 90,
      "Oceania", "Other Cattle", 60,
      "Latin America", "Dairy Cattle", 72,
      "Latin America", "Other Cattle", 56,
      "Asia", "Dairy Cattle", 68,
      "Asia", "Other Cattle", 47,
      "Africa", "Dairy Cattle", 46,
      "Africa", "Other Cattle", 31,
      "Middle East", "Dairy Cattle", 46,
      "Middle East", "Other Cattle", 31,
      "Global", "Buffalo", 55,
      "Global", "Sheep", 8,
      "Global", "Goats", 5,
      "Global", "Swine", 1.5,
      "Global", "Horses", 18,
      "Global", "Mules and Asses", 10
    ),
    
    # Tier 1 Manure Management Emission Factors (2006)
    # Table 10.14 (approximate values for key regions)
    ipcc_2006_manure_ef = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr, ~temp_zone,
      "North America", "Dairy Cattle", 53, "Cool",
      "North America", "Other Cattle", 2, "Cool",
      "Western Europe", "Dairy Cattle", 20, "Cool",
      "Western Europe", "Other Cattle", 6, "Cool",
      "Latin America", "Dairy Cattle", 1, "Warm",
      "Latin America", "Other Cattle", 1, "Warm",
      "Asia", "Dairy Cattle", 16, "Warm",
      "Asia", "Other Cattle", 1, "Warm",
      "Global", "Buffalo", 2, "Warm",
      "Global", "Sheep", 0.19, "All",
      "Global", "Goats", 0.13, "All",
      "Global", "Swine", 6, "All",
      "Global", "Poultry", 0.02, "All"
    ),
    
    # MCF by Temperature (2006)
    # Table 10.17 (Temperature based)
    ipcc_2006_mcf_temp = tibble::tribble(
      ~system, ~temp_c, ~mcf_percent,
      "Liquid/Slurry", 10, 17,
      "Liquid/Slurry", 15, 25,
      "Liquid/Slurry", 20, 35,
      "Liquid/Slurry", 25, 48,
      "Solid Storage", 10, 2,
      "Solid Storage", 15, 4,
      "Solid Storage", 20, 5,
      "Solid Storage", 25, 6,
      "Pasture/Range/Paddock", 10, 1,
      "Pasture/Range/Paddock", 15, 1.5,
      "Pasture/Range/Paddock", 20, 2,
      "Pasture/Range/Paddock", 25, 2.5,
      "Daily Spread", 10, 0.1,
      "Daily Spread", 15, 0.5,
      "Daily Spread", 20, 1,
      "Daily Spread", 25, 1.5
    )
  )
}

#' Generate IPCC Tier 2 Default Parameters
#' 
#' @description
#' Default parameters for IPCC Tier 2 calculations.
#' 
#' @return Named list of dataframes
#' 
#' @source IPCC 2019 Refinement, Volume 4, Chapter 10, Equations 10.3-10.16
generate_ipcc_tier2_params <- function() {
  list(
    # Energy coefficients (Cfi, Ca, Cp)
    # Used in: GE calculation (Equation 10.3)
    energy_coefs = tibble::tribble(
      ~category, ~cfi_mj_day_kg075, ~ca_pasture, ~ca_feedlot, ~cp, ~cw, ~cl, ~energy_content_gain_mj_kg, ~ge_content_feed_mj_kg_dm,
      "Cattle", 0.386, 0.17, 0.00, 0.10, 0.00, 0.10, 22.0, 18.45,
      "Buffalo", 0.386, 0.17, 0.00, 0.10, 0.00, 0.10, 20.0, 18.45,
      "Sheep", 0.236, 0.10, 0.00, 0.13, 0.00, 0.10, 23.0, 18.45,
      "Goats", 0.246, 0.10, 0.00, 0.13, 0.00, 0.10, 23.0, 18.45
    ),
    
    # Methane conversion factor (Ym) - % of GE
    # Used in: Enteric CH4 calculation (Equation 10.21)
    ym_values = tibble::tribble(
      ~category, ~diet_quality, ~ym_percent,
      "Cattle", "High", 6.5,
      "Cattle", "Medium", 6.5,
      "Cattle", "Low", 6.5,
      "Buffalo", "High", 6.5,
      "Buffalo", "Medium", 6.5,
      "Buffalo", "Low", 6.5,
      "Sheep", "High", 6.5,
      "Sheep", "Medium", 6.5,
      "Sheep", "Low", 6.5,
      "Goats", "High", 5.5,
      "Goats", "Medium", 5.5,
      "Goats", "Low", 5.5
    ),
    
    # Maximum methane producing capacity (Bo)
    # Used in: Manure CH4 calculation (Equation 10.23)
    bo_values = tibble::tribble(
      ~category, ~bo_m3_kg_vs,
      "Cattle", 0.24,
      "Buffalo", 0.10,
      "Sheep", 0.19,
      "Goats", 0.18,
      "Swine", 0.45,
      "Poultry", 0.39,
      "Horses", 0.30
    ),
    
    # Ash content of manure
    # Used in: VS calculation (Equation 10.24)
    ash_content = tibble::tribble(
      ~category, ~ash_percent,
      "Cattle", 8.0,
      "Buffalo", 8.0,
      "Sheep", 8.0,
      "Goats", 8.0,
      "Swine", 4.0,
      "Poultry", 25.0,
      "Horses", 20.0,
      "Camels", 8.0,
      "Mules and Asses", 8.0
    ),
    
    # Feed Characteristics for Dynamic Calculations
    # Used in: Dynamic DE, REM, and Ym calculations
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10 (Typical values)
    feed_characteristics = tibble::tribble(
      ~diet_quality, ~de_percent, ~ndf_percent, ~ge_content_mj_kg_dm,
      "High", 75.0, 35.0, 18.45,
      "Medium", 65.0, 50.0, 18.45,
      "Low", 55.0, 65.0, 18.45
    ),
    
    # Nitrogen Retention Fractions (Nret)
    # Used in: N excretion calculation (Equation 10.30)
    # Source: IPCC 2019, Vol 4, Ch 10, Table 10.20 / Eq 10.30 text
    n_retention_frac = tibble::tribble(
      ~category, ~n_retention_frac,
      "Cattle", 0.20, # Typical for dairy/growing
      "Buffalo", 0.20,
      "Sheep", 0.10,
      "Goats", 0.10,
      "Swine", 0.30, # Growing pigs
      "Poultry", 0.30,
      "Horses", 0.05,
      "Camels", 0.05,
      "Mules and Asses", 0.05,
      "Other", 0.05
    ),
    
    # Default Production Parameters (Fallbacks)
    # Used when specific data is missing
    # Source: NRC 2001, IPCC 2019 Chapter 10
    production_defaults = tibble::tribble(
      ~category, ~fat_percent, ~protein_percent, ~lactose_percent, ~weight_gain_kg_day, ~work_hours_day, ~pregnant_fraction, ~cp_percent,
      "Dairy Cattle", 4.0, 3.2, 4.85, 0.0, 0.0, 0.9, 15.0,
      "Other Cattle", 0.0, 0.0, 0.0, 0.5, 0.0, 0.0, 10.0,
      "Buffalo", 7.0, 4.5, 4.9, 0.2, 2.0, 0.6, 12.0,  # Buffalo milk has higher protein
      "Sheep", 7.0, 5.5, 4.8, 0.1, 0.0, 0.5, 10.0,  # Sheep milk high in protein/fat
      "Goats", 4.0, 3.5, 4.5, 0.05, 0.0, 0.5, 10.0,
      "Swine", 0.0, 0.0, 0.0, 0.6, 0.0, 0.0, 16.0,
      "Poultry", 0.0, 0.0, 0.0, 0.05, 0.0, 0.0, 20.0,
      "Horses", 0.0, 0.0, 0.0, 0.0, 4.0, 0.0, 10.0,
      "Camels", 4.0, 3.7, 5.0, 0.0, 2.0, 0.4, 10.0,
      "Mules and Asses", 0.0, 0.0, 0.0, 0.0, 6.0, 0.0, 10.0
    ),
    
    # Climate-adjusted MCF (Methane Conversion Factor)
    # Used in: Manure CH4 calculation with climate adjustment
    # Source: IPCC 2019, Vol 4, Ch 10, Table 10.17
    climate_mcf = tibble::tribble(
      ~mms_type, ~climate_zone, ~mcf_percent,
      "Daily Spread", "Cool", 0.1,
      "Daily Spread", "Temperate", 0.5,
      "Daily Spread", "Warm", 1.0,
      "Solid Storage", "Cool", 1.0,
      "Solid Storage", "Temperate", 2.0,
      "Solid Storage", "Warm", 4.0,
      "Dry Lot", "Cool", 0.5,
      "Dry Lot", "Temperate", 1.5,
      "Dry Lot", "Warm", 2.5,
      "Liquid/Slurry", "Cool", 17.0,
      "Liquid/Slurry", "Temperate", 39.0,
      "Liquid/Slurry", "Warm", 66.0,
      "Anaerobic Lagoon", "Cool", 20.0,
      "Anaerobic Lagoon", "Temperate", 50.0,
      "Anaerobic Lagoon", "Warm", 80.0,
      "Pasture/Range/Paddock", "Cool", 0.5,
      "Pasture/Range/Paddock", "Temperate", 1.0,
      "Pasture/Range/Paddock", "Warm", 1.5,
      "Poultry Manure", "Cool", 0.5,
      "Poultry Manure", "Temperate", 1.5,
      "Poultry Manure", "Warm", 3.0
    ),
    
    # Regional MMS Distribution
    # Used in: Distributing manure across management systems by region
    # Source: GLEAM 3.0 / FAO statistics (simplified)
    regional_mms_distribution = tibble::tribble(
      ~region, ~species, ~mms_type, ~fraction,
      # North America - Cattle
      "North America", "Cattle", "Liquid/Slurry", 0.40,
      "North America", "Cattle", "Solid Storage", 0.30,
      "North America", "Cattle", "Pasture/Range/Paddock", 0.25,
      "North America", "Cattle", "Daily Spread", 0.05,
      # Western Europe - Cattle
      "Western Europe", "Cattle", "Liquid/Slurry", 0.35,
      "Western Europe", "Cattle", "Solid Storage", 0.45,
      "Western Europe", "Cattle", "Pasture/Range/Paddock", 0.15,
      "Western Europe", "Cattle", "Daily Spread", 0.05,
      # Latin America - Cattle
      "Latin America and Caribbean", "Cattle", "Pasture/Range/Paddock", 0.70,
      "Latin America and Caribbean", "Cattle", "Solid Storage", 0.15,
      "Latin America and Caribbean", "Cattle", "Daily Spread", 0.10,
      "Latin America and Caribbean", "Cattle", "Liquid/Slurry", 0.05,
      # Global Default - Cattle
      "Global", "Cattle", "Pasture/Range/Paddock", 0.50,
      "Global", "Cattle", "Solid Storage", 0.30,
      "Global", "Cattle", "Liquid/Slurry", 0.15,
      "Global", "Cattle", "Daily Spread", 0.05,
      # Swine - North America
      "North America", "Swine", "Liquid/Slurry", 0.70,
      "North America", "Swine", "Solid Storage", 0.20,
      "North America", "Swine", "Anaerobic Lagoon", 0.10,
      # Swine - Global Default
      "Global", "Swine", "Liquid/Slurry", 0.50,
      "Global", "Swine", "Solid Storage", 0.40,
      "Global", "Swine", "Daily Spread", 0.10,
      # Poultry - Global Default
      "Global", "Poultry", "Poultry Manure", 0.80,
      "Global", "Poultry", "Solid Storage", 0.20
    ),
    
    # Temperature adjustment coefficients
    # Used in: Adjusting NEm for temperature stress
    # Source: IPCC 2019 / NRC 2001
    temperature_adjustment = tibble::tribble(
      ~temp_range, ~temp_min, ~temp_max, ~adjustment_factor,
      "Cold Stress", -Inf, 5, 0.20,      # 20% increase in NEm below 5°C
      "Thermoneutral", 5, 25, 0.00,      # No adjustment 5-25°C
      "Heat Stress", 25, Inf, 0.10       # 10% increase in NEm above 25°C
    ),
    
    # Indirect N₂O Emission Factors
    # Used in: Calculating indirect N₂O from volatilization and leaching
    # Source: IPCC 2019, Vol 4, Ch 11, Table 11.3
    indirect_n2o_ef = tibble::tribble(
      ~pathway, ~ef_value, ~description,
      "volatilization", 0.01, "EF4: N₂O from atmospheric deposition of volatilized N",
      "leaching", 0.0075, "EF5: N₂O from N leaching and runoff",
      "frac_gasms", 0.20, "FracGasMS: Fraction of N lost as NH3 and NOx from MMS",
      "frac_leach", 0.30, "FracLeach: Fraction of N lost through leaching/runoff"
    ),
    
    # Uncertainty Ranges for Key Parameters
    # Used in: Calculating confidence intervals
    # Source: IPCC 2019 uncertainty guidance
    uncertainty_ranges = tibble::tribble(
      ~parameter, ~lower_bound, ~upper_bound, ~distribution,
      "Ym", 0.85, 1.15, "normal",          # ±15% for Ym
      "MCF", 0.70, 1.30, "normal",         # ±30% for MCF
      "Bo", 0.80, 1.20, "normal",          # ±20% for Bo
      "EF_N2O", 0.50, 2.00, "lognormal",   # -50% to +100% for N2O EF
      "Nex", 0.90, 1.10, "normal"          # ±10% for N excretion
    ),
    
    # Grazing and Activity Energy Coefficients
    # Used in: Calculating energy cost of walking/grazing
    # Source: NRC 2001 (0.00045 Mcal/kg BW per km converted to MJ)
    grazing_energy_coefs = tibble::tribble(
      ~parameter, ~value_mj_kg_km, ~source,
      "walking_energy_cost", 0.0019, "NRC 2001 (converted from 0.00045 Mcal/kg/km)"
    ),
    
    # Fallback Constants (used when data is missing)
    # These should be documented in Method column when applied
    fallback_constants = tibble::tribble(
      ~parameter, ~value, ~unit, ~note,
      "ym_default", 6.5, "percent", "IPCC default Ym when diet quality unknown",
      "mcf_default", 1.0, "percent", "Conservative MCF when system unknown",
      "ash_content_default", 8.0, "percent", "IPCC default ash content",
      "de_percent_default", 65.0, "percent", "Medium diet quality DE",
      "ue_fraction_default", 0.04, "fraction", "IPCC default urinary energy fraction"
    )
  )
}

# Physical Constants ----

#' Physical and Conversion Constants
#' 
#' @description
#' Physical constants used in emissions calculations.
#' 
#' @source IPCC 2019 Refinement, Volume 4, Chapter 10
livestock_constants <- list(
  # Energy content of methane
  energy_content_ch4_mj_kg = 55.65,
  
  # Density of methane
  ch4_density_kg_m3 = 0.67,
  
  # Energy content of volatile solids
  vs_energy_content_mj_kg = 18.45,
  
  # Molecular weight conversion N to N2O
  n_to_n2o = 44/28,
  
  # Days in year
  days_in_year = 365,
  
  # Default digestibility (%)
  default_de_percent = 65,
  
  # Default urinary energy (fraction of GE)
  default_ue_fraction = 0.04,
  
  # Default ratio of NE available for maintenance
  default_rem = 0.50
)

# Main Execution ----

main <- function() {
  message("\n=== Livestock Coefficients Data Generation ===\n")
  
  # Extract GLEAM Excel tables
  gleam_file <- "data-raw/GLEAM_3.0_Supplement_S1.xlsx"
  if (file.exists(gleam_file)) {
    gleam_raw <- extract_gleam_tables(gleam_file)
    message("\nExtracted ", length(gleam_raw), " GLEAM Excel tables")
    
    # Process and Rename GLEAM Tables
    gleam_excel_tables <- list()
    
    # Direct renames
    if (!is.null(gleam_raw$tab_s31)) gleam_excel_tables$gleam_crop_residue_params <- gleam_raw$tab_s31
    if (!is.null(gleam_raw$tab_s32)) gleam_excel_tables$gleam_feed_composition <- gleam_raw$tab_s32
    if (!is.null(gleam_raw$tab_s33)) gleam_excel_tables$gleam_feed_digestibility <- gleam_raw$tab_s33
    if (!is.null(gleam_raw$tab_s34)) gleam_excel_tables$gleam_feed_conversion_ratios <- gleam_raw$tab_s34
    if (!is.null(gleam_raw$tab_s91)) gleam_excel_tables$gleam_dressing_percentages <- gleam_raw$tab_s91
    
    # Special handling for geographic hierarchy (Tab. S.A1-S.A2)
    if (!is.null(gleam_raw$tab_sa1sa2)) {
      geo_raw <- gleam_raw$tab_sa1sa2
      # First row contains actual headers
      if (nrow(geo_raw) > 1) {
        col_names <- as.character(geo_raw[1, ])
        col_names <- clean_names(col_names)
        geo_data <- geo_raw[-1, ]
        names(geo_data) <- col_names
        gleam_excel_tables$gleam_geographic_hierarchy <- geo_data
      } else {
        gleam_excel_tables$gleam_geographic_hierarchy <- gleam_raw$tab_sa1sa2
      }
    }
    
    # Merge Production System Parameters (S.6.1 - S.6.9)
    s6_tables <- grep("tab_s6", names(gleam_raw), value = TRUE)
    if (length(s6_tables) > 0) {
      message("Merging ", length(s6_tables), " production system tables...")
      gleam_excel_tables$gleam_production_system_params <- dplyr::bind_rows(
        lapply(s6_tables, function(t) {
          df <- gleam_raw[[t]]
          df$source_table <- t
          df
        })
      )
    }
    
    # Merge Manure Management System Parameters (S.7.1 - S.7.7)
    s7_tables <- grep("tab_s7", names(gleam_raw), value = TRUE)
    if (length(s7_tables) > 0) {
      message("Merging ", length(s7_tables), " manure system tables...")
      gleam_excel_tables$gleam_manure_system_params <- dplyr::bind_rows(
        lapply(s7_tables, function(t) {
          df <- gleam_raw[[t]]
          df$source_table <- t
          df
        })
      )
    }
    
  } else {
    warning("GLEAM Excel file not found: ", gleam_file)
    gleam_excel_tables <- list()
  }
  
  # Generate GLEAM PDF tables
  message("\nGenerating GLEAM PDF tables...")
  gleam_pdf_tables <- generate_gleam_pdf_tables()
  message("Generated ", length(gleam_pdf_tables), " GLEAM PDF tables")
  
  # Generate IPCC 2019 tables
  message("\nGenerating IPCC 2019 tables...")
  ipcc_raw <- generate_ipcc_2019_tables()
  
  # Rename IPCC tables
  ipcc_2019 <- list(
    ipcc_2019_enteric_ef_cattle = ipcc_raw$table_10_10,
    ipcc_2019_enteric_ef_other = ipcc_raw$table_10_11,
    ipcc_2019_manure_ch4_ef_cattle = ipcc_raw$table_10_14_cattle,
    ipcc_2019_manure_ch4_ef_other = ipcc_raw$table_10_14_other,
    ipcc_2019_mcf_manure = ipcc_raw$table_10_17,
    ipcc_2019_n_excretion = ipcc_raw$table_10_19,
    ipcc_2019_n2o_ef_direct = ipcc_raw$table_10_21
  )
  message("Generated ", length(ipcc_2019), " IPCC 2019 tables")
  
  # Generate IPCC 2006 tables
  message("\nGenerating IPCC 2006 tables...")
  ipcc_2006 <- generate_ipcc_2006_tables()
  message("Generated ", length(ipcc_2006), " IPCC 2006 tables")
  
  # Generate IPCC Tier 2 parameters
  message("\nGenerating IPCC Tier 2 parameters...")
  ipcc_t2_raw <- generate_ipcc_tier2_params()
  
  # Rename Tier 2 parameters
  ipcc_tier2 <- list(
    ipcc_tier2_energy_coefs = ipcc_t2_raw$energy_coefs,
    ipcc_tier2_ym_values = ipcc_t2_raw$ym_values,
    ipcc_tier2_bo_values = ipcc_t2_raw$bo_values,
    ipcc_tier2_manure_ash = ipcc_t2_raw$ash_content,
    ipcc_tier2_n_retention = ipcc_t2_raw$n_retention_frac,
    livestock_production_defaults = ipcc_t2_raw$production_defaults,
    feed_characteristics = ipcc_t2_raw$feed_characteristics,
    climate_mcf = ipcc_t2_raw$climate_mcf,
    regional_mms_distribution = ipcc_t2_raw$regional_mms_distribution,
    temperature_adjustment = ipcc_t2_raw$temperature_adjustment,
    indirect_n2o_ef = ipcc_t2_raw$indirect_n2o_ef,
    uncertainty_ranges = ipcc_t2_raw$uncertainty_ranges
  )
  message("Generated ", length(ipcc_tier2), " parameter sets")
  
  # Combine all objects
  all_objects <- c(
    gleam_excel_tables,
    gleam_pdf_tables,
    ipcc_2019,
    ipcc_2006,
    ipcc_tier2,
    list(livestock_constants = livestock_constants)
  )
  
  # Save
  message("\nSaving ", length(all_objects), " objects to data/livestock_coefs.rda...")
  save(list = names(all_objects), 
       file = "data/livestock_coefs.rda", 
       compress = "xz",
       envir = list2env(all_objects, envir = new.env()))
  
  message("\n=== Complete ===\n")
  message("Total objects saved: ", length(all_objects))
  message("  GLEAM Excel tables: ", length(gleam_excel_tables))
  message("  GLEAM PDF tables: ", length(gleam_pdf_tables))
  message("  IPCC 2019 tables: ", length(ipcc_2019))
  message("  IPCC 2006 tables: ", length(ipcc_2006))
  message("  IPCC Tier 2 params: ", length(ipcc_tier2))
  message("  Constants: 1")
}

# Execute
main()
