
# Load packages
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}
library(openxlsx)
library(dplyr)

#' Extract GLEAM Coefficients
#'
#' @param path Path to GLEAM Excel file.
#' @return List of dataframes.
extract_gleam_coefs <- function(path) {
  if (!file.exists(path)) {
    warning("GLEAM file not found.")
    return(list())
  }
  
  sheets <- openxlsx::getSheetNames(path)
  coefs <- list()
  
  for (sheet in sheets) {
    if (sheet == "Table of contents") next
    tryCatch({
      coefs[[sheet]] <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE)
    }, error = function(e) {
      warning(paste("Could not read", sheet, ":", e$message))
    })
  }
  return(coefs)
}

#' Generate IPCC Tier 1 Tables
#'
#' @return List of dataframes.
generate_ipcc_tier1 <- function() {
  list(
    enteric = data.frame(
      species = c("Cattle", "Dairy Cattle", "Other Cattle", "Buffalo", "Sheep", "Goats", "Pigs", "Horses", "Mules", "Asses", "Poultry"),
      region = "Global",
      ef_enteric = c(55, 126, 52, 78, 9, 9, 1.5, 18, 10, 10, 0)
    ),
    manure = data.frame(
      species = c("Dairy Cattle", "Other Cattle", "Buffalo", "Sheep", "Goats", "Pigs", "Poultry"),
      region = "Global",
      temperature = "Temperate",
      ef_manure_ch4 = c(35, 2, 5, 0.20, 0.20, 7, 0.03),
      ef_manure_n2o = c(0.5, 0.5, 0.5, 0.1, 0.1, 0.5, 0.01)
    )
  )
}

#' Generate IPCC Tier 2 Tables
#'
#' @return List of dataframes.
generate_ipcc_tier2 <- function() {
  list(
    energy = data.frame(
      species = c("Cattle", "Sheep", "Goats"),
      Cfi = c(0.386, 0.236, 0.246),
      Ca_stall = c(0, 0, 0),
      Ca_pasture = c(0.17, 0.10, 0.10),
      Cp = c(0.1, 0.13, 0.13)
    ),
    methane = data.frame(
      species = c("Cattle", "Dairy Cattle", "Other Cattle", "Sheep", "Goats", "Pigs"),
      Ym = c(6.5, 6.5, 6.5, 6.5, 5.5, 0),
      Bo = c(0.24, 0.24, 0.24, 0.19, 0.18, 0.45),
      MCF_cool = c(0.10, 0.10, 0.10, 0.10, 0.10, 0.10),
      MCF_temp = c(0.15, 0.15, 0.15, 0.15, 0.15, 0.15),
      MCF_warm = c(0.20, 0.20, 0.20, 0.20, 0.20, 0.20)
    )
  )
}

#' Generate Additional Parameters (Weights, Feed, Manure)
#'
#' @return List of dataframes.
generate_additional_params <- function() {
  list(
    weights = data.frame(
      species = c("Cattle", "Cattle", "Cattle", "Cattle", "Sheep", "Goats", "Pigs"),
      cohort = c("Adult Female", "Adult Male", "Replacement Female", "Replacement Male", "Adult", "Adult", "Fattening"),
      weight_kg = c(550, 600, 300, 300, 45, 40, 50)
    ),
    feed = data.frame(
      species = c("Cattle", "Sheep", "Goats", "Pigs"),
      DE_percent = c(65, 65, 65, 75),
      UE_frac = c(0.04, 0.04, 0.04, 0.02),
      REM_ratio = c(0.50, 0.50, 0.50, 0.50),
      Ash_content = c(0.08, 0.08, 0.08, 0.04)
    ),
    manure = data.frame(
      species = c("Cattle", "Sheep", "Goats", "Pigs"),
      Nex_kg_head_yr = c(50, 12, 12, 10),
      EF_n2o_direct = c(0.005, 0.005, 0.005, 0.005)
    ),
    constants = list(
      energy_content_ch4 = 55.65,
      ch4_density = 0.67,
      vs_energy_content = 18.45,
      n2o_n_conversion = 44/28,
      days_in_year = 365
    )
  )
}

#' Generate GLEAM Default Shares
#'
#' @return List of dataframes.
generate_gleam_shares <- function() {
  list(
    cohort = data.frame(
      species = c(rep("Cattle", 4), rep("Sheep", 2), rep("Goats", 2), rep("Pigs", 2)),
      cohort = c("Adult Female", "Adult Male", "Replacement Female", "Replacement Male",
                 "Adult", "Young",
                 "Adult", "Young",
                 "Breeding", "Fattening"),
      share = c(0.4, 0.1, 0.25, 0.25,
                0.6, 0.4,
                0.6, 0.4,
                0.1, 0.9)
    ),
    system = data.frame(
      species = c("Cattle", "Cattle", "Sheep", "Sheep", "Goats", "Goats", "Pigs", "Pigs"),
      system = c("Grassland", "Mixed", "Grassland", "Mixed", "Grassland", "Mixed", "Backyard", "Intermediate"),
      share = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
    )
  )
}

#' Main Execution Function
#'
#' @return None. Saves data to file.
main <- function() {
  message("Starting coefficient generation...")
  
  # 1. GLEAM
  message("Extracting GLEAM data...")
  gleam_coefs <- extract_gleam_coefs("data-raw/GLEAM_3.0_Supplement_S1.xlsx")
  
  # 2. IPCC Tier 1
  message("Generating IPCC Tier 1...")
  t1 <- generate_ipcc_tier1()
  ipcc_tier1_enteric <- t1$enteric
  ipcc_tier1_manure <- t1$manure
  
  # 3. IPCC Tier 2
  message("Generating IPCC Tier 2...")
  t2 <- generate_ipcc_tier2()
  ipcc_tier2_energy <- t2$energy
  ipcc_tier2_methane <- t2$methane
  
  # 4. Additional Params
  message("Generating Additional Params...")
  add <- generate_additional_params()
  livestock_weights <- add$weights
  feed_params <- add$feed
  manure_params <- add$manure
  livestock_constants <- add$constants
  
  # 5. GLEAM Shares
  message("Generating GLEAM Shares...")
  shares <- generate_gleam_shares()
  gleam_default_cohort_shares <- shares$cohort
  gleam_default_system_shares <- shares$system
  
  # 6. Save
  message("Saving to data/livestock_coefs.rda ...")
  save(gleam_coefs, 
       ipcc_tier1_enteric, ipcc_tier1_manure, 
       gleam_default_cohort_shares, gleam_default_system_shares,
       ipcc_tier2_energy, ipcc_tier2_methane,
       livestock_weights, feed_params, manure_params, livestock_constants,
       file = "data/livestock_coefs.rda", compress = "xz")
  
  message("Done!")
}

# Execute
main()
