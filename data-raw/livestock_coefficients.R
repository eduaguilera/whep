# Livestock Coefficients Data Extraction
#
# This script extracts and documents all coefficient tables needed for
# livestock emissions calculations following GLEAM 3.0 and IPCC 2019/2006
# methodologies.
#
# Sources:
# - GLEAM 3.0: MacLeod et al. (2018)
#   https://doi.org/10.1088/1748-9326/aad4d8
#   Supplement S1: GLEAM_3.0_Supplement_S1.xlsx
# - IPCC 2019: 2019 Refinement to the 2006 IPCC Guidelines
#   Volume 4, Chapter 10: Emissions from Livestock and Manure Management
#   https://www.ipcc-nggip.iges.or.jp/public/2019rf/vol4.html
# - IPCC 2006: 2006 IPCC Guidelines for National GHG Inventories
#   Volume 4, Chapter 10

if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)

# Helper Functions ----

clean_names <- function(names) {
  names |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^[:alnum:]_]", "") |>
    tolower() |>
    make.unique(sep = "_")
}

# GLEAM Data Extraction ----

extract_gleam_tables <- function(path) {
  message("Extracting GLEAM tables from: ", path)

  temp_file <- tempfile(fileext = ".xlsx")
  file.copy(path, temp_file)
  on.exit(unlink(temp_file))

  tryCatch(
    {
      sheets <- openxlsx::getSheetNames(temp_file)
      tables <- list()

      for (sheet in sheets) {
        if (sheet == "Table of contents") {
          next
        }
        tryCatch(
          {
            df <- openxlsx::read.xlsx(temp_file, sheet = sheet, colNames = TRUE)
            names(df) <- clean_names(names(df))
            clean_sheet <- clean_names(sheet)
            tables[[clean_sheet]] <- df
            message("  Extracted: ", sheet)
          },
          error = function(e) {
            warning("  Failed to extract ", sheet, ": ", e$message)
          }
        )
      }
      tables
    },
    error = function(e) {
      warning("Failed to open GLEAM file: ", e$message)
      list()
    }
  )
}

# GLEAM PDF Tables ----

generate_gleam_pdf_tables <- function() {
  list(
    gleam_livestock_categories = tibble::tribble(
      ~species, ~production_system, ~cohort, ~description,
      "Cattle", "Dairy", "Adult Female",
        "Milking cows",
      "Cattle", "Dairy", "Adult Male",
        "Bulls",
      "Cattle", "Dairy", "Replacement Female",
        "Heifers",
      "Cattle", "Dairy", "Replacement Male",
        "Young bulls",
      "Cattle", "Dairy", "Surplus Female",
        "Culled heifers",
      "Cattle", "Dairy", "Surplus Male",
        "Calves for meat",
      "Cattle", "Beef", "Adult Female",
        "Breeding cows",
      "Cattle", "Beef", "Adult Male",
        "Bulls",
      "Cattle", "Beef", "Replacement Female",
        "Heifers",
      "Cattle", "Beef", "Replacement Male",
        "Young bulls",
      "Cattle", "Beef", "Fattening",
        "Fattening cattle",
      "Buffalo", "Dairy", "Adult Female",
        "Milking buffalo",
      "Buffalo", "Dairy", "Replacement",
        "Young buffalo",
      "Buffalo", "Other", "Adult",
        "Draft buffalo",
      "Sheep", "Dairy", "Adult Female",
        "Milking ewes",
      "Sheep", "Dairy", "Replacement",
        "Young ewes",
      "Sheep", "Meat", "Adult Female",
        "Breeding ewes",
      "Sheep", "Meat", "Fattening",
        "Lambs",
      "Goats", "Dairy", "Adult Female",
        "Milking goats",
      "Goats", "Dairy", "Replacement",
        "Young goats",
      "Goats", "Meat", "Adult Female",
        "Breeding goats",
      "Goats", "Meat", "Fattening",
        "Kids",
      "Pigs", "Breeding", "Sows",
        "Breeding sows",
      "Pigs", "Breeding", "Boars",
        "Breeding boars",
      "Pigs", "Fattening", "Fattening",
        "Fattening pigs",
      "Poultry", "Layers", "Layers",
        "Laying hens",
      "Poultry", "Broilers", "Broilers",
        "Meat chickens"
    ),

    gleam_feed_categories = tibble::tribble(
      ~feed_category, ~feed_type, ~description,
      "Grass",           "Pasture",    "Grazed grass and fodder",
      "Crop residues",   "Residues",   "Straw, stovers, husks",
      "Concentrates",    "Crops",      "Grains, oilseeds, pulses",
      "Fodder crops",    "Crops",      "Cultivated fodder",
      "Processed feeds", "Industrial", "Brans, meals, cakes",
      "Animal products", "Animal",     "Milk, fish meal"
    ),

    gleam_enteric_params = tibble::tribble(
      ~species,   ~system,   ~ym_percent, ~notes,
      "Cattle",   "Grazing", 6.5, "IPCC default",
      "Cattle",   "Mixed",   6.5, "IPCC default",
      "Cattle",   "Feedlot", 3.0, "High concentrate diet",
      "Buffalo",  "Grazing", 6.5, "IPCC default",
      "Buffalo",  "Mixed",   6.5, "IPCC default",
      "Sheep",    "Grazing", 6.5, "IPCC default",
      "Sheep",    "Mixed",   6.5, "IPCC default",
      "Goats",    "Grazing", 5.5, "IPCC default",
      "Goats",    "Mixed",   5.5, "IPCC default",
      "Pigs",     "All",     0.0, "Negligible enteric CH4"
    ),

    gleam_mms_shares = tibble::tribble(
      ~region, ~species, ~system, ~mms, ~share_percent,
      "Western Europe", "Cattle", "Dairy", "Liquid/Slurry", 60,
      "Western Europe", "Cattle", "Dairy", "Solid Storage", 30,
      "Western Europe", "Cattle", "Dairy", "Pasture", 10,
      "Western Europe", "Cattle", "Beef", "Pasture", 70,
      "Western Europe", "Cattle", "Beef", "Solid Storage", 30,
      "Sub-Saharan Africa", "Cattle", "All", "Pasture", 90,
      "Sub-Saharan Africa", "Cattle", "All",
        "Daily Spread", 10,
      "Latin America", "Cattle", "All", "Pasture", 95,
      "Latin America", "Cattle", "All", "Solid Storage", 5,
      "South Asia", "Cattle", "All", "Daily Spread", 60,
      "South Asia", "Cattle", "All", "Solid Storage", 30,
      "South Asia", "Cattle", "All", "Pasture", 10,
      "East Asia", "Pigs", "All", "Liquid/Slurry", 70,
      "East Asia", "Pigs", "All", "Solid Storage", 30
    ),

    gleam_animal_weights = tibble::tribble(
      ~region, ~species, ~system, ~cohort, ~weight_kg,
      "Western Europe", "Cattle", "Dairy",
        "Adult Female", 650,
      "Western Europe", "Cattle", "Dairy",
        "Adult Male", 1000,
      "Western Europe", "Cattle", "Beef",
        "Adult Female", 600,
      "Western Europe", "Cattle", "Beef",
        "Fattening", 400,
      "North America", "Cattle", "Dairy",
        "Adult Female", 680,
      "North America", "Cattle", "Dairy",
        "Adult Male", 1000,
      "North America", "Cattle", "Beef",
        "Adult Female", 550,
      "North America", "Cattle", "Beef",
        "Fattening", 450,
      "Sub-Saharan Africa", "Cattle", "All",
        "Adult Female", 250,
      "Sub-Saharan Africa", "Cattle", "All",
        "Adult Male", 350,
      "South Asia", "Cattle", "Dairy",
        "Adult Female", 350,
      "South Asia", "Buffalo", "Dairy",
        "Adult Female", 450,
      "Latin America", "Cattle", "Beef",
        "Adult Female", 450,
      "Latin America", "Cattle", "Beef",
        "Fattening", 350,
      "Global", "Cattle", "All",
        "Adult Female", 400,
      "Global", "Cattle", "All",
        "Adult Male", 600,
      "Global", "Cattle", "All",
        "Fattening", 300,
      "Global", "Sheep", "All", "Adult", 45,
      "Global", "Goats", "All", "Adult", 40,
      "Global", "Pigs", "Fattening",
        "Fattening", 50,
      "Global", "Pigs", "Breeding", "Sows", 200
    ),

    gleam_milk_production = tibble::tribble(
      ~region, ~species, ~system,
        ~milk_kg_head_yr, ~lactation_days,
      "Western Europe", "Cattle", "Dairy",   7500, 305,
      "North America",  "Cattle", "Dairy",   9500, 305,
      "Oceania",        "Cattle", "Dairy",   5500, 270,
      "Latin America",  "Cattle", "Dairy",   2500, 240,
      "Sub-Saharan Africa", "Cattle", "Dairy", 800, 180,
      "South Asia",     "Cattle", "Dairy",   1500, 240,
      "South Asia",     "Buffalo", "Dairy",  1800, 270,
      "Western Europe", "Sheep", "Dairy",     200, 180,
      "Western Europe", "Goats", "Dairy",     450, 240
    )
  )
}

# IPCC 2019 Refinement Tables ----

generate_ipcc_2019_tables <- function() {
  list(
    # Table 10.10: Enteric Fermentation EF - Cattle (kg CH4/head/yr).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.10.
    table_10_10 = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr,
      "North America",        "Dairy Cattle",  128,
      "North America",        "Other Cattle",   53,
      "Western Europe",       "Dairy Cattle",  117,
      "Western Europe",       "Other Cattle",   57,
      "Eastern Europe",       "Dairy Cattle",   99,
      "Eastern Europe",       "Other Cattle",   58,
      "Oceania",              "Dairy Cattle",   90,
      "Oceania",              "Other Cattle",   60,
      "Latin America",        "Dairy Cattle",   72,
      "Latin America",        "Other Cattle",   56,
      "Asia",                 "Dairy Cattle",   68,
      "Asia",                 "Other Cattle",   47,
      "Africa",               "Dairy Cattle",   46,
      "Africa",               "Other Cattle",   31,
      "Middle East",          "Dairy Cattle",   63,
      "Middle East",          "Other Cattle",   31,
      "Indian Subcontinent",  "Dairy Cattle",   68,
      "Indian Subcontinent",  "Other Cattle",   47,
      "Global",               "Dairy Cattle",   80,
      "Global",               "Other Cattle",   47
    ),

    # Table 10.11: Enteric Fermentation EF - Other Livestock.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.11.
    table_10_11 = tibble::tribble(
      ~category,             ~ef_kg_head_yr,
      "Buffalo",              55,
      "Sheep",                 8,
      "Goats",                 5,
      "Camels",               46,
      "Horses",               18,
      "Mules and Asses",      10,
      "Swine - Market",        1.5,
      "Swine - Breeding",      1.5,
      "Poultry",               0
    ),

    # Table 10.14: Manure Management CH4 EF - Cattle.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.14.
    table_10_14_cattle = tibble::tribble(
      ~region, ~category, ~climate, ~ef_kg_head_yr,
      "North America",   "Dairy Cattle", "Cool",      27,
      "North America",   "Dairy Cattle", "Temperate", 42,
      "North America",   "Dairy Cattle", "Warm",      60,
      "North America",   "Other Cattle", "Cool",       2,
      "North America",   "Other Cattle", "Temperate",  3,
      "North America",   "Other Cattle", "Warm",       4,
      "Western Europe",  "Dairy Cattle", "Cool",      31,
      "Western Europe",  "Dairy Cattle", "Temperate", 39,
      "Western Europe",  "Other Cattle", "Cool",       1,
      "Western Europe",  "Other Cattle", "Temperate",  1,
      "Eastern Europe",  "Dairy Cattle", "Cool",      21,
      "Eastern Europe",  "Dairy Cattle", "Temperate", 23,
      "Eastern Europe",  "Other Cattle", "Cool",       1,
      "Eastern Europe",  "Other Cattle", "Temperate",  1,
      "Oceania",         "Dairy Cattle", "Temperate", 24,
      "Oceania",         "Other Cattle", "Temperate",  1,
      "Latin America",   "Dairy Cattle", "Warm",      47,
      "Latin America",   "Other Cattle", "Warm",       1,
      "Africa",          "Dairy Cattle", "Warm",      31,
      "Africa",          "Other Cattle", "Warm",       1,
      "Middle East",     "Dairy Cattle", "Warm",      31,
      "Middle East",     "Other Cattle", "Warm",       1,
      "Asia",            "Dairy Cattle", "Warm",      39,
      "Asia",            "Other Cattle", "Warm",       2,
      "Global",          "Dairy Cattle", "Temperate", 36,
      "Global",          "Other Cattle", "Temperate",  2
    ),

    # Table 10.14: Manure Management CH4 EF - Other Livestock.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.14.
    table_10_14_other = tibble::tribble(
      ~category,                ~climate, ~ef_kg_head_yr,
      "Buffalo",                "All",     2,
      "Sheep",                  "All",     0.19,
      "Goats",                  "All",     0.13,
      "Swine - Market",         "All",     6,
      "Swine - Breeding",       "All",     6,
      "Poultry - Broilers",     "All",     0.02,
      "Poultry - Layers",       "All",     0.03,
      "Horses",                 "All",     1.64,
      "Mules and Asses",        "All",     0.90,
      "Camels",                 "All",     1.92
    ),

    # Table 10.12: Ym values (% GE converted to CH4).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.
    # Note: The 2019 Refinement introduced feedlot distinction
    # and body-weight distinction for sheep.
    table_10_12 = tibble::tribble(
      ~category, ~feed_situation, ~ym_percent, ~ym_uncertainty,
      "Cattle",  "Pasture/Range",          6.5, 1.0,
      "Cattle",  "Mixed",                  6.5, 1.0,
      "Cattle",  "Feedlot (>90% conc.)",   3.0, 1.0,
      "Buffalo", "Pasture/Range",          6.5, 1.0,
      "Buffalo", "Mixed",                  6.5, 1.0,
      "Sheep",   "Large body (>=75kg)",    6.7, 1.0,
      "Sheep",   "Small body (<75kg)",     4.7, 1.0,
      "Goats",   "All",                    5.5, 1.0,
      "Camels",  "All",                    5.0, 1.0
    ),

    # Table 10.16: Bo - Maximum CH4 producing capacity (m3/kg VS).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16.
    # Note: Dairy and Other cattle have DIFFERENT Bo values.
    table_10_16 = tibble::tribble(
      ~category,            ~bo_m3_kg_vs,
      "Dairy Cattle",        0.24,
      "Other Cattle",        0.18,
      "Buffalo",             0.10,
      "Swine - Market",      0.45,
      "Swine - Breeding",    0.27,
      "Sheep",               0.19,
      "Goats",               0.18,
      "Horses",              0.33,
      "Mules and Asses",     0.33,
      "Camels",              0.10,
      "Poultry - Layers",    0.39,
      "Poultry - Broilers",  0.24
    ),

    # Table 10.17: MCF by MMS type and annual average temperature.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.17.
    # Note: Full temperature detail (Cool <=10, Temperate 11-25,
    # Warm >=26). The 2019 Refinement provides MCF for each
    # degree Celsius; we use representative values.
    table_10_17 = tibble::tribble(
      ~system, ~climate_zone, ~mcf_percent,
      "Pasture/Range/Paddock",         "Cool",       1.0,
      "Pasture/Range/Paddock",         "Temperate",  1.5,
      "Pasture/Range/Paddock",         "Warm",       2.0,
      "Daily Spread",                  "Cool",       0.1,
      "Daily Spread",                  "Temperate",  0.5,
      "Daily Spread",                  "Warm",       1.0,
      "Solid Storage",                 "Cool",       2.0,
      "Solid Storage",                 "Temperate",  4.0,
      "Solid Storage",                 "Warm",       5.0,
      "Dry Lot",                       "Cool",       1.5,
      "Dry Lot",                       "Temperate",  2.5,
      "Dry Lot",                       "Warm",       4.0,
      "Liquid/Slurry - No Crust",      "Cool",      17.0,
      "Liquid/Slurry - No Crust",      "Temperate", 35.0,
      "Liquid/Slurry - No Crust",      "Warm",      80.0,
      "Liquid/Slurry - With Crust",    "Cool",      10.0,
      "Liquid/Slurry - With Crust",    "Temperate", 17.0,
      "Liquid/Slurry - With Crust",    "Warm",      47.0,
      "Uncovered Anaerobic Lagoon",    "Cool",      66.0,
      "Uncovered Anaerobic Lagoon",    "Temperate", 73.0,
      "Uncovered Anaerobic Lagoon",    "Warm",      80.0,
      "Pit Storage - <1 month",        "Cool",       3.0,
      "Pit Storage - <1 month",        "Temperate",  3.0,
      "Pit Storage - <1 month",        "Warm",       5.0,
      "Pit Storage - >1 month",        "Cool",      17.0,
      "Pit Storage - >1 month",        "Temperate", 35.0,
      "Pit Storage - >1 month",        "Warm",      80.0,
      "Anaerobic Digester",            "All",        0.0,
      "Burned for Fuel",               "All",       10.0,
      "Composting - In-vessel",        "Cool",       0.5,
      "Composting - In-vessel",        "Temperate",  0.5,
      "Composting - In-vessel",        "Warm",       0.5,
      "Composting - Static Pile",      "Cool",       0.5,
      "Composting - Static Pile",      "Temperate",  0.5,
      "Composting - Static Pile",      "Warm",       0.5,
      "Composting - Intensive",        "Cool",       0.5,
      "Composting - Intensive",        "Temperate",  0.5,
      "Composting - Intensive",        "Warm",       0.5,
      "Composting - Passive",          "Cool",       1.0,
      "Composting - Passive",          "Temperate",  1.0,
      "Composting - Passive",          "Warm",       1.5,
      "Poultry Manure - High Rise",    "Cool",       1.5,
      "Poultry Manure - High Rise",    "Temperate",  1.5,
      "Poultry Manure - High Rise",    "Warm",       1.5,
      "Poultry Manure - Deep Litter",  "Cool",       1.5,
      "Poultry Manure - Deep Litter",  "Temperate",  1.5,
      "Poultry Manure - Deep Litter",  "Warm",       1.5
    ),

    # Table 10.19: N Excretion Rates (kg N/head/yr).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.19.
    table_10_19 = tibble::tribble(
      ~region, ~category, ~nex_kg_n_head_yr,
      "North America",        "Dairy Cattle",      105,
      "North America",        "Other Cattle",       56,
      "Western Europe",       "Dairy Cattle",      100,
      "Western Europe",       "Other Cattle",       50,
      "Eastern Europe",       "Dairy Cattle",       80,
      "Eastern Europe",       "Other Cattle",       50,
      "Oceania",              "Dairy Cattle",       80,
      "Oceania",              "Other Cattle",       40,
      "Latin America",        "Dairy Cattle",       50,
      "Latin America",        "Other Cattle",       40,
      "Africa",               "Dairy Cattle",       40,
      "Africa",               "Other Cattle",       30,
      "Middle East",          "Dairy Cattle",       40,
      "Middle East",          "Other Cattle",       30,
      "Asia",                 "Dairy Cattle",       50,
      "Asia",                 "Other Cattle",       40,
      "Indian Subcontinent",  "Dairy Cattle",       50,
      "Indian Subcontinent",  "Other Cattle",       40,
      "Global",               "Dairy Cattle",       70,
      "Global",               "Other Cattle",       40,
      "Global",               "Buffalo",            55,
      "Global",               "Sheep",              12,
      "Global",               "Goats",              12,
      "Global",               "Swine - Market",     15,
      "Global",               "Swine - Breeding",   18,
      "Global",               "Horses",             50,
      "Global",               "Mules and Asses",    35,
      "Global",               "Camels",             46,
      "Global",               "Poultry - Broilers",  0.6,
      "Global",               "Poultry - Layers",    0.8
    ),

    # Table 10.21: Direct N2O Emission Factors
    # (kg N2O-N per kg N in MMS).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.21.
    table_10_21 = tibble::tribble(
      ~system,                          ~ef_kg_n2o_n_per_kg_n,
      "Uncovered Anaerobic Lagoon",      0.001,
      "Liquid/Slurry - No Crust",        0.002,
      "Liquid/Slurry - With Crust",      0.005,
      "Liquid/Slurry",                   0.002,
      "Solid Storage and Dry Lot",       0.005,
      "Solid Storage",                   0.005,
      "Dry Lot",                         0.005,
      "Pasture/Range/Paddock",           0.01,
      "Daily Spread",                    0.01,
      "Anaerobic Digester",              0.0,
      "Burned for Fuel",                 0.0,
      "Composting - In-vessel",          0.006,
      "Composting - Static Pile",        0.006,
      "Composting - Intensive",          0.006,
      "Composting - Passive",            0.01,
      "Poultry Manure - High Rise",      0.001,
      "Poultry Manure - Deep Litter",    0.001,
      "Other",                           0.005
    ),

    # Table 10.4: Cfi coefficients (MJ/day/kg^0.75).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.3.
    # Note: Cfi differs between lactating and non-lactating
    # cattle; the 2019 Refinement does NOT change these from
    # the 2006 values.
    table_10_4 = tibble::tribble(
      ~category, ~subcategory, ~cfi_mj_day_kg075,
      "Cattle",  "Lactating cow",          0.386,
      "Cattle",  "Non-lactating/Bulls",    0.322,
      "Buffalo", "Lactating cow",          0.386,
      "Buffalo", "Non-lactating/Bulls",    0.322,
      "Sheep",   "All",                    0.217,
      "Goats",   "All",                    0.217
    )
  )
}

# IPCC 2006 Tables ----

generate_ipcc_2006_tables <- function() {
  list(
    # Table 10.11 (2006): Tier 1 Enteric Fermentation EFs.
    ipcc_2006_enteric_ef = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr,
      "North America",   "Dairy Cattle",     128,
      "North America",   "Other Cattle",      53,
      "Western Europe",  "Dairy Cattle",     117,
      "Western Europe",  "Other Cattle",      57,
      "Eastern Europe",  "Dairy Cattle",      99,
      "Eastern Europe",  "Other Cattle",      58,
      "Oceania",         "Dairy Cattle",      90,
      "Oceania",         "Other Cattle",      60,
      "Latin America",   "Dairy Cattle",      72,
      "Latin America",   "Other Cattle",      56,
      "Asia",            "Dairy Cattle",      68,
      "Asia",            "Other Cattle",      47,
      "Africa",          "Dairy Cattle",      46,
      "Africa",          "Other Cattle",      31,
      "Middle East",     "Dairy Cattle",      46,
      "Middle East",     "Other Cattle",      31,
      "Global",          "Buffalo",           55,
      "Global",          "Sheep",              8,
      "Global",          "Goats",              5,
      "Global",          "Swine",              1.5,
      "Global",          "Horses",            18,
      "Global",          "Mules and Asses",   10
    ),

    # Table 10.14 (2006): Tier 1 Manure CH4 EFs.
    ipcc_2006_manure_ef = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr, ~temp_zone,
      "North America",   "Dairy Cattle",  53,  "Cool",
      "North America",   "Other Cattle",   2,  "Cool",
      "Western Europe",  "Dairy Cattle",  20,  "Cool",
      "Western Europe",  "Other Cattle",   6,  "Cool",
      "Latin America",   "Dairy Cattle",   1,  "Warm",
      "Latin America",   "Other Cattle",   1,  "Warm",
      "Asia",            "Dairy Cattle",  16,  "Warm",
      "Asia",            "Other Cattle",   1,  "Warm",
      "Global",          "Buffalo",        2,  "Warm",
      "Global",          "Sheep",          0.19, "All",
      "Global",          "Goats",          0.13, "All",
      "Global",          "Swine",          6,  "All",
      "Global",          "Poultry",        0.02, "All"
    ),

    # Table 10.17 (2006): MCF by Temperature.
    ipcc_2006_mcf_temp = tibble::tribble(
      ~system, ~temp_c, ~mcf_percent,
      "Liquid/Slurry",          10, 17,
      "Liquid/Slurry",          15, 25,
      "Liquid/Slurry",          20, 35,
      "Liquid/Slurry",          25, 48,
      "Solid Storage",          10,  2,
      "Solid Storage",          15,  4,
      "Solid Storage",          20,  5,
      "Solid Storage",          25,  6,
      "Pasture/Range/Paddock",  10,  1,
      "Pasture/Range/Paddock",  15,  1.5,
      "Pasture/Range/Paddock",  20,  2,
      "Pasture/Range/Paddock",  25,  2.5,
      "Daily Spread",           10,  0.1,
      "Daily Spread",           15,  0.5,
      "Daily Spread",           20,  1,
      "Daily Spread",           25,  1.5
    )
  )
}

# IPCC Tier 2 Parameters ----

generate_ipcc_tier2_params <- function() {
  list(
    # Energy coefficients for Tier 2 GE calculation.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.3-10.16.
    # Note: Cfi now distinguishes dairy (lactating) vs
    # non-dairy (non-lactating/bulls).
    # Ca = activity coefficient (IPCC Eq 10.4).
    # Cp = pregnancy coefficient (IPCC Eq 10.13).
    # REG_gain_mj_kg = typical energy content of gain
    #   (used with REG in Eq 10.6).
    energy_coefs = tibble::tribble(
      ~category, ~subcategory,
        ~cfi_mj_day_kg075, ~ca_pasture, ~ca_feedlot,
        ~cp, ~cw, ~energy_content_gain_mj_kg,
      "Cattle", "Dairy",
        0.386, 0.17, 0.00, 0.10, 0.00, 22.0,
      "Cattle", "Non-Dairy",
        0.322, 0.17, 0.00, 0.10, 0.00, 22.0,
      "Buffalo", "Dairy",
        0.386, 0.17, 0.00, 0.10, 0.00, 20.0,
      "Buffalo", "Non-Dairy",
        0.322, 0.17, 0.00, 0.10, 0.00, 20.0,
      "Sheep", "All",
        0.217, 0.0107, 0.00, 0.077, 0.00, 23.0,
      "Goats", "All",
        0.217, 0.0107, 0.00, 0.077, 0.00, 23.0
    ),

    # Ym values (% GE) used for Tier 2 enteric CH4.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.
    # Note: The 2019 Refinement differentiates cattle by
    # feed situation and sheep by body weight.
    ym_values = tibble::tribble(
      ~category, ~feed_situation, ~ym_percent,
      "Cattle",  "High",    6.5,
      "Cattle",  "Medium",  6.5,
      "Cattle",  "Low",     6.5,
      "Cattle",  "Feedlot", 3.0,
      "Buffalo", "High",    6.5,
      "Buffalo", "Medium",  6.5,
      "Buffalo", "Low",     6.5,
      "Sheep",   "High",    6.7,
      "Sheep",   "Medium",  6.7,
      "Sheep",   "Low",     4.7,
      "Goats",   "High",    5.5,
      "Goats",   "Medium",  5.5,
      "Goats",   "Low",     5.5,
      "Camels",  "High",    5.0,
      "Camels",  "Medium",  5.0,
      "Camels",  "Low",     5.0
    ),

    # Bo - Maximum CH4 producing capacity (m3 CH4/kg VS).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16.
    # Note: Dairy and non-dairy cattle have DIFFERENT Bo.
    bo_values = tibble::tribble(
      ~category,             ~bo_m3_kg_vs,
      "Dairy Cattle",         0.24,
      "Other Cattle",         0.18,
      "Buffalo",              0.10,
      "Swine - Market",       0.45,
      "Swine - Breeding",     0.27,
      "Sheep",                0.19,
      "Goats",                0.18,
      "Horses",               0.33,
      "Mules and Asses",      0.33,
      "Camels",               0.10,
      "Poultry - Layers",     0.39,
      "Poultry - Broilers",   0.24
    ),

    # Ash content of manure (%).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.24.
    ash_content = tibble::tribble(
      ~category,          ~ash_percent,
      "Cattle",             8.0,
      "Buffalo",            8.0,
      "Sheep",              8.0,
      "Goats",              8.0,
      "Swine",              4.0,
      "Poultry",           25.0,
      "Horses",            20.0,
      "Camels",             8.0,
      "Mules and Asses",    8.0
    ),

    # Feed Characteristics by diet quality.
    # Source: IPCC 2019, Vol 4, Ch 10, typical values.
    feed_characteristics = tibble::tribble(
      ~diet_quality, ~de_percent, ~ndf_percent,
        ~ge_content_mj_kg_dm, ~cp_percent,
      "High",    75.0, 35.0, 18.45, 16.0,
      "Medium",  65.0, 50.0, 18.45, 12.0,
      "Low",     55.0, 65.0, 18.45,  8.0
    ),

    # Nitrogen Retention Fractions.
    # Source: IPCC 2019, Vol 4, Ch 10, Table 10.20.
    n_retention_frac = tibble::tribble(
      ~category,          ~n_retention_frac,
      "Dairy Cattle",       0.20,
      "Other Cattle",       0.07,
      "Buffalo",            0.20,
      "Sheep",              0.10,
      "Goats",              0.10,
      "Swine",              0.30,
      "Poultry",            0.30,
      "Horses",             0.05,
      "Camels",             0.05,
      "Mules and Asses",    0.05
    ),

    # Default Production Parameters.
    # Source: NRC 2001 / IPCC 2019 Ch 10.
    production_defaults = tibble::tribble(
      ~category, ~fat_percent, ~protein_percent,
        ~lactose_percent, ~weight_gain_kg_day,
        ~work_hours_day, ~pregnant_fraction,
      "Dairy Cattle", 4.0, 3.2,  4.85, 0.0, 0.0, 0.9,
      "Other Cattle", 0.0, 0.0,  0.0,  0.5, 0.0, 0.0,
      "Buffalo",      7.0, 4.5,  4.9,  0.2, 2.0, 0.6,
      "Sheep",        7.0, 5.5,  4.8,  0.1, 0.0, 0.5,
      "Goats",        4.0, 3.5,  4.5,  0.05, 0.0, 0.5,
      "Swine",        0.0, 0.0,  0.0,  0.6, 0.0, 0.0,
      "Poultry",      0.0, 0.0,  0.0,  0.05, 0.0, 0.0,
      "Horses",       0.0, 0.0,  0.0,  0.0, 4.0, 0.0,
      "Camels",       4.0, 3.7,  5.0,  0.0, 2.0, 0.4,
      "Mules and Asses", 0.0, 0.0, 0.0, 0.0, 6.0, 0.0
    ),

    # Climate-zone MCF (Methane Conversion Factor).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.17.
    # This is the simplified 3-zone version. Full table is in
    # table_10_17 above.
    climate_mcf = tibble::tribble(
      ~mms_type, ~climate_zone, ~mcf_percent,
      "Daily Spread",              "Cool",       0.1,
      "Daily Spread",              "Temperate",  0.5,
      "Daily Spread",              "Warm",       1.0,
      "Solid Storage",             "Cool",       2.0,
      "Solid Storage",             "Temperate",  4.0,
      "Solid Storage",             "Warm",       5.0,
      "Dry Lot",                   "Cool",       1.5,
      "Dry Lot",                   "Temperate",  2.5,
      "Dry Lot",                   "Warm",       4.0,
      "Liquid/Slurry",             "Cool",      17.0,
      "Liquid/Slurry",             "Temperate", 35.0,
      "Liquid/Slurry",             "Warm",      80.0,
      "Anaerobic Lagoon",          "Cool",      66.0,
      "Anaerobic Lagoon",          "Temperate", 73.0,
      "Anaerobic Lagoon",          "Warm",      80.0,
      "Pasture/Range/Paddock",     "Cool",       1.0,
      "Pasture/Range/Paddock",     "Temperate",  1.5,
      "Pasture/Range/Paddock",     "Warm",       2.0,
      "Poultry Manure",            "Cool",       1.5,
      "Poultry Manure",            "Temperate",  1.5,
      "Poultry Manure",            "Warm",       1.5,
      "Composting - Intensive",    "All",        0.5,
      "Composting - Passive",      "All",        1.0,
      "Anaerobic Digester",        "All",        0.0,
      "Burned for Fuel",           "All",       10.0
    ),

    # Regional MMS Distribution.
    # Source: GLEAM 3.0 / FAO statistics (simplified).
    regional_mms_distribution = tibble::tribble(
      ~region, ~species, ~mms_type, ~fraction,
      "North America", "Cattle",
        "Liquid/Slurry", 0.40,
      "North America", "Cattle",
        "Solid Storage", 0.30,
      "North America", "Cattle",
        "Pasture/Range/Paddock", 0.25,
      "North America", "Cattle",
        "Daily Spread", 0.05,
      "Western Europe", "Cattle",
        "Liquid/Slurry", 0.35,
      "Western Europe", "Cattle",
        "Solid Storage", 0.45,
      "Western Europe", "Cattle",
        "Pasture/Range/Paddock", 0.15,
      "Western Europe", "Cattle",
        "Daily Spread", 0.05,
      "Latin America and Caribbean", "Cattle",
        "Pasture/Range/Paddock", 0.70,
      "Latin America and Caribbean", "Cattle",
        "Solid Storage", 0.15,
      "Latin America and Caribbean", "Cattle",
        "Daily Spread", 0.10,
      "Latin America and Caribbean", "Cattle",
        "Liquid/Slurry", 0.05,
      "Global", "Cattle",
        "Pasture/Range/Paddock", 0.50,
      "Global", "Cattle",
        "Solid Storage", 0.30,
      "Global", "Cattle",
        "Liquid/Slurry", 0.15,
      "Global", "Cattle",
        "Daily Spread", 0.05,
      "North America", "Swine",
        "Liquid/Slurry", 0.70,
      "North America", "Swine",
        "Solid Storage", 0.20,
      "North America", "Swine",
        "Anaerobic Lagoon", 0.10,
      "Global", "Swine",
        "Liquid/Slurry", 0.50,
      "Global", "Swine",
        "Solid Storage", 0.40,
      "Global", "Swine",
        "Daily Spread", 0.10,
      "Global", "Poultry",
        "Poultry Manure", 0.80,
      "Global", "Poultry",
        "Solid Storage", 0.20,
      "Global", "Sheep",
        "Pasture/Range/Paddock", 1.0,
      "Global", "Goats",
        "Pasture/Range/Paddock", 1.0,
      "Global", "Buffalo",
        "Pasture/Range/Paddock", 0.60,
      "Global", "Buffalo",
        "Daily Spread", 0.30,
      "Global", "Buffalo",
        "Solid Storage", 0.10,
      "Global", "Horses",
        "Pasture/Range/Paddock", 0.80,
      "Global", "Horses",
        "Solid Storage", 0.20,
      "Global", "Camels",
        "Pasture/Range/Paddock", 1.0,
      "Global", "Mules and Asses",
        "Pasture/Range/Paddock", 1.0
    ),

    # Temperature Adjustment Coefficients for NEm.
    # Source: NRC 2001 / IPCC 2019.
    temperature_adjustment = tibble::tribble(
      ~temp_range,     ~temp_min, ~temp_max, ~adjustment_factor,
      "Cold Stress",   -Inf,       5,         0.20,
      "Thermoneutral",    5,      25,         0.00,
      "Heat Stress",     25,     Inf,         0.10
    ),

    # Indirect N2O Emission Factors.
    # Source: IPCC 2019, Vol 4, Ch 10, Table 10.22 and
    # Vol 4, Ch 11, Table 11.3.
    indirect_n2o_ef = tibble::tribble(
      ~parameter,      ~value, ~description,
      "ef4_volatilization", 0.010,
        "EF4: N2O-N per kg NH3-N + NOx-N volatilized",
      "ef5_leaching",       0.0075,
        "EF5: N2O-N per kg N leached/runoff",
      "frac_gasms",         0.20,
        "FracGasMS: fraction N lost as NH3+NOx from MMS",
      "frac_leach",         0.30,
        "FracLeach: fraction N lost via leaching/runoff"
    ),

    # Uncertainty Ranges for key parameters.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10.
    uncertainty_ranges = tibble::tribble(
      ~parameter, ~lower_mult, ~upper_mult, ~distribution,
      "Ym",       0.85, 1.15, "normal",
      "MCF",      0.70, 1.30, "normal",
      "Bo",       0.80, 1.20, "normal",
      "EF_N2O",   0.50, 2.00, "lognormal",
      "Nex",      0.90, 1.10, "normal"
    ),

    # Grazing energy coefficients.
    # Source: NRC 2001 (converted 0.00045 Mcal/kg/km to MJ).
    grazing_energy_coefs = tibble::tribble(
      ~parameter, ~value_mj_kg_km, ~source,
      "walking_energy_cost", 0.0019,
        "NRC 2001 (0.00045 Mcal/kg/km)"
    )
  )
}

# Physical Constants ----

livestock_constants <- list(
  energy_content_ch4_mj_kg = 55.65,
  ch4_density_kg_m3 = 0.67,
  vs_energy_content_mj_kg = 18.45,
  n_to_n2o = 44 / 28,
  days_in_year = 365,
  default_de_percent = 65,
  default_ue_fraction = 0.04,
  # Eq 10.12 default Ym for sheep wool production.
  ev_wool_mj_kg = 24.0
)

# Main ----

main <- function() {
  message("\n=== Livestock Coefficients Data Generation ===\n")

  # GLEAM Excel tables
  gleam_file <- "data-raw/GLEAM_3.0_Supplement_S1.xlsx"
  gleam_excel_tables <- list()
  if (file.exists(gleam_file)) {
    gleam_raw <- extract_gleam_tables(gleam_file)
    message("\nExtracted ", length(gleam_raw), " GLEAM Excel tables")

    if (!is.null(gleam_raw$tab_s31)) {
      gleam_excel_tables$gleam_crop_residue_params <-
        gleam_raw$tab_s31
    }
    if (!is.null(gleam_raw$tab_s32)) {
      gleam_excel_tables$gleam_feed_composition <-
        gleam_raw$tab_s32
    }
    if (!is.null(gleam_raw$tab_s33)) {
      gleam_excel_tables$gleam_feed_digestibility <-
        gleam_raw$tab_s33
    }
    if (!is.null(gleam_raw$tab_s34)) {
      gleam_excel_tables$gleam_feed_conversion_ratios <-
        gleam_raw$tab_s34
    }
    if (!is.null(gleam_raw$tab_s91)) {
      gleam_excel_tables$gleam_dressing_percentages <-
        gleam_raw$tab_s91
    }

    if (!is.null(gleam_raw$tab_sa1sa2)) {
      geo_raw <- gleam_raw$tab_sa1sa2
      if (nrow(geo_raw) > 1) {
        col_names <- clean_names(as.character(geo_raw[1, ]))
        geo_data <- geo_raw[-1, ]
        names(geo_data) <- col_names
        gleam_excel_tables$gleam_geographic_hierarchy <- geo_data
      } else {
        gleam_excel_tables$gleam_geographic_hierarchy <-
          gleam_raw$tab_sa1sa2
      }
    }

    s6_tables <- grep("tab_s6", names(gleam_raw), value = TRUE)
    if (length(s6_tables) > 0) {
      gleam_excel_tables$gleam_production_system_params <-
        dplyr::bind_rows(
          lapply(s6_tables, function(t) {
            df <- gleam_raw[[t]]
            df$source_table <- t
            df
          })
        )
    }

    s7_tables <- grep("tab_s7", names(gleam_raw), value = TRUE)
    if (length(s7_tables) > 0) {
      gleam_excel_tables$gleam_manure_system_params <-
        dplyr::bind_rows(
          lapply(s7_tables, function(t) {
            df <- gleam_raw[[t]]
            df$source_table <- t
            df
          })
        )
    }
  } else {
    warning("GLEAM Excel file not found: ", gleam_file)
  }

  # GLEAM PDF tables
  message("\nGenerating GLEAM PDF tables...")
  gleam_pdf_tables <- generate_gleam_pdf_tables()

  # IPCC 2019 tables
  message("\nGenerating IPCC 2019 tables...")
  ipcc_raw <- generate_ipcc_2019_tables()
  ipcc_2019 <- list(
    ipcc_2019_enteric_ef_cattle = ipcc_raw$table_10_10,
    ipcc_2019_enteric_ef_other = ipcc_raw$table_10_11,
    ipcc_2019_manure_ch4_ef_cattle = ipcc_raw$table_10_14_cattle,
    ipcc_2019_manure_ch4_ef_other = ipcc_raw$table_10_14_other,
    ipcc_2019_mcf_manure = ipcc_raw$table_10_17,
    ipcc_2019_n_excretion = ipcc_raw$table_10_19,
    ipcc_2019_n2o_ef_direct = ipcc_raw$table_10_21,
    ipcc_2019_ym = ipcc_raw$table_10_12,
    ipcc_2019_bo = ipcc_raw$table_10_16,
    ipcc_2019_cfi = ipcc_raw$table_10_4
  )

  # IPCC 2006 tables
  message("\nGenerating IPCC 2006 tables...")
  ipcc_2006 <- generate_ipcc_2006_tables()

  # IPCC Tier 2 parameters
  message("\nGenerating IPCC Tier 2 parameters...")
  ipcc_t2_raw <- generate_ipcc_tier2_params()
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
    uncertainty_ranges = ipcc_t2_raw$uncertainty_ranges,
    grazing_energy_coefs = ipcc_t2_raw$grazing_energy_coefs
  )

  # Combine all objects
  all_objects <- c(
    gleam_excel_tables,
    gleam_pdf_tables,
    ipcc_2019,
    ipcc_2006,
    ipcc_tier2,
    list(livestock_constants = livestock_constants)
  )

  message(
    "\nSaving ",
    length(all_objects),
    " objects to data/livestock_coefs.rda..."
  )
  save(
    list = names(all_objects),
    file = "data/livestock_coefs.rda",
    compress = "xz",
    envir = list2env(all_objects, envir = new.env())
  )

  message("\n=== Complete ===")
  message("Total objects saved: ", length(all_objects))
}

main()
