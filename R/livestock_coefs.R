# Livestock Coefficient Tables Documentation
#
# Roxygen2 documentation for all livestock coefficient data
# objects stored in data/livestock_coefs.rda.

# GLEAM Excel Supplement Tables ----

#' GLEAM crop residue parameters.
#'
#' @description
#' Parameters for crop residues and dry matter content.
#'
#' @format A dataframe with crop-specific residue parameters.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Table S.3.1. \doi{10.1088/1748-9326/aad4d8}
"gleam_crop_residue_params"

#' GLEAM geographic hierarchy.
#'
#' @description
#' Maps countries (ISO3) to GLEAM regions, FAOSTAT regions,
#' and classification indicators.
#'
#' @format A dataframe with columns:
#' \describe{
#'   \item{iso3}{ISO3 country code.}
#'   \item{country}{Country name.}
#'   \item{continent}{Continent.}
#'   \item{faostat_region}{FAOSTAT regional grouping.}
#'   \item{gleam_region}{GLEAM regional grouping.}
#' }
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Tables S.A1-S.A2. \doi{10.1088/1748-9326/aad4d8}
"gleam_geographic_hierarchy"

#' GLEAM feed composition.
#'
#' @description
#' Nutritional composition of feed items (DM, CP, ME, etc.).
#'
#' @format A dataframe with feed-level nutrient parameters.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Table S.3.2.
"gleam_feed_composition"

#' GLEAM feed digestibility.
#'
#' @description
#' Digestibility coefficients by feed type.
#'
#' @format A dataframe with digestibility parameters.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Table S.3.3.
"gleam_feed_digestibility"

#' GLEAM feed conversion ratios.
#'
#' @description
#' Feed conversion ratios by species and production system.
#'
#' @format A dataframe with FCR values.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Table S.3.4.
"gleam_feed_conversion_ratios"

#' GLEAM production system parameters.
#'
#' @description
#' Species-specific production system parameters
#' (herd structure, productivity, etc.).
#'
#' @format A dataframe with system-level parameters.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Tables S.6.x.
"gleam_production_system_params"

#' GLEAM manure system parameters.
#'
#' @description
#' Manure management system parameters by species and region.
#'
#' @format A dataframe with MMS parameters.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Tables S.7.x.
"gleam_manure_system_params"

#' GLEAM dressing percentages.
#'
#' @description
#' Carcass weight as percentage of live weight by species
#' and region.
#'
#' @format A dataframe with dressing percentages.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Supplement S1,
#'   Table S.9.1.
"gleam_dressing_percentages"

#' GLEAM livestock categories.
#'
#' @description
#' Species, production systems, and cohort definitions from
#' GLEAM 3.0.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{species}{Animal species.}
#'   \item{production_system}{Dairy, Beef, Meat, etc.}
#'   \item{cohort}{Age/sex cohort.}
#'   \item{description}{Cohort description.}
#' }
#'
#' @source MacLeod et al. (2018) GLEAM 3.0 Model Description.
"gleam_livestock_categories"

#' GLEAM feed categories.
#'
#' @description
#' Feed classification used in GLEAM 3.0.
#'
#' @format A tibble with `feed_category`, `feed_type`,
#'   `description`.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0.
"gleam_feed_categories"

#' GLEAM enteric fermentation parameters.
#'
#' @description
#' Ym (% GE) values by species and production system.
#' Feedlot cattle use 3.0% per IPCC 2019 Table 10.12.
#'
#' @format A tibble with `species`, `system`, `ym_percent`,
#'   `notes`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.
"gleam_enteric_params"

#' GLEAM manure management system shares.
#'
#' @description
#' Regional MMS allocation by species and system.
#'
#' @format A tibble with `region`, `species`, `system`, `mms`,
#'   `share_percent`.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0.
"gleam_mms_shares"

#' GLEAM animal weights.
#'
#' @description
#' Typical live weights by region, species, system, and cohort.
#'
#' @format A tibble with `region`, `species`, `system`,
#'   `cohort`, `weight_kg`.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0.
"gleam_animal_weights"

#' GLEAM milk production.
#'
#' @description
#' Average annual milk yields and lactation lengths by region.
#'
#' @format A tibble with `region`, `species`, `system`,
#'   `milk_kg_head_yr`, `lactation_days`.
#'
#' @source MacLeod et al. (2018) GLEAM 3.0.
"gleam_milk_production"

# IPCC 2019 Refinement Tables ----

#' IPCC 2019 enteric EF for cattle.
#'
#' @description
#' Table 10.10: Tier 1 enteric fermentation emission factors
#' for cattle by region (kg CH4/head/yr).
#'
#' @format A tibble with `region`, `category`,
#'   `ef_kg_head_yr`, `source`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.10.
"ipcc_2019_enteric_ef_cattle"

#' IPCC 2019 enteric EF for non-cattle.
#'
#' @description
#' Table 10.11: Tier 1 enteric fermentation emission factors
#' for non-cattle species (kg CH4/head/yr).
#'
#' @format A tibble with `category`, `ef_kg_head_yr`,
#'   `source`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.11.
"ipcc_2019_enteric_ef_other"

#' IPCC 2019 manure CH4 EF for cattle.
#'
#' @description
#' Table 10.14: Tier 1 manure management CH4 emission factors
#' for cattle by region (kg CH4/head/yr).
#'
#' @format A tibble with `region`, `category`,
#'   `ef_kg_head_yr`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.14.
"ipcc_2019_manure_ch4_ef_cattle"

#' IPCC 2019 manure CH4 EF for non-cattle.
#'
#' @description
#' Table 10.14: Tier 1 manure management CH4 emission factors
#' for non-cattle species (kg CH4/head/yr).
#'
#' @format A tibble with `category`, `ef_kg_head_yr`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.14.
"ipcc_2019_manure_ch4_ef_other"

#' IPCC 2019 MCF for manure management.
#'
#' @description
#' Table 10.17: Methane Conversion Factors by manure management
#' system and annual average temperature.
#'
#' @format A tibble with `system`, `annual_temp_c`,
#'   `mcf_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.17.
"ipcc_2019_mcf_manure"

#' IPCC 2019 nitrogen excretion rates.
#'
#' @description
#' Table 10.19: Daily N excretion rates by species and region
#' (kg N/1000 kg animal mass/day).
#'
#' @format A tibble with `region`, `category`,
#'   `nex_kg_per_1000kg_day`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.19.
"ipcc_2019_n_excretion"

#' IPCC 2019 direct N2O emission factors.
#'
#' @description
#' Table 10.21: EF3 values (kg N2O-N/kg N) by manure
#' management system.
#'
#' @format A tibble with `mms_type`, `ef3_kg_n2on_kg_n`,
#'   `source`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.21.
"ipcc_2019_n2o_ef_direct"

#' IPCC 2019 Ym values (Table 10.12).
#'
#' @description
#' Methane conversion rate (% GE) by species and feed
#' situation. The 2019 Refinement differentiates:
#' - Cattle feedlot (>90% concentrate): 3.0%.
#' - Sheep >= 75 kg body weight: 6.7%.
#' - Sheep < 75 kg body weight: 4.7%.
#'
#' @format A tibble with `category`, `feed_situation`,
#'   `ym_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.
"ipcc_2019_ym"

#' IPCC 2019 Bo values (Table 10.16).
#'
#' @description
#' Maximum CH4 producing capacity of manure
#' (m3 CH4/kg VS). Dairy cattle (0.24) differs from
#' other cattle (0.18).
#'
#' @format A tibble with `category`, `bo_m3_kg_vs`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16.
"ipcc_2019_bo"

#' IPCC 2019 Cfi values (Table 10.4).
#'
#' @description
#' Net energy maintenance coefficients (MJ/day/kg^0.75).
#' Dairy (lactating) cattle use 0.386; non-dairy 0.322.
#'
#' @format A tibble with `category`, `subcategory`,
#'   `cfi_mj_day_kg075`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.4.
"ipcc_2019_cfi"

# IPCC 2006 Tables ----

#' IPCC 2006 Tier 1 enteric emission factors.
#'
#' @description
#' Table 10.11 (2006): Tier 1 regional EFs for enteric
#' fermentation.
#'
#' @format A tibble with `region`, `category`,
#'   `ef_kg_head_yr`.
#'
#' @source IPCC 2006, Vol 4, Ch 10, Table 10.11.
"ipcc_2006_enteric_ef"

#' IPCC 2006 Tier 1 manure emission factors.
#'
#' @description
#' Table 10.14 (2006): Tier 1 regional EFs for manure CH4.
#'
#' @format A tibble with `region`, `category`,
#'   `ef_kg_head_yr`, `temp_zone`.
#'
#' @source IPCC 2006, Vol 4, Ch 10, Table 10.14.
"ipcc_2006_manure_ef"

#' IPCC 2006 MCF by temperature.
#'
#' @description
#' Table 10.17 (2006): MCF values by MMS type and
#' annual temperature.
#'
#' @format A tibble with `system`, `temp_c`, `mcf_percent`.
#'
#' @source IPCC 2006, Vol 4, Ch 10, Table 10.17.
"ipcc_2006_mcf_temp"

# IPCC Tier 2 Parameters ----

#' Tier 2 energy coefficients.
#'
#' @description
#' Coefficients for IPCC Tier 2 GE calculation including
#' Cfi (maintenance), Ca (activity), Cp (pregnancy),
#' Cw (work), and energy content of weight gain. Now
#' includes `subcategory` column to differentiate dairy
#' (lactating) vs non-dairy cattle.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{category}{Species (Cattle, Buffalo, Sheep, etc.).}
#'   \item{subcategory}{Dairy, Non-Dairy, or All.}
#'   \item{cfi_mj_day_kg075}{NEm coefficient (MJ/day/kg^0.75).}
#'   \item{ca_pasture}{Activity coefficient for grazing.}
#'   \item{ca_feedlot}{Activity coefficient for confined.}
#'   \item{cp}{Pregnancy coefficient.}
#'   \item{cw}{Work coefficient.}
#'   \item{energy_content_gain_mj_kg}{Energy per kg gain.}
#' }
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10,
#'   Eq 10.3-10.16; Tables 10.4-10.5.
"ipcc_tier2_energy_coefs"

#' Tier 2 Ym values.
#'
#' @description
#' Methane conversion rate by species and feed situation
#' for Tier 2 enteric CH4. Includes feedlot distinction
#' and sheep body weight differentiation.
#'
#' @format A tibble with `category`, `feed_situation`,
#'   `ym_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.
"ipcc_tier2_ym_values"

#' Tier 2 Bo values.
#'
#' @description
#' Maximum CH4 producing capacity by detailed category.
#' Dairy cattle 0.24 vs other cattle 0.18.
#'
#' @format A tibble with `category`, `bo_m3_kg_vs`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16.
"ipcc_tier2_bo_values"

#' Tier 2 manure ash content.
#'
#' @description
#' Ash content of manure as percent of dry matter,
#' used in VS calculation (Eq 10.24).
#'
#' @format A tibble with `category`, `ash_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10.
"ipcc_tier2_manure_ash"

#' Tier 2 nitrogen retention fractions.
#'
#' @description
#' Fraction of N intake retained in animal products.
#' Dairy cattle 0.20 vs other cattle 0.07.
#'
#' @format A tibble with `category`, `n_retention_frac`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.20.
"ipcc_tier2_n_retention"

#' Default production parameters.
#'
#' @description
#' Default values for fat%, protein%, lactose%, weight gain,
#' work hours, and pregnancy fraction by species.
#'
#' @format A tibble with production defaults per species.
#'
#' @source NRC 2001; IPCC 2019, Vol 4, Ch 10.
"livestock_production_defaults"

#' Feed characteristics by diet quality.
#'
#' @description
#' DE%, NDF%, GE content, and crude protein percentage
#' for High/Medium/Low diet quality levels.
#'
#' @format A tibble with `diet_quality`, `de_percent`,
#'   `ndf_percent`, `ge_content_mj_kg_dm`, `cp_percent`.
#'
#' @source IPCC 2019, Vol 4, Ch 10.
"feed_characteristics"

#' Climate-zone MCF values.
#'
#' @description
#' Methane Conversion Factors by MMS type and climate zone
#' (Cool/Temperate/Warm).
#'
#' @format A tibble with `mms_type`, `climate_zone`,
#'   `mcf_percent`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.17.
"climate_mcf"

#' Regional MMS distribution.
#'
#' @description
#' Fraction of manure managed in each MMS type by region
#' and species.
#'
#' @format A tibble with `region`, `species`, `mms_type`,
#'   `fraction`.
#'
#' @source GLEAM 3.0 / FAO statistics.
"regional_mms_distribution"

#' Temperature adjustment factors for NEm.
#'
#' @description
#' Adjustment multipliers for net energy maintenance
#' under cold stress, thermoneutral, and heat stress
#' conditions.
#'
#' @format A tibble with `temp_range`, `temp_min`,
#'   `temp_max`, `adjustment_factor`.
#'
#' @source NRC 2001; IPCC 2019.
"temperature_adjustment"

#' Indirect N2O emission factors.
#'
#' @description
#' Parameters for indirect N2O emissions from manure
#' management: EF4 (volatilization), EF5 (leaching),
#' FracGasMS, FracLeach.
#'
#' @format A tibble with `parameter`, `value`,
#'   `description`.
#'
#' @source IPCC 2019, Vol 4, Ch 10, Table 10.22;
#'   Vol 4, Ch 11, Table 11.3.
"indirect_n2o_ef"

#' Uncertainty ranges for emission parameters.
#'
#' @description
#' Lower and upper multipliers for key emission
#' parameters (Ym, MCF, Bo, EF_N2O, Nex).
#'
#' @format A tibble with `parameter`, `lower_mult`,
#'   `upper_mult`, `distribution`.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10.
"uncertainty_ranges"

#' Grazing energy coefficients.
#'
#' @description
#' Walking energy cost for grazing animals
#' (MJ/kg body weight/km).
#'
#' @format A tibble with `parameter`, `value_mj_kg_km`,
#'   `source`.
#'
#' @source NRC 2001 (0.00045 Mcal/kg/km converted to MJ).
"grazing_energy_coefs"

#' Livestock physical constants.
#'
#' @description
#' Named list of physical constants used in livestock
#' emission calculations:
#' - `energy_ch4_mj_kg`: 55.65 MJ/kg CH4.
#' - `ge_content_mj_kg_dm`: 18.45 MJ/kg DM.
#' - `ue_factor`: 0.04 (urinary energy as fraction of GE).
#' - `n_to_protein`: 6.25 (N to protein conversion).
#' - `default_de_percent`: 65%.
#' - `ev_wool_mj_kg`: 24.0 MJ/kg clean wool.
#'
#' @format A named list.
#'
#' @source IPCC 2019 Refinement, Vol 4, Ch 10.
"livestock_constants"
