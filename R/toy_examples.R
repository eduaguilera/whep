#' Toy example outputs for documentation
#'
#' @description
#' Internal functions that return small example tibbles for documentation
#' and testing purposes. These avoid the need to download remote data during
#' CRAN checks and allow users to see function outputs quickly.
#'
#' @keywords internal
#' @noRd

# One row per (proc_group, type) combination the real builder emits, sampled
# from a real build_supply_use() run over 1967, 1978, 1985, 1996, 2003, 2010,
# 2011 and 2021. The fixture this replaced showed only three of the five
# documented process groups, and one of its ten rows had no `area_code` at all
# (whep#417). The real builder does still emit area-less rows -- 515 of
# 1,211,820 in that run, all of them crop-residue supply from a name-keyed
# join, whep#684 -- but they are a defect to fix at the source, not a shape to
# teach the reader in the documented example.
.example_build_supply_use <- function() {
  tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    1967L, 11L, "processing", 2543L, 2543L, "use", 8000,
    1978L, 130L, "crop_production", 2536L, 2106L, "supply", 393876,
    1985L, 26L, "husbandry", 1053L, 2763L, "use", 466.613,
    1985L, 206L, "animal_draught", 1126L, 4000L, "supply", 2949.67,
    2003L, 102L, "crop_production", 2517L, 2517L, "use", 500,
    2003L, 156L, "slaughtering", 961L, 2737L, "supply", 227298,
    2011L, 72L, "husbandry", 1016L, 1016L, "supply", 512000,
    2011L, 174L, "processing", 2617L, 2543L, "supply", 14.5604,
    2021L, 96L, "slaughtering", 1049L, 1049L, "use", 1538870
  ) |>
    .add_reporting_polity_columns()
}

.example_get_bilateral_trade <- function() {
  tibble::tribble(
    ~year, ~item_cbs_code, ~bilateral_trade,
    2003L, 2552, matrix(1, nrow = 187, ncol = 187),
    2015L, 2672, matrix(1, nrow = 187, ncol = 187),
    2015L, 2664, matrix(1, nrow = 187, ncol = 187),
    2011L, 2543, matrix(1, nrow = 187, ncol = 187),
    1991L, 2613, matrix(1, nrow = 187, ncol = 187),
    1999L, 2578, matrix(1, nrow = 187, ncol = 187),
    2001L, 2590, matrix(1, nrow = 187, ncol = 187),
    2003L, 2613, matrix(1, nrow = 187, ncol = 187),
    2018L, 2671, matrix(1, nrow = 187, ncol = 187),
    2021L, 2582, matrix(1, nrow = 187, ncol = 187)
  )
}

# Eleven rows sampled from a real get_feed_intake() run (national grain, IPCC
# demand tier, historical feed mode) over 1967, 1978, 1985, 1996, 2003, 2010,
# 2011 and 2021, covering all five feed types. `loss` and `loss_share` are zero
# because the redistribute-feed allocator is demand-pull: every allocated tonne
# is eaten, so supply == intake and underfeeding is carried by redistribute's
# scaling factor. They are zero in all 291,916 rows of that run. The fixture
# this replaced predated that migration and carried a 10% loss plus two rows
# with no `area_code` at all (whep#417); the roxygen block still describes a
# loss the allocator cannot produce, which is whep#689.
.example_get_feed_intake <- function() {
  tibble::tribble(
    ~year, ~area_code, ~live_anim_code, ~item_cbs_code, ~feed_type, ~supply, ~intake, ~intake_dry_matter, ~loss, ~loss_share,
    1967L, 96L, 976L, 3500L, "scavenging", 2.83572, 2.83572, 0.567144, 0, 0,
    1978L, 33L, 1052L, 2736L, "animals", 72.575, 72.575, 21.7725, 0, 0,
    1978L, 226L, 1016L, 3500L, "scavenging", 432336, 432336, 86467.3, 0, 0,
    1978L, 234L, 1096L, 2533L, "crops", 492.388, 492.388, 127.036, 0, 0,
    1985L, 72L, 961L, 2106L, "residues", 5806.92, 5806.92, 5224.29, 0, 0,
    1996L, 10L, 961L, 2101L, "crops", 15982.3, 15982.3, 14479.9, 0, 0,
    1996L, 244L, 960L, 3000L, "grass", 10249.1, 10249.1, 2049.82, 0, 0,
    2003L, 191L, 1049L, 2514L, "crops", 338.62, 338.62, 291.891, 0, 0,
    2010L, 170L, 960L, 3000L, "grass", 2844900, 2844900, 568980, 0, 0,
    2010L, 235L, 961L, 2763L, "animals", 1852.27, 1852.27, 463.068, 0, 0,
    2021L, 59L, 1016L, 2104L, "residues", 169951, 169951, 151766, 0, 0
  ) |>
    .add_reporting_polity_columns()
}

.example_grassland_extension <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u, ~method_grassland,
    1986L, 10L, 3000L, 2.5e7, "occupation",
    1986L, 100L, 3000L, 8.0e6, "occupation",
    1987L, 10L, 3000L, 2.5e7, "occupation",
    1987L, 100L, 3000L, 8.1e6, "occupation"
  ) |>
    .add_reporting_polity_columns()
}

.example_ghg_extension <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u, ~method_ghg,
    1986L, 10L, 960L, 6.156e8, "IPCC_2019_Tier1_AR6",
    1986L, 10L, 961L, 3.078e9, "IPCC_2019_Tier1_AR6",
    1986L, 10L, 976L, 1.10565e9, "IPCC_2019_Tier1_AR6",
    1986L, 100L, 961L, 2.2464e9, "IPCC_2019_Tier1_AR6",
    1987L, 10L, 961L, 3.10878e9, "IPCC_2019_Tier1_AR6",
    1987L, 100L, 960L, 8.424e8, "IPCC_2019_Tier1_AR6"
  ) |>
    .add_reporting_polity_columns()
}

.example_energy_co2_extension <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u, ~method_energy,
    2010L, 21L, 961L, 1.7669e9, "GLEAM_3.0_energy_meat",
    2010L, 21L, 1053L, 3.4077e9, "GLEAM_3.0_energy_meat",
    2010L, 231L, 961L, 8.3289e9, "GLEAM_3.0_energy_meat",
    2010L, 231L, 976L, 5.7395e7, "GLEAM_3.0_energy_meat",
    2010L, 231L, 1016L, 1.7517e7, "GLEAM_3.0_energy_meat",
    2010L, 231L, 1049L, 1.9281e9, "GLEAM_3.0_energy_meat",
    2010L, 231L, 1051L, 2.1423e8, "GLEAM_3.0_energy_meat",
    2010L, 231L, 1053L, 1.1171e10, "GLEAM_3.0_energy_meat"
  ) |>
    .add_reporting_polity_columns()
}

.example_soil_n2o_extension <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u, ~method_soil_n2o,
    ~method_synthetic,
    2010L, 10L, 2511L, 4.126122e8, "IPCC_2019_Tier1_AR6", "coello",
    2010L, 10L, 2513L, 1.768338e8, "IPCC_2019_Tier1_AR6", "coello"
  ) |>
    .add_reporting_polity_columns()
}

.example_soil_carbon_inputs <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_prod_code, ~year,
    ~residue_c_mgc_ha_yr, ~root_c_mgc_ha_yr, ~weed_c_mgc_ha_yr,
    ~manure_c_mgc_ha_yr, ~total_c_input_mgc_ha_yr, ~humified_fraction,
    0.25, 0.25, 1L, "15", 2020L, 1.5, 1.0, 0.25, 0.5, 3.25, 0.156083313609467,
    0.75, 0.25, 1L, "15", 2020L, 1.5, 1.0, 0.25, 0.5, 3.25, 0.156083313609467,
    0.25, 0.25, 1L, "27", 2020L, 1.5, 0.5, 0.25, 0.5, 2.75, 0.152053748675567,
    0.75, 0.25, 1L, "27", 2020L, 1.5, 0.5, 0.25, 0.5, 2.75, 0.152053748675567
  ) |>
    dplyr::mutate(method_c_input = "humified_weighted") |>
    .add_reporting_polity_columns()
}

.ex_get_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~live_anim_code, ~unit, ~value,
    1969, 216, 1049, 1049, NA, "heads", 4326071.,
    2018, 100, 265, 2570, NA, "tonnes", 1567560.,
    1962, 16, 987, 2746, 976, "t_LU", 0.0138,
    1974, 101, 1091, 2744, 1068, "tonnes", 37200.,
    1990, 225, 960, 960, NA, "LU", 23894.,
    2005, 4, 406, 2605, NA, "ha", 10848.,
    1988, 137, 1052, 1052, NA, "heads", 460.,
    1981, 130, 486, 2615, NA, "ha", 17600.,
    1962, 171, 122, 2533, NA, "t_ha", 5.15,
    1964, 173, 1037, 2737, 1049, "t_head", 0.0167
  ) |>
    .add_reporting_polity_columns()
}

.example_get_primary_residues <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_crop, ~item_cbs_code_residue, ~value,
    2010, 174, 2611, 2107, 46260.,
    1975, 54, 2511, 2105, 569199.,
    1988, 53, 2561, 2106, 8213.,
    2020, 178, 2513, 2105, 161992.,
    1972, 131, 2514, 2105, 38845.,
    2011, 4, 2611, 2107, 238808.,
    1965, 144, 2517, 2105, 33688.,
    2018, 167, 2549, 2105, 13578.,
    1994, 109, 2605, 2105, 5597.,
    1982, 194, 2605, 2106, 280552.
  ) |>
    .add_reporting_polity_columns()
}

.example_get_processing_coefs <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_to_process, ~value_to_process, ~item_cbs_code_processed, ~initial_conversion_factor, ~initial_value_processed, ~conversion_factor_scaling, ~final_conversion_factor, ~final_value_processed,
    1974, 203, 2617, 1118., 2659, 0.00767, 8.6, 1.0, 0.00767, 8.6,
    1991, 28, 2536, 1928388., 2542, 0.102, 196696., 1.0, 0.102, 196696.,
    1983, 68, 2555, 836000., 2590, 0.791, 661276., 1.0, 0.791, 661276.,
    1999, 68, 2559, 768., 2594, 0.506, 388.6, 1.0, 0.506, 388.6,
    2020, 202, 2561, 1000., 2597, 0.63, 630., 1.0, 0.63, 630.,
    2010, 20, 2513, 8183., 2102, 0.28, 2291.2, 1.0, 0.28, 2291.2,
    1972, 226, 2559, 143074., 2575, 0.161, 23035., 1.0, 0.161, 23035.,
    1974, 103, 2570, 874., 2586, 0.397, 347., 1.0, 0.397, 347.,
    1995, 230, 2625, 59452., 2658, 0.0201, 1195., 1.0, 0.0201, 1195.,
    1970, 223, 2511, 6.80, 2656, 4.74, 32.2, 1.0, 4.74, 32.2
  ) |>
    .add_reporting_polity_columns()
}

.example_get_wide_cbs <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~domestic_supply, ~food, ~production, ~feed, ~seed, ~import, ~export, ~other_uses, ~processing, ~stock_withdrawal, ~stock_addition,
    1987L, 250L, 2106, 13741247., 0., 13741247., 1.37e7, 0., 0., 0., 0., 0., 0., 0.,
    2012L, 41L, 2633, 82000., 82000., 0., 0., 0., 1.46e5, 68158., 0., 0., 0., 0.,
    1984L, 123L, 2595, 1207., 0., 3854., 1.21e3, 0., 0., 4147., 0., 0., 0., 0.,
    1982L, 165L, 2633, 86.5, 86.5, 0., 0., 0., 8.75e1, 1., 0., 0., 0., 0.,
    1977L, 159L, 2658, 2218., 2218., 0., 0., 0., 2.22e3, 0., 0., 0., 0., 0.,
    1995L, 234L, 2671, 4312., 0., 2500., 0., 0., 1.99e3, 178., 4312., 0., 0., 0.,
    1975L, 10L, 677L, 2270., 2270., 2270., 0., 0., 0., 0., 0., 0., 0., 0.,
    1961L, 156L, 2658, 6877., 6877., 2000., 0., 0., 4.88e3, 0., 0., 0., 0., 0.,
    1961L, 236L, 2620, 11177., 11177., 0., 0., 0., 1.12e4, 0., 0., 0., 0., 0.,
    1995L, 49L, 2734, 71117., 71117., 56724., 0., 0., 1.48e4, 0., 0., 0., 0., 0.
  ) |>
    .add_reporting_polity_columns()
}

.example_create_n_prov_destiny <- function() {
  tibble::tribble(
    ~year, ~province_name, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n,
    1865, "Huesca", "Sugarbeet pulp", NA, "Cropland", "Outside", "livestock_mono", 4.52e-3,
    1929, "Tarragona", "Olives (including preserved)", "Irrigated", "Cropland", "Cropland", "export", 6.92e+1,
    1955, "Albacete", "Wheat and products", "Rainfed", NA, "Synthetic", "Cropland", 6.16e+2,
    1957, "Gipuzkoa", "Wheat and products", NA, "Cropland", "Outside", "population_food", 8.17e-1,
    1862, "Huesca", "Grapes and products (excl wine)", "Irrigated", "Cropland", "Cropland", "population_food", 9.39e-2,
    1980, "Lleida", "Wheat and products", "Irrigated", NA, "Fixation", "Cropland", 1.71e+2,
    1863, "A_Coruna", "Millet and products", "Rainfed", NA, "Fixation", "Cropland", 9.95e-3,
    1987, "Lugo", "Tomatoes and products", "Irrigated", "Cropland", "Cropland", "livestock_mono", 1.29e-3,
    1950, "Castello", "Apples and products", "Irrigated", "Cropland", "Cropland", "livestock_mono", 1.96e-2,
    1988, "Zaragoza", "Grassland", NA, "semi_natural_agroecosystems", "semi_natural_agroecosystems", "livestock_rum", 1.32e+3
  )
}

.example_create_n_soil_inputs <- function() {
  tibble::tribble(
    ~year, ~province_name, ~item, ~irrig_cat, ~box, ~deposition, ~fixation, ~synthetic, ~manure, ~urban,
    1976, "Burgos", "Wheat and products", "Irrigated", "Cropland", 16.5, 13.0, 184., 2.04e+1, 0.509,
    1912, "Alicante", "Apples and products", "Irrigated", "Cropland", 0.0198, 0.159, 0.0132, 2.54e-2, 0.0208,
    1974, "Lleida", "Fallow", "Rainfed", "Cropland", 263., 151., 0., 0., 8.35,
    1947, "Caceres", "Fodder cereal and grass", "Rainfed", "Cropland", 27.5, 80.0, 0.0214, 1.17e+2, 3.20,
    1924, "Huesca", "Wheat and products", "Irrigated", "Cropland", 34.2, 157., 156., 5.14e+2, 6.80,
    1953, "Huesca", "Apples and products", "Rainfed", "Cropland", 0.441, 1.04, 3.35, 4.91e-1, 0.0419,
    1901, "Avila", "Cereals, Other", "Rainfed", "Cropland", 0.164, 0.729, 0.0448, 9.07e-2, 0.0386,
    1909, "Cordoba", "Beans", "Irrigated", "Cropland", 0.207, 4.86, 0.0818, 6.94e-1, 0.0605,
    1931, "Leon", "Pulses, Other and products", "Rainfed", "Cropland", 8.04, 148., 3.11, 1.37e+1, 2.06,
    1950, "Navarra", "Apples and products", "Irrigated", "Cropland", 0.806, 2.40, 1.34, 3.44e-1, 0.171
  )
}

.example_create_n_production <- function() {
  tibble::tribble(
    ~year, ~province_name, ~item, ~box, ~prod,
    1931, "Valencia", "Meat, Other", "Livestock", 36.5,
    1990, "Granada", "Nuts and products", "Cropland", 532.,
    1957, "Teruel", "Tomatoes and products", "Cropland", 9.19,
    1898, "Barcelona", "Brans", "Cropland", 0.518,
    1943, "Barcelona", "Urea", "Agro-industry", 1.66,
    1953, "Lleida", "Demersal Fish", "Fish", 0.,
    1904, "Salamanca", "Wheat and products", "Cropland", 1840.,
    2015, "Burgos", "Sweet potatoes", "Cropland", 0.00461,
    1988, "Zaragoza", "DDGS Barley", "Cropland", 619.,
    2014, "Soria", "Crustaceans", "Fish", 0.
  )
}

.example_calculate_nue_crops <- function() {
  tibble::tribble(
    ~year, ~province_name, ~item, ~box, ~nue,
    1937, "Tenerife", "Oranges, Mandarines", "Cropland", 97.3,
    1905, "Cantabria", "Apples and products", "Cropland", 59.8,
    2005, "Badajoz", "Firewood", "semi_natural_agroecosystems", 0.345,
    1968, "Murcia", "Millet and products", "Cropland", 746.,
    1943, "Gipuzkoa", "Hard Fibres, Other", "Cropland", 59.4,
    1954, "Malaga", "Firewood", "semi_natural_agroecosystems", 0.491,
    1973, "Lugo", "Tomatoes and products", "Cropland", 28.4,
    1953, "Almeria", "Lemons, Limes and products", "Cropland", 24.4,
    1860, "Lleida", "Pulses, Other and products", "Cropland", 68.3,
    2015, "Valencia", "Grapefruit and products", "Cropland", 6.39
  )
}

.ex_calc_nue_livestock <- function() {
  tibble::tribble(
    ~year, ~province_name, ~livestock_cat, ~item, ~prod_n, ~feed_n, ~excretion_n, ~nue, ~mass_balance,
    1921, "Lugo", "Horses", "Meat, Other", 7.28, 1078., 1158., 0.675, 1.08,
    1994, "Huelva", "Horses", "Offals, Edible", 0.921, 397., 423., 0.232, 1.07,
    2001, "Cuenca", "Goats", "Mutton & Goat Meat", 4.22, 347., 355., 1.22, 1.04,
    1876, "Avila", "Cattle_milk", "Milk - Excluding Butter", 40.4, 179., 147., 22.6, 1.05,
    1918, "Malaga", "Horses", "Meat, Other", 4.30, 635., 684., 0.678, 1.08,
    1902, "Madrid", "Cattle_meat", "Fats, Animals, Raw", 0., 654., 599., 0., 0.916,
    1926, "Zaragoza", "Sheep", "Hides and skins", 119., 8965., 8454., 1.33, 0.956,
    2017, "Badajoz", "Pigs", "Offals, Edible", 581., 21434., 15567., 2.71, 0.753,
    1928, "Leon", "Poultry", "Poultry Meat", 10.6, 177., 127., 6.00, 0.780,
    1861, "Girona", "Horses", "Meat, Other", 5.23, 842., 832., 0.621, 0.994
  )
}

.example_calculate_system_nue <- function() {
  tibble::tribble(
    ~year, ~province_name, ~total_prod, ~inputs, ~nue_system,
    1917, "Tarragona", 4740., 9924., 47.8,
    1989, "A_Coruna", 27403., 52128., 52.6,
    1967, "Tenerife", 2080., 11614., 17.9,
    2010, "Albacete", 18671., 70849., 26.4,
    1923, "Albacete", 8767., 19187., 45.7,
    1968, "Salamanca", 11821., 34294., 34.5,
    1932, "Palencia", 6206., 15537., 39.9,
    1944, "Almeria", 2538., 12783., 19.9,
    1911, "Avila", 4112., 16873., 24.4,
    1893, "Malaga", 3296., 13713., 24.0
  )
}

.example_create_n_nat_destiny <- function() {
  tibble::tribble(
    ~year, ~item, ~irrig_cat, ~box, ~origin, ~destiny, ~mg_n, ~province_name,
    1863, "Hard Fibres, Other", "Irrigated", NA, "Livestock", "Cropland", 52.1, "Spain",
    2012, "Nuts and products", "Irrigated", NA, "People", "Cropland", 212., "Spain",
    1955, "Pulses, Other and products", "Irrigated", "Cropland", "Cropland", "export", 502., "Spain",
    1976, "Sorghum and products", "Irrigated", "Cropland", "Cropland", "population_other_uses", 24.7, "Spain",
    1901, "Pulses, Other and products", "Irrigated", "Cropland", "Cropland", "livestock_mono", 35.4, "Spain",
    1922, "Nuts and products", "Rainfed", NA, "Fixation", "Cropland", 2481., "Spain",
    1993, "Oats", "Irrigated", NA, "People", "Cropland", 5.65, "Spain",
    1874, "Fodder mix", "Rainfed", "Cropland", "Cropland", "livestock_rum", 605., "Spain",
    1983, "Oranges, Mandarines", "Irrigated", NA, "Livestock", "Cropland", 5290., "Spain",
    1997, "Barley and products", "Rainfed", "Cropland", "Cropland", "livestock_rum", 17549., "Spain"
  )
}

.example_build_primary_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~item_cbs_code, ~live_anim_code, ~unit, ~value, ~source,
    1912, 165, "772",  772,  NA_character_, "tonnes", 325.,      "LUH2_cropland",
    2012, 112, "982",  2848, "976",         "t_head", 0.0268,    "FAOSTAT_prod",
    1943,  41, "515",  2617, NA_character_, "t_ha",   0.600,     "LUH2_cropland",
    1979,  45, "977",  2732, "976",         "tonnes", 33.,       "FAOSTAT_prod",
    1910, 141, "1098", 2736, "1096",        "t_LU",   0.00186,   "LUH2_agriland",
    1867,  90, "976",  976,  NA_character_, "heads",  111941.,   NA_character_,
    1939,  15, "157",  2537, NA_character_, "ha",     45921.,    "LUH2_cropland",
    1935, 211, "270",  2558, NA_character_, "ha",     4018.,     "LUH2_cropland",
    1937,   9, "772",  772,  NA_character_, "ha",     785953.,   "LUH2_cropland",
    2000,   9, "571",  2625, NA_character_, "ha",     236.,      "FAOSTAT_prod"
  ) |>
    .add_reporting_polity_columns()
}

.example_build_commodity_bal <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,             ~value,     ~source,             ~fao_flag,
    2010, 120, 2731, "import",            1.76e3,     "FAOSTAT_FBS_New", NA_character_,
    1981, 222, 2734, "domestic_supply",   4.10e4,     "FAOSTAT_FBS_Old", NA_character_,
    1906, 203, 2655, "processing",        6.35e4,     "historical_fill", NA_character_,
    1899, 175, 2744, "food",              7.26e1,     "historical_fill", NA_character_,
    2018,  48, 2562, "domestic_supply",   1.20e5,     "FAOSTAT_FBS_New", NA_character_,
    1871,  10, 2746, "stock_variation",  -7.28e-12,   NA_character_,     NA_character_,
    1938, 226, 2848, "production",        1.51e5,     "historical_fill", NA_character_,
    1924,  11, 2557, "production",        1.61e2,     "historical_fill", NA_character_,
    1928,  96, 2625, "domestic_supply",   1.85e4,     NA_character_,     NA_character_,
    1879, 236, 2547, "seed",              3.83e-8,    "historical_fill", NA_character_
  ) |>
    .add_reporting_polity_columns()
}

.example_build_proc_coefs <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code_to_process, ~value_to_process, ~item_cbs_code_processed, ~initial_conversion_factor, ~initial_value_processed, ~conversion_factor_scaling, ~final_conversion_factor, ~final_value_processed,
    2012, 150, 2570,  3000.,       2598, 0.163,   489.,      0.0542,  0.00883,  26.5,
    1968, 173, 2537,  14230000.,   2544, 0.0320,  455762.,   1.19,    0.0380,   540700.,
    2015, 150, 2558,  462000.,     2574, 0.412,   190460.,   0.845,   0.348,    161000.,
    1885, 248, 2807,  0.138,       2657, 0.0824,  0.0114,    0.213,   0.0175,   0.00243,
    1896, 191, 2544,  116.,        2543, 0.0822,  9.57,      1.16,    0.0954,   11.1,
    1873,  67, 2514,  354.,        2598, 0.169,   59.7,      3.09,    0.522,    185.,
    1987,  79, 2537,  25142000.,   2659, 0.00386, 97129.,    0.750,   0.00290,  72852.,
    2007,  19, 2615,  1044.,       2657, 0.00841, 8.78,      0.415,   0.00349,  3.64,
    1969,  51, 2537,  5589000.,    2542, 0.119,   667851.,   1.07,    0.128,    716000.,
    1984, 171, 2513,  104400.,     2659, 0.0240,  2501.,     0.0575,  0.00138,  144.
  ) |>
    .add_reporting_polity_columns()
}

.example_build_detailed_trade <- function() {
  tibble::tribble(
    ~year, ~area_code, ~area_code_partner, ~element,
    ~item_cbs_code, ~unit, ~value, ~country_share,
    2010L, 4L, 100L, "import", 2511L, "tonnes", 125000., 0.35,
    2010L, 4L, 79L, "import", 2511L, "tonnes", 89000., 0.25,
    2015L, 100L, 4L, "export", 2536L, "tonnes", 45000., 0.18,
    2015L, 100L, 79L, "export", 2536L, "tonnes", 72000., 0.29,
    2018L, 79L, 4L, "import", 2807L, "tonnes", 310000., 0.42,
    2018L, 79L, 100L, "import", 2807L, "tonnes", 150000., 0.20,
    2005L, 4L, 79L, "export", 2555L, "tonnes", 63000., 0.55,
    2005L, 4L, 100L, "export", 2555L, "tonnes", 28000., 0.24,
    2012L, 100L, 4L, "import", 2570L, "tonnes", 98000., 0.31,
    2012L, 100L, 79L, "import", 2570L, "tonnes", 54000., 0.17
  ) |>
    .add_trade_polity_columns()
}

.example_build_trade_prices <- function() {
  tibble::tribble(
    ~year, ~item_trade, ~item_code_trade, ~element,
    ~kdollars, ~tonnes, ~price,
    2010L, "Wheat", 15L, "export", 3.5e7, 1.2e8, 0.292,
    2010L, "Wheat", 15L, "import", 3.8e7, 1.3e8, 0.292,
    2015L, "Rice", 31L, "export", 1.9e7, 4.5e7, 0.422,
    2015L, "Rice", 31L, "import", 2.0e7, 4.7e7, 0.426,
    2010L, "Maize", 56L, "export", 2.8e7, 1.1e8, 0.255,
    2010L, "Maize", 56L, "import", 3.0e7, 1.2e8, 0.250,
    2018L, "Soybeans", 236L, "export", 5.2e7, 1.5e8, 0.347,
    2018L, "Soybeans", 236L, "import", 5.5e7, 1.6e8, 0.344,
    2015L, "Sugar", 162L, "export", 1.1e7, 5.5e7, 0.200,
    2015L, "Sugar", 162L, "import", 1.2e7, 5.8e7, 0.207
  )
}

.example_build_primary_prices <- function() {
  tibble::tribble(
    ~year, ~item_prod_code, ~price,
    2010L, "15", 0.292,
    2015L, "15", 0.210,
    2010L, "56", 0.255,
    2015L, "56", 0.185,
    2010L, "236", 0.410,
    2015L, "236", 0.347,
    2010L, "31", 0.395,
    2015L, "31", 0.422,
    2018L, "406", 0.330,
    2018L, "486", 0.180
  )
}

.example_build_cbs_prices <- function() {
  tibble::tribble(
    ~year, ~element, ~item_cbs_code, ~price,
    2010L, "export", 2511L, 0.292,
    2010L, "import", 2511L, 0.295,
    2015L, "export", 2807L, 0.422,
    2015L, "import", 2807L, 0.426,
    2010L, "export", 2536L, 0.255,
    2010L, "import", 2536L, 0.250,
    2018L, "export", 2555L, 0.347,
    2018L, "import", 2555L, 0.344,
    2015L, "export", 2105L, 0.029,
    2015L, "import", 2105L, 0.030
  )
}

.example_feed_demand <- function() {
  tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category, ~item_cbs_code,
    ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
    2000L, "ESP", "prov_a", "cattle", 2514L, "cereals", "high_quality", 100, TRUE,
    2000L, "ESP", "prov_a", "cattle", 2555L, "grass", "grass", 50, TRUE,
    2000L, "ESP", "prov_b", "pigs", 2514L, "cereals", "high_quality", 30, TRUE
  )
}

.example_feed_avail <- function() {
  tibble::tribble(
    ~year, ~sub_territory, ~item_cbs_code, ~feed_group, ~feed_quality,
    ~avail_dm_t, ~feed_scale,
    2000L, "prov_a", 2514L, "cereals", "high_quality", 80, "national",
    2000L, "prov_a", 2555L, "grass", "grass", 40, "provincial",
    2000L, "prov_b", 2514L, "cereals", "high_quality", 100, "national",
    2000L, "prov_b", 2555L, "grass", "grass", 60, "provincial"
  )
}

.example_crop_land_extension <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u, ~method_land,
    2000L, 33L, 2511L, 17562678, "cropland_apportion",
    2000L, 33L, 2514L, 2159391, "cropland_apportion",
    2000L, 33L, 2516L, 3206883, "cropland_apportion",
    2000L, 33L, 2555L, 7782531, "cropland_apportion",
    2000L, 100L, 2511L, 27345112, "cropland_apportion",
    2000L, 100L, 2513L, 9810455, "cropland_apportion",
    2000L, 100L, 2531L, 1204599, "cropland_apportion",
    2000L, 100L, 2555L, 7218004, "cropland_apportion",
    2000L, 110L, 2511L, 1188233, "cropland_apportion",
    2000L, 110L, 2805L, 1503221, "cropland_apportion"
  ) |>
    .add_reporting_polity_columns()
}

# afsetools parity fixtures were removed: afsetools::load_general_data() reads
# Codes_coefs.xlsx via openxlsx (segfaults intermittently on R 4.5.x) and is not
# a CI dependency, so the live parity test is not run. See test_redistribute_feed.R.

# Three 0.5-degree cells around Madrid. `area_code` is the FAOSTAT area code
# the cell grid assigns to them (203, Spain), not Spain's ISO-3166 numeric code
# (724): only the former resolves to a polity.
.example_local_intake <- function() {
  tibble::tribble(
    ~year, ~area_code, ~sub_territory, ~live_anim_code, ~item_cbs_code,
    ~feed_type, ~supply, ~intake, ~intake_dry_matter, ~loss, ~loss_share,
    2000L, 203L, "-3.75_40.25", 960L, 3000L, "grass", 1250, 1250, 250, 0, 0,
    2000L, 203L, "-3.75_40.25", 960L, 2591L, "crops", 11, 11, 10, 0, 0,
    2000L, 203L, "-3.25_40.25", 961L, 3000L, "grass", 900, 900, 180, 0, 0,
    2000L, 203L, "-3.25_40.25", 976L, 3500L, "scavenging", 0, 0, 0, 0, 0,
    2000L, 203L, "-3.25_40.75", 1049L, 2591L, "crops", 22, 22, 20, 0, 0
  ) |>
    .add_reporting_polity_columns()
}

.example_build_feed_demand <- function(by = "category") {
  if (by == "feed_type") {
    return(tibble::tribble(
      ~year, ~territory, ~sub_territory, ~livestock_category, ~item_cbs_code,
      ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
      2000L, "79", NA_character_, "Cattle_milk", NA_integer_,
      NA_character_, "grass", 1800000, TRUE,
      2000L, "79", NA_character_, "Cattle_milk", NA_integer_,
      NA_character_, "high_quality", 2800000, FALSE,
      2000L, "79", NA_character_, "Cattle_milk", NA_integer_,
      NA_character_, "residues", 1200000, FALSE,
      2000L, "79", NA_character_, "Pigs", NA_integer_,
      NA_character_, "high_quality", 7400000, FALSE,
      2000L, "79", NA_character_, "Pigs", NA_integer_,
      NA_character_, "scavenging", 1300000, FALSE
    ))
  }
  tibble::tribble(
    ~year, ~area_code, ~livestock_category, ~demand_dm_t, ~method_demand,
    2000L, 79L, "Cattle_milk", 5.8e6, "ipcc_tier2_energy",
    2000L, 79L, "Cattle_meat", 9.4e6, "ipcc_tier2_energy",
    2000L, 79L, "Sheep", 1.1e6, "ipcc_tier2_energy",
    2000L, 79L, "Goats", 2.0e5, "ipcc_tier2_energy",
    2000L, 79L, "Pigs", 8.7e6, "bouwman_fcr",
    2000L, 79L, "Poultry", 3.9e6, "bouwman_fcr",
    2000L, 79L, "Horses", 1.5e5, "krausmann_per_head",
    2000L, 79L, "Other", 3.0e4, "krausmann_per_head"
  ) |>
    .add_reporting_polity_columns()
}

.ex_land_balance_footprint <- function() {
  tibble::tribble(
    ~area_code, ~item_cbs_code, ~value, ~method,
    10L, 3000L, 328899491, "land_balance",
    41L, 3000L, 391524410, "land_balance",
    101L, 2615L, 109730, "land_balance",
    114L, 772L, 962, "land_balance",
    122L, 2520L, 3865, "land_balance",
    126L, 2617L, 6491, "land_balance",
    137L, 2613L, 40.6, "land_balance",
    179L, 2514L, 5009, "land_balance",
    188L, 2535L, 1.83, "land_balance",
    236L, 2537L, 1304, "land_balance"
  )
}

# Gridded water-balance fixture. Constructed so that, for every row, the 4-term
# identity water_input_mm == aet_mm + runoff_mm + drainage_mm +
# soil_water_change_mm holds exactly (and aet_mm == aet_blue_mm + aet_green_mm),
# and the additive identity water_input_mm == prec_mm + irrig_mm holds exactly,
# letting both the closure and prec/irrig-split tests pass. drainage_mm =
# water_input - aet - runoff - soil_water_change for each row. blue/green
# consumptive water mirror the blue/green AET (the per-CFT inputs are absent in
# the fixture); cft_nir_mm is NA (no net-irrigation-requirement input).
# method_water carries the default cft_native blue/green label.
.example_water_balance <- function() {
  label <- "aet:components|drain:seepage|bg:cft_native"
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~water_input_mm, ~prec_mm, ~irrig_mm,
    ~pet_mm, ~aet_mm, ~aet_blue_mm, ~aet_green_mm, ~blue_consump_mm,
    ~green_consump_mm, ~cft_nir_mm, ~drainage_mm, ~runoff_mm,
    ~soil_water_change_mm, ~method_water, ~polity_frac, ~cell_area_ha,
    9.25, 47.75, 11L, 2000L, 1200, 950, 250, NA, 800, 200, 600, 200, 600, NA,
    300, 50, 50, label, 1, 30100,
    9.75, 47.75, 11L, 2000L, 1100, 880, 220, NA, 760, 180, 580, 180, 580, NA,
    260, 40, 40, label, 1, 30100,
    -55.25, -12.25, 21L, 2000L, 1800, 1300, 500, NA, 1300, 400, 900, 400, 900,
    NA, 400, 80, 20, label, 1, 33500,
    -55.75, -12.25, 21L, 2000L, 1750, 1270, 480, NA, 1260, 380, 880, 380, 880,
    NA, 400, 70, 20, label, 1, 33500,
    35.75, -1.25, 79L, 2000L, 900, 720, 180, NA, 650, 150, 500, 150, 500, NA,
    170, 30, 50, label, 1, 30900,
    35.25, -1.25, 79L, 2000L, 950, 760, 190, NA, 690, 160, 530, 160, 530, NA,
    190, 30, 40, label, 1, 30900,
    -3.75, 40.25, 203L, 2000L, 600, 500, 100, NA, 420, 80, 340, 80, 340, NA,
    130, 20, 30, label, 1, 27500,
    -3.25, 40.25, 203L, 2000L, 650, 540, 110, NA, 460, 90, 370, 90, 370, NA,
    140, 20, 30, label, 1, 27500
  )
}

# Monthly SOC climate-driver fixture (one cell, three months). Temperature and
# topsoil soil-water saturation drive the SOC decomposition modifiers; clay is a
# soil-texture covariate. precip_mm and pet_mm (monthly) drive the Century
# modifier; water_minus_pet_mm is the monthly RothC/HSOC surplus (here
# precip_mm - pet_mm, irrigation zero); water_balance_mm is the annual sum of
# that surplus (-10 + 5 + 20 = 15), repeated on every month for the AMG modifier.
# theta/t_field/t_wilt/porosity drive the ICBM moisture response: t_field, t_wilt
# and porosity are the loam-class references (0.29/0.14/0.43) and theta is the
# monthly volumetric water content swc_topsoil * porosity.
.example_soc_climate_drivers <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~month, ~temp_c, ~swc_topsoil, ~precip_mm,
    ~pet_mm, ~water_minus_pet_mm, ~water_balance_mm, ~clay_pct, ~theta,
    ~t_field, ~t_wilt, ~porosity, ~method_water_input,
    9.25, 47.75, 11L, 2000L, 1L, 1.2, 0.62, 45, 55, -10, 15, 18, 0.2666,
    0.29, 0.14, 0.43, "lpjml_prec_irrig",
    9.25, 47.75, 11L, 2000L, 2L, 3.4, 0.58, 50, 45, 5, 15, 18, 0.2494,
    0.29, 0.14, 0.43, "lpjml_prec_irrig",
    9.25, 47.75, 11L, 2000L, 3L, 7.8, 0.51, 60, 40, 20, 15, 18, 0.2193,
    0.29, 0.14, 0.43, "lpjml_prec_irrig"
  ) |>
    .add_reporting_polity_columns()
}

# SOC dynamics selector output (ICBM model, six years x two pools) in the
# uniform long schema: the young and old pool stocks, the year's total repeated
# on each pool row, and the method_soc stamp naming the model that ran.
.example_soc_dynamics <- function() {
  tibble::tribble(
    ~year, ~pool, ~stock_mgc_ha, ~soc_total, ~method_soc,
    0L, "y", 2.748751, 50.000000, "icbm",
    0L, "o", 47.251249, 50.000000, "icbm",
    1L, "y", 2.611771, 49.854975, "icbm",
    1L, "o", 47.243204, 49.854975, "icbm",
    2L, "y", 2.550222, 49.775657, "icbm",
    2L, "o", 47.225435, 49.775657, "icbm",
    3L, "y", 2.522566, 49.725947, "icbm",
    3L, "o", 47.203381, 49.725947, "icbm",
    4L, "y", 2.510140, 49.689628, "icbm",
    4L, "o", 47.179488, 49.689628, "icbm",
    5L, "y", 2.504556, 49.659408, "icbm",
    5L, "o", 47.154852, 49.659408, "icbm"
  )
}

# Historical gridded SOC balance fixture (one cell, two land-use classes, three
# years). Generated from a real build_carbon_balance(model = "hsoc") run: the
# cell starts at the fraction-weighted equilibrium density, marches forward on
# the yearly areas, and in 2001 Cropland shrinks while NonCropland grows so the
# land-use-change transfer (luc_transfer_mgc_ha) sums to zero across the cell.
.example_carbon_balance <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~land_use, ~year, ~area_ha, ~stock_mgc_ha,
    ~mineralization_mgc_ha, ~c_input_mgc_ha, ~luc_transfer_mgc_ha,
    ~rate_mgc_ha, ~son_change_kgn_ha, ~method_soc,
    0.250000, 0.250000, 1L, "Cropland", 2000L, 60.000000, 37.346076,
    2.096878, 2.500000, 0.000000, 0.403122, -36.647441, "hsoc",
    0.250000, 0.250000, 1L, "NonCropland", 2000L, 40.000000, 37.346076,
    2.107845, 1.500000, 0.000000, -0.607845, 55.258678, "hsoc",
    0.250000, 0.250000, 1L, "Cropland", 2001L, 50.000000, 37.749198,
    2.119512, 2.500000, -7.549840, 0.380488, -34.589790, "hsoc",
    0.250000, 0.250000, 1L, "NonCropland", 2001L, 50.000000, 36.940424,
    2.084950, 1.500000, 7.549840, -0.584950, 53.177282, "hsoc",
    0.250000, 0.250000, 1L, "Cropland", 2002L, 50.000000, 38.129686,
    2.140876, 2.500000, 0.000000, 0.359124, -32.647669, "hsoc",
    0.250000, 0.250000, 1L, "NonCropland", 2002L, 50.000000, 36.355474,
    2.051935, 1.500000, 0.000000, -0.551935, 50.175910, "hsoc"
  ) |>
    .add_reporting_polity_columns()
}

# Toy fixture for read_cru_climate (sampled from a real CRU 4.09 tmp read,
# degrees Celsius, year 2000).
.example_cru_climate <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~value, ~var,
    22.75, -31.25, 2000L, 7L, 8.6, "tmp",
    49.25, -14.25, 2000L, 1L, 20.8, "tmp",
    109.75, 1.25, 2000L, 1L, 26.2, "tmp",
    57.25, 25.75, 2000L, 1L, 20.7, "tmp",
    -1.75, 27.25, 2000L, 1L, 12.7, "tmp",
    68.25, 27.25, 2000L, 1L, 16.2, "tmp",
    -92.75, 38.75, 2000L, 1L, -0.1, "tmp",
    42.25, 58.75, 2000L, 7L, 19.0, "tmp",
    18.75, 66.25, 2000L, 7L, 13.0, "tmp",
    80.75, 72.75, 2000L, 7L, 5.6, "tmp"
  )
}

# Gridded LUH2 land-use-class fixture: three 0.5-degree cells, one year, the
# four carbon-balance classes. Sampled from a real
# read_luh2_landuse(resolution = "grid", years = 2015) run on the LUH2-GCB2022
# states and the polycell support, so it shows what the schema really looks
# like: cell (9.25, 47.75) is a BORDER cell, shared between Germany (79) and
# Switzerland (211), whose two polycells carry the SAME `fraction` -- LUH2's
# share of the whole cell -- and different `area_ha`, each class's share of that
# cell's LUH2 land spread over its own polycell's measured land.
.example_luh2_landuse <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~year,
    ~land_use,
    ~fraction,
    ~area_ha,
    ~method_land_area,
    -3.25, 40.25, 203L, 2015L, "cropland", 0.4717921, 111305.54, "polycell_land",
    -3.25, 40.25, 203L, 2015L, "grassland", 0.2671119, 63017.22, "polycell_land",
    -3.25, 40.25, 203L, 2015L, "natural", 0.2285400, 53917.33, "polycell_land",
    -3.25, 40.25, 203L, 2015L, "urban", 0.0325560, 7680.63, "polycell_land",
    9.25, 47.75, 79L, 2015L, "cropland", 0.2331222, 33737.83, "polycell_land",
    9.25, 47.75, 79L, 2015L, "grassland", 0.2005688, 29026.65, "polycell_land",
    9.25, 47.75, 79L, 2015L, "natural", 0.3654755, 52892.21, "polycell_land",
    9.25, 47.75, 79L, 2015L, "urban", 0.0498313, 7211.67, "polycell_land",
    9.25, 47.75, 211L, 2015L, "cropland", 0.2331222, 14178.27, "polycell_land",
    9.25, 47.75, 211L, 2015L, "grassland", 0.2005688, 12198.40, "polycell_land",
    9.25, 47.75, 211L, 2015L, "natural", 0.3654755, 22227.86, "polycell_land",
    9.25, 47.75, 211L, 2015L, "urban", 0.0498313, 3030.69, "polycell_land",
    35.25, -1.25, 114L, 2015L, "cropland", 0.1634304, 50505.91, "polycell_land",
    35.25, -1.25, 114L, 2015L, "grassland", 0.4838348, 149522.47, "polycell_land",
    35.25, -1.25, 114L, 2015L, "natural", 0.3527347, 109007.80, "polycell_land",
    35.25, -1.25, 114L, 2015L, "urban", 0.0000000, 0.00, "polycell_land"
  ) |>
    .add_reporting_polity_columns()
}

# Per-PFT annual LPJmL NPP fixture: two 0.5-degree cells, one year, a handful
# of PFT bands (one natural tree, one natural grass, the two managed
# grasslands). Values are per-PFT-stand gC/m2/yr. Mirrors read_lpjml_npp()
# output. Sampled from the real pft_npp.nc (year 2000, indicative magnitudes).
.example_lpjml_npp <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~npft, ~name_pft, ~value,
    26.25, 35.25, 2000L, 3L, "temperate needleleaved evergreen tree", 48.6,
    26.25, 35.25, 2000L, 9L, "Tropical C4 grass", 325.0,
    26.25, 35.25, 2000L, 10L, "Temperate C3 grass", 66.9,
    26.25, 35.25, 2000L, 25L, "rainfed grassland", 496.0,
    -64.25, -35.75, 2000L, 3L, "temperate needleleaved evergreen tree", 699.0,
    -64.25, -35.75, 2000L, 10L, "Temperate C3 grass", 96.2,
    -64.25, -35.75, 2000L, 25L, "rainfed grassland", 910.0,
    -74.75, -52.25, 2000L, 10L, "Temperate C3 grass", 279.0,
    -74.75, -52.25, 2000L, 25L, "rainfed grassland", 325.0,
    -74.75, -52.25, 2000L, 41L, "irrigated grassland", 0.0
  )
}

# Grassland + natural soil carbon input fixture: two cells, one year, the two
# carbon-balance classes. c_input_mgc_ha_yr is (NPP - harvest) in MgC/ha/yr
# (grassland also adds grazing excreta); humified_fraction is the weed value for
# grassland and the woody value for natural. Mirrors
# build_grass_natural_carbon_inputs() output at "grid" resolution. `area_code`
# is the FAOSTAT area code the cell grid assigns to each cell: 84 (Greece) for
# the Crete cell and 9 (Argentina) for the pampas cell. Their ISO-3166 numeric
# codes (300, 32) do not belong in this column -- 300 resolves to no polity at
# all and 32 is FAOSTAT's code for Cameroon.
# nolint start: object_length_linter.
.example_grass_natural_carbon_inputs <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use,
    ~c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    26.25, 35.25, 84L, 2000L, "grassland",
    4.35, 0.1153467, "lpjml_npp_minus_harvest",
    26.25, 35.25, 84L, 2000L, "natural",
    4.56, 0.325, "lpjml_npp_minus_harvest",
    -64.25, -35.75, 9L, 2000L, "grassland",
    1.95, 0.1153467, "lpjml_npp_minus_harvest",
    -64.25, -35.75, 9L, 2000L, "natural",
    9.26, 0.325, "lpjml_npp_minus_harvest"
  ) |>
    .add_reporting_polity_columns()
}
# nolint end

# Per-land-use-class carbon inputs mirroring build_carbon_inputs() output at
# "grid" resolution: the cropland class (aggregated from per-crop inputs) plus
# the grassland and natural classes, keyed (lon, lat, area_code, year,
# land_use) with c_input_mgc_ha_yr and the carbon-weighted humified_fraction.
.example_carbon_inputs <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~year, ~land_use,
    ~c_input_mgc_ha_yr, ~humified_fraction, ~method_c_input,
    0.25, 0.25, 1L, 2000L, "cropland", 2.75, 0.1818182, "humified_weighted",
    0.25, 0.25, 1L, 2000L, "grassland", 4.0, 0.1153467, "lpjml_npp_minus_harvest",
    0.25, 0.25, 1L, 2000L, "natural", 6.0, 0.325, "lpjml_npp_minus_harvest"
  ) |>
    .add_reporting_polity_columns()
}

.ex_grazing_feed_footprint <- function() {
  tibble::tribble(
    ~area_code, ~item_cbs_code, ~value, ~method,
    10L, 2848L, 184625300, "grazing_feed_allocation",
    10L, 2731L, 71204900, "grazing_feed_allocation",
    41L, 2731L, 38950100, "grazing_feed_allocation",
    33L, 2848L, 24310700, "grazing_feed_allocation",
    33L, 2732L, 9875400, "grazing_feed_allocation",
    100L, 2731L, 6420300, "grazing_feed_allocation",
    79L, 2848L, 3155800, "grazing_feed_allocation",
    179L, 2735L, 812600, "grazing_feed_allocation",
    122L, 2740L, 49120, "grazing_feed_allocation",
    137L, 2732L, 18430, "grazing_feed_allocation"
  )
}

.example_get_faostat_data <- function() {
  tibble::tribble(
    ~area, ~item, ~element, ~year, ~value, ~unit, ~ISO3_CODE,
    "Portugal", "Asses", "stocks", 2010L, 1500, "An", "PRT",
    "Portugal", "Cattle, dairy", "stocks", 2010L, 245000, "An", "PRT",
    "Portugal", "Cattle, non-dairy", "stocks", 2010L, 1180000, "An", "PRT",
    "Portugal", "Chickens, broilers", "stocks", 2010L, 27000, "1000 An", "PRT",
    "Portugal", "Goats", "stocks", 2010L, 412000, "An", "PRT",
    "Portugal", "Horses", "stocks", 2010L, 22000, "An", "PRT",
    "Portugal", "Mules and hinnies", "stocks", 2010L, 3200, "An", "PRT",
    "Portugal", "Sheep", "stocks", 2010L, 2230000, "An", "PRT",
    "Portugal", "Swine, breeding", "stocks", 2010L, 340000, "An", "PRT",
    "Portugal", "Swine, market", "stocks", 2010L, 1980000, "An", "PRT"
  )
}

.example_create_grafs_plot_df <- function() {
  tibble::tribble(
    ~province, ~year, ~label, ~data, ~align, ~arrowColor,
    "Huesca", 2000, "{ARAiN}", "12.34", "R", "",
    "Huesca", 2000, "{CROPS_TO_LIVESTOCK}", "45.6", "L", "",
    "Huesca", 2000, "{POPULATIONM}", "0.22", "L", "",
    "Huesca", 2000, "{PROVINCE_NAME}", "Huesca", "L", "",
    "Huesca", 2000, "{WIDTH_MAX}", "1500", "L", "",
    "Huesca", 2000, "{YEAR}", "2000", "L", "",
    "Lleida", 2000, "{ARArN}", "8.9", "R", "",
    "Lleida", 2000, "{LVSTCKTOTN}", "3.21", "L", "",
    "Spain", 2000, "{CRPLNDTOTN}", "120.5", "R", "",
    "Spain", 2000, "{POPULATIONM}", "40.1", "L", ""
  )
}

.example_critical_n <- function() {
  tibble::tribble(
    ~lon, ~lat, ~value,
    -0.75, 51.75, 9,
    -0.25, 51.75, 84,
    0.25, 51.75, 12,
    -0.75, 51.25, 120,
    -0.25, 51.25, 47,
    0.25, 51.25, 63
  )
}

# A small build_nitrogen_balance()-shaped fixture (8 crop-cell-year rows, two
# cells, a nitrogen deficit and a zero-surplus row included) constructed so the
# harvest-removal surplus is exactly checkable. burnt_residue_n_t varies but
# does not enter the surplus.
.example_n_surplus_balance <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~n_input_std_t,
    ~prod_n_t,
    ~used_residue_n_t,
    ~grazed_weeds_n_t,
    ~burnt_residue_n_t,
    ~n_balance_t,
    0.25, 0.25, 1L, 2511L, 2010L, 100, 50, 20, 5, 0, 3, 22,
    0.25, 0.25, 1L, 2513L, 2010L, 50, 10, 8, 1, 0, 1, 0,
    0.25, 0.25, 1L, 2555L, 2010L, 40, 4, 6, 0, 0, 0, -2,
    0.75, 0.25, 1L, 2511L, 2010L, 200, 120, 40, 10, 8, 5, 55,
    0.75, 0.25, 1L, 2513L, 2010L, 80, 30, 12, 3, 0, 2, 12,
    0.25, 0.25, 1L, 2511L, 2011L, 100, 60, 25, 5, 2, 4, 20,
    0.75, 0.25, 1L, 3000L, 2010L, 300, 15, 5, 0, 10, 0, -3,
    0.25, 0.25, 1L, 2555L, 2011L, 20, 8, 3, 1, 0, 1, 3
  )
}

.example_n_surplus <- function() {
  calculate_n_surplus(.example_n_surplus_balance())
}

# A shared 2x2-cell surplus + critical pair, run through the real
# build_n_boundary_exceedance() at grid resolution (surplus metric), spanning a
# crop above the critical value and one below it.
.example_n_boundary_exceedance <- function() {
  surplus <- tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~n_input_std_t,
    ~surplus_kgn_ha,
    0.25, 0.25, 1L, 2511L, 2010L, 100, 12, 80,
    0.25, 0.25, 1L, 2513L, 2010L, 50, 4, 30,
    0.75, 0.25, 1L, 2511L, 2010L, 200, 30, 90,
    0.25, 0.75, 1L, 2511L, 2010L, 40, 6, 60,
    0.75, 0.75, 1L, 2555L, 2010L, 10, 3, 150
  )
  critical <- tibble::tribble(
    ~lon, ~lat, ~value, ~source_area_ha, ~image_region,
    0.25, 0.25, 50, 100, 11L,
    0.75, 0.25, 120, 100, 11L,
    0.25, 0.75, 40, 100, 11L,
    0.75, 0.75, 100, 100, 11L
  ) |>
    dplyr::mutate(
      critical_var = "critical_n_surplus",
      critical_land_use = "all",
      critical_threshold = "mi",
      critical_year = 2010L
    )
  build_n_boundary_exceedance(
    surplus = surplus,
    critical = critical,
    land_use = "all",
    resolution = "grid",
    metric = "surplus",
    actual_year = 2010L,
    critical_reference_year = 2010L
  )
}

# The embodied-nitrogen trade footprint fixture (fp_all + fp_food). Produced by
# tracing the exceedance category of a minimal 2-region x 2-item scenario through
# build_sjos_n_footprint(): area 1 exports part of its item-10 nitrogen to area
# 2's food demand (the single traded flow), everything else is consumed
# domestically. Total fp_all embodied N (175 t) equals the extension total; the
# food subset drops area 1's item-20 other-uses flow (20 t).
.ex_build_sjos_n_footprint <- function() {
  fp_all <- tibble::tribble(
    ~year, ~origin_area, ~origin_item, ~target_area, ~target_item, ~target_fd,
    ~origin, ~impact_u, ~item_cbs_code, ~category,
    2000L, 1L, 10L, 1L, 10L, "food",
    "Domestic consumption", 60, 10L, "exceedance",
    2000L, 1L, 20L, 1L, 20L, "other_uses",
    "Domestic consumption", 20, 20L, "exceedance",
    2000L, 1L, 10L, 2L, 10L, "food",
    "Traded", 40, 10L, "exceedance",
    2000L, 2L, 10L, 2L, 10L, "food",
    "Domestic consumption", 40, 10L, "exceedance",
    2000L, 2L, 20L, 2L, 20L, "food",
    "Domestic consumption", 15, 20L, "exceedance"
  )
  fp_food <- tibble::tribble(
    ~year, ~origin_area, ~origin_item, ~target_area, ~target_item, ~target_fd,
    ~origin, ~impact_u, ~item_cbs_code, ~category,
    2000L, 1L, 10L, 1L, 10L, "food",
    "Domestic consumption", 60, 10L, "exceedance",
    2000L, 1L, 10L, 2L, 10L, "food",
    "Traded", 40, 10L, "exceedance",
    2000L, 2L, 10L, 2L, 10L, "food",
    "Domestic consumption", 40, 10L, "exceedance",
    2000L, 2L, 20L, 2L, 20L, "food",
    "Domestic consumption", 15, 20L, "exceedance"
  )
  list(fp_all = fp_all, fp_food = fp_food)
}

.example_n_pathway_exceedance <- function() {
  balance <- tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~nh3_n_t,
    ~no3_n_t,
    0.25, 0.25, 1L, 2511L, 2010L, 100, 3.0, 5.0,
    0.25, 0.25, 1L, 2513L, 2010L, 50, 0.5, 0.8,
    0.75, 0.25, 1L, 2511L, 2010L, 200, 8.0, 4.0,
    0.25, 0.75, 1L, 2555L, 2010L, 40, 1.2, 3.0
  )
  critical_loads <- list(
    crit_nh3_emission = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 20,
      0.75, 0.25, 25,
      0.25, 0.75, 15
    ) |>
      dplyr::mutate(critical_var = "crit_nh3_emission"),
    crit_leaching_gw = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 30,
      0.75, 0.25, 40,
      0.25, 0.75, 50
    ) |>
      dplyr::mutate(critical_var = "crit_leaching_gw"),
    crit_load_sw = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 40,
      0.75, 0.25, 20,
      0.25, 0.75, 60
    ) |>
      dplyr::mutate(critical_var = "crit_load_sw")
  )
  build_n_pathway_exceedance(
    balance = balance,
    critical_loads = critical_loads,
    nh3_source = "soil",
    resolution = "grid"
  )
}

# Per-capita food-supply fixture (whep_native path, default "edible_portion"
# protein basis). Taken from a real build_food_supply() run on this input set,
# so it can be regenerated by pasting it back in:
#
#   coefs <- tibble::tribble(
#     ~Name_biomass, ~N_kgN_kgFM, ~Product_kgN_kgDM, ~Product_kgDM_kgFM,
#     ~Edible_portion, ~GE_product_edible_portion_MJ_kgFM, ~GE_product_MJ_kgFM,
#     "Wheat",  0.019,    0.020, 0.87, 1.00, 13.0,     14.0,
#     "Potato", NA_real_, 0.014, 0.21, 0.85, NA_real_,  3.5
#   )
#   items <- tibble::tribble(
#     ~item_cbs_code, ~Name_biomass, 2511L, "Wheat", 2531L, "Potato"
#   )
#   cbs <- tibble::tribble(
#     ~year, ~area_code, ~item_cbs_code, ~food_t,
#     2010L, 10L, 2511L, 1200, 2010L, 10L, 2531L, 800,
#     2010L, 32L, 2511L,  400,
#     2011L, 10L, 2511L, 1000, 2011L, 10L, 2531L, 600
#   )
#   pop <- tibble::tribble(
#     ~year, ~area_code, ~population,
#     2010L, 10L, 10000, 2010L, 32L, 5000, 2011L, 10L, 10200
#   )
#
# Wheat exercises the N_kgN_kgFM branch with no inedible fraction; Potato the
# product-nitrogen fallback scaled by an Edible_portion below 1, and the
# energy coalesce from edible to whole product.
.example_build_food_supply <- function() {
  tibble::tribble(
    ~year, ~area_code, ~protein_g_cap_day, ~energy_kcal_cap_day, ~population,
    ~method_food_supply, ~method_protein_basis,
    2010L, 10L, 42.4643835616, 1204.850834228, 10000, "whep_native",
    "edible_portion",
    2010L, 32L, 26.0273972603, 681.002645433, 5000, "whep_native",
    "edible_portion",
    2011L, 10L, 34.4134434596, 969.375937598, 10200, "whep_native",
    "edible_portion"
  ) |>
    .add_reporting_polity_columns()
}

# The single coherent input set that drives build_sjos_nitrogen(example = TRUE)
# end to end. Two countries (1, 2), one year (2010), four 0.5-degree cells and
# three crops, laid out so every module join is non-empty and consistent: the
# balance grid keys match the critical and critical-load cells; the balance
# country/year keys match the commodity-balance food, population and
# nitrogen-input keys; and the biomass_coefs / items_full bridge covers every
# food item. The surplus, ammonia and nitrate values put some crop-cells above
# and some below their critical value so the exceedance, pathway and
# classification tables each carry both outcomes. Populations and input masses
# are world-country scale so the per-capita boundary and nourishment scores land
# in a sensible range. A single named list, one entry per injected module input.
.sjos_n_example_data <- function() {
  list(
    balance = .sjos_n_balance_fixture(),
    critical = .sjos_n_critical_fixture(),
    critical_loads = .sjos_n_crit_loads_fixture(),
    cbs_food = .sjos_n_cbs_food_fixture(),
    population = .sjos_n_pop_fixture(),
    n_inputs = .sjos_n_inputs_fixture(),
    biomass_coefs = .sjos_n_coefs_fixture(),
    items_full = .sjos_n_items_fixture()
  )
}

# Gridded nitrogen-balance fixture carrying both the surplus terms (net input
# and the harvested-nitrogen exports) and the pathway losses (ammonia, nitrate),
# so the one balance feeds calculate_n_surplus() and build_n_pathway_exceedance()
# alike.
.sjos_n_balance_fixture <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~n_input_std_t,
    ~prod_n_t,
    ~used_residue_n_t,
    ~grazed_weeds_n_t,
    ~burnt_residue_n_t,
    ~n_balance_t,
    ~nh3_n_t,
    ~no3_n_t,
    0.25, 0.25, 1L, 2511L, 2010L, 100, 50, 20, 5, 0, 3, 22, 3.0, 5.0,
    0.25, 0.25, 1L, 2513L, 2010L, 50, 10, 8, 1, 0, 1, 0, 0.5, 0.8,
    0.75, 0.25, 1L, 2511L, 2010L, 200, 120, 40, 10, 8, 5, 55, 8.0, 4.0,
    0.75, 0.25, 1L, 2555L, 2010L, 40, 4, 6, 0, 0, 0, -2, 1.2, 3.0,
    10.25, 5.25, 2L, 2511L, 2010L, 80, 30, 12, 3, 0, 2, 12, 2.5, 6.0,
    10.25, 5.25, 2L, 2513L, 2010L, 60, 8, 6, 1, 0, 0, 1, 0.4, 0.5,
    10.75, 5.25, 2L, 2555L, 2010L, 20, 8, 3, 1, 0, 1, 3, 0.6, 2.0
  )
}

# Critical nitrogen surplus (kg N/ha/yr) at the four balance cells.
.sjos_n_critical_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~value, ~source_area_ha, ~image_region,
    0.25, 0.25, 50, 150, 11L,
    0.75, 0.25, 300, 240, 11L,
    10.25, 5.25, 200, 140, 20L,
    10.75, 5.25, 300, 20, 20L
  ) |>
    dplyr::mutate(
      critical_var = "critical_n_surplus",
      critical_land_use = "ara",
      critical_threshold = "mi",
      critical_year = 2010L
    )
}

# The three medium-specific critical loads (kg N/ha/yr) at the four balance
# cells: ammonia emission (air), groundwater leaching and surface-water load.
.sjos_n_crit_loads_fixture <- function() {
  list(
    crit_nh3_emission = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 20,
      0.75, 0.25, 25,
      10.25, 5.25, 20,
      10.75, 5.25, 15
    ) |>
      dplyr::mutate(critical_var = "crit_nh3_emission"),
    crit_leaching_gw = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 30,
      0.75, 0.25, 40,
      10.25, 5.25, 30,
      10.75, 5.25, 50
    ) |>
      dplyr::mutate(critical_var = "crit_leaching_gw"),
    crit_load_sw = tibble::tribble(
      ~lon, ~lat, ~value,
      0.25, 0.25, 40,
      0.75, 0.25, 20,
      10.25, 5.25, 40,
      10.75, 5.25, 60
    ) |>
      dplyr::mutate(critical_var = "crit_load_sw")
  )
}

# Commodity-balance food tonnes per country-crop, sized so the per-capita
# protein lands under the floor for country 1 and over the ceiling for country
# 2 (an Under and an Over nourishment class).
.sjos_n_cbs_food_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~food_t,
    2010L, 1L, 2511L, 5.0e8,
    2010L, 1L, 2513L, 1.0e8,
    2010L, 1L, 2555L, 4.0e7,
    2010L, 2L, 2511L, 6.0e8,
    2010L, 2L, 2513L, 8.0e7,
    2010L, 2L, 2555L, 4.0e7
  )
}

# National populations (absolute persons), world-country scale so the per-capita
# boundary bounds are realistic.
.sjos_n_pop_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~population,
    2010L, 1L, 4.0e9,
    2010L, 2L, 3.0e9
  )
}

# Long-format nitrogen inputs for the per-capita reactive-nitrogen axis: the
# synthetic and biological-fixation terms feed the anthropogenic total; the
# manure term is present to confirm it is excluded by the framing.
.sjos_n_inputs_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~fert_type, ~n_input_t,
    2010L, 1L, "synthetic", 4.0e7,
    2010L, 1L, "bnf", 2.0e7,
    2010L, 1L, "manure", 1.0e7,
    2010L, 2L, "synthetic", 3.4e7,
    2010L, 2L, "bnf", 1.7e7,
    2010L, 2L, "manure", 1.0e7
  )
}

# Nutrition coefficients for the three food items (protein via Edible_N_kgFM x
# 6.25, gross energy via GE_product_edible_portion_MJ_kgFM). The other columns of
# the coalesce chains are present but unused here.
# Nitrogen sits in N_kgN_kgFM with a fully edible fraction, so the protein
# values are the same as when this fixture carried them in Edible_N_kgFM, which
# build_food_supply() no longer reads (#361).
.sjos_n_coefs_fixture <- function() {
  tibble::tribble(
    ~Name_biomass,
    ~N_kgN_kgFM,
    ~Product_kgN_kgDM,
    ~Product_kgDM_kgFM,
    ~Edible_portion,
    ~GE_product_edible_portion_MJ_kgFM,
    ~GE_product_MJ_kgFM,
    "Wheat", 0.020, NA, NA, 1, 13.0, NA,
    "Barley", 0.018, NA, NA, 1, 12.5, NA,
    "Soybeans", 0.055, NA, NA, 1, 17.0, NA
  )
}

# The item_cbs_code to Name_biomass bridge for the three food items.
.sjos_n_items_fixture <- function() {
  tibble::tribble(
    ~item_cbs_code, ~Name_biomass,
    2511L, "Wheat",
    2513L, "Barley",
    2555L, "Soybeans"
  )
}

# Ten of the 50 provinces returned by create_typologies_grafs_spain() for its
# default map_year of 1980, sampled from a real run.
.ex_typologies_grafs_spain <- function() {
  tibble::tribble(
    ~Province_name, ~Typologie,
    "Albacete", "Extensive cropping system",
    "Alicante", "Extensive cropping system",
    "Araba", "Extensive cropping system",
    "Asturias", "Specialized livestock-farming system",
    "Avila", "Extensive cropping system",
    "Huelva", "Extensive cropping system",
    "Jaen", "Extensive cropping system",
    "Leon", "Extensive cropping system",
    "Lleida", "Specialized livestock-farming system",
    "Teruel", "Extensive cropping system"
  )
}

# The three data elements create_typologies_of_josette() returns, sampled from
# a real run at year 2020 over the first ten provinces alphabetically. The
# fourth element of the real output is a ggplot, which is left out so the
# example needs no plotting package.
.example_typologies_josette <- function() {
  list(
    typologies_df = .ex_josette_typologies(),
    n_input_df = .ex_josette_n_inputs(),
    imported_feed_share_df = .ex_josette_feed_share()
  )
}

.ex_josette_typologies <- function() {
  tibble::tribble(
    ~Year, ~Province_name, ~Typology,
    2020, "A_Coruna", "Forage-based crop & livestock system",
    2020, "Albacete", "Specialized stockless cropping system",
    2020, "Alicante", "Urban system",
    2020, "Almeria", "Forage-based crop & livestock system",
    2020, "Araba", "Specialized stockless cropping system",
    2020, "Asturias", "Grass-based crop & livestock system",
    2020, "Avila", "Forage-based crop & livestock system",
    2020, "Badajoz", "Forage-based crop & livestock system",
    2020, "Barcelona", "Urban system",
    2020, "Bizkaia", "Urban system"
  )
}

.ex_josette_n_inputs <- function() {
  tibble::tribble(
    ~Year, ~Province_name, ~item, ~irrig_cat, ~Box, ~MgN_dep, ~MgN_fix, ~MgN_syn, ~MgN_manure, ~MgN_urban,
    2020, "A_Coruna", "Fodder vegetables and roots", "Irrigated", "Cropland", 1.99, 1.06, 10.2, 0., 1.32,
    2020, "Albacete", "Nuts and products", "Irrigated", "Cropland", 116., 85.1, 2190., 676., 11.4,
    2020, "Albacete", "Maize and products", "Irrigated", "Cropland", 39.4, 16.6, 2070., 25.4, 3.87,
    2020, "Albacete", "Peas", "Irrigated", "Cropland", 2.90, 33.5, 14.9, 0., 0.284,
    2020, "Almeria", "Grapes and products (excl wine)", "Irrigated", "Cropland", 1.12, 1.00, 39.0, 0., 0.971,
    2020, "Araba", "Beans", "Irrigated", "Cropland", 1.48, 7.46, 6.21, 0., 0.764,
    2020, "Araba", "Soyabeans", "Rainfed", "Cropland", 0.0215, 0.146, 0.0798, 0., 0.0111,
    2020, "Avila", "Rye and products", "Irrigated", "Cropland", 0.411, 0.242, 8.06, 0., 0.0729,
    2020, "Badajoz", "Fodder legumes", "Rainfed", "Cropland", 65.9, 1660., 13.1, 212., 12.3,
    2020, "Badajoz", "Firewood", "Rainfed", "semi_natural_agroecosystems", 6490., 12500., 0., 16400., 0.
  )
}

# Ten rows of a real build_historical_land_areas(1850:1961) run at its DEFAULT
# `boundary_step = "level_step"`, sampled across the span and across the cases
# that make this method differ from the present-day one: Ethiopia either side of
# the 1952 Eritrea handover, the dissolved federations the method reaches
# without a successor union, and Belgium, which the raster route halved by
# splitting its cells with the overlapping Belgium-Luxembourg polygon
# (whep#800).
#
# The rows this replaces were taken from a `"relink"` run, so they disagreed
# with the pin the default produces: Ethiopia 1850 read 3.2414 Mha of cropland
# where both the shipped pin and this run read 1.5174.
.example_historical_land_areas <- function() {
  tibble::tribble(
    ~year, ~area_code, ~polity_code, ~Cropland, ~Pasture, ~agriland,
    1961L, 255L, "BEL-1831-2025", 1.0152, 0.7175, 1.7327,
    1961L, 51L, "F51-1947-1993", 5.3510, 1.8063, 7.1573,
    1900L, 203L, "ESP-1800-2025", 16.1666, 8.2026, 24.3692,
    1961L, 228L, "F228-1945-1991", 237.8785, 331.6635, 569.5420,
    1850L, 238L, "ETH-1800-1889", 1.5174, 1.8841, 3.4015,
    1900L, 238L, "ETH-1897-1902", 6.0023, 13.5575, 19.5598,
    1951L, 238L, "ETH-1941-1952", 9.4543, 22.9532, 32.4075,
    1952L, 238L, "ETH-1952-1993", 10.2061, 30.0426, 40.2487,
    1961L, 238L, "ETH-1952-1993", 11.9517, 29.5874, 41.5391,
    1961L, 248L, "F248-1947-1991", 8.3956, 6.4600, 14.8556
  )
}

.ex_josette_feed_share <- function() {
  tibble::tribble(
    ~Year, ~Province_name, ~LU_total, ~Feed_import_MgN, ~Domestic_feed_MgN, ~Total_feed_MgN, ~Imported_feed_share,
    2020, "A_Coruna", 414814., 365672., 29936.3, 395608., 0.924,
    2020, "Albacete", 178482., 157337., 8753.91, 166091., 0.947,
    2020, "Alicante", 57203.3, 50426.5, 2561.46, 52987.9, 0.952,
    2020, "Almeria", 224946., 198297., 11141.4, 209439., 0.947,
    2020, "Araba", 87341.1, 76993.9, 2576.11, 79570.0, 0.968,
    2020, "Asturias", 407178., 358940., 15068.6, 374009., 0.960,
    2020, "Avila", 339709., 299464., 14377.8, 313842., 0.954,
    2020, "Badajoz", 1005423., 886311., 37603.4, 923915., 0.959,
    2020, "Barcelona", 776463., 684476., 39563.0, 724039., 0.945,
    2020, "Bizkaia", 106968., 94295.4, 3505.52, 97800.9, 0.964
  )
}
