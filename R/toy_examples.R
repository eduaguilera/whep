#' Toy example outputs for documentation
#'
#' @description
#' Internal functions that return small example tibbles for documentation
#' and testing purposes. These avoid the need to download remote data during
#' CRAN checks and allow users to see function outputs quickly.
#'
#' @keywords internal
#' @noRd

.example_build_supply_use <- function() {
  tibble::tribble(
    ~year, ~area_code, ~proc_group, ~proc_cbs_code, ~item_cbs_code, ~type, ~value,
    2021, 255, "husbandry", 1053, 2106, "use", 1.17e+5,
    2003, 84, "crop_production", 2511, 2105, "supply", 1.62e+6,
    1982, 3, "husbandry", 976, 2737, "supply", 3.67e+2,
    2000, 8, "crop_production", 2534, 2534, "supply", 2.98e+1,
    2013, 170, "husbandry", 1049, 2594, "use", 3.21e+3,
    1985, 75, "husbandry", 1190, 1190, "supply", 1.95e+3,
    1969, 144, "husbandry", 1052, 1052, "supply", 2.18e+0,
    2010, NA, "husbandry", 976, 2807, "use", 3.33e-14,
    1998, 115, "processing", 2544, 2543, "supply", 3.77e+3,
    1967, 238, "husbandry", 960, 2740, "supply", 1.05e+3
  )
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

.example_get_feed_intake <- function() {
  tibble::tribble(
    ~year, ~area_code, ~live_anim_code, ~item_cbs_code, ~feed_type, ~supply, ~intake, ~intake_dry_matter, ~loss, ~loss_share,
    1990, 51, 1096, 2515, "crops", 429., 386., 338., 4.29e+1, 0.1,
    2007, 3, 976, 2570, "crops", 5.51, 4.96, 4.56, 5.51e-1, 0.1,
    1996, 54, 960, 2531, "crops", 5451., 4906., 1114., 5.45e+2, 0.1,
    2011, NA, 1052, 2532, "crops", 0.840, 0.756, 0.315, 8.40e-2, 0.1,
    1996, 110, 1052, 2549, "crops", 17.1, 15.4, 14.5, 1.71e+0, 0.1,
    1986, 4, 1053, 2514, "crops", 297464., 267717., 230772., 2.97e+4, 0.1,
    2010, 150, 1068, 2595, "crops", 2645., 2380., 2176., 2.64e+2, 0.1,
    1978, NA, 1096, 2536, "crops", 159., 143., 42.2, 1.59e+1, 0.1,
    2021, 23, 1053, 2511, "crops", 627., 347., 305., 2.80e+2, 0.446,
    1977, 114, 976, 2517, "crops", 32.5, 29.2, 25.7, 3.25e+0, 0.1
  )
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
  )
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
  )
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
  )
}

.example_get_wide_cbs <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~domestic_supply, ~food, ~production, ~feed, ~seed, ~import, ~export, ~other_uses, ~processing, ~stock_retrieval,
    1987L, 250L, 2106, 13741247., 0., 13741247., 1.37e7, 0., 0., 0., 0., 0., 0.,
    2012L, 41L, 2633, 82000., 82000., 0., 0., 0., 1.46e5, 68158., 0., 0., 0.,
    1984L, 123L, 2595, 1207., 0., 3854., 1.21e3, 0., 0., 4147., 0., 0., 0.,
    1982L, 165L, 2633, 86.5, 86.5, 0., 0., 0., 8.75e1, 1., 0., 0., 0.,
    1977L, 159L, 2658, 2218., 2218., 0., 0., 0., 2.22e3, 0., 0., 0., 0.,
    1995L, 234L, 2671, 4312., 0., 2500., 0., 0., 1.99e3, 178., 4312., 0., 0.,
    1975L, 10L, 677L, 2270., 2270., 2270., 0., 0., 0., 0., 0., 0., 0.,
    1961L, 156L, 2658, 6877., 6877., 2000., 0., 0., 4.88e3, 0., 0., 0., 0.,
    1961L, 236L, 2620, 11177., 11177., 0., 0., 0., 1.12e4, 0., 0., 0., 0.,
    1995L, 49L, 2734, 71117., 71117., 56724., 0., 0., 1.48e4, 0., 0., 0., 0.
  )
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
