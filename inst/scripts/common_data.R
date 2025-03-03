# Common vectors and settings of Spain_Hist repository
# Eduardo Aguilera, IEGD, CSIC
# Start date: May, 2020

# Define local routes ----

input_path <- here::here("inst/extdata/input/raw")

# Folder to store large files. All input and output large files are stored in subfolders of this folder
large_files_path <- paste0(input_path, "/large-files/")

# Folder with general coefficients and functions. Route to the location of the local copy of the General repository
# By default, the General repository is a sub-folder of the main folder path
general_path <- paste0(input_path, "/general/")

# By default, the General repository is a sub-folder of the main folder path
spain_path <- paste0(input_path, "/spain/")

# Load functions, coefficients and settings from the General repository ----
source(paste0(general_path, "R/Codes_coefs_fx.R"))

# Create time series ----
min_year <- 1860
max_year <- 2021
years <- c(min_year:max_year)
Years <- base::data.frame(years, years) |>
  dplyr::rename(Year = years)
years_agg <- c(1900, 1910, 1922, 1933, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2008) # Time points in works from Agroecosystem History Laboratory
years_ext <- c(1850:max_year)
Years_ext <- dplyr::tibble(Year = years_ext)
Normal_61_90 <- c(1961:1990)
Normal_01_30 <- c(1901:1930)
Years_ext <- base::data.frame(years_ext, years_ext) |>
  dplyr::rename(Year = years_ext)
All_years <- c(1594:max_year)
All_Years <- base::data.frame(All_years, All_years) |>
  dplyr::rename(Year = All_years)
Cultidata_years <- c(1990:2020)

# Functions ----

# Extract production and trade data from FAOSTAT
ExtractSpain <- function(x) {
  utils::read.csv(paste0(large_files_path, x)) |>
    dplyr::filter(
      Area == "Spain",
      Item.Code < 2905,
      !Item.Code %in% c(2815, 2827, 2556, 2738, 2739, 2818), # Remove redundant categories
      !Item %in% c(
        "Sugar, Refined Equiv",
        "Whey",
        "Milk, Whole",
        "Milk, Skimmed",
        "Cheese",
        "Cheese (All Kinds)",
        "Cream",
        "Roots & Tuber Dry Equiv",
        "Oilseeds, cake equivalent"
      )
    ) |>
    dplyr::mutate(
      Item = ifelse(Item == "Groundnuts (in Shell Eq)",
        "Groundnuts",
        Item
      ),
      Element = stringr::str_replace(Element, "Domestic supply quantity", "Domestic_supply"), # Harmonize element names
      Element = stringr::str_replace(Element, "Stock Variation", "Stock_variation"),
      Element = stringr::str_replace(Element, "Other uses \\(non-food\\)", "Other_uses"),
      Element = stringr::str_replace(Element, "Other uses", "Other_uses"),
      Element = stringr::str_replace(Element, "Food supply quantity \\(tonnes\\)", "Food")
    ) |>
    dplyr::select(-Area, -Area.Code) |>
    dplyr::mutate(Element = stringr::str_replace(Element, " Quantity", ""))
}

# Load cultidata (MAPA 1990-2020 series, correcting some errors
load_cultidata <- function() {
  utils::read.csv("./output/Cultidata_1990_2020.csv") |>
    dplyr::left_join(Names_MAPA |>
      dplyr::select(COD_CULTI, Name)) |>
    dplyr::left_join(Biomass_names |>
      dplyr::select(Name, Name_biomass)) |>
    dplyr::left_join(Names_cats |>
      dplyr::select(Name, Cat_1)) |>
    dplyr::left_join(Biomass_coefs |>
      dplyr::select(Name_biomass, Product_kgDM_kgFM)) |>
    dplyr::mutate(
      Prod_Mg = ifelse(Cat_1 == "Fodder_green", # Express the production of fodder crops as fresh matter
        Prod_Mg / Product_kgDM_kgFM,
        Prod_Mg
      ),
      Yield = Prod_Mg / Area_ha,
      Area_ha = ifelse(is.na(Prod_Mg),
        Area_ha,
        ifelse(Yield > 1000, # Three cases with too high yields (Flowers in Tarragona in 2008 and Huelva in 2012)
          Area_ha * 10,
          Area_ha
        )
      ),
      Area_ha = ifelse(Province_name == "Guadalajara" & Irrig_cat == "Rainfed" & Year == 2009 & COD_CULTI == "L28", # Cases with big jumps in the area of a perennial crop are corrected manually
        21768,
        ifelse(Province_name == "A_Coruna" & Irrig_cat == "Rainfed" & Year == 2007 & COD_CULTI == "L25",
          Area_ha / 5,
          Area_ha
        )
      ),
      Prod_Mg = ifelse(Province_name == "A_Coruna" & Irrig_cat == "Rainfed" & Year == 2007 & COD_CULTI == "L25",
        Prod_Mg / 5,
        Prod_Mg
      ),
      Synthetic_tN = ifelse(Province_name == "A_Coruna" & Irrig_cat == "Rainfed" & Year == 2007 & COD_CULTI == "L25",
        Synthetic_tN / 5,
        Synthetic_tN
      ),
      Yield = Prod_Mg / Area_ha
    ) |>
    dplyr::select(-Product_kgDM_kgFM) |>
    dplyr::full_join(openxlsx::read.xlsx("./input/Crops.xlsx",
      sheet = "Rice_2016",
      startRow = 1,
      check.names = TRUE
    ) |>
      dplyr::rename(
        Area_rice = Area_ha,
        Prod_rice = Prod_Mg
      ) |>
      dplyr::mutate(
        Year = 2016,
        Name_biomass = "Rice",
        Irrig_cat = "Irrigated",
        COD_CULTI = "H07",
        CULTIVOS = "Arroz",
        Name = "Rice",
        Cat_1 = "Cereals"
      ) |>
      dplyr::left_join(Prov_codes |>
        dplyr::select(Province_name, COD_PROV))) |>
    dplyr::mutate(
      Area_ha = ifelse(is.na(Area_rice),
        Area_ha,
        Area_rice
      ),
      Prod_Mg = ifelse(is.na(Prod_rice),
        Prod_Mg,
        Prod_rice
      ),
      Yield = Prod_Mg / Area_ha,
      Synthetic_kgNha = Synthetic_tN * 1000 / Area_ha
    ) |>
    dplyr::arrange(Year) |>
    dplyr::group_by(Province_name, Name_biomass, Name, CULTIVOS, Irrig_cat) |>
    Filling(Synthetic_kgNha, Year) |>
    dplyr::mutate(
      Synthetic_tN = Synthetic_kgNha * Area_ha / 1000,
      Manure_tN = tidyr::replace_na(Manure_tN, 0),
      Organic_other_tN = tidyr::replace_na(Organic_other_tN, 0)
    ) |>
    dplyr::mutate(Province_name = ifelse(Year == 2001 & Province_name == "Teruel" & Name_biomass == "Rice",
      "Zaragoza",
      Province_name
    )) |>
    dplyr::mutate(
      Grazed_ratio = Grazed_area_share / Grazed_prod_share, # In most of the cases (ca. 99%), the grazed area and production shares are the same. Therefore I ignore the area and focus on production only
      Grazed_prod_Mg = Prod_Mg * Grazed_prod_share
    ) |>
    dplyr::select(-Area_rice, -Prod_rice, -Grazed_area_share, -Grazed_prod_share, -Grazed_ratio)
}

# Load provincial data of selected crops
Tidy_prov <- function(x) {
  tidyxl::xlsx_cells("./input/Crops_prov.xlsx",
    sheets = x
  ) |>
    dplyr::select(row, col, data_type, numeric, character) |>
    unpivotr::behead("N", Unit) |> # Extract column headers
    unpivotr::behead("N", Irrig) |>
    unpivotr::behead("N", Name) |>
    unpivotr::behead("W", PROVINCIA) |>
    unpivotr::behead("W", Year) |>
    dplyr::select(numeric, Unit:Year) |>
    dplyr::rename(Value = numeric) |>
    dplyr::mutate(Year = as.numeric(Year)) |>
    dplyr::right_join(Years) |>
    dplyr::select(-years.1) |>
    tidyr::complete(Year, Name, Irrig, PROVINCIA, Unit) |>
    dplyr::filter(!is.na(Unit))
}

Extract_by_ProvLU <- function(VarMap, LUMap) {
  VarMap <- raster::crop(VarMap, prov_Spain)
  var_LU <- raster::resample(VarMap, LUMap, method = "bilinear") |> # Set the extent of the variable to the extent of crop
    raster::stack(LUMap) # Join land use and variable maps, and extract weighted mean (https://gis.stackexchange.com/questions/324599/weighted-mean-of-raster-by-polygons-in-r)
  var_lu <- raster::extract(var_LU, prov_Spain)
  var_LU_provs <- data.frame(
    unlist(lapply(var_lu, FUN = function(x) {
      weighted.mean(x = x[, 1], w = x[, 2], na.rm = TRUE)
    })),
    prov_Spain$COD_PROV
  ) |>
    dplyr::rename(varname = 1, "COD_PROV" = 2)
}

Sort_named_provinces <- function(x) {
  x |>
    dplyr::left_join(Prov_codes |>
      dplyr::select(Province_name, NAMENUTS3)) |>
    dplyr::mutate(
      Province_name = factor(Province_name,
        levels = c(Prov_levels_all)
      ),
      NAMENUT3 = ifelse(Province_name == "Spain",
        "Spain",
        as.character(NAMENUTS3)
      ),
      NAMENUT3 = factor(NAMENUTS3,
        levels = c(NAMENUTS3_levels_all)
      )
    )
}

# Products and coproducts to Primary product
Products_from_Processing <- function(dataset_processing, dataset_aggregated) {
  dataset_processing |>
    dplyr::left_join(CB_processing) |>
    dplyr::filter(!is.na(ProcessedItem)) |>
    dplyr::left_join(dataset_aggregated |>
      dplyr::filter(Element == "Production") |>
      dplyr::rename(Value_Product = Value) |>
      dplyr::select(-Element)) |>
    dplyr::left_join(dataset_aggregated |>
      dplyr::filter(Element == "Production") |>
      dplyr::rename(
        ProcessedItem = Item,
        Value_Coproduct = Value
      ) |>
      dplyr::select(-Element)) |>
    dplyr::mutate(
      Product_from_processing = Value_Product / Processing,
      Coproduct_from_processing = Value_Coproduct / Processing,
      Coproduct_to_product = Value_Coproduct / Value_Product
    )
}

# Impacts per Functional Unit
Impacts_per_FU <- function(x) {
  x |>
    dplyr::mutate(
      Value_ha = Value * 1000 / Area_ygpit_ha,
      Value_Prod = Value * 1000 / Prod_ygpit_Mg,
      Value_ref = Value * 1000 / Ref_var
    )
}



# Calculating seed rates. used in Trade_Uses.R and Scenarios.R
Seed_rates <- function(Crop_AreaNPP, PIE_FullDestinies) {
  Crop_AreaNPP |>
    dplyr::left_join(Names_biomass_CB) |>
    dplyr::group_by(Year, Item) |>
    dplyr::summarize(Area_yp_ha = sum(Area_ygpit_ha)) |>
    dplyr::left_join(PIE_FullDestinies |>
      dplyr::filter(
        Element == "Domestic_supply",
        Destiny == "Seed"
      ) |>
      dplyr::select(Year, Item, Value_destiny)) |>
    dplyr::mutate(Value_kgha = Value_destiny * 1000000 / Area_yp_ha) # Some strange values (oats, beans, etc.)
}

Merge_irrig <- function(x) {
  x |>
    dplyr::mutate(Irrig = ifelse(Irrig_cat == "Rainfed",
      "Rainfed",
      "Irrigated"
    ))
}

Year_Months <- Month_numbers |>
  dplyr::mutate(Year = 2000) |>
  dplyr::right_join(Years_ext) |>
  tidyr::complete(Month_names, Year) |>
  dplyr::select(Year, Month_names) |>
  dplyr::filter(
    !is.na(Year),
    !is.na(Month_names)
  )

# Codes and coefficients ----
Names_Livestock <- openxlsx::read.xlsx(paste0(spain_path, "Livestock.xlsx"),
  sheet = "Names_categoria_Species"
)

Names_Livestock_NIR <- openxlsx::read.xlsx(paste0(spain_path, "Livestock.xlsx"),
  sheet = "Names_NIR_Species"
)

Names_Livestock_cat <- openxlsx::read.xlsx(paste0(spain_path, "Livestock.xlsx"),
  sheet = "Names_cat_Species"
)

Names_live <- openxlsx::read.xlsx(paste0(spain_path, "Livestock.xlsx"),
  sheet = "Names_live"
)

Names_poultry_rabbits <- openxlsx::read.xlsx(paste0(spain_path, "Livestock.xlsx"),
  sheet = "Poultry_rabbits"
)

Names_manure_dest <- openxlsx::read.xlsx(paste0(spain_path, "Livestock.xlsx"),
  sheet = "Names_manure_dest"
)

# Land use codes and coefficients (LU_type_es, LU_simple_es. GL_es, LandUse_coefs)
wb <- openxlsx::loadWorkbook(paste0(spain_path, "LU_codes.xlsx"))
sheet_names <- names(wb)
for (sheet_name in sheet_names) {
  df <- openxlsx::read.xlsx(wb, sheet = sheet_name)
  assign(sheet_name, df)
}

LU_simple_es <- LU_simple_es |>
  tidyr::pivot_wider(
    names_from = LU_simple,
    values_from = LU_esp
  )

GL_es <- GL_es |>
  tidyr::pivot_wider(
    names_from = Geog_Level,
    values_from = GL_esp
  )

# #Country codes
Country_codes <- openxlsx::read.xlsx(paste0(spain_path, "Country_codes.xlsx"),
  sheet = "Country_codes"
)

Country_classif <- Country_codes |>
  dplyr::select(Country, Continent)

# Inputs keys
inputs_key <- openxlsx::read.xlsx(paste0(spain_path, "Inputs_coefs.xlsx"),
  sheet = "Item_key",
  startRow = 1
)

variable_key <- openxlsx::read.xlsx(paste0(spain_path, "Inputs_coefs.xlsx"),
  sheet = "Variable_key",
  startRow = 1
)


Item_eq <- openxlsx::read.xlsx(paste0(spain_path, "Farm_inputs.xlsx"),
  sheet = "Item_Type_eq",
  startRow = 1
)

Input_type_key <- openxlsx::read.xlsx(paste0(spain_path, "Inputs_coefs.xlsx"),
  sheet = "Input_type_key",
  startRow = 1
)

# Grazing emission factors
Grazing_EFs <- openxlsx::read.xlsx(paste0(spain_path, "Livestock.xlsx"),
  sheet = "Grazing_EFs",
  startRow = 1
)

# Food waste shares
Waste_shares <- openxlsx::read.xlsx(paste0(spain_path, "Waste.xlsx"),
  sheet = "Waste_shares",
  startRow = 1
)



# Provincial spatial data ----

# Define projection for Spain
Spain_crs <- "+proj=utm +zone=29 +ellps=intl +units=m +no_defs" # https://spatialreference.org/ref/epsg/ed50-utm-zone-30n/

# Province codes
Prov_codes <- openxlsx::read.xlsx(paste0(spain_path, "Province_Codes.xlsx"),
  sheet = "Codes"
)

# Province boundaries
prov_Spain <- sf::st_read(paste0(large_files_path, "NaturalEarth/States_Provs_shape/ne_10m_admin_1_states_provinces.shp")) |>
  dplyr::filter(iso_a2 == "ES") |>
  dplyr::right_join(Prov_codes |>
    dplyr::filter(!is.na(iso_3166_2))) |>
  dplyr::select(
    COD_PROV,
    geometry
  )

prov_sf <- prov_Spain |> # Create sf object with provinces, excluding Canary islands
  dplyr::left_join(Prov_codes) |>
  dplyr::filter(Province_name %!in% c("Las_Palmas", "Tenerife")) |>
  dplyr::select(Province_name, geometry)

# Get the total province area
prov_area <- prov_Spain |>
  dplyr::left_join(Prov_codes |>
    dplyr::select(COD_PROV, Province_name)) |>
  dplyr::mutate(Area_Mha = as.numeric(sf::st_area(prov_Spain) / 10000000000)) |>
  dplyr::select(Province_name, Area_Mha) |>
  sf::st_drop_geometry()

# Get latitudes of province centroids
prov_Spain$prov_lat <- sf::st_centroid(prov_Spain$geometry)

prov_Spain_centroids <- data.frame(
  prov_Spain$COD_PROV,
  prov_Spain$prov_lat
)
prov_Spain_coords <- data.frame(sf::st_coordinates(prov_Spain_centroids$geometry))

prov_Spain_lat_lon <- data.frame(
  COD_PROV = prov_Spain$COD_PROV,
  long = prov_Spain_coords$X,
  lat = prov_Spain_coords$Y
) |>
  dplyr::left_join(Prov_codes) |>
  dplyr::select(Province_name, long, lat)

prov_Spain_lat <- data.frame(
  prov_Spain$COD_PROV,
  prov_Spain_coords$Y
) |>
  dplyr::rename("COD_PROV" = 1, "lat" = 2)

# Province levels
Prov_codes <- prov_Spain_lat |>
  dplyr::left_join(Prov_codes) |>
  dplyr::arrange(desc(lat)) |>
  dplyr::mutate(
    Province_name = as.character(Province_name),
    NAMENUTS3 = as.character(NAMENUT3)
  )

Prov_levels <- dplyr::pull(
  Prov_codes,
  Province_name
)
Prov_levels_all <- c("Spain", Prov_levels)

NAMENUTS3_levels <- dplyr::pull(
  Prov_codes,
  NAMENUTS3
)
NAMENUTS3_levels_all <- c("Spain", NAMENUTS3_levels)

Prov_codes <- Prov_codes |> # Order Prov_codes by latitude
  dplyr::mutate(Province_name = factor(Province_name, levels = Prov_levels))

# Land uses in 2000, from Global Land Cover Share ############### (ONLY CROPLAND USED FOR NOW)
get_LU <- function(LU_path) {
  raster::raster(LU_path) |>
    raster::crop(prov_Spain) |>
    raster::aggregate(fact = 10, fun = mean)
}
Artificial_sp <- get_LU(paste0(large_files_path, "GLC_Share_GlobalLandCoverShare/glc_Artificial_shv10_01.tif"))
Cropland_sp <- get_LU(paste0(large_files_path, "GLC_Share_GlobalLandCoverShare/glc_Cropland_shv10_02.tif"))
Grassland_sp <- get_LU(paste0(large_files_path, "GLC_Share_GlobalLandCoverShare/glc_Grassland_shv10_03.tif"))
Forest_sp <- get_LU(paste0(large_files_path, "GLC_Share_GlobalLandCoverShare/glc_Trees_shv10_04.tif"))

# DRI data
DRI_ages <- openxlsx::read.xlsx(paste0(spain_path, "Diets_coefs.xlsx"),
  sheet = "DRI",
  startRow = 1
) |>
  dplyr::filter(!is.na(Variable)) |>
  dplyr::select(-`Gest_1st_half`, -`Gest_2nd_half`, -Lactation) |>
  tidyr::pivot_longer(`1_3_y`:Women_over_60,
    names_to = "Age_group",
    values_to = "DRI_value"
  ) |>
  dplyr::select(Variable, Threshold, Age_group, DRI_value) |>
  dplyr::filter(!is.na(DRI_value))

# Nutrients in organic vs conventional foods
Nutrients_org_conv <- openxlsx::read.xlsx(paste0(spain_path, "Diets_coefs.xlsx"),
  sheet = "Nutrients_org_conv",
  startRow = 1
) |>
  dplyr::select(Cat_diet_agg, Cat_animal, Variable, Org_conv) |>
  dplyr::filter(!is.na(Variable))

Nutrients_org_conv_Cat_diet_agg <- Nutrients_org_conv |>
  dplyr::filter(!is.na(Cat_diet_agg)) |>
  dplyr::select(Cat_diet_agg, Variable, Org_conv)

Nutrients_org_conv_AniVeg <- Nutrients_org_conv |>
  dplyr::filter(!is.na(Cat_animal)) |>
  dplyr::mutate(Org_conv_aniveg = Org_conv) |>
  dplyr::select(Variable, Cat_animal, Org_conv_aniveg)

Nutrients_org_conv_DM <- Nutrients_org_conv |>
  dplyr::filter(
    Cat_animal == "Vegetal",
    Variable == "DM"
  ) |>
  dplyr::select(Cat_animal, Org_conv) |>
  dplyr::rename(Org_conv_DM = Org_conv)
