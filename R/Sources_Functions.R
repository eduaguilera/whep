library(tidyverse)
library(openxlsx)
library(readxl)


#' Goal of function: Intakes a dataset and a harmonization table and 
#' outputs the dataset with values harmonized within years
#' 
#' dataset: the dataset in which are the products you need to harmonize
#'       Required Columns:
#'           items--------String-----The names of the original items
#'           year---------Double-----The year of the information
#'           measurement--String-----Unit of Measurement
#'           value--------Double-----What you want summarized
#'        Warning! Make sure to have dataset in long format!
#'        
#' eq_table: the equivalency table with your product names and their 
#' corresponding harmonized product
#'        Required Columns: 
#'            items-----------String----The names of the original items
#'            item_name_harm--String----The name of the harmonized item
#'            item_code_harm--Double----The id of the harmonized item
#'            harm_info-------String----The source being harmonized and the 
#'                                      end format
#'                  Example: CUST8 1899 -> FAOSTAT = cust8_1899_faostat

harmonize_product_single <- function(dataset, eq_table, source_harmonization){

  eq_table <- eq_table |>
    filter(harm_info == source_harmonization)
  
  dataset <- dataset |>
    left_join(eq_table, by = c('items' = 'items')) |>
    filter(is.na(item_name_harm) == 0) |>
    group_by(item_code_harm, item_name_harm, measurement, year) |>
    summarize(value = sum(value, na.rm = T)) |>
    rename(items = item_name_harm, item_code = item_code_harm) |>
    ungroup()
  
  return(dataset)
}

harmonize_product_adjustable_grouping <- function(dataset, eq_table, 
                                                  source_harmonization, ...){
  
  eq_table <- eq_table |>
    filter(harm_info == source_harmonization)
  
  dataset <- dataset |>
    left_join(eq_table, by = c('items' = 'items')) |>
    filter(is.na(item_name_harm) == 0) |>
    group_by(item_code_harm, item_name_harm, measurement, year, ...) |>
    summarize(value = sum(value, na.rm = T)) |>
    rename(items = item_name_harm, item_code = item_code_harm) |>
    ungroup()
  
  return(dataset)
}

#' Goal of function: Intakes a dataset and a weight harmonization table and 
#' outputs the dataset with values harmonized
#' 
#' dataset: the dataset in which are the products you need to harmonize
#'       Required Columns:
#'           items--------String-----The names of the original items
#'           measurement--String-----Unit of Measurement
#'           value--------Double-----What you want summarized
#'        Warning! Make sure to have dataset in long format!
#'        
#' eq_table: the equivalency table with your product names and their 
#' corresponding harmonized product
#'        Required Columns: 
#'            measurement---------String----The names of the original items
#'            measurement_final---String----The name of the harmonized item
#'            conversion----------Double----The id of the harmonized item
#'            product_specific----Double----Is the measurement product specific 
#'                                          (volume vs weight)
#'            product_name--------String----Standardized Product Name (FAOSTAT)
#'            product_id----------Double----Standardized Product Weight (FAOSTAT)

harmonize_weights <- function(dataset, weight_eq_table){
  product_specific_eq <- weight_eq_table |>
    filter(product_specific == 1)
  general_eq <- weight_eq_table |> 
    filter(product_specific == 0)
  
  product_specific_data <- dataset |>
    inner_join(product_specific_eq, by = c("measurement", 
                                           "items" = "product_name"))
  
  general_data <- dataset |>
    anti_join(product_specific_data, by = c("items", "measurement")) |>  
    left_join(general_eq, by = "measurement")
  
  dataset <- bind_rows(product_specific_data, general_data) |>
    mutate(value_harmonized = value * conversion) |>
    select(items, item_code, year, measurement_final, value_harmonized) |>
    rename(measurement = measurement_final, value = value_harmonized) |> 
    ungroup() |> 
    group_by(year, measurement, items, item_code) |> 
    summarize(value = sum(value, na.rm = T))
  
  return(dataset)
}

#' Goal of function: Intakes a dataset and a country harmonization table and 
#' outputs the dataset with values harmonized
#' 
#' dataset: the dataset in which are the products you need to harmonize
#'       Required Columns:
#'           items---------String-----The names of the original items
#'           measurement---String-----Unit of Measurement
#'           value---------Double-----What you want summarized
#'           from_location-String-----Country exporting (for imports)
#'           to_location---String-----Country importing (for exports)
#'        Warning! Make sure to have dataset in long format!
#'        
#' eq_table: the equivalency table with your product names and their 
#' corresponding harmonized product
#'        Required Columns: 
#'            country-------------String----The names of the original countries
#'            country_modern------String----The name of the modern state
#'            country_region------String----FAOSTAT Region of country
#'            iso3----------------String----3 letter code for modern country
#'            
#' aggregation: Level of aggregation requested for country harmonization
#'        Values:
#'            1: Harmonization of countries
#'            2: Harmonization of regions
#'            
#' import: use from_country or to_country
#'        Values:
#'            1: import doc, use from_country
#'            0: export doc, use to_country
#'            

harmonize_countries <- function(dataset, country_eq_table, aggregation = 1, 
                                import = 1, ...){
  if(import == 1) {
    dataset <- dataset |> 
      left_join(country_eq_table, by = c('from_location' = 'country'))
  }
  if(import == 0){
    dataset <- dataset |> 
      left_join(country_eq_table, by = c('to_location' = 'country'))
  }
  if(aggregation == 1){
    dataset <- dataset |> 
      group_by(items, item_code, measurement, country_modern, iso3, ...) |> 
      summarize(value = sum(value, na.rm = T)) |> 
      ungroup()
  }
  if(aggregation == 2){
    dataset <- dataset |> 
      group_by(items, item_code, measurement, country_region,...) |> 
      summarize(value = sum(value, na.rm = T)) |> 
      ungroup()
  }
  if(aggregation != 1 & aggregation != 2){
    print("Invalid Aggregation Parameter")
  }
  return(dataset)
}

#' Goal of function: import both sheets of CUST8 Excel file
#' 
#' 
read_and_format_cust_8 <- function(sheet, file_path) {
  read_excel(file_path, sheet = sheet) |> 
    as_tibble() |> 
    rename(articles = 1, measurement = 2, quantity = 3, value = 4) |> 
    mutate(
      quantity = str_replace_all(quantity, replacement_table) |>  
        str_remove_all("[^0-9]") |> as.numeric(),
      value = str_replace_all(value, replacement_table) |>  
        str_remove_all("[^0-9]") |>  as.numeric()
    )
}

#' Goal of function: join CUST8 sheets, and store as a tibble object
#' 
#' 

import_and_merge_cust8_sheets <- function(file_path) {
  file_base_name <- tools::file_path_sans_ext(basename(file_path))
  country_name <- str_remove(file_base_name, "_Reviewed$")
  sheet1 <- read_and_format_cust_8(1, file_path)
  sheet2 <- read_and_format_cust_8(2, file_path)
  merged_sheet <- bind_rows(sheet1, sheet2)  |> 
    mutate(to_location = country_name, from_location = "UK")
  if(ncol(merged_sheet) != 6){
    print(country_name)
  }
  assign(file_base_name, merged_sheet, envir = .GlobalEnv)
}

#' Fills gaps by linear interpolation, when possible, or carrying forward or 
#' backwards, when not possible, and labels output accordingly
#' Remember to group the data when needed before running the function
Filling <- function(data,var,Index){ #data=dataframe, var=variable to be filled, Index=time index (usually year)
  data  |>  
    dplyr::mutate(Value_interpfilled=zoo::na.approx({{var}},
                                                    x= zoo::index({{Index}}),
                                                    na.rm=FALSE),
                  Value_CarriedBackward=zoo::na.locf0({{var}}),
                  Value_CarriedForward=zoo::na.locf0({{var}},fromLast=TRUE),
                  "Source_{{var}}":=ifelse(!is.na({{var}}),
                                           "Original",
                                           ifelse(!is.na(Value_interpfilled),
                                                  "Linear interpolation",
                                                  ifelse(!is.na(Value_CarriedBackward),
                                                         "Last value carried forward",
                                                         "First value carried backwards"))),
                  "{{var}}":=ifelse(!is.na({{var}}),
                                    {{var}},
                                    ifelse(!is.na(Value_interpfilled),
                                           Value_interpfilled,
                                           ifelse(!is.na(Value_CarriedBackward),
                                                  Value_CarriedBackward,
                                                  Value_CarriedForward)))) |> 
    dplyr::select(-Value_interpfilled,-Value_CarriedForward,-Value_CarriedBackward,
                  -Value_CarriedForward)}



# Fills gaps by using changes in a proxy variable, using ratios between filled 
# variable and proxy variable, and labels output accordingly
# Remember to group the data when needed before running the function
FillingProxy <- function(data,var,proxyvar,Index){ #data=dataframe, var=variable to be filled, proxyvar=variable used as proxy, Index=time index (usually year)
  data |> 
    dplyr::mutate(Proxy_ratio={{var}}/{{proxyvar}}) |> 
    Filling(Proxy_ratio,Year) |> 
    dplyr::mutate("Source_{{var}}":=ifelse(!is.na({{var}}),
                                           "Original",
                                           ifelse(Source_Proxy_ratio=="Linear interpolation",
                                                  "Proxy interpolated",
                                                  ifelse(Source_Proxy_ratio=="Last value carried forward",
                                                         "Proxy carried forward",
                                                         ifelse(Source_Proxy_ratio=="First value carried backwards",
                                                                "Proxy carried backwards",
                                                                NA)))),
                  "{{var}}":=ifelse(!is.na({{var}}),
                                    {{var}},
                                    Proxy_ratio*{{proxyvar}}))}


#' Goal of function: Intakes a dataset and a harmonization table and 
#' outputs the dataset with values harmonized within years
#' 
#' data1: the dataset in which are the products you need to harmonize
#'       Further inputs:
#'           indicator_column1------String-----Name of the column with original names
#'           value_column-----------String-----Name of the column with values
#'       Warning! Make sure to have dataset in long format!
#'        
#' data2: the equivalency table with your product names and their corresponding harmonized product
#'        further inputs: 
#'            indicator_column2-----String-----Name of the column with original names
#' ...: any further columns needed to group by before harmonizing. 

harmonize_this <- function(data1, indicator_column1, value_column, data2, indicator_column2, ...){
  data1 |>
    ungroup() |> 
    left_join(data2, by = setNames(indicator_column2,indicator_column1)) |> 
    group_by(...) |> 
    summarise(value = sum(.data[[value_column]], na.rm = T)) |> 
    filter(is.na(value) == 0)
}

# harmonize_this(apples_data, "item", "value", apples, "item", year, faostat_code)
# 
# uk_imports_linked <- harmonize_this(uk_imports_linked, "items", "value", conversion_sheet_total_imports,
#                                     "items", item_code_harm, item_name_harm,  measurement, year) 
# 
# harmonize_this(country_data, "from_location", "value", country, "country", country_modern, year, 
#                item_code, items, measurement)
# 
# view(harmonize_this(clifford_standardized,  "from_location", "value", clifford_country_conversions, "country", country_modern, year, 
#                item_code, items, measurement))

view(filter(uk_bilateral_imports, iso3 == "OTC"))
