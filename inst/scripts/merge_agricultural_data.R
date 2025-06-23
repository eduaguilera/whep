# Install required libraries
install.packages(c("dplyr", "readr", "readxl", "ggplot2", "tidyr"))
library(dplyr)
library(readr)
library(readxl)
library(ggplot2)
library(tidyr)

# Load required functions and paths
source("C:/Users/Usuario/Desktop/WHEP_ALE/CODES/Global/R/ComDat_Global.R", echo = TRUE)
# Load primary production and land use data
Primary_all <- utils::read.csv(paste0(L_files_path, "Global/output/Primary_all.csv"))

# Read Europe and LATAM animal data
europe_animal_data <- read_excel("C:/Users/Usuario/Desktop/WHEP_ALE/DATA/EXTRACTED/Europe_FAO_completed.xlsx", sheet = "Livestock")
latam_animal_data <- read_excel("C:/Users/Usuario/Desktop/WHEP_ALE/DATA/EXTRACTED/LatinAmerica_FAO_completed.xlsx", sheet = "Livestock")

# Identifying year columns for animal data
year_columns_animal <- grep("^(18|19)[0-9]{2}$", colnames(latam_animal_data), value = TRUE)

# Convert year columns to numeric for LATAM animal data
latam_animal_data[year_columns_animal] <- latam_animal_data[year_columns_animal] |>
  lapply(function(x) suppressWarnings(as.numeric(x)))

# Read Europe animal data with NA handling
europe_animal_data <- read_excel(
  "C:/Users/Usuario/Desktop/WHEP_ALE/DATA/EXTRACTED/Europe_FAO_completed.xlsx",
  sheet = "Livestock",
  na = c("", "NA", "#N/A"),
  guess_max = 10000,
  .name_repair = "minimal"
)

# Clean and process Europe animal data
europe_animal_data <- europe_animal_data |>
  dplyr::rename_with(~ gsub("^X", "", .), starts_with("X")) |>
  dplyr::mutate(
    Area = as.character(Area),
    ISO3 = as.character(ISO3),
    Area_Code = as.numeric(Area_Code),
    Item = as.character(Item),
    Item_Code = as.character(Item_Code),
    Unit = as.character(Unit)
  )
# Remove ".0" from the year columns to make them valid years (e.g., "1800" instead of "1800.0")
colnames(europe_animal_data) <- gsub("\\.0$", "", colnames(europe_animal_data))

# Identify year columns for Europe animal data
year_columns_europe_animal <- grep("^(18|19)[0-9]{2}$", colnames(europe_animal_data), value = TRUE)

# Convert year columns to integers for Europe animal data (to remove decimals)
europe_animal_data <- europe_animal_data |>
  dplyr::mutate(across(
    all_of(year_columns_europe_animal),
    ~ as.integer(as.character(.))
  )) # Remove decimal places

# Convert to long format for Europe animal data and adjust the 'Livestock' column and 'Unit'
europe_animal_long <- europe_animal_data |>
  pivot_longer(
    cols = all_of(year_columns_europe_animal),
    names_to = "Year",
    values_to = "Livestock"
  ) |>
  dplyr::mutate(
    Livestock = Livestock * 1000,
    Unit = "heads",
    Region = "Europe",
    Item_Code = as.character(Item_Code)
  )

# Convert to long format for LATAM animal data and adjust the 'Livestock' column and 'Unit'
latam_animal_long <- latam_animal_data |>
  pivot_longer(
    cols = all_of(year_columns_animal),
    names_to = "Year",
    values_to = "Livestock"
  ) |>
  dplyr::mutate(
    Livestock = Livestock * 1000,
    Unit = "heads",
    Region = "Americas",
    Item_Code = as.character(Item_Code)
  )

# Combine the Europe and LATAM animal data
animal_manual <- dplyr::bind_rows(latam_animal_long, europe_animal_long)
animal_hq <- dplyr::bind_rows(
  animal_manual |>
    dplyr::rename(
      item = Item,
      item_code = Item_Code,
      area_code = Area_Code,
      Value = Livestock,
      unit = Unit
    ) |>
    dplyr::mutate(
      unit = "heads",
      item_code = as.numeric(item_code),
      Year = as.numeric(Year),
      Source = "Original"
    ) |>
    dplyr::select(-ISO3),
  Primary_all |>
    dplyr::mutate(Source = "FAO")
)


# Read Europe and LATAM production data
europe_production <- read_excel("C:/Users/Usuario/Desktop/WHEP_ALE/DATA/EXTRACTED/Europe_FAO_completed.xlsx", sheet = "Production")
latam_production <- read_excel("C:/Users/Usuario/Desktop/WHEP_ALE/DATA/EXTRACTED/LatinAmerica_FAO_completed.xlsx", sheet = "Production")

# Clean up column names by removing ".0" from the year columns in Europe production
colnames(europe_production) <- gsub("\\.0$", "", colnames(europe_production))

# Clean up column names by removing ".0" from the year columns in LATAM production
colnames(latam_production) <- gsub("\\.0$", "", colnames(latam_production))

# Identify year columns for Europe production (no more ".0")
year_columns_europe_production <- grep("^(18|19)[0-9]{2}$", colnames(europe_production), value = TRUE)

# Print the year columns to confirm they're being identified
print(year_columns_europe_production)

# Convert year columns to numeric for Europe production data
europe_production <- europe_production |>
  dplyr::mutate(across(all_of(year_columns_europe_production), ~ as.numeric(as.character(.))))

# Convert to long format for Europe production data
europe_production_long <- europe_production |>
  pivot_longer(
    cols = all_of(year_columns_europe_production),
    names_to = "Year",
    values_to = "Production"
  ) |>
  dplyr::mutate(
    Production = Production * 1000,
    Region = "Europe"
  )

# Identify year columns for LATAM production (no more ".0")
year_columns_latam_production <- grep("^(18|19)[0-9]{2}$", colnames(latam_production), value = TRUE)

# Print the year columns to confirm they're being identified
print(year_columns_latam_production)

# Convert year columns to numeric for LATAM production data
latam_production <- latam_production |>
  dplyr::mutate(across(all_of(year_columns_latam_production), ~ as.numeric(as.character(.))))

# Convert to long format for LATAM production data
latam_production_long <- latam_production |>
  pivot_longer(
    cols = all_of(year_columns_latam_production),
    names_to = "Year",
    values_to = "Production"
  ) |>
  dplyr::mutate(
    Production = Production * 1000,
    Region = "Americas"
  )

# Combine Europe and LATAM production data
production_manual <- dplyr::bind_rows(latam_production_long, europe_production_long)

production_hq <- dplyr::bind_rows(
  production_manual |>
    dplyr::rename(
      item = Item,
      item_code = Item_Code,
      area = Area,
      area_code = Area_Code,
      Value = Production,
      unit = Unit
    ) |>
    dplyr::mutate(
      unit = "MT",
      item_code = as.numeric(item_code),
      Year = as.numeric(Year),
      Source = "Original"
    ) |>
    dplyr::select(-ISO3),
  Primary_all |>
    dplyr::mutate(Source = "FAO")
)


# upload LATAM and Europe data
europe_data <- "C:/Users/Usuario/Desktop/WHEP_ALE/DATA/EXTRACTED/Area_Europe.xlsx"
area_europe_data <- read_excel(europe_data)

latam_data <- "C:/Users/Usuario/Desktop/WHEP_ALE/DATA/EXTRACTED/Area_LATAM.xlsx"
area_latam_data <- read_excel(latam_data)

# identifying year columuns in latam data
year_columns_latam <- grep("^(18|19)[0-9]{2}$", colnames(area_latam_data),
  value = TRUE
)

# years columns in LATAM to be numeric
area_latam_data[year_columns_latam] <- area_latam_data[year_columns_latam] |>
  lapply(function(x) suppressWarnings(as.numeric(x)))

# Read the Excel file with explicit na handling and sheet specification
area_europe_data <- read_excel(
  europe_data,
  na = c("", "NA", "#N/A"),
  guess_max = 10000, # Increase guess_max for better column type detection
  .name_repair = "minimal"
)

# Print the first few rows and structure to verify the loading
print("Initial data structure:")
str(area_europe_data)
print("\nFirst few rows:")
head(area_europe_data)

# Clean and process the data
area_europe_data <- area_europe_data |>
  # Ensure all columns are present and properly named
  dplyr::rename_with(~ gsub("^X", "", .), starts_with("X")) |>
  # Remove X if present
  # Convert columns to proper types
  dplyr::mutate(
    Area = as.character(Area),
    ISO3 = as.character(ISO3),
    Area_Code = as.numeric(Area_Code),
    Item = as.character(Item),
    Item_Code = as.character(Item_Code),
    Unit = as.character(Unit)
  )

# Identify year columns again (in case column names were affected)
year_columns_europe <- grep("^(18|19)[0-9]{2}$", colnames(area_europe_data),
  value = TRUE
)

# Convert year columns to numeric
area_europe_data <- area_europe_data |>
  dplyr::mutate(across(
    all_of(year_columns_europe),
    ~ as.numeric(as.character(.))
  ))

# Convert to long format
area_europe_long <- area_europe_data |>
  pivot_longer(
    cols = all_of(year_columns_europe),
    names_to = "Year",
    values_to = "Area_Manual"
  ) |>
  dplyr::mutate(
    Area_Manual = Area_Manual * 1000,
    Region = "Europe"
  )

# latam to long format
area_latam_long <- area_latam_data |>
  pivot_longer(
    cols = all_of(year_columns_latam),
    names_to = "Year",
    values_to = "Area_Manual"
  ) |>
  dplyr::mutate(
    Area_Manual = Area_Manual * 1000,
    Region = "Americas"
  )


# area_manual y area_hq
area_manual <- dplyr::bind_rows(area_latam_long, area_europe_long)
area_hq <- dplyr::bind_rows(
  area_manual |>
    dplyr::rename(
      item = Item,
      item_code = Item_Code,
      area = Area,
      area_code = Area_Code,
      Value = Area_Manual,
      unit = Unit
    ) |>
    dplyr::mutate(
      unit = "ha",
      item_code = as.numeric(item_code),
      Year = as.numeric(Year),
      Source = "Original"
    ) |>
    dplyr::select(-ISO3),
  Primary_all |>
    dplyr::mutate(Source = "FAO")
)

### MERGE DATA with new data sets
hq_data <- area_hq |>
  bind_rows(production_hq) |>
  bind_rows(animal_hq)


data_raw <- hq_data |>
  dplyr::left_join(regions_full |>
    dplyr::rename(area_code = code) |>
    dplyr::select(area_code, iso3c)) |>
  dplyr::left_join(regions_full |>
    dplyr::select(iso3c, name, region_UN) |>
    dplyr::rename(Country_name = name))


data_manual <- area_manual |>
  bind_rows(production_manual) |>
  bind_rows(animal_manual)



data_manual <- data_manual |>
  dplyr::mutate(
    Unit = as.character(Unit),
    value = coalesce(Production, Livestock, Area_Manual),
    value = as.numeric(value),
    value = case_when(
      Unit == "1000 ha" ~ value * 1000,
      Unit == "1000 MT" ~ value * 1000,
      Unit == "1000" ~ value * 1000,
      TRUE ~ value
    ),
    Unit = case_when(
      Unit == "1000 ha" ~ "ha",
      Unit == "1000 MT" ~ "MT",
      Unit == "1000" ~ "heads",
      TRUE ~ Unit
    )
  ) |>
  dplyr::select(-Production, -Livestock, -Area_Manual) |>
  dplyr::mutate(Source = "Manual")


## Loading mitchells dataset

mitchells_data <- utils::read.csv("C:/Users/Usuario/Desktop/WHEP/inst/extdata/output/data_1961.csv")


## Harmonizing values
mitchells_data <- mitchells_data |>
  filter(year <= 1961) |>
  mutate(
    Source = "Mitchell",
    value = as.numeric(value),
    value = case_when(
      unit == "1000 ha" ~ value * 1000,
      unit == "1000 metric tons" ~ value * 1000,
      unit == "1000 MT" ~ value * 1000,
      unit == "1000 hectolitres" ~ value * 1000,
      unit == "1000 tons" ~ value * 1000,
      unit == "1000" ~ value * 1000,
      unit == "1000000" ~ value * 1000000,
      TRUE ~ value
    ),
    unit = case_when(
      unit == "metric tons" ~ "MT",
      unit == "1000 tons" ~ "tons",
      unit == "1000 ha" ~ "ha",
      unit == "1000 metric tons" ~ "MT",
      unit == "1000" ~ "heads",
      unit == "1000000" ~ "heads",
      TRUE ~ unit
    )
  )

# Harmonizing countries

mitchells_data <- mitchells_data |>
  mutate(
    country = as.character(country),
    country = case_when(
      country == "Bolivia" ~ "Bolivia (Plurinational State of)",
      TRUE ~ country
    )
  )

# Harmonizing items

mitchells_data <- mitchells_data |>
  mutate(
    item = as.character(item),
    item = case_when(
      item == "Maize" ~ "Maize (corn)",
      TRUE ~ item
    )
  )


# Standardize column names in data_manual
data_manual <- data_manual |>
  rename(year = Year, item = Item, unit = Unit, country = Area)
# Clean the year column by converting to numeric and removing non-numeric values
mitchells_data_filtered <- mitchells_data |>
  mutate(year = as.numeric(year)) |>
  filter(!is.na(year))

data_manual_filtered <- data_manual |>
  mutate(year = as.numeric(year)) |>
  filter(!is.na(year))

# Filter datasets to include only years <= 1961
mitchells_data_filtered <- mitchells_data_filtered |>
  filter(year <= 1961)

data_manual_filtered <- data_manual_filtered |>
  filter(year <= 1961)


merged_data <- full_join(mitchells_data_filtered, data_manual_filtered, by = c("year", "country", "item", "unit")) |>
  mutate(
    value = coalesce(value.y, value.x),
    Source = coalesce(Source.x, Source.y)
  ) |>
  dplyr::select(year, country, item, unit, value, Source, source_file, sheet, -value.x, -value.y, -Source.x, -Source.y)


# Standardize FAO dataset column names BEFORE merging
primary_all <- read.csv(paste0(L_files_path, "Global/output/Primary_all.csv")) |>
  # Immediately rename to match our standard
  dplyr::rename(
    year = Year,
    country = area,
    area_code = area_code,
    value = Value
  ) |>
  # Add source and ensure consistent data type
  dplyr::mutate(
    Source = "FAO",
    item_code = as.numeric(item_code)
  )

# 4. Create item code lookup with consistent naming
item_code_lookup <- primary_all |>
  dplyr::select(item, item_code) |>
  dplyr::filter(!is.na(item_code)) |>
  distinct(item, .keep_all = TRUE) # no duplicates



merged_data <- merged_data |>
  mutate(
    year = as.numeric(year),
    country = as.character(country),
    item = as.character(item),
    unit = as.character(unit)
  ) |>
  filter(year <= 1961)

primary_all <- primary_all |>
  mutate(
    year = as.numeric(year),
    country = as.character(country),
    item = as.character(item),
    unit = as.character(unit)
  )



merged_data <- full_join(merged_data, primary_all, by = c("year", "country", "item", "unit")) |>
  mutate(
    value = coalesce(value.y, value.x),
    Source = coalesce(Source.x, Source.y)
  ) |>
  dplyr::select(year, country, item, unit, value, Source, source_file, sheet, -value.x, -value.y, -Source.x, -Source.y)


# Add item codes consistently
final_data <- merged_data |>
  left_join(item_code_lookup, by = "item") |>
  mutate(item_code = coalesce(item_code, NA_real_))


# Join with region data consistently
country_to_area_code <- regions_full |>
  rename(area_code = code) |>
  dplyr::select(name, area_code) |>
  rename(country = name)

# Create final dataset with consistent naming throughout
final_data_full <- final_data |>
  filter(!is.na(value)) |>
  left_join(country_to_area_code, by = "country") |>
  left_join(
    regions_full |>
      rename(area_code = code) |>
      dplyr::select(area_code, iso3c),
    by = "area_code"
  ) |>
  left_join(
    regions_full |>
      dplyr::select(iso3c, name, region_UN) |>
      rename(country_name = name),
    by = "iso3c"
  ) |>
  dplyr::select(-country_name)


before_1960 <- final_data_full |>
  filter(year <= 1960)

# Saving CSV full merged data
write_delim(final_data_full, "final_data_full.csv", delim = "\t", quote = "none")

# Saving CSV mixed data before 1960 (mitchell's and manual data)
write_delim(before_1960, "before_1960_full.csv", delim = "\t", quote = "none")





final_data_full |>
  dplyr::filter(item == "Cow's milk" | year <= 1961 | unit == "MT") |>
  dplyr::group_by(year, region_UN, unit, Source) |>
  dplyr::summarize(
    value = sum(value, na.rm = TRUE) / 1000000,
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = year, y = value, color = Source, linetype = Source)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  geom_point(size = 2.5, alpha = 0.9) +
  facet_wrap(~region_UN, scales = "free_y") +
  labs(
    title = "Sugar Production by UN Region and Source",
    subtitle = "Values in millions",
    x = "Year",
    y = "Value (millions of ha)",
    color = "Data Source",
    linetype = "Data Source"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.title = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 5))




before_1960 |>
  dplyr::filter(item == "Maize (corn)" | unit == "MT" | region_UN == "Africa") |>
  dplyr::group_by(year, region_UN, unit, Source) |>
  dplyr::summarize(
    value = sum(value, na.rm = TRUE) / 1000000,
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = year, y = value, color = Source, linetype = Source)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  labs(
    title = "Maize(corn) production by UN Region and Source",
    subtitle = "Values in millions of MT",
    x = "Year",
    y = "MM MT",
    color = "Data Source",
    linetype = "Data Source"
  )


before_1960 |>
  dplyr::filter(
    item == "Wheat",
    unit == "ha",
    region_UN == "Americas"
  ) |>
  dplyr::group_by(year, region_UN, unit, Source) |>
  dplyr::summarize(
    value = sum(value, na.rm = TRUE) / 1000000,
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = year, y = value, color = Source, linetype = Source)) +
  geom_line(linewidth = 1, alpha = 0.8) +
  labs(
    title = "Wheat in Africa by Source",
    subtitle = "Values in millions of ha",
    x = "Year",
    y = "MM ha",
    color = "Data Source",
    linetype = "Data Source"
  )
