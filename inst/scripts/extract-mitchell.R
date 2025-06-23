# extract-mitchell

# load libraries
install.packages(c(
  "dplyr", "here", "readr", "purrr",
  "readxl", "ggplot2", "tidyr", "stringr"
))
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(here)

# folder data (change as needed)
folder <- "C:/Users/Usuario/Desktop/WHEP/inst/extdata/input/processed/mitchell"

# extracting data in excel files
excel_files <- list.files(
  path = folder, pattern = "\\.xlsx$|\\.xls$",
  full.names = TRUE
)

# cleaning country names (without dots, numbers, etc)
clean_country_name <- function(country) {
  # erase numbers, dots, etc
  cleaned <- str_replace(country, "\\.\\.\\.[0-9]+$", "")
  # erase spaces
  cleaned <- str_trim(cleaned)
  return(cleaned)
}

# processing every sheet
process_sheet <- function(sheet_data) {
  # convert to dataframe
  sheet_data <- as.data.frame(sheet_data)

  # country names
  countries <- names(sheet_data)[-c(1, 2)]
  countries <- sapply(countries, clean_country_name)

  # item names
  items <- as.character(unlist(sheet_data[1, -c(1, 2)]))

  # units (1000, 1000 metric tons and 1000 ha)
  units <- as.character(sheet_data[3:nrow(sheet_data), 2])

  # extracting years
  years <- sheet_data[3:nrow(sheet_data), 1]
  years <- as.numeric(as.character(years))

  # new data frame
  result <- data.frame()

  # in every country
  for (i in seq_along(countries)) {
    # values in every country
    values <- as.character(unlist(sheet_data[3:nrow(sheet_data), i + 2]))

    # replacing "..." to NA
    values[values == "..." | values == ".." | values == "." | values == ""] <- NA

    temp_df <- data.frame(
      year = years,
      country = countries[i],
      item = items[i],
      unit = units,
      value = values,
      stringsAsFactors = FALSE
    )
    result <- rbind(result, temp_df)
  }

  # clean and convert values handling NAs
  result$value <- suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(result$value))))
  result$year <- as.numeric(as.character(result$year))

  # erase rows where the year is NA
  result <- result[!is.na(result$year), ]

  return(result)
}

# process every excel sheet
process_excel <- function(file_path) {
  # obtaining the sheet names
  sheets <- excel_sheets(file_path)

  # processing every sheet
  all_data <- map_df(sheets, function(sheet) {
    tryCatch(
      {
        sheet_data <- read_excel(file_path, sheet = sheet)
        processed_data <- process_sheet(sheet_data)
        # add the name of the data source
        processed_data$source_file <- basename(file_path)
        processed_data$sheet <- sheet
        return(processed_data)
      },
      error = function(e) {
        warning(sprintf(
          "error! in this sheet%s in this folder! %s: %s",
          sheet, basename(file_path), e$message
        ))
        return(NULL)
      }
    )
  })

  return(all_data)
}

#  verifying and joining the data
unify_types <- function(df) {
  df$unit <- as.character(df$unit)
  df$year <- as.numeric(as.character(df$year))
  df$country <- as.character(df$country)
  df$item <- as.character(df$item)
  df$value <- as.numeric(as.character(df$value))
  df$source_file <- as.character(df$source_file)
  df$sheet <- as.character(df$sheet)
  return(df)
}

#  processing data with errors
all_data <- data.frame()

for (file in excel_files) {
  tryCatch(
    {
      processed_file <- process_excel(file)
      if (!is.null(processed_file) && nrow(processed_file) > 0) {
        processed_file <- unify_types(processed_file)
        all_data <- bind_rows(all_data, processed_file)
      }
    },
    error = function(e) {
      warning(sprintf("error processing this file %s: %s", basename(file), e$message))
    }
  )
}


## Harmonizing values
data_1961 <- all_data |>
  filter(year <= 1961) |>
  mutate(
    unit = case_when(
      unit == "1000 metric tons" ~ "1000 MT",
      unit == "metric tons" ~ "MT",
      TRUE ~ unit
    )
  )

# saving data in CSV
library(readr)
write_delim(data_1961, "data_1961.csv", delim = ",", quote = "none")

# (maybe is not necessary)
final_data <- all_data |>
  arrange(country, year, item)

# saving data in CSV
library(readr)
write_delim(final_data, "final_data.csv", delim = ",", quote = "none")


### Categories

crop_prod_country <- data_1961 |>
  group_by(year, country, item, unit, value)

###               Plots examples:
#### crop production, area, animal production

crop_prod_country |>
  filter(country == "Brazil", unit == "1000 hectolitres", year <= 1961) |>
  ggplot(aes(x = year, y = value, fill = value)) +
  geom_area(alpha = 0.6) +
  labs(title = "Beer in Brazil (until 1961)", y = "1000 hectolitres") +
  facet_wrap(~item, scales = "free")


crop_prod_country |>
  filter(
    country %in%
      c("Australia", "China", "Japan", "South Africa", "Indochina"),
    item == "Pigs", unit == "1000", year <= 1961
  ) |>
  ggplot(aes(x = year, y = value, fill = country)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "Pig Production in Australia, China, South Africa, Indochina and Japan",
    x = "Year",
    y = "Production (1000 heads)"
  ) +
  theme_minimal() +
  facet_wrap(~country, scales = "free")


crop_prod_country |>
  filter(
    country %in% c("Australia"),
    item == "Wheat",
    unit == "1000 metric tons", year <= 1961
  ) |>
  ggplot(aes(x = year, y = value, fill = country)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "Wheat Area in Australia",
    x = "Year",
    y = "Area (1000 metric tons)"
  ) +
  theme_minimal() +
  facet_wrap(~country, scales = "free")

crop_prod_country |>
  filter(
    country %in% c("Australia"),
    item == "Wheat", unit == "1000 ha", year <= 1961
  ) |>
  ggplot(aes(x = year, y = value, fill = country)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "Wheat production in Australia",
    x = "Year",
    y = "Production (1000 ha)"
  ) +
  theme_minimal() +
  facet_wrap(~country, scales = "free")


crop_prod_country |>
  filter(
    country %in%
      c(
        "Australia", "China", "Manchuria",
        "Japan", "Indochina", "Iran"
      ),
    item == "Rice", unit == "1000 ha", year <= 1961
  ) |>
  ggplot(aes(x = year, y = value, fill = country)) +
  geom_area(alpha = 0.6) +
  labs(
    title = "Rice Production in Australia, China, Indochina, Iran and Japan",
    x = "Year",
    y = "Production (1000 ha)"
  ) +
  theme_minimal() +
  facet_wrap(~country, scales = "free")




crop_prod_country |>
  filter(country == "China", item == "Rice", unit == "1000 ha", year <= 1961) |>
  ggplot(aes(x = year, y = value, fill = value)) +
  geom_area() +
  labs(title = "Crop area in Algeria until 1961", y = "MT") +
  facet_wrap(~item, scales = "free")
