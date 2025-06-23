library(sf)
library(dplyr)
library(tidyr)
library(readxl)

# Definir usuario
user <- "Usuario"

# OPEN WHEP-POLITIES FILE:
whep_polities_path <- paste0("C:/Users/", user, "/Desktop/WHEP/inst/extdata/input/processed/polities/whep-polities.xlsx")
whep_polities <- read_excel(whep_polities_path)

colnames(whep_polities)

# OPEN FEDERICO-TENA FILE
federico_tena_path <- paste0("C:/Users/", user, "/Desktop/WHEP/inst/extdata/input/processed/polities/federico-tena.xlsx")
federico_tena <- read_excel(federico_tena_path)
colnames(federico_tena)


# Renaming columns of FT
federico_tena_clean <- federico_tena |>
  select(
    polity_name_FT = `List of trading polities`,
    start_year = `Trading polity Starting`,
    end_year = `Trading polity End`,
    `Comments FT` = Notes
  ) |>
  #changing manually the polity names that are same as WHEP/FAO or part of the new category other *continent*

    mutate(`polity_name_FT` = case_when(
    `polity_name_FT` == "United States" ~ "United States of America",
    `polity_name_FT` == "Australian Commonwealth" ~ "Australia",


    TRUE ~ `polity_name_FT`
  )) |>
  mutate(
    polity_code = NA,
    polity_name = polity_name_FT,  # Use polity_name_FT as polity_name
    polity_name_full = polity_name_FT,  # Use polity_name_FT as polity_name_full
    polity_code_full = NA,
    polity_name_FAO = NA,
    polity_name_source = "FT",
    polity_code_source = "FT"
  )

# WHEP data
whep_polities_clean <- whep_polities |>
  mutate(
    polity_name_source = case_when(
      is.na(polity_code_source) | is.na(polity_name_source) ~ "WHEP",
      TRUE ~ polity_name_source
    ),
    # Ensure Comments FT column exists
    `Comments FT` = if("Comments FT" %in% names(.)) `Comments FT` else NA
  )

# Identify polity_name_FT that don't exist in WHEP
new_ft <- federico_tena_clean$polity_name_FT[
  !federico_tena_clean$polity_name_FT %in% c(whep_polities_clean$polity_name_full,
                                             whep_polities_clean$polity_name)
]

# Filter only new FT records
federico_new <- federico_tena_clean |>
  filter(polity_name_FT %in% new_ft)

#MERGING
final_columns <- c("polity_code", "polity_name", "polity_name_full",
                   "polity_code_full", "polity_name_FAO", "polity_name_FT",
                   "polity_name_source", "polity_code_source", "start_year",
                   "end_year", "Comments FT")


whep_final <- whep_polities_clean |>
  mutate(polity_name_FT = if("polity_name_FT" %in% names(.)) polity_name_FT else NA) |>
  select(any_of(final_columns)) |>
  # Add missing columns with NA if they don't exist
  {
    missing_cols <- setdiff(final_columns, names(.))
    for(col in missing_cols) {
      .[[col]] <- NA
    }
    .
  } |>
  select(all_of(final_columns))

# Adjust Federico-Tena to have all columns
federico_final <- federico_new |>
  select(any_of(final_columns)) |>
  # Add missing columns with NA if they don't exist
  {
    missing_cols <- setdiff(final_columns, names(.))
    for(col in missing_cols) {
      .[[col]] <- NA
    }
    .
  } |>
  select(all_of(final_columns))

# merging the datasets
polities_whep <- bind_rows(whep_final, federico_final)

# NA to WHEP
polities_whep <- polities_whep |>
  mutate(
    polity_name_source = case_when(
      is.na(polity_name_source) ~ "WHEP",
      TRUE ~ polity_name_source
    )
  )


# Save the dataset
output_path <- paste0("C:/Users/", user, "/Desktop/WHEP/inst/extdata/output/polities_whep.xlsx")
write.xlsx(polities_whep, output_path, rowNames = FALSE)
