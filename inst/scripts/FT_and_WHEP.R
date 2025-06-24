# Merging Federico&Tena's dataset with WHEP polities dataset.
# Alejandra Fuentes-Hinojosa - WHEP project
# 06.23.2025 - IEGD/CCHS/CSIC

user <- "Usuario"

# OPEN WHEP-POLITIES FILE:
whep_polities_path <- paste0(
  "C:/Users/",
  user,
  "/Desktop/WHEP/inst/extdata/input/processed/polities/whep-polities.xlsx"
)
whep_polities <- readxl::read_excel(whep_polities_path)

colnames(whep_polities)

# OPEN FEDERICO-TENA'S CLEANED DATASET
federico_tena_clean <- paste0(
  "C:/Users/", user, "/Desktop/WHEP/inst/extdata/output/federico_tena_cleaned.csv"
)

federico_tena_clean <- readr::read_csv(federico_tena_clean)
colnames(federico_tena_clean)


whep_polities_clean <- whep_polities |>
  dplyr::mutate(polity_name_source = dplyr::case_when(
    is.na(polity_code_source) |
      is.na(polity_name_source) ~ "WHEP",
    TRUE ~ polity_name_source
  ))

ft_names <- federico_tena_clean |>
  dplyr::select(
    name_match = polity_name_FT,
    ft_name = polity_name_FT
  ) |>
  dplyr::filter(!is.na(name_match) & name_match != "") |>
  dplyr::distinct()



whep_polities_clean <- whep_polities_clean |>
  dplyr::left_join(
    ft_names,
    by = c("polity_name_full" = "name_match")
  ) |>
  dplyr::left_join(
    ft_names,
    by = c("polity_name" = "name_match"),
    suffix = c("", "_alt")
  ) |>
  # if there's matches
  dplyr::mutate(
    polity_name_FT = dplyr::coalesce(ft_name, ft_name_alt)
  ) |>
  dplyr::select(-ft_name, -ft_name_alt)


new_ft <- federico_tena_clean$polity_name_FT[
  !federico_tena_clean$polity_name_FT %in% c(
    whep_polities_clean$polity_name_full,
    whep_polities_clean$polity_name
  )
]

federico_new <- federico_tena_clean |>
  dplyr::filter(polity_name_FT %in% new_ft)


# MERGING
final_columns <- c(
  "polity_code", "polity_name", "polity_name_full",
  "polity_code_full", "polity_name_FAO", "polity_name_FT",
  "polity_name_source", "polity_code_source", "start_year",
  "end_year", "Comments FT"
)


whep_raw <- whep_polities_clean

if (!("polity_name_FT" %in% names(whep_raw))) {
  whep_raw$polity_name_FT <- NA_character_
}

whep_raw <- dplyr::select(whep_raw, any_of(final_columns))

missing_cols <- setdiff(final_columns, names(whep_raw))
for (col in missing_cols) {
  whep_raw[[col]] <- NA
}

whep_final <- dplyr::select(whep_raw, all_of(final_columns))

# Adjust Federico-Tena to have all columns

federico_final <- dplyr::select(federico_new, any_of(final_columns))


missing_cols <- setdiff(final_columns, names(federico_final))
for (col in missing_cols) {
  federico_final[[col]] <- NA
}

federico_final <- dplyr::select(federico_final, all_of(final_columns))


# merging the datasets
polities_whep <- dplyr::bind_rows(whep_final, federico_final)


# NA to WHEP
polities_whep <- polities_whep |>
  dplyr::mutate(
    polity_name_source = dplyr::case_when(
      is.na(polity_name_source) ~ "WHEP",
      TRUE ~ polity_name_source
    )
  )


output <- paste0(
  "C:/Users/",
  user,
  "/Desktop/WHEP/inst/extdata/output/polities_whep.csv"
)
readr::write_csv(polities_whep, output)
