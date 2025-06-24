user <- "Usuario"

# OPEN FEDERICO-TENA FILE
federico_tena_path <- paste0(
  "C:/Users/",
  user,
  "/Desktop/WHEP/inst/extdata/input/processed/polities/federico-tena.xlsx"
)
federico_tena <- readxl::read_excel(federico_tena_path)
colnames(federico_tena)


# Renaming columns of FT
federico_tena_clean <- federico_tena |>
  dplyr::select(
    polity_name_FT_raw = `List of trading polities`,
    start_year = `Trading polity Starting`,
    end_year = `Trading polity End`,
    `Comments FT` = Notes
  ) |>
  dplyr::mutate(
    polity_code = NA,
    polity_name = NA,
    polity_name_full = polity_name_FT_raw,
    polity_code_full = NA,
    polity_name_FAO = NA,
    polity_name_source = "FT",
    polity_code_source = "FT"
  )

# Standardize polity names and create final structure
federico_tena_clean <- federico_tena_clean |>
  dplyr::select(
    polity_name_FT = polity_name_FT_raw,
    start_year, end_year, `Comments FT`, polity_name_full
  ) |>
  # Manually changing polity names
  dplyr::mutate(polity_name_FT = dplyr::case_when(
    polity_name_FT ==
      "United States" ~ "United States of America",
    polity_name_FT ==
      "Australia Commonwealth" ~ "Australia",
    polity_name_FT ==
      "Cameroon (Kamerun)" ~ "Cameroon",
    polity_name_FT ==
      "Ceylon (Sri Lanka)" ~ "Sri Lanka",
    polity_name_FT ==
      "Guinea Bisau (Portuguese Guinea)" ~ "Guinea-Bissau",
    polity_name_FT ==
      "Hong-Kong" ~ "China, Hong Kong SAR",
    polity_name_FT ==
      "South Australia" ~ "Australia",
    polity_name_FT ==
      "Wallis and Futuna Island" ~ "Wallis and Futuna Islands",
    polity_name_FT ==
      "Western Australia" ~ "Australia",
    polity_name_FT ==
      "Western Samoa" ~ "American Samoa",
    polity_name_FT ==
      "Morocco (French)" ~ "Morocco",
    TRUE ~ polity_name_FT
  )) |>
  dplyr::mutate(
    polity_code = NA,
    polity_name = polity_name_FT,
    # keeping the original
    polity_name_full = polity_name_full,
    polity_code_full = NA,
    polity_name_FAO = NA,
    polity_name_source = "FT",
    polity_code_source = "FT"
  )

federico_tena_clean_output <- paste0(
  "C:/Users/",
  user,
  "/Desktop/WHEP/inst/extdata/output/federico_tena_cleaned.csv"
)
readr::write_csv(federico_tena_clean, federico_tena_clean_output)
