#' Polities data integration
#'
#' @description
#' Combines polities data from two sources to create a unified dataset while
#' maintaining data integrity and applying specific transformation rules. This
#' function integrates data from the WHEP-polities sheet in Codes_coefs.xlsx and the Federico-Tena
#' dataset to produce a harmonized polities dataset for the project.
#'
#' The integration process ensures consistency across both data sources and
#' applies standardized transformation protocols including manual name
#' standardization and proper source attribution.
#'
#' @returns
#' A tibble with the polities data in wide format containing the following columns:
#' - `polity_code`: WHEP internal code for each polity (may be NA for some records).
#' - `polity_name`: Standardized polity name used across the WHEP project.
#' - `polity_name_full`: Full original polity name for reference purposes.
#' - `polity_code_full`: Extended polity code when available (may be NA).
#' - `polity_name_FAO`: FAO standardized name for the polity when available.
#' - `polity_name_FT`: Federico-Tena standardized polity name (NA for WHEP-only records).
#' - `polity_name_source`: Source attribution indicating data origin ("WHEP" or "FT").
#' - `polity_code_source`: Code system source attribution ("WHEP" or "FT").
#' - `start_year`: Starting year for the trading polity period (from Federico-Tena data).
#' - `end_year`: Ending year for the trading polity period (from Federico-Tena data).
#' - `Comments FT`: Notes and comments from Federico-Tena dataset.
#'
#' The function applies the following name standardization rules to Federico-Tena data. These are some examples:
#' - "United States" → "United States of America"
#' - "Australia Commonwealth" → "Australia"
#' - "Cameroon (Kamerun)" → "Cameroon"
#' - "Ceylon (Sri Lanka)" → "Sri Lanka"
#' - "Guinea Bisau (Portuguese Guinea)" → "Guinea-Bissau"
#' - "Wallis and Futuna Island" → "Wallis and Futuna Islands"
#' - "Western Samoa" → "American Samoa"
#' - "Morocco (French)" → "Morocco"
#' - All other names remain unchanged
#'
#' @details
#' The function performs the following operations:
#' 1. Reads and preprocesses Federico-Tena data with standardized column mapping
#' 2. Applies manual name corrections to align with WHEP/FAO standards
#' 3. Loads WHEP-polities data and ensures proper source labeling
#' 4. Identifies new records from Federico-Tena not present in WHEP data
#' 5. Standardizes column structure across both datasets
#' 6. Combines datasets vertically while maintaining data integrity
#' 7. Applies final cleaning rules for consistent source attribution
#'
#' Data sources:
#' - WHEP-polities sheet from the WHEP project database
#' - Federico-Tena trading polities dataset with temporal coverage
#'
#' The resulting dataset provides comprehensive polity information suitable for
#' historical trade analysis with proper source attribution and standardized
#' naming conventions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get combined polities dataset
#' polities_data <- get_polities()
#'
#' # View column structure
#' colnames(polities_data)
#'
#' # Check source distribution
#' table(polities_data$polity_name_source)
#' }
#'
get_polities <- function() {
  federico_tena <- read_ft()
  get_final_polities(federico_tena)
}


read_ft <- function() {
  # open federico-tena file
  federico_tena <- .read_local_xlsx("input/processed/polities/federico-tena.xlsx")

  colnames(federico_tena)


  # Renaming columns of ft
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

  # standardize polity names and create final structure
  federico_tena_clean |>
    dplyr::select(
      polity_name_FT = polity_name_FT_raw,
      start_year, end_year, `Comments FT`, polity_name_full
    ) |>
    # manually changing polity names
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
        "Wallis and Futuna Island" ~ "Wallis and Futuna Islands",
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
}


get_final_polities <- function(federico_tena_clean) {
  # Open whep-polities file:
  whep_polities <- .read_local_xlsx("input/processed/polities/whep-polities.xlsx")

  colnames(whep_polities)


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
    # If there's matches:
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


  # Merging:
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

  # Adjust Federico-Tena to have all columns:

  federico_final <- dplyr::select(federico_new, any_of(final_columns))


  missing_cols <- setdiff(final_columns, names(federico_final))
  for (col in missing_cols) {
    federico_final[[col]] <- NA
  }

  federico_final <- dplyr::select(federico_final, all_of(final_columns))


  # Merging the datasets:
  polities_whep <- dplyr::bind_rows(whep_final, federico_final)


  # NA to WHEP:
  polities_whep |>
    dplyr::mutate(
      polity_name_source = dplyr::case_when(
        is.na(polity_name_source) ~ "WHEP",
        TRUE ~ polity_name_source
      )
    )
}
