#' Polities data integration
#'
#' @description
#' Combines polities data from two sources to create a unified dataset while
#' maintaining data integrity and applying specific transformation rules. This
#' function integrates data from the WHEP-polities sheet in Codes_coefs.xlsx
#' and the Federico-Tena
#' dataset to produce a harmonized polities dataset for the project.
#'
#' The integration process ensures consistency across both data sources and
#' applies standardized transformation protocols including manual name
#' standardization and proper source attribution.
#'
#' @returns
#' A tibble with the polities data in wide format containing the
#' following columns:
#' - `polity_code`: WHEP internal code for each polity (may be NA for some
#' records).
#' - `polity_name`: Standardized polity name used across the WHEP project.
#' - `polity_name_full`: Full original polity name for reference purposes.
#' - `polity_code_full`: Extended polity code when available (may be NA).
#' - `polity_name_FAO`: FAO standardized name for the polity when available.
#' - `polity_name_FT`: Federico-Tena standardized polity name
#' (NA for WHEP-only records).
#' - `polity_name_source`: Source attribution indicating data origin
#' ("WHEP" or "FT").
#' - `polity_code_source`: Code system source attribution
#'  ("WHEP" or "FT").
#' - `start_year`: Starting year for the trading polity period
#' (from Federico-Tena data).
#' - `end_year`: Ending year for the trading polity period
#' (from Federico-Tena data).
#' - `Comments FT`: Notes and comments from Federico-Tena dataset.
#'
#' The function applies the following name standardization rules to
#' Federico-Tena data. These are some examples:
#'
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
#' 1. Reads and preprocesses Federico-Tena data with standardized column
#' mapping
#'
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
#' historical production and trade analysis with proper source attribution and
#' standardized naming conventions.
#'
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
  federico_tena <- .read_local_xlsx(
    "input/processed/polities/federico-tena.xlsx"
  )

  colnames(federico_tena)


  federico_tena_clean <- federico_tena |>
    dplyr::select(
      polity_name_raw = `List of trading polities`,
      polity_name_FT = `List of trading polities`,
      start_year = `Trading polity Starting`,
      end_year = `Trading polity End`,
      comments_ft = Notes
    ) |>
    # manually changing polity names
    dplyr::mutate(polity_name = dplyr::case_when(
      polity_name_raw ==
        "United States" ~ "United States of America",
      polity_name_raw ==
        "Australia Commonwealth" ~ "Australia",
      polity_name_raw ==
        "Cameroon (Kamerun)" ~ "Cameroon",
      polity_name_raw ==
        "Ceylon (Sri Lanka)" ~ "Sri Lanka",
      polity_name_raw ==
        "Guinea Bisau (Portuguese Guinea)" ~ "Guinea-Bissau",
      polity_name_raw ==
        "Wallis and Futuna Island" ~ "Wallis and Futuna Islands",
      polity_name_raw ==
        "Western Samoa" ~ "American Samoa",
      polity_name_raw ==
        "Morocco (French)" ~ "Morocco",
      TRUE ~ polity_name_raw
    )) |>
    dplyr::mutate(
      polity_code = NA,
      polity_name_full = polity_name_raw,
      polity_name_FT = polity_name_raw,
      polity_code_full = NA,
      polity_name_FAO = NA,
      polity_name_source = "FT",
      polity_code_source = "FT"
    ) |>
    dplyr::select(
      polity_name,
      polity_name_FT,
      start_year, end_year, comments_ft,
      polity_code, polity_name_full, polity_code_full,
      polity_name_FAO, polity_name_source, polity_code_source
    )
}


get_final_polities <- function(federico_tena_clean) {
  whep_polities <- .read_local_xlsx(
    "input/processed/polities/whep-polities.xlsx"
  )

  colnames(whep_polities)


  whep_polities_clean <- whep_polities |>
    dplyr::mutate(polity_name_source = dplyr::case_when(
      is.na(polity_code_source) |
        is.na(polity_name_source) ~ "WHEP",
      TRUE ~ polity_name_source
    ))

  # merging
  merged_data <- whep_polities_clean |>
    dplyr::left_join(
      federico_tena_clean |>
        dplyr::select(
          polity_name, polity_name_FT, start_year, end_year, comments_ft
        ),
      by = "polity_name",
      suffix = c("", "_from_ft")
    ) |>
    # keeping polity_name_FT
    dplyr::mutate(
      polity_name_FT = dplyr::case_when(
        !is.na(polity_name_FT_from_ft) ~ polity_name_FT_from_ft,
        TRUE ~ polity_name_FT
      ),
      start_year = dplyr::coalesce(start_year, start_year_from_ft),
      end_year = dplyr::coalesce(end_year, end_year_from_ft),
      `Comments FT` = dplyr::coalesce(`Comments FT`, comments_ft)
    ) |>
    dplyr::select(-ends_with("_from_ft"))

  # Adding polities that only exist in FT
  ft_only <- federico_tena_clean |>
    dplyr::anti_join(whep_polities_clean, by = "polity_name")

  # Final columns structure
  final_columns <- c(
    "polity_code", "polity_name", "polity_name_full",
    "polity_code_full", "polity_name_FAO", "polity_name_FT",
    "polity_name_source", "polity_code_source", "start_year",
    "end_year", "Comments FT"
  )

  # Prepare FT data
  ft_final <- ft_only |>
    dplyr::select(any_of(final_columns))

  missing_cols <- setdiff(final_columns, names(ft_final))
  for (col in missing_cols) {
    ft_final[[col]] <- NA
  }

  ft_final <- dplyr::select(ft_final, all_of(final_columns))

  # Ensure merged_data has all columns
  missing_cols <- setdiff(final_columns, names(merged_data))
  for (col in missing_cols) {
    merged_data[[col]] <- NA
  }

  merged_final <- dplyr::select(merged_data, all_of(final_columns))

  # merged
  polities_final <- dplyr::bind_rows(merged_final, ft_final) |>
    dplyr::mutate(
      polity_name_source = dplyr::case_when(
        is.na(polity_name_source) ~ "WHEP",
        TRUE ~ polity_name_source
      )
    )
}
