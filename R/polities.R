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
  .merge_datasets() |>
    .fill_year_range() |>
    dplyr::mutate(
      polity_name = m49_name,
      polity_code = dplyr::row_number()
    ) |>
    .build_display_code() |>
    dplyr::mutate(
      across(c(polity_code, start_year, end_year), as.integer),
      across(c(polity_name, display_code, m49_code), as.character)
    ) |>
    dplyr::select(
      polity_code,
      polity_name,
      display_code,
      start_year,
      end_year,
      m49_code
    )
}

.merge_datasets <- function() {
  # TODO: Look at FAOSTAT regions not in other datasets if any, consider adding?
  k_historical_m49 |>
    # TODO: Do this cleanly. M49 code 728 is reused, so only use for new country
    dplyr::mutate(
      m49_code = ifelse(m49_name == "Spanish North Africa", NA, m49_code)
    ) |>
    dplyr::left_join(
      k_faostat_regions,
      by = "m49_code",
      suffix = c("_m49", "_fao")
    ) |>
    # TODO: Obviously wrong, use instead lookup table with correct namings
    dplyr::full_join(
      k_federico_tena_polities,
      by = dplyr::join_by(m49_name == polity_name),
      suffix = c("_m49", "_ft")
    ) |>
    dplyr::rename(start_year_ft = start_year, end_year_ft = end_year)
}

.build_display_code <- function(polities) {
  polities |>
    dplyr::mutate(
      display_code = dplyr::case_when(
        !is.na(iso3_code) ~ stringr::str_glue("{iso3_code}_{start_year}"),
        # TODO: Use something better, this is just to pass tests
        .default = stringr::str_glue("{polity_code}_{start_year}")
      )
    )
}

.fill_year_range <- function(polities) {
  # TODO: Use United Nations defined year range for out of use M49 regions
  polities |>
    pointblank::col_vals_expr(~ start_year_ft <= end_year_ft) |>
    pointblank::col_vals_expr(~ start_year_fao <= end_year_fao) |>
    dplyr::mutate(
      has_fao_years = !is.na(start_year_fao) | !is.na(end_year_fao),
      start_year = ifelse(has_fao_years, start_year_fao, start_year_ft),
      end_year = ifelse(has_fao_years, end_year_fao, end_year_ft),
    ) |>
    dplyr::select(-has_fao_years) |>
    tidyr::replace_na(list(start_year = 1970, end_year = 2025))
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
      ),
      polity_code = dplyr::case_when(
        is.na(polity_code) & !is.na(polity_code_full) ~ polity_code_full,
        TRUE ~ polity_code
      ),
      polity_name = dplyr::case_when(
        polity_code == "ROCE" ~ "Oceania Other",
        polity_code == "RAFR" ~ "Africa Other",
        polity_code == "RASI" ~ "Asia Other",
        polity_code == "REUR" ~ "Europe Other",
        polity_code == "RLAM" ~ "Latin America Other",
        polity_code == "RNAM" ~ "North America Other",
        TRUE ~ polity_name
      )
    )
}

# Used for dataset generation in constants.R
.clean_faostat_regions <- function() {
  "faostat_regions" |>
    whep_read_file() |>
    dplyr::select(
      country_code = `Country Code`,
      country_name = Country,
      m49_code = `M49 Code`,
      iso2_code = `ISO2 Code`,
      iso3_code = `ISO3 Code`,
      start_year = `Start Year`,
      end_year = `End Year`
    )
}

.clean_unstats_m49 <- function() {
  "unstats_m49" |>
    whep_read_file() |>
    dplyr::select(
      m49_code = `M49 Code`,
      m49_name = `Country or Area`,
      region1_code = `Intermediate Region Code`,
      region1_name = `Intermediate Region Name`,
      region2_code = `Sub-region Code`,
      region2_name = `Sub-region Name`,
      region3_code = `Region Code`,
      region3_name = `Region Name`,
      region4_code = `Global Code`,
      region4_name = `Global Name`,
      iso2_code = `ISO-alpha2 Code`,
      iso3_code = `ISO-alpha3 Code`,
      least_developed = `Least Developed Countries (LDC)`,
      land_locked_developing = `Land Locked Developing Countries (LLDC)`,
      small_island_developing = `Small Island Developing States (SIDS)`
    ) |>
    dplyr::mutate(
      across(
        c(least_developed, land_locked_developing, small_island_developing),
        ~ !is.na(.x)
      )
    )
}

.clean_historical_m49 <- function() {
  "historical_m49" |>
    whep_read_file() |>
    dplyr::mutate(
      is_old = !is.na(end_year),
      m49_code = stringr::str_pad(
        m49_code,
        width = 3,
        side = "left",
        pad = "0"
      )
    ) |>
    dplyr::select(-iso3166_code, -code_mismatch)
}

.clean_federico_tena <- function() {
  # open federico-tena file
  "input/processed/polities/federico-tena.xlsx" |>
    .read_local_xlsx() |>
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

.clean_federico_tena_polities <- function() {
  "federico_tena_polities" |>
    whep_read_file() |>
    dplyr::select(
      polity_name = list_of_trading_polities,
      start_year = trading_polity_starting,
      end_year = trading_polity_end,
      population = population_1913_000,
      notes = notes
    ) |>
    # There was an empty row in the original dataset
    dplyr::filter(!is.na(polity_name))
}
