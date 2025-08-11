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
    .add_common_names() |>
    .aggregate_cols() |>
    .set_polity_name_code() |>
    .build_display_code() |>
    .set_column_types() |>
    dplyr::arrange(polity_name) |>
    dplyr::select(
      polity_name,
      polity_code,
      start_year,
      end_year,
      m49_code,
      iso3_code,
      iso2_code,
      display_code
    )
}

get_polity_sources <- function(polity_codes = NULL) {
  if (is.null(polity_codes)) {
    polity_codes <- get_polities() |>
      dplyr::select(polity_code)
  } else {
    polity_codes <- tibble::tibble(polity_code = polity_codes)
  }

  polity_names <- get_polities() |>
    dplyr::inner_join(polity_codes, by = "polity_code") |>
    dplyr::select(common_name = polity_name, polity_code)

  .merge_datasets() |>
    .add_common_names() |>
    dplyr::inner_join(polity_names, by = "common_name") |>
    dplyr::arrange(original_name)
}

.merge_datasets <- function() {
  dplyr::bind_rows(
    .prepare_historical_m49(),
    .prepare_faostat(),
    .prepare_federico_tena(),
    .prepare_cshapes(),
    .prepare_whep_fixes()
  )
}

# TODO: For this to make sense... This wasn't discussed but I like
# the idea of also making the polity name (i.e. common_name) unique.
# For users, if we can afford showing longer names (instead of display_code)
# then the polity name should be enough for someone to discern.
# There's no clear usefulness for polity name not being unique.
# If we want to group by historical situations of a modern polity we will
# clearly implement other more complete methods for achieving that.
.add_common_names <- function(merged_datasets) {
  merged_datasets |>
    dplyr::inner_join(
      k_polity_common_names,
      by = c("original_name", "source"),
      unmatched = "error",
    )
}

.set_polity_name_code <- function(merged_datasets) {
  merged_datasets |>
    dplyr::rename(polity_name = common_name) |>
    dplyr::inner_join(
      k_polity_codes,
      by = "polity_name",
      unmatched = "error"
    )
}

.set_column_types <- function(polities) {
  int_cols <- c("start_year", "end_year")
  chr_cols <- setdiff(names(polities), int_cols)

  polities |>
    dplyr::mutate(
      across(all_of(int_cols), as.integer),
      across(all_of(chr_cols), as.character)
    )
}

.prepare_historical_m49 <- function() {
  k_historical_m49 |>
    # TODO: Do this cleanly. M49 code 728 is reused, so only use for new country
    dplyr::mutate(
      m49_code = ifelse(m49_name == "Spanish North Africa", NA, m49_code)
    ) |>
    dplyr::mutate(source = k_source_m49) |>
    dplyr::select(
      original_name = m49_name,
      source,
      start_year,
      end_year,
      notes,
      m49_code
    )
}

.prepare_faostat <- function() {
  k_faostat_regions |>
    dplyr::mutate(source = k_source_faostat) |>
    dplyr::select(
      original_name = country_name,
      source,
      start_year,
      end_year,
      m49_code,
      iso2_code,
      iso3_code
    )
}

.prepare_federico_tena <- function() {
  k_federico_tena_polities |>
    dplyr::mutate(
      source = k_source_federico_tena,
      # Force first year because federico tena has earliest cover
      start_year = ifelse(is.na(start_year), k_polity_first_year, start_year)
    ) |>
    dplyr::select(
      original_name = polity_name,
      source,
      start_year,
      end_year,
      notes
    )
}

.prepare_cshapes <- function() {
  k_cshapes |>
    dplyr::mutate(
      source = k_source_cshapes,
      polity_name = .add_years_in_name(polity_name, start_year, end_year)
    ) |>
    dplyr::select(
      original_name = polity_name,
      source,
      start_year,
      end_year,
      geometry
    )
}

.prepare_whep_fixes <- function() {
  k_whep_polity_fixes |>
    dplyr::mutate(source = k_source_whep) |>
    dplyr::select(
      original_name = polity_name,
      source,
      start_year,
      end_year,
      notes
    )
}

# TODO: Think if worth to shorten, maybe another mapping table
.build_display_code <- function(polities) {
  polities |>
    dplyr::mutate(display_code = polity_code)
}

.aggregate_cols <- function(polities) {
  all_cols <- names(polities)
  unique_cols <- c("m49_code", "iso2_code", "iso3_code")

  polities |>
    dplyr::summarise(
      start_year = .aggregate_start_year(start_year, source),
      end_year = .aggregate_end_year(end_year, source),
      across(all_of(unique_cols), .check_unique_value),
      .by = "common_name"
    )
}

.aggregate_start_year <- function(start_year, source) {
  # Force year from whep source if present
  i <- purrr::detect_index(source, ~ .x == k_source_whep)

  # TODO: Make this a better estimation
  # Now assuming 1938-1970 gap without country changes
  if (i > 0 && is.na(start_year[i])) {
    k_polity_first_year
  } else if (i > 0) {
    start_year[i]
  } else if (all(is.na(start_year))) {
    k_polity_first_year
  } else {
    min(start_year, na.rm = TRUE)
  }
}

.aggregate_end_year <- function(end_year, source) {
  i <- purrr::detect_index(source, ~ .x == k_source_whep)

  # TODO: Make this a better estimation
  # Now assuming 1938-1970 gap without country changes
  if (i > 0 && is.na(end_year[i])) {
    k_polity_last_year
  } else if (i > 0) {
    end_year[i]
  } else if (all(is.na(end_year))) {
    k_polity_last_year
  } else {
    max(end_year, na.rm = TRUE)
  }
}

.check_unique_value <- function(column) {
  unique_vals <- column |>
    na.omit() |>
    unique()

  if (length(unique_vals) > 1) {
    cli::cli_abort("Can't summarise, more than one unique value: {unique_vals}")
  } else if (length(unique_vals) == 0) {
    NA
  } else {
    unique_vals
  }
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
    dplyr::mutate(is_old = !is.na(end_year)) |>
    dplyr::select(-iso3166_code, -code_mismatch)
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
    dplyr::filter(!is.na(polity_name)) |>
    # Remove virtual time boundaries due to dataset study range
    dplyr::mutate(
      start_year = ifelse(start_year == 1800, NA, start_year),
      end_year = ifelse(end_year >= 1938, NA, end_year)
    )
}

.clean_cshapes <- function() {
  .load_cshapes() |>
    .filter_relevant_area_changes() |>
    # Remove virtual time boundaries due to dataset study range
    dplyr::mutate(
      start_year = ifelse(start_year <= 1886, NA, start_year),
      end_year = ifelse(end_year >= 2019, NA, end_year)
    ) |>
    dplyr::select(
      polity_name = country_name,
      start_year,
      end_year,
      area
    ) |>
    dplyr::arrange(polity_name)
}

.load_cshapes <- function() {
  countries <- cshapes::cshp() |>
    sf::st_make_valid()

  countries |>
    dplyr::mutate(
      area = countries |>
        sf::st_area() |>
        units::set_units(km^2),
      start_year = lubridate::year(start),
      end_year = lubridate::year(end),
    )
}

.filter_relevant_area_changes <- function(countries) {
  zero_km2 <- units::set_units(0, km^2)

  countries |>
    dplyr::arrange(country_name, start_year) |>
    dplyr::group_by(country_name) |>
    # Remove same year changes
    dplyr::filter(dplyr::n() == 1 | start_year != end_year) |>
    # Aggregate adjacent entries with no area change
    dplyr::mutate(group_tmp = cumsum(c(0, diff(area) != zero_km2))) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      geometry = dplyr::first(geometry),
      area = dplyr::first(area),
      start_year = min(start_year),
      end_year = max(end_year),
      .by = c("country_name", "group_tmp")
    ) |>
    dplyr::select(-group_tmp)
}

.add_years_in_name <- function(name, start_year, end_year) {
  dplyr::case_when(
    is.na(end_year) ~ stringr::str_glue("{name}"),
    is.na(start_year) ~ stringr::str_glue("{name} (to {end_year})"),
    .default = stringr::str_glue("{name} ({start_year}-{end_year})"),
  )
}

.build_auto_polity_code <- function(name, start_year, end_year) {
  start_year <- ifelse(is.na(start_year), k_polity_first_year, start_year)
  end_year <- ifelse(is.na(end_year), k_polity_last_year, end_year)
  short <- name |>
    stringr::str_to_upper() |>
    stringr::str_sub(1, 3)

  stringr::str_glue("{short}-{start_year}-{end_year}")
}

# Use with `get_polities()` as argument for debugging when
# you get failed test because of polity code not matching
# short name or year range
.get_bad_code_polities <- function(polities) {
  get_polities() |>
    tidyr::separate_wider_delim(
      polity_code,
      "-",
      names = c("code_iso", "code_start_year", "code_end_year"),
      cols_remove = FALSE
    ) |>
    dplyr::filter(
      as.integer(code_start_year) != start_year |
        as.integer(code_end_year) != end_year
    ) |>
    dplyr::select(-code_iso, -code_start_year, -code_end_year)
}

# TODO: todos from removed old code:
# - Set source of polity (but can be more than one)
# - Revise Edu's polities for special handlings
# - Introduce 'rest of continent'-like polities
#   "ROCE" "RAFR" "RASI" "REUR" "RLAM" "RNAM"
