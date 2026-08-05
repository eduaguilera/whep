#' Scrape activity data from FAOSTAT and post-process it
#'
#' @description
#' Important: Dynamically allows for the introduction of subsets as `"..."`.
#'
#' Note: overhead by individually scraping FAOSTAT code QCL for crop data;
#' it's fine.
#'
#' @param activity_data activity data required from FAOSTAT; needs
#'   to be one of `c('livestock','crop_area','crop_yield','crop_production')`.
#' @param ... can be whichever column name from `get_faostat_bulk`,
#'   particularly `year`, `area` or `ISO3_CODE`.
#' @param example Logical. If `TRUE`, return a small hardcoded example
#'   `tibble` instead of scraping FAOSTAT. Useful for offline demos and
#'   documentation. Default `FALSE`.
#'
#' @returns `tibble` of FAOSTAT for `activity_data` with columns `area`,
#'   `item`, `element`, `year`, `value`, `unit` and `ISO3_CODE`; default is
#'   for all years and countries.
#'
#' @export
#'
#' @examples
#' get_faostat_data(example = TRUE)
get_faostat_data <- function(activity_data, ..., example = FALSE) {
  if (example) {
    return(.example_get_faostat_data())
  }

  faostat_converters <- .faostat_converter(activity_data)

  # scrape bulk data from FAOSTAT for a specific parameter
  faostat_data <- FAOSTAT::get_faostat_bulk(
    code = faostat_converters[["FAOSTAT_code"]]
  )

  # subset based on activity_data OR element in FAOSTAT
  # also subset only necessary columns for post-processing
  faostat_data <- faostat_data[
    faostat_data$element == faostat_converters[["FAOSTAT_param"]],
    c("area", "item", "element", "year", "value", "unit")
  ]

  # create ISO3 codes
  faostat_data <- .populate_iso3_code(faostat_data)

  # Dynamically filter based on additional arguments passed via ...
  filter_args <- list(...)
  # Check if any filtering arguments were provided
  if (length(filter_args) > 0) {
    for (filter_name in names(filter_args)) {
      # Ensure the column exists
      if (filter_name %in% names(faostat_data)) {
        faostat_data <- faostat_data[
          faostat_data[[filter_name]] %in% filter_args[[filter_name]],
        ]
      } else {
        warning(paste("Column", filter_name, "not found in FAOSTAT data."))
      }
    }
  }

  faostat_data |>
    tibble::as_tibble()
}

#' Populates ISO3CODE based on "area" column from FAOSTAT
#'   also postprocesses "wrong" ISO3 codes
#'
#' @param df data.frame from FAOSTAT
#'
#' @noRd
#'
#' @returns data.frame
.populate_iso3_code <- function(df) {
  # create new column "ISO3_CODE" and fill it
  df[["ISO3_CODE"]] <- .match_fao_area_to_iso3(df[["area"]])

  # manually fix some crazy countries/ISO3_CODE
  df[df$area == "China, mainland", "ISO3_CODE"] <- "CHN"
  df[df$area == "T\u00FCrkiye", "ISO3_CODE"] <- "TUR"
  df[df$area == "Netherlands (Kingdom of the)", "ISO3_CODE"] <- "NLD"
  df[df$area == "Sudan", "ISO3_CODE"] <- "SDN"
  df[df$area == "South Sudan", "ISO3_CODE"] <- "SSD"
  df[df$area == "Czechia", "ISO3_CODE"] <- "CZE"
  df[df$area == "Lao People's Democratic Republic", "ISO3_CODE"] <- "LAO"

  .warn_unmatched_fao_areas(unique(df$area[is.na(df$ISO3_CODE)]))

  df
}

#' Converts activity_data on the necessary FAOSTAT code
#'   (to scrape from FAOSTAT) and the necessary FAO parameter
#'
#' @note to add new parameters from FAOSTAT IS HERE
#' @param activity_data activity data required from FAOSTAT;
#'   needs to be one of
#'   `c('livestock','crop_area','crop_yield','crop_production')`
#'
#' @noRd
#'
#' @returns list of length n=2; first index is FAOSTAT code and second index
#'   is FAOSTAT parameter
.faostat_converter <- function(activity_data) {
  if (
    length(activity_data) != 1 ||
      !(activity_data %in% .activity_data_choices())
  ) {
    stop(.bad_activity_data_param_error())
  }

  # create list to translate activity_data into FAOSTAT code
  fao_cat_converter <- list(
    "livestock" = "EMN",
    "crop_area" = "QCL",
    "crop_yield" = "QCL",
    "crop_production" = "QCL"
  )

  fao_param_converter <- list(
    "livestock" = "stocks",
    "crop_area" = "area_harvested",
    "crop_yield" = "yield",
    "crop_production" = "production"
  )

  list(
    FAOSTAT_code = fao_cat_converter[[activity_data]],
    FAOSTAT_param = fao_param_converter[[activity_data]]
  )
}

.match_fao_area_to_iso3 <- function(areas) {
  lookup <- .fao_area_iso3_lookup()

  lookup[["iso3_code"]][match(areas, lookup[["fao_area_name"]])]
}

.warn_unmatched_fao_areas <- function(unmatched) {
  if (length(unmatched) == 0) {
    return(invisible())
  }

  shown <- utils::head(unmatched, 5)
  cli::cli_warn(c(
    "Could not match {length(unmatched)} FAOSTAT area name{?s} to an
     {.field ISO3_CODE}.",
    i = "First unmatched: {.val {shown}}.",
    i = "Regional aggregates and multi-territory areas have no ISO3 code."
  ))
}

# Builds the FAOSTAT area name -> ISO3 code lookup. Reproduces the matching
# rule of FAOSTAT::fillCountryCode(): an area name is compared for exact
# equality against the six name columns of `FAOcountryProfile`, and only
# resolves when all its matches fall in a single profile row. Names matching
# several rows (e.g. the "China" aggregate) stay unmatched.
.fao_area_iso3_lookup <- function() {
  name_cols <- .fao_profile_name_cols()

  .fao_country_profile(c("ISO3_CODE", name_cols)) |>
    dplyr::mutate(
      profile_row = dplyr::row_number(),
      iso3_code = as.character(ISO3_CODE)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(name_cols),
      values_to = "fao_area_name",
      values_transform = as.character
    ) |>
    dplyr::filter(!is.na(fao_area_name)) |>
    dplyr::distinct(fao_area_name, profile_row, iso3_code) |>
    dplyr::summarise(
      iso3_code = dplyr::if_else(
        dplyr::n() == 1L,
        iso3_code[1],
        NA_character_
      ),
      .by = fao_area_name
    )
}

.fao_profile_name_cols <- function() {
  c(
    "OFFICIAL_FAO_NAME",
    "SHORT_NAME",
    "FAO_TABLE_NAME",
    "UNOFFICIAL1_NAME",
    "UNOFFICIAL2_NAME",
    "UNOFFICIAL3_NAME"
  )
}

# FAOSTAT::fillCountryCode() reads `FAOcountryProfile` as a free variable and
# so only works while the package is attached; prefixed calls fail with
# "object 'FAOcountryProfile' not found" (#520). Load the dataset explicitly
# instead, which does not depend on that lazy-load behaviour.
.fao_country_profile <- function(required_cols) {
  profile_env <- new.env(parent = emptyenv())
  utils::data("FAOcountryProfile", package = "FAOSTAT", envir = profile_env)

  if (!rlang::env_has(profile_env, "FAOcountryProfile")) {
    faostat_version <- as.character(utils::packageVersion("FAOSTAT"))
    cli::cli_abort(
      "Dataset {.val FAOcountryProfile} is not available in
       {.pkg FAOSTAT} {faostat_version}."
    )
  }

  profile <- rlang::env_get(profile_env, "FAOcountryProfile") |>
    tibble::as_tibble()

  missing_cols <- setdiff(required_cols, names(profile))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Column{?s} {.field {missing_cols}} missing from
       {.val FAOcountryProfile}."
    )
  }

  profile |>
    dplyr::select(dplyr::all_of(required_cols))
}

.activity_data_choices <- function() {
  c("livestock", "crop_area", "crop_yield", "crop_production")
}

.bad_activity_data_param_error <- function() {
  paste(
    "Please, ensure activity_data is one of",
    '"livestock,crop_area,crop_yield,crop_production."'
  )
}
