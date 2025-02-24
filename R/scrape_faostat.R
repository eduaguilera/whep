#' Populates ISO3CODE based on "area" column from FAOSTAT
#' also postprocesses "wrong" ISO3 codes
#'
#' @param df data.frame from FAOSTAT
#'
#' @returns data.frame
.populate_iso3_code <- function(df) {
  # create new column "ISO3_CODE" and fill it
  df <- FAOSTAT::fillCountryCode(
    country = "area", data = df, outCode = "ISO3_CODE"
  )

  # manually fix some crazy countries/ISO3_CODE
  df[df$area == "China, mainland", "ISO3_CODE"] <- "CHN"
  df[df$area == "T\u00FCrkiye", "ISO3_CODE"] <- "TUR"
  df[df$area == "Netherlands (Kingdom of the)", "ISO3_CODE"] <- "NLD"
  df[df$area == "Sudan", "ISO3_CODE"] <- "SDN"
  df[df$area == "South Sudan", "ISO3_CODE"] <- "SSD"
  df[df$area == "Czechia", "ISO3_CODE"] <- "CZE"
  df[df$area == "Lao People's Democratic Republic", "ISO3_CODE"] <- "LAO"
  df
}

#' Converts activity_data_param on the necessary FAOSTAT code
#' (to scrape from FAOSTAT) and the necessary FAO parameter
#'
#' @note to add new parameters from FAOSTAT IS HERE
#' @param activity_data_param activity data required from FAOSTAT;
#' needs to be one of c('livestock','crop_area','crop_yield','crop_production')
#'
#' @returns list of length n=2; first index is FAOSTAT code and second index
#' is FAOSTAT parameter
.faostat_converter <- function(
    activity_data_param = c(
      "livestock", "crop_area", "crop_yield", "crop_production"
    )) {
  if (activity_data_param %in% c(
    "livestock", "crop_area", "crop_yield", "crop_production"
  )) {
    # create list to translate activity_data_param into FAOSTAT code
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

    return(list(
      FAOSTAT_code = fao_cat_converter[[activity_data_param]],
      FAOSTAT_param = fao_param_converter[[activity_data_param]]
    ))
  } else {
    stop(paste(
      "Please, ensure activity_data_param is one of",
      '"livestock,crop_area,crop_yield,crop_production."'
    ))
  }
}

#' Scrapes activity_data_param from FAOSTAT and slightly post-processes it.
#' Important: Dynamically allows for the introduction of subsets as "...".
#' Note: overhead by individually scraping FAOSTAT code QCL for crop data;
#' it's fine.
#'
#' @param activity_data_param activity data required from FAOSTAT; needs
#' to be one of c('livestock','crop_area','crop_yield','crop_production')
#' @param ... can be whichever column name from get_faostat_bulk,
#' particularly year, area or ISO3_CODE
#'
#' @returns data.frame of FAOSTAT for activity_data_param; default is for
#' all years and countries
#'
#' @export
#'
#' @examples
#' get_faostat_data("livestock")
#' get_faostat_data("livestock", year == 2010)
#' get_faostat_data("livestock", year == 2010 & area == "Portugal")
#' get_faostat_data("livestock", area == "Portugal")
get_faostat_data <- function(
    activity_data_param = c(
      "livestock", "crop_area", "crop_yield", "crop_production"
    ),
    ...) {
  # Some functions from FAOSTAT pkg don't work by only using prefixed functions.
  # It is detached again at the end of this function call.
  library("FAOSTAT")

  faostat_converters <- .faostat_converter(activity_data_param)

  # scrape bulk data from FAOSTAT for a specific parameter
  faostat_data <- FAOSTAT::get_faostat_bulk(
    code = faostat_converters[["FAOSTAT_code"]]
  )

  # subset based on activity_data_param OR element in FAOSTAT
  # also subset only necessary columns for post-processing
  faostat_data <- subset(
    faostat_data, element == faostat_converters[["FAOSTAT_param"]]
  )

  faostat_data <- subset(
    faostat_data,
    select = c("area", "item", "element", "year", "value", "unit")
  )

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

  # Properly detach FAOSTAT to avoid issues
  detach("package:FAOSTAT", unload = TRUE)

  return(faostat_data)
}
