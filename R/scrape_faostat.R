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
  # Some functions from FAOSTAT pkg don't work by only using prefixed functions.
  # It is detached again at the end of this function call.
  # Also this is another way to write require("FAOSTAT") without triggering
  # R CMD check warning
  do.call(require, list("FAOSTAT"))

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

  # Properly detach FAOSTAT to avoid issues
  detach("package:FAOSTAT", unload = TRUE)

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
  df <- FAOSTAT::fillCountryCode(
    country = "area",
    data = df,
    outCode = "ISO3_CODE"
  )

  .correct_iso3_from_polities(df)
}

# Correct `FAOSTAT::fillCountryCode()`'s answer against the polities crosswalk.
#
# This replaced seven hardcoded patches introduced as "manually fix some crazy
# countries/ISO3_CODE": China mainland -> CHN, T\u00FCrkiye -> TUR, Netherlands (Kingdom of the)
# -> NLD, Sudan -> SDN, South Sudan -> SSD, Czechia -> CZE, and Lao People's Democratic
# Republic -> LAO. Every one of the seven agrees with `area_iso3c` in the crosswalk, so the
# list was a hand-maintained copy of something already published -- and a copy that only
# covers the seven names somebody hit. The next awkward rename lands as a silent NA.
#
# The crosswalk is the right authority here rather than a second opinion: this function's
# input is FAOSTAT area NAMES, and `area_iso3c` is upstream's own statement of the ISO3 for a
# FAOSTAT area. So it corrects wherever the two disagree, not merely where the code is
# missing, which is what the seven patches were doing.
#
# Restricted to rows with a non-NA `area_code`, i.e. actual reporting areas, and that
# restriction is what makes the lookup safe. Unrestricted, three names are ambiguous --
# "France" maps to FRA and BLM, "United Kingdom" to GGY, JEY and IMN, "Finland" to FIN and
# ALA -- because the crosswalk also carries dependencies that have a polity and an ISO3 but no
# FAOSTAT area of their own (whep#407). Filtered to reporting areas, all 265 names are unique.
#
# Names the crosswalk does not know are left exactly as `fillCountryCode()` returned them:
# this corrects what it can prove and does not guess.
.correct_iso3_from_polities <- function(df) {
  if (!all(c("area", "ISO3_CODE") %in% names(df))) {
    return(df)
  }
  cw <- as.data.frame(whep::polity_area_crosswalk)
  keep <- which(
    !is.na(cw$area_name) & !is.na(cw$area_iso3c) & !is.na(cw$area_code)
  )
  lookup <- unique(cw[keep, c("area_name", "area_iso3c")])

  authoritative <- lookup$area_iso3c[match(
    as.character(df$area),
    lookup$area_name
  )]
  current <- as.character(df$ISO3_CODE)
  differs <- !is.na(authoritative) &
    (is.na(current) | current != authoritative)

  if (any(differs)) {
    shown <- unique(paste0(
      as.character(df$area)[differs],
      ": ",
      ifelse(is.na(current[differs]), "NA", current[differs]),
      " -> ",
      authoritative[differs]
    ))
    cli::cli_inform(c(
      "i" = "Corrected {sum(differs)} FAOSTAT ISO3 code{?s} against the polities
         crosswalk.",
      "*" = "{.val {utils::head(sort(shown), 10)}}"
    ))
    df$ISO3_CODE[differs] <- authoritative[differs]
  }

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

.activity_data_choices <- function() {
  c("livestock", "crop_area", "crop_yield", "crop_production")
}

.bad_activity_data_param_error <- function() {
  paste(
    "Please, ensure activity_data is one of",
    '"livestock,crop_area,crop_yield,crop_production."'
  )
}
