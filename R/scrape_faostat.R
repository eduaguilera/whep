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
#'   for all years and countries. `ISO3_CODE` is resolved from the `area_iso3c`
#'   column of [polity_area_crosswalk] and is `NA` for FAOSTAT's regional and
#'   multi-territory aggregates, including the `"China"` aggregate (area 351),
#'   which by design has no ISO3 code of its own.
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
#'
#' @param df data.frame from FAOSTAT
#'
#' @noRd
#'
#' @returns data.frame
.populate_iso3_code <- function(df) {
  df[["ISO3_CODE"]] <- .match_fao_area_to_iso3(df[["area"]])

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

# Builds the FAOSTAT area name -> ISO3 code lookup from
# [polity_area_crosswalk], which is the table the rest of the package already
# bridges ISO3 on (`build_primary_production()`, `build_commodity_balances()`,
# `arable_permanent_land.R`). It replaced FAOSTAT's vendored
# `FAOcountryProfile` name table, which is stale relative to the labels
# FAOSTAT publishes today and left eight reporters unresolved even with a
# hand-maintained fix block in whep (#541); the crosswalk's `area_iso3c` is
# maintained upstream in whep-polities, so territorial attributions are
# inherited rather than hardcoded here (#458).
#
# Rows with no `area_code` are dependencies and aggregate buckets that borrow a
# parent area's name — Guernsey, Jersey and the Isle of Man all sit under
# "United Kingdom" — so keying on the name alone would be ambiguous. Dropping
# them leaves exactly one row per FAOSTAT area name. Areas the crosswalk gives
# no `area_iso3c`, notably the "China" aggregate 351 (#158, #313), stay `NA`.
.fao_area_iso3_lookup <- function() {
  whep::polity_area_crosswalk |>
    dplyr::filter(!is.na(area_code)) |>
    dplyr::distinct(
      fao_area_name = area_name,
      iso3_code = area_iso3c
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
