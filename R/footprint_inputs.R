#' Get land footprint data for local production.
#'
#' @description
#' Read and clean the `land_fp` input file, returning only land-use
#' entries from local production (`Impact == "Land"` and
#' `Origin == "Production"`).
#'
#' @param example If `TRUE`, return a small example output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble with columns:
#' - `year`: Year.
#' - `area_code`: Numeric area code.
#' - `item_cbs_code`: Commodity balance sheet item code.
#' - `impact`: Impact category.
#' - `element`: Source element.
#' - `origin`: Origin type.
#' - `group`: Group category.
#' - `impact_u`: Land footprint value (TODO: find which unit).
#'
#' @export
#'
#' @examples
#' get_land_fp_production(example = TRUE)
get_land_fp_production <- function(example = FALSE) {
  if (example) {
    return(.example_land_fp_production())
  }

  whep_read_file("land_fp") |>
    dplyr::filter(Impact == "Land", Origin == "Production") |>
    add_area_code(name_column = "area", code_column = "code") |>
    dplyr::rename_with(tolower) |>
    dplyr::transmute(
      year = as.integer(year),
      area_code = as.integer(code),
      item_cbs_code = as.integer(item_code),
      impact,
      element,
      origin,
      group,
      impact_u
    )
}

.example_land_fp_production <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact, ~element, ~origin, ~group, ~impact_u,
    2020L, 32L, 2511L, "Land", "Cropland", "Production", "Crops", 1000,
    2020L, 76L, 2514L, "Land", "Cropland", "Production", "Crops", 800
  )
}
