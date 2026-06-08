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
    .add_land_fp_area_code(name_column = "area", code_column = "code") |>
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

.add_land_fp_area_code <- function(
  data,
  name_column = "area",
  code_column = "code"
) {
  bridge <- .land_fp_area_code_bridge(name_column, code_column)

  data |>
    dplyr::left_join(bridge, by = name_column)
}

.land_fp_area_code_bridge <- function(
  name_column = "area",
  code_column = "code"
) {
  alias_cols <- intersect(
    c("polity_name", "FAOSTAT_name", "name"),
    names(whep::regions_full)
  )
  polity_codes <- whep::polities |>
    dplyr::transmute(
      polity_code = .data$iso3c,
      !!code_column := as.integer(.data$area_code)
    )

  whep::regions_full |>
    dplyr::select(
      "polity_code",
      dplyr::all_of(alias_cols)
    ) |>
    dplyr::filter(!is.na(.data$polity_code)) |>
    dplyr::left_join(polity_codes, by = "polity_code") |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(alias_cols),
      values_to = name_column,
      names_to = "source"
    ) |>
    dplyr::filter(
      !is.na(.data[[name_column]]),
      nzchar(.data[[name_column]]),
      !is.na(.data[[code_column]])
    ) |>
    dplyr::mutate(
      !!code_column := as.integer(.data[[code_column]])
    ) |>
    dplyr::distinct(.data[[name_column]], .data[[code_column]]) |>
    dplyr::add_count(.data[[name_column]], name = "n_codes") |>
    dplyr::filter(.data$n_codes == 1L) |>
    dplyr::select(-"n_codes")
}
