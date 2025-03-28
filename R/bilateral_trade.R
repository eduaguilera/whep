get_bilateral_trade <- function(file_path) {
  file_path |>
    readr::read_csv() |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      unit = ifelse(unit == "Head", "heads", unit),
      from_code = ifelse(element == "Export", area_code, area_code_p),
      to_code = ifelse(element == "Export", area_code_p, area_code),
    ) |>
    prefer_flow_direction("Export") |>
    dplyr::select(year, from_code, to_code, item, unit, value)
}

# Keep all rows with preferred direction (Import, Export)
# when both of them exist. Otherwise use the one present.
prefer_flow_direction <- function(bilateral_trade, direction) {
  preferred_direction <- dplyr::filter(bilateral_trade, element == direction)

  bilateral_trade |>
    dplyr::anti_join(
      preferred_direction,
      by = c("from_code", "to_code", "year", "item")
    ) |>
    dplyr::bind_rows(preferred_direction)
}
