get_wide_cbs <- function(file_path) {
  file_path |>
    readr::read_csv() |>
    tidyr::pivot_wider(names_from = Element, values_from = Value) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(stock_retrieval = -stock_variation, .keep = "unused")
}

get_processing_coefs <- function(file_path) {
  file_path |>
    readr::read_csv() |>
    dplyr::select(-Item, -Element) |>
    dplyr::rename_with(tolower)
}
