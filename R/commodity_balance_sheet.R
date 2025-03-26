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
    dplyr::rename_with(tolower) |>
    dplyr::rename(
      item_to_process = processeditem,
      item_to_process_code = item_code,
      value_to_process = value,
      item_processed = item,
      initial_conversion_factor = product_fraction,
      initial_value_processed = value_proc_raw,
      conversion_factor_scaling = scaling,
      final_conversion_factor = cf,
      final_value_processed = value_proc
    )
}
