#' Commodity balance sheet data
#'
#' @description
#' States supply and use parts for each commodity balance sheet (CBS) item.
#'
#' @returns
#' A tibble with the commodity balance sheet data in wide format.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `item_cbs_code`: FAOSTAT internal code for each item. For code details
#'   see e.g. `add_item_cbs_name()`.
#'
#' The other columns are quantities (measured in tonnes), where total supply
#' and total use should be balanced.
#'
#' For supply:
#'    - `production`: Produced locally.
#'    - `import`: Obtained from importing from other countries.
#'    - `stock_retrieval`: Available as net stock from previous years. For ease,
#'      only one stock column is included here as supply. If the value is
#'      positive, there is a stock quantity available as supply. Otherwise, it
#'      means a larger quantity was stored for later years and cannot be used as
#'      supply, having to deduce it from total supply. Since in this case it is
#'      negative, the total supply is still computed as the sum of all of these.
#'
#' For use:
#'    - `food`: Food for humans.
#'    - `feed`: Food for animals.
#'    - `export`: Released as export for other countries.
#'    - `seed`: Intended for new production.
#'    - `processing`: The product will be used to obtain other subproducts.
#'    - `other_uses`: Any other use not included in the above ones.
#'
#' There is an additional column `domestic_supply` which is computed as the
#' total use excluding `export`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_wide_cbs()
#' }
get_wide_cbs <- function() {
  "commodity_balance_sheet" |>
    whep_read_file() |>
    tidyr::pivot_wider(names_from = Element, values_from = Value) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      stock_retrieval = -stock_variation,
      dplyr::across(c(year, area_code), as.integer),
      .keep = "unused"
    ) |>
    dplyr::select(-area, -item) |>
    dplyr::rename(item_cbs_code = item_code)
}

#' Processed products share factors
#'
#' @description
#' Reports quantities of commodity balance sheet items used for `processing`
#' and quantities of their corresponding processed output items.
#'
#' @returns
#' A tibble with the quantities for each processed product.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `item_cbs_code_to_process`: FAOSTAT internal code for each one of the
#'    items that are being processed and will give other subproduct items.
#'    For code details see e.g. `add_item_cbs_name()`.
#' - `value_to_process`: tonnes of this item that are being processed. It
#'    matches the amount found in the `processing` column from the data
#'    obtained by `get_wide_cbs()`.
#' - `item_cbs_code_processed`: FAOSTAT internal code for each one of the
#'    subproduct items that are obtained when processing. For code details
#'    see e.g. `add_item_cbs_name()`.
#' - `initial_conversion_factor`: estimate for the number of tonnes of
#'    `item_cbs_code_processed` obtained for each tonne of
#'    `item_cbs_code_to_process`. It will be used to compute the
#'    `final_conversion_factor`, which leaves everything balanced.
#'    TODO: explain how it's computed.
#' - `initial_value_processed`: first estimate for the number of tonnes of
#'    `item_cbs_code_processed` obtained from `item_cbs_code_to_process`. It
#'    is computed as `value_to_process * initial_conversion_factor`.
#' - `conversion_factor_scaling`: computed scaling needed to adapt
#'    `initial_conversion_factor` so as to get a final balanced total of
#'    subproduct quantities. TODO: explain how it's computed.
#' - `final_conversion_factor`: final used estimate for the number of tonnes of
#'    `item_cbs_code_processed` obtained for each tonne of
#'    `item_cbs_code_to_process`. It is computed as
#'    `initial_conversion_factor * conversion_factor_scaling`.
#' - `final_value_processed`: final estimate for the number of tonnes of
#'    `item_cbs_code_processed` obtained from `item_cbs_code_to_process`. It
#'    is computed as `initial_value_processed * final_conversion_factor`.
#'
#' For the final data obtained, the quantities `final_value_processed` are
#' balanced in the following sense: the total sum of `final_value_processed`
#' for each unique tuple of `(year, area_code, item_cbs_code_processed)`
#' should be exactly the quantity reported for that year, country and
#' `item_cbs_code_processed` item in the `production` column obtained from
#' `get_wide_cbs()`. This is because they are not primary products, so the
#' amount from 'production' is actually the amount of subproduct obtained.
#' TODO: Fix few data where this doesn't hold.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_processing_coefs()
#' }
get_processing_coefs <- function() {
  "processing_coefs" |>
    whep_read_file() |>
    dplyr::select(-Item, -Element) |>
    dplyr::rename_with(tolower) |>
    add_item_cbs_code(name_column = "item") |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code_to_process = item_code,
      value_to_process = value,
      item_cbs_code_processed = item_cbs_code,
      initial_conversion_factor = product_fraction,
      initial_value_processed = value_proc_raw,
      conversion_factor_scaling = scaling,
      final_conversion_factor = cf,
      final_value_processed = value_proc
    )
}
