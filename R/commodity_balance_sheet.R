#' Commodity balance sheet data.
#'
#' @description
#' Retrieve supply and use parts for each commodity balance sheet
#' (CBS) item. Stock variations are split into two non-negative
#' columns following the FABIO methodology.
#'
#' @param example If `TRUE`, return a small example output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @returns
#' A tibble with the commodity balance sheet data in wide format.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from.
#'    For code details see e.g. `add_area_name()`.
#' - `item_cbs_code`: FAOSTAT internal code for each item. For
#'   code details see e.g. `add_item_cbs_name()`.
#'
#' The other columns are quantities (measured in tonnes), where
#' total supply and total use should be balanced.
#'
#' For supply:
#'    - `production`: Produced locally.
#'    - `import`: Obtained from importing from other countries.
#'    - `stock_withdrawal`: Biomass taken out of storage
#'      (non-negative). Positive when stocks decrease.
#'
#' For use:
#'    - `food`: Food for humans.
#'    - `feed`: Food for animals.
#'    - `export`: Released as export for other countries.
#'    - `seed`: Intended for new production.
#'    - `processing`: Used to obtain other subproducts.
#'    - `other_uses`: Any other use not included above.
#'    - `stock_addition`: Biomass placed into storage
#'      (non-negative). Positive when stocks increase.
#'
#' There is an additional column `domestic_supply` which is
#' computed as total use excluding `export`.
#'
#' @export
#'
#' @examples
#' get_wide_cbs(example = TRUE)
get_wide_cbs <- function(example = FALSE) {
  if (example) {
    return(.example_get_wide_cbs())
  }

  fao_cbs <- "commodity_balance_sheet" |>
    whep_read_file() |>
    tidyr::pivot_wider(
      names_from = Element,
      values_from = Value,
      values_fill = 0
    ) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(
      stock_withdrawal = pmax(-stock_variation, 0),
      stock_addition = pmax(stock_variation, 0),
      dplyr::across(c(year, area_code), as.integer),
      .keep = "unused"
    ) |>
    dplyr::select(-area, -item) |>
    dplyr::rename(item_cbs_code = item_code)

  livestock_cbs <- get_livestock_cbs(get_primary_production())

  dplyr::bind_rows(fao_cbs, livestock_cbs)
}

#' Livestock commodity balance sheet entries
#'
#' @description
#' Build CBS rows for live animals from primary production data.
#' Live animals are not included in the FAO commodity balance sheet
#' but are needed as explicit intermediates in the IO model.
#'
#' For meat animals (`livestock_meat` in `items_cbs`), all production
#' flows to `processing` (i.e. enters the slaughtering process).
#' For draft animals (`livestock_draft` in `items_cbs`), all
#' production flows to `other_uses`.
#'
#' Production is estimated from Livestock Unit (LU) data using the
#' standard conversion factor.
#'
#' @param primary_prod Tibble from [get_primary_production()].
#'
#' @returns A tibble with the same columns as [get_wide_cbs()].
#'
#' @keywords internal
get_livestock_cbs <- function(primary_prod) {
  meat_items <- whep::items_cbs |>
    dplyr::filter(item_type == "livestock_meat") |>
    dplyr::select(item_cbs_code)

  draft_items <- whep::items_cbs |>
    dplyr::filter(item_type == "livestock_draft") |>
    dplyr::select(item_cbs_code)

  all_livestock <- dplyr::bind_rows(
    dplyr::mutate(meat_items, is_meat = TRUE),
    dplyr::mutate(draft_items, is_meat = FALSE)
  )

  live_prod <- primary_prod |>
    dplyr::filter(unit == "LU") |>
    dplyr::inner_join(
      all_livestock,
      dplyr::join_by(item_cbs_code)
    ) |>
    dplyr::mutate(value = k_tonnes_per_livestock_unit * value) |>
    dplyr::summarise(
      production = sum(value, na.rm = TRUE),
      is_meat = dplyr::first(is_meat),
      .by = c(year, area_code, item_cbs_code)
    )

  dplyr::bind_rows(
    live_prod |>
      dplyr::filter(is_meat) |>
      dplyr::mutate(
        import = 0,
        export = 0,
        food = 0,
        feed = 0,
        seed = 0,
        processing = .data$production,
        other_uses = 0,
        stock_withdrawal = 0,
        stock_addition = 0,
        domestic_supply = .data$production
      ),
    live_prod |>
      dplyr::filter(!is_meat) |>
      dplyr::mutate(
        import = 0,
        export = 0,
        food = 0,
        feed = 0,
        seed = 0,
        processing = 0,
        other_uses = .data$production,
        stock_withdrawal = 0,
        stock_addition = 0,
        domestic_supply = .data$production
      )
  ) |>
    dplyr::select(-is_meat) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      production,
      import,
      export,
      food,
      feed,
      seed,
      processing,
      other_uses,
      stock_withdrawal,
      stock_addition,
      domestic_supply
    )
}

#' Processed products share factors
#'
#' @description
#' Reports quantities of commodity balance sheet items used for `processing`
#' and quantities of their corresponding processed output items.
#'
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
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
#' get_processing_coefs(example = TRUE)
get_processing_coefs <- function(example = FALSE) {
  if (example) {
    return(.example_get_processing_coefs())
  }

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
