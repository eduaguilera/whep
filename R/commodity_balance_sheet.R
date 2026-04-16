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
#' The other columns are quantities where total supply and total
#' use should be balanced. Units are tonnes for most items,
#' and heads for live animals (see [items_cbs] `item_type`).
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

  primary_prod <- .cache_get("primary_prod", build_primary_production())

  cbs_built <- .cache_get("cbs_built", {
    cli::cli_h1("Building commodity balance sheets")
    build_commodity_balances(primary_prod)
  })

  .cache_get("cbs_wide", {
    cli::cli_progress_step("Adding livestock CBS rows")
    cbs <- .pivot_cbs_wide(cbs_built)
    livestock_cbs <- get_livestock_cbs(primary_prod)
    dplyr::bind_rows(cbs, livestock_cbs)
  })
}

#' Livestock commodity balance sheet entries
#'
#' @description
#' Build CBS rows for live animals from primary production data
#' and bilateral trade. Live animals are not included in the FAO
#' commodity balance sheet but are needed as explicit intermediates
#' in the IO model.
#'
#' Following the FABIO methodology, production is estimated as
#' `slaughtered + exported - imported` (animals raised in the
#' country), and domestic supply (`processing` for meat animals,
#' `other_uses` for draft animals) equals
#' `production + import - export`.
#'
#' Units are heads (number of animals).
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

  slaughtered <- primary_prod |>
    dplyr::inner_join(
      all_livestock,
      dplyr::join_by(item_cbs_code)
    ) |>
    dplyr::filter(
      (is_meat & unit == "slaughtered_heads") |
        (!is_meat & unit == "heads")
    ) |>
    dplyr::summarise(
      slaughtered = sum(value, na.rm = TRUE),
      is_meat = dplyr::first(is_meat),
      .by = c(year, area_code, item_cbs_code)
    )

  live_trade <- .get_livestock_trade_totals(all_livestock$item_cbs_code)

  live_prod <- slaughtered |>
    dplyr::left_join(
      live_trade,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      import = tidyr::replace_na(import, 0),
      export = tidyr::replace_na(export, 0),
      # FABIO convention: production = animals raised in country
      production = pmax(slaughtered + export - import, 0),
      domestic_supply = production + import - export
    )

  dplyr::bind_rows(
    live_prod |>
      dplyr::filter(is_meat) |>
      dplyr::mutate(
        food = 0,
        feed = 0,
        seed = 0,
        processing = domestic_supply,
        other_uses = 0,
        stock_withdrawal = 0,
        stock_addition = 0
      ),
    live_prod |>
      dplyr::filter(!is_meat) |>
      dplyr::mutate(
        food = 0,
        feed = 0,
        seed = 0,
        processing = 0,
        other_uses = domestic_supply,
        stock_withdrawal = 0,
        stock_addition = 0
      )
  ) |>
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

# Extract per-country import and export totals for live animals
# from the raw bilateral trade data.
.get_livestock_trade_totals <- function(livestock_items) {
  btd <- tryCatch(
    "bilateral_trade" |>
      whep_read_file() |>
      .clean_bilateral_trade() |>
      dplyr::filter(
        unit == "heads",
        item_cbs_code %in% livestock_items
      ),
    error = function(e) {
      cli::cli_warn(
        "Could not read bilateral trade for livestock: {e$message}"
      )
      return(NULL)
    }
  )

  if (is.null(btd) || nrow(btd) == 0) {
    return(tibble::tibble(
      year = integer(),
      area_code = integer(),
      item_cbs_code = integer(),
      import = numeric(),
      export = numeric()
    ))
  }

  imports <- btd |>
    dplyr::summarise(
      import = sum(value, na.rm = TRUE),
      .by = c(year, to_code, item_cbs_code)
    ) |>
    dplyr::rename(area_code = to_code)

  exports <- btd |>
    dplyr::summarise(
      export = sum(value, na.rm = TRUE),
      .by = c(year, from_code, item_cbs_code)
    ) |>
    dplyr::rename(area_code = from_code)

  dplyr::full_join(
    imports,
    exports,
    by = c("year", "area_code", "item_cbs_code")
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
  primary_prod <- .cache_get("primary_prod", build_primary_production())

  cbs_built <- .cache_get("cbs_built", {
    cli::cli_h1("Building commodity balance sheets")
    build_commodity_balances(primary_prod)
  })

  .cache_get("proc_coefs", {
    cli::cli_h1("Building processing coefficients")
    build_processing_coefs(cbs_built)
  })
}

# Pivot long-format CBS to wide and split stock_variation into
# stock_withdrawal (positive) and stock_addition (negative).
.pivot_cbs_wide <- function(cbs_long) {
  cbs_long |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      element,
      value
    ) |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value,
      values_fill = 0
    ) |>
    dplyr::mutate(
      stock_withdrawal = dplyr::if_else(
        stock_variation > 0,
        stock_variation,
        0
      ),
      stock_addition = dplyr::if_else(
        stock_variation < 0,
        -stock_variation,
        0
      )
    ) |>
    dplyr::select(-stock_variation)
}
