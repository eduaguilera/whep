#' Commodity balance sheet data.
#'
#' @description
#' Retrieve supply and use parts for each commodity balance sheet
#' (CBS) item. Stock variations are split into two non-negative
#' columns following the FABIO methodology.
#'
#' @param years Optional integer vector of years to build. When `NULL`
#'   (default) the whole series is built. Supplying a window builds only that
#'   range rather than building 1850-2023 and discarding the rest, and caches it
#'   under a window-specific key. The window is widened internally to 2011 when
#'   it reaches 2013, because that overlap is what splices the old FBS series
#'   onto `FAOSTAT_FBS_New`.
#' @param trade_recovery One of `"none"` (default) or `"net_import"`, passed
#'   to [build_commodity_balances()], which documents what each does and what
#'   `"net_import"` moves. Each method is built and cached under its own slot,
#'   so asking for one never serves the other's result. `"net_import"` is not
#'   the default because two allocation questions it raises are still open
#'   (whep#762).
#' @param example If `TRUE`, return a small example output without
#'   downloading remote data. Default is `FALSE`. The example is the same
#'   fixture under either `trade_recovery`.
#'
#' @returns
#' A tibble with the commodity balance sheet data in wide format.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from.
#'    For code details see e.g. `add_area_name()`.
#' - `item_cbs_code`: FAOSTAT internal code for each item. For
#'   code details see e.g. `add_item_cbs_name()`.
#' - `unit`: The denomination of every quantity in the row. `"tonnes"` for
#'   the rows of the FAO-style balance sheet and `"heads"` (number of
#'   animals) for the live-animal rows added by the livestock balance. Set by
#'   the builder that produced the row, so a derived row carries the same
#'   unit as a reported one. Never sum quantities across rows of different
#'   units.
#'
#' The other columns are quantities where total supply and total
#' use should be balanced, in the row's `unit`.
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
#'    - `processing_primary`: Used to obtain other subproducts, for the
#'      handful of primary items (palm fruit, hops, seed cotton,
#'      coconuts, hemp, kapok fruit, linum) whose entire domestic supply
#'      is destined for processing. Zero for every other item.
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
get_wide_cbs <- function(
  years = NULL,
  trade_recovery = c("none", "net_import"),
  example = FALSE
) {
  trade_recovery <- rlang::arg_match(trade_recovery)
  if (example) {
    return(.example_get_wide_cbs())
  }
  build_years <- .build_years(years)
  cbs_built <- .cached_cbs_built(build_years, trade_recovery)
  primary_prod <- .cached_primary_prod(.context_years(build_years))

  .cache_get(
    .cache_key("cbs_wide", build_years, .cbs_cache_method(trade_recovery)),
    .cbs_long_to_wide(cbs_built, primary_prod, build_years)
  )
}

#' Livestock commodity balance sheet entries
#'
#' @description
#' Build CBS rows for live animals from primary production data
#' and bilateral trade. Live animals are not included in the FAO
#' commodity balance sheet but are needed as explicit intermediates
#' in the IO model.
#'
#' Following the FABIO methodology, live-animal production is estimated
#' from slaughter counts as `slaughtered + exported - imported`
#' (animals raised in the country), and domestic supply (`processing`)
#' equals `production + import - export`. Only live animals with explicit
#' slaughter-product outputs are added; other animal products are supplied
#' directly by husbandry.
#'
#' Units are heads (number of animals).
#'
#' @param primary_prod Tibble from [get_primary_production()].
#' @param method_head_units How the live-animal trade this balance rests
#'   on treats FAOSTAT's `1000 Head` rows. Passed to
#'   [build_detailed_trade()]'s helper of the same name; see its *Live
#'   animals are reported in two head units* section. `"convert"`
#'   (default) rescales them by 1,000 onto `heads`, `"drop"` discards
#'   them with a warning, `"abort"` refuses.
#'
#' @returns A tibble with the same columns as [get_wide_cbs()].
#'
#' @keywords internal
get_livestock_cbs <- function(
  primary_prod,
  method_head_units = c("convert", "drop", "abort")
) {
  head_method <- rlang::arg_match(method_head_units)
  slaughter_livestock <- .slaughter_livestock_items(primary_prod) |>
    dplyr::rename(item_cbs_code = live_anim_code)

  slaughtered <- primary_prod |>
    dplyr::filter(unit == "slaughtered_heads") |>
    .fold_split_slaughter() |>
    dplyr::inner_join(
      slaughter_livestock,
      dplyr::join_by(item_cbs_code)
    ) |>
    dplyr::summarise(
      slaughtered = sum(value, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    )

  live_trade <- .get_livestock_trade_totals(
    slaughter_livestock$item_cbs_code,
    head_method
  )

  # A left_join here would drop any (year, area_code, item_cbs_code) that
  # trades live animals but has no `slaughtered_heads` row of its own (e.g.
  # a pure importer that never slaughters that species itself) -- its whole
  # trade volume would vanish from the CBS/IO model instead of entering it
  # (whep#168). full_join keeps that key, with `slaughtered` filled to 0.
  live_prod_raw <- dplyr::full_join(
    slaughtered,
    live_trade,
    by = c("year", "area_code", "item_cbs_code")
  )
  .warn_trade_only_livestock(live_prod_raw)

  live_prod <- live_prod_raw |>
    dplyr::mutate(
      slaughtered = tidyr::replace_na(slaughtered, 0),
      import = tidyr::replace_na(import, 0),
      export = tidyr::replace_na(export, 0),
      # FABIO convention: production = animals raised in country
      production = pmax(slaughtered + export - import, 0),
      domestic_supply = production + import - export
    )

  live_prod |>
    dplyr::mutate(
      # Every quantity here is a count of animals: `slaughtered` sums the
      # `slaughtered_heads` rows and the trade totals keep `unit == "heads"`
      # only. Labelled so the wide CBS, which binds these rows onto the
      # tonnes CBS, says which rows are counts (whep#1055).
      unit = "heads",
      food = 0,
      feed = 0,
      seed = 0,
      processing = domestic_supply,
      processing_primary = 0,
      other_uses = 0,
      stock_withdrawal = 0,
      stock_addition = 0
    ) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      unit,
      production,
      import,
      export,
      food,
      feed,
      seed,
      processing,
      processing_primary,
      other_uses,
      stock_withdrawal,
      stock_addition,
      domestic_supply
    )
}

# Put the slaughter FAOSTAT books on a stock sub-item back onto the live
# animal its products and its trade are keyed on (whep#1149).
#
# `.split_slaughter_by_shares()` divides pig slaughter between 1049 "Swine,
# market" and 1051 "Swine, breeding" by stock share, but every pig product
# carries `live_anim_code = 1049` and live-pig trade (FAOSTAT 1034) resolves to
# 1049 too, so the inner_join on `.slaughter_livestock_items()` kept only the
# 1049 half: on a real 2020 build 131,912,454 of 1,319,124,485 slaughtered
# pigs (10.0%) never reached the live-pig balance, while trade entered whole.
#
# Folding is a choice, not an identity. The alternative is a live-animal
# balance of its own for 1051, which needs an `items_cbs` row and so a new
# husbandry sector in `build_supply_use()` and every footprint; the total
# slaughter is the same either way. Folding keeps supply and trade on one key.
# Only swine is folded: the dairy-cattle (960) and layer (1052) shares are
# dropped by the same join, but each of those is an IO sector of its own, so
# where their cull belongs is a separate question.
.fold_split_slaughter <- function(slaughter) {
  folds <- tibble::tribble(
    ~item_cbs_code, ~folded_code,
    1051, 1049
  )
  slaughter |>
    dplyr::left_join(folds, by = "item_cbs_code") |>
    dplyr::mutate(
      item_cbs_code = dplyr::coalesce(.data$folded_code, .data$item_cbs_code)
    ) |>
    dplyr::select(-"folded_code")
}

# Report (year, area_code, item_cbs_code) keys that trade live animals with no
# matching `slaughtered_heads` row, so the full_join filling them to 0 is
# never a silent substitute for real slaughter data (whep#168).
.warn_trade_only_livestock <- function(live_prod_raw) {
  trade_only <- dplyr::filter(live_prod_raw, is.na(slaughtered))
  if (nrow(trade_only) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "!" = "{nrow(trade_only)} live-animal (year, area, item) key{?s} \\
           traded with no {.val slaughtered_heads} row.",
    "i" = "Treating {cli::qty(nrow(trade_only))} th{?is/ese} as 0 slaughter \\
           instead of dropping the trade volume."
  ))
}

.slaughter_product_codes <- function(items_cbs = whep::items_cbs) {
  items_cbs |>
    dplyr::filter(item_type == "slaughter_product") |>
    dplyr::pull(item_cbs_code) |>
    unique()
}

.slaughter_livestock_items <- function(
  primary_prod,
  slaughter_product_codes = .slaughter_product_codes()
) {
  if (length(slaughter_product_codes) == 0L) {
    return(tibble::tibble(live_anim_code = integer()))
  }

  primary_prod |>
    dplyr::filter(
      unit == "tonnes",
      !is.na(live_anim_code),
      item_cbs_code %in% slaughter_product_codes
    ) |>
    dplyr::distinct(live_anim_code = as.integer(live_anim_code))
}

# Extract per-country import and export totals for live animals
# from the raw bilateral trade data.
#
# The head counts this returns are FAOSTAT's, not model output: the
# `bilateral_trade` pin's values match the raw FAOSTAT Detailed Trade Matrix
# exactly. FAOSTAT reports the small species in `1000 Head`, though, and a
# bare filter on `"heads"` dropped every one of those rows -- 89,073 rows and
# 76,141,882 thousand head over 1986-2021, against the 11,707,083,640 head
# that survived -- so live broiler chicken, turkey, duck, goose, rabbit and
# rodent trade left without a word, and `production` below collapsed to
# `slaughtered` alone for exactly the species whose live trade is largest
# (whep#1092, same class as whep#865, which fixed `1000 An` for
# `faostat-trade-totals`; surfaced by #1054).
#
# `.normalise_trade_units()` is what now makes the `unit == "heads"` filter
# below cover the whole live-animal record. The pin
# `20250714T123347Z-2c392` still carries `tonnes` and `Head` only, because
# its producer applied the same filter, so this changes no published number
# until that pin is rebuilt from `build_detailed_trade()`; it is the filter,
# not the pin, that has to stop dropping them first.
.get_livestock_trade_totals <- function(
  livestock_items,
  method_head_units = "convert"
) {
  btd <- tryCatch(
    "bilateral_trade" |>
      whep_read_file() |>
      .clean_bilateral_trade() |>
      .normalise_trade_units(method_head_units) |>
      dplyr::filter(
        unit == "heads",
        item_cbs_code %in% livestock_items
      ) |>
      .map_livestock_trade_polities(),
    error = function(e) {
      # A refused unit is a deliberate stop, not a failed read: let it out
      # instead of degrading `method_head_units = "abort"` into a warning.
      if (inherits(e, "whep_unhandled_trade_unit")) {
        rlang::cnd_signal(e)
      }
      cli::cli_warn(
        "Could not read bilateral trade for livestock: {e$message}"
      )
      NULL
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

# Map the raw FAOSTAT reporter/partner codes of the bilateral trade data
# onto the same year-aware polity area-code space used by the polity-coded
# slaughter counts (see `.aggregate_to_polities()` in the production build),
# so live-animal trade and slaughter reconcile on a consistent join key.
# Rows whose reporter or partner code does not map to a polity are dropped,
# mirroring the slaughter side, and codes are re-aggregated at polity level.
.map_livestock_trade_polities <- function(btd) {
  dt <- data.table::as.data.table(btd)

  dt <- dt |>
    .add_polity_columns_dt(
      code_col = "from_code",
      year_col = "year",
      prefix = "from_",
      include_unmapped = FALSE
    ) |>
    .add_polity_columns_dt(
      code_col = "to_code",
      year_col = "year",
      prefix = "to_",
      include_unmapped = FALSE
    )

  dt <- dt[!is.na(from_polity_code) & !is.na(to_polity_code)]
  dt[, from_code := from_polity_area_code]
  dt[, to_code := to_polity_area_code]

  dt[,
    .(value = sum(value, na.rm = TRUE)),
    by = c("year", "from_code", "to_code", "item_cbs_code", "unit")
  ] |>
    tibble::as_tibble()
}

#' Processed products share factors
#'
#' @description
#' Reports quantities of commodity balance sheet items used for `processing`
#' and quantities of their corresponding processed output items.
#'
#' @param years Optional integer vector of years to build. When `NULL`
#'   (default) the whole series is built. Supplying a window builds only that
#'   range rather than building 1850-2023 and discarding the rest, and caches it
#'   under a window-specific key.
#' @param trade_recovery One of `"none"` (default) or `"net_import"`, selecting
#'   the CBS the coefficients are calibrated on. See
#'   [build_commodity_balances()] and [get_wide_cbs()]. Pass the same value
#'   here as to [get_wide_cbs()]: coefficients calibrated on one CBS do not
#'   describe the other.
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
get_processing_coefs <- function(
  years = NULL,
  trade_recovery = c("none", "net_import"),
  example = FALSE
) {
  trade_recovery <- rlang::arg_match(trade_recovery)
  if (example) {
    return(.example_get_processing_coefs())
  }
  build_years <- .build_years(years)
  cbs_built <- .cached_cbs_built(build_years, trade_recovery)
  method <- .cbs_cache_method(trade_recovery)

  .cache_get(.cache_key("proc_coefs", build_years, method), {
    cli::cli_h1("Building processing coefficients")
    .build_proc_coefs_years(cbs_built, build_years)
  })
}

# Pivot long-format CBS to wide and split stock_variation into
# stock_addition (positive) and stock_withdrawal (negative). Guards two
# element-vocabulary failure modes (#219): a `(year, area_code,
# item_cbs_code, element)` key that repeats would otherwise silently become
# a list-column under pivot_wider(), and an input with no `stock_variation`
# row at all would otherwise crash the mutate() below with "object
# 'stock_variation' not found".
.pivot_cbs_wide <- function(cbs_long) {
  selected <- cbs_long |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      element,
      value
    )

  dup_keys <- selected |>
    dplyr::count(year, area_code, item_cbs_code, element) |>
    dplyr::filter(n > 1)

  if (nrow(dup_keys) > 0) {
    cli::cli_abort(c(
      "{.fn .pivot_cbs_wide} found {nrow(dup_keys)} duplicate
       {.val year}/{.val area_code}/{.val item_cbs_code}/{.val element}
       key{?s} in {.arg cbs_long}.",
      "i" = "Each combination must have exactly one {.arg value}."
    ))
  }

  selected |>
    tidyr::pivot_wider(
      names_from = element,
      values_from = value,
      values_fill = 0
    ) |>
    ensure_columns(
      tibble::tibble(stock_variation = double()),
      defaults = list(stock_variation = 0)
    ) |>
    dplyr::mutate(
      stock_addition = dplyr::if_else(
        stock_variation > 0,
        stock_variation,
        0
      ),
      stock_withdrawal = dplyr::if_else(
        stock_variation < 0,
        -stock_variation,
        0
      )
    ) |>
    dplyr::select(-stock_variation)
}
