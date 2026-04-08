# Price computation pipeline migrated from Global/R/prices.r.
#
# Three public functions build successively richer price datasets:
#   build_trade_prices()   — global prices from FAOSTAT trade data
#   build_primary_prices() — primary-item prices (export + production)
#   build_cbs_prices()     — CBS-item prices incl. residues & estimates

# -- Trade prices --------------------------------------------------------------

#' Build global trade prices
#'
#' @description
#' Compute global prices of traded items from FAOSTAT trade data.
#' For each item and element (import/export), the price is
#' `KDollars / tonnes` aggregated across all countries.
#'
#' @param raw_trade A data.table or tibble of FAOSTAT bilateral trade
#'   data with columns `year`, `item_trade`, `item_code_trade`,
#'   `unit`, `element`, and `value`. Must include both quantity
#'   (`"tonnes"`) and value (`"1000 US$"`) rows. If `NULL` (default),
#'   the data is read from the `"faostat-trade-bilateral"` pin.
#' @param example Logical. If `TRUE`, return a small example tibble.
#'   Default `FALSE`.
#'
#' @returns A tibble with columns:
#'   - `year`: Integer year.
#'   - `item_trade`: Trade item name.
#'   - `item_code_trade`: Numeric FAOSTAT trade item code.
#'   - `element`: `"import"` or `"export"`.
#'   - `kdollars`: Total trade value in thousand US dollars.
#'   - `tonnes`: Total trade quantity in tonnes.
#'   - `price`: Price in KDollars per tonne.
#'
#' @export
#'
#' @examples
#' build_trade_prices(example = TRUE)
build_trade_prices <- function(raw_trade = NULL, example = FALSE) {
  if (example) {
    return(.example_build_trade_prices())
  }
  cli::cli_h1("Building trade prices")
  tibble::as_tibble(.compute_trade_prices(raw_trade))
}

# -- Primary prices ------------------------------------------------------------

#' Build primary item prices
#'
#' @description
#' Compute prices for primary production items. Export trade prices are
#' preferred; when unavailable, production value prices (gross
#' production value divided by quantity) are used as fallback. Gaps are
#' filled via linear interpolation.
#'
#' @param primary_prod A tibble of primary production, as returned by
#'   [build_primary_production()] or [get_primary_production()].
#' @param value_of_production A data frame with FAOSTAT Value of
#'   Production data. Must contain columns `Item.Code` (or
#'   `item_code_prod`), `Element`, `Unit`, `Year`, `Value`,
#'   `Area.Code` (or `area_code`). If `NULL`, only trade prices are
#'   used.
#' @param trade_prices A tibble as returned by [build_trade_prices()].
#'   If `NULL`, it is computed internally.
#' @param example Logical. If `TRUE`, return a small example tibble.
#'   Default `FALSE`.
#'
#' @returns A tibble with columns:
#'   - `year`: Integer year.
#'   - `item_prod_code`: Numeric production item code.
#'   - `price`: Price in KDollars per tonne.
#'
#' @export
#'
#' @examples
#' build_primary_prices(example = TRUE)
build_primary_prices <- function(
  primary_prod,
  value_of_production = NULL,
  trade_prices = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_build_primary_prices())
  }
  cli::cli_h1("Building primary prices")

  if (is.null(trade_prices)) {
    trade_prices <- .compute_trade_prices()
  }

  tibble::as_tibble(
    .compute_primary_prices(primary_prod, value_of_production, trade_prices)
  )
}

# -- CBS prices ----------------------------------------------------------------

#' Build CBS item prices
#'
#' @description
#' Compute prices for all commodity balance sheet items, including
#' processed products and crop residues. Prices are derived from trade
#' data, with special handling for items without direct trade prices
#' (palm kernels, soy hulls, brans, etc.). Crop residue prices are
#' estimated as a fraction of the product price.
#'
#' @param cbs A tibble of commodity balance sheets, as returned by
#'   [build_commodity_balances()] or [get_wide_cbs()].
#' @param trade_prices A tibble as returned by [build_trade_prices()].
#'   If `NULL`, it is computed internally.
#' @param residue_price_factor Numeric. Relative price of crop residues
#'   compared to the product. Default `0.1`.
#' @param example Logical. If `TRUE`, return a small example tibble.
#'   Default `FALSE`.
#'
#' @returns A tibble with columns:
#'   - `year`: Integer year.
#'   - `element`: `"import"` or `"export"`.
#'   - `item_cbs_code`: Numeric CBS item code.
#'   - `price`: Price in KDollars per tonne.
#'
#' @export
#'
#' @examples
#' build_cbs_prices(example = TRUE)
build_cbs_prices <- function(
  cbs,
  trade_prices = NULL,
  residue_price_factor = 0.1,
  example = FALSE
) {
  if (example) {
    return(.example_build_cbs_prices())
  }
  cli::cli_h1("Building CBS item prices")

  if (is.null(trade_prices)) {
    trade_prices <- .compute_trade_prices()
  }

  dt <- .compute_cbs_prices_raw(trade_prices)
  dt <- .estimate_missing_prices(dt)
  dt <- .add_residue_prices(dt, residue_price_factor)
  dt <- .add_proxy_prices(dt)
  dt <- .finalise_cbs_prices(dt, cbs)

  tibble::as_tibble(dt)
}

# -- Internal: trade prices ----------------------------------------------------

.compute_trade_prices <- function(raw_trade = NULL) {
  cli::cli_progress_step("Reading FAOSTAT trade data")
  dt <- raw_trade %||% .read_bilateral_trade_for_prices()

  # Keep only non-zero values
  dt <- dt[!is.na(value) & value != 0]

  # Aggregate globally
  dt <- dt[,
    .(value = sum(value, na.rm = TRUE)),
    by = c("year", "item_trade", "item_code_trade", "unit", "element")
  ]

  # Standardise unit names
  dt[unit == "1000 USD" | unit == "1000 US$", unit := "kdollars"]
  dt[unit == "tonnes", unit := "tonnes"]

  # Keep only value and quantity rows
  dt <- dt[unit %in% c("kdollars", "tonnes")]

  # Pivot wider: one column per unit
  dt <- data.table::dcast(
    dt,
    year + item_trade + item_code_trade + element ~ unit,
    value.var = "value"
  )

  if (!all(c("kdollars", "tonnes") %in% names(dt))) {
    return(dt[0])
  }

  dt[, price := kdollars / tonnes]
  dt <- dt[!is.na(price) & !is.infinite(price)]
  dt
}

.read_bilateral_trade_for_prices <- function() {
  dt <- whep_read_file("faostat-trade-bilateral")
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  data.table::setnames(dt, tolower)

  # Rename FAOSTAT columns to internal names
  fao_cols <- c("item code", "item", "element", "year", "unit", "value")
  int_cols <- c(
    "item_code_trade", "item_trade", "element", "year", "unit", "value"
  )
  present <- fao_cols %in% names(dt)
  data.table::setnames(dt, fao_cols[present], int_cols[present])

  # Standardise element names
  dt[, element := data.table::fcase(
    grepl("Import", element), "import",
    grepl("Export", element), "export",
    default = tolower(element)
  )]

  dt[, .(year, item_trade, item_code_trade, unit, element, value)]
}

# -- Internal: primary prices --------------------------------------------------

.compute_primary_prices <- function(
  primary_prod,
  value_of_production,
  trade_prices
) {
  cli::cli_progress_step("Computing primary prices")
  if (!data.table::is.data.table(primary_prod)) {
    data.table::setDT(primary_prod)
  }

  items_prod <- data.table::as.data.table(whep::items_prod_full)
  prod_bridge <- unique(
    items_prod[, .(item_prod_code, item_prod, item_cbs_code)]
  )

  # Global production totals by item
  prod_totals <- primary_prod[
    unit == "tonnes",
    .(value = sum(value, na.rm = TRUE)),
    by = c("year", "item_prod_code")
  ]
  prod_totals <- merge(
    prod_totals,
    prod_bridge,
    by = "item_prod_code",
    all.x = TRUE
  )

  # Export trade prices
  export_prices <- trade_prices[element == "export"]
  export_bridge <- unique(
    items_prod[, .(item_prod, item_prod_code)]
  )
  export_prices <- merge(
    export_prices,
    export_bridge,
    by.x = "item_trade",
    by.y = "item_prod",
    all.x = TRUE
  )
  export_prices <- export_prices[
    !is.na(item_prod_code),
    .(year, item_prod_code, price_export = price)
  ]

  dt <- merge(
    prod_totals,
    export_prices,
    by = c("year", "item_prod_code"),
    all.x = TRUE
  )

  # Add production value prices if available
  if (!is.null(value_of_production)) {
    vop <- .clean_value_of_production(value_of_production)
    vop_totals <- vop[,
      .(kdollars_prod = sum(value, na.rm = TRUE)),
      by = c("year", "item_prod_code")
    ]
    dt <- merge(
      dt,
      vop_totals,
      by = c("year", "item_prod_code"),
      all.x = TRUE
    )
    dt[, price_prod := kdollars_prod / value]
  } else {
    dt[, price_prod := NA_real_]
  }

  # Prefer export price, fallback to production price
  dt[,
    price := data.table::fifelse(
      is.na(price_export),
      price_prod,
      price_export
    )
  ]

  # Gap-fill
  dt <- fill_linear(
    dt,
    price,
    time_col = year,
    .by = c("item_prod_code")
  )
  data.table::setDT(dt)

  dt <- dt[!is.na(price)]
  dt[, .(year, item_prod_code, price)]
}

.clean_value_of_production <- function(vop) {
  vop <- data.table::as.data.table(vop)

  # Handle raw FAOSTAT columns (spaced or dotted) and pre-cleaned names
  data.table::setnames(vop, tolower)
  fao_cols <- c(
    "item code", "item.code",
    "area code", "area.code",
    "unit", "element", "year", "value"
  )
  int_cols <- c(
    "item_prod_code", "item_prod_code",
    "area_code", "area_code",
    "unit", "element", "year", "value"
  )
  present <- fao_cols %in% names(vop)
  data.table::setnames(
    vop, fao_cols[present], int_cols[present]
  )

  # Ensure item_prod_code is character (matches items_prod_full)
  vop[, item_prod_code := as.character(item_prod_code)]

  vop <- vop[
    grepl(
      "Gross Production Value.*constant.*US\\$",
      element,
      ignore.case = TRUE
    )
  ]
  vop[, `:=`(unit = "kdollars", element = "production")]

  # Aggregate to polity level
  .aggregate_to_polities(vop, item_prod_code)
}

# -- Internal: CBS prices ------------------------------------------------------

.compute_cbs_prices_raw <- function(trade_prices) {
  cli::cli_progress_step("Computing raw CBS item prices")
  cbs_trade <- data.table::as.data.table(whep::cbs_trade_codes)
  items <- data.table::as.data.table(whep::items_full)
  items_bridge <- unique(items[, .(item_cbs, item_cbs_code)])

  dt <- merge(
    trade_prices,
    cbs_trade[, .(item_code_trade, item_cbs)],
    by = "item_code_trade",
    all.x = TRUE
  )
  dt <- dt[!is.na(item_cbs)]

  dt <- dt[,
    .(
      tonnes = sum(tonnes, na.rm = TRUE),
      kdollars = sum(kdollars, na.rm = TRUE)
    ),
    by = c("year", "item_cbs", "element")
  ]
  dt[, price := kdollars / tonnes]

  dt <- merge(dt, items_bridge, by = "item_cbs", all.x = TRUE)
  dt <- dt[!is.na(item_cbs_code)]
  dt
}

.estimate_missing_prices <- function(dt) {
  cli::cli_progress_step("Estimating missing prices")

  # Palm kernel prices: estimate from palm oil price ratio
  pk <- dt[item_cbs_code %in% c(2562L, 2577L)]
  if (nrow(pk) > 0) {
    pk_wide <- data.table::dcast(
      pk[, .(year, item_cbs_code, element, price)],
      year + element ~ paste0("x", item_cbs_code),
      value.var = "price"
    )
    if (all(c("x2562", "x2577") %in% names(pk_wide))) {
      pk_wide[, price_scaling := x2562 / x2577]
      pk_wide <- fill_linear(
        pk_wide,
        price_scaling,
        time_col = year,
        .by = "element"
      )
      data.table::setDT(pk_wide)

      pk_fill <- pk_wide[
        source_price_scaling == "First value carried backwards"
      ]
      if (nrow(pk_fill) > 0) {
        pk_fill[, x2562 := x2577 * price_scaling]
        pk_new <- pk_fill[, .(
          year,
          element,
          item_cbs_code = 2562L,
          price = x2562
        )]
        # Remove existing 2562 rows for those years and replace
        dt <- dt[
          !(item_cbs_code == 2562L &
            year %in% pk_new$year &
            element %in% pk_new$element)
        ]
        items <- data.table::as.data.table(whep::items_full)
        pk_new[,
          item_cbs := items$item_cbs[
            match(2562L, items$item_cbs_code)
          ]
        ]
        dt <- data.table::rbindlist(
          list(dt, pk_new),
          use.names = TRUE,
          fill = TRUE
        )
      }
    }
  }

  # Remove infinite prices
  dt <- dt[!is.infinite(price) & !is.na(item_cbs)]

  # Add proxy prices for items without data
  .add_price_proxies(dt)
}

.add_price_proxies <- function(dt) {
  proxy_map <- list(
    list(from = "Soyabean Cake", to = "Soy hulls", factor = 0.1),
    list(
      from = "Sugar (Raw Equivalent)",
      to = "Alcohol, Non-Food",
      factor = 1.0
    ),
    list(from = "DDGS", to = "DDGS Barley", factor = 1.0),
    list(from = "DDGS", to = "Cake, maize", factor = 1.0)
  )

  items <- data.table::as.data.table(whep::items_full)
  items_bridge <- unique(items[, .(item_cbs, item_cbs_code)])

  for (pm in proxy_map) {
    source_rows <- dt[item_cbs == pm$from]
    if (nrow(source_rows) > 0) {
      new_rows <- data.table::copy(source_rows)
      new_rows[, `:=`(
        item_cbs = pm$to,
        price = price * pm$factor
      )]
      new_rows[,
        item_cbs_code := items_bridge$item_cbs_code[
          match(pm$to, items_bridge$item_cbs)
        ]
      ]
      dt <- data.table::rbindlist(
        list(dt, new_rows),
        use.names = TRUE,
        fill = TRUE
      )
    }
  }
  dt
}

.add_residue_prices <- function(dt, residue_price_factor) {
  cli::cli_progress_step("Adding residue prices")
  items_prod <- data.table::as.data.table(whep::items_prod_full)
  items <- data.table::as.data.table(whep::items_full)

  # Build per-item Cat_1 + Herb_Woody bridge
  herb_woody <- unique(
    items_prod[
      !is.na(Herb_Woody) & !is.na(item_cbs_code),
      .(item_cbs_code, Herb_Woody)
    ]
  )
  herb_woody <- herb_woody[
    !duplicated(item_cbs_code),
  ]

  # Add category info
  items_cats <- unique(items[, .(item_cbs, item_cbs_code, Cat_1)])
  dt <- merge(
    dt, items_cats,
    by = c("item_cbs", "item_cbs_code"), all.x = TRUE
  )
  dt <- merge(dt, herb_woody, by = "item_cbs_code", all.x = TRUE)

  # Ensure tonnes and kdollars are filled for proxy items
  dt[is.na(tonnes), tonnes := 1000]
  dt[is.na(kdollars), kdollars := tonnes * price]

  # Create product and residue rows
  product <- data.table::copy(dt)
  product[, product_residue := "Product"]

  residue <- data.table::copy(dt)
  residue[, product_residue := "Residue"]

  combined <- data.table::rbindlist(list(product, residue))
  combined[, land_use := "Cropland"]

  # Apply residues_as_items logic (from afsetools)
  combined[
    product_residue == "Residue",
    item_cbs := data.table::fifelse(
      Cat_1 %in% c("Cereals", "Pulses"),
      "Straw",
      data.table::fifelse(
        land_use != "Cropland" | Herb_Woody == "Woody",
        "Firewood",
        "Other crop residues"
      )
    )
  ]

  # Scale residue prices
  combined[
    product_residue == "Residue",
    kdollars := kdollars * residue_price_factor
  ]

  # Reaggregate
  combined <- combined[,
    .(
      tonnes = sum(tonnes, na.rm = TRUE),
      kdollars = sum(kdollars, na.rm = TRUE)
    ),
    by = c("year", "element", "item_cbs")
  ]
  combined[, price := kdollars / tonnes]

  # Re-add item codes
  items_bridge <- unique(items[, .(item_cbs, item_cbs_code)])
  combined <- merge(
    combined,
    items_bridge,
    by = "item_cbs",
    all.x = TRUE
  )
  combined[, source := "original"]
  combined
}

.add_proxy_prices <- function(dt) {
  cli::cli_progress_step("Adding proxy prices for missing items")
  items <- data.table::as.data.table(whep::items_full)
  items_bridge <- unique(items[, .(item_cbs, item_cbs_code)])

  # Proxy mappings: source item -> target item (+ price factor)
  proxy_map <- list(
    list(
      from = "Wheat and products",
      to = "Brans",
      code = 2111L,
      factor = 0.2
    ),
    list(
      from = "Sugar (Raw Equivalent)",
      to = "Sugar non-centrifugal",
      code = 2541L,
      factor = 1.0
    ),
    list(
      from = "Rice and products",
      to = "Ricebran Oil",
      code = 2581L,
      factor = 1.0
    )
  )

  estimated <- list()
  for (pm in proxy_map) {
    source_rows <- dt[item_cbs == pm$from]
    if (nrow(source_rows) > 0) {
      new_rows <- data.table::copy(source_rows)
      new_rows[, `:=`(
        item_cbs = pm$to,
        item_cbs_code = pm$code,
        price = price * pm$factor,
        source = "estimated"
      )]
      estimated <- c(estimated, list(new_rows))
    }
  }

  if (length(estimated) > 0) {
    est_dt <- data.table::rbindlist(estimated, use.names = TRUE, fill = TRUE)
    dt <- data.table::rbindlist(
      list(dt, est_dt),
      use.names = TRUE,
      fill = TRUE
    )
  }

  # Where both original and estimated exist, prefer original
  dt <- dt[!is.na(item_cbs_code)]
  dt_wide <- data.table::dcast(
    dt[, .(year, element, item_cbs, item_cbs_code, price, source)],
    year + element + item_cbs + item_cbs_code ~ source,
    value.var = "price",
    fun.aggregate = function(x) x[1]
  )

  if ("original" %in% names(dt_wide) && "estimated" %in% names(dt_wide)) {
    dt_wide[,
      price := data.table::fifelse(
        is.na(original),
        estimated,
        original
      )
    ]
    dt_wide[, c("original", "estimated") := NULL]
  } else if ("original" %in% names(dt_wide)) {
    data.table::setnames(dt_wide, "original", "price")
  } else if ("estimated" %in% names(dt_wide)) {
    data.table::setnames(dt_wide, "estimated", "price")
  }

  dt_wide
}

.finalise_cbs_prices <- function(dt, cbs) {
  cli::cli_progress_step("Finalising CBS prices")
  if (!data.table::is.data.table(cbs)) {
    data.table::setDT(cbs)
  }

  # Get all year x item combinations present in CBS
  cbs_items <- cbs[,
    .(year, item_cbs_code)
  ]
  cbs_items <- unique(cbs_items)

  # Join with prices to ensure coverage
  dt <- merge(
    cbs_items,
    dt,
    by = c("year", "item_cbs_code"),
    all = TRUE
  )

  # Complete all year x item x element combinations
  dt <- tidyr::complete(
    tibble::as_tibble(dt),
    year,
    tidyr::nesting(
      !!!rlang::syms(c("item_cbs", "item_cbs_code", "element"))
    )
  )
  data.table::setDT(dt)

  # Gap-fill prices over time
  dt <- fill_linear(
    dt,
    price,
    time_col = year,
    .by = c("item_cbs_code", "element")
  )
  data.table::setDT(dt)

  dt <- dt[!is.na(item_cbs) & !is.na(element)]
  dt[, .(year, element, item_cbs_code, price)]
}
