# Internal helpers to read and pre-process raw FAOSTAT inputs.
# These replace Global's ExtractFAO(), ExtractCB(), filter_areas()
# and harmonize_countries() in whep conventions.
#
# Uses data.table for performance instead of dplyr/tidyr.

# -- Area code conversion ------------------------------------------------------

#' Convert ISO3 area_code to FAOSTAT numeric area_code
#'
#' @details
#' An ISO3 code can name more than one FAOSTAT reporting area, because FAOSTAT
#' keeps the pre-split entity alongside its successor: `ETH` names both 62
#' ("Ethiopia PDR", dissolved 1993) and 238 ("Ethiopia"), and `SDN` names both
#' 206 ("Sudan (former)") and 276 ("Sudan").
#'
#' The tie used to be broken with `unique(bridge, by = "iso3c")`, i.e. on row
#' order. `.current_area_lookup()` happens to order by `area_code`, so that kept
#' the LOWEST code, which for `ETH` is the dissolved 62 — for every year, 2021
#' included.
#'
#' The tie is now broken on the polities database instead: prefer the area code
#' that IS its polity's `polity_area_code`, i.e. the canonical reporting area
#' WHEP aggregates that polity to. That picks 238 for `ETH` and leaves `SDN` at
#' 206 (276 folds into bucket 206, so 206 is the canonical one).
#'
#' ISO3 codes with no canonical area are the territories that fold into an
#' aggregate bucket, whose code never equals their own. Each of those names
#' exactly one area, so they need no tie-break — but rather than assume it, all
#' rows are kept for such an ISO3 and the function aborts if any is still
#' ambiguous, instead of guessing as before.
#'
#' The bridge is year-insensitive by construction: `.current_area_lookup()` is
#' one row per `area_code`, so an ISO3 resolves to the same FAOSTAT area for
#' every year. That is what the caller wants -- `.proxy_polity_key()` stamps
#' the reporting area and then resolves the polity year-by-year with
#' `.add_polity_columns_dt()` -- but it means the stamped `area_code` must not
#' be read as "the area that reported in that row's year".
#'
#' @noRd
.iso3_to_fao_area_code <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df
  bridge <- .iso3_area_code_bridge()

  dt <- merge(
    dt,
    bridge,
    by.x = "area_code",
    by.y = "iso3c",
    all.x = TRUE,
    sort = FALSE
  )
  dt[, area_code := NULL]
  data.table::setnames(dt, "area_code_fao", "area_code")
  dt
}

# One FAOSTAT `area_code` per ISO3, with the tie broken on the polities
# database rather than on row order. See `.iso3_to_fao_area_code()`.
.iso3_area_code_bridge <- function() {
  bridge <- .current_area_lookup(include_unmapped = FALSE)[
    !is.na(area_iso3c),
    .(
      iso3c = area_iso3c,
      area_code_fao = area_code,
      is_canonical = !is.na(polity_area_code) & area_code == polity_area_code
    )
  ]
  bridge <- unique(bridge)
  # Keep the canonical rows for an ISO3 when it has any, all of them otherwise.
  bridge[, keep := if (any(is_canonical)) is_canonical else TRUE, by = "iso3c"]
  bridge <- bridge[keep == TRUE][, c("is_canonical", "keep") := NULL]

  ambiguous <- sort(unique(bridge$iso3c[duplicated(bridge$iso3c)]))
  if (length(ambiguous) > 0L) {
    cli::cli_abort(
      c(
        "Cannot map {length(ambiguous)} ISO3 code{?s} to one FAOSTAT area.",
        x = "Still ambiguous after the canonical-area rule: {.val {ambiguous}}.",
        i = paste(
          "Give the intended reporting area a matching",
          "{.field polity_area_code} in the polities database, rather than",
          "letting row order decide."
        )
      ),
      class = "whep_ambiguous_iso3_area"
    )
  }
  bridge
}

# -- Reading helpers -----------------------------------------------------------

.filter_years <- function(df, years) {
  if (is.null(years)) {
    return(df)
  }

  dt <- if (data.table::is.data.table(df)) df else data.table::as.data.table(df)
  year_col <- .detect_year_col(dt)

  # Fast path for contiguous ranges (common case in build pipelines).
  y_min <- min(years, na.rm = TRUE)
  y_max <- max(years, na.rm = TRUE)
  if (length(years) == (y_max - y_min + 1L)) {
    return(dt[dt[[year_col]] >= y_min & dt[[year_col]] <= y_max])
  }

  dt[dt[[year_col]] %in% years]
}

.detect_year_col <- function(df) {
  nms <- names(df)
  if ("year" %in% nms) {
    return("year")
  }
  if ("Year" %in% nms) {
    return("Year")
  }
  if ("year_code" %in% nms) {
    return("year_code")
  }
  if ("Year.Code" %in% nms) {
    return("Year.Code")
  }

  candidates <- c("year", "Year", "year_code", "Year.Code")
  cli::cli_abort(
    "Could not find a year column. Checked: {paste(candidates, collapse = ', ')}."
  )
}

.items_cbs_bridge <- local({
  cache <- NULL

  function() {
    if (is.null(cache)) {
      cache <<- data.table::as.data.table(whep::items_full)[,
        .(item_cbs, item_cbs_code)
      ]
      cache <<- unique(cache, by = c("item_cbs", "item_cbs_code"))
    }
    cache
  }
})


.read_faostat_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, name_repair = "universal")
}

# Get local file paths for a pin alias (download if needed, don't read).
.download_pin_paths <- function(file_alias) {
  file_info <- .fetch_file_info(file_alias, whep::whep_inputs)
  version <- .choose_version(file_info$version, NULL)

  tryCatch(
    .get_local_board() |>
      pins::pin_download(file_alias, version = version),
    error = function(e) {
      tryCatch(
        file_info |>
          .get_remote_board() |>
          pins::pin_download(file_alias, version = version),
        error = function(e) {
          .get_cache_paths(file_info, file_alias, version, e)
        }
      )
    }
  )
}

# Read a parquet file, optionally filtering by year range.
# When years and year_col are provided, uses arrow to leverage row-group
# statistics for predicate pushdown — only row groups overlapping the
# requested year range are read from disk, cutting both I/O time and
# peak memory (e.g. 990 MB → 56 MB for faostat-fbs-old).
# Falls back to nanoparquet for unfiltered reads (lighter dependency).
.read_input <- function(pin_alias, years = NULL, year_col = NULL) {
  cli::cli_alert_info("Fetching files for {pin_alias}...")
  paths <- .download_pin_paths(pin_alias)
  parquet_path <- grep("\\.parquet$", paths, value = TRUE)

  if (length(parquet_path) == 0L) {
    dt <- whep_read_file(pin_alias)
    data.table::setDT(dt)
    return(dt)
  }

  if (!is.null(years) && !is.null(year_col)) {
    y_min <- min(years, na.rm = TRUE)
    y_max <- max(years, na.rm = TRUE)
    dt <- arrow::open_dataset(parquet_path, format = "parquet") |>
      dplyr::filter(
        .data[[year_col]] >= y_min,
        .data[[year_col]] <= y_max
      ) |>
      dplyr::collect() |>
      data.table::as.data.table()
  } else {
    dt <- nanoparquet::read_parquet(parquet_path)
    data.table::setDT(dt)
  }

  dt
}

# -- FAOSTAT extraction --------------------------------------------------------

.harmonize_element_names <- function(dt) {
  lookup <- c(
    "Domestic supply quantity" = "domestic_supply",
    "Stock Variation" = "stock_variation",
    "Other uses (non-food)" = "other_uses",
    "Other uses" = "other_uses",
    "Food supply quantity (tonnes)" = "food",
    "Food" = "food",
    "Import Quantity" = "import",
    "Import quantity" = "import",
    "Export Quantity" = "export",
    "Export quantity" = "export",
    "Feed" = "feed",
    "Seed" = "seed",
    "Processing" = "processing",
    "Production" = "production"
  )
  # The 2026-06-15 Commodity Balances (non-food) release added a "Processed"
  # element (5023) for rubber, wool and silk. It is absent here, so those rows
  # are filtered out by .extract_fao() before .get_fiber_tobacco() ever sees
  # them -- even though cbs_trade_codes maps all three onto CBS items. Adding
  # it would introduce a processing flow those items do not currently carry,
  # which moves published values; that is #811, not this change.
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  mapped <- unname(lookup[dt$element])
  dt[, element := data.table::fifelse(is.na(mapped), element, mapped)]
  dt
}

.normalise_units <- function(dt) {
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }
  dt[,
    value := data.table::fifelse(
      unit %in% c("1000 tonnes", "1000 t"),
      value * 1000,
      value
    )
  ]
  dt[,
    unit := data.table::fifelse(
      unit %in% c("1000 tonnes", "1000 t"),
      "tonnes",
      data.table::fifelse(unit == "1000 US$", "kdollars", as.character(unit))
    )
  ]
  dt
}

# Paddy-to-milled extraction rate. FAO's Technical Conversion Factors for
# Agricultural Commodities gives national paddy-to-milled rates with a median of
# 65% (range 60-73; China mainland 67, India 66), and the FAO Food Balance
# Sheets handbook worked example uses 67%. WHEP applies the single global rate
# where the source carries a country dimension.
.rice_milled_extraction_rate <- function() {
  0.67
}

# Item names under which a source reports rice on a PADDY (rough-rice) basis,
# and which therefore need converting to WHEP's milled-equivalent contract.
#
# Which name means paddy depends on the FAOSTAT vintage, verified against the
# pins at India 2010 production:
#   faostat-fbs-new        2807 "Rice and products"        143,963 kt  paddy
#   faostat-fbs-old        2805 "Rice (Milled Equivalent)"  96,023 kt  milled
#   faostat-cbs-old-crops  2804 "Rice (Paddy Equivalent)"  143,963,008 t paddy
#   faostat-cbs-old-crops  2805 "Rice (Milled Equivalent)"  96,023,326 t milled
# (96,023 / 143,963 = 0.6670.)
#
# `"faostat"` is only correct where `item_cbs` still holds the source's own item
# label. Once a frame has been through the `items_full` lookup, every 2807 row
# is called "Rice and products" whatever its basis, so that path keeps the
# `"labelled"` default and is left alone (#751).
.paddy_rice_names <- function(vintage = c("labelled", "faostat")) {
  vintage <- rlang::arg_match(vintage)
  paddy <- c("Rice, paddy", "Rice (Paddy Equivalent)")
  if (vintage == "faostat") c(paddy, "Rice and products") else paddy
}

.fix_item_codes <- function(dt, paddy_rice_names = .paddy_rice_names()) {
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  }

  rice_key_cols <- intersect(
    c("year", "area_code", "area", "element", "unit"),
    names(dt)
  )
  if (length(rice_key_cols) > 0L && "item_cbs" %in% names(dt)) {
    milled_rice_keys <- unique(
      dt[
        item_cbs_code == 2805L &
          item_cbs == "Rice (Milled Equivalent)",
        rice_key_cols,
        with = FALSE
      ]
    )
    if (nrow(milled_rice_keys) > 0L) {
      milled_rice_keys[, .has_milled_rice := TRUE]
      dt[
        milled_rice_keys,
        .has_milled_rice := i..has_milled_rice,
        on = rice_key_cols
      ]
      dt <- dt[
        !(!is.na(.has_milled_rice) &
          item_cbs_code %in% c(2804L, 2807L) &
          item_cbs == "Rice (Paddy Equivalent)")
      ]
      dt[, .has_milled_rice := NULL]
    }
  }

  if ("value" %in% names(dt)) {
    dt[
      item_cbs_code %in%
        c(2804L, 2807L) &
        item_cbs %in% paddy_rice_names,
      value := value * .rice_milled_extraction_rate()
    ]
  }

  dt[
    item_cbs_code %in%
      c(2804L, 2805L, 2807L) &
      item_cbs %in%
        c(
          "Rice, paddy",
          "Rice (Milled Equivalent)",
          "Rice (Paddy Equivalent)"
        ),
    `:=`(
      item_cbs_code = 2807L,
      item_cbs = "Rice and products"
    )
  ]

  dt[
    item_cbs_code == 2820L,
    `:=`(
      item_cbs_code = 2552L,
      item_cbs = "Groundnuts"
    )
  ]

  dt
}

.aggregate_to_polities <- function(df, ..., source_label = NULL) {
  dots <- as.character(match.call(expand.dots = FALSE)$...)

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df
  dt <- .add_polity_columns_dt(
    dt,
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  )
  dt <- dt[!is.na(polity_code)]
  # A bucket can fold several live territories. Say so out loud here, where the
  # fold is created, rather than letting the summed value travel with a polity
  # that covers only part of it (whep#414).
  .warn_partial_bucket_polities(dt)
  .warn_folded_areas(dt, source_label)
  # `polity_name` is deliberately NOT a grouping key. It is a property of the
  # member row, so keying on it splits a bucket whose members resolve to
  # different polities -- the bucket stops summing without a single value
  # moving (whep#563). The label is attached after the sum instead, from the
  # bucket's own code, which is what `polity_bucket_coverage()` and the
  # reporting columns already say a bucket is called.
  by_cols <- c("year", "polity_area_code", "unit", "element", dots)
  labels <- .bucket_area_labels(dt)

  has_flag <- "fao_flag" %in% names(dt)
  if (has_flag) {
    dt <- dt[,
      .(value = sum(value, na.rm = TRUE), fao_flag = fao_flag[1L]),
      by = by_cols
    ]
  } else {
    dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = by_cols]
  }

  .apply_bucket_area_labels(dt, labels)
}

.extract_fao <- function(pin_alias, years = NULL) {
  cb_elements <- c(
    "production",
    "import",
    "export",
    "stock_variation",
    "domestic_supply",
    "food",
    "feed",
    "seed",
    "processing",
    "other_uses"
  )

  dt <- .read_input(pin_alias, years = years, year_col = "Year")
  data.table::setnames(
    dt,
    c(
      "Item Code",
      "Item",
      "Area",
      "Area Code",
      "Unit",
      "Element",
      "Year",
      "Value"
    ),
    c(
      "item_cbs_code",
      "item_cbs",
      "area",
      "area_code",
      "unit",
      "element",
      "year",
      "value"
    )
  )

  # Rename FAOSTAT flag if present
  if ("Flag" %in% names(dt)) {
    data.table::setnames(dt, "Flag", "fao_flag")
  }
  dt <- .harmonize_element_names(dt)
  dt <- .normalise_units(dt)
  # `item_cbs` still holds FAOSTAT's own item label here, so a "Rice and
  # products" row is the new Food Balances item and is on a paddy basis.
  dt <- .fix_item_codes(dt, paddy_rice_names = .paddy_rice_names("faostat"))
  dt <- dt[element %in% cb_elements]
  cols <- c(
    "area",
    "area_code",
    "item_cbs",
    "item_cbs_code",
    "element",
    "unit",
    "year",
    "value"
  )
  if ("fao_flag" %in% names(dt)) {
    cols <- c(cols, "fao_flag")
  }
  dt <- dt[, cols, with = FALSE]
  out <- .aggregate_to_polities(
    dt,
    item_cbs,
    item_cbs_code,
    source_label = pin_alias
  )
  # Pin the row order, for the same reason `.extract_cb()` below does and one
  # stage earlier, so the two callers that stop here get it too: the CBS build
  # reads `faostat-cbs-new` and `faostat-trade-totals` through this function
  # and never reaches `.extract_cb()`. `.read_input()` reads the parquet
  # through arrow's multi-threaded scanner, whose row order varies between
  # sessions, and the `by=` aggregation above emits groups in order of first
  # appearance -- so it hands that variation straight on. Measured on the real
  # pins at 1950-1965, `.read_fao_trade()` came back in a different order in
  # every one of three sessions (339,220 rows, same rows, same values,
  # whep#420). The key is the aggregation key, so the order is total.
  data.table::setorderv(
    out,
    intersect(
      c("year", "area_code", "item_cbs_code", "item_cbs", "element", "unit"),
      names(out)
    )
  )
  out
}

.extract_cb <- function(pin_alias, years = NULL) {
  dt <- .extract_fao(pin_alias, years = years)
  items <- .items_cbs_bridge()
  out <- merge(dt, items, by = c("item_cbs", "item_cbs_code"), sort = FALSE)
  # Pin the row order. Nothing above this line pins one: `.read_input()` reads
  # the parquet through arrow's multi-threaded scanner, whose row order varies
  # between sessions, and neither the `by=` aggregation in
  # `.aggregate_to_polities()` nor `merge(sort = FALSE)` restores one. These
  # four tables travel as the `.cb_extracts` attribute of
  # `build_primary_production()`, so an unpinned order made that published
  # object not `identical()` to itself across sessions -- same rows, same
  # values, different order -- and broke the `identical()` reproducibility
  # control that a change is judged with (whep#747). The key below is unique
  # (it is the aggregation key), so the order is total.
  data.table::setorderv(
    out,
    intersect(
      c("year", "area_code", "item_cbs_code", "item_cbs", "element", "unit"),
      names(out)
    )
  )
  out
}

# -- Processing helpers (from comdat_global) -----------------------------------

.processed_raw <- function(df, cb_processing_eq) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- data.table::copy(df)
  if (!data.table::is.data.table(cb_processing_eq)) {
    cb_processing_eq <- data.table::as.data.table(cb_processing_eq)
  }
  rhs <- data.table::copy(cb_processing_eq)
  data.table::setnames(rhs, "ProcessedItem", "processed_item")
  rhs[, Value_fraction := NULL]
  rhs <- rhs[!is.na(Product_fraction)]

  join_keys <- "processed_item"
  if ("year" %in% names(rhs)) {
    join_keys <- c(join_keys, "year")
  }

  data.table::setnames(dt, "item_cbs", "processed_item")
  dt <- dt[element == "processing"]
  dt <- merge(
    dt,
    rhs,
    by = join_keys,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  dt[, `:=`(
    value_proc = value * Product_fraction,
    element = "production"
  )]
  dt
}

.correct_processed <- function(
  processed_df,
  cbs,
  no_data_products = character()
) {
  cb_proc_required <- data.table::as.data.table(whep::cb_processing)
  cb_proc_required <- cb_proc_required[,
    .(required = sum(Required, na.rm = TRUE)),
    by = "item_cbs"
  ]
  cb_proc_required <- cb_proc_required[required > 0]

  if (!data.table::is.data.table(cbs)) {
    data.table::setDT(cbs)
  }
  cbs_dt <- cbs
  cbs_summary <- cbs_dt[,
    .(value = sum(value, na.rm = TRUE), item_cbs_code = item_cbs_code[1L]),
    by = c("area", "area_code", "year", "item_cbs", "element")
  ]

  if (!data.table::is.data.table(processed_df)) {
    data.table::setDT(processed_df)
  }
  dt <- processed_df[,
    .(value_proc = sum(value_proc, na.rm = TRUE)),
    by = c("area", "area_code", "year", "item_cbs", "element")
  ]
  dt <- merge(dt, cb_proc_required, by = "item_cbs", all.x = TRUE, sort = FALSE)
  dt <- merge(
    dt,
    cbs_summary,
    by = c("area", "area_code", "year", "item_cbs", "element"),
    all.x = TRUE,
    sort = FALSE
  )
  dt[, scaling_raw := value / value_proc]
  dt[scaling_raw == 0, scaling_raw := NA_real_]

  # This fill is unbounded along the year axis, and it decides whether a
  # processing output EXISTS, not only what it is worth: with no anchor in the
  # frame at all, `scaling_raw` stays NA and the `scaling` below collapses to 0
  # for every item that is neither `Required` nor a `no_data_product`, so the
  # pathway emits a zero that the `value != 0` filters downstream delete.
  #
  # That makes the frame's year span load-bearing, which is half of whep#833:
  # a year-scoped build hands this function a truncated axis and loses outputs
  # a full-range build keeps. Measured at 2010, the 14 keys the scoped build
  # lost sit 7 to 49 years from their nearest anchor -- Italy's Ricebran Oil is
  # calibrated at 2010 off a single 1961 observation carried forward 49 years.
  # A wider `.context_years()` margin therefore cannot fix it; see the margin
  # comment in R/build_cache.R.
  dt <- fill_linear(
    dt,
    scaling_raw,
    time_col = year,
    .by = c("area", "area_code", "item_cbs", "element"),
    .copy = FALSE
  )
  if (!data.table::is.data.table(dt)) {
    data.table::setDT(dt)
  } else {
    data.table::setalloccol(dt)
  }

  dt[, item_cbs_code := NULL]
  dt[,
    scaling := data.table::fifelse(
      is.na(scaling_raw),
      data.table::fifelse(
        !is.na(required) | item_cbs %in% no_data_products,
        1,
        0
      ),
      data.table::fifelse(
        source_scaling_raw == "Original",
        scaling_raw,
        data.table::fifelse(
          scaling_raw > 5,
          5,
          data.table::fifelse(scaling_raw < 0.2, 0.2, scaling_raw)
        )
      )
    )
  ]
  dt[, value_final := value_proc * scaling]
  dt
}

# -- CBS testing helpers -------------------------------------------------------

.test_cbs <- function(df) {
  items_prod <- data.table::as.data.table(whep::items_prod_full)
  prim_double <- data.table::as.data.table(whep::primary_double)

  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df

  # pivot_wider: element -> columns, value -> values, fill with 0
  id_cols <- setdiff(names(dt), c("element", "value", "source"))
  form <- stats::as.formula(
    paste(paste(id_cols, collapse = " + "), "~ element")
  )
  dt <- data.table::dcast(dt, form, value.var = "value", fill = 0)
  expected_elements <- c(
    "domestic_supply",
    "production",
    "import",
    "export",
    "stock_variation",
    "food",
    "feed",
    "seed",
    "processing",
    "processing_primary",
    "other_uses"
  )
  missing_elements <- setdiff(expected_elements, names(dt))
  if (length(missing_elements) > 0L) {
    dt[, (missing_elements) := 0]
  }

  dt[, `:=`(
    ds_destinies = round(
      domestic_supply -
        food -
        feed -
        seed -
        processing -
        processing_primary -
        other_uses,
      4
    ),
    balance = round(
      production +
        import -
        export -
        stock_variation -
        food -
        feed -
        seed -
        processing -
        processing_primary -
        other_uses,
      4
    ),
    balance2 = round(
      production + import - export - stock_variation - domestic_supply,
      4
    )
  )]

  # Join with prim_double to get Multi_type
  pd_sub <- prim_double[is.na(Item_area)]
  pd_sub <- merge(
    pd_sub,
    items_prod,
    by = "item_prod",
    all.x = TRUE,
    sort = FALSE
  )
  pd_sub <- pd_sub[, .(item_cbs_code, Multi_type)]
  dt <- merge(dt, pd_sub, by = "item_cbs_code", all.x = TRUE, sort = FALSE)

  dt[,
    multi_type := data.table::fifelse(
      is.na(Multi_type),
      "Single",
      Multi_type
    )
  ]
  dt[,
    destiny_replacement := data.table::fifelse(
      multi_type != "Single",
      "none",
      data.table::fifelse(
        ds_destinies == balance,
        "default_prone",
        "none"
      )
    )
  ]
  dt[,
    check := data.table::fifelse(
      multi_type != "Single",
      ds_destinies == balance,
      balance == 0
    )
  ]
  dt[, Multi_type := NULL]
  dt
}

.untest_cbs <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df

  # Remove test columns
  drop_cols <- intersect(
    c(
      "ds_destinies",
      "balance",
      "balance2",
      "multi_type",
      "default_destiny",
      "destiny_replacement",
      "check"
    ),
    names(dt)
  )
  if (length(drop_cols) > 0L) {
    dt[, (drop_cols) := NULL]
  }

  # Identify element columns to melt (those that were pivoted wider)
  element_cols <- c(
    "domestic_supply",
    "export",
    "feed",
    "food",
    "import",
    "other_uses",
    "processing",
    "processing_primary",
    "production",
    "seed",
    "stock_variation"
  )
  measure_cols <- intersect(element_cols, names(dt))
  data.table::melt(
    dt,
    measure.vars = measure_cols,
    variable.name = "element",
    value.name = "value"
  )
}
