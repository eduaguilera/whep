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
#' keeps the pre-split entity alongside its successor: `ETH` is both 62
#' ("Ethiopia PDR") and 238 ("Ethiopia"), and `SDN` is both 276 and 206. This
#' used to be resolved with `unique(bridge, by = "iso3c")`, which keeps whichever
#' row happens to come first — and that silently returned the DISSOLVED area for
#' Ethiopia, so ISO3-keyed input for any year, 2000 included, was stamped 62.
#'
#' The tie is now broken on the polities database rather than on row order:
#' prefer the area code that IS its polity's `polity_area_code`, i.e. the
#' canonical reporting area WHEP resolves that polity to. That picks 238 for
#' `ETH` and 206 for `SDN`, and it is never over-determined — no ISO3 has two
#' canonical areas.
#'
#' The 62 ISO3 codes with NO canonical area are the small territories that
#' collapse into an aggregate polity (`ASM`, `AND`, ... into `ROW-1850-2023`), so
#' their area code never equals the aggregate's. Each names exactly one area, so
#' they need no tie-break — but rather than assume that, the function aborts if
#' any ISO3 is still ambiguous after the rule, instead of guessing as before.
#'
#' @noRd
.iso3_to_fao_area_code <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df
  bridge <- .current_area_lookup(include_unmapped = FALSE)[
    !is.na(area_iso3c),
    .(
      iso3c = area_iso3c,
      area_code_fao = area_code,
      canonical = area_code == polity_area_code
    )
  ]
  bridge <- unique(bridge)
  # Keep the canonical rows for an ISO3 when it has any, all of them otherwise.
  bridge[,
    keep := if (any(canonical)) canonical else rep(TRUE, .N),
    by = "iso3c"
  ]
  bridge <- bridge[keep == TRUE]
  bridge[, c("canonical", "keep") := NULL]

  ambiguous <- unique(bridge$iso3c[duplicated(bridge$iso3c)])
  if (length(ambiguous) > 0L) {
    cli::cli_abort(c(
      "Cannot map ISO3 to a single FAOSTAT area code.",
      x = "Ambiguous: {.val {ambiguous}}.",
      i = paste(
        "Each must resolve to one reporting area. Give the intended one a",
        "matching {.field polity_area_code} in the polities database, rather",
        "than letting row order decide."
      )
    ))
  }

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

#' Convert FAOSTAT numeric area_code to ISO3 area_code and add area name
#' @noRd
.fao_to_iso3_area_code <- function(df) {
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  }
  dt <- df
  bridge <- .current_area_lookup(include_unmapped = TRUE)[,
    .(area_code_fao = area_code, iso3c = area_iso3c, area = area_name)
  ]
  bridge <- unique(bridge, by = "area_code_fao")

  dt <- merge(
    dt,
    bridge,
    by.x = "area_code",
    by.y = "area_code_fao",
    all.x = TRUE,
    sort = FALSE
  )
  dt[, area_code := NULL]
  data.table::setnames(dt, "iso3c", "area_code")
  dt
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

.rice_milled_extraction_rate <- function() {
  0.67
}

.fix_item_codes <- function(dt) {
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
        item_cbs %in% c("Rice, paddy", "Rice (Paddy Equivalent)"),
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

.polity_bridge <- local({
  bridge <- NULL

  function() {
    if (is.null(bridge)) {
      bridge <<- .current_area_lookup(include_unmapped = FALSE)[,
        .(area_code, polity_code, polity_name, polity_area_code)
      ]
    }
    bridge
  }
})

.aggregate_to_polities <- function(df, ...) {
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
  # Say what is being dropped. This filter removed 1,109,466 rows of real FAOSTAT
  # production — 26% — in total silence: FAOSTAT's own regional groups plus the
  # deliberately unmapped aggregates. All expected, none of it visible, so a genuinely
  # unknown area code would have been just as quiet.
  .warn_unmapped_codes(dt, "polity_code", "area_code", "input")
  dt <- dt[!is.na(polity_code)]
  # GROUP BY THE KEY, NOT BY THE LABEL. `polity_area_code` is the reporting bucket this
  # function aggregates to, and several FAOSTAT areas can fold into one bucket. Grouping by
  # `polity_name` alongside it split those buckets by name, so the folded areas were never
  # summed: the result carried two rows for one `(year, area_code, ...)` key after the
  # rename below.
  #
  # Measured at 1990-2023: 1,525 duplicate
  # `(area_code, year, item_cbs, item_cbs_code, element, source)` keys, all in bucket 206,
  # in FAOSTAT_FBS_New and FAOSTAT_trade, 2014-2023. That bucket folds FAOSTAT areas 206
  # "Sudan (former)", 276 "Sudan" and 277 "South Sudan"; from 2014 Sudan and South Sudan
  # both report, and they carry different polity names, so the split fires.
  #
  # WHAT THAT COSTS DOWNSTREAM IS WORSE THAN THE DUPLICATE. `.select_best_source()` casts
  # these rows wide with no `fun.aggregate`, so data.table falls back to `length()` and the
  # "values" become ROW COUNTS. Whether that is visible is luck: the counts are integer,
  # and `FBS_Old_scaled` is double only when `scale_new_old` applies, so the build either
  # dies in `fcoalesce` with a type clash or completes with counts in place of quantities.
  # At 1990-2023 it dies; over the full range it completes. The crash was the good case.
  #
  # This is pre-existing and identical on main — both fail the same way at the same window,
  # with the same 1,525 keys — so it is not a consequence of routing more areas through the
  # polities database. It is, though, exactly the inconsistency this work is about: the
  # bucket is the polity-derived key, and summing into it is what the bucket means.
  by_cols <- c(
    "year",
    "polity_area_code",
    "polity_name",
    "unit",
    "element",
    dots
  )

  has_flag <- "fao_flag" %in% names(dt)
  if (has_flag) {
    dt <- dt[,
      .(value = sum(value, na.rm = TRUE), fao_flag = fao_flag[1L]),
      by = by_cols
    ]
  } else {
    dt <- dt[, .(value = sum(value, na.rm = TRUE)), by = by_cols]
  }

  # `area` MUST use the same vocabulary every other producer uses, and an earlier version
  # of this broke that. It named the bucket from the bucket's own `area_name` — stable, one
  # name per code, deterministic — which is defensible in isolation and wrong here, because
  # `.read_crop_residues()` still emits the period-specific `polity_name` as `area` and
  # several INNER joins key on `c("area", "area_code", ...)`:
  # read_raw_inputs.R:564/572/578 and build_production.R:362. With one side carrying
  # "Albania" and the other "Albania (1913-2025)", those joins miss and the rows disappear
  # without a word.
  #
  # Measured: 652,562 rows of real observations lost from a full-range `get_wide_cbs()`,
  # across 45 areas per item, 43 of them `national` polities — Albania, Bolivia, Ecuador,
  # Cabo Verde, Cambodia, Laos and others, exactly the ones where `area_name` and
  # `polity_name` differ. Every test passed: the loss is invisible to a suite that never
  # compares total row counts against a baseline.
  #
  # So the polity name stays, and the determinism problem that motivated the change is
  # solved differently: pick the representative by sorting rather than by row order, so a
  # bucket folding several differently-named areas gets the same label every run.
  data.table::setnames(
    dt,
    c("polity_area_code", "polity_name"),
    c("area_code", "area")
  )
  dt
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
  dt <- .fix_item_codes(dt)
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
  .aggregate_to_polities(dt, item_cbs, item_cbs_code)
}

.extract_cb <- function(pin_alias, years = NULL) {
  dt <- .extract_fao(pin_alias, years = years)
  items <- .items_cbs_bridge()
  merge(dt, items, by = c("item_cbs", "item_cbs_code"), sort = FALSE)
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
