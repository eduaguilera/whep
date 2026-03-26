# compare_global_whep.R
# ─────────────────────────────────────────────────────────────
# End-to-end comparison of whep pipeline outputs against the
# Global reference files stored as pins.  Covers:
#
#   1.  Tolerance-based value comparison  (Primary, CBS, Coefs)
#   2.  Deep-dive diagnostics            (ratio patterns, name diffs)
#   3.  Sanity checks                    (year ranges, elements, units)
#
# Usage:
#   source("inst/scripts/compare_global_whep.R")
# ─────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(cli)

# -- Configuration ------------------------------------------------------------

# Versions of the Global reference data to compare against.
# Find available versions with:
#   whep::whep_list_file_versions("primary_prod")
#   whep::whep_list_file_versions("commodity_balance_sheet")
#   whep::whep_list_file_versions("processing_coefs")
global_primary_version <- "20250714T123350Z-74e7f"
global_cbs_version <- "20250714T123343Z-114b5"
global_coefs_version <- "20250714T123348Z-06c63"

# Tolerance for numeric comparison (relative)
rel_tol <- 1e-6
# Tolerance for numeric comparison (absolute, for values near zero)
abs_tol <- 1e-4

# == Helpers ===================================================================

# -- Harmonise Global reference (pin format) to comparison format --------------

harmonise_global_primary <- function(df) {
  df |>
    dplyr::rename(
      year = Year,
      value = Value,
      item_prod = item,
      item_prod_code = item_code,
      live_anim = Live_anim,
      live_anim_code = Live_anim_code,
      item_cbs_code = item_code_cbs
    ) |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      year = as.integer(year),
      item_prod_code = as.integer(item_prod_code)
    ) |>
    dplyr::as_tibble()
}

harmonise_global_cbs <- function(df) {
  elem_map <- c(
    "Domestic_supply" = "domestic_supply",
    "Production" = "production",
    "Export" = "export",
    "Import" = "import",
    "Stock_variation" = "stock_variation",
    "Food" = "food",
    "Feed" = "feed",
    "Seed" = "seed",
    "Other_uses" = "other_uses",
    "Processing" = "processing",
    "Processing_primary" = "processing_primary"
  )
  df |>
    dplyr::rename(
      year = Year,
      value = Value,
      element = Element,
      item_cbs = item,
      item_cbs_code = item_code
    ) |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      year = as.integer(year),
      item_cbs_code = as.integer(item_cbs_code),
      element = dplyr::recode(element, !!!elem_map)
    ) |>
    dplyr::as_tibble()
}

harmonise_global_coefs <- function(df) {
  items <- whep::items_full |>
    dplyr::select(item_cbs, item_cbs_code)

  df |>
    dplyr::rename(
      year = Year,
      item_cbs_code_to_process = item_code,
      value_to_process = Value,
      initial_conversion_factor = Product_fraction,
      initial_value_processed = value_proc_raw,
      conversion_factor_scaling = Scaling,
      final_conversion_factor = cf,
      final_value_processed = value_proc
    ) |>
    # Look up CBS code for the processed product name
    dplyr::left_join(items, by = c("item" = "item_cbs")) |>
    dplyr::rename(item_cbs_code_processed = item_cbs_code) |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      year = as.integer(year)
    ) |>
    dplyr::as_tibble()
}

# -- Harmonise WHEP pipeline output to comparison format -----------------------

harmonise_whep_primary <- function(df) {
  # Build comprehensive name map: crops from items_prod, livestock from

  # animals_codes (which covers stock items that items_prod lacks)
  crop_names <- whep::items_prod |>
    dplyr::select(item_prod_code, item_prod = item_prod_name)
  livestock_names <- whep::animals_codes |>
    dplyr::select(item_prod_code = item_cbs_code, item_prod = item_cbs) |>
    dplyr::distinct()
  item_names <- dplyr::bind_rows(crop_names, livestock_names) |>
    dplyr::distinct(item_prod_code, .keep_all = TRUE)

  polities <- whep::polities |>
    dplyr::select(area_code, area = area_name)

  df |>
    dplyr::mutate(
      item_prod_code = as.integer(item_prod_code),
      live_anim_code = as.integer(live_anim_code)
    ) |>
    dplyr::left_join(item_names, by = "item_prod_code") |>
    dplyr::left_join(polities, by = "area_code") |>
    dplyr::as_tibble()
}

harmonise_whep_cbs <- function(df) {
  items <- whep::items_full |>
    dplyr::select(item_cbs_code, item_cbs)
  polities <- whep::polities |>
    dplyr::select(area_code, area = area_name)

  df |>
    dplyr::mutate(
      stock_variation = -stock_retrieval,
      .keep = "unused"
    ) |>
    tidyr::pivot_longer(
      cols = -c(year, area_code, item_cbs_code),
      names_to = "element",
      values_to = "value"
    ) |>
    dplyr::left_join(items, by = "item_cbs_code") |>
    dplyr::left_join(polities, by = "area_code") |>
    dplyr::as_tibble()
}

# -- Generic tolerance-based comparison ----------------------------------------

compare_outputs <- function(
  whep_df,
  global_df,
  key_cols,
  value_cols,
  label = "dataset"
) {
  cli::cli_h1("Comparing {label}")

  # --- Dimensions ---
  cli::cli_text("WHEP: {nrow(whep_df)} rows x {ncol(whep_df)} cols")
  cli::cli_text("Global: {nrow(global_df)} rows x {ncol(global_df)} cols")

  # --- Column check ---
  common_cols <- intersect(names(whep_df), names(global_df))
  only_whep <- setdiff(names(whep_df), names(global_df))
  only_global <- setdiff(names(global_df), names(whep_df))
  if (length(only_whep) > 0) {
    cli::cli_alert_info(
      "Columns only in WHEP: {paste(only_whep, collapse=', ')}"
    )
  }
  if (length(only_global) > 0) {
    cli::cli_alert_info(
      "Columns only in Global: {paste(only_global, collapse=', ')}"
    )
  }

  # --- Restrict to shared columns ---
  shared <- intersect(common_cols, c(key_cols, value_cols))
  whep_sub <- whep_df |> dplyr::select(dplyr::all_of(shared))
  global_sub <- global_df |> dplyr::select(dplyr::all_of(shared))

  # --- Key coverage ---
  shared_keys <- intersect(key_cols, shared)
  whep_keys <- whep_sub |>
    dplyr::select(dplyr::all_of(shared_keys)) |>
    dplyr::distinct()
  global_keys <- global_sub |>
    dplyr::select(dplyr::all_of(shared_keys)) |>
    dplyr::distinct()

  only_in_whep <- dplyr::anti_join(whep_keys, global_keys, by = shared_keys)
  only_in_global <- dplyr::anti_join(global_keys, whep_keys, by = shared_keys)

  cli::cli_text("Keys only in WHEP:   {nrow(only_in_whep)}")
  cli::cli_text("Keys only in Global: {nrow(only_in_global)}")

  if (nrow(only_in_whep) > 0 && nrow(only_in_whep) <= 20) {
    cli::cli_alert_warning("Sample keys only in WHEP:")
    print(head(only_in_whep, 20))
  }
  if (nrow(only_in_global) > 0 && nrow(only_in_global) <= 20) {
    cli::cli_alert_warning("Sample keys only in Global:")
    print(head(only_in_global, 20))
  }

  # --- Value comparison on matched rows ---
  val_cols_present <- intersect(value_cols, shared)

  merged <- dplyr::inner_join(
    whep_sub |>
      dplyr::rename_with(
        ~ paste0(.x, "_whep"),
        dplyr::all_of(val_cols_present)
      ),
    global_sub |>
      dplyr::rename_with(
        ~ paste0(.x, "_global"),
        dplyr::all_of(val_cols_present)
      ),
    by = shared_keys
  ) |>
    dplyr::as_tibble()

  cli::cli_text("Matched rows: {nrow(merged)}")

  for (vc in val_cols_present) {
    w_col <- paste0(vc, "_whep")
    g_col <- paste0(vc, "_global")

    w <- merged[[w_col]]
    g <- merged[[g_col]]

    abs_diff <- abs(w - g)
    denom <- pmax(abs(g), abs_tol)
    rel_diff <- abs_diff / denom

    n_exact <- sum(w == g, na.rm = TRUE)
    n_close <- sum(rel_diff <= rel_tol, na.rm = TRUE)
    n_differ <- sum(rel_diff > rel_tol, na.rm = TRUE)
    n_na_whep <- sum(is.na(w) & !is.na(g))
    n_na_glob <- sum(!is.na(w) & is.na(g))
    max_rel <- if (n_differ > 0) {
      max(rel_diff[rel_diff > rel_tol], na.rm = TRUE)
    } else {
      0
    }
    max_abs <- if (n_differ > 0) {
      max(abs_diff[rel_diff > rel_tol], na.rm = TRUE)
    } else {
      0
    }

    cli::cli_h2("Value column: {vc}")
    cli::cli_text("  Exact matches:      {n_exact}")
    cli::cli_text("  Close (rel<{rel_tol}): {n_close}")
    cli::cli_text("  Differ:             {n_differ}")
    cli::cli_text("  NA only in WHEP:    {n_na_whep}")
    cli::cli_text("  NA only in Global:  {n_na_glob}")
    if (n_differ > 0) {
      cli::cli_text("  Max abs diff:       {signif(max_abs, 5)}")
      cli::cli_text("  Max rel diff:       {signif(max_rel, 5)}")

      # Show worst mismatches
      idx <- order(rel_diff, decreasing = TRUE)[1:min(10, n_differ)]
      cli::cli_alert_warning("Top mismatches:")
      print(merged[idx, c(shared_keys, w_col, g_col)] |> as.data.frame())
    }
  }

  # --- Summary verdict ---
  if (nrow(only_in_whep) == 0 &&
        nrow(only_in_global) == 0 &&
        n_differ == 0) {
    cli::cli_alert_success("{label}: IDENTICAL (within tolerance)")
  } else {
    cli::cli_alert_warning("{label}: DIFFERENCES FOUND")
  }

  invisible(list(
    merged = merged,
    only_in_whep = only_in_whep,
    only_in_global = only_in_global
  ))
}

# == Load data =================================================================

cli::cli_h1("Loading data")

# ---- Global reference (from pins) ----
cli::cli_text("Reading Global reference data from pins...")

global_primary <- whep::whep_read_file(
  "primary_prod",
  version = global_primary_version
) |>
  harmonise_global_primary()

global_cbs <- whep::whep_read_file(
  "commodity_balance_sheet",
  version = global_cbs_version
) |>
  harmonise_global_cbs()

global_coefs <- whep::whep_read_file(
  "processing_coefs",
  version = global_coefs_version
) |>
  harmonise_global_coefs()

cli::cli_alert_success("Global reference data loaded")

# ---- WHEP outputs (run pipeline live) ----
cli::cli_text("Running WHEP pipeline...")
whep_primary_raw <- whep::build_primary_production()
whep_cbs_raw <- whep::build_commodity_balances(whep_primary_raw)
whep_coefs <- whep::build_processing_coefs(whep_cbs_raw)
cli::cli_alert_success("WHEP pipeline complete")

# Harmonise WHEP outputs to comparison format
whep_primary <- harmonise_whep_primary(whep_primary_raw)
whep_cbs <- harmonise_whep_cbs(whep_cbs_raw)

# == 1. Tolerance-based comparison =============================================

result_primary <- compare_outputs(
  whep_df = whep_primary,
  global_df = global_primary,
  key_cols = c(
    "year",
    "area_code",
    "item_prod_code",
    "unit"
  ),
  value_cols = c("value"),
  label = "Primary Production"
)

result_cbs <- compare_outputs(
  whep_df = whep_cbs,
  global_df = global_cbs,
  key_cols = c(
    "year",
    "area_code",
    "item_cbs_code",
    "element"
  ),
  value_cols = c("value"),
  label = "Commodity Balance Sheets"
)

result_coefs <- compare_outputs(
  whep_df = whep_coefs,
  global_df = global_coefs,
  key_cols = c(
    "year",
    "area_code",
    "item_cbs_code_to_process",
    "item_cbs_code_processed"
  ),
  value_cols = c(
    "value_to_process",
    "initial_conversion_factor",
    "initial_value_processed",
    "conversion_factor_scaling",
    "final_conversion_factor",
    "final_value_processed"
  ),
  label = "Processing Coefficients"
)

# == 2. Deep-dive diagnostics ==================================================

cli::cli_h1("Deep-dive: Primary mismatch patterns")

# Helper: build a name lookup for item_prod_code (for display)
.item_name <- function(code) {
  nms <- dplyr::bind_rows(
    whep::items_prod |>
      dplyr::select(item_prod_code, name = item_prod_name),
    whep::animals_codes |>
      dplyr::select(item_prod_code = item_cbs_code, name = item_cbs) |>
      dplyr::distinct()
  ) |> dplyr::distinct(item_prod_code, .keep_all = TRUE)
  out <- nms$name[match(code, nms$item_prod_code)]
  dplyr::if_else(is.na(out), paste0("code_", code), out)
}

# Helper: build a name lookup for area_code (for display)
.area_name <- function(code) {
  nms <- whep::polities |>
    dplyr::select(area_code, name = area_name) |>
    dplyr::distinct()
  out <- nms$name[match(code, nms$area_code)]
  dplyr::if_else(is.na(out), paste0("code_", code), out)
}

# ---- 2a. Ratio distribution (whep / Global)
prim_merged <- result_primary$merged
if ("value_whep" %in% names(prim_merged)) {
  prim_mis <- prim_merged |>
    dplyr::mutate(
      ratio = value_whep / value_global,
      abs_diff = abs(value_whep - value_global),
      rel_diff = dplyr::if_else(
        value_global != 0,
        abs_diff / abs(value_global),
        abs_diff
      )
    ) |>
    dplyr::filter(rel_diff >= rel_tol)

  if (nrow(prim_mis) > 0) {
    ratio_bins <- prim_mis |>
      dplyr::filter(is.finite(ratio), ratio != 0) |>
      dplyr::mutate(
        ratio_bin = dplyr::case_when(
          abs(ratio - 1000) < 1 ~ "~1000x (WHEP >> Global)",
          abs(ratio - 0.001) < 0.0001 ~ "~0.001x (WHEP << Global)",
          ratio > 100 ~ ">100x",
          ratio < 0.01 ~ "<0.01x",
          abs(ratio - 1) < 0.05 ~ "~1x (small diff, <5%)",
          abs(ratio - 1) < 0.2 ~ "~1x (moderate diff, 5-20%)",
          TRUE ~ "other (>20% diff)"
        )
      ) |>
      dplyr::count(ratio_bin) |>
      dplyr::arrange(dplyr::desc(n))

    cli::cli_h2("Ratio bins (WHEP / Global)")
    print(ratio_bins)

    # ---- 2b. Livestock vs non-livestock breakdown
    livestock_units <- c("heads", "LU", "t_head", "t_LU")

    livestock_mis <- prim_mis |>
      dplyr::filter(unit %in% livestock_units)
    other_mis <- prim_mis |>
      dplyr::filter(!unit %in% livestock_units)

    cli::cli_h2("Livestock mismatches ({nrow(livestock_mis)} rows)")
    if (nrow(livestock_mis) > 0) {
      # Rounded ratio distribution
      cli::cli_text("Ratio distribution:")
      livestock_mis |>
        dplyr::filter(is.finite(ratio)) |>
        dplyr::mutate(ratio_check = round(ratio, 2)) |>
        dplyr::count(ratio_check) |>
        dplyr::arrange(dplyr::desc(n)) |>
        utils::head(10) |>
        print()
      # Which items have ~1000x ratio? (poultry unit bug in Global)
      poultry_1000x <- livestock_mis |>
        dplyr::filter(abs(ratio - 1000) < 1) |>
        dplyr::mutate(item = .item_name(item_prod_code)) |>
        dplyr::count(item, item_prod_code, unit) |>
        dplyr::arrange(dplyr::desc(n))
      if (nrow(poultry_1000x) > 0) {
        cli::cli_h3("Items with ~1000x ratio (Global unit bug)")
        print(poultry_1000x)
      }
    }

    cli::cli_h2("Non-livestock (crop) mismatches ({nrow(other_mis)} rows)")
    if (nrow(other_mis) > 0) {
      other_mis |>
        dplyr::count(unit) |>
        dplyr::arrange(dplyr::desc(n)) |>
        print()
      # Top items by number of mismatches
      cli::cli_text("Top items by mismatch count:")
      other_mis |>
        dplyr::mutate(item = .item_name(item_prod_code)) |>
        dplyr::count(item, item_prod_code) |>
        dplyr::arrange(dplyr::desc(n)) |>
        utils::head(15) |>
        print()
    }
  } else {
    cli::cli_alert_success("No primary mismatches to deep-dive into")
  }
}

# ---- 2c. Unmatched items (Primary) — by code
cli::cli_h1("Deep-dive: Primary unmatched items")

g_items <- global_primary |>
  dplyr::distinct(item_prod_code) |>
  dplyr::pull()
w_items <- whep_primary |>
  dplyr::distinct(item_prod_code) |>
  dplyr::pull()

g_only_items <- setdiff(g_items, w_items)
w_only_items <- setdiff(w_items, g_items)

cli::cli_text("Global-only item codes: {length(g_only_items)}")
if (length(g_only_items) > 0) {
  labels <- paste0(.item_name(g_only_items), " (", g_only_items, ")")
  cli::cli_text("{paste(labels, collapse = ', ')}")
}

cli::cli_text("WHEP-only item codes:   {length(w_only_items)}")
if (length(w_only_items) > 0) {
  labels <- paste0(.item_name(w_only_items), " (", w_only_items, ")")
  cli::cli_text("{paste(utils::head(labels, 20), collapse = ', ')}")
}

# ---- 2d. CBS element overlap
cli::cli_h1("Deep-dive: CBS diagnostics")

g_elems <- sort(unique(global_cbs$element))
w_elems <- sort(unique(whep_cbs$element))
cli::cli_h2("Element overlap")
cli::cli_text(
  "Common:      {paste(intersect(g_elems, w_elems), collapse = ', ')}"
)
cli::cli_text(
  "Global-only: {paste(setdiff(g_elems, w_elems), collapse = ', ')}"
)
cli::cli_text(
  "WHEP-only:   {paste(setdiff(w_elems, g_elems), collapse = ', ')}"
)

# ---- 2e. CBS area overlap — by code
g_area_codes <- global_cbs |>
  dplyr::filter(year >= 1961L) |>
  dplyr::distinct(area_code) |>
  dplyr::pull()
w_area_codes <- whep_cbs |>
  dplyr::distinct(area_code) |>
  dplyr::pull()

cli::cli_h2("Area overlap (CBS, year >= 1961)")
cli::cli_text(
  "Global areas: {length(g_area_codes)}   WHEP areas: {length(w_area_codes)}   Common: {length(intersect(g_area_codes, w_area_codes))}"
)
g_only_areas <- setdiff(g_area_codes, w_area_codes)
w_only_areas <- setdiff(w_area_codes, g_area_codes)
if (length(g_only_areas) > 0) {
  labels <- paste0(.area_name(g_only_areas), " (", g_only_areas, ")")
  cli::cli_text("Global-only: {paste(labels, collapse = ', ')}")
}
if (length(w_only_areas) > 0) {
  labels <- paste0(.area_name(w_only_areas), " (", w_only_areas, ")")
  cli::cli_text("WHEP-only:   {paste(labels, collapse = ', ')}")
}

# ---- 2f. CBS mismatch breakdown by element
if ("value_whep" %in% names(result_cbs$merged)) {
  cli::cli_h2("CBS mismatches by element")
  cbs_merged <- result_cbs$merged
  cbs_mis <- cbs_merged |>
    dplyr::mutate(
      ratio = value_whep / value_global,
      abs_diff = abs(value_whep - value_global),
      rel_diff = dplyr::if_else(
        value_global != 0,
        abs_diff / abs(value_global),
        abs_diff
      )
    ) |>
    dplyr::filter(rel_diff >= rel_tol)

  if (nrow(cbs_mis) > 0) {
    cbs_mis |>
      dplyr::group_by(element) |>
      dplyr::summarise(
        n_mismatch = dplyr::n(),
        median_ratio = median(ratio[is.finite(ratio)], na.rm = TRUE),
        pct_near_1 = round(
          100 * mean(abs(ratio - 1) < 0.05, na.rm = TRUE), 1
        ),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(n_mismatch)) |>
      print()

    # Zero-vs-nonzero analysis
    zero_whep <- cbs_mis |>
      dplyr::filter(abs(value_whep) < 1e-8, abs(value_global) > 1)
    zero_global <- cbs_mis |>
      dplyr::filter(abs(value_global) < 1e-8, abs(value_whep) > 1)

    cli::cli_h3("Zero in WHEP, nonzero in Global: {nrow(zero_whep)} rows")
    if (nrow(zero_whep) > 0) {
      zero_whep |>
        dplyr::count(element) |>
        dplyr::arrange(dplyr::desc(n)) |>
        print()
    }
    cli::cli_h3("Zero in Global, nonzero in WHEP: {nrow(zero_global)} rows")
    if (nrow(zero_global) > 0) {
      zero_global |>
        dplyr::count(element) |>
        dplyr::arrange(dplyr::desc(n)) |>
        print()
    }
  }
}

# == 3. Sanity checks =========================================================

cli::cli_h1("Sanity checks")

cli::cli_h2("Year ranges")
cli::cli_text(
  "Global Primary: {min(global_primary$year)}\u2013{max(global_primary$year)}"
)
cli::cli_text(
  "WHEP Primary:   {min(whep_primary$year)}\u2013{max(whep_primary$year)}"
)
cli::cli_text(
  "Global CBS:     {min(global_cbs$year)}\u2013{max(global_cbs$year)}"
)
cli::cli_text("WHEP CBS:       {min(whep_cbs$year)}\u2013{max(whep_cbs$year)}")

cli::cli_h2("CBS elements")
cli::cli_text(
  "Global: {paste(sort(unique(global_cbs$element)), collapse = ', ')}"
)
cli::cli_text(
  "WHEP:   {paste(sort(unique(whep_cbs$element)),   collapse = ', ')}"
)

cli::cli_h2("Production units")
cli::cli_text(
  "Global: {paste(sort(unique(global_primary$unit)), collapse = ', ')}"
)
cli::cli_text(
  "WHEP:   {paste(sort(unique(whep_primary$unit)),   collapse = ', ')}"
)

cli::cli_alert_success("Comparison complete")
