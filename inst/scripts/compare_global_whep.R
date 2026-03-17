# compare_global_whep.R
# ─────────────────────────────────────────────────────────────
# End-to-end comparison of whep pipeline outputs against the
# Global CSV reference files.  Covers:
#
#   1.  Tolerance-based value comparison  (Primary, CBS, Coefs)
#   2.  Deep-dive diagnostics            (ratio patterns, name diffs)
#   3.  Sanity checks                    (year ranges, elements, units)
#
# Usage:
#   source("inst/scripts/compare_global_whep.R")
#
#   # --- or step by step: ---
#   # 1. Run whep pipeline (needs pins uploaded) and save:
#   #    primary <- whep::build_primary_production()
#   #    cbs     <- whep::build_commodity_balances(primary)
#   #    coefs   <- whep::build_processing_coefs(cbs)
#   #    saveRDS(primary, "inst/scripts/whep_primary.rds")
#   #    saveRDS(cbs,     "inst/scripts/whep_cbs.rds")
#   #    saveRDS(coefs,   "inst/scripts/whep_coefs.rds")
#   #
#   # 2. Then run this comparison:
#   #    source("inst/scripts/compare_global_whep.R")
# ─────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(cli)

# -- Configuration ------------------------------------------------------------

# Path to Global output CSVs
global_output <- "WHEP_LFILES_DIR/Global/output"

# Path to whep RDS files (if running offline, save whep outputs first)
whep_cache <- file.path(getwd(), "inst/scripts")

# Tolerance for numeric comparison (relative)
rel_tol <- 1e-6
# Tolerance for numeric comparison (absolute, for values near zero)
abs_tol <- 1e-4

# == Helpers ===================================================================

#' Harmonise Global column names & values to whep conventions
harmonise_global_primary <- function(df) {
  df |>
    dplyr::rename(year = Year, value = Value) |>
    dplyr::mutate(
      area_code = as.character(area_code),
      year = as.integer(year)
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
    dplyr::rename(year = Year, value = Value, element = Element) |>
    dplyr::mutate(
      area_code = as.character(area_code),
      year = as.integer(year),
      element = dplyr::recode(element, !!!elem_map)
    ) |>
    dplyr::as_tibble()
}

harmonise_global_coefs <- function(df) {
  df |>
    dplyr::rename(
      year = Year,
      value = Value,
      element = Element,
      processed_item = ProcessedItem,
      scaling = Scaling
    ) |>
    dplyr::mutate(
      area_code = as.character(area_code),
      year = as.integer(year),
      element = tolower(element)
    ) |>
    dplyr::as_tibble()
}

#' Generic tolerance-based comparison of two tibbles
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
  whep_keys <- whep_sub |>
    dplyr::select(dplyr::all_of(key_cols)) |>
    dplyr::distinct()
  global_keys <- global_sub |>
    dplyr::select(dplyr::all_of(key_cols)) |>
    dplyr::distinct()

  only_in_whep <- dplyr::anti_join(whep_keys, global_keys, by = key_cols)
  only_in_global <- dplyr::anti_join(global_keys, whep_keys, by = key_cols)

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
    by = key_cols
  )

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
      print(merged[idx, c(key_cols, w_col, g_col)] |> as.data.frame())
    }
  }

  # --- Summary verdict ---
  if (nrow(only_in_whep) == 0 && nrow(only_in_global) == 0 && n_differ == 0) {
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

# ---- Global reference ----
cli::cli_text("Reading Global CSVs from {global_output}...")

global_primary <- readr::read_csv(
  file.path(global_output, "Primary_all.csv"),
  show_col_types = FALSE
) |>
  harmonise_global_primary()

global_cbs <- readr::read_csv(
  file.path(global_output, "CBS.csv"),
  show_col_types = FALSE
) |>
  harmonise_global_cbs()

global_coefs <- readr::read_csv(
  file.path(global_output, "Processing_coefs.csv"),
  show_col_types = FALSE
) |>
  harmonise_global_coefs()

cli::cli_alert_success("Global data loaded")

# ---- WHEP outputs ----
# Try loading from RDS cache first; fall back to running the pipeline live
whep_primary_path <- file.path(whep_cache, "whep_primary.rds")
whep_cbs_path <- file.path(whep_cache, "whep_cbs.rds")
whep_coefs_path <- file.path(whep_cache, "whep_coefs.rds")

if (all(file.exists(c(whep_primary_path, whep_cbs_path, whep_coefs_path)))) {
  cli::cli_text("Reading WHEP outputs from cached RDS files...")
  whep_primary <- readRDS(whep_primary_path)
  whep_cbs <- readRDS(whep_cbs_path)
  whep_coefs <- readRDS(whep_coefs_path)
  cli::cli_alert_success("WHEP data loaded from cache")
} else {
  cli::cli_text("No cached RDS files found. Running WHEP pipeline live...")
  whep_primary <- whep::build_primary_production()
  whep_cbs <- whep::build_commodity_balances(whep_primary)
  whep_coefs <- whep::build_processing_coefs(whep_cbs)

  # Cache for next time
  saveRDS(whep_primary, whep_primary_path)
  saveRDS(whep_cbs, whep_cbs_path)
  saveRDS(whep_coefs, whep_coefs_path)
  cli::cli_alert_success("WHEP pipeline complete; outputs cached")
}

# == 1. Tolerance-based comparison =============================================

result_primary <- compare_outputs(
  whep_df = whep_primary,
  global_df = global_primary,
  key_cols = c(
    "year",
    "area",
    "area_code",
    "item_prod",
    "item_code_prod",
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
    "area",
    "area_code",
    "item_cbs",
    "item_code_cbs",
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
    "area",
    "area_code",
    "processed_item",
    "item_code_cbs",
    "item_cbs"
  ),
  value_cols = c(
    "value",
    "Product_fraction",
    "value_proc_raw",
    "scaling",
    "cf",
    "value_proc"
  ),
  label = "Processing Coefficients"
)

# == 2. Deep-dive diagnostics ==================================================
# Ratio-pattern analysis and name-difference checks migrated from the former
# run_comparison_deep.R script.

cli::cli_h1("Deep-dive: Primary mismatch patterns")

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
          abs(ratio - 1000) < 1 ~ "~1000x",
          abs(ratio - 0.001) < 0.0001 ~ "~0.001x",
          abs(ratio - 1) < 0.01 ~ "~1x (small diff)",
          TRUE ~ "other"
        )
      ) |>
      dplyr::count(ratio_bin) |>
      dplyr::arrange(dplyr::desc(n))

    cli::cli_h2("Ratio bins (whep / Global)")
    print(ratio_bins)

    # ---- 2b. Livestock vs non-livestock breakdown
    livestock_units <- c("heads", "LU", "t_head", "t_LU")

    livestock_mis <- prim_mis |>
      dplyr::filter(unit %in% livestock_units)
    other_mis <- prim_mis |>
      dplyr::filter(!unit %in% livestock_units)

    cli::cli_h2("Livestock mismatches ({nrow(livestock_mis)} rows)")
    if (nrow(livestock_mis) > 0) {
      livestock_mis |>
        dplyr::mutate(ratio_check = round(ratio, 2)) |>
        dplyr::count(ratio_check) |>
        dplyr::arrange(dplyr::desc(n)) |>
        utils::head(10) |>
        print()
    }

    cli::cli_h2("Non-livestock mismatches ({nrow(other_mis)} rows)")
    if (nrow(other_mis) > 0) {
      other_mis |>
        dplyr::count(unit) |>
        dplyr::arrange(dplyr::desc(n)) |>
        print()
      cli::cli_text("Sample non-livestock mismatches:")
      other_mis |> utils::head(10) |> print()
    }
  } else {
    cli::cli_alert_success("No primary mismatches to deep-dive into")
  }
}

# ---- 2c. Unmatched items (Primary)
cli::cli_h1("Deep-dive: Primary unmatched items")

g_items <- global_primary |> dplyr::distinct(item_prod) |> dplyr::pull()
w_items <- whep_primary |> dplyr::distinct(item_prod) |> dplyr::pull()

g_only_items <- setdiff(g_items, w_items)
w_only_items <- setdiff(w_items, g_items)

cli::cli_text("Global-only items: {length(g_only_items)}")
if (length(g_only_items) > 0) {
  cli::cli_text("{paste(utils::head(g_only_items, 20), collapse = ', ')}")
}

cli::cli_text("WHEP-only items:   {length(w_only_items)}")
if (length(w_only_items) > 0) {
  cli::cli_text("{paste(utils::head(w_only_items, 20), collapse = ', ')}")
}

# ---- 2d. CBS element overlap
cli::cli_h1("Deep-dive: CBS names")

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

# ---- 2e. CBS area overlap
g_areas <- global_cbs |>
  dplyr::filter(year >= 1961L) |>
  dplyr::distinct(area) |>
  dplyr::pull()
w_areas <- whep_cbs |> dplyr::distinct(area) |> dplyr::pull()

cli::cli_h2("Area overlap (CBS, year >= 1961)")
cli::cli_text(
  "Global areas: {length(g_areas)}   WHEP areas: {length(w_areas)}   Common: {length(intersect(g_areas, w_areas))}"
)
g_only_areas <- setdiff(g_areas, w_areas)
w_only_areas <- setdiff(w_areas, g_areas)
if (length(g_only_areas) > 0) {
  cli::cli_text(
    "Global-only: {paste(utils::head(g_only_areas, 20), collapse = ', ')}"
  )
}
if (length(w_only_areas) > 0) {
  cli::cli_text(
    "WHEP-only:   {paste(utils::head(w_only_areas, 20), collapse = ', ')}"
  )
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
