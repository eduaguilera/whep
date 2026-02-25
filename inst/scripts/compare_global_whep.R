# compare_global_whep.R
# Compare outputs of whep::build_primary_production() and
# whep::build_commodity_balances() with Global CSV reference files.
#
# Usage:
#   source("inst/scripts/compare_global_whep.R")
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

library(dplyr)
library(tidyr)
library(cli)

# -- Configuration ------------------------------------------------------------

# Path to Global output CSVs
global_output <- "C:/Users/53272530E/OneDrive/L_files/Global/output"

# Path to whep RDS files (if running offline, save whep outputs first)
whep_cache <- file.path(getwd(), "inst/scripts")

# Tolerance for numeric comparison (relative)
rel_tol <- 1e-6
# Tolerance for numeric comparison (absolute, for values near zero)
abs_tol <- 1e-4

# -- Helpers -------------------------------------------------------------------

#' Harmonise Global column names & values to whep conventions
harmonise_global_primary <- function(df) {
  df |>
    dplyr::rename(
      year  = Year,
      value = Value
    ) |>
    dplyr::mutate(
      area_code = as.character(area_code),
      year      = as.integer(year)
    ) |>
    dplyr::as_tibble()
}

harmonise_global_cbs <- function(df) {
  elem_map <- c(
    "Domestic_supply"    = "domestic_supply",
    "Production"         = "production",
    "Export"             = "export",
    "Import"             = "import",
    "Stock_variation"    = "stock_variation",
    "Food"               = "food",
    "Feed"               = "feed",
    "Seed"               = "seed",
    "Other_uses"         = "other_uses",
    "Processing"         = "processing",
    "Processing_primary" = "processing_primary"
  )
  df |>
    dplyr::rename(
      year    = Year,
      value   = Value,
      element = Element
    ) |>
    dplyr::mutate(
      area_code = as.character(area_code),
      year      = as.integer(year),
      element   = dplyr::recode(element, !!!elem_map)
    ) |>
    dplyr::as_tibble()
}

harmonise_global_coefs <- function(df) {
  df |>
    dplyr::rename(
      year           = Year,
      value          = Value,
      element        = Element,
      processed_item = ProcessedItem,
      scaling        = Scaling
    ) |>
    dplyr::mutate(
      area_code = as.character(area_code),
      year      = as.integer(year),
      element   = tolower(element)
    ) |>
    dplyr::as_tibble()
}

#' Compare two tibbles by key columns
compare_outputs <- function(
    whep_df, global_df,
    key_cols, value_cols,
    label = "dataset"
) {
  cli::cli_h1("Comparing {label}")

  # --- Dimensions ---
  cli::cli_text("WHEP: {nrow(whep_df)} rows x {ncol(whep_df)} cols")
  cli::cli_text("Global: {nrow(global_df)} rows x {ncol(global_df)} cols")

  # --- Column check ---
  common_cols <- intersect(names(whep_df), names(global_df))
  only_whep   <- setdiff(names(whep_df), names(global_df))
  only_global <- setdiff(names(global_df), names(whep_df))
  if (length(only_whep) > 0)
    cli::cli_alert_info("Columns only in WHEP: {paste(only_whep, collapse=', ')}")
  if (length(only_global) > 0)
    cli::cli_alert_info("Columns only in Global: {paste(only_global, collapse=', ')}")

  # --- Restrict to shared columns ---
  shared <- intersect(common_cols, c(key_cols, value_cols))
  whep_sub   <- whep_df   |> dplyr::select(dplyr::all_of(shared))
  global_sub <- global_df |> dplyr::select(dplyr::all_of(shared))

  # --- Key coverage ---
  whep_keys   <- whep_sub   |> dplyr::select(dplyr::all_of(key_cols)) |> dplyr::distinct()
  global_keys <- global_sub |> dplyr::select(dplyr::all_of(key_cols)) |> dplyr::distinct()

  only_in_whep   <- dplyr::anti_join(whep_keys, global_keys, by = key_cols)
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
    whep_sub   |> dplyr::rename_with(~ paste0(.x, "_whep"),   dplyr::all_of(val_cols_present)),
    global_sub |> dplyr::rename_with(~ paste0(.x, "_global"), dplyr::all_of(val_cols_present)),
    by = key_cols
  )

  cli::cli_text("Matched rows: {nrow(merged)}")

  for (vc in val_cols_present) {
    w_col <- paste0(vc, "_whep")
    g_col <- paste0(vc, "_global")

    w <- merged[[w_col]]
    g <- merged[[g_col]]

    abs_diff <- abs(w - g)
    denom    <- pmax(abs(g), abs_tol)
    rel_diff <- abs_diff / denom

    n_exact    <- sum(w == g,               na.rm = TRUE)
    n_close    <- sum(rel_diff <= rel_tol,  na.rm = TRUE)
    n_differ   <- sum(rel_diff > rel_tol,   na.rm = TRUE)
    n_na_whep  <- sum(is.na(w) & !is.na(g))
    n_na_glob  <- sum(!is.na(w) & is.na(g))
    max_rel    <- if (n_differ > 0) max(rel_diff[rel_diff > rel_tol], na.rm = TRUE) else 0
    max_abs    <- if (n_differ > 0) max(abs_diff[rel_diff > rel_tol], na.rm = TRUE) else 0

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
    merged         = merged,
    only_in_whep   = only_in_whep,
    only_in_global = only_in_global
  ))
}

# -- Load data -----------------------------------------------------------------

cli::cli_h1("Loading data")

# ---- Global reference ----
cli::cli_text("Reading Global CSVs from {global_output}...")

global_primary <- data.table::fread(
  file.path(global_output, "Primary_all.csv")
) |> harmonise_global_primary()

global_cbs <- data.table::fread(
  file.path(global_output, "CBS.csv")
) |> harmonise_global_cbs()

global_coefs <- data.table::fread(
  file.path(global_output, "Processing_coefs.csv")
) |> harmonise_global_coefs()

cli::cli_alert_success("Global data loaded")

# ---- WHEP outputs ----
# Try loading from RDS cache first; fall back to running the pipeline live
whep_primary_path <- file.path(whep_cache, "whep_primary.rds")
whep_cbs_path     <- file.path(whep_cache, "whep_cbs.rds")
whep_coefs_path   <- file.path(whep_cache, "whep_coefs.rds")

if (all(file.exists(c(whep_primary_path, whep_cbs_path, whep_coefs_path)))) {
  cli::cli_text("Reading WHEP outputs from cached RDS files...")
  whep_primary <- readRDS(whep_primary_path)
  whep_cbs     <- readRDS(whep_cbs_path)
  whep_coefs   <- readRDS(whep_coefs_path)
  cli::cli_alert_success("WHEP data loaded from cache")
} else {
  cli::cli_text("No cached RDS files found. Running WHEP pipeline live...")
  whep_primary <- whep::build_primary_production()
  whep_cbs     <- whep::build_commodity_balances(whep_primary)
  whep_coefs   <- whep::build_processing_coefs(whep_cbs)

  # Cache for next time
  saveRDS(whep_primary, whep_primary_path)
  saveRDS(whep_cbs,     whep_cbs_path)
  saveRDS(whep_coefs,   whep_coefs_path)
  cli::cli_alert_success("WHEP pipeline complete; outputs cached")
}

# -- Compare -------------------------------------------------------------------

result_primary <- compare_outputs(
  whep_df    = whep_primary,
  global_df  = global_primary,
  key_cols   = c("year", "area", "area_code", "item_prod",
                 "item_code_prod", "unit"),
  value_cols = c("value"),
  label      = "Primary Production"
)

result_cbs <- compare_outputs(
  whep_df    = whep_cbs,
  global_df  = global_cbs,
  key_cols   = c("year", "area", "area_code", "item_cbs",
                 "item_code_cbs", "element"),
  value_cols = c("value"),
  label      = "Commodity Balance Sheets"
)

result_coefs <- compare_outputs(
  whep_df    = whep_coefs,
  global_df  = global_coefs,
  key_cols   = c("year", "area", "area_code", "processed_item",
                 "item_code_cbs", "item_cbs"),
  value_cols = c("value", "Product_fraction", "value_proc_raw",
                 "scaling", "cf", "value_proc"),
  label      = "Processing Coefficients"
)

# -- Year-range & element sanity checks ----------------------------------------

cli::cli_h1("Sanity checks")

cli::cli_h2("Year ranges")
cli::cli_text("Global Primary: {min(global_primary$year)}–{max(global_primary$year)}")
cli::cli_text("WHEP Primary:   {min(whep_primary$year)}–{max(whep_primary$year)}")
cli::cli_text("Global CBS:     {min(global_cbs$year)}–{max(global_cbs$year)}")
cli::cli_text("WHEP CBS:       {min(whep_cbs$year)}–{max(whep_cbs$year)}")

cli::cli_h2("CBS elements")
cli::cli_text("Global: {paste(sort(unique(global_cbs$element)), collapse=', ')}")
cli::cli_text("WHEP:   {paste(sort(unique(whep_cbs$element)),   collapse=', ')}")

cli::cli_h2("Production units")
cli::cli_text("Global: {paste(sort(unique(global_primary$unit)), collapse=', ')}")
cli::cli_text("WHEP:   {paste(sort(unique(whep_primary$unit)),   collapse=', ')}")

cli::cli_alert_success("Comparison complete")
