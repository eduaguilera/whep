# analyse_trade_impact.R
# Quick analysis: what items/countries/tonnes are affected by trade imputation?

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
  library(tidyr)
})

cat("Building pipeline...\n")
primary <- whep::build_primary_production()
cbs <- whep::build_commodity_balances(primary)
cbs_raw <- whep:::.read_cbs(primary, 1961L, 2021L)

cat("Computing trade comparison...\n")
original_trade <- cbs_raw |>
  dplyr::filter(element %in% c("import", "export")) |>
  dplyr::select(year, area_code, item_cbs_code, element, value_original = value)

cbs_long <- cbs |>
  dplyr::mutate(stock_variation = -stock_retrieval, .keep = "unused") |>
  tidyr::pivot_longer(
    -c(year, area_code, item_cbs_code),
    names_to = "element",
    values_to = "value"
  )

tc <- cbs_long |>
  dplyr::filter(element %in% c("import", "export"), year >= 1961) |>
  dplyr::left_join(
    original_trade,
    by = c("year", "area_code", "item_cbs_code", "element")
  ) |>
  dplyr::mutate(is_imputed = is.na(value_original) & value != 0)

# ── 1. Share of total trade TONNES ──
cat("\n=== Share of total trade TONNES imputed ===\n")
tc |>
  dplyr::group_by(element, is_imputed) |>
  dplyr::summarise(total_t = sum(value, na.rm = TRUE), .groups = "drop") |>
  dplyr::group_by(element) |>
  dplyr::mutate(pct = round(100 * total_t / sum(total_t), 2)) |>
  print(n = Inf)

items_lookup <- whep::items_full |>
  dplyr::select(item_cbs_code, item_cbs) |>
  dplyr::distinct()
polities_lookup <- whep::polities |>
  dplyr::select(area_code, area_name) |>
  dplyr::distinct()

# ── 2. Top items ──
cat("\n=== Top 20 items by imputed trade tonnes ===\n")
tc |>
  dplyr::filter(is_imputed) |>
  dplyr::left_join(items_lookup, by = "item_cbs_code") |>
  dplyr::group_by(item_cbs) |>
  dplyr::summarise(
    imp_t = sum(value, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(imp_t)) |>
  utils::head(20) |>
  print()

# ── 3. Top countries ──
cat("\n=== Top 20 countries by imputed trade tonnes ===\n")
tc |>
  dplyr::filter(is_imputed) |>
  dplyr::left_join(polities_lookup, by = "area_code") |>
  dplyr::group_by(area_name) |>
  dplyr::summarise(
    imp_t = sum(value, na.rm = TRUE),
    n = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(imp_t)) |>
  utils::head(20) |>
  print()

# ── 4. Suspect series ──
cat("\n=== Suspect series (ratio>10x or <0.1x), top 25 by imputed tonnes ===\n")
mix <- tc |>
  dplyr::group_by(area_code, item_cbs_code, element) |>
  dplyr::summarise(
    n_orig = sum(!is_imputed & value != 0),
    mean_orig = mean(value[!is_imputed & value != 0], na.rm = TRUE),
    imp_t = sum(value[is_imputed], na.rm = TRUE),
    mean_imp = mean(value[is_imputed], na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(n_orig > 3, imp_t > 0) |>
  dplyr::mutate(ratio = mean_imp / mean_orig) |>
  dplyr::filter(is.finite(ratio), ratio > 10 | ratio < 0.1) |>
  dplyr::left_join(items_lookup, by = "item_cbs_code") |>
  dplyr::left_join(polities_lookup, by = "area_code") |>
  dplyr::arrange(dplyr::desc(imp_t)) |>
  dplyr::select(area_name, item_cbs, element, n_orig, ratio, imp_t)
print(utils::head(mix, 25), n = 25)

cat("\nDone.\n")
