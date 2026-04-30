# diagnose_trade_imputation.R
# ─────────────────────────────────────────────────────────────
# Diagnostic script to validate imputed trade values (exports
# and imports) against the rest of each time series.
#
# Produces faceted ggplot figures showing:
#   - Original vs imputed export/import values over time
#   - Comparison with domestic supply and production
#   - Flagging of suspect imputed values
#
# Requires: whep loaded, ggplot2 installed
# ─────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(ggplot2)
library(cli)

# == 1. Build CBS with source tracking ========================================

cli::cli_h1("Building CBS pipeline")
primary <- whep::build_primary_production()
cbs <- whep::build_commodity_balances(primary)

# The CBS output is wide: year, area_code, item_cbs_code, + element columns
# We need to identify which export/import values were imputed.
# Since .cbs_impute_trade() fills NA trade with DS-production residual,
# imputed values are those where the original FAOSTAT had no trade data.
# We re-run the read step to get the pre-imputation state.

cli::cli_h1("Reading pre-imputation CBS for source comparison")
cbs_raw <- whep:::.read_cbs(primary, 1961L, 2021L)

# Extract original trade values (before imputation)
original_trade <- cbs_raw |>
  dplyr::filter(element %in% c("import", "export")) |>
  dplyr::select(year, area_code, item_cbs_code, element, value_original = value)

# Final CBS in long format
cbs_long <- cbs |>
  dplyr::mutate(stock_variation = -stock_retrieval, .keep = "unused") |>
  tidyr::pivot_longer(
    cols = -c(year, area_code, item_cbs_code),
    names_to = "element",
    values_to = "value"
  )

# Merge to identify imputed trade
trade_comparison <- cbs_long |>
  dplyr::filter(element %in% c("import", "export")) |>
  dplyr::left_join(
    original_trade,
    by = c("year", "area_code", "item_cbs_code", "element")
  ) |>
  dplyr::mutate(
    is_imputed = is.na(value_original) & value != 0,
    value_final = value
  )

# == 2. Summary statistics ====================================================

cli::cli_h1("Trade imputation summary")

trade_summary <- trade_comparison |>
  dplyr::group_by(element) |>
  dplyr::summarise(
    total_rows = dplyr::n(),
    n_imputed = sum(is_imputed, na.rm = TRUE),
    n_original = sum(!is_imputed, na.rm = TRUE),
    pct_imputed = round(100 * n_imputed / total_rows, 1),
    median_imputed = median(value_final[is_imputed], na.rm = TRUE),
    median_original = median(value_final[!is_imputed], na.rm = TRUE),
    .groups = "drop"
  )

print(trade_summary)

# == 3. Coherence check: compare imputed exports with series context ==========

cli::cli_h1("Checking imputed trade coherence")

# Get production and DS for context
balance_elements <- cbs_long |>
  dplyr::filter(element %in% c("production", "domestic_supply")) |>
  tidyr::pivot_wider(names_from = element, values_from = value)

trade_with_context <- trade_comparison |>
  dplyr::filter(is_imputed) |>
  dplyr::left_join(
    balance_elements,
    by = c("year", "area_code", "item_cbs_code")
  )

# Flag suspect: imputed export > production (possible overestimation)
suspect_exports <- trade_with_context |>
  dplyr::filter(
    element == "export",
    value_final > production,
    production > 0
  ) |>
  dplyr::mutate(ratio = value_final / production)

cli::cli_text(
  "Imputed exports exceeding production: {nrow(suspect_exports)} rows"
)

if (nrow(suspect_exports) > 0) {
  suspect_exports |>
    dplyr::group_by(area_code) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_ratio = median(ratio, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n)) |>
    utils::head(15) |>
    print()
}

# Flag suspect: imputed import > domestic_supply * 2
suspect_imports <- trade_with_context |>
  dplyr::filter(
    element == "import",
    value_final > domestic_supply * 2,
    domestic_supply > 0
  ) |>
  dplyr::mutate(ratio = value_final / domestic_supply)

cli::cli_text(
  "Imputed imports > 2x domestic supply: {nrow(suspect_imports)} rows"
)

# == 4. Visualization: example series =========================================

cli::cli_h1("Generating diagnostic plots")

# Helper: item name lookup
items_lookup <- whep::items_full |>
  dplyr::select(item_cbs_code, item_cbs) |>
  dplyr::distinct()
polities_lookup <- whep::polities |>
  dplyr::select(area_code, area_name) |>
  dplyr::distinct()

# Pick the top affected country-item combinations (most imputed export rows)
top_cases <- trade_comparison |>
  dplyr::filter(is_imputed, element == "export") |>
  dplyr::count(area_code, item_cbs_code, sort = TRUE) |>
  utils::head(12)

plot_data <- cbs_long |>
  dplyr::inner_join(
    top_cases |> dplyr::select(area_code, item_cbs_code),
    by = c("area_code", "item_cbs_code")
  ) |>
  dplyr::filter(
    element %in% c("production", "domestic_supply", "export", "import")
  ) |>
  dplyr::left_join(items_lookup, by = "item_cbs_code") |>
  dplyr::left_join(polities_lookup, by = "area_code") |>
  dplyr::mutate(
    panel = paste0(area_name, " - ", item_cbs)
  )

# Add imputation flag for export/import
plot_data <- plot_data |>
  dplyr::left_join(
    trade_comparison |>
      dplyr::select(year, area_code, item_cbs_code, element, is_imputed),
    by = c("year", "area_code", "item_cbs_code", "element")
  ) |>
  dplyr::mutate(
    is_imputed = tidyr::replace_na(is_imputed, FALSE)
  )

p1 <- ggplot(
  plot_data,
  aes(x = year, y = value, colour = element)
) +
  geom_line(linewidth = 0.5) +
  geom_point(
    data = plot_data |> dplyr::filter(is_imputed),
    aes(x = year, y = value),
    shape = 4,
    size = 1.5
  ) +
  facet_wrap(~panel, scales = "free_y", ncol = 3) +
  scale_colour_manual(
    values = c(
      production = "darkgreen",
      domestic_supply = "steelblue",
      export = "firebrick",
      import = "darkorange"
    )
  ) +
  labs(
    title = "CBS series with imputed trade (x = imputed values)",
    x = "Year",
    y = "Tonnes",
    colour = "Element"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 7)
  )

# Save
output_dir <- file.path("inst", "scripts")
ggsave(
  file.path(output_dir, "diagnostic_trade_imputation.png"),
  p1,
  width = 14,
  height = 10,
  dpi = 150
)
cli::cli_alert_success(
  "Saved: {file.path(output_dir, 'diagnostic_trade_imputation.png')}"
)

# == 5. Series-level coherence: ratio of imputed vs original trade ============

# For series that have BOTH original and imputed trade years,
# compare the magnitude
mixed_series <- trade_comparison |>
  dplyr::group_by(area_code, item_cbs_code, element) |>
  dplyr::summarise(
    n_original = sum(!is_imputed & value_final != 0),
    n_imputed = sum(is_imputed),
    mean_original = mean(
      value_final[!is_imputed & value_final != 0],
      na.rm = TRUE
    ),
    mean_imputed = mean(value_final[is_imputed], na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(n_original > 3, n_imputed > 0) |>
  dplyr::mutate(
    ratio = mean_imputed / mean_original,
    suspect = abs(log10(ratio)) > 1
  )

cli::cli_h2("Imputed/original ratio distribution (series with both)")
cli::cli_text("Total mixed series: {nrow(mixed_series)}")
cli::cli_text(
  "Suspect (ratio > 10x or < 0.1x): {sum(mixed_series$suspect, na.rm = TRUE)}"
)

p2 <- ggplot(
  mixed_series |> dplyr::filter(is.finite(ratio), ratio > 0),
  aes(x = log10(ratio), fill = element)
) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Imputed / original trade ratio (log10 scale)",
    subtitle = "0 = imputed matches original magnitude; outliers suggest problems",
    x = "log10(mean_imputed / mean_original)",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  file.path(output_dir, "diagnostic_trade_ratio.png"),
  p2,
  width = 10,
  height = 6,
  dpi = 150
)
cli::cli_alert_success(
  "Saved: {file.path(output_dir, 'diagnostic_trade_ratio.png')}"
)

cli::cli_alert_success("Trade imputation diagnostic complete")
