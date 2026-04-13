# diagnose_processing_coefs.R
# Check processing coefficient coherence:
# 1. Within-series: does scaling jump wildly between years?
# 2. Cross-country: how does a country's coefficient compare to global?

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
  library(ggplot2)
})

cat("Building pipeline...\n")
primary <- whep::build_primary_production()
cbs <- whep::build_commodity_balances(primary)
coefs <- whep::build_processing_coefs(cbs)

items_lookup <- whep::items_full |>
  dplyr::select(item_cbs_code, item_cbs) |>
  dplyr::distinct()
polities_lookup <- whep::polities |>
  dplyr::select(area_code, area_name) |>
  dplyr::distinct()

coefs <- coefs |>
  dplyr::left_join(
    items_lookup |> dplyr::rename(item_to_process = item_cbs),
    by = c("item_cbs_code_to_process" = "item_cbs_code")
  ) |>
  dplyr::left_join(
    items_lookup |> dplyr::rename(item_processed = item_cbs),
    by = c("item_cbs_code_processed" = "item_cbs_code")
  ) |>
  dplyr::left_join(polities_lookup, by = "area_code")

# == 1. Scaling factor distribution ============================================

cat("\n=== Conversion factor scaling distribution ===\n")
cat("Total rows:", nrow(coefs), "\n")
cat("Rows with scaling != 0:", sum(coefs$conversion_factor_scaling != 0), "\n")
print(quantile(
  coefs$conversion_factor_scaling[coefs$conversion_factor_scaling > 0],
  c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
  na.rm = TRUE
))

# Extreme scaling (>5 or <0.2)
extreme <- coefs |>
  dplyr::filter(
    conversion_factor_scaling > 5 | conversion_factor_scaling < 0.2,
    conversion_factor_scaling > 0,
    year >= 1961
  )
cat("\nExtreme scaling (>5x or <0.2x):", nrow(extreme), "rows\n")

cat("\n=== Top items with extreme scaling ===\n")
extreme |>
  dplyr::count(item_to_process, item_cbs_code_to_process, sort = TRUE) |>
  utils::head(15) |>
  print()

# == 2. Year-to-year jumps =====================================================

cat("\n=== Year-to-year scaling jumps ===\n")
jumps <- coefs |>
  dplyr::filter(year >= 1961) |>
  dplyr::arrange(
    area_code,
    item_cbs_code_to_process,
    item_cbs_code_processed,
    year
  ) |>
  dplyr::group_by(
    area_code,
    item_cbs_code_to_process,
    item_cbs_code_processed
  ) |>
  dplyr::mutate(
    prev_cf = dplyr::lag(final_conversion_factor),
    cf_ratio = final_conversion_factor / prev_cf
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    is.finite(cf_ratio),
    cf_ratio > 0,
    abs(log10(cf_ratio)) > 1
  )

cat("Series with >10x year-to-year CF jump:", nrow(jumps), "\n")

cat("\n=== Top 20 jumps by magnitude ===\n")
jumps |>
  dplyr::arrange(dplyr::desc(abs(log10(cf_ratio)))) |>
  dplyr::select(
    year,
    area_name,
    item_to_process,
    item_processed,
    prev_cf,
    final_conversion_factor,
    cf_ratio
  ) |>
  utils::head(20) |>
  print()

# == 3. Cross-country comparison ===============================================

cat("\n=== Cross-country coefficient comparison ===\n")
# Global median CF per item pair
global_cf <- coefs |>
  dplyr::filter(year >= 1990, final_conversion_factor > 0) |>
  dplyr::group_by(item_cbs_code_to_process, item_cbs_code_processed) |>
  dplyr::summarise(
    global_median_cf = median(final_conversion_factor, na.rm = TRUE),
    global_mad_cf = mad(final_conversion_factor, na.rm = TRUE),
    n_countries = dplyr::n_distinct(area_code),
    .groups = "drop"
  )

# Country CFs that deviate > 5x from global median
outliers <- coefs |>
  dplyr::filter(year >= 1990, final_conversion_factor > 0) |>
  dplyr::group_by(
    area_code,
    area_name,
    item_cbs_code_to_process,
    item_to_process,
    item_cbs_code_processed,
    item_processed
  ) |>
  dplyr::summarise(
    country_median_cf = median(final_conversion_factor, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    global_cf,
    by = c("item_cbs_code_to_process", "item_cbs_code_processed")
  ) |>
  dplyr::mutate(
    ratio_to_global = country_median_cf / global_median_cf
  ) |>
  dplyr::filter(
    is.finite(ratio_to_global),
    abs(log10(ratio_to_global)) > log10(5)
  )

cat("Country-item pairs with CF >5x off global median:", nrow(outliers), "\n")
cat("\n=== Top 20 outliers ===\n")
outliers |>
  dplyr::arrange(dplyr::desc(abs(log10(ratio_to_global)))) |>
  dplyr::select(
    area_name,
    item_to_process,
    item_processed,
    country_median_cf,
    global_median_cf,
    ratio_to_global
  ) |>
  utils::head(20) |>
  print()

# == 4. Plot: scaling factor over time for top items ===========================

top_items <- coefs |>
  dplyr::filter(year >= 1961) |>
  dplyr::count(item_to_process, item_cbs_code_to_process, sort = TRUE) |>
  utils::head(6)

plot_data <- coefs |>
  dplyr::filter(
    item_cbs_code_to_process %in% top_items$item_cbs_code_to_process,
    year >= 1961,
    conversion_factor_scaling > 0,
    conversion_factor_scaling < 10
  )

p <- ggplot(plot_data, aes(x = year, y = conversion_factor_scaling)) +
  geom_point(alpha = 0.05, size = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  facet_wrap(~item_to_process, scales = "free_y") +
  labs(
    title = "Processing coefficient scaling factor over time",
    subtitle = "Each point = one country-year. Red line = no scaling needed",
    x = "Year",
    y = "Scaling factor"
  ) +
  theme_minimal(base_size = 10)

ggsave(
  "inst/scripts/diagnostic_processing_coefs.png",
  p,
  width = 12,
  height = 8,
  dpi = 150
)

cat("\nSaved: inst/scripts/diagnostic_processing_coefs.png\n")
cat("Done.\n")
