# diagnose_stock_variation.R
# Check stock variation coherence: is it within plausible bounds?

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

cat("Building pipeline...\n")
primary <- whep::build_primary_production()
cbs <- whep::build_commodity_balances(primary)

# Wide format with all elements
cbs_wide <- cbs |>
  dplyr::mutate(stock_variation = -stock_retrieval) |>
  dplyr::select(-stock_retrieval)

# Compute SV as share of domestic supply
cbs_wide <- cbs_wide |>
  dplyr::mutate(
    sv_ds_ratio = dplyr::if_else(
      domestic_supply != 0,
      abs(stock_variation) / abs(domestic_supply),
      NA_real_
    ),
    sv_prod_ratio = dplyr::if_else(
      production != 0,
      abs(stock_variation) / production,
      NA_real_
    )
  )

cat("\n=== Stock variation as share of domestic supply ===\n")
cat(
  "Rows with SV != 0:",
  sum(cbs_wide$stock_variation != 0, na.rm = TRUE),
  "\n"
)
cat("Rows with |SV/DS| > 1:", sum(cbs_wide$sv_ds_ratio > 1, na.rm = TRUE), "\n")
cat("Rows with |SV/DS| > 5:", sum(cbs_wide$sv_ds_ratio > 5, na.rm = TRUE), "\n")

cat("\nQuantiles of |SV/DS| (where both nonzero):\n")
print(quantile(
  cbs_wide$sv_ds_ratio[cbs_wide$sv_ds_ratio > 0],
  c(0.5, 0.75, 0.9, 0.95, 0.99, 1),
  na.rm = TRUE
))

cat("\n=== Stock variation as share of production ===\n")
cat(
  "Rows with |SV/prod| > 0.5:",
  sum(cbs_wide$sv_prod_ratio > 0.5, na.rm = TRUE),
  "\n"
)
print(quantile(
  cbs_wide$sv_prod_ratio[cbs_wide$sv_prod_ratio > 0],
  c(0.5, 0.75, 0.9, 0.95, 0.99, 1),
  na.rm = TRUE
))

# Top offenders
items_lookup <- whep::items_full |>
  dplyr::select(item_cbs_code, item_cbs) |>
  dplyr::distinct()
polities_lookup <- whep::polities |>
  dplyr::select(area_code, area_name) |>
  dplyr::distinct()

cat("\n=== Top 20 rows by |SV/DS| ratio ===\n")
cbs_wide |>
  dplyr::filter(sv_ds_ratio > 5, year >= 1961) |>
  dplyr::left_join(items_lookup, by = "item_cbs_code") |>
  dplyr::left_join(polities_lookup, by = "area_code") |>
  dplyr::arrange(dplyr::desc(sv_ds_ratio)) |>
  dplyr::select(
    year,
    area_name,
    item_cbs,
    production,
    domestic_supply,
    stock_variation,
    sv_ds_ratio
  ) |>
  utils::head(20) |>
  print()

# Distribution plot
p <- ggplot(
  cbs_wide |>
    dplyr::filter(sv_ds_ratio > 0, sv_ds_ratio < 10, year >= 1961),
  aes(x = sv_ds_ratio)
) +
  geom_histogram(bins = 100, fill = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "red") +
  labs(
    title = "|Stock variation / Domestic supply| distribution",
    subtitle = "Values > 1 mean SV exceeds DS (red line)",
    x = "|SV / DS|",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  "inst/scripts/diagnostic_stock_variation.png",
  p,
  width = 10,
  height = 6,
  dpi = 150
)

cat("\nSaved: inst/scripts/diagnostic_stock_variation.png\n")
cat("Done.\n")
