# analyse_trade_vs_faostat.R
# Compare WHEP CBS trade values against standalone FAOSTAT trade dataset
# (Trade_Crops_Livestock from L_files)

suppressPackageStartupMessages({
  devtools::load_all(".")
  library(data.table)
  library(dplyr)
})

# == 1. Read FAOSTAT trade (standalone) ========================================

lfiles <- Sys.getenv("WHEP_LFILES")
fao_trade_path <- file.path(
  lfiles,
  "FAOSTAT",
  "Trade_Crops_Livestock_E_All_Data_(Normalized).csv"
)
cat("Reading FAOSTAT trade from L_files...\n")
fao_trade <- data.table::fread(
  fao_trade_path,
  select = c("Area Code", "Item Code", "Element", "Year", "Value", "Unit"),
  showProgress = FALSE
)
setnames(
  fao_trade,
  c("Area Code", "Item Code", "Element", "Year", "Value", "Unit"),
  c("area_code", "item_code", "element", "year", "value_fao", "unit")
)

# Keep only import/export quantity in tonnes
fao_trade <- fao_trade[
  element %in% c("Import quantity", "Export quantity") & unit == "t"
]
fao_trade[,
  element := fifelse(
    element == "Import quantity",
    "import",
    "export"
  )
]
fao_trade[, c("unit") := NULL]

# Map item_code to item_cbs_code via cbs_trade_codes
cbs_tc <- data.table::as.data.table(whep::cbs_trade_codes)
# cbs_trade_codes has: item_code_trade -> item_cbs (name)
# We need to get item_cbs_code from items_full
items_bridge <- data.table::as.data.table(whep::items_full)[,
  .(item_cbs, item_cbs_code)
]
items_bridge <- unique(items_bridge, by = "item_cbs")

trade_to_cbs <- merge(
  cbs_tc[, .(item_code_trade, item_cbs)],
  items_bridge,
  by = "item_cbs",
  all.x = TRUE
)
trade_to_cbs <- unique(trade_to_cbs[
  !is.na(item_cbs_code),
  .(item_code = as.integer(item_code_trade), item_cbs_code)
])

fao_trade <- merge(fao_trade, trade_to_cbs, by = "item_code", all.x = TRUE)
# Drop items without CBS mapping
fao_trade <- fao_trade[!is.na(item_cbs_code)]

# Aggregate to CBS item level (multiple prod items -> one CBS item)
fao_agg <- fao_trade[,
  .(value_fao = sum(value_fao, na.rm = TRUE)),
  by = .(year, area_code, item_cbs_code, element)
]

# Also map area_code to polity (aggregate sub-regions)
polity_bridge <- data.table::as.data.table(whep::regions_full)[,
  .(area_code = code, polity_code)
]
pol_codes <- data.table::as.data.table(whep::polities)[,
  .(polity_code = iso3c, polity_area_code = area_code)
]
bridge <- merge(polity_bridge, pol_codes, by = "polity_code", all.x = TRUE)

fao_agg <- merge(
  fao_agg,
  bridge[, .(area_code, polity_area_code)],
  by = "area_code",
  all.x = TRUE
)
fao_agg[!is.na(polity_area_code), area_code := polity_area_code]
fao_agg[, polity_area_code := NULL]
fao_agg <- fao_agg[,
  .(value_fao = sum(value_fao, na.rm = TRUE)),
  by = .(year, area_code, item_cbs_code, element)
]

cat("FAOSTAT trade rows (aggregated to CBS items):", nrow(fao_agg), "\n")

# == 2. Get WHEP CBS trade ====================================================

cat("Building WHEP pipeline...\n")
primary <- whep::build_primary_production()
cbs <- whep::build_commodity_balances(primary)

cbs_long <- as.data.table(cbs)
cbs_long[, stock_variation := -stock_retrieval]
cbs_long[, stock_retrieval := NULL]
cbs_long <- melt(
  cbs_long,
  id.vars = c("year", "area_code", "item_cbs_code"),
  variable.name = "element",
  value.name = "value_whep"
)
cbs_trade <- cbs_long[element %in% c("import", "export") & year >= 1961]

# == 3. Merge and compare =====================================================

cat("Merging...\n")
comp <- merge(
  cbs_trade,
  fao_agg,
  by = c("year", "area_code", "item_cbs_code", "element"),
  all = TRUE
)

comp[is.na(value_whep), value_whep := 0]
comp[is.na(value_fao), value_fao := 0]

# Classification
comp[, `:=`(
  both_zero = value_whep == 0 & value_fao == 0,
  whep_only = value_whep != 0 & value_fao == 0,
  fao_only = value_whep == 0 & value_fao != 0,
  both_nonzero = value_whep != 0 & value_fao != 0
)]

cat("\n=== Coverage summary ===\n")
cat("Total rows:", nrow(comp), "\n")
cat("Both zero:", sum(comp$both_zero), "\n")
cat("Both nonzero:", sum(comp$both_nonzero), "\n")
cat("WHEP only (no FAOSTAT):", sum(comp$whep_only), "\n")
cat("FAOSTAT only (no WHEP):", sum(comp$fao_only), "\n")

cat("\n=== By element ===\n")
print(comp[,
  .(
    both_nonzero = sum(both_nonzero),
    whep_only = sum(whep_only),
    fao_only = sum(fao_only),
    whep_tonnes = sum(value_whep),
    fao_tonnes = sum(value_fao)
  ),
  by = element
])

# Ratio analysis for matched rows
matched <- comp[both_nonzero == TRUE & value_fao > 0]
matched[, ratio := value_whep / value_fao]

cat("\n=== Ratio distribution (WHEP/FAOSTAT) for matched rows ===\n")
cat("Rows:", nrow(matched), "\n")
cat("Median ratio:", round(median(matched$ratio, na.rm = TRUE), 3), "\n")
cat("Mean ratio:", round(mean(matched$ratio, na.rm = TRUE), 3), "\n")
print(quantile(
  matched$ratio,
  c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99),
  na.rm = TRUE
))

# WHEP-only trade: how many tonnes are these?
cat("\n=== WHEP-only trade (imputed, no FAOSTAT counterpart) ===\n")
whep_only_dt <- comp[whep_only == TRUE]
cat("Total rows:", nrow(whep_only_dt), "\n")
cat("Total tonnes:", round(sum(whep_only_dt$value_whep)), "\n")
cat(
  "Share of all WHEP trade tonnes:",
  round(100 * sum(whep_only_dt$value_whep) / sum(comp$value_whep), 2),
  "%\n"
)

items_lookup <- as.data.table(whep::items_full)[, .(item_cbs_code, item_cbs)]
items_lookup <- unique(items_lookup, by = "item_cbs_code")
polities_lookup <- as.data.table(whep::polities)[, .(area_code, area_name)]

cat("\n=== Top 20 items: WHEP-only trade tonnes ===\n")
print(
  whep_only_dt[, .(imp_t = sum(value_whep), n = .N), by = item_cbs_code][
    items_lookup,
    on = "item_cbs_code",
    nomatch = 0
  ][order(-imp_t)][1:20, .(item_cbs, imp_t, n)]
)

cat("\n=== Top 20 countries: WHEP-only trade tonnes ===\n")
print(
  whep_only_dt[, .(imp_t = sum(value_whep), n = .N), by = area_code][
    polities_lookup,
    on = "area_code",
    nomatch = 0
  ][order(-imp_t)][1:20, .(area_name, imp_t, n)]
)

cat("\nDone.\n")
