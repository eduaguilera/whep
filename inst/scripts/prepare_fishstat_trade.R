# prepare_fishstat_trade.R
# ─────────────────────────────────────────────────────────────
# Prepare FishStat trade data for upload as a whep pin.
#
# Reads raw FishStat TRADE_QUANTITY.csv, maps ISSCAAP commodity
# groups to CBS item codes, maps UN M49 country codes to FAOSTAT
# area codes, and outputs an aggregated dataset ready for
# pin_upload to SACO.
#
# ─────────────────────────────────────────────────────────────

library(data.table)
library(dplyr)
library(cli)

devtools::load_all(".")

# Helper from prepare_upload.R (inlined to avoid executing its example call)
create_version <- function(data, board, name, ...) {
  paths <- file.path(
    tempdir(),
    c(
      stringr::str_glue("{name}.csv"),
      stringr::str_glue("{name}.parquet")
    )
  )
  readr::write_csv(data, paths[[1]])
  nanoparquet::write_parquet(data, paths[[2]])
  board |> pins::pin_upload(paths, name, ...)
  board |> pins::pin_versions(name) |> tail(1) |> dplyr::pull(version)
}

lfiles <- Sys.getenv("WHEP_LFILES")
fishstat_dir <- file.path(lfiles, "fisheries", "fao_trade")

# == 1. Read raw FishStat trade ================================================

cli::cli_h1("Reading FishStat trade data")

trade_raw <- data.table::fread(
  file.path(fishstat_dir, "TRADE_QUANTITY.csv"),
  showProgress = FALSE
)
# Keep only tonnes (Q_tpw), drop reexports (R)
trade_raw <- trade_raw[
  MEASURE == "Q_tpw" &
    `TRADE_FLOW.ALPHA_CODE` %in% c("I", "E")
]
data.table::setnames(
  trade_raw,
  c(
    "TRADE_FLOW.ALPHA_CODE",
    "COUNTRY_REPORTER.UN_CODE",
    "COMMODITY.FAO_CODE",
    "PERIOD",
    "VALUE"
  ),
  c("flow", "un_code", "fao_code", "year", "value")
)
trade_raw[, element := data.table::fifelse(flow == "I", "import", "export")]
trade_raw[, un_code := sprintf("%03d", as.integer(un_code))]

cat("Raw rows:", nrow(trade_raw), "\n")

# == 2. Map commodity to ISSCAAP group → CBS item code =========================

cli::cli_h1("Mapping commodities to CBS items")

commodities <- data.table::fread(
  file.path(fishstat_dir, "CL_FI_COMMODITY_ISSCFC.csv"),
  select = c("Code", "ISSCAAP")
)
commodities[, ISSCAAP := as.integer(ISSCAAP)]

bridge <- data.table::fread(
  system.file(
    "extdata",
    "harmonization",
    "fishstat_cbs_bridge.csv",
    package = "whep"
  )
)

trade_raw <- merge(
  trade_raw,
  commodities[, .(fao_code = Code, isscaap_group = ISSCAAP)],
  by = "fao_code",
  all.x = TRUE
)
trade_raw <- trade_raw[!is.na(isscaap_group)]

trade_raw <- merge(
  trade_raw,
  bridge[, .(isscaap_group, item_cbs_code)],
  by = "isscaap_group",
  all.x = TRUE
)
trade_raw <- trade_raw[!is.na(item_cbs_code)]

cat("After commodity mapping:", nrow(trade_raw), "\n")

# == 3. Map UN M49 → FAOSTAT area_code ========================================

cli::cli_h1("Mapping countries")

countries <- data.table::fread(
  file.path(fishstat_dir, "CL_FI_COUNTRY_GROUPS.csv"),
  select = c("UN_Code", "ISO3_Code")
)
polities <- data.table::as.data.table(whep::polities)[,
  .(iso3c, area_code = as.integer(area_code))
]

countries[, un_code := sprintf("%03d", as.integer(UN_Code))]
trade_raw <- merge(
  trade_raw,
  countries[, .(un_code, iso3c = ISO3_Code)],
  by = "un_code",
  all.x = TRUE
)
trade_raw <- merge(trade_raw, polities, by = "iso3c", all.x = TRUE)
trade_raw <- trade_raw[!is.na(area_code)]

cat("After country mapping:", nrow(trade_raw), "\n")

# == 4. Aggregate to CBS item level ============================================

cli::cli_h1("Aggregating")

trade_raw[,
  Element := data.table::fifelse(
    element == "import",
    "Import quantity",
    "Export quantity"
  )
]
trade_raw[, Year := as.integer(year)]
fishstat_trade <- trade_raw[,
  .(Value = sum(value, na.rm = TRUE)),
  by = .(Year, `Area Code` = area_code, `Item Code` = item_cbs_code, Element)
]
fishstat_trade[, Unit := "tonnes"]

cat("Final rows:", nrow(fishstat_trade), "\n")
cat("Years:", min(fishstat_trade$Year), "-", max(fishstat_trade$Year), "\n")
cat("Areas:", length(unique(fishstat_trade$`Area Code`)), "\n")
cat("Items:", length(unique(fishstat_trade$`Item Code`)), "\n")

# == 5. Upload as pin ==========================================================

cli::cli_h1("Preparing pin upload")

fishstat_tbl <- tibble::as_tibble(fishstat_trade)

board <- pins::board_temp(versioned = TRUE)
version <- create_version(fishstat_tbl, board, "fishstat-trade")

output_path <- file.path(board$path, "fishstat-trade", version)

# Copy to a persistent location for manual upload
persistent_dir <- file.path(
  "inst",
  "scripts",
  "pin_upload",
  "fishstat-trade",
  version
)
dir.create(persistent_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(
  list.files(output_path, full.names = TRUE),
  persistent_dir,
  overwrite = TRUE
)

cli::cli_alert_info(
  "1. Upload folder {persistent_dir} to SACO board."
)
tryCatch(clipr::write_clip(persistent_dir), error = function(e) NULL)
cli::cli_alert_info(
  "2. Add line '- fishstat-trade/{version}/' to _pins.yaml"
)
cli::cli_alert_info(
  "3. Add row to whep_inputs.csv: fishstat-trade,<board_url>,{version}"
)
cli::cli_alert_success("Done. Version: {version}")
cli::cli_text("Files saved to: {persistent_dir}")
