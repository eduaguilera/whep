# prepare_fishstat_trade.R
# ─────────────────────────────────────────────────────────────
# Prepare FishStat trade data for upload as a whep pin.
#
# Reads raw FishStat TRADE_QUANTITY.csv, maps ISSCAAP commodity
# groups to CBS item codes, maps each FishStat reporter to its FAOSTAT
# area code and to the polity live in each year, and outputs an
# aggregated dataset ready for pin_upload to SACO.
#
# Every source reporter is kept. Rows whose territory has no polity in
# their year stay in the pin with `Coverage Status` saying why, and the
# CBS reader leaves them out by name (whep#1618). See
# `R/fishstat_trade.R` for the mapping and its source.
#
# ─────────────────────────────────────────────────────────────

library(data.table)
library(dplyr)
library(cli)

devtools::load_all(".")

# Helper from prepare_upload.R (inlined to avoid executing its example call)
create_version <- function(data, board, name, ...) {
  # Restricted inputs must never be republished as a public pin (#1386).
  whep::whep_assert_publishable(data)

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

# == 1. Read raw FishStat tables ===============================================

cli::cli_h1("Reading FishStat trade data")

trade_raw <- data.table::fread(
  file.path(fishstat_dir, "TRADE_QUANTITY.csv"),
  showProgress = FALSE
)
commodities <- data.table::fread(
  file.path(fishstat_dir, "CL_FI_COMMODITY_ISSCFC.csv"),
  select = c("Code", "ISSCAAP")
)
countries <- data.table::fread(
  file.path(fishstat_dir, "CL_FI_COUNTRY_GROUPS.csv"),
  select = c("UN_Code", "Identifier", "ISO3_Code")
)
bridge <- data.table::fread(
  system.file(
    "extdata",
    "harmonization",
    "fishstat_cbs_bridge.csv",
    package = "whep"
  )
)
cat("Raw rows:", nrow(trade_raw), "\n")

# == 2. Map commodities, reporters and polities ================================

cli::cli_h1("Mapping commodities, reporters and polities")

fishstat_trade <- whep:::.prepare_fishstat_trade(
  trade_raw,
  commodities,
  countries,
  bridge
)
coverage <- whep:::.fishstat_trade_coverage(fishstat_trade)
held <- dplyr::filter(coverage, .data$`Coverage Status` != "adopted")
if (nrow(held) > 0L) {
  cli::cli_alert_warning("Rows kept in the pin but not adopted:")
  print(held, n = Inf)
}

cat("Final rows:", nrow(fishstat_trade), "\n")
cat("Years:", min(fishstat_trade$Year), "-", max(fishstat_trade$Year), "\n")
cat("Reporters:", length(unique(fishstat_trade$`Reporter Code`)), "\n")
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
