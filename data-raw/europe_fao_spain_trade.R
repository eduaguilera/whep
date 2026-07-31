# Extract Spain's historical Export/Import series from the source workbook
# `Europe_FAO_completed.xlsx` into `inst/extdata/europe_fao_spain_trade.csv`.
#
# The workbook is a 16.5 MB, 19-sheet, 35-country compilation, of which this
# package reads two sheets and only the Spanish rows (253 of 9113, i.e. 2.8%).
# Shipping the whole workbook inside the package added ~16 MB to every install
# and clone, so only the extract is checked in. The workbook itself is not
# redistributed here; point this script at your local copy.
#
# Usage:
#   WHEP_EUROPE_FAO_XLSX=/path/to/Europe_FAO_completed.xlsx \
#     Rscript data-raw/europe_fao_spain_trade.R
#
# Values in the workbook are stored as "1000 MT", hence the *1000 conversion
# to Mg (fresh matter) applied here rather than at read time.

library(dplyr)

source_path <- Sys.getenv("WHEP_EUROPE_FAO_XLSX", unset = NA_character_)
if (is.na(source_path) || !file.exists(source_path)) {
  cli::cli_abort(c(
    "Set {.envvar WHEP_EUROPE_FAO_XLSX} to your copy of
     {.file Europe_FAO_completed.xlsx}.",
    i = "It is not redistributed with the package; see this script's header."
  ))
}

read_sheet <- function(sheet) {
  raw <- readxl::read_excel(source_path, sheet = sheet)
  year_cols <- names(raw)[stringr::str_detect(names(raw), "^[0-9]{4}\\.0$")]

  raw |>
    dplyr::filter(Area == "Spain", !is.na(Item)) |>
    dplyr::select(Item, dplyr::all_of(year_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(year_cols), as.numeric)) |>
    tidyr::pivot_longer(
      dplyr::all_of(year_cols),
      names_to = "Year",
      values_to = "value_fm"
    ) |>
    dplyr::mutate(
      Year = as.integer(readr::parse_number(Year)),
      value_fm = value_fm * 1000
    ) |>
    dplyr::filter(!is.na(value_fm))
}

spain_trade <- c(Export = "Export", Import = "Import") |>
  purrr::map(read_sheet) |>
  dplyr::bind_rows(.id = "Element") |>
  dplyr::arrange(Element, Item, Year)

readr::write_csv(spain_trade, "inst/extdata/europe_fao_spain_trade.csv")

cli::cli_alert_success(
  "Wrote {nrow(spain_trade)} rows covering
   {min(spain_trade$Year)}-{max(spain_trade$Year)}."
)
