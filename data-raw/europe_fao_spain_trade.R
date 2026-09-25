# Build the package dataset `europe_fao_spain_trade` from
# `data-raw/europe_fao_spain_trade.csv` (whep#64).
#
# The CSV is Spain's historical Export/Import series (Mg fresh matter), already
# extracted from `Europe_FAO_completed.xlsx` by
# `data-raw/europe_fao_spain_trade_extract.R`, which needs that workbook and so
# cannot run offline. This stage reads only the checked-in CSV, so the data-raw
# freshness gate re-runs it and fails if the .rda and the CSV ever disagree.
#
# The column types are pinned rather than guessed, so the dataset keeps the
# exact schema the runtime reader used to impose when this table was read from
# inst/extdata at run time.

europe_fao_spain_trade <- here::here(
  "data-raw",
  "europe_fao_spain_trade.csv"
) |>
  readr::read_csv(
    col_types = readr::cols(
      Element = readr::col_character(),
      Item = readr::col_character(),
      Year = readr::col_integer(),
      value_fm = readr::col_double()
    )
  ) |>
  tibble::as_tibble()

attr(europe_fao_spain_trade, "spec") <- NULL
attr(europe_fao_spain_trade, "problems") <- NULL

usethis::use_data(europe_fao_spain_trade, overwrite = TRUE)
