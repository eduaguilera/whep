coefs <- "inst/extdata/input/processed/processing_coefs.csv" |>
  here::here() |>
  get_processing_coefs()

cbs <- "inst/extdata/input/processed/cbs.csv" |>
  here::here() |>
  get_wide_cbs()


df <- coefs |>
  dplyr::filter(
    area == "Afghanistan",
    year == 1961,
    processeditem == "Sugar beet"
  ) |>
  dplyr::left_join(cbs, by = dplyr::join_by(year, area, area_code, item)) |>
  dplyr::group_by(year, area_code, processeditem) |>
  dplyr::summarize(total_proc = sum(value_proc), dplyr::across())


df |>
  View()
