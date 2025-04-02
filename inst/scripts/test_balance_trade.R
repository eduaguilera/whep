df <- get_bilateral_trade(get_file_path("bilateral_trade"))

df <- df |>
  dplyr::filter(unit == "tonnes") |>
  dplyr::select(-unit)

xd <- df |>
  tidyr::nest(.by = c(year, item)) |>
  head(1) |>
  dplyr::mutate(
    data = purrr::map(data, function(x) {
      x |>
        tidyr::complete(
          from_code = tidyr::full_seq(c(from_code, to_code), 1),
          to_code = tidyr::full_seq(c(from_code, to_code), 1),
          fill = list(unit = "tonnes", value = 0)
        )
    })
  )

one <- dplyr::pull(xd, data) |> purrr::pluck(1)

xdone <-
  one |>
  tidyr::pivot_wider(names_from = to_code, values_from = value) |>
  tibble::column_to_rownames(var = "from_code")
  as.matrix()
