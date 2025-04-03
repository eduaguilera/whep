btd <- get_bilateral_trade(get_file_path("bilateral_trade"))
cbs <- get_wide_cbs(get_file_path("commodity_balance_sheet")) |>
  dplyr::select(year, item, area_code, export, import)

nested_cbs <- tidyr::nest(
  cbs,
  total_trade = c(area_code, export, import), .by = c(year, item)
)

btd <- btd |>
  dplyr::filter(unit == "tonnes") |>
  dplyr::select(-unit)

btd_items <- btd |>
  dplyr::pull(item) |>
  unique() |>
  sort()

cbs_items <- cbs |>
  dplyr::pull(item) |>
  unique() |>
  sort()

# TODO: Also include these (need total export/import reports)
items_not_in_cbs <- btd_items[!btd_items %in% cbs_items]

btd <- btd |>
  dplyr::filter(!item %in% items_not_in_cbs)

codes <- sort(
  unique(
    c(
      dplyr::pull(btd, from_code),
      dplyr::pull(btd, to_code),
      dplyr::pull(cbs, area_code)
    )
  )
)

estimate_bilateral_trade <- function(exports, imports) {
  est1 <- outer(exports, imports) / sum(imports)
  est2 <- outer(imports, exports) / sum(exports)
  average_est <- sum(est1 + est2, na.rm = TRUE) / 2
  # Can have NAs if we divided by 0 because of 0 imports and exports
  average_est[is.na(average_est)] <- 0
  average_est
}

xd <- btd |>
  tidyr::nest(
    bilateral_trade = c(from_code, to_code, value),
    .by = c(year, item)
  ) |>
  print() |>
  # head(101) |>
  dplyr::left_join(nested_cbs, c("year", "item")) |>
  print() |>
  dplyr::mutate(
    bilateral_trade = purrr::map2(
      bilateral_trade,
      total_trade,
      function(btd, total) {
        btd <- tidyr::complete(
          btd,
          from_code = codes,
          to_code = codes,
        )
        total <- tidyr::complete(
          total,
          area_code = codes,
          fill = list(export = 0, import = 0)
        ) |>
          dplyr::arrange(area_code)

        estimate <- estimate_bilateral_trade(
          dplyr::pull(total, export),
          dplyr::pull(total, import)
        )

        real <- btd |>
          tidyr::pivot_wider(names_from = to_code, values_from = value) |>
          tibble::column_to_rownames(var = "from_code") |>
          as.matrix()

        # print(estimate)
        stopifnot(dim(real) == dim(estimate))
        stopifnot(all(!is.na(estimate)))

        filled <- ifelse(is.na(real), estimate, real)
        filled
      }
    )
  )

one <- dplyr::pull(xd, bilateral_trade) |> purrr::pluck(1)

xd_tidy <- xd |>
  head(200) |>
  dplyr::mutate(
    bilateral_trade = purrr::map(
      bilateral_trade,
      function(btd) {
        btd |>
          tibble::as_tibble() |>
          tibble::rownames_to_column(var = "from_code") |>
          tidyr::pivot_longer(
            setdiff(tidyr::everything(), tidyr::one_of("from_code")),
            names_to = "to_code"
          )
      }
    )
  ) |>
  tidyr::unnest(cols = bilateral_trade) |>
  dplyr::select(-total_trade) |>
  dplyr::mutate(dplyr::across(c(from_code, to_code), as.integer))
