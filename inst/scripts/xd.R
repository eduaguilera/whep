trade_old <- get_bilateral_trade()
system.time(
  trade_new <- get_bilateral_trade()
)

xd1 <- trade_old |>
  dplyr::arrange(year, item_cbs_code) |>
  dplyr::pull(bilateral_trade)

xd2 <- trade_new |>
  dplyr::arrange(year, item_cbs_code) |>
  dplyr::pull(bilateral_trade)
