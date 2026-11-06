# Trade selection changes quantities and therefore their provenance together.
# The existing CBS cascade still uses its (year, bucket, item, element) adapter;
# reporting polity identity is parked and restored by the build (whep#458).
# This metadata repair does not redefine the adapter or source selection.
.cbs_trade_keys <- function() {
  c("year", "area_code", "item_cbs_code", "element")
}

.sum_trade_provenance <- function(trade, keys = .cbs_trade_keys()) {
  rows <- tibble::as_tibble(trade) |>
    ensure_columns(tibble::tibble(source = character(), fao_flag = character()))
  out <- rows |>
    dplyr::summarise(
      value = sum(.data$value, na.rm = TRUE),
      source = .trade_source_label(.data$source),
      .by = dplyr::all_of(keys)
    )
  # An unflagged contribution cannot inherit the flag of a different source.
  .add_folded_fao_flags(out, rows, keys, unflagged = "blocks") |>
    tibble::as_tibble()
}

.trade_source_label <- function(source) {
  if (anyNA(source) || any(!nzchar(source))) {
    return(NA_character_)
  }
  paste(sort(unique(source)), collapse = " + ")
}

.cbs_all_trade <- function(fao_trade, fishstat_trade) {
  fao <- .mass_only_trade(
    .aggregate_fao_trade_to_cbs(fao_trade),
    "faostat-trade-totals"
  )
  if (is.null(fishstat_trade) || nrow(fishstat_trade) == 0L) {
    return(fao)
  }
  # The legacy FishStat pin has no observation flag. Naming its dataset here
  # does not invent one; the aggregation retains NA when it is not supplied.
  fish <- tibble::as_tibble(fishstat_trade) |>
    dplyr::mutate(source = "FishStat_trade") |>
    .mass_only_trade("fishstat-trade")
  dplyr::bind_rows(tibble::as_tibble(fao), tibble::as_tibble(fish)) |>
    .sum_trade_provenance() |>
    data.table::as.data.table()
}

.cbs_trade_sources <- function(out, before, trade, lookup, trade_zero) {
  keys <- .cbs_trade_keys()
  if (is.null(trade)) {
    trade <- before[0L, ]
  }
  supplied <- tibble::as_tibble(trade) |>
    ensure_columns(tibble::tibble(
      source = character(),
      fao_flag = character()
    )) |>
    dplyr::select(
      dplyr::all_of(c(keys, "source", "fao_flag")),
      record_value = "value"
    )
  original <- before |>
    dplyr::select(dplyr::all_of(keys), before_value = "value")
  changed <- out |>
    dplyr::filter(.data$element %in% c("import", "export")) |>
    dplyr::select(dplyr::all_of(c(keys, "value"))) |>
    dplyr::left_join(original, by = keys, relationship = "one-to-one") |>
    dplyr::left_join(supplied, by = keys, relationship = "one-to-one") |>
    dplyr::filter(
      is.na(.data$before_value) |
        (trade_zero == "prefer_record" &
          .is_trade_zero_conflict(.data$before_value, .data$record_value))
    ) |>
    dplyr::mutate(
      source = dplyr::case_when(
        !is.na(.data$record_value) ~ .data$source,
        .data$value > 0 ~ "trade_balance_residual",
        .default = "trade_default_zero"
      ),
      fao_flag = dplyr::if_else(
        !is.na(.data$record_value),
        .data$fao_flag,
        NA_character_
      )
    ) |>
    dplyr::select(dplyr::all_of(c(keys, "source", "fao_flag")))
  lookup <- dplyr::bind_rows(
    dplyr::anti_join(tibble::as_tibble(lookup), changed, by = keys),
    changed
  )
  dplyr::left_join(out, lookup, by = keys, relationship = "many-to-one")
}
