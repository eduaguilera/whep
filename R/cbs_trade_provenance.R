# Trade selection changes quantities and therefore their provenance together.
# The existing CBS cascade still uses its (year, bucket, item, element) adapter;
# reporting polity identity is parked and restored by the build (whep#458).
# This metadata repair does not redefine the adapter or source selection.
.cbs_trade_keys <- function() {
  c("year", "area_code", "item_cbs_code", "element")
}

# Sums trade by `keys` and names the datasets behind each sum. Vectorised:
# it runs over millions of trade keys per build, where a per-group R call to
# the labelling function took ~80 s on 2.6M keys. The data.table `sum()` also
# keeps the values bit-identical to the plain aggregation it replaced.
.sum_trade_provenance <- function(trade, keys = .cbs_trade_keys()) {
  rows <- tibble::as_tibble(trade) |>
    ensure_columns(tibble::tibble(source = character(), fao_flag = character()))
  dt <- data.table::as.data.table(rows)
  labels <- .trade_source_labels(dt, keys)
  out <- dt[, list(value = sum(value, na.rm = TRUE)), by = keys]
  out[, source := NA_character_]
  out[labels, source := i.source, on = keys]
  # An unflagged contribution cannot inherit the flag of a different source.
  .add_folded_fao_flags(out, rows, keys, unflagged = "blocks") |>
    tibble::as_tibble()
}

# One label per key: its distinct sources sorted and joined by `" + "`. A key
# with any missing or empty source is unattributable and labels as `NA`.
# Equivalent to `.trade_source_label()` applied per group, which
# `test_cbs_trade_provenance.R` asserts.
#
# Each source is one bit of a per-key mask, so the per-key work is a GForce
# `sum()` and only the few distinct masks are ever pasted. A missing or empty
# source has no bit, and its `NA` propagates through the sum to block the key.
# A mask is exact only while it fits a double's 53-bit significand; past 52
# sources the labels are pasted per key instead, which is slower but the same.
.trade_source_labels <- function(dt, keys) {
  known <- !is.na(dt$source) & nzchar(dt$source)
  # Base `sort()` sets the order, so labels match the scalar helper's collation.
  sources <- sort(unique(dt$source[known]))
  if (length(sources) > 52L) {
    return(dt[, list(source = .trade_source_label(source)), by = keys])
  }
  bits <- data.table::as.data.table(dt[, keys, with = FALSE])
  bits[, .source_bit := 2^(match(dt$source, sources) - 1)]
  masks <- unique(bits)[,
    list(.source_mask = sum(.source_bit)),
    by = keys
  ]
  distinct <- unique(masks$.source_mask[!is.na(masks$.source_mask)])
  masks[,
    source := .mask_source_labels(distinct, sources)[
      match(.source_mask, distinct)
    ]
  ]
  masks[, .source_mask := NULL]
  masks
}

.mask_source_labels <- function(masks, sources) {
  members <- outer(masks, 2^(seq_along(sources) - 1), `%/%`) %% 2 == 1
  apply(members, 1L, \(row) paste(sources[row], collapse = " + ")) |>
    as.character()
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
