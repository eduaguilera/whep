# FishStat trade weight basis at the CBS boundary (whep#1617) -----------------
#
# The `fishstat-trade` pin is FishStat's `TRADE_QUANTITY.csv` filtered to
# `MEASURE == "Q_tpw"`, which FAO defines as "Tonnes – net product weight"
# (FI_Trade 2025.1.0, `FSJ_UNIT.csv`, code `Q_tpw`; every quantity row of that
# release carries this measure, so no live-weight trade exists to select
# instead). FAO's fish balances, which the CBS fish items 2761-2775 come from,
# are on another basis: "All calculations have been made in terms of
# live-weight equivalent" (FAO 2019, Fishery and Aquaculture Statistics 2017,
# Food balance sheets, General notes, p. vii), each product being converted
# "into its corresponding live-weight equivalent, by making use of specific
# technical conversion factors" (Taglioni 2020, Migration of Fishery SUA and
# FBS compilation into the SWS, "Standardization to FBS",
# https://sws-methodology.github.io/faoswsFisheriesSUAFBSdocumentation/).
# Both are tonnes, so `.mass_only_trade()`, which checks the unit and nothing
# else, passes the pin; a fillet tonne then enters a live-weight account as a
# tonne of whole fish.
#
# The pin cannot be converted. It is aggregated to CBS items, so the commodity
# (fillet, meal, oil, canned) the conversion factor depends on is gone, and
# the factors themselves differ by product and by country-year (FAO's generic
# SWS tree has 0.2 for pelagic meal; Peru's own 2017 rate is 0.24). Two more
# facts rule out using any of it as it stands:
#
# * Its items do not mean what their codes say. The bridge it was built with
#   (commit 68582d02) booked ISSCAAP 81 and 83 under 2781 Fish, Body Oil and
#   82 under 2782 Fish, Liver Oil, but ISSCAAP 81-83 are "Pearls,
#   mother-of-pearl, shells", "Corals" and "Sponges" (FI_Trade 2025.1.0,
#   `ISSCAAP.pdf`, division 8). The one pair of items FAO keeps in product
#   weight therefore holds no oil, while meals and body oils sit in the fish
#   items (whep#1616).
# * FAO's fish balances are compiled from this same FishStat trade, so where
#   a balance sheet books a fish trade as zero the commodity behind the
#   FishStat figure is usually already in another item. Peru's 2017 exports
#   of Marine Fish, Other (2764) are 0 in the balance sheet and 1,253,750 t in
#   the pin, of which 1,030,540 t is "Fishmeals, nei" and 171,087 t "Fish body
#   oils, nei" (FI_Trade 2025.1.0, reporter 604, `TRADE_QUANTITY.csv`). The balance sheet already books
#   that meal, converted, in its 4.53 Mt of Pelagic Fish (2763) exports, and
#   the oil as 171,000 t of Fish, Body Oil (2781).
#
# So `"withhold"`, the default, keeps the pin out of the CBS: it supplies no
# Tier-1 trade fill and no trade-only recovered row. It is still read, so
# that a raw or weight-labelled candidate still aborts
# (`.check_fishstat_cbs_basis()`), and every build states the tonnage it
# leaves out: the pin rows at keys where the balance sheet books no trade,
# which is what `"as_live_weight"` fills. The rest of the pin, where the
# balance sheet has its own figure, never reached the balance under either
# option. The policy is stamped on the fish rows as `method_fish_trade`. `"as_live_weight"` books the product weight as live weight, the
# behaviour before whep#1617, for reproducing an older build. It is not a
# conversion.
#
# Measured on complete 2017 builds (`build_primary_production()` then
# `build_commodity_balances()`, 181,174 rows), the legacy behaviour against
# the default:
#
# * Tier-1 fill: 1,040 keys where the balance sheet books zero (4.17 Mt of
#   exports, 2.44 Mt of imports; 2764 alone 3.99 Mt and 2.36 Mt) and 63
#   keys where it books nothing (0.32 Mt and 0.16 Mt).
# * Trade-only recovery: 3.60 Mt of fish trade created where the CBS has no
#   fish row, 3.04 Mt of it Japan, whose fish account the `faostat-fbs-new`
#   pin does not carry from 2014 on. Under the default those areas keep no
#   fish account instead of one with product-weight trade and no production.
#
# A live-weight FishStat input, converted by product from the source
# commodities, is the remaining work of whep#1617; until it exists no FishStat
# quantity reaches a fish balance by default.

# The two answers, default first.
.fishstat_trade_choices <- function() {
  c("withhold", "as_live_weight")
}

# The CBS items the `fishstat-trade` pin can reach (fishstat_cbs_bridge.csv).
.fishstat_cbs_items <- function() {
  c(2761:2767, 2769, 2775, 2781, 2782)
}

# Apply the selected policy to the aggregated pin rows `.read_fishstat_trade()`
# read. `dt` has `year`, `area_code`, `item_cbs_code`, `element` (`"import"`
# or `"export"`) and `value`; `balance` is the combined CBS before balancing,
# with `year`, `area_code`, `item_cbs_code`, `element` and `value`. Returns
# `dt` under `"as_live_weight"` and `NULL` under `"withhold"`, the value the
# reader already returns for an unregistered pin, so the trade merge needs no
# second branch.
.fishstat_weight_basis <- function(dt, method, balance) {
  method <- rlang::arg_match(method, .fishstat_trade_choices())
  if (is.null(dt) || nrow(dt) == 0L) {
    return(if (method == "withhold") NULL else dt)
  }
  gross <- .fishstat_tonnes_by_flow(dt)
  fills <- .fishstat_tonnes_by_flow(.fishstat_unbooked_rows(dt, balance))
  if (method == "as_live_weight") {
    cli::cli_inform(
      c(
        "!" = "FishStat net product weight is booked as live weight where \\
               the balance sheet books no trade: {fills$import} t of \\
               imports and {fills$export} t of exports.",
        "i" = "{.arg fishstat_trade} = {.val as_live_weight} reproduces \\
               builds made before whep#1617; it is not a conversion."
      ),
      class = "whep_fishstat_product_weight"
    )
    return(dt)
  }
  cli::cli_inform(
    c(
      "i" = "FishStat trade withheld from the fish balances: \\
             {fills$import} t of imports and {fills$export} t of exports \\
             in net product weight, at keys where the balance sheet books no \\
             trade, which FAO's live-weight fish balances cannot take \\
             (whep#1617).",
      "i" = "The pin holds {gross$import} t and {gross$export} t in all; \\
             the rest falls where the balance sheet has its own figure, \\
             which takes precedence under either option.",
      "i" = "Set {.arg fishstat_trade} = {.val as_live_weight} to book \\
             them as live weight, as before."
    ),
    class = "whep_fishstat_withheld"
  )
  NULL
}

# The pin rows `"as_live_weight"` would book: a positive value at a key where
# the balance has no trade row or a zero one. The tier-1 trade fill takes
# both (`.fill_tier1_trade()` under the default `trade_zero`), and the
# trade-only recovery takes the keys of an area with no row for the item.
.fishstat_unbooked_rows <- function(dt, balance) {
  keys <- c("year", "area_code", "item_cbs_code", "element")
  booked <- balance |>
    tibble::as_tibble() |>
    dplyr::filter(
      .data$element %in% c("import", "export"),
      .data$value != 0
    ) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::mutate(item_cbs_code = as.numeric(.data$item_cbs_code))
  dt |>
    tibble::as_tibble() |>
    dplyr::filter(.data$value > 0) |>
    dplyr::mutate(item_cbs_code = as.numeric(.data$item_cbs_code)) |>
    dplyr::anti_join(booked, by = keys)
}

# Rounded import and export tonnage, formatted for a message.
.fishstat_tonnes_by_flow <- function(dt) {
  flows <- c("import", "export")
  totals <- vapply(
    flows,
    function(flow) sum(dt$value[dt$element == flow], na.rm = TRUE),
    numeric(1)
  )
  as.list(format(round(totals), big.mark = ",", scientific = FALSE))
}

# Record the policy on the rows of the items the pin can reach; other items
# carry `NA`. `"not_applied"` marks a `.fixed_data` balance, which never read
# the pin.
.cbs_stamp_fish_trade <- function(data, method) {
  dplyr::mutate(
    data,
    method_fish_trade = dplyr::if_else(
      .data$item_cbs_code %in% .fishstat_cbs_items(),
      method,
      NA_character_
    )
  )
}
