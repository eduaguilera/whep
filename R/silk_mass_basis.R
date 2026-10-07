# Silk mass basis of the commodity balances (whep#1251).
#
# WHEP's CBS item Silk (2747) is a three-link FAOSTAT chain that
# `cbs_trade_codes` maps onto one item: reelable cocoons (1185) are reeled
# into raw silk (1186), with silk waste (1187) on the side. From 2014 on the
# Silk balance is read from the non-food Commodity Balances
# (`faostat-cbs-new`), which report each link in its OWN mass: a tonne of raw
# silk is several tonnes of cocoons. Summing the links unconverted counts the
# raw silk once as cocoons and again as raw silk, and books the reeling loss as
# stock build-up, because the cocoons sent to reeling (element "Processed",
# 5023) are not a final use and are dropped (whep#811). Measured on a real
# 2019-2021 build: 2020 Silk carries 267 kt of stock variation against 536 kt
# of production, 2021 391 kt against 517 kt.
#
# The choice of mass basis is a science decision, so it is selectable; see
# `silk_basis` in `build_commodity_balances()`. Only the 2014+ `faostat-cbs-new`
# rows and the FAOSTAT trade of those years are converted. Before 2014 Silk
# comes from FAO's own aggregated old Commodity Balance item 2747, which
# carries no link breakdown and is kept as published.

.silk_basis_choices <- function() {
  c("cocoon", "raw_silk", "mixed")
}

# FAOSTAT item codes of the silk chain.
.silk_cocoon_code <- 1185
.silk_raw_code <- 1186

# The first year whose Silk balance is read from `faostat-cbs-new`; it matches
# the `year > 2013` filter in `.get_fiber_tobacco()`.
.silk_basis_first_year <- 2014L

# Raw silk obtained per tonne of fresh reelable cocoons.
#
# Source: Lee, Y.-W. (1999). Silk reeling and testing manual. FAO Agricultural
# Services Bulletin 136. FAO, Rome. ISBN 92-5-104293-4. Chapter 2,
# "Characteristics of the cocoon", "Raw silk percentage": "The normal range is
# 65 to 84 percent for the weight of the cocoon shell and 12 to 20 percent for
# the weight of the whole fresh cocoon"
# (https://www.fao.org/4/x2099e/x2099e03.htm, read 2026-09-24).
#
# 0.16 is the MIDPOINT of that range, chosen by WHEP; FAO publishes no single
# global factor. FAO's Technical Conversion Factors for Agricultural
# Commodities carries no silk entry at all (checked in both the 1997
# openknowledge edition and the ESS tcf.pdf). For comparison only, FAO's own
# chain in `faostat-cbs-new` implies 0.10-0.16 for most producers (India 0.15,
# 2020) but 0.34 for China mainland; that ratio is NOT used here.
.silk_raw_extraction_rate <- function() {
  0.16
}

# Put the `faostat-cbs-new` silk links onto one mass basis.
#
# `cbs_new` is the `.extract_fao()` frame read with
# `keep_elements = "Processed"`, still keyed on FAOSTAT item codes. Every
# "Processed" row is removed on return, whatever the method, so no chain
# transfer can reach `.get_fiber_tobacco()` as a use (whep#811).
#
# In cocoon terms the chain closes as
#   cocoons: production + net import = other_uses + processed
#   raw silk: production + net import - stock = other_uses
# so the item's uses are the cocoons used as such, the cocoons reeled
# (`processed`, FAO's own cocoon mass) and the raw silk not reeled from
# domestic cocoons, `(other_uses - production) / rate`. The extraction rate
# therefore only touches raw silk that crossed a border or a stock.
#
# `cbs_new` may also carry FAO's "Residuals" element; it is read only to
# confirm a duplicated cocoon use (`.silk_drop_duplicate_use()`) and, like
# "Processed", never survives the return.
.cbs_silk_mass_basis <- function(
  cbs_new,
  method = .silk_basis_choices(),
  rate = .silk_raw_extraction_rate(),
  drop_duplicate_use = TRUE
) {
  method <- rlang::arg_match(method, .silk_basis_choices())
  dt <- data.table::as.data.table(cbs_new)
  in_scope <- dt$item_cbs_code %in%
    c(.silk_cocoon_code, .silk_raw_code) &
    dt$year >= .silk_basis_first_year
  silk <- dt[in_scope]
  rest <- dt[!in_scope & !element %in% c("Processed", "Residuals")]
  if (drop_duplicate_use) {
    silk <- .silk_drop_duplicate_use(silk)
  }
  silk <- silk[element != "Residuals"]

  silk <- switch(
    method,
    mixed = .silk_mixed_basis(silk),
    cocoon = .silk_cocoon_basis(silk, rate),
    raw_silk = .silk_scale_links(.silk_cocoon_basis(silk, rate), rate)
  )
  data.table::rbindlist(list(rest, silk), use.names = TRUE, fill = TRUE)
}

# Tolerance of the duplicate match below, in tonnes. In the pinned release
# FAO publishes every cocoon `Residuals` value in whole tonnes and the use
# elements to two decimals, so a residual that is minus a use it duplicates
# lies within half a tonne of it (China mainland 2020: -156,938 against
# 156,938.24). The same half tonne is applied to the two uses, which then have
# to agree to FAO's own precision (whep#1281).
.silk_duplicate_tolerance <- 0.5

# Drop a cocoon `other_uses` that repeats the cocoons' `Processed`.
#
# FAO's own balance per row is
#   production + import - export - stock - other uses - processed = residual
# so when other uses equals processed and the residual is minus that amount,
# FAO's balance closes on either element alone and the other is a second
# booking of the same cocoons. China mainland 2020 in the pinned
# `faostat-cbs-new`: 156,938 t booked as both, against 156,690 t produced,
# residual -156,938 t. Kept, it became a -157 kt Silk stock variation under
# every `silk_basis` (whep#1281). The `other_uses` row is the one dropped, as
# the `Processed` row is the one the raw-silk link continues from; both land
# in Silk `other_uses` under every basis, so the choice moves no number.
#
# An imbalance whose two elements differ is left as published, because the
# record does not say which of them is wrong: in the same pin Viet Nam
# 2014-2015 and Indonesia 2015-2018 report more cocoons processed than
# supplied, Afghanistan 2015-2018 books its production as processed and its
# supply as other uses, and China mainland 2022-2023 carries 18.7 kt of other
# uses on top of processing that already equals supply.
.silk_drop_duplicate_use <- function(silk) {
  key <- setdiff(
    names(silk),
    c("item_cbs", "element", "unit", "value", "fao_flag")
  )
  cocoon <- silk[item_cbs_code == .silk_cocoon_code]
  terms <- cocoon[,
    .(
      processed = sum(value[element == "Processed"], na.rm = TRUE),
      other_uses = sum(value[element == "other_uses"], na.rm = TRUE),
      residual = sum(value[element == "Residuals"], na.rm = TRUE)
    ),
    by = key
  ]
  tol <- .silk_duplicate_tolerance
  duplicated_use <- terms[
    processed > tol &
      abs(other_uses - processed) <= tol &
      abs(residual + processed) <= tol,
    key,
    with = FALSE
  ]
  if (nrow(duplicated_use) == 0L) {
    return(silk)
  }
  labels <- paste(duplicated_use$area, duplicated_use$year)
  cli::cli_inform(c(
    "i" = "Dropped {length(labels)} silk cocoon {.field other_uses} \\
      row{?s} that repeat{?s/} the row's {.field Processed}: \\
      {.val {labels}} (whep#1281)."
  ))
  duplicated_use[, element := "other_uses"]
  silk <- silk[!duplicated_use, on = c(key, "element")]
  # The surviving `Processed` row is FAO's number, but the balance it sits in
  # is no longer the one FAO published under that flag.
  duplicated_use[, element := "Processed"]
  if ("fao_flag" %in% names(silk)) {
    silk[duplicated_use, on = c(key, "element"), fao_flag := NA_character_]
  }
  silk[]
}

# Mass as FAO reports it per link, summed. The cocoons sent to reeling are
# booked as `other_uses`, which closes the balance but counts raw silk twice,
# once as the cocoons it was reeled from and again as raw silk. This is the
# convention of FAO's pre-2014 aggregate item 2747.
.silk_mixed_basis <- function(silk) {
  silk[element == "Processed", element := "other_uses"]
  silk
}

.silk_cocoon_basis <- function(silk, rate) {
  key <- setdiff(
    names(silk),
    c("item_cbs", "item_cbs_code", "element", "value", "fao_flag")
  )
  is_raw <- silk$item_cbs_code == .silk_raw_code
  terms <- silk[,
    .(
      processed = sum(
        value[item_cbs_code == .silk_cocoon_code & element == "Processed"],
        na.rm = TRUE
      ),
      raw_use = sum(
        value[item_cbs_code == .silk_raw_code & element == "other_uses"],
        na.rm = TRUE
      ),
      raw_prod = sum(
        value[item_cbs_code == .silk_raw_code & element == "production"],
        na.rm = TRUE
      ),
      has_raw_use = any(
        item_cbs_code == .silk_raw_code & element == "other_uses"
      ),
      has_processed = any(
        item_cbs_code == .silk_cocoon_code & element == "Processed"
      )
    ),
    by = key
  ]
  reeled <- terms[has_raw_use | has_processed]
  reeled[,
    `:=`(
      item_cbs = "Raw silk (not thrown)",
      item_cbs_code = .silk_raw_code,
      element = "other_uses",
      value = processed +
        data.table::fifelse(has_raw_use, (raw_use - raw_prod) / rate, 0)
    )
  ]
  if ("fao_flag" %in% names(silk)) {
    reeled[, fao_flag := NA_character_]
  }
  reeled <- reeled[, names(silk), with = FALSE]

  kept <- silk[
    element != "Processed" &
      !(is_raw & element %in% c("production", "other_uses"))
  ]
  kept <- .silk_rescale(kept, kept$item_cbs_code == .silk_raw_code, 1 / rate)
  data.table::rbindlist(list(kept, reeled), use.names = TRUE)
}

# From cocoon to raw-silk mass: every link times the extraction rate.
.silk_scale_links <- function(silk, rate) {
  .silk_rescale(silk, rep(TRUE, nrow(silk)), rate)
}

# A converted value is no longer the number FAOSTAT published under its flag.
.silk_rescale <- function(dt, rows, factor) {
  idx <- which(rows)
  if (length(idx) == 0L) {
    return(dt)
  }
  data.table::set(dt, i = idx, j = "value", value = dt$value[idx] * factor)
  if ("fao_flag" %in% names(dt)) {
    data.table::set(dt, i = idx, j = "fao_flag", value = NA_character_)
  }
  dt
}

# The FAOSTAT trade of the converted years, onto the same basis: raw silk up
# to cocoons (`"cocoon"`), cocoons down to raw silk (`"raw_silk"`). Silk waste
# (1187) keeps its own mass under every method.
.trade_silk_mass_basis <- function(
  fao_trade,
  method = .silk_basis_choices(),
  rate = .silk_raw_extraction_rate()
) {
  method <- rlang::arg_match(method, .silk_basis_choices())
  if (method == "mixed") {
    return(fao_trade)
  }
  dt <- data.table::copy(data.table::as.data.table(fao_trade))
  converted_code <- if (method == "cocoon") {
    .silk_raw_code
  } else {
    .silk_cocoon_code
  }
  factor <- if (method == "cocoon") 1 / rate else rate
  rows <- dt$item_code_trade == converted_code &
    dt$year >= .silk_basis_first_year &
    dt$element %in% c("import", "export") &
    dt$unit %in% .mass_trade_units()
  .silk_rescale(dt, rows, factor)
}
