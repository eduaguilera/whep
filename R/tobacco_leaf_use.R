# Tobacco leaf manufactured into products (whep#1390).
#
# The non-food Commodity Balances (`faostat-cbs-new`) report tobacco as
# unmanufactured leaf (826) and three manufactured products (828 cigarettes,
# 829 cigars, 831 other manufactured tobacco), all mapped onto CBS Tobacco.
# The leaf link has no `Processed` element, so the leaf that goes into a
# factory is booked as 826 `other_uses`, and the products made from it are
# then exported or used again. Since whep#1276 the products' production is
# not booked as supply (`.cb_chain_downstream_codes()`), so the summed uses
# exceed the supply by the manufactured output and the balance closes through
# a stock withdrawal that has no stock behind it: on the pin, the Netherlands
# 2019 reports 66.2 kt of leaf `other_uses` and 54.3 kt of 831 production,
# 64.1 kt of 831 exported.
#
# Removing the second count needs the leaf content of a tonne of product,
# and no sourced ratio is in the repo: a cigarette also holds paper and
# filter, and Ukraine 2019 reports 56 kt of products against 29.7 kt of leaf
# `other_uses`. Which treatment to use is a science decision, so it is
# selectable; see `tobacco_leaf_use` in `build_commodity_balances()`.
#
# No precedent settles the ratio (searched 2026-10-07). FAO's Technical
# Conversion Factors for Agricultural Commodities
# (https://www.fao.org/fileadmin/templates/ess/documents/methodology/tcf.pdf)
# has no tobacco entry; it is "limited almost exclusively to edible
# products". FABIO (github.com/fineprint-global/fabio) carries two factor
# sets that disagree, and neither cites a source: `inst/tcf_btd.csv` turns
# traded product into leaf by dividing by 0.9 (828), 0.6 (829) and 0.9 (831),
# while `inst/sua/tcf_sua_expert.csv` gives leaf-to-product extraction rates
# of 0.68 (cigarettes) and 0.375 (cigars). Edu's Global `commodity_balances.r`,
# from which `.get_fiber_tobacco()` descends, sums all four links unconverted,
# which is the double count itself. The afse-wiki decision
# `methodological-decisions-ask-before-acting` forbids choosing a conversion
# factor silently, so the default keeps the record as published.

.tobacco_leaf_use_choices <- function() {
  c("as_published", "one_to_one")
}

# FAOSTAT item codes of the tobacco chain.
.tobacco_leaf_code <- 826L
.tobacco_product_codes <- c(828L, 829L, 831L)

# Treat the leaf manufactured into products in `faostat-cbs-new`.
#
# `"as_published"` returns `cbs_new` as read. `"one_to_one"` subtracts each
# country-year's production of 828/829/831 from its 826 `other_uses`,
# floored at zero, as leaf processed into products. Leaf content per tonne
# of product of 1 is ASSUMED, UNVERIFIED: no conversion factor was found.
# The floor keeps a use from going negative where the products outweigh the
# leaf use; the remainder then stays in the balance as stock change rather
# than being hidden.
.cbs_tobacco_leaf_use <- function(
  cbs_new,
  method = .tobacco_leaf_use_choices()
) {
  method <- rlang::arg_match(method, .tobacco_leaf_use_choices())
  if (method == "as_published") {
    return(cbs_new)
  }
  dt <- data.table::copy(data.table::as.data.table(cbs_new))
  manufactured <- dt[
    item_cbs_code %in% .tobacco_product_codes & element == "production",
    .(manufactured = sum(value, na.rm = TRUE)),
    by = c("area_code", "year")
  ]
  is_leaf_use <- dt$item_cbs_code == .tobacco_leaf_code &
    dt$element == "other_uses"
  leaf <- manufactured[
    dt[is_leaf_use],
    on = c("area_code", "year")
  ]
  # Areas with no manufactured production keep their leaf use: zero is the
  # absence of a production row, not a fill.
  leaf[is.na(manufactured), manufactured := 0]
  leaf[
    manufactured > 0,
    `:=`(value = pmax(value - manufactured, 0))
  ]
  if ("fao_flag" %in% names(leaf)) {
    leaf[manufactured > 0, fao_flag := NA_character_]
  }
  leaf[, manufactured := NULL]
  data.table::rbindlist(
    list(dt[!is_leaf_use], leaf[, names(dt), with = FALSE]),
    use.names = TRUE
  )
}
