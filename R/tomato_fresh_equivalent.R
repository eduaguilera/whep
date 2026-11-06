# Fresh-tomato equivalent of product-level tomato trade (whep#1631).
#
# `fbs_trade_basis = "rebased"` puts FBS item 2601 "Tomatoes and products" on
# the old food-balance vintage's trade level (whep#1550, `R/fbs_vintage_rebase.R`).
# These helpers rebuild that trade from the traded products themselves, so the
# anchor can be checked against an account that does not come from either
# vintage. They feed `inst/scripts/diagnose_tomato_trade_basis.R`; nothing in
# the build reads them, and no published value depends on them.
#
# Products are keyed by their EU Combined Nomenclature (CN8) code, because the
# CN splits HS 2002 90 ("paste of tomatoes", FAOSTAT item 391) by dry matter,
# and that split is what decides the conversion: a kilogram of passata and a
# kilogram of triple concentrate stand for very different tonnages of fresh
# tomatoes. FAOSTAT's HS6 trade cannot tell them apart.

# CN8 codes of tomato products and the class each is converted as. The class
# limits are the CN8 descriptions (Eurostat Comext DS-045409 product labels,
# 2010-2013 nomenclature). Ketchup (2103 20) and dried tomatoes (0712 90 30)
# are outside FBS 2601 in FAOSTAT's commodity tree and are kept as their own
# classes, so a comparison with a national account that includes them can
# add them explicitly instead of silently.
.tomato_cn8_classes <- function() {
  tibble::tribble(
    ~cn8,       ~product_class, ~in_fbs_2601,
    "07020000", "fresh",        TRUE,
    "20021010", "peeled",       TRUE,
    "20021090", "peeled",       TRUE,
    "20029011", "dm_below_12",  TRUE,
    "20029019", "dm_below_12",  TRUE,
    "20029031", "dm_12_to_30",  TRUE,
    "20029039", "dm_12_to_30",  TRUE,
    "20029091", "dm_above_30",  TRUE,
    "20029099", "dm_above_30",  TRUE,
    "20095010", "juice",        TRUE,
    "20095090", "juice",        TRUE,
    "21032000", "ketchup",      FALSE,
    "07129030", "dried",        FALSE
  )
}

# The factor sets: kg of fresh tomatoes per kg of traded product, per class.
# Sources (copies in the private research-sources store, whep#1631 report):
#
# * FAO, *Technical Conversion Factors for Agricultural Commodities* (FAO
#   Statistics Division; 1992-1996 national averages; publication year not
#   printed, PDF created 1999), extraction rates in percent of the fresh
#   tomato. The modal printed rates are paste 30 (5 of 25 countries), peeled
#   tomatoes 80 (19 of 34) and single-strength juice 80 (8 of 36). Italy,
#   printed p. 572: juice 74, paste 26, peeled 78.
# * USDA ERS, *Weights, Measures, and Conversion Factors for Agricultural
#   Commodities and Their Products*, Agriculture Handbook 697 (1992), table 45,
#   "Pounds farm weight from pounds canned": tomatoes 1.553, tomato juice
#   1.527, tomato puree (11% solids) 3.247.
# * CULINARIA Europe, *Code of Practice Tomato Ketchup* (2015), technical
#   annex: fresh tomatoes fixed at 5 g dry matter per 100 g; double
#   concentrate (28 g dry matter) 5.6, triple concentrate (36 g) 7.2.
# * LGL Bayern (2016), *Schwermetalle in Tomatenmark*: the trade grades single
#   (14-22% dry matter), double (28-30%) and triple (36-40%) concentrate.
#
# The sets:
#
# * `fao_tcf_modal`: the FAO modal rates, every HS 2002 90 class alike. It
#   reproduces the old vintage's tomato trade (2010 imports of the 80 areas
#   above 20 kt: median old / reconstructed 1.000, interquartile range 0.99
#   to 1.01), so it is the conversion the rebase's anchor carries;
# * `dm_bound_low` / `dm_bound_high`: the lowest and highest factor each class
#   admits, from the CN dry-matter limits at CULINARIA's 5% fresh dry matter
#   (12/5, 30/5, and 40/5 for the top of LGL's triple grade), the highest FAO
#   peeled and juice extraction rates (90 and 80) on the low side, and USDA's
#   canned-tomato, juice and puree factors on the high side. Below 12% dry
#   matter the floor is 1: a product cannot need less than its own weight of
#   fresh tomatoes. These are bounds of the conversion, not of the trade data;
#   a fresh dry matter below 5% (EU processing standard 4.8-5.4% soluble,
#   Commission Regulation (EC) No 1398/96, annex I) raises every
#   concentrate factor;
# * `grade_typical`: each class at its common trade grade -- double
#   concentrate for 12-30%, triple for over 30% (CULINARIA), the 12% ceiling
#   for passata, and Italy's FAO peeled and juice rates, Italy being the
#   dominant supplier assumed, unverified. A point estimate inside the bounds,
#   not a measured mix.
.tomato_fresh_equivalent_factors <- function() {
  tibble::tribble(
    ~product_class, ~factor_set,     ~kg_fresh_per_kg,
    "fresh",        "fao_tcf_modal", 1,
    "peeled",       "fao_tcf_modal", 100 / 80,
    "dm_below_12",  "fao_tcf_modal", 100 / 30,
    "dm_12_to_30",  "fao_tcf_modal", 100 / 30,
    "dm_above_30",  "fao_tcf_modal", 100 / 30,
    "juice",        "fao_tcf_modal", 100 / 80,
    "fresh",        "dm_bound_low",  1,
    "peeled",       "dm_bound_low",  100 / 90,
    "dm_below_12",  "dm_bound_low",  1,
    "dm_12_to_30",  "dm_bound_low",  12 / 5,
    "dm_above_30",  "dm_bound_low",  30 / 5,
    "juice",        "dm_bound_low",  100 / 80,
    "fresh",        "grade_typical", 1,
    "peeled",       "grade_typical", 100 / 78,
    "dm_below_12",  "grade_typical", 12 / 5,
    "dm_12_to_30",  "grade_typical", 5.6,
    "dm_above_30",  "grade_typical", 7.2,
    "juice",        "grade_typical", 100 / 74,
    "fresh",        "dm_bound_high", 1,
    "peeled",       "dm_bound_high", 1.553,
    "dm_below_12",  "dm_bound_high", 3.247,
    "dm_12_to_30",  "dm_bound_high", 30 / 5,
    "dm_above_30",  "dm_bound_high", 40 / 5,
    "juice",        "dm_bound_high", 1.527
  )
}

# Fresh-tomato equivalent of CN8-level tomato trade, per factor set.
#
# `trade` has a character `cn8` column, a `value` column in tonnes of product
# and any key columns (reporter, year, flow, ...), which are kept. Every
# `cn8` must be one `.tomato_cn8_classes()` knows: an unknown code aborts
# rather than being dropped, because a dropped product would understate the
# reconstruction exactly as a missing input would. Ketchup and dried tomatoes
# have no factor here and are left out, since FBS 2601 does not hold them.
#
# Returns one row per key and factor set, with `value` the product tonnes and
# `fresh_equivalent` the fresh-tomato tonnes.
.tomato_fresh_equivalent <- function(
  trade,
  factor_sets = unique(.tomato_fresh_equivalent_factors()$factor_set)
) {
  .check_tomato_trade(trade)
  factors <- .tomato_fresh_equivalent_factors() |>
    dplyr::filter(factor_set %in% factor_sets)
  keys <- setdiff(names(trade), c("cn8", "value"))
  trade |>
    dplyr::inner_join(.tomato_cn8_classes(), by = "cn8") |>
    dplyr::filter(in_fbs_2601) |>
    dplyr::inner_join(
      factors,
      by = "product_class",
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      fresh_equivalent = sum(value * kg_fresh_per_kg),
      value = sum(value),
      .by = dplyr::all_of(c(keys, "factor_set"))
    )
}

.check_tomato_trade <- function(trade) {
  missing <- setdiff(c("cn8", "value"), names(trade))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg trade} lacks column{?s} {.field {missing}}.",
      class = "whep_tomato_trade_columns"
    )
  }
  unknown <- setdiff(unique(trade$cn8), .tomato_cn8_classes()$cn8)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "{.arg trade} holds CN8 code{?s} with no tomato product class: \\
        {.val {unknown}}.",
        "i" = "Add {cli::qty(length(unknown))}{?it/them} to \\
        {.fn .tomato_cn8_classes} with a class."
      ),
      class = "whep_tomato_trade_unknown_cn8"
    )
  }
  invisible(trade)
}
