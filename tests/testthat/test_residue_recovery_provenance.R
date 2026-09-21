# Pins the provenance of inst/extdata/coefs/residue_recovery.csv.
#
# The table shipped for years as `residue_krausmann.csv`, but both of its
# numeric columns are Wirsenius, S. (2000), "Human Use of Land and Organic
# Materials: Modeling the Turnover of Biomass in the Global Food System", PhD
# thesis, Chalmers University of Technology / Goteborg University:
#
#   * `residue_dm_product_dm` is Table 3.16 (harvest index, p. 92), as the
#     residue share divided by the product share, rounded to one decimal;
#   * `recovery_rates` is Table 3.17 (recovery rates, p. 94).
#
# Open access: publications.lib.chalmers.se/records/fulltext/827.pdf
#
# Both tables are transcribed below so an edit to the CSV that drifts away from
# the source, or a source label that stops describing its own row, fails
# loudly. Wirsenius partitions each crop into shares that sum to one, so only
# the residue share is transcribed and the product share is its complement
# (oil palm is fruit bunch / leaves / trunk, and leaves + trunk is the
# residue). See whep#1132 and whep#1150.
#
# Table 3.17 states its rates per crop CATEGORY, not per crop: one "Cereals
# straw & stover" row governs every cereal, and one "Sugar crops tops &
# leaves" row governs both crops Table 3.16 files under "Sugar crops" (cane
# and beet). Reading it category-wise is what lets Millet take the cereals
# rate, and the same reading gives sugar beet 0.90 in every region. The rates
# this table carries for sugar beet are 0 to 0.75.

# The eight regions of Wirsenius Tables 3.16 and 3.17, in his column order.
# They are also the eight labels of regions_full$region_HANPP.
wirsenius_regions <- c(
  "East Asia",
  "East Europe",
  "Latin America and Caribbean",
  "North Africa and West Asia",
  "North America and Oceania",
  "South and Central Asia",
  "Sub-saharan Africa",
  "West Europe"
)

cereals_recovery <- c(0.80, 0.75, 0.80, 0.80, 0.70, 0.90, 0.90, 0.70)

# The closed vocabulary of the two provenance columns.
ratio_tab_3_16 <- "Wirsenius 2000 Tab.3.16"
ratio_sorghum <- "Wirsenius 2000 Tab.3.16 sorghum"
ratio_assumed <- "assumed, unverified"
recovery_tab_3_17 <- "Wirsenius 2000 Tab.3.17"
recovery_below <- "assumed, unverified; below Wirsenius 2000 Tab.3.17 (0.90)"
recovery_omitted <- paste(
  "assumed, unverified; Wirsenius 2000 Tab.3.17 omits the flow",
  "(p.94 default near 1.00)"
)
recovery_no_flow <- paste(
  "assumed, unverified; no residue flow in Wirsenius 2000",
  "(nearest Tab.3.17 grass-legume 0.90)"
)

# Table 3.16, residue share of the crop, per region.
residue_share_3_16 <- tibble::tribble(
  ~cat_krausmann,         ~share,
  "Wheat, other cereals", c(.60, .60, .60, .60, .55, .63, .70, .50),
  "Rice, Paddy",          c(.50, .55, .55, .55, .55, .60, .60, .55),
  "Maize",                c(.75, .65, .75, .75, .55, .78, .78, .55),
  "Sorghum",              c(.75, .65, .75, .75, .55, .78, .78, .55),
  "Cassava",              rep(.45, 8),
  "Roots and Tubers",     rep(.50, 8),
  "Sugar Cane",           rep(.40, 8),
  "Sugar Beets",          c(.40, .35, .40, .40, .35, .40, .40, .35),
  "Soybeans",             c(.55, .60, .60, .60, .55, .60, .60, .55),
  "Groundnuts in Shell",  c(.55, .55, .60, .60, .55, .60, .60, .55),
  "Sunflower Seed",       c(.70, .65, .70, .70, .65, .70, .70, .65),
  "Rapeseed, oil crops",  c(.70, .65, .70, .70, .65, .70, .70, .65),
  "Oil Palm Fruit",       c(.60, .65, .65, .65, .65, .65, .65, .65)
)

# Table 3.17, recovery rate, per region, for every category one of its rows
# governs. Millet is a cereal and takes the cereals row; sugar beet and the
# FAO catch-all "Sugar Crops nes" take the sugar crops row.
recovery_3_17 <- tibble::tribble(
  ~cat_krausmann,         ~recovery,
  "Wheat, other cereals", cereals_recovery,
  "Rice, Paddy",          cereals_recovery,
  "Maize",                cereals_recovery,
  "Millet",               cereals_recovery,
  "Sorghum",              cereals_recovery,
  "Sugar Cane",           rep(0.90, 8),
  "Sugar Beets",          rep(0.90, 8),
  "Sugar Crops nes",      rep(0.90, 8),
  "Soybeans",             cereals_recovery,
  "Groundnuts in Shell",  rep(0.90, 8),
  "Sunflower Seed",       rep(0.50, 8),
  "Rapeseed, oil crops",  rep(0.70, 8)
)

# Categories Table 3.17 does not govern at all. Wirsenius states that recovery
# rates for the crop flows it omits "were assumed to be close to 100 percent"
# (p. 94); this table assumes 0 to 0.9 for them instead.
recovery_omitted_cats <- c(
  "Roots and Tubers",
  "Cassava",
  "Beans, Dry",
  "Pulses",
  "Oil Palm Fruit",
  "Castor Beans",
  "Permanent crops"
)

expand_wirsenius <- function(x, value_col) {
  x |>
    dplyr::mutate(region_krausmann = list(wirsenius_regions)) |>
    tidyr::unnest(c({{ value_col }}, "region_krausmann"))
}

test_that("residue_recovery region set is Wirsenius's eight HANPP regions", {
  tbl <- whep::whep_coef_table("residue_recovery")
  testthat::expect_setequal(
    unique(tbl$region_krausmann),
    wirsenius_regions
  )
  hanpp <- whep::regions_full$region_HANPP
  testthat::expect_setequal(
    unique(hanpp[!is.na(hanpp) & nzchar(hanpp)]),
    wirsenius_regions
  )
  # Every crop category is stated for every region, as Wirsenius states them.
  testthat::expect_equal(
    nrow(tbl),
    length(unique(tbl$cat_krausmann)) * length(wirsenius_regions)
  )
})

test_that("both provenance columns use a closed vocabulary", {
  tbl <- whep::whep_coef_table("residue_recovery")
  testthat::expect_true(rlang::has_name(tbl, "source_ratio"))
  testthat::expect_true(rlang::has_name(tbl, "source_recovery"))
  testthat::expect_setequal(
    unique(tbl$source_ratio),
    c(ratio_tab_3_16, ratio_sorghum, ratio_assumed)
  )
  testthat::expect_setequal(
    unique(tbl$source_recovery),
    c(recovery_tab_3_17, recovery_below, recovery_omitted, recovery_no_flow)
  )
  # A label describes a crop category, never one region of it.
  labelled <- tbl |>
    dplyr::summarise(
      n_ratio = dplyr::n_distinct(source_ratio),
      n_recovery = dplyr::n_distinct(source_recovery),
      .by = cat_krausmann
    )
  testthat::expect_true(all(labelled$n_ratio == 1L))
  testthat::expect_true(all(labelled$n_recovery == 1L))
})

test_that("residue:product ratios match Wirsenius Table 3.16", {
  tbl <- whep::whep_coef_table("residue_recovery")
  got <- residue_share_3_16 |>
    expand_wirsenius(share) |>
    dplyr::mutate(ratio = round(share / (1 - share), 1)) |>
    dplyr::left_join(tbl, by = c("cat_krausmann", "region_krausmann"))
  testthat::expect_equal(got$residue_dm_product_dm, got$ratio)
  testthat::expect_true(all(got$source_ratio == ratio_tab_3_16))
  # Millet has no row of its own; it reuses sorghum's ratio and says so.
  millet <- dplyr::filter(tbl, cat_krausmann == "Millet")
  sorghum <- dplyr::filter(tbl, cat_krausmann == "Sorghum")
  testthat::expect_true(all(millet$source_ratio == ratio_sorghum))
  testthat::expect_equal(
    millet$residue_dm_product_dm[order(millet$region_krausmann)],
    sorghum$residue_dm_product_dm[order(sorghum$region_krausmann)]
  )
})

test_that("recovery rates match Table 3.17 where the label claims they do", {
  tbl <- whep::whep_coef_table("residue_recovery")
  claimed <- recovery_3_17 |>
    expand_wirsenius(recovery) |>
    dplyr::left_join(tbl, by = c("cat_krausmann", "region_krausmann")) |>
    dplyr::filter(source_recovery == recovery_tab_3_17)
  testthat::expect_equal(claimed$recovery_rates, claimed$recovery)
  testthat::expect_setequal(
    unique(claimed$cat_krausmann),
    c(
      "Wheat, other cereals",
      "Rice, Paddy",
      "Maize",
      "Millet",
      "Sorghum",
      "Sugar Cane",
      "Soybeans",
      "Sunflower Seed",
      "Rapeseed, oil crops"
    )
  )
})

test_that("no recovery rate ever exceeds the Table 3.17 rate above it", {
  # The departures are one-directional: every rate Table 3.17 governs is at or
  # below the rate the thesis gives. A future edit that crosses the source
  # from below has to say so here (whep#1150).
  tbl <- whep::whep_coef_table("residue_recovery")
  governed <- recovery_3_17 |>
    expand_wirsenius(recovery) |>
    dplyr::left_join(tbl, by = c("cat_krausmann", "region_krausmann"))
  testthat::expect_true(all(governed$recovery_rates <= governed$recovery))
})

test_that("the categories below Table 3.17 are named and really are below", {
  tbl <- whep::whep_coef_table("residue_recovery")
  below <- dplyr::filter(tbl, source_recovery == recovery_below)
  testthat::expect_setequal(
    unique(below$cat_krausmann),
    c("Groundnuts in Shell", "Sugar Beets", "Sugar Crops nes")
  )
  # Table 3.17 gives groundnut stalks and sugar crops tops & leaves 0.90 in
  # every region. Each of the three sits under that somewhere.
  gap <- below |>
    dplyr::summarise(min_rate = min(recovery_rates), .by = cat_krausmann)
  testthat::expect_true(all(gap$min_rate < 0.90))
})

test_that("categories Table 3.17 omits are labelled, and sit under 1.00", {
  tbl <- whep::whep_coef_table("residue_recovery")
  omitted <- dplyr::filter(tbl, source_recovery == recovery_omitted)
  testthat::expect_setequal(
    unique(omitted$cat_krausmann),
    recovery_omitted_cats
  )
  testthat::expect_length(
    intersect(recovery_omitted_cats, recovery_3_17$cat_krausmann),
    0L
  )
  # Wirsenius's own answer for these flows is "close to 100 percent"; not one
  # of them reaches even 0.90 in its most generous region.
  testthat::expect_true(all(omitted$recovery_rates <= 0.90))
})

test_that("fodder crops are labelled as having no Wirsenius residue flow", {
  tbl <- whep::whep_coef_table("residue_recovery")
  fodder <- dplyr::filter(tbl, source_recovery == recovery_no_flow)
  testthat::expect_setequal(unique(fodder$cat_krausmann), "Fodder crops")
  testthat::expect_true(all(fodder$recovery_rates == 0))
  testthat::expect_true(all(fodder$source_ratio == ratio_assumed))
})

test_that("every production category has a rate for every region", {
  # .residue_destiny_recovery() replaces an unmatched recovery rate with 0,
  # which books the whole category to soil. A Cat_Krausmann that exists in
  # items_prod_full but not here would therefore vanish from the commodity
  # balance silently, so the cover has to be complete (whep#1150).
  tbl <- whep::whep_coef_table("residue_recovery")
  produced <- unique(whep::items_prod_full$Cat_Krausmann)
  produced <- produced[!is.na(produced)]
  missing <- tidyr::expand_grid(
    cat_krausmann = produced,
    region_krausmann = wirsenius_regions
  ) |>
    dplyr::anti_join(tbl, by = c("cat_krausmann", "region_krausmann"))
  testthat::expect_equal(nrow(missing), 0L)
  # The converse: two categories are in the table and in no production item,
  # so their rates reach no tonne. Adding one to items_prod_full has to be a
  # deliberate change, not a silent one.
  testthat::expect_setequal(
    setdiff(unique(tbl$cat_krausmann), produced),
    c("Sugar Crops nes", "Oil Palm Fruit")
  )
})
