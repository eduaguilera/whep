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
# Transcribed below so an edit to the CSV that drifts away from the source, or
# a `source` label that stops describing its own row, fails loudly. Wirsenius
# partitions each crop into shares that sum to one, so only the residue share
# is transcribed and the product share is its complement (oil palm is
# fruit bunch / leaves / trunk, and leaves + trunk is the residue). See
# whep#1132.

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

# Table 3.17, recovery rate, per region. Millet is a cereal and takes the
# cereals row; Wirsenius lists no millet of its own in either table.
recovery_3_17 <- tibble::tribble(
  ~cat_krausmann,         ~recovery,
  "Wheat, other cereals", cereals_recovery,
  "Rice, Paddy",          cereals_recovery,
  "Maize",                cereals_recovery,
  "Millet",               cereals_recovery,
  "Sorghum",              cereals_recovery,
  "Sugar Cane",           rep(0.90, 8),
  "Soybeans",             cereals_recovery,
  "Groundnuts in Shell",  rep(0.90, 8),
  "Sunflower Seed",       rep(0.50, 8),
  "Rapeseed, oil crops",  rep(0.70, 8)
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

test_that("every residue_recovery row declares a source", {
  tbl <- whep::whep_coef_table("residue_recovery")
  testthat::expect_true(rlang::has_name(tbl, "source"))
  testthat::expect_false(any(is.na(tbl$source)))
  testthat::expect_true(all(nzchar(tbl$source)))
})

test_that("residue:product ratios match Wirsenius Table 3.16", {
  tbl <- whep::whep_coef_table("residue_recovery")
  expected <- residue_share_3_16 |>
    expand_wirsenius(share) |>
    dplyr::mutate(ratio = round(share / (1 - share), 1))
  got <- expected |>
    dplyr::left_join(tbl, by = c("cat_krausmann", "region_krausmann"))
  testthat::expect_equal(got$residue_dm_product_dm, got$ratio)
  testthat::expect_true(all(grepl("Tab.3.16", got$source, fixed = TRUE)))
})

test_that("recovery rates match Wirsenius Table 3.17 where claimed", {
  tbl <- whep::whep_coef_table("residue_recovery")
  expected <- recovery_3_17 |>
    expand_wirsenius(recovery) |>
    dplyr::left_join(tbl, by = c("cat_krausmann", "region_krausmann"))
  claimed <- dplyr::filter(
    expected,
    grepl("Tab.3.17", .data$source, fixed = TRUE),
    !grepl("differs", .data$source, fixed = TRUE)
  )
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

test_that("the one row flagged as differing from Table 3.17 really does", {
  tbl <- whep::whep_coef_table("residue_recovery")
  flagged <- dplyr::filter(
    tbl,
    grepl("differs from Tab.3.17", .data$source, fixed = TRUE)
  )
  testthat::expect_setequal(
    unique(flagged$cat_krausmann),
    "Groundnuts in Shell"
  )
  # Wirsenius gives groundnut stalks 0.90 in every region; this table instead
  # reuses the cereals pattern, so five of the eight regions disagree.
  testthat::expect_false(all(flagged$recovery_rates == 0.90))
})

test_that("categories absent from Wirsenius are labelled unverified", {
  tbl <- whep::whep_coef_table("residue_recovery")
  unverified <- tbl |>
    dplyr::filter(!grepl("Wirsenius", .data$source, fixed = TRUE)) |>
    dplyr::pull(cat_krausmann) |>
    unique()
  testthat::expect_setequal(
    unverified,
    c(
      "Sugar Crops nes",
      "Beans, Dry",
      "Pulses",
      "Castor Beans",
      "Fodder crops",
      "Permanent crops"
    )
  )
  testthat::expect_true(all(
    tbl$source[tbl$cat_krausmann %in% unverified] == "assumed, unverified"
  ))
  # These are not in Wirsenius Table 3.16 or Table 3.17 at all.
  testthat::expect_length(
    intersect(unverified, residue_share_3_16$cat_krausmann),
    0L
  )
  testthat::expect_length(
    intersect(unverified, recovery_3_17$cat_krausmann),
    0L
  )
})

test_that("recovery rates absent from Table 3.17 are declared as such", {
  tbl <- whep::whep_coef_table("residue_recovery")
  assumed <- tbl |>
    dplyr::filter(grepl("recovery assumed", .data$source, fixed = TRUE)) |>
    dplyr::pull(cat_krausmann) |>
    unique()
  testthat::expect_setequal(
    assumed,
    c("Roots and Tubers", "Cassava", "Sugar Beets", "Oil Palm Fruit")
  )
  testthat::expect_length(intersect(assumed, recovery_3_17$cat_krausmann), 0L)
})
