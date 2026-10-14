# Pins the provenance of inst/extdata/coefs/residue_feed_assignment.csv and
# residue_feed_regions.csv (whep#1398).
#
# The feed shares are Wirsenius, S. (2000), "Human Use of Land and Organic
# Materials: Modeling the Turnover of Biomass in the Global Food System", PhD
# thesis, Chalmers University of Technology / Goteborg University, Table 3.20
# (p. 102), the "Assignm." rows for crop by-products. Open access:
# publications.lib.chalmers.se/records/fulltext/827.pdf
#
# Table 3.20 is "expressed as share of the amount distributed (on DM basis)",
# and distributed is what is left of the recovered residue after "dry matter
# losses were uniformly set to 10 percent" for all crop by-products (p. 98)
# and, in North America & Oceania only, after 5% of the recovered cereal and
# soybean straw is left in the field (p. 95, note 117). WHEP's
# `feed_use_fraction` multiplies the RECOVERED residue, so the table converts:
# share of recovered = share of distributed x distributed per recovered.

# Wirsenius's eight regions, in his column order.
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

# Table 3.20, crop by-products, "Assignm." rows, transcribed.
table_3_20 <- list(
  "Rice straw" = c(0.30, 0, 0.60, 0.60, 0, 0.80, 0.80, 0),
  "Other cereals straw and stover" = c(
    0.30,
    0.10,
    0.60,
    0.60,
    0.05,
    0.80,
    0.80,
    0.05
  ),
  "Cassava leaves" = c(0.05, 0, 0.20, 0, 0, 0.40, 0.60, 0),
  "Potato tops" = c(0.80, 0.60, 0.60, 0, 0, 0.80, 0.80, 0),
  "Sugar cane tops" = c(0.40, 0, 0.60, 0.60, 0, 0.80, 0.80, 0),
  "Sugar beet tops" = c(0.15, 0.40, 0, 0.40, 0.60, 0.15, 0, 0.90),
  "Groundnut stalks" = c(0.40, 0, 0.60, 0.60, 0, 0.80, 0.80, 0),
  "Other oil crops straw and stalks" = c(0.25, 0, 0.60, 0.60, 0, 0.80, 0.80, 0)
)

# Categories whose straw note 117 leaves 5% in the field in North America &
# Oceania: cereals straw and stover, and soybean straw.
left_in_field <- c(
  "Wheat, other cereals",
  "Rice, Paddy",
  "Maize",
  "Millet",
  "Sorghum",
  "Soybeans"
)

feed_table <- function() {
  whep::whep_coef_table("residue_feed_assignment")
}

test_that("the feed table covers every recovery category in every region", {
  feed <- feed_table()
  recovery <- whep::whep_coef_table("residue_recovery")
  testthat::expect_equal(nrow(feed), nrow(recovery))
  testthat::expect_setequal(feed$cat_krausmann, recovery$cat_krausmann)
  testthat::expect_setequal(feed$region_wirsenius, wirsenius_regions)
  testthat::expect_equal(
    nrow(dplyr::distinct(feed, cat_krausmann, region_wirsenius)),
    nrow(feed)
  )
})

test_that("every sourced cell is Table 3.20 as printed", {
  feed <- dplyr::filter(feed_table(), !is.na(tab_3_20_row))
  testthat::expect_setequal(unique(feed$tab_3_20_row), names(table_3_20))
  expected <- purrr::map2_dbl(
    feed$tab_3_20_row,
    feed$region_wirsenius,
    \(row, region) table_3_20[[row]][match(region, wirsenius_regions)]
  )
  testthat::expect_equal(feed$feed_assignment_distributed, expected)
})

test_that("the share of distributed is converted to a share of recovered", {
  feed <- dplyr::filter(feed_table(), !is.na(tab_3_20_row))
  expected_ratio <- dplyr::if_else(
    feed$cat_krausmann %in%
      left_in_field &
      feed$region_wirsenius == "North America and Oceania",
    0.95 * 0.90,
    0.90
  )
  testthat::expect_equal(feed$distributed_per_recovered, expected_ratio)
  testthat::expect_equal(
    feed$feed_use_fraction,
    feed$feed_assignment_distributed * feed$distributed_per_recovered
  )
  testthat::expect_true(all(feed$feed_use_fraction <= 1))
})

test_that("the crop rows map onto the categories they name", {
  feed <- dplyr::distinct(feed_table(), cat_krausmann, tab_3_20_row)
  row_of <- stats::setNames(feed$tab_3_20_row, feed$cat_krausmann)
  testthat::expect_equal(row_of[["Rice, Paddy"]], "Rice straw")
  for (cereal in c("Wheat, other cereals", "Maize", "Sorghum", "Millet")) {
    testthat::expect_equal(
      row_of[[cereal]],
      "Other cereals straw and stover"
    )
  }
  testthat::expect_equal(row_of[["Cassava"]], "Cassava leaves")
  testthat::expect_equal(row_of[["Roots and Tubers"]], "Potato tops")
  testthat::expect_equal(row_of[["Sugar Cane"]], "Sugar cane tops")
  testthat::expect_equal(row_of[["Sugar Beets"]], "Sugar beet tops")
  testthat::expect_equal(row_of[["Groundnuts in Shell"]], "Groundnut stalks")
  for (oil in c("Soybeans", "Sunflower Seed", "Rapeseed, oil crops")) {
    testthat::expect_equal(row_of[[oil]], "Other oil crops straw and stalks")
  }
})

test_that("a category with no Table 3.20 row carries no value and says so", {
  feed <- feed_table()
  silent <- dplyr::filter(feed, is.na(tab_3_20_row))
  testthat::expect_setequal(
    unique(silent$cat_krausmann),
    c(
      "Beans, Dry",
      "Pulses",
      "Castor Beans",
      "Fodder crops",
      "Permanent crops",
      "Oil Palm Fruit",
      "Sugar Crops nes"
    )
  )
  testthat::expect_true(all(is.na(silent$feed_use_fraction)))
  testthat::expect_true(all(stringr::str_detect(
    silent$source_feed,
    "^no Wirsenius 2000 Tab\\.3\\.20 row"
  )))
  sourced <- dplyr::filter(feed, !is.na(tab_3_20_row))
  testthat::expect_true(all(stringr::str_detect(
    sourced$source_feed,
    "^Wirsenius 2000 Tab\\.3\\.20"
  )))
})

test_that("every regions_full pair resolves to one of Wirsenius's regions", {
  pairs <- whep::regions_full |>
    dplyr::filter(!is.na(.data$region_krausmann)) |>
    dplyr::distinct(.data$region_krausmann, .data$region_UN_sub) |>
    dplyr::mutate(
      region_hanpp = whep:::.residue_recovery_region(.data$region_krausmann)
    )
  resolved <- whep:::.residue_wirsenius_region(
    pairs$region_hanpp,
    pairs$region_UN_sub
  )
  testthat::expect_length(resolved, nrow(pairs))
  testthat::expect_true(all(resolved %in% wirsenius_regions))

  overrides <- whep::whep_coef_table("residue_feed_regions")
  testthat::expect_equal(
    nrow(dplyr::distinct(overrides, region_hanpp, region_un_sub)),
    nrow(overrides)
  )
  testthat::expect_true(all(overrides$region_wirsenius %in% wirsenius_regions))
})

test_that("areas Table 3.1 places away from their HANPP label are moved", {
  # Table 3.1, p. 58: Southeast Asia is East Asia, Russia and Belarus are East
  # Europe, the Caucasus and Sudan are North Africa & West Asia. HANPP files
  # all but Sudan under South and Central Asia, whose cereal feed share is
  # 0.80 against East Asia's 0.30 and East Europe's 0.10.
  areas <- c(
    "Indonesia" = "East Asia",
    "Viet Nam" = "East Asia",
    "Russian Federation" = "East Europe",
    "Belarus" = "East Europe",
    "Georgia" = "North Africa and West Asia",
    "Sudan (former)" = "North Africa and West Asia",
    "USSR" = "East Europe",
    "Czechoslovakia" = "East Europe",
    "India" = "South and Central Asia",
    "Kazakhstan" = "South and Central Asia",
    "France" = "West Europe",
    "Kenya" = "Sub-saharan Africa"
  )
  regions <- whep::regions_full |>
    dplyr::filter(.data$name %in% names(areas)) |>
    dplyr::distinct(.data$name, .keep_all = TRUE)
  testthat::expect_setequal(regions$name, names(areas))
  resolved <- whep:::.residue_wirsenius_region(
    whep:::.residue_recovery_region(regions$region_krausmann),
    regions$region_UN_sub
  )
  testthat::expect_equal(resolved, unname(areas[regions$name]))
})
