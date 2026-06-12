test_that("conv_bouwman has the expected schema and feed types", {
  d <- whep::conv_bouwman
  expect_true(all(
    c("item_bouwman", "feed_type", "year", "region_bouwman", "conversion") %in%
      names(d)
  ))
  expect_setequal(
    unique(d$feed_type),
    c("animals", "crops", "residues", "grass", "scavenging")
  )
  expect_true(all(d$conversion >= 0))
})

test_that("conv_krausmann gives positive per head intake", {
  d <- whep::conv_krausmann
  expect_true(all(c("item_cbs_code", "species", "conversion") %in% names(d)))
  expect_true(all(d$conversion >= 0))
})

test_that("feed_taxonomy maps items to codes without row explosion", {
  d <- whep::feed_taxonomy
  expect_equal(anyDuplicated(d$item_cbs), 0L) # one row per item, no join explosion
  # every item has a CBS code except non-CBS feed items (e.g. ethanol)
  expect_true(all(!is.na(d$item_cbs_code) | d$feed_quality == "non_feed"))
  expect_true(all(
    c(
      "item_cbs_code",
      "item_cbs",
      "feed_group",
      "feed_quality",
      "feed_quality_rank",
      "granivore_feedtype",
      "grazer_feedtype",
      "zoot_fixed"
    ) %in%
      names(d)
  ))
})

test_that("feed_taxonomy quality classes are snake_case and ranked", {
  d <- whep::feed_taxonomy
  expect_true(all(d$feed_quality == tolower(d$feed_quality)))
  ranks <- d |>
    dplyr::filter(feed_quality == "grass") |>
    dplyr::pull(feed_quality_rank) |>
    unique()
  expect_equal(ranks, 4L)
  expect_true(all(d$zoot_fixed == (d$feed_quality == "zoot_fixed")))
})

test_that("max_intake_share caps are in [0, 1]", {
  d <- whep::max_intake_share
  expect_true(all(
    c("livestock_category", "var", "var_value", "max_intake_share") %in%
      names(d)
  ))
  expect_true(all(d$max_intake_share >= 0 & d$max_intake_share <= 1))
})
