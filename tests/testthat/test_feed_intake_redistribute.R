test_that("get_feed_intake routes the unimplemented redistribute path to an error", {
  expect_error(
    whep::get_feed_intake(grain = "provincial"),
    "not yet implemented"
  )
  expect_error(
    whep::get_feed_intake(demand_tier = "ipcc"),
    "not yet implemented"
  )
  expect_error(
    whep::get_feed_intake(grain = "provincial", demand_tier = "ipcc"),
    "not yet implemented"
  )
})

test_that("get_feed_intake rejects unknown grain / demand_tier", {
  expect_error(whep::get_feed_intake(grain = "global"))
  expect_error(whep::get_feed_intake(demand_tier = "bouwman"))
})

test_that("get_feed_intake(example = TRUE) is unchanged by the new arguments", {
  expect_identical(
    whep::get_feed_intake(example = TRUE),
    whep::get_feed_intake(
      example = TRUE,
      grain = "national",
      demand_tier = "fcr"
    )
  )
})

test_that(".livestock_crosswalk is the verified 22-row mapping", {
  cw <- whep:::.livestock_crosswalk()
  expect_equal(nrow(cw), 22L)
  expect_setequal(unique(cw$demand_source), c("ipcc", "krausmann"))
  ipcc <- cw[cw$demand_source == "ipcc", ]
  expect_setequal(
    unique(ipcc$energy_species),
    c("Cattle", "Buffalo", "Sheep", "Goats", "Pigs", "Poultry")
  )
  expect_true(all(is.na(cw$energy_species[cw$demand_source == "krausmann"])))
  expect_true(all(!is.na(cw$live_anim_code)))
  expect_true(all(nzchar(cw$livestock_category)))
})
