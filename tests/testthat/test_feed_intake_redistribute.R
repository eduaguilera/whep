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
