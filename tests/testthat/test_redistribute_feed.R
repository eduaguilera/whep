test_that("redistribute_feed rejects missing required demand columns", {
  bad <- whep:::.example_feed_demand() |> dplyr::select(-demand_dm_t)
  expect_error(
    whep::redistribute_feed(bad, whep:::.example_feed_avail()),
    "demand_dm_t"
  )
})

test_that("redistribute_feed rejects non-logical fixed_demand", {
  bad <- whep:::.example_feed_demand() |> dplyr::mutate(fixed_demand = "yes")
  expect_error(
    whep::redistribute_feed(bad, whep:::.example_feed_avail()),
    "fixed_demand"
  )
})
