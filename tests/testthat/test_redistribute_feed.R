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

test_that("exact provincial item match allocates without exceeding availability", {
  out <- whep::redistribute_feed(
    whep:::.example_feed_demand() |> dplyr::filter(item_cbs_code == 2555L),
    whep:::.example_feed_avail() |> dplyr::filter(item_cbs_code == 2555L)
  )
  expect_equal(sum(out$intake_dm_t), 50, tolerance = 1e-6)
  expect_true(all(out$intake_dm_t >= 0))
})
