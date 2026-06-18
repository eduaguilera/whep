test_that("grass_access_shares returns the documented defaults", {
  s <- whep::grass_access_shares()
  expect_equal(s$aboveground, 0.46)
  expect_equal(s$grazable, 1)
  expect_equal(s$w_c_dm, 0.45)
})

test_that(".lpjml_grass_to_dm converts gC/m2/yr to grazable t DM/ha/yr", {
  shares <- whep::grass_access_shares(
    aboveground = 0.46,
    grazable = 1,
    w_c_dm = 0.45
  )
  # 1 gC/m2 = 0.01 tC/ha; x aboveground x grazable / w_c_dm -> t DM/ha.
  expect_equal(
    whep:::.lpjml_grass_to_dm(600, shares),
    600 * 0.46 * 1 * 0.01 / 0.45,
    tolerance = 1e-9
  )
})

test_that("a lower grazable share lowers availability proportionally", {
  full <- whep::grass_access_shares(grazable = 1)
  half <- whep::grass_access_shares(grazable = 0.5)
  expect_equal(
    whep:::.lpjml_grass_to_dm(600, half),
    whep:::.lpjml_grass_to_dm(600, full) / 2,
    tolerance = 1e-9
  )
})

test_that("build_grass_availability_lpjml(example = TRUE) returns the tidy schema", {
  av <- whep::build_grass_availability_lpjml(example = TRUE)
  expect_s3_class(av, "tbl_df")
  expect_setequal(
    names(av),
    c(
      "lon",
      "lat",
      "year",
      "grass_npp_gc_m2",
      "grass_avail_dm_t_ha",
      "grass_avail_dm_t"
    )
  )
  expect_true(all(av$grass_avail_dm_t_ha >= 0))
  expect_true(all(av$grass_avail_dm_t >= 0))
})

test_that("build_grass_availability dispatches lpjml and records the method", {
  av <- whep::build_grass_availability(method = "lpjml", example = TRUE)
  expect_true("method_grass" %in% names(av))
  expect_equal(unique(av$method_grass), "lpjml")
})

test_that("build_grass_availability errors on the unimplemented coefficient method", {
  expect_error(
    whep::build_grass_availability(method = "coefficient"),
    "not yet implemented"
  )
})
