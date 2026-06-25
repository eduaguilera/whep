testthat::test_that("build_grassland_land_extension example has expected structure", {
  result <- whep::build_grassland_land_extension(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_grassland")
  )
  pointblank::expect_col_vals_gt(result, "impact_u", 0)
})

testthat::test_that("luh2 occupation sums grass area and drops crops and fallow", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 3000L, "ha", 1000, # grassland
    2000L, 10L, 3000L, "ha", 500, # grassland, second prod item
    2000L, 10L, 3003L, "ha", 9999, # fallow, excluded
    2000L, 10L, 2511L, "ha", 7777, # wheat crop, excluded
    2000L, 10L, 3000L, "tonnes", 4242 # grass biomass, excluded (not ha)
  )

  result <- whep::build_grassland_land_extension(
    source = "luh2",
    grassland_metric = "occupation",
    data = list(primary_prod = primary_prod)
  )

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$item_cbs_code, 3000L)
  testthat::expect_equal(result$impact_u, 1500)
  testthat::expect_equal(result$method_grassland, "occupation")
})

testthat::test_that("faostat_pasture filters item 6655 from the landuse pin", {
  landuse <- tibble::tribble(
    ~`Area Code`, ~`Item Code`, ~Element, ~Year, ~Value,
    10, 6655, "Area", 2000, 50000, # Australia perm. pasture (1000 ha)
    10, 6620, "Area", 2000, 9999, # cropland -> excluded (wrong item)
    351, 6655, "Area", 2000, 40000 # China aggregate (NA iso3c) -> excluded
  )

  result <- whep::build_grassland_land_extension(
    source = "faostat_pasture",
    data = list(landuse = landuse)
  )

  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(result$area_code, 10L)
  testthat::expect_equal(result$impact_u, 5e7)
  testthat::expect_true(all(result$item_cbs_code == 3000L))
  testthat::expect_equal(result$method_grassland, "occupation")
})

testthat::test_that("active_grazing caps area at grazing intake over yield", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 3000L, "ha", 1000, # capped by intake
    2000L, 20L, 3000L, "ha", 100 # uncapped (intake implies more)
  )
  feed_intake <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~feed_type, ~intake_dry_matter,
    2000L, 10L, 3000L, "grass", 1000, # 1000 / 2 = 500 ha, below 1000
    2000L, 20L, 3000L, "grass", 1000 # 1000 / 2 = 500 ha, above 100
  )

  result <- whep::build_grassland_land_extension(
    source = "luh2",
    grassland_metric = "active_grazing",
    usable_grass_yield_dm_t_ha = 2,
    data = list(primary_prod = primary_prod, feed_intake = feed_intake)
  )

  capped <- dplyr::filter(result, area_code == 10L)
  uncapped <- dplyr::filter(result, area_code == 20L)
  testthat::expect_equal(capped$impact_u, 500)
  testthat::expect_equal(uncapped$impact_u, 100)
  testthat::expect_true(all(result$method_grassland == "active_grazing"))
})

testthat::test_that("active_grazing rejects invalid usable grass yield", {
  primary_prod <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~unit, ~value,
    2000L, 10L, 3000L, "ha", 1000
  )
  feed_intake <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~feed_type, ~intake_dry_matter,
    2000L, 10L, 3000L, "grass", 1000
  )

  testthat::expect_error(
    whep::build_grassland_land_extension(
      grassland_metric = "active_grazing",
      usable_grass_yield_dm_t_ha = -1,
      data = list(primary_prod = primary_prod, feed_intake = feed_intake)
    )
  )
})
