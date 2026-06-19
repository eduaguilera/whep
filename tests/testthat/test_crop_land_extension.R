test_that("cropland_apportion splits cell cropland by harvested share", {
  gridded_crops <- tibble::tribble(
    ~lon, ~lat, ~year, ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    0.25, 50.25, 2000L, 1L, 15L, 600, 0,
    0.25, 50.25, 2000L, 1L, 27L, 200, 0,
    0.75, 50.25, 2000L, 1L, 15L, 400, 0
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000,
    0.75, 50.25, 2000L, 500
  )
  items <- tibble::tribble(
    ~item_prod_code, ~item_cbs_code,
    15L, 2511L,
    27L, 2805L
  )

  res <- whep::build_crop_land_extension(
    gridded_crops,
    gridded_cropland,
    items_prod_full = items
  )

  # cell 1 cropland 1000 split 600:200 -> 750:250; cell 2 (500) all to crop 15
  expect_equal(
    res$impact_u[res$item_cbs_code == 2511L],
    750 + 500
  )
  expect_equal(
    res$impact_u[res$item_cbs_code == 2805L],
    250
  )
  # physical land conserves to total cropland of cells that contain crops
  expect_equal(sum(res$impact_u), 1500)
  expect_true(all(res$method_land == "cropland_apportion"))
})

test_that("intensity_divide divides harvested by multi-cropping factor", {
  gridded_crops <- tibble::tribble(
    ~lon, ~lat, ~year, ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    0.25, 50.25, 2000L, 1L, 15L, 600, 200
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000
  )
  multicropping <- tibble::tribble(
    ~lon, ~lat, ~mc_rainfed, ~mc_irrigated,
    0.25, 50.25, 2, 1
  )
  items <- tibble::tribble(~item_prod_code, ~item_cbs_code, 15L, 2511L)

  res <- whep::build_crop_land_extension(
    gridded_crops,
    gridded_cropland,
    items_prod_full = items,
    method = "intensity_divide",
    multicropping = multicropping
  )

  # 600 / 2 (rainfed) + 200 / 1 (irrigated) = 500
  expect_equal(res$impact_u, 500)
  expect_true(all(res$method_land == "intensity_divide"))
})

test_that("crops without a CBS mapping are dropped with a warning", {
  gridded_crops <- tibble::tribble(
    ~lon, ~lat, ~year, ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    0.25, 50.25, 2000L, 1L, 15L, 500, 0,
    0.25, 50.25, 2000L, 1L, 99L, 500, 0
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000
  )
  items <- tibble::tribble(~item_prod_code, ~item_cbs_code, 15L, 2511L)

  expect_warning(
    res <- whep::build_crop_land_extension(
      gridded_crops,
      gridded_cropland,
      items_prod_full = items
    ),
    "item_cbs_code"
  )
  expect_equal(nrow(res), 1L)
  expect_equal(res$item_cbs_code, 2511L)
})

test_that("intensity_divide requires multicropping", {
  gridded_crops <- tibble::tribble(
    ~lon, ~lat, ~year, ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    0.25, 50.25, 2000L, 1L, 15L, 500, 0
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000
  )
  items <- tibble::tribble(~item_prod_code, ~item_cbs_code, 15L, 2511L)

  expect_error(
    whep::build_crop_land_extension(
      gridded_crops,
      gridded_cropland,
      items_prod_full = items,
      method = "intensity_divide"
    ),
    "multicropping"
  )
})

test_that("missing input columns abort", {
  bad <- tibble::tibble(lon = 0, lat = 0)
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000
  )
  expect_error(
    whep::build_crop_land_extension(bad, gridded_cropland),
    "gridded_crops"
  )
})

test_that("get_crop_land_extension(example = TRUE) returns the expected shape", {
  res <- whep::get_crop_land_extension(example = TRUE)
  expect_true(all(
    c("year", "area_code", "item_cbs_code", "impact_u", "method_land") %in%
      names(res)
  ))
  expect_true(all(res$impact_u > 0))
})

test_that("build_cropgrids_land_extension applies per-crop physical ratio", {
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 33L, 2511L, 1000,
    2000L, 33L, 2807L, 500
  )
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    33L, 2511L, 990, 1000,
    33L, 2807L, 400, 500
  )
  res <- whep::build_cropgrids_land_extension(harvested, cropgrids)
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 990) # 1000 * 0.99
  expect_equal(res$impact_u[res$item_cbs_code == 2807L], 400) # 500 * 0.80
  expect_true(all(res$method_land == "cropgrids"))
})

test_that("build_cropgrids_land_extension falls back to the global item ratio", {
  # area 99 absent from cropgrids -> use the item's global physical/harvested
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 99L, 2807L, 1000
  )
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    33L, 2807L, 800, 1000,
    50L, 2807L, 800, 1000
  )
  res <- whep::build_cropgrids_land_extension(harvested, cropgrids)
  expect_equal(res$impact_u, 800) # global ratio 1600/2000 = 0.8
})

test_that("build_cropgrids_land_extension validates harvested columns", {
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    33L, 2511L, 990, 1000
  )
  expect_error(
    whep::build_cropgrids_land_extension(
      tibble::tibble(year = 2000L, area_code = 33L),
      cropgrids
    ),
    "harvested"
  )
})

test_that("attribute_fallow_to_crops distributes fallow by allocation weight", {
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    1L, 2511L, 500, 500,
    1L, 2807L, 400, 400
  )
  fallow_total <- tibble::tribble(~area_code, ~fallow_ha, 1L, 200)
  alloc_weight <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~weight,
    1L, 2511L, 1, # all weight on wheat
    1L, 2807L, 0
  )
  res <- whep::attribute_fallow_to_crops(cropgrids, fallow_total, alloc_weight)
  expect_equal(res$physical_ha[res$item_cbs_code == 2511L], 700) # 500 + 200
  expect_equal(res$physical_ha[res$item_cbs_code == 2807L], 400)
  expect_equal(sum(res$physical_ha), 900 + 200)
})

test_that("attribute_fallow_to_crops leaves crops unchanged when fallow is zero", {
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    1L, 2511L, 500, 500
  )
  fallow_total <- tibble::tribble(~area_code, ~fallow_ha, 1L, 0)
  alloc_weight <- tibble::tribble(~area_code, ~item_cbs_code, ~weight, 1L, 2511L, 1)
  res <- whep::attribute_fallow_to_crops(cropgrids, fallow_total, alloc_weight)
  expect_equal(res$physical_ha, 500)
})

test_that("gridded_fallow_weights scores rainfed crops by agro-climatic zone", {
  gridded_crops <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~rainfed_ha,
    0.25, 50.25, 1L, 2511L, 100, # wheat, arid cell
    0.25, 50.25, 1L, 2536L, 100 # sugar cane (minor), same cell
  )
  grid_aez <- tibble::tribble(~lon, ~lat, ~lgp, ~thermal, 0.25, 50.25, 60, 7L)
  propensity <- tibble::tribble(
    ~item_cbs_code, ~zone, ~fallow_propensity,
    2511L, "arid", 1.0,
    2536L, "arid", 0.1
  )
  w <- whep::gridded_fallow_weights(gridded_crops, grid_aez, propensity)
  expect_equal(w$weight[w$item_cbs_code == 2511L], 100) # 100 * 1.0
  expect_equal(w$weight[w$item_cbs_code == 2536L], 10) # 100 * 0.1
})

test_that("gridded_fallow_weights uses the cell's agro-climatic zone", {
  # same crop, two cells: arid (high propensity) vs humid (low) -> different weight
  gridded_crops <- tibble::tribble(
    ~lon, ~lat, ~area_code, ~item_cbs_code, ~rainfed_ha,
    0.25, 50.25, 1L, 2511L, 100, # arid cell
    0.75, 50.25, 1L, 2511L, 100 # humid cell
  )
  grid_aez <- tibble::tribble(
    ~lon, ~lat, ~lgp, ~thermal,
    0.25, 50.25, 60, 7L, # arid (LGP < 90)
    0.75, 50.25, 320, 7L # humid (LGP >= 270)
  )
  propensity <- tibble::tribble(
    ~item_cbs_code, ~zone, ~fallow_propensity,
    2511L, "arid", 1.0,
    2511L, "humid", 0.05
  )
  w <- whep::gridded_fallow_weights(gridded_crops, grid_aez, propensity)
  expect_equal(w$weight, 100 * 1.0 + 100 * 0.05) # 105
})
