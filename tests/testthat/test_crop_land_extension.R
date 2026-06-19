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

test_that("build_hayr_land_extension reweights physical area by cycle length", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 2807L, 100,
    2000L, 1L, 2511L, 100
  )
  season <- tibble::tribble(
    ~item_cbs_code, ~season_months,
    2807L, 5,
    2511L, 8
  )
  # default conserve = "physical": total preserved, redistributed by 5:8
  res <- whep::build_hayr_land_extension(physical, season = season)
  expect_equal(sum(res$impact_u), 200)
  expect_equal(res$impact_u[res$item_cbs_code == 2807L], 200 * 500 / 1300)
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 200 * 800 / 1300)
  expect_gt(
    res$impact_u[res$item_cbs_code == 2511L],
    res$impact_u[res$item_cbs_code == 2807L]
  )
  expect_true(all(res$method_land == "cropgrids_fallow_hayr"))
})

test_that("build_hayr_land_extension is the identity for equal cycle lengths", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 2807L, 300,
    2000L, 1L, 2511L, 100
  )
  season <- tibble::tribble(
    ~item_cbs_code, ~season_months,
    2807L, 6,
    2511L, 6
  )
  res <- whep::build_hayr_land_extension(physical, season = season)
  expect_equal(res$impact_u[res$item_cbs_code == 2807L], 300)
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 100)
})

test_that("build_hayr_land_extension conserves to cropland when requested", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 2807L, 100,
    2000L, 1L, 2511L, 100
  )
  cropland <- tibble::tribble(~year, ~area_code, ~cropland_ha, 2000L, 1L, 150)
  season <- tibble::tribble(
    ~item_cbs_code, ~season_months,
    2807L, 5,
    2511L, 8
  )
  res <- whep::build_hayr_land_extension(
    physical,
    cropland,
    season,
    conserve = "cropland"
  )
  # cropland/physical = 150/200 = 0.75 (within bounds) -> sums to 150
  expect_equal(sum(res$impact_u), 150)
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 150 * 800 / 1300)
})

test_that("build_hayr_land_extension clamps cropland scaling to bounds", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 2807L, 100,
    2000L, 1L, 2511L, 100
  )
  # cropland 1000 vs physical 200 -> raw scale 5, clamped to hi = 2 -> total 400
  cropland <- tibble::tribble(~year, ~area_code, ~cropland_ha, 2000L, 1L, 1000)
  season <- tibble::tribble(
    ~item_cbs_code, ~season_months,
    2807L, 6,
    2511L, 6
  )
  res <- whep::build_hayr_land_extension(
    physical,
    cropland,
    season,
    conserve = "cropland",
    scale_bounds = c(0.5, 2)
  )
  expect_equal(sum(res$impact_u), 400)
})

test_that("build_hayr_land_extension validates physical columns", {
  season <- tibble::tribble(~item_cbs_code, ~season_months, 2807L, 5)
  expect_error(
    whep::build_hayr_land_extension(
      tibble::tibble(year = 2000L, area_code = 1L),
      season = season
    ),
    "physical"
  )
})

test_that("build_hayr_land_extension rejects a degenerate season table", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 2807L, 100
  )
  expect_error(
    whep::build_hayr_land_extension(
      physical,
      season = tibble::tibble(
        item_cbs_code = integer(0),
        season_months = numeric(0)
      )
    ),
    "no usable"
  )
  expect_error(
    whep::build_hayr_land_extension(
      physical,
      season = tibble::tribble(~item_cbs_code, ~season_months, 2807L, 5, 2807L, 6)
    ),
    "duplicate"
  )
  expect_error(
    whep::build_hayr_land_extension(
      physical,
      season = tibble::tribble(~item_cbs_code, ~season_months, 2807L, 0)
    ),
    "positive"
  )
})

test_that("build_hayr_land_extension warns and falls back on missing cropland", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    1950L, 1L, 2807L, 100,
    2000L, 1L, 2807L, 100
  )
  cropland <- tibble::tribble(~year, ~area_code, ~cropland_ha, 2000L, 1L, 80)
  season <- tibble::tribble(~item_cbs_code, ~season_months, 2807L, 5)
  expect_warning(
    res <- whep::build_hayr_land_extension(
      physical,
      cropland,
      season,
      conserve = "cropland"
    ),
    "no FAOSTAT cropland"
  )
  # 1950 has no cropland -> conserves to physical (100); 2000 scales to 80
  expect_equal(res$impact_u[res$year == 1950L], 100)
  expect_equal(res$impact_u[res$year == 2000L], 80)
})

test_that("build_cropgrids_land_extension drops grass items from crop land", {
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 33L, 2511L, 1000,
    2000L, 33L, 3000L, 5000 # grassland -> separate grass extension, must drop
  )
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    33L, 2511L, 990, 1000
  )
  res <- whep::build_cropgrids_land_extension(harvested, cropgrids)
  expect_false(3000L %in% res$item_cbs_code)
  expect_equal(res$item_cbs_code, 2511L)
})

test_that("build_cropgrids_land_extension warns on the ratio=1 coverage fallback", {
  # two absent items, so the cli pluralization + vector listing is exercised
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 33L, 2511L, 1000,
    2000L, 33L, 2050L, 1000, # absent from cropgrids -> ratio 1 fallback
    2000L, 33L, 2051L, 1000 # absent from cropgrids -> ratio 1 fallback
  )
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    33L, 2511L, 990, 1000
  )
  expect_warning(
    res <- whep::build_cropgrids_land_extension(harvested, cropgrids),
    "absent from"
  )
  expect_equal(res$impact_u[res$item_cbs_code == 2050L], 1000)
})

test_that("build_crop_land_extension drops grass items", {
  gridded_crops <- tibble::tribble(
    ~lon, ~lat, ~year, ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
    0.25, 50.25, 2000L, 1L, 15L, 600, 0,
    0.25, 50.25, 2000L, 1L, 99L, 400, 0
  )
  gridded_cropland <- tibble::tribble(
    ~lon, ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 2000L, 1000
  )
  items <- tibble::tribble(
    ~item_prod_code, ~item_cbs_code,
    15L, 2511L,
    99L, 3000L # grassland -> must be dropped
  )
  res <- whep::build_crop_land_extension(
    gridded_crops,
    gridded_cropland,
    items_prod_full = items
  )
  expect_false(3000L %in% res$item_cbs_code)
  expect_equal(res$item_cbs_code, 2511L)
})

test_that("build_hayr_land_extension drops grass and non-positive physical", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 2511L, 100,
    2000L, 1L, 3000L, 100, # grass -> dropped
    2000L, 1L, 2807L, -50 # negative -> dropped
  )
  season <- tibble::tribble(
    ~item_cbs_code, ~season_months,
    2511L, 8, 3000L, 12, 2807L, 5
  )
  res <- whep::build_hayr_land_extension(physical, season = season)
  expect_equal(res$item_cbs_code, 2511L)
  expect_equal(sum(res$impact_u), 100)
})

test_that("build_hayr_land_extension warns when a crop has no MIRCA season", {
  physical <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2000L, 1L, 2511L, 100,
    2000L, 1L, 9998L, 100, # no season row -> median default
    2000L, 1L, 9999L, 100 # no season row -> median default
  )
  season <- tibble::tribble(~item_cbs_code, ~season_months, 2511L, 8)
  expect_warning(
    whep::build_hayr_land_extension(physical, season = season),
    "no MIRCA season"
  )
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
