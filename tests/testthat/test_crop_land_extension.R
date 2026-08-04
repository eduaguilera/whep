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

  # rainfed 600 at intensity 2 plus irrigated 200 at intensity 1 gives 500
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
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 990) # ratio 0.99 of 1000
  expect_equal(res$impact_u[res$item_cbs_code == 2807L], 400) # ratio 0.80 of 500
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

test_that("build_cropgrids_land_extension ignores tiny-area CROPGRIDS stubs", {
  # area 44 is a few-hectare CROPGRIDS rounding stub with an implausible ratio;
  # below min_cropgrids_ha it must fall through to the global item ratio rather
  # than use (the cap of) its own stub ratio.
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 44L, 2807L, 1000
  )
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    33L, 2807L, 900, 1000, # real producer
    44L, 2807L, 50, 5 # stub: 5 ha harvested, ratio 10 (would cap to 1.5)
  )
  res <- whep::build_cropgrids_land_extension(harvested, cropgrids)
  # global ratio = (900+50)/(1000+5) = 0.945 -> impact_u ~ 945, NOT 1500
  expect_equal(round(res$impact_u), 945)
})

test_that(".cropgrids_to_polity_area re-keys CROPGRIDS onto polity area codes", {
  # The cropgrids-land pin is built at CROPGRIDS' own reporting grain, the raw
  # FAOSTAT `code`, but `harvested` comes from production, which is keyed on
  # `polity_area_code`. Sudan (276) and South Sudan (277) both map to
  # polity_area_code 206, a code the pin never carries, so before this re-key
  # neither matched `harvested` and both silently took the global per-item ratio
  # instead of their own CROPGRIDS multi-cropping ratio.
  #
  # MEASURED on the shipped pin: 7 areas, 276 of 6269 rows, 1.71% of pin
  # harvested area were mis-keyed; re-keying moves Sudan's crop land extension
  # +4.78% (18.04 -> 18.90 Mha, using the pin's own harvested area as a proxy
  # base). The pin sits behind whep_read_file(), so this exercises the helper
  # against a fixture rather than the shipped data.
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    276L, 2518L, 999, 1000, # Sudan, sorghum
    277L, 2518L, 240, 300, # South Sudan, sorghum
    33L, 2518L, 500, 1000, # Canada: code == polity_area_code, untouched
    8888L, 2518L, 90, 100 # not in the crosswalk at all
  )
  res <- whep:::.cropgrids_to_polity_area(cropgrids)

  expect_false(any(c(276L, 277L) %in% res$area_code))
  expect_true(206L %in% res$area_code)
  sdn <- res[res$area_code == 206L, ]
  expect_equal(nrow(sdn), 1L)
  # Physical and harvested area are summed BEFORE the ratio is taken, so the
  # merged ratio is the area-weighted 1239/1300 = 0.953, not the unweighted mean
  # of 0.999 and 0.8.
  expect_equal(sdn$physical_ha, 1239)
  expect_equal(sdn$harvested_ha, 1300)
  expect_equal(res$harvested_ha[res$area_code == 33L], 1000)
  # An area the crosswalk cannot resolve keeps its own code instead of being
  # dropped, so the re-key degrades to the previous behaviour rather than losing
  # the area. This guards the coalesce fallback, which nothing else exercises.
  expect_true(8888L %in% res$area_code)
  # The re-key is an aggregation, so it neither creates nor loses area.
  expect_equal(sum(res$physical_ha), sum(cropgrids$physical_ha))
  expect_equal(sum(res$harvested_ha), sum(cropgrids$harvested_ha))
})

test_that(".cropgrids_to_polity_area is idempotent", {
  # The pin may one day be rebuilt directly on polity_area_code. Re-keying an
  # already-re-keyed table must then be a no-op rather than aggregate twice.
  # This holds because every polity_area_code in polity_area_crosswalk maps to
  # itself: checked over all 266 crosswalk area codes, 0 non-identity targets.
  # Verified against the real pin too (6101 rows unchanged on a second pass).
  cropgrids <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    276L, 2518L, 999, 1000,
    277L, 2518L, 240, 300,
    206L, 2511L, 300, 305, # already a polity_area_code
    999L, 2807L, 80, 100 # the ROW aggregation bucket
  )
  once <- whep:::.cropgrids_to_polity_area(cropgrids)
  twice <- whep:::.cropgrids_to_polity_area(once)
  expect_equal(twice, once)
})

test_that(".read_cropgrids_land re-keys both CROPGRIDS pins on read", {
  # The re-key has to happen at the read boundary, because that is where the
  # raw-FAOSTAT-keyed pins enter the package. Both aliases must get it: the
  # cropgrids-fallow-land pin is built from cropgrids-land, so it inherits the
  # same keying. The pins are remote, so mock the read.
  pin <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    276L, 2518L, 999, 1000,
    277L, 2518L, 240, 300
  )
  seen <- character()
  testthat::local_mocked_bindings(
    whep_read_file = function(file_alias, ...) {
      seen <<- c(seen, file_alias)
      pin
    }
  )
  for (source in c("cropgrids", "cropgrids_fallow")) {
    res <- whep:::.read_cropgrids_land(source)
    expect_equal(res$area_code, 206L)
    expect_equal(res$physical_ha, 1239)
    expect_equal(res$harvested_ha, 1300)
  }
  expect_equal(seen, c("cropgrids-land", "cropgrids-fallow-land"))
})

test_that("re-keyed CROPGRIDS gives merged areas their own ratio", {
  # End-to-end statement of the defect: harvested area keyed on polity_area_code
  # 206 finds no 206 row in a raw-FAOSTAT-keyed table and falls through to the
  # global per-item ratio, which here is dragged far below Sudan's actual one by
  # a heavily multi-cropped third area.
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2020L, 206L, 2518L, 1000
  )
  raw_keyed <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
    276L, 2518L, 999, 1000,
    277L, 2518L, 240, 300,
    33L, 2518L, 500, 1000
  )
  fallback <- whep::build_cropgrids_land_extension(harvested, raw_keyed)
  # global ratio = (999 + 240 + 500) / (1000 + 300 + 1000) = 1739/2300 = 0.756
  expect_equal(fallback$impact_u, 1000 * 1739 / 2300)

  merged <- whep::build_cropgrids_land_extension(
    harvested,
    whep:::.cropgrids_to_polity_area(raw_keyed)
  )
  # Sudan's own merged ratio, 1239/1300 = 0.953, now applies instead.
  expect_equal(merged$impact_u, 1000 * 1239 / 1300)
  expect_gt(merged$impact_u, fallback$impact_u)
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
  expect_equal(res$physical_ha[res$item_cbs_code == 2511L], 700) # 500 cropped plus 200 fallow
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

test_that("build_hayr_land_extension occupation counts every harvest", {
  # rice double-cropped (200 ha harvested on its fields), wheat single-cropped
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 1L, 2807L, 200,
    2000L, 1L, 2511L, 100
  )
  season <- tibble::tribble(
    ~item_cbs_code, ~season_months,
    2807L, 5,
    2511L, 8
  )
  fallow <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~fallow_ha,
    2000L, 1L, 2807L, 0,
    2000L, 1L, 2511L, 0
  )
  res <- whep::build_hayr_land_extension(harvested, fallow, season)
  # occupation is harvested area times cycle fraction of the year
  expect_equal(res$impact_u[res$item_cbs_code == 2807L], 200 * 5 / 12)
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 100 * 8 / 12)
  expect_true(all(res$method_land == "cropgrids_fallow_hayr"))
})

test_that("build_hayr_land_extension adds rotational fallow", {
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 1L, 2511L, 100
  )
  season <- tibble::tribble(~item_cbs_code, ~season_months, 2511L, 6)
  fallow <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~fallow_ha,
    2000L, 1L, 2511L, 30
  )
  res <- whep::build_hayr_land_extension(harvested, fallow, season)
  # growing 100*6/12 = 50, plus 30 fallow = 80
  expect_equal(res$impact_u, 50 + 30)
})

test_that("build_hayr_land_extension base = 'cropgrids' excludes fallow", {
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 1L, 2807L, 200
  )
  season <- tibble::tribble(~item_cbs_code, ~season_months, 2807L, 6)
  res <- whep::build_hayr_land_extension(
    harvested,
    season = season,
    base = "cropgrids"
  )
  expect_equal(res$impact_u, 200 * 6 / 12) # growing only, no fallow
  expect_true(all(res$method_land == "cropgrids_hayr"))
})

test_that("build_hayr_land_extension drops grass and validates inputs", {
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 1L, 2511L, 100,
    2000L, 1L, 3000L, 500 # grassland item, excluded
  )
  season <- tibble::tribble(~item_cbs_code, ~season_months, 2511L, 6)
  res <- whep::build_hayr_land_extension(
    harvested,
    season = season,
    base = "cropgrids"
  )
  expect_false(3000L %in% res$item_cbs_code)
  expect_equal(res$item_cbs_code, 2511L)
  expect_error(
    whep::build_hayr_land_extension(
      tibble::tibble(year = 2000L, area_code = 1L),
      season = season,
      base = "cropgrids"
    ),
    "harvested"
  )
})

test_that("build_hayr_land_extension rejects a degenerate season table", {
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 1L, 2807L, 100
  )
  fallow <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~fallow_ha, 2000L, 1L, 2807L, 0
  )
  expect_error(
    whep::build_hayr_land_extension(
      harvested,
      fallow,
      tibble::tibble(item_cbs_code = integer(0), season_months = numeric(0))
    ),
    "no usable"
  )
  expect_error(
    whep::build_hayr_land_extension(
      harvested,
      fallow,
      tibble::tribble(~item_cbs_code, ~season_months, 2807L, 5, 2807L, 6)
    ),
    "duplicate"
  )
  expect_error(
    whep::build_hayr_land_extension(
      harvested,
      fallow,
      tibble::tribble(~item_cbs_code, ~season_months, 2807L, 0)
    ),
    "positive"
  )
})

test_that("build_hayr_land_extension warns on crops with no MIRCA season", {
  harvested <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
    2000L, 1L, 2511L, 100,
    2000L, 1L, 9998L, 100,
    2000L, 1L, 9999L, 100
  )
  season <- tibble::tribble(~item_cbs_code, ~season_months, 2511L, 8)
  expect_warning(
    whep::build_hayr_land_extension(
      harvested,
      season = season,
      base = "cropgrids"
    ),
    "no MIRCA season"
  )
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
  expect_equal(w$weight[w$item_cbs_code == 2511L], 100) # 100 ha at propensity 1.0
  expect_equal(w$weight[w$item_cbs_code == 2536L], 10) # 100 ha at propensity 0.1
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
    0.25, 50.25, 60, 7L, # arid zone, LGP below 90
    0.75, 50.25, 320, 7L # humid zone, LGP at least 270
  )
  propensity <- tibble::tribble(
    ~item_cbs_code, ~zone, ~fallow_propensity,
    2511L, "arid", 1.0,
    2511L, "humid", 0.05
  )
  w <- whep::gridded_fallow_weights(gridded_crops, grid_aez, propensity)
  expect_equal(w$weight, 100 * 1.0 + 100 * 0.05) # 105
})

# --- FAO fallow-inclusive arable / permanent land base ------------------------

# Minimal FAOSTAT RL land-use fixture in the raw pin schema, using real
# single-polity area codes (222 Tunisia, 174 Portugal, 3 Albania, 4 Algeria)
# plus the China aggregate 351 (must be dropped).
.rl_fixture <- function() {
  tibble::tribble(
    ~`Area Code`, ~`Item Code`, ~Element, ~Unit, ~Year, ~Value,
    222, 6620, "Area", "1000 ha", 2020, 4950.5, # TUN cropland
    222, 6621, "Area", "1000 ha", 2020, 2831.3, # TUN arable
    222, 6650, "Area", "1000 ha", 2020, 2119.2, # TUN permanent (reported)
    174, 6620, "Area", "1000 ha", 2020, 1841.0, # PRT cropland
    174, 6621, "Area", "1000 ha", 2020, 972.9, # PRT arable
    174, 6650, "Area", "1000 ha", 2020, 868.1, # PRT permanent (reported)
    3, 6620, "Area", "1000 ha", 2020, 1000.0, # ALB cropland
    3, 6621, "Area", "1000 ha", 2020, 700.0, # ALB arable, no permanent reported
    4, 6620, "Area", "1000 ha", 2020, 500.0, # DZA-code cropland
    4, 6650, "Area", "1000 ha", 2020, 120.0, # permanent, no arable reported
    351, 6620, "Area", "1000 ha", 2020, 99999.0, # China aggregate -> drop
    351, 6621, "Area", "1000 ha", 2020, 88888.0
  )
}

test_that("get_arable_permanent_land reconstructs FAO cropland = arable + permanent", {
  ap <- whep::get_arable_permanent_land(data = .rl_fixture(), years = 2020)
  # arable + permanent == cropland exactly (all fixture rows have arable<=cropland)
  expect_equal(ap$arable_ha + ap$permanent_ha, ap$cropland_ha)
  # missing-permanent country: permanent filled from cropland - arable
  alb <- ap[ap$area_code == 3L, ]
  expect_equal(alb$permanent_ha, 300000) # inferred remainder, converted to ha
  # missing-arable country: arable filled from cropland - permanent
  dza <- ap[ap$area_code == 4L, ]
  expect_equal(dza$arable_ha, 380000) # inferred remainder, converted to ha
  expect_true(all(ap$source == "fao"))
})

test_that("reported zero land components remain distinct from missing reports", {
  zero_components <- tibble::tribble(
    ~`Area Code`, ~`Item Code`, ~Element, ~Unit, ~Year, ~Value,
    3, 6620, "Area", "1000 ha", 2020, 10,
    3, 6621, "Area", "1000 ha", 2020, 0,
    4, 6620, "Area", "1000 ha", 2020, 10,
    4, 6650, "Area", "1000 ha", 2020, 0
  )

  ap <- whep::get_arable_permanent_land(
    data = zero_components,
    years = 2020L
  )
  all_permanent <- ap[ap$area_code == 3L, ]
  all_arable <- ap[ap$area_code == 4L, ]

  expect_equal(all_permanent$arable_ha, 0)
  expect_equal(all_permanent$permanent_ha, 10000)
  expect_equal(all_arable$arable_ha, 10000)
  expect_equal(all_arable$permanent_ha, 0)
})

test_that("get_arable_permanent_land drops the FAOSTAT China aggregate 351", {
  ap <- whep::get_arable_permanent_land(data = .rl_fixture(), years = 2020)
  expect_false(351L %in% ap$area_code)
})

test_that("get_arable_permanent_land gives TUN/PRT the physical permanent share (~0.43/0.47, not 0.73)", {
  ap <- whep::get_arable_permanent_land(data = .rl_fixture(), years = 2020)
  tun <- ap[ap$area_code == 222L, ]
  prt <- ap[ap$area_code == 174L, ]
  tun_share <- tun$permanent_ha / tun$cropland_ha
  prt_share <- prt$permanent_ha / prt$cropland_ha
  # Tunisia physical permanent share is ~0.43, NOT the ~0.73 of harvested-area
  # methods.
  expect_gt(tun_share, 0.40)
  expect_lt(tun_share, 0.46)
  expect_lt(tun_share, 0.60) # unambiguously below the harvested-area 0.73
  # Portugal recent permanent share ~0.47, no spurious step change.
  expect_gt(prt_share, 0.40)
  expect_lt(prt_share, 0.50)
})

test_that("get_arable_permanent_land backcasts pre-1961 from LUH2, spliced at 1961", {
  fao <- tibble::tribble(
    ~`Area Code`, ~`Item Code`, ~Element, ~Unit, ~Year, ~Value,
    222, 6620, "Area", "1000 ha", 1961, 4000.0, # TUN cropland 1961
    222, 6621, "Area", "1000 ha", 1961, 3000.0 # TUN arable 1961 (perm frac 0.25)
  )
  # LUH2 fixture for Tunisia (iso3c TUN -> polity 222): flat perennial fraction,
  # cropland shrinking backwards.
  luh2 <- tibble::tribble(
    ~ISO3, ~Year, ~Land_Use, ~Area_Mha,
    "TUN", 1959, "c3ann", 0.75, "TUN", 1959, "c3per", 0.25,
    "TUN", 1960, "c3ann", 0.78, "TUN", 1960, "c3per", 0.26,
    "TUN", 1961, "c3ann", 0.80, "TUN", 1961, "c3per", 0.27
  )
  ap <- whep::get_arable_permanent_land(
    data = fao,
    luh2_data = luh2,
    years = 1959:1961
  )
  pre <- ap[ap$year < 1961, ]
  expect_true(all(pre$source == "luh2"))
  expect_true(all(ap$year[ap$source == "fao"] >= 1961))
  # splice continuity: 1960 cropland is close to the FAO 1961 level, no jump
  c1960 <- ap$cropland_ha[ap$year == 1960]
  c1961 <- ap$cropland_ha[ap$year == 1961]
  expect_lt(abs(c1960 - c1961) / c1961, 0.05)
  # perennial fraction spliced to the FAO 1961 value (0.25) at the boundary
  s1960 <- ap$permanent_ha[ap$year == 1960] / ap$cropland_ha[ap$year == 1960]
  expect_lt(abs(s1960 - 0.25), 0.02)
})

test_that("land-base readers accept snake case without mutating supplied data tables", {
  fao <- data.table::data.table(
    area_code = c(3L, 3L),
    item_code = c(6620L, 6621L),
    element = "Area",
    unit = "1000 ha",
    year = 2020L,
    value = c(10, 7)
  )
  fao_names <- names(fao)
  ap <- whep::get_arable_permanent_land(data = fao, years = 2020L)

  expect_named(fao, fao_names)
  expect_equal(ap$arable_ha, 7000)
  expect_equal(ap$permanent_ha, 3000)

  luh2 <- data.table::data.table(
    ISO3 = c("TUN", "PRT", "PRT"),
    Year = 1960L,
    Land_Use = c("c3ann", "c3ann", "c3per"),
    Area_Mha = c(1, 1, 0.2)
  )
  luh2_names <- names(luh2)
  cft <- whep:::.read_luh2_cft(luh2)
  tun <- cft[cft$area_code == 222L, ]

  expect_named(luh2, luh2_names)
  expect_equal(tun$annual, 1e6)
  expect_equal(tun$perennial, 0)
  expect_equal(tun$luh2_cropland, 1e6)
})

# Fixtures for the per-crop FAO-arable fallow extension.
.fao_fallow_items <- function() {
  tibble::tribble(
    ~item_cbs_code, ~Herb_Woody,
    2511L, "Herbaceous", # wheat (arable)
    2513L, "Herbaceous", # barley (arable)
    2514L, "Herbaceous", # maize (arable)
    2560L, "Woody" # perennial crops such as coconuts and olives
  )
}

# No temporary grassland in the scenario: passed to opt out of CBS 3002 netting
# so reconciliation-mechanics tests stay pin-free (a NULL default would build
# the real grassland occupation extension).
.no_temp_grassland <- function() {
  tibble::tribble(
    ~area_code, ~year, ~item_cbs_code, ~impact_u
  )
}

test_that("build_fao_arable_fallow_extension distributes fallow summing to fallow_total", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 300,
    2020L, 1L, 2513L, 200
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 600, 0
  )
  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    temporary_grassland = .no_temp_grassland(),
    items_prod_full = .fao_fallow_items()
  )
  # fallow_total = 600 - (300 + 200) = 100, distributed by cropped area share
  expect_equal(sum(res$impact_u), 600) # matches the FAO arable total
  expect_equal(sum(res$impact_u) - sum(base$impact_u), 100) # adds all fallow
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 360) # wheat share
  expect_equal(res$impact_u[res$item_cbs_code == 2513L], 240) # barley share
  expect_true(all(res$method_land == "fao_arable_fallow"))
})

test_that("build_fao_arable_fallow_extension gives perennials zero fallow and scales them to FAO permanent", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 300, # wheat (arable)
    2020L, 1L, 2560L, 100 # coconuts (perennial)
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 500, 150
  )
  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    temporary_grassland = .no_temp_grassland(),
    items_prod_full = .fao_fallow_items()
  )
  wheat <- res$impact_u[res$item_cbs_code == 2511L]
  coco <- res$impact_u[res$item_cbs_code == 2560L]
  # all fallow (200) goes to the arable crop; none to the perennial
  expect_equal(wheat, 500) # 300 + fallow 200
  # perennial only scaled to FAO permanent (150), no fallow term
  expect_equal(coco, 150) # 100 * 150 / 100, not 100 + fallow
  expect_equal(sum(res$impact_u[res$item_cbs_code == 2511L]), 500) # arable == FAO arable
})

test_that("build_fao_arable_fallow_extension leaves intensity-1 arable (FRA/USA-like) unchanged", {
  # harvested-derived arable already equals FAO arable: no fallow to add.
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 480,
    2020L, 1L, 2514L, 20,
    2020L, 1L, 2560L, 10
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 500, 10
  )
  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    temporary_grassland = .no_temp_grassland(),
    items_prod_full = .fao_fallow_items()
  )
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 480) # unchanged
  expect_equal(res$impact_u[res$item_cbs_code == 2514L], 20) # unchanged
  expect_equal(res$impact_u[res$item_cbs_code == 2560L], 10) # perennial unchanged
})

test_that("build_fao_arable_fallow_extension scales arable down when cropped physical exceeds FAO arable", {
  # heavy multi-cropping / inflated fodder: cropped arable (800) > FAO arable
  # (600); no fallow to add, so arable is scaled down to the FAO physical
  # container instead.
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 600,
    2020L, 1L, 2513L, 200
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 600, 0
  )
  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    temporary_grassland = .no_temp_grassland(),
    items_prod_full = .fao_fallow_items()
  )
  expect_equal(sum(res$impact_u), 600) # matches the FAO arable total
  expect_equal(res$impact_u[res$item_cbs_code == 2511L], 450) # wheat share
  expect_equal(res$impact_u[res$item_cbs_code == 2513L], 150) # barley share
})

test_that("fallow weights fall back independently for unsupported areas", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 300,
    2020L, 1L, 2513L, 200,
    2020L, 2L, 2511L, 100,
    2020L, 2L, 2513L, 300,
    2020L, 3L, 2511L, 200,
    2020L, 3L, 2513L, 200
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 600, 0,
    2L, 2020L, 500, 0,
    3L, 2020L, 500, 0
  )
  weights <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~weight,
    1L, 2511L, 1,
    1L, 2513L, 0,
    2L, 2511L, 0,
    2L, 2513L, 0
  )

  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    fallow_weights = weights,
    temporary_grassland = .no_temp_grassland(),
    items_prod_full = .fao_fallow_items()
  )

  totals <- res |>
    dplyr::summarise(impact_u = sum(.data$impact_u), .by = area_code)
  expect_equal(totals$impact_u[match(1:3, totals$area_code)], c(600, 500, 500))
  expect_equal(res$impact_u[res$area_code == 1L], c(400, 200))
  expect_equal(res$impact_u[res$area_code == 2L], c(125, 375))
  expect_equal(res$impact_u[res$area_code == 3L], c(250, 250))
})

test_that("impossible arable reconciliation fails explicitly", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 0
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 100, 0
  )
  weights <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~weight,
    1L, 2511L, 0
  )

  expect_error(
    whep::build_fao_arable_fallow_extension(
      base_extension = base,
      arable_permanent = ap,
      fallow_weights = weights,
      temporary_grassland = .no_temp_grassland(),
      items_prod_full = .fao_fallow_items()
    ),
    "Arable totals do not reconcile"
  )
})

test_that("invalid custom weights cannot create negative crop areas", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 100,
    2020L, 1L, 2513L, 300
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 500, 0
  )
  weights <- tibble::tribble(
    ~area_code, ~item_cbs_code, ~weight,
    1L, 2511L, -1,
    1L, 2513L, 2
  )

  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    fallow_weights = weights,
    temporary_grassland = .no_temp_grassland(),
    items_prod_full = .fao_fallow_items()
  )

  expect_equal(res$impact_u, c(125, 375))
  expect_true(all(res$impact_u >= 0))
})

test_that("positive land targets require crop support", {
  perennial_only <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2560L, 100
  )
  arable_target <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 50, 100
  )
  expect_error(
    whep::build_fao_arable_fallow_extension(
      base_extension = perennial_only,
      arable_permanent = arable_target,
      temporary_grassland = .no_temp_grassland(),
      items_prod_full = .fao_fallow_items()
    ),
    "without arable crop rows"
  )

  arable_only <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 100
  )
  permanent_target <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 100, 50
  )
  expect_error(
    whep::build_fao_arable_fallow_extension(
      base_extension = arable_only,
      arable_permanent = permanent_target,
      temporary_grassland = .no_temp_grassland(),
      items_prod_full = .fao_fallow_items()
    ),
    "without positive perennial base area"
  )
})

test_that("build_fao_arable_fallow_extension nets temporary grassland (CBS 3002) so crop + CBS 3002 = FAO arable", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 300,
    2020L, 1L, 2513L, 200
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 600, 0
  )
  # FAO arable 600 already contains 100 ha of temporary grassland (CBS 3002).
  temp <- tibble::tribble(
    ~area_code, ~year, ~item_cbs_code, ~impact_u,
    1L, 2020L, 3002L, 100
  )
  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    temporary_grassland = temp,
    items_prod_full = .fao_fallow_items()
  )
  # ordinary crops reconcile to the netted arable target 600 - 100 = 500
  expect_equal(sum(res$impact_u), 500)
  # the enforced invariant: ordinary crops + CBS 3002 == FAO arable land
  expect_equal(sum(res$impact_u) + sum(temp$impact_u), 600)
})

test_that("build_fao_arable_fallow_extension ignores non-3002 grassland rows (no netting)", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 300,
    2020L, 1L, 2513L, 200
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 600, 0
  )
  # only permanent pasture (CBS 3000); no temporary grassland to net out
  temp <- tibble::tribble(
    ~area_code, ~year, ~item_cbs_code, ~impact_u,
    1L, 2020L, 3000L, 999
  )
  res <- whep::build_fao_arable_fallow_extension(
    base_extension = base,
    arable_permanent = ap,
    temporary_grassland = temp,
    items_prod_full = .fao_fallow_items()
  )
  expect_equal(sum(res$impact_u), 600) # full FAO arable target, unchanged
})

test_that("build_fao_arable_fallow_extension warns and clamps when CBS 3002 exceeds FAO arable", {
  base <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2020L, 1L, 2511L, 300
  )
  ap <- tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    1L, 2020L, 400, 0
  )
  # modelled temporary grassland (500) exceeds FAO arable land (400)
  temp <- tibble::tribble(
    ~area_code, ~year, ~item_cbs_code, ~impact_u,
    1L, 2020L, 3002L, 500
  )
  expect_warning(
    res <- whep::build_fao_arable_fallow_extension(
      base_extension = base,
      arable_permanent = ap,
      temporary_grassland = temp,
      items_prod_full = .fao_fallow_items()
    ),
    "exceeds FAO arable"
  )
  # arable target clamped at 0, so no ordinary crop area survives
  expect_equal(nrow(res), 0L)
})
