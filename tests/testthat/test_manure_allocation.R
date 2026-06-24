# Field-available manure (output shape of apply_management_losses), two streams.
.toy_applied <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~livestock_category,
    ~stream,
    ~applied_n,
    ~applied_c,
    ~applied_vs,
    2020L,
    "ESP",
    NA,
    "Cattle_milk",
    "collected",
    80,
    800,
    40,
    2020L,
    "ESP",
    NA,
    "Cattle_milk",
    "grazing",
    20,
    380,
    12,
    2020L,
    "ESP",
    NA,
    "Pigs",
    "collected",
    30,
    270,
    18
  )
}

# Cropland layer: two crops with West-2014 receptivity weights and tight caps
# (sum cap 108 t N < collected 110 t N) to force a grassland spill.
.toy_crops <- function() {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~crop,
    ~crop_area_ha,
    ~manure_n_receptivity,
    ~crop_n_cap,
    2020L,
    "ESP",
    NA,
    "barley",
    1000,
    359005,
    50,
    2020L,
    "ESP",
    NA,
    "wheat",
    800,
    257384,
    40
  )
}

.toy_grass <- function(cap = 5) {
  tibble::tribble(
    ~year,
    ~territory,
    ~sub_territory,
    ~grass_area_ha,
    ~grass_n_cap,
    2020L,
    "ESP",
    NA,
    3000,
    cap
  )
}

.toy_gridded <- function(grass_cap = 5) {
  list(crops = .toy_crops(), grass = .toy_grass(grass_cap))
}

test_that("allocate_manure_to_land conserves N, C and VS", {
  res <- whep::allocate_manure_to_land(.toy_applied(), .toy_gridded())
  expect_equal(sum(res$applied_n), 130, tolerance = 1e-8) # collected plus grazed
  expect_equal(sum(res$applied_c), 1450, tolerance = 1e-8) # collected plus grazed
  expect_equal(sum(res$applied_vs), 70, tolerance = 1e-8) # collected plus grazed
})

test_that("grazing excreta is deposited in situ on grassland, uncapped", {
  res <- whep::allocate_manure_to_land(.toy_applied(), .toy_gridded())
  graz <- res[res$source_stream == "grazing", ]
  expect_equal(nrow(graz), 1L)
  expect_equal(graz$land_use, "Grassland")
  expect_true(is.na(graz$crop))
  expect_equal(graz$applied_n, 20)
  expect_equal(graz$applied_c, 380)
  expect_equal(graz$applied_vs, 12)
})

test_that("cropland fills to cap, surplus spills to grassland (cap-and-spill)", {
  res <- whep::allocate_manure_to_land(.toy_applied(), .toy_gridded())
  crop <- res[res$land_use == "Cropland" & res$source_stream == "collected", ]
  # Worked: barley fills to cap 60 (50*1.2), wheat to cap 48 (40*1.2), sum 108.
  expect_equal(crop$applied_n[crop$crop == "barley"], 60, tolerance = 1e-6)
  expect_equal(crop$applied_n[crop$crop == "wheat"], 48, tolerance = 1e-6)
  # No cropland crop row exceeds its cap.
  expect_true(all(crop$applied_n <= c(60, 48) + 1e-6))
  # Leftover 2 t N spills onto grassland (collected stream), within grass cap 5.
  spill <- res[
    res$land_use == "Grassland" & res$source_stream == "collected",
  ]
  expect_equal(spill$applied_n, 2, tolerance = 1e-6)
})

test_that("collected streams carry C and VS at the post-storage bundle ratio", {
  res <- whep::allocate_manure_to_land(.toy_applied(), .toy_gridded())
  coll <- res[res$source_stream == "collected", ]
  # collected bundle: C:N = 1070/110, VS:N = 58/110, applied to every coll row.
  expect_equal(coll$applied_c / coll$applied_n, rep(1070 / 110, nrow(coll)))
  expect_equal(coll$applied_vs / coll$applied_n, rep(58 / 110, nrow(coll)))
})

test_that("over_apply_local routes residual above cap on the source cell", {
  # No grassland sink and tight caps => residual disposed over the local cap.
  g <- list(crops = .toy_crops()) # grass omitted -> grass cap 0
  res <- whep::allocate_manure_to_land(.toy_applied(), g)
  expect_equal(sum(res$applied_n), 130, tolerance = 1e-8)
  over <- res[isTRUE(res$over_cap) | res$over_cap, ]
  over <- res[res$over_cap, ]
  expect_equal(sum(over$applied_n), 2, tolerance = 1e-6) # 110 - 108 cap
  expect_true(all(over$land_use == "Cropland"))
  expect_true(all(res$disposal_method == "over_apply_local"))
})

test_that("disposal warns when a polity disposes more than 5% of collected N", {
  tiny <- .toy_crops()
  tiny$crop_n_cap <- c(10, 10) # *1.2 -> 24 t cap, collected 110 -> ~78% disposed
  expect_warning(
    whep::allocate_manure_to_land(
      .toy_applied(),
      list(crops = tiny)
    ),
    "dispos"
  )
})

test_that("unmanaged_disposal and retain_unallocated keep mass in labelled rows", {
  tiny <- .toy_crops()
  tiny$crop_n_cap <- c(10, 10)
  res_u <- suppressWarnings(whep::allocate_manure_to_land(
    .toy_applied(),
    list(crops = tiny),
    options = list(disposal_method = "unmanaged_disposal")
  ))
  expect_equal(sum(res_u$applied_n), 130, tolerance = 1e-8)
  expect_true("Disposal" %in% res_u$land_use)
  res_r <- suppressWarnings(whep::allocate_manure_to_land(
    .toy_applied(),
    list(crops = tiny),
    options = list(disposal_method = "retain_unallocated")
  ))
  expect_equal(sum(res_r$applied_n), 130, tolerance = 1e-8)
  expect_true("Unallocated" %in% res_r$land_use)
})

test_that("fixed_ceiling cap uses crop area and the EU Nitrates rate", {
  # 170 kg N/ha * area: barley 1000 ha -> 170 t, wheat 800 ha -> 136 t; both
  # well above collected, so the whole collected pool lands on cropland.
  res <- whep::allocate_manure_to_land(
    .toy_applied(),
    .toy_gridded(),
    options = list(cap_method = "fixed_ceiling")
  )
  crop <- res[res$land_use == "Cropland" & res$source_stream == "collected", ]
  expect_equal(sum(crop$applied_n), 110, tolerance = 1e-6)
  expect_false(any(res$over_cap))
  expect_true(all(res$method_cap == "fixed_ceiling"))
})

test_that("crop_n_demand allocation uses the demand weight", {
  crops <- .toy_crops()
  crops$crop_n_demand <- c(1, 9) # reverse the receptivity ordering
  crops$crop_n_cap <- c(200, 200) # loose caps: pure proportional split
  res <- whep::allocate_manure_to_land(
    .toy_applied(),
    list(crops = crops, grass = .toy_grass()),
    options = list(method = "crop_n_demand")
  )
  crop <- res[res$land_use == "Cropland", ]
  expect_equal(crop$applied_n[crop$crop == "barley"], 11, tolerance = 1e-6)
  expect_equal(crop$applied_n[crop$crop == "wheat"], 99, tolerance = 1e-6)
  expect_true(all(res$method_allocation == "crop_n_demand"))
})

test_that("allocate_manure_to_land guards bad options and missing layers", {
  expect_error(
    whep::allocate_manure_to_land(
      .toy_applied(),
      .toy_gridded(),
      options = list(method = "x")
    ),
    "method"
  )
  expect_error(
    whep::allocate_manure_to_land(
      .toy_applied(),
      .toy_gridded(),
      options = list(cap_method = "x")
    ),
    "cap_method"
  )
  expect_error(
    whep::allocate_manure_to_land(
      .toy_applied(),
      .toy_gridded(),
      options = list(disposal_method = "x")
    ),
    "disposal"
  )
  # potential_uptake needs a precomputed crop_n_cap; absence aborts.
  bare <- dplyr::select(.toy_crops(), -"crop_n_cap")
  expect_error(
    whep::allocate_manure_to_land(.toy_applied(), list(crops = bare)),
    "crop_n_cap"
  )
  # missing the crops layer entirely aborts.
  expect_error(whep::allocate_manure_to_land(.toy_applied()), "crops")
})
