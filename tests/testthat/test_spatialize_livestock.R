# -- Fixtures ----------------------------------------------------------

livestock_data <- tibble::tribble(
  ~year, ~area_code, ~species_group, ~heads, ~enteric_ch4_kt,
    ~manure_n_mg,
  2000L, 1L, "cattle", 10000, 1.0, 50.0,
  2000L, 1L, "pigs", 5000, 0.0, 20.0,
  2000L, 2L, "cattle", 8000, 0.8, 40.0,
  2000L, 2L, "sheep_goats", 3000, 0.1, 5.0,
  2001L, 1L, "cattle", 10500, 1.1, 52.5,
  2001L, 2L, "cattle", 8200, 0.82, 41.0
)

gridded_pasture <- tibble::tribble(
  ~lon, ~lat, ~year, ~pasture_ha, ~rangeland_ha,
  0.25, 50.25, 2000L, 600, 200,
  0.75, 50.25, 2000L, 400, 100,
  1.25, 50.25, 2000L, 300, 300,
  0.25, 50.25, 2001L, 590, 210,
  0.75, 50.25, 2001L, 410, 90,
  1.25, 50.25, 2001L, 310, 290
)

gridded_cropland <- tibble::tribble(
  ~lon, ~lat, ~year, ~cropland_ha,
  0.25, 50.25, 2000L, 800,
  0.75, 50.25, 2000L, 500,
  1.25, 50.25, 2000L, 200,
  0.25, 50.25, 2001L, 810,
  0.75, 50.25, 2001L, 490,
  1.25, 50.25, 2001L, 210
)

country_grid <- tibble::tribble(
  ~lon, ~lat, ~area_code,
  0.25, 50.25, 1L,
  0.75, 50.25, 1L,
  1.25, 50.25, 2L
)


# -- Tests -------------------------------------------------------------

test_that("years argument filters livestock inputs", {
  result <- whep::build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid,
    years = 2001L
  )
  expect_setequal(unique(result$year), 2001L)
})

test_that("years warns for missing years in livestock_data", {
  expect_warning(
    whep::build_gridded_livestock(
      livestock_data,
      gridded_pasture,
      gridded_cropland,
      country_grid,
      years = c(2001L, 1999L)
    ),
    "1999"
  )
})

test_that("build_gridded_livestock returns expected columns", {
  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  expect_true(all(
    c(
      "year",
      "species_group",
      "lon",
      "lat",
      "heads",
      "enteric_ch4_kt",
      "manure_n_mg"
    ) %in%
      names(result)
  ))
})

test_that("heads conservation is exact", {
  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  # Per year, total gridded heads == total input heads
  for (yr in unique(livestock_data$year)) {
    input_heads <- sum(
      livestock_data$heads[livestock_data$year == yr]
    )
    grid_heads <- sum(
      result$heads[result$year == yr]
    )
    expect_equal(grid_heads, input_heads, tolerance = 1e-6)
  }
})

test_that("enteric_ch4_kt conservation is exact", {
  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  for (yr in unique(livestock_data$year)) {
    input_ch4 <- sum(
      livestock_data$enteric_ch4_kt[livestock_data$year == yr]
    )
    grid_ch4 <- sum(
      result$enteric_ch4_kt[result$year == yr]
    )
    expect_equal(grid_ch4, input_ch4, tolerance = 1e-6)
  }
})

test_that("grass-weighted heads conserve when NPP covers all countries", {
  # Every country's grazer cells carry a grass_npp value, so the
  # grass-productivity weighting must not drop any country: national totals
  # stay conserved. Guards the silent per-country loss that occurs when
  # grass_npp is missing for a country's grazer cells.
  grass_npp <- tibble::tribble(
    ~lon, ~lat, ~grass_npp,
    0.25, 50.25, 300,
    0.75, 50.25, 500,
    1.25, 50.25, 200
  )

  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid,
    grass_productivity = grass_npp
  )

  for (yr in unique(livestock_data$year)) {
    input_heads <- sum(livestock_data$heads[livestock_data$year == yr])
    grid_heads <- sum(result$heads[result$year == yr])
    expect_equal(grid_heads, input_heads, tolerance = 1e-6)
  }
})

test_that("cattle uses pasture proxy (not cropland)", {
  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  cattle_2000 <- result[
    result$year == 2000L &
      result$species_group == "cattle" &
      result$lon %in% c(0.25, 0.75),
  ]
  # Country 1: two cells with pasture 800 and 500 ha

  # Share should be 800/1300 = 0.615 and 500/1300 = 0.385
  expect_equal(nrow(cattle_2000), 2L)
  shares <- cattle_2000$heads / sum(cattle_2000$heads)
  expect_equal(shares[1], 800 / 1300, tolerance = 1e-4)
  expect_equal(shares[2], 500 / 1300, tolerance = 1e-4)
})

test_that("pigs uses cropland proxy", {
  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  pigs <- result[
    result$year == 2000L & result$species_group == "pigs",
  ]
  # Country 1 cells: cropland 800 and 500
  expect_equal(nrow(pigs), 2L)
  shares <- pigs$heads / sum(pigs$heads)
  expect_equal(shares[1], 800 / 1300, tolerance = 1e-4)
})

test_that("sheep_goats uses pasture proxy for country 2", {
  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  sg <- result[
    result$year == 2000L & result$species_group == "sheep_goats",
  ]
  # Country 2 has only cell (1.25, 50.25) with pasture_ha + rangeland_ha
  expect_equal(nrow(sg), 1L)
  expect_equal(sg$heads, 3000)
})

test_that("multi-year output has correct year structure", {
  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  expect_true(all(c(2000L, 2001L) %in% result$year))
})

test_that("manure_pattern modulates weights", {
  manure_pat <- tibble::tribble(
    ~lon, ~lat, ~manure_intensity,
    0.25, 50.25, 1.0,
    0.75, 50.25, 0.1,
    1.25, 50.25, 0.5
  )

  result_plain <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  result_manure <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid,
    manure_pattern = manure_pat
  )

  # With manure pattern, cell 0.25 should get relatively more
  # (high manure intensity) vs cell 0.75 (low intensity)
  plain_c1 <- result_plain[
    result_plain$year == 2000L &
      result_plain$species_group == "cattle" &
      result_plain$lon == 0.25,
  ]$heads
  manure_c1 <- result_manure[
    result_manure$year == 2000L &
      result_manure$species_group == "cattle" &
      result_manure$lon == 0.25,
  ]$heads

  # Cell 0.25 has manure_intensity=1.0 so should get more share
  expect_true(manure_c1 > plain_c1)
})

test_that("custom species_proxy is respected", {
  # Override: make cattle use cropland instead of pasture
  custom_proxy <- tibble::tribble(
    ~species_group, ~spatial_proxy,
    "cattle", "cropland",
    "pigs", "cropland",
    "sheep_goats", "pasture",
    "other", "mixed"
  )

  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid,
    species_proxy = custom_proxy
  )

  cattle_2000 <- result[
    result$year == 2000L &
      result$species_group == "cattle" &
      result$lon %in% c(0.25, 0.75),
  ]
  # Now cattle should use cropland shares: 800/1300, 500/1300
  shares <- cattle_2000$heads / sum(cattle_2000$heads)
  expect_equal(shares[1], 800 / 1300, tolerance = 1e-4)
})

test_that("missing species_group falls back to pasture", {
  # Data with a group not in default proxy mapping
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2000L, 1L, "unknown_species", 1000
  )

  result <- build_gridded_livestock(
    ld,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  # Should still produce output (falls back to pasture proxy)
  expect_gt(nrow(result), 0L)
  expect_equal(sum(result$heads), 1000)
})

test_that("validation rejects missing columns", {
  bad_data <- tibble::tibble(year = 2000L, area_code = 1L)

  expect_error(
    build_gridded_livestock(
      bad_data,
      gridded_pasture,
      gridded_cropland,
      country_grid
    ),
    "species_group"
  )
})

test_that("all numeric columns are distributed", {
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads, ~extra_val,
    2000L, 1L, "cattle", 4000, 100.0
  )
  result <- build_gridded_livestock(
    ld,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  expect_true("extra_val" %in% names(result))
  expect_equal(sum(result$extra_val), 100.0, tolerance = 1e-6)
})

test_that("zero-heads country is handled gracefully", {
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2000L, 1L, "cattle", 0
  )
  result <- build_gridded_livestock(
    ld,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  # Zero heads should distribute to zero everywhere
  expect_equal(sum(result$heads), 0)
})

test_that("shared cells keep independent livestock polity compartments", {
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads, ~manure_n_mg,
    2000L,         1L,        "pigs",    100,         10,
    2000L,         2L,        "pigs",      0,          0
  )
  pasture <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
    0.25, 50.25, 2000L,           0,             0
  )
  cropland <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~cropland_ha,
    0.25, 50.25, 2000L,         1000
  )
  cg <- tibble::tribble(
    ~polycell_id, ~lon,  ~lat, ~area_code, ~cell_area_frac,
    "a",         0.25, 50.25,         1L,             0.4,
    "b",         0.25, 50.25,         2L,             0.6
  )

  result <- whep::build_gridded_livestock(
    ld,
    pasture,
    cropland,
    cg
  )

  totals <- result |>
    dplyr::summarise(
      heads = sum(heads),
      manure_n_mg = sum(manure_n_mg),
      .by = area_code
    ) |>
    dplyr::arrange(area_code)

  expect_equal(totals$heads, c(100, 0), tolerance = 1e-6)
  expect_equal(totals$manure_n_mg, c(10, 0), tolerance = 1e-6)
  expect_setequal(result$polycell_id, c("a", "b"))
})

test_that(".build_proxy_grid weights grazer cells by grass productivity", {
  pasture <- tibble::tibble(
    lon = c(0.25, 1.25),
    lat = 0.25,
    year = 2000L,
    pasture_ha = 100,
    rangeland_ha = 0
  )
  cg <- tibble::tibble(
    lon = c(0.25, 1.25),
    lat = 0.25,
    area_code = 1L,
    cell_area_frac = 1
  )
  gp <- tibble::tibble(lon = c(0.25, 1.25), lat = 0.25, grass_npp = c(1, 3))
  base <- whep:::.build_proxy_grid("pasture", pasture, NULL, cg, NULL, NULL)
  prod <- whep:::.build_proxy_grid("pasture", pasture, NULL, cg, NULL, gp)
  expect_equal(base$weight[base$lon == 0.25], base$weight[base$lon == 1.25])
  expect_equal(
    prod$weight[prod$lon == 1.25] / prod$weight[prod$lon == 0.25],
    3,
    tolerance = 1e-9
  )
})

test_that(".build_proxy_grid leaves cropland proxy unaffected by grass productivity", {
  cropland <- tibble::tibble(
    lon = c(0.25, 1.25),
    lat = 0.25,
    year = 2000L,
    cropland_ha = 100
  )
  cg <- tibble::tibble(
    lon = c(0.25, 1.25),
    lat = 0.25,
    area_code = 1L,
    cell_area_frac = 1
  )
  gp <- tibble::tibble(lon = c(0.25, 1.25), lat = 0.25, grass_npp = c(1, 3))
  out <- whep:::.build_proxy_grid("cropland", NULL, cropland, cg, NULL, gp)
  expect_equal(out$weight[out$lon == 0.25], out$weight[out$lon == 1.25])
})
