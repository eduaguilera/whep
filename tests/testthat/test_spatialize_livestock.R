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
  ~lon, ~lat, ~area_code, ~cell_area_frac,
  0.25, 50.25, 1L, 1,
  0.75, 50.25, 1L, 1,
  1.25, 50.25, 2L, 1
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

test_that("grass-weighted heads fall back to area weight for cells with NA NPP", {
  # Cells missing from grass_productivity (NA after left_join) must keep their
  # area-based weight, not be zeroed out and silently dropped.
  grass_npp_partial <- tibble::tribble(
    ~lon, ~lat, ~grass_npp,
    0.25, 50.25, 300
    # 0.75 and 1.25 intentionally absent
  )

  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid,
    grass_productivity = grass_npp_partial
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

test_that("an unmapped species_group aborts naming the group", {
  # Was "missing species_group falls back to pasture" (whep#1000, T15a-i):
  # the silent `"pasture"` fallback gave any unmapped group -- a typo, a new
  # FAOSTAT item, a poultry class -- a grazing distribution, with nothing in
  # the output to say so.
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2000L, 1L, "unknown_species", 1000
  )

  expect_error(
    build_gridded_livestock(
      ld,
      gridded_pasture,
      gridded_cropland,
      country_grid
    ),
    "unknown_species"
  )
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

# S-A6 -- the pig case ----------------------------------------------------------
#
# The existing shared-cell test above uses 100 head against 0, which any
# implementation passes once the neighbour is empty. This one is deliberately
# non-degenerate: two polycells per shared cell, unequal shares (10/90),
# national herds differing 100-fold and both non-zero, and each polity also
# holding a cell alone so its share denominator is testable.
.sa6_livestock_fixture <- function(heads_1 = 1000, heads_2 = 10) {
  list(
    livestock_data = tibble::tribble(
      ~year, ~area_code, ~species_group, ~heads, ~manure_n_mg,
      2000L,         1L,         "pigs", heads_1,   heads_1 / 10,
      2000L,         2L,         "pigs", heads_2,   heads_2 / 10
    ),
    gridded_pasture = tibble::tribble(
      ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
      0.25, 50.25, 2000L,            0,             0,
      0.75, 50.25, 2000L,            0,             0,
      1.25, 50.25, 2000L,            0,             0
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,         1000,
      0.75, 50.25, 2000L,         1000,
      1.25, 50.25, 2000L,         1000
    ),
    country_grid = tibble::tribble(
      ~polycell_id,  ~lon,  ~lat, ~area_code, ~cell_area_frac,
      "shared-1",   0.25, 50.25,         1L,             0.1,
      "shared-2",   0.25, 50.25,         2L,             0.9,
      "own-1",      0.75, 50.25,         1L,             1.0,
      "own-2",      1.25, 50.25,         2L,             1.0
    )
  )
}

test_that("a shared cell delivers only the herd its own polycell carries", {
  result <- do.call(whep::build_gridded_livestock, .sa6_livestock_fixture())

  expect_setequal(
    result$polycell_id,
    c("shared-1", "shared-2", "own-1", "own-2")
  )
  totals <- result |>
    dplyr::summarise(heads = sum(heads), .by = area_code) |>
    dplyr::arrange(area_code)
  expect_equal(totals$heads, c(1000, 10), tolerance = 1e-9)

  # Weights are per polity: polity 1 holds 1000 * 0.1 in the shared cell
  # against 1000 in its own; polity 2 holds 1000 * 0.9 against 1000.
  by_pc <- stats::setNames(result$heads, result$polycell_id)
  expect_equal(unname(by_pc[["shared-1"]]), 1000 * 100 / 1100, tolerance = 1e-9)
  expect_equal(unname(by_pc[["shared-2"]]), 10 * 900 / 1900, tolerance = 1e-9)

  # The discriminator: a cell-then-split scheme puts the two compartments in
  # their AREA ratio, 0.1 / 0.9. Keyed on the polycell the ratio is ~19.
  ratio <- by_pc[["shared-1"]] / by_pc[["shared-2"]]
  expect_gt(ratio, 19)
  expect_false(isTRUE(all.equal(ratio, 0.1 / 0.9)))

  # Every distributed column follows the same weights, not just `heads`.
  expect_equal(
    result$manure_n_mg,
    result$heads / 10,
    tolerance = 1e-9
  )
})

test_that("a neighbour's national herd cannot move a polycell", {
  small <- do.call(whep::build_gridded_livestock, .sa6_livestock_fixture())
  large <- do.call(
    whep::build_gridded_livestock,
    .sa6_livestock_fixture(5e7, 10)
  )
  pick <- function(x) {
    x |>
      dplyr::filter(area_code == 2L) |>
      dplyr::arrange(polycell_id) |>
      dplyr::select(polycell_id, heads, manure_n_mg)
  }
  expect_identical(pick(small), pick(large))
  expect_gt(
    sum(dplyr::filter(large, area_code == 1L)$heads),
    sum(dplyr::filter(small, area_code == 1L)$heads)
  )
})

test_that(".build_proxy_grid carries only the compartment key and weight", {
  fix <- .sa6_livestock_fixture()
  grid <- whep:::.normalize_country_grid(fix$country_grid)
  out <- whep:::.build_proxy_grid(
    "cropland",
    fix$gridded_pasture,
    fix$gridded_cropland,
    grid,
    NULL,
    NULL
  )
  expect_setequal(
    names(out),
    c("polycell_id", "area_code", "lon", "lat", "weight")
  )

  # A `country_grid` column colliding with a distributed value column would be
  # suffixed by the join in `.allocate_livestock_to_grid()` and then never
  # distributed at all. Carrying only the key makes that unreachable.
  colliding <- fix
  colliding$country_grid$heads <- 12345
  expect_equal(
    sum(do.call(whep::build_gridded_livestock, colliding)$heads),
    1010,
    tolerance = 1e-9
  )
})

test_that("country with totals but no proxy cell warns and keeps others", {
  # area_code 3 has national heads but no cell in country_grid, so it has
  # no allocatable proxy weight. It must not vanish silently: warn, and
  # still allocate the country that does have cells (area_code 1).
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2000L,         1L,       "cattle",  10000,
    2000L,         3L,       "cattle",   7000
  )

  warnings <- testthat::capture_warnings(
    result <- build_gridded_livestock(
      ld,
      gridded_pasture,
      gridded_cropland,
      country_grid
    )
  )
  expect_match(warnings, "no proxy grid cell", all = FALSE)
  # The grid has no cell for area 3 at all, so the per-call guard fires too.
  expect_match(warnings, "no cell in", all = FALSE)

  # Country 1 is fully allocated; country 3 (no cell) is not silently mixed in
  expect_equal(sum(result$heads), 10000, tolerance = 1e-6)
})

test_that("no-proxy-cell warning survives several area codes", {
  # Two cell-less countries, not one. `area_code` is integer and cli's
  # make_quantity() aborts on a numeric quantity of length > 1, so a plural
  # marker reading the code vector made this warning a hard error whenever
  # more than one country was unallocatable.
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2000L,         1L,       "cattle",  10000,
    2000L,         3L,       "cattle",   7000,
    2000L,         4L,       "cattle",   2000
  )

  warnings <- testthat::capture_warnings(
    result <- build_gridded_livestock(
      ld,
      gridded_pasture,
      gridded_cropland,
      country_grid
    )
  )
  expect_match(
    warnings,
    "no proxy grid cell[\\s\\S]*2 area_codes",
    all = FALSE,
    perl = TRUE
  )

  expect_equal(sum(result$heads), 10000, tolerance = 1e-6)
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

# -- area_key ----------------------------------------------------------

# 276 Sudan and 277 South Sudan are both reporting areas of bucket 206; 68 is
# its own bucket and must come through the re-key untouched.
off_bucket_livestock <- function() {
  list(
    livestock_data = tibble::tribble(
      ~year, ~area_code, ~species_group, ~heads, ~manure_n_mg,
      2000L,       276L,       "cattle",  10000,         50.0,
      2000L,       277L,       "cattle",   4000,         20.0,
      2000L,        68L,       "cattle",   2000,         10.0
    ),
    country_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,       276L,             1.0,
      0.75, 50.25,       276L,             0.6,
      0.75, 50.25,       277L,             0.4,
      1.25, 50.25,        68L,             1.0
    )
  )
}

test_that("build_gridded_livestock warns on off-bucket area codes", {
  fix <- off_bucket_livestock()

  expect_warning(
    result <- whep::build_gridded_livestock(
      fix$livestock_data,
      gridded_pasture,
      gridded_cropland,
      fix$country_grid,
      years = 2000L
    ),
    "cannot join"
  )
  expect_setequal(result$area_code, c(276L, 277L, 68L))
  expect_gt(sum(result$area_code != result$polity_area_code), 0L)
  expect_false(rlang::has_name(result, "grid_area_code"))
})

test_that("livestock area_key = polity_area leaves no disagreeing key", {
  fix <- off_bucket_livestock()

  result <- whep::build_gridded_livestock(
    fix$livestock_data,
    gridded_pasture,
    gridded_cropland,
    fix$country_grid,
    years = 2000L,
    area_key = "polity_area"
  )

  expect_equal(sum(result$area_code != result$polity_area_code), 0L)
  expect_equal(whep:::.cell_polity_off_bucket(result), integer(0))
  labels <- dplyr::distinct(
    result,
    area_code,
    reporting_polity_code,
    reporting_polity_name
  )
  expect_equal(nrow(labels), dplyr::n_distinct(result$area_code))
})

test_that("livestock area_key = polity_area carries the raw code and mass", {
  fix <- off_bucket_livestock()

  keyed <- whep::build_gridded_livestock(
    fix$livestock_data,
    gridded_pasture,
    gridded_cropland,
    fix$country_grid,
    years = 2000L,
    area_key = "polity_area"
  )
  raw <- suppressWarnings(whep::build_gridded_livestock(
    fix$livestock_data,
    gridded_pasture,
    gridded_cropland,
    fix$country_grid,
    years = 2000L
  ))

  expect_equal(sum(keyed$heads), sum(raw$heads), tolerance = 1e-9)
  expect_equal(sum(keyed$manure_n_mg), sum(raw$manure_n_mg), tolerance = 1e-9)
  shared <- dplyr::filter(keyed, lon == 0.75)
  expect_equal(nrow(shared), 1L)
  expect_equal(shared$grid_area_code, "276+277")
  expect_equal(
    shared$heads,
    sum(dplyr::filter(raw, lon == 0.75)$heads),
    tolerance = 1e-9
  )
  expect_equal(dplyr::filter(keyed, area_code == 68L)$grid_area_code, "68")
})

test_that("build_gridded_livestock rejects an unknown area_key", {
  fix <- off_bucket_livestock()

  expect_error(
    whep::build_gridded_livestock(
      fix$livestock_data,
      gridded_pasture,
      gridded_cropland,
      fix$country_grid,
      years = 2000L,
      area_key = "polity"
    ),
    class = "rlang_error"
  )
})

# -- Reporting areas the grid cannot represent at all (whep#461) ---------
#
# `.warn_unallocated_livestock()` fires per species per year, so a country the
# grid has no cell for at all is one more entry in a list that is already
# long. Substituting a crosswalk keyed on a different reporting-code vintage
# deletes such a country outright; report that once, on its own, with the
# heads at stake.
test_that("a re-keyed grid names the countries it deletes, with heads", {
  # `cell_area_frac` is explicit because C8/S-A5 forbids the share-less grid
  # this fixture arrived with: `.abort_missing_polity_share()` refuses a
  # crosswalk carrying no share rather than defaulting it to 1. `1` is the
  # honest value here -- each cell is owned outright by the polity keyed on it
  # -- so the deleted-country behaviour under test is unchanged.
  retired_grid <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25,         1L,               1,
    0.75, 50.25,         1L,               1,
    1.25, 50.25,        62L,               1
  )

  warnings <- testthat::capture_warnings(
    result <- whep::build_gridded_livestock(
      livestock_data,
      gridded_pasture,
      gridded_cropland,
      retired_grid,
      years = 2000L
    )
  )

  expect_match(warnings, "no cell in .*country_grid.* at all", all = FALSE)
  expect_match(warnings, "11000 head", all = FALSE)
  expect_false(2L %in% result$area_code)
})

test_that("a grid holding every country raises no missing-reporter warning", {
  fn <- whep:::.warn_grid_missing_reporters

  expect_no_warning(fn(livestock_data, country_grid, "heads", "head"))
})

# -- proxy_method (whep#1000, T15a-i) -----------------------------------
#
# `glw_density` used to be tried first and silently abandoned per species
# per year whenever it produced no cell, so one output could mix GLW3 and
# LUH2 evidence with nothing recording which group ran on what. The proxy
# is now selected, refused when its inputs are missing, and stamped on
# every row.

test_that("the default proxy_method reproduces the LUH2 allocation", {
  # Captured from the engine as it stood before `proxy_method` existed
  # (commit 44721cd4, printed at 17 significant digits). The claim under
  # test is that the default path is the same arithmetic, not merely a
  # conserving one: every one of these nine values is a share of a national
  # total that ships in `gridded_livestock.parquet`.
  expected <- tibble::tibble(
    year = c(rep(2000L, 6L), rep(2001L, 3L)),
    species_group = c(
      "cattle",
      "cattle",
      "cattle",
      "pigs",
      "pigs",
      "sheep_goats",
      "cattle",
      "cattle",
      "cattle"
    ),
    area_code = c(1L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L),
    lon = c(0.25, 0.75, 1.25, 0.25, 0.75, 1.25, 0.25, 0.75, 1.25),
    lat = 50.25,
    heads = c(
      6153.8461538461543,
      3846.1538461538462,
      8000,
      3076.9230769230771,
      1923.0769230769231,
      3000,
      6461.5384615384619,
      4038.4615384615386,
      8200
    ),
    enteric_ch4_kt = c(
      0.61538461538461542,
      0.38461538461538464,
      0.80000000000000004,
      0,
      0,
      0.10000000000000001,
      0.67692307692307696,
      0.42307692307692313,
      0.81999999999999995
    ),
    manure_n_mg = c(
      30.769230769230770,
      19.230769230769234,
      40,
      12.307692307692308,
      7.6923076923076925,
      5,
      32.307692307692307,
      20.192307692307693,
      41
    )
  )

  result <- build_gridded_livestock(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )

  expect_equal(
    dplyr::select(result, dplyr::all_of(names(expected))),
    expected,
    tolerance = 1e-12
  )
  # The only schema change is one appended column: a consumer selecting by
  # position or binding to an older parquet keeps working.
  expect_identical(
    names(result),
    c(
      "year",
      "area_code",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "species_group",
      "lon",
      "lat",
      "heads",
      "enteric_ch4_kt",
      "manure_n_mg",
      "method_livestock_proxy"
    )
  )
  expect_setequal(result$method_livestock_proxy, "luh2_area")
})

test_that("grass weighting is recorded per group, not per call", {
  # `.build_proxy_grid()` multiplies by grass NPP for the pasture and
  # rangeland proxies only, so pigs on the cropland proxy stay on area
  # weights in the very same call and must say so.
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

  methods <- result |>
    dplyr::distinct(species_group, method_livestock_proxy) |>
    dplyr::arrange(species_group)
  expect_identical(
    methods$species_group,
    c("cattle", "pigs", "sheep_goats")
  )
  expect_identical(
    methods$method_livestock_proxy,
    c("luh2_grass", "luh2_area", "luh2_grass")
  )
})

test_that("method_livestock_proxy survives the polity_area re-key", {
  # The column is a key, not a value, so it must not stop two reporting
  # areas of one bucket folding into a single cell row.
  fix <- off_bucket_livestock()

  keyed <- whep::build_gridded_livestock(
    fix$livestock_data,
    gridded_pasture,
    gridded_cropland,
    fix$country_grid,
    years = 2000L,
    area_key = "polity_area"
  )

  expect_setequal(keyed$method_livestock_proxy, "luh2_area")
  expect_equal(nrow(keyed), 3L)
  expect_equal(sum(keyed$heads), 16000, tolerance = 1e-9)
})

# GLW3 density and the LUH2 extent disagree on purpose here: cell 1.25 holds
# almost all the density and no land use at all, so an implementation that
# forgets the extent mask puts 96% of the herd in a cell LUH2 says is empty,
# and one that quietly reverts to the LUH2 proxy reproduces the 800/1300
# pasture split instead of the 1/3 density split.
.glw_livestock_fixture <- function() {
  list(
    livestock_data = tibble::tribble(
      ~year, ~area_code, ~species_group, ~heads,
      2000L,         1L,       "cattle",  10000
    ),
    gridded_pasture = tibble::tribble(
      ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
      0.25, 50.25, 2000L,         600,           200,
      0.75, 50.25, 2000L,         400,           100,
      1.25, 50.25, 2000L,           0,             0
    ),
    gridded_cropland = tibble::tribble(
      ~lon,  ~lat,  ~year, ~cropland_ha,
      0.25, 50.25, 2000L,          800,
      0.75, 50.25, 2000L,          500,
      1.25, 50.25, 2000L,            0
    ),
    country_grid = tibble::tribble(
      ~lon,  ~lat, ~area_code, ~cell_area_frac,
      0.25, 50.25,         1L,               1,
      0.75, 50.25,         1L,               1,
      1.25, 50.25,         1L,               1
    ),
    glw_density = tibble::tribble(
      ~lon,  ~lat, ~species_group, ~density,
      0.25, 50.25,       "cattle",        1,
      0.75, 50.25,       "cattle",        3,
      1.25, 50.25,       "cattle",       96
    )
  )
}

test_that("proxy_method glw3 allocates by density, masked by LUH2 extent", {
  fix <- .glw_livestock_fixture()

  result <- whep::build_gridded_livestock(
    fix$livestock_data,
    fix$gridded_pasture,
    fix$gridded_cropland,
    fix$country_grid,
    glw_density = fix$glw_density,
    proxy_method = "glw3"
  )

  by_lon <- stats::setNames(result$heads, result$lon)
  expect_setequal(names(by_lon), c("0.25", "0.75"))
  expect_equal(unname(by_lon[["0.25"]]), 2500, tolerance = 1e-9)
  expect_equal(unname(by_lon[["0.75"]]), 7500, tolerance = 1e-9)
  expect_equal(sum(result$heads), 10000, tolerance = 1e-9)
  expect_setequal(result$method_livestock_proxy, "glw3")

  # The discriminator against a silent revert to the LUH2 pasture proxy.
  expect_false(isTRUE(all.equal(
    unname(by_lon[["0.25"]]),
    10000 * 800 / 1300
  )))
})

test_that("proxy_method glw3 aborts when glw_density is NULL", {
  fix <- .glw_livestock_fixture()

  expect_error(
    whep::build_gridded_livestock(
      fix$livestock_data,
      fix$gridded_pasture,
      fix$gridded_cropland,
      fix$country_grid,
      proxy_method = "glw3"
    ),
    "glw_density"
  )
})

test_that("proxy_method glw3 aborts for a group the density table misses", {
  fix <- .glw_livestock_fixture()
  fix$livestock_data <- dplyr::bind_rows(
    fix$livestock_data,
    tibble::tibble(
      year = 2000L,
      area_code = 1L,
      species_group = "pigs",
      heads = 500
    )
  )

  expect_error(
    whep::build_gridded_livestock(
      fix$livestock_data,
      fix$gridded_pasture,
      fix$gridded_cropland,
      fix$country_grid,
      glw_density = fix$glw_density,
      proxy_method = "glw3"
    ),
    "pigs"
  )
})

test_that("proxy_method glw3 aborts on a group with no positive density", {
  # Present in the table but zero everywhere: the same silent revert as an
  # absent group, so it is refused the same way.
  fix <- .glw_livestock_fixture()
  fix$glw_density$density <- c(0, 0, NA)

  expect_error(
    whep::build_gridded_livestock(
      fix$livestock_data,
      fix$gridded_pasture,
      fix$gridded_cropland,
      fix$country_grid,
      glw_density = fix$glw_density,
      proxy_method = "glw3"
    ),
    "cattle"
  )
})

test_that("glw_density under the luh2 method is warned about, not used", {
  fix <- .glw_livestock_fixture()

  expect_warning(
    result <- whep::build_gridded_livestock(
      fix$livestock_data,
      fix$gridded_pasture,
      fix$gridded_cropland,
      fix$country_grid,
      glw_density = fix$glw_density
    ),
    "ignored"
  )

  by_lon <- stats::setNames(result$heads, result$lon)
  expect_equal(
    unname(by_lon[["0.25"]]),
    10000 * 800 / 1300,
    tolerance = 1e-9
  )
  expect_setequal(result$method_livestock_proxy, "luh2_area")
})

test_that("build_gridded_livestock rejects an unknown proxy_method", {
  expect_error(
    build_gridded_livestock(
      livestock_data,
      gridded_pasture,
      gridded_cropland,
      country_grid,
      proxy_method = "glw4"
    ),
    class = "rlang_error"
  )
})

test_that("a group whose proxy has no weighted cell is reported", {
  # Camels take the rangeland proxy; with no rangeland the group's national
  # total is dropped for the year before `.warn_unallocated_livestock()`
  # can see it, so the drop is reported here or nowhere.
  ld <- tibble::tribble(
    ~year, ~area_code, ~species_group, ~heads,
    2000L,         1L,       "camels",    500,
    2000L,         1L,       "cattle",   1000
  )
  pasture <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
    0.25, 50.25, 2000L,         600,             0
  )
  cropland <- tibble::tribble(
    ~lon,  ~lat,  ~year, ~cropland_ha,
    0.25, 50.25, 2000L,          800
  )
  cg <- tibble::tribble(
    ~lon,  ~lat, ~area_code, ~cell_area_frac,
    0.25, 50.25,         1L,               1
  )

  expect_warning(
    result <- build_gridded_livestock(ld, pasture, cropland, cg),
    "camels"
  )
  expect_setequal(result$species_group, "cattle")
  expect_equal(sum(result$heads), 1000, tolerance = 1e-9)
})

test_that(".livestock_proxy_types keeps the first proxy of a repeated group", {
  # `.read_livestock_mapping()` hands "other" over twice: cropland from
  # items 1140/1150 and mixed from 1171 in
  # inst/extdata/livestock_mapping.csv. Taking the first is what the
  # replaced `proxy_row$spatial_proxy[1]` lookup did.
  species_proxy <- tibble::tribble(
    ~species_group, ~spatial_proxy,
    "other",        "cropland",
    "other",        "mixed"
  )
  expect_identical(
    whep:::.livestock_proxy_types(species_proxy, "other"),
    c(other = "cropland")
  )
})

test_that(".livestock_proxy_types refuses an unknown proxy class", {
  # `.build_glw_proxy_grid()`'s switch has no default, so an unchecked class
  # returns NULL land use and fails somewhere unrelated.
  species_proxy <- tibble::tribble(
    ~species_group, ~spatial_proxy,
    "cattle",       "savanna"
  )
  expect_error(
    whep:::.livestock_proxy_types(species_proxy, "cattle"),
    "savanna"
  )
})

test_that(".livestock_proxy_types treats an NA proxy as unmapped", {
  species_proxy <- tibble::tibble(
    species_group = "cattle",
    spatial_proxy = NA_character_
  )
  expect_error(
    whep:::.livestock_proxy_types(species_proxy, "cattle"),
    "cattle"
  )
})

test_that(".livestock_proxy_method labels the regime, not the cell", {
  fn <- whep:::.livestock_proxy_method
  grass <- tibble::tibble(lon = 0.25, lat = 50.25, grass_npp = 1)

  expect_identical(fn("glw3", "pasture", NULL), "glw3")
  expect_identical(fn("glw3", "cropland", grass), "glw3")
  expect_identical(fn("luh2", "pasture", NULL), "luh2_area")
  expect_identical(fn("luh2", "pasture", grass), "luh2_grass")
  expect_identical(fn("luh2", "rangeland", grass), "luh2_grass")
  # Grass NPP never reaches the cropland or mixed weights.
  expect_identical(fn("luh2", "cropland", grass), "luh2_area")
  expect_identical(fn("luh2", "mixed", grass), "luh2_area")
})
