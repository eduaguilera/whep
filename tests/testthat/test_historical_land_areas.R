# Two cells, one bucket, one handover. Cell A is the old territory, cell B is
# annexed in year 3. Cropland grows 10% a year in A and is flat in B, so the
# only step in the series is the map changing, and the two boundary rules give
# visibly different answers.
.two_cell_polity_areas <- function() {
  tibble::tribble(
    ~year, ~area_code, ~polity_code,
    1L, 1L, "OLD",
    2L, 1L, "OLD",
    3L, 1L, "NEW",
    4L, 1L, "NEW"
  ) |>
    data.table::as.data.table()
}

.two_cell_cover <- function() {
  tibble::tribble(
    ~polity_code, ~lon, ~lat, ~frac,
    "OLD", 0.25, 0.25, 1,
    "NEW", 0.25, 0.25, 1,
    "NEW", 0.75, 0.25, 1
  ) |>
    data.table::as.data.table()
}

# The same two cells in the polycell grain: one row per cell x polity x
# validity interval. Two things are deliberate. `NEW` holds cell A over two
# successive intervals carrying the SAME geometry, which is what reducing the
# support to one weight per cell has to survive. And `land_area_ha` is NOT equal
# to `polity_area_ha` anywhere -- cell B is 60% inland water and `OLD`'s share
# of cell A is entirely water, the cap that whep#800 measured on 1,502 real
# polycells -- so a test of which column is the weight cannot pass vacuously.
.two_cell_support <- function() {
  tibble::tribble(
    ~polity_code, ~cell_id, ~lon, ~lat, ~start_year, ~end_year, ~polity_area_ha, ~land_area_ha, ~inland_water_ha, ~ice_area_ha,
    "OLD",        1L,       0.25, 0.25, 1L,          3L,        100,             0,             100,              0,
    "NEW",        1L,       0.25, 0.25, 3L,          5L,        100,             70,            30,               0,
    "NEW",        1L,       0.25, 0.25, 5L,          9L,        100,             70,            30,               0,
    "NEW",        2L,       0.75, 0.25, 3L,          9L,        100,             40,            60,               0
  )
}

.two_cell_areas <- function() {
  tidyr::expand_grid(
    year = 1:4,
    lon = c(0.25, 0.75),
    land_use = c("cropland", "grassland")
  ) |>
    dplyr::mutate(
      lat = 0.25,
      area_ha = dplyr::if_else(
        .data$lon == 0.25,
        1e6 * 1.1^(.data$year - 1L),
        5e5
      )
    ) |>
    data.table::as.data.table()
}

.two_cell_land <- function(boundary_step) {
  build_historical_land_areas(
    years = 1:4,
    boundary_step = boundary_step,
    data = list(
      polity_areas = .two_cell_polity_areas(),
      cover = .two_cell_cover(),
      cell_areas = .two_cell_areas()
    )
  )
}

test_that("each year is measured inside that year's own polygon", {
  plain <- .two_cell_land("level_step")
  # Years 1-2 see cell A only (1.0, 1.1 Mha); years 3-4 see both
  # (1.21 + 0.5, 1.331 + 0.5).
  expect_equal(plain$Cropland, c(1, 1.1, 1.71, 1.831), tolerance = 1e-9)
})

test_that("the boundary rule keeps growth inside one polygon", {
  # Without the rule, the year-3 ratio is 1.71 / 1.1 = 1.5545: a 55% jump that
  # is all map, and `fill_proxy_growth()` would compound it down the whole
  # back-cast. With it, year 2 is re-measured inside the INCOMING polygon
  # (1.1 + 0.5 = 1.6) and the ratio is 1.71 / 1.6 = 1.0687, which is the real
  # growth of the land plus the flat annexed cell.
  plain <- .two_cell_land("level_step")
  relinked <- .two_cell_land("relink")

  expect_equal(plain$Cropland[3] / plain$Cropland[2], 1.5545, tolerance = 1e-4)
  expect_equal(
    relinked$Cropland[3] / relinked$Cropland[2],
    1.06875,
    tolerance = 1e-4
  )
  # The last year is the anchor and is identical under both rules; every
  # earlier year is lifted by exactly the step the rule removed. Year 2
  # becomes the 1.6 it measures inside the incoming polygon, and year 1
  # follows it down the real 10% growth of cell A: 1.6 / 1.1.
  expect_equal(dplyr::last(relinked$Cropland), dplyr::last(plain$Cropland))
  expect_equal(
    relinked$Cropland,
    c(1.6 / 1.1, 1.6, 1.71, 1.831),
    tolerance = 1e-9
  )
})

test_that("a cell's land is shared between the polygons that cover it", {
  # Both polities cover cell A in year 3; each gets half of it, and the bucket
  # holding both sees the cell once.
  shared <- whep:::.land_in_polygons(
    .two_cell_areas()[year == 3L, .(lon, lat, land_use, area_ha)],
    data.table::data.table(
      area_code = c(1L, 2L),
      polity_code = c("OLD", "NEW")
    ),
    .two_cell_cover()
  )
  expect_equal(
    shared[area_code == 2L & land_use == "cropland"]$land_mha,
    1.21 / 2 + 0.5,
    tolerance = 1e-9
  )
  expect_equal(
    shared[area_code == 1L & land_use == "cropland"]$land_mha,
    1.21 / 2,
    tolerance = 1e-9
  )
})

test_that("an unmeasurable year cuts off only the years before it", {
  # Four polities reachable from a reporting area have no polygon, so a bucket
  # can lose one year in the middle of its chain. Accumulating the chain from
  # the front would make that one hole NA the bucket's whole 1850-1961 series;
  # accumulating from the end stops it at the break.
  measured <- data.table::data.table(
    area_code = 1L,
    land_use = "cropland",
    year = 1:4,
    land_now = c(1, NA, 3, 4),
    land_next = c(1, NA, 3, 4)
  )
  linked <- whep:::.chain_link_land(measured, "relink")
  expect_equal(linked$Cropland, c(NA, NA, 3, 4))
})

test_that("a residual polity standing in for several buckets is dropped", {
  resolved <- tibble::tribble(
    ~year, ~area_code, ~polity_code, ~mapping_status,
    1L, 1L, "OWN", "matched",
    1L, 2L, "ROW", "matched",
    1L, 3L, "ROW", "matched",
    1L, 4L, "LATER", "out_of_span"
  ) |>
    data.table::as.data.table()
  kept <- whep:::.keep_measurable_polities(resolved)
  expect_equal(kept$area_code, 1L)
})

test_that("a polity with no polycell is named, not silently dropped", {
  expect_warning(
    whep:::.warn_land_without_polycell(
      data.table::data.table(year = 1L, area_code = 7L, polity_code = "GONE"),
      data.table::data.table(polity_code = "OTHER", lon = 0, lat = 0, frac = 1)
    ),
    "GONE"
  )
})

test_that("the cell cover is read off the polycell support", {
  cover <- whep:::.polity_cell_cover(c("OLD", "NEW"), .two_cell_support()) |>
    tibble::as_tibble()
  expect_equal(names(cover), c("polity_code", "lon", "lat", "frac"))
  # Three (polity, cell) pairs out of four support rows: `NEW` holds cell A
  # over two intervals of identical geometry, and reducing them with anything
  # additive would give it 200 ha there -- doubling its weight against `OLD`
  # in the very cell the two of them share.
  expect_equal(nrow(cover), 3L)
  expect_equal(
    cover |>
      dplyr::filter(.data$polity_code == "NEW", .data$lon == 0.25) |>
      dplyr::pull(.data$frac),
    100
  )
  # The weight is the polity's whole TERRITORY in the cell, not its land: the
  # fixture's land is 0 / 70 / 40 against 100 everywhere, so weighting by
  # `land_area_ha` would read 110 here and would erase `OLD` from the cell it
  # shares with `NEW` -- which is the failure whep#800 measured on 1,502 real
  # polycells whose territory the inland-water cap consumes entirely.
  expect_equal(sum(cover$frac), 300)
  expect_true(all(cover$frac == 100))
})

test_that("a support carrying none of the requested polities aborts", {
  expect_error(
    whep:::.polity_cell_cover("ABSENT", .two_cell_support()),
    "polycell support"
  )
})

test_that("the series can be built straight from a polycell support", {
  # The reader path, with the support injected instead of read: no pin, no
  # network, and the same answer as the pre-reduced `cover` fixture gives.
  from_support <- whep::build_historical_land_areas(
    years = 1:4,
    boundary_step = "level_step",
    data = list(
      polity_areas = .two_cell_polity_areas(),
      support = .two_cell_support(),
      cell_areas = .two_cell_areas()
    )
  )
  expect_equal(from_support$Cropland, c(1, 1.1, 1.71, 1.831), tolerance = 1e-9)
})

test_that("the example fixture has the seam's shape", {
  fixture <- build_historical_land_areas(example = TRUE)
  expect_true(tibble::is_tibble(fixture))
  expect_true(all(
    c(
      "year",
      "area_code",
      "polity_code",
      "Cropland",
      "Pasture",
      "agriland"
    ) %in%
      names(fixture)
  ))
  expect_equal(fixture$agriland, fixture$Cropland + fixture$Pasture)
})

test_that("an unknown boundary rule aborts", {
  expect_error(
    build_historical_land_areas(years = 1:2, boundary_step = "nonsense"),
    class = "rlang_error"
  )
})
