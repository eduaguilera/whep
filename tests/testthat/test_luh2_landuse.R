# Raw-states fixture: two 0.5-degree cells, two years (1750, 2000), the 12
# LUH2 v2h state names. Per cell-year the 12 state fractions sum to 1 exactly,
# so the four carbon-balance classes must also sum to 1.
.luh2_raw_fixture <- function() {
  states <- c(
    "c3ann",
    "c4ann",
    "c3per",
    "c4per",
    "c3nfx",
    "pastr",
    "range",
    "primf",
    "secdf",
    "primn",
    "secdn",
    "urban"
  )
  cell_a <- c(
    0.10,
    0.05,
    0.02,
    0.01,
    0.02,
    0.15,
    0.10,
    0.20,
    0.15,
    0.05,
    0.10,
    0.05
  )
  cell_b <- c(
    0.04,
    0.02,
    0.01,
    0.01,
    0.02,
    0.05,
    0.05,
    0.40,
    0.20,
    0.05,
    0.10,
    0.05
  )
  grid <- tidyr::expand_grid(
    tibble::tibble(lon = c(-3.25, 35.25), lat = c(40.25, -1.25)),
    year = c(1750L, 2000L)
  )
  purrr::pmap_dfr(grid, function(lon, lat, year) {
    frac <- if (lon < 0) cell_a else cell_b
    tibble::tibble(
      lon = lon,
      lat = lat,
      year = year,
      land_use = states,
      fraction = frac
    )
  })
}

# Minimal country grid: each fixture cell wholly inside one polity.
.luh2_country_grid_fixture <- function() {
  tibble::tribble(
    ~lon, ~lat, ~area_code, ~cell_area_frac,
    -3.25, 40.25, 203L, 1,
    35.25, -1.25, 79L, 1
  )
}

test_that("example returns the documented schema", {
  ex <- whep::read_luh2_landuse(example = TRUE)
  pointblank::expect_col_exists(
    ex,
    c("lon", "lat", "area_code", "year", "land_use", "fraction", "area_ha")
  )
  testthat::expect_true(all(
    ex$land_use %in% c("cropland", "grassland", "natural", "urban")
  ))
  testthat::expect_true(all(ex$fraction >= 0 & ex$fraction <= 1))
})

test_that("class mapping sums member states and fractions tile to ~1", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )

  testthat::expect_setequal(
    unique(out$land_use),
    c("cropland", "grassland", "natural", "urban")
  )

  # cropland class aggregates the five crop states for cell A (lon -3.25)
  crop_a <- out |>
    dplyr::filter(lon == -3.25, year == 1750L, land_use == "cropland") |>
    dplyr::pull(fraction)
  testthat::expect_equal(crop_a, 0.10 + 0.05 + 0.02 + 0.01 + 0.02)

  # grassland class aggregates the pastr and range states
  grass_a <- out |>
    dplyr::filter(lon == -3.25, year == 1750L, land_use == "grassland") |>
    dplyr::pull(fraction)
  testthat::expect_equal(grass_a, 0.15 + 0.10)

  # per cell-year the 4 classes tile to ~1
  totals <- out |>
    dplyr::summarise(tot = sum(fraction), .by = c(lon, lat, year)) |>
    dplyr::pull(tot)
  testthat::expect_true(all(abs(totals - 1) < 1e-8))
})

test_that(".luh2_cell_area_ha equator ~309100 ha and shrinks with latitude", {
  eq <- whep:::.luh2_cell_area_ha(0)
  testthat::expect_equal(eq, 309100, tolerance = 0.01)
  mid <- whep:::.luh2_cell_area_ha(45)
  high <- whep:::.luh2_cell_area_ha(60)
  testthat::expect_true(eq > mid && mid > high)
  # cos-law: 45-deg cell ~ cos(45) of equatorial
  testthat::expect_equal(mid / eq, cos(45 * pi / 180), tolerance = 0.01)
})

test_that("area_ha equals fraction times cell_area_ha", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "grid",
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )
  chk <- out |>
    dplyr::mutate(
      expected = fraction * whep:::.luh2_cell_area_ha(lat)
    )
  testthat::expect_equal(chk$area_ha, chk$expected, tolerance = 1e-6)
})

test_that("1750 is retrievable when the source covers it", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    years = 1750L,
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )
  testthat::expect_true(all(out$year == 1750L))
  testthat::expect_true(nrow(out) > 0L)
})

test_that("polity resolution sums area_ha to (area_code, year, land_use)", {
  raw <- .luh2_raw_fixture()
  out <- whep::read_luh2_landuse(
    resolution = "polity",
    data = list(states = raw, country_grid = .luh2_country_grid_fixture())
  )
  pointblank::expect_col_exists(
    out,
    c("area_code", "year", "land_use", "area_ha")
  )
  testthat::expect_false(any(c("lon", "lat") %in% names(out)))
  testthat::expect_setequal(unique(out$area_code), c(203L, 79L))
})

test_that("real pin smoke test (skipped when unreadable)", {
  states <- tryCatch(
    whep:::.luh2_read_states(years = 1750L),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(states), "luh2_v2h_states pin not readable")
  cat(
    "\nLUH2 states found:",
    paste(sort(unique(states$land_use)), collapse = ", "),
    "\n"
  )
  print(utils::head(as.data.frame(states)))
  pointblank::expect_col_exists(
    states,
    c("lon", "lat", "year", "land_use", "fraction")
  )
})
