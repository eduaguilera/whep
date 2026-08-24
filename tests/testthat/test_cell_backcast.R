# Two cells, one polity, two crops with disjoint footprints. Cell A's cropland
# halves every year going back; cell B's is flat. So the two crops MUST get
# different back-cast ratios, and a method that used the country's total
# cropland -- the thing this one replaces -- would give them the same one.
#
# Cell A also carries a SECOND crop with a different area, so a rule that
# scaled every crop by the cell's total (or mean) 1961 harvest rather than by
# its own would land on a different number.
.cell_gridded_1961 <- function() {
  tibble::tribble(
    ~lon,  ~lat, ~item_prod_code, ~harvested_ha,
    0.25, 50.25,             15L,           100,
    0.25, 50.25,             71L,            20,
    0.75, 50.25,             56L,           200
  )
}

.cell_cropland <- function() {
  tibble::tribble(
    ~lon,  ~lat, ~year, ~cropland_ha,
    0.25, 50.25, 1959L,          250,
    0.25, 50.25, 1960L,          500,
    0.25, 50.25, 1961L,         1000,
    0.75, 50.25, 1959L,         1000,
    0.75, 50.25, 1960L,         1000,
    0.75, 50.25, 1961L,         1000
  )
}

.cell_polity_areas <- function() {
  tibble::tibble(year = 1959:1961, area_code = 1L, polity_code = "P") |>
    data.table::as.data.table()
}

.cell_cover <- function() {
  tibble::tribble(
    ~polity_code, ~lon,  ~lat, ~frac,
    "P",          0.25, 50.25,     1,
    "P",          0.75, 50.25,     1
  ) |>
    data.table::as.data.table()
}

.cell_land <- function(...) {
  overrides <- list(...)
  args <- list(
    gridded_1961 = .cell_gridded_1961(),
    cell_cropland = .cell_cropland(),
    polity_areas = .cell_polity_areas(),
    cover = .cell_cover()
  )
  args[names(overrides)] <- overrides
  whep::build_cell_crop_land(years = 1959:1961, data = args)
}

test_that("build_cell_crop_land scales each crop by its own cells", {
  land <- .cell_land()

  wheat <- land |>
    dplyr::filter(item_prod_code == 15) |>
    dplyr::arrange(year) |>
    dplyr::pull(cropland_mha)
  maize <- land |>
    dplyr::filter(item_prod_code == 56) |>
    dplyr::arrange(year) |>
    dplyr::pull(cropland_mha)

  rye <- land |>
    dplyr::filter(item_prod_code == 71) |>
    dplyr::arrange(year) |>
    dplyr::pull(cropland_mha)

  # Wheat follows cell A: 250/1000 and 500/1000 of its 1961 100 ha.
  expect_equal(wheat, c(25, 50, 100) / 1e6)
  # Rye shares cell A, so it takes the same RATIO from its own 20 ha -- not the
  # cell's total or mean 1961 harvest.
  expect_equal(rye, c(5, 10, 20) / 1e6)
  # Maize follows cell B, whose cropland never moves.
  expect_equal(maize, rep(200 / 1e6, 3))
  # The two cells therefore do NOT share a ratio, which is the whole point.
  expect_false(isTRUE(all.equal(wheat / wheat[3], maize / maize[3])))
})

test_that("build_cell_crop_land drops cells with no 1961 cropland", {
  flat <- .cell_cropland()
  flat$cropland_ha[flat$lon == 0.25 & flat$year == 1961L] <- 0

  expect_warning(
    land <- .cell_land(cell_cropland = flat),
    "no 1961 cropland"
  )
  expect_setequal(unique(land$item_prod_code), 56)
})

test_that("build_cell_crop_land shares a cell between two crops", {
  land <- .cell_land()
  a_cell <- land |>
    dplyr::filter(year == 1960L, item_prod_code %in% c(15, 71)) |>
    dplyr::arrange(item_prod_code) |>
    dplyr::pull(cropland_mha)
  # 50 and 10, not 30 and 30: the two crops in one cell keep their own levels.
  expect_equal(a_cell, c(50, 10) / 1e6)
})

test_that("build_cell_crop_land shares a cell between two polities", {
  areas <- data.table::data.table(
    year = rep(1959:1961, each = 2L),
    area_code = c(1L, 2L),
    polity_code = c("P", "Q")
  )
  cover <- data.table::data.table(
    polity_code = c("P", "Q"),
    lon = 0.25,
    lat = 50.25,
    frac = c(3, 1)
  )
  land <- .cell_land(polity_areas = areas, cover = cover)

  # Cell A only: cell B is in no polity now, so it contributes nothing, and the
  # 3:1 territory split is what each crop's area is apportioned by.
  by_area <- land |>
    dplyr::filter(year == 1961L, item_prod_code == 15) |>
    dplyr::arrange(area_code) |>
    dplyr::pull(cropland_mha)
  expect_equal(by_area, c(75, 25) / 1e6)
  expect_equal(sum(by_area), 100 / 1e6)
  expect_setequal(unique(land$item_prod_code), c(15, 71))
})

# --- The identity the method rests on ---------------------------------------

# `pre_crop` as `.fill_pre_faostat()` hands it over: one series, 1961 observed,
# every earlier year missing, with the crop-specific proxy still to be joined.
.anchor_frame <- function(years = 1950:1961) {
  tibble::tibble(
    year = years,
    area_code = 1L,
    item_prod_code = 15L,
    unit = "tonnes",
    value_cropland = dplyr::if_else(years == 1961L, 1000, NA_real_),
    .observed_value = years == 1961L,
    Cropland = 1.5 * 1.02^(years - 1850L)
  )
}

.anchor_proxy <- function(years = 1950:1961, hole = integer()) {
  tibble::tibble(
    year = years,
    area_code = 1L,
    item_prod_code = 15L,
    cropland_mha = 1.5 * 1.02^(years - 1850L)
  ) |>
    dplyr::filter(!year %in% hole)
}

.fill_cols <- c("area_code", "item_prod_code", "unit")
.keys <- c("year", "area_code", "item_prod_code")

test_that("the anchor ratio and the year-by-year walk telescope to the same series", {
  walked <- whep::fill_proxy_growth(
    .anchor_frame(),
    value_col = value_cropland,
    proxy_col = "Cropland",
    time_col = year,
    .by = .fill_cols,
    verbose = FALSE
  ) |>
    dplyr::arrange(year) |>
    dplyr::pull(value_cropland)

  anchored <- whep:::.fill_anchor_ratio(
    .anchor_frame(),
    .anchor_proxy(),
    keys = .keys,
    fill_cols = .fill_cols
  ) |>
    dplyr::arrange(year) |>
    dplyr::pull(value_cropland)

  expect_equal(anchored, walked, tolerance = 1e-10)
  # And it really is the single 1961 ratio, not a coincidence of the fixture.
  expect_equal(anchored[1], 1000 * (1.02^(1950 - 1850)) / (1.02^(1961 - 1850)))
})

test_that("a one-year hole costs one year, not every year before it", {
  frame <- .anchor_frame()
  holed <- .anchor_frame()
  holed$Cropland[holed$year == 1955L] <- NA_real_

  walked <- whep::fill_proxy_growth(
    holed,
    value_col = value_cropland,
    proxy_col = "Cropland",
    time_col = year,
    .by = .fill_cols,
    verbose = FALSE
  ) |>
    dplyr::arrange(year)

  anchored <- whep:::.fill_anchor_ratio(
    frame,
    .anchor_proxy(hole = 1955L),
    keys = .keys,
    fill_cols = .fill_cols
  ) |>
    dplyr::arrange(year)

  # The walk cannot step across the gap, so 1950-1955 all go.
  expect_true(all(is.na(walked$value_cropland[walked$year <= 1955L])))
  # The anchor ratio loses 1955 alone; 1950-1954 are unaffected because no
  # other year's result was ever computed from 1955's.
  expect_true(is.na(anchored$value_cropland[anchored$year == 1955L]))
  expect_false(any(is.na(anchored$value_cropland[anchored$year != 1955L])))
  expect_equal(
    anchored$value_cropland[anchored$year == 1954L],
    1000 * (1.02^(1954 - 1850)) / (1.02^(1961 - 1850))
  )
})

test_that("the anchor ratio refuses a zero or missing proxy rather than dividing", {
  zeroed <- .anchor_proxy()
  zeroed$cropland_mha[zeroed$year == 1957L] <- 0

  out <- whep:::.fill_anchor_ratio(
    .anchor_frame(),
    zeroed,
    keys = .keys,
    fill_cols = .fill_cols
  ) |>
    dplyr::arrange(year)

  expect_true(is.na(out$value_cropland[out$year == 1957L]))
  expect_true(all(is.finite(out$value_cropland[out$year != 1957L])))
})

test_that("the anchor ratio never overwrites an observed value", {
  frame <- .anchor_frame()
  out <- whep:::.fill_anchor_ratio(
    frame,
    .anchor_proxy(),
    keys = .keys,
    fill_cols = .fill_cols
  )
  expect_equal(
    out$value_cropland[out$year == 1961L],
    frame$value_cropland[frame$year == 1961L]
  )
})

test_that("the anchor ratio uses the nearest observation, not a fixed year", {
  frame <- .anchor_frame()
  frame$value_cropland[frame$year == 1952L] <- 40
  frame$.observed_value[frame$year == 1952L] <- TRUE

  out <- whep:::.fill_anchor_ratio(
    frame,
    .anchor_proxy(),
    keys = .keys,
    fill_cols = .fill_cols
  ) |>
    dplyr::arrange(year)

  # 1951 sits next to the 1952 observation, so it is referenced to that one.
  expect_equal(
    out$value_cropland[out$year == 1951L],
    40 * (1.02^(1951 - 1850)) / (1.02^(1952 - 1850))
  )
  # 1953 is also nearest to 1952 ...
  expect_equal(
    out$value_cropland[out$year == 1953L],
    40 * (1.02^(1953 - 1850)) / (1.02^(1952 - 1850))
  )
  # ... but 1958 is nearer to 1961, so a rule that always reached backwards to
  # the last observation at or before the year would answer differently here.
  expect_equal(
    out$value_cropland[out$year == 1958L],
    1000 * (1.02^(1958 - 1850)) / (1.02^(1961 - 1850))
  )
})

test_that(".fill_crop_backcast dispatches on whether a cell table is given", {
  frame <- .anchor_frame()
  holed <- .anchor_frame()
  holed$Cropland[holed$year == 1955L] <- NA_real_

  walked <- whep:::.fill_crop_backcast(
    holed,
    crop_land = NULL,
    join_keys = c("year", "area_code"),
    fill_cols = .fill_cols
  ) |>
    dplyr::arrange(year)
  anchored <- whep:::.fill_crop_backcast(
    frame,
    crop_land = .anchor_proxy(hole = 1955L),
    join_keys = c("year", "area_code"),
    fill_cols = .fill_cols
  ) |>
    dplyr::arrange(year)

  # Same input hole, two methods, two answers -- and the dispatch is what
  # decides which, so a branch that ignored `crop_land` would collapse them.
  expect_true(all(is.na(walked$value_cropland[walked$year <= 1955L])))
  expect_equal(sum(is.na(anchored$value_cropland)), 1L)
})

test_that("build_primary_production rejects an unknown land_method", {
  expect_error(
    whep::build_primary_production(land_method = "cells"),
    class = "rlang_error"
  )
})

test_that("the cell land table is only read for the cell method", {
  expect_null(whep:::.cell_crop_land_wide("present_day", 1850:2023))
  expect_null(whep:::.cell_crop_land_wide("historical_polity", 1850:2023))
  # A post-1961 window needs no back-cast at all.
  expect_null(whep:::.cell_crop_land_wide("cell_polity", 1990:2000))
})

test_that("a cell land table missing requested years aborts", {
  withr::local_envvar(
    WHEP_CELL_CROP_LAND_PATH = withr::local_tempfile(
      fileext = ".parquet"
    )
  )
  path <- Sys.getenv("WHEP_CELL_CROP_LAND_PATH")
  nanoparquet::write_parquet(
    tibble::tibble(
      year = 1961L,
      area_code = 1L,
      item_prod_code = 15,
      cropland_mha = 1
    ),
    path
  )
  expect_error(
    whep:::.read_cell_crop_land(1960:1961),
    "does not cover"
  )
})

test_that("build_cell_crop_land has a runnable example fixture", {
  fixture <- whep::build_cell_crop_land(example = TRUE)
  expect_s3_class(fixture, "tbl_df")
  expect_named(
    fixture,
    c("year", "area_code", "polity_code", "item_prod_code", "cropland_mha")
  )
  expect_true(all(fixture$cropland_mha >= 0))
})
