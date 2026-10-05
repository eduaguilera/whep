# Annual irrigation depth per cell, item and water regime. Offline
# throughout: the LPJmL cubes are injected or the readers mocked.

# Applied water per month as a share of the peak month: 12 peaks a year, so
# an annual depth is exactly 12 times the stand's `peak_mm`.
.lid_month_pattern <- function() {
  c(0, 0, 0, 0, 1, 2, 4, 3, 2, 0, 0, 0)
}

# Two cells. Cell A grows every crop the tests need, irrigated and rainfed;
# cell B only rainfed maize. Annual depths: maize 120, rice 600, rainfed
# (paddy) rice 240, others 360, temperate cereals 180, tropical cereals 60.
.lid_stands <- function() {
  tibble::tribble(
    ~lon, ~lat, ~band_name,                    ~stand_frac, ~peak_mm,
    0.25, 0.25, "irrigated maize",             0.2,         10,
    0.25, 0.25, "rainfed maize",               0.3,         0,
    0.25, 0.25, "irrigated rice",              0.1,         50,
    0.25, 0.25, "rainfed rice",                0.05,        20,
    0.25, 0.25, "irrigated others",            0.1,         30,
    0.25, 0.25, "rainfed others",              0.2,         0,
    0.25, 0.25, "irrigated temperate cereals", 0.05,        15,
    0.25, 0.25, "irrigated tropical cereals",  0.15,        5,
    0.75, 0.25, "rainfed maize",               0.5,         0
  )
}

# The run's 32-band vocabulary, as cftfrac.nc names and numbers it.
.lid_band_vocab <- function() {
  path <- system.file("extdata", "lpjml_cft_bands.csv", package = "whep")
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::filter(.data$output == "cftfrac") |>
    dplyr::select("band", "band_name")
}

# Every band appears at a third cell with no stand, so both cubes carry the
# full vocabulary as a run does; zero-area stands carry no water.
.lid_background <- function() {
  tidyr::expand_grid(
    lon = 10.25,
    lat = 10.25,
    .lid_band_vocab(),
    stand_frac = 0,
    peak_mm = 0
  )
}

.lid_cube_rows <- function(stands = .lid_stands(), year = 2010L) {
  dplyr::bind_rows(stands, .lid_background()) |>
    dplyr::select(-dplyr::any_of("band")) |>
    dplyr::left_join(.lid_band_vocab(), by = "band_name") |>
    dplyr::mutate(year = year)
}

.lid_airrig <- function(stands = .lid_stands(), year = 2010L) {
  .lid_cube_rows(stands, year) |>
    tidyr::expand_grid(month = 1:12) |>
    dplyr::mutate(
      value = .data$peak_mm * .lid_month_pattern()[.data$month]
    ) |>
    dplyr::select("lon", "lat", "year", "month", "band", "band_name", "value")
}

.lid_stand_frac <- function(stands = .lid_stands(), year = 2010L) {
  .lid_cube_rows(stands, year) |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "band",
      "band_name",
      value = "stand_frac"
    )
}

.lid_vintage <- function() {
  tibble::tibble(
    lpjml_version = "LPJmL C Version 6.1.1",
    lpjml_git_hash = "e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605",
    lpjml_run = "fixture/scenario_1, started Mon Aug 31 14:13:10 2026"
  )
}

.lid_data <- function(stands = .lid_stands(), year = 2010L) {
  list(
    airrig_month = .lid_airrig(stands, year),
    stand_frac = .lid_stand_frac(stands, year),
    vintage = .lid_vintage()
  )
}

.lid_row <- function(x, lon, cbs, regime = "irrigated") {
  x[
    x$lon == lon & x$item_cbs_code == cbs & x$water_regime == regime,
  ]
}

testthat::test_that("the twelve months sum to the annual depth per stand", {
  crop <- whep:::.lid_crop_depth(data = .lid_data())
  a <- crop[crop$lon == 0.25, ]
  depth <- stats::setNames(a$irrig_mm_irrigated, a$lpjml_crop)
  testthat::expect_equal(
    depth[c("maize", "rice", "others", "temperate cereals")],
    c(maize = 120, rice = 600, others = 360, "temperate cereals" = 180)
  )
  maize <- a[a$lpjml_crop == "maize", ]
  testthat::expect_equal(maize$stand_frac_irrigated, 0.2)
  testthat::expect_equal(maize$stand_frac_rainfed, 0.3)
  testthat::expect_equal(maize$irrig_mm_rainfed, 0)
  # Cell B has no irrigated maize stand: no depth, and not a zero one.
  b <- crop[crop$lon == 0.75, ]
  testthat::expect_identical(b$lpjml_crop, "maize")
  testthat::expect_true(is.na(b$irrig_mm_irrigated))
  # The zero-area background cell yields no row at all.
  testthat::expect_false(any(crop$lon == 10.25))
})

testthat::test_that("the crop grain keeps the paddy water on rainfed rice", {
  crop <- whep:::.lid_crop_depth(data = .lid_data())
  rice <- crop[crop$lon == 0.25 & crop$lpjml_crop == "rice", ]
  testthat::expect_equal(rice$irrig_mm_rainfed, 240)
  testthat::expect_equal(rice$irrig_mm_irrigated, 600)
})

testthat::test_that("every row carries the run's vintage", {
  out <- whep:::.lpjml_irrig_depth(data = .lid_data())
  testthat::skip_if_not_installed("pointblank")
  pointblank::expect_col_vals_in_set(
    out,
    "lpjml_git_hash",
    "e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605"
  )
  pointblank::expect_col_vals_not_null(out, "lpjml_version")
  pointblank::expect_col_vals_not_null(out, "lpjml_run")
})

testthat::test_that("items take their crop's depth; others is a proxy", {
  out <- whep:::.lpjml_irrig_depth(data = .lid_data())
  maize <- .lid_row(out, 0.25, 2514L)
  testthat::expect_equal(maize$irrig_mm, 120)
  testthat::expect_identical(maize$method_irrig_depth, "lpjml_airrig_stand")
  rice <- .lid_row(out, 0.25, 2807L)
  testthat::expect_equal(rice$irrig_mm, 600)
  # Tomatoes sit on LPJmL's composite "others" stand.
  tomatoes <- .lid_row(out, 0.25, 2601L)
  testthat::expect_equal(tomatoes$irrig_mm, 360)
  testthat::expect_identical(tomatoes$method_irrig_depth, "lpjml_others_proxy")
  testthat::expect_identical(tomatoes$lpjml_crop, "others")
})

testthat::test_that("an item pooling crops weights them by irrigated stand", {
  out <- whep:::.lpjml_irrig_depth(data = .lid_data())
  # Cereals, Other: temperate (180 mm on 0.05) and tropical (60 on 0.15).
  cereals <- .lid_row(out, 0.25, 2520L)
  testthat::expect_equal(cereals$irrig_mm, (180 * 0.05 + 60 * 0.15) / 0.2)
  testthat::expect_identical(cereals$method_irrig_depth, "lpjml_airrig_stand")
  testthat::expect_identical(
    cereals$lpjml_crop,
    "temperate cereals+tropical cereals"
  )
  # Fodder cereals: all three crops stand in by proxy, maize included.
  fodder <- .lid_row(out, 0.25, 2000L)
  testthat::expect_equal(
    fodder$irrig_mm,
    (120 * 0.2 + 180 * 0.05 + 60 * 0.15) / 0.4
  )
  testthat::expect_identical(fodder$method_irrig_depth, "lpjml_crop_proxy")
  # Vegetables, Other: green maize directly, the rest on "others"; the least
  # direct method labels the row.
  veg <- .lid_row(out, 0.25, 2605L)
  testthat::expect_equal(veg$irrig_mm, (120 * 0.2 + 360 * 0.1) / 0.3)
  testthat::expect_identical(veg$method_irrig_depth, "lpjml_others_proxy")
  testthat::expect_identical(veg$lpjml_crop, "maize+others")
})

testthat::test_that("a depth under the recycled pre-1901 weather says so", {
  out <- whep:::.lpjml_irrig_depth(data = .lid_data(year = 1890L))
  testthat::expect_identical(
    .lid_row(out, 0.25, 2514L)$method_irrig_depth,
    "lpjml_airrig_stand_recycled_climate"
  )
  testthat::expect_identical(
    .lid_row(out, 0.25, 2601L)$method_irrig_depth,
    "lpjml_others_proxy_recycled_climate"
  )
  # Only LPJmL's own depths are stamped: a missing depth stays as it is.
  testthat::expect_identical(
    .lid_row(out, 0.75, 2514L)$method_irrig_depth,
    "no_irrigated_stand"
  )
  testthat::expect_identical(
    .lid_row(out, 0.25, 2514L, "rainfed")$method_irrig_depth,
    "rainfed_regime"
  )
})

testthat::test_that("rainfed rows are zero by definition, rice included", {
  out <- whep:::.lpjml_irrig_depth(data = .lid_data())
  rainfed <- out[out$water_regime == "rainfed", ]
  irrigated <- out[out$water_regime == "irrigated", ]
  testthat::expect_identical(nrow(rainfed), nrow(irrigated))
  testthat::expect_true(all(rainfed$irrig_mm == 0))
  testthat::expect_true(all(rainfed$method_irrig_depth == "rainfed_regime"))
  # Rice's rainfed stand received 240 mm of paddy water, but the rainfed
  # regime is unirrigated by definition.
  testthat::expect_equal(.lid_row(out, 0.25, 2807L, "rainfed")$irrig_mm, 0)
})

testthat::test_that("no irrigated stand in the cell means no depth, not 0", {
  out <- whep:::.lpjml_irrig_depth(data = .lid_data())
  maize_b <- .lid_row(out, 0.75, 2514L)
  testthat::expect_true(is.na(maize_b$irrig_mm))
  testthat::expect_identical(maize_b$method_irrig_depth, "no_irrigated_stand")
  testthat::expect_identical(maize_b$lpjml_crop, "maize")
  # A pooled item none of whose crops is irrigated there names them all.
  veg_b <- .lid_row(out, 0.75, 2605L)
  testthat::expect_true(is.na(veg_b$irrig_mm))
  testthat::expect_identical(veg_b$method_irrig_depth, "no_irrigated_stand")
})

testthat::test_that("keys: an item with no LPJmL crop is stamped, not filled", {
  keys <- tibble::tribble(
    ~lon,  ~lat, ~year, ~item_cbs_code, ~water_regime, ~area_code,
    0.25,  0.25, 2010,  3003,           "irrigated",   1L,
    0.25,  0.25, 2010,  3003,           "rainfed",     1L,
    0.25,  0.25, 2010,  2514,           "irrigated",   1L,
    20.25, 0.25, 2010,  2514,           "irrigated",   2L
  )
  out <- whep:::.lpjml_irrig_depth(data = .lid_data(), keys = keys)
  testthat::expect_identical(nrow(out), nrow(keys))
  testthat::expect_identical(out$area_code, keys$area_code)
  testthat::expect_identical(
    out$method_irrig_depth,
    c("no_band", "rainfed_regime", "lpjml_airrig_stand", "no_irrigated_stand")
  )
  testthat::expect_equal(out$irrig_mm, c(NA, 0, 120, NA))
})

testthat::test_that("keys need a known water regime", {
  keys <- tibble::tibble(
    lon = 0.25,
    lat = 0.25,
    year = 2010L,
    item_cbs_code = 2514L,
    water_regime = "flooded"
  )
  testthat::expect_error(
    whep:::.lpjml_irrig_depth(data = .lid_data(), keys = keys),
    class = "whep_lpjml_irrig_depth_keys"
  )
})

testthat::test_that("depth x irrigated stand reproduces the irrigated water", {
  # The reference is build_crop_water_use() on the same cubes: its
  # airrig_cell_mm, summed over the irrigated bands and the months and times
  # the cell area, is the irrigated part of the cell's applied water.
  d <- .lid_data()
  cwu <- whep::build_crop_water_use(
    data = list(
      airrig_month = d$airrig_month,
      stand_frac = d$stand_frac,
      country_grid = tibble::tibble(
        lon = 0.25,
        lat = 0.25,
        area_code = 1L,
        cell_area_frac = 1
      )
    )
  )
  cell_area_ha <- whep:::.cell_area_ha_lat(0.25)
  reference <- cwu |>
    dplyr::filter(stringr::str_starts(.data$band_name, "irrigated")) |>
    dplyr::summarise(m3 = sum(.data$airrig_cell_mm) * cell_area_ha * 10) |>
    dplyr::pull("m3")
  crop <- whep:::.lid_crop_depth(data = d)
  crop_a <- crop[crop$lon == 0.25, ]
  from_depth <- sum(
    crop_a$irrig_mm_irrigated * crop_a$stand_frac_irrigated,
    na.rm = TRUE
  ) *
    cell_area_ha *
    10
  testthat::expect_gt(reference, 0)
  testthat::expect_equal(from_depth, reference)
  # Through the item mapping: every item carries its crop's depth unchanged,
  # so each crop's water is still depth x stand, whichever items share it.
  items <- whep:::.lid_item_depth(crop_a)
  per_crop <- dplyr::distinct(
    items[is.finite(items$irrig_mm), ],
    .data$lpjml_crop,
    .data$irrig_mm,
    .data$stand_frac_irrigated
  )
  testthat::expect_identical(anyDuplicated(per_crop$lpjml_crop), 0L)
  testthat::expect_equal(
    sum(per_crop$irrig_mm * per_crop$stand_frac_irrigated) *
      cell_area_ha *
      10,
    reference
  )
})

testthat::test_that("a stand short of a month aborts where the gap is made", {
  d <- .lid_data()
  d$airrig_month <- d$airrig_month[
    !(d$airrig_month$band_name == "irrigated maize" &
      d$airrig_month$lon == 0.25 &
      d$airrig_month$month == 7L),
  ]
  # The cell-water identity holds over the eleven months that remain, so it
  # cannot see the absent July; the guard must.
  cwu <- whep::build_crop_water_use(
    data = list(
      airrig_month = d$airrig_month,
      stand_frac = d$stand_frac,
      country_grid = tibble::tibble(
        lon = 0.25,
        lat = 0.25,
        area_code = 1L,
        cell_area_frac = 1
      )
    )
  )
  expect_supplied_guard(
    identity = isTRUE(all.equal(
      sum(cwu$airrig_cell_mm),
      sum(cwu$airrig_stand_mm * cwu$stand_frac)
    )),
    guard = whep:::.lid_crop_depth(data = d),
    class = "whep_lpjml_irrig_depth_months"
  )
})

testthat::test_that("a stand with area and no water row aborts", {
  d <- .lid_data()
  d$airrig_month <- d$airrig_month[
    !(d$airrig_month$band_name == "irrigated rice" &
      d$airrig_month$lon == 0.25),
  ]
  testthat::expect_error(
    whep:::.lid_crop_depth(data = d),
    class = "whep_lpjml_irrig_depth_gap"
  )
})

testthat::test_that("a cube missing a crop band aborts", {
  d <- .lid_data()
  d$airrig_month <- d$airrig_month[
    d$airrig_month$band_name != "irrigated pulses",
  ]
  testthat::expect_error(
    whep:::.lid_crop_depth(data = d),
    class = "whep_lpjml_irrig_depth_bands"
  )
})

testthat::test_that("the single-band cube of the defective vintage aborts", {
  # The 2026-08-27 cube put every crop's water on one band.
  d <- .lid_data()
  d$airrig_month$value[d$airrig_month$band_name != "irrigated others"] <- 0
  testthat::expect_error(
    whep:::.lid_crop_depth(data = d),
    "single band"
  )
})

testthat::test_that("a run without cft_airrig_month aborts, never falls back", {
  run <- withr::local_tempdir()
  file.create(file.path(run, "cftfrac.nc"))
  testthat::expect_error(
    whep:::.lpjml_irrig_depth(years = 2010L, run_dir = run),
    class = "whep_lpjml_irrig_depth_vintage"
  )
})

testthat::test_that("with no run and no published pin, the error names both", {
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "")
  testthat::local_mocked_bindings(.lid_pin_registered = function() FALSE)
  testthat::expect_error(
    whep:::.lpjml_irrig_depth(years = 2010L),
    class = "whep_lpjml_irrig_depth_no_pin"
  )
  testthat::expect_error(
    whep:::.lpjml_irrig_depth(years = 2010L),
    "run_dir"
  )
})

testthat::test_that("the pin is read when published and no run is named", {
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "")
  pinned <- whep:::.lid_crop_depth(data = .lid_data())
  testthat::local_mocked_bindings(
    .lid_pin_registered = function() TRUE,
    .read_lpjml_pin = function(alias, years = NULL) {
      testthat::expect_identical(alias, "lpjml-crop-irrig-depth")
      pinned
    }
  )
  out <- whep:::.lpjml_irrig_depth(years = 2010L)
  testthat::expect_equal(.lid_row(out, 0.25, 2514L)$irrig_mm, 120)
})

testthat::test_that("reading without years aborts instead of exhausting memory", {
  testthat::expect_error(
    whep:::.lpjml_irrig_depth(),
    class = "whep_lpjml_irrig_depth_years"
  )
})

testthat::test_that("supplied cubes need the run they came from", {
  d <- .lid_data()
  d$vintage <- NULL
  testthat::expect_error(
    whep:::.lid_crop_depth(data = d),
    class = "whep_lpjml_irrig_depth_vintage"
  )
})

testthat::test_that("the vintage is read from both cubes, which must agree", {
  attrs <- list(
    source = "LPJmL C Version 6.1.1",
    GIT_hash = "e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605",
    history = "Mon Aug 31 14:13:10 2026: ./bin/lpjml config.json"
  )
  testthat::local_mocked_bindings(.lid_file_attrs = function(path) attrs)
  run <- file.path("runs", "global_v2", "output", "scenario_1")
  vintage <- whep:::.lid_run_vintage(run)
  testthat::expect_identical(vintage$lpjml_version, "LPJmL C Version 6.1.1")
  testthat::expect_identical(
    vintage$lpjml_run,
    "global_v2/scenario_1, started Mon Aug 31 14:13:10 2026"
  )
  testthat::local_mocked_bindings(
    .lid_file_attrs = function(path) {
      if (basename(path) == "cftfrac.nc") {
        attrs$history <- "Thu Aug 27 09:00:00 2026: ./bin/lpjml config.json"
      }
      attrs
    }
  )
  testthat::expect_error(
    whep:::.lid_run_vintage(run),
    class = "whep_lpjml_irrig_depth_vintage"
  )
})

testthat::test_that("a table mixing two runs cannot be stamped", {
  crop <- whep:::.lid_crop_depth(data = .lid_data())
  crop$lpjml_run[1] <- "another run"
  testthat::expect_error(
    whep:::.lid_vintage_of(crop),
    class = "whep_lpjml_irrig_depth_vintage"
  )
})

testthat::test_that("every mapped item's basis is a known one", {
  bands <- whep:::.lid_item_bands()
  testthat::expect_setequal(
    unique(bands$method_irrig_depth),
    c("lpjml_airrig_stand", "lpjml_crop_proxy", "lpjml_others_proxy")
  )
  # Each item's crop is one LPJmL actually has a band for.
  testthat::expect_true(all(
    bands$lpjml_crop %in%
      whep:::.lrg_crop_bands(include_others = TRUE)$lpjml_crop
  ))
})
