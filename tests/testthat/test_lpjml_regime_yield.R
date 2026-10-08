# read_lpjml_regime_yield(): the LPJmL rainfed and irrigated per-stand crop
# yields behind the regime split of #1233. Fully offline: the reader is fed
# injected tibbles, mocked readers or a tiny NetCDF written to a temp dir.
# Nothing here reaches a WHEP_* path.

.lrg_band_names <- function() {
  path <- system.file("extdata", "lpjml_cft_bands.csv", package = "whep")
  bands <- utils::read.csv(path, stringsAsFactors = FALSE)
  bands$band_name[bands$output == "pft_harvestc"]
}

# Injected inputs in the shape the two run readers return: every one of the
# 32 bands in every cell, zero unless `harvest`/`frac` (named by band) set it.
.lrg_inputs <- function(
  harvest = numeric(),
  frac = numeric(),
  lon = 10.25,
  lat = 45.25,
  year = 2010L
) {
  names_pft <- .lrg_band_names()
  grid <- tidyr::expand_grid(
    tibble::tibble(lon = lon, lat = lat),
    year = year,
    npft = seq_along(names_pft)
  ) |>
    dplyr::mutate(name_pft = names_pft[.data$npft])
  h <- unname(harvest[grid$name_pft])
  f <- unname(frac[grid$name_pft])
  list(
    harvestc = dplyr::mutate(grid, value = dplyr::coalesce(h, 0)),
    stand_frac = grid |>
      dplyr::transmute(
        lon,
        lat,
        year,
        band = .data$npft,
        value = dplyr::coalesce(f, 0),
        band_name = .data$name_pft
      )
  )
}

testthat::test_that("the yield is the per-stand harvest, not divided again", {
  # pft_harvestc is already per m2 of the band's own stand (see the header of
  # R/lpjml_regime_yield.R), so a stand fraction of 0.2 must not inflate
  # 100 gC/m2 to 500.
  data <- .lrg_inputs(
    harvest = c("rainfed maize" = 100, "irrigated maize" = 150),
    frac = c("rainfed maize" = 0.2, "irrigated maize" = 0.05)
  )
  out <- whep:::.lrg_crop_yield(data = data)
  testthat::expect_identical(nrow(out), 1L)
  testthat::expect_identical(out$lpjml_crop, "maize")
  testthat::expect_identical(out$yield_rainfed, 100)
  testthat::expect_identical(out$yield_irrigated, 150)
})

testthat::test_that("a regime with no stand is NA; a failed stand stays 0", {
  data <- .lrg_inputs(
    harvest = c("rainfed rice" = 80, "rainfed pulses" = 0),
    frac = c(
      "rainfed rice" = 0.3,
      "rainfed pulses" = 0.1,
      "irrigated pulses" = 0.1
    )
  )
  out <- whep:::.lrg_crop_yield(data = data)
  rice <- out[out$lpjml_crop == "rice", ]
  pulses <- out[out$lpjml_crop == "pulses", ]
  testthat::expect_identical(rice$yield_rainfed, 80)
  testthat::expect_true(is.na(rice$yield_irrigated))
  # Both pulse stands have area; neither harvested anything.
  testthat::expect_identical(pulses$yield_rainfed, 0)
  testthat::expect_identical(pulses$yield_irrigated, 0)
  # A crop with no stand in either regime gets no row at all.
  testthat::expect_setequal(out$lpjml_crop, c("rice", "pulses"))
})

testthat::test_that("harvest on a band with no area does not make a yield", {
  data <- .lrg_inputs(
    harvest = c("rainfed maize" = 90, "irrigated maize" = 500),
    frac = c("rainfed maize" = 0.4)
  )
  out <- whep:::.lrg_crop_yield(data = data)
  testthat::expect_identical(out$yield_rainfed, 90)
  testthat::expect_true(is.na(out$yield_irrigated))
})

testthat::test_that("non-crop and catch-all bands never become a crop", {
  data <- .lrg_inputs(
    harvest = c(
      "rainfed grassland" = 40,
      "irrigated others" = 120,
      "rainfed biomass grass" = 300
    ),
    frac = c(
      "rainfed grassland" = 0.5,
      "irrigated others" = 0.1,
      "rainfed biomass grass" = 0.1
    )
  )
  out <- whep:::.lrg_crop_yield(data = data)
  testthat::expect_identical(nrow(out), 0L)
  testthat::expect_identical(
    names(out),
    names(whep:::.lrg_crop_prototype())
  )
})

testthat::test_that("the others stand is kept only on request", {
  data <- .lrg_inputs(
    harvest = c(
      "rainfed others" = 60,
      "irrigated others" = 120,
      "rainfed grassland" = 40
    ),
    frac = c(
      "rainfed others" = 0.2,
      "irrigated others" = 0.1,
      "rainfed grassland" = 0.5
    )
  )
  out <- whep:::.lrg_crop_yield(data = data, include_others = TRUE)
  # Grassland stays out: only the catch-all crop stand is added.
  testthat::expect_identical(out$lpjml_crop, "others")
  testthat::expect_identical(out$yield_rainfed, 60)
  testthat::expect_identical(out$yield_irrigated, 120)
  testthat::expect_identical(out$stand_frac_rainfed, 0.2)
  testthat::expect_identical(out$stand_frac_irrigated, 0.1)
  testthat::expect_identical(
    names(out),
    names(whep:::.lrg_crop_prototype())
  )
})

testthat::test_that("an absent stand has no stand fraction either", {
  data <- .lrg_inputs(
    harvest = c("rainfed maize" = 90),
    frac = c("rainfed maize" = 0.4)
  )
  out <- whep:::.lrg_crop_yield(data = data)
  testthat::expect_identical(out$stand_frac_rainfed, 0.4)
  testthat::expect_true(is.na(out$stand_frac_irrigated))
})

testthat::test_that("include_others expands others to its cft_mapping items", {
  data <- .lrg_inputs(
    harvest = c("rainfed others" = 60, "rainfed maize" = 90),
    frac = c("rainfed others" = 0.2, "rainfed maize" = 0.3)
  )
  with_others <- whep::read_lpjml_regime_yield(
    data = data,
    include_others = TRUE
  )
  without <- whep::read_lpjml_regime_yield(data = data)
  others_items <- as.integer(
    whep::cft_mapping$item_prod_code[whep::cft_mapping$cft_lpjml == "others"]
  )
  testthat::expect_setequal(
    with_others$item_prod_code[with_others$lpjml_crop == "others"],
    others_items
  )
  testthat::expect_false("others" %in% without$lpjml_crop)
  # The default output keeps its documented columns.
  testthat::expect_identical(names(with_others), names(without))
  testthat::expect_false("stand_frac_rainfed" %in% names(without))
})

testthat::test_that("each LPJmL crop expands to its production items", {
  testthat::skip_if_not_installed("pointblank")
  data <- .lrg_inputs(
    harvest = c("rainfed temperate cereals" = 200),
    frac = c("rainfed temperate cereals" = 0.3)
  )
  out <- whep::read_lpjml_regime_yield(data = data)
  expected <- whep::cft_mapping$item_prod_code[
    whep::cft_mapping$cft_lpjml == "temperate_cereals"
  ]
  testthat::expect_setequal(out$item_prod_code, as.integer(expected))
  testthat::expect_true(all(out$yield_rainfed == 200))
  wheat <- out[out$item_prod_code == 15L, ]
  testthat::expect_identical(wheat$item_cbs_code, 2511L)
  pointblank::expect_col_vals_not_null(out, "item_cbs_code")
})

testthat::test_that("every crop-specific cft_mapping entry resolves to a band", {
  testthat::skip_if_not_installed("pointblank")
  items <- whep:::.lrg_item_bands()
  mapped <- whep::cft_mapping[whep::cft_mapping$cft_lpjml != "others", ]
  # A renamed CFT in either table would silently drop its items; this pins
  # that all 41 crop-specific items find a band, and "others" finds none.
  testthat::expect_identical(nrow(items), 41L)
  testthat::expect_setequal(
    items$item_prod_code,
    as.integer(mapped$item_prod_code)
  )
  testthat::expect_identical(length(unique(items$lpjml_crop)), 12L)
  testthat::expect_identical(anyDuplicated(items$item_prod_code), 0L)
  pointblank::expect_col_vals_not_null(items, "item_cbs_code")
})

testthat::test_that("years before 1901 are stamped as recycled climate", {
  data <- .lrg_inputs(
    harvest = c("rainfed maize" = 90),
    frac = c("rainfed maize" = 0.4),
    year = c(1900L, 1901L)
  )
  out <- whep:::.lrg_crop_yield(data = data)
  testthat::expect_identical(
    out$method_regime_yield[order(out$year)],
    c("lpjml_band_harvest_recycled_climate", "lpjml_band_harvest")
  )
  one <- whep:::.lrg_crop_yield(years = 1901L, data = data)
  testthat::expect_identical(one$year, 1901L)
})

testthat::test_that("a run missing a crop band aborts rather than guessing", {
  data <- .lrg_inputs(
    harvest = c("rainfed maize" = 90),
    frac = c("rainfed maize" = 0.4)
  )
  data$harvestc <- dplyr::filter(
    data$harvestc,
    .data$name_pft != "irrigated sugarcane"
  )
  testthat::expect_error(
    whep:::.lrg_crop_yield(data = data),
    class = "whep_lpjml_regime_yield_bands"
  )
})

testthat::test_that("a stand with area but no harvest value aborts", {
  data <- .lrg_inputs(
    harvest = c("rainfed maize" = 90),
    frac = c("rainfed maize" = 0.4, "irrigated maize" = 0.1)
  )
  data$harvestc <- dplyr::filter(
    data$harvestc,
    !(.data$name_pft == "irrigated maize")
  ) |>
    dplyr::bind_rows(
      tibble::tibble(
        lon = 10.25,
        lat = 45.25,
        year = 2010L,
        npft = 19L,
        name_pft = "irrigated maize",
        value = NA_real_
      )
    )
  testthat::expect_error(
    whep:::.lrg_crop_yield(data = data),
    class = "whep_lpjml_regime_yield_gap"
  )
})

testthat::test_that("with no run directory the reader reads the pinned layer", {
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "")
  seen <- new.env()
  pinned <- tibble::tribble(
    ~lon, ~lat, ~year, ~lpjml_crop, ~yield_rainfed, ~yield_irrigated,
    ~stand_frac_rainfed, ~stand_frac_irrigated, ~method_regime_yield,
    0.25, 40.25, 2010L, "maize", 400, 600, 0.2, 0.1, "lpjml_band_harvest",
    0.25, 40.25, 2010L, "others", 100, 150, 0.1, 0.0, "lpjml_band_harvest",
    0.25, 40.25, 2011L, "maize", 420, 610, 0.2, 0.1, "lpjml_band_harvest"
  )
  testthat::local_mocked_bindings(
    .read_input = function(pin_alias, years = NULL, year_col = NULL) {
      seen$alias <- pin_alias
      data.table::as.data.table(pinned)
    },
    .package = "whep"
  )
  crops <- whep:::.lrg_crop_yield(years = 2010L)
  testthat::expect_equal(seen$alias, "lpjml-crop-regime-yield")
  testthat::expect_equal(crops$lpjml_crop, "maize")
  testthat::expect_equal(crops$year, 2010L)
  with_others <- whep:::.lrg_crop_yield(years = 2010L, include_others = TRUE)
  testthat::expect_setequal(with_others$lpjml_crop, c("maize", "others"))
})

testthat::test_that("a named but missing run directory aborts, never reads the pin", {
  withr::local_envvar(WHEP_LPJML_RUN_DIR = "")
  testthat::expect_error(
    whep::read_lpjml_regime_yield(
      years = 2010L,
      run_dir = file.path(withr::local_tempdir(), "absent")
    ),
    class = "whep_lpjml_regime_yield_no_run"
  )
})

testthat::test_that("a run directory without pft_harvestc.nc aborts", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "cftfrac.nc"))
  testthat::expect_error(
    whep::read_lpjml_regime_yield(years = 2010L, run_dir = dir),
    class = "whep_lpjml_regime_yield_no_run"
  )
})

testthat::test_that("the run path reads each year from the given run", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, c("pft_harvestc.nc", "cftfrac.nc")))
  seen <- new.env()
  seen$dirs <- character()
  seen$years <- integer()
  testthat::local_mocked_bindings(
    read_lpjml_npp = function(var, years, run_dir, ...) {
      seen$dirs <- c(seen$dirs, run_dir)
      seen$years <- c(seen$years, years)
      .lrg_inputs(
        harvest = c("rainfed maize" = 90),
        frac = c("rainfed maize" = 0.4),
        year = years
      )$harvestc
    },
    read_lpjml_hydrology = function(var, run_dir, years, ...) {
      seen$dirs <- c(seen$dirs, run_dir)
      .lrg_inputs(
        harvest = c("rainfed maize" = 90),
        frac = c("rainfed maize" = 0.4),
        year = years
      )$stand_frac
    }
  )
  out <- whep::read_lpjml_regime_yield(years = c(1990L, 2010L), run_dir = dir)
  testthat::expect_identical(seen$years, c(1990L, 2010L))
  testthat::expect_true(all(seen$dirs == dir))
  testthat::expect_setequal(out$year, c(1990L, 2010L))
  testthat::expect_setequal(out$lpjml_crop, "maize")
})

# A tiny run directory: pft_harvestc.nc and cftfrac.nc for 2 cells x 32
# bands x 2 years, with the harvest file's bands in REVERSED order so that a
# positional join would pair every band with the wrong crop.
.lrg_write_run <- function(first_year = 1900L) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  names_pft <- .lrg_band_names()
  lon <- c(10.25, 10.75)
  lat <- 45.25
  n_year <- 2L
  frac <- array(0, dim = c(2, 1, 32, n_year))
  harv <- array(0, dim = c(2, 1, 32, n_year))
  rm <- match("rainfed maize", names_pft)
  im <- match("irrigated maize", names_pft)
  frac[1, 1, rm, ] <- 0.3
  frac[1, 1, im, ] <- 0.1
  frac[2, 1, rm, ] <- 0.2
  harv[1, 1, rm, ] <- c(120, 130)
  harv[1, 1, im, ] <- c(200, 210)
  harv[2, 1, rm, ] <- c(50, 60)
  rev_idx <- rev(seq_along(names_pft))
  .lrg_write_cube(dir, "cftfrac.nc", "CFTfrac", frac, names_pft, first_year)
  .lrg_write_cube(
    dir,
    "pft_harvestc.nc",
    "harvestc",
    harv[,, rev_idx, , drop = FALSE],
    names_pft[rev_idx],
    first_year
  )
  dir
}

.lrg_write_cube <- function(dir, file, var_name, vals, names_pft, first_year) {
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", c(10.25, 10.75))
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", 45.25)
  dim_pft <- ncdf4::ncdim_def("npft", "", seq_along(names_pft))
  dim_time <- ncdf4::ncdim_def(
    "time",
    paste0("years since ", first_year, "-1-1"),
    seq_len(dim(vals)[4]) - 1L
  )
  dim_char <- ncdf4::ncdim_def(
    "len",
    "",
    seq_len(max(nchar(names_pft))),
    create_dimvar = FALSE
  )
  name_var <- ncdf4::ncvar_def(
    "NamePFT",
    "",
    list(dim_char, dim_pft),
    prec = "char"
  )
  var <- ncdf4::ncvar_def(
    var_name,
    "",
    list(dim_lon, dim_lat, dim_pft, dim_time),
    missval = -9999
  )
  nc <- ncdf4::nc_create(file.path(dir, file), list(name_var, var))
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::ncvar_put(nc, name_var, names_pft)
  ncdf4::nc_close(nc)
}

testthat::test_that("a real NetCDF run joins the two files by band name", {
  testthat::skip_if_not_installed("ncdf4")
  dir <- .lrg_write_run(first_year = 1900L)
  out <- whep:::.lrg_crop_yield(run_dir = dir)
  out <- out[order(out$year, out$lon), ]
  testthat::expect_identical(out$year, c(1900L, 1900L, 1901L, 1901L))
  testthat::expect_identical(out$lpjml_crop, rep("maize", 4L))
  testthat::expect_identical(out$yield_rainfed, c(120, 50, 130, 60))
  testthat::expect_identical(
    out$yield_irrigated,
    c(200, NA_real_, 210, NA_real_)
  )
  testthat::expect_identical(
    out$method_regime_yield,
    rep(
      c("lpjml_band_harvest_recycled_climate", "lpjml_band_harvest"),
      each = 2
    )
  )
  one <- whep::read_lpjml_regime_yield(years = 1901L, run_dir = dir)
  testthat::expect_true(all(one$year == 1901L))
  testthat::expect_setequal(one$item_prod_code, c(56L, 446L))
})

testthat::test_that("the example returns the documented schema", {
  testthat::skip_if_not_installed("pointblank")
  out <- whep::read_lpjml_regime_yield(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_identical(
    names(out),
    c(
      "lon",
      "lat",
      "year",
      "item_prod_code",
      "item_cbs_code",
      "lpjml_crop",
      "yield_rainfed",
      "yield_irrigated",
      "method_regime_yield"
    )
  )
  pointblank::expect_col_vals_in_set(
    out,
    "method_regime_yield",
    c("lpjml_band_harvest", "lpjml_band_harvest_recycled_climate")
  )
  testthat::expect_type(out$yield_irrigated, "double")
  testthat::expect_type(out$item_prod_code, "integer")
})
