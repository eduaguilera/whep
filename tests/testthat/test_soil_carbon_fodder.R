# method_unspatialized = "fodder_pattern" (whep#1118): fodder carbon the crop
# pattern cannot place goes on the Monfreda (2008) forage layer instead of
# being spread uniformly over the polity's cropland. Hand-built fixtures, no
# pins, no WHEP_* path: the one real-raster reader test writes its own tiny
# GeoTIFFs to a temp directory.

# Polity 1, two cells. Crop 15 is on the crop pattern (30 ha in cell A, 10 ha
# in cell B); alfalfa (641) and crop 27 are not. The fodder layer puts alfalfa
# only in cell B, and also carries a crop-15 row it must NOT be allowed to use.
.fod_data <- function() {
  list(
    npp = tibble::tribble(
      ~area_code, ~item_prod_code, ~year,
      ~residue_c_t, ~residue_soil_c_t, ~root_c_t, ~weed_npp_c_t,
      1L, "15", 2020L, 100, 60, 40, 10,
      1L, "27", 2020L, 50, 30, 10, 5,
      1L, "641", 2020L, 80, 40, 20, 0
    ),
    manure = tibble::tribble(
      ~year, ~territory, ~sub_territory, ~land_use, ~crop, ~applied_c,
      2020L, "1", NA, "Cropland", "15", 20
    ),
    country_grid = tibble::tribble(
      ~lon, ~lat, ~area_code, ~cell_area_frac,
      0.25, 0.25, 1L, 1,
      0.75, 0.25, 1L, 1
    ),
    crop_patterns = tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~crop_area_ha,
      0.25, 0.25, "15", 30,
      0.75, 0.25, "15", 10
    ),
    fodder_patterns = tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~crop_area_ha,
      0.75, 0.25, "641", 8,
      0.75, 0.25, "15", 50
    ),
    harvested_area = tibble::tribble(
      ~area_code, ~item_prod_code, ~year, ~faostat_area_ha,
      1L, "15", 2020L, 40,
      1L, "27", 2020L, 20,
      1L, "641", 2020L, 16
    ),
    residue_humification = whep::residue_humification
  )
}

.fod_mass <- function(data) {
  sum(data$npp$residue_soil_c_t) +
    sum(data$npp$root_c_t) +
    sum(data$npp$weed_npp_c_t) +
    sum(data$manure$applied_c)
}

.fod_run <- function(method, data = .fod_data()) {
  suppressWarnings(suppressMessages(
    whep::build_soil_carbon_inputs(
      resolution = "grid",
      data = data,
      method_unspatialized = method
    )
  ))
}

.fod_cell_mass <- function(out, code) {
  out |>
    dplyr::filter(.data$item_prod_code == code) |>
    dplyr::mutate(c = .data$total_c_input_mgc_ha_yr * .data$crop_area_ha) |>
    dplyr::arrange(.data$lon) |>
    dplyr::pull("c")
}

testthat::test_that("fodder carbon goes on the fodder layer, not on cropland", {
  out <- .fod_run("fodder_pattern")
  # Alfalfa carries 40 + 20 = 60 Mg C, all of it in cell B, on its 16 ha.
  alfalfa <- dplyr::filter(out, .data$item_prod_code == "641")
  testthat::expect_equal(alfalfa$lon, 0.75)
  testthat::expect_equal(alfalfa$crop_area_ha, 16)
  testthat::expect_equal(.fod_cell_mass(out, "641"), 60)
  testthat::expect_true(all(out$method_unspatialized == "fodder_pattern"))
})

testthat::test_that("uniform reallocation is still selectable and differs", {
  out <- .fod_run("reallocate")
  # Uniform over cropland: 30/40 and 10/40 of the 60 Mg C.
  testthat::expect_equal(.fod_cell_mass(out, "641"), c(45, 15))
  testthat::expect_true(all(out$method_unspatialized == "reallocate"))
})

testthat::test_that("a non-fodder crop is still reallocated uniformly", {
  out <- .fod_run("fodder_pattern")
  # Crop 27 has no fodder row, so it falls back to the cropland rule:
  # 30 + 10 + 5 = 45 Mg C split 30/40 and 10/40.
  testthat::expect_equal(.fod_cell_mass(out, "27"), c(45 * 0.75, 45 * 0.25))
})

testthat::test_that("the fodder layer never overrides a crop-pattern crop", {
  out <- .fod_run("fodder_pattern")
  # Crop 15's own pattern (30/10) wins over the fodder layer's crop-15 row:
  # 60 + 40 + 10 + 20 = 130 Mg C split 3:1.
  testthat::expect_equal(.fod_cell_mass(out, "15"), c(97.5, 32.5))
})

testthat::test_that("fodder_pattern conserves every polity's carbon mass", {
  data <- .fod_data()
  for_method <- purrr::map_dbl(
    c("reallocate", "fodder_pattern"),
    \(m) {
      out <- .fod_run(m, data)
      sum(out$total_c_input_mgc_ha_yr * out$crop_area_ha)
    }
  )
  testthat::expect_equal(for_method, rep(.fod_mass(data), 2))
})

testthat::test_that("the carbon placed on the fodder layer is reported", {
  testthat::expect_message(
    suppressWarnings(whep::build_soil_carbon_inputs(
      resolution = "grid",
      data = .fod_data(),
      method_unspatialized = "fodder_pattern"
    )),
    "60 Mg C.*fodder layer"
  )
})

testthat::test_that("fodder_pattern refuses to run without the layer", {
  withr::local_envvar(WHEP_MONFREDA_DIR = "")
  data <- .fod_data()
  data$fodder_patterns <- NULL
  testthat::expect_error(
    whep::build_soil_carbon_inputs(
      data = data,
      method_unspatialized = "fodder_pattern"
    ),
    class = "whep_missing_monfreda"
  )
})

testthat::test_that("the other methods never read the fodder layer", {
  withr::local_envvar(WHEP_MONFREDA_DIR = "")
  data <- .fod_data()
  data$fodder_patterns <- NULL
  out <- .fod_run("reallocate", data)
  testthat::expect_true(all(out$method_unspatialized == "reallocate"))
})

testthat::test_that("a Monfreda folder without forage layers is refused", {
  dir <- withr::local_tempdir()
  testthat::expect_error(
    whep:::.sci_monfreda_geotiff_dir(dir),
    class = "whep_missing_monfreda"
  )
})

testthat::test_that("the layer crosswalk is the 16 forage items, one to one", {
  layers <- whep:::.fodder_earthstat_layers()
  testthat::expect_equal(nrow(layers), 16L)
  testthat::expect_false(anyDuplicated(layers$earthstat_name) > 0)
  testthat::expect_false(anyDuplicated(layers$item_prod_code) > 0)
  names <- whep::items_prod_full$item_prod[
    match(
      layers$item_prod_code,
      as.character(whep::items_prod_full$item_prod_code)
    )
  ]
  testthat::expect_false(anyNA(names))
  testthat::expect_true(all(stringr::str_detect(
    names,
    stringr::regex("forage|fodder|grasses and legumes", ignore_case = TRUE)
  )))
  # Every layer is a raster the EarthStat archive actually ships.
  path <- system.file("extdata", "earthstat_mapping.csv", package = "whep")
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "..",
      "..",
      "inst",
      "extdata",
      "earthstat_mapping.csv"
    )
  }
  shipped <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(.data$in_raster_archive) |>
    dplyr::pull("earthstat_name")
  testthat::expect_true(all(layers$earthstat_name %in% shipped))
})

testthat::test_that("the raster reader pools the 16 forage layers per cell", {
  testthat::skip_if_not_installed("terra")
  root <- withr::local_tempdir()
  geotiff <- file.path(root, "GeoTiff")
  layers <- whep:::.fodder_earthstat_layers()
  # A 1 x 0.5 degree strip at 0.25 degrees: two 0.5-degree cells after the
  # 2 x 2 aggregation. Layer i is i/1000 in the west cell and 0 in the east.
  purrr::walk(seq_len(nrow(layers)), \(i) {
    name <- layers$earthstat_name[i]
    dir.create(file.path(geotiff, name), recursive = TRUE)
    r <- terra::rast(
      nrows = 2,
      ncols = 4,
      xmin = 0,
      xmax = 1,
      ymin = 0,
      ymax = 0.5,
      crs = "EPSG:4326"
    )
    terra::values(r) <- rep(c(i / 1000, i / 1000, 0, 0), 2)
    terra::writeRaster(
      r,
      file.path(geotiff, name, paste0(name, "_HarvestedAreaFraction.tif"))
    )
  })
  out <- whep:::.sci_fodder_harvest_fraction(root)
  # Only the west cell has signal; every fodder code gets the pooled sum.
  testthat::expect_setequal(out$item_prod_code, layers$item_prod_code)
  testthat::expect_equal(unique(out$lon), 0.25)
  testthat::expect_equal(
    unique(round(out$harvest_fraction, 10)),
    round(sum(seq_len(16)) / 1000, 10)
  )
})
