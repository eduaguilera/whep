# Tests for the polycell input layers (plan
# `plans/2026-08-03-polycell-spatial-support.md`, DA-6, DA-9, DA-17).
#
# The real layers are multi-hundred-megabyte local files behind environment
# variables, so what is exercised here is the reader contract: the GLWD
# class-to-fraction derivation, the s2 repair the ice layer needs, and the path
# resolution that must abort with an instruction rather than a hardcoded
# machine path.

# A tiny GLWD-shaped class raster: 4x4 source pixels aggregating 2:1 onto a
# 2x2 grid, so every aggregated cell averages exactly four source pixels and
# the expected fraction is countable by hand rather than asserted from a run.
#
# The temporary directory is created HERE against the CALLER's frame. Passing
# `withr::local_tempdir()` in as an argument instead ties its lifetime to this
# function, so it is deleted the moment the fixture is written and every test
# then reports a missing raster.
pcl_write_glwd <- function(classes, area_pct = NULL) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  v2 <- file.path(dir, "GLWD", "GLWD_v2", "GLWD_v2_0_combined_classes")
  dir.create(v2, recursive = TRUE, showWarnings = FALSE)
  # 0.25-degree source pixels, so the 0.5-degree aggregation factor is 2 and
  # each output cell averages exactly four of them. An extent that made the
  # source already 0.5 degrees would give a factor of 1 and silently test no
  # aggregation at all.
  r <- terra::rast(
    nrows = 4L,
    ncols = 4L,
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    crs = "EPSG:4326",
    vals = classes
  )
  terra::writeRaster(r, file.path(v2, "GLWD_v2_0_main_class.tif"))
  if (!is.null(area_pct)) {
    terra::writeRaster(
      terra::setValues(r, area_pct),
      file.path(v2, "GLWD_v2_0_area_pct.tif")
    )
  }
  dir
}

testthat::test_that("only lake and river classes count as inland water", {
  skip_if_not_installed("terra")
  # Row 1: classes 1, 2, 3, 7 -- all water. Row 2: 17, 25, 28, 33 -- palustrine
  # wetland, peatland, mangrove and rice paddies, none of them open water.
  # Rows 3-4: dryland. Each output cell averages a water pixel pair and a
  # non-water pair, so the top two read 0.5 and the bottom two 0.
  dir <- pcl_write_glwd(
    classes = c(1L, 2L, 3L, 7L, 17L, 25L, 28L, 33L, rep(0L, 8L))
  )

  water <- whep::glwd_water_fraction(file.path(dir, "GLWD"))

  testthat::expect_equal(attr(water, "glwd_version"), "v2")
  testthat::expect_setequal(water$water_frac, c(0.5, 0.5, 0, 0))
})

testthat::test_that("a wetland-only cell carries no inland water at all", {
  skip_if_not_installed("terra")
  # The distinction the class list exists to make: these are land that is wet,
  # and `build_polycell_support()` books them under `land_area_ha`. A reader
  # that took "any GLWD class" would return 1 here.
  dir <- pcl_write_glwd(
    classes = rep(c(17L, 25L, 28L, 33L), 4L)
  )

  water <- whep::glwd_water_fraction(file.path(dir, "GLWD"))

  testthat::expect_true(all(water$water_frac == 0))
})

testthat::test_that("area_pct weights a partially covered source pixel", {
  skip_if_not_installed("terra")
  # Every source pixel is a lake, but each covers only half its pixel, so the
  # aggregated fraction is 0.5 rather than 1. Without the weighting this reads
  # 1 and every partially wet cell is over-counted.
  dir <- pcl_write_glwd(
    classes = rep(1L, 16L),
    area_pct = rep(50, 16L)
  )

  water <- whep::glwd_water_fraction(file.path(dir, "GLWD"))

  testthat::expect_true(all(abs(water$water_frac - 0.5) < 1e-9))
})

testthat::test_that("an uncovered cell is dry rather than NA", {
  skip_if_not_installed("terra")
  # `NA` here would propagate into `land_area_ha` and delete the cell's land,
  # so a cell the raster says nothing about has to read as 0.
  dir <- pcl_write_glwd(
    classes = c(rep(1L, 4L), rep(NA_integer_, 12L))
  )

  water <- whep::glwd_water_fraction(file.path(dir, "GLWD"))

  testthat::expect_false(anyNA(water$water_frac))
  testthat::expect_true(all(water$water_frac >= 0 & water$water_frac <= 1))
})

testthat::test_that("no-data divides by the whole cell, not by its land", {
  skip_if_not_installed("terra")
  # A coastal cell: two lake pixels and two the raster has no data for. The
  # water covers half the CELL, and `water_frac` is a fraction of the whole
  # cell, so the answer is 0.5.
  #
  # The no-data has to arrive through `area_pct`, which is where it arrives in
  # the real layer: `terra::classify(others = 0)` already folds an NA CLASS to
  # zero, so a fixture that only blanks the class raster passes under either
  # aggregation and tests nothing. Blanking `area_pct` is what reintroduces NA
  # into the weighted mask.
  #
  # Averaging over the non-NA pixels alone returns 1.0 -- the cell reads as
  # entirely water because the ocean half was dropped from the denominator
  # rather than counted as dry. That is the +6.4% coastal inflation this
  # aggregation carried while it lived in `prepare_spatialize_all.R`.
  # The water and the no-data must fall in the SAME aggregation block to bite:
  # the top-left 2x2 is rows 1-2 of columns 1-2, so it holds two lake pixels
  # over two whose `area_pct` is absent.
  dir <- pcl_write_glwd(
    classes = c(
      1L,
      1L,
      0L,
      0L,
      1L,
      1L,
      0L,
      0L,
      rep(0L, 8L)
    ),
    area_pct = c(
      100,
      100,
      100,
      100,
      NA,
      NA,
      100,
      100,
      rep(100, 8L)
    )
  )

  water <- whep::glwd_water_fraction(file.path(dir, "GLWD"))

  testthat::expect_equal(max(water$water_frac), 0.5)
})

testthat::test_that("sampling at given cells returns those cells", {
  skip_if_not_installed("terra")
  dir <- pcl_write_glwd(classes = rep(1L, 16L))
  cells <- tibble::tibble(lon = c(0.5, 1.5), lat = c(1.5, 0.5))

  water <- whep::glwd_water_fraction(file.path(dir, "GLWD"), cells = cells)

  testthat::expect_equal(water$lon, cells$lon)
  testthat::expect_equal(water$lat, cells$lat)
})

testthat::test_that("a directory with no GLWD raster says how to fetch one", {
  testthat::expect_error(
    whep::glwd_water_fraction(withr::local_tempdir()),
    "download_hydrology"
  )
})

testthat::test_that("layer readers name the environment variable they need", {
  withr::local_envvar(
    WHEP_LPJML_INPUT_DIR = "",
    WHEP_NATURALEARTH_DIR = "",
    WHEP_LUH2_DIR = ""
  )

  testthat::expect_error(
    whep::read_glwd_water(),
    "WHEP_LPJML_INPUT_DIR"
  )
  testthat::expect_error(
    whep:::.whep_layer_dir(NULL, "WHEP_MISSING_DIR", "a layer"),
    "WHEP_MISSING_DIR"
  )
})

testthat::test_that("s2-invalid polygons are repaired, classified and kept", {
  testthat::skip_if_not_installed("sf")

  # A bow-tie ring is invalid under both engines; a ring carrying a repeated
  # vertex is what `sf::st_intersection()` itself emits on the antimeridian,
  # and s2 then refuses to read its own output back. Both must be classified,
  # not crashed on.
  bowtie <- sf::st_polygon(list(cbind(
    c(0, 1, 0, 1, 0),
    c(0, 1, 1, 0, 0)
  )))
  duplicated_vertex <- sf::st_polygon(list(cbind(
    c(10, 10.5, 10.5, 10.5, 10, 10),
    c(45, 45, 45, 45.5, 45.5, 45)
  )))
  clean <- sf::st_polygon(list(cbind(
    c(20, 20.5, 20.5, 20, 20),
    c(45, 45, 45.5, 45.5, 45)
  )))

  fixed <- whep:::.s2_repair(
    sf::st_sfc(bowtie, duplicated_vertex, clean, crs = 4326)
  )

  testthat::expect_equal(fixed$status[[3L]], "ok")
  testthat::expect_true(all(fixed$status %in% c("ok", "repaired")))
  # Every repaired geometry is now readable by the spherical engine, and the
  # clean one is untouched.
  testthat::expect_equal(
    as.numeric(sf::st_area(fixed$geom[[3L]] |> sf::st_sfc(crs = 4326))),
    as.numeric(sf::st_area(sf::st_sfc(clean, crs = 4326)))
  )
  testthat::expect_true(all(is.finite(as.numeric(sf::st_area(fixed$geom)))))
})

testthat::test_that("read_polycell_support prefers a local parquet", {
  testthat::skip_if_not_installed("sf")

  dir <- withr::local_tempdir()
  path <- file.path(dir, "support.parquet")
  support <- whep::build_polycell_support(
    years = 2015L,
    geometries = whep::polycell_example_geometries()
  )
  nanoparquet::write_parquet(support, path)
  withr::local_envvar(WHEP_POLYCELL_SUPPORT_PATH = path)

  testthat::expect_equal(
    whep::read_polycell_support()$polycell_id,
    support$polycell_id
  )
  testthat::expect_error(
    whep::read_polycell_support(path = file.path(dir, "absent.parquet")),
    "not found"
  )
})

# whep#803 — the reader is the consumer-side half of the overlap contract ------

testthat::test_that("the reader returns the partition unless asked", {
  testthat::skip_if_not_installed("sf")

  # A support table may carry aggregate polities, whose polygons cover their
  # members'. Every consumer of this table sums hectares over it, so the
  # DEFAULT decides whether admitting an aggregate upstream is a new capability
  # or a silent double count in a caller that never asked for one.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "support.parquet")
  support <- tibble::tribble(
    ~polycell_id, ~polity_code, ~polity_area_ha, ~support_role,
    "WES@1", "WES-2000-2020", 100, "partition",
    "EAS@1", "EAS-2000-2020", 150, "partition",
    "AGG@1", "AGG-2000-2020", 250, "overlap"
  )
  nanoparquet::write_parquet(support, path)

  testthat::expect_equal(
    whep::read_polycell_support(path = path)$polycell_id,
    c("WES@1", "EAS@1")
  )
  testthat::expect_equal(
    whep::read_polycell_support(path = path, role = "overlap")$polycell_id,
    "AGG@1"
  )
  testthat::expect_equal(
    nrow(whep::read_polycell_support(path = path, role = "all")),
    3L
  )
  # Summing the partition is the cell's territory once; summing everything
  # counts the members twice, which is what the default exists to prevent.
  testthat::expect_equal(
    sum(whep::read_polycell_support(path = path)$polity_area_ha),
    250
  )
})

testthat::test_that("a support with no role column is all partition", {
  testthat::skip_if_not_installed("sf")

  # Every polycell published before whep#803 has no `support_role` column, and
  # every row of it partitions its cell. It must answer "partition" with all of
  # itself rather than with nothing -- and it must ABORT on "overlap" rather
  # than hand back zero rows, because a consumer asking for a reporting
  # bucket's territory and silently receiving none reports the bucket as having
  # no land, which is the failure whep#803 exists to fix.
  dir <- withr::local_tempdir()
  path <- file.path(dir, "legacy.parquet")
  nanoparquet::write_parquet(
    tibble::tibble(
      polycell_id = c("WES@1", "EAS@1"),
      polity_area_ha = c(100, 150)
    ),
    path
  )

  testthat::expect_equal(
    nrow(whep::read_polycell_support(path = path)),
    2L
  )
  testthat::expect_equal(
    nrow(whep::read_polycell_support(path = path, role = "all")),
    2L
  )
  testthat::expect_error(
    whep::read_polycell_support(path = path, role = "overlap"),
    class = "whep_polycell_no_overlap_layer"
  )
  testthat::expect_error(
    whep::read_polycell_support(path = path, role = "nonsense")
  )
})

testthat::test_that("sampled centres land on WHEP's canonical half-degree grid", {
  skip_if_not_installed("terra")
  # `terra::xyFromCell()` walks out from the raster origin and accumulates float
  # error, so it returns -130.25 as -130.24999999999994. The polycells form
  # their centres as `k * 0.5 + 0.25` exactly and the two are joined on
  # `c("lon", "lat")`, so a drift far below printing precision is enough to miss
  # every cell -- which it did: 36 of 720 longitudes matched and the build
  # reported 0.00 Mha of inland water worldwide.
  dir <- pcl_write_glwd(classes = rep(1L, 16L))

  water <- whep::glwd_water_fraction(file.path(dir, "GLWD"))

  canonical <- floor(water$lon / 0.5) * 0.5 + 0.25
  testthat::expect_identical(water$lon, canonical)
  testthat::expect_identical(water$lat, floor(water$lat / 0.5) * 0.5 + 0.25)
})
