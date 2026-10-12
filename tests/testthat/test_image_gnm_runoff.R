# read_image_gnm_runoff() reads the IMAGE-GNM supplement's two 0.5-degree
# layers. The fixture under fixtures/image_gnm/ keeps the supplement's layout
# and file formats (fQsro.map with a NODATA line, slope.map without one) on a
# 4 x 2 grid at 0E-2E, 50N-51N, so no test touches the network or a WHEP_*
# path.

.gnm_fixture_dir <- function() {
  testthat::test_path("fixtures", "image_gnm")
}

testthat::test_that("read_image_gnm_runoff reads both layers on the land cells", {
  out <- whep::read_image_gnm_runoff(
    dir = .gnm_fixture_dir(),
    verify = FALSE
  )
  expected <- tibble::tribble(
    ~lon, ~lat,  ~surface_runoff_fraction, ~slope_m_per_km,
    0.25, 50.75, 0.1,                      1,
    0.75, 50.75, 0.2,                      25,
    1.75, 50.75, 0.4,                      400,
    0.25, 50.25, 0,                        0,
    0.75, 50.25, 0.5,                      12,
    1.25, 50.25, 0.6,                      0
  )
  testthat::expect_equal(
    dplyr::select(
      out,
      "lon",
      "lat",
      "surface_runoff_fraction",
      "slope_m_per_km"
    ),
    expected
  )
  # The NODATA cells of fQsro.map are off the land mask: their slopes (3 and
  # 7 m/km) are not read as land.
  testthat::expect_false(any(out$slope_m_per_km %in% c(3, 7)))
  testthat::expect_identical(
    unique(out$source_image_gnm),
    "doi:10.5194/gmd-8-4045-2015-supplement"
  )
})

testthat::test_that("read_image_gnm_runoff refuses a layer that fails its checksum", {
  testthat::expect_error(
    whep::read_image_gnm_runoff(dir = .gnm_fixture_dir()),
    class = "whep_image_gnm_source"
  )
})

testthat::test_that("read_image_gnm_runoff aborts on a missing layer", {
  empty <- withr::local_tempdir()
  testthat::expect_error(
    whep::read_image_gnm_runoff(dir = empty, verify = FALSE),
    class = c("whep_image_gnm_source")
  )
})

testthat::test_that("read_image_gnm_runoff takes WHEP_IMAGE_GNM_DIR", {
  withr::local_envvar(WHEP_IMAGE_GNM_DIR = .gnm_fixture_dir())
  out <- whep::read_image_gnm_runoff(verify = FALSE)
  testthat::expect_equal(nrow(out), 6L)
})

testthat::test_that("the supplement cache downloads once and verifies", {
  cache <- withr::local_tempdir()
  calls <- 0L
  fake_download <- function(dir) {
    calls <<- calls + 1L
    file.path(dir, "supplement.zip")
  }
  fake_extract <- function(archive, dir) {
    fixture <- file.path(.gnm_fixture_dir(), "GMD_beusen_supplement")
    file.copy(fixture, dir, recursive = TRUE)
    invisible(dir)
  }
  first <- whep:::.gnm_cached_dir(cache, fake_download, fake_extract)
  testthat::expect_identical(first, cache)
  testthat::expect_identical(calls, 1L)
  # A second call finds the layers and does not download again.
  whep:::.gnm_cached_dir(cache, fake_download, fake_extract)
  testthat::expect_identical(calls, 1L)
  # An archive that does not unpack the layers aborts.
  other <- withr::local_tempdir()
  testthat::expect_error(
    whep:::.gnm_cached_dir(other, fake_download, \(archive, dir) dir),
    class = "whep_image_gnm_source"
  )
})

testthat::test_that("a downloaded supplement that fails its checksum aborts", {
  cache <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .gnm_fetch = function(url, path) {
      writeLines("not the supplement", path)
      invisible(path)
    }
  )
  testthat::expect_error(
    suppressMessages(whep:::.gnm_download(cache)),
    class = "whep_image_gnm_source"
  )
})

testthat::test_that("read_image_gnm_runoff example fixture is schema-complete", {
  out <- whep::read_image_gnm_runoff(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "lon",
      "lat",
      "surface_runoff_fraction",
      "slope_m_per_km",
      "source_image_gnm"
    )
  )
  pointblank::expect_col_vals_between(out, "surface_runoff_fraction", 0, 1)
  pointblank::expect_col_vals_gte(out, "slope_m_per_km", 0)
})
