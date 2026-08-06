# test_natural_earth.R — tests for R/natural_earth.R
#
# Nothing here reaches the network: the cache directory is redirected to a
# temporary one, and the failure path is driven by an unsupported URL scheme
# that libcurl rejects locally.

test_that(".natural_earth_url points at the official Natural Earth CDN", {
  expect_equal(
    whep:::.natural_earth_url("ne_10m_admin_1_states_provinces"),
    paste0(
      "https://naciscdn.org/naturalearth/10m/cultural/",
      "ne_10m_admin_1_states_provinces.zip"
    )
  )
})

test_that(".natural_earth_cache_dir sits inside the whep user cache", {
  dir <- whep:::.natural_earth_cache_dir()

  expect_equal(basename(dir), "naturalearth")
  # dirname() expands "~", so the expectation has to as well.
  expect_equal(
    dirname(dir),
    path.expand(rappdirs::user_cache_dir("whep"))
  )
})

# .provinces_shapefile --------------------------------------------------------

test_that(".provinces_shapefile returns an existing explicit path", {
  shp <- withr::local_tempfile(fileext = ".shp")
  file.create(shp)

  expect_equal(whep:::.provinces_shapefile(shp), shp)
})

test_that(".provinces_shapefile honours the session-wide option", {
  shp <- withr::local_tempfile(fileext = ".shp")
  file.create(shp)
  withr::local_options(whep.provinces_shapefile = shp)

  # No argument, so resolution falls through to the option before any
  # download is considered.
  expect_equal(whep:::.provinces_shapefile(), shp)
})

test_that(".provinces_shapefile aborts on a path that does not exist", {
  missing <- file.path(withr::local_tempdir(), "not_there.shp")

  expect_error(
    whep:::.provinces_shapefile(missing),
    "Provinces shapefile not found"
  )
})

test_that(".provinces_shapefile prefers the argument over the option", {
  option_shp <- withr::local_tempfile(fileext = ".shp")
  file.create(option_shp)
  withr::local_options(whep.provinces_shapefile = option_shp)
  missing <- file.path(withr::local_tempdir(), "not_there.shp")

  # The argument wins, so an unusable argument aborts even though the option
  # points at a readable file.
  expect_error(
    whep:::.provinces_shapefile(missing),
    "Provinces shapefile not found"
  )
})

# .download_natural_earth -----------------------------------------------------

test_that(".download_natural_earth reuses a cached layer without fetching", {
  cache <- withr::local_tempdir()
  local_mocked_bindings(
    .natural_earth_cache_dir = function() cache,
    .natural_earth_url = function(layer) {
      cli::cli_abort("The cached layer must be used without a download.")
    }
  )
  shp <- file.path(cache, "test_layer.shp")
  file.create(shp)

  expect_equal(whep:::.download_natural_earth("test_layer"), shp)
})

test_that(".download_natural_earth aborts with recovery instructions", {
  cache <- file.path(withr::local_tempdir(), "nested")
  local_mocked_bindings(
    .natural_earth_cache_dir = function() cache,
    # An unsupported scheme is rejected by libcurl locally, so the failure
    # branch is exercised without touching the network.
    .natural_earth_url = function(layer) "whepnoproto://test_layer.zip"
  )

  err <- expect_error(
    suppressWarnings(whep:::.download_natural_earth("test_layer")),
    "Could not download the Natural Earth layer"
  )
  # The instruction interpolates the layer URL. cli >= 3.4.0 reads a `{}`
  # expression starting with a dot as a style name, so interpolating
  # `.natural_earth_url(layer)` inline made this branch abort with
  # "Invalid cli literal" and swallowed the instructions.
  expect_match(conditionMessage(err), "whepnoproto", fixed = TRUE)
  expect_match(conditionMessage(err), "shapefile_path", fixed = TRUE)
  # The cache directory is created on the way, and the partial archive is
  # removed rather than left behind to be mistaken for a good download.
  expect_true(dir.exists(cache))
  expect_false(file.exists(file.path(cache, "test_layer.zip")))
})
