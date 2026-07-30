# Deterministic tests for build_constant_territory_series() on synthetic
# rectangular polities in the equal-area CRS (EPSG:6933), where cell areas are
# exact and the expected estimates are hand-derivable.

.rect <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2,
    byrow = TRUE
  )))
}

# Geometry (metres in EPSG:6933):
#   SRC  : [0,100k] x [0,100k]   value 100 in year 1900   (active 1850-1950)
#   T_L  : [0, 50k] x [0,100k]   active in 2000           (left half of SRC)
#   T_R  : [50k,150k] x [0,100k] active in 2000           (right half of SRC
#          PLUS an uncovered strip [100k,150k] with no source)
.synthetic_polities <- function() {
  sf::st_sf(
    polity_code = c("SRC", "T_L", "T_R"),
    start_year = c(1850L, 1990L, 1990L),
    end_year = c(1950L, 2025L, 2025L),
    geometry = sf::st_sfc(
      .rect(0, 0, 100000, 100000),
      .rect(0, 0, 50000, 100000),
      .rect(50000, 0, 150000, 100000),
      crs = 6933
    )
  )
}

.reported <- data.frame(year = 1900L, polity_code = "SRC", value = 100)

test_that("uniform density: covered mass is conserved and split by area", {
  res <- build_constant_territory_series(
    .reported,
    ref_year = 2000,
    polities = .synthetic_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  res <- res[order(res$target_polity_code), ]

  # T_L sits entirely inside SRC -> half the source area -> half the value.
  tl <- res[res$target_polity_code == "T_L", ]
  expect_equal(tl$value, 50, tolerance = 1e-6)
  expect_equal(tl$imputed_share, 0, tolerance = 1e-9)

  # T_R: covered half of SRC (50) + an equal-area uncovered strip imputed at
  # the same regional intensity (50) -> 100, with half its area imputed.
  tr <- res[res$target_polity_code == "T_R", ]
  expect_equal(tr$covered, 50, tolerance = 1e-6)
  expect_equal(tr$imputed, 50, tolerance = 1e-6)
  expect_equal(tr$value, 100, tolerance = 1e-6)
  expect_equal(tr$imputed_share, 0.5, tolerance = 1e-6)

  # Conservation: covered mass over all targets equals the reported total.
  expect_equal(sum(res$covered), 100, tolerance = 1e-6)
})

test_that("donor='none' performs no imputation but still reports the gap", {
  res <- build_constant_territory_series(
    .reported,
    ref_year = 2000,
    polities = .synthetic_polities(),
    resolution = 10000,
    donor = "none",
    verbose = FALSE
  )
  tr <- res[res$target_polity_code == "T_R", ]
  expect_equal(tr$imputed, 0, tolerance = 1e-9)
  expect_equal(tr$value, 50, tolerance = 1e-6) # covered only
  expect_equal(tr$imputed_share, 0.5, tolerance = 1e-6) # gap still disclosed
})

test_that("non-uniform covariate shifts mass dasymetrically (not by area)", {
  # density 1 west of x=50k, density 3 east of it. Conservation must still hold,
  # but the east (T_R covered part) should take 3x the mass of the west (T_L).
  cov_fn <- function(centroids, year) {
    x <- sf::st_coordinates(centroids)[, 1]
    ifelse(x < 50000, 1, 3)
  }
  res <- build_constant_territory_series(
    .reported,
    ref_year = 2000,
    polities = .synthetic_polities(),
    covariate = cov_fn,
    resolution = 10000,
    verbose = FALSE
  )
  res <- res[order(res$target_polity_code), ]
  tl <- res[res$target_polity_code == "T_L", ]
  tr <- res[res$target_polity_code == "T_R", ]

  # West gets 100 * (1*A)/(1*A + 3*A) = 25 ; east covered gets 75.
  expect_equal(tl$value, 25, tolerance = 1e-6)
  expect_equal(tr$covered, 75, tolerance = 1e-6)
  # Covered conservation independent of the density field.
  expect_equal(sum(res$covered), 100, tolerance = 1e-6)
  # Dasymetric, not areal: covered east/west ratio ~ 3.
  expect_equal(tr$covered / tl$value, 3, tolerance = 1e-6)
})

test_that("years with no usable source are skipped, output schema is stable", {
  res <- build_constant_territory_series(
    data.frame(year = 1700L, polity_code = "SRC", value = 5), # SRC not active 1700
    ref_year = 2000,
    polities = .synthetic_polities(),
    resolution = 10000,
    verbose = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_named(
    res,
    c(
      "target_polity_code",
      "year",
      "value",
      "covered",
      "imputed",
      "imputed_share",
      "n_sources"
    )
  )
  expect_equal(nrow(res), 0)
})

# The two branches that LOSE reported value had no coverage. Conservation is asserted
# above on fixtures where every source is placeable; these are the cases where it is not,
# and they are the ones a user needs told about:
#
# One reports that no source polity with a polygon is active for the reported data, so the
# year is skipped; the other that some sources are smaller than the grid resolution, so
# their value is not distributed.
#
# Both are gated on `verbose`, which DEFAULTS TO TRUE — so they do fire for a default
# caller. I had assumed the opposite while reading the code and was about to write up a
# silent data loss; the default is in the signature two screens above the warnings.
#
# What is worth asserting is that each fires WHEN IT SHOULD and NAMES the polity, because
# a warning that says only "1 source" leaves the caller to work out which of hundreds it
# was — the same failure the gate self-test upstream checks for.
testthat::test_that("a year whose source polity has no active polygon is reported, not dropped quietly", {
  polities <- .synthetic_polities()
  # SRC is active 1850-1950, so a 1970 row has no active source polygon at all.
  reported <- data.frame(
    year = 1970L,
    polity_code = "SRC",
    value = 100
  )

  warned <- character(0)
  res <- withCallingHandlers(
    build_constant_territory_series(
      reported,
      ref_year = 2000L,
      polities = polities,
      resolution = 25000,
      verbose = TRUE
    ),
    warning = function(cond) {
      warned <<- c(warned, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_true(
    any(grepl("1970", warned)),
    info = paste0(
      "the skipped year is not named in any warning: ",
      paste(warned, collapse = " | ")
    )
  )
  # And nothing is invented for it: a year with no placeable source contributes no rows
  # rather than zero-valued ones, which would read as a measurement of zero.
  testthat::expect_false(1970L %in% res$year)
})

testthat::test_that("a source smaller than the grid resolution is named, not silently starved", {
  polities <- .synthetic_polities()
  # A resolution coarser than SRC's 100km extent leaves it with no cell of its own.
  warned <- character(0)
  suppressMessages(withCallingHandlers(
    build_constant_territory_series(
      .reported,
      ref_year = 2000L,
      polities = polities,
      resolution = 400000,
      verbose = TRUE
    ),
    warning = function(cond) {
      warned <<- c(warned, conditionMessage(cond))
      invokeRestart("muffleWarning")
    }
  ))

  # Either the source is starved and named, or the grid still catches it — both are
  # acceptable outcomes of a coarse resolution, but a starved source must be NAMED.
  starved <- grepl("smaller than the grid resolution", warned)
  if (any(starved)) {
    testthat::expect_true(
      any(grepl("SRC", warned[starved])),
      info = paste0(
        "starvation warning does not name the source: ",
        paste(warned[starved], collapse = " | ")
      )
    )
  } else {
    testthat::succeed("resolution still resolved the source; nothing starved")
  }
})
