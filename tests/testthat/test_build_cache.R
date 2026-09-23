# test_build_cache.R — tests for R/build_cache.R

# The cache is session-level, so anything already in it is put back afterwards
# rather than being wiped out from under the rest of the suite.
local_isolated_build_cache <- function(envir = parent.frame()) {
  cache <- whep:::.build_cache
  saved <- as.list(cache, all.names = TRUE)
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  withr::defer(
    {
      rm(list = ls(cache, all.names = TRUE), envir = cache)
      list2env(saved, envir = cache)
    },
    envir = envir
  )
  cache
}

test_that(".cache_get evaluates the expression only on a miss", {
  local_isolated_build_cache()

  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  build <- function() {
    counter$n <- counter$n + 1L
    tibble::tibble(value = 1)
  }

  first <- whep:::.cache_get("coverage_probe", build())
  second <- suppressMessages(whep:::.cache_get("coverage_probe", build()))

  expect_equal(first, tibble::tibble(value = 1))
  expect_equal(second, first)
  # `expr` is a promise: on a hit it is returned from before it is forced, so
  # the expensive build never runs a second time.
  expect_equal(counter$n, 1L)
})

test_that(".cache_get says when it is serving a cached value", {
  local_isolated_build_cache()

  whep:::.cache_get("coverage_probe", 42)

  expect_message(
    whep:::.cache_get("coverage_probe", 42),
    "Using cached coverage_probe"
  )
})

test_that(".cache_get caches a NULL result instead of recomputing it", {
  local_isolated_build_cache()

  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  build <- function() {
    counter$n <- counter$n + 1L
    NULL
  }

  expect_null(whep:::.cache_get("coverage_probe", build()))
  expect_message(
    expect_null(whep:::.cache_get("coverage_probe", build())),
    "Using cached coverage_probe"
  )
  # A NULL payload is a real cache hit, not an empty slot (whep#172): the
  # expensive build must not run a second time.
  expect_equal(counter$n, 1L)
})

test_that("whep_clear_cache empties the cache and returns NULL invisibly", {
  cache <- local_isolated_build_cache()

  whep:::.cache_get("coverage_probe", 42)
  expect_equal(ls(cache), "coverage_probe")

  expect_message(cleared <- whep::whep_clear_cache(), "Build cache cleared")

  expect_equal(ls(cache), character(0))
  expect_null(cleared)
  # A rebuild is now a miss again.
  expect_equal(whep:::.cache_get("coverage_probe", 7), 7)
})

test_that("whep_clear_cache is a no-op on an already empty cache", {
  local_isolated_build_cache()

  expect_message(whep::whep_clear_cache(), "Build cache cleared")
  expect_equal(ls(whep:::.build_cache), character(0))
})

test_that(".cache_key gives each CBS build method its own slot", {
  none <- whep:::.cbs_cache_method("none")
  recovered <- whep:::.cbs_cache_method("net_import")

  # The default method adds no suffix, so a default build keeps the slots it
  # has always used and nothing that pre-dates method selection moves.
  expect_null(none)
  expect_equal(
    whep:::.cache_key("cbs_built", 2010, none),
    whep:::.cache_key("cbs_built", 2010)
  )
  # Any other method needs its own slot: serving one method's CBS to a caller
  # that asked for the other is silent and unrecoverable (whep#762).
  expect_false(
    whep:::.cache_key("cbs_built", 2010, recovered) ==
      whep:::.cache_key("cbs_built", 2010)
  )
  expect_equal(
    whep:::.cache_key("cbs_built", NULL, recovered),
    "cbs_built__net_import"
  )
  # The window still qualifies the slot on top of the method (whep#243).
  expect_equal(
    whep:::.cache_key("cbs_built", c(2001, 1999), recovered),
    "cbs_built__net_import__1999__2001"
  )
})

test_that(".cached_cbs_built keeps the two trade_recovery builds apart", {
  local_isolated_build_cache()

  seen <- character()
  local_mocked_bindings(
    .cached_primary_prod = function(years) tibble::tibble(year = 2010),
    build_commodity_balances = function(
      primary_all,
      trade_recovery = "none"
    ) {
      seen <<- c(seen, trade_recovery)
      tibble::tibble(year = 2010L, trade_recovery = trade_recovery)
    },
    .package = "whep"
  )

  first <- suppressMessages(whep:::.cached_cbs_built(2010, "none"))
  second <- suppressMessages(whep:::.cached_cbs_built(2010, "net_import"))
  again <- suppressMessages(whep:::.cached_cbs_built(2010, "none"))

  # Two builds ran, not one served twice; the third is the cached default.
  expect_equal(seen, c("none", "net_import"))
  expect_equal(first$trade_recovery, "none")
  expect_equal(second$trade_recovery, "net_import")
  expect_equal(again$trade_recovery, "none")
})

# whep#833. Two fills inside `.fix_cbs()` carry one observation across the
# whole year axis and decide whether a processing output exists at all, so a
# CBS built over a window loses what the full-range build carries in from
# outside it (and invents what the full-range build does not). The stand-in
# below is that mechanism and nothing else: the only anchor sits at 1961, and
# `fill_linear()` carries it to every year the frame covers.
.fake_anchored_cbs <- function(start_year = 1850, end_year = 2023) {
  tibble::tibble(
    year = start_year:end_year,
    area_code = 106L,
    item_cbs_code = 2581L,
    value = dplyr::if_else(year == 1961L, 4339.65, NA_real_)
  ) |>
    whep::fill_linear(value, time_col = year) |>
    dplyr::filter(!is.na(value))
}

local_fake_cbs_chain <- function(envir = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$windows <- list()
  testthat::local_mocked_bindings(
    .cached_primary_prod = function(years) tibble::tibble(year = 2010L),
    build_commodity_balances = function(
      primary_all,
      start_year = 1850,
      end_year = 2023,
      trade_recovery = "none"
    ) {
      seen$windows <- c(seen$windows, list(c(start_year, end_year)))
      .fake_anchored_cbs(start_year, end_year)
    },
    .package = "whep",
    .env = envir
  )
  seen
}

test_that("the stand-in CBS really is window-dependent (whep#833)", {
  # Guards the fixture: if the stand-in stopped depending on its window, the
  # test below would pass on the old wiring too and prove nothing.
  full <- .fake_anchored_cbs() |> dplyr::filter(year == 2010L)
  scoped <- .fake_anchored_cbs(2005, 2015) |> dplyr::filter(year == 2010L)

  expect_equal(nrow(full), 1L)
  expect_equal(nrow(scoped), 0L)
})

test_that("a scoped CBS is the full-range CBS filtered (whep#833)", {
  local_isolated_build_cache()
  seen <- local_fake_cbs_chain()

  full <- suppressMessages(whep:::.cached_cbs_built(NULL))
  scoped <- suppressMessages(whep:::.cached_cbs_built(2010L))
  wider <- suppressMessages(whep:::.cached_cbs_built(2005:2015))

  # The identity the year window promises, exactly: the scoped build keeps
  # the key the full-range build carries in from its 1961 anchor.
  expect_equal(scoped, dplyr::filter(full, year == 2010L))
  expect_equal(wider, dplyr::filter(full, year %in% 2005:2015))
  expect_s3_class(scoped, "tbl_df")
  expect_equal(dplyr::pull(scoped, value), 4339.65)
  # Each window is cut from the one full-range build, never a build of its
  # own, so a narrow window can never see a narrower axis than the full one.
  expect_equal(seen$windows, list(c(1850, 2023)))
})

test_that("a scoped CBS reads the full-range primary production", {
  local_isolated_build_cache()
  asked <- list()
  testthat::local_mocked_bindings(
    .cached_primary_prod = function(years) {
      asked <<- c(asked, list(years))
      tibble::tibble(year = 2010L)
    },
    build_commodity_balances = function(primary_all, ...) {
      .fake_anchored_cbs()
    },
    .package = "whep"
  )

  suppressMessages(whep:::.cached_cbs_built(2010L))

  # The CBS reads production across every year it fills over, so a scoped
  # request must hand it the full-range production, not the window's.
  expect_equal(asked, list(NULL))
})
