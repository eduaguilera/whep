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
    .build_cbs_years = function(
      primary_prod,
      years,
      context_years = years,
      trade_recovery = "none"
    ) {
      seen <<- c(seen, trade_recovery)
      tibble::tibble(trade_recovery = trade_recovery)
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
