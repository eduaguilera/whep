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
