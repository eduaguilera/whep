testthat::test_that(".pins_has_expired returns TRUE for NULL", {
  testthat::expect_true(.pins_has_expired(NULL))
})

testthat::test_that(".pins_has_expired returns TRUE for past times", {
  past <- unclass(Sys.time()) - 1000
  testthat::expect_true(.pins_has_expired(past))
})

testthat::test_that(".pins_has_expired returns FALSE for future times", {
  future <- unclass(Sys.time()) + 100000
  testthat::expect_false(.pins_has_expired(future))
})

testthat::test_that(".pins_http_date returns NULL for NULL input", {
  testthat::expect_null(.pins_http_date(NULL))
})

testthat::test_that(".pins_http_date returns formatted date string", {
  t <- as.POSIXct("2024-01-15 10:30:00", tz = "UTC")
  result <- .pins_http_date(t)

  testthat::expect_type(result, "character")
  testthat::expect_true(
    stringr::str_detect(result, "Mon, 15 Jan 2024")
  )
  testthat::expect_true(
    stringr::str_detect(result, "UTC")
  )
})

testthat::test_that(".pins_to_utf8 handles character vectors", {
  result <- .pins_to_utf8(c("hello", "world"))
  testthat::expect_equal(result, c("hello", "world"))
})

testthat::test_that(".pins_to_utf8 handles nested lists", {
  input <- list(a = "text", b = list(c = "nested"))
  result <- .pins_to_utf8(input)

  testthat::expect_equal(result$a, "text")
  testthat::expect_equal(result$b$c, "nested")
})

testthat::test_that(".pins_to_utf8 returns non-char non-list as-is", {
  testthat::expect_equal(.pins_to_utf8(42), 42)
  testthat::expect_equal(.pins_to_utf8(TRUE), TRUE)
})

testthat::test_that(".pins_read_cache returns empty list for missing file", {
  result <- .pins_read_cache(
    tempfile(fileext = ".yaml")
  )
  testthat::expect_equal(result, list())
})

testthat::test_that(".pins_read_cache reads existing yaml cache", {
  path <- tempfile(fileext = ".yaml")
  yaml::write_yaml(
    list("https://example.com" = list(etag = "abc")),
    path
  )
  result <- .pins_read_cache(path)

  testthat::expect_true(
    "https://example.com" %in% names(result)
  )
  testthat::expect_equal(
    result[["https://example.com"]]$etag,
    "abc"
  )
})

testthat::test_that(".pins_update_cache writes and returns value", {
  path <- tempfile(fileext = ".yaml")
  value <- list(expires = 999, etag = "test")
  result <- .pins_update_cache(
    path,
    "my_key",
    value
  )

  testthat::expect_equal(result, value)
  cache <- .pins_read_cache(path)
  testthat::expect_equal(cache$my_key$etag, "test")
})

testthat::test_that(".pins_show_progress returns FALSE in non-interactive", {
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )
  testthat::expect_false(.pins_show_progress())
  testthat::expect_false(.pins_show_progress(size = 10^8))
})
