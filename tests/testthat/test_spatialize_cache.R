.fake_production <- function(years = 2000:2001, area_codes = c(206L, 208L)) {
  tidyr::expand_grid(year = years, area_code = area_codes) |>
    dplyr::mutate(item_prod_code = 15L, value = 1)
}

test_that("dataset discovery finds the package tables", {
  names <- whep:::.whep_dataset_names()
  expect_gt(length(names), 40)
  expect_true("regions_full" %in% names)
  expect_false(any(stringr::str_detect(names, " ")))
})

test_that("the fingerprint hashes every dataset object once", {
  digests <- whep:::.whep_data_digests(c("regions_full", "items_cbs"))
  expect_named(digests, c("items_cbs", "regions_full"))
  expect_identical(
    whep:::.prod_cache_fingerprint(digests),
    whep:::.prod_cache_fingerprint(rev(digests))
  )
})

test_that("the fingerprint moves when any dataset changes", {
  digests <- c(regions_full = "aaa", items_cbs = "bbb")
  moved <- c(regions_full = "ccc", items_cbs = "bbb")
  expect_false(identical(
    whep:::.prod_cache_fingerprint(digests),
    whep:::.prod_cache_fingerprint(moved)
  ))
})

test_that("the real fingerprint is a stable non-empty hash", {
  fingerprint <- whep:::.prod_cache_fingerprint()
  expect_type(fingerprint, "character")
  expect_gt(nchar(fingerprint), 16)
  expect_identical(fingerprint, whep:::.prod_cache_fingerprint())
})

test_that("the table digest tracks columns and the area_code domain", {
  base <- .fake_production()
  expect_identical(
    whep:::.prod_cache_table_digest(base),
    whep:::.prod_cache_table_digest(dplyr::arrange(base, dplyr::desc(year)))
  )
  # The whep#628 case: pre-polity codes 276/277 instead of 206.
  old_model <- .fake_production(area_codes = c(276L, 277L))
  expect_false(identical(
    whep:::.prod_cache_table_digest(base),
    whep:::.prod_cache_table_digest(old_model)
  ))
  expect_false(identical(
    whep:::.prod_cache_table_digest(base),
    whep:::.prod_cache_table_digest(dplyr::mutate(base, extra = 1))
  ))
})

test_that("the table digest aborts without an area_code column", {
  expect_error(
    whep:::.prod_cache_table_digest(tibble::tibble(year = 2000L)),
    "area_code"
  )
})

test_that("a matching cache is reusable", {
  prod <- .fake_production(years = 1999:2005)
  meta <- whep:::.prod_cache_meta(prod, "fp1")
  expect_null(
    whep:::.prod_cache_stale_reason(meta, 2000:2001, "fp1", prod)
  )
})

test_that("a cache with no sidecar is stale", {
  prod <- .fake_production()
  expect_match(
    whep:::.prod_cache_stale_reason(NULL, 2000:2001, "fp1", prod),
    "no fingerprint sidecar"
  )
  expect_match(
    whep:::.prod_cache_stale_reason(
      list(min_year = 1999L, max_year = 2005L),
      2000:2001,
      "fp1",
      prod
    ),
    "no fingerprint sidecar"
  )
})

test_that("a cache not covering the requested years is stale", {
  prod <- .fake_production(years = 2000:2001)
  meta <- whep:::.prod_cache_meta(prod, "fp1")
  expect_match(
    whep:::.prod_cache_stale_reason(meta, 1995:2001, "fp1", prod),
    "covers 2000-2001, request is 1995-2001"
  )
  expect_match(
    whep:::.prod_cache_stale_reason(meta, 2000:2010, "fp1", prod),
    "covers 2000-2001, request is 2000-2010"
  )
})

test_that("an area-model change invalidates a year-covering cache", {
  # The whep#657 defect: same year span, older area vocabulary.
  old <- .fake_production(years = 1961:2020, area_codes = c(276L, 277L))
  meta <- whep:::.prod_cache_meta(old, "fp-june")
  expect_null(
    whep:::.prod_cache_stale_reason(meta, 2020, "fp-june", old)
  )
  expect_match(
    whep:::.prod_cache_stale_reason(meta, 2020, "fp-after-628", old),
    "package data changed"
  )
})

test_that("a cache whose table no longer matches its sidecar is stale", {
  prod <- .fake_production(years = 2000:2001)
  meta <- whep:::.prod_cache_meta(prod, "fp1")
  swapped <- .fake_production(years = 2000:2001, area_codes = c(276L, 277L))
  expect_match(
    whep:::.prod_cache_stale_reason(meta, 2000:2001, "fp1", swapped),
    "no longer matches its recorded schema"
  )
})
