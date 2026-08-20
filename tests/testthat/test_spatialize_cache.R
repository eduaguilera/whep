.fake_production <- function(years = 2000:2001, area_codes = c(206L, 208L)) {
  tidyr::expand_grid(year = years, area_code = area_codes) |>
    dplyr::mutate(item_prod_code = 15L, value = 1)
}

test_that("the data payload is discovered and every file exists", {
  files <- whep:::.whep_data_files()
  expect_gt(length(files), 0)
  expect_true(all(file.exists(files)))
  expect_false(anyDuplicated(files) > 0)
})

test_that("the digest set covers every dataset the package ships", {
  digests <- whep:::.whep_data_digests()
  expect_gt(length(digests), 0)
  expect_false(anyNA(digests))
  expect_true(all(nchar(digests) > 16))
  expect_named(digests, basename(whep:::.whep_data_files()))
  # Nothing may be silently missed: either the source layout, where every
  # dataset is its own `<name>.rda`, or the installed lazy-load database,
  # which holds all of them (whep#657 -- an earlier version keyed on object
  # names and dropped the 45 objects inside multi-object archives).
  shipped <- unique(stringr::str_remove(
    utils::data(package = "whep")$results[, "Item"],
    " .*$"
  ))
  covered <- "Rdata.rdb" %in%
    names(digests) ||
    all(paste0(shipped, ".rda") %in% names(digests))
  expect_true(covered)
})

test_that("the digest set is identical across calls", {
  expect_identical(
    whep:::.whep_data_digests(),
    whep:::.whep_data_digests()
  )
})

test_that("digests are a pure function of file bytes", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "fake.rda")
  writeBin(as.raw(1:10), path)
  before <- whep:::.whep_data_digests(path)
  expect_identical(before, whep:::.whep_data_digests(path))
  writeBin(as.raw(c(99L, 2:10)), path)
  expect_false(identical(before, whep:::.whep_data_digests(path)))
})

test_that("digesting a missing file aborts", {
  expect_error(
    whep:::.whep_data_digests(file.path(tempdir(), "absent.rda")),
    "missing file"
  )
})

test_that("the fingerprint is order-independent and stable", {
  digests <- c(a = "aaa", b = "bbb")
  expect_identical(
    whep:::.prod_cache_fingerprint(digests),
    whep:::.prod_cache_fingerprint(rev(digests))
  )
  fingerprint <- whep:::.prod_cache_fingerprint()
  expect_type(fingerprint, "character")
  expect_gt(nchar(fingerprint), 16)
  expect_identical(fingerprint, whep:::.prod_cache_fingerprint())
  expect_identical(fingerprint, whep:::.prod_cache_fingerprint())
})

test_that("the fingerprint moves when any one dataset changes", {
  digests <- c(regions_full.rda = "aaa", items_cbs.rda = "bbb")
  moved <- c(regions_full.rda = "ccc", items_cbs.rda = "bbb")
  expect_false(identical(
    whep:::.prod_cache_fingerprint(digests),
    whep:::.prod_cache_fingerprint(moved)
  ))
})

test_that("the fingerprint refuses an empty or NA digest set", {
  expect_error(whep:::.prod_cache_fingerprint(character(0)), "empty digest")
  expect_error(
    whep:::.prod_cache_fingerprint(c(a = NA_character_)),
    "empty digest"
  )
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
