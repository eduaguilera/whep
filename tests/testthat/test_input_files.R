testthat::test_that(".find_cache_dir returns NULL for uncached version", {
  file_info <- .fetch_file_info(
    "commodity_balance_sheet",
    whep::whep_inputs
  )
  result <- .find_cache_dir(
    file_info,
    "commodity_balance_sheet",
    "99999999T999999Z-fake0"
  )

  testthat::expect_null(result)
})

testthat::test_that(".read_file matches the extension literally, not as a
  regex (a '.' does not stand in for any character)", {
  tmpdir <- withr::local_tempdir()

  # A decoy that would match the *regex* "tar.gz" (the "." standing in for
  # "X"), but is not actually a tar.gz file at all.
  decoy <- file.path(tmpdir, "decoy_tarXgz")
  writeLines("not a tarball", decoy)

  # The real tar.gz, containing one known file.
  member_dir <- withr::local_tempdir()
  member_file <- file.path(member_dir, "member.txt")
  writeLines("hello", member_file)
  archive <- file.path(tmpdir, "archive.tar.gz")
  utils::tar(archive, files = member_file, compression = "gzip")

  result <- .read_file(c(decoy, archive), "tar.gz")

  testthat::expect_true(any(fs::path_file(result) == "member.txt"))
})

testthat::test_that("whep_read_file produces valid tibble", {
  testthat::expect_message(
    result <- whep_read_file("read_example"),
    "Fetching files"
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(nrow(result) > 0)
  testthat::expect_true(ncol(result) > 0)
})

testthat::test_that("whep_read_file reads both csv and parquet formats", {
  result_csv <- whep_read_file("read_example", type = "csv")
  result_parquet <- whep_read_file("read_example", type = "parquet")

  testthat::expect_s3_class(result_csv, "tbl_df")
  testthat::expect_s3_class(result_parquet, "tbl_df")
  testthat::expect_equal(nrow(result_csv), nrow(result_parquet))
  testthat::expect_equal(ncol(result_csv), ncol(result_parquet))
})

testthat::test_that("whep_read_file errors with invalid file alias", {
  testthat::expect_error(
    whep_read_file("nonexistent_alias_xyz"),
    "There is no file entry"
  )
})

testthat::test_that("whep_read_file errors with invalid file type", {
  testthat::expect_error(
    whep_read_file("read_example", type = "invalid_type"),
    "Unknown file type"
  )
})

testthat::test_that("a missing NetCDF member errors instead of returning NULL", {
  # Issue #457: the nc and nc4 types hand back a path rather than contents, and
  # were missing from the known-formats list, so a pin with no NetCDF member
  # returned NULL and the caller failed later and somewhere else.
  testthat::expect_error(
    whep_read_file("read_example", type = "nc"),
    "no .*nc.* file"
  )
})

testthat::test_that("whep_read_file errors when remote down and no cache", {
  local_mocked_bindings(
    .check_remote_reachable = function(...) {
      cli::cli_abort("Remote host is not reachable.")
    },
    .find_cache_dir = function(...) NULL
  )

  # The alias is one of the frozen predecessor-pipeline references, so the read
  # flags its provenance before it gets as far as the cache.
  testthat::expect_warning(
    testthat::expect_error(
      whep_read_file("commodity_balance_sheet"),
      "No local cached copy"
    ),
    "predecessor"
  )
})

# .choose_version -----------------------------------------------------------

testthat::test_that(".choose_version returns frozen when user is NULL", {
  result <- .choose_version("20240101T000000Z-abc", NULL)
  testthat::expect_equal(
    result,
    "20240101T000000Z-abc"
  )
})

testthat::test_that(".choose_version returns NULL for blank registry version", {
  testthat::expect_null(.choose_version(NA_character_, NULL))
  testthat::expect_null(.choose_version("", NULL))
})

testthat::test_that(".choose_version returns NULL for 'latest'", {
  result <- .choose_version(
    "20240101T000000Z-abc",
    "latest"
  )
  testthat::expect_null(result)
})

testthat::test_that(".choose_version returns user version when specified", {
  result <- .choose_version(
    "20240101T000000Z-abc",
    "custom-version"
  )
  testthat::expect_equal(result, "custom-version")
})

# .fetch_file_info ----------------------------------------------------------

testthat::test_that(".fetch_file_info returns correct entry", {
  result <- .fetch_file_info(
    "read_example",
    whep::whep_inputs
  )
  testthat::expect_type(result, "list")
  testthat::expect_equal(result$alias, "read_example")
})

testthat::test_that(".fetch_file_info errors on unknown alias", {
  testthat::expect_error(
    .fetch_file_info(
      "nonexistent_xyz",
      whep::whep_inputs
    ),
    "There is no file entry"
  )
})

testthat::test_that(".fetch_file_info errors on duplicate alias", {
  duped_inputs <- dplyr::bind_rows(
    whep::whep_inputs,
    whep::whep_inputs |> dplyr::slice(1)
  )
  alias <- whep::whep_inputs$alias[[1]]

  testthat::expect_error(
    .fetch_file_info(alias, duped_inputs),
    "there should be only one"
  )
})

# whep_list_file_versions ---------------------------------------------------

testthat::test_that("whep_list_file_versions works for local example", {
  result <- whep_list_file_versions("read_example")
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(nrow(result) >= 1)
})

# One name, one table -------------------------------------------------------

testthat::test_that("no pin alias shadows a packaged dataset name", {
  # #489: `biomass_coefs` was reachable two ways -- `whep::biomass_coefs`,
  # built from `inst/extdata/harmonization/biomass_coefs.csv`, and a
  # `whep_read_file("biomass_coefs")` pin frozen at 20250728T082553Z. The two
  # disagreed on 12 of their 36 shared columns, so `build_food_supply()` and
  # `create_n_prov_destiny()` ran on different nitrogen coefficients for the
  # same commodity with nothing declaring which was authoritative. The defect
  # is one name serving two tables, so the guard is on the name space, not on
  # any single coefficient: no alias may shadow a packaged dataset.
  aliases <- whep::whep_inputs$alias
  datasets <- utils::data(package = "whep")$results[, "Item"]

  testthat::expect_setequal(intersect(aliases, datasets), character(0))
  # Not vacuous: both vocabularies are populated.
  testthat::expect_gt(length(aliases), 50L)
  testthat::expect_gt(length(datasets), 50L)
})

# Offline cache fallback ----------------------------------------------------

# Builds a pins url-board cache on disk exactly as `pins::pin_download()`
# leaves it: one directory per version, named after the hash of that version's
# URL, holding the pinned files, the pin metadata (`data.txt`) and the
# download bookkeeping (`download-cache.yaml`) naming the URLs they came from.
.write_fake_pin_cache <- function(cache_root, pin_url, created, hash_prefix) {
  version <- paste0(created, "-", hash_prefix)
  version_url <- paste0(pin_url, version, "/")
  dir <- fs::dir_create(
    fs::path(cache_root, "url", rlang::hash(version_url))
  )
  file_name <- "pinned.parquet"
  nanoparquet::write_parquet(
    tibble::tibble(version = version, value = 1),
    fs::path(dir, file_name)
  )
  yaml::write_yaml(
    list(
      file = file_name,
      pin_hash = paste0(hash_prefix, strrep("0", 27L)),
      type = "file",
      created = created,
      api_version = 1L
    ),
    fs::path(dir, "data.txt")
  )
  readr::write_lines(
    c(
      paste0("? ", version_url, "data.txt"),
      ": expires: ~",
      paste0("? ", version_url, file_name),
      ": expires: ~"
    ),
    fs::path(dir, "download-cache.yaml")
  )

  dir
}

.local_pin_cache_root <- function(env = parent.frame()) {
  cache_root <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(
    c(
      PINS_CACHE_DIR = cache_root,
      R_CONFIG_ACTIVE = NA,
      PINS_USE_CACHE = NA
    ),
    .local_envir = env
  )

  cache_root
}

.pin_url_for <- function(file_alias) {
  file_alias |>
    .fetch_file_info(whep::whep_inputs) |>
    purrr::pluck("board_url") |>
    stringr::str_replace("_pins\\.yaml$", "") |>
    paste0(file_alias, "/")
}

testthat::test_that(".pins_cache_base honours PINS_CACHE_DIR", {
  # pins resolves its own cache through `PINS_CACHE_DIR`, so a base that
  # ignores it looks in a directory the download never wrote to (#245).
  cache_root <- withr::local_tempdir()
  withr::local_envvar(
    c(
      PINS_CACHE_DIR = cache_root,
      R_CONFIG_ACTIVE = NA,
      PINS_USE_CACHE = NA
    )
  )

  testthat::expect_equal(
    fs::path(.pins_cache_base()),
    fs::path(cache_root)
  )
})

testthat::test_that(".find_cache_dir resolves a NULL version from cache", {
  # #245: a request for the latest version, and a blank frozen version, both
  # reach `.find_cache_dir()` as NULL. That NULL used to be pasted into the URL
  # as nothing at all, hashing `.../alias//`, which never matches the directory
  # the download actually wrote.
  cache_root <- .local_pin_cache_root()
  alias <- "commodity_balance_sheet"
  pin_url <- .pin_url_for(alias)
  .write_fake_pin_cache(cache_root, pin_url, "20240101T000000Z", "aaaaa")
  newest <- .write_fake_pin_cache(
    cache_root,
    pin_url,
    "20250101T000000Z",
    "bbbbb"
  )
  file_info <- .fetch_file_info(alias, whep::whep_inputs)

  result <- .find_cache_dir(file_info, alias, NULL)

  # The newest cached version is what a `"latest"` request must resolve to.
  testthat::expect_equal(fs::path(result), fs::path(newest))
})

testthat::test_that(".find_cache_dir still finds a concrete version", {
  cache_root <- .local_pin_cache_root()
  alias <- "commodity_balance_sheet"
  pin_url <- .pin_url_for(alias)
  wanted <- .write_fake_pin_cache(
    cache_root,
    pin_url,
    "20240101T000000Z",
    "aaaaa"
  )
  .write_fake_pin_cache(cache_root, pin_url, "20250101T000000Z", "bbbbb")
  file_info <- .fetch_file_info(alias, whep::whep_inputs)

  result <- .find_cache_dir(file_info, alias, "20240101T000000Z-aaaaa")

  testthat::expect_equal(fs::path(result), fs::path(wanted))
})

testthat::test_that(".find_cache_dir returns NULL when nothing is cached", {
  .local_pin_cache_root()
  file_info <- .fetch_file_info(
    "commodity_balance_sheet",
    whep::whep_inputs
  )

  testthat::expect_null(
    .find_cache_dir(file_info, "commodity_balance_sheet", NULL)
  )
  testthat::expect_null(
    .find_cache_dir(
      file_info,
      "commodity_balance_sheet",
      "99999999T999999Z-fake0"
    )
  )
})

testthat::test_that(".find_cache_dir ignores another pin's cache", {
  cache_root <- .local_pin_cache_root()
  .write_fake_pin_cache(
    cache_root,
    .pin_url_for("faostat-fertilizer-nutrients"),
    "20250101T000000Z",
    "bbbbb"
  )
  file_info <- .fetch_file_info(
    "commodity_balance_sheet",
    whep::whep_inputs
  )

  testthat::expect_null(
    .find_cache_dir(file_info, "commodity_balance_sheet", NULL)
  )
})

testthat::test_that("whep_read_file falls back to cache for 'latest'", {
  cache_root <- .local_pin_cache_root()
  alias <- "commodity_balance_sheet"
  .write_fake_pin_cache(
    cache_root,
    .pin_url_for(alias),
    "20250101T000000Z",
    "bbbbb"
  )
  local_mocked_bindings(
    .check_remote_reachable = function(...) {
      cli::cli_abort("Remote host is not reachable.")
    }
  )

  # Two warnings, in this order: the provenance flag on a predecessor-pipeline
  # alias, then the cache fallback. The inner expectation takes the first.
  testthat::expect_warning(
    testthat::expect_warning(
      result <- whep_read_file(alias, version = "latest"),
      "predecessor"
    ),
    "Using cached local copy"
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(result$version, "20250101T000000Z-bbbbb")
})

testthat::test_that(".find_cache_dir survives an unreadable neighbour", {
  # An interrupted download leaves a half-written `data.txt`. Scanning happens
  # only once the remote is already unreachable, so one unparseable directory
  # must not stop a good cached copy from being found.
  cache_root <- .local_pin_cache_root()
  alias <- "commodity_balance_sheet"
  pin_url <- .pin_url_for(alias)
  wanted <- .write_fake_pin_cache(
    cache_root,
    pin_url,
    "20250101T000000Z",
    "bbbbb"
  )
  broken <- fs::dir_create(fs::path(cache_root, "url", "broken0000"))
  readr::write_lines("file:\n  - a: [", fs::path(broken, "data.txt"))
  empty <- fs::dir_create(fs::path(cache_root, "url", "empty00000"))
  file_info <- .fetch_file_info(alias, whep::whep_inputs)

  result <- .find_cache_dir(file_info, alias, NULL)

  testthat::expect_true(fs::dir_exists(empty))
  testthat::expect_equal(fs::path(result), fs::path(wanted))
})

# -- the year filter is pushed into the parquet -------------------------------

# `lpjml-soc-hydrology` holds 193,317,960 monthly rows over 1901-2022. Reading
# it whole and filtering afterwards is what made a one-year gridded carbon
# balance unrunnable -- two hours of CPU without ever leaving the input stage.
# The pushdown is only worth having if it returns exactly what the whole-file
# read returned, so that is what these pin.

.year_pin_fixture <- function(path) {
  full <- tibble::tibble(
    lon = rep(c(0.25, 0.75), times = 5L),
    lat = rep(c(10.25, 10.75), times = 5L),
    year = rep(2001:2005, each = 2L),
    value = (1:10) * 1.5
  )
  nanoparquet::write_parquet(full, path)
  full
}

testthat::test_that(".read_parquet_years agrees with the whole-file read", {
  path <- withr::local_tempfile(fileext = ".parquet")
  .year_pin_fixture(path)

  fast <- whep:::.read_parquet_years(path, 2003L)
  slow <- nanoparquet::read_parquet(path) |>
    tibble::as_tibble() |>
    whep:::.filter_years_if_present(2003L)

  testthat::expect_equal(fast, slow)
  testthat::expect_equal(nrow(fast), 2L)
})

testthat::test_that(".read_parquet_years honours non-contiguous years", {
  path <- withr::local_tempfile(fileext = ".parquet")
  .year_pin_fixture(path)

  out <- whep:::.read_parquet_years(path, c(2001L, 2003L))

  # The pushdown can only express a RANGE, so 2002 is read off the disk and
  # has to be dropped afterwards. Without that second filter this returns a
  # year the caller did not ask for.
  testthat::expect_equal(sort(unique(out$year)), c(2001L, 2003L))
})

testthat::test_that(".read_parquet_years reads it all when years is NULL", {
  path <- withr::local_tempfile(fileext = ".parquet")
  full <- .year_pin_fixture(path)

  testthat::expect_equal(whep:::.read_parquet_years(path, NULL), full)
})

testthat::test_that(".read_parquet_years aborts with no year column", {
  path <- withr::local_tempfile(fileext = ".parquet")
  yearless <- tibble::tibble(lon = c(0.25, 0.75), value = c(1.5, 3))
  nanoparquet::write_parquet(yearless, path)

  # Both ways of absorbing this are worse than stopping: returning the whole
  # file hands back every year when one was asked for, and dropping the filter
  # silently reinstates the read the pushdown exists to avoid.
  testthat::expect_error(
    whep:::.read_parquet_years(path, 2003L, year_col = "vintage"),
    "no .*vintage.* column"
  )
})

testthat::test_that("whep_read_file forwards years to the parquet read", {
  path <- withr::local_tempfile(fileext = ".parquet")
  .year_pin_fixture(path)

  # `.read_file()` is the wiring point: it is what `whep_read_file()` hands
  # the downloaded paths to, so this pins that `years` actually reaches it.
  out <- whep:::.read_file(path, "parquet", years = 2004L)

  testthat::expect_equal(unique(out$year), 2004L)
})

testthat::test_that("whep_read_file refuses years on a path-returning type", {
  # Silently ignoring the filter would hand the caller every year it asked to
  # exclude, which is the failure the argument exists to prevent.
  testthat::expect_error(
    whep:::.read_file("some.nc", "nc", years = 2004L),
    class = "rlang_error"
  )
})

testthat::test_that(".read_parquet_years does not push down a text year", {
  path <- withr::local_tempfile(fileext = ".parquet")
  # Arrow does not refuse this: it coerces silently, so a text column whose
  # order differs from its numeric order would quietly drop requested years.
  # The whole-file path coerces in R instead, which is why it is taken here.
  textual <- tibble::tibble(
    year = as.character(c(998, 1000, 1002)),
    value = c(1, 2, 3)
  )
  nanoparquet::write_parquet(textual, path)

  out <- whep:::.read_parquet_years(path, c(998L, 1000L, 1002L))

  testthat::expect_equal(nrow(out), 3L)
})

testthat::test_that(".read_parquet_years honours a non-default year_col", {
  path <- withr::local_tempfile(fileext = ".parquet")
  # Both a `Year` and a `year` column, which is the shape that used to fail
  # silently: the range was pushed down on one and the exact set applied to
  # the other, returning a subset the caller never asked for.
  nanoparquet::write_parquet(
    tibble::tibble(
      Year = c(2001L, 2002L, 2003L),
      year = c(2003L, 2002L, 2001L),
      value = c(1, 2, 3)
    ),
    path
  )

  out <- whep:::.read_parquet_years(path, 2003L, year_col = "Year")

  testthat::expect_equal(out$Year, 2003L)
  testthat::expect_equal(out$value, 3)
})

testthat::test_that(".read_parquet_years asks the file for nothing", {
  path <- withr::local_tempfile(fileext = ".parquet")
  .year_pin_fixture(path)

  # An empty request used to fall through to the whole-file read -- about
  # 12 GB for `lpjml-soc-hydrology` -- and then discard every row of it.
  out <- whep:::.read_parquet_years(path, integer(0))

  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_setequal(names(out), c("lon", "lat", "year", "value"))
})

testthat::test_that(".filter_years_if_present honours year_col", {
  d <- tibble::tibble(vintage = c(2001L, 2002L), value = c(1, 2))

  testthat::expect_equal(
    whep:::.filter_years_if_present(d, 2002L, "vintage")$value,
    2
  )
  testthat::expect_error(
    whep:::.filter_years_if_present(d, 2002L),
    class = "whep_year_filter_error"
  )
})

# Frozen predecessor-pipeline references ------------------------------------

testthat::test_that("reading a predecessor-pipeline pin says so", {
  # #1030: the `primary_prod` pin is the 2025-07-14 snapshot of the pipeline
  # that preceded this package, kept only as a benchmark. Its 2020-2021 fodder
  # harvested area is carried forward from 2019 -- 85.93 Mha a year over 468
  # country-item series, equal to 2019 to the last digit -- where current code
  # emits no fodder row after 2019, because `eu-agridb-fodder` stops in 2019,
  # `faostat-production-old` in 2013, and `faostat-production` carries none of
  # the 16 fodder item codes at all. Same schema, plausible magnitude and no
  # flag was the whole defect, so the flag is what is tested.
  testthat::expect_warning(
    .warn_legacy_reference("primary_prod"),
    "predecessor"
  )
  testthat::expect_warning(
    .warn_legacy_reference("primary_prod"),
    "fodder"
  )
})

testthat::test_that("the predecessor flag skips aliases code reads", {
  purrr::walk(
    .legacy_reference_aliases(),
    ~ testthat::expect_warning(.warn_legacy_reference(.x), "predecessor")
  )
  # Aliases current code reads on its default build path must stay silent,
  # `crop_residues` and `bilateral_trade` included even though they come from
  # the same 2025-07-14 batch.
  purrr::walk(
    c("faostat-production", "bilateral_trade", "crop_residues", "luh2-areas"),
    ~ testthat::expect_no_warning(.warn_legacy_reference(.x))
  )
})

testthat::test_that("every flagged predecessor alias is a real alias", {
  # A typo here would disable the flag silently, which is the failure mode the
  # flag exists to prevent.
  testthat::expect_setequal(
    setdiff(.legacy_reference_aliases(), whep::whep_inputs$alias),
    character(0)
  )
  testthat::expect_length(.legacy_reference_aliases(), 4L)
})
