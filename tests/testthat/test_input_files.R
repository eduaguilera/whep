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

  testthat::expect_error(
    whep_read_file("commodity_balance_sheet"),
    "No local cached copy"
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

  testthat::expect_warning(
    result <- whep_read_file(alias, version = "latest"),
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
