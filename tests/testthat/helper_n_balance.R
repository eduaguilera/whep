# Shared, build-once fixtures for test_n_balance.R, test_n_balance_inputs.R,
# test_n_boundary_exceedance.R and test_build_sjos_nitrogen.R (#1349). Keys
# carry a per-file prefix ("nb_", "nbi_", "nbx_", "sjos_").
#
# Many tests in those files build the SAME balance from the SAME fixture
# inputs and then check one property of it. Building it once per run and
# letting each test read the shared result keeps every check while paying
# for the build once. A test that needs a different input still calls the
# builder itself; only an unchanged default build goes through the memo.
#
# Two rules keep the sharing honest:
#   * Memoise only builds made outside any `local_mocked_bindings()`, so a
#     mocked result can never be cached and served to a later test.
#   * The cached values are tibbles and lists of tibbles, which R copies on
#     modify, so one test editing its copy cannot leak into another. Each
#     file's last test asserts the cache was not edited in place: either by
#     rebuilding its fixtures (`expect_n_fixtures_untouched()`) or by
#     comparing their hashes at build time (`expect_n_fixtures_unchanged()`).
#     Both catch any by-reference mutation.

.n_fixture_cache <- new.env(parent = emptyenv())
.n_fixture_builds <- new.env(parent = emptyenv())

.n_fixture_hashes <- new.env(parent = emptyenv())

memo_n_fixture <- function(key, build) {
  if (!exists(key, envir = .n_fixture_cache, inherits = FALSE)) {
    value <- build()
    assign(key, value, envir = .n_fixture_cache)
    assign(key, build, envir = .n_fixture_builds)
    assign(key, rlang::hash(value), envir = .n_fixture_hashes)
  }
  get(key, envir = .n_fixture_cache, inherits = FALSE)
}

# The cheaper guard: asserts every fixture memoised under `prefix` still
# hashes to what it hashed to when it was built, so no test edited the shared
# value in place (a copy-on-modify edit leaves the cached value untouched; a
# data.table `:=` does not). It does not rebuild, so it costs a hash instead
# of a second build per fixture.
expect_n_fixtures_unchanged <- function(prefix) {
  keys <- ls(.n_fixture_cache)
  keys <- keys[startsWith(keys, prefix)]
  testthat::expect_gt(length(keys), 0L)
  for (key in keys) {
    testthat::expect_identical(
      rlang::hash(get(key, envir = .n_fixture_cache)),
      get(key, envir = .n_fixture_hashes),
      label = paste("hash of cached fixture", key)
    )
  }
}

# Rebuilds every fixture memoised under `prefix` and asserts the cached copy
# is still identical to a fresh build: nothing a test did to its copy reached
# the shared value. Also asserts the memo was really used, so the check is
# not vacuous.
expect_n_fixtures_untouched <- function(prefix) {
  keys <- ls(.n_fixture_cache)
  keys <- keys[startsWith(keys, prefix)]
  testthat::expect_gt(length(keys), 0L)
  for (key in keys) {
    fresh <- suppressMessages(get(key, envir = .n_fixture_builds)())
    testthat::expect_identical(
      get(key, envir = .n_fixture_cache),
      fresh,
      label = paste("cached fixture", key)
    )
  }
}
