# Shared, build-once fixtures for test_n_balance.R and
# test_n_balance_inputs.R (#1349).
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
#     file's last test rebuilds its fixtures and asserts the cache still
#     holds exactly that, which catches any by-reference mutation.

.n_fixture_cache <- new.env(parent = emptyenv())
.n_fixture_builds <- new.env(parent = emptyenv())

memo_n_fixture <- function(key, build) {
  if (!exists(key, envir = .n_fixture_cache, inherits = FALSE)) {
    assign(key, build(), envir = .n_fixture_cache)
    assign(key, build, envir = .n_fixture_builds)
  }
  get(key, envir = .n_fixture_cache, inherits = FALSE)
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
