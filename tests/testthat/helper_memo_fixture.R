# Build-once fixtures shared by the tests of one file (#1349).
#
# Several files build the SAME result from the SAME inputs in many tests and
# then check one property of it each. `memo_fixture()` builds it the first
# time a key is asked for and hands the stored value back afterwards, so every
# check still runs while the build is paid for once.
#
# Rules that keep the sharing honest:
#   * Memoise only a call whose inputs are fully named by the key, made
#     outside any `local_mocked_bindings()`, and whose messages and warnings
#     no test asserts: a cached call does not signal them again.
#   * The stored value's `rlang::hash()` is recorded when it is built, and
#     `expect_memo_fixtures_untouched()` asserts every stored value still has
#     that hash. A by-reference change (data.table `:=`, `setDT()`,
#     `setattr()`) made by one test therefore fails the file instead of
#     silently reaching the next test. A copy-on-modify edit of a tibble or a
#     list never reaches the stored value in the first place.

.memo_fixture_cache <- new.env(parent = emptyenv())
.memo_fixture_hashes <- new.env(parent = emptyenv())

memo_fixture <- function(key, build) {
  if (!exists(key, envir = .memo_fixture_cache, inherits = FALSE)) {
    value <- build()
    assign(key, value, envir = .memo_fixture_cache)
    assign(key, rlang::hash(value), envir = .memo_fixture_hashes)
  }
  get(key, envir = .memo_fixture_cache, inherits = FALSE)
}

# Asserts that every value memoised under `prefix` still hashes to what it
# hashed to when it was built, and that the memo was really used, so the check
# is not vacuous.
expect_memo_fixtures_untouched <- function(prefix) {
  keys <- ls(.memo_fixture_cache)
  keys <- keys[startsWith(keys, prefix)]
  testthat::expect_gt(length(keys), 0L)
  for (key in keys) {
    testthat::expect_identical(
      rlang::hash(get(key, envir = .memo_fixture_cache)),
      get(key, envir = .memo_fixture_hashes),
      label = paste("hash of memoised fixture", key)
    )
  }
}
