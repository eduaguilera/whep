.fixed_time <- function() {
  as.POSIXct("2026-01-01", tz = "UTC")
}

.inputs_fixture <- function() {
  tibble::tribble(
    ~alias, ~board_url, ~version,
    "trade", "url-a", "v1",
    "production", "url-b", "v2"
  )
}

testthat::test_that("record_provenance records all inputs by default", {
  prov <- whep::record_provenance(
    inputs = .inputs_fixture(),
    recorded_at = .fixed_time()
  )

  prov |>
    pointblank::expect_col_exists(
      c(
        "recorded_at",
        "whep_version",
        "r_version",
        "input_alias",
        "input_version"
      )
    )
  testthat::expect_equal(nrow(prov), 2)
  testthat::expect_setequal(prov$input_alias, c("trade", "production"))
  testthat::expect_true(all(prov$recorded_at == .fixed_time()))
})

testthat::test_that("record_provenance filters to requested aliases", {
  prov <- whep::record_provenance(
    aliases = "production",
    inputs = .inputs_fixture(),
    recorded_at = .fixed_time()
  )

  testthat::expect_equal(prov$input_alias, "production")
  testthat::expect_equal(prov$input_version, "v2")
})

testthat::test_that("record_provenance aborts on unknown alias", {
  testthat::expect_error(
    whep::record_provenance(
      aliases = "nope",
      inputs = .inputs_fixture()
    ),
    "Unknown input alias"
  )
})

testthat::test_that("attach/get provenance round-trips", {
  prov <- whep::record_provenance(
    inputs = .inputs_fixture(),
    recorded_at = .fixed_time()
  )
  result <- whep::attach_provenance(tibble::tibble(value = 1), prov)

  testthat::expect_equal(whep::get_provenance(result), prov)
  testthat::expect_equal(nrow(result), 1)
})

testthat::test_that("get_provenance returns NULL when absent", {
  testthat::expect_null(whep::get_provenance(tibble::tibble(value = 1)))
})
