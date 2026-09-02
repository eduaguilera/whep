# Tests for the IBGE SIDRA admin-statistics reader (#1000, T30).
#
# Nothing here reaches the network: every test stubs the private
# `.fetch_sidra()` with a payload parsed from a fixture under
# `fixtures/sidra_*.json`. The fixtures are raw SIDRA responses saved
# verbatim on 2026-09-02 from the unauthenticated values API:
#
# - `sidra_5457_pam_states.json`: t/5457/n3/11,17,35,41,43/v/8331,216,214/
#   p/1985,1990,2020/c782/0,40122,40102,40127 -- 180 rows carrying the "-",
#   "..." and ".." codes, the "Total" category 0, and Tocantins, which did
#   not exist in 1985.
# - `sidra_3939_ppm_herds.json`: t/3939/n3/11,17,35,41,43/v/105/p/1990,2020/
#   c79/all -- 100 rows, all ten herd types.
# - `sidra_94_milked_cows.json`: t/94/n3/11,17,35,41,43/v/107/p/1990,2020 --
#   10 rows, a table with no classification.
#
# The stub ignores the URL it is handed, so a fixture's own years reach the
# result whatever years the call requested; assertions filter by year.
#
# The fourth documented code, "X" (withheld to protect the informant), is
# absent from these slices because none of the sampled cells is suppressed;
# it is exercised from a constructed value vector, which is stated where it
# happens.
#
# The package's NAMESPACE is not regenerated in this wave, so the exported
# reader is reached as `whep:::read_admin_stats_sidra()` rather than
# `whep::`.

sidra_payload <- function(name) {
  testthat::skip_if_not_installed("jsonlite")
  jsonlite::fromJSON(
    testthat::test_path("fixtures", name),
    simplifyVector = FALSE
  )
}

# A `.fetch_sidra()` stub returning `payloads` in call order (the last one
# repeating) and recording the URLs it was asked for.
sidra_stub <- function(payloads) {
  state <- new.env(parent = emptyenv())
  state$urls <- character()
  state$fetch <- function(url) {
    state$urls <- c(state$urls, url)
    payloads[[min(length(state$urls), length(payloads))]]
  }
  state
}

sidra_header_only <- function(payload) {
  payload[1]
}

testthat::test_that(".sidra_values_url builds the documented URL pattern", {
  testthat::expect_equal(
    whep:::.sidra_values_url("5457", 2019:2020),
    paste0(
      "https://apisidra.ibge.gov.br/values/t/5457/n3/all/",
      "v/8331,216,214/p/2019,2020/c782/all?formato=json"
    )
  )
  testthat::expect_equal(
    whep:::.sidra_values_url("3939", 2020, items = c("2681", "2677")),
    paste0(
      "https://apisidra.ibge.gov.br/values/t/3939/n3/all/",
      "v/105/p/2020/c79/2681,2677?formato=json"
    )
  )
  # Table 94 has no classification, so the URL carries no /c segment.
  testthat::expect_equal(
    whep:::.sidra_values_url("94", 2020),
    paste0(
      "https://apisidra.ibge.gov.br/values/t/94/n3/all/",
      "v/107/p/2020?formato=json"
    )
  )
})

testthat::test_that(".sidra_year_blocks pages without losing a year", {
  blocks <- whep:::.sidra_year_blocks(1974:2024, 5)

  testthat::expect_length(blocks, 11)
  testthat::expect_true(all(lengths(blocks) <= 5))
  testthat::expect_equal(sort(unlist(blocks)), 1974:2024)
  testthat::expect_equal(blocks[[1]], 1974:1978)
  testthat::expect_equal(blocks[[11]], 2024L)
  testthat::expect_error(whep:::.sidra_year_blocks(1974:1980, 0))
})

# A request costs units x categories x variables x years values. SIDRA serves
# 27 states; classification 782 has 72 categories and table 5457 is read for
# three variables, which is the worst case of the three tables.
testthat::test_that("default year blocks stay under the API value cap", {
  cost <- function(table, categories, states = 27) {
    spec <- whep:::.sidra_table_spec(table)
    states * categories * length(spec$variables) * spec$block
  }

  testthat::expect_lt(cost("5457", 72), whep:::.sidra_value_cap())
  testthat::expect_lt(cost("3939", 10), whep:::.sidra_value_cap())
  testthat::expect_lt(cost("94", 1), whep:::.sidra_value_cap())
})

testthat::test_that("the reader pages by year block and concatenates", {
  payload <- sidra_payload("sidra_5457_pam_states.json")
  stub <- sidra_stub(list(payload, sidra_header_only(payload)))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  out <- whep:::read_admin_stats_sidra(
    "5457",
    years = 1990:1999,
    years_per_request = 5
  )

  testthat::expect_length(stub$urls, 2)
  testthat::expect_true(grepl("/p/1990,1991,1992,1993,1994/", stub$urls[1]))
  testthat::expect_true(grepl("/p/1995,1996,1997,1998,1999/", stub$urls[2]))
  # Only the first block returned rows; the second was header-only.
  testthat::expect_gt(nrow(out), 0)
  testthat::expect_setequal(unique(out$year), c(1985L, 1990L, 2020L))
})

testthat::test_that("the reader returns the source-native contract columns", {
  payload <- sidra_payload("sidra_5457_pam_states.json")
  stub <- sidra_stub(list(payload))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  out <- whep:::read_admin_stats_sidra("5457", years = 2020)

  testthat::expect_named(
    out,
    c(
      "source",
      "source_native_unit_id",
      "source_native_unit_name",
      "source_native_item_code",
      "source_native_item_name",
      "indicator_used",
      "quantity",
      "year",
      "value",
      "value_unit",
      "value_flag",
      "grain",
      "nuts_version",
      "source_version",
      "recorded_at"
    )
  )
  testthat::expect_true(is.character(out$source_native_unit_id))
  testthat::expect_true(is.integer(out$year))
  testthat::expect_true(is.double(out$value))
  testthat::expect_equal(unique(out$source), "IBGE_PAM")
  testthat::expect_equal(unique(out$grain), "admin1")
  testthat::expect_true(all(is.na(out$nuts_version)))
  # SIDRA exposes no vintage stamp, so `source_version` is always NA and the
  # provenance a row carries is its fetch time.
  testthat::expect_true(all(is.na(out$source_version)))
  testthat::expect_true(all(
    grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      out$recorded_at
    )
  ))
  testthat::expect_setequal(
    unique(out$indicator_used),
    c("area_harvested", "area_planted_or_sown", "production")
  )
  testthat::expect_setequal(unique(out$value_unit), c("ha", "tonnes"))
  testthat::expect_setequal(unique(out$quantity), c("area", "production"))
  # UF codes are the 2-digit IBGE codes, kept as served.
  testthat::expect_setequal(
    unique(out$source_native_unit_id),
    c("11", "17", "35", "41", "43")
  )
})

testthat::test_that("special value codes parse as documented", {
  payload <- sidra_payload("sidra_5457_pam_states.json")
  stub <- sidra_stub(list(payload))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  # The fixture spans 1985, so the pre-1988 planted-area warning fires; it
  # has its own test below.
  out <- suppressWarnings(
    whep:::read_admin_stats_sidra("5457", years = c(1985L, 1990L, 2020L))
  )

  # "-" is an absolute zero: Rondonia (UF 11) grows no wheat (40127).
  zero <- dplyr::filter(
    out,
    source_native_unit_id == "11",
    source_native_item_code == "40127",
    indicator_used == "area_harvested",
    year == 1985L
  )
  testthat::expect_equal(zero$value, 0)
  testthat::expect_true(is.na(zero$value_flag))

  # "..." is not available: Tocantins (UF 17) did not exist in 1985.
  unavailable <- dplyr::filter(
    out,
    source_native_unit_id == "17",
    source_native_item_code == "40122",
    indicator_used == "area_harvested",
    year == 1985L
  )
  testthat::expect_true(is.na(unavailable$value))
  testthat::expect_equal(unavailable$value_flag, "...")

  # Value and flag are complements: a flagged row has no value, a valued row
  # no flag.
  testthat::expect_true(all(is.na(out$value) == !is.na(out$value_flag)))

  # ".." is not applicable: the "Total" category has no production in tonnes.
  not_applicable <- whep:::read_admin_stats_sidra(
    "5457",
    years = 2020,
    items = "0"
  ) |>
    dplyr::filter(
      source_native_unit_id == "11",
      source_native_item_code == "0",
      indicator_used == "production",
      year == 2020L
    )
  testthat::expect_true(is.na(not_applicable$value))
  testthat::expect_equal(not_applicable$value_flag, "..")
})

# "X" (withheld to protect the informant) is documented by SIDRA but absent
# from the sampled fixture slices, so the vector below is constructed.
testthat::test_that(".sidra_parse_value_codes handles X and refuses junk", {
  parsed <- whep:::.sidra_parse_value_codes(
    c("12345", "-", "...", "..", "X", "1.5")
  )

  testthat::expect_equal(parsed$value, c(12345, 0, NA, NA, NA, 1.5))
  testthat::expect_equal(parsed$flag, c(NA, NA, "...", "..", "X", NA))
  # A locale or format change must abort, not become a silent NA.
  testthat::expect_error(
    whep:::.sidra_parse_value_codes(c("12345", "1.234,5")),
    class = "rlang_error"
  )
})

testthat::test_that("planted area is a proxy indicator absent before 1988", {
  payload <- sidra_payload("sidra_5457_pam_states.json")
  stub <- sidra_stub(list(payload))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  # Asking for pre-1988 years warns rather than silently returning nothing.
  testthat::expect_warning(
    out <- whep:::read_admin_stats_sidra("5457", years = c(1985L, 1990L)),
    "planted-or-sown"
  )

  planted <- dplyr::filter(out, indicator_used == "area_planted_or_sown")
  before <- dplyr::filter(planted, year == 1985L)
  after <- dplyr::filter(planted, year == 1990L)

  testthat::expect_gt(nrow(before), 0)
  testthat::expect_true(all(is.na(before$value)))
  testthat::expect_equal(unique(before$value_flag), "...")
  testthat::expect_gt(nrow(after), 0)
  testthat::expect_true(any(!is.na(after$value)))

  # Harvested area, the indicator the allocation binds on, is reported in
  # 1985, so the proxy is never needed to cover that year.
  harvested_1985 <- dplyr::filter(
    out,
    indicator_used == "area_harvested",
    year == 1985L
  )
  testthat::expect_true(any(!is.na(harvested_1985$value)))

  testthat::expect_no_warning(
    whep:::read_admin_stats_sidra("5457", years = 1990:1994)
  )
})

testthat::test_that("the reader drops the all-crops Total category", {
  payload <- sidra_payload("sidra_5457_pam_states.json")
  stub <- sidra_stub(list(payload))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  default <- whep:::read_admin_stats_sidra("5457", years = 2020)
  requested <- whep:::read_admin_stats_sidra(
    "5457",
    years = 2020,
    items = c("0", "40122")
  )

  testthat::expect_false("0" %in% default$source_native_item_code)
  testthat::expect_true("0" %in% requested$source_native_item_code)
})

testthat::test_that("PPM herd rows come back as head counts", {
  payload <- sidra_payload("sidra_3939_ppm_herds.json")
  stub <- sidra_stub(list(payload))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  out <- whep:::read_admin_stats_sidra("3939", years = 2020)

  testthat::expect_equal(unique(out$source), "IBGE_PPM")
  testthat::expect_equal(unique(out$quantity), "heads")
  testthat::expect_equal(unique(out$value_unit), "heads")
  # Head counts are not one of `admin_shares_schema()`'s indicators.
  testthat::expect_true(all(is.na(out$indicator_used)))
  # The four herd codes T10 must decide on: Caprino (2681) + Ovino (2677)
  # for sheep_goats, and "Galinaceos - galinhas" (32793) against
  # "Galinaceos - total" (32796) for layers/broilers against poultry.
  testthat::expect_true(
    all(c("2681", "2677", "32793", "32796") %in% out$source_native_item_code)
  )
  cattle <- dplyr::filter(
    out,
    source_native_item_code == "2670",
    source_native_unit_id == "35",
    year == 2020L
  )
  testthat::expect_equal(cattle$value, 10568637)
})

testthat::test_that("table 94 supplies the milked-cow dairy split", {
  payload <- sidra_payload("sidra_94_milked_cows.json")
  stub <- sidra_stub(list(payload))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  out <- whep:::read_admin_stats_sidra("94", years = 2020)

  testthat::expect_equal(unique(out$source), "IBGE_PPM")
  # The table has no classification, so the item code is NA and the item
  # name falls back to the served variable name.
  testthat::expect_true(all(is.na(out$source_native_item_code)))
  testthat::expect_equal(
    unique(out$source_native_item_name),
    "Vacas ordenhadas"
  )
  testthat::expect_equal(unique(out$quantity), "heads")
  testthat::expect_true(all(is.na(out$indicator_used)))
  sao_paulo <- dplyr::filter(out, source_native_unit_id == "35", year == 2020L)
  testthat::expect_equal(sao_paulo$value, 1008099)

  testthat::expect_error(
    whep:::read_admin_stats_sidra("94", years = 2020, items = "2670"),
    class = "rlang_error"
  )
})

testthat::test_that("a changed unit of measure aborts instead of converting", {
  payload <- sidra_payload("sidra_5457_pam_states.json")
  # Re-serve harvested area in tonnes (unit code 1017), the shape a silent
  # upstream unit change would take.
  payload[-1] <- lapply(payload[-1], function(row) {
    if (identical(row$D2C, "216")) {
      row$MC <- "1017"
      row$MN <- "Toneladas"
    }
    row
  })
  stub <- sidra_stub(list(payload))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )

  testthat::expect_error(
    whep:::read_admin_stats_sidra("5457", years = 2020),
    class = "rlang_error"
  )
})

testthat::test_that("a malformed payload aborts and an empty one is empty", {
  payload <- sidra_payload("sidra_94_milked_cows.json")

  testthat::expect_error(
    whep:::.sidra_parse_values(list()),
    class = "rlang_error"
  )
  testthat::expect_error(
    whep:::.sidra_parse_values(list(list(V = "Valor"))),
    class = "rlang_error"
  )

  stub <- sidra_stub(list(sidra_header_only(payload)))
  testthat::local_mocked_bindings(
    .fetch_sidra = stub$fetch,
    .package = "whep"
  )
  # SIDRA drops years it has not published, so a future year is an empty
  # result, not an error.
  out <- whep:::read_admin_stats_sidra("94", years = 2100)

  testthat::expect_equal(nrow(out), 0)
  testthat::expect_true("value_flag" %in% names(out))
})

testthat::test_that("read_admin_stats_sidra(example = TRUE) needs no fetch", {
  testthat::local_mocked_bindings(
    .fetch_sidra = function(url) {
      testthat::fail("example = TRUE must not fetch")
    },
    .package = "whep"
  )

  out <- whep:::read_admin_stats_sidra(example = TRUE)

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gte(nrow(out), 10)
  testthat::expect_setequal(unique(out$source), c("IBGE_PAM", "IBGE_PPM"))
  testthat::expect_setequal(unique(out$value_unit), c("ha", "tonnes", "heads"))
  testthat::expect_true(all(out$grain == "admin1"))
  testthat::expect_true(all(is.na(out$source_version)))
  testthat::expect_true(all(is.na(out$value) == !is.na(out$value_flag)))
})
