testthat::test_that("unconverted and partial candidates cannot enter CBS", {
  legacy <- tibble::tibble(Year = 2023L, Value = 1, Unit = "tonnes")
  testthat::expect_silent(whep:::.check_fishstat_cbs_basis(legacy))
  for (column in c("Artifact Type", "Weight Basis", "Source Measure")) {
    candidate <- legacy
    candidate[[column]] <- NA_character_
    testthat::expect_error(
      whep:::.check_fishstat_cbs_basis(candidate),
      class = "whep_fishstat_unconverted"
    )
  }
})

testthat::test_that("the reader rejects candidate metadata before legacy routing", {
  candidate <- tibble::tibble(
    Year = 2023L,
    Value = 1,
    Unit = "tonnes",
    `Source Measure` = "Q_tpw"
  )
  testthat::expect_error(
    whep:::.fishstat_adopted_rows(candidate),
    class = "whep_fishstat_unconverted"
  )
})

.fishstat_candidate_fixture <- function() {
  list(
    trade = tibble::tribble(
      ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
      ~COMMODITY.FAO_CODE, ~MEASURE, ~STATUS,
      690L, "I", 2010L, 1, "034.1.5.2.19", "Q_tpw", "X",
      690L, "I", 2010L, 2, "081.42.1.5.9.99", "Q_tpw", "E",
      583L, "E", 1990L, 3, "034.1.5.2.19", "Q_tpw", "X",
      999L, "I", 2010L, NA_real_, "999.99", "Q_tpw", NA_character_
    ),
    commodities = tibble::tribble(
      ~Code, ~ISSCAAP,
      "034.1.5.2.19", 32L,
      "081.42.1.5.9.99", 39L
    ),
    countries = tibble::tribble(
      ~UN_Code, ~Identifier, ~ISO3_Code,
      690L, 196L, "SYC",
      583L, 145L, "FSM",
      999L, 9999L, ""
    ),
    bridge = tibble::tribble(
      ~isscaap_group, ~item_cbs_code,
      32L, 2762L,
      39L, 2764L
    )
  )
}

testthat::test_that("typed raw candidate retains observations and identities", {
  input <- .fishstat_candidate_fixture()
  out <- do.call(whep:::.prepare_fishstat_candidate, input)
  testthat::expect_equal(nrow(out), nrow(input$trade))
  testthat::expect_equal(out$Value, input$trade$VALUE)
  testthat::expect_equal(out$`Source Flag`, input$trade$STATUS)
  testthat::expect_equal(
    out$`Source Commodity Code`,
    input$trade$COMMODITY.FAO_CODE
  )
  testthat::expect_equal(unique(out$`Weight Basis`), "net_product_weight")
  testthat::expect_equal(
    unique(out$`Artifact Type`),
    "fishstat_classified_raw_v1"
  )
  testthat::expect_equal(
    out$`Coverage Status`,
    c("adopted", "adopted", "polity_not_live", "area_unresolved")
  )
  testthat::expect_equal(
    out$`Classification Status`,
    c("classified", "held_scope", "classified", "unmapped_commodity")
  )
  testthat::expect_false(anyNA(out$`Polity Code`[1:2]))
  testthat::expect_true(all(is.na(out$`Polity Code`[3:4])))
  testthat::expect_error(
    whep:::.fishstat_adopted_rows(out),
    class = "whep_fishstat_unconverted"
  )
})

testthat::test_that("candidate container does not retain data.table pointers", {
  input <- .fishstat_candidate_fixture()
  input$trade <- data.table::as.data.table(input$trade)
  out <- do.call(whep:::.prepare_fishstat_candidate, input)
  testthat::expect_null(attr(out, ".internal.selfref"))
  testthat::expect_equal(out$Value, input$trade$VALUE)
})

testthat::test_that("empty candidates retain the complete typed schema", {
  input <- .fishstat_candidate_fixture()
  input$trade <- input$trade[0, ]
  out <- do.call(whep:::.prepare_fishstat_candidate, input)
  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_true(all(
    c(
      "Artifact Type",
      "Weight Basis",
      "Source Commodity Code",
      "Source Flag",
      "Polity Code",
      "Coverage Status",
      "Classification Status",
      "Value"
    ) %in%
      names(out)
  ))
})

testthat::test_that("provenance fingerprints runtime tables, not ambient files", {
  before <- whep:::.fishstat_runtime_provenance()
  withr::local_envvar(WHEP_POLITIES_FAOSTAT_MAP = "unused-build-input.csv")
  after <- whep:::.fishstat_runtime_provenance()
  testthat::expect_identical(before, after)
  testthat::expect_equal(before$table_rows$polities, nrow(whep::polities))
  testthat::expect_gt(length(before$package_data_files), 0L)
  table <- tibble::tibble(polity_code = "fixture-a")
  changed <- tibble::tibble(polity_code = "fixture-b")
  testthat::expect_false(identical(
    whep:::.fishstat_table_hash(table),
    whep:::.fishstat_table_hash(changed)
  ))
})
