# Source classifications, not conversion factors. FAO Yearbook2021, A.8,
# printed p.204; FI_Trade2025.1.0 codelists. Actual source codes are fixtures.

.fishstat_species_bridge <- function() {
  readr::read_csv(
    system.file(
      "extdata",
      "harmonization",
      "fishstat_cbs_bridge.csv",
      package = "whep"
    ),
    show_col_types = FALSE
  )
}

testthat::test_that("species groups match the source classification", {
  bridge <- .fishstat_species_bridge()
  groups <- c(21L, 25L, 31L, 32L, 33L, 38L, 76L, 77L, 91L, 94L)
  expected <- c(
    2761L,
    2761L,
    2762L,
    2762L,
    2762L,
    2762L,
    2769L,
    2769L,
    2775L,
    2775L
  )
  got <- bridge$item_cbs_code[match(groups, bridge$isscaap_group)]
  testthat::expect_equal(got, expected)
})

testthat::test_that("commodity form separates oils from source species", {
  commodities <- tibble::tribble(
    ~Code,             ~ISSCAAP,
    "034.1.5.2.19",     32L,
    "411.11.5.2.19",    32L,
    "411.12.5.5.19",    35L,
    "081.42.1.5.2.19",  32L,
    "292.9.1.21",       92L
  )
  out <- whep:::.fishstat_classify(commodities, .fishstat_species_bridge())
  testthat::expect_equal(nrow(out), nrow(commodities))
  testthat::expect_setequal(out$fao_code, commodities$Code)
  testthat::expect_equal(
    out$product_form,
    c("aquatic_product", "liver_oil", "body_oil", "meal", "plant_product")
  )
  testthat::expect_equal(
    out$item_cbs_code,
    c(2762L, 2782L, 2781L, 2762L, 2775L)
  )
})

testthat::test_that("unsupported scope is retained rather than misclassified", {
  commodities <- tibble::tribble(
    ~Code,                ~ISSCAAP,
    "411.13.1.10",        61L,
    "291.1.5.21",         81L,
    "081.42.1.5.9.99",    39L,
    "411.14.1.70",        57L,
    "292.9.1.79",         94L
  )
  out <- whep:::.fishstat_classify(commodities, .fishstat_species_bridge())
  testthat::expect_setequal(out$fao_code, commodities$Code)
  testthat::expect_true(all(is.na(out$item_cbs_code)))
  testthat::expect_true(all(out$classification_status == "held_scope"))
})

testthat::test_that("new source codes and mapping gaps stay visible", {
  commodities <- tibble::tribble(
    ~Code,                ~ISSCAAP,
    "034.5",              39L,
    "036.0.2.3.5.951",    55L,
    "036.0.3.5.10",       47L,
    "999.1",              35L,
    "034.9.99",           99L
  )
  out <- whep:::.fishstat_classify(commodities, .fishstat_species_bridge())
  testthat::expect_equal(nrow(out), 5L)
  testthat::expect_equal(out$item_cbs_code[1:3], c(2764L, 2767L, 2765L))
  testthat::expect_equal(
    out$classification_status[4:5],
    c("unmapped_form", "unmapped_group")
  )
  testthat::expect_true(all(is.na(out$item_cbs_code[4:5])))
})

testthat::test_that("classification keys cannot drop or fan out commodities", {
  commodity <- tibble::tibble(Code = "034.5", ISSCAAP = 39L)
  bridge <- .fishstat_species_bridge()
  testthat::expect_error(
    whep:::.fishstat_classify(dplyr::bind_rows(commodity, commodity), bridge),
    class = "whep_fishstat_classification"
  )
  testthat::expect_error(
    whep:::.fishstat_classify(commodity, dplyr::bind_rows(bridge, bridge[1, ])),
    class = "whep_fishstat_classification"
  )
  testthat::expect_error(
    whep:::.fishstat_classify(commodity["Code"], bridge),
    class = "whep_fishstat_classification"
  )
  commodity$Code <- NA_character_
  testthat::expect_error(
    whep:::.fishstat_classify(commodity, bridge),
    class = "whep_fishstat_classification"
  )
  commodity$Code <- "034.5"
  bridge$classification_status[1] <- "invented"
  testthat::expect_error(
    whep:::.fishstat_classify(commodity, bridge),
    class = "whep_fishstat_classification"
  )
})

testthat::test_that("the source codelist heading is retained as unmapped", {
  commodities <- tibble::tibble(Code = "03", ISSCAAP = NA_character_)
  out <- whep:::.fishstat_classify(commodities, .fishstat_species_bridge())
  testthat::expect_equal(out$fao_code, "03")
  testthat::expect_equal(out$classification_status, "unmapped_group")
  testthat::expect_true(is.na(out$item_cbs_code))
})

testthat::test_that("raw classification preserves flags, quantities and gaps", {
  trade <- tibble::tribble(
    ~TRADE_FLOW.ALPHA_CODE, ~COUNTRY_REPORTER.UN_CODE,
    ~COMMODITY.FAO_CODE, ~MEASURE, ~PERIOD, ~STATUS, ~VALUE,
    "I", "004", "411.11.5.2.19", "Q_tpw", 2023L, "X", 2.5,
    "E", "004", "999.99", "Q_tpw", 2023L, "E", NA_real_,
    "R", "004", "411.11.5.2.19", "Q_tpw", 2023L, "X", 3,
    "I", "004", "411.11.5.2.19", "Q_tlw", 2023L, "X", 8
  )
  commodities <- tibble::tibble(Code = "411.11.5.2.19", ISSCAAP = 32L)
  out <- whep:::.fishstat_candidate_items(
    trade,
    commodities,
    .fishstat_species_bridge()
  )
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(out$source_flag, c("X", "E"))
  testthat::expect_equal(out$source_measure, c("Q_tpw", "Q_tpw"))
  testthat::expect_equal(out$value, c(2.5, NA_real_))
  testthat::expect_equal(out$fao_code, c("411.11.5.2.19", "999.99"))
  testthat::expect_equal(
    out$classification_status,
    c("classified", "unmapped_commodity")
  )
  testthat::expect_error(
    whep:::.fishstat_candidate_items(
      dplyr::select(trade, -"STATUS"),
      commodities,
      .fishstat_species_bridge()
    ),
    class = "whep_fishstat_source_schema"
  )
})
