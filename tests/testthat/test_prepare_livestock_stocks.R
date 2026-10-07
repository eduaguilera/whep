# Offline tests for the national livestock stocks Section 8 of
# inst/scripts/prepare_spatialize_all.R groups into the
# `spatialize-livestock-country-data` pin.
#
# Issue whep#1274: the registered pin carried 48.3 M equine head for 2020
# where production carries 115.5 M. The heads were built before whep#1106
# restored the asses, mules and horses of countries that report no meat for
# them, and the pin was never rebuilt. Rebuilding it from today's production
# would still have dropped one herd: the 94 M breeding swine (1051) whep#1153
# restored, which `livestock_mapping.csv` did not name.

.source_prepare_spatialize()

.stock_mapping <- function() {
  # system.file(), not a path relative to tests/: covr and R CMD check run
  # the tests against the installed package, where ../../inst does not exist.
  readr::read_csv(
    system.file("extdata", "livestock_mapping.csv", package = "whep"),
    show_col_types = FALSE
  )
}

# Two areas, one year, the items whep#1274 is about. Values are arbitrary;
# only their propagation is under test.
.stock_prod <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit,     ~value,
    2020L,        1L,          "1049",  "heads",   900,
    2020L,        1L,          "1051",  "heads",   100,
    2020L,        1L,          "1096",  "heads",    50,
    2020L,        1L,          "1107",  "heads",    30,
    2020L,        1L,          "1110",  "heads",    20,
    2020L,        1L,          "1181",  "heads",     7,
    2020L,        2L,          "1107",  "heads",    40,
    2020L,        2L,          "1107",  "LU",       20,
    2020L,        2L,          "1096",  "heads",     0
  )
}

test_that("breeding swine reach the pigs group", {
  .need_spatialize_helper(".livestock_country_stocks")
  out <- .livestock_country_stocks(.stock_prod(), .stock_mapping())
  pigs <- dplyr::filter(out, species_group == "pigs")
  # 900 market + 100 breeding. Without the 1051 row the mapping drops the
  # breeding herd, which is 10% of every FAOSTAT swine total.
  expect_equal(pigs$heads, 1000)
})

test_that("every equine member is summed, with or without meat", {
  .need_spatialize_helper(".livestock_country_stocks")
  out <- .livestock_country_stocks(.stock_prod(), .stock_mapping())
  equines <- out |>
    dplyr::filter(species_group == "equines") |>
    dplyr::arrange(area_code)
  # Area 2 reports asses only, and a zero-head horse row and an LU row that
  # must not be counted as heads.
  expect_equal(equines$area_code, c(1L, 2L))
  expect_equal(equines$heads, c(100, 40))
})

test_that("a declared non-livestock item is left out, not an abort", {
  .need_spatialize_helper(".livestock_country_stocks")
  out <- .livestock_country_stocks(.stock_prod(), .stock_mapping())
  expect_false(any(out$species_group == "Beehives"))
  expect_equal(sum(out$heads), 1000 + 100 + 40)
  expect_true(1181L %in% .livestock_unspatialized_items()$item_code)
  expect_false(any(
    .livestock_unspatialized_items()$item_code %in% .stock_mapping()$item_code
  ))
})

test_that("a head item the mapping does not know aborts", {
  .need_spatialize_helper(".livestock_country_stocks")
  mapping <- dplyr::filter(.stock_mapping(), item_code != 1051L)
  expect_error(
    .livestock_country_stocks(.stock_prod(), mapping),
    class = "whep_livestock_unmapped_item"
  )
  expect_error(
    .livestock_country_stocks(.stock_prod(), mapping),
    "1051 \\(100 head-years\\)"
  )
})

test_that("the conservation check passes on the grouped stocks", {
  .need_spatialize_helper(".check_livestock_heads_conserved")
  prod <- .stock_prod()
  out <- .livestock_country_stocks(prod, .stock_mapping())
  expect_no_error(
    .check_livestock_heads_conserved(out, prod, .stock_mapping())
  )
})

test_that("the conservation check fires on a duplicating join", {
  .need_spatialize_helper(".check_livestock_heads_conserved")
  prod <- .stock_prod()
  out <- .livestock_country_stocks(prod, .stock_mapping())
  # The shape of a left join onto a table with a repeated key.
  duplicated <- dplyr::bind_rows(
    out,
    dplyr::filter(out, species_group == "pigs")
  )
  expect_error(
    .check_livestock_heads_conserved(duplicated, prod, .stock_mapping()),
    class = "whep_livestock_heads_not_conserved"
  )
})

test_that("the conservation check fires on a dropped group", {
  .need_spatialize_helper(".check_livestock_heads_conserved")
  prod <- .stock_prod()
  out <- .livestock_country_stocks(prod, .stock_mapping())
  dropped <- dplyr::filter(out, species_group != "equines")
  # The identity `sum(heads) == sum(heads)` would hold on the dropped table
  # against itself; the check is against production, which still has them.
  expect_error(
    .check_livestock_heads_conserved(dropped, prod, .stock_mapping()),
    "equines"
  )
})

# Sudan in the two vocabularies, two years either side of the 2011 secession.
# Under the un-fold the retired bucket 206 is carried forward beside its
# successors; under the default fold it is their sum and they are absent.
.sudan_buckets <- function() {
  tibble::tribble(
    ~bucket, ~last_year, ~member,
    206L,    2011L,      276L,
    206L,    2011L,      277L
  )
}

.sudan_prod_unfolded <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit,   ~value,
    2011L,      206L,           "960", "heads",     10,
    2012L,      206L,           "960", "heads",     10,
    2012L,      206L,           "960", "LU",         7,
    2012L,      276L,           "960", "heads",      8,
    2012L,      277L,           "960", "heads",      3
  )
}

test_that("the bucket table names Sudan's successors and its last year", {
  .need_spatialize_helper(".livestock_predecessor_buckets")
  buckets <- .livestock_predecessor_buckets()
  sudan <- dplyr::filter(buckets, bucket == 206L)
  expect_setequal(sudan$member, c(276L, 277L))
  expect_equal(unique(sudan$last_year), 2011L)
})

test_that("a retired bucket carried past its end is dropped", {
  .need_spatialize_helper(".livestock_reporting_areas")
  expect_message(
    out <- .livestock_reporting_areas(
      .sudan_prod_unfolded(),
      .sudan_buckets()
    ),
    "retired predecessor bucket"
  )
  # 2011 keeps 206; 2012 keeps only the successors, in every unit.
  expect_equal(out$area_code[out$year == 2011L], 206L)
  expect_setequal(out$area_code[out$year == 2012L], c(276L, 277L))
  expect_equal(sum(out$value[out$unit == "heads" & out$year == 2012L]), 11)
})

test_that("a production table in the default fold is refused", {
  .need_spatialize_helper(".livestock_reporting_areas")
  folded <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~unit,   ~value,
    2011L,      206L,           "960", "heads",     10,
    2012L,      206L,           "960", "heads",     11
  )
  # Dropping 206 after 2011 here would delete Sudan outright.
  expect_error(
    .livestock_reporting_areas(folded, .sudan_buckets()),
    class = "whep_livestock_folded_production"
  )
})

test_that("a table that never reaches the bucket's end is untouched", {
  .need_spatialize_helper(".livestock_reporting_areas")
  prod <- .stock_prod()
  expect_identical(.livestock_reporting_areas(prod, .sudan_buckets()), prod)
})
