# Partial source coverage must be decided before source selection (whep#1481).
.coverage_cbs_inputs <- function() {
  old <- tibble::tribble(
    ~year, ~area_code, ~area, ~item_cbs_code,
    ~item_cbs, ~element, ~unit, ~value,
    2011L, 206L, "Combined", 2511L, "Wheat", "food", "tonnes", 60,
    2012L, 206L, "Combined", 2511L, "Wheat", "food", "tonnes", 20,
    2013L, 206L, "Combined", 2511L, "Wheat", "food", "tonnes", 30
  )
  attr(old, "bucket_members") <- tibble::tribble(
    ~year, ~polity_area_code, ~area_code,
    2011L, 206L, 206L,
    2012L, 206L, 276L,
    2013L, 206L, 276L,
    2013L, 206L, 277L
  )
  empty <- old[0L, ]
  attr(empty, "bucket_members") <- NULL
  list(
    fbs_old = old,
    fbs_new = empty,
    cbs_crops = empty,
    cbs_animals = empty,
    cbs_new = empty,
    fao_trade = empty,
    primary_cbs = dplyr::mutate(old, element = "production", value = 100),
    crop_residues = empty,
    trade_hist = empty,
    fbs_rebase = NULL
  )
}

.local_cbs_members <- function() {
  testthat::local_mocked_bindings(
    .resolve_all_area_years = \(years) {
      tibble::tribble(
      ~year, ~polity_area_code, ~area_code, ~mapping_status,
      2011L, 206L, 206L, "matched",
      2012L, 206L, 276L, "matched",
      2012L, 206L, 277L, "matched",
      2013L, 206L, 276L, "matched",
      2013L, 206L, 277L, "matched"
    ) |>
        dplyr::mutate(first_reported_year = NA, last_reported_year = NA)
    },
    .package = "whep",
    .env = parent.frame()
  )
}

testthat::test_that("partial uses cannot be selected as the whole bucket", {
  .local_cbs_members()
  inputs <- .coverage_cbs_inputs()
  testthat::local_mocked_bindings(
    .fix_palm_kernels = \(inputs) data.table::copy(inputs$fbs_new),
    .get_traded_residues = \(...) data.table::copy(inputs$fbs_new),
    .get_fiber_tobacco = \(...) data.table::copy(inputs$fbs_new),
    .package = "whep"
  )
  selected <- whep:::.cbs_combine_sources(inputs)
  # The source's own totals reconcile while its 2012 row covers Sudan alone.
  testthat::expect_equal(sum(inputs$fbs_old$value), 110)
  testthat::expect_false(any(
    selected$year == 2012 & selected$element == "food"
  ))
  testthat::expect_equal(
    dplyr::filter(selected, element == "food") |> dplyr::pull(value),
    c(60, 30)
  )
  testthat::expect_equal(
    dplyr::filter(selected, element == "production") |> dplyr::pull(value),
    rep(100, 3)
  )
})


testthat::test_that("coverage decisions are per source and exact year", {
  .local_cbs_members()
  inputs <- .coverage_cbs_inputs()
  # The crops source reports all members in 2012; the old FBS does not.
  inputs$cbs_crops <- inputs$fbs_old
  attr(inputs$cbs_crops, "bucket_members") <- dplyr::bind_rows(
    attr(inputs$cbs_crops, "bucket_members"),
    tibble::tibble(year = 2012L, polity_area_code = 206L, area_code = 277L)
  )
  out <- whep:::.cbs_coverage_inputs(inputs, "complete_sources")
  testthat::expect_equal(out$fbs_old$year, c(2011L, 2013L))
  testthat::expect_equal(out$cbs_crops$year, 2011:2013)
  testthat::expect_identical(out$primary_cbs, inputs$primary_cbs)
  log <- attr(out, "coverage_log")
  testthat::expect_equal(log$source, "fbs_old")
  testthat::expect_equal(log$year, 2012L)
  testthat::expect_equal(log$covered, "276")
  testthat::expect_equal(log$missing, "277")
})

testthat::test_that("world shares omit partial sources and record the method", {
  .local_cbs_members()
  inputs <- .coverage_cbs_inputs()
  out <- whep:::.cbs_coverage_inputs(inputs, "world_shares")
  testthat::expect_equal(out$fbs_old$year, c(2011L, 2013L))
  stamped <- whep:::.cbs_stamp_coverage(
    inputs$primary_cbs,
    attr(out, "coverage_log")
  )
  testthat::expect_equal(
    stamped$method_cbs_coverage,
    c("reported_members", "world_shares", "reported_members")
  )
  testthat::expect_equal(attr(stamped, "cbs_coverage_log")$missing, "277")
  testthat::expect_error(
    whep:::.cbs_coverage_inputs(inputs, "guess"),
    class = "rlang_error"
  )
})

testthat::test_that("absence of member metadata is not labelled complete", {
  inputs <- .coverage_cbs_inputs()
  attr(inputs$fbs_old, "bucket_members") <- NULL
  out <- whep:::.cbs_coverage_inputs(inputs, "complete_sources")
  testthat::expect_identical(out, inputs)
  stamped <- whep:::.cbs_stamp_coverage(inputs$primary_cbs, NULL)
  testthat::expect_equal(unique(stamped$method_cbs_coverage), "not_checked")
})

testthat::test_that("real Sudan membership excludes the retired predecessor", {
  members <- whep:::.resolve_all_area_years(2011:2013) |>
    whep:::.in_span_members() |>
    dplyr::filter(polity_area_code == 206L)
  old <- tibble::tribble(
    ~year, ~polity_area_code, ~area_code,
    2011L, 206L, 206L,
    2012L, 206L, 276L,
    2013L, 206L, 276L
  )
  log <- whep:::.cbs_partial_years(members, old)
  testthat::expect_equal(log$year, 2012:2013)
  testthat::expect_equal(log$missing, rep("277", 2))
})


testthat::test_that("estimated bucket uses cannot claim a member's flag", {
  data <- tibble::tribble(
    ~year, ~area_code, ~element, ~value, ~fao_flag,
    2012L, 206L, "production", 100, "A",
    2012L, 206L, "food",        60, "S",
    2011L, 206L, "food",        50, "S"
  )
  log <- tibble::tibble(
    year = 2012L,
    polity_area_code = 206L,
    method = "world_shares"
  )
  out <- whep:::.cbs_stamp_coverage(data, log)
  testthat::expect_equal(out$value, data$value)
  testthat::expect_equal(out$fao_flag, c("A", NA_character_, "S"))
  log$method <- "complete_sources"
  testthat::expect_equal(
    whep:::.cbs_stamp_coverage(data, log)$fao_flag,
    data$fao_flag
  )
})

testthat::test_that("whole-bucket anchors supply a missing destiny pattern", {
  .local_cbs_members()
  inputs <- .coverage_cbs_inputs()
  # The destiny estimator's commodity lookup expects the canonical label.
  inputs$fbs_old$item_cbs <- "Wheat and products"
  inputs$primary_cbs$item_cbs <- "Wheat and products"
  members <- attr(inputs$fbs_old, "bucket_members")
  inputs$fbs_old <- dplyr::bind_rows(
    inputs$fbs_old,
    dplyr::mutate(inputs$fbs_old, element = "feed", value = 100 - value)
  )
  attr(inputs$fbs_old, "bucket_members") <- members
  testthat::local_mocked_bindings(
    .fix_palm_kernels = \(inputs) data.table::copy(inputs$fbs_new),
    .get_traded_residues = \(...) data.table::copy(inputs$fbs_new),
    .get_fiber_tobacco = \(...) data.table::copy(inputs$fbs_new),
    .package = "whep"
  )
  complete <- whep:::.cbs_combine_sources(inputs, "complete_sources")
  world_selected <- whep:::.cbs_combine_sources(inputs, "world_shares")
  supply <- dplyr::mutate(
    inputs$primary_cbs,
    element = "domestic_supply",
    source = "FAOSTAT_prod"
  )
  filled <- dplyr::bind_rows(complete, supply) |>
    whep:::.cbs_fill_destinies()
  donor <- tibble::tribble(
    ~year, ~area_code, ~area, ~item_cbs_code, ~item_cbs, ~element, ~value,
    2012L, 203L, "Spain", 2511L, "Wheat and products", "domestic_supply", 100,
    2012L, 203L, "Spain", 2511L, "Wheat and products", "food", 60,
    2012L, 203L, "Spain", 2511L, "Wheat and products", "feed", 30,
    2012L, 203L, "Spain", 2511L, "Wheat and products", "other_uses", 10
  ) |>
    dplyr::mutate(source = "FAOSTAT_FBS_Old")
  sensitivity <- dplyr::bind_rows(world_selected, supply, donor) |>
    whep:::.cbs_fill_destinies(attr(world_selected, "coverage_log"))
  testthat::expect_equal(
    dplyr::filter(filled, year == 2012, element == "food") |>
      dplyr::pull(value),
    45
  )
  testthat::expect_equal(
    dplyr::filter(
      sensitivity,
      year == 2012,
      area_code == 206,
      element == "food"
    ) |>
      dplyr::pull(value),
    60
  )
  world_uses <- dplyr::filter(
    sensitivity,
    area_code == 206,
    year == 2012,
    element %in% c("food", "feed", "other_uses")
  )
  testthat::expect_equal(sum(world_uses$value), 100)
  testthat::expect_equal(
    dplyr::filter(world_uses, element == "other_uses") |> dplyr::pull(value),
    10
  )
  expect_supplied_guard(
    identity = sum(
      dplyr::filter(filled, element %in% c("food", "feed"))$value
    ) ==
      sum(supply$value),
    guard = whep:::.cbs_fill_destinies(
      dplyr::bind_rows(world_selected, supply),
      attr(world_selected, "coverage_log")
    ),
    class = "whep_cbs_world_profile_absent"
  )
  uses <- dplyr::filter(filled, element %in% c("food", "feed")) |>
    dplyr::summarise(value = sum(value), .by = year)
  testthat::expect_equal(uses$value, rep(100, 3))
})


testthat::test_that("world shares retain structural processing-residue feed", {
  data <- tibble::tribble(
    ~year, ~area_code, ~area, ~item_cbs_code, ~item_cbs, ~element, ~value,
    2012L, 206L, "Combined", 2104L, "Sugarbeet pulp", "domestic_supply", 100,
    2012L, 206L, "Combined", 2104L, "Sugarbeet pulp", "feed", 0
  ) |>
    dplyr::mutate(source = "processed")
  log <- tibble::tibble(
    year = 2012L,
    polity_area_code = 206L,
    method = "world_shares"
  )
  out <- whep:::.cbs_fill_destinies(data, log)
  testthat::expect_equal(
    dplyr::filter(out, element == "feed") |> dplyr::pull(value),
    100
  )
})


testthat::test_that("old cached extracts are refreshed before source selection", {
  inputs <- .coverage_cbs_inputs()
  extracts <- inputs[c("fbs_new", "fbs_old", "cbs_crops", "cbs_animals")]
  primary <- inputs$primary_cbs
  attr(primary, ".cb_extracts") <- extracts
  testthat::local_mocked_bindings(
    .extract_cb = \(pin_alias, years, keep_members) {
      testthat::expect_true(keep_members)
      cli::cli_abort(
        "Refresh the old extract",
        class = "coverage_refresh_probe"
      )
    },
    .package = "whep"
  )
  testthat::expect_error(
    whep:::.cbs_read_inputs(primary, 2011:2013),
    class = "coverage_refresh_probe"
  )
  testthat::expect_false(whep:::.cbs_reuse_extracts(NULL))
  testthat::expect_false(whep:::.cbs_reuse_extracts(extracts))
  extracts <- purrr::map(extracts, \(data) {
    attr(data, "bucket_members") <- tibble::tibble(
      year = integer(),
      polity_area_code = integer(),
      area_code = integer()
    )
    data
  })
  testthat::expect_true(whep:::.cbs_reuse_extracts(extracts))
})

testthat::test_that("commodity transformations preserve source-member coverage", {
  raw <- tibble::tribble(
    ~year, ~area_code, ~area, ~item_cbs_code, ~element, ~value,
    2012L, 206L, "Combined", 826L, "other_uses", 20,
    2012L, 206L, "Combined", 831L, "production", 10
  ) |>
    dplyr::mutate(unit = "tonnes", item_cbs = "Tobacco", fao_flag = "A")
  members <- tibble::tribble(
    ~year, ~polity_area_code, ~area_code,
    2012L, 206L, 276L
  )
  attr(raw, "bucket_members") <- members

  silk <- whep:::.cbs_silk_mass_basis(raw)
  tobacco <- whep:::.cbs_tobacco_leaf_use(silk, "one_to_one")

  testthat::expect_identical(attr(silk, "bucket_members"), members)
  testthat::expect_identical(attr(tobacco, "bucket_members"), members)
  out <- whep:::.cbs_coverage_inputs(
    list(cbs_new = tobacco),
    "complete_sources"
  )
  testthat::expect_equal(nrow(out$cbs_new), 0L)
  testthat::expect_equal(attr(out, "coverage_log")$missing, "277")
})
