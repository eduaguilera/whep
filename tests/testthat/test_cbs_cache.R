# Cached production extracts must not choose the palm-kernel anchor (whep#1624).
.cache_palm_sources <- function() {
  old <- tibble::tribble(
    ~year, ~item_cbs_code, ~item_cbs, ~element, ~value,
    2009L, 2562L, "Palm kernels", "processing", 80,
    2009L, 2562L, "Palm kernels", "food", 8,
    2009L, 2562L, "Palm kernels", "other_uses", 16,
    2009L, 2576L, "Palmkernel Oil", "production", 40,
    2010L, 2576L, "Palmkernel Oil", "production", 42,
    2011L, 2576L, "Palmkernel Oil", "production", 44,
    2012L, 2576L, "Palmkernel Oil", "production", 48,
    2013L, 2562L, "Palm kernels", "processing", 100,
    2013L, 2562L, "Palm kernels", "food", 10,
    2013L, 2562L, "Palm kernels", "other_uses", 20,
    2013L, 2576L, "Palmkernel Oil", "production", 50
  ) |>
    dplyr::mutate(area = "Spain", area_code = 203L, unit = "tonnes")
  new <- tibble::tribble(
    ~year, ~item_cbs_code, ~item_cbs, ~element, ~value,
    2014L, 2576L, "Palmkernel Oil", "production", 75,
    2015L, 2576L, "Palmkernel Oil", "production", 100,
    2016L, 2576L, "Palmkernel Oil", "production", 125
  ) |>
    dplyr::mutate(area = "Spain", area_code = 203L, unit = "tonnes")
  list(`faostat-fbs-old` = old, `faostat-fbs-new` = new)
}

.cache_extract_fixture <- function(pin_alias, years = NULL, ...) {
  sources <- .cache_palm_sources()
  rows <- sources[[pin_alias]] %||% sources[[1]][0, ]
  if (!is.null(years)) {
    rows <- dplyr::filter(rows, year %in% years)
  }
  attr(rows, "bucket_members") <- rows |>
    dplyr::distinct(year, area_code) |>
    dplyr::mutate(polity_area_code = area_code)
  attr(rows, "source_accounts") <- tibble::tibble()
  attr(rows, "stock_convention") <- "addition_positive"
  attr(rows, "cbs_extract_query") <- list(
    pin_alias = pin_alias,
    all_years = is.null(years),
    years = years,
    elements = NULL
  )
  rows
}

.local_cache_input_reads <- function() {
  empty <- .cache_palm_sources()[[1]][0, ]
  testthat::local_mocked_bindings(
    .extract_cb = .cache_extract_fixture,
    .extract_fao = \(...) empty,
    .cbs_silk_mass_basis = \(data, ...) data,
    .cbs_tobacco_leaf_use = \(data, ...) data,
    .read_fao_trade = \(...) empty,
    .trade_silk_mass_basis = \(data, ...) data,
    .read_fishstat_trade = \(...) empty,
    .read_historical_trade = \(...) empty,
    .read_gdp_pop = \(...) empty,
    .primary_to_cbs = \(...) empty,
    .primary_to_cbs_area = \(...) empty,
    get_primary_residues = \(...) empty,
    .read_crop_residues = \(...) empty,
    .cbs_residue_backcast = \(...) empty,
    .residue_feed_kgdm = \(...) empty,
    .read_land_areas_wide = \(...) empty,
    .package = "whep",
    .env = parent.frame()
  )
}

testthat::test_that("palm-kernel CBS does not depend on production cache", {
  .local_cache_input_reads()
  reads <- new.env(parent = emptyenv())
  reads$calls <- list()
  testthat::local_mocked_bindings(
    .extract_cb = \(pin_alias, years = NULL, ...) {
      reads$calls <- append(
        reads$calls,
        list(list(
          pin_alias = pin_alias,
          years = years
        ))
      )
      .cache_extract_fixture(pin_alias, years)
    },
    .package = "whep"
  )
  primary <- tibble::tibble()
  attr(primary, ".cb_extracts") <- list(
    fbs_old = .cache_extract_fixture("faostat-fbs-old", 2012:2018),
    fbs_new = .cache_extract_fixture("faostat-fbs-new", 2012:2018),
    cbs_crops = .cache_extract_fixture("faostat-cbs-old-crops", 2012:2018),
    cbs_animals = .cache_extract_fixture("faostat-cbs-old-animal", 2012:2018)
  )
  testthat::expect_true(
    whep:::.cbs_reuse_extracts(attr(primary, ".cb_extracts"))
  )
  frozen <- serialize(primary, NULL)
  cached <- whep:::.cbs_read_inputs(
    primary,
    2015L,
    fbs_trade_basis = "as_published"
  )
  testthat::expect_length(reads$calls, 1L)
  testthat::expect_identical(reads$calls[[1]]$pin_alias, "faostat-fbs-old")
  testthat::expect_null(reads$calls[[1]]$years)
  reads$calls <- list()
  attr(primary, ".cb_extracts") <- NULL
  reread <- whep:::.cbs_read_inputs(
    primary,
    2015L,
    fbs_trade_basis = "as_published"
  )
  testthat::expect_length(reads$calls, 6L)
  testthat::expect_setequal(
    purrr::map_chr(reads$calls, "pin_alias"),
    c(
      "faostat-fbs-new",
      "faostat-fbs-old",
      "faostat-cbs-old-crops",
      "faostat-cbs-old-animal"
    )
  )
  cached_balance <- whep:::.fix_palm_kernels(cached) |>
    dplyr::filter(year == 2015)
  fresh_balance <- whep:::.fix_palm_kernels(reread) |>
    dplyr::filter(year == 2015)
  testthat::expect_equal(fresh_balance, cached_balance)
  testthat::expect_equal(
    dplyr::filter(fresh_balance, year == 2015, element == "processing") |>
      dplyr::pull(value),
    200
  )
  # A cache read must not mutate the production object supplied by its caller.
  restored <- unserialize(frozen)
  whep:::.cbs_read_inputs(restored, 2015L, fbs_trade_basis = "as_published")
  testthat::expect_identical(serialize(restored, NULL), frozen)
})

testthat::test_that("palm support keeps earlier genuine old-vintage anchors", {
  testthat::local_mocked_bindings(
    .extract_cb = \(pin_alias, years = NULL, ...) {
      rows <- .cache_extract_fixture(pin_alias, years)
      if (pin_alias == "faostat-fbs-old") {
        rows <- dplyr::filter(
          rows,
          year != 2013L | item_cbs != "Palm kernels"
        )
      }
      rows
    },
    .package = "whep"
  )
  inputs <- list(
    fbs_old = .cache_extract_fixture("faostat-fbs-old", 2015L),
    fbs_new = .cache_extract_fixture("faostat-fbs-new", 2015L)
  )
  support <- whep:::.cbs_palm_support(inputs, 2015L)
  testthat::expect_true(2009L %in% support$fbs_old$year)
  actual <- whep:::.fix_palm_kernels(support) |>
    dplyr::filter(year == 2015L, element == "processing")
  testthat::expect_equal(actual$value, 200)
})

testthat::test_that("sparse requested years retain the annual proxy bridge", {
  testthat::local_mocked_bindings(
    .extract_cb = .cache_extract_fixture,
    .package = "whep"
  )
  inputs <- list(
    fbs_old = .cache_extract_fixture("faostat-fbs-old", 2015L),
    fbs_new = .cache_extract_fixture("faostat-fbs-new", c(2015L, 2017L))
  )
  support <- whep:::.cbs_palm_support(inputs, c(2015L, 2017L))
  testthat::expect_identical(
    attr(support$fbs_new, "cbs_extract_query")$years,
    2014:2017
  )
  testthat::expect_true(2014L %in% support$fbs_new$year)
  testthat::expect_true(2016L %in% support$fbs_new$year)
})

testthat::test_that("pre-2014 requests do not fetch palm extrapolation support", {
  testthat::local_mocked_bindings(
    .extract_cb = \(...) cli::cli_abort("Unexpected palm support read"),
    .package = "whep"
  )
  testthat::expect_null(whep:::.cbs_palm_support(list(), 2000:2003))
  testthat::expect_null(whep:::.cbs_palm_support(list(), 2010:2013))
})

testthat::test_that("queried-empty support differs from absent query provenance", {
  empty <- .cache_extract_fixture("faostat-fbs-old", NULL)[0, ]
  attr(empty, "cbs_extract_query") <- list(
    pin_alias = "faostat-fbs-old",
    all_years = TRUE,
    years = NULL,
    elements = NULL
  )
  testthat::local_mocked_bindings(
    .extract_cb = \(...) {
      cli::cli_abort(
        "Unsupplied support refreshed",
        class = "cbs_cache_refresh_probe"
      )
    },
    .package = "whep"
  )
  supplied <- whep:::.cbs_palm_source(empty, "faostat-fbs-old", NULL)
  testthat::expect_equal(nrow(supplied), 0L)
  testthat::expect_true(attr(supplied, "cbs_extract_query")$all_years)
  attr(empty, "cbs_extract_query") <- NULL
  testthat::expect_error(
    whep:::.cbs_palm_source(empty, "faostat-fbs-old", NULL),
    class = "cbs_cache_refresh_probe"
  )
  attr(empty, "cbs_extract_query") <- list(
    pin_alias = "faostat-fbs-old",
    all_years = TRUE,
    years = NULL,
    elements = "production"
  )
  testthat::expect_error(
    whep:::.cbs_palm_source(empty, "faostat-fbs-old", NULL),
    class = "cbs_cache_refresh_probe"
  )
  attr(empty, "cbs_extract_query") <- list(
    pin_alias = "faostat-fbs-new",
    all_years = TRUE,
    years = NULL,
    elements = NULL
  )
  testthat::expect_error(
    whep:::.cbs_palm_source(empty, "faostat-fbs-old", NULL),
    class = "cbs_cache_refresh_probe"
  )
})

testthat::test_that("selected CBS values agree on cached and fresh inputs", {
  .local_cache_input_reads()
  empty <- .cache_palm_sources()[[1]][0, ]
  testthat::local_mocked_bindings(
    .get_traded_residues = \(...) {
      data.table::copy(data.table::as.data.table(empty))
    },
    .get_fiber_tobacco = \(...) {
      data.table::copy(data.table::as.data.table(empty))
    },
    .package = "whep"
  )
  primary <- tibble::tibble()
  attr(primary, ".cb_extracts") <- list(
    fbs_old = .cache_extract_fixture("faostat-fbs-old", 2012:2018),
    fbs_new = .cache_extract_fixture("faostat-fbs-new", 2012:2018),
    cbs_crops = .cache_extract_fixture("faostat-cbs-old-crops", 2012:2018),
    cbs_animals = .cache_extract_fixture("faostat-cbs-old-animal", 2012:2018)
  )
  cached <- whep:::.cbs_read_inputs(
    primary,
    2015L,
    fbs_trade_basis = "as_published"
  ) |>
    whep:::.cbs_combine_sources()
  attr(primary, ".cb_extracts") <- NULL
  fresh <- whep:::.cbs_read_inputs(
    primary,
    2015L,
    fbs_trade_basis = "as_published"
  ) |>
    whep:::.cbs_combine_sources()
  testthat::expect_equal(
    dplyr::filter(fresh, year == 2015L),
    dplyr::filter(cached, year == 2015L),
    ignore_attr = TRUE
  )
})

testthat::test_that("extract query provenance survives a genuinely empty read", {
  testthat::local_mocked_bindings(
    .extract_fao = \(...) {
      .cache_palm_sources()[[1]][0, ] |> data.table::as.data.table()
    },
    .package = "whep"
  )
  empty <- whep:::.extract_cb("faostat-fbs-old", years = 2015:2020)
  testthat::expect_equal(nrow(empty), 0L)
  testthat::expect_identical(
    attr(empty, "cbs_extract_query"),
    list(
      pin_alias = "faostat-fbs-old",
      all_years = FALSE,
      years = 2015:2020,
      elements = NULL
    )
  )
  all <- whep:::.extract_cb("faostat-fbs-old")
  testthat::expect_true(attr(all, "cbs_extract_query")$all_years)
})

testthat::test_that("dedicated palm support respects source-member coverage", {
  old <- .cache_extract_fixture("faostat-fbs-old", NULL) |>
    dplyr::mutate(
      area = "Combined",
      area_code = 206L,
      value = dplyr::if_else(
        year == 2013L & element == "processing",
        200,
        value
      )
    )
  new <- .cache_extract_fixture("faostat-fbs-new", 2014:2016) |>
    dplyr::mutate(area = "Combined", area_code = 206L)
  attr(old, "bucket_members") <- tidyr::crossing(
    year = unique(old$year),
    area_code = c(276L, 277L)
  ) |>
    dplyr::mutate(polity_area_code = 206L) |>
    dplyr::filter(year != 2013L | area_code != 277L)
  attr(new, "bucket_members") <- tidyr::crossing(
    year = unique(new$year),
    area_code = c(276L, 277L)
  ) |>
    dplyr::mutate(polity_area_code = 206L)
  empty <- old[0, ]
  attr(empty, "bucket_members") <- NULL
  testthat::local_mocked_bindings(
    .resolve_all_area_years = \(years) {
      tidyr::crossing(year = years, area_code = c(276L, 277L)) |>
        dplyr::mutate(
          polity_area_code = 206L,
          mapping_status = "matched",
          first_reported_year = NA,
          last_reported_year = NA
        )
    },
    .get_traded_residues = \(...) {
      data.table::copy(data.table::as.data.table(empty))
    },
    .get_fiber_tobacco = \(...) {
      data.table::copy(data.table::as.data.table(empty))
    },
    .package = "whep"
  )
  inputs <- list(
    fbs_old = empty,
    fbs_new = new,
    cbs_crops = empty,
    cbs_animals = empty,
    cbs_new = empty,
    fao_trade = empty,
    primary_cbs = empty,
    crop_residues = empty,
    trade_hist = empty,
    fbs_rebase = NULL,
    palm_support = list(fbs_old = old, fbs_new = new)
  )
  actual <- whep:::.cbs_combine_sources(inputs) |>
    dplyr::filter(year == 2015L, element == "processing")
  # Dropping the partial 2013 source also removes its oil bridge. The
  # existing estimator cannot cross that gap; it must not publish 400 t.
  testthat::expect_equal(nrow(actual), 0L)
  log <- attr(actual, "palm_kernel_support")$coverage_log
  testthat::expect_true(any(log$year == 2013L & log$source == "fbs_old"))
})

testthat::test_that("empty queried palm support retains its supply receipt", {
  old <- .cache_extract_fixture("faostat-fbs-old", NULL)[0, ]
  attr(old, "bucket_members") <- tibble::tibble()
  new <- .cache_extract_fixture("faostat-fbs-new", 2014:2015)
  inputs <- whep:::.cbs_palm_coverage(
    list(palm_support = list(fbs_old = old, fbs_new = new)),
    "complete_sources"
  )
  record <- attr(inputs, "palm_kernel_support")
  testthat::expect_true(record$sources$fbs_old$query$all_years)
  testthat::expect_equal(record$sources$fbs_old$rows_read, 0L)
  testthat::expect_equal(record$rows_used[["fbs_old"]], 0L)
})

testthat::test_that("the public balance retains its palm support receipt", {
  fixed <- readRDS(testthat::test_path("fixtures", "cbs_fixed_small.rds"))
  receipt <- list(sources = list(old = list(all_years = TRUE, rows_read = 0L)))
  attr(fixed, "palm_kernel_support") <- receipt
  actual <- whep::build_commodity_balances(.fixed_data = fixed)
  testthat::expect_identical(attr(actual, "palm_kernel_support"), receipt)
})

testthat::test_that("queried empty new-vintage support retains its year schema", {
  old <- .cache_extract_fixture("faostat-fbs-old", NULL)
  new <- .cache_extract_fixture("faostat-fbs-new", 2014:2015)[0, ]
  testthat::local_mocked_bindings(
    .extract_cb = \(...) cli::cli_abort("Supplied empty query was reread"),
    .package = "whep"
  )
  support <- whep:::.cbs_palm_support(
    list(fbs_old = old, fbs_new = new),
    2015L
  )
  testthat::expect_s3_class(support$fbs_new, "tbl_df")
  testthat::expect_equal(nrow(support$fbs_new), 0L)
  testthat::expect_true(rlang::has_name(support$fbs_new, "year"))
  testthat::expect_identical(
    attr(support$fbs_new, "cbs_extract_query"),
    attr(new, "cbs_extract_query")
  )
  # A supplied old anchor without a new oil proxy invents no target-year flow.
  testthat::expect_equal(nrow(whep:::.fix_palm_kernels(support)), 0L)
})
