# test_silk_mass_basis.R — tests for R/silk_mass_basis.R (whep#1251)

# The silk chain of one country-year, in the raw `faostat-cbs-new` layout.
# 1,000 t of cocoons; 800 t are reeled into 128 t of raw silk (0.16), and
# every link balances on its own, as FAO reports it:
#   cocoons   1000 - 10 export        = 190 other uses + 800 processed
#   raw silk   128 + 5 import - 20 export = 113 other uses
#   waste        7 import               =   7 other uses
# A rubber "Processed" row stands in for every other chain, and a 2013 silk
# row for the years the old aggregated Commodity Balances cover.
.silk_fixture <- function() {
  tibble::tribble(
    ~`Item Code`, ~Item,                    ~Element,                ~Value, ~Year,
    1185L,        "Silk-worm cocoons",      "Production",            1000,   2020L,
    1185L,        "Silk-worm cocoons",      "Export quantity",       10,     2020L,
    1185L,        "Silk-worm cocoons",      "Other uses (non-food)", 190,    2020L,
    1185L,        "Silk-worm cocoons",      "Processed",             800,    2020L,
    1186L,        "Raw silk (not thrown)",  "Production",            128,    2020L,
    1186L,        "Raw silk (not thrown)",  "Import quantity",       5,      2020L,
    1186L,        "Raw silk (not thrown)",  "Export quantity",       20,     2020L,
    1186L,        "Raw silk (not thrown)",  "Other uses (non-food)", 113,    2020L,
    1187L,        "Silk waste",             "Import quantity",       7,      2020L,
    1187L,        "Silk waste",             "Other uses (non-food)", 7,      2020L,
    836L,         "Natural rubber",         "Processed",             50,     2020L,
    1186L,        "Raw silk (not thrown)",  "Production",            99,     2013L
  ) |>
    dplyr::mutate(
      `Area Code` = 203L,
      Area = "Testland",
      Unit = "t",
      Flag = "A"
    ) |>
    data.table::as.data.table()
}

.local_silk_extract <- function(env = parent.frame()) {
  crosswalk <- data.table::data.table(
    area_code = 203L,
    area_name = "Testland",
    area_iso3c = "TST",
    polity_area_code = 203L,
    polity_code = "TST-1900-2025",
    polity_name = "Testland",
    polity_start_year = 1900L,
    polity_end_year = 2025L,
    polity_type = "national",
    mapping_status = "matched",
    has_geometry = TRUE
  )
  fixture <- .silk_fixture()
  testthat::local_mocked_bindings(
    .polity_crosswalk = function(include_unmapped = TRUE) {
      data.table::copy(crosswalk)
    },
    .read_input = function(pin_alias, years = NULL, year_col = NULL) {
      data.table::copy(fixture)
    },
    .env = env
  )
  whep:::.extract_fao("faostat-cbs-new", keep_elements = "Processed")
}

# The Silk balance as `.get_fiber_tobacco()` books it, one column per element.
.silk_balance <- function(cbs_new) {
  cbs_new |>
    whep:::.get_fiber_tobacco(whep::cbs_trade_codes, whep::items_full) |>
    dplyr::filter(item_cbs == "Silk") |>
    dplyr::select(element, value) |>
    tidyr::pivot_wider(names_from = element, values_from = value) |>
    dplyr::mutate(
      supply = production + import - export,
      unbooked = supply - other_uses
    )
}

test_that("the unconverted chain books the reeled cocoons as stock", {
  # What `main` did before whep#1251: Processed dropped, links summed. The
  # 800 t of reeled cocoons have no use, so the balance leaves them as stock.
  legacy <- .local_silk_extract() |>
    dplyr::filter(element != "Processed") |>
    .silk_balance()

  expect_equal(legacy$production, 1128)
  expect_equal(legacy$unbooked, 800)
})

test_that("every silk basis closes the chain's balance", {
  extracted <- .local_silk_extract()

  purrr::walk(whep:::.silk_basis_choices(), \(method) {
    balance <- whep:::.cbs_silk_mass_basis(extracted, method) |>
      .silk_balance()
    expect_equal(balance$unbooked, 0, tolerance = 1e-9, label = method)
  })
})

test_that("the cocoon basis counts production once, in cocoon mass", {
  rate <- whep:::.silk_raw_extraction_rate()
  balance <- .local_silk_extract() |>
    whep:::.cbs_silk_mass_basis("cocoon") |>
    .silk_balance()

  expect_equal(balance$production, 1000)
  expect_equal(balance$import, 5 / rate + 7)
  expect_equal(balance$export, 10 + 20 / rate)
  # cocoons used as such + cocoons reeled + raw silk not reeled at home
  expect_equal(balance$other_uses, 190 + 800 + (113 - 128) / rate + 7)
})

test_that("the raw-silk basis is the cocoon basis times the rate", {
  rate <- whep:::.silk_raw_extraction_rate()
  extracted <- .local_silk_extract()
  cocoon <- whep:::.cbs_silk_mass_basis(extracted, "cocoon") |>
    .silk_balance()
  raw <- whep:::.cbs_silk_mass_basis(extracted, "raw_silk") |>
    .silk_balance()

  expect_equal(raw$production, 1000 * rate)
  # Silk waste (7 t each way) keeps its own mass.
  expect_equal(raw$import - 7, (cocoon$import - 7) * rate)
  expect_equal(raw$other_uses - 7, (cocoon$other_uses - 7) * rate)
})

test_that("the mixed basis books the reeled cocoons as other_uses", {
  balance <- .local_silk_extract() |>
    whep:::.cbs_silk_mass_basis("mixed") |>
    .silk_balance()

  expect_equal(balance$production, 1128)
  expect_equal(balance$other_uses, 190 + 800 + 113 + 7)
})

test_that("no Processed row survives any basis, silk or not", {
  extracted <- .local_silk_extract()
  expect_true("Processed" %in% extracted$element)

  purrr::walk(whep:::.silk_basis_choices(), \(method) {
    out <- whep:::.cbs_silk_mass_basis(extracted, method)
    expect_false("Processed" %in% out$element, label = method)
  })
})

test_that("years before the new Commodity Balances are left as read", {
  extracted <- .local_silk_extract()
  old <- extracted |> dplyr::filter(year == 2013)

  purrr::walk(c("cocoon", "raw_silk"), \(method) {
    out <- whep:::.cbs_silk_mass_basis(extracted, method) |>
      dplyr::filter(year == 2013)
    expect_equal(out$value, old$value, label = method)
  })
})

test_that("converted rows lose their FAOSTAT flag, untouched rows keep it", {
  out <- .local_silk_extract() |>
    whep:::.cbs_silk_mass_basis("cocoon") |>
    dplyr::filter(year == 2020)

  converted <- out |>
    dplyr::filter(item_cbs_code == 1186)
  untouched <- out |>
    dplyr::filter(item_cbs_code %in% c(1185, 1187))

  expect_true(all(is.na(converted$fao_flag)))
  expect_true(all(untouched$fao_flag == "A"))
})

test_that("the default silk basis is cocoon", {
  expect_equal(whep:::.silk_basis_choices()[[1]], "cocoon")
})

test_that("an unknown silk basis is refused", {
  expect_error(
    whep:::.cbs_silk_mass_basis(.silk_fixture(), "fresh"),
    class = "rlang_error"
  )
  expect_error(
    whep:::.trade_silk_mass_basis(data.table::data.table(), "fresh"),
    class = "rlang_error"
  )
})

test_that("FAOSTAT trade of the converted years moves onto the basis", {
  rate <- whep:::.silk_raw_extraction_rate()
  trade <- tibble::tribble(
    ~year, ~item_code_trade, ~element, ~unit, ~value, ~fao_flag,
    2020L, 1185,             "import", "t",   10,     "A",
    2020L, 1186,             "import", "t",   16,     "A",
    2020L, 1187,             "export", "t",   3,      "A",
    2013L, 1186,             "import", "t",   16,     "A",
    2020L, 1186,             "import", "An",  4,      "A"
  )

  cocoon <- whep:::.trade_silk_mass_basis(trade, "cocoon")
  raw <- whep:::.trade_silk_mass_basis(trade, "raw_silk")
  mixed <- whep:::.trade_silk_mass_basis(trade, "mixed")

  expect_equal(cocoon$value, c(10, 16 / rate, 3, 16, 4))
  expect_equal(raw$value, c(10 * rate, 16, 3, 16, 4))
  expect_equal(mixed$value, trade$value)
  expect_equal(is.na(cocoon$fao_flag), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # The input is not modified in place.
  expect_equal(trade$value, c(10, 16, 3, 16, 4))
})

test_that("build_commodity_balances validates silk_basis", {
  expect_error(
    build_commodity_balances(example = TRUE, silk_basis = "fresh"),
    class = "rlang_error"
  )
  expect_warning(
    build_commodity_balances(
      .fixed_data = tibble::tibble(
        year = c(2010L, 2011L),
        area = "Spain",
        area_code = 203L,
        item_cbs = "Wheat and products",
        item_cbs_code = 2511L,
        element = "import",
        value = c(1, 2),
        source = "FAOSTAT_trade"
      ),
      silk_basis = "mixed"
    ),
    "silk_basis"
  )
})
