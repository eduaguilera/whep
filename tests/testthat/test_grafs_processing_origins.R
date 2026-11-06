# Joint origin of processing feedstock (#1014, PR #1591) -----------------------

.origin_availability <- function() {
  tibble::tribble(
    ~Year, ~Item, ~origin, ~available, ~source,
    2020, "Soyabeans", "domestic", 90, "post_seed_production",
    2020, "Soyabeans", "import", 100, "cbs_gross_import"
  )
}

.origin_targets <- function(requested = 95) {
  tibble::tibble(Year = 2020, Item = "Soyabeans", requested = requested)
}

test_that("joint proportional origins split the target over both pools", {
  result <- .allocate_processing_origins(
    .origin_availability(),
    .origin_targets()
  )

  # 95 t over 90 domestic + 100 imported: 45 and 50, equally likely crushed.
  expect_equal(result$allocated, c(45, 50))
  expect_equal(sum(result$allocated), 95)
  expect_true(all(result$allocated <= result$available))
  expect_identical(
    result$source,
    c("post_seed_production", "cbs_gross_import")
  )
  expect_true(all(result$method_processing_origin == "proportional"))
})

test_that("priority methods are explicit, bounded alternatives", {
  imports_first <- .allocate_processing_origins(
    .origin_availability(),
    .origin_targets(),
    method = "imports_first"
  )
  domestic_first <- .allocate_processing_origins(
    .origin_availability(),
    .origin_targets(),
    method = "domestic_first"
  )

  alloc <- function(x, o) x$allocated[x$origin == o]
  expect_equal(alloc(imports_first, "import"), 95)
  expect_equal(alloc(imports_first, "domestic"), 0)
  expect_equal(alloc(domestic_first, "domestic"), 90)
  expect_equal(alloc(domestic_first, "import"), 5)
  for (x in list(imports_first, domestic_first)) {
    expect_equal(sum(x$allocated), 95)
    expect_true(all(x$allocated >= x$origin_lower - 1e-12))
    expect_true(all(x$allocated <= x$origin_upper + 1e-12))
  }
  expect_true(all(imports_first$method_processing_origin == "imports_first"))
  expect_error(
    .allocate_processing_origins(
      .origin_availability(),
      .origin_targets(),
      method = "stocks_first"
    ),
    class = "rlang_error"
  )
})

test_that("infeasible or missing supply is an error, never a stock", {
  expect_error(
    .allocate_processing_origins(.origin_availability(), .origin_targets(200)),
    class = "whep_processing_origin_infeasible"
  )
  # A missing import pool is not a zero pool.
  expect_error(
    .allocate_processing_origins(
      dplyr::slice_head(.origin_availability(), n = 1),
      .origin_targets()
    ),
    class = "whep_processing_origin_missing"
  )
  unavailable <- .origin_availability()
  unavailable$available[2] <- NA_real_
  expect_error(
    .allocate_processing_origins(unavailable, .origin_targets()),
    class = "whep_processing_origin_invalid"
  )
  stock <- dplyr::mutate(.origin_availability(), origin = "stock")
  expect_error(
    .allocate_processing_origins(stock, .origin_targets()),
    class = "whep_processing_origin_invalid"
  )
  expect_error(
    .allocate_processing_origins(
      dplyr::bind_rows(.origin_availability(), .origin_availability()),
      .origin_targets()
    ),
    class = "whep_processing_origin_duplicate"
  )
})

test_that("a supplied zero pool is allocated nothing", {
  zero_import <- .origin_availability() |>
    dplyr::mutate(available = dplyr::if_else(origin == "import", 0, available))

  result <- .allocate_processing_origins(zero_import, .origin_targets(80))

  expect_equal(result$allocated, c(80, 0))
})

# Ledger over the GRAFS processing targets ------------------------------------

.ledger_targets <- function() {
  tibble::tribble(
    ~Year, ~Item, ~value_to_process,
    2020, "Soyabeans", 95,
    2020, "Olives", 250,
    2020, "Molasses", 30,
    2020, "Grapes", 40
  )
}

.ledger_production <- function() {
  tibble::tribble(
    ~Year, ~Item, ~national_production_fm,
    2020, "Soyabeans", 90,
    2020, "Olives", 150,
    2020, "Grapes", 100
  )
}

.ledger_imports <- function() {
  tibble::tribble(
    ~Year, ~Item, ~import_fm,
    2020, "Soyabeans", 100,
    2020, "Olives", 40,
    2020, "Molasses", 30,
    2020, "Grapes", 0
  )
}

.ledger <- function(method = "proportional", imports = .ledger_imports()) {
  .processing_origin_ledger(
    .ledger_targets(),
    .ledger_production(),
    imports,
    cropland_items = c("Soyabeans", "Olives", "Grapes"),
    method = method
  )
}

.by_item <- function(df, col) rlang::set_names(df[[col]], df$Item)

test_that("the ledger books domestic and imported processing jointly", {
  out <- .ledger()

  share <- .by_item(out$shares, "share_processing")
  fed <- .by_item(out$import_fed, "import_fed_fm")
  # Soy: 95 of 190 available, half of each pool.
  expect_equal(share[["Soyabeans"]], 0.5)
  expect_equal(fed[["Soyabeans"]], 50)
  # No double attribution: domestic + imported processing is the target.
  expect_equal(share[["Soyabeans"]] * 90 + fed[["Soyabeans"]], 95)
  # Grapes: a supplied zero import leaves the domestic share unchanged.
  expect_equal(share[["Grapes"]], 0.4)
  expect_false("Grapes" %in% out$import_fed$Item)
})

test_that("the ledger reports, never repairs, an infeasible target", {
  out <- .ledger()

  share <- .by_item(out$shares, "share_processing")
  fed <- .by_item(out$import_fed, "import_fed_fm")
  excess <- .by_item(out$excess, "excess_fm")
  # Olives: 250 requested against 150 + 40 of supply. Both pools are used
  # whole; the 60 t shortfall stays in the excess report (#1625), and no
  # stock or other origin is invented to cover it.
  expect_equal(share[["Olives"]], 1)
  expect_equal(fed[["Olives"]], 40)
  expect_equal(excess[["Olives"]], 60)
  expect_equal(excess[["Soyabeans"]], 0)
})

test_that("unproduced inputs keep the domestic-only booking", {
  out <- .ledger()

  # Molasses is a second-stage input GRAFS never produces: no share, no
  # import-fed crush, and its whole target stays in the excess report.
  expect_equal(.by_item(out$shares, "share_processing")[["Molasses"]], 0)
  expect_false("Molasses" %in% out$import_fed$Item)
  expect_equal(.by_item(out$excess, "excess_fm")[["Molasses"]], 30)
})

test_that("the ledger alternatives move the domestic share as expected", {
  domestic_first <- .ledger("domestic_first")
  imports_first <- .ledger("imports_first")

  expect_equal(
    .by_item(domestic_first$shares, "share_processing")[["Soyabeans"]],
    1
  )
  expect_equal(
    .by_item(domestic_first$import_fed, "import_fed_fm")[["Soyabeans"]],
    5
  )
  expect_equal(
    .by_item(imports_first$shares, "share_processing")[["Soyabeans"]],
    0
  )
  expect_equal(
    .by_item(imports_first$import_fed, "import_fed_fm")[["Soyabeans"]],
    95
  )
})

test_that("an eligible item with no import row aborts", {
  imports <- .ledger_imports() |> dplyr::filter(Item != "Soyabeans")

  expect_error(
    .ledger(imports = imports),
    class = "whep_processing_origin_missing"
  )
})

test_that(".spain_gross_imports reads gross, not net, imports", {
  wide <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~import, ~export,
    2015, 203L, 2555L, 3500, 30,
    2015, 203L, 2563L, 70, 510,
    2015, 79L, 2555L, 4000, 0
  )

  out <- .spain_gross_imports(wide)

  fm <- .by_item(out, "import_fm")
  expect_equal(nrow(out), 2)
  expect_equal(fm[["Soyabeans"]], 3500)
  # A net exporter still imports: re-exports do not identify which tonnes
  # were crushed, so the gross import is the pool.
  expect_equal(fm[["Olives (including preserved)"]], 70)
})

test_that("a covered target reports an exact zero excess", {
  # 1 t over 3 + 7: the split carries rounding, the excess must not, or the
  # excess warning would list covered items at "0 t".
  out <- .processing_origin_ledger(
    tibble::tibble(Year = 2020, Item = "Sunflower seed", value_to_process = 1),
    tibble::tibble(
      Year = 2020,
      Item = "Sunflower seed",
      national_production_fm = 3
    ),
    tibble::tibble(Year = 2020, Item = "Sunflower seed", import_fm = 7),
    cropland_items = "Sunflower seed"
  )

  expect_identical(out$excess$excess_fm, 0)
  expect_equal(
    out$shares$share_processing * 3 + out$import_fed$import_fed_fm,
    1
  )
})
