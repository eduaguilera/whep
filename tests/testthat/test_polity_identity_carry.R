# test_polity_identity_carry.R -- tests for R/polity_identity_carry.R
#
# Why these exist (whep#707): `.aggregate_to_polities()` emits the reporting
# identity, and both shipped builds dropped it in their reductions, so the tail
# resolved every frame from scratch and the carried path never ran. The identity is now parked
# around those reductions and written back by key. What has to be true for that
# to be safe is tested first -- the write-back changes no row, and what reaches
# the tail equals resolving afresh -- and then that each build's tail keeps it.

# Record, for every tail call, whether `.add_reporting_polity_columns()` kept a
# carried identity rather than resolving the frame again.
.local_carry_tally <- function(env = parent.frame()) {
  tally <- new.env()
  tally$kept <- logical(0)
  # Held before mocking: the mock replaces the binding the real helper would
  # otherwise be reached through.
  carried <- whep:::.carried_reporting_polity
  testthat::local_mocked_bindings(
    .carried_reporting_polity = function(dt, code_column, mode) {
      kept <- carried(dt, code_column, mode)
      tally$kept <- c(tally$kept, kept)
      kept
    },
    .package = "whep",
    .env = env
  )
  tally
}

# A plain area (40 Chile), the Sudan bucket (206), an area whose only polity
# has ended (51 Czechoslovakia) and Rest of World (999).
.identity_keys <- function() {
  tibble::tibble(area_code = c(40L, 206L, 51L, 999L), year = 2015)
}

# Several item rows per key, which is the shape a build frame has.
.item_rows <- function(keys = .identity_keys(), n_items = 3L) {
  keys[rep(seq_len(nrow(keys)), each = n_items), ] |>
    dplyr::mutate(
      item_cbs_code = rep(seq_len(n_items), nrow(keys)),
      value = as.numeric(dplyr::row_number())
    )
}

.with_identity <- function(df) {
  whep:::.add_reporting_polity_columns(df)
}

.without_identity <- function(df) {
  dplyr::select(df, -dplyr::any_of(whep:::.reporting_polity_cols()))
}

.n_groups <- function(df, by) {
  nrow(dplyr::distinct(df, dplyr::across(dplyr::all_of(by))))
}

# The helpers ----------------------------------------------------------------

testthat::test_that("the write-back changes no row, value or order", {
  parked <- whep:::.park_polity_identity(list(
    .with_identity(.identity_keys()[1:2, ])
  ))
  rows <- .item_rows()

  out <- whep:::.attach_polity_identity(rows, parked)

  testthat::expect_true(tibble::is_tibble(out))
  testthat::expect_identical(.without_identity(out), rows)
  pointblank::expect_col_vals_not_null(out, "reporting_polity_code")
})

testthat::test_that("what reaches the tail is kept, and is what it resolves", {
  rows <- .item_rows()
  parked <- whep:::.park_polity_identity(list(
    .with_identity(.identity_keys()[1:2, ])
  ))
  seen <- integer(0)
  resolve <- whep:::.add_polity_columns_dt
  testthat::local_mocked_bindings(
    .add_polity_columns_dt = function(data, ...) {
      seen <<- c(seen, nrow(data))
      resolve(data, ...)
    }
  )

  out <- whep:::.attach_polity_identity(rows, parked)
  # Only the two keys no frame carried are resolved, once each, not their six
  # rows.
  testthat::expect_equal(seen, 2L)

  tally <- .local_carry_tally()
  kept <- whep:::.add_reporting_polity_columns(out)
  fresh <- whep:::.add_reporting_polity_columns(rows)
  testthat::expect_identical(tally$kept, c(TRUE, FALSE))
  testthat::expect_identical(kept, fresh)
})

testthat::test_that("a key the fold left without a polity is not parked", {
  # 99999 is no area code at all, so it resolves to no polity. Parking its NA
  # would make the tail read the carry as incomplete and resolve everything.
  frame <- .with_identity(tibble::tibble(
    area_code = c(40L, 99999L),
    year = 2015
  ))
  testthat::expect_true(is.na(frame$reporting_polity_code[2]))

  parked <- whep:::.park_polity_identity(list(frame))
  testthat::expect_equal(parked$area_code, 40L)

  # The write-back resolves 99999 itself, so the tail is handed a complete
  # carry; the tally starts after it.
  attached <- whep:::.attach_polity_identity(.without_identity(frame), parked)
  tally <- .local_carry_tally()
  out <- whep:::.add_reporting_polity_columns(attached)
  testthat::expect_true(tally$kept)
  testthat::expect_identical(out, frame)
})

testthat::test_that("frames that disagree on a key warn and leave it out", {
  agreed <- .with_identity(.identity_keys()[1:2, ])
  contradicting <- agreed[1, ]
  contradicting$reporting_polity_code <- "XXX-1900-2000"

  testthat::expect_warning(
    parked <- whep:::.park_polity_identity(list(agreed, contradicting)),
    class = "whep_warn_polity_identity_conflict"
  )
  testthat::expect_equal(parked$area_code, 206L)

  # Left out, the key is resolved afresh, so neither answer is published on
  # the strength of which frame came first.
  out <- whep:::.attach_polity_identity(.item_rows(), parked)
  testthat::expect_equal(
    unique(out$reporting_polity_code[out$area_code == 40L]),
    "CHL-1902-2025"
  )
})

testthat::test_that("nothing parked leaves the frame for the tail", {
  rows <- .item_rows()
  testthat::expect_identical(whep:::.attach_polity_identity(rows, NULL), rows)
  # A frame without the identity parks nothing.
  nothing <- whep:::.park_polity_identity(list(rows, NULL, "not a frame"))
  testthat::expect_equal(nrow(nothing), 0L)
  testthat::expect_identical(
    whep:::.attach_polity_identity(rows, nothing),
    rows
  )
})

testthat::test_that("a carried row keeps its identity; only the gaps are filled", {
  # `bind_rows()` of a carrying branch and one the fold never saw -- fodder
  # beside FAOSTAT production -- leaves the columns present and NA on the
  # second half. "The columns exist" must not be read as "the frame is carried".
  keys <- .identity_keys()[1:2, ]
  carried <- .with_identity(.item_rows(keys))
  parked <- whep:::.park_polity_identity(list(carried))
  marked <- carried
  marked$reporting_polity_name[1] <- "kept as carried"
  bound <- dplyr::bind_rows(marked, .item_rows(keys))
  testthat::expect_equal(sum(is.na(bound$reporting_polity_code)), 6L)

  out <- whep:::.attach_polity_identity(bound, parked)

  testthat::expect_identical(.without_identity(out), .without_identity(bound))
  pointblank::expect_col_vals_not_null(out, "reporting_polity_code")
  # Overwriting by key would hide a row someone re-keyed after the fold, which
  # is what the tail's own check is there to catch.
  testthat::expect_equal(out$reporting_polity_name[1], "kept as carried")
})

testthat::test_that("the write-back cannot split a group the way whep#563 did", {
  # The reason the identity is parked rather than added to a `by =`. On a frame
  # where one key holds a carried row and an uncarried one, keying on the
  # identity splits every group in two -- the bucket stops summing and no value
  # moves to say so. After the write-back both rows of a key carry the same
  # identity, so the identity adds nothing to the key.
  keys <- .identity_keys()[1:2, ]
  carried <- .with_identity(.item_rows(keys))
  bound <- dplyr::bind_rows(carried, .item_rows(keys))
  key <- c("year", "area_code", "item_cbs_code")
  with_identity <- c(key, whep:::.reporting_polity_cols())

  testthat::expect_equal(.n_groups(bound, key), 6L)
  testthat::expect_equal(.n_groups(bound, with_identity), 12L)

  out <- whep:::.attach_polity_identity(
    bound,
    whep:::.park_polity_identity(list(carried))
  )
  testthat::expect_equal(.n_groups(out, with_identity), .n_groups(out, key))
})

testthat::test_that("a double `year` and an integer one park as one key", {
  # Both builds publish `year` as double while the folds emit integer.
  as_double <- .with_identity(.identity_keys()[1, ])
  as_integer <- dplyr::mutate(as_double, year = as.integer(year))

  parked <- whep:::.park_polity_identity(list(as_double, as_integer))
  testthat::expect_equal(nrow(parked), 1L)
  testthat::expect_type(parked$year, "integer")
})

# The builds -----------------------------------------------------------------

testthat::test_that(".format_cbs_output() keeps the identity its input carries", {
  cbs <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,     ~value, ~source,
    2015,         40L,          2511L, "production",    100, "FAOSTAT_FBS_New",
    2015,         40L,          2511L, "production",    200, "FAOSTAT_CBS_Old",
    2015,        206L,          2513L, "food",           50, "FAOSTAT_FBS_New",
    2015,        999L,          2513L, "food",            5, "FAOSTAT_FBS_New"
  )
  carried <- .with_identity(cbs)

  tally <- .local_carry_tally()
  out <- whep:::.format_cbs_output(carried)
  # The grouped sum keeps only the key; the identity is written back after it.
  testthat::expect_identical(tally$kept, TRUE)
  testthat::expect_identical(out, whep:::.format_cbs_output(cbs))
  testthat::expect_equal(nrow(out), 3L)
})

testthat::test_that("build_commodity_balances() writes back what .read_cbs() parked", {
  fixed <- tibble::tribble(
    ~year, ~area, ~area_code, ~item_cbs, ~item_cbs_code, ~element,     ~value,
    2015,  "a",          40L, "Wheat",           2511L, "production",    100,
    2015,  "a",          40L, "Wheat",           2511L, "food",          100,
    2015,  "b",         206L, "Barley",          2513L, "production",     50,
    2015,  "b",         206L, "Barley",          2513L, "feed",           50
  ) |>
    dplyr::mutate(source = "FAOSTAT_FBS_New")
  parked <- whep:::.park_polity_identity(list(
    .with_identity(.identity_keys()[1:2, ])
  ))
  testthat::local_mocked_bindings(
    .read_cbs = function(primary_all, ...) {
      structure(fixed, .polity_identity = parked)
    },
    .fix_cbs = function(df, ...) {
      # Built afresh by the balancing steps, so no identity and no attribute.
      fixed
    }
  )

  tally <- .local_carry_tally()
  out <- suppressMessages(
    whep::build_commodity_balances(tibble::tibble(), 2015, 2015)
  )
  testthat::expect_identical(tally$kept, TRUE)
  reference <- suppressMessages(
    whep::build_commodity_balances(.fixed_data = fixed)
  )
  testthat::expect_identical(out, reference)
})

testthat::test_that(".read_cbs() parks the identity its folded inputs carry", {
  # Historical trade is resolved under its own year's borders rather than at
  # the 1961 back-cast anchor the tail uses, so it must not be parked: give it
  # an identity no published row has, and check it is absent.
  folded <- .with_identity(.identity_keys()[1:2, ])
  historical <- .with_identity(.identity_keys()[4, ])
  inputs <- list(
    fbs_new = folded,
    fao_trade = folded[1, ],
    trade_hist = historical,
    fishstat_trade = NULL
  )
  cbs_rows <- tibble::tibble(
    year = 2015,
    area_code = 40L,
    item_cbs_code = 2511L,
    element = "food",
    value = 1
  )
  testthat::local_mocked_bindings(
    .cbs_read_inputs = function(...) inputs,
    .cbs_combine_sources = function(inputs) cbs_rows,
    .prepare_historical_cbs = function(...) tibble::tibble(),
    .cbs_extend_historical = function(cbs_raw0, ...) cbs_raw0,
    .aggregate_fao_trade_to_cbs = function(fao_trade) fao_trade,
    .mass_only_trade = function(trade, ...) trade
  )

  raw <- suppressMessages(
    whep:::.read_cbs(.with_identity(.identity_keys()[3, ]), 2015, 2015)
  )
  parked <- attr(raw, ".polity_identity")
  # 40 and 206 from the extracts, 51 from the production rows.
  testthat::expect_setequal(parked$area_code, c(40L, 206L, 51L))
  testthat::expect_false(999L %in% parked$area_code)
})

testthat::test_that(".cbs_long_to_wide() keeps the identity the long CBS carries", {
  testthat::local_mocked_bindings(
    .get_livestock_trade_totals = function(livestock_items, ...) {
      tibble::tibble(
        year = integer(),
        area_code = integer(),
        item_cbs_code = integer(),
        import = numeric(),
        export = numeric()
      )
    }
  )
  long <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,         ~value,
    2015,         40L,          2511L, "production",       6000,
    2015,         40L,          2511L, "food",             6000,
    2015,         40L,          2511L, "domestic_supply",  6000,
    2015,        206L,          2513L, "production",       3000,
    2015,        206L,          2513L, "feed",             3000,
    2015,        206L,          2513L, "domestic_supply",  3000
  )
  livestock <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~live_anim_code, ~unit, ~value,
    2015,         40L,          1096L,              NA, "heads", 100,
    2015,         40L,          1096L,              NA, "slaughtered_heads", 10,
    2015,         40L,          2735L,           1096L, "tonnes", 2
  )

  tally <- .local_carry_tally()
  out <- whep:::.cbs_long_to_wide(
    .with_identity(long),
    .with_identity(livestock),
    2015
  )
  testthat::expect_identical(tally$kept[3], TRUE)
  testthat::expect_identical(
    out,
    whep:::.cbs_long_to_wide(long, livestock, 2015)
  )
  testthat::expect_true(1096L %in% out$item_cbs_code)
})

testthat::test_that("build_primary_production() keeps a carried identity", {
  raw <- readRDS(testthat::test_path("fixtures", "prod_raw_small.rds"))
  carried <- .with_identity(raw)

  tally <- .local_carry_tally()
  out <- suppressMessages(
    whep::build_primary_production(.raw_data = carried)
  )
  testthat::expect_identical(tally$kept, TRUE)
  reference <- suppressMessages(whep::build_primary_production(.raw_data = raw))
  testthat::expect_identical(out, reference)
})

testthat::test_that(".read_production() writes the fold's identity back", {
  # The stubbed yield chain keeps only `year` and `area_code`, as the real
  # reductions keep only their key, so the identity the FAOSTAT rows carry is
  # gone by the end unless it is written back.
  carrying_rows <- function(years) {
    .with_identity(.stub_fao_rows(years))
  }
  out <- .run_stubbed_read_production(2010, 2010, fao_rows = carrying_rows)$out
  plain <- .run_stubbed_read_production(2010, 2010)$out
  # Travels with the frame to the CBS build, and is not what is compared here.
  attr(out, ".cb_extracts") <- NULL
  attr(plain, ".cb_extracts") <- NULL

  testthat::expect_true(all(whep:::.reporting_polity_cols() %in% names(out)))
  testthat::expect_identical(.without_identity(out), plain)
  tally <- .local_carry_tally()
  kept <- whep:::.add_reporting_polity_columns(out)
  testthat::expect_identical(tally$kept, TRUE)
  testthat::expect_identical(kept, whep:::.add_reporting_polity_columns(plain))
})
