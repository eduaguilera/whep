# Small crafted long CBS fixture, the shape the build pipeline actually
# produces. Supply is production plus import plus stock withdrawal; use is
# export plus food, feed, seed, processing, other uses and stock addition.
# Spain adds stock (positive `stock_variation`) and France withdraws from it
# (negative), so the split `.pivot_cbs_wide()` performs is on both sides of
# the identity rather than on the zero that satisfies it either way.
.make_cbs_long_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,         ~value,
    2000L,       203L,          2511L, "production",       6000,
    2000L,       203L,          2511L, "import",           1000,
    2000L,       203L,          2511L, "export",            500,
    2000L,       203L,          2511L, "food",             3000,
    2000L,       203L,          2511L, "feed",             1500,
    2000L,       203L,          2511L, "seed",              200,
    2000L,       203L,          2511L, "processing",        500,
    2000L,       203L,          2511L, "other_uses",        300,
    2000L,       203L,          2511L, "stock_variation",  1000,
    2000L,       203L,          2511L, "domestic_supply",  5500,
    2000L,        68L,          2513L, "production",       3000,
    2000L,        68L,          2513L, "import",            500,
    2000L,        68L,          2513L, "export",            200,
    2000L,        68L,          2513L, "food",             3000,
    2000L,        68L,          2513L, "feed",              800,
    2000L,        68L,          2513L, "seed",              100,
    2000L,        68L,          2513L, "processing",        200,
    2000L,        68L,          2513L, "other_uses",        200,
    2000L,        68L,          2513L, "stock_variation", -1000,
    2000L,        68L,          2513L, "domestic_supply",  4300
  )
}

# Primary production rows for the live-animal items the FAO sheet omits, so
# `.cbs_wide_core()` has livestock rows to append.
.make_livestock_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~live_anim_code,
    ~unit,               ~value,
    2000L,       203L,          1096L,              NA,
    "heads",                100,
    2000L,       203L,          1096L,              NA,
    "slaughtered_heads",     10,
    2000L,       203L,          2735L,           1096L,
    "tonnes",                  2
  )
}

# A long CBS carrying a processing flow (wheat) and the production of the
# item it is processed into (non-food alcohol), which is what
# `build_processing_coefs()` calibrates its conversion factors against.
.make_proc_cbs_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,      ~value,
    2000L,       203L,          2511L, "processing",    1000,
    2000L,       203L,          2659L, "production",     200,
    2000L,        68L,          2511L, "processing",     400,
    2000L,        68L,          2659L, "production",      50
  )
}

.empty_livestock_trade <- function(livestock_items, ...) {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    item_cbs_code = integer(),
    import = numeric(),
    export = numeric()
  )
}

k_tolerance <- 1e-6

# Replaces "wide CBS has consistent supply-use balance", which recomputed
# both sides of the identity from an already-balanced hand-entered tibble and
# called no package function at all (whep#177). `.cbs_wide_core()` is the
# assembly `build_io_model()` consumes and had no test of its own: deleting
# its `bind_rows(livestock_cbs)` left the whole suite green.
testthat::test_that(".cbs_wide_core balances and keeps its livestock rows", {
  local_mocked_bindings(
    .get_livestock_trade_totals = .empty_livestock_trade
  )

  wide <- .cbs_wide_core(
    .make_cbs_long_fixture(),
    .make_livestock_fixture(),
    2000L
  )

  # A row-wise balance check cannot see a row that is simply absent, so
  # assert the live-animal rows arrived before asserting they reconcile.
  testthat::expect_true(1096L %in% wide$item_cbs_code)
  testthat::expect_setequal(wide$item_cbs_code, c(2511L, 2513L, 1096L))

  balance <- whep::check_supply_use_balance(wide, tol = k_tolerance)
  testthat::expect_equal(nrow(balance), nrow(wide))
  testthat::expect_true(all(balance$balanced))

  pointblank::expect_col_vals_expr(
    wide,
    rlang::expr(
      dplyr::near(
        domestic_supply,
        food + feed + seed + processing + other_uses,
        tol = !!k_tolerance
      )
    )
  )
})

testthat::test_that(".pivot_cbs_wide splits stock variation by balance sign", {
  cbs_long <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2000L, 1L, 10L, "production", 100,
    2000L, 1L, 10L, "stock_variation", 30,
    2000L, 2L, 10L, "production", 100,
    2000L, 2L, 10L, "stock_variation", -20
  )

  result <- .pivot_cbs_wide(cbs_long) |>
    dplyr::arrange(.data$area_code)

  testthat::expect_equal(result$stock_addition, c(30, 0))
  testthat::expect_equal(result$stock_withdrawal, c(0, 20))
})

testthat::test_that(".pivot_cbs_wide fills stock_variation as 0 when absent (#219)", {
  cbs_long <- tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~element, ~value,
      2010L, 1L, 100L, "production", 50,
      2010L, 1L, 100L, "import", 5
    )

  result <- .pivot_cbs_wide(cbs_long)

  testthat::expect_equal(result$stock_addition, 0)
  testthat::expect_equal(result$stock_withdrawal, 0)
})

testthat::test_that(".pivot_cbs_wide aborts on a duplicate element key (#219)", {
  cbs_long <- tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~element, ~value,
      2010L, 1L, 100L, "production", 50,
      2010L, 1L, 100L, "production", 20,
      2010L, 1L, 100L, "stock_variation", 5
    )

  testthat::expect_error(.pivot_cbs_wide(cbs_long))
})

# Replaces "processing coefficients are internally consistent", which
# asserted the three conversion-factor identities against a hand-entered
# tibble that already satisfied them (whep#177). Swapping
# `initial_conversion_factor` and `final_conversion_factor` in
# `.format_proc_output()` left the whole suite green, because nothing called
# the real builder: `build_processing_coefs()` appeared in the suite only as
# `example = TRUE`, which returns a hardcoded tribble.
testthat::test_that("build_processing_coefs returns consistent coefficients", {
  coefs <- build_processing_coefs(
    .make_proc_cbs_fixture(),
    start_year = 2000,
    end_year = 2000
  )

  # Both areas must survive the calibration joins; an absent row balances
  # vacuously.
  testthat::expect_setequal(coefs$area_code, c(203L, 68L))
  testthat::expect_equal(nrow(coefs), 2L)

  pointblank::expect_col_vals_expr(
    coefs,
    rlang::expr(
      dplyr::near(
        value_to_process * initial_conversion_factor,
        initial_value_processed,
        tol = !!k_tolerance
      )
    )
  )

  pointblank::expect_col_vals_expr(
    coefs,
    rlang::expr(
      dplyr::near(
        initial_value_processed * conversion_factor_scaling,
        final_value_processed,
        tol = !!k_tolerance
      )
    )
  )

  pointblank::expect_col_vals_expr(
    coefs,
    rlang::expr(
      dplyr::near(
        initial_conversion_factor * conversion_factor_scaling,
        final_conversion_factor,
        tol = !!k_tolerance
      )
    )
  )

  # What the per-area scaling is for: the calibrated processed output has to
  # reproduce the observed production of the processed item, area by area.
  # The globally calibrated factor alone does not -- it is one number for
  # both areas (0.1786 here, from a raw table fraction of 0.28), and the
  # per-area scaling that closes the gap differs between them (1.12 and
  # 0.70).
  observed <- .make_proc_cbs_fixture() |>
    dplyr::filter(element == "production") |>
    dplyr::select(year, area_code, item_cbs_code, observed = value)

  testthat::expect_equal(
    coefs |>
      dplyr::select(
        year,
        area_code,
        item_cbs_code = item_cbs_code_processed,
        final_value_processed
      ) |>
      dplyr::inner_join(
        observed,
        by = c("year", "area_code", "item_cbs_code")
      ) |>
      dplyr::arrange(area_code) |>
      dplyr::pull(final_value_processed),
    dplyr::arrange(observed, area_code)$observed
  )
})

testthat::test_that("livestock CBS routes slaughter animals to processing", {
  local_mocked_bindings(
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

  primary <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~live_anim_code, ~unit, ~value,
    2000L, 1L, 1096L, NA, "heads", 100,
    2000L, 1L, 1096L, NA, "slaughtered_heads", 10,
    2000L, 1L, 2735L, 1096L, "tonnes", 2,
    2000L, 1L, 946L, NA, "heads", 200,
    2000L, 1L, 946L, NA, "slaughtered_heads", 20,
    2000L, 1L, 2731L, 946L, "tonnes", 5
  )

  result <- get_livestock_cbs(primary)
  horse <- dplyr::filter(result, item_cbs_code == 1096L)
  buffalo <- dplyr::filter(result, item_cbs_code == 946L)

  testthat::expect_equal(horse$production, 10)
  testthat::expect_equal(horse$domestic_supply, 10)
  testthat::expect_equal(horse$other_uses, 0)
  testthat::expect_equal(horse$processing, 10)

  testthat::expect_equal(buffalo$production, 20)
  testthat::expect_equal(buffalo$domestic_supply, 20)
  testthat::expect_equal(buffalo$processing, 20)
  testthat::expect_equal(buffalo$other_uses, 0)
})

testthat::test_that(".map_livestock_trade_polities maps raw FAO codes to polity codes", {
  # Raw FAOSTAT area_code 62 (Ethiopia) maps to polity area_code 238,
  # while 231 (United States) maps to itself.
  btd <- tibble::tribble(
      ~year, ~from_code, ~to_code, ~item_cbs_code, ~unit, ~value,
      2000L, 62L, 231L, 1096L, "heads", 30,
      2000L, 231L, 62L, 1096L, "heads", 10
    )

  mapped <- .map_livestock_trade_polities(btd)

  testthat::expect_setequal(mapped$from_code, c(238L, 231L))
  testthat::expect_setequal(mapped$to_code, c(231L, 238L))
  testthat::expect_false(any(mapped$from_code == 62L))
  testthat::expect_false(any(mapped$to_code == 62L))
})

testthat::test_that("livestock trade reconciles with polity-coded slaughter (Ethiopia)", {
  # Bilateral trade is keyed by RAW FAOSTAT codes (Ethiopia = 62),
  # whereas slaughter counts are polity-coded (Ethiopia = 238). Both
  # sides must be reconciled on the polity area code before joining.
  local_mocked_bindings(
    whep_read_file = function(...) NULL,
    .clean_bilateral_trade = function(...) {
      tibble::tribble(
          ~year, ~from_code, ~to_code, ~item_cbs_code, ~unit, ~value,
          2000L, 62L, 231L, 1096L, "heads", 30,
          2000L, 231L, 62L, 1096L, "heads", 10
        )
    }
  )

  primary <- tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~live_anim_code, ~unit, ~value,
      2000L, 238L, 2735L, 1096L, "tonnes", 2,
      2000L, 238L, 1096L, NA, "slaughtered_heads", 100
    )

  # USA (area 231) trades this item but has no slaughtered_heads row of its
  # own, so building the CBS warns about it (whep#168).
  testthat::expect_warning(result <- get_livestock_cbs(primary))
  ethiopia <- dplyr::filter(
    result,
    item_cbs_code == 1096L,
    area_code == 238L
  )

  # Live-animal trade now matches the polity-coded slaughter row.
  testthat::expect_equal(ethiopia$area_code, 238L)
  testthat::expect_equal(ethiopia$import, 10)
  testthat::expect_equal(ethiopia$export, 30)
  # Production is slaughtered plus export less import, so 100 plus 30 less 10.
  testthat::expect_equal(ethiopia$production, 120)
  # Domestic supply is production plus import less export, so 120 plus 10 less 30.
  testthat::expect_equal(ethiopia$domestic_supply, 100)

  # The USA side of the trade (area 231) has no slaughter row of its own for
  # this item -- it must still survive as a trade-only row (whep#168) instead
  # of being dropped by the join, with slaughtered treated as 0.
  usa <- dplyr::filter(result, item_cbs_code == 1096L, area_code == 231L)
  testthat::expect_equal(nrow(usa), 1L)
  testthat::expect_equal(usa$import, 30)
  testthat::expect_equal(usa$export, 10)
  # No slaughter row for the USA side of this trade, so production (animals
  # raised in country) is clamped to 0 and the imports sit as domestic supply:
  # 0 (slaughtered) + 10 (export) - 30 (import), clamped, then + 30 - 10.
  testthat::expect_equal(usa$production, 0)
  testthat::expect_equal(usa$domestic_supply, 20)
})

testthat::test_that("livestock trade survives when the importer has no slaughter row (#168)", {
  # Country 2 imports live horses (item 1096) and never slaughters any
  # itself for that item -- it has no `slaughtered_heads` row at all, only
  # a trade row. Country 1 slaughters and exports. Country 2's import
  # volume must still enter the CBS instead of vanishing because it has no
  # matching `slaughtered` row to left_join onto.
  local_mocked_bindings(
    .get_livestock_trade_totals = function(livestock_items, ...) {
      tibble::tribble(
          ~year, ~area_code, ~item_cbs_code, ~import, ~export,
          2000L, 1L, 1096L, 0, 30,
          2000L, 2L, 1096L, 30, 0
        )
    }
  )

  primary <- tibble::tribble(
      ~year, ~area_code, ~item_cbs_code, ~live_anim_code, ~unit, ~value,
      2000L, 1L, 1096L, NA, "slaughtered_heads", 100,
      2000L, 1L, 2735L, 1096L, "tonnes", 2
    )

  testthat::expect_warning(result <- get_livestock_cbs(primary))
  importer <- dplyr::filter(
    result,
    area_code == 2L,
    item_cbs_code == 1096L
  )

  testthat::expect_equal(nrow(importer), 1L)
  testthat::expect_equal(importer$import, 30)
  # No slaughter row for country 2, so production (animals raised in
  # country) is clamped to 0 and the import sits entirely as domestic
  # supply instead of vanishing.
  testthat::expect_equal(importer$production, 0)
  testthat::expect_equal(importer$domestic_supply, 30)
})

# whep#762 -- trade recovery must be reachable from the cached build chain,
# which is the only path the IO model, the extensions and the nourishment axis
# take. Before this, build_commodity_balances() was the sole entry point that
# could select it, so the recovered CBS could not be carried into a build.
testthat::test_that("get_wide_cbs takes and validates trade_recovery", {
  testthat::expect_true(
    "trade_recovery" %in% names(formals(whep::get_wide_cbs))
  )
  # Validated before any build is started, so a typo aborts offline rather
  # than after a several-minute read.
  testthat::expect_error(
    whep::get_wide_cbs(example = TRUE, trade_recovery = "net-import")
  )
  testthat::expect_no_error(
    whep::get_wide_cbs(example = TRUE, trade_recovery = "net_import")
  )
})

testthat::test_that("get_wide_cbs threads trade_recovery into the chain", {
  seen <- NULL
  testthat::local_mocked_bindings(
    .cached_cbs_built = function(years, trade_recovery = "none") {
      seen <<- trade_recovery
      rlang::abort("chain reached", class = "whep_chain_probe")
    },
    .package = "whep"
  )

  testthat::expect_error(
    whep::get_wide_cbs(years = 2010, trade_recovery = "net_import"),
    class = "whep_chain_probe"
  )
  testthat::expect_equal(seen, "net_import")
})

testthat::test_that("get_processing_coefs takes trade_recovery", {
  testthat::expect_true(
    "trade_recovery" %in% names(formals(whep::get_processing_coefs))
  )
  testthat::expect_error(
    whep::get_processing_coefs(example = TRUE, trade_recovery = "net-import")
  )

  seen <- NULL
  testthat::local_mocked_bindings(
    .cached_cbs_built = function(years, trade_recovery = "none") {
      seen <<- trade_recovery
      rlang::abort("chain reached", class = "whep_chain_probe")
    },
    .package = "whep"
  )

  testthat::expect_error(
    whep::get_processing_coefs(years = 2010, trade_recovery = "net_import"),
    class = "whep_chain_probe"
  )
  testthat::expect_equal(seen, "net_import")
})

# whep#1092: the live-animal trade the livestock balance rests on was
# filtered to `unit == "heads"`, which is only half of FAOSTAT's live-animal
# vocabulary. The small species -- broiler chickens, turkeys, ducks, geese,
# rabbits, rodents -- are reported in `1000 Head` and were dropped whole, so
# `production = slaughtered + export - import` collapsed to `slaughtered`.
.fake_livestock_btd <- function() {
  tibble::tribble(
    ~area_code, ~area_code_p, ~year, ~Element, ~unit,       ~value,
    231L,       9L,           2010L, "Export", "Head",      40,
    231L,       9L,           2010L, "Export", "1000 Head", 5,
    231L,       9L,           2010L, "Export", "No",        7
  ) |>
    dplyr::mutate(
      item = c("Cattle, non-dairy", "Chickens, broilers", "Bees")
    )
}

testthat::test_that("livestock trade keeps FAOSTAT's '1000 Head' rows", {
  local_mocked_bindings(
    whep_read_file = function(...) .fake_livestock_btd()
  )

  testthat::expect_warning(
    totals <- .get_livestock_trade_totals(c(961L, 1053L)),
    class = "whep_unhandled_trade_unit"
  )

  chickens <- dplyr::filter(totals, item_cbs_code == 1053L)
  cattle <- dplyr::filter(totals, item_cbs_code == 961L)

  # 5 thousand head of live broilers become 5,000 head; the `Head` row is
  # carried at face value, as it always was.
  testthat::expect_equal(sum(chickens$export, na.rm = TRUE), 5000)
  testthat::expect_equal(sum(cattle$export, na.rm = TRUE), 40)
})

testthat::test_that("'drop' reproduces the pre-#1092 livestock trade", {
  local_mocked_bindings(
    whep_read_file = function(...) .fake_livestock_btd()
  )

  testthat::expect_warning(
    totals <- .get_livestock_trade_totals(c(961L, 1053L), "drop"),
    class = "whep_unhandled_trade_unit"
  )

  testthat::expect_false(1053L %in% totals$item_cbs_code)
  testthat::expect_equal(sum(totals$export, na.rm = TRUE), 40)
})

testthat::test_that("'abort' is not swallowed by the read's tryCatch", {
  # The unit refusal is a deliberate stop, not a failed read, so it must
  # not degrade into "Could not read bilateral trade for livestock".
  local_mocked_bindings(
    whep_read_file = function(...) .fake_livestock_btd()
  )

  testthat::expect_error(
    .get_livestock_trade_totals(c(961L, 1053L), "abort"),
    class = "whep_unhandled_trade_unit"
  )
})

testthat::test_that("get_livestock_cbs rejects an unknown head method", {
  testthat::expect_error(
    get_livestock_cbs(
      tibble::tibble(
        year = integer(),
        area_code = integer(),
        item_cbs_code = integer(),
        live_anim_code = integer(),
        unit = character(),
        value = numeric()
      ),
      method_head_units = "rescale"
    ),
    class = "rlang_error"
  )
})
