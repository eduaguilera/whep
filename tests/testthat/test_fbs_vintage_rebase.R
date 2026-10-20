# -- FBS trade rebase (whep#1495, whep#1550) -----------------------------------

# Two areas trading nuts. The old vintage closes the world: the exporter's
# export is the importer's import. The new vintage books the importer's side of
# the same flow at 3x (FAOSTAT's import-side standardisation of kernels), and
# the excess goes on into the importer's food, so the new vintage's world
# imports are 3x its world exports while each row still balances.
.nut_fbs_rows <- function() {
  flow <- function(year) 100 + (year - 2005) * 10
  old <- tidyr::expand_grid(year = 2005:2013, area_code = c(1L, 2L)) |>
    dplyr::mutate(source = "FAOSTAT_FBS_Old", inflate = 1)
  new <- tidyr::expand_grid(year = 2010:2016, area_code = c(1L, 2L)) |>
    dplyr::mutate(source = "FAOSTAT_FBS_New", inflate = 3)
  dplyr::bind_rows(old, new) |>
    dplyr::mutate(
      production = dplyr::if_else(area_code == 1L, flow(year) + 50, 0),
      export = dplyr::if_else(area_code == 1L, flow(year), 0),
      import = dplyr::if_else(area_code == 2L, flow(year) * inflate, 0),
      food = production + import - export,
      domestic_supply = food
    ) |>
    dplyr::select(-inflate) |>
    tidyr::pivot_longer(
      c(production, export, import, food, domestic_supply),
      names_to = "element"
    ) |>
    dplyr::mutate(
      area = dplyr::if_else(area_code == 1L, "Exporter", "Importer"),
      item_cbs = "Nuts and products",
      item_cbs_code = 2551,
      unit = "tonnes"
    )
}

.nut_vintage <- function(rows, src) {
  rows |>
    dplyr::filter(source == src) |>
    dplyr::select(-source)
}

.nut_factors <- function(rows = .nut_fbs_rows()) {
  whep:::.fbs_rebase_factors(
    .nut_vintage(rows, "FAOSTAT_FBS_New"),
    .nut_vintage(rows, "FAOSTAT_FBS_Old"),
    "rebased"
  )
}

# The new vintage rebased, back in the source frame `.select_best_source()`
# reads, as `.cbs_combine_sources()` assembles it.
.nut_select <- function(rows = .nut_fbs_rows(), factors = .nut_factors(rows)) {
  new <- whep:::.rebase_fbs_trade(
    .nut_vintage(rows, "FAOSTAT_FBS_New"),
    factors
  )
  dplyr::bind_rows(
    tibble::as_tibble(new) |> dplyr::mutate(source = "FAOSTAT_FBS_New"),
    rows |> dplyr::filter(source == "FAOSTAT_FBS_Old")
  ) |>
    whep:::.select_best_source(rebased = attr(new, "rebased_keys"))
}

.nut_wide <- function(selected, yr) {
  selected |>
    dplyr::filter(year == yr) |>
    dplyr::select(area_code, element, value) |>
    tidyr::pivot_wider(names_from = element, values_from = value) |>
    dplyr::arrange(area_code)
}

test_that("rebased nut trade closes the world the new vintage does not", {
  rows <- .nut_fbs_rows()
  before <- .nut_wide(whep:::.select_best_source(rows), 2016L)
  after <- .nut_wide(.nut_select(rows), 2016L)

  expect_equal(sum(before$import) / sum(before$export), 3)
  expect_equal(sum(after$import) / sum(after$export), 1)
  # The excess came out of the importer's food, nothing else.
  expect_equal(after$food, c(50, 210))
  expect_equal(after$production, before$production)
})

test_that("rebasing keeps every row's supply-use identity", {
  after <- .nut_select() |>
    dplyr::filter(year >= 2010L) |>
    dplyr::select(year, area_code, element, value) |>
    tidyr::pivot_wider(names_from = element, values_from = value) |>
    dplyr::mutate(gap = production + import - export - domestic_supply)
  expect_equal(after$gap, rep(0, nrow(after)))
  expect_true(all(after$food >= 0))
})

test_that("rebased rows are labelled and keep FBS_New's flag", {
  rows <- .nut_fbs_rows() |>
    dplyr::mutate(
      fao_flag = dplyr::if_else(source == "FAOSTAT_FBS_New", "I", "S")
    )
  selected <- .nut_select(rows)
  moved <- selected |>
    dplyr::filter(source == "FAOSTAT_FBS_New_rebased")

  expect_setequal(unique(moved$element), c("import", "food", "domestic_supply"))
  expect_true(all(moved$area_code == 2L))
  expect_true(all(moved$fao_flag == "I"))
  # The exporter's trade did not move, so it keeps the plain label.
  exporter <- selected |>
    dplyr::filter(area_code == 1L, year >= 2010L)
  expect_true(all(exporter$source == "FAOSTAT_FBS_New"))
})

test_that("the old vintage follows the rebased level, not the published one", {
  rows <- .nut_fbs_rows()
  importer_2005 <- function(selected) {
    selected |>
      dplyr::filter(area_code == 2L, year == 2005L, element == "import") |>
      dplyr::pull(value)
  }
  # Before: FBS_Old was scaled up to the inflated new vintage.
  expect_equal(importer_2005(whep:::.select_best_source(rows)), 300)
  # After: the rebased new vintage is on the old level, so it stays put.
  expect_equal(importer_2005(.nut_select(rows)), 100)
})

test_that("rebasing does not modify the frame it was given", {
  fbs_new <- data.table::as.data.table(
    .nut_vintage(.nut_fbs_rows(), "FAOSTAT_FBS_New")
  )
  before <- data.table::copy(fbs_new)
  rebased <- whep:::.rebase_fbs_trade(fbs_new, .nut_factors())
  expect_gt(nrow(attr(rebased, "rebased_keys")), 0L)
  expect_identical(fbs_new, before)
})

test_that("as_published leaves the new vintage as FAOSTAT published it", {
  rows <- .nut_fbs_rows()
  factors <- whep:::.fbs_rebase_factors(
    .nut_vintage(rows, "FAOSTAT_FBS_New"),
    .nut_vintage(rows, "FAOSTAT_FBS_Old"),
    "as_published"
  )
  expect_equal(nrow(factors), 0L)
  expect_identical(
    .nut_select(rows, factors),
    whep:::.select_best_source(rows)
  )
})

test_that(".fbs_rebase_factors takes old over new, clamped to [lower, 1]", {
  factors <- .nut_factors()
  importer <- factors[factors$area_code == 2L & factors$element == "import", ]
  expect_equal(importer$rebase_old_new, 1 / 3)
  expect_equal(factors$rebase_old_new[factors$area_code == 1L], 1)

  # The defect only inflates: an old vintage above the new one is not
  # carried into a correction that adds mass.
  low_new <- .nut_fbs_rows() |>
    dplyr::mutate(
      value = dplyr::if_else(source == "FAOSTAT_FBS_New", value / 3, value)
    )
  expect_true(all(.nut_factors(low_new)$rebase_old_new == 1))

  # A 10x gap is clamped to the forward scaling's lower bound.
  wild <- .nut_fbs_rows() |>
    dplyr::mutate(
      value = dplyr::if_else(source == "FAOSTAT_FBS_New", value * 10, value)
    )
  bounds <- whep:::.fbs_scale_ratio_bounds()
  wild_import <- .nut_factors(wild) |>
    dplyr::filter(area_code == 2L, element == "import")
  expect_equal(wild_import$rebase_old_new, bounds$lower)
})

test_that("a correction larger than the row's food leaves it as published", {
  # In 2016 the importer re-exports almost all it imports and eats 10 t. The
  # cut the old vintage implies (420 t) exceeds that, so the old vintage is
  # no anchor for the row and it stays as FAOSTAT published it.
  rows <- .nut_fbs_rows() |>
    dplyr::mutate(
      value = dplyr::case_when(
        source != "FAOSTAT_FBS_New" | area_code != 2L | year != 2016L ~ value,
        element %in% c("food", "domestic_supply") ~ 10,
        element == "export" ~ 620,
        .default = value
      )
    )
  after <- .nut_wide(.nut_select(rows), 2016L)
  published <- .nut_wide(whep:::.select_best_source(rows), 2016L)
  expect_equal(after$import, published$import)
  expect_equal(after$food, published$food)
})

test_that(".fbs_rebase_factors needs two overlap years and skips other items", {
  thin <- .nut_fbs_rows() |>
    dplyr::filter(!(source == "FAOSTAT_FBS_Old" & year > 2010L))
  testthat::local_mocked_bindings(
    .fbs_overlap_frames = function(fbs_new, fbs_old) {
      list(new = fbs_new, old = fbs_old)
    }
  )
  expect_equal(nrow(.nut_factors(thin)), 0L)

  wheat <- .nut_fbs_rows() |>
    dplyr::mutate(item_cbs = "Wheat and products", item_cbs_code = 2511)
  expect_equal(nrow(.nut_factors(wheat)), 0L)
})

test_that(".fbs_rebase_factors reads the overlap a build does not span", {
  rows <- .nut_fbs_rows()
  spanning <- .nut_factors(rows)
  read <- character()
  testthat::local_mocked_bindings(
    .extract_cb = function(pin_alias, years = NULL, ...) {
      read <<- c(read, pin_alias)
      src <- c(
        "faostat-fbs-new" = "FAOSTAT_FBS_New",
        "faostat-fbs-old" = "FAOSTAT_FBS_Old"
      )[[pin_alias]]
      .nut_vintage(rows, src) |> dplyr::filter(year %in% years)
    }
  )
  late_new <- .nut_vintage(rows, "FAOSTAT_FBS_New") |>
    dplyr::filter(year >= 2015L)
  factors <- whep:::.fbs_rebase_factors(late_new, NULL, "rebased")

  expect_setequal(read, c("faostat-fbs-new", "faostat-fbs-old"))
  expect_equal(factors, spanning)

  # A build spanning the overlap reads nothing more.
  read <- character()
  .nut_factors(rows)
  expect_length(read, 0L)
})

test_that(".fbs_rebase_factors reads nothing when the build has no nuts", {
  testthat::local_mocked_bindings(
    .extract_cb = function(...) cli::cli_abort("no read expected")
  )
  early <- .nut_fbs_rows() |> dplyr::filter(year < 2010L)
  factors <- whep:::.fbs_rebase_factors(
    .nut_vintage(early, "FAOSTAT_FBS_New"),
    .nut_vintage(early, "FAOSTAT_FBS_Old"),
    "rebased"
  )
  expect_equal(nrow(factors), 0L)
})

test_that("an absent old vintage is reported, not passed off as rebased", {
  rows <- .nut_fbs_rows()
  fbs_new <- .nut_vintage(rows, "FAOSTAT_FBS_New")
  no_old <- .nut_vintage(rows, "FAOSTAT_FBS_Old") |> dplyr::filter(FALSE)
  supplied <- .nut_factors(rows)
  testthat::local_mocked_bindings(
    .fbs_overlap_frames = function(fbs_new, fbs_old) {
      list(new = fbs_new, old = no_old)
    }
  )
  factors <- whep:::.fbs_rebase_factors(fbs_new, no_old, "rebased")

  # Every row still balances -- the new vintage's own rows do -- so no
  # identity on the output can notice the rebase did not happen. The guard
  # must.
  recent <- rows |> dplyr::filter(year >= 2010L)
  selected <- .nut_select(recent, factors) |>
    dplyr::select(year, area_code, element, value) |>
    tidyr::pivot_wider(names_from = element, values_from = value)
  expect_supplied_guard(
    identity = all(
      selected$production + selected$import - selected$export ==
        selected$domestic_supply
    ),
    guard = whep:::.warn_fbs_rebase_absent(fbs_new, factors, "rebased"),
    class = "whep_fbs_rebase_absent",
    condition = "warning"
  )
  expect_no_warning(
    whep:::.warn_fbs_rebase_absent(fbs_new, factors, "as_published")
  )
  expect_no_warning(
    whep:::.warn_fbs_rebase_absent(fbs_new, supplied, "rebased")
  )
})

test_that("build_commodity_balances validates fbs_trade_basis", {
  expect_error(
    whep::build_commodity_balances(fbs_trade_basis = "bogus", example = TRUE),
    class = "rlang_error"
  )
})

# -- Milk and the other processed-heavy items (whep#1550) ----------------------

# The same two areas trading Milk - Excluding Butter. The new vintage books the
# importer's side at 3x, and the importer uses 60% of its supply as food and
# 40% as feed: FAOSTAT's import side converts cheese, which goes on into food,
# and whey powder, which goes on into feed, to milk equivalent at far more
# than its export side does.
.milk_fbs_rows <- function() {
  .nut_fbs_rows() |>
    tidyr::pivot_wider(names_from = element, values_from = value) |>
    dplyr::mutate(
      feed = dplyr::if_else(area_code == 2L, 0.4 * domestic_supply, 0),
      food = domestic_supply - feed
    ) |>
    tidyr::pivot_longer(
      c(production, export, import, food, feed, domestic_supply),
      names_to = "element"
    ) |>
    dplyr::mutate(item_cbs = "Milk - Excluding Butter", item_cbs_code = 2848)
}

.factors_for <- function(rows, basis = "rebased_with_milk") {
  whep:::.fbs_rebase_factors(
    .nut_vintage(rows, "FAOSTAT_FBS_New"),
    .nut_vintage(rows, "FAOSTAT_FBS_Old"),
    basis
  )
}

test_that("milk rebased with milk closes the world, out of food and feed", {
  rows <- .milk_fbs_rows()
  before <- .nut_wide(whep:::.select_best_source(rows), 2016L)
  after <- .nut_wide(.nut_select(rows, .factors_for(rows)), 2016L)

  expect_equal(sum(before$import) / sum(before$export), 3)
  expect_equal(sum(after$import) / sum(after$export), 1)
  # 2016: the importer's 630 t import falls to 210 t. The 420 t cut comes out
  # of food (378 t) and feed (252 t) in proportion, so the 60:40 split holds.
  expect_equal(after$food, c(50, 126))
  expect_equal(after$feed, c(0, 84))
  expect_equal(after$production, before$production)
  expect_equal(
    after$production + after$import - after$export,
    after$domestic_supply
  )
  expect_equal(after$food + after$feed, after$domestic_supply)
})

test_that("a milk cut beyond food but within food and feed is applied", {
  # The importer feeds most of its milk: the 420 t cut is larger than its
  # 63 t of food but not than food plus feed, as for the Netherlands at 2010.
  rows <- .milk_fbs_rows() |>
    dplyr::mutate(
      value = dplyr::case_when(
        source != "FAOSTAT_FBS_New" | area_code != 2L | year != 2016L ~ value,
        element == "food" ~ 63,
        element == "feed" ~ 567,
        .default = value
      )
    )
  after <- .nut_wide(.nut_select(rows, .factors_for(rows)), 2016L)
  expect_equal(after$import[2], 210)
  expect_equal(after$food[2], 21)
  expect_equal(after$feed[2], 189)
})

test_that("a milk cut beyond food and feed leaves the row as published", {
  rows <- .milk_fbs_rows() |>
    dplyr::mutate(
      value = dplyr::case_when(
        source != "FAOSTAT_FBS_New" | area_code != 2L | year != 2016L ~ value,
        element %in% c("food", "feed") ~ 5,
        element == "domestic_supply" ~ 10,
        element == "export" ~ 620,
        .default = value
      )
    )
  after <- .nut_wide(.nut_select(rows, .factors_for(rows)), 2016L)
  published <- .nut_wide(whep:::.select_best_source(rows), 2016L)
  expect_equal(after, published)
})

test_that("milk stays as published under the default", {
  rows <- .milk_fbs_rows()
  expect_identical(
    .nut_select(rows, .factors_for(rows, "rebased")),
    whep:::.select_best_source(rows)
  )
})

test_that("nut feed is not a destiny the rebase takes from", {
  # Only food absorbed the nut excess. The same rows under 2551: the 420 t
  # cut exceeds the importer's 378 t of food, and feed is no destiny it may
  # take from, so the row stays as published.
  rows <- .milk_fbs_rows() |>
    dplyr::mutate(item_cbs = "Nuts and products", item_cbs_code = 2551)
  after <- .nut_wide(.nut_select(rows, .factors_for(rows)), 2016L)
  expect_equal(after$import[2], 630)
  expect_equal(after$food[2], 378)
  expect_equal(after$feed[2], 252)
})

test_that("fbs_trade_basis selects which items are rebased", {
  expect_setequal(
    whep:::.fbs_rebase_items("rebased"),
    c(2551L, 2563L, 2601L, 2612L, 2618L)
  )
  expect_setequal(
    whep:::.fbs_rebase_items("rebased_with_milk"),
    c(2551L, 2563L, 2601L, 2612L, 2618L, 2848L)
  )
  expect_identical(whep:::.fbs_rebase_items("nuts_only"), 2551L)
  expect_length(whep:::.fbs_rebase_items("as_published"), 0L)

  milk <- .milk_fbs_rows()
  expect_gt(nrow(.factors_for(milk, "rebased_with_milk")), 0L)
  expect_equal(nrow(.factors_for(milk, "rebased")), 0L)
  expect_equal(nrow(.factors_for(milk, "nuts_only")), 0L)
  # The processed fruit items are rebased by default.
  olives <- .nut_fbs_rows() |>
    dplyr::mutate(
      item_cbs = "Olives (including preserved)",
      item_cbs_code = 2563
    )
  expect_gt(nrow(.factors_for(olives, "rebased")), 0L)
  # Under "nuts_only" the nut rebase is exactly the whep#1495 one.
  nuts <- .nut_fbs_rows()
  expect_identical(.factors_for(nuts, "nuts_only"), .nut_factors(nuts))
})

test_that("every rebased item names at least one absorbing destiny", {
  destinies <- whep:::.fbs_rebase_destinies()
  expect_setequal(
    unique(destinies$item_cbs_code),
    whep:::.fbs_rebase_items("rebased_with_milk")
  )
  expect_true(all(destinies$element %in% c("food", "feed")))
  expect_setequal(
    destinies$element[destinies$item_cbs_code == 2848L],
    c("food", "feed")
  )
})
