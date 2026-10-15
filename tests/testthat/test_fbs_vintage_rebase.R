# -- FBS 2551 vintage rebase (whep#1495) --------------------------------------

# Two areas trading nuts. The old vintage closes the world: the exporter's 100
# is the importer's 100. The new vintage books the importer's side of the same
# flow at 3x (FAOSTAT's import-side standardisation of kernels), the
# exporter's at 1x, so its world imports are 3x its world exports.
.nut_fbs_rows <- function() {
  old <- tidyr::expand_grid(year = 2005:2013, area_code = c(1L, 2L)) |>
    dplyr::mutate(
      element = dplyr::if_else(area_code == 1L, "export", "import"),
      value = 100 + (year - 2005) * 10,
      source = "FAOSTAT_FBS_Old"
    )
  new <- tidyr::expand_grid(year = 2010:2016, area_code = c(1L, 2L)) |>
    dplyr::mutate(
      element = dplyr::if_else(area_code == 1L, "export", "import"),
      value = (100 + (year - 2005) * 10) * dplyr::if_else(area_code == 2L, 3, 1),
      source = "FAOSTAT_FBS_New"
    )
  dplyr::bind_rows(old, new) |>
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
    "old_fbs"
  )
}

.world_trade <- function(selected, yr) {
  selected |>
    dplyr::filter(year == yr) |>
    dplyr::summarise(value = sum(value), .by = element) |>
    tibble::deframe()
}

test_that("rebased nuts close world trade where the new vintage does not", {
  rows <- .nut_fbs_rows()
  as_published <- whep:::.select_best_source(rows)
  rebased <- whep:::.select_best_source(rows, rebase = .nut_factors(rows))

  # The identity the new vintage breaks, and this fix restores, in a year
  # only the new vintage reports.
  before <- .world_trade(as_published, 2016L)
  after <- .world_trade(rebased, 2016L)
  expect_equal(unname(before["import"] / before["export"]), 3)
  expect_equal(unname(after["import"] / after["export"]), 1)
})

test_that("rebased nuts keep the old vintage before 2014, unscaled", {
  rows <- .nut_fbs_rows()
  rebased <- whep:::.select_best_source(rows, rebase = .nut_factors(rows))
  importer <- rebased |>
    dplyr::filter(area_code == 2L) |>
    dplyr::arrange(year)

  # 2005: the forward scaling would have tripled it to 300.
  expect_equal(importer$value[importer$year == 2005L], 100)
  expect_equal(importer$source[importer$year == 2005L], "FAOSTAT_FBS_Old")
  # 2011: the old vintage's own number, not the new one divided by a ratio.
  expect_equal(importer$value[importer$year == 2011L], 160)
  expect_equal(importer$source[importer$year == 2011L], "FAOSTAT_FBS_Old")
  # 2016: the new vintage's number on the old level, labelled as derived.
  expect_equal(importer$value[importer$year == 2016L], 210)
  expect_equal(
    importer$source[importer$year == 2016L],
    "FAOSTAT_FBS_New_rebased"
  )
})

test_that("without a rebase the forward scaling is unchanged", {
  rows <- .nut_fbs_rows()
  old_way <- whep:::.select_best_source(rows)
  empty <- whep:::.select_best_source(
    rows,
    rebase = whep:::.fbs_rebase_factors(
      .nut_vintage(rows, "FAOSTAT_FBS_New"),
      .nut_vintage(rows, "FAOSTAT_FBS_Old"),
      "new_fbs"
    )
  )
  expect_identical(empty, old_way)
  importer_2005 <- old_way |>
    dplyr::filter(area_code == 2L, year == 2005L)
  expect_equal(importer_2005$value, 300)
  expect_equal(importer_2005$source, "FAOSTAT_FBS_Old_scaled")
})

test_that("a rebased FBS_New value keeps FBS_New's flag", {
  rows <- .nut_fbs_rows() |>
    dplyr::mutate(
      fao_flag = dplyr::if_else(source == "FAOSTAT_FBS_New", "I", "S")
    )
  rebased <- whep:::.select_best_source(rows, rebase = .nut_factors(rows)) |>
    dplyr::filter(source == "FAOSTAT_FBS_New_rebased")
  expect_gt(nrow(rebased), 0L)
  expect_true(all(rebased$fao_flag == "I"))
})

test_that(".fbs_rebase_factors takes the clamped median of old over new", {
  factors <- .nut_factors()
  expect_setequal(factors$area_code, c(1L, 2L))
  expect_equal(factors$rebase_old_new[factors$area_code == 1L], 1)
  expect_equal(factors$rebase_old_new[factors$area_code == 2L], 1 / 3)

  # A 10x gap is clamped to the forward scaling's band.
  wild <- .nut_fbs_rows() |>
    dplyr::mutate(
      value = dplyr::if_else(source == "FAOSTAT_FBS_New", value * 10, value)
    )
  bounds <- whep:::.fbs_scale_ratio_bounds()
  expect_true(all(.nut_factors(wild)$rebase_old_new == bounds$lower))
})

test_that(".fbs_rebase_factors needs two overlap years and only rebases 2551", {
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
  read <- character()
  testthat::local_mocked_bindings(
    .extract_cb = function(pin_alias, years = NULL, ...) {
      read <<- c(read, pin_alias)
      expect_equal(years, 2010:2013)
      src <- c(
        "faostat-fbs-new" = "FAOSTAT_FBS_New",
        "faostat-fbs-old" = "FAOSTAT_FBS_Old"
      )[[pin_alias]]
      .nut_vintage(rows, src) |> dplyr::filter(year %in% years)
    }
  )
  late_new <- .nut_vintage(rows, "FAOSTAT_FBS_New") |>
    dplyr::filter(year >= 2015L)
  factors <- whep:::.fbs_rebase_factors(late_new, NULL, "old_fbs")

  expect_setequal(read, c("faostat-fbs-new", "faostat-fbs-old"))
  expect_equal(factors, .nut_factors(rows))

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
    "old_fbs"
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
  factors <- whep:::.fbs_rebase_factors(fbs_new, no_old, "old_fbs")

  # The selection runs and returns exactly what FAOSTAT published, so no
  # identity on its output can notice the rebase did not happen. The guard
  # must.
  selected <- whep:::.select_best_source(
    rows |> dplyr::filter(source == "FAOSTAT_FBS_New"),
    rebase = factors
  )
  expect_supplied_guard(
    identity = sum(selected$value) == sum(fbs_new$value),
    guard = whep:::.warn_fbs_rebase_absent(fbs_new, factors, "old_fbs"),
    class = "whep_fbs_rebase_absent",
    condition = "warning"
  )
  expect_no_warning(
    whep:::.warn_fbs_rebase_absent(fbs_new, factors, "new_fbs")
  )
  expect_no_warning(
    whep:::.warn_fbs_rebase_absent(fbs_new, supplied, "old_fbs")
  )
})

test_that("build_commodity_balances validates nut_basis", {
  expect_error(
    whep::build_commodity_balances(nut_basis = "bogus", example = TRUE),
    class = "rlang_error"
  )
})
