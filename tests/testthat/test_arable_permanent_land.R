# The FAOSTAT RL vocabulary guard (whep#1034). `element == "Area"` and the
# "1000 ha" unit label are whole-table filters: if either moves, every row
# goes and the land base becomes "no cropland anywhere" rather than an error.

.rl_raw_fixture <- function() {
  tibble::tribble(
    ~"Area Code", ~"Item Code", ~"Element", ~"Unit", ~"Year", ~"Value",
    10L,          6620L,        "Area",     "1000 ha", 2010L,  1000,
    10L,          6621L,        "Area",     "1000 ha", 2010L,  700,
    10L,          6650L,        "Area",     "1000 ha", 2010L,  300
  )
}

testthat::test_that("the FAO land identity holds on a vacuous read", {
  # The identity this reader enforces -- cropland = arable + permanent -- is
  # satisfied perfectly by a table with no rows, which is what a moved label
  # produces. That is the state the guard has to catch instead.
  empty <- tibble::tibble(
    area_code = integer(),
    year = integer(),
    item_code = integer(),
    ha = numeric()
  )
  wide <- whep:::.fao_rl_to_wide(empty)

  expect_supplied_guard(
    identity = all(
      wide$cropland_ha == wide$arable_ha + wide$permanent_ha
    ),
    guard = whep::get_arable_permanent_land(
      years = 2010L,
      data = dplyr::mutate(.rl_raw_fixture(), Element = "Area harvested")
    )
  )
})

testthat::test_that("a boolean Unit column is refused, not filtered away", {
  # whep#1025: the sibling faostat-cbs-new pin shipped every one of its
  # 58,107 Unit values as the boolean TRUE. The same corruption here removes
  # every land row.
  corrupted <- dplyr::mutate(.rl_raw_fixture(), Unit = TRUE)

  testthat::expect_error(
    whep::get_arable_permanent_land(years = 2010L, data = corrupted),
    class = "whep_absent_label"
  )
})

testthat::test_that("the refusal names the label the pin now carries", {
  renamed <- dplyr::mutate(
    .rl_raw_fixture(),
    Element = "Area under cultivation"
  )

  testthat::expect_error(
    whep::get_arable_permanent_land(years = 2010L, data = renamed),
    "Area under cultivation"
  )
})

testthat::test_that("the accepted unit spellings all still read", {
  # "1000 ha", "1000 Ha" and "1000ha" are one label, and folding them is what
  # lets the guard above assert a single spelling.
  spellings <- c("1000 ha", "1000 Ha", "1000ha")
  totals <- purrr::map_dbl(spellings, function(unit) {
    raw <- dplyr::mutate(.rl_raw_fixture(), Unit = unit)
    sum(whep::get_arable_permanent_land(years = 2010L, data = raw)$cropland_ha)
  })

  testthat::expect_equal(totals, rep(1e6, length(spellings)))
})

testthat::test_that("an intact table is read unchanged", {
  out <- whep::get_arable_permanent_land(
    years = 2010L,
    data = .rl_raw_fixture()
  )

  pointblank::expect_col_exists(
    out,
    c("area_code", "year", "arable_ha", "permanent_ha", "cropland_ha")
  )
  testthat::expect_equal(out$cropland_ha, 1e6)
  testthat::expect_equal(out$arable_ha, 7e5)
  testthat::expect_equal(out$permanent_ha, 3e5)
})

testthat::test_that("a logical Note in the landuse pin does not move the result", {
  # whep#1178: the registered faostat-landuse pin carries `Note` as an all-NA
  # logical (readr's type guess on an empty column). The reader never selects
  # it; the result must not depend on which type it has.
  read <- function(note) {
    whep::get_arable_permanent_land(
      years = 2010L,
      data = dplyr::mutate(.rl_raw_fixture(), Note = note)
    )
  }

  testthat::expect_identical(read(NA), read(NA_character_))
  testthat::expect_equal(nrow(read(NA)), 1L)
})

# --- The 2020 fodder break is announced in the output (whep#938) -------------

# Country 10 reports fodder mix to 2019 and none after, the shape the FAOSTAT
# fodder items take in the real panel. Country 20 never reports fodder, and
# country 30 has only a perennial, so no arable base row at all.
.fodder_break_base <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~impact_u,
    2018L, 10L,        2003L,          100,
    2018L, 10L,        2511L,          900,
    2019L, 10L,        2003L,          100,
    2019L, 10L,        2511L,          900,
    2020L, 10L,        2511L,          900,
    2021L, 10L,        2511L,          900,
    2020L, 20L,        2511L,          500,
    2020L, 30L,        2560L,          50
  )
}

.fodder_break_arable <- function() {
  tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha,
    10L,        2018L, 1200,       0,
    10L,        2019L, 1200,       0,
    10L,        2020L, 1200,       0,
    10L,        2021L, 1200,       0,
    20L,        2020L, 600,        0,
    30L,        2020L, 0,          50
  )
}

.fodder_break_items <- function() {
  tibble::tribble(
    ~item_cbs_code, ~Herb_Woody,
    2003L,          "Herbaceous", # fodder mix
    2511L,          "Herbaceous", # wheat
    2560L,          "Woody" # coconuts, perennial
  )
}

.fodder_break_extension <- function(...) {
  suppressWarnings(whep::build_fao_arable_fallow_extension(
    base_extension = .fodder_break_base(),
    arable_permanent = .fodder_break_arable(),
    temporary_grassland = tibble::tibble(
      area_code = integer(),
      year = integer(),
      item_cbs_code = integer(),
      impact_u = numeric()
    ),
    items_prod_full = .fodder_break_items(),
    ...
  ))
}

.coverage_by_year <- function(res, area) {
  res |>
    dplyr::filter(area_code == area) |>
    dplyr::distinct(year, fodder_coverage) |>
    dplyr::arrange(year)
}

testthat::test_that("fodder_coverage marks the years after fodder stops", {
  res <- .fodder_break_extension()

  pointblank::expect_col_vals_in_set(
    res,
    fodder_coverage,
    c("reported", "lapsed", "not_reported")
  )
  pointblank::expect_col_vals_not_null(res, fodder_coverage)
  testthat::expect_equal(
    .coverage_by_year(res, 10L)$fodder_coverage,
    c("reported", "reported", "lapsed", "lapsed")
  )
  # One label per country-year: every row of a country-year agrees.
  testthat::expect_equal(
    nrow(dplyr::distinct(res, area_code, year, fodder_coverage)),
    nrow(dplyr::distinct(res, area_code, year))
  )
  testthat::expect_equal(
    .coverage_by_year(res, 20L)$fodder_coverage,
    "not_reported"
  )
  # A perennial-only country-year has no fodder input either.
  testthat::expect_equal(
    .coverage_by_year(res, 30L)$fodder_coverage,
    "not_reported"
  )
})

testthat::test_that("fodder_coverage describes the input, not the treatment", {
  # carry_forward fills 2020-2021 with fodder, but the label still says the
  # input had none there, so a filled year cannot pass for a reported one.
  carried <- .fodder_break_extension(fodder_gap = "carry_forward")
  testthat::expect_true(any(
    carried$item_cbs_code == 2003L & carried$year == 2021L
  ))
  testthat::expect_equal(
    .coverage_by_year(carried, 10L)$fodder_coverage,
    c("reported", "reported", "lapsed", "lapsed")
  )
  dropped <- .fodder_break_extension(fodder_gap = "drop")
  testthat::expect_false(any(dropped$item_cbs_code == 2003L))
  testthat::expect_equal(
    .coverage_by_year(dropped, 10L)$fodder_coverage,
    c("reported", "reported", "lapsed", "lapsed")
  )
})

testthat::test_that("the default leaves the reconciled numbers unchanged", {
  res <- .fodder_break_extension()
  # 2020: fodder gone, wheat alone reconciles to the whole 1200 ha -- the
  # break itself, which this PR announces and does not move.
  wheat <- res |> dplyr::filter(area_code == 10L, item_cbs_code == 2511L)
  testthat::expect_equal(wheat$impact_u, c(1080, 1080, 1200, 1200))
})

testthat::test_that("check_series_jumps sees the fodder break with dropouts", {
  res <- .fodder_break_extension() |>
    dplyr::filter(!is.na(item_cbs_code))
  quiet <- whep::check_series_jumps(
    res,
    impact_u,
    .by = c("area_code", "item_cbs_code"),
    verbose = FALSE
  )
  testthat::expect_equal(nrow(quiet), 0L)

  flags <- whep::check_series_jumps(
    res,
    impact_u,
    .by = c("area_code", "item_cbs_code"),
    dropouts = "area_code",
    verbose = FALSE
  )
  # Only the fodder series: countries 20 and 30 end with the panel of their
  # own, which is not a composition change.
  testthat::expect_equal(flags$area_code, 10L)
  testthat::expect_equal(flags$item_cbs_code, 2003L)
  testthat::expect_identical(flags$year, 2020L)
  testthat::expect_equal(flags$ratio, 0)
})
