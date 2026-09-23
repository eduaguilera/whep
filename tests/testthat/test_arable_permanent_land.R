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
