test_that("resolve_polity_lineage leaves a modern row on its anchor", {
  national <- tibble::tribble(
    ~area_code, ~year, ~harvested_area_ha,
           185, 2015L,           50000000
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "RUS-2014-2025",      2014L,     2025L
  )
  out <- whep::resolve_polity_lineage(national, support)
  expect_equal(out$lineage_polity_code, "RUS-2014-2025")
  expect_equal(out$method_polity_lineage, "anchor")
  expect_equal(nrow(out), nrow(national))
})

test_that("a successor row in a predecessor year lands on the predecessor", {
  national <- tibble::tribble(
    ~area_code, ~year, ~harvested_area_ha,
           185, 1961L,           85049155
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",      1945L,     1991L
  )
  out <- whep::resolve_polity_lineage(national, support)
  expect_equal(out$lineage_polity_code, "F228-1945-1991")
  expect_equal(out$method_polity_lineage, "predecessor")
})

test_that("the walk prefers the interval the support actually carries", {
  # `polities` carries two overlapping Yugoslav intervals, F248-1920-1991 and
  # F248-1947-1991; the published support emitted only the second. A lineage
  # resolved against `polities` alone answers with the first and still finds
  # no cell.
  national <- tibble::tribble(
    ~area_code, ~year,
            98, 1961L
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F248-1947-1991",      1947L,     1991L
  )
  out <- whep::resolve_polity_lineage(national, support)
  expect_equal(out$lineage_polity_code, "F248-1947-1991")
  expect_equal(out$method_polity_lineage, "sibling_interval")
})

test_that("constant_territory keeps the anchor and is recorded as such", {
  national <- tibble::tribble(
    ~area_code, ~year,
           185, 1961L
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",      1945L,     1991L
  )
  out <- whep::resolve_polity_lineage(
    national,
    support,
    basis = "constant_territory"
  )
  expect_equal(out$lineage_polity_code, "RUS-1991-2014")
  expect_equal(out$method_polity_lineage, "constant_territory")
})

test_that("an unresolvable row keeps NA and is warned about, not dropped", {
  national <- tibble::tribble(
    ~area_code, ~year,
           185, 1961L,
           249, 1961L
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",      1945L,     1991L
  )
  expect_warning(
    out <- whep::resolve_polity_lineage(national, support),
    class = "whep_lineage_unresolved"
  )
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$lineage_polity_code[out$area_code == 249]))
  expect_equal(out$method_polity_lineage[out$area_code == 249], "unresolved")
})

test_that("an empty support is refused rather than answered from polities", {
  national <- tibble::tribble(
    ~area_code, ~year,
           185, 1961L
  )
  empty <- tibble::tibble(
    polity_code = character(0),
    start_year = integer(0),
    end_year = integer(0)
  )
  expect_error(
    whep::resolve_polity_lineage(national, empty),
    class = "whep_lineage_support_empty"
  )
})

test_that("a support covering none of the requested years is refused", {
  national <- tibble::tribble(
    ~area_code, ~year,
           185, 1961L
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "RUS-2014-2025",      2014L,     2025L
  )
  expect_error(
    whep::resolve_polity_lineage(national, support),
    class = "whep_lineage_support_year"
  )
})

test_that("a missing column is named rather than silently tolerated", {
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",      1945L,     1991L
  )
  expect_error(
    whep::resolve_polity_lineage(tibble::tibble(year = 1961L), support)
  )
  expect_error(
    whep::resolve_polity_lineage(
      tibble::tibble(area_code = 185L, year = 1961L),
      dplyr::select(support, -"end_year")
    )
  )
})

test_that("an unknown basis is refused by arg_match", {
  national <- tibble::tribble(
    ~area_code, ~year,
           185, 1961L
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",      1945L,     1991L
  )
  expect_error(
    whep::resolve_polity_lineage(national, support, basis = "modern"),
    class = "rlang_error"
  )
})

test_that("a polities table with no predecessor edge cannot claim success", {
  # The edges are the lineage's only input. A `polities` table that carries
  # none of them still satisfies every totals check -- the anchor is returned
  # and every row is accounted for -- so the supply of the edges is asserted
  # rather than inferred from the output reconciling.
  national <- tibble::tribble(
    ~area_code, ~year,
           185, 1961L
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",      1945L,     1991L
  )
  edgeless <- dplyr::mutate(whep::polities, predecessor = NA_character_)
  expect_error(
    whep::resolve_polity_lineage(national, support, polities = edgeless),
    class = "whep_lineage_no_edges"
  )
})

test_that("the lineage is joined on codes and adds no row", {
  national <- tibble::tribble(
    ~area_code, ~year, ~item_prod_code, ~harvested_area_ha,
           185, 1961L,             15L,           20000000,
           185, 1961L,             27L,           10000000,
           108, 1961L,             15L,           22511306
  )
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",      1945L,     1991L
  )
  out <- whep::resolve_polity_lineage(national, support)
  expect_equal(nrow(out), nrow(national))
  expect_equal(
    sum(out$harvested_area_ha),
    sum(national$harvested_area_ha)
  )
  expect_true(all(out$lineage_polity_code == "F228-1945-1991"))
})

test_that("the 1961 areas a year-aware support cannot place resolve", {
  # The 27 reporting areas measured as unplaceable at 1961 on the live
  # `polycell_support` pin (20260907T111653Z-e654d), carrying 164.2 Mha of
  # harvested area, 17.2% of the world. The support here is the handful of
  # predecessor intervals that pin really holds at 1961, hand-built so the
  # test is offline; the verdicts are the ones the pin produces.
  codes <- c(
    1L,
    52L,
    57L,
    73L,
    80L,
    98L,
    108L,
    113L,
    146L,
    154L,
    167L,
    185L,
    198L,
    199L,
    208L,
    213L,
    230L,
    235L,
    237L,
    249L,
    272L,
    273L,
    276L,
    901L,
    903L,
    904L,
    906L
  )
  support <- tibble::tribble(
    ~polity_code,        ~start_year, ~end_year,
    "F228-1945-1991",          1945L,     1991L,
    "AZE-SSR-1920-1991",       1920L,     1991L,
    "F51-1947-1993",           1947L,     1993L,
    "F248-1947-1991",          1947L,     1991L,
    "SUD-1956-2011",           1956L,     2011L,
    "DRV-1954-1975",           1954L,     1975L,
    "RVN-1954-1975",           1954L,     1975L
  )
  national <- tibble::tibble(area_code = codes, year = 1961L)

  out <- suppressWarnings(whep::resolve_polity_lineage(national, support))

  expect_equal(nrow(out), length(codes))
  expect_equal(sum(out$method_polity_lineage != "unresolved"), 21L)
  expect_equal(
    out$lineage_polity_code[out$area_code == 185],
    "F228-1945-1991"
  )
  expect_equal(
    out$lineage_polity_code[out$area_code == 98],
    "F248-1947-1991"
  )
  expect_equal(
    out$lineage_polity_code[out$area_code == 52],
    "AZE-SSR-1920-1991"
  )
  # The residue is exactly the reporting areas whose polity is an aggregate:
  # Viet Nam and Yemen, whose members the support carries separately, and the
  # four Rest-of-region buckets, which no vintage of the support carries. What
  # is missing there is a share rule, not a lineage edge.
  expect_setequal(
    out$area_code[out$method_polity_lineage == "unresolved"],
    c(237L, 249L, 901L, 903L, 904L, 906L)
  )
})

test_that("constant_territory leaves those rows on modern polities", {
  codes <- c(185L, 108L, 230L, 98L, 272L, 276L)
  support <- tibble::tribble(
    ~polity_code, ~start_year, ~end_year,
    "F228-1945-1991",    1945L,     1991L
  )
  national <- tibble::tibble(area_code = codes, year = 1961L)

  out <- whep::resolve_polity_lineage(
    national,
    support,
    basis = "constant_territory"
  )

  expect_true(all(out$method_polity_lineage == "constant_territory"))
  # Every one of them is a polity the 1961 support holds no cell for, which is
  # the attribution error this basis keeps.
  expect_false(any(out$lineage_polity_code %in% support$polity_code))
})
