# `add_area_code()` routes names the crosswalk cannot match through the polities database.
#
# The crosswalk is keyed on the FAOSTAT area name, so a source spelling an area any other
# way got NA even though the code was there. Measured on `get_primary_residues()`, whose
# areas are resolved by name: 44,985 of 475,688 residue rows (9.5%) unmatched before, 200
# (0.04%) after. Two changes got there — this fallback, worth 34,963 rows, and rescoping four
# aliases upstream from source=faostat to blanket, worth another 9,822. The 200 that remain
# are all Tanzania, for the reason given under "resolution is year-aware" below.
#
# One risk was measured and is absent: filling codes can give ONE code TWO labels, and several
# builders group by `c("area", "area_code")` together, which would then split one territory in
# two — the shape behind the livestock-share defect in test_area_grouped_denominators.R. After
# the fallback, no area_code in the residue source carries more than one label (the source
# spells each area exactly one way, using its own 185-label vocabulary and no FAOSTAT long
# forms at all). This is a property of that source rather than a guarantee of the fallback: a
# caller whose table mixes spellings will get one code under both, which is correct for the
# code and worth knowing before grouping on the name.

test_that("a short-form name reaches its FAOSTAT code", {
  table <- tibble::tibble(
    area = c("Netherlands", "Tanzania", "United Kingdom", "South Korea"),
    year = 2000L
  )
  expect_equal(
    add_area_code(table, name_column = "area")$area_code,
    c(150L, 215L, 229L, 117L)
  )
})

test_that("a name the crosswalk already matches is left alone", {
  # The fallback fills NA only. Anything the exact join answered keeps that answer, so the
  # crosswalk stays authoritative wherever it has one and this cannot move existing data.
  table <- tibble::tibble(
    area_name = c("Armenia", "Afghanistan", "Albania"),
    year = 2000L
  )
  with_fallback <- add_area_code(table)
  expect_equal(with_fallback$area_code, c(1L, 2L, 3L))
})

test_that("an unknown name stays NA", {
  table <- tibble::tibble(area_name = "Dummy Country", year = 2000L)
  expect_true(is.na(add_area_code(table)$area_code))
})

test_that("resolution is year-aware", {
  # "Czechoslovakia" and "Czechia" are separate reporting areas (51 and 167), and the year
  # decides which a label means. Tanzania shows the other side: 1961-1964 rows resolve to
  # TAN-1922-1964 through an alias written against the years in the CODE, while that
  # polity's own columns end in 1961 and it carries no reporting area — the downstream cost
  # of the code/column disagreement baselined in whep-polities'
  # validate_code_year_agreement.py.
  table <- tibble::tibble(
    area = c("Tanzania", "Tanzania"),
    year = c(2000L, 1962L)
  )
  out <- add_area_code(table, name_column = "area")
  expect_equal(out$area_code[1], 215L)
  expect_true(is.na(out$area_code[2]))
})

test_that("a table with no year column still resolves unambiguous names", {
  # Without a year only names carried by exactly one polity can resolve, which is the same
  # rule the alias route applies when no year is given. The ambiguous half of this belongs
  # to the resolver's own tests, not here: an ambiguous polity name like "Italy" or "Peru"
  # is also a FAOSTAT area name, so the exact join answers it long before the fallback runs.
  table <- tibble::tibble(area = c("Netherlands", "Dummy Country"))
  out <- add_area_code(table, name_column = "area")
  expect_equal(out$area_code[1], 150L)
  expect_true(is.na(out$area_code[2]))
})

test_that("the fallback preserves row count and column type", {
  table <- tibble::tibble(
    area = c("Netherlands", "Dummy Country", "Armenia"),
    year = 2000L
  )
  out <- add_area_code(table, name_column = "area")
  expect_equal(nrow(out), 3L)
  expect_type(out$area_code, "integer")
})
