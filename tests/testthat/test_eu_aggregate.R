# `.eu_aggregate_iso3()` replaced a 28-element ISO3 literal that lived in
# `inst/scripts/compare_fabio_footprints.R` (whep#421). Two things have to stay
# true for that replacement to be safe, and they pull in opposite directions:
#
#   * the derivation must still be able to produce exactly the old list, so the
#     numbers the script used to print stay reachable; and
#   * the default must NOT be that list, because it omits the dissolved
#     predecessors `BLX` and `CSK` under which FABIO books four member states
#     before 2000 and 1993 respectively.
#
# The old list is therefore pinned here by identity rather than in the script.

# The literal `compare_fabio_footprints.R` carried before whep#421, verbatim.
.eu28_literal <- function() {
  c(
    "AUT",
    "BEL",
    "BGR",
    "HRV",
    "CYP",
    "CZE",
    "DNK",
    "EST",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "IRL",
    "ITA",
    "LVA",
    "LTU",
    "LUX",
    "MLT",
    "NLD",
    "POL",
    "PRT",
    "ROU",
    "SVK",
    "SVN",
    "ESP",
    "SWE",
    "GBR"
  )
}

test_that("the states basis reproduces the retired 28-element literal", {
  expect_identical(
    whep:::.eu_aggregate_iso3("eu28_states"),
    sort(.eu28_literal())
  )
})

test_that("the territory basis keeps the dissolved predecessors", {
  territory <- whep:::.eu_aggregate_iso3("eu28_territory")

  expect_true(all(c("BLX", "CSK") %in% territory))
  expect_setequal(
    setdiff(territory, .eu28_literal()),
    c("BLX", "CSK")
  )
  expect_length(setdiff(.eu28_literal(), territory), 0L)
})

test_that("the EU27 bases differ from the EU28 ones by exactly GBR", {
  expect_setequal(
    setdiff(
      whep:::.eu_aggregate_iso3("eu28_territory"),
      whep:::.eu_aggregate_iso3("eu27_territory")
    ),
    "GBR"
  )
  expect_setequal(
    setdiff(
      whep:::.eu_aggregate_iso3("eu28_states"),
      whep:::.eu_aggregate_iso3("eu27_states")
    ),
    "GBR"
  )
  expect_false("GBR" %in% whep:::.eu_aggregate_iso3("eu27_territory"))
})

test_that("the EU27 territory basis is the published flag verbatim", {
  flagged <- whep::regions_full |>
    dplyr::filter(.data$EU27 %in% TRUE, !is.na(.data$iso3c)) |>
    dplyr::pull(.data$iso3c) |>
    unique()

  expect_identical(
    whep:::.eu_aggregate_iso3("eu27_territory"),
    sort(flagged)
  )
  # The flag is the thing being read, so a silently emptied or truncated flag
  # must not pass as "a smaller EU".
  expect_gte(length(flagged), 29L)
})

test_that("every code returned is one the polities database knows", {
  known <- unique(whep::polities$iso3_code)

  purrr::walk(
    c("eu28_territory", "eu27_territory", "eu28_states", "eu27_states"),
    function(basis) {
      expect_length(setdiff(whep:::.eu_aggregate_iso3(basis), known), 0L)
    }
  )
})

test_that("the predecessors dropped by the states bases really are dissolved", {
  dropped <- setdiff(
    whep:::.eu_aggregate_iso3("eu27_territory"),
    whep:::.eu_aggregate_iso3("eu27_states")
  )
  last_open <- max(whep::polities$end_year, na.rm = TRUE)

  expect_setequal(dropped, c("BLX", "CSK"))
  purrr::walk(dropped, function(iso3c) {
    expect_lt(
      max(whep::polities$end_year[whep::polities$iso3_code %in% iso3c]),
      last_open
    )
  })
  # ... and the extant test is not vacuous the other way round: every member
  # state the states basis keeps is still open at the database's last year.
  purrr::walk(whep:::.eu_aggregate_iso3("eu27_states"), function(iso3c) {
    expect_equal(
      max(whep::polities$end_year[whep::polities$iso3_code %in% iso3c]),
      last_open
    )
  })
})

test_that("an unknown basis aborts instead of falling back", {
  expect_error(
    whep:::.eu_aggregate_iso3("eu28"),
    class = "rlang_error"
  )
})

test_that("GBR is stated once, and only the EU28 bases pick it up", {
  expect_identical(whep:::.eu_withdrawn_member_iso3(), "GBR")
  expect_false("GBR" %in% whep:::.eu_aggregate_iso3("eu27_states"))
})
