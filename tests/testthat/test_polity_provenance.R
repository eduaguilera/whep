# whep#740: the shipped crosswalk is the upstream FAOSTAT-to-polity map plus
# rows this package manufactures by ISO3-prefix match. These tests pin what the
# manufactured rows actually decide, which is much less than what they are: 262
# prefix_outside_map rows are shipped and 5 of them are ever selected.
#
# Everything here runs on package data alone -- no pin, no network, no
# `WHEP_*` path.

test_that("provenance is read off the row the resolver picked", {
  keys <- tibble::tibble(
    area_code = c(238L, 212L, 231L),
    year = c(1900L, 1975L, 2000L)
  )

  provenance <- whep::polity_mapping_provenance(keys)
  resolved <- whep::add_polity_code(keys)

  expect_equal(
    dplyr::arrange(provenance, area_code)$polity_code,
    dplyr::arrange(resolved, area_code)$polity_code
  )
})

test_that("a caller's rows are counted, not its distinct area-years", {
  keys <- tibble::tibble(
    area_code = c(238L, 238L, 238L, 212L),
    year = c(1900L, 1900L, 2000L, 2000L),
    value = 1
  )

  provenance <- whep::polity_mapping_provenance(keys)

  expect_equal(sum(provenance$n_rows), nrow(keys))
  expect_equal(
    provenance |>
      dplyr::filter(area_code == 238L, year == 1900L) |>
      dplyr::pull(n_rows),
    2L
  )
})

test_that("a missing area-code column aborts instead of resolving nothing", {
  expect_error(
    whep::polity_mapping_provenance(tibble::tibble(country = 238L)),
    "area_code"
  )
})

test_that("without a year column the current mapping is reported", {
  provenance <- whep::polity_mapping_provenance(
    tibble::tibble(area_code = c(212L, 238L))
  )

  expect_true(all(is.na(provenance$year)))
  expect_equal(
    provenance |> dplyr::filter(area_code == 238L) |> dplyr::pull(polity_code),
    "ETH-1993-2025"
  )
})

test_that("every mapping_source the crosswalk ships is classified", {
  sources <- unique(whep::polity_area_crosswalk$mapping_source)

  expect_false(any(is.na(whep:::.mapping_authority(sources))))
  expect_setequal(
    whep:::.mapping_source_authority()$mapping_source,
    sources[!is.na(sources)]
  )
})

test_that("an unclassified mapping_source aborts rather than being bucketed", {
  # The whole point of the column is to say who decided the territory, so a new
  # class of manufactured row must not arrive already looking legitimate.
  expect_error(
    whep:::.mapping_authority(c("upstream_map", "prefix_by_continent")),
    "prefix_by_continent"
  )
})

test_that("no authority beyond the four documented ones is reported", {
  provenance <- whep::polity_mapping_provenance()

  expect_setequal(
    unique(provenance$authority),
    c("upstream", "whep_prefix", "whep_bucket", "unresolved")
  )

  # `"unresolved"` has to stay confined to the areas that really resolve to no
  # polity, or a broken provenance join would look like an honest gap: 351
  # China (the aggregate the crosswalk leaves unmapped) for its whole span, and
  # 15 Belgium-Luxembourg / 151 Netherlands Antilles past their composite end,
  # which `.add_polity_columns_dt()` refuses to extend.
  expect_equal(
    sort(unique(provenance$area_code[provenance$authority == "unresolved"])),
    c(15L, 151L, 351L)
  )
})

# The guard whep#740 asks for. It does not forbid a manufactured row; it pins
# WHICH reporting areas' identity one decides, so a change that puts a WHEP
# guess under a territory upstream does answer for has to say so here.
test_that("only the recorded areas resolve through a WHEP prefix guess", {
  provenance <- whep::polity_mapping_provenance()

  guessed <- provenance |>
    dplyr::filter(authority == "whep_prefix") |>
    dplyr::pull(area_code) |>
    unique() |>
    sort()

  # 238 Ethiopia (`ETH-1952-1993`), the three Baltic areas on their
  # Soviet-annexation periods, and the six FAOSTAT regional "Other" buckets,
  # which the upstream map does not mention at all.
  #
  # 62 Ethiopia PDR left this list in whep#741: its only guessed row was
  # `ETH-1993-2025`, a period the upstream map awards to 238, and #743 stopped
  # the prefix expansion handing an area a polity outside its own fold. That is
  # the ratchet moving the right way -- lower it when a guess is retired, never
  # raise it to admit one.
  expect_equal(
    guessed,
    c(63L, 119L, 126L, 238L, 901L, 902L, 903L, 904L, 905L, 906L)
  )

  # The narrower population whep#740 proposes deleting outright.
  outside_map <- provenance |>
    dplyr::filter(mapping_source == "prefix_outside_map") |>
    dplyr::distinct(area_code, polity_code)

  expect_equal(
    sort(unique(outside_map$area_code)),
    c(63L, 119L, 126L, 238L)
  )

  # 261 `prefix_outside_map` rows are shipped and 4 of them are ever the answer
  # to an `(area_code, year)`. The other 257 decide nothing -- which is why #740
  # is NOT the 288-row deletion its title claims: the live population is tiny,
  # and one of the four (238 -> `ETH-1952-1993`) is load-bearing for 35,558 rows,
  # so it wants an upstream answer rather than a delete.
  #
  # Both numbers fell by one in whep#741/#743; see the note above.
  expect_equal(nrow(outside_map), 4L)
  expect_equal(
    sum(whep::polity_area_crosswalk$mapping_source == "prefix_outside_map"),
    261L
  )
})

test_that("the back-cast anchor is what keeps the invented periods inert", {
  # Not a preference: it is the mechanism behind the test above. The resolver
  # floors every lookup at the anchor, so the pre-1961 era resolves through
  # whatever answers 1961 and never asks which polity held the territory in
  # 1850. Remove the floor and the manufactured periods take over the era.
  anchored <- whep::polity_mapping_provenance()
  by_year <- whep::polity_mapping_provenance(backcast_anchor = -Inf)

  anchored_n <- sum(
    anchored$mapping_source == "prefix_outside_map",
    na.rm = TRUE
  )
  by_year_n <- sum(by_year$mapping_source == "prefix_outside_map", na.rm = TRUE)

  # Measured on the shipped snapshot: 599 area-years against 7,881, a 13x rise
  # over the same grid.
  expect_gt(by_year_n, 5L * anchored_n)
  expect_gt(
    dplyr::n_distinct(by_year$area_code[
      !is.na(by_year$mapping_source) &
        by_year$mapping_source == "prefix_outside_map"
    ]),
    5L
  )
})
