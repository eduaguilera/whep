# The compact country grid in inst/extdata/regions.csv is a present-day list of
# area codes with no year column, and it feeds every static country pin in the
# package. Nine of its codes used to resolve to the wrong polity (whep#459):
# seven were forced onto ROW-1850-2023 -- a non-territorial aggregate -- by the
# crosswalk's FABIO rest-of-world override, and Sudan/South Sudan resolved to
# their post-2011 successors for every year including pre-2011 ones.
#
# These tests pin the corrected mapping for all nine, and pin the invariant that
# makes the correction safe: `polity_area_code` is the numeric FABIO aggregation
# bucket and it is NOT what changed. Un-folding at the numeric level inflates
# global feed 13.7x (whep#419), so a future change that "fixes" the fold by
# promoting these areas to their own bucket must fail here rather than ship.

test_that("grid countries with their own polity are not identified as ROW", {
  # All five have a real dedicated polity in `polities` covering the reported
  # FAOSTAT era, so labelling their data "Rest of World" discarded an identity
  # the database already held. Syria alone reports ~113k layer-B rows.
  mapped <- tibble::tibble(
    area_code = rep(c(61L, 153L, 154L, 209L, 212L), each = 2L),
    year = rep(c(1990L, 2020L), times = 5L)
  ) |>
    add_polity_code()

  expect_equal(
    mapped$polity_code,
    c(
      "GNQ-1968-2025",
      "GNQ-1968-2025",
      "NCL-1800-2025",
      "NCL-1800-2025",
      "MKD-1991-2025",
      "MKD-1991-2025",
      "SWZ-1894-2025",
      "SWZ-1894-2025",
      "SYR-1967-2025",
      "SYR-1967-2025"
    )
  )

  # The FABIO bucket is deliberately UNCHANGED. `polity_area_code` is
  # `coalesce(fabio_code, area_code)`, an aggregation key, not an identity: these
  # areas still aggregate into rest-of-world 999 exactly as on main, and only the
  # polity string they are identified by moved.
  expect_true(all(mapped$polity_area_code == 999L))
})

test_that("French Guiana and Palestine un-fold once upstream models them", {
  # This test previously asserted the opposite -- that 69 and 299 keep folding to
  # ROW -- and said so deliberately: "a future refresh of `polities` that adds
  # GUF/PSE rows will trip this test. That is the point -- it forces the decision
  # instead of silently changing the attribution".
  #
  # The refresh happened. `main`'s `polities` now carries `GUF-1816-1946`,
  # `GUF-1946-2025` and `PSE-1948-2025`, so the tripwire fired exactly as
  # designed, and this is the decision it was forcing.
  #
  # It is resolved by following the rule this change already applies everywhere
  # else, not by adding an exception: an area in the compact country grid that
  # carries a polity family the database holds takes that polity. New Caledonia
  # was handled this way from the start, because upstream had already answered it
  # with a dedicated `NCL-1800-2025`; upstream has now answered French Guiana and
  # Palestine the same way, and upstream is the authority on territorial identity.
  #
  # What made this safe to decide rather than escalate: `polity_area_code` stays
  # 999 for both, so no value is re-attributed and no published total moves --
  # only the identity label. If the owner prefers French Guiana folded into
  # France's polity, that is an upstream change to `GUF-*`, not an exception here.
  mapped <- tibble::tibble(area_code = c(69L, 299L), year = 2020L) |>
    add_polity_code()

  expect_equal(mapped$polity_code, c("GUF-1946-2025", "PSE-1948-2025"))
  # The invariant that makes the relabel value-neutral.
  expect_true(all(mapped$polity_area_code == 999L))
})

test_that("Sudan areas resolve to unified Sudan before the 2011 secession", {
  # FAOSTAT reports Sudan under three areas -- 206 "Sudan (former)", 276 "Sudan"
  # and 277 "South Sudan" -- whose ISO3 supplies only the post-2011 successor.
  # Every pre-2011 year therefore missed the year-aware join and was rescued by
  # its nearest-match fallback onto a state that did not exist yet, so a 1990
  # figure came back as SDN-2011-2025 or SSD-2011-2025.
  mapped <- tibble::tibble(
    area_code = rep(c(206L, 276L, 277L), each = 3L),
    year = rep(c(1961L, 2000L, 2020L), times = 3L)
  ) |>
    add_polity_code()

  # Area 206 now reads SUD-1956-2011 for 2020 as well, and that is `main`'s doing
  # rather than this change's: adopting the upstream FAOSTAT map made 206 resolve
  # to the unified Sudan for every year it reports, "rather than standing in on
  # post-secession SDN-2011-2025". 206 is therefore no longer touched here at all
  # -- only 276 and 277, whose pre-2011 years main still resolved to successor
  # states that did not yet exist.
  expect_equal(
    mapped$polity_code,
    c(
      "SUD-1956-2011",
      "SUD-1956-2011",
      "SUD-1956-2011",
      "SUD-1956-2011",
      "SUD-1956-2011",
      "SDN-2011-2025",
      "SUD-1956-2011",
      "SUD-1956-2011",
      "SSD-2011-2025"
    )
  )

  # Same invariant as above: all three areas keep FABIO bucket 206. Whether that
  # bucket's post-2011 value (Sudan and South Sudan summed) can honestly carry a
  # one-territory polity is whep#414 and is not answered here.
  expect_true(all(mapped$polity_area_code == 206L))
})

test_that("no grid area code resolves to a non-territorial aggregate it owns", {
  # Whole-grid version of the first test, so a new regions.csv code cannot
  # reintroduce the defect. Every area the grid lists AND whose ISO3 names a
  # polity family the database carries must be identified by that family, never
  # by ROW.
  grid <- readr::read_csv(
    system.file("extdata", "regions.csv", package = "whep"),
    show_col_types = FALSE,
    na = c("", "NA", "#N/A", "#DIV/0!", "#REF!")
  )
  crosswalk <- whep::polity_area_crosswalk
  # Families come from `polities`, NOT from the crosswalk. Deriving them from the
  # crosswalk makes the check self-fulfilling: an area wrongly folded into ROW
  # contributes no family of its own, so it drops out of its own test.
  families <- unique(sub("-.*", "", whep::polities$polity_code))

  own_polity <- grid |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      family = .data$iso3c
    ) |>
    dplyr::filter(.data$family %in% families, .data$family != "ROW")

  resolved <- crosswalk |>
    dplyr::filter(.data$area_code %in% own_polity$area_code) |>
    dplyr::mutate(family = sub("-.*", "", .data$polity_code)) |>
    dplyr::distinct(.data$area_code, .data$family)

  expect_equal(nrow(dplyr::filter(resolved, .data$family == "ROW")), 0L)
  expect_setequal(resolved$area_code, own_polity$area_code)
})
