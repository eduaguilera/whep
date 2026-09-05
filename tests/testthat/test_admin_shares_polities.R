# Every call goes through `whep:::` rather than `whep::`: this file runs
# before the roxygen pass that adds `resolve_admin_units()` to NAMESPACE,
# and `whep:::` reaches the private helpers the same way.
#
# Fixture-first, per the plan's Execution DAG (#1000, T34): the
# source-scoped alias rows these slugs need are a whep-polities
# deliverable that is not published yet, so every test but the delegation
# one injects its own alias table. Fixture polity codes are prefixed
# `FIX-` so they cannot be mistaken for upstream codes; the one real code
# used, `ESP-1800-2025`, is read back from the shipped `polities`.

# Reader-shaped rows: the columns `resolve_admin_units()` reads, plus the
# verbatim name it must never read.
unit_rows <- function(
  ids,
  years,
  nuts_version = NA_character_,
  source = "TEST_SOURCE"
) {
  tibble::tibble(
    source = source,
    source_native_unit_id = ids,
    source_native_unit_name = paste("name of", ids),
    nuts_version = nuts_version,
    year = as.integer(years)
  )
}

# One or more rows in the `polity_label_aliases` schema, including the
# three columns this function does not read, so the injected table is
# shaped exactly like the published one.
alias_rows <- function(
  source_label,
  source,
  polity_code,
  year_start = NA_integer_,
  year_end = NA_integer_
) {
  tibble::tibble(
    source_label = source_label,
    source = source,
    year_start = as.integer(year_start),
    year_end = as.integer(year_end),
    polity_code = polity_code,
    common_name = polity_code,
    confidence = "high",
    observed_rows = NA_integer_
  )
}

test_that("a NUTS code resolves differently under two version slugs", {
  # FR21 is Champagne-Ardenne under NUTS 2013; a NUTS 2016 table that
  # reused the code for another polygon means something else by it.
  aliases <- alias_rows(
    source_label = c("FR21", "FR21"),
    source = c("eurostat-nuts2013", "eurostat-nuts2016"),
    polity_code = c("FIX-CHAMPARD-1790-2016", "FIX-OTHER-2016-2025")
  )
  rows <- unit_rows(
    ids = c("FR21", "FR21"),
    years = c(1995L, 1995L),
    nuts_version = c("2013", "2016")
  )

  out <- whep:::resolve_admin_units(rows, "eurostat-nuts", aliases = aliases)

  expect_identical(
    out$rows$alias_source,
    c("eurostat-nuts2013", "eurostat-nuts2016")
  )
  expect_identical(
    out$rows$level_polity_code,
    c("FIX-CHAMPARD-1790-2016", "FIX-OTHER-2016-2025")
  )
})

test_that("FR21 and FRF2 resolve to one polity, overlap counted once", {
  # The 2016 recoding renames the same polygon, so both codes are aliases
  # of one polity. DEDUPING THE OVERLAP IS THE READER'S JOB: the Eurostat
  # reader keeps the newest vintage of a mixed-vintage response
  # (R/admin_stats_eurostat.R), and this function resolves what it is
  # given. Both halves are asserted below.
  aliases <- alias_rows(
    source_label = c("FR21", "FRF2"),
    source = c("eurostat-nuts2013", "eurostat-nuts2016"),
    polity_code = "FIX-CHAMPARD-1790-2016"
  )
  deduped <- dplyr::bind_rows(
    unit_rows("FR21", 1989L, nuts_version = "2013"),
    unit_rows(rep("FRF2", 10L), 1990:1999, nuts_version = "2016")
  )

  out <- whep:::resolve_admin_units(deduped, "eurostat-nuts", aliases = aliases)
  per_year <- dplyr::count(out$rows, year)

  expect_identical(
    unique(out$rows$level_polity_code),
    "FIX-CHAMPARD-1790-2016"
  )
  expect_true(all(per_year$n == 1L))
  expect_identical(nrow(per_year), 11L)

  # The same input WITHOUT the reader's dedupe: resolution maps both
  # vintages onto the one polity and leaves the double count standing.
  overlapping <- dplyr::bind_rows(
    deduped,
    unit_rows(rep("FR21", 10L), 1990:1999, nuts_version = "2013")
  )
  both <- whep:::resolve_admin_units(
    overlapping,
    "eurostat-nuts",
    aliases = aliases
  )

  expect_identical(
    unique(both$rows$level_polity_code),
    "FIX-CHAMPARD-1790-2016"
  )
  expect_identical(max(dplyr::count(both$rows, year)$n), 2L)
})

test_that("a genuine boundary change resolves to two polities by year", {
  # One identifier, two territorial periods: the year decides, exactly as
  # the year-scoped alias route decides for a country label.
  aliases <- alias_rows(
    source_label = c("35", "35"),
    source = c("ibge-uf", "ibge-uf"),
    polity_code = c("FIX-SP-1900-1960", "FIX-SP-1960-2025"),
    year_start = c(1900L, 1960L),
    year_end = c(1959L, 2025L)
  )
  rows <- unit_rows(c("35", "35"), c(1950L, 2000L))

  out <- whep:::resolve_admin_units(rows, "ibge-uf", aliases = aliases)

  expect_identical(
    out$rows$level_polity_code,
    c("FIX-SP-1900-1960", "FIX-SP-1960-2025")
  )
})

test_that("a scoped alias never applies under another code system", {
  # "35" is an IBGE UF and a NUTS-shaped string at once. A scoped alias
  # speaks for its own code system only, so the Eurostat slug gets NA
  # rather than Sao Paulo.
  aliases <- alias_rows("35", "ibge-uf", "FIX-SP-1960-2025")
  rows <- unit_rows("35", 2000L, nuts_version = "2016")

  as_nuts <- whep:::resolve_admin_units(
    rows,
    "eurostat-nuts",
    aliases = aliases
  )
  as_uf <- whep:::resolve_admin_units(rows, "ibge-uf", aliases = aliases)

  expect_identical(as_nuts$rows$alias_source, "eurostat-nuts2016")
  expect_identical(as_nuts$rows$level_polity_code, NA_character_)
  expect_identical(as_uf$rows$level_polity_code, "FIX-SP-1960-2025")
})

test_that("an unscoped alias applies under any code system", {
  # Inherited from `resolve_polity_label()`, deliberately: an unscoped
  # alias applies to any source there, and mirroring that is what keeps
  # the injected and published routes one authority rather than two. It
  # is also why upstream should scope every admin alias by code system --
  # native identifiers collide across systems, and an unscoped one would
  # answer for all of them.
  aliases <- alias_rows("35", NA_character_, "FIX-ANY-1900-2025")
  rows <- unit_rows("35", 2000L, nuts_version = "2016")

  as_nuts <- whep:::resolve_admin_units(
    rows,
    "eurostat-nuts",
    aliases = aliases
  )
  as_uf <- whep:::resolve_admin_units(rows, "ibge-uf", aliases = aliases)

  expect_identical(as_nuts$rows$level_polity_code, "FIX-ANY-1900-2025")
  expect_identical(as_uf$rows$level_polity_code, "FIX-ANY-1900-2025")
})

test_that("the more specific alias wins where several match", {
  # Year-scoped over unscoped, then source-scoped, then the narrower
  # span: the ordering `resolve_polity_label()` applies.
  aliases <- dplyr::bind_rows(
    alias_rows("19", NA_character_, "FIX-UNSCOPED"),
    alias_rows("19", "usda-nass-fips", "FIX-SOURCE-SCOPED"),
    alias_rows(
      "19",
      "usda-nass-fips",
      "FIX-YEAR-SCOPED",
      year_start = 1990L,
      year_end = 2025L
    )
  )
  rows <- unit_rows(c("19", "19"), c(1980L, 2000L))

  out <- whep:::resolve_admin_units(rows, "usda-nass-fips", aliases = aliases)

  expect_identical(
    out$rows$level_polity_code,
    c("FIX-SOURCE-SCOPED", "FIX-YEAR-SCOPED")
  )
})

test_that("resolution never reads the verbatim unit name", {
  # An alias keyed on the NAME must not resolve a row keyed on the code:
  # names are diagnostic here, and matching them would be a second
  # authority for what a unit is.
  aliases <- alias_rows(
    "name of FR21",
    "eurostat-nuts2013",
    "FIX-CHAMPARD-1790-2016"
  )
  rows <- unit_rows("FR21", 1995L, nuts_version = "2013")

  out <- whep:::resolve_admin_units(rows, "eurostat-nuts", aliases = aliases)

  expect_identical(out$rows$level_polity_code, NA_character_)
})

test_that("the whep-lab family slug resolves a Spain province id", {
  # The in-house families are keyed on the compilation's own
  # `admin_unit_id` ("ESP-ES111"), one code system per family.
  aliases <- alias_rows(
    "ESP-ES111",
    "whep-lab-spain-provinces",
    "FIX-ES111-1833-2025"
  )
  rows <- unit_rows(
    c("ESP-ES111", "ESP-ES112"),
    c(2000L, 2000L),
    source = "ES_provinces"
  )

  out <- whep:::resolve_admin_units(
    rows,
    "whep-lab-spain-provinces",
    aliases = aliases
  )

  expect_identical(
    out$rows$level_polity_code,
    c("FIX-ES111-1833-2025", NA_character_)
  )
  expect_identical(out$diagnostics$n_unresolved, 1L)
  expect_identical(out$diagnostics$example_ids, "ESP-ES112")
})

test_that("unresolved rows keep NA and are counted in the diagnostics", {
  aliases <- alias_rows("19", "usda-nass-fips", "FIX-IOWA-1846-2025")
  rows <- unit_rows(c("19", "27", "31"), c(2020L, 2020L, 2020L))

  out <- whep:::resolve_admin_units(rows, "usda-nass-fips", aliases = aliases)

  expect_identical(nrow(out$rows), 3L)
  expect_identical(sum(is.na(out$rows$level_polity_code)), 2L)
  expect_identical(
    names(out$diagnostics),
    c("source", "alias_source", "n_rows", "n_unresolved", "example_ids")
  )
  expect_identical(out$diagnostics$n_rows, 3L)
  expect_identical(out$diagnostics$n_unresolved, 2L)
  expect_identical(out$diagnostics$example_ids, "27|31")
})

test_that("a fully resolved group reports no example ids", {
  aliases <- alias_rows("19", "usda-nass-fips", "FIX-IOWA-1846-2025")
  rows <- unit_rows("19", 2020L)

  out <- whep:::resolve_admin_units(rows, "usda-nass-fips", aliases = aliases)

  expect_identical(out$diagnostics$n_unresolved, 0L)
  expect_identical(out$diagnostics$example_ids, NA_character_)
})

test_that("a versioned system with no version resolves nothing", {
  # Falling back to the bare "eurostat-nuts" slug would silently borrow
  # whichever vintage upstream scoped its alias to.
  aliases <- alias_rows("FR21", "eurostat-nuts2013", "FIX-CHAMPARD-1790-2016")
  rows <- unit_rows("FR21", 1995L, nuts_version = NA_character_)

  out <- whep:::resolve_admin_units(rows, "eurostat-nuts", aliases = aliases)

  expect_identical(out$rows$alias_source, NA_character_)
  expect_identical(out$rows$level_polity_code, NA_character_)
  expect_identical(out$diagnostics$n_unresolved, 1L)
})

test_that("code_system may vary per row", {
  aliases <- dplyr::bind_rows(
    alias_rows("19", "usda-nass-fips", "FIX-IOWA-1846-2025"),
    alias_rows("35", "ibge-uf", "FIX-SP-1960-2025")
  )
  rows <- unit_rows(c("19", "35"), c(2020L, 2020L))

  out <- whep:::resolve_admin_units(
    rows,
    c("usda-nass-fips", "ibge-uf"),
    aliases = aliases
  )

  expect_identical(
    out$rows$level_polity_code,
    c("FIX-IOWA-1846-2025", "FIX-SP-1960-2025")
  )
})

test_that("a non-default year column is honoured", {
  aliases <- alias_rows(
    "35",
    "ibge-uf",
    "FIX-SP-1960-2025",
    year_start = 1960L,
    year_end = 2025L
  )
  rows <- unit_rows(c("35", "35"), c(1950L, 2000L)) |>
    dplyr::rename(reference_year = "year")

  out <- whep:::resolve_admin_units(
    rows,
    "ibge-uf",
    year_col = "reference_year",
    aliases = aliases
  )

  expect_identical(
    out$rows$level_polity_code,
    c(NA_character_, "FIX-SP-1960-2025")
  )
})

test_that("an empty input returns empty rows and diagnostics", {
  rows <- unit_rows(character(), integer())

  out <- whep:::resolve_admin_units(
    rows,
    "ibge-uf",
    aliases = alias_rows(
      "35",
      "ibge-uf",
      "FIX-SP-1960-2025"
    )
  )

  expect_identical(nrow(out$rows), 0L)
  expect_true("level_polity_code" %in% names(out$rows))
  expect_identical(nrow(out$diagnostics), 0L)
})

test_that("an unknown code system aborts naming the vocabulary", {
  rows <- unit_rows("19", 2020L)

  expect_error(
    whep:::resolve_admin_units(rows, "nass"),
    "code_system"
  )
})

test_that("a missing identifier column aborts naming it", {
  rows <- unit_rows("19", 2020L) |>
    dplyr::select(-"source_native_unit_id")

  expect_error(
    whep:::resolve_admin_units(rows, "usda-nass-fips"),
    "source_native_unit_id"
  )
})

test_that("a versioned system without nuts_version aborts", {
  rows <- unit_rows("FR21", 1995L) |> dplyr::select(-"nuts_version")

  expect_error(
    whep:::resolve_admin_units(rows, "eurostat-nuts"),
    "nuts_version"
  )
})

test_that("an alias table missing schema columns aborts", {
  rows <- unit_rows("19", 2020L)
  aliases <- alias_rows("19", "usda-nass-fips", "FIX-IOWA-1846-2025") |>
    dplyr::select(-"year_end")

  expect_error(
    whep:::resolve_admin_units(rows, "usda-nass-fips", aliases = aliases),
    "year_end"
  )
})

test_that("a code_system of the wrong length aborts", {
  rows <- unit_rows(c("19", "27"), c(2020L, 2020L))

  expect_error(
    whep:::resolve_admin_units(
      rows,
      c("usda-nass-fips", "ibge-uf", "ibge-uf")
    ),
    "length 1"
  )
})

test_that("a non-numeric year column aborts", {
  rows <- unit_rows("19", 2020L) |>
    dplyr::mutate(year = as.character(year))

  expect_error(
    whep:::resolve_admin_units(rows, "usda-nass-fips"),
    "numeric"
  )
})

test_that("aliases = NULL reads the published alias map, ALIAS ROUTE ONLY", {
  # The one leg that reads package data (`polity_label_aliases`), so it is
  # also the leg proving the production default is wired to the real
  # table. No network and no WHEP_* path, so it belongs in the suite; on
  # the development machine it must be run through PowerShell, where
  # loading `data/polities.rda` does not segfault.
  #
  # "ESP" is not an administrative identifier -- it is the container's own
  # ISO3. `resolve_polity_label()`'s identity routes would answer for it
  # (its NAME/ISO3 fallback), but `resolve_admin_units()` must never take
  # that route: an administrative unit id that happens to collide with a
  # polity name or ISO3 code must resolve to NA, not to the container.
  # NOT ONE published alias is scoped to a code system this function names
  # (1,007 rows over 15 sources on the 2026-09-03 snapshot, none of them
  # `usda-nass-fips`, `whep-lab-spain-provinces` and friends), so both
  # identifiers below are unresolved under the real, unmocked table --
  # that is finding #1000/T34-3's regression pin.
  rows <- unit_rows(c("ESP", "19"), c(2000L, 2020L))

  out <- whep:::resolve_admin_units(
    rows,
    c("whep-lab-spain-provinces", "usda-nass-fips")
  )

  expect_true(is.na(out$rows$level_polity_code[1]))
  expect_true(is.na(out$rows$level_polity_code[2]))
  expect_identical(out$diagnostics$n_unresolved, c(1L, 1L))
  expect_identical(
    sort(out$diagnostics$example_ids),
    c("19", "ESP")
  )
})

test_that("aliases = NULL still resolves a genuine published alias", {
  # Mirrors the previous test's other direction: once a code-system-scoped
  # row exists in the published table, the production path must resolve
  # it -- the fix narrows the route, it must not also break resolution.
  # `polity_label_aliases` is package data, not a function, but
  # `local_mocked_bindings()` (testthat >= 3.2) rebinds any named object
  # in the namespace, so this stands in for the whep-polities deliverable
  # landing without waiting for it.
  fake_aliases <- alias_rows("19", "usda-nass-fips", "FIX-IOWA-1846-2025")
  testthat::local_mocked_bindings(
    polity_label_aliases = fake_aliases,
    .package = "whep"
  )
  rows <- unit_rows("19", 2020L)

  out <- whep:::resolve_admin_units(rows, "usda-nass-fips")

  expect_identical(out$rows$level_polity_code, "FIX-IOWA-1846-2025")
  expect_identical(out$diagnostics$n_unresolved, 0L)
})

test_that("aliases = NULL never falls through to the name or ISO3 route", {
  # Direct regression for finding #1000/T34-3: even when the published
  # table (mocked here) carries rows, an id with no matching alias row
  # must stay NA under aliases = NULL -- it must not fall through to
  # `resolve_polity_label()`'s name/ISO3 identity routes, which would
  # answer for "ESP" via the container polity's own ISO3 code.
  testthat::local_mocked_bindings(
    polity_label_aliases = alias_rows(
      "19",
      "usda-nass-fips",
      "FIX-IOWA-1846-2025"
    ),
    .package = "whep"
  )
  rows <- unit_rows("ESP", 2000L)

  out <- whep:::resolve_admin_units(rows, "whep-lab-spain-provinces")

  expect_true(is.na(out$rows$level_polity_code))
  expect_identical(out$diagnostics$n_unresolved, 1L)
})
