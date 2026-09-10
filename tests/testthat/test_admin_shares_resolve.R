# Every call goes through `whep:::` rather than `whep::`: this file runs
# before the roxygen pass that adds `resolve_admin_shares()` to NAMESPACE,
# and `whep:::` reaches the private helpers the same way.

# Complete a compact tibble of the varying columns onto the admin-shares
# contract, so each fixture below shows only what it is testing.
resolve_rows <- function(rows) {
  defaults <- tibble::tibble(
    area_code = 840L,
    level = 1L,
    indicator_used = "area_harvested",
    nuts_version = NA_character_,
    concept_break = FALSE,
    source_version = NA_character_,
    recorded_at = "2026-01-01T00:00:00Z",
    treatment_year = "observed",
    value_flag = NA_character_,
    share = NA_real_
  )
  absent <- setdiff(names(defaults), names(rows))
  rows |>
    dplyr::bind_cols(defaults[absent]) |>
    dplyr::mutate(
      source_native_id = level_polity_code,
      source_native_name = level_polity_code,
      source_id = source
    ) |>
    whep:::ensure_columns(whep:::admin_shares_prototype())
}

# Two sibling units of one source, for one container-item-year.
two_units <- function(
  source,
  tier,
  grain,
  years,
  area_code = 840L,
  item_prod_code = 15L,
  indicator = "area_harvested",
  nuts_version = NA_character_
) {
  tidyr::crossing(
    year = as.integer(years),
    level_polity_code = paste0(source, c("-U1", "-U2"))
  ) |>
    dplyr::mutate(
      area_code = area_code,
      item_prod_code = item_prod_code,
      indicator_used = indicator,
      nuts_version = nuts_version,
      source = source,
      tier = as.integer(tier),
      grain = grain,
      value = 100
    ) |>
    resolve_rows()
}

# One year of the seam series below, with its own reporting-unit set.
seam_year_rows <- function(year, source, tier, grain, nuts, indicator, units) {
  tibble::tibble(
    area_code = 76L,
    item_prod_code = 15L,
    year = as.integer(year),
    source = source,
    tier = as.integer(tier),
    grain = grain,
    nuts_version = nuts,
    indicator_used = indicator,
    level_polity_code = units,
    value = 100
  )
}

# One container-item series carrying every seam kind exactly once.
seam_series <- function() {
  pair <- c("U1", "U2")
  trio <- c("U1", "U2", "U3")
  dplyr::bind_rows(
    seam_year_rows(1990, "S1", 1, "admin1", NA, "area_harvested", pair),
    seam_year_rows(1991, "S1", 1, "admin1", NA, "area_harvested", trio),
    seam_year_rows(1992, "S2", 1, "admin1", "2016", "area_harvested", trio),
    seam_year_rows(1993, "S3", 2, "admin2", "2016", "area_harvested", trio),
    seam_year_rows(1994, "S4", 1, "admin2", "2016", "production", trio)
  ) |>
    dplyr::mutate(nuts_version = as.character(nuts_version)) |>
    resolve_rows()
}

# Two containers, each with two competing sources, so a change confined to
# one container can be diffed against the other.
two_containers <- function() {
  dplyr::bind_rows(
    two_units("A_source", 1L, "admin1", 2000:2002, area_code = 840L),
    two_units("B_source", 3L, "admin1", 2000:2002, area_code = 840L),
    two_units("A_source", 1L, "admin1", 2000:2002, area_code = 724L),
    two_units("B_source", 3L, "admin1", 2000:2002, area_code = 724L)
  )
}

admin_shares_key <- function() {
  c(
    "area_code",
    "level_polity_code",
    "level",
    "item_prod_code",
    "indicator_used",
    "year"
  )
}

# The three invariants this task is judged on, asserted together so every
# fixture below can call them.
expect_resolution_invariants <- function(input, resolved) {
  contract <- names(whep:::admin_shares_prototype())
  parts <- nrow(resolved$shares) +
    nrow(resolved$dropped) +
    nrow(resolved$excluded)
  testthat::expect_equal(parts, nrow(input))

  winners_key <- resolved$shares[, admin_shares_key()]
  testthat::expect_equal(nrow(dplyr::distinct(winners_key)), nrow(winners_key))

  expected <- dplyr::semi_join(
    input,
    resolved$shares,
    by = c(admin_shares_key(), "source")
  )
  testthat::expect_equal(resolved$shares[, contract], expected)
}

test_that("the shipped fixture resolves as a single candidate throughout", {
  fixture <- .level1_admin_shares()
  resolved <- whep:::resolve_admin_shares(fixture)

  expect_named(
    resolved,
    c("shares", "seams", "coverage", "excluded", "dropped")
  )
  expect_equal(nrow(resolved$shares), nrow(fixture))
  expect_equal(nrow(resolved$dropped), 0L)
  expect_equal(nrow(resolved$excluded), 0L)
  expect_setequal(resolved$shares$resolution_rule, "single_candidate")
  expect_equal(resolved$shares$resolved_source, resolved$shares$source)
  expect_equal(resolved$shares$resolved_tier, resolved$shares$tier)
  expect_equal(resolved$shares$resolved_grain, resolved$shares$grain)
  expect_resolution_invariants(fixture, resolved)
})

test_that("the coverage report counts the fixture's reporting units", {
  resolved <- whep:::resolve_admin_shares(.level1_admin_shares())
  coverage <- resolved$coverage

  expect_equal(names(coverage), names(whep:::.admin_coverage_prototype()))
  expect_equal(nrow(coverage), 6L)
  expect_equal(coverage$year, rep(1974:1976, times = 2))
  expect_equal(coverage$n_units_reporting, rep(c(1L, 2L, 2L), times = 2))
  expect_equal(coverage$reporting_units[1], "A-A1-1900-2100")
  expect_equal(
    coverage$reporting_units[2],
    "A-A1-1900-2100|A-A2-1975-2100"
  )
  expect_equal(coverage$coverage_change, rep(c(FALSE, TRUE, FALSE), times = 2))
  expect_setequal(coverage$resolved_indicator, "area_harvested")
})

test_that("A2 entering at 1975 is a coverage-change seam, and 1974 a start", {
  resolved <- whep:::resolve_admin_shares(.level1_admin_shares())
  seams <- resolved$seams

  expect_equal(names(seams), names(whep:::.admin_seams_prototype()))
  expect_equal(nrow(seams), 4L)
  expect_setequal(seams$seam_kind, c("start", "coverage_change"))
  starts <- dplyr::filter(seams, seam_kind == "start")
  expect_setequal(starts$seam_year, 1974L)
  expect_true(all(is.na(starts$previous_year)))
  expect_setequal(starts$value_to, "fixture")
  changes <- dplyr::filter(seams, seam_kind == "coverage_change")
  expect_setequal(changes$seam_year, 1975L)
  expect_setequal(changes$previous_year, 1974L)
  expect_setequal(changes$value_from, "A-A1-1900-2100")
  expect_setequal(changes$value_to, "A-A1-1900-2100|A-A2-1975-2100")
})

test_that("tier 1 beats tier 3 at equal grain, and at finer grain", {
  equal_grain <- dplyr::bind_rows(
    two_units("public_t1", 1L, "admin1", 2000:2002),
    two_units("panel_t3", 3L, "admin1", 2000:2002)
  )
  resolved <- whep:::resolve_admin_shares(equal_grain)

  expect_setequal(resolved$shares$resolved_source, "public_t1")
  expect_setequal(resolved$shares$resolution_rule, "tier")
  expect_setequal(resolved$dropped$source, "panel_t3")
  expect_setequal(resolved$dropped$drop_reason, "tier")
  expect_resolution_invariants(equal_grain, resolved)

  finer_public <- dplyr::bind_rows(
    two_units("public_t1", 1L, "admin2", 2000:2002),
    two_units("panel_t3", 3L, "admin1", 2000:2002)
  )
  finer <- whep:::resolve_admin_shares(finer_public)
  expect_setequal(finer$shares$resolved_source, "public_t1")
  expect_setequal(finer$shares$resolution_rule, "grain")
})

test_that("grain beats tier where the lower tier is coarser", {
  input <- dplyr::bind_rows(
    two_units("public_t1", 1L, "admin1", 2000:2002),
    two_units("panel_t3", 3L, "admin2", 2000:2002)
  )
  resolved <- whep:::resolve_admin_shares(input)

  expect_setequal(resolved$shares$resolved_source, "panel_t3")
  expect_setequal(resolved$shares$resolved_grain, "admin2")
  expect_setequal(resolved$shares$resolved_tier, 3L)
  expect_setequal(resolved$shares$resolution_rule, "grain")
  expect_setequal(resolved$dropped$drop_reason, "grain")
  expect_resolution_invariants(input, resolved)
})

test_that("Spain post-2000 resolves to the NUTS-3 tier-2 provinces", {
  spain <- dplyr::bind_rows(
    two_units(
      "Eurostat_apro_cpshr",
      1L,
      "admin1",
      2000:2005,
      area_code = 724L,
      nuts_version = "2021"
    ),
    two_units(
      "ES_provinces_nuts3",
      2L,
      "admin2",
      2000:2005,
      area_code = 724L,
      nuts_version = "2016"
    )
  )
  resolved <- whep:::resolve_admin_shares(spain)

  expect_setequal(resolved$coverage$resolved_source, "ES_provinces_nuts3")
  expect_setequal(resolved$coverage$resolved_tier, 2L)
  expect_setequal(resolved$coverage$resolved_grain, "admin2")
  expect_setequal(resolved$coverage$resolution_rule, "grain")
  expect_setequal(resolved$coverage$resolved_nuts_version, "2016")
  expect_setequal(resolved$dropped$source, "Eurostat_apro_cpshr")
  expect_equal(nrow(resolved$seams), 1L)
  expect_equal(resolved$seams$seam_kind, "start")
  expect_resolution_invariants(spain, resolved)
})

test_that("the run-length tie-break is local to the year in question", {
  # alpha is present 1990:2000 except 1998, so its runs are 1990-1997 (8)
  # and 1999-2000 (2); beta is present 1996:1999 (4) and 2005 (1).
  input <- dplyr::bind_rows(
    two_units("alpha_src", 2L, "admin1", setdiff(1990:2000, 1998)),
    two_units("beta_src", 2L, "admin1", c(1996:1999, 2005L))
  )
  resolved <- whep:::resolve_admin_shares(input)
  by_year <- resolved$coverage |>
    dplyr::select(year, resolved_source, resolution_rule)

  expect_equal(
    dplyr::filter(by_year, year == 1996L)$resolved_source,
    "alpha_src"
  )
  expect_equal(
    dplyr::filter(by_year, year == 1996L)$resolution_rule,
    "run_length"
  )
  expect_equal(
    dplyr::filter(by_year, year == 1999L)$resolved_source,
    "beta_src"
  )
  expect_equal(
    dplyr::filter(by_year, year == 1999L)$resolution_rule,
    "run_length"
  )
  expect_equal(
    dplyr::filter(by_year, year == 1994L)$resolution_rule,
    "single_candidate"
  )
  expect_equal(
    dplyr::filter(by_year, year == 2005L)$resolved_source,
    "beta_src"
  )
  expect_setequal(
    dplyr::filter(resolved$dropped, year == 1996L)$drop_reason,
    "run_length"
  )
  expect_setequal(
    dplyr::filter(resolved$dropped, year == 1996L)$source,
    "beta_src"
  )
  expect_resolution_invariants(input, resolved)
})

test_that("each change of resolved source is a seam", {
  input <- dplyr::bind_rows(
    two_units("alpha_src", 2L, "admin1", setdiff(1990:2000, 1998)),
    two_units("beta_src", 2L, "admin1", c(1996:1999, 2005L))
  )
  seams <- whep:::resolve_admin_shares(input)$seams

  # 1998 is alpha's gap year, so beta stands alone there; 2000 is beta's,
  # and 2005 is beta's isolated late year.
  switches <- dplyr::filter(seams, seam_kind == "source_switch")
  expect_equal(switches$seam_year, c(1998L, 2000L, 2005L))
  expect_equal(switches$value_from, c("alpha_src", "beta_src", "alpha_src"))
  expect_equal(switches$value_to, c("beta_src", "alpha_src", "beta_src"))
  expect_equal(switches$previous_year, c(1997L, 1999L, 2000L))
  expect_equal(dplyr::filter(seams, seam_kind == "start")$seam_year, 1990L)
})

test_that("an unbroken tie falls to the source name, and warns", {
  input <- dplyr::bind_rows(
    two_units("zeta_src", 2L, "admin1", 2000:2001),
    two_units("alpha_src", 2L, "admin1", 2000:2001)
  )

  expect_warning(
    resolved <- whep:::resolve_admin_shares(input),
    "tied on grain"
  )
  expect_setequal(resolved$shares$resolved_source, "alpha_src")
  expect_setequal(resolved$shares$resolution_rule, "source_name")
  expect_setequal(resolved$dropped$drop_reason, "source_name")
  expect_resolution_invariants(input, resolved)
})

test_that("the tie warning names both tied sources", {
  input <- dplyr::bind_rows(
    two_units("zeta_src", 2L, "admin1", 2000:2001),
    two_units("alpha_src", 2L, "admin1", 2000:2001)
  )

  expect_warning(whep:::resolve_admin_shares(input), "alpha_src")
  expect_warning(whep:::resolve_admin_shares(input), "zeta_src")
})

test_that("every seam kind fires where the series changes", {
  input <- seam_series()
  seams <- whep:::resolve_admin_shares(input)$seams
  fired <- seams |>
    dplyr::select(seam_kind, seam_year) |>
    dplyr::arrange(seam_kind, seam_year)

  expect_setequal(
    unique(fired$seam_kind),
    c(
      "start",
      "coverage_change",
      "source_switch",
      "grain_switch",
      "nuts_version_switch",
      "indicator_switch"
    )
  )
  expect_equal(dplyr::filter(fired, seam_kind == "start")$seam_year, 1990L)
  expect_equal(
    dplyr::filter(fired, seam_kind == "coverage_change")$seam_year,
    1991L
  )
  expect_equal(
    dplyr::filter(fired, seam_kind == "source_switch")$seam_year,
    c(1992L, 1993L, 1994L)
  )
  expect_equal(
    dplyr::filter(fired, seam_kind == "nuts_version_switch")$seam_year,
    1992L
  )
  expect_equal(
    dplyr::filter(fired, seam_kind == "grain_switch")$seam_year,
    1993L
  )
  expect_equal(
    dplyr::filter(fired, seam_kind == "indicator_switch")$seam_year,
    1994L
  )
})

test_that("an override changes only the container it names", {
  input <- two_containers()
  base <- whep:::resolve_admin_shares(input)
  overrides <- tibble::tibble(
    area_code = 840L,
    item_prod_code = NA_integer_,
    source = "B_source"
  )
  forced <- whep:::resolve_admin_shares(input, overrides = overrides)

  expect_equal(
    dplyr::filter(forced$shares, area_code == 724L),
    dplyr::filter(base$shares, area_code == 724L)
  )
  expect_equal(
    dplyr::filter(forced$coverage, area_code == 724L),
    dplyr::filter(base$coverage, area_code == 724L)
  )
  expect_equal(
    dplyr::filter(forced$seams, area_code == 724L),
    dplyr::filter(base$seams, area_code == 724L)
  )
  changed <- dplyr::filter(forced$shares, area_code == 840L)
  expect_setequal(changed$resolved_source, "B_source")
  expect_setequal(changed$resolution_rule, "override")
  expect_setequal(
    dplyr::filter(forced$dropped, area_code == 840L)$drop_reason,
    "override"
  )
  expect_setequal(
    dplyr::filter(base$shares, area_code == 840L)$resolved_source,
    "A_source"
  )
  expect_resolution_invariants(input, forced)
})

test_that("an item-specific override beats a container-wide one", {
  input <- dplyr::bind_rows(
    two_containers(),
    two_units("A_source", 1L, "admin1", 2000L, item_prod_code = 44L),
    two_units("B_source", 3L, "admin1", 2000L, item_prod_code = 44L)
  )
  overrides <- tibble::tibble(
    area_code = c(840L, 840L),
    item_prod_code = c(NA_integer_, 44L),
    source = c("B_source", "A_source")
  )
  resolved <- whep:::resolve_admin_shares(input, overrides = overrides)
  by_item <- dplyr::filter(resolved$coverage, area_code == 840L)

  expect_setequal(
    dplyr::filter(by_item, item_prod_code == 15L)$resolved_source,
    "B_source"
  )
  expect_setequal(
    dplyr::filter(by_item, item_prod_code == 44L)$resolved_source,
    "A_source"
  )
  expect_setequal(by_item$resolution_rule, "override")
})

test_that("overrides may omit item_prod_code entirely", {
  input <- two_containers()
  overrides <- tibble::tibble(area_code = 840L, source = "B_source")
  resolved <- whep:::resolve_admin_shares(input, overrides = overrides)

  expect_setequal(
    dplyr::filter(resolved$shares, area_code == 840L)$resolved_source,
    "B_source"
  )
  expect_setequal(
    dplyr::filter(resolved$shares, area_code == 724L)$resolved_source,
    "A_source"
  )
})

test_that("an override may keep a coarser source over a finer one", {
  input <- dplyr::bind_rows(
    two_units("public_t1", 1L, "admin1", 2000:2001),
    two_units("panel_t3", 3L, "admin2", 2000:2001)
  )
  base <- whep:::resolve_admin_shares(input)
  overrides <- tibble::tibble(
    area_code = 840L,
    item_prod_code = 15L,
    source = "public_t1"
  )
  forced <- whep:::resolve_admin_shares(input, overrides = overrides)

  expect_setequal(base$coverage$resolved_source, "panel_t3")
  expect_setequal(forced$coverage$resolved_source, "public_t1")
  expect_setequal(forced$coverage$resolved_grain, "admin1")
  expect_setequal(forced$coverage$resolution_rule, "override")
  expect_resolution_invariants(input, forced)
})

test_that("an override on an uncontested group is still recorded", {
  input <- two_units("only_src", 2L, "admin1", 2000:2001)
  overrides <- tibble::tibble(
    area_code = 840L,
    item_prod_code = NA_integer_,
    source = "only_src"
  )
  resolved <- whep:::resolve_admin_shares(input, overrides = overrides)

  expect_setequal(resolved$coverage$resolution_rule, "override")
  expect_setequal(
    whep:::resolve_admin_shares(input)$coverage$resolution_rule,
    "single_candidate"
  )
})

test_that("an override that forces no winner warns", {
  input <- two_containers()
  overrides <- tibble::tibble(
    area_code = 840L,
    item_prod_code = NA_integer_,
    source = "C_source_typo"
  )

  expect_warning(
    resolved <- whep:::resolve_admin_shares(input, overrides = overrides),
    "forced no winner"
  )
  expect_setequal(
    dplyr::filter(resolved$shares, area_code == 840L)$resolved_source,
    "A_source"
  )
})

test_that("an exclusion changes only the container-years it names", {
  input <- two_containers()
  base <- whep:::resolve_admin_shares(input)
  held_out <- whep:::resolve_admin_shares(
    input,
    constraint_exclude = list("840" = 2001:2002)
  )

  expect_equal(
    dplyr::filter(held_out$shares, area_code == 724L),
    dplyr::filter(base$shares, area_code == 724L)
  )
  expect_equal(
    dplyr::filter(held_out$coverage, area_code == 724L),
    dplyr::filter(base$coverage, area_code == 724L)
  )
  # Container 840 carries two sources x two units x two withheld years.
  expect_equal(nrow(held_out$excluded), 8L)
  expect_setequal(held_out$excluded$area_code, 840L)
  expect_setequal(held_out$excluded$year, 2001:2002)
  expect_equal(
    unique(dplyr::filter(held_out$shares, area_code == 840L)$year),
    2000L
  )
  expect_named(held_out$excluded, names(whep:::admin_shares_prototype()))
  expect_resolution_invariants(input, held_out)
})

test_that("an excluded year is not evidence of a contiguous run", {
  # z_long_src runs 1990:2000 and wins 1998 on run length; withholding
  # 1993:1995, years only it covers, cuts that run to a_late_src's own.
  input <- dplyr::bind_rows(
    two_units("z_long_src", 2L, "admin1", 1990:2000),
    two_units("a_late_src", 2L, "admin1", 1996:2000)
  )
  base <- whep:::resolve_admin_shares(input)
  at_base <- dplyr::filter(base$coverage, year == 1998L)
  expect_equal(at_base$resolved_source, "z_long_src")
  expect_equal(at_base$resolution_rule, "run_length")

  expect_warning(
    held_out <- whep:::resolve_admin_shares(
      input,
      constraint_exclude = list("840" = 1993:1995)
    ),
    "tied on grain"
  )
  at_held_out <- dplyr::filter(held_out$coverage, year == 1998L)
  expect_equal(at_held_out$resolved_source, "a_late_src")
  expect_equal(at_held_out$resolution_rule, "source_name")
})

test_that("changing one container's tier changes only that container", {
  input <- two_containers()
  base <- whep:::resolve_admin_shares(input)
  retiered <- input |>
    dplyr::mutate(
      tier = dplyr::if_else(
        area_code == 840L & source == "B_source",
        1L,
        tier
      )
    )
  # B_source now ties A_source on tier, grain and run length for 840, and
  # the name tie-break settles it; 724 is untouched.
  expect_warning(
    moved <- whep:::resolve_admin_shares(retiered),
    "tied on grain"
  )

  expect_equal(
    dplyr::filter(moved$coverage, area_code == 724L),
    dplyr::filter(base$coverage, area_code == 724L)
  )
  expect_setequal(
    dplyr::filter(moved$coverage, area_code == 840L)$resolution_rule,
    "source_name"
  )
  expect_setequal(
    dplyr::filter(base$coverage, area_code == 840L)$resolution_rule,
    "tier"
  )
})

test_that("production loses to area, and yield never binds", {
  input <- dplyr::bind_rows(
    two_units("public_t1", 1L, "admin1", 2000:2001),
    two_units("public_t1", 1L, "admin1", 2000:2001, indicator = "production"),
    two_units("public_t1", 1L, "admin1", 2000:2001, indicator = "yield"),
    two_units("public_t1", 1L, "admin1", 2002L, indicator = "production")
  )
  resolved <- whep:::resolve_admin_shares(input)

  expect_setequal(
    dplyr::filter(resolved$coverage, year < 2002L)$resolved_indicator,
    "area_harvested"
  )
  expect_setequal(
    dplyr::filter(resolved$coverage, year == 2002L)$resolved_indicator,
    "production"
  )
  expect_setequal(
    dplyr::filter(resolved$dropped, indicator_used == "yield")$drop_reason,
    "indicator_never_binds"
  )
  expect_setequal(
    dplyr::filter(
      resolved$dropped,
      indicator_used == "production" & year < 2002L
    )$drop_reason,
    "indicator_precedence"
  )
  expect_equal(
    dplyr::filter(resolved$seams, seam_kind == "indicator_switch")$seam_year,
    2002L
  )
  expect_resolution_invariants(input, resolved)
})

test_that("a non-observed row aborts before anything is resolved", {
  input <- two_units("public_t1", 1L, "admin1", 2000:2001) |>
    dplyr::mutate(
      treatment_year = dplyr::if_else(year == 2001L, "carried", treatment_year)
    )

  expect_error(
    whep:::resolve_admin_shares(input),
    class = "whep_error_admin_not_observed"
  )
  expect_error(whep:::resolve_admin_shares(input), "observed rows only")
})

test_that("a zero-row input returns the typed empty tables", {
  resolved <- whep:::resolve_admin_shares(whep:::admin_shares_prototype())

  expect_equal(
    vapply(resolved, nrow, integer(1)),
    c(shares = 0L, seams = 0L, coverage = 0L, excluded = 0L, dropped = 0L)
  )
  expect_equal(resolved$shares, whep:::.resolved_shares_prototype())
  expect_equal(resolved$dropped, whep:::.dropped_shares_prototype())
  expect_equal(resolved$coverage, whep:::.admin_coverage_prototype())
  expect_equal(resolved$seams, whep:::.admin_seams_prototype())
})

test_that("the winners' columns match the prototype the empty case returns", {
  resolved <- whep:::resolve_admin_shares(.level1_admin_shares())

  expect_equal(
    names(resolved$shares),
    names(whep:::.resolved_shares_prototype())
  )
  expect_equal(
    vapply(resolved$shares, class, character(1)),
    vapply(whep:::.resolved_shares_prototype(), class, character(1))
  )
})

test_that("a source carrying two tiers in one group aborts", {
  input <- two_units("public_t1", 1L, "admin1", 2000L) |>
    dplyr::mutate(tier = c(1L, 3L))

  expect_error(
    whep:::resolve_admin_shares(input),
    class = "whep_error_admin_tier_conflict"
  )
})

test_that("a mixed-grain candidate warns and keeps its finer units", {
  input <- two_units("public_t1", 1L, "admin1", 2000L) |>
    dplyr::mutate(grain = c("admin1", "admin2"))

  expect_warning(
    resolved <- whep:::resolve_admin_shares(input),
    "more than one grain"
  )
  expect_equal(nrow(resolved$shares), 2L)
  expect_equal(resolved$coverage$resolved_grain, "admin1|admin2")
})

test_that("a mixed-grain candidate is ranked on its coarsest grain", {
  input <- dplyr::bind_rows(
    two_units("mixed_t1", 1L, "admin1", 2000L) |>
      dplyr::mutate(grain = c("admin1", "admin3")),
    two_units("even_t3", 3L, "admin2", 2000L)
  )

  expect_warning(
    resolved <- whep:::resolve_admin_shares(input),
    "more than one grain"
  )
  expect_setequal(resolved$shares$resolved_source, "even_t3")
  expect_setequal(resolved$shares$resolution_rule, "grain")
})

test_that("a malformed constraint_exclude or overrides argument aborts", {
  input <- two_units("public_t1", 1L, "admin1", 2000L)

  expect_error(
    whep:::resolve_admin_shares(input, constraint_exclude = list(1961:1989)),
    class = "whep_error_admin_exclude"
  )
  expect_error(
    whep:::resolve_admin_shares(
      input,
      constraint_exclude = list(USA = 1961:1989)
    ),
    class = "whep_error_admin_exclude"
  )
  expect_error(
    whep:::resolve_admin_shares(
      input,
      constraint_exclude = list("840" = "1961")
    ),
    class = "whep_error_admin_exclude"
  )
  expect_error(
    whep:::resolve_admin_shares(
      input,
      overrides = tibble::tibble(area_code = 840L)
    ),
    class = "whep_error_admin_overrides"
  )
  expect_error(
    whep:::resolve_admin_shares(
      input,
      overrides = tibble::tibble(
        area_code = c(840L, 840L),
        item_prod_code = NA_integer_,
        source = c("a", "b")
      )
    ),
    class = "whep_error_admin_overrides"
  )
})

test_that("an empty constraint_exclude list or override table withholds nothing", {
  input <- two_units("public_t1", 1L, "admin1", 2000:2001)
  base <- whep:::resolve_admin_shares(input)

  expect_equal(
    whep:::resolve_admin_shares(input, constraint_exclude = list()),
    base
  )
  expect_equal(
    whep:::resolve_admin_shares(
      input,
      overrides = tibble::tibble(
        area_code = integer(),
        item_prod_code = integer(),
        source = character()
      )
    ),
    base
  )
})

test_that("a table that is not admin-shares shaped aborts", {
  expect_error(
    whep:::resolve_admin_shares(tibble::tibble(area_code = 840L)),
    class = "whep_error_schema_violation"
  )
  expect_error(
    whep:::resolve_admin_shares("not a table"),
    class = "whep_error_admin_resolve_input"
  )
})

# A seeded random candidate set: four sources with random tiers, grains
# and NUTS versions, ragged year coverage, three indicators (one of which
# never binds), two containers, two units per source.
random_shares <- function(seed) {
  withr::local_seed(seed)
  sources <- c("s_alpha", "s_beta", "s_gamma", "s_delta")
  properties <- tibble::tibble(
    source = sources,
    tier = sample(1:3, length(sources), replace = TRUE),
    grain = sample(
      c("admin1", "admin2", "admin3"),
      length(sources),
      replace = TRUE
    )
  )
  grid <- tidyr::crossing(
    area_code = c(724L, 840L),
    item_prod_code = c(15L, 44L),
    indicator_used = c("area_harvested", "production", "yield"),
    year = 1995:2005,
    source = sources,
    unit = 1:2
  )
  grid[stats::runif(nrow(grid)) < 0.55, ] |>
    dplyr::left_join(properties, by = "source") |>
    dplyr::mutate(
      level_polity_code = paste0(source, "-U", unit),
      value = stats::runif(dplyr::n(), 1, 100),
      nuts_version = sample(
        c(NA, "2016", "2021"),
        dplyr::n(),
        replace = TRUE
      )
    ) |>
    dplyr::select(-unit) |>
    resolve_rows()
}

expect_random_invariants <- function(seed) {
  input <- random_shares(seed)
  overrides <- tibble::tibble(
    area_code = 840L,
    item_prod_code = c(NA_integer_, 44L),
    source = c("s_beta", "s_gamma")
  )
  constraint_exclude <- list("724" = 1998:2000)
  resolve <- function() {
    suppressWarnings(
      whep:::resolve_admin_shares(input, overrides, constraint_exclude)
    )
  }
  resolved <- resolve()

  expect_resolution_invariants(input, resolved)
  testthat::expect_true(all(
    resolved$shares$resolution_rule %in%
      c(
        "grain",
        "tier",
        "run_length",
        "override",
        "single_candidate",
        "source_name"
      )
  ))
  per_year <- resolved$shares |>
    dplyr::summarise(
      n_indicators = dplyr::n_distinct(indicator_used),
      .by = c(area_code, level, item_prod_code, year)
    )
  testthat::expect_setequal(per_year$n_indicators, 1L)
  per_group <- resolved$shares |>
    dplyr::summarise(
      n_sources = dplyr::n_distinct(source),
      .by = c(area_code, level, item_prod_code, indicator_used, year)
    )
  testthat::expect_setequal(per_group$n_sources, 1L)
  cov_key <- resolved$coverage[,
    c("area_code", "level", "item_prod_code", "year")
  ]
  testthat::expect_equal(nrow(dplyr::distinct(cov_key)), nrow(cov_key))
  testthat::expect_equal(resolved, resolve())
}

# Five seeds keep the suite quick; the same sweep was run over seeds
# 1:200 while developing this file, with no invariant violated.
test_that("the invariants hold across seeded random candidate sets", {
  purrr::walk(1:5, expect_random_invariants)
})

test_that("the contiguous run counts consecutive years around the year", {
  candidates <- tibble::tibble(
    area_code = 840L,
    level = 1L,
    item_prod_code = 15L,
    indicator_used = "area_harvested",
    source = "s",
    year = c(1990L, 1991L, 1992L, 1995L, 1996L)
  )
  runs <- whep:::.add_admin_run_lengths(candidates)

  expect_equal(runs$run_length, c(3L, 3L, 3L, 2L, 2L))
})

testthat::test_that("not_shipped flags resolved rows of a withheld family", {
  shares <- .level1_admin_shares()
  out <- whep:::resolve_admin_shares(shares, not_shipped = "fixture")
  testthat::expect_true("not_shipped" %in% names(out$coverage))
  testthat::expect_true(all(out$coverage$not_shipped))
  out2 <- whep:::resolve_admin_shares(shares)
  testthat::expect_false(any(out2$coverage$not_shipped))
})

# --- T38: shares-only rows are ranked and carried ----------------------------
#
# The Latin American family is consented as DERIVED SHARES ONLY (T23,
# 2026-09-02), so `value` is absent on every one of its rows. Resolution
# ranks on indicator, grain, tier and run length, none of which reads
# `value`, so a shares-only candidate must compete and win on the same
# terms as a valued one -- and must arrive intact, still carrying no value.

# Two sibling units of one source that ships shares and no values.
two_share_units <- function(
  source,
  tier,
  grain,
  years,
  area_code = 19L,
  item_prod_code = 661L
) {
  tidyr::crossing(
    year = as.integer(years),
    level_polity_code = paste0(source, c("-U1", "-U2"))
  ) |>
    dplyr::mutate(
      area_code = area_code,
      item_prod_code = item_prod_code,
      indicator_used = "area_harvested",
      nuts_version = NA_character_,
      source = source,
      tier = as.integer(tier),
      grain = grain,
      value = NA_real_,
      share = dplyr::if_else(endsWith(level_polity_code, "-U1"), 0.6, 0.4)
    ) |>
    resolve_rows()
}

test_that("a shares-only candidate wins on grain and keeps no value", {
  rows <- dplyr::bind_rows(
    two_units(
      "Eurostat",
      1,
      "admin1",
      2000,
      area_code = 19L,
      item_prod_code = 661L
    ),
    two_share_units("admin-stats-latam", 3, "admin2", 2000)
  )

  resolved <- whep:::resolve_admin_shares(rows)

  expect_equal(unique(resolved$shares$resolved_source), "admin-stats-latam")
  expect_equal(unique(resolved$coverage$resolution_rule), "grain")
  expect_true(all(is.na(resolved$shares$value)))
  expect_equal(sort(resolved$shares$share), c(0.4, 0.6))
  expect_equal(unique(resolved$dropped$source), "Eurostat")
})

test_that("a shares-only candidate can also lose, and is dropped whole", {
  rows <- dplyr::bind_rows(
    two_units(
      "ES_provinces",
      2,
      "admin2",
      2000,
      area_code = 19L,
      item_prod_code = 661L
    ),
    two_share_units("admin-stats-latam", 3, "admin1", 2000)
  )

  resolved <- whep:::resolve_admin_shares(rows)

  expect_equal(unique(resolved$shares$resolved_source), "ES_provinces")
  expect_equal(unique(resolved$dropped$source), "admin-stats-latam")
  expect_true(all(is.na(resolved$dropped$value)))
})

test_that("a shares-only series is a run like any other", {
  # Run length is counted on years present, not on values present.
  rows <- dplyr::bind_rows(
    two_share_units("latam_long", 3, "admin1", 2000:2004),
    two_units(
      "rival_short",
      3,
      "admin1",
      2002,
      area_code = 19L,
      item_prod_code = 661L
    )
  )

  resolved <- whep:::resolve_admin_shares(rows)

  won <- dplyr::filter(resolved$coverage, year == 2002L)
  expect_equal(won$resolved_source, "latam_long")
  expect_equal(won$resolution_rule, "run_length")
})

test_that("resolve_admin_shares aborts on a row with neither measurement", {
  rows <- two_share_units("admin-stats-latam", 3, "admin1", 2000)
  rows$share <- NA_real_

  expect_error(
    whep:::resolve_admin_shares(rows),
    class = "whep_error_admin_no_measure"
  )
})

test_that("resolve_admin_shares aborts on a non-finite measurement", {
  # `is.na(NaN)` is TRUE, so without this rule a 0/0 artefact enters
  # resolution indistinguishable from the consented shares-only case and
  # wins or loses a container on a value nobody computed.
  rows <- two_share_units("admin-stats-latam", 3, "admin1", 2000)
  rows$value <- c(NaN, NaN)

  expect_error(
    whep:::resolve_admin_shares(rows),
    class = "whep_error_admin_nonfinite"
  )
})
