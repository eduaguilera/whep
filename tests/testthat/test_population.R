# Tests for read_population().

.popf_raw <- function() {
  tibble::tribble(
    ~Year,
    ~area,
    ~area_code,
    ~pop,
    2010L,
    "Spain",
    "ESP",
    46.6,
    2010L,
    "Germany",
    "DEU",
    81.8,
    2010L,
    "Ethiopia",
    "ETH",
    87.6,
    2010L,
    "Sudan",
    "SDN",
    35.0,
    2010L,
    "Africa Other",
    "RAFR",
    0.2,
    2011L,
    "Spain",
    "ESP",
    46.7
  )
}

# The rows of the real `gdp-population` pin for 2015 whose ISO3 codes share an
# `area_code` with another, plus Spain as an unfolded control and one regional
# residual. Real pin values (thousands), so the folded totals this fixture
# produces are the ones a real read_population(years = 2015L) prints: 999 =
# 28,278,673 and 206 = 49,365,472.
.popf_folded <- function() {
  tibble::tribble(
    ~Year, ~area,               ~area_code, ~pop,
    2015L, "Spain",             "ESP",      46431.344,
    2015L, "Sudan",             "SDN",      38171.172,
    2015L, "South Sudan",       "SSD",      11194.300,
    2015L, "Syria",             "SYR",      19205.178,
    2015L, "North Macedonia",   "MKD",      2107.965,
    2015L, "Palestine",         "PSE",      4484.615,
    2015L, "Eswatini",          "SWZ",      1133.941,
    2015L, "Equatorial Guinea", "GNQ",      1346.974,
    2015L, "Africa Other",      "RAFR",     2498.894
  )
}

testthat::test_that("the example fixture matches the documented contract", {
  out <- whep::read_population(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  # The polity columns are part of the contract now, not an extra: `read_population()`
  # resolves ISO3 to a numeric `area_code` and carries a `year`, so it is an area-keyed
  # export with a year and #424 requires it to say which polity each row belongs to.
  # `expect_named()` is exact, so it has to name them -- and asserting the exact set is
  # the point: a silently dropped polity column should fail here.
  testthat::expect_named(
    out,
    c(
      "year",
      "area_code",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "population",
      "method_territory_overlap"
    )
  )
  testthat::expect_true(all(out$population > 0))
  testthat::expect_type(out$area_code, "integer")
  # Populated, not merely present.
  testthat::expect_false(anyNA(out$reporting_polity_code))
})

testthat::test_that("ISO3 becomes a numeric area code and thousands persons", {
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_raw())),
    "no numeric"
  )
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  esp <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 203L)
  testthat::expect_equal(esp$population, 46600)
  deu <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 79L)
  testthat::expect_equal(deu$population, 81800)
})

testthat::test_that("historical twin ISO3 codes resolve to one area code each", {
  # ETH is both 238 (Ethiopia) and 62 (Ethiopia PDR) in regions_full$code, and
  # SDN both 276 and 206. Mapping through polity_area_code collapses each pair,
  # so neither country is duplicated nor lands on its predecessor.
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  testthat::expect_equal(sum(out$area_code == 238L & out$year == 2010L), 1L)
  testthat::expect_equal(sum(out$area_code == 206L & out$year == 2010L), 1L)
  testthat::expect_false(any(out$area_code %in% c(62L, 276L)))
})

testthat::test_that("regional residual aggregates are dropped and reported", {
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_raw())),
    "RAFR"
  )
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  testthat::expect_false(any(is.na(out$area_code)))
  # The residual's 200 people are gone, so the total is countries-only.
  testthat::expect_equal(sum(out$population[out$year == 2010L]), 251000)
})

testthat::test_that("years filter the result", {
  out <- suppressMessages(
    whep::read_population(
      years = 2011L,
      data = list(gdp_population = .popf_raw())
    )
  )
  testthat::expect_setequal(out$year, 2011L)
  testthat::expect_equal(nrow(out), 1L)
})

testthat::test_that("missing required columns abort", {
  testthat::expect_error(
    whep::read_population(data = list(gdp_population = tibble::tibble(x = 1))),
    "gdp_population"
  )
})

# ---- The many-to-one fold onto aggregate buckets ---------------------------
#
# `area_code` is `polity_area_code`, a bucket rather than an identity, so these
# tests pin the aggregation BY VALUE: it has to read as deliberate, and a change
# to which ISO3 codes land on 999 or 206 has to fail something (#482).

testthat::test_that("the Rest-of-World fold is reported, not silent", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_folded())),
    "aggregate"
  )
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_folded())),
    "999 \\(GNQ \\+ MKD \\+ PSE \\+ SWZ \\+ SYR\\)"
  )
  # Two buckets, seven ISO3 codes between them: 5 on 999 and SDN + SSD on 206.
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_folded())),
    "Folded 7 ISO3 codes into 2 aggregate"
  )
})

testthat::test_that("the folded rows carry the summed population", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_folded()))
  )
  # These are the real 2015 pin values: reading them off the fixture must give
  # the same numbers a real read_population(years = 2015L) prints.
  row_of <- function(code) {
    dplyr::pull(dplyr::filter(out, .data$area_code == code), "population")
  }
  testthat::expect_equal(row_of(999L), 28278673)
  testthat::expect_equal(row_of(206L), 49365472)
  testthat::expect_equal(row_of(203L), 46431344)
  # One row per bucket, and no member territory survives as its own row.
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_setequal(out$area_code, c(203L, 206L, 999L))
  # The output itself says 999 is not a country.
  testthat::expect_equal(
    dplyr::pull(
      dplyr::filter(out, .data$area_code == 999L),
      "polity_area_code"
    ),
    999L
  )
})

testthat::test_that("the fold-bucket summary lists members per bucket", {
  # Scoped to the explicit fold. WHEP now models the reporting members of
  # bucket 999 in their own right (#459), so there is no Rest-of-World fold
  # by default; what this pins is the fold behaviour itself, which still has
  # to work for anyone reproducing a published-before number.
  withr::local_options(whep.unfold_rest_of_world = "none")
  parsed <- whep:::.pop_parse(.popf_folded(), NULL)
  folded <- whep:::.pop_folded_buckets(whep:::.pop_folded_cells(parsed))
  testthat::expect_equal(folded$area_code, c(206L, 999L))
  testthat::expect_equal(folded$n_iso3, c(2L, 5L))
  testthat::expect_equal(folded$codes[[1]], "SDN + SSD")
  # An unfolded pin says nothing.
  unfolded <- whep:::.pop_parse(
    tibble::tibble(Year = 2015L, area_code = "ESP", pop = 46431.344),
    NULL
  )
  testthat::expect_equal(nrow(whep:::.pop_folded_cells(unfolded)), 0L)
  testthat::expect_null(whep:::.pop_report_folded(unfolded))
})

# ---- The areas the denominator never covers --------------------------------
#
# The fold above is the small half of #543. The large half is coverage: the pin
# reaches 190 of the 256 area codes the crosswalk resolves, and both per-capita
# consumers inner-join this table, so an uncovered area is missing from their
# output rather than wrong in it. These pin the report that says so.

# Two areas the real pin does not cover (Bhutan 18, Comoros 45), one it does
# (Spain 203), and Bhutan appearing in two years so the area-year count cannot
# be confused with the area count.
.popf_agg <- function() {
  tibble::tribble(
    ~year, ~area_code, ~protein_t,
    2010L, 18L,        30,
    2010L, 45L,        20,
    2010L, 203L,       950,
    2011L, 18L,        10
  )
}

.popf_denominator <- function() {
  tibble::tribble(
    ~year, ~area_code, ~population,
    2010L, 203L,       46431344
  )
}

testthat::test_that("areas with no denominator are named, not dropped silently", {
  testthat::expect_warning(
    whep:::.warn_missing_population(
      .popf_agg(),
      .popf_denominator(),
      "protein_t",
      "food protein"
    ),
    "Bhutan \\(18, 2 area-years\\)"
  )
  testthat::expect_warning(
    whep:::.warn_missing_population(
      .popf_agg(),
      .popf_denominator(),
      "protein_t",
      "food protein"
    ),
    "Comoros \\(45, 1 area-year\\)"
  )
  # Three area-years over two areas: counting areas alone would say 2, and the
  # 2011 Bhutan row is a separate loss.
  testthat::expect_warning(
    whep:::.warn_missing_population(
      .popf_agg(),
      .popf_denominator(),
      "protein_t",
      "food protein"
    ),
    "2 areas .* 3 area-years"
  )
})

testthat::test_that("the share is of the quantity in range, not of the world", {
  dropped <- suppressWarnings(
    whep:::.warn_missing_population(
      .popf_agg(),
      .popf_denominator(),
      "protein_t",
      "food protein"
    )
  )
  # Heaviest first, one row per area, keyed on area_code and never on a label.
  testthat::expect_equal(dropped$area_code, c(18L, 45L))
  testthat::expect_equal(dropped$mass, c(40, 20))
  testthat::expect_equal(dropped$area_years, c(2L, 1L))
  # 60 of the 1010 protein tonnes in range = 5.94%, the number the message
  # quotes. Measuring it against world population instead is what let a 19%
  # loss read as 0.07% (#543).
  testthat::expect_warning(
    whep:::.warn_missing_population(
      .popf_agg(),
      .popf_denominator(),
      "protein_t",
      "food protein"
    ),
    "5\\.94"
  )
})

testthat::test_that("a fully covered denominator says nothing", {
  covered <- tibble::tribble(
    ~year, ~area_code, ~population,
    2010L, 18L,        750000,
    2010L, 45L,        700000,
    2010L, 203L,       46431344,
    2011L, 18L,        755000
  )
  testthat::expect_silent(
    out <- whep:::.warn_missing_population(
      .popf_agg(),
      covered,
      "protein_t",
      "food protein"
    )
  )
  testthat::expect_equal(nrow(out), 0L)
})

testthat::test_that("the coverage warning can be switched off", {
  withr::local_options(whep.warn_missing_population = FALSE)
  testthat::expect_silent(
    whep:::.warn_missing_population(
      .popf_agg(),
      .popf_denominator(),
      "protein_t",
      "food protein"
    )
  )
})

testthat::test_that("a bucket with one member in a year is not a fold", {
  # 206 carries Sudan alone before South Sudan exists, so that row IS a country
  # and must not be counted as an aggregate. Counting per bucket instead of per
  # bucket-year would report every pre-2012 Sudan row as folded.
  pre <- tibble::tribble(
    ~Year, ~area_code, ~pop,
    2005L, "SDN",      33.0,
    2005L, "ESP",      43.0,
    2015L, "SDN",      38.0,
    2015L, "SSD",      11.0,
    2015L, "ESP",      46.0
  )
  cells <- whep:::.pop_folded_cells(whep:::.pop_parse(pre, NULL))
  testthat::expect_setequal(cells$year, 2015L)
  testthat::expect_setequal(cells$iso3c, c("SDN", "SSD"))
  # 49,000 of the 171,000 persons in range, not the 82,000 that counting whole
  # buckets (SDN 2005 + SDN 2015 + SSD 2015) would claim.
  testthat::expect_equal(sum(cells$population), 49000)
})

# ---- population_source: the UN WPP fallback (#644) --------------------------

.popf_wpp <- function() {
  tibble::tribble(
    ~year, ~area_code, ~iso3c, ~population,
    2010L, 203L,       "ESP",  46000000,
    2010L, 18L,        "BTN",  701633,
    2010L, 45L,        "COM",  654839
  )
}

testthat::test_that("the default source is the pin alone", {
  # Nothing published may move by default, so the fallback must not be reached
  # unless it is asked for. An injected reader that errors proves it is not.
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  testthat::expect_false(any(out$area_code %in% c(18L, 45L)))
  testthat::expect_equal(unique(out$source_pop), "pin")
})

testthat::test_that("the fallback fills areas the pin never covers", {
  out <- suppressMessages(
    whep::read_population(
      data = list(gdp_population = .popf_raw(), wpp_population = .popf_wpp()),
      population_source = "pin_wpp_fallback"
    )
  )
  filled <- dplyr::filter(out, .data$area_code %in% c(18L, 45L))
  testthat::expect_setequal(filled$area_code, c(18L, 45L))
  testthat::expect_equal(unique(filled$source_pop), "UN WPP 2024")
  testthat::expect_equal(
    dplyr::filter(filled, .data$area_code == 18L)$population,
    701633
  )
})

testthat::test_that("the fallback never overwrites a pin row", {
  # Spain is in both, at different values. The pin has to win, or turning the
  # fallback on would silently move a published denominator.
  out <- suppressMessages(
    whep::read_population(
      data = list(gdp_population = .popf_raw(), wpp_population = .popf_wpp()),
      population_source = "pin_wpp_fallback"
    )
  )
  esp <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 203L)
  testthat::expect_equal(nrow(esp), 1L)
  testthat::expect_equal(esp$population, 46600)
  testthat::expect_equal(esp$source_pop, "pin")
})

testthat::test_that("the pin's own Source_pop vocabulary is carried", {
  raw <- dplyr::mutate(
    .popf_raw(),
    Source_pop = ifelse(
      .data$area_code == "ESP",
      "Original",
      "Linear
      interpolation"
    )
  )
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = raw))
  )
  esp <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 203L)
  testthat::expect_equal(esp$source_pop, "Original")
})

testthat::test_that("a bucket mixing sources reports both, not one", {
  # area_code is a bucket: several ISO3 codes are summed into one row, and they
  # need not share a provenance. Reporting only the first would hide that half
  # the row is interpolated.
  raw <- tibble::tribble(
    ~Year, ~area,         ~area_code, ~pop,  ~Source_pop,
    2015L, "Sudan",       "SDN",      38171, "Original",
    2015L, "South Sudan", "SSD",      11194, "Linear interpolation"
  )
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = raw))
  )
  testthat::expect_equal(
    out$source_pop,
    "Linear interpolation + Original"
  )
})

testthat::test_that("an unknown population_source is rejected", {
  testthat::expect_error(
    whep::read_population(
      data = list(gdp_population = .popf_raw()),
      population_source = "wpp"
    ),
    "arg_match|must be one of|wpp"
  )
})

# ---- population_source: the FAOSTAT FBS fallback (#862) ---------------------

# A read_fbs_population() output. Real `faostat-fbs-old` values for area 186
# Serbia and Montenegro, which neither the pin nor UN WPP can reach, plus Spain
# at a DIFFERENT value from the pin so the precedence is testable.
.popf_fbs <- function() {
  tibble::tribble(
    ~year, ~area_code, ~population, ~source_pop,
    2000L, 186L,       10801000,    "FAOSTAT FBS old",
    2010L, 203L,       46840470,    "FAOSTAT FBS new",
    2010L, 151L,       201000,      "FAOSTAT FBS old"
  )
}

testthat::test_that("the FBS fill is not reached unless it is asked for", {
  # Nothing published may move by default. An injected FBS table carrying
  # areas the pin lacks proves neither of the two other sources reaches it.
  out <- suppressMessages(
    whep::read_population(
      data = list(
        gdp_population = .popf_raw(),
        wpp_population = .popf_wpp(),
        fbs_population = .popf_fbs()
      ),
      population_source = "pin_wpp_fallback"
    )
  )
  testthat::expect_false(any(out$area_code %in% c(186L, 151L)))
})

testthat::test_that("the FBS fill closes the dissolved-federation areas", {
  # #862 and #787: area 186 Serbia and Montenegro and area 151 Netherlands
  # Antilles carry commodity-balance food and have no denominator, because both
  # other sources are keyed on a present-day ISO3 that no longer names them.
  out <- suppressMessages(
    whep::read_population(
      data = list(
        gdp_population = .popf_raw(),
        wpp_population = .popf_wpp(),
        fbs_population = .popf_fbs()
      ),
      population_source = "pin_wpp_fbs_fallback"
    )
  )
  filled <- dplyr::filter(out, .data$area_code %in% c(186L, 151L))
  testthat::expect_setequal(filled$area_code, c(186L, 151L))
  testthat::expect_equal(unique(filled$source_pop), "FAOSTAT FBS")
  testthat::expect_equal(
    dplyr::filter(filled, .data$area_code == 186L)$population,
    10801000
  )
})

testthat::test_that("the FBS fill never overwrites a pin or WPP row", {
  # Spain is in the pin and in the FBS table at different values, and Bhutan is
  # in WPP and could be in neither. Turning the fill on must be incapable of
  # moving a denominator that was already published.
  out <- suppressMessages(
    whep::read_population(
      data = list(
        gdp_population = .popf_raw(),
        wpp_population = .popf_wpp(),
        fbs_population = dplyr::bind_rows(
          .popf_fbs(),
          tibble::tibble(
            year = 2010L,
            area_code = 18L,
            population = 1,
            source_pop = "FAOSTAT FBS old"
          )
        )
      ),
      population_source = "pin_wpp_fbs_fallback"
    )
  )
  esp <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 203L)
  testthat::expect_equal(esp$population, 46600)
  testthat::expect_equal(esp$source_pop, "pin")
  btn <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 18L)
  testthat::expect_equal(btn$population, 701633)
  testthat::expect_equal(btn$source_pop, "UN WPP 2024")
})

# ---- Two codes, one territory (#939) ---------------------------------------
#
# Neither fill can overwrite a key the previous source already has, and that is
# verifiable bitwise on the real pins. It does not make the composed table
# duplicate-free: two DIFFERENT area codes can name the same ground in the same
# year, and an anti-join on `(year, area_code)` cannot see it. These pin the
# guard that can, on the three real shapes -- one inside the pin itself, one
# created by the UN WPP fill, one by the FAOSTAT FBS fill.

# Real `gdp-population` values (thousands) for 1961. The pin carries `CSK`
# 1850-1992 AND `CZE` 1850-2021, so Czechia is reported inside Czechoslovakia
# for 143 years by the DEFAULT source, with no fill involved. `SUN` is the
# federation the WPP fill duplicates; Spain is the control that overlaps
# nothing.
.popf_overlap <- function() {
  tibble::tribble(
    ~Year, ~area,            ~area_code, ~pop,
    1961L, "Czechoslovakia", "CSK",      13755.489,
    1961L, "Czechia",        "CZE",      9570.406,
    1961L, "USSR",           "SUN",      214456.648,
    1961L, "Spain",          "ESP",      30711.868
  )
}

# A read_wpp_population() output for the same year: the real WPP 2024 figure for
# Russia in 1961, a successor state the pin has no row for at all, so the fill
# adds it beside the pin's USSR row.
.popf_wpp_successor <- function() {
  tibble::tribble(
    ~year, ~area_code, ~iso3c, ~population,
    1961L, 185L,       "RUS",  121604302
  )
}

.popf_overlap_read <- function(...) {
  suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_overlap()), ...)
  )
}

testthat::test_that("the pin's own federation-inside-federation row goes", {
  # 51 Czechoslovakia and 167 Czechia are both `Original` rows of the same pin
  # in the same year, so this half of the defect is in the DEFAULT source and no
  # fill has to be switched on to reach it.
  out <- .popf_overlap_read()
  testthat::expect_false(any(out$area_code == 167L))
  testthat::expect_setequal(out$area_code, c(51L, 203L, 228L))
  # The federation's own value is untouched; only the duplicate row went.
  testthat::expect_equal(
    dplyr::pull(dplyr::filter(out, .data$area_code == 51L), "population"),
    13755489
  )
})

testthat::test_that("keeping both is what double counts, and it is measured", {
  # `"none"` is the pre-#939 composition. 9,570,406 of the 268,494,411 persons
  # in the year is 3.56%, which is what the warning has to quote: the guard is
  # only worth having if the number it removes is the number it reports.
  out <- suppressWarnings(.popf_overlap_read(territory_overlap = "none"))
  testthat::expect_true(all(c(51L, 167L) %in% out$area_code))
  testthat::expect_equal(sum(out$population), 268494411)
  testthat::expect_warning(
    whep::read_population(
      data = list(gdp_population = .popf_overlap()),
      territory_overlap = "none"
    ),
    "double counts"
  )
  testthat::expect_warning(
    whep::read_population(
      data = list(gdp_population = .popf_overlap()),
      territory_overlap = "none"
    ),
    "3\\.56"
  )
})

testthat::test_that("the overlap is named, not resolved in silence", {
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_overlap())),
    "Czechoslovakia \\(51, 1961-1961\\) over 167"
  )
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_overlap())),
    "Kept the federation"
  )
})

testthat::test_that("the successors branch drops the federation instead", {
  # The alternative the maintainer may prefer, and the reason it is not the
  # default: 51 covers Slovakia as well as Czechia, so keeping 167 and dropping
  # 51 removes Slovakia's people from the year rather than de-duplicating
  # anything -- 13,755,489 persons out instead of 9,570,406.
  out <- .popf_overlap_read(territory_overlap = "successors")
  testthat::expect_false(any(out$area_code == 51L))
  testthat::expect_true(any(out$area_code == 167L))
  testthat::expect_equal(sum(out$population), 254738922)
})

testthat::test_that("the WPP fill cannot add a successor beside its federation", {
  # 185 Russia has no pin row in 1961 at all, so the anti-join lets it in beside
  # the pin's 228 USSR: same ground, two codes, both summed. This is the half of
  # the defect that made the real 1961 world sum 8.2% high.
  out <- suppressMessages(
    whep::read_population(
      data = list(
        gdp_population = .popf_overlap(),
        wpp_population = .popf_wpp_successor()
      ),
      population_source = "pin_wpp_fallback"
    )
  )
  testthat::expect_false(any(out$area_code == 185L))
  testthat::expect_equal(
    dplyr::pull(dplyr::filter(out, .data$area_code == 228L), "population"),
    214456648
  )
})

testthat::test_that("the FBS fill cannot stack a federation on WPP's parts", {
  # 1992-2005 is where the two fills meet: the FBS fill supplies 186 Serbia and
  # Montenegro (the row #862 wants) while the WPP fill has already supplied 272
  # Serbia and 273 Montenegro for the same years. All three describe the same
  # ground.
  out <- suppressMessages(
    whep::read_population(
      data = list(
        gdp_population = tibble::tribble(
          ~Year, ~area,   ~area_code, ~pop,
          2000L, "Spain", "ESP",      40283
        ),
        wpp_population = tibble::tribble(
          ~year, ~area_code, ~iso3c, ~population,
          2000L, 272L,       "SRB",  7694604,
          2000L, 273L,       "MNE",  634195
        ),
        fbs_population = tibble::tribble(
          ~year, ~area_code, ~population, ~source_pop,
          2000L, 186L,       10801000,    "FAOSTAT FBS old"
        )
      ),
      population_source = "pin_wpp_fbs_fallback"
    )
  )
  testthat::expect_setequal(out$area_code, c(186L, 203L))
  testthat::expect_equal(
    dplyr::pull(dplyr::filter(out, .data$area_code == 186L), "population"),
    10801000
  )
})

testthat::test_that("a table with no overlap is left alone and says nothing", {
  # The branch that must not fire. Every area in this fixture is disjoint from
  # every other, so the guard has to be a no-op and emit no message of its own.
  before <- suppressMessages(
    whep::read_population(
      data = list(gdp_population = .popf_raw()),
      territory_overlap = "none"
    )
  )
  # No overlap, so `"none"` does not warn either: the warning is the duplicate's
  # and not the mode's.
  testthat::expect_no_warning(suppressMessages(
    whep::read_population(
      data = list(gdp_population = .popf_raw()),
      territory_overlap = "none"
    )
  ))
  after <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  testthat::expect_equal(
    dplyr::select(after, -"method_territory_overlap"),
    dplyr::select(before, -"method_territory_overlap")
  )
  testthat::expect_equal(nrow(whep:::.pop_overlap_pairs(after)), 0L)
  testthat::expect_null(
    whep:::.pop_report_overlaps(after, whep:::.pop_overlap_pairs(after), "none")
  )
})

testthat::test_that("the chosen treatment is recorded in the output", {
  out <- .popf_overlap_read(territory_overlap = "successors")
  testthat::expect_equal(unique(out$method_territory_overlap), "successors")
  testthat::expect_equal(
    unique(.popf_overlap_read()$method_territory_overlap),
    "federation"
  )
})

testthat::test_that("an unknown territory_overlap is rejected", {
  testthat::expect_error(
    whep::read_population(
      data = list(gdp_population = .popf_raw()),
      territory_overlap = "drop"
    ),
    "arg_match|must be one of|drop"
  )
})
