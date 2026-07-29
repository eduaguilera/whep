# The gdp-population input reaches the CBS build through .fill_with_proxies(), which joins on
# `c("year", "area")` — the area NAME, not a code. So a name this pin spells differently from
# FAOSTAT is a proxy that silently does not apply: an unmatched join produces NA, not an error,
# and the country simply goes unfilled.
#
# Eleven of its 196 names matched nothing, 1,892 rows or 6.4% of the input: Bolivia,
# China Taiwan, DR Congo, Iran, Ivory Coast, Lao, North Korea, Syria, Tanzania, Turkey,
# Venezuela. All short forms where the crosswalk carries FAOSTAT's long ones — "Côte d'Ivoire",
# "Türkiye", "Lao People's Democratic Republic".
#
# Every one resolves through the published alias map, so .read_gdp_pop() now resolves the name
# to a POLITY and substitutes that polity's canonical area name. Deliberately not a synonym
# list written by hand here: the alias map is the place that knowledge belongs, and it is
# already gated upstream.

testthat::test_that("every gdp-population area name matches a crosswalk area", {
  dt <- tryCatch(whep:::.read_gdp_pop(), error = function(e) NULL)
  testthat::skip_if(is.null(dt), "gdp-population pin unavailable")
  testthat::expect_gt(nrow(dt), 20000L)

  canonical <- unique(stats::na.omit(
    as.data.frame(whep::polity_area_crosswalk)$area_name
  ))
  unmatched <- sort(setdiff(unique(stats::na.omit(dt$area)), canonical))
  testthat::expect_equal(
    length(unmatched),
    0L,
    info = paste0(
      "these gdp-population names join to no crosswalk area, so their population and land ",
      "proxies never apply: ",
      paste(utils::head(unmatched, 10), collapse = ", ")
    )
  )
})

testthat::test_that("canonicalisation leaves already-matching names alone", {
  # The rewrite must be narrow. If it also touched names that already matched, a future
  # crosswalk rename could silently redirect data that was arriving correctly.
  raw <- tryCatch(whep:::.read_input("gdp-population"), error = function(e) {
    NULL
  })
  testthat::skip_if(is.null(raw), "gdp-population pin unavailable")
  fixed <- whep:::.canonicalise_gdp_pop_area(data.table::as.data.table(raw))

  canonical <- unique(stats::na.omit(
    as.data.frame(whep::polity_area_crosswalk)$area_name
  ))
  was_ok <- !is.na(raw$area) & raw$area %in% canonical
  testthat::expect_gt(sum(was_ok), 20000L)
  testthat::expect_identical(fixed$area[was_ok], raw$area[was_ok])
})

testthat::test_that("the eleven renamed countries actually reach a population proxy", {
  # The name check above is necessary but not sufficient: matching the crosswalk's vocabulary
  # does not by itself prove the proxy JOIN succeeds. This asserts the outcome that matters —
  # .fill_with_proxies() merges on c("year", "area"), so a CBS frame carrying canonical names
  # must find a population for each.
  #
  # Measured both ways rather than reasoned about. With canonicalisation, 33 of 33 requested
  # rows (eleven countries x three years) find a population. Without it, 0 of 33 — every one
  # silently NA, which is what the build was doing before.
  gdp <- tryCatch(whep:::.read_gdp_pop(), error = function(e) NULL)
  testthat::skip_if(is.null(gdp), "gdp-population pin unavailable")

  canonical <- c(
    "Bolivia (Plurinational State of)",
    "China, Taiwan Province of",
    "Democratic Republic of the Congo",
    "Iran (Islamic Republic of)",
    "C\u00f4te d'Ivoire",
    "Lao People's Democratic Republic",
    "Democratic People's Republic of Korea",
    "Syrian Arab Republic",
    "United Republic of Tanzania",
    "T\u00fcrkiye",
    "Venezuela (Bolivarian Republic of)"
  )
  frame <- expand.grid(
    year = c(1990L, 2000L, 2010L),
    area = canonical,
    stringsAsFactors = FALSE
  )
  pop <- unique(as.data.frame(gdp)[, c("year", "area", "pop")])
  joined <- merge(frame, pop, by = c("year", "area"), all.x = TRUE)

  unmatched <- joined[is.na(joined$pop), ]
  testthat::expect_equal(
    nrow(unmatched),
    0L,
    info = paste0(
      "these canonical area names find no population proxy, so .fill_with_proxies() will ",
      "leave them NA: ",
      paste(unique(unmatched$area), collapse = ", ")
    )
  )
})
