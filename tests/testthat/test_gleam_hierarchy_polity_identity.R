# gleam_geographic_hierarchy is a 204-country table carrying its own `continent`,
# `faostat_region`, `eu27` and `oecd` attributes, and .energy_country_grouping() derives
# three GLEAM regional schemes from it in code. Neither the table nor the derivation
# was compared against the polities, and the table is internal, so the sweep over
# EXPORTED datasets never reached it.
#
# The comparison that matters here is `continent`, because both tables carry one and
# nothing required them to agree. Measured: 39 of 201 comparable rows differ, and every
# single one is the Americas split -- the polities database says "North America" (26) or
# "South America" (13) where GLEAM says "Americas". The other 162 agree exactly across
# Africa, Asia, Europe and Oceania.
#
# So the two are compatible at different granularities, not in conflict. That is worth
# ASSERTING rather than noting, because a genuine error -- a country placed on the wrong
# continent -- would sit inside a count of 39 differences and look like more of the same.
# Declaring the correspondence makes the 40th difference a failure.
#
# The 4 GLEAM codes that reach no 2020-live polity are ATF, SGS, WLF and ESH: French
# Southern Territories, South Georgia, Wallis and Futuna, and Western Sahara. All four
# are territories whose data folds into an aggregate, so their crosswalk row carries the
# aggregate's iso3 rather than their own. Pinned by name so a fifth is looked at.
testthat::test_that("GLEAM continents agree with the polities at their own granularity", {
  g <- as.data.frame(get(
    "gleam_geographic_hierarchy",
    envir = asNamespace("whep")
  ))
  testthat::expect_true(all(c("iso3", "continent") %in% names(g)))
  testthat::expect_gt(nrow(g), 200L)

  p <- as.data.frame(whep::polities)
  live <- which(
    !is.na(p$iso3_code) &
      !is.na(p$continent) &
      p$start_year <= 2020L &
      p$end_year > 2020L
  )
  pc <- p[live, c("iso3_code", "continent")]
  pc <- pc[!duplicated(pc$iso3_code), ]

  idx <- match(g$iso3, pc$iso3_code)
  from_polities <- pc$continent[idx]
  comparable <- !is.na(from_polities) & !is.na(g$continent)
  # Non-vacuous: an empty overlap would make the loop below compare nothing.
  testthat::expect_gt(sum(comparable), 190L)

  # The declared correspondence: GLEAM's coarser label maps to a set of polity
  # continents. Anything outside it is a real disagreement.
  coarser <- list("Americas" = c("North America", "South America"))
  ok <- vapply(
    which(comparable),
    function(i) {
      gl <- g$continent[i]
      pol <- from_polities[i]
      if (identical(gl, pol)) {
        return(TRUE)
      }
      allowed <- coarser[[gl]]
      !is.null(allowed) && pol %in% allowed
    },
    logical(1)
  )
  testthat::expect_equal(
    sum(!ok),
    0L,
    info = paste0(
      "countries whose GLEAM continent is neither equal to nor a declared ",
      "coarsening of the polities' continent: ",
      paste(
        utils::head(
          sprintf(
            "%s (gleam %s, polities %s)",
            g$iso3[which(comparable)][!ok],
            g$continent[which(comparable)][!ok],
            from_polities[which(comparable)][!ok]
          ),
          6
        ),
        collapse = "; "
      )
    )
  )

  # And the coarsening must actually be exercised, otherwise the allowance above is
  # dead weight that would hide a future mismatch.
  americas <- sum(
    comparable &
      g$continent == "Americas" &
      from_polities %in% c("North America", "South America")
  )
  testthat::expect_equal(americas, 39L)
})

testthat::test_that("GLEAM and LDC country codes are real polities", {
  g <- as.data.frame(get(
    "gleam_geographic_hierarchy",
    envir = asNamespace("whep")
  ))
  cw <- as.data.frame(whep::polity_area_crosswalk)
  iso <- unique(cw$iso3_code[which(
    !is.na(cw$iso3_code) &
      nzchar(cw$iso3_code) &
      cw$polity_start_year <= 2020L &
      cw$polity_end_year > 2020L
  )])

  # Folded territories carry their aggregate's iso3 in the crosswalk, so these four
  # reach no polity of their own. Pinned by name, not tolerated by a count.
  testthat::expect_setequal(
    setdiff(g$iso3, iso),
    c("ATF", "SGS", "WLF", "ESH")
  )

  # The least-developed-country list is a UN designation, so it is legitimately
  # hardcoded rather than derived -- but every code in it must still name a real
  # territory, since a typo would silently drop a country from the grouping.
  ldc <- whep:::.energy_ldc_iso3()
  testthat::expect_equal(sum(duplicated(ldc)), 0L)
  testthat::expect_equal(
    length(setdiff(ldc, iso)),
    0L,
    info = paste0(
      "LDC codes naming no live polity: ",
      paste(setdiff(ldc, iso), collapse = ", ")
    )
  )

  # TUV is in the LDC list but absent from the hierarchy the list is applied to, so
  # its branch of .energy_country_grouping()'s case_when can never fire. Harmless
  # today -- there is no Tuvalu row to classify -- but pinned so that a SECOND inert
  # entry is noticed rather than blending into an existing exception.
  testthat::expect_setequal(setdiff(ldc, g$iso3), "TUV")
})
