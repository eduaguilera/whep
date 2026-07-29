# expand_trade_sources() was the last exported area-keyed table in this package with no
# polity on it. Its rows are already one-year-per-row, which is exactly the shape
# year-aware resolution needs, so the Reporter and the Year settle each row with no
# interpolation.
#
# 34 aliases were added UPSTREAM under source "trade-sources" (whep-polities#39) rather
# than name-matched here, because that repository owns label-to-polity identity. One
# alias per polity period each reporter's own span crosses:
#
#   United Kingdom  2   Germany  8   China  10   United States  4
#   India           3   France   3   Canada  3   Egypt           1
#
# Only 8 distinct reporters, and 6 of their names match a canonical area name exactly.
# The two that do not -- "United Kingdom of Great Britain and Northern Ireland (the)"
# and "United States of America (the)" -- differ by the trailing UN M49 "(the)" and
# nothing else, which is precisely what an alias table is for.
#
# CHINA COULD NOT GO THROUGH AN AREA AT ALL. Its FAOSTAT area 351 is the deliberate
# China aggregate that maps to no polity, so area-mediated lookup returns nothing. The
# aliases target the CHN chain directly, which the alias table permits because it maps
# labels to polities rather than to areas. The superseded CHN-1921-1945 is excluded, so
# 1921-1945 resolves through CHN-1921-1932 and CHN-1932-1945 instead of a row that may
# never receive data.
#
# Coverage is 1,133 of 1,133 expanded rows across 31 distinct polities. Asserted at
# 100% rather than as a floor: unlike the Lassaletta names, this is a closed set of 8
# reporters over known spans, so anything less than complete is a defect and not a
# backlog.
testthat::test_that("every expanded trade-source row carries the polity of its reporter and year", {
  path <- system.file("extdata", "trade_sources.csv", package = "whep")
  testthat::skip_if(path == "", "trade_sources.csv not installed")
  src <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  testthat::expect_true(all(c("Reporter", "Timeline_Start") %in% names(src)))

  out <- as.data.frame(expand_trade_sources(src))
  testthat::expect_true("reporting_polity_code" %in% names(out))
  # Non-vacuous: an empty expansion would make the coverage assertion meaningless.
  testthat::expect_gt(nrow(out), 1000L)

  missing <- out[is.na(out$reporting_polity_code), ]
  testthat::expect_equal(
    nrow(missing),
    0L,
    info = paste0(
      "expanded rows whose reporter does not resolve for their year: ",
      paste(
        utils::head(
          unique(paste0(missing$Reporter, " ", missing$Year)),
          6
        ),
        collapse = "; "
      )
    )
  )

  # Every resolved code must be a live polity. Coverage says nothing about whether the
  # targets exist, and a typo in an alias would keep coverage at 100% while pointing
  # nowhere.
  p <- as.data.frame(whep::polities)
  live <- p$polity_code[!p$wiki_status %in% c("retired", "superseded")]
  testthat::expect_equal(
    length(setdiff(out$reporting_polity_code, live)),
    0L,
    info = paste0(
      "codes that are absent or dead: ",
      paste(
        utils::head(setdiff(out$reporting_polity_code, live), 6),
        collapse = ", "
      )
    )
  )

  # The reporter must change polity when its territory does. A single polity per
  # reporter across 150 years would mean the year scoping is being ignored, which is
  # the failure a coverage check alone cannot see.
  per_reporter <- tapply(
    out$reporting_polity_code,
    out$Reporter,
    function(v) length(unique(v))
  )
  testthat::expect_gt(max(per_reporter), 3L)
  testthat::expect_true(any(per_reporter == 1L))
})

testthat::test_that("expand_trade_sources tolerates input without a Reporter column", {
  # The exported example in the roxygen has no Reporter, so resolution must degrade to
  # NA rather than error. Checked because adding a column that assumes its neighbours
  # exist is how a documented example stops running.
  toy <- tibble::tibble(
    Name = c("a", "b"),
    Trade = c("t1", "t2"),
    Info_Format = c("year", "year"),
    Timeline_Start = c(1L, 1L),
    Timeline_End = c(3L, 4L),
    Timeline_Freq = c(1L, 1L),
    `Imp/Exp` = "Imp",
    SACO_link = NA
  )
  out <- expand_trade_sources(toy)
  testthat::expect_true("reporting_polity_code" %in% names(out))
  testthat::expect_true(all(is.na(out$reporting_polity_code)))
})
