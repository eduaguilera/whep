# `regions_full` carries 17 regional classification columns (region_krausmann,
# region_UN, region_IPCC, region_ILO1-3, ...). They are a parallel geography: a
# grouping of countries that exists alongside the polity structure rather than
# being derived from it.
#
# That is legitimate — these groupings belong to the sources and models that use
# them, and redefining region_krausmann in terms of polity continents would
# change which coefficient gets applied. What is NOT legitimate is a grouping
# that contradicts itself across a polity: when several FAOSTAT reporting areas
# collapse to one polity, they must agree on that polity's region, or the value
# a consumer sees depends on which row it happened to read.
#
# Aggregate polities are exempt by construction — ROW-1850-2023 folds in ~60
# territories across every continent, so it genuinely has no single region.

# DISCOVERED, not listed. The enumerated version had two defects of exactly the kind
# a list acquires: it named `continent`, which is not a column of this table at all, so
# that entry silently checked nothing; and it missed `iea`, a real classification column
# that therefore went unchecked. Matching on the column names present means a
# classification added tomorrow is covered without anyone remembering.
classification_cols <- function() {
  nm <- names(as.data.frame(whep::regions_full))
  grep(
    "region|continent|EU27|ADB|krausmann|HANPP|ILO|IEA|IPCC|labour",
    nm,
    ignore.case = TRUE,
    value = TRUE
  )
}

aggregate_polities <- function() {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  unique(cw$polity_code[!is.na(cw$polity_type) & cw$polity_type == "aggregate"])
}

test_that("region classifications do not contradict themselves within a polity", {
  rf <- as.data.frame(whep::regions_full)
  rf <- rf[!is.na(rf$reporting_polity_code), ]
  aggs <- aggregate_polities()
  rf <- rf[!rf$reporting_polity_code %in% aggs, ]

  for (col in intersect(classification_cols(), names(rf))) {
    n_distinct <- tapply(
      rf[[col]],
      rf$reporting_polity_code,
      function(v) length(unique(v[!is.na(v)]))
    )
    conflicting <- names(n_distinct)[n_distinct > 1]
    expect_equal(
      length(conflicting),
      0L,
      info = paste0(
        col,
        " takes several values within one polity: ",
        paste(utils::head(conflicting, 5), collapse = ", "),
        " — a consumer's answer would depend on which reporting area it read."
      )
    )
  }
})

test_that("the consumed classifications cover every polity-resolved row", {
  # Only three of the 17 columns have any consumer in R/ (region_krausmann in 18
  # places, region_HANPP in 4, ADB_Region in 2 — see whep#386 for the other 14).
  # The two used for weighting must be complete, because a missing region
  # silently drops a polity from whatever it aggregates.
  rf <- as.data.frame(whep::regions_full)
  rf <- rf[!is.na(rf$reporting_polity_code), ]

  for (col in c("region_krausmann", "region_HANPP")) {
    missing <- unique(rf$reporting_polity_code[is.na(rf[[col]])])
    expect_equal(
      length(missing),
      0L,
      info = paste0(
        col,
        " is missing for: ",
        paste(utils::head(missing, 5), collapse = ", ")
      )
    )
  }

  # ADB_Region is deliberately sparse — it is an Asian Development Bank
  # grouping, so most polities have none. Asserted as sparse rather than
  # complete so that "mostly NA" is a documented property, not a suspicion.
  expect_true(sum(is.na(rf$ADB_Region)) > 0)
})
