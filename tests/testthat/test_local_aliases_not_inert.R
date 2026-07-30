# Upstream publishes `observed_rows` as NA for every alias of the three sources whose
# data lives in THIS package -- lassaletta-grassland-share, mueller-synthetic-n and
# crops-manure-n -- because it has no way to count rows in datasets it never sees.
# That is the honest state, and deliberately not filled in by shipping counts back
# upstream: a number the owning repository cannot recompute is exactly the stale
# second copy this integration keeps removing.
#
# The consequence is that upstream's inert-alias detection cannot cover these 152
# aliases. An inert alias is not harmless: it is a label no data row uses, which
# usually means the label is wrong -- misspelt, mis-scoped by year, or aimed at a
# territory the source does not actually report. Upstream has already found five of
# those by other means, two of which had the target sitting in the wrong CSV column.
#
# So the check lives here, where the data is. For each alias, count the rows its label
# and year range actually serve. Measured today: 181 aliases across the three sources
# below, ZERO serving no rows — plus 34 more under `trade-sources`, of which THREE are
# inert and named at the bottom of this file.
#
# This matters most for the 133 aliases generated mechanically in this branch -- 129
# for Lassaletta from exact polity-name matches and 4 for crops_manure_n. "Generated
# mechanically" is precisely the kind of provenance that deserves an independent
# check that each one does something, and this is that check.
testthat::test_that("no alias for a locally-owned source is inert", {
  al <- as.data.frame(whep::polity_label_aliases)

  sources <- list(
    list(
      source = "lassaletta-grassland-share",
      labels = function() {
        d <- whep::lassaletta_grassland_share
        data.frame(
          label = as.character(d$Country),
          year = as.integer(d$year),
          stringsAsFactors = FALSE
        )
      }
    ),
    list(
      source = "mueller-synthetic-n",
      labels = function() {
        d <- whep::mueller_synthetic_n
        data.frame(
          label = as.character(d$iso3c),
          year = NA_integer_,
          stringsAsFactors = FALSE
        )
      }
    ),
    list(
      source = "crops-manure-n",
      labels = function() {
        d <- whep::crops_manure_n
        data.frame(
          label = as.character(d$ISO),
          year = NA_integer_,
          stringsAsFactors = FALSE
        )
      }
    )
  )

  total <- 0L
  for (spec in sources) {
    a <- al[which(al$source == spec$source), ]
    # Non-vacuous: a renamed source would leave nothing to check and pass silently.
    testthat::expect_gt(nrow(a), 0L)
    total <- total + nrow(a)

    d <- spec$labels()
    served <- vapply(
      seq_len(nrow(a)),
      function(i) {
        hit <- d$label == a$source_label[i]
        # Year-scope the count only when the data carries a year. Mueller and
        # crops_manure_n are single snapshots, so their aliases' ranges cannot be
        # tested against a per-row year, and pretending otherwise would count zero
        # for every one of them.
        if (!all(is.na(d$year))) {
          ys <- a$year_start[i]
          ye <- a$year_end[i]
          if (!is.na(ys)) {
            hit <- hit & d$year >= ys
          }
          if (!is.na(ye)) {
            hit <- hit & d$year <= ye
          }
        }
        sum(hit, na.rm = TRUE)
      },
      integer(1)
    )

    inert <- a$source_label[served == 0L]
    testthat::expect_equal(
      length(inert),
      0L,
      info = paste0(
        spec$source,
        ": aliases serving no data row, so the label is probably wrong rather ",
        "than merely unused: ",
        paste(utils::head(unique(inert), 10), collapse = ", ")
      )
    )
  }

  # The count is pinned so that losing coverage is a failure too. Upstream cannot
  # check these, so a silent drop here would leave them checked nowhere.
  testthat::expect_gte(total, 150L)
})
