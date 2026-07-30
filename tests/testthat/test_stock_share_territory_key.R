# Livestock split shares divide a parent item's stock between sub-items -- chickens into
# broilers and layers, cattle into dairy and non-dairy -- and they were computed as
# `value / sum(value)` grouped by `(year, area_code, Item_Code)`.
#
# `area_code` there is polity_area_code, and I broke the assumption that it identifies one
# territory. Earlier in this branch I unfolded four areas that carry their own commodity
# balances -- New Caledonia, North Macedonia, Eswatini and Syria -- so each reports its own
# polity. Three of them took their own polity_area_code (153, 154, 212); ESWATINI KEPT
# FABIO's 999, and that one is enough. The 999 group then held two territories, and
# `sum(value)` spanned them:
#
#   Eswatini       1,053,000 broilers   share 0.9419   <- denominator included ROW
#   Rest of World      3,000 broilers   share 0.0027   <- denominator included Eswatini
#
# Two different shares for one (year, area_code, Item_Code, item_cbs_code), so which one a
# downstream inner_join used was a matter of row order. After keying by the reporting
# territory the same two rows read 0.9461 and 0.600 -- each its own broilers over its own
# chickens. Rest of World's share is corrected by a factor of 222.
#
# HOW LONG IT HID, which is the part worth recording: the only symptom was
# "Duplicate year values found within groups. 24 group/time combination(s)" from
# fill_linear, and I wrote it off as pre-existing data noise TWICE while smoke-testing
# other things. Nothing else complained. The build completed, the row count was right, and
# every polity column was populated.
#
# The invariant below is the one that could not have been satisfied before: shares within a
# territory-item group must sum to 1. Under the old key they summed to 1 across a group
# that spanned territories, which is a different and meaningless statement.
testthat::test_that("stock shares are keyed by reporting territory and sum to one", {
  testthat::skip_on_ci()
  sh <- tryCatch(
    whep:::.compute_stock_shares(1990:1991),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(sh), "livestock stocks pin unavailable")
  sh <- as.data.frame(sh)

  # `area` is half the key. Its absence is what allowed one polity_area_code to stand for
  # several territories.
  testthat::expect_true(
    all(
      c("year", "area_code", "area", "Item_Code", "item_cbs_code", "share") %in%
        names(sh)
    )
  )
  testthat::expect_gt(nrow(sh), 100L)

  key <- paste(
    sh$year,
    sh$area_code,
    sh$area,
    sh$Item_Code,
    sh$item_cbs_code
  )
  testthat::expect_equal(
    sum(duplicated(key)),
    0L,
    info = paste0(
      "duplicate share keys, so a downstream join picks one by row order: ",
      paste(utils::head(key[duplicated(key)], 5), collapse = "; ")
    )
  )

  # The real invariant. A share group is one territory's one parent item, so its parts
  # must be exactly the whole.
  grp <- paste(sh$year, sh$area_code, sh$area, sh$Item_Code)
  sums <- tapply(sh$share, grp, sum)
  testthat::expect_gt(length(sums), 50L)
  testthat::expect_equal(
    sum(abs(sums - 1) >= 1e-9),
    0L,
    info = paste0(
      "share groups not summing to 1: ",
      paste(utils::head(names(sums)[abs(sums - 1) >= 1e-9], 5), collapse = "; ")
    )
  )
})

testthat::test_that("no reporting territory shares the FABIO rest-of-world bucket", {
  # THE PREMISE OF THE FIX ABOVE HAS BEEN REMOVED, and this test is what noticed.
  #
  # It previously asserted that Eswatini shared polity_area_code 999 with rest-of-world,
  # which was true and was the cause of the cross-territory denominator. The root of that
  # was a label match: area 209 is named "Eswatini" in regions_full while its 180,663
  # observed rows sit on the alias "Swaziland", so the derivation that decides which
  # folded areas keep their own code could not see its data. Naming the exception fixed
  # it, Eswatini now aggregates to 209, and the bucket holds only rest-of-world.
  #
  # So the `area` half of the share key is now defence rather than a repair: nothing
  # shares a bucket today, and if something does again, the shares stay correct. Both
  # halves are kept deliberately -- the invariant above would pass either way, and this
  # assertion is what distinguishes "no longer broken" from "no longer possible".
  cw <- as.data.frame(whep::polity_area_crosswalk)
  in_bucket <- cw[which(cw$polity_area_code == 999L), ]
  testthat::expect_gt(nrow(in_bucket), 40L)

  own_polity <- sort(unique(in_bucket$area_code[which(
    !is.na(in_bucket$polity_code) &
      !startsWith(in_bucket$polity_code, "ROW-")
  )]))
  testthat::expect_equal(
    length(own_polity),
    0L,
    info = paste0(
      "areas reporting their own polity while aggregating into rest-of-world, so ",
      "their shares share a denominator with it: ",
      paste(own_polity, collapse = ", ")
    )
  )

  # All four areas unfolded in this branch must have their own aggregation key. Eswatini
  # was the one that silently did not, for three commits.
  for (code in c(153L, 154L, 209L, 212L)) {
    rows <- cw[which(cw$area_code == code), ]
    testthat::expect_true(
      nrow(rows) > 0L,
      info = paste0("area ", code, " is absent from the crosswalk")
    )
    testthat::expect_true(
      all(rows$polity_area_code == code),
      info = paste0(
        "area ",
        code,
        " aggregates into a shared bucket, so its stock shares share a denominator ",
        "with another territory"
      )
    )
  }

  # The two tables carrying this column must agree, and now do EVERYWHERE. This
  # assertion previously exempted 351 China, whose crosswalk row held an aggregation key
  # while its polity_code was NA -- an inert trap, since consumers drop unmapped rows
  # before aggregating, but one that would have built a China bucket double-counting
  # mainland, Hong Kong, Macao and Taiwan for anyone who aggregated first. Giving an
  # unmapped area no key closed both the trap and the disagreement.
  #
  # test_polities_cats_vs_regions_full.R asserts the same property directly; this keeps it
  # next to the build-order hazard that produced a stale value here once already.
  testthat::expect_equal(
    length(differing_aggregation_keys()),
    0L,
    info = paste0(
      "areas whose polity_area_code differs between regions_full and the crosswalk: ",
      paste(utils::head(differing_aggregation_keys(), 10), collapse = ", ")
    )
  )
})

# Shared by the assertion above and by test_polities_cats_vs_regions_full.R's version, so
# the two cannot drift into checking subtly different things.
differing_aggregation_keys <- function() {
  r <- as.data.frame(whep::regions_full)
  r <- r[which(!is.na(r$code)), ]
  r <- r[!duplicated(r$code), ]
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[which(!is.na(cw$area_code)), ]
  cw <- cw[!duplicated(cw$area_code), ]
  idx <- match(r$code, cw$area_code)
  from_cw <- cw$polity_area_code[idx]
  ok <- !is.na(idx)
  a <- r$polity_area_code[ok]
  b <- from_cw[ok]
  r$code[ok][which((is.na(a) != is.na(b)) | (!is.na(a) & !is.na(b) & a != b))]
}

# The derivation deciding which folded areas keep their own aggregation key has TWO
# routes, unioned, and the second is gated on `cbs`. Both halves of that structure are
# load-bearing in ways a future simplification would break, so both are asserted.
#
#   route 1  the area's own label carries observed data
#   route 2  the area reports its own commodity balances AND its polity has data under
#            ANY of its labels
#
# Route 2 exists because `observed_rows` is counted per LABEL, so a renamed country files
# its count on the name the area is not called -- area 209 is "Eswatini" while its 180,663
# rows sit on "Swaziland". It replaced a hardcoded exception list containing exactly that
# one name.
#
# The `cbs` gate on route 2 is what keeps it from over-reaching. Ungated it adds six areas
# that must NOT unfold: 351 China, deliberately unmapped against double-counting its own
# components, and 42, 65, 161, 190, 239, which the folded_into_aggregate baseline keeps
# folded because they carry no commodity balances. All six are cbs = FALSE.
#
# And the gate cannot simply REPLACE route 1: the twelve territories unfolded earlier in
# this branch are cbs = FALSE, so a cbs-only rule would re-fold every one of them.
testthat::test_that("the cbs gate on the polity route is load-bearing", {
  al <- as.data.frame(whep::polity_label_aliases)
  r <- as.data.frame(whep::regions_full)

  live <- al[which(!startsWith(al$polity_code, "ROW-")), ]
  with_data <- which(!is.na(live$observed_rows) & live$observed_rows > 0)
  obs_polities <- unique(live$polity_code[with_data])
  labels_direct <- unique(live$source_label[with_data])
  labels_via_polity <- unique(
    live$source_label[live$polity_code %in% obs_polities]
  )
  testthat::expect_gt(length(labels_via_polity), length(labels_direct))

  matches <- function(labels, require_cbs) {
    keep <- (r$FAOSTAT_name %in% labels | r$name %in% labels) & !is.na(r$code)
    if (require_cbs) {
      keep <- keep & r$cbs
    }
    sort(unique(as.integer(r$code[which(keep)])))
  }

  direct <- matches(labels_direct, FALSE)
  gated <- union(direct, matches(labels_via_polity, TRUE))
  ungated <- union(direct, matches(labels_via_polity, FALSE))

  # Ungated, the polity route reaches areas that must stay folded. Named, because the
  # point is WHICH ones and why.
  over_reach <- setdiff(ungated, gated)
  testthat::expect_setequal(over_reach, c(42L, 65L, 161L, 190L, 239L, 351L))
  testthat::expect_true(all(!r$cbs[match(over_reach, r$code)]))

  # And a cbs-only rule would lose the territories unfolded earlier, so the union is
  # necessary rather than belt-and-braces.
  cbs_only <- matches(labels_via_polity, TRUE)
  testthat::expect_gt(length(setdiff(direct, cbs_only)), 5L)

  # The gated rule is what the built data reflects: Eswatini reaches it, China does not.
  testthat::expect_true(209L %in% gated)
  testthat::expect_false(351L %in% gated)
  cw <- as.data.frame(whep::polity_area_crosswalk)
  testthat::expect_equal(
    unique(cw$polity_area_code[which(cw$area_code == 209L)]),
    209L
  )
})
