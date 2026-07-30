# `regions_full` and `polities_cats` carry the SAME 40 columns, and `polities_cats` is a
# 198-row subset of the 272-row `regions_full` by area code — every code in the smaller table
# is in the larger one. Two tables of one schema describing one set of areas is a standing
# invitation to disagree, and comparing them found that they did, on 17 of their 39 shared
# columns.
#
# Thirteen of those seventeen were not disagreements at all but an ENCODING difference.
# `polities_cats.csv` was exported with a literal "0" wherever a value is absent — in `eia`,
# `iea` and every `region_*` classification — while `regions_full.csv` leaves those blank, so
# it reads as NA. "0" reads as data: `!is.na(iea)` kept all 198 rows rather than the 139 with
# an IEA name, a join on `iea` would match 59 rows to each other as one country, and grouping
# by `region_UN` produced a "0" region. `blank_zero_sentinels()` now clears it in character
# columns only, since a numeric 0 is a real value in `EU27` and `cbs`.
#
# FOUR disagreements remain and they are deliberate. `polities_cats` files Bhutan under RASI
# and Comoros under RAFR, folding them into rest-of-Asia and rest-of-Africa, where
# `regions_full` keeps BTN and COM as their own reporting entities. RASI and RAFR are real
# upstream prefixes, and harmonization_tables.R says so explicitly — deriving every prefix
# from the reporting code "would silently undo those modelling choices". So the four are
# pinned by name and by row count, which is what separates a modelling choice from a drift.

test_that("no character column in either table uses a zero sentinel", {
  for (nm in c("regions_full", "polities_cats")) {
    tbl <- as.data.frame(get(nm, envir = asNamespace("whep")))
    chr <- names(tbl)[vapply(tbl, is.character, logical(1))]
    # Non-vacuous: both tables are mostly character, so an empty set here would mean the
    # sweep found nothing to check rather than nothing wrong.
    expect_gt(length(chr), 10L)
    zeros <- vapply(
      chr,
      function(cl) sum(tbl[[cl]] == "0", na.rm = TRUE),
      numeric(1)
    )
    expect_equal(unname(zeros[zeros > 0]), numeric(0))
  }
})

test_that("the two tables disagree only where a modelling choice says they should", {
  rf <- as.data.frame(whep::regions_full)
  pc <- as.data.frame(whep::polities_cats)
  shared <- intersect(pc$code, rf$code)
  expect_gt(length(shared), 150L)
  # Every polities_cats code is a regions_full code; the subset relation is the premise of
  # this comparison, so assert it rather than assume it.
  expect_equal(setdiff(pc$code, rf$code), integer(0))

  differing <- character(0)
  counts <- integer(0)
  for (cl in setdiff(names(pc), "code")) {
    a <- rf[[cl]][match(shared, rf$code)]
    b <- pc[[cl]][match(shared, pc$code)]
    both_na <- is.na(a) & is.na(b)
    one_na <- is.na(a) != is.na(b)
    unequal <- !is.na(a) & !is.na(b) & as.character(a) != as.character(b)
    n <- sum(!both_na & (one_na | unequal))
    if (n > 0L) {
      differing <- c(differing, cl)
      counts <- c(counts, n)
    }
  }

  # Bhutan and Comoros, folded in one table and not the other. Four columns carry that one
  # decision: the prefix, the name it implies, the CBS flag and the FABIO code.
  expect_setequal(
    differing,
    c("polity_prefix", "polity_name", "cbs", "fabio_code")
  )
  # Two rows each, not more: a third folded area would be a new modelling choice, and it
  # should be made deliberately rather than arrive inside a column that already differs.
  expect_equal(unname(counts), rep(2L, length(differing)))
})
