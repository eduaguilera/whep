# test_polities_cats_vs_regions_full.R — polities_cats is a view of regions_full

# -- the two tables may differ only where a fold says so (issue #406) ----------

# polities_cats and regions_full ship the same 38 columns, and all 198 of
# polities_cats' area codes are among regions_full's 272: it is a row-filtered
# view of the same table. Both used to be read from their own vendored CSV, each
# carrying its own copy of every column, so the copies drifted. Measured on
# `main` before this test existed: over the 198 shared codes, 17 of 39 columns
# disagreed — `legacy_polity_prefix` (then named `polity_code`), `polity_name`,
# `cbs`, `fabio_code` and thirteen label columns. 95 of the disagreeing cells were an encoding artefact, the
# literal string "0" in `eia` (5), `iea` (59) and eleven `region_*` columns (31)
# where regions_full leaves NA; "0" is not the name of a region in any taxonomy.
# The other 8 were the deliberate Bhutan/Comoros fold pinned below.
#
# polities_cats is now built in data-raw/harmonization_tables.R by filtering
# regions_full to the vendored membership and applying that fold explicitly, so
# a repair or an override written on regions_full can no longer miss its copy,
# and the only differences left are the ones someone wrote on purpose.
#
# Tested on the shipped datasets rather than on the builder, because the builder
# lives in data-raw/ and is not loadable from tests. The two .rda files are what
# consumers actually get, and they are what drifted.
test_that("polities_cats matches regions_full outside the documented fold", {
  cats <- whep::polities_cats
  regions <- whep::regions_full

  # The subset relation the derivation rests on. The build aborts if a
  # membership code is absent from regions_full, so this is belt and braces.
  expect_true(all(cats$code %in% regions$code))
  expect_named(cats, names(regions))

  folded_codes <- c(18L, 45L) # Bhutan, Comoros
  fold_columns <- c(
    "legacy_polity_prefix",
    "polity_name",
    "cbs",
    "fabio_code"
  )

  same_codes <- regions[match(cats$code, regions$code), ]
  disagreeing <- character(0)
  cells <- 0L
  for (col in names(cats)) {
    left <- as.character(same_codes[[col]])
    right <- as.character(cats[[col]])
    differs <- !((is.na(left) & is.na(right)) |
      (!is.na(left) & !is.na(right) & left == right))
    cells <- cells + length(differs)
    if (any(differs)) {
      disagreeing <- c(disagreeing, col)
      # A column may only disagree for the two folded areas, never for a third.
      expect_setequal(cats$code[differs], folded_codes)
    }
  }

  # Non-vacuous: every cell of the subset was compared, not a sample of them.
  expect_equal(cells, 198L * ncol(cats))
  expect_equal(sort(disagreeing), sort(fold_columns))
})

# The fold is the one difference that is a modelling choice rather than drift:
# polities_cats files Bhutan under rest-of-Asia and Comoros under
# rest-of-Africa, because neither country had a commodity balance sheet in the
# CBS vintage the table was compiled against. Whether it still holds is issue
# #395 — the faostat-cbs-new pin carries 175 rows for Bhutan and 237 for
# Comoros on the 2026-06-15 CB release, up from 91 and 135 on the previous one
# — so the values are pinned here rather than harmonised, to make revisiting
# them an explicit edit instead of a silent one.
test_that("polities_cats folds exactly Bhutan and Comoros into rest-of-world", {
  cats <- whep::polities_cats
  fold <- cats[order(cats$code), ]
  fold <- fold[fold$code %in% c(18L, 45L), ]

  expect_equal(fold$code, c(18L, 45L))
  expect_equal(fold$legacy_polity_prefix, c("RASI", "RAFR"))
  expect_equal(fold$polity_name, c("Asia Other", "Africa Other"))
  expect_equal(fold$cbs, c(FALSE, FALSE))
  expect_equal(fold$fabio_code, c(999, 999))

  # regions_full models both individually. That asymmetry is the whole content
  # of #395, so it must stay visible instead of being quietly reconciled.
  regions <- whep::regions_full
  modelled <- regions[order(regions$code), ]
  modelled <- modelled[modelled$code %in% c(18L, 45L), ]
  expect_equal(modelled$legacy_polity_prefix, c("BTN", "COM"))
  expect_equal(modelled$fabio_code, c(18, 45))
  expect_true(all(modelled$cbs))
})

# The 95 cells that drifted were all the same defect: an Excel export wrote the
# number zero into label columns that mean "this source does not report the
# area", and regions_full writes NA for the same thing. Asserted directly as
# well as through the agreement check above, because a "0" reappearing in
# regions_full would satisfy agreement while still being wrong.
test_that("no area label in either table is the literal zero", {
  offenders <- character(0)
  checked <- 0L
  for (name in c("polities_cats", "regions_full")) {
    table <- get(name, envir = asNamespace("whep"))
    for (col in names(table)[vapply(table, is.character, logical(1))]) {
      checked <- checked + 1L
      zeros <- sum(!is.na(table[[col]]) & table[[col]] == "0")
      if (zeros > 0L) {
        offenders <- c(offenders, paste0(name, "$", col, " (", zeros, ")"))
      }
    }
  }
  # Non-vacuous: zero character columns would make the loop prove nothing.
  expect_gt(checked, 40L)
  expect_equal(offenders, character(0))
})
