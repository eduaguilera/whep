# test_polities_cats_vs_regions_full.R — polities_cats is a view of regions_full

# -- the two tables must not disagree anywhere (issues #406, #395) -------------

# polities_cats and regions_full ship the same 38 columns, and all 198 of
# polities_cats' area codes are among regions_full's 272: it is a row-filtered
# view of the same table. Both used to be read from their own vendored CSV, each
# carrying its own copy of every column, so the copies drifted. Measured on
# `main` before this test existed: over the 198 shared codes, 17 of 39 columns
# disagreed — `legacy_polity_prefix` (then named `polity_code`), `polity_name`,
# `cbs`, `fabio_code` and thirteen label columns. 95 of the disagreeing cells
# were an encoding artefact, the literal string "0" in `eia` (5), `iea` (59) and
# eleven `region_*` columns (31) where regions_full leaves NA; "0" is not the
# name of a region in any taxonomy. The other 8 were a Bhutan/Comoros
# rest-of-world fold, kept as a deliberate override until #395 settled it.
#
# #395 settled it against the fold, so there is no override left and the two
# tables must now agree cell for cell. Three measurements decided it:
#   * `cbs = FALSE` was the fold's stated reason and is refuted by the pin WHEP
#     reads: `faostat-cbs-new` carries 175 rows for Bhutan (2019-2023, 12 items)
#     and 237 for Comoros (2010-2023, 10 items).
#   * the fold was never coherently applied. The four areas polities_cats really
#     does fold — Andorra, Anguilla, Saint Pierre and Miquelon, American Samoa —
#     carry `polity_area_code` 999, while the Bhutan and Comoros rows kept
#     `polity_area_code` 18 and 45 and `reporting_polity_code`
#     `BTN-1800-2025` / `COM-1975-2025`. The override moved three legacy label
#     columns and left every consumed identity column saying the opposite.
#   * no package code reads polities_cats at all, so the disputed cells reached
#     no computation either way.
#
# polities_cats is built in data-raw/harmonization_tables.R by filtering
# regions_full to the vendored membership, so a repair or an override written on
# regions_full can no longer miss its copy.
#
# Tested on the shipped datasets rather than on the builder, because the builder
# lives in data-raw/ and is not loadable from tests. The two .rda files are what
# consumers actually get, and they are what drifted.
test_that("polities_cats agrees with regions_full in every shared cell", {
  cats <- whep::polities_cats
  regions <- whep::regions_full

  # The subset relation the derivation rests on. The build aborts if a
  # membership code is absent from regions_full, so this is belt and braces.
  expect_true(all(cats$code %in% regions$code))
  expect_named(cats, names(regions))

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
      disagreeing <- c(
        disagreeing,
        paste0(
          col,
          " (",
          paste(cats$code[differs], collapse = ", "),
          ")"
        )
      )
    }
  }

  # Non-vacuous: every cell of the subset was compared, not a sample of them.
  expect_equal(cells, 198L * ncol(cats))
  expect_equal(disagreeing, character(0))
})

# The regression guard for #395 itself. The check above would also catch the
# fold coming back, but only as an anonymous "some column disagrees"; this names
# the two areas and the four columns the override used to rewrite, so a
# reappearance is diagnosed rather than merely detected.
test_that("Bhutan and Comoros are modelled individually in both tables", {
  for (name in c("polities_cats", "regions_full")) {
    table <- get(name, envir = asNamespace("whep"))
    rows <- table[table$code %in% c(18L, 45L), ]
    rows <- rows[order(rows$code), ]

    expect_equal(rows$code, c(18L, 45L), info = name)
    expect_equal(rows$legacy_polity_prefix, c("BTN", "COM"), info = name)
    expect_equal(rows$polity_name, c("Bhutan", "Comoros"), info = name)
    expect_equal(rows$fabio_code, c(18, 45), info = name)
    expect_true(all(rows$cbs), info = name)
    # The identity columns that always said this, even under the old override.
    expect_equal(rows$polity_area_code, c(18L, 45L), info = name)
    expect_equal(
      rows$reporting_polity_code,
      c("BTN-1800-2025", "COM-1975-2025"),
      info = name
    )
  }
})

# The areas that ARE folded, for contrast: the check above must not be read as
# "nothing is ever folded". Four rest-of-region members carry an `R***` stem and
# `polity_area_code` 999 in both tables, and that is what a coherent fold looks
# like — every column agreeing that the area is inside a bucket.
test_that("the genuinely folded areas keep their rest-of-region bucket", {
  folded <- c(5L, 6L, 190L, 258L) # American Samoa, Andorra, SPM, Anguilla
  for (name in c("polities_cats", "regions_full")) {
    table <- get(name, envir = asNamespace("whep"))
    rows <- table[table$code %in% folded, ]
    rows <- rows[order(rows$code), ]

    expect_equal(rows$code, folded, info = name)
    expect_equal(
      rows$legacy_polity_prefix,
      c("ROCE", "REUR", "RNAM", "RLAM"),
      info = name
    )
    expect_equal(rows$fabio_code, rep(999, 4), info = name)
    expect_equal(rows$polity_area_code, rep(999L, 4), info = name)
    expect_false(any(rows$cbs), info = name)
  }
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
