# `regions_full` and `polities_cats` used to ship a column called `polity_code`
# that held bare ISO3-shaped family prefixes ("AFG", "ROW") — 271 of them, and
# not one value that resolved against `polities`, whose codes are periodized
# ("AFG-1919-2025"). Any consumer doing the obvious join got zero rows, or
# worse, quietly filtered its data to nothing.
#
# The column is now `polity_prefix`, and the real code lives in
# `reporting_polity_code`. These tests keep the two from being confused again:
# a prefix column must never contain codes, and a code column must never
# contain prefixes.

# Anchored on the trailing year pair because a prefix may contain hyphens of its
# own (AZE-SSR-1920-1991, IDN-JVM-1949-1951 are real codes).
code_re <- "^.+-[0-9]{4}-[0-9]{4}$"

for (nm in c("regions_full", "polities_cats")) {
  test_that(paste(nm, "names its prefix column honestly"), {
    d <- get(nm, envir = asNamespace("whep"))

    expect_false(
      "polity_code" %in% names(d),
      info = paste0(
        nm,
        " has a `polity_code` column again. If it holds prefixes, call it ",
        "`polity_prefix`; if it holds real codes, it must resolve against ",
        "whep::polities."
      )
    )
    expect_true("polity_prefix" %in% names(d))

    # A prefix is never a code.
    offending <- unique(stats::na.omit(
      d$polity_prefix[grepl(code_re, d$polity_prefix)]
    ))
    expect_equal(
      length(offending),
      0L,
      info = paste0(
        nm,
        "$polity_prefix contains periodized codes: ",
        paste(utils::head(offending, 5), collapse = ", ")
      )
    )
  })

  test_that(paste(nm, "prefixes name a family that actually exists"), {
    # "Not a code" was necessary but not sufficient. Three values passed that
    # check while naming no polity whatsoever: CSK, SUN and YUG, the legacy
    # ISO3-shaped keys for Czechoslovakia, the Soviet Union and Yugoslavia.
    # Upstream files those chains under F51, F228 and F248, so grouping by
    # `polity_prefix` to collect every period of Czechoslovakia matched nothing,
    # and the key could not be joined to upstream at all. The rows' own
    # `reporting_polity_code` was correct the whole time, which is why the
    # existing checks stayed green — only the family key dangled.
    #
    # data-raw/harmonization_tables.R now repairs a dangling prefix from the
    # resolved reporting code. This asserts the repair, and catches the next
    # legacy key to arrive in a vendored table.
    d <- get(nm, envir = asNamespace("whep"))
    prefixes <- unique(stats::na.omit(d$polity_prefix))
    testthat::skip_if(length(prefixes) == 0)

    # Whitelist derived from the embedded crosswalk rather than the upstream CSV,
    # so this runs on CI. Sound: every value in it comes from a real polity_code,
    # so nothing invalid can be admitted. It covers a subset of all upstream
    # prefixes, which only makes the check conservative — never a false alarm.
    known <- unique(sub(
      "-.*",
      "",
      stats::na.omit(whep::polity_area_crosswalk$polity_code)
    ))
    dangling <- setdiff(prefixes, known)
    expect_equal(
      length(dangling),
      0L,
      info = paste0(
        nm,
        "$polity_prefix contains keys that name no polity: ",
        paste(utils::head(sort(dangling), 5), collapse = ", "),
        " — the prefix must be a real polity family, e.g. F51 not CSK."
      )
    )

    # A prefix may legitimately differ from its row's own reporting code, since
    # polities_cats folds some countries into regional aggregates (Bhutan under
    # RASI, Comoros under RAFR). Assert the fold is intact so the repair above
    # cannot quietly widen into "derive every prefix from the reporting code",
    # which would erase those choices.
    if (nm == "polities_cats") {
      folded <- d[!is.na(d$code) & d$code %in% c(18L, 45L), ]
      expect_setequal(folded$polity_prefix, c("RASI", "RAFR"))
    }
  })

  test_that(paste(nm, "resolves reporting_polity_code against polities"), {
    d <- get(nm, envir = asNamespace("whep"))
    codes <- stats::na.omit(d$reporting_polity_code)
    testthat::skip_if(length(codes) == 0)

    # This is the column consumers are told to join on, so every non-missing
    # value must actually exist upstream. A stale data/*.rda is the usual way
    # this breaks: it kept pointing at polities that had been split away.
    unknown <- setdiff(unique(codes), whep::polities$polity_code)
    expect_equal(
      length(unknown),
      0L,
      info = paste0(
        nm,
        "$reporting_polity_code has values absent from whep::polities: ",
        paste(utils::head(unknown, 5), collapse = ", "),
        " — re-run data-raw/harmonization_tables.R and commit data/."
      )
    )
  })
}
