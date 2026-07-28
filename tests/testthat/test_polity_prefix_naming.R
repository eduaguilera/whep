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

CODE_RE <- "^[A-Za-z0-9]+-[0-9]{4}-[0-9]{4}$"

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
      d$polity_prefix[grepl(CODE_RE, d$polity_prefix)]
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
