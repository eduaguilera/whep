# Mueller et al.'s synthetic-N rate table is keyed by a column called `iso3c` that is not
# entirely ISO 3166-1: ten of its 156 codes are FAO-style legacy three-letter codes
# (BZE, COS, ELS, GUA, HAI, HON, ROM, TRI, ZAR) plus one that is neither (SRM). Upstream
# aliases them under source "mueller-synthetic-n", so they resolve — the point of this
# test is the property that makes those aliases correct.
#
# Each row of that file is one country-crop rate, so the codes name distinct entities: the
# 156 codes must reach 156 DISTINCT polities. A collision means one polity gets two sets of
# rates while another gets none, silently.
#
# That is exactly what was wrong. SRM was aliased to SUR-1975-2025 on the reading that it
# was a legacy code for Suriname — but SUR appears in the same file as its own code and
# resolves to that polity directly, so Suriname held two rate sets and SRM's real entity
# held none. Identified by elimination rather than resemblance: every ex-Yugoslav republic
# is present (HRV, BIH, MKD, SVN) while SRB, SCG and MNE are all absent, so SRM is Serbia
# and Montenegro — the federal state that existed while these rates were compiled. Now
# aliased to SCG-1992-2006.

testthat::test_that("every Mueller country code resolves to a distinct polity", {
  d <- readr::read_csv(
    system.file("extdata", "mueller_synthetic_n.csv", package = "whep"),
    show_col_types = FALSE
  )
  codes <- sort(unique(stats::na.omit(d$iso3c)))
  # Non-vacuous: an empty or renamed column would make the rest pass for free.
  testthat::expect_gt(length(codes), 150L)

  # Resolve each code the way a consumer would: by alias under this source, falling back
  # to the code being a real ISO3 that the crosswalk already knows.
  aliased <- resolve_polity_label(
    codes,
    source = "mueller-synthetic-n",
    year = 2000L
  )
  cw <- as.data.frame(whep::polity_area_crosswalk)
  iso_to_polity <- cw[
    which(!is.na(cw$iso3_code) & nzchar(cw$iso3_code) & !is.na(cw$polity_code)),
    c("iso3_code", "polity_code", "polity_start_year", "polity_end_year")
  ]
  iso_to_polity <- iso_to_polity[
    which(
      iso_to_polity$polity_start_year <= 2000L &
        iso_to_polity$polity_end_year > 2000L
    ),
  ]
  direct <- iso_to_polity$polity_code[match(codes, iso_to_polity$iso3_code)]
  resolved <- ifelse(is.na(aliased), direct, aliased)

  unresolved <- codes[is.na(resolved)]
  testthat::expect_equal(
    length(unresolved),
    0L,
    info = paste0(
      "Mueller codes reaching no polity: ",
      paste(utils::head(unresolved, 10), collapse = ", ")
    )
  )

  # The property that matters: distinct codes, distinct polities.
  dup <- table(resolved[!is.na(resolved)])
  collisions <- names(dup)[dup > 1]
  testthat::expect_equal(
    length(collisions),
    0L,
    info = paste0(
      "polities reached by more than one Mueller country code, so one entity holds ",
      "several rate sets and another holds none: ",
      paste(
        vapply(
          utils::head(collisions, 5),
          function(p) {
            paste0(p, " <- ", paste(codes[resolved == p], collapse = "+"))
          },
          character(1)
        ),
        collapse = "; "
      )
    )
  )

  # Pin the correction so it cannot silently revert.
  testthat::expect_equal(
    resolve_polity_label("SRM", source = "mueller-synthetic-n", year = 2000L),
    "SCG-1992-2006"
  )
})
