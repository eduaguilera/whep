# Every FAOSTAT reporting area the package knows about must resolve to a polity.
# An area with no polity is not inert: `add_polity_code()` yields NA for it and
# the pipelines filter those rows out, so an unmapped area means data silently
# leaving the build. That is precisely how the two grassland sources ended up
# keyed on different area bases before.
#
# One area is deliberately unmapped, and only one.

# FAOSTAT 351 "China" is an aggregate of 41 (mainland) + 96 (Hong Kong) +
# 128 (Macao) + 214 (Taiwan), reported ALONGSIDE its components for every year
# from 1961 to 2024 — a full overlap, unlike the dissolved-state aggregates
# (Czechoslovakia, USSR, Yugoslav SFR) whose reporting stops when their
# successors begin. Its components already map to CHN/HKG/MAC/TWN, so mapping
# 351 as well double-counted China across every FAOSTAT domain. It is left
# unmapped so it is dropped as a statistical aggregate.
DELIBERATELY_UNMAPPED <- c(351L)

test_that("every FAOSTAT area resolves to a polity, bar the documented aggregate", {
  lk <- as.data.frame(whep:::.current_area_lookup(include_unmapped = TRUE))
  unmapped <- lk[is.na(lk$polity_code), ]

  unexpected <- setdiff(as.integer(unmapped$area_code), DELIBERATELY_UNMAPPED)
  expect_equal(
    length(unexpected),
    0L,
    info = paste0(
      "FAOSTAT areas resolving to no polity, so their rows are dropped from ",
      "every build: ",
      paste(
        utils::head(
          paste0(
            unmapped$area_code[as.integer(unmapped$area_code) %in% unexpected],
            " (",
            unmapped$area_name[as.integer(unmapped$area_code) %in% unexpected],
            ")"
          ),
          10
        ),
        collapse = ", "
      ),
      ". Either map it in whep-polities, or add it to DELIBERATELY_UNMAPPED ",
      "with the reason."
    )
  )

  # The reverse: an entry in the allowlist that HAS become mapped is stale, and
  # keeping it invites someone to unmap it again on this test's authority.
  still_unmapped <- intersect(
    DELIBERATELY_UNMAPPED,
    as.integer(unmapped$area_code)
  )
  expect_setequal(still_unmapped, DELIBERATELY_UNMAPPED)
})

test_that("the China aggregate stays unmapped while its components resolve", {
  # Stated as a positive assertion rather than left implicit in the allowlist:
  # the reason 351 may be dropped is that nothing is lost by dropping it.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  components <- c(41L, 96L, 128L, 214L)

  for (code in components) {
    resolved <- cw$polity_code[
      !is.na(cw$area_code) & as.integer(cw$area_code) == code
    ]
    expect_true(
      length(stats::na.omit(resolved)) > 0,
      info = paste0(
        "FAOSTAT area ",
        code,
        " is a China component and must map to its own ",
        "polity, otherwise dropping the 351 aggregate loses it entirely."
      )
    )
  }

  # 351 is PRESENT in the crosswalk but with a NULL polity_code, rather than
  # omitted. That is the deliberate shape: the area is acknowledged and its
  # non-resolution recorded, so a consumer joining on area_code sees an explicit
  # unmapped row instead of a missing key it might read as an oversight.
  row_351 <- cw[!is.na(cw$area_code) & as.integer(cw$area_code) == 351L, ]
  expect_equal(nrow(row_351), 1L)
  expect_true(is.na(row_351$polity_code))
})
