# `.iso3_to_fao_area_code()` maps ISO3 to a FAOSTAT reporting area. An ISO3 can
# name two areas, because FAOSTAT keeps the pre-split entity next to its
# successor (ETH is 62 "Ethiopia PDR" and 238 "Ethiopia"; SDN is 276 and 206).
#
# The tie used to be broken by `unique(bridge, by = "iso3c")`, i.e. by row order,
# which resolved ETH to the DISSOLVED area 62 for every year. It is now broken on
# the polities database: prefer the area that is its polity's polity_area_code.

test_that("ambiguous ISO3 codes resolve to the current reporting area", {
  df <- data.frame(
    area_code = c("ETH", "SDN", "FRA"),
    year = 2000L,
    v = 1
  )
  out <- as.data.frame(whep:::.iso3_to_fao_area_code(df))

  # 238 not 62: Ethiopia PDR is the pre-1993 entity and must not receive
  # modern rows.
  expect_equal(out$area_code[1], 238L)
  # 206 is Sudan's canonical polity_area_code, which is what WHEP resolves
  # SDN to regardless of FAOSTAT's "(former)" label on it.
  expect_equal(out$area_code[2], 206L)
  expect_false(anyNA(out$area_code))
})

test_that("the rule uniquely resolves every ISO3 in the lookup", {
  lk <- as.data.frame(whep:::.current_area_lookup(include_unmapped = FALSE))
  lk <- lk[!is.na(lk$area_iso3c), ]
  d <- unique(lk[, c("area_iso3c", "area_code", "polity_area_code")])
  d$canonical <- d$area_code == d$polity_area_code

  n_canonical <- tapply(d$canonical, d$area_iso3c, sum)
  # Over-determination would mean the rule itself has to guess, which is the
  # failure mode it exists to remove.
  expect_equal(
    sum(n_canonical > 1),
    0L,
    info = paste0(
      "ISO3 codes with more than one canonical area: ",
      paste(names(n_canonical)[n_canonical > 1], collapse = ", ")
    )
  )

  # Where the rule does not apply (territories folded into an aggregate polity,
  # whose area code never equals the aggregate's), there must be nothing to
  # tie-break in the first place.
  no_canonical <- names(n_canonical)[n_canonical == 0]
  multi <- vapply(
    no_canonical,
    function(i) length(unique(d$area_code[d$area_iso3c == i])),
    integer(1)
  )
  expect_equal(
    sum(multi > 1),
    0L,
    info = paste0(
      "ISO3 codes with no canonical area AND several candidates, so the ",
      "function would abort: ",
      paste(names(multi)[multi > 1], collapse = ", ")
    )
  )
})

test_that("no FAOSTAT area maps to two ISO3 codes", {
  # The inverse bridge relies on this, and unlike the forward direction it has
  # no tie-break at all.
  lk <- as.data.frame(whep:::.current_area_lookup(include_unmapped = TRUE))
  d <- unique(lk[, c("area_code", "area_iso3c")])
  dup <- unique(d$area_code[duplicated(d$area_code)])
  expect_equal(
    length(dup),
    0L,
    info = paste0(
      "areas with several ISO3 codes: ",
      paste(dup, collapse = ", ")
    )
  )
})

test_that("the ISO3 bridge round-trips through the FAOSTAT bridge", {
  df <- data.frame(
    area_code = c("ETH", "SDN", "FRA", "BRA"),
    year = 2000L,
    v = 1
  )
  fao <- whep:::.iso3_to_fao_area_code(df)
  back <- as.data.frame(whep:::.fao_to_iso3_area_code(fao))
  expect_setequal(back$area_code, c("ETH", "SDN", "FRA", "BRA"))
})
