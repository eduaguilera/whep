# Several published datasets carry a country or area identifier that is NOT
# resolvable against the polities database. None of them is joined by anything in
# R/ today, so nothing is currently wrong in the outputs — but each is a trap
# primed for the first person who writes the obvious join, which is exactly how
# `regions_full$polity_code` (271 bare prefixes, joined to nothing) went unnoticed.
#
# Each set is baselined rather than asserted empty, and asserted in BOTH
# directions: an identifier that stops resolving fails, and one that starts
# resolving fails until it is removed from the baseline. So the lists shrink as
# the data is fixed and cannot quietly license a regression.
#
# The right fix is upstream aliases, not a lookup table here — whep-polities
# already resolves source labels to polities that way, and "Cape Verde",
# "Swaziland", "Turkey", "ROM" and "ZAR" are textbook alias cases. See whep#389.

known_area_iso3 <- function() {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  unique(stats::na.omit(cw$area_iso3c))
}

known_area_names <- function() {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  unique(c(stats::na.omit(cw$area_name), stats::na.omit(cw$polity_name)))
}

# `mueller_synthetic_n$iso3c` is named for ISO3 but holds FAO-style legacy codes
# for ten countries: Belize is BLZ not BZE, Romania ROU not ROM, and ZAR is Zaire,
# which became COD in 1997. 328 of 5043 rows.
mueller_unresolved_iso3 <- c(
  "BZE",
  "COS",
  "ELS",
  "GUA",
  "HAI",
  "HON",
  "ROM",
  "SRM",
  "TRI",
  "ZAR"
)

# `lassaletta_grassland_share$Country` holds free-text names with spelling and
# vintage variants: Cabo Verde as "Cape Verde", Eswatini as "Swaziland", Türkiye
# as "Turkey", Réunion unaccented, plus "FSU" for the former Soviet Union, which
# is an aggregate rather than a country. 441 of 6909 rows.
lassaletta_unresolved_names <- c(
  "Belgium-Luxemburg",
  "Cape Verde",
  "Cote d'Ivoire",
  "DPRepublic of Korea",
  "FSU",
  "Occupied Palestinian Territory",
  "Reunion",
  "Swaziland",
  "Turkey"
)

test_that("mueller_synthetic_n's iso3c column resolves, bar the known legacy codes", {
  m <- as.data.frame(whep::mueller_synthetic_n)
  unresolved <- sort(setdiff(unique(m$iso3c), known_area_iso3()))

  expect_setequal(unresolved, mueller_unresolved_iso3)
})

test_that("lassaletta_grassland_share's Country column resolves, bar the known variants", {
  l <- as.data.frame(whep::lassaletta_grassland_share)
  unresolved <- sort(setdiff(unique(l$Country), known_area_names()))

  expect_setequal(unresolved, lassaletta_unresolved_names)
})

test_that("urban_n_reference's area_code is an ISO3 string, not a FAOSTAT area code", {
  # Documented as a fact rather than asserted away. The column is named
  # `area_code`, which everywhere else in this package means the numeric FAOSTAT
  # reporting area, but it holds "ESP". A consumer joining it to `area_code` gets
  # nothing, and gets it silently. The dataset is Spain-only reference data used
  # in a derivation, so renaming the column is a breaking change and the
  # maintainer's call.
  u <- as.data.frame(whep::urban_n_reference)
  expect_true(is.character(u$area_code))
  expect_setequal(unique(u$area_code), "ESP")
  # And the thing that makes it a trap: it does not resolve as an area code.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  expect_false(any(as.character(cw$area_code) %in% unique(u$area_code)))
  # It WOULD resolve as an iso3, which is what it actually is.
  expect_true("ESP" %in% known_area_iso3())
})

test_that("region-taxonomy columns are not expected to resolve to polities", {
  # conv_bouwman$region_bouwman is a 17-region source taxonomy, like
  # region_krausmann — owned by the model that uses it, not derivable from
  # polities (see test_region_classifications.R). Asserted so that a future
  # reader does not mistake it for an unfixed gap alongside the three above.
  b <- as.data.frame(whep::conv_bouwman)
  expect_true("region_bouwman" %in% names(b))

  regions <- unique(b$region_bouwman)
  expect_equal(length(regions), 17L)

  # The taxonomy mixes multi-country regions ("Eastern Africa", "South Asia")
  # with singletons that are one country ("Canada", "Japan", "USA"). So it is not
  # true that no value looks like a country identifier — "USA" is also an ISO3.
  # Pinned exactly, because a blanket "no region resolves as an area" assertion is
  # false here and pretending otherwise would just be a failing test someone
  # eventually deletes.
  expect_setequal(intersect(regions, known_area_iso3()), "USA")
})
