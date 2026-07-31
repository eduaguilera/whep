# Which ISO3 codes do this package's own tables use that no polity claims?
#
# `regions_full$iso3c` and `polity_area_crosswalk$area_iso3c` are how area-keyed data reaches a
# territory identity, and both are consumed by real code — the LUH2 bridge joins on ISO3, GLEAM
# is keyed by it, `.energy_ldc_iso3()` names 46 of them. A code in these tables that resolves to
# no polity is a dead end: it looks like an identity and joins to nothing.
#
# There are 39, and the two tables agree exactly on which — itself worth knowing, since they are
# built by separate `data-raw` scripts and a disagreement would mean one was regenerated alone.
#
# They fall into four classes. Only the first is a defect, and it is upstream's:
#
#   DISSOLVED STATES (3)         CSK Czechoslovakia, SUN USSR, YUG Yugoslav SFR. Polities for
#                                these plainly exist -- `F51-1947-1993` is one -- and carry
#                                no iso3_code at all. Upstream is inconsistent here: Burma's
#                                four polities carry their successor's MMR and Netherlands
#                                Antilles carries its own ANT, while these carry nothing.
#                                Filed as whep-polities issue 55; until it is decided, a
#                                consumer resolving CSK or SUN gets nothing back for a polity
#                                that plainly exists.
#
#   NOT A TERRITORY (4)          NTZ Neutral Zone, OXY "Others (adjustment)", UXY "Unspecified",
#                                PCI Pacific Islands Trust Territory. Statistical sentinels. No
#                                polity should exist and none does.
#
#   NO AGRICULTURAL REPORTING    ATA Antarctica, ATF, BVT, HMD, SGS, IOT, CTE, JTN, MID, WAK,
#   OF THEIR OWN (14)            PCN, CCK, TKL, UMI. Uninhabited, research stations, or atolls
#                                whose output is not separately reported.
#
#   DEPENDENCY OF A REPORTING    ABW, AIA, ALA, BLM, CUW, CYM, GGY, GIB, IMN, JEY, MAF, MNP,
#   PARENT (18)                  MYT, SJM, SXM, TCA, VAT, WLF. These are the interesting ones:
#                                their row carries the PARENT's name, so `ALA` reads "Finland",
#                                `BLM` reads "France", and GGY/IMN/JEY all read "United Kingdom".
#                                That is exactly what makes a name-to-ISO3 lookup ambiguous, and
#                                why `.correct_iso3_from_polities()` filters to rows with an
#                                `area_code`. Six of them are whep#407, where the polity exists
#                                upstream but there is no FAOSTAT area to aggregate through.
#
# Pinned by identity rather than by count. A NEW unresolvable code means an area was added whose
# identity joins to nothing, and 39 -> 40 would say nothing about which or why.

.unresolvable_iso3 <- function() {
  pol <- sf::st_drop_geometry(whep::polities)
  known <- unique(stats::na.omit(c(pol$iso3_code, pol$iso3c)))
  r <- as.data.frame(whep::regions_full)
  cw <- as.data.frame(whep::polity_area_crosswalk)
  used <- unique(stats::na.omit(c(r$iso3c, cw$area_iso3c)))
  sort(setdiff(used, known))
}

testthat::test_that("the ISO3 codes no polity claims are the known 39", {
  # Non-vacuous: an empty `known` set would make every code unresolvable and the list below
  # would be meaningless.
  pol <- sf::st_drop_geometry(whep::polities)
  testthat::expect_gt(
    length(unique(stats::na.omit(c(pol$iso3_code, pol$iso3c)))),
    200L
  )

  testthat::expect_setequal(
    .unresolvable_iso3(),
    c(
      # dissolved states whose polities exist but carry no ISO3 (whep-polities#55)
      "CSK",
      "SUN",
      "YUG",
      # statistical sentinels, not territories
      "NTZ",
      "OXY",
      "PCI",
      "UXY",
      # no agricultural reporting of their own
      "ATA",
      "ATF",
      "BVT",
      "CCK",
      "CTE",
      "HMD",
      "IOT",
      "JTN",
      "MID",
      "PCN",
      "SGS",
      "TKL",
      "UMI",
      "WAK",
      # dependency of a reporting parent
      "ABW",
      "AIA",
      "ALA",
      "BLM",
      "CUW",
      "CYM",
      "GGY",
      "GIB",
      "IMN",
      "JEY",
      "MAF",
      "MNP",
      "MYT",
      "SJM",
      "SXM",
      "TCA",
      "VAT",
      "WLF"
    )
  )
})

testthat::test_that("both tables agree on which ISO3 codes are unresolvable", {
  # They are built by separate data-raw scripts, and the promotion withdrawn on this branch
  # survived one round precisely because a value was written down twice and only one copy was
  # rebuilt. So the agreement is asserted, not assumed.
  pol <- sf::st_drop_geometry(whep::polities)
  known <- unique(stats::na.omit(c(pol$iso3_code, pol$iso3c)))
  r <- as.data.frame(whep::regions_full)
  cw <- as.data.frame(whep::polity_area_crosswalk)

  from_regions <- sort(setdiff(unique(stats::na.omit(r$iso3c)), known))
  from_crosswalk <- sort(setdiff(unique(stats::na.omit(cw$area_iso3c)), known))
  # Non-vacuous: both must actually carry codes.
  testthat::expect_gt(length(from_regions), 20L)
  testthat::expect_gt(length(from_crosswalk), 20L)
  testthat::expect_setequal(from_regions, from_crosswalk)
})

testthat::test_that("the dissolved states have polities that simply lack the code", {
  # This separates the one defect class from the other three. For CSK, SUN and YUG a polity
  # exists and is findable BY NAME while no polity carries the ISO3 -- which is what makes them
  # different in kind from Antarctica, where there is correctly nothing to find.
  pol <- sf::st_drop_geometry(whep::polities)

  for (nm in c("Czechoslovakia", "USSR|Soviet", "Yugoslav")) {
    hits <- pol[grep(nm, pol$polity_name, ignore.case = TRUE), ]
    testthat::expect_gt(nrow(hits), 0L)
  }

  # And the codes really are absent, so this is an omission rather than a lookup that missed.
  known <- unique(stats::na.omit(c(pol$iso3_code, pol$iso3c)))
  for (iso in c("CSK", "SUN")) {
    testthat::expect_false(iso %in% known)
  }

  # Contrast, and the reason the classes are worth distinguishing: Burma's polities DO carry a
  # code -- their successor's, MMR -- so upstream solves the same problem two ways.
  #
  # Checked against Burma rather than Zaire, which is what I first reached for and got wrong:
  # there is NO polity named Zaire. A loose `"Zaire|Congo"` pattern matched twelve Congo
  # polities carrying their own correct COD/COG, which is not an example of anything. The
  # difference matters because the whole argument in whep-polities#55 is about which code a
  # dissolved entity's polity carries.
  burma <- pol[grep("Burma", pol$polity_name, ignore.case = TRUE), ]
  testthat::skip_if(nrow(burma) == 0L, "no Burma polity to compare against")
  testthat::expect_true(all(!is.na(burma$iso3_code)))
  # Its own 3166-3 code is BUR; what it actually carries is the successor's.
  testthat::expect_true("MMR" %in% burma$iso3_code)
  testthat::expect_false("BUR" %in% known)
})

testthat::test_that("upstream's local codes and our unresolvable codes are disjoint", {
  # Closes the loop with the published contract, and the relationship is the opposite of what I
  # first assumed -- worth recording, because the wrong version looked obviously right.
  #
  # `polities_manifest.json` now publishes `local_iso3_codes`: the 56 values in `polities` that
  # are LOCAL identifiers rather than ISO 3166 codes, because there is no ISO code for
  # Austria-Hungary and inventing `AUH` beats leaving it blank.
  #
  # I expected CSK, SUN and YUG to be on that list, since they are the dissolved states this
  # file reports as unresolvable. They are not, and cannot be: a code is "local" because
  # `polities` USES it, while these are unresolvable precisely because `polities` uses nothing
  # for them (whep-polities#55). The two sets are therefore **disjoint by construction**:
  #
  #   local        = codes polities carries that are not ISO      -> always resolvable here
  #   unresolvable = codes our tables reference that polities lacks -> never local
  #
  # Measured: 56 local, 39 unresolvable, **0 in both**, and the 10 local codes that appear in
  # our tables are all resolvable.
  #
  # That disjointness is the property worth guarding. A local code turning up as unresolvable
  # would mean upstream dropped a polity while our tables still reference its code -- exactly
  # the drift this integration exists to prevent, and invisible without comparing the two lists.
  path <- Sys.getenv("WHEP_POLITIES_MANIFEST", unset = "")
  testthat::skip_if(
    path == "" || !file.exists(path),
    "upstream manifest not reachable; set WHEP_POLITIES_MANIFEST"
  )
  mf <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  local_codes <- mf$local_iso3_codes
  testthat::skip_if(
    is.null(local_codes),
    "manifest predates local_iso3_codes; regenerate upstream"
  )
  testthat::expect_gt(length(local_codes), 40L)

  unresolvable <- .unresolvable_iso3()

  # The invariant: nothing upstream calls local may be unresolvable here.
  testthat::expect_equal(
    intersect(local_codes, unresolvable),
    character(0),
    info = paste(
      "upstream declares these local, yet they resolve to no polity here --",
      "either a polity was dropped or the baseline is stale:",
      paste(intersect(local_codes, unresolvable), collapse = ", ")
    )
  )

  # Non-vacuous: some local codes must actually appear in our tables, or the check above is
  # comparing against nothing. Ten do.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  r <- as.data.frame(whep::regions_full)
  used <- unique(stats::na.omit(c(r$iso3c, cw$area_iso3c)))
  testthat::expect_gte(length(intersect(local_codes, used)), 5L)
})
