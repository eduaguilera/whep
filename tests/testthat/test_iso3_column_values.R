# Columns named `iso3`, `iso3c` or `*_iso3c` should hold ISO 3166-1 alpha-3 codes. Across the
# exported datasets, 77 distinct values are not in the current ISO list — and checking them one by
# one, every single one is legitimate. They fall into four groups:
#
#   WITHDRAWN ISO codes, which were real assignments and are absent only from a CURRENT list:
#     CSK Czechoslovakia, SUN Soviet Union, YUG Yugoslavia, SCG Serbia and Montenegro, ANT
#     Netherlands Antilles, NTZ Neutral Zone. Flagging these would be wrong: the data is historical
#     and so are the codes.
#   territories with no ISO code, ever: CTE Canton and Enderbury, JTN Johnston, MID Midway, WAK
#     Wake, PCI Pacific Islands Trust Territory, ICN Canary Islands, KOS Kosovo.
#   historical entities predating ISO 3166 entirely: AUH Austria-Hungary, OTT Ottoman Empire, AOF
#     French West Africa, AEF French Equatorial Africa, NSW New South Wales, TUS Tuscany, and
#     most of the rest.
#   WHEP-internal keys for aggregates and source-specific labels: BLX, RAFR, RASI, REUR, RLAM,
#     RNAM, ROCE, ROW, OXY, UXY, plus the FAO-style legacy codes in Mueller's table (BZE, COS,
#     ELS, GUA, HAI, HON, ROM, SRM, TRI, ZAR), which are aliased upstream.
#
# So the value of a check here is not "find a wrong code" — there are none — but to catch an
# INVENTED one: a typo, or a plausible-looking code someone makes up for a territory that has a real
# one. The baseline is the 77 known values; anything else fails.
#
# Kept as a count plus a set rather than prose, because the four groups above are a judgement I made
# once and should not have to remake.

testthat::test_that("no iso3-named column gains an unrecognised code", {
  # The ISO list is EMBEDDED, not read from countrycode. That package is not a dependency of this
  # one — it happened to be installed on the machine where this test was written — so on CI the
  # test skipped with "{countrycode} is not installed" and guarded nothing. Confirmed from CI's own
  # skip list rather than assumed.
  #
  # Exactly the same failure and the same fix as scripts/validate_iso_codes.py upstream, which
  # reported "skipped" inside a passing run for the same reason. Having now made it twice, the
  # general rule: a guard that depends on an ambient package is a guard that does not exist where
  # it matters.
  valid <- strsplit(
    paste(
      "ABW AFG AGO AIA ALA ALB AND ARE ARG ARM ASM ATA ATF ATG AUS AUT AZE BDI BEL BEN BES BFA ",
      "BGD BGR BHR BHS BIH BLM BLR BLZ BMU BOL BRA BRB BRN BTN BVT BWA CAF CAN CCK CHE CHL CHN ",
      "CIV CMR COD COG COK COL COM CPV CRI CUB CUW CXR CYM CYP CZE DEU DJI DMA DNK DOM DZA ECU ",
      "EGY ERI ESH ESP EST ETH FIN FJI FLK FRA FRO FSM GAB GBR GEO GGY GHA GIB GIN GLP GMB GNB ",
      "GNQ GRC GRD GRL GTM GUF GUM GUY HKG HMD HND HRV HTI HUN IDN IMN IND IOT IRL IRN IRQ ISL ",
      "ISR ITA JAM JEY JOR JPN KAZ KEN KGZ KHM KIR KNA KOR KWT LAO LBN LBR LBY LCA LIE LKA LSO ",
      "LTU LUX LVA MAC MAF MAR MCO MDA MDG MDV MEX MHL MKD MLI MLT MMR MNE MNG MNP MOZ MRT MSR ",
      "MTQ MUS MWI MYS MYT NAM NCL NER NFK NGA NIC NIU NLD NOR NPL NRU NZL OMN PAK PAN PCN PER ",
      "PHL PLW PNG POL PRI PRK PRT PRY PSE PYF QAT REU ROU RUS RWA SAU SDN SEN SGP SGS SHN SJM ",
      "SLB SLE SLV SMR SOM SPM SRB SSD STP SUR SVK SVN SWE SWZ SXM SYC SYR TCA TCD TGO THA TJK ",
      "TKL TKM TLS TON TTO TUN TUR TUV TWN TZA UGA UKR UMI URY USA UZB VAT VCT VEN VGB VIR VNM ",
      "VUT WLF WSM YEM ZAF ZMB ZWE ",
      collapse = ""
    ),
    " +"
  )[[1]]
  valid <- valid[nzchar(valid)]
  testthat::expect_gt(length(valid), 240L)

  known_non_iso <- c(
    "AEF",
    "ANG",
    "ANT",
    "AOF",
    "AOI",
    "AUH",
    "BEC",
    "BLI",
    "BLX",
    "BSS",
    "BWI",
    "BZE",
    "CAP",
    "COS",
    "CSK",
    "CTE",
    "DMO",
    "DPA",
    "ELS",
    "FID",
    "FRN",
    "GCO",
    "GUA",
    "HAI",
    "HON",
    "ICN",
    "ITS",
    "JTN",
    "KOS",
    "MAN",
    "MID",
    "MOR",
    "NAT",
    "NFL",
    "NNI",
    "NSW",
    "NTZ",
    "NWR",
    "OTT",
    "OXY",
    "PAL",
    "PAP",
    "PCI",
    "QUE",
    "RAFR",
    "RASI",
    "REUR",
    "RLAM",
    "RNAM",
    "ROCE",
    "ROM",
    "ROW",
    "RWB",
    "SAA",
    "SAB",
    "SAC",
    "SAR",
    "SCG",
    "SER",
    "SMO",
    "SNI",
    "SNW",
    "SRM",
    "SUD",
    "SUN",
    "SWA",
    "TAN",
    "TAS",
    "TRI",
    "TUS",
    "TWO",
    "UXY",
    "VIC",
    "WAK",
    "YUG",
    "ZAR",
    "ZNZ"
  )

  exported <- utils::data(package = "whep")$results[, "Item"]
  seen <- character()
  columns <- 0L
  for (nm in exported) {
    d <- tryCatch(get(nm, envir = asNamespace("whep")), error = function(e) {
      NULL
    })
    if (is.null(d) || !is.data.frame(d)) {
      next
    }
    for (col in grep("^iso3|iso3c$|_iso3c$", names(d), value = TRUE)) {
      columns <- columns + 1L
      v <- unique(stats::na.omit(as.character(d[[col]])))
      seen <- c(seen, v[nzchar(v)])
    }
  }
  # Non-vacuous: several datasets carry such a column.
  testthat::expect_gte(columns, 5L)

  unrecognised <- sort(setdiff(unique(seen), c(valid, known_non_iso)))
  testthat::expect_equal(
    length(unrecognised),
    0L,
    info = paste0(
      "these values sit in an iso3-named column and are neither ISO 3166-1 nor a known ",
      "historical/internal code — most likely a typo or an invented code: ",
      paste(unrecognised, collapse = ", ")
    )
  )

  # Bidirectional: a baselined code that disappears should be removed from the list, so the
  # judgement above stays tied to the data rather than accumulating dead entries.
  stale <- sort(setdiff(known_non_iso, unique(seen)))
  testthat::expect_equal(
    length(stale),
    0L,
    info = paste0(
      "baselined non-ISO codes no longer present anywhere: ",
      paste(stale, collapse = ", ")
    )
  )
})
