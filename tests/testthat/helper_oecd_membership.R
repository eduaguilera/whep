# The OECD's own membership list, as ISO3 (whep#574) -------------------------
#
# The 38 Members named on <https://www.oecd.org/en/about/members-partners.html>
# (read 2026-08-25): Australia, Austria, Belgium, Canada, Chile, Colombia,
# Costa Rica, Czechia, Denmark, Estonia, Finland, France, Germany, Greece,
# Hungary, Iceland, Ireland, Israel, Italy, Japan, Korea, Latvia, Lithuania,
# Luxembourg, Mexico, Netherlands, New Zealand, Norway, Poland, Portugal,
# Slovak Republic, Slovenia, Spain, Sweden, Switzerland, Turkiye, the United
# Kingdom and the United States. Comoros appears there in none of the three
# lists -- not as a Member, an accession candidate or a Key Partner.
#
# `gleam_geographic_hierarchy$oecd` is asserted to equal this set exactly, not
# merely to contain it: GLEAM 3.0 Supplement S1 cell G41 flags Comoros as OECD,
# which `data-raw/livestock_coefficients.R` now corrects. Set equality is what
# makes an accession, a withdrawal or a reintroduced upstream typo fail loudly
# instead of silently repricing a country's energy intensity.
#
# One list, used by both test_datasets.R (the shipped column) and
# test_energy_co2_extension.R (every Member has a row, which is what lets the
# derived rows set `oecd = 0`), so the two cannot drift apart.
oecd_members_iso3 <- function() {
  c(
    "AUS",
    "AUT",
    "BEL",
    "CAN",
    "CHL",
    "COL",
    "CRI",
    "CZE",
    "DNK",
    "EST",
    "FIN",
    "FRA",
    "DEU",
    "GRC",
    "HUN",
    "ISL",
    "IRL",
    "ISR",
    "ITA",
    "JPN",
    "KOR",
    "LVA",
    "LTU",
    "LUX",
    "MEX",
    "NLD",
    "NZL",
    "NOR",
    "POL",
    "PRT",
    "SVK",
    "SVN",
    "ESP",
    "SWE",
    "CHE",
    "TUR",
    "GBR",
    "USA"
  )
}
