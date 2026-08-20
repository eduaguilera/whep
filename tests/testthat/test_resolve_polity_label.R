# resolve_polity_label() is the label-keyed counterpart of add_polity_code().
# Before it existed there was no supported path in this package from a country
# LABEL to a polity, only from a numeric FAOSTAT area code, so every dataset that
# names its territories in words carried an identifier resolving to nothing
# (whep#389). These tests pin the resolution rules AND the measured coverage the
# primitive buys, because a resolver that quietly answers less than it should is
# indistinguishable from one that works.

test_that("FAO-style legacy codes resolve where plain lookups cannot", {
  # mueller_synthetic_n$iso3c is NOT ISO3 despite its name. Ten values are
  # FAO-style legacy codes with no match in polities$iso3_code, which is what
  # made 328 of its 5,043 rows unresolvable. The alias map published by
  # whep-polities carries all ten, so they must resolve here.
  legacy <- c(
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
  resolved <- resolve_polity_label(
    legacy,
    source = "mueller-synthetic-n",
    year = 2000L
  )

  expect_false(any(is.na(resolved)))
  # ZAR is Zaire, renamed 1997; the alias must land on the Congo polity rather
  # than on a ZAR prefix that no longer exists.
  expect_equal(resolved[legacy == "ZAR"], "COD-1960-2025")
  expect_equal(resolved[legacy == "ROM"], "ROU-1947-2025")
  expect_equal(resolved[legacy == "BZE"], "BLZ-1981-2025")

  # None of the ten is reachable through the polity ISO3 index, so the alias map
  # is doing the work and not the fallback below.
  iso3 <- stats::na.omit(whep::polities$iso3_code)
  expect_equal(intersect(legacy, iso3), character(0))
})

test_that("resolution is year-aware, so a label reaches the right period", {
  # "Cape Verde" in 1970 is the Portuguese colony and in 1990 the republic. A
  # label whose referent changes is precisely why the alias map is year-scoped;
  # a spelling-only fix would collapse both onto one polity.
  resolved <- resolve_polity_label(
    c("Cape Verde", "Cape Verde"),
    source = "lassaletta-grassland-share",
    year = c(1970L, 1990L)
  )
  expect_equal(resolved, c("CPV-1886-1975", "CPV-1975-2025"))
})

test_that("a source-scoped alias never applies to another source", {
  # The IIA aliases for "burundi" route 1922-1961 to Ruanda-Urundi, because that
  # is the entity IIA reported under the label; FAOSTAT means the modern state.
  # Reading the IIA rule while processing FAOSTAT would misattribute the data.
  expect_equal(
    resolve_polity_label("burundi", source = "iia", year = 1930L),
    "RWB-1922-1962"
  )
  expect_equal(
    resolve_polity_label("burundi", source = "faostat", year = 2000L),
    "BDI-1962-2025"
  )
  # With no source given, only unscoped aliases apply. Every "burundi" alias is
  # source-scoped, so the name route gets its turn -- and refuses, because the
  # rules that speak about 1930 name the RWB family while the name names BDI.
  expect_true(is.na(resolve_polity_label("burundi", year = 1930L)))
})

test_that("a missing year bound is unbounded on that side, not unscoped", {
  # One published alias is `italy | iia | (blank) | 1860 -> SAR-1800-1860`.
  # Requiring both bounds before honouring either made the 1860 bound do nothing,
  # so IIA data labelled "italy" resolved to Sardinia in the year 2000.
  expect_equal(
    resolve_polity_label("italy", source = "iia", year = 1850L),
    "SAR-1800-1860"
  )
  expect_equal(
    resolve_polity_label("italy", source = "iia", year = 2000L),
    "ITA-1919-2025"
  )
})

test_that("a year-scoped alias is silent outside its span, not contradicted", {
  # The only "natal" alias covers 1910-1957 and routes to South Africa, which is
  # right from Union onwards. In 1900 no rule speaks, so the name route may
  # answer the colony that actually held the territory.
  expect_equal(resolve_polity_label("natal", year = 1900L), "NAT-1895-1910")
})

test_that("ambiguous identifiers return NA instead of picking by row order", {
  # Panama in 1970 is the live case: `PAN-1903-1979` and `CZN-1903-1979`, the
  # Canal Zone, both carry ISO3 `PAN` and both cover that year. They are a real
  # territorial overlap rather than a data defect -- the Zone was administered
  # separately inside Panama's borders -- so no re-sync will remove it, which is
  # what makes it the right case to pin the guard on. Resolving by row order
  # would invent an answer; NA says the label needs an alias.
  expect_true(is.na(resolve_polity_label("PAN", year = 1970L)))
  # And unambiguous once the Zone ends: exactly one live polity carries `PAN`.
  expect_equal(resolve_polity_label("PAN", year = 2000L), "PAN-1979-2025")

  # Montenegro USED to be this test's case, on `MNE-1913-1915` overlapping
  # `MNE-1913-1918` while both were draft upstream. That was a data defect, filed
  # as whep-polities#62, and upstream has retired `MNE-1913-1915`. So 1914 now has
  # one live answer, and the guard needed a case that is not a bug waiting to be
  # fixed -- hence Panama above.
  expect_equal(
    resolve_polity_label("Montenegro", year = 1914L),
    "MNE-1913-1918"
  )
  expect_equal(
    resolve_polity_label("Montenegro", year = 2010L),
    "MNE-2006-2025"
  )

  # "SDN" motivated the guard on the ISO3 route, and upstream has since answered
  # it properly rather than leaving it to be refused. When this was written the
  # only polity carrying that ISO3 started in 2011, so a pre-secession year had
  # no answer and NA was the honest one. `SUD-1956-2011` now carries `SDN` as its
  # ISO3 too, so a 2000 row resolves to the unified Sudan that actually existed
  # then -- which is the outcome #387 wanted, reached upstream instead of here.
  expect_equal(resolve_polity_label("SDN", year = 2000L), "SUD-1956-2011")
  expect_equal(resolve_polity_label("SDN", year = 2015L), "SDN-2011-2025")
})

test_that("a label naming a deliberately unmapped area is refused", {
  # FAOSTAT area 351 "China" is the aggregate of mainland (41), Hong Kong (96),
  # Macao (128) and Taiwan (214), each reporting separately, and the crosswalk
  # leaves it unmapped for that reason. The name route would resolve it anyway,
  # because normalisation drops parenthesised qualifiers and so folds
  # CHN-1950-2025 "China (PRC)" onto "china" -- which double-counts the aggregate
  # against its four components.
  expect_true(is.na(resolve_polity_label("China", year = 2000L)))
  expect_true(is.na(resolve_polity_label("China", source = "faostat", 2000L)))

  # The alias route is untouched: a curator who states what a given source means
  # by "China" still wins, and that is where the decision belongs.
  expect_equal(
    resolve_polity_label("China", source = "trade-sources", year = 1900L),
    "CHN-1895-1913"
  )
})

test_that("an aggregate label stays NA after its territory stopped existing", {
  # Sources keep reporting dissolved aggregates for decades. lassaletta's "FSU"
  # runs to 2009 though nothing has held that territory since 1991. Routing the
  # later years to a successor would fabricate coverage; NA is a real answer.
  expect_equal(
    resolve_polity_label("FSU", source = "lassaletta-grassland-share", 1980L),
    "F228-1945-1991"
  )
  expect_true(is.na(
    resolve_polity_label("FSU", source = "lassaletta-grassland-share", 2000L)
  ))
})

test_that("resolve_polity_label recovers the coverage whep#389 measured", {
  # These are the row counts that make the primitive worth having, measured on
  # the datasets the issue named. They are asserted as lower bounds so a polity
  # refresh that resolves MORE labels does not fail the test, while a regression
  # that resolves fewer does.
  #
  # mueller_synthetic_n$iso3c: 328 rows carried a legacy code matching no polity,
  # and the other 4,715 carried an ISO3 the package had no label route for at all.
  # All 5,043 now resolve.
  #
  # This bound has moved twice and the history is worth keeping, because both
  # moves were the guard working rather than the resolver regressing. It was 4,999
  # when written. The #530 re-sync then dropped it to 4,809, because a corrected
  # polity period and the row it superseded both stayed in the published `polities`
  # table and both covered 2000, so the ambiguity guard refused to choose --
  # `CAN-1948-2025` beside `CAN-1949-2025`, and the same shape for BRA, ARG, AGO,
  # GRC and IRQ. That exposed a real defect: the inference routes were reading
  # every row of `polities`, including retired and superseded ones, where the
  # crosswalk build has always filtered them. With that fixed, every one of the
  # 234 rows resolves, because each had exactly one LIVE candidate all along.
  #
  # Asserted as a lower bound, so a future vintage that resolves more still passes.
  mueller <- resolve_polity_label(
    whep::mueller_synthetic_n$iso3c,
    source = "mueller-synthetic-n",
    year = 2000L
  )
  expect_gte(sum(!is.na(mueller)), 5043)

  # lassaletta_grassland_share$Country was matched by exact string against
  # regions$area_name, which covered 6,370 of 6,909 rows (92.2%). Resolution
  # reaches 6,781 (98.1%), recovering Ethiopia PDR, Occupied Palestinian
  # Territory, Swaziland and the other name variants. The 128 rows still NA are
  # dissolved aggregates reported past their end (Belgium-Luxemburg,
  # Czechoslovakia, Ethiopia PDR, FSU, Yugoslav SFR) plus South Sudan reported
  # before 2011, all of which are honestly unresolvable.
  lassaletta <- resolve_polity_label(
    whep::lassaletta_grassland_share$Country,
    source = "lassaletta-grassland-share",
    year = as.integer(whep::lassaletta_grassland_share$year)
  )
  expect_gte(sum(!is.na(lassaletta)), 6781)

  # crops_manure_n's "RoW" rows (172 of 31,648) were dropped unconditionally.
  # Upstream answers the routing question with a curated alias, so they resolve
  # to the Rest of World polity rather than needing a policy call here.
  #
  # THE BUCKET'S OWN CODE MOVED, 2023 -> 2025. Rest of World is a statistical
  # reporting bucket, not a historical state, and it stopped three years before
  # FAOSTAT stopped reporting it -- so whep-polities extended all seven buckets
  # (RLAM, RAFR, RASI, REUR, RNAM, ROCE, ROW) to 2025 and retired the old codes.
  # `ROW-1850-2023` is `wiki_status: retired` upstream, so the value asserted here
  # is the live successor, not a renamed alias of the same row.
  expect_equal(
    resolve_polity_label("RoW", source = "crops-manure-n", year = 2000L),
    "ROW-1850-2025"
  )

  # urban_n_reference is FIXED ON MAIN and no longer needs this route at all.
  # This asserted that resolving its `area_code` rescued all 10 rows, because that
  # column held the string "ESP" in a slot that means the numeric FAOSTAT area
  # everywhere else in the package, so a consumer joining on it got zero rows. The
  # dataset now ships `area_code = 203` and carries `polity_code` directly, so the
  # defect is gone at source and a label lookup on a numeric code correctly
  # resolves nothing. Pinned in its fixed shape so the regression would be caught.
  expect_type(whep::urban_n_reference$area_code, "integer")
  expect_equal(unique(whep::urban_n_reference$area_code), 203L)
  expect_equal(unique(whep::urban_n_reference$polity_code), "ESP-1800-2025")
})

test_that("source and year must be scalar or the same length as label", {
  expect_error(
    resolve_polity_label(c("Spain", "France"), year = c(1961L, 1962L, 1963L)),
    "same length"
  )
})

test_that("expand_trade_sources attaches the reporter's polity for each year", {
  # expand_trade_sources() output was the last area-naming table in the package
  # with no polity column at all: trade_sources.csv's Reporter holds bare country
  # names and nothing resolved them. Its rows are already one-year-per-row, so
  # the reporter and the Year settle the polity with no interpolation.
  trade_sources <- readr::read_csv(
    system.file("extdata", "trade_sources.csv", package = "whep"),
    show_col_types = FALSE
  )
  expanded <- trade_sources |>
    expand_trade_sources() |>
    dplyr::ungroup()

  expect_true("reporting_polity_code" %in% names(expanded))
  # All 1,133 expanded rows resolve. China is the one that could not have gone
  # through an area code: FAOSTAT 351 is the unmapped aggregate, so the aliases
  # target the CHN chain directly.
  expect_equal(sum(is.na(expanded$reporting_polity_code)), 0L)
  expect_equal(
    unique(expanded$reporting_polity_code[
      expanded$Reporter == "China" & expanded$Year == 1900
    ]),
    "CHN-1895-1913"
  )
  # The reporter spelled with a trailing "(the)" differs from the canonical area
  # name by nothing else, which is exactly what the alias table is for.
  expect_equal(
    unique(expanded$reporting_polity_code[
      startsWith(expanded$Reporter, "United Kingdom") & expanded$Year == 1950
    ]),
    "GBR-1921-2025"
  )
})

test_that("expand_trade_sources still works without a Reporter column", {
  # The exported example in expand_trade_sources() has no Reporter and no real
  # years, so a missing reporter must yield NA rather than an error.
  toy <- tibble::tibble(
    Name = c("a", "b", "c"),
    Trade = c("t1", "t2", "t3"),
    Info_Format = c("year", "partial_series", "year"),
    Timeline_Start = c(1, 1, 2),
    Timeline_End = c(3, 4, 5),
    Timeline_Freq = c(1, 1, 2),
    `Imp/Exp` = "Imp",
    SACO_link = NA,
  )
  expanded <- expand_trade_sources(toy)

  expect_equal(nrow(expanded), 9L)
  expect_true(all(is.na(expanded$reporting_polity_code)))
})

test_that("the published alias map keeps the contract this package reads", {
  # data-raw/table_mappings.R declares an exhaustive col_types list, so an
  # upstream column that is not named there is a column the build cannot see.
  # This pins the shape rather than the content: the map is authored upstream and
  # its rows change, but a renamed or dropped column would silently disable
  # resolution instead of failing.
  aliases <- whep::polity_label_aliases

  expect_equal(
    names(aliases),
    c(
      "source_label",
      "source",
      "year_start",
      "year_end",
      "polity_code",
      "common_name",
      "confidence",
      "observed_rows"
    )
  )
  expect_false(any(is.na(aliases$source_label)))
  expect_false(any(is.na(aliases$polity_code)))
  # Every alias must name a real WHEP polity code, prefix included.
  expect_true(all(grepl("^[A-Z0-9-]+-[0-9]{4}-[0-9]{4}$", aliases$polity_code)))
})

test_that("a still-open period covers the open-period sentinel year", {
  # #712: the year filter read `end_year` STRICTLY exclusively, so every polity
  # whose interval ends at the open-period sentinel stopped covering its own
  # terminal year and the label route answered NA for essentially every country
  # that exists -- 1 of the 204 `gleam_geographic_hierarchy` ISO3 codes resolved
  # at 2025 against 204 at 2024 -- while `add_polity_code()`, which goes through
  # `.polity_join_end_year()`, resolved them normally. The convention is
  # exclusive at a succession, INCLUSIVE AT AN OPEN END (#577).
  #
  # The sentinel is read from the snapshot rather than written down: it has moved
  # twice in this epic (#530, #551), and a literal would stop testing anything.
  sentinel <- max(whep::polities$end_year, na.rm = TRUE)
  expect_equal(
    resolve_polity_label("ESP", year = sentinel),
    paste0("ESP-1800-", sentinel)
  )
  expect_equal(
    resolve_polity_label("Netherlands", year = sentinel),
    paste0("NLD-1830-", sentinel)
  )

  # The coverage claim itself, as an invariant rather than a hand-picked row: the
  # sentinel year must resolve as many present-day countries as the year before
  # it. Nothing succeeds those polities, so no country can drop out.
  iso3 <- unique(whep::gleam_geographic_hierarchy$iso3)
  expect_equal(
    sum(!is.na(resolve_polity_label(iso3, year = sentinel))),
    sum(!is.na(resolve_polity_label(iso3, year = sentinel - 1L)))
  )

  # Past the sentinel there is nothing to cover: the widening adds ONE year, it
  # does not make an open period unbounded.
  expect_true(is.na(resolve_polity_label("ESP", year = sentinel + 1L)))
})

test_that("a period nothing succeeds covers its own last year", {
  # The same rule away from the sentinel. `ANT-1961-2010` is the Netherlands
  # Antilles, dissolved in 2010 with no successor recorded, and 2010 is a year it
  # reported in -- the case `.polity_join_end_year()` calls out for the numeric
  # route. Read strictly exclusively the label route lost it.
  expect_equal(resolve_polity_label("ANT", year = 2010L), "ANT-1961-2010")
  expect_true(is.na(resolve_polity_label("ANT", year = 2011L)))
})

test_that("a succession year still resolves to exactly one polity", {
  # THE FAILURE MODE THE WIDENING MUST NOT INTRODUCE (#720): widen every open
  # period unconditionally and a boundary year gets two candidates, because a
  # terminated aggregate records no successor and is therefore "open" by the
  # successor test. Measured on the shipped snapshot, three collide --
  # `EGYSUD-1934-1956` beside `EGY-1925-1967` at 1956, `CODRU-1922-1960` beside
  # `COD-1960-2025` at 1960, `MASG-1946-1963` beside `MYS-1963-1965` at 1963 --
  # and the ambiguity guard would answer NA for a year that used to resolve.
  # `.polity_year_candidates()` consults the widened bound only when nothing
  # claims the year outright, so declared containment keeps winning.
  expect_equal(resolve_polity_label("EGY", year = 1956L), "EGY-1925-1967")
  expect_equal(resolve_polity_label("COD", year = 1960L), "COD-1960-2025")
  expect_equal(resolve_polity_label("MYS", year = 1963L), "MYS-1963-1965")

  # And the plain succession boundaries: the year belongs to the SUCCESSOR, which
  # is the half of the convention the exclusive reading buys.
  expect_equal(resolve_polity_label("SDN", year = 2011L), "SDN-2011-2025")
  expect_equal(resolve_polity_label("PAN", year = 1979L), "PAN-1979-2025")
  expect_equal(resolve_polity_label("EGY", year = 1967L), "EGY-1967-1979")
  expect_equal(resolve_polity_label("MYS", year = 1965L), "MYS-1965-2025")
})
