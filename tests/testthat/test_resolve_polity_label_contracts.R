# Three whep-polities contracts `resolve_polity_label()` has to honour, each
# reproduced on a fixture because the shipped snapshot (whep-polities e10c7421)
# predates them: it carries no subnational Santa Cruz or Amazonas, no
# `back_cast` alias and no label-item rule. Every fixture row below is copied
# from whep-polities main at 4bce63d4, so the tests exercise the resolver on
# the shapes the next re-sync will ship.

.contract_polities <- function() {
  tibble::tribble(
    ~polity_code, ~polity_name, ~start_year, ~end_year, ~iso3_code,
    ~wiki_status, ~predecessor, ~successor,
    "ARG-SANTACRUZ-1955-2025", "Santa Cruz (province of Argentina)",
    1955L, 2025L, "ARG", "draft", NA, NA,
    "BOL-SZ-1825-2025", "Santa Cruz (department of Bolivia)",
    1825L, 2025L, "BOL", "draft", NA, NA,
    "BRA-AMAZONAS-1889-2025", "Amazonas",
    1889L, 2025L, "BRA", "draft", NA, NA,
    "COL-AMA-1991-2025", "Amazonas (department of Colombia)",
    1991L, 2025L, "COL", "draft", NA, NA,
    "BRA-DFRJ-1900-1959", "Distrito Federal (Rio de Janeiro)",
    1900L, 1959L, "BRA", "draft", NA, NA,
    "BRA-DF-1960-2025", "Distrito Federal (Brasília)",
    1960L, 2025L, "BRA", "draft", NA, NA,
    "MEX-CMX-1824-2025", "Ciudad de México (Distrito Federal)",
    1824L, 2025L, "MEX", "draft", NA, NA,
    "BRA-TOCANTINS-1988-2025", "Tocantins",
    1988L, 2025L, "BRA", "draft", NA, NA,
    "CAP-1800-1895", "Cape Colony (to 1895)",
    1800L, 1895L, "CAP", "draft", NA, "CAP-1895-1910",
    "CAP-1895-1910", "Cape Colony (1895-1910)",
    1895L, 1910L, "CAP", "draft", "CAP-1800-1895", "ZAF-1910-2025",
    "NAT-1843-1895", "Natal (to 1895)",
    1843L, 1895L, "NAT", "reviewed", "ZUL-1816-1879", "NAT-1895-1910",
    "NAT-1895-1910", "Natal (1895-1910)",
    1895L, 1910L, "NAT", "reviewed", "NAT-1843-1895", "ZAF-1910-2025",
    "ZAF-1910-2025", "South Africa (Union and Republic)",
    1910L, 2025L, "ZAF", "reviewed", "CAP-1895-1910; NAT-1895-1910", NA,
    "NSW-1800-1901", "New South Wales (to 1900)",
    1800L, 1901L, "NSW", "draft", NA, "AUS-1901-2025",
    "AUS-NSW-1901-2025", "New South Wales (state of Australia)",
    1901L, 2025L, "AUS", "draft", "NSW-1800-1901", NA,
    "MOR-1904-1911", "Morocco (1904-1911)",
    1904L, 1911L, "MOR", "draft", "MOR-1800-1904", "MAR-1911-1958",
    "MAR-1911-1958", "Morocco (to 1958)",
    1911L, 1958L, "MAR", "draft", "MOR-1904-1911", "MAR-1958-1975",
    "FID-1887-1954", "French Indochina",
    1887L, 1954L, "FID", "draft", NA, NA,
    "VNM-1887-1954", "Vietnam (French Indochina)",
    1887L, 1954L, "VNM", "draft", NA, NA,
    # whep-polities b44802d8 (#692): the targets of its unrouting and
    # iso-clearing rules.
    "JAM-1800-2025", "Jamaica",
    1800L, 2025L, "JAM", "draft", NA, NA,
    "BWI-1833-1962", "British West Indies (colonial aggregate)",
    1833L, 1962L, "BWI", "draft", NA, NA,
    "SPM-1816-2025", "Saint Pierre and Miquelon",
    1816L, 2025L, "SPM", "draft", NA, NA
  ) |>
    # Upstream types every unit whose code carries a unit segment
    # (`BOL-SZ-...`) as subnational, and these fixture rows follow it.
    dplyr::mutate(
      polity_type = dplyr::if_else(
        stringr::str_count(.data$polity_code, "-") == 3L,
        "subnational",
        "national"
      )
    )
}

.contract_aliases <- function() {
  tibble::tribble(
    ~source_label, ~source, ~year_start, ~year_end, ~polity_code,
    ~disposition,
    "BRA-TOCANTINS", "whep-lab-latam", 1900L, 1987L,
    "BRA-TOCANTINS-1988-2025", "back_cast",
    "BRA-TOCANTINS", "whep-lab-latam", 1988L, 2023L,
    "BRA-TOCANTINS-1988-2025", NA,
    "south africa", NA, 1852L, 1894L, "CAP-1800-1895", NA,
    "south africa", NA, 1895L, 1909L, "CAP-1895-1910", NA,
    "south africa", NA, 1910L, 2025L, "ZAF-1910-2025", NA,
    "south africa", "iia", 1910L, 2025L, "ZAF-1910-2025", NA
  )
}

# The four Mitchell/IIA rules of whep-polities #677 that touch South Africa,
# verbatim apart from `evidence`.
.contract_corrections <- function() {
  tibble::tribble(
    ~source, ~source_label, ~item, ~year_start, ~year_end, ~correct_label,
    ~polity_code,
    "mitchell", "south africa", "sugar cane", 1859L, 1894L, "natal",
    "NAT-1843-1895",
    "mitchell", "south africa", "sugar cane", 1895L, 1909L, "natal",
    "NAT-1895-1910",
    "iia", "south africa", "sugar raw centrifugal", 1909L, 1909L, "natal",
    "NAT-1895-1910",
    "mitchell", "natal", "horses", 1945L, 1957L, "south africa",
    "ZAF-1910-2025"
  )
}

# whep-polities #692's rules, verbatim apart from `evidence`: the doubled
# British West Indies cotton iia files under "jamaica" belongs to no polity,
# and Saint-Pierre-et-Miquelon's eggs iia files under "france" are relabelled
# with the row's iso code cleared.
.contract_corrections_692 <- function() {
  tibble::tribble(
    ~source, ~source_label, ~item, ~year_start, ~year_end, ~correct_label,
    ~polity_code,
    "iia", "jamaica", "cotton lint", 1934L, 1945L,
    "british west indies federation", "UNROUTED",
    "iia", "jamaica", "cotton seed", 1934L, 1945L,
    "british west indies federation", "UNROUTED",
    "iia", "france", "eggs, hen, in shell", 1939L, 1945L,
    "saint pierre and miquelon", "SPM-1816-2025"
  )
}

.resolve_on_contract <- function(
  label,
  ...,
  back_cast = TRUE,
  corrections = .contract_corrections()
) {
  whep:::.resolve_polity_label(
    label,
    query = list(...),
    back_cast = back_cast,
    tables = list(
      aliases = .contract_aliases(),
      polities = .contract_polities(),
      corrections = corrections
    )
  )
}

test_that("a bare subnational name does not resolve to another country", {
  # whep-polities #680: normalisation strips the Bolivian department's
  # parenthesised qualifier, so the panel's Argentine "Santa Cruz" -- reported from 1900, while Argentina's
  # province polity only begins in 1955 -- met Bolivia's department, the only
  # candidate that year. 8,928 panel rows went to Bolivia that way, 5,526
  # Colombian "Amazonas" rows to Brazil and 868 Mexican "Distrito Federal" rows
  # to Brazil.
  labels <- c("Santa Cruz", "Amazonas", "Distrito Federal")
  years <- c(1920L, 1950L, 1950L)

  expect_warning(
    resolved <- .resolve_on_contract(labels, year = years),
    class = "whep_warn_ambiguous_polity_name"
  )
  expect_equal(resolved, rep(NA_character_, 3))

  # With the reporting country, each resolves inside it or not at all: there is
  # no Argentine Santa Cruz polity before 1955, no Colombian Amazonas before
  # 1991, and Mexico City's polity is not named "Distrito Federal".
  expect_equal(
    .resolve_on_contract(
      labels,
      year = years,
      country = c("ARG", "COL", "MEX")
    ),
    rep(NA_character_, 3)
  )
  expect_equal(
    .resolve_on_contract(
      c(labels, "Santa Cruz", "Amazonas"),
      year = c(years, 1960L, 1995L),
      country = c("BOL", "BRA", "BRA", "ARG", "COL")
    ),
    c(
      "BOL-SZ-1825-2025",
      "BRA-AMAZONAS-1889-2025",
      "BRA-DFRJ-1900-1959",
      "ARG-SANTACRUZ-1955-2025",
      "COL-AMA-1991-2025"
    )
  )
})

test_that("the refusal names what it refused and spares unshared names", {
  cnd <- expect_warning(
    .resolve_on_contract(c("Santa Cruz", "Tocantins"), year = 1990L),
    class = "whep_warn_ambiguous_polity_name"
  )
  expect_match(conditionMessage(cnd), "Santa Cruz")
  expect_no_match(conditionMessage(cnd), "Tocantins")
  # A name only one country carries still resolves without a country.
  expect_no_warning(
    expect_equal(
      .resolve_on_contract("Tocantins", year = 1990L),
      "BRA-TOCANTINS-1988-2025"
    )
  )
})

test_that("one territory's lineage across an ISO3 change is not a collision", {
  # The colony and the state, or the protectorate and the kingdom, carry one
  # name under two ISO3 codes but never at the same time, so a year picks the
  # right one and nothing is ambiguous. Refusing these would have undone every
  # period of New South Wales, Morocco, Palestine and the Russian chain.
  expect_no_warning(
    resolved <- .resolve_on_contract(
      c("New South Wales", "New South Wales", "Morocco", "Morocco"),
      year = c(1900L, 1901L, 1910L, 1911L)
    )
  )
  expect_equal(
    resolved,
    c("NSW-1800-1901", "AUS-NSW-1901-2025", "MOR-1904-1911", "MAR-1911-1958")
  )
})

test_that("a national polity's qualifier is not another name for it", {
  # "Vietnam (French Indochina)" names its container, not itself; counting it
  # as a name refused "French Indochina" for all 67 of its years.
  expect_no_warning(
    expect_equal(
      .resolve_on_contract("French Indochina", year = 1930L),
      "FID-1887-1954"
    )
  )
})

test_that("a country restricts the name route, never the alias route", {
  # An alias names its polity explicitly; the reporting country is a guard on
  # inference only.
  expect_equal(
    .resolve_on_contract("south africa", year = 1900L, country = "ZAF"),
    "CAP-1895-1910"
  )
  expect_true(is.na(
    .resolve_on_contract("Tocantins", year = 1990L, country = "ARG")
  ))
})

test_that("back_cast aliases route by default and can be excluded", {
  # whep-polities #667: 1900-1987 Tocantins is a reconstruction onto the
  # 1988 boundary, routed to the modern state while its span still starts
  # in 1988.
  years <- c(1950L, 1990L)
  expect_equal(
    .resolve_on_contract(
      rep("BRA-TOCANTINS", 2),
      source = "whep-lab-latam",
      year = years
    ),
    rep("BRA-TOCANTINS-1988-2025", 2)
  )
  expect_equal(
    .resolve_on_contract(
      rep("BRA-TOCANTINS", 2),
      source = "whep-lab-latam",
      year = years,
      back_cast = FALSE
    ),
    c(NA, "BRA-TOCANTINS-1988-2025")
  )
  expect_error(
    .resolve_on_contract("BRA-TOCANTINS", back_cast = NA),
    "back_cast"
  )
})

test_that("a map without a disposition column is all observation", {
  # The shipped snapshot is older than the column; dropping reconstructions
  # from it must drop nothing rather than fail.
  aliases <- dplyr::select(.contract_aliases(), -"disposition")
  expect_identical(
    whep:::.alias_rules_by_disposition(aliases, back_cast = FALSE),
    aliases
  )
})

test_that("item corrections relabel before resolution, inclusively", {
  # whep-polities #677: Mitchell's pre-Union cane under "south africa" is
  # Natal's; every other item of those years stays with the Cape.
  resolved <- .resolve_on_contract(
    rep("south africa", 5),
    source = "mitchell",
    item = c("sugar cane", "sugar cane", "sugar cane", "maize", "sugar cane"),
    year = c(1859L, 1894L, 1895L, 1894L, 1910L)
  )
  expect_equal(
    resolved,
    c(
      "NAT-1843-1895",
      "NAT-1843-1895",
      "NAT-1895-1910",
      "CAP-1800-1895",
      "ZAF-1910-2025"
    )
  )
  # Each rule lands where upstream records, which is what `polity_code` pins.
  rules <- .contract_corrections()
  expect_equal(
    .resolve_on_contract(
      rules$source_label,
      source = rules$source,
      item = rules$item,
      year = rules$year_start
    ),
    rules$polity_code
  )
})

test_that("item corrections need the source, the item and a year", {
  # Another source, no item or a period row with no year: no correction.
  expect_equal(
    .resolve_on_contract(
      rep("south africa", 3),
      source = c("fao1952", "mitchell", "mitchell"),
      item = c("sugar cane", NA, "sugar cane"),
      year = c(1900L, 1900L, NA)
    )[1:2],
    rep("CAP-1895-1910", 2)
  )
  labels <- rep("south africa", 3)
  expect_identical(
    whep:::.apply_label_item_corrections(
      labels,
      source = "mitchell",
      item = "sugar cane",
      year = NA,
      rules = .contract_corrections()
    ),
    list(
      label = labels,
      relabelled = rep(FALSE, 3),
      unrouted = rep(FALSE, 3)
    )
  )
})

test_that("item corrections do not chain", {
  # "south africa" -> "natal" (cane) and "natal" -> "south africa" (horses)
  # would loop if the second rule read the first's output.
  rules <- .contract_corrections()
  rules$item <- "sugar cane"
  rules$year_start[4] <- 1859L
  expect_equal(
    whep:::.apply_label_item_corrections(
      c("south africa", "natal"),
      source = "mitchell",
      item = "sugar cane",
      year = 1860L,
      rules = rules
    )$label,
    c("natal", "south africa")
  )
})

test_that("an UNROUTED rule leaves its rows unassigned", {
  # whep-polities #692: iia's "jamaica" cotton 1934-1945 is the British West
  # Indies total, doubled. Routing it to BWI-1833-1962 would sum it beside
  # iia's Barbados, which it contains, so upstream routes it nowhere.
  rules <- .contract_corrections_692()
  resolved <- .resolve_on_contract(
    rep("jamaica", 6),
    source = "iia",
    item = c(
      "cotton lint",
      "cotton seed",
      "cotton lint",
      "cotton lint",
      "cotton lint",
      "sugar cane"
    ),
    year = c(1934L, 1945L, 1933L, 1946L, 1940L, 1940L),
    country = c("JAM", "JAM", "JAM", "JAM", NA, "JAM"),
    corrections = rules
  )
  # Inside the rule, with or without `country`: NA. Outside its years or
  # items, Jamaica as before.
  expect_equal(
    resolved,
    c(NA, NA, "JAM-1800-2025", "JAM-1800-2025", NA, "JAM-1800-2025")
  )
  # NA even where the corrected label WOULD resolve: the sentinel, not the
  # label, is what drops the rows.
  routable <- rules
  routable$correct_label <- "british west indies (colonial aggregate)"
  expect_true(is.na(
    .resolve_on_contract(
      "jamaica",
      source = "iia",
      item = "cotton lint",
      year = 1940L,
      corrections = routable
    )
  ))
  expect_equal(
    .resolve_on_contract(
      "british west indies (colonial aggregate)",
      source = "iia",
      item = "cotton lint",
      year = 1940L,
      corrections = routable
    ),
    "BWI-1833-1962"
  )
  expect_identical(
    whep:::.apply_label_item_corrections(
      c("jamaica", "france", "jamaica"),
      source = "iia",
      item = c("cotton lint", "eggs, hen, in shell", "maize"),
      year = 1940L,
      rules = rules
    ),
    list(
      label = c(
        "british west indies federation",
        "saint pierre and miquelon",
        "jamaica"
      ),
      relabelled = c(TRUE, TRUE, FALSE),
      unrouted = c(TRUE, FALSE, FALSE)
    )
  )
})

test_that("an UNROUTED row raises no ambiguous-name warning", {
  # "distrito federal" is shared across countries in the fixture. A row an
  # UNROUTED rule drops was never offered to the name route, so even with
  # that as its corrected label there is no ambiguity to report.
  rules <- .contract_corrections_692()[1, ]
  rules$correct_label <- "distrito federal"
  expect_no_warning(
    resolved <- .resolve_on_contract(
      "jamaica",
      source = "iia",
      item = "cotton lint",
      year = 1940L,
      corrections = rules
    )
  )
  expect_true(is.na(resolved))
})

test_that("a relabelled row does not keep its misfiled country", {
  # whep-polities #692 clears the row's iso code on relabelling: it came with
  # the misfiled label. Here the reporter is France, and restricting the
  # corrected "saint pierre and miquelon" to French polities would lose it.
  rules <- .contract_corrections_692()
  expect_equal(
    .resolve_on_contract(
      c("france", "saint pierre and miquelon"),
      source = "iia",
      item = "eggs, hen, in shell",
      year = 1940L,
      country = "FRA",
      corrections = rules
    ),
    # The unrelabelled row keeps its `country`, so the guard still applies.
    c("SPM-1816-2025", NA)
  )
})

test_that("a relabelled row is not read as an ISO3 code", {
  # A corrected label is a territory's name. Reading a three-letter one as an
  # ISO3 code would reopen the code route the correction bypasses.
  rules <- .contract_corrections_692()[3, ]
  rules$correct_label <- "jam"
  expect_true(is.na(
    .resolve_on_contract(
      "france",
      source = "iia",
      item = "eggs, hen, in shell",
      year = 1940L,
      corrections = rules
    )
  ))
  # The same label passed by the caller still takes the ISO3 route.
  expect_equal(
    .resolve_on_contract("jam", year = 1940L, corrections = rules),
    "JAM-1800-2025"
  )
})

test_that("overlapping item corrections are an error, not a choice", {
  rules <- .contract_corrections()[c(1, 1), ]
  rules$correct_label[2] <- "cape colony"
  expect_error(
    whep:::.apply_label_item_corrections(
      "south africa",
      source = "mitchell",
      item = "sugar cane",
      year = 1860L,
      rules = rules
    ),
    class = "whep_error_overlapping_label_item_corrections"
  )
})

test_that("the shipped corrections table keeps the published contract", {
  expect_equal(
    names(whep::polity_label_item_corrections),
    c(
      "source",
      "source_label",
      "item",
      "year_start",
      "year_end",
      "correct_label",
      "polity_code",
      "observed_rows",
      "issue",
      "evidence"
    )
  )
  expect_equal(
    setdiff(
      whep::polity_label_item_corrections$polity_code,
      c(whep::polities$polity_code, "UNROUTED")
    ),
    character(0)
  )
})

test_that("the country guard moves nothing on the shipped snapshot", {
  # Measured over every polity name, alias label and ISO3 code at 1850-2025
  # before and after the change: no resolution moved. Pinned here on the
  # package's own caller shapes, so a future re-sync that makes a name shared
  # across countries shows up as a test to read rather than a silent NA.
  expect_no_warning(
    resolve_polity_label(
      unique(whep::mueller_synthetic_n$iso3c),
      source = "mueller-synthetic-n",
      year = 2000L
    )
  )
  expect_equal(
    resolve_polity_label("Netherlands", year = 2000L),
    "NLD-1830-2025"
  )
})
