# FishStat trade preparation (whep#1618) ----------------------------------------
#
# `inst/scripts/prepare_fishstat_trade.R` turns FAO's `FI_Trade` release into
# the `fishstat-trade` pin. Its geography used to be a merge on ISO3 against
# whether `whep::polities` carried an `area_code`, followed by
# `[!is.na(area_code)]`. The pin of 2026-03-30 was built against the 192-row
# table of commit 71ead2ad, which had no row for Bhutan, Comoros, Equatorial
# Guinea, French Guiana, the Marshall Islands, Micronesia, Nauru, Palestine,
# Seychelles, Tonga or Tuvalu; those 11 were added two days later in cf8559be.
# Replaying the script with the older table reproduces all 99,031 pin rows
# exactly, so the 2,946 rows the pin lacks were a missing mapping, not a scope
# decision. The same filter also dropped every reporter with a blank ISO3
# (`Sudan [former]`, `Ethiopia PDR [former]`), and every territory outside that
# table (the Faroe Islands, Greenland, Palau, ...), without a word.
#
# The helpers here keep every source reporter. Each row is resolved to a FAOSTAT
# area code and then to the polity live in its own year, and carries one of
# three `coverage_status` values:
#
# * `adopted`: the polity covers the row's year. Only these reach the CBS.
# * `polity_not_live`: the area resolves, but no polity covers the year.
#   Micronesia 1988-1990 is the case the issue names: FAO says the series
#   "Includes Yap, Truk, Pohnpei and Kosrae" (the four states of the
#   Federated States), while `FSM-1991-2025` starts at UN admission. The rows
#   are kept and named, not handed to the 1991 polity, the Trust Territory or
#   the United States.
# * `area_unresolved`: no FAOSTAT area code represents the reporter.
#
# Source: FAO (2025), FishStat `FI_Trade_2025.1.0.zip`, files
# `CL_FI_COUNTRY_GROUPS.csv` and `Trade_countries_notes.pdf` ("Notes on
# individual countries or areas", pp. 251-254),
# https://www.fao.org/fishery/static/Data/FI_Trade_2025.1.0.zip.

# Build the `fishstat-trade` pin rows from FishStat's raw tables.
#
# `trade` is `TRADE_QUANTITY.csv`, `commodities` is `CL_FI_COMMODITY_ISSCFC.csv`
# (`Code`, `ISSCAAP`), `countries` is `CL_FI_COUNTRY_GROUPS.csv` and `bridge` is
# `fishstat_cbs_bridge.csv`. The commodity bridge is unchanged here; whep#1616
# owns that classification and whep#1617 its weight basis.
#
# Returns a tibble with the legacy pin columns (`Year`, `Area Code`,
# `Item Code`, `Element`, `Value`, `Unit`) plus `Reporter Code` (FishStat's UN
# code), `Polity Code` and `Coverage Status`.
.prepare_fishstat_trade <- function(trade, commodities, countries, bridge) {
  rows <- .fishstat_trade_items(trade, commodities, bridge)
  reporters <- .fishstat_reporter_areas(countries, unique(rows$reporter_code))
  missing <- setdiff(unique(rows$reporter_code), reporters$reporter_code)
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "FishStat trade names reporters the country list does not carry.",
      "x" = "Reporter code{cli::qty(length(missing))}{?s}: {.val {missing}}."
    ))
  }
  rows <- dplyr::left_join(
    rows,
    reporters,
    by = "reporter_code",
    relationship = "many-to-one"
  )
  rows <- .fishstat_resolve_polities(rows)
  .check_fishstat_inclusions(rows)
  .check_fishstat_polity_overlap(rows)
  .check_fishstat_containment(rows)
  .fishstat_trade_pin_rows(rows)
}

# Keep the pin rows whose polity covers their year, and name the rest.
#
# A pin prepared by `.prepare_fishstat_trade()` keeps every source reporter and
# says in `Coverage Status` which rows have a polity. The others stay in the
# pin so they are not lost, and are refused here, by name, on every read: they
# must not reach the CBS until the territory they report has a polity
# (whep#1618). A pin from before that column existed passes through unchanged.
.fishstat_adopted_rows <- function(dt) {
  if (!rlang::has_name(dt, "Coverage Status")) {
    return(dt)
  }
  dt <- data.table::as.data.table(dt)
  held <- dt[dt[["Coverage Status"]] != "adopted"]
  if (nrow(held) > 0L) {
    .warn_fishstat_held_rows(held)
  }
  extra <- intersect(
    c("Reporter Code", "Polity Code", "Coverage Status"),
    names(dt)
  )
  dt <- dt[dt[["Coverage Status"]] == "adopted"]
  dt[, (extra) := NULL]
  dt
}

.warn_fishstat_held_rows <- function(held) {
  by_reporter <- held[,
    .(
      first = min(get("Year")),
      last = max(get("Year")),
      tonnes = sum(get("Value"), na.rm = TRUE)
    ),
    by = c("Reporter Code", "Coverage Status")
  ]
  labels <- paste0(
    "reporter ",
    by_reporter[["Reporter Code"]],
    " ",
    by_reporter$first,
    "-",
    by_reporter$last,
    " (",
    by_reporter[["Coverage Status"]],
    ", ",
    format(round(by_reporter$tonnes), big.mark = ","),
    " t)"
  )
  cli::cli_warn(
    c(
      "FishStat trade rows with no polity in their year are left out.",
      "x" = "{labels}.",
      "i" = "They stay in the pin until the territory has a polity."
    ),
    class = "whep_fishstat_unadopted"
  )
}

# Summarise a prepared pin by reporter and coverage status, for the script's
# report and for tests: one row per (reporter, polity, status) with its year
# span, row count and tonnes.
.fishstat_trade_coverage <- function(pin) {
  pin |>
    dplyr::summarise(
      first_year = min(.data$Year),
      last_year = max(.data$Year),
      rows = dplyr::n(),
      tonnes = sum(.data$Value),
      .by = c("Reporter Code", "Area Code", "Polity Code", "Coverage Status")
    ) |>
    dplyr::arrange(.data$`Coverage Status`, dplyr::desc(.data$tonnes))
}

# Map each FishStat reporter to a FAOSTAT area code.
#
# `Identifier` in `CL_FI_COUNTRY_GROUPS.csv` is FAO's country code, the
# FAOSTAT area code for every reporter but the three in
# `.fishstat_area_overrides()`. Matching on it rather than on ISO3 keeps the
# former entities, whose ISO3 FishStat leaves blank (`Sudan [former]` 206,
# `Ethiopia PDR [former]` 62). Where FishStat does give an ISO3 and FAOSTAT's
# area carries one too, they must agree: a disagreement means the two code
# spaces diverged on that reporter, and the row would be booked to the wrong
# country without changing a single total. Only the reporters that trade are
# checked: the country list also carries entries FishStat numbers in a third
# way (Zanzibar, 836, holds identifier 276, FAOSTAT's Sudan) and reports
# nothing under.
.fishstat_reporter_areas <- function(countries, reporter_codes) {
  overrides <- .fishstat_area_overrides()
  areas <- countries |>
    dplyr::transmute(
      reporter_code = sprintf("%03d", as.integer(.data$UN_Code)),
      reporter_iso3c = dplyr::na_if(.data$ISO3_Code, ""),
      area_code = as.integer(.data$Identifier)
    ) |>
    dplyr::left_join(
      overrides,
      by = c(area_code = "fishstat_identifier"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      area_code = dplyr::coalesce(.data$faostat_area_code, .data$area_code)
    ) |>
    dplyr::filter(.data$reporter_code %in% reporter_codes) |>
    dplyr::select("reporter_code", "reporter_iso3c", "area_code")
  .check_fishstat_area_iso3(areas)
}

# FishStat identifiers that are not the FAOSTAT area code of the same country,
# each verified by FishStat's own ISO3 against the FAOSTAT area's ISO3 in
# `.polity_crosswalk()`: FishStat 286/290/298 carry ISO3 CUW/SSD/SDN, which
# FAOSTAT codes 279/277/276. None of 286, 290 or 298 is a FAOSTAT area.
.fishstat_area_overrides <- function() {
  tibble::tribble(
    ~fishstat_identifier, ~faostat_area_code,
    286L,                 279L,
    290L,                 277L,
    298L,                 276L
  )
}

# Abort when a reporter's ISO3 disagrees with the ISO3 of the FAOSTAT area it
# was mapped to. A code the FAOSTAT vocabulary does not know is left in place:
# it resolves to no polity and is reported as `area_unresolved` downstream.
.check_fishstat_area_iso3 <- function(areas) {
  vocabulary <- .polity_crosswalk(include_unmapped = TRUE) |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$area_iso3c)) |>
    dplyr::distinct(area_code = as.integer(.data$area_code), .data$area_iso3c)
  clash <- areas |>
    dplyr::inner_join(
      vocabulary,
      by = "area_code",
      relationship = "many-to-many"
    )
  clash <- clash |>
    dplyr::filter(
      !is.na(.data$reporter_iso3c),
      .data$reporter_iso3c != .data$area_iso3c
    )
  if (nrow(clash) > 0L) {
    labels <- paste0(
      clash$reporter_code,
      " (",
      clash$reporter_iso3c,
      ") -> area ",
      clash$area_code,
      " (",
      clash$area_iso3c,
      ")"
    )
    cli::cli_abort(
      c(
        "FishStat reporters map to a FAOSTAT area of another country.",
        "x" = "{labels}.",
        "i" = "Add the pair to {.fn .fishstat_area_overrides} once verified."
      ),
      class = "whep_fishstat_area_mismatch"
    )
  }
  areas
}

# Filter to tonnes of imports and exports and attach the CBS item, as the
# original script did. Rows whose commodity has no ISSCAAP group or no bridge
# row are dropped here; that is whep#1616's classification, not geography.
.fishstat_trade_items <- function(trade, commodities, bridge) {
  groups <- commodities |>
    dplyr::transmute(
      fao_code = .data$Code,
      isscaap_group = as.integer(.data$ISSCAAP)
    )
  trade |>
    dplyr::filter(
      .data$MEASURE == "Q_tpw",
      .data$TRADE_FLOW.ALPHA_CODE %in% c("I", "E")
    ) |>
    dplyr::transmute(
      reporter_code = sprintf(
        "%03d",
        as.integer(.data$COUNTRY_REPORTER.UN_CODE)
      ),
      fao_code = .data$COMMODITY.FAO_CODE,
      year = as.integer(.data$PERIOD),
      flow = .data$TRADE_FLOW.ALPHA_CODE,
      value = .data$VALUE
    ) |>
    dplyr::inner_join(groups, by = "fao_code", relationship = "many-to-one") |>
    dplyr::inner_join(
      dplyr::distinct(bridge, .data$isscaap_group, .data$item_cbs_code),
      by = "isscaap_group",
      relationship = "many-to-one"
    )
}

# Resolve every (area code, year) to the polity live in that year, strictly:
# FishStat trade starts in 1976, so no back-cast anchor applies, and a polity
# that has not begun or has ended is reported rather than substituted.
.fishstat_resolve_polities <- function(rows) {
  keys <- rows |>
    dplyr::distinct(.data$area_code, .data$year) |>
    dplyr::filter(!is.na(.data$area_code))
  resolved <- add_polity_code(keys, backcast_anchor = -Inf) |>
    dplyr::transmute(
      .data$area_code,
      .data$year,
      polity_code = .data$polity_code,
      coverage_status = dplyr::case_when(
        is.na(.data$polity_code) ~ "area_unresolved",
        .data$mapping_status %in% c("matched", "manual") ~ "adopted",
        .default = "polity_not_live"
      ),
      polity_code = dplyr::if_else(
        .data$coverage_status == "adopted",
        .data$polity_code,
        NA_character_
      )
    )
  rows |>
    dplyr::left_join(
      resolved,
      by = c("area_code", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      coverage_status = dplyr::coalesce(
        .data$coverage_status,
        "area_unresolved"
      )
    )
}

# Territories FAO's country notes book inside another reporter, with the years
# they are included. A row from the included reporter in those years would be
# counted twice. Source: FAO, `FI_Trade_2025.1.0`, `Trade_countries_notes.pdf`,
# pp. 251-254 ("Trade data for Anguilla are included with those of Saint Kitts
# and Nevis"; France "includes Monaco and ... French Guiana, Guadeloupe,
# Martinique and Réunion from 1996, and Mayotte from 2011"; Luxembourg "Until
# 1999 ... included with those of Belgium"; Curaçao "Until 2010 ... included
# with those of Netherlands Antilles"; Italy "Includes the Republic of San
# Marino"; Switzerland "Includes data for Liechtenstein"; United States
# "Includes data for Puerto Rico"; Sudan and South Sudan "Until 2011 ...
# reported under Sudan (former)"). Reporter codes are FishStat's UN codes;
# `NA` years are open-ended.
.fishstat_included_reporters <- function() {
  tibble::tribble(
    ~included_code, ~container_code, ~from_year, ~to_year,
    "660",          "659",           NA,         NA,
    "492",          "250",           NA,         NA,
    "254",          "250",           1996L,      NA,
    "312",          "250",           1996L,      NA,
    "474",          "250",           1996L,      NA,
    "638",          "250",           1996L,      NA,
    "175",          "250",           2011L,      NA,
    "442",          "056",           NA,         1999L,
    "531",          "530",           NA,         2010L,
    "674",          "380",           NA,         NA,
    "438",          "756",           NA,         NA,
    "630",          "840",           NA,         NA,
    "728",          "736",           NA,         2011L,
    "729",          "736",           NA,         2011L
  )
}

# Abort when a territory reports in a year FAO books it inside another
# reporter. Only rows holding a value count: an empty row adds nothing.
.check_fishstat_inclusions <- function(rows) {
  overlap <- rows |>
    dplyr::filter(!is.na(.data$value), .data$value != 0) |>
    dplyr::distinct(.data$reporter_code, .data$year) |>
    dplyr::inner_join(
      .fishstat_included_reporters(),
      by = c(reporter_code = "included_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      is.na(.data$from_year) | .data$year >= .data$from_year,
      is.na(.data$to_year) | .data$year <= .data$to_year
    )
  if (nrow(overlap) > 0L) {
    labels <- unique(paste0(
      overlap$reporter_code,
      " in ",
      overlap$container_code,
      " (",
      overlap$year,
      ")"
    ))
    cli::cli_abort(
      c(
        "FishStat reports a territory in a year FAO includes it elsewhere.",
        "x" = "{labels}.",
        "i" = "Booking both would count the territory's trade twice."
      ),
      class = "whep_fishstat_double_count"
    )
  }
  invisible(rows)
}

# Abort when two FishStat reporters resolve to one polity in one year. The pin
# keys rows by area code, so the two would be summed into one value. As above,
# only rows holding a value count: FishStat closes `Ethiopia PDR [former]` with
# one zero row in 1992, the first year `Ethiopia` reports, and both resolve to
# `ETH-1952-1993` there.
.check_fishstat_polity_overlap <- function(rows) {
  shared <- rows |>
    dplyr::filter(
      .data$coverage_status == "adopted",
      !is.na(.data$value),
      .data$value != 0
    ) |>
    dplyr::distinct(.data$polity_code, .data$year, .data$reporter_code) |>
    dplyr::filter(dplyr::n() > 1L, .by = c("polity_code", "year"))
  if (nrow(shared) > 0L) {
    labels <- unique(paste0(shared$polity_code, " (", shared$year, ")"))
    cli::cli_abort(
      c(
        "Several FishStat reporters resolve to one polity in one year.",
        "x" = "{labels}."
      ),
      class = "whep_fishstat_double_count"
    )
  }
  invisible(rows)
}

# Reporters whose figures FAO's country notes say EXCLUDE a territory that
# `polity_containment` places inside them: "For statistical purposes, data for
# China do not include Hong Kong Special Administrative Region (Hong Kong SAR),
# Taiwan Province of China and Macao Special Administrative Region (Macao
# SAR)" (`Trade_countries_notes.pdf`, p. 251). FishStat UN codes.
.fishstat_excluded_members <- function() {
  tibble::tribble(
    ~member_code, ~container_code,
    "344",        "156",
    "446",        "156",
    "158",        "156"
  )
}

# Abort when a reporter and a reporter whose polity contains it both carry
# values in one year, unless FAO documents that the container excludes it. The
# CBS sums neither, but the container's polity would then describe ground the
# member's row also reports, which is the double count the polity table exists
# to prevent. Measured on `FI_Trade_2025.1.0`, the only such pairs are Hong Kong
# and Macao inside China (`.fishstat_excluded_members()`).
.check_fishstat_containment <- function(
  rows,
  containment = polity_containment
) {
  live <- rows |>
    dplyr::filter(
      .data$coverage_status == "adopted",
      !is.na(.data$value),
      .data$value != 0
    ) |>
    dplyr::distinct(.data$reporter_code, .data$polity_code, .data$year)
  edges <- tibble::as_tibble(containment) |>
    dplyr::select(
      member_polity = "member_code",
      container_polity = "container_code",
      "start_year",
      "end_year"
    )
  nested <- live |>
    dplyr::inner_join(
      edges,
      by = c(polity_code = "member_polity"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$year >= .data$start_year,
      .data$year < .data$end_year
    ) |>
    dplyr::inner_join(
      dplyr::rename(
        live,
        container_reporter = "reporter_code",
        container_polity = "polity_code"
      ),
      by = c("container_polity", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::anti_join(
      .fishstat_excluded_members(),
      by = c(
        reporter_code = "member_code",
        container_reporter = "container_code"
      )
    )
  if (nrow(nested) > 0L) {
    labels <- unique(paste0(
      nested$polity_code,
      " in ",
      nested$container_polity,
      " (",
      nested$year,
      ")"
    ))
    cli::cli_abort(
      c(
        "A FishStat reporter and its container both report in one year.",
        "x" = "{labels}.",
        "i" = "Check FAO's country notes for whether the container includes it."
      ),
      class = "whep_fishstat_double_count"
    )
  }
  invisible(rows)
}

.fishstat_trade_pin_rows <- function(rows) {
  rows |>
    dplyr::mutate(
      Element = dplyr::if_else(
        .data$flow == "I",
        "Import quantity",
        "Export quantity"
      )
    ) |>
    dplyr::summarise(
      Value = sum(.data$value, na.rm = TRUE),
      .by = c(
        "year",
        "reporter_code",
        "area_code",
        "polity_code",
        "coverage_status",
        "item_cbs_code",
        "Element"
      )
    ) |>
    dplyr::rename(
      Year = "year",
      `Reporter Code` = "reporter_code",
      `Area Code` = "area_code",
      `Polity Code` = "polity_code",
      `Coverage Status` = "coverage_status",
      `Item Code` = "item_cbs_code"
    ) |>
    dplyr::mutate(Unit = "tonnes") |>
    dplyr::arrange(
      .data$Year,
      .data$`Reporter Code`,
      .data$`Item Code`,
      .data$Element
    )
}
