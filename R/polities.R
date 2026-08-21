# Polity helpers ---------------------------------------------------------------

.polity_crosswalk <- function(include_unmapped = TRUE) {
  out <- data.table::as.data.table(polity_area_crosswalk)
  if (!include_unmapped) {
    out <- out[!is.na(polity_code)]
  }
  out <- .unfold_rest_of_world(out)
  data.table::copy(out)
}

.current_area_lookup <- function(include_unmapped = TRUE) {
  out <- .polity_crosswalk(include_unmapped = include_unmapped)
  out <- out[!is.na(area_code)]
  # A period is "open" when it runs to the latest end year present in the
  # crosswalk (the open-period sentinel). Derive it from the data rather than
  # hardcoding a literal year, so it tracks future data extensions.
  open_end_year <- max(out$polity_end_year, na.rm = TRUE)
  out[,
    `:=`(
      has_polity = !is.na(polity_code),
      is_current = !is.na(polity_end_year) & polity_end_year >= open_end_year
    )
  ]
  data.table::setorderv(
    out,
    c(
      "area_code",
      "has_polity",
      "is_current",
      "polity_end_year",
      "polity_start_year"
    ),
    order = c(1L, -1L, -1L, -1L, -1L),
    na.last = TRUE
  )
  out <- unique(out, by = "area_code")
  out[, c("has_polity", "is_current") := NULL]
  out
}

# Crown dependencies and overseas territories (JEY, GGY, IMN, ALA, BLM, SXM)
# sit in the crosswalk with their sovereign's `polity_code` but with no
# FAOSTAT `area_code`, so `.current_area_lookup()`'s `!is.na(area_code)` filter
# cannot see them and any source keyed by ISO3 silently loses their rows.
# This returns, for each such ISO3, the ISO3 of a territory that shares its
# polity and does have an aggregation bucket, i.e. its sovereign.
.dependency_sovereign_iso3 <- function() {
  sovereign <- .current_area_lookup(include_unmapped = FALSE)[
    !is.na(area_iso3c),
    .(polity_code, sovereign_iso3c = area_iso3c, area_code)
  ]
  # Keep the resolution deterministic where one polity spans several buckets
  # (ROW, and the Sudan/Ethiopia splits) by taking the lowest area code.
  data.table::setorderv(sovereign, c("polity_code", "area_code"))
  sovereign <- unique(sovereign, by = "polity_code")
  sovereign[, area_code := NULL]

  dependency <- .polity_crosswalk(include_unmapped = FALSE)[
    is.na(area_code) & !is.na(area_iso3c),
    .(iso3c = area_iso3c, polity_code, polity_start_year, polity_end_year)
  ]
  # One crosswalk row per polity period; take the most recent, which is how
  # `.current_area_lookup()` picks a code's current polity.
  data.table::setorderv(
    dependency,
    c("iso3c", "polity_end_year", "polity_start_year"),
    order = c(1L, -1L, -1L),
    na.last = TRUE
  )
  dependency <- unique(dependency, by = "iso3c")

  out <- merge(dependency, sovereign, by = "polity_code", sort = FALSE)
  out <- out[sovereign_iso3c != iso3c, .(iso3c, sovereign_iso3c)]
  data.table::setorderv(out, "iso3c")
  out
}

# Exclusive upper bound of the years a crosswalk period covers, i.e. the period
# runs `polity_start_year:(polity_end_year - 1)`.
#
# `polity_end_year` is EXCLUSIVE. That is what `whep-polities` publishes -- a
# successor's `start_year` equals its predecessor's `end_year` (F51-1947-1993
# hands over to CZE-1993-2025 and SVK-1993-2025; SUD-1956-2011 to SDN-2011-2025
# and SSD-2011-2025), and 240 of the 245 FAOSTAT-map rows in the crosswalk carry
# `polity_end_year == map_year_end + 1L` against an inclusive `map_year_end`.
# It is also what `.area_year_polity_conflicts()`, `.polity_area_years()`,
# `resolve_polity_label()` and the crosswalk build already compute with.
#
# `map_year_end` is the last year upstream declares the reporting area reports
# under this period, inclusive, and the map is the authority on reporting years.
# In the four rows where it reaches past the territorial span it wins, so a
# reported year is never dropped for being one past a polity's end: four areas
# whose last reported year equals `polity_end_year` (15 Belgium-Luxembourg 1999,
# 151 Netherlands Antilles 2010, 206 Sudan (former) 2011, 228 USSR 1991) keep it,
# while the areas whose map span stops earlier (51 Czechoslovakia 1993, 186
# Serbia and Montenegro 2006, 248 Yugoslav SFR 1992) no longer answer for a year
# their polity had already ended in.
.polity_join_end_year <- function(polity_end_year, map_year_end, is_open) {
  territorial <- data.table::fifelse(
    is.na(polity_end_year),
    Inf,
    as.numeric(polity_end_year)
  )
  # EXCLUSIVE AT A SUCCESSION, INCLUSIVE AT THE OPEN END.
  #
  # A boundary between two epochs belongs to the successor, which is what the
  # exclusive reading buys. But a still-open interval has nothing after it, so
  # there is no double-count to prevent and excluding its terminal year simply
  # deletes a year. Measured on the shipped snapshot: 228 live polities end at
  # 2025 and a strictly exclusive rule left NONE of them covering 2025, so every
  # current-year row degraded from `matched` to `out_of_span` -- resolved only by
  # the nearest-period fallback, which is the pathology this epic removes.
  #
  # Openness is detected by ABSENCE OF A SUCCESSOR, not by comparing the end year
  # to the table maximum. The year test re-introduces the double-count for any
  # polity whose last interval ends at the maximum AND has a successor, and the
  # maximum itself moves (#530 took the table from 740 rows to 749). Measured:
  # 242 live polities are open, 228 of them end at 2025, and ZERO live polities
  # ending at 2025 are succeeded -- so the two agree today and the successor
  # test is the one that keeps agreeing.
  # `fifelse()` will not recycle its test, so a scalar `is_open` (which is what a
  # caller testing one period naturally passes) has to be widened here.
  is_open <- rep_len(as.logical(is_open), length(territorial))
  territorial <- data.table::fifelse(is_open, territorial + 1, territorial)
  reported <- data.table::fifelse(
    is.na(map_year_end),
    -Inf,
    as.numeric(map_year_end) + 1
  )
  pmax(territorial, reported)
}

# Which polity codes upstream declares nothing succeeds. Read from `polities`
# rather than the crosswalk because succession is a fact about the polity, and
# the crosswalk does not carry the relation.
#
# READ IN BOTH DIRECTIONS, because upstream fills the two sides of that relation
# independently. `AGO-1975-2025` names `ANG-1905-1975` as its predecessor while
# `ANG-1905-1975` names no successor, so the forward column alone calls colonial
# Angola open, `.polity_join_end_year()` widens it into 1975 -- the year
# `AGO-1975-2025` starts -- and FAOSTAT area 7 gets two candidates for 1975,
# decided by row order (#683). Reading `predecessor` as well is not a second
# authority on succession, which is what the paragraph above argues against: it
# is the same upstream record, read symmetrically.
.open_polity_codes <- function() {
  p <- polities
  succ <- p$successor
  open <- is.na(succ) | !nzchar(trimws(succ))
  setdiff(unique(p$polity_code[open]), .handed_over_polity_codes())
}

# Periods some other period is recorded as taking over from, AT THEIR END YEAR.
#
# THE YEAR TEST IS LOAD-BEARING, because `predecessor` records two different
# relations. One is a hand-over, where the predecessor stops: `AGO-1975-2025`
# from `ANG-1905-1975`, `BMU-1968-2025` from `BMU-1684-1968`, `REU-1946-2025`
# from `REU-1816-1946`. The other is a partial derivation, where a piece was
# carved out and the predecessor went on existing: `TRS-1947-1954` names
# `ITA-1919-2025`, `SYC-1903-2025` names `MUS-1800-2025`, `SWE-1905-2025` names
# `NOR-1800-2025`. Only the first ends the period, and requiring the successor
# to BEGIN where the predecessor ENDS (`polity_end_year` is exclusive, #577) is
# what separates them.
#
# Measured on the shipped snapshot: 8 codes are named as somebody's predecessor
# while recording no successor of their own, and exactly 3 begin-at-end --
# `ANG-1905-1975`, `BMU-1684-1968`, `REU-1816-1946`, all genuine hand-overs.
# Dropping the year test closes the other 5 as well, and 5 FAOSTAT areas then
# lose 2025 to the nearest-period fallback: the exact regression the widening
# exists to prevent.
#
# The `inner_join()` below is keyed on `polity_code` with no `year`, so it is
# registered in `.territorial_join_baseline()`. It is written as a join, and the
# renaming spelled in `by =` rather than in an upstream `select()`, so the audit
# in `R/join_audit.R` can see it: hiding it would be the debt that gate exists
# to stop.
.handed_over_polity_codes <- function(periods = polities) {
  # Column by column rather than `as.data.frame()`: `polities` is an `sf`
  # object, and materialising it whole to drop the geometry costs more than
  # everything else here put together, on a helper the resolver calls once per
  # lookup build.
  p <- tibble::tibble(
    polity_code = periods$polity_code,
    start_year = periods$start_year,
    end_year = periods$end_year,
    predecessor = periods$predecessor
  )
  named <- p |>
    dplyr::select("polity_code", "start_year", "predecessor") |>
    dplyr::filter(!is.na(.data$predecessor), nzchar(.data$predecessor)) |>
    tidyr::separate_longer_delim("predecessor", delim = ";") |>
    dplyr::mutate(predecessor = stringr::str_trim(.data$predecessor)) |>
    dplyr::filter(nzchar(.data$predecessor))

  named |>
    dplyr::inner_join(
      dplyr::select(p, "polity_code", "end_year"),
      by = c("predecessor" = "polity_code")
    ) |>
    dplyr::filter(.data$end_year == .data$start_year) |>
    dplyr::pull("predecessor") |>
    unique()
}

# Resolve an (ISO3 area label, data year) pair to the polity code active in that
# year, against the polity's own span in `polity_area_crosswalk`.
#
# This is what `data-raw/balance_coefficients.R` stamps `urban_n_reference` with.
# It lives here rather than in the builder because the year predicate IS the
# package-wide `polity_end_year` convention, and that convention had four
# independent re-implementations, three of which read the bound inclusively
# (#550, #577). The builder's copy was the fourth and the only silent one: on a
# boundary year it answered with the interval that had ENDED on it, so a
# coefficient was attributed to a polity that no longer existed, with no error
# and no warning (#565). Here it is one definition with one test.
#
# Deliberately NOT `add_polity_code()`: a vendored national series reports each
# benchmark year under the borders that year actually had, while WHEP's pre-1961
# FAOSTAT series are back-cast onto the 1961 anchor territory and need that
# resolver's floor. Deliberately not `resolve_polity_label()` either -- that
# answers `NA` on ambiguity, and a builder writing packaged data must stop rather
# than ship a row whose territory it could not decide.
.iso3_year_to_polity_code <- function(
  iso3,
  year,
  crosswalk = polity_area_crosswalk,
  open_codes = .open_polity_codes()
) {
  spans <- .iso3_polity_spans(crosswalk, open_codes, iso3)
  found <- purrr::map2(
    iso3,
    year,
    function(one_iso3, one_year) {
      spans$polity_code[
        spans$area_iso3c == one_iso3 &
          spans$from_year <= one_year &
          spans$to_year > one_year
      ]
    }
  )
  .abort_polity_year_misses(found, paste(iso3, year))
  unlist(found, use.names = FALSE)
}

# The candidate periods of the named ISO3 areas, with the half-open year bounds
# the containment test above uses.
.iso3_polity_spans <- function(crosswalk, open_codes, iso3) {
  crosswalk |>
    dplyr::filter(.data$area_iso3c %in% iso3, !is.na(.data$polity_code)) |>
    dplyr::distinct(
      .data$area_iso3c,
      .data$polity_code,
      .data$polity_start_year,
      .data$polity_end_year
    ) |>
    dplyr::mutate(
      # A missing start bound is unbounded below. The upper bound is EXCLUSIVE at
      # a succession and INCLUSIVE at an open end, which is exactly what
      # `.polity_join_end_year()` encodes for the main resolver; no `map_year_end`
      # is passed because this series is keyed to its real historical borders,
      # not to a FAOSTAT reporting area's declared years.
      from_year = dplyr::coalesce(as.numeric(.data$polity_start_year), -Inf),
      to_year = .polity_join_end_year(
        .data$polity_end_year,
        NA_integer_,
        .data$polity_code %in% open_codes
      )
    )
}

# A build error, not a fallback: a re-dated or split territory has to be looked
# at rather than resolved by whichever candidate sorted first.
.abort_polity_year_misses <- function(found, labels) {
  unresolved <- labels[lengths(found) == 0]
  if (length(unresolved) > 0) {
    cli::cli_abort(c(
      "Cannot resolve an ISO3 area label and year to a polity code.",
      "x" = "No polity active in polity_area_crosswalk: {.val {unresolved}}."
    ))
  }
  ambiguous <- labels[lengths(found) > 1]
  if (length(ambiguous) > 0) {
    cli::cli_abort(c(
      "An ISO3 area label and year map to more than one polity.",
      "x" = "Ambiguous: {.val {ambiguous}}.",
      "i" = "Label the source rows with the polity they cover."
    ))
  }
  invisible(NULL)
}

# Which stand-in a row gets when NO mapped period covers its (anchored) year.
#
# `"forward"` (default) prefers a period that has NOT STARTED yet over one that
# has already ENDED; `"nearest"` is the pre-#705 behaviour, pure year distance.
# Both then break ties on distance and on the earlier start.
#
# Distance alone splits one reporting area's series between two entities at the
# year the arithmetic happens to flip, with nothing in the data marking the
# break. FAOSTAT area 178 Eritrea resolved 1850-1972 to `ERI-1889-1952`, the
# Italian colonial administration, and 1973-1992 to `ERI-1993-2025` -- 1973 is
# simply where 1993 gets nearer than 1952. Area 273 Montenegro split at 1961
# between `MNE-1913-1918` and `MNE-2006-2025`, and that one turned on a SINGLE
# year: 1961 - 1918 + 1 = 44 against 2006 - 1961 = 45.
#
# Preferring the not-yet-started period keeps each area on one entity and keeps
# the back-cast anchor's own intent, which is to map pre-anchor data to the
# 1961 territory "instead of a larger historical-extent period". When no period
# covers 1961 the nearest one behind it is exactly such a historical-extent
# period. It also makes the two areas consistent with the other 22 that have no
# period at the anchor -- the post-Soviet and post-Yugoslav areas, whose only
# period starts in 1991/1992 and which therefore already resolve forward.
#
# Measured over the whole crosswalk x 1850-2025: 235 of 46,640 (area, year)
# pairs change, all of them areas 178 and 273. See whep#705.
.polity_stand_in_mode <- function() {
  valid <- c("forward", "nearest")
  mode <- getOption("whep.polity_stand_in", "forward")
  if (!rlang::is_string(mode) || !mode %in% valid) {
    cli::cli_abort(c(
      "{.code options(whep.polity_stand_in)} must be one of {.val {valid}}.",
      "x" = "It is {.val {mode}}."
    ))
  }
  mode
}

.order_stand_in_matches <- function(matches, rowid_col) {
  keys <- if (identical(.polity_stand_in_mode(), "forward")) {
    c(rowid_col, "stand_in_ended", "year_distance", "join_start_year")
  } else {
    c(rowid_col, "year_distance", "join_start_year")
  }
  data.table::setorderv(
    matches,
    keys,
    order = rep(1L, length(keys)),
    na.last = TRUE
  )
}

# Say when a row's polity was resolved AT THE ANCHOR rather than at its own
# year, instead of reporting it as `"matched"`.
#
# The floor below is the honest label for a back-cast VALUE: a pre-anchor row is
# the anchor year's tonnage walked backwards, so the anchor year's territory is
# what it describes. What is not honest is the STATUS. `"matched"` is a claim
# about the year -- "the year fell inside this polity's period" -- and for
# 12,208 of the 29,415 `(area, year)` cells of 1850-1960 x the crosswalk's 265
# resolving reporting areas it is false. FAOSTAT area 238's 1850 row reads
# `ETH-1952-1993`, `matched`, 102 years before that polity began.
#
# The floor is applied BEFORE the span check, so `polity_coverage_gaps()` -- the
# instrument built to audit exactly this -- resolved such a row as 1961 and came
# back clean: 9,544 of those cells were invisible to it (whep#763).
#
# Only the rows the anchor actually moved are marked. A floored row whose anchor
# polity DOES cover its own year keeps `"matched"`, which is 125 of the 265
# areas for all 111 back-cast years. `"out_of_span"` outranks this: it says no
# period covered even the ANCHORED year, which is the stronger statement and the
# one the diagnostic already reported (2,664 of the 12,208).
.mark_backcast_anchor_status <- function(map, data_years, rowid_col) {
  map[data_years, on = rowid_col, "data_year" := i.data_year]
  map[
    !is.na(mapping_status) &
      mapping_status != "out_of_span" &
      !is.na(polity_start_year) &
      !is.na(data_year) &
      data_year < polity_start_year,
    mapping_status := "backcast_anchor"
  ]
  map[, "data_year" := NULL]
  map
}

.add_polity_columns_dt <- function(
  data,
  code_col = "area_code",
  year_col = "year",
  prefix = "",
  include_unmapped = FALSE,
  backcast_anchor = 1961L
) {
  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }
  dt <- data.table::copy(data)

  if (!code_col %in% names(dt)) {
    cli::cli_abort("Column {.field {code_col}} is required for polity mapping.")
  }

  base_cols <- c(
    "area_name",
    "area_iso3c",
    "polity_area_code",
    "polity_code",
    "polity_name",
    "polity_start_year",
    "polity_end_year",
    "mapping_status",
    "has_geometry"
  )
  mapped_cols <- paste0(prefix, base_cols)
  old_cols <- intersect(mapped_cols, names(dt))
  if (length(old_cols) > 0) {
    dt[, (old_cols) := NULL]
  }

  rowid_col <- "..whep_polity_rowid"
  dt[, (rowid_col) := .I]

  if (!is.null(year_col) && year_col %in% names(dt)) {
    lookup <- .polity_crosswalk(include_unmapped = include_unmapped)
    lookup <- lookup[!is.na(area_code)]
    # A caller-supplied or mocked crosswalk need not carry the upstream map's
    # reporting years; without them the territorial span is the only bound.
    if (!rlang::has_name(lookup, "map_year_end")) {
      lookup[, "map_year_end" := NA_integer_]
    }
    lookup <- lookup[,
      c(
        "area_code",
        "join_start_year",
        "join_end_year",
        base_cols,
        "lookup_polity_type"
      ) := .(
        area_code,
        data.table::fifelse(
          is.na(polity_start_year),
          -Inf,
          as.numeric(polity_start_year)
        ),
        .polity_join_end_year(
          polity_end_year,
          get("map_year_end"),
          polity_code %in% .open_polity_codes()
        ),
        area_name,
        area_iso3c,
        polity_area_code,
        polity_code,
        polity_name,
        polity_start_year,
        polity_end_year,
        mapping_status,
        has_geometry,
        get("polity_type")
      )
    ][,
      c(
        "area_code",
        "join_start_year",
        "join_end_year",
        base_cols,
        "lookup_polity_type"
      ),
      with = FALSE
    ]

    # WHEP's pre-1962 series are NOT reported under their data-year borders:
    # they are back-cast from the first reported FAOSTAT year (~1961) onto that
    # year's territory. So a 1900 "Austria" figure represents 1961 Austria, not
    # the 1900 Habsburg crownland. Floor the polity-lookup year at the anchor so
    # pre-anchor data maps to the entity active in 1961 (e.g. AUT-1919-2025, the
    # modern republic; USSR/Yugoslavia/Czechoslovakia for entities that only
    # dissolved AFTER 1961) instead of a larger historical-extent period.
    # Genuine historical-source data (reported under real historical borders) is
    # handled separately, keyed directly to its polity, not via this lookup.
    join_data <- dt[,
      .(
        ..whep_polity_rowid = get(rowid_col),
        area_code = get(code_col),
        year = pmax(as.numeric(get(year_col)), as.numeric(backcast_anchor))
      )
    ]
    matches <- lookup[
      join_data,
      on = .(
        area_code,
        join_start_year <= year,
        join_end_year > year
      ),
      allow.cartesian = TRUE
    ]
    # Prefer the most recent applicable period. An `exact_start` tiebreak used
    # to sit here but was dead code: data.table's non-equi join overwrites
    # `join_start_year` with the query year, so `polity_start_year ==
    # join_start_year` degraded to `== year` and never changed the pick. The
    # `polity_start_year DESC` order below is what actually decides ties.
    data.table::setorderv(
      matches,
      c("..whep_polity_rowid", "polity_start_year"),
      order = c(1L, -1L),
      na.last = TRUE
    )
    matches <- unique(matches, by = "..whep_polity_rowid")
    map <- matches[, c("..whep_polity_rowid", base_cols), with = FALSE]
    fallback_rowids <- map[is.na(polity_code), get(rowid_col)]
    if (length(fallback_rowids) > 0L) {
      fallback_data <- join_data[
        get(rowid_col) %in% fallback_rowids & !is.na(area_code)
      ]
      fallback_matches <- lookup[
        fallback_data,
        on = "area_code",
        allow.cartesian = TRUE
      ]
      # Do not silently extend dataset-specific aggregate reporting areas.
      #
      # An earlier revision of this branch DID extend them, to stop aggregates
      # whose period was cut short from losing their most-recent years -- area
      # 904 "Latin America Other" reaching only 2013 because `RLAM-1850-2013`
      # ends there, while FAOSTAT keeps reporting it. That was the wrong place to
      # fix it, for two measured reasons.
      #
      # It is an upstream data defect, and upstream has fixed it: all seven
      # reporting buckets now run to 2025 ("Extend the seven reporting buckets to
      # 2025 so the data they exist for resolves"), so `RLAM-1850-2013` becomes
      # `RLAM-1850-2025` and the six `-1850-2021` buckets likewise. The
      # short-period vintage this package still embeds is what makes the symptom
      # visible; the re-sync in #470 removes the cause. A territorial validity
      # span is upstream's fact, not something to paper over on read.
      #
      # And extending on nearest-distance is not symmetric in a safe way. It
      # back-fills years BEFORE an aggregate's start as readily as after its end,
      # which attributes a figure to a bucket that did not exist: an 1830
      # Guadeloupe row would be booked to `ROW-1850-2023`. That is 64 rows /
      # 1,722,000 t in the historical trade feed, and dropping them rather than
      # back-filling is deliberate -- see `test_build_cbs.R`'s
      # `.resolve_hist_trade_polities drops pre-range aggregate rows`.
      fallback_matches <- fallback_matches[
        !is.na(polity_code) & get("lookup_polity_type") != "aggregate"
      ]
      if (nrow(fallback_matches) > 0L) {
        # `join_end_year` is the exclusive upper bound, so the last year a
        # period covers is `join_end_year - 1` and a row at `join_end_year`
        # itself is already one year past it.
        fallback_matches[,
          "year_distance" := data.table::fcase(
            year < join_start_year   ,
            join_start_year - year   ,
            year >= join_end_year    ,
            year - join_end_year + 1 ,
            default = 0
          )
        ]
        fallback_matches[,
          "stand_in_ended" := as.integer(!(year < join_start_year))
        ]
        .order_stand_in_matches(fallback_matches, rowid_col)
        fallback_matches <- unique(
          fallback_matches,
          by = "..whep_polity_rowid"
        )
        fallback_map <- fallback_matches[,
          c("..whep_polity_rowid", base_cols),
          with = FALSE
        ]
        # Every row reaching here failed the span join, so the period the
        # fallback lands on does NOT contain the (anchored) year: the polity did
        # not exist then. FAOSTAT area 206 "Sudan (former)" in 1970 lands on
        # SDN-2011-2025, post-secession Sudan, which by definition excludes the
        # territory the 1970 figure covers. Copying the crosswalk's "matched" or
        # "manual" status made that indistinguishable from a real period hit, so
        # the misattribution was invisible rather than merely uncertain. Over the
        # FAOSTAT era, 993 of 16638 resolved area-years across 36 areas are such
        # nearest-period stand-ins, in both directions: pre-independence years
        # (Sudan 1961-2010) and post-dissolution years (Czechoslovakia 1994-2023
        # on F51-1947-1993). Report the substitution instead of hiding it.
        fallback_map[, mapping_status := "out_of_span"]
        data.table::setkeyv(map, rowid_col)
        data.table::setkeyv(fallback_map, rowid_col)
        for (col in base_cols) {
          map[fallback_map, (col) := get(paste0("i.", col))]
        }
        data.table::setkey(map, NULL)
      }
    }
    map <- .mark_backcast_anchor_status(
      map,
      dt[,
        .(
          ..whep_polity_rowid = get(rowid_col),
          data_year = as.numeric(get(year_col))
        )
      ],
      rowid_col
    )
  } else {
    lookup <- .current_area_lookup(include_unmapped = include_unmapped)
    lookup <- lookup[, c("area_code", base_cols), with = FALSE]
    join_data <- dt[,
      .(
        ..whep_polity_rowid = get(rowid_col),
        area_code = get(code_col)
      )
    ]
    map <- lookup[join_data, on = "area_code"]
    map <- map[, c("..whep_polity_rowid", base_cols), with = FALSE]
  }

  data.table::setnames(map, base_cols, mapped_cols)
  out <- merge(dt, map, by = rowid_col, all.x = TRUE, sort = FALSE)
  data.table::setorderv(out, rowid_col)
  out[, (rowid_col) := NULL]
  out
}

#' Add WHEP polity codes to a table
#'
#' @description
#' Adds periodized `polity_code` information from [polity_area_crosswalk] to
#' a table with FAOSTAT/FABIO `area_code` values. If a `year` column is
#' present, the mapping is year-aware; otherwise the current/default mapping
#' is used.
#'
#' When no mapped period covers a row's year, another period of the same area
#' is used as a stand-in and `mapping_status` reports `"out_of_span"` rather
#' than the crosswalk's `"matched"`/`"manual"`. Such a row is attributed to a
#' polity that did not exist in that year, so treat it as a coverage gap:
#' either the area needs the missing period added to the crosswalk, or the
#' reporting area outlived (or predates) every polity mapped to it.
#'
#' @section The status vocabulary:
#' `mapping_status` here is a property of resolving one `(area_code, year)`,
#' **not** the same column as [polity_area_crosswalk]'s, which is a property of
#' a published crosswalk row (whep#637). The resolver carries the selected
#' crosswalk row's own status through, and overwrites it wherever the resolution
#' substituted something for a real period hit:
#'
#' - `"matched"` / `"manual"`: the year fell inside the polity's period, and the
#'   value is the crosswalk row's own provenance, carried through.
#' - `"backcast_anchor"`: the row is before `backcast_anchor`, so it was
#'   resolved at the anchor year, and the polity live then is **not** live in
#'   the row's own year. That polity is still the honest label -- the value is a
#'   reconstruction on the anchor year's territory -- but the row is no evidence
#'   the polity existed then, which is exactly what `"matched"` asserts.
#'   FAOSTAT area 238 reads `ETH-1952-1993` from 1850, 102 years before that
#'   polity began: `"backcast_anchor"` for 1850-1951, `"matched"` from 1952. A
#'   pre-anchor row whose anchor polity *does* cover its own year keeps
#'   `"matched"`.
#' - `"out_of_span"`: no mapped period covered even the anchored year, so a
#'   nearest-period stand-in was used.
#' - `"unmapped"`, or `NA`: no polity at all, carried through from the crosswalk
#'   or left by an area with no applicable period. `polity_code` is `NA` too.
#'
#' `"backcast_anchor"` and `"out_of_span"` exist only here, so a tibble carrying
#' either is unambiguously this column and not the crosswalk's. The two still
#' overlap in `"matched"`, `"manual"` and `"unmapped"`, which is whep#637 and is
#' not resolved here.
#'
#' @section Which stand-in is picked:
#' A period that has **not started yet** is preferred over one that has
#' already **ended**, and only then is the nearest in years taken. Ranking by
#' distance alone split one reporting area's series between two entities at
#' whichever year the arithmetic flipped: FAOSTAT area 178 Eritrea read
#' `ERI-1889-1952` (the Italian colonial administration) up to 1972 and
#' `ERI-1993-2025` from 1973, and area 273 Montenegro split at 1961 between
#' `MNE-1913-1918` and `MNE-2006-2025` on a one-year margin. Preferring the
#' not-yet-started period keeps each area on one entity and agrees with the
#' back-cast anchor, whose purpose is to avoid resolving back-cast rows onto a
#' larger historical-extent period. Set
#' `options(whep.polity_stand_in = "nearest")` to restore ranking by distance
#' alone; it changes 235 of the crosswalk's 46,640 `(area, year)` pairs over
#' 1850-2025, all of them areas 178 and 273 (whep#705).
#'
#' @param table A data frame.
#' @param code_column Name of the column containing numeric area codes.
#' @param year_column Name of the column containing years. Set to `NULL` to
#'   force current/default mapping.
#' @param polity_code_column Name of the output polity-code column.
#' @param backcast_anchor First year of reported (non-back-cast) FAOSTAT data,
#'   default `1961`. Years before it are matched to the polity active in the
#'   anchor year, because WHEP's pre-anchor series are back-cast onto the
#'   anchor-year territory rather than reported under their data-year borders.
#'   Such a row reports `mapping_status == "backcast_anchor"` where the anchor
#'   polity is not live in its own year. Set to `-Inf` to disable and match
#'   strictly by data year.
#'
#' @returns A tibble with added polity metadata columns.
#' @seealso [polity_coverage_gaps()], which reports the `"out_of_span"` and
#'   `"backcast_anchor"` rows of an already-built table, whose published columns
#'   no longer carry `mapping_status`.
#' @export
#'
#' @examples
#' # The same area code resolves to different polities in different years:
#' # area 16 reports as East Pakistan before 1971 and as Bangladesh after it.
#' tibble::tibble(area_code = c(16L, 16L), year = c(1965L, 2000L)) |>
#'   add_polity_code() |>
#'   dplyr::select(area_code, year, polity_code, polity_name, mapping_status)
#'
#' # Without a year column the current/default mapping is used.
#' add_polity_code(tibble::tibble(area_code = 231L), year_column = NULL) |>
#'   dplyr::select(area_code, polity_code, polity_name)
add_polity_code <- function(
  table,
  code_column = "area_code",
  year_column = "year",
  polity_code_column = "polity_code",
  backcast_anchor = 1961L
) {
  dt <- data.table::as.data.table(table)
  year_col <- if (!is.null(year_column) && year_column %in% names(dt)) {
    year_column
  } else {
    NULL
  }
  out <- .add_polity_columns_dt(
    dt,
    code_col = code_column,
    year_col = year_col,
    include_unmapped = TRUE,
    backcast_anchor = backcast_anchor
  )

  if (polity_code_column != "polity_code" && "polity_code" %in% names(out)) {
    data.table::setnames(out, "polity_code", polity_code_column)
  }
  tibble::as_tibble(out)
}

#' Find rows attributed to a polity not live in the row's year
#'
#' @description
#' [add_polity_code()] reports these rows in `mapping_status`, but WHEP's
#' published outputs do not carry that column: `reporting_polity_code` and
#' `reporting_polity_name` say which polity a row was attributed to, and nothing
#' says the polity did not exist in that row's year. This answers that question
#' for a table that has already been built, so a consumer joining on
#' `reporting_polity_code` can tell a real period hit from the two kinds of
#' substitute without re-deriving the crosswalk.
#'
#' Neither kind is an error and no row is dropped. It means the polygon,
#' population and period of the returned polity describe a different year than
#' the value does, so `gap_kind` names which kind a row is:
#'
#' - `"backcast_anchor"`: the row is before `backcast_anchor` and its polity was
#'   resolved at the anchor year, which is WHEP's own back-cast convention --
#'   pre-1961 series are reconstructions on the anchor year's territory, so a
#'   Soviet republic's 1900 land is booked to the republic that reports it
#'   today. The polity was matched at the anchor and simply had not begun by the
#'   row's own year.
#' - `"polity_not_started"`: no mapped period covered even the anchored year and
#'   the stand-in taken begins after it.
#' - `"polity_ended"`: the polity had ended, so the value covers a territory
#'   that entity no longer describes. This is the harder case, and the one
#'   whep#414 is about: FAOSTAT areas 276 Sudan and 277 South Sudan fold into
#'   bucket 206, whose label `SUD-1956-2011` ended at the secession, and no live
#'   polity means "Sudan and South Sudan".
#'
#' `gap_kind` is not derivable from the returned columns, which is why it is
#' returned rather than left to the caller. `"backcast_anchor"` is not visible
#' in the years at all -- the resolver matched a real period, at the anchor --
#' and the direction of the other two is read at the year the resolver actually
#' matched on, which the back-cast anchor floors at `backcast_anchor`, so a
#' pre-anchor row is classified as the anchor year it was resolved as rather
#' than as the year it carries.
#'
#' Measured on a real full-range `get_primary_production()`: 2,301 `(area,
#' year)` pairs / 7,247 rows are stand-ins, and the back-cast class adds 9,544
#' pairs the floor previously hid from this function entirely (whep#763).
#'
#' The resolution here is the same one the builds use, including the back-cast
#' anchor, so it reports what the table actually got rather than a second
#' reading of the crosswalk. The area column may hold either a FAOSTAT
#' `area_code` or the `polity_area_code` bucket that published outputs are keyed
#' by; both resolve through the same lookup.
#'
#' @param table A data frame carrying an area-code column, and a year column if
#'   the resolution is to be year-aware.
#' @param code_column Name of the column holding numeric area codes.
#' @param year_column Name of the column holding years. Set to `NULL`, or leave
#'   it absent from `table`, to use the current/default mapping, which has no
#'   gaps by construction.
#' @param backcast_anchor First year of reported (non-back-cast) FAOSTAT data;
#'   passed to the same resolution [add_polity_code()] documents.
#'
#' @returns A tibble with one row per reported `(area_code, year)`, ordered by
#'   area code and year, carrying `area_code`, `year`, `polity_code`,
#'   `polity_name`, `polity_start_year`, `polity_end_year`, `gap_kind`
#'   (`"backcast_anchor"`, `"polity_not_started"` or `"polity_ended"`) and
#'   `n_rows`, the number of rows of `table` that pair carries. Zero rows means
#'   every row of `table` landed inside its polity's period, which is the
#'   intended state.
#'
#' @seealso [add_polity_code()] for the resolution itself, and
#'   [polity_bucket_coverage()] for the related question of which buckets sum
#'   more than one territory, and whether their label covers the sum.
#' @export
#' @examples
#' # FAOSTAT area 206 "Sudan (former)" is the live case: it keeps reporting
#' # after `SUD-1956-2011` ends, so post-2011 rows are stand-ins. Area 238's
#' # 1850 row is the back-cast case: `ETH-1952-1993` labels it because that is
#' # the polity live at the anchor, 102 years later.
#' polity_coverage_gaps(
#'   tibble::tibble(
#'     area_code = c(206L, 206L, 238L),
#'     year = c(2005L, 2015L, 1850L),
#'     value = 1
#'   )
#' )
polity_coverage_gaps <- function(
  table,
  code_column = "area_code",
  year_column = "year",
  backcast_anchor = 1961L
) {
  dt <- data.table::as.data.table(table)
  if (!rlang::has_name(dt, code_column)) {
    cli::cli_abort(
      "Column {.field {code_column}} is required for {.arg table}."
    )
  }
  year_col <- if (!is.null(year_column) && rlang::has_name(dt, year_column)) {
    year_column
  } else {
    NULL
  }

  resolved <- .add_polity_columns_dt(
    dt[, c(code_column, year_col), with = FALSE],
    code_col = code_column,
    year_col = year_col,
    include_unmapped = TRUE,
    backcast_anchor = backcast_anchor
  )
  data.table::setnames(resolved, code_column, "area_code")
  if (is.null(year_col)) {
    resolved[, year := NA_integer_]
  } else if (year_col != "year") {
    data.table::setnames(resolved, year_col, "year")
  }

  resolved[
    !is.na(mapping_status) & mapping_status %in% .polity_gap_statuses(),
    .(n_rows = .N),
    by = .(
      area_code,
      year,
      polity_code,
      polity_name,
      polity_start_year,
      polity_end_year,
      mapping_status
    )
  ] |>
    tibble::as_tibble() |>
    dplyr::mutate(
      gap_kind = .polity_gap_kind(
        .data$year,
        .data$polity_start_year,
        backcast_anchor,
        .data$mapping_status
      ),
      .before = "n_rows"
    ) |>
    dplyr::select(-"mapping_status") |>
    dplyr::arrange(.data$area_code, .data$year)
}

# The two resolutions that attribute a row to a polity not live in its year:
# the nearest-period stand-in, and the back-cast anchor. Named once, because
# `.polity_validity_gaps()` selects on the classes and a literal there would
# silently stop tracking this one.
.polity_gap_statuses <- function() {
  c("out_of_span", "backcast_anchor")
}

# Which class of gap a reported row is.
#
# `"backcast_anchor"` is read off the status rather than off the years, and has
# to be: the whole point of that class is that the resolver matched a real
# period AT THE ANCHOR, so no comparison the caller can write on the returned
# columns separates it from a row that matched at its own year. Its direction
# is not in question -- a period covering the anchor that does not cover an
# earlier row's year can only start after it -- so naming the direction again
# would say nothing, while naming the CAUSE distinguishes WHEP's own back-cast
# convention from a hole in the crosswalk.
#
# For the stand-ins the direction is the question, and the comparison is
# against the year the resolver matched on, not the row's year:
# `.add_polity_columns_dt()` floors the lookup year at `backcast_anchor`, so a
# pre-anchor row is matched as the anchor year and could land on a polity that
# had already ENDED by then, which comparing the raw year would call
# `"polity_not_started"`. The two answers used to differ for 165 rows of a real
# `get_primary_production()`, areas 178 and 273; whep#705 made the stand-in
# prefer a not-yet-started period, so on the
# shipped snapshot they now agree for every (area, year) pair of the crosswalk.
# Keep the matched year anyway -- it is what the resolver decided on, and a
# future crosswalk with an area whose only periods all lie behind the anchor
# brings the divergence back.
#
# A polity with no published start year cannot be one this row precedes, so it
# is reported as ended: `mapping_status == "out_of_span"` already established
# that the row is outside the period in one direction or the other.
.polity_gap_kind <- function(
  year,
  polity_start_year,
  backcast_anchor,
  mapping_status = NA_character_
) {
  match_year <- pmax(as.numeric(year), as.numeric(backcast_anchor))
  not_started <- !is.na(polity_start_year) &
    !is.na(match_year) &
    match_year < polity_start_year
  # `%in%` rather than `==`, so a missing status is FALSE instead of NA, and
  # `rep_len()` so the default scalar widens to the vector `fcase()` needs.
  anchored <- rep_len(
    mapping_status %in% "backcast_anchor",
    length(not_started)
  )
  data.table::fcase(
    anchored    , "backcast_anchor"    ,
    not_started , "polity_not_started" ,
    default = "polity_ended"
  )
}

# ---- ISO3 -> numeric area_code -----------------------------------------
#
# The canonical iso3c -> area_code lookup. It maps to `polity_area_code`, NOT
# to `code`: two ISO3 codes carry a historical predecessor as a second `code`
# (ETH is both 238 Ethiopia and 62 Ethiopia PDR; SDN is both 276 Sudan and 206
# Sudan (former)), so a `code` lookup returns two rows for them. Both members
# of each pair already share one `polity_area_code` (238 and 206), which is
# also the code the commodity balances actually carry, so mapping there is
# unique by construction rather than by picking a winner: 257 iso3c, 257 rows.
.iso3c_area_code_lookup <- function() {
  # `regions_full` states the fold a SECOND time, and the promotion this guards
  # against once survived a withdrawal by only one of the two tables being
  # rebuilt (#419). So the unfold switch has to reach both or the two lookups
  # disagree about where a Rest-of-World member's rows belong.
  whep::regions_full |>
    .unfold_regions_full() |>
    dplyr::filter(!is.na(.data$iso3c), !is.na(.data$polity_area_code)) |>
    dplyr::distinct(
      iso3c = as.character(.data$iso3c),
      area_code = as.integer(.data$polity_area_code)
    )
}

# Resolve a character vector of ISO3 codes to numeric area codes, preserving
# length and order. Unknown codes come back NA; the caller decides whether that
# is fatal, since some callers legitimately carry non-country aggregates.
.iso3c_to_area_code <- function(iso3c) {
  lookup <- .iso3c_area_code_lookup()
  lookup$area_code[match(as.character(iso3c), lookup$iso3c)]
}

# The out-of-span signal on published output: available, and OFF by default.
#
# `add_polity_code()` says a nearest-period stand-in "is attributed to a polity
# that did not exist in that year, so treat it as a coverage gap", and reports it
# as `mapping_status == "out_of_span"`. The reporting-column boundary below then
# deleted that column, so the documented uncertainty was visible in the resolver
# and invisible in every built dataset (whep#545).
#
# Which of the two repairs to adopt is an owner decision, not a bug fix, because
# either changes the schema of ~100 exported outputs and some consumers assert
# exact column sets. So both are implemented and neither is imposed: the default
# leaves every published schema exactly where it is, and
# `options(whep.polity_mapping_status = )` selects
#
# - `"none"` (default): today's behaviour, no extra column.
# - `"flag"`: one logical `reporting_polity_out_of_span` /
#   `partner_polity_out_of_span`, the only part of the status a consumer can act
#   on, leaving `"matched"`/`"manual"` provenance to `polity_area_crosswalk`.
#   It stays STRICTLY `"out_of_span"`: whep#763 added `"backcast_anchor"` to the
#   vocabulary, and folding it into a column named after the other class would
#   move the values of an already-published (if opt-in) column while making
#   the two indistinguishable again, which is the defect that issue is about.
#   `"status"` and `polity_coverage_gaps()` both carry the new class.
# - `"status"`: the full `reporting_mapping_status` / `partner_mapping_status`,
#   which loses no information.
#
# One switch covers every call site, as with `whep.unfold_rest_of_world`, because
# ~100 outputs disagreeing about whether they carry the column would be worse
# than none of them carrying it. `polity_coverage_gaps()` answers the same
# question for an already-built table without any schema change at all, and is
# what a consumer should reach for first.
.polity_status_mode <- function(mode = NULL) {
  valid <- c("none", "flag", "status")
  if (!is.null(mode)) {
    return(rlang::arg_match(mode, valid))
  }
  # A mistyped option would otherwise be reported as a bad `mode` argument the
  # caller never passed, and silently ignoring it would leave the signal off in
  # exactly the run that asked for it.
  mode <- getOption("whep.polity_mapping_status", "none")
  if (!rlang::is_string(mode) || !mode %in% valid) {
    cli::cli_abort(c(
      "{.code options(whep.polity_mapping_status)} must be one of
       {.val {valid}}.",
      "x" = "It is {.val {mode}}."
    ))
  }
  mode
}

# Both column names the switch can emit for a role, so a re-run drops whichever
# the previous run left behind instead of appending a duplicate.
.polity_status_cols <- function(prefix) {
  paste0(prefix, c("mapping_status", "polity_out_of_span"))
}

# Returns the status column `dt` should keep for this role, adding the boolean
# one by reference in `"flag"` mode. Empty when the switch is off, which makes
# the caller's drop list the full set again.
.keep_polity_status_col <- function(dt, prefix, mode) {
  status_col <- paste0(prefix, "mapping_status")
  if (mode == "none" || !status_col %in% names(dt)) {
    return(character(0))
  }
  if (mode == "status") {
    return(status_col)
  }
  flag_col <- paste0(prefix, "polity_out_of_span")
  status <- dt[[status_col]]
  dt[, (flag_col) := !is.na(status) & status == "out_of_span"]
  flag_col
}

# Does the frame already carry a reporting identity resolved for the key it
# still has?
#
# `.aggregate_to_polities()` resolves the polity when it creates the fold and now
# emits it, so the tail helper does not have to resolve it a second time
# (whep#670). Two conditions make keeping it safe rather than hopeful:
#
# - the fixed-point test. A bucket code resolves to itself, so an aggregated
#   frame satisfies `polity_area_code == code_column`. Anything that has since
#   re-keyed the frame -- a FABIO collapse, a `bind_rows()` with rows the
#   aggregator never saw, a join that brought in foreign area codes -- breaks it,
#   and the full resolution below runs as before.
# - the agreement test, on the DISTINCT (code, year) pairs rather than the whole
#   frame, so it costs a fraction of the resolution it is checking. It is what
#   turns "the two paths should agree" into something the build asserts.
#
# The status switch is deliberately excluded: `mapping_status` is not part of the
# carried set, so a run that asked for it re-resolves and gets it.
.carried_reporting_polity <- function(dt, code_column, mode) {
  if (mode != "none") {
    return(FALSE)
  }
  if (!all(c(code_column, .reporting_polity_cols()) %in% names(dt))) {
    return(FALSE)
  }
  key <- dt[[code_column]]
  bucket <- dt[["polity_area_code"]]
  # NA is allowed on the bucket side and only there: a bucket whose own code
  # resolves to no polity in that year has NA, and so does the tail resolution.
  # A non-NA bucket that is not the key is a frame someone re-keyed.
  if (!all(is.na(bucket) | (!is.na(key) & bucket == key))) {
    return(FALSE)
  }
  .carried_polity_agrees(dt, code_column)
}

# Re-resolve the distinct keys the carried identity claims to describe and say
# so out loud if they disagree, rather than publishing either answer silently.
.carried_polity_agrees <- function(dt, code_column) {
  year_col <- if ("year" %in% names(dt)) "year" else NULL
  key_cols <- c(code_column, year_col)
  pairs <- unique(dt[, c(key_cols, .reporting_polity_cols()), with = FALSE])
  resolved <- .add_polity_columns_dt(
    pairs[, key_cols, with = FALSE],
    code_col = code_column,
    year_col = year_col,
    prefix = "reporting_",
    include_unmapped = TRUE
  )
  carried <- list(
    pairs$reporting_polity_code,
    pairs$reporting_polity_name,
    pairs$reporting_polity_has_geometry,
    pairs$polity_area_code
  )
  fresh <- list(
    resolved$reporting_polity_code,
    resolved$reporting_polity_name,
    resolved$reporting_has_geometry,
    resolved$reporting_polity_area_code
  )
  if (all(purrr::map2_lgl(carried, fresh, .polity_values_equal))) {
    return(TRUE)
  }
  # A carried NA where the resolution has an answer is an INCOMPLETE carry, not
  # a contradiction -- `bind_rows()` filling in rows the fold never saw is the
  # ordinary way to get one -- so it just re-resolves. Two different non-NA
  # answers for one key cannot both be right, and that is worth saying out loud.
  if (any(purrr::map2_lgl(carried, fresh, .polity_values_contradict))) {
    cli::cli_warn(c(
      "A carried reporting polity contradicts re-resolving
       {.field {code_column}}.",
      "i" = "Re-resolving, which is what this helper has always published.",
      "i" = "Something re-keyed the frame after {.fun .aggregate_to_polities}
             without dropping the polity columns it emits."
    ))
  }
  FALSE
}

.polity_values_equal <- function(x, y) {
  isTRUE(all.equal(x, y, check.attributes = FALSE))
}

.polity_values_contradict <- function(x, y) {
  both <- !is.na(x) & !is.na(y)
  any(both & x != y)
}

.add_reporting_polity_columns <- function(
  table,
  code_column = "area_code",
  mapping_status = NULL
) {
  mode <- .polity_status_mode(mapping_status)
  dt <- data.table::as.data.table(table)
  if (.carried_reporting_polity(dt, code_column, mode)) {
    # A copy, because `as.data.table()` hands back the caller's own data.table
    # when it is given one, and the resolving path below never reorders the
    # input's columns by reference.
    out <- data.table::copy(dt)
    # The identity is kept, but a status column from an earlier run under a
    # different `whep.polity_mapping_status` is not: the carried path only runs
    # in `"none"` mode, where that column is exactly what the resolving path
    # drops, and leaving it would publish a status no longer tracking anything.
    stale <- intersect(.polity_status_cols("reporting_"), names(out))
    if (length(stale) > 0L) {
      out[, (stale) := NULL]
    }
    return(.order_reporting_polity_cols(out, code_column, character(0)))
  }
  drop_existing <- intersect(
    c(
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      .polity_status_cols("reporting_")
    ),
    names(dt)
  )
  if (length(drop_existing) > 0L) {
    dt[, (drop_existing) := NULL]
  }

  year_col <- if ("year" %in% names(dt)) "year" else NULL
  out <- .add_polity_columns_dt(
    dt,
    code_col = code_column,
    year_col = year_col,
    prefix = "reporting_",
    include_unmapped = TRUE
  )
  if ("reporting_has_geometry" %in% names(out)) {
    data.table::setnames(
      out,
      "reporting_has_geometry",
      "reporting_polity_has_geometry"
    )
  }
  out[, polity_area_code := reporting_polity_area_code]
  kept <- .keep_polity_status_col(out, "reporting_", mode)
  out[,
    setdiff(
      c(
        "reporting_area_name",
        "reporting_area_iso3c",
        "reporting_polity_area_code",
        "reporting_polity_start_year",
        "reporting_polity_end_year",
        "reporting_mapping_status"
      ),
      kept
    ) := NULL
  ]

  .order_reporting_polity_cols(out, code_column, kept)
}

# The published column order and type of a reporting-annotated table. Shared by
# the resolving path and the carried one so the two cannot drift apart in
# anything but where the values came from.
.order_reporting_polity_cols <- function(out, code_column, kept) {
  leading_cols <- c(
    "year",
    code_column,
    "polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry",
    kept
  )
  data.table::setcolorder(
    out,
    c(intersect(leading_cols, names(out)), setdiff(names(out), leading_cols))
  )
  out <- tibble::as_tibble(out)
  # data.table's over-allocation pointer survives the tibble conversion, which
  # makes an otherwise unchanged output compare unequal to a plain tibble and
  # can trigger data.table's shallow-copy warning downstream.
  attr(out, ".internal.selfref") <- NULL
  out
}

# Attach the reporting-polity columns only to a frame that still carries the
# area key. A few outputs have no `area_code` to resolve a polity from -- the
# IMAGE-region aggregate is keyed by region, and `calculate_n_surplus()` accepts
# any balance the caller hands it -- and aborting there would be a regression
# rather than a caught error.
.add_polity_columns_if_keyed <- function(table, code_column = "area_code") {
  if (!rlang::has_name(table, code_column)) {
    return(table)
  }
  .add_reporting_polity_columns(table, code_column = code_column)
}

.add_partner_polity_columns <- function(
  table,
  code_column = "area_code_partner",
  mapping_status = NULL
) {
  mode <- .polity_status_mode(mapping_status)
  dt <- data.table::as.data.table(table)
  drop_existing <- intersect(
    c(
      "partner_polity_code",
      "partner_polity_name",
      "partner_polity_has_geometry",
      .polity_status_cols("partner_")
    ),
    names(dt)
  )
  if (length(drop_existing) > 0L) {
    dt[, (drop_existing) := NULL]
  }

  year_col <- if ("year" %in% names(dt)) "year" else NULL
  out <- .add_polity_columns_dt(
    dt,
    code_col = code_column,
    year_col = year_col,
    prefix = "partner_",
    include_unmapped = TRUE
  )
  if ("partner_has_geometry" %in% names(out)) {
    data.table::setnames(
      out,
      "partner_has_geometry",
      "partner_polity_has_geometry"
    )
  }
  # Keep `partner_polity_area_code` so FABIO-collapsed partners are
  # canonicalized symmetrically with the reporting side's `polity_area_code`.
  kept <- .keep_polity_status_col(out, "partner_", mode)
  out[,
    setdiff(
      c(
        "partner_area_name",
        "partner_area_iso3c",
        "partner_polity_start_year",
        "partner_polity_end_year",
        "partner_mapping_status"
      ),
      kept
    ) := NULL
  ]

  leading_cols <- c(
    "year",
    code_column,
    "partner_polity_area_code",
    "partner_polity_code",
    "partner_polity_name",
    "partner_polity_has_geometry",
    kept
  )
  data.table::setcolorder(
    out,
    c(intersect(leading_cols, names(out)), setdiff(names(out), leading_cols))
  )
  tibble::as_tibble(out)
}

.reporting_polity_cols <- function() {
  c(
    "polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry"
  )
}

.role_polity_cols <- function(role) {
  paste0(
    role,
    c(
      "_polity_code",
      "_polity_name",
      "_polity_has_geometry"
    )
  )
}

.add_label_polity_cols <- function(labels, year = NULL) {
  out <- tibble::as_tibble(labels)
  if (!"area_code" %in% names(out)) {
    cli::cli_abort("{.arg labels} must include {.field area_code}.")
  }

  if (all(.reporting_polity_cols() %in% names(out))) {
    return(out)
  }

  added_year <- FALSE
  if (!is.null(year) && !"year" %in% names(out)) {
    out <- dplyr::mutate(out, year = as.integer(year))
    added_year <- TRUE
  }

  out <- .add_reporting_polity_columns(out)
  if (added_year) {
    out <- dplyr::select(out, -year)
  }
  out
}

.label_reporting_polity_lookup <- function(labels) {
  .add_label_polity_cols(labels) |>
    dplyr::select(dplyr::any_of(c("area_code", .reporting_polity_cols()))) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

.bind_area_label_sources <- function(...) {
  sources <- list(...)
  sources <- purrr::keep(
    sources,
    ~ is.data.frame(.x) && "area_code" %in% names(.x)
  )
  if (length(sources) == 0L) {
    return(tibble::tibble(area_code = integer(0)))
  }

  sources |>
    purrr::map(.add_label_polity_cols) |>
    dplyr::bind_rows() |>
    dplyr::select(dplyr::any_of(c("area_code", .reporting_polity_cols()))) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

.add_role_polity_from_labels <- function(
  table,
  labels,
  role,
  code_column = paste0(role, "_area")
) {
  out <- tibble::as_tibble(table)
  if (!code_column %in% names(out)) {
    cli::cli_abort(
      "Column {.field {code_column}} is required for polity mapping."
    )
  }

  role_cols <- .role_polity_cols(role)
  out <- dplyr::select(out, -dplyr::any_of(role_cols))
  lookup <- .label_reporting_polity_lookup(labels) |>
    dplyr::transmute(
      "{code_column}" := .data$area_code,
      "{role_cols[[1]]}" := .data$reporting_polity_code,
      "{role_cols[[2]]}" := .data$reporting_polity_name,
      "{role_cols[[3]]}" := .data$reporting_polity_has_geometry
    )

  out |>
    dplyr::left_join(lookup, by = code_column) |>
    dplyr::relocate(
      dplyr::all_of(role_cols),
      .after = dplyr::all_of(code_column)
    )
}

#' Get WHEP polity geometries
#'
#' @description
#' Returns the periodized polity database, including geometry. Pass
#' `polity_codes` to retrieve a subset that can be joined to outputs from
#' [add_polity_code()].
#'
#' @param polity_codes Optional character vector of WHEP polity codes.
#'   Subsetting by code needs the suggested package `sf` to be installed; the
#'   whole table is returned without it.
#'
#' @returns An sf data frame.
#' @export
#'
#' @examples
#' # sf is only suggested, and its methods are what make the geometry column
#' # printable, so guard the example on it.
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   codes <- add_polity_code(
#'     tibble::tibble(area_code = c(203L, 68L), year = c(2000L, 2000L))
#'   )$polity_code
#'   geometries <- get_polity_geometries(codes)
#'   print(geometries[, c("polity_code", "polity_name", "polygon_source")])
#' }
get_polity_geometries <- function(polity_codes = NULL) {
  if (is.null(polity_codes)) {
    return(polities)
  }
  .subset_polity_geometries(polities, polity_codes)
}

#' Find FAOSTAT areas whose polity resolution is ambiguous
#'
#' A FAOSTAT area maps to a sequence of polities that is meant to partition time,
#' so `(area_code, year)` has exactly one answer. Where two live polities cover the
#' same year the answer depends on row order rather than on the data, and
#' [add_polity_code()] silently returns whichever the ordering surfaces.
#'
#' This detects that. It is separate from the upstream check that no two periods of
#' one polity *family* overlap: two different families can both map to one FAOSTAT
#' area, which is the case this finds and that one does not.
#'
#' @param crosswalk A crosswalk frame; defaults to [polity_area_crosswalk].
#' @return A data frame with one row per ambiguous `(area_code, year)`, carrying
#'   `area_code`, `year`, `n` and `polity_codes` (comma-separated). Zero rows when
#'   resolution is unique, which is the intended state.
#' @keywords internal
#' @noRd
.area_year_polity_conflicts <- function(crosswalk = NULL) {
  # THE RESOLVED CROSSWALK, NOT THE SHIPPED TABLE, and since whep#717 those are
  # different questions. The shipped table carries BOTH answers for a
  # Rest-of-World member -- the bucket's `ROW-1850-2025` over 1850-2025 and, for
  # the 31 upstream names, that member's own periods -- and
  # `.unfold_rest_of_world()` keeps exactly one per area. Read raw, every one of
  # those pairs looks like an overlap; read as `add_polity_code()` reads it,
  # which is what a published value rests on, it is a partition again. This
  # matches `.polity_join_conflicts()`, which has always read the resolved
  # table.
  cw <- crosswalk %||% .polity_crosswalk(include_unmapped = TRUE)
  cw <- as.data.frame(cw)
  keep <- !is.na(cw$area_code) &
    !is.na(cw$polity_code) &
    !is.na(cw$polity_start_year) &
    !is.na(cw$polity_end_year)
  cw <- cw[keep, ]
  # DA-24's other half -- that an interval nothing succeeds also covers
  # its terminal year -- is deliberately NOT applied here: this detector's
  # remit is the crosswalk's DECLARED succession boundaries. The reading
  # the resolver actually joins on is `.polity_join_conflicts()`, which
  # widens the open end. On polities 751 / `0e52f1ff` (596 crosswalk rows,
  # 262 widened by that rule) the two readings DISAGREE: declared spans
  # report 0 ambiguous `(area_code, year)` pairs and the widened spans
  # report 1 -- area 7 at 1975, `AGO-1975-2025` against `ANG-1905-1975`,
  # which is #683. An earlier note here claimed the two agreed; that was
  # measured on a snapshot since superseded.
  .area_year_span_conflicts(data.frame(
    area_code = cw$area_code,
    polity_code = cw$polity_code,
    span_start = cw$polity_start_year,
    span_end = cw$polity_end_year,
    stringsAsFactors = FALSE
  ))
}

# The years the contract is asserted over: FAOSTAT's first reported year to the
# vintage's horizon.
#
# Both ends are derived, not written down. The lower end is
# `add_polity_code()`'s `backcast_anchor`, which floors every lookup, so nothing
# resolves under a pre-anchor year at all. The upper end is the largest
# `polity_end_year` the crosswalk carries, the same open-period sentinel
# `.current_area_lookup()` reads -- a literal would silently stop covering the
# newest year the next time the snapshot moves, and it has moved twice in this
# epic (#530, #551).
.reporting_era_years <- function(crosswalk) {
  seq.int(
    eval(formals(add_polity_code)$backcast_anchor),
    max(as.integer(crosswalk$polity_end_year), na.rm = TRUE)
  )
}

# The same detection over the spans `add_polity_code()` ACTUALLY JOINS ON, which
# are not the spans the crosswalk declares.
#
# `.area_year_polity_conflicts()` reads `polity_end_year` as written. The
# resolver reads it through `.polity_join_end_year()`, which widens an OPEN
# period by one year (exclusive at a succession, inclusive at an open end,
# #577) and to the inclusive `map_year_end` where the upstream map declares a
# reported year past the territorial span. 263 of the shipped crosswalk's
# area-polity rows are widened that way today -- 264 before #683 closed
# `ANG-1905-1975`, whose successor upstream records only in the inverse
# direction.
#
# So the declared-period check can be clean while the resolution is still
# ambiguous: give an area an open period ending 2025 and a successor starting
# 2025 and the declared spans [.,2025) and [2025,.) do not touch, while the
# joined spans [.,2026) and [2025,.) both cover 2025. `add_polity_code()` would
# then pick by row order -- `unique(matches, by = rowid)` after a
# `polity_start_year DESC` sort keeps exactly one candidate, so the ambiguity
# never shows up as a duplicated output row and cannot be seen downstream.
# This is what makes the contract a property of the resolution rather than of
# the table.
.polity_join_conflicts <- function(crosswalk = NULL, years = NULL) {
  cw <- if (is.null(crosswalk)) {
    .polity_crosswalk(include_unmapped = TRUE)
  } else {
    crosswalk
  }
  cw <- as.data.frame(cw)
  years <- years %||% .reporting_era_years(cw)
  if (!rlang::has_name(cw, "map_year_end")) {
    cw$map_year_end <- NA_integer_
  }
  cw <- cw[!is.na(cw$area_code) & !is.na(cw$polity_code), ]
  span_end <- .polity_join_end_year(
    cw$polity_end_year,
    cw$map_year_end,
    cw$polity_code %in% .open_polity_codes()
  )
  span_start <- ifelse(
    is.na(cw$polity_start_year),
    -Inf,
    as.numeric(cw$polity_start_year)
  )
  # Clamp to the window rather than filtering, so a period that merely starts
  # before it still competes for the years inside it.
  .area_year_span_conflicts(data.frame(
    area_code = cw$area_code,
    polity_code = cw$polity_code,
    span_start = pmax(span_start, min(years)),
    span_end = pmin(span_end, max(years) + 1),
    stringsAsFactors = FALSE
  ))
}

# Which `(polity_area_code, year)` pairs do NOT recover a single polity.
#
# The bucket is a key rows are aggregated on, not an identity: several
# `area_code` values can share one, and then the bucket answers with as many
# polities as its members resolve to. Measured over the reporting era this is
# bucket 206 alone (Sudan (former) 206, Sudan 276 and South Sudan 277 share
# it), which is #414 and not decided here.
#
# Driven through `add_polity_code()` rather than through the spans, because
# what a consumer keying on the bucket gets is the resolution, including the
# nearest-period stand-ins: 206 is ambiguous in every reported year, not only
# in the years its three periods overlap.
.bucket_year_polity_conflicts <- function(years = NULL) {
  cw <- .polity_crosswalk(include_unmapped = TRUE)
  years <- years %||% .reporting_era_years(cw)
  areas <- sort(unique(stats::na.omit(cw$area_code)))
  grid <- tibble::tibble(
    area_code = rep(as.integer(areas), each = length(years)),
    year = rep(as.integer(years), times = length(areas))
  )
  resolved <- as.data.frame(add_polity_code(grid))
  resolved <- resolved[
    !is.na(resolved$polity_code) & !is.na(resolved$polity_area_code),
  ]
  out <- .summarise_conflicts(data.frame(
    area_code = resolved$polity_area_code,
    year = resolved$year,
    polity_code = resolved$polity_code,
    stringsAsFactors = FALSE
  ))
  names(out)[names(out) == "area_code"] <- "polity_area_code"
  out
}

# One row per (area, year) a period covers, then the conflict summary.
# `span_end` is EXCLUSIVE at a succession, so [1920, 1947) covers 1920:1946 --
# getting that wrong would report a spurious conflict at every boundary. DA-24's
# other half, that an interval nothing succeeds also covers its terminal year,
# is deliberately NOT applied here: this is an overlap detector, and its remit
# is the shipped crosswalk's succession boundaries. Measured on that crosswalk,
# adding the open end would change nothing -- 190 intervals are open and 0 areas
# resolve to more than one polity on the open end -- so the two readings agree
# today.
.area_year_span_conflicts <- function(spans) {
  spans <- unique(spans[!is.na(spans$span_start) & !is.na(spans$span_end), ])
  if (nrow(spans) == 0L) {
    return(.empty_conflict_frame())
  }
  long <- Map(
    function(a, p, s, e) {
      if (e <= s) {
        return(NULL)
      }
      data.frame(
        area_code = a,
        year = seq.int(s, e - 1L),
        polity_code = p,
        stringsAsFactors = FALSE
      )
    },
    spans$area_code,
    spans$polity_code,
    as.integer(spans$span_start),
    as.integer(spans$span_end)
  )
  long <- long[!vapply(long, is.null, logical(1))]
  if (length(long) == 0L) {
    return(.empty_conflict_frame())
  }
  .summarise_conflicts(do.call(rbind, long))
}

# `long` carries one row per (area_code, year, polity_code) candidate.
#
# Deduplicated first: an ambiguity is TWO POLITIES answering for one key, not
# two rows. Several areas sharing a bucket and agreeing on the polity is what
# the Rest-of-World fold does to every one of its members, and counting rows
# would report that agreement as a conflict.
.summarise_conflicts <- function(long) {
  long <- unique(long)
  key <- paste(long$area_code, long$year, sep = ":")
  counts <- table(key)
  dup <- names(counts)[counts > 1L]
  if (length(dup) == 0L) {
    return(.empty_conflict_frame())
  }

  hit <- long[key %in% dup, ]
  hit <- hit[order(hit$area_code, hit$year, hit$polity_code), ]
  agg <- stats::aggregate(
    polity_code ~ area_code + year,
    data = hit,
    FUN = function(x) paste(sort(unique(x)), collapse = ", ")
  )
  names(agg)[names(agg) == "polity_code"] <- "polity_codes"
  agg$n <- lengths(strsplit(agg$polity_codes, ", ", fixed = TRUE))
  agg <- agg[order(-agg$n, agg$area_code, agg$year), ]
  rownames(agg) <- NULL
  agg[, c("area_code", "year", "n", "polity_codes")]
}

.empty_conflict_frame <- function() {
  data.frame(
    area_code = integer(0),
    year = integer(0),
    n = integer(0),
    polity_codes = character(0),
    stringsAsFactors = FALSE
  )
}

# Normalisation of a country label, mirroring `matchlib.norm` in whep-polities.
# It must match EXACTLY or the two sides resolve the same input differently.
# Lowercasing and whitespace squishing alone are not enough: upstream also folds
# accents, DROPS parenthesised qualifiers and strips a leading "the".
#
# The parenthetical rule is the consequential one. Upstream reduces
# "Sudan (former)" to "sudan", which merges it into the `sudan` rule set and
# decides which alias wins; treating it as a separate label picks a different
# polity for 2011. Each step below mirrors one line of matchlib.norm.
.norm_polity_label <- function(x) {
  x <- tolower(trimws(x))
  # NFKD + drop non-ASCII: "Reunion" and "Turkiye" lose their diacritics.
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\s*\\(.*?\\)\\s*", " ", x)
  x <- sub("^the\\s+", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Labels the canonical-name route must refuse, derived from the crosswalk rather
# than listed here.
#
# An area the crosswalk leaves `unmapped` has no polity to attribute to, and
# letting a name lookup supply one behind its back is the second-authority
# problem again. Today that is exactly one area: FAOSTAT 351 "China", the
# aggregate of mainland (41), Hong Kong (96), Macao (128) and Taiwan (214), each
# of which reports separately. The name route resolved it anyway, because
# normalisation drops parenthesised qualifiers -- the rule that lets
# "Zimbabwe (1900-1953)" answer to "zimbabwe" also folds CHN-1950-2025
# "China (PRC)" onto "china" -- which attributes aggregate rows to the mainland
# polity and double-counts them against 41 + 96 + 128 + 214.
#
# The ALIAS route is untouched. A curator who decides what a given source means
# by "China" still wins, and that is where such a decision belongs.
.refused_polity_label_names <- function() {
  cw <- polity_area_crosswalk
  unmapped <- cw[!is.na(cw$mapping_status) & cw$mapping_status == "unmapped", ]
  unique(stats::na.omit(.norm_polity_label(c(
    unmapped$area_name,
    unmapped$reporting_polity_name
  ))))
}

#' Resolve a source's country label to a polity
#'
#' @description
#' Maps a country or area **label**, as a source writes it, to a WHEP polity
#' code. This complements [add_polity_code()], which resolves numeric
#' FAOSTAT/FABIO area codes: before this existed there was no supported path
#' from a label to a polity, so datasets carrying labels went unresolved.
#' [mueller_synthetic_n]'s `iso3c` column holds FAO-style legacy codes (`"BZE"`
#' for Belize, `"ROM"` for Romania, `"ZAR"` for Zaire) and
#' [lassaletta_grassland_share]'s `Country` holds name variants (`"Cape Verde"`,
#' `"Swaziland"`), none of which resolve against [polities] directly.
#'
#' @details
#' The mapping is [polity_label_aliases], a copy of the map published by
#' `whep-polities`. It is deliberately NOT computed here: a label's meaning is a
#' fact about the source, upstream already decides it, and a second lookup in
#' this package would be a second authority for the same question.
#'
#' Resolution is **source- and year-aware**, and both matter:
#'
#' - An alias may be scoped to one `source`, because the same label can mean
#'   different things in different sources. A scoped alias never applies to
#'   another source; an unscoped one applies to any.
#' - An alias may be scoped to a year range, because a label's referent changes.
#'   `"Cape Verde"` in 1970 is the Portuguese colony `CPV-1886-1975`; in 1990 it
#'   is `CPV-1975-2025`.
#'
#' Where several aliases match, the most specific wins: year-scoped over
#' unscoped, then source-scoped, then the narrower year range. That ordering
#' mirrors `matchlib.Matcher.match_alias` upstream, so both sides agree.
#'
#' Where no alias applies, a second route tries the polity's own `polity_name`
#' and then, for a three-letter label, its `iso3_code`. That mirrors upstream's
#' "alias, then ISO/name family + year containment", and both halves are needed.
#' Without the name half a caller passing the database's own name for a polity
#' got `NA`: `resolve_polity_label("Netherlands")` found nothing while [polities]
#' carried a polity named exactly that. Without the ISO3 half the map answers
#' only for labels a curator had to decide about, which is 380 of
#' [mueller_synthetic_n]'s 5,043 rows -- the 11 legacy codes -- against all
#' 5,043 with it, asked at `year = 2000`. Asking without a year resolves only
#' 1,255, because the guard below then refuses every identifier more than one
#' live polity has ever carried. Two guards bound both halves.
#'
#' - An identifier resolves only when **exactly one** polity carries it in the
#'   year asked about, because otherwise row order would decide and `NA` is the
#'   honest answer. Sharing an identifier is common in the shipped [polities]
#'   snapshot: of its 726 live rows, 110 normalised names and 133 ISO3 codes are
#'   carried by more than one polity. A year separates nearly all of them -- no
#'   two live polities sharing a normalised name cover a common year -- but not
#'   the ISO3 index, where 69 pairs still do, 62 of them naming different
#'   territories rather than successive periods of one. `"PAN"` in 1970 is the
#'   case that matters: `PAN-1903-1979` and the Canal Zone `CZN-1903-1979` both
#'   carry that ISO3 then -- a real territorial overlap no re-sync removes --
#'   so the answer is `NA`, while `"PAN"` in 2000 resolves to `PAN-1979-2025`.
#' - An alias covering that year outranks both, whatever its source, and a label
#'   naming an area the crosswalk leaves unmapped is refused outright.
#'
#' Returns `NA` when neither route resolves, which is a real answer rather than a
#' failure. Some labels are aggregates a source keeps reporting after the
#' territory stopped existing -- `"FSU"` runs to 2009 though nothing has held
#' that territory since 1991 -- and those years are deliberately unmapped rather
#' than routed to a polity that had ended.
#'
#' Every resolved code is one [get_polity_geometries()] can return a row for,
#' and that is an invariant rather than a happy accident: [polity_label_aliases]
#' and [polities] are regenerated together from a single upstream revision, and
#' `data-raw/table_mappings.R` aborts the build if any alias names a polity the
#' shipped table does not carry. A dangling resolution therefore cannot ship.
#'
#' @param label Character vector of source labels.
#' @param source Optional source slug (e.g. `"lassaletta-grassland-share"`).
#'   Length 1, or the same length as `label`. On the alias route `NULL` matches
#'   unscoped aliases only -- 180 of 903 -- so a `NULL` source narrows that route
#'   sharply; the identity routes then get their turn, subject to the guards
#'   above.
#' @param year Optional integer vector of years. Length 1, or the same length as
#'   `label`. On the alias route `NULL` matches aliases with no year scope only,
#'   which is the 15 of 903 published aliases carrying NEITHER bound. The name
#'   and ISO3 routes can still answer without a year, but only for an identifier
#'   exactly one polity has ever carried, so supplying a year remains much the
#'   stronger question: it is what lets a label resolve to the right *period*
#'   rather than to nothing.
#'
#' @returns A character vector of polity codes, `NA` where nothing matched.
#'
#' @examples
#' resolve_polity_label("ZAR", source = "mueller-synthetic-n", year = 2000)
#' resolve_polity_label(
#'   c("Cape Verde", "Cape Verde"),
#'   source = "lassaletta-grassland-share",
#'   year = c(1970L, 1990L)
#' )
#'
#' @seealso [add_polity_code()] for numeric area codes.
#' @export
resolve_polity_label <- function(label, source = NULL, year = NULL) {
  aliases <- polity_label_aliases
  n <- length(label)

  recycle <- function(x, nm) {
    if (is.null(x)) {
      return(rep(NA, n))
    }
    if (length(x) == 1L) {
      return(rep(x, n))
    }
    if (length(x) != n) {
      cli::cli_abort(
        "{.arg {nm}} must be length 1 or the same length as {.arg label}."
      )
    }
    x
  }
  source <- recycle(source, "source")
  year <- recycle(year, "year")

  # Normalise both sides once: each route below needs the same key for a label.
  alias_key <- .norm_polity_label(aliases$source_label)
  label_key <- .norm_polity_label(label)

  # Identity fallbacks, tried only after the alias route misses.
  #
  # A DEAD POLITY MUST NOT BE A RESOLUTION CANDIDATE, for the same reason
  # `data-raw/table_mappings.R` filters them out of the crosswalk build: upstream
  # retires a polity when a finer or corrected split supersedes it, and both rows
  # stay in the published `polities` table because looking up a code you already
  # hold is legitimate. What must not happen is resolving TO one.
  #
  # Only the INFERENCE routes below are filtered. The alias route is not: an alias
  # names a code explicitly, and a curator who decided a label means a particular
  # polity has made that decision whatever its status.
  #
  # Unfiltered, a corrected period and the row it corrected both cover the same
  # year, the ambiguity guard cannot separate them, and the label resolves to
  # nothing. Measured after the #530 re-sync: `CAN-1948-2025` (retired) sat beside
  # `CAN-1949-2025`, and the same shape held for BRA, ARG, AGO, GRC and IRQ --
  # 234 of `mueller_synthetic_n`'s 5,043 rows unresolvable, every one of them with
  # exactly one LIVE candidate.
  #
  # `polities` is an sf data frame and sf is only suggested, so the attribute
  # columns are taken by name rather than through `sf::st_drop_geometry()`.
  # `.polity_is_live()` is the package's one reading of which rows are dead.
  alive <- .polity_is_live(polities$wiki_status)
  pol <- data.frame(
    polity_code = polities$polity_code[alive],
    start_year = polities$start_year[alive],
    end_year = polities$end_year[alive],
    stringsAsFactors = FALSE
  )
  # The exclusive upper bound the year test below compares against: EXCLUSIVE AT
  # A SUCCESSION, INCLUSIVE AT AN OPEN END, the same reading
  # `.polity_join_end_year()` gives `add_polity_code()` (#577). Read strictly
  # exclusively, the label route answered `NA` for every polity whose interval
  # ends at the open-period sentinel -- 203 of the 204 present-day countries in
  # `gleam_geographic_hierarchy` at 2025 -- while the numeric route resolved them
  # normally (#712).
  #
  # Openness is ABSENCE OF A SUCCESSOR, not `end_year == max(end_year)`. The year
  # test would widen a period that ends at the maximum AND is succeeded, and that
  # is what puts two candidates on a succession year; `.open_polity_codes()`
  # already excludes the handed-over periods, so the widening can only add years
  # nothing else claims.
  pol$join_end_year <- .polity_join_end_year(
    pol$end_year,
    NA_integer_,
    pol$polity_code %in% .open_polity_codes()
  )
  name_key <- .norm_polity_label(polities$polity_name[alive])
  # The ISO3 index is what makes this usable for the datasets that motivated it.
  # The alias map is keyed on the labels curators had to decide about, so a label
  # that is simply a current ISO3 code is not in it: without this route,
  # `mueller_synthetic_n`'s `iso3c` column resolved 380 of 5,043 rows -- only the
  # 11 FAO-style legacy codes the map does carry -- and `crops_manure_n`'s `ISO`
  # 860 of 31,648. Upstream's matcher resolves "by alias, then ISO/name family +
  # year containment", so this is the second half of that rule, not a new one.
  iso_key <- toupper(trimws(polities$iso3_code[alive]))
  refuse_names <- .refused_polity_label_names()

  family <- function(code) sub("-.*", "", code)

  by_name <- function(i) {
    if (label_key[i] %in% refuse_names) {
      return(NA_character_)
    }
    hit <- which(name_key == label_key[i])
    # An ISO3 code is only ever three letters, so trying the ISO3 index for
    # anything longer cannot match and would only widen the failure surface.
    if (length(hit) == 0L && grepl("^[a-z]{3}$", label_key[i])) {
      hit <- which(!is.na(iso_key) & iso_key == toupper(label_key[i]))
    }
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    cand <- pol[hit, , drop = FALSE]
    if (!is.na(year[i])) {
      cand <- .polity_year_candidates(cand, year[i])
    }
    # THE AMBIGUITY GUARD IS THE DESIGN. Nested periodisations and known
    # duplicates make several polities share a normalised name, so resolving by
    # row order would invent an answer -- which is precisely what the alias map
    # exists to state explicitly. With no year given, several periods of one
    # territory are as ambiguous as two different territories.
    if (nrow(cand) != 1L) {
      return(NA_character_)
    }

    # A CURATED RULE THAT SPEAKS ABOUT THIS YEAR OUTRANKS THE NAME, whatever its
    # source. Falling through on any alias miss lets the name contradict every
    # rule written for the label; refusing to fall through at all sends labels
    # that merely carry a source-scoped alias back to NA. What separates the two
    # is not source but AGREEMENT, so the name answers only where no rule speaks
    # about that year, or where the rules that do speak agree on the family.
    # Year-scoped rules are silent outside their span.
    speaks <- which(alias_key == label_key[i])
    if (length(speaks) > 0L) {
      rules <- aliases[speaks, ]
      # Same half-open rule as the year filter below: a rule bounded on one side
      # speaks about the years inside that bound, not about every year.
      rlo <- ifelse(is.na(rules$year_start), -Inf, rules$year_start)
      rhi <- ifelse(is.na(rules$year_end), Inf, rules$year_end)
      scoped <- !is.na(rules$year_start) | !is.na(rules$year_end)
      covering <- !scoped |
        is.na(year[i]) |
        (year[i] >= rlo & year[i] <= rhi)
      fams <- unique(family(rules$polity_code[covering]))
      if (length(fams) > 0L && !family(cand$polity_code[1]) %in% fams) {
        return(NA_character_)
      }
    }
    cand$polity_code[1]
  }

  vapply(
    seq_len(n),
    function(i) {
      hit <- which(alias_key == label_key[i])
      if (length(hit) == 0L) {
        return(by_name(i))
      }
      cand <- aliases[hit, ]

      # A source-scoped alias applies only to that source.
      keep <- is.na(cand$source) |
        cand$source == "" |
        (!is.na(source[i]) & cand$source == source[i])
      cand <- cand[keep, ]
      # No applicable rule for this source -- the name route gets its turn, and
      # its own agreement check decides whether answering would contradict the
      # rules that do exist.
      if (nrow(cand) == 0L) {
        return(by_name(i))
      }

      # A year-scoped alias applies only inside its range. With no year given,
      # only unscoped aliases can match -- guessing a year would invent an
      # answer.
      #
      # A MISSING BOUND IS UNBOUNDED ON THAT SIDE, not unscoped on both.
      # Requiring both bounds before honouring either makes a half-open range
      # do nothing at all: one published alias is `italy | iia | (blank) | 1860
      # -> SAR-1800-1860`, and with `year_start` empty the 1860 bound would be
      # ignored, so IIA data labelled "italy" would resolve to Sardinia in the
      # year 2000. Upstream's validate_alias_chain_overlaps.py applies the same
      # rule for the same reason.
      lo <- ifelse(is.na(cand$year_start), -Inf, cand$year_start)
      hi <- ifelse(is.na(cand$year_end), Inf, cand$year_end)
      scoped <- !is.na(cand$year_start) | !is.na(cand$year_end)
      in_range <- !scoped |
        (!is.na(year[i]) & year[i] >= lo & year[i] <= hi)
      cand <- cand[in_range, ]
      # A YEAR mismatch does fall through, unlike the source mismatch above: for
      # years no alias speaks about, the name route answers correct history
      # rather than overriding curation. A source-scoped alias claims a label's
      # meaning for a reporter; a year-scoped one claims it for a span, and
      # outside that span it is silent, not contradicted.
      if (nrow(cand) == 0L) {
        return(by_name(i))
      }

      # Most specific first: year-scoped, then source-scoped, then narrower
      # span. A half-open range counts as scoped and gets an infinite span, so
      # it outranks a rule with no bounds at all and loses to any rule bounded
      # on both sides.
      lo <- ifelse(is.na(cand$year_start), -Inf, cand$year_start)
      hi <- ifelse(is.na(cand$year_end), Inf, cand$year_end)
      scoped <- !is.na(cand$year_start) | !is.na(cand$year_end)
      span <- hi - lo
      ord <- order(
        -(2L *
          as.integer(scoped) +
          as.integer(!is.na(cand$source) & cand$source != "")),
        span
      )
      cand$polity_code[ord[1]]
    },
    character(1)
  )
}

# The candidate periods of one identifier that cover `year`, read the way
# `add_polity_code()` reads a span: EXCLUSIVE AT A SUCCESSION, INCLUSIVE AT AN
# OPEN END (#577).
#
# STRICT CONTAINMENT OUTRANKS THE WIDENING, and that precedence is the whole
# reason this is a helper rather than one predicate. Widening every open period
# unconditionally is what puts two candidates on a boundary year -- the failure
# mode of #720 -- because a terminated aggregate records no successor and is
# therefore open by the successor test: `EGYSUD-1934-1956` reaches 1956 beside
# `EGY-1925-1967`, `CODRU-1922-1960` reaches 1960 beside `COD-1960-2025`, and
# `MASG-1946-1963` reaches 1963 beside `MYS-1963-1965`. The ambiguity guard in
# the caller then answers `NA` for a year that used to resolve. A period whose
# declared span really contains the year is the better answer than one that only
# reaches it by the open-end rule, so the widened bound is consulted only when
# nothing claims the year outright. Measured on the shipped snapshot: over 1,020
# identifiers x 1850:2026 this adds 700 resolutions (680 at the 2025 sentinel,
# 20 in the last year of a terminated period upstream records no successor for,
# `ANT-1961-2010` in 2010 among them) and moves NONE.
.polity_year_candidates <- function(cand, year) {
  starts <- !is.na(cand$start_year) & year >= cand$start_year
  declared <- starts & !is.na(cand$end_year) & year < cand$end_year
  widened <- starts & !is.na(cand$join_end_year) & year < cand$join_end_year
  cand[if (any(declared)) declared else widened, , drop = FALSE]
}

# -- Dissolved-federation successor closure ------------------------------------

# A dissolved federation (USSR, Czechoslovakia, Yugoslav SFR) carries no
# present-day ISO3 code, so any lookup keyed on present-day ISO3 -- LUH2 land
# use among them -- cannot reach its territory at all. The polities database
# publishes the dissolution relation as `successor`, so the territory is
# recoverable as the union of the states that replaced it. The walk has to be
# transitive: the Yugoslav SFR reaches Serbia only through the 1992-2006
# Serbia-and-Montenegro union, three hops down.
#
# `available_iso3` is the ISO3 vocabulary the caller can actually resolve, and a
# branch stops as soon as it lands inside it, so no successor is expanded past
# the point where it becomes reachable.
.successor_iso3_map <- function(polity_codes, available_iso3, max_depth = 12L) {
  edges <- .polity_successor_edges()
  iso3 <- .polity_iso3_lookup()
  polity_codes <- unique(polity_codes[!is.na(polity_codes)])
  purrr::map(
    rlang::set_names(polity_codes),
    \(code) .walk_successor_iso3(code, edges, iso3, available_iso3, max_depth)
  )
}

.walk_successor_iso3 <- function(
  polity_code,
  edges,
  iso3,
  available_iso3,
  max_depth
) {
  frontier <- polity_code
  seen <- character(0)
  found <- character(0)
  depth <- 0L
  while (length(frontier) > 0L && depth < max_depth) {
    frontier <- setdiff(frontier, seen)
    seen <- c(seen, frontier)
    reached <- unname(iso3[frontier])
    resolved <- !is.na(reached) & reached %in% available_iso3
    found <- c(found, reached[resolved])
    frontier <- unique(unlist(
      edges[frontier[!resolved]],
      use.names = FALSE
    ))
    depth <- depth + 1L
  }
  sort(unique(found))
}

.polity_successor_edges <- function() {
  successors <- stringr::str_split(polities$successor, ";\\s*")
  successors <- purrr::map(
    successors,
    \(codes) codes[!is.na(codes) & codes != ""]
  )
  rlang::set_names(successors, polities$polity_code)
}

.polity_iso3_lookup <- function() {
  rlang::set_names(polities$iso3_code, polities$polity_code)
}

# Row-subsetting an sf data frame only keeps its geometry column an `sfc`
# through `[.sf`, and that method exists only once the sf namespace is loaded.
# sf is suggested here, not imported, so nothing guarantees it is. Without the
# guard below `[.data.frame` ran instead and returned an object that still
# claimed class `sf` and still carried `attr(, "sf_column") == "geom"`, while
# the column those point at came back a bare list -- so it passed every cheap
# structural check and only aborted later, inside sf, complaining about a
# column nobody had renamed (whep#620). `.sf_namespace_available()` both
# states the requirement and loads the namespace, which registers the method.
.subset_polity_geometries <- function(geometries, polity_codes) {
  if (!.sf_namespace_available()) {
    cli::cli_abort(
      c(
        "Package {.pkg sf} is required to subset polity geometries by code.",
        i = "Install {.pkg sf}, or call {.fn get_polity_geometries} without
             {.arg polity_codes} to get the whole table."
      ),
      class = "whep_sf_required"
    )
  }
  geometries[geometries$polity_code %in% polity_codes, ]
}

.sf_namespace_available <- function() {
  requireNamespace("sf", quietly = TRUE)
}
