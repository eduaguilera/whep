items_cbs <- here::here("inst", "extdata", "items_cbs.csv") |>
  readr::read_csv()

items_prod <- here::here("inst", "extdata", "items_prod.csv") |>
  readr::read_csv()

whep_polities_gpkg <- Sys.getenv(
  "WHEP_POLITIES_GPKG",
  unset = path.expand("~/whep-polities/data/final/polities_database.gpkg")
)

polities <- sf::st_read(whep_polities_gpkg, quiet = TRUE)

# Kept as a belt-and-braces guard even though upstream now normalises this at the
# source (lbm364dl/whep-polities#39 writes NULL rather than "NA" or ""). It costs a
# vectorised comparison per character column and it reported 79 iso3 and 185 cow
# values the first time it ran, so it stays until the upstream fix has been in place
# long enough to trust — and it makes this build correct against an older GeoPackage.
#
# The GeoPackage round-trip used to write missing text as the literal string "NA", so
# reading it back gave a value that LOOKED present. `iso3_code` had 79 such rows
# against 3 real NAs and `cow_code` 185, which means a consumer filtering
# `is.na(iso3_code)` found 3 missing codes when 82 were missing. Worse, any check
# of the form `!is.na(iso3)` treated those rows as having a valid ISO3 — the
# ISO3-keyed bridges in read_raw_inputs.R do exactly that.
#
# Converted for every character column: "NA" is not a legitimate value for any of
# them (Namibia is NAM, not NA), so there is nothing to preserve. Deliberately
# done here, at the read, rather than in each consumer — the artifact belongs to
# the file format, not to any one use of the data.
chr_cols <- names(polities)[vapply(polities, is.character, logical(1))]
na_text <- vapply(
  chr_cols,
  function(col) sum(polities[[col]] == "NA", na.rm = TRUE),
  integer(1)
)
for (col in chr_cols[na_text > 0L]) {
  polities[[col]][polities[[col]] == "NA"] <- NA_character_
}
if (any(na_text > 0L)) {
  affected <- paste0(
    names(na_text)[na_text > 0L],
    " (",
    na_text[na_text > 0L],
    ")",
    collapse = ", "
  )
  cli::cli_inform(
    "Converted literal \"NA\" text to missing in: {affected}."
  )
}

polities$iso3c <- polities$iso3_code
polities$has_geometry <- !sf::st_is_empty(polities)

# `wiki_status` values that mean the row must NEVER receive data: `retired` (the
# row was withdrawn) and `superseded` (it was split or merged into finer rows,
# and carries a `superseded_by` pointer upstream). whep-polities enforces the
# same exclusion in its own matcher (`matchlib.Matcher.DEAD_STATUS`), and without
# it here the crosswalk routed 24 FAOSTAT area codes to withdrawn polities —
# Brazil 21 to the collapsed BRA-1800-2025 rather than the three rows that
# replaced it at the 1903 Acre acquisition, India 100 and Indonesia 101 likewise.
# That both attributes data to a row that no longer exists AND hides the period
# splits that replaced it.
#
# The rows are kept in `polities` for provenance (a reader may need to know what
# a historical code used to mean) but are excluded from anything that RESOLVES
# data, which is `polity_attrs` and therefore the crosswalk.
whep_dead_polity_status <- c("retired", "superseded")

polities_live <- polities |>
  dplyr::filter(!.data$wiki_status %in% whep_dead_polity_status)

n_dead <- nrow(polities) - nrow(polities_live)
if (n_dead > 0L) {
  cli::cli_inform(paste0(
    "Excluded {n_dead} polit{?y/ies} with wiki_status in ",
    "{.val {whep_dead_polity_status}} from polity resolution ",
    "(kept in `polities` for provenance)."
  ))
}

polity_attrs <- polities_live |>
  sf::st_drop_geometry() |>
  dplyr::mutate(polity_prefix = sub("-.*", "", .data$polity_code)) |>
  dplyr::select(
    polity_prefix,
    polity_code,
    polity_name,
    polity_start_year = start_year,
    polity_end_year = end_year,
    polity_type,
    iso3_code,
    cow_code,
    continent,
    wiki_status,
    polygon_status,
    has_geometry
  )
known_polity_prefixes <- unique(polity_attrs$polity_prefix)

excel_na <- c("", "NA", "#N/A", "#DIV/0!", "#REF!")

# The source-label -> polity map published by whep-polities
# (data/final/label_alias_map.csv, gated there by write_label_alias_map.py
# --check). Embedded rather than resolved at runtime, for the same reason
# `polities` is: a package function cannot depend on a sibling checkout existing.
#
# This exists because `add_polity_code()` resolves NUMERIC area codes and nothing
# resolved a country LABEL. Datasets carrying labels therefore had no supported
# path to a polity: mueller_synthetic_n's `iso3c` column holds FAO-style legacy
# codes (BZE, ROM, ZAR) and lassaletta_grassland_share's `Country` holds name
# variants (Cape Verde, Swaziland), and both simply went unresolved. Building a
# lookup here instead of consuming the published one would make this package a
# second authority for label -> polity, which is what misattributed 118
# area-years of FAOSTAT data (whep#387).
whep_label_alias_map <- Sys.getenv(
  "WHEP_POLITIES_LABEL_ALIAS_MAP",
  unset = path.expand("~/whep-polities/data/final/label_alias_map.csv")
)

# Fail with an explanation rather than readr's bare "does not exist". This file is
# published by whep-polities and arrives with lbm364dl/whep-polities#39, whereas
# polities_database.gpkg is already on that repo's main — so between the two PRs
# merging, this is the one upstream artifact a regeneration can be missing, and the
# raw error names a path without saying what provides it.
if (!file.exists(whep_label_alias_map)) {
  cli::cli_abort(c(
    "The published label alias map is missing.",
    x = "Looked for {.path {whep_label_alias_map}}.",
    i = paste(
      "It is published by whep-polities as {.path data/final/label_alias_map.csv}",
      "and gated there by {.code scripts/write_label_alias_map.py --check}."
    ),
    i = paste(
      "If that repository is checked out elsewhere, point",
      "{.envvar WHEP_POLITIES_LABEL_ALIAS_MAP} at the file."
    ),
    i = paste(
      "MERGE ORDER: whep-polities#39 introduces this file, so it must merge before",
      "this package's data/ can be regenerated. The committed data/*.rda already",
      "carry the aliases, so only regeneration is affected, not use."
    )
  ))
}

polity_label_aliases <- readr::read_csv(
  whep_label_alias_map,
  show_col_types = FALSE,
  na = excel_na,
  col_types = readr::cols(
    source_label = readr::col_character(),
    source = readr::col_character(),
    year_start = readr::col_integer(),
    year_end = readr::col_integer(),
    polity_code = readr::col_character(),
    common_name = readr::col_character(),
    confidence = readr::col_character(),
    # New in the upstream contract: how many source rows were actually observed for
    # this label, 0 when the label is merely mappable. Declared explicitly because
    # this col_types list is exhaustive by intent — an upstream column that is not
    # named here is a column this script cannot see.
    observed_rows = readr::col_double()
  )
)

# Every published alias must name a polity this package carries. Upstream gates
# the same invariant, so a failure here means the two copies have drifted rather
# than that the map is wrong.
unknown_alias_targets <- setdiff(
  polity_label_aliases$polity_code,
  polities$polity_code
)
if (length(unknown_alias_targets) > 0L) {
  cli::cli_abort(c(
    "The published label alias map targets polities this package does not carry.",
    x = "Unknown: {.val {utils::head(unknown_alias_targets, 5)}}.",
    i = "Rebuild from the same whep-polities revision that produced the map."
  ))
}

cli::cli_inform(paste0(
  "Loaded {nrow(polity_label_aliases)} published label aliases over ",
  "{length(unique(polity_label_aliases$source_label))} labels."
))

regions_full_raw <- here::here(
  "inst",
  "extdata",
  "harmonization",
  "regions_full.csv"
) |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

regions_compact <- here::here("inst", "extdata", "regions.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

regions_for_crosswalk <- dplyr::bind_rows(
  regions_full_raw,
  regions_compact |>
    dplyr::anti_join(
      regions_full_raw |>
        dplyr::filter(!is.na(.data$code)) |>
        dplyr::transmute(area_code = as.integer(.data$code)),
      by = "area_code"
    ) |>
    dplyr::transmute(
      # `polity_prefix`, not a polity code: this is an ISO3-shaped family key
      # that the prefix join below resolves to a period-specific polity.
      polity_prefix = .data$iso3c,
      polity_name = .data$area_name,
      code = as.integer(.data$area_code),
      iso3c = .data$iso3c,
      FAOSTAT_name = .data$area_name,
      name = .data$area_name,
      cbs = FALSE,
      fabio_code = as.integer(.data$area_code),
      region = .data$region
    )
)

# Only dissolved-state aggregates whose FAOSTAT reporting does NOT overlap their
# successor states in time belong here (Czechoslovakia -> Czechia/Slovakia in
# 1993, USSR -> successors in 1992, Yugoslav SFR -> successors in 1992): the
# aggregate is the sole China-style overlap, so mapping it is lossless.
#
# FAOSTAT area 351 "China" is deliberately NOT mapped: it is an aggregate of
# 41 (mainland) + 96 (Hong Kong) + 128 (Macao) + 214 (Taiwan) reported ALONGSIDE
# those components for every year (1961-2024, full overlap). Those components
# already map to their own polities (CHN/HKG/MAC/TWN), so mapping 351 to CHN as
# well double-counted China across every FAOSTAT domain. Left unmapped, 351 is
# dropped as a statistical aggregate (its iso3c and polity_code are NA).
# Area-to-prefix overrides, DERIVED from the published FAOSTAT map rather than written out.
#
# Eleven of these were hand-maintained here, and the file's own comment conceded that
# "upstream's own FAOSTAT matching pipeline already assigns exactly these polities". Comparing
# them proved it: ten of the eleven were byte-identical to what the published map yields. Keeping
# a second copy of a decision upstream already makes is exactly what resolve_polity_label()'s
# documentation forbids for aliases, and for the same reason.
#
# The comparison also showed the hand-written list was INCOMPLETE. Upstream gives more than one
# prefix to seven areas; the list covered six. Area 240, United States Virgin Islands, was
# missing, and upstream maps it across two eras — DWI-1800-1917 (Danish West Indies) and
# VIR-1917-2025. That omission is currently masked because area 240 has no observed data and so
# folds into rest-of-world, but it is latent rather than harmless: if the area ever gained data
# its pre-1917 years would resolve to VIR-1917-2025, which is precisely the misattribution this
# mechanism exists to prevent.
#
# Deriving it cannot be incomplete. An area appears here when upstream's own mapping needs a
# prefix the area's ISO3 does not supply, or needs more than one.
whep_faostat_area_map <- Sys.getenv(
  "WHEP_POLITIES_FAOSTAT_MAP",
  unset = path.expand("~/whep-polities/data/final/faostat_area_polity_map.csv")
)
# All three upstream inputs must come from ONE checkout. They are three separate
# environment variables with three separate defaults and nothing has required them
# to agree, which is not a hypothetical: this script was run with the alias map
# pointed at a branch worktree and WHEP_POLITIES_CSV -- a variable this script does
# not read at all -- pointed at the same place, so the polities table silently came
# from the sibling repo's MAIN branch instead. The rebuilt data carried main's
# retired polygon_status vocabulary (`missing`, `excluded`, `approximate`,
# `derived`) while the alias map carried the branch's, and the only reason it
# surfaced was a documentation test that pins the vocabulary.
#
# Mixing sources produces artefacts indistinguishable from real drift. Cheaper to
# refuse.
upstream_dirs <- unique(dirname(c(
  whep_polities_gpkg,
  whep_label_alias_map,
  whep_faostat_area_map
)))
if (length(upstream_dirs) > 1L) {
  cli::cli_abort(c(
    "The upstream polities inputs come from more than one directory.",
    x = "Resolved to: {.path {upstream_dirs}}",
    i = paste(
      "Set WHEP_POLITIES_GPKG, WHEP_POLITIES_LABEL_ALIAS_MAP and",
      "WHEP_POLITIES_FAOSTAT_MAP to files in the SAME whep-polities checkout.",
      "Note that WHEP_POLITIES_CSV is read by the tests, not by this script."
    )
  ))
}

if (!file.exists(whep_faostat_area_map)) {
  cli::cli_abort(c(
    "The published FAOSTAT area map is missing.",
    x = "Looked for {.path {whep_faostat_area_map}}.",
    i = paste(
      "It is published by whep-polities as",
      "{.path data/final/faostat_area_polity_map.csv} and gated there by",
      "{.code scripts/write_faostat_area_map.py --check}."
    ),
    i = "Point {.envvar WHEP_POLITIES_FAOSTAT_MAP} at it if checked out elsewhere."
  ))
}

upstream_area_prefixes <- readr::read_csv(
  whep_faostat_area_map,
  show_col_types = FALSE,
  na = excel_na
) |>
  dplyr::transmute(
    area_code = as.integer(.data$area_code),
    manual_polity_prefix = sub("-.*", "", .data$polity_code),
    iso3 = .data$iso3
  ) |>
  dplyr::filter(!is.na(.data$area_code)) |>
  dplyr::distinct() |>
  # Only areas that actually NEED an override: those upstream spreads across more than one
  # prefix, and those whose single prefix is not the one the area's ISO3 would supply on its
  # own. Deriving for every mapped area instead marks 564 of 622 crosswalk rows
  # `mapping_status == "manual"`, which erases the distinction between an area matched by ISO3
  # and one deliberately redirected — the status is metadata a reader relies on.
  dplyr::mutate(
    n_prefix = dplyr::n_distinct(.data$manual_polity_prefix),
    .by = "area_code"
  ) |>
  dplyr::filter(
    .data$n_prefix > 1L |
      is.na(.data$iso3) |
      .data$manual_polity_prefix != .data$iso3
  ) |>
  dplyr::select("area_code", "manual_polity_prefix")

# Area 206 is augmented, and it is the one place the two sources disagree. Upstream maps
# "Sudan (former)" to the SUD chain only, declining any post-2011 mapping; this package also
# routes post-2011 years to SDN-2011-2025 so the area resolves across the 2011 secession. Both
# readings are defensible and the difference is inert in practice, since FAOSTAT reports
# post-2011 years under areas 276 and 277 rather than 206. Kept explicit so the divergence is a
# recorded choice rather than a silent edit to derived data.
manual_area_prefixes <- dplyr::bind_rows(
  upstream_area_prefixes,
  tibble::tibble(area_code = 206L, manual_polity_prefix = "SDN")
) |>
  dplyr::distinct() |>
  dplyr::mutate(
    manual_note = paste0(
      "Prefix assigned by the published whep-polities FAOSTAT area map",
      dplyr::if_else(
        .data$area_code == 206L & .data$manual_polity_prefix == "SDN",
        ", augmented here so area 206 resolves across the 2011 Sudan secession",
        ""
      ),
      "."
    )
  )

# Areas whose label has data of its own, per the upstream alias map's observed_rows.
#
# The `cbs` flag alone was too narrow a test for "has data". Eleven areas that FABIO
# folds into rest-of-world carry no commodity balances yet hold substantial production
# and trade: Bermuda 67,310 observed rows, Faroe Islands 45,036, Cook Islands 42,137,
# Palestine 32,534, Equatorial Guinea 23,719, Niue 22,055, Reunion 13,083, Guadeloupe
# 11,766, Martinique 9,541, Palau 9,051, French Guiana 8,934. All eleven were being
# routed to ROW-1850-2023 while each has its own live polity that the alias map already
# targets for the same label — two published contracts disagreeing about where one
# territory's data belongs.
#
# Deliberately keyed on observed data rather than on "an alias exists". Fourteen more
# folded areas do have an alias but no observed rows — Monaco, San Marino, Montserrat,
# Norfolk Island and the like. Unfolding those would change no data and would diverge
# from FABIO's aggregation for nothing, so they keep folding, which is the same
# reasoning as before; only the test for it is now correct.
#
# "No observed rows" now covers two upstream states rather than one: 0, meaning
# measured and none, and NA, meaning that source's corpus is not in the upstream repo
# at all. The filter below already excluded NA explicitly, so nothing here changed
# when upstream stopped coercing the second into the first -- confirmed by rebuilding
# and diffing every area's polity_area_code, 0 of 218 moved. Worth stating because the
# obvious reading of `observed_rows == 0` as "this label is inert" is now wrong, and
# 393 of 806 aliases are in the NA state.
alias_observed <- polity_label_aliases |>
  dplyr::filter(
    !is.na(.data$observed_rows),
    .data$observed_rows > 0,
    !startsWith(.data$polity_code, "ROW-")
  ) |>
  dplyr::distinct(source_label = .data$source_label)

# Every label of a polity that has data under ANY of its labels. `observed_rows` is
# counted per LABEL, so a renamed country files its count on the name the area is not
# called: area 209 is "Eswatini" in both label columns while its 180,663 rows sit on
# "Swaziland", same polity SWZ-1894-2025. Label matching alone therefore left Eswatini
# folded into FABIO's rest-of-world bucket while three areas unfolded in the same change
# took their own codes — and that asymmetry produced a cross-territory denominator in the
# livestock split shares.
alias_observed_via_polity <- polity_label_aliases |>
  dplyr::filter(!startsWith(.data$polity_code, "ROW-")) |>
  dplyr::filter(
    .data$polity_code %in%
      polity_label_aliases$polity_code[which(
        !is.na(polity_label_aliases$observed_rows) &
          polity_label_aliases$observed_rows > 0
      )]
  ) |>
  dplyr::distinct(source_label = .data$source_label)

# Match on EITHER label column, not just FAOSTAT_name. Bermuda (17) and Palau (180)
# carry the literal Excel error string "#N/A" there while their `name` column is
# correct, so a FAOSTAT_name-only join silently left both folded — 67,310 and 9,051
# observed rows respectively — and the fix looked like it worked because the other nine
# areas moved.
#
# TWO ROUTES, UNIONED, and the second is gated on `cbs`. Expanding to any label of a
# data-carrying polity is too broad on its own: it adds 351 China, which is deliberately
# unmapped against double-counting its components, plus five territories the
# folded_into_aggregate baseline deliberately keeps folded. All six have cbs = FALSE, and
# every area the expansion should reach reports its own commodity balances. The gate
# cannot simply replace the first route either — the twelve territories unfolded earlier
# are cbs = FALSE and would re-fold.
#
# This replaces a hardcoded `c("Eswatini")` exception with the property that made it an
# exception. It reaches one further area, 151, whose FAOSTAT_name is NA so no label match
# could ever succeed; its polity_area_code is already its own, so nothing moves.
areas_with_observed_data <- regions_for_crosswalk |>
  # `code`, not `area_code` — the latter only exists after the transmute below.
  dplyr::filter(!is.na(.data$code)) |>
  dplyr::select("code", "FAOSTAT_name", "name", "cbs") |>
  tidyr::pivot_longer(
    c("FAOSTAT_name", "name"),
    values_to = "label",
    values_drop_na = TRUE
  ) |>
  dplyr::filter(
    .data$label %in%
      alias_observed$source_label |
      (.data$cbs &
        .data$label %in% alias_observed_via_polity$source_label)
  ) |>
  dplyr::pull("code") |>
  as.integer() |>
  unique()

polity_area_crosswalk <- regions_for_crosswalk |>
  dplyr::transmute(
    area_code = as.integer(.data$code),
    area_name = dplyr::coalesce(
      .data$FAOSTAT_name,
      .data$name,
      .data$polity_name
    ),
    area_iso3c = .data$iso3c,
    # A PREFIX, not a code — named accordingly. This column used to be called
    # `reporting_polity_code` and reached the published crosswalk under that name
    # while holding 609 bare family prefixes and zero periodized codes: 206 of its
    # distinct values named no polity at all, so the join the docs point consumers
    # at returned nothing. The real code is `polity_code`, added by the polity_attrs
    # join below, and it is 609/609 valid.
    #
    # The old name was also a latent collision: .add_polity_columns_dt(prefix =
    # "reporting_") manufactures its own `reporting_polity_code` from the lookup's
    # `polity_code`, so the lookup carried two columns of that name meaning
    # different things. Package outputs happened to get the right one.
    reporting_polity_prefix = .data$polity_prefix,
    reporting_polity_name = .data$polity_name,
    cbs = .data$cbs,
    fabio_code = as.integer(.data$fabio_code),
    region = .data$region
  ) |>
  dplyr::left_join(manual_area_prefixes, by = "area_code") |>
  dplyr::mutate(
    area_iso3c_prefix = dplyr::if_else(
      .data$area_iso3c %in% known_polity_prefixes,
      .data$area_iso3c,
      NA_character_
    ),
    reporting_prefix = dplyr::if_else(
      .data$reporting_polity_prefix %in% known_polity_prefixes,
      .data$reporting_polity_prefix,
      NA_character_
    ),
    # FABIO folds many small territories into its Rest of World bucket, which is
    # carried by `polity_area_code` above (it takes `fabio_code`, so 999). Forcing
    # `polity_code` to ROW as well is redundant for FABIO and harmful for an area
    # that has data of its own: 4 of the 31 folded areas are flagged `cbs`, i.e.
    # they have their own commodity balance sheets — New Caledonia, North
    # Macedonia, Eswatini and Syria — and their CBS rows were resolving to
    # ROW-1850-2023 while a real polity for each exists upstream. Syria alone is
    # ~113k layer-B rows.
    #
    # So the ROW override applies only where the area has NO data of its own. The
    # other 27 keep folding, correctly: they carry no CBS data, so routing them
    # individually would diverge from FABIO for nothing.
    fabio_row_prefix = dplyr::if_else(
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        !(.data$cbs %in% TRUE) &
        !(.data$area_code %in% areas_with_observed_data),
      "ROW",
      NA_character_
    ),
    mapping_prefix = dplyr::coalesce(
      # `fabio_row_prefix` FIRST, so the rest-of-world fold outranks a derived override.
      #
      # This ordering only matters for one area, checked rather than assumed: of the twelve
      # areas needing an override, area 240 (United States Virgin Islands) is the only one that
      # is also a fold candidate. It has no observed data, so folding it is what the fold rule
      # says — "unfolding an area with no data would change no data while diverging from FABIO's
      # aggregation for nothing". With `manual` first, deriving the overrides from upstream
      # silently pulled 240 out of the fold, which test_region_continent_agreement.R caught.
      #
      # For every other area the two are never both set: an area with data has no
      # `fabio_row_prefix`, so its override still wins.
      .data$fabio_row_prefix,
      .data$manual_polity_prefix,
      .data$area_iso3c_prefix,
      .data$reporting_prefix,
      # Keep these last so unmatched reporting buckets remain visible.
      .data$reporting_polity_prefix,
      .data$area_iso3c
    )
  ) |>
  dplyr::left_join(
    polity_attrs,
    by = c("mapping_prefix" = "polity_prefix"),
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(
    # Takes `fabio_code` — which is 999 for every area FABIO folds into rest of world —
    # EXCEPT for areas that report data of their own. Without that exception the fold
    # removed at the polity level stayed in force at the numeric level, and the numeric
    # level is what the build actually keys on: get_primary_production() emits
    # `polity_area_code` as its `area_code`, so the Faroe Islands' 2,458 raw production
    # rows and Palestine's 9,606 were still being summed into area 999 and attributed to
    # ROW-1850-2023, even though the crosswalk resolved them to FRO-1800-2025 and
    # PSE-1948-2025.
    #
    # Two representations of the same decision, and only one of them had been fixed. A
    # smoke run against the real pins is what exposed it: the built output contained zero
    # rows for either area while the raw input had thousands.
    # The exception applies ONLY to the rest-of-world fold, i.e. fabio_code 999.
    #
    # My first version excluded data-reporting areas from `fabio_code` entirely, and that
    # broke something the redirect was quietly also doing: mapping a FORMER entity onto its
    # modern successor's FABIO code. Ethiopia PDR and Sudan (former) both carry their
    # successor's code, which is what kept exactly one area per ISO3 satisfying
    # `area_code == polity_area_code` — the `canonical` tie-break in read_raw_inputs.R.
    # Freeing them gave ETH and SDN two canonical areas each, and .iso3_to_fao_area_code()
    # aborts on precisely that ambiguity.
    #
    # So the condition names 999 explicitly. Un-folding rest-of-world is the intent; the
    # former-to-successor redirect is left alone.
    polity_area_code = dplyr::if_else(
      !is.na(.data$fabio_code) &
        !(.data$fabio_code == 999L &
          .data$area_code %in% areas_with_observed_data),
      .data$fabio_code,
      .data$area_code
    ),
    mapping_status = dplyr::case_when(
      !is.na(.data$manual_polity_prefix) & !is.na(.data$polity_code) ~ "manual",
      !is.na(.data$polity_code) ~ "matched",
      is.na(.data$area_code) ~ "not_a_reporting_area",
      TRUE ~ "unmapped"
    ),
    mapping_note = dplyr::case_when(
      !is.na(.data$manual_note) ~ .data$manual_note,
      # Only claim the fold happened when it actually did. This used to fire on any
      # area with fabio_code 999, so once areas carrying their own data stopped being
      # folded, their rows kept a note asserting they were collapsed into Rest of
      # World while pointing at BMU-1968-2025 or PSE-1948-2025 — a note contradicting
      # the mapping it annotates.
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        .data$area_code != 999L &
        !is.na(.data$polity_code) &
        startsWith(.data$polity_code, "ROW-") ~
        "FABIO collapses this source area into the Rest of World reporting polity.",
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        .data$area_code != 999L &
        !is.na(.data$polity_code) ~
        paste(
          "FABIO collapses this source area into Rest of World, but it reports data",
          "of its own, so WHEP routes it to its own polity instead."
        ),
      .data$mapping_status ==
        "unmapped" ~ "No real WHEP polity is available yet; treat this as a statistical reporting area without a polygon.",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::select(
    area_code,
    area_name,
    area_iso3c,
    reporting_polity_prefix,
    reporting_polity_name,
    cbs,
    fabio_code,
    region,
    polity_area_code,
    polity_code,
    polity_name,
    polity_start_year,
    polity_end_year,
    polity_type,
    iso3_code,
    cow_code,
    continent,
    wiki_status,
    polygon_status,
    has_geometry,
    mapping_status,
    mapping_note
  ) |>
  dplyr::arrange(.data$area_code, .data$polity_start_year, .data$polity_code)

usethis::use_data(items_cbs, overwrite = TRUE)
usethis::use_data(items_prod, overwrite = TRUE)
usethis::use_data(polities, overwrite = TRUE, compress = "xz")
usethis::use_data(polity_area_crosswalk, overwrite = TRUE)
usethis::use_data(polity_label_aliases, overwrite = TRUE)
