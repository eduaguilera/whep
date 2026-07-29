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
    confidence = readr::col_character()
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
manual_area_prefixes <- tibble::tribble(
  ~area_code, ~manual_polity_prefix, ~manual_note,
  51L, "F51", "FAOSTAT Czechoslovakia reporting area maps to WHEP Czechoslovakia polities.",
  228L, "F228", "FAOSTAT USSR reporting area maps to WHEP Russian Empire/USSR polities.",
  248L, "F248", "FAOSTAT Yugoslav SFR reporting area maps to WHEP Yugoslavia polities.",
  72L, "FRS", paste0(
    "FAOSTAT Djibouti (72) maps to the WHEP FRS chain (FRS-1884-1977 French ",
    "Somaliland, FRS-1977-2025 Djibouti). Needed because the DJI-1886-2025 row ",
    "that previously served this area was RETIRED upstream as a duplicate of ",
    "that chain, and excluding dead polities left the area with no mapping at ",
    "all. The iso3 prefix DJI no longer names a live polity."
  ),

  # -- Areas whose chain spans TWO prefixes ----------------------------------
  #
  # An area is mapped by prefix, and upstream gives the colonial and modern
  # polities of these seven chains DIFFERENT prefixes. Listing only the ISO3 one
  # left the colonial polity unreachable, so pre-independence FAOSTAT years fell
  # through to the modern polity: 1965 Angola resolved to AGO-1975-2025 "Angola
  # (independent, 1975-2025)", and 1970 Sudan to SDN-2011-2025 — post-secession
  # Sudan, which by definition EXCLUDES the territory that 1970 Sudan reported.
  # That is data attributed to a polity that did not exist, not data dropped.
  #
  # Both prefixes are listed per area, which the many-to-many join expands into
  # the union of both families. No year bounds are needed: every one of these
  # chains is contiguous and overlap-free (checked — Zimbabwe and Viet Nam even
  # interleave, ZWE/ZWE/SRH/ZWE/ZWE and VNM/F237/VNM, without overlapping), so
  # the year-aware resolution in add_polity_code() picks the right era on its own.
  #
  # Upstream's own FAOSTAT matching pipeline already assigns exactly these
  # polities; test_upstream_faostat_agreement.R compares the two. See whep#387 —
  # if the prefixes are ever unified upstream, these entries become redundant
  # rather than wrong.
  7L, "ANG", "Portuguese Angola (ANG-1800-1890 .. ANG-1905-1975) before the 1975 independence; AGO after.",
  7L, "AGO", "Independent Angola (AGO-1975-2025); the colonial era is the ANG chain.",
  20L, "BEC", "Bechuanaland Protectorate (BEC-1885-1966) before the 1966 independence.",
  20L, "BWA", "Botswana (BWA-1966-2025); the protectorate era is BEC.",
  181L, "SRH", "Southern Rhodesia (SRH-1953-1964), the federation era between ZWE-1900-1953 and ZWE-1964-1980.",
  181L, "ZWE", "Zimbabwe chain either side of the SRH federation era.",
  206L, "SUD", "Sudan before the 2011 secession (SUD-1899-1934 .. SUD-1956-2011), which INCLUDED present-day South Sudan.",
  206L, "SDN", "Sudan after the 2011 secession (SDN-2011-2025), which excludes South Sudan.",
  237L, "F237", "Partitioned Viet Nam (F237-1954-1975) between VNM-1887-1954 and VNM-1975-2025.",
  237L, "VNM", "Viet Nam either side of the 1954-1975 partition.",
  249L, "F249", "Yemen before the 1990 unification (F249-1918-1990).",
  249L, "YEM", "Unified Yemen (YEM-1990-2025).",
  251L, "NRH", "Northern Rhodesia (NRH-1911-1953, NRH-1953-1964) before the 1964 independence.",
  251L, "ZMB", "Zambia (ZMB-1964-2025); the protectorate era is the NRH chain."
)

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
        !(.data$cbs %in% TRUE),
      "ROW",
      NA_character_
    ),
    mapping_prefix = dplyr::coalesce(
      .data$manual_polity_prefix,
      .data$fabio_row_prefix,
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
    polity_area_code = dplyr::if_else(
      !is.na(.data$fabio_code),
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
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        .data$area_code !=
          999L ~ "FABIO collapses this source area into the Rest of World reporting polity.",
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
