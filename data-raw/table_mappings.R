items_cbs <- here::here("inst", "extdata", "items_cbs.csv") |>
  readr::read_csv()

items_prod <- here::here("inst", "extdata", "items_prod.csv") |>
  readr::read_csv()

whep_polities_gpkg <- Sys.getenv(
  "WHEP_POLITIES_GPKG",
  unset = path.expand("~/whep-polities/data/final/polities_database.gpkg")
)

polities <- sf::st_read(whep_polities_gpkg, quiet = TRUE)
polities$iso3c <- polities$iso3_code
polities$has_geometry <- !sf::st_is_empty(polities)

polity_attrs <- polities |>
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
# second authority for label -> polity, which is exactly what misattributed
# FAOSTAT data in #387.
whep_label_alias_map <- Sys.getenv(
  "WHEP_POLITIES_LABEL_ALIAS_MAP",
  unset = path.expand("~/whep-polities/data/final/label_alias_map.csv")
)

# Fail with an explanation rather than readr's bare "does not exist". This file
# is published by whep-polities and arrives with lbm364dl/whep-polities#39,
# whereas polities_database.gpkg is already on that repo's main -- so this is the
# one upstream artifact a regeneration can be missing, and the raw error names a
# path without saying what provides it.
if (!file.exists(whep_label_alias_map)) {
  cli::cli_abort(c(
    "The published label alias map is missing.",
    x = "Looked for {.path {whep_label_alias_map}}.",
    i = paste(
      "It is published by whep-polities as",
      "{.path data/final/label_alias_map.csv} and gated there by",
      "{.code scripts/write_label_alias_map.py --check}."
    ),
    i = paste(
      "If that repository is checked out elsewhere, point",
      "{.envvar WHEP_POLITIES_LABEL_ALIAS_MAP} at the file."
    ),
    i = paste(
      "The committed data/polity_label_aliases.rda already carries the",
      "aliases, so only regeneration is affected, not use."
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
    # How many source rows were actually observed for this label, 0 when the
    # label is merely mappable. Declared explicitly because this col_types list
    # is exhaustive by intent -- an upstream column that is not named here is a
    # column this script cannot see.
    observed_rows = readr::col_double()
  )
)

# Every published alias must name a polity the upstream database carries.
# Upstream gates the same invariant, so a failure here means the alias map and
# the GeoPackage were taken from different revisions rather than that the map is
# wrong. Checked against the freshly read `polities`, not against the committed
# data/polities.rda, because the two are regenerated together from this script.
unknown_alias_targets <- setdiff(
  polity_label_aliases$polity_code,
  polities$polity_code
)
if (length(unknown_alias_targets) > 0L) {
  cli::cli_abort(c(
    "The published label alias map targets polities this package cannot carry.",
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
      polity_code = .data$iso3c,
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
  248L, "F248", "FAOSTAT Yugoslav SFR reporting area maps to WHEP Yugoslavia polities."
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
    reporting_polity_code = .data$polity_code,
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
      .data$reporting_polity_code %in% known_polity_prefixes,
      .data$reporting_polity_code,
      NA_character_
    ),
    fabio_row_prefix = dplyr::if_else(
      !is.na(.data$fabio_code) & .data$fabio_code == 999L,
      "ROW",
      NA_character_
    ),
    mapping_prefix = dplyr::coalesce(
      .data$manual_polity_prefix,
      .data$fabio_row_prefix,
      .data$area_iso3c_prefix,
      .data$reporting_prefix,
      # Keep these last so unmatched reporting buckets remain visible.
      .data$reporting_polity_code,
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
    reporting_polity_code,
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
