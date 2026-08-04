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

regions_full_raw <- here::here(
  "inst",
  "extdata",
  "harmonization",
  "regions_full.csv"
) |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

regions_compact <- here::here("inst", "extdata", "regions.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

# The area codes the package's own compact country grid treats as individually
# modelled territories. Consulted below to decide which FABIO rest-of-world
# folds may override an area's own polity: an area this package models as a
# country cannot honestly be identified as a non-territorial aggregate.
grid_area_codes <- as.integer(regions_compact$area_code)

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
#
# The three Sudan areas are here for a different reason: they need TWO prefixes
# each, and more than one row per area is how this table says so (the prefix join
# below fans out, and each prefix contributes its own periods). Their ISO3 alone
# supplies only the post-2011 successor -- SDN for 206 "Sudan (former)" and 276
# "Sudan", SSD for 277 "South Sudan" -- so every pre-2011 year missed the
# year-aware join entirely and was rescued by its nearest-match fallback onto a
# state that did not exist yet: a 1990 figure for any of the three came back as
# SDN-2011-2025 or SSD-2011-2025. Adding SUD gives all three the unified-Sudan
# chain (SUD-1899-1934, SUD-1934-1956, SUD-1956-2011) for the years it covers,
# while the successor prefix keeps 2011 onwards.
#
# The numeric bucket is untouched: all three keep fabio_code 206, so
# polity_area_code still folds them together. Whether that bucket's post-2011
# value (Sudan and South Sudan summed) can honestly carry a one-territory polity
# is whep#414 and is not decided here.
manual_area_prefixes <- tibble::tribble(
  ~area_code, ~manual_polity_prefix, ~manual_note,
  51L, "F51", "FAOSTAT Czechoslovakia reporting area maps to WHEP Czechoslovakia polities.",
  206L, "SUD", "FAOSTAT Sudan (former) reporting area resolves across the 2011 secession.",
  206L, "SDN", "FAOSTAT Sudan (former) reporting area resolves across the 2011 secession.",
  228L, "F228", "FAOSTAT USSR reporting area maps to WHEP Russian Empire/USSR polities.",
  248L, "F248", "FAOSTAT Yugoslav SFR reporting area maps to WHEP Yugoslavia polities.",
  276L, "SUD", "FAOSTAT Sudan reporting area resolves across the 2011 secession.",
  276L, "SDN", "FAOSTAT Sudan reporting area resolves across the 2011 secession.",
  277L, "SUD", "FAOSTAT South Sudan reporting area resolves across the 2011 secession.",
  277L, "SSD", "FAOSTAT South Sudan reporting area resolves across the 2011 secession."
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
    # FABIO folds many small territories into its Rest of World bucket. That fold
    # is carried by `polity_area_code` below, which takes `fabio_code` and so
    # stays 999 for every one of them. Forcing `polity_code` to ROW as well is
    # redundant there and destructive for an area this package models as a
    # country in its own right: five codes of the compact country grid --
    # 61 Equatorial Guinea, 153 New Caledonia, 154 North Macedonia, 209 Eswatini
    # and 212 Syrian Arab Republic -- were identified as ROW-1850-2023, a
    # non-territorial aggregate with no borders of its own, for every year, while
    # `polities` carries a real dedicated polity for each.
    #
    # So the ROW override yields to an area that is BOTH in the grid and has a
    # polity of its own. Every other folded area keeps folding: it is not
    # individually modelled here, so routing it to its own polity would diverge
    # from FABIO's aggregation for nothing (that trade-off, at the numeric level,
    # is whep#419). Grid areas 69 French Guiana and 299 Palestine keep folding
    # too, because no GUF/PSE polity exists in this vintage of `polities` to
    # route them to.
    grid_country_prefix = dplyr::if_else(
      .data$area_code %in% grid_area_codes,
      .data$area_iso3c_prefix,
      NA_character_
    ),
    fabio_row_prefix = dplyr::if_else(
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        is.na(.data$grid_country_prefix),
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
      # Only claim the fold happened where it actually did. This used to fire on
      # any area with fabio_code 999, so the grid countries that stopped being
      # folded kept a note asserting they were collapsed into Rest of World while
      # pointing at SYR-1967-2025 or SWZ-1894-2025 -- a note contradicting the
      # mapping it annotates.
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        .data$area_code != 999L &
        startsWith(.data$polity_code, "ROW-") ~
        "FABIO collapses this source area into the Rest of World reporting polity.",
      !is.na(.data$fabio_code) &
        .data$fabio_code == 999L &
        .data$area_code != 999L &
        !is.na(.data$polity_code) ~
        paste(
          "FABIO collapses this source area into Rest of World, but WHEP models",
          "it as a country of its own, so it keeps its own polity."
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
