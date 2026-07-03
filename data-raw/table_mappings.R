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

# Authoritative area_code -> polity prefix(es) from the curated alias table
# (whep-polities pipelines/faostat-era-matching). This REPLACES the historical
# prefix GUESS (the iso3c / reporting-code coalesce) for every area the
# reviewed table covers. An area can map to several polity chains over time
# (e.g. 206 "Sudan (former)" -> SUD pre-2011 then SDN), so this is a long
# area_code x prefix table, not a scalar per area. Areas the table does NOT
# cover keep the legacy coalesce fallback below — importantly the FABIO
# fabio_code == 999 -> Rest of World bucket, so no area that currently routes
# somewhere silently becomes NA (which the base build would drop).
alias_path <- Sys.getenv(
  "WHEP_POLITIES_ALIASES",
  unset = path.expand(
    "~/whep-polities/pipelines/faostat-era-matching/state/faostat_aliases.csv"
  )
)
alias_area_prefix <- if (file.exists(alias_path)) {
  readr::read_csv(alias_path, show_col_types = FALSE) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      mapping_prefix = sub("-.*", "", .data$target_polity_code)
    ) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$mapping_prefix)) |>
    dplyr::distinct()
} else {
  warning(
    "Curated alias table not found at ",
    alias_path,
    "; falling back to the legacy prefix-guess crosswalk. ",
    "Set WHEP_POLITIES_ALIASES or check out whep-polities."
  )
  tibble::tibble(area_code = integer(), mapping_prefix = character())
}
covered_areas <- unique(alias_area_prefix$area_code)

# Per-area metadata + the legacy fallback prefix, used only where the alias
# table has no entry (aggregates like 351, and areas with no polity yet).
area_meta <- regions_for_crosswalk |>
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
  dplyr::distinct(.data$area_code, .keep_all = TRUE) |>
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
    fallback_prefix = dplyr::coalesce(
      .data$manual_polity_prefix,
      .data$fabio_row_prefix,
      .data$area_iso3c_prefix,
      .data$reporting_prefix,
      .data$reporting_polity_code,
      .data$area_iso3c
    )
  )

# Long area_code -> mapping_prefix: the curated alias where it exists, else
# the legacy fallback for uncovered areas.
area_prefix_long <- dplyr::bind_rows(
  alias_area_prefix,
  area_meta |>
    dplyr::filter(!.data$area_code %in% covered_areas) |>
    dplyr::transmute(.data$area_code, mapping_prefix = .data$fallback_prefix)
) |>
  dplyr::distinct()

polity_area_crosswalk <- area_meta |>
  dplyr::select(
    area_code,
    area_name,
    area_iso3c,
    reporting_polity_code,
    reporting_polity_name,
    cbs,
    fabio_code,
    region
  ) |>
  dplyr::left_join(
    area_prefix_long,
    by = "area_code",
    relationship = "many-to-many"
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
      is.na(.data$area_code) ~ "not_a_reporting_area",
      .data$area_code %in%
        covered_areas &
        !is.na(.data$polity_code) ~ "matched",
      !is.na(.data$polity_code) ~ "matched",
      TRUE ~ "unmapped"
    ),
    mapping_note = dplyr::case_when(
      .data$area_code %in%
        covered_areas &
        !is.na(
          .data$polity_code
        ) ~ "Mapped via the curated faostat-era alias table (whep-polities).",
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
usethis::use_data(polities, overwrite = TRUE)
usethis::use_data(polity_area_crosswalk, overwrite = TRUE)
