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

manual_area_prefixes <- tibble::tribble(
  ~area_code, ~manual_polity_prefix, ~manual_note,
  351L, "CHN", "Legacy generic China reporting area maps to WHEP China polities.",
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
usethis::use_data(polities, overwrite = TRUE)
usethis::use_data(polity_area_crosswalk, overwrite = TRUE)
