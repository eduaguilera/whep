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

# A DEAD POLITY MUST NOT BE A RESOLUTION CANDIDATE.
#
# Upstream retires a polity when a finer split supersedes it -- `F248-1920-1991`
# was retired once `F248-1920-1947` and `F248-1947-1991` replaced it -- and marks
# that in `wiki_status`. We carried the column through and never filtered on it, so
# a retired polity stayed a candidate and `(area, year)` resolution had two answers
# for the same year. Which one `add_polity_code()` returned depended on row order.
#
# Upstream already draws this line and publishes it both ways: the manifest carries
# `counts = {total, live, dead}`, and whep-polities' own matcher reports "excluded
# from matching: 26 dead polities (retired/superseded)". We were the only consumer
# ignoring it.
#
# The filter belongs HERE and not on `polities`. The published `polities` table keeps
# every row, because looking up a retired code is legitimate -- a consumer holding
# historical output needs to resolve the code it already has. What must not happen is
# resolving TO one.
#
# Inert for the COMMITTED snapshot and not for long. The committed `polities.rda` holds
# one `superseded` polity and it is not in the crosswalk, so regenerating against it
# changes no byte -- which is why this commit carries no `data/` diff. Run against the
# current upstream database it already excludes 27, and 22 of those are crosswalk
# candidates, so the filter becomes load-bearing the moment the snapshot is refreshed
# (#485). Measured rather than assumed: regenerating against the current upstream
# database reports excluding 27 dead polities and retaining 713.
#
# It does NOT fix every ambiguity. Two polities can be live and still overlap --
# Montenegro's MNE-1913-1915 and MNE-1913-1918 both cover 1913-1914 and are both
# marked draft upstream. No downstream filter can resolve that; it is filed upstream
# as whep-polities issue 62. The conflict detector added alongside this finds the
# class, and test_polity_resolution_uniqueness.R pins the known instance.
live_polity_attrs <- polity_attrs |>
  dplyr::filter(
    is.na(.data$wiki_status) |
      !.data$wiki_status %in% c("retired", "superseded")
  )
dropped_dead <- nrow(polity_attrs) - nrow(live_polity_attrs)
if (dropped_dead > 0L) {
  cli::cli_inform(
    "Excluded {dropped_dead} retired/superseded polities from resolution
     candidates; {nrow(live_polity_attrs)} remain."
  )
}

excel_na <- c("", "NA", "#N/A", "#DIV/0!", "#REF!")

# repair_table_labels(): shared with harmonization_tables.R, which reads the same
# vendored regions_full.csv to build regions_full and polities_cats.
source("data-raw/_labels.R")

regions_full_raw <- here::here(
  "inst",
  "extdata",
  "harmonization",
  "regions_full.csv"
) |>
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  repair_table_labels()

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
    live_polity_attrs,
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
