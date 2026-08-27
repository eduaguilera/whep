harmonization_dir <- here::here("inst", "extdata", "harmonization")

# Excel exports use #N/A, #DIV/0!, and #REF! for missing/error cells
excel_na <- c("", "NA", "#N/A", "#DIV/0!", "#REF!")

# repair_table_labels(): shared with table_mappings.R, which re-reads the same
# vendored regions_full.csv to build the crosswalk.
source("data-raw/_labels.R")

# Guard against non-unique join keys and duplicate rows in mapping tables.
# A non-unique key silently fans out downstream joins; duplicate rows
# double-count. Fail loudly at build time so bad data never ships.
.assert_unique_key <- function(table, key, name) {
  values <- table[[key]]
  dup_keys <- unique(values[!is.na(values) & duplicated(values)])
  if (length(dup_keys) > 0) {
    cli::cli_abort(c(
      "Non-unique key {.field {key}} in {.val {name}}.",
      "x" = "Duplicated value{?s}: {.val {dup_keys}}."
    ))
  }
  dup_rows <- nrow(table) - nrow(dplyr::distinct(table))
  if (dup_rows > 0) {
    cli::cli_abort(c(
      "Duplicate rows in {.val {name}}.",
      "x" = "{dup_rows} fully duplicated row{?s} found."
    ))
  }
  invisible(table)
}

# Direct reads ----------------------------------------------------------------

regions_full <- file.path(harmonization_dir, "regions_full.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  repair_table_labels()

items_full <- file.path(harmonization_dir, "items_full.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

cb_processing <- file.path(harmonization_dir, "cb_processing.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

primary_double <- file.path(harmonization_dir, "primary_double.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

cbs_trade_codes <- file.path(harmonization_dir, "cbs_trade_codes.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

if (!exists("polity_area_crosswalk")) {
  load(here::here("data", "polity_area_crosswalk.rda"))
}

# A REST-OF-WORLD MEMBER CARRIES TWO ANSWERS, and only one of them is the
# present-day carrier. Since whep#717 the shipped crosswalk holds both the
# bucket's `fabio_row_fold` row (`ROW-1850-2025`, spanning 1850-2025) and, for
# the 31 areas upstream names, the member's own `fabio_row_promoted` periods.
# `.unfold_rest_of_world()` keeps the promoted rows under the default mode, so
# that is what `add_polity_code()` resolves and what this column must equal --
# `test_territorial_identity.R` re-executes the resolver and compares.
#
# Stated as a filter rather than left to the ordering below, which would reach
# the right row today only by accident: `SYR-1967-2025` outranks
# `ROW-1850-2025` on `polity_start_year` alone, and an area whose own period
# started before 1850 would silently take the bucket's label instead.
row_promoted_areas <- polity_area_crosswalk$area_code[
  polity_area_crosswalk$mapping_source == "fabio_row_promoted"
]

current_area_polities <- polity_area_crosswalk |>
  dplyr::filter(
    !(.data$mapping_source == "fabio_row_fold" &
      .data$area_code %in% row_promoted_areas)
  ) |>
  dplyr::filter(!is.na(.data$area_code), !is.na(.data$polity_code)) |>
  dplyr::mutate(
    current_or_latest = !is.na(.data$polity_end_year) &
      .data$polity_end_year >= 2025
  ) |>
  dplyr::arrange(
    .data$area_code,
    dplyr::desc(.data$current_or_latest),
    dplyr::desc(.data$polity_end_year),
    dplyr::desc(.data$polity_start_year)
  ) |>
  dplyr::distinct(.data$area_code, .keep_all = TRUE) |>
  dplyr::transmute(
    code = as.integer(.data$area_code),
    polity_area_code = .data$polity_area_code,
    reporting_polity_code = .data$polity_code,
    reporting_polity_name = .data$polity_name,
    reporting_polity_has_geometry = .data$has_geometry,
    crosswalk_polity_prefix = sub("-.*", "", .data$polity_code)
  )

# `legacy_polity_prefix` is the ISO3-LIKE STEM the vendored table has always
# carried ("AFG", "ROW", "RAFR"), and it is not a polity code: not one of its
# values is in `polities$polity_code`. It was named `polity_code` until #687,
# where the name promising an identity the column does not hold made every join
# from these tables to `polities` or `polity_area_crosswalk` come back empty
# with nothing warning. The real carrier is `reporting_polity_code`, resolved
# just above. The stem is still filled from the crosswalk where the vendored
# table leaves it NA, so the coalesce below is the only place the two meet.
add_current_area_polities <- function(table) {
  table |>
    dplyr::select(
      -dplyr::any_of(c(
        "polity_area_code",
        "reporting_polity_code",
        "reporting_polity_name",
        "reporting_polity_has_geometry",
        "crosswalk_polity_prefix"
      ))
    ) |>
    dplyr::mutate(code = as.integer(.data$code)) |>
    dplyr::left_join(current_area_polities, by = "code") |>
    dplyr::mutate(
      legacy_polity_prefix = dplyr::coalesce(
        .data$legacy_polity_prefix,
        .data$crosswalk_polity_prefix
      ),
      polity_name = dplyr::coalesce(
        .data$polity_name,
        .data$reporting_polity_name
      )
    ) |>
    dplyr::select(-"crosswalk_polity_prefix")
}

regions_full <- add_current_area_polities(regions_full)

# Derived: polities_cats ------------------------------------------------------

# polities_cats is a row-filtered view of regions_full: the same columns, and
# every one of its 198 area codes is one of regions_full's. It used to be read
# from a second vendored CSV carrying its own copy of all 39 columns, so the two
# copies drifted: 17 columns disagreed over the 198 shared codes (#406). 95 of
# those cells were an encoding artefact -- the literal string "0" in eia, iea
# and eleven region_ columns where regions_full leaves NA -- and none of the
# disagreements was reaching a computation, because no package code reads
# polities_cats.
#
# Only the membership is read from that CSV now; every column value comes from
# regions_full, so a repair or an override applied there can no longer miss the
# subset, and the deliberate difference below is the only difference left. The
# membership itself stays vendored because it encodes no rule: the closest
# predicate in regions_full is cbs == TRUE, and 18 areas contradict it, 7 in the
# subset with cbs FALSE and 11 with cbs TRUE left out of it.
polities_cats_codes <- file.path(harmonization_dir, "polities_cats.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  dplyr::pull("code") |>
  as.integer()

unknown_codes <- setdiff(polities_cats_codes, regions_full$code)
if (length(unknown_codes) > 0) {
  cli::cli_abort(c(
    "Area codes in {.file polities_cats.csv} are absent from regions_full.",
    "x" = "Unknown codes: {.val {unknown_codes}}."
  ))
}

# polities_cats files Bhutan under rest-of-Asia and Comoros under
# rest-of-Africa, where regions_full models both individually: neither country
# had a commodity balance sheet in the CBS vintage the table was compiled
# against. This is the one difference between the two tables that is a modelling
# choice rather than drift, so it is stated here in code instead of being
# implied by a second copy of the table. Whether it still holds now that the
# faostat-cbs-new pin carries 91 rows for Bhutan and 135 for Comoros is #395,
# and is deliberately not decided here: these values are exactly what the table
# shipped before it became derived.
rest_of_world_folds <- tibble::tribble(
  ~code, ~legacy_polity_prefix, ~polity_name, ~cbs, ~fabio_code,
  18L, "RASI", "Asia Other", FALSE, 999,
  45L, "RAFR", "Africa Other", FALSE, 999
)

polities_cats <- regions_full |>
  dplyr::filter(.data$code %in% polities_cats_codes) |>
  # Keep the vendored row order, so deriving the table does not reshuffle it.
  dplyr::arrange(match(.data$code, polities_cats_codes)) |>
  dplyr::rows_update(rest_of_world_folds, by = "code", unmatched = "error")

animals_codes <- file.path(harmonization_dir, "animals_codes.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

liv_lu_coefs <- file.path(harmonization_dir, "liv_lu_coefs.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

crops_eurostat <- file.path(harmonization_dir, "crops_eurostat.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

# Three all-caps rows in the upstream Biomass_coefs.xlsx are section headers of
# the spreadsheet, not commodities (#752). TRANSFORMED PRODUCTS and
# AGRO-INDUSTRY BYPRODUCTS are entirely empty. ANIMAL PRODUCTS is worse: its
# only seven populated cells hold 2, 4, 3, 5, 6, 7, 8 -- the column-index
# vector the Coefs sheet's VLOOKUPs address absolutely, so read as data it
# claims an Edible_portion of 4 and 3 kg of nitrogen per kg of fresh matter.
# It cannot be cleaned upstream without breaking the workbook, so it is
# dropped here instead, once, rather than at each consumer.
biomass_coefs_headers <- c(
  "TRANSFORMED PRODUCTS",
  "AGRO-INDUSTRY BYPRODUCTS",
  "ANIMAL PRODUCTS"
)

biomass_coefs <- file.path(harmonization_dir, "biomass_coefs.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  dplyr::select(!dplyr::starts_with("...")) |>
  dplyr::filter(!.data$Name_biomass %in% biomass_coefs_headers)

# Derived: items_prod_full ----------------------------------------------------

names_cats_raw <- file.path(harmonization_dir, "names_cats.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

items_prod_full_raw <- file.path(
  harmonization_dir,
  "items_prod_full_raw.csv"
) |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

if (!"Fallow" %in% as.character(items_prod_full_raw$item_prod_code)) {
  items_prod_full_raw <- items_prod_full_raw |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code))
  fallow_row <- items_prod_full_raw[1, ]
  fallow_row[1, ] <- NA
  fallow_row$item_prod <- "Fallow"
  fallow_row$item_prod_code <- "Fallow"
  fallow_row$Name <- "Fallow"
  fallow_row$Name_biomass <- "Fallow"
  fallow_row$group <- "Primary crops"
  items_prod_full_raw <- dplyr::bind_rows(items_prod_full_raw, fallow_row)
}

items_prod_full <- items_prod_full_raw |>
  dplyr::left_join(
    names_cats_raw |> dplyr::select(-Order, -Farm_class),
    by = "Name"
  )

# Derived: items_prim ---------------------------------------------------------

items_prim <- dplyr::bind_rows(
  items_prod_full |>
    dplyr::select(
      item_prod,
      item_prod_code,
      item_cbs,
      item_cbs_code,
      Farm_class,
      Cat_Labour,
      Cat_FAO1
    ) |>
    dplyr::filter(!is.na(item_cbs)),
  items_full |>
    dplyr::filter(comm_group == "Live animals") |>
    dplyr::select(item_cbs_code, item_cbs)
) |>
  dplyr::left_join(
    items_full |> dplyr::select(item_cbs_code, group),
    by = "item_cbs_code"
  ) |>
  dplyr::left_join(
    animals_codes |>
      dplyr::select(item_cbs_code, Farm_class, Cat_Labour, Cat_FAO1) |>
      dplyr::rename(
        Farm_class2 = Farm_class,
        Cat_Labour2 = Cat_Labour,
        Cat_FAO12 = Cat_FAO1
      ),
    by = "item_cbs_code"
  ) |>
  dplyr::mutate(
    Farm_class = dplyr::coalesce(Farm_class, Farm_class2),
    Cat_Labour = dplyr::coalesce(Cat_Labour, Cat_Labour2),
    Cat_FAO1 = dplyr::coalesce(Cat_FAO1, Cat_FAO12)
  ) |>
  dplyr::select(-Farm_class2, -Cat_Labour2, -Cat_FAO12)

# Integrity guards ------------------------------------------------------------

.assert_unique_key(items_prod_full, "item_prod_code", "items_prod_full")
.assert_unique_key(items_full, "item_cbs_code", "items_full")
.assert_unique_key(regions_full, "code", "regions_full")
.assert_unique_key(cbs_trade_codes, "item_code_trade", "cbs_trade_codes")
.assert_unique_key(animals_codes, "item_cbs_code", "animals_codes")

# Save as package data --------------------------------------------------------

usethis::use_data(regions_full, overwrite = TRUE)
usethis::use_data(items_full, overwrite = TRUE)
usethis::use_data(cb_processing, overwrite = TRUE)
usethis::use_data(primary_double, overwrite = TRUE)
usethis::use_data(cbs_trade_codes, overwrite = TRUE)
usethis::use_data(polities_cats, overwrite = TRUE)
usethis::use_data(animals_codes, overwrite = TRUE)
usethis::use_data(liv_lu_coefs, overwrite = TRUE)
usethis::use_data(crops_eurostat, overwrite = TRUE)
usethis::use_data(biomass_coefs, overwrite = TRUE)
usethis::use_data(items_prod_full, overwrite = TRUE)
usethis::use_data(items_prim, overwrite = TRUE)
