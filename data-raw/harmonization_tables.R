harmonization_dir <- here::here("inst", "extdata", "harmonization")

# Excel exports use #N/A, #DIV/0!, and #REF! for missing/error cells
excel_na <- c("", "NA", "#N/A", "#DIV/0!", "#REF!")

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
  readr::read_csv(show_col_types = FALSE, na = excel_na)

items_full <- file.path(harmonization_dir, "items_full.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

cb_processing <- file.path(harmonization_dir, "cb_processing.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

primary_double <- file.path(harmonization_dir, "primary_double.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

cbs_trade_codes <- file.path(harmonization_dir, "cbs_trade_codes.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

polities_cats <- file.path(harmonization_dir, "polities_cats.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  dplyr::select(!dplyr::starts_with("0..."))

if (!exists("polity_area_crosswalk")) {
  load(here::here("data", "polity_area_crosswalk.rda"))
}

current_area_polities <- polity_area_crosswalk |>
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
    # A prefix, not a code: the ISO3-shaped family key. Used only to fill
    # `polity_prefix` for area codes the crosswalk does not resolve.
    legacy_polity_prefix = sub("-.*", "", .data$polity_code)
  )

add_current_area_polities <- function(table) {
  table |>
    dplyr::select(
      -dplyr::any_of(c(
        "polity_area_code",
        "reporting_polity_code",
        "reporting_polity_name",
        "reporting_polity_has_geometry",
        "legacy_polity_prefix"
      ))
    ) |>
    dplyr::mutate(code = as.integer(.data$code)) |>
    dplyr::left_join(current_area_polities, by = "code") |>
    dplyr::mutate(
      polity_prefix = dplyr::coalesce(
        .data$polity_prefix,
        .data$legacy_polity_prefix
      ),
      polity_name = dplyr::coalesce(
        .data$polity_name,
        .data$reporting_polity_name
      )
    ) |>
    dplyr::select(-"legacy_polity_prefix")
}

regions_full <- add_current_area_polities(regions_full)
polities_cats <- add_current_area_polities(polities_cats)

animals_codes <- file.path(harmonization_dir, "animals_codes.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

liv_lu_coefs <- file.path(harmonization_dir, "liv_lu_coefs.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

crops_eurostat <- file.path(harmonization_dir, "crops_eurostat.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na)

biomass_coefs <- file.path(harmonization_dir, "biomass_coefs.csv") |>
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  dplyr::select(!dplyr::starts_with("..."))

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
