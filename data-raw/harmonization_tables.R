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
  readr::read_csv(show_col_types = FALSE, na = excel_na) |>
  # Two ADB region codes are blank in the vendored table while every other EU country
  # carries its two-letter code, and the EuropeAgriDB fodder pin reports under `AT` and
  # `GB`. The consequence was measurable: 2,030 of 23,183 fodder rows — 8.8% — resolved
  # to no area at all, because .read_fodder_euadb() bridges through ADB_Region.
  #
  # Filled here rather than in inst/extdata/harmonization/regions_full.csv, which is a
  # vendored file: patching it would diverge from its source, and this is the same
  # override pattern `manual_area_prefixes` already uses in table_mappings.R. `AT` and
  # `GB` are the ISO 3166-1 alpha-2 codes the pin uses, so neither is a guess.
  dplyr::mutate(
    ADB_Region = dplyr::case_when(
      is.na(.data$ADB_Region) & .data$code == 11 ~ "AT",
      is.na(.data$ADB_Region) & .data$code == 229 ~ "GB",
      .default = .data$ADB_Region
    )
  )

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

# Prefixes of every polity the crosswalk knows. Used to tell a real family key from
# a legacy one. Sound as a whitelist: every prefix here comes from an upstream
# polity_code, so nothing invalid can sneak in. It is a subset of all upstream
# prefixes (213 of 375), which only means the check is conservative.
crosswalk_polity_prefixes <- unique(
  sub(
    "-.*",
    "",
    polity_area_crosswalk$polity_code[
      !is.na(polity_area_crosswalk$polity_code)
    ]
  )
)

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
    # The continent comes from the polity, not from this table's own vendored
    # `region_UN` column. Those two disagree for 54 of 253 comparable areas, and
    # the vendored one is the unreliable side: area 228 "USSR" is filed under
    # Asia while area 185 "Russian Federation" — the same territorial family — is
    # filed under Europe. Upstream is consistent, putting all twelve F228/RUS
    # periods in Europe. Nothing in the package reads `region_UN`, so rather than
    # hand-patch one cell of a vendored table, publish the polity's own answer
    # next to it and let consumers prefer the sourced column.
    reporting_polity_continent = .data$continent,
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
        "reporting_polity_continent",
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
    # Repair `polity_prefix` values that name no polity at all. The vendored
    # tables carry legacy ISO3-shaped keys for three dissolved federations —
    # CSK, SUN, YUG — and upstream files those chains under F51, F228 and F248.
    # The `reporting_polity_code` on those same rows is already correct
    # (F51-1947-1993, F228-1945-1991, F248-1991-1992); only the family key
    # dangled, so grouping by `polity_prefix` to collect "every period of
    # Czechoslovakia" returned nothing and could not be joined to upstream.
    #
    # Repair only the DANGLING values, deliberately not all of them. Several
    # rows legitimately carry an aggregate prefix that differs from their own
    # reporting code — polities_cats files Bhutan under RASI and Comoros under
    # RAFR, folding them into rest-of-Asia and rest-of-Africa — and RASI/RAFR
    # are real upstream prefixes. Deriving every prefix from the reporting code
    # would silently undo those modelling choices.
    dplyr::mutate(
      polity_prefix = dplyr::if_else(
        !is.na(.data$reporting_polity_code) &
          !.data$polity_prefix %in% crosswalk_polity_prefixes,
        sub("-.*", "", .data$reporting_polity_code),
        .data$polity_prefix
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
