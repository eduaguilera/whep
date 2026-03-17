harmonization_dir <- here::here("inst", "extdata", "harmonization")

# Direct reads ----------------------------------------------------------------

regions_full <- file.path(harmonization_dir, "regions_full.csv") |>
  readr::read_csv(show_col_types = FALSE)

items_full <- file.path(harmonization_dir, "items_full.csv") |>
  readr::read_csv(show_col_types = FALSE)

cb_processing <- file.path(harmonization_dir, "cb_processing.csv") |>
  readr::read_csv(show_col_types = FALSE)

primary_double <- file.path(harmonization_dir, "primary_double.csv") |>
  readr::read_csv(show_col_types = FALSE)

cbs_trade_codes <- file.path(harmonization_dir, "cbs_trade_codes.csv") |>
  readr::read_csv(show_col_types = FALSE)

polities_cats <- file.path(harmonization_dir, "polities_cats.csv") |>
  readr::read_csv(show_col_types = FALSE)

animals_codes <- file.path(harmonization_dir, "animals_codes.csv") |>
  readr::read_csv(show_col_types = FALSE)

liv_lu_coefs <- file.path(harmonization_dir, "liv_lu_coefs.csv") |>
  readr::read_csv(show_col_types = FALSE)

crops_eurostat <- file.path(harmonization_dir, "crops_eurostat.csv") |>
  readr::read_csv(show_col_types = FALSE)

biomass_coefs <- file.path(harmonization_dir, "biomass_coefs.csv") |>
  readr::read_csv(show_col_types = FALSE)

# Derived: items_prod_full ----------------------------------------------------

names_cats_raw <- file.path(harmonization_dir, "names_cats.csv") |>
  readr::read_csv(show_col_types = FALSE)

items_prod_full_raw <- file.path(
  harmonization_dir,
  "items_prod_full_raw.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

if (!"Fallow" %in% as.character(items_prod_full_raw$item_code_prod)) {
  items_prod_full_raw <- items_prod_full_raw |>
    dplyr::mutate(item_code_prod = as.character(item_code_prod))
  fallow_row <- items_prod_full_raw[1, ]
  fallow_row[1, ] <- NA
  fallow_row$item_prod <- "Fallow"
  fallow_row$item_code_prod <- "Fallow"
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
      item_code_prod,
      item_cbs,
      item_code_cbs,
      Farm_class,
      Cat_Labour,
      Cat_FAO1
    ) |>
    dplyr::filter(!is.na(item_cbs)),
  items_full |>
    dplyr::filter(comm_group == "Live animals") |>
    dplyr::select(item_code_cbs, item_cbs)
) |>
  dplyr::left_join(
    items_full |> dplyr::select(item_code_cbs, group),
    by = "item_code_cbs"
  ) |>
  dplyr::left_join(
    animals_codes |>
      dplyr::select(item_code_cbs, Farm_class, Cat_Labour, Cat_FAO1) |>
      dplyr::rename(
        Farm_class2 = Farm_class,
        Cat_Labour2 = Cat_Labour,
        Cat_FAO12 = Cat_FAO1
      ),
    by = "item_code_cbs"
  ) |>
  dplyr::mutate(
    Farm_class = dplyr::coalesce(Farm_class, Farm_class2),
    Cat_Labour = dplyr::coalesce(Cat_Labour, Cat_Labour2),
    Cat_FAO1 = dplyr::coalesce(Cat_FAO1, Cat_FAO12)
  ) |>
  dplyr::select(-Farm_class2, -Cat_Labour2, -Cat_FAO12)

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
