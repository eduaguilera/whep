lassaletta_grassland_share <- here::here(
  "inst", "extdata", "Synthetic_N_Grassland_share.csv"
) |>
  readr::read_csv2(show_col_types = FALSE)

crops_manure_n <- here::here(
  "inst", "extdata", "Crops_manure_N.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

mueller_synthetic_n <- here::here(
  "inst", "extdata", "mueller_synthetic_n.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

usethis::use_data(
  lassaletta_grassland_share,
  crops_manure_n,
  mueller_synthetic_n,
  overwrite = TRUE
)
