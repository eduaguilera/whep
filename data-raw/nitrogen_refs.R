lassaletta_grassland_share <- here::here(
  "inst",
  "extdata",
  "Synthetic_N_Grassland_share.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

crops_manure_n <- here::here(
  "inst",
  "extdata",
  "Crops_manure_N.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

mueller_synthetic_n <- here::here(
  "inst",
  "extdata",
  "mueller_synthetic_n.csv"
) |>
  readr::read_csv(show_col_types = FALSE)

# Smil (2001) "Enriching the Earth", Tables 5.2 and 5.3 (Smil 2002,
# Ambio 31:126-131 cross-check). Global synthetic-nitrogen production
# anchors 1913-2000 (kt N). Pre-1913 = 0 by construction. Used by
# prepare_nitrogen_inputs() to backcast country-level synthetic N
# before the FAOSTAT envelope (1961+).
smil_2001_synthetic_n_global <- here::here(
  "inst",
  "extdata",
  "smil_2001_synthetic_n_global.csv"
) |>
  readr::read_csv(
    show_col_types = FALSE,
    col_types = readr::cols(
      year = readr::col_integer(),
      global_kt_n = readr::col_double()
    )
  )

usethis::use_data(
  lassaletta_grassland_share,
  crops_manure_n,
  mueller_synthetic_n,
  smil_2001_synthetic_n_global,
  overwrite = TRUE
)
