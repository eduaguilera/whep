# Generates the cft_mapping dataset for use as exported package data.
# The mapping connects FAOSTAT item codes to LPJmL crop functional types.
# Source: LandInG crop_types_FAOSTAT_LPJmL_default.csv, adapted.

cft_mapping <- here::here("inst", "extdata", "cft_mapping.csv") |>
  readr::read_csv(
    col_types = readr::cols(
      item_prod_code = readr::col_integer(),
      item_prod_name = readr::col_character(),
      cft_name = readr::col_character()
    )
  )

usethis::use_data(cft_mapping, overwrite = TRUE)
