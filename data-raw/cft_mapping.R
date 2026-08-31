# Generates the cft_mapping dataset for use as exported package data.
# The mapping connects FAOSTAT item codes to LPJmL crop functional types.
# Source: LandInG crop_types_FAOSTAT_LPJmL_default.csv, adapted.

cft_mapping <- here::here("inst", "extdata", "cft_mapping.csv") |>
  readr::read_csv(
    col_types = readr::cols(
      item_prod_code = readr::col_integer(),
      item_prod_name = readr::col_character(),
      cft_name = readr::col_character(),
      cft_lpjml = readr::col_character(),
      luh2_type = readr::col_character()
    )
  )

# Catch a duplicated item_prod_code here, at build time, rather than only at
# first use: this table ships as package data, so a bad CSV edit should fail
# the build, not a user's `build_gridded_landuse()` call (#224). The runtime
# guard in R/spatialize.R and R/run_spatialize.R stays too -- it also covers
# a caller-supplied `config$cft_mapping`, which never passes through here.
whep:::.assert_unique_cft_mapping(cft_mapping)

usethis::use_data(cft_mapping, overwrite = TRUE)
