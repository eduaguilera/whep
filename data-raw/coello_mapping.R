# Generates the coello_mapping CSV that maps FAOSTAT item_prod_code to
# Coello et al. (2025) crop groups.
#
# Coello et al. (2025) "A global gridded crop-specific fertilization
# dataset from 1961 to 2019". doi:10.1038/s41597-024-04215-x
#
# 13 crop groups:
#   1_1 Wheat, 1_2 Maize, 1_3 Rice, 1_4 Other cereals,
#   2_1 Soybean, 2_2 Palm, 2_3 Other oilseeds,
#   3_1 Vegetables, 3_2 Fruits,
#   4 Roots and tubers, 5 Sugar crops, 6 Fiber crops, 7 Other crops
#
# The mapping is stored in inst/extdata/coello_mapping.csv and used by
# inst/scripts/prepare_nitrogen_inputs.R to expand crop-group rates to
# individual FAOSTAT items.

coello_mapping <- readr::read_csv(
  here::here("inst", "extdata", "coello_mapping.csv"),
  col_types = readr::cols(
    item_prod_code = readr::col_integer(),
    item_prod_name = readr::col_character(),
    coello_crop_code = readr::col_character(),
    coello_crop_name = readr::col_character()
  )
)

# Verify all crop groups are represented
stopifnot(
  setequal(
    unique(coello_mapping$coello_crop_code),
    c("1_1", "1_2", "1_3", "1_4", "2_1", "2_2", "2_3",
      "3_1", "3_2", "4", "5", "6", "7")
  )
)

cat(
  sprintf(
    "Coello mapping: %d items -> %d crop groups\n",
    nrow(coello_mapping),
    length(unique(coello_mapping$coello_crop_code))
  )
)

# Print summary by group
coello_mapping |>
  dplyr::count(coello_crop_code, coello_crop_name, name = "n_items") |>
  print(n = 13)
