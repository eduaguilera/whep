# Generates the regime_yield_crop_mapping dataset for use as exported package
# data: every primary crop item_prod_code -> the SPAM2010 crop whose
# irrigated:rainfed yield ratio sets the level of the regime yield ratio, and
# the LPJmL CFT whose band ratio supplies its relative anomaly (issue #1233).
#
# The SPAM side transcribes Table S3 of the Yu et al. (2020) ESSD supplement
# (doi:10.5194/essd-12-3545-2020, supplement pp. 9-10), the published SPAM2010
# crop-to-FAO-code list; the LPJmL side is cft_mapping's `cft_lpjml`. Items
# neither source classifies carry a stand-in with its rationale; the fodder
# composites and the Linum/Hemp dominance rule are rules of the method.

regime_yield_crop_mapping <- here::here(
  "inst",
  "extdata",
  "regime_yield_crop_mapping.csv"
) |>
  readr::read_csv(
    col_types = readr::cols(
      item_prod_code = readr::col_integer(),
      item_prod_name = readr::col_character(),
      spam_crop = readr::col_character(),
      spam_basis = readr::col_character(),
      lpjml_cft = readr::col_character(),
      lpjml_basis = readr::col_character(),
      rationale = readr::col_character()
    )
  )

# A duplicated item would make every join against this table fan out; fail
# the build rather than a user's run.
stopifnot(!anyDuplicated(regime_yield_crop_mapping$item_prod_code))

usethis::use_data(regime_yield_crop_mapping, overwrite = TRUE)
