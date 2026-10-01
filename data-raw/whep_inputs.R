# Every column is text. `public_alternative` is blank on every public row, and
# readr would guess an all-blank column as logical, so the type is fixed here
# rather than left to the guess (#1386).
whep_inputs <- here::here("inst", "extdata", "whep_inputs.csv") |>
  readr::read_csv(col_types = readr::cols(.default = "c"))

usethis::use_data(whep_inputs, overwrite = TRUE)
