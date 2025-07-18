whep_inputs <- here::here("inst", "extdata", "whep_inputs.csv") |>
  readr::read_csv()

usethis::use_data(whep_inputs, overwrite = TRUE)
