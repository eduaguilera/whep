k_faostat_regions |>
  tidyr::replace_na(list(start_year = 1961, end_year = 2025)) |>
  dplyr::group_by(country_name) |>
  tidyr::expand(year = seq(start_year, end_year)) |>
  dplyr::ungroup() |>
  dplyr::select(original_name = country_name, year) |>
  get_polity_code(open_mismatches = TRUE)
