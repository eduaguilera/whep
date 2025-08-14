# Only for running when integrating with CShapes data for first time

polities_inputs_path <- here::here("data-raw", "polities_inputs")

cshapes_renames <- here::here() |>
  fs::path(polities_inputs_path, "rename_cshapes.csv") |>
  readr::read_csv()

cshapes <- k_cshapes |>
  tibble::as_tibble() |>
  dplyr::arrange(polity_name, end_year) |>
  dplyr::inner_join(
    cshapes_renames,
    by = dplyr::join_by(polity_name == country_name),
    unmatched = "error"
  ) |>
  dplyr::mutate(
    original_name = .add_years_in_name(polity_name, start_year, end_year),
    source = "cshapes",
    common_name = .add_years_in_name(common_name, start_year, end_year)
  )

cshapes_common_names <- cshapes |>
  dplyr::select(original_name, source, common_name)

k_polity_common_names |>
  dplyr::bind_rows(cshapes_common_names) |>
  readr::write_csv(
    fs::path(polities_inputs_path, "common_names.csv")
  )

cshapes_polity_codes <- cshapes |>
  dplyr::anti_join(
    k_polity_codes,
    by = dplyr::join_by(common_name == polity_name)
  ) |>
  dplyr::mutate(
    polity_code = .build_auto_polity_code(polity_name, start_year, end_year)
  ) |>
  dplyr::distinct(polity_name = common_name, polity_code)

k_polity_codes |>
  dplyr::bind_rows(cshapes_polity_codes) |>
  dplyr::arrange(polity_name, polity_code) |>
  readr::write_csv(
    fs::path(polities_inputs_path, "polity_codes.csv")
  )
