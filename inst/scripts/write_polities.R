get_polities() |>
  readr::write_csv("polities.csv")

read_ft() |>
  readr::write_csv("ft_cleaned.csv")
