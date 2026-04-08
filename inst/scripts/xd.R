local_vs_import |>
  tidyr::pivot_longer(
    cols = c(food_share, feed_share, other_uses_share),
    names_to = "share_type",
    values_to = "share"
  ) |>
  dplyr::mutate(
    MgN = local_consumption * share,
    Destiny = dplyr::case_when(
      share_type == "food_share" ~ "population_food",
      share_type == "feed_share" ~ "livestock",
      share_type == "other_uses_share" ~ "population_other_uses"
    ),
    Origin = Box
  ) |>
  # dplyr::group_by(
  #   Year,
  #   Province_name,
  #   Item,
  #   Irrig_cat,
  #   Box,
  #   Origin,
  #   Destiny
  # ) |>
  # dplyr::summarise(MgN = sum(MgN, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(Year, Province_name, Item, Box, Irrig_cat, Origin, Destiny) |>
  dplyr::group_by(Year, Province_name, Item, Box, Irrig_cat, Origin, Destiny) |>
  dplyr::filter(dplyr::n() > 1)
