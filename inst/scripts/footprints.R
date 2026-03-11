# Years to process
years <- c(1986, 1987)

# Build IO model for selected years.
io <- build_io_model(years = years)
land_use <- get_land_fp_production()

# Compute footprints for all selected years.
footprints <- purrr::pmap_dfr(
  list(
    io$year,
    io$Z,
    io$X,
    io$Y,
    io$labels,
    io$fd_labels
  ),
  function(yr, z_mat, x_vec, y_mat, labels, fd_labels) {
    extensions <- land_use |>
      dplyr::filter(year == yr) |>
      dplyr::right_join(labels, by = c("area_code", "item_cbs_code")) |>
      tidyr::replace_na(list(impact_u = 0)) |>
      dplyr::arrange(index) |>
      dplyr::pull(impact_u)

    compute_footprint(
      z_mat = z_mat,
      x_vec = x_vec,
      y_mat = y_mat,
      extensions = extensions,
      labels = labels,
      fd_labels = fd_labels
    ) |>
      dplyr::mutate(year = yr)
  }
)

footprints |>
  add_area_name(
    name_column = "origin_area_name",
    code_column = "origin_area"
  ) |>
  add_area_name(
    name_column = "target_area_name",
    code_column = "target_area"
  ) |>
  add_item_cbs_name(
    name_column = "origin_item_name",
    code_column = "origin_item"
  ) |>
  add_item_cbs_name(
    name_column = "target_item_name",
    code_column = "target_item"
  ) |>
  dplyr::filter(origin_item != target_item) |>
  print(n = 100)
