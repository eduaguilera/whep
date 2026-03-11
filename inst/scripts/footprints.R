# Supply-use tables (who produces and uses what)
supply_use <- build_supply_use()

# Bilateral trade matrices (who trades with whom)
bilateral_trade <- get_bilateral_trade()

# Commodity balance sheets (production, consumption, trade)
cbs <- get_wide_cbs()

# Build IO model for all years
io <- build_io_model(supply_use, bilateral_trade, cbs, years = c(1986))

land_fp <- readr::read_csv("~/Downloads/Land_FP.csv")

# Extract matrices for one year
z_mat <- io$Z[[1]]
y_mat <- io$Y[[1]]
x_vec <- io$X[[1]]
labels <- io$labels[[1]]
fd_labels <- io$fd_labels[[1]]

land_fp |>
  dplyr::filter(Element == "Production") |>
  dplyr::filter(item == "Beer")
dplyr::distinct(item) |>
  print(n = 100)

land_use <- land_fp |>
  dplyr::filter(Impact == "Land", Origin == "Production") |>
  add_area_code(name_column = "area", code_column = "code") |>
  dplyr::rename_with(tolower) |>
  dplyr::select(
    year,
    area_code = code,
    item_cbs_code = item_code,
    impact,
    element,
    origin,
    group,
    value,
    impact_u,
    u_ton,
    exp_share,
    export_u,
    consumption_u
  )

extensions <- land_use |>
  dplyr::filter(year == 1986) |>
  dplyr::right_join(labels, by = c("area_code", "item_cbs_code")) |>
  tidyr::replace_na(list(impact_u = 0)) |>
  dplyr::arrange(index) |>
  dplyr::pull(impact_u)

footprint <- compute_footprint(
  z_mat = z_mat,
  x_vec = x_vec,
  y_mat = y_mat,
  extensions = extensions,
  labels = labels,
  fd_labels = fd_labels
)
footprint

footprint |>
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
