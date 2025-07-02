items_prod <- .read_local_csv("input/raw/items_prod.csv")
crop_production <- .read_local_csv("input/raw/crop_production_items.csv")

items_prod |>
  dplyr::inner_join(crop_production) |>
  dplyr::mutate(item_type = "crop_product") |>
  dplyr::bind_rows(
    items_prod |>
      dplyr::anti_join(crop_production) |>
      dplyr::mutate(item_type = "other")
  ) |>
  dplyr::arrange(item_prod_name) |>
  readr::write_csv("inst/extdata/input/raw/items_prod.csv")

items_cbs <- .read_local_csv("input/raw/items_cbs.csv")
husbandry_items <- .read_local_csv("input/raw/husbandry_items.csv")

items_cbs |>
  dplyr::inner_join(husbandry_items, dplyr::join_by(item_cbs_code == live_anim_code)) |>
  dplyr::mutate(item_type = "livestock") |>
  dplyr::bind_rows(
    items_cbs |>
      dplyr::anti_join(husbandry_items, dplyr::join_by(item_cbs_code == live_anim_code)) |>
      dplyr::mutate(item_type = "other")
  ) |>
  dplyr::arrange(item_cbs_name) |>
  readr::write_csv("inst/extdata/input/raw/items_cbs.csv")
