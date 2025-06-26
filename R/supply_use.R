# TODO: Revisit the whole file. I'm pretty sure there are mistakes,
# along with incomplete parts. It's better to first finish the model
# and then come back here.

#' Supply and use tables
#'
#' @description
#' Create a table with processes, their inputs (_use_) and their
#' outputs (_supply_).
#'
#' @returns
#' A tibble with the supply and use data for processes.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `proc_code`: The code of the process taking place. For code details see
#'   e.g. `add_process_name()`.
#' - `item_cbs_code`: The code of the item taking part in the process. For code
#'    details see e.g. `add_item_cbs_name()`.
#' - `type`: Can have two values:
#'    - `use`: The given item is an input of the process.
#'    - `supply`: The given item is an output of the process.
#' - `value`: Quantity in tonnes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' build_supply_use()
#' }
build_supply_use <- function() {
  .build_supply_use_from_inputs(
    crop_prod_items = .read_local_csv("input/raw/crop_production_items.csv"),
    husbandry_items = .read_local_csv("input/raw/husbandry_items.csv"),
    coeffs = get_processing_coefs(get_file_path("processing_coefs")),
    cbs = get_wide_cbs(get_file_path("commodity_balance_sheet")),
    crop_residues = get_primary_residues(get_file_path("crop_residues")),
    primary_prod = get_primary_production(get_file_path("primary_prod")),
    feed_intake = get_feed_intake(get_file_path("feed_intake"))
  )
}

.build_supply_use_from_inputs <- function(
    crop_prod_items,
    husbandry_items,
    coeffs,
    cbs,
    crop_residues,
    primary_prod,
    feed_intake) {
  dplyr::bind_rows(
    .build_crop_production(crop_prod_items, cbs, primary_prod, crop_residues),
    .build_husbandry(husbandry_items, feed_intake, primary_prod),
    .build_processing(coeffs),
  ) |>
    dplyr::select(
      year,
      area_code,
      proc_group,
      proc_cbs_code,
      item_cbs_code,
      value,
      type
    )
}

.build_crop_production <- function(
    crop_prod_items,
    cbs,
    primary_prod,
    crop_residues) {
  supply_crop_production <- .build_supply_crop_production(
    crop_prod_items,
    primary_prod,
    crop_residues
  ) |>
    dplyr::mutate(type = "supply")

  cbs_items <- supply_crop_production |>
    dplyr::distinct(item_cbs_code)

  use_crop_production <- .build_use_crop_production(cbs_items, cbs) |>
    dplyr::mutate(type = "use")

  dplyr::bind_rows(supply_crop_production, use_crop_production) |>
    dplyr::mutate(proc_group = "crop_production")
}

.build_supply_crop_production <- function(
    crop_prod_items,
    primary_prod,
    crop_residues) {
  supply_crop_product <- .build_supply_crop_product(
    crop_prod_items,
    primary_prod
  )

  cbs_items <- supply_crop_product |>
    dplyr::distinct(item_cbs_code_crop = item_cbs_code)

  supply_crop_residue <- .build_supply_crop_residue(cbs_items, crop_residues)

  dplyr::bind_rows(supply_crop_product, supply_crop_residue)
}

.build_use_crop_production <- function(cbs_items, cbs) {
  cbs |>
    dplyr::inner_join(cbs_items, "item_cbs_code") |>
    dplyr::filter(seed > 0) |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = item_cbs_code,
      item_cbs_code = item_cbs_code,
      value = seed
    )
}

.build_supply_crop_product <- function(crop_prod_items, primary_prod) {
  primary_prod |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::inner_join(crop_prod_items, "item_prod_code") |>
    dplyr::summarise(
      value = sum(value),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::mutate(proc_cbs_code = item_cbs_code)
}

.build_supply_crop_residue <- function(cbs_items, crop_residues) {
  crop_residues |>
    dplyr::inner_join(cbs_items, "item_cbs_code_crop") |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = item_cbs_code_crop,
      item_cbs_code = item_cbs_code_residue,
      value
    )
}

.build_husbandry <- function(husbandry_items, feed_intake, primary_prod) {
  dplyr::bind_rows(
    .build_use_husbandry(feed_intake),
    .build_supply_husbandry(husbandry_items, primary_prod),
  ) |>
    dplyr::mutate(proc_group = "husbandry")
}

.build_use_husbandry <- function(feed_intake) {
  feed_intake |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = live_anim_code,
      item_cbs_code,
      value = supply
    ) |>
    dplyr::mutate(type = "use")
}

.build_supply_husbandry <- function(husbandry_items, primary_prod) {
  dplyr::bind_rows(
    .build_livestock_supply(primary_prod, husbandry_items),
    .build_livestock_prods_supply(primary_prod, husbandry_items),
  ) |>
    dplyr::mutate(type = "supply")
}

.build_livestock_supply <- function(primary_prod, husbandry_items) {
  k_tonnes_per_livestock_unit <- 0.65

  primary_prod |>
    dplyr::filter(unit == "LU") |>
    dplyr::inner_join(
      husbandry_items,
      dplyr::join_by(item_cbs_code == live_anim_code)
    ) |>
    dplyr::mutate(value = k_tonnes_per_livestock_unit * value) |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = item_cbs_code,
      item_cbs_code = item_cbs_code,
      value
    )
}

.build_livestock_prods_supply <- function(primary_prod, husbandry_items) {
  primary_prod |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::inner_join(husbandry_items, "live_anim_code") |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = live_anim_code,
      item_cbs_code,
      value
    )
}

.build_processing <- function(coeffs) {
  processing_use <- coeffs |>
    dplyr::distinct(
      year,
      area_code,
      proc_cbs_code = item_cbs_code_to_process,
      item_cbs_code = item_cbs_code_to_process,
      value = value_to_process
    ) |>
    dplyr::mutate(type = "use")

  processing_supply <- coeffs |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = item_cbs_code_to_process,
      item_cbs_code = item_cbs_code_processed,
      value = final_value_processed
    ) |>
    dplyr::mutate(type = "supply")

  dplyr::bind_rows(processing_use, processing_supply) |>
    dplyr::mutate(proc_group = "processing")
}
