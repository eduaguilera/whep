# TODO: Revisit the whole file. I'm pretty sure there are mistakes,
# along with incomplete parts. It's better to first finish the model
# and then come back here.

#' Supply and use tables
#'
#' @description
#' Create a table with processes, their inputs (_use_) and their
#' outputs (_supply_).
#'
#' @param cbs_version File version passed to `get_wide_cbs()` call.
#' @param feed_intake_version File version passed to `get_feed_intake()` call.
#' @param primary_prod_version File version passed to
#'   `get_primary_production()` call.
#' @param primary_residues_version File version passed to
#'   `get_primary_residues()` call.
#' @param processing_coefs_version File version passed to
#'   `get_processing_coefs()` call.
#'
#' @returns
#' A tibble with the supply and use data for processes.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `proc_group`: The type of process taking place. It can be one of:
#'    - `crop_production`: Production of crops and their residues, e.g. rice
#'    production, coconut production, etc.
#'    - `husbandry`: Animal husbandry, e.g. dairy cattle husbandry, non-dairy
#'    cattle husbandry, layers chickens farming, etc.
#'    - `processing`: Derived subproducts obtained from processing other items.
#'    The items used as inputs are those that have a non-zero processing use in
#'    the commodity balance sheet. See `get_wide_cbs()` for more details.
#'    In each process there is a single input. In some processes like olive oil
#'    extraction or soyabean oil extraction this might make sense. Others like
#'    alcohol production need multiple inputs (e.g. multiple crops work), so
#'    in this data there would not be a process like alcohol production but
#'    rather a _virtual_ process like 'Wheat and products processing', giving
#'    all its possible outputs. This is a constraint because of how the data was
#'    obtained and might be improved in the future. See
#'    `get_processing_coefs()` for more details.
#' - `proc_cbs_code`: The code of the main item in the process taking place.
#'    Together with `proc_group`, these two columns uniquely represent a
#'    process. The main item is predictable depending on the value of
#'    `proc_group`:
#'    - `crop_production`: The code is from the item for which seed usage
#'    (if any) is reported in the commodity balance sheet (see
#'    `get_wide_cbs()` for more). For example, the rice code for a rice
#'    production process or the cottonseed code for the cotton production one.
#'    - `husbandry`: The code of the farmed animal, e.g. bees for beekeeping,
#'    non-dairy cattle for non-dairy cattle husbandry, etc.
#'    - `processing`: The code of the item that is used as input, i.e., the one
#'    that is processed to get other derived products. This uniquely defines a
#'    process within the group because of the nature of the data that was used,
#'    which you can see in `get_processing_coefs()`.
#'
#'    For code details see e.g. `add_item_cbs_name()`.
#' - `item_cbs_code`: The code of the item produced or used in the process.
#'    Note that this might be the same value as `proc_cbs_code`, e.g., in rice
#'    production process for the row defining the amount of rice produced or
#'    the amount of rice seed as input, but it might also have a different
#'    value, e.g. for the row defining the amount of straw residue from rice
#'    production. For code details see e.g. `add_item_cbs_name()`.
#' - `type`: Can have two values:
#'    - `use`: The given item is an input of the process.
#'    - `supply`: The given item is an output of the process.
#' - `value`: Quantity in tonnes.
#'
#' @export
#'
#' @examples
#' # Note: These are smaller samples to show outputs, not the real data.
#' # For all data, call the function with default versions (i.e. no arguments).
#' build_supply_use(
#'   cbs_version = "20250721T132006Z-8ea47",
#'   feed_intake_version = "20250721T143825Z-c1313",
#'   primary_prod_version = "20250721T145805Z-8e12a",
#'   primary_residues_version = "20250721T150132Z-dfd94",
#'   processing_coefs_version = "20250721T143403Z-216d7"
#' )
build_supply_use <- function(
    cbs_version = NULL,
    feed_intake_version = NULL,
    primary_prod_version = NULL,
    primary_residues_version = NULL,
    processing_coefs_version = NULL) {
  .build_supply_use_from_inputs(
    items_prod = whep::items_prod,
    items_cbs = whep::items_cbs,
    coeffs = get_processing_coefs(version = processing_coefs_version),
    cbs = get_wide_cbs(version = cbs_version),
    crop_residues = get_primary_residues(version = primary_residues_version),
    primary_prod = get_primary_production(version = primary_prod_version),
    feed_intake = get_feed_intake(version = feed_intake_version)
  )
}

.build_supply_use_from_inputs <- function(
    items_prod,
    items_cbs,
    coeffs,
    cbs,
    crop_residues,
    primary_prod,
    feed_intake) {
  husbandry_items <- items_cbs |>
    dplyr::filter(item_type == "livestock") |>
    dplyr::select(live_anim_code = item_cbs_code)

  crop_prod_items <- items_prod |>
    dplyr::filter(item_type == "crop_product") |>
    dplyr::select(item_prod_code)

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
      type,
      value
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
    # TODO: This is essentially double counting animals' mass both as livestock
    # and their products. Go back to this when integrating input-output matrix
    # with trade, to look for a satisfying fix.
    .build_livestock_supply(primary_prod, husbandry_items),
    .build_livestock_prods_supply(primary_prod, husbandry_items),
  ) |>
    dplyr::mutate(type = "supply")
}

.build_livestock_supply <- function(primary_prod, husbandry_items) {
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
