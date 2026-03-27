# TODO: Revisit the whole file. I'm pretty sure there are mistakes,
# along with incomplete parts. It's better to first finish the model
# and then come back here.

#' Supply and use tables
#'
#' @description
#' Create a table with processes, their inputs (_use_) and their
#' outputs (_supply_).
#'
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
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
#'    cattle husbandry, layers chickens farming, etc. Husbandry processes
#'    take feed as input and produce live animals and non-slaughter livestock
#'    products (milk, eggs, wool, honey, etc.) as output.
#'    - `slaughtering`: Slaughtering of livestock. Each slaughtering process
#'    takes a live animal as input and produces slaughter products (meat,
#'    offals, fats, hides and skins) as output.
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
#' - `value`: Quantity in tonnes for most items, or heads for live
#'    animals (see [items_cbs] `item_type`).
#'
#' @export
#'
#' @examples
#' build_supply_use(example = TRUE)
build_supply_use <- function(example = FALSE) {
  if (example) {
    return(.example_build_supply_use())
  }

  # Run build pipeline once; session-cached (see ?whep_clear_cache).
  primary_prod <- .cache_get("primary_prod", build_primary_production())

  cbs_built <- .cache_get("cbs_built", {
    cli::cli_h1("Building commodity balance sheets")
    build_commodity_balances(primary_prod)
  })

  coeffs <- .cache_get("proc_coefs", {
    cli::cli_h1("Building processing coefficients")
    build_processing_coefs(cbs_built)
  })

  cbs <- .cache_get("cbs_wide", {
    cli::cli_progress_step("Adding livestock CBS rows")
    wide <- cbs_built |>
      dplyr::mutate(
        stock_withdrawal = -stock_retrieval,
        stock_addition = stock_retrieval,
        .keep = "unused"
      )
    livestock_cbs <- get_livestock_cbs(primary_prod)
    dplyr::bind_rows(wide, livestock_cbs)
  })

  .cache_get("supply_use", {
    cli::cli_h1("Building supply-use tables")
    cli::cli_progress_step("Reading crop residues")
    crop_residues <- get_primary_residues()
    cli::cli_progress_step("Reading feed intake")
    feed_intake <- get_feed_intake()

    cli::cli_progress_step("Assembling supply-use tables")
    .build_supply_use_from_inputs(
      items_prod = whep::items_prod,
      items_cbs = whep::items_cbs,
      coeffs = coeffs,
      cbs = cbs,
      crop_residues = crop_residues,
      primary_prod = primary_prod,
      feed_intake = feed_intake
    )
  })
}

.build_supply_use_from_inputs <- function(
  items_prod,
  items_cbs,
  coeffs,
  cbs,
  crop_residues,
  primary_prod,
  feed_intake
) {
  husbandry_items <- items_cbs |>
    dplyr::filter(startsWith(item_type, "livestock")) |>
    dplyr::select(live_anim_code = item_cbs_code)

  slaughter_products <- items_cbs |>
    dplyr::filter(item_type == "slaughter_product") |>
    dplyr::select(item_cbs_code)

  crop_prod_items <- items_prod |>
    dplyr::filter(item_type == "crop_product") |>
    dplyr::select(item_prod_code)

  dplyr::bind_rows(
    .build_crop_production(crop_prod_items, cbs, primary_prod, crop_residues),
    .build_husbandry(
      husbandry_items,
      feed_intake,
      primary_prod,
      slaughter_products
    ),
    .build_slaughtering(husbandry_items, primary_prod, slaughter_products),
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
  crop_residues
) {
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
  crop_residues
) {
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

.build_husbandry <- function(
  husbandry_items,
  feed_intake,
  primary_prod,
  slaughter_products
) {
  dplyr::bind_rows(
    .build_use_husbandry(feed_intake),
    .build_supply_husbandry(husbandry_items, primary_prod, slaughter_products),
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

.build_supply_husbandry <- function(
  husbandry_items,
  primary_prod,
  slaughter_products
) {
  dplyr::bind_rows(
    .build_livestock_supply(primary_prod, husbandry_items),
    .build_livestock_prods_supply(
      primary_prod,
      husbandry_items,
      slaughter_products
    ),
  ) |>
    dplyr::mutate(type = "supply")
}

.build_slaughtering <- function(
  husbandry_items,
  primary_prod,
  slaughter_products
) {
  supply <- .build_supply_slaughtering(
    primary_prod,
    husbandry_items,
    slaughter_products
  )

  use <- .build_use_slaughtering(primary_prod, husbandry_items, supply)

  dplyr::bind_rows(supply, use) |>
    dplyr::mutate(proc_group = "slaughtering")
}

.build_supply_slaughtering <- function(
  primary_prod,
  husbandry_items,
  slaughter_products
) {
  primary_prod |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::inner_join(husbandry_items, "live_anim_code") |>
    dplyr::inner_join(slaughter_products, "item_cbs_code") |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = live_anim_code,
      item_cbs_code,
      value
    ) |>
    dplyr::mutate(type = "supply")
}

.build_use_slaughtering <- function(primary_prod, husbandry_items, supply) {
  slaughter_animals <- supply |>
    dplyr::distinct(year, area_code, proc_cbs_code)

  primary_prod |>
    dplyr::filter(unit == "heads") |>
    dplyr::inner_join(
      husbandry_items,
      dplyr::join_by(item_cbs_code == live_anim_code)
    ) |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = item_cbs_code,
      item_cbs_code = item_cbs_code,
      value
    ) |>
    dplyr::semi_join(
      slaughter_animals,
      by = c("year", "area_code", "proc_cbs_code")
    ) |>
    dplyr::mutate(type = "use")
}

.build_livestock_supply <- function(primary_prod, husbandry_items) {
  primary_prod |>
    dplyr::filter(unit == "heads") |>
    dplyr::inner_join(
      husbandry_items,
      dplyr::join_by(item_cbs_code == live_anim_code)
    ) |>
    dplyr::select(
      year,
      area_code,
      proc_cbs_code = item_cbs_code,
      item_cbs_code = item_cbs_code,
      value
    )
}

.build_livestock_prods_supply <- function(
  primary_prod,
  husbandry_items,
  slaughter_products
) {
  primary_prod |>
    dplyr::filter(unit == "tonnes") |>
    dplyr::inner_join(husbandry_items, "live_anim_code") |>
    dplyr::anti_join(slaughter_products, "item_cbs_code") |>
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
