#' Primary items production
#'
#' @description
#' Get amount of crops, livestock and livestock products.
#'
#' @returns
#' A tibble with the item production data.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `item_prod_code`: FAOSTAT internal code for each produced item.
#' - `item_cbs_code`: FAOSTAT internal code for each commodity balance sheet
#'    item. The commodity balance sheet contains an aggregated version of
#'    production items. This field is the code for the corresponding
#'    aggregated item.
#' - `live_anim_code`: Commodity balance sheet code for the type of livestock
#'    that produces the livestock product. It can be:
#'    - `NA`: The entry is not a livestock product.
#'    - Non-`NA`: The code for the livestock type. The name can also be
#'    retrieved by using `add_item_cbs_name()`.
#' - `unit`: Measurement unit for the data. Here, keep in mind three groups of
#'    items: crops (e.g. `Apples and products`, `Beans`...), livestock (e.g.
#'    `Cattle, dairy`, `Goats`...) and livestock products (e.g. `Poultry
#'    Meat`, `Offals, Edible`...). Then the unit can be one of:
#'    - `tonnes`: Available for crops and livestock products.
#'    - `ha`: Hectares, available for crops.
#'    - `t_ha`: Tonnes per hectare, available for crops.
#'    - `heads`: Number of animals, available for livestock.
#'    - `LU`: Standard Livestock Unit measure, available for livestock.
#'    - `t_head`: tonnes per head, available for livestock products.
#'    - `t_LU`: tonnes per Livestock Unit, available for livestock products.
#' - `value`: The amount of item produced, measured in `unit`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_primary_production()
#' }
get_primary_production <- function() {
  "primary_prod" |>
    whep_read_file() |>
    dplyr::rename_with(tolower) |>
    dplyr::select(
      year,
      area_code,
      item_prod_code = item_code,
      item_cbs_code = item_code_cbs,
      live_anim_code,
      unit,
      value
    )
}

#' Crop residue items
#'
#' @description
#' Get type and amount of residue produced for each crop production item.
#'
#' @returns
#' A tibble with the crop residue data.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `item_cbs_code_crop`: FAOSTAT internal code for each commodity balance
#'    sheet item. This is the crop that is generating the residue.
#' - `item_cbs_code_residue`: FAOSTAT internal code for each commodity balance
#'    sheet item. This is the obtained residue. In the commodity balance sheet,
#'    this can be three different items right now:
#'    - `2105`: `Straw`
#'    - `2106`: `Other crop residues`
#'    - `2107`: `Firewood`
#'
#'    These are actually not FAOSTAT defined items, but custom defined by us.
#'    When necessary, FAOSTAT codes are extended for our needs.
#' - `value`: The amount of residue produced, measured in tonnes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_primary_residues()
#' }
get_primary_residues <- function() {
  "crop_residues" |>
    whep_read_file() |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(product_residue == "Residue") |>
    add_area_code(name_column = "area") |>
    add_item_cbs_code(
      name_column = "item_cbs_crop",
      code_column = "item_cbs_code_crop"
    ) |>
    add_item_cbs_code(
      name_column = "item_cbs",
      code_column = "item_cbs_code_residue"
    ) |>
    dplyr::summarise(
      value = sum(prod_ygpit_mg),
      .by = c(year, area_code, item_cbs_code_crop, item_cbs_code_residue)
    ) |>
    dplyr::filter(value > 0) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code_crop,
      item_cbs_code_residue,
      value
    ) |>
    .use_seed_cbs_item()
}

# TODO: This is dirty, revisit when we build the data here directly.
# Change CBS names to the item that is used as seed
.use_seed_cbs_item <- function(crop_residues) {
  crop_residues |>
    dplyr::mutate(
      item_cbs_code_crop = dplyr::case_match(
        item_cbs_code_crop,
        # "Seed cotton" changes to "Cottonseed",
        328 ~ 2559,
        # "Coconuts" changes to "Coconuts - Incl Copra",
        248 ~ 2560,
        # "Oil, palm fruit" changes to "Palm kernels"
        254 ~ 2562,
        # Leave others unchanged
        .default = item_cbs_code_crop
      )
    )
}
