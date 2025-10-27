#' Livestock feed intake
#'
#' @description
#' Get amount of items used for feeding livestock.
#'
#' @param version File version to use as input. See [whep_inputs] for details.
#'
#' @returns
#' A tibble with the feed intake data.
#' It contains the following columns:
#' - `year`: The year in which the recorded event occurred.
#' - `area_code`: The code of the country where the data is from. For code
#'    details see e.g. `add_area_name()`.
#' - `live_anim_code`: Commodity balance sheet code for the type of livestock
#'    that is fed. For code details see e.g. `add_item_cbs_name()`.
#' - `item_cbs_code`: The code of the item that is used for feeding the animal.
#'    For code details see e.g. `add_item_cbs_name()`.
#' - `feed_type`: The type of item that is being fed. It can be one of:
#'    - `animals`: Livestock product, e.g. `Bovine Meat`, `Butter, Ghee`, etc.
#'    - `crops`: Crop product, e.g. `Vegetables, Other`, `Oats`, etc.
#'    - `residues`: Crop residue, e.g. `Straw`, `Fodder legumes`, etc.
#'    - `grass`: Grass, e.g. `Grassland`, `Temporary grassland`, etc.
#'    - `scavenging`: Other residues. Single `Scavenging` item.
#' - `supply`: The computed amount in tonnes of this item that should be fed to
#'    this animal, when sharing the total item `feed` use from the Commodity
#'    Balance Sheet among all livestock.
#' - `intake`: The actual amount in tonnes that the animal needs, which can be
#'    less than the theoretical used amount from `supply`.
#' - `intake_dry_matter`: The amount specified by `intake` but only considering
#'    dry matter, so it should be less than `intake`.
#' - `loss`: The amount that is not used for feed. This is `supply - intake`.
#' - `loss_share`: The percent that is lost. This is `loss / supply`.
#'
#' @export
#'
#' @examples
#' # Note: These are smaller samples to show outputs, not the real data.
#' # For all data, call the function with default version (i.e. no arguments).
#' get_feed_intake(version = "example")
get_feed_intake <- function(version = NULL) {
  "feed_intake" |>
    whep_read_file(version = version) |>
    dplyr::rename_with(tolower) |>
    add_area_code(name_column = "area") |>
    add_item_cbs_code(
      name_column = "live_anim",
      code_column = "live_anim_code"
    ) |>
    add_item_cbs_code(name_column = "item", code_column = "item_cbs_code") |>
    dplyr::select(
      year,
      area_code,
      live_anim_code,
      item_cbs_code,
      feed_type = feedtype,
      supply,
      intake,
      intake_dry_matter = intake_dm,
      loss,
      loss_share
    )
}
