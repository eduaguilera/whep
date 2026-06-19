#' Livestock feed intake
#'
#' @description
#' Get amount of items used for feeding livestock.
#'
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
#' @param grain Spatial grain of the feed allocation. `"national"` (default, the
#'   current supply-driven allocator) or `"provincial"` (the `redistribute_feed`
#'   engine at 0.5-degree cell grain). The provincial engine is a migration in
#'   progress and not yet implemented.
#' @param demand_tier Demand-estimation tier. `"fcr"` (default, Bouwman /
#'   Krausmann feed-conversion) or `"ipcc"` (IPCC Tier-2 energy where it covers
#'   the species, Krausmann elsewhere). The `"ipcc"` tier is a migration in
#'   progress and not yet implemented. The defaults reproduce the current output
#'   for back-compatibility; they will switch to the more rigorous tier once the
#'   new path is built and regression-validated.
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
#' get_feed_intake(example = TRUE)
get_feed_intake <- function(
  example = FALSE,
  grain = c("national", "provincial"),
  demand_tier = c("fcr", "ipcc")
) {
  grain <- rlang::arg_match(grain)
  demand_tier <- rlang::arg_match(demand_tier)
  if (example) {
    return(.example_get_feed_intake())
  }
  if (grain == "national" && demand_tier == "fcr") {
    return(.build_feed_intake_from_inputs(
      cbs = get_wide_cbs(),
      primary_prod = get_primary_production()
    ))
  }
  .build_redistribute_intake(grain = grain, demand_tier = demand_tier)
}
