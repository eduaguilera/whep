#' Livestock feed intake
#'
#' @description
#' Get amount of items used for feeding livestock.
#'
#' @param example If `TRUE`, return a small example output without downloading
#'   remote data. Default is `FALSE`.
#' @param grain Spatial grain of the feed allocation. `"national"` (default, one
#'   allocation per country) or `"local"` (the per-cell 0.5-degree engine,
#'   which is heavy and run via [build_feed_intake_local()]; calling it here
#'   redirects there).
#' @param demand_tier Demand-estimation tier. `"ipcc"` (default, the rigorous
#'   IPCC Tier-2 energy demand for the ruminant species it covers, Bouwman FCR
#'   for pigs and poultry, Krausmann per-head for draft / other species) or
#'   `"fcr"` (the Bouwman / Krausmann feed-conversion magnitude for every
#'   species). Both grains allocate with `redistribute_feed()`.
#' @param feed_mode Whether to distribute surplus feed availability.
#'   `"historical"` (default) suppresses the surplus-distribution pass: the CBS
#'   feed element is treated as realised consumption, so leftover availability is
#'   not dumped onto variable-demand livestock (which would inflate non-grass
#'   intake). `"scenario"` distributes the surplus.
#' @param years Integer vector of years to build, or `NULL` (default) for every
#'   year in the production data (1850-2023 via the LUH2 extension). Restricting
#'   the range cuts run time proportionally; allocation is independent per year,
#'   so a subset returns exactly the same rows for those years.
#' @param feed_eligibility Which feeds each livestock category may receive.
#'   `"feed_table"` (default) follows `feed_taxonomy`: an item with a grazer
#'   feed type but no granivore feed type (straw and green fodder) is fibrous
#'   roughage that pigs and poultry do not eat, so it is never allocated to
#'   them, at any allocation level. Granivore demand that the feeds they may
#'   eat cannot cover stays unmet rather than being filled with roughage.
#'   `"none"` lets any category receive any item (the behaviour before
#'   whep#1218), for sensitivity analysis.
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
  grain = c("national", "local"),
  demand_tier = c("ipcc", "fcr"),
  feed_mode = c("historical", "scenario"),
  years = NULL,
  feed_eligibility = c("feed_table", "none")
) {
  grain <- rlang::arg_match(grain)
  demand_tier <- rlang::arg_match(demand_tier)
  feed_mode <- rlang::arg_match(feed_mode)
  feed_eligibility <- rlang::arg_match(feed_eligibility)
  if (example) {
    return(.example_get_feed_intake())
  }
  .build_redistribute_intake(
    grain = grain,
    demand_tier = demand_tier,
    feed_mode = feed_mode,
    years = years,
    feed_eligibility = feed_eligibility
  ) |>
    .add_reporting_polity_columns()
}
