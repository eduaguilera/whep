#' Allocate grazing land forward to livestock products.
#'
#' @description
#' Push grazing land forward along the feed chain instead of tracing it
#' backward through a Leontief inverse. Grass is not traded, so the
#' multi-regional input-output footprint leaks the grazing land that
#' meat and dairy consumption actually drives (the feed-conversion
#' columns make the system non-productive). This function hands every
#' tonne of grazing land forward in two mass-conserving steps:
#'
#' 1. pool each country's grazing land and split it across the grazing
#'    animals that fed there, in proportion to their forage intake;
#' 2. split each animal's share across its output products in
#'    proportion to production mass, landing the land on the meat and
#'    milk items that are actually traded.
#'
#' The result is a direct-land extension keyed by livestock output
#' item, ready to route to consumers with
#' [compute_footprint_balance()]. Because each step redistributes a
#' total without creating or destroying it, the country-level land
#' total is conserved whenever an animal fed and produced an output
#' there.
#'
#' @param grass_land Tibble with `year`, `area_code` and `value`
#'   (grazing land, e.g. hectares). Multiple rows per country (for
#'   example one per grass item) are pooled.
#' @param grazer_intake Tibble with `year`, `area_code`,
#'   `live_anim_code` and `value` (the intake by which to split the
#'   land across animals), as from [get_feed_intake()]. Using grazer
#'   forage intake (grass plus roughage residues) rather than the
#'   `"grass"` feed type alone keeps extensive-grazing countries, whose
#'   forage the feed model classes as residues, from dropping out.
#' @param livestock_production Tibble with `year`, `area_code`,
#'   `live_anim_code`, `item_cbs_code` and `value` (output production,
#'   tonnes).
#' @param products Which output items absorb the land. `"all"`
#'   (default) spreads it across every livestock output by mass;
#'   `"meat_milk"` restricts it to meat and dairy items, so all grazing
#'   land lands on meat and dairy consumers per the forward-allocation
#'   goal.
#'
#' @return A tibble with `year`, `area_code`, `item_cbs_code` (a
#'   livestock output item), `value` (grazing land allocated to it) and
#'   `method_allocation` (the chosen `products`).
#'
#' @export
#'
#' @examples
#' grass_land <- tibble::tibble(
#'   year = 2010L, area_code = 1L, value = 100
#' )
#' grazer_intake <- tibble::tibble(
#'   year = 2010L,
#'   area_code = 1L,
#'   live_anim_code = c(960L, 976L),
#'   value = c(75, 25)
#' )
#' livestock_production <- tibble::tibble(
#'   year = 2010L,
#'   area_code = 1L,
#'   live_anim_code = c(960L, 960L, 976L),
#'   item_cbs_code = c(2848L, 2731L, 2732L),
#'   value = c(90, 10, 5)
#' )
#' allocate_grazing_to_products(
#'   grass_land, grazer_intake, livestock_production
#' )
allocate_grazing_to_products <- function(
  grass_land,
  grazer_intake,
  livestock_production,
  products = c("all", "meat_milk")
) {
  products <- rlang::arg_match(products)
  .require_cols(grass_land, c("year", "area_code", "value"), "grass_land")
  .require_cols(
    grazer_intake,
    c("year", "area_code", "live_anim_code", "value"),
    "grazer_intake"
  )
  .require_cols(
    livestock_production,
    c("year", "area_code", "live_anim_code", "item_cbs_code", "value"),
    "livestock_production"
  )

  land_by_animal <- .grazing_land_to_animal(grass_land, grazer_intake)
  weights <- .grazing_output_weights(livestock_production, products)
  .grazing_land_to_products(land_by_animal, weights) |>
    dplyr::mutate(method_allocation = products)
}

#' Build a grazing-land footprint by forward feed-allocation.
#'
#' @description
#' End-to-end grazing-land footprint for one year on real WHEP data.
#' Grazing land (the grassland extension) is pushed forward onto
#' livestock meat and milk via [allocate_grazing_to_products()], then
#' routed to consuming countries through the bilateral meat-trade
#' network with [compute_footprint_balance()]. This attributes grazing
#' land to the meat and dairy consumption that drives it -- the chain
#' the Leontief footprint leaks because grass is non-productive and not
#' traded.
#'
#' The inputs are assembled from [build_grassland_land_extension()],
#' [get_feed_intake()], [get_primary_production()] and
#' [get_bilateral_trade()]. Supply any of them through `data` to reuse
#' cached inputs or to test without remote reads.
#'
#' `intake_basis` selects the intake that splits grazing land across
#' animals. The default `"grazer_forage"` uses every grazing animal's
#' grass plus roughage-residue intake, because the feed model classes
#' the forage of major extensive-grazing countries (for example
#' Australia, the United States and Argentina) as residues rather than
#' grass; keying on the `"grass"` feed type alone would silently strip
#' that land. `"grass"` reproduces the narrower grass-feed basis for
#' sensitivity analysis. Land in countries with no grazer intake at all
#' cannot be attributed and is reported via a warning.
#'
#' @param year Year to build the footprint for.
#' @param products Co-product split passed to
#'   [allocate_grazing_to_products()]. `"all"` (default) or
#'   `"meat_milk"`.
#' @param intake_basis Intake used to split land across animals,
#'   `"grazer_forage"` (default) or `"grass"`.
#' @param data Optional named list of pre-built inputs, any of
#'   `grass_land`, `grazer_intake`, `livestock_production` and `trade`.
#'   Each falls back to its builder when absent.
#' @param example If `TRUE`, return a small example output without
#'   downloading remote data. Default is `FALSE`.
#'
#' @return A tibble with `area_code` (consuming country),
#'   `item_cbs_code` (meat or milk item), `value` (embodied grazing
#'   land) and `method` (`"grazing_feed_allocation"`).
#'
#' @export
#'
#' @examples
#' build_grazing_feed_footprint(example = TRUE)
build_grazing_feed_footprint <- function(
  year,
  products = c("all", "meat_milk"),
  intake_basis = c("grazer_forage", "grass"),
  data = list(),
  example = FALSE
) {
  products <- rlang::arg_match(products)
  intake_basis <- rlang::arg_match(intake_basis)
  if (isTRUE(example)) {
    return(.ex_grazing_feed_footprint())
  }
  .validate_year(year)

  grass_land <- data$grass_land %||% .grazing_grass_land(year)
  grazer_intake <- data$grazer_intake %||%
    .grazing_forage_intake(year, intake_basis)
  production <- data$livestock_production %||% .grazing_production(year)
  .warn_unattributed_land(grass_land, grazer_intake)

  extension <- allocate_grazing_to_products(
    grass_land,
    grazer_intake,
    production,
    products
  ) |>
    dplyr::select(area_code, item_cbs_code, value)

  out_items <- sort(unique(extension$item_cbs_code))
  prod_wide <- production |>
    dplyr::filter(item_cbs_code %in% out_items) |>
    dplyr::summarise(value = sum(value), .by = c(area_code, item_cbs_code))
  trade <- data$trade %||% .grazing_meat_trade(year, out_items)

  compute_footprint_balance(prod_wide, trade, extension) |>
    dplyr::mutate(method = "grazing_feed_allocation")
}

# --- Allocation helpers ---

# Pool grazing land per country and split it across grazing animals by
# their share of total forage intake. Conserves the per-country land
# total whenever any animal fed there.
.grazing_land_to_animal <- function(grass_land, grazer_intake) {
  land <- grass_land |>
    dplyr::summarise(land = sum(value, na.rm = TRUE), .by = c(year, area_code))
  grazer_intake |>
    dplyr::summarise(
      intake = sum(value, na.rm = TRUE),
      .by = c(year, area_code, live_anim_code)
    ) |>
    dplyr::mutate(
      intake_share = .safe_share(intake),
      .by = c(year, area_code)
    ) |>
    dplyr::inner_join(land, by = c("year", "area_code")) |>
    dplyr::transmute(
      year,
      area_code,
      live_anim_code,
      land_animal = land * intake_share
    ) |>
    dplyr::filter(land_animal > 0)
}

# Production-mass weight of each output item within its animal, optionally
# restricted to meat and dairy so all grazing land lands on those items.
.grazing_output_weights <- function(livestock_production, products) {
  if (products == "meat_milk") {
    livestock_production <- dplyr::filter(
      livestock_production,
      item_cbs_code %in% .meat_milk_item_codes()
    )
  }
  livestock_production |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(year, area_code, live_anim_code, item_cbs_code)
    ) |>
    dplyr::filter(value > 0) |>
    dplyr::mutate(
      output_share = .safe_share(value),
      .by = c(year, area_code, live_anim_code)
    ) |>
    dplyr::select(year, area_code, live_anim_code, item_cbs_code, output_share)
}

# Combine the per-animal land with the per-animal output weights.
.grazing_land_to_products <- function(land_by_animal, weights) {
  land_by_animal |>
    dplyr::inner_join(
      weights,
      by = c("year", "area_code", "live_anim_code")
    ) |>
    dplyr::summarise(
      value = sum(land_animal * output_share),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::filter(value > 0)
}

# CBS items counted as meat or dairy output (carcass meats, milk, butter).
.meat_milk_item_codes <- function() {
  c(2731L, 2732L, 2733L, 2734L, 2735L, 2740L, 2848L)
}

# Feed types that represent grazed forage and roughage, the intake that
# grassland productivity supplies (as opposed to concentrates).
.forage_feed_types <- function() {
  c("grass", "residues")
}

# Live-animal codes of the grazing (ruminant and equid) classes; only
# these graze grassland, so granivores never receive grazing land.
.grazer_animal_codes <- function() {
  whep::animals_codes |>
    dplyr::filter(.data$Graniv_grazers == "Grazers") |>
    dplyr::pull(.data$item_cbs_code) |>
    unique() |>
    as.integer()
}

# Livestock output items (group "Livestock products" with a producing
# animal), the universe of items grazing land can be allocated to.
.livestock_output_items <- function() {
  whep::items_prod_full |>
    dplyr::filter(
      .data$group == "Livestock products",
      !is.na(.data$live_anim_code)
    ) |>
    dplyr::pull(.data$item_cbs_code) |>
    unique() |>
    as.integer()
}

# --- Real-data input builders ---

# Grazing land per country for one year (grass items pooled), from the
# native grassland land extension.
.grazing_grass_land <- function(year) {
  build_grassland_land_extension(grassland_metric = "occupation") |>
    dplyr::filter(year == .env$year) |>
    dplyr::transmute(
      year = as.integer(year),
      area_code = as.integer(area_code),
      value = .data$impact_u
    )
}

# Forage intake per country and grazing animal for one year. The
# `"grazer_forage"` basis sums grass and roughage-residue intake of the
# grazing classes; `"grass"` keeps the narrower grass feed type.
.grazing_forage_intake <- function(year, basis = "grazer_forage") {
  feed_types <- if (basis == "grass") "grass" else .forage_feed_types()
  intake <- get_feed_intake(years = as.integer(year)) |>
    dplyr::filter(.data$feed_type %in% feed_types)
  if (basis == "grazer_forage") {
    intake <- dplyr::filter(
      intake,
      .data$live_anim_code %in% .grazer_animal_codes()
    )
  }
  intake |>
    dplyr::summarise(
      value = sum(intake_dry_matter, na.rm = TRUE),
      .by = c(year, area_code, live_anim_code)
    ) |>
    dplyr::mutate(
      year = as.integer(year),
      area_code = as.integer(area_code),
      live_anim_code = as.integer(live_anim_code)
    )
}

# Livestock output production (tonnes) per country, animal and item.
.grazing_production <- function(year) {
  out_items <- .livestock_output_items()
  get_primary_production() |>
    .collapse_production_to_fabio() |>
    dplyr::filter(
      .data$unit == "tonnes",
      year == .env$year,
      !is.na(live_anim_code),
      item_cbs_code %in% out_items,
      .data$value > 0
    ) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c(year, area_code, live_anim_code, item_cbs_code)
    ) |>
    dplyr::mutate(
      year = as.integer(year),
      area_code = as.integer(area_code),
      live_anim_code = as.integer(live_anim_code),
      item_cbs_code = as.integer(item_cbs_code)
    )
}

# Bilateral trade for the meat and milk output items in one year.
.grazing_meat_trade <- function(year, items) {
  get_bilateral_trade() |>
    melt_bilateral_trade() |>
    dplyr::filter(year == .env$year, item_cbs_code %in% items) |>
    dplyr::select(from_code, to_code, item_cbs_code, value)
}

# Surface grazing land in countries with no grazer intake: it cannot be
# handed forward to any animal, so it is dropped rather than silently
# conserved. Reported, never hidden.
.warn_unattributed_land <- function(grass_land, grazer_intake) {
  total <- sum(grass_land$value, na.rm = TRUE)
  if (total <= 0) {
    return(invisible(NULL))
  }
  covered <- unique(grazer_intake$area_code)
  dropped <- grass_land |>
    dplyr::filter(!area_code %in% covered)
  lost <- sum(dropped$value, na.rm = TRUE)
  if (lost <= 0) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "!" = "{round(100 * lost / total, 1)}% of grazing land is in
      {nrow(dropped)} countr{?y/ies} with no grazer intake and cannot be
      attributed to meat or dairy.",
    "i" = "These areas have grassland but no grazing-animal feed intake to
      hand the land forward to."
  ))
  invisible(dropped)
}
