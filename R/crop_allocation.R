# The one within-polity crop placement the nitrogen and the soil carbon chains
# share (whep#1002). A crop's polity total reaches cells by the same weights
# whichever chain carries it, so the carbon a crop returns to a cell and the
# nitrogen it receives there are spread alike:
#
# - a polity-crop the spatialization engine places ([build_gridded_landuse()]
#   on the pinned spatialization inputs and the run's cell support, year by
#   year) takes the engine's harvested-area shares;
# - a fodder crop the engine leaves out (whep#1372) takes the fodder layer's
#   shares, as build_soil_carbon_inputs() places it under its default
#   fodder-pattern rule for unspatialized crops;
# - any other polity-crop is spread over the polity's cropland, each cell
#   weighted by its share of the polity's engine crop area
#   (.sci_cropland_weights()).
#
# The weights are built by the carbon chain's own helpers, so the two chains
# cannot drift apart again. `data$crop_allocation` supplies them pre-built (a
# list with `weights` and `fallback`, each carrying `year`), which is how a
# run builds them once and every term reads the same table; it may carry
# `share`, the name of the polity share of the cell it was built on.

# The allocation for `years` on the cell support `cell_polity`: a list of
# `weights` (`lon`, `lat`, `area_code`, integer `item_prod_code`, `year`,
# `crop_area_ha`, `area_weight` summing to 1 per polity-crop-year) and
# `fallback` (`lon`, `lat`, `area_code`, `year`, `cropland_weight` summing to
# 1 per polity-year), and `share`, the polity share of the cell the support
# carried (`cell_polity`'s own name for it, as .normalize_country_grid()
# picks it, unless the caller names it). A built allocation goes through the
# same checks as a supplied one.
.crop_allocation <- function(years, cell_polity, data = list(), share = NULL) {
  if (!is.null(data$crop_allocation)) {
    return(.crop_allocation_check(data$crop_allocation, years))
  }
  inputs <- data$engine_inputs %||% .sci_read_engine_inputs()
  fodder <- data$fodder_patterns %||% .sci_read_fodder_patterns()
  parts <- purrr::map(sort(unique(as.integer(years))), \(yr) {
    .crop_allocation_year(yr, cell_polity, inputs, fodder, data$gridded_crops)
  })
  list(
    weights = dplyr::bind_rows(purrr::map(parts, "weights")),
    fallback = dplyr::bind_rows(purrr::map(parts, "fallback")),
    share = share %||% .crop_allocation_share(cell_polity)
  ) |>
    .crop_allocation_check(years, supplied = FALSE)
}

# The polity share of the cell a support carries, by its own name: a support
# .normalize_country_grid() has copied into `cell_area_frac` keeps the column
# the copy was taken from, and that is the name recorded.
.crop_allocation_share <- function(cell_polity) {
  shares <- intersect(.polity_share_cols(), names(cell_polity))
  c(setdiff(shares, "cell_area_frac"), shares)[1]
}

# The static crop-pattern layer is not read by the nitrogen chain, which
# places every crop by the crop allocation. Handed in, it would be ignored
# while the allocation is read from the pins in its place, so the caller
# would get a different geography from the one they supplied without being
# told: refused, as build_soil_carbon_inputs() refuses it under its engine
# weights (.sci_check_weight_layers()).
.crop_alloc_refuse_patterns <- function(data, fn) {
  if (is.null(data$crop_patterns)) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.code data$crop_patterns} is the static crop-pattern layer, which
       {.fn {fn}} does not read.",
      i = "Crops are placed by the crop allocation the soil carbon inputs
           share (whep#1002): supply {.code data$crop_allocation}, or
           {.code data$gridded_crops} or {.code data$engine_inputs} to build
           it from."
    ),
    class = "whep_crop_allocation_static_layer"
  )
}

# One year of the allocation, through the carbon chain's own weight builders.
.crop_allocation_year <- function(yr, cell_polity, inputs, fodder, gridded) {
  layer <- gridded %||% .sci_engine_crops(yr, cell_polity, inputs)
  crops <- layer |>
    dplyr::filter(.data$year == yr) |>
    .sci_spatialized_weights()
  w <- .sci_support_weights(
    crops,
    "fodder_pattern",
    list(country_grid = cell_polity, fodder_patterns = fodder)
  )
  list(
    weights = w$weights |>
      dplyr::mutate(
        item_prod_code = .as_integer_quiet(.data$item_prod_code),
        year = yr
      ),
    fallback = dplyr::mutate(w$fallback, year = yr)
  )
}

# An allocation must hold both tables and carry weights that sum to 1 within
# each group, and a supplied one must cover the years asked for: a partial
# table would leave a crop's nitrogen with no cells while its polity total
# still reconciled. A built one may hold no row in a year the engine placed
# nothing in; the term spreading onto it refuses that with its own message.
.crop_allocation_check <- function(allocation, years, supplied = TRUE) {
  .check_columns(
    allocation$weights,
    c("lon", "lat", "area_code", "item_prod_code", "year", "area_weight"),
    "crop_allocation$weights"
  )
  .check_columns(
    allocation$fallback,
    c("lon", "lat", "area_code", "year", "cropland_weight"),
    "crop_allocation$fallback"
  )
  weights <- tibble::as_tibble(allocation$weights) |>
    dplyr::mutate(
      area_code = as.integer(.data$area_code),
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      year = as.integer(.data$year)
    )
  fallback <- tibble::as_tibble(allocation$fallback) |>
    dplyr::mutate(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year)
    )
  absent <- setdiff(as.integer(years), fallback$year)
  if (supplied && length(absent) > 0L) {
    cli::cli_abort(
      "{.code data$crop_allocation} covers no year {.val {absent}}.",
      class = c("whep_crop_allocation", "whep_absent_input")
    )
  }
  .crop_allocation_check_sums(weights, "area_weight", "item_prod_code")
  .crop_allocation_check_sums(fallback, "cropland_weight", NULL)
  list(weights = weights, fallback = fallback, share = allocation$share)
}

.crop_allocation_check_sums <- function(table, col, item) {
  invalid <- !is.finite(table[[col]]) | table[[col]] < 0
  if (any(invalid)) {
    cli::cli_abort(
      "{sum(invalid)} weight{?s} in {.field {col}} of
       {.code data$crop_allocation} {?is/are} missing, non-finite or negative.",
      class = "whep_crop_allocation"
    )
  }
  sums <- dplyr::summarise(
    table,
    total = sum(.data[[col]]),
    .by = dplyr::all_of(c("area_code", item, "year"))
  )
  bad <- abs(sums$total - 1) > 1e-9
  if (any(bad)) {
    cli::cli_abort(
      "{sum(bad)} group{?s} of {.code data$crop_allocation} {?has/have}
       {.field {col}} not summing to 1.",
      class = "whep_crop_allocation"
    )
  }
}

# Each polity-crop's cell shares for commodity balance items: the production
# items of an item pooled by their own engine hectares (`crop_area_ha`), so a
# polity's cell share of the item is its share of the item's placed area.
# Returns `lon`, `lat`, `area_code`, `year`, `item_cbs_code`, `cell_share`
# summing to 1 per polity-item-year.
.crop_allocation_cbs <- function(allocation, item_prod_codes) {
  allocation$weights |>
    dplyr::inner_join(
      item_prod_codes,
      by = "item_prod_code",
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      ha = sum(.data$crop_area_ha),
      .by = c("lon", "lat", "area_code", "year", "item_cbs_code")
    ) |>
    dplyr::filter(is.finite(.data$ha), .data$ha > 0) |>
    dplyr::mutate(
      cell_share = .data$ha / sum(.data$ha),
      .by = c("area_code", "year", "item_cbs_code")
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "item_cbs_code",
      "cell_share"
    )
}

# Spread the extensive columns of a table keyed by polity, production item and
# year onto cells: by the polity-crop's weights where the allocation places
# it, else over the polity's cropland (`fallback`). Each extensive column's
# polity-crop total is kept exactly; a polity-crop with no cell at all is
# returned in the `unplaced` element for the caller to report or refuse. The
# table's own key columns keep their types: the join runs on integer copies.
.crop_allocation_spread <- function(table, allocation, extensive) {
  table <- dplyr::mutate(
    table,
    .k_area = as.integer(.data$area_code),
    .k_item = .as_integer_quiet(.data$item_prod_code),
    .k_year = as.integer(.data$year)
  )
  keys <- c(".k_area", ".k_item", ".k_year")
  weights <- dplyr::transmute(
    allocation$weights,
    .k_area = .data$area_code,
    .k_item = .data$item_prod_code,
    .k_year = .data$year,
    .data$lon,
    .data$lat,
    .weight = .data$area_weight
  )
  fallback <- dplyr::transmute(
    allocation$fallback,
    .k_area = .data$area_code,
    .k_year = .data$year,
    .data$lon,
    .data$lat,
    .weight = .data$cropland_weight
  )
  matched <- dplyr::inner_join(
    table,
    weights,
    by = keys,
    relationship = "many-to-many"
  )
  rest <- dplyr::anti_join(table, weights, by = keys)
  moved <- dplyr::inner_join(
    rest,
    fallback,
    by = c(".k_area", ".k_year"),
    relationship = "many-to-many"
  )
  unplaced <- dplyr::anti_join(rest, fallback, by = c(".k_area", ".k_year"))
  spread <- dplyr::bind_rows(matched, moved) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(extensive), \(v) v * .data$.weight)
    ) |>
    dplyr::select(-".weight", -dplyr::all_of(keys))
  list(
    spread = spread,
    unplaced = dplyr::select(unplaced, -dplyr::all_of(keys))
  )
}

# The national per-crop NPP chain on cells, for the gridded nitrogen balance
# (.n_inputs_bnf(), .n_inputs_recycling() and prod_n_t read lon/lat off it):
# every extensive column spread by the crop allocation, so the crop NPP, the
# BNF and the recycled residue of a polity-crop land where its soil carbon
# inputs do. Polity totals are kept exactly; a polity-crop with no cell at all
# is left out and reported by the caller (.nbd_report_unspatialized()).
.n_grid_npp <- function(npp, allocation) {
  keys <- c("year", "area_code", "item_prod_code")
  extensive <- setdiff(names(npp)[vapply(npp, is.numeric, TRUE)], keys)
  # .n_inputs_recycling() reads item_cbs_code straight off this table, while
  # .n_inputs_bnf() derives it from item_prod_code. Apply the package's own
  # mapping once here so all three consumers of this table see the same codes.
  out <- npp |>
    dplyr::mutate(
      item_cbs_code = .ni_item_cbs_from_prod(.data$item_prod_code)
    ) |>
    .crop_allocation_spread(allocation, extensive)
  out$spread
}
