# FAOSTAT as an opt-in source for the nitrogen balance's manure terms
# (whep#1197).
#
# build_n_inputs() takes its three manure terms from the WHEP manure engine
# (build_livestock_nutrient_flows() over the realised feed intake). This file is
# the selectable alternative, `manure_method = "faostat"`: the same three terms
# read from the FAOSTAT livestock-emissions domain (the
# `faostat-emissions-livestock` pin) and spread to crops and cells with the
# spreaders WHEP already uses for national totals. It is a cross-check on the
# engine, never the default and never a fallback.
#
#   * manure_solid + manure_liquid <- "Manure applied to soils (N content)",
#     Item "All Animals". Read by .manure_applied_n_country()
#     (R/crop_soil_n2o_extension.R), which keeps the "FAO TIER 1" source,
#     converts kg to t and re-keys onto the polity vocabulary, dropping the
#     aggregate reporting codes. Split into solid and liquid with the ENGINE's
#     own post-storage solid share per country-year, so the loss cascade's
#     Solid/Liquid factors still apply; spread to crops by harvested-area share
#     (.n_crop_area_shares(), the build_crop_soil_n2o_extension() precedent) and
#     to cells by spatialize_country_n_to_crops(), the spreader the synthetic
#     term already uses.
#   * excreta <- "Manure left on pasture (N content)", read PER SPECIES, mapped
#     to a species group through inst/extdata/livestock_mapping.csv and spread
#     to cells by build_gridded_livestock() with that mapping's spatial proxies
#     -- the livestock spreader the local feed-intake grain already uses.

# ---- Method vocabulary -------------------------------------------------------

.ni_manure_methods <- function() {
  c("livestock_intake", "faostat")
}

# Read off `data` so build_nitrogen_balance(), which forwards its whole `data`
# list, can select the source without an argument of its own. The default is the
# engine, so an unset value moves no published number.
.ni_manure_method <- function(data) {
  rlang::arg_match0(
    data$manure_method %||% "livestock_intake",
    .ni_manure_methods(),
    "manure_method"
  )
}

# The `data` entries whose presence asks for the manure stream. Under
# "faostat" the engine intake is still needed, for the solid:liquid split.
.ni_manure_stream_inputs <- function(method) {
  if (method == "faostat") {
    return(c("manure", "livestock_intake"))
  }
  "livestock_intake"
}

# The extra inputs a driver has to build for the chosen source, and only for
# it: under the default nothing here is built, so nothing here can fail.
.ni_manure_stages <- function(method) {
  if (method == "faostat") {
    return(c("manure", "livestock_spatial"))
  }
  character()
}

# ---- The FAOSTAT manure stream -----------------------------------------------

# Choosing the source is itself the request for these terms, so a missing input
# aborts rather than returning no manure. `data` is read with `[[` throughout:
# `data$manure` partial-matches `data$manure_method`.
.n_inputs_manure_faostat <- function(data) {
  .ni_check_faostat_inputs(data)
  dplyr::bind_rows(
    .ni_faostat_applied(data),
    .ni_faostat_pasture(data)
  )
}

.ni_check_faostat_inputs <- function(data) {
  needed <- c("manure", "livestock_intake", "primary_prod")
  if (!is.null(data[["cell_polity"]])) {
    needed <- c(needed, "livestock_spatial")
  }
  missing <- needed[purrr::map_lgl(needed, \(nm) is.null(data[[nm]]))]
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "{.code manure_method = \"faostat\"} needs {.field data${missing}}.",
      i = "{.field manure} is the {.val faostat-emissions-livestock} pin,
           {.field livestock_intake} supplies the manure engine's solid:liquid
           split, {.field primary_prod} the crop area shares and
           {.field livestock_spatial} the surfaces
           {.fn build_gridded_livestock} spreads pasture manure over."
    ),
    class = "whep_manure_faostat_input"
  )
}

# ---- Applied manure (cropland, solid + liquid) -------------------------------

.ni_faostat_applied <- function(data) {
  .ni_faostat_one_source(data[["manure"]]) |>
    dplyr::filter(.data$Element == "Manure applied to soils (N content)") |>
    .ni_check_faostat_unit("kg")
  split <- .manure_applied_n_country(data[["manure"]]) |>
    .ni_split_applied(.ni_engine_solid_share(data))
  shares <- .n_crop_area_shares(data$primary_prod)
  split |>
    dplyr::distinct(.data$fert_type, .data$method_manure) |>
    purrr::pmap(\(fert_type, method_manure) {
      split |>
        dplyr::filter(
          .data$fert_type == !!fert_type,
          .data$method_manure == !!method_manure
        ) |>
        dplyr::select("year", "area_code", "n_t") |>
        .ni_spread_country_to_crops(shares, data) |>
        dplyr::mutate(fert_type = fert_type, method_manure = method_manure)
    }) |>
    dplyr::bind_rows()
}

# The engine's post-storage solid share of collected manure N per
# country-year, over every land use it reached. The same engine run the
# default manure term uses, so the split is the default's own.
.ni_engine_solid_share <- function(data) {
  .ni_manure_flows(data)$applied |>
    dplyr::summarise(
      solid = sum(.data$applied_n[.data$manure_type == "Solid"]),
      liquid = sum(.data$applied_n[.data$manure_type == "Liquid"]),
      .by = c("year", "territory")
    ) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = .manure_territory_to_area_code(.data$territory),
      solid_share = .data$solid / (.data$solid + .data$liquid)
    ) |>
    dplyr::filter(is.finite(.data$solid_share))
}

# Split each national applied total by the engine's solid share. A country-year
# the engine gives no share for (no collected manure there at all) is booked
# wholly as solid and stamped "faostat_all_solid", and the warning names every
# such country-year with its tonnage: the mass stays, and the assumption is
# visible on the row that carries it.
.ni_split_applied <- function(totals, shares) {
  joined <- totals |>
    dplyr::filter(.data$manure_applied_n_t > 0) |>
    dplyr::left_join(shares, by = c("year", "area_code"))
  .ni_warn_all_solid(dplyr::filter(joined, is.na(.data$solid_share)))
  solid <- dplyr::transmute(
    joined,
    .data$year,
    .data$area_code,
    fert_type = "manure_solid",
    n_t = .data$manure_applied_n_t * dplyr::coalesce(.data$solid_share, 1),
    method_manure = dplyr::if_else(
      is.na(.data$solid_share),
      "faostat_all_solid",
      "faostat"
    )
  )
  liquid <- joined |>
    dplyr::filter(!is.na(.data$solid_share)) |>
    dplyr::transmute(
      .data$year,
      .data$area_code,
      fert_type = "manure_liquid",
      n_t = .data$manure_applied_n_t * (1 - .data$solid_share),
      method_manure = "faostat"
    )
  dplyr::bind_rows(solid, liquid)
}

.ni_warn_all_solid <- function(unsplit) {
  if (nrow(unsplit) == 0L) {
    return(invisible(NULL))
  }
  labels <- paste0(
    unsplit$area_code,
    "/",
    unsplit$year,
    ": ",
    signif(unsplit$manure_applied_n_t, 6),
    " t N"
  )
  cli::cli_warn(
    c(
      "Booking FAOSTAT applied manure as solid for {nrow(unsplit)}
       country-year{?s} the manure engine gives no solid:liquid split for.",
      i = "{.field area_code}/{.field year}: {.val {labels}}.",
      i = "Stamped {.val faostat_all_solid} in {.field method_manure}."
    ),
    class = "whep_manure_faostat_all_solid"
  )
}

# spatialize_country_n_to_crops(), called exactly as the synthetic term calls
# it: to crops only when there are no cells, to cells when there are.
.ni_spread_country_to_crops <- function(totals, shares, data) {
  if (is.null(data$cell_polity)) {
    return(
      spatialize_country_n_to_crops(
        totals,
        shares,
        cell_polity = NULL,
        resolution = "polity_crop",
        polity_validity = .ni_polity_validity(data)
      ) |>
        dplyr::transmute(
          lon = NA_real_,
          lat = NA_real_,
          .data$area_code,
          .data$item_cbs_code,
          .data$year,
          n_input_t = .data$n_t
        )
    )
  }
  spatialize_country_n_to_crops(
    totals,
    shares,
    cell_polity = data$cell_polity,
    resolution = "grid",
    polity_validity = .ni_polity_validity(data),
    data = list(
      crop_patterns = data$crop_patterns,
      type_cropland = data$type_cropland
    )
  ) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      .data$area_code,
      .data$item_cbs_code,
      .data$year,
      n_input_t = .data$n_t
    )
}

# ---- Pasture manure (grassland, excreta) -------------------------------------

.ni_faostat_pasture <- function(data) {
  species <- .ni_pasture_by_species(data[["manure"]]) |>
    .ni_pasture_species_groups(.ni_livestock_mapping(data))
  rows <- if (is.null(data$cell_polity)) {
    .ni_pasture_polity(species)
  } else {
    .ni_pasture_grid(species, data)
  }
  dplyr::mutate(
    rows,
    item_cbs_code = 3000L,
    fert_type = "excreta",
    method_manure = "faostat"
  )
}

# The FAOSTAT aggregate items of the livestock-emissions domain, each the sum of
# leaf items also in the pin: All Animals, Cattle, Chickens, Poultry Birds,
# Sheep and Goats, Swine, Mules and Asses, Camels and Llamas. Reading a leaf
# and its aggregate would count the same manure twice, so these are excluded
# by code and the leaves are then checked to sum to "All Animals".
.ni_faostat_aggregate_items <- function() {
  c(1755L, 1757L, 1054L, 2029L, 1749L, 1048L, 1759L, 1760L)
}

# Per-species "Manure left on pasture (N content)" in tonnes N, one reporting
# source, territories only (the aggregate reporting codes are dropped through
# the same crosswalk .n_country_to_polity() uses).
.ni_pasture_by_species <- function(manure) {
  element <- "Manure left on pasture (N content)"
  check_labels_supplied(
    manure,
    "Element",
    element,
    details = c(i = "Source: the {.val faostat-emissions-livestock} pin.")
  )
  rows <- .ni_faostat_one_source(manure) |>
    dplyr::filter(.data$Element == element)
  .ni_check_faostat_unit(rows, "kg")
  rows |>
    dplyr::transmute(
      year = as.integer(.data$Year),
      area_code = as.integer(.data[["Area Code"]]),
      item_code = as.integer(.data[["Item Code"]]),
      item = .data$Item,
      n_t = .data$Value / 1000
    ) |>
    dplyr::filter(
      !is.na(.data$n_t),
      .data$n_t >= 0,
      .data$area_code %in% .ni_territory_codes()
    ) |>
    .ni_check_pasture_leaves()
}

.ni_faostat_one_source <- function(manure) {
  if (rlang::has_name(manure, "Source")) {
    manure <- dplyr::filter(manure, .data$Source == "FAO TIER 1")
  }
  manure
}

.ni_check_faostat_unit <- function(rows, unit) {
  if (!rlang::has_name(rows, "Unit")) {
    return(invisible(rows))
  }
  found <- setdiff(unique(rows$Unit), unit)
  if (length(found) > 0L) {
    cli::cli_abort(c(
      "FAOSTAT manure N is not in {.val {unit}}.",
      i = "Found unit{?s}: {.val {found}}."
    ))
  }
  invisible(rows)
}

# The reporting codes that are territories: every code the polity crosswalk
# resolves to a polity in some year, the set .n_country_to_polity() keeps.
# FAOSTAT's rollups -- 5000 "World", the 5xxx regions, 420 "Sub-Saharan
# Africa" and 351 "China" -- resolve to none, and reading them alongside their
# members would count the same manure twice (420 alone is 18.4 Mt of pasture N
# in 2010).
.ni_territory_codes <- function() {
  crosswalk <- tibble::as_tibble(as.data.frame(.polity_crosswalk()))
  keep <- !is.na(crosswalk$area_code) & !is.na(crosswalk$polity_code)
  unique(as.integer(crosswalk$area_code[keep]))
}

# The leaves (every item except the aggregates) must add back to "All
# Animals" in every country-year that reports it; a new aggregate or a lost
# leaf would otherwise double count or drop manure without a trace.
.ni_check_pasture_leaves <- function(rows) {
  aggregates <- .ni_faostat_aggregate_items()
  total <- rows |>
    dplyr::filter(.data$item_code == 1755L) |>
    dplyr::select("year", "area_code", all_animals = "n_t")
  leaves <- dplyr::filter(rows, !.data$item_code %in% aggregates)
  gap <- leaves |>
    dplyr::summarise(
      leaf_sum = sum(.data$n_t),
      .by = c("year", "area_code")
    ) |>
    dplyr::inner_join(total, by = c("year", "area_code")) |>
    dplyr::filter(
      abs(.data$leaf_sum - .data$all_animals) >
        1e-6 * pmax(.data$all_animals, 1)
    )
  if (nrow(gap) > 0L) {
    cli::cli_abort(
      c(
        "FAOSTAT per-species pasture manure N does not add up to
         {.val All Animals} in {nrow(gap)} country-year{?s}.",
        i = "Largest gap: {signif(max(abs(gap$leaf_sum - gap$all_animals)),
             6)} t N. The pin may have gained an aggregate item or lost a
             species."
      ),
      class = "whep_manure_faostat_leaves"
    )
  }
  leaves
}

# FAOSTAT live-animal item code -> species group and spatial proxy, the table
# build_gridded_livestock()'s production callers pass (run_spatialize(), the
# local feed-intake grain). `data$livestock_spatial$species_proxy` overrides
# only the proxy of each group.
.ni_livestock_mapping <- function(data) {
  path <- system.file("extdata", "livestock_mapping.csv", package = "whep")
  mapping <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::transmute(
      item_code = as.integer(.data$item_code),
      .data$species_group,
      .data$spatial_proxy
    )
  proxy <- data$livestock_spatial$species_proxy
  if (is.null(proxy)) {
    return(mapping)
  }
  mapping |>
    dplyr::select(-"spatial_proxy") |>
    dplyr::left_join(
      dplyr::distinct(proxy, .data$species_group, .data$spatial_proxy),
      by = "species_group"
    )
}

# Map each leaf item to its species group. An item the mapping does not name
# is excluded with a warning that carries its tonnage, never dropped silently;
# a group with no spatial proxy aborts, because build_gridded_livestock() would
# otherwise place it on pasture by its own silent fallback.
.ni_pasture_species_groups <- function(species, mapping) {
  joined <- dplyr::left_join(
    species,
    dplyr::select(mapping, "item_code", "species_group", "spatial_proxy"),
    by = "item_code"
  )
  .ni_warn_unmapped_species(dplyr::filter(joined, is.na(.data$species_group)))
  mapped <- dplyr::filter(joined, !is.na(.data$species_group))
  no_proxy <- unique(mapped$species_group[is.na(mapped$spatial_proxy)])
  if (length(no_proxy) > 0L) {
    cli::cli_abort(c(
      "No spatial proxy for species group{?s} {.val {no_proxy}}.",
      i = "Every group needs a {.field spatial_proxy} in
           {.file livestock_mapping.csv} or
           {.field data$livestock_spatial$species_proxy}."
    ))
  }
  mapped |>
    dplyr::summarise(
      pasture_n_t = sum(.data$n_t),
      .by = c("year", "area_code", "species_group")
    )
}

.ni_warn_unmapped_species <- function(unmapped) {
  if (nrow(unmapped) == 0L) {
    return(invisible(NULL))
  }
  by_item <- unmapped |>
    dplyr::summarise(n_t = sum(.data$n_t), .by = c("item_code", "item")) |>
    dplyr::mutate(
      label = paste0(
        .data$item,
        " (",
        .data$item_code,
        "): ",
        signif(.data$n_t, 6),
        " t N"
      )
    )
  cli::cli_warn(
    c(
      "Excluding FAOSTAT pasture manure N of {nrow(by_item)} item{?s} that
       {.file livestock_mapping.csv} maps to no species group.",
      i = "{.val {by_item$label}}.",
      i = "Add the item to the mapping to spread it."
    ),
    class = "whep_manure_faostat_unmapped"
  )
}

.ni_pasture_polity <- function(species) {
  species |>
    dplyr::summarise(
      n_t = sum(.data$pasture_n_t),
      .by = c("year", "area_code")
    ) |>
    .n_country_to_polity("n_t") |>
    dplyr::transmute(
      lon = NA_real_,
      lat = NA_real_,
      .data$area_code,
      .data$year,
      n_input_t = .data$n_t
    )
}

# build_gridded_livestock() over the balance's own cell partition
# (`cell_polity`), with the pasture, cropland and manure-intensity surfaces
# its production callers use. Reporting codes are kept (`area_key = "grid"`),
# the code space every other gridded term of the balance carries.
.ni_pasture_grid <- function(species, data) {
  spatial <- data$livestock_spatial
  proxy <- dplyr::distinct(species, .data$species_group) |>
    dplyr::left_join(
      dplyr::distinct(
        .ni_livestock_mapping(data),
        .data$species_group,
        .data$spatial_proxy
      ),
      by = "species_group"
    )
  gridded <- build_gridded_livestock(
    livestock_data = species,
    gridded_pasture = spatial$gridded_pasture,
    gridded_cropland = spatial$gridded_cropland,
    country_grid = data$cell_polity,
    species_proxy = proxy,
    manure_pattern = spatial$manure_pattern,
    area_key = "grid"
  )
  .ni_warn_pasture_unplaced(species, gridded)
  gridded |>
    dplyr::summarise(
      n_input_t = sum(.data$pasture_n_t),
      .by = c("lon", "lat", "area_code", "year")
    )
}

# build_gridded_livestock() warns about countries it could not place but counts
# heads, not nitrogen; this says how much pasture N that cost.
.ni_warn_pasture_unplaced <- function(species, gridded) {
  national <- sum(species$pasture_n_t)
  placed <- sum(gridded$pasture_n_t)
  lost <- species |>
    dplyr::anti_join(
      dplyr::distinct(gridded, .data$year, .data$area_code),
      by = c("year", "area_code")
    )
  if (national - placed <= 1e-9 * max(national, 1)) {
    return(invisible(NULL))
  }
  codes <- sort(unique(lost$area_code))
  cli::cli_warn(
    c(
      "{signif(national - placed, 6)} t of {signif(national, 6)} t FAOSTAT
       pasture manure N found no proxy cell and is not in the balance.",
      i = "{cli::qty(length(codes))}Unplaced {.field area_code}{?s}:
           {.val {codes}}."
    ),
    class = "whep_manure_faostat_unplaced"
  )
}

# The surfaces .ni_pasture_grid() spreads over, read from the same pins
# build_gridded_livestock()'s production callers read (run_spatialize()'s
# .load_livestock_inputs()), scoped to `years`. The manure-intensity pattern is
# required here, where production treats it as optional, so the weights cannot
# change silently with a failed read.
.ni_read_livestock_spatial <- function(years) {
  aliases <- .spatial_input_aliases()
  read <- \(file, alias) .read_spatial_input(NULL, file, aliases[[alias]])
  list(
    gridded_pasture = read("gridded_pasture.parquet", "gridded_pasture") |>
      dplyr::filter(.data$year %in% years),
    gridded_cropland = read("gridded_cropland.parquet", "gridded_cropland") |>
      dplyr::filter(.data$year %in% years),
    manure_pattern = read("manure_pattern.parquet", "manure_pattern")
  )
}

# ---- Applied manure with nowhere to go --------------------------------------

# FAOSTAT applied manure of polity-years with no crop share or no cropland cell,
# which spatialize_country_n_to_crops() refuses to spread (it aborts rather
# than lose nitrogen). The same gap, and the same answer, as the synthetic
# fertiliser's .n_drop_uncelled_fertilizer(): "drop" removes the raw FAOSTAT
# applied rows of those polities and RETURNS what it removed, per year and
# area_code with its share of that year's global applied manure N; "abort"
# refuses. Measured on the pin: 2010, 23 polities and 0.52% of global applied
# manure N (0.126 Mt of it the Sudan bucket 206); 1990, 30 polities and 25.2%
# (5.84 Mt of it the USSR, 228), because the cell-polity map is year-invariant
# (whep#458). Only the applied element is touched; pasture manure is spread by
# build_gridded_livestock(), which reports what it cannot place.
.n_drop_uncelled_manure <- function(
  manure,
  supported,
  action = c("drop", "abort")
) {
  action <- rlang::arg_match(action)
  element <- "Manure applied to soils (N content)"
  removed <- .manure_applied_n_country(manure) |>
    dplyr::mutate(
      global_applied_n_t = sum(.data$manure_applied_n_t),
      .by = "year"
    ) |>
    dplyr::filter(.data$manure_applied_n_t > 0) |>
    dplyr::anti_join(
      dplyr::distinct(supported, .data$year, .data$area_code),
      by = c("year", "area_code")
    ) |>
    dplyr::mutate(
      share_of_global = .data$manure_applied_n_t / .data$global_applied_n_t,
      method_unsupported_manure = action
    ) |>
    dplyr::arrange(.data$year, dplyr::desc(.data$manure_applied_n_t))
  if (nrow(removed) == 0L) {
    return(list(manure = manure, removed = removed))
  }
  if (action == "abort") {
    cli::cli_abort(
      c(
        "{nrow(removed)} polity-year{?s} report FAOSTAT applied manure N but
         ha{?s/ve} no crop share or cropland cell to spread it on.",
        i = "{signif(sum(removed$manure_applied_n_t), 4)} t N; area codes:
             {unique(removed$area_code)}.",
        i = "Choose {.val drop} to remove it and record the removal."
      ),
      class = "whep_uncelled_manure"
    )
  }
  drop <- removed |>
    dplyr::reframe(
      raw = .polity_raw_area_codes(.data$area_code),
      .by = "year"
    )
  keep <- manure |>
    dplyr::mutate(
      .drop = .data$Element == element &
        paste(as.integer(.data$Year), as.integer(.data[["Area Code"]])) %in%
          paste(drop$year, drop$raw)
    )
  list(
    manure = dplyr::select(dplyr::filter(keep, !.data$.drop), -".drop"),
    removed = removed
  )
}
