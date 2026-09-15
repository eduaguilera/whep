# nolint start: object_length_linter.
# Per-cell IPCC livestock emissions.
#
# WHY THIS EXISTS (whep#1016): the gridded livestock artifact carries
# enteric_ch4_kt, manure_ch4_kt and manure_n2o_kt as EXACTLY ZERO in all
# 208,244 country rows it is spatialized from, because the country table it
# joins no longer holds those FAOSTAT elements and the join's NAs are turned
# into literal zeros. This module computes the three quantities per cell from
# WHEP's own IPCC model instead of disaggregating a national total, so the
# climate zone, the ambient temperature and the diet vary within a country
# rather than being one national constant.
#
# CONFIRMED FACTS (checked in this repo; do not re-derive):
# - `estimate_energy_demand()` performs no aggregation, no national join and no
#   normalisation over its input: every summarise() is over package reference
#   tables and every join is a broadcast lookup. It is therefore safe to apply
#   per cell unchanged, which is what makes a per-cell run tractable at all.
# - The FAOSTAT emission family (`*_kt`, kilotonnes) and the IPCC family
#   (`enteric_ch4_tier1|2`, kilograms) never met anywhere in the package.
#   `livestock_emissions_to_kt()` below is that bridge, in one place.
# - `ipcc_tier2_energy_coefs` covers Cattle, Buffalo, Sheep and Goats only, so
#   Tier 2 leaves camels, equines, swine and poultry unresolved (NA, warned),
#   never zero.

#' Build per-cell livestock greenhouse-gas emissions.
#'
#' @description
#' Run the IPCC 2019 livestock emission model on a gridded herd, one 0.5-degree
#' cell at a time, and return enteric CH4, manure CH4 and manure N2O in
#' kilotonnes per cell, year and species.
#'
#' Unlike a spatial disaggregation of a national emission total, this resolves
#' the drivers that actually vary within a country: the manure-management
#' climate zone and the ambient temperature come from the cell
#' ([build_cell_climate_zone()]), and the diet quality comes from the cell's own
#' feed mix when one is available. A national total spread over cells cannot
#' show any of that, because every cell of a country then carries the same
#' implied climate and diet.
#'
#' **Both grains are reported.** The gridded sum is the primary output. The
#' same model is also run once per country on the head-weighted national mean
#' temperature and the national diet, and each cell additionally carries that
#' national estimate rescaled onto it (`*_national_kt`) plus the per-country
#' ratio between the two (`divergence_*`). The global difference between the
#' two grains is small while the per-country difference is not, so the
#' per-country ratio is emitted per row rather than summarised away.
#'
#' @param gridded_livestock A tibble of gridded head counts (required unless
#'   `example = TRUE`), for example from
#'   [build_gridded_livestock()], with columns `lon`, `lat`, `year`,
#'   `area_code`, `heads`, and either `species` (an IPCC species label such as
#'   `"Cattle, dairy"`) or `species_group` (a spatializer group label). Groups
#'   that name more than one IPCC species (`"sheep_goats"`, `"equines"`,
#'   `"poultry"`, `"other"`) abort rather than being split on an assumption.
#'   Polity columns and cell identifiers are preserved when present.
#' @param method_diet How each row's `diet_quality` is resolved, in decreasing
#'   rigour:
#'   - `"per_cell_feed"` (default): from the cell's own feed mix, falling back
#'     per row to that country's national mix where a cell has no classifiable
#'     feed. Needs `data$feed_intake` at cell grain (a `sub_territory` column),
#'     for example from [build_feed_intake_local()].
#'   - `"national_feed"`: from the country's feed mix. Needs
#'     `data$feed_intake`, for example from [get_feed_intake()].
#'   - `"uniform_medium"`: every row gets the IPCC `"Medium"` diet. This is an
#'     assumption, not a measurement, and is never selected implicitly.
#'
#'   Whatever is requested, the value actually used is recorded per row in
#'   `method_diet`. A row that no requested method resolves aborts.
#' @param tier IPCC tier, `2` (default) or `1`. Tier 2 is the default here
#'   because the per-cell drivers only enter the Tier 2 energy and
#'   manure-management equations; Tier 1 emission factors carry no climate,
#'   temperature or diet dimension, so a Tier 1 grid differs from a
#'   disaggregated national total only by rounding.
#' @param data Optional named list of pre-loaded inputs: `cell_climate` (a
#'   [build_cell_climate_zone()] output) and `feed_intake` (a feed-intake
#'   table). `cell_climate` falls back to [build_cell_climate_zone()], which
#'   reads CRU from `WHEP_CRU_DIR`. `feed_intake` has no fallback: the readers
#'   that produce it rebuild the whole feed allocation, so it is supplied or
#'   the diet method is `"uniform_medium"`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with one row per `year`, `area_code`, `lon`, `lat` and
#'   `species`:
#' - `heads`: Head count in the cell.
#' - `enteric_ch4_kt`, `manure_ch4_kt`, `manure_n2o_kt`: Gridded emissions
#'   (kilotonnes), the primary output.
#' - `enteric_ch4_national_kt`, `manure_ch4_national_kt`,
#'   `manure_n2o_national_kt`: The national-grain estimate rescaled onto the
#'   cell, so summing these over a country reproduces the national-grain run.
#' - `divergence_enteric_ch4`, `divergence_manure_ch4`,
#'   `divergence_manure_n2o`: Per-country ratio of the gridded total to the
#'   national-grain total. `1` means the two grains agree.
#' - `mean_annual_temp_c`, `climate_zone`, `diet_quality`: The resolved
#'   per-cell drivers.
#' - `method_climate_zone`, `method_diet`, `method_enteric`,
#'   `method_manure_ch4`, `method_manure_n2o`: Method tracking.
#'
#' plus the polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' build_gridded_livestock_emissions(example = TRUE)
build_gridded_livestock_emissions <- function(
  gridded_livestock = NULL,
  method_diet = c("per_cell_feed", "national_feed", "uniform_medium"),
  tier = 2,
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_gridded_livestock_emissions())
  }
  method_diet <- rlang::arg_match(method_diet)
  tier <- .check_ghg_tier(tier)
  cells <- gridded_livestock |>
    .check_gridded_livestock() |>
    .resolve_gridded_species() |>
    .join_cell_climate(data$cell_climate) |>
    dplyr::mutate(sub_territory = .cell_id(lon, lat)) |>
    .resolve_diet_quality(method_diet, data$feed_intake)

  gridded <- .emissions_at_grain(cells, tier, .cell_group_keys(cells))
  national <- cells |>
    .national_livestock_input() |>
    .resolve_diet_quality(
      .national_diet_method(method_diet),
      data$feed_intake
    ) |>
    .emissions_at_grain(tier, c("year", "area_code", "species"))

  .attach_national_grain(gridded, national) |>
    .add_reporting_polity_columns()
}

#' Convert IPCC per-animal livestock emissions from kilograms to kilotonnes.
#'
#' @description
#' Bridge the two unit families the package carries. The IPCC calculators
#' ([calculate_livestock_emissions()] and friends) emit kilograms of gas in
#' `enteric_ch4_tier1`/`enteric_ch4_tier2`, `manure_ch4_tier1`/
#' `manure_ch4_tier2` and `manure_n2o_total`. The FAOSTAT-shaped gridded and
#' national artifacts carry kilotonnes in `enteric_ch4_kt`, `manure_ch4_kt` and
#' `manure_n2o_kt`. Nothing converted between them, so the two families never
#' met; this is the one place that conversion happens.
#'
#' The factor is exact: 1 kilotonne is 1e6 kilograms.
#'
#' @param data A tibble from the IPCC livestock calculators, carrying the
#'   kilogram columns for the requested `tier`. A missing column aborts rather
#'   than being treated as zero.
#' @param tier IPCC tier the kilogram columns come from, `2` (default) or `1`.
#'
#' @return `data` with `enteric_ch4_kt`, `manure_ch4_kt` and `manure_n2o_kt`
#'   added.
#' @export
#'
#' @examples
#' tibble::tibble(
#'   enteric_ch4_tier1 = 8e7,
#'   manure_ch4_tier1 = 1e7,
#'   manure_n2o_total = 5e5
#' ) |>
#'   livestock_emissions_to_kt(tier = 1)
livestock_emissions_to_kt <- function(data, tier = 2) {
  tier <- .check_ghg_tier(tier)
  pairs <- .emission_kt_columns(tier)
  missing <- setdiff(pairs$kg, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "Cannot convert to kilotonnes: {.field {missing}} {?is/are} absent.",
      i = "Expected the Tier {tier} output of
           {.fun calculate_livestock_emissions}."
    ))
  }
  converted <- purrr::set_names(
    purrr::map(pairs$kg, \(col) data[[col]] / .kg_per_kilotonne()),
    pairs$kt
  )
  dplyr::mutate(data, !!!converted)
}

# Private helpers ----

# 1 kilotonne = 1e6 kilograms. Exact, by definition of the SI prefixes.
.kg_per_kilotonne <- function() {
  1e6
}

.emission_kt_names <- function() {
  c("enteric_ch4_kt", "manure_ch4_kt", "manure_n2o_kt")
}

# Kilogram source column -> kilotonne target column, per tier. Manure N2O has
# one name at both tiers because `.calc_manure_n2o_tier1()` writes into the
# same `manure_n2o_total` column the Tier 2 path uses.
.emission_kt_columns <- function(tier) {
  tibble::tibble(
    kg = c(
      paste0("enteric_ch4_tier", tier),
      paste0("manure_ch4_tier", tier),
      "manure_n2o_total"
    ),
    kt = .emission_kt_names()
  )
}

# Columns a gridded herd must carry, plus the zero-head trim. A zero-head cell
# contributes nothing and would give the head-weighted national temperature a
# zero weight, so it is dropped before anything else.
.check_gridded_livestock <- function(gridded_livestock) {
  if (is.null(gridded_livestock)) {
    cli::cli_abort(
      "{.arg gridded_livestock} is required unless {.code example = TRUE}."
    )
  }
  cells <- tibble::as_tibble(gridded_livestock)
  required <- c("lon", "lat", "year", "area_code", "heads")
  missing <- setdiff(required, names(cells))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "{.arg gridded_livestock} is missing required column{?s}:
       {.field {missing}}."
    )
  }
  if (!any(rlang::has_name(cells, c("species", "species_group")))) {
    cli::cli_abort(
      "{.arg gridded_livestock} needs a {.field species} or
       {.field species_group} column."
    )
  }
  dplyr::filter(cells, !is.na(heads), heads > 0)
}

# Spatializer group -> IPCC species label and the live-animal commodity code the
# feed-intake tables key their diets on.
#
# The four groups left out are aggregates over species whose IPCC coefficients
# differ, and splitting them needs a head split this function does not have:
# "sheep_goats" (Ym 6.7 vs 5.5 and different weights), "equines" (horses vs
# mules and asses), "poultry" (ducks, geese and turkeys share an EF but not one
# commodity code) and "other". They abort rather than being assigned to
# whichever member sorts first.
.gridded_species_map <- function() {
  tibble::tribble(
    ~species_group,       ~species,             ~item_cbs_code,
    "cattle_dairy",       "Cattle, dairy",      960L,
    "cattle_non_dairy",   "Cattle, non-dairy",  961L,
    "buffalo",            "Buffalo",            946L,
    "camels",             "Camels",             1126L,
    "pigs",               "Pigs",               1049L,
    "chickens_layers",    "Chickens, layers",   1052L,
    "chickens_broilers",  "Chickens, broilers", 1053L
  )
}

# Give every row an IPCC `species` and the `item_cbs_code` the diet is keyed on.
# A caller-supplied `species` wins; a `species_group` is resolved through the
# crosswalk above and aborts where the group is an aggregate.
.resolve_gridded_species <- function(cells) {
  if (rlang::has_name(cells, "species")) {
    return(.attach_species_item_code(cells))
  }
  lookup <- .gridded_species_map()
  unresolved <- setdiff(unique(cells$species_group), lookup$species_group)
  if (length(unresolved) > 0L) {
    cli::cli_abort(c(
      "{length(unresolved)} {.field species_group} value{?s} name{?s/} more
       than one IPCC species: {.val {unresolved}}.",
      i = "Splitting {cli::qty(unresolved)}{?it/them} is a category decision
           this function will not take.",
      i = "Supply a {.field species} column with an IPCC species label
           instead, or spatialize at species grain."
    ))
  }
  dplyr::left_join(cells, lookup, by = "species_group")
}

# Key a species-labelled herd to the live-animal commodity code the feed-intake
# tables call the same animal, unless the caller keyed it already.
.attach_species_item_code <- function(cells) {
  if (rlang::has_name(cells, "item_cbs_code")) {
    return(dplyr::mutate(cells, item_cbs_code = as.integer(item_cbs_code)))
  }
  codes <- tibble::as_tibble(animals_codes) |>
    dplyr::distinct(item_cbs, .keep_all = TRUE) |>
    dplyr::transmute(
      species = item_cbs,
      item_cbs_code = as.integer(item_cbs_code)
    )
  dplyr::left_join(cells, codes, by = "species")
}

# Attach the per-cell climate zone and the ambient temperature the Tier 2 energy
# model needs. A cell with no climate row is an unresolved cell, not a
# temperate one, so it aborts.
.join_cell_climate <- function(cells, cell_climate) {
  climate <- cell_climate %||%
    build_cell_climate_zone(years = sort(unique(cells$year)))
  joined <- cells |>
    dplyr::left_join(
      dplyr::select(
        tibble::as_tibble(climate),
        lon,
        lat,
        year,
        mean_annual_temp_c,
        climate_zone,
        method_climate_zone
      ),
      by = c("lon", "lat", "year")
    )
  n_missing <- sum(is.na(joined$climate_zone))
  if (n_missing > 0L) {
    cli::cli_abort(c(
      "{n_missing} gridded livestock row{?s} {?has/have} no climate zone.",
      i = "Every cell needs a mean annual temperature: an unresolved cell
           would silently take the hardcoded {.val Temperate} zone and its
           animals would be booked against the wrong methane conversion
           factor.",
      i = "Check that {.arg cell_climate} covers the same cells and years as
           {.arg gridded_livestock}."
    ))
  }
  dplyr::mutate(joined, temperature_c = mean_annual_temp_c)
}

# Time and space columns the two rungs of the diet ladder group on. The
# consuming animal is always added to these, and the feed item is what the
# taxonomy join reads, so neither appears here.
.cell_diet_group_cols <- function() {
  c("year", "area_code", "sub_territory")
}

.national_diet_group_cols <- function() {
  c("year", "area_code")
}

# Grouping keys the per-cell emissions are summed back to after the Tier 2
# cohort expansion, keeping the resolved drivers and any polity or cell
# identifiers the input carried.
.cell_group_keys <- function(cells) {
  c(
    "year",
    "area_code",
    "lon",
    "lat",
    "species",
    intersect(
      c(
        "species_group",
        "item_cbs_code",
        "polity_area_code",
        "reporting_polity_code",
        "reporting_polity_name",
        "reporting_polity_has_geometry",
        "polycell_id",
        "cell_id"
      ),
      names(cells)
    ),
    "heads",
    "mean_annual_temp_c",
    "climate_zone",
    "diet_quality",
    "method_climate_zone",
    "method_diet"
  )
}

# The national counterpart of the gridded herd: one row per country, year and
# species, carrying the head-weighted mean of the cells' temperatures. This is
# the grain a national emission total implies, and is what the gridded run is
# compared against.
.national_livestock_input <- function(cells) {
  cells |>
    dplyr::summarise(
      # Weight before the herd is summed: `summarise()` evaluates in order, so
      # this line still sees the per-cell heads that the next line collapses.
      mean_annual_temp_c = stats::weighted.mean(mean_annual_temp_c, heads),
      heads = sum(heads),
      .by = c(year, area_code, species, item_cbs_code)
    ) |>
    dplyr::mutate(
      climate_zone = .climate_zone_from_mat(mean_annual_temp_c),
      temperature_c = mean_annual_temp_c,
      method_climate_zone = "national_head_weighted_mean"
    )
}

# The national run mirrors the cell run's diet source: a feed-based cell diet is
# compared against the same country's feed-based national diet, and an assumed
# uniform diet against itself, so the divergence isolates the grain rather than
# mixing in a change of diet source.
.national_diet_method <- function(method_diet) {
  if (method_diet == "uniform_medium") "uniform_medium" else "national_feed"
}

# Run the IPCC chain and sum back to `keys` in kilotonnes. Tier 2 expands the
# herd into GLEAM cohorts first; Tier 1 stays at species grain, as
# `build_livestock_ghg_extension()` does. NA is not suppressed: an unresolved
# cohort must reach the output as NA, never as a zero.
.emissions_at_grain <- function(data, tier, keys) {
  expanded <- if (tier == 2L) calculate_cohorts_systems(data) else data
  expanded |>
    calculate_livestock_emissions(tier = tier) |>
    livestock_emissions_to_kt(tier = tier) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(.emission_kt_names()), sum),
      .by = dplyr::all_of(c(
        keys,
        "method_enteric",
        "method_manure_ch4",
        "method_manure_n2o"
      ))
    )
}

# Attach the national-grain estimate and the per-country divergence to every
# cell, then warn about whatever stayed unresolved.
.attach_national_grain <- function(gridded, national) {
  keys <- c("year", "area_code", "species")
  ratios <- gridded |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(.emission_kt_names()),
        \(x) sum(x, na.rm = TRUE)
      ),
      .by = dplyr::all_of(keys)
    ) |>
    dplyr::inner_join(
      dplyr::select(national, dplyr::all_of(c(keys, .emission_kt_names()))),
      by = keys,
      suffix = c("_grid", "_nat")
    ) |>
    dplyr::transmute(
      year,
      area_code,
      species,
      divergence_enteric_ch4 = .divergence_ratio(
        enteric_ch4_kt_grid,
        enteric_ch4_kt_nat
      ),
      divergence_manure_ch4 = .divergence_ratio(
        manure_ch4_kt_grid,
        manure_ch4_kt_nat
      ),
      divergence_manure_n2o = .divergence_ratio(
        manure_n2o_kt_grid,
        manure_n2o_kt_nat
      )
    )

  gridded |>
    dplyr::left_join(ratios, by = keys) |>
    dplyr::mutate(
      enteric_ch4_national_kt = enteric_ch4_kt / divergence_enteric_ch4,
      manure_ch4_national_kt = manure_ch4_kt / divergence_manure_ch4,
      manure_n2o_national_kt = manure_n2o_kt / divergence_manure_n2o
    ) |>
    .warn_unresolved_cells()
}

# Gridded total over national total. A zero or missing national total leaves the
# ratio undefined rather than infinite.
.divergence_ratio <- function(gridded_total, national_total) {
  dplyr::if_else(
    is.na(national_total) | national_total == 0,
    NA_real_,
    gridded_total / national_total
  )
}

# Say which species were left unresolved and how many head they carry, so a
# coverage gap is visible in the log rather than only in the NAs.
.warn_unresolved_cells <- function(emissions) {
  unresolved <- emissions |>
    dplyr::filter(dplyr::if_any(
      dplyr::all_of(.emission_kt_names()),
      is.na
    )) |>
    dplyr::summarise(heads = sum(heads), .by = species)
  if (nrow(unresolved) == 0L) {
    return(emissions)
  }
  # No cli pluralisation here: a `{?}` bound to a numeric VECTOR (rather than a
  # scalar count) aborts inside cli, which would turn a warning about a
  # coverage gap into a hard failure of the whole build.
  cli::cli_warn(c(
    "!" = "Unresolved emissions in at least one cell for
      {nrow(unresolved)} species: {.val {unresolved$species}}.",
    i = "Head count affected, in the same order:
         {.val {round(unresolved$heads)}}.",
    i = "{.field ipcc_tier2_energy_coefs} covers Cattle, Buffalo, Sheep and
         Goats only, so Tier 2 cannot resolve the others. They are returned as
         {.val NA}, never as zero."
  ))
  emissions
}

# --- Diet quality -----------------------------------------------------------

# Digestible-energy anchor per feed-taxonomy quality class.
#
# ASSUMED, UNVERIFIED. No published table maps whep's `feed_taxonomy` quality
# classes onto an IPCC diet class, so this mapping is an assumption of this
# package, not a measurement. It deliberately introduces NO new numbers: each
# class is anchored to one of the three DE percentages `feed_characteristics`
# already ships (High 75, Medium 65, Low 55, IPCC 2019 Vol.4 Ch.10), so the
# only judgement is which class sits at which anchor.
#
# The reasoning, class by class:
# - `lactation` (milk) and `high_quality` (oilseed cakes, grain by-products,
#   molasses, oils, fish and animal meals) are concentrates: High.
# - `low_quality` (brans, soy hulls, fodder crops, temporary grassland) and
#   `grass` (permanent grassland, fallow, acorns) are forages of ordinary
#   digestibility: Medium. Anchoring grass at Medium also means the classifier
#   only moves a grazing cell away from the package's existing uniform Medium
#   assumption when its concentrate or residue share actually does so.
# - `residues` (straw, other crop residues) are the least digestible: Low.
# - `zoot_fixed` (minerals, vitamins, amino acids, salts), `draught`,
#   `scavenging` and `non_feed` have no defensible single DE value -- a mineral
#   premix has no digestible energy at all -- so they are left unanchored and
#   excluded from the weighted mean rather than given an invented number.
.feed_quality_de_anchors <- function() {
  tibble::tribble(
    ~feed_quality,   ~de_anchor_class,
    "lactation",     "High",
    "high_quality",  "High",
    "low_quality",   "Medium",
    "grass",         "Medium",
    "residues",      "Low"
  ) |>
    dplyr::left_join(
      dplyr::select(
        tibble::as_tibble(feed_characteristics),
        de_anchor_class = diet_quality,
        de_anchor_percent = de_percent
      ),
      by = "de_anchor_class"
    )
}

# Classify a diet's mass-weighted mean DE% into the shipped IPCC diet classes by
# nearest anchor. The cut points are derived from `feed_characteristics` itself
# (the midpoints between consecutive anchors, 60 and 70 on the shipped table),
# not written down here, so they cannot drift away from the anchors they split.
.diet_quality_from_de <- function(de_percent) {
  anchors <- tibble::as_tibble(feed_characteristics) |>
    dplyr::distinct(diet_quality, de_percent) |>
    dplyr::arrange(de_percent)
  cuts <- (utils::head(anchors$de_percent, -1L) +
    utils::tail(anchors$de_percent, -1L)) /
    2
  anchors$diet_quality[findInterval(de_percent, cuts) + 1L]
}

# Mass-weighted diet class per consuming animal, from a feed-intake table.
# `group_cols` are the time and space keys; the consuming animal
# (`live_anim_code`) is always added, and is renamed to `item_cbs_code` on the
# way out because that is what the herd table calls the same animal. Feed
# classes with no DE anchor are excluded from the weighting.
.diet_from_intake <- function(feed_intake, group_cols) {
  intake <- .check_feed_intake(feed_intake, group_cols)
  intake |>
    dplyr::left_join(
      dplyr::select(
        tibble::as_tibble(feed_taxonomy),
        item_cbs_code,
        feed_quality
      ),
      by = "item_cbs_code"
    ) |>
    dplyr::left_join(.feed_quality_de_anchors(), by = "feed_quality") |>
    dplyr::filter(intake_dry_matter > 0) |>
    dplyr::summarise(
      de_percent_diet = .anchored_mean_de(de_anchor_percent, intake_dry_matter),
      .by = dplyr::all_of(c(group_cols, "live_anim_code"))
    ) |>
    dplyr::filter(!is.na(de_percent_diet)) |>
    dplyr::mutate(diet_quality = .diet_quality_from_de(de_percent_diet)) |>
    dplyr::rename(item_cbs_code = live_anim_code)
}

# Columns a feed-intake table must carry before it can classify a diet.
.check_feed_intake <- function(feed_intake, group_cols) {
  intake <- tibble::as_tibble(feed_intake)
  required <- c(
    group_cols,
    "live_anim_code",
    "item_cbs_code",
    "intake_dry_matter"
  )
  missing <- setdiff(required, names(intake))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.arg feed_intake} is missing required column{?s}: {.field {missing}}.",
      i = "A cell-grain diet additionally needs {.field sub_territory}, for
           example from {.fun build_feed_intake_local}."
    ))
  }
  dplyr::mutate(
    intake,
    item_cbs_code = as.integer(item_cbs_code),
    live_anim_code = as.integer(live_anim_code)
  )
}

# Mass-weighted DE% over the anchored feed only; NA when no anchored feed mass
# exists, which marks the key as unclassifiable rather than average.
.anchored_mean_de <- function(de_anchor_percent, intake_dry_matter) {
  keep <- !is.na(de_anchor_percent)
  if (!any(keep) || sum(intake_dry_matter[keep]) == 0) {
    return(NA_real_)
  }
  stats::weighted.mean(de_anchor_percent[keep], intake_dry_matter[keep])
}

# Resolve `diet_quality` for every row under the requested ladder, recording per
# row which rung actually supplied it. An unresolved row aborts: a missing diet
# is never quietly filled with "Medium", which would move gross energy, enteric
# CH4, volatile solids and nitrogen excretion at once.
.resolve_diet_quality <- function(data, method_diet, feed_intake) {
  if (method_diet == "uniform_medium") {
    return(
      .seed_supplied_diet(data) |>
        dplyr::mutate(
          method_diet = dplyr::if_else(
            is.na(diet_quality),
            "uniform_medium",
            method_diet
          ),
          diet_quality = dplyr::coalesce(diet_quality, "Medium")
        )
    )
  }
  if (is.null(feed_intake)) {
    cli::cli_abort(c(
      "{.arg method_diet} {.val {method_diet}} needs a feed-intake table.",
      i = "Pass it as {.code data$feed_intake}, or select
           {.val uniform_medium} to assume the IPCC {.val Medium} diet
           explicitly."
    ))
  }
  out <- .seed_supplied_diet(data)
  if (method_diet == "per_cell_feed") {
    cell_cols <- .cell_diet_group_cols()
    .check_diet_keys(out, cell_cols, method_diet)
    out <- .apply_diet_layer(
      out,
      .diet_from_intake(feed_intake, cell_cols),
      c(cell_cols, "item_cbs_code"),
      "per_cell_feed"
    )
  }
  nation_cols <- .national_diet_group_cols()
  .check_diet_keys(out, nation_cols, "national_feed")
  out <- .apply_diet_layer(
    out,
    .diet_from_intake(
      .aggregate_intake_to_nation(feed_intake, nation_cols),
      nation_cols
    ),
    c(nation_cols, "item_cbs_code"),
    "national_feed"
  )
  .check_diet_resolved(out, method_diet)
}

# Sum a cell-grain intake table up to the national grain, so one classifier
# serves both rungs of the ladder from one input.
.aggregate_intake_to_nation <- function(feed_intake, nation_cols) {
  tibble::as_tibble(feed_intake) |>
    dplyr::summarise(
      intake_dry_matter = sum(intake_dry_matter, na.rm = TRUE),
      .by = dplyr::all_of(c(nation_cols, "live_anim_code", "item_cbs_code"))
    )
}

# A diet the caller already supplied is the top rung: it is kept and labelled,
# never overwritten by a derived one.
.seed_supplied_diet <- function(data) {
  if (!rlang::has_name(data, "diet_quality")) {
    return(dplyr::mutate(
      data,
      diet_quality = NA_character_,
      method_diet = NA_character_
    ))
  }
  dplyr::mutate(
    data,
    method_diet = dplyr::if_else(
      is.na(diet_quality),
      NA_character_,
      "supplied"
    )
  )
}

# The herd table must carry the keys a rung joins on, or that rung would match
# nothing and quietly hand every row to the next one down.
.check_diet_keys <- function(data, cols, label) {
  missing <- setdiff(c(cols, "item_cbs_code"), names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(
      "The {.val {label}} diet needs column{?s} {.field {missing}}."
    )
  }
  n_na <- sum(is.na(data$item_cbs_code))
  if (n_na > 0L) {
    cli::cli_abort(c(
      "{n_na} row{?s} {?has/have} no {.field item_cbs_code}, so {?its/their}
       diet cannot be keyed to a consuming animal.",
      i = "Supply {.field item_cbs_code}, or use a {.field species} label that
           appears in {.field animals_codes}."
    ))
  }
  invisible(NULL)
}

# Fill only the rows still without a diet, stamping the rung that filled them.
.apply_diet_layer <- function(data, diet, keys, label) {
  data |>
    dplyr::left_join(
      dplyr::select(diet, dplyr::all_of(keys), layer_diet = diet_quality),
      by = keys
    ) |>
    dplyr::mutate(
      method_diet = dplyr::if_else(
        is.na(diet_quality) & !is.na(layer_diet),
        label,
        method_diet
      ),
      diet_quality = dplyr::coalesce(diet_quality, layer_diet)
    ) |>
    dplyr::select(-layer_diet)
}

# Fail closed on rows no rung of the ladder resolved.
.check_diet_resolved <- function(data, method_diet) {
  n_missing <- sum(is.na(data$diet_quality))
  if (n_missing == 0L) {
    return(data)
  }
  cli::cli_abort(c(
    "{n_missing} row{?s} {?has/have} no diet under {.arg method_diet}
     {.val {method_diet}}.",
    i = "Their feed mix carries no feed with a digestible-energy anchor, so a
         diet class cannot be derived from it.",
    i = "Select {.val uniform_medium} to assume the IPCC {.val Medium} diet
         explicitly, or extend the feed-intake table to cover them."
  ))
}
# nolint end
