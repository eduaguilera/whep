# Gridded nitrogen INPUT assembly (Module C, Task C6, final phase).
#
# Combines every N-input source already built elsewhere in the package into
# one long-format tibble keyed by (lon, lat, area_code, item_cbs_code, year,
# fert_type). Each fert_type's heavy upstream computation (BNF, crop NPP,
# livestock nutrient flows, atmospheric deposition, urban N, the SOC/SON
# balance) is run by its own dedicated function elsewhere in the package;
# this file only assembles their outputs into the common schema, plus the
# ONE genuinely new assembly for synthetic fertiliser (country total ->
# crop-spatialized via spatialize_country_n_to_crops(), R/n_balance_
# spatialize.R).
#
# Non-item inputs are allocated only over an explicit agricultural land-support
# table. Deposition is multiplied by agricultural hectares rather than whole-cell
# area, so forest/natural deposition never enters the agricultural balance.
# Urban N, SOM mineralization, and manure already assigned upstream to Cropland
# but lacking a crop are apportioned only across local cropland items. Cropland
# support carries crop CBS items; all pasture/rangeland support uses CBS 3000
# without claiming a hard intensive/extensive historical split.
#
# accum_loss (perennial-crop standing-biomass N accumulation/decumulation,
# from Spain_Hist's N_balance.R) is a DOCUMENTED GAP: its source computation
# was not available for this task. It is listed in the fert_type vocabulary
# below but never emitted -- do not guess a formula.

#' Assemble gridded nitrogen inputs from every WHEP N-input source.
#'
#' @description
#' Combines biological nitrogen fixation ([calculate_bnf()]), residue/root N
#' recycling ([calculate_npp_carbon_nitrogen()]), livestock manure
#' ([build_livestock_nutrient_flows()]), atmospheric deposition
#' ([build_n_deposition()]), urban/human-excreta N ([build_urban_n()]), soil
#' organic-matter mineralization ([build_carbon_balance()]'s
#' `son_change_kgn_ha`) and synthetic fertiliser (a country total
#' spatialized to crops and cells via [spatialize_country_n_to_crops()])
#' into one long-format tibble of nitrogen inputs to agricultural land.
#'
#' `fert_type` values: `"bnf"`, `"recycling"`, `"manure_solid"`,
#' `"manure_liquid"`, `"excreta"`, `"deposition"`, `"urban"`,
#' `"som_mineralization"`, `"synthetic"` and `"accum_loss"`. The last is a
#' documented gap (perennial-crop standing-biomass N accumulation from
#' Spain_Hist's N_balance.R): its source computation was not available for
#' this task, so it is never emitted, only reserved in the vocabulary.
#'
#' Terms that are fundamentally per-cell or per-land-use rather than per-crop
#' are allocated over `data$ag_land_support`. Deposition uses both cropland and
#' grassland support. `"urban"`, `"som_mineralization"`, and manure already
#' assigned upstream to Cropland but lacking a crop use only local cropland
#' support, so manure is not reassigned to grassland after the manure engine's
#' capacity allocation. Forest and natural land are outside that support and
#' therefore outside the agricultural balance. Grassland is represented by CBS
#' 3000; no intensive/extensive class is inferred.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year the assembled inputs cover.
#' @param synthetic_method Synthetic-N crop allocation method, `"coello"` or
#'   `"area_share"`. When `NULL` (default), uses
#'   `data$synthetic_method %||% "coello"` for backwards compatibility.
#' @param resolution `"grid"` (default, per cell/crop/year/fert_type) or
#'   `"polity"` (summed to `area_code`/`item_cbs_code`/`year`/`fert_type`).
#' @param data Named list of pre-loaded, caller-supplied upstream inputs.
#'   Each of the following is required for its corresponding `fert_type` to
#'   be emitted (a missing one silently skips that source rather than
#'   erroring, since callers may only want a subset):
#'   * `bnf_input`: [calculate_bnf()]'s required input tibble (`lon`, `lat`,
#'     `area_code`, `year`, `item_prod_code`, `crop_npp_n_t`, `product_n_t`,
#'     `weed_npp_n_t`, `land_use`, `legumes_seeded`,
#'     `seeded_cover_crop_share`, `area_ha`).
#'   * `npp_n_input`: [calculate_npp_carbon_nitrogen()]'s required input
#'     tibble (`lon`, `lat`, `area_code`, `year`, `item_prod_code`,
#'     `item_cbs_code`, `product_dm_t`, `residue_dm_t`, `root_dm_t`,
#'     optionally `residue_soil_dm_t`).
#'   * `livestock_intake`: [build_livestock_nutrient_flows()]'s `intake`
#'     argument (the [redistribute_feed()] realised-intake contract), plus
#'     `gridded` (its land-surface layer) and `resolution`/`methods`
#'     (forwarded as-is).
#'   * `nhx`, `noy`, `cell_polity`: [build_n_deposition()]'s inputs.
#'   * `ag_land_support`: agricultural physical land support keyed by `lon`,
#'     `lat`, `area_code`, `year`, `item_cbs_code`, with `land_use`
#'     (`"cropland"` or `"grassland"`) and positive `area_ha`. Required when
#'     deposition or another non-item input is present. Cropland rows identify
#'     crop CBS items; all pasture/rangeland rows use CBS 3000.
#'   * `urban_population`, `cropland_ha`, `cell_polity`: [build_urban_n()]'s
#'     inputs.
#'   * `carbon_balance`: [build_carbon_balance()]'s `"grid"`-resolution
#'     output (`lon`, `lat`, `area_code`, `land_use`, `year`, `area_ha`,
#'     `son_change_kgn_ha`); this driver requires it supplied directly, it
#'     is never computed here.
#'   * `primary_prod`, `fertilizer`, `crop_patterns`, `type_cropland`,
#'     `cell_polity`: the synthetic-fertiliser assembly (country total from
#'     `fertilizer`, the `faostat-fertilizer-nutrients` pin, split to crops
#'     by the chosen crop-share method, then to cells by
#'     `crop_patterns`/`type_cropland`).
#'   * `synthetic_method`: how the synthetic-N country total is split across
#'     crops, `"coello"` (default; Coello 2025 rate-weighted, FAOSTAT-
#'     conserving) or `"area_share"` (harvested-area shares only).
#'   * `coello_rates`: crop-specific synthetic-N rate table shaped like
#'     [coello_synthetic_n] (`year`, `area_code`, `item_cbs_code`,
#'     `kg_n_ha`); defaults to `whep::coello_synthetic_n`. Used only when
#'     `synthetic_method = "coello"`.
#'   * `gridded`, `resolution` (of the manure engine, default `"national"`),
#'     `methods`: forwarded to [build_livestock_nutrient_flows()].
#' @param example If `TRUE`, return a small fixture instead of assembling
#'   real data. Defaults to `FALSE`.
#' @return A tibble. At `resolution = "grid"`: `lon`, `lat`, `area_code`,
#'   `item_cbs_code`, `year`, `fert_type`, `n_input_t`,
#'   `method_recycling_n`, `method_synthetic`. At `resolution = "polity"`:
#'   `area_code`, `item_cbs_code`, `year`, `fert_type`, `method_recycling_n`,
#'   `method_synthetic`, `n_input_t` (summed over cells).
#'   `method_recycling_n` records which residue basis the `"recycling"` term
#'   used: `"residue_soil_returned"` when the upstream NPP input supplied
#'   `residue_soil_dm_t` (residue N net of removal for feed/fuel/burning) or
#'   `"total_residue"` when only gross residue N was available; it is `NA` for
#'   every other `fert_type`. `method_synthetic` records the synthetic
#'   crop-split basis (`"coello"` or `"area_share"`) on `"synthetic"` rows and
#'   is `NA` for every other `fert_type`.
#' @export
#' @examples
#' build_n_inputs(example = TRUE)
build_n_inputs <- function(
  years = NULL,
  resolution = c("grid", "polity"),
  synthetic_method = NULL,
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_n_inputs())
  }
  if (!is.null(synthetic_method)) {
    data$synthetic_method <- rlang::arg_match(
      synthetic_method,
      c("coello", "area_share")
    )
  }
  data$.n_input_resolution <- resolution
  if (
    resolution == "grid" ||
      !is.null(data$ag_land_support)
  ) {
    data$resolution <- "subnational"
  }
  assembled <- dplyr::bind_rows(
    .n_inputs_bnf(data),
    .n_inputs_recycling(data),
    .n_inputs_manure(data),
    .n_inputs_deposition(data),
    .n_inputs_urban(data),
    .n_inputs_som(data),
    .n_inputs_synthetic(data)
  )
  assembled |>
    .ni_allocate_unattributed(data) |>
    .ni_filter_years(years) |>
    .ni_validate_resolution(resolution) |>
    .ni_resolve(resolution)
}

# ---- Private helpers: schema + resolution ------------------------------

# Common output schema every source helper must produce. `method_recycling_n`
# records which residue basis the "recycling" term used (soil-returned vs
# total residue N); it is NA for every other fert_type.
.ni_schema <- function() {
  c(
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "fert_type",
    "n_input_t",
    "method_recycling_n",
    "method_synthetic"
  )
}

.ni_filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% years)
}

.ni_validate_resolution <- function(x, resolution) {
  if (
    resolution == "grid" &&
      any(
        is.na(x$lon) |
          is.na(x$lat) |
          is.na(x$area_code) |
          is.na(x$year)
      )
  ) {
    cli::cli_abort(
      "Grid-resolution nitrogen inputs contain missing spatial keys."
    )
  }
  x
}

.ni_resolve <- function(x, resolution) {
  if (resolution == "grid") {
    return(dplyr::select(x, dplyr::all_of(.ni_schema())))
  }
  x |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t, na.rm = TRUE),
      .by = c(
        "area_code",
        "item_cbs_code",
        "year",
        "fert_type",
        "method_recycling_n",
        "method_synthetic"
      )
    )
}

# ---- 1. BNF --------------------------------------------------------------

.n_inputs_bnf <- function(data) {
  if (is.null(data$bnf_input)) {
    return(.ni_empty())
  }
  data$bnf_input |>
    calculate_bnf() |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_cbs_code = .ni_item_cbs_from_prod(.data$item_prod_code),
      year = .data$year,
      fert_type = "bnf",
      n_input_t = .data$bnf_t
    )
}

# ---- 2. Recycling (residue + root N returned to soil) --------------------

.n_inputs_recycling <- function(data) {
  npp <- .n_balance_npp(data)
  if (is.null(npp)) {
    return(.ni_empty())
  }
  basis <- .ni_recycling_basis(npp)
  dplyr::transmute(
    npp,
    lon = .data$lon,
    lat = .data$lat,
    area_code = .data$area_code,
    item_cbs_code = .data$item_cbs_code,
    year = .data$year,
    fert_type = "recycling",
    n_input_t = .data$root_n_t + .data[[basis$column]],
    method_recycling_n = basis$method
  )
}

# The recycling term's residue-N basis depends on whether the upstream NPP
# input carried `residue_soil_dm_t` (so calculate_npp_carbon_nitrogen() emitted
# `residue_soil_n_t`, the residue N actually returned to soil after removal for
# feed/fuel/burning). When it did not, only gross `residue_n_t` (the FULL
# residue N) is available and is used instead, overstating the soil return.
# This basis switch is not a silent fallback: it is stamped in
# `method_recycling_n` so downstream consumers can tell the two apart.
.ni_recycling_basis <- function(npp) {
  if (rlang::has_name(npp, "residue_soil_n_t")) {
    return(list(
      column = "residue_soil_n_t",
      method = "residue_soil_returned"
    ))
  }
  list(column = "residue_n_t", method = "total_residue")
}

# Shared NPP-N computation: build_nitrogen_balance() (Task C7) needs the SAME
# calculate_npp_carbon_nitrogen() result for prod_n_t/residue N, so this is
# exposed as a package-internal helper rather than letting each caller
# invoke calculate_npp_carbon_nitrogen(data$npp_n_input) separately. Returns
# NULL (not an empty tibble) when data$npp_n_input is absent, so callers can
# distinguish "not computed" from "computed but empty".
#
# data$.npp_cache lets a caller that needs this result more than once within
# a single top-level call (build_nitrogen_balance() calls this both via
# build_n_inputs()'s "recycling" term and its own prod_n_t term) compute it
# ONCE and short-circuit every subsequent call, rather than re-running
# calculate_npp_carbon_nitrogen() per call site.
.n_balance_npp <- function(data) {
  if (!is.null(data$.npp_cache)) {
    return(data$.npp_cache)
  }
  if (is.null(data$npp_n_input)) {
    return(NULL)
  }
  calculate_npp_carbon_nitrogen(data$npp_n_input)
}

# item_cbs_code from item_prod_code via the same crosswalk used elsewhere
# (whep::items_prod_full).
.ni_item_cbs_from_prod <- function(item_prod_code) {
  lookup <- whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = as.character(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::distinct()
  tibble::tibble(item_prod_code = as.character(item_prod_code)) |>
    dplyr::left_join(lookup, by = "item_prod_code") |>
    dplyr::pull("item_cbs_code")
}

# ---- 3. Manure (solid / liquid / excreta) ---------------------------------

.n_inputs_manure <- function(data) {
  if (is.null(data$livestock_intake)) {
    return(.ni_empty())
  }
  flows <- build_livestock_nutrient_flows(
    data$livestock_intake,
    resolution = data$resolution %||% "national",
    methods = data$methods %||% list(),
    gridded = data$gridded
  )
  .manure_to_n_inputs(flows$applied)
}

# Map build_livestock_nutrient_flows()'s $applied grain (territory,
# sub_territory, crop, manure_type) to this function's schema. Grassland
# rows (crop = NA) use item_cbs_code 3000L, the package-wide grass code (see
# build_production.R / grassland_land_extension.R); a missing sub_territory
# (national/global resolution, no cell grain) drops lon/lat to NA -- those
# rows are only usable at resolution = "polity".
.manure_to_n_inputs <- function(applied) {
  coords <- .ni_manure_coords(applied$sub_territory)
  applied |>
    dplyr::mutate(
      lon = coords$lon,
      lat = coords$lat,
      area_code = .manure_territory_to_area_code(.data$territory),
      item_cbs_code = .ni_manure_item_cbs(.data$crop, .data$land_use),
      fert_type = .ni_manure_fert_type(.data$manure_type)
    ) |>
    dplyr::filter(
      .data$land_use %in% c("Cropland", "Grassland", "transported")
    ) |>
    dplyr::summarise(
      n_input_t = sum(.data$applied_n, na.rm = TRUE),
      .by = c(
        "lon",
        "lat",
        "area_code",
        "item_cbs_code",
        "year",
        "fert_type"
      )
    )
}

# build_livestock_nutrient_flows()'s $applied$territory carries either a
# stringified area_code (the real pipeline's own convention, e.g.
# feed_intake_redistribute.R:805 territory = as.character(area_code)) or an
# ISO3 code (the function's own roxygen @examples and test fixtures use
# territory = "ESP"). Resolve both rather than assuming one and silently
# NA-ing the other: try integer parsing first, then fall back to an
# iso3c -> area_code lookup via whep::regions_full; abort on anything that
# resolves to neither, rather than propagating NA into area_code.
.manure_territory_to_area_code <- function(territory) {
  as_int <- suppressWarnings(as.integer(territory))
  still_missing <- is.na(as_int) & !is.na(territory)
  if (any(still_missing)) {
    iso3_lookup <- whep::regions_full |>
      dplyr::filter(!is.na(.data$iso3c)) |>
      dplyr::distinct(.data$iso3c, .data$code)
    resolved <- tibble::tibble(iso3c = territory[still_missing]) |>
      dplyr::left_join(iso3_lookup, by = "iso3c") |>
      dplyr::pull("code")
    as_int[still_missing] <- resolved
  }
  unresolved <- unique(territory[is.na(as_int) & !is.na(territory)])
  if (length(unresolved) > 0) {
    cli::cli_abort(c(
      "Could not resolve {.field territory} to an {.field area_code}.",
      i = "Unrecognised value{?s}: {.val {unresolved}}. Expected a
           stringified area_code or a known {.field iso3c} in
           {.code whep::regions_full}."
    ))
  }
  as_int
}

.ni_manure_coords <- function(sub_territory) {
  if (all(is.na(sub_territory))) {
    return(list(
      lon = rep(NA_real_, length(sub_territory)),
      lat = rep(NA_real_, length(sub_territory))
    ))
  }
  .parse_cell_id(sub_territory)
}

# Grassland's crop is always NA in the manure engine's grain (grazing/
# grassland-spilled manure is not attributed to any single crop): map those
# rows to item_cbs_code 3000L, the grass sentinel used package-wide. Every
# other row whose crop is NA keeps the ordinary NA_integer_ no-specific-item
# sentinel: a Cropland residual (e.g. an over_apply_local remainder) and a
# transport landing (pooled across crop and grass capacity with no crop)
# alike. The is.na(crop) branch is checked before the crosswalk default so a
# missing crop never reaches .ni_crop_name_to_item_cbs(). A real crop name
# resolves via the item_prod crosswalk.
.ni_manure_item_cbs <- function(crop, land_use) {
  resolved <- .ni_crop_name_to_item_cbs(crop)
  dplyr::case_when(
    land_use == "Grassland" ~ 3000L,
    is.na(crop) ~ NA_integer_,
    .default = resolved
  )
}

# Cropland `crop` names are free-form lowercase strings from the manure
# engine; resolve each to item_cbs_code via the case-folded item_prod
# crosswalk. A non-NA crop name that matches nothing is a genuine mapping gap
# (a renamed or free-form crop the crosswalk does not know), so abort naming it
# rather than emit an NA item_cbs_code indistinguishable from the deliberately
# non-crop-specific deposition/urban/SOM rows, mirroring
# .manure_territory_to_area_code()'s treatment of unresolvable territories. An
# NA crop never reaches this abort: .ni_manure_item_cbs() assigns either the
# grass code or the no-specific-item sentinel from land_use.
.ni_crop_name_to_item_cbs <- function(crop) {
  lookup <- whep::items_prod_full |>
    dplyr::transmute(
      crop_lower = stringr::str_to_lower(.data$item_prod),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::distinct(.data$crop_lower, .keep_all = TRUE)
  resolved <- tibble::tibble(crop_lower = stringr::str_to_lower(crop)) |>
    dplyr::left_join(lookup, by = "crop_lower", na_matches = "never")
  unresolved <- unique(
    crop[!is.na(resolved$crop_lower) & is.na(resolved$item_cbs_code)]
  )
  if (length(unresolved) > 0) {
    cli::cli_abort(c(
      "Could not resolve manure {.field crop} to an {.field item_cbs_code}.",
      i = "Unrecognised value{?s}: {.val {unresolved}}. Expected a crop name
           matching {.field item_prod} in {.code whep::items_prod_full}
           (matched case-insensitively)."
    ))
  }
  resolved$item_cbs_code
}

.ni_manure_fert_type <- function(manure_type) {
  unexpected <- setdiff(unique(manure_type), c("Excreta", "Solid", "Liquid"))
  if (length(unexpected) > 0) {
    cli::cli_abort(c(
      "Unexpected {.field manure_type} value{?s}: {.val {unexpected}}.",
      i = "Expected one of {.val Excreta}, {.val Solid} or {.val Liquid}."
    ))
  }
  dplyr::case_match(
    manure_type,
    "Excreta" ~ "excreta",
    "Solid" ~ "manure_solid",
    "Liquid" ~ "manure_liquid"
  )
}

# ---- 4. Atmospheric deposition (agricultural land support only) -----------

.n_inputs_deposition <- function(data) {
  if (is.null(data$cell_polity)) {
    return(.ni_empty())
  }
  support <- .ni_land_support(data)
  deposition <- build_n_deposition(
    data = list(nhx = data$nhx, noy = data$noy, cell_polity = data$cell_polity)
  ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "deposition_kgn_ha"
    )
  dplyr::inner_join(
    support,
    deposition,
    by = c("lon", "lat", "area_code", "year"),
    relationship = "many-to-one"
  ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_cbs_code = .data$item_cbs_code,
      year = .data$year,
      fert_type = "deposition",
      n_input_t = .data$deposition_kgn_ha * .data$area_ha / 1000
    )
}

# ---- 5. Urban N (cell-level, not crop-specific) ---------------------------

.n_inputs_urban <- function(data) {
  if (is.null(data$urban_population) || is.null(data$cropland_ha)) {
    return(.ni_empty())
  }
  build_urban_n(
    data = list(
      urban_population = data$urban_population,
      cell_polity = data$cell_polity,
      cropland_ha = data$cropland_ha
    )
  ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      # build_urban_n() stringifies area_code internally (its manure-
      # transport reuse needs a character territory key). Resolve either its
      # normal numeric-string code or an ISO3 fixture/input through the same
      # checked resolver as the manure path; never turn an ISO3 into silent NA.
      area_code = .manure_territory_to_area_code(.data$area_code),
      item_cbs_code = NA_integer_,
      year = .data$year,
      fert_type = "urban",
      n_input_t = .data$urban_n_t
    )
}

# ---- 6. SOM mineralization (positive son_change_kgn_ha only) -------------

# Simple sentinel approach (not crop-weighted): SOM mineralization is a
# per-land-use flux, not per-crop, in build_carbon_balance() itself, so it
# is assigned the same NA_integer_ "not crop-specific" code as deposition
# and urban rather than area-weight-split across the cell's actual crops.
# A crop-level split via spatialize_country_n_to_crops()'s crop-pattern
# weights is a defensible future refinement, not required by this task.
.n_inputs_som <- function(data) {
  if (is.null(data$carbon_balance)) {
    return(.ni_empty())
  }
  data$carbon_balance |>
    dplyr::filter(
      stringr::str_to_lower(.data$land_use) == "cropland",
      .data$son_change_kgn_ha > 0
    ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_cbs_code = NA_integer_,
      year = .data$year,
      fert_type = "som_mineralization",
      n_input_t = .data$son_change_kgn_ha * .data$area_ha / 1000
    )
}

# ---- Allocate non-item agricultural inputs over explicit support ------------

.ni_land_support <- function(data) {
  support <- data$ag_land_support
  if (is.null(support)) {
    cli::cli_abort(c(
      "Non-item nitrogen inputs require explicit agricultural land support.",
      i = "Supply {.field data$ag_land_support} with cell/polity/year/item,
           {.field land_use}, and physical {.field area_ha}."
    ))
  }
  .check_columns(
    support,
    c(
      "lon",
      "lat",
      "area_code",
      "item_cbs_code",
      "year",
      "land_use",
      "area_ha"
    ),
    "ag_land_support"
  )
  out <- support |>
    dplyr::mutate(
      land_use = stringr::str_to_lower(.data$land_use),
      item_cbs_code = as.integer(.data$item_cbs_code)
    )
  invalid_land <- setdiff(unique(out$land_use), c("cropland", "grassland"))
  if (length(invalid_land) > 0L) {
    cli::cli_abort(c(
      "{.field ag_land_support$land_use} is agricultural support only.",
      i = "Unexpected value{?s}: {.val {invalid_land}}."
    ))
  }
  invalid_grass <- dplyr::filter(
    out,
    .data$land_use == "grassland",
    .data$item_cbs_code != 3000L
  )
  if (nrow(invalid_grass) > 0L) {
    cli::cli_abort(
      "Grassland support must use {.field item_cbs_code = 3000}."
    )
  }
  out |>
    dplyr::filter(
      is.finite(.data$area_ha),
      .data$area_ha > 0,
      !is.na(.data$item_cbs_code)
    ) |>
    dplyr::summarise(
      area_ha = sum(.data$area_ha),
      .by = c(
        "lon",
        "lat",
        "area_code",
        "item_cbs_code",
        "year",
        "land_use"
      )
    )
}

.ni_allocate_unattributed <- function(inputs, data) {
  unattributed <- dplyr::filter(inputs, is.na(.data$item_cbs_code))
  if (nrow(unattributed) == 0L) {
    return(inputs)
  }
  support <- .ni_land_support(data)
  allocated <- unattributed |>
    dplyr::select(-"item_cbs_code") |>
    dplyr::mutate(.source_row = dplyr::row_number()) |>
    dplyr::inner_join(
      dplyr::filter(support, .data$land_use == "cropland"),
      by = c("lon", "lat", "area_code", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      support_ha = sum(.data$area_ha),
      .by = ".source_row"
    ) |>
    dplyr::filter(.data$support_ha > 0) |>
    dplyr::mutate(
      n_input_t = .data$n_input_t * .data$area_ha / .data$support_ha
    ) |>
    dplyr::select(dplyr::all_of(.ni_schema()))
  source_mass <- sum(unattributed$n_input_t, na.rm = TRUE)
  allocated_mass <- sum(allocated$n_input_t, na.rm = TRUE)
  if (!isTRUE(all.equal(source_mass, allocated_mass, tolerance = 1e-8))) {
    cli::cli_abort(c(
      "Could not allocate all non-item nitrogen over agricultural support.",
      i = "Source: {source_mass} t N; allocated: {allocated_mass} t N."
    ))
  }
  dplyr::bind_rows(
    dplyr::filter(inputs, !is.na(.data$item_cbs_code)),
    allocated
  )
}

# ---- 7. Synthetic fertiliser (country total -> crop -> grid) -------------

.n_inputs_synthetic <- function(data) {
  if (is.null(data$primary_prod) || is.null(data$fertilizer)) {
    return(.ni_empty())
  }
  country_totals <- .synthetic_n_country(data$fertilizer) |>
    dplyr::transmute(.data$year, .data$area_code, n_t = .data$synthetic_n_t)
  crop_shares <- .n_synthetic_crop_shares(
    data$primary_prod,
    data$synthetic_method %||% "coello",
    data$coello_rates
  )
  spatialized <- if (is.null(data$cell_polity)) {
    .ni_synthetic_polity(country_totals, crop_shares)
  } else {
    .ni_synthetic_grid(country_totals, crop_shares, data)
  }
  .ni_attach_synthetic_method(spatialized, crop_shares)
}

# method_synthetic is uniform within a (year, area_code) group (the coello vs
# area_share choice is per country-year), so join it back by (year, area_code)
# after spatialization, which drops non-key columns.
.ni_attach_synthetic_method <- function(spatialized, crop_shares) {
  lookup <- dplyr::distinct(
    crop_shares,
    .data$year,
    .data$area_code,
    .data$method_synthetic
  )
  dplyr::left_join(spatialized, lookup, by = c("year", "area_code"))
}

.ni_synthetic_polity <- function(country_totals, crop_shares) {
  spatialize_country_n_to_crops(
    country_totals,
    crop_shares,
    cell_polity = NULL,
    resolution = "polity_crop"
  ) |>
    dplyr::transmute(
      lon = NA_real_,
      lat = NA_real_,
      area_code = .data$area_code,
      item_cbs_code = .data$item_cbs_code,
      year = .data$year,
      fert_type = "synthetic",
      n_input_t = .data$n_t
    )
}

.ni_synthetic_grid <- function(country_totals, crop_shares, data) {
  spatialize_country_n_to_crops(
    country_totals,
    crop_shares,
    cell_polity = data$cell_polity,
    resolution = "grid",
    data = list(
      crop_patterns = data$crop_patterns,
      type_cropland = data$type_cropland
    )
  ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_cbs_code = .data$item_cbs_code,
      year = .data$year,
      fert_type = "synthetic",
      n_input_t = .data$n_t
    )
}

# ---- Shared helpers --------------------------------------------------------

.ni_empty <- function() {
  tibble::tibble(
    lon = double(),
    lat = double(),
    area_code = integer(),
    item_cbs_code = integer(),
    year = integer(),
    fert_type = character(),
    n_input_t = double(),
    method_recycling_n = character(),
    method_synthetic = character()
  )
}

# Toy fixture for a runnable example: a small multi-fert_type slice.
.example_n_inputs <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~fert_type,
    ~n_input_t,
    -0.25,
    -0.25,
    1L,
    2511L,
    2020L,
    "bnf",
    3.2,
    -0.25,
    -0.25,
    1L,
    2511L,
    2020L,
    "recycling",
    5.6,
    -0.25,
    -0.25,
    1L,
    2511L,
    2020L,
    "synthetic",
    12.4,
    -0.25,
    -0.25,
    1L,
    NA_integer_,
    2020L,
    "deposition",
    0.9,
    -0.25,
    -0.25,
    1L,
    NA_integer_,
    2020L,
    "urban",
    4.5,
    -0.25,
    -0.25,
    1L,
    NA_integer_,
    2020L,
    "som_mineralization",
    1.1,
    -0.25,
    -0.25,
    1L,
    3000L,
    2020L,
    "excreta",
    2.3,
    -0.25,
    -0.25,
    1L,
    2511L,
    2020L,
    "manure_solid",
    1.8,
    -0.25,
    -0.25,
    1L,
    2511L,
    2020L,
    "manure_liquid",
    0.7
  ) |>
    dplyr::mutate(
      method_recycling_n = dplyr::if_else(
        .data$fert_type == "recycling",
        "total_residue",
        NA_character_
      ),
      method_synthetic = dplyr::if_else(
        .data$fert_type == "synthetic",
        "coello",
        NA_character_
      )
    )
}
