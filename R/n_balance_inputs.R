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
# item_cbs_code sentinel convention: terms that are fundamentally per-cell
# or per-land-use, not per-crop (deposition, urban, SOM mineralization), use
# NA_integer_, the SAME "no specific item" sentinel already used package-wide
# for non-crop rows (see item_cbs_code = NA_integer_ in
# R/feed_intake_redistribute.R and R/redistribute_feed.R), rather than
# inventing a new magic code.
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
#' Terms that are fundamentally per-cell or per-land-use rather than
#' per-crop (`"deposition"`, `"urban"`, `"som_mineralization"`) carry
#' `item_cbs_code = NA_integer_`, the same "no specific item" sentinel
#' already used package-wide for non-crop rows (e.g.
#' [redistribute_feed()]'s grass-border-grazing rows).
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year the assembled inputs cover.
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
#'   * `urban_population`, `cropland_ha`, `cell_polity`: [build_urban_n()]'s
#'     inputs.
#'   * `carbon_balance`: [build_carbon_balance()]'s `"grid"`-resolution
#'     output (`lon`, `lat`, `area_code`, `land_use`, `year`, `area_ha`,
#'     `son_change_kgn_ha`); this driver requires it supplied directly, it
#'     is never computed here.
#'   * `primary_prod`, `fertilizer`, `crop_patterns`, `type_cropland`,
#'     `cell_polity`: the synthetic-fertiliser assembly (country total from
#'     `fertilizer`, the `faostat-fertilizer-nutrients` pin, split to crops
#'     by `primary_prod` harvested-area shares, then to cells by
#'     `crop_patterns`/`type_cropland`).
#'   * `gridded`, `resolution` (of the manure engine, default `"national"`),
#'     `methods`: forwarded to [build_livestock_nutrient_flows()].
#' @param example If `TRUE`, return a small fixture instead of assembling
#'   real data. Defaults to `FALSE`.
#' @return A tibble. At `resolution = "grid"`: `lon`, `lat`, `area_code`,
#'   `item_cbs_code`, `year`, `fert_type`, `n_input_t`. At
#'   `resolution = "polity"`: `area_code`, `item_cbs_code`, `year`,
#'   `fert_type`, `n_input_t` (summed over cells).
#' @export
#' @examples
#' build_n_inputs(example = TRUE)
build_n_inputs <- function(
  years = NULL,
  resolution = c("grid", "polity"),
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_n_inputs())
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
    .ni_filter_years(years) |>
    .ni_resolve(resolution)
}

# ---- Private helpers: schema + resolution ------------------------------

# Common output schema every source helper must produce.
.ni_schema <- function() {
  c(
    "lon",
    "lat",
    "area_code",
    "item_cbs_code",
    "year",
    "fert_type",
    "n_input_t"
  )
}

.ni_filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% years)
}

.ni_resolve <- function(x, resolution) {
  if (resolution == "grid") {
    return(dplyr::select(x, dplyr::all_of(.ni_schema())))
  }
  x |>
    dplyr::summarise(
      n_input_t = sum(.data$n_input_t, na.rm = TRUE),
      .by = c("area_code", "item_cbs_code", "year", "fert_type")
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
  if (is.null(data$npp_n_input)) {
    return(.ni_empty())
  }
  npp <- calculate_npp_carbon_nitrogen(data$npp_n_input)
  residue_soil_n <- if (rlang::has_name(npp, "residue_soil_n_t")) {
    npp$residue_soil_n_t
  } else {
    npp$residue_n_t
  }
  dplyr::transmute(
    npp,
    lon = .data$lon,
    lat = .data$lat,
    area_code = .data$area_code,
    item_cbs_code = .data$item_cbs_code,
    year = .data$year,
    fert_type = "recycling",
    n_input_t = .data$root_n_t + residue_soil_n
  )
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
      area_code = .as_integer_quiet(.data$territory),
      item_cbs_code = .ni_manure_item_cbs(.data$crop, .data$land_use),
      fert_type = .ni_manure_fert_type(.data$manure_type)
    ) |>
    dplyr::filter(.data$land_use %in% c("Cropland", "Grassland")) |>
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
# grassland-spilled manure is not attributed to any single crop): map it to
# item_cbs_code 3000L, the grass sentinel used package-wide. A real crop
# name (Cropland rows) resolves via the same item_prod-name crosswalk as
# .ni_item_cbs_from_prod(), matched case-insensitively since the manure
# engine's `crop` values are free-form lowercase strings (e.g. "barley")
# while whep::items_prod_full$item_prod is title-case ("Barley").
.ni_manure_item_cbs <- function(crop, land_use) {
  dplyr::if_else(
    land_use == "Grassland" | is.na(crop),
    3000L,
    .ni_crop_name_to_item_cbs(crop)
  )
}

.ni_crop_name_to_item_cbs <- function(crop) {
  lookup <- whep::items_prod_full |>
    dplyr::transmute(
      crop_lower = stringr::str_to_lower(.data$item_prod),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::distinct(.data$crop_lower, .keep_all = TRUE)
  tibble::tibble(crop_lower = stringr::str_to_lower(crop)) |>
    dplyr::left_join(lookup, by = "crop_lower") |>
    dplyr::pull("item_cbs_code")
}

.ni_manure_fert_type <- function(manure_type) {
  dplyr::case_match(
    manure_type,
    "Excreta" ~ "excreta",
    "Solid" ~ "manure_solid",
    "Liquid" ~ "manure_liquid",
    .default = "excreta"
  )
}

# ---- 4. Atmospheric deposition (cell-level, not crop-specific) -----------

.n_inputs_deposition <- function(data) {
  if (is.null(data$cell_polity)) {
    return(.ni_empty())
  }
  build_n_deposition(
    data = list(nhx = data$nhx, noy = data$noy, cell_polity = data$cell_polity)
  ) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_cbs_code = NA_integer_,
      year = .data$year,
      fert_type = "deposition",
      n_input_t = .data$deposition_n_t
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
      # transport reuse needs a character territory key); cast back to
      # integer here so it matches every other source's area_code type
      # (the cell_polity crosswalk's own, e.g. the FAOSTAT Area Code).
      area_code = .as_integer_quiet(.data$area_code),
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

# ---- 7. Synthetic fertiliser (country total -> crop -> grid) -------------

.n_inputs_synthetic <- function(data) {
  if (is.null(data$primary_prod) || is.null(data$fertilizer)) {
    return(.ni_empty())
  }
  country_totals <- .synthetic_n_country(data$fertilizer) |>
    dplyr::transmute(.data$year, .data$area_code, n_t = .data$synthetic_n_t)
  crop_shares <- .n_crop_area_shares(data$primary_prod)
  if (is.null(data$cell_polity)) {
    return(.ni_synthetic_polity(country_totals, crop_shares))
  }
  .ni_synthetic_grid(country_totals, crop_shares, data)
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
    n_input_t = double()
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
  )
}
