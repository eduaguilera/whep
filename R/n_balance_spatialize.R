# Real cell-polity crosswalk assembly + polity-total-to-crop spatialization
# (Module C, Task C6).
#
# CONFIRMED FACTS (local files inspected; do not re-guess):
# - The cell-polity parquet at Sys.getenv("WHEP_POLITY_FRACTION_PATH") has
#   lon, lat, area_code, polity_frac already but no cell_area_ha; that is
#   added here from latitude via the SAME formula .cell_area_ha_lat() in
#   R/feed_lpjml.R (a private package helper, called directly, never
#   redefined).
# - Its size is NOT fixed. This comment used to assert 68,527 rows; that is
#   the pre-#381 vintage. The deployed file now measures 62,808 rows / 58,791
#   cells / 182 area codes, after the producer restricted the crosswalk to
#   the simulated grid. Do not re-assert a literal here.
# - crop_patterns.parquet (Sys.getenv("WHEP_CROP_PATTERNS_PATH")): lon, lat,
#   item_prod_code, harvest_fraction, a STATIC crop-pattern weight (no year
#   dimension), 2,247,239 rows.
# - type_cropland.parquet (Sys.getenv("WHEP_TYPE_CROPLAND_PATH")): lon, lat,
#   year, luh2_type, type_ha, type_irrig_ha, an annual per-cell cropland
#   area by LUH2 class, 27,496,275 rows.
# - Per-cell per-crop hectares = type_ha * harvest_fraction, the exact
#   formula already used by make_lpjml_covariate()'s "crop_pattern" weighting
#   branch (R/lpjml_covariate.R); reused verbatim here rather than
#   reimplemented, and summed across every luh2_type row for a cell-year
#   before multiplying (a cell can carry more than one LUH2 cropland class).
# - Both parquets are too large to read whole into a package-load context
#   (2.2M / 27.5M rows): the reader filters immediately after reading, before
#   any join, to the requested years/item_prod_codes.

#' Assemble WHEP's cell-polity crosswalk with true grid-cell area.
#'
#' @description
#' Reads the cached cell-polity fraction parquet (`lon`, `lat`, `area_code`,
#' `polity_frac`) and adds `cell_area_ha`, computed from latitude with the
#' same 0.5-degree cell-area formula used across the package (see
#' [build_grass_availability_lpjml()]). This assembles the
#' `data$cell_polity` contract that every Module C function (e.g.
#' [build_n_deposition()], [build_urban_n()], [get_soc_climate_drivers()])
#' expects as a required input.
#'
#' @section Which area code the grid is keyed on:
#' The parquet is rasterized from present-day polygons through
#' `inst/extdata/regions.csv`, so its `area_code` is a raw reporting-area code
#' and **not** necessarily a [polity_area_crosswalk] `polity_area_code`: the
#' bucket every polity-keyed national table in whep is aggregated on. Grid
#' codes that are not a bucket cannot join to national data at all: the join
#' is silently empty on both sides.
#'
#' `area_key` selects which of the two the output carries. It is not a
#' fallback: `"grid"` is the default, reproduces the parquet's own codes
#' bit-for-bit, and warns naming the codes that cannot resolve;
#' `"polity_area"` resolves each code to its bucket through
#' [polity_area_crosswalk] and re-sums `polity_frac` within
#' `(lon, lat, area_code)`, so a cell straddling two areas of the same bucket
#' stays one row per bucket and each cell's fractions still sum to 1. It
#' respects `options(whep.unfold_rest_of_world = TRUE)` (see
#' [folded_reporting_areas()]), so the grid and the national tables agree
#' about where a Rest-of-World member's rows belong.
#'
#' Under `"polity_area"` the raw reporting code is **carried, not replaced**:
#' the output gains `grid_area_code` holding the parquet's own code, joined with
#' `+` where a cell's areas collapse into one bucket. So the fold this performs
#' is recoverable at the join rather than baked into the grid — a derived key
#' silently overwriting the raw one it came from is what whep#582 reports from
#' the output side, and the same fold is what dropped Sudan's 40.8 M goats and
#' doubled its sugar cane in the published production series (whep#563).
#'
#' The output deliberately does **not** gain `polity_code` /
#' `reporting_polity_*`. A bucket is not a polity: `999` holds up to 17
#' territories at once and `206` holds Sudan and South Sudan together, so no
#' polity code string is recoverable from this year-less grid. Carrying that
#' identity needs the cell x polity x validity-interval unit tracked by epic
#' whep#458, not a column added here.
#'
#' @param polity_fraction_path Path to the cell-polity fraction parquet.
#'   Defaults to `Sys.getenv("WHEP_POLITY_FRACTION_PATH")`.
#' @param area_key Which area code the output is keyed on: `"grid"` (default,
#'   the parquet's own reporting-area codes) or `"polity_area"` (the
#'   [polity_area_crosswalk] bucket national tables are aggregated on).
#' @return A tibble with `lon`, `lat`, `area_code`, `polity_frac` and
#'   `cell_area_ha`.
#' @export
#' @examples
#' # Requires WHEP_POLITY_FRACTION_PATH to be set; not run without it.
#' if (nzchar(Sys.getenv("WHEP_POLITY_FRACTION_PATH"))) {
#'   build_cell_polity()
#' }
build_cell_polity <- function(
  polity_fraction_path = NULL,
  area_key = c("grid", "polity_area")
) {
  area_key <- rlang::arg_match(area_key)
  path <- .resolve_polity_fraction_path(polity_fraction_path)
  raw <- nanoparquet::read_parquet(path) |> tibble::as_tibble()
  .check_columns(
    raw,
    c("lon", "lat", "area_code", "polity_frac"),
    "cell_polity"
  )
  raw |>
    .cell_polity_apply_area_key(area_key) |>
    dplyr::mutate(cell_area_ha = .cell_area_ha_lat(.data$lat))
}

#' Spatialize a polity-level nitrogen total to crops and grid cells.
#'
#' @description
#' Promotes a single polity-total nitrogen input (one row per `year`,
#' `area_code`, for one fertiliser type) to the crop level by the same
#' harvested-area-share logic used by
#' [build_crop_soil_n2o_extension()] (`year`/`area_code`-matched, weighted by
#' each crop's share of harvested cropland area), then optionally further to
#' the grid level by distributing each polity-crop total across cells in
#' proportion to the cell's share of that polity-crop's total crop-pattern
#' area (`type_ha * harvest_fraction`, summed over LUH2 cropland classes;
#' the exact formula used by [make_lpjml_covariate()]'s `crop_pattern`
#' weighting). A polity-crop with no crop-pattern hectares (crop absent from
#' the static pattern raster, or its pattern area sums to zero in the polity)
#' is instead spread uniformly across the polity's cropland cells, weighted by
#' each cell's cropland area, so the grid output still re-aggregates to the
#' polity total. Such reallocations emit a warning naming the affected crops
#' and the reallocated nitrogen.
#'
#' @param country_totals A tibble with `year`, `area_code`, `n_t`: the
#'   polity-level nitrogen total for one fertiliser type.
#' @param crop_shares A tibble with `year`, `area_code`, `item_cbs_code`,
#'   `area_share`: harvested-area-weighted crop shares within each
#'   country-year, e.g. from [build_crop_soil_n2o_extension()]'s internal
#'   crop-area-share helper.
#' @param cell_polity The [build_cell_polity()]-shaped crosswalk (`lon`,
#'   `lat`, `area_code`, `polity_frac`, `cell_area_ha`). Only required when
#'   `resolution` includes `"grid"`.
#' @param resolution Which resolution(s) to return: `"polity_crop"` (default,
#'   `year`/`area_code`/`item_cbs_code` totals only) or `"grid"` (also
#'   distributes to `lon`/`lat` grid cells; requires `crop_patterns` and
#'   `type_cropland` in `data`).
#' @param data Optional named list of pre-loaded grid inputs, used only when
#'   `resolution = "grid"`: `crop_patterns` (`lon`, `lat`, `item_prod_code`,
#'   `harvest_fraction`) and `type_cropland` (`lon`, `lat`, `year`,
#'   `luh2_type`, `type_ha`), each falling back to a lazy parquet read from
#'   `Sys.getenv("WHEP_CROP_PATTERNS_PATH")` /
#'   `Sys.getenv("WHEP_TYPE_CROPLAND_PATH")` when absent. `item_cbs_code` in
#'   `crop_shares`/`country_totals` is matched to the `item_prod_code` column
#'   of `crop_patterns` via [whep::items_prod_full] (the same crosswalk
#'   [build_crop_land_extension()] uses).
#' @return A tibble. For `resolution = "polity_crop"`: `year`, `area_code`,
#'   `item_cbs_code`, `n_t`. For `resolution = "grid"`: `lon`, `lat`,
#'   `area_code`, `year`, `item_cbs_code`, `n_t`.
#' @export
#' @examples
#' spatialize_country_n_to_crops(
#'   country_totals = tibble::tribble(
#'     ~year, ~area_code, ~n_t,
#'     2010L, 10L, 100
#'   ),
#'   crop_shares = tibble::tribble(
#'     ~year, ~area_code, ~item_cbs_code, ~area_share,
#'     2010L, 10L, 2511L, 0.7,
#'     2010L, 10L, 2513L, 0.3
#'   ),
#'   cell_polity = NULL,
#'   resolution = "polity_crop"
#' )
spatialize_country_n_to_crops <- function(
  country_totals,
  crop_shares,
  cell_polity,
  resolution = c("polity_crop", "grid"),
  data = list()
) {
  resolution <- rlang::arg_match(resolution)
  .n_check_totals_shares(country_totals, crop_shares)
  polity_crop <- .n_polity_crop_totals(country_totals, crop_shares)
  if (resolution == "polity_crop") {
    return(polity_crop)
  }
  .check_columns(
    cell_polity,
    c("lon", "lat", "area_code", "polity_frac", "cell_area_ha"),
    "cell_polity"
  )
  .n_grid_totals(polity_crop, cell_polity, data)
}

# ---- Private helpers --------------------------------------------------

# Apply the requested area key. "grid" is today's behaviour, kept as the
# default because switching moves published values for every gridded consumer;
# it only gains the diagnostic that today's silence hides. The alternative is
# selected, never a fallback.
.cell_polity_apply_area_key <- function(raw, area_key) {
  if (area_key == "grid") {
    .warn_cell_polity_off_bucket(raw)
    return(raw)
  }
  .cell_polity_to_bucket(raw)
}

# area_code -> polity_area_code, one row per code. Read through
# .polity_crosswalk() rather than the shipped table so the unfold switch
# reaches the grid too; a code mapping to two buckets would be a crosswalk
# defect, so it aborts rather than picking a winner.
.cell_polity_bucket_lookup <- function() {
  lookup <- .polity_crosswalk() |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$polity_area_code)) |>
    dplyr::distinct(
      area_code = as.integer(.data$area_code),
      polity_area_code = as.integer(.data$polity_area_code)
    )
  ambiguous <- lookup$area_code[duplicated(lookup$area_code)]
  if (length(ambiguous) > 0) {
    codes <- sort(unique(ambiguous))
    n_ambiguous <- length(codes)
    cli::cli_abort(c(
      "{.field polity_area_crosswalk} maps {n_ambiguous} area
       code{?s} to more than one {.field polity_area_code}.",
      # qty() pinned, and the codes pulled into a variable: with the marker
      # ahead of them and nothing numeric before it, cli tries to read the
      # quantity off the INTEGER code vector and dies on
      # "length(object) == 1 is not TRUE" -- so the abort that is supposed to
      # name the offending codes instead reports a cli internal, exactly when
      # someone needs to know which codes. Same class as #618, see #621.
      x = "{cli::qty(n_ambiguous)}Ambiguous area code{?s}: {codes}."
    ))
  }
  lookup
}

# Reporting-area codes present in the grid that are not a polity_area_code.
# Their cells can never match a polity-keyed national table.
.cell_polity_off_bucket <- function(raw) {
  buckets <- unique(.cell_polity_bucket_lookup()$polity_area_code)
  sort(setdiff(unique(as.integer(raw$area_code)), buckets))
}

.warn_cell_polity_off_bucket <- function(raw) {
  codes <- .cell_polity_off_bucket(raw)
  if (length(codes) == 0) {
    return(invisible(raw))
  }
  affected <- raw[as.integer(raw$area_code) %in% codes, c("lon", "lat")]
  n_cells <- nrow(unique(affected))
  cli::cli_warn(c(
    "!" = "{length(codes)} grid area code{?s} covering {n_cells} cell{?s}
      cannot join to any polity-keyed national table: no
      {.field polity_area_code} carries them.",
    i = "Area code{cli::qty(length(codes))}{?s}: {codes}.",
    i = "Pass {.code area_key = \"polity_area\"} to key the grid on the
      buckets national tables are aggregated on."
  ))
  invisible(raw)
}

# Re-key the grid on polity_area_code. Border cells that held two areas of one
# bucket (Sudan/South Sudan) collapse to a single row, so polity_frac is
# re-summed within the cell; codes absent from the crosswalk keep their own
# code rather than being dropped, so a gap stays visible.
#
# THE RAW CODE IS CARRIED, NOT REPLACED. The bucket arrives as an added
# `grid_area_code` alongside the keyed `area_code`, so the fold this performs
# stays recoverable at the join instead of becoming irrecoverable in the grid.
# That asymmetry -- a derived key overwriting the raw one it was derived from --
# is what whep#582 reports from the output side, and doing it here would put the
# same ambiguity somewhere much harder to unwind: measured upstream, 41 cells /
# 12.18 Mha fold two distinct polity codes onto bucket 206, which is the same
# fold that dropped Sudan's 40.8 M goats and doubled its sugar cane in the
# published production series (whep#563, fixed in whep#591).
#
# Where a cell's areas collapse into one bucket the raw codes are joined with a
# separator rather than one of them being picked, because picking would be the
# silent half of the same problem.
.cell_polity_to_bucket <- function(raw) {
  raw |>
    dplyr::mutate(area_code = as.integer(.data$area_code)) |>
    dplyr::left_join(.cell_polity_bucket_lookup(), by = "area_code") |>
    dplyr::mutate(
      grid_area_code = .data$area_code,
      area_code = dplyr::coalesce(.data$polity_area_code, .data$area_code)
    ) |>
    dplyr::summarise(
      polity_frac = sum(.data$polity_frac),
      grid_area_code = paste(
        sort(unique(.data$grid_area_code)),
        collapse = "+"
      ),
      .by = c(lon, lat, area_code)
    )
}

# Validate the two required inputs' columns.
.n_check_totals_shares <- function(country_totals, crop_shares) {
  .check_columns(
    country_totals,
    c("year", "area_code", "n_t"),
    "country_totals"
  )
  .check_columns(
    crop_shares,
    c("year", "area_code", "item_cbs_code", "area_share"),
    "crop_shares"
  )
}

# Resolve the cell-polity fraction parquet path from the argument, else the
# env var.
.resolve_polity_fraction_path <- function(polity_fraction_path) {
  resolved <- polity_fraction_path %||%
    Sys.getenv("WHEP_POLITY_FRACTION_PATH")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No cell-polity fraction parquet available.",
      i = "Pass {.arg polity_fraction_path} or set
           {.envvar WHEP_POLITY_FRACTION_PATH}."
    ))
  }
  resolved
}

# Each crop's harvested cropland hectares per country-year (grassland
# excluded), the shared numerator of both the area-share and the Coello
# rate-weighted share.
.n_crop_area_ha <- function(primary_prod) {
  grass <- c(3000L, 3002L, 3003L)
  primary_prod |>
    dplyr::filter(
      .data$unit == "ha",
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% grass,
      .data$value > 0
    ) |>
    dplyr::summarise(
      area_ha = sum(.data$value),
      .by = c(year, area_code, item_cbs_code)
    )
}

# Each crop's share of national harvested cropland area per year (grassland
# excluded). This is the promoted, shared version of
# crop_soil_n2o_extension.R's private .crop_area_shares(), which now
# call-throughs to this function.
.n_crop_area_shares <- function(primary_prod) {
  .n_crop_area_ha(primary_prod) |>
    dplyr::mutate(
      area_share = .data$area_ha / sum(.data$area_ha),
      .by = c(year, area_code)
    ) |>
    dplyr::select(year, area_code, item_cbs_code, area_share)
}

# Rate-weighted crop share of the national synthetic-N total: each crop's
# Coello application rate (kg N/ha) times its harvested area, normalised
# within (year, area_code) so shares sum to 1 and the FAOSTAT national total
# is conserved (decision 3). Country-years with no Coello coverage fall back
# to plain harvested-area shares; the choice is stamped in method_synthetic,
# never a silent fallback (multi-method contract).
.n_crop_rate_shares <- function(primary_prod, coello_rates) {
  .n_crop_area_ha(primary_prod) |>
    dplyr::left_join(
      coello_rates,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    .n_rate_shares_resolve()
}

# Resolve the joined area+rate table into normalised shares. Within a
# country-year that has ANY Coello rate, crops missing a rate are imputed the
# country-year mean rate so every cropland crop still receives a share;
# country-years with no coverage use area weights.
.n_rate_shares_resolve <- function(joined) {
  joined |>
    dplyr::mutate(
      rate_observed = is.finite(.data$kg_n_ha) & .data$kg_n_ha >= 0,
      covered = any(.data$rate_observed & .data$kg_n_ha > 0),
      mean_rate = mean(
        dplyr::if_else(.data$rate_observed, .data$kg_n_ha, NA_real_),
        na.rm = TRUE
      ),
      rate = dplyr::if_else(
        .data$rate_observed,
        .data$kg_n_ha,
        .data$mean_rate
      ),
      .by = c(year, area_code)
    ) |>
    dplyr::mutate(
      weight = dplyr::if_else(
        .data$covered,
        .data$rate * .data$area_ha,
        .data$area_ha
      ),
      # sum(weight) is always positive, so this division needs no guard:
      # .n_crop_area_ha() keeps only rows with value > 0, so every area_ha is
      # finite and positive, and covered = TRUE requires some crop with a rate
      # above zero. A country-year with no positive rate takes the area_share
      # branch, where weight IS area_ha.
      area_share = .data$weight / sum(.data$weight),
      method_synthetic = dplyr::if_else(
        .data$covered,
        "coello",
        "area_share"
      ),
      .by = c(year, area_code)
    ) |>
    dplyr::select(
      year,
      area_code,
      item_cbs_code,
      area_share,
      method_synthetic
    )
}

# Shared synthetic-N crop-share origin for both the build_n_inputs() and
# build_crop_soil_n2o_extension() synthetic paths: default "coello"
# (rate-weighted, FAOSTAT-conserving) or "area_share" (harvested-area only).
# Selecting one origin here is what stops the two consumers from diverging
# (fix at origin, decision 3).
.n_synthetic_crop_shares <- function(
  primary_prod,
  method = c("coello", "area_share"),
  coello_rates = NULL
) {
  method <- rlang::arg_match(method)
  if (method == "area_share") {
    return(
      .n_crop_area_shares(primary_prod) |>
        dplyr::mutate(method_synthetic = "area_share")
    )
  }
  coello_rates <- coello_rates %||% whep::coello_synthetic_n
  .n_crop_rate_shares(primary_prod, coello_rates)
}

# Polity-total N (one fert_type) x crop-area-share -> polity x crop N.
.n_polity_crop_totals <- function(country_totals, crop_shares) {
  missing_support <- country_totals |>
    dplyr::anti_join(
      dplyr::distinct(crop_shares, .data$year, .data$area_code),
      by = c("year", "area_code")
    )
  if (nrow(missing_support) > 0L) {
    cli::cli_abort(c(
      paste0(
        "Cannot allocate {nrow(missing_support)} polity N total{?s}: ",
        "no crop-area shares are available."
      ),
      i = paste0(
        "Affected year/area_code pairs: ",
        "{unique(paste(missing_support$year, ",
        "missing_support$area_code, sep = '/'))}."
      )
    ))
  }
  crop_shares |>
    dplyr::inner_join(
      country_totals,
      by = c("year", "area_code"),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      year,
      area_code,
      item_cbs_code,
      n_t = .data$n_t * .data$area_share
    )
}

# Distribute each polity-crop N total across grid cells in proportion to the
# cell's share of that polity-crop's crop-pattern hectares. Any polity-crop
# absent from the crop-pattern raster (or whose pattern area sums to zero in
# the polity) is instead spread uniformly across the polity's cropland cells
# (weighted by cell cropland area), so grid totals still re-aggregate to the
# polity total rather than silently dropping that crop's nitrogen.
.n_grid_totals <- function(polity_crop, cell_polity, data) {
  years <- unique(polity_crop$year)
  item_prod_codes <- .n_item_prod_codes(unique(polity_crop$item_cbs_code))
  cropland_ha <- .n_cropland_ha(data, years)
  pattern_ha <- .n_crop_pattern_ha(
    cropland_ha,
    data,
    item_prod_codes$item_prod_code
  )
  pattern_weights <- .n_cell_weights(pattern_ha, cell_polity, item_prod_codes)
  cropland_weights <- .n_cropland_cell_weights(cropland_ha, cell_polity)
  matched <- .n_grid_matched(polity_crop, pattern_weights)
  unmatched <- .n_grid_unmatched(polity_crop, pattern_weights, cropland_weights)
  dplyr::bind_rows(matched, unmatched)
}

# Polity-crops that have crop-pattern weights: distribute by cell_share.
.n_grid_matched <- function(polity_crop, pattern_weights) {
  polity_crop |>
    dplyr::inner_join(
      pattern_weights,
      by = c("year", "area_code", "item_cbs_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      lon,
      lat,
      area_code,
      year,
      item_cbs_code,
      n_t = .data$n_t * .data$cell_share
    )
}

# Polity-crops absent from the crop-pattern weights: warn on the reallocated
# tonnage, then spread uniformly across the polity's cropland cells so no
# nitrogen is lost from the grid total.
.n_grid_unmatched <- function(polity_crop, pattern_weights, cropland_weights) {
  unmatched <- polity_crop |>
    dplyr::anti_join(
      dplyr::distinct(
        pattern_weights,
        .data$year,
        .data$area_code,
        .data$item_cbs_code
      ),
      by = c("year", "area_code", "item_cbs_code")
    )
  if (nrow(unmatched) == 0L) {
    return(.n_empty_grid())
  }
  missing_support <- unmatched |>
    dplyr::anti_join(
      dplyr::distinct(cropland_weights, .data$year, .data$area_code),
      by = c("year", "area_code")
    )
  if (nrow(missing_support) > 0L) {
    cli::cli_abort(c(
      paste0(
        "Cannot spatialize {nrow(missing_support)} polity-crop total{?s}: ",
        "no positive cropland cells are available."
      ),
      i = paste0(
        "Affected year/area_code pairs: ",
        "{unique(paste(missing_support$year, ",
        "missing_support$area_code, sep = '/'))}."
      )
    ))
  }
  .n_warn_unmatched(unmatched)
  unmatched |>
    dplyr::inner_join(
      cropland_weights,
      by = c("year", "area_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      lon,
      lat,
      area_code,
      year,
      item_cbs_code,
      n_t = .data$n_t * .data$cropland_share
    )
}

# A zero-row tibble with the grid output schema.
.n_empty_grid <- function() {
  tibble::tibble(
    lon = numeric(),
    lat = numeric(),
    area_code = integer(),
    year = integer(),
    item_cbs_code = integer(),
    n_t = numeric()
  )
}

# Warn that some crops' nitrogen was reallocated to cropland cells because the
# crop-pattern raster had no hectares for them in the polity.
.n_warn_unmatched <- function(unmatched) {
  cli::cli_warn(c(
    "!" = "{nrow(unmatched)} polity-crop total{?s} ({round(sum(unmatched$n_t), 3)}
           t N) had no crop-pattern grid cells; reallocating uniformly across
           the polity's cropland cells.",
    "i" = "Affected item_cbs_code{?s}: {as.character(sort(unique(unmatched$item_cbs_code)))}."
  ))
}

# item_cbs_code -> item_prod_code lookup (the same crosswalk used by
# build_crop_land_extension()), restricted to the requested codes.
.n_item_prod_codes <- function(item_cbs_codes) {
  whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(
      .data$item_cbs_code %in% item_cbs_codes,
      !is.na(.data$item_prod_code)
    ) |>
    dplyr::distinct(.data$item_cbs_code, .data$item_prod_code)
}

# Per-cell total cropland hectares = type_ha summed over luh2_type, for the
# requested years. The crop-independent cropland area used both for the
# per-crop pattern weights and for the fallback uniform reallocation.
.n_cropland_ha <- function(data, years) {
  .n_read_type_cropland(data$type_cropland, years) |>
    dplyr::summarise(
      type_ha = sum(.data$type_ha, na.rm = TRUE),
      .by = c(lon, lat, year)
    )
}

# Per-cell per-crop hectares = type_ha (summed over luh2_type) * harvest_
# fraction, the exact formula used by make_lpjml_covariate()'s
# "crop_pattern" branch (R/lpjml_covariate.R), reused verbatim.
.n_crop_pattern_ha <- function(cropland_ha, data, item_prod_codes) {
  crop_patterns <- .n_read_crop_patterns(data$crop_patterns, item_prod_codes)
  dplyr::inner_join(cropland_ha, crop_patterns, by = c("lon", "lat")) |>
    dplyr::mutate(
      crop_pattern_ha = .data$type_ha * .data$harvest_fraction
    ) |>
    dplyr::select(lon, lat, year, item_prod_code, crop_pattern_ha)
}

# Read type_cropland.parquet, filtering to the requested years immediately
# after reading (before any join), so a single-year query stays fast against
# the ~27.5M-row real file.
.n_read_type_cropland <- function(type_cropland, years) {
  raw <- type_cropland %||% .n_read_parquet_env("WHEP_TYPE_CROPLAND_PATH")
  .check_columns(
    raw,
    c("lon", "lat", "year", "luh2_type", "type_ha"),
    "type_cropland"
  )
  dplyr::filter(tibble::as_tibble(raw), .data$year %in% years)
}

# Read crop_patterns.parquet, filtering to the requested item_prod_codes
# immediately after reading (before any join), so a single-crop query stays
# fast against the ~2.2M-row real file.
.n_read_crop_patterns <- function(crop_patterns, item_prod_codes) {
  raw <- crop_patterns %||% .n_read_parquet_env("WHEP_CROP_PATTERNS_PATH")
  .check_columns(
    raw,
    c("lon", "lat", "item_prod_code", "harvest_fraction"),
    "crop_patterns"
  )
  dplyr::filter(
    tibble::as_tibble(raw),
    .data$item_prod_code %in% item_prod_codes
  )
}

# Read a parquet path from an env var, aborting with the env var name if
# unset (never hardcode the real local path).
.n_read_parquet_env <- function(env_var) {
  path <- Sys.getenv(env_var)
  if (!.has_path(path)) {
    cli::cli_abort(c(
      "No {env_var} input available.",
      i = "Pass it via {.arg data}, or set {.envvar {env_var}}."
    ))
  }
  nanoparquet::read_parquet(path) |> tibble::as_tibble()
}

# Each cell's share of its polity-crop's total crop_pattern_ha, joined to
# the cell-polity crosswalk (area_code) and the item_cbs_code<->item_prod_code
# lookup. Polity-crops whose weighted crop-pattern area sums to zero are
# dropped here (returned as unmatched) so the caller reallocates them across
# the polity's cropland cells instead of distributing to a zero share.
.n_cell_weights <- function(pattern_ha, cell_polity, item_prod_codes) {
  pattern_ha |>
    dplyr::inner_join(item_prod_codes, by = "item_prod_code") |>
    dplyr::inner_join(
      dplyr::select(cell_polity, lon, lat, area_code, polity_frac),
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(weighted_ha = .data$crop_pattern_ha * .data$polity_frac) |>
    dplyr::mutate(
      group_ha = sum(.data$weighted_ha),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::filter(.data$group_ha > 0) |>
    dplyr::mutate(cell_share = .data$weighted_ha / .data$group_ha) |>
    dplyr::select(lon, lat, area_code, year, item_cbs_code, cell_share)
}

# Each cell's share of its polity's total cropland area (crop-independent),
# used to spread the nitrogen of crops absent from the crop-pattern raster.
.n_cropland_cell_weights <- function(cropland_ha, cell_polity) {
  cropland_ha |>
    dplyr::inner_join(
      dplyr::select(cell_polity, lon, lat, area_code, polity_frac),
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(weighted_ha = .data$type_ha * .data$polity_frac) |>
    dplyr::mutate(
      group_ha = sum(.data$weighted_ha),
      .by = c(year, area_code)
    ) |>
    dplyr::filter(.data$group_ha > 0) |>
    dplyr::mutate(cropland_share = .data$weighted_ha / .data$group_ha) |>
    dplyr::select(lon, lat, area_code, year, cropland_share)
}
