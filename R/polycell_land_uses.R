#' Partition each polycell into mutually exclusive land uses.
#'
#' @description
#' Split every polycell-year's `land_area_ha` into `cropland`, `grassland`,
#' `urban`, `natural` and `unclassified`, so territorial quantities can be
#' attributed to a land class instead of being assumed agricultural or dropped
#' (issue #423).
#'
#' The **level** of each agricultural class comes from the statistical record,
#' which is authoritative; LUH2 supplies only the **within-country spatial
#' pattern**. The two provenances are recorded in separate columns
#' (`level_source`, `pattern_source`), and their per-polycell difference is
#' emitted as `statistical_pattern_disagreement_ha` rather than being absorbed
#' into the natural class. That column is a transition instrument: its magnitude
#' measures how much LUH2 is still doing, and it is the criterion for retiring
#' LUH2 as a source.
#'
#' Inland water and ice are not land uses. They stay on the polycell as their
#' own categories and never enter a class.
#'
#' @details
#' Per polycell-year:
#'
#' 1. Class **shares** come from the LUH2 `fraction` column - LUH2's share of
#'    the whole cell, which is identical on every polycell of a border cell and
#'    invariant to the reader's `area_basis`. They are applied to the polycell's
#'    own measured `land_area_ha`, so the classes tile the polycell's land by
#'    construction rather than to a tolerance.
#' 2. FAO counts temporary meadows and pastures (item 6633) inside arable land,
#'    while LUH2 books that ground as grassland. That component is therefore
#'    spread over the LUH2 **grassland** pattern but still emitted as
#'    `cropland`, because FAO's class definition is the one being anchored.
#' 3. Each agricultural class is rescaled so its polycells sum to the
#'    statistical national total for that `(area_code, year)`.
#' 4. Where the anchored area exceeds a polycell's land, it is reconciled by
#'    `overfull_method`, never renormalised in silence.
#' 5. `natural` takes the remainder of `land_area_ha`.
#' 6. A polycell with no pattern coverage is `unclassified` in full - never
#'    `natural`.
#'
#' Where a statistical level is absent for a class-country-year, the LUH2
#' pattern level is used and the row is labelled (`level_source = "luh2"`,
#' `area_source = "pattern_only"`, `allocation_status = "level_missing"`). It is
#' never silently filled. `urban` has no statistical source and is always
#' pattern-only.
#'
#' # Pre-1961 levels
#'
#' FAOSTAT land use starts in 1961, so an unextended level would step at that
#' year. Both agricultural classes are therefore backcast the same way: the FAO
#' 1961 level is carried backwards by LUH2's own national trend, matching FAO
#' exactly at the splice. [get_arable_permanent_land()] already does this for
#' cropland; the pasture backcast here mirrors it, reading the same
#' `luh2-areas` input. A country with no FAO 1961 anchor receives no backcast.
#'
#' A backcast row is labelled `luh2_backcast_cropland` or
#' `luh2_backcast_pasture` in `level_source` and is **excluded** from the
#' statistical-versus-pattern diagnostic, because comparing a backcast with the
#' pattern compares LUH2 with itself.
#'
#' # Reconciling an overfull polycell
#'
#' A national statistical total spread by the LUH2 pattern can give a polycell
#' more agricultural land than it has, driven by countries where FAO and LUH2
#' disagree about how much land is permanent pasture. Measured on this function
#' at 2020: 63.50 Mha, 1.33% of the anchored agricultural area, of which Saudi
#' Arabia is 35.10 Mha and Sudan (former) 14.20 Mha. `overfull_method` selects
#' the treatment and is recorded per row in `method_overfull`:
#'
#' * `"spillover"` (default) places the excess on same-country neighbours,
#'   widening the search ring until it is absorbed, taking non-forested natural
#'   land (LUH2 `primn`, `secdn`) first and forest (`primf`, `secdf`) only as a
#'   fallback. The ring each hectare reached is reported in
#'   `spillover_max_ring`; at 2020 it places **all** 63.50 Mha, reaching a
#'   median ring of 2 and a maximum of 22.
#' * `"cap"` caps the agricultural classes at `land_area_ha` pro rata and leaves
#'   the whole 63.50 Mha in `unplaceable_statistical_ha`. It is the sensitivity
#'   baseline that quantifies what spillover buys.
#'
#' The methods are alternatives, never fallbacks. If `"spillover"` cannot place
#' a hectare anywhere in the country, it stays in `unplaceable_statistical_ha`
#' with a warning and the row still reads `method_overfull = "spillover"`.
#'
#' @param years Integer vector of calendar years to return, or `NULL` (default)
#'   for every year present in the support.
#' @param grassland_level_source Grassland level basis, `"faostat_pasture"`
#'   (default, FAO Land Use item 6655) or `"luh2"`. The default matches the
#'   cropland anchor so both agricultural classes rest on one convention;
#'   `"luh2"` remains selectable for sensitivity analysis and is recorded in
#'   `level_source`. It is never used as a fallback. Note this differs from
#'   [build_grassland_land_extension()]'s own default, which is `"luh2"`: item
#'   6655 excludes temporary meadows and pastures, so the two are a difference
#'   in what `grassland` means and not only in provenance. The divergence is
#'   deliberate and tracked in whep#759; this function always passes its choice
#'   explicitly rather than inheriting a default.
#' @param overfull_method How to reconcile a polycell whose anchored
#'   agricultural area exceeds its land: `"spillover"` (default) or `"cap"`.
#'   See Details.
#' @param data Named list of pre-loaded inputs bypassing the readers:
#'   `polycell_support` (the [read_polycell_support()] table, interval or year
#'   grain), `pattern` (the [read_luh2_landuse()] grid table), `natural_split`
#'   (per cell, the non-forested share of natural land), `cropland_level` (the
#'   [get_arable_permanent_land()] table), `grassland_level` (the
#'   [build_grassland_land_extension()] table) and `temporary_meadows`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with one row per polycell-year-class: `polycell_id`, `lon`,
#'   `lat`, `polity_code`, `area_code`, `year`, `land_use`, `area_ha`,
#'   `area_source` (`anchored`, `pattern_only`, `residual` or `unclassified`),
#'   `level_source`, `pattern_source`, `allocation_status`,
#'   `statistical_pattern_disagreement_ha` (`NA` where no statistical level
#'   applies), `unplaceable_statistical_ha`, `method_overfull`,
#'   `spillover_max_ring`, `coverage_status`, and the polity reporting columns
#'   carried through from the support.
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' build_polycell_land_uses(example = TRUE)
build_polycell_land_uses <- function(
  years = NULL,
  grassland_level_source = c("faostat_pasture", "luh2"),
  overfull_method = c("spillover", "cap"),
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_polycell_land_uses())
  }
  grassland_level_source <- rlang::arg_match(grassland_level_source)
  overfull_method <- rlang::arg_match(overfull_method)

  support <- .plu_support(data$polycell_support, years)
  pattern <- .plu_pattern_shares(.plu_pattern(data$pattern, years), support)
  levels <- .plu_levels(data, pattern, grassland_level_source, years)

  .plu_check_level_key(levels)
  allocated <- .plu_anchored(pattern, levels)
  .plu_warn_unplaced_levels(pattern, levels)
  allocated <- .plu_reconcile(allocated, support, overfull_method, data, years)

  dplyr::bind_rows(
    allocated,
    .plu_residual_natural(support, allocated),
    .plu_unclassified(support, pattern)
  ) |>
    .plu_finalise(overfull_method)
}

# ---- Private helpers: inputs -------------------------------------------------

# The classes carried by the spatial pattern. `natural` is deliberately absent:
# it is the residual of `land_area_ha`, not a patterned quantity, so taking it
# from LUH2 as well would let the pattern and the statistical anchor both claim
# the same hectares.
.plu_pattern_classes <- function() {
  c("cropland", "grassland", "urban")
}

# The support is read at whatever grain it was published in. The pin is
# interval-keyed (start_year/end_year), so `year` has to be expanded rather than
# assumed; the reader itself validates nothing, hence the column check here.
.plu_support <- function(support, years) {
  support <- support %||% read_polycell_support()
  .plu_check_cols(
    support,
    c("polycell_id", "lon", "lat", "polity_code", "land_area_ha"),
    "data$polycell_support"
  )
  if (!rlang::has_name(support, "year")) {
    support <- expand_polycell_years(support, years)
  }
  keep <- c(
    "polycell_id",
    "lon",
    "lat",
    "polity_code",
    "area_code",
    "year",
    "land_area_ha",
    intersect(.plu_polity_cols(), names(support))
  )
  support |>
    dplyr::select(dplyr::any_of(unique(keep))) |>
    .plu_filter_years(years) |>
    .plu_check_support_key() |>
    .plu_check_land_area()
}

# Every balance-critical sum below uses `na.rm = TRUE`, so an NA or negative
# land area would propagate to an NA `natural` that the overfull guard cannot
# see and nothing warns about. Missing has to stay missing (S-B7), so it is
# refused here rather than becoming a zero three joins later.
.plu_check_land_area <- function(support) {
  bad <- support |>
    dplyr::filter(is.na(.data$land_area_ha) | .data$land_area_ha < 0)
  if (nrow(bad) == 0L) {
    return(support)
  }
  cli::cli_abort(c(
    "{.field land_area_ha} is missing or negative in {nrow(bad)} polycell-year{?s}.",
    i = "First: {.val {bad$polycell_id[1]}} in {.val {bad$year[1]}}
         ({.val {bad$land_area_ha[1]}}).",
    i = "A land area that cannot be partitioned must not be silently zeroed."
  ))
}

# (polycell_id, year) is the support's only unique key: the interval grain
# splits one polity's interval in a cell whenever a co-occupant arrives or
# leaves, so polycell_id alone repeats. Both the overfull reconciliation and the
# natural residual assume uniqueness, so assert it rather than double-count.
.plu_check_support_key <- function(support) {
  dup <- support |>
    dplyr::summarise(n = dplyr::n(), .by = c("polycell_id", "year")) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(dup) > 0L) {
    cli::cli_abort(c(
      "{.arg data$polycell_support} repeats {nrow(dup)} {.field polycell_id} by
       {.field year} pair{?s}.",
      i = "Expand intervals to years before partitioning them.",
      i = "First: {.val {dup$polycell_id[1]}} in {.val {dup$year[1]}}."
    ))
  }
  support
}

# Whole-cell class shares. `fraction` is LUH2's share of the WHOLE cell and is
# repeated identically on every polycell of a border cell, so it is a source
# datum rather than a per-polity quantity: taking shares from it makes the
# partition independent of the reader's area_basis and of its static 2015
# cell-to-country snapshot. Summing `area_ha` over area_code would instead
# import that snapshot into every historical year.
.plu_pattern <- function(pattern, years) {
  pattern <- pattern %||% read_luh2_landuse(resolution = "grid", years = years)
  .plu_check_cols(
    pattern,
    c("lon", "lat", "year", "land_use", "fraction"),
    "data$pattern"
  )
  pattern |>
    .plu_filter_years(years) |>
    dplyr::summarise(
      fraction = dplyr::first(.data$fraction),
      .by = c("lon", "lat", "year", "land_use")
    )
}

# Turn whole-cell shares into polycell areas. The shares are renormalised over
# the four classes so they tile the polycell's own land exactly; a cell whose
# states sum to zero carries no composition and drops out here, to be recovered
# as `unclassified`.
.plu_pattern_shares <- function(pattern, support) {
  shares <- pattern |>
    dplyr::mutate(
      total = sum(.data$fraction, na.rm = TRUE),
      .by = c("lon", "lat", "year")
    ) |>
    dplyr::filter(.data$total > 0) |>
    dplyr::mutate(share = .data$fraction / .data$total) |>
    dplyr::filter(.data$land_use %in% .plu_pattern_classes())

  support |>
    dplyr::inner_join(
      dplyr::select(shares, "lon", "lat", "year", "land_use", "share"),
      by = c("lon", "lat", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(pattern_ha = .data$share * .data$land_area_ha) |>
    dplyr::select(-"share")
}

# ---- Private helpers: statistical levels -------------------------------------

.plu_levels <- function(data, pattern, grassland_level_source, years) {
  meadows <- .plu_temporary_meadows(data$temporary_meadows, years)
  dplyr::bind_rows(
    .plu_cropland_level(data$cropland_level, meadows, years),
    .plu_grassland_level(
      data$grassland_level,
      pattern,
      grassland_level_source,
      years,
      data$luh2_grassland,
      sort(unique(pattern$year))
    )
  )
}

# FAO physical cropland (arable plus permanent), the DB-1 anchor. Its own
# `source` column distinguishes the FAO record from the pre-1961 LUH2 backcast
# and must be carried through: a backcast row's disagreement with the pattern is
# LUH2 measured against itself, not evidence about LUH2.
.plu_cropland_level <- function(cropland, meadows, years) {
  cropland <- cropland %||% get_arable_permanent_land(years = years)
  .plu_check_cols(
    cropland,
    c("area_code", "year", "cropland_ha", "source"),
    "data$cropland_level"
  )
  cropland |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      land_use = "cropland",
      level_ha = as.numeric(.data$cropland_ha),
      level_source = dplyr::if_else(
        .data$source == "fao",
        "fao_cropland",
        "luh2_backcast_cropland"
      )
    ) |>
    dplyr::filter(!is.na(.data$level_ha)) |>
    .plu_split_meadows(meadows)
}

# FAO books temporary meadows and pastures inside arable land; LUH2 books that
# ground as grassland. Anchoring the whole cropland level onto the cropland
# pattern therefore squeezes pasture-shaped land into a crop-shaped pattern. The
# component keeps its FAO class and moves to the grassland pattern instead.
.plu_split_meadows <- function(cropland, meadows) {
  if (is.null(meadows)) {
    return(dplyr::mutate(cropland, pattern_class = .data$land_use))
  }
  cropland |>
    dplyr::left_join(meadows, by = c("area_code", "year")) |>
    dplyr::mutate(
      meadow_ha = pmin(
        dplyr::coalesce(.data$meadow_ha, 0),
        .data$level_ha
      )
    ) |>
    .plu_meadow_rows() |>
    dplyr::select(-"meadow_ha") |>
    dplyr::filter(.data$level_ha > 0)
}

# The core keeps the cropland pattern; the meadow component keeps its FAO class
# but is routed to the grassland pattern via `pattern_class`.
.plu_meadow_rows <- function(x) {
  dplyr::bind_rows(
    x |>
      dplyr::mutate(
        level_ha = .data$level_ha - .data$meadow_ha,
        pattern_class = "cropland"
      ),
    x |>
      dplyr::filter(.data$meadow_ha > 0) |>
      dplyr::mutate(
        level_ha = .data$meadow_ha,
        pattern_class = "grassland"
      )
  )
}

# FAOSTAT Land Use item 6633, in hectares.
.plu_temporary_meadows <- function(meadows, years) {
  meadows <- meadows %||% .plu_read_landuse_item(6633L, years)
  if (is.null(meadows) || nrow(meadows) == 0L) {
    return(NULL)
  }
  .plu_check_cols(meadows, c("area_code", "year", "meadow_ha"), "meadows")
  meadows |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      meadow_ha = as.numeric(.data$meadow_ha)
    ) |>
    dplyr::filter(!is.na(.data$meadow_ha)) |>
    dplyr::summarise(
      meadow_ha = sum(.data$meadow_ha),
      .by = c("area_code", "year")
    )
}

# DB-2. The "luh2" method takes the level from the pattern itself, so the
# disagreement collapses to zero by construction and `level_source` records the
# choice. It is a selectable method, never a fallback.
.plu_grassland_level <- function(
  grassland,
  pattern,
  grassland_level_source,
  years,
  luh2_grassland,
  built_years
) {
  if (identical(grassland_level_source, "luh2")) {
    return(.plu_grassland_from_pattern(pattern))
  }
  # Passed explicitly, never inherited: the extension's own default is "luh2"
  # (whep#759), which is a different class definition, not a different source.
  grassland <- grassland %||%
    build_grassland_land_extension(source = "faostat_pasture")
  .plu_check_cols(
    grassland,
    c("area_code", "year", "impact_u"),
    "data$grassland_level"
  )
  grassland |>
    .plu_grassland_statistical() |>
    .plu_bind_pasture_backcast(built_years, luh2_grassland) |>
    .plu_filter_years(years) |>
    dplyr::mutate(pattern_class = "grassland")
}

.plu_grassland_from_pattern <- function(pattern) {
  pattern |>
    dplyr::filter(.data$land_use == "grassland") |>
    dplyr::summarise(
      level_ha = sum(.data$pattern_ha, na.rm = TRUE),
      .by = c("area_code", "year", "land_use")
    ) |>
    dplyr::mutate(level_source = "luh2", pattern_class = "grassland")
}

# `impact_u` summed over (area_code, year) is FAO item 6655 itself: verified
# 2026-08-11 at a median ratio of 1.0000 over 204 matched areas, with no area
# differing by more than 1%.
.plu_grassland_statistical <- function(grassland) {
  grassland |>
    dplyr::summarise(
      level_ha = sum(.data$impact_u, na.rm = TRUE),
      .by = c("area_code", "year")
    ) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      land_use = "grassland",
      level_ha = .data$level_ha,
      level_source = "faostat_pasture"
    ) |>
    dplyr::filter(!is.na(.data$level_ha))
}

# ---- Private helpers: the pre-1961 pasture backcast --------------------------

.plu_anchor_year <- function() 1961L

# Carry the FAO 1961 pasture level backwards on LUH2's own national trend, so
# the gridded grassland series does not step at FAOSTAT's start. This mirrors
# .luh2_perennial_backcast() in R/arable_permanent_land.R exactly - same input,
# same splice, same "no FAO anchor means no backcast" rule - so cropland and
# grassland rest on one convention rather than two.
.plu_bind_pasture_backcast <- function(grassland, built_years, luh2_grassland) {
  anchor_year <- .plu_anchor_year()
  # Decided by the years actually being built. A modern-only build needs no
  # backcast, and must not read the trend input to discover that.
  if (length(built_years) == 0L || all(built_years >= anchor_year)) {
    return(grassland)
  }
  luh2 <- luh2_grassland %||% .plu_luh2_grassland()
  if (is.null(luh2)) {
    return(grassland)
  }
  anchor <- grassland |>
    dplyr::filter(.data$year == anchor_year, .data$level_ha > 0) |>
    dplyr::select("area_code", fao_1961 = "level_ha")

  # The LUH2 anchor is the same table's own 1961 row, so it is a grouped lookup
  # rather than a second join.
  pre <- luh2 |>
    dplyr::mutate(
      luh2_1961 = .data$luh2_ha[match(anchor_year, .data$year)],
      .by = "area_code"
    ) |>
    dplyr::filter(
      .data$year < anchor_year,
      !is.na(.data$luh2_1961),
      .data$luh2_1961 > 0
    ) |>
    dplyr::inner_join(anchor, by = "area_code") |>
    dplyr::transmute(
      .data$area_code,
      .data$year,
      land_use = "grassland",
      level_ha = .data$fao_1961 * .data$luh2_ha / .data$luh2_1961,
      level_source = "luh2_backcast_pasture"
    ) |>
    dplyr::filter(is.finite(.data$level_ha), .data$level_ha > 0)

  dplyr::bind_rows(grassland, pre)
}

# National LUH2 grassland (pastr + range) per area_code and year, from the same
# `luh2-areas` input the cropland backcast reads.
.plu_luh2_grassland <- function() {
  tryCatch(
    .luh2_national_states(c("pastr", "range")),
    error = function(e) NULL
  )
}

# ---- Private helpers: allocation ---------------------------------------------

# The level join must be many-to-many, because the meadow split deliberately
# puts two levels on the grassland pattern. That permits a fan-out, so the
# levels' own key is asserted here instead: a duplicated row would double a
# country's anchored area and read downstream as twice its cropland.
.plu_check_level_key <- function(levels) {
  dup <- levels |>
    dplyr::summarise(
      n = dplyr::n(),
      .by = c("area_code", "year", "land_use", "pattern_class")
    ) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(dup) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{nrow(dup)} statistical level{?s} {?is/are} duplicated.",
    i = "First: {.field {dup$land_use[1]}} for area {.val {dup$area_code[1]}}
         in {.val {dup$year[1]}}, {dup$n[1]} rows.",
    i = "One class-country-year carries one level."
  ))
}

# Rescale each class so its polycells sum to the statistical national total, and
# record the per-polycell difference from the raw pattern. The join is on
# `pattern_class`, so a level whose FAO class differs from the pattern that
# carries it (temporary meadows) lands on the right shape.
.plu_anchored <- function(pattern, levels) {
  pattern |>
    dplyr::mutate(
      pattern_national_ha = sum(.data$pattern_ha, na.rm = TRUE),
      .by = c("area_code", "year", "land_use")
    ) |>
    dplyr::left_join(
      levels,
      by = c("area_code", "year", "land_use" = "pattern_class"),
      relationship = "many-to-many",
      suffix = c("", "_level")
    ) |>
    dplyr::mutate(
      land_use = dplyr::coalesce(.data$land_use_level, .data$land_use),
      anchored = !is.na(.data$level_ha) & .data$pattern_national_ha > 0
    ) |>
    .plu_anchored_columns() |>
    .plu_collapse_split_levels()
}

# Sum the components a split level produced, so one polycell-year-class is one
# row. area_ha and the disagreement are additive; the labels are identical
# across the components because they come from the same statistical level.
.plu_collapse_split_levels <- function(x) {
  x |>
    dplyr::summarise(
      area_ha = sum(.data$area_ha),
      statistical_pattern_disagreement_ha = sum(
        .data$statistical_pattern_disagreement_ha
      ),
      .by = c(
        "polycell_id",
        "lon",
        "lat",
        "polity_code",
        "area_code",
        "year",
        "land_use",
        "area_source",
        "level_source",
        "pattern_source",
        "allocation_status",
        "coverage_status"
      )
    )
}

.plu_anchored_columns <- function(joined) {
  joined |>
    dplyr::mutate(
      area_ha = dplyr::if_else(
        .data$anchored,
        .data$level_ha * .data$pattern_ha / .data$pattern_national_ha,
        .data$pattern_ha
      ),
      statistical_pattern_disagreement_ha = dplyr::if_else(
        .data$anchored & .plu_is_statistical(.data$level_source),
        .data$area_ha - .data$pattern_ha,
        NA_real_
      ),
      area_source = dplyr::if_else(.data$anchored, "anchored", "pattern_only"),
      level_source = dplyr::if_else(.data$anchored, .data$level_source, "luh2"),
      allocation_status = dplyr::case_when(
        .data$anchored ~ "ok",
        .data$land_use == "urban" ~ "no_level_source",
        .default = "level_missing"
      ),
      pattern_source = "luh2",
      coverage_status = "observed"
    )
}

# A backcast level is LUH2 carried on the FAO 1961 anchor, so its difference
# from the LUH2 pattern measures LUH2 against itself and must not enter the
# LUH2-exit metric.
.plu_is_statistical <- function(level_source) {
  level_source %in% c("fao_cropland", "faostat_pasture")
}

# A statistical level whose class has no pattern anywhere in the country cannot
# be placed, so the anchor does not bind and the national total will not
# reproduce the source (S-B2). That must never pass silently.
.plu_warn_unplaced_levels <- function(pattern, levels) {
  placed <- pattern |>
    dplyr::summarise(
      pattern_national_ha = sum(.data$pattern_ha, na.rm = TRUE),
      .by = c("area_code", "year", "land_use")
    ) |>
    dplyr::filter(.data$pattern_national_ha > 0)
  unplaced <- levels |>
    dplyr::filter(.data$level_ha > 0) |>
    dplyr::anti_join(
      placed,
      by = c("area_code", "year", "pattern_class" = "land_use")
    )
  if (nrow(unplaced) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "{nrow(unplaced)} statistical level{?s} could not be placed: the class has
     no spatial pattern in that polity-year.",
    i = "Affected classes: {.val {sort(unique(unplaced$land_use))}}.",
    i = "Unplaced in total: {.val {sum(unplaced$level_ha)}} ha.",
    i = "Those polity-years do not reproduce their statistical total."
  ))
  invisible(unplaced)
}

# ---- Private helpers: overfull reconciliation --------------------------------

# Dispatch on the selected method. The two are alternatives, not a fallback
# chain: "spillover" that cannot place a hectare reports it rather than capping.
.plu_reconcile <- function(allocated, support, overfull_method, data, years) {
  overfull <- .plu_overfull(allocated, support)
  if (nrow(overfull) == 0L) {
    return(dplyr::mutate(
      allocated,
      unplaceable_statistical_ha = 0,
      spillover_max_ring = NA_integer_
    ))
  }
  if (identical(overfull_method, "cap")) {
    return(.plu_cap(allocated, overfull))
  }
  .plu_spillover(allocated, support, overfull, data, years)
}

# Agricultural area a polycell cannot hold, given that urban is pattern-only and
# is not rescaled.
.plu_overfull <- function(allocated, support) {
  allocated |>
    dplyr::summarise(
      claimed_ha = sum(.data$area_ha, na.rm = TRUE),
      .by = c("polycell_id", "year")
    ) |>
    dplyr::inner_join(
      dplyr::select(support, "polycell_id", "year", "land_area_ha"),
      by = c("polycell_id", "year")
    ) |>
    dplyr::mutate(excess_ha = .data$claimed_ha - .data$land_area_ha) |>
    dplyr::filter(.data$excess_ha > .plu_area_tolerance(.data$land_area_ha))
}

# Relative, because real polycells reach 1e5 ha and a fixed 1e-6 ha bound sits
# at the edge of double-precision noise.
.plu_area_tolerance <- function(land_area_ha) {
  1e-6 + 1e-9 * abs(land_area_ha)
}

# Cap the rescaled classes pro rata at the polycell's land and leave the
# shortfall named. Measured cost at 2020: 63.50 Mha, 1.33% of the anchored
# total, of which Saudi Arabia alone is 35.10 Mha.
.plu_cap <- function(allocated, overfull) {
  allocated |>
    dplyr::left_join(
      dplyr::select(
        overfull,
        "polycell_id",
        "year",
        "claimed_ha",
        "land_area_ha"
      ),
      by = c("polycell_id", "year")
    ) |>
    dplyr::mutate(
      anchored_ha = sum(.data$area_ha[.data$area_source == "anchored"]),
      other_ha = sum(.data$area_ha[.data$area_source != "anchored"]),
      .by = c("polycell_id", "year")
    ) |>
    dplyr::mutate(
      scale = dplyr::if_else(
        !is.na(.data$claimed_ha) &
          .data$anchored_ha > 0 &
          .data$area_source == "anchored",
        pmax(.data$land_area_ha - .data$other_ha, 0) / .data$anchored_ha,
        1
      ),
      unplaceable_statistical_ha = .data$area_ha * (1 - .data$scale),
      area_ha = .data$area_ha * .data$scale,
      spillover_max_ring = NA_integer_
    ) |>
    dplyr::select(
      -"scale",
      -"claimed_ha",
      -"land_area_ha",
      -"anchored_ha",
      -"other_ha"
    )
}

# The published schema, in order. Single source of truth, so the producer and
# the toy fixture cannot drift apart: the fixture selects the same vector.
.plu_output_cols <- function() {
  c(
    "polycell_id",
    "lon",
    "lat",
    "polity_code",
    "area_code",
    "year",
    "land_use",
    "area_ha",
    "area_source",
    "level_source",
    "pattern_source",
    "allocation_status",
    "statistical_pattern_disagreement_ha",
    "unplaceable_statistical_ha",
    "method_overfull",
    "spillover_max_ring",
    "coverage_status",
    .plu_polity_cols()
  )
}

.plu_finalise <- function(x, overfull_method) {
  x |>
    dplyr::mutate(
      method_overfull = overfull_method,
      unplaceable_statistical_ha = dplyr::coalesce(
        .data$unplaceable_statistical_ha,
        0
      )
    ) |>
    dplyr::select(dplyr::any_of(.plu_output_cols())) |>
    dplyr::arrange(.data$year, .data$polycell_id, .data$land_use) |>
    tibble::as_tibble()
}

# `natural` is the remainder of land_area_ha. It carries no disagreement of its
# own: the anchor gap is named on the agricultural rows that produced it.
.plu_residual_natural <- function(support, allocated) {
  allocated |>
    dplyr::summarise(
      allocated_ha = sum(.data$area_ha, na.rm = TRUE),
      .by = c("polycell_id", "year")
    ) |>
    dplyr::inner_join(support, by = c("polycell_id", "year")) |>
    dplyr::mutate(
      land_use = "natural",
      area_ha = pmax(.data$land_area_ha - .data$allocated_ha, 0),
      area_source = "residual",
      level_source = NA_character_,
      pattern_source = "luh2",
      allocation_status = "ok",
      statistical_pattern_disagreement_ha = NA_real_,
      unplaceable_statistical_ha = 0,
      spillover_max_ring = NA_integer_,
      coverage_status = "observed"
    )
}

# Step 6: land the pattern cannot classify stays `unclassified`, never natural.
# A cell whose LUH2 states sum to zero is dropped by .plu_pattern_shares() and
# so is absent here, which is what routes its land to this branch rather than
# letting the residual book it as natural.
.plu_unclassified <- function(support, pattern) {
  support |>
    dplyr::anti_join(
      dplyr::distinct(dplyr::select(pattern, "polycell_id", "year")),
      by = c("polycell_id", "year")
    ) |>
    dplyr::mutate(
      land_use = "unclassified",
      area_ha = .data$land_area_ha,
      area_source = "unclassified",
      level_source = NA_character_,
      pattern_source = NA_character_,
      allocation_status = "pattern_missing",
      statistical_pattern_disagreement_ha = NA_real_,
      unplaceable_statistical_ha = 0,
      spillover_max_ring = NA_integer_,
      coverage_status = "unavailable"
    )
}

# ---- Private helpers: spillover ----------------------------------------------

# Place the excess on same-country neighbours instead of discarding it, widening
# the search ring until it is absorbed (there is no distance cap: measured at
# 2020 it reaches a median ring of 2 but a maximum of 22, because overfull
# polycells cluster and the free land is far from the countries that need it;
# capping the search would strand what the tail places).
# Non-forested natural land is consumed first and forest only as a fallback.
.plu_spillover <- function(allocated, support, overfull, data, years) {
  donors <- .plu_donor_excess(allocated, overfull)
  receivers <- .plu_receiver_capacity(allocated, support, data, years)
  moved <- .plu_spill_rings(donors, receivers)

  allocated |>
    .plu_apply_donor_excess(donors) |>
    .plu_apply_received(moved$received) |>
    dplyr::left_join(
      moved$unplaced,
      by = c("polycell_id", "year", "land_use")
    ) |>
    dplyr::mutate(
      unplaceable_statistical_ha = dplyr::coalesce(
        .data$unplaceable_statistical_ha,
        0
      )
    )
}

# The polycell-year excess, apportioned across that polycell's classes pro rata,
# so a hectare keeps the class it was anchored as when it moves.
.plu_donor_excess <- function(allocated, overfull) {
  allocated |>
    dplyr::inner_join(
      dplyr::select(overfull, "polycell_id", "year", "excess_ha", "claimed_ha"),
      by = c("polycell_id", "year")
    ) |>
    dplyr::mutate(
      anchored_ha = sum(.data$area_ha[.data$area_source == "anchored"]),
      .by = c("polycell_id", "year")
    ) |>
    dplyr::mutate(
      donated_ha = dplyr::if_else(
        .data$anchored_ha > 0 & .data$area_source == "anchored",
        .data$excess_ha * .data$area_ha / .data$anchored_ha,
        0
      )
    ) |>
    dplyr::filter(.data$donated_ha > 0) |>
    dplyr::select(
      "polycell_id",
      "year",
      "land_use",
      "area_code",
      "lon",
      "lat",
      "donated_ha"
    )
}

# What a polycell can absorb: its unclaimed land, which the residual would
# otherwise book as natural. Split by the non-forested share so `primn`/`secdn`
# is consumed before `primf`/`secdf`.
.plu_receiver_capacity <- function(allocated, support, data, years) {
  split <- .plu_natural_split(data$natural_split, years)
  allocated |>
    dplyr::summarise(
      claimed_ha = sum(.data$area_ha, na.rm = TRUE),
      .by = c("polycell_id", "year")
    ) |>
    dplyr::right_join(support, by = c("polycell_id", "year")) |>
    dplyr::mutate(
      claimed_ha = dplyr::coalesce(.data$claimed_ha, 0),
      spare_ha = pmax(.data$land_area_ha - .data$claimed_ha, 0)
    ) |>
    dplyr::filter(.data$spare_ha > 0) |>
    dplyr::left_join(split, by = c("lon", "lat")) |>
    dplyr::mutate(
      nonforest_share = dplyr::coalesce(.data$nonforest_share, 1),
      free_nonforest = .data$spare_ha * .data$nonforest_share,
      free_forest = .data$spare_ha - .data$free_nonforest
    ) |>
    dplyr::select(
      "polycell_id",
      "year",
      "area_code",
      "lon",
      "lat",
      "free_nonforest",
      "free_forest"
    )
}

# The non-forested share of natural land per cell, from the raw LUH2 states.
# Absent, every hectare of spare land counts as non-forested, which makes the
# forest fallback unreachable rather than silently permissive.
.plu_natural_split <- function(split, years) {
  if (!is.null(split)) {
    .plu_check_cols(split, c("lon", "lat", "nonforest_share"), "natural_split")
    return(dplyr::select(split, "lon", "lat", "nonforest_share"))
  }
  .plu_read_natural_split(years)
}

# WHEP's LUH2 reader collapses primf, secdf, primn and secdn into one `natural`
# class, so the forest split has to come from the raw states. Without it every
# hectare of spare land would count as non-forested, the forest fallback would
# be unreachable, and spillover would silently ignore the preference it
# documents. Returns an empty table when the states cannot be read, which makes
# the preference inert rather than wrong.
.plu_read_natural_split <- function(years) {
  states <- tryCatch(
    .luh2_read_states_source(years = years),
    error = function(e) NULL
  )
  if (is.null(states) || !rlang::has_name(states, "land_use")) {
    return(.plu_empty_natural_split())
  }
  states |>
    dplyr::filter(.data$land_use %in% .plu_natural_states()) |>
    dplyr::mutate(
      forested = .data$land_use %in% .plu_forest_states()
    ) |>
    dplyr::summarise(
      nonforest = sum(.data$fraction[!.data$forested], na.rm = TRUE),
      total = sum(.data$fraction, na.rm = TRUE),
      .by = c("lon", "lat")
    ) |>
    dplyr::transmute(
      .data$lon,
      .data$lat,
      nonforest_share = dplyr::if_else(
        .data$total > 0,
        .data$nonforest / .data$total,
        1
      )
    )
}

.plu_natural_states <- function() c("primf", "secdf", "primn", "secdn")

.plu_forest_states <- function() c("primf", "secdf")

.plu_empty_natural_split <- function() {
  tibble::tibble(
    lon = numeric(0),
    lat = numeric(0),
    nonforest_share = numeric(0)
  )
}

# Widen the ring until nothing is left to place or no capacity remains anywhere
# in the affected countries. Both bounds are needed: the first is the intent,
# the second is what guarantees the loop ends when a country is genuinely full.
.plu_spill_rings <- function(donors, receivers) {
  left <- dplyr::mutate(donors, left_ha = .data$donated_ha)
  free <- receivers
  received <- .plu_empty_received()
  ring <- 1L
  repeat {
    left <- dplyr::filter(left, .data$left_ha > 1e-9)
    # Capacity only counts if it sits in a country that still has demand:
    # checking the global pool would keep one full country searching for as
    # long as any OTHER country had spare land.
    free <- free |>
      dplyr::filter(.data$free_nonforest + .data$free_forest > 1e-9) |>
      dplyr::semi_join(dplyr::distinct(left, .data$area_code), by = "area_code")
    if (nrow(left) == 0L || nrow(free) == 0L || ring > .plu_max_ring()) {
      break
    }
    step <- .plu_spill_one_ring(left, free, ring)
    received <- dplyr::bind_rows(received, step$moved)
    left <- step$left
    free <- step$free
    ring <- ring + 1L
  }
  unplaced <- left |>
    dplyr::filter(.data$left_ha > 1e-9) |>
    dplyr::select(
      "polycell_id",
      "year",
      "land_use",
      unplaceable_statistical_ha = "left_ha"
    )
  .plu_warn_unplaced_spill(unplaced, ring)
  list(received = received, unplaced = unplaced)
}

# The documented contract: spillover that cannot place a hectare says so rather
# than quietly behaving like `cap`.
.plu_warn_unplaced_spill <- function(unplaced, ring) {
  if (nrow(unplaced) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_warn(c(
    "Spillover could not place {.val {sum(unplaced$unplaceable_statistical_ha)}}
     ha across {nrow(unplaced)} polycell-class{?es}.",
    i = "The search reached ring {ring}.",
    i = "It stays in {.field unplaceable_statistical_ha}; it is not capped."
  ))
  invisible(unplaced)
}

# An unconditional ceiling on the search. It has to be unconditional: a ring
# that keeps placing vanishing amounts would otherwise never satisfy a
# "moved nothing" exit and the loop would not terminate. 200 rings is 100
# degrees, far beyond the widest country and an order of magnitude past the
# 22 the 2020 build actually needed, so it binds only on a pathological input --
# and when it binds the remainder is reported rather than absorbed.
.plu_max_ring <- function() 200L

.plu_empty_received <- function() {
  tibble::tibble(
    polycell_id = character(0),
    year = integer(0),
    land_use = character(0),
    received_ha = numeric(0),
    ring = integer(0)
  )
}

# One ring, allocated in two passes: capacity is shared among the donors
# claiming it, then any donor granted more than it still owed returns the
# surplus. Without the second pass a donor claiming from several receivers in
# one ring is credited more than its demand.
.plu_spill_one_ring <- function(left, free, ring) {
  first <- .plu_allocate_pool(left, free, "free_nonforest", ring)
  second <- .plu_allocate_pool(
    first$left,
    first$free,
    "free_forest",
    ring
  )
  list(
    moved = dplyr::bind_rows(first$moved, second$moved),
    left = second$left,
    free = second$free
  )
}

# One ring against ONE capacity pool. Running the non-forested pool to
# exhaustion before the forest pool is what makes SP-2 real: it decides which
# RECEIVER absorbs the excess, not merely which of a receiver's own pools is
# debited first. Allocated in two passes -- capacity is shared among the donors
# claiming it, then a donor granted more than it still owed returns the surplus,
# because without that a donor claiming from several receivers in one ring is
# credited more than its demand.
.plu_allocate_pool <- function(left, free, pool, ring) {
  empty <- list(moved = .plu_empty_received()[0, ], left = left, free = free)
  usable <- dplyr::filter(free, .data[[pool]] > 1e-9)
  demand <- dplyr::filter(left, .data$left_ha > 1e-9)
  if (nrow(usable) == 0L || nrow(demand) == 0L) {
    return(empty)
  }
  pairs <- .plu_ring_pairs(demand, usable, ring, pool)
  if (nrow(pairs) == 0L) {
    return(empty)
  }
  granted <- pairs |>
    dplyr::mutate(
      demand_tot = sum(.data$left_ha),
      .by = c("r_polycell_id", "year")
    ) |>
    dplyr::mutate(
      offer = pmin(
        .data$left_ha,
        .data$capacity * .data$left_ha / .data$demand_tot
      )
    ) |>
    dplyr::mutate(
      offer_tot = sum(.data$offer),
      .by = c("polycell_id", "year", "land_use")
    ) |>
    dplyr::mutate(
      take = dplyr::if_else(
        .data$offer_tot > .data$left_ha,
        .data$offer * .data$left_ha / .data$offer_tot,
        .data$offer
      )
    ) |>
    dplyr::filter(.data$take > 1e-12)
  if (nrow(granted) == 0L) {
    return(empty)
  }
  list(
    moved = granted |>
      dplyr::summarise(
        received_ha = sum(.data$take),
        .by = c("r_polycell_id", "year", "land_use")
      ) |>
      dplyr::rename(polycell_id = "r_polycell_id") |>
      dplyr::mutate(ring = as.integer(ring)),
    left = .plu_debit_donors(left, granted),
    free = .plu_debit_receivers(free, granted, pool)
  )
}


# Same country, Chebyshev distance `ring` on the 0.5-degree grid.
.plu_ring_pairs <- function(left, free, ring, pool) {
  offsets <- .plu_ring_offsets(ring)
  left |>
    dplyr::mutate(k = 1L) |>
    dplyr::inner_join(
      dplyr::mutate(offsets, k = 1L),
      by = "k",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      t_lon = .data$lon + .data$d_lon * 0.5,
      t_lat = .data$lat + .data$d_lat * 0.5
    ) |>
    dplyr::inner_join(
      free |>
        dplyr::rename(
          r_polycell_id = "polycell_id",
          t_lon = "lon",
          t_lat = "lat"
        ),
      by = c("area_code", "year", "t_lon", "t_lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(capacity = .data[[pool]]) |>
    dplyr::select(
      "polycell_id",
      "year",
      "land_use",
      "left_ha",
      "r_polycell_id",
      "capacity"
    )
}

.plu_ring_offsets <- function(ring) {
  span <- seq.int(-ring, ring)
  tidyr::expand_grid(d_lon = span, d_lat = span) |>
    dplyr::filter(pmax(abs(.data$d_lon), abs(.data$d_lat)) == ring)
}

.plu_debit_donors <- function(left, granted) {
  left |>
    dplyr::left_join(
      granted |>
        dplyr::summarise(
          taken = sum(.data$take),
          .by = c("polycell_id", "year", "land_use")
        ),
      by = c("polycell_id", "year", "land_use")
    ) |>
    dplyr::mutate(
      left_ha = pmax(.data$left_ha - dplyr::coalesce(.data$taken, 0), 0)
    ) |>
    dplyr::select(-"taken")
}

# Debit the pool that was actually offered this pass.
.plu_debit_receivers <- function(free, granted, pool) {
  free |>
    dplyr::left_join(
      granted |>
        dplyr::summarise(
          used = sum(.data$take),
          .by = c("r_polycell_id", "year")
        ) |>
        dplyr::rename(polycell_id = "r_polycell_id"),
      by = c("polycell_id", "year")
    ) |>
    dplyr::mutate(
      used = dplyr::coalesce(.data$used, 0),
      !!pool := pmax(.data[[pool]] - .data$used, 0)
    ) |>
    dplyr::select(-"used")
}

.plu_apply_donor_excess <- function(allocated, donors) {
  allocated |>
    dplyr::left_join(
      dplyr::select(donors, "polycell_id", "year", "land_use", "donated_ha"),
      by = c("polycell_id", "year", "land_use")
    ) |>
    dplyr::mutate(
      area_ha = .data$area_ha - dplyr::coalesce(.data$donated_ha, 0)
    ) |>
    dplyr::select(-"donated_ha")
}

.plu_apply_received <- function(allocated, received) {
  if (nrow(received) == 0L) {
    return(dplyr::mutate(allocated, spillover_max_ring = NA_integer_))
  }
  allocated |>
    dplyr::left_join(
      received |>
        dplyr::summarise(
          received_ha = sum(.data$received_ha),
          spillover_max_ring = max(.data$ring),
          .by = c("polycell_id", "year", "land_use")
        ),
      by = c("polycell_id", "year", "land_use")
    ) |>
    dplyr::mutate(
      area_ha = .data$area_ha + dplyr::coalesce(.data$received_ha, 0)
    ) |>
    dplyr::select(-"received_ha")
}

# ---- Private helpers: shared -------------------------------------------------

# The reporting tail PR A resolves on the support. Carrying it through is what
# lets a consumer see that one `area_code` bucket can hold several polities
# (Sudan and South Sudan share 206), which a bare `area_code` hides.
.plu_polity_cols <- function() {
  c(
    "polity_area_code",
    "reporting_polity_code",
    "reporting_polity_name",
    "reporting_polity_has_geometry"
  )
}

# One FAOSTAT Land Use item, in hectares. Returns NULL when the input is not
# reachable, so the temporary-meadow split degrades to "not applied" and is
# visible in `level_source`, rather than silently changing the anchor.
.plu_read_landuse_item <- function(item_code, years) {
  raw <- tryCatch(
    .read_input("faostat-landuse", years = NULL),
    error = function(e) NULL
  )
  if (is.null(raw)) {
    return(NULL)
  }
  out <- tibble::as_tibble(raw)
  names(out) <- .plu_landuse_names(names(out))
  out |>
    dplyr::filter(
      .data$item_code == !!item_code,
      .data$element_code == 5110L
    ) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      meadow_ha = as.numeric(.data$value) * 1000
    ) |>
    .plu_filter_years(years)
}

.plu_landuse_names <- function(nms) {
  nms |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_remove("_$")
}

.plu_filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% as.integer(years))
}

.plu_check_cols <- function(x, required, arg) {
  missing <- required[!purrr::map_lgl(required, \(nm) rlang::has_name(x, nm))]
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    "{.arg {arg}} is missing column{?s}: {.field {missing}}."
  )
}
