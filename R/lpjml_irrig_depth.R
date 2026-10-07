# Annual irrigation depth per cell, crop and water regime, from LPJmL's
# per-crop monthly applied irrigation. It is the irrigation half of the water
# term WHEP puts into the IPCC 2019 wet/dry climate test, whose thresholds
# compare annual water with potential evapotranspiration (2019 Refinement,
# Vol. 4, Chapter 11, footnotes to Table 11.1, p. 11.13, and Table 11.3,
# p. 11.26). Counting the water applied to irrigated land as well as the
# precipitation is WHEP's choice; precipitation alone stays selectable in the
# classifier. Rainfed land receives no irrigation by definition of its regime.
#
# CONFIRMED LPJmL FACTS (run inspected 2026-10-05; do not re-guess):
# - cft_airrig_month.nc holds var "irrig", "CFT-specific applied irrigation",
#   mm/month, 32 bands named in NamePFT exactly as cftfrac.nc names them. It
#   is a density per square metre of the band's OWN STAND (the section "Per-CFT
#   cubes are per-STAND densities" of read_lpjml_hydrology()), so the twelve
#   months summed are the depth applied to the irrigated stand in a year, and
#   that depth times cftfrac is the same water spread over the whole cell.
# - Rainfed rice is watered: LPJmL books paddy water on the RAINFED rice band
#   (read_lpjml_hydrology(), section "Rainfed rice carries applied water").
#   This layer keeps that depth beside the irrigated one at the crop grain,
#   so it stays visible, and the regime rows give rainfed land zero, as the
#   regime defines it.
# - Every LPJmL output carries global attributes `source` (the model version,
#   "LPJmL C Version 6.1.1"), `GIT_hash` (the model source) and `history`
#   (the start time and configuration of the simulation). They are the run's
#   vintage, stamped on every row.
# - cft_airrig_month first exists in the run of 2026-08-27, whose cube put
#   every crop's water on one band; read_lpjml_hydrology() refuses that cube
#   (.hydro_check_band_spread()). The rerun started 2026-08-31 is the first
#   usable vintage (whep#916). Earlier runs have no cft_airrig_month.nc at all,
#   and there is no substitute for it: the crop-less mirrig.nc cannot say which
#   crop received the water.
# - Climate before 1901: the run's forcing starts in 1901 and LPJmL recycles
#   the 1901-1930 window for earlier years (see R/lpjml_regime_yield.R), so a
#   pre-1901 depth is the model's irrigation of a historical land-use state
#   under recycled weather, stamped "_recycled_climate".
# - Run dir from Sys.getenv("WHEP_LPJML_RUN_DIR"); never hardcode a path.

# Annual irrigation depth per cell, polity, commodity balance item, year and
# water regime: the table the wet/dry classifier adds to precipitation.
#
# `areas`: WHEP's own regime areas, the hectares the N chain splits
# (`lon`, `lat`, `area_code`, `item_prod_code`, `year`, `rainfed_ha`,
# `irrigated_ha`), as `.sci_engine_crops()` builds them. They give the rows
# (an irrigated row where the item's irrigated hectares are positive, a
# rainfed row where its rainfed hectares are) and the weights of the pooled
# items below. `years` defaults to the years `areas` carries.
#
# Returns `irrig_mm` (mm/yr per square metre of the irrigated stand; 0 on
# rainfed rows), `method_irrig_depth`, `lpjml_crop` (the LPJmL crops the depth
# comes from, "+"-joined) and the vintage stamps `lpjml_version`,
# `lpjml_git_hash` and `lpjml_run`. `method_irrig_depth` is one of:
# - "lpjml_airrig_stand": the item's own LPJmL crop;
# - "lpjml_crop_proxy": another named LPJmL crop stands in for the item, per
#   regime_yield_crop_mapping (fodder crops, for example);
# - "lpjml_others_proxy": the item sits on LPJmL's "others" stand, a
#   composite of minor crops, so its depth is that composite's;
# - "no_irrigated_stand": a production item irrigated in WHEP has no
#   irrigated stand of its LPJmL crop in the cell, so there is no depth
#   (`irrig_mm` NA);
# - "no_band": an irrigated production item has no LPJmL crop at all
#   (`irrig_mm` NA);
# - "rainfed_regime": a rainfed row, 0 by definition.
# The three LPJmL methods carry the suffix "_recycled_climate" before 1901.
#
# Where a commodity balance item pools production items on several LPJmL
# crops, the depth is the mean of their crops' depths weighted by WHEP's own
# irrigated hectares of each production item in the cell, the rule by which
# the regime split sums production items to the balance item. LPJmL's stand
# area is not a weight: it pools a whole LPJmL crop (all maize, say) into
# whichever minor item shares the crop.
.lpjml_irrig_depth <- function(
  areas,
  years = NULL,
  run_dir = NULL,
  data = NULL
) {
  areas <- .lid_check_areas(areas)
  years <- years %||% sort(unique(areas$year))
  areas <- dplyr::filter(areas, .data$year %in% years)
  crop <- .lid_crop_depth(years, run_dir, data)
  vintage <- .lid_vintage_of(crop)
  areas |>
    dplyr::filter(.data$irrigated_ha > 0) |>
    .lid_item_depth(crop = crop) |>
    .lid_cbs_depth() |>
    .lid_regime_rows(areas = areas, vintage = vintage)
}

# -- Crop grain ---------------------------------------------------------------

# Per cell, LPJmL crop and year: the annual applied depth on the irrigated
# and on the rainfed stand (`irrig_mm_irrigated`, `irrig_mm_rainfed`, mm/yr
# per square metre of that stand; NA where the stand is absent), both stand
# fractions of the cell, and the run's vintage stamps. This is the compact
# form of the layer (thirteen crops, "others" included, rather than one row
# per item), the grain a pinned copy holds.
#
# `data` (optional, in place of a run): `airrig_month` and `stand_frac` as
# read_lpjml_hydrology() returns "cft_airrig_month" and "stand_frac", and
# `vintage`, a one-row tibble with `lpjml_version`, `lpjml_git_hash` and
# `lpjml_run`.
.lid_crop_depth <- function(years = NULL, run_dir = NULL, data = NULL) {
  if (!is.null(data)) {
    vintage <- .lid_check_vintage(data$vintage)
    .hydro_check_band_spread(data$airrig_month, "cft_airrig_month")
    out <- .lid_band_depth(data$airrig_month, data$stand_frac, vintage)
    return(.filter_years_if_present(out, years))
  }
  run_dir <- .lid_run_dir_or_pin(run_dir)
  if (is.null(run_dir)) {
    return(.lid_read_pin(years))
  }
  vintage <- .lid_run_vintage(run_dir)
  years <- years %||% .lid_run_years(run_dir)
  # One year at a time: a year of cft_airrig_month is 7.7e7 long-form rows,
  # so the record cannot be held at once.
  purrr::map(
    as.integer(years),
    .lid_read_year,
    run_dir = run_dir,
    vintage = vintage
  ) |>
    dplyr::bind_rows()
}

.lid_read_year <- function(year, run_dir, vintage) {
  airrig <- read_lpjml_hydrology(
    "cft_airrig_month",
    run_dir = run_dir,
    years = year
  )
  stand_frac <- read_lpjml_hydrology(
    "stand_frac",
    run_dir = run_dir,
    years = year
  )
  .lid_band_depth(airrig, stand_frac, vintage)
}

# Join the monthly applied irrigation onto the stands with area (reusing
# build_crop_water_use()'s assembly, so the per-stand convention has one
# definition), sum the year, and spread the two regimes of each crop side by
# side. Bands are matched by NAME, never by position.
.lid_band_depth <- function(airrig, stand_frac, vintage) {
  bands <- .lrg_crop_bands(include_others = TRUE)
  .check_columns(airrig, c("band_name", "value"), "data$airrig_month")
  .check_columns(stand_frac, c("band_name", "value"), "data$stand_frac")
  .lid_check_bands(airrig$band_name, stand_frac$band_name, bands)
  stand_frac <- dplyr::filter(stand_frac, .data$band_name %in% bands$band_name)
  annual <- airrig |>
    dplyr::filter(.data$band_name %in% bands$band_name) |>
    .cwu_assemble(stand_frac) |>
    .lid_annual_depth()
  # Checked only over the years the irrigation cube covers: a supplied stand
  # table may carry more, and those years have nothing to be missing from.
  .lid_check_stands_watered(stand_frac, annual, unique(airrig$year))
  annual |>
    dplyr::inner_join(bands, by = "band_name") |>
    .lid_spread_regimes() |>
    dplyr::mutate(
      # Two-decimal centres, as the other grid layers key their cells; the
      # .25/.75 centres are exact in binary, so this never moves a cell.
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2),
      year = as.integer(.data$year),
      lpjml_version = vintage$lpjml_version,
      lpjml_git_hash = vintage$lpjml_git_hash,
      lpjml_run = vintage$lpjml_run
    ) |>
    ensure_columns(.lid_crop_prototype(), extra = "drop")
}

# The twelve monthly depths of each stand summed to the year. A stand short of
# a month would return eleven months' water as the year's, with nothing
# downstream able to tell, so it aborts here, where the gap is created.
.lid_annual_depth <- function(monthly) {
  out <- monthly |>
    dplyr::summarise(
      irrig_mm = sum(.data$airrig_stand_mm),
      stand_frac = dplyr::first(.data$stand_frac),
      n_months = dplyr::n_distinct(.data$month),
      n_rows = dplyr::n(),
      .by = c("lon", "lat", "year", "band_name")
    )
  short <- out$n_months != 12L | out$n_rows != 12L
  if (any(short)) {
    cli::cli_abort(
      c(
        "{sum(short)} crop stand{?s} do{?es/} not carry exactly twelve finite
         monthly values of {.field cft_airrig_month}.",
        i = "An annual depth summed over fewer months would be too small, and
             nothing downstream could tell."
      ),
      class = "whep_lpjml_irrig_depth_months"
    )
  }
  dplyr::select(out, -"n_months", -"n_rows")
}

# A stand with area and no applied-irrigation row at all means the two cubes
# do not describe the same cells; refuse rather than leave the crop dry.
.lid_check_stands_watered <- function(stand_frac, annual, years) {
  stands <- stand_frac |>
    dplyr::filter(
      .data$year %in% years,
      is.finite(.data$value),
      .data$value > 0
    ) |>
    dplyr::select("lon", "lat", "year", "band_name") |>
    dplyr::distinct()
  dry <- dplyr::anti_join(
    stands,
    annual,
    by = c("lon", "lat", "year", "band_name")
  )
  if (nrow(dry) == 0L) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "{nrow(dry)} crop stand{?s} with area in {.file cftfrac.nc} ha{?s/ve} no
       applied irrigation in {.file cft_airrig_month.nc}.",
      i = "The two outputs should cover the same cells; check they come from
           the same run."
    ),
    class = "whep_lpjml_irrig_depth_gap"
  )
}

# Every crop band must be present, by name, in both cubes, so a run with a
# different band set fails here instead of leaving a crop without a depth.
.lid_check_bands <- function(airrig_names, stand_names, bands) {
  missing_a <- setdiff(bands$band_name, airrig_names)
  missing_s <- setdiff(bands$band_name, stand_names)
  if (length(missing_a) == 0L && length(missing_s) == 0L) {
    return(invisible(TRUE))
  }
  cli::cli_abort(
    c(
      "The LPJmL cubes do not carry every crop band this layer needs.",
      x = "Absent from {.file cft_airrig_month.nc}: {.val {missing_a}}.",
      x = "Absent from {.file cftfrac.nc}: {.val {missing_s}}.",
      i = "Bands are matched by name against
           {.file inst/extdata/lpjml_cft_bands.csv}."
    ),
    class = "whep_lpjml_irrig_depth_bands"
  )
}

# One row per cell, crop and year with both regimes side by side; a regime
# with no stand in the cell has no row going in and comes out NA.
.lid_spread_regimes <- function(annual) {
  annual |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "lpjml_crop",
      "regime",
      "irrig_mm",
      "stand_frac"
    ) |>
    tidyr::pivot_wider(
      names_from = "regime",
      values_from = c("irrig_mm", "stand_frac"),
      names_glue = "{.value}_{regime}"
    )
}

.lid_crop_prototype <- function() {
  tibble::tibble(
    lon = double(),
    lat = double(),
    year = integer(),
    lpjml_crop = character(),
    irrig_mm_irrigated = double(),
    irrig_mm_rainfed = double(),
    stand_frac_irrigated = double(),
    stand_frac_rainfed = double(),
    lpjml_version = character(),
    lpjml_git_hash = character(),
    lpjml_run = character()
  )
}

# -- Items --------------------------------------------------------------------

# Production items -> LPJmL crop and the depth's method, from the mapping the
# regime yield ratio uses (regime_yield_crop_mapping, `lpjml_cft` and
# `lpjml_basis`).
.lid_item_bands <- function() {
  mapping <- whep::regime_yield_crop_mapping
  odd <- setdiff(mapping$lpjml_basis, .lid_known_basis())
  if (length(odd) > 0L) {
    cli::cli_abort(
      "Unknown {.field lpjml_basis} {.val {odd}} in
       {.code regime_yield_crop_mapping}.",
      class = "whep_lpjml_irrig_depth_items"
    )
  }
  mapping |>
    dplyr::transmute(
      item_prod_code = as.integer(.data$item_prod_code),
      lpjml_crop = stringr::str_replace_all(.data$lpjml_cft, "_", " "),
      method_irrig_depth = .lid_item_method(.data$lpjml_crop, .data$lpjml_basis)
    )
}

.lid_known_basis <- function() {
  c("direct", "direct_aggregate", "proxy", "group_proxy")
}

# The item's own crop, another named crop standing in for it, or LPJmL's
# composite "others" stand.
.lid_item_method <- function(lpjml_crop, lpjml_basis) {
  dplyr::case_when(
    lpjml_crop == "others" ~ "lpjml_others_proxy",
    lpjml_basis %in% c("direct", "direct_aggregate") ~ "lpjml_airrig_stand",
    .default = "lpjml_crop_proxy"
  )
}

# Methods from most to least direct. A commodity balance item pooling crops of
# different standing carries the least direct of them, so a proxy is never
# relabelled as the crop's own depth.
.lid_method_rank <- function() {
  c(.lid_lpjml_methods(), "no_irrigated_stand", "no_band")
}

# The methods whose depth LPJmL computed.
.lid_lpjml_methods <- function() {
  c("lpjml_airrig_stand", "lpjml_crop_proxy", "lpjml_others_proxy")
}

# Each production item's commodity balance item.
.lid_prod_cbs <- function() {
  whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      item_cbs_code = .as_integer_quiet(.data$item_cbs_code)
    ) |>
    dplyr::filter(!is.na(.data$item_prod_code)) |>
    dplyr::distinct()
}

# WHEP's regime areas normalised the way the other grid layers key their rows
# (two-decimal centres, integer codes), summed to one row per cell, polity,
# production item and year, with each item's commodity balance item. A
# production item with no balance item (flowers) has no balance row to carry a
# depth, so it leaves here, counted.
.lid_check_areas <- function(areas) {
  .check_columns(areas, .lid_area_cols(), "regime_areas")
  key <- c("lon", "lat", "area_code", "item_prod_code", "year")
  out <- tibble::as_tibble(areas) |>
    dplyr::transmute(
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2),
      area_code = as.integer(.data$area_code),
      item_prod_code = .as_integer_quiet(.data$item_prod_code),
      year = as.integer(.data$year),
      rainfed_ha = as.numeric(.data$rainfed_ha),
      irrigated_ha = as.numeric(.data$irrigated_ha)
    )
  .lid_check_area_values(out)
  out |>
    dplyr::summarise(
      rainfed_ha = sum(.data$rainfed_ha),
      irrigated_ha = sum(.data$irrigated_ha),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::left_join(
      .lid_prod_cbs(),
      by = "item_prod_code",
      relationship = "many-to-one"
    ) |>
    .lid_drop_unbalanced()
}

.lid_area_cols <- function() {
  c(
    "lon",
    "lat",
    "area_code",
    "item_prod_code",
    "year",
    "rainfed_ha",
    "irrigated_ha"
  )
}

# A missing key or hectare cannot be told from an absent crop, and a negative
# hectare is not an area: the regime areas are both the rows and the weights,
# so neither is repaired here.
.lid_check_area_values <- function(areas) {
  bad <- !stats::complete.cases(areas) |
    areas$rainfed_ha < 0 |
    areas$irrigated_ha < 0
  bad <- bad %in% TRUE
  if (any(bad)) {
    cols <- .lid_area_cols()
    cli::cli_abort(
      c(
        "{sum(bad)} row{?s} of the regime areas carr{?ies/y} a missing key, a
         missing hectare or a negative hectare.",
        i = "Each row needs {.field {cols}}, with non-negative hectares."
      ),
      class = "whep_lpjml_irrig_depth_areas"
    )
  }
}

.lid_drop_unbalanced <- function(areas) {
  orphan <- is.na(areas$item_cbs_code)
  if (any(orphan)) {
    codes <- sort(unique(areas$item_prod_code[orphan]))
    hectares <- signif(
      sum(areas$rainfed_ha[orphan] + areas$irrigated_ha[orphan]),
      3
    )
    cli::cli_inform(
      "Production item{?s} {.val {codes}} ha{?s/ve} no commodity balance item
       to carry a depth; {.val {hectares}} ha left out.",
      class = "whep_lpjml_irrig_depth_unbalanced"
    )
  }
  areas[!orphan, ]
}

# Per production item row of `items` (WHEP's regime areas; other columns ride
# along): the irrigated depth of the item's LPJmL crop in the cell that year.
# An item with no LPJmL crop is "no_band" and one whose crop has no irrigated
# stand in the cell "no_irrigated_stand"; neither has a depth (NA).
.lid_item_depth <- function(items, crop) {
  stands <- dplyr::select(
    crop,
    "lon",
    "lat",
    "year",
    "lpjml_crop",
    "irrig_mm_irrigated",
    "stand_frac_irrigated"
  )
  items |>
    dplyr::left_join(
      .lid_item_bands(),
      by = "item_prod_code",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      stands,
      by = c("lon", "lat", "year", "lpjml_crop"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      watered = .lid_has_irrigated_stand(
        .data$stand_frac_irrigated,
        .data$irrig_mm_irrigated
      ),
      irrig_mm = dplyr::if_else(.data$watered, .data$irrig_mm_irrigated, NA),
      method_irrig_depth = dplyr::case_when(
        is.na(.data$lpjml_crop) ~ "no_band",
        !.data$watered ~ "no_irrigated_stand",
        .default = .data$method_irrig_depth
      )
    ) |>
    dplyr::select(-"irrig_mm_irrigated", -"watered")
}

# A crop has an irrigated depth in a cell only where its irrigated stand has
# area; a zero-area stand has no depth to report, which is not a zero depth.
.lid_has_irrigated_stand <- function(stand_frac, irrig_mm) {
  is.finite(stand_frac) & stand_frac > 0 & is.finite(irrig_mm)
}

# Per cell, polity, commodity balance item and year: the depth of the item's
# irrigated production items, weighted by WHEP's own irrigated hectares of
# each. A balance item resting on one LPJmL crop takes that crop's depth
# unchanged, however many production items share it. One with any irrigated
# production item lacking a depth gets none (NA), labelled with the least
# direct method among them, so a missing stand is never averaged away.
.lid_cbs_depth <- function(items) {
  .lid_check_weights(items)
  key <- intersect(
    c("lon", "lat", "area_code", "item_cbs_code", "year"),
    names(items)
  )
  items |>
    dplyr::filter(.data$irrigated_ha > 0) |>
    dplyr::mutate(
      rank = match(.data$method_irrig_depth, .lid_method_rank())
    ) |>
    dplyr::arrange(.data$lpjml_crop) |>
    dplyr::summarise(
      irrig_mm = sum(.data$irrig_mm * .data$irrigated_ha) /
        sum(.data$irrigated_ha),
      rank = max(.data$rank),
      lpjml_crop = paste(
        unique(.data$lpjml_crop[!is.na(.data$lpjml_crop)]),
        collapse = "+"
      ),
      .by = dplyr::all_of(key)
    ) |>
    dplyr::mutate(
      method_irrig_depth = .lid_method_rank()[.data$rank],
      lpjml_crop = dplyr::na_if(.data$lpjml_crop, "")
    ) |>
    dplyr::select(-"rank")
}

# The weights are WHEP's own irrigated hectares per production item; nothing
# else, LPJmL's stand area included, may stand in for them.
.lid_check_weights <- function(items) {
  has_weights <- rlang::has_name(items, "irrigated_ha") &&
    !anyNA(items$irrigated_ha)
  if (!has_weights) {
    cli::cli_abort(
      c(
        "Pooling production items into a commodity balance item needs WHEP's
         irrigated hectares of each, in {.field irrigated_ha}, on every row.",
        i = "LPJmL's stand area is not a weight: it pools a whole LPJmL crop
             into whichever minor item shares it."
      ),
      class = "whep_lpjml_irrig_depth_weights"
    )
  }
}

# -- Regimes ------------------------------------------------------------------

# One row per cell, polity, balance item, year and regime of the regime areas:
# an irrigated row where the balance item's irrigated hectares are positive,
# carrying the depth; a rainfed row where its rainfed hectares are, carrying 0
# by definition of the regime.
.lid_regime_rows <- function(cbs, areas, vintage) {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  keys <- .lid_regime_keys(areas)
  irrigated <- keys |>
    dplyr::filter(.data$water_regime == "irrigated") |>
    dplyr::left_join(cbs, by = key, relationship = "one-to-one") |>
    dplyr::mutate(
      method_irrig_depth = .lid_climate_stamp(
        .data$method_irrig_depth,
        .data$year
      )
    )
  rainfed <- keys |>
    dplyr::filter(.data$water_regime == "rainfed") |>
    dplyr::mutate(irrig_mm = 0, method_irrig_depth = "rainfed_regime")
  dplyr::bind_rows(irrigated, rainfed) |>
    dplyr::mutate(
      lpjml_version = vintage$lpjml_version,
      lpjml_git_hash = vintage$lpjml_git_hash,
      lpjml_run = vintage$lpjml_run
    ) |>
    dplyr::arrange(.data$year, .data$lon, .data$lat, .data$item_cbs_code)
}

# The rows of the regime areas at the balance item grain: per cell, polity,
# balance item and year, one row per regime with positive hectares.
.lid_regime_keys <- function(areas) {
  key <- c("lon", "lat", "area_code", "item_cbs_code", "year")
  areas |>
    dplyr::summarise(
      irrigated = sum(.data$irrigated_ha),
      rainfed = sum(.data$rainfed_ha),
      .by = dplyr::all_of(key)
    ) |>
    tidyr::pivot_longer(
      c("irrigated", "rainfed"),
      names_to = "water_regime",
      values_to = "hectares"
    ) |>
    dplyr::filter(.data$hectares > 0) |>
    dplyr::select(-"hectares")
}

# A depth LPJmL computed under the recycled 1901-1930 weather says so.
.lid_climate_stamp <- function(method, year) {
  lpjml <- method %in% .lid_lpjml_methods()
  dplyr::if_else(
    lpjml & year < .lrg_climate_first_year(),
    paste0(method, "_recycled_climate"),
    method
  )
}

# -- Vintage ------------------------------------------------------------------

.lid_vintage_cols <- function() {
  c("lpjml_version", "lpjml_git_hash", "lpjml_run")
}

.lid_check_vintage <- function(vintage) {
  cols <- .lid_vintage_cols()
  if (is.null(vintage)) {
    cli::cli_abort(
      c(
        "{.code data$vintage} is missing.",
        i = "Supplied cubes need the run they came from: a one-row tibble
             with {.field {cols}}."
      ),
      class = "whep_lpjml_irrig_depth_vintage"
    )
  }
  .check_columns(vintage, cols, "data$vintage")
  .lid_vintage_of(tibble::as_tibble(vintage))
}

# The one vintage a crop table carries. A table mixing runs cannot be stamped
# with any one of them.
.lid_vintage_of <- function(crop) {
  vintage <- dplyr::distinct(crop[.lid_vintage_cols()])
  if (nrow(vintage) != 1L) {
    cli::cli_abort(
      c(
        "The irrigation depth layer carries {nrow(vintage)} LPJmL vintages;
         it must carry exactly one.",
        i = "No vintage means no crop stand in the years asked for; several
             mean rows from different runs were combined."
      ),
      class = "whep_lpjml_irrig_depth_vintage"
    )
  }
  vintage
}

# The run's vintage from the two cubes' own global attributes. Both must come
# from the same simulation; a cube copied in from another run would otherwise
# be weighted by stands it was never computed on.
.lid_run_vintage <- function(run_dir) {
  files <- c("cft_airrig_month.nc", "cftfrac.nc")
  attrs <- purrr::map(file.path(run_dir, files), .lid_file_attrs)
  if (!identical(attrs[[1L]], attrs[[2L]])) {
    cli::cli_abort(
      c(
        "{.file {files[[1L]]}} and {.file {files[[2L]]}} come from different
         LPJmL simulations.",
        i = "Their {.field source}, {.field GIT_hash} or {.field history}
             attributes differ. Run directory: {.file {run_dir}}."
      ),
      class = "whep_lpjml_irrig_depth_vintage"
    )
  }
  tibble::tibble(
    lpjml_version = attrs[[1L]]$source,
    lpjml_git_hash = attrs[[1L]]$GIT_hash,
    lpjml_run = .lid_run_label(run_dir, attrs[[1L]]$history)
  )
}

.lid_file_attrs <- function(path) {
  rlang::check_installed("ncdf4")
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  out <- purrr::map(
    c(source = "source", GIT_hash = "GIT_hash", history = "history"),
    .lid_attr,
    attrs = ncdf4::ncatt_get(nc, 0)
  )
  if (is.na(out$source)) {
    cli::cli_abort(
      c(
        "{.file {basename(path)}} carries no {.field source} attribute, so the
         LPJmL version that wrote it is unknown.",
        i = "Every LPJmL output stamps it; this file may not come from a
             finished run."
      ),
      class = "whep_lpjml_irrig_depth_vintage"
    )
  }
  out
}

# One global attribute as a string; NA when the file does not carry it.
.lid_attr <- function(name, attrs) {
  as.character(attrs[[name]] %||% NA_character_)
}

# The run's name (the directory above `output/` in an LPJmL run layout, plus
# the simulation's own folder) and the time the simulation started, from the
# `history` attribute ("Mon Aug 31 14:13:10 2026: ./bin/lpjml ...").
.lid_run_label <- function(run_dir, history) {
  path <- normalizePath(run_dir, winslash = "/", mustWork = FALSE)
  parent <- dirname(path)
  name <- if (identical(basename(parent), "output")) {
    paste0(basename(dirname(parent)), "/", basename(path))
  } else {
    basename(path)
  }
  started <- stringr::str_match(history, "^(.+?\\d{4}):")[, 2]
  if (is.na(started)) {
    return(name)
  }
  paste0(name, ", started ", started)
}

# -- Run or pin ---------------------------------------------------------------

.lid_pin_alias <- function() {
  "lpjml-crop-irrig-depth"
}

# The run to derive the layer from: an explicit `run_dir`, else
# `WHEP_LPJML_RUN_DIR`, else NULL, meaning the pinned layer is read instead.
# A run that is named but too old still aborts: it is never swapped for the
# pin.
.lid_run_dir_or_pin <- function(run_dir) {
  if (is.null(run_dir) && !.has_path(Sys.getenv("WHEP_LPJML_RUN_DIR"))) {
    return(NULL)
  }
  .lid_resolve_run_dir(run_dir)
}

# Detects a run that predates the per-crop monthly applied irrigation the way
# the other LPJmL readers detect a missing output: by the file. The defective
# 2026-08-27 cube that does exist is refused on read, by its single band.
.lid_resolve_run_dir <- function(run_dir) {
  resolved <- run_dir %||% Sys.getenv("WHEP_LPJML_RUN_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(
      c(
        "No LPJmL run to derive the irrigation depth from.",
        i = "Pass {.arg run_dir} or set {.envvar WHEP_LPJML_RUN_DIR} to a
             finished LPJmL run's output directory."
      ),
      class = "whep_lpjml_irrig_depth_no_run"
    )
  }
  if (!file.exists(file.path(resolved, "cft_airrig_month.nc"))) {
    cli::cli_abort(
      c(
        "The LPJmL run has no {.file cft_airrig_month.nc}.",
        x = "Runs before whep#916 do not write per-crop monthly applied
             irrigation, and nothing else says which crop received the water.",
        i = "Run directory: {.file {resolved}}.",
        i = "Use a run that writes {.file cft_airrig_month.nc} (LPJmL runs
             from 2026-08-31 on)."
      ),
      class = "whep_lpjml_irrig_depth_vintage"
    )
  }
  if (!file.exists(file.path(resolved, "cftfrac.nc"))) {
    cli::cli_abort(
      c(
        "The LPJmL run has no {.file cftfrac.nc}.",
        i = "Run directory: {.file {resolved}}."
      ),
      class = "whep_lpjml_irrig_depth_no_run"
    )
  }
  resolved
}

# The pinned crop-grain layer, once published. Until it is registered in
# whep_inputs.csv, there is nothing to read, and the way out is a run.
.lid_read_pin <- function(years = NULL) {
  alias <- .lid_pin_alias()
  if (!.lid_pin_registered()) {
    cli::cli_abort(
      c(
        "The {.val {alias}} pin has not been published yet.",
        i = "Derive the irrigation depth from a run instead: pass
             {.arg run_dir} or set {.envvar WHEP_LPJML_RUN_DIR} to a finished
             LPJmL run that writes {.file cft_airrig_month.nc}."
      ),
      class = "whep_lpjml_irrig_depth_no_pin"
    )
  }
  .read_lpjml_pin(alias, years = years) |>
    tibble::as_tibble() |>
    .filter_years_if_present(years)
}

.lid_pin_registered <- function() {
  .lid_pin_alias() %in% whep::whep_inputs$alias
}

# Every calendar year cft_airrig_month.nc carries, from its own time axis.
.lid_run_years <- function(run_dir) {
  rlang::check_installed("ncdf4")
  nc <- ncdf4::nc_open(file.path(run_dir, "cft_airrig_month.nc"))
  on.exit(ncdf4::nc_close(nc))
  first_year <- .lpjml_resolve_first_year(nc, NULL, "cft_airrig_month.nc")
  first_year + seq_len(nc$dim[["time"]]$len %/% 12L) - 1L
}
