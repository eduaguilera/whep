# IPCC 2019 Tier 1 wet/dry climate classes for the soil N2O, indirect N2O and
# leaching factors.
#
# SOURCE FACTS (IPCC 2019 Refinement to the 2006 IPCC Guidelines, Volume 4):
# - Chapter 11, Table 11.1, footnote 4 (p. 11.13): wet climates are temperate
#   and boreal zones where annual precipitation divided by potential
#   evapotranspiration exceeds 1, and tropical zones where annual precipitation
#   exceeds 1000 mm; dry climates are the converse. Both tests are strict, so a
#   ratio of exactly 1 and exactly 1000 mm are dry. The footnote bases the
#   disaggregation on long-term averages, which is why the default window
#   averages 30 years before classifying.
# - Chapter 3, Figure 3A.5.2 (p. 3.48): a zone is tropical when mean annual
#   temperature exceeds 18 degC AND there are at most 7 frost days per year.
#   The manure "Warm" zone (`.climate_zone_from_mat()`, > 18 degC) has no frost
#   test, so it cannot stand in for "tropical" and is not used here.
#
# `method_moisture_regime` vocabulary. Fields are joined by ";", in this order:
# 1. `ipcc2019_wet_dry`: the rule above.
# 2. The window: `annual` (the year itself), `trailing_30` (the mean of years
#    y-29..y) or `normal_<first>_<last>` (the mean of one fixed period, the
#    same for every year).
# 3. The water term: `precip` (precipitation, the default) or `precip_irrig`
#    (precipitation plus the irrigation depth of the row's crop and regime).
#    The default follows footnote 4, which defines the classes on
#    precipitation only; Annex 11A.2 (p. 11.30) says 63% of the dry-climate
#    EF1 cases were irrigated, so the dry factors already price irrigated dry
#    land and `precip_irrig` would price irrigation twice (#1531). This
#    default is provisional (wiki decision
#    whep-wet-dry-class-on-precipitation-only): IPCC 2019 is its one
#    independent source, since Cayuela et al. (2017), which agrees (irrigated
#    Mediterranean EF 0.63%), is among the databases the IPCC EF1 was fitted
#    on (Annex 11A.2, p. 11.29).
# 4. Optional, one per side of the record a window reaches past:
#    `climatology_<first>_<last>_<n>of<m>`, meaning n of the window's m years
#    lie outside the climate record and took the held climatology of years
#    first-last (the first 30 years of the record before it, the last 30
#    after it), the rule `build_cell_climate_zone()` follows.
# 5. Optional, only on an irrigated row classified from a separate irrigation
#    record: `irrig_years_<n>of<m>`, meaning the row's land was irrigated in
#    n of the window's m years, and its irrigation is the mean of those n
#    years. Climate is still the mean of all m.
# 5b. Optional, only on a row whose cell lacks CRU climate variables (all four
#    off CRU's land mask, fewer where CRU maps the cell for some only):
#    `nearest_cru_cell=<col>[+<col>...]`, naming the variables the cell took
#    from the climate record of its nearest CRU land cell, `climate_fill_km`
#    away; the cell's other variables are its own. build_n_loss_drivers() adds
#    `cell_rainfed_class=<reason>` here on a balance row with no class of its
#    own.
# 6. Optional, only on an unclassified row (`moisture_regime` is NA):
#    `missing=<col>[+<col>...]`, the inputs whose window mean is unavailable
#    because at least one window year has no value or no row; and/or
#    `pet_not_positive`, a window-mean PET of zero, for which the ratio is
#    undefined. Nothing is zero-filled.

#' Build the IPCC 2019 wet/dry moisture regime of WHEP's crop land.
#'
#' @description
#' Classify each cell, polity, commodity balance item, water regime and year
#' of WHEP's own crop land use as IPCC 2019 wet or dry, the classes the Tier 1
#' managed-soil factors are keyed on.
#'
#' A zone is tropical where the mean annual temperature exceeds 18 degC and
#' there are at most 7 frost days per year (IPCC 2019 Refinement, Volume 4,
#' Chapter 3, Figure 3A.5.2, p. 3.48). A tropical row is wet where its annual
#' water exceeds 1000 mm; any other row is wet where its annual water divided
#' by potential evapotranspiration (PET) exceeds 1; every other row is dry.
#' Both tests are strict, so a value of exactly 1 or exactly 1000 mm is dry
#' (Volume 4, Chapter 11, Table 11.1, footnote 4, p. 11.13). Footnote 4 bases
#' the disaggregation on long-term averages, so by default the inputs are
#' averaged over the 30 years ending in each year before they are classified.
#'
#' The water term is precipitation alone (`water = "precip"`, the default),
#' as footnote 4 defines the classes, or precipitation plus, on irrigated
#' rows, the irrigation depth of the row's crops (`water = "precip_irrig"`).
#' Precipitation alone is the default because the IPCC dry factors already
#' price irrigated dry land: 63% of the dry-climate cases the dry EF1 was
#' fitted on were irrigated, and irrigated dry fields alone give an EF1 of
#' 0.004 (n = 94) against 0.005 for all dry and 0.014 for wet climates
#' (Volume 4, Chapter 11, Annex 11A.2, p. 11.30, and Table 2A.2, p. 11.31).
#' The leaching fraction treats "dry climate regions where irrigation ... is
#' used" as a dry-climate case of its own (p. 11.25). Counting irrigation as
#' precipitation would put irrigated dry land on the wet factors, pricing
#' irrigation twice (issue #1531). Rainfed rows get no irrigation in either
#' case, and only `"precip_irrig"` reads the LPJmL irrigation depth.
#'
#' @details
#' Inputs:
#'
#' - Climate comes from CRU TS: annual mean temperature, precipitation, PET
#'   and ground-frost days. CRU ships PET as a mean daily rate in mm per day,
#'   which is multiplied by the days of each month and summed over the year.
#'   CRU's frost count is synthetic: CRU derives it from the gridded minimum
#'   temperature. Years before the CRU record (1901) hold the 1901-1930
#'   climatology, and years after it hold the climatology of its last 30 years.
#'   A cell of WHEP's crop land that holds no value of a CRU variable in any
#'   year (all four for a cell off CRU's land mask, mostly coastal and island
#'   cells; fewer for a cell CRU maps for some variables only) takes that
#'   variable's whole record from its nearest CRU land cell by great-circle
#'   distance between cell centres, among the cells that hold it in every year
#'   read. One donor supplies every variable the cell lacks, and the cell
#'   keeps the variables it has; a variable with gaps is not lent but left to
#'   the classifier, which reports the years without it. The distance is
#'   `climate_fill_km`, and a message of class
#'   `whep_moisture_regime_nearest_climate` counts the cells by the variables
#'   they took. The cell's own water term, irrigation included under
#'   `water = "precip_irrig"`, is then applied.
#' - Irrigation depth (`water = "precip_irrig"` only) is LPJmL's applied
#'   irrigation per crop stand, pooled to the commodity balance item by
#'   WHEP's irrigated hectares. LPJmL's paddy
#'   water on its rainfed rice stand is not counted: rainfed rows get none.
#'
#' @param years Integer vector of the years to classify.
#' @param window How the inputs are averaged before they are classified:
#'   `"trailing_30"` (default, the 30 years ending in each year), `"annual"`
#'   or `"normal"` (one fixed period, `config$normal_period`).
#' @param water The water term: `"precip"` (default, precipitation alone, as
#'   IPCC defines the classes) or `"precip_irrig"` (precipitation plus the
#'   irrigation depth of the row's crops and regime).
#' @param data Optional list of inputs used in place of reading them. Each
#'   element is optional:
#'   - `climate`: `lon`, `lat`, `year`, `mean_annual_temp_c`,
#'     `annual_precip_mm`, `annual_pet_mm`, `annual_frost_days`; its years are
#'     the climate record.
#'   - `crop_depth`: the LPJmL irrigation depth per cell, crop and year
#'     (`lon`, `lat`, `year`, `lpjml_crop`, `irrig_mm_irrigated`,
#'     `irrig_mm_rainfed`, `stand_frac_irrigated`, `stand_frac_rainfed`,
#'     `lpjml_version`, `lpjml_git_hash`, `lpjml_run`).
#'   - `lpjml_years`: the years the LPJmL run covers (default: the years of
#'     `crop_depth`).
#'   - `lpjml_grid`: the cells (`lon`, `lat`) the LPJmL run's output covers.
#'     Needed with `crop_depth` when an irrigated row's crop has no
#'     irrigated LPJmL stand in its cell; otherwise read from the run.
#'   - `regime_areas`: WHEP's rainfed and irrigated hectares (`lon`, `lat`,
#'     `area_code`, `item_prod_code`, `year`, `rainfed_ha`, `irrigated_ha`).
#'   - `country_grid`: the cell support the regime areas are built on when
#'     they are not supplied.
#' @param config Optional list: `cru_dir` (default `WHEP_CRU_DIR`), `run_dir`
#'   (default `WHEP_LPJML_RUN_DIR`, else the pinned depth layer),
#'   `normal_period` (default `1991:2020`) and `max_rows` (default `2e6`, the
#'   classifier input rows per block of cells, about 1.3 KB each at peak).
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#'
#' @return A tibble with one row per `lon`, `lat`, `area_code`,
#'   `item_cbs_code`, `water_regime` and `year` where WHEP has hectares of
#'   that regime, carrying the window-mean `mat_c`, `frost_days`, `prec_mm`,
#'   `pet_mm` and `irrig_mm`, `water_mm`, `moisture_ratio`, `tropical`,
#'   `moisture_regime` (`"wet"`, `"dry"` or `NA`), `method_moisture_regime`,
#'   `climate_fill_km` (the distance to the CRU cell whose climate variables a
#'   cell lacking some took, `NA` for a cell with all of its own),
#'   `method_irrig_depth`, `lpjml_crop` and the LPJmL vintage columns
#'   `lpjml_version`, `lpjml_git_hash` and `lpjml_run`, plus the polity
#'   columns below.
#'
#'   An irrigated row's irrigation is averaged over the window years in which
#'   WHEP irrigates that land, its climate over the whole window; a row
#'   irrigated in fewer than all of them carries `irrig_years_<n>of<m>` in
#'   `method_moisture_regime`. Where WHEP irrigates a crop the LPJmL run has
#'   no irrigated stand of in the cell, in a year the run covers, the depth
#'   is the crop's LPJmL depth averaged over the polity's cells that have the
#'   stand, weighted by WHEP's irrigated hectares, and `method_irrig_depth`
#'   says why: `lpjml_landuse_vintage` for a cell on the LPJmL grid (the
#'   run's land-use forcing differs from WHEP's there) or `no_lpjml_cell`
#'   for one outside it, with the suffix `_world_mean` where the polity has
#'   no such stand and the world's mean is used. A message of class
#'   `whep_moisture_regime_stand_gap` counts these rows and hectares by
#'   cause; its `diagnosis` field lists every case.
#'
#' @section Irrigation depth stamps:
#' `method_irrig_depth` holds the least direct method over the window years
#' of the row's irrigation record, one of:
#'
#' - `lpjml_airrig_stand`: the item's own LPJmL crop has an irrigated stand
#'   in the cell.
#' - `lpjml_crop_proxy`: another named LPJmL crop stands in for the item, as
#'   mapped in `regime_yield_crop_mapping` (fodder crops, for example).
#' - `lpjml_others_proxy`: the item sits on LPJmL's composite "others" stand
#'   of minor crops.
#' - `lpjml_landuse_vintage`: WHEP irrigates the crop in a cell of the LPJmL
#'   grid where the run has no irrigated stand of it, because the run's
#'   land-use forcing predates WHEP's current land use. The depth is the
#'   crop's mean over the polity's cells that have the stand, weighted by
#'   WHEP's irrigated hectares.
#' - `no_lpjml_cell`: the same fill for a cell outside the LPJmL grid.
#' - `lpjml_landuse_vintage_world_mean` and `no_lpjml_cell_world_mean`: the
#'   two fills above where the polity has no such stand that year, so the
#'   world's mean over the cells that have it is used.
#' - `rainfed_regime`: a rainfed row, zero by definition.
#' - `NA`: `water = "precip"`, where there is no irrigation term.
#'
#' A stamp carries two optional additions. The suffix `_recycled_climate`
#' follows any of the seven LPJmL-derived methods above when a window year
#' precedes 1901, because LPJmL recycles the 1901-1930 weather before its
#' forcing starts. The field `;outside_run_<n>of<m>_<fill>` follows it when
#' `n` of the window's `m` years lie outside the LPJmL run: those years take
#' the mean of the series' own window years inside the run (`<fill>` is
#' `series_mean`) or, where it has none, its country's mean for the balance
#' item (`country_mean`).
#'
#' @section Moisture regime stamps:
#' `method_moisture_regime` joins these fields with `;`, in this order:
#'
#' - `ipcc2019_wet_dry`: the rule.
#' - The window: `annual`, `trailing_30` or `normal_<first>_<last>`.
#' - The water term: `precip` or `precip_irrig`.
#' - Optional `climatology_<first>_<last>_<n>of<m>`: `n` of the window's `m`
#'   years lie outside the CRU record and took the held climatology of the
#'   years `<first>` to `<last>`.
#' - Optional `irrig_years_<n>of<m>`: an irrigated row whose land is
#'   irrigated in `n` of the window's `m` years. Its irrigation is the mean of
#'   those `n` years and its climate the mean of all `m`.
#' - Optional `nearest_cru_cell=<col>[+<col>...]`: the cell lacks these CRU
#'   climate variables (`mat_c`, `frost_days`, `prec_mm`, `pet_mm`) and took
#'   them from its nearest CRU land cell, `climate_fill_km` away; its other
#'   variables are its own.
#' - Optional, on an unclassified row (`moisture_regime` is `NA`):
#'   `missing=<col>[+<col>...]`, the inputs whose window mean is unavailable,
#'   and `pet_not_positive`, a window-mean PET of zero. Nothing is
#'   zero-filled.
#'
#' @section Errors:
#' The function aborts, with these condition classes, when:
#'
#' - `whep_moisture_regime_years`: `years` is `NULL`, empty or has a missing
#'   value.
#' - `whep_moisture_regime_config`: `config` has an unknown entry, or
#'   `max_rows` is not one positive number.
#' - `whep_moisture_regime_period`: `normal_period` is not a run of
#'   consecutive whole years.
#' - `whep_moisture_regime_columns`, `whep_moisture_regime_empty`,
#'   `whep_moisture_regime_key`, `whep_moisture_regime_climate`: an input
#'   table lacks a required column, has no rows, has a duplicated or missing
#'   key, or carries more than one climate for a cell-year; or no cell of the
#'   climate holds, in every year read, a variable that a cell lacks.
#' - `whep_moisture_regime_domain`: an input holds a negative precipitation,
#'   PET, frost count or irrigation depth, more than 366 frost days, a
#'   `water_regime` other than `"rainfed"` or `"irrigated"`, or irrigation on
#'   a rainfed row.
#' - `whep_moisture_regime_lpjml_grid`: `data$crop_depth` has stand gaps but
#'   no `data$lpjml_grid` is supplied.
#' - `whep_moisture_regime_stand_mapping`: an irrigated production item has
#'   no LPJmL crop band.
#' - `whep_moisture_regime_stand_no_mean`: a crop has no irrigated LPJmL
#'   stand anywhere that year, so there is no depth to fill from.
#' - `whep_moisture_regime_outside_run`: an irrigated series-year outside the
#'   LPJmL run has no depth of its own or of its country to fill from.
#' - `whep_absent_input`: precipitation, PET, the irrigated hectares or the
#'   irrigation depth of the irrigated rows is absent, missing or all zero.
#' - `whep_lpjml_irrig_depth_*`: the LPJmL irrigation depth layer rejects the
#'   regime areas, the run or its output cubes, or the pinned layer.
#'
#' Messages of class `whep_moisture_regime_unclassified`,
#' `whep_moisture_regime_area_years` and
#' `whep_moisture_regime_nearest_climate` report rows left without a regime,
#' window years the regime areas do not cover and cells that took climate
#' variables from their nearest CRU cell.
#'
#' @source
#' IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
#' Greenhouse Gas Inventories, Volume 4: Chapter 3, Figure 3A.5.2 (p. 3.48);
#' Chapter 11, Table 11.1, footnote 4 (p. 11.13).
#'
#' CRU TS version 4.09 (files `cru_ts4.09.1901.2024.<var>.dat.nc`),
#' Climatic Research Unit, University of East Anglia.
#'
#' @inheritSection whep_polity_columns Polity columns
#' @export
#'
#' @examples
#' build_cell_moisture_regime(example = TRUE)
build_cell_moisture_regime <- function(
  years = NULL,
  window = c("trailing_30", "annual", "normal"),
  water = c("precip", "precip_irrig"),
  data = NULL,
  config = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_cell_moisture_regime())
  }
  spec <- .cmr_spec(
    years,
    rlang::arg_match(window),
    rlang::arg_match(water),
    config
  )
  areas <- .cmr_regime_areas(data, spec$needed)
  rows <- .lid_regime_keys(dplyr::filter(areas, .data$year %in% spec$years))
  irrigation <- if (spec$water == "precip_irrig") {
    .cmr_irrigation(areas, rows, data, spec)
  }
  climate <- .cmr_climate(
    data,
    spec,
    dplyr::distinct(rows, .data$lon, .data$lat)
  )
  .cmr_year_groups(spec$years, irrigation) |>
    purrr::map(\(group) {
      .cmr_classify_group(group, rows, climate, irrigation, spec)
    }) |>
    dplyr::bind_rows() |>
    .cmr_stamp_climate_fill(climate$fills) |>
    ensure_columns(.cmr_prototype(), extra = "drop") |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c("year", .cmr_row_key())))) |>
    .add_reporting_polity_columns() |>
    tibble::as_tibble() |>
    .cmr_report_unclassified()
}

# Classify rows into the IPCC 2019 wet/dry climate classes.
#
# `data` holds one row per `lon`, `lat`, `year` and, optionally,
# `item_cbs_code` and `water_regime` (`"rainfed"` or `"irrigated"`). It carries
# annual `mat_c` (mean annual temperature, degC), `frost_days` (days per year),
# `prec_mm` and `pet_mm` (annual totals, mm per year; CRU ships PET in mm per
# day, which must be converted before it reaches here) and, for
# `water = "precip_irrig"`, `irrig_mm` (annual irrigation depth of the row's
# crop and regime, mm per year). Climate is a property of the cell, so it must
# be identical on every row of one cell-year. The years present form the
# climate record.
#
# The inputs are averaged over the window first and the mean is classified.
# `years` are the years to classify (`NULL`: every year of the record).
# `normal_period` is used only by `window = "normal"`.
#
# `irrigation` (optional, `water = "precip_irrig"` only) holds the irrigation
# depth as a record of its own: the series key columns, `year` and `irrig_mm`,
# from a model run that covers every window year, years before the climate
# record included. `data` then carries no `irrig_mm`, and the irrigation
# window mean takes each window year's own depth: no climatology is held for
# it. The record holds a row for each year the series' land was irrigated, so
# a window year it lacks is a year that land was not irrigated: it has no
# irrigation to average, and the mean is taken over the years it does hold,
# their count stamped. A row it holds with a missing depth still gives a
# missing mean. Climate is averaged over the whole window regardless.
#
# Returns one row per series (`lon`, `lat` and the optional `area_code`,
# `item_cbs_code`, `water_regime`) and requested year, carrying the
# window-mean inputs that were classified, `water_mm`, `moisture_ratio`
# (water over PET), `tropical`, `moisture_regime` (`"wet"`, `"dry"` or NA) and
# `method_moisture_regime`. Other input columns are not carried.
.classify_moisture_regime <- function(
  data,
  years = NULL,
  window = c("trailing_30", "annual", "normal"),
  water = c("precip", "precip_irrig"),
  # 1991-2020 is an assumed choice, the most recent complete 30-year period;
  # no source used here prescribes it.
  normal_period = 1991:2020,
  irrigation = NULL
) {
  window <- rlang::arg_match(window)
  water <- rlang::arg_match(water)
  own <- water == "precip_irrig" && !is.null(irrigation)
  inputs <- .check_moisture_inputs(data, if (own) "precip" else water)
  if (own) {
    irrigation <- .check_moisture_irrigation(irrigation, inputs)
  }
  covered <- sort(unique(inputs$year))
  frame <- .moisture_frame(
    .resolve_climate_years(years, covered),
    window,
    normal_period,
    covered
  )
  .moisture_series_years(inputs, frame$years) |>
    .join_window_climate(inputs, frame) |>
    .join_window_irrigation(inputs, frame, water, irrigation) |>
    .apply_wet_dry(water) |>
    .report_unclassified() |>
    .stamp_moisture_regime(frame, water)
}

# Private helpers ----

# Length of the trailing window, in years.
.trailing_window_n <- 30L

.moisture_climate_cols <- function() {
  c("mat_c", "frost_days", "prec_mm", "pet_mm")
}

# A series is one cell, or one crop and regime within it when those columns
# are present.
.moisture_series_key <- function(data) {
  c(
    "lon",
    "lat",
    intersect(c("area_code", "item_cbs_code", "water_regime"), names(data))
  )
}

# The years whose inputs are averaged into `year`'s classification.
.moisture_window_years <- function(year, window, period) {
  switch(
    window,
    annual = year,
    trailing_30 = seq.int(year - .trailing_window_n + 1L, year),
    normal = period
  )
}

# Which years each window needs, and how they split into measured years and
# years held from a climatology. The split is `build_cell_climate_zone()`'s
# own plan, reused so the two never disagree on where the record ends or which
# 30 years stand in for the years beyond it.
.moisture_frame <- function(years, window, normal_period, covered) {
  period <- if (window == "normal") {
    .check_normal_period(normal_period)
  }
  needed <- switch(
    window,
    annual = years,
    trailing_30 = seq.int(min(years) - .trailing_window_n + 1L, max(years)),
    normal = period
  )
  list(
    window = window,
    years = years,
    period = period,
    covered = covered,
    needed = needed,
    plan = .climate_year_plan(needed, covered)
  )
}

.check_normal_period <- function(normal_period) {
  ok <- is.numeric(normal_period) &&
    length(normal_period) > 0L &&
    !anyNA(normal_period) &&
    all(normal_period == round(normal_period)) &&
    all(diff(normal_period) == 1)
  if (!ok) {
    cli::cli_abort(
      "{.arg normal_period} must be a run of consecutive years, such as
       {.code 1991:2020}.",
      class = "whep_moisture_regime_period"
    )
  }
  as.integer(normal_period)
}

# Validate the supplied table and return it as a data.table with integer years
# and double values.
.check_moisture_inputs <- function(data, water) {
  required <- c(
    "lon",
    "lat",
    "year",
    .moisture_climate_cols(),
    if (water == "precip_irrig") "irrig_mm"
  )
  .check_moisture_columns(data, required)
  key <- c(.moisture_series_key(data), "year")
  values <- setdiff(required, c("lon", "lat", "year"))
  inputs <- data.table::as.data.table(data)[,
    unique(c(key, values)),
    with = FALSE
  ]
  inputs[, year := as.integer(year)]
  inputs[, (values) := lapply(.SD, as.double), .SDcols = values]
  .check_moisture_key(inputs, key)
  .check_moisture_domain(inputs)
  .check_moisture_supplied(inputs, water)
  inputs
}

# Validate a separate irrigation record against the series of `inputs` and
# return it as a data.table, under the same domain and supply rules as an
# `irrig_mm` column of `data`.
.check_moisture_irrigation <- function(irrigation, inputs) {
  key <- c(.moisture_series_key(inputs), "year")
  .check_moisture_columns(irrigation, c(key, "irrig_mm"))
  irrigation <- data.table::as.data.table(irrigation)[,
    c(key, "irrig_mm"),
    with = FALSE
  ]
  irrigation[, year := as.integer(year)]
  irrigation[, irrig_mm := as.double(irrig_mm)]
  .check_moisture_key(irrigation, key)
  .check_moisture_domain(irrigation)
  irrigated <- if (rlang::has_name(irrigation, "water_regime")) {
    irrigation[water_regime == "irrigated"]
  } else {
    irrigation
  }
  if (nrow(irrigated) > 0L) {
    check_inputs_supplied(
      irrigated,
      "irrig_mm",
      details = c(
        i = "Every irrigated row has zero or missing irrigation depth."
      )
    )
  }
  irrigation
}

.check_moisture_columns <- function(data, required) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{.arg data} is missing required column{?s}: {.field {missing}}.",
        i = if ("irrig_mm" %in% missing) {
          "Use {.code water = \"precip\"} to classify on precipitation alone."
        }
      ),
      class = "whep_moisture_regime_columns"
    )
  }
  if (nrow(data) == 0L) {
    cli::cli_abort(
      "{.arg data} has no rows, so there is no climate record to classify.",
      class = "whep_moisture_regime_empty"
    )
  }
}

.check_moisture_key <- function(inputs, key) {
  n_missing <- sum(!stats::complete.cases(inputs[, key, with = FALSE]))
  n_duplicated <- sum(duplicated(inputs, by = key))
  if (n_missing + n_duplicated > 0L) {
    cli::cli_abort(
      c(
        "{.arg data} must hold one row per {.field {key}}.",
        i = "Found {n_duplicated} duplicated key{?s} and {n_missing} row{?s}
             with a missing key value."
      ),
      class = "whep_moisture_regime_key"
    )
  }
}

# A negative depth or count, a frost count above a year's length, or a regime
# label outside the vocabulary is a corrupted input: no row can be classified
# from it, so it aborts rather than being stamped.
.check_moisture_domain <- function(inputs) {
  counts <- intersect(
    c("frost_days", "prec_mm", "pet_mm", "irrig_mm"),
    names(inputs)
  )
  negative <- counts[purrr::map_lgl(counts, \(col) {
    any(inputs[[col]] < 0, na.rm = TRUE)
  })]
  .abort_moisture_domain(negative, "{.field {bad}} must not be negative.")
  long_year <- if (any(inputs$frost_days > 366, na.rm = TRUE)) "frost_days"
  .abort_moisture_domain(
    long_year,
    "{.field {bad}} counts days per year, so it cannot exceed 366."
  )
  if (rlang::has_name(inputs, "water_regime")) {
    .check_moisture_regimes(inputs)
  }
}

.check_moisture_regimes <- function(inputs) {
  labels <- setdiff(unique(inputs$water_regime), c("rainfed", "irrigated"))
  .abort_moisture_domain(
    labels,
    "{.field water_regime} must be {.val rainfed} or {.val irrigated}, not
     {.val {bad}}."
  )
  if (rlang::has_name(inputs, "irrig_mm")) {
    rainfed <- inputs$water_regime == "rainfed"
    watered <- if (any(inputs$irrig_mm[rainfed] != 0, na.rm = TRUE)) "irrig_mm"
    .abort_moisture_domain(
      watered,
      "{.field {bad}} must be zero on rainfed rows: rainfed land receives no
       irrigation."
    )
  }
}

.abort_moisture_domain <- function(bad, message) {
  if (length(bad) > 0L) {
    cli::cli_abort(message, class = "whep_moisture_regime_domain")
  }
}

# An identically zero PET or precipitation column, or a zero irrigation layer
# on every irrigated row, is an input that never arrived. A zero PET would
# otherwise classify every cell wet through a division by zero.
.check_moisture_supplied <- function(inputs, water) {
  check_inputs_supplied(
    inputs,
    c("prec_mm", "pet_mm"),
    details = c(i = "Supply annual totals in mm per year.")
  )
  if (water == "precip_irrig" && rlang::has_name(inputs, "water_regime")) {
    irrigated <- inputs[water_regime == "irrigated"]
    if (nrow(irrigated) > 0L) {
      check_inputs_supplied(
        irrigated,
        "irrig_mm",
        details = c(
          i = "Every irrigated row has zero or missing irrigation depth."
        )
      )
    }
  }
}

# Every series crossed with every requested year: the output rows.
.moisture_series_years <- function(inputs, years) {
  key <- .moisture_series_key(inputs)
  unique(inputs[, key, with = FALSE]) |>
    .cross_years(years)
}

# Repeat each row of `dt` once per year, adding a `year` column.
.cross_years <- function(dt, years) {
  out <- dt[rep(seq_len(nrow(dt)), times = length(years))]
  out[, year := rep(years, each = nrow(dt))]
  out
}

# Climate is averaged per cell, not per series: a cell's climate in a year
# exists whether or not a given crop was grown there that year.
#
# `rows` arrives through the pipe as a promise, and data.table forces a
# bare-symbol `i` inside its own error handler, which turns an abort raised
# upstream into a "not found" error and drops the abort's class. So the join
# helpers force it first.
.join_window_climate <- function(rows, inputs, frame) {
  force(rows)
  cols <- .moisture_climate_cols()
  cells <- unique(inputs[, c("lon", "lat", "year", cols), with = FALSE])
  if (anyDuplicated(cells, by = c("lon", "lat", "year")) > 0L) {
    cli::cli_abort(
      c(
        "{.arg data} carries more than one climate for some cell-years.",
        i = "Climate belongs to the cell: {.field {cols}} must be identical on
             every row of one {.field lon}, {.field lat} and {.field year}."
      ),
      class = "whep_moisture_regime_climate"
    )
  }
  climate <- .moisture_window_means(cells, c("lon", "lat"), cols, frame)
  climate[rows, on = c("lon", "lat", "year")]
}

# Irrigation is averaged per series. Rainfed land is not irrigated by
# definition, so its irrigation term is a structural zero of the regime split
# WHEP defines, not a filled absence.
#
# A separate `irrigation` record is averaged over the window years in which
# the series' land was irrigated, the years it holds a row for.
.join_window_irrigation <- function(rows, inputs, frame, water, irrigation) {
  force(rows)
  if (water == "precip") {
    return(rows)
  }
  key <- .moisture_series_key(inputs)
  has_regime <- rlang::has_name(inputs, "water_regime")
  source <- irrigation %||% inputs
  irrigated <- if (has_regime) source[water_regime == "irrigated"] else source
  if (nrow(irrigated) == 0L) {
    # Every row is rainfed, so every irrigation term is the structural zero.
    return(rows[, irrig_mm := 0])
  }
  values <- irrigated[, c(key, "year", "irrig_mm"), with = FALSE]
  means <- if (is.null(irrigation)) {
    .moisture_window_means(values, key, "irrig_mm", frame)
  } else {
    .irrigated_years_means(values, key, frame)
  }
  out <- means[rows, on = c(key, "year")]
  if (has_regime) {
    out[water_regime == "rainfed", irrig_mm := 0]
  }
  out
}

# Per series and requested year: the mean depth over the window years the
# record holds (the years the land was irrigated) and, where that is fewer
# than the window's years, `.irrig_years`, the stamp's count. Every window
# year is measured, none held. A year the record lacks enters as zero depth
# and zero weight, so the window sums divide to the mean of the irrigated
# years exactly; a held year with a missing depth keeps the sum missing.
.irrigated_years_means <- function(values, key, frame) {
  frame$plan <- .climate_year_plan(frame$needed, frame$needed)
  values[, irrigated_year := 1]
  panel <- .complete_panel(values, key, frame$needed)
  panel[is.na(irrigated_year), `:=`(irrig_mm = 0, irrigated_year = 0)]
  n_window <- length(
    .moisture_window_years(frame$years[[1L]], frame$window, frame$period)
  )
  means <- .apply_moisture_window(
    panel,
    key,
    c("irrig_mm", "irrigated_year"),
    frame
  )
  means[, n_irrigated := as.integer(round(irrigated_year * n_window))]
  means[,
    irrig_mm := data.table::fifelse(
      n_irrigated > 0L,
      irrig_mm / irrigated_year,
      NA_real_
    )
  ]
  means[,
    .irrig_years := data.table::fifelse(
      n_irrigated < n_window,
      paste0("irrig_years_", n_irrigated, "of", n_window),
      NA_character_
    )
  ]
  means[, c(key, "year", "irrig_mm", ".irrig_years"), with = FALSE]
}

# Window means of `cols` per `by_cols` group for each requested year. A window
# year with no value, or no row, gives a missing mean: means are never taken
# over fewer years than the window holds.
.moisture_window_means <- function(values, by_cols, cols, frame) {
  plan <- frame$plan
  panel <- .complete_panel(values, by_cols, plan$read_years)
  data.table::rbindlist(
    list(
      panel[year %in% plan$measured],
      .held_window_mean(panel, by_cols, cols, plan$back_window, plan$back),
      .held_window_mean(panel, by_cols, cols, plan$forward_window, plan$forward)
    ),
    use.names = TRUE
  ) |>
    .apply_moisture_window(by_cols, cols, frame)
}

# Every group crossed with every year read, so an absent row becomes NA.
.complete_panel <- function(values, by_cols, years) {
  groups <- unique(values[, by_cols, with = FALSE])
  values[.cross_years(groups, years), on = c(by_cols, "year")]
}

# The per-group mean over a climatology window, held across `years`. As in
# `build_cell_climate_zone()`, it is held constant, not extrapolated.
.held_window_mean <- function(panel, by_cols, cols, window, years) {
  if (length(years) == 0L) {
    return(NULL)
  }
  panel[year %in% window, lapply(.SD, mean), by = by_cols, .SDcols = cols] |>
    .cross_years(years)
}

# `extended` holds every year the window needs, measured or held, for every
# group.
.apply_moisture_window <- function(extended, by_cols, cols, frame) {
  switch(
    frame$window,
    annual = extended[year %in% frame$years],
    trailing_30 = .trailing_window_mean(extended, by_cols, cols)[
      year %in% frame$years
    ],
    normal = extended[
      year %in% frame$period,
      lapply(.SD, mean),
      by = by_cols,
      .SDcols = cols
    ] |>
      .cross_years(frame$years)
  )
}

# Right-aligned rolling mean over consecutive years. Each group carries the
# same unbroken run of years starting 29 years before the first requested one,
# so every requested year has a full window.
.trailing_window_mean <- function(extended, by_cols, cols) {
  data.table::setorderv(extended, c(by_cols, "year"))
  extended[,
    (cols) := data.table::frollmean(
      .SD,
      n = .trailing_window_n,
      algo = "exact"
    ),
    by = by_cols,
    .SDcols = cols
  ]
  extended
}

# Apply the wet/dry rule to the window means.
.apply_wet_dry <- function(rows, water) {
  required <- c(
    .moisture_climate_cols(),
    if (water == "precip_irrig") "irrig_mm"
  )
  tibble::as_tibble(rows) |>
    dplyr::mutate(
      water_mm = if (water == "precip_irrig") prec_mm + irrig_mm else prec_mm,
      moisture_ratio = dplyr::if_else(pet_mm > 0, water_mm / pet_mm, NA_real_),
      # Figure 3A.5.2 (p. 3.48): MAT above 18 degC and at most 7 frost days.
      tropical = mat_c > 18 & frost_days <= 7,
      .unclassified = .unclassified_reason(
        dplyr::pick(dplyr::all_of(required)),
        pet_mm
      ),
      # Table 11.1 footnote 4 (p. 11.13): tropical rows are wet above 1000 mm
      # of water, other rows above a water:PET ratio of 1. Both are strict.
      moisture_regime = dplyr::case_when(
        !is.na(.unclassified) ~ NA_character_,
        tropical ~ dplyr::if_else(water_mm > 1000, "wet", "dry"),
        .default = dplyr::if_else(moisture_ratio > 1, "wet", "dry")
      )
    )
}

# Why a row cannot be classified, in the stamp's vocabulary, or NA when it can.
.unclassified_reason <- function(values, pet_mm) {
  missing <- purrr::imap(values, \(value, col) {
    dplyr::if_else(is.na(value), paste0("+", col), "")
  }) |>
    purrr::reduce(paste0) |>
    stringr::str_remove("^\\+")
  reasons <- tibble::tibble(
    missing = dplyr::if_else(missing == "", NA, paste0("missing=", missing)),
    pet = dplyr::if_else(pet_mm <= 0, "pet_not_positive", NA, missing = NA)
  )
  dplyr::case_when(
    !is.na(reasons$missing) & !is.na(reasons$pet) ~
      paste(reasons$missing, reasons$pet, sep = ";"),
    !is.na(reasons$missing) ~ reasons$missing,
    .default = reasons$pet
  )
}

# Say how many rows were left without a class, and why.
.report_unclassified <- function(rows) {
  reasons <- rows$.unclassified[!is.na(rows$.unclassified)]
  if (length(reasons) == 0L) {
    return(rows)
  }
  counts <- table(reasons)
  bullets <- rlang::set_names(
    paste0(names(counts), ": ", as.integer(counts)),
    rep("*", length(counts))
  )
  n_rows <- nrow(rows)
  cli::cli_inform(
    c(
      "{length(reasons)} row{?s} of {n_rows} left without a moisture regime;
       nothing was zero-filled.",
      bullets
    ),
    class = "whep_moisture_regime_unclassified"
  )
  rows
}

# Attach `method_moisture_regime` and keep the documented columns.
.stamp_moisture_regime <- function(rows, frame, water) {
  key <- .moisture_series_key(rows)
  values <- c(
    .moisture_climate_cols(),
    if (water == "precip_irrig") "irrig_mm"
  )
  rows |>
    ensure_columns(tibble::tibble(.irrig_years = character())) |>
    dplyr::left_join(.moisture_year_labels(frame, water), by = "year") |>
    dplyr::mutate(
      .method = dplyr::if_else(
        is.na(.irrig_years),
        .method,
        paste(.method, .irrig_years, sep = ";")
      ),
      method_moisture_regime = dplyr::if_else(
        is.na(.unclassified),
        .method,
        paste(.method, .unclassified, sep = ";")
      )
    ) |>
    dplyr::select(dplyr::all_of(c(
      key,
      "year",
      values,
      "water_mm",
      "moisture_ratio",
      "tropical",
      "moisture_regime",
      "method_moisture_regime"
    ))) |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c("year", key))))
}

# The stamp's year-level fields: rule, window, water term and any climatology
# a window reached for.
.moisture_year_labels <- function(frame, water) {
  window_label <- if (frame$window == "normal") {
    paste0("normal_", min(frame$period), "_", max(frame$period))
  } else {
    frame$window
  }
  tibble::tibble(
    year = frame$years,
    .method = purrr::map_chr(frame$years, \(yr) {
      paste(
        c(
          "ipcc2019_wet_dry",
          window_label,
          water,
          .climatology_fills(yr, frame)
        ),
        collapse = ";"
      )
    })
  )
}

.climatology_fills <- function(year, frame) {
  span <- .moisture_window_years(year, frame$window, frame$period)
  record <- frame$covered
  c(
    .climatology_fill(
      span[span < min(record)],
      frame$plan$back_window,
      length(span)
    ),
    .climatology_fill(
      span[span > max(record)],
      frame$plan$forward_window,
      length(span)
    )
  )
}

.climatology_fill <- function(outside, window, n_span) {
  if (length(outside) == 0L) {
    return(NULL)
  }
  paste0(
    "climatology_",
    min(window),
    "_",
    max(window),
    "_",
    length(outside),
    "of",
    n_span
  )
}

# build_cell_moisture_regime() helpers ----
#
# The wrapper assembles the classifier's inputs:
# - Rows: WHEP's own crop land use, the regime areas the N chain splits, one
#   row per cell, polity, commodity balance item, regime and year where that
#   regime has hectares.
# - Climate: CRU TS annual MAT, precipitation, PET and frost days. Only CRU's
#   own years are passed as the record, so years before 1901 take the
#   classifier's held 1901-1930 climatology.
# - Irrigation (irrigated rows): the LPJmL depth of each production item's
#   crop, pooled to the balance item by WHEP's irrigated hectares. It is a
#   record of its own: LPJmL runs before 1901 too, on recycled 1901-1930
#   weather, and those depths are used as the model computed them, labelled.
# - Structural stand check: LPJmL is run on WHEP's own crop land use, so an
#   irrigated WHEP row whose crop has no irrigated LPJmL stand is a mismatch
#   between the two, counted by cause. An item with no LPJmL crop band is a
#   mapping defect and aborts. A year outside the run takes the series'
#   window mean. In a year the run covers, the row takes its crop's LPJmL
#   depth averaged over the polity's cells that have the stand, weighted by
#   WHEP's irrigated hectares (else over the world's), stamped by whether
#   the cell is on the LPJmL grid; with no stand anywhere it aborts.
# - Window: an irrigated series' irrigation is averaged over the window
#   years its land was irrigated (WHEP irrigated hectares above zero),
#   stamped with their count; its climate over the whole window.
# Classification is cell-independent, so it runs in blocks of cells.

# The series a row belongs to (one cell, polity and balance item), and the
# row key that adds the regime.
.cmr_series_key <- function() {
  c("lon", "lat", "area_code", "item_cbs_code")
}

.cmr_row_key <- function() {
  c(.cmr_series_key(), "water_regime")
}

# The arguments resolved once, with every year the windows reach.
.cmr_spec <- function(years, window, water, config) {
  config <- .cmr_config(config)
  years <- .cmr_check_years(years)
  list(
    years = years,
    window = window,
    water = water,
    config = config,
    needed = .cmr_window_years(years, window, config$normal_period)
  )
}

.cmr_config <- function(config) {
  defaults <- list(
    cru_dir = NULL,
    run_dir = NULL,
    # 1991-2020 is an assumed choice, the most recent complete 30-year
    # period; no source used here prescribes it.
    normal_period = 1991:2020,
    max_rows = 2e6
  )
  unknown <- setdiff(names(config), names(defaults))
  if (length(unknown) > 0L) {
    cli::cli_abort(
      c(
        "Unknown {.arg config} entr{?y/ies}: {.val {unknown}}.",
        i = "Known entries: {.val {names(defaults)}}."
      ),
      class = "whep_moisture_regime_config"
    )
  }
  out <- utils::modifyList(defaults, config)
  rows_ok <- is.numeric(out$max_rows) &&
    length(out$max_rows) == 1L &&
    isTRUE(out$max_rows >= 1)
  if (!rows_ok) {
    cli::cli_abort(
      "{.code config$max_rows} must be one positive number.",
      class = "whep_moisture_regime_config"
    )
  }
  out
}

.cmr_check_years <- function(years) {
  ok <- is.numeric(years) && length(years) > 0L && !anyNA(years)
  if (!ok) {
    cli::cli_abort(
      c(
        "{.arg years} must name the years to classify.",
        i = "Expanded to cells, crops and regimes, the whole record does not
             fit in memory."
      ),
      class = "whep_moisture_regime_years"
    )
  }
  sort(unique(as.integer(years)))
}

# Every year one of the requested years' windows holds.
.cmr_window_years <- function(years, window, normal_period) {
  period <- if (window == "normal") .check_normal_period(normal_period)
  purrr::map(years, \(yr) .moisture_window_years(yr, window, period)) |>
    unlist() |>
    unique() |>
    sort() |>
    as.integer()
}

# WHEP's regime areas for every year the windows reach: the rows to classify
# and, on irrigated land, the weights of the pooled items. A window year they
# do not cover leaves its irrigated rows without a depth there, which the
# classifier reports as missing; it is said here once.
.cmr_regime_areas <- function(data, needed) {
  areas <- data$regime_areas %||%
    .sci_engine_crops(
      needed,
      data$country_grid %||% .sci_read_country_grid()
    )
  out <- .lid_check_areas(areas) |>
    dplyr::filter(.data$year %in% needed)
  absent <- setdiff(needed, out$year)
  if (length(absent) > 0L) {
    cli::cli_inform(
      c(
        "The regime areas hold no rows for {length(absent)} year{?s} the
         windows reach: {.val {absent}}.",
        i = "Irrigated rows whose window reaches them have no irrigation depth
             there and are left unclassified."
      ),
      class = "whep_moisture_regime_area_years"
    )
  }
  out
}

# -- Irrigation ---------------------------------------------------------------

# The irrigation record of the irrigated series: per series and window year,
# the pooled depth (in-run years) and the irrigated hectares of the years
# outside the run, which the classification groups fill.
.cmr_irrigation <- function(areas, rows, data, spec) {
  check_inputs_supplied(
    areas,
    "irrigated_ha",
    details = c(i = "The regime areas carry no irrigated hectares at all.")
  )
  key <- .cmr_series_key()
  series <- rows |>
    dplyr::filter(.data$water_regime == "irrigated") |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(key)))
  if (nrow(series) == 0L) {
    return(NULL)
  }
  irrigated <- dplyr::filter(areas, .data$irrigated_ha > 0)
  lpjml <- .cmr_crop_depth(sort(unique(irrigated$year)), data, spec$config)
  # Every irrigated row of the window years gets a depth: the country and
  # world means of a stand-gap fill average over all of them, the series'
  # own rows are the ones classified.
  every <- .lid_item_depth(irrigated, lpjml$crop)
  depth <- dplyr::semi_join(every, series, by = key) |>
    .cmr_fill_stand_gaps(every, lpjml, data, spec$config)
  in_run <- depth$year %in% lpjml$run_years
  list(
    cbs = .cmr_cbs_depth(depth[in_run, ]) |>
      .cmr_absent_area_years(series, areas, spec$needed),
    outside = dplyr::summarise(
      depth[!in_run, ],
      irrigated_ha = sum(.data$irrigated_ha),
      .by = dplyr::all_of(c(key, "year"))
    ),
    run_years = lpjml$run_years,
    vintage = .cmr_vintage(lpjml$crop)
  )
}

# The LPJmL crop-grain depth for `years`, and the years the run covers: the
# supplied table, else the run (only its own years are read), else the pin.
.cmr_crop_depth <- function(years, data, config) {
  if (!is.null(data$crop_depth)) {
    crop <- tibble::as_tibble(data$crop_depth)
    return(list(
      crop = .filter_years_if_present(crop, years),
      run_years = as.integer(data$lpjml_years %||% sort(unique(crop$year)))
    ))
  }
  run_dir <- .lid_run_dir_or_pin(config$run_dir)
  if (is.null(run_dir)) {
    crop <- .lid_crop_depth(years)
    return(list(crop = crop, run_years = sort(unique(crop$year))))
  }
  run_years <- .lid_run_years(run_dir)
  read <- intersect(years, run_years)
  crop <- if (length(read) > 0L) {
    .lid_crop_depth(read, run_dir)
  } else {
    .lid_crop_prototype()
  }
  list(crop = crop, run_years = as.integer(run_years))
}

# The pooled depth with each series-year's irrigated hectares beside it, the
# weights of a country mean.
.cmr_cbs_depth <- function(depth) {
  key <- c(.cmr_series_key(), "year")
  weights <- dplyr::summarise(
    depth,
    irrigated_ha = sum(.data$irrigated_ha),
    .by = dplyr::all_of(key)
  )
  .lid_cbs_depth(depth) |>
    dplyr::left_join(weights, by = key, relationship = "one-to-one")
}

# A window year the regime areas hold no rows for at all says nothing about
# whether a series' land was irrigated, so it is not read as a year without
# irrigation: every irrigated series carries it as a missing depth, which
# leaves the series' window mean missing.
.cmr_absent_area_years <- function(cbs, series, areas, needed) {
  absent <- setdiff(needed, unique(areas$year))
  if (length(absent) == 0L) {
    return(cbs)
  }
  dplyr::bind_rows(cbs, tidyr::expand_grid(series, year = as.integer(absent)))
}

.cmr_vintage <- function(crop) {
  if (nrow(crop) == 0L) {
    return(tibble::tibble(
      lpjml_version = NA_character_,
      lpjml_git_hash = NA_character_,
      lpjml_run = NA_character_
    ))
  }
  .lid_vintage_of(crop)
}

# -- Structural stand check ---------------------------------------------------

# The series' production item rows with every stand gap in a year the run
# covers filled (see the section header), after the mapping check. `every`
# is every irrigated row of the window years, the rows the means average.
.cmr_fill_stand_gaps <- function(depth, every, lpjml, data, config) {
  gaps <- .cmr_check_stands(depth, lpjml)
  if (is.null(gaps)) {
    return(depth)
  }
  diagnosis <- .cmr_stand_diagnosis(
    gaps,
    lpjml$crop,
    .cmr_lpjml_grid(data, config)
  )
  filled <- .cmr_apply_stand_means(depth, diagnosis$cases, every)
  .cmr_inform_stand_gaps(diagnosis, filled)
  filled
}

# Every irrigated production item row without an LPJmL depth, by cause:
# - "outside_run": the year lies outside the LPJmL run; filled later;
# - "mapping": the item has no LPJmL crop band; aborts;
# - "stand_gap": the item's crop has no irrigated stand in the cell although
#   the run covers the year; returned, to be filled.
# NULL when there is no stand gap.
.cmr_check_stands <- function(depth, lpjml) {
  defects <- depth |>
    dplyr::filter(!.data$method_irrig_depth %in% .lid_lpjml_methods()) |>
    dplyr::mutate(
      cause = dplyr::case_when(
        .data$method_irrig_depth == "no_band" ~ "mapping",
        !.data$year %in% lpjml$run_years ~ "outside_run",
        .default = "stand_gap"
      )
    )
  .cmr_abort_mapping(defects)
  gaps <- dplyr::filter(defects, .data$cause == "stand_gap")
  if (nrow(gaps) == 0L) {
    return(NULL)
  }
  gaps
}

# Per stand gap, why LPJmL has no irrigated stand of the crop, and the fill
# stamp that follows: `detail` says whether the cell is on the LPJmL grid,
# whether LPJmL has any crop stand in it and whether it has a rainfed stand
# of the crop; `fill` is `no_lpjml_cell` off the grid, else
# `lpjml_landuse_vintage`. `summary` counts rows and hectares by both.
.cmr_stand_diagnosis <- function(gaps, crop, grid) {
  cells <- crop |>
    dplyr::distinct(.data$lon, .data$lat, .data$year) |>
    dplyr::mutate(cell_has_crops = TRUE)
  rainfed <- crop |>
    dplyr::filter(
      is.finite(.data$stand_frac_rainfed),
      .data$stand_frac_rainfed > 0
    ) |>
    dplyr::distinct(.data$lon, .data$lat, .data$year, .data$lpjml_crop) |>
    dplyr::mutate(crop_rainfed = TRUE)
  cases <- gaps |>
    dplyr::left_join(
      grid,
      by = c("lon", "lat"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(cells, by = c("lon", "lat", "year")) |>
    dplyr::left_join(rainfed, by = c("lon", "lat", "year", "lpjml_crop")) |>
    dplyr::mutate(
      detail = dplyr::case_when(
        is.na(.data$in_lpjml) ~ "outside_lpjml_grid",
        is.na(.data$cell_has_crops) ~ "no_lpjml_crop_stand_in_cell",
        .data$crop_rainfed %in% TRUE ~ "lpjml_crop_rainfed_only",
        .default = "lpjml_crop_absent_from_cell"
      ),
      fill = dplyr::if_else(
        is.na(.data$in_lpjml),
        "no_lpjml_cell",
        "lpjml_landuse_vintage"
      )
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "item_prod_code",
      "item_cbs_code",
      "year",
      "irrigated_ha",
      "lpjml_crop",
      "detail",
      "fill"
    ) |>
    dplyr::arrange(dplyr::desc(.data$irrigated_ha))
  list(summary = .cmr_stand_summary(cases, c("fill", "detail")), cases = cases)
}

.cmr_stand_summary <- function(cases, by) {
  cases |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      irrigated_ha = sum(.data$irrigated_ha),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::arrange(.data[[by[[1L]]]], dplyr::desc(.data$irrigated_ha))
}

# The cells the LPJmL run covers at all (any band): the supplied grid, else
# the run's (or, with no run, the pinned layer's), the grid the regime yield
# ratio reads. Supplied crop depths come with their own grid, never the
# run's.
.cmr_lpjml_grid <- function(data, config) {
  if (!is.null(data$lpjml_grid)) {
    return(.ryr_lpjml_grid(data$lpjml_grid))
  }
  if (!is.null(data$crop_depth)) {
    cli::cli_abort(
      c(
        "{.code data$crop_depth} has stand gaps, so the LPJmL grid it was
         computed on is needed too.",
        i = "Supply {.code data$lpjml_grid}, the cells ({.field lon},
             {.field lat}) the run's output covers."
      ),
      class = "whep_moisture_regime_lpjml_grid"
    )
  }
  .ryr_lpjml_grid(.ryr_read_lpjml_grid(config$run_dir))
}

# The crop's LPJmL depth for each stand gap: the mean over the polity's
# production item rows that LPJmL computed a depth for that year, weighted
# by WHEP's irrigated hectares, else the same over the world. A crop with no
# irrigated stand anywhere that year has no depth to give: that aborts.
.cmr_apply_stand_means <- function(depth, cases, every) {
  row_key <- c("lon", "lat", "area_code", "item_prod_code", "year")
  means <- .cmr_stand_means(every)
  out <- depth |>
    dplyr::left_join(
      dplyr::select(cases, dplyr::all_of(row_key), "fill"),
      by = row_key,
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      means$country,
      by = c("area_code", "lpjml_crop", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      means$world,
      by = c("lpjml_crop", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      irrig_mm = dplyr::case_when(
        is.na(.data$fill) ~ .data$irrig_mm,
        !is.na(.data$country_mm) ~ .data$country_mm,
        .default = .data$world_mm
      ),
      method_irrig_depth = dplyr::case_when(
        is.na(.data$fill) ~ .data$method_irrig_depth,
        !is.na(.data$country_mm) ~ .data$fill,
        .default = paste0(.data$fill, "_world_mean")
      )
    )
  .cmr_check_stand_means(out)
  dplyr::select(out, -"fill", -"country_mm", -"world_mm")
}

.cmr_stand_means <- function(every) {
  known <- dplyr::filter(
    every,
    .data$method_irrig_depth %in% .lid_lpjml_methods()
  )
  list(
    country = .cmr_weighted_depth(known, "country_mm", c("area_code")),
    world = .cmr_weighted_depth(known, "world_mm", character())
  )
}

.cmr_weighted_depth <- function(known, name, by) {
  dplyr::summarise(
    known,
    "{name}" := sum(.data$irrig_mm * .data$irrigated_ha) /
      sum(.data$irrigated_ha),
    .by = dplyr::all_of(c(by, "lpjml_crop", "year"))
  )
}

.cmr_check_stand_means <- function(filled) {
  bare <- filled[!is.na(filled$fill) & is.na(filled$irrig_mm), ]
  if (nrow(bare) == 0L) {
    return(invisible(NULL))
  }
  crops <- sort(unique(paste0(bare$lpjml_crop, " (", bare$year, ")")))
  n_rows <- nrow(bare)
  cli::cli_abort(
    c(
      "{n_rows} irrigated row{?s} of WHEP's land use lack an irrigated LPJmL
       stand of their crop, and the run has none of it anywhere that year.",
      i = "Crops (years): {crops}.",
      x = "There is no LPJmL depth of the crop to stand in."
    ),
    class = "whep_moisture_regime_stand_no_mean"
  )
}

# Say how many rows were filled, how many hectares, and why: the condition's
# `diagnosis` holds every case.
.cmr_inform_stand_gaps <- function(diagnosis, filled) {
  row_key <- c("lon", "lat", "area_code", "item_prod_code", "year")
  cases <- diagnosis$cases |>
    dplyr::select(-"fill") |>
    dplyr::left_join(
      dplyr::select(
        filled,
        dplyr::all_of(row_key),
        fill = "method_irrig_depth"
      ),
      by = row_key,
      relationship = "one-to-one"
    )
  diagnosis <- list(
    summary = .cmr_stand_summary(cases, c("fill", "detail")),
    cases = cases
  )
  details <- diagnosis$summary
  bullets <- rlang::set_names(
    paste0(
      details$fill,
      ", ",
      details$detail,
      ": ",
      details$n_rows,
      " rows, ",
      .cmr_format_ha(details$irrigated_ha),
      " ha"
    ),
    rep("*", nrow(details))
  )
  n_rows <- nrow(cases)
  hectares <- .cmr_format_ha(sum(cases$irrigated_ha))
  cli::cli_inform(
    c(
      "{n_rows} irrigated row{?s} of WHEP's land use ({hectares} ha), in years
       the LPJmL run covers, have no irrigated LPJmL stand of their crop; each
       takes its crop's mean LPJmL depth, stamped in
       {.field method_irrig_depth}.",
      bullets,
      i = "The condition's {.field diagnosis} holds every case."
    ),
    class = "whep_moisture_regime_stand_gap",
    diagnosis = diagnosis
  )
}

.cmr_abort_mapping <- function(defects) {
  cases <- defects[defects$cause == "mapping", ]
  if (nrow(cases) == 0L) {
    return(invisible(NULL))
  }
  n_rows <- nrow(cases)
  hectares <- .cmr_format_ha(sum(cases$irrigated_ha))
  largest <- .cmr_largest(cases, "item_prod_code")
  cases <- dplyr::select(
    cases,
    "lon",
    "lat",
    "area_code",
    "item_prod_code",
    "item_cbs_code",
    "year",
    "irrigated_ha",
    "cause"
  )
  diagnosis <- list(
    summary = .cmr_stand_summary(cases, "cause"),
    cases = dplyr::arrange(cases, dplyr::desc(.data$irrigated_ha))
  )
  cli::cli_abort(
    c(
      "{n_rows} irrigated row{?s} of WHEP's land use ({hectares} ha) belong to
       production items with no LPJmL crop band.",
      x = "LPJmL is run on WHEP's own crop land use, so every irrigated item
           must map to an LPJmL crop: this is a mapping defect, not a gap.",
      i = "Largest items: {largest}.",
      i = "Map them in {.code regime_yield_crop_mapping}; the condition's
           {.field diagnosis} holds every case."
    ),
    class = "whep_moisture_regime_stand_mapping",
    diagnosis = diagnosis
  )
}

# The five largest groups of `cases` by irrigated hectares, as text.
.cmr_largest <- function(cases, by) {
  cases |>
    dplyr::summarise(
      irrigated_ha = sum(.data$irrigated_ha),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::slice_max(.data$irrigated_ha, n = 5L, with_ties = FALSE) |>
    dplyr::mutate(
      label = paste0(
        purrr::pmap_chr(dplyr::pick(dplyr::all_of(by)), paste, sep = "/"),
        " (",
        .cmr_format_ha(.data$irrigated_ha),
        " ha)"
      )
    ) |>
    dplyr::pull("label") |>
    paste(collapse = ", ")
}

.cmr_format_ha <- function(hectares) {
  format(round(hectares), big.mark = ",", scientific = FALSE, trim = TRUE)
}

# -- Classification -----------------------------------------------------------

# The requested years classified together, unless a window year outside the
# LPJmL run needs a fill: a fill averages the series' own window, which
# differs between requested years, so each year is then its own group.
.cmr_year_groups <- function(years, irrigation) {
  if (is.null(irrigation) || nrow(irrigation$outside) == 0L) {
    return(list(years))
  }
  as.list(years)
}

.cmr_classify_group <- function(group, rows, climate, irrigation, spec) {
  frame <- .moisture_frame(
    group,
    spec$window,
    spec$config$normal_period,
    climate$record
  )
  ctx <- list(
    group = group,
    rows = dplyr::filter(rows, .data$year %in% group),
    climate = dplyr::filter(
      climate$table,
      .data$year %in% frame$plan$read_years
    ),
    read_years = frame$plan$read_years,
    depth = .cmr_group_irrigation(irrigation, group, spec),
    spec = spec
  )
  .cmr_cell_blocks(ctx$rows, frame, spec$config$max_rows) |>
    purrr::map(\(cells) .cmr_classify_block(cells, ctx)) |>
    dplyr::bind_rows()
}

# The irrigation record the group's windows read, with the years outside the
# run filled: the mean of the series' own window years with a depth, else the
# country mean of its balance item over those years, stamped.
.cmr_group_irrigation <- function(irrigation, group, spec) {
  if (is.null(irrigation)) {
    return(NULL)
  }
  span <- .cmr_window_years(group, spec$window, spec$config$normal_period)
  depth <- dplyr::filter(irrigation$cbs, .data$year %in% span)
  outside <- dplyr::filter(irrigation$outside, .data$year %in% span)
  fills <- .cmr_fill_outside_run(depth, outside, span, irrigation$run_years)
  list(
    rows = dplyr::bind_rows(depth, fills$rows),
    fills = fills$stamps,
    vintage = irrigation$vintage
  )
}

.cmr_fill_outside_run <- function(depth, outside, span, run_years) {
  if (nrow(outside) == 0L) {
    return(list(rows = NULL, stamps = NULL))
  }
  key <- .cmr_series_key()
  in_run <- intersect(span, run_years)
  known <- dplyr::filter(
    depth,
    .data$year %in% in_run,
    !is.na(.data$irrig_mm)
  )
  own <- dplyr::summarise(
    known,
    series_fill = mean(.data$irrig_mm),
    .by = dplyr::all_of(key)
  )
  country <- known |>
    dplyr::summarise(
      irrig_mm = sum(.data$irrig_mm * .data$irrigated_ha) /
        sum(.data$irrigated_ha),
      .by = c("area_code", "item_cbs_code", "year")
    ) |>
    dplyr::summarise(
      country_fill = mean(.data$irrig_mm),
      .by = c("area_code", "item_cbs_code")
    )
  filled <- outside |>
    dplyr::left_join(own, by = key) |>
    dplyr::left_join(country, by = c("area_code", "item_cbs_code")) |>
    dplyr::mutate(
      irrig_mm = dplyr::coalesce(.data$series_fill, .data$country_fill),
      fill = dplyr::if_else(
        is.na(.data$series_fill),
        "country_mean",
        "series_mean"
      )
    )
  .cmr_check_filled(filled)
  list(
    rows = dplyr::select(filled, dplyr::all_of(c(key, "year", "irrig_mm"))),
    stamps = dplyr::summarise(
      filled,
      n_filled = dplyr::n(),
      fill = dplyr::if_else(
        any(.data$fill == "country_mean"),
        "country_mean",
        "series_mean"
      ),
      .by = dplyr::all_of(key)
    )
  )
}

# A window wholly outside the run, or one where neither the series nor its
# country has a depth, has nothing to average: no fill exists.
.cmr_check_filled <- function(filled) {
  unfilled <- is.na(filled$irrig_mm)
  if (!any(unfilled)) {
    return(invisible(NULL))
  }
  outside <- sort(unique(filled$year))
  n_rows <- sum(unfilled)
  cli::cli_abort(
    c(
      "{n_rows} irrigated series-year{?s} in years outside the LPJmL run
       ({.val {outside}}) ha{?s/ve} no depth to fill from.",
      i = "The fill averages the series' own window years inside the run,
           else its country's; here neither has one.",
      i = "Use a run covering the years, or a window reaching into the run."
    ),
    class = "whep_moisture_regime_outside_run"
  )
}

# Blocks of cells whose classifier inputs stay near `max_rows` rows: a cell
# carries one rainfed series and its irrigated series, each with one row per
# climate year read and, irrigated, one irrigation row per window year. Cells
# are dealt round-robin, so a block mixes cells from the whole grid.
.cmr_cell_blocks <- function(rows, frame, max_rows) {
  n_read <- length(frame$plan$read_years)
  n_span <- length(frame$needed)
  cells <- rows |>
    dplyr::mutate(irrigated = .data$water_regime == "irrigated") |>
    dplyr::summarise(
      n_irrigated = dplyr::n_distinct(
        .data$area_code[.data$irrigated],
        .data$item_cbs_code[.data$irrigated]
      ),
      .by = c("lon", "lat")
    ) |>
    dplyr::arrange(.data$lon, .data$lat)
  n_input <- sum((1 + cells$n_irrigated) * n_read + cells$n_irrigated * n_span)
  n_blocks <- min(max(1L, ceiling(n_input / max_rows)), nrow(cells))
  block <- (seq_len(nrow(cells)) - 1L) %% n_blocks
  unname(split(dplyr::select(cells, "lon", "lat"), block))
}

.cmr_classify_block <- function(cells, ctx) {
  rows <- dplyr::semi_join(ctx$rows, cells, by = c("lon", "lat"))
  climate <- tidyr::expand_grid(cells, year = ctx$read_years) |>
    dplyr::left_join(ctx$climate, by = c("lon", "lat", "year"))
  if (ctx$spec$water == "precip") {
    return(.cmr_precip_rows(rows, climate, ctx))
  }
  dplyr::bind_rows(
    .cmr_rainfed_rows(rows, climate, ctx),
    .cmr_irrigated_rows(rows, climate, ctx)
  )
}

# The classifier on one block, its per-block report of unclassified rows
# held back: the wrapper reports once over every block.
.cmr_run_classifier <- function(data, ctx, water, irrigation = NULL) {
  withCallingHandlers(
    .classify_moisture_regime(
      data,
      years = ctx$group,
      window = ctx$spec$window,
      water = water,
      normal_period = ctx$spec$config$normal_period,
      irrigation = irrigation
    ),
    whep_moisture_regime_unclassified = .cmr_muffle
  )
}

.cmr_muffle <- function(cnd) {
  invokeRestart("muffleMessage")
}

# Precipitation alone is a property of the cell: every row of a cell takes
# the cell's class.
.cmr_precip_rows <- function(rows, climate, ctx) {
  cells <- .cmr_run_classifier(climate, ctx, "precip") |>
    dplyr::mutate(irrig_mm = NA_real_)
  rows |>
    dplyr::left_join(
      cells,
      by = c("lon", "lat", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      method_irrig_depth = NA_character_,
      lpjml_crop = NA_character_
    )
}

# Rainfed land receives no irrigation, by definition of the regime, so its
# water term is the cell's precipitation and every rainfed row of a cell
# takes one class, computed once per cell.
.cmr_rainfed_rows <- function(rows, climate, ctx) {
  rainfed <- dplyr::filter(rows, .data$water_regime == "rainfed")
  if (nrow(rainfed) == 0L) {
    return(NULL)
  }
  cells <- climate |>
    dplyr::semi_join(rainfed, by = c("lon", "lat")) |>
    dplyr::mutate(water_regime = "rainfed", irrig_mm = 0) |>
    .cmr_run_classifier(ctx, "precip_irrig") |>
    dplyr::select(-"water_regime")
  rainfed |>
    dplyr::left_join(
      cells,
      by = c("lon", "lat", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      method_irrig_depth = "rainfed_regime",
      lpjml_crop = NA_character_
    )
}

.cmr_irrigated_rows <- function(rows, climate, ctx) {
  irrigated <- dplyr::filter(rows, .data$water_regime == "irrigated")
  if (nrow(irrigated) == 0L) {
    return(NULL)
  }
  key <- .cmr_series_key()
  series <- dplyr::distinct(irrigated, dplyr::pick(dplyr::all_of(key)))
  depth <- dplyr::semi_join(ctx$depth$rows, series, by = key)
  input <- series |>
    dplyr::inner_join(
      climate,
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(water_regime = "irrigated")
  classes <- .cmr_run_classifier(
    input,
    ctx,
    "precip_irrig",
    irrigation = dplyr::mutate(
      dplyr::select(depth, dplyr::all_of(c(key, "year", "irrig_mm"))),
      water_regime = "irrigated"
    )
  )
  irrigated |>
    dplyr::left_join(
      classes,
      by = c(.cmr_row_key(), "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      .cmr_irrigation_stamps(irrigated, depth, ctx),
      by = c(key, "year"),
      relationship = "one-to-one"
    )
}

# Per irrigated row: the least direct depth method over its window years,
# "_recycled_climate" when a window year predates the CRU record LPJmL's
# weather starts with, the fill of years outside the run, the crops behind
# the year's own depth and the run's vintage.
.cmr_irrigation_stamps <- function(irrigated, depth, ctx) {
  key <- .cmr_series_key()
  spec <- ctx$spec
  windows <- tibble::tibble(year = ctx$group) |>
    dplyr::mutate(
      window_year = purrr::map(.data$year, \(yr) {
        .moisture_window_years(yr, spec$window, spec$config$normal_period)
      })
    ) |>
    tidyr::unnest("window_year")
  window_depth <- dplyr::select(
    depth,
    dplyr::all_of(key),
    window_year = "year",
    "method_irrig_depth",
    "irrig_mm"
  )
  irrigated |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c(key, "year")))) |>
    dplyr::inner_join(windows, by = "year", relationship = "many-to-many") |>
    dplyr::left_join(window_depth, by = c(key, "window_year")) |>
    dplyr::summarise(
      rank = max(
        dplyr::coalesce(
          match(.data$method_irrig_depth, .lid_method_rank()),
          0L
        )
      ),
      recycled = any(
        .data$window_year < .lrg_climate_first_year() & !is.na(.data$irrig_mm)
      ),
      n_window = dplyr::n(),
      .by = dplyr::all_of(c(key, "year"))
    ) |>
    .cmr_stamp_fills(ctx$depth$fills) |>
    dplyr::left_join(
      dplyr::select(depth, dplyr::all_of(c(key, "year")), "lpjml_crop"),
      by = c(key, "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      lpjml_version = ctx$depth$vintage$lpjml_version,
      lpjml_git_hash = ctx$depth$vintage$lpjml_git_hash,
      lpjml_run = ctx$depth$vintage$lpjml_run
    )
}

.cmr_stamp_fills <- function(stamps, fills) {
  fills <- fills %||%
    tibble::tibble(
      lon = double(),
      lat = double(),
      area_code = integer(),
      item_cbs_code = integer(),
      n_filled = integer(),
      fill = character()
    )
  stamps |>
    dplyr::left_join(fills, by = .cmr_series_key()) |>
    dplyr::mutate(
      method = .lid_method_rank()[dplyr::na_if(.data$rank, 0L)],
      method = dplyr::if_else(
        .data$recycled &
          .data$method %in% c(.lid_lpjml_methods(), .lid_stand_fill_methods()),
        paste0(.data$method, "_recycled_climate"),
        .data$method
      ),
      method_irrig_depth = dplyr::if_else(
        is.na(.data$n_filled),
        .data$method,
        paste0(
          .data$method,
          ";outside_run_",
          .data$n_filled,
          "of",
          .data$n_window,
          "_",
          .data$fill
        )
      )
    ) |>
    dplyr::select(
      dplyr::all_of(c(.cmr_series_key(), "year")),
      "method_irrig_depth"
    )
}

# -- Climate ------------------------------------------------------------------

# CRU's annual climate under the classifier's names, for the cells to
# classify, the record (the years CRU covers) and the cells whose climate is
# their nearest CRU land cell's (`fills`). `data$.cru_climate` is that list
# already read once, which build_n_loss_drivers() shares between its calls.
.cmr_climate <- function(data, spec, cells) {
  climate <- data$.cru_climate %||% .cmr_source_climate(data, spec)
  check_inputs_supplied(
    climate$table,
    c("prec_mm", "pet_mm"),
    details = c(i = "Supply annual totals in mm per year.")
  )
  filled <- .cmr_nearest_climate(climate$table, cells)
  climate$table <- filled$table
  climate$fills <- filled$fills
  climate
}

# The supplied climate table, else CRU over the years the windows read.
.cmr_source_climate <- function(data, spec) {
  if (is.null(data$climate)) {
    return(.cmr_read_cru(spec))
  }
  table <- .cmr_rename_climate(data$climate)
  list(table = table, record = sort(unique(table$year)))
}

# The climate of the cells to classify. A variable a cell holds no value of in
# any year is lent by the nearest CRU land cell, by great-circle distance
# between cell centres, among the cells that hold that variable in every year
# read; every variable the cell does hold stays its own. A cell off CRU's land
# mask (a coastal or island cell) holds none of the four and so takes all of
# them; a cell CRU maps for some variables only takes the rest. A variable
# with gaps is not lacking: it stays the cell's own and the classifier
# reports the years without it. One donor supplies all the variables a cell
# lacks, so the borrowed variables always come from a single place. The fill
# is a climate, not a class: the classifier then applies the cell's own water
# term (its irrigation included) to it. `fills` lists each filled cell with
# its distance, `climate_fill_km`, and the variables it took,
# `climate_fill_vars`.
.cmr_nearest_climate <- function(table, cells) {
  cells <- cells |>
    dplyr::transmute(
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2)
    ) |>
    dplyr::distinct()
  coverage <- .cmr_climate_coverage(table)
  lacking <- .cmr_lacking_variables(cells, coverage)
  kept <- table |>
    dplyr::semi_join(cells, by = c("lon", "lat")) |>
    dplyr::anti_join(lacking, by = c("lon", "lat"))
  if (nrow(lacking) == 0L) {
    return(list(table = kept, fills = .cmr_fill_prototype()))
  }
  .cmr_check_donors(coverage, unique(lacking$climate_fill_vars))
  fills <- .cmr_by_variables(lacking, \(group) {
    .cmr_nearest_donor(group, coverage)
  })
  copied <- .cmr_by_variables(fills, \(group) .cmr_copy_climate(table, group))
  .cmr_inform_nearest(fills)
  list(
    table = dplyr::bind_rows(kept, copied),
    fills = dplyr::select(
      fills,
      "lon",
      "lat",
      "climate_fill_km",
      "climate_fill_vars"
    )
  )
}

.cmr_fill_prototype <- function() {
  tibble::tibble(
    lon = double(),
    lat = double(),
    climate_fill_km = double(),
    climate_fill_vars = character()
  )
}

# Apply `fun` to the cells that lack the same variables, and bind the results.
.cmr_by_variables <- function(cells, fun) {
  unname(split(cells, cells$climate_fill_vars)) |>
    purrr::map(fun) |>
    dplyr::bind_rows()
}

# One row per cell of the table: for each CRU variable, whether the cell holds
# any value of it (`<col>_has`) and a value in every year the table holds
# (`<col>_full`); and whether it has a row in every one of those years.
.cmr_climate_coverage <- function(table) {
  n_years <- dplyr::n_distinct(table$year)
  table |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(.moisture_climate_cols()),
        list(has = \(x) any(!is.na(x)), full = \(x) all(!is.na(x)))
      ),
      years_full = dplyr::n_distinct(.data$year) == .env$n_years,
      .by = c("lon", "lat")
    )
}

# The cells that lack at least one CRU variable, with the variables they lack
# (`climate_fill_vars`), joined by "+" in the order and spelling of the
# `missing=` stamp. A cell absent from the table lacks all four.
.cmr_lacking_variables <- function(cells, coverage) {
  joined <- dplyr::left_join(cells, coverage, by = c("lon", "lat"))
  vars <- purrr::map(.moisture_climate_cols(), \(col) {
    has <- dplyr::coalesce(joined[[paste0(col, "_has")]], FALSE)
    dplyr::if_else(has, "", paste0("+", col))
  }) |>
    purrr::reduce(paste0) |>
    stringr::str_remove("^\\+")
  joined |>
    dplyr::select("lon", "lat") |>
    dplyr::mutate(climate_fill_vars = .env$vars) |>
    dplyr::filter(.data$climate_fill_vars != "")
}

# The variables of a `climate_fill_vars` stamp.
.cmr_fill_variables <- function(key) {
  stringr::str_split_1(key, "\\+")
}

# For cells that lack the same variables, the nearest donor of all of them and
# its distance.
.cmr_nearest_donor <- function(group, coverage) {
  key <- group$climate_fill_vars[[1]]
  donors <- .cmr_climate_donors(coverage, .cmr_fill_variables(key))
  .cmr_nearest_cells(group, donors) |>
    dplyr::mutate(climate_fill_vars = key)
}

# The cells that can lend `vars`: a value of each in every year the table
# holds, ordered so that a tie in distance always resolves to the same cell.
.cmr_climate_donors <- function(coverage, vars) {
  coverage |>
    dplyr::filter(
      .data$years_full,
      dplyr::if_all(dplyr::all_of(paste0(vars, "_full")))
    ) |>
    dplyr::arrange(.data$lon, .data$lat)
}

# Every set of variables some cell lacks needs a donor of all of them. Checked
# before any cell is filled, outside the per-set iteration, so the abort keeps
# its own condition classes.
.cmr_check_donors <- function(coverage, keys) {
  donorless <- purrr::keep(keys, \(key) {
    nrow(.cmr_climate_donors(coverage, .cmr_fill_variables(key))) == 0L
  })
  if (length(donorless) == 0L) {
    return(invisible(NULL))
  }
  vars <- unique(unlist(purrr::map(donorless, .cmr_fill_variables)))
  cli::cli_abort(
    c(
      "No cell of the climate table holds {.field {vars}} in every year read
       to lend it to a cell without it.",
      i = "Every variable a cell lacks needs a cell with the full record."
    ),
    class = c("whep_moisture_regime_climate", "whep_absent_input")
  )
}

# The donor's values of the variables the cells lack, in every year it holds,
# beside the cells' own values of the others.
.cmr_copy_climate <- function(table, group) {
  vars <- .cmr_fill_variables(group$climate_fill_vars[[1]])
  own <- table |>
    dplyr::semi_join(group, by = c("lon", "lat")) |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      dplyr::all_of(setdiff(.moisture_climate_cols(), vars))
    )
  table |>
    dplyr::select(
      donor_lon = "lon",
      donor_lat = "lat",
      "year",
      dplyr::all_of(vars)
    ) |>
    dplyr::inner_join(
      dplyr::select(group, "lon", "lat", "donor_lon", "donor_lat"),
      by = c("donor_lon", "donor_lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::select(-"donor_lon", -"donor_lat") |>
    dplyr::left_join(own, by = c("lon", "lat", "year"))
}

# For each cell that lacks climate variables, the nearest donor and its
# distance (the first in donor order on a tie).
.cmr_nearest_cells <- function(missing, donors) {
  purrr::map2(missing$lon, missing$lat, \(lon, lat) {
    km <- .great_circle_km(lon, lat, donors$lon, donors$lat)
    nearest <- which.min(km)
    tibble::tibble(
      lon = lon,
      lat = lat,
      donor_lon = donors$lon[nearest],
      donor_lat = donors$lat[nearest],
      climate_fill_km = km[nearest]
    )
  }) |>
    dplyr::bind_rows()
}

# Great-circle (haversine) distance in km between points given in degrees, on
# the mean Earth radius R1 = 6371.0088 km of the Geodetic Reference System
# 1980 (Moritz 2000, Journal of Geodesy 74: 128-133).
.great_circle_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  h <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371.0088 * asin(sqrt(pmin(1, h)))
}

.cmr_inform_nearest <- function(fills) {
  n_cells <- nrow(fills)
  counts <- table(fills$climate_fill_vars)
  bullets <- rlang::set_names(
    paste0(names(counts), ": ", as.integer(counts)),
    rep("*", length(counts))
  )
  cli::cli_inform(
    c(
      "{n_cells} cell{?s} lacking CRU climate took the missing variables from
       the nearest CRU land cell, stamped {.val nearest_cru_cell}.",
      bullets,
      i = "Distance: median {signif(stats::median(fills$climate_fill_km), 3)}
           km, largest {signif(max(fills$climate_fill_km), 3)} km."
    ),
    class = "whep_moisture_regime_nearest_climate"
  )
}

# Each row of a cell whose climate is a nearest-cell fill carries the
# distance, `climate_fill_km`, and the field `nearest_cru_cell=<col>[+<col>...]`
# naming the variables it took, in its `method_moisture_regime`; every other
# row carries NA.
.cmr_stamp_climate_fill <- function(out, fills) {
  fills <- fills %||% .cmr_fill_prototype()
  out |>
    dplyr::select(
      -dplyr::any_of(c("climate_fill_km", "climate_fill_vars"))
    ) |>
    dplyr::left_join(
      fills,
      by = c("lon", "lat"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      method_moisture_regime = dplyr::if_else(
        is.na(.data$climate_fill_vars),
        .data$method_moisture_regime,
        .moisture_add_field(
          .data$method_moisture_regime,
          paste0("nearest_cru_cell=", .data$climate_fill_vars)
        )
      )
    ) |>
    dplyr::select(-"climate_fill_vars")
}

# Add a field to a `method_moisture_regime` stamp, ahead of the reasons an
# unclassified row carries, which always come last.
.moisture_add_field <- function(stamp, field) {
  tail <- ";(missing=|pet_not_positive).*$"
  reasons <- stringr::str_extract(stamp, tail)
  paste0(
    stringr::str_remove(stamp, tail),
    ";",
    field,
    dplyr::coalesce(reasons, "")
  )
}

# The class of each cell on its precipitation alone, per requested year:
# what a row with no crop takes, since no crop means no irrigation. Same
# climate, nearest-cell fill and window as the crop rows.
.cmr_cell_classes <- function(cells, spec, data) {
  climate <- .cmr_climate(data, spec, cells)
  frame <- .moisture_frame(
    spec$years,
    spec$window,
    spec$config$normal_period,
    climate$record
  )
  ctx <- list(
    group = spec$years,
    spec = spec,
    read_years = frame$plan$read_years
  )
  table <- dplyr::filter(climate$table, .data$year %in% ctx$read_years)
  cells <- dplyr::distinct(
    dplyr::transmute(
      cells,
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2)
    )
  )
  n_blocks <- min(
    max(
      1L,
      ceiling(nrow(cells) * length(ctx$read_years) / spec$config$max_rows)
    ),
    nrow(cells)
  )
  block <- (seq_len(nrow(cells)) - 1L) %% n_blocks
  unname(split(cells, block)) |>
    purrr::map(\(cell_block) {
      tidyr::expand_grid(cell_block, year = ctx$read_years) |>
        dplyr::left_join(table, by = c("lon", "lat", "year")) |>
        .cmr_run_classifier(ctx, "precip")
    }) |>
    dplyr::bind_rows() |>
    .cmr_stamp_climate_fill(climate$fills)
}

# The four CRU variables over the years the classifier reads: the record's
# years inside the windows, plus the held 1901-1930 (or last 30 years')
# climatology when a window reaches past the record.
.cmr_read_cru <- function(spec) {
  dir <- .resolve_cru_dir(spec$config$cru_dir)
  record <- .cru_covered_years(dir)
  read <- .moisture_frame(
    spec$years,
    spec$window,
    spec$config$normal_period,
    record
  )$plan$read_years
  table <- purrr::map(c("tmp", "pre", "pet", "frs"), \(var) {
    .cru_annual(dir, read, var)
  }) |>
    purrr::reduce(dplyr::full_join, by = c("lon", "lat", "year"))
  list(table = .cmr_rename_climate(table), record = record)
}

# The CRU reader's column names, as the classifier names them.
.cmr_climate_names <- function() {
  c(
    mat_c = "mean_annual_temp_c",
    frost_days = "annual_frost_days",
    prec_mm = "annual_precip_mm",
    pet_mm = "annual_pet_mm"
  )
}

.cmr_rename_climate <- function(climate) {
  names_map <- .cmr_climate_names()
  .check_columns(climate, c("lon", "lat", "year", names_map), "data$climate")
  tibble::as_tibble(climate) |>
    dplyr::select("lon", "lat", "year", dplyr::all_of(names_map)) |>
    dplyr::mutate(
      lon = round(as.numeric(.data$lon), 2),
      lat = round(as.numeric(.data$lat), 2),
      year = as.integer(.data$year)
    )
}

# -- Report -------------------------------------------------------------------

# One report over every block: how many rows were left without a class, by
# regime and reason. Nothing is zero-filled.
.cmr_report_unclassified <- function(out) {
  reason <- stringr::str_extract(
    out$method_moisture_regime,
    "(missing=.*|pet_not_positive)$"
  )
  unclassified <- !is.na(reason)
  if (!any(unclassified)) {
    return(out)
  }
  counts <- table(paste0(
    out$water_regime[unclassified],
    ", ",
    reason[unclassified]
  ))
  bullets <- rlang::set_names(
    paste0(names(counts), ": ", as.integer(counts)),
    rep("*", length(counts))
  )
  n_left <- sum(unclassified)
  n_rows <- nrow(out)
  cli::cli_inform(
    c(
      "{n_left} row{?s} of {n_rows} left without a moisture regime; nothing
       was zero-filled.",
      bullets
    ),
    class = "whep_moisture_regime_unclassified"
  )
  out
}

# The output schema, so every block and regime binds to the same columns.
.cmr_prototype <- function() {
  tibble::tibble(
    lon = double(),
    lat = double(),
    area_code = integer(),
    item_cbs_code = integer(),
    water_regime = character(),
    year = integer(),
    mat_c = double(),
    frost_days = double(),
    prec_mm = double(),
    pet_mm = double(),
    irrig_mm = double(),
    water_mm = double(),
    moisture_ratio = double(),
    tropical = logical(),
    moisture_regime = character(),
    method_moisture_regime = character(),
    climate_fill_km = double(),
    method_irrig_depth = character(),
    lpjml_crop = character(),
    lpjml_version = character(),
    lpjml_git_hash = character(),
    lpjml_run = character()
  )
}
