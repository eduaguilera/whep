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
# 3. The water term: `precip` (precipitation) or `precip_irrig`
#    (precipitation plus the irrigation depth of the row's crop and regime).
# 4. Optional, one per side of the record a window reaches past:
#    `climatology_<first>_<last>_<n>of<m>`, meaning n of the window's m years
#    lie outside the climate record and took the held climatology of years
#    first-last (the first 30 years of the record before it, the last 30
#    after it), the rule `build_cell_climate_zone()` follows.
# 5. Optional, only on an unclassified row (`moisture_regime` is NA):
#    `missing=<col>[+<col>...]`, the inputs whose window mean is unavailable
#    because at least one window year has no value or no row; and/or
#    `pet_not_positive`, a window-mean PET of zero, for which the ratio is
#    undefined. Nothing is zero-filled.

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
# Returns one row per series (`lon`, `lat` and the optional `item_cbs_code`,
# `water_regime`) and requested year, carrying the window-mean inputs that
# were classified, `water_mm`, `moisture_ratio` (water over PET), `tropical`,
# `moisture_regime` (`"wet"`, `"dry"` or NA) and `method_moisture_regime`.
# Other input columns are not carried.
.classify_moisture_regime <- function(
  data,
  years = NULL,
  window = c("trailing_30", "annual", "normal"),
  water = c("precip_irrig", "precip"),
  # 1991-2020 is an assumed choice, the most recent complete 30-year period;
  # no source used here prescribes it.
  normal_period = 1991:2020
) {
  window <- rlang::arg_match(window)
  water <- rlang::arg_match(water)
  inputs <- .check_moisture_inputs(data, water)
  covered <- sort(unique(inputs$year))
  frame <- .moisture_frame(
    .resolve_climate_years(years, covered),
    window,
    normal_period,
    covered
  )
  .moisture_series_years(inputs, frame$years) |>
    .join_window_climate(inputs, frame) |>
    .join_window_irrigation(inputs, frame, water) |>
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
  c("lon", "lat", intersect(c("item_cbs_code", "water_regime"), names(data)))
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
.join_window_irrigation <- function(rows, inputs, frame, water) {
  force(rows)
  if (water == "precip") {
    return(rows)
  }
  key <- .moisture_series_key(inputs)
  has_regime <- rlang::has_name(inputs, "water_regime")
  irrigated <- if (has_regime) inputs[water_regime == "irrigated"] else inputs
  if (nrow(irrigated) == 0L) {
    # Every row is rainfed, so every irrigation term is the structural zero.
    return(rows[, irrig_mm := 0])
  }
  means <- .moisture_window_means(
    irrigated[, c(key, "year", "irrig_mm"), with = FALSE],
    key,
    "irrig_mm",
    frame
  )
  out <- means[rows, on = c(key, "year")]
  if (has_regime) {
    out[water_regime == "rainfed", irrig_mm := 0]
  }
  out
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
    dplyr::left_join(.moisture_year_labels(frame, water), by = "year") |>
    dplyr::mutate(
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
