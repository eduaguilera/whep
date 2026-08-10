#' Normalise a country grid for polity-compartment spatialization.
#' @noRd
.normalize_country_grid <- function(country_grid) {
  .check_columns(country_grid, c("lon", "lat", "area_code"), "country_grid")

  country_grid <- tibble::as_tibble(country_grid)
  frac_col <- intersect(
    c(
      "cell_area_frac",
      "polity_frac",
      "area_frac",
      "country_frac",
      "landfrac"
    ),
    names(country_grid)
  )

  if (length(frac_col) == 0L) {
    country_grid <- dplyr::mutate(country_grid, cell_area_frac = 1)
  } else if (frac_col[[1L]] != "cell_area_frac") {
    country_grid <- dplyr::mutate(
      country_grid,
      cell_area_frac = .data[[frac_col[[1L]]]]
    )
  }

  country_grid |>
    dplyr::mutate(
      area_code = as.integer(area_code),
      cell_area_frac = as.numeric(cell_area_frac),
      cell_area_frac = dplyr::coalesce(cell_area_frac, 1),
      cell_area_frac = pmin(pmax(cell_area_frac, 0), 1)
    )
}

#' Warn about reporting areas the grid cannot represent at all.
#'
#' `.warn_unallocated_crops()` fires per (country, crop) per year and
#' `.warn_unallocated_livestock()` per (species, year), so a reporting area
#' the `country_grid` holds no cell for **at all** is reported as more of the
#' same routine per-crop leakage and is impossible to separate from it. That is
#' exactly the failure mode a `country_grid` substitution produces: two grids
#' rasterized through different vintages of `regions.csv` disagree about which
#' reporting codes exist, and every area whose code the new grid does not use
#' loses its entire national total in silence. Report it once per call, on its
#' own, with the national quantity at stake.
#' @noRd
.warn_grid_missing_reporters <- function(
  national,
  country_grid,
  value_col,
  quantity
) {
  codes <- sort(setdiff(
    unique(as.integer(national$area_code)),
    unique(as.integer(country_grid$area_code))
  ))
  if (length(codes) == 0L) {
    return(invisible(NULL))
  }
  at_stake <- .grid_missing_quantity(national, codes, value_col)
  stake_msg <- if (is.na(at_stake)) {
    ""
  } else {
    paste0(", carrying ", round(at_stake), " ", quantity)
  }
  # `codes` is integer, so the plural marker must follow an explicit scalar
  # count: cli's make_quantity() errors on a numeric vector of length > 1.
  cli::cli_warn(c(
    paste0(
      "{length(codes)} reporting area{?s} in the national table have no ",
      "cell in {.arg country_grid} at all",
      stake_msg,
      ":"
    ),
    "x" = "{length(codes)} area_code{?s}: {.val {codes}}.",
    "i" = "Their whole national total is dropped. A jump here between two
       {.arg country_grid} tables means the grids disagree about which
       reporting codes exist, not that the allocation improved."
  ))
}

#' National quantity carried by area codes the grid cannot represent.
#' @noRd
.grid_missing_quantity <- function(national, codes, value_col) {
  if (!rlang::has_name(national, value_col)) {
    return(NA_real_)
  }
  national |>
    dplyr::filter(as.integer(area_code) %in% codes) |>
    dplyr::pull(dplyr::all_of(value_col)) |>
    sum(na.rm = TRUE)
}

#' Columns that identify a polity compartment within a physical cell.
#' @noRd
.compartment_id_cols <- function(data) {
  intersect(c("polycell_id", "cell_id", "area_code"), names(data))
}

#' Join/grouping columns for a compartment-resolved cell.
#' @noRd
.compartment_cell_cols <- function(data) {
  unique(c(.compartment_id_cols(data), "lon", "lat"))
}

#' Validate the area key a spatialize engine was asked for.
#' @noRd
.resolve_spatialize_area_key <- function(area_key) {
  if (is.null(area_key)) {
    area_key <- "grid"
  }
  rlang::arg_match0(area_key, c("grid", "polity_area"), arg_nm = "area_key")
}

#' Apply the requested area key to a spatialized output.
#'
#' The spatialize chain allocates *from* a national table keyed on
#' `area_code` and *into* a `country_grid` keyed the same way, so both sides
#' speak the raw reporting vocabulary the grid was rasterized in. WHEP's
#' polity-keyed national tables are aggregated on `polity_area_code` instead,
#' so a reporting code that is not itself a bucket leaves the output carrying
#' two territorial keys that disagree within one row -- `area_code = 276`
#' beside `polity_area_code = 206` -- and a consumer's choice of join column
#' silently decides whether Sudan exists in its result (whep#582).
#'
#' `"grid"` is today's behaviour, kept as the default because switching moves
#' published values for every consumer joining gridded output on `area_code`;
#' it only gains the diagnostic that today's silence hides. The alternative is
#' selected, never a fallback.
#' @noRd
.spatialize_apply_area_key <- function(result, area_key, value_cols) {
  if (area_key == "grid") {
    .warn_cell_polity_off_bucket(result)
    return(result)
  }
  .spatialize_to_bucket(result, value_cols)
}

#' Re-key a spatialized output on `polity_area_code`.
#'
#' THE RAW CODE IS CARRIED, NOT REPLACED, exactly as `build_cell_polity()`
#' does under `area_key = "polity_area"` (whep#579): the reporting code the
#' engine allocated on arrives as an added `grid_area_code`, so the fold this
#' performs stays recoverable at the join instead of becoming irrecoverable in
#' the output. Where two reporting areas of one bucket meet in a cell their
#' rows collapse, their values are summed, and the raw codes are joined with a
#' separator rather than one of them being picked -- picking would be the
#' silent half of the same problem. Codes absent from the crosswalk keep their
#' own code, so a gap stays visible rather than turning into an `NA` key.
#' @noRd
.spatialize_to_bucket <- function(result, value_cols) {
  value_cols <- intersect(value_cols, names(result))
  dt <- data.table::as.data.table(result)
  dt[, area_code := as.integer(area_code)]
  lookup <- data.table::as.data.table(.cell_polity_bucket_lookup())
  dt[lookup, polity_bucket := i.polity_area_code, on = "area_code"]
  dt[, grid_area_code := area_code]
  dt[!is.na(polity_bucket), area_code := polity_bucket]
  dt[, polity_bucket := NULL]
  group_cols <- setdiff(names(dt), c(value_cols, "grid_area_code"))
  out <- dt[,
    c(
      lapply(.SD, sum, na.rm = TRUE),
      list(
        grid_area_code = paste(sort(unique(grid_area_code)), collapse = "+")
      )
    ),
    by = group_cols,
    .SDcols = value_cols
  ]
  data.table::setcolorder(
    out,
    c(intersect(names(result), names(out)), "grid_area_code")
  )
  tibble::as_tibble(out)
}

#' Detect whether a country grid changes through time.
#' @noRd
.country_grid_is_dynamic <- function(country_grid) {
  any(
    c(
      "year",
      "valid_from",
      "valid_to",
      "start_year",
      "end_year",
      "from_year",
      "to_year"
    ) %in%
      names(country_grid)
  )
}

#' Select the polity-cell rows valid for one simulation year.
#' @noRd
.filter_country_grid_year <- function(country_grid, yr) {
  if ("year" %in% names(country_grid)) {
    return(dplyr::filter(country_grid, .data$year == yr))
  }

  start_col <- intersect(
    c("valid_from", "start_year", "from_year"),
    names(country_grid)
  )
  end_col <- intersect(
    c("valid_to", "end_year", "to_year"),
    names(country_grid)
  )

  if (length(start_col) > 0L || length(end_col) > 0L) {
    start_vals <- if (length(start_col) > 0L) {
      country_grid[[start_col[[1L]]]]
    } else {
      -Inf
    }
    end_vals <- if (length(end_col) > 0L) {
      country_grid[[end_col[[1L]]]]
    } else {
      Inf
    }
    start_vals[is.na(start_vals)] <- -Inf
    end_vals[is.na(end_vals)] <- Inf
    return(country_grid[yr >= start_vals & yr <= end_vals, , drop = FALSE])
  }

  country_grid
}

#' Build the static crop-pattern by country-compartment table.
#' @noRd
.build_base_grid_cp <- function(
  country_grid,
  crop_patterns,
  type_lookup = NULL
) {
  cg_dt <- data.table::as.data.table(country_grid)
  cp_dt <- data.table::as.data.table(crop_patterns)
  base_grid_cp <- cg_dt[cp_dt, on = .(lon, lat), allow.cartesian = TRUE]
  base_grid_cp[,
    harvest_fraction := data.table::fifelse(
      is.na(harvest_fraction),
      0,
      harvest_fraction
    )
  ]
  if (!is.null(type_lookup)) {
    tlu_dt <- data.table::as.data.table(type_lookup)
    base_grid_cp[tlu_dt, luh2_type := i.luh2_type, on = .(item_prod_code)]
  }
  data.table::setkey(base_grid_cp, lon, lat)
  base_grid_cp
}
