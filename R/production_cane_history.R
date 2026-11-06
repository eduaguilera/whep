# Prepare a local historical_data artifact; no pin is published here.
# Source basis: Kurosaki (2011), Hi-Stat DP169, Tables8–11, pp7–10.
# https://gcoe.ier.hit-u.ac.jp/research/discussion/2008/gd10-169.html
# Interpolation and constant-yield land backcasting are explicit modelling
# assumptions, not observations. The rolling source holdouts are recorded in
# whep#1580; they do not validate long extrapolation or other territories.
.prepare_cane_history <- function(
  observations,
  land,
  method = c("source_anchored_land", "source_anchored_constant", "source_only")
) {
  method <- rlang::arg_match(method)
  .check_cane_history_inputs(observations, land)
  keys <- c("year", "reporting_polity_code")
  land |>
    dplyr::left_join(
      dplyr::select(observations, -"area_code"),
      by = keys,
      relationship = "one-to-one"
    ) |>
    dplyr::group_by(.data$reporting_polity_code) |>
    dplyr::group_modify(.reconstruct_cane_group, method = method) |>
    dplyr::ungroup() |>
    dplyr::mutate(item_prod_code = "156") |>
    tidyr::pivot_longer(
      c("ha", "tonnes"),
      names_to = "unit",
      values_to = "value"
    )
}

.check_cane_history_inputs <- function(observations, land) {
  obs_cols <- c(
    "year",
    "reporting_polity_code",
    "area_code",
    "ha",
    "tonnes",
    "source",
    "method_cane",
    "source_period",
    "source_footprint",
    "mass_basis"
  )
  land_cols <- c(
    "year",
    "reporting_polity_code",
    "area_code",
    "land_area_ha",
    "land_source"
  )
  if (
    !all(rlang::has_name(observations, obs_cols)) ||
      !all(rlang::has_name(land, land_cols))
  ) {
    cli::cli_abort(
      "Missing cane source or land columns.",
      class = "whep_error_cane_schema"
    )
  }
  if (nrow(observations) == 0L || nrow(land) == 0L) {
    cli::cli_abort(
      "Cane observations and land rows must be supplied.",
      class = "whep_error_cane_input"
    )
  }
  if (
    anyNA(observations$mass_basis) ||
      any(observations$mass_basis != "fresh_cane")
  ) {
    cli::cli_abort(
      "Cane source observations must declare fresh-cane mass.",
      class = "whep_error_cane_basis"
    )
  }
  if (
    !all(vapply(observations[c("ha", "tonnes")], is.numeric, logical(1))) ||
      !is.numeric(land$land_area_ha)
  ) {
    cli::cli_abort(
      "Cane and land quantities must be numeric.",
      class = "whep_error_cane_input"
    )
  }
  provenance <- observations[c(
    "source",
    "method_cane",
    "source_period",
    "source_footprint"
  )]
  if (anyNA(provenance) || any(!nzchar(as.matrix(provenance)))) {
    cli::cli_abort(
      "Missing cane source provenance.",
      class = "whep_error_cane_input"
    )
  }
  .check_cane_history_keys(observations)
  .check_cane_history_keys(land)
  .check_historical_identity(
    observations,
    observations$area_code,
    observations$year
  )
  .check_historical_identity(land, land$area_code, land$year)
  if (
    any(!is.finite(observations$ha) | observations$ha <= 0) ||
      any(!is.finite(observations$tonnes) | observations$tonnes < 0) ||
      any(!is.finite(land$land_area_ha) | land$land_area_ha < 0) ||
      anyNA(land$land_source) ||
      any(!nzchar(land$land_source))
  ) {
    cli::cli_abort(
      "Invalid cane quantity or missing land provenance.",
      class = "whep_error_cane_input"
    )
  }
  missing <- dplyr::anti_join(
    observations,
    land,
    by = c("year", "reporting_polity_code")
  )
  if (nrow(missing) > 0L) {
    cli::cli_abort(
      "Source anchor lacks a matching land row.",
      class = "whep_error_cane_land",
      coverage = missing
    )
  }
}

.check_cane_history_keys <- function(x) {
  keys <- dplyr::select(x, "year", "reporting_polity_code", "area_code")
  if (
    !is.numeric(keys$year) ||
      !is.numeric(keys$area_code) ||
      any(!is.finite(keys$year)) ||
      any(!is.finite(keys$area_code)) ||
      any(keys$area_code != round(keys$area_code)) ||
      any(keys$year != round(keys$year))
  ) {
    cli::cli_abort(
      "Cane years and area codes must be numeric year keys.",
      class = "whep_error_cane_keys"
    )
  }
  if (
    anyNA(keys) ||
      any(!nzchar(keys$reporting_polity_code)) ||
      nrow(dplyr::distinct(keys, .data$year, .data$reporting_polity_code)) !=
        nrow(keys)
  ) {
    cli::cli_abort(
      "Missing or duplicate canonical cane keys.",
      class = "whep_error_cane_keys"
    )
  }
}

.reconstruct_cane_group <- function(.x, .y, method) {
  x <- dplyr::arrange(.x, .data$year)
  observed <- !is.na(x$ha) & !is.na(x$tonnes)
  if (!any(observed) || (method == "source_only" && any(!observed))) {
    cli::cli_abort(
      "Cane source coverage is incomplete for {.val {(.y$reporting_polity_code)}}.",
      class = "whep_error_cane_coverage",
      coverage = dplyr::mutate(
        dplyr::filter(x, !observed),
        reporting_polity_code = .y$reporting_polity_code
      )
    )
  }
  x$cane_anchor_start <- x$year
  x$cane_anchor_end <- x$year
  x$cane_extrapolation_years <- 0L
  anchors <- dplyr::filter(x, observed)
  inside <- !observed & x$year > min(anchors$year) & x$year < max(anchors$year)
  x <- .interpolate_cane_history(x, anchors, inside)
  x <- .backcast_cane_history(x, anchors, !observed & !inside, method)
  x
}

.interpolate_cane_history <- function(x, anchors, inside) {
  if (!any(inside)) {
    return(x)
  }
  index <- findInterval(x$year[inside], anchors$year)
  x$cane_anchor_start[inside] <- anchors$year[index]
  x$cane_anchor_end[inside] <- anchors$year[index + 1L]
  x$ha[inside] <- stats::approx(
    anchors$year,
    anchors$ha,
    xout = x$year[inside]
  )$y
  x$tonnes[inside] <- stats::approx(
    anchors$year,
    anchors$tonnes,
    xout = x$year[inside]
  )$y
  x$method_cane[inside] <- "linear_area_production"
  x$source[inside] <- "historical_cane_linear_area_production"
  x$mass_basis[inside] <- "fresh_cane"
  x$source_period[inside] <- NA_character_
  x$source_footprint[inside] <- paste(
    "Interpolated between",
    anchors$source_footprint[index],
    "and",
    anchors$source_footprint[index + 1L]
  )
  x
}

.backcast_cane_history <- function(x, anchors, outside, method) {
  if (!any(outside)) {
    return(x)
  }
  selected <- ifelse(x$year[outside] < min(anchors$year), 1L, nrow(anchors))
  anchor <- anchors[selected, ]
  factor <- rep(1, nrow(anchor))
  if (method == "source_anchored_land") {
    if (any(anchor$land_area_ha <= 0)) {
      cli::cli_abort(
        "A land backcast anchor must have positive land area.",
        class = "whep_error_cane_land"
      )
    }
    factor <- x$land_area_ha[outside] / anchor$land_area_ha
  }
  x$cane_anchor_start[outside] <- anchor$year
  x$cane_anchor_end[outside] <- anchor$year
  x$cane_extrapolation_years[outside] <- abs(x$year[outside] - anchor$year)
  x$ha[outside] <- anchor$ha * factor
  x$tonnes[outside] <- anchor$tonnes * factor
  x$method_cane[outside] <- method
  x$source[outside] <- paste0("historical_cane_", method)
  x$mass_basis[outside] <- "fresh_cane"
  x$source_period[outside] <- anchor$source_period
  x$source_footprint[outside] <- anchor$source_footprint
  x
}

# Exact cane quantities and canonical keys must survive historical ingestion;
# matching row counts alone can hide a changed unit, value or territory.
.check_cane_ingestion <- function(artifact, prepared) {
  keys <- c("year", "reporting_polity_code", "item_prod_code", "unit")
  expected <- dplyr::select(artifact, dplyr::all_of(c(keys, "value")))
  observed <- add_polity_code(
    prepared,
    polity_code_column = "reporting_polity_code"
  ) |>
    dplyr::select(dplyr::all_of(c(keys, "value")))
  comparison <- dplyr::full_join(
    expected,
    observed,
    by = keys,
    suffix = c("_artifact", "_ingested"),
    relationship = "one-to-one"
  )
  if (
    nrow(comparison) != nrow(artifact) ||
      anyNA(comparison) ||
      any(comparison$value_artifact != comparison$value_ingested)
  ) {
    cli::cli_abort(
      "Historical ingestion changed cane identity, unit or value.",
      class = "whep_error_cane_ingestion",
      comparison = comparison
    )
  }
  invisible(NULL)
}
