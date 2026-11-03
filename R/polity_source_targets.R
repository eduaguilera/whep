# Resolve source-reported crop targets before any geographic aggregation.
# Mappings are source-specific, with half-open intervals supplied by the
# authoritative source adapter. This helper validates matching; it cannot
# establish the documentary correctness of a mapping (issues #458 and #1483).
.prepare_polity_luh2 <- function(data) {
  targets <- .polity_source_targets(data$production, data$mapping) |>
    .attach_polity_crop_type(data$type_mapping)
  totals <- .polity_luh2_totals(
    data$luh2,
    data$support,
    unique(data$type_mapping$luh2_type),
    unique(targets$polity_code)
  )
  .allocate_polity_irrigation(targets, totals)
}

.attach_polity_crop_type <- function(targets, mapping) {
  .check_columns(mapping, c("item_prod_code", "luh2_type"), "type_mapping")
  if (
    anyNA(mapping$item_prod_code) ||
      anyDuplicated(mapping$item_prod_code) ||
      any(!targets$item_prod_code %in% mapping$item_prod_code)
  ) {
    cli::cli_abort(
      "Every target crop needs one unambiguous LUH2 type mapping.",
      class = "whep_source_territory"
    )
  }
  .check_source_target_strings(mapping, "luh2_type")
  dplyr::left_join(
    targets,
    dplyr::select(mapping, "item_prod_code", "luh2_type"),
    by = "item_prod_code",
    relationship = "many-to-one"
  )
}

.allocate_polity_irrigation <- function(targets, totals) {
  targets <- dplyr::left_join(
    targets,
    totals,
    by = c("year", "polity_code", "luh2_type"),
    relationship = "many-to-one"
  )
  if (anyNA(targets$irrig_ha)) {
    cli::cli_abort(
      "Crop targets lack supplied irrigation measurements for their territory.",
      class = "whep_source_territory"
    )
  }
  # Same LUH2-proportional method as prepare_country_areas(), now grouped on
  # the reporting polity. Zero harvested area explicitly requests no crop;
  # missing physical inputs have already failed, rather than become zeros.
  targets |>
    dplyr::mutate(
      .crop_total = sum(.data$harvested_area_ha),
      irrigated_area_ha = dplyr::if_else(
        .data$.crop_total > 0,
        pmin(
          .data$harvested_area_ha,
          .data$harvested_area_ha / .data$.crop_total * .data$irrig_ha
        ),
        0
      ),
      method_irrigation = "luh2_proportional",
      .by = c("year", "polity_code", "luh2_type")
    ) |>
    dplyr::select(-dplyr::all_of(c(".crop_total", "crop_ha", "irrig_ha")))
}

.polity_source_targets <- function(production, mapping) {
  .check_source_target_inputs(production, mapping)
  resolved <- production |>
    dplyr::mutate(.source_row = dplyr::row_number()) |>
    dplyr::left_join(
      mapping,
      by = dplyr::join_by(
        source,
        source_area_code,
        year >= start_year,
        year < end_year
      ),
      relationship = "many-to-many"
    )
  if (anyNA(resolved$polity_code) || anyDuplicated(resolved$.source_row)) {
    cli::cli_abort(
      "Each source observation needs exactly one year-valid territory map.",
      class = "whep_source_territory"
    )
  }
  keys <- c("year", "polity_code", "item_prod_code")
  if (anyDuplicated(resolved[keys])) {
    cli::cli_abort(
      "Several observations claim the same polity, year and crop.",
      class = "whep_source_territory"
    )
  }
  dplyr::select(resolved, -dplyr::all_of(".source_row"))
}

.check_source_target_inputs <- function(production, mapping) {
  .check_columns(
    production,
    c(
      "source",
      "source_area_code",
      "year",
      "item_prod_code",
      "harvested_area_ha",
      "territorial_basis"
    ),
    "production"
  )
  .check_columns(
    mapping,
    c("source", "source_area_code", "start_year", "end_year", "polity_code"),
    "mapping"
  )
  .check_source_target_strings(
    production,
    c(
      "source",
      "source_area_code",
      "territorial_basis"
    )
  )
  .check_source_target_strings(
    mapping,
    c(
      "source",
      "source_area_code",
      "polity_code"
    )
  )
  .check_source_target_values(production, mapping)
  if (
    any(production$territorial_basis != "source_reported") ||
      rlang::has_name(production, "polity_code") ||
      any(c(".source_row", "start_year", "end_year") %in% names(production))
  ) {
    cli::cli_abort(
      "Supply original source-reported targets, not relabelled reconstructions.",
      class = "whep_source_territory"
    )
  }
}

.check_source_target_strings <- function(data, columns) {
  valid <- vapply(
    data[columns],
    function(x) {
      is.character(x) && !anyNA(x) && all(nzchar(trimws(x)))
    },
    logical(1)
  )
  if (!all(valid)) {
    cli::cli_abort(
      "Source and territory identifiers must be nonempty character strings.",
      class = "whep_source_territory"
    )
  }
}

.check_source_target_values <- function(production, mapping) {
  years <- list(production$year, mapping$start_year, mapping$end_year)
  valid_years <- vapply(
    years,
    function(x) {
      is.numeric(x) && all(is.finite(x)) && all(x == trunc(x))
    },
    logical(1)
  )
  area <- production$harvested_area_ha
  valid_area <- is.numeric(area) && all(is.finite(area)) && all(area >= 0)
  if (
    !all(valid_years) ||
      !valid_area ||
      anyNA(production$item_prod_code) ||
      any(mapping$start_year >= mapping$end_year)
  ) {
    cli::cli_abort(
      "Targets need finite areas, crop keys and whole years in valid intervals.",
      class = "whep_source_territory"
    )
  }
}
