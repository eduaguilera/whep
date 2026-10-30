# Polity-native level-0 support (Refs #458 and #1483). Source-system codes
# are deliberately absent from the matching key. Existing area-code readers
# remain explicit compatibility paths while their consumers are migrated.
.read_polity_grid <- function(support, reference_year, options) {
  if (
    !identical(as.integer(options$level), 0L) ||
      options$vintage != "year_aware" ||
      !is.null(options$containment) ||
      !is.null(options$containers)
  ) {
    cli::cli_abort(
      "Polity-keyed grids currently require level 0 and year-aware support.",
      class = "whep_polity_grid_mode"
    )
  }
  grid <- .polity_grid_year_aware(support %||% read_polycell_support())
  if (!is.null(reference_year)) {
    grid <- .filter_country_grid_year(grid, reference_year)
  }
  grid
}

.polity_grid_year_aware <- function(support) {
  support <- tibble::as_tibble(support)
  .check_polity_grid_input(support)
  .level0_check_epochs(support)
  .check_polity_grid_claims(support)
  support |>
    dplyr::mutate(
      cell_land_ha = sum(.data$land_area_ha),
      .by = c("lon", "lat", "start_year", "end_year")
    ) |>
    dplyr::filter(.data$cell_land_ha > 0) |>
    dplyr::mutate(cell_area_frac = .data$land_area_ha / .data$cell_land_ha) |>
    dplyr::select(dplyr::all_of(c(
      "lon",
      "lat",
      "polity_code",
      "start_year",
      "end_year",
      "cell_area_ha",
      "land_area_ha",
      "cell_area_frac"
    )))
}

.check_polity_grid_input <- function(support) {
  .check_columns(
    support,
    c(
      "lon",
      "lat",
      "polity_code",
      "start_year",
      "end_year",
      "cell_area_ha",
      "land_area_ha"
    ),
    "support"
  )
  code <- support$polity_code
  if (!is.character(code) || anyNA(code) || any(!nzchar(trimws(code)))) {
    cli::cli_abort(
      "Every support row must carry a nonempty character polity code.",
      class = "whep_polity_grid_identity"
    )
  }
  .check_polity_grid_values(support)
  key <- c("lon", "lat", "polity_code", "start_year", "end_year")
  if (anyDuplicated(support[key])) {
    cli::cli_abort(
      "Support repeats a polity within a cell and validity interval.",
      class = "whep_polity_grid_duplicate"
    )
  }
}

.check_polity_grid_values <- function(support) {
  cols <- c(
    "lon",
    "lat",
    "start_year",
    "end_year",
    "cell_area_ha",
    "land_area_ha"
  )
  valid <- vapply(
    support[cols],
    function(x) {
      is.numeric(x) && all(is.finite(x))
    },
    logical(1)
  )
  if (
    nrow(support) == 0L ||
      !all(valid) ||
      any(support$cell_area_ha <= 0 | support$land_area_ha < 0) ||
      any(support$start_year >= support$end_year)
  ) {
    cli::cli_abort(
      "Support needs finite areas and valid, explicit nonempty intervals.",
      class = "whep_polity_grid_area"
    )
  }
  if (rlang::has_name(support, "support_role")) {
    if (
      anyNA(support$support_role) || any(support$support_role != "partition")
    ) {
      cli::cli_abort(
        "Polity-keyed allocation requires partition support, not overlaps.",
        class = "whep_polity_grid_role"
      )
    }
  }
}

.check_polity_grid_claims <- function(support) {
  # Exact measured excess is refused: no new tolerance or geometric repair
  # policy is introduced here. This one-sided test cannot certify a partition.
  claims <- support |>
    dplyr::summarise(
      capacity = min(.data$cell_area_ha),
      same_capacity = dplyr::n_distinct(.data$cell_area_ha) == 1L,
      claimed = sum(.data$land_area_ha),
      .by = c("lon", "lat", "start_year", "end_year")
    )
  if (any(!claims$same_capacity | claims$claimed > claims$capacity)) {
    cli::cli_abort(
      "Support has inconsistent cell areas or claims more land than a cell.",
      class = "whep_polity_grid_overclaim"
    )
  }
}
