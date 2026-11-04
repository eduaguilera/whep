#' Validate annual livestock consumption against cohort exposure.
#'
#' @description
#' Adapt extensive direct-water quantities to an annual consumption ledger,
#' matching an independently supplied cohort inventory before any aggregation.
#' Drinking requirements and free drinking intake remain distinct from net
#' consumptive blue water. FAO (2019), *Water use of livestock production
#' systems and supply chains*, sections 4.5.4–4.5.5, distinguishes supplied,
#' returned and evaporated water (ISBN 978-92-5-131713-6). No return fraction,
#' animal-day exposure, year or territory is inferred by this adapter.
#'
#' Source and inventory scope declarations are attestations, not proof of
#' applicability or completeness. The inventory must enumerate all cohorts
#' for the stated target scope independently of rate availability. Annual
#' snapshot heads are not automatically annual animal-days. This function
#' preserves canonical identities and does not aggregate compatibility buckets.
#'
#' @param ledger Tibble of extensive water quantities, for example from
#'   [calculate_livestock_water()]. It must carry the inventory matching
#'   columns below, `quantity_basis`, `consumption_m3`,
#'   `coverage_direct_water`, `consumption_source`, `consumption_basis`,
#'   `water_origin`, `consumption_boundary` and `boundary_source`.
#'   Eligible quantities have complete drinking/service coverage, known
#'   nonnegative consumption, `water_origin = "surface_or_groundwater"`,
#'   and `consumption_boundary = "net_freshwater_consumption"`, describing
#'   withdrawals less returns to the same relevant freshwater system and
#'   assessment interval. A directly supplied `"net_consumptive_blue_water"`
#'   quantity requires its own consumption provenance. A `"withdrawal"`
#'   ledger additionally requires drinking/service volumes and explicit
#'   fractions, whose calculation must match the supplied consumption.
#'   Requirement and intake rows can be retained under partial coverage but
#'   never become consumptive water. Unknown metadata may be supplied as NA.
#' @param inventory Tibble unique by `exposure_id`, with `year`, canonical
#'   `reporting_polity_code`, `item_cbs_code`, `cohort`, `production_system`,
#'   `animal_days`, `period_start`, `period_end`, `inventory_source`, and
#'   `inventory_scope`. Dates must be Date vectors within the single declared
#'   calendar year. Each ledger ID must match these identity/exposure columns
#'   exactly; missing ledger IDs remain visible as missing exposure. Inventory
#'   metadata are retained in the `exposure_inventory` attribute.
#' @param coverage `"abort"` requires eligible quantities for every inventory
#'   row. `"partial"` warns and retains all inventory rows, with unknown
#'   `impact_u` for uncovered rows. It does not declare a complete footprint.
#' @return A tibble retaining ledger fields and inventory identities, with
#'   `coverage_status`, `impact_u` (eligible consumptive cubic metres only),
#'   `impact_quantity_basis`, `method_livestock_water` and `polity_year_basis`.
#'   The source ledger and independent inventory are retained as attributes.
#'   Structural mismatches
#'   and invalid annual scope always abort, even under partial coverage.
#' @export
#' @examples
#' # Entirely illustrative quantities, not observed farm data.
#' identity <- tibble::tibble(year = 2000L, area_code = 203L) |>
#'   add_polity_code()
#' inventory <- tibble::tibble(
#'   exposure_id = "illustrative", year = 2000L,
#'   reporting_polity_code = identity$polity_code, item_cbs_code = 960L,
#'   cohort = "lactating", production_system = "illustrative",
#'   animal_days = 100, period_start = as.Date("2000-01-01"),
#'   period_end = as.Date("2000-12-31"), inventory_source = "Toy inventory",
#'   inventory_scope = "Toy herd with one cohort"
#' )
#' ledger <- dplyr::mutate(
#'   inventory, quantity_basis = "net_consumptive_blue_water",
#'   consumption_m3 = 3, coverage_direct_water = "complete",
#'   consumption_source = "Toy water balance", consumption_basis = "assumed",
#'   water_origin = "surface_or_groundwater",
#'   consumption_boundary = "net_freshwater_consumption",
#'   boundary_source = "Toy matched supply and returns"
#' )
#' as_annual_livestock_water(ledger, inventory)
as_annual_livestock_water <- function(
  ledger,
  inventory,
  coverage = c("abort", "partial")
) {
  coverage <- rlang::arg_match(coverage)
  keys <- c(
    "exposure_id",
    "year",
    "reporting_polity_code",
    "item_cbs_code",
    "cohort",
    "production_system",
    "animal_days",
    "period_start",
    "period_end"
  )
  .annual_water_scope(ledger, keys)
  .annual_water_scope(inventory, keys)
  .annual_water_labels(inventory, c("inventory_source", "inventory_scope"))
  validated <- .annual_water_polity(inventory)
  .annual_water_match(ledger, inventory, keys)
  .annual_water_columns(
    ledger,
    c(
      "quantity_basis",
      "consumption_m3",
      "coverage_direct_water",
      "consumption_source",
      "consumption_basis",
      "water_origin",
      "consumption_boundary",
      "boundary_source"
    )
  )
  reserved <- c(
    "impact_u",
    "impact_quantity_basis",
    "coverage_status",
    "method_livestock_water",
    "polity_year_basis",
    ".record_present"
  )
  if (length(intersect(names(ledger), reserved))) {
    .annual_water_abort("Generated annual-water columns already exist.")
  }
  out <- validated |>
    dplyr::select(dplyr::all_of(c(keys, "polity_year_basis"))) |>
    dplyr::left_join(
      dplyr::mutate(ledger, .record_present = TRUE),
      by = keys,
      relationship = "one-to-one"
    ) |>
    .annual_water_status(coverage)
  attr(out, "exposure_inventory") <- inventory
  attr(out, "source_water_ledger") <- ledger
  out
}

.annual_water_scope <- function(data, keys) {
  .annual_water_columns(data, keys, "whep_annual_water_scope")
  .annual_water_labels(
    data,
    c(
      "exposure_id",
      "reporting_polity_code",
      "cohort",
      "production_system"
    )
  )
  dates <- inherits(data$period_start, "Date") &&
    inherits(data$period_end, "Date")
  year <- data$year
  valid <- is.numeric(year) && all(is.finite(year) & year == trunc(year))
  if (!dates || !valid || anyNA(data[keys])) {
    .annual_water_abort(
      "Declare resolved annual identities and Date intervals.",
      "whep_annual_water_scope"
    )
  }
  within <- format(data$period_start, "%Y") == as.character(year) &
    format(data$period_end, "%Y") == as.character(year) &
    data$period_start <= data$period_end
  if (!all(within)) {
    .annual_water_abort(
      "Exposure intervals must lie in the declared year.",
      "whep_annual_water_scope"
    )
  }
  .annual_water_numbers(data$animal_days)
  .annual_water_numbers(data$item_cbs_code)
}

.annual_water_match <- function(ledger, inventory, keys) {
  invalid <- anyDuplicated(ledger$exposure_id) ||
    anyDuplicated(inventory$exposure_id) ||
    nrow(dplyr::anti_join(ledger, inventory, by = keys)) > 0L
  if (invalid) {
    .annual_water_abort(
      "Exposure IDs must be unique and match inventory identity and exposure.",
      "whep_annual_water_identity"
    )
  }
}

.annual_water_polity <- function(data) {
  index <- match(data$reporting_polity_code, polities$polity_code)
  reported <- .polity_crosswalk(include_unmapped = FALSE) |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$map_year_end)) |>
    dplyr::summarise(
      map_year_end = max(.data$map_year_end),
      .by = "polity_code"
    )
  open <- data$reporting_polity_code %in% .open_polity_codes()
  end <- .polity_join_end_year(polities$end_year[index], NA_integer_, open)
  reporting_end <- .polity_join_end_year(
    polities$end_year[index],
    reported$map_year_end[match(
      data$reporting_polity_code,
      reported$polity_code
    )],
    open
  )
  valid <- !is.na(index) &
    !polities$wiki_status[index] %in% c("retired", "superseded") &
    data$year >= polities$start_year[index] &
    data$year < reporting_end
  if (anyNA(valid) || !all(valid)) {
    .annual_water_abort(
      "Annual water requires a current canonical polity.",
      "whep_annual_water_polity"
    )
  }
  dplyr::mutate(
    data,
    polity_year_basis = dplyr::if_else(
      .data$year < end,
      "territorial_period",
      "declared_reporting_year"
    )
  )
}

.annual_water_status <- function(data, coverage) {
  .annual_water_numbers(data$consumption_m3, allow_na = TRUE)
  withdrawal <- which(
    data$quantity_basis == "withdrawal" & !is.na(data$consumption_m3)
  )
  if (length(withdrawal)) {
    .annual_water_withdrawal(data[withdrawal, ])
  }
  provenance <- .annual_water_known(data$consumption_source) &
    .annual_water_known(data$consumption_basis) &
    .annual_water_known(data$boundary_source)
  out <- dplyr::mutate(
    data,
    coverage_status = dplyr::case_when(
      is.na(.data$.record_present) ~ "missing_exposure",
      !.data$quantity_basis %in% c("withdrawal", "net_consumptive_blue_water") ~
        "incompatible_quantity",
      is.na(.data$coverage_direct_water) |
        .data$coverage_direct_water != "complete" ~ "incomplete_components",
      is.na(.data$water_origin) |
        .data$water_origin != "surface_or_groundwater" ~ "incompatible_origin",
      is.na(.data$consumption_boundary) |
        .data$consumption_boundary != "net_freshwater_consumption" |
        !provenance ~ "unresolved_boundary",
      is.na(.data$consumption_m3) ~ "missing_consumption",
      .default = "matched"
    ),
    impact_u = dplyr::if_else(
      .data$coverage_status == "matched",
      .data$consumption_m3,
      NA_real_
    ),
    impact_quantity_basis = dplyr::if_else(
      .data$coverage_status == "matched",
      "net_consumptive_blue_water",
      NA_character_
    ),
    method_livestock_water = "supplied_annual_consumption"
  ) |>
    dplyr::select(-dplyr::all_of(".record_present"))
  missing <- sum(out$coverage_status != "matched")
  if (missing > 0) {
    text <- "Annual livestock consumption does not cover {missing} exposure(s)."
    if (coverage == "abort") {
      .annual_water_abort(text, "whep_annual_water_coverage")
    }
    cli::cli_warn(text, class = "whep_annual_water_partial")
  }
  out
}

.annual_water_withdrawal <- function(data) {
  columns <- c(
    "drinking_m3",
    "service_m3",
    "drinking_fraction",
    "service_fraction"
  )
  .annual_water_columns(data, columns)
  .annual_water_numbers(unlist(data[columns], use.names = FALSE))
  calculated <- data$drinking_m3 *
    data$drinking_fraction +
    data$service_m3 * data$service_fraction
  if (
    any(data$drinking_fraction > 1 | data$service_fraction > 1) ||
      any(!is.na(data$consumption_m3) & data$consumption_m3 != calculated)
  ) {
    .annual_water_abort("Consumption must match explicit withdrawal fractions.")
  }
}

.annual_water_known <- function(value) {
  is.character(value) &
    !is.na(value) &
    nzchar(stringr::str_trim(as.character(value)))
}

.annual_water_labels <- function(data, columns) {
  .annual_water_columns(data, columns)
  if (!all(.annual_water_known(unlist(data[columns], use.names = FALSE)))) {
    .annual_water_abort(
      "Annual scope and inventory source labels are required."
    )
  }
}

.annual_water_columns <- function(
  data,
  columns,
  class = "whep_annual_water_input"
) {
  if (!all(rlang::has_name(data, columns))) {
    .annual_water_abort("Required annual-water columns are missing.", class)
  }
}

.annual_water_numbers <- function(value, allow_na = FALSE) {
  valid <- is.numeric(value) &&
    all(
      (is.finite(value) & value >= 0) |
        (allow_na & is.na(value) & !is.nan(value))
    )
  if (!valid) {
    .annual_water_abort("Annual quantities must be nonnegative finite numbers.")
  }
}

.annual_water_abort <- function(message, class = "whep_annual_water_input") {
  cli::cli_abort(message, class = class, .envir = rlang::caller_env())
}
