#' Attribute grazing water with an explicit accounting basis.
#'
#' @description
#' Calculate water attributed to grazing from supplied evapotranspiration and
#' biomass or valuation inputs. This calculator preserves the input's spatial
#' and temporal context; it does not infer a reporting territory or annualize
#' a study period. It does not download data or choose missing parameters.
#'
#' `biomass_allocation` multiplies occupied-domain ET by consumed dry matter
#' divided by reference available dry matter. Heinke et al. (2020),
#' doi:10.1029/2019WR026995, Methods p. 3, use this attribution principle with
#' ET and maximum biomass yield from dedicated grazing-intensity simulations.
#' This function does not turn whole-plant NPP or realized herd intake into
#' that reference yield. The caller must identify the reference basis and
#' distinguish sourced estimates from explicit unverified assumptions.
#' Reference ET and biomass must describe the same management scenario: ET
#' from an actual stocking-rate run cannot silently be paired with a biomass
#' maximum from a different experiment.
#'
#' `occupation` attributes all supplied grassland ET to occupation of that
#' domain. `value_allocation` applies a supplied economic allocation fraction
#' to the same domain. Schyns et al. (2019), doi:10.1073/pnas.1817380116,
#' Supplement S3, describe economic attribution; this function applies the
#' supplied fraction and does not derive or validate its ecosystem valuation.
#' These methods answer different accounting questions. None is a silent
#' fallback for unavailable inputs to another method.
#'
#' @param data A tibble with `component` (`"green"` or `"blue"`),
#'   `grass_water_m3` (ET over the reference grassland domain), and
#'   `source_water`. Biomass attribution also requires `grass_intake_dm_t`,
#'   `reference_dm_t`, `source_intake`, `source_reference`, `reference_basis`
#'   and `reference_status` (`"sourced"` or `"assumed_unverified"`). Matching
#'   `water_scenario` and `reference_scenario` labels identify the common
#'   reference experiment. An assumed reference requires a nonempty
#'   `reference_assumption` description.
#'   Economic attribution requires `value_fraction` between zero and one and
#'   `source_fraction`. Quantities must share the same domain and period.
#'   Source labels record provenance assertions; they are not verification.
#'   The input must not already contain this function's output columns.
#' @param method Attribution method. The default `"biomass_allocation"`
#'   requires the supplied biomass reference. Alternatives are `"occupation"`
#'   and `"value_allocation"`.
#' @param excess Policy when intake exceeds reference biomass. `"abort"`
#'   (default) requires reconciliation of the inputs. `"extrapolate"` warns,
#'   preserves all intake, and reports an intensity-based water requirement
#'   which can exceed supplied ET. Such rows are not a physical ET partition.
#'
#' @return The input tibble with `allocation_fraction`, `water_attributed_m3`,
#'   `water_unallocated_m3`, `excess_intake_dm_t`, `quantity_basis` and
#'   `method_grazing_water`. Extrapolated excess rows have unknown unallocated
#'   water, recorded as `NA`, rather than a negative physical remainder.
#'   Non-biomass methods leave excess intake unknown. A declared zero intake
#'   and zero reference allocates zero water; positive intake against zero
#'   reference aborts under either policy.
#'   Green and blue rows can repeat the same intake/reference context. Do not
#'   sum those repeated biomass quantities across water components as separate
#'   feed. Compare methods by reusing the original inputs, not these results.
#'
#' @export
#'
#' @examples
#' # Illustrative input, not a scientific coefficient or reference value.
#' data <- tibble::tribble(
#'   ~component, ~grass_water_m3, ~source_water,
#'   "green",    1000,            "Illustrative ET fixture"
#' )
#' calculate_grazing_water(data, method = "occupation")
calculate_grazing_water <- function(
  data,
  method = c("biomass_allocation", "occupation", "value_allocation"),
  excess = c("abort", "extrapolate")
) {
  method <- rlang::arg_match(method)
  excess <- rlang::arg_match(excess)
  data <- .gwater_input(data) |>
    .gwater_numeric("grass_water_m3") |>
    .gwater_labels(c("component", "source_water"))
  if (any(!data$component %in% c("green", "blue"))) {
    .gwater_abort("Each component must be green or blue evapotranspiration.")
  }
  out <- switch(
    method,
    biomass_allocation = .gwater_biomass(data, excess),
    occupation = .gwater_occupation(data),
    value_allocation = .gwater_value(data)
  )
  dplyr::mutate(out, method_grazing_water = method)
}

.gwater_biomass <- function(data, excess) {
  data <- .gwater_numeric(data, c("grass_intake_dm_t", "reference_dm_t")) |>
    .gwater_labels(c(
      "source_intake",
      "source_reference",
      "reference_basis",
      "reference_status",
      "water_scenario",
      "reference_scenario"
    )) |>
    .gwater_reference()
  if (any(data$reference_dm_t == 0 & data$grass_intake_dm_t > 0)) {
    cli::cli_abort(
      "Positive grass intake cannot use a zero biomass reference.",
      class = "whep_grazing_zero_reference"
    )
  }
  data <- dplyr::mutate(
    data,
    allocation_fraction = dplyr::if_else(
      .data$grass_intake_dm_t == 0,
      0,
      .data$grass_intake_dm_t / .data$reference_dm_t
    ),
    excess_intake_dm_t = pmax(.data$grass_intake_dm_t - .data$reference_dm_t, 0)
  )
  .gwater_excess(data, excess)
}

.gwater_reference <- function(data) {
  if (any(data$water_scenario != data$reference_scenario)) {
    .gwater_abort("Reference biomass and ET must identify the same scenario.")
  }
  if (any(!data$reference_status %in% c("sourced", "assumed_unverified"))) {
    .gwater_abort("Declare each reference as sourced or assumed_unverified.")
  }
  assumed <- data$reference_status == "assumed_unverified"
  if (any(assumed)) {
    .gwater_labels(data[assumed, ], "reference_assumption")
  }
  data
}

.gwater_excess <- function(data, excess) {
  over <- data$excess_intake_dm_t > 0
  if (any(over)) {
    text <- "Grass intake exceeds its reference on {sum(over)} row{?s}."
    if (excess == "abort") {
      cli::cli_abort(text, class = "whep_grazing_reference_exceeded")
    }
    cli::cli_warn(
      c(text, i = "Extrapolated requirements are not a physical ET partition."),
      class = "whep_grazing_extrapolated"
    )
  }
  data |>
    .gwater_partition("biomass_attributed_et") |>
    dplyr::mutate(
      water_unallocated_m3 = dplyr::if_else(
        over,
        NA_real_,
        .data$water_unallocated_m3
      ),
      quantity_basis = dplyr::if_else(
        over,
        "extrapolated_grazing_requirement",
        .data$quantity_basis
      )
    )
}

.gwater_occupation <- function(data) {
  data |>
    dplyr::mutate(allocation_fraction = 1, excess_intake_dm_t = NA_real_) |>
    .gwater_partition("occupied_grassland_et")
}

.gwater_value <- function(data) {
  data <- .gwater_numeric(data, "value_fraction") |>
    .gwater_labels("source_fraction")
  if (any(data$value_fraction > 1)) {
    .gwater_abort("Economic allocation fractions must not exceed one.")
  }
  data |>
    dplyr::mutate(
      allocation_fraction = .data$value_fraction,
      excess_intake_dm_t = NA_real_
    ) |>
    .gwater_partition("economically_attributed_et")
}

.gwater_partition <- function(data, basis) {
  dplyr::mutate(
    data,
    water_attributed_m3 = .data$grass_water_m3 * .data$allocation_fraction,
    water_unallocated_m3 = .data$grass_water_m3 - .data$water_attributed_m3,
    quantity_basis = basis
  )
}

.gwater_input <- function(data) {
  generated <- c(
    "allocation_fraction",
    "water_attributed_m3",
    "water_unallocated_m3",
    "excess_intake_dm_t",
    "quantity_basis",
    "method_grazing_water"
  )
  present <- intersect(names(data), generated)
  if (length(present)) {
    .gwater_abort(
      "Input already contains output field{?s}: {.field {present}}."
    )
  }
  data
}

.gwater_numeric <- function(data, columns) {
  .gwater_require(data, columns)
  if (!all(vapply(data[columns], is.numeric, logical(1)))) {
    .gwater_abort("Water and biomass quantities must be numeric.")
  }
  values <- unlist(data[columns], use.names = FALSE)
  if (any(!is.finite(values) | values < 0)) {
    .gwater_abort("Water and biomass quantities must be known and nonnegative.")
  }
  data
}

.gwater_labels <- function(data, columns) {
  .gwater_require(data, columns)
  values <- unlist(data[columns], use.names = FALSE)
  if (any(is.na(values) | !nzchar(stringr::str_trim(values)))) {
    .gwater_abort("Source, basis and assumption labels must be nonempty.")
  }
  data
}

.gwater_require <- function(data, columns) {
  missing <- columns[!rlang::has_name(data, columns)]
  if (length(missing)) {
    .gwater_abort("Missing grazing-water input{?s}: {.field {missing}}.")
  }
}

.gwater_abort <- function(message) {
  cli::cli_abort(
    message,
    class = "whep_grazing_water_input",
    .envir = rlang::caller_env()
  )
}
