#' Calculate cohort and production system distribution.
#'
#' @description
#' Distributes national herd totals across GLEAM-defined
#' cohorts and production systems using
#' `gleam_livestock_categories` and regional weight data.
#'
#' @param data Dataframe with `species`, `heads`, and
#'   optionally `iso3` or `region`.
#' @param system_shares Optional dataframe with `species`,
#'   `system`, `share` columns. If `NULL`, uses
#'   GLEAM defaults.
#'
#' @return Dataframe expanded to cohort level with
#'   `cohort`, `system`, `cohort_heads`, and
#'   `cohort_fraction` columns.
#' @export
#'
#' @examples
#' \dontrun{
#'   tibble::tibble(
#'     species = "Cattle", heads = 10000,
#'     iso3 = "DEU"
#'   ) |>
#'     calculate_cohorts_systems()
#' }
calculate_cohorts_systems <- function(data,
                                      system_shares = NULL) {
  categories <- gleam_livestock_categories

  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species)
    )

  if (is.null(system_shares)) {
    system_shares <- .default_system_shares()
  }

  # Join production systems
  data <- data |>
    dplyr::left_join(
      system_shares,
      by = "species_gen",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      system_heads = heads * system_share
    )

  # Join cohorts within each system
  cohort_fracs <- .get_cohort_fractions(categories)

  data |>
    dplyr::left_join(
      cohort_fracs,
      by = c(
        "species_gen" = "species",
        "system" = "production_system"
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      cohort_fraction = system_share * cohort_share,
      cohort_heads = heads * cohort_fraction
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "system_heads",
        "system_share",
        "cohort_share"
      ))
    )
}

# Private helpers ----

#' Default production system shares.
#' @noRd
.default_system_shares <- function() {
  tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Cattle", "Dairy", 0.30,
    "Cattle", "Beef", 0.70,
    "Buffalo", "Dairy", 0.60,
    "Buffalo", "Other", 0.40,
    "Sheep", "Dairy", 0.20,
    "Sheep", "Meat", 0.80,
    "Goats", "Dairy", 0.30,
    "Goats", "Meat", 0.70,
    "Swine", "Breeding", 0.15,
    "Swine", "Fattening", 0.85,
    "Poultry", "Layers", 0.50,
    "Poultry", "Broilers", 0.50
  )
}

#' Get cohort fractions within each production system.
#' @noRd
.get_cohort_fractions <- function(categories) {
  categories |>
    dplyr::mutate(
      n_cohorts = dplyr::n(),
      cohort_share = 1 / n_cohorts,
      .by = c(species, production_system)
    ) |>
    dplyr::select(
      species, production_system, cohort, cohort_share
    )
}
