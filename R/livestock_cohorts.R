#' Calculate cohort and production system distribution.
#'
#' @description
#' Distributes national herd totals across GLEAM-defined
#' cohorts and production systems using
#' `gleam_livestock_categories` and regional weight data.
#'
#' @param data Dataframe with `species`, `heads`, and
#'   optionally `iso3` or `region`.
#' @param system_shares Optional dataframe with `species_gen`,
#'   `system`, `system_share` columns. If `NULL`, uses GLEAM
#'   defaults and routes dairy/non-dairy commodities to their
#'   matching production system. Supplying this overrides both,
#'   so the supplied shares are used verbatim.
#'
#' @return Dataframe expanded to cohort level with
#'   `cohort`, `system`, `cohort_heads`, and
#'   `cohort_fraction` columns.
#' @export
#'
#' @examples
#' tibble::tibble(
#'   species = "Cattle", heads = 10000,
#'   iso3 = "DEU"
#' ) |>
#'   calculate_cohorts_systems()
calculate_cohorts_systems <- function(data, system_shares = NULL) {
  categories <- gleam_livestock_categories

  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species)
    )

  use_default <- is.null(system_shares)
  if (use_default) {
    system_shares <- .default_system_shares()
  }

  # Join production systems
  data <- data |>
    dplyr::left_join(
      system_shares,
      by = "species_gen",
      relationship = "many-to-many"
    )

  # Default shares are keyed by general species, so both cattle commodities
  # ("Cattle, dairy" / "Cattle, non-dairy") would otherwise receive the same
  # generic Dairy/Beef blend (issue #109). When the commodity name itself names
  # a dairy/non-dairy subcategory, send the whole herd to that system instead.
  if (use_default) {
    data <- .route_to_commodity_system(data)
  }

  data <- data |>
    dplyr::mutate(
      system_heads = heads * system_share
    )

  # Join cohorts within each system. Key on the GENERAL species so that a species
  # whose raw GLEAM-category name differs from its general species (e.g. "Pigs" ->
  # "Swine") still matches the system-share table, which is keyed by general species.
  # Without this, pig cohort_fraction came back NA because raw "Pigs" never matched
  # the mapped "Swine" species_gen, so pigs dropped out entirely.
  cohort_fracs <- .get_cohort_fractions(categories)

  data |>
    dplyr::left_join(
      cohort_fracs,
      by = c(
        "species_gen" = "species_gen",
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

#' Route a herd to the production system named by its commodity.
#'
#' Default system shares are keyed by general species, so a dairy and a non-dairy
#' commodity of the same species share one blend. When the commodity name
#' identifies a dairy/non-dairy subcategory, keep only that subcategory's system
#' and give it the full share. Commodities that name no subcategory keep the
#' generic blend untouched.
#' @noRd
.route_to_commodity_system <- function(data) {
  routing <- .subcategory_system_map() |>
    dplyr::rename(routed_system = system)

  data |>
    dplyr::mutate(subcategory = .commodity_subcategory(species)) |>
    dplyr::left_join(routing, by = c("species_gen", "subcategory")) |>
    dplyr::filter(is.na(routed_system) | system == routed_system) |>
    dplyr::mutate(
      system_share = dplyr::if_else(is.na(routed_system), system_share, 1)
    ) |>
    dplyr::select(-routed_system, -subcategory)
}

#' Dairy/non-dairy subcategory named by a commodity, else `NA`.
#'
#' Unlike [.get_subcategory()], this returns `NA` (not "Non-Dairy") when the name
#' does not literally say dairy/non-dairy, so single-commodity species (e.g.
#' "Buffalo") keep the generic system blend instead of collapsing to one system.
#' @noRd
.commodity_subcategory <- function(species) {
  dplyr::case_when(
    .is_dairy(species) ~ "Dairy",
    stringr::str_detect(species, "(?i)non[- ]?dairy") ~ "Non-Dairy",
    TRUE ~ NA_character_
  )
}

#' Production system each commodity subcategory routes to, by species.
#'
#' Only cattle has separate dairy and non-dairy commodities in `animals_codes`.
#' @noRd
.subcategory_system_map <- function() {
  tibble::tribble(
    ~species_gen, ~subcategory, ~system,
    "Cattle", "Dairy", "Dairy",
    "Cattle", "Non-Dairy", "Beef"
  )
}

#' Number of animals a single emissions row represents.
#'
#' Per-head IPCC emissions are scaled to totals by the animal count of the row.
#' After [calculate_cohorts_systems()] expands a national herd, each row is one
#' GLEAM cohort, so that count is `cohort_heads` (national `heads` times the
#' cohort fraction), not the national `heads` the row still carries. Scaling
#' expanded rows by `heads` and then summing over cohorts inflated Tier 2
#' totals by the cohort count (issue #106). Without expansion only `heads` is
#' present and is itself the animal count.
#' @noRd
.animal_count <- function(data) {
  if (rlang::has_name(data, "cohort_heads")) {
    data$cohort_heads
  } else {
    data$heads
  }
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
    # Map to general species so the cohort key matches the system-share key
    # (system shares are keyed by general species, e.g. "Swine" not "Pigs").
    dplyr::mutate(species_gen = .get_general_species(species)) |>
    dplyr::select(
      species_gen,
      production_system,
      cohort,
      cohort_share
    )
}
