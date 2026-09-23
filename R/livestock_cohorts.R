#' Calculate cohort and production system distribution.
#'
#' @description
#' Distributes national herd totals across GLEAM-defined
#' cohorts and production systems using
#' `gleam_livestock_categories` and regional weight data.
#'
#' GLEAM supplies only the taxonomy here: which systems and cohorts each
#' species has. It supplies no herd shares. `gleam_livestock_categories` has no
#' share column, and the herd-parameter tables of the GLEAM 2.0 and 3.0
#' supplements give demographic rates and live weights, not a dairy/meat or
#' layer/broiler split of the herd (whep#1194).
#'
#' Where FAOSTAT already reports the herd split into the items
#' [build_primary_production()] carries (`"Cattle, dairy"` /
#' `"Cattle, non-dairy"`, `"Pigs"` / `"Hogs"` for market / breeding swine, and
#' `"Chickens, layers"` / `"Chickens, broilers"`), the whole herd goes to the
#' system its item names and no share is applied. Every other herd (buffalo,
#' sheep, goats, ducks, turkeys, geese, and any aggregate cattle, swine or
#' poultry label) is split by WHEP's default shares, which are **assumed,
#' unverified** placeholders with no source. The `method_system_share` column
#' says which of the two applied to each row.
#'
#' @param data Dataframe with `species`, `heads`, and
#'   optionally `iso3` or `region`.
#' @param system_shares Optional dataframe with `species_gen`,
#'   `system`, `system_share` columns. If `NULL`, a herd whose commodity names
#'   a production system goes wholly to it, and every other herd uses WHEP's
#'   assumed, unverified default shares. Supplying this overrides both, so the
#'   supplied shares are used verbatim.
#'
#' @return Dataframe expanded to cohort level with
#'   `cohort`, `system`, `cohort_heads`, and
#'   `cohort_fraction` columns, plus `method_system_share`: `"reported"` when
#'   the commodity itself names the system, `"assumed"` when WHEP's unsourced
#'   default split was applied, or `"supplied"` when `system_shares` was given.
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
    ) |>
    dplyr::mutate(
      method_system_share = if (use_default) "assumed" else "supplied"
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
#'
#' **Assumed, unverified (whep#1194).** These are WHEP's own round numbers and
#' have no source. They used to be attributed to GLEAM, but no GLEAM table WHEP
#' holds carries herd shares (see [calculate_cohorts_systems()]). They are kept
#' only so that a herd no reported item splits still reaches cohorts, and every
#' row they reach is stamped `method_system_share = "assumed"`.
#'
#' What each row still applies to, given that [.route_to_commodity_system()]
#' sends a herd whose commodity names its system wholly to that system:
#' * Cattle, Swine: only a herd under an aggregate label (`"Cattle"`,
#'   `"Swine"`). The production items are reported-split (cattle 960/961,
#'   swine 1049/1051).
#' * Poultry: ducks, turkeys, geese and an aggregate `"Poultry"` label. The
#'   chicken items are reported-split (1052/1053).
#' * Buffalo, Sheep, Goats: every herd. FAOSTAT publishes one stock item per
#'   species; its only dairy signal is the QCL "Milk Animals" element, a count
#'   of milked females. That is one cohort of the dairy system, not the
#'   system's share of the herd, so it does not give these values directly.
#' @noRd
.default_system_shares <- function() {
  # Assumed, unverified: no source for any value below (whep#1194).
  tibble::tribble(
    ~species_gen, ~system,     ~system_share,
    "Cattle",     "Dairy",              0.30,
    "Cattle",     "Beef",               0.70,
    "Buffalo",    "Dairy",              0.60,
    "Buffalo",    "Other",              0.40,
    "Sheep",      "Dairy",              0.20,
    "Sheep",      "Meat",               0.80,
    "Goats",      "Dairy",              0.30,
    "Goats",      "Meat",               0.70,
    "Swine",      "Breeding",           0.15,
    "Swine",      "Fattening",          0.85,
    "Poultry",    "Layers",             0.50,
    "Poultry",    "Broilers",           0.50
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
      system_share = dplyr::if_else(is.na(routed_system), system_share, 1),
      method_system_share = dplyr::if_else(
        is.na(routed_system),
        method_system_share,
        "reported"
      )
    ) |>
    dplyr::select(-routed_system, -subcategory)
}

#' Production-system subcategory named by a commodity, else `NA`.
#'
#' Unlike [.get_subcategory()], this returns `NA` (not "Non-Dairy") when the name
#' does not name a subcategory, so single-commodity species (e.g. "Buffalo")
#' keep the generic system blend instead of collapsing to one system.
#'
#' Swine joined cattle here at whep#1107. FAOSTAT reports the market and
#' breeding halves of the herd as separate stock items (1049 and 1051), so once
#' both reach production the herd is already split and the assumed
#' `Breeding 0.15 / Fattening 0.85` blend must not be applied on top of it --
#' that would book 15% of the reported *market* herd as breeding on top of the
#' reported breeding herd.
#'
#' Chickens joined at whep#1194 for the same reason. Production splits the
#' chicken stock into 1052 `"Chickens, layers"` and 1053 `"Chickens,
#' broilers"` by the emissions-domain stock items, and the assumed
#' `Layers 0.50 / Broilers 0.50` blend booked half of each reported flock in
#' the other system. Ducks, turkeys and geese name no system and keep it.
#' @noRd
.commodity_subcategory <- function(species) {
  is_swine <- .get_general_species(species) == "Swine"
  dplyr::case_when(
    .is_dairy(species) ~ "Dairy",
    stringr::str_detect(species, "(?i)non[- ]?dairy") ~ "Non-Dairy",
    is_swine & .is_breeding_swine(species) ~ "Breeding",
    is_swine ~ "Market",
    stringr::str_detect(species, "(?i)chicken.*layer") ~ "Layers",
    stringr::str_detect(species, "(?i)chicken.*broiler") ~ "Broilers",
    TRUE ~ NA_character_
  )
}

#' Production system each commodity subcategory routes to, by species.
#'
#' Cattle (dairy / non-dairy), swine (breeding / market) and chickens
#' (layers / broilers) are the species `animals_codes` splits into separate
#' commodities.
#' @noRd
.subcategory_system_map <- function() {
  tibble::tribble(
    ~species_gen, ~subcategory, ~system,
    "Cattle",     "Dairy",      "Dairy",
    "Cattle",     "Non-Dairy",  "Beef",
    "Swine",      "Breeding",   "Breeding",
    "Swine",      "Market",     "Fattening",
    "Poultry",    "Layers",     "Layers",
    "Poultry",    "Broilers",   "Broilers"
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
