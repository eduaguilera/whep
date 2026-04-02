#' Prepare production data for livestock emission calculations.
#'
#' @description
#' Bridge between `build_primary_production()` output and the
#' livestock emission functions. Maps species codes to names,
#' converts area codes to ISO3, extracts milk and meat yields,
#' and optionally expands herds into GLEAM cohorts.
#'
#' Any extra columns present in the input (e.g., `weight`,
#' `diet_quality`, `fat_percent`) are preserved and flow
#' through to the emission functions automatically.
#'
#' @param data A tibble from `build_primary_production()` with
#'   columns `item_cbs_code`, `unit`, `value`, and optionally
#'   `year`, `area_code`, `live_anim_code`.
#' @param expand_cohorts Logical. If `TRUE`, distributes herds
#'   across GLEAM cohorts and production systems via
#'   `calculate_cohorts_systems()`. Default `FALSE`.
#' @param system_shares Optional dataframe with custom system
#'   shares. Passed to `calculate_cohorts_systems()`.
#'
#' @return A tibble with columns `species`, `heads`, `iso3`
#'   (if `area_code` present), and optionally
#'   `milk_yield_kg_day`, `meat_yield_t_head`, cohort columns,
#'   plus all extra columns from the input.
#' @export
#'
#' @examples
#' \dontrun{
#'   build_primary_production() |>
#'     prepare_livestock_emissions() |>
#'     calculate_livestock_emissions(tier = 1)
#' }
prepare_livestock_emissions <- function(
    data,
    expand_cohorts = FALSE,
    system_shares = NULL) {
  .validate_production_input(data)

  animals <- animals_codes
  excluded <- .excluded_livestock_codes()

  # Identify livestock head rows
  heads_data <- data |>
    dplyr::filter(unit == "heads") |>
    dplyr::semi_join(animals, by = "item_cbs_code") |>
    .exclude_non_ipcc_species(excluded, animals)

  # Map species name from animals_codes
  species_map <- animals |>
    dplyr::select(item_cbs_code, species = item_cbs) |>
    dplyr::distinct(item_cbs_code, .keep_all = TRUE)

  heads_data <- heads_data |>
    dplyr::left_join(species_map, by = "item_cbs_code") |>
    dplyr::rename(heads = value)

  # Extract production yields (milk, meat)
  yields <- .extract_production_yields(data, animals)

  if (!is.null(yields)) {
    heads_data <- heads_data |>
      dplyr::left_join(
        yields,
        by = .yield_join_keys(heads_data, yields)
      )
  }

  # Map area_code -> iso3
  if (rlang::has_name(heads_data, "area_code")) {
    heads_data <- heads_data |>
      dplyr::left_join(
        polities |>
          dplyr::select(area_code, iso3 = iso3c),
        by = "area_code"
      )
  }

  if (expand_cohorts) {
    heads_data <- heads_data |>
      calculate_cohorts_systems(
        system_shares = system_shares
      )
  }

  heads_data
}

# Private helpers ----

#' @noRd
.validate_production_input <- function(data) {
  required <- c("item_cbs_code", "unit", "value")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      "Missing required column{?s}: {.field {missing}}."
    )
  }
}

#' @noRd
.excluded_livestock_codes <- function() {
  # Rabbits, Rodents, Animals live nes, Bees, Game
  c(1140L, 1150L, 1171L, 1181L, 1190L)
}

#' @noRd
.exclude_non_ipcc_species <- function(data, excluded,
                                      animals) {
  excluded_rows <- data |>
    dplyr::filter(item_cbs_code %in% excluded)

  if (nrow(excluded_rows) > 0) {
    dropped_names <- animals |> # nolint: object_usage.
      dplyr::filter(item_cbs_code %in% excluded) |>
      dplyr::pull(item_cbs) |>
      unique()
    cli::cli_inform(c(
      "i" = paste0(
        "Excluded {nrow(excluded_rows)} row{?s} with no ",
        "IPCC emission factors: ",
        "{.val {dropped_names}}."
      )
    ))
  }

  data |>
    dplyr::filter(!item_cbs_code %in% excluded)
}

#' @noRd
.extract_production_yields <- function(data, animals) {
  if (!rlang::has_name(data, "unit")) return(NULL)

  yield_rows <- data |>
    dplyr::filter(unit == "t_head")

  if (nrow(yield_rows) == 0) return(NULL)

  # Get product-to-animal mapping from animals_codes
  product_map <- animals |>
    dplyr::filter(!is.na(Item_Code_product)) |>
    dplyr::select(
      item_cbs_code,
      Item_Code_product,
      Liv_prod_cat
    ) |>
    dplyr::distinct()

  if (!rlang::has_name(yield_rows, "live_anim_code")) {
    return(NULL)
  }

  # Build animal-code lookup from product map
  anim_lookup <- product_map |>
    dplyr::mutate(
      live_anim_code = as.character(item_cbs_code)
    ) |>
    dplyr::select(live_anim_code, Liv_prod_cat)

  tagged <- yield_rows |>
    dplyr::inner_join(anim_lookup, by = "live_anim_code")

  if (nrow(tagged) == 0) return(NULL)

  # Milk yields
  milk_yields <- tagged |>
    dplyr::filter(Liv_prod_cat == "Milk") |>
    dplyr::mutate(
      item_cbs_code = as.integer(live_anim_code),
      milk_yield_kg_day = value * 1000 / 365
    ) |>
    dplyr::select(
      dplyr::any_of(c("year", "area_code")),
      item_cbs_code,
      milk_yield_kg_day
    )

  # Meat/egg yields
  meat_yields <- tagged |>
    dplyr::filter(Liv_prod_cat != "Milk") |>
    dplyr::mutate(
      item_cbs_code = as.integer(live_anim_code),
      meat_yield_t_head = value
    ) |>
    dplyr::select(
      dplyr::any_of(c("year", "area_code")),
      item_cbs_code,
      meat_yield_t_head
    )

  if (nrow(milk_yields) > 0 && nrow(meat_yields) > 0) {
    yields <- milk_yields |>
      dplyr::full_join(
        meat_yields,
        by = intersect(names(milk_yields), names(meat_yields))
      )
  } else if (nrow(milk_yields) > 0) {
    yields <- milk_yields
  } else {
    yields <- meat_yields
  }

  if (nrow(yields) == 0) return(NULL)
  yields
}

#' @noRd
.yield_join_keys <- function(heads_data, yields) {
  intersect(
    c("year", "area_code", "item_cbs_code"),
    intersect(names(heads_data), names(yields))
  )
}
