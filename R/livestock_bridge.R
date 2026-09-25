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
#' tibble::tibble(
#'   item_cbs_code = 961,
#'   unit = "heads",
#'   value = 5000,
#'   area_code = 79L
#' ) |>
#'   prepare_livestock_emissions()
prepare_livestock_emissions <- function(
  data,
  expand_cohorts = FALSE,
  system_shares = NULL
) {
  .validate_production_input(data)
  .check_head_unit(data)
  data <- .as_livestock_tibble(data)

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
    area_bridge <- .current_area_lookup(include_unmapped = FALSE) |>
      tibble::as_tibble() |>
      dplyr::select(area_code, iso3 = area_iso3c) |>
      dplyr::filter(!is.na(.data$iso3)) |>
      dplyr::distinct(.data$area_code, .keep_all = TRUE)

    heads_data <- heads_data |>
      dplyr::left_join(
        area_bridge,
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

#' Take an exported livestock entry point's input onto the package's tibble
#' contract.
#'
#' `get_primary_production(years = ...)` returns a `data.table` --
#' `.filter_years()` converts -- and dplyr carries that class through every
#' verb, so the emission engines used to receive one. Two of them abort on it,
#' and on the class alone rather than on the data:
#' `.ensure_production_cols()` adds its optional columns with
#' `data[missing] <- NA_real_`, which `[<-.data.table` refuses, and
#' `ensure_columns()` requires a tibble by contract. Converting once, at each
#' exported boundary, is what CLAUDE.md asks for -- `data.table` stays an
#' internal detail of private helpers (whep#1136).
#'
#' `as.data.frame()` first, and the attribute dropped afterwards, because
#' `as_tibble()` on a `data.table` carries its `.internal.selfref` pointer out
#' as an attribute; a frame that still has one compares unequal to a plain
#' tibble and can trigger data.table's shallow-copy warning downstream.
#' @noRd
.as_livestock_tibble <- function(data) {
  if (tibble::is_tibble(data) && is.null(attr(data, ".internal.selfref"))) {
    return(data)
  }
  out <- tibble::as_tibble(as.data.frame(data))
  attr(out, ".internal.selfref") <- NULL
  out
}

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

# Herds are selected by `unit == "heads"`. A production table in another unit
# vocabulary matches no row, and every emission engine downstream then receives
# no animals: enteric and manure CH4 and manure N2O come back as an empty frame,
# which a GHG extension distributes to nothing while every conservation check
# passes (whep#1034). So the label is asserted before the filter.
#' @noRd
.check_head_unit <- function(data) {
  check_labels_supplied(
    data,
    "unit",
    "heads",
    details = c(
      i = "Livestock numbers are read from the {.val heads} rows of
           {.fn get_primary_production}."
    )
  )
}

#' @noRd
.excluded_livestock_codes <- function() {
  # Rabbits, Rodents, Animals live nes, Bees, Game
  c(1140L, 1150L, 1171L, 1181L, 1190L)
}

#' @noRd
.exclude_non_ipcc_species <- function(data, excluded, animals) {
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
  if (!rlang::has_name(data, "unit")) {
    return(NULL)
  }

  yield_rows <- data |>
    dplyr::filter(unit == "t_head")

  if (nrow(yield_rows) == 0) {
    return(NULL)
  }

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

  tagged <- .tag_yields_to_animal_product(yield_rows, product_map)
  .check_yields_tagged(tagged)

  if (is.null(tagged) || nrow(tagged) == 0) {
    return(NULL)
  }

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

  # Meat/egg yields: convert carcass weight per head to daily live-weight gain
  # for the energy model's ne_growth term via IPCC dressing fractions and
  # typical fattening periods (IPCC 2019 Vol 4, Ch 10).
  meat_yields_raw <- tagged |>
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
  meat_yields <- .meat_to_weight_gain(meat_yields_raw, animals)

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

  if (nrow(yields) == 0) {
    return(NULL)
  }
  yields
}

# Yield rows were supplied (the caller only gets here with `t_head` rows that
# carry a `live_anim_code`), so a tagging that matches none of them is a key
# that moved -- a product code in another vocabulary or type -- not an absent
# yield. Unguarded, `.extract_production_yields()` returns NULL, no milk yield
# or weight gain is joined, and `estimate_energy_demand()` silently gives every
# country its species' global `livestock_production_defaults` yield instead of
# the realised one (whep#1034). Measured on the 2010 primary production: 1,342
# of 4,584 `t_head` rows tag, the rest being non-designated co-products, so a
# partial match is the normal state and only a match of none is refused.
.check_yields_tagged <- function(tagged) {
  check_inputs_supplied(
    tibble::tibble(tagged_yield_rows = nrow(tagged)),
    c(designated_product_yield = "tagged_yield_rows"),
    details = c(
      i = "No {.val t_head} row matches an animal's designated product
           ({.field Item_Code_product} in {.code animals_codes}) on
           {.field live_anim_code} and {.field item_prod_code}."
    )
  )
}

# Convert `meat_yield_t_head` (FAOSTAT carcass weight per head, tonnes) to
# `weight_gain_kg_day` (average daily live-weight gain over the fattening period)
# using IPCC-standard dressing fractions and fattening period lengths (IPCC 2019
# Vol 4, Ch 10, Table 10.1 / GLEAM defaults). Applied at the species level —
# the same granularity as the existing `livestock_production_defaults` baseline
# it replaces — so country-specific realised carcass weights drive the energy
# model instead of a fixed global default.
.meat_to_weight_gain <- function(meat_yields_raw, animals) {
  fattening_params <- tibble::tribble(
    ~species_gen, ~dressing_frac, ~fattening_days, ~birth_weight_kg,
    "Cattle",     0.55,           547.5,           40,
    "Buffalo",    0.55,           547.5,           40,
    "Sheep",      0.45,           365.0,            4,
    "Goats",      0.45,           365.0,            3,
    "Swine",      0.75,           183.0,            1.5
  )
  species_key <- tibble::as_tibble(animals) |>
    dplyr::transmute(
      item_cbs_code = as.integer(item_cbs_code),
      species_gen = .get_general_species(item_cbs)
    ) |>
    dplyr::distinct(item_cbs_code, .keep_all = TRUE)
  meat_yields_raw |>
    dplyr::left_join(species_key, by = "item_cbs_code") |>
    dplyr::left_join(fattening_params, by = "species_gen") |>
    dplyr::mutate(
      live_weight_kg = meat_yield_t_head *
        1000 /
        dplyr::coalesce(dressing_frac, 0.55),
      weight_gain_kg_day = dplyr::if_else(
        !is.na(dressing_frac) & live_weight_kg > birth_weight_kg,
        (live_weight_kg - birth_weight_kg) /
          dplyr::coalesce(fattening_days, 547.5),
        NA_real_
      )
    ) |>
    dplyr::select(
      dplyr::any_of(c("year", "area_code")),
      item_cbs_code,
      weight_gain_kg_day
    )
}

# Tag each t_head yield row with the producing animal's product category,
# matching on the animal's DESIGNATED product (`Item_Code_product`), not just on
# `live_anim_code`.
.tag_yields_to_animal_product <- function(yield_rows, product_map) {
  # `live_anim_code` arrives as an integer from real production data but the
  # lookups key on it as character, so coerce before any join to keep the
  # key types compatible.
  yield_rows <- dplyr::mutate(
    yield_rows,
    live_anim_code = as.character(.data$live_anim_code)
  )
  if (!rlang::has_name(yield_rows, "item_prod_code")) {
    anim_lookup <- product_map |>
      dplyr::transmute(
        live_anim_code = as.character(item_cbs_code),
        Liv_prod_cat
      )
    return(dplyr::inner_join(yield_rows, anim_lookup, by = "live_anim_code"))
  }
  product_lookup <- product_map |>
    dplyr::transmute(
      live_anim_code = as.character(item_cbs_code),
      item_prod_code = as.character(Item_Code_product),
      Liv_prod_cat
    )
  yield_rows |>
    dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
    dplyr::inner_join(
      product_lookup,
      by = c("live_anim_code", "item_prod_code")
    )
}

#' @noRd
.yield_join_keys <- function(heads_data, yields) {
  intersect(
    c("year", "area_code", "item_cbs_code"),
    intersect(names(heads_data), names(yields))
  )
}
