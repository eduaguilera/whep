#' Manure engine options
#'
#' @description
#' Shared description of the `options` list the IPCC manure engine takes,
#' documented once and inherited by the functions that accept it.
#'
#' @param options A named list of manure-engine options. Every default
#'   reproduces the behaviour in force before whep#949, so passing none leaves
#'   published values unchanged.
#'
#'   `mms_region` selects how the manure-management split in
#'   [regional_mms_distribution] is keyed:
#'   * `"as_available"` (default): a row uses its own region when the frame
#'     already carries a `region` column, and the `region == "Global"` split
#'     otherwise. Tier 1 resolves a region for the (sourced) per-head
#'     N-excretion table and so takes the region-specific split; Tier 2 carries
#'     no region and so takes the Global one.
#'   * `"resolve"`: the IPCC region is resolved from `iso3`, `area_code` or
#'     `polity_area_code` where it is missing, which makes the table's four
#'     region-specific `(region, species)` pairs live on the Tier 2 path too.
#'     Those four pairs are an unsourced placeholder (whep#921), which is why
#'     this is opt-in rather than the default.
#'   * `"global"`: every row takes the `region == "Global"` split, whatever
#'     region column it carries.
#'
#'   `climate_source` selects where the climate zone the methane conversion
#'   factors in [climate_mcf] are read at comes from. A `climate_zone` a row
#'   already carries is always used and stamped `climate_from_data`; the option
#'   governs only the rows left without one, whether that is a hole in a
#'   supplied column or a wholly absent column.
#'   * `"assumed"` (default): fill with `assumed_climate_zone`.
#'   * `"from_data"`: abort instead of assuming.
#'
#'   `assumed_climate_zone` is the zone `"assumed"` fills in: `"Cool"`,
#'   `"Temperate"` (default) or `"Warm"`. It is an assumption, not a
#'   measurement; `method_manure_ch4` records per row which of the sources
#'   applied, and this argument exists so the sensitivity to the assumption can
#'   be measured (whep#949).
#'
#' @name manure_engine_options
#' @keywords internal
NULL

#' IPCC 2019 Tier 1 manure CH4.
#' @noRd
.calc_manure_ch4_tier1 <- function(data) {
  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species),
      method_manure_ch4 = "IPCC_2019_Tier1"
    )

  data <- .join_manure_ch4_ef_tier1(data)

  n_animals <- .animal_count(data)
  data |>
    dplyr::mutate(
      manure_ch4_tier1 = n_animals * manure_ef_kgch4
    )
}

#' IPCC 2019 Tier 1 manure N2O (direct + indirect).
#'
#' Uses default per-head nitrogen excretion rates (`ipcc_2019_n_excretion`)
#' instead of the energy-balance N intake, then reuses the Tier 2 direct and
#' indirect N2O helpers (which need only `n_excretion` and `heads`).
#' @noRd
.calc_manure_n2o_tier1 <- function(data, options = list()) {
  if (!rlang::has_name(data, "species_gen")) {
    data <- dplyr::mutate(data, species_gen = .get_general_species(species))
  }
  data <- data |>
    dplyr::mutate(method_manure_n2o = "IPCC_2019_Tier1") |>
    .join_n_excretion_tier1() |>
    .calc_direct_n2o(options) |>
    .calc_indirect_n2o()
  data |>
    dplyr::mutate(
      manure_n2o_total = manure_n2o_direct + manure_n2o_indirect
    )
}

#' Join default per-head N excretion (kg N/head/yr) for Tier 1.
#' @noRd
.join_n_excretion_tier1 <- function(data) {
  data <- data |>
    dplyr::mutate(
      manure_category = dplyr::case_when(
        .is_dairy(species) & species_gen == "Cattle" ~ "Dairy Cattle",
        species_gen == "Cattle" ~ "Other Cattle",
        TRUE ~ species_gen
      )
    )
  if (!rlang::has_name(data, "region") && .has_gleam_region_key(data)) {
    data <- .add_ipcc_region(data)
  }

  # Augment the table with base-species rows (mean over subcategories, e.g.
  # "Swine - Market"/"Swine - Breeding" -> "Swine") so a species_gen key with no
  # exact subcategory still matches.
  nex_base <- ipcc_2019_n_excretion |>
    dplyr::mutate(
      cat_base = stringr::str_trim(stringr::str_extract(category, "^[^-]+"))
    ) |>
    dplyr::summarise(
      nex_kg_n_head_yr = mean(nex_kg_n_head_yr, na.rm = TRUE),
      .by = c(region, cat_base)
    ) |>
    dplyr::rename(category = cat_base)
  nex_all <- dplyr::bind_rows(ipcc_2019_n_excretion, nex_base) |>
    dplyr::distinct(region, category, .keep_all = TRUE)
  global_nex <- nex_all |>
    dplyr::filter(region == "Global") |>
    dplyr::select(category, nex_global = nex_kg_n_head_yr)

  if (rlang::has_name(data, "region")) {
    data |>
      dplyr::left_join(
        dplyr::filter(nex_all, region != "Global"),
        by = c("region", "manure_category" = "category")
      ) |>
      dplyr::left_join(global_nex, by = c("manure_category" = "category")) |>
      dplyr::mutate(
        n_excretion = dplyr::coalesce(nex_kg_n_head_yr, nex_global)
      ) |>
      dplyr::select(-dplyr::any_of(c("nex_kg_n_head_yr", "nex_global")))
  } else {
    data |>
      dplyr::left_join(global_nex, by = c("manure_category" = "category")) |>
      dplyr::mutate(n_excretion = nex_global) |>
      dplyr::select(-dplyr::any_of("nex_global"))
  }
}

#' IPCC 2019 Tier 2 manure CH4.
#' @noRd
.calc_manure_ch4_tier2 <- function(data, options = list()) {
  if (!rlang::has_name(data, "gross_energy")) {
    cli::cli_abort(
      "{.fun .calc_manure_ch4_tier2} requires {.var gross_energy}. \\
       Run {.fun estimate_energy_demand} first."
    )
  }
  opt <- .manure_options(options)

  data <- data |>
    dplyr::mutate(
      species_gen = dplyr::coalesce(
        species_gen,
        .get_general_species(species)
      ),
      subcategory = dplyr::coalesce(
        subcategory,
        .get_subcategory(species)
      ),
      method_manure_ch4 = "IPCC_2019_Tier2"
    ) |>
    .resolve_manure_region(opt$mms_region)

  data <- .calc_volatile_solids(data)
  data <- .join_bo(data)
  data <- .calc_weighted_mcf(data, options)

  ch4_density <- 0.67 # kg/m3 CH4 at STP

  n_animals <- .animal_count(data)
  data |>
    dplyr::mutate(
      manure_ch4_per_head = volatile_solids *
        365 *
        methane_potential *
        ch4_density *
        weighted_mcf,
      manure_ch4_tier2 = n_animals * manure_ch4_per_head
    )
}

#' IPCC 2019 manure N2O (direct + indirect).
#' @noRd
.calc_manure_n2o <- function(data, options = list()) {
  if (!rlang::has_name(data, "gross_energy")) {
    cli::cli_abort(
      "{.fun .calc_manure_n2o} requires {.var gross_energy}. \\
       Run {.fun estimate_energy_demand} first."
    )
  }

  data <- data |>
    dplyr::mutate(
      species_gen = dplyr::coalesce(
        species_gen,
        .get_general_species(species)
      ),
      subcategory = dplyr::coalesce(
        subcategory,
        .get_subcategory(species)
      ),
      method_manure_n2o = "IPCC_2019_Tier2"
    )

  data <- .calc_n_excretion(data)
  data <- .calc_direct_n2o(data, options)
  data <- .calc_indirect_n2o(data)

  data |>
    dplyr::mutate(
      manure_n2o_total = manure_n2o_direct +
        manure_n2o_indirect
    )
}

# Private helpers ----

#' Join Tier 1 manure CH4 emission factors.
#' @noRd
.join_manure_ch4_ef_tier1 <- function(data) {
  all_categories <- c(
    # nolint: object_usage_linter.
    ipcc_2019_manure_ch4_ef_cattle$category,
    ipcc_2019_manure_ch4_ef_other$category
  ) |>
    unique()

  # Map species to EF table categories, preserving
  # exact subcategory names when they exist in the table.
  # Buffalo uses Table 10.14b (other), not cattle table.
  data <- data |>
    dplyr::mutate(
      manure_category = dplyr::case_when(
        .is_dairy(species) & species_gen == "Cattle" ~ "Dairy Cattle",
        species_gen == "Cattle" ~ "Other Cattle",
        species %in% all_categories ~ species,
        TRUE ~ species_gen
      )
    )

  region_added <- !rlang::has_name(data, "region") &&
    .has_gleam_region_key(data)
  if (region_added) {
    data <- .add_ipcc_region(data)
  }

  # Try regional match first
  if (rlang::has_name(data, "region")) {
    # Tier 1 carries no climate input, so average the cattle EFs over the
    # climate zones IPCC reports for each region/category.
    cattle_ef <- ipcc_2019_manure_ch4_ef_cattle |>
      dplyr::summarise(
        ef_kg_head_yr = mean(ef_kg_head_yr, na.rm = TRUE),
        .by = c(region, category)
      )
    global_cattle <- cattle_ef |>
      dplyr::filter(region == "Global") |>
      dplyr::select(category, ef_global = ef_kg_head_yr)
    other_ef <- ipcc_2019_manure_ch4_ef_other |>
      dplyr::select(category, ef_kg_head_yr)

    # Buffalo uses other table, not cattle table
    is_cattle <- data$species_gen == "Cattle"
    cattle_rows <- data |>
      dplyr::filter(is_cattle) |>
      dplyr::left_join(
        cattle_ef,
        by = c(
          "region",
          "manure_category" = "category"
        )
      ) |>
      dplyr::left_join(
        global_cattle,
        by = c("manure_category" = "category")
      ) |>
      dplyr::mutate(
        ef_kg_head_yr = dplyr::coalesce(ef_kg_head_yr, ef_global)
      ) |>
      dplyr::select(-ef_global)

    other_rows <- data |>
      dplyr::filter(!is_cattle) |>
      .join_ef_with_subcategories(
        other_ef,
        "manure_category",
        "ef_kg_head_yr"
      )

    data <- dplyr::bind_rows(cattle_rows, other_rows) |>
      dplyr::rename(manure_ef_kgch4 = ef_kg_head_yr)
  } else {
    # Global fallback
    all_ef <- dplyr::bind_rows(
      ipcc_2019_manure_ch4_ef_cattle |>
        dplyr::filter(region == "Global"),
      ipcc_2019_manure_ch4_ef_other
    ) |>
      dplyr::select(category, ef_kg_head_yr) |>
      dplyr::distinct(category, .keep_all = TRUE)

    data <- data |>
      .join_ef_with_subcategories(
        all_ef,
        "manure_category",
        "ef_kg_head_yr"
      ) |>
      dplyr::rename(manure_ef_kgch4 = ef_kg_head_yr)
  }

  if (region_added) {
    data <- data |> dplyr::select(-dplyr::any_of("region"))
  }
  data |>
    dplyr::select(-manure_category)
}

#' Calculate Volatile Solids (VS) - IPCC Eq 10.24.
#' @noRd
.calc_volatile_solids <- function(data) {
  ue_factor <- livestock_constants$default_ue_fraction
  ge_content <- livestock_constants$vs_energy_content_mj_kg

  # Get ash content by species
  ash_tbl <- ipcc_tier2_manure_ash |>
    dplyr::select(category, ash_percent)

  data |>
    dplyr::left_join(
      ash_tbl,
      by = c("species_gen" = "category")
    ) |>
    .assume_missing_ash() |>
    dplyr::mutate(
      # IPCC 2019 Eq 10.24:
      #   VS = GE * [(1 - DE/100) + UE] * (1 - ASH/100) / 18.45
      # UE is the urinary energy fraction of GE (default 0.04); it enters as an
      # additive term, not scaled again by DE.
      volatile_solids = gross_energy *
        (1 - de_percent / 100 + ue_factor) *
        (1 - ash_percent / 100) /
        ge_content
    )
}

#' Declare the manure ash content of a species `ipcc_tier2_manure_ash` omits.
#'
#' The bare `coalesce(ash_percent, 8.0)` this replaces handed every such
#' species the ruminant ash content as if it had been looked up.
#' @noRd
.assume_missing_ash <- function(data) {
  .fill_assumed_param(
    data,
    col = "ash_percent",
    kind = "digestion_like",
    values = .named_values(ipcc_tier2_manure_ash, "category", "ash_percent"),
    quantity = "manure ash content",
    tag = "ash",
    method_col = "method_manure_ch4"
  )
}

#' Join Bo values differentiated by dairy/other.
#' @noRd
.join_bo <- function(data) {
  bo_tbl <- ipcc_tier2_bo_values

  # Map to Bo category
  data <- data |>
    dplyr::mutate(
      bo_category = .get_bo_category(species, species_gen)
    )

  data |>
    dplyr::left_join(
      bo_tbl,
      by = c("bo_category" = "category")
    ) |>
    dplyr::rename(methane_potential = bo_m3_kg_vs) |>
    .assume_missing_bo() |>
    dplyr::select(-bo_category)
}

#' Declare the Bo of a species `ipcc_tier2_bo_values` omits.
#'
#' The bare `coalesce(methane_potential, 0.18)` this replaces handed every such
#' species Other Cattle's methane potential with nothing recording that it had
#' not been looked up. `uncertainty_ranges` puts Bo at 0.80-1.20 of its central
#' value; an assumed Bo is at least that uncertain, and the shipped values span
#' 0.10 (buffalo) to 0.45 (market swine).
#' @noRd
.assume_missing_bo <- function(data) {
  .fill_assumed_param(
    data,
    col = "methane_potential",
    kind = "digestion_like",
    values = .named_values(ipcc_tier2_bo_values, "category", "bo_m3_kg_vs"),
    quantity = "methane potential (Bo)",
    tag = "bo",
    method_col = "method_manure_ch4"
  )
}

#' Map species to Bo category.
#' @noRd
.get_bo_category <- function(species, species_gen) {
  dplyr::case_when(
    .is_dairy(species) & species_gen == "Cattle" ~ "Dairy Cattle",
    species_gen == "Cattle" ~ "Other Cattle",
    species_gen == "Swine" &
      stringr::str_detect(species, "(?i)Breed") ~
      "Swine - Breeding",
    species_gen == "Swine" ~ "Swine - Market",
    species_gen == "Poultry" &
      stringr::str_detect(species, "(?i)Layer|Hen") ~
      "Poultry - Layers",
    species_gen == "Poultry" ~ "Poultry - Broilers",
    TRUE ~ species_gen
  )
}

# Declared assumptions for species no IPCC table covers ------------------------

#' The nearest covered species for one the IPCC tables give no parameters for.
#'
#' `livestock_mapping.csv` carries three species whose `species_group` is
#' `"other"` -- Rabbits and hares, Rodents other and Animals live nes -- and
#' none of them is keyed in `ipcc_tier2_bo_values`, `ipcc_tier2_manure_ash`,
#' `ipcc_tier2_n_retention` or `regional_mms_distribution`. Their manure exists,
#' so the row is not refused and is not given a bare number either: it takes the
#' parameters of the nearest covered species and records that it did.
#'
#' The neighbour differs by what the parameter depends on, which is why there
#' are two columns:
#' - `digestion_like` sets the parameters the gut decides (Bo, manure ash).
#'   Rabbits and cavies are hindgut-fermenting herbivores on high-fibre diets,
#'   as horses and asses are, so the horse values are the nearest shipped ones.
#' - `husbandry_like` sets the parameters the housing decides (the
#'   manure-management split, and the share of nitrogen retained in product).
#'   Both are caged small stock kept over dry litter and slaughtered young,
#'   which is the poultry pattern rather than the horse one.
#'
#' ASSUMED, UNVERIFIED: neither the IPCC 2006 Guidelines nor the 2019 Refinement
#' publishes Tier 2 manure parameters for rabbits or rodents, so these are
#' arguments from the nearest covered species, not values read from a table.
#'
#' The table is also the closed list of species that may be filled at all. A
#' species that is not on it resolves no manure-management split and
#' `.check_mms_matched()` aborts on it (whep#950), because a split invented for
#' an animal nobody has argued a husbandry for is a guess, not an estimate.
#' `"Animals live nes"` is on the list with no neighbour in either column: it is
#' a residual FAOSTAT category with no single husbandry, so it takes the
#' unlisted-species fallbacks (`.fill_assumed_param()` for the gut parameters
#' and `.assumed_mms_fallback()` for the split) rather than a neighbour it
#' cannot be said to resemble. `animals_codes` and `livestock_mapping.csv`
#' spell the rodent category differently, so both spellings are listed.
#' @noRd
.assumed_species_neighbours <- function() {
  tibble::tribble(
    ~species_gen, ~digestion_like, ~husbandry_like,
    "Rabbits and hares", "Horses", "Poultry",
    "Rodents other", "Horses", "Poultry",
    "Rodents, other", "Horses", "Poultry",
    "Animals live nes", NA_character_, NA_character_
  )
}

#' The neighbour of each row's species for one kind of parameter, `NA` when the
#' species is not one the table above argues a neighbour for.
#' @noRd
.assumed_neighbour_of <- function(species_gen, kind) {
  neighbours <- .assumed_species_neighbours()
  neighbours[[kind]][match(species_gen, neighbours$species_gen)]
}

#' A shipped coefficient table as a named vector, keyed by its category.
#' @noRd
.named_values <- function(tbl, key_col, value_col) {
  stats::setNames(tbl[[value_col]], tbl[[key_col]])
}

#' Fill one per-species manure parameter that resolved no value, and say so.
#'
#' Three states, and only the third is acceptable here: a bare default is
#' indistinguishable from a measurement once downstream, and an abort excludes
#' manure that exists. So the row keeps a value, takes it from the nearest
#' covered species where one is argued (`.assumed_species_neighbours()`) and
#' otherwise from the unweighted mean over the categories the shipped table does
#' cover, and carries the basis in its `method_*` column so a consumer can
#' filter assumed rows from measured ones.
#'
#' ASSUMED, UNVERIFIED: the mean of a coefficient table is a central estimate
#' for an animal the IPCC does not parameterise, not an IPCC value. Its
#' uncertainty is at least the spread of the table it is drawn from.
#' @noRd
.fill_assumed_param <- function(
  data,
  col,
  kind,
  values,
  quantity,
  tag,
  method_col
) {
  gap <- is.na(data[[col]])
  if (!any(gap)) {
    return(data)
  }
  neighbour <- .assumed_neighbour_of(data$species_gen, kind)
  from_neighbour <- unname(values[match(neighbour, names(values))])
  basis <- dplyr::if_else(
    is.na(from_neighbour),
    "table_mean",
    stringr::str_replace_all(tolower(neighbour), " ", "_")
  )
  filled <- dplyr::coalesce(from_neighbour, mean(values, na.rm = TRUE))
  data[[col]][gap] <- filled[gap]
  .warn_assumed(data$species_gen[gap], quantity, basis[gap])
  .stamp_assumption(data, method_col, paste0(tag, "_assumed_", basis), gap)
}

#' Say out loud which species took a declared assumption, and from what.
#' @noRd
.warn_assumed <- function(species, quantity, basis) {
  species <- sort(unique(species[!is.na(species)]))
  cli::cli_warn(c(
    "!" = "No {quantity} in the IPCC tables for {.val {species}}.",
    i = "Assumed from {.val {sort(unique(basis))}} and stamped in the method
         column: a declared assumption, not a measured value."
  ))
}

#' Append an assumption tag to a method column, on the assumed rows only.
#'
#' The column is created when a caller did not supply one, so a helper called
#' on its own still returns its assumptions rather than dropping them. `tag` may
#' be one label or one per row; `where` selects the rows that actually took the
#' assumption, so a measured row is never stamped.
#' @noRd
.stamp_assumption <- function(data, col, tag, where) {
  # `where` may be one value for the whole frame; `if_else()` sizes its output
  # from the condition, so recycle it before the branches are built.
  where <- rep_len(!is.na(where) & where, nrow(data))
  if (!any(where)) {
    return(data)
  }
  if (!rlang::has_name(data, col)) {
    data[[col]] <- rep(NA_character_, nrow(data))
  }
  current <- data[[col]]
  stamped <- dplyr::if_else(is.na(current), tag, paste0(current, "; ", tag))
  data[[col]] <- dplyr::if_else(where, stamped, current)
  data
}

#' The manure-management split assumed for a species with no shipped one.
#'
#' Every fraction still comes from `regional_mms_distribution`: what is assumed
#' is only which covered species the row is managed like
#' (`.assumed_species_neighbours()`), so the split cannot drift from the shipped
#' table. A species that argues no neighbour takes `.assumed_mms_fallback()`.
#' @noRd
.assumed_mms_shares <- function(species_gen) {
  wanted <- tibble::tibble(species_gen = unique(species_gen))
  mapped <- wanted |>
    dplyr::inner_join(.assumed_species_neighbours(), by = "species_gen") |>
    dplyr::inner_join(
      dplyr::filter(regional_mms_distribution, region == "Global"),
      by = c("husbandry_like" = "species"),
      relationship = "many-to-many"
    ) |>
    dplyr::transmute(
      species_gen,
      mms_type,
      fraction,
      mms_basis = tolower(husbandry_like)
    )
  dplyr::bind_rows(
    mapped,
    dplyr::cross_join(
      dplyr::anti_join(wanted, mapped, by = "species_gen"),
      .assumed_mms_fallback()
    )
  )
}

#' Where the manure of an unlisted species is assumed to go.
#'
#' Every minor species `regional_mms_distribution` does cover is predominantly
#' pasture/range/paddock -- Sheep, Goats, Camels and Mules and Asses at 100 %,
#' Horses at 80 % -- so deposited-where-it-falls is the shipped table's own
#' treatment of minor livestock, and the nearest defensible basis for a minor
#' species it omits. ASSUMED, UNVERIFIED: no IPCC table assigns a management
#' split to these species.
#' @noRd
.assumed_mms_fallback <- function() {
  tibble::tibble(
    mms_type = "Pasture/Range/Paddock",
    fraction = 1,
    mms_basis = "pasture"
  )
}

#' Replace unresolved manure-management shares with the declared assumption.
#'
#' `regional_mms_distribution` covers nine species. WHEP's livestock vocabulary
#' carries four more with manure -- rabbits, rodents (two spellings) and the
#' residual `"Animals live nes"` -- and the IPCC publishes no split for any of
#' them. That is an absent quantity, not an absent contract: the manure exists,
#' no maintainer can fix it by editing a table, and refusing the row would drop
#' it out of the balance. So those species, and only those species, take the
#' argued split of `.assumed_species_neighbours()`; `mms_basis` names it and is
#' `NA` on every measured row.
#'
#' Anything else keeps its unresolved rows and reaches `.check_mms_matched()`,
#' which aborts (whep#950). That is deliberate: an unknown species arriving here
#' is a vocabulary defect the maintainer can and should fix, and inventing a
#' split for it would hide exactly the failure this fill exists to make visible.
#' @noRd
.fill_assumed_mms_shares <- function(shares) {
  shares <- dplyr::mutate(shares, mms_basis = NA_character_)
  gap <- (is.na(shares$mms_type) | is.na(shares$fraction)) &
    shares$species_gen %in% .assumed_species_neighbours()$species_gen
  if (!any(gap)) {
    return(shares)
  }
  assumed <- shares[gap, ] |>
    dplyr::select(-dplyr::any_of(c("mms_type", "fraction", "mms_basis"))) |>
    dplyr::left_join(
      .assumed_mms_shares(shares$species_gen[gap]),
      by = "species_gen",
      relationship = "many-to-many"
    )
  .warn_assumed(
    assumed$species_gen,
    "manure management distribution",
    assumed$mms_basis
  )
  dplyr::bind_rows(shares[!gap, ], assumed)
}

#' Calculate weighted MCF across MMS types.
#'
#' Every row's MCF is the mean over its own MMS distribution, so an MMS type
#' with no `climate_mcf` row for the row's climate zone aborts rather than
#' taking a flat default: it means the MMS vocabulary and the MCF table have
#' drifted apart, which is a defect in the tables, not a modelling choice
#' (whep#950).
#'
#' The four species `regional_mms_distribution` omits but WHEP's livestock
#' vocabulary carries are the one exception, and they are handled before that
#' abort by `.fill_assumed_mms_shares()`: their manure exists and no table
#' publishes its split, so the row takes an argued one and says so in
#' `method_manure_ch4` rather than being refused.
#' @noRd
.calc_weighted_mcf <- function(data, options = list()) {
  opt <- .manure_options(options)
  data <- .apply_climate_zone(data, opt)

  # Get MCF by MMS and climate zone
  mcf_tbl <- climate_mcf |>
    dplyr::select(mms_type, climate_zone, mcf_percent)

  # For each row, compute weighted MCF over its MMS distribution.
  data <- data |>
    dplyr::mutate(row_id = dplyr::row_number())

  mcf_rows <- data |>
    dplyr::select(
      row_id,
      species_gen,
      climate_zone,
      dplyr::any_of("region")
    ) |>
    .resolve_mms_shares(.mms_region_col(opt$mms_region)) |>
    .fill_assumed_mms_shares() |>
    dplyr::left_join(
      mcf_tbl,
      by = c("mms_type", "climate_zone")
    ) |>
    .check_mms_matched("mcf_percent")

  mms_joined <- mcf_rows |>
    dplyr::summarise(
      weighted_mcf = sum(fraction * mcf_percent / 100),
      mms_basis = dplyr::first(mms_basis),
      .by = row_id
    )

  out <- data |>
    dplyr::left_join(mms_joined, by = "row_id") |>
    dplyr::select(-row_id)
  .check_weighted_mcf(out)
  out |>
    .stamp_assumption(
      "method_manure_ch4",
      paste0("mms_assumed_", out$mms_basis),
      !is.na(out$mms_basis)
    ) |>
    dplyr::select(-mms_basis)
}

#' The climate zones `climate_mcf` actually keys.
#'
#' `"All"` is the wildcard row for systems whose MCF does not vary with climate
#' (anaerobic digester, burned for fuel, composting), not a label a row can
#' carry, so it is not a zone a caller may supply. Derived from the shipped
#' table rather than written down, so the two cannot drift apart.
#' @noRd
.climate_mcf_zones <- function() {
  setdiff(sort(unique(climate_mcf$climate_zone)), "All")
}

#' Fail closed on a climate-zone label `climate_mcf` does not key.
#'
#' A label like `"Boreal"` finds no MCF, and the `coalesce()` that used to sit
#' below would hand it the 2% default of a liquid/slurry system regardless of
#' what the animal's manure actually goes to. This is the one case that is not
#' an absence: the caller supplied a zone, from a vocabulary that is not the
#' one `climate_mcf` keys, and mapping it onto Cool/Temperate/Warm would be
#' guessing at a measurement rather than estimating an absent one. A `NA` zone
#' is absence and takes the declared assumption above instead.
#' @noRd
.check_climate_zone <- function(data) {
  known <- .climate_mcf_zones()
  unknown <- setdiff(unique(data$climate_zone), known)
  if (length(unknown) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{length(unknown)} {.field climate_zone} label{?s} not keyed in
     {.var climate_mcf}: {.val {unknown}}.",
    x = "Such a label matches no methane conversion factor, and used to take
         the 2% liquid/slurry default with no warning and no method stamp.",
    i = "Keyed zones: {.val {known}}.",
    i = "{.fun build_cell_climate_zone} emits exactly those; a
         caller-supplied {.arg cell_climate} must use the same vocabulary."
  ))
}

#' Fail closed on a row that resolved no weighted MCF.
#'
#' Every species now resolves a distribution and every distribution an MCF, so
#' this is the assertion that they do: it turns a future gap into a named abort
#' instead of an `NA` that a downstream `na.rm` sum would read as zero.
#' @noRd
.check_weighted_mcf <- function(data) {
  n_na <- sum(is.na(data$weighted_mcf))
  if (n_na == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{n_na} row{?s} resolved no {.field weighted_mcf}.",
    i = "Every row should have been caught by the climate-zone, MMS-share or
         MCF check above, so this is a gap none of them covers."
  ))
}

#' Calculate nitrogen excretion (n_excretion).
#' @noRd
.calc_n_excretion <- function(data) {
  ge_content <- livestock_constants$vs_energy_content_mj_kg

  # Get CP% from feed_characteristics (not hardcoded)
  if (!rlang::has_name(data, "cp_percent")) {
    data <- .join_diet_cp(data)
  }

  data |>
    .assume_missing_cp() |>
    .join_n_retention() |>
    .assume_missing_n_retention() |>
    dplyr::mutate(
      n_intake = (gross_energy / ge_content) *
        (cp_percent / 100) /
        6.25,
      n_excretion = n_intake *
        (1 - n_retention_frac) *
        livestock_constants$days_in_year
    ) |>
    dplyr::select(-dplyr::any_of("n_ret_category"))
}

#' Attach the crude protein of each row's own diet, where it has one.
#' @noRd
.join_diet_cp <- function(data) {
  if (!rlang::has_name(data, "diet_quality")) {
    return(dplyr::mutate(data, cp_percent = NA_real_))
  }
  data |>
    dplyr::left_join(
      feed_characteristics |>
        dplyr::select(diet_quality, cp_percent),
      by = "diet_quality"
    )
}

#' Declare the crude protein assumed for a row whose diet resolved none.
#'
#' The bare 12 this replaces is not a free-standing number: it is the Medium
#' diet's `cp_percent` in `feed_characteristics`. Reading it from there says
#' which diet the row was assumed to eat, and the stamp says that it was
#' assumed rather than resolved -- nitrogen intake, and so Tier 2 manure N2O,
#' scales linearly with it.
#' @noRd
.assume_missing_cp <- function(data) {
  gap <- is.na(data$cp_percent)
  if (!any(gap)) {
    return(data)
  }
  assumed <- .assumed_diet_cp()
  data$cp_percent[gap] <- assumed
  cli::cli_warn(c(
    "!" = "{sum(gap)} row{?s} {?has/have} no diet to take a crude protein
       content from.",
    i = "Assumed the {.val Medium} diet's {assumed}% and stamped it in
         {.field method_manure_n2o}."
  ))
  .stamp_assumption(data, "method_manure_n2o", "cp_assumed_medium_diet", gap)
}

#' The crude protein assumed when a row carries no diet.
#' @noRd
.assumed_diet_cp <- function() {
  feed_characteristics$cp_percent[
    feed_characteristics$diet_quality == "Medium"
  ]
}

#' Join the retained-nitrogen fraction, by subcategory then by species.
#'
#' `.get_bo_category()` keys swine and poultry by subcategory
#' (`"Swine - Market"`, `"Poultry - Layers"`) while `ipcc_tier2_n_retention`
#' keys them by species, so the exact join never matched for either and both
#' silently took the bare 0.07 that used to sit below -- Other Cattle's
#' retention, which inflated their nitrogen excretion by
#' `(1 - 0.07) / (1 - 0.30)` = 33%. The base-category leg below is what makes
#' the shipped 0.30 reachable.
#' @noRd
.join_n_retention <- function(data) {
  n_ret_tbl <- ipcc_tier2_n_retention |>
    dplyr::select(category, n_retention_frac)
  data |>
    dplyr::mutate(
      n_ret_category = .get_bo_category(species, species_gen),
      n_ret_base = .base_category(n_ret_category)
    ) |>
    dplyr::left_join(n_ret_tbl, by = c("n_ret_category" = "category")) |>
    dplyr::left_join(
      dplyr::rename(n_ret_tbl, n_retention_base = n_retention_frac),
      by = c("n_ret_base" = "category")
    ) |>
    dplyr::mutate(
      n_retention_frac = dplyr::coalesce(
        n_retention_frac,
        n_retention_base
      )
    ) |>
    dplyr::select(-n_ret_base, -n_retention_base)
}

#' The species part of a subcategory label ("Swine - Market" -> "Swine").
#' @noRd
.base_category <- function(category) {
  stringr::str_trim(stringr::str_extract(category, "^[^-]+"))
}

#' Declare the retained-nitrogen fraction of a species IPCC omits.
#' @noRd
.assume_missing_n_retention <- function(data) {
  .fill_assumed_param(
    data,
    col = "n_retention_frac",
    kind = "husbandry_like",
    values = .named_values(
      ipcc_tier2_n_retention,
      "category",
      "n_retention_frac"
    ),
    quantity = "nitrogen retention fraction",
    tag = "n_retention",
    method_col = "method_manure_n2o"
  )
}

#' Calculate direct N2O from manure management.
#'
#' EF3 is always the mean over the row's own MMS distribution. Before whep#949
#' this branched on a `region` column being present, and a frame without one
#' (every Tier 2 frame) took the pasture EF3 for its whole manure stream,
#' liquid slurry and lagoons included. The MMS resolver needs no region: with
#' none it returns the `region == "Global"` split, which is the right
#' distribution to weight over, not a reason to abandon the weighting.
#' @noRd
.calc_direct_n2o <- function(data, options = list()) {
  opt <- .manure_options(options)
  data <- .resolve_manure_region(data, opt$mms_region)
  .calc_weighted_direct_n2o(
    data,
    .manure_ef3(),
    livestock_constants$n_to_n2o,
    opt$mms_region
  )
}

#' Direct N2O weighted over the row's manure-management distribution.
#'
#' `ef3_tbl` is [.manure_ef3()], the crosswalk that resolves each of the six
#' MMS labels the engine carries onto its `ipcc_2019_n2o_ef_direct` row. An
#' unresolved label aborts: it used to silently take the table's 0.005 `Other`
#' value, which is what put 80% of poultry manure on 0.005 where the litter
#' rows give 0.001 (whep#950).
#'
#' A species with no shipped split is the separate case, and the separate rule:
#' `.fill_assumed_mms_shares()` gives the four WHEP carries and the IPCC omits
#' an argued split first, and everything else still reaches the abort.
#' @noRd
.calc_weighted_direct_n2o <- function(
  data,
  ef3_tbl,
  n2o_to_n,
  mms_region = "as_available"
) {
  data <- data |>
    dplyr::mutate(row_id_n2o = dplyr::row_number())

  n2o_weighted <- data |>
    dplyr::select(
      row_id_n2o,
      species_gen,
      n_excretion,
      heads,
      dplyr::any_of("region")
    ) |>
    .resolve_mms_shares(.mms_region_col(mms_region)) |>
    .fill_assumed_mms_shares() |>
    dplyr::left_join(ef3_tbl, by = "mms_type") |>
    .check_mms_matched("ef3") |>
    dplyr::summarise(
      weighted_ef3 = sum(fraction * ef3),
      mms_basis = dplyr::first(mms_basis),
      .by = row_id_n2o
    )

  out <- data |>
    dplyr::left_join(n2o_weighted, by = "row_id_n2o")
  .check_weighted_ef3(out)

  n_animals <- .animal_count(out)
  out |>
    dplyr::mutate(
      manure_n2o_direct = n_animals *
        n_excretion *
        weighted_ef3 *
        n2o_to_n
    ) |>
    .stamp_assumption(
      "method_manure_n2o",
      paste0("mms_assumed_", out$mms_basis),
      !is.na(out$mms_basis)
    ) |>
    dplyr::select(-row_id_n2o, -weighted_ef3, -mms_basis)
}

#' Fail closed on a row that resolved no weighted EF3.
#'
#' Every species now resolves a distribution and every system an EF3, so this
#' is the assertion that they do: it turns a future gap into a named abort
#' rather than the bare 0.005 that used to stand in for one.
#' @noRd
.check_weighted_ef3 <- function(data) {
  n_na <- sum(is.na(data$weighted_ef3))
  if (n_na == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "{n_na} row{?s} resolved no {.field weighted_ef3}.",
    i = "Every row should have taken either a shipped or a declared manure
         management distribution, so this is a gap neither covers."
  ))
}

#' Calculate indirect N2O (volatilization + leaching).
#' Uses `indirect_n2o_ef` table - no hardcoded values.
#' @noRd
.calc_indirect_n2o <- function(data) {
  n2o_to_n <- 44 / 28

  # Read all parameters from the table
  ef4 <- .get_indirect_param("ef4_volatilization")
  ef5 <- .get_indirect_param("ef5_leaching")
  frac_gas <- .get_indirect_param("frac_gasms")
  frac_leach <- .get_indirect_param("frac_leach")

  n_animals <- .animal_count(data)
  data |>
    dplyr::mutate(
      n2o_volatilization = n_animals * n_excretion * frac_gas * ef4 * n2o_to_n,
      n2o_leaching = n_animals * n_excretion * frac_leach * ef5 * n2o_to_n,
      manure_n2o_indirect = n2o_volatilization +
        n2o_leaching
    ) |>
    dplyr::select(-n2o_volatilization, -n2o_leaching)
}

#' Get a parameter value from the indirect_n2o_ef table.
#' @noRd
.get_indirect_param <- function(param_name) {
  indirect_n2o_ef$value[
    indirect_n2o_ef$parameter == param_name
  ]
}

#' Join EF table handling subcategories by aggregation.
#'
#' Falls back to mean EF across subcategories (e.g.
#' "Swine - Market" / "Swine - Breeding") when an exact
#' match on the join column is not found.
#' @noRd
.join_ef_with_subcategories <- function(
  data,
  ef_tbl,
  join_col,
  ef_col
) {
  ef_agg <- ef_tbl |>
    dplyr::mutate(
      species_base = stringr::str_extract(
        category,
        "^[^-]+"
      ) |>
        stringr::str_trim()
    ) |>
    dplyr::summarise(
      ef_agg = mean(.data[[ef_col]], na.rm = TRUE),
      .by = species_base
    )

  data |>
    dplyr::left_join(
      ef_tbl,
      by = stats::setNames("category", join_col)
    ) |>
    dplyr::left_join(
      ef_agg,
      by = stats::setNames("species_base", join_col)
    ) |>
    dplyr::mutate(
      !!rlang::sym(ef_col) := dplyr::coalesce(
        .data[[ef_col]],
        ef_agg
      )
    ) |>
    dplyr::select(-ef_agg)
}

# Manure engine options ----

#' Validate and default the manure engine's options.
#'
#' The defaults reproduce the behaviour in force before whep#949 exactly: the
#' `region == "Global"` MMS split on any frame that does not already carry a
#' `region` column, and an assumed Temperate climate zone.
#' @noRd
.manure_options <- function(options = list()) {
  defaults <- list(
    mms_region = "as_available",
    climate_source = "assumed",
    assumed_climate_zone = "Temperate"
  )
  unknown <- setdiff(names(options), names(defaults))
  if (length(unknown) > 0) {
    cli::cli_abort(
      "Unknown manure {.arg options}: {.val {unknown}}. \\
       Valid names are {.val {names(defaults)}}.",
      class = "whep_manure_options"
    )
  }
  # rlang::arg_match() needs a symbol, so each option is bound to one first.
  opt <- utils::modifyList(defaults, options)
  mms_region <- opt$mms_region
  climate_source <- opt$climate_source
  assumed_climate_zone <- opt$assumed_climate_zone
  list(
    mms_region = rlang::arg_match(
      mms_region,
      c("as_available", "resolve", "global")
    ),
    climate_source = rlang::arg_match(
      climate_source,
      c("assumed", "from_data")
    ),
    assumed_climate_zone = rlang::arg_match(
      assumed_climate_zone,
      c("Cool", "Temperate", "Warm")
    )
  )
}

#' Which column `.resolve_mms_shares()` keys the MMS split on.
#'
#' `NULL` makes it use the `region == "Global"` rows for every row.
#' @noRd
.mms_region_col <- function(mms_region) {
  if (identical(mms_region, "global")) NULL else "region"
}

#' Resolve the IPCC region the MMS split is keyed on, and record which split
#' the frame will take.
#'
#' `"resolve"` is opt-in because the only thing a region changes here is which
#' rows of `regional_mms_distribution` apply, and its four region-specific
#' `(region, species)` pairs are an unsourced placeholder (whep#921): making
#' them live propagates placeholder detail into more of the output, which is a
#' decision for the maintainer and not a wiring cleanup (whep#949). This
#' mirrors `split_manure_management()`, whose `mms_source` defaults to the
#' Global rows for the same reason.
#'
#' A `"resolve"` request that cannot be honoured -- no `iso3`, `area_code` or
#' `polity_area_code` to resolve a region from -- warns rather than aborting,
#' because a frame with no territory (a global aggregate, a toy example) is a
#' legitimate input; `method_mms` then records the split actually used. The CH4
#' and N2O legs both pass through here, so an existing `method_mms` means the
#' frame has already been resolved and warned about once.
#' @noRd
.resolve_manure_region <- function(data, mms_region) {
  resolved_before <- rlang::has_name(data, "method_mms")
  if (identical(mms_region, "resolve") && !rlang::has_name(data, "region")) {
    if (.has_gleam_region_key(data)) {
      data <- .add_ipcc_region(data)
    } else if (!resolved_before) {
      cli::cli_warn(
        c(
          "{.arg mms_region} {.val resolve} needs {.var iso3}, {.var area_code} \\
           or {.var polity_area_code}; the frame carries none.",
          i = "Using the {.val Global} MMS split; {.var method_mms} records it."
        ),
        class = "whep_no_region_key"
      )
    }
  }
  regional <- !identical(mms_region, "global") &&
    rlang::has_name(data, "region")
  dplyr::mutate(
    data,
    method_mms = if (regional) "region_specific" else "regional_default"
  )
}

#' Attach the climate zone the MCF is read at, and record where it came from.
#'
#' `climate_source` is the one place the question "where does the zone come
#' from" is answered, and it is answered per row rather than per frame. A zone
#' the frame already carries always wins, so nothing supplied upstream is
#' discarded and those rows are stamped `climate_from_data`. Only the rows left
#' without one reach `climate_source`, which is why a hole inside a supplied
#' column and a wholly absent column are the same case here: both are an absent
#' quantity, and refusing either would drop an animal whose manure exists out of
#' the balance.
#'
#' `"from_data"` refuses to fill and aborts. `"assumed"` fills with
#' `assumed_climate_zone`, which is ASSUMED, UNVERIFIED for any particular row:
#' `"Temperate"` is the middle of the three zones `climate_mcf` keys, so it is
#' the least-committal choice, not a measurement, and `assumed_climate_zone`
#' exists so the sensitivity to it can be measured (whep#949). A gap inside a
#' supplied column also warns, because there the caller meant to resolve the
#' zone and a row got away; a wholly absent column is the documented default and
#' stamps without warning.
#'
#' An unknown *label* is not an absence and is not filled: see
#' `.check_climate_zone()`.
#' @noRd
.apply_climate_zone <- function(data, opt) {
  supplied <- rlang::has_name(data, "climate_zone")
  if (!supplied) {
    data <- dplyr::mutate(data, climate_zone = NA_character_)
  }
  gap <- is.na(data$climate_zone)
  data <- .stamp_assumption(
    data,
    "method_manure_ch4",
    "climate_from_data",
    !gap
  )
  if (any(gap)) {
    data <- .fill_climate_zone(data, gap, opt, warn = supplied)
  }
  .check_climate_zone(data)
  data
}

#' Fill the rows that carry no climate zone, the way `climate_source` says.
#' @noRd
.fill_climate_zone <- function(data, gap, opt, warn) {
  if (identical(opt$climate_source, "from_data")) {
    cli::cli_abort(
      "{.arg climate_source} {.val from_data} needs a {.var climate_zone} \\
       column holding one of {.val {c('Cool', 'Temperate', 'Warm')}} for \\
       every row; {sum(gap)} row{?s} {?has/have} none.",
      class = "whep_missing_climate_zone"
    )
  }
  zone <- opt$assumed_climate_zone
  if (warn) {
    cli::cli_warn(c(
      "!" = "{sum(gap)} row{?s} {?has/have} no {.field climate_zone}.",
      i = "Assumed {.val {zone}} and stamped in {.field method_manure_ch4};
           resolve it upstream and pass it in {.field climate_zone}."
    ))
  }
  data$climate_zone[gap] <- zone
  .stamp_assumption(
    data,
    "method_manure_ch4",
    paste0("climate_assumed_", tolower(zone)),
    gap
  )
}

#' Abort when the MMS split did not resolve a coefficient for every row.
#'
#' Both manure coefficient joins are by MMS label, and an unmatched label used
#' to take a flat default -- 2.0% for the MCF, 0.005 (IPCC's `Other`) for EF3
#' -- indistinguishable in the output from a real table hit. That is what hid
#' whep#950 for as long as it hid. A label the tables do not carry, or a
#' species `regional_mms_distribution` has no rows for, is a vocabulary defect
#' in the shipped tables: the user cannot work around it and no alternative
#' value is defensible, so it aborts naming what is missing. This is also what
#' `split_manure_management()` and `apply_management_losses()` do on the same
#' tables.
#' @noRd
.check_mms_matched <- function(joined, coef_col) {
  missing_species <- unique(joined$species_gen[is.na(joined$mms_type)])
  if (length(missing_species) > 0) {
    cli::cli_abort(
      "No {.var regional_mms_distribution} rows for species \\
       {.val {missing_species}}.",
      class = "whep_missing_mms_species"
    )
  }
  bad <- joined |>
    dplyr::filter(is.na(.data[[coef_col]])) |>
    dplyr::distinct(mms_type, dplyr::across(dplyr::any_of("climate_zone")))
  if (nrow(bad) == 0) {
    return(joined)
  }
  if (identical(coef_col, "ef3")) {
    cli::cli_abort(
      c(
        "No EF3 for manure-management system{?s} {.val {bad$mms_type}}.",
        i = "Add the {.field ipcc_2019_n2o_ef_direct} row it maps to in \\
             {.fun .manure_ef3}."
      ),
      class = "whep_missing_ef3"
    )
  }
  pairs <- paste(bad$mms_type, bad$climate_zone, sep = " / ")
  cli::cli_abort(
    c(
      "No {.var climate_mcf} row for system / climate zone \\
       {.val {pairs}}.",
      i = "Every MMS type in {.var regional_mms_distribution} needs an MCF at \\
           the climate zone in use."
    ),
    class = "whep_missing_mcf"
  )
}
