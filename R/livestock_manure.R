#' IPCC 2019 Tier 1 manure CH4.
#' @noRd
.calc_manure_ch4_tier1 <- function(data) {
  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species),
      Method_Manure_CH4 = "IPCC_2019_Tier1"
    )

  data <- .join_manure_ch4_ef_tier1(data)

  data |>
    dplyr::mutate(
      manure_ch4_tier1 = heads * manure_ef_kgch4
    )
}

#' IPCC 2019 Tier 2 manure CH4.
#' @noRd
.calc_manure_ch4_tier2 <- function(data) {
  if (!rlang::has_name(data, "GE")) {
    cli::cli_abort(
      "{.fun .calc_manure_ch4_tier2} requires {.var GE}. \\
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
      Method_Manure_CH4 = "IPCC_2019_Tier2"
    )

  data <- .calc_volatile_solids(data)
  data <- .join_bo(data)
  data <- .calc_weighted_mcf(data)

  ch4_density <- 0.67 # kg/m3 CH4 at STP

  data |>
    dplyr::mutate(
      manure_ch4_per_head = VS * 365 * Bo * ch4_density * weighted_mcf,
      manure_ch4_tier2 = heads * manure_ch4_per_head
    )
}

#' IPCC 2019 manure N2O (direct + indirect).
#' @noRd
.calc_manure_n2o <- function(data) {
  if (!rlang::has_name(data, "GE")) {
    cli::cli_abort(
      "{.fun .calc_manure_n2o} requires {.var GE}. \\
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
      Method_Manure_N2O = "IPCC_2019_Tier2"
    )

  data <- .calc_n_excretion(data)
  data <- .calc_direct_n2o(data)
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
        stringr::str_detect(species, "(?i)Dairy") &
          species_gen == "Cattle" ~ "Dairy Cattle",
        species_gen == "Cattle" ~ "Other Cattle",
        species %in% all_categories ~ species,
        TRUE ~ species_gen
      )
    )

  # Try regional match first
  if (rlang::has_name(data, "region")) {
    cattle_ef <- ipcc_2019_manure_ch4_ef_cattle |>
      dplyr::select(region, category, ef_kg_head_yr)
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
      )

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
    dplyr::mutate(
      ash_percent = dplyr::coalesce(ash_percent, 8.0),
      VS = GE *
        (1 - DE_percent / 100 + ue_factor * DE_percent / 100) *
        (1 - ash_percent / 100) /
        ge_content
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
    dplyr::rename(Bo = bo_m3_kg_vs) |>
    dplyr::mutate(
      Bo = dplyr::coalesce(Bo, 0.18)
    ) |>
    dplyr::select(-bo_category)
}

#' Map species to Bo category.
#' @noRd
.get_bo_category <- function(species, species_gen) {
  dplyr::case_when(
    stringr::str_detect(species, "(?i)Dairy") &
      species_gen == "Cattle" ~ "Dairy Cattle",
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

#' Calculate weighted MCF across MMS types.
#' @noRd
.calc_weighted_mcf <- function(data) {
  if (!rlang::has_name(data, "climate_zone")) {
    data <- data |>
      dplyr::mutate(
        climate_zone = "Temperate",
        Method_Manure_CH4 = paste0(
          Method_Manure_CH4,
          "; climate_assumed_temperate"
        )
      )
  }

  region_col <- if (rlang::has_name(data, "region")) {
    "region"
  } else {
    NULL
  }

  # Get MCF by MMS and climate zone
  mcf_tbl <- climate_mcf |>
    dplyr::select(mms_type, climate_zone, mcf_percent)

  # Get MMS distribution (use Global when no region)
  if (!is.null(region_col)) {
    mms_tbl <- regional_mms_distribution |>
      dplyr::select(region, species, mms_type, fraction)
  } else {
    mms_tbl <- regional_mms_distribution |>
      dplyr::filter(region == "Global") |>
      dplyr::select(species, mms_type, fraction)
  }

  # For each row, compute weighted MCF
  data <- data |>
    dplyr::mutate(row_id = dplyr::row_number())

  join_keys <- if (!is.null(region_col)) {
    c("species_gen" = "species", "region")
  } else {
    c("species_gen" = "species")
  }

  mms_joined <- data |>
    dplyr::select(
      row_id,
      species_gen,
      climate_zone,
      dplyr::any_of("region")
    ) |>
    dplyr::left_join(
      mms_tbl,
      by = join_keys,
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(
      mcf_tbl,
      by = c("mms_type", "climate_zone")
    ) |>
    dplyr::mutate(
      mcf_percent = dplyr::coalesce(mcf_percent, 2.0),
      fraction = dplyr::coalesce(fraction, 1.0)
    ) |>
    dplyr::summarise(
      weighted_mcf = sum(
        fraction * mcf_percent / 100,
        na.rm = TRUE
      ),
      .by = row_id
    )

  data |>
    dplyr::left_join(mms_joined, by = "row_id") |>
    dplyr::mutate(
      weighted_mcf = dplyr::coalesce(weighted_mcf, 0.02)
    ) |>
    dplyr::select(-row_id)
}

#' Build join keys for MMS distribution.
#' @noRd
.get_mms_join_keys <- function(region_col) {
  keys <- c("species_gen" = "species")
  if (!is.null(region_col)) {
    keys <- c(keys, "region" = "region")
  }
  keys
}

#' Calculate nitrogen excretion (Nex).
#' @noRd
.calc_n_excretion <- function(data) {
  ge_content <- livestock_constants$vs_energy_content_mj_kg

  # Get CP% from feed_characteristics (not hardcoded)
  if (!rlang::has_name(data, "cp_percent")) {
    if (rlang::has_name(data, "diet_quality")) {
      data <- data |>
        dplyr::left_join(
          feed_characteristics |>
            dplyr::select(diet_quality, cp_percent),
          by = "diet_quality"
        )
    } else {
      data <- data |>
        dplyr::mutate(cp_percent = 12.0)
    }
  }

  # Get N retention from table (differentiated dairy/other)
  n_ret_tbl <- ipcc_tier2_n_retention |>
    dplyr::select(category, n_retention_frac)

  data <- data |>
    dplyr::mutate(
      n_ret_category = .get_bo_category(species, species_gen)
    ) |>
    dplyr::left_join(
      n_ret_tbl,
      by = c("n_ret_category" = "category")
    ) |>
    dplyr::mutate(
      n_retention_frac = dplyr::coalesce(
        n_retention_frac,
        0.07
      ),
      N_intake = (GE / ge_content) *
        (cp_percent / 100) /
        6.25,
      Nex = N_intake * (1 - n_retention_frac) * livestock_constants$days_in_year
    ) |>
    dplyr::select(-n_ret_category)
}

#' Calculate direct N2O from manure management.
#' @noRd
.calc_direct_n2o <- function(data) {
  # EF3 from Table 10.21. Column names: system, ef_kg_n2o_n_per_kg_n
  ef3_tbl <- ipcc_2019_n2o_ef_direct |>
    dplyr::rename(mms_type = system, ef3 = ef_kg_n2o_n_per_kg_n)

  n2o_to_n <- livestock_constants$n_to_n2o

  if (rlang::has_name(data, "region")) {
    data <- .calc_weighted_direct_n2o(
      data,
      ef3_tbl,
      n2o_to_n
    )
  } else {
    pasture_ef <- ef3_tbl |>
      dplyr::filter(
        stringr::str_detect(mms_type, "(?i)Pasture")
      ) |>
      dplyr::pull(ef3) |>
      mean(na.rm = TRUE)
    pasture_ef <- dplyr::coalesce(pasture_ef, 0.02)

    data <- data |>
      dplyr::mutate(
        manure_n2o_direct = heads * Nex * pasture_ef * n2o_to_n
      )
  }

  data
}

#' Weighted direct N2O when region is available.
#' @noRd
.calc_weighted_direct_n2o <- function(data, ef3_tbl, n2o_to_n) {
  mms_tbl <- regional_mms_distribution |>
    dplyr::select(region, species, mms_type, fraction)

  data <- data |>
    dplyr::mutate(row_id_n2o = dplyr::row_number())

  n2o_weighted <- data |>
    dplyr::select(
      row_id_n2o,
      species_gen,
      Nex,
      heads,
      dplyr::any_of("region")
    ) |>
    dplyr::left_join(
      mms_tbl,
      by = c("species_gen" = "species", "region"),
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(ef3_tbl, by = "mms_type") |>
    dplyr::mutate(
      ef3 = dplyr::coalesce(ef3, 0.005),
      fraction = dplyr::coalesce(fraction, 1.0)
    ) |>
    dplyr::summarise(
      weighted_ef3 = sum(fraction * ef3, na.rm = TRUE),
      .by = row_id_n2o
    )

  data |>
    dplyr::left_join(n2o_weighted, by = "row_id_n2o") |>
    dplyr::mutate(
      manure_n2o_direct = heads *
        Nex *
        dplyr::coalesce(weighted_ef3, 0.005) *
        n2o_to_n
    ) |>
    dplyr::select(-row_id_n2o, -weighted_ef3)
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

  data |>
    dplyr::mutate(
      n2o_volatilization = heads * Nex * frac_gas * ef4 * n2o_to_n,
      n2o_leaching = heads * Nex * frac_leach * ef5 * n2o_to_n,
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
