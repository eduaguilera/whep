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
#'   `mms_shares` selects which half of [regional_mms_distribution] the
#'   split is read from: `"gleam_2_0"` (default) is the GLEAM 2.0 Supplement
#'   S1 Tab. 4.2-4.11 ingest, `"placeholder"` the unsourced table it replaced
#'   in whep#958. The placeholder stays selectable so the values WHEP
#'   published before that ingest remain reproducible and the sensitivity to
#'   it stays measurable; it is not a defensible alternative estimate.
#'
#'   `mms_region` selects how the manure-management split in
#'   [regional_mms_distribution] is keyed:
#'   * `"as_available"` (default): a row uses its own region when the frame
#'     already carries a `region` column, and the `region == "Global"` split
#'     otherwise. Tier 1 resolves a region for the (sourced) per-head
#'     N-excretion table and so takes the region-specific split; Tier 2 carries
#'     no region and so takes the Global one.
#'   * `"resolve"`: the IPCC region is resolved from `iso3`, `area_code` or
#'     `polity_area_code` where it is missing, which makes the table's
#'     region-specific rows live on the Tier 2 path too. Opt-in because it
#'     changes which rows of the table apply, not because the rows are
#'     doubtful: since whep#958 they are the GLEAM 2.0 ingest.
#'   * `"global"`: every row takes the `region == "Global"` split, whatever
#'     region column it carries.
#'
#'   `climate_source` selects the climate zone the methane conversion factors
#'   in [climate_mcf] are read at. A `climate_zone` column already on the frame
#'   is always used. `"assumed"` (default) fills a missing one with
#'   `assumed_climate_zone`; `"from_data"` aborts instead of assuming.
#'
#'   `assumed_climate_zone` is the zone `"assumed"` fills in: `"Cool"`,
#'   `"Temperate"` (default) or `"Warm"`. WHEP has no territory-to-zone
#'   crosswalk, so the whole world is assumed Temperate unless a caller
#'   supplies zones; `method_manure_ch4` records which of the two happened, and
#'   this argument exists so the sensitivity to the assumption can be measured
#'   (whep#949).
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
    dplyr::mutate(
      ash_percent = dplyr::coalesce(ash_percent, 8.0),
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
    dplyr::mutate(
      methane_potential = dplyr::coalesce(methane_potential, 0.18)
    ) |>
    dplyr::select(-bo_category)
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

#' Calculate weighted MCF across MMS types.
#'
#' Every row's MCF is the mean over its own MMS distribution, so an MMS type
#' with no `climate_mcf` row for the row's climate zone aborts rather than
#' taking a flat default: it means the MMS vocabulary and the MCF table have
#' drifted apart, which is a defect in the tables, not a modelling choice
#' (whep#950).
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

  mms_joined <- data |>
    dplyr::select(
      row_id,
      species_gen,
      climate_zone,
      dplyr::any_of("region")
    ) |>
    .resolve_mms_shares(
      .mms_region_col(opt$mms_region),
      shares = opt$mms_shares
    ) |>
    dplyr::left_join(
      mcf_tbl,
      by = c("mms_type", "climate_zone")
    ) |>
    .check_mms_matched("mcf_percent")

  weighted <- mms_joined |>
    dplyr::summarise(
      weighted_mcf = sum(fraction * mcf_percent / 100),
      .by = row_id
    )

  data |>
    dplyr::left_join(weighted, by = "row_id") |>
    dplyr::select(-row_id)
}

#' Calculate nitrogen excretion (n_excretion).
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
      # Default crude protein when the diet join left it NA, so N intake (and
      # thus Tier 2 manure N2O) resolves instead of propagating NA.
      cp_percent = dplyr::coalesce(cp_percent, 12),
      n_intake = (gross_energy / ge_content) *
        (cp_percent / 100) /
        6.25,
      n_excretion = n_intake *
        (1 - n_retention_frac) *
        livestock_constants$days_in_year
    ) |>
    dplyr::select(-n_ret_category)
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
    opt$mms_region,
    opt$mms_shares
  )
}

#' Direct N2O weighted over the row's manure-management distribution.
#'
#' `ef3_tbl` is [.manure_ef3()], the crosswalk that resolves each of the six
#' MMS labels the engine carries onto its `ipcc_2019_n2o_ef_direct` row. An
#' unresolved label aborts: it used to silently take the table's 0.005 `Other`
#' value, which is what put 80% of poultry manure on 0.005 where the litter
#' rows give 0.001 (whep#950).
#' @noRd
.calc_weighted_direct_n2o <- function(
  data,
  ef3_tbl,
  n2o_to_n,
  mms_region = "as_available",
  mms_shares = "gleam_2_0"
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
    .resolve_mms_shares(
      .mms_region_col(mms_region),
      shares = mms_shares
    ) |>
    dplyr::left_join(ef3_tbl, by = "mms_type") |>
    .check_mms_matched("ef3") |>
    dplyr::summarise(
      weighted_ef3 = sum(fraction * ef3),
      .by = row_id_n2o
    )

  n_animals <- .animal_count(data)
  data |>
    dplyr::left_join(n2o_weighted, by = "row_id_n2o") |>
    dplyr::mutate(
      manure_n2o_direct = n_animals *
        n_excretion *
        weighted_ef3 *
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
#' Every default but `mms_shares` reproduces the behaviour in force before
#' whep#949 exactly: the `region == "Global"` MMS split on any frame that does
#' not already carry a `region` column, and an assumed Temperate climate zone.
#' `mms_shares` defaults to the sourced GLEAM 2.0 ingest, which is what moved
#' published manure emissions in whep#958.
#' @noRd
.manure_options <- function(options = list()) {
  defaults <- list(
    mms_shares = "gleam_2_0",
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
  mms_shares <- opt$mms_shares
  mms_region <- opt$mms_region
  climate_source <- opt$climate_source
  assumed_climate_zone <- opt$assumed_climate_zone
  list(
    mms_shares = .mms_shares_arg(mms_shares),
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
#' rows of `regional_mms_distribution` apply, and turning it on moves numbers
#' on every Tier 2 frame at once; that is a decision for the maintainer, not a
#' wiring cleanup (whep#949). Before whep#958 there was a second reason -- the
#' region-specific rows were an unsourced placeholder (whep#921) -- which the
#' GLEAM 2.0 ingest removed. This mirrors `split_manure_management()`, whose
#' `mms_source` defaults to the Global rows.
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
#' A zone the frame already carries always wins, so nothing supplied upstream
#' is discarded. `"from_data"` demands one; `"assumed"` fills a missing one
#' with `assumed_climate_zone` and says so in `method_manure_ch4`.
#'
#' Where the zone *should* come from is an open question (whep#949): IPCC 2019
#' Vol 4 Ch 10 defines Cool / Temperate / Warm by annual mean temperature, and
#' WHEP has CRU air temperature, but the thresholds and the aggregation from
#' cells to a reporting territory are a methodological choice that is not made
#' here. `assumed_climate_zone` exists so the sensitivity to that choice can be
#' measured in the meantime.
#' @noRd
.apply_climate_zone <- function(data, opt) {
  if (rlang::has_name(data, "climate_zone")) {
    return(.stamp_ch4_method(data, "climate_from_data"))
  }
  if (identical(opt$climate_source, "from_data")) {
    cli::cli_abort(
      "{.arg climate_source} {.val from_data} needs a {.var climate_zone} \\
       column holding one of {.val {c('Cool', 'Temperate', 'Warm')}}.",
      class = "whep_missing_climate_zone"
    )
  }
  data |>
    dplyr::mutate(climate_zone = opt$assumed_climate_zone) |>
    .stamp_ch4_method(
      paste0("climate_assumed_", tolower(opt$assumed_climate_zone))
    )
}

#' Append a marker to `method_manure_ch4` when the frame carries one.
#'
#' The tier functions create the column; the private helpers are also called
#' directly on bare frames in tests, which have no method column to append to.
#' @noRd
.stamp_ch4_method <- function(data, marker) {
  if (!rlang::has_name(data, "method_manure_ch4")) {
    return(data)
  }
  dplyr::mutate(
    data,
    method_manure_ch4 = paste0(method_manure_ch4, "; ", marker)
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
