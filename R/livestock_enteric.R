#' Calculate enteric methane emissions - Tier 1.
#'
#' @description
#' IPCC 2019 Tier 1 approach: applies regional emission factors
#' from Tables 10.10/10.11 to animal populations.
#' CH4 (kg/yr) = heads * EF (kg CH4/head/yr).
#'
#' @param data Dataframe with `species`, `heads`, and optionally
#'   `region`.
#'
#' @return Dataframe with `enteric_ch4_tier1` (kg/yr) and
#'   `Method_Enteric` column.
#' @export
#'
#' @examples
#' \dontrun{
#'   tibble::tibble(
#'     species = "Dairy Cattle", heads = 100,
#'     region = "Western Europe"
#'   ) |>
#'     calc_enteric_ch4_tier1()
#' }
calc_enteric_ch4_tier1 <- function(data) {
  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species),
      Method_Enteric = "IPCC_2019_Tier1"
    )

  # Determine best EF table
  cattle_ef <- ipcc_2019_enteric_ef_cattle |>
    dplyr::rename(
      ef_cattle = ef_kg_head_yr
    )
  other_ef <- ipcc_2019_enteric_ef_other |>
    dplyr::rename(
      ef_other = ef_kg_head_yr
    )

  data <- .join_enteric_ef_tier1(data, cattle_ef, other_ef)

  data |>
    dplyr::mutate(
      enteric_ch4_tier1 = heads * enteric_ef_kgch4
    ) |>
    dplyr::select(-dplyr::any_of(c("ef_cattle", "ef_other")))
}

#' Calculate enteric methane emissions - Tier 2.
#'
#' @description
#' IPCC 2019 Tier 2 approach:
#' CH4 (kg/head/yr) = GE * (Ym/100) * 365 / 55.65.
#' Ym sourced from `ipcc_tier2_ym_values` (Table 10.12).
#'
#' Requires a prior call to `estimate_energy_demand()` so that
#' `GE` is available. If `GE` is missing, falls back to Tier 1.
#'
#' @param data Dataframe with `GE`, `species`, `heads`,
#'   optionally `diet_quality`, `system`, and `weight`.
#'
#' @return Dataframe with `enteric_ch4_tier2` (kg/yr) and
#'   `Method_Enteric` column.
#' @export
#'
#' @examples
#' \dontrun{
#'   tibble::tibble(
#'     species = "Dairy Cattle", cohort = "Adult Female",
#'     heads = 100, weight = 600, diet_quality = "High",
#'     milk_yield_kg_day = 20
#'   ) |>
#'     estimate_energy_demand() |>
#'     calc_enteric_ch4_tier2()
#' }
calc_enteric_ch4_tier2 <- function(data) {
  if (!"GE" %in% names(data)) {
    cli::cli_abort(
      "{.fun calc_enteric_ch4_tier2} requires {.var GE}. \\
       Run {.fun estimate_energy_demand} first."
    )
  }

  data <- data |>
    dplyr::mutate(
      species_gen = dplyr::coalesce(
        species_gen, .get_general_species(species)
      )
    )

  data <- .join_ym(data)

  energy_conversion <- livestock_constants$energy_content_ch4_mj_kg

  data |>
    dplyr::mutate(
      enteric_ch4_per_head = GE * (Ym / 100) * 365 /
        energy_conversion,
      enteric_ch4_tier2 = heads * enteric_ch4_per_head,
      Method_Enteric = "IPCC_2019_Tier2"
    )
}

# Private helpers ----

#' Join Tier 1 enteric EF (cattle tables have regional detail).
#' @noRd
.join_enteric_ef_tier1 <- function(data, cattle_ef, other_ef) {
  cattle_rows <- data |>
    dplyr::filter(species_gen %in% c("Cattle", "Buffalo"))

  other_rows <- data |>
    dplyr::filter(
      !species_gen %in% c("Cattle", "Buffalo")
    )

  if (nrow(cattle_rows) > 0) {
    cattle_rows <- .join_cattle_ef(cattle_rows, cattle_ef)
  }

  if (nrow(other_rows) > 0) {
    other_rows <- .join_other_ef(other_rows, other_ef)
  }

  dplyr::bind_rows(cattle_rows, other_rows) |>
    dplyr::rename(enteric_ef_kgch4 = dplyr::any_of(
      c("ef_cattle", "ef_other", "enteric_ef_kgch4")
    ))
}

#' Join cattle-specific enteric EFs with regional fallback.
#' @noRd
.join_cattle_ef <- function(cattle_rows, cattle_ef) {
  cattle_category <- dplyr::case_when(
    grepl("Dairy", cattle_rows$species, ignore.case = TRUE) ~
      "Dairy Cattle",
    TRUE ~ "Other Cattle"
  )
  cattle_rows <- cattle_rows |>
    dplyr::mutate(cattle_category = cattle_category)

  if ("region" %in% names(cattle_rows)) {
    cattle_rows <- cattle_rows |>
      dplyr::left_join(
        cattle_ef,
        by = c(
          "region" = "region",
          "cattle_category" = "category"
        )
      )
    # Global fallback for missing regions
    missing <- is.na(cattle_rows$ef_cattle)
    if (any(missing)) {
      global_ef <- cattle_ef |>
        dplyr::filter(region == "Global") |>
        dplyr::select(category, ef_global = ef_cattle)
      cattle_rows <- cattle_rows |>
        dplyr::left_join(
          global_ef,
          by = c("cattle_category" = "category")
        ) |>
        dplyr::mutate(
          ef_cattle = dplyr::coalesce(ef_cattle, ef_global)
        ) |>
        dplyr::select(-ef_global)
    }
  } else {
    global_ef <- cattle_ef |>
      dplyr::filter(region == "Global") |>
      dplyr::select(category, ef_cattle)
    cattle_rows <- cattle_rows |>
      dplyr::left_join(
        global_ef,
        by = c("cattle_category" = "category")
      )
  }

  cattle_rows |>
    dplyr::rename(enteric_ef_kgch4 = ef_cattle) |>
    dplyr::select(-cattle_category)
}

#' Join non-cattle enteric EFs.
#' Handles subcategories (e.g. "Swine - Market") by
#' matching on prefix when exact match fails.
#' @noRd
.join_other_ef <- function(other_rows, other_ef) {
  ef_tbl <- other_ef |>
    dplyr::select(category, ef_other)

  # Create aggregated EF for species with subcategories
  ef_agg <- ef_tbl |>
    dplyr::mutate(
      species_base = stringr::str_extract(
        category, "^[^-]+"
      ) |>
        stringr::str_trim()
    ) |>
    dplyr::summarise(
      ef_agg = mean(ef_other, na.rm = TRUE),
      .by = species_base
    )

  other_rows |>
    dplyr::left_join(
      ef_tbl,
      by = c("species_gen" = "category")
    ) |>
    dplyr::left_join(
      ef_agg,
      by = c("species_gen" = "species_base")
    ) |>
    dplyr::mutate(
      enteric_ef_kgch4 = dplyr::coalesce(ef_other, ef_agg)
    ) |>
    dplyr::select(-ef_other, -ef_agg)
}

#' Join Ym values from Table 10.12 with feed situation mapping.
#' @noRd
.join_ym <- function(data) {
  ym_tbl <- ipcc_tier2_ym_values

  if (!"diet_quality" %in% names(data)) {
    data <- data |>
      dplyr::mutate(diet_quality = "Medium")
  }

  # Ensure system column exists
  if (!"system" %in% names(data)) {
    data <- data |> dplyr::mutate(system = NA_character_)
  }

  # Map diet_quality to feed_situation
  data <- data |>
    dplyr::mutate(
      feed_situation = dplyr::case_when(
        !is.na(system) & system == "Feedlot" ~ "Feedlot",
        TRUE ~ diet_quality
      )
    )

  # Map sheep by body weight per Table 10.12 footnote
  data <- data |>
    dplyr::mutate(
      feed_situation = dplyr::case_when(
        species_gen == "Sheep" & !is.na(weight) &
          weight < 75 ~ "Low",
        species_gen == "Sheep" & !is.na(weight) &
          weight >= 75 ~ "High",
        TRUE ~ feed_situation
      )
    )

  data |>
    dplyr::left_join(
      ym_tbl,
      by = c(
        "species_gen" = "category",
        "feed_situation"
      )
    ) |>
    dplyr::mutate(
      Ym = dplyr::coalesce(ym_percent, 6.5)
    ) |>
    dplyr::select(-dplyr::any_of(c(
      "ym_percent", "feed_situation"
    )))
}
