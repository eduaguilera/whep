#' Estimate energy demand (Gross Energy) - Tier 2
#'
#' @description
#' Calculate Gross Energy (GE) intake per IPCC 2019 Tier 2 equations
#' (Vol 4, Ch 10). Estimates Net Energy for Maintenance (NEm),
#' Activity (NEa), Lactation (NEl), Work (NEwork), Pregnancy (NEp),
#' Growth (NEg), and Wool (NEwool), then derives total GE using the
#' REM/REG ratio approach from IPCC Eq 10.16.
#'
#' All coefficients come from internal package data.
#'
#' @param data A dataframe with columns `species`, `cohort`, `heads`,
#'   and optionally `iso3`. Optional production columns: `weight`,
#'   `milk_yield_kg_day`, `fat_percent`, `weight_gain_kg_day`,
#'   `work_hours_day`, `pregnant_fraction`, `temperature_c`,
#'   `diet_quality`, `grazing_distance_km`, `system`.
#' @param method Method for calculation (default `"ipcc2019"`).
#'
#' @return Dataframe with added `GE` (MJ/day), intermediate NE
#'   components, and `Method_Energy` tracking column.
#' @export
#'
#' @examples
#' \dontrun{
#'   data <- tibble::tibble(
#'     species = "Dairy Cattle", cohort = "Adult Female",
#'     heads = 100, weight = 600, diet_quality = "High",
#'     milk_yield_kg_day = 20
#'   )
#'   estimate_energy_demand(data)
#' }
estimate_energy_demand <- function(data, method = "ipcc2019") {
  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species),
      subcategory = .get_subcategory(species),
      Method_Energy = "IPCC_2019_Tier2"
    )

  data <- .join_weights(data)
  data <- .join_energy_coefs(data)
  data <- .ensure_production_cols(data)

  data <- .join_production_defaults(data)
  data <- .set_activity_coef(data)
  data <- .join_feed_characteristics(data)
  data <- .join_temperature_adjustment(data)

  data <- data |>
    calc_energy_maintenance() |>
    calc_energy_activity() |>
    calc_energy_lactation() |>
    calc_energy_wool() |>
    calc_energy_work() |>
    calc_energy_pregnancy() |>
    calc_energy_growth()

  data <- data |>
    dplyr::mutate(
      NE_maintenance = NEm + NEa + NEl + NEwork + NEp,
      NE_growth = NEg + NEwool
    )

  estimate_gross_energy(data)
}

#' Calculate net energy for maintenance.
#'
#' @description
#' IPCC Eq 10.3: NEm = Cfi * Weight^0.75 * (1 + temp_adjustment).
#'
#' @param data Dataframe with `cfi_mj_day_kg075`, `weight`,
#'   `temp_adjustment`.
#'
#' @return Dataframe with added `NEm` column.
#' @export
calc_energy_maintenance <- function(data) {
  data |>
    dplyr::mutate(
      NEm = cfi_mj_day_kg075 * weight^0.75 *
        (1 + temp_adjustment)
    )
}

#' Calculate net energy for activity.
#'
#' @description
#' IPCC Eq 10.4: NEa = Ca * NEm. Walking energy added per
#' NRC 2001.
#'
#' @param data Dataframe with `Ca`, `NEm`, `weight`,
#'   `grazing_distance_km`.
#'
#' @return Dataframe with added `NEa` column.
#' @export
calc_energy_activity <- function(data) {
  walking_cost <- grazing_energy_coefs$value_mj_kg_km[
    grazing_energy_coefs$parameter == "walking_energy_cost"
  ]
  data |>
    dplyr::mutate(
      NEa = Ca * NEm +
        walking_cost * weight * grazing_distance_km
    )
}

#' Calculate net energy for lactation.
#'
#' @description
#' IPCC Eq 10.8 (cattle/buffalo):
#' NEl = Milk * (1.47 + 0.40 * Fat%).
#' IPCC Eq 10.9 (sheep/goats): NEl = Milk * 4.6.
#' If protein and lactose data are available, uses the enhanced
#' NRC equation:
#' NEl = Milk * (0.389*Fat + 0.229*Prot + 0.165*Lact).
#'
#' @param data Dataframe with `milk_yield_kg_day`, `fat_percent`,
#'   `protein_percent`, `lactose_percent`, `species_gen`.
#'
#' @return Dataframe with added `NEl` column.
#' @export
calc_energy_lactation <- function(data) {
  data |>
    dplyr::mutate(
      NEl = dplyr::case_when(
        is.na(milk_yield_kg_day) |
          milk_yield_kg_day == 0 ~ 0,
        !is.na(protein_percent) & protein_percent > 0 &
          !is.na(lactose_percent) & lactose_percent > 0 ~
          milk_yield_kg_day * (
            0.389 * fat_percent +
              0.229 * protein_percent +
              0.165 * lactose_percent
          ),
        species_gen %in% c("Sheep", "Goats") ~
          milk_yield_kg_day * 4.6,
        TRUE ~
          milk_yield_kg_day * (1.47 + 0.40 * fat_percent)
      )
    )
}

#' Calculate net energy for wool production.
#'
#' @description
#' IPCC Eq 10.12: NEwool = EVwool * wool_production / 365.
#' EVwool = 24 MJ/kg clean wool. Only applies to sheep.
#'
#' @param data Dataframe with `wool_production_kg_yr`,
#'   `species_gen`.
#'
#' @return Dataframe with added `NEwool` column.
#' @export
calc_energy_wool <- function(data) {
  ev_wool <- livestock_constants$ev_wool_mj_kg
  data |>
    dplyr::mutate(
      NEwool = dplyr::if_else(
        species_gen == "Sheep",
        dplyr::coalesce(wool_production_kg_yr, 0) /
          365 * ev_wool,
        0
      )
    )
}

#' Calculate net energy for work.
#'
#' @description
#' IPCC Eq 10.11: NEwork = Cw * NEm * hours_per_day.
#'
#' @param data Dataframe with `cw`, `NEm`, `work_hours_day`.
#'
#' @return Dataframe with added `NEwork` column.
#' @export
calc_energy_work <- function(data) {
  data |>
    dplyr::mutate(NEwork = cw * NEm * work_hours_day)
}

#' Calculate net energy for pregnancy.
#'
#' @description
#' IPCC Eq 10.13: NEp = Cp * NEm * pregnant_fraction.
#'
#' @param data Dataframe with `cp`, `NEm`, `pregnant_fraction`.
#'
#' @return Dataframe with added `NEp` column.
#' @export
calc_energy_pregnancy <- function(data) {
  data |>
    dplyr::mutate(NEp = cp * NEm * pregnant_fraction)
}

#' Calculate net energy for growth.
#'
#' @description
#' Simplified IPCC Eq 10.6:
#' NEg = weight_gain_kg_day * energy_content_gain_mj_kg.
#' Species-specific energy content from `ipcc_tier2_energy_coefs`.
#'
#' @param data Dataframe with `weight_gain_kg_day`,
#'   `energy_content_gain_mj_kg`.
#'
#' @return Dataframe with added `NEg` column.
#' @export
calc_energy_growth <- function(data) {
  data |>
    dplyr::mutate(
      NEg = weight_gain_kg_day * energy_content_gain_mj_kg
    )
}

#' Estimate Gross Energy from Net Energy components.
#'
#' @description
#' IPCC Eq 10.16:
#' GE = (NE_maintenance/REM + NE_growth/REG) / (DE%/100).
#'
#' REM from IPCC Eq 10.14. REG from IPCC Eq 10.15.
#'
#' @param data Dataframe with `NE_maintenance`, `NE_growth`,
#'   optionally `de_percent`.
#'
#' @return Dataframe with added `GE`, `REM`, `REG`, `DE_percent`.
#' @export
estimate_gross_energy <- function(data) {
  if (!rlang::has_name(data, "de_percent")) {
    data <- data |>
      dplyr::mutate(
        de_percent = livestock_constants$default_de_percent
      )
  }

  data |>
    dplyr::mutate(
      DE_percent = de_percent,
      REM = .calc_rem(DE_percent),
      REG = .calc_reg(DE_percent),
      GE = (NE_maintenance / REM + NE_growth / REG) /
        (DE_percent / 100)
    )
}

# Private helpers ----

#' Map species string to general category.
#' @noRd
.get_general_species <- function(s) {
  dplyr::case_when(
    grepl("Cattle", s, ignore.case = TRUE)    ~ "Cattle",
    grepl("Buffalo", s, ignore.case = TRUE)   ~ "Buffalo",
    grepl("Sheep", s, ignore.case = TRUE)     ~ "Sheep",
    grepl("Goat", s, ignore.case = TRUE)      ~ "Goats",
    grepl("Pig|Swine", s, ignore.case = TRUE) ~ "Swine",
    grepl("Poultry|Chicken|Hen", s,
          ignore.case = TRUE)                 ~ "Poultry",
    grepl("Horse", s, ignore.case = TRUE)     ~ "Horses",
    grepl("Camel", s, ignore.case = TRUE)     ~ "Camels",
    grepl("Mule|Ass|Donkey", s,
          ignore.case = TRUE)                 ~ "Mules and Asses",
    TRUE ~ s
  )
}

#' Determine subcategory (Dairy vs Non-Dairy).
#' @noRd
.get_subcategory <- function(s) {
  dplyr::case_when(
    grepl("Dairy", s, ignore.case = TRUE) ~ "Dairy",
    grepl("Cattle|Buffalo", s,
          ignore.case = TRUE)             ~ "Non-Dairy",
    TRUE ~ "All"
  )
}

#' Join animal weights with regional fallback.
#' @noRd
.join_weights <- function(data) {
  if (!rlang::has_name(data, "weight")) {
    data$weight <- NA_real_
  }

  global_wts <- gleam_animal_weights |>
    dplyr::filter(region == "Global") |>
    dplyr::group_by(species, cohort) |>
    dplyr::summarise(
      weight_kg_global = mean(weight_kg, na.rm = TRUE),
      .groups = "drop"
    )

  if ("iso3" %in% names(data)) {
    data <- data |>
      dplyr::left_join(
        gleam_geographic_hierarchy |>
          dplyr::select(iso3, gleam_region),
        by = "iso3"
      ) |>
      dplyr::left_join(
        gleam_animal_weights |>
          dplyr::group_by(region, species, cohort) |>
          dplyr::summarise(
            weight_kg = mean(weight_kg, na.rm = TRUE),
            .groups = "drop"
          ),
        by = c(
          "gleam_region" = "region",
          "species_gen" = "species",
          "cohort"
        )
      ) |>
      dplyr::left_join(
        global_wts,
        by = c("species_gen" = "species", "cohort")
      ) |>
      dplyr::mutate(
        weight = dplyr::coalesce(
          weight, weight_kg, weight_kg_global
        )
      ) |>
      dplyr::select(
        -dplyr::any_of(c("weight_kg", "weight_kg_global"))
      )
  } else {
    data <- data |>
      dplyr::left_join(
        global_wts,
        by = c("species_gen" = "species", "cohort")
      ) |>
      dplyr::mutate(
        weight = dplyr::coalesce(weight, weight_kg_global)
      ) |>
      dplyr::select(-dplyr::any_of("weight_kg_global"))
  }
  data
}

#' Join energy coefficients by species and subcategory.
#' @noRd
.join_energy_coefs <- function(data) {
  data |>
    dplyr::left_join(
      ipcc_tier2_energy_coefs,
      by = c("species_gen" = "category", "subcategory")
    )
}

#' Ensure all optional production columns exist as NA.
#' @noRd
.ensure_production_cols <- function(data) {
  optional <- c(
    "milk_yield_kg_day", "fat_percent", "protein_percent",
    "lactose_percent", "weight_gain_kg_day", "work_hours_day",
    "pregnant_fraction", "wool_production_kg_yr"
  )
  for (col in optional) {
    if (!col %in% names(data)) {
      data[[col]] <- NA_real_
    }
  }
  data
}

#' Join production defaults and fill missing values.
#' @noRd
.join_production_defaults <- function(data) {
  defaults <- livestock_production_defaults |>
    dplyr::rename(species_match = category)

  data |>
    dplyr::left_join(
      defaults,
      by = c("species" = "species_match"),
      suffix = c("", "_default")
    ) |>
    dplyr::mutate(
      milk_yield_kg_day = dplyr::coalesce(
        milk_yield_kg_day, 0
      ),
      fat_percent = dplyr::coalesce(
        fat_percent, fat_percent_default
      ),
      protein_percent = dplyr::coalesce(
        protein_percent, protein_percent_default
      ),
      lactose_percent = dplyr::coalesce(
        lactose_percent, lactose_percent_default
      ),
      weight_gain_kg_day = dplyr::coalesce(
        weight_gain_kg_day, weight_gain_kg_day_default, 0
      ),
      work_hours_day = dplyr::coalesce(
        work_hours_day, work_hours_day_default, 0
      ),
      pregnant_fraction = dplyr::coalesce(
        pregnant_fraction, pregnant_fraction_default, 0
      )
    ) |>
    dplyr::select(-dplyr::ends_with("_default"))
}

#' Set activity coefficient based on production system.
#' @noRd
.set_activity_coef <- function(data) {
  if (!"system" %in% names(data)) {
    data <- data |> dplyr::mutate(Ca = ca_pasture)
  } else {
    data <- data |>
      dplyr::mutate(
        Ca = dplyr::if_else(
          system == "Feedlot", ca_feedlot, ca_pasture
        )
      )
  }
  if (!"grazing_distance_km" %in% names(data)) {
    data <- data |> dplyr::mutate(grazing_distance_km = 0)
  }
  data
}

#' Join feed characteristics by diet quality.
#' @noRd
.join_feed_characteristics <- function(data) {
  if (!"diet_quality" %in% names(data)) {
    cli::cli_warn(
      "{.arg diet_quality} not provided. Using default DE%."
    )
    data <- data |>
      dplyr::mutate(
        diet_quality = NA_character_,
        Method_Energy = paste0(
          Method_Energy, "; diet_quality_MISSING"
        )
      )
  }
  has_de <- rlang::has_name(data, "de_percent")

  data <- data |>
    dplyr::left_join(
      feed_characteristics |>
        dplyr::select(
          diet_quality, de_percent, ndf_percent
        ),
      by = "diet_quality",
      suffix = c("", "_feed")
    )

  if (has_de) {
    data <- data |>
      dplyr::mutate(
        de_percent = dplyr::coalesce(
          de_percent, de_percent_feed
        )
      ) |>
      dplyr::select(-de_percent_feed)
  }

  data
}

#' Join temperature adjustment factors.
#' @noRd
.join_temperature_adjustment <- function(data) {
  if (!"temperature_c" %in% names(data)) {
    data <- data |>
      dplyr::mutate(
        temperature_c = 15,
        Method_Energy = paste0(
          Method_Energy, "; temp_assumed_15C"
        )
      )
  }

  temp_adj <- temperature_adjustment |>
    dplyr::select(temp_min, temp_max, adjustment_factor)

  data |>
    dplyr::cross_join(temp_adj) |>
    dplyr::filter(
      temperature_c >= temp_min & temperature_c < temp_max
    ) |>
    dplyr::group_by(
      dplyr::across(
        -c(temp_min, temp_max, adjustment_factor)
      )
    ) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename(temp_adjustment = adjustment_factor) |>
    dplyr::select(-temp_min, -temp_max)
}

#' REM: ratio NE-maintenance to DE consumed.
#' IPCC 2006 Eq 10.14.
#' @noRd
.calc_rem <- function(de_pct) {
  rem <- 1.123 - (4.092e-3 * de_pct) +
    (1.126e-5 * de_pct^2) - (25.4 / de_pct)
  pmax(0.3, pmin(0.8, rem))
}

#' REG: ratio NE-growth to DE consumed.
#' IPCC 2006 Eq 10.15.
#' @noRd
.calc_reg <- function(de_pct) {
  reg <- 1.164 - (5.160e-3 * de_pct) +
    (1.308e-5 * de_pct^2) - (37.4 / de_pct)
  pmax(0.2, pmin(0.6, reg))
}
