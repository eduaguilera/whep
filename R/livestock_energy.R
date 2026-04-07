#' Estimate energy demand (Gross Energy) - Tier 2
#'
#' @description
#' Calculate gross energy (GE) intake per IPCC 2019 Tier 2 equations
#' (Vol 4, Ch 10). Estimates net energy components for maintenance,
#' activity, lactation, work, pregnancy, growth, and wool, then
#' derives total gross energy using the REM/REG ratio approach from
#' IPCC Eq 10.16.
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
#' @return Dataframe with added `gross_energy` (MJ/day), intermediate
#'   net energy components, and `method_energy` tracking column.
#' @export
#'
#' @examples
#' tibble::tibble(
#'   species = "Dairy Cattle", cohort = "Adult Female",
#'   heads = 100, weight = 600, diet_quality = "High",
#'   milk_yield_kg_day = 20
#' ) |>
#'   estimate_energy_demand() |>
#'   dplyr::select(species, cohort, heads, ne_maintenance,
#'     ne_activity, ne_lactation, ne_growth, gross_energy)
estimate_energy_demand <- function(data, method = "ipcc2019") {
  data <- data |>
    dplyr::mutate(
      species_gen = .get_general_species(species),
      subcategory = .get_subcategory(species),
      method_energy = "IPCC_2019_Tier2"
    )

  data <- .join_weights(data)
  data <- .join_energy_coefs(data)
  data <- .ensure_production_cols(data)

  data <- .join_production_defaults(data)
  data <- .set_activity_coef(data)
  data <- .join_feed_characteristics(data)
  data <- .join_temperature_adjustment(data)

  data <- data |>
    .calc_energy_maintenance() |>
    .calc_energy_activity() |>
    .calc_energy_lactation() |>
    .calc_energy_wool() |>
    .calc_energy_work() |>
    .calc_energy_pregnancy() |>
    .calc_energy_growth()

  data <- data |>
    dplyr::mutate(
      ne_total_maintenance = ne_maintenance +
        ne_activity +
        ne_lactation +
        ne_work +
        ne_pregnancy,
      ne_total_growth = ne_growth + ne_wool
    )

  .estimate_gross_energy(data)
}

#' NEm: IPCC Eq 10.3.
#' @noRd
.calc_energy_maintenance <- function(data) {
  data |>
    dplyr::mutate(
      ne_maintenance = cfi_mj_day_kg075 * weight^0.75 * (1 + temp_adjustment)
    )
}

#' NEa: IPCC Eq 10.4.
#' @noRd
.calc_energy_activity <- function(data) {
  walking_cost <- grazing_energy_coefs$value_mj_kg_km[
    grazing_energy_coefs$parameter == "walking_energy_cost"
  ]
  data |>
    dplyr::mutate(
      ne_activity = activity_coef *
        ne_maintenance +
        walking_cost * weight * grazing_distance_km
    )
}

#' NEl: IPCC Eq 10.8/10.9.
#' @noRd
.calc_energy_lactation <- function(data) {
  data |>
    dplyr::mutate(
      ne_lactation = dplyr::case_when(
        is.na(milk_yield_kg_day) |
          milk_yield_kg_day == 0 ~
          0,
        !is.na(protein_percent) &
          protein_percent > 0 &
          !is.na(lactose_percent) &
          lactose_percent > 0 ~
          milk_yield_kg_day *
            (0.389 *
              fat_percent +
              0.229 * protein_percent +
              0.165 * lactose_percent),
        species_gen %in% c("Sheep", "Goats") ~ milk_yield_kg_day * 4.6,
        TRUE ~ milk_yield_kg_day * (1.47 + 0.40 * fat_percent)
      )
    )
}

#' NEwool: IPCC Eq 10.12.
#' @noRd
.calc_energy_wool <- function(data) {
  ev_wool <- livestock_constants$ev_wool_mj_kg
  data |>
    dplyr::mutate(
      ne_wool = dplyr::if_else(
        species_gen == "Sheep",
        dplyr::coalesce(wool_production_kg_yr, 0) /
          365 *
          ev_wool,
        0
      )
    )
}

#' NEwork: IPCC Eq 10.11.
#' @noRd
.calc_energy_work <- function(data) {
  data |>
    dplyr::mutate(ne_work = cw * ne_maintenance * work_hours_day)
}

#' NEp: IPCC Eq 10.13.
#' @noRd
.calc_energy_pregnancy <- function(data) {
  data |>
    dplyr::mutate(ne_pregnancy = cp * ne_maintenance * pregnant_fraction)
}

#' NEg: IPCC Eq 10.6.
#' @noRd
.calc_energy_growth <- function(data) {
  data |>
    dplyr::mutate(
      ne_growth = weight_gain_kg_day * energy_content_gain_mj_kg
    )
}

#' GE from NE components: IPCC Eq 10.16.
#' @noRd
.estimate_gross_energy <- function(data) {
  if (!rlang::has_name(data, "de_percent")) {
    data <- data |>
      dplyr::mutate(
        de_percent = livestock_constants$default_de_percent
      )
  }

  data |>
    dplyr::mutate(
      rem = .calc_rem(de_percent),
      reg = .calc_reg(de_percent),
      gross_energy = (ne_total_maintenance / rem + ne_total_growth / reg) /
        (de_percent / 100)
    )
}

# Private helpers ----

#' Map species string to general category.
#' @noRd
.get_general_species <- function(s) {
  dplyr::case_when(
    stringr::str_detect(s, "(?i)Cattle") ~ "Cattle",
    stringr::str_detect(s, "(?i)Buffalo") ~ "Buffalo",
    stringr::str_detect(s, "(?i)Sheep") ~ "Sheep",
    stringr::str_detect(s, "(?i)Goat") ~ "Goats",
    stringr::str_detect(s, "(?i)Pig|Swine|Hog") ~ "Swine",
    stringr::str_detect(
      s,
      "(?i)Poultry|Chicken|Hen|Duck|Geese|Goose|Turkey"
    ) ~
      "Poultry",
    stringr::str_detect(s, "(?i)Horse") ~ "Horses",
    stringr::str_detect(s, "(?i)Camel") ~ "Camels",
    stringr::str_detect(s, "(?i)Mule|Ass|Donkey") ~ "Mules and Asses",
    TRUE ~ s
  )
}

#' Determine subcategory (Dairy vs Non-Dairy).
#' @noRd
.get_subcategory <- function(s) {
  dplyr::case_when(
    stringr::str_detect(s, "(?i)Dairy") ~ "Dairy",
    stringr::str_detect(s, "(?i)Cattle|Buffalo") ~ "Non-Dairy",
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
    dplyr::summarise(
      weight_kg_global = mean(weight_kg, na.rm = TRUE),
      .by = c(species, cohort)
    )

  if (rlang::has_name(data, "iso3")) {
    data <- data |>
      dplyr::left_join(
        gleam_geographic_hierarchy |>
          dplyr::select(iso3, gleam_region),
        by = "iso3"
      ) |>
      dplyr::left_join(
        gleam_animal_weights |>
          dplyr::summarise(
            weight_kg = mean(weight_kg, na.rm = TRUE),
            .by = c(region, species, cohort)
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
          weight,
          weight_kg,
          weight_kg_global
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
    "milk_yield_kg_day",
    "fat_percent",
    "protein_percent",
    "lactose_percent",
    "weight_gain_kg_day",
    "work_hours_day",
    "pregnant_fraction",
    "wool_production_kg_yr"
  )
  missing <- setdiff(optional, names(data))
  data[missing] <- NA_real_
  data
}

#' Join production defaults and fill missing values.
#' @noRd
.join_production_defaults <- function(data) {
  defaults <- livestock_production_defaults |>
    dplyr::rename(species_match = category)

  # Map species to defaults key (e.g. "Beef Cattle" -> "Other Cattle")
  data <- data |>
    dplyr::mutate(
      defaults_key = dplyr::case_when(
        stringr::str_detect(species, "(?i)Dairy") &
          species_gen == "Cattle" ~
          "Dairy Cattle",
        species_gen == "Cattle" ~ "Other Cattle",
        TRUE ~ species_gen
      )
    )

  data |>
    dplyr::left_join(
      defaults,
      by = c("defaults_key" = "species_match"),
      suffix = c("", "_default")
    ) |>
    dplyr::select(-defaults_key) |>
    dplyr::mutate(
      milk_yield_kg_day = dplyr::coalesce(
        milk_yield_kg_day,
        0
      ),
      fat_percent = dplyr::coalesce(
        fat_percent,
        fat_percent_default
      ),
      protein_percent = dplyr::coalesce(
        protein_percent,
        protein_percent_default
      ),
      lactose_percent = dplyr::coalesce(
        lactose_percent,
        lactose_percent_default
      ),
      weight_gain_kg_day = dplyr::coalesce(
        weight_gain_kg_day,
        weight_gain_kg_day_default,
        0
      ),
      work_hours_day = dplyr::coalesce(
        work_hours_day,
        work_hours_day_default,
        0
      ),
      pregnant_fraction = dplyr::coalesce(
        pregnant_fraction,
        pregnant_fraction_default,
        0
      )
    ) |>
    dplyr::select(-dplyr::ends_with("_default"))
}

#' Set activity coefficient based on production system.
#' @noRd
.set_activity_coef <- function(data) {
  if (!rlang::has_name(data, "system")) {
    data <- data |> dplyr::mutate(activity_coef = ca_pasture)
  } else {
    data <- data |>
      dplyr::mutate(
        activity_coef = dplyr::if_else(
          system == "Feedlot",
          ca_feedlot,
          ca_pasture
        )
      )
  }
  if (!rlang::has_name(data, "grazing_distance_km")) {
    data <- data |> dplyr::mutate(grazing_distance_km = 0)
  }
  data
}

#' Join feed characteristics by diet quality.
#' @noRd
.join_feed_characteristics <- function(data) {
  if (!rlang::has_name(data, "diet_quality")) {
    cli::cli_warn(
      "{.arg diet_quality} not provided. Using default DE%."
    )
    data <- data |>
      dplyr::mutate(
        diet_quality = NA_character_,
        method_energy = paste0(
          method_energy,
          "; diet_quality_MISSING"
        )
      )
  }
  has_de <- rlang::has_name(data, "de_percent")

  data <- data |>
    dplyr::left_join(
      feed_characteristics |>
        dplyr::select(
          diet_quality,
          de_percent,
          ndf_percent
        ),
      by = "diet_quality",
      suffix = c("", "_feed")
    )

  if (has_de) {
    data <- data |>
      dplyr::mutate(
        de_percent = dplyr::coalesce(
          de_percent,
          de_percent_feed
        )
      ) |>
      dplyr::select(-de_percent_feed)
  }

  data
}

#' Join temperature adjustment factors.
#' @noRd
.join_temperature_adjustment <- function(data) {
  if (!rlang::has_name(data, "temperature_c")) {
    data <- data |>
      dplyr::mutate(
        temperature_c = 15,
        method_energy = paste0(
          method_energy,
          "; temp_assumed_15C"
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
    dplyr::rename(temp_adjustment = adjustment_factor) |>
    dplyr::select(-temp_min, -temp_max)
}

#' REM: ratio NE-maintenance to DE consumed.
#' IPCC 2006 Eq 10.14.
#' @noRd
.calc_rem <- function(de_pct) {
  rem <- 1.123 - (4.092e-3 * de_pct) + (1.126e-5 * de_pct^2) - (25.4 / de_pct)
  pmax(0.3, pmin(0.8, rem))
}

#' REG: ratio NE-growth to DE consumed.
#' IPCC 2006 Eq 10.15.
#' @noRd
.calc_reg <- function(de_pct) {
  reg <- 1.164 - (5.160e-3 * de_pct) + (1.308e-5 * de_pct^2) - (37.4 / de_pct)
  pmax(0.2, pmin(0.6, reg))
}
