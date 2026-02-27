# Helper fixtures for livestock emission tests ---------------------------------

#' Minimal Tier 2 dairy cattle input.
dairy_tier2_fixture <- function() {
  tibble::tibble(
    species = "Dairy Cattle",
    cohort = "Adult Female",
    weight = 600,
    milk_yield_kg_day = 20,
    fat_percent = 4.0,
    diet_quality = "High",
    heads = 100
  )
}

#' Minimal Tier 2 beef cattle input.
beef_tier2_fixture <- function() {
  tibble::tibble(
    species = "Beef Cattle",
    cohort = "Adult Male",
    weight = 500,
    weight_gain_kg_day = 0.5,
    diet_quality = "Medium",
    heads = 200
  )
}

#' Minimal Tier 1 input for multiple species.
tier1_fixture <- function() {
  tibble::tibble(
    species = c(
      "Dairy Cattle", "Beef Cattle", "Sheep",
      "Swine", "Horses"
    ),
    heads = c(1000, 2000, 5000, 10000, 200)
  )
}

#' Single-species Tier 1 input.
single_tier1_fixture <- function(species = "Dairy Cattle",
                                 heads = 1000) {
  tibble::tibble(species = species, heads = heads)
}
