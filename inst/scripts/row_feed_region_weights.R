# Re-derive the Rest-of-World Bouwman feed-region weights (whep#467).
#
# `.row_member_herds()` in R/feed_intake_build.R hardcodes the livestock each
# member of reporting bucket 999 carries, because deriving it needs a full
# `get_primary_production()` run and the bucket's membership only changes when
# the polities vintage is re-synced. This script re-derives it, so the table can
# be checked and refreshed rather than trusted.
#
# It reads pins and takes several minutes. Run it manually:
#   Rscript --vanilla inst/scripts/row_feed_region_weights.R

options(whep.unfold_rest_of_world = TRUE, whep.warn_polity_folds = FALSE)
devtools::load_all(quiet = TRUE)

# Unfolding promotes each Rest-of-World member to its own polity_area_code, so
# the members' herds stop being summed into a single bucket row.
production <- suppressWarnings(get_primary_production())

members <- whep::polity_area_crosswalk |>
  tibble::as_tibble() |>
  dplyr::filter(
    !is.na(.data$area_code),
    .data$polity_area_code == 999L,
    .data$area_code != 999L
  ) |>
  dplyr::distinct(
    member_area_code = as.integer(.data$area_code),
    member_area_name = .data$area_name,
    region_bouwman = .data$region
  )

# Livestock units, not feed demand: the IPCC Tier-2 demand model is itself
# region-dependent, so weighting by demand would make the weights circular.
herds <- production |>
  dplyr::filter(.data$unit == "LU", !is.na(.data$value)) |>
  dplyr::inner_join(
    members,
    by = c(area_code = "member_area_code"),
    keep = TRUE
  ) |>
  dplyr::summarise(
    livestock_units = round(sum(.data$value)),
    .by = c("member_area_code", "member_area_name", "region_bouwman")
  ) |>
  dplyr::arrange(dplyr::desc(.data$livestock_units))

print(as.data.frame(herds), digits = 10)

weights <- herds |>
  dplyr::filter(!is.na(.data$region_bouwman)) |>
  dplyr::summarise(
    livestock_units = sum(.data$livestock_units),
    .by = "region_bouwman"
  ) |>
  dplyr::mutate(
    region_weight = .data$livestock_units / sum(.data$livestock_units)
  ) |>
  dplyr::arrange(dplyr::desc(.data$region_weight))

print(as.data.frame(weights), digits = 8)
