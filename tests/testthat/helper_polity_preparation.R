.polity_preparation_data <- function() {
  list(
    production = tibble::tribble(
      ~source, ~source_area_code, ~year, ~item_prod_code,
      ~harvested_area_ha, ~territorial_basis,
      "fixture", "region", 1900L, 15L, 30, "source_reported",
      "fixture", "region", 1900L, 56L, 70, "source_reported"
    ),
    mapping = tibble::tribble(
      ~source, ~source_area_code, ~start_year, ~end_year, ~polity_code,
      "fixture", "region", 1800L, 1950L, "AAA-1800-1950"
    ),
    support = tibble::tribble(
      ~lon, ~lat, ~polity_code, ~start_year, ~end_year,
      ~cell_area_ha, ~land_area_ha,
      0.25, 0.25, "AAA-1800-1950", 1800L, 1950L, 100, 100
    ),
    luh2 = tibble::tribble(
      ~year, ~lon, ~lat, ~luh2_type, ~crop_ha, ~irrig_ha,
      1900L, 0.25, 0.25, "c3ann", 100, 20
    ),
    type_mapping = tibble::tribble(
      ~item_prod_code, ~luh2_type,
      15L, "c3ann",
      56L, "c3ann"
    ),
    crop_patterns = tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
      0.25, 0.25, 15L, 0.3,
      0.25, 0.25, 56L, 0.7
    ),
    mirca = tibble::tribble(
      ~lon, ~lat, ~item_prod_code, ~irrig_ha, ~rainfed_ha,
      0.25, 0.25, 15L, 50, 50,
      0.25, 0.25, 56L, 10, 90
    ),
    input_revisions = list(
      production = "fixture-production",
      mapping = "fixture-mapping",
      support = "fixture-support",
      luh2 = "fixture-luh2",
      crop_patterns = "fixture-patterns",
      mirca = "fixture-mirca"
    )
  )
}
