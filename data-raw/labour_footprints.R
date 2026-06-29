# Prepare the labour-footprint coefficient pin consumed by
# build_labour_extension(). None of the coefficient values are hardcoded in the
# package; they live in a pinned table registered in inst/extdata/whep_inputs.csv
# and read at runtime via whep_read_file().
#
# One pin is needed (the alias must match build_labour_extension()):
#
#   1. gld-labour-crop   crop working hours + harvested area, by crop x
#                        country x year                              [NOT YET]
#
# Source (citation to be confirmed before upload -- see note below):
#   - Global Labour Database, compiled by Juan Infante-Amate and colleagues:
#     agricultural working hours per crop and country, disaggregated into a
#     total and several social tiers (extreme-poverty, lower- and
#     upper-middle-income-country poverty, not-covered, child and forced
#     labour). The synthesis file is `labour_synthesis_items.csv`, used in the
#     `~/Edu/Global` footprint pipeline (R/footprints.r, "Labour and land
#     footprint of products"). It is NOT yet in a public repository: obtain it
#     from the authors. The full bibliographic citation / DOI must be verified
#     with the authors before the pin is uploaded -- do not invent one.
#
# Uploading is manual (saco.csic.es board access via ~/whep_inputs). After
# uploading, put the printed version into the matching whep_inputs.csv row and
# re-run data-raw/whep_inputs.R to rebuild the package data.

library(readr)
library(dplyr)

# Crop working hours (gld-labour-crop) -----------------------------------------
# The synthesis file `labour_synthesis_items.csv` carries, per (year, country,
# crop): the harvested area worked (`area_harvested`, ha) and the working hours
# for each tier (`hours_total`, `hours_extreme_poverty`, `hours_lmic_poverty`,
# `hours_umic_poverty`, `hours_not_covered`, `hours_child_labor`,
# `hours_forced_labor`). build_labour_extension() divides each tier's hours by
# the harvested area to get hours ha^-1, so the pin keeps both the hours and the
# area, not a pre-computed intensity.
#
# build_labour_extension() expects FAO numeric keys, so map the source crop and
# country to FAO codes: `item_primary_code` is already the FAO item (crop) code,
# and the source country is mapped to the FAO area code via whep's region
# lookup. The synthesis file's country column is assumed to be ISO3 here
# (`country_iso3`); if the delivered file uses country names or another code
# instead, change the join key accordingly. Rename the child/forced columns to
# the short tier names the builder uses.
labour_src_csv <- file.path(
  Sys.getenv("WHEP_LABOUR_INPUT_DIR", "."),
  "labour_synthesis_items.csv"
)

regions <- whep::regions |>
  dplyr::distinct(country_iso3 = iso3c, country_code = area_code)

gld_labour_crop <- read_csv(labour_src_csv, show_col_types = FALSE) |>
  dplyr::left_join(regions, by = "country_iso3") |>
  transmute(
    crop_code = as.integer(item_primary_code),
    crop_name = item_primary,
    country_code = as.integer(country_code),
    year = as.integer(year),
    area_harvested_ha = as.numeric(area_harvested),
    hours_total = as.numeric(hours_total),
    hours_extreme_poverty = as.numeric(hours_extreme_poverty),
    hours_lmic_poverty = as.numeric(hours_lmic_poverty),
    hours_umic_poverty = as.numeric(hours_umic_poverty),
    hours_not_covered = as.numeric(hours_not_covered),
    hours_child = as.numeric(hours_child_labor),
    hours_forced = as.numeric(hours_forced_labor)
  ) |>
  filter(
    !is.na(crop_code),
    !is.na(country_code),
    !is.na(year),
    !is.na(area_harvested_ha),
    area_harvested_ha > 0
  )
# upload_csv(<written gld_labour_crop>, "gld-labour-crop") via ~/whep_inputs
