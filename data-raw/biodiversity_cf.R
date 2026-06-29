# Build the `biodiversity_cf` package dataset: country-level land-use
# biodiversity characterization factors (CFs) for the biodiversity footprint
# extension (see `build_biodiversity_extension()`).
#
# Source: Chaudhary, A. & Brooks, T. M. (2018). "Land Use Intensity-Specific
# Global Characterization Factors to Assess Product Biodiversity Footprints."
# Environmental Science & Technology, 52(9), 5094-5104.
# DOI: 10.1021/acs.est.7b05570. Supporting Information, Table S5
# ("Country aggregated global land occupation characterization factors").
#
# We use the *taxa-aggregated*, *global*, *average-approach* occupation CFs in
# units of Potentially Disappeared Fraction of species (PDF) per m2 of land
# occupied, with the published 95% confidence interval. Only the agricultural
# land-use classes are retained: crop and pasture, each at three land-use
# intensities (minimal / light / intense).
#
# `data-raw/chaudhary_occupation_cf_raw.csv` is a verbatim extract of the
# crop/pasture columns of Table S5 (factual data; full attribution above).

library(dplyr)
library(readr)
library(stringr)

raw <- read_csv(
  here::here("data-raw/chaudhary_occupation_cf_raw.csv"),
  show_col_types = FALSE
)

# Map Chaudhary country names to ISO3, so the dataset joins to WHEP reporting
# areas via the standard area_code -> area_iso3c lookup at runtime.
country_names <- raw |>
  distinct(chaudhary_country) |>
  mutate(
    clean_name = chaudhary_country |>
      str_replace_all("_", " ") |>
      str_to_title(),
    iso3c = countrycode::countrycode(
      clean_name,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Byelarus" = "BLR",
        "Netherlands Antilles" = "ANT",
        "Virgin Islands" = "VIR"
      ),
      warn = FALSE
    )
  )

unmatched <- country_names |>
  filter(is.na(iso3c)) |>
  pull(chaudhary_country)
if (length(unmatched) > 0) {
  message(
    "Dropping ",
    length(unmatched),
    " unmappable areas (tiny territories): ",
    paste(unmatched, collapse = ", ")
  )
}

# A few Chaudhary territories share one ISO3 (Gaza Strip + West Bank -> PSE);
# collapse them to a single CF per (iso3c, land_use_type, intensity) by mean so
# the runtime join key stays unique.
biodiversity_cf <- raw |>
  left_join(
    select(country_names, chaudhary_country, iso3c),
    by = "chaudhary_country"
  ) |>
  filter(!is.na(iso3c)) |>
  summarise(
    cf_mean = mean(cf_mean),
    cf_lo = mean(cf_lo),
    cf_hi = mean(cf_hi),
    .by = c(iso3c, land_use_type, intensity)
  ) |>
  arrange(iso3c, land_use_type, intensity) |>
  tibble::as_tibble()

usethis::use_data(biodiversity_cf, overwrite = TRUE)
