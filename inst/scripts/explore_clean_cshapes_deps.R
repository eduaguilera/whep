## Run .clean_cshapes() with dependencies=TRUE and see what comes out.
## Self-contained: manually sources the needed functions.
## Shows exactly what original_name values .prepare_cshapes() will produce.

library(cshapes)
library(dplyr)
library(sf)
library(lubridate)
library(readr)
library(units)

polities_inputs_path <- here::here("data-raw", "polities_inputs")

# ---- Copy the pipeline functions we need ----

.add_years_in_name <- function(name, start_year, end_year) {
  dplyr::case_when(
    is.na(end_year) ~ as.character(name),
    is.na(start_year) ~ paste0(name, " (to ", end_year, ")"),
    .default = paste0(name, " (", start_year, "-", end_year, ")")
  )
}

.filter_relevant_area_changes <- function(countries) {
  zero_km2 <- units::set_units(0, km^2)
  countries |>
    dplyr::arrange(country_name, start_year) |>
    dplyr::group_by(country_name) |>
    dplyr::filter(dplyr::n() == 1 | start_year != end_year) |>
    dplyr::mutate(group_tmp = cumsum(c(0, diff(area) != zero_km2))) |>
    dplyr::ungroup() |>
    dplyr::summarise(
      geometry = dplyr::first(geometry),
      area = dplyr::first(area),
      start_year = min(start_year),
      end_year = max(end_year),
      .by = c("country_name", "group_tmp")
    ) |>
    dplyr::select(-group_tmp)
}

.clean_cshapes <- function() {
  cat("Loading CShapes (dependencies = TRUE)...\n")
  countries <- cshapes::cshp(dependencies = TRUE) |> sf::st_make_valid()
  countries |>
    dplyr::mutate(
      area = countries |> sf::st_area() |> units::set_units(km^2),
      start_year = lubridate::year(start),
      end_year   = lubridate::year(end)
    ) |>
    .filter_relevant_area_changes() |>
    dplyr::mutate(
      start_year = ifelse(start_year <= 1886, NA, start_year),
      end_year   = ifelse(end_year >= 2019, NA, end_year)
    ) |>
    dplyr::select(polity_name = country_name, start_year, end_year, area) |>
    dplyr::arrange(polity_name)
}

# ---- Run it ----
cs_clean <- .clean_cshapes()
cat("Rows from .clean_cshapes():", nrow(cs_clean), "\n\n")

# Generate original_names (what .prepare_cshapes() will produce)
cs_prepared <- cs_clean |>
  mutate(original_name = .add_years_in_name(polity_name, start_year, end_year))

cat("All original_names:\n")
for (nm in sort(cs_prepared$original_name)) cat(" ", nm, "\n")

# Compare to existing common_names for cshapes source
existing_common <- read_csv(file.path(polities_inputs_path, "common_names.csv"),
                            show_col_types = FALSE) |>
  filter(source == "cshapes")

cat("\n\n--- NEW original_names not yet in common_names.csv ---\n")
new_entries <- cs_prepared |>
  filter(!original_name %in% existing_common$original_name)
cat("Count:", nrow(new_entries), "\n")
for (nm in sort(new_entries$original_name)) cat(" ", nm, "\n")

cat("\n--- Entries in common_names.csv that no longer exist in new cshapes output ---\n")
orphans <- existing_common |>
  filter(!original_name %in% cs_prepared$original_name)
cat("Count:", nrow(orphans), "\n")
for (nm in sort(orphans$original_name)) cat(" ", nm, "\n")

# Save new entries for mapping
write_csv(
  new_entries |> select(polity_name, start_year, end_year, area, original_name),
  file.path(polities_inputs_path, "cshapes_new_entries.csv")
)
cat("\nSaved", nrow(new_entries), "new entries to cshapes_new_entries.csv\n")
