## Regenerate common_names.csv and polity_codes.csv for the new CShapes dependency data.
## Self-contained version that doesn't need devtools::load_all().
## Run after rename_cshapes.csv has been updated.

library(cshapes)
library(dplyr)
library(sf)
library(lubridate)
library(readr)
library(units)
library(here)

polities_inputs_path <- here("data-raw", "polities_inputs")
read_polity_file <- function(f) read_csv(file.path(polities_inputs_path, f),
                                          show_col_types = FALSE)

# ---- Reproduce the pipeline functions ----

.add_years_in_name <- function(name, start_year, end_year) {
  dplyr::case_when(
    is.na(end_year) ~ as.character(name),
    is.na(start_year) ~ paste0(name, " (to ", end_year, ")"),
    .default = paste0(name, " (", start_year, "-", end_year, ")")
  )
}

.build_auto_polity_code <- function(name, start_year, end_year) {
  start_year <- ifelse(is.na(start_year), 1800, start_year)
  end_year   <- ifelse(is.na(end_year),   2025, end_year)
  short <- toupper(substr(name, 1, 3))
  paste0(short, "-", start_year, "-", end_year)
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

# ---- Build k_cshapes (with dependencies) ----
cat("Loading CShapes with dependencies...\n")
k_cshapes <- cshapes::cshp(dependencies = TRUE) |>
  sf::st_make_valid() |>
  dplyr::mutate(
    area = sf::st_area(geometry) |> units::set_units(km^2),
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
cat("k_cshapes rows:", nrow(k_cshapes), "\n")

# ---- Load rename + existing mappings ----
cshapes_renames   <- read_polity_file("rename_cshapes.csv")
k_polity_codes    <- read_polity_file("polity_codes.csv")
k_polity_common   <- read_polity_file("common_names.csv")

# ---- Build the new cshapes entries (replicating integrate_cshapes.R) ----
cshapes_mapped <- k_cshapes |>
  tibble::as_tibble() |>
  dplyr::arrange(polity_name, end_year) |>
  dplyr::inner_join(
    cshapes_renames,
    by = dplyr::join_by(polity_name == country_name),
    unmatched = "error"
  ) |>
  dplyr::mutate(
    original_name = .add_years_in_name(polity_name, start_year, end_year),
    source = "cshapes",
    common_name = .add_years_in_name(common_name, start_year, end_year)
  )

cshapes_common_names <- cshapes_mapped |>
  dplyr::select(original_name, source, common_name)

# ---- Overrides: fix cases where auto-naming creates duplicates or redundant polities ----
# These map specific CShapes original_names to the correct existing WHEP common_name.
overrides <- tibble::tribble(
  ~original_name,                                    ~source,    ~common_name,
  # GDR/FRG: CShapes has 1945 start; WHEP polities start 1949
  "German Democratic Republic (1945-1990)",          "cshapes",  "East Germany",
  "German Federal Republic (1945-1990)",             "cshapes",  "West Germany",
  "German Federal Republic",                         "cshapes",  "Germany",
  # Russia: CShapes uses "Russia (Soviet Union)" for all periods including post-1991
  "Russia (Soviet Union) (1991-2014)",               "cshapes",  "Russia (1991-2014)",
  "Russia (Soviet Union)",                           "cshapes",  "Russia",
  # Vietnam: CShapes uses "Democratic Republic of" name for modern Vietnam too
  "Vietnam, Democratic Republic of (1954-1975)",     "cshapes",  "North Vietnam",
  "Vietnam, Democratic Republic of",                 "cshapes",  "Vietnam",
  "Vietnam, Republic of (1954-1975)",                "cshapes",  "South Vietnam",
  # Yemen: CShapes uses "Arab Republic" name through post-unification period
  "Yemen (Arab Republic of Yemen) (1918-1990)",      "cshapes",  "North Yemen",
  "Yemen (Arab Republic of Yemen) (1990-2000)",      "cshapes",  "Yemen (1990-2000)",
  "Yemen (Arab Republic of Yemen)",                  "cshapes",  "Yemen",
  "Yemen, People's Republic of (1967-1990)",         "cshapes",  "South Yemen",
)

cshapes_common_names <- cshapes_common_names |>
  dplyr::rows_update(overrides, by = c("original_name", "source"), unmatched = "ignore")

cat("CShapes entries produced:", nrow(cshapes_common_names), "\n")
cat("Overrides applied:", nrow(overrides), "\n")

# ---- Merge into common_names.csv ----
# Remove OLD cshapes entries from common_names, then add new ones
non_cshapes_common <- k_polity_common |>
  dplyr::filter(source != "cshapes")

new_common_names <- dplyr::bind_rows(non_cshapes_common, cshapes_common_names) |>
  dplyr::arrange(original_name, source)

write_csv(new_common_names, file.path(polities_inputs_path, "common_names.csv"))
cat("Wrote common_names.csv:", nrow(new_common_names), "rows\n")
cat("  CShapes entries:", nrow(cshapes_common_names), "\n")
cat("  Non-CShapes entries:", nrow(non_cshapes_common), "\n")

# ---- Find NEW polities (not yet in polity_codes.csv) ----
# Use the post-override common_names joined back to cshapes_mapped for year info.
new_polity_codes <- cshapes_mapped |>
  dplyr::select(original_name, source, start_year, end_year, polity_name) |>
  dplyr::inner_join(
    cshapes_common_names,
    by = c("original_name", "source")
  ) |>
  dplyr::anti_join(
    k_polity_codes,
    by = dplyr::join_by(common_name == polity_name)
  ) |>
  dplyr::mutate(
    polity_code = .build_auto_polity_code(polity_name, start_year, end_year)
  ) |>
  dplyr::distinct(polity_name = common_name, polity_code)

cat("\nNew polity_codes entries to add:", nrow(new_polity_codes), "\n")
for (i in seq_len(nrow(new_polity_codes))) {
  cat(sprintf("  %s -> %s\n", new_polity_codes$polity_name[i],
              new_polity_codes$polity_code[i]))
}

# ---- Write updated polity_codes.csv ----
updated_polity_codes <- k_polity_codes |>
  dplyr::bind_rows(new_polity_codes) |>
  dplyr::arrange(polity_name, polity_code)

write_csv(updated_polity_codes, file.path(polities_inputs_path, "polity_codes.csv"))
cat("\nWrote polity_codes.csv:", nrow(updated_polity_codes), "rows\n")
cat("  Previous:", nrow(k_polity_codes), "\n")
cat("  New entries added:", nrow(new_polity_codes), "\n")

# ---- Verify: check that all cshapes original_names have a common_names entry ----
check <- cshapes_common_names |>
  dplyr::anti_join(new_common_names, by = c("original_name", "source"))
if (nrow(check) > 0) {
  cat("\nWARNING: Some CShapes entries have no common_names entry!\n")
  print(check)
} else {
  cat("\nAll CShapes entries have common_names.csv entries. OK.\n")
}

# ---- Verify: check that all common_names (cshapes) map to a polity_code ----
missing_codes <- cshapes_common_names |>
  dplyr::anti_join(updated_polity_codes, by = c("common_name" = "polity_name"))
if (nrow(missing_codes) > 0) {
  cat("\nWARNING: Some CShapes common_names have no polity_code!\n")
  print(missing_codes)
} else {
  cat("All CShapes common_names map to a polity_code. OK.\n")
}
