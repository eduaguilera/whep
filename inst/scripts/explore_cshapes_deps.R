## Exploration: what does cshapes::cshp(dependencies = TRUE) return?
## Run this ONCE to understand the data before integrating.
## Output: data-raw/polities_inputs/cshapes_dependencies_inventory.csv

library(cshapes)
library(dplyr)
library(sf)
library(lubridate)
library(readr)
library(here)

polities_inputs_path <- here("data-raw", "polities_inputs")

cat("Loading CShapes (with dependencies)...\n")
cs_all <- cshp(dependencies = TRUE) |> st_make_valid()

cat("\n--- Column names ---\n")
print(names(cs_all))

cat("\n--- Row counts ---\n")
cat("With dependencies:", nrow(cs_all), "\n")

# All dependent territory rows (status != "independent")
deps <- cs_all |> filter(status != "independent")

cat("\n--- Dependency status counts ---\n")
print(table(deps$status))

cat("\n--- Unique dependency country_names (not independent) ---\n")
dep_unique <- deps |>
  st_drop_geometry() |>
  mutate(
    start_year = year(start),
    end_year   = year(end)
  ) |>
  distinct(country_name) |>
  arrange(country_name)

cat("Unique dependency names:", nrow(dep_unique), "\n\n")
for (nm in dep_unique$country_name) cat(" -", nm, "\n")

cat("\n--- Pre-1886 entries (all statuses) ---\n")
pre_1886 <- cs_all |>
  st_drop_geometry() |>
  filter(year(start) < 1886) |>
  mutate(start_year = year(start), end_year = year(end)) |>
  distinct(country_name, .keep_all = TRUE) |>
  select(country_name, start_year, end_year, status) |>
  arrange(country_name)
cat("Entries starting before 1886:", nrow(pre_1886), "\n")
for (i in seq_len(nrow(pre_1886))) {
  cat(sprintf("  %s [%s-%s] (%s)\n",
              pre_1886$country_name[i],
              pre_1886$start_year[i],
              pre_1886$end_year[i],
              pre_1886$status[i]))
}

# Save full dependency inventory
dep_inventory <- deps |>
  st_drop_geometry() |>
  mutate(
    start_year = year(start),
    end_year   = year(end)
  ) |>
  select(country_name, start_year, end_year, status, owner, gwcode) |>
  arrange(country_name, start_year)

write_csv(dep_inventory,
          file.path(polities_inputs_path, "cshapes_dependencies_inventory.csv"))
cat("\nSaved dependency inventory:", nrow(dep_inventory), "rows,",
    n_distinct(dep_inventory$country_name), "unique names\n")
