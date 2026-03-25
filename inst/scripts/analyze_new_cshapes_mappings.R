## Analyze what rename_cshapes.csv entries are needed for the new dependency data.
## For base polity_names already in rename_cshapes.csv, integrate_cshapes.R will
## auto-generate common_names and polity_codes for all new date-range periods.
## This script identifies only the TRULY MISSING base names.

library(dplyr)
library(readr)
library(here)

polities_inputs_path <- here("data-raw", "polities_inputs")
read_csv2 <- function(f) read_csv(file.path(polities_inputs_path, f),
                                   show_col_types = FALSE)

rename_cs <- read_csv2("rename_cshapes.csv")
polity_codes <- read_csv2("polity_codes.csv")
new_entries <- read_csv2("cshapes_new_entries.csv")

# Extract base polity_name from new entries (i.e., the CShapes country_name
# that appears in k_cshapes$polity_name - before .add_years_in_name() is applied)
# The polity_name column in cshapes_new_entries.csv IS the base name.
new_base_names <- new_entries |> distinct(polity_name) |> arrange(polity_name)

cat("Unique base polity_names with new entries:", nrow(new_base_names), "\n\n")

# Which ones are NOT in rename_cshapes.csv?
missing_in_rename <- new_base_names |>
  filter(!polity_name %in% rename_cs$country_name)

cat("Base names MISSING from rename_cshapes.csv (need new entries):\n")
for (nm in missing_in_rename$polity_name) cat(" -", nm, "\n")
cat("\nCount:", nrow(missing_in_rename), "\n\n")

# Which are already in rename_cshapes.csv?
already_mapped <- new_base_names |>
  filter(polity_name %in% rename_cs$country_name) |>
  left_join(rename_cs, by = c("polity_name" = "country_name"))

cat("Base names ALREADY in rename_cshapes.csv (date-range entries auto-generated):\n")
for (i in seq_len(nrow(already_mapped))) {
  cat(sprintf("  %s -> %s\n", already_mapped$polity_name[i],
              already_mapped$common_name[i]))
}
