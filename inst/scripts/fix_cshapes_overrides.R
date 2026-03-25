## Fix specific common_names.csv overrides where the auto-generated mapping
## creates duplicate polity codes or redundant polities.
## Run AFTER run_cshapes_integration.R.

library(dplyr)
library(readr)
library(here)

polities_inputs_path <- here("data-raw", "polities_inputs")
read_polity_file <- function(f) read_csv(file.path(polities_inputs_path, f),
                                          show_col_types = FALSE)

common_names  <- read_polity_file("common_names.csv")
polity_codes  <- read_polity_file("polity_codes.csv")

# Overrides: original_name (CShapes country_name + year suffix),source -> correct common_name
# These cases result in duplicate polity_codes or redundant geometry-only polities.
overrides <- tibble::tribble(
  ~original_name,                                              ~source,    ~common_name,
  # GDR / FRG: CShapes extends back to 1945 but WHEP polities start 1949
  "German Democratic Republic (1945-1990)",                    "cshapes",  "East Germany",
  "German Federal Republic (1945-1990)",                       "cshapes",  "West Germany",
  "German Federal Republic",                                   "cshapes",  "Germany",
  # Russia: CShapes keeps calling it "Russia (Soviet Union)" after 1991
  "Russia (Soviet Union) (1991-2014)",                         "cshapes",  "Russia (1991-2014)",
  "Russia (Soviet Union)",                                     "cshapes",  "Russia",
  # North/South Vietnam: CShapes has year suffix; WHEP polities don't
  "Vietnam, Democratic Republic of (1954-1975)",               "cshapes",  "North Vietnam",
  "Vietnam, Republic of (1954-1975)",                          "cshapes",  "South Vietnam",
  # Yemen: CShapes keeps the "Arab Republic" name through unification
  "Yemen (Arab Republic of Yemen) (1918-1990)",                "cshapes",  "North Yemen",
  "Yemen (Arab Republic of Yemen) (1990-2000)",                "cshapes",  "Yemen (1990-2000)",
  "Yemen, People's Republic of (1967-1990)",                   "cshapes",  "South Yemen",
)

cat("Applying", nrow(overrides), "overrides to common_names.csv...\n")

# Apply: replace common_name for specific (original_name, source) pairs
updated_common <- common_names |>
  dplyr::rows_update(
    overrides,
    by = c("original_name", "source"),
    unmatched = "error"
  )

# Write back
write_csv(updated_common, file.path(polities_inputs_path, "common_names.csv"))
cat("common_names.csv updated.\n")

# Show what changed
cat("\nOverride details:\n")
for (i in seq_len(nrow(overrides))) {
  old <- common_names |>
    filter(original_name == overrides$original_name[i], source == overrides$source[i]) |>
    pull(common_name)
  cat(sprintf("  %s\n    OLD: %s\n    NEW: %s\n",
              overrides$original_name[i], old, overrides$common_name[i]))
}

# Now regenerate polity_codes.csv: remove the new polity_codes entries that
# are now superseded (since they map to existing polities via override)
# and fix any remaining duplicates.

# Re-run the polity_codes step: which new CShapes common_names have no existing polity?
cshapes_common <- updated_common |>
  filter(source == "cshapes") |>
  distinct(common_name)

missing_from_codes <- cshapes_common |>
  filter(!common_name %in% polity_codes$polity_name)

cat("\nCShapes common_names with no polity_code:", nrow(missing_from_codes), "\n")

# Check for any remaining duplicates in polity_codes
dups <- polity_codes |>
  group_by(polity_code) |>
  filter(n() > 1) |>
  arrange(polity_code)

if (nrow(dups) > 0) {
  cat("\nRemaining duplicate codes in polity_codes.csv:\n")
  print(as.data.frame(dups))

  # Fix: for NEW entries (not in original polity_codes), assign unique codes
  # The GDR/FRG and Vietnam/Yemen duplicates should now be resolved by overrides.
  # But we still need to remove the obsolete entries.

  # The entries that are now mapped to existing polities should NOT have their
  # own entries in polity_codes. Remove them.
  obsolete_names <- overrides$original_name  # These were the auto-generated names
  # Actually, the polity_name in polity_codes comes from common_name.
  # After the override, the OLD auto-generated common_names are gone.
  # But polity_codes was already written with the old entries. We need to remove them.

  # The old auto-generated polity_names (from the first run) that are now obsolete:
  # These are the polity_names (common_names) that were auto-generated but are
  # now overridden to map to existing polities instead.
  old_auto_names <- c(
    # GDR/FRG periods - now map to existing East/West Germany polities
    "German Democratic Republic (1945-1990)",
    "Federal Republic of Germany (1945-1990)",
    "Federal Republic of Germany",
    "East Germany (1949-1990)",
    "West Germany (1949-1990)",
    # Russia post-1991 - now maps to existing Russia polities
    "USSR (1991-2014)",
    "USSR",
    # Vietnam split - now maps to existing North/South Vietnam polities
    "North Vietnam (1954-1975)",
    "South Vietnam (1954-1975)",
    # Yemen - now maps to existing polities
    "North Yemen (1918-1990)",
    "North Yemen (1990-2000)",
    "South Yemen (1967-1990)"
  )

  cleaned_codes <- polity_codes |>
    filter(!polity_name %in% old_auto_names)

  write_csv(cleaned_codes, file.path(polities_inputs_path, "polity_codes.csv"))
  cat("\nRemoved", nrow(polity_codes) - nrow(cleaned_codes), "obsolete entries from polity_codes.csv\n")
  cat("New total:", nrow(cleaned_codes), "polities\n")
} else {
  cat("\nNo duplicate codes remaining.\n")
}
