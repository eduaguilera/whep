# diagnose_item_coverage.R
# ─────────────────────────────────────────────────────────────
# Diagnostic script to check which expected item codes are
# missing from the WHEP primary production output and trace
# whether they exist in the raw FAOSTAT inputs.
#
# Requires: the whep package loaded (devtools::load_all("."))
# ─────────────────────────────────────────────────────────────

library(dplyr)
library(cli)

# == 1. Build production and check item coverage ==============================

cli::cli_h1("Building primary production")
result <- whep::build_primary_production()

# -- Expected items -----------------------------------------------------------

expected_crops <- whep::items_prod_full |>
  dplyr::filter(group %in% c("Primary crops", "Livestock products")) |>
  dplyr::pull(item_prod_code) |>
  unique()

expected_animals <- unique(as.character(whep::animals_codes$item_cbs_code))
expected_all <- unique(c(expected_crops, expected_animals))

output_items <- unique(as.character(result$item_prod_code))
missing <- setdiff(expected_all, output_items)

# -- Name lookup --------------------------------------------------------------

items_lookup <- whep::items_prod_full |>
  dplyr::select(item_prod_code, item_prod) |>
  dplyr::distinct()
animals_lookup <- whep::animals_codes |>
  dplyr::select(item_prod_code = item_cbs_code, item_prod = item_cbs) |>
  dplyr::mutate(item_prod_code = as.character(item_prod_code)) |>
  dplyr::distinct()
lookup <- dplyr::bind_rows(items_lookup, animals_lookup) |>
  dplyr::distinct(item_prod_code, .keep_all = TRUE)

item_name <- function(code) {
  out <- lookup$item_prod[match(as.character(code), lookup$item_prod_code)]
  dplyr::if_else(is.na(out), paste0("code_", code), out)
}

# -- Report missing items -----------------------------------------------------

cli::cli_h1("Item coverage check")
cli::cli_text(
  "Expected items: {length(expected_all)}   Output items: {length(output_items)}   Missing: {length(missing)}"
)

if (length(missing) > 0L) {
  missing_labels <- paste0(item_name(missing), " (", missing, ")")

  cli::cli_alert_warning(
    "{length(missing)} expected item code{?s} absent from output:"
  )
  for (nm in missing_labels) {
    cli::cli_bullets(c("!" = nm))
  }
}

# == 2. Trace missing items in FAOSTAT raw inputs =============================

if (length(missing) > 0L) {
  cli::cli_h1("Tracing missing items in FAOSTAT inputs")

  fao_raw <- whep:::.read_input(
    "faostat-production",
    years = 1961:2021,
    year_col = "Year"
  )
  fao_item_codes <- unique(as.integer(fao_raw[["Item Code"]]))

  for (code in missing) {
    in_fao <- as.integer(code) %in% fao_item_codes
    name <- item_name(code)
    if (in_fao) {
      rows <- fao_raw[fao_raw[["Item Code"]] == as.integer(code), ]
      n_rows <- nrow(rows)
      n_areas <- length(unique(rows[["Area Code"]]))
      n_years <- length(unique(rows[["Year"]]))
      cli::cli_alert_warning(
        "{name} ({code}): PRESENT in faostat-production ({n_rows} rows, {n_areas} areas, {n_years} years) -- dropped during pipeline"
      )
    } else {
      cli::cli_alert_info(
        "{name} ({code}): NOT in faostat-production input"
      )
    }
  }
}

cli::cli_alert_success("Diagnostic complete")
