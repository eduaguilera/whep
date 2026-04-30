# test_datasets.R — tests for package data consistency

# -- default_destiny values ----------------------------------------------------

test_that("items_full has correct default_destiny for non-food items", {
  items <- whep::items_full

  fodder <- items |>
    dplyr::filter(item_cbs_code %in% c(2000, 2001, 2002, 2003))
  expect_true(all(fodder$default_destiny == "Feed", na.rm = TRUE))

  ethanol <- items |> dplyr::filter(item_cbs_code == 2659)
  expect_true(all(ethanol$default_destiny == "Other_uses", na.rm = TRUE))

  tobacco <- items |> dplyr::filter(item_cbs_code == 2671)
  expect_true(all(tobacco$default_destiny == "Other_uses", na.rm = TRUE))

  cotton <- items |> dplyr::filter(item_cbs_code == 2661)
  expect_true(all(cotton$default_destiny == "Other_uses", na.rm = TRUE))

  rubber <- items |> dplyr::filter(item_cbs_code == 2672)
  expect_true(all(rubber$default_destiny == "Other_uses", na.rm = TRUE))

  wool <- items |> dplyr::filter(item_cbs_code == 2746)
  expect_true(all(wool$default_destiny == "Other_uses", na.rm = TRUE))
})


# -- polity coverage -----------------------------------------------------------

test_that("polities includes promoted FAOSTAT-reporting countries", {
  pol <- whep::polities
  promoted <- c("BTN", "COM", "MHL", "FSM", "NRU", "SYC", "TON", "TUV")
  for (code in promoted) {
    expect_true(
      code %in% pol$iso3c,
      info = paste(code, "should be a standalone polity")
    )
  }
  expect_gte(nrow(pol), 200L)
})

test_that("regions_full maps promoted countries to own polity_code", {
  reg <- whep::regions_full

  btn <- reg |> dplyr::filter(iso3c == "BTN")
  expect_true(nrow(btn) > 0)
  expect_equal(btn$polity_code[1], "BTN")

  com <- reg |> dplyr::filter(iso3c == "COM")
  expect_true(nrow(com) > 0)
  expect_equal(com$polity_code[1], "COM")
})


# -- source_flags.csv consistency ----------------------------------------------

test_that("source_flags.csv covers all source labels used in code", {
  flags <- readr::read_csv(
    system.file("extdata", "source_flags.csv", package = "whep"),
    show_col_types = FALSE
  )

  required <- c(
    "FAOSTAT_prod",
    "EuropeAgriDB",
    "fill_linear",
    "imputed_yield",
    "imputed_cbs_ratio",
    "LUH2_cropland",
    "LUH2_agriland",
    "LUH2_grassland",
    "FAOSTAT_FBS_New",
    "FAOSTAT_FBS_Old",
    "FAOSTAT_FBS_Old_scaled",
    "FAOSTAT_CBS",
    "FAOSTAT_trade",
    "fishstat_trade"
  )

  for (src in required) {
    expect_true(
      src %in% flags$source,
      info = paste(src, "must be documented in source_flags.csv")
    )
  }
})


# -- livestock coefficient dataset integrity -----------------------------------

# Helper: assert a dataset is a non-empty tibble with the
# expected columns, all of the correct type, and no generic
# column names from bad Excel parsing.
assert_clean_tibble <- function(obj, name, expected_cols, min_rows = 1L) {
  expect_true(
    tibble::is_tibble(obj),
    info = paste(name, "must be a tibble")
  )
  expect_gte(
    nrow(obj),
    min_rows,
    label = paste(name, "row count")
  )
  for (col in expected_cols) {
    expect_true(
      col %in% names(obj),
      info = paste(name, "missing column:", col)
    )
  }
  # No generic Excel-parsed column names
  bad <- grep(
    "^x\\d+$|^\\.\\.\\.\\d+$|^V\\d+$",
    names(obj)
  )
  expect_true(
    length(bad) == 0L,
    info = paste(
      name,
      "has generic column names:",
      paste(names(obj)[bad], collapse = ", ")
    )
  )
  # No list columns (Excel parsing artifact)
  list_cols <- names(obj)[vapply(obj, is.list, logical(1))]
  expect_true(
    length(list_cols) == 0L,
    info = paste(name, "has list columns:", paste(list_cols, collapse = ", "))
  )
  # No all-NA columns (sign of failed parsing)
  all_na <- names(obj)[
    vapply(obj, function(x) all(is.na(x)), logical(1))
  ]
  expect_true(
    length(all_na) == 0L,
    info = paste(name, "has all-NA columns:", paste(all_na, collapse = ", "))
  )
  # No character columns that are secretly numeric
  char_cols <- names(obj)[vapply(obj, is.character, logical(1))]
  for (cc in char_cols) {
    vals <- obj[[cc]][!is.na(obj[[cc]])]
    if (length(vals) == 0L) {
      next
    }
    numeric_share <- mean(grepl(
      "^-?[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?$",
      vals
    ))
    expect_true(
      numeric_share < 0.5,
      info = paste0(
        name,
        "$",
        cc,
        " is character but ",
        round(numeric_share * 100),
        "% of values look numeric"
      )
    )
  }
}

# Helper: assert numeric columns contain only numeric values
# (not character strings that look numeric).
assert_numeric_cols <- function(obj, name, cols) {
  for (col in cols) {
    expect_true(
      is.numeric(obj[[col]]),
      info = paste(name, "column", col, "must be numeric")
    )
  }
}


test_that("gleam_field_operation_ef is a clean tibble", {
  obj <- whep::gleam_field_operation_ef
  assert_clean_tibble(
    obj,
    "gleam_field_operation_ef",
    c(
      "material_number",
      "material",
      "emission_factor_kg_co2eq_ha",
      "species_group"
    ),
    min_rows = 50L
  )
  expect_equal(ncol(obj), 4L)
  assert_numeric_cols(
    obj,
    "gleam_field_operation_ef",
    c("material_number", "emission_factor_kg_co2eq_ha")
  )
  expect_setequal(
    unique(obj$species_group),
    c("ruminant", "monogastric")
  )
  # No duplicate material within a species group
  dupes <- obj |>
    dplyr::count(material_number, species_group) |>
    dplyr::filter(n > 1L)
  expect_equal(nrow(dupes), 0L, info = "duplicate materials")
  # Emission factors must be non-negative
  expect_true(
    all(obj$emission_factor_kg_co2eq_ha >= 0, na.rm = TRUE)
  )
})

test_that("gleam_mechanization_levels is a clean tibble", {
  obj <- whep::gleam_mechanization_levels
  assert_clean_tibble(
    obj,
    "gleam_mechanization_levels",
    c(
      "country",
      "continent",
      "region",
      "feed_material",
      "mechanization_level",
      "species_group"
    ),
    min_rows = 5000L
  )
  expect_equal(ncol(obj), 6L)
  assert_numeric_cols(
    obj,
    "gleam_mechanization_levels",
    "mechanization_level"
  )
  expect_setequal(
    unique(obj$species_group),
    c("ruminant", "monogastric")
  )
  # No duplicate country + feed_material within species group
  dupes <- obj |>
    dplyr::count(country, feed_material, species_group) |>
    dplyr::filter(n > 1L)
  expect_equal(nrow(dupes), 0L, info = "duplicate keys")
  # Both species groups have multiple countries
  n_per_group <- obj |>
    dplyr::summarise(
      n = dplyr::n_distinct(country),
      .by = species_group
    )
  expect_true(all(n_per_group$n > 100L))
})

test_that("gleam_processing_transport_ef is a clean tibble", {
  obj <- whep::gleam_processing_transport_ef
  assert_clean_tibble(
    obj,
    "gleam_processing_transport_ef",
    c(
      "material_number",
      "material",
      "processing_g_co2eq_kg_dm",
      "transport_g_co2eq_kg_dm",
      "species_group"
    ),
    min_rows = 50L
  )
  expect_equal(ncol(obj), 5L)
  assert_numeric_cols(
    obj,
    "gleam_processing_transport_ef",
    c(
      "material_number",
      "processing_g_co2eq_kg_dm",
      "transport_g_co2eq_kg_dm"
    )
  )
  expect_setequal(
    unique(obj$species_group),
    c("ruminant", "monogastric")
  )
  # No NAs — asterisk-footnoted values must be parsed
  expect_true(
    all(!is.na(obj$processing_g_co2eq_kg_dm)),
    info = "processing EFs must not be NA"
  )
  expect_true(
    all(!is.na(obj$transport_g_co2eq_kg_dm)),
    info = "transport EFs must not be NA"
  )
})

test_that("gleam_crop_residue_nitrogen is a clean tibble", {
  obj <- whep::gleam_crop_residue_nitrogen
  assert_clean_tibble(
    obj,
    "gleam_crop_residue_nitrogen",
    c(
      "material_number",
      "material",
      "n_ag",
      "rbg_bio",
      "n_bg",
      "species_group"
    ),
    min_rows = 50L
  )
  expect_equal(ncol(obj), 6L)
  assert_numeric_cols(
    obj,
    "gleam_crop_residue_nitrogen",
    c("material_number", "n_ag", "rbg_bio", "n_bg")
  )
  expect_setequal(
    unique(obj$species_group),
    c("ruminant", "monogastric")
  )
  # Nitrogen values should be non-negative
  expect_true(all(obj$n_ag >= 0, na.rm = TRUE))
  expect_true(all(obj$n_bg >= 0, na.rm = TRUE))
})

test_that("gleam_fracremove is a clean tibble", {
  obj <- whep::gleam_fracremove
  assert_clean_tibble(
    obj,
    "gleam_fracremove",
    c("country", "continent", "region", "fracremove"),
    min_rows = 10L
  )
  expect_equal(ncol(obj), 4L)
  assert_numeric_cols(
    obj,
    "gleam_fracremove",
    "fracremove"
  )
  expect_true(all(obj$fracremove > 0 & obj$fracremove < 1))
  # Each country appears only once
  expect_equal(
    length(unique(obj$country)),
    nrow(obj),
    info = "country must be unique"
  )
})

test_that("gleam_energy_use_ef is a clean tibble", {
  obj <- whep::gleam_energy_use_ef
  assert_clean_tibble(
    obj,
    "gleam_energy_use_ef",
    c(
      "grouping",
      "species",
      "system",
      "climate",
      "energy_type",
      "emission_factor"
    ),
    min_rows = 100L
  )
  expect_equal(ncol(obj), 6L)
  assert_numeric_cols(
    obj,
    "gleam_energy_use_ef",
    "emission_factor"
  )
  expect_setequal(
    unique(obj$energy_type),
    c("embedded", "direct")
  )
  expected_species <- c(
    "dairy_cattle",
    "small_ruminants",
    "pigs",
    "chickens",
    "dairy_cattle_buffalo",
    "large_ruminants"
  )
  expect_true(all(obj$species %in% expected_species))
  # All 6 species are present (no silently dropped table)
  expect_setequal(unique(obj$species), expected_species)
  # Both energy types have multiple species
  species_per_type <- obj |>
    dplyr::summarise(
      n = dplyr::n_distinct(species),
      .by = energy_type
    )
  expect_true(all(species_per_type$n >= 3L))
  # Emission factors must be non-negative where not NA
  expect_true(
    all(obj$emission_factor >= 0, na.rm = TRUE)
  )
  # No footnote text in grouping column
  expect_false(
    any(grepl("^a\\s", obj$grouping)),
    info = "footnote rows must be filtered out"
  )
  # Pigs and chickens must have non-NA emission factors
  # (catches middle-dot notation parsing failure)
  pigs_ef <- obj |>
    dplyr::filter(species == "pigs", energy_type == "embedded")
  expect_true(
    all(!is.na(pigs_ef$emission_factor)),
    info = "pigs embedded EFs must not be NA"
  )
  chickens_ef <- obj |>
    dplyr::filter(
      species == "chickens",
      energy_type == "embedded"
    )
  expect_true(
    all(!is.na(chickens_ef$emission_factor)),
    info = "chickens embedded EFs must not be NA"
  )
  # At least 95% of all emission factors should be non-NA
  non_na_share <- mean(!is.na(obj$emission_factor))
  expect_true(
    non_na_share > 0.95,
    info = paste0(
      "only ",
      round(non_na_share * 100),
      "% non-NA emission factors"
    )
  )
})

test_that("gleam_geographic_hierarchy has correct types", {
  obj <- whep::gleam_geographic_hierarchy
  assert_clean_tibble(
    obj,
    "gleam_geographic_hierarchy",
    c(
      "iso3",
      "country",
      "continent",
      "faostat_region",
      "gleam_region",
      "eu27",
      "oecd"
    ),
    min_rows = 200L
  )
  expect_equal(ncol(obj), 7L)
  expect_true(
    is.integer(obj$eu27),
    info = "eu27 must be integer, not character"
  )
  expect_true(
    is.integer(obj$oecd),
    info = "oecd must be integer, not character"
  )
  expect_true(all(obj$eu27 %in% c(0L, 1L)))
  expect_true(all(obj$oecd %in% c(0L, 1L)))
  # ISO3 codes should be unique and 3 characters
  expect_equal(
    length(unique(obj$iso3)),
    nrow(obj),
    info = "iso3 must be unique"
  )
  expect_true(
    all(nchar(obj$iso3) == 3L),
    info = "iso3 must be 3 characters"
  )
  # No footnotes leaked in as data rows
  expect_false(
    any(grepl("^http|^Source|^List", obj$iso3)),
    info = "footnote rows must be filtered out"
  )
})

test_that("gleam_crop_residue_params is a clean tibble", {
  obj <- whep::gleam_crop_residue_params
  assert_clean_tibble(
    obj,
    "gleam_crop_residue_params",
    c("crop", "dry_matter_pct", "slope", "intercept"),
    min_rows = 15L
  )
  expect_equal(ncol(obj), 4L)
  assert_numeric_cols(
    obj,
    "gleam_crop_residue_params",
    c("dry_matter_pct", "slope", "intercept")
  )
})

test_that("gleam_feed_composition is a clean tibble", {
  obj <- whep::gleam_feed_composition
  assert_clean_tibble(
    obj,
    "gleam_feed_composition",
    c(
      "feed_group",
      "feed_type",
      "gleam_region",
      "feed_use_efficiency"
    ),
    min_rows = 15L
  )
  expect_equal(ncol(obj), 4L)
  assert_numeric_cols(
    obj,
    "gleam_feed_composition",
    "feed_use_efficiency"
  )
})

test_that("gleam_feed_digestibility is a clean tibble", {
  obj <- whep::gleam_feed_digestibility
  assert_clean_tibble(
    obj,
    "gleam_feed_digestibility",
    c(
      "number",
      "material",
      "gross_energy_mj_kg",
      "n_content_g_kg",
      "digestibility_pct"
    ),
    min_rows = 20L
  )
  expect_equal(ncol(obj), 5L)
  assert_numeric_cols(
    obj,
    "gleam_feed_digestibility",
    c("gross_energy_mj_kg", "n_content_g_kg", "digestibility_pct")
  )
})

test_that("gleam_feed_conversion_ratios is a clean tibble", {
  obj <- whep::gleam_feed_conversion_ratios
  assert_clean_tibble(
    obj,
    "gleam_feed_conversion_ratios",
    c(
      "number",
      "material",
      "gross_energy_j_kg",
      "n_content_g_kg",
      "me_chicken_j_kg",
      "me_pigs_j_kg",
      "digestibility_pct"
    ),
    min_rows = 30L
  )
  expect_equal(ncol(obj), 7L)
  assert_numeric_cols(
    obj,
    "gleam_feed_conversion_ratios",
    c("gross_energy_j_kg", "n_content_g_kg")
  )
})

test_that("gleam_dressing_percentages is a clean tibble", {
  obj <- whep::gleam_dressing_percentages
  assert_clean_tibble(
    obj,
    "gleam_dressing_percentages",
    c(
      "species",
      "production_system",
      "cohort",
      "gleam_region",
      "dressing_percent"
    ),
    min_rows = 100L
  )
  expect_equal(ncol(obj), 6L)
  assert_numeric_cols(
    obj,
    "gleam_dressing_percentages",
    "dressing_percent"
  )
  # Dressing percent should be 0–100 range
  non_na <- obj$dressing_percent[!is.na(obj$dressing_percent)]
  expect_true(all(non_na >= 0 & non_na <= 100))
  # Has both regional (country = NA) and country-specific rows
  expect_true(any(is.na(obj$country)))
  expect_true(any(!is.na(obj$country)))
})

test_that("gleam_livestock_categories is a clean tibble", {
  obj <- whep::gleam_livestock_categories
  assert_clean_tibble(
    obj,
    "gleam_livestock_categories",
    c("species", "production_system", "cohort", "description"),
    min_rows = 20L
  )
})

test_that("gleam_enteric_params is a clean tibble", {
  obj <- whep::gleam_enteric_params
  assert_clean_tibble(
    obj,
    "gleam_enteric_params",
    c("species", "system", "ym_percent"),
    min_rows = 5L
  )
  assert_numeric_cols(
    obj,
    "gleam_enteric_params",
    "ym_percent"
  )
})

test_that("gleam_mms_shares is a clean tibble", {
  obj <- whep::gleam_mms_shares
  assert_clean_tibble(
    obj,
    "gleam_mms_shares",
    c("region", "species", "system", "mms", "share_percent"),
    min_rows = 10L
  )
  assert_numeric_cols(
    obj,
    "gleam_mms_shares",
    "share_percent"
  )
})

test_that("gleam_animal_weights is a clean tibble", {
  obj <- whep::gleam_animal_weights
  assert_clean_tibble(
    obj,
    "gleam_animal_weights",
    c("region", "species", "system", "cohort", "weight_kg"),
    min_rows = 15L
  )
  assert_numeric_cols(
    obj,
    "gleam_animal_weights",
    "weight_kg"
  )
})

test_that("gleam_milk_production is a clean tibble", {
  obj <- whep::gleam_milk_production
  assert_clean_tibble(
    obj,
    "gleam_milk_production",
    c("region", "species", "system", "milk_kg_head_yr", "lactation_days"),
    min_rows = 5L
  )
  assert_numeric_cols(
    obj,
    "gleam_milk_production",
    c("milk_kg_head_yr", "lactation_days")
  )
})

# -- IPCC datasets integrity --------------------------------------------------

test_that("IPCC 2019 datasets are clean tibbles", {
  ipcc_datasets <- list(
    ipcc_2019_enteric_ef_cattle = c(
      "category",
      "ef_kg_head_yr"
    ),
    ipcc_2019_enteric_ef_other = c(
      "category",
      "ef_kg_head_yr"
    ),
    ipcc_2019_manure_ch4_ef_cattle = c("category"),
    ipcc_2019_manure_ch4_ef_other = c("category"),
    ipcc_2019_mcf_manure = c("system", "mcf_percent"),
    ipcc_2019_n_excretion = c(
      "category",
      "nex_kg_n_head_yr"
    ),
    ipcc_2019_n2o_ef_direct = c(
      "system",
      "ef_kg_n2o_n_per_kg_n"
    ),
    ipcc_2019_ym = c("category", "ym_percent"),
    ipcc_2019_bo = c("category", "bo_m3_kg_vs"),
    ipcc_2019_cfi = c("category", "cfi_mj_day_kg075")
  )

  for (nm in names(ipcc_datasets)) {
    obj <- getExportedValue("whep", nm)
    assert_clean_tibble(obj, nm, ipcc_datasets[[nm]])
    has_numeric <- any(vapply(
      obj,
      is.numeric,
      logical(1)
    ))
    expect_true(
      has_numeric,
      info = paste(nm, "must have numeric columns")
    )
  }
})

test_that("IPCC 2006 datasets are clean tibbles", {
  ipcc_2006 <- list(
    ipcc_2006_enteric_ef = c(
      "category",
      "ef_kg_head_yr"
    ),
    ipcc_2006_manure_ef = c(
      "category",
      "ef_kg_head_yr"
    ),
    ipcc_2006_mcf_temp = c("system", "mcf_percent")
  )
  for (nm in names(ipcc_2006)) {
    obj <- getExportedValue("whep", nm)
    assert_clean_tibble(obj, nm, ipcc_2006[[nm]])
  }
})

test_that("IPCC Tier 2 datasets are clean tibbles", {
  tier2 <- c(
    "ipcc_tier2_energy_coefs",
    "ipcc_tier2_ym_values",
    "ipcc_tier2_bo_values",
    "ipcc_tier2_manure_ash",
    "ipcc_tier2_n_retention"
  )
  for (nm in tier2) {
    obj <- getExportedValue("whep", nm)
    expect_true(
      tibble::is_tibble(obj),
      info = paste(nm, "must be a tibble")
    )
    expect_gte(nrow(obj), 1L, label = nm)
    has_numeric <- any(vapply(
      obj,
      is.numeric,
      logical(1)
    ))
    expect_true(
      has_numeric,
      info = paste(nm, "must have numeric columns")
    )
  }
})

# -- Other livestock coefficient datasets --------------------------------------

test_that("livestock_production_defaults is a clean tibble", {
  obj <- whep::livestock_production_defaults
  assert_clean_tibble(
    obj,
    "livestock_production_defaults",
    expected_cols = character(0),
    min_rows = 5L
  )
})

test_that("feed_characteristics is a clean tibble", {
  obj <- whep::feed_characteristics
  assert_clean_tibble(
    obj,
    "feed_characteristics",
    expected_cols = character(0),
    min_rows = 2L
  )
})

test_that("livestock_constants is a named list", {
  obj <- whep::livestock_constants
  expect_true(is.list(obj))
  expect_true(
    all(vapply(obj, is.numeric, logical(1)))
  )
  expect_true("energy_content_ch4_mj_kg" %in% names(obj))
  expect_true("days_in_year" %in% names(obj))
})
