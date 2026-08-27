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
  promoted <- c(
    "ATG",
    "BHS",
    "BRB",
    "BTN",
    "COM",
    "FJI",
    "GRD",
    "MAC",
    "MHL",
    "FSM",
    "NCL",
    "NRU",
    "PRI",
    "PYF",
    "SYC",
    "SLB",
    "TON",
    "TUV"
  )
  for (code in promoted) {
    expect_true(
      code %in% pol$iso3_code,
      info = paste(code, "should be a standalone polity")
    )
  }
  expect_gte(nrow(pol), 560L)
})

test_that("polity_area_crosswalk maps promoted countries to real polities", {
  crosswalk <- whep::polity_area_crosswalk

  btn <- crosswalk |>
    dplyr::filter(area_iso3c == "BTN", mapping_status == "matched")
  expect_true(nrow(btn) > 0)
  expect_true(any(grepl("^BTN-", btn$polity_code)))

  com <- crosswalk |>
    dplyr::filter(area_iso3c == "COM", mapping_status == "matched")
  expect_true(nrow(com) > 0)
  expect_true(any(grepl("^COM-", com$polity_code)))
})

test_that("CBS and FABIO area codes map to polity database rows", {
  crosswalk <- whep::polity_area_crosswalk

  cbs_unmapped <- crosswalk |>
    dplyr::filter(
      .data$cbs %in% TRUE,
      .data$mapping_status == "unmapped"
    ) |>
    dplyr::distinct(.data$area_code, .data$area_name)

  fabio_unmapped <- crosswalk |>
    dplyr::filter(
      !is.na(.data$fabio_code),
      .data$mapping_status == "unmapped"
    ) |>
    dplyr::distinct(.data$area_code, .data$area_name)

  expect_equal(nrow(cbs_unmapped), 0L)
  expect_equal(nrow(fabio_unmapped), 0L)

  aggregate_codes <- crosswalk |>
    dplyr::filter(.data$area_code %in% c(15L, 151L, 901:906, 999L)) |>
    dplyr::distinct(.data$area_code, .data$polity_code, .data$has_geometry)

  expect_equal(nrow(aggregate_codes), 9L)
  expect_true(all(!is.na(aggregate_codes$polity_code)))
  expect_true(all(aggregate_codes$has_geometry))

  fabio_row_sources <- crosswalk |>
    dplyr::filter(.data$area_code %in% c(30L, 69L, 152L, 252L, 254L, 299L)) |>
    dplyr::distinct(
      .data$area_code,
      .data$fabio_code,
      .data$polity_area_code,
      .data$polity_code,
      .data$mapping_source,
      .data$has_geometry
    )

  # Eight rows over six areas since whep#717: French Guiana (69) and Palestine
  # (299) carry the upstream map's answer as well as the bucket's, and
  # `.unfold_rest_of_world()` chooses. The bucket's row still says
  # `ROW-1850-2025` for all six, which is what `whep.unfold_rest_of_world =
  # "none"` restores; the promoted rows say the territory.
  expect_equal(nrow(fabio_row_sources), 8L)
  expect_true(all(fabio_row_sources$fabio_code == 999L))
  expect_true(all(fabio_row_sources$polity_area_code == 999L))
  expect_true(all(fabio_row_sources$has_geometry))

  fold <- fabio_row_sources[
    fabio_row_sources$mapping_source == "fabio_row_fold",
  ]
  expect_setequal(fold$area_code, c(30L, 69L, 152L, 252L, 254L, 299L))
  expect_true(all(fold$polity_code == "ROW-1850-2025"))

  promoted <- fabio_row_sources[
    fabio_row_sources$mapping_source == "fabio_row_promoted",
  ]
  expect_setequal(promoted$area_code, c(69L, 299L))
  expect_setequal(promoted$polity_code, c("GUF-1946-2025", "PSE-1948-2025"))
})


# -- the fabio_code rule and its exceptions (issue #556) -----------------------

# `fabio_code` is not merely a fact about FABIO: `polity_area_code` is derived
# from it, so it is also the instruction saying which bucket an area's rows are
# summed into. The rule it follows is exact -- own code for a `cbs` reporter,
# 999 otherwise -- which is what makes its seven exceptions readable, and what
# makes four of them a contradiction rather than a convention.

test_that("regions_full sets fabio_code from cbs, with seven exceptions", {
  regions <- whep::regions_full |>
    dplyr::filter(!is.na(.data$code))

  # 62 into 238 and 276/277 into 206 are successor-state folds: territorial
  # identities, not a FABIO convention.
  #
  # 153, 154, 209 and 212 are the #556 contradiction. They are flagged as
  # reporting a balance sheet of their own AND folded into Rest of World, and
  # FABIO does NOT fold them: `io_codes.csv` of the FABIO v1.1 release (Zenodo
  # record 2577067) gives each of the four its own 125-commodity block among 192
  # areas, and the FABIO source repository (fineprint-global/fabio) marks all
  # four `current == TRUE` in `inst/regions_full.csv`, which is exactly the flag
  # its `replace_RoW()` keeps out of bucket 999. Lifting the fold moves
  # published values, so the four are pinned here rather than corrected; when
  # the decision is made they leave this table.
  exceptions <- tibble::tribble(
    ~code, ~fabio_code,
    62L,   238L,
    153L,  999L,
    154L,  999L,
    209L,  999L,
    212L,  999L,
    276L,  206L,
    277L,  206L
  )

  reporters <- regions |> dplyr::filter(.data$cbs %in% TRUE)
  expect_equal(nrow(reporters), 202L)

  mismatched <- reporters |>
    dplyr::filter(
      is.na(.data$fabio_code) | .data$fabio_code != .data$code
    ) |>
    dplyr::transmute(
      code = as.integer(.data$code),
      fabio_code = as.integer(.data$fabio_code)
    ) |>
    dplyr::arrange(.data$code)

  expect_equal(mismatched, exceptions)

  # The converse, which is what makes `cbs` an exact discriminator: no
  # non-reporter keeps its own code, so `cbs` alone separates the 57 folds
  # FABIO also makes from the 4 it does not.
  kept <- regions |>
    dplyr::filter(
      !.data$cbs %in% TRUE,
      !is.na(.data$fabio_code),
      .data$fabio_code == .data$code
    )
  expect_equal(nrow(kept), 0L)
})


# -- area label encoding (issue #399) ------------------------------------------

# No area label in a published table may be mojibake. Three territory names
# shipped corrupt across eight cells: Curacao (area 279) in three columns of
# regions_full and polity_area_crosswalk, Cote d'Ivoire (area 107) in four
# columns of regions_full and polities_cats, and "Netherlands Antilles /
# Curacao" (area 151) in one. Each was the UTF-8 bytes of the accented letter
# decoded as a pair of Latin-1 characters in the vendored harmonization CSVs, now
# repaired on read in data-raw/_labels.R.
#
# Swept across every character column rather than a list of label columns,
# because a repair aimed at label columns alone fixes area 279's `name` and
# leaves the identical corruption in `iea`, `water_area` and `Lassaletta`.
# Mojibake is never wanted in any string column, so the rule is the column's
# type.
#
# It was not costing a join, and that was checked rather than assumed: no alias
# resolves on either spelling of Curacao. It was a latent trap all the same,
# because area 279's FAOSTAT_name is NA, so the corrupt `name` was the only label
# it had -- an alias added later under the correct spelling would have missed in
# silence.
test_that("published area tables carry no mojibake in any label", {
  tables <- c(
    "regions_full",
    "polities_cats",
    "polity_area_crosswalk",
    "polities"
  )
  offenders <- character(0)
  checked <- 0L
  for (nm in tables) {
    d <- get(nm, envir = asNamespace("whep"))
    for (col in names(d)[vapply(d, is.character, logical(1))]) {
      checked <- checked + 1L
      # Every Latin-1-decoded UTF-8 byte pair opens with U+00C3.
      hits <- unique(grep("\u00c3", d[[col]], value = TRUE))
      if (length(hits) > 0L) {
        offenders <- c(
          offenders,
          paste0(
            nm,
            "$",
            col,
            " (",
            paste(utils::head(hits, 3), collapse = ", "),
            ")"
          )
        )
      }
    }
  }
  # Non-vacuous: zero character columns would make the loop prove nothing.
  expect_gt(checked, 40L)
  expect_equal(
    length(offenders),
    0L,
    info = paste("mojibake in area labels:", paste(offenders, collapse = "; "))
  )

  # And the repaired names read correctly, so a repair that silently stopped
  # working fails here instead of reverting to a corrupt string nobody reads.
  regions <- whep::regions_full
  expect_true("Cura\u00e7ao" %in% regions$name)
  expect_true("C\u00f4te d'Ivoire" %in% regions$iea)
  crosswalk <- whep::polity_area_crosswalk
  expect_true(
    "Cura\u00e7ao" %in% crosswalk$area_name[which(crosswalk$area_code == 279L)]
  )
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
      "species",
      "herd",
      "grouping",
      "grouping_scheme",
      "system",
      "climate",
      "energy_type",
      "denominator",
      "emission_factor"
    ),
    min_rows = 100L
  )
  expect_equal(ncol(obj), 9L)
  assert_numeric_cols(
    obj,
    "gleam_energy_use_ef",
    "emission_factor"
  )
  expect_setequal(
    unique(obj$energy_type),
    c("embedded", "direct")
  )
  expect_setequal(
    unique(obj$grouping_scheme),
    c("development3", "region5", "detailed15")
  )
  expect_setequal(
    unique(obj$denominator),
    c("lw", "milk", "egg")
  )
  expected_species <- c(
    "cattle",
    "buffalo",
    "small_ruminants",
    "pigs",
    "chickens",
    "large_ruminants"
  )
  expect_true(all(obj$species %in% expected_species))
  # All species are present (no silently dropped table)
  expect_setequal(unique(obj$species), expected_species)
  # Both energy types have multiple species
  species_per_type <- obj |>
    dplyr::summarise(
      n = dplyr::n_distinct(species),
      .by = energy_type
    )
  expect_true(all(species_per_type$n >= 3L))
  # Footnote-derived rows: meat cattle embedded is half of dairy cattle.
  dairy <- obj |>
    dplyr::filter(
      species == "cattle",
      herd == "dairy",
      energy_type == "embedded"
    )
  meat <- obj |>
    dplyr::filter(
      species == "cattle",
      herd == "non_dairy",
      energy_type == "embedded"
    )
  joined <- dplyr::inner_join(
    dairy,
    meat,
    by = c("grouping", "system", "climate"),
    suffix = c("_dairy", "_meat")
  )
  expect_equal(joined$emission_factor_meat, joined$emission_factor_dairy * 0.5)
  # Emission factors must be non-negative and fully resolved (this catches the
  # middle-dot notation parsing failure for pigs and chickens).
  expect_true(all(obj$emission_factor >= 0))
  expect_false(any(is.na(obj$emission_factor)))
  # No footnote text in grouping column
  expect_false(
    any(grepl("^a\\s", obj$grouping)),
    info = "footnote rows must be filtered out"
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
      "oecd",
      "reporting_polity_code",
      "reporting_polity_name"
    ),
    min_rows = 200L
  )
  expect_equal(ncol(obj), 9L)
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

test_that("gleam_geographic_hierarchy oecd is exactly the 38 Members", {
  # whep#574: the workbook flags Comoros as OECD, and the flag is not decorative
  # -- `.energy_country_grouping()` reads it for two of the three GLEAM schemes,
  # so it priced Comoros' meat at OECD energy intensity (up to +129%).
  # `data-raw/livestock_coefficients.R` corrects the column against the OECD's
  # own membership list; this is the gate on that, because `livestock_coefs` is
  # one of the seven datasets `test_data_raw_freshness.R` cannot rebuild.
  obj <- whep::gleam_geographic_hierarchy
  expect_setequal(obj$iso3[obj$oecd == 1L], oecd_members_iso3())
  expect_equal(sum(obj$oecd), 38L)
  expect_equal(obj$oecd[obj$iso3 == "COM"], 0L)
  # The EU27 column beside it is untouched and already correct, which is part of
  # why the OECD cell reads as a data-entry slip rather than a GLEAM grouping
  # that merely borrows the name.
  expect_equal(sum(obj$eu27), 27L)
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

test_that("regional_mms_distribution keeps its whep#921 values", {
  # This table is an unsourced placeholder (see its @source) and it is LIVE:
  # `.resolve_mms_shares()` weights the Tier 2 manure CH4 methane conversion
  # factor and the Tier 1 manure direct-N2O EF3 with it. GLEAM 2.0 Supplement
  # S1 Tables 4.2-4.11 publish real regional shares that disagree materially,
  # so revaluing this table is a maintainer decision (whep#921), never a
  # drive-by edit. What is locked here is the two effective factors the table
  # actually feeds, per (region, species), so any revalue has to come through
  # a deliberate update of these expectations with its numbers stated.
  #
  # The Poultry EF3 of 0.005 is the `dplyr::coalesce()` default, not a table
  # value: `mms_type` "Poultry Manure" and "Anaerobic Lagoon" match no row of
  # `ipcc_2019_n2o_ef_direct`, whose labels are "Poultry Manure - High Rise" /
  # "- Deep Litter" and "Uncovered Anaerobic Lagoon" (all 0.001).
  mms <- whep::regional_mms_distribution
  temperate_mcf <- whep::climate_mcf |>
    dplyr::filter(.data$climate_zone == "Temperate") |>
    dplyr::select("mms_type", "mcf_percent")
  ef3 <- whep::ipcc_2019_n2o_ef_direct |>
    dplyr::rename(mms_type = "system", ef3 = "ef_kg_n2o_n_per_kg_n")

  effective <- mms |>
    dplyr::left_join(temperate_mcf, by = "mms_type") |>
    dplyr::left_join(ef3, by = "mms_type") |>
    dplyr::mutate(
      mcf_percent = dplyr::coalesce(.data$mcf_percent, 2),
      ef3 = dplyr::coalesce(.data$ef3, 0.005)
    ) |>
    dplyr::summarise(
      share_total = sum(.data$fraction),
      weighted_mcf = sum(.data$fraction * .data$mcf_percent / 100),
      weighted_ef3 = sum(.data$fraction * .data$ef3),
      .by = c("region", "species")
    ) |>
    dplyr::arrange(.data$species, .data$region)

  expected <- tibble::tribble(
    ~region,           ~species,          ~weighted_mcf, ~weighted_ef3,
    "Global",          "Buffalo",         0.01450,       0.00950,
    "Global",          "Camels",          0.01500,       0.01000,
    "Global",          "Cattle",          0.07225,       0.00730,
    "Latin America",   "Cattle",          0.03450,       0.00885,
    "North America",   "Cattle",          0.15600,       0.00530,
    "Western Europe",  "Cattle",          0.14300,       0.00495,
    "Global",          "Goats",           0.01500,       0.01000,
    "Global",          "Horses",          0.02000,       0.00900,
    "Global",          "Mules and Asses", 0.01500,       0.01000,
    "Global",          "Poultry",         0.02000,       0.00500,
    "Global",          "Sheep",           0.01500,       0.01000,
    "Global",          "Swine",           0.19150,       0.00400,
    "North America",   "Swine",           0.32600,       0.00290
  ) |>
    dplyr::arrange(.data$species, .data$region)

  testthat::expect_equal(nrow(mms), 33L)
  # Every (region, species) group sums to one, which is what keeps the
  # renormalisation in `.mms_global_shares()` inert and the split mass-
  # conserving.
  testthat::expect_equal(
    effective$share_total,
    rep(1, nrow(effective))
  )
  testthat::expect_equal(effective$region, expected$region)
  testthat::expect_equal(effective$species, expected$species)
  testthat::expect_equal(effective$weighted_mcf, expected$weighted_mcf)
  testthat::expect_equal(effective$weighted_ef3, expected$weighted_ef3)
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

test_that("gleam_animal_weights regions resolve as whep#881 measured", {
  # The values in gleam_animal_weights are unsourced placeholders (see its
  # @source): GLEAM's own live weights, in Supplement S1 Tables 2.4-2.16 of the
  # 2.0 Model description, differ by -27% to +33%. Until they are re-ingested,
  # this locks WHICH regions the placeholders reach, so a rename cannot
  # silently widen or narrow their footprint without moving Tier 2 gross
  # energy. `.gleam_region_of()` emits the labels of
  # `gleam_geographic_hierarchy`, so a region absent from that vocabulary is a
  # dead row whose territories fall back to the Global weights.
  weight_regions <- setdiff(unique(whep::gleam_animal_weights$region), "Global")
  gleam_regions <- unique(whep::gleam_geographic_hierarchy$gleam_region)

  testthat::expect_setequal(
    intersect(weight_regions, gleam_regions),
    c("Western Europe", "North America", "Sub-Saharan Africa", "South Asia")
  )
  # Known dead row: GLEAM 3.0 calls this region "Central & South America".
  testthat::expect_setequal(
    setdiff(weight_regions, gleam_regions),
    "Latin America"
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

test_that("Bo values match IPCC 2019 Table 10.16a (high-productivity)", {
  # Regression guard for issues #252 (Horses) and #253 (Poultry-Broilers).
  # Values verified against IPCC 2019 Refinement Vol 4 Ch 10 Table 10.16a,
  # high-productivity systems column (the tier the rest of the table uses).
  expected <- tibble::tribble(
    ~category, ~bo_m3_kg_vs,
    "Horses", 0.30,
    "Mules and Asses", 0.33,
    "Poultry - Layers", 0.39,
    "Poultry - Broilers", 0.36
  )

  for (nm in c("ipcc_2019_bo", "ipcc_tier2_bo_values")) {
    obj <- getExportedValue("whep", nm)
    got <- expected |>
      dplyr::left_join(obj, by = "category", suffix = c("_exp", "_got"))
    testthat::expect_equal(
      got$bo_m3_kg_vs_got,
      got$bo_m3_kg_vs_exp,
      info = nm
    )

    bo <- function(cat) obj$bo_m3_kg_vs[obj$category == cat]
    # #252: Horses must not be copied from Mules and Asses.
    testthat::expect_false(bo("Horses") == bo("Mules and Asses"), info = nm)
    # #253: broilers and layers share the high-productivity tier.
    testthat::expect_gt(bo("Poultry - Broilers"), 0.24, label = nm)
  }
})

# The `ipcc_2019_*` objects do not all hold 2019 Refinement values; whep#601
# tracks the decision on whether to revalue them, rename them or expose both
# editions. Until that is settled these expectations lock the values in place
# and lock them to the provenance stated in `?ipcc_2019_enteric_ef_cattle` and
# friends, so a revalue can only happen deliberately and with the numeric
# consequence measured. Every reference value below was read off the published
# PDFs (2019 Refinement Vol 4 Ch 10 Tables 10.10, 10.11, 10.16A, 10.21;
# 2006 Guidelines Vol 4 Ch 10 same numbers; 2006 and 2019 Vol 4 Ch 11
# Table 11.1).
test_that("ipcc_2019 tables still hold the provenance whep#601 documents", {
  cattle <- whep::ipcc_2019_enteric_ef_cattle
  ef_of <- function(reg, cat) {
    cattle$ef_kg_head_yr[cattle$region == reg & cattle$category == cat]
  }
  # 2006 Table 10.11, not the 2019 Refinement's 138/64 and 126/52.
  testthat::expect_equal(ef_of("North America", "Dairy Cattle"), 128)
  testthat::expect_equal(ef_of("North America", "Other Cattle"), 53)
  testthat::expect_equal(ef_of("Western Europe", "Dairy Cattle"), 117)
  testthat::expect_equal(ef_of("Western Europe", "Other Cattle"), 57)
  # Every cell shared with the separate 2006 object is identical to it, bar
  # the one Middle East dairy cell asserted below.
  shared <- whep::ipcc_2006_enteric_ef |>
    dplyr::filter(
      region != "Global",
      !(region == "Middle East" & category == "Dairy Cattle")
    ) |>
    dplyr::inner_join(
      cattle,
      by = c("region", "category"),
      suffix = c("_06", "_19")
    )
  testthat::expect_equal(nrow(shared), 15L)
  testthat::expect_equal(shared$ef_kg_head_yr_19, shared$ef_kg_head_yr_06)
  # Cells that match neither edition: Oceania dairy is 100 in 2006 and 93 in
  # 2019; Middle East dairy is 46 (grouped with Africa) and 76; Indian
  # Subcontinent dairy is 58 and 73.
  testthat::expect_equal(ef_of("Oceania", "Dairy Cattle"), 90)
  testthat::expect_equal(ef_of("Middle East", "Dairy Cattle"), 63)
  testthat::expect_equal(ef_of("Indian Subcontinent", "Dairy Cattle"), 68)
  # The Global fallback row is in no IPCC table.
  testthat::expect_equal(ef_of("Global", "Dairy Cattle"), 80)

  # 2006 Table 10.10 developed-countries column; the 2019 Refinement splits
  # sheep and goats 9 high / 5 low and moved buffalo to Table 10.11.
  other <- whep::ipcc_2019_enteric_ef_other
  oth_of <- function(cat) other$ef_kg_head_yr[other$category == cat]
  testthat::expect_equal(oth_of("Buffalo"), 55)
  testthat::expect_equal(oth_of("Sheep"), 8)
  testthat::expect_equal(oth_of("Goats"), 5)

  # EF3: neither edition gives these. Table 10.21 has 0 for daily spread,
  # no-crust slurry and uncovered lagoon, and 0.02 for dry lot.
  ef3 <- whep::ipcc_2019_n2o_ef_direct
  ef3_of <- function(sys) ef3$ef_kg_n2o_n_per_kg_n[ef3$system == sys]
  testthat::expect_equal(ef3_of("Daily Spread"), 0.01)
  testthat::expect_equal(ef3_of("Dry Lot"), 0.005)
  testthat::expect_equal(ef3_of("Uncovered Anaerobic Lagoon"), 0.001)
  testthat::expect_equal(ef3_of("Liquid/Slurry - No Crust"), 0.002)
  # Pasture/range/paddock is the 2006 Ch 11 EF3PRP,SO; 2019 gives 0.004.
  testthat::expect_equal(ef3_of("Pasture/Range/Paddock"), 0.01)

  # Table 10.16A publishes one swine Bo; breeding swine share the market
  # swine value in both editions, so 0.27 is unsourced.
  bo <- whep::ipcc_2019_bo
  testthat::expect_equal(
    bo$bo_m3_kg_vs[bo$category == "Swine - Breeding"],
    0.27
  )

  # Table 10.4 publishes 0.370 for intact bulls in both editions; this
  # object folds bulls into the 0.322 non-lactating row.
  cfi <- whep::ipcc_2019_cfi
  testthat::expect_false(any(cfi$cfi_mj_day_kg075 == 0.370))
  testthat::expect_true(any(
    cfi$subcategory == "Non-lactating/Bulls" & cfi$cfi_mj_day_kg075 == 0.322
  ))

  # 2019 Table 10.4 (Updated) adds a goat row the 2006 table lacks:
  # Goats 0.315, Sheep (older than 1 year) 0.217. Goats inheriting the
  # sheep value was #249; lock both, and that they stay distinct.
  cfi_of <- function(cat) cfi$cfi_mj_day_kg075[cfi$category == cat]
  testthat::expect_equal(cfi_of("Goats"), 0.315)
  testthat::expect_equal(cfi_of("Sheep"), 0.217)
  testthat::expect_false(isTRUE(all.equal(cfi_of("Goats"), cfi_of("Sheep"))))

  # Nex is stored per head per year while both editions publish a rate per
  # 1000 kg animal mass per day, so no stored value may be read as a rate.
  nex <- whep::ipcc_2019_n_excretion
  testthat::expect_equal(
    nex$nex_kg_n_head_yr[
      nex$region == "North America" & nex$category == "Dairy Cattle"
    ],
    105
  )
})

test_that("Tier 2 goat coefficients are the goat rows, not the sheep ones", {
  # The sheep and goat coefficients sit one row apart in two IPCC tables
  # and were copied across in both directions (#249, PR #267). Lock each
  # against the published value.
  # Cfi: 2019 Refinement Vol 4 Ch 10 Table 10.4 (Updated) -- Goats 0.315,
  # Sheep (older than 1 year) 0.217.
  # Ca: Table 10.5 (Updated) -- Lowland goats 0.019, Grazing flat pasture
  # (sheep) 0.0107, Hill and mountain goats 0.024.
  coefs <- whep::ipcc_tier2_energy_coefs
  row_of <- function(cat) coefs[coefs$category == cat, ]
  goats <- row_of("Goats")
  sheep <- row_of("Sheep")

  testthat::expect_equal(goats$cfi_mj_day_kg075, 0.315)
  testthat::expect_equal(sheep$cfi_mj_day_kg075, 0.217)
  testthat::expect_equal(goats$ca_pasture, 0.019)
  testthat::expect_equal(sheep$ca_pasture, 0.0107)
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

test_that("smil_2001_synthetic_n_global covers 1913-2000", {
  obj <- whep::smil_2001_synthetic_n_global
  assert_clean_tibble(
    obj,
    "smil_2001_synthetic_n_global",
    expected_cols = c("year", "global_kt_n"),
    min_rows = 10L
  )
  expect_equal(min(obj$year), 1913L)
  expect_equal(max(obj$year), 2000L)
  # Monotone growth between WWII recovery (1945) and 2000.
  recovery <- obj |> dplyr::filter(year >= 1945, year <= 2000)
  expect_true(all(diff(recovery$global_kt_n) > 0))
  # Haber-Bosch first commercial year (1913) is the smallest anchor.
  expect_equal(obj$global_kt_n[obj$year == 1913L], min(obj$global_kt_n))
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

# -- mapping key uniqueness (issue #178) ---------------------------------------

# A non-unique join key silently fans out downstream merges; a fully
# duplicated row double-counts. Assert both invariants on the shipped
# mapping tables. NA keys are allowed (unmapped rows), but non-NA keys
# must be unique.
assert_unique_key <- function(obj, name, key) {
  values <- obj[[key]]
  dup_keys <- unique(values[!is.na(values) & duplicated(values)])
  expect_true(
    length(dup_keys) == 0L,
    info = paste(
      name,
      "has non-unique key",
      key,
      "-",
      paste(dup_keys, collapse = ", ")
    )
  )
  expect_equal(
    nrow(obj),
    nrow(dplyr::distinct(obj)),
    label = paste(name, "has fully duplicated rows")
  )
}

test_that("mapping tables have unique keys and no duplicate rows", {
  assert_unique_key(whep::items_prod_full, "items_prod_full", "item_prod_code")
  assert_unique_key(whep::items_full, "items_full", "item_cbs_code")
  assert_unique_key(whep::regions_full, "regions_full", "code")
  assert_unique_key(whep::cbs_trade_codes, "cbs_trade_codes", "item_code_trade")
  assert_unique_key(whep::animals_codes, "animals_codes", "item_cbs_code")
})

test_that("FAOSTAT production code 1807 maps only to Sheep and Goat Meat", {
  # Verified against FAOSTAT: 1807 = Sheep and Goat Meat,
  # Citrus Fruit, Total = 1804 (issue #178).
  at_1807 <- whep::items_prod_full |>
    dplyr::filter(item_prod_code == 1807)
  expect_equal(nrow(at_1807), 1L)
  expect_equal(at_1807$item_prod, "Sheep and Goat Meat")

  citrus <- whep::items_prod_full |>
    dplyr::filter(item_prod == "Citrus Fruit, Total")
  expect_equal(citrus$item_prod_code, "1804")
})

testthat::test_that("coello_synthetic_n has the expected schema + range", {
  x <- whep::coello_synthetic_n
  pointblank::expect_col_exists(
    x,
    c("year", "area_code", "item_cbs_code", "kg_n_ha")
  )
  testthat::expect_true(is.integer(x$area_code))
  testthat::expect_true(is.integer(x$item_cbs_code))
  testthat::expect_equal(min(x$year), 1961L)
  testthat::expect_equal(max(x$year), 2023L)
  testthat::expect_true(all(x$kg_n_ha >= 0))
  # Data-quality safeguard: implausible Coello outliers (>1000 kg N/ha) are
  # dropped to missing in the builder, so no rate exceeds the threshold.
  testthat::expect_true(all(x$kg_n_ha <= 1000))
  testthat::expect_gt(nrow(x), 0L)
})

# -- Documented @format columns match the built data ---------------------------

# Nothing checked a dataset's @format against the dataset itself, which is how
# #173 happened: five documented datasets named columns that do not exist
# (`nex_kg_per_1000kg_day`, `annual_temp_c`, `mms_type`, ...), and only one had
# been spotted. Report the column names the \format section of one Rd topic
# claims but its dataset does not have.
format_column_mismatches <- function(rd_path, ignore) {
  lines <- readLines(rd_path, warn = FALSE)
  topic <- stringr::str_match(lines, "^\\\\name\\{(.+)\\}$")[, 2]
  topic <- topic[!is.na(topic)][1]
  obj <- tryCatch(
    getExportedValue("whep", topic),
    error = function(e) NULL
  )
  claimed <- rd_format_claims(lines)
  if (!is.data.frame(obj) || length(claimed) == 0L) {
    return(character(0))
  }
  missing <- setdiff(claimed, c(names(obj), ignore))
  if (length(missing) == 0L) {
    return(character(0))
  }
  paste0(
    topic,
    " @format names ",
    paste(missing, collapse = ", "),
    "; columns are ",
    paste(names(obj), collapse = ", ")
  )
}

# Column names claimed by the prose part of a \format section, i.e. the
# "A tibble with \code{a}, \code{b}." form. Per-column \itemize / \describe
# lists are left out: their bullets mix column names with the column's own
# values and cross-references, so reading them as column names would flag
# prose. Only snake_case tokens can be column names here.
rd_format_claims <- function(lines) {
  block <- rd_section_lines(lines, "format")
  listed <- stringr::str_which(block, "\\\\(itemize|describe|tabular)\\{")
  if (length(listed) > 0L) {
    block <- block[seq_len(listed[1] - 1L)]
  }
  tokens <- stringr::str_match_all(block, "\\\\code\\{([^{}]+)\\}")
  tokens <- unique(unlist(lapply(tokens, function(m) m[, 2])))
  tokens[stringr::str_detect(tokens, "^[a-z][a-z0-9_]*$")]
}

# Lines of one Rd section, delimited by brace depth so nested environments
# stay whole.
rd_section_lines <- function(lines, tag) {
  start <- which(stringr::str_detect(lines, paste0("^\\\\", tag, "\\{")))[1]
  if (is.na(start)) {
    return(character(0))
  }
  depth <- cumsum(
    stringr::str_count(lines, stringr::fixed("{")) -
      stringr::str_count(lines, stringr::fixed("}"))
  )
  end <- which(depth == 0L)
  end <- end[end >= start][1]
  if (is.na(end)) {
    return(character(0))
  }
  lines[start:end]
}

testthat::test_that("documented @format columns exist in the dataset", {
  man_dir <- testthat::test_path("..", "..", "man")
  testthat::skip_if_not(
    dir.exists(man_dir),
    "man/ is only there when testing from the package source"
  )
  # A token naming another documented object is a cross-reference, not a
  # column, and \code{tibble} is prose.
  ignore <- c(
    getNamespaceExports("whep"),
    utils::data(package = "whep")$results[, "Item"],
    "tibble"
  )
  mismatches <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE) |>
    lapply(format_column_mismatches, ignore = ignore) |>
    unlist() |>
    as.character()
  testthat::expect_equal(mismatches, character(0))
})


# -- dataset provenance --------------------------------------------------------

# #652: `lassaletta_grassland_share` shipped `@source` "Lassaletta et al.
# nitrogen flow dataset. See pipeline documentation for full citation.", and no
# such pipeline documentation exists. A citation that names no paper cannot be
# checked by a reader, so it is worse than an explicit "unverified" note.
testthat::test_that("no documented topic defers its citation to nowhere", {
  man_dir <- testthat::test_path("..", "..", "man")
  testthat::skip_if_not(
    dir.exists(man_dir),
    "man/ is only there when testing from the package source"
  )
  offenders <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE) |>
    purrr::keep(\(rd) {
      text <- paste(readLines(rd, warn = FALSE), collapse = " ")
      stringr::str_detect(text, "See pipeline documentation")
    }) |>
    basename()
  testthat::expect_equal(offenders, character(0))
})

testthat::test_that("lassaletta_grassland_share cites its paper by DOI", {
  man_dir <- testthat::test_path("..", "..", "man")
  testthat::skip_if_not(
    dir.exists(man_dir),
    "man/ is only there when testing from the package source"
  )
  rd <- file.path(man_dir, "lassaletta_grassland_share.Rd")
  text <- paste(readLines(rd, warn = FALSE), collapse = " ")
  testthat::expect_true(
    stringr::str_detect(text, stringr::fixed("10.1088/1748-9326/9/10/105011"))
  )
})

# The invariants below are the fingerprint tying the shipped table to
# Lassaletta et al. (2014): its 1961-2009 span, and Ireland and the
# Netherlands as the two extreme countries the paper singles out by name.
testthat::test_that("lassaletta_grassland_share matches its source's shape", {
  share <- whep::lassaletta_grassland_share
  testthat::expect_equal(sort(unique(share$year)), 1961:2009)
  testthat::expect_true(all(table(share$Country) == 49L))
  testthat::expect_true(all(share$grass_share >= 0 & share$grass_share <= 1))
  extremes <- share |>
    dplyr::slice_max(grass_share, n = 1, by = Country, with_ties = FALSE) |>
    dplyr::slice_max(grass_share, n = 2, with_ties = FALSE) |>
    dplyr::pull(Country)
  testthat::expect_setequal(extremes, c("Ireland", "Netherlands"))
})

# The label set is not a partition: a historical entity and its successors
# coexist for the whole span, which is why the consumer needs a dedup rule.
testthat::test_that("Sudan and Sudan (former) are duplicate labels", {
  share <- whep::lassaletta_grassland_share
  sudan <- c("Sudan", "Sudan (former)", "South Sudan")
  testthat::expect_true(all(sudan %in% share$Country))
  wide <- share |>
    dplyr::filter(Country %in% sudan[1:2]) |>
    tidyr::pivot_wider(names_from = Country, values_from = grass_share)
  testthat::expect_equal(wide$Sudan, wide$`Sudan (former)`)
})
