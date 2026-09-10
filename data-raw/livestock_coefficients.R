# Livestock Coefficients Data Extraction
# This script extracts and documents all coefficient tables needed for
# livestock emissions calculations following GLEAM 3.0 and IPCC 2019/2006
# methodologies.
#
# Sources:
# - GLEAM 3.0: FAO (2022) Global Livestock Environmental Assessment Model,
#   Version 3.0. Supplement S1 (the supplementary-tables workbook), read from
#   data-raw/GLEAM_3.0_Supplement_S1.xlsx, which is byte-identical (149119
#   bytes, md5 207e3e928c176b2189e520bddcb0c5f6) to
#   https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx
#   FAO issues no DOI for it. It is NOT MacLeod et al. (2018), and the DOI
#   10.1088/1748-9326/aad4d8 previously cited here is unregistered (whep#607).
#   The tables built by generate_gleam_pdf_tables() below are hardcoded, not
#   read from that workbook. whep#881 traced them: see the @source blocks in
#   R/livestock_coefs.R for the per-table verdict, and the note below.
# - GLEAM 2.0: FAO (2018) GLEAM Model description, Version 2.0, Revision 5,
#   July 2018, data reference year 2010.
#   https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Model_description.pdf
#   Its own supplementary-tables workbook,
#   https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Supplement_S1.xlsx
#   (md5 72fd2ea477dfe8b30cd3657b2baa4af1, retrieved 2026-08-26), is the only
#   GLEAM publication that carries REGIONAL HERD PARAMETERS and REGIONAL MMS
#   SHARES: Tables 2.4-2.16 (live weights, replacement/fertility/mortality
#   rates, age at first calving) and Tables 4.2-4.11 (MMS shares). Version 3.0
#   dropped both table families, which is why the committed 3.0 workbook has
#   no sheet for them. Nothing here reads the 2.0 workbook yet; re-ingesting
#   it changes published numbers (whep#881) and is a maintainer decision.
# - GLEAM 3.0 model description PDF (for the in-document tables, as opposed to
#   the workbook): https://www.fao.org/3/cd8425en/cd8425en.pdf
#   (md5 5c8d20e480174cad65ed9e1b80fc4d71, retrieved 2026-08-26).
# - IPCC 2019: 2019 Refinement to the 2006 IPCC Guidelines
#   Volume 4, Chapter 10: Emissions from Livestock and Manure Management
#   https://www.ipcc-nggip.iges.or.jp/public/2019rf/vol4.html
# - IPCC 2006: 2006 IPCC Guidelines for National GHG Inventories
#   Volume 4, Chapter 10

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)

# Helper Functions ----

clean_names <- function(names) {
  names |>
    stringr::str_replace_all("\\s+", "_") |>
    stringr::str_replace_all("[^[:alnum:]_]", "") |>
    tolower() |>
    make.unique(sep = "_")
}

# GLEAM Table Parsers ----

# Region abbreviation mapping used in GLEAM S.9.1
gleam_region_abbrevs <- c(
  "NA" = "North America",
  "RUS" = "Russia",
  "WE" = "Western Europe",
  "EE" = "Eastern Europe",
  "NENA" = "Near East and North Africa",
  "ESEA" = "East and Southeast Asia",
  "OCE" = "Oceania",
  "SA" = "South Asia",
  "LAC" = "Latin America and Caribbean",
  "SSA" = "Sub-Saharan Africa"
)

parse_dressing_percentages <- function(raw) {
  # Row 1 = title, Row 2 = region abbreviations in cols 2:11
  # The first region "NA" (North America) must not become R's NA
  regions <- as.character(raw[2, 2:11])
  regions[is.na(regions)] <- "NA"

  # --- Section 1: Regional dressing percentages (rows 3-25) ---
  data_rows <- raw[3:25, ]
  col1 <- data_rows[[1]]

  # Build species/production_system/cohort mapping by parsing
  # the indentation-based hierarchy
  rows <- list()
  current_species <- NA_character_
  current_system <- NA_character_

  for (i in seq_len(nrow(data_rows))) {
    label <- col1[i]
    if (is.na(label)) {
      next
    }
    vals <- as.character(data_rows[i, 2:11])
    all_na <- all(is.na(vals) | vals == "")
    is_indented <- stringr::str_detect(label, "^\\s{2,}")
    clean_label <- stringr::str_trim(label)

    if (!is_indented && all_na) {
      # Species header (e.g. "Dairy cattle", "Pigs", "Chicken")
      parsed <- .parse_species_header(clean_label)
      current_species <- parsed$species
      current_system <- parsed$system
      next
    }

    if (!is_indented && !all_na) {
      # Single-row species with no sub-cohorts (Sheep, Goats)
      for (j in seq_along(regions)) {
        v <- .parse_dressing_value(vals[j])
        rows <- c(
          rows,
          list(tibble::tibble(
            species = clean_label,
            production_system = NA_character_,
            cohort = NA_character_,
            gleam_region = regions[j],
            dressing_percent = v
          ))
        )
      }
      next
    }

    if (is_indented) {
      # Sub-row: could be a cohort or a production system
      parsed <- .parse_dressing_subrow(
        clean_label,
        vals,
        regions,
        current_species,
        current_system
      )
      if (!is.null(parsed)) {
        rows <- c(rows, list(parsed))
      }
    }
  }

  regional <- dplyr::bind_rows(rows) |>
    dplyr::mutate(
      production_system = stringr::str_remove(
        production_system,
        "\\s+systems?$"
      )
    )

  # --- Section 2: Country-specific pig industrial values ---
  # Rows 28-60 (indices in raw): header at row 27, data from 28
  country_start <- which(
    raw[[1]] == "COUNTRY" |
      (!is.na(raw[[1]]) & raw[[1]] == "COUNTRY")
  )
  if (length(country_start) > 0) {
    country_data <- raw[(country_start + 1):nrow(raw), 1:3]
    names(country_data) <- c("country", "gleam_region", "dressing_percent")
    country_data <- country_data |>
      dplyr::filter(!is.na(country)) |>
      dplyr::mutate(
        dressing_percent = as.numeric(dressing_percent),
        species = "Pigs",
        production_system = "Industrial",
        cohort = NA_character_
      ) |>
      dplyr::select(
        species,
        production_system,
        cohort,
        country,
        gleam_region,
        dressing_percent
      )
  } else {
    country_data <- tibble::tibble(
      species = character(),
      production_system = character(),
      cohort = character(),
      country = character(),
      gleam_region = character(),
      dressing_percent = numeric()
    )
  }

  # Add country column to regional (NA for all)
  regional <- regional |>
    dplyr::mutate(country = NA_character_) |>
    dplyr::select(
      species,
      production_system,
      cohort,
      country,
      gleam_region,
      dressing_percent
    )

  dplyr::bind_rows(regional, country_data)
}

.parse_species_header <- function(label) {
  if (stringr::str_detect(label, "(?i)dairy cattle")) {
    list(species = "Cattle", system = "Dairy")
  } else if (stringr::str_detect(label, "(?i)beef cattle")) {
    list(species = "Cattle", system = "Beef")
  } else {
    # Buffaloes, Pigs, Chicken — no system in header
    list(species = label, system = NA_character_)
  }
}

.parse_dressing_value <- function(x) {
  if (is.na(x) || x == "" || stringr::str_detect(x, "^-")) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x))
}

.parse_dressing_subrow <- function(
  label,
  vals,
  regions,
  species,
  system
) {
  # Determine if this is a cohort row or a production system row
  is_cohort <- stringr::str_detect(
    label,
    "(?i)(adult|replacement|surplus)"
  )
  is_system <- stringr::str_detect(
    label,
    "(?i)(backyard|intermediate|industrial|layers|broilers)"
  )

  # Skip header-only sub-rows (e.g. "Backyard systems" for chicken
  # that has no numeric values)
  all_na <- all(
    is.na(vals) | vals == "" | stringr::str_detect(vals, "^[A-Za-z]")
  )
  if (all_na) {
    return(NULL)
  }

  rows <- list()
  for (j in seq_along(regions)) {
    v <- .parse_dressing_value(vals[j])
    if (is_cohort) {
      rows <- c(
        rows,
        list(tibble::tibble(
          species = species,
          production_system = system,
          cohort = label,
          gleam_region = regions[j],
          dressing_percent = v
        ))
      )
    } else if (is_system) {
      rows <- c(
        rows,
        list(tibble::tibble(
          species = species,
          production_system = label,
          cohort = NA_character_,
          gleam_region = regions[j],
          dressing_percent = v
        ))
      )
    }
  }
  if (length(rows) > 0) dplyr::bind_rows(rows) else NULL
}

parse_crop_residue_params <- function(raw) {
  # Row 1 = title, Row 2 = column headers, Row 3+ = data
  data <- raw[-(1:2), , drop = FALSE]
  names(data) <- c("crop", "dry_matter_pct", "slope", "intercept")
  data |>
    dplyr::mutate(
      dry_matter_pct = as.numeric(dry_matter_pct),
      slope = as.numeric(slope),
      intercept = as.numeric(intercept)
    )
}

parse_feed_digestibility <- function(raw) {
  # Row 1 = title, Row 2 = headers, Row 3+ = data
  # Rows with only col1 populated are category headers
  # (Roughages, Cereals, By-products)
  # Last row is a footnote
  data <- raw[-(1:2), , drop = FALSE]
  names(data) <- c(
    "number",
    "material",
    "gross_energy_mj_kg",
    "n_content_g_kg",
    "digestibility_pct"
  )

  # Remove category headers and footnotes
  data |>
    dplyr::filter(
      !is.na(number),
      !stringr::str_detect(number, "(?i)(roughage|cereal|by-product|\\*)")
    ) |>
    dplyr::mutate(
      # Assign feed category based on position
      feed_category = dplyr::case_when(
        as.numeric(number) <= 15 ~
          dplyr::case_when(
            as.numeric(number) <= 6 ~ "Roughages",
            as.numeric(number) <= 15 ~ "Roughages",
            .default = NA_character_
          ),
        .default = NA_character_
      )
    ) |>
    # Reassign categories properly using original row position
    dplyr::select(-feed_category) |>
    dplyr::mutate(
      number = as.integer(number),
      # Remove asterisks from values before converting
      gross_energy_mj_kg = as.numeric(
        stringr::str_remove(gross_energy_mj_kg, "\\*")
      ),
      n_content_g_kg = as.numeric(
        stringr::str_remove(n_content_g_kg, "\\*")
      ),
      digestibility_pct = as.numeric(
        stringr::str_remove(digestibility_pct, "\\*")
      )
    )
}

parse_feed_composition <- function(raw) {
  # Row 1 = title, Row 2 = "FUE" header
  # Row 3: feed group description + column headers
  # Rows 4-10: regions for feed groups 1-6
  # Row 11: second feed group description
  # Rows 12-18: regions for feed groups 9-15

  data <- raw[-(1:2), , drop = FALSE] # skip title + "FUE"
  names(data) <- c("description", "col2", "col3")

  rows <- list()

  # Section 1: feed groups 1-6 (mixed grasses, crop residues)
  sec1_header <- as.character(data[1, ])
  sec1_feeds <- stringr::str_trim(c(sec1_header[2], sec1_header[3]))
  sec1_data <- data[2:8, ]
  for (i in seq_len(nrow(sec1_data))) {
    region <- as.character(sec1_data[i, 1])
    for (j in seq_along(sec1_feeds)) {
      feed <- sec1_feeds[j]
      if (is.na(feed) || feed == "") {
        next
      }
      val <- as.numeric(as.character(sec1_data[i, j + 1]))
      rows <- c(
        rows,
        list(tibble::tibble(
          feed_group = "Feed materials 1-6",
          feed_type = feed,
          gleam_region = region,
          feed_use_efficiency = val
        ))
      )
    }
  }

  # Section 2: feed groups 9-15
  sec2_header <- as.character(data[9, ])
  sec2_feeds <- stringr::str_trim(sec2_header[2])
  sec2_data <- data[10:16, ]
  for (i in seq_len(nrow(sec2_data))) {
    region <- as.character(sec2_data[i, 1])
    val <- as.numeric(as.character(sec2_data[i, 2]))
    rows <- c(
      rows,
      list(tibble::tibble(
        feed_group = "Feed materials 9-15",
        feed_type = sec2_feeds,
        gleam_region = region,
        feed_use_efficiency = val
      ))
    )
  }

  dplyr::bind_rows(rows)
}

parse_feed_conversion_ratios <- function(raw) {
  # Row 1 = title, Row 2 = headers, Row 3 = sub-headers
  # Row 4+ = data with category headers interspersed
  data <- raw[-(1:3), , drop = FALSE] # skip title + both headers
  names(data) <- c(
    "number",
    "material",
    "gross_energy_j_kg",
    "n_content_g_kg",
    "me_chicken_j_kg",
    "me_pigs_j_kg",
    "digestibility_pct"
  )

  # Remove category header rows (no number) and footnotes
  data |>
    dplyr::filter(
      !is.na(number),
      stringr::str_detect(number, "^\\d+$")
    ) |>
    dplyr::mutate(
      number = as.integer(number),
      gross_energy_j_kg = as.numeric(gross_energy_j_kg),
      n_content_g_kg = as.numeric(n_content_g_kg),
      me_chicken_j_kg = as.numeric(me_chicken_j_kg),
      me_pigs_j_kg = as.numeric(me_pigs_j_kg),
      digestibility_pct = as.numeric(digestibility_pct)
    )
}

# GLEAM Data Extraction ----

extract_gleam_tables <- function(path) {
  message("Extracting GLEAM tables from: ", path)

  temp_file <- tempfile(fileext = ".xlsx")
  file.copy(path, temp_file)
  on.exit(unlink(temp_file))

  tryCatch(
    {
      sheets <- openxlsx::getSheetNames(temp_file)
      tables <- list()

      for (sheet in sheets) {
        if (sheet == "Table of contents") {
          next
        }
        tryCatch(
          {
            # Read raw without headers — row 1 is the title,
            # row 2 is headers, row 3+ is data.
            df <- openxlsx::read.xlsx(
              temp_file,
              sheet = sheet,
              colNames = FALSE
            )
            clean_sheet <- clean_names(sheet)
            tables[[clean_sheet]] <- df
            message("  Extracted: ", sheet)
          },
          error = function(e) {
            warning("  Failed to extract ", sheet, ": ", e$message)
          }
        )
      }
      tables
    },
    error = function(e) {
      warning("Failed to open GLEAM file: ", e$message)
      list()
    }
  )
}

# Strip the title row, use row 2 as headers, return data
# from row 3 onward. Removes footnote rows where all value
# columns are NA.
.gleam_skip_title <- function(raw, col_names = NULL) {
  if (is.null(col_names)) {
    col_names <- clean_names(as.character(raw[2, ]))
  }
  data <- raw[-(1:2), , drop = FALSE]
  names(data) <- col_names
  data
}

# S.6.1 + S.6.2: field operation emission factors
parse_field_operation_ef <- function(raw_ruminant, raw_monogastric) {
  .parse_one <- function(raw, species_group) {
    df <- .gleam_skip_title(
      raw,
      c("material_number", "material", "emission_factor_kg_co2eq_ha")
    )
    # Remove category header rows (material col is NA)
    # and footnote rows
    df <- df[!is.na(df$material), ]
    df$species_group <- species_group
    df |>
      dplyr::mutate(
        material_number = as.integer(material_number),
        emission_factor_kg_co2eq_ha = as.numeric(
          emission_factor_kg_co2eq_ha
        )
      )
  }
  dplyr::bind_rows(
    .parse_one(raw_ruminant, "ruminant"),
    .parse_one(raw_monogastric, "monogastric")
  )
}

# S.6.3 + S.6.4: mechanization levels by country and feed
parse_mechanization_levels <- function(raw_ruminant, raw_monogastric) {
  .parse_one <- function(raw, species_group) {
    headers <- clean_names(as.character(raw[2, ]))
    df <- raw[-(1:2), , drop = FALSE]
    names(df) <- headers
    # Pivot feed material columns to long format
    key_cols <- c("country", "continent", "region")
    feed_cols <- setdiff(names(df), key_cols)
    df |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(feed_cols),
        names_to = "feed_material",
        values_to = "mechanization_level"
      ) |>
      dplyr::mutate(
        mechanization_level = as.numeric(mechanization_level),
        species_group = species_group
      )
  }
  dplyr::bind_rows(
    .parse_one(raw_ruminant, "ruminant"),
    .parse_one(raw_monogastric, "monogastric")
  )
}

# S.6.5 + S.6.6: processing and transport emission factors
parse_processing_transport_ef <- function(raw_ruminant, raw_monogastric) {
  .parse_one <- function(raw, species_group) {
    df <- .gleam_skip_title(
      raw,
      c(
        "material_number",
        "material",
        "processing_g_co2eq_kg_dm",
        "transport_g_co2eq_kg_dm"
      )
    )
    # Remove category headers, footnotes, and blank rows
    df <- df[!is.na(df$material), ]
    df <- df[!grepl("^\\*", df$material_number), ]
    df$species_group <- species_group
    # Strip asterisks from footnoted values before parsing
    df |>
      dplyr::mutate(
        material_number = as.integer(material_number),
        processing_g_co2eq_kg_dm = as.numeric(
          stringr::str_remove(processing_g_co2eq_kg_dm, "\\*")
        ),
        transport_g_co2eq_kg_dm = as.numeric(
          stringr::str_remove(transport_g_co2eq_kg_dm, "\\*")
        )
      )
  }
  dplyr::bind_rows(
    .parse_one(raw_ruminant, "ruminant"),
    .parse_one(raw_monogastric, "monogastric")
  )
}

# S.6.7 + S.6.8: nitrogen from crop residues
parse_crop_residue_nitrogen <- function(raw_ruminant, raw_monogastric) {
  .parse_one <- function(raw, species_group) {
    df <- .gleam_skip_title(
      raw,
      c("material_number", "material", "n_ag", "rbg_bio", "n_bg")
    )
    df <- df[!is.na(df$material), ]
    df$species_group <- species_group
    df |>
      dplyr::mutate(
        material_number = as.integer(material_number),
        n_ag = as.numeric(n_ag),
        rbg_bio = as.numeric(rbg_bio),
        n_bg = as.numeric(n_bg)
      )
  }
  dplyr::bind_rows(
    .parse_one(raw_ruminant, "ruminant"),
    .parse_one(raw_monogastric, "monogastric")
  )
}

# S.6.9: FracReMove by country
parse_fracremove <- function(raw) {
  df <- .gleam_skip_title(
    raw,
    c("country", "continent", "region", "fracremove")
  )
  df |>
    dplyr::filter(!is.na(country)) |>
    dplyr::mutate(fracremove = as.numeric(fracremove))
}

# Convert GLEAM middle-dot scientific notation
# (e.g. "4.75·10-2") to standard form.
.parse_gleam_numeric <- function(x) {
  # Replace middle-dot notation: "X·10-Y" -> "Xe-Y"
  x <- stringr::str_replace(
    x,
    "\u00b710(-?\\d+)",
    "e\\1"
  )
  as.numeric(x)
}

# Remove footnote rows (start with "a " or "Note")
.drop_footnotes <- function(df, col = 1L) {
  vals <- df[[col]]
  keep <- !is.na(vals) & !grepl("^a\\s|^Note", vals)
  df[keep, , drop = FALSE]
}

# Footnote/derived rows: copy `df`, scale the EF, relabel herd (and species).
# Base assignment avoids the data-mask shadowing of `mutate(herd = herd)`.
.energy_scaled_rows <- function(df, factor, herd, species = NULL) {
  df$emission_factor <- df$emission_factor * factor
  df$herd <- herd
  if (!is.null(species)) {
    df$species <- species
  }
  df
}

.norm_gleam_system <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "(?i)grassland") ~ "grassland_based",
    stringr::str_detect(x, "(?i)mixed") ~ "mixed_farming",
    .default = x
  )
}

# S.7.1-S.7.7: energy use emission factors.
#
# Re-ingested cleanly (issue #105): every species is captured at both the
# embedded and direct stages, and each row is tagged with its grouping scheme,
# production system, climate zone, energy stage and reporting `denominator`
# (live weight, milk or egg) so downstream code never has to guess the basis.
# The GLEAM footnotes are materialised as derived rows: meat (non-dairy) cattle
# and all buffalo embedded energy is half of dairy cattle (S.7.1 note a);
# non-dairy small ruminant embedded energy is half of the listed values (S.7.2
# note a); dairy small ruminant direct energy is double the dairy cattle values
# (S.7.5 note a).
parse_energy_use_ef <- function(gleam_raw) {
  rows <- list()

  # S.7.1 embedded, dairy cattle (kg CO2-eq / kg LW): grouping x system x clim.
  df <- .gleam_skip_title(
    gleam_raw$tab_s71,
    c("grouping", "system", "arid", "humid", "temperate")
  )
  df <- df[!is.na(df$grouping) | !is.na(df$system), ]
  df$grouping <- zoo::na.locf(df$grouping, na.rm = FALSE)
  df <- .drop_footnotes(df, "grouping")
  s71 <- df |>
    tidyr::pivot_longer(
      cols = c("arid", "humid", "temperate"),
      names_to = "climate",
      values_to = "emission_factor"
    ) |>
    dplyr::mutate(
      species = "cattle",
      herd = "dairy",
      grouping_scheme = "development3",
      system = .norm_gleam_system(system),
      energy_type = "embedded",
      denominator = "lw",
      emission_factor = .parse_gleam_numeric(emission_factor)
    )
  rows <- c(
    rows,
    list(
      s71,
      .energy_scaled_rows(s71, 0.5, "non_dairy"),
      .energy_scaled_rows(s71, 0.5, "all", species = "buffalo")
    )
  )

  # S.7.2 embedded, small ruminants (kg CO2-eq / kg LW): grouping x climate.
  df <- .gleam_skip_title(
    gleam_raw$tab_s72,
    c("grouping", "arid", "humid", "temperate")
  )
  df <- .drop_footnotes(df, "grouping")
  s72 <- df |>
    tidyr::pivot_longer(
      cols = c("arid", "humid", "temperate"),
      names_to = "climate",
      values_to = "emission_factor"
    ) |>
    dplyr::mutate(
      species = "small_ruminants",
      herd = "dairy",
      grouping_scheme = "development3",
      system = NA_character_,
      energy_type = "embedded",
      denominator = "lw",
      emission_factor = .parse_gleam_numeric(emission_factor)
    )
  rows <- c(rows, list(s72, .energy_scaled_rows(s72, 0.5, "non_dairy")))

  # S.7.3 embedded, pigs (kg CO2-eq / kg LW): grouping x system.
  df <- .gleam_skip_title(
    gleam_raw$tab_s73,
    c("grouping", "industrial", "intermediate", "backyard")
  )
  df <- .drop_footnotes(df, "grouping")
  rows <- c(
    rows,
    list(
      df |>
        tidyr::pivot_longer(
          cols = c("industrial", "intermediate", "backyard"),
          names_to = "system",
          values_to = "emission_factor"
        ) |>
        dplyr::mutate(
          species = "pigs",
          herd = NA_character_,
          grouping_scheme = "region5",
          climate = NA_character_,
          energy_type = "embedded",
          denominator = "lw",
          emission_factor = .parse_gleam_numeric(emission_factor)
        )
    )
  )

  # S.7.4 embedded, chickens: broilers (kg / kg LW), layers (kg / kg egg).
  df <- .gleam_skip_title(
    gleam_raw$tab_s74,
    c("grouping", "broilers", "layers")
  )
  df <- .drop_footnotes(df, "grouping")
  rows <- c(
    rows,
    list(
      df |>
        tidyr::pivot_longer(
          cols = c("broilers", "layers"),
          names_to = "herd",
          values_to = "emission_factor"
        ) |>
        dplyr::mutate(
          species = "chickens",
          grouping_scheme = "region5",
          system = NA_character_,
          climate = NA_character_,
          energy_type = "embedded",
          denominator = dplyr::if_else(herd == "layers", "egg", "lw"),
          emission_factor = .parse_gleam_numeric(emission_factor)
        )
    )
  )

  # S.7.5 direct, dairy cattle & buffalo (kg CO2-eq / kg MILK): two-row header
  # (system x climate). Footnote: dairy small ruminants assumed double.
  col_names <- c(
    "grouping",
    "grassland_based_arid",
    "grassland_based_humid",
    "grassland_based_temperate",
    "mixed_farming_arid",
    "mixed_farming_humid",
    "mixed_farming_temperate"
  )
  df <- gleam_raw$tab_s75[-(1:3), , drop = FALSE]
  names(df) <- col_names
  df <- df[!is.na(df$grouping), ]
  df <- df[!grepl("^a\\s|^Note", df$grouping), ]
  s75 <- df |>
    tidyr::pivot_longer(
      cols = -"grouping",
      names_to = "sys_clim",
      values_to = "emission_factor"
    ) |>
    tidyr::separate_wider_regex(
      sys_clim,
      c(system = "grassland_based|mixed_farming", "_", climate = ".*")
    ) |>
    dplyr::mutate(
      species = "cattle",
      herd = "dairy",
      grouping_scheme = "detailed15",
      energy_type = "direct",
      denominator = "milk",
      emission_factor = .parse_gleam_numeric(emission_factor)
    )
  rows <- c(
    rows,
    list(
      s75,
      .energy_scaled_rows(s75, 1, "dairy", species = "buffalo"),
      .energy_scaled_rows(s75, 2, "dairy", species = "small_ruminants")
    )
  )

  # S.7.6 direct, non-dairy ruminants (kg CO2-eq / kg LW).
  df <- .gleam_skip_title(gleam_raw$tab_s76, NULL)
  names(df) <- c(
    "grouping",
    "large_ruminants_grassland_based",
    "large_ruminants_mixed_farming",
    "small_ruminants"
  )
  df <- .drop_footnotes(df, "grouping")
  rows <- c(
    rows,
    list(
      df |>
        tidyr::pivot_longer(
          cols = -"grouping",
          names_to = "key",
          values_to = "emission_factor"
        ) |>
        dplyr::mutate(
          species = dplyr::if_else(
            stringr::str_detect(key, "small"),
            "small_ruminants",
            "large_ruminants"
          ),
          system = dplyr::case_when(
            stringr::str_detect(key, "grassland") ~ "grassland_based",
            stringr::str_detect(key, "mixed") ~ "mixed_farming",
            .default = NA_character_
          ),
          herd = "non_dairy",
          grouping_scheme = "detailed15",
          climate = NA_character_,
          energy_type = "direct",
          denominator = "lw",
          emission_factor = .parse_gleam_numeric(emission_factor)
        ) |>
        dplyr::select(-"key")
    )
  )

  # S.7.7 direct, monogastrics: pigs (kg / kg LW), layers (kg / kg egg),
  # broilers (kg / kg LW). Two-row header.
  df <- gleam_raw$tab_s77[-(1:3), , drop = FALSE]
  names(df) <- c(
    "grouping",
    "pigs_intermediate",
    "pigs_industrial",
    "chickens_layers",
    "chickens_broilers"
  )
  df <- .drop_footnotes(df, "grouping")
  rows <- c(
    rows,
    list(
      df |>
        tidyr::pivot_longer(
          cols = -"grouping",
          names_to = "key",
          values_to = "emission_factor"
        ) |>
        tidyr::separate_wider_regex(
          key,
          c(species = "pigs|chickens", "_", herd_sys = ".*")
        ) |>
        dplyr::mutate(
          system = dplyr::if_else(species == "pigs", herd_sys, NA_character_),
          herd = dplyr::if_else(species == "chickens", herd_sys, NA_character_),
          grouping_scheme = "detailed15",
          climate = NA_character_,
          energy_type = "direct",
          denominator = dplyr::if_else(
            species == "chickens" & herd == "layers",
            "egg",
            "lw"
          ),
          emission_factor = .parse_gleam_numeric(emission_factor)
        ) |>
        dplyr::select(-"herd_sys")
    )
  )

  dplyr::bind_rows(rows) |>
    dplyr::filter(!is.na(emission_factor)) |>
    dplyr::select(
      "species",
      "herd",
      "grouping",
      "grouping_scheme",
      "system",
      "climate",
      "energy_type",
      "denominator",
      "emission_factor"
    ) |>
    dplyr::arrange(species, herd, energy_type, grouping, system, climate)
}

# S.A1–S.A2: geographic hierarchy
parse_geographic_hierarchy <- function(raw) {
  df <- .gleam_skip_title(
    raw,
    c(
      "iso3",
      "country",
      "continent",
      "faostat_region",
      "gleam_region",
      "eu27",
      "oecd"
    )
  )
  df |>
    dplyr::filter(!is.na(country)) |>
    dplyr::mutate(
      eu27 = as.integer(eu27),
      oecd = as.integer(oecd)
    ) |>
    correct_gleam_oecd_flags() |>
    add_present_day_polity()
}

# The one cell of S.A1-S.A2 this file does not carry through (whep#574).
#
# Cell G41 of `Tab. S.A1-S.A2` holds `OECD = 1` for Comoros. The Comoros is not
# an OECD member. The OECD's own membership page
# <https://www.oecd.org/en/about/members-partners.html> (read 2026-08-25) names
# the 38 Members, Australia through the United States, and Comoros appears on
# none of its three lists -- not a Member, not an accession candidate, not a
# Key Partner.
#
# That the column is meant as literal membership, not a GLEAM grouping that
# merely borrows the name, is settled by the rest of the column: the other 38
# flagged iso3 codes are EXACTLY the 38 real Members, Colombia (2020) and Costa
# Rica (2021) included, and the `EU27` column beside it is exactly the 27 real
# EU members. A modelling grouping would not reproduce the real membership
# 38-for-38 and then add one Indian Ocean island.
#
# The mechanism looks like a single-cell spill, not the column shift of
# whep#855: every other field on the Comoros row is right, the cell is a
# hand-typed literal (no formula), and the row sits IMMEDIATELY BELOW Colombia
# (G40 = 1), whose accession is one of the two the sheet was evidently updated
# for. So the whole `oecd` column is corrected here, by set equality against the
# published membership, rather than one cell being patched: `test_datasets.R`
# pins the same equality, so a coefficient refresh that reintroduces the error
# -- or a real accession -- fails the suite instead of passing silently.
correct_gleam_oecd_flags <- function(hierarchy) {
  members <- oecd_member_iso3()
  missing <- setdiff(members, hierarchy$iso3)
  if (length(missing) > 0) {
    stop(
      "GLEAM S.A1-S.A2 has no row for OECD member(s): ",
      paste(missing, collapse = ", "),
      ". The energy extension assumes every member is listed; ",
      "see whep#574.",
      call. = FALSE
    )
  }
  dplyr::mutate(hierarchy, oecd = as.integer(iso3 %in% members))
}

# The 38 OECD Members, as ISO3, from the OECD's own membership list
# <https://www.oecd.org/en/about/members-partners.html> (read 2026-08-25).
# Kept as one exported-looking helper because `test_datasets.R` asserts the
# shipped `oecd` column equals exactly this set; update it when a country
# accedes, and the dataset follows on the next rebuild.
oecd_member_iso3 <- function() {
  c(
    "AUS", # Australia
    "AUT", # Austria
    "BEL", # Belgium
    "CAN", # Canada
    "CHL", # Chile
    "COL", # Colombia
    "CRI", # Costa Rica
    "CZE", # Czechia
    "DNK", # Denmark
    "EST", # Estonia
    "FIN", # Finland
    "FRA", # France
    "DEU", # Germany
    "GRC", # Greece
    "HUN", # Hungary
    "ISL", # Iceland
    "IRL", # Ireland
    "ISR", # Israel
    "ITA", # Italy
    "JPN", # Japan
    "KOR", # Korea
    "LVA", # Latvia
    "LTU", # Lithuania
    "LUX", # Luxembourg
    "MEX", # Mexico
    "NLD", # Netherlands
    "NZL", # New Zealand
    "NOR", # Norway
    "POL", # Poland
    "PRT", # Portugal
    "SVK", # Slovak Republic
    "SVN", # Slovenia
    "ESP", # Spain
    "SWE", # Sweden
    "CHE", # Switzerland
    "TUR", # Turkiye
    "GBR", # United Kingdom
    "USA" # United States
  )
}

# The present-day polity of each country GLEAM lists (whep#688).
#
# The table is GLEAM's registry of the countries that exist today -- it carries
# South Sudan, independent since 2011, and no dissolved entity at all -- so
# `polity_identity_conventions()` types it `present_day_polity`, the same
# reading and the same route as `regions_full`. Without the column every
# consumer resolved a polity ad hoc and joined on the bare `iso3`, which is
# year-less: 38 of the 204 iso3 values name a DIFFERENT polity at 1961 than at
# 2010, so an unyeared join was silently picking one of them.
#
# `whep:::.present_day_polity_year()` is the year "today" means for a label,
# derived from the shipped `polities` snapshot; the register's `resolver`
# column names this call and `test_territorial_identity.R` recomputes it, so
# this table cannot drift from the resolver without the suite failing.
#
# All 204 resolve as of the whep-polities 2830fb7 re-sync. Three used to keep NA
# rather than being dropped or guessed at -- ATF (French Southern and Antarctic
# Territories), SGS (South Georgia and the South Sandwich Islands) and WLF
# (Wallis and Futuna) sat in whep-polities' own `registry_unmapped.csv` as
# "registry area with no polity family", and proposing a polity was upstream's
# call, not this script's. Upstream made it in whep-polities#187, so the three
# now carry `ATF-1800-2025`, `SGS-1800-2025` and `WLF-1800-2025`. That is the
# only cell in this file the re-sync moved, and the equality in
# `test_territorial_identity.R` is what caught the table going stale.
add_present_day_polity <- function(hierarchy) {
  if (!requireNamespace("whep", quietly = TRUE)) {
    stop(
      "whep must be loaded (devtools::load_all()) to resolve polity codes.",
      call. = FALSE
    )
  }
  codes <- whep::resolve_polity_label(
    hierarchy$iso3,
    year = whep:::.present_day_polity_year()
  )
  # `polities` is an sf data frame and sf is only suggested, so the two
  # attribute columns are taken by name rather than through `st_drop_geometry`.
  names <- tibble::tibble(
    reporting_polity_code = whep::polities$polity_code,
    reporting_polity_name = whep::polities$polity_name
  ) |>
    dplyr::distinct()
  hierarchy |>
    dplyr::mutate(reporting_polity_code = codes) |>
    dplyr::left_join(names, by = "reporting_polity_code")
}

# GLEAM PDF Tables ----

# Despite the section name none of these is read from a GLEAM PDF. whep#881
# traced each one; the verdict and the numeric consequence live in the @source
# blocks in R/livestock_coefs.R. In short:
# - gleam_animal_weights: GLEAM 2.0 Supplement S1, live-weight block of
#   Tables 2.4-2.16, publishes the real values and they differ. NOT replaced
#   here: it is the Tier 2 live weight, so a re-ingest moves gross energy and
#   enteric CH4 by -16% to +14% depending on cohort. Maintainer decision.
# - gleam_mms_shares: GLEAM 2.0 Supplement S1 Tables 4.2-4.11 publish the real
#   values and they differ. No consumer in R/, so not replaced.
# - gleam_livestock_categories: the cohort vocabulary matches GLEAM Table 2.1,
#   the Dairy/Beef layout and the equal 1/n cohort split it induces do not.
# - gleam_milk_production, gleam_feed_categories: GLEAM publishes no such
#   table. Still unverified placeholders; no consumer in R/.
# Do not "fix" a value here without a citation to the table it comes from.

generate_gleam_pdf_tables <- function() {
  list(
    gleam_livestock_categories = tibble::tribble(
      ~species, ~production_system, ~cohort, ~description,
      "Cattle", "Dairy", "Adult Female",
        "Milking cows",
      "Cattle", "Dairy", "Adult Male",
        "Bulls",
      "Cattle", "Dairy", "Replacement Female",
        "Heifers",
      "Cattle", "Dairy", "Replacement Male",
        "Young bulls",
      "Cattle", "Dairy", "Surplus Female",
        "Culled heifers",
      "Cattle", "Dairy", "Surplus Male",
        "Calves for meat",
      "Cattle", "Beef", "Adult Female",
        "Breeding cows",
      "Cattle", "Beef", "Adult Male",
        "Bulls",
      "Cattle", "Beef", "Replacement Female",
        "Heifers",
      "Cattle", "Beef", "Replacement Male",
        "Young bulls",
      "Cattle", "Beef", "Fattening",
        "Fattening cattle",
      "Buffalo", "Dairy", "Adult Female",
        "Milking buffalo",
      "Buffalo", "Dairy", "Replacement",
        "Young buffalo",
      "Buffalo", "Other", "Adult",
        "Draft buffalo",
      "Sheep", "Dairy", "Adult Female",
        "Milking ewes",
      "Sheep", "Dairy", "Replacement",
        "Young ewes",
      "Sheep", "Meat", "Adult Female",
        "Breeding ewes",
      "Sheep", "Meat", "Fattening",
        "Lambs",
      "Goats", "Dairy", "Adult Female",
        "Milking goats",
      "Goats", "Dairy", "Replacement",
        "Young goats",
      "Goats", "Meat", "Adult Female",
        "Breeding goats",
      "Goats", "Meat", "Fattening",
        "Kids",
      "Pigs", "Breeding", "Sows",
        "Breeding sows",
      "Pigs", "Breeding", "Boars",
        "Breeding boars",
      "Pigs", "Fattening", "Fattening",
        "Fattening pigs",
      "Poultry", "Layers", "Layers",
        "Laying hens",
      "Poultry", "Broilers", "Broilers",
        "Meat chickens"
    ),

    gleam_feed_categories = tibble::tribble(
      ~feed_category, ~feed_type, ~description,
      "Grass",           "Pasture",    "Grazed grass and fodder",
      "Crop residues",   "Residues",   "Straw, stovers, husks",
      "Concentrates",    "Crops",      "Grains, oilseeds, pulses",
      "Fodder crops",    "Crops",      "Cultivated fodder",
      "Processed feeds", "Industrial", "Brans, meals, cakes",
      "Animal products", "Animal",     "Milk, fish meal"
    ),

    gleam_enteric_params = tibble::tribble(
      ~species,   ~system,   ~ym_percent, ~notes,
      "Cattle",   "Grazing", 6.5, "IPCC default",
      "Cattle",   "Mixed",   6.5, "IPCC default",
      "Cattle",   "Feedlot", 3.0, "High concentrate diet",
      "Buffalo",  "Grazing", 6.5, "IPCC default",
      "Buffalo",  "Mixed",   6.5, "IPCC default",
      "Sheep",    "Grazing", 6.5, "IPCC default",
      "Sheep",    "Mixed",   6.5, "IPCC default",
      "Goats",    "Grazing", 5.5, "IPCC default",
      "Goats",    "Mixed",   5.5, "IPCC default",
      "Pigs",     "All",     0.0, "Negligible enteric CH4"
    ),

    gleam_mms_shares = tibble::tribble(
      ~region, ~species, ~system, ~mms, ~share_percent,
      "Western Europe", "Cattle", "Dairy", "Liquid/Slurry", 60,
      "Western Europe", "Cattle", "Dairy", "Solid Storage", 30,
      "Western Europe", "Cattle", "Dairy", "Pasture", 10,
      "Western Europe", "Cattle", "Beef", "Pasture", 70,
      "Western Europe", "Cattle", "Beef", "Solid Storage", 30,
      "Sub-Saharan Africa", "Cattle", "All", "Pasture", 90,
      "Sub-Saharan Africa", "Cattle", "All",
        "Daily Spread", 10,
      "Latin America", "Cattle", "All", "Pasture", 95,
      "Latin America", "Cattle", "All", "Solid Storage", 5,
      "South Asia", "Cattle", "All", "Daily Spread", 60,
      "South Asia", "Cattle", "All", "Solid Storage", 30,
      "South Asia", "Cattle", "All", "Pasture", 10,
      "East Asia", "Pigs", "All", "Liquid/Slurry", 70,
      "East Asia", "Pigs", "All", "Solid Storage", 30
    ),

    gleam_animal_weights = tibble::tribble(
      ~region, ~species, ~system, ~cohort, ~weight_kg,
      "Western Europe", "Cattle", "Dairy",
        "Adult Female", 650,
      "Western Europe", "Cattle", "Dairy",
        "Adult Male", 1000,
      "Western Europe", "Cattle", "Beef",
        "Adult Female", 600,
      "Western Europe", "Cattle", "Beef",
        "Fattening", 400,
      "North America", "Cattle", "Dairy",
        "Adult Female", 680,
      "North America", "Cattle", "Dairy",
        "Adult Male", 1000,
      "North America", "Cattle", "Beef",
        "Adult Female", 550,
      "North America", "Cattle", "Beef",
        "Fattening", 450,
      "Sub-Saharan Africa", "Cattle", "All",
        "Adult Female", 250,
      "Sub-Saharan Africa", "Cattle", "All",
        "Adult Male", 350,
      "South Asia", "Cattle", "Dairy",
        "Adult Female", 350,
      "South Asia", "Buffalo", "Dairy",
        "Adult Female", 450,
      "Latin America", "Cattle", "Beef",
        "Adult Female", 450,
      "Latin America", "Cattle", "Beef",
        "Fattening", 350,
      # Cattle (Global): Adult Female/Male and Fattening are GLEAM-consistent
      # global-average mature/finishing liveweights (project assumption,
      # bracketed by the explicit regional rows above: SSA 250 ... N.Am 680;
      # pending citation to a specific GLEAM-i liveweight table). Replacement and
      # Surplus cohorts are mature x growth fraction, a documented assumption
      # where GLEAM gives no per-cohort liveweight: Replacement = 0.6 x same-sex
      # adult (RF 240 = 0.6*400, RM 360 = 0.6*600); Surplus Female = culled
      # heifer ~0.5*AF (200); Surplus Male = "calves for meat", younger/lighter
      # ~0.4*AF (160) - so Surplus Male < Surplus Female is intentional. Cohort
      # names match gleam_livestock_categories so the energy-model weight join
      # resolves.
      "Global", "Cattle", "All", "Adult Female", 400,
      "Global", "Cattle", "All", "Adult Male", 600,
      "Global", "Cattle", "All", "Replacement Female", 240,
      "Global", "Cattle", "All", "Replacement Male", 360,
      "Global", "Cattle", "All", "Surplus Female", 200,
      "Global", "Cattle", "All", "Surplus Male", 160,
      "Global", "Cattle", "All", "Fattening", 300,
      # Buffalo (Global): adult anchored on the South Asia dairy buffalo
      # liveweight (450 kg); Replacement = 0.6 x adult; draft Adult slightly
      # heavier.
      "Global", "Buffalo", "All", "Adult Female", 450,
      "Global", "Buffalo", "All", "Replacement", 270,
      "Global", "Buffalo", "All", "Adult", 480,
      # Sheep / Goats (Global): adult is the GLEAM global average; cohort names
      # harmonised to gleam_livestock_categories (Adult Female / Replacement /
      # Fattening) so the weight join resolves. Replacement / Fattening are
      # mature x growth fraction (lighter young / finishing animals).
      "Global", "Sheep", "All", "Adult Female", 45,
      "Global", "Sheep", "All", "Replacement", 30,
      "Global", "Sheep", "All", "Fattening", 25,
      "Global", "Goats", "All", "Adult Female", 40,
      "Global", "Goats", "All", "Replacement", 26,
      "Global", "Goats", "All", "Fattening", 22,
      # Pigs (Global): monogastric demand uses Bouwman FCR, not the energy
      # model; weights retained for Tier-2 manure/enteric coverage.
      "Global", "Pigs", "Fattening", "Fattening", 50,
      "Global", "Pigs", "Breeding", "Sows", 200
    ),

    gleam_milk_production = tibble::tribble(
      ~region, ~species, ~system,
        ~milk_kg_head_yr, ~lactation_days,
      "Western Europe", "Cattle", "Dairy",   7500, 305,
      "North America",  "Cattle", "Dairy",   9500, 305,
      "Oceania",        "Cattle", "Dairy",   5500, 270,
      "Latin America",  "Cattle", "Dairy",   2500, 240,
      "Sub-Saharan Africa", "Cattle", "Dairy", 800, 180,
      "South Asia",     "Cattle", "Dairy",   1500, 240,
      "South Asia",     "Buffalo", "Dairy",  1800, 270,
      "Western Europe", "Sheep", "Dairy",     200, 180,
      "Western Europe", "Goats", "Dairy",     450, 240
    )
  )
}

# IPCC 2019 Refinement Tables ----

generate_ipcc_2019_tables <- function() {
  list(
    # Tier 1 enteric fermentation EF, cattle (kg CH4/head/yr). Regional
    # cattle factors are Table 10.11 in BOTH editions; Table 10.10 is the
    # non-cattle table in both, so the element names here follow the
    # published numbering rather than the reverse.
    # Source: predominantly the 2006 Guidelines, Vol 4, Ch 10, Table 10.11 --
    # NOT the 2019 Refinement's Table 10.11 (Updated), which gives 138/64 for
    # North America and 126/52 for Western Europe. Oceania dairy 90, Middle
    # East dairy 63 and Indian Subcontinent 68/47 match neither edition, and
    # the Global fallback row 80/47 is in no IPCC table (assumed,
    # unverified). Per-cell detail in `?ipcc_2019_enteric_ef_cattle`;
    # the revalue decision is #601.
    table_10_11_cattle = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr,
      "North America",        "Dairy Cattle",  128,
      "North America",        "Other Cattle",   53,
      "Western Europe",       "Dairy Cattle",  117,
      "Western Europe",       "Other Cattle",   57,
      "Eastern Europe",       "Dairy Cattle",   99,
      "Eastern Europe",       "Other Cattle",   58,
      "Oceania",              "Dairy Cattle",   90,
      "Oceania",              "Other Cattle",   60,
      "Latin America",        "Dairy Cattle",   72,
      "Latin America",        "Other Cattle",   56,
      "Asia",                 "Dairy Cattle",   68,
      "Asia",                 "Other Cattle",   47,
      "Africa",               "Dairy Cattle",   46,
      "Africa",               "Other Cattle",   31,
      "Middle East",          "Dairy Cattle",   63,
      "Middle East",          "Other Cattle",   31,
      "Indian Subcontinent",  "Dairy Cattle",   68,
      "Indian Subcontinent",  "Other Cattle",   47,
      "Global",               "Dairy Cattle",   80,
      "Global",               "Other Cattle",   47
    ),

    # Tier 1 enteric fermentation EF, non-cattle species. Table 10.10 in
    # both editions.
    # Source: the 2006 Guidelines, Vol 4, Ch 10, Table 10.10,
    # developed-countries column -- NOT the 2019 Refinement, whose Table
    # 10.10 (Updated) splits sheep 9/5, goats 9/5 and swine 1.5/1.0 by
    # productivity system and moves buffalo into the regional Table 10.11.
    # Poultry 0 is a project choice; both editions say "insufficient data
    # for calculation". See `?ipcc_2019_enteric_ef_other` and #601.
    table_10_10_other = tibble::tribble(
      ~category,             ~ef_kg_head_yr,
      "Buffalo",              55,
      "Sheep",                 8,
      "Goats",                 5,
      "Camels",               46,
      "Horses",               18,
      "Mules and Asses",      10,
      "Swine - Market",        1.5,
      "Swine - Breeding",      1.5,
      "Poultry",               0
    ),

    # Tier 1 manure management CH4 EF, cattle (kg CH4/head/yr).
    # Source: UNKNOWN AND UNVERIFIED. Not the 2019 Refinement: its Table
    # 10.14 (Updated) is g CH4 per kg VS by productivity class and ten
    # climate zones, and that edition publishes no per-head Tier 1 manure
    # CH4 table for cattle at all. The per-head shape is the 2006
    # Guidelines' Table 10.14, but the values are not that table's either --
    # North American dairy cattle 27/42/60 against 48/78/112, Latin American
    # dairy cattle 47 against 2, African dairy cattle 31 against 1. Do not
    # add a citation here until the real provenance is established; #601.
    table_10_14_cattle = tibble::tribble(
      ~region, ~category, ~climate, ~ef_kg_head_yr,
      "North America",   "Dairy Cattle", "Cool",      27,
      "North America",   "Dairy Cattle", "Temperate", 42,
      "North America",   "Dairy Cattle", "Warm",      60,
      "North America",   "Other Cattle", "Cool",       2,
      "North America",   "Other Cattle", "Temperate",  3,
      "North America",   "Other Cattle", "Warm",       4,
      "Western Europe",  "Dairy Cattle", "Cool",      31,
      "Western Europe",  "Dairy Cattle", "Temperate", 39,
      "Western Europe",  "Other Cattle", "Cool",       1,
      "Western Europe",  "Other Cattle", "Temperate",  1,
      "Eastern Europe",  "Dairy Cattle", "Cool",      21,
      "Eastern Europe",  "Dairy Cattle", "Temperate", 23,
      "Eastern Europe",  "Other Cattle", "Cool",       1,
      "Eastern Europe",  "Other Cattle", "Temperate",  1,
      "Oceania",         "Dairy Cattle", "Temperate", 24,
      "Oceania",         "Other Cattle", "Temperate",  1,
      "Latin America",   "Dairy Cattle", "Warm",      47,
      "Latin America",   "Other Cattle", "Warm",       1,
      "Africa",          "Dairy Cattle", "Warm",      31,
      "Africa",          "Other Cattle", "Warm",       1,
      "Middle East",     "Dairy Cattle", "Warm",      31,
      "Middle East",     "Other Cattle", "Warm",       1,
      "Asia",            "Dairy Cattle", "Warm",      39,
      "Asia",            "Other Cattle", "Warm",       2,
      "Global",          "Dairy Cattle", "Temperate", 36,
      "Global",          "Other Cattle", "Temperate",  2
    ),

    # Tier 1 manure management CH4 EF, non-cattle species.
    # Source: the 2006 Guidelines, Vol 4, Ch 10, Table 10.14 (buffalo,
    # swine) and Table 10.15 (the rest) -- NOT the 2019 Refinement, which
    # publishes no per-head Tier 1 manure CH4 table. The temperature column
    # varies by species: sheep 0.19 and goats 0.13 are developed-country
    # cool, horses 1.64, mules 0.90 and camels 1.92 developing-country
    # temperate. For buffalo 2 and swine 6 the source region is not
    # recorded and more than one cell of Table 10.14 carries each value.
    # See `?ipcc_2019_manure_ch4_ef_other` and #601.
    table_10_14_other = tibble::tribble(
      ~category,                ~climate, ~ef_kg_head_yr,
      "Buffalo",                "All",     2,
      "Sheep",                  "All",     0.19,
      "Goats",                  "All",     0.13,
      "Swine - Market",         "All",     6,
      "Swine - Breeding",       "All",     6,
      "Poultry - Broilers",     "All",     0.02,
      "Poultry - Layers",       "All",     0.03,
      "Horses",                 "All",     1.64,
      "Mules and Asses",        "All",     0.90,
      "Camels",                 "All",     1.92
    ),

    # Ym, methane conversion rate (% of gross energy).
    # Source: MIXED ACROSS EDITIONS.
    # - Sheep 6.7 and goats 5.5 are the 2019 Refinement, Vol 4, Ch 10,
    #   Table 10.13 (Updated). That table gives a SINGLE Ym for sheep
    #   "irrespective of feed quality", with no body-weight split (#250):
    #   there is no IPCC source for a <75kg/>=75kg distinction or a 4.7
    #   value. The 2006 Table 10.13 has no goat row and gives mature sheep
    #   6.5 and lambs under one year 4.5.
    # - Cattle and buffalo 6.5 pasture/range and mixed are the 2006
    #   Guidelines Table 10.12, which gives 6.5 for every non-feedlot
    #   cattle and buffalo class. The 2019 Refinement's Table 10.12
    #   (Updated) instead resolves them by production level and feed
    #   digestibility: 5.7, 6.0, 6.3, 6.5 for dairy cows by yield class,
    #   7.0 for >75 percent forage non-dairy, 6.3 for mixed rations, 4.0
    #   for grain feedlots and 3.0 for steam-flaked-corn feedlots. The
    #   feedlot 3.0 stored here is therefore the 2006 ">=90 percent
    #   concentrate" value.
    # - Camels 5.0 is in NO IPCC table (assumed, unverified). Both
    #   editions direct compilers to reuse the other-cattle or buffalo Ym
    #   for camels, i.e. 6.5.
    # See `?ipcc_2019_ym` and #601.
    table_10_12 = tibble::tribble(
      ~category, ~feed_situation, ~ym_percent, ~ym_uncertainty,
      "Cattle",  "Pasture/Range",          6.5, 1.0,
      "Cattle",  "Mixed",                  6.5, 1.0,
      "Cattle",  "Feedlot (>90% conc.)",   3.0, 1.0,
      "Buffalo", "Pasture/Range",          6.5, 1.0,
      "Buffalo", "Mixed",                  6.5, 1.0,
      "Sheep",   "All",                    6.7, 1.0,
      "Goats",   "All",                    5.5, 1.0,
      "Camels",  "All",                    5.0, 1.0
    ),

    # Bo, maximum CH4 producing capacity of manure (m3 CH4/kg VS).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16A (Updated).
    # The number is 10.16A, not 10.16: that edition has no Table 10.16 at
    # all, and 10.16 is the 2006 number for the deer/reindeer/rabbit/
    # fur-bearing manure CH4 table. The 2006 edition has no Bo table --
    # its defaults live in Annex 10A.2.
    # Note: dairy and other cattle have DIFFERENT Bo values. Other Cattle
    # 0.18 is the Western European non-dairy column (North America 0.19,
    # Eastern Europe and Oceania 0.17), and Swine - Market 0.45 the
    # non-North-American high-productivity column (North America 0.48).
    # Swine - Breeding 0.27 is in NEITHER edition (assumed, unverified):
    # Table 10.16A publishes one swine Bo and 2006 Annex 10A.2 gives
    # breeding swine the same Bo as market swine. 0.27 coincides with the
    # North American market-swine VOLATILE-SOLIDS rate of 0.27 kg VS per
    # head per day in that annex. See `?ipcc_2019_bo` and #601.
    table_10_16a = tibble::tribble(
      ~category,            ~bo_m3_kg_vs,
      "Dairy Cattle",        0.24,
      "Other Cattle",        0.18,
      "Buffalo",             0.10,
      "Swine - Market",      0.45,
      "Swine - Breeding",    0.27,
      "Sheep",               0.19,
      "Goats",               0.18,
      "Horses",              0.30,
      "Mules and Asses",     0.33,
      "Camels",              0.26,
      "Poultry - Layers",    0.39,
      "Poultry - Broilers",  0.36
    ),

    # MCF, methane conversion factor by manure system and climate class.
    # Source: predominantly the 2006 Guidelines, Vol 4, Ch 10, Table 10.17,
    # whose cool/temperate/warm structure this follows. It is the 2006
    # edition, not the 2019 Refinement, that resolves MCF per degree
    # Celsius; the Refinement's Table 10.17 (Updated) resolves ten climate
    # zones and liquid retention time, and differs in level -- a single
    # 0.47 percent for pasture/range/paddock against 1.0/1.5/2.0 here, and
    # 1.0/2.0/2.5 for static-pile and passive-windrow composting.
    # Where a 2006 row is per degree Celsius the value taken is not always
    # the mid-point of the class: uncovered anaerobic lagoon temperate 73
    # is the 14 degree column and the two liquid rows take 35 for
    # temperate, the 18 degree column rather than 42 at 20 degrees.
    # Cells matching NEITHER edition: dry lot 1.5/2.5/4.0, intensive-windrow
    # composting 0.5/0.5/0.5, passive-windrow composting 1.0/1.0/1.5, pit
    # storage under one month 3/3/5 and liquid/slurry with crust warm 47.
    # Anaerobic Digester 0 is also unpublished in both (assumed,
    # unverified). See `?ipcc_2019_mcf_manure` and #601.
    table_10_17 = tibble::tribble(
      ~system, ~climate_zone, ~mcf_percent,
      "Pasture/Range/Paddock",         "Cool",       1.0,
      "Pasture/Range/Paddock",         "Temperate",  1.5,
      "Pasture/Range/Paddock",         "Warm",       2.0,
      "Daily Spread",                  "Cool",       0.1,
      "Daily Spread",                  "Temperate",  0.5,
      "Daily Spread",                  "Warm",       1.0,
      "Solid Storage",                 "Cool",       2.0,
      "Solid Storage",                 "Temperate",  4.0,
      "Solid Storage",                 "Warm",       5.0,
      "Dry Lot",                       "Cool",       1.5,
      "Dry Lot",                       "Temperate",  2.5,
      "Dry Lot",                       "Warm",       4.0,
      "Liquid/Slurry - No Crust",      "Cool",      17.0,
      "Liquid/Slurry - No Crust",      "Temperate", 35.0,
      "Liquid/Slurry - No Crust",      "Warm",      80.0,
      "Liquid/Slurry - With Crust",    "Cool",      10.0,
      "Liquid/Slurry - With Crust",    "Temperate", 17.0,
      "Liquid/Slurry - With Crust",    "Warm",      47.0,
      "Uncovered Anaerobic Lagoon",    "Cool",      66.0,
      "Uncovered Anaerobic Lagoon",    "Temperate", 73.0,
      "Uncovered Anaerobic Lagoon",    "Warm",      80.0,
      "Pit Storage - <1 month",        "Cool",       3.0,
      "Pit Storage - <1 month",        "Temperate",  3.0,
      "Pit Storage - <1 month",        "Warm",       5.0,
      "Pit Storage - >1 month",        "Cool",      17.0,
      "Pit Storage - >1 month",        "Temperate", 35.0,
      "Pit Storage - >1 month",        "Warm",      80.0,
      "Anaerobic Digester",            "All",        0.0,
      "Burned for Fuel",               "All",       10.0,
      "Composting - In-vessel",        "Cool",       0.5,
      "Composting - In-vessel",        "Temperate",  0.5,
      "Composting - In-vessel",        "Warm",       0.5,
      "Composting - Static Pile",      "Cool",       0.5,
      "Composting - Static Pile",      "Temperate",  0.5,
      "Composting - Static Pile",      "Warm",       0.5,
      "Composting - Intensive",        "Cool",       0.5,
      "Composting - Intensive",        "Temperate",  0.5,
      "Composting - Intensive",        "Warm",       0.5,
      "Composting - Passive",          "Cool",       1.0,
      "Composting - Passive",          "Temperate",  1.0,
      "Composting - Passive",          "Warm",       1.5,
      "Poultry Manure - High Rise",    "Cool",       1.5,
      "Poultry Manure - High Rise",    "Temperate",  1.5,
      "Poultry Manure - High Rise",    "Warm",       1.5,
      "Poultry Manure - Deep Litter",  "Cool",       1.5,
      "Poultry Manure - Deep Litter",  "Temperate",  1.5,
      "Poultry Manure - Deep Litter",  "Warm",       1.5
    ),

    # Nitrogen excretion, stored as kg N per head per year -- which is
    # what `.calc_manure_n2o_tier1()` consumes.
    # Source: UNVERIFIED. Table 10.19 publishes a RATE, kg N per 1000 kg
    # animal mass per day, in both editions (dairy cattle North America
    # 0.59 in the 2019 Refinement, 0.44 in 2006), and these annual
    # per-head numbers do not follow from either. The Refinement's Table
    # 10A.1 (New) supplies the missing weight, so rate x weight x 365 is
    # fully sourced and gives 140 for North America against the 105 here,
    # 118 Western Europe (100), 84 Eastern Europe (80), 128 Oceania (80),
    # 72 Latin America (50), 62 Asia (50), 42 Africa (40), 64 Middle East
    # (40) and 68 Indian Subcontinent (50). Other cattle needs the cohort
    # population mix of Table 10A.2 (New) weighted the same way.
    # See `?ipcc_2019_n_excretion` and #601.
    table_10_19 = tibble::tribble(
      ~region, ~category, ~nex_kg_n_head_yr,
      "North America",        "Dairy Cattle",      105,
      "North America",        "Other Cattle",       56,
      "Western Europe",       "Dairy Cattle",      100,
      "Western Europe",       "Other Cattle",       50,
      "Eastern Europe",       "Dairy Cattle",       80,
      "Eastern Europe",       "Other Cattle",       50,
      "Oceania",              "Dairy Cattle",       80,
      "Oceania",              "Other Cattle",       40,
      "Latin America",        "Dairy Cattle",       50,
      "Latin America",        "Other Cattle",       40,
      "Africa",               "Dairy Cattle",       40,
      "Africa",               "Other Cattle",       30,
      "Middle East",          "Dairy Cattle",       40,
      "Middle East",          "Other Cattle",       30,
      "Asia",                 "Dairy Cattle",       50,
      "Asia",                 "Other Cattle",       40,
      "Indian Subcontinent",  "Dairy Cattle",       50,
      "Indian Subcontinent",  "Other Cattle",       40,
      "Global",               "Dairy Cattle",       70,
      "Global",               "Other Cattle",       40,
      "Global",               "Buffalo",            55,
      "Global",               "Sheep",              12,
      "Global",               "Goats",              12,
      "Global",               "Swine - Market",     15,
      "Global",               "Swine - Breeding",   18,
      "Global",               "Horses",             50,
      "Global",               "Mules and Asses",    35,
      "Global",               "Camels",             46,
      "Global",               "Poultry - Broilers",  0.6,
      "Global",               "Poultry - Layers",    0.8
    ),

    # EF3, direct N2O emission factors (kg N2O-N per kg N excreted).
    # Source: MIXED, and not consistently the 2019 Refinement's Table 10.21.
    # - Both editions: liquid/slurry with crust 0.005, in-vessel composting
    #   0.006, poultry with and without litter 0.001.
    # - 2006 only: solid storage 0.005 (2019: 0.010), static-pile
    #   composting 0.006 (0.010), passive-windrow composting 0.01 (0.005),
    #   anaerobic digester 0 (0.0006).
    # - Neither edition: daily spread 0.01, liquid/slurry without crust
    #   0.002 and uncovered anaerobic lagoon 0.001 (all three are 0 in
    #   both), dry lot 0.005 (0.02 in both), intensive-windrow composting
    #   0.006 (2019: 0.005; 2006: 0.1).
    # Pasture/range/paddock is not in Table 10.21 in either edition, which
    # defers it to Ch 11. Its 0.01 is the 2006 Ch 11 Table 11.1 EF3PRP,SO
    # for sheep and other animals; the 2019 Table 11.1 (Updated) gives
    # 0.004 for cattle, poultry and pigs and 0.003 for sheep and other.
    # Liquid/Slurry 0.002, Solid Storage and Dry Lot 0.005, Burned for Fuel
    # 0 and Other 0.005 are WHEP composite or fallback labels with no
    # counterpart system in either edition (assumed, unverified).
    # See `?ipcc_2019_n2o_ef_direct` and #601.
    table_10_21 = tibble::tribble(
      ~system,                          ~ef_kg_n2o_n_per_kg_n,
      "Uncovered Anaerobic Lagoon",      0.001,
      "Liquid/Slurry - No Crust",        0.002,
      "Liquid/Slurry - With Crust",      0.005,
      "Liquid/Slurry",                   0.002,
      "Solid Storage and Dry Lot",       0.005,
      "Solid Storage",                   0.005,
      "Dry Lot",                         0.005,
      "Pasture/Range/Paddock",           0.01,
      "Daily Spread",                    0.01,
      "Anaerobic Digester",              0.0,
      "Burned for Fuel",                 0.0,
      "Composting - In-vessel",          0.006,
      "Composting - Static Pile",        0.006,
      "Composting - Intensive",          0.006,
      "Composting - Passive",            0.01,
      "Poultry Manure - High Rise",      0.001,
      "Poultry Manure - Deep Litter",    0.001,
      "Other",                           0.005
    ),

    # Table 10.4: Cfi coefficients (MJ/day/kg^0.75).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10,
    # Table 10.4 (Updated) -- the coefficient table itself, not Eq 10.3
    # which consumes it.
    # Note: the 2019 Refinement is NOT a verbatim repeat of the 2006
    # Table 10.4. It ADDS the goat row (Goats = 0.315); the 2006 table has
    # no goat row at all, which is why goats must never inherit the sheep
    # value 0.217 (see #249, and the sheep/goat Ca trap in Table 10.5).
    # Two published rows are deliberately folded here and tracked in #601:
    # intact bulls take 0.370, not the 0.322 of non-lactating cows, steers
    # and juveniles; lambs under one year take 0.236, not the 0.217 of
    # sheep older than one year.
    table_10_4 = tibble::tribble(
      ~category, ~subcategory, ~cfi_mj_day_kg075,
      "Cattle",  "Lactating cow",          0.386,
      "Cattle",  "Non-lactating/Bulls",    0.322,
      "Buffalo", "Lactating cow",          0.386,
      "Buffalo", "Non-lactating/Bulls",    0.322,
      "Sheep",   "All",                    0.217,
      "Goats",   "All",                    0.315
    )
  )
}

# IPCC 2006 Tables ----

generate_ipcc_2006_tables <- function() {
  list(
    # Tier 1 enteric fermentation EFs.
    # Source: 2006 Guidelines, Vol 4, Ch 10, Table 10.11 for the cattle rows
    # and Table 10.10 (developed-countries column) for the Global
    # non-cattle rows. Three departures from the published tables: Oceania
    # dairy cattle is 90 here where Table 10.11 gives 100; the published
    # table groups Africa AND the Middle East in one row, 46 dairy and 31
    # other, which is repeated here as two regions; and the Indian
    # Subcontinent row, 58 dairy and 27 other, is absent.
    # See `?ipcc_2006_enteric_ef` and #601.
    ipcc_2006_enteric_ef = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr,
      "North America",   "Dairy Cattle",     128,
      "North America",   "Other Cattle",      53,
      "Western Europe",  "Dairy Cattle",     117,
      "Western Europe",  "Other Cattle",      57,
      "Eastern Europe",  "Dairy Cattle",      99,
      "Eastern Europe",  "Other Cattle",      58,
      "Oceania",         "Dairy Cattle",      90,
      "Oceania",         "Other Cattle",      60,
      "Latin America",   "Dairy Cattle",      72,
      "Latin America",   "Other Cattle",      56,
      "Asia",            "Dairy Cattle",      68,
      "Asia",            "Other Cattle",      47,
      "Africa",          "Dairy Cattle",      46,
      "Africa",          "Other Cattle",      31,
      "Middle East",     "Dairy Cattle",      46,
      "Middle East",     "Other Cattle",      31,
      "Global",          "Buffalo",           55,
      "Global",          "Sheep",              8,
      "Global",          "Goats",              5,
      "Global",          "Swine",              1.5,
      "Global",          "Horses",            18,
      "Global",          "Mules and Asses",   10
    ),

    # Tier 1 manure CH4 EFs.
    # Source: 2006 Guidelines, Vol 4, Ch 10, Table 10.14 for cattle, swine
    # and buffalo and Table 10.15 for sheep, goats and poultry. Table 10.14
    # is resolved per degree Celsius, and the value taken for a temp_zone is
    # not always the bound of that class, nor always present in the row:
    # North American dairy cows 53 is the 12 degree column rather than the
    # 48 of the cool class, North American other cattle 2 is the 15 degree
    # value where the whole cool class is 1, Asian dairy cows 16 is the 18
    # degree column rather than the 31 of the warm class, Latin American
    # dairy cows 1 is the cool value where the warm class gives 2, and
    # Western European dairy cows 20 appears in no column of that row (its
    # cool value is 21). See `?ipcc_2006_manure_ef` and #601.
    ipcc_2006_manure_ef = tibble::tribble(
      ~region, ~category, ~ef_kg_head_yr, ~temp_zone,
      "North America",   "Dairy Cattle",  53,  "Cool",
      "North America",   "Other Cattle",   2,  "Cool",
      "Western Europe",  "Dairy Cattle",  20,  "Cool",
      "Western Europe",  "Other Cattle",   6,  "Cool",
      "Latin America",   "Dairy Cattle",   1,  "Warm",
      "Latin America",   "Other Cattle",   1,  "Warm",
      "Asia",            "Dairy Cattle",  16,  "Warm",
      "Asia",            "Other Cattle",   1,  "Warm",
      "Global",          "Buffalo",        2,  "Warm",
      "Global",          "Sheep",          0.19, "All",
      "Global",          "Goats",          0.13, "All",
      "Global",          "Swine",          6,  "All",
      "Global",          "Poultry",        0.02, "All"
    ),

    # MCF by temperature, loosely 2006 Guidelines Vol 4 Ch 10 Table 10.17
    # but re-resolved onto a 10/15/20/25 degree grid that table does not
    # use. The temp_c == 25 value of all four rows is in no column of
    # Table 10.17 (assumed, unverified), and for the three class-resolved
    # rows the 20 degree value is the warm-class figure although 20 degrees
    # falls inside the published temperate class. Nothing in R/ reads this
    # object. See `?ipcc_2006_mcf_temp` and #601.
    ipcc_2006_mcf_temp = tibble::tribble(
      ~system, ~temp_c, ~mcf_percent,
      "Liquid/Slurry",          10, 17,
      "Liquid/Slurry",          15, 25,
      "Liquid/Slurry",          20, 35,
      "Liquid/Slurry",          25, 48,
      "Solid Storage",          10,  2,
      "Solid Storage",          15,  4,
      "Solid Storage",          20,  5,
      "Solid Storage",          25,  6,
      "Pasture/Range/Paddock",  10,  1,
      "Pasture/Range/Paddock",  15,  1.5,
      "Pasture/Range/Paddock",  20,  2,
      "Pasture/Range/Paddock",  25,  2.5,
      "Daily Spread",           10,  0.1,
      "Daily Spread",           15,  0.5,
      "Daily Spread",           20,  1,
      "Daily Spread",           25,  1.5
    )
  )
}

# GLEAM 2.0 manure-management shares ----
#
# Tab. 4.2-4.11 of the GLEAM 2.0 Supplement S1 workbook are the only published
# regional manure-management (MMS) shares in any GLEAM release: version 3.0
# dropped the whole table family, which is why the committed 3.0 workbook has
# no sheet for them. They are read here from
# data-raw/GLEAM_2.0_Supplement_S1.xlsx, byte-identical (219715 bytes, md5
# 72fd2ea477dfe8b30cd3657b2baa4af1) to
# https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Supplement_S1.xlsx
# re-downloaded and verified 2026-09-09.
#
# FAO. 2018. GLEAM Model description, Version 2.0, Revision 5, July 2018.
# Data reference year 2010. FAO issues no DOI for it.
#
# The published grain is finer than `regional_mms_distribution`'s in three
# ways at once -- production system, region and MMS vocabulary -- so the
# ingest is a crosswalk, not a copy. The four choices it forces are whep#958;
# each one is stated where it is applied below and summarised in the
# `@source` block of `?regional_mms_distribution`.

# The 10 GLEAM regions, in the column order every Tab. 4.x sheet uses. They
# are read positionally, not by header: openxlsx reads the "NA" (North
# America) header as a missing value, and Tab. 4.5 / 4.6 head the same two
# columns "RUSS" and "OC".
gleam2_mms_regions <- c(
  "NA",
  "RUS",
  "WE",
  "EE",
  "NENA",
  "ESEA",
  "OCE",
  "SA",
  "LAC",
  "SSA"
)

# CHOICE 1 of 4 -- species collapse. GLEAM publishes one table per production
# system; `regional_mms_distribution` is keyed on `species_gen` alone, so the
# systems have to be collapsed onto a species.
#
# Tab. 4.4 (feedlot cattle) is deliberately NOT read: GLEAM models the feedlot
# as a sub-system of the beef herd rather than a herd of its own, and WHEP has
# no feedlot category to attach it to, so reading it would give a minority
# system a third of the weight of all cattle manure. Tab. 4.7 covers sheep and
# goats together, so both species take it.
#
# Tab. 4.11 holds three systems (Layer / Broiler / Backyard) in one sheet,
# marked by its "Share (...)" rows, so its system is read from the sheet.
gleam2_mms_sheets <- function() {
  tibble::tribble(
    ~sheet, ~system, ~species,
    "Tab. 4.2", "Dairy cattle", "Cattle",
    "Tab. 4.3", "Beef cattle", "Cattle",
    "Tab. 4.5", "Dairy buffalo", "Buffalo",
    "Tab. 4.6", "Non-dairy buffalo", "Buffalo",
    "Tab. 4.7", "Small ruminants", "Sheep",
    "Tab. 4.7", "Small ruminants", "Goats",
    "Tab. 4.8", "Backyard pig", "Swine",
    "Tab. 4.9", "Intermediate pig", "Swine",
    "Tab. 4.10", "Industrial pig", "Swine",
    "Tab. 4.11", NA_character_, "Poultry"
  )
}

# CHOICE 2 of 4 -- region collapse. GLEAM's 10 regions are mapped onto the
# IPCC labels `.add_ipcc_region()` emits, so the ingested rows are keyed in
# exactly the vocabulary the runtime resolves a territory to. That crosswalk
# sends the Russian Federation and Eastern Europe to the same label, so those
# two published columns are averaged (unweighted) into one row.
gleam2_mms_ipcc_region <- function() {
  tibble::tribble(
    ~gleam_region, ~region,
    "NA", "North America",
    "RUS", "Eastern Europe",
    "WE", "Western Europe",
    "EE", "Eastern Europe",
    "NENA", "Middle East",
    "ESEA", "Asia",
    "OCE", "Oceania",
    "SA", "Indian Subcontinent",
    "LAC", "Latin America",
    "SSA", "Africa"
  )
}

# CHOICE 4 of 4 -- MMS vocabulary. GLEAM names 14 systems; WHEP's manure chain
# serves six labels, and only those six, in all four tables it reads by MMS
# name (`climate_mcf` at the three real climate zones, `.manure_ef3()`,
# `manure_loss_fractions.csv` and `.mms_manure_type()`). Every GLEAM system is
# therefore mapped onto one of the six, or excluded:
#
# - Six map by identity (pasture, daily spread, solid storage, liquid slurry,
#   uncovered anaerobic lagoon, poultry manure with litter). GLEAM assumes 50
#   percent of its liquid slurry carries a natural crust; WHEP's
#   "Liquid/Slurry" EF3 is the no-crust 0.002, not the 0.0035 a half-and-half
#   mix would give.
# - Drylot -> "Solid Storage", following IPCC's own combined category "solid
#   storage and dry lot", which `ipcc_2019_n2o_ef_direct` carries at the same
#   EF3 (0.005) as solid storage. The alternative is a separate "Dry Lot"
#   label, which `climate_mcf` already serves at a LOWER methane conversion
#   factor (2.5 against 4.0 percent, Temperate).
# - Composting - intensive windrow -> "Solid Storage". ASSUMED, UNVERIFIED.
#   Reached only by Tab. 4.4, which is not read, so it moves nothing today.
# - Pit storage (all three published variants) -> "Poultry Manure" for
#   chickens, "Liquid/Slurry" otherwise. ASSUMED, UNVERIFIED. A layer deep pit
#   is IPCC's "poultry manure without litter", which `ipcc_2019_n2o_ef_direct`
#   holds at the same 0.001 as the with-litter row; a pig pit below the
#   confinement is a slurry system. WHEP has no pit-storage label of its own,
#   and adding one needs an MCF and an EF3 this ingest cannot source.
# - Burned for fuel and anaerobic digester are EXCLUDED, and the remaining
#   systems renormalised to one. Neither has a servable WHEP label: both are
#   in `climate_mcf` only at `climate_zone == "All"`, which the MCF join
#   cannot reach, and neither is in `manure_loss_fractions.csv` at all.
#   Excluding them says the excreted nitrogen and volatile solids pass through
#   the systems that remain, which OVERSTATES those systems. It matters most
#   for buffalo: over the tables read, 18.5 percent of published buffalo
#   shares are excluded this way (dung burnt for fuel), against 4.1 percent
#   for cattle and 5.2 percent for swine, and none for the other species.
gleam2_mms_crosswalk <- function() {
  tibble::tribble(
    ~gleam_mms, ~mms_type,
    "Pasture, range, paddock", "Pasture/Range/Paddock",
    "Daily spread", "Daily Spread",
    "Solid storage", "Solid Storage",
    "Liquid slurry", "Liquid/Slurry",
    "Liquid slurry*", "Liquid/Slurry",
    "Uncovered anaerobic lagoon", "Anaerobic Lagoon",
    "Poultry manure with litter", "Poultry Manure",
    "Drylot", "Solid Storage",
    "Composting – intensive windrow", "Solid Storage",
    "Pit storage", "<pit>",
    "Pit storage (<1 month)", "<pit>",
    "Pit storage (>1 month)", "<pit>",
    "Burned for fuel", NA_character_,
    "Anaerobic digester", NA_character_
  )
}

# Species `regional_mms_distribution` carries that GLEAM 2.0 has no table for.
# Their rows are the pre-whep#958 placeholder values, kept so the manure engine
# still resolves a split for them, and flagged unsourced in `reference`.
gleam2_mms_unsourced_species <- function() {
  c("Horses", "Camels", "Mules and Asses")
}

# One Tab. 4.x sheet, long. Returns `system` (from the sheet's "Share (...)"
# rows, NA where the sheet has only the bare "Share" marker), `gleam_region`,
# `gleam_mms` and `share_percent`, with the unpublished "-" cells dropped.
parse_gleam2_mms_sheet <- function(path, sheet) {
  raw <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE)
  label <- stringr::str_trim(as.character(raw[[1]]))
  label[is.na(label)] <- ""
  is_share <- stringr::str_detect(label, "^Share")
  block <- label[is_share] |>
    stringr::str_remove("^Share") |>
    stringr::str_remove_all("[()]") |>
    stringr::str_squish()
  block[block == ""] <- NA_character_
  is_note <- stringr::str_detect(
    label,
    "^(TABLE|Regions:|\\*|Manure management system)"
  )
  values <- raw[, 2:11] |>
    purrr::map(~ suppressWarnings(as.numeric(.x))) |>
    rlang::set_names(gleam2_mms_regions) |>
    tibble::as_tibble()

  tibble::tibble(
    system = c(NA_character_, block)[cumsum(is_share) + 1L],
    gleam_mms = label,
    keep = !is_share & !is_note & label != ""
  ) |>
    dplyr::bind_cols(values) |>
    dplyr::filter(.data$keep) |>
    dplyr::select(-"keep") |>
    tidyr::pivot_longer(
      dplyr::all_of(gleam2_mms_regions),
      names_to = "gleam_region",
      values_to = "share_percent"
    ) |>
    dplyr::filter(!is.na(.data$share_percent))
}

# Tab. 4.2-4.11 as published, one row per
# (species, production system, GLEAM region, GLEAM MMS label).
parse_gleam2_mms <- function(path) {
  sheets <- gleam2_mms_sheets()
  unique(sheets$sheet) |>
    rlang::set_names() |>
    purrr::map(~ parse_gleam2_mms_sheet(path, .x)) |>
    purrr::list_rbind(names_to = "sheet") |>
    dplyr::inner_join(
      sheets,
      by = "sheet",
      suffix = c("", "_sheet"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      system = dplyr::coalesce(.data$system, .data$system_sheet)
    ) |>
    dplyr::select(
      "sheet",
      "species",
      "system",
      "gleam_region",
      "gleam_mms",
      "share_percent"
    )
}

# The published shares crosswalked onto WHEP's six `mms_type` labels and
# renormalised within each (species, system, GLEAM region), so the excluded
# systems are reallocated to the ones that remain rather than leaving the
# split short of one.
gleam2_mms_by_system <- function(published) {
  published |>
    dplyr::left_join(gleam2_mms_crosswalk(), by = "gleam_mms") |>
    dplyr::mutate(
      mms_type = dplyr::case_when(
        is.na(.data$mms_type) ~ NA_character_,
        .data$mms_type != "<pit>" ~ .data$mms_type,
        .data$species == "Poultry" ~ "Poultry Manure",
        .default = "Liquid/Slurry"
      )
    ) |>
    dplyr::filter(!is.na(.data$mms_type), .data$share_percent > 0) |>
    dplyr::summarise(
      share = sum(.data$share_percent),
      .by = c("species", "system", "gleam_region", "mms_type")
    ) |>
    dplyr::mutate(
      fraction = .data$share / sum(.data$share),
      .by = c("species", "system", "gleam_region")
    ) |>
    dplyr::select(-"share")
}

# Unweighted mean of the distributions in `keys`, over the groups that publish
# one. A system or region with no published column simply does not vote; a
# label absent from one voter's distribution counts as zero there, which is
# what dividing by the number of voters rather than by the number of rows
# does.
gleam2_mms_mean <- function(shares, keys, over) {
  voters <- shares |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(keys, over)))) |>
    dplyr::count(dplyr::across(dplyr::all_of(keys)), name = "n_voters")
  shares |>
    dplyr::summarise(
      fraction = sum(.data$fraction),
      .by = dplyr::all_of(c(keys, "mms_type"))
    ) |>
    dplyr::left_join(voters, by = keys) |>
    dplyr::mutate(fraction = .data$fraction / .data$n_voters) |>
    dplyr::select(-"n_voters")
}

# CHOICE 3 of 4 -- the `Global` row. GLEAM publishes regional tables and no
# global one, and most WHEP output resolves to `Global` (whep#678), so the row
# carrying the most weight is the one with no published value.
#
# It is derived here as the unweighted mean over the 10 GLEAM regions of the
# species-collapsed distributions, counting a region only where the source
# publishes one. A herd-weighted or manure-weighted mean would be better, and
# is NOT available from the source: Supplement S1 publishes herd *parameters*
# (Tab. 2.4-2.21: live weights, fertility, mortality, yields) and no regional
# animal numbers or production-system shares anywhere, so any weighting would
# have to come from outside GLEAM. Averaging over the 10 published regions
# rather than over the 9 IPCC labels keeps the Russian Federation and Eastern
# Europe at one vote each instead of half a vote each.
#
# This row is WHEP's, not FAO's. `reference` says so on every row of it.
gleam2_mms_distribution <- function(path, placeholder) {
  by_system <- parse_gleam2_mms(path) |>
    gleam2_mms_by_system()
  by_region <- gleam2_mms_mean(
    by_system,
    keys = c("species", "gleam_region"),
    over = "system"
  )
  global <- by_region |>
    gleam2_mms_mean(keys = "species", over = "gleam_region") |>
    dplyr::mutate(
      region = "Global",
      reference = paste(
        "GLEAM 2.0 Supplement S1 Tab. 4.2-4.11, unweighted mean over the",
        "10 GLEAM regions (WHEP-derived; GLEAM publishes no global row)"
      )
    )
  regional <- by_region |>
    dplyr::left_join(gleam2_mms_ipcc_region(), by = "gleam_region") |>
    gleam2_mms_mean(keys = c("species", "region"), over = "gleam_region") |>
    dplyr::mutate(
      reference = "GLEAM 2.0 Supplement S1 Tab. 4.2-4.11"
    )
  unsourced <- placeholder |>
    dplyr::filter(
      .data$species %in% gleam2_mms_unsourced_species(),
      .data$region == "Global"
    ) |>
    dplyr::mutate(
      reference = paste(
        "unsourced placeholder retained: GLEAM 2.0 publishes no manure-",
        "management table for this species (whep#921, whep#958)"
      )
    )

  dplyr::bind_rows(global, regional, unsourced) |>
    dplyr::mutate(
      fraction = .data$fraction / sum(.data$fraction),
      .by = c("region", "species")
    ) |>
    dplyr::mutate(source = "gleam_2_0") |>
    dplyr::select(
      "source",
      "region",
      "species",
      "mms_type",
      "fraction",
      "reference"
    ) |>
    dplyr::arrange(.data$species, .data$region, .data$mms_type)
}

# IPCC Tier 2 Parameters ----

generate_ipcc_tier2_params <- function() {
  list(
    # Energy coefficients for Tier 2 GE calculation.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.3-10.16, with
    # cfi_mj_day_kg075 from Table 10.4 (Updated) and ca_pasture from
    # Table 10.5 (Updated).
    # Note: Cfi now distinguishes dairy (lactating) vs
    # non-dairy (non-lactating/bulls).
    # Ca = activity coefficient (IPCC Eq 10.4).
    # Cp = pregnancy coefficient (IPCC Eq 10.13).
    # REG_gain_mj_kg = typical energy content of gain
    #   (used with REG in Eq 10.6).
    energy_coefs = tibble::tribble(
      ~category, ~subcategory,
        ~cfi_mj_day_kg075, ~ca_pasture, ~ca_feedlot,
        ~cp, ~cw, ~energy_content_gain_mj_kg,
      "Cattle", "Dairy",
        0.386, 0.17, 0.00, 0.10, 0.00, 22.0,
      "Cattle", "Non-Dairy",
        0.322, 0.17, 0.00, 0.10, 0.00, 22.0,
      "Buffalo", "Dairy",
        0.386, 0.17, 0.00, 0.10, 0.00, 20.0,
      "Buffalo", "Non-Dairy",
        0.322, 0.17, 0.00, 0.10, 0.00, 20.0,
      "Sheep", "All",
        0.217, 0.0107, 0.00, 0.077, 0.00, 23.0,
      # Goats: species-distinct IPCC 2019 Refinement Vol4 Ch10 coefficients.
      # Cfi = 0.315 (Table 10.4; earlier code duplicated the sheep 0.217,
      # underestimating goat NEm ~31%). ca_pasture = 0.019 (Table 10.5 goat
      # flat/lowland pasture; earlier code duplicated the sheep flat-pasture
      # value 0.0107, understating goat activity energy ~44%). Flat pasture is
      # the global default here, matching the sheep convention; terrain-specific
      # values (goat hilly 0.024) belong in a per-consumer override, not the
      # global table.
      "Goats", "All",
        0.315, 0.019, 0.00, 0.077, 0.00, 23.0
    ),

    # Ym values (% GE) used for Tier 2 enteric CH4.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.13 (Updated).
    # Note: The 2019 Refinement differentiates cattle by feed situation.
    # Sheep get a SINGLE Ym "irrespective of feed quality" (no body-weight
    # or diet-quality split -- see #250); the value is repeated across
    # every feed_situation (incl. Feedlot, for safety) so the join in
    # .join_ym() always resolves to the same, correct 6.7 regardless of
    # which feed_situation a sheep row happens to carry.
    ym_values = tibble::tribble(
      ~category, ~feed_situation, ~ym_percent,
      "Cattle",  "High",    6.5,
      "Cattle",  "Medium",  6.5,
      "Cattle",  "Low",     6.5,
      "Cattle",  "Feedlot", 3.0,
      "Buffalo", "High",    6.5,
      "Buffalo", "Medium",  6.5,
      "Buffalo", "Low",     6.5,
      "Sheep",   "High",    6.7,
      "Sheep",   "Medium",  6.7,
      "Sheep",   "Low",     6.7,
      "Sheep",   "Feedlot", 6.7,
      "Goats",   "High",    5.5,
      "Goats",   "Medium",  5.5,
      "Goats",   "Low",     5.5,
      "Camels",  "High",    5.0,
      "Camels",  "Medium",  5.0,
      "Camels",  "Low",     5.0
    ),

    # Bo - Maximum CH4 producing capacity (m3 CH4/kg VS).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16a.
    # Note: Dairy and non-dairy cattle have DIFFERENT Bo. Values are the
    # "High productivity systems" column (matches the convention already
    # used for Buffalo = 0.10). Camels = 0.26 (was previously miscopied
    # from Buffalo's 0.10 -- see issue #251).
    bo_values = tibble::tribble(
      ~category,             ~bo_m3_kg_vs,
      "Dairy Cattle",         0.24,
      "Other Cattle",         0.18,
      "Buffalo",              0.10,
      "Swine - Market",       0.45,
      "Swine - Breeding",     0.27,
      "Sheep",                0.19,
      "Goats",                0.18,
      "Horses",               0.30,
      "Mules and Asses",      0.33,
      "Camels",               0.26,
      "Poultry - Layers",     0.39,
      "Poultry - Broilers",   0.36
    ),

    # Ash content of manure (%).
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.24.
    ash_content = tibble::tribble(
      ~category,          ~ash_percent,
      "Cattle",             8.0,
      "Buffalo",            8.0,
      "Sheep",              8.0,
      "Goats",              8.0,
      "Swine",              4.0,
      "Poultry",           25.0,
      "Horses",            20.0,
      "Camels",             8.0,
      "Mules and Asses",    8.0
    ),

    # Feed Characteristics by diet quality.
    # Source: IPCC 2019, Vol 4, Ch 10, typical values.
    # ge_content_mj_kg_dm is the IPCC single default (18.45 MJ/kg DM) for all
    # diet qualities; in the gross-energy -> dry-matter intake conversion diet
    # quality therefore enters via DE% (digestibility, hence GE), NOT via feed
    # energy density. The three identical 18.45 values are intentional.
    feed_characteristics = tibble::tribble(
      ~diet_quality, ~de_percent, ~ndf_percent,
        ~ge_content_mj_kg_dm, ~cp_percent,
      "High",    75.0, 35.0, 18.45, 16.0,
      "Medium",  65.0, 50.0, 18.45, 12.0,
      "Low",     55.0, 65.0, 18.45,  8.0
    ),

    # Nitrogen Retention Fractions.
    # Source: IPCC 2019, Vol 4, Ch 10, Table 10.20.
    n_retention_frac = tibble::tribble(
      ~category,          ~n_retention_frac,
      "Dairy Cattle",       0.20,
      "Other Cattle",       0.07,
      "Buffalo",            0.20,
      "Sheep",              0.10,
      "Goats",              0.10,
      "Swine",              0.30,
      "Poultry",            0.30,
      "Horses",             0.05,
      "Camels",             0.05,
      "Mules and Asses",    0.05
    ),

    # Default Production Parameters.
    # Source: NRC 2001 / IPCC 2019 Ch 10.
    production_defaults = tibble::tribble(
      ~category, ~fat_percent, ~protein_percent,
        ~lactose_percent, ~weight_gain_kg_day,
        ~work_hours_day, ~pregnant_fraction,
      "Dairy Cattle", 4.0, 3.2,  4.85, 0.0, 0.0, 0.9,
      "Other Cattle", 0.0, 0.0,  0.0,  0.5, 0.0, 0.0,
      "Buffalo",      7.0, 4.5,  4.9,  0.2, 2.0, 0.6,
      "Sheep",        7.0, 5.5,  4.8,  0.1, 0.0, 0.5,
      "Goats",        4.0, 3.5,  4.5,  0.05, 0.0, 0.5,
      "Swine",        0.0, 0.0,  0.0,  0.6, 0.0, 0.0,
      "Poultry",      0.0, 0.0,  0.0,  0.05, 0.0, 0.0,
      "Horses",       0.0, 0.0,  0.0,  0.0, 4.0, 0.0,
      "Camels",       4.0, 3.7,  5.0,  0.0, 2.0, 0.4,
      "Mules and Asses", 0.0, 0.0, 0.0, 0.0, 6.0, 0.0
    ),

    # Climate-zone MCF (Methane Conversion Factor). The simplified 3-zone
    # version of table_10_17 above, and the one that is live:
    # `.calc_manure_ch4_tier2()` weights it by the manure-system mix.
    # Source: predominantly the 2006 Guidelines, Vol 4, Ch 10, Table 10.17,
    # NOT the 2019 Refinement, and with the same provenance profile as
    # table_10_17. Daily spread, solid storage, poultry manure and burned
    # for fuel match both editions; pasture/range/paddock 1.0/1.5/2.0 is
    # 2006 only, against a single 0.47 percent in the Refinement;
    # liquid/slurry 35 and anaerobic lagoon 73 are the 18 and 14 degree
    # columns of the 2006 per-degree rows rather than the class bound; and
    # dry lot 1.5/2.5/4.0, the single-value composting rows and Anaerobic
    # Digester 0 match neither edition (assumed, unverified).
    # See `?climate_mcf` and #601.
    climate_mcf = tibble::tribble(
      ~mms_type, ~climate_zone, ~mcf_percent,
      "Daily Spread",              "Cool",       0.1,
      "Daily Spread",              "Temperate",  0.5,
      "Daily Spread",              "Warm",       1.0,
      "Solid Storage",             "Cool",       2.0,
      "Solid Storage",             "Temperate",  4.0,
      "Solid Storage",             "Warm",       5.0,
      "Dry Lot",                   "Cool",       1.5,
      "Dry Lot",                   "Temperate",  2.5,
      "Dry Lot",                   "Warm",       4.0,
      "Liquid/Slurry",             "Cool",      17.0,
      "Liquid/Slurry",             "Temperate", 35.0,
      "Liquid/Slurry",             "Warm",      80.0,
      "Anaerobic Lagoon",          "Cool",      66.0,
      "Anaerobic Lagoon",          "Temperate", 73.0,
      "Anaerobic Lagoon",          "Warm",      80.0,
      "Pasture/Range/Paddock",     "Cool",       1.0,
      "Pasture/Range/Paddock",     "Temperate",  1.5,
      "Pasture/Range/Paddock",     "Warm",       2.0,
      "Poultry Manure",            "Cool",       1.5,
      "Poultry Manure",            "Temperate",  1.5,
      "Poultry Manure",            "Warm",       1.5,
      "Composting - Intensive",    "All",        0.5,
      "Composting - Passive",      "All",        1.0,
      "Anaerobic Digester",        "All",        0.0,
      "Burned for Fuel",           "All",       10.0
    ),

    # Regional MMS Distribution, the pre-whep#958 placeholder.
    # UNVERIFIED (whep#881, whep#921). Annotated "GLEAM 3.0 / FAO statistics
    # (simplified)" but traceable to no table: the GLEAM 3.0 workbook carries
    # no MMS shares, and these are round to 5 percentage points.
    # Since whep#958 this is no longer what the manure engines read by
    # default. It ships as the `source == "placeholder"` half of
    # `regional_mms_distribution`, selectable with the `mms_shares` engine
    # option, so the values published before the GLEAM 2.0 ingest stay
    # reproducible and the sensitivity to the ingest stays measurable.
    # The sourced half is built by gleam2_mms_distribution() above.
    regional_mms_placeholder = tibble::tribble(
      ~region, ~species, ~mms_type, ~fraction,
      "North America", "Cattle",
        "Liquid/Slurry", 0.40,
      "North America", "Cattle",
        "Solid Storage", 0.30,
      "North America", "Cattle",
        "Pasture/Range/Paddock", 0.25,
      "North America", "Cattle",
        "Daily Spread", 0.05,
      "Western Europe", "Cattle",
        "Liquid/Slurry", 0.35,
      "Western Europe", "Cattle",
        "Solid Storage", 0.45,
      "Western Europe", "Cattle",
        "Pasture/Range/Paddock", 0.15,
      "Western Europe", "Cattle",
        "Daily Spread", 0.05,
      "Latin America", "Cattle",
        "Pasture/Range/Paddock", 0.70,
      "Latin America", "Cattle",
        "Solid Storage", 0.15,
      "Latin America", "Cattle",
        "Daily Spread", 0.10,
      "Latin America", "Cattle",
        "Liquid/Slurry", 0.05,
      "Global", "Cattle",
        "Pasture/Range/Paddock", 0.50,
      "Global", "Cattle",
        "Solid Storage", 0.30,
      "Global", "Cattle",
        "Liquid/Slurry", 0.15,
      "Global", "Cattle",
        "Daily Spread", 0.05,
      "North America", "Swine",
        "Liquid/Slurry", 0.70,
      "North America", "Swine",
        "Solid Storage", 0.20,
      "North America", "Swine",
        "Anaerobic Lagoon", 0.10,
      "Global", "Swine",
        "Liquid/Slurry", 0.50,
      "Global", "Swine",
        "Solid Storage", 0.40,
      "Global", "Swine",
        "Daily Spread", 0.10,
      "Global", "Poultry",
        "Poultry Manure", 0.80,
      "Global", "Poultry",
        "Solid Storage", 0.20,
      "Global", "Sheep",
        "Pasture/Range/Paddock", 1.0,
      "Global", "Goats",
        "Pasture/Range/Paddock", 1.0,
      "Global", "Buffalo",
        "Pasture/Range/Paddock", 0.60,
      "Global", "Buffalo",
        "Daily Spread", 0.30,
      "Global", "Buffalo",
        "Solid Storage", 0.10,
      "Global", "Horses",
        "Pasture/Range/Paddock", 0.80,
      "Global", "Horses",
        "Solid Storage", 0.20,
      "Global", "Camels",
        "Pasture/Range/Paddock", 1.0,
      "Global", "Mules and Asses",
        "Pasture/Range/Paddock", 1.0
    ),

    # Temperature Adjustment Coefficients for NEm.
    # Source: NRC 2001 / IPCC 2019.
    temperature_adjustment = tibble::tribble(
      ~temp_range,     ~temp_min, ~temp_max, ~adjustment_factor,
      "Cold Stress",   -Inf,       5,         0.20,
      "Thermoneutral",    5,      25,         0.00,
      "Heat Stress",     25,     Inf,         0.10
    ),

    # Indirect N2O Emission Factors.
    # Source: IPCC 2019, Vol 4, Ch 10, Table 10.22 and
    # Vol 4, Ch 11, Table 11.3.
    indirect_n2o_ef = tibble::tribble(
      ~parameter,      ~value, ~description,
      "ef4_volatilization", 0.010,
        "EF4: N2O-N per kg NH3-N + NOx-N volatilized",
      "ef5_leaching",       0.0075,
        "EF5: N2O-N per kg N leached/runoff",
      "frac_gasms",         0.20,
        "FracGasMS: fraction N lost as NH3+NOx from MMS",
      "frac_leach",         0.30,
        "FracLeach: fraction N lost via leaching/runoff"
    ),

    # Uncertainty Ranges for key parameters.
    # Source: IPCC 2019 Refinement, Vol 4, Ch 10.
    uncertainty_ranges = tibble::tribble(
      ~parameter, ~lower_mult, ~upper_mult, ~distribution,
      "Ym",       0.85, 1.15, "normal",
      "MCF",      0.70, 1.30, "normal",
      "Bo",       0.80, 1.20, "normal",
      "EF_N2O",   0.50, 2.00, "lognormal",
      "Nex",      0.90, 1.10, "normal"
    ),

    # Grazing energy coefficients.
    # Source: NRC 2001 (converted 0.00045 Mcal/kg/km to MJ).
    grazing_energy_coefs = tibble::tribble(
      ~parameter, ~value_mj_kg_km, ~source,
      "walking_energy_cost", 0.0019,
        "NRC 2001 (0.00045 Mcal/kg/km)"
    )
  )
}

# Physical Constants ----

livestock_constants <- list(
  energy_content_ch4_mj_kg = 55.65,
  ch4_density_kg_m3 = 0.67,
  vs_energy_content_mj_kg = 18.45,
  n_to_n2o = 44 / 28,
  days_in_year = 365,
  default_de_percent = 65,
  default_ue_fraction = 0.04,
  # Eq 10.12 default Ym for sheep wool production.
  ev_wool_mj_kg = 24.0
)

# Main ----

main <- function() {
  message("\n=== Livestock Coefficients Data Generation ===\n")

  # GLEAM Excel tables
  gleam_file <- "data-raw/GLEAM_3.0_Supplement_S1.xlsx"
  gleam_excel_tables <- list()
  if (file.exists(gleam_file)) {
    gleam_raw <- extract_gleam_tables(gleam_file)
    message("\nExtracted ", length(gleam_raw), " GLEAM Excel tables")

    if (!is.null(gleam_raw$tab_s31)) {
      gleam_excel_tables$gleam_crop_residue_params <-
        parse_crop_residue_params(gleam_raw$tab_s31)
    }
    if (!is.null(gleam_raw$tab_s32)) {
      gleam_excel_tables$gleam_feed_composition <-
        parse_feed_composition(gleam_raw$tab_s32)
    }
    if (!is.null(gleam_raw$tab_s33)) {
      gleam_excel_tables$gleam_feed_digestibility <-
        parse_feed_digestibility(gleam_raw$tab_s33)
    }
    if (!is.null(gleam_raw$tab_s34)) {
      gleam_excel_tables$gleam_feed_conversion_ratios <-
        parse_feed_conversion_ratios(gleam_raw$tab_s34)
    }
    if (!is.null(gleam_raw$tab_s91)) {
      gleam_excel_tables$gleam_dressing_percentages <-
        parse_dressing_percentages(gleam_raw$tab_s91)
    }

    if (!is.null(gleam_raw$tab_sa1sa2)) {
      gleam_excel_tables$gleam_geographic_hierarchy <-
        parse_geographic_hierarchy(gleam_raw$tab_sa1sa2)
    }

    # S.6 tables: feed production system parameters
    if (
      !is.null(gleam_raw$tab_s61) &&
        !is.null(gleam_raw$tab_s62)
    ) {
      gleam_excel_tables$gleam_field_operation_ef <-
        parse_field_operation_ef(
          gleam_raw$tab_s61,
          gleam_raw$tab_s62
        )
    }
    if (
      !is.null(gleam_raw$tab_s63) &&
        !is.null(gleam_raw$tab_s64)
    ) {
      gleam_excel_tables$gleam_mechanization_levels <-
        parse_mechanization_levels(
          gleam_raw$tab_s63,
          gleam_raw$tab_s64
        )
    }
    if (
      !is.null(gleam_raw$tab_s65) &&
        !is.null(gleam_raw$tab_s66)
    ) {
      gleam_excel_tables$gleam_processing_transport_ef <-
        parse_processing_transport_ef(
          gleam_raw$tab_s65,
          gleam_raw$tab_s66
        )
    }
    if (
      !is.null(gleam_raw$tab_s67) &&
        !is.null(gleam_raw$tab_s68)
    ) {
      gleam_excel_tables$gleam_crop_residue_nitrogen <-
        parse_crop_residue_nitrogen(
          gleam_raw$tab_s67,
          gleam_raw$tab_s68
        )
    }
    if (!is.null(gleam_raw$tab_s69)) {
      gleam_excel_tables$gleam_fracremove <-
        parse_fracremove(gleam_raw$tab_s69)
    }

    # S.7 tables: energy use emission factors
    gleam_excel_tables$gleam_energy_use_ef <-
      parse_energy_use_ef(gleam_raw)
  } else {
    warning("GLEAM Excel file not found: ", gleam_file)
  }

  # GLEAM PDF tables
  message("\nGenerating GLEAM PDF tables...")
  gleam_pdf_tables <- generate_gleam_pdf_tables()

  # IPCC 2019 tables
  message("\nGenerating IPCC 2019 tables...")
  ipcc_raw <- generate_ipcc_2019_tables()
  ipcc_2019 <- list(
    ipcc_2019_enteric_ef_cattle = ipcc_raw$table_10_11_cattle,
    ipcc_2019_enteric_ef_other = ipcc_raw$table_10_10_other,
    ipcc_2019_manure_ch4_ef_cattle = ipcc_raw$table_10_14_cattle,
    ipcc_2019_manure_ch4_ef_other = ipcc_raw$table_10_14_other,
    ipcc_2019_mcf_manure = ipcc_raw$table_10_17,
    ipcc_2019_n_excretion = ipcc_raw$table_10_19,
    ipcc_2019_n2o_ef_direct = ipcc_raw$table_10_21,
    ipcc_2019_ym = ipcc_raw$table_10_12,
    ipcc_2019_bo = ipcc_raw$table_10_16a,
    ipcc_2019_cfi = ipcc_raw$table_10_4
  )

  # IPCC 2006 tables
  message("\nGenerating IPCC 2006 tables...")
  ipcc_2006 <- generate_ipcc_2006_tables()

  # IPCC Tier 2 parameters
  message("\nGenerating IPCC Tier 2 parameters...")
  ipcc_t2_raw <- generate_ipcc_tier2_params()

  # regional_mms_distribution: the GLEAM 2.0 ingest (whep#958) plus the
  # placeholder it replaced, kept selectable. The workbook is required -- the
  # sourced half cannot be reconstructed without it, and a silent fall back to
  # the placeholder is exactly the provenance failure whep#921 was opened for.
  gleam2_file <- "data-raw/GLEAM_2.0_Supplement_S1.xlsx"
  if (!file.exists(gleam2_file)) {
    stop("GLEAM 2.0 workbook not found: ", gleam2_file)
  }
  message("\nIngesting GLEAM 2.0 Tab. 4.2-4.11 manure-management shares...")
  regional_mms_distribution <- dplyr::bind_rows(
    gleam2_mms_distribution(
      gleam2_file,
      ipcc_t2_raw$regional_mms_placeholder
    ),
    ipcc_t2_raw$regional_mms_placeholder |>
      dplyr::mutate(
        source = "placeholder",
        reference = paste(
          "unsourced placeholder, superseded by the GLEAM 2.0 ingest",
          "(whep#921, whep#958)"
        )
      ) |>
      dplyr::select(
        "source",
        "region",
        "species",
        "mms_type",
        "fraction",
        "reference"
      )
  )

  ipcc_tier2 <- list(
    ipcc_tier2_energy_coefs = ipcc_t2_raw$energy_coefs,
    ipcc_tier2_ym_values = ipcc_t2_raw$ym_values,
    ipcc_tier2_bo_values = ipcc_t2_raw$bo_values,
    ipcc_tier2_manure_ash = ipcc_t2_raw$ash_content,
    ipcc_tier2_n_retention = ipcc_t2_raw$n_retention_frac,
    livestock_production_defaults = ipcc_t2_raw$production_defaults,
    feed_characteristics = ipcc_t2_raw$feed_characteristics,
    climate_mcf = ipcc_t2_raw$climate_mcf,
    regional_mms_distribution = regional_mms_distribution,
    temperature_adjustment = ipcc_t2_raw$temperature_adjustment,
    indirect_n2o_ef = ipcc_t2_raw$indirect_n2o_ef,
    uncertainty_ranges = ipcc_t2_raw$uncertainty_ranges,
    grazing_energy_coefs = ipcc_t2_raw$grazing_energy_coefs
  )

  # Combine all objects
  all_objects <- c(
    gleam_excel_tables,
    gleam_pdf_tables,
    ipcc_2019,
    ipcc_2006,
    ipcc_tier2,
    list(livestock_constants = livestock_constants)
  )

  # Ensure all data.frames are tibbles
  all_objects <- lapply(all_objects, function(x) {
    if (is.data.frame(x)) tibble::as_tibble(x) else x
  })

  message(
    "\nSaving ",
    length(all_objects),
    " objects to data/livestock_coefs.rda..."
  )
  save(
    list = names(all_objects),
    file = "data/livestock_coefs.rda",
    compress = "xz",
    envir = list2env(all_objects, envir = new.env())
  )

  message("\n=== Complete ===")
  message("Total objects saved: ", length(all_objects))
}

main()
