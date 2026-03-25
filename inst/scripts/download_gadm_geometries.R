# Download GADM geometries for polities missing geometry from CShapes
#
# This script downloads GADM 4.1 boundaries for all polities identified in
# 16_polygon_source_mapping.csv that have source=GADM or GADM_combine.
# It also handles NatEarth fallbacks for tiny territories.
#
# Usage: Run interactively or via Rscript after devtools::load_all()
#   R_PROFILE_USER=/dev/null Rscript -e "devtools::load_all(); source('inst/scripts/download_gadm_geometries.R')"
#
# Output: data-raw/polities_inputs/gadm_geometries.gpkg

library(sf)
library(dplyr)

# Disable S2 to avoid geometry validation errors with GADM data
sf::sf_use_s2(FALSE)
# Suppress planar assumption warnings (expected with S2 off)
options(warn = 1)  # Print warnings immediately but don't stop
# Increase download timeout for large files
options(timeout = 300)

# ---- Configuration ----
gadm_cache_dir <- here::here("data-raw", "gadm_cache")
dir.create(gadm_cache_dir, showWarnings = FALSE, recursive = TRUE)

output_path <- here::here("data-raw", "polities_inputs", "gadm_geometries.gpkg")

# ---- Helper functions ----

#' Download a GADM file for a given ISO3 code and level
#' Tries GeoJSON first, falls back to GeoPackage for problematic files
#' Returns an sf object, cached locally
download_gadm <- function(iso3, level = 0) {
  # Try GeoJSON first
  json_filename <- sprintf("gadm41_%s_%d.json", iso3, level)
  json_path <- file.path(gadm_cache_dir, json_filename)

  if (!file.exists(json_path)) {
    json_url <- sprintf(
      "https://geodata.ucdavis.edu/gadm/gadm4.1/json/%s", json_filename
    )
    message("  Downloading ", json_url)
    tryCatch(
      download.file(json_url, json_path, mode = "wb", quiet = TRUE),
      error = function(e) {
        message("  FAILED to download JSON: ", e$message)
      }
    )
  } else {
    message("  Using cached: ", json_filename)
  }

  # Try reading JSON
  if (file.exists(json_path) && file.size(json_path) > 100) {
    result <- tryCatch(
      sf::st_read(json_path, quiet = TRUE) |> sf::st_make_valid(),
      error = function(e) {
        message("  FAILED to read JSON: ", e$message)
        NULL
      }
    )
    if (!is.null(result)) return(result)
    # Remove corrupt file
    unlink(json_path)
  }

  # Fallback: try GPKG (whole country, all levels)
  gpkg_filename <- sprintf("gadm41_%s.gpkg", iso3)
  gpkg_path <- file.path(gadm_cache_dir, gpkg_filename)
  layer_name <- sprintf("ADM_ADM_%d", level)

  if (!file.exists(gpkg_path)) {
    gpkg_url <- sprintf(
      "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/%s", gpkg_filename
    )
    message("  Fallback: downloading GPKG from ", gpkg_url)
    tryCatch(
      download.file(gpkg_url, gpkg_path, mode = "wb", quiet = TRUE),
      error = function(e) {
        message("  FAILED to download GPKG: ", e$message)
        return(NULL)
      }
    )
  }

  if (!file.exists(gpkg_path)) return(NULL)

  # List layers and find the right one
  tryCatch({
    layers <- sf::st_layers(gpkg_path)$name
    target_layer <- layers[grepl(sprintf("ADM_%d", level), layers)]
    if (length(target_layer) == 0) target_layer <- layers[1]
    sf::st_read(gpkg_path, layer = target_layer[1], quiet = TRUE) |>
      sf::st_make_valid()
  }, error = function(e) {
    message("  FAILED to read GPKG: ", e$message)
    NULL
  })
}

#' Get L0 (country-level) boundary for an ISO3 code
gadm_l0 <- function(iso3) {
  g <- download_gadm(iso3, level = 0)
  if (is.null(g)) {
    return(NULL)
  }
  tryCatch({
    g |>
      sf::st_union() |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x)
  }, error = function(e) {
    message("  ERROR in st_union for ", iso3, ": ", e$message)
    # Fallback: just use first geometry
    tryCatch({
      g[1, ] |>
        dplyr::select(geometry) |>
        sf::st_cast("MULTIPOLYGON")
    }, error = function(e2) {
      message("  FALLBACK also failed: ", e2$message)
      NULL
    })
  })
}

#' Get L1 boundary filtered by NAME_1 (case-insensitive partial match)
#' Also tries matching with spaces removed (GADM sometimes uses CamelCase)
gadm_l1_filter <- function(iso3, name1_patterns) {
  g <- download_gadm(iso3, level = 1)
  if (is.null(g)) {
    return(NULL)
  }

  # Try matching by NAME_1 (also match without spaces for CamelCase names)
  pattern <- paste(name1_patterns, collapse = "|")
  matched <- g |> dplyr::filter(
    grepl(pattern, NAME_1, ignore.case = TRUE) |
    grepl(gsub(" ", "", pattern), NAME_1, ignore.case = TRUE) |
    grepl(pattern, gsub("([a-z])([A-Z])", "\\1 \\2", NAME_1), ignore.case = TRUE)
  )

  if (nrow(matched) == 0) {
    message("  WARNING: No L1 match for ", iso3, " with pattern '", pattern, "'")
    message("  Available NAME_1 values: ",
            paste(unique(g$NAME_1), collapse = ", "))
    return(NULL)
  }

  tryCatch({
    # Extract polygon parts in case st_make_valid produced GEOMETRYCOLLECTION
    geom <- sf::st_geometry(matched)
    if (any(sf::st_geometry_type(geom) == "GEOMETRYCOLLECTION")) {
      geom <- sf::st_collection_extract(geom, "POLYGON")
    }
    geom |>
      sf::st_union() |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x)
  }, error = function(e) {
    message("  ERROR in L1 st_union for ", iso3, ": ", e$message)
    NULL
  })
}

#' Get L2 boundary filtered by NAME_2 (or NAME_1 if needed)
gadm_l2_filter <- function(iso3, name_patterns, field = "NAME_2") {
  g <- download_gadm(iso3, level = 2)
  if (is.null(g)) {
    return(NULL)
  }

  pattern <- paste(name_patterns, collapse = "|")
  col_vals <- g[[field]]
  matched <- g |> dplyr::filter(
    grepl(pattern, col_vals, ignore.case = TRUE) |
    grepl(gsub(" ", "", pattern), col_vals, ignore.case = TRUE) |
    grepl(pattern, gsub("([a-z])([A-Z])", "\\1 \\2", col_vals), ignore.case = TRUE)
  )

  if (nrow(matched) == 0) {
    message("  WARNING: No L2 match for ", iso3, " with pattern '", pattern,
            "' in field ", field)
    message("  Available values: ",
            paste(head(unique(col_vals), 30), collapse = ", "))
    return(NULL)
  }

  tryCatch({
    geom <- sf::st_geometry(matched)
    if (any(sf::st_geometry_type(geom) == "GEOMETRYCOLLECTION")) {
      geom <- sf::st_collection_extract(geom, "POLYGON")
    }
    geom |>
      sf::st_union() |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x)
  }, error = function(e) {
    message("  ERROR in L2 st_union for ", iso3, ": ", e$message)
    NULL
  })
}

#' Combine L0 boundaries from multiple ISO3 codes
gadm_combine_l0 <- function(iso3_codes) {
  geoms <- lapply(iso3_codes, gadm_l0)
  geoms <- Filter(\(x) !is.null(x), geoms)
  if (length(geoms) == 0) {
    return(NULL)
  }
  tryCatch({
    dplyr::bind_rows(geoms) |>
      sf::st_union() |>
      sf::st_cast("MULTIPOLYGON") |>
      sf::st_as_sf() |>
      dplyr::rename(geometry = x)
  }, error = function(e) {
    message("  ERROR in combine st_union: ", e$message)
    NULL
  })
}

#' Wrap a geometry result with a polity_name
wrap_result <- function(polity_name, geom_sf) {
  if (is.null(geom_sf) || nrow(geom_sf) == 0) {
    message("  SKIPPED: ", polity_name, " (no geometry obtained)")
    return(NULL)
  }
  tibble::tibble(
    polity_name = polity_name,
    geometry = sf::st_geometry(geom_sf)
  ) |>
    sf::st_as_sf()
}


# ============================================================
# GADM L0 DIRECT DOWNLOADS
# Each polity maps to a single country boundary
# ============================================================

message("\n=== STEP 1: GADM L0 direct downloads ===\n")

l0_mapping <- tibble::tribble(
  ~polity_name,                                       ~iso3,
  "American Samoa",                                    "ASM",
  "Andorra",                                           "AND",
  "Anguilla",                                          "AIA",
  "Antigua and Barbuda",                               "ATG",
  "Aruba",                                             "ABW",
  "Bermuda",                                           "BMU",
  "Bonaire, Sint Eustatius and Saba",                  "BES",
  "British Indian Ocean Territory",                    "IOT",
  "British Virgin Islands",                            "VGB",
  "Cayman Islands",                                    "CYM",
  "Christmas Island",                                  "CXR",
  "Cocos (Keeling) Islands",                           "CCK",
  "Cook Islands",                                      "COK",
  "Cura\u00e7ao",                                      "CUW",
  "Dominica",                                          "DMA",
  "French Guiana",                                     "GUF",
  "French Polynesia",                                  "PYF",
  "French Southern Territories",                       "ATF",
  "Gibraltar",                                         "GIB",
  "Greenland",                                         "GRL",
  "Grenada",                                           "GRD",
  "Guadeloupe",                                        "GLP",
  "Guam",                                              "GUM",
  "Guernsey",                                          "GGY",
  # Hong Kong and Macao are under CHN in GADM, handled in L1 section
  "Isle of Man",                                       "IMN",
  "Jersey",                                            "JEY",
  "Kiribati",                                          "KIR",
  "Liechtenstein",                                     "LIE",
  # (Hong Kong and Macao moved to L1 section under CHN)
  "Marshall Islands",                                  "MHL",
  "Martinique",                                        "MTQ",
  "Mayotte",                                           "MYT",
  "Montserrat",                                        "MSR",
  "Nauru",                                             "NRU",
  "Niue",                                              "NIU",
  "Norfolk Island",                                    "NFK",
  "Northern Mariana Islands",                          "MNP",
  "Palau",                                             "PLW",
  "Pitcairn",                                          "PCN",
  "Puerto Rico",                                       "PRI",
  "R\u00e9union",                                      "REU",
  "Saint Barth\u00e9lemy",                             "BLM",
  "Saint Helena, Ascension and Tristan da Cunha",      "SHN",
  "Saint Kitts and Nevis",                             "KNA",
  "Saint Lucia",                                       "LCA",
  "Saint Martin (French part)",                        "MAF",
  "Saint Pierre and Miquelon",                         "SPM",
  "Saint Vincent and the Grenadines",                  "VCT",
  "Samoa",                                             "WSM",
  "San Marino",                                        "SMR",
  "Sao Tome and Principe",                             "STP",
  "Seychelles",                                        "SYC",
  "Sint Maarten (Dutch part)",                         "SXM",
  "Solomon Islands",                                   "SLB",
  "South Georgia and the South Sandwich Islands",      "SGS",
  "Svalbard and Jan Mayen",                            "SJM",
  "Tokelau",                                           "TKL",
  "Tonga",                                             "TON",
  "Turks and Caicos Islands",                          "TCA",
  "Tuvalu",                                            "TUV",
  "United States Virgin Islands",                      "VIR",
  "Vanuatu",                                           "VUT",
  "Wallis and Futuna",                                 "WLF",
  "Western Sahara",                                    "ESH",
  # Historical polities using modern country as proxy
  "Danish Virgin Islands",                             "VIR",
  "St. Helena",                                        "SHN",
  "St. Luc\u00eda",                                    "LCA",
  "St. Vincent",                                       "VCT",
  "Panama (before Canal Zone)",                        "PAN",
  "Luxembourg (old)",                                  "LUX",
  "Netherlands (old)",                                 "NLD",
  "Nigeria (old)",                                     "NGA",
  "Germany/Zollverein",                                "DEU",
  "Egypt (to 1922)",                                   "EGY",
  "Mariana Island",                                    "MNP",
  "New Caledonia",                                     "NCL",
  # Additional L0 downloads
  "Heard Island and McDonald Islands",                 "HMD",
  "Bouvet Island",                                     "BVT",
  "French Somalia",                                    "DJI",
)

l0_results <- list()
for (i in seq_len(nrow(l0_mapping))) {
  row <- l0_mapping[i, ]
  message("Processing L0: ", row$polity_name, " (", row$iso3, ")")
  geom <- gadm_l0(row$iso3)
  result <- wrap_result(row$polity_name, geom)
  if (!is.null(result)) {
    l0_results[[length(l0_results) + 1]] <- result
  }
}


# ============================================================
# GADM L1 SUBNATIONAL DOWNLOADS
# Each polity maps to a filtered subset of a country's L1 divisions
# ============================================================

message("\n=== STEP 2: GADM L1 subnational ===\n")

l1_mapping <- list(
  # UAE Emirates
  list("Abu Dhabi",       "ARE", c("Abu Dhabi", "Abu Zaby")),
  list("Ajman",           "ARE", c("Ajman", "'Ajman")),
  list("Fujairah",        "ARE", c("Fujairah", "Al Fujayrah")),
  list("Ras al Khaimah",  "ARE", c("RasAl", "Khaimah")),
  list("Sharjah",         "ARE", c("Sharjah", "Ash Shariqah")),
  list("Umm al Qawain",   "ARE", c("Ummal", "Qaywayn")),
  # Spain
  list("Canary Islands",  "ESP", c("Islas Canarias", "Canarias")),
  # Greece
  list("Crete",           "GRC", c("Kriti", "Crete")),
  # Afghanistan
  list("Herat",           "AFG", c("Herat", "Hirat")),
  list("Badakhshan",      "AFG", c("Badakhshan")),
  # India
  list("Sikkim",          "IND", c("Sikkim")),
  # China (HKG and MAC are under CHN in GADM 4.1)
  list("Hong Kong",       "CHN", c("Hong Kong", "Xianggang")),
  list("Macao",           "CHN", c("Macao", "Macau", "Aomen")),
  list("Tibet (old)",     "CHN", c("Xizang", "Tibet")),
  list("Yunnan",          "CHN", c("Yunnan")),
  # Japan
  list("Ryukyu Islands",  "JPN", c("Okinawa")),
  # Finland (Åland may be under "SouthernFinland" or "WesternFinland" in GADM L1)
  # Try L2 for better granularity
  # list("Åland Islands", "FIN", c("Ahvenanmaa", "Åland", "Aland")),
  # Canada
  list("British Columbia",   "CAN", c("British Columbia")),
  list("Lower Quebec",       "CAN", c("Qu\u00e9bec", "Quebec")),
  list("New Brunswick",      "CAN", c("New Brunswick")),
  list("Newfoundland",       "CAN", c("Newfoundland")),
  # Nova Scotia moved to bottom of L1 list with better pattern matching
  list("Ontario",            "CAN", c("Ontario")),
  list("Prince Edward Island", "CAN", c("Prince Edward Island")),
  # Australia
  list("Queensland",       "AUS", c("Queensland")),
  list("South Australia",  "AUS", c("South Australia")),
  list("Victoria",         "AUS", c("Victoria")),
  list("Western Australia", "AUS", c("Western Australia")),
  # Malaysia L1 for historical polities
  list("Sabah (British Borneo)",   "MYS", c("Sabah")),
  list("Sarawak",                  "MYS", c("Sarawak")),
  list("Federated Malay States",   "MYS",
       c("Perak", "Selangor", "Negeri Sembilan", "Pahang")),
  # China L1 for Manchukuo
  list("Manchukuo",  "CHN", c("Heilongjiang", "Jilin", "Liaoning")),
  # Indonesia L1 for Dutch New Guinea
  list("Dutch new Guinea", "IDN", c("Papua", "West Papua", "Irian")),
  # PNG subnational
  list("British New Guinea", "PNG",
       c("Central", "Gulf", "Milne Bay", "Northern", "Western")),
  list("New Guinea", "PNG",
       c("East Sepik", "West Sepik", "Madang", "Morobe", "Enga",
         "Western Highlands", "Eastern Highlands", "Southern Highlands",
         "Chimbu", "Simbu")),
  # ---- Italian pre-unification states (ITA L1) ----
  list("Tuscany",          "ITA", c("Toscana")),
  list("Two Sicilies",     "ITA",
       c("Sicily", "Campania", "Calabria", "Apulia", "Basilicata",
         "Molise", "Abruzzo")),
  list("Sardinia",         "ITA",
       c("Sardegna", "Piemonte", "Liguria", "Valled'Aosta")),
  list("Papal States",     "ITA", c("Lazio", "Umbria", "Marche")),
  # ---- Central Asian khanates (UZB/TKM L1) ----
  list("Bukhara",          "UZB",
       c("Buxoro", "Samarqand", "Navoiy", "Qashqadaryo", "Surxondaryo")),
  list("Kokand",           "UZB", c("Farg'ona", "Namangan", "Andijon")),
  # ---- French Polynesia sub-territories (PYF L1) ----
  list("Gambier Island",       "PYF", c("Tuamotu", "Gambier")),
  list("Marquesas Island",     "PYF", c("Marquises")),
  list("Society Islands  (Tahiti)", "PYF", c("Vent", "Sous-le-Vent")),
  # ---- Gaza Strip (PSE L1) ----
  list("Gaza Strip",       "PSE", c("Gaza")),
  # ---- Berlin divisions (DEU L1, same proxy for both) ----
  list("East Berlin",      "DEU", c("Berlin")),
  list("West Berlin",      "DEU", c("Berlin")),
  # ---- German pre-unification states (DEU L1) ----
  list("Bavaria",              "DEU", c("Bayern")),
  list("Saxony",               "DEU", c("Sachsen")),
  list("Hanover",              "DEU", c("Niedersachsen")),
  list("Hesse",                "DEU", c("Hessen")),
  list("Mecklenburg",          "DEU", c("Mecklenburg-Vorpommern")),
  list("Thuringia",            "DEU", c("Th\u00fcringen")),
  list("Schleswig-Holstein",   "DEU", c("Schleswig-Holstein")),
  list("Bremen (city-state)",  "DEU", c("Bremen")),
  list("Oldenburg",            "DEU", c("Niedersachsen")),
  list("W\u00fcrttemberg",     "DEU", c("Baden-W\u00fcrttemberg")),
  list("Baden",                "DEU", c("Baden-W\u00fcrttemberg")),
  # ---- Nova Scotia retry (CAN L1) ----
  # Previous attempt failed; keep in list to retry
  list("Nova Scotia  Cape Breton Isl.", "CAN", c("NovaScotia", "Nova Scotia"))
)

l1_results <- list()
for (entry in l1_mapping) {
  polity_name <- entry[[1]]
  iso3 <- entry[[2]]
  patterns <- entry[[3]]
  message("Processing L1: ", polity_name, " (", iso3, ")")
  geom <- gadm_l1_filter(iso3, patterns)
  result <- wrap_result(polity_name, geom)
  if (!is.null(result)) {
    l1_results[[length(l1_results) + 1]] <- result
  }
}


# ============================================================
# GADM L2 SUBNATIONAL DOWNLOADS
# ============================================================

message("\n=== STEP 3: GADM L2 subnational ===\n")

l2_results <- list()

# Ceuta y Melilla / Spanish North Africa (same geometry)
message("Processing L2: Ceuta Y Melilla (ESP)")
ceuta_melilla <- gadm_l1_filter("ESP", c("Ceuta", "Melilla"))
r <- wrap_result("Ceuta Y Melilla", ceuta_melilla)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r
r <- wrap_result("Spanish North Africa", ceuta_melilla)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Ionian islands (Greece) - GADM NAME_2 has "IonianIslands"
message("Processing L2: Ionian islands (GRC)")
ionian <- gadm_l2_filter("GRC", c("IonianIslands", "Ionian"), field = "NAME_2")
r <- wrap_result("Ionian islands", ionian)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Dodecanese (Greece) - under "SouthAegean" in GADM NAME_2
message("Processing L2: Dodecanese Is. (GRC)")
dodecanese <- gadm_l2_filter("GRC", c("SouthAegean"), field = "NAME_2")
r <- wrap_result("Dodecanese Is.", dodecanese)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Bonin island (Japan L2)
message("Processing L2: Bonin island (JPN)")
bonin <- gadm_l2_filter("JPN", c("Ogasawara"), field = "NAME_2")
if (is.null(bonin)) {
  # Try broader Tokyo L1 as fallback
  bonin <- gadm_l1_filter("JPN", c("Tokyo"))
}
r <- wrap_result("Bonin island", bonin)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Duchy Modena (ITA L2: Modena + Reggio Emilia provinces)
message("Processing L2: Duchy Modena (ITA)")
modena <- gadm_l2_filter("ITA", c("Modena", "ReggioNell"), field = "NAME_2")
r <- wrap_result("Duchy Modena", modena)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Duchy Parma (ITA L2: Parma + Piacenza provinces)
message("Processing L2: Duchy Parma (ITA)")
parma <- gadm_l2_filter("ITA", c("Parma", "Piacenza"), field = "NAME_2")
r <- wrap_result("Duchy Parma", parma)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Kiautchou (German concession = CHN Shandong L2 Qingdao)
message("Processing L2: Kiautchou (CHN Qingdao)")
kiautchou <- gadm_l2_filter("CHN", c("Qingdao"), field = "NAME_2")
r <- wrap_result("Kiautchou", kiautchou)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Kwantung / Port Arthur (Russian/Japanese concession = CHN Liaoning L2 Dalian)
message("Processing L2: Kwantung (CHN Dalian)")
kwantung <- gadm_l2_filter("CHN", c("Dalian"), field = "NAME_2")
r <- wrap_result("Kwantung (Port Arthur)", kwantung)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Kwang-Chou-Wan (French concession = CHN Guangdong L2 Zhanjiang)
message("Processing L2: Kwang-Chou-Wan (CHN Zhanjiang)")
kwangchou <- gadm_l2_filter("CHN", c("Zhanjiang"), field = "NAME_2")
r <- wrap_result("Kwang-Chou-Wan", kwangchou)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r

# Vancouver's Island (CAN BC L2 - island districts)
message("Processing L2: Vancouver's Island (CAN BC)")
vancouver <- gadm_l2_filter("CAN",
  c("Alberni", "Capital", "Comox", "Cowichan", "MountWaddington", "Nanaimo"),
  field = "NAME_2")
r <- wrap_result("Vancouver's Island", vancouver)
if (!is.null(r)) l2_results[[length(l2_results) + 1]] <- r


# ============================================================
# MULTI-COUNTRY COMBINES (L0 unions)
# ============================================================

message("\n=== STEP 4: Multi-country combines ===\n")

combine_mapping <- list(
  list("Channel Islands",             c("GGY", "JEY")),
  list("Caroline Island",             c("FSM", "PLW")),
  list("Gilbert and Ellice Island",   c("KIR", "TUV")),
  list("Netherlands Antilles",        c("CUW", "SXM", "BES")),
  list("Netherlands Antilles (original)", c("ABW", "CUW", "SXM", "BES")),
  list("Dutch West Indies (Netherland Antilles)", c("ABW", "CUW", "SXM", "BES")),
  list("St Martin and St Barth\u00e9lemy", c("MAF", "BLM")),
  list("Saint Kitts-Nevis-Anguilla",  c("KNA", "AIA")),
  list("Ethiopia (old)",              c("ETH", "ERI")),
  list("Rwanda and Burundi",          c("RWA", "BDI")),
  list("India (to 1947)",             c("IND", "PAK", "BGD", "MMR")),
  list("Palestine/Jordan",            c("JOR", "PSE")),
  list("Sudan (Anglo-Egyptian)",      c("SDN", "SSD")),
  list("Syria and Lebanon",           c("SYR", "LBN")),
  list("British East Africa",         c("KEN", "UGA")),
  list("Mayotte and Noissi-be",       c("MYT", "MDG")),
  # Ottoman Empire = Turkey + Arab provinces + Balkans (broad proxy)
  list("Ottoman Empire",             c("TUR", "SYR", "IRQ", "JOR",
                                       "PSE", "LBN", "SAU", "YEM",
                                       "GRC", "BGR", "ROU", "SRB",
                                       "BIH", "MNE", "ALB", "MKD",
                                       "HRV", "LBY", "TUN", "EGY",
                                       "SDN", "ERI", "DJI", "SOM",
                                       "KWT", "BHR", "QAT", "CYP"))
)

combine_results <- list()
for (entry in combine_mapping) {
  polity_name <- entry[[1]]
  iso3_codes <- entry[[2]]
  message("Processing combine: ", polity_name,
          " (", paste(iso3_codes, collapse = "+"), ")")
  geom <- gadm_combine_l0(iso3_codes)
  result <- wrap_result(polity_name, geom)
  if (!is.null(result)) {
    combine_results[[length(combine_results) + 1]] <- result
  }
}


# ============================================================
# HISTORICAL PROXIES (modern country = historical territory)
# These use L0 of the modern successor state
# ============================================================

message("\n=== STEP 5: Historical proxies (L0) ===\n")

proxy_mapping <- tibble::tribble(
  ~polity_name,                              ~iso3,
  "Basutoland",                               "LSO",
  "Bechuanaland Protectorate",                "BWA",
  "Belgian Congo",                            "COD",
  "Cameroon (Kamerun)",                       "CMR",
  "Dutch East Indies (Indonesia)",            "IDN",
  "Dutch Guayana",                            "SUR",
  "German West Africa",                       "TGO",
  "Gold Coast",                               "GHA",
  "Italian Libia Cyrenaica (Lybia)",          "LBY",
  "New Hebrides",                             "VUT",
  "Nyasaland Protectorate (Malawi)",          "MWI",
  "Spanish Guinea",                           "GNQ",
)

proxy_results <- list()
for (i in seq_len(nrow(proxy_mapping))) {
  row <- proxy_mapping[i, ]
  message("Processing proxy: ", row$polity_name, " (", row$iso3, ")")
  geom <- gadm_l0(row$iso3)
  result <- wrap_result(row$polity_name, geom)
  if (!is.null(result)) {
    proxy_results[[length(proxy_results) + 1]] <- result
  }
}


# ============================================================
# SPECIAL CASES (China mainland, etc.)
# ============================================================

message("\n=== STEP 6: Special cases ===\n")

special_results <- list()

# China, mainland = CHN L0 (GADM China includes Taiwan as a province, but
# the L0 boundary is the whole thing. For now just use CHN L0 since Taiwan
# has its own CShapes entry already.)
message("Processing: China, mainland (CHN L0)")
china <- gadm_l0("CHN")
r <- wrap_result("China, mainland", china)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# British Malaya = Peninsular Malaysia states
message("Processing: British Malaya (MYS L1 peninsular)")
brit_malaya <- gadm_l1_filter("MYS",
  c("Johor", "Kedah", "Kelantan", "Perak", "Selangor",
    "Negeri Sembilan", "Pahang", "Perlis", "Terengganu",
    "Pulau Pinang", "Melaka", "Penang", "Malacca"))
r <- wrap_result("British Malaya", brit_malaya)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# British Protectorate (British Somaliland) = NW Somalia
message("Processing: British Protectorate (British Somaliland) (SOM L1)")
brit_somaliland <- gadm_l1_filter("SOM",
  c("Awdal", "Woqooyi Galbeed", "Togdheer", "Sanaag", "Sool"))
r <- wrap_result("British Protectorate (British Somaliland)", brit_somaliland)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Italian Somaliland = Southern Somalia
message("Processing: Italian Somaliland (SOM L1)")
ital_somaliland <- gadm_l1_filter("SOM",
  c("Banadir", "Shabeellaha Hoose", "Shabeellaha Dhexe",
    "Bay", "Bakool", "Gedo", "Jubbada Hoose", "Jubbada Dhexe",
    "Hiiraan", "Mudug", "Galgaduud", "Nugaal"))
r <- wrap_result("Italian Somaliland", ital_somaliland)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# German East Africa (Tanganyika) = mainland Tanzania
message("Processing: German East Africa (Tanganyika) (TZA L1 mainland)")
# Download TZA, exclude Zanzibar/Pemba (island regions)
tza <- download_gadm("TZA", level = 1)
if (!is.null(tza)) {
  tanganyika <- tza |>
    dplyr::filter(!grepl("Zanzibar|Pemba|Unguja", NAME_1, ignore.case = TRUE)) |>
    sf::st_union() |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = x)
  r <- wrap_result("German East Africa (Tanganyika)", tanganyika)
  if (!is.null(r)) special_results[[length(special_results) + 1]] <- r
}

# British Cameroon = parts of Nigeria and Cameroon border regions
# Simplification: use modern Cameroon as proxy (close enough for historical)
message("Processing: British Cameroon (CMR+NGA border - using CMR L0 proxy)")
brit_cameroon <- gadm_l0("CMR")
r <- wrap_result("British Cameroon", brit_cameroon)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Brunei (greater) = Brunei + parts of Sabah/Sarawak
message("Processing: Brunei (greater) (BRN + MYS parts)")
brunei_greater <- gadm_combine_l0(c("BRN"))
# Just use Brunei L0 as the core - historical extent is approximate
r <- wrap_result("Brunei (greater)", brunei_greater)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Straits Settlement = Singapore + Penang + Malacca
message("Processing: Straits Settlement (SGP + MYS L1)")
sgp <- gadm_l0("SGP")
penang_malacca <- gadm_l1_filter("MYS", c("Pulau Pinang", "Penang", "Melaka", "Malacca"))
if (!is.null(sgp) && !is.null(penang_malacca)) {
  straits <- dplyr::bind_rows(sgp, penang_malacca) |>
    sf::st_union() |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = x)
  r <- wrap_result("Straits Settlement", straits)
  if (!is.null(r)) special_results[[length(special_results) + 1]] <- r
}

# Aden = British Aden Protectorate territory (southern Yemen, not just city)
# Includes: Adan, Lahij, Abyan, Shabwah, Hadramawt, Al Mahrah
message("Processing: Aden (YEM L1 southern protectorate)")
aden <- gadm_l1_filter("YEM",
  c("Adan", "Aden", "Lahij", "Abyan", "Shabwah", "Hadramawt", "Mahrah"))
r <- wrap_result("Aden", aden)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Portuguese India = Goa + Daman + Diu
message("Processing: Portuguese India (IND L1)")
port_india <- gadm_l1_filter("IND", c("Goa"))
r <- wrap_result("Portuguese India", port_india)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Khiva Khanate = UZB Xorazm + TKM Daşoguz
message("Processing: Khiva (UZB Xorazm + TKM Daşoguz)")
khiva_uzb <- gadm_l1_filter("UZB", c("Xorazm"))
khiva_tkm <- gadm_l1_filter("TKM", c("Da\u015foguz", "Dashoguz"))
khiva_parts <- Filter(\(x) !is.null(x), list(khiva_uzb, khiva_tkm))
if (length(khiva_parts) > 0) {
  khiva <- dplyr::bind_rows(khiva_parts) |>
    sf::st_union() |>
    sf::st_cast("MULTIPOLYGON") |>
    sf::st_as_sf() |>
    dplyr::rename(geometry = x)
  r <- wrap_result("Khiva", khiva)
  if (!is.null(r)) special_results[[length(special_results) + 1]] <- r
}

# ---- Backward-compatibility period proxies ----
# CShapes 2.0 merged these time periods; use modern country as geometry proxy
bc_proxy_mapping <- list(
  list("Afghanistan (to 1888)",        "AFG"),
  list("Cameroon (1960-1961)",         "CMR"),
  list("Egypt (1922-1925)",            "EGY"),
  list("Malaysia (1957-1963)",         "MYS"),
  list("Mauritania (1960-1975)",       "MRT"),
  list("Montenegro (1913-1915)",       "MNE"),
  list("Nigeria (1960-1961)",          "NGA"),
  list("Serbia (1913-1915)",           "SRB"),
  list("Syrian Arab Republic (1946-1967)", "SYR"),
  list("Tanzania (1961-1964)",         "TZA"),
  list("Morocco (to 1958)",            "MAR"),
  list("Saudi Arabia (1924-1932)",     "SAU"),
  list("Korea (to 1910)",              "KOR"),
  list("Serbia (1816-1913)",           "SRB")
)

for (entry in bc_proxy_mapping) {
  polity_name <- entry[[1]]
  iso3 <- entry[[2]]
  message("Processing BC proxy: ", polity_name, " (", iso3, ")")
  geom <- gadm_l0(iso3)
  result <- wrap_result(polity_name, geom)
  if (!is.null(result)) {
    special_results[[length(special_results) + 1]] <- result
  }
}

# Serbia and Montenegro combine
message("Processing: Serbia and Montenegro (SRB+MNE)")
srb_mne <- gadm_combine_l0(c("SRB", "MNE"))
r <- wrap_result("Serbia and Montenegro", srb_mne)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Sudan (1956-2011) = modern Sudan + South Sudan
message("Processing: Sudan (1956-2011) (SDN+SSD)")
sudan_old <- gadm_combine_l0(c("SDN", "SSD"))
r <- wrap_result("Sudan (1956-2011)", sudan_old)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Van Diemen's Land (Tasmania) = AUS L1 Tasmania
message("Processing: Van Diemen's Land (AUS L1 Tasmania)")
tasmania <- gadm_l1_filter("AUS", c("Tasmania"))
r <- wrap_result("Van Diemen's Land (Tasmania)", tasmania)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Zanzibar = TZA L1 Zanzibar islands
message("Processing: Zanzibar (TZA L1)")
zanzibar <- gadm_l1_filter("TZA", c("Zanzibar", "Pemba", "Unguja"))
r <- wrap_result("Zanzibar", zanzibar)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Leeward Islands = combine of Caribbean islands
message("Processing: Leeward Islands (ATG+DMA+KNA+MSR+VGB)")
leeward <- gadm_combine_l0(c("ATG", "DMA", "KNA", "MSR", "VGB"))
r <- wrap_result(
  "Leeward Islands (Antigua, Dominica, St.Christopher, Montserrat, Nevis, Virgin Island)",
  leeward)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r

# Åland Islands - has its own ISO code ALA in GADM
message("Processing: Åland Islands (ALA L0)")
aland <- gadm_l0("ALA")
r <- wrap_result("\u00c5land Islands", aland)
if (!is.null(r)) special_results[[length(special_results) + 1]] <- r


# ============================================================
# NATURAL EARTH FALLBACKS
# For territories not in GADM (tiny islands, Antarctic claims)
# ============================================================

message("\n=== STEP 7: Natural Earth fallbacks ===\n")

natearth_results <- list()

# Try to use rnaturalearth if available
if (requireNamespace("rnaturalearth", quietly = TRUE)) {
  message("Loading Natural Earth data...")
  ne_countries <- rnaturalearth::ne_countries(
    scale = 50, returnclass = "sf"
  )

  ne_mapping <- tibble::tribble(
    ~polity_name,                                ~ne_field, ~ne_value,
    "Antarctica",                                "iso_a3",  "ATA",
    "Holy See",                                  "name",    "Vatican",
    "Monaco",                                    "iso_a3",  "MCO",
    "Palestine",                                 "name",    "Palestine",
    "United States Minor Outlying Islands",       "name",    "U.S. Minor",
  )

  for (i in seq_len(nrow(ne_mapping))) {
    row <- ne_mapping[i, ]
    message("Processing NatEarth: ", row$polity_name)
    matched <- ne_countries |>
      dplyr::filter(grepl(row$ne_value, .data[[row$ne_field]], ignore.case = TRUE))
    if (nrow(matched) > 0) {
      geom <- matched |>
        sf::st_union() |>
        sf::st_cast("MULTIPOLYGON") |>
        sf::st_as_sf() |>
        dplyr::rename(geometry = x)
      r <- wrap_result(row$polity_name, geom)
      if (!is.null(r)) natearth_results[[length(natearth_results) + 1]] <- r
    } else {
      message("  WARNING: No NatEarth match for ", row$polity_name)
    }
  }
} else {
  message("rnaturalearth not available, skipping NatEarth fallbacks")
  message("Install with: install.packages('rnaturalearth')")
}


# ============================================================
# COMBINE ALL RESULTS AND SAVE
# ============================================================

message("\n=== Combining all results ===\n")

all_results <- c(l0_results, l1_results, l2_results, combine_results,
                 proxy_results, special_results, natearth_results)

if (length(all_results) == 0) {
  stop("No geometries were downloaded. Check network connectivity.")
}

gadm_all <- dplyr::bind_rows(all_results)

message("Total polities with GADM geometry: ", nrow(gadm_all))
message("Polity names: ")
message(paste("  -", gadm_all$polity_name, collapse = "\n"))

# Ensure CRS is WGS84
gadm_all <- sf::st_transform(gadm_all, crs = 4326)

# Save as GeoPackage
message("\nSaving to: ", output_path)
sf::st_write(gadm_all, output_path, delete_dsn = TRUE, quiet = TRUE)
message("Done! Saved ", nrow(gadm_all), " polity geometries.")

# Optional coverage summary (requires polygon source mapping file)
mapping_path <- here::here("inst", "extdata", "output", "polities_research",
                           "16_polygon_source_mapping.csv")
if (file.exists(mapping_path)) {
  all_mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)
  covered <- all_mapping |>
    dplyr::filter(polity_name %in% gadm_all$polity_name)
  not_covered <- all_mapping |>
    dplyr::filter(!polity_name %in% gadm_all$polity_name)

  message("\n=== Coverage Summary ===")
  message("Mapped polities in source mapping: ", nrow(all_mapping))
  message("Successfully downloaded: ", nrow(covered))
  message("Still missing: ", nrow(not_covered))
  if (nrow(not_covered) > 0) {
    message("Missing polities:")
    message(paste("  -", not_covered$polity_name,
                  " (", not_covered$polygon_source, ")", collapse = "\n"))
  }
}
