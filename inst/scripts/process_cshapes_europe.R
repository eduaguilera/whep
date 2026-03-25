# Process CShapes-Europe geometries for WHEP polities pipeline
#
# This script reads ~/Downloads/CShapes-Europe.geojson and produces a GPKG
# file with geometries mapped to WHEP pipeline common_names.
#
# CShapes-Europe (Schvitz et al.) provides historical European boundaries
# 1806-2023, complementing CShapes 2.0 (1886-2019) with pre-1886 data and
# finer-grained European states (small German/Italian principalities).
#
# Priority in the pipeline: CShapes 2.0 > CShapes-Europe > GADM
# So CShapes-Europe mainly adds value for pre-1886 European polities.
#
# Usage:
#   Rscript inst/scripts/process_cshapes_europe.R
#
# Input:  ~/Downloads/CShapes-Europe.geojson
# Output: data-raw/polities_inputs/cshapes_europe_geometries.gpkg

library(sf)
library(dplyr)

sf::sf_use_s2(FALSE)

input_path <- fs::path_expand("~/Downloads/CShapes-Europe.geojson")
output_path <- here::here(
  "data-raw", "polities_inputs", "cshapes_europe_geometries.gpkg"
)

stopifnot(file.exists(input_path))

message("Reading CShapes-Europe from: ", input_path)
cse <- st_read(input_path, quiet = TRUE) |>
  st_make_valid()

message("  ", nrow(cse), " rows, ", length(unique(cse$Name)), " unique polities")

# ---- Helper: pick geometry with largest area for a polity ----
pick_largest_geometry <- function(data, polity_name) {
  rows <- data |> filter(Name == polity_name)
  if (nrow(rows) == 0) return(NULL)
  # Pick the period with the largest area
  areas <- st_area(rows)
  best <- rows[which.max(areas), ]
  best |>
    st_geometry() |>
    st_union() |>
    st_cast("MULTIPOLYGON")
}

# ---- Helper: union geometries from multiple polities ----
union_geometries <- function(data, polity_names) {
  rows <- data |> filter(Name %in% polity_names)
  if (nrow(rows) == 0) return(NULL)
  # For each polity, pick the largest-area period, then union all
  geoms <- lapply(polity_names, function(n) {
    pick_largest_geometry(data, n)
  })
  geoms <- geoms[!sapply(geoms, is.null)]
  if (length(geoms) == 0) return(NULL)
  combined <- do.call(c, geoms)
  combined |>
    st_union() |>
    st_cast("MULTIPOLYGON")
}

# ---- Build the mapping from CShapes-Europe names to pipeline names ----
# Category 1: Existing polities - upgrade from GADM proxy
# These use identity mapping (CSE name == pipeline common_name) or explicit remap
existing_upgrades <- tribble(
  ~cse_name,                    ~pipeline_name,
  "Bavaria",                    "Bavaria",
  "Saxony",                     "Saxony",
  "Hanover",                    "Hanover",
  "Baden",                      "Baden",
  "Württemberg",               "Württemberg",
  "Bremen",                     "Bremen (city-state)",
  "Oldenburg",                  "Oldenburg",
  "Ottoman Empire",             "Ottoman Empire",
  "Papal States",               "Papal States",
  "Modena",                     "Duchy Modena",
  "Parma",                      "Duchy Parma",
  "Liechtenstein",              "Liechtenstein",
  "San Marino",                 "San Marino",
  "Monaco",                     "Monaco",
  "Andorra",                    "Andorra",
)

# Category 2: Existing polities - compound geometry (union multiple CSE polities)
# These already exist in the pipeline with GADM proxy
compound_existing <- list(
  list(
    pipeline_name = "Mecklenburg",
    cse_names = c("Mecklenburg-Schwerin", "Mecklenburg-Strelitz")
  ),
  list(
    pipeline_name = "Thuringia",
    cse_names = c(
      "Saxe-Weimar", "Saxe-Altenburg", "Saxe-Meiningen",
      "Saxe-Coburg-Gotha", "Saxe-Coburg-Saalfeld",
      "Saxe-Gotha-Altenberg", "Saxe-Hildburgchausen",
      "Reuss", "Schaumburg Lippe"
    )
  ),
  list(
    pipeline_name = "Hesse",
    cse_names = c("Hesse-Darmstadt (Ducal", "Hesse-Kassel (Electoral)")
  )
)

# Category 3: New polities to add
# Italian states
new_italian <- tribble(
  ~cse_name,           ~pipeline_name,     ~start_year, ~end_year,
  "Kingdom of Naples", "Kingdom of Naples", 1816L,       1861L,
  "Piedmont",          "Piedmont",          1816L,       1861L,
  "Lucca",             "Lucca",             1816L,       1847L,
  "Massa",             "Massa",             1816L,       1829L,
)

# German states (not already aggregated into existing pipeline polities)
new_german <- tribble(
  ~cse_name,                   ~pipeline_name,              ~start_year, ~end_year,
  "Frankfurt",                 "Frankfurt (Free City)",      1816L,       1866L,
  "Nassau",                    "Nassau",                     1816L,       1866L,
  "Hesse-Homburg",             "Hesse-Homburg",              1816L,       1866L,
  "Anhalt",                    "Anhalt",                     1864L,       1870L,
  "Anhalt-Bernberg",           "Anhalt-Bernburg",            1816L,       1864L,
  "Anhalt-Dessau",             "Anhalt-Dessau",              1816L,       1863L,
  "Lippe-Detmold",             "Lippe-Detmold",              1816L,       1870L,
  "Waldeck",                   "Waldeck",                    1816L,       1870L,
  "Wolfenbuttel",              "Wolfenbüttel",               1816L,       1870L,
  "Hohenzollern-Hechingen",    "Hohenzollern-Hechingen",     1816L,       1850L,
  "Hohenzollern-Sigmaringen",  "Hohenzollern-Sigmaringen",   1816L,       1850L,
  "Hohengeroldseck",           "Hohengeroldseck",            1816L,       1818L,
)

# Thuringian states (individual, alongside the Thuringia aggregate)
new_thuringian <- tribble(
  ~cse_name,               ~pipeline_name,         ~start_year, ~end_year,
  "Saxe-Weimar",           "Saxe-Weimar",           1816L,       1870L,
  "Saxe-Altenburg",        "Saxe-Altenburg",        1827L,       1870L,
  "Saxe-Coburg-Gotha",     "Saxe-Coburg-Gotha",     1827L,       1870L,
  "Saxe-Meiningen",        "Saxe-Meiningen",        1816L,       1870L,
  "Saxe-Coburg-Saalfeld",  "Saxe-Coburg-Saalfeld",  1816L,       1826L,
  "Saxe-Gotha-Altenberg",  "Saxe-Gotha-Altenberg",  1816L,       1826L,
  "Saxe-Hildburgchausen",  "Saxe-Hildburgchausen",  1816L,       1826L,
  "Reuss",                 "Reuss",                  1816L,       1870L,
  "Schaumburg Lippe",      "Schaumburg-Lippe",       1816L,       1870L,
)

# Other notable new polities
# Note: Danzig, Bosnia, Herzegovina are excluded because they already have
# CShapes 2.0 geometry as "Danzig (1919-1938)", "Bosnia (to 1908)", etc.
new_other <- tribble(
  ~cse_name,     ~pipeline_name,       ~start_year, ~end_year,
  "Cracow",      "Cracow (Free City)",  1816L,       1846L,
  "Chechens",    "Chechnya",            1816L,       1857L,
  "Circassia",   "Circassia",           1816L,       1861L,
)

all_new <- bind_rows(new_italian, new_german, new_thuringian, new_other)

# ---- Process geometries ----
results <- list()

# Process Category 1: direct single-polity mappings
message("\n=== Processing existing polity upgrades ===")
for (i in seq_len(nrow(existing_upgrades))) {
  cse_name <- existing_upgrades$cse_name[i]
  pipeline_name <- existing_upgrades$pipeline_name[i]
  geom <- pick_largest_geometry(cse, cse_name)
  if (!is.null(geom)) {
    results[[length(results) + 1]] <- st_sf(
      polity_name = pipeline_name,
      geometry = geom
    )
    message("  OK: ", cse_name, " -> ", pipeline_name)
  } else {
    message("  MISSING: ", cse_name)
  }
}

# Process Category 2: compound union mappings
message("\n=== Processing compound geometry upgrades ===")
for (comp in compound_existing) {
  geom <- union_geometries(cse, comp$cse_names)
  if (!is.null(geom)) {
    results[[length(results) + 1]] <- st_sf(
      polity_name = comp$pipeline_name,
      geometry = geom
    )
    message("  OK: ", paste(comp$cse_names, collapse = " + "),
            " -> ", comp$pipeline_name)
  } else {
    message("  MISSING: ", paste(comp$cse_names, collapse = " + "))
  }
}

# Process Category 3: new polities (single geometry each)
message("\n=== Processing new polities ===")
for (i in seq_len(nrow(all_new))) {
  cse_name <- all_new$cse_name[i]
  pipeline_name <- all_new$pipeline_name[i]
  geom <- pick_largest_geometry(cse, cse_name)
  if (!is.null(geom)) {
    results[[length(results) + 1]] <- st_sf(
      polity_name = pipeline_name,
      geometry = geom
    )
    message("  OK: ", cse_name, " -> ", pipeline_name)
  } else {
    message("  MISSING: ", cse_name)
  }
}

# ---- Combine and save ----
combined <- do.call(rbind, results) |>
  st_cast("MULTIPOLYGON") |>
  st_transform(crs = 4326) |>
  st_make_valid()

message("\n=== Summary ===")
message("  Total polities with geometry: ", nrow(combined))
message("  Geometry types: ", paste(unique(st_geometry_type(combined)),
                                     collapse = ", "))

# Verify no duplicates
dupes <- combined$polity_name[duplicated(combined$polity_name)]
if (length(dupes) > 0) {
  warning("Duplicate polity names: ", paste(dupes, collapse = ", "))
}

message("\nWriting to: ", output_path)
st_write(combined, output_path, delete_dsn = TRUE, quiet = TRUE)
message("Done. File size: ",
        round(file.info(output_path)$size / 1024 / 1024, 1), " MB")
