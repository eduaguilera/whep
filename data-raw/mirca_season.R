# Build inst/extdata/mirca_season.csv: per-(item_cbs_code) mean crop cycle
# length in MONTHS, the occupation-time weight used by the hectare-year land
# extension (build_hayr_land_extension()).
#
# Source: MIRCA2000 (Portmann et al. 2010) condensed cropping calendars, which
# give, per spatial unit and per crop class, up to five sub-crops each with a
# growing area (ha) and start/end month. The cycle length of a sub-crop is
# ((end - start) mod 12) + 1 months; the per-class length is the area-weighted
# mean over all sub-crops and units (rainfed and irrigated combined). MIRCA's
# 26 crop classes are mapped to FAOSTAT production items via the Monfreda-FAO-
# MIRCA crosswalk, then aggregated to commodity-balance items (item_cbs_code).
#
# Crops outside MIRCA's 21 named classes fall into "Others annual" (~4.7 mo) or
# "Others perennial" (~12 mo); perennials (oil palm, citrus, cocoa, coffee,
# grapes, sugar cane) come out at the full 12 months, as they should.
#
# Inputs (paths via env vars, defaulting to the local LandInG tree):
#   WHEP_MIRCA_DIR        condensed_cropping_calendars/ directory
#   WHEP_MIRCA_NAMES      mirca_names.txt (class order = class number)
#   WHEP_MIRCA_BRIDGE     crop_types_Monfreda_FAOSTAT_MIRCA.csv

library(data.table)
library(readr)
library(dplyr)
devtools::load_all(".")

mirca_dir <- Sys.getenv(
  "WHEP_MIRCA_DIR",
  "/home/usuario/LandInG/landuse/MIRCA2000/condensed_cropping_calendars"
)
names_file <- Sys.getenv(
  "WHEP_MIRCA_NAMES",
  "/home/usuario/LandInG/landuse/MIRCA2000/mirca_names.txt"
)
bridge_file <- Sys.getenv(
  "WHEP_MIRCA_BRIDGE",
  "/home/usuario/LandInG/landuse/crop_types_Monfreda_FAOSTAT_MIRCA.csv"
)

mirca_names <- readLines(names_file)
class_names <- data.table(
  mirca_class = seq_along(mirca_names),
  mirca_name = mirca_names
)

# Parse one condensed calendar file: unit, class, n_sc, then (area start end)
# repeated for up to five sub-crops. Variable column count, four header lines.
read_ccc <- function(path) {
  raw <- fread(
    path,
    skip = 4,
    fill = TRUE,
    header = FALSE,
    sep = " ",
    blank.lines.skip = TRUE
  )
  setnames(raw, 1:3, c("unit", "mirca_class", "n_sc"))
  raw[, mirca_class := as.integer(mirca_class)]
  blocks <- list()
  for (sc in 1:5) {
    cols <- 3L + (sc - 1L) * 3L + 1:3
    if (max(cols) > ncol(raw)) {
      break
    }
    blocks[[sc]] <- raw[, .(
      mirca_class,
      area = as.numeric(get(paste0("V", cols[1]))),
      start = as.integer(get(paste0("V", cols[2]))),
      end = as.integer(get(paste0("V", cols[3])))
    )]
  }
  data.table::rbindlist(blocks)
}

ccc <- data.table::rbindlist(list(
  read_ccc(file.path(mirca_dir, "cropping_calendar_rainfed.txt")),
  read_ccc(file.path(mirca_dir, "cropping_calendar_irrigated.txt"))
))
ccc <- ccc[!is.na(area) & area > 0 & !is.na(start) & !is.na(end)]
ccc[, length_months := ((end - start) %% 12L) + 1L]

class_length <- ccc[,
  .(season_months = sum(area * length_months) / sum(area)),
  by = mirca_class
] |>
  merge(class_names, by = "mirca_class")

# FAOSTAT production item -> MIRCA class name -> item_cbs_code
bridge <- read_csv(bridge_file, show_col_types = FALSE)
names(bridge) <- c(
  "monfreda",
  "fao_name",
  "item_prod_code",
  "notes",
  "mirca_name"
)
bridge <- bridge |>
  dplyr::mutate(
    item_prod_code = suppressWarnings(as.integer(item_prod_code))
  ) |>
  dplyr::filter(!is.na(item_prod_code), mirca_name %in% mirca_names)

prod_to_cbs <- whep::items_prod_full |>
  dplyr::transmute(
    item_prod_code = suppressWarnings(as.integer(item_prod_code)),
    item_cbs_code = suppressWarnings(as.integer(item_cbs_code))
  ) |>
  dplyr::filter(!is.na(item_prod_code), !is.na(item_cbs_code)) |>
  dplyr::distinct()

class_lookup <- tibble::as_tibble(class_length) |>
  dplyr::select(mirca_name, season_months)

mirca_season <- bridge |>
  dplyr::left_join(class_lookup, by = "mirca_name") |>
  dplyr::inner_join(prod_to_cbs, by = "item_prod_code") |>
  dplyr::summarise(
    season_months = round(mean(season_months, na.rm = TRUE), 2),
    .by = item_cbs_code
  ) |>
  dplyr::filter(!is.na(season_months))

# Standalone primary-crop CBS codes the Monfreda-FAO-MIRCA bridge misses,
# because their bridge entry maps a processing/aggregate FAO item to an
# aggregate CBS code (e.g. coconut prod 249 -> CBS 2560), never reaching the
# standalone CBS code the harvested data uses (248). Fodder crops (2000-2003)
# are dropped entirely because their Monfreda bridge rows carry a blank FAO
# code. Assign each its MIRCA class length directly so it is not left to the
# median default downstream.
supplementary <- tibble::tribble(
  ~item_cbs_code, ~mirca_name,
  248L, "Others perennial", # Coconuts
  310L, "Others perennial", # Kapok fruit
  772L, "Others annual", # Linum (flax fibre)
  776L, "Others annual", # Hemp
  2000L, "Fodder grasses", # Fodder cereal/grasses
  2001L, "Fodder grasses", # Fodder legumes
  2002L, "Others annual", # Fodder roots and vegetables
  2003L, "Fodder grasses" # Fodder mix
) |>
  dplyr::left_join(class_lookup, by = "mirca_name") |>
  dplyr::transmute(item_cbs_code, season_months = round(season_months, 2))

mirca_season <- mirca_season |>
  dplyr::filter(!item_cbs_code %in% supplementary$item_cbs_code) |>
  dplyr::bind_rows(supplementary) |>
  dplyr::arrange(item_cbs_code)

write_csv(mirca_season, "inst/extdata/mirca_season.csv")
