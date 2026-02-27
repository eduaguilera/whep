# -----------------------------------------------------------------------
# prepare_livestock_spatialize_inputs.R
#
# Reads raw data to prepare inputs for build_gridded_livestock().
# Produces parquet files consumed by run_livestock_spatialize.R.
#
# Data sources and references:
#   1. FAOSTAT Production_Livestock (FAO 2024)
#      Element 5111 = "Stocks" (live animals, heads), 1961-present.
#      22 species, ~200 countries.
#      https://www.fao.org/faostat/en/#data/QCL
#
#   2. FAOSTAT Emissions_livestock (FAO 2024)
#      Enteric fermentation CH4, manure management CH4/N2O,
#      manure applied/pasture N content, by species. 1961-present.
#      https://www.fao.org/faostat/en/#data/GLE
#
#   3. LUH2 v2h (Hurtt et al. 2020)
#      states.nc: pasture (pastr) + rangeland (range) fractions
#      management.nc: irrigation fractions per crop type
#      staticData_quarterdeg.nc: cell area (carea)
#      0.25 deg, 850-2022. doi:10.5194/gmd-13-5425-2020
#
#   4. West et al. (2014) Manure_Westetal2014
#      Gridded manure N application rates per crop (kg N/ha),
#      ~5 arcmin (~10 km). Static reference (global).
#      doi:10.1038/nclimate2353
#
#   5. IPCC (2006/2019) Nex rates for N excretion defaults,
#      emission factors (incorporated via livestock_mapping.csv).
#
# Pre-FAOSTAT methodology (before 1961):
#   Livestock stocks are back-extrapolated using 1961 species
#   composition scaled by LUH2 pasture+rangeland trends per country.
#
# Outputs (to L_files/whep/inputs/):
#   - livestock_country_data.parquet:
#       year, area_code, species_group, heads, enteric_ch4_kt,
#       manure_ch4_kt, manure_n2o_kt, manure_n_mg
#   - gridded_pasture.parquet:
#       lon, lat, year, pasture_ha, rangeland_ha
#   - manure_pattern.parquet:
#       lon, lat, manure_intensity
# -----------------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)

# ==== Configuration ====================================================

l_files_dir <- "C:/Users/53272530E/OneDrive/L_files"
output_dir <- file.path(l_files_dir, "whep", "inputs")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

target_res <- 0.5
year_range <- 1850L:2022L

# ==== Helper: find extdata =============================================

.find_extdata_file <- function(filename) {
  local_path <- file.path("inst", "extdata", filename)
  if (file.exists(local_path)) return(local_path)
  pkg_path <- system.file("extdata", filename, package = "whep")
  if (nchar(pkg_path) > 0) return(pkg_path)
  cli::cli_abort("Cannot find {.file {filename}} in inst/extdata.")
}

# ==== Helper: raster to tibble =========================================

.raster_to_tibble <- function(r, value_name) {
  coords <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  vals <- terra::values(r)[, 1]
  tibble::tibble(
    lon = round(coords[, 1], 2),
    lat = round(coords[, 2], 2),
    !!value_name := vals
  )
}

# ==== Helper: read LUH2 variable ======================================

.read_luh2_variable <- function(nc_path, varname, time_idx) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))

  lat <- ncdf4::ncvar_get(nc, "lat")
  vals <- ncdf4::ncvar_get(
    nc, varname,
    start = c(1, 1, time_idx), count = c(-1, -1, 1)
  )
  vals[is.na(vals)] <- 0

  r <- terra::rast(
    t(vals),
    extent = terra::ext(-180, 180, -90, 90)
  )
  # Flip if latitude is ascending

  if (lat[1] < lat[length(lat)]) {
    r <- terra::flip(r, direction = "vertical")
  }
  r
}

# ==== Helper: FAOSTAT → WHEP area_code mapping =======================
#
# FAOSTAT "Area Code" is NOT the same as WHEP polity area_code.
# The safe path is:
#   FAOSTAT area_name → ISO3 → whep::polities$area_code
# We handle known name mismatches with a manual lookup.

.map_fao_to_whep <- function(fao_area_name) {
  pol <- whep::polities

  # Manual name corrections: FAOSTAT name → WHEP area_name
  name_fixes <- c(
    "China"                                    = "China, mainland",
    "Czechia"                                  = "Czech Republic",
    "The former Yugoslav Republic of Macedonia" = "North Macedonia",
    # encoding-safe versions of accented names
    "C\u00f4te d'Ivoire"                       = "C\u00f4te d'Ivoire",
    "R\u00e9union"                             = "R\u00e9union",
    "T\u00fcrkiye"                             = "T\u00fcrkiye",
    "Occupied Palestinian Territory"            = "Palestine",
    "Netherlands Antilles (former)"             = NA_character_,
    "Pacific Islands Trust Territory"          = NA_character_
  )

  # Predecessor entities: the ones that already exist in polities
  # (USSR=228, Yugoslav SFR=248, Czechoslovakia=51, Belgium-Luxembourg=15,
  # Serbia and Montenegro=186) will match by name directly.
  # These two aren't in polities, so assign conventional codes
  # matching the crop pipeline:
  predecessor_codes <- c(
    "Ethiopia PDR"   = 62L,
    "Sudan (former)" = 206L
  )

  out <- rep(NA_real_, length(fao_area_name))

  for (i in seq_along(fao_area_name)) {
    nm <- fao_area_name[i]
    if (is.na(nm)) next

    # Check predecessors first
    pred_code <- predecessor_codes[nm]
    if (!is.na(pred_code)) {
      out[i] <- pred_code
      next
    }

    # Try direct match
    idx <- match(nm, pol[["area_name"]])
    if (!is.na(idx)) {
      out[i] <- pol[["area_code"]][idx]
      next
    }

    # Try manual fix
    fixed <- name_fixes[nm]
    if (!is.na(fixed) && nchar(fixed) > 0) {
      idx2 <- match(fixed, pol[["area_name"]])
      if (!is.na(idx2)) {
        out[i] <- pol[["area_code"]][idx2]
        next
      }
    }

    # Try case-insensitive matching (guard against encoding issues)
    idx3 <- tryCatch(
      which(tolower(pol[["area_name"]]) == tolower(nm)),
      error = function(e) integer(0)
    )
    if (length(idx3) == 1L) {
      out[i] <- pol[["area_code"]][idx3[1]]
    }
  }

  as.integer(out)
}


# ==== 1. Read livestock mapping ========================================

cli::cli_h1("Preparing livestock spatialization inputs")

mapping_path <- .find_extdata_file("livestock_mapping.csv")
livestock_mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)

cli::cli_alert_success(
  "Livestock mapping: {nrow(livestock_mapping)} species in ",
  "{n_distinct(livestock_mapping$species_group)} groups"
)

# ==== 2. Read FAOSTAT livestock stocks =================================

cli::cli_h2("FAOSTAT livestock stocks")

fao_csv <- file.path(
  l_files_dir,
  "FAOSTAT/Production_Livestock_E_All_Data_(Normalized).csv"
)

if (!file.exists(fao_csv)) {
  cli::cli_abort("FAOSTAT livestock file not found: {fao_csv}")
}

fao_raw <- data.table::fread(fao_csv, encoding = "Latin-1") |>
  tibble::as_tibble()

stocks <- fao_raw |>
  filter(
    `Element Code` == 5111L,
    `Item Code` %in% livestock_mapping$item_code,
    !is.na(Value),
    Value > 0
  ) |>
  transmute(
    area_code = .map_fao_to_whep(Area),
    item_code = as.integer(`Item Code`),
    year = as.integer(Year),
    heads = Value
  ) |>
  filter(!is.na(area_code)) |>
  inner_join(
    select(livestock_mapping, item_code, species_group, nex_kg_n_head),
    by = "item_code"
  )

# Aggregate to species_group level
stocks_grouped <- stocks |>
  summarise(
    heads = sum(heads, na.rm = TRUE),
    # Weighted mean Nex for the group
    nex_kg_n_head = sum(heads * nex_kg_n_head) / sum(heads),
    .by = c(year, area_code, species_group)
  )

n_grp <- n_distinct(stocks_grouped$species_group)
n_ctry <- n_distinct(stocks_grouped$area_code)
cli::cli_alert_success(
  "Stocks: {n_grp} groups, {n_ctry} countries, ",
  "{min(stocks_grouped$year)}-{max(stocks_grouped$year)}"
)


# ==== 3. Read FAOSTAT emissions ========================================

cli::cli_h2("FAOSTAT livestock emissions")

emi_csv <- file.path(
  l_files_dir,
  "FAOSTAT/Emissions_livestock_E_All_Data_(Normalized).csv"
)

if (!file.exists(emi_csv)) {
  cli::cli_alert_warning(
    "Emissions_livestock not found - will use Nex-based manure N only"
  )
  emissions <- NULL
} else {
  emi_raw <- data.table::fread(emi_csv, encoding = "Latin-1") |>
    tibble::as_tibble()

  # Species mapping for emissions (FAOSTAT Emissions uses different
  # item names than Production_Livestock)
  emi_species_map <- tribble(
    ~emi_item, ~species_group,
    "Cattle, dairy", "cattle",
    "Cattle, non-dairy", "cattle",
    "Buffalo", "buffalo",
    "Sheep", "sheep_goats",
    "Goats", "sheep_goats",
    "Swine, market", "pigs",
    "Swine, breeding", "pigs",
    "Chickens, layers", "poultry",
    "Chickens, broilers", "poultry",
    "Ducks", "poultry",
    "Turkeys", "poultry",
    "Horses", "equines",
    "Asses", "equines",
    "Mules and hinnies", "equines",
    "Mules and Asses", "equines",
    "Camels", "camels",
    "Camels and Llamas", "camels",
  )

  # Extract enteric CH4 (kt)
  enteric_ch4 <- emi_raw |>
    filter(Element == "Enteric fermentation (Emissions CH4)") |>
    transmute(
      area_code = .map_fao_to_whep(Area),
      year = as.integer(Year),
      emi_item = Item,
      enteric_ch4_kt = Value  # kilotonnes
    ) |>
    filter(!is.na(area_code)) |>
    inner_join(emi_species_map, by = "emi_item") |>
    summarise(
      enteric_ch4_kt = sum(enteric_ch4_kt, na.rm = TRUE),
      .by = c(year, area_code, species_group)
    )

  # Extract manure management CH4 (kt)
  manure_ch4 <- emi_raw |>
    filter(Element == "Manure management (Emissions CH4)") |>
    transmute(
      area_code = .map_fao_to_whep(Area),
      year = as.integer(Year),
      emi_item = Item,
      manure_ch4_kt = Value
    ) |>
    filter(!is.na(area_code)) |>
    inner_join(emi_species_map, by = "emi_item") |>
    summarise(
      manure_ch4_kt = sum(manure_ch4_kt, na.rm = TRUE),
      .by = c(year, area_code, species_group)
    )

  # Extract manure management N2O (kt)
  manure_n2o <- emi_raw |>
    filter(Element == "Manure management (Emissions N2O)") |>
    transmute(
      area_code = .map_fao_to_whep(Area),
      year = as.integer(Year),
      emi_item = Item,
      manure_n2o_kt = Value
    ) |>
    filter(!is.na(area_code)) |>
    inner_join(emi_species_map, by = "emi_item") |>
    summarise(
      manure_n2o_kt = sum(manure_n2o_kt, na.rm = TRUE),
      .by = c(year, area_code, species_group)
    )

  emissions <- enteric_ch4 |>
    full_join(manure_ch4, by = c("year", "area_code", "species_group")) |>
    full_join(manure_n2o, by = c("year", "area_code", "species_group")) |>
    mutate(
      enteric_ch4_kt = if_else(is.na(enteric_ch4_kt), 0, enteric_ch4_kt),
      manure_ch4_kt = if_else(is.na(manure_ch4_kt), 0, manure_ch4_kt),
      manure_n2o_kt = if_else(is.na(manure_n2o_kt), 0, manure_n2o_kt)
    )

  n_emi <- n_distinct(emissions$species_group)
  cli::cli_alert_success(
    "Emissions: {n_emi} groups, ",
    "{min(emissions$year)}-{max(emissions$year)}"
  )
}


# ==== 4. Merge stocks + emissions ======================================

cli::cli_h2("Merging stocks and emissions")

# Compute Nex-based manure N (Mg = metric tonnes)
livestock_country <- stocks_grouped |>
  mutate(
    manure_n_mg = heads * nex_kg_n_head / 1000
  )

if (!is.null(emissions)) {
  livestock_country <- livestock_country |>
    left_join(
      emissions,
      by = c("year", "area_code", "species_group")
    ) |>
    mutate(
      enteric_ch4_kt = if_else(
        is.na(enteric_ch4_kt), 0, enteric_ch4_kt
      ),
      manure_ch4_kt = if_else(
        is.na(manure_ch4_kt), 0, manure_ch4_kt
      ),
      manure_n2o_kt = if_else(
        is.na(manure_n2o_kt), 0, manure_n2o_kt
      )
    )
} else {
  livestock_country <- livestock_country |>
    mutate(
      enteric_ch4_kt = 0,
      manure_ch4_kt = 0,
      manure_n2o_kt = 0
    )
}

cli::cli_alert_success(
  "Combined: {nrow(livestock_country)} records, ",
  "{n_distinct(livestock_country$area_code)} countries"
)


# ==== 5. Historical predecessor redistribution =========================

cli::cli_h2("Historical predecessor redistribution")

# Same predecessors as crop spatialization
predecessors <- list(
  # USSR -> 15 successors
  list(
    code = 228L, end_year = 1991L,
    successors = c(
      1L, 52L, 57L, 63L, 73L, 108L, 113L,
      119L, 126L, 146L, 185L, 208L, 213L, 230L, 235L
    )
  ),
  # Yugoslav SFR -> 6 successors
  list(
    code = 248L, end_year = 1991L,
    successors = c(80L, 98L, 154L, 198L, 272L, 273L)
  ),
  # Czechoslovakia -> CZ + SK
  list(
    code = 51L, end_year = 1992L,
    successors = c(167L, 199L)
  ),
  # Belgium-Luxembourg -> BE + LU
  list(
    code = 15L, end_year = 1999L,
    successors = c(255L, 256L)
  ),
  # Serbia and Montenegro -> RS + ME
  list(
    code = 186L, end_year = 2005L,
    successors = c(272L, 273L)
  ),
  # Ethiopia PDR -> ET + ER
  list(
    code = 62L, end_year = 1992L,
    successors = c(238L, 178L)
  ),
  # Sudan (former) -> SD + SS
  list(
    code = 206L, end_year = 2011L,
    successors = c(276L, 277L)
  )
)

# Load country grid for pasture-based redistribution weights
country_grid_file <- file.path(output_dir, "country_grid.parquet")
if (!file.exists(country_grid_file)) {
  cli::cli_abort(
    "country_grid.parquet not found - run prepare_spatialize_inputs.R first"
  )
}
country_grid <- nanoparquet::read_parquet(country_grid_file)
cli::cli_alert_success("Country grid: {nrow(country_grid)} cells")

# Read LUH2 for pasture-based redistribution weights
luh2_dir <- file.path(l_files_dir, "LUH2", "LUH2 v2h")
states_path <- file.path(luh2_dir, "states.nc")
carea_rast <- terra::rast(
  file.path(luh2_dir, "staticData_quarterdeg.nc"),
  subds = "carea"
)
carea_ha <- carea_rast * 100 # km2 -> ha
agg_factor <- as.integer(target_res / 0.25)

# Compute pasture weights per successor per year
# Split into: (a) raster read (cached per year) and
#              (b) weight computation (per predecessor's successors)
.grazing_cache <- list()

.get_grazing_by_country <- function(yr) {
  yr_char <- as.character(yr)
  if (!is.null(.grazing_cache[[yr_char]])) {
    return(.grazing_cache[[yr_char]])
  }
  time_idx <- yr - 850L + 1L
  pastr_r <- .read_luh2_variable(states_path, "pastr", time_idx)
  range_r <- .read_luh2_variable(states_path, "range", time_idx)
  grazing_ha <- (pastr_r + range_r) * carea_ha
  grazing_ha <- terra::aggregate(
    grazing_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
  )
  tbl <- .raster_to_tibble(grazing_ha, "grazing_ha") |>
    filter(!is.na(grazing_ha), grazing_ha > 0) |>
    inner_join(country_grid, by = c("lon", "lat")) |>
    summarise(grazing_ha = sum(grazing_ha), .by = "area_code")
  .grazing_cache[[yr_char]] <<- tbl
  tbl
}

.get_pasture_weights <- function(yr, successor_codes) {
  tbl <- .get_grazing_by_country(yr) |>
    filter(area_code %in% successor_codes)
  total <- sum(tbl$grazing_ha)
  if (total > 0) {
    tbl$share <- tbl$grazing_ha / total
  } else {
    # equal split if no grazing data
    n <- length(successor_codes)
    tbl <- tibble::tibble(
      area_code = successor_codes,
      grazing_ha = 0,
      share = 1 / n
    )
  }
  tbl
}

# No need for additional weight_cache — raster reads are cached in
# .get_grazing_by_country() and weights are cheap to compute.
n_redistributed <- 0L
for (pred in predecessors) {
  pred_rows <- filter(livestock_country, area_code == pred$code)
  if (nrow(pred_rows) == 0) next

  cli::cli_alert_info(
    "  Predecessor {pred$code}: {nrow(pred_rows)} rows -> ",
    "{length(pred$successors)} successors"
  )
  pred_years <- unique(pred_rows$year)
  for (yr in pred_years) {
    weights <- .get_pasture_weights(yr, pred$successors)
    if (nrow(weights) == 0) next

    yr_data <- filter(pred_rows, year == yr)
    expanded <- yr_data |>
      select(-area_code) |>
      tidyr::crossing(
        select(weights, area_code, share)
      ) |>
      mutate(
        across(
          c(heads, manure_n_mg, enteric_ch4_kt,
            manure_ch4_kt, manure_n2o_kt),
          \(x) x * share
        )
      ) |>
      select(-share)

    livestock_country <- bind_rows(
      filter(livestock_country, !(area_code == pred$code & year == yr)),
      expanded
    )
    n_redistributed <- n_redistributed + nrow(expanded)
  }
}

# Aggregate any duplicate area_code × year × group from merging
livestock_country <- livestock_country |>
  summarise(
    heads = sum(heads, na.rm = TRUE),
    nex_kg_n_head = weighted.mean(nex_kg_n_head, heads, na.rm = TRUE),
    manure_n_mg = sum(manure_n_mg, na.rm = TRUE),
    enteric_ch4_kt = sum(enteric_ch4_kt, na.rm = TRUE),
    manure_ch4_kt = sum(manure_ch4_kt, na.rm = TRUE),
    manure_n2o_kt = sum(manure_n2o_kt, na.rm = TRUE),
    .by = c(year, area_code, species_group)
  )

cli::cli_alert_success(
  "Redistributed {n_redistributed} predecessor records"
)

# Verify no predecessor codes remain
pred_codes_remaining <- intersect(
  vapply(predecessors, \(x) x$code, integer(1)),
  unique(livestock_country$area_code)
)
if (length(pred_codes_remaining) > 0) {
  cli::cli_alert_warning(
    "Predecessor codes still present: ",
    "{paste(pred_codes_remaining, collapse = ', ')}"
  )
} else {
  cli::cli_alert_success("All predecessor codes redistributed")
}


# ==== 6. Pre-FAOSTAT backfill (1850-1960) ==============================

cli::cli_h2("Pre-FAOSTAT livestock backfill (1850-1960)")

fao_first <- 1961L
backfill_years <- year_range[year_range < fao_first]

if (length(backfill_years) > 0) {
  # Compute LUH2 pasture ratio per country for back-extrapolation
  cli::cli_alert_info(
    "Reading LUH2 pasture for {length(backfill_years) + 1} years"
  )

  # Read 1961 baseline pasture
  base_pasture <- .get_pasture_weights(fao_first, unique(
    livestock_country$area_code
  )) |>
    select(area_code, base_ha = grazing_ha)

  # Process backfill in chunks of 10 years for memory
  backfill_list <- list()
  chunk_years <- split(
    backfill_years,
    ceiling(seq_along(backfill_years) / 10)
  )

  base_stocks <- livestock_country |>
    filter(year == fao_first)

  for (chunk in chunk_years) {
    chunk_ratios <- purrr::map(chunk, \(yr) {
      pw <- .get_pasture_weights(yr, unique(base_pasture$area_code))
      pw |>
        inner_join(base_pasture, by = "area_code") |>
        mutate(
          ratio = if_else(base_ha > 0, grazing_ha / base_ha, 1),
          year = yr
        ) |>
        select(year, area_code, ratio)
    }) |>
      bind_rows()

    chunk_backfill <- chunk_ratios |>
      inner_join(
        base_stocks |>
          select(
            area_code, species_group, nex_kg_n_head,
            base_heads = heads,
            base_enteric = enteric_ch4_kt,
            base_manure_ch4 = manure_ch4_kt,
            base_manure_n2o = manure_n2o_kt
          ),
        by = "area_code",
        relationship = "many-to-many"
      ) |>
      mutate(
        heads = base_heads * ratio,
        manure_n_mg = heads * nex_kg_n_head / 1000,
        enteric_ch4_kt = base_enteric * ratio,
        manure_ch4_kt = base_manure_ch4 * ratio,
        manure_n2o_kt = base_manure_n2o * ratio
      ) |>
      filter(heads > 0) |>
      select(
        year, area_code, species_group, heads, nex_kg_n_head,
        manure_n_mg, enteric_ch4_kt, manure_ch4_kt, manure_n2o_kt
      )

    backfill_list <- c(backfill_list, list(chunk_backfill))
  }

  livestock_country <- bind_rows(
    livestock_country,
    bind_rows(backfill_list)
  ) |>
    arrange(year, area_code, species_group)

  cli::cli_alert_success(
    "Backfilled to {min(livestock_country$year)}: ",
    "{nrow(livestock_country)} total records"
  )
}


# ==== 7. Save livestock_country_data.parquet ===========================

cli::cli_h2("Saving livestock country data")

livestock_out <- livestock_country |>
  select(
    year, area_code, species_group, heads,
    enteric_ch4_kt, manure_ch4_kt, manure_n2o_kt, manure_n_mg
  )

nanoparquet::write_parquet(
  livestock_out,
  file.path(output_dir, "livestock_country_data.parquet")
)

sz <- round(
  file.size(
    file.path(output_dir, "livestock_country_data.parquet")
  ) / 1024 / 1024, 1
)
cli::cli_alert_success(
  "livestock_country_data.parquet: {nrow(livestock_out)} rows ({sz} MB)"
)


# ==== 8. Prepare gridded_pasture.parquet ===============================

cli::cli_h2("Preparing gridded pasture (LUH2)")

# Read pasture + rangeland for all needed years
# Process in annual batches (parallelize later if needed)

pasture_years <- sort(unique(livestock_country$year))
cli::cli_alert_info(
  "Processing {length(pasture_years)} years of LUH2 pasture data"
)

pasture_list <- list()
for (i in seq_along(pasture_years)) {
  yr <- pasture_years[i]

  if (yr %% 50 == 0 || yr == min(pasture_years)) {
    cli::cli_alert("  Year {yr} ({i}/{length(pasture_years)})")
  }

  time_idx <- yr - 850L + 1L

  pastr_r <- .read_luh2_variable(states_path, "pastr", time_idx)
  range_r <- .read_luh2_variable(states_path, "range", time_idx)

  pastr_ha <- pastr_r * carea_ha
  range_ha <- range_r * carea_ha

  pastr_ha <- terra::aggregate(
    pastr_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
  )
  range_ha <- terra::aggregate(
    range_ha, fact = agg_factor, fun = "sum", na.rm = TRUE
  )

  p_tbl <- .raster_to_tibble(pastr_ha, "pasture_ha")
  r_tbl <- .raster_to_tibble(range_ha, "rangeland_ha")

  yr_tbl <- left_join(p_tbl, r_tbl, by = c("lon", "lat")) |>
    mutate(
      pasture_ha = if_else(is.na(pasture_ha), 0, pasture_ha),
      rangeland_ha = if_else(is.na(rangeland_ha), 0, rangeland_ha)
    ) |>
    filter(pasture_ha > 0 | rangeland_ha > 0) |>
    mutate(year = yr)

  pasture_list[[i]] <- yr_tbl
}

gridded_pasture <- bind_rows(pasture_list)

nanoparquet::write_parquet(
  gridded_pasture,
  file.path(output_dir, "gridded_pasture.parquet")
)

sz <- round(
  file.size(
    file.path(output_dir, "gridded_pasture.parquet")
  ) / 1024 / 1024, 1
)
n_cells <- n_distinct(paste(gridded_pasture$lon, gridded_pasture$lat))
cli::cli_alert_success(
  "gridded_pasture.parquet: {nrow(gridded_pasture)} rows, ",
  "{n_cells} cells, {length(pasture_years)} years ({sz} MB)"
)


# ==== 9. Prepare manure_pattern.parquet (West et al. 2014) =============

cli::cli_h2("Preparing manure intensity pattern (West et al. 2014)")

west_dir <- file.path(l_files_dir, "Manure_Westetal2014", "N")

if (dir.exists(west_dir)) {
  nc_files <- list.files(west_dir, pattern = "\\.nc$", full.names = TRUE)
  cli::cli_alert_info("Found {length(nc_files)} manure N rasters")

  # Read all crop-specific manure N per ha and sum to total intensity
  # Each file is kg N / ha applied from manure for one crop
  manure_total <- NULL

  for (nc_path in nc_files) {
    r <- terra::rast(nc_path)
    # Aggregate from ~5 arcmin to 0.5 deg (factor ~ 6)
    agg_f <- round(target_res / terra::res(r)[1])
    if (agg_f > 1) {
      r <- terra::aggregate(r, fact = agg_f, fun = "mean", na.rm = TRUE)
    }

    if (is.null(manure_total)) {
      manure_total <- r
    } else {
      # Ensure same extent/resolution before adding
      if (terra::compareGeom(manure_total, r, stopOnError = FALSE)) {
        manure_total <- manure_total + r
      } else {
        # Resample to match
        r_aligned <- terra::resample(r, manure_total, method = "bilinear")
        manure_total <- manure_total + r_aligned
      }
    }
  }

  # Convert to tibble
  manure_tbl <- .raster_to_tibble(manure_total, "manure_intensity") |>
    filter(
      !is.na(manure_intensity),
      manure_intensity > 0
    )

  # Normalize to 0-1 scale for use as multiplicative weight
  max_intensity <- max(manure_tbl$manure_intensity, na.rm = TRUE)
  if (max_intensity > 0) {
    manure_tbl <- manure_tbl |>
      mutate(manure_intensity = manure_intensity / max_intensity)
  }

  nanoparquet::write_parquet(
    manure_tbl,
    file.path(output_dir, "manure_pattern.parquet")
  )

  sz <- round(
    file.size(
      file.path(output_dir, "manure_pattern.parquet")
    ) / 1024 / 1024, 1
  )
  cli::cli_alert_success(
    "manure_pattern.parquet: {nrow(manure_tbl)} cells ({sz} MB)"
  )
} else {
  cli::cli_alert_warning(
    "West et al. 2014 not found at {west_dir} - skipping manure pattern"
  )
}


# ==== 10. Ensure gridded_cropland is available =========================

# The crop spatialization pipeline already produces
# gridded_cropland.parquet. Check it exists; if not, warn.

cropland_path <- file.path(output_dir, "gridded_cropland.parquet")
if (!file.exists(cropland_path)) {
  cli::cli_alert_warning(
    "gridded_cropland.parquet not found - run prepare_spatialize_inputs.R"
  )
  cli::cli_alert_info(
    "Livestock spatialization will be limited without cropland data"
  )
} else {
  sz <- round(file.size(cropland_path) / 1024 / 1024, 1)
  cli::cli_alert_success(
    "gridded_cropland.parquet available ({sz} MB)"
  )
}


# ==== Summary ===========================================================

cli::cli_h1("Livestock input preparation complete")

cat("\n")
cli::cli_alert_success("Outputs in {output_dir}:")
cli::cli_ul(c(
  "livestock_country_data.parquet",
  "gridded_pasture.parquet",
  "manure_pattern.parquet (if West et al. available)"
))
cat("\n")
cli::cli_alert_info(
  "Next: run inst/scripts/run_livestock_spatialize.R"
)
