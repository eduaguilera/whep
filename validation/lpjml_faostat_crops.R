# Validates a finished LPJmL run's crop output against FAOSTAT, per crop
# functional type (CFT) and country.
#
# READ THIS FIRST -- what this script can and cannot establish.
#
# LPJmL is not expected to reproduce FAOSTAT production in absolute terms
# unless it has been calibrated to. The model represents management intensity
# through the maximum leaf area index, the harvest index and a scaling
# parameter, and agreement with FAO yield statistics is obtained by
# calibrating the maximum LAI per country (Fader et al. 2010,
# doi:10.1016/j.jhydrol.2010.04.011). An uncalibrated run therefore sits
# above FAO yields, and that offset is a property of the calibration state,
# NOT evidence of a defect. So the absolute checks below characterise the
# run; only the constant-free ones can validate a change to the model.
#
# The reports, ordered by how few assumptions each one needs:
#
# 1. HARVESTED AREA -- assumption-free, and the one hard absolute check.
#    LPJmL's `cftfrac` x cell area is directly comparable to FAOSTAT "Area
#    harvested" in ha: same quantity, same unit, no conversion. This is the
#    sharpest available test of the land-use forcing and of whether crops
#    establish at all. Expect LPJmL slightly BELOW FAO: `cftfrac` is a
#    physical land fraction, whereas FAOSTAT counts a multi-cropped field
#    once per harvest.
#
# 2. YIELD LEVEL -- a sanity check on this script, not on the model. An
#    error in cell area or in the gC-to-tonne factor lands here as a yield
#    that is orders of magnitude out, which is obvious; a calibration offset
#    is a factor of ~2, which is expected (see above).
#
# 3. IMPLIED CARBON CONTENT -- one diagnostic, no assumed constant. LPJmL
#    reports harvested carbon (gC); FAOSTAT reports fresh matter (t). The
#    conversion needs per-crop carbon and water contents that this repo does
#    not carry, so rather than invent them we back out the IMPLIED gC per g
#    fresh matter and ask whether it is physically possible at all. Dry
#    matter is roughly 40-50% carbon, and field crops run from ~10% dry
#    matter (sugarcane, roots) to ~90% (cereals), which bounds any real crop
#    to roughly 0.04-0.45 gC/gFM. Above that band the model is producing
#    more carbon than the FAO tonnage can physically contain, which
#    quantifies the calibration offset per crop.
#
# 4. SPATIAL PATTERN -- constant-free, and the most informative check.
#    The LPJmL-vs-FAOSTAT Spearman correlation across countries is invariant
#    to any per-CFT multiplicative constant, so it tests whether the model
#    puts production in the right PLACES independently of its level, which
#    is exactly what a calibration offset leaves untouched.
#
# 5. RUN vs BASELINE RUN -- constant-free, and the only check that can
#    validate a model change. Every column is a ratio between two runs, so
#    the unknown conversion and the calibration state both cancel exactly.
#
# Deliberately self-contained: a validation script that imported the package
# internals it is checking would not be an independent check.
#
# Usage:
#   Rscript validation/lpjml_faostat_crops.R <run_dir> [baseline_dir] [years]
#
# Example (6.1.1 against the 5.9.7 baseline, mean of 2000/2005/2010):
#   Rscript validation/lpjml_faostat_crops.R \
#     /path/to/611/output/scenario_1 /path/to/597/output/scenario_1 \
#     2000,2005,2010

suppressMessages({
  library(dplyr)
  library(tibble)
})

# The 12 CFTs LPJmL simulates explicitly. "others" is excluded throughout: it
# is a catch-all stand (fruit, nuts, vegetables, fibres, stimulants) whose
# FAOSTAT counterpart is a heterogeneous basket, so neither its area nor its
# carbon is interpretable as a single crop.
CFTS <- c(
  "temperate_cereals",
  "rice",
  "maize",
  "tropical_cereals",
  "pulses",
  "temperate_roots",
  "tropical_roots",
  "oil_crops_sunflower",
  "oil_crops_soybean",
  "oil_crops_groundnut",
  "oil_crops_rapeseed",
  "sugarcane"
)

# FAOSTAT element codes.
FAO_PRODUCTION <- 5510L
FAO_AREA_HARVESTED <- 5312L

# Carbon per gram of fresh matter that any real field crop must fall within
# (see note 2 in the header). Used only to judge the implied factor, never to
# convert anything.
PLAUSIBLE_GC_PER_G_FM <- c(low = 0.04, high = 0.45)

# LPJmL's own carbon-to-dry-matter constant, used by the model itself
# ("carbon2DM 1/0.45", src/soil/littersom.c). Taken from the source rather
# than assumed, and used only to express carbon as dry matter -- never to
# convert to the fresh-matter basis FAOSTAT reports on.
CARBON_PER_DRY_MATTER <- 0.45

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L) {
    cat("usage: lpjml_faostat_crops.R <run_dir> [baseline_dir] [years]\n")
    return(invisible(NULL))
  }
  run_dir <- args[[1L]]
  baseline_dir <- if (length(args) >= 2L && nzchar(args[[2L]])) {
    args[[2L]]
  } else {
    NULL
  }
  years <- if (length(args) >= 3L) {
    as.integer(strsplit(args[[3L]], ",")[[1L]])
  } else {
    c(2000L, 2005L, 2010L)
  }

  message("years: ", paste(years, collapse = ", "))
  fao <- read_faostat_cfts(years)
  run <- read_lpjml_cfts(run_dir, years)

  report_area(run, fao)
  report_yield(run, fao)
  report_implied_carbon(run, fao)
  report_spatial_pattern(run, fao)

  if (!is.null(baseline_dir)) {
    base <- read_lpjml_cfts(baseline_dir, years)
    report_version_delta(run, base)
  }
  invisible(NULL)
}

# ---- LPJmL side -------------------------------------------------------------

# Per-CFT harvested carbon and harvested area per country, summed over the
# rainfed and irrigated stands of each CFT.
#
# The per-PFT bands are PER STAND (gC per m2 of that CFT's own stand), not per
# grid cell, so cell-level production is harvestc * cftfrac * cell_area. This
# is the same reconstruction the package's own readers document; getting it
# wrong overstates production several-fold.
read_lpjml_cfts <- function(run_dir, years) {
  cache <- lpjml_cache_path(run_dir, years)
  if (file.exists(cache)) {
    message("reading LPJmL (cached): ", basename(cache))
    return(readRDS(cache))
  }
  message("reading LPJmL: ", run_dir)
  cell_polity <- read_cell_polity()
  per_year <- lapply(years, function(y) {
    read_lpjml_year(run_dir, y, cell_polity)
  })
  out <- bind_rows(per_year) |>
    summarise(
      harvest_gc = mean(harvest_gc),
      area_ha = mean(area_ha),
      .by = c(area_code, cft)
    )
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, cache)
  out
}

# Reading 24 bands out of two 3 GB cubes takes minutes, and the reports get
# iterated on far more often than the cubes change. The key keeps the whole
# run path (sanitised) rather than a prefix of it, so two runs cannot collide.
lpjml_cache_path <- function(run_dir, years) {
  key <- paste0(
    gsub("[^A-Za-z0-9]+", "_", normalizePath(run_dir, mustWork = FALSE)),
    "__",
    paste(years, collapse = "-")
  )
  file.path("validation/cache/lpjml_cfts", paste0(key, ".rds"))
}

read_lpjml_year <- function(run_dir, year, cell_polity) {
  harvest <- read_lpjml_slab(run_dir, "pft_harvestc.nc", "harvestc", year)
  frac <- read_lpjml_slab(run_dir, "cftfrac.nc", "CFTfrac", year)

  cells <- inner_join(
    harvest |> rename(harvest_gc_m2 = value),
    frac |> rename(cft_frac = value),
    by = c("lon", "lat", "cft")
  ) |>
    mutate(cell_area_ha = cell_area_ha_lat(lat)) |>
    mutate(
      area_ha = cft_frac * cell_area_ha,
      # gC/m2 of stand x ha of stand x 1e4 m2/ha -> gC
      harvest_gc = harvest_gc_m2 * area_ha * 1e4
    ) |>
    filter(area_ha > 0)

  # Border cells belong to more than one country; split by the same fraction
  # the package uses so national totals stay additive.
  cells |>
    inner_join(
      cell_polity,
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    summarise(
      harvest_gc = sum(harvest_gc * polity_frac),
      area_ha = sum(area_ha * polity_frac),
      .by = c(area_code, cft)
    ) |>
    mutate(year = year)
}

# One year's [lon, lat, band] slab, reduced to the 12 CFTs by summing each
# CFT's rainfed and irrigated stands. Bands are matched by NAME: the band
# order is not guaranteed to agree between files or between model versions.
read_lpjml_slab <- function(run_dir, file, var, year) {
  path <- file.path(run_dir, file)
  if (!file.exists(path)) {
    stop("missing LPJmL output: ", path, call. = FALSE)
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))

  names_pft <- as.character(ncdf4::ncvar_get(nc, "NamePFT"))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  time_index <- lpjml_time_index(nc, year)

  slab <- ncdf4::ncvar_get(
    nc,
    var,
    start = c(1L, 1L, 1L, time_index),
    count = c(length(lon), length(lat), length(names_pft), 1L)
  )
  # LPJmL preallocates every output file at its full size before the run
  # reaches those years, so a year that has not been simulated yet is present
  # in the file, readable, and entirely fill value. Without this check the
  # reports would come back empty or near-empty rather than saying why, which
  # reads as "the model produced nothing" instead of "the run is not finished".
  if (!any(is.finite(slab))) {
    stop(
      "year ",
      year,
      " is all fill value in ",
      file,
      " -- the run has not simulated it yet",
      call. = FALSE
    )
  }

  # Expanded outside tibble(): tibble() evaluates its arguments sequentially
  # with data masking, so `length(lon)` inside the call would resolve to the
  # already-expanded lon COLUMN rather than to the 720-value axis.
  grid_lon <- rep(lon, times = length(lat))
  grid_lat <- rep(lat, each = length(lon))

  wanted <- cft_band_index(names_pft)
  parts <- lapply(seq_len(nrow(wanted)), function(i) {
    tibble(
      lon = grid_lon,
      lat = grid_lat,
      cft = wanted$cft[[i]],
      value = as.vector(slab[,, wanted$band[[i]]])
    )
  })
  bind_rows(parts) |>
    filter(is.finite(value)) |>
    summarise(value = sum(value), .by = c(lon, lat, cft))
}

# CFT name (underscored, as in whep::cft_mapping) -> band positions of its
# rainfed and irrigated stands (band names use spaces).
cft_band_index <- function(names_pft) {
  specs <- lapply(CFTS, function(cft) {
    stem <- gsub("_", " ", cft)
    bands <- match(paste(c("rainfed", "irrigated"), stem), names_pft)
    if (anyNA(bands)) {
      stop("CFT bands not found for '", cft, "'", call. = FALSE)
    }
    tibble(cft = cft, band = bands)
  })
  bind_rows(specs)
}

lpjml_time_index <- function(nc, year, first_year = 1901L) {
  index <- year - first_year + 1L
  if (index < 1L || index > nc$dim[["time"]]$len) {
    stop("year ", year, " outside the run's time axis", call. = FALSE)
  }
  index
}

# Area of a 0.5-degree cell in ha. Cells narrow towards the poles, so a
# constant area would bias every high-latitude crop.
cell_area_ha_lat <- function(lat, resolution = 0.5, earth_radius_m = 6371000) {
  step <- resolution * pi / 180
  band <- sin(lat * pi / 180 + step / 2) - sin(lat * pi / 180 - step / 2)
  earth_radius_m^2 * step * band / 1e4
}

read_cell_polity <- function() {
  path <- Sys.getenv("WHEP_POLITY_FRACTION_PATH")
  if (!nzchar(path)) {
    path <- file.path(
      "/home/usuario/WHEP/LPJmL_inputs/whep/inputs",
      "cell_polity_fraction.parquet"
    )
  }
  if (!file.exists(path)) {
    stop("no cell-polity parquet; set WHEP_POLITY_FRACTION_PATH", call. = FALSE)
  }
  nanoparquet::read_parquet(path) |>
    as_tibble() |>
    select(lon, lat, area_code, polity_frac)
}

# ---- FAOSTAT side -----------------------------------------------------------

# FAOSTAT production and harvested area aggregated to the 12 CFTs, per
# country. LPJmL's "temperate cereals" is wheat + barley + rye + oats + ...,
# so the FAOSTAT items have to be summed the same way; whep::cft_mapping is
# that crosswalk.
read_faostat_cfts <- function(years) {
  message("reading FAOSTAT")
  raw <- nanoparquet::read_parquet(faostat_path()) |> as_tibble()

  mapping <- read_cft_mapping() |>
    select(item_code = item_prod_code, cft = cft_lpjml) |>
    filter(cft %in% CFTS)

  raw |>
    select(
      area_code = `Area Code`,
      item_code = `Item Code`,
      element_code = `Element Code`,
      year = Year,
      value = Value
    ) |>
    filter(
      year %in% years,
      element_code %in% c(FAO_PRODUCTION, FAO_AREA_HARVESTED),
      !is.na(value)
    ) |>
    drop_faostat_aggregates() |>
    inner_join(mapping, by = "item_code") |>
    summarise(
      value = sum(value),
      .by = c(area_code, cft, element_code, year)
    ) |>
    summarise(value = mean(value), .by = c(area_code, cft, element_code)) |>
    tidyr::pivot_wider(
      names_from = element_code,
      values_from = value,
      names_prefix = "e"
    ) |>
    rename(
      fao_production_t = paste0("e", FAO_PRODUCTION),
      fao_area_ha = paste0("e", FAO_AREA_HARVESTED)
    )
}

# FAOSTAT ships regional aggregates and, for China, an aggregate (351) that
# sits alongside its own components (mainland 41, Taiwan 214, Hong Kong 96,
# Macao 128). Summing all of them double-counts, so the aggregates go.
drop_faostat_aggregates <- function(x) {
  filter(x, area_code < 5000L, area_code != 351L)
}

# Read the crosswalk from its CSV source rather than from the installed
# package: this script has to run without whep on the library path, and an
# independent check should not import the package it is checking.
read_cft_mapping <- function() {
  candidates <- c(
    Sys.getenv("WHEP_CFT_MAPPING_PATH"),
    "inst/extdata/cft_mapping.csv",
    "/home/usuario/WHEP/inst/extdata/cft_mapping.csv"
  )
  path <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(path) == 0L) {
    stop("cft_mapping.csv not found", call. = FALSE)
  }
  utils::read.csv(path[[1L]], stringsAsFactors = FALSE) |> as_tibble()
}

faostat_path <- function() {
  path <- Sys.getenv("WHEP_FAOSTAT_PRODUCTION_PATH")
  if (nzchar(path) && file.exists(path)) {
    return(path)
  }
  cached <- Sys.glob(file.path(
    "/home/usuario/.cache/pins/url/*",
    "faostat-production.parquet"
  ))
  if (length(cached) == 0L) {
    stop("no cached faostat-production.parquet", call. = FALSE)
  }
  cached[[1L]]
}

# ---- Reports ----------------------------------------------------------------

# Check 1: harvested area, same unit on both sides, so this is a like-for-like
# absolute comparison.
report_area <- function(run, fao) {
  header("1. HARVESTED AREA vs FAOSTAT (no unit conversion involved)")
  joined <- inner_join(run, fao, by = c("area_code", "cft"))
  global <- joined |>
    summarise(
      lpjml_mha = sum(area_ha) / 1e6,
      fao_mha = sum(fao_area_ha) / 1e6,
      .by = cft
    ) |>
    mutate(ratio = lpjml_mha / fao_mha) |>
    arrange(desc(fao_mha))

  print(as.data.frame(global), digits = 3, row.names = FALSE)
  total <- summarise(
    global,
    lpjml = sum(lpjml_mha),
    fao = sum(fao_mha)
  )
  cat(sprintf(
    "\n  all 12 CFTs: LPJmL %.1f Mha vs FAOSTAT %.1f Mha (ratio %.2f)\n",
    total$lpjml,
    total$fao,
    total$lpjml / total$fao
  ))
}

# Absolute yields, mostly as a sanity check on the reconstruction itself: an
# error in cell area or in the gC/m2-to-tonne factor shows up here as a yield
# that is orders of magnitude off, which is easy to spot and hard to mistake
# for a model bias. Carbon is converted to dry matter with LPJmL's own
# constant (carbon2DM = 1/0.45, littersom.c), so the LPJmL column is t DM/ha.
# FAOSTAT is fresh matter, so the two columns are NOT expected to be equal --
# cereals sit near 13% moisture, roots and sugarcane near 75%.
report_yield <- function(run, fao) {
  header("2. YIELD LEVEL (LPJmL t DM/ha vs FAOSTAT t FM/ha -- sanity check)")
  joined <- inner_join(run, fao, by = c("area_code", "cft"))
  yields <- joined |>
    summarise(
      harvest_gc = sum(harvest_gc),
      area_ha = sum(area_ha),
      fao_t = sum(fao_production_t),
      fao_area_ha = sum(fao_area_ha),
      .by = cft
    ) |>
    mutate(
      # gC -> g DM -> t DM, over ha of that CFT's stands
      lpjml_t_dm_ha = (harvest_gc / CARBON_PER_DRY_MATTER) / 1e6 / area_ha,
      fao_t_fm_ha = fao_t / fao_area_ha
    ) |>
    arrange(desc(lpjml_t_dm_ha))

  print(
    as.data.frame(select(yields, cft, lpjml_t_dm_ha, fao_t_fm_ha)),
    digits = 3,
    row.names = FALSE
  )
}

# Check 3: turn the unknown carbon-to-fresh-matter conversion into a
# diagnostic instead of an assumption.
report_implied_carbon <- function(run, fao) {
  header("3. IMPLIED gC per g FRESH MATTER (physically bounded, not assumed)")
  joined <- inner_join(run, fao, by = c("area_code", "cft"))
  implied <- joined |>
    summarise(
      harvest_gc = sum(harvest_gc),
      fao_t = sum(fao_production_t),
      .by = cft
    ) |>
    # gC / (t * 1e6 g/t) -> gC per g fresh matter
    mutate(
      gc_per_g_fm = harvest_gc / (fao_t * 1e6),
      verdict = case_when(
        gc_per_g_fm < PLAUSIBLE_GC_PER_G_FM[["low"]] ~ "TOO LOW",
        gc_per_g_fm > PLAUSIBLE_GC_PER_G_FM[["high"]] ~ "TOO HIGH",
        .default = "plausible"
      )
    ) |>
    arrange(gc_per_g_fm)

  print(
    as.data.frame(select(implied, cft, gc_per_g_fm, verdict)),
    digits = 3,
    row.names = FALSE
  )
  cat(sprintf(
    "\n  plausible band: %.2f-%.2f gC/gFM; %d of %d CFTs inside it\n",
    PLAUSIBLE_GC_PER_G_FM[["low"]],
    PLAUSIBLE_GC_PER_G_FM[["high"]],
    sum(implied$verdict == "plausible"),
    nrow(implied)
  ))
}

# Check 3: shares and rank correlation are invariant to any per-CFT constant,
# so they test placement independently of level.
report_spatial_pattern <- function(run, fao) {
  header("4. SPATIAL PATTERN across countries (constant-free)")
  joined <- inner_join(run, fao, by = c("area_code", "cft")) |>
    filter(fao_production_t > 0, harvest_gc > 0)

  stats <- joined |>
    summarise(
      n_countries = dplyr::n(),
      rho_production = suppressWarnings(stats::cor(
        harvest_gc,
        fao_production_t,
        method = "spearman"
      )),
      rho_area = suppressWarnings(stats::cor(
        area_ha,
        fao_area_ha,
        method = "spearman"
      )),
      .by = cft
    ) |>
    arrange(rho_production)

  print(as.data.frame(stats), digits = 2, row.names = FALSE)
  cat(sprintf(
    "\n  median Spearman rho: production %.2f, area %.2f\n",
    stats::median(stats$rho_production, na.rm = TRUE),
    stats::median(stats$rho_area, na.rm = TRUE)
  ))
}

# The migration effect. Every column here is a ratio between two runs, so the
# carbon-to-fresh-matter conversion cancels and nothing has to be assumed.
report_version_delta <- function(run, base) {
  header("5. THIS RUN vs BASELINE RUN (ratios; conversion cancels)")
  joined <- inner_join(
    run |> rename(new_gc = harvest_gc, new_ha = area_ha),
    base |> rename(old_gc = harvest_gc, old_ha = area_ha),
    by = c("area_code", "cft")
  )
  delta <- joined |>
    summarise(
      new_gc = sum(new_gc),
      old_gc = sum(old_gc),
      new_ha = sum(new_ha),
      old_ha = sum(old_ha),
      .by = cft
    ) |>
    mutate(
      harvest_ratio = new_gc / old_gc,
      area_ratio = new_ha / old_ha
    ) |>
    arrange(harvest_ratio)

  print(
    as.data.frame(select(delta, cft, harvest_ratio, area_ratio)),
    digits = 3,
    row.names = FALSE
  )
  totals <- summarise(
    delta,
    gc = sum(new_gc) / sum(old_gc),
    ha = sum(new_ha) / sum(old_ha)
  )
  cat(sprintf(
    "\n  all 12 CFTs: harvest %.3f x, area %.3f x\n",
    totals$gc,
    totals$ha
  ))
}

header <- function(text) {
  cat("\n", strrep("=", 74), "\n", text, "\n", strrep("=", 74), "\n", sep = "")
}

if (sys.nframe() == 0L) {
  main()
}
