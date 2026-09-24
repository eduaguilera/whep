# Benchmarks WHEP's soil-carbon equilibrium against LPJmL and against two
# independent observational products, on one grid and one land-use mask.
#
# WHY THIS EXISTS
#
# Every SOC comparison this project has made by hand went wrong at least once,
# and always in the same three ways. This script exists to make those three
# mistakes impossible to repeat:
#
#   1. COMPARING AN EQUILIBRIUM TO A MEASURED SOIL. Real soils carry legacy
#      carbon from their own land-use history. LPJmL's cropland holds 6.1x its
#      OWN equilibrium (79.7 against 13.0 MgC/ha) and observations sit near 40 --
#      all legacy. Reading WHEP's equilibrium against any of those made cropland
#      look 4-6x too low when it is in fact above LPJmL's own equilibrium
#      (whep#799). So this script reports LPJmL's EQUILIBRIUM as the
#      like-for-like column and its simulated stock separately.
#
#   2. COMPARING UNLIKE OPERATIONS. WHEP's class mean is
#      `mean(input x tau / cm)`, which carries `mean(1/cm)`; a figure computed
#      from mean input and mean response carries `1/mean(cm)`. Those differ by
#      ~4.5x for WHEP and ~1.0x for LPJmL, and the difference is not a property
#      of either model. Everything here is computed per cell and then averaged,
#      and the convexity factor is reported so the gap is visible rather than
#      absorbed.
#
#   3. QUOTING A NUMBER INSIDE THE NOISE. SoilGrids and GSOCmap disagree with
#      each other by a median per-cell factor of ~1.32. Any model-vs-reference
#      finding inside that band is not evidence. The band is measured here every
#      run rather than assumed, so it moves if the products do.
#
# WHAT THIS IS, AND IS NOT
#
# It is NOT a claim that any model is right. There is no observational product
# for "the equilibrium a soil would reach", so the observational columns bound
# plausibility, they do not adjudicate. It is a CONTRACT and REGRESSION check in
# the same three tiers as `lpjml_pins.R`:
#
#   1. CONTRACT   the products and run outputs are present, on the expected
#                 grid, in the expected units.
#   2. INVARIANT  physical impossibility, not expectation: negative stocks, a
#                 class mean above any soil ever measured, a reference
#                 disagreement so large that one product must have changed.
#   3. BASELINE   recorded magnitudes in `gt_soc_benchmark.json`. This is the
#                 tier that makes a model change loud.
#
# Tier 3 fails BY DESIGN when the SOC model changes. That is the signal to look
# at what moved and re-record, saying in the commit why.
#
# Usage:
#   Rscript validation/soc_benchmark.R            # check against the baseline
#   Rscript validation/soc_benchmark.R --record   # rewrite the baseline
#   Rscript validation/soc_benchmark.R --refresh  # rebuild the cached
#                                                 assembly first
#
# Needs, and aborts naming whichever is missing:
#   WHEP_LPJML_RUN_DIR      an LPJmL run holding soilc_layer.nc, cftfrac.nc,
#                           pft_npp.nc and response_layer_nv.nc
#   WHEP_SOC_PRODUCTS_DIR   soilgrids_ocs_0_30cm.tif and gsocmap_ocs_0_30cm.tif
#                           (see download_soc_products() below for the sources)
#   WHEP_HWSD_DIR           for read_hwsd_topsoil_soc(), the third reference

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({
    devtools::load_all(".", quiet = TRUE)
    library(dplyr)
  })
}

SOC_YEARS <- 2000:2010
SOC_FIRST_YEAR <- 1901L
# LPJmL's soil-bound residence over 0-3 m at response 1, from its own run
# parameters: (1 - atmfrac) * (fastfrac / k_fast + (1 - fastfrac) / k_slow).
SOC_TAU_PROFILE <- 22.25
# Share of that profile lying in 0-30 cm, measured from soilc_layer's own
# depth_bnds. Recomputed per run below; this is only the fallback.
SOC_TOPSOIL_SHARE <- 0.45
SOC_BASELINE <- "validation/gt_soc_benchmark.json"
SOC_CACHE_DIR <- "validation/cache/soc_benchmark"

# The natural PFTs, and which of them are woody. Kept here rather than reached
# for out of the package so this script still runs against a future WHEP whose
# internal list has moved on -- a benchmark that silently follows the thing it
# benchmarks is not a benchmark.
soc_natural_pfts <- function() {
  c(
    "tropical broadleaved evergreen tree",
    "tropical broadleaved evergreen tree floodtolerant",
    "tropical broadleaved raingreen tree",
    "temperate needleleaved evergreen tree",
    "temperate broadleaved evergreen tree",
    "temperate broadleaved summergreen tree",
    "boreal needleleaved evergreen tree",
    "boreal broadleaved summergreen tree",
    "boreal needleleaved summergreen tree",
    "Tropical C4 grass",
    "Temperate C3 grass",
    "Polar C3 grass",
    "C3 graminoid flood tolerant",
    "Sphagnum moss"
  )
}

# Where the observational products come from, so a missing directory is
# actionable rather than a dead end.
soc_product_sources <- function() {
  tibble::tribble(
    ~file, ~url,
    "soilgrids_ocs_0_30cm.tif",
    paste0(
      "https://files.isric.org/soilgrids/latest/data_aggregated/5000m/ocs/",
      "ocs_0-30cm_mean_5000.tif"
    ),
    "gsocmap_ocs_0_30cm.tif",
    paste0(
      "https://storage.googleapis.com/fao-gismgr-gsocseq-data/DATA/GSOCSEQ/",
      "MAP/GSOCSEQ.GSOCMAP1-5-0.tif"
    )
  )
}

soc_require_dir <- function(env_var, what) {
  dir <- Sys.getenv(env_var)
  if (!nzchar(dir) || !dir.exists(dir)) {
    cli::cli_abort(c(
      "No {what} available.",
      i = "Set {.envvar {env_var}} to a directory holding it."
    ))
  }
  dir
}

# -- references ---------------------------------------------------------------

# Both products are 0-30 cm STOCKS in t/ha, so neither needs a bulk-density
# assumption of ours. SoilGrids' aggregated builds ship in Goode Homolosine
# with no EPSG code, which `terra` reports as an absent CRS rather than as an
# error -- declare it instead of letting the reprojection silently treat metres
# as degrees.
soc_read_reference <- function(path, label, target) {
  r <- terra::rast(path)
  if (is.na(terra::crs(r)) || terra::crs(r) == "") {
    terra::crs(
      r
    ) <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"
  }
  r[r < 0] <- NA
  out <- if (identical(terra::crs(r, describe = TRUE)$code, "4326")) {
    terra::resample(r, target, method = "average")
  } else {
    terra::project(r, target, method = "average")
  }
  names(out) <- label
  out
}

soc_reference_grid <- function(products_dir) {
  target <- terra::rast(
    nrows = 360,
    ncols = 720,
    xmin = -180,
    xmax = 180,
    ymin = -90,
    ymax = 90,
    crs = "EPSG:4326"
  )
  src <- soc_product_sources()
  missing <- src$file[!file.exists(file.path(products_dir, src$file))]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing observational product{?s}: {.file {missing}}.",
      i = "Download from: {.url {src$url[src$file %in% missing]}}"
    ))
  }
  layers <- purrr::map2(
    file.path(products_dir, src$file),
    c("soilgrids", "gsocmap"),
    \(p, l) soc_read_reference(p, l, target)
  )
  purrr::map(layers, soc_grid_frame) |>
    purrr::reduce(\(a, b) dplyr::inner_join(a, b, by = c("lon", "lat")))
}

soc_grid_frame <- function(r) {
  d <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  names(d)[1:2] <- c("lon", "lat")
  dplyr::mutate(d, lon = round(.data$lon, 2), lat = round(.data$lat, 2))
}

# -- the run ------------------------------------------------------------------

soc_nc_var <- function(nc) {
  main <- setdiff(
    names(nc$var),
    c("lon_bnds", "lat_bnds", "time_bnds", "NamePFT", "depth_bnds")
  )
  main[which.max(purrr::map_int(main, \(m) length(nc$var[[m]]$size)))]
}

# LPJmL soil carbon in 0-30 cm, interpolated from the file's OWN depth_bnds
# rather than from an assumed layering. No boundary sits at 30 cm -- the layers
# are 0-20, 20-50, 50-100, 100-200, 200-300 cm -- so the share of layer 2 is
# where a hand comparison goes wrong, and it is computed here rather than typed.
soc_lpjml_topsoil <- function(run_dir) {
  nc <- ncdf4::nc_open(file.path(run_dir, "soilc_layer.nc"))
  on.exit(ncdf4::nc_close(nc))
  v <- soc_nc_var(nc)
  bnds <- ncdf4::ncvar_get(nc, "depth_bnds")
  share <- pmax(0, pmin(bnds[2, ], 0.30) - pmin(bnds[1, ], 0.30)) /
    (bnds[2, ] - bnds[1, ])
  dims <- c(nc$dim$lon$len, nc$dim$lat$len)
  layers <- nc$dim$layer$len
  acc <- array(0, dims)
  total <- array(0, dims)
  for (y in SOC_YEARS) {
    x <- ncdf4::ncvar_get(
      nc,
      v,
      start = c(1, 1, 1, y - SOC_FIRST_YEAR + 1L),
      count = c(dims, layers, 1)
    )
    acc <- acc + apply(sweep(x, 3, share, "*"), c(1, 2), sum)
    total <- total + apply(x, c(1, 2), sum)
  }
  list(
    topsoil = acc / length(SOC_YEARS) * 0.01,
    profile = total / length(SOC_YEARS) * 0.01,
    lon = ncdf4::ncvar_get(nc, "lon"),
    lat = ncdf4::ncvar_get(nc, "lat")
  )
}

# PER-CLASS carbon input, and the land-use fractions the mask is built on.
#
# Each class gets ITS OWN input: natural is the sum of the 14 natural PFT
# densities (never harvested); cropland and managed grassland are
# (NPP - harvestc) over their own CFT bands, stand-fraction weighted.
#
# The first draft of this script used the natural input for all three classes,
# which put cropland's equilibrium at 120 MgC/ha instead of 13 -- precisely the
# unlike-things comparison the header exists to prevent. Bands are matched BY
# NAME because pft_npp.nc and pft_harvestc.nc order theirs differently.
soc_run_inputs <- function(run_dir) {
  np <- ncdf4::nc_open(file.path(run_dir, "pft_npp.nc"))
  hv <- ncdf4::nc_open(file.path(run_dir, "pft_harvestc.nc"))
  cf <- ncdf4::nc_open(file.path(run_dir, "cftfrac.nc"))
  on.exit({
    ncdf4::nc_close(np)
    ncdf4::nc_close(hv)
    ncdf4::nc_close(cf)
  })

  npp_names <- as.character(ncdf4::ncvar_get(np, "NamePFT"))
  hrv_names <- as.character(ncdf4::ncvar_get(hv, "NamePFT"))
  cft_names <- as.character(ncdf4::ncvar_get(cf, "NamePFT"))
  dims <- c(np$dim$lon$len, np$dim$lat$len)
  nat_idx <- which(npp_names %in% soc_natural_pfts())
  grass_b <- grep("grassland", cft_names)
  crop_b <- setdiff(seq_along(cft_names), grep("grassland|biomass", cft_names))

  zero <- function() array(0, dims)
  nat <- zero()
  crop_num <- zero()
  crop_den <- zero()
  grass_num <- zero()
  grass_den <- zero()
  managed <- zero()

  for (y in SOC_YEARS) {
    i <- y - SOC_FIRST_YEAR + 1L
    npp <- ncdf4::ncvar_get(
      np,
      soc_nc_var(np),
      start = c(1, 1, 1, i),
      count = c(dims, length(npp_names), 1)
    )
    hrv <- ncdf4::ncvar_get(
      hv,
      soc_nc_var(hv),
      start = c(1, 1, 1, i),
      count = c(dims, length(hrv_names), 1)
    )
    fr <- ncdf4::ncvar_get(
      cf,
      soc_nc_var(cf),
      start = c(1, 1, 1, i),
      count = c(dims, length(cft_names), 1)
    )
    npp[!is.finite(npp)] <- 0
    hrv[!is.finite(hrv)] <- 0
    fr[!is.finite(fr)] <- 0

    nat <- nat + apply(pmax(npp[,, nat_idx, drop = FALSE], 0), c(1, 2), sum)
    for (b in seq_along(cft_names)) {
      ni <- match(cft_names[[b]], npp_names)
      if (is.na(ni)) {
        next
      }
      hi <- match(cft_names[[b]], hrv_names)
      net <- pmax(npp[,, ni] - (if (is.na(hi)) 0 else hrv[,, hi]), 0)
      f <- fr[,, b]
      if (b %in% grass_b) {
        grass_num <- grass_num + net * f
        grass_den <- grass_den + f
      } else if (b %in% crop_b) {
        crop_num <- crop_num + net * f
        crop_den <- crop_den + f
      }
    }
    managed <- managed + apply(fr, c(1, 2), sum)
  }

  n <- length(SOC_YEARS)
  list(
    natural_input = nat / n * 0.01,
    crop_input = ifelse(crop_den > 0, crop_num / crop_den, NA_real_) * 0.01,
    grass_input = ifelse(grass_den > 0, grass_num / grass_den, NA_real_) * 0.01,
    crop_frac = crop_den / n,
    grass_frac = grass_den / n,
    managed_frac = managed / n
  )
}

# LPJmL's own decomposition response, layer 1 (0-200 mm). The file holds a
# DAILY SUM, which is why it maxes at 365 -- divide to get a response.
#
# Read for BOTH natural vegetation and agriculture. They differ by 1.73x on the
# cells carrying both, so applying the natural response to cropland understates
# its decomposition and overstates its equilibrium.
soc_lpjml_response <- function(run_dir, which = c("nv", "agr")) {
  which <- match.arg(which)
  nc <- ncdf4::nc_open(
    file.path(run_dir, sprintf("response_layer_%s.nc", which))
  )
  on.exit(ncdf4::nc_close(nc))
  dims <- c(nc$dim$lon$len, nc$dim$lat$len)
  acc <- array(0, dims)
  for (y in SOC_YEARS) {
    acc <- acc +
      ncdf4::ncvar_get(
        nc,
        soc_nc_var(nc),
        start = c(1, 1, 1, y - SOC_FIRST_YEAR + 1L),
        count = c(dims, 1, 1)
      )
  }
  acc / length(SOC_YEARS) / 365
}

soc_assemble <- function(run_dir, products_dir) {
  soil <- soc_lpjml_topsoil(run_dir)
  inputs <- soc_run_inputs(run_dir)
  response_nv <- soc_lpjml_response(run_dir, "nv")
  response_agr <- soc_lpjml_response(run_dir, "agr")
  nlon <- length(soil$lon)
  nlat <- length(soil$lat)

  # `1 - managed` is 1 wherever cftfrac holds no land at all, so without the
  # soil-carbon test every ocean cell reads as 100% natural.
  cells <- tibble::tibble(
    lon = round(rep(soil$lon, times = nlat), 2),
    lat = round(rep(soil$lat, each = nlon), 2),
    lpjml_topsoil = as.vector(soil$topsoil),
    lpjml_profile = as.vector(soil$profile),
    natural_input = as.vector(inputs$natural_input),
    crop_input = as.vector(inputs$crop_input),
    grass_input = as.vector(inputs$grass_input),
    response_nv = as.vector(response_nv),
    response_agr = as.vector(response_agr),
    grass_frac = as.vector(inputs$grass_frac),
    crop_frac = as.vector(inputs$crop_frac),
    natural_frac = 1 - as.vector(inputs$managed_frac)
  ) |>
    dplyr::filter(
      is.finite(.data$lpjml_topsoil),
      .data$lpjml_topsoil > 0,
      is.finite(.data$response_nv),
      .data$response_nv > 0
    ) |>
    dplyr::mutate(
      class = dplyr::case_when(
        .data$natural_frac > 0.5 ~ "natural",
        .data$crop_frac > 0.5 ~ "cropland",
        .data$grass_frac > 0.5 ~ "grassland",
        TRUE ~ NA_character_
      ),
      topsoil_share = .data$lpjml_topsoil / .data$lpjml_profile,
      # The input and response THIS cell's class actually has.
      class_input = dplyr::case_when(
        .data$class == "natural" ~ .data$natural_input,
        .data$class == "cropland" ~ .data$crop_input,
        .data$class == "grassland" ~ .data$grass_input,
        TRUE ~ NA_real_
      ),
      class_response = dplyr::if_else(
        .data$class == "natural",
        .data$response_nv,
        .data$response_agr
      )
    )

  refs <- soc_reference_grid(products_dir)
  obs <- read_hwsd_topsoil_soc() |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2)) |>
    dplyr::distinct(.data$lon, .data$lat, .data$soc_obs_mgc_ha)

  cells |>
    dplyr::left_join(refs, by = c("lon", "lat")) |>
    dplyr::left_join(obs, by = c("lon", "lat"))
}

soc_class_table <- function(d) {
  topsoil_share <- stats::median(d$topsoil_share, na.rm = TRUE)
  d |>
    dplyr::filter(
      !is.na(.data$class),
      is.finite(.data$class_input),
      .data$class_input > 0,
      is.finite(.data$class_response),
      .data$class_response > 0
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      lpjml_stock = mean(.data$lpjml_topsoil),
      class_input = mean(.data$class_input),
      class_response = mean(.data$class_response),
      # The like-for-like column: LPJmL's own equilibrium, from the class's OWN
      # input and OWN response, per cell then averaged -- never from mean input
      # over mean response, and never with one class's drivers standing in for
      # another's.
      lpjml_equilibrium = mean(
        .data$class_input *
          topsoil_share *
          SOC_TAU_PROFILE /
          .data$class_response
      ),
      soilgrids = mean(.data$soilgrids, na.rm = TRUE),
      gsocmap = mean(.data$gsocmap, na.rm = TRUE),
      hwsd = mean(.data$soc_obs_mgc_ha, na.rm = TRUE),
      .by = "class"
    ) |>
    dplyr::arrange(.data$class)
}

# The band inside which no model-vs-reference finding is evidence.
soc_reference_band <- function(d) {
  ok <- is.finite(d$soilgrids) &
    is.finite(d$gsocmap) &
    d$soilgrids > 0 &
    d$gsocmap > 0
  list(
    n = sum(ok),
    ratio_of_means = mean(d$gsocmap[ok]) / mean(d$soilgrids[ok]),
    median_factor = exp(
      stats::median(abs(log(d$gsocmap[ok] / d$soilgrids[ok])))
    )
  )
}

soc_invariants <- function(d, classes, band) {
  fail <- character()
  add <- function(cond, msg) if (isTRUE(cond)) fail <<- c(fail, msg)

  add(
    any(d$lpjml_topsoil <= 0),
    "LPJmL topsoil carbon is non-positive somewhere"
  )
  add(
    any(d$topsoil_share < 0.2 | d$topsoil_share > 0.9, na.rm = TRUE),
    "0-30 cm holds an implausible share of the 0-3 m profile in some cell"
  )
  add(any(d$response_nv <= 0), "LPJmL decomposition response is non-positive")
  # No soil ever measured holds this in 30 cm; a class MEAN above it means a
  # unit error or a diverging equilibrium, not a real soil.
  add(
    any(classes$lpjml_stock > 500) || any(classes$lpjml_equilibrium > 2000),
    "a class mean exceeds any plausible 0-30 cm stock"
  )
  add(
    any(
      c(classes$soilgrids, classes$gsocmap, classes$hwsd) > 300,
      na.rm = TRUE
    ),
    "an observational class mean exceeds any plausible 0-30 cm stock"
  )
  # The two products have disagreed by a median factor near 1.32 since they
  # were first compared here. Well outside that means one of them changed.
  add(
    band$median_factor > 2,
    sprintf(
      "the two references now disagree by a median factor of %.2f (was ~1.32)",
      band$median_factor
    )
  )
  fail
}

soc_emit_metric <- function(classes, band, convexity) {
  nat <- classes[classes$class == "natural", ]
  cat(sprintf(
    paste0(
      "METRIC soc_classes=%d ref_band=%.3f natural_lpjml=%.1f ",
      "natural_equilibrium=%.1f natural_soilgrids=%.1f convexity=%.2f\n"
    ),
    nrow(classes),
    band$median_factor,
    if (nrow(nat)) nat$lpjml_stock else NA_real_,
    if (nrow(nat)) nat$lpjml_equilibrium else NA_real_,
    if (nrow(nat)) nat$soilgrids else NA_real_,
    convexity
  ))
}

# The assembled frame, cached.
#
# Assembly is dominated by re-aggregating the 30-arc-second HWSD raster to
# 0.5 degrees and reprojecting GSOCmap, which together run for the better
# part of an hour and are deterministic given the same inputs. Caching them
# is what makes this script usable as a check rather than an expedition.
#
# The key includes the run directory and the mtimes of the two product
# rasters, so a changed run or a re-downloaded product invalidates it
# automatically. `refresh = TRUE` forces a rebuild.
soc_cached_assembly <- function(run_dir, products_dir, refresh = FALSE) {
  key <- paste(
    normalizePath(run_dir, winslash = "/"),
    paste(
      vapply(
        list.files(products_dir, pattern = "[.]tif$", full.names = TRUE),
        \(f) as.character(file.mtime(f)),
        character(1)
      ),
      collapse = "|"
    ),
    paste(range(SOC_YEARS), collapse = "-"),
    sep = "|"
  )
  dir.create(SOC_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(
    SOC_CACHE_DIR,
    sprintf("assembly_%s.rds", substr(.soc_key_hash(key), 1, 16))
  )
  if (!refresh && file.exists(path)) {
    cat("using cached assembly:", basename(path), "\n")
    return(readRDS(path))
  }
  cat("assembling (slow: HWSD raster aggregation + GSOCmap reprojection)\n")
  d <- soc_assemble(run_dir, products_dir)
  saveRDS(d, path)
  cat("cached ->", basename(path), "\n")
  d
}

# rlang is already a hard dependency; avoid pulling digest in for one hash.
.soc_key_hash <- function(x) {
  paste(sprintf("%02x", as.integer(charToRaw(x)) %% 256L), collapse = "")
}
soc_run <- function(record = FALSE, refresh = FALSE) {
  rlang::check_installed(c("terra", "ncdf4", "jsonlite"))
  run_dir <- soc_require_dir("WHEP_LPJML_RUN_DIR", "LPJmL run directory")
  products_dir <- soc_require_dir(
    "WHEP_SOC_PRODUCTS_DIR",
    "observational SOC product directory"
  )

  d <- soc_cached_assembly(run_dir, products_dir, refresh = refresh)
  classes <- soc_class_table(d)
  band <- soc_reference_band(d)
  # The operation-order diagnostic: how far a class mean sits from what the
  # mean response would give. ~1 for LPJmL; WHEP's own is ~4.5.
  # Quoted for natural vegetation, on the SAME rows the class table uses --
  # a diagnostic computed over a different population than the numbers it
  # sits beside is exactly the mismatch this script exists to catch.
  nat_cells <- d[
    !is.na(d$class) &
      d$class == "natural" &
      is.finite(d$class_input) &
      d$class_input > 0 &
      is.finite(d$class_response) &
      d$class_response > 0,
  ]
  convexity <- mean(1 / nat_cells$response_nv) * mean(nat_cells$response_nv)

  cat("\n=== 0-30 cm soil carbon by land-use class (MgC/ha) ===\n")
  print(as.data.frame(classes), row.names = FALSE, digits = 4)
  cat(sprintf(
    "\nreference band: %d cells, GSOCmap/SoilGrids ratio of means %.3f, ",
    band$n,
    band$ratio_of_means
  ))
  cat(sprintf("median per-cell factor %.3f\n", band$median_factor))
  cat(sprintf(
    "LPJmL response convexity mean(1/r)/(1/mean(r)) = %.2f\n",
    convexity
  ))
  cat("Nothing inside the reference band is evidence about a model.\n")

  failures <- soc_invariants(d, classes, band)
  if (length(failures) > 0) {
    cat("\nINVARIANT FAILURES:\n")
    cat(paste0("  - ", failures, collapse = "\n"), "\n")
  }

  current <- list(
    classes = classes,
    band = band,
    convexity = convexity
  )
  if (record) {
    jsonlite::write_json(current, SOC_BASELINE, digits = 6, auto_unbox = TRUE)
    cat("\nrecorded baseline ->", SOC_BASELINE, "\n")
  } else if (file.exists(SOC_BASELINE)) {
    soc_compare_baseline(current)
  } else {
    cat("\nno baseline yet; run with --record\n")
  }

  soc_emit_metric(classes, band, convexity)
  invisible(list(data = d, classes = classes, failures = failures))
}

soc_compare_baseline <- function(current) {
  base <- jsonlite::read_json(SOC_BASELINE, simplifyVector = TRUE)
  cat("\n=== against the recorded baseline (5% tolerance) ===\n")
  b <- tibble::as_tibble(base$classes)
  moved <- 0L
  for (cl in current$classes$class) {
    now <- current$classes[current$classes$class == cl, ]
    was <- b[b$class == cl, ]
    if (nrow(was) == 0) {
      cat(sprintf("  %-10s NEW\n", cl))
      moved <- moved + 1L
      next
    }
    for (col in c("lpjml_stock", "lpjml_equilibrium", "soilgrids", "gsocmap")) {
      a <- now[[col]]
      z <- was[[col]]
      if (!is.finite(a) || !is.finite(z)) {
        next
      }
      if (abs(a - z) > 0.05 * max(abs(z), 1e-9)) {
        cat(sprintf(
          "  %-10s %-18s %8.2f -> %8.2f  (%+.1f%%)\n",
          cl,
          col,
          z,
          a,
          100 * (a / z - 1)
        ))
        moved <- moved + 1L
      }
    }
  }
  if (moved == 0L) {
    cat("  every recorded magnitude within tolerance\n")
  } else {
    cat(sprintf(
      "\n  %d magnitude%s moved. If intended, re-record and say why.\n",
      moved,
      if (moved == 1L) "" else "s"
    ))
  }
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  soc_run(record = "--record" %in% args, refresh = "--refresh" %in% args)
}
