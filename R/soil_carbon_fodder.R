# Fodder crop spatial layer for the soil carbon inputs (whep#1118).
#
# The `spatialize-crop-patterns` pin carries no fodder crop: the 16 forage
# layers of the Monfreda et al. (2008) 175-crop archive are the rows of
# `inst/extdata/earthstat_mapping.csv` with `unmapped_reason ==
# "no_fao_crop_name"` (15) plus `legumenes`, which that crosswalk sends to 463.
# At 2010 that leaves ~298 of the ~310 Mt C the crop-pattern join cannot
# place on fodder items, and `method_unspatialized = "reallocate"` spreads it
# uniformly over the polity's cropland. The rasters themselves exist and are
# reproducibly obtainable (`inst/scripts/download/download_monfreda.R`); this
# file reads them so the fodder carbon can be placed where Monfreda puts the
# fodder harvested area instead, as `method_unspatialized = "fodder_pattern"`.
#
# Source: Monfreda, C., N. Ramankutty and J. A. Foley (2008), Farming the
# planet: 2. Geographic distribution of crop areas, yields, physiological
# types, and net primary production in the year 2000, Global Biogeochem.
# Cycles 22, GB1022, doi:10.1029/2007GB002947. Every layer below is in group
# "Forage" of the archive's METADATA_HarvestedAreaYield175Crops_June2018.pdf.

# EarthStat forage layer -> FAOSTAT item_prod_code.
#
# The archive's metadata gives no FAO name for these layers (its Cropname_FAO
# column repeats the EarthStat code), so the codes are matched on the layer
# name against the FAOSTAT forage items in `items_prod_full`: assumed,
# unverified against a Monfreda table. The match is one-to-one -- 16 layers,
# 16 FAOSTAT forage items -- which is what makes the name reading unambiguous
# for all but `fornes` ("forage nes" -> 651 Forage products) and `vegfor`
# (-> 655 Vegetables and roots fodder), the two catch-all classes.
.fodder_earthstat_layers <- function() {
  tibble::tribble(
    ~earthstat_name, ~item_prod_code,
    "maizefor",      "636",
    "sorghumfor",    "637",
    "ryefor",        "638",
    "grassnes",      "639",
    "clover",        "640",
    "alfalfa",       "641",
    "oilseedfor",    "642",
    "legumenes",     "643",
    "cabbagefor",    "644",
    "mixedgrass",    "645",
    "turnipfor",     "646",
    "beetfor",       "647",
    "carrotfor",     "648",
    "swedefor",      "649",
    "fornes",        "651",
    "vegfor",        "655"
  )
}

# The fodder layer in the same shape `.sci_read_crop_patterns()` returns
# (`lon`, `lat`, `item_prod_code`, `crop_area_ha`), so a fodder crop is gridded
# exactly as it would be had the crop-pattern pin carried it: the static
# harvest fraction times the cell's mean gridded cropland.
.sci_read_fodder_patterns <- function(
  monfreda_dir = Sys.getenv("WHEP_MONFREDA_DIR")
) {
  .sci_combine_crop_patterns(
    .sci_fodder_harvest_fraction(monfreda_dir),
    whep_read_file("spatialize-gridded-cropland")
  )
}

# Per-cell harvest fraction of fodder at 0.5 degrees, aggregated from the
# archive's 5-arcminute HarvestedAreaFraction rasters the same way
# `prepare_crop_patterns()` builds the crop pin (mean over the 6 x 6 block,
# then the float32 underflow floor), and then POOLED: every fodder item gets
# the sum of all 16 forage layers, through the same `.share_pattern_groups()`
# the crop pin uses for its own pooled group.
#
# Pooled rather than per item because the per-item layers describe the
# circa-2000 reporting vocabulary, not where each fodder item is grown in
# later years. The United States reports 639 "Forage and silage, grasses nes"
# at 2010 (18.3 Mt C of the unplaced carbon), but Monfreda's `grassnes` layer
# has 56 US cells, all south of 33 N along the Mexican border -- Mexican
# grassnes spilling across border cells -- so a per-item layer would put that
# carbon on the Rio Grande (L1 against uniform = 2.00: disjoint support). The
# pooled layer is where Monfreda puts the fodder crops as a group, which is
# the claim the source supports.
.sci_fodder_harvest_fraction <- function(monfreda_dir, target_res = 0.5) {
  geotiff_dir <- .sci_monfreda_geotiff_dir(monfreda_dir)
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort(
      "{.pkg terra} is needed to read the Monfreda fodder rasters."
    )
  }
  layers <- .fodder_earthstat_layers()
  purrr::map2(
    layers$earthstat_name,
    layers$item_prod_code,
    \(name, code) .sci_read_fodder_layer(geotiff_dir, name, code, target_res)
  ) |>
    dplyr::bind_rows() |>
    .share_pattern_groups(dplyr::mutate(layers, pattern_group = "fodder"))
}

# The GeoTiff directory, from either the dataset folder the download script
# writes (`<dest_dir>/HarvestedAreaYield175Crops_Geotiff`) or its `GeoTiff`
# child. Aborts with the instruction when the env var is unset or wrong: never
# a silent fallback to uniform reallocation.
.sci_monfreda_geotiff_dir <- function(monfreda_dir) {
  if (!.has_path(monfreda_dir)) {
    cli::cli_abort(
      c(
        "{.code method_unspatialized = \"fodder_pattern\"} needs the Monfreda
         et al. (2008) crop rasters.",
        i = "Run {.file inst/scripts/download/download_monfreda.R} and set
             {.envvar WHEP_MONFREDA_DIR} to its
             {.file HarvestedAreaYield175Crops_Geotiff} folder, or pass
             {.code data$fodder_patterns}."
      ),
      class = "whep_missing_monfreda"
    )
  }
  candidates <- c(file.path(monfreda_dir, "GeoTiff"), monfreda_dir)
  found <- candidates[dir.exists(file.path(candidates, "alfalfa"))]
  if (length(found) == 0) {
    cli::cli_abort(
      c(
        "No Monfreda forage layers under {.path {monfreda_dir}}.",
        i = "Expected {.file GeoTiff/alfalfa/alfalfa_HarvestedAreaFraction.tif}
             beneath {.envvar WHEP_MONFREDA_DIR}."
      ),
      class = "whep_missing_monfreda"
    )
  }
  found[[1]]
}

.sci_read_fodder_layer <- function(geotiff_dir, name, code, target_res) {
  path <- file.path(
    geotiff_dir,
    name,
    paste0(name, "_HarvestedAreaFraction.tif")
  )
  if (!file.exists(path)) {
    cli::cli_abort(
      "Monfreda forage layer {.val {name}} is missing: {.path {path}}.",
      class = "whep_missing_monfreda"
    )
  }
  r <- terra::rast(path)
  fact <- max(1L, as.integer(round(target_res / terra::res(r)[[1]])))
  if (fact > 1L) {
    r <- terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
  }
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  tibble::tibble(
    lon = df[[1]],
    lat = df[[2]],
    item_prod_code = code,
    harvest_fraction = df[[3]]
  ) |>
    dplyr::filter(
      is.finite(.data$harvest_fraction),
      .data$harvest_fraction >= .crop_pattern_signal_floor()
    )
}

# Add the fodder layer's cell weights for the polity-crops the crop-pattern
# weights do not already place. A crop that the crop pattern carries keeps its
# own cells: the fodder layer only fills the hole, it never overrides a layer.
.sci_add_fodder_weights <- function(weights, country_grid, fodder_patterns) {
  if (is.null(fodder_patterns) || nrow(fodder_patterns) == 0) {
    return(weights)
  }
  fodder <- .sci_grid_weights(country_grid, fodder_patterns) |>
    dplyr::anti_join(
      dplyr::distinct(weights, .data$area_code, .data$item_prod_code),
      by = c("area_code", "item_prod_code")
    )
  dplyr::bind_rows(weights, fodder)
}

# Report the carbon the fodder layer placed, so the move from uniform
# reallocation is visible and sized. Informational: placing carbon on a sourced
# layer is not a loss.
.sci_inform_fodder_placed <- function(components, weights, fodder_weights) {
  added <- dplyr::anti_join(
    dplyr::distinct(fodder_weights, .data$area_code, .data$item_prod_code),
    dplyr::distinct(weights, .data$area_code, .data$item_prod_code),
    by = c("area_code", "item_prod_code")
  )
  placed <- dplyr::semi_join(
    components,
    added,
    by = c("area_code", "item_prod_code")
  )
  if (nrow(placed) == 0) {
    return(invisible(NULL))
  }
  n <- nrow(placed)
  mass <- round(sum(placed$c_mass_mg, na.rm = TRUE), 3)
  cli::cli_inform(c(
    i = "{cli::qty(n)}{n} polity-crop carbon component{?s} ({mass} Mg C)
         placed on the Monfreda et al. (2008) fodder layer."
  ))
  invisible(placed)
}
