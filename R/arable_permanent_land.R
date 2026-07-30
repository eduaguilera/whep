#' Physical arable and permanent-crop land base (fallow-inclusive).
#'
#' @description
#' Return FAO's physical land-use split of cropland into **arable land**
#' (annual/temporary crops plus their rotational fallow and temporary
#' meadows) and **permanent-crop land** (orchards, plantations, vineyards),
#' keyed by `(area_code, year)`.
#'
#' whep's other crop-area paths ([get_crop_land_extension()],
#' [build_cropgrids_land_extension()]) are all derived from crop *production* /
#' harvested area and therefore cannot recover the physical fallow-inclusive
#' arable land of rain-fed, fallow-prone economies: in a drought year a country's
#' cereal harvest collapses while its arable land (which counts the resting
#' fallow) is unchanged, so a harvested-area method assigns that land to
#' perennials and over-states the permanent share (e.g. Tunisia 2020 permanent
#' share 0.73 from harvested area vs 0.43 physical). FAO's RL land-use survey
#' (`Cropland` = `Arable land` + `Permanent crops`) is the physical land base;
#' this function ingests it.
#'
#' From 1961 the split is FAO's own (`source == "fao"`). Before 1961 (FAOSTAT's
#' start) it is backcast from LUH2 land use: LUH2's annual vs. perennial crop
#' functional types give a perennial fraction and a cropland shape that are
#' spliced onto the FAO 1961 level so the series is continuous
#' (`source == "luh2"`). See Details.
#'
#' @details
#' The FAO identity `Cropland = Arable land + Permanent crops` holds in the
#' source to rounding for essentially all country-years; `permanent_ha` is taken
#' as `Cropland - Arable land` (clamped at 0) so `arable_ha + permanent_ha`
#' reconstructs `cropland_ha` exactly wherever FAO reports `Arable <= Cropland`.
#' Where FAO reports `Arable land` but not `Permanent crops` (924 country-years,
#' mostly arable-only economies) this yields the permanent land the survey
#' implies; where it reports `Permanent crops` but not `Arable land` (a few
#' coconut atolls) `arable_ha` is filled from `Cropland - Permanent crops`.
#'
#' Pre-1961 backcast: LUH2 annual cropland is `c3ann + c4ann + c3nfx`, perennial
#' is `c3per + c4per`. For each country the perennial fraction and the cropland
#' level are rescaled by their ratio to the LUH2 value at 1961 and multiplied by
#' the FAO 1961 perennial fraction and cropland, so both match FAO exactly at the
#' 1961 splice point and carry LUH2's earlier dynamics backwards. Countries
#' without a FAO 1961 anchor receive no backcast.
#'
#' @param years Integer vector of years to return, or `NULL` (default) for all
#'   available (1700-2025). The pre-1961 LUH2 backcast is computed only when
#'   `years` is `NULL` or requests a year before 1961.
#' @param input_dir Optional directory holding a local FAOSTAT RL land-use file
#'   (`faostat_land_use.csv` or a parquet with the FAOSTAT RL columns). If `NULL`
#'   (default) the pinned `faostat-landuse` dataset is read via [whep_read_file()].
#' @param data Optional in-memory FAOSTAT RL table in the raw pin schema (columns
#'   `Area Code`, `Item Code`, `Element`, `Unit`, `Year`, `Value`), used instead
#'   of the pin (chiefly for testing).
#' @param luh2_data Optional in-memory LUH2 land-use table (columns `ISO3`,
#'   `Year`, `Land_Use`, `Area_Mha`) used for the pre-1961 backcast instead of
#'   the pinned `luh2-areas` dataset (chiefly for testing).
#' @param example If `TRUE`, return a small illustrative table without reading
#'   remote data. Defaults to `FALSE`.
#'
#' @return A tibble with one row per `(area_code, year)`:
#' - `area_code`: integer FAOSTAT area code (harmonised via
#'   [polity_area_crosswalk]; the FAOSTAT "China" aggregate 351 is dropped).
#' - `year`: integer.
#' - `arable_ha`, `permanent_ha`, `cropland_ha`: physical land area in hectares.
#' - `source`: provenance, `"fao"` (>= 1961) or `"luh2"` (pre-1961 backcast).
#'
#' @export
#'
#' @examples
#' get_arable_permanent_land(example = TRUE)
get_arable_permanent_land <- function(
  years = NULL,
  input_dir = NULL,
  data = NULL,
  luh2_data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_arable_permanent_land())
  }
  years <- if (is.null(years)) NULL else as.integer(years)

  rl_long <- .read_fao_rl(data = data, input_dir = input_dir)
  fao <- .fao_rl_to_wide(rl_long)
  fao$source <- "fao"

  need_pre <- is.null(years) || any(years < 1961L)
  out <- fao
  if (need_pre) {
    pre <- .luh2_perennial_backcast(fao, years = years, luh2_data = luh2_data)
    if (!is.null(pre) && nrow(pre) > 0L) {
      out <- dplyr::bind_rows(fao, pre)
    }
  }

  if (!is.null(years)) {
    out <- out[out$year %in% years, , drop = FALSE]
  }
  out <- out[order(out$area_code, out$year), , drop = FALSE]
  tibble::as_tibble(out)
}

# -- FAO RL land-use ingestion -------------------------------------------------

# FAOSTAT RL "Land Use" domain item codes for the physical land base.
.fao_rl_items <- function() {
  c(cropland = 6620L, arable = 6621L, permanent = 6650L)
}

# Read the FAOSTAT RL land-use domain and return a normalised long table with
# columns (area_code, year, item_code, ha) for the Cropland / Arable land /
# Permanent crops physical Area rows only, harmonised to whep area_code.
.read_fao_rl <- function(data = NULL, input_dir = NULL) {
  raw <- .fetch_fao_rl(data = data, input_dir = input_dir)
  # The normalisation below renames and adds columns by reference. Copy first so
  # a caller-owned data.table is not modified; internally read data can be
  # normalised in place without doubling its memory footprint.
  dt <- data.table::as.data.table(raw)
  if (!is.null(data) && data.table::is.data.table(raw)) {
    dt <- data.table::copy(dt)
  }

  # Accept both the raw FAOSTAT pin schema (spaced names) and an already
  # snake_cased local file.
  ren <- c(
    "Area Code" = "area_code_fao",
    "Item Code" = "item_code",
    "Element" = "element",
    "Unit" = "unit",
    "Year" = "year",
    "Value" = "value",
    "area_code" = "area_code_fao",
    "area_fao_code" = "area_code_fao",
    "item" = "item_name"
  )
  for (from in names(ren)) {
    if (from %in% names(dt) && !ren[[from]] %in% names(dt)) {
      data.table::setnames(dt, from, ren[[from]])
    }
  }
  # A local cross-check csv identifies items by name, not code.
  if (!"item_code" %in% names(dt) && "item_name" %in% names(dt)) {
    dt[,
      item_code := data.table::fcase(
        tolower(item_name) == "cropland"        ,
        6620L                                   ,
        tolower(item_name) == "arable land"     ,
        6621L                                   ,
        tolower(item_name) == "permanent crops" ,
        6650L                                   ,
        default = NA_integer_
      )
    ]
  }
  if (!"element" %in% names(dt)) {
    dt[, element := "Area"]
  }
  if (!"unit" %in% names(dt)) {
    dt[, unit := "1000 ha"]
  }
  items <- .fao_rl_items()
  dt <- dt[
    element == "Area" &
      unit %in% c("1000 ha", "1000 Ha", "1000ha") &
      item_code %in% items
  ]
  dt[, `:=`(
    area_code_fao = as.integer(area_code_fao),
    year = as.integer(year),
    item_code = as.integer(item_code),
    ha = as.numeric(value) * 1000
  )]
  dt <- dt[!is.na(area_code_fao) & !is.na(year) & !is.na(ha)]

  # Harmonise FAOSTAT area code -> whep polity area_code (the key used by
  # get_primary_production() and every whep output), so split/merged FAOSTAT
  # territories line up (Ethiopia PDR 62 -> 238, Sudan 276/South Sudan 277 ->
  # 206, ...). FAOSTAT aggregates with no ISO3 (notably "China" 351, which
  # overlaps 41/96/128/214) have no crosswalk row and are dropped.
  bridge <- data.table::as.data.table(whep::polity_area_crosswalk)[
    !is.na(area_iso3c),
    .(
      area_code_fao = as.integer(area_code),
      polity = as.integer(polity_area_code)
    )
  ]
  bridge <- unique(bridge, by = "area_code_fao")
  dt <- merge(dt, bridge, by = "area_code_fao")
  dt[, .(area_code = polity, year, item_code, ha)]
}

.fetch_fao_rl <- function(data = NULL, input_dir = NULL) {
  if (!is.null(data)) {
    return(data)
  }
  if (!is.null(input_dir) && nzchar(input_dir)) {
    csv <- file.path(input_dir, "faostat_land_use.csv")
    pq <- list.files(input_dir, pattern = "\\.parquet$", full.names = TRUE)
    if (file.exists(csv)) {
      return(data.table::fread(csv, showProgress = FALSE))
    }
    if (length(pq) > 0L) {
      return(nanoparquet::read_parquet(pq[[1L]]))
    }
    cli::cli_abort(c(
      "No FAOSTAT RL land-use file found in {.path {input_dir}}.",
      "i" = "Expected {.file faostat_land_use.csv} or a {.file .parquet}."
    ))
  }
  whep_read_file("faostat-landuse")
}

# Turn the normalised RL long table into one wide row per (area_code, year) with
# cropland_ha / arable_ha / permanent_ha, enforcing the FAO identity
# Cropland = Arable + Permanent (see Details).
.fao_rl_to_wide <- function(rl_long) {
  .check_required_cols(
    rl_long,
    c("area_code", "year", "item_code", "ha"),
    "rl_long"
  )
  items <- .fao_rl_items()
  dt <- data.table::as.data.table(rl_long)
  w <- data.table::dcast(
    dt,
    area_code + year ~ item_code,
    value.var = "ha",
    fun.aggregate = sum,
    fill = NA_real_
  )
  # Keep an unreported item as NA while preserving an explicitly reported zero.
  # The distinction is needed for all-permanent and all-arable economies.
  for (nm in as.character(items)) {
    if (!nm %in% names(w)) {
      w[, (nm) := NA_real_]
    }
  }
  w[, cropland_ha := get(as.character(items[["cropland"]]))]
  w[, arable_rep := get(as.character(items[["arable"]]))]
  w[, permanent_rep := get(as.character(items[["permanent"]]))]
  # Zero total cropland carries no physical land extension. Component zeros,
  # however, are valid observations and must remain distinguishable from NA.
  w[cropland_ha == 0, cropland_ha := NA_real_]

  # arable = reported Arable land; where absent but Cropland and Permanent are
  # present, fill from the FAO identity so all-permanent economies are kept.
  w[,
    arable_ha := data.table::fifelse(
      is.na(arable_rep) & !is.na(cropland_ha) & !is.na(permanent_rep),
      pmax(cropland_ha - permanent_rep, 0),
      arable_rep
    )
  ]
  w <- w[!is.na(cropland_ha) & !is.na(arable_ha)]
  w[, permanent_ha := pmax(cropland_ha - arable_ha, 0)]
  w[, .(
    area_code = as.integer(area_code),
    year = as.integer(year),
    arable_ha = as.numeric(arable_ha),
    permanent_ha = as.numeric(permanent_ha),
    cropland_ha = as.numeric(cropland_ha)
  )]
}

# -- Pre-1961 LUH2 backcast ----------------------------------------------------

# Per (area_code, year) LUH2 annual vs perennial cropland (Mha -> ha), mapped to
# whep area_code via ISO3. annual = c3ann+c4ann+c3nfx, perennial = c3per+c4per.
.read_luh2_cft <- function(luh2_data = NULL) {
  raw <- if (!is.null(luh2_data)) {
    luh2_data
  } else {
    .read_input("luh2-areas", years = NULL, year_col = "Year")
  }
  # setnames() mutates by reference, so preserve a caller-owned data.table while
  # avoiding an extra full copy of the internally read LUH2 dataset.
  dt <- data.table::as.data.table(raw)
  if (!is.null(luh2_data) && data.table::is.data.table(raw)) {
    dt <- data.table::copy(dt)
  }
  if ("ISO3" %in% names(dt) && !"iso3c" %in% names(dt)) {
    data.table::setnames(dt, "ISO3", "iso3c")
  }
  if ("Year" %in% names(dt) && !"year" %in% names(dt)) {
    data.table::setnames(dt, "Year", "year")
  }
  annual <- c("c3ann", "c4ann", "c3nfx")
  perennial <- c("c3per", "c4per")
  dt <- dt[Land_Use %in% c(annual, perennial)]
  dt[, kind := data.table::fifelse(Land_Use %in% annual, "annual", "perennial")]

  bridge <- data.table::as.data.table(whep::polity_area_crosswalk)[
    !is.na(area_iso3c),
    .(iso3c = area_iso3c, area_code = as.integer(polity_area_code))
  ]
  bridge <- unique(bridge, by = "iso3c")
  dt <- merge(dt, bridge, by = "iso3c", sort = FALSE)
  agg <- dt[,
    .(area_ha = sum(Area_Mha, na.rm = TRUE) * 1e6),
    by = .(area_code, year = as.integer(year), kind)
  ]
  w <- data.table::dcast(
    agg,
    area_code + year ~ kind,
    value.var = "area_ha",
    fill = 0
  )
  if (!"annual" %in% names(w)) {
    w[, annual := 0]
  }
  if (!"perennial" %in% names(w)) {
    w[, perennial := 0]
  }
  w[, luh2_cropland := annual + perennial]
  w
}

# Backcast arable/permanent for years < 1961 by splicing the LUH2 perennial
# fraction and cropland shape onto the FAO 1961 anchor (see Details on the
# exported function).
.luh2_perennial_backcast <- function(fao, years = NULL, luh2_data = NULL) {
  anchor_year <- 1961L
  fao_dt <- data.table::as.data.table(fao)
  anchor <- fao_dt[
    year == anchor_year,
    .(
      area_code,
      fao_crop1961 = cropland_ha,
      fao_permfrac1961 = permanent_ha / cropland_ha
    )
  ]
  if (nrow(anchor) == 0L) {
    return(NULL)
  }

  luh2 <- .read_luh2_cft(luh2_data = luh2_data)
  pre_years <- sort(unique(luh2[year < anchor_year, year]))
  if (!is.null(years)) {
    pre_years <- pre_years[pre_years %in% years]
  }
  if (length(pre_years) == 0L) {
    return(NULL)
  }

  luh2_anchor <- luh2[
    year == anchor_year,
    .(
      area_code,
      luh2_crop1961 = luh2_cropland,
      luh2_permfrac1961 = data.table::fifelse(
        luh2_cropland > 0,
        perennial / luh2_cropland,
        NA_real_
      )
    )
  ]

  pre <- luh2[year %in% pre_years]
  pre <- merge(pre, anchor, by = "area_code")
  pre <- merge(pre, luh2_anchor, by = "area_code")
  pre <- pre[luh2_crop1961 > 0]

  pre[,
    luh2_permfrac := data.table::fifelse(
      luh2_cropland > 0,
      perennial / luh2_cropland,
      NA_real_
    )
  ]
  # Splice: cropland level and perennial fraction each rescaled to the FAO 1961
  # value by their LUH2 ratio to 1961, so both match FAO exactly at the splice.
  pre[, cropland_ha := fao_crop1961 * luh2_cropland / luh2_crop1961]
  pre[,
    permfrac := data.table::fifelse(
      is.na(luh2_permfrac) | is.na(luh2_permfrac1961) | luh2_permfrac1961 == 0,
      fao_permfrac1961,
      fao_permfrac1961 * luh2_permfrac / luh2_permfrac1961
    )
  ]
  pre[, permfrac := pmin(pmax(permfrac, 0), 1)]
  pre[, `:=`(
    permanent_ha = permfrac * cropland_ha,
    arable_ha = (1 - permfrac) * cropland_ha
  )]
  pre[, source := "luh2"]
  pre[
    is.finite(cropland_ha) & cropland_ha > 0,
    .(
      area_code = as.integer(area_code),
      year = as.integer(year),
      arable_ha,
      permanent_ha,
      cropland_ha,
      source
    )
  ]
}

.example_arable_permanent_land <- function() {
  tibble::tribble(
    ~area_code, ~year, ~arable_ha, ~permanent_ha, ~cropland_ha, ~source,
    222L, 2020L, 2831300, 2119200, 4950500, "fao",
    222L, 1960L, 2600000, 1500000, 4100000, "luh2"
  )
}

#' Build a per-crop physical land extension with FAO fallow-inclusive arable land.
#'
#' @description
#' Turn per-crop harvested-derived physical area into a fallow-inclusive physical
#' land extension whose arable-crop total reconciles to FAO's physical
#' **Arable land** and whose perennial-crop total reconciles to FAO's physical
#' **Permanent crops** ([get_arable_permanent_land()]), per `(area_code, year)`.
#'
#' This is the FAO-land-base analogue of [build_cropgrids_land_extension()]`(source
#' = "cropgrids_fallow")`. The existing method takes the fallow *magnitude* from
#' FAOSTAT "Temporary fallow" (item 6640, a sparse and, for many rain-fed
#' economies, absent series) applied to a single CROPGRIDS 2020 snapshot. Here
#' the fallow magnitude is the physical arable land that carried no harvest in
#' that specific year, `FAO Arable land - sum(cropped arable physical)`, so a
#' drought year's resting cropland is charged to the crops whose rotation it
#' supports and the arable-crop footprint totals match FAO's land survey in every
#' year (see the Tunisia/Portugal motivation in [get_arable_permanent_land()]).
#'
#' Reconciliation, per `(area_code, year)`:
#' - **Arable crops** (`items_prod_full$Herb_Woody != "Woody"`): rotational
#'   fallow `max(0, arable_ha - S)` (with `S` the cropped arable physical total)
#'   is distributed with [attribute_fallow_to_crops()] using `fallow_weights`, so
#'   the arable total reaches `arable_ha`. Where the cropped physical already
#'   exceeds `arable_ha` (heavy multi-cropping, or inflated fodder harvested
#'   area) there is no fallow to add and the arable crops are scaled down to
#'   `arable_ha` instead, the physical-container correction. Either way the
#'   arable total equals FAO `arable_ha` by construction.
#' - **Perennial crops** (`Herb_Woody == "Woody"`) receive no fallow and are
#'   scaled so their total equals FAO `permanent_ha`, preserving the within-group
#'   physical pattern.
#' A positive target without a corresponding arable crop row or positive
#' perennial base area is reported as an error because it cannot be reconciled
#' without inventing a crop allocation.
#'
#' This is the crop-side default of the land-balance footprint
#' ([build_land_balance_footprint()]).
#'
#' @section Temporary grassland (no double-count):
#' FAO's **Arable land** total includes *temporary meadows and pastures* —
#' temporary grassland is part of cropland, not grassland. That land is also
#' reported separately as CBS 3002 (`Temporary grassland`) by
#' [build_grassland_land_extension()], so summing both extensions naively would
#' count it twice. Pass that grassland occupation as `temporary_grassland` and
#' its CBS 3002 is netted out of the arable target before reconciling ordinary
#' crops, enforcing the invariant per `(area_code, year)`
#' `ordinary crop occupation (incl. fallow) + CBS 3002 = FAO Arable land`. The
#' land-balance footprint ([build_land_balance_footprint()]) does exactly this,
#' passing the grassland occupation it has already built. When
#' `temporary_grassland` is `NULL` (default) the grassland occupation extension
#' is built internally so netting still happens — correct but slow, since that
#' build reruns much of the pipeline; supply the table to avoid the rebuild.
#' Where modelled CBS 3002 exceeds FAO Arable land (survey vs.
#' fodder-reconstruction mismatch) the arable target is clamped at 0 and a
#' warning is emitted.
#'
#' @param harvested Tibble of harvested area with columns `year`, `area_code`,
#'   `item_cbs_code`, `harvested_ha`. If `NULL`, built from
#'   [get_primary_production()] (`unit == "ha"`); passing a cached harvested
#'   table avoids that rebuild.
#' @param arable_permanent Tibble of FAO physical land base with columns
#'   `area_code`, `year`, `arable_ha`, `permanent_ha`. If `NULL`,
#'   [get_arable_permanent_land()] is called for the years present in
#'   `base_extension`.
#' @param base_extension Tibble of cropped (fallow-excluding) per-crop physical
#'   area with columns `year`, `area_code`, `item_cbs_code`, `impact_u`. If
#'   `NULL`, built with [build_cropgrids_land_extension()]`(source = "cropgrids")`
#'   from `harvested`.
#' @param fallow_weights Tibble of `area_code`, `item_cbs_code`, `weight` giving
#'   the within-country fallow allocation weight, e.g. from
#'   [gridded_fallow_weights()] (the recommended agro-climatic, rainfed-gated
#'   weight). If `NULL`, fallow is distributed in proportion to each arable
#'   crop's cropped physical area (perennials always excluded). The cropped-area
#'   fallback is used independently for an area when it has no usable supplied
#'   weights, a non-finite or negative supplied weight, or a non-positive total.
#' @param temporary_grassland Tibble of grassland occupation in the
#'   [build_grassland_land_extension()] schema (`area_code`, `year`,
#'   `item_cbs_code`, `impact_u`); its CBS 3002 rows are the temporary grassland
#'   netted out of the arable target so ordinary crops plus CBS 3002 reconcile to
#'   FAO Arable land (see the temporary-grassland section). If `NULL` (default)
#'   it is built with [build_grassland_land_extension()]`(grassland_metric =
#'   "occupation")` so netting still applies (correct but slow); supply the table
#'   to skip that rebuild, or pass one with no CBS 3002 rows to opt out.
#' @param items_prod_full Crosswalk used to classify `item_cbs_code` as arable or
#'   perennial via `Herb_Woody`. Defaults to [items_prod_full].
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (fallow-inclusive physical land in hectares), and `method_land`
#'   (`"fao_arable_fallow"`).
#'
#' @export
#'
#' @examples
#' harvested <- tibble::tribble(
#'   ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
#'   2020L, 1L, 2511L, 300, # wheat (arable)
#'   2020L, 1L, 2560L, 100 # coconuts (perennial)
#' )
#' base_extension <- tibble::tribble(
#'   ~year, ~area_code, ~item_cbs_code, ~impact_u,
#'   2020L, 1L, 2511L, 300,
#'   2020L, 1L, 2560L, 100
#' )
#' arable_permanent <- tibble::tribble(
#'   ~area_code, ~year, ~arable_ha, ~permanent_ha,
#'   1L, 2020L, 500, 100
#' )
#' items <- tibble::tribble(
#'   ~item_cbs_code, ~Herb_Woody,
#'   2511L, "Herbaceous",
#'   2560L, "Woody"
#' )
#' temporary_grassland <- tibble::tribble(
#'   ~area_code, ~year, ~item_cbs_code, ~impact_u,
#'   1L, 2020L, 3002L, 100 # temporary grassland netted out of arable
#' )
#' build_fao_arable_fallow_extension(
#'   harvested, arable_permanent, base_extension,
#'   temporary_grassland = temporary_grassland,
#'   items_prod_full = items
#' )
# nolint start: object_length_linter.
build_fao_arable_fallow_extension <- function(
  harvested = NULL,
  arable_permanent = NULL,
  base_extension = NULL,
  fallow_weights = NULL,
  temporary_grassland = NULL, # nolint: object_length_linter.
  items_prod_full = whep::items_prod_full
) {
  if (is.null(base_extension)) {
    base_extension <- build_cropgrids_land_extension(
      harvested = harvested,
      source = "cropgrids"
    )
  }
  .check_required_cols(
    base_extension,
    c("year", "area_code", "item_cbs_code", "impact_u"),
    "base_extension"
  )
  base <- data.table::as.data.table(base_extension)[, .(
    year = as.integer(year),
    area_code = as.integer(area_code),
    item_cbs_code = as.integer(item_cbs_code),
    physical_ha = as.numeric(impact_u)
  )]

  if (is.null(arable_permanent)) {
    arable_permanent <- get_arable_permanent_land(
      years = sort(unique(base$year))
    )
  }
  .check_required_cols(
    arable_permanent,
    c("area_code", "year", "arable_ha", "permanent_ha"),
    "arable_permanent"
  )
  ap <- data.table::as.data.table(arable_permanent)[, .(
    area_code = as.integer(area_code),
    year = as.integer(year),
    arable_ha = as.numeric(arable_ha),
    permanent_ha = as.numeric(permanent_ha)
  )]

  # FAO Arable land already contains temporary meadows and pastures (CBS 3002),
  # which the grassland extension reports separately. Net that land out of the
  # arable target so ordinary crops reconcile to the arable land they alone
  # occupy and the invariant ordinary + CBS 3002 = FAO arable holds.
  ap <- .net_temporary_grassland(ap, temporary_grassland)

  perennial_codes <- .item_cbs_perennial(items_prod_full)
  base[,
    kind := data.table::fifelse(
      item_cbs_code %in% perennial_codes,
      "perennial",
      "arable"
    )
  ]

  # Static (year-independent) allocation weight, e.g. gridded_fallow_weights().
  # When NULL, each year's fallow is distributed by that year's cropped arable
  # physical area (built per year inside .reconcile_fao_arable_fallow()).
  weights <- NULL
  if (!is.null(fallow_weights)) {
    .check_required_cols(
      fallow_weights,
      c("area_code", "item_cbs_code", "weight"),
      "fallow_weights"
    )
    weights <- data.table::as.data.table(fallow_weights)[, .(
      area_code = as.integer(area_code),
      item_cbs_code = as.integer(item_cbs_code),
      weight = as.numeric(weight)
    )]
    weights <- weights[!item_cbs_code %in% perennial_codes]
  }

  out <- .reconcile_fao_arable_fallow(base, ap, weights)
  out[, method_land := "fao_arable_fallow"]
  out <- out[impact_u > 0]
  data.table::setorder(out, year, area_code, item_cbs_code)
  tibble::as_tibble(out)
}
# nolint end

# Integer item_cbs_code values that are perennial (Woody), resolved by majority
# where an item_cbs maps to item_prod of mixed Herb_Woody class.
.item_cbs_perennial <- function(items_prod_full) {
  dt <- data.table::as.data.table(items_prod_full)
  if (!"Herb_Woody" %in% names(dt) || !"item_cbs_code" %in% names(dt)) {
    return(integer(0))
  }
  dt <- dt[
    !is.na(item_cbs_code) & !is.na(Herb_Woody),
    .(
      item_cbs_code = as.integer(item_cbs_code),
      Herb_Woody
    )
  ]
  tally <- dt[, .N, by = .(item_cbs_code, Herb_Woody)]
  data.table::setorder(tally, item_cbs_code, -N)
  majority <- tally[, .SD[1L], by = item_cbs_code]
  majority[Herb_Woody == "Woody", item_cbs_code]
}

# Subtract temporary grassland (CBS 3002) from each (area_code, year) arable
# target so ordinary arable crops reconcile to the arable land they alone
# occupy. Modelled CBS 3002 can exceed FAO arable land for a few country-years
# (survey vs. fodder-reconstruction mismatch); those are clamped at 0 and warned.
.net_temporary_grassland <- function(ap, temporary_grassland) {
  temp <- .temporary_grassland_ha(temporary_grassland)
  if (nrow(temp) == 0L) {
    return(ap)
  }
  ap <- merge(ap, temp, by = c("area_code", "year"), all.x = TRUE)
  ap[is.na(temp_grassland_ha), temp_grassland_ha := 0]
  overshoot <- ap[temp_grassland_ha > arable_ha]
  if (nrow(overshoot) > 0L) {
    keys <- paste(
      paste(overshoot$area_code, overshoot$year, sep = "/"),
      collapse = ", "
    )
    cli::cli_warn(c(
      "Modelled temporary grassland (CBS 3002) exceeds FAO arable land for {.val {keys}}.",
      i = "Arable target clamped at 0; combined crop plus CBS 3002 occupation will exceed FAO arable land there."
    ))
  }
  ap[, arable_ha := pmax(0, arable_ha - temp_grassland_ha)]
  ap[, temp_grassland_ha := NULL]
  ap[]
}

# Temporary grassland (CBS 3002) hectares per (area_code, year). NULL builds the
# grassland occupation extension so netting is applied by default (correct but
# slow); a supplied table (grassland extension schema: area_code, year,
# item_cbs_code, impact_u) is reused as-is, from which CBS 3002 is kept. Pass a
# table with no CBS 3002 rows to opt out of netting.
.temporary_grassland_ha <- function(temporary_grassland) {
  if (is.null(temporary_grassland)) {
    temporary_grassland <- build_grassland_land_extension(
      grassland_metric = "occupation"
    )
  }
  .check_required_cols(
    temporary_grassland,
    c("area_code", "year", "item_cbs_code", "impact_u"),
    "temporary_grassland"
  )
  dt <- data.table::as.data.table(temporary_grassland)
  dt <- dt[as.integer(item_cbs_code) == 3002L] # CBS 3002 temporary grassland.
  dt[,
    .(temp_grassland_ha = sum(as.numeric(impact_u))),
    by = .(area_code = as.integer(area_code), year = as.integer(year))
  ]
}

# Per (area_code, year): add rotational fallow to arable crops up to FAO Arable
# land (scaling the cropped physical down instead when it already exceeds it, the
# physical-container correction), and scale perennial crops to FAO Permanent
# crops. The additive fallow distribution reuses attribute_fallow_to_crops().
.reconcile_fao_arable_fallow <- function(base, ap, weights) {
  arable <- base[kind == "arable"]
  peren <- base[kind == "perennial"]

  # A positive target cannot be manufactured when the corresponding crop kind
  # has no row (or, for proportional perennial scaling, has zero base area).
  support <- base[,
    .(
      arable_rows = sum(kind == "arable"),
      perennial_base = sum(physical_ha[kind == "perennial"])
    ),
    by = .(area_code, year)
  ]
  support <- merge(
    support,
    ap[, .(area_code, year, arable_ha, permanent_ha)],
    by = c("area_code", "year"),
    all.x = TRUE
  )
  unsupported_arable <- support[
    !is.na(arable_ha) & arable_ha > 0 & arable_rows == 0L
  ]
  if (nrow(unsupported_arable) > 0L) {
    keys <- paste(
      paste(unsupported_arable$area_code, unsupported_arable$year, sep = "/"),
      collapse = ", "
    )
    cli::cli_abort(
      "Cannot reconcile positive arable totals without arable crop rows: {.val {keys}}."
    )
  }
  unsupported_perennial <- support[
    !is.na(permanent_ha) & permanent_ha > 0 & perennial_base <= 0
  ]
  if (nrow(unsupported_perennial) > 0L) {
    keys <- paste(
      paste(
        unsupported_perennial$area_code,
        unsupported_perennial$year,
        sep = "/"
      ),
      collapse = ", "
    )
    cli::cli_abort(
      "Cannot reconcile positive permanent-crop totals without positive perennial base area: {.val {keys}}."
    )
  }

  # --- arable: pre-scale any per-year overshoot down to FAO arable, then let
  #     attribute_fallow_to_crops() distribute the remaining slack as fallow. ---
  s_arable <- arable[,
    .(base_arable = sum(physical_ha)),
    by = .(area_code, year)
  ]
  s_arable <- merge(
    s_arable,
    ap[, .(area_code, year, arable_ha)],
    by = c("area_code", "year"),
    all.x = TRUE
  )
  s_arable[is.na(arable_ha), arable_ha := base_arable]
  s_arable[,
    cap := data.table::fifelse(
      base_arable > arable_ha & base_arable > 0,
      arable_ha / base_arable,
      1
    )
  ]

  arable <- merge(arable, s_arable, by = c("area_code", "year"), all.x = TRUE)
  arable[, physical_ha := physical_ha * cap]

  arable_out <- vector("list", 0L)
  for (yr in sort(unique(arable$year))) {
    ay <- arable[year == yr]
    sy <- s_arable[year == yr]
    cropgrids_y <- ay[, .(
      area_code,
      item_cbs_code,
      physical_ha,
      harvested_ha = physical_ha
    )]
    fallow_y <- sy[, .(
      area_code,
      fallow_ha = pmax(arable_ha - base_arable * cap, 0)
    )]
    weights_y <- if (is.null(weights)) {
      # Default: distribute this year's fallow by cropped arable physical area.
      ay[, .(area_code, item_cbs_code, weight = physical_ha)]
    } else {
      weights_y <- merge(
        ay[, .(
          area_code,
          item_cbs_code,
          fallback_weight = physical_ha
        )],
        weights,
        by = c("area_code", "item_cbs_code"),
        all.x = TRUE
      )
      weights_y[,
        invalid_weight := any(
          !is.na(weight) & (!is.finite(weight) | weight < 0)
        ),
        by = area_code
      ]
      weights_y[is.na(weight), weight := 0]
      weights_y[,
        weight_sum := sum(weight),
        by = area_code
      ]
      weights_y[
        invalid_weight | !is.finite(weight_sum) | weight_sum <= 0,
        weight := fallback_weight
      ]
      weights_y[, .(area_code, item_cbs_code, weight)]
    }
    attributed <- attribute_fallow_to_crops(cropgrids_y, fallow_y, weights_y)
    attributed <- data.table::as.data.table(attributed)

    reconciliation <- attributed[,
      .(actual_arable = sum(physical_ha)),
      by = area_code
    ]
    reconciliation <- merge(
      sy[, .(area_code, target_arable = arable_ha)],
      reconciliation,
      by = "area_code",
      all.x = TRUE
    )
    reconciliation[,
      tolerance := pmax(1e-8, abs(target_arable) * 1e-10)
    ]
    failed <- reconciliation[
      is.na(actual_arable) |
        !is.finite(actual_arable) |
        abs(actual_arable - target_arable) > tolerance
    ]
    if (nrow(failed) > 0L) {
      failed_areas <- paste(failed$area_code, collapse = ", ")
      cli::cli_abort(
        "Arable totals do not reconcile for area codes: {.val {failed_areas}}."
      )
    }

    attributed[, year := yr]
    arable_out[[length(arable_out) + 1L]] <- attributed[, .(
      year,
      area_code,
      item_cbs_code,
      impact_u = physical_ha
    )]
  }
  arable_out <- data.table::rbindlist(arable_out)

  # --- perennial: scale to FAO Permanent crops, preserving within-group pattern.
  s_peren <- peren[, .(base_peren = sum(physical_ha)), by = .(area_code, year)]
  s_peren <- merge(
    s_peren,
    ap[, .(area_code, year, permanent_ha)],
    by = c("area_code", "year"),
    all.x = TRUE
  )
  peren <- merge(peren, s_peren, by = c("area_code", "year"), all.x = TRUE)
  peren[,
    impact_u := data.table::fifelse(
      !is.na(permanent_ha) & base_peren > 0,
      physical_ha * permanent_ha / base_peren,
      physical_ha
    )
  ]

  if (nrow(peren) > 0L) {
    perennial_reconciliation <- peren[,
      .(
        actual_permanent = sum(impact_u),
        target_permanent = permanent_ha[[1L]]
      ),
      by = .(area_code, year)
    ]
    perennial_reconciliation[,
      tolerance := pmax(1e-8, abs(target_permanent) * 1e-10)
    ]
    failed_perennial <- perennial_reconciliation[
      !is.na(target_permanent) &
        (!is.finite(actual_permanent) |
          abs(actual_permanent - target_permanent) > tolerance)
    ]
    if (nrow(failed_perennial) > 0L) {
      failed_areas <- paste(failed_perennial$area_code, collapse = ", ")
      cli::cli_abort(
        "Permanent-crop totals do not reconcile for area codes: {.val {failed_areas}}."
      )
    }
  }

  data.table::rbindlist(
    list(
      arable_out,
      peren[, .(year, area_code, item_cbs_code, impact_u)]
    )
  )
}
