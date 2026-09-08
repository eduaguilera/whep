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
#' - `area_code`: integer FAOSTAT area code, harmonised onto the
#'   `polity_area_code` bucket the rest of the pipeline aggregates on, at the
#'   fold state `options(whep.unfold_rest_of_world)` selects (the FAOSTAT
#'   "China" aggregate 351 is dropped). See [polity_area_crosswalk].
#' - `year`: integer.
#' - `arable_ha`, `permanent_ha`, `cropland_ha`: physical land area in hectares.
#' - `source`: provenance, `"fao"` (>= 1961) or `"luh2"` (pre-1961 backcast).
#'
#' Plus the polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
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
  tibble::as_tibble(out) |>
    .add_reporting_polity_columns()
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
  #
  # Read through `.polity_crosswalk()`, the one place `.unfold_rest_of_world()`
  # is applied (`R/polities.R`), so this bridge shares the fold state the rest
  # of the pipeline resolves through. The shipped `polity_area_crosswalk` still
  # puts the 61 Rest-of-World members promoted by whep#628 -- Syria 212,
  # Greenland 85, Bermuda 17, ... -- in bucket 999, and summing their land there
  # hides it from a production side keyed on their own code (whep#716).
  bridge <- .polity_crosswalk()[
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

# ISO3 -> area_code, deduplicated to one row per ISO3. Extracted so the LUH2
# national readers share one bridge instead of each growing their own: it is the
# only place the iso3c identity join lives.
#
# Read through `.polity_crosswalk()` for the same reason as
# `.fao_rl_to_polity()` above: on the shipped object the promoted Rest-of-World
# members still carry bucket 999, so the back-cast leg would key them
# differently from the FAO leg it is spliced onto (whep#716).
.luh2_bridge_iso3c <- function(dt) {
  bridge <- .polity_crosswalk()[
    !is.na(area_iso3c),
    .(iso3c = area_iso3c, area_code = as.integer(polity_area_code))
  ]
  bridge <- unique(bridge, by = "iso3c")
  merge(dt, bridge, by = "iso3c", sort = FALSE)
}

# National LUH2 area (ha) per (area_code, year) for an arbitrary set of states,
# from the same `luh2-areas` input the cropland back-cast reads.
.luh2_national_states <- function(states, luh2_data = NULL) {
  raw <- if (!is.null(luh2_data)) {
    luh2_data
  } else {
    .read_input("luh2-areas", years = NULL, year_col = "Year")
  }
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
  dt <- dt[Land_Use %in% states]
  dt <- .luh2_bridge_iso3c(dt)
  tibble::as_tibble(dt[,
    .(luh2_ha = sum(Area_Mha, na.rm = TRUE) * 1e6),
    by = .(area_code, year = as.integer(year))
  ])
}


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

  dt <- .luh2_bridge_iso3c(dt)
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
  ) |>
    .add_reporting_polity_columns()
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
#' perennial base area cannot be reconciled without inventing a crop
#' allocation; `unsupported_target` decides what happens to it (see the
#' unsupported-target section).
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
#' @section Netting basis, and the 2019/2020 seam:
#' Modelled CBS 3002 comes from EU AgriDB alone — FAOSTAT production item 996
#' is in neither production pin, and the EU AgriDB fodder source runs
#' 1961-2019 for all 28 of its region keys. So over 2001-2023 the netting term
#' exists for **26 EU polities and the years 2001-2019 only**, and is
#' identically zero everywhere else. For those 26 polities the arable target
#' therefore steps **from 96.4 Mha in 2019 to 103 Mha in 2020** while their own
#' FAO arable land *falls*, so ordinary arable crops there gain land with no
#' land-use change behind it. Measured over 2001-2023 on the real inputs, the
#' land the netting removes is 8.2-9.8 Mha a year to 2019 and **exactly 0** from
#' 2020. `temp_grassland_basis` exposes the alternatives measured in whep#937
#' and whep#354; `"modelled"` remains the default so this argument changes no
#' published number until a basis is chosen deliberately.
#' `temp_grassland_netted_ha` in the output, and
#' [check_arable_composition()], make the switch-off visible either way.
#'
#' FAO's own item 6633 "Temporary meadows and pastures" measures the same
#' concept, runs 2001-2023, and is what the `"fao_*"` bases read. It is not a
#' drop-in replacement: only ~19% of it is an official value, Greece and Poland
#' are imputed zeros throughout while WHEP models 2.10 and 4.78 Mha there, and
#' its scope is country-dependent — for Ireland, Sweden, the United Kingdom,
#' the Netherlands, Belgium, Luxembourg and Czechia it equals WHEP's CBS 3002
#' to the digit, while for Germany, Italy, Romania, Spain, Denmark, Austria and
#' Bulgaria it is 3-40 times larger and lands near the whole green-fodder
#' group. `validation/temp_grassland_6633.R` records that comparison.
#'
#' @section Fodder gap:
#' FAOSTAT's fodder tonnage (`faostat-production-old`, production only, no
#' harvested area at all) runs to 2013, whose rows `.combine_fodder()` drops,
#' so it effectively ends in 2012; EU AgriDB, the only other source, ends in
#' 2019. Fodder harvested area is reconstructed from those two, so a build
#' reaching 2020 has **no fodder at all** from that year: measured over
#' 2001-2023 on the real inputs, fodder is 9.2% of the reconciled arable land
#' extension in 2001 and 7.6% in 2019, then **0%** from 2020, with ordinary
#' arable crops absorbing the difference (whep#938). A second, earlier
#' composition change sits inside the covered window: from 2013 the
#' FAOSTAT-derived fodder area disappears and the dry-matter-yield estimate
#' jumps from 2.3 to 75.0 Mha, held flat to 2019.
#' `fodder_gap` exposes the treatments; `"as_reported"` remains the default.
#'
#' @section Unsupported land targets:
#' FAO reports positive land for some country-years in which the crop panel has
#' nothing of the matching kind to carry it, so the reconciliation has no crop
#' to attribute the land to. Over the full 1850-2023 span on the real default
#' inputs there are 923 such country-years (whep#1026):
#' - **arable**: 33 country-years, all Marshall Islands (`area_code` 127),
#'   1991-2023, 500 ha of FAO Arable land each (16,500 ha in total) with no
#'   arable crop row at all -- its only crop area is perennial.
#' - **permanent crops**: 890 country-years in 8 areas whose perennial base
#'   area is zero: Poland (173) 1850-1964, 27.72 Mha summed over years
#'   (up to 287,779 ha in a year); Nepal (149) 1850-1973, 2.40 Mha;
#'   Burkina Faso (233) 1850-1976, 1.35 Mha; Denmark (54) 1850-1984,
#'   1.10 Mha; Saint Kitts and Nevis (188) 1850-1984, 0.67 Mha;
#'   Sweden (210) 1850-1965, 0.39 Mha; Chad (39) 1850-1984, 0.35 Mha;
#'   Mongolia (141) 1983-1985, 3,000 ha. 33.98 Mha summed over all
#'   890 country-years.
#'
#' Before whep#1026 this was an error, which made the function's own documented
#' span unreachable and blocked [build_land_balance_footprint()] entirely.
#' `unsupported_target` now selects the treatment, and none of them invents a
#' crop allocation:
#' - `"unallocated"` (default) keeps the hectares in the ledger as one row per
#'   affected country-year with `item_cbs_code` `NA` -- real reported land that
#'   no crop can be named for. Nothing is lost and the gap is visible.
#' - `"zero"` treats the FAO total as the error: the unreconcilable target
#'   contributes no land, the rest of the country-year reconciles unchanged.
#' - `"drop"` removes the whole affected `(area_code, year)`, including the
#'   crop rows that *were* supported. For Poland that deletes 115 years of
#'   Polish arable land, so it is the most destructive option.
#' - `"abort"` is the pre-whep#1026 behaviour and refuses to continue.
#'
#' Only the `NA`-item rows separate `"unallocated"` from `"zero"`; every other
#' country-year is identical under all three continuing treatments. Restricted
#' to 2001-2023 the whole difference from `"abort"`'s (unreachable) output is
#' the 23 Marshall Islands rows, 11,500 ha.
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
#' @param temp_grassland_basis Which measurement of temporary grassland is
#'   netted out of the arable target. `"modelled"` (default) is the published
#'   behaviour: WHEP's own CBS 3002, which exists for 26 EU polities and stops
#'   in 2019. `"modelled_then_fao"` keeps that and fills every other
#'   country-year from official FAO 6633. `"fao_official"` uses official FAO
#'   6633 everywhere, `"fao_all"` uses FAO 6633 on every observation-status
#'   flag (~81% of it is FAO-imputed), and `"none"` nets nothing, the behaviour
#'   before whep#349. See the netting-basis section.
#' @param fodder_gap How the FAOSTAT fodder items (CBS `2000`-`2003`) are
#'   treated where their sources have run out. `"as_reported"` (default) is the
#'   published behaviour: no fodder from 2020, so ordinary crops absorb its
#'   land. `"carry_forward"` extends each fodder series' last observed physical
#'   area over the rest of that country's panel. `"drop"` removes fodder from
#'   the whole panel. See the fodder-gap section.
#' @param unsupported_target What happens to a country-year whose positive FAO
#'   land target has no crop row of the matching kind to carry it.
#'   `"unallocated"` (default) keeps the hectares as an `item_cbs_code` `NA`
#'   row, `"zero"` lets the unreconcilable target contribute no land, `"drop"`
#'   removes the whole affected country-year, and `"abort"` refuses to continue
#'   (the behaviour before whep#1026). See the unsupported-target section.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (fallow-inclusive physical land in hectares), `method_land`
#'   (`"fao_arable_fallow"`), `temp_grassland_netted_ha` (hectares netted out of
#'   that country-year's arable target, `0` where the netting term is
#'   structurally absent), `method_temp_grassland` (the `temp_grassland_basis`
#'   in force), `method_fodder` (the `fodder_gap` in force) and
#'   `method_unsupported_target` (the `unsupported_target` in force). Under
#'   `unsupported_target = "unallocated"` a row with `item_cbs_code` `NA` carries
#'   the FAO land no crop can be named for.
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
  items_prod_full = whep::items_prod_full,
  temp_grassland_basis = c(
    "modelled",
    "modelled_then_fao",
    "fao_official",
    "fao_all",
    "none"
  ),
  fodder_gap = c("as_reported", "carry_forward", "drop"),
  unsupported_target = c("unallocated", "zero", "drop", "abort")
) {
  temp_grassland_basis <- rlang::arg_match(temp_grassland_basis)
  fodder_gap <- rlang::arg_match(fodder_gap)
  unsupported_target <- rlang::arg_match(unsupported_target)
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
  ap <- .net_temporary_grassland(
    ap,
    temporary_grassland,
    basis = temp_grassland_basis
  )

  perennial_codes <- .item_cbs_perennial(items_prod_full)
  base[,
    kind := data.table::fifelse(
      item_cbs_code %in% perennial_codes,
      "perennial",
      "arable"
    )
  ]
  base <- .apply_fodder_gap(base, fodder_gap)

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

  out <- .reconcile_fao_arable_fallow(base, ap, weights, unsupported_target)
  out[, `:=`(
    method_land = "fao_arable_fallow",
    method_temp_grassland = temp_grassland_basis, # nolint: object_length_linter.
    method_fodder = fodder_gap,
    method_unsupported_target = unsupported_target # nolint: object_length_linter.
  )]
  out <- merge(
    out,
    ap[, .(area_code, year, temp_grassland_netted_ha)],
    by = c("area_code", "year"),
    all.x = TRUE
  )
  out[
    is.na(temp_grassland_netted_ha),
    temp_grassland_netted_ha := 0
  ]
  out <- out[impact_u > 0]
  data.table::setorder(out, year, area_code, item_cbs_code)
  out <- tibble::as_tibble(out) |>
    dplyr::relocate(
      "year",
      "area_code",
      "item_cbs_code",
      "impact_u",
      "method_land"
    )
  .warn_fodder_land_share(out, items_prod_full)
  .warn_arable_composition(out, items_prod_full)
  out
}
# nolint end

#' Check how much arable land the fallow split attributes to fodder crops.
#'
#' @description
#' Report, per `(year, area_code)`, the share of the reconciled arable land
#' extension that lands on the FAOSTAT fodder items (commodity-balance items of
#' `Cat_1 == "Fodder_green"` that are not grass, i.e. `2000`-`2003`).
#'
#' With `fallow_weights = NULL` — the default, and the path the land-balance
#' footprint ([build_land_balance_footprint()]) takes —
#' [build_fao_arable_fallow_extension()] rescales every arable crop's cropped
#' physical area proportionally up to FAO Arable land. Each crop's share of the
#' output is therefore exactly its share of the input harvested area, so an
#' inflated fodder harvested area is passed straight through into the published
#' per-crop arable land footprint, taking land away from the ordinary crops
#' (whep#356). Fodder harvested area is reconstructed rather than surveyed
#' (dry-matter yield imputation, EU AgriDB splicing, linear filling), so it is
#' the term most likely to be wrong, and in some country-years it alone exceeds
#' the country's whole FAO arable land.
#'
#' This is a diagnostic, not a correction: nothing is rescaled or dropped. It
#' flags where the attribution is implausible so the fodder reconstruction can
#' be inspected, or agro-climatic `fallow_weights` (see
#' [gridded_fallow_weights()]) supplied instead.
#'
#' @param extension Tibble of the arable/permanent land extension with columns
#'   `year`, `area_code`, `item_cbs_code` and `impact_u`, as returned by
#'   [build_fao_arable_fallow_extension()].
#' @param threshold Share of arable land above which a `(year, area_code)` is
#'   flagged (default `0.5`). Fodder is real land use, so a moderate share is
#'   expected; half a country's arable land is not.
#' @param items_prod_full Crosswalk used to classify `item_cbs_code` as
#'   perennial via `Herb_Woody`. Defaults to [items_prod_full].
#'
#' @return A tibble with one row per `(year, area_code)` that has at least one
#'   arable row in `extension`, ordered by descending `fodder_share`:
#'   - `year`, `area_code`: the country-year.
#'   - `fodder_ha`: arable land attributed to fodder items.
#'   - `arable_ha`: total arable (non-perennial) land attributed.
#'   - `fodder_share`: `fodder_ha / arable_ha` (`NA` when `arable_ha` is zero).
#'   - `flagged`: `TRUE` when `fodder_share > threshold`.
#'
#' @export
#'
#' @examples
#' extension <- tibble::tribble(
#'   ~year, ~area_code, ~item_cbs_code, ~impact_u,
#'   2000L, 10L, 2003L, 700, # fodder mix
#'   2000L, 10L, 2511L, 300 # wheat
#' )
#' check_fodder_land_share(extension)
check_fodder_land_share <- function(
  extension,
  threshold = 0.5,
  items_prod_full = whep::items_prod_full
) {
  .check_required_cols(
    extension,
    c("year", "area_code", "item_cbs_code", "impact_u"),
    "extension"
  )
  if (
    !rlang::is_scalar_double(threshold) && !rlang::is_scalar_integer(threshold)
  ) {
    cli::cli_abort("{.arg threshold} must be a single number.")
  }
  perennial_codes <- .item_cbs_perennial(items_prod_full)
  fodder_codes <- .item_cbs_fodder()

  extension |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code)
    ) |>
    # An NA item is unallocated FAO land, not a crop, so it belongs in neither
    # the fodder numerator nor the arable denominator (whep#1026).
    dplyr::filter(
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% perennial_codes
    ) |>
    dplyr::summarise(
      fodder_ha = sum(
        .data$impact_u[.data$item_cbs_code %in% fodder_codes],
        na.rm = TRUE
      ),
      arable_ha = sum(.data$impact_u, na.rm = TRUE),
      .by = c(year, area_code)
    ) |>
    dplyr::mutate(
      fodder_share = dplyr::if_else(
        .data$arable_ha > 0,
        .data$fodder_ha / .data$arable_ha,
        NA_real_
      ),
      flagged = !is.na(.data$fodder_share) & .data$fodder_share > threshold
    ) |>
    dplyr::arrange(dplyr::desc(.data$fodder_share))
}

#' Check where the arable-land extension changes composition mid-panel.
#'
#' @description
#' Report, per term and `area_code`, whether a term of the fallow-inclusive
#' arable land extension is present for part of a country's panel and absent
#' for the rest. Two terms switch off inside the published panel and neither
#' switch is a land-use change:
#'
#' - **`fodder`** — the FAOSTAT fodder items (`Cat_1 == "Fodder_green"` and not
#'   grass, i.e. CBS `2000`-`2003`). FAOSTAT's fodder tonnage
#'   (`faostat-production-old`, production only) ends in 2013 and EU AgriDB, the
#'   only other source, ends in 2019, so a build reaching 2020 has no fodder at
#'   all from that year on and every ordinary arable crop silently absorbs
#'   fodder's share of the country's arable land (whep#938).
#' - **`temp_grassland_netting`** — the temporary grassland netted out of the
#'   arable target. Modelled CBS 3002 comes from EU AgriDB alone, so it exists
#'   for 26 EU polities and stops at 2019; from 2020 the netting term is
#'   identically zero while FAO's arable land still contains temporary meadows,
#'   and the reconciliation changes method at the boundary (whep#937).
#'
#' This is a diagnostic, not a correction: it flags the discontinuity so a
#' series is not read across it. The treatments live behind
#' [build_fao_arable_fallow_extension()]'s `fodder_gap` and
#' `temp_grassland_basis` arguments, whose defaults reproduce the published
#' behaviour.
#'
#' @details
#' [check_series_jumps()] cannot find either break. A term does not fall to a
#' small value at the boundary, it stops having rows, and its `min_value`
#' guard skips any pair involving a zero, so a scan over the completed series
#' would not flag it either. Coverage, not a ratio, is what has to be checked.
#'
#' @param extension Tibble of the arable/permanent land extension as returned
#'   by [build_fao_arable_fallow_extension()]: `year`, `area_code`,
#'   `item_cbs_code`, `impact_u`, and optionally `temp_grassland_netted_ha`.
#'   The `temp_grassland_netting` term is reported only when that column is
#'   present.
#' @param items_prod_full Crosswalk used to classify `item_cbs_code` as
#'   perennial via `Herb_Woody`. Defaults to [items_prod_full].
#'
#' @return A tibble with one row per `(term, area_code)`:
#'   - `term`: `"fodder"` or `"temp_grassland_netting"`.
#'   - `area_code`: the country.
#'   - `panel_first_year`, `panel_last_year`: the years that country has arable
#'     rows for.
#'   - `term_first_year`, `term_last_year`: the years the term is positive
#'     (`NA` when it never is).
#'   - `n_years_absent`: panel years in which the term is absent.
#'   - `break_year`: the first panel year after `term_last_year` with no term
#'     (`NA` when the term runs to the end of the panel, or never appears).
#'   - `broken`: `TRUE` when the term is present in some panel year and absent
#'     in a later one.
#'   - `never_present`: `TRUE` when the term is absent for the whole panel.
#'
#' @export
#'
#' @examples
#' extension <- tibble::tribble(
#'   ~year, ~area_code, ~item_cbs_code, ~impact_u, ~temp_grassland_netted_ha,
#'   2019L, 10L, 2003L, 100, 50, # fodder mix, netting fires
#'   2019L, 10L, 2511L, 900, 50,
#'   2020L, 10L, 2511L, 1000, 0 # fodder gone, netting off
#' )
#' check_arable_composition(extension)
check_arable_composition <- function(
  extension,
  items_prod_full = whep::items_prod_full
) {
  .check_required_cols(
    extension,
    c("year", "area_code", "item_cbs_code", "impact_u"),
    "extension"
  )
  perennial_codes <- .item_cbs_perennial(items_prod_full)
  arable <- extension |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code)
    ) |>
    # An NA item is unallocated FAO land, not a crop (whep#1026).
    dplyr::filter(
      !is.na(.data$item_cbs_code),
      !.data$item_cbs_code %in% perennial_codes
    )

  panel <- arable |>
    dplyr::distinct(.data$year, .data$area_code)
  if (nrow(panel) == 0L) {
    return(.arable_composition_proto())
  }

  terms <- list(
    fodder = arable |>
      dplyr::filter(.data$item_cbs_code %in% .item_cbs_fodder()) |>
      dplyr::summarise(term_ha = sum(.data$impact_u), .by = c(year, area_code))
  )
  if (rlang::has_name(arable, "temp_grassland_netted_ha")) {
    terms$temp_grassland_netting <- arable |>
      dplyr::summarise(
        term_ha = max(.data$temp_grassland_netted_ha),
        .by = c(year, area_code)
      )
  }

  purrr::imap(terms, \(present, nm) .term_coverage(panel, present, nm)) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$term, .data$area_code)
}

# Zero-row prototype of the report, for an extension with no arable rows at all
# (every arable target clamped to 0, so nothing survives the positivity filter).
.arable_composition_proto <- function() {
  tibble::tibble(
    term = character(0),
    area_code = integer(0),
    panel_first_year = integer(0),
    panel_last_year = integer(0),
    term_first_year = integer(0),
    term_last_year = integer(0),
    n_years_absent = integer(0),
    break_year = integer(0),
    broken = logical(0),
    never_present = logical(0)
  )
}

# Coverage of one term over each area's own arable panel. `present` carries the
# term's hectares per country-year; a country-year missing from it, or carrying
# a non-positive value, counts as absent.
.term_coverage <- function(panel, present, term) {
  panel |>
    dplyr::left_join(
      dplyr::filter(present, .data$term_ha > 0),
      by = c("year", "area_code")
    ) |>
    dplyr::summarise(
      panel_first_year = min(.data$year),
      panel_last_year = max(.data$year),
      term_first_year = .min_or_na(.data$year[!is.na(.data$term_ha)]),
      term_last_year = .max_or_na(.data$year[!is.na(.data$term_ha)]),
      n_years_absent = sum(is.na(.data$term_ha)),
      .by = area_code
    ) |>
    dplyr::mutate(
      term = term,
      never_present = is.na(.data$term_last_year),
      broken = !.data$never_present &
        .data$term_last_year < .data$panel_last_year,
      break_year = dplyr::if_else(
        .data$broken,
        .data$term_last_year + 1L,
        NA_integer_
      )
    ) |>
    dplyr::select(
      "term",
      "area_code",
      "panel_first_year",
      "panel_last_year",
      "term_first_year",
      "term_last_year",
      "n_years_absent",
      "break_year",
      "broken",
      "never_present"
    )
}

.min_or_na <- function(x) {
  if (length(x) == 0L) NA_integer_ else min(x)
}

.max_or_na <- function(x) {
  if (length(x) == 0L) NA_integer_ else max(x)
}

# One aggregated warning per broken term. Warn-only: the numbers are unchanged,
# but a term that switches off inside the panel is no longer silent
# (whep#937, whep#938).
.warn_arable_composition <- function(out, items_prod_full) {
  report <- check_arable_composition(out, items_prod_full = items_prod_full)
  broken <- report[report$broken, ]
  if (nrow(broken) == 0L) {
    return(invisible(NULL))
  }
  for (nm in unique(broken$term)) {
    rows <- broken[broken$term == nm, ]
    first_break <- min(rows$break_year)
    arg <- if (identical(nm, "fodder")) "fodder_gap" else "temp_grassland_basis"
    absent <- sum(report$term == nm & report$never_present)
    cli::cli_warn(c(
      "!" = "The {.field {nm}} term of the arable land extension stops inside
        the panel for {nrow(rows)} {cli::qty(nrow(rows))}area{?s}, the earliest
        from {.val {first_break}}.",
      "*" = "From then on the land it carried is absorbed by the other arable
        crops, with no land-use change behind it.",
      "*" = "A further {absent} {cli::qty(absent)}area{?s} never carr{?ies/y}
        the term at all.",
      "i" = "See {.fun check_arable_composition}; the treatments are
        {.arg {arg}} in {.fun build_fao_arable_fallow_extension}."
    ))
  }
  invisible(NULL)
}

# Integer item_cbs_code values of the FAOSTAT fodder items that the crop land
# extension treats as ordinary arable crops. Temporary grassland (3002) is
# `Cat_1 == "Fodder_green"` too but is grass, reported by the grassland
# extension and netted out of the arable target, so it is excluded here.
.item_cbs_fodder <- function() {
  whep::items_full |>
    dplyr::filter(
      .data$Cat_1 == "Fodder_green",
      .data$group != "Grass"
    ) |>
    dplyr::pull(.data$item_cbs_code) |>
    unique() |>
    as.integer()
}

# One aggregated warning for the country-years where fodder crops take an
# implausible share of the arable land the fallow split distributes. Warn-only:
# the numbers are unchanged, but the skew is no longer silent (whep#356).
.warn_fodder_land_share <- function(out, items_prod_full) {
  report <- check_fodder_land_share(out, items_prod_full = items_prod_full)
  flagged <- report[report$flagged, ]
  if (nrow(flagged) == 0L) {
    return(invisible(NULL))
  }
  worst <- flagged |>
    dplyr::summarise(
      share = max(.data$fodder_share),
      .by = area_code
    ) |>
    dplyr::arrange(dplyr::desc(.data$share)) |>
    utils::head(5)
  areas <- worst$area_code
  shares <- paste0(round(100 * worst$share, 1), "%")
  cli::cli_warn(c(
    "!" = "Fodder crops take over half the arable land in
      {nrow(flagged)} country-year{?s} of the fallow-inclusive extension.",
    "*" = "Worst {.field area_code}: {.val {areas}} (up to {.val {shares}}).",
    "i" = "Fodder harvested area is reconstructed, not surveyed; see
      {.fun check_fodder_land_share} and whep#356. Supplying
      {.arg fallow_weights} from {.fun gridded_fallow_weights} makes the
      fallow split independent of it."
  ))
}

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
# `temp_grassland_netted_ha` is always added, zero included, so a country-year
# where the netting term is structurally absent says so in the output (whep#937).
.net_temporary_grassland <- function(ap, temporary_grassland, basis) {
  temp <- .temporary_grassland_ha(
    temporary_grassland,
    basis = basis,
    years = sort(unique(ap$year))
  )
  if (nrow(temp) == 0L) {
    ap[, temp_grassland_netted_ha := 0]
    return(ap[])
  }
  ap <- merge(ap, temp, by = c("area_code", "year"), all.x = TRUE)
  ap[is.na(temp_grassland_ha), temp_grassland_ha := 0]
  ap[, temp_grassland_netted_ha := pmin(temp_grassland_ha, arable_ha)]
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

# Temporary grassland hectares per (area_code, year) under one netting basis.
# `"none"` nets nothing; the two `"fao_*"` bases read FAOSTAT RL item 6633
# instead of the modelled series; `"modelled_then_fao"` keeps the modelled value
# where WHEP has one and fills the rest from official 6633. See the
# netting-basis section of build_fao_arable_fallow_extension().
.temporary_grassland_ha <- function(
  temporary_grassland,
  basis = "modelled",
  years = NULL
) {
  if (identical(basis, "none")) {
    return(data.table::data.table(
      area_code = integer(0),
      year = integer(0),
      temp_grassland_ha = numeric(0)
    ))
  }
  if (basis %in% c("fao_official", "fao_all")) {
    return(.fao_temp_meadows_ha(
      official_only = identical(basis, "fao_official"),
      years = years
    ))
  }
  modelled <- .modelled_temp_grassland_ha(temporary_grassland)
  if (identical(basis, "modelled")) {
    return(modelled)
  }
  # "modelled_then_fao": the modelled reconstruction wherever it exists, FAO's
  # own official measurement of the same concept everywhere else. Only official
  # (flag "A") 6633 rows fill the gap -- ~81% of item 6633 is FAO-imputed and
  # Greece and Poland are imputed zeros throughout, so filling from every flag
  # would net FAO's gap-filling (see whep#354 and validation/temp_grassland_6633.R).
  fao <- .fao_temp_meadows_ha(official_only = TRUE, years = years)
  data.table::rbindlist(list(
    modelled,
    fao[!modelled, on = c("area_code", "year")]
  ))
}

# The modelled CBS 3002 side. NULL builds the grassland occupation extension so
# netting is applied by default (correct but slow); a supplied table (grassland
# extension schema: area_code, year, item_cbs_code, impact_u) is reused as-is,
# from which CBS 3002 is kept. Pass a table with no CBS 3002 rows to opt out.
.modelled_temp_grassland_ha <- function(temporary_grassland) {
  if (is.null(temporary_grassland)) {
    # The LUH2 source is pinned explicitly here: it is the grassland
    # extension's own default, but that default disagrees with the
    # whole-territory land-use ledger, which anchors grassland on the
    # FAOSTAT pasture statistic instead (whep#759). Spelling it out here
    # means a future default change upstream cannot silently move this
    # netting.
    temporary_grassland <- build_grassland_land_extension(
      source = "luh2",
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

# FAOSTAT RL item 6633 "Temporary meadows and pastures" in hectares per
# (area_code, year), on the same polity key as the arable/permanent base. The
# observation-status flag decides what counts: "A" is an official value, and
# FAO's own standard says an imputed zero is flagged "I", not "N", so an
# unfiltered read nets FAO's gap-filling rather than a measurement. The flag
# semantics and the standard they come from are quoted in full at the top of
# `validation/temp_grassland_6633.R`, which is where they were verified; this
# reader only applies them. The item only starts
# in 2001, so no earlier year gets a term from this source.
.fao_temp_meadows_ha <- function(official_only, years = NULL) {
  raw <- .fetch_fao_rl()
  dt <- data.table::as.data.table(raw)
  ren <- c(
    "Area Code" = "area_code_fao",
    "Item Code" = "item_code",
    "Element" = "element",
    "Unit" = "unit",
    "Year" = "year",
    "Value" = "value",
    "Flag" = "flag"
  )
  for (from in names(ren)) {
    if (from %in% names(dt) && !ren[[from]] %in% names(dt)) {
      data.table::setnames(dt, from, ren[[from]])
    }
  }
  missing <- setdiff(unname(ren), names(dt))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "The FAOSTAT RL land-use source is missing column{?s}
        {.field {missing}}.",
      "i" = "FAO item 6633 is needed by
        {.arg temp_grassland_basis = {.val fao_official}}; supply
        {.arg temporary_grassland} instead, or use
        {.arg temp_grassland_basis = {.val modelled}}."
    ))
  }
  dt <- dt[
    as.integer(item_code) == 6633L &
      element == "Area" &
      unit %in% c("1000 ha", "1000 Ha", "1000ha")
  ]
  if (isTRUE(official_only)) {
    dt <- dt[!is.na(flag) & flag == "A"]
  }
  dt <- dt[, .(
    area_code_fao = as.integer(area_code_fao),
    year = as.integer(year),
    ha = as.numeric(value) * 1000
  )]
  dt <- dt[!is.na(area_code_fao) & !is.na(year) & !is.na(ha)]
  if (!is.null(years)) {
    dt <- dt[year %in% as.integer(years)]
  }
  dt <- merge(dt, .fao_rl_area_bridge(), by = "area_code_fao")
  dt[,
    .(temp_grassland_ha = sum(ha)),
    by = .(area_code = polity, year)
  ]
}

# FAOSTAT area code -> whep polity area_code, read through `.polity_crosswalk()`
# so this bridge shares the fold state the rest of the pipeline resolves
# through. FAOSTAT aggregates with no ISO3 (notably "China" 351, which overlaps
# 41/96/128/214) have no crosswalk row and are dropped.
.fao_rl_area_bridge <- function() {
  bridge <- .polity_crosswalk()[
    !is.na(area_iso3c),
    .(
      area_code_fao = as.integer(area_code),
      polity = as.integer(polity_area_code)
    )
  ]
  unique(bridge, by = "area_code_fao")
}

# Fodder treatment for the arable reconciliation (whep#938). "as_reported"
# leaves the base untouched; "drop" removes the fodder items from the whole
# panel; "carry_forward" is `.carry_fodder_forward()` below.
.apply_fodder_gap <- function(base, treatment) {
  if (identical(treatment, "as_reported")) {
    return(base)
  }
  fodder_codes <- .item_cbs_fodder()
  if (identical(treatment, "drop")) {
    return(base[!(kind == "arable" & item_cbs_code %in% fodder_codes)])
  }
  .carry_fodder_forward(base, fodder_codes)
}

# Hold each fodder series' last observed physical area over the panel years
# that follow it, with `fill_linear()`'s carry-forward -- the same instrument
# the fodder reconstruction itself already uses upstream in
# `.fill_fodder_gaps()`, so a filled year here means what a filled year there
# means. `interpolate = FALSE` because an interior hole is not what whep#938
# is about, and `fill_backward = FALSE` so no fodder is invented before a
# series starts. The year axis is the global panel restricted to each area's
# own arable years, so a country that has left the panel gains nothing.
.carry_fodder_forward <- function(base, fodder_codes) {
  arable <- base[kind == "arable"]
  fodder <- arable[item_cbs_code %in% fodder_codes]
  if (nrow(fodder) == 0L) {
    return(base)
  }
  grid <- .cross_join(
    unique(arable[, .(year)]),
    unique(fodder[, .(area_code, item_cbs_code)])
  )
  grid <- merge(
    grid,
    unique(arable[, .(year, area_code)]),
    by = c("year", "area_code")
  )
  grid <- merge(
    grid,
    fodder[, .(year, area_code, item_cbs_code, physical_ha)],
    by = c("year", "area_code", "item_cbs_code"),
    all.x = TRUE
  )
  filled <- fill_linear(
    grid,
    physical_ha,
    time_col = year,
    interpolate = FALSE,
    fill_backward = FALSE,
    .by = c("area_code", "item_cbs_code"),
    .copy = FALSE
  )
  filled <- data.table::as.data.table(filled)[!is.na(physical_ha)]
  filled[, kind := "arable"]
  data.table::rbindlist(
    list(
      base[!(kind == "arable" & item_cbs_code %in% fodder_codes)],
      filled[, .(year, area_code, item_cbs_code, physical_ha, kind)]
    ),
    use.names = TRUE
  )
}

# Per (area_code, year): add rotational fallow to arable crops up to FAO Arable
# land (scaling the cropped physical down instead when it already exceeds it, the
# physical-container correction), and scale perennial crops to FAO Permanent
# crops. The additive fallow distribution reuses attribute_fallow_to_crops().
.reconcile_fao_arable_fallow <- function(
  base,
  ap,
  weights,
  unsupported_target = "abort"
) {
  # A positive target cannot be manufactured when the corresponding crop kind
  # has no row (or, for proportional perennial scaling, has zero base area).
  # `unsupported_target` decides what happens there (whep#1026).
  resolved <- .resolve_unsupported_target(base, ap, unsupported_target)
  base <- resolved$base
  ap <- resolved$ap
  arable <- base[kind == "arable"]
  peren <- base[kind == "perennial"]

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
      peren[, .(year, area_code, item_cbs_code, impact_u)],
      resolved$unallocated
    ),
    use.names = TRUE
  )
}

# Country-years whose positive FAO land target has no crop row of the matching
# kind to carry it, one row per (area_code, year, kind) with the hectares at
# stake. `support` is keyed on the crop panel, so a country-year with no crop
# rows at all never reaches the reconciliation and is not reported here.
.unsupported_targets <- function(base, ap) {
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
  data.table::rbindlist(list(
    support[
      !is.na(arable_ha) & arable_ha > 0 & arable_rows == 0L,
      .(area_code, year, kind = "arable", ha = arable_ha)
    ],
    support[
      !is.na(permanent_ha) & permanent_ha > 0 & perennial_base <= 0,
      .(area_code, year, kind = "perennial", ha = permanent_ha)
    ]
  ))
}

# Apply the chosen `unsupported_target` treatment, returning the crop panel and
# the FAO targets to reconcile plus the unallocated rows to append. No treatment
# invents a crop allocation: the land is either carried on an NA item, left out
# of the reconciled total, or removed with its country-year.
.resolve_unsupported_target <- function(base, ap, treatment) {
  unsupported <- .unsupported_targets(base, ap)
  if (nrow(unsupported) == 0L) {
    return(list(base = base, ap = ap, unallocated = NULL))
  }
  if (identical(treatment, "abort")) {
    .abort_unsupported_target(unsupported)
  }
  .warn_unsupported_target(unsupported, treatment)
  if (identical(treatment, "drop")) {
    keys <- unique(unsupported[, .(area_code, year)])
    return(list(
      base = base[!keys, on = c("area_code", "year")],
      ap = ap[!keys, on = c("area_code", "year")],
      unallocated = NULL
    ))
  }
  # "zero" and "unallocated" both take the unreconcilable target out of the
  # reconciliation; only "unallocated" keeps its hectares in the ledger.
  ap[
    unsupported[kind == "arable", .(area_code, year)],
    arable_ha := 0,
    on = c("area_code", "year")
  ]
  ap[
    unsupported[kind == "perennial", .(area_code, year)],
    permanent_ha := 0,
    on = c("area_code", "year")
  ]
  unallocated <- NULL
  if (identical(treatment, "unallocated")) {
    unallocated <- unsupported[,
      .(item_cbs_code = NA_integer_, impact_u = sum(ha)),
      by = .(year, area_code)
    ]
  }
  list(base = base, ap = ap, unallocated = unallocated)
}

# The pre-whep#1026 behaviour, kept selectable. The arable message fires first
# when both kinds are unsupported, as it did before the treatments existed.
.abort_unsupported_target <- function(unsupported) {
  arable <- unsupported[kind == "arable"]
  hint <- paste(
    "Set {.arg unsupported_target} to {.val unallocated}, {.val zero} or",
    "{.val drop} to continue instead."
  )
  if (nrow(arable) > 0L) {
    keys <- .unsupported_keys(arable)
    cli::cli_abort(c(
      "Cannot reconcile positive arable totals without arable crop rows: {.val {keys}}.",
      i = hint
    ))
  }
  keys <- .unsupported_keys(unsupported)
  cli::cli_abort(c(
    "Cannot reconcile positive permanent-crop totals without positive perennial base area: {.val {keys}}.",
    i = hint
  ))
}

.unsupported_keys <- function(unsupported) {
  paste(
    paste(unsupported$area_code, unsupported$year, sep = "/"),
    collapse = ", "
  )
}

# One aggregated warning naming the hectares at stake, so a continuing
# treatment is never a silent fallback.
.warn_unsupported_target <- function(unsupported, treatment) {
  n <- nrow(unsupported)
  by_kind <- unsupported[, .(n = .N, ha = round(sum(ha))), by = kind]
  detail <- paste0(by_kind$kind, " ", by_kind$n, " (", by_kind$ha, " ha)")
  worst <- unsupported[, .(ha = sum(ha)), by = area_code]
  data.table::setorder(worst, -ha)
  areas <- utils::head(worst$area_code, 5L)
  cli::cli_warn(c(
    "!" = "No crop row can carry the positive FAO land target in
      {cli::qty(n)}{n} country-year{?s}.",
    "*" = "Country-years by kind: {detail}.",
    "*" = "Treated as {.val {treatment}}; largest {.field area_code}
      {.val {areas}}.",
    "i" = "{.arg unsupported_target} {.val abort} refuses to continue instead;
      see {.fun build_fao_arable_fallow_extension}."
  ))
}
