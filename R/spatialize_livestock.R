#' Build gridded livestock dataset
#'
#' @description
#' Disaggregate country-level FAOSTAT livestock stocks and emissions to a
#' 0.5-degree grid. Each species group uses a tailored spatial proxy:
#'
#' - **Ruminants** (cattle, buffalo, sheep/goats, equines): LUH2
#'   managed pasture (`pastr`) plus rangeland (`range`), optionally
#'   weighted by a static manure-intensity reference (West et al. 2014).
#' - **Confined animals** (pigs, poultry): LUH2 aggregate cropland,
#'   reflecting intensive farming co-location with crop production.
#' - **Range specialists** (camels): LUH2 rangeland only.
#' - **Mixed** (other animals): 50/50 blend of pasture and cropland.
#'
#' For each country, year, and species group the function distributes the
#' national total proportionally to cell-level proxy weights:
#'
#' \deqn{\text{cell} = \frac{w_i}{\sum_{j \in \text{country}} w_j}
#'       \times T_{\text{country}}}
#'
#' where \eqn{w_i} is the proxy weight in cell \eqn{i} (land-use
#' hectares times optional reference-pattern intensity) and \eqn{T} is
#' the country total (heads or emissions).
#'
#' ## Methodology
#'
#' Livestock spatialization is not covered by LandInG (Ostberg et al.
#' 2023), which focuses on crops only. The approach here extends the
#' LandInG framework by using the same LUH2-based spatial proxies
#' (pasture, rangeland, cropland) for livestock distribution.
#'
#' Country-level data comes from [build_primary_production()] (stocks)
#' and the `faostat-emissions-livestock` pin (CH4/N2O emissions), with
#' predecessor redistribution and pre-1961 backfill already applied.
#'
#' The Zenodo livestock density input (Heinke 2025,
#' doi:10.5281/zenodo.14946695) provides an alternative calibrated
#' LSU/ha reference for use with the `glw_density` parameter.
#'
#' ## Data sources and references
#'
#' | Source | Use |
#' |--------|-----|
#' | FAOSTAT Production_Livestock (FAO 2024) | Country-level heads |
#' | FAOSTAT Emissions_livestock (FAO 2024) | Enteric CH4, manure CH4/N2O |
#' | LUH2 v2h (Hurtt et al. 2020) | Time-varying pasture + cropland |
#' | West et al. (2014) | Static manure-N intensity reference |
#' | GLW3 (Gilbert et al. 2018) | Species-specific density (optional) |
#' | Heinke (2025) | Calibrated LSU/ha density (optional) |
#' | IPCC 2006/2019 | N-excretion rates, emission factors |
#'
#' @param livestock_data A tibble with country-level livestock data.
#'   Required columns:
#'   - `year`: Integer year.
#'   - `area_code`: Country code (WHEP polities).
#'   - `species_group`: Livestock functional-type name (e.g.
#'     `"cattle"`, `"pigs"`, `"poultry"`).
#'   - `heads`: Live animal count (number of head).
#'   Any additional numeric columns (e.g. `enteric_ch4_kt`,
#'   `manure_ch4_kt`, `manure_n2o_kt`, `manure_n_mg`) are
#'   distributed to the grid using the same proportional weights
#'   as `heads`.
#' @param gridded_pasture A tibble with annual gridded pasture extent.
#'   Required columns:
#'   - `lon`, `lat`: Cell centre coordinates (0.5 degree).
#'   - `year`: Integer year.
#'   - `pasture_ha`: Managed pasture area in hectares (LUH2 `pastr`).
#'   - `rangeland_ha`: Rangeland area in hectares (LUH2 `range`).
#' @param gridded_cropland A tibble with annual gridded cropland extent.
#'   Required columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `year`: Integer year.
#'   - `cropland_ha`: Total cropland area in hectares.
#' @param country_grid A tibble mapping grid cells to countries.
#'   Required columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `area_code`: Country code.
#'   - `cell_area_frac` (or `polity_frac`, `area_frac`, `country_frac`):
#'     This polity compartment's share of the physical cell, a partition
#'     summing to 1 over the polities that overlap the cell. Required: a
#'     grid carrying no share is refused, because defaulting it to 1 gives
#'     a border cell wholly to one polity. Pass 1 only where the polity
#'     does own the whole cell. A land fraction (`landfrac`) is a
#'     different quantity and is refused rather than reinterpreted.
#'   Optional columns:
#'   - `polycell_id`, `cell_id`: Stable compartment/cell identifiers
#'     preserved in outputs when present.
#'   - `year` or validity intervals (`valid_from`/`valid_to`,
#'     `start_year`/`end_year`, `from_year`/`to_year`) for historical,
#'     time-varying polity overlays. The start bound is inclusive; the end
#'     bound is **exclusive at a succession** and **inclusive at the open
#'     end**, so 2014 selects `"RUS-2014-2025"` and not `"RUS-1991-2014"`,
#'     while 2025 still selects `"RUS-2014-2025"` because no later interval of
#'     that compartment follows it. See [polities] for the full rule.
#' @param species_proxy A tibble mapping each `species_group` to its
#'   spatial proxy type: `"pasture"`, `"cropland"`, `"rangeland"`, or
#'   `"mixed"`.
#'   Required columns:
#'   - `species_group`: Group name (must match `livestock_data`).
#'   - `spatial_proxy`: One of `"pasture"`, `"cropland"`,
#'     `"rangeland"`, or `"mixed"`.
#'   If `NULL`, a default mapping is used (see Details).
#' @param manure_pattern A tibble with static manure-intensity
#'   weights (e.g. from West et al. 2014). Optional.
#'   Expected columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `manure_intensity`: Relative intensity (kg N per ha or
#'     similar). Values are used multiplicatively with the
#'     land-use proxy.
#'   If `NULL`, land-use weights are used alone.
#' @param glw_density A tibble with species-specific gridded
#'   livestock density from GLW3 (Gilbert et al. 2018). Required when
#'   `proxy_method = "glw3"`; ignored, with a warning, under
#'   `proxy_method = "luh2"`.
#'   Expected columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `species_group`: Must match `livestock_data`.
#'   - `density`: Heads per cell (reference year ~2010).
#'   Under `"glw3"` it **replaces** the LUH2 proxy for every group, still
#'   masked by that year's LUH2 extent so a cell whose land use has gone
#'   receives nothing.
#' @param grass_productivity A tibble with grass productivity per cell
#'   (`lon`, `lat`, `grass_npp`) from [read_lpjml_grass_productivity()].
#'   Optional. When provided, it multiplies the `pasture`/`rangeland`
#'   (grazer) proxy weights so animals follow grass production rather than
#'   area alone; cropland/mixed proxies are unaffected. If `NULL`, area
#'   proxies are used alone.
#' @param years Integer vector of years to spatialize. If `NULL`
#'   (default), all years present in `livestock_data` are processed.
#'   When supplied, `livestock_data`, `gridded_pasture`, and
#'   `gridded_cropland` are filtered to this set before processing.
#' @param proxy_method Which spatial proxy carries the within-country
#'   weight: `"luh2"` (default) or `"glw3"`, validated with
#'   [rlang::arg_match()]. They are alternatives, never fallbacks: under
#'   `"glw3"` a `NULL` `glw_density`, or a `species_group` that table has
#'   no positive cell for, aborts instead of quietly reverting to the LUH2
#'   proxy. The resolved value is recorded per row in
#'   `method_livestock_proxy`. See *Which livestock proxy the weights come
#'   from*.
#' @param area_key Which area code the output is keyed on: `"grid"`
#'   (default, the reporting codes `livestock_data` and `country_grid` are
#'   keyed on) or `"polity_area"` (the [polity_area_crosswalk] bucket
#'   national tables are aggregated on). See
#'   [build_gridded_landuse()]'s *Which area code the output is keyed on*.
#'
#' @return A tibble with gridded livestock data. Columns:
#'   - `lon`, `lat`: Cell centre coordinates.
#'   - `area_code`: WHEP polity code for this cell compartment.
#'   - `polity_area_code`, `reporting_polity_code`,
#'     `reporting_polity_name`, `reporting_polity_has_geometry`: Polity
#'     metadata for `area_code`.
#'   - `grid_area_code`: Only under `area_key = "polity_area"`; the
#'     reporting code the engine allocated on.
#'   - `polycell_id`, `cell_id`: Preserved when supplied in
#'     `country_grid`.
#'   - `year`: Integer year.
#'   - `species_group`: Livestock functional type.
#'   - `heads`: Allocated live animal count.
#'   - Any additional numeric columns from `livestock_data`
#'     (e.g. `enteric_ch4_kt`, `manure_ch4_kt`).
#'   - `method_livestock_proxy`: Which proxy produced the weights for
#'     this row, one of `"luh2_area"`, `"luh2_grass"`, `"glw3"`.
#'     Constant within a `(year, species_group)` block.
#'
#' @inheritSection build_gridded_landuse Which area code the output is keyed on
#'
#' @section Which livestock proxy the weights come from:
#' `proxy_method` selects the within-country weight, and every output row
#' records the resolved value in `method_livestock_proxy`:
#'
#' - `"luh2_area"`: LUH2 extent alone (`proxy_method = "luh2"`).
#' - `"luh2_grass"`: LUH2 extent times grass NPP (`proxy_method = "luh2"`
#'   with `grass_productivity` supplied). The grass weighting reaches the
#'   `pasture` and `rangeland` proxies only, so `cropland` and `mixed`
#'   groups in the same call stay `"luh2_area"`. The label is per species
#'   group, not per cell: a grazer cell with no `grass_npp` keeps its area
#'   weight but still travels under `"luh2_grass"`, because what the
#'   column records is the weighting regime the group ran under.
#' - `"glw3"`: GLW3 density masked by that year's LUH2 extent
#'   (`proxy_method = "glw3"`).
#'
#' The default stays `"luh2"` even though `"glw3"` is the better-informed
#' proxy. WHEP has no data mechanism for GLW3 yet -- no download script,
#' env var, pin or reader (whep#1000, task T15a-ii) -- so a `"glw3"`
#' default would abort every production run. This is a deliberate,
#' documented deviation from "the default is the most rigorous available
#' method", of the same shape as the interim `area_key = "grid"` default
#' in `R/spatialize_compartments.R`; whep#1000 task T20 is the gate that
#' revisits it.
#'
#' @section Species groups must be mapped, not guessed:
#' Every `species_group` in `livestock_data` must have a row in
#' `species_proxy`; an unmapped group aborts naming it. It used to fall
#' back to the `"pasture"` proxy silently, so a typo or a new FAOSTAT item
#' was given a grazing distribution with no trace in the output.
#'
#' The catch-all is explicit, not implicit.
#' `inst/extdata/livestock_mapping.csv` maps FAOSTAT items 1140 and 1150
#' (rabbits and hares, other rodents) and 1171 (live animals nes) onto the
#' group `"other"` with the `cropland` and `mixed` proxies, so a catch-all
#' group is reached by an explicit item mapping upstream and never by
#' name-matching here. A group carrying several proxies keeps the first,
#' as before.
#'
#' @export
#'
#' @examples
#' # Minimal example with toy data
#' livestock_data <- tibble::tribble(
#'   ~year, ~area_code, ~species_group, ~heads,
#'   2000L,         1L,       "cattle",   5000
#' )
#' gridded_pasture <- tibble::tribble(
#'   ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
#'    0.25, 50.25, 2000L,         600,           200,
#'    0.75, 50.25, 2000L,         400,           100
#' )
#' gridded_cropland <- tibble::tribble(
#'   ~lon,  ~lat,  ~year, ~cropland_ha,
#'    0.25, 50.25, 2000L,          800,
#'    0.75, 50.25, 2000L,          500
#' )
#' country_grid <- tibble::tribble(
#'   ~lon,  ~lat, ~area_code, ~cell_area_frac,
#'    0.25, 50.25,         1L,               1,
#'    0.75, 50.25,         1L,               1
#' )
#' build_gridded_livestock(
#'   livestock_data, gridded_pasture, gridded_cropland, country_grid
#' )
build_gridded_livestock <- function(
  livestock_data,
  gridded_pasture,
  gridded_cropland,
  country_grid,
  species_proxy = NULL,
  manure_pattern = NULL,
  glw_density = NULL,
  grass_productivity = NULL,
  years = NULL,
  proxy_method = c("luh2", "glw3"),
  area_key = c("grid", "polity_area")
) {
  proxy_method <- rlang::arg_match(proxy_method)
  area_key <- rlang::arg_match(area_key)
  .validate_livestock_inputs(
    livestock_data,
    gridded_pasture,
    gridded_cropland,
    country_grid
  )
  country_grid <- .normalize_country_grid(country_grid)

  if (!is.null(years)) {
    years <- sort(unique(as.integer(years)))
    filtered <- .filter_livestock_years(
      years,
      livestock_data,
      gridded_pasture,
      gridded_cropland
    )
    livestock_data <- filtered$livestock_data
    gridded_pasture <- filtered$gridded_pasture
    gridded_cropland <- filtered$gridded_cropland
  }

  .warn_grid_missing_reporters(
    livestock_data,
    country_grid,
    "heads",
    "head"
  )

  if (is.null(species_proxy)) {
    species_proxy <- .default_species_proxy()
  }

  # Identify numeric value columns to distribute (beyond grouping keys)
  key_cols <- c("year", "area_code", "species_group")
  numeric_cols <- setdiff(
    names(livestock_data)[
      vapply(livestock_data, is.numeric, logical(1))
    ],
    key_cols
  )
  if (length(numeric_cols) == 0L) {
    cli::cli_abort("No numeric columns found in {.arg livestock_data}.")
  }

  years <- sort(unique(livestock_data$year))
  groups <- sort(unique(livestock_data$species_group))

  proxy_types <- .livestock_proxy_types(species_proxy, groups)
  .check_livestock_proxy_inputs(proxy_method, glw_density, groups)

  cli::cli_alert_info(
    "Spatializing {length(groups)} groups over {length(years)} years"
  )

  result <- purrr::map(
    years,
    \(yr) {
      country_grid_yr <- .filter_country_grid_year(
        country_grid,
        yr
      )
      if (nrow(country_grid_yr) == 0L) {
        cli::cli_abort("No {.arg country_grid} rows valid for year {yr}.")
      }
      .spatialize_livestock_year(
        yr = yr,
        livestock_yr = dplyr::filter(livestock_data, year == yr),
        pasture_yr = dplyr::filter(gridded_pasture, year == yr),
        cropland_yr = dplyr::filter(gridded_cropland, year == yr),
        country_grid = country_grid_yr,
        proxy_types = proxy_types,
        manure_pattern = manure_pattern,
        glw_density = glw_density,
        grass_productivity = grass_productivity,
        proxy_method = proxy_method,
        numeric_cols = numeric_cols
      )
    },
    .progress = length(years) > 5L
  ) |>
    dplyr::bind_rows()

  result |>
    .spatialize_apply_area_key(area_key, numeric_cols) |>
    .add_reporting_polity_columns()
}


# --- Private helpers : livestock spatialization ---------------------------

#' Default species-group → spatial-proxy mapping.
#' @noRd
.default_species_proxy <- function() {
  tibble::tribble(
    ~species_group,  ~spatial_proxy,
    "cattle",        "pasture",
    "buffalo",       "pasture",
    "sheep_goats",   "pasture",
    "equines",       "pasture",
    "camels",        "rangeland",
    "pigs",          "cropland",
    "poultry",       "cropland",
    "other",         "mixed"
  )
}


#' Build proxy weight grid for one proxy type.
#'
#' Returns a tibble with `lon`, `lat`, `area_code`, `weight` for
#' every grid cell that has the relevant land-use type.
#' @noRd
.build_proxy_grid <- function(
  proxy_type,
  pasture_yr,
  cropland_yr,
  country_grid,
  manure_pattern,
  grass_productivity = NULL
) {
  grid <- switch(
    proxy_type,
    pasture = {
      pasture_yr |>
        dplyr::transmute(
          lon,
          lat,
          weight = pasture_ha + rangeland_ha
        )
    },
    rangeland = {
      pasture_yr |>
        dplyr::transmute(
          lon,
          lat,
          weight = rangeland_ha
        )
    },
    cropland = {
      cropland_yr |>
        dplyr::transmute(
          lon,
          lat,
          weight = cropland_ha
        )
    },
    mixed = {
      # Combine pasture+rangeland and cropland 50/50
      p <- pasture_yr |>
        dplyr::transmute(lon, lat, p_wt = pasture_ha + rangeland_ha)
      c <- cropland_yr |>
        dplyr::transmute(lon, lat, c_wt = cropland_ha)
      dplyr::full_join(p, c, by = c("lon", "lat")) |>
        dplyr::mutate(
          p_wt = dplyr::if_else(is.na(p_wt), 0, p_wt),
          c_wt = dplyr::if_else(is.na(c_wt), 0, c_wt),
          weight = 0.5 * p_wt + 0.5 * c_wt
        ) |>
        dplyr::select(lon, lat, weight)
    },
    cli::cli_abort("Unknown proxy type: {.val {proxy_type}}")
  )

  # Join country assignment

  grid <- grid |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
    dplyr::mutate(weight = weight * cell_area_frac) |>
    dplyr::filter(weight > 0)

  # Optionally weight grazers by grass productivity so animals follow grass
  # production, not just area (deserts get fewer than savannas). Uses the
  # exogenous natural-grass NPP to avoid the livestock -> lsuha -> NPP loop.
  if (
    !is.null(grass_productivity) && proxy_type %in% c("pasture", "rangeland")
  ) {
    grid <- grid |>
      dplyr::left_join(
        dplyr::select(grass_productivity, lon, lat, grass_npp),
        by = c("lon", "lat")
      ) |>
      dplyr::mutate(
        weight = dplyr::if_else(is.na(grass_npp), weight, weight * grass_npp)
      ) |>
      dplyr::filter(weight > 0) |>
      dplyr::select(-grass_npp)
  }

  # Optionally multiply by manure-intensity reference pattern
  if (!is.null(manure_pattern)) {
    grid <- grid |>
      dplyr::left_join(
        dplyr::select(manure_pattern, lon, lat, manure_intensity),
        by = c("lon", "lat")
      ) |>
      dplyr::mutate(manure_intensity = dplyr::coalesce(manure_intensity, 0))
    max_intensity <- if (nrow(grid) > 0L) max(grid$manure_intensity) else 0
    if (max_intensity > 0) {
      grid <- grid |>
        dplyr::mutate(
          # Blend: 70% land-use, 30% manure-reference (avoid zero-out)
          weight = weight * (0.7 + 0.3 * manure_intensity / max_intensity)
        )
    }
  }

  # Carry only the compartment key and the weight, as the GLW3 path does. The
  # join above brings every `country_grid` column along, and any of them
  # colliding with a `livestock_data` value column would be silently suffixed
  # in `.allocate_livestock_to_grid()`'s join and then never distributed.
  grid |>
    dplyr::select(
      dplyr::any_of(.compartment_id_cols(grid)),
      lon,
      lat,
      weight
    )
}


#' Build proxy weight grid from GLW3 species-specific density,
#' scaled by LUH2 time trend.
#'
#' A group absent from `glw_density` yields a zero-row grid, which the
#' caller reports. It cannot arrive here: `.check_livestock_proxy_inputs()`
#' has already refused the whole call for such a group.
#' @noRd
.build_glw_proxy_grid <- function(
  group,
  glw_density,
  pasture_yr,
  cropland_yr,
  country_grid,
  proxy_type
) {
  glw_grp <- dplyr::filter(glw_density, species_group == group)

  # Get current land-use for temporal scaling
  lu <- switch(
    proxy_type,
    pasture = ,
    rangeland = {
      pasture_yr |>
        dplyr::transmute(
          lon,
          lat,
          lu_now = pasture_ha + rangeland_ha
        )
    },
    cropland = {
      cropland_yr |>
        dplyr::transmute(lon, lat, lu_now = cropland_ha)
    },
    mixed = {
      p <- pasture_yr |>
        dplyr::transmute(lon, lat, p_wt = pasture_ha + rangeland_ha)
      c <- cropland_yr |>
        dplyr::transmute(lon, lat, c_wt = cropland_ha)
      dplyr::full_join(p, c, by = c("lon", "lat")) |>
        dplyr::mutate(
          lu_now = dplyr::if_else(is.na(p_wt), 0, p_wt) +
            dplyr::if_else(is.na(c_wt), 0, c_wt)
        ) |>
        dplyr::select(lon, lat, lu_now)
    }
  )

  # GLW density as reference weight, scaled by land-use presence
  glw_grp |>
    dplyr::select(lon, lat, density) |>
    dplyr::inner_join(lu, by = c("lon", "lat")) |>
    dplyr::inner_join(country_grid, by = c("lon", "lat")) |>
    dplyr::mutate(
      # Where LU is zero now, the cell gets zero even if GLW has density
      weight = density * dplyr::if_else(lu_now > 0, 1, 0) * cell_area_frac
    ) |>
    dplyr::filter(weight > 0) |>
    dplyr::select(
      dplyr::any_of(.compartment_id_cols(country_grid)),
      lon,
      lat,
      weight
    )
}


#' Spatialize all species groups for a single year.
#'
#' `proxy_method` picks the weight builder outright: the LUH2 branch never
#' runs under `"glw3"` and the GLW3 branch never runs under `"luh2"`. The
#' try-GLW3-then-fall-back-to-LUH2 block this replaces changed the proxy of
#' whichever group the density table happened to be thin on, silently and
#' per year, so two groups in one output could rest on different evidence
#' with nothing recorded (whep#1000, task T15a-i).
#' @noRd
.spatialize_livestock_year <- function(
  yr,
  livestock_yr,
  pasture_yr,
  cropland_yr,
  country_grid,
  proxy_types,
  manure_pattern,
  glw_density,
  grass_productivity,
  proxy_method,
  numeric_cols
) {
  groups <- unique(livestock_yr$species_group)

  purrr::map(groups, \(grp) {
    grp_data <- dplyr::filter(livestock_yr, species_group == grp)
    proxy_type <- proxy_types[[grp]]

    proxy_grid <- if (proxy_method == "glw3") {
      .build_glw_proxy_grid(
        grp,
        glw_density,
        pasture_yr,
        cropland_yr,
        country_grid,
        proxy_type
      )
    } else {
      .build_proxy_grid(
        proxy_type,
        pasture_yr,
        cropland_yr,
        country_grid,
        manure_pattern,
        grass_productivity
      )
    }

    if (nrow(proxy_grid) == 0L) {
      .warn_empty_proxy_grid(grp, yr, proxy_method)
      return(tibble::tibble())
    }

    .allocate_livestock_to_grid(
      grp_data,
      proxy_grid,
      numeric_cols
    ) |>
      dplyr::mutate(
        year = yr,
        species_group = grp,
        .before = 1L
      ) |>
      dplyr::mutate(
        method_livestock_proxy = .livestock_proxy_method(
          proxy_method,
          proxy_type,
          grass_productivity
        )
      )
  }) |>
    dplyr::bind_rows()
}


#' Resolve each species group's spatial proxy, or abort naming the group.
#'
#' Returns a named character vector, group -> proxy class. Where a group
#' carries several proxy rows the first wins, which is what the replaced
#' `proxy_row$spatial_proxy[1]` lookup did: `livestock_mapping.csv` maps
#' `"other"` to both `cropland` (rabbits, rodents) and `mixed` (live
#' animals nes), and `.read_livestock_mapping()` hands both rows over.
#'
#' The proxy class is checked here rather than at use: `.build_proxy_grid()`
#' aborts on an unknown class through its `switch()` default, but
#' `.build_glw_proxy_grid()`'s `switch()` has no default and would return
#' `NULL` land use and fail somewhere else entirely.
#' @noRd
.livestock_proxy_types <- function(species_proxy, groups) {
  .check_columns(
    species_proxy,
    c("species_group", "spatial_proxy"),
    "species_proxy"
  )
  lookup <- species_proxy |>
    dplyr::filter(species_group %in% groups, !is.na(spatial_proxy)) |>
    dplyr::distinct(species_group, .keep_all = TRUE)

  unmapped <- setdiff(groups, lookup$species_group)
  if (length(unmapped) > 0L) {
    cli::cli_abort(c(
      "{length(unmapped)} {.field species_group} value{?s} in \\
       {.arg livestock_data} {?has/have} no {.arg species_proxy} row:",
      "x" = "{.val {unmapped}}.",
      "i" = "An unmapped group used to fall back to the {.val pasture}
             proxy silently. Map it in {.arg species_proxy}, or upstream in
             {.file inst/extdata/livestock_mapping.csv}, which routes
             catch-all items onto the group {.val other}."
    ))
  }

  classes <- c("pasture", "cropland", "rangeland", "mixed")
  unknown <- setdiff(lookup$spatial_proxy, classes)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.arg species_proxy} carries {length(unknown)} unknown \\
       {.field spatial_proxy} value{?s}:",
      "x" = "{.val {unknown}}.",
      "i" = "Expected one of {.val {classes}}."
    ))
  }

  stats::setNames(lookup$spatial_proxy, lookup$species_group)
}


#' Check the proxy table the requested method needs.
#'
#' `"glw3"` is a selected method, not an opportunistic one: a missing table
#' or a group the table has no positive cell for aborts rather than
#' allocating that group on the LUH2 proxy under a `"glw3"` label.
#' @noRd
.check_livestock_proxy_inputs <- function(proxy_method, glw_density, groups) {
  if (proxy_method != "glw3") {
    if (!is.null(glw_density)) {
      cli::cli_warn(c(
        "{.arg glw_density} is ignored under {.arg proxy_method} \\
         {.val {proxy_method}}.",
        "i" = "Pass {.code proxy_method = \"glw3\"} to allocate on it."
      ))
    }
    return(invisible(NULL))
  }
  if (is.null(glw_density)) {
    cli::cli_abort(c(
      "{.arg glw_density} is required when {.arg proxy_method} is \\
       {.val glw3}.",
      "i" = "The GLW3 density table is the whole weight under this method;
             there is no LUH2 fallback."
    ))
  }
  .check_columns(
    glw_density,
    c("lon", "lat", "species_group", "density"),
    "glw_density"
  )
  covered <- glw_density |>
    dplyr::filter(!is.na(density), density > 0) |>
    dplyr::pull(species_group) |>
    unique()
  missing_groups <- setdiff(groups, covered)
  if (length(missing_groups) > 0L) {
    cli::cli_abort(c(
      "{.arg glw_density} has no positive cell for {length(missing_groups)} \\
       {.field species_group} value{?s} in {.arg livestock_data}:",
      "x" = "{.val {missing_groups}}.",
      "i" = "Add the group to {.arg glw_density} or run those groups under
             {.code proxy_method = \"luh2\"}."
    ))
  }
  invisible(NULL)
}


#' The `method_livestock_proxy` value one species group runs under.
#'
#' Constant within a group: the grass weighting in `.build_proxy_grid()` is
#' applied to the whole grazer grid or to none of it.
#' @noRd
.livestock_proxy_method <- function(
  proxy_method,
  proxy_type,
  grass_productivity
) {
  if (proxy_method == "glw3") {
    return("glw3")
  }
  grazed <- proxy_type %in% c("pasture", "rangeland")
  if (!is.null(grass_productivity) && grazed) "luh2_grass" else "luh2_area"
}


#' Report a species group whose proxy has no positive cell this year.
#'
#' The group's national totals are dropped for the year -- the early return
#' happens before `.allocate_livestock_to_grid()`, so
#' `.warn_unallocated_livestock()` never sees them.
#' @noRd
.warn_empty_proxy_grid <- function(grp, yr, proxy_method) {
  cli::cli_warn(c(
    "No {.val {proxy_method}} proxy cell carries weight for \\
     {.val {grp}} in {yr}.",
    "x" = "The group's national totals are dropped for that year."
  ))
}


#' Proportionally allocate country totals to grid cells.
#'
#' For each country, distributes every numeric column using the
#' same cell weights (proxy area or density).
#' @noRd
.allocate_livestock_to_grid <- function(
  country_data,
  proxy_grid,
  numeric_cols
) {
  # Get countries that have data
  needed <- unique(country_data$area_code)

  # Add country weight sums
  grid <- proxy_grid |>
    dplyr::filter(area_code %in% needed) |>
    dplyr::mutate(
      weight_sum = sum(weight),
      share = dplyr::if_else(
        weight_sum > 0,
        weight / weight_sum,
        0
      ),
      .by = area_code
    )

  # Surface countries with national totals but no allocatable proxy cell
  # before the inner join below silently drops them.
  .warn_unallocated_livestock(needed, grid, country_data)

  # Join country totals
  join_cols <- dplyr::select(
    country_data,
    area_code,
    dplyr::all_of(numeric_cols)
  )

  grid <- grid |>
    dplyr::inner_join(join_cols, by = "area_code")

  # Distribute each numeric column proportionally
  for (col in numeric_cols) {
    grid <- grid |>
      dplyr::mutate(
        !!col := .data[[col]] * share
      )
  }

  grid |>
    dplyr::select(
      dplyr::any_of(.compartment_id_cols(grid)),
      lon,
      lat,
      dplyr::all_of(numeric_cols)
    )
}


#' Warn about countries whose national totals cannot be allocated.
#'
#' A country with national livestock totals but no proxy grid cell (no
#' land-use weight in any of its cells) would be dropped by the inner
#' join with `country_data`, leaking its national total. Detect these
#' and warn with the count, leaked heads (when available), and identities.
#' @noRd
.warn_unallocated_livestock <- function(needed, grid, country_data) {
  dropped <- setdiff(needed, unique(grid$area_code))
  if (length(dropped) == 0L) {
    return(invisible(NULL))
  }
  lost_heads <- if (rlang::has_name(country_data, "heads")) {
    country_data |>
      dplyr::filter(area_code %in% dropped) |>
      dplyr::pull(heads) |>
      sum(na.rm = TRUE)
  } else {
    NA_real_
  }
  head_msg <- if (is.na(lost_heads)) {
    ""
  } else {
    " ({round(lost_heads)} head{?s} dropped)"
  }
  # `codes` is integer, so the plural marker must follow an explicit scalar
  # count: cli's make_quantity() errors on a numeric vector of length > 1.
  codes <- sort(dropped)
  cli::cli_warn(c(
    paste0(
      "{length(dropped)} countr{?y/ies} have national livestock totals ",
      "but no proxy grid cell",
      head_msg,
      ":"
    ),
    "x" = "{length(codes)} area_code{?s}: {.val {codes}}."
  ))
}

#' Validate inputs for build_gridded_livestock.
#' @noRd
.validate_livestock_inputs <- function(
  livestock_data,
  gridded_pasture,
  gridded_cropland,
  country_grid
) {
  .check_columns(
    livestock_data,
    c("year", "area_code", "species_group"),
    "livestock_data"
  )
  .check_columns(
    gridded_pasture,
    c("lon", "lat", "year", "pasture_ha", "rangeland_ha"),
    "gridded_pasture"
  )
  .check_columns(
    gridded_cropland,
    c("lon", "lat", "year", "cropland_ha"),
    "gridded_cropland"
  )
  .check_columns(
    country_grid,
    c("lon", "lat", "area_code"),
    "country_grid"
  )
}

#' Filter year-keyed livestock inputs to the requested years.
#'
#' Warns if any requested year is absent from `livestock_data` and
#' filters all three year-keyed inputs accordingly.
#' @noRd
.filter_livestock_years <- function(
  years,
  livestock_data,
  gridded_pasture,
  gridded_cropland
) {
  available <- unique(as.integer(livestock_data$year))
  missing_years <- setdiff(years, available)
  if (length(missing_years) > 0L) {
    cli::cli_warn(c(
      "{length(missing_years)} requested year{?s} not found in \\
       {.arg livestock_data}:",
      "x" = "{.val {missing_years}}."
    ))
  }
  list(
    livestock_data = dplyr::filter(livestock_data, year %in% years),
    gridded_pasture = dplyr::filter(gridded_pasture, year %in% years),
    gridded_cropland = dplyr::filter(gridded_cropland, year %in% years)
  )
}
