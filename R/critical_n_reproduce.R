# Critical-nitrogen allowances computed with the method of Schulte-Uebbing,
# Beusen, Bouwman & de Vries (2022), Nature 610, 507-512,
# doi:10.1038/s41586-022-05158-2, Supplementary Information (SI) Eqs. 1-36 and
# Supplementary Tables 2-5, instead of read as the fixed 2010 layers of their
# archive (Zenodo doi:10.5281/zenodo.6395016 v1.0). Issue #1291, step 1.
#
# The SI prints the equations for cells with one reducible agricultural land
# use. Everything else below was recovered from the archive itself, by
# recomputing its own "Output_files" from its own "Input_files" (2026-10-06):
#
# * Fertiliser and manure enter gross of NH3. The archive's n_fe_eff_* and
#   n_man_eff_* layers are net of the NH3 they emit: the deposited current
#   inputs (critical + exceedance, all 28,573 arable and 11,740 intensive
#   grassland cells) equal net + NH3 to the 0.001 kg N/ha the rasters carry,
#   and Critical losses/nle_crit_ph.asc is reproduced in all 31,072 cells only
#   with that gross total.
# * The groundwater limit is 11.6 mg NO3-N/l, the SI value. The Methods
#   section of the article prints 11.3 (50 mg NO3/l); 11.6 reproduces
#   nle_crit_ph.asc exactly, 11.3 in 11% of cells.
# * Ice (biome code 7) gets 5 kg N/ha. SI Supplementary Table 2 lists "n.a.";
#   all 524 ice cells of Critical losses/nem_crit_ph.asc carry 5.000.
# * The fertiliser share of fertiliser plus manure is clipped to
#   [1e-4, 1 - 1e-4]. Cells with only one of the two carry a critical input
#   1.0001e-4 times the other above the unclipped solution, in every such cell.
# * The yield-gap ratios carry three decimals; SI Supplementary Table 5 prints
#   two. See .critn_region_ratios().
# * Mixed cells, deposition: the agricultural NH3 the critical deposition
#   leaves room for is shared between arable land and intensive grassland by
#   their current NH3 emission (2026-10-08; see .critn_env_deposition()).
#   Surface water: both land uses' fertiliser plus manure scale by one common
#   factor. The reported input adds the other land use's deposition; uptake
#   of a cut-off land use is its uptake at yield potential, otherwise current
#   NUE times the input before any cut-off.
# * The cut-off binds where uptake at the critical input reaches uptake at
#   yield potential, not where fertiliser plus manure reaches its own cut-off
#   value (2026-10-08; see .critn_reaches_potential()).
# * No allowance for a land use whose fertiliser and manure emit no NH3, nor
#   in a mixed cell whose other land use has no uptake (2026-10-08).
# * Groundwater in cells with both arable land and intensive grassland is the
#   one rule the SI leaves unprinted. The rule below (each land use against
#   its area share of the critical leaching, the one that must fall further
#   solved first) is a reconstruction; see .critn_env_groundwater().
# * "All impacts" is the lowest of the three (SI Eq. 31). The archive departs
#   from that in 57 arable cells; see .critn_env_minimum().
#
# How close the result is, layer by layer, is measured by
# inst/scripts/validate_critical_n_reproduction.R and pinned by the real-data
# test in tests/testthat/test_critical_n_reproduce.R.

#' Calculate critical-nitrogen allowances with the Schulte-Uebbing method.
#'
#' @description
#' Computes the critical nitrogen input and surplus of every 0.5-degree cell
#' with the equations of Schulte-Uebbing et al. (2022, Nature 610, 507-512,
#' doi:10.1038/s41586-022-05158-2, Supplementary Information Eqs. 1-36),
#' instead of reading the fixed 2010 layers of their archive
#' (doi:10.5281/zenodo.6395016) as [read_critical_n()] does by default. The
#' critical input is the fertiliser and manure that keeps each of three
#' impacts at its threshold, plus the constant fixation and the deposition it
#' implies:
#'
#' * `"de"`: deposition at the critical load of the cell's biome (5 to
#'   20 kg N/ha of cell area, SI Supplementary Table 2);
#' * `"sw"`: 5 mg N/l in runoff to surface water (SI Eq. 12);
#' * `"gw"`: 11.6 mg NO3-N/l in water leaching from agricultural soil (SI
#'   Eq. 29);
#' * `"mi"`: the lowest of the three, land use by land use (SI Eqs. 31-32).
#'
#' Fertiliser and manure change in their current proportion (SI Eq. 3);
#' fixation, the non-agricultural sources and extensive grassland stay at
#' their current values. Where the non-agricultural sources alone exceed a
#' threshold, fertiliser and manure are set to zero (`critical_rule`
#' `"non_agricultural_floor"`). Where the threshold allows more than crops can
#' use, the input is cut off at the input needed for the regional yield
#' potential at current nitrogen use efficiency, capped at 0.8 (Methods
#' Eqs. 1-2; `"yield_potential_cap"`). The critical surplus is input minus
#' uptake (SI Eqs. 5-6).
#'
#' With the archive's own 2010 inputs the result reproduces the deposited
#' layers closely but not exactly, because the source does not print every
#' rule it applied (see the section below). Supplying `inputs` from another
#' year or model applies the same method to them.
#'
#' @section Rules recovered from the deposited layers:
#' The SI prints the equations for cells with one reducible agricultural land
#' use and states that cells combining arable land with grassland follow
#' "slightly different" formulas. The rules for those cells, and several
#' values the text leaves out, were recovered by recomputing the archive's
#' outputs from its inputs: fertiliser and manure enter gross of their NH3
#' emission; the groundwater limit is 11.6 mg NO3-N/l (the article's Methods
#' print 11.3); ice cells get 5 kg N/ha of critical deposition (Supplementary
#' Table 2 prints "n.a."); the fertiliser share is clipped to
#' \[1e-4, 1 - 1e-4\]; the regional yield-gap ratios carry three decimals
#' (each rounds to the two printed in Supplementary Table 5). The cut-off
#' binds where uptake at the critical input (with the deposition of every
#' land use in the cell) reaches uptake at yield potential; with NUE above
#' 0.8 this raises the input to the cut-off input. In cells with both arable
#' land and intensive grassland, the NH3 the critical deposition leaves room
#' for is shared by their current NH3 emission, and both land uses are scaled
#' by one factor for the surface-water threshold. For groundwater there, each
#' land use is held to its area share of the critical leaching and the one
#' that must fall further is solved first; this rule is a reconstruction, and
#' those cells carry the remaining differences.
#'
#' One departure is deliberate: `"mi"` is the lowest of the three thresholds
#' as SI Eq. 31 defines it. In 57 arable cells where deposition and
#' groundwater both sit at the non-agricultural floor, the deposited
#' all-impacts layer carries the higher surface-water value instead.
#'
#' @param inputs Optional tibble with one row per cell and the IMAGE-GNM
#'   quantities listed by `whep:::.critn_input_specs()`: `cell_id`, `lon`,
#'   `lat`, areas in hectares (`area_total_ha`, `area_arable_ha`,
#'   `area_intensive_ha`, `area_extensive_ha`, `area_natural_ha`), the IMAGE
#'   `biome` and `image_region` codes, `runoff_l` (litres per year) and the
#'   nitrogen flows in kg N per cell per year. Fertiliser and manure are net
#'   of NH3, as the archive deposits them. When `NULL` (default) the 2010
#'   inputs are read from the archive's `Input_files`.
#' @param dir Optional archive directory, resolved as in [read_critical_n()].
#'   Ignored when `inputs` is supplied.
#' @param verify_source If `TRUE` (default), the archive's input rasters are
#'   checked against the package's content manifest before they are read.
#'   Ignored when `inputs` is supplied.
#' @param example If `TRUE`, return the result for a small fixture of four
#'   cells instead of reading data. Defaults to `FALSE`.
#' @return A tibble with one row per cell, threshold and land-use scope:
#'   `cell_id`, `lon`, `lat`, `image_region`, `critical_threshold` (`"de"`,
#'   `"sw"`, `"gw"` or `"mi"`), `critical_land_use` (`"ara"`, `"igl"` or
#'   `"all"`), `area_ha` (the hectares of that scope), the critical
#'   `critical_n_input_kgn_ha` and `critical_n_surplus_kgn_ha`, the current
#'   `current_n_input_kgn_ha` and `current_n_surplus_kgn_ha` (all kg N per
#'   hectare per year), `critical_rule` (`"environmental_threshold"`,
#'   `"non_agricultural_floor"` or `"yield_potential_cap"`; `NA` for `"all"`)
#'   and `method_critical_n = "reproduced"`. A cell whose land use has no
#'   current fertiliser or manure, no crop uptake, or no agricultural leaching
#'   has no critical value there, as in the archive.
#' @export
#' @examples
#' calculate_critical_n(example = TRUE)
calculate_critical_n <- function(
  inputs = NULL,
  dir = NULL,
  verify_source = TRUE,
  example = FALSE
) {
  if (isTRUE(example)) {
    inputs <- .example_critical_n_inputs()
  }
  if (is.null(inputs)) {
    root <- .critn_root_path(.resolve_critical_n_dir(dir))
    if (isTRUE(verify_source)) {
      .critn_verify_paths(root, .critn_input_paths())
    }
    inputs <- .critn_read_inputs(root)
  }
  .critn_check_inputs(inputs)
  prep <- .critn_prepare(inputs)
  env <- list(
    de = .critn_env_deposition(prep),
    sw = .critn_env_surface_water(prep),
    gw = .critn_env_groundwater(prep)
  )
  env$mi <- .critn_env_minimum(env)
  purrr::imap(env, \(x, threshold) .critn_finish(prep, x, threshold)) |>
    purrr::list_rbind() |>
    dplyr::mutate(method_critical_n = "reproduced")
}

# ---- Constants -----------------------------------------------------------

# Critical concentrations, kg N per litre. Surface water: 5 mg N/l in runoff
# (SI "Critical concentration for N runoff to surface water", 2.5 mg N/l in
# surface water at 50% retention). Groundwater: 11.6 mg NO3-N/l (SI text
# above Eq. 29; the Methods print 11.3, the archive was computed with 11.6).
.critn_conc_surface_water <- function() 5e-6

.critn_conc_groundwater <- function() 11.6e-6

# NUE ceiling in the cut-off (Methods, "Cut-off value for critical N
# surpluses and N inputs": "we capped the NUE ... at 0.8").
.critn_nue_cap <- function() 0.8

# Bounds on the fertiliser share of fertiliser plus manure. Not printed in the
# source; recovered from the archive, where every cell with only one of the
# two carries a critical input 1.0001e-4 times the other above the unclipped
# solution, which is the effect of this clip.
.critn_fertilizer_share_clip <- function() 1e-4

# Critical N deposition per IMAGE biome, kg N per ha of cell area per year:
# Schulte-Uebbing et al. (2022), SI Supplementary Table 2. `biome` is the code
# in the archive's Input_files/gnlct.asc. The code-to-rate match was measured
# against Output_files/Critical losses/nem_crit_ph.asc, which equals this rate
# in all 66,222 cells; biomes sharing a rate (codes 8-9, 10-11, 12-13, 19-20)
# are named in IMAGE's land-cover order, which the rates cannot tell apart.
# Ice: the table prints "n.a."; the archive applies 5.
.critn_biome_rates <- function() {
  tibble::tribble(
    ~biome, ~biome_name,                  ~critical_deposition_kgn_ha,
    7L,     "Ice",                        5,
    8L,     "Tundra",                     10,
    9L,     "Wooded tundra",              10,
    10L,    "Boreal forest",              7.5,
    11L,    "Cool coniferous forest",     7.5,
    12L,    "Temperate mixed forest",     12.5,
    13L,    "Temperate deciduous forest", 12.5,
    14L,    "Warm mixed forest",          10,
    15L,    "Grassland and steppe",       17.5,
    16L,    "Hot desert",                 5,
    17L,    "Scrubland",                  7.5,
    18L,    "Savanna",                    15,
    19L,    "Tropical woodland",          20,
    20L,    "Tropical forest",            20
  )
}

# Yield-gap ratios per IMAGE region (archive Input_files/image_region28.asc
# codes 1-26, in the row order of SI Supplementary Table 5). Arable: yield
# potential over actual yield (Mueller et al. 2012). Intensive grassland:
# maximum uptake over current uptake, at least 1 (Table 5 note 6). The table
# prints two decimals (`*_si`); the archive was computed with three. They were
# recovered as (critical input - critical surplus) / current uptake in the
# cells at the cut-off, where uptake is uptake at yield potential: 37 to
# 6,359 cells per region, standard deviation at most 4e-5. Every value rounds
# to the printed one (a test checks this).
.critn_region_ratios <- function() {
  tibble::tribble(
    ~image_region, ~region_name,       ~ratio_arable, ~ratio_arable_si,
    ~ratio_grass, ~ratio_grass_si,
    1L,  "Canada",                    1.274, 1.27, 1.000, 0.62,
    2L,  "USA",                       1.252, 1.25, 2.336, 2.34,
    3L,  "Mexico",                    1.566, 1.57, 2.467, 2.47,
    4L,  "Central America",           1.797, 1.80, 2.341, 2.34,
    5L,  "Brazil",                    1.343, 1.34, 2.486, 2.49,
    6L,  "Rest of South America",     1.532, 1.53, 2.857, 2.86,
    7L,  "Northern Africa",           2.711, 2.71, 1.365, 1.37,
    8L,  "Western Africa",            2.363, 2.36, 2.639, 2.64,
    9L,  "Eastern Africa",            2.424, 2.42, 1.814, 1.81,
    10L, "South Africa",              1.848, 1.85, 1.000, 0.94,
    11L, "Western Europe",            1.177, 1.18, 1.039, 1.04,
    12L, "Central Europe",            1.982, 1.98, 1.309, 1.31,
    13L, "Turkey",                    1.797, 1.80, 1.993, 1.99,
    14L, "Ukraine region",            2.633, 2.63, 1.641, 1.64,
    15L, "Central Asia",              2.928, 2.93, 1.502, 1.50,
    16L, "Russia region",             2.391, 2.39, 2.264, 2.26,
    17L, "Middle East",               2.170, 2.17, 1.000, 0.71,
    18L, "India",                     1.508, 1.51, 1.000, 0.61,
    19L, "Korea region",              1.180, 1.18, 1.226, 1.23,
    20L, "China region",              1.503, 1.50, 1.826, 1.83,
    21L, "Southeastern Asia",         1.479, 1.48, 1.440, 1.44,
    22L, "Indonesia region",          1.267, 1.27, 1.486, 1.49,
    23L, "Japan",                     1.180, 1.18, 1.000, 0.77,
    24L, "Oceania",                   1.487, 1.49, 1.322, 1.32,
    25L, "Rest of South Asia",        1.870, 1.87, 1.573, 1.57,
    26L, "Rest of Southern Africa",   2.551, 2.55, 3.995, 3.99
  )
}

# The archive raster behind each input column. Fertiliser and manure are net
# of NH3 (the *_eff layers); grazing manure is part of intensive and extensive
# grassland manure. Fertiliser on grassland is booked to intensive grassland:
# SI Supplementary Table 3 lists synthetic fertiliser on arable land and
# intensive grassland only, and NH3 from fertiliser on extensive grassland is
# zero in every archive cell.
.critn_input_specs <- function() {
  tibble::tribble(
    ~file,                      ~column,
    "a_tot",                    "area_total_ha",
    "a_crop",                   "area_arable_ha",
    "a_gr_int",                 "area_intensive_ha",
    "a_gr_ext",                 "area_extensive_ha",
    "a_nat",                    "area_natural_ha",
    "gnlct",                    "biome",
    "image_region28",           "image_region",
    "q",                        "runoff_l",
    "n_fe_eff_crop",            "fertilizer_net_arable_kg",
    "n_fe_eff_grass",           "fertilizer_net_grass_kg",
    "n_man_eff_crops",          "manure_net_arable_kg",
    "n_man_eff_grass_int",      "manure_net_intensive_kg",
    "n_man_eff_grass_ext",      "manure_net_extensive_kg",
    "nh3_spread_fe_crops",      "nh3_fertilizer_arable_kg",
    "nh3_spread_fe_grass_int",  "nh3_fertilizer_intensive_kg",
    "nh3_spread_fe_grass_ext",  "nh3_fertilizer_extensive_kg",
    "nh3_spread_man_crops",     "nh3_spreading_arable_kg",
    "nh3_spread_man_grass_int", "nh3_spreading_intensive_kg",
    "nh3_spread_man_grass_ext", "nh3_spreading_extensive_kg",
    "nh3_graz_int",             "nh3_grazing_intensive_kg",
    "nh3_graz_ext",             "nh3_grazing_extensive_kg",
    "nh3_stor",                 "nh3_storage_kg",
    "ndep",                     "deposition_kg",
    "nfix_crop",                "fixation_arable_kg",
    "nfix_grass_int",           "fixation_intensive_kg",
    "nfix_grass_ext",           "fixation_extensive_kg",
    "nfix_nat",                 "fixation_natural_kg",
    "n_up_crops",               "uptake_arable_kg",
    "n_up_grass_int",           "uptake_intensive_kg",
    "n_up_grass_ext",           "uptake_extensive_kg",
    "nsro_ag",                  "surface_runoff_ag_kg",
    "nsro_nat",                 "surface_runoff_natural_kg",
    "nle_ag",                   "leaching_ag_kg",
    "nle_nat",                  "leaching_natural_kg",
    "ngw_ag",                   "groundwater_ag_kg",
    "ngw_nat",                  "groundwater_natural_kg",
    "fgw_rec_ag",               "groundwater_recent_ag",
    "fgw_rec_nat",              "groundwater_recent_natural",
    "nero_ag",                  "erosion_ag_kg",
    "nero_nat",                 "erosion_natural_kg",
    "nww",                      "wastewater_kg",
    "nallo",                    "allochthonous_kg",
    "naqua",                    "aquaculture_kg",
    "ndep_sw",                  "deposition_water_kg"
  )
}

# ---- Reading the archive inputs -----------------------------------------

.critn_input_paths <- function() {
  file.path("Input_files", paste0(.critn_input_specs()$file, ".asc"))
}

# One row per cell with a total area. Each raster is read as a full 720 x 360
# vector, so the canonical cell id is the vector index (row 1 = north); a
# raster off that grid aborts. NODATA becomes NA.
.critn_read_inputs <- function(root) {
  specs <- .critn_input_specs()
  values <- purrr::map(
    rlang::set_names(.critn_input_paths(), specs$column),
    \(path) .critn_read_vector(file.path(root, path))
  )
  tibble::as_tibble(values) |>
    dplyr::mutate(
      cell_id = seq_len(720L * 360L),
      lon = -179.75 + ((.data$cell_id - 1L) %% 720L) * 0.5,
      lat = 89.75 - ((.data$cell_id - 1L) %/% 720L) * 0.5,
      biome = as.integer(.data$biome),
      image_region = as.integer(.data$image_region),
      .before = 1L
    ) |>
    dplyr::filter(!is.na(.data$area_total_ha))
}

.critn_read_vector <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("Critical-nitrogen input raster not found: {.file {path}}.")
  }
  header <- .read_asc_header(path)
  canonical <- c(
    ncols = 720,
    nrows = 360,
    xllcorner = -180,
    yllcorner = -90,
    cellsize = 0.5
  )
  if (!isTRUE(all.equal(header[names(canonical)], canonical))) {
    cli::cli_abort(
      "{.file {basename(path)}} is not on the canonical 0.5-degree grid."
    )
  }
  values <- scan(path, skip = 6, quiet = TRUE)
  values[values == header[["nodata_value"]]] <- NA_real_
  values
}

# The contract a caller-built `inputs` must meet. Areas and the land-use
# flows of a cell with agricultural land must be present; the flows that set
# every allowance must also have been supplied, not zero-filled.
.critn_check_inputs <- function(inputs) {
  specs <- .critn_input_specs()
  .check_columns(inputs, c("cell_id", "lon", "lat", specs$column), "inputs")
  ag <- inputs$area_arable_ha > 0 | inputs$area_intensive_ha > 0
  ag[is.na(ag)] <- FALSE
  flows <- setdiff(specs$column, c("biome", "image_region"))
  n_na <- purrr::map_int(flows, \(col) sum(ag & is.na(inputs[[col]])))
  if (any(n_na > 0)) {
    cli::cli_abort(
      c(
        "Missing critical-nitrogen inputs on cells with agricultural land.",
        x = "{.field {flows[n_na > 0]}}.",
        i = "An absent IMAGE layer is a reader defect, not a zero."
      ),
      class = "whep_critn_missing_input"
    )
  }
  inputs[ag, , drop = FALSE] |>
    dplyr::mutate(
      uptake_reducible_kg = .data$uptake_arable_kg + .data$uptake_intensive_kg
    ) |>
    check_inputs_supplied(
      c(
        runoff = "runoff_l",
        leaching = "leaching_ag_kg",
        deposition = "deposition_kg",
        uptake = "uptake_reducible_kg"
      ),
      details = c(i = "Every critical allowance is computed from these.")
    )
  invisible(inputs)
}

# ---- Current state and fractions -----------------------------------------

# Adds the per-cell terms of SI Supplementary Table 4. Intermediate columns
# are named after the SI symbols they hold (fsro_ag is f_sro,ag, fnup_ara is
# fN_up,ara, ...); `x_*` is fertiliser plus manure and `c_*` the NH3 it emits
# per kg.
.critn_prepare <- function(inputs) {
  inputs |>
    .critn_gross_inputs() |>
    .critn_current_inputs() |>
    .critn_loss_fractions() |>
    .critn_cut_off() |>
    .critn_limits()
}

# Gross fertiliser and manure per land use, their NH3, and the emissions that
# stay fixed. Storage NH3 is shared by net manure (SI Supplementary Table 4
# rows 8-10). Fixed emissions are NOx plus all NH3 from extensive grassland:
# corrected deposition (row 6, at least total NH3) minus the NH3 of arable
# land and intensive grassland.
.critn_gross_inputs <- function(inputs) {
  inputs |>
    dplyr::mutate(
      manure_net = .data$manure_net_arable_kg +
        .data$manure_net_intensive_kg +
        .data$manure_net_extensive_kg,
      storage_ara = .critn_div(.data$manure_net_arable_kg, .data$manure_net) *
        .data$nh3_storage_kg,
      storage_igl = .critn_div(
        .data$manure_net_intensive_kg,
        .data$manure_net
      ) *
        .data$nh3_storage_kg,
      nh3_fer_ara = .data$nh3_fertilizer_arable_kg,
      nh3_man_ara = .data$nh3_spreading_arable_kg + .data$storage_ara,
      nh3_fer_igl = .data$nh3_fertilizer_intensive_kg,
      nh3_man_igl = .data$nh3_spreading_intensive_kg +
        .data$nh3_grazing_intensive_kg +
        .data$storage_igl,
      nh3_egl = .data$nh3_fertilizer_extensive_kg +
        .data$nh3_spreading_extensive_kg +
        .data$nh3_grazing_extensive_kg +
        .data$nh3_storage_kg -
        .data$storage_ara -
        .data$storage_igl,
      fer_ara = .data$fertilizer_net_arable_kg + .data$nh3_fer_ara,
      man_ara = .data$manure_net_arable_kg + .data$nh3_man_ara,
      fer_igl = .data$fertilizer_net_grass_kg + .data$nh3_fer_igl,
      man_igl = .data$manure_net_intensive_kg + .data$nh3_man_igl,
      input_egl_fixed = .data$manure_net_extensive_kg + .data$nh3_egl,
      nh3_reducible = .data$nh3_fer_ara +
        .data$nh3_man_ara +
        .data$nh3_fer_igl +
        .data$nh3_man_igl,
      deposition_corr = pmax(
        .data$deposition_kg,
        .data$nh3_reducible + .data$nh3_egl
      ),
      emission_fixed = .data$deposition_corr - .data$nh3_reducible,
      x_ara = .data$fer_ara + .data$man_ara,
      x_igl = .data$fer_igl + .data$man_igl,
      c_ara = .critn_nh3_per_input(
        .data$fer_ara,
        .data$man_ara,
        .data$nh3_fer_ara,
        .data$nh3_man_ara
      ),
      c_igl = .critn_nh3_per_input(
        .data$fer_igl,
        .data$man_igl,
        .data$nh3_fer_igl,
        .data$nh3_man_igl
      )
    )
}

# NH3 emitted per kg of fertiliser plus manure at the current (clipped)
# fertiliser share: SI Supplementary Table 4 rows 11-14 and 30-31.
.critn_nh3_per_input <- function(fer, man, nh3_fer, nh3_man) {
  clip <- .critn_fertilizer_share_clip()
  share <- pmin(pmax(.critn_div(fer, fer + man), clip), 1 - clip)
  share * .critn_div(nh3_fer, fer) + (1 - share) * .critn_div(nh3_man, man)
}

# Current inputs per land use (SI Supplementary Table 4 rows 15-23).
.critn_current_inputs <- function(prep) {
  prep |>
    dplyr::mutate(
      f_ara = .data$area_arable_ha / .data$area_total_ha,
      f_igl = .data$area_intensive_ha / .data$area_total_ha,
      f_egl = .data$area_extensive_ha / .data$area_total_ha,
      f_nat = .data$area_natural_ha / .data$area_total_ha,
      input_ara = .data$x_ara +
        .data$fixation_arable_kg +
        .data$f_ara * .data$deposition_corr,
      input_igl = .data$x_igl +
        .data$fixation_intensive_kg +
        .data$f_igl * .data$deposition_corr,
      input_egl = .data$input_egl_fixed +
        .data$fixation_extensive_kg +
        .data$f_egl * .data$deposition_corr,
      input_nat = .data$fixation_natural_kg +
        .data$f_nat * .data$deposition_corr,
      uptake_ag = .data$uptake_arable_kg +
        .data$uptake_intensive_kg +
        .data$uptake_extensive_kg
    )
}

# Loss and delivery fractions (SI Supplementary Table 4 rows 24-37), the
# constant part of the load to surface water (SI Eq. 14) and the uptake
# fraction of each reducible land use (rows 28-29). Undefined fractions of a
# flow that is zero are zero.
.critn_loss_fractions <- function(prep) {
  prep |>
    dplyr::mutate(
      input_ag = .data$input_ara + .data$input_igl + .data$input_egl,
      fsro_ag = .critn_div(.data$surface_runoff_ag_kg, .data$input_ag),
      fle_ag = .critn_div(
        .data$leaching_ag_kg,
        .data$input_ag - .data$uptake_ag - .data$surface_runoff_ag_kg
      ),
      fsro_nat = .critn_div(.data$surface_runoff_natural_kg, .data$input_nat),
      fle_nat = .critn_div(
        .data$leaching_natural_kg,
        .data$input_nat - .data$surface_runoff_natural_kg
      ),
      fgw_ag = .critn_div(
        .data$groundwater_ag_kg * .data$groundwater_recent_ag,
        .data$leaching_ag_kg
      ),
      fgw_nat = .critn_div(
        .data$groundwater_natural_kg * .data$groundwater_recent_natural,
        .data$leaching_natural_kg
      ),
      load_fixed = .data$erosion_ag_kg +
        .data$erosion_natural_kg +
        .data$groundwater_ag_kg * (1 - .data$groundwater_recent_ag) +
        .data$groundwater_natural_kg * (1 - .data$groundwater_recent_natural) +
        .data$wastewater_kg +
        .data$allochthonous_kg +
        .data$aquaculture_kg +
        .data$deposition_water_kg,
      fnup_ara = .critn_div(
        .data$uptake_arable_kg,
        .data$input_ara * (1 - .data$fsro_ag)
      ),
      fnup_igl = .critn_div(
        .data$uptake_intensive_kg,
        .data$input_igl * (1 - .data$fsro_ag)
      ),
      nue_ara = .data$uptake_arable_kg / .data$input_ara,
      nue_igl = .data$uptake_intensive_kg / .data$input_igl
    )
}

# The cut-off (Methods Eqs. 1-2): the input that reaches the regional yield
# potential at current NUE, NUE capped at 0.8. Expressed as fertiliser plus
# manure, it is solved with the deposition from the land use's own emissions
# only (SI Eq. 2), which is what the archive's mixed cells show.
.critn_cut_off <- function(prep) {
  cap <- .critn_nue_cap()
  prep |>
    dplyr::left_join(
      dplyr::select(
        .critn_region_ratios(),
        "image_region",
        "ratio_arable",
        "ratio_grass"
      ),
      by = "image_region",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      uptake_max_ara = .data$uptake_arable_kg * .data$ratio_arable,
      uptake_max_igl = .data$uptake_intensive_kg * .data$ratio_grass,
      x_max_ara = (.data$uptake_max_ara /
        pmin(.data$nue_ara, cap) -
        .data$fixation_arable_kg -
        .data$f_ara * .data$emission_fixed) /
        (1 + .data$f_ara * .data$c_ara),
      x_max_igl = (.data$uptake_max_igl /
        pmin(.data$nue_igl, cap) -
        .data$fixation_intensive_kg -
        .data$f_igl * .data$emission_fixed) /
        (1 + .data$f_igl * .data$c_igl)
    )
}

# Critical deposition (SI Eq. 7), load to surface water (SI Eq. 12) and
# leaching from each reducible land use (SI Eq. 29, area share of the cell).
.critn_limits <- function(prep) {
  water <- (1 - prep$fsro_ag) * prep$runoff_l * .critn_conc_groundwater()
  prep |>
    dplyr::left_join(
      dplyr::select(
        .critn_biome_rates(),
        "biome",
        "critical_deposition_kgn_ha"
      ),
      by = "biome",
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      limit_de = .data$critical_deposition_kgn_ha * .data$area_total_ha,
      limit_sw = .data$runoff_l * .critn_conc_surface_water(),
      limit_gw_ara = .env$water * .data$f_ara,
      limit_gw_igl = .env$water * .data$f_igl
    )
}

# ---- Critical fertiliser plus manure per threshold ------------------------

# Total emission and load to surface water when arable land and intensive
# grassland receive `x_ara` and `x_igl` kg of fertiliser plus manure: SI
# Eqs. 1-2 and 13-25, with extensive grassland's inputs and uptake constant.
.critn_forward <- function(p, x_ara, x_igl) {
  emission <- p$emission_fixed + p$c_ara * x_ara + p$c_igl * x_igl
  ara <- x_ara + p$fixation_arable_kg + p$f_ara * emission
  igl <- x_igl + p$fixation_intensive_kg + p$f_igl * emission
  egl <- p$input_egl_fixed + p$fixation_extensive_kg + p$f_egl * emission
  ag <- ara + igl + egl
  runoff <- p$fsro_ag * ag
  uptake <- (1 - p$fsro_ag) *
    (p$fnup_ara * ara + p$fnup_igl * igl) +
    p$uptake_extensive_kg
  leaching <- p$fle_ag * (ag - uptake - runoff)
  nat <- p$fixation_natural_kg + p$f_nat * emission
  nat_leaching <- p$fle_nat * (1 - p$fsro_nat) * nat
  list(
    emission = emission,
    load = runoff +
      p$fsro_nat * nat +
      p$fgw_ag * leaching +
      p$fgw_nat * nat_leaching +
      p$load_fixed
  )
}

# The one factor that brings a threshold quantity to its limit when both
# reducible land uses scale their current fertiliser plus manure by it. The
# quantity is affine in the factor, so two evaluations solve it.
.critn_common_factor <- function(p, quantity, limit) {
  at_zero <- .critn_forward(p, 0, 0)[[quantity]]
  at_one <- .critn_forward(p, p$x_ara, p$x_igl)[[quantity]]
  factor <- (limit - at_zero) / (at_one - at_zero)
  list(x_ara = factor * p$x_ara, x_igl = factor * p$x_igl)
}

# SI Eqs. 7-10. The agricultural NH3 the critical deposition leaves room for
# (Eq. 8) is shared between arable land and intensive grassland in
# proportion to their current NH3 emission, and each share is turned into
# fertiliser plus manure by that land use's NH3 per kg (Eq. 10). Recovered
# from the archive: sharing by current NH3 rather than by NH3 per kg times
# current input (which differ by the fertiliser-share clip) reproduces all
# 11,432 mixed cells of the deposited "de" layers within 0.01 kg N/ha.
.critn_env_deposition <- function(p) {
  allowance <- p$limit_de - p$emission_fixed
  nh3_ara <- p$nh3_fer_ara + p$nh3_man_ara
  nh3_igl <- p$nh3_fer_igl + p$nh3_man_igl
  list(
    x_ara = .critn_div(
      allowance * .critn_div(nh3_ara, nh3_ara + nh3_igl),
      p$c_ara
    ),
    x_igl = .critn_div(
      allowance * .critn_div(nh3_igl, nh3_ara + nh3_igl),
      p$c_igl
    )
  )
}

# SI Eqs. 11-28.
.critn_env_surface_water <- function(p) {
  .critn_common_factor(p, "load", p$limit_sw)
}

# SI Eq. 30, per land use against its area share of the critical leaching
# (SI Eq. 29). With both arable land and intensive grassland in a cell (rule
# not printed in the source, reconstructed): each is first solved with the
# other at its current emission; the one whose fertiliser plus manure must
# fall further keeps that value, and the other is solved again with the
# deposition of the first one's critical emission.
.critn_env_groundwater <- function(p) {
  first_ara <- .critn_solve_leaching(p, "ara", p$c_igl * p$x_igl)
  first_igl <- .critn_solve_leaching(p, "igl", p$c_ara * p$x_ara)
  ara_first <- first_ara / p$x_ara <= first_igl / p$x_igl
  ara_first[is.na(ara_first)] <- TRUE
  list(
    x_ara = dplyr::if_else(
      ara_first,
      first_ara,
      .critn_solve_leaching(p, "ara", p$c_igl * pmax(first_igl, 0))
    ),
    x_igl = dplyr::if_else(
      ara_first,
      .critn_solve_leaching(p, "igl", p$c_ara * pmax(first_ara, 0)),
      first_igl
    )
  )
}

# Fertiliser plus manure on one land use at which its leaching meets its
# limit, given `other` kg of NH3 emitted by the other reducible land use.
.critn_solve_leaching <- function(p, land_use, other) {
  f <- p[[paste0("f_", land_use)]]
  nh3 <- p[[paste0("c_", land_use)]]
  fix <- p[[.critn_land_use_col(land_use, "fixation")]]
  per_input <- p$fle_ag * (1 - p$fsro_ag) * (1 - p[[paste0("fnup_", land_use)]])
  input <- p[[paste0("limit_gw_", land_use)]] / per_input
  (input - fix - f * (p$emission_fixed + other)) / (1 + f * nh3)
}

# SI Eqs. 31-32: the lowest critical fertiliser plus manure of the three
# thresholds, land use by land use. The deposited all-impacts layers depart
# from this in 57 arable cells (2 arable-only, 51 with extensive grassland, 4
# with intensive grassland; no grassland cell): where deposition and
# groundwater both sit at the non-agricultural floor, they carry the
# surface-water value. All 57 are reproduced by a choice with strict
# comparisons (deposition if strictly below both others, else groundwater if
# strictly below both, else surface water), which a tie at the floor sends to
# surface water. That allowance exceeds two of the three thresholds, so the
# minimum the SI defines is kept. Measured 2026-10-08.
.critn_env_minimum <- function(env) {
  list(
    x_ara = do.call(pmin, purrr::map(env, \(x) pmax(x$x_ara, 0))),
    x_igl = do.call(pmin, purrr::map(env, \(x) pmax(x$x_igl, 0)))
  )
}

# ---- From critical fertiliser plus manure to allowances --------------------

# Floor at zero, cut off, report. The reported input carries the deposition of
# both land uses' final emissions. Uptake is uptake at yield potential where
# the cut-off applies, else current NUE times the input at the deposition
# before any cut-off (SI Eq. 5); surplus is input minus uptake (SI Eq. 6).
.critn_finish <- function(p, x, threshold) {
  floor_ara <- .critn_present(p$area_arable_ha, pmax(x$x_ara, 0))
  floor_igl <- .critn_present(p$area_intensive_ha, pmax(x$x_igl, 0))
  pre_cut <- p$emission_fixed + p$c_ara * floor_ara + p$c_igl * floor_igl
  cut_ara <- .critn_reaches_potential(p, "ara", floor_ara, pre_cut)
  cut_igl <- .critn_reaches_potential(p, "igl", floor_igl, pre_cut)
  final_ara <- dplyr::if_else(cut_ara, p$x_max_ara, floor_ara)
  final_igl <- dplyr::if_else(cut_igl, p$x_max_igl, floor_igl)
  emission <- p$emission_fixed + p$c_ara * final_ara + p$c_igl * final_igl
  deposition <- list(final = emission, pre_cut = pre_cut)
  ara <- .critn_land_use_result(p, "ara", final_ara, cut_ara, deposition)
  igl <- .critn_land_use_result(p, "igl", final_igl, cut_igl, deposition)
  ara$rule <- .critn_rule(floor_ara, cut_ara)
  igl$rule <- .critn_rule(floor_igl, cut_igl)
  .critn_long(p, ara, igl, threshold)
}

# Whether a land use is cut off: its uptake at the critical input (current NUE
# times fertiliser plus manure, fixation and the deposition of both land uses
# before any cut-off) reaches its uptake at yield potential (Methods Eqs.
# 1-2). Recovered from the archive. Testing fertiliser plus manure against
# its own cut-off value instead misses two cases the deposited layers cut
# off: a mixed cell where the other land use's NH3 deposition lifts the
# input past the cut-off input, and NUE above 0.8, where uptake reaches
# yield potential below the cut-off input (uptake at yield potential over
# 0.8), which then raises the input to it.
.critn_reaches_potential <- function(p, land_use, x, deposition) {
  f <- p[[paste0("f_", land_use)]]
  fix <- p[[.critn_land_use_col(land_use, "fixation")]]
  uptake <- p[[paste0("nue_", land_use)]] * (x + fix + f * deposition)
  reached <- uptake >= p[[paste0("uptake_max_", land_use)]]
  reached[is.na(reached)] <- FALSE
  .critn_present(p[[.critn_land_use_col(land_use, "area")]], reached, FALSE)
}

.critn_rule <- function(floor, cut) {
  dplyr::case_when(
    cut ~ "yield_potential_cap",
    floor <= 0 ~ "non_agricultural_floor",
    .default = "environmental_threshold"
  )
}

# The input column holding a reducible land use's own quantity.
.critn_land_use_col <- function(land_use, quantity) {
  cols <- list(
    ara = c(
      fixation = "fixation_arable_kg",
      uptake = "uptake_arable_kg",
      area = "area_arable_ha"
    ),
    igl = c(
      fixation = "fixation_intensive_kg",
      uptake = "uptake_intensive_kg",
      area = "area_intensive_ha"
    )
  )
  cols[[land_use]][[quantity]]
}

# `deposition` holds the cell's final emission and its emission before any
# cut-off (see .critn_finish()).
.critn_land_use_result <- function(p, land_use, x, cut, deposition) {
  f <- p[[paste0("f_", land_use)]]
  fix <- p[[.critn_land_use_col(land_use, "fixation")]]
  nue <- p[[paste0("nue_", land_use)]]
  input <- x + fix + f * deposition$final
  uptake <- dplyr::if_else(
    cut,
    p[[paste0("uptake_max_", land_use)]],
    nue * (x + fix + f * deposition$pre_cut)
  )
  current <- p[[paste0("input_", land_use)]]
  current_uptake <- p[[.critn_land_use_col(land_use, "uptake")]]
  area <- p[[.critn_land_use_col(land_use, "area")]]
  # No allowance without current fertiliser plus manure, the NH3 it emits
  # (SI Eq. 10 divides by it; the archive leaves all 56 such arable cells
  # empty in every layer), crop uptake or agricultural leaching. Nor where
  # the deposition is undefined: the other land use of a mixed cell has
  # fertiliser and manure but no uptake, so no NUE and no cut-off (archive
  # cell 72198, empty for both land uses in every layer).
  defined <- area > 0 &
    p[[paste0("x_", land_use)]] > 0 &
    p[[paste0("c_", land_use)]] > 0 &
    current_uptake > 0 &
    p$leaching_ag_kg > 0 &
    is.finite(deposition$final)
  defined[is.na(defined)] <- FALSE
  list(
    defined = defined,
    area = area,
    input = dplyr::if_else(defined, input, NA_real_),
    uptake = dplyr::if_else(defined, uptake, NA_real_),
    current = dplyr::if_else(defined, current, NA_real_),
    current_uptake = dplyr::if_else(defined, current_uptake, NA_real_)
  )
}

# One row per cell and land-use scope with a critical value. `"all"` sums the
# two land uses where each has one; a cell holding only one of them carries
# that one's value.
.critn_long <- function(p, ara, igl, threshold) {
  all <- .critn_all_scope(ara, igl)
  purrr::imap(
    list(ara = ara, igl = igl, all = all),
    \(lu, scope) {
      tibble::tibble(
        cell_id = p$cell_id,
        lon = p$lon,
        lat = p$lat,
        image_region = p$image_region,
        critical_threshold = threshold,
        critical_land_use = scope,
        area_ha = lu$area,
        critical_n_input_kgn_ha = lu$input / lu$area,
        critical_n_surplus_kgn_ha = (lu$input - lu$uptake) / lu$area,
        current_n_input_kgn_ha = lu$current / lu$area,
        current_n_surplus_kgn_ha = (lu$current - lu$current_uptake) / lu$area,
        critical_rule = lu$rule
      ) |>
        dplyr::filter(lu$defined)
    }
  ) |>
    purrr::list_rbind()
}

.critn_all_scope <- function(ara, igl) {
  area <- .critn_add(
    dplyr::if_else(ara$defined, ara$area, NA_real_),
    dplyr::if_else(igl$defined, igl$area, NA_real_)
  )
  list(
    defined = ara$defined | igl$defined,
    area = area,
    input = .critn_add(ara$input, igl$input),
    uptake = .critn_add(ara$uptake, igl$uptake),
    current = .critn_add(ara$current, igl$current),
    current_uptake = .critn_add(ara$current_uptake, igl$current_uptake),
    rule = NA_character_
  )
}

# The sum over the two land uses of a quantity undefined where a land use
# has no critical value, which then contributes nothing.
.critn_add <- function(a, b) {
  dplyr::coalesce(a, 0) + dplyr::coalesce(b, 0)
}

# A land use absent from a cell receives nothing and is never cut off, so its
# undefined ratios cannot reach the deposition the other land use receives.
.critn_present <- function(area, value, absent = 0) {
  present <- !is.na(area) & area > 0
  dplyr::if_else(present, value, absent)
}

# Division that treats an undefined ratio of two zero flows as zero.
.critn_div <- function(num, den) {
  out <- num / den
  out[!is.finite(out)] <- 0
  out
}
