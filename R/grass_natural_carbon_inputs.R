# nolint start: object_length_linter.
# Grassland and natural-land soil carbon inputs from the LPJmL run (Module B,
# Task B2c-2). The carbon returned to soil under grassland and natural
# vegetation is the LPJmL net primary production minus harvested carbon,
# floored at zero, per the RESOLVED phase-2C spec (F1/F2). This is the
# per-land-use-class counterpart of build_soil_carbon_inputs() (which handles
# cropland per crop). The two feed the c_inputs contract of
# build_carbon_balance().
#
# PER-STAND VS PER-CELL (resolved empirically, not guessed): pft_npp values are
# PER PFT STAND (gC per m2 of the PFT's own stand), verified by reconstructing
# mnpp.nc's annual total as natfrac*sum(natural pft NPP) + sum(cftfrac*cft NPP)
# to within 0.1% at sampled land cells (the naive 43-band sum overshoots
# 2-20x). The per-stand density IS the per-hectare-of-that-land-use value the
# carbon balance needs, so:
#   - natural: the 14 natural PFTs coexist in one natural stand, so their
#     per-m2 densities ADD -> sum over bands 1-14 (they are not harvested).
#   - grassland: rainfed and irrigated grassland are SEPARATE stands, so the
#     per-hectare-of-grassland density is the stand-area-weighted mean of their
#     net (NPP - harvest) densities (weights from the cftfrac stand fractions).
# No litterfall coefficient is applied: NPP minus harvest IS the mass returned.

#' Build grassland and natural-land soil carbon inputs from LPJmL.
#'
#' @description
#' Assemble the carbon returned to soil under grassland and natural vegetation
#' as the layer the soil-organic-carbon turnover models consume. The
#' LPJmL-derived net carbon density is read from the pinned
#' `lpjml-grass-natural-net-c` artifact by default, so running LPJmL is not a
#' prerequisite; pass `run_dir` (or set `WHEP_LPJML_RUN_DIR`) to derive it from
#' a finished local run instead, or `data$net_c` to supply it directly. The
#' pin holds only LPJmL-derived quantities: the grazing excreta, both
#' humification fractions and the polity attachment are always computed here,
#' so they never differ between the pinned and the run-derived path.
#' The class carbon input is the net primary production
#' minus harvested carbon (both per-plant-functional-type, [read_lpjml_npp()]),
#' floored at zero and converted to megagrams of carbon per hectare per year
#' (1 gC/m2 = 0.01 MgC/ha). Natural land sums the eleven natural
#' plant-functional-types (they coexist in one stand); grassland takes the
#' stand-area-weighted mean of the rainfed and irrigated grassland net inputs
#' and adds the grazing-excreta carbon from
#' [build_livestock_nutrient_flows()]'s `applied` stream spread uniformly over
#' the polity's grassland area. The humification fraction is the
#' spontaneous-grass value for grassland and the woody-residue value for
#' natural land (both from [residue_humification]).
#'
#' # Which grassland hectares the grazing excreta is divided by
#'
#' The excreta carbon is a polity total, so it becomes a per-hectare density by
#' division, and `excreta_area_basis` picks the hectares to divide by. It
#' matters because the density is charged only to cells where LPJmL wrote a
#' grassland stand, while the divisor has always been the polity's **whole**
#' LUH2 grassland area, and [build_carbon_balance()] then multiplies the
#' density by the LUH2 grassland area of each charged cell. Whatever the
#' uncharged hectares would have received is therefore lost.
#'
#' Measured on the `lpjml-grass-natural-net-c` pin against
#' [read_luh2_landuse()], the cells with a grassland stand hold 98.6% of the
#' LUH2 grassland area at 2010 (98.4% at 1960, 98.6% at 2020), leaving 46.6 Mha
#' uncharged at 2010. How much excreta carbon that loses depends on where the
#' herds are: 1.4% if excreta is proportional to grassland area, 13.8% under an
#' equal-carbon-per-polity probe, because the shortfall is concentrated in
#' small polities — 101 of 188 lose more than 1%, 53 more than 10%, 7 more than
#' half, and 13 (islands and city states, each under 0.01 Mha of grassland)
#' have no grassland stand at all and lose everything. The reverse gap is
#' negligible: 6 of 42,391 grassland cell-rows at 2010 have no LUH2 grassland
#' row.
#'
#' The three bases are alternatives, not fallbacks, and the chosen one is
#' recorded in `method_excreta_area`:
#'
#' - `"luh2_grassland"` (default, the published behaviour): divide by the
#'   polity's whole LUH2 grassland area. Does **not** conserve the polity's
#'   excreta carbon whenever a grassland hectare carries no LPJmL stand.
#' - `"charged_grassland"`: divide by the LUH2 grassland area of the cells the
#'   density is actually charged to, so every polity that has a grassland stand
#'   keeps its whole excreta carbon by construction. Raises the density on
#'   those cells by the reciprocal of the coverage above (1.4% globally on an
#'   area weighting, up to 17x in the worst measured polity) and leaves the
#'   uncovered hectares at zero, as they already are for net primary
#'   production. A polity with no grassland stand anywhere still loses all of
#'   its excreta — there is nowhere to charge it — which on the probe above is
#'   13 polities and 6.9% of the carbon.
#' - `"luh2_all_grassland"`: keep the whole-area divisor and emit a grassland
#'   row for every LUH2 grassland cell instead, carrying zero net primary
#'   production where LPJmL has no stand (4,117 extra rows at 2010, 8.8% more
#'   grassland rows). The only basis that conserves the excreta carbon
#'   globally, and it keeps the original spatial spread, at the cost of
#'   grassland rows in cells the LPJmL run does not simulate as grassland.
#'
#' Dividing by the LPJmL grassland **stand** area is deliberately not offered.
#' It would conserve nothing under the area basis [build_carbon_balance()]
#' actually uses, and it would buy little: measured at 1901 the stand area and
#' the LUH2 grassland area agree to a median 0.1% per cell (95.5% of shared
#' cells within 10%, global totals 1560.5 against 1576.0 Mha), because WHEP's
#' LPJmL land-use forcing is itself LUH2-derived.
#'
#' @param resolution `"grid"` (default, per cell and class) or `"polity"`
#'   (aggregated to `area_code`, area-weighting the per-hectare densities).
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the inputs cover. Threaded into the default
#'   LPJmL NPP, stand-fraction and land-use readers so they slice to the
#'   requested years; ignored for inputs supplied via `data`.
#' @param run_dir Path to a finished LPJmL run output directory holding
#'   `pft_npp.nc`, `pft_harvestc.nc` and `cftfrac.nc` (the `scenario_*` output
#'   folder). `NULL` (default) uses `WHEP_LPJML_RUN_DIR` when set, and the
#'   pinned artifact otherwise.
#' @param data Named list of pre-loaded inputs, each falling back to its reader
#'   when absent: `net_c` (the LPJmL net carbon density, `lon`, `lat`, `year`,
#'   `land_use`, `npp_c_mgc_ha_yr`; takes precedence over both `run_dir` and
#'   the pin); `npp` and `harvestc` (per cell, PFT and year, the
#'   [read_lpjml_npp()] output); `stand_frac` (per cell, year and PFT name the
#'   managed-grassland stand fractions with columns `lon`, `lat`, `year`,
#'   `name_pft`, `stand_frac`). Supplying all three of `npp`, `harvestc` and
#'   `stand_frac` derives `net_c` without needing a run directory or the pin.
#'   Also: `country_grid`, the polycell support resolved to one row per cell and
#'   `area_code` (`lon`, `lat`, `area_code`, `cell_area_frac`), refused when a
#'   cell-`area_code` group is duplicated or `NA` (DA-23);
#'   `land_use` (per-cell class `area_ha`, used to spread
#'   excreta and to area-weight polity output); `excreta` (the `applied` tibble
#'   of [build_livestock_nutrient_flows()], grassland rows carry `applied_c`
#'   tonnes C); `residue_humification` (defaults to [residue_humification]).
#' @param excreta_area_basis Which grassland hectares the polity's grazing
#'   excreta carbon is divided by: `"luh2_grassland"` (default),
#'   `"charged_grassland"` or `"luh2_all_grassland"`. See the section below;
#'   the choice is recorded in `method_excreta_area`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
#'   resolution (or `(area_code, year, land_use)` at `"polity"`), with
#'   `c_input_mgc_ha_yr`, `humified_fraction`, `method_c_input` and
#'   `method_excreta_area`, for `land_use` in `"grassland"` and `"natural"`,
#'   plus the polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @source LPJmL run net primary production and harvested carbon; grassland and
#'   natural carbon inputs per the WHEP historical carbon-balance design.
#' @export
#' @examples
#' build_grass_natural_carbon_inputs(example = TRUE)
build_grass_natural_carbon_inputs <- function(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  run_dir = NULL,
  excreta_area_basis = c(
    "luh2_grassland",
    "charged_grassland",
    "luh2_all_grassland"
  ),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  basis <- rlang::arg_match(excreta_area_basis)
  if (isTRUE(example)) {
    return(.example_grass_natural_carbon_inputs())
  }
  d <- .gn_resolve_inputs(data, years, run_dir)
  natural <- .gn_natural_input(d)
  grassland <- .gn_grassland_input(d, basis)
  dplyr::bind_rows(natural, grassland) |>
    dplyr::mutate(method_excreta_area = basis) |>
    .gn_finalise(resolution, d$land_use) |>
    .add_reporting_polity_columns()
}

# -- Input resolution ---------------------------------------------------------

# `excreta` alone has no reader fallback: the `applied` stream is a livestock
# pipeline output, not a readable input, so the grazing-excreta term is live
# only when a caller injects it. Nothing in the package does -- neither
# build_carbon_inputs() nor build_carbon_balance() passes one -- so the
# excreta-area choice below is dormant on the in-repo pipelines and reaches a
# published number only through a caller that supplies the stream.
.gn_resolve_inputs <- function(data, years = NULL, run_dir = NULL) {
  list(
    net_c = .gn_resolve_net_c(data, years, run_dir),
    country_grid = data$country_grid %||% .gn_read_country_grid(),
    land_use = data$land_use %||% .gn_read_land_use(years),
    excreta = data$excreta,
    residue_humification = data$residue_humification %||%
      whep::residue_humification
  )
}

# The pin seam. Everything that needs the LPJmL run -- the three NetCDF reads
# and the per-stand PFT algebra -- lives behind this one helper, and its output
# schema IS the pinned artifact's schema. That is deliberate: the pin then
# carries only LPJmL-derived quantities, so nothing downstream (the grazing
# excreta, either humification fraction, the polity attachment) is silently
# baked into it. A caller-supplied table always wins; a resolvable run
# directory is read next; the pin is the default so a user who has never run
# LPJmL still gets the real layer.
.gn_resolve_net_c <- function(data, years, run_dir = NULL) {
  if (!is.null(data$net_c)) {
    return(.gn_check_net_c(data$net_c, "data$net_c"))
  }
  if (.gn_can_read_run(data, run_dir)) {
    return(.gn_net_c_from_lpjml(data, years, run_dir))
  }
  .read_lpjml_pin(.gn_net_c_alias()) |>
    .gn_check_net_c(.gn_net_c_alias()) |>
    .filter_years_if_present(years)
}

# ESTABLISHED 2026-08-18 (whep#807): the shipped pin was built from the 46-band
# LPJmL 6.1.1 run but with the eleven-name list, so it is MISSING the three
# natural PFTs added below. Proven by reading the pin and the run side by side at
# 2010: the pin is identical to the eleven-band sum over all 58,795 natural cells
# (max absolute difference 0) and differs from the fourteen-band sum by 0.48
# MgC/ha on average, up to 16.25. So the pin path returns numbers ~7% lower than
# the run path until the pin is regenerated, which is deferred to the next LPJmL
# run so all four LPJmL-derived pins are refreshed from one model version.
.gn_net_c_alias <- function() {
  "lpjml-grass-natural-net-c"
}

# A run is readable when the caller passed a run directory, the shared env var
# is set, or every per-PFT input was supplied directly (the injected-data path
# the tests use, which needs no run at all).
.gn_can_read_run <- function(data, run_dir) {
  supplied <- !is.null(data$npp) &&
    !is.null(data$harvestc) &&
    !is.null(data$stand_frac)
  supplied || .has_path(run_dir) || .has_path(Sys.getenv("WHEP_LPJML_RUN_DIR"))
}

.gn_check_net_c <- function(x, source) {
  .check_columns(
    x,
    c("lon", "lat", "year", "land_use", "npp_c_mgc_ha_yr"),
    source
  )
  tibble::as_tibble(x)
}

# Derive the net carbon density per cell, year and land-use class from the run:
# natural sums the fourteen natural PFT densities (one shared stand, never
# harvested); grassland takes the stand-area-weighted mean of the rainfed and
# irrigated net (NPP - harvest) densities.
.gn_net_c_from_lpjml <- function(data, years, run_dir = NULL) {
  npp <- data$npp %||% read_lpjml_npp("npp", years = years, run_dir = run_dir)
  harvestc <- data$harvestc %||%
    read_lpjml_npp("harvestc", years = years, run_dir = run_dir)
  stand_frac <- data$stand_frac %||%
    .gn_read_stand_frac(run_dir = run_dir, years = years)
  .gn_check_natural_pfts(npp)
  natural <- npp |>
    dplyr::filter(.data$name_pft %in% .gn_natural_pfts()) |>
    dplyr::summarise(
      npp_c_mgc_ha_yr = sum(pmax(.data$value, 0)) * 0.01,
      .by = c("lon", "lat", "year")
    ) |>
    dplyr::mutate(land_use = "natural")
  grassland <- .gn_grassland_net(npp, harvestc, stand_frac) |>
    dplyr::mutate(land_use = "grassland")
  dplyr::bind_rows(natural, grassland) |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "land_use",
      "npp_c_mgc_ha_yr"
    )
}

.gn_net_c_class <- function(net_c, class) {
  net_c |>
    dplyr::filter(.data$land_use == class) |>
    dplyr::select("lon", "lat", "year", "npp_c_mgc_ha_yr")
}

# The natural-land PFT names (natural stand: trees plus the natural grasses and
# moss), matching pft_npp.nc bands 1-14 in LPJmL 6.x. Joined by name, never band
# index, because pft_npp.nc and pft_harvestc.nc order their bands differently.
#
# LPJmL 5.x had eleven; 6.x adds the flood-tolerant tropical tree, the
# flood-tolerant C3 graminoid and Sphagnum moss. Measured on
# global_1901-2023_spinup_300_our_inputs_lpjml611, the three carry 7.30% of
# natural net primary production at 2010 (7.85 of 107.53 PgC) in 35,639 of
# 56,008 land cells, rising from 3.31% in 1901 to 8.92% in 2023.
.gn_natural_pfts <- function() {
  c(
    "tropical broadleaved evergreen tree",
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
    "tropical broadleaved evergreen tree floodtolerant",
    "C3 graminoid flood tolerant",
    "Sphagnum moss"
  )
}

.gn_grassland_pfts <- function() {
  c("rainfed grassland", "irrigated grassland")
}

# The natural PFT bands a per-PFT LPJmL file actually carries. LPJmL writes the
# natural PFTs first and every managed band is prefixed "rainfed " or
# "irrigated ", so the natural block is whatever precedes the first managed
# band. Used only to compare the file against .gn_natural_pfts().
.gn_natural_bands_present <- function(npp) {
  names_in_order <- npp |>
    dplyr::distinct(.data$npft, .data$name_pft) |>
    dplyr::arrange(.data$npft) |>
    dplyr::pull("name_pft")
  managed <- stringr::str_detect(
    names_in_order,
    "^(rainfed|irrigated) "
  )
  if (!any(managed)) {
    return(names_in_order)
  }
  names_in_order[seq_len(which(managed)[1] - 1L)]
}

# Guard the name-keyed natural-PFT selection against an LPJmL run whose natural
# PFT set is wider than .gn_natural_pfts(). The selection is a `%in%` filter, so
# a band the run writes but the list omits contributes zero carbon with no error
# and no warning. That is how 6.x's three additions went unnoticed after the 5.x
# list stopped matching the run (whep#400); the list now covers 6.x, and this
# guard is what makes the next such divergence visible instead of silent.
# Warns rather than aborts so a newer LPJmL still runs, just loudly.
#
# Deliberately does NOT check the reverse direction. A caller may legitimately
# pass a subset of bands (every test fixture does), so a name in the list with
# no matching band is not evidence of a rename at this seam.
.gn_check_natural_pfts <- function(npp) {
  extra <- setdiff(.gn_natural_bands_present(npp), .gn_natural_pfts())
  if (length(extra) > 0) {
    cli::cli_warn(c(
      "Ignoring {length(extra)} natural PFT
       band{cli::qty(length(extra))}{?s} the LPJmL run writes but
       {.fun .gn_natural_pfts} does not list: {.val {extra}}.",
      i = "Their net primary production is excluded from the natural-land
           carbon input."
    ))
  }
  invisible(npp)
}

# -- Natural-land carbon input ------------------------------------------------

# Natural land is not harvested, so its input is the sum of the natural PFT
# NPP densities (they coexist in one stand), converted to MgC/ha.
.gn_natural_input <- function(d) {
  hf <- .gn_humified(d$residue_humification, "woody_residue")
  .gn_net_c_class(d$net_c, "natural") |>
    dplyr::rename(c_input_mgc_ha_yr = "npp_c_mgc_ha_yr") |>
    .gn_attach_polity(d$country_grid) |>
    dplyr::mutate(
      land_use = "natural",
      humified_fraction = hf,
      method_c_input = "lpjml_npp_minus_harvest"
    )
}

# -- Grassland carbon input ---------------------------------------------------

# Grassland input: the stand-area-weighted mean of the rainfed and irrigated
# grassland net (NPP - harvest) densities, plus the grazing-excreta density.
.gn_grassland_input <- function(d, basis = "luh2_grassland") {
  # Grass litter (weed coefficient) and grazing excreta (excreta coefficient,
  # ~2.2x higher) humify differently; carbon-weight the two so each stream keeps
  # its own humification fraction, matching the crop path (.sci_humified_fraction).
  hf_npp <- .gn_humified(d$residue_humification, "weed")
  hf_excreta <- .gn_humified(d$residue_humification, "excreta")
  net <- .gn_net_c_class(d$net_c, "grassland") |>
    .gn_attach_polity(d$country_grid) |>
    .gn_extend_to_luh2(d$land_use, d$excreta, basis)
  excreta <- .gn_excreta_density(
    d$excreta,
    d$land_use,
    d$country_grid,
    net,
    basis
  )
  net |>
    dplyr::left_join(excreta, by = c("area_code", "year")) |>
    dplyr::mutate(
      npp_c = .data$npp_c_mgc_ha_yr,
      excreta_c = dplyr::coalesce(.data$excreta_c_mgc_ha_yr, 0),
      c_input_mgc_ha_yr = .data$npp_c + .data$excreta_c,
      humified_fraction = dplyr::if_else(
        .data$c_input_mgc_ha_yr > 0,
        (.data$npp_c * hf_npp + .data$excreta_c * hf_excreta) /
          .data$c_input_mgc_ha_yr,
        hf_npp
      ),
      land_use = "grassland",
      method_c_input = "lpjml_npp_minus_harvest"
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "land_use",
      "c_input_mgc_ha_yr",
      "humified_fraction",
      "method_c_input"
    )
}

# Per-cell grassland NPP density (MgC/ha): net (NPP - harvest) per grassland
# stand, floored at zero, area-weighted over the rainfed/irrigated stands by
# their stand fractions. A grassland stand absent from the cell (no stand
# fraction) gets zero weight, so a nominal zero-NPP band does not dilute the
# productive stand's density; when no stand carries a fraction the density is a
# simple mean (see .gn_wmean).
.gn_grassland_net <- function(npp, harvestc, stand_frac) {
  npp_g <- .gn_grassland_pft_values(npp, "value")
  harv_g <- .gn_grassland_pft_values(harvestc, "harvest")
  npp_g |>
    dplyr::left_join(
      harv_g,
      by = c("lon", "lat", "year", "name_pft")
    ) |>
    dplyr::mutate(
      net_c = pmax(.data$value - dplyr::coalesce(.data$harvest, 0), 0)
    ) |>
    dplyr::left_join(
      stand_frac,
      by = c("lon", "lat", "year", "name_pft")
    ) |>
    dplyr::mutate(stand_frac = dplyr::coalesce(.data$stand_frac, 0)) |>
    # A band absent from the cell (no stand fraction and no production) is not a
    # real stand; drop it so it neither weights nor mean-dilutes the density.
    dplyr::filter(.data$stand_frac > 0 | .data$net_c > 0) |>
    dplyr::summarise(
      npp_c_mgc_ha_yr = .gn_wmean(.data$net_c, .data$stand_frac) * 0.01,
      .by = c("lon", "lat", "year")
    )
}

.gn_grassland_pft_values <- function(x, out_name) {
  x |>
    dplyr::filter(.data$name_pft %in% .gn_grassland_pfts()) |>
    dplyr::select("lon", "lat", "year", "name_pft", "value") |>
    dplyr::rename(!!out_name := "value")
}

# Grazing-excreta carbon density (MgC/ha of grassland), uniform per polity:
# total applied grazing-excreta carbon (tonnes C = MgC) over grassland area.
# WHICH grassland area is `excreta_area_basis` (see the exported function's
# documentation section): the polity's whole LUH2 grassland area, or only the
# hectares the density is charged to. `charged` is the grassland cell table the
# density will be joined onto, which is what makes the difference measurable
# here rather than downstream. Returns zero density when no excreta is injected.
.gn_excreta_density <- function(
  excreta,
  land_use,
  country_grid,
  charged = NULL,
  basis = "luh2_grassland"
) {
  if (is.null(excreta)) {
    return(tibble::tibble(
      area_code = integer(),
      year = integer(),
      excreta_c_mgc_ha_yr = numeric()
    ))
  }
  grass_c <- .gn_grass_excreta_mass(excreta)
  total_area <- .gn_grass_area(land_use, country_grid)
  charged_area <- .gn_grass_area(land_use, country_grid, charged)
  .gn_report_excreta_area(grass_c, total_area, charged_area, basis)
  grass_area <- if (basis == "charged_grassland") charged_area else total_area
  grass_c |>
    dplyr::inner_join(grass_area, by = c("area_code", "year")) |>
    dplyr::mutate(
      excreta_c_mgc_ha_yr = dplyr::if_else(
        .data$grass_area_ha > 0,
        .data$excreta_c_mg / .data$grass_area_ha,
        0
      )
    ) |>
    dplyr::select("area_code", "year", "excreta_c_mgc_ha_yr") |>
    .gn_check_excreta_mass(grass_c, charged_area, basis)
}

# Add a grassland row for every LUH2 grassland cell the LPJmL run has no
# grassland stand in, carrying zero net primary production, so the excreta
# density divided by the whole LUH2 grassland area is charged to all of it.
# Only for `excreta_area_basis = "luh2_all_grassland"`, and only when excreta is
# actually supplied: without it these rows would add nothing but zeroes.
.gn_extend_to_luh2 <- function(net, land_use, excreta, basis) {
  if (basis != "luh2_all_grassland" || is.null(excreta)) {
    return(net)
  }
  land_use |>
    dplyr::filter(
      stringr::str_to_lower(.data$land_use) == "grassland",
      .data$area_ha > 0
    ) |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2)) |>
    dplyr::distinct(.data$lon, .data$lat, .data$area_code, .data$year) |>
    dplyr::anti_join(net, by = c("lon", "lat", "area_code", "year")) |>
    dplyr::mutate(npp_c_mgc_ha_yr = 0) |>
    dplyr::bind_rows(net)
}

# Report the grassland hectares in the LUH2 basis but not in the charged set --
# the quantity the divisor choice turns into either lost carbon or a raised
# density. Silent when the two agree, which is what a fully covered polity set
# (and every hand-built fixture) looks like.
.gn_report_excreta_area <- function(grass_c, total_area, charged_area, basis) {
  gap <- total_area |>
    dplyr::inner_join(grass_c, by = c("area_code", "year")) |>
    dplyr::left_join(
      dplyr::rename(charged_area, charged_ha = "grass_area_ha"),
      by = c("area_code", "year")
    ) |>
    dplyr::mutate(charged_ha = dplyr::coalesce(.data$charged_ha, 0)) |>
    dplyr::filter(.data$charged_ha < .data$grass_area_ha)
  if (nrow(gap) == 0) {
    return(invisible(gap))
  }
  lost <- sum(gap$excreta_c_mg * (1 - gap$charged_ha / gap$grass_area_ha))
  share <- signif(100 * lost / sum(grass_c$excreta_c_mg), 3)
  uncovered <- signif(sum(gap$grass_area_ha - gap$charged_ha), 3)
  lost <- signif(lost, 3)
  cli::cli_warn(c(
    "!" = "{nrow(gap)} polity-year{?s} hold {uncovered} ha of LUH2 grassland
      with no LPJmL grassland stand.",
    i = if (basis == "luh2_grassland") {
      "{.val {basis}} divides by those hectares too, so {lost} MgC
       ({share}%) of the grazing excreta carbon is charged to nothing."
    } else {
      "{.val {basis}} charges their {lost} MgC ({share}%) of grazing excreta
       carbon to the covered hectares instead."
    }
  ))
  invisible(gap)
}

# The conservation assertion the two mass-conserving bases must satisfy: the
# carbon the charged hectares receive equals the polity's grazing excreta
# carbon. A relative tolerance, because the density is a division; polities
# whose grassland area is zero in the charged set legitimately receive nothing
# and are excluded (they have nowhere to put it).
.gn_check_excreta_mass <- function(density, grass_c, charged_area, basis) {
  if (basis == "luh2_grassland") {
    return(density)
  }
  bad <- density |>
    dplyr::inner_join(grass_c, by = c("area_code", "year")) |>
    dplyr::inner_join(charged_area, by = c("area_code", "year")) |>
    dplyr::filter(.data$grass_area_ha > 0) |>
    dplyr::mutate(
      charged_c_mg = .data$excreta_c_mgc_ha_yr * .data$grass_area_ha
    ) |>
    dplyr::filter(
      abs(.data$charged_c_mg - .data$excreta_c_mg) >
        1e-8 * pmax(abs(.data$excreta_c_mg), 1)
    )
  if (nrow(bad) > 0) {
    cli::cli_abort(c(
      "{.val {basis}} must charge every polity its whole grazing excreta
       carbon, but {nrow(bad)} polity-year{?s} do{?es/} not.",
      i = "Worst: {.val {bad$area_code[1]}} at {.val {bad$year[1]}},
           {round(bad$charged_c_mg[1], 3)} against
           {round(bad$excreta_c_mg[1], 3)} MgC."
    ))
  }
  density
}

# Total grazing-excreta carbon (MgC) applied to grassland per polity-year. The
# applied stream carries land_use "Grassland" and territory on the area_code key.
.gn_grass_excreta_mass <- function(excreta) {
  excreta |>
    dplyr::filter(.data$land_use == "Grassland") |>
    dplyr::summarise(
      excreta_c_mg = sum(.data$applied_c, na.rm = TRUE),
      .by = c("year", "territory")
    ) |>
    dplyr::transmute(
      area_code = .manure_territory_to_area_code(.data$territory),
      year = as.integer(.data$year),
      excreta_c_mg = .data$excreta_c_mg
    )
}

# Total grassland area (ha) per polity-year from the land-use layer. The
# read_luh2_landuse() grid contract has already scaled area_ha by the cell's
# polity fraction, so the country-grid join only validates the compartment and
# must not apply that fraction a second time. `cells`, when given, restricts the
# sum to those (lon, lat, area_code, year) compartments -- the grassland cells
# the excreta density is charged to, as opposed to every grassland hectare LUH2
# reports.
.gn_grass_area <- function(land_use, country_grid, cells = NULL) {
  cg <- .normalize_carbon_support(country_grid) |>
    dplyr::select("lon", "lat", "area_code")
  grass <- land_use |>
    dplyr::filter(stringr::str_to_lower(.data$land_use) == "grassland") |>
    dplyr::inner_join(cg, by = c("lon", "lat", "area_code")) |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2))
  if (!is.null(cells)) {
    keys <- c("lon", "lat", "area_code", "year")
    grass <- dplyr::semi_join(grass, dplyr::distinct(cells[keys]), by = keys)
  }
  dplyr::summarise(
    grass,
    grass_area_ha = sum(.data$area_ha),
    .by = c("area_code", "year")
  )
}

# -- Shared helpers -----------------------------------------------------------

# Attach the overlapping polities to each cell via the country grid; a border
# cell keeps every polity it overlaps.
.gn_attach_polity <- function(cells, country_grid) {
  cg <- .normalize_carbon_support(country_grid) |>
    dplyr::select("lon", "lat", "area_code")
  cells |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2)) |>
    dplyr::inner_join(
      dplyr::mutate(cg, lon = round(.data$lon, 2), lat = round(.data$lat, 2)),
      by = c("lon", "lat")
    )
}

.gn_humified <- function(residue_humification, input_type) {
  v <- residue_humification$humified_fraction[
    residue_humification$input_type == input_type
  ]
  if (length(v) != 1L) {
    cli::cli_abort(
      "{.field residue_humification} needs one {.val {input_type}} row."
    )
  }
  v
}

.gn_wmean <- function(value, weight) {
  if (sum(weight) == 0) {
    return(mean(value))
  }
  sum(value * weight) / sum(weight)
}

# Grid output keeps the per-cell per-class rows; polity output aggregates to
# (area_code, year, land_use) area-weighting the per-hectare densities by the
# matching land-use area so total carbon mass is conserved. A class absent from
# the supplied land-use layer retains the historical plain-mean fallback.
.gn_finalise <- function(x, resolution, land_use) {
  if (resolution == "grid") {
    return(tibble::as_tibble(x))
  }
  class_area <- land_use |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      land_use = stringr::str_to_lower(.data$land_use)
    ) |>
    dplyr::summarise(
      class_area_ha = sum(.data$area_ha),
      .by = c("lon", "lat", "area_code", "year", "land_use")
    )
  x |>
    dplyr::left_join(
      class_area,
      by = c("lon", "lat", "area_code", "year", "land_use")
    ) |>
    dplyr::mutate(class_area_ha = dplyr::coalesce(.data$class_area_ha, 0)) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = .gn_wmean(
        .data$c_input_mgc_ha_yr,
        .data$class_area_ha
      ),
      humified_fraction = .data$humified_fraction[1],
      method_c_input = .data$method_c_input[1],
      method_excreta_area = .data$method_excreta_area[1],
      .by = c("area_code", "year", "land_use")
    ) |>
    tibble::as_tibble()
}

# -- Default input readers ----------------------------------------------------

# Per-cell managed-grassland stand fractions from the LPJmL run's cftfrac.nc.
# The rainfed and irrigated grassland bands (matched by NamePFT, never band
# index) are sliced per year and reshaped to (lon, lat, year, name_pft,
# stand_frac), keeping only present (finite, positive) stands. The run
# directory follows the shared LPJmL convention (WHEP_LPJML_RUN_DIR).
.gn_read_stand_frac <- function(
  run_dir = NULL,
  years = NULL,
  first_year = 1901L
) {
  rlang::check_installed("ncdf4")
  run_dir <- .resolve_run_dir(run_dir)
  path <- file.path(run_dir, "cftfrac.nc")
  if (!file.exists(path)) {
    cli::cli_abort("LPJmL managed-fraction file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  keep <- .gn_stand_frac_keep_years(nc, first_year, years)
  parts <- purrr::map(keep, function(ti) {
    .gn_stand_frac_slice(nc, first_year, ti)
  })
  tibble::as_tibble(data.table::rbindlist(parts))
}

# Annual time-step indices to read from cftfrac.nc: all, or only the requested
# calendar years (index 1 = first_year).
.gn_stand_frac_keep_years <- function(nc, first_year, years) {
  n_time <- nc$dim[["time"]]$len
  stamp_years <- first_year + seq_len(n_time) - 1L
  if (is.null(years)) {
    seq_len(n_time)
  } else {
    which(stamp_years %in% as.integer(years))
  }
}

# One year's rainfed and irrigated grassland stand fractions, long by name_pft.
.gn_stand_frac_slice <- function(nc, first_year, time_index) {
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  names_pft <- as.character(ncdf4::ncvar_get(nc, "NamePFT"))
  bands <- which(names_pft %in% .gn_grassland_pfts())
  year <- first_year + time_index - 1L
  parts <- purrr::map(bands, function(k) {
    slab <- ncdf4::ncvar_get(
      nc,
      "CFTfrac",
      start = c(1L, 1L, k, time_index),
      count = c(-1L, -1L, 1L, 1L)
    )
    dt <- data.table::data.table(
      lon = rep(lon, times = length(lat)),
      lat = rep(lat, each = length(lon)),
      year = as.integer(year),
      name_pft = names_pft[k],
      stand_frac = as.vector(slab)
    )
    dt[is.finite(stand_frac) & stand_frac > 0]
  })
  data.table::rbindlist(parts)
}

# The carbon path's shared polycell support (see `.carbon_cell_support()` in
# R/carbon_balance.R), so the cells this path attaches polities to are exactly
# the cells `read_luh2_landuse()` measured its areas on.
.gn_read_country_grid <- function() {
  .carbon_cell_support()
}

# Per-cell grassland (and other class) areas from LUH2 v2h.
.gn_read_land_use <- function(years = NULL) {
  read_luh2_landuse(resolution = "grid", years = years)
}
# nolint end
