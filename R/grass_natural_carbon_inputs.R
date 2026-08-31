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
#' @param method_natural_c Which LPJmL quantity is natural land's carbon
#'   input. `"npp"` (default) is the natural PFTs' whole primary production;
#'   `"litterfall"` is `litfallc_nv`, what the model actually returns to the
#'   soil, excluding the increment retained in living biomass and what fire
#'   and land conversion remove. Litterfall is the physically correct soil
#'   input and runs 0.917 times production on near-pure natural cells at 2010
#'   (0.844 across all cells carrying natural vegetation), but it needs a run
#'   from 2026-08-27 or later, so the default stays `"npp"` while the
#'   published pin predates those outputs. Recorded in `method_c_input`.
#' @param method_natural_hf How natural land's humification fraction is set.
#'   `"woody_share"` (default) carbon-weights the [residue_humification]
#'   woody and herbaceous coefficients by the share of each cell-year's
#'   natural production that the woody PFTs made; `"woody"` applies the
#'   woody coefficient everywhere, which was the previous behaviour. Five of
#'   the fourteen natural PFTs are not woody and carry 28.1% of natural
#'   production at 2010. Falls back to `"woody"`, with a warning, when the
#'   net-carbon layer has no `woody_share` column.
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
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
#'   resolution (or `(area_code, year, land_use)` at `"polity"`), with
#'   `c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
#'   `land_use` in `"grassland"` and `"natural"`, plus the polity columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @source LPJmL run net primary production and harvested carbon; grassland and
#'   natural carbon inputs per the WHEP historical carbon-balance design.
#' @export
#' @examples
#' build_grass_natural_carbon_inputs(example = TRUE)
build_grass_natural_carbon_inputs <- function(
  resolution = c("grid", "polity"),
  method_natural_hf = c("woody_share", "woody"),
  method_natural_c = c("npp", "litterfall"),
  data = list(),
  years = NULL,
  run_dir = NULL,
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  method_natural_hf <- rlang::arg_match(method_natural_hf)
  method_natural_c <- rlang::arg_match(method_natural_c)
  if (isTRUE(example)) {
    return(.example_grass_natural_carbon_inputs())
  }
  d <- .gn_resolve_inputs(data, years, run_dir)
  natural <- .gn_natural_input(d, method_natural_hf, method_natural_c)
  grassland <- .gn_grassland_input(d)
  dplyr::bind_rows(natural, grassland) |>
    .gn_finalise(resolution, d$land_use) |>
    .add_reporting_polity_columns()
}

# -- Input resolution ---------------------------------------------------------

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

# RESOLVED 2026-08-25. This note previously said the shipped pin was an
# eleven-band sum missing three natural PFTs, and that the pin path therefore
# returned numbers ~7% below the run path (whep#807, established 2026-08-18).
# That was true of the pin as it stood then and is no longer true: the four
# LPJmL-derived pins were regenerated together from the 6.1.1 socn_diag run,
# and `lpjml-grass-natural-net-c` rose +7.12% at 2010 as the fix reached pin
# users (see NEWS).
#
# Re-measured 2026-08-25 against the run: the pin equals the FOURTEEN-band
# natural sum with a maximum absolute difference of 0 across 56,008 matched
# cells at 2010. Pin and run paths now agree exactly.
#
# Left in place rather than deleted because the stale version of this comment
# was itself cited as evidence that the pin is still eleven-band, so a reader
# arriving from that claim needs to find the correction here.
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
  # Whether this call is actually reading a run. A caller that injected npp
  # is not, so the litterfall layer must be injected too rather than fetched
  # from whatever WHEP_LPJML_RUN_DIR happens to point at -- which would make
  # an injected-data path, and every test using one, read a real NetCDF.
  read_run <- is.null(data$npp)
  npp <- data$npp %||% read_lpjml_npp("npp", years = years, run_dir = run_dir)
  harvestc <- data$harvestc %||%
    read_lpjml_npp("harvestc", years = years, run_dir = run_dir)
  stand_frac <- data$stand_frac %||%
    .gn_read_stand_frac(run_dir = run_dir, years = years)
  .gn_check_natural_pfts(npp)
  natural <- npp |>
    dplyr::filter(.data$name_pft %in% .gn_natural_pfts()) |>
    dplyr::mutate(woody = .data$name_pft %in% .gn_woody_natural_pfts()) |>
    dplyr::summarise(
      npp_c_mgc_ha_yr = sum(pmax(.data$value, 0)) * 0.01,
      woody_share = .gn_woody_share(pmax(.data$value, 0), .data$woody),
      .by = c("lon", "lat", "year")
    ) |>
    dplyr::mutate(land_use = "natural") |>
    .gn_attach_litterfall(data, years, run_dir, read_run)
  grassland <- .gn_grassland_net(npp, harvestc, stand_frac) |>
    dplyr::mutate(land_use = "grassland")
  dplyr::bind_rows(natural, grassland) |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "land_use",
      "npp_c_mgc_ha_yr",
      dplyr::any_of("litterfall_c_mgc_ha_yr"),
      dplyr::any_of("woody_share")
    )
}

# Per-STAND natural litterfall, attached when the run carries it.
#
# litfallc_nv is a whole-CELL density and pft_npp is per-stand, so the two
# only sit in one table after dividing by the natural stand fraction. That
# division is self-limiting on the 1750-2023 run -- the largest per-stand
# value is 44.5 MgC/ha/yr and it falls at stand fraction 0.74, not at a small
# one -- so no floor is imposed.
#
# Runs before 2026-08-27 wrote neither litfallc_nv nor fpc, so the column is
# simply absent there and `method_natural_c = "litterfall"` says so rather
# than quietly using production instead.
.gn_attach_litterfall <- function(
  natural,
  data,
  years,
  run_dir,
  read_run = TRUE
) {
  litterfall <- data$litterfall_nv
  cover <- data$natural_cover
  if (is.null(litterfall) || is.null(cover)) {
    if (!read_run || !.gn_has_litterfall_outputs(run_dir)) {
      return(natural)
    }
    litterfall <- litterfall %||%
      read_lpjml_litterfall("nv", years = years, run_dir = run_dir)
    cover <- cover %||%
      read_lpjml_natural_cover(years = years, run_dir = run_dir)
  }
  litterfall |>
    dplyr::inner_join(cover, by = c("lon", "lat", "year")) |>
    dplyr::filter(.data$natural_stand_frac > 0) |>
    dplyr::mutate(
      litterfall_c_mgc_ha_yr = .data$litterfall_c_mgc_ha_yr /
        .data$natural_stand_frac
    ) |>
    dplyr::select("lon", "lat", "year", "litterfall_c_mgc_ha_yr") |>
    dplyr::right_join(natural, by = c("lon", "lat", "year"))
}

# Both files, or neither: the per-cell to per-stand conversion needs the
# stand fraction, so litterfall without fpc is not usable.
.gn_has_litterfall_outputs <- function(run_dir) {
  dir <- tryCatch(.resolve_run_dir(run_dir), error = function(e) NULL)
  if (is.null(dir)) {
    return(FALSE)
  }
  all(file.exists(file.path(dir, c("litfallc_nv.nc", "fpc.nc"))))
}

# The nine natural PFTs that are woody. The remaining five of
# `.gn_natural_pfts()` -- three grasses, a flood-tolerant graminoid and
# Sphagnum moss -- are not, and they carry 28.1% of natural net primary
# production at 2010, so a single woody humification coefficient is applied
# to more than a quarter of a flux that is not wood.
.gn_woody_natural_pfts <- function() {
  c(
    "tropical broadleaved evergreen tree",
    "tropical broadleaved evergreen tree floodtolerant",
    "tropical broadleaved raingreen tree",
    "temperate needleleaved evergreen tree",
    "temperate broadleaved evergreen tree",
    "temperate broadleaved summergreen tree",
    "boreal needleleaved evergreen tree",
    "boreal broadleaved summergreen tree",
    "boreal needleleaved summergreen tree"
  )
}

# Woody share of a cell-year's natural production, by CARBON. Weighting by
# area instead would understate it, because forest out-produces the
# non-forest it would be weighted against. A cell with no production at all
# gets 1, which reproduces the previous woody constant rather than inventing
# a split for a stand that is not growing.
.gn_woody_share <- function(value, woody) {
  total <- sum(value)
  if (total <= 0) {
    return(1)
  }
  sum(value[woody]) / total
}

.gn_net_c_class <- function(net_c, class) {
  net_c |>
    dplyr::filter(.data$land_use == class) |>
    dplyr::select(
      "lon",
      "lat",
      "year",
      "npp_c_mgc_ha_yr",
      dplyr::any_of("litterfall_c_mgc_ha_yr"),
      dplyr::any_of("woody_share")
    )
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
.gn_natural_input <- function(
  d,
  method_natural_hf = "woody_share",
  method_natural_c = "npp"
) {
  hf_woody <- .gn_humified(d$residue_humification, "woody_residue")
  hf_herb <- .gn_humified(d$residue_humification, "weed")
  rows <- .gn_net_c_class(d$net_c, "natural")
  hf <- .gn_natural_hf(rows, method_natural_hf, hf_woody, hf_herb)
  column <- .gn_natural_c_column(rows, method_natural_c)
  rows |>
    dplyr::rename(c_input_mgc_ha_yr = dplyr::all_of(column)) |>
    dplyr::select(
      -dplyr::any_of(
        setdiff(c("npp_c_mgc_ha_yr", "litterfall_c_mgc_ha_yr"), column)
      )
    ) |>
    dplyr::mutate(humified_fraction = hf) |>
    dplyr::select(-dplyr::any_of("woody_share")) |>
    .gn_attach_polity(d$country_grid) |>
    dplyr::mutate(
      land_use = "natural",
      # Not "..._minus_harvest": `harvestc` is read for the grassland branch
      # only, and natural land is never harvested, so nothing is subtracted
      # here. The old label claimed a subtraction that does not happen, which
      # obscured that this class receives the whole of its PFTs' primary
      # production -- the term that sets its equilibrium (whep#799).
      method_c_input = paste0("lpjml_", method_natural_c)
    )
}

# Which column carries natural land's carbon input.
#
# `"npp"` is the whole of the natural PFTs' primary production.
# `"litterfall"` is what LPJmL actually returns to the soil, which is the
# physically right quantity: production also contains the increment that stays
# in living biomass, plus what fire and land conversion remove. Measured at
# 2010, litterfall is 0.917 times production on near-pure natural cells and
# 0.844 across all cells carrying natural vegetation, so the choice moves
# natural equilibrium carbon by that factor.
#
# Asking for litterfall when the layer has none aborts. Falling back to
# production would be a silent method substitution, and the two differ by far
# more than a rounding.
.gn_natural_c_column <- function(rows, method_natural_c) {
  if (identical(method_natural_c, "npp")) {
    return("npp_c_mgc_ha_yr")
  }
  if (!rlang::has_name(rows, "litterfall_c_mgc_ha_yr")) {
    cli::cli_abort(c(
      "The natural carbon layer carries no {.field litterfall_c_mgc_ha_yr}.",
      i = "Only runs from 2026-08-27 write {.val litfallc_nv} and {.val fpc}.",
      i = "Regenerate {.val lpjml-grass-natural-net-c} from such a run, pass",
      i = "{.arg run_dir}, or use {.code method_natural_c = 'npp'}."
    ))
  }
  "litterfall_c_mgc_ha_yr"
}
# The natural class's humification fraction.
#
# `"woody_share"` (default) carbon-weights the tabulated woody and
# herbaceous coefficients by the share of natural production the run's woody
# PFTs actually made, per cell and per year. HSOC's residence time is
# (1 - hf) / 0.48 + hf / 0.02, so this one scalar is the largest single lever
# on natural-land equilibrium carbon: 0.325 holds 17.7 years of input where
# the herbaceous 0.1153 holds 7.6. Carbon-weighted it becomes 0.266 at 2010
# (woody share 0.719), for 14.8 years and a 0.84x equilibrium.
#
# Sphagnum moss is grouped with the herbaceous PFTs because
# [residue_humification] has no peat coefficient. Peat stabilises carbon far
# more efficiently than grass, so that grouping errs low -- but moss is only
# 0.73% of natural production, so it cannot matter either way. Flagged rather
# than filled with an invented value.
#
# `"woody"` is the previous behaviour: the woody constant everywhere.
#
# A net-carbon layer with no `woody_share` column -- a pin built before this
# existed -- falls back to the constant and says so. Guessing a share would
# be worse than keeping a documented constant.
.gn_natural_hf <- function(rows, method_natural_hf, hf_woody, hf_herb) {
  if (identical(method_natural_hf, "woody")) {
    return(hf_woody)
  }
  if (!rlang::has_name(rows, "woody_share")) {
    cli::cli_warn(c(
      "!" = "The natural carbon layer carries no {.field woody_share}, so",
      " " = "the humification fraction stays at the woody constant",
      " " = "{.val {hf_woody}}.",
      "i" = "Regenerate {.val lpjml-grass-natural-net-c}, or supply",
      " " = "{.code data$net_c} from a run directory, to carbon-weight it."
    ))
    return(hf_woody)
  }
  share <- dplyr::coalesce(rows$woody_share, 1)
  share * hf_woody + (1 - share) * hf_herb
}
# -- Grassland carbon input ---------------------------------------------------

# Grassland input: the stand-area-weighted mean of the rainfed and irrigated
# grassland net (NPP - harvest) densities, plus the grazing-excreta density.
.gn_grassland_input <- function(d) {
  # Grass litter (weed coefficient) and grazing excreta (excreta coefficient,
  # ~2.2x higher) humify differently; carbon-weight the two so each stream keeps
  # its own humification fraction, matching the crop path (.sci_humified_fraction).
  hf_npp <- .gn_humified(d$residue_humification, "weed")
  hf_excreta <- .gn_humified(d$residue_humification, "excreta")
  net <- .gn_net_c_class(d$net_c, "grassland") |>
    .gn_attach_polity(d$country_grid)
  excreta <- .gn_excreta_density(d$excreta, d$land_use, d$country_grid)
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
# total applied grazing-excreta carbon (tonnes C = MgC) over the polity's total
# grassland area. Returns zero density when no excreta is injected.
.gn_excreta_density <- function(excreta, land_use, country_grid) {
  if (is.null(excreta)) {
    return(tibble::tibble(
      area_code = integer(),
      year = integer(),
      excreta_c_mgc_ha_yr = numeric()
    ))
  }
  grass_c <- .gn_grass_excreta_mass(excreta)
  grass_area <- .gn_grass_area(land_use, country_grid)
  grass_c |>
    dplyr::inner_join(grass_area, by = c("area_code", "year")) |>
    dplyr::mutate(
      excreta_c_mgc_ha_yr = dplyr::if_else(
        .data$grass_area_ha > 0,
        .data$excreta_c_mg / .data$grass_area_ha,
        0
      )
    ) |>
    dplyr::select("area_code", "year", "excreta_c_mgc_ha_yr")
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
# must not apply that fraction a second time.
.gn_grass_area <- function(land_use, country_grid) {
  cg <- .normalize_carbon_support(country_grid) |>
    dplyr::select("lon", "lat", "area_code")
  land_use |>
    dplyr::filter(stringr::str_to_lower(.data$land_use) == "grassland") |>
    dplyr::inner_join(cg, by = c("lon", "lat", "area_code")) |>
    dplyr::summarise(
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
      # Carbon-weighted, matching `.ci_wmean()` on the cropland path: taking
      # the first cell's fraction was harmless only while natural land carried
      # one global constant, and wrong for grassland, whose fraction is already
      # a per-cell blend of weed and excreta carbon.
      humified_fraction = .gn_wmean(
        .data$humified_fraction,
        .data$c_input_mgc_ha_yr * .data$class_area_ha
      ),
      method_c_input = .data$method_c_input[1],
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
  first_year = NULL
) {
  rlang::check_installed("ncdf4")
  run_dir <- .resolve_run_dir(run_dir)
  path <- file.path(run_dir, "cftfrac.nc")
  if (!file.exists(path)) {
    cli::cli_abort("LPJmL managed-fraction file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  first_year <- .lpjml_resolve_first_year(nc, first_year, "cftfrac.nc")
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
