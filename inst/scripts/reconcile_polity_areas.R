# reconcile_polity_areas.R
#
# T-A7 / S-A8. Reconciles every polity's polycell total against its official
# national area, and attributes each material discrepancy to one of three
# owners rather than tabulating it:
#
#   polygon quality  - upstream `whep-polities`: the shipped polygon is not
#                      the country (e.g. Maldives at a fraction of its area).
#   identity gap     - the crosswalk half (DA-28): the areas are right but the
#                      polity carries no `area_code`, so it receives no data.
#   convention       - ours (DA-3): inland water and ice are territory but not
#                      land, so `land_area_ha` is legitimately below an
#                      official area that counts inland water.
#
# COMPARE LIKE WITH LIKE. `polity_area_ha` = land + inland water + ice is the
# quantity that reconciles with an official national area (DA-3);
# `land_area_ha` is not and must not be expected to.
#
# Official areas: FAOSTAT Land Use domain (RL), element "Area" (5110), items
# "Country area" (6600), "Land area" (6601), "Inland waters" (6680) and
# "Coastal waters" (6773), read from the package's own `faostat-landuse` pin
# (`inst/extdata/whep_inputs.csv`, version `20260624T095700Z-5c591`), so the
# reference is versioned rather than typed in.
# Cite as: FAO. FAOSTAT Land Use (RL). Rome.
# https://www.fao.org/faostat/en/#data/RL
#
#   The FAOSTAT Land Use Country Notes (release June 2025, "All countries")
#   state it exactly: `"Country area" is defined by FAO as the sum of "Land
#   area," "Inland waters" and "Coastal waters." For data prior to 2007,
#   countries reported using the older FAO definition, which excluded "Coastal
#   waters" as a subcomponent.` So FAO's own "Country area" is NOT the
#   conventional national area -- it puts Canada at 15.64 Mkm2 against its
#   9.98 Mkm2. The official reference used here is therefore
#   `Land area + Inland waters`, which is the country's territory excluding
#   coastal waters and the EEZ, and is what DA-3's `polity_area_ha` means.
#   FAO's "Land area" includes glaciers, so it is compared against
#   `land_area_ha + ice_area_ha`, and "Inland waters" against
#   `inland_water_ha`.
#
# What it reports:
#   O    the official-area reference: coverage, the coastal-waters correction
#        and FAO's own internal residual, which is not absorbed.
#   R    the whole reconciliation at the year, on `polity_area_ha`, plus the
#        DA-3 decomposition against FAO's land / inland-water split.
#   G1   the four zero-land island states of EA6/EA9 (French Polynesia,
#        Kiribati, Micronesia, Maldives): the whole cell each drew under the
#        crosswalk, what the polycell gives now, and the official area.
#   G2   the polities with over half their land in shared cells, on BOTH the
#        crosswalk basis (EA6's 23, AM-29's 21 of 191) and the polycell basis
#        (AM-29's 29 of 187), reconciled against both published censuses.
#   G3   polygon quality: the EA9 pair MDV/MCO against the current 749-row
#        table, then every polity whose polygon disagrees with the official
#        area by more than the reporting threshold. EA9 measured both on the
#        CShapes polygons that whep-polities issue #56 filed; PR #70 rebound
#        both to GADM, which is what this section re-measures.
#   I    the identity gap (DA-28, AM-42), split into its three mechanisms:
#        no `area_code`, an `area_code` that is a DA-23 bucket rather than the
#        polity's own FAO area, and no polycell at all.
#   D    DA-22 great-circle borders, as a NAMED CAUSE where they bear on a
#        discrepancy: the USA/Canada 49th-parallel displacement is 123,276
#        km2. It is filed upstream (whep-polities #151) and is NOT in the
#        embedded polygon copy, so it is live in every number below.
#   V    cells two live polities BOTH claim, which is double-counted land no
#        per-polity conservation check can see (whep-polities #142).
#   U    the DA-7 unclaimed-land magnitude (S-A11), un-renormalised.
#   A    the attribution roll-up: every material discrepancy against exactly
#        one of the three causes, with none left unattributed.
#
# This script MEASURES. It never adjusts the producer to close a gap: a wrong
# area is a finding about the polygon, the crosswalk or the convention, and
# each has a different owner.
#
# Run:
#   Rscript inst/scripts/reconcile_polity_areas.R
#
# Inputs, all resolved from environment variables (never hardcode the path):
#   WHEP_LPJML_INPUT_DIR      grid.clm + glwd_lakes_and_rivers_30arcmin.clm.
#                             Optional; inland water is skipped when unset,
#                             and then only the DA-3 decomposition in R is
#                             unavailable -- `polity_area_ha` is unaffected.
#   WHEP_NATURALEARTH_DIR     ne_10m_glaciated_areas/. Optional; as above.
#   WHEP_LUH2_DIR             staticData_quarterdeg.nc. Required for G2's
#                             crosswalk basis and for U.
#   WHEP_POLITY_FRACTION_PATH cell_polity_fraction.parquet. Required for G1's
#                             whole-cell draw and for G2's crosswalk basis.
#   WHEP_RPA_SUPPORT_RDS      Optional cache. The whole-domain intersection
#                             takes about an hour; when this points at a
#                             readable file the support is read from it, and
#                             when it points at a missing one the freshly
#                             built support is written there. The build call
#                             is the same either way -- see `.rpa_support()`.
#   WHEP_RPA_POLITY_CODES     Optional comma-separated subset, for editing
#                             this file without paying the full build.
#   WHEP_RPA_YEAR             Optional reconciliation year, default 2015.
#
# Note for anyone whose environment variables look unset: R reads `.Renviron`
# in the working directory INSTEAD of `~/.Renviron` (issue #456). Run with
# R_ENVIRON_USER pointing at the user file, or export them in the shell.

.rpa_h <- function(x) cli::cli_h2(x)

.rpa_env <- function(name) {
  value <- Sys.getenv(name, "")
  if (nzchar(value)) value else NULL
}

.rpa_codes_from_env <- function() {
  codes <- Sys.getenv("WHEP_RPA_POLITY_CODES", "")
  if (!nzchar(codes)) {
    return(NULL)
  }
  stringr::str_split_1(codes, ",") |> stringr::str_trim()
}

# ---- Official areas ---------------------------------------------------------

# The four RL items this reconciliation needs, by FAOSTAT item code. Element
# 5110 is "Area", reported in 1000 ha throughout the domain.
.RPA_ITEMS <- c(
  country_ha = 6600,
  land_ha = 6601,
  inland_ha = 6680,
  coastal_ha = 6773
)

.rpa_official <- function(year) {
  .rpa_h(paste0("O: official areas, FAOSTAT Land Use (RL) at ", year))
  raw <- whep::whep_read_file("faostat-landuse")
  wide <- raw |>
    dplyr::rename(
      area_code = "Area Code",
      area_name = "Area",
      item_code = "Item Code",
      element_code = "Element Code",
      fao_year = "Year",
      value = "Value"
    ) |>
    dplyr::filter(
      .data$element_code == 5110L,
      .data$fao_year == year,
      .data$item_code %in% .RPA_ITEMS
    ) |>
    dplyr::mutate(
      quantity = names(.RPA_ITEMS)[match(.data$item_code, .RPA_ITEMS)],
      value = .data$value * 1000
    ) |>
    dplyr::select("area_code", "area_name", "quantity", "value") |>
    tidyr::pivot_wider(names_from = "quantity", values_from = "value") |>
    dplyr::mutate(
      area_code = as.integer(.data$area_code),
      coastal_ha = dplyr::coalesce(.data$coastal_ha, 0),
      inland_reported = !is.na(.data$inland_ha),
      official_ha = .data$land_ha + dplyr::coalesce(.data$inland_ha, 0),
      fao_residual_ha = .data$country_ha -
        (.data$official_ha + .data$coastal_ha)
    )
}

# FAO's own additivity is reported rather than absorbed: `Country area` is
# DEFINED as the sum of the three components, so a non-zero residual is a
# defect in the reference, and a reference defect must not be silently
# attributed to WHEP's polygons.
#
# It is also why the official reference here is `Land area + Inland waters`
# and not `Country area - Coastal waters`. Canada's country area does absorb
# its 5.76 Mkm2 of coastal waters (residual 0), but France's does not: land
# 538,950 + inland 10,240 + coastal 57,220 km2 against a country area of
# 549,087 km2, so subtracting coastal waters would knock 57,220 km2 off a
# France that never included them. Summing the two land-side components is
# immune to that, and is exactly the quantity DA-3's `polity_area_ha` means.
#
# Restricted to the areas the reconciliation actually uses: the domain also
# carries FAO aggregates (World, OECD, Africa), whose residuals are the sum of
# their members' and say nothing about any country's reference.
.rpa_report_official <- function(official, bridge) {
  used <- dplyr::semi_join(official, bridge, by = "area_code")
  bad <- dplyr::filter(used, abs(.data$fao_residual_ha) > 1000)
  cli::cli_text(
    "{nrow(official)} FAOSTAT areas, {nrow(used)} of them reachable from an
     ISO code; {sum(used$inland_reported)} report inland waters;
     {sum(used$coastal_ha > 0)} report coastal waters
     ({round(sum(used$coastal_ha) / 1e6, 2)} Mha), which the official
     reference excludes. FAO's own components fail to re-sum on {nrow(bad)}
     used area{?s}."
  )
  if (nrow(bad) > 0L) {
    print(
      as.data.frame(dplyr::arrange(
        dplyr::select(
          bad,
          "area_code",
          "area_name",
          "country_ha",
          "land_ha",
          "inland_ha",
          "coastal_ha",
          "official_ha",
          "fao_residual_ha"
        ),
        dplyr::desc(abs(.data$fao_residual_ha))
      )),
      digits = 8
    )
  }
}

# FAO's Country Notes name their own exceptions, and two of them decide an
# attribution rather than decorating it. Quoted verbatim from the release
# above, because a reference that means something different for one country is
# not a defect in WHEP's polygon and must not be booked as one.
# Keyed on ISO code, never on a hand-typed FAO area code: the numeric codes
# are looked up through the same bridge as everything else, so a wrong number
# cannot silently attach a note to the wrong country.
.RPA_FAO_NOTES <- tibble::tribble(
  ~iso3c, ~fao_note, ~ice_free_reference,
  # "Greenland 'Country area' refers to area free from ice." So the reference
  # is the ice-free area, and `polity_area_ha` -- which carries 177.5 Mha of
  # ice -- cannot match it. The comparable quantity is land + inland water.
  "GRL", "country area is the ICE-FREE area", TRUE,
  # "Data for the categories 'Country area' and 'Land area' include the Golan
  # Heights." Recorded so a Golan-sized residual is not read as geometry.
  "ISR", "includes the Golan Heights", FALSE,
  # "'Country area' include 'Coastal waters', starting with the year 2013."
  # Already removed by the Land area + Inland waters reference; noted so the
  # removal is traceable to the country's own statement.
  "NOR", "country area includes coastal waters from 2013", FALSE
)

# iso3c -> FAOSTAT area code, from the package's own crosswalk rather than a
# hand-typed table. Two ISO codes carry a historical and a current FAO area
# (ETH 62/238, SDN 206/276); the tie is broken on which of them FAO actually
# reports at the reconciliation year, and an unbroken tie aborts rather than
# silently taking the first.
.rpa_bridge <- function(official) {
  bridge <- whep::polity_area_crosswalk |>
    dplyr::distinct(.data$area_code, .data$area_iso3c) |>
    dplyr::filter(!is.na(.data$area_iso3c)) |>
    dplyr::mutate(area_code = as.integer(.data$area_code)) |>
    dplyr::semi_join(official, by = "area_code")
  ambiguous <- bridge |>
    dplyr::count(.data$area_iso3c) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(ambiguous) > 0L) {
    cli::cli_abort(
      "{nrow(ambiguous)} ISO code{?s} still map to more than one FAOSTAT area
       after the year filter: {.val {ambiguous$area_iso3c}}."
    )
  }
  cli::cli_text(
    "iso3c bridge: {nrow(bridge)} FAOSTAT areas reachable from an ISO code."
  )
  bridge
}

# ---- Inputs -----------------------------------------------------------------

.rpa_water <- function() {
  if (is.null(.rpa_env("WHEP_LPJML_INPUT_DIR"))) {
    cli::cli_alert_warning("WHEP_LPJML_INPUT_DIR unset: no inland water.")
    return(NULL)
  }
  whep::read_glwd_water()
}

.rpa_ice <- function() {
  if (is.null(.rpa_env("WHEP_NATURALEARTH_DIR"))) {
    cli::cli_alert_warning("WHEP_NATURALEARTH_DIR unset: no ice.")
    return(NULL)
  }
  whep::read_glaciated_areas()
}

.rpa_luh2 <- function() {
  if (is.null(.rpa_env("WHEP_LUH2_DIR"))) {
    cli::cli_alert_warning("WHEP_LUH2_DIR unset: no LUH2 layer.")
    return(NULL)
  }
  whep::read_luh2_terrestrial()
}

.rpa_crosswalk <- function() {
  if (is.null(.rpa_env("WHEP_POLITY_FRACTION_PATH"))) {
    cli::cli_alert_warning("WHEP_POLITY_FRACTION_PATH unset: no crosswalk.")
    return(NULL)
  }
  whep::build_cell_polity()
}

# One build call, cached or not. The cache exists so this file can be EDITED
# and re-run: `inst/scripts/` is under no test, so an unexecuted change here
# is unverified by construction.
.rpa_support <- function(polity_codes, year, water, ice, luh2, crosswalk) {
  cache <- .rpa_env("WHEP_RPA_SUPPORT_RDS")
  if (!is.null(cache) && file.exists(cache)) {
    cli::cli_alert_info("Reading the cached support from {.file {cache}}.")
    return(readRDS(cache))
  }
  cli::cli_alert_info("Building the polycell support table...")
  support <- whep::build_polycell_support(
    geometries = whep::get_polity_geometries(polity_codes),
    water = water,
    ice = ice,
    data = list(luh2 = luh2, crosswalk = crosswalk, crosswalk_year = year)
  )
  if (!is.null(cache)) {
    saveRDS(support, cache)
    cli::cli_alert_info("Cached the support at {.file {cache}}.")
  }
  support
}

# ---- The reconciliation -----------------------------------------------------

# One row per polity live at `year`, carrying the polycell totals, the
# geometry-table attributes needed to attribute a discrepancy, and the
# official reference joined through iso3c.
.rpa_polity_table <- function(support, official, bridge, year) {
  at_year <- whep::expand_polycell_years(support, year)
  shared <- at_year |>
    dplyr::count(.data$cell_id, name = "cell_polycells") |>
    dplyr::filter(.data$cell_polycells > 1L)
  totals <- at_year |>
    dplyr::mutate(is_shared = .data$cell_id %in% shared$cell_id) |>
    dplyr::summarise(
      polity_area_ha = sum(.data$polity_area_ha),
      land_area_ha = sum(.data$land_area_ha),
      inland_water_ha = sum(.data$inland_water_ha),
      ice_area_ha = sum(.data$ice_area_ha),
      polycells = dplyr::n(),
      shared_polycells = sum(.data$is_shared),
      shared_land_ha = sum(.data$land_area_ha * .data$is_shared),
      terra_pieces = sum(.data$area_engine == "terra"),
      area_code = dplyr::first(.data$area_code),
      polygon_status = dplyr::first(.data$polygon_status),
      .by = "polity_code"
    )
  .rpa_attach_official(totals, official, bridge, year)
}

.rpa_attach_official <- function(totals, official, bridge, year) {
  live <- .rpa_live_polities(year)
  orphan <- setdiff(totals$polity_code, live$polity_code)
  if (length(orphan) > 0L) {
    cli::cli_abort(
      "{length(orphan)} polit{?y/ies} produced polycells at {year} but are not
       in the live set: {.val {utils::head(orphan, 10)}}. The two year rules
       have drifted apart."
    )
  }
  live |>
    dplyr::left_join(totals, by = "polity_code") |>
    dplyr::left_join(
      dplyr::rename(bridge, fao_area_code = "area_code"),
      by = c(iso3c = "area_iso3c")
    ) |>
    dplyr::left_join(
      dplyr::rename(official, fao_area_code = "area_code"),
      by = "fao_area_code"
    ) |>
    dplyr::left_join(.RPA_FAO_NOTES, by = "iso3c") |>
    dplyr::mutate(
      # The comparable quantity, per FAO's own note. Where the reference is
      # the ice-free area, ice is taken OUT of the WHEP side rather than added
      # to FAO's -- FAO states what it excludes, and inventing an ice figure
      # for it would be the silent reconciliation DA-5 forbids.
      ice_free_reference = dplyr::coalesce(.data$ice_free_reference, FALSE),
      comparable_ha = dplyr::if_else(
        .data$ice_free_reference,
        .data$polity_area_ha - .data$ice_area_ha,
        .data$polity_area_ha
      ),
      rel = .data$comparable_ha / .data$official_ha - 1,
      abs_gap_ha = .data$comparable_ha - .data$official_ha
    )
}

# The DA-7 live-real-polity filter followed by the package's own year
# resolution. Both are BORROWED rather than restated: `.polity_is_live()` is
# the one reading of which rows are dead, and `.active_polities()` carries the
# DA-24 open-interval rule and the DA-29 tie-break. A hand-rolled
# `year < end_year | end_year == max(end_year)` looks equivalent and is not --
# eight polities in the 749-row table have two intervals ending at the domain
# end, so it opens both (AM-27).
.rpa_live_polities <- function(year) {
  attrs <- sf::st_drop_geometry(whep::get_polity_geometries())
  live <- whep:::.polity_is_live(attrs$wiki_status) &
    !(attrs$polity_type %in% "aggregate")
  whep:::.active_polities(attrs[live, ], year) |>
    dplyr::select(
      "polity_code",
      "polity_name",
      "iso3c",
      "polygon_source",
      "polygon_area_km2",
      "last_ingest",
      polygon_status_table = "polygon_status"
    ) |>
    tibble::as_tibble()
}

.rpa_reconcile <- function(polities, year) {
  .rpa_h(paste0("R: polity_area_ha against the official area at ", year))
  matched <- dplyr::filter(
    polities,
    !is.na(.data$official_ha),
    !is.na(.data$polity_area_ha)
  )
  cli::cli_text(
    "{nrow(polities)} live polities; {sum(!is.na(polities$official_ha))} carry
     an official area; {nrow(matched)} of those also produced polycells.
     Median |rel| {signif(stats::median(abs(matched$rel)), 3)};
     within 2%: {sum(abs(matched$rel) <= 0.02)};
     over 5%: {sum(abs(matched$rel) > 0.05)};
     global polycell territory {round(sum(matched$polity_area_ha) / 1e9, 4)}
     Gha against {round(sum(matched$official_ha) / 1e9, 4)} Gha official."
  )
  print(
    as.data.frame(utils::head(
      dplyr::arrange(
        dplyr::select(
          matched,
          "polity_code",
          "polity_name",
          "polity_area_ha",
          "comparable_ha",
          "official_ha",
          "rel",
          "fao_note"
        ),
        dplyr::desc(abs(.data$rel))
      ),
      20
    )),
    digits = 6
  )
  .rpa_decomposition(matched)
  .rpa_unreferenced(polities)
  invisible(matched)
}

# A live polity with no FAOSTAT area is not a defect in either direction: it
# is territory WHEP splits more finely than FAO reports it, so its hectares
# are missing from whichever FAO area they belong to. Printed with its area so
# the neighbour's shortfall is readable as the same hectares, not as a second
# independent finding.
.rpa_unreferenced <- function(polities) {
  loose <- polities |>
    dplyr::filter(is.na(.data$official_ha), !is.na(.data$polity_area_ha)) |>
    dplyr::select("polity_code", "polity_name", "iso3c", "polity_area_ha")
  if (nrow(loose) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_text(
    "{nrow(loose)} live polit{?y/ies} hold {round(sum(loose$polity_area_ha) /
     1e6, 3)} Mha with no FAOSTAT area to be judged against; those hectares
     are missing from the FAO area that reports them."
  )
  print(as.data.frame(loose), digits = 6)
}

# DA-3, stated as a measurement rather than as a claim: `land_area_ha` is
# BELOW the official area by construction wherever inland water and ice are
# territory, and that is the convention working, not a defect.
.rpa_decomposition <- function(matched) {
  usable <- dplyr::filter(matched, .data$inland_reported)
  cli::cli_text(
    "DA-3 decomposition on the {nrow(usable)} polities whose FAO area reports
     inland waters: median land+ice / FAO land area
     {signif(stats::median((usable$land_area_ha + usable$ice_area_ha) /
       usable$land_ha), 4)};
     WHEP inland water {round(sum(usable$inland_water_ha) / 1e6, 1)} Mha
     against FAO {round(sum(usable$inland_ha) / 1e6, 1)} Mha;
     land_area_ha alone sits {round(100 * (sum(usable$land_area_ha) /
       sum(usable$official_ha) - 1), 2)}% below the official total, which is
     DA-3 and not a discrepancy."
  )
}

# ---- G1: the four zero-land island states -----------------------------------

.RPA_ISLANDS <- c(
  "French Polynesia" = "PYF",
  "Kiribati" = "KIR",
  "Micronesia" = "FSM",
  "Maldives" = "MDV"
)

.rpa_islands <- function(polities, crosswalk, year) {
  .rpa_h("G1: the four zero-land island states (EA6/EA9)")
  rows <- polities |>
    dplyr::filter(.data$iso3c %in% .RPA_ISLANDS) |>
    dplyr::select(
      "polity_code",
      "iso3c",
      "polycells",
      "polity_area_ha",
      "land_area_ha",
      "official_ha",
      "rel"
    )
  if (!is.null(crosswalk)) {
    rows <- rows |>
      dplyr::left_join(
        .rpa_crosswalk_draw(crosswalk, rows$iso3c),
        by = "iso3c"
      ) |>
      dplyr::mutate(
        crosswalk_x = .data$crosswalk_ha / .data$official_ha,
        polycell_x = .data$polity_area_ha / .data$official_ha
      )
  }
  print(as.data.frame(rows), digits = 6)
  cli::cli_text(
    "EA6/EA9 measured these four holding 18 / 9 / 4 / 1 crosswalk cells with
     ZERO LUH2 terrestrial area, each drawing a whole 0.5-degree cell, so
     over-counting by ~13x / ~34x / ~18x / ~10x. `crosswalk_ha` is that draw
     and `crosswalk_x` its ratio to the official area; `polity_area_ha` and
     `polycell_x` are what the polycell gives now. `crosswalk_x` for French
     Polynesia is above EA9's ~13x because EA9 divided by ~0.42 Mha while
     FAOSTAT's land area is 0.3471 Mha -- a difference in the REFERENCE, not
     in the draw, which is why the source is named."
  )
  invisible(rows)
}

.rpa_crosswalk_draw <- function(crosswalk, iso3c) {
  bridge <- whep::polity_area_crosswalk |>
    dplyr::distinct(.data$area_code, .data$area_iso3c) |>
    dplyr::filter(.data$area_iso3c %in% iso3c) |>
    dplyr::mutate(area_code = as.integer(.data$area_code))
  crosswalk |>
    dplyr::mutate(area_code = as.integer(.data$area_code)) |>
    dplyr::inner_join(bridge, by = "area_code") |>
    dplyr::summarise(
      crosswalk_cells = dplyr::n(),
      crosswalk_ha = sum(.data$cell_area_ha * .data$polity_frac),
      .by = "area_iso3c"
    ) |>
    dplyr::rename(iso3c = "area_iso3c")
}

# ---- G2: land in border-shared cells ----------------------------------------

# Two censuses on two supports, and BOTH WEIGHTINGS on each, because the two
# published figures differ only in the weighting and neither says which it
# used. "Their land" can mean the polity's LUH2 terrestrial hectares in the
# cell, or the whole-cell hectares the crosswalk hands it; the two disagree
# because a polity's share of a cell's LAND is not its share of the CELL.
.rpa_shared_census <- function(polities, support, crosswalk, luh2, year) {
  .rpa_h("G2: polities with over half their land in border-shared cells")
  crosswalk_side <- .rpa_shared_crosswalk(crosswalk, luh2)
  polycell_side <- .rpa_shared_polycell(support, year)
  .rpa_report_crosswalk_census(crosswalk_side)
  .rpa_report_polycell_census(polycell_side)
  .rpa_shared_named(crosswalk_side, polities)
  invisible(list(crosswalk = crosswalk_side, polycell = polycell_side))
}

.rpa_shared_crosswalk <- function(crosswalk, luh2) {
  if (is.null(crosswalk) || is.null(luh2)) {
    return(NULL)
  }
  per_cell <- dplyr::count(crosswalk, .data$lon, .data$lat, name = "n_polities")
  crosswalk |>
    dplyr::mutate(area_code = as.integer(.data$area_code)) |>
    dplyr::inner_join(luh2, by = c("lon", "lat")) |>
    dplyr::inner_join(per_cell, by = c("lon", "lat")) |>
    dplyr::mutate(
      is_shared = .data$n_polities > 1L,
      land_w = .data$terrestrial_ha * .data$polity_frac,
      cell_w = .data$cell_area_ha * .data$polity_frac
    ) |>
    dplyr::summarise(
      land_ha = sum(.data$land_w),
      shared_land_ha = sum(.data$land_w * .data$is_shared),
      cell_ha = sum(.data$cell_w),
      shared_cell_ha = sum(.data$cell_w * .data$is_shared),
      .by = "area_code"
    ) |>
    .rpa_add_shares() |>
    dplyr::left_join(.rpa_area_names(), by = "area_code")
}

.rpa_area_names <- function() {
  whep::polity_area_crosswalk |>
    dplyr::distinct(.data$area_code, .data$area_name) |>
    dplyr::mutate(area_code = as.integer(.data$area_code)) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

.rpa_shared_polycell <- function(support, year) {
  at_year <- whep::expand_polycell_years(support, year)
  shared <- at_year |>
    dplyr::count(.data$cell_id, name = "cell_polycells") |>
    dplyr::filter(.data$cell_polycells > 1L)
  flagged <- at_year |>
    dplyr::mutate(
      is_shared = .data$cell_id %in% shared$cell_id,
      land_w = .data$land_area_ha,
      cell_w = .data$polity_area_ha
    )
  list(
    by_polity = .rpa_shared_by(flagged, "polity_code"),
    by_area = .rpa_shared_by(
      dplyr::filter(flagged, !is.na(.data$area_code)),
      "area_code"
    )
  )
}

.rpa_shared_by <- function(flagged, key) {
  flagged |>
    dplyr::summarise(
      land_ha = sum(.data$land_w),
      shared_land_ha = sum(.data$land_w * .data$is_shared),
      cell_ha = sum(.data$cell_w),
      shared_cell_ha = sum(.data$cell_w * .data$is_shared),
      .by = dplyr::all_of(key)
    ) |>
    .rpa_add_shares()
}

.rpa_add_shares <- function(x) {
  x |>
    dplyr::mutate(
      share_land = dplyr::if_else(
        .data$land_ha > 0,
        .data$shared_land_ha / .data$land_ha,
        NA_real_
      ),
      share_cell = dplyr::if_else(
        .data$cell_ha > 0,
        .data$shared_cell_ha / .data$cell_ha,
        NA_real_
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$share_land))
}

.rpa_over <- function(x, column, threshold) {
  sum(x[[column]] > threshold, na.rm = TRUE)
}

# The counts are bound to locals before the message: a `{.rpa_over(...)}` in a
# cli string is read as INLINE MARKUP, not as a call.
.rpa_report_crosswalk_census <- function(x) {
  if (is.null(x)) {
    cli::cli_alert_warning("No crosswalk or no LUH2: crosswalk basis skipped.")
    return(invisible(NULL))
  }
  areas <- nrow(x)
  zero_land <- sum(is.na(x$share_land))
  land50 <- .rpa_over(x, "share_land", 0.5)
  cell50 <- .rpa_over(x, "share_cell", 0.5)
  land45 <- .rpa_over(x, "share_land", 0.45)
  cell45 <- .rpa_over(x, "share_cell", 0.45)
  cli::cli_text(
    "crosswalk basis, {areas} area_codes ({zero_land} hold no LUH2 land at
     all, so {areas - zero_land} are judgeable): over 50% on the LUH2-LAND
     weighting {land50}, on the WHOLE-CELL weighting {cell50}; over 45%
     {land45} and {cell45}. EA6 published 23 of 191 and AM-29 re-measured 21:
     those are the two WEIGHTINGS of one census, not a correction."
  )
  print(
    as.data.frame(dplyr::select(
      dplyr::filter(x, .data$share_land > 0.45),
      "area_code",
      "area_name",
      "land_ha",
      "share_land",
      "share_cell"
    )),
    digits = 4
  )
}

.rpa_report_polycell_census <- function(x) {
  polities <- nrow(x$by_polity)
  areas <- nrow(x$by_area)
  p_land <- .rpa_over(x$by_polity, "share_land", 0.5)
  p_cell <- .rpa_over(x$by_polity, "share_cell", 0.5)
  a_land <- .rpa_over(x$by_area, "share_land", 0.5)
  a_cell <- .rpa_over(x$by_area, "share_cell", 0.5)
  cli::cli_text(
    "polycell basis: {polities} polities ({p_land} over 50% on land,
     {p_cell} on territory); {areas} area_codes ({a_land} and {a_cell}).
     AM-29 published 29 of 187 on this basis."
  )
  print(
    as.data.frame(dplyr::select(
      dplyr::filter(x$by_polity, .data$share_land > 0.5),
      "polity_code",
      "land_ha",
      "share_land",
      "share_cell"
    )),
    digits = 4
  )
}

# EA6 named Singapore and Macao at 100% and "Bahrain 46%"; AM-29 reported that
# Bahrain measures 29.9% and that Croatia is the 46.5% entry. Both are printed
# on both weightings, on the crosswalk the two censuses were taken on, so the
# disagreement is settled by the numbers rather than by whose amendment is
# later.
.rpa_shared_named <- function(crosswalk_side, polities) {
  if (is.null(crosswalk_side)) {
    return(invisible(NULL))
  }
  codes <- polities |>
    dplyr::filter(.data$iso3c %in% c("SGP", "MAC", "BHR", "HRV")) |>
    dplyr::select("polity_code", "iso3c", "area_code")
  named <- codes |>
    dplyr::inner_join(crosswalk_side, by = "area_code") |>
    dplyr::select(
      "polity_code",
      "iso3c",
      "area_code",
      "land_ha",
      "share_land",
      "share_cell"
    )
  print(as.data.frame(named), digits = 4)
}

# ---- G3: polygon quality ----------------------------------------------------

# EA9's two named polygon defects, re-measured against the CURRENT table so
# any upstream repair shows up as movement. `polygon_area_km2` and
# `last_ingest` come from the geometry table itself, so the row states what
# upstream believes about it beside what this producer measures and what FAO
# reports -- three independent numbers, none of them derived from another.
.RPA_EA9_POLYGONS <- tibble::tribble(
  ~polity_code, ~ea9_km2, ~ea9_official_km2,
  "MDV-1800-2025", 24.4, 300,
  "MCO-1800-2025", 19.7, 2.08
)

.rpa_polygon_quality <- function(polities, threshold) {
  .rpa_h("G3: polygon quality against the official area")
  named <- .RPA_EA9_POLYGONS |>
    dplyr::left_join(
      dplyr::select(
        polities,
        "polity_code",
        "polygon_source",
        "polygon_status_table",
        "polygon_area_km2",
        "last_ingest",
        "polity_area_ha",
        "official_ha",
        "rel"
      ),
      by = "polity_code"
    ) |>
    dplyr::mutate(
      now_km2 = .data$polity_area_ha / 100,
      official_km2 = .data$official_ha / 100
    )
  print(
    as.data.frame(dplyr::select(
      named,
      "polity_code",
      "polygon_source",
      "polygon_status_table",
      "last_ingest",
      "polygon_area_km2",
      "ea9_km2",
      "now_km2",
      "ea9_official_km2",
      "official_km2",
      "rel"
    )),
    digits = 6
  )
  worst <- polities |>
    dplyr::filter(!is.na(.data$rel), abs(.data$rel) > threshold) |>
    dplyr::arrange(dplyr::desc(abs(.data$rel)))
  cli::cli_text(
    "{nrow(worst)} live polities disagree with the official area by more than
     {round(100 * threshold)}%, holding
     {round(sum(abs(worst$abs_gap_ha)) / 1e6, 1)} Mha of absolute gap."
  )
  print(
    as.data.frame(dplyr::select(
      worst,
      "polity_code",
      "polygon_source",
      "polygon_status_table",
      "polity_area_ha",
      "official_ha",
      "rel",
      "inland_reported"
    )),
    digits = 6
  )
  invisible(worst)
}

# ---- V: two polities holding one polygon ------------------------------------

# The producer emits this rather than deciding who owns the ground, and it is
# the single largest entry in the reconciliation after the reference cases: a
# polity whose polygon contains a neighbour that is ALSO a live polity is over
# its official area by exactly that neighbour, and the neighbour is counted
# twice in every global total. Filed upstream as whep-polities #142.
.rpa_overlap <- function(support, year) {
  .rpa_h(paste0("V: cells two live polities both claim, ", year))
  at_year <- whep::expand_polycell_years(support, year)
  cells <- at_year |>
    dplyr::summarise(
      claimed_ha = sum(.data$polity_area_ha),
      .by = c("cell_id", "cell_area_ha")
    ) |>
    dplyr::filter(.data$claimed_ha > .data$cell_area_ha * (1 + 1e-4))
  if (nrow(cells) == 0L) {
    cli::cli_alert_success("No cell holds more territory than the cell.")
    return(invisible(character()))
  }
  by_polity <- at_year |>
    dplyr::semi_join(cells, by = "cell_id") |>
    dplyr::summarise(
      overlap_cells = dplyr::n(),
      overlap_ha = sum(.data$polity_area_ha),
      .by = "polity_code"
    ) |>
    dplyr::arrange(dplyr::desc(.data$overlap_ha))
  cli::cli_text(
    "{nrow(cells)} cells hold more territory than the cell, an excess of
     {round(sum(cells$claimed_ha - cells$cell_area_ha) / 1e6, 3)} Mha over
     {nrow(by_polity)} polities. This is DOUBLE-COUNTED land, invisible to
     every per-polity conservation check."
  )
  print(as.data.frame(utils::head(by_polity, 12)), digits = 6)
  by_polity$polity_code
}

# ---- I: the identity gap ----------------------------------------------------

# AM-42's nine polities lose their deposition outright. Their AREAS are not
# the problem -- this section exists to show that, so the gap is filed against
# the identity half of DA-28 and not against the polygons.
#
# The gap has THREE mechanisms, not one, and lumping them would hide two of
# them:
#   (a) no `area_code` at all, so nothing keyed on `area_code` reaches the
#       polity (MKD, SYR, PSE, GNQ, SWZ, NCL);
#   (b) an `area_code` that is a DA-23 reporting BUCKET rather than the
#       polity's own FAO area, so its data is delivered under a different
#       country's name (Sudan and South Sudan both fold onto 206, "Sudan
#       (former)", which is why AM-29 saw 276 and 277 go to zero while 206
#       gained their sum);
#   (c) a polygon s2 cannot read, so the polity produces no polycell at all
#       and there is nothing to key (Fiji, whep-polities #147, still open).
.RPA_AM42_IDENTITY <- c(
  "SDN",
  "SSD",
  "SYR",
  "MKD",
  "SWZ",
  "GNQ",
  "PSE",
  "FJI",
  "NCL"
)

.rpa_identity <- function(polities, crosswalk) {
  .rpa_h("I: the identity gap (DA-28, AM-42)")
  reachable <- polities$iso3c[
    !is.na(polities$fao_area_code) &
      .data_in(polities$fao_area_code, crosswalk)
  ]
  no_code <- polities |>
    dplyr::filter(!is.na(.data$polity_area_ha), is.na(.data$area_code))
  folded <- polities |>
    dplyr::filter(
      !is.na(.data$area_code),
      !is.na(.data$fao_area_code),
      .data$area_code != .data$fao_area_code
    )
  no_polycell <- dplyr::filter(polities, is.na(.data$polity_area_ha))
  cli::cli_text(
    "{nrow(no_code)} live polities hold measured territory but carry no
     `area_code` ({round(sum(no_code$polity_area_ha) / 1e6, 1)} Mha);
     {nrow(folded)} carry an `area_code` that is not their own FAO area
     ({round(sum(folded$polity_area_ha) / 1e6, 1)} Mha);
     {nrow(no_polycell)} produced no polycell at all."
  )
  print(
    as.data.frame(dplyr::arrange(
      dplyr::select(
        no_code,
        "polity_code",
        "iso3c",
        "polity_area_ha",
        "official_ha",
        "rel"
      ),
      dplyr::desc(.data$polity_area_ha)
    )),
    digits = 6
  )
  if (nrow(folded) > 0L) {
    print(
      as.data.frame(dplyr::arrange(
        dplyr::select(
          folded,
          "polity_code",
          "iso3c",
          "area_code",
          "fao_area_code",
          "polity_area_ha",
          "official_ha",
          "rel"
        ),
        dplyr::desc(.data$polity_area_ha)
      )),
      digits = 6
    )
  }
  if (nrow(no_polycell) > 0L) {
    print(
      as.data.frame(dplyr::select(
        no_polycell,
        "polity_code",
        "iso3c",
        "polygon_source",
        "polygon_status_table",
        "official_ha"
      )),
      digits = 6
    )
  }
  .rpa_identity_setcheck(
    unique(c(no_code$iso3c, folded$iso3c, no_polycell$iso3c)),
    reachable
  )
  invisible(no_code)
}

# TRUE where a FAO area code is one the deployed crosswalk actually carries.
# With no crosswalk to check against, nothing is claimed to be reachable --
# the wider-gap line then says so instead of asserting a number it cannot see.
.data_in <- function(fao_area_code, crosswalk) {
  if (is.null(crosswalk)) {
    return(rep(FALSE, length(fao_area_code)))
  }
  fao_area_code %in% as.integer(crosswalk$area_code)
}

# AM-42's nine is a SUBSET, and saying so is the point. It counted the
# polities that LOSE deposition, which requires that the deployed crosswalk
# was delivering to them in the first place; the identity gap itself is wider,
# because a territory the crosswalk never reached loses nothing by not being
# keyed. So the check that must hold is containment -- every one of the nine
# is still here -- and the surplus is reported as the wider gap rather than as
# a failure. A missing member IS a failure: it means the measurement stopped
# measuring, or upstream fixed it and the plan has not been updated.
.rpa_identity_setcheck <- function(found, reachable) {
  missing <- setdiff(.RPA_AM42_IDENTITY, found)
  extra <- setdiff(found, .RPA_AM42_IDENTITY)
  cli::cli_text(
    "AM-42 named {length(.RPA_AM42_IDENTITY)}; the identity gap is
     {length(found)} polities wide, of which
     {length(intersect(found, reachable))} are reachable through the deployed
     crosswalk -- which is the condition AM-42's nine were counted under.
     Missing from the measurement: {length(missing)}."
  )
  if (length(missing) > 0L) {
    cli::cli_alert_danger("In AM-42's list but not found: {.val {missing}}.")
  } else {
    cli::cli_alert_success("All of AM-42's nine are still in the gap.")
  }
  cli::cli_text(
    "Wider than AM-42 by {length(extra)}: {.val {extra}}. These carry no
     `area_code` either, but the crosswalk never delivered to them, so they
     lost nothing that AM-42 could measure."
  )
}

# ---- D: DA-22 great-circle borders ------------------------------------------

.rpa_long_edges <- function(support, polities) {
  .rpa_h("D: DA-22 great-circle borders, as a named cause")
  edges <- attr(support, "long_edges")
  if (is.null(edges) || nrow(edges) == 0L) {
    cli::cli_alert_success("No polity edge spans a degree along a parallel.")
    return(invisible(NULL))
  }
  live <- dplyr::semi_join(edges, polities, by = "polity_code")
  cli::cli_text(
    "{nrow(edges)} long edges over all intervals, {nrow(live)} on polities
     live at the reconciliation year; worst bulge
     {round(max(live$bulge_deg), 4)} degrees."
  )
  print(
    as.data.frame(utils::head(
      dplyr::arrange(live, dplyr::desc(.data$span_deg)),
      10
    )),
    digits = 6
  )
  affected <- c(
    .rpa_border_pair(polities, live, c("USA", "CAN"), 123276),
    .rpa_border_pair(polities, live, c("EGY", "SDN"), 2359)
  )
  invisible(affected)
}

# The displacement is mutual and conserves land, so no conservation check can
# see it (DA-22). What it DOES leave is a signed pair of residuals against the
# official areas, which is why it is reported here beside them. Only polities
# that actually still carry a long edge are returned as DA-22-affected: a
# named pair whose polygons have been repaired upstream must stop being
# attributed to a defect it no longer has.
.rpa_border_pair <- function(polities, live_edges, iso3c, expected_km2) {
  pair <- polities |>
    dplyr::filter(.data$iso3c %in% .env$iso3c) |>
    dplyr::mutate(
      long_edges = .data$polity_code %in% live_edges$polity_code
    ) |>
    dplyr::select(
      "polity_code",
      "iso3c",
      "long_edges",
      "polity_area_ha",
      "official_ha",
      "abs_gap_ha",
      "rel"
    )
  cli::cli_text(
    "{paste(iso3c, collapse = '/')}: DA-22 measured a mutual displacement of
     {expected_km2} km2 = {round(expected_km2 / 10000, 3)} Mha. Filed upstream
     as whep-polities #151 and NOT in the embedded polygon copy, which is why
     `long_edges` below is expected to be TRUE. Signed residuals against the
     official areas:"
  )
  print(as.data.frame(pair), digits = 6)
  pair$polity_code[pair$long_edges]
}

# ---- U: unclaimed land ------------------------------------------------------

.rpa_unclaimed <- function(support, year) {
  .rpa_h(paste0("U: DA-7 unclaimed land at ", year, " (S-A11)"))
  disagreement <- attr(support, "unassigned")
  if (is.null(disagreement)) {
    cli::cli_alert_warning("No LUH2 layer: the S-A11 magnitude is unavailable.")
    return(invisible(NULL))
  }
  at_year <- dplyr::filter(
    disagreement,
    .data$start_year <= year,
    year < .data$end_year
  )
  cli::cli_text(
    "{sum(at_year$unassigned_land_ha > 0)} cells hold
     {round(sum(at_year$unassigned_land_ha) / 1e6, 2)} Mha of LUH2 land no live
     polity claims; {sum(at_year$over_claimed_land_ha > 0)} cells over-claim
     {round(sum(at_year$over_claimed_land_ha) / 1e6, 2)} Mha. Neither is
     renormalised away."
  )
  invisible(at_year)
}

# ---- A: attribution ---------------------------------------------------------

# Every material discrepancy lands in exactly one bucket. The order is the
# order of ownership, not of size: a polity with no official reference cannot
# be judged at all, an unusable polygon is upstream by construction, and only
# what survives all of them is a live geometry-versus-statistics disagreement.
#
# The convention bucket is EARNED, not assumed. It is entered only when the
# solid part of the territory -- `land_area_ha + ice_area_ha`, which is what
# FAO's "Land area" means, glaciers included -- reconciles while the total
# does not, i.e. the whole discrepancy sits in the water and ice categories.
# A polity whose land alone already disagrees cannot have its gap explained by
# how inland water is counted, whichever way the sign runs.
.rpa_attribute <- function(polities, threshold, border_codes, overlap_codes) {
  .rpa_h("A: attribution of every material discrepancy")
  classified <- polities |>
    dplyr::mutate(
      solid_ha = dplyr::if_else(
        .data$ice_free_reference,
        .data$land_area_ha,
        .data$land_area_ha + .data$ice_area_ha
      ),
      rel_land = .data$solid_ha / .data$land_ha - 1,
      material = is.na(.data$rel) | abs(.data$rel) > threshold,
      cause = dplyr::case_when(
        !.data$material ~ "reconciles within threshold",
        is.na(.data$official_ha) ~ "no official reference (not judgeable)",
        is.na(.data$polity_area_ha) ~ "polygon unusable (upstream)",
        .data$polity_code %in% overlap_codes ~
          "polygon held by two polities (upstream)",
        .data$polity_code %in% border_codes ~ "DA-22 border (upstream)",
        !is.na(.data$rel_land) & abs(.data$rel_land) <= threshold ~
          "water/ice category only (convention)",
        TRUE ~ "polygon quality (upstream)"
      )
    )
  print(as.data.frame(dplyr::arrange(
    dplyr::summarise(
      classified,
      polities = dplyr::n(),
      abs_gap_mha = round(
        sum(abs(.data$abs_gap_ha), na.rm = TRUE) / 1e6,
        2
      ),
      .by = "cause"
    ),
    dplyr::desc(.data$polities)
  )))
  upstream <- dplyr::filter(
    classified,
    .data$material,
    .data$cause == "polygon quality (upstream)"
  )
  cli::cli_text(
    "{sum(classified$material)} material discrepancies at the
     {round(100 * threshold)}% threshold, all attributed;
     {nrow(upstream)} of them are geometry-versus-statistics and belong to
     `whep-polities`."
  )
  print(
    as.data.frame(dplyr::arrange(
      dplyr::select(
        upstream,
        "polity_code",
        "polygon_source",
        "polygon_status_table",
        "rel",
        "rel_land"
      ),
      dplyr::desc(abs(.data$rel))
    )),
    digits = 4
  )
  invisible(classified)
}

# ---- Run --------------------------------------------------------------------

.rpa_main <- function(
  year = as.integer(Sys.getenv("WHEP_RPA_YEAR", "2015")),
  threshold = 0.05,
  polity_codes = .rpa_codes_from_env()
) {
  rlang::check_installed(c("sf", "terra"))
  official <- .rpa_official(year)
  bridge <- .rpa_bridge(official)
  .rpa_report_official(official, bridge)
  water <- .rpa_water()
  ice <- .rpa_ice()
  luh2 <- .rpa_luh2()
  crosswalk <- .rpa_crosswalk()
  support <- .rpa_support(polity_codes, year, water, ice, luh2, crosswalk)
  cli::cli_alert_success(
    "{nrow(support)} interval rows
     ({dplyr::n_distinct(support$polycell_id)} polycells)."
  )
  polities <- .rpa_polity_table(support, official, bridge, year)
  .rpa_reconcile(polities, year)
  .rpa_islands(polities, crosswalk, year)
  .rpa_shared_census(polities, support, crosswalk, luh2, year)
  .rpa_polygon_quality(polities, threshold)
  overlap_codes <- .rpa_overlap(support, year)
  .rpa_identity(polities, crosswalk)
  border_codes <- .rpa_long_edges(support, polities)
  .rpa_unclaimed(support, year)
  .rpa_attribute(polities, threshold, border_codes, overlap_codes)
  cli::cli_alert_success("Done.")
  invisible(polities)
}

.rpa_main()
