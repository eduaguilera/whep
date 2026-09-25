# Intensive/extensive grassland classes through time.
#
# IMAGE 2010 (the land-use map behind Schulte-Uebbing et al. 2022, Zenodo
# doi:10.5281/zenodo.6395016) classes every grassland cell as intensive or
# extensive. Those classes are livestock production systems -- mixed
# systems are intensive, pastoral systems extensive -- not a threshold on
# grazing density (SI, "Calculation of the spatial distribution of N inputs
# by manure and fertilizer"). IMAGE publishes the map for 2010 only.
#
# To move the map through time, each 2010 country's intensive share of
# grassland area is scaled by the trend in its national grazing density
# (livestock units per grassland ha, relative to 2010). That trend is a
# declared proxy for the trend in IMAGE's production-system split, not a
# measurement of it. Cells are then reclassified by rank: demotion drops the
# IMAGE-intensive cells with the least manure N per ha first, promotion
# adds the IMAGE-extensive cells with the most manure N per ha first, and
# the number of cells switched is the one whose intensive area lands
# nearest to the national target. The ranked reclassification is a rule
# constructed for WHEP with no published precedent (issue #1285). At 2010
# the density ratio is 1 and the classes equal the IMAGE map exactly.

# The exported name is fixed by the design (issue #1285) and is 33
# characters long.
# nolint start: object_length_linter.
#' Classify grassland cells as intensive or extensive through time
#'
#' @description
#' Moves the IMAGE 2010 intensive/extensive grassland map behind the
#' Schulte-Uebbing et al. (2022) critical-nitrogen archive (Zenodo
#' doi:10.5281/zenodo.6395016) through time, one row per 0.5-degree cell and
#' year. It is the class table the `land_use = "all"` comparison of
#' [build_n_boundary_exceedance()] consumes (issue #1285).
#'
#' Each cell belongs to its 2010 country: the crosswalk area code holding the
#' largest share of the cell (ties to the smallest code). The national grazing
#' density of a polity in a year is the livestock units of grazer species it
#' reported that year per hectare of WHEP grassland (pasture plus rangeland)
#' inside its territory. A cell's `density_ratio` is the density of the polity
#' its 2010 country belonged to in the year, over the density of that country
#' in 2010. Each 2010 country's intensive share of IMAGE grassland is scaled by
#' the area-weighted mean ratio of its cells and clamped to `[0, 1]`; cells are
#' then switched by rank of 2010 manure N per hectare until the intensive area
#' is nearest the target (demotion from the lowest-manure intensive cell up,
#' promotion from the highest-manure extensive cell down). At 2010 the ratio
#' is 1 and the classes equal the IMAGE map.
#'
#' Livestock units are reported under the polity that existed in the year --
#' the USSR, not Russia, in 1961 -- while the crosswalk knows present-day area
#' codes only. Each present-day code is resolved to its reporting polity in the
#' year with [resolve_polity_lineage()], against the polities the livestock
#' units are actually reported under that year. A lineage answer that is a
#' different interval of a polity another present-day code reports under
#' itself is refused (it would book, say, Belgium on the Netherlands). A code
#' still unresolved is placed on a reporting polity whose published successors
#' reach it (the Baltic states and Belgium-Luxembourg), when exactly one does,
#' after WHEP's own `polity_area_code` fold has had its chance (Sudan and South
#' Sudan are reported folded into area 206 from 2012).
#' Grassland of a code that resolves to no reporting polity enters no
#' denominator, and its cells keep their IMAGE class.
#'
#' A country's density is its own reporting polity's density in the years it
#' reports on its own (`density_basis = "own"`), and the shared bucket's
#' density in the years it is reported folded into one after a split
#' (`"bucket"`, Sudan and South Sudan in area 206 from 2012). For the years
#' before it reported on its own, the country keeps its own level and borrows
#' only the trend of the polity its cells reported under
#' (`"chained_predecessor_trend"`): with `t0` the first later year the country
#' has a positive density and `t0_pred` the last year up to `t0 - 1` the
#' predecessor has one, `D_k(t) = D_k(t0) * D_pred(t) / D_pred(t0_pred)`.
#' Chains of successions are linked one step at a time from the present back
#' (Yugoslavia, then Serbia and Montenegro, then Serbia). A missing link
#' leaves the density, and the ratio, undefined. Densities are therefore read
#' for every year from the earliest requested one to the last year both the
#' livestock units and the grassland surface cover, not only the requested
#' years. Kazakhstan shows why: in 1961 the USSR's national density is six
#' times Kazakhstan's own 2010 density, so taking it as Kazakhstan's level
#' would promote most of Kazakhstan's grassland to intensive.
#'
#' Declared assumptions, each a constructed choice rather than a measurement:
#'
#' * IMAGE's classes are livestock production systems (mixed systems
#'   intensive, pastoral systems extensive), not a threshold on grazing
#'   density. The national density trend is a proxy for the trend in that
#'   split.
#' * The ranked reclassification has no published precedent (issue #1285).
#' * WHEP grassland in a cell with no IMAGE grassland is extensive, stamped
#'   `"no_image_grassland"` (maintainer decision 2026-09-24, issue #1285). Its
#'   allowance is applied downstream.
#' * Livestock units are the `"LU"` rows of [get_primary_production()] for
#'   the `animals_codes` "Grazers", from `liv_lu_coefs`, whose source is
#'   generic ("standard livestock unit definitions from FAO and European
#'   agricultural statistics"). Only density ratios are used, so the
#'   coefficients matter through changes in species mix alone.
#' * Chaining a successor onto its predecessor's trend is a constructed rule
#'   (maintainer decision 2026-09-24, issue #1285): it assumes the successor's
#'   density moved with its predecessor's before it reported on its own.
#' * A cell whose 2010 country has no finite, positive 2010 density, or whose
#'   polity has no density in the year, keeps its IMAGE class and is stamped
#'   `"image2010_fixed_no_density"`. So is an IMAGE grassland cell the
#'   crosswalk assigns to no country.
#'
#' @param years Integer vector of years to classify. 2010 is always read as
#'   the base of the density ratio; it is returned only when requested.
#' @param data Optional named list of injected inputs, each replacing its real
#'   read:
#'   * `stock_lu`: [get_primary_production()]-shaped rows with `year`,
#'     `area_code`, `item_prod_code`, `unit` and `value`. Default: that
#'     function from the earliest requested year (or 2010) to the last year
#'     of the grassland surface.
#'   * `gridded_pasture`: `lon`, `lat`, `year`, `pasture_ha`, `rangeland_ha`.
#'     Default: the parquet at `WHEP_GRIDDED_PASTURE_PATH` when set, else the
#'     `spatialize-gridded-pasture` pin.
#'   * `cell_polity`: `lon`, `lat`, `area_code`, `polity_frac`. Default: the
#'     `spatialize-cell-polity-fraction` pin (`WHEP_POLITY_FRACTION_PATH`
#'     overrides it).
#'   * `grassland_layers`: one row per IMAGE cell with `cell_id`, `lon`, `lat`,
#'     `a_crop_ha`, `a_gr_int_ha`, `a_gr_ext_ha`, `manure_int_n_kg`,
#'     `manure_ext_n_kg`, `image_class_2010` and `image_region`. Default: the
#'     critical-nitrogen archive, resolved as [read_critical_n()] resolves it.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble, one row per cell and year, sorted by `cell_id` and
#'   `year`. Cells are those with IMAGE grassland plus those with WHEP
#'   grassland in any requested year. Columns:
#'   * `cell_id`: canonical 0.5-degree cell key.
#'   * `lon`, `lat`: cell centre, degrees.
#'   * `year`: calendar year.
#'   * `country_2010`: crosswalk area code holding the largest share of the
#'     cell; `NA` for a cell the crosswalk does not cover.
#'   * `image_region`: IMAGE region (1-26); `NA` outside the archive.
#'   * `a_crop_ha`: IMAGE 2010 cropland area, ha (0 outside the archive).
#'   * `grass_ha_image`: IMAGE 2010 intensive plus extensive grassland, ha
#'     (0 where IMAGE has none).
#'   * `whep_grass_ha`: WHEP pasture plus rangeland in the year, ha (0 where
#'     the surface carries none).
#'   * `image_class_2010`: `"intensive"`, `"extensive"`, or `NA` where IMAGE
#'     has no grassland.
#'   * `grassland_class`: `"intensive"` or `"extensive"`.
#'   * `density_ratio`: the cell's grazing-density ratio to 2010; `NA` where
#'     undefined.
#'   * `target_share`: the 2010 country's target intensive share; `NA` where
#'     undefined and for cells with no IMAGE grassland.
#'   * `method_grassland_split`: `"image2010_density_rank"`,
#'     `"image2010_fixed_no_density"` or `"no_image_grassland"`.
#'   * `density_basis`: how the 2010 country's density in the year was
#'     formed: `"own"`, `"chained_predecessor_trend"` or `"bucket"`; `NA`
#'     where the cell has no 2010 country or its code resolves to no
#'     reporting polity that year.
#' @export
#' @examples
#' build_grassland_intensity_classes(example = TRUE)
build_grassland_intensity_classes <- function(
  years,
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_grassland_intensity_classes())
  }
  years <- .gic_check_years(years)
  base_years <- sort(unique(c(years, .gic_base_year())))
  inputs <- .gic_inputs(data, base_years)
  cells <- .gic_cells(inputs, years)
  lineage <- .gic_reporting_map(
    sort(unique(inputs$cell_polity$area_code)),
    .gic_span_years(inputs),
    inputs$grazers
  )
  code_density <- .gic_code_density(
    lineage$map,
    .gic_polity_density(inputs, lineage)
  )
  ratios <- .gic_cell_ratios(cells, code_density, years)
  .gic_inform_unresolved(
    dplyr::filter(lineage$map, .data$year %in% base_years),
    cells
  )
  .gic_classify(cells, ratios, inputs$pasture, years)
}
# nolint end

# Allowed IMAGE 2010 grassland classes.
.grassland_class_levels <- c("intensive", "extensive")

#' Reclassify IMAGE 2010 grassland cells by a national density trend.
#'
#' Pure engine: no I/O. For each 2010 country `k` and year `t`, the target
#' intensive share is `s_k(t) = s_k(2010) * m_k(t)`, clamped to `[0, 1]`,
#' where `m_k(t)` is the grassland-area-weighted mean of the finite
#' `density_ratio` over `k`'s cells. A country-year with no finite ratio is
#' undefined: its cells keep the IMAGE class, are stamped
#' `"image2010_fixed_no_density"`, and one `cli_inform()` (class
#' `whep_grassland_no_density`) names the countries.
#'
#' @param cells One row per IMAGE 2010 grassland cell: `cell_id`,
#'   `country_2010`, `grass_ha` (> 0), `image_class_2010` (`"intensive"` or
#'   `"extensive"`) and `manure_n_kg_ha` (manure N per ha of the cell's own
#'   class, the ranking key).
#' @param ratios One row per cell and year: `cell_id`, `year`,
#'   `density_ratio` (may be `NA`). Must cover every cell in every year.
#' @return A tibble, one row per row of `ratios`: `cell_id`, `year`,
#'   `country_2010`, `image_class_2010`, `grassland_class`, `density_ratio`,
#'   `target_share` (`NA` when undefined), `method_grassland_split`.
#' @noRd
.classify_grassland_cells <- function(cells, ratios) {
  .check_grassland_cells(cells)
  .check_grassland_ratios(ratios, cells)

  ranked <- .rank_grassland_cells(cells)
  targets <- .grassland_targets(ratios, ranked)
  .inform_no_density(targets)

  ratios |>
    dplyr::select("cell_id", "year", "density_ratio") |>
    dplyr::inner_join(ranked, by = "cell_id", relationship = "many-to-one") |>
    dplyr::inner_join(
      targets,
      by = c("country_2010", "year"),
      relationship = "many-to-one"
    ) |>
    .reclassify_ranked() |>
    dplyr::arrange(.data$cell_id, .data$year) |>
    dplyr::select(
      "cell_id",
      "year",
      "country_2010",
      "image_class_2010",
      "grassland_class",
      "density_ratio",
      "target_share",
      "method_grassland_split"
    )
}

# Validate the static per-cell table.
.check_grassland_cells <- function(cells) {
  .check_columns(
    cells,
    c(
      "cell_id",
      "country_2010",
      "grass_ha",
      "image_class_2010",
      "manure_n_kg_ha"
    ),
    "cells"
  )
  bad_class <- setdiff(cells$image_class_2010, .grassland_class_levels)
  bad_area <- sum(!is.finite(cells$grass_ha) | cells$grass_ha <= 0)
  bad_manure <- sum(
    !is.finite(cells$manure_n_kg_ha) | cells$manure_n_kg_ha < 0
  )
  bad_key <- sum(is.na(cells$cell_id) | is.na(cells$country_2010))
  dup_ids <- sum(duplicated(cells$cell_id))
  problems <- c(
    x = if (nrow(cells) == 0) "{.arg cells} has no rows.",
    x = if (length(bad_class) > 0) {
      "Unknown {.field image_class_2010}: {.val {bad_class}}."
    },
    x = if (bad_area > 0) {
      "{bad_area} cell{?s} with {.field grass_ha} not finite and > 0."
    },
    x = if (bad_manure > 0) {
      "{bad_manure} cell{?s} with {.field manure_n_kg_ha} not finite >= 0."
    },
    x = if (bad_key > 0) "{bad_key} cell{?s} with a missing id or country.",
    x = if (dup_ids > 0) "{dup_ids} duplicated {.field cell_id}{?s}."
  )
  .abort_grassland_input(problems, "cells")
}

# Validate the per-cell-year density ratios against the cell table.
.check_grassland_ratios <- function(ratios, cells) {
  .check_columns(ratios, c("cell_id", "year", "density_ratio"), "ratios")
  unknown <- sum(!ratios$cell_id %in% cells$cell_id)
  dup_rows <- sum(duplicated(ratios[c("cell_id", "year")]))
  negative <- sum(is.finite(ratios$density_ratio) & ratios$density_ratio < 0)
  # Valid only once unknown cells and duplicates are ruled out.
  n_missing <- length(unique(ratios$year)) * nrow(cells) - nrow(ratios)
  problems <- c(
    x = if (anyNA(ratios$cell_id) || anyNA(ratios$year)) {
      "{.field cell_id} and {.field year} must not be missing."
    },
    x = if (unknown > 0) {
      "{unknown} ratio row{?s} for cells absent from {.arg cells}."
    },
    x = if (dup_rows > 0) "{dup_rows} duplicated cell-year row{?s}.",
    x = if (negative > 0) "{negative} negative {.field density_ratio}{?s}.",
    x = if (unknown == 0 && dup_rows == 0 && n_missing > 0) {
      "{n_missing} cell-year{?s} missing: every cell needs every year."
    }
  )
  .abort_grassland_input(problems, "ratios")
}

# Abort with every collected input problem, if any.
.abort_grassland_input <- function(problems, arg_name) {
  if (length(problems) == 0) {
    return(invisible(NULL))
  }
  header <- paste0(
    "Invalid {.arg ",
    arg_name,
    "} for grassland classification."
  )
  cli::cli_abort(
    c(header, problems),
    class = "whep_grassland_input",
    .envir = parent.frame()
  )
}

# Attach each cell's country totals and its position in the switch order.
# Within a country and class, cells are ordered by manure N per ha
# descending (ties: cell_id ascending). `cum_ha` is the area of the prefix
# ending at the cell, `cum_prev_ha` the prefix before it. For intensive
# cells the prefix is the set kept intensive; for extensive cells it is the
# set promoted.
.rank_grassland_cells <- function(cells) {
  cells |>
    dplyr::mutate(
      total_ha = sum(.data$grass_ha),
      int_ha_2010 = sum(
        .data$grass_ha[.data$image_class_2010 == "intensive"]
      ),
      .by = "country_2010"
    ) |>
    dplyr::arrange(
      .data$country_2010,
      .data$image_class_2010,
      dplyr::desc(.data$manure_n_kg_ha),
      .data$cell_id
    ) |>
    dplyr::mutate(
      switch_rank = dplyr::row_number(),
      cum_ha = cumsum(.data$grass_ha),
      cum_prev_ha = dplyr::lag(.data$cum_ha, default = 0),
      .by = c("country_2010", "image_class_2010")
    )
}

# National target per 2010 country and year. `target_ha` is computed as
# the clamp of int_ha_2010 * m rather than s(t) * total_ha: the two are
# equal in exact arithmetic, and the first reproduces the IMAGE intensive
# area bit-for-bit when m = 1.
.grassland_targets <- function(ratios, ranked) {
  ratios |>
    dplyr::inner_join(
      dplyr::select(
        ranked,
        "cell_id",
        "country_2010",
        "grass_ha",
        "total_ha",
        "int_ha_2010"
      ),
      by = "cell_id"
    ) |>
    dplyr::mutate(finite = is.finite(.data$density_ratio)) |>
    dplyr::summarise(
      finite_ha = sum(.data$grass_ha[.data$finite]),
      ratio_ha = sum(
        .data$grass_ha[.data$finite] * .data$density_ratio[.data$finite]
      ),
      total_ha = dplyr::first(.data$total_ha),
      int_ha_2010 = dplyr::first(.data$int_ha_2010),
      .by = c("country_2010", "year")
    ) |>
    .grassland_target_share()
}

# Turn the weighted ratio sums into the clamped target share and area.
.grassland_target_share <- function(country_years) {
  country_years |>
    dplyr::mutate(
      defined = .data$finite_ha > 0,
      mean_ratio = dplyr::if_else(
        .data$defined,
        .data$ratio_ha / .data$finite_ha,
        NA_real_
      ),
      target_share = pmin(
        1,
        pmax(0, .data$int_ha_2010 / .data$total_ha * .data$mean_ratio)
      ),
      target_ha = pmin(
        .data$total_ha,
        pmax(0, .data$int_ha_2010 * .data$mean_ratio)
      )
    ) |>
    dplyr::select(
      "country_2010",
      "year",
      "defined",
      "target_share",
      "target_ha"
    )
}

# Report the country-years with no finite density ratio, once.
.inform_no_density <- function(targets) {
  undefined <- dplyr::filter(targets, !.data$defined)
  if (nrow(undefined) == 0) {
    return(invisible(NULL))
  }
  codes <- sort(unique(undefined$country_2010))
  n_country <- length(codes)
  n_year <- length(unique(undefined$year))
  cli::cli_inform(
    c(
      "No grazing density for {n_country} countr{?y/ies} in
       {n_year} year{?s} ({nrow(undefined)} country-year{?s}).",
      "i" = "Their cells keep the IMAGE 2010 grassland class.",
      "i" = "{cli::qty(n_country)}2010 country code{?s}: {.val {codes}}."
    ),
    class = "whep_grassland_no_density"
  )
}

# Pick the switched prefix per country-year and stamp the method.
#
# Prefix areas A_k (intensive: cum_ha; extensive: int_ha_2010 + cum_ha)
# increase with k, so |A_k - T| falls and then rises. The nearest prefix is
# therefore the run of cells whose own step moves the area no further from
# T than the previous prefix did. Ties: `<=` extends the kept-intensive
# prefix (demotion keeps the longer one), `<` stops the promoted prefix
# (promotion takes the shorter one). The same predicates yield no switch
# when the target is on the other side of the 2010 area: every intensive
# step moves towards a larger T, and every extensive step moves away from a
# smaller T. `cumall()` keeps the chosen set a prefix of the rank order.
# The unimodality needs every step to change the running area in floating
# point: a cell smaller than the rounding step of the country total (~5e-7
# ha at 3.2e9 ha) would stall it. The smallest positive IMAGE grassland
# cell is 3.5e-5 ha, so the archive cannot trigger this.
.reclassify_ranked <- function(rows) {
  rows |>
    dplyr::mutate(
      is_int = .data$image_class_2010 == "intensive",
      offset = dplyr::if_else(.data$is_int, 0, .data$int_ha_2010),
      gap_now = abs(.data$offset + .data$cum_ha - .data$target_ha),
      gap_prev = abs(.data$offset + .data$cum_prev_ha - .data$target_ha),
      step_ok = dplyr::if_else(
        .data$is_int,
        .data$gap_now <= .data$gap_prev,
        .data$gap_now < .data$gap_prev
      )
    ) |>
    dplyr::arrange(
      .data$country_2010,
      .data$year,
      .data$image_class_2010,
      .data$switch_rank
    ) |>
    dplyr::mutate(
      in_prefix = dplyr::cumall(.data$step_ok),
      .by = c("country_2010", "year", "image_class_2010")
    ) |>
    .stamp_grassland_class()
}

# Final class and method per cell-year.
.stamp_grassland_class <- function(rows) {
  rows |>
    dplyr::mutate(
      grassland_class = dplyr::case_when(
        !.data$defined ~ .data$image_class_2010,
        .data$in_prefix ~ "intensive",
        .default = "extensive"
      ),
      method_grassland_split = dplyr::if_else(
        .data$defined,
        "image2010_density_rank",
        "image2010_fixed_no_density"
      )
    )
}

# National grazing livestock density: livestock units of grazer species
# per hectare of national grassland, one row per country and year.

#' National grazing density, LU of grazers per ha of grassland.
#'
#' Sums livestock units (`unit == "LU"`) of the grazer item codes (see
#' `.grazer_item_codes()`) by `area_code` and `year`, then divides by
#' national grassland area. A country-year missing from one input carries
#' `NA` for that input's own quantity, and `grazing_density` is `NA`
#' wherever `grassland_ha` is missing or not positive, or `grazing_lu` is
#' `NA`. `sum()` is never given `na.rm = TRUE`: a country-year whose grazer
#' rows are all missing stays `NA` rather than becoming a false zero, and a
#' country-year with no grazer rows at all is `NA` because it contributes
#' no group to sum over.
#'
#' @param stock_lu A [get_primary_production()]-shaped tibble: `year`,
#'   `area_code`, `item_prod_code`, `unit`, `value`. These are the `"LU"`
#'   rows `build_primary_production()` derives from `animals_codes` and
#'   `liv_lu_coefs` (`R/build_production.R:1502-1530`).
#' @param grassland_ha One row per `area_code` and `year`: `area_code`,
#'   `year`, `grassland_ha`. No duplicated `(area_code, year)` key.
#' @return A tibble, one row per `(area_code, year)` present in either
#'   input, sorted by `area_code`, `year`: `area_code`, `year`,
#'   `grazing_lu`, `grassland_ha`, `grazing_density`.
#' @noRd
.grazing_density <- function(stock_lu, grassland_ha) {
  .check_columns(
    stock_lu,
    c("year", "area_code", "item_prod_code", "unit", "value"),
    "stock_lu"
  )
  .check_columns(
    grassland_ha,
    c("area_code", "year", "grassland_ha"),
    "grassland_ha"
  )
  .check_grazing_density_keys(grassland_ha)

  grazer_lu <- stock_lu |>
    dplyr::filter(
      .data$unit == "LU",
      .data$item_prod_code %in% .grazer_item_codes()
    ) |>
    dplyr::summarise(
      grazing_lu = sum(.data$value),
      .by = c("area_code", "year")
    )

  grazer_lu |>
    dplyr::full_join(grassland_ha, by = c("area_code", "year")) |>
    dplyr::mutate(
      # Zero grazer LU is undefined, not a density of 0 (spec, issue
      # #1285): a ratio of 0 would demote every intensive cell of the
      # country, and a zero is far likelier a reporting gap than a herd.
      grazing_density = dplyr::if_else(
        !is.na(.data$grassland_ha) &
          .data$grassland_ha > 0 &
          !is.na(.data$grazing_lu) &
          .data$grazing_lu > 0,
        .data$grazing_lu / .data$grassland_ha,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$area_code, .data$year) |>
    dplyr::select(
      "area_code",
      "year",
      "grazing_lu",
      "grassland_ha",
      "grazing_density"
    )
}

# The grazer item codes: the package's own animals_codes "Grazers"
# classification. Reuses .grazer_animal_codes() (R/footprint_grazing.R,
# same filter/pull/unique/as.integer logic) instead of re-deriving the
# same set under a second name.
.grazer_item_codes <- function() {
  .grazer_animal_codes()
}

# Abort when grassland_ha has a duplicated (area_code, year) key.
.check_grazing_density_keys <- function(grassland_ha) {
  dup <- sum(duplicated(grassland_ha[c("area_code", "year")]))
  if (dup == 0) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    "{dup} duplicated {.field area_code}/{.field year} key{?s} in
     {.arg grassland_ha}.",
    class = "whep_grazing_density_input"
  )
}

# Nearest-cell rate transfer for grassland allowance classes.
#
# Allowances are per-hectare rates by land class (managed, extensive). A
# cell whose class has no rate of its own -- an IMAGE-intensive cell
# demoted to extensive with no extensive rate, a cell where IMAGE has no
# grassland at all, or an extensive-only cell promoted to intensive with
# no managed rate -- borrows the rate of the nearest cell that has one,
# preferring the same country, then the same IMAGE region (maintainer
# decision 2026-09-24, issue #1285).

#' Borrow each class rate from the nearest cell that has one.
#'
#' For each of the `managed` and `extensive` classes, a cell with a
#' finite own rate keeps it (`method = "archive"`). Otherwise the rate is
#' taken from the nearest cell (great-circle distance between `lon`/`lat`
#' cell centres) with a finite rate for that class: first among cells
#' sharing the same `country`, then, if none, among cells sharing the
#' same `image_region`; a cell with neither gets `NA` and
#' `method = "none"`. Distance ties go to the donor with the smallest
#' `cell_id`.
#'
#' @param cells One row per cell: `cell_id`, `lon`, `lat` (degrees, cell
#'   centres), `country` (may be `NA`), `image_region` (may be `NA`),
#'   `managed_rate`, `extensive_rate` (`NA` where the cell has none).
#' @return A tibble sorted by `cell_id`: `cell_id`, `managed_rate`,
#'   `managed_method`, `extensive_rate`, `extensive_method`.
#' @noRd
.nearest_class_rate <- function(cells) {
  .check_columns(
    cells,
    c(
      "cell_id",
      "lon",
      "lat",
      "country",
      "image_region",
      "managed_rate",
      "extensive_rate"
    ),
    "cells"
  )
  .check_nearest_rate_keys(cells)

  managed <- .nearest_rate_one_class(cells, "managed_rate") |>
    dplyr::rename(managed_rate = "rate", managed_method = "method")
  extensive <- .nearest_rate_one_class(cells, "extensive_rate") |>
    dplyr::rename(extensive_rate = "rate", extensive_method = "method")

  cells |>
    dplyr::select("cell_id") |>
    dplyr::arrange(.data$cell_id) |>
    dplyr::left_join(managed, by = "cell_id") |>
    dplyr::left_join(extensive, by = "cell_id") |>
    dplyr::select(
      "cell_id",
      "managed_rate",
      "managed_method",
      "extensive_rate",
      "extensive_method"
    )
}

# Abort when `cells` has a duplicated cell_id.
.check_nearest_rate_keys <- function(cells) {
  dup <- sum(duplicated(cells$cell_id))
  if (dup == 0) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    "{dup} duplicated {.field cell_id}{?s} in {.arg cells}.",
    class = "whep_nearest_rate_input"
  )
}

# Great-circle distance (km) between every row of (lon1, lat1) and every
# row of (lon2, lat2): returns an n x m matrix. Earth radius 6371 km.
# Longitude differences need no wrapping: sin(dlon / 2)^2 already gives
# the minimal angular separation across the antimeridian.
.haversine_km_matrix <- function(lon1, lat1, lon2, lat2) {
  earth_radius_km <- 6371
  lon1r <- lon1 * pi / 180
  lat1r <- lat1 * pi / 180
  lon2r <- lon2 * pi / 180
  lat2r <- lat2 * pi / 180
  dlat <- outer(lat1r, lat2r, "-")
  dlon <- outer(lon1r, lon2r, "-")
  hav <- sin(dlat / 2)^2 +
    outer(cos(lat1r), cos(lat2r)) * sin(dlon / 2)^2
  # pmax()/pmin() do not preserve the matrix `dim` attribute, so it is
  # reinstated explicitly after the clamp; without this, max.col()
  # downstream silently misreads the result as a plain vector (found
  # by the delegated verifier's own reproduction, 2026-09-24).
  clamped <- pmin(1, pmax(0, hav))
  dim(clamped) <- dim(hav)
  2 * earth_radius_km * asin(sqrt(clamped))
}

# Nearest donor's cell_id for each row of `recipients`, processed in
# chunks of at most `chunk_size` rows so no single distance matrix spans
# the whole group. Donors are sorted by cell_id first, so max.col()'s
# "first" tie rule resolves an exact distance tie to the smallest donor
# cell_id.
.nearest_donor_id <- function(recipients, donors, chunk_size = 1000L) {
  donors <- dplyr::arrange(donors, .data$cell_id)
  idx <- seq_len(nrow(recipients))
  chunk <- ceiling(idx / chunk_size)
  purrr::map(split(idx, chunk), \(rows) {
    rec <- recipients[rows, ]
    # Rounded to 1e-6 km (1 mm) so geometrically equal distances tie: two
    # donors 0.5 degrees either side of a recipient in one grid row come
    # out ~1e-13 km apart in floating point, and the smallest-cell_id rule
    # would otherwise never fire. round() keeps the matrix dim.
    d <- round(
      .haversine_km_matrix(rec$lon, rec$lat, donors$lon, donors$lat),
      6
    )
    nearest <- max.col(-d, ties.method = "first")
    tibble::tibble(
      cell_id = rec$cell_id,
      donor_cell_id = donors$cell_id[nearest]
    )
  }) |>
    dplyr::bind_rows()
}

# Rate and method for one class ("managed_rate" or "extensive_rate"):
# the cell's own rate where finite, else the nearest same-country donor,
# else the nearest same-region donor, else NA with method "none".
.nearest_rate_one_class <- function(cells, rate_col) {
  base <- tibble::tibble(
    cell_id = cells$cell_id,
    lon = cells$lon,
    lat = cells$lat,
    country = cells$country,
    image_region = cells$image_region,
    rate = cells[[rate_col]]
  )
  own <- dplyr::filter(base, is.finite(.data$rate))
  recipients <- dplyr::filter(base, !is.finite(.data$rate))

  country_hits <- .nearest_by_group(recipients, own, "country")
  left <- dplyr::anti_join(recipients, country_hits, by = "cell_id")
  region_hits <- .nearest_by_group(left, own, "image_region")
  none_hits <- left |>
    dplyr::anti_join(region_hits, by = "cell_id") |>
    dplyr::transmute(.data$cell_id, rate = NA_real_, method = "none")

  dplyr::bind_rows(
    dplyr::transmute(own, .data$cell_id, .data$rate, method = "archive"),
    country_hits,
    region_hits,
    none_hits
  )
}

# Nearest donor rate within each non-NA value of `group_col` shared by a
# recipient and at least one donor. Recipients whose own `group_col`
# value is NA, or has no matching donor group, are absent from the
# result (left to the caller's next fallback stage).
.nearest_by_group <- function(recipients, donors, group_col) {
  method_label <- if (group_col == "country") {
    "nearest_country"
  } else {
    "nearest_region"
  }
  empty <- tibble::tibble(
    cell_id = integer(),
    rate = double(),
    method = character()
  )
  rec_group <- recipients[[group_col]]
  don_group <- donors[[group_col]]
  groups <- intersect(
    unique(rec_group[!is.na(rec_group)]),
    unique(don_group[!is.na(don_group)])
  )

  purrr::map(groups, \(g) {
    rec_g <- recipients[!is.na(rec_group) & rec_group == g, ]
    don_g <- donors[!is.na(don_group) & don_group == g, ]
    nearest <- .nearest_donor_id(rec_g, don_g)
    nearest |>
      dplyr::left_join(
        dplyr::select(don_g, donor_cell_id = "cell_id", rate = "rate"),
        by = "donor_cell_id"
      ) |>
      dplyr::transmute(.data$cell_id, .data$rate, method = method_label)
  }) |>
    c(list(empty)) |>
    dplyr::bind_rows()
}

# ---- build_grassland_intensity_classes() wiring ----------------------------

# The year every density ratio is taken against: the IMAGE map's own year.
.gic_base_year <- function() 2010L

# The year whose polity names a present-day crosswalk code's ISO3 for the
# successor fallback. The crosswalk carries present-day codes only (276 and
# 277, which exist from 2011), so any year after 2011 names them the same way.
.gic_present_year <- function() 2020L

.gic_check_years <- function(years) {
  ok <- is.numeric(years) &&
    length(years) > 0L &&
    !anyNA(years) &&
    all(years == round(years))
  if (!ok) {
    cli::cli_abort(
      "{.arg years} must be a non-empty vector of whole years.",
      class = "whep_grassland_input"
    )
  }
  sort(unique(as.integer(years)))
}

# Read (or take injected) every input, each checked where it enters.
.gic_inputs <- function(data, base_years) {
  layers <- data$grassland_layers %||% .gic_read_archive()
  cell_polity <- data$cell_polity %||% .read_cell_polity_fraction(NULL)
  pasture <- data$gridded_pasture %||% .gic_read_gridded_pasture()
  stock_lu <- data$stock_lu %||%
    get_primary_production(
      years = seq.int(
        min(base_years),
        max(c(base_years, pasture$year[pasture$year >= min(base_years)]))
      )
    )
  list(
    layers = .gic_check_layers(layers),
    grazers = .gic_grazer_lu(stock_lu, base_years),
    cell_polity = .gic_key_cell_polity(cell_polity),
    pasture = .gic_pasture(pasture, base_years)
  )
}

# The IMAGE 2010 grassland layers of the critical-N archive, with each cell's
# IMAGE region, from the archive directory read_critical_n() resolves.
.gic_read_archive <- function() {
  root <- .critn_root_path(.resolve_critical_n_dir(NULL))
  .critical_n_grassland_layers(root) |>
    dplyr::left_join(
      .critn_image_region(root),
      by = "cell_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(image_region = as.integer(.data$image_region))
}

# The gridded pasture + rangeland surface ag_land_support reads: the
# WHEP_GRIDDED_PASTURE_PATH parquet when set (as .als_read_gridded_pasture()
# reads it), else the spatialize-gridded-pasture pin run_spatialize() reads.
.gic_read_gridded_pasture <- function() {
  if (.has_path(Sys.getenv("WHEP_GRIDDED_PASTURE_PATH"))) {
    return(.n_read_parquet_env("WHEP_GRIDDED_PASTURE_PATH"))
  }
  .read_spatial_input(
    NULL,
    "gridded_pasture.parquet",
    .spatial_input_aliases()[["gridded_pasture"]]
  )
}

.gic_check_layers <- function(layers) {
  .check_columns(
    layers,
    c(
      "cell_id",
      "lon",
      "lat",
      "a_crop_ha",
      "a_gr_int_ha",
      "a_gr_ext_ha",
      "manure_int_n_kg",
      "manure_ext_n_kg",
      "image_class_2010",
      "image_region"
    ),
    "data$grassland_layers"
  )
  check_inputs_supplied(
    layers,
    c(
      intensive_grassland = "a_gr_int_ha",
      extensive_grassland = "a_gr_ext_ha",
      intensive_manure = "manure_int_n_kg",
      extensive_manure = "manure_ext_n_kg"
    ),
    details = c(
      i = "The IMAGE 2010 grassland layers come from the critical-N archive,
           Zenodo doi:10.5281/zenodo.6395016."
    )
  )
  dup <- sum(duplicated(layers$cell_id))
  if (dup > 0L) {
    cli::cli_abort(
      "{dup} duplicated {.field cell_id}{?s} in the grassland layers.",
      class = "whep_grassland_input"
    )
  }
  tibble::as_tibble(layers)
}

# Grazer livestock-unit rows from the earliest base year on: the chained
# densities need the years between the requested ones. A filter that
# matched nothing is judged as an absent input here, not passed as an
# empty table: every density would otherwise be NA and every cell would
# silently keep its 2010 class.
.gic_grazer_lu <- function(stock_lu, base_years) {
  .check_columns(
    stock_lu,
    c("year", "area_code", "item_prod_code", "unit", "value"),
    "data$stock_lu"
  )
  check_labels_supplied(stock_lu, "unit", "LU")
  grazers <- stock_lu |>
    dplyr::filter(
      .data$unit == "LU",
      .data$item_prod_code %in% .grazer_item_codes(),
      .data$year >= min(base_years)
    ) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_prod_code = .data$item_prod_code,
      unit = .data$unit,
      value = .data$value
    )
  probe <- if (nrow(grazers) == 0L) {
    tibble::tibble(value = NA_real_)
  } else {
    grazers
  }
  check_inputs_supplied(
    probe,
    c(grazing_livestock_units = "value"),
    details = c(
      i = "Grazer livestock units are the {.val LU} rows of
           {.fun get_primary_production} for the animals_codes Grazers."
    )
  )
  .gic_check_year_coverage(grazers$year, base_years, "grazer livestock units")
  grazers
}

.gic_key_cell_polity <- function(cell_polity) {
  .check_columns(
    cell_polity,
    c("lon", "lat", "area_code", "polity_frac"),
    "data$cell_polity"
  )
  check_inputs_supplied(cell_polity, c(polity_share = "polity_frac"))
  tibble::as_tibble(cell_polity) |>
    dplyr::select("lon", "lat", "area_code", "polity_frac") |>
    .nbx_add_cell_key("cell-polity crosswalk") |>
    dplyr::transmute(
      cell_id = .data$cell_id,
      area_code = as.integer(.data$area_code),
      polity_frac = .data$polity_frac
    )
}

# WHEP grassland per cell and year from the earliest base year on: pasture +
# rangeland, whole cell.
.gic_pasture <- function(pasture, base_years) {
  .check_columns(
    pasture,
    c("lon", "lat", "year", "pasture_ha", "rangeland_ha"),
    "data$gridded_pasture"
  )
  rows <- tibble::as_tibble(pasture) |>
    dplyr::select("lon", "lat", "year", "pasture_ha", "rangeland_ha") |>
    dplyr::filter(.data$year >= min(base_years))
  check_inputs_supplied(
    rows,
    c(pasture = "pasture_ha", rangeland = "rangeland_ha"),
    details = c(
      i = "WHEP grassland is the gridded pasture + rangeland surface
           ({.envvar WHEP_GRIDDED_PASTURE_PATH} or the
           {.val spatialize-gridded-pasture} pin)."
    )
  )
  .gic_check_year_coverage(rows$year, base_years, "gridded grassland")
  keyed <- rows |>
    .nbx_add_cell_key("gridded pasture surface") |>
    dplyr::transmute(
      cell_id = .data$cell_id,
      lon = .data$lon,
      lat = .data$lat,
      year = as.integer(.data$year),
      whep_grass_ha = .data$pasture_ha + .data$rangeland_ha
    )
  dup <- sum(duplicated(keyed[c("cell_id", "year")]))
  if (dup > 0L) {
    cli::cli_abort(
      "{dup} duplicated cell-year{?s} in the gridded pasture surface.",
      class = "whep_grassland_input"
    )
  }
  keyed
}

# The years densities are formed in: every year both the livestock units and
# the grassland surface carry, from the earliest base year on. A year the
# lineage cannot be resolved in has no support, so it is left out rather than
# resolved against nothing.
.gic_span_years <- function(inputs) {
  sort(intersect(unique(inputs$grazers$year), unique(inputs$pasture$year)))
}

# Abort when an input carries no row for a year the build needs. An absent
# year would make every density of that year NA, which reads as a country
# without grazing rather than as an input that was never read.
.gic_check_year_coverage <- function(present_years, base_years, what) {
  missing <- setdiff(base_years, present_years)
  if (length(missing) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(
    c(
      "No {what} for {length(missing)} year{?s}: {.val {missing}}.",
      i = "The density ratio needs every requested year and 2010."
    ),
    class = "whep_grassland_year_coverage"
  )
}

# The static per-cell table: every cell with IMAGE grassland, plus every cell
# with WHEP grassland in a requested year. A cell the archive does not carry
# has no IMAGE area of any class there, so its IMAGE areas are structural
# zeros, not absent measurements.
.gic_cells <- function(inputs, years) {
  image <- inputs$layers |>
    dplyr::transmute(
      cell_id = as.integer(.data$cell_id),
      lon = .data$lon,
      lat = .data$lat,
      image_region = as.integer(.data$image_region),
      a_crop_ha = .data$a_crop_ha,
      grass_ha_image = .data$a_gr_int_ha + .data$a_gr_ext_ha,
      image_class_2010 = .data$image_class_2010,
      manure_n_kg_ha = dplyr::case_when(
        .data$image_class_2010 == "intensive" ~
          .data$manure_int_n_kg / .data$a_gr_int_ha,
        .data$image_class_2010 == "extensive" ~
          .data$manure_ext_n_kg / .data$a_gr_ext_ha,
        .default = NA_real_
      )
    )
  whep <- inputs$pasture |>
    dplyr::filter(.data$year %in% years, .data$whep_grass_ha > 0) |>
    dplyr::distinct(.data$cell_id, .keep_all = TRUE) |>
    dplyr::select("cell_id", "lon", "lat")
  outside <- whep |>
    dplyr::filter(!.data$cell_id %in% image$cell_id) |>
    dplyr::mutate(
      image_region = NA_integer_,
      a_crop_ha = 0,
      grass_ha_image = 0,
      image_class_2010 = NA_character_,
      manure_n_kg_ha = NA_real_
    )
  dplyr::bind_rows(image, outside) |>
    dplyr::filter(
      .data$grass_ha_image > 0 | .data$cell_id %in% whep$cell_id
    ) |>
    dplyr::left_join(
      .gic_country_2010(inputs$cell_polity),
      by = "cell_id",
      relationship = "one-to-one"
    )
}

# Each cell's 2010 country: the crosswalk code with the largest share of the
# cell, ties to the smallest code.
.gic_country_2010 <- function(cell_polity) {
  cell_polity |>
    dplyr::arrange(
      .data$cell_id,
      dplyr::desc(.data$polity_frac),
      .data$area_code
    ) |>
    dplyr::distinct(.data$cell_id, .keep_all = TRUE) |>
    dplyr::transmute(cell_id = .data$cell_id, country_2010 = .data$area_code)
}

# ---- Present-day code -> reporting polity of the year ----------------------

# Map every present-day crosswalk code, in every base year, to the polity the
# grazer livestock units are reported under that year. Returns `map`
# (`area_code`, `year`, `polity_code`, `method_polity_lineage`) and
# `reporters` (`area_code`, `year`, `polity_code` of the livestock rows).
.gic_reporting_map <- function(codes, years, grazers) {
  reporters <- .gic_reporters(grazers)
  map <- .gic_lineage(codes, years, reporters) |>
    .gic_drop_sibling_collisions() |>
    .gic_bucket_fold(reporters) |>
    .gic_successor_fallback(reporters)
  list(map = map, reporters = reporters)
}

# WHEP's own aggregation fold for codes still open: a present-day code whose
# `polity_area_code` bucket is another area that reports livestock that year
# is placed on that area's polity. The livestock series is built on that
# bucket, so this is the grain the numerator already has: from 2012 Sudan
# (276) and South Sudan (277) are reported folded into area 206, whose 2011+
# polity is an aggregate no predecessor or successor edge reaches.
.gic_bucket_fold <- function(map, reporters) {
  open <- dplyr::filter(map, is.na(.data$polity_code))
  if (nrow(open) == 0L) {
    return(map)
  }
  folded <- open |>
    dplyr::select("area_code", "year") |>
    .add_reporting_polity_columns() |>
    tibble::as_tibble() |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      bucket_code = as.integer(.data$polity_area_code)
    ) |>
    dplyr::filter(
      !is.na(.data$bucket_code),
      .data$bucket_code != .data$area_code
    ) |>
    dplyr::inner_join(
      dplyr::rename(reporters, bucket_code = "area_code"),
      by = c("bucket_code", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      area_code = .data$area_code,
      year = .data$year,
      fold_code = .data$polity_code
    )
  map |>
    dplyr::left_join(
      folded,
      by = c("area_code", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      method_polity_lineage = dplyr::if_else(
        is.na(.data$polity_code) & !is.na(.data$fold_code),
        "bucket_fold",
        .data$method_polity_lineage
      ),
      polity_code = dplyr::coalesce(.data$polity_code, .data$fold_code)
    ) |>
    dplyr::select(-"fold_code")
}

# The reporting polity of every (area_code, year) the livestock units carry,
# resolved by the same year-aware helper the production build uses. A pair
# resolving to no polity cannot be placed on any territory.
.gic_reporters <- function(grazers) {
  grazers |>
    dplyr::distinct(.data$area_code, .data$year) |>
    .add_reporting_polity_columns() |>
    tibble::as_tibble() |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      polity_code = .data$reporting_polity_code
    ) |>
    dplyr::filter(!is.na(.data$polity_code))
}

# resolve_polity_lineage() walks each present-day code's predecessor edges
# back to a polity the livestock units are reported under in the year. Its
# unresolved warning is muffled because the fallback below still gets a
# chance, and what stays unresolved is reported once, after it.
.gic_lineage <- function(codes, years, reporters) {
  support <- reporters |>
    dplyr::distinct(.data$polity_code, .data$year) |>
    dplyr::transmute(
      polity_code = .data$polity_code,
      start_year = .data$year,
      end_year = .data$year + 1L
    )
  national <- tidyr::expand_grid(area_code = codes, year = years)
  withCallingHandlers(
    resolve_polity_lineage(national, support),
    whep_lineage_unresolved = function(w) invokeRestart("muffleWarning")
  ) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      year = as.integer(.data$year),
      polity_code = .data$lineage_polity_code,
      method_polity_lineage = .data$method_polity_lineage
    )
}

# Refuse a "sibling_interval" answer that another present-day code holds as
# its own anchor that year. The sibling rule matches any interval of the same
# polity family, and a family that lost territory lives on as another
# country: present-day Belgium walks to NLD-1815-1830 and from there to
# NLD-1830-2025, the Netherlands, and present-day Taiwan to China. Booking
# either on that country's density puts its grassland under livestock it
# never reported.
.gic_drop_sibling_collisions <- function(map) {
  anchors <- map |>
    dplyr::filter(.data$method_polity_lineage == "anchor") |>
    dplyr::transmute(
      polity_code = .data$polity_code,
      year = .data$year,
      anchor_code = .data$area_code
    )
  collide <- map |>
    dplyr::filter(.data$method_polity_lineage == "sibling_interval") |>
    dplyr::inner_join(
      anchors,
      by = c("polity_code", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(.data$anchor_code != .data$area_code) |>
    dplyr::distinct(.data$area_code, .data$year) |>
    dplyr::mutate(collides = TRUE)
  map |>
    dplyr::left_join(
      collide,
      by = c("area_code", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      collides = dplyr::coalesce(.data$collides, FALSE),
      polity_code = dplyr::if_else(.data$collides, NA, .data$polity_code),
      method_polity_lineage = dplyr::if_else(
        .data$collides,
        "unresolved",
        .data$method_polity_lineage
      )
    ) |>
    dplyr::select(-"collides")
}

# The other direction for codes the predecessor walk leaves open: a reporting
# polity no present-day code anchors (a dissolved federation, an aggregate
# reporting bucket) is walked down its published successor edges with
# .successor_stop_map(), the walk the production build uses to bridge
# dissolved federations to their successors' land. An open code is placed on
# a source whose walk stops on the code's ISO3, when exactly one source does.
# It is what reaches the Baltic states from the USSR (their predecessor chain
# runs through their own pre-1940 polities, not the USSR) and Belgium and
# Luxembourg from Belgium-Luxembourg (an aggregate no predecessor edge names).
.gic_successor_fallback <- function(map, reporters) {
  open <- dplyr::filter(map, is.na(.data$polity_code))
  if (nrow(open) == 0L) {
    return(map)
  }
  present_iso3 <- .gic_present_iso3(unique(map$area_code))
  anchored <- map |>
    dplyr::filter(.data$method_polity_lineage == "anchor") |>
    dplyr::distinct(.data$polity_code, .data$year)
  sources <- reporters |>
    dplyr::distinct(.data$polity_code, .data$year) |>
    dplyr::anti_join(anchored, by = c("polity_code", "year"))
  found <- .gic_successor_hits(open, sources, present_iso3)
  map |>
    dplyr::left_join(
      found,
      by = c("area_code", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      method_polity_lineage = dplyr::if_else(
        is.na(.data$polity_code) & !is.na(.data$fallback_code),
        "successor_walk",
        .data$method_polity_lineage
      ),
      polity_code = dplyr::coalesce(.data$polity_code, .data$fallback_code)
    ) |>
    dplyr::select(-"fallback_code")
}

# The open (area_code, year) pairs reached by exactly one source polity's
# successor walk, with that source as `fallback_code`.
.gic_successor_hits <- function(open, sources, present_iso3) {
  stops <- .successor_stop_map(
    unique(sources$polity_code),
    unique(stats::na.omit(unname(present_iso3)))
  )
  iso3_of <- .polity_iso3_lookup()
  reach <- sources |>
    dplyr::mutate(
      iso3 = purrr::map(.data$polity_code, \(p) unname(iso3_of[stops[[p]]]))
    ) |>
    tidyr::unnest_longer("iso3") |>
    dplyr::filter(!is.na(.data$iso3)) |>
    dplyr::distinct(.data$polity_code, .data$year, .data$iso3)
  open |>
    dplyr::transmute(
      area_code = .data$area_code,
      year = .data$year,
      iso3 = unname(present_iso3[as.character(.data$area_code)])
    ) |>
    dplyr::inner_join(
      reach,
      by = c("iso3", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(dplyr::n() == 1L, .by = c("area_code", "year")) |>
    dplyr::transmute(
      area_code = .data$area_code,
      year = .data$year,
      fallback_code = .data$polity_code
    )
}

# Present-day ISO3 of each crosswalk code, named by the code.
.gic_present_iso3 <- function(codes) {
  present <- .gic_present_polity(codes)
  rlang::set_names(unname(.polity_iso3_lookup()[present]), names(present))
}

# The polity each crosswalk code reports under in .gic_present_year(), named
# by the code.
.gic_present_polity <- function(codes) {
  present <- tibble::tibble(
    area_code = as.integer(codes),
    year = .gic_present_year()
  ) |>
    .add_reporting_polity_columns() |>
    tibble::as_tibble()
  rlang::set_names(present$reporting_polity_code, present$area_code)
}

# ---- Densities and ratios ---------------------------------------------------

# Grazing density per reporting polity and year: the polity's grazer
# livestock units over the WHEP grassland of every present-day code mapped to
# it that year, each crosswalk cell weighted by its polity share. The polity
# code stands in the `area_code` slot of .grazing_density(), which only groups
# and joins on it.
.gic_polity_density <- function(inputs, lineage) {
  lu <- inputs$grazers |>
    dplyr::inner_join(
      lineage$reporters,
      by = c("area_code", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$polity_code,
      item_prod_code = .data$item_prod_code,
      unit = .data$unit,
      value = .data$value
    )
  .grazing_density(lu, .gic_polity_grassland(inputs, lineage$map)) |>
    dplyr::rename(polity_code = "area_code")
}

# WHEP grassland per reporting polity and year, in the `area_code` slot
# .grazing_density() reads. A code mapped to no polity adds to none.
.gic_polity_grassland <- function(inputs, map) {
  inputs$cell_polity |>
    dplyr::inner_join(
      dplyr::select(inputs$pasture, "cell_id", "year", "whep_grass_ha"),
      by = "cell_id",
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      grassland_ha = sum(.data$whep_grass_ha * .data$polity_frac),
      .by = c("area_code", "year")
    ) |>
    dplyr::inner_join(
      dplyr::select(map, "area_code", "year", "polity_code"),
      by = c("area_code", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::filter(!is.na(.data$polity_code)) |>
    dplyr::summarise(
      grassland_ha = sum(.data$grassland_ha),
      .by = c("polity_code", "year")
    ) |>
    dplyr::rename(area_code = "polity_code")
}

# Grazing density per present-day code and year, and its basis. Years the
# code reports under its own polity (an anchor, or a sibling interval of its
# own present-day polity family) carry that polity's density; years folded
# into a shared bucket carry the bucket's; every other resolved year borrows
# its predecessor's trend onto the code's own later level
# (.gic_chain_code()). Returns `area_code`, `year`, `code_density`,
# `density_basis`.
.gic_code_density <- function(map, density) {
  family <- .lineage_family(.gic_present_polity(unique(map$area_code)))
  density <- dplyr::select(density, "polity_code", "year", "grazing_density")
  rows <- map |>
    dplyr::left_join(
      density,
      by = c("polity_code", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      own_family = dplyr::coalesce(
        .lineage_family(.data$polity_code) ==
          unname(family[as.character(.data$area_code)]),
        FALSE
      ),
      level = .data$method_polity_lineage %in%
        c("anchor", "bucket_fold") |
        (.data$method_polity_lineage == "sibling_interval" & .data$own_family)
    )
  # One series per code with the year as its axis, the shape of the fill_*()
  # helpers: the walk runs along the years of one code, so the split is on
  # the code alone.
  split(rows, rows$area_code) |>
    purrr::map(\(code_rows) .gic_chain_code(code_rows, density)) |>
    dplyr::bind_rows()
}

# One code's series. Level-bearing years keep their own (or bucket) density.
# The other resolved years form runs of one predecessor polity each, and each
# run is linked, latest first, to the code's first later year with a positive
# density: D(t) = D(t0) * D_pred(t) / D_pred(t0_pred), with t0_pred the last
# year up to t0 - 1 the predecessor has a positive density. Linking latest
# first lets an older run chain onto a newer run's chained values.
.gic_chain_code <- function(code_rows, density) {
  rows <- code_rows |>
    dplyr::arrange(.data$year) |>
    dplyr::mutate(
      run = dplyr::consecutive_id(.data$polity_code, .data$level)
    )
  start <- list(
    value = dplyr::if_else(rows$level, rows$grazing_density, NA_real_),
    basis = dplyr::case_when(
      rows$level & rows$method_polity_lineage == "bucket_fold" ~ "bucket",
      rows$level ~ "own",
      .default = NA_character_
    )
  )
  chained <- sort(
    unique(rows$run[!rows$level & !is.na(rows$polity_code)]),
    decreasing = TRUE
  )
  out <- purrr::reduce(
    chained,
    \(acc, run) .gic_chain_run(acc, rows, run, density),
    .init = start
  )
  tibble::tibble(
    area_code = rows$area_code,
    year = rows$year,
    code_density = out$value,
    density_basis = out$basis
  )
}

# Link one predecessor run onto the code's first later positive density.
# Without such a year, or without a positive predecessor density up to the
# year before it, the run's densities stay NA.
.gic_chain_run <- function(acc, rows, run, density) {
  idx <- which(rows$run == run)
  acc$basis[idx] <- "chained_predecessor_trend"
  later <- which(
    rows$year > max(rows$year[idx]) & is.finite(acc$value) & acc$value > 0
  )
  if (length(later) == 0L) {
    return(acc)
  }
  t0 <- rows$year[later[1L]]
  pred <- density[density$polity_code == rows$polity_code[idx[1L]], ]
  usable <- pred$year <= t0 - 1L &
    is.finite(pred$grazing_density) &
    pred$grazing_density > 0
  if (!any(usable)) {
    return(acc)
  }
  pred_t0 <- pred$grazing_density[usable][which.max(pred$year[usable])]
  acc$value[idx] <- acc$value[later[1L]] * rows$grazing_density[idx] / pred_t0
  acc
}

# One density ratio per cell and requested year: the density of the cell's
# 2010 country in the year over its density in 2010, both from
# .gic_code_density(). NA where either is missing or the 2010 density is not
# positive; a cell with no 2010 country has none. `density_basis` is the
# basis of the year's density.
.gic_cell_ratios <- function(cells, code_density, years) {
  base <- code_density |>
    dplyr::filter(.data$year == .gic_base_year()) |>
    dplyr::select("area_code", base_density = "code_density") |>
    tidyr::expand_grid(year = years)
  code_ratio <- code_density |>
    dplyr::filter(.data$year %in% years) |>
    dplyr::inner_join(
      base,
      by = c("area_code", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      density_ratio = dplyr::if_else(
        is.finite(.data$base_density) &
          .data$base_density > 0 &
          is.finite(.data$code_density),
        .data$code_density / .data$base_density,
        NA_real_
      )
    )
  tidyr::expand_grid(
    dplyr::select(cells, "cell_id", "country_2010"),
    year = years
  ) |>
    dplyr::left_join(
      dplyr::select(
        code_ratio,
        "area_code",
        "year",
        "density_ratio",
        "density_basis"
      ),
      by = c(country_2010 = "area_code", "year"),
      relationship = "many-to-one"
    ) |>
    dplyr::select("cell_id", "year", "density_ratio", "density_basis")
}

# Report the 2010 countries of in-scope cells whose code resolves to no
# reporting polity in some year: their grassland enters no density, and
# their cells keep the IMAGE class through the undefined-ratio path.
.gic_inform_unresolved <- function(map, cells) {
  open <- map |>
    dplyr::filter(
      is.na(.data$polity_code),
      .data$area_code %in% cells$country_2010
    )
  if (nrow(open) == 0L) {
    return(invisible(NULL))
  }
  codes <- sort(unique(open$area_code))
  n_code <- length(codes)
  cli::cli_inform(
    c(
      "{n_code} present-day area code{?s} resolve{?s/} to no reporting
       polity in {nrow(open)} code-year{?s}.",
      "i" = "{cli::qty(n_code)}Code{?s}: {.val {codes}}.",
      "i" = "Their grassland enters no grazing density that year."
    ),
    class = "whep_grassland_lineage_unresolved"
  )
}

# ---- Classification and output ---------------------------------------------

# IMAGE grassland cells with a 2010 country go through the ranked engine;
# IMAGE grassland cells the crosswalk assigns to no country keep their IMAGE
# class (no density can be defined for them); WHEP grassland in a cell with
# no IMAGE grassland is extensive.
.gic_classify <- function(cells, ratios, pasture, years) {
  image <- dplyr::filter(cells, .data$grass_ha_image > 0)
  ranked <- dplyr::filter(image, !is.na(.data$country_2010))
  stateless <- dplyr::filter(image, is.na(.data$country_2010))
  no_image <- dplyr::filter(cells, .data$grass_ha_image == 0)
  .gic_inform_stateless(stateless)

  dplyr::bind_rows(
    .gic_rank(ranked, ratios),
    .gic_fixed_rows(stateless, years),
    ratios |>
      dplyr::filter(.data$cell_id %in% no_image$cell_id) |>
      dplyr::select("cell_id", "year", "density_ratio") |>
      dplyr::mutate(
        grassland_class = "extensive",
        target_share = NA_real_,
        method_grassland_split = "no_image_grassland"
      )
  ) |>
    dplyr::left_join(
      dplyr::select(ratios, "cell_id", "year", "density_basis"),
      by = c("cell_id", "year"),
      relationship = "one-to-one"
    ) |>
    .gic_attach_cells(cells, pasture)
}

.gic_rank <- function(ranked, ratios) {
  if (nrow(ranked) == 0L) {
    return(NULL)
  }
  .classify_grassland_cells(
    dplyr::transmute(
      ranked,
      cell_id = .data$cell_id,
      country_2010 = .data$country_2010,
      grass_ha = .data$grass_ha_image,
      image_class_2010 = .data$image_class_2010,
      manure_n_kg_ha = .data$manure_n_kg_ha
    ),
    dplyr::filter(ratios, .data$cell_id %in% ranked$cell_id) |>
      dplyr::select("cell_id", "year", "density_ratio")
  ) |>
    dplyr::select(
      "cell_id",
      "year",
      "grassland_class",
      "density_ratio",
      "target_share",
      "method_grassland_split"
    )
}

.gic_fixed_rows <- function(stateless, years) {
  tidyr::expand_grid(
    dplyr::select(stateless, "cell_id", grassland_class = "image_class_2010"),
    year = years
  ) |>
    dplyr::mutate(
      density_ratio = NA_real_,
      target_share = NA_real_,
      method_grassland_split = "image2010_fixed_no_density"
    )
}

.gic_inform_stateless <- function(stateless) {
  if (nrow(stateless) == 0L) {
    return(invisible(NULL))
  }
  mha <- round(sum(stateless$grass_ha_image) / 1e6, 2)
  cli::cli_inform(
    c(
      "{nrow(stateless)} IMAGE grassland cell{?s} ({mha} Mha) ha{?s/ve} no
       country in the cell-polity crosswalk.",
      "i" = "They keep the IMAGE 2010 class in every year."
    ),
    class = "whep_grassland_no_country"
  )
}

# Attach the static cell columns and the year's WHEP grassland. A cell-year
# the pasture surface carries no row for has no WHEP grassland: the surface
# is WHEP's own lattice, so the missing row is a structural zero.
.gic_attach_cells <- function(rows, cells, pasture) {
  rows |>
    dplyr::left_join(
      dplyr::select(cells, -"manure_n_kg_ha"),
      by = "cell_id",
      relationship = "many-to-one"
    ) |>
    dplyr::left_join(
      dplyr::select(pasture, "cell_id", "year", "whep_grass_ha"),
      by = c("cell_id", "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(whep_grass_ha = dplyr::coalesce(.data$whep_grass_ha, 0)) |>
    dplyr::arrange(.data$cell_id, .data$year) |>
    dplyr::select(
      "cell_id",
      "lon",
      "lat",
      "year",
      "country_2010",
      "image_region",
      "a_crop_ha",
      "grass_ha_image",
      "whep_grass_ha",
      "image_class_2010",
      "grassland_class",
      "density_ratio",
      "target_share",
      "method_grassland_split",
      "density_basis"
    )
}
